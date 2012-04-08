unit mongoWire;

interface

uses SysUtils, SyncObjs, Classes, Sockets, bsonDoc;

type
  TBSONDocumentsEnumerator=class //abstract
  public
    function Next(Doc:IBSONDocument):boolean; virtual; abstract;
  end;

  TMongoWire=class(TObject)
  private
    FSocket:TTcpClient;
    FData:TStreamAdapter;
    FWriteLock,FReadLock:TCriticalSection;
    FQueue:array of record
      RequestID:integer;
      Data:TStreamAdapter;
    end;
    FQueueIndex,FRequestIndex:integer;
    procedure DataCString(x: WideString);
    procedure OpenMsg(OpCode,Flags:integer;Collection:WideString);
    function CloseMsg(Data:TStreamAdapter=nil):integer;//RequestID
    procedure ReadMsg(RequestID:integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open(ServerName:string='localhost';Port:integer=27017);
    procedure Close;

    function Get(
      Collection:WideString;
      QryObj:IBSONDocument;
      ReturnFieldSelector:IBSONDocument=nil
    ):IBSONDocument;
    //Query: see TMongoWireQuery.Create
    procedure Update(
      Collection:WideString;
      Selector,Doc:IBSONDocument;
      Upsert:boolean=false;
      MultiUpdate:boolean=false
    );
    procedure Insert(
      Collection:WideString;
      Doc:IBSONDocument
    ); overload;
    procedure Insert(
      Collection:WideString;
      const Docs:array of IBSONDocument
    ); overload;
    procedure Insert(
      Collection:WideString;
      Docs:TBSONDocumentsEnumerator
    ); overload;
    procedure Delete(
      Collection:WideString;
      Selector:IBSONDocument;
      SingleRemove:boolean=false
    );
  end;

  TMongoWireQuery=class(TBSONDocumentsEnumerator)
  private
    FOwner:TMongoWire;
    FData:TStreamAdapter;
    FNumberToReturn,FNumberToSkip,FPageIndex,FNumberReturned:integer;
    FCollection:WideString;
    FCursorID:int64;
    procedure KillCursor;
    procedure ReadResponse(RequestID: integer);
  public
    constructor Create(MongoWire:TMongoWire);
    destructor Destroy; override;
    procedure Query(
      Collection:WideString;
      QryObj:IBSONDocument;
      ReturnFieldSelector:IBSONDocument=nil;
      Flags:integer=0);
    function Next(Doc:IBSONDocument):boolean; override;
    property NumberToReturn:integer read FNumberToReturn write FNumberToReturn;
    property NumberToSkip:integer read FNumberToSkip write FNumberToSkip;//TODO: set?

    function Explain(Collection: WideString; Query,OrderBy: IBSONDocument): IBSONDocument;
  end;

  //TODO: TBSONDocumentsFromVariantArray=class(TBSONDocumentsEnumerator)

  EMongoException=class(Exception);
  EMongoConnectFailed=class(EMongoException);
  EMongoNotConnected=class(EMongoException);
  EMongoTransferError=class(EMongoException);
  EMongoQueryError=class(EMongoException);

const
  mongoWire_QueryFlag_TailableCursor  = $0002;
  mongoWire_QueryFlag_SlaveOk	        = $0004;
  mongoWire_QueryFlag_OplogReplay	    = $0008;
  mongoWire_QueryFlag_NoCursorTimeout = $0010;
  mongoWire_QueryFlag_AwaitData       = $0020;
  mongoWire_QueryFlag_Exhaust         = $0040;

implementation

uses ActiveX, Variants;

const
  OP_REPLY        = 1;
  OP_MSG          = 1000;
  OP_UPDATE       = 2001;
  OP_INSERT       = 2002;
  OP_QUERY        = 2004;
  OP_GET_MORE     = 2005;
  OP_DELETE       = 2006;
  OP_KILL_CURSORS = 2007;

  MongoWideStartDataSize=$10000;

type
  TMongoWireMsgHeader=packed record
    MsgLength:integer;
    RequestID:integer;
    ResponseTo:integer;
    OpCode:integer;
    Flags:integer;
    //for server response only:
    CursorID:int64;
    StartingFrom:integer;
    NumberReturned:integer;
  end;
  PMongoWireMsgHeader=^TMongoWireMsgHeader;

{ TMongoWire }

constructor TMongoWire.Create;
var
  m:TMemoryStream;
begin
  inherited Create;
  FSocket:=TTcpClient.Create(nil);
  m:=TMemoryStream.Create;
  m.Size:=MongoWideStartDataSize;//start keeping some data
  FData:=TStreamAdapter.Create(m,soOwned);
  FWriteLock:=TCriticalSection.Create;
  FReadLock:=TCriticalSection.Create;
  FRequestIndex:=1;
end;

destructor TMongoWire.Destroy;
begin
  FSocket.Free;
  FData.Free;
  FWriteLock.Free;
  FReadLock.Free;
  inherited;
end;

procedure TMongoWire.Open(ServerName: string; Port: integer);
begin
  FSocket.Close;
  FSocket.RemoteHost:=ServerName;
  FSocket.RemotePort:=IntToStr(Port);
  FSocket.Open;
  if not FSocket.Connected then
    raise EMongoConnectFailed.Create('MongoWire: failed to connect to "'+ServerName+':'+IntToStr(Port)+'"');
end;

procedure TMongoWire.Close;
begin
  FSocket.Close;
end;

procedure TMongoWire.DataCString(x:WideString);
var
  s:UTF8String;
  l:integer;
begin
  s:=UTF8Encode(x);
  l:=Length(s);
  if l=0 then
    FData.Stream.Write(l,1)
  else
    FData.Stream.Write(s[1],l+1);
end;

procedure TMongoWire.OpenMsg(OpCode,Flags:integer;Collection:WideString);
var
  p:PMongoWireMsgHeader;
begin
  //assert caller did FQueueLock !!!
  if not FSocket.Connected then
    raise EMongoNotConnected.Create('MongoWire: not connected');
  //message header
  p:=(FData.Stream as TMemoryStream).Memory;
  //p.RequestID:=//see CloseMsg (within lock)
  p.ResponseTo:=0;
  p.OpCode:=OpCode;
  p.Flags:=Flags;
  FData.Stream.Position:=20;//SizeOf first part of TMongoWireMsgHeader
  if OpCode<>OP_KILL_CURSORS then DataCString(Collection);
end;

function TMongoWire.CloseMsg(Data:TStreamAdapter):integer;
var
  i,r,l:integer;
  p:PMongoWireMsgHeader;
begin
  ///ATTENTION: please resist the temptation to put CloseMsg in a try/finally:
  /// this would cause incomplete requests to be sent to the server!

  //assert caller did FQueueLock !!!

  //get next RequestID
  r:=FRequestIndex;
  inc(FRequestIndex);
  if FRequestIndex<0 then FRequestIndex:=1;

  //add to queue
  if Data<>nil then
   begin
    i:=0;
    l:=Length(FQueue);
    while (i<FQueueIndex) and (FQueue[i].RequestID=0) do inc(i);
    if i=FQueueIndex then
     begin
      if l=FQueueIndex then SetLength(FQueue,l+4);//grow in steps
      inc(FQueueIndex);
     end;
    FQueue[i].RequestID:=r;
    FQueue[i].Data:=Data;
   end;

  //fill in MsgLength, RequestID
  i:=FData.Stream.Position;
  p:=(FData.Stream as TMemoryStream).Memory;
  p.RequestID:=r;
  p.MsgLength:=i;

  //send data
  FSocket.SendBuf((FData.Stream as TMemoryStream).Memory^,i);

  Result:=r;
end;

procedure TMongoWire.ReadMsg(RequestID:integer);
const
  dSize=$10000;
var
  i,l:integer;
  h:array[0..2] of integer;
  d:array[0..dSize-1] of byte;
  dx:TMemoryStream;
begin
  //TODO: timeout?
  //TODO: retries?
  FReadLock.Enter;
  try
    //is the request already in?
    i:=0;
    while (i<FQueueIndex) and (FQueue[i].RequestID<>RequestID) do inc(i);
    if i=FQueueIndex then
      raise Exception.Create('MongoWire: ReadMsg called on not queued requestID');
    if FQueue[i].Data=nil then
     begin
      //yes, it's in, release this spot on the queue
      //FQueueLock? assert requestID is unique!
      FQueue[i].RequestID:=0;
     end
    else
     begin
      //nope, read response(s)
      repeat
        //MsgLength,RequestID,ResponseTo
        i:=FSocket.ReceiveBuf(h[0],12);
        if i<>12 then raise EMongoException.Create('MongoWire: invalid response');

        //find request on queue
        i:=0;
        while (i<FQueueIndex) and (FQueue[i].RequestID<>h[2]) do inc(i);
        if i=FQueueIndex then
         begin
          //odd, not on queue, read to flush,
          l:=h[0]-12;
          while (l>0) and (i<>0) do
           begin
            if l<dSize then i:=l else i:=dSize;
            i:=FSocket.ReceiveBuf(d[0],i);
            dec(l,i);
           end;
          //then raise
          raise EMongoTransferError.Create('MongoWire: unexpected response, requestID:'+IntToStr(h[2]));
         end
        else
         begin
          //queue found, flag done
          //FQueueLock? assert requestID is unique!
          dx:=FQueue[i].Data.Stream as TMemoryStream;
          FQueue[i].Data:=nil;
          //is this the request we're waiting for? then release spot on queue
          if h[2]=RequestID then FQueue[i].RequestID:=0;
          //forward start of header
          dx.Position:=0;
          dx.Write(h[0],12);
          //TODO: check p.OpCode=OP_REPLY!!!
          //read data
          dx.Position:=12;
          l:=h[0]-12;
          while l>0 do
           begin
            if l<dSize then i:=l else i:=dSize;
            i:=FSocket.ReceiveBuf(d[0],i);
            if i=0 then raise EMongoException.Create('MongoWire: response aborted');
            dx.Write(d[0],i);
            dec(l,i);
           end;
          //set position after message header
          if h[0]<36 then dx.Position:=h[0] else dx.Position:=36;
         end;
      until h[2]=RequestID;
      //request is in!
     end;
  finally
    FReadLock.Leave;
  end;
end;

function TMongoWire.Get(Collection: WideString; QryObj,
  ReturnFieldSelector: IBSONDocument): IBSONDocument;
var
  i:integer;
  p:PMongoWireMsgHeader;
begin
  FWriteLock.Enter;
  try
    OpenMsg(OP_QUERY,0,Collection);
    i:=0;
    FData.Stream.Write(i,4);//NumberToSkip
    i:=1;//-1;
    FData.Stream.Write(i,4);//NumberToReturn
    if QryObj=nil then
     begin
      i:=5;//empty document
      FData.Stream.Write(i,4);
      i:=0;//terminator
      FData.Stream.Write(i,1);
     end
    else
      (QryObj as IPersistStream).Save(FData,false);
    if ReturnFieldSelector<>nil then (ReturnFieldSelector as IPersistStream).Save(FData,false);
    ReadMsg(CloseMsg(FData));

    p:=(FData.Stream as TMemoryStream).Memory;
    if (p.Flags and $0001)<>0 then
      raise EMongoQueryError.Create('MongoWire.Get: cursor not found');

    //CursorID//assert 0
    //StartingFrom//assert 0
    if p.NumberReturned=0 then
      raise EMongoQueryError.Create('MongoWire.Get: no documents returned');
      //Result:=nil?

    Result:=TBSONDocument.Create;
    (Result as IPersistStream).Load(FData);

    if (p.Flags and $0002)<>0 then
      raise EMongoQueryError.Create('MongoWire.Get: '+VarToStr(Result.Item['$err']));

  finally
    FWriteLock.Leave;
  end;
end;

procedure TMongoWire.Update(Collection: WideString; Selector,
  Doc: IBSONDocument; Upsert, MultiUpdate: boolean);
var
  i:integer;
begin
  if Doc=nil then raise EMongoException.Create('MongoWire.Update: Doc required');
  FWriteLock.Enter;
  try
    OpenMsg(OP_UPDATE,0,Collection);
    i:=0;
    if Upsert then inc(i,$0001);
    if MultiUpdate then inc(i,$0002);
    FData.Stream.Write(i,4);
    if Selector=nil then
     begin
      i:=5;//empty document
      FData.Stream.Write(i,4);
      i:=0;//terminator
      FData.Stream.Write(i,1);
     end
    else
      (Selector as IPersistStream).Save(FData,false);
    (Doc as IPersistStream).Save(FData,false);
    CloseMsg;
  finally
    FWriteLock.Leave;
  end;
end;

procedure TMongoWire.Insert(Collection: WideString; Doc: IBSONDocument);
begin
  if Doc=nil then raise EMongoException.Create('MongoWire.Insert: Doc required');
  FWriteLock.Enter;
  try
    OpenMsg(OP_INSERT,0,Collection);
    (Doc as IPersistStream).Save(FData,false);
    CloseMsg;
  finally
    FWriteLock.Leave;
  end;
end;

procedure TMongoWire.Insert(Collection: WideString;
  const Docs: array of IBSONDocument);
var
  i:integer;
begin
  FWriteLock.Enter;
  try
    OpenMsg(OP_INSERT,0,Collection);
    for i:=0 to Length(Docs)-1 do (Docs[i] as IPersistStream).Save(FData,false);
    CloseMsg;
  finally
    FWriteLock.Leave;
  end;
end;

procedure TMongoWire.Insert(Collection: WideString; Docs: TBSONDocumentsEnumerator);
var
  d:IBSONDocument;
begin
  FWriteLock.Enter;
  try
    OpenMsg(OP_INSERT,0,Collection);
    d:=TBSONDocument.Create as IBSONDocument;
    while Docs.Next(d) do (d as IPersistStream).Save(FData,false);
    CloseMsg;
  finally
    FWriteLock.Leave;
  end;
end;

procedure TMongoWire.Delete(Collection: WideString;
  Selector: IBSONDocument; SingleRemove: boolean);
var
  i:integer;
begin
  FWriteLock.Enter;
  try
    OpenMsg(OP_DELETE,0,Collection);
    i:=0;
    if SingleRemove then inc(i,$0001);
    FData.Stream.Write(i,4);
    if Selector=nil then
     begin
      //raise? warning?
      i:=5;//empty document
      FData.Stream.Write(i,4);
      i:=0;//terminator
      FData.Stream.Write(i,1);
     end
    else
      (Selector as IPersistStream).Save(FData,false);
    CloseMsg;
  finally
    FWriteLock.Leave;
  end;
end;

{ TMongoWireQuery }

constructor TMongoWireQuery.Create(MongoWire: TMongoWire);
var
  m:TMemoryStream;
begin
  inherited Create;
  FOwner:=MongoWire;
  //TODO: register for invalidation on owner's TMongoWire.Destroy
  m:=TMemoryStream.Create;
  m.Size:=MongoWideStartDataSize;//start keeping some data
  FData:=TStreamAdapter.Create(m,soOwned);
  FNumberToReturn:=0;//use db default
  FNumberToSkip:=0;
  FPageIndex:=0;
  FNumberReturned:=0;
  FCursorID:=0;
end;

destructor TMongoWireQuery.Destroy;
begin
  KillCursor;
  FOwner:=nil;//TODO: unregister
  FData.Free;
  inherited;
end;

procedure TMongoWireQuery.KillCursor;
var
  i:integer;
begin
  if FCursorID<>0 then
   begin
    FOwner.FReadLock.Enter;
    try
      FOwner.OpenMsg(OP_KILL_CURSORS,0,'');
      i:=1;
      FOwner.FData.Stream.Write(i,4);//just the one
      FOwner.FData.Stream.Write(FCursorID,8);
      FOwner.CloseMsg(nil);
    finally
      FOwner.FReadLock.Leave;
    end;
    FCursorID:=0;
   end;
end;

procedure TMongoWireQuery.Query(Collection: WideString; QryObj,
  ReturnFieldSelector: IBSONDocument; Flags: integer);
var
  i:integer;
begin
  KillCursor;
  FOwner.FReadLock.Enter;
  try
    FOwner.OpenMsg(OP_QUERY,Flags,Collection);
    FOwner.FData.Stream.Write(FNumberToSkip,4);
    FOwner.FData.Stream.Write(FNumberToReturn,4);
    if QryObj=nil then
     begin
      i:=5;//empty document
      FOwner.FData.Stream.Write(i,4);
      i:=0;//terminator
      FOwner.FData.Stream.Write(i,1);
     end
    else
      (QryObj as IPersistStream).Save(FOwner.FData,false);
    if ReturnFieldSelector<>nil then
      (ReturnFieldSelector as IPersistStream).Save(FOwner.FData,false);
    i:=FOwner.CloseMsg(FData);//queue self's data
  finally
    FOwner.FReadLock.Leave;
  end;
  ReadResponse(i);
  FCollection:=Collection;
end;

function TMongoWireQuery.Next(Doc: IBSONDocument): boolean;
var
  i:integer;
begin
  if Doc=nil then EMongoException.Create('MongoWireQuery.Next: Doc required');
  if FPageIndex=FNumberReturned then
   begin
    if FCursorID=0 then
      Result:=false
    else
     begin
      //get more
      FOwner.FReadLock.Enter;
      try
        FOwner.OpenMsg(OP_GET_MORE,0,FCollection);
        FOwner.FData.Stream.Write(FNumberToReturn,4);
        FOwner.FData.Stream.Write(FCursorID,8);
        i:=FOwner.CloseMsg(FData);//queue self's data
      finally
        FOwner.FReadLock.Leave;
      end;
      ReadResponse(i);
      Result:=FNumberReturned<>0;
     end;
   end
  else
    Result:=true;
  if Result then
   begin
    //assert(FData.Position<PMongoWireMsgHeader((FData.Stream as TMemoryStream).Memory)^.MsgLength);
    inc(FNumberToSkip);
    inc(FPageIndex);
    Doc.Clear;//?
    (Doc as IPersistStream).Load(FData);
   end;
end;

procedure TMongoWireQuery.ReadResponse(RequestID:integer);
var
  i:integer;
  d:IBSONDocument;
  p:PMongoWireMsgHeader;
begin
  FOwner.ReadMsg(RequestID);
  p:=(FData.Stream as TMemoryStream).Memory;
  i:=p.Flags;
  //assert StartingFrom=0
  FCursorID:=p.CursorID;
  FPageIndex:=0;
  FNumberReturned:=p.NumberReturned;
  if (i and $0001)<>0 then raise EMongoQueryError.Create('MongoWire.Query: cursor not found');
  if (i and $0002)<>0 then //query failure
   begin
    //FCursorID:=0;//?
    FNumberReturned:=0;
    d:=TBSONDocument.Create as IBSONDocument;
    (d as IPersistStream).Load(FData);
    raise EMongoQueryError.Create('MongoWire.Query: '+VarToStr(d['$err']));
   end;
end;

function TMongoWireQuery.Explain(Collection: WideString; Query,OrderBy: IBSONDocument): IBSONDocument;
var
  vQuery: IBSONDocument;
begin
  vQuery := BSON;
  vQuery.Item['query'] := Query;

  if Assigned(OrderBy) then
  begin
    vQuery.Item['orderby'] := OrderBy;
  end;

  vQuery.Item['$explain'] := True;

  Self.Query(Collection, vQuery);

  Result := BSON;
  
  Self.Next(Result);
end;


end.
