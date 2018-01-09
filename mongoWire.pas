{

TMongoWire: mongoWire.pas

Copyright 2010-2018 Stijn Sanders
Made available under terms described in file "LICENSE"
https://github.com/stijnsanders/TMongoWire

v1.2.1

}
unit mongoWire;

{$D-}
{$L-}

interface

uses SysUtils, SyncObjs, Classes, simpleSock, jsonDoc, bsonTools;

type
  TMongoWire=class(TObject)
  private
    FSocket: TTcpSocket;
    FData: TMemoryStream;
    FNameSpace: WideString;
    FWriteLock, FReadLock: TCriticalSection;
    FQueue: array of record
      RequestID: integer;
      Data: TMemoryStream;
    end;
    FQueueIndex, FRequestIndex: integer;
    FWriteConcern: IJSONDocument;
    procedure DataCString(const x: WideString);
    procedure OpenMsg(OpCode, Flags: integer; const Collection: WideString);
    function CloseMsg(Data: TMemoryStream): integer;//RequestID
    procedure ReadMsg(RequestID: integer);
  public
    constructor Create(const NameSpace: Widestring);
    destructor Destroy; override;

    procedure Open(const ServerName: AnsiString = 'localhost';
      Port: integer = 27017);
    procedure OpenSecure(const ServerName: AnsiString = 'localhost';
      Port: integer = 27017);
    procedure Close;

    function Get(
      const Collection: WideString;
      const QryObj: IJSONDocument;
      const ReturnFieldSelector: IJSONDocument = nil
    ): IJSONDocument;
    //Query: see TMongoWireQuery.Create
    procedure Update(
      const Collection: WideString;
      const Selector, Doc: IJSONDocument;
      Upsert: boolean = false;
      MultiUpdate: boolean = false
    );
    procedure Insert(
      const Collection: WideString;
      const Doc: IJSONDocument
    ); overload;
    procedure Insert(
      const Collection: WideString;
      const Docs: array of IJSONDocument
    ); overload;
    procedure Insert(
      const Collection: WideString;
      const Docs: IJSONDocArray
    ); overload;
    procedure Delete(
      const Collection: WideString;
      const Selector: IJSONDocument;
      SingleRemove: boolean = false
    );
    function Ping: Boolean;
    procedure EnsureIndex(
      const Collection: WideString;
      const Index: IJSONDocument;
      const Options: IJSONDocument = nil
    );

    function RunCommand(
      const CmdObj: IJSONDocument
    ): IJSONDocument;

    function Count(const Collection: WideString): integer;
    function Distinct(const Collection, Key: WideString;
      const Query: IJSONDocument=nil): Variant;

    function Eval(const Collection, JSFn: WideString;
      const Args: array of Variant; NoLock: boolean=false):Variant;

    property NameSpace: WideString read FNameSpace write FNameSpace;
    property WriteConcern: IJSONDocument read FWriteConcern write FWriteConcern;
  end;

  TMongoWireQuery=class(TObject)
  private
    FOwner:TMongoWire;
    FData:TMemoryStream;
    FNumberToReturn,FNumberToSkip,FPageIndex,FNumberReturned:integer;
    FCollection:WideString;
    FCursorID:int64;
    procedure KillCursor;
    procedure ReadResponse(RequestID: integer);
  public
    constructor Create(MongoWire:TMongoWire);
    destructor Destroy; override;
    procedure Query(
      const Collection:WideString;
      const QryObj:IJSONDocument;
      const ReturnFieldSelector:IJSONDocument=nil;
      Flags:integer=0);
    function Next(const Doc:IJSONDocument):boolean;
    property NumberToReturn:integer read FNumberToReturn write FNumberToReturn;
    property NumberToSkip:integer read FNumberToSkip write FNumberToSkip;//TODO: set?
  end;

  //TODO: TBSONDocumentsFromVariantArray=class(TBSONDocumentsEnumerator)

  EMongoException=class(Exception);
  EMongoConnectFailed=class(EMongoException);
  EMongoNotConnected=class(EMongoException);
  EMongoTransferError=class(EMongoException);
  EMongoQueryError=class(EMongoException);
  EMongoCommandError=class(EMongoException);

const
  mongoWire_QueryFlag_TailableCursor  = $0002;
  mongoWire_QueryFlag_SlaveOk	        = $0004;
  mongoWire_QueryFlag_OplogReplay	    = $0008;
  mongoWire_QueryFlag_NoCursorTimeout = $0010;
  mongoWire_QueryFlag_AwaitData       = $0020;
  mongoWire_QueryFlag_Exhaust         = $0040;

  mongoWire_Db_SystemIndexCollection = 'system.indexes';

implementation

uses ActiveX, Variants, WinSock;

const
  OP_REPLY        = 1;
  OP_MSG          = 1000;
  //OP_UPDATE       = 2001;
  //OP_INSERT       = 2002;
  OP_QUERY        = 2004;
  OP_GET_MORE     = 2005;
  //OP_DELETE       = 2006;
  OP_KILL_CURSORS = 2007;

  MongoWireStartDataSize=$10000;

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

constructor TMongoWire.Create(const NameSpace: WideString);
begin
  inherited Create;
  FNameSpace:=NameSpace;
  FSocket:=nil;
  FData:=TMemoryStream.Create;
  FData.Size:=MongoWireStartDataSize;//start keeping some data
  FWriteLock:=TCriticalSection.Create;
  FReadLock:=TCriticalSection.Create;
  FRequestIndex:=1;
  FWriteConcern:=nil;//:=BSON(['w',1]);
end;

destructor TMongoWire.Destroy;
begin
  if FSocket<>nil then FSocket.Free;
  FData.Free;
  FWriteLock.Free;
  FReadLock.Free;
  FWriteConcern:=nil;
  inherited;
end;

procedure TMongoWire.Open(const ServerName: AnsiString; Port: integer);
begin
  if FSocket<>nil then
   begin
    FSocket.Disconnect;
    FSocket.Free;
   end;
  FSocket:=TTcpSocket.Create;
  FSocket.Connect(ServerName,Port);
  if not FSocket.Connected then
    raise EMongoConnectFailed.Create(
      'MongoWire: failed to connect to "'+string(ServerName)+':'+IntToStr(Port)+'"');
end;

procedure TMongoWire.OpenSecure(const ServerName: AnsiString; Port: integer);
begin
  if FSocket<>nil then
   begin
    FSocket.Disconnect;
    FSocket.Free;
   end;
  FSocket:=TTcpSecureSocket.Create;
  FSocket.Connect(ServerName,Port);
  if not FSocket.Connected then
    raise EMongoConnectFailed.Create(
      'MongoWire: failed to connect to "'+string(ServerName)+':'+IntToStr(Port)+'"');
end;

procedure TMongoWire.Close;
begin
  if FSocket<>nil then
    FSocket.Disconnect;
end;

procedure TMongoWire.DataCString(const x:WideString);
var
  s:UTF8String;
  l:integer;
begin
  s:=UTF8Encode(x);
  l:=Length(s);
  if l=0 then
    FData.Write(l,1)
  else
    FData.Write(s[1],l+1);
end;

procedure TMongoWire.OpenMsg(OpCode,Flags:integer;const Collection:WideString);
var
  p:PMongoWireMsgHeader;
begin
  //assert caller did FQueueLock !!!
  if (FSocket=nil) or not(FSocket.Connected) then
    raise EMongoNotConnected.Create('MongoWire: not connected');
  //message header
  p:=FData.Memory;
  //p.RequestID:=//see CloseMsg (within lock)
  p.ResponseTo:=0;
  p.OpCode:=OpCode;
  p.Flags:=Flags;
  FData.Position:=20;//SizeOf first part of TMongoWireMsgHeader
  if OpCode<>OP_KILL_CURSORS then
    DataCString(FNameSpace+'.'+Collection);
end;

function TMongoWire.CloseMsg(Data: TMemoryStream): integer;
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
    while (i<FQueueIndex) and (FQueue[i].RequestID<>0) do inc(i);
    if i=FQueueIndex then
     begin
      if l=FQueueIndex then SetLength(FQueue,l+4);//grow in steps
      inc(FQueueIndex);
     end;
    FQueue[i].RequestID:=r;
    FQueue[i].Data:=Data;
   end;

  //fill in MsgLength, RequestID
  i:=FData.Position;
  p:=FData.Memory;
  p.RequestID:=r;
  p.MsgLength:=i;

  //send data
  FSocket.SendBuf(FData.Memory^,i);

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
      raise EMongoException.Create('MongoWire: ReadMsg called on unknown requestID');
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
          dx:=FQueue[i].Data;
          //release spot on queue
          FQueue[i].Data:=nil;
          FQueue[i].RequestID:=0;
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

function TMongoWire.Get(const Collection: WideString; const QryObj,
  ReturnFieldSelector: IJSONDocument): IJSONDocument;
var
  i:integer;
  p:PMongoWireMsgHeader;
begin
  FWriteLock.Enter;
  try
    OpenMsg(OP_QUERY,0,Collection);
    i:=0;
    FData.Write(i,4);//NumberToSkip
    i:=1;//-1;
    FData.Write(i,4);//NumberToReturn
    if QryObj=nil then
     begin
      i:=5;//empty document
      FData.Write(i,4);
      i:=0;//terminator
      FData.Write(i,1);
     end
    else
      SaveBSON(QryObj,FData);
    if ReturnFieldSelector<>nil then
      SaveBSON(ReturnFieldSelector,FData);
    ReadMsg(CloseMsg(FData));

    p:=FData.Memory;
    if (p.Flags and $0001)<>0 then
      raise EMongoQueryError.Create('MongoWire.Get: cursor not found');

    //CursorID//assert 0
    //StartingFrom//assert 0
    if p.NumberReturned=0 then
      raise EMongoQueryError.Create('MongoWire.Get: no documents returned');
      //Result:=nil?

    Result:=JSON;
    LoadBSON(FData,Result);

    if (p.Flags and $0002)<>0 then
      raise EMongoQueryError.Create('MongoWire.Get: '+VarToStr(Result.Item['$err']));

  finally
    FWriteLock.Leave;
  end;
end;

procedure TMongoWire.Update(const Collection: WideString; const Selector,
  Doc: IJSONDocument; Upsert, MultiUpdate: boolean);
var
  d:IJSONDocument;
begin
  if Doc=nil then
    raise EMongoException.Create('MongoWire.Update: Doc required');
  d:=JSON(
    ['q',Selector
    ,'u',Doc
    ]);
  if Upsert then d['upsert']:=true;
  if MultiUpdate then d['multi']:=true;
  RunCommand(JSON(
    ['update',Collection
    ,'updates',ja([d])
    ,'writeConcern',FWriteConcern
    ]));
end;

procedure TMongoWire.Insert(const Collection: WideString;
  const Doc: IJSONDocument);
begin
  if Doc=nil then
    raise EMongoException.Create('MongoWire.Insert: Doc required');
  RunCommand(JSON(
    ['insert',Collection
    ,'documents',ja([Doc])
    ,'writeConcern',FWriteConcern
    ]));
end;

procedure TMongoWire.Insert(const Collection: WideString;
  const Docs: array of IJSONDocument);
var
  i,l:integer;
  v:Variant;
begin
  l:=Length(Docs);
  v:=VarArrayCreate([0,l-1],varUnknown);
  for i:=0 to l-1 do v[i]:=Docs[i];
  RunCommand(JSON(
    ['insert',Collection
    ,'documents',v
    ,'writeConcern',FWriteConcern
    ]));
end;

procedure TMongoWire.Insert(const Collection: WideString;
  const Docs: IJSONDocArray);
var
  d,dx:IJSONDocument;
  i:integer;
begin
  d:=JSON;
  dx:=JSON(
    ['insert',Collection
    ,'documents',d
    ,'writeConcern',FWriteConcern
    ]);
  for i:=0 to Docs.Count-1 do
   begin
    Docs.LoadItem(i,d);
    RunCommand(dx);
   end;
end;

procedure TMongoWire.Delete(const Collection: WideString;
  const Selector: IJSONDocument; SingleRemove: boolean);
var
  l:integer;
begin
  if SingleRemove then l:=1 else l:=0;
  RunCommand(JSON(
    ['delete',Collection
    ,'deletes',ja([JSON(
      ['q',Selector
      ,'limit',l
      ])])
    ,'writeConcern',FWriteConcern
    ]));
end;

function TMongoWire.Ping: Boolean;
var
  ns:WideString;
begin
  ns:=FNameSpace;
  try
    FNameSpace:='admin';
    Result := Get('$cmd', JSON(['ping', 1]))['ok'] = 1;
  except
    Result := False;
  end;
  FNameSpace:=ns;
end;

procedure TMongoWire.EnsureIndex(const Collection: WideString;
  const Index, Options: IJSONDocument);
var
  Document: IJSONDocument;
  Name: String;
  I: Integer;
  IndexArray: Variant;
begin
  Document := JSON([
    'ns', FNameSpace + '.' + Collection,
    'key', Index
  ]);
  
  if (Options = nil) or (Options['name'] = Null) then begin
    Name := '';
    IndexArray := Index.ToVarArray;
    for I := VarArrayLowBound(IndexArray, 1) to VarArrayHighBound(IndexArray, 1) do
      Name := Name + VarToStr(VarArrayGet(IndexArray, [I, 0])) + '_' + VarToStr(VarArrayGet(IndexArray, [I, 1]));

    if Length(FNameSpace + '.' + Collection + '.' + Name) > 128 then
      raise Exception.Create('Index name too long, please specify a custom name using the name option.');

    Document['name'] := Name;
  end else
    Document['name'] := Options['name'];

  if Options <> nil then begin
    if Options['background'] <> Null then
      Document['background'] := Options['background'];
    if Options['dropDups'] <> Null then
      Document['dropDups'] := Options['dropDups'];
    if Options['sparse'] <> Null then
      Document['sparse'] := Options['sparse'];
    if Options['unique'] <> Null then
      Document['unique'] := Options['unique'];
    if Options['v'] <> Null then
      Document['v'] := Options['v'];
  end;

  Insert(mongoWire_Db_SystemIndexCollection, Document);
end;

function TMongoWire.RunCommand(const CmdObj: IJSONDocument): IJSONDocument;
begin
  Result:=Get('$cmd',CmdObj);
  if Result['ok']<>1 then
    try
      if VarIsNull(Result['errmsg']) then
        raise EMongoCommandError.Create('Unspecified error with "'+
          VarToStr(CmdObj.ToVarArray[0,0])+'"')
      else
        raise EMongoCommandError.Create('Command "'+
          VarToStr(CmdObj.ToVarArray[0,0])+'" failed: '+
          VarToStr(Result['errmsg']));
    except
      on EMongoCommandError do
        raise;
      on Exception do
        raise EMongoCommandError.Create('Command failed');
    end;
end;

function TMongoWire.Count(const Collection: WideString): integer;
begin
  Result:=RunCommand(JSON(['count',Collection]))['n'];
end;

function TMongoWire.Distinct(const Collection, Key: WideString;
  const Query: IJSONDocument): Variant;
var
  d:IJSONDocument;
begin
  d:=JSON(['distinct',Collection,'key',Key]);
  if Query<>nil then d['query']:=Query;
  Result:=RunCommand(d)['values'];
end;

function TMongoWire.Eval(const Collection, JSFn: WideString;
  const Args: array of Variant; NoLock: boolean): Variant;
begin
  Result:=RunCommand(JSON(['eval',bsonJavaScriptCodePrefix+
    JSFn,'args',ja(Args)]))['retval'];
end;

{ TMongoWireQuery }

constructor TMongoWireQuery.Create(MongoWire: TMongoWire);
begin
  inherited Create;
  FOwner:=MongoWire;
  //TODO: register for invalidation on owner's TMongoWire.Destroy
  FData:=TMemoryStream.Create;
  FData.Size:=MongoWireStartDataSize;//start keeping some data
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
      FOwner.FData.Write(i,4);//just the one
      FOwner.FData.Write(FCursorID,8);
      FOwner.CloseMsg(nil);
    finally
      FOwner.FReadLock.Leave;
    end;
    FCursorID:=0;
   end;
end;

procedure TMongoWireQuery.Query(const Collection: WideString; const QryObj,
  ReturnFieldSelector: IJSONDocument; Flags: integer);
var
  i:integer;
begin
  KillCursor;
  FOwner.FReadLock.Enter;
  try
    FOwner.OpenMsg(OP_QUERY,Flags,Collection);
    FOwner.FData.Write(FNumberToSkip,4);
    FOwner.FData.Write(FNumberToReturn,4);
    if QryObj=nil then
     begin
      i:=5;//empty document
      FOwner.FData.Write(i,4);
      i:=0;//terminator
      FOwner.FData.Write(i,1);
     end
    else
      SaveBSON(QryObj,FOwner.FData);
    if ReturnFieldSelector<>nil then
      SaveBSON(ReturnFieldSelector,FOwner.FData);
    i:=FOwner.CloseMsg(FData);//queue self's data
  finally
    FOwner.FReadLock.Leave;
  end;
  ReadResponse(i);
  FCollection:=Collection;
end;

function TMongoWireQuery.Next(const Doc: IJSONDocument): boolean;
var
  i:integer;
begin
  if Doc=nil then
    raise EMongoException.Create('MongoWireQuery.Next: Doc required');
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
        FOwner.FData.Write(FNumberToReturn,4);
        FOwner.FData.Write(FCursorID,8);
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
    LoadBSON(FData,Doc);
   end;
end;

procedure TMongoWireQuery.ReadResponse(RequestID:integer);
var
  i:integer;
  d:IJSONDocument;
  p:PMongoWireMsgHeader;
begin
  FOwner.ReadMsg(RequestID);
  p:=FData.Memory;
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
    d:=JSON;
    LoadBSON(FData,d);
    raise EMongoQueryError.Create('MongoWire.Query: '+VarToStr(d['$err']));
   end;
end;

end.
