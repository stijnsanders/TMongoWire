{

TMongoWire: mongoWire.pas

Copyright 2010-2022 Stijn Sanders
Made available under terms described in file "LICENSE"
https://github.com/stijnsanders/TMongoWire

v2.0.0

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
  public
    constructor Create(const NameSpace: Widestring);
    destructor Destroy; override;

    procedure Open(const ServerName: AnsiString = 'localhost';
      Port: integer = 27017);
    procedure OpenSecure(const ServerName: AnsiString = 'localhost';
      Port: integer = 27017);
    procedure Close;

    function Msg(const MsgObj: IJSONDocument; Data: TMemoryStream;
      const LoadInto: IJSONDocument = nil): IJSONDocument;
    
    function Get(
      const Collection: WideString;
      const QryObj: IJSONDocument;
      const Projection: IJSONDocument = nil
    ): IJSONDocument;
    //Query: see TMongoWireQuery.Create
    function Update(
      const Collection: WideString;
      const Selector, Doc: IJSONDocument;
      Upsert: boolean = false;
      MultiUpdate: boolean = false
    ): IJSONDocument;
    function Insert(
      const Collection: WideString;
      const Doc: IJSONDocument
    ): IJSONDocument; overload;
    function Insert(
      const Collection: WideString;
      const Docs: array of IJSONDocument
    ): IJSONDocument; overload;
    procedure Insert(
      const Collection: WideString;
      const Docs: IJSONDocArray
    ); overload;
    function Delete(
      const Collection: WideString;
      const Selector: IJSONDocument;
      SingleRemove: boolean = false
    ): IJSONDocument;
    function Ping: Boolean;
    function EnsureIndex(
      const Collection: WideString;
      const Index: IJSONDocument;
      const Options: IJSONDocument = nil
    ): IJSONDocument;

    function Count(const Collection: WideString): integer;
    function Distinct(const Collection, Key: WideString;
      const Query: IJSONDocument=nil): Variant;

    property NameSpace: WideString read FNameSpace write FNameSpace;
    property WriteConcern: IJSONDocument read FWriteConcern write FWriteConcern;
  end;

  TMongoWireQuery=class(TObject)
  private
    FOwner:TMongoWire;
    FData:TMemoryStream;
    FNumberToReturn,FNumberToSkip:integer;
    FCollection:WideString;
    FCursorID:int64;
    FCursorData:IJSONDocument;
    FBatch:IJSONDocArray;
    FBatchIndex:integer;
    procedure KillCursor;
  public
    constructor Create(MongoWire:TMongoWire);
    destructor Destroy; override;
    procedure Query(
      const Collection:WideString;
      const QryObj:IJSONDocument;
      const Projection:IJSONDocument=nil;
      const Sort:IJSONDocument=nil;
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
  mongoWire_MsgFlag_ChecksumPresent = $00000001;
  mongoWire_MsgFlag_MoreToCome      = $00000002;
  mongoWire_MsgFlag_ExhaustAllowed  = $00010000;

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
  OP_COMPRESSED  = 2012;
  OP_MSG         = 2013;

  MongoWireStartDataSize=$10000;

type
  TMongoWireMsgHeader=packed record
    MsgLength:integer;
    RequestID:integer;
    ResponseTo:integer;
    OpCode:integer;
    Flags:integer;
  end;
  PMongoWireMsgHeader=^TMongoWireMsgHeader;

{ TMongoWire }

constructor TMongoWire.Create(const NameSpace: WideString);
begin
  inherited Create;
  FSocket:=nil;
  FData:=TMemoryStream.Create;
  FData.Size:=MongoWireStartDataSize;//start keeping some data
  FNameSpace:=NameSpace;
  FWriteLock:=TCriticalSection.Create;
  FReadLock:=TCriticalSection.Create;
  FRequestIndex:=1;
  FQueueIndex:=0;
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

function TMongoWire.Msg(const MsgObj: IJSONDocument; Data: TMemoryStream;
  const LoadInto: IJSONDocument = nil): IJSONDocument;
const
  dSize=$10000;
var
  p:PMongoWireMsgHeader;
  r,i,l:integer;
  h:TMongoWireMsgHeader;
  d:array[0..dSize-1] of byte;
  dx:TMemoryStream;
begin
  if (FSocket=nil) or not(FSocket.Connected) then
    raise EMongoNotConnected.Create('MongoWire: not connected');
  //assert Data<>nil

  FWriteLock.Enter;
  try

    //message header
    p:=FData.Memory;
    p.ResponseTo:=0;
    p.OpCode:=OP_MSG;
    p.Flags:=0;
    FData.Position:=20;//SizeOf first part of TMongoWireMsgHeader

    //kind:body
    i:=0;
    FData.Write(i,1);
    SaveBSON(MsgObj,FData);

    //update header
    r:=FRequestIndex;
    p.RequestID:=r;
    p.MsgLength:=FData.Position;

    inc(FRequestIndex);
    if FRequestIndex<0 then FRequestIndex:=1;

    //send request
    FSocket.SendBuf(FData.Memory^,FData.Position);

    //add to queue
    i:=0;
    l:=Length(FQueue);
    while (i<FQueueIndex) and (FQueue[i].RequestID<>0) do inc(i);
    if i=FQueueIndex then
     begin
      if l=FQueueIndex then SetLength(FQueue,l+4);//grow in steps
      inc(FQueueIndex);
     end;
    FQueue[i].RequestID:=r;
    if Data=nil then Data:=FData;//default to connection data cache
    FQueue[i].Data:=Data;

  finally
    FWriteLock.Leave;
  end;

  FReadLock.Enter;
  try
    //is the request already in?
    i:=0;
    while (i<FQueueIndex) and (FQueue[i].RequestID<>r) do inc(i);
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
        //read header
        i:=FSocket.ReceiveBuf(h,12);
        if i<>12 then raise EMongoException.Create('MongoWire: invalid response');

        //find request on queue
        i:=0;
        while (i<FQueueIndex) and (FQueue[i].RequestID<>h.ResponseTo) do inc(i);
        if i=FQueueIndex then
         begin
          //odd, not on queue, read to flush,
          l:=h.MsgLength-12;
          while (l>0) and (i<>0) do
           begin
            if l<dSize then i:=l else i:=dSize;
            i:=FSocket.ReceiveBuf(d[0],i);
            dec(l,i);
           end;
          //then raise
          raise EMongoTransferError.Create('MongoWire: unexpected response, requestID:'+IntToStr(h.ResponseTo));
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
          dx.Write(h,12);
          //read data
          //dx.Position:=12;
          l:=h.MsgLength-12;
          while l>0 do
           begin
            if l<dSize then i:=l else i:=dSize;
            i:=FSocket.ReceiveBuf(d[0],i);
            if i=0 then raise EMongoException.Create('MongoWire: response aborted');
            dx.Write(d[0],i);
            dec(l,i);
           end;

          //TODO:
          if (h.Flags and mongoWire_MsgFlag_MoreToCome)<>0 then
            raise Exception.Create('//TODO: support mongoWire_MsgFlag_MoreToCome');
          //if (h.Flags and mongoWire_MsgFlag_ChecksumPresent)<>0 then
          // TODO: check checksum

          //set position after message header
          //case h.OpCode of OP_MSG:?
          dx.Position:=21;
         end;
      until h.ResponseTo=r;
      //request is in!
     end;
  finally
    FReadLock.Leave;
  end;


  if LoadInto=nil then Result:=JSON else Result:=LoadInto;
  LoadBSON(Data,Result);

  if Result['ok']<>1 then
    try
      if VarIsNull(Result['errmsg']) then
        raise EMongoCommandError.Create('Unspecified error with "'+
          VarToStr(MsgObj.ToVarArray[0,0])+'"')
      else
        raise EMongoCommandError.Create('Command "'+
          VarToStr(MsgObj.ToVarArray[0,0])+'" failed: '+
          VarToStr(Result['errmsg']));
    except
      on EMongoCommandError do
        raise;
      on Exception do
        raise EMongoCommandError.Create('Command failed');
    end;
end;

function TMongoWire.Get(const Collection: WideString; const QryObj,
  Projection: IJSONDocument): IJSONDocument;
begin
  Result:=JSON(JSON(Msg(JSON(
    ['find',Collection
    ,'$db',FNameSpace
    ,'filter',QryObj
    ,'projection',Projection
    ,'limit',1
    ]),FData)['cursor'])['firstBatch'][0]);
end;

function TMongoWire.Update(const Collection: WideString; const Selector,
  Doc: IJSONDocument; Upsert, MultiUpdate: boolean): IJSONDocument;
begin
  if Doc=nil then
    raise EMongoException.Create('MongoWire.Update: Doc required');
  Result:=Msg(JSON(
    ['update',Collection
    ,'$db',FNameSpace
    ,'updates',VarArrayOf([JSON(
      ['q',Selector
      ,'u',Doc
      //,'c',
      ,'upsert',Upsert
      ,'multi',MultiUpdate
      //,'collation',
      //,'arrayFilters',
      //,'hint',
      ])])
    //,'ordered',
    ,'writeConcern',FWriteConcern
    //,'bypassDocumentValidation',
    //,'comment',
    //,'let ',
    ]),FData);
end;

function TMongoWire.Insert(const Collection: WideString;
  const Doc: IJSONDocument): IJSONDocument;
begin
  if Doc=nil then
    raise EMongoException.Create('MongoWire.Insert: Doc required');
  Result:=Msg(JSON(
    ['insert',Collection
    ,'$db',FNameSpace
    ,'documents',ja([Doc])
    ,'writeConcern',FWriteConcern
    //,'bypassDocumentValidation',
    //,'comment',
    ]),FData);
end;

function TMongoWire.Insert(const Collection: WideString;
  const Docs: array of IJSONDocument): IJSONDocument;
var
  i,l:integer;
  v:Variant;
begin
  l:=Length(Docs);
  v:=VarArrayCreate([0,l-1],varUnknown);
  for i:=0 to l-1 do v[i]:=Docs[i];
  Result:=Msg(JSON(
    ['insert',Collection
    ,'$db',FNameSpace
    ,'documents',v
    ,'writeConcern',FWriteConcern
    //,'bypassDocumentValidation',
    //,'comment',
    ]),FData);
end;

procedure TMongoWire.Insert(const Collection: WideString;
  const Docs: IJSONDocArray);
begin
  Msg(JSON(
    ['insert',Collection
    ,'$db',FNameSpace
    ,'documents',Docs
    ,'writeConcern',FWriteConcern
    //,'bypassDocumentValidation',
    //,'comment',
    ]),FData);
end;

function TMongoWire.Delete(const Collection: WideString;
  const Selector: IJSONDocument; SingleRemove: boolean): IJSONDocument;
var
  l:integer;
begin
  if SingleRemove then l:=1 else l:=0;
  Result:=Msg(JSON(
    ['delete',Collection
    ,'$db',FNameSpace
    ,'deletes',ja([JSON(
      ['q',Selector
      ,'limit',l
      //,'collation',
      //,'hint',
      ])])
    //,'comment',
    //,'let',
    //,'ordered',
    ,'writeConcern',FWriteConcern
    ]),FData);
end;

function TMongoWire.Ping: Boolean;
begin
  Result:=Msg(JSON(['ping',1,'$db','admin']),FData)['ok']=1;
end;

function TMongoWire.EnsureIndex(const Collection: WideString;
  const Index, Options: IJSONDocument): IJSONDocument;
var
  Document: IJSONDocument;
  Name: String;
  I: Integer;
  IndexArray: Variant;
begin
  Document := JSON(
    ['ns', FNameSpace + '.' + Collection
    ,'key', Index
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

  Result:=Insert(mongoWire_Db_SystemIndexCollection, Document);
end;

function TMongoWire.Count(const Collection: WideString): integer;
begin
  Result:=Msg(JSON(['count',Collection]),FData)['n'];
end;

function TMongoWire.Distinct(const Collection, Key: WideString;
  const Query: IJSONDocument): Variant;
begin
  Result:=Msg(JSON(
    ['distinct',Collection
    ,'$db',FNameSpace
    ,'key',Key
    ,'query',Query
    ]),FData)['values'];
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
  FCursorID:=0;
  FBatchIndex:=0;
end;

destructor TMongoWireQuery.Destroy;
begin
  KillCursor;
  FOwner:=nil;//TODO: unregister
  FBatch:=nil;
  FCursorData:=nil;
  FData.Free;
  inherited;
end;

procedure TMongoWireQuery.KillCursor;
begin
  if FCursorData<>nil then
   begin
    FOwner.Msg(JSON(
      ['killCursors',FCollection
      ,'$db',FOwner.NameSpace
      ,'cursors',VarArrayOf([FCursorID])
      ]),FData);
    FCursorData:=nil;
   end;
  FCursorID:=0;
end;

procedure TMongoWireQuery.Query(const Collection: WideString; const QryObj,
  Projection, Sort: IJSONDocument; Flags: integer);
var
  d,p:IJSONDocument;
begin
  KillCursor;
  FCollection:=Collection;
  d:=JSON(
    ['find',FCollection
    ,'$db',FOwner.NameSpace
    ,'filter',QryObj
    ,'sort',Sort
    ,'projection',Projection
    //,'hint'
    ]);
  if FNumberToSkip<>0 then d['skip']:=FNumberToSkip;
  if FNumberToReturn<>0 then d['limit']:=FNumberToReturn;
  //batchSize
  //singleBatch
  //comment
  //maxTimeMS
  //readConcern
  //max
  //returnKey
  //showRecordId
  //tailable
  //oplogReplay
  //noCursorTimeout
  //awaitData
  //allowPartialResults
  //allowDiskUse
  //left

  FBatch:=JSONDocArray;
  FBatchIndex:=0;
  FCursorData:=JSON(['firstBatch',FBatch]);
  p:=JSON(['cursor',FCursorData]);

  FOwner.Msg(d,FData,p);

  FCursorID:=FCursorData['id'];
end;

function TMongoWireQuery.Next(const Doc: IJSONDocument): boolean;
var
  p:IJSONDocument;
begin
  if Doc=nil then
    raise EMongoException.Create('MongoWireQuery.Next: Doc required');
  if FBatch=nil then
    Result:=false
  else
  if FBatchIndex>=FBatch.Count then
   begin
    if (FCursorID<>0) then //and (FCursorData['partialResultsReturned']=true) then
     begin
      FCursorData.Delete('firstBatch');
      FCursorData['nextBatch']:=FBatch;
      FBatchIndex:=0;
      p:=JSON(['cursor',FCursorData]);

      FOwner.Msg(JSON(
        ['getMore',FCursorID
        ,'collection',FCollection
        ,'$db',FOwner.NameSpace
        //,'batchSize',
        //,'maxTimeMS',
        //,'comment',
        ]),FData,p);

      FCursorID:=FCursorData['id'];
      Result:=FBatch.Count<>0;
     end
    else
      Result:=false;
   end
  else
    Result:=true;
  if Result then
   begin
    FBatch.LoadItem(FBatchIndex,Doc);
    inc(FBatchIndex);
   end;
end;

end.
