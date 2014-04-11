{

TMongoWire: mongoStream.pas

Copyright 2010-2014 Stijn Sanders
Made available under terms described in file "LICENSE"
https://github.com/stijnsanders/TMongoWire

}
unit mongoStream;

interface

uses Classes, mongoWire, bsonDoc;

type
  TMongoStream = class(TStream)
  private
    Fdb:TMongoWire;
    Fprefix:WideString;
    Fdata,Fchunk:IBSONDocument;
    Fsize,FchunkSize,FchunkIndex,FchunkPos:integer;
    procedure InitData;
  public
    constructor Create(db:TMongoWire;const prefix:WideString;id:OleVariant); overload;
    constructor Create(db:TMongoWire;const prefix:WideString;filequery:IBSONDocument); overload;
    class function Add(db:TMongoWire;prefix:WideString;stream:TStream;info:IBSONDocument):OleVariant; overload;
    class function Add(db:TMongoWire;const prefix,filepath:WideString):OleVariant; overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
  end;

const
  //do not localize
  mongoStreamFilesSuffix='.files';
  mongoStreamChunksSuffix='.chunks';
  mongoStreamIDField='_id';
  mongoStreamLengthField='length';
  mongoStreamChunkSizeField='chunkSize';
  mongoStreamFilesIDField='files_id';
  mongoStreamNField='n';
  mongoStreamDataField='data';
  mongoStreamFileNameField='filename';
  mongoStreamUploadDateField='uploadDate';

implementation

uses SysUtils, Variants, mongoID;

const
  //do not localize
  mongoStreamDefaultPrefix='fs';
  mongoStreamDefaultChunkSize=$40000;//256KB

function IsNull(x,def:OleVariant):OleVariant;
begin
  if VarIsNull(x) then Result:=def else Result:=x;
end;

{ TMongoStream }

constructor TMongoStream.Create(db: TMongoWire; const prefix: WideString;
  id: OleVariant);
begin
  inherited Create;
  Fdb:=db;//assert Fdb.Connected
  Fprefix:=prefix;
  if Fprefix='' then Fprefix:=mongoStreamDefaultPrefix;
  Fdata:=Fdb.Get(Fprefix+mongoStreamFilesSuffix,BSON([mongoStreamIDField,id]));
  InitData;
end;

constructor TMongoStream.Create(db: TMongoWire; const prefix: WideString;
  filequery: IBSONDocument);
begin
  inherited Create;
  Fdb:=db;//assert Fdb.Connected
  Fprefix:=prefix;
  if Fprefix='' then Fprefix:=mongoStreamDefaultPrefix;
  Fdata:=Fdb.Get(Fprefix+mongoStreamFilesSuffix,filequery);
  InitData;
end;

procedure TMongoStream.InitData;
begin
  Fsize:=Fdata[mongoStreamLengthField];
  FchunkSize:=IsNull(Fdata[mongoStreamChunkSizeField],mongoStreamDefaultChunkSize);
  FchunkIndex:=0;
  Fchunk:=nil;
  //TODO: detect any change to data?
end;

function TMongoStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  i:integer;
begin
  case Origin of
    soBeginning:Result:=Offset;
    soCurrent:Result:=FchunkIndex*FchunkSize+FchunkPos+Offset;
    soEnd:Result:=FSize-Offset;
    else raise Exception.Create('TMongoStream.Seek unsopprted offset parameter');
  end;
  i:=FchunkIndex;
  FchunkIndex:=Result div FchunkSize;
  FchunkPos:=Result mod FchunkSize;
  if FchunkIndex<>i then Fchunk:=nil;
end;

function TMongoStream.Read(var Buffer; Count: Integer): Longint;
var
  v:OleVariant;
  p:PAnsiChar;
begin
  if Fchunk=nil then Fchunk:=Fdb.Get(Fprefix+mongoStreamChunksSuffix,BSON([
    mongoStreamFilesIDField,Fdata[mongoStreamIDField],
    mongoStreamNField,FchunkIndex
  ]));
  if FchunkPos+Count>FchunkSize then Result:=FchunkSize-FchunkPos else Result:=Count;
  if FchunkIndex*FchunkSize+FchunkPos+Result>Fsize then Result:=Fsize-FchunkIndex*FchunkSize-FchunkPos;
  if Result<>0 then
   begin
    v:=Fchunk[mongoStreamDataField];//assert var array of byte
    p:=VarArrayLock(v);
    try
      Move(p[FchunkPos],Buffer,Result);
    finally
      VarArrayUnlock(v);
    end;
    inc(FchunkPos,Result);
    if FchunkPos>=FchunkSize then
     begin
      Fchunk:=nil;
      FchunkPos:=0;
      inc(FchunkIndex);
     end;
   end;
end;

procedure TMongoStream.SaveToFile(const FileName: string);
var
  f:TFileStream;
begin
  f:=TFileStream.Create(FileName,fmCreate);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

procedure TMongoStream.SaveToStream(Stream: TStream);
var
  i,s,p:integer;
  v:OleVariant;
  x:pointer;
begin
  p:=Fsize;
  for i:=0 to (Fsize div FchunkSize) do
   begin
    //TODO: reuse any Fchunk already fetched?
    if p<FchunkSize then s:=p else s:=FchunkSize;
    v:=Fdb.Get(Fprefix+mongoStreamChunksSuffix,BSON([
      mongoStreamFilesIDField,Fdata[mongoStreamIDField],
      mongoStreamNField,i
    ]))[mongoStreamDataField];
    x:=VarArrayLock(v);
    try
      Stream.Write(x^,s);
      //assert returns same value as s
    finally
      VarArrayUnlock(v);
    end;
    dec(p,s);
   end;
  //TODO: check md5 checksum?
end;

procedure TMongoStream.SetSize(NewSize: Integer);
begin
  //inherited;
  raise Exception.Create('TMongoStream is read-only');
end;

function TMongoStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('TMongoStream is read-only');
end;

class function TMongoStream.Add(db: TMongoWire; prefix: WideString;
  stream: TStream; info: IBSONDocument): OleVariant;
var
  i,l,chunkSize:integer;
  v:OleVariant;
  p:PAnsiChar;
begin
  if prefix='' then prefix:=mongoStreamDefaultPrefix;
  if stream.Size>$80000000 then raise Exception.Create('TMongoStream max 2GB supported');
  info[mongoStreamLengthField]:=integer(stream.Size);
  if VarIsNull(info[mongoStreamIDField]) then
   begin
    Result:=mongoObjectId;
    info[mongoStreamIDField]:=Result;
   end
  else
    Result:=info[mongoStreamIDField];
  if VarIsNull(info[mongoStreamChunkSizeField]) then
   begin
    chunkSize:=mongoStreamDefaultChunkSize;
    info[mongoStreamChunkSizeField]:=chunkSize;
   end
  else
    chunkSize:=info[mongoStreamChunkSizeField];
  info[mongoStreamUploadDateField]:=VarFromDateTime(Now);
  //TODO: 'md5'?
  //assert db.Connected
  db.Insert(prefix+mongoStreamFilesSuffix,info);
  stream.Position:=0;//?
  v:=VarArrayCreate([0,chunkSize-1],varByte);
  l:=chunkSize;
  i:=0;
  while l<>0 do
   begin
    p:=VarArrayLock(v);
    try
      l:=stream.Read(p^,chunkSize);
    finally
      VarArrayUnlock(v);
    end;
    //assert l=chunkSize
    if l<>0 then
     begin
      if l<>chunkSize then VarArrayRedim(v,l);//assert last read from stream!
      db.Insert(prefix+mongoStreamChunksSuffix,BSON([
        mongoStreamFilesIDField,Result,
        mongoStreamNField,i,
        mongoStreamDataField,v
      ]));
      inc(i);
     end;
   end;
end;

class function TMongoStream.Add(db: TMongoWire; const prefix,
  filepath: WideString): OleVariant;
var
  f:TFileStream;
begin
  f:=TFileStream.Create(filepath,fmOpenRead);
  try
    Result:=Add(db,prefix,f,BSON([
      mongoStreamFileNameField,ExtractFileName(filepath)
      //'contentType'?
      //'metadata'?
    ]));
  finally
    f.Free;
  end;
end;

end.
