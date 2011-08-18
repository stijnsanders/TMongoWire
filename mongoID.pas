unit mongoID;

interface

function mongoObjectId:string;

implementation

uses Windows, SysUtils, bsonDoc, Registry;

var
  mongoObjectID_MachineID,mongoObjectID_Counter:integer;

function mongoObjectId:string;
var
  a,b,c,d:integer;
const
  hex:array[0..15] of char='0123456789abcdef';
begin
  //juse one way of generating mongoDB objectID's
  a:=Round((Now-UnixDateDelta)*SecsPerDay);
  b:=mongoObjectID_MachineID; //see initialization
  c:=GetCurrentThreadId;//GetCurrentProcessId;
  d:=InterlockedIncrement(mongoObjectID_Counter);
  Result:=
    bsonObjectIDPrefix+
    hex[(a shr 28) and $F]+hex[(a shr 24) and $F]+
    hex[(a shr 20) and $F]+hex[(a shr 16) and $F]+
    hex[(a shr 12) and $F]+hex[(a shr  8) and $F]+
    hex[(a shr  4) and $F]+hex[(a       ) and $F]+

    hex[(b shr 20) and $F]+hex[(b shr 16) and $F]+
    hex[(b shr 12) and $F]+hex[(b shr  8) and $F]+
    hex[(b shr  4) and $F]+hex[(b       ) and $F]+

    hex[(c shr 12) and $F]+hex[(c shr  8) and $F]+
    hex[(c shr  4) and $F]+hex[(c       ) and $F]+

    hex[(d shr 20) and $F]+hex[(d shr 16) and $F]+
    hex[(d shr 12) and $F]+hex[(d shr  8) and $F]+
    hex[(d shr  4) and $F]+hex[(d       ) and $F]+

    bsonObjectIDSuffix;
end;

procedure InitMongoObjectID;
var
  r:TRegistry;
  g:TGUID;
begin
  r:=TRegistry.Create;
  try
    //use cryptography machineguid, keep a local copy of this in initialization?
    r.RootKey:=HKEY_LOCAL_MACHINE;
    if r.OpenKeyReadOnly('\Software\Microsoft\Cryptography') then
      g:=StringToGUID('{'+r.ReadString('MachineGuid')+'}')
    else
      CreateGUID(g);//?
    mongoObjectID_MachineID:=g.D1 shr 8;
    mongoObjectID_Counter:=GetTickCount;//0?
  finally
    r.Free;
  end;
end;

initialization
  InitMongoObjectID;
end.
