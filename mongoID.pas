unit mongoID;

interface

function mongoObjectId:string;

implementation

uses Windows, SysUtils, bsonDoc;

var
  mongoObjectID_MachineID,mongoObjectID_Counter:integer;

function mongoObjectId:string;
var
  st:TSystemTime;
  a,b,c,d:integer;
const
  hex:array[0..15] of char='0123456789abcdef';
begin
  //juse one way of generating mongoDB objectID's
  GetSystemTime(st);
  a:=(((Round(EncodeDate(st.wYear,st.wMonth,st.wDay))-UnixDateDelta)*24+st.wHour)*60+st.wMinute)*60+st.wSecond;
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
  s:string;
  i,l:integer;
begin
  //render a number out of the host name
  l:=MAX_PATH;
  SetLength(s,l);
  if GetComputerName(PChar(s),cardinal(l)) then SetLength(s,l) else
    s:=GetEnvironmentVariable('COMPUTERNAME');
  mongoObjectID_Counter:=GetTickCount;//0?
  mongoObjectID_MachineID:=$10101;
  for i:=1 to Length(s) do
    case s[i] of
      '0'..'9':
        mongoObjectID_MachineID:=(mongoObjectID_MachineID*36+
          (byte(s[i]) and $0F)) and $FFFFFF;
      'A'..'Z','a'..'z':
        mongoObjectID_MachineID:=(mongoObjectID_MachineID*36+
          (byte(s[i]) and $1F)+9) and $FFFFFF;
      //else ignore
    end;
end;

initialization
  InitMongoObjectID;
end.
