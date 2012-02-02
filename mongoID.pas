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
const
  KEY_WOW64_64KEY=$0100;
var
  r:HKEY;
  s:string;
  i,l:integer;
begin
  //use cryptography machineguid, keep a local copy of this in initialization?
  l:=40;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,PChar('Software\Microsoft\Cryptography'),
    0,KEY_QUERY_VALUE,r)=ERROR_SUCCESS then
   begin
    SetLength(s,l);
    if RegQueryValue(r,'MachineGuid',PChar(s),l)=ERROR_SUCCESS then
     begin
      SetLength(s,l);
      RegCloseKey(r);
     end
    else
     begin
      //try from-32-to-64
      RegCloseKey(r);
      if RegOpenKeyEx(HKEY_LOCAL_MACHINE,PChar('Software\Microsoft\Cryptography'),
        0,KEY_QUERY_VALUE or KEY_WOW64_64KEY,r)=ERROR_SUCCESS then
       begin
        l:=40;
        if RegQueryValue(r,'MachineGuid',PChar(s),l)=ERROR_SUCCESS then
          SetLength(s,l)
        else
          l:=0;
        RegCloseKey(r);
       end;
     end;
   end;
  if l=36 then
    mongoObjectID_MachineID:=StrToInt('$'+Copy(s,1,6))
  else
   begin
    //render a number out of the host name
    s:=UpperCase(GetEnvironmentVariable('COMPUTERNAME'));
    mongoObjectID_MachineID:=$10101;
    for i:=1 to Length(s) do mongoObjectID_MachineID:=
      (mongoObjectID_MachineID*26+((byte(s[1])-$21) mod $39)) and $FFFFFF;
   end;
  mongoObjectID_Counter:=GetTickCount;//0?
end;

initialization
  InitMongoObjectID;
end.
