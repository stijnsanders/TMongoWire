{

TMongoWire: bsonTools.pas

Copyright 2010-2018 Stijn Sanders
Made available under terms described in file "LICENSE"
https://github.com/stijnsanders/TMongoWire

v1.1.1

}
unit bsonTools;

{$WARN SYMBOL_PLATFORM OFF}
{$D-}
{$L-}

interface

uses
  ComObj, SysUtils, Classes, jsonDoc;

const
  //special prefix with unassigned(?) unicode symbols from the Special range
  bsonJavaScriptCodePrefix:WideString=#$FFF1'bsonJavaScriptCode'#$FFF2;
  bsonRegExPrefix:WideString=#$FFF1'bsonRegEx'#$FFF2; //see also $IFDEF BSON_SUPPORT_REGEX
  bsonObjectIDPrefix:WideString='ObjectID("';
  bsonObjectIDSuffix:WideString='")';
  bsonObjectID_L=36;//=Length(bsonObjectIDPrefix)+24+Length(bsonObjectIDSuffix);

  IID_IBSONDocArray
    : TGUID = '{42534F4E-0000-0003-C000-000000000003}';

type
  IBSONDocArray=interface(IUnknown)
    ['{42534F4E-0000-0003-C000-000000000003}']
    procedure ReadAll(Data: TStream); stdcall;
    procedure WriteAll(Data: TStream); stdcall;
  end;

  TBSONDocArray=class(TJSONImplBaseObj, IJSONArray, IJSONDocArray,
    IBSONDocArray)
  private
    FData:TStream;
    FRef:array of integer;//int64?
    FRefIndex,FRefLength:integer;
    FCurrent:Variant;
    function AddRef(FromSize: boolean): integer;
  protected
    //IJSONArray
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function Count: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function IJSONDocArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
    property Item[Idx: integer]: Variant read Get_Item write Set_Item; default;
    //IJSONDocArray
    function Add(const Doc: IJSONDocument): integer; stdcall;
    function AddJson(const Data: WideString): integer; stdcall;
    procedure LoadItem(Index: integer; const Doc: IJSONDocument); stdcall;
    procedure Clear; stdcall;
    function GetJSON(Index: integer): WideString; stdcall;
    //IBSONDocArray
    procedure ReadAll(Data: TStream); stdcall;
    procedure WriteAll(Data: TStream); stdcall;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

function BSONDocArray: IJSONDocArray;

procedure LoadBSON(Data: TStream; const Doc: IJSONDocument);
procedure SaveBSON(const Doc: IJSONDocument; Data: TStream);

type
  EBSONException=class(Exception);

implementation

uses
  Windows, ActiveX,
  {$IFDEF BSON_SUPPORT_REGEX}
  VBScript_RegExp_55_TLB,
  {$ENDIF}
  Variants;

const
  BSONArrayBaseIndex=0;//1?
  BSONDetectVarArrayType=true;

  //bsonElement
  bsonDouble = $00000001;
  bsonString = $00000002;
  bsonEmbeddedDocument = $00000003;
  bsonArray = $00000004;
  bsonBinary = $00000005;
  bsonObjectID = $00000007;
  bsonBoolean = $00000008;
  bsonUTCDateTime = $00000009;
  bsonNULL = $0000000A;
  bsonRegEx = $0000000B;
  bsonJavaScriptCode = $0000000D;
  bsonSymbol = $0000000E;
  bsonJavaScriptCodeWithScope = $0000000F;
  bsonInt32 = $00000010;
  bsonTimestamp = $00000011;
  bsonInt64 = $00000012;
  bsonMinKey = $FFFFFFFF;
  bsonMaxKey = $0000007F;

  //bsonBinarySubType
  bsonBinaryGeneric = $00000000;
  bsonBinaryFunction = $00000001;
  bsonBinaryOldBinary = $00000002;
  bsonBinaryUUID = $00000003;
  bsonBinaryMD5 = $00000005;
  bsonBinaryUserDefined = $00000006;

{$IF not Declared(UTF8ToWideString)}
function UTF8ToWideString(const s: UTF8String): WideString;
begin
  Result:=UTF8Decode(s);
end;
{$IFEND}

function BSONDocArray:IJSONDocArray;
begin
  Result:=TBSONDocArray.Create;
end;

procedure LoadBSON(Data: TStream; const Doc: IJSONDocument);
var
  i,lstart,ltotal,ltmax:integer;
  procedure dRead(p:pointer;s:integer);
  begin
    if Data.Read(p^,s)<>s then
      raise EBSONException.Create('Unexpected end of stream');
    inc(ltotal,s);
    if ltotal>ltmax then
      raise EBSONException.Create('More BSON data than declared');
  end;
var //outside of stmReadCString to recycle memory
  ss:AnsiString;
  sx,sl:integer;
  function dReadCString: WideString;
  var
    c:AnsiChar;
  begin
    sx:=0;
    dRead(@c,1);
    while c<>#0 do
     begin
      if sx=sl then
       begin
        inc(sl,$100);//grow in steps
        SetLength(ss,sl);
       end;
      inc(sx);
      ss[sx]:=c;
      dRead(@c,1);
     end;
    Result:=UTF8ToWideString(Copy(ss,1,sx));
  end;
  function dReadString: WideString;
  var
    l:integer;
    s:AnsiString;
  begin
    dRead(@l,4);
    if l<2 then s:='' else
     begin
      SetLength(s,l-1);
      dRead(@s[1],l-1);
     end;
    //read closing null
    l:=0;
    dRead(@l,1);
    if l<>0 then
      raise EBSONException.Create('BSON string incorrectly terminated at offset '+IntToHex(lstart,8));
    Result:=UTF8ToWideString(s);
  end;
  {$IFDEF BSON_SUPPORT_REGEX}
  function dReadRegEx: IRegExp2;
  var
    i:integer;
    s:string;
  begin
    Result:=CoRegExp.Create;
    Result.Pattern:=stmReadCString;
    s:=dReadCString;
    for i:=1 to Length(s)-1 do
      case s[i] of
        'i','I':Result.IgnoreCase:=true;
        'm','M':Result.Multiline:=true;
        'g','G':Result.Global:=true;
        //'x','X':;//verbose
        //'l','L':;//locale
        //'s','S':;//dotall
        //'u','U':;//unicode
        else raise EBSONException.Create('Unsupported regex option "'+s+'" at offset '+IntToHex(lstart,8));
      end;
  end;
  {$ENDIF}

const
  stackGrowStep=$20;//not too much, not too little (?)
  arrGrowStep=$20;
var
  IsArray:boolean;
  a1,ai,al:integer;
  key:WideString;
  d:IJSONDocument;
  a:array of Variant;
  at,vt:TVarType;
  procedure SetValue(const v:Variant);
  begin
    //asset da=nil
    if IsArray then
     begin
      if ai=al then
       begin
        inc(al,arrGrowStep);//not too much, not too little (?)
        SetLength(a,al);
       end;
      a[ai]:=v;
      //detect same type elements array
      vt:=TVarData(v).VType;
      if at=varEmpty then at:=vt else
        case at of
          //TODO: what with signed/unsigned mixed?
          varSmallint://i2
            if not(vt in [varSmallint,
              varShortInt,varByte]) then at:=varVariant;
          varInteger://i4
            if not(vt in [varSmallint,
              varInteger,varShortInt,varByte,varWord]) then at:=varVariant;
          varWord:
            if not(vt in [varSmallint,
              varByte,varWord]) then at:=varVariant;
          varLongWord:
            if not(vt in [varSmallint,
              varShortInt,varByte,varWord,varLongWord]) then at:=varVariant;
          varInt64:
            if not(vt in [varSmallint,varInteger,varShortInt,
              varByte,varWord,varLongWord,varInt64]) then at:=varVariant;
          varVariant:;//Already creating an VarArray of variants
          //TODO: more?
          else if at<>vt then at:=varVariant;
        end;
      inc(ai);
     end
    else
      d[key]:=v;//TODO: v0(key)?
  end;
var
  stack:array of record
    key:WideString;
    d:IJSONDocument;
    k1,k2:integer;
  end;
  stackIndex,stackSize:integer;
  procedure Push;
  begin
    if stackIndex=stackSize then
     begin
      inc(stackSize,stackGrowStep);
      SetLength(stack,stackSize);
     end;
    if IsArray then
     begin
      stack[stackIndex].d:=nil;
      stack[stackIndex].k1:=a1;
      stack[stackIndex].k2:=at;
     end
    else
     begin
      stack[stackIndex].key:=key;
      stack[stackIndex].d:=d;
     end;
    inc(stackIndex);
  end;
var
  d1:IJSONDocument;
  dr:IJSONDocWithReUse;
  da:IJSONDocArray;
  dz:IJSONDocument;
  d0:integer;
  ba:IBSONDocArray;
  aa:TJSONArray;
  v:Variant;
  j:integer;

  o:array[0..15] of byte;
  ii:int64 absolute o;
  dd:double absolute o;
  pp:pointer absolute o;
  gg:TGUID absolute o;
  pv:PVariant;
const
  hex:array[0..15] of char='0123456789abcdef';
begin
  //Doc.Clear;? let caller decide.

  ltotal:=0;
  lstart:=0;
  ltmax:=4;
  dRead(@ltMax,4);

  stackSize:=0;
  stackIndex:=0;
  ai:=0;
  a1:=0;
  al:=0;
  da:=nil;
  dz:=nil;
  d0:=0;
  IsArray:=false;
  d:=Doc;
  sl:=0;
  ss:='';
  while (ltotal<ltmax) and (stackIndex<>-1) do
   begin
    i:=0;
    lstart:=ltotal;
    dRead(@i,1);
    if i<>0 then key:=dReadCString;
    case i of
      bsonDouble:
       begin
        dRead(@dd,8);
        SetValue(dd);
       end;
      bsonString:
        SetValue(dReadString);
      bsonEmbeddedDocument:
       begin
        if IsArray then
          if (da<>nil) and (d0=stackIndex) then
            v:=dz //re-use keys
          else
           begin
            if ai=al then
             begin
              inc(al,arrGrowStep);
              SetLength(a,al);
             end;
            v:=JSON;
            a[ai]:=v;
            inc(ai);
           end
        else
         begin
          if d.QueryInterface(IID_IJSONDocWithReUse,dr)=S_OK then
           begin
            v:=dr.ReUse(key);
            dr:=nil;
           end
          else
            v:=Null;
          if (TVarData(v).VType in [varDispatch,varUnknown]) and
            (IUnknown(v).QueryInterface(IID_IJSONDocument,d1)=S_OK) then
            d1:=nil
          else
           begin
            v:=JSON;
            d[key]:=v;
           end;
         end;
        Push;
        IsArray:=false;
        d:=IUnknown(v) as IJSONDocument;
        dRead(@i,4);//document length
        //TODO: keep value and check if correct?
       end;
      bsonArray:
       begin
        if d.QueryInterface(IID_IJSONDocWithReUse,dr)=S_OK then
         begin
          v:=dr.ReUse(key);
          dr:=nil;
          if (da=nil) and (TVarData(v).VType in [varDispatch,varUnknown]) then
            if IUnknown(v).QueryInterface(IID_IBSONDocArray,ba)=S_OK then
             begin
              i:=Data.Position;
              ba.ReadAll(Data);
              i:=Data.Position-i;
              inc(ltotal,i-4);
              if ltotal>ltmax then
                raise EBSONException.Create('More BSON data than declared');
              i:=0;//skip push below
             end
            else if IUnknown(v).QueryInterface(IID_IJSONDocArray,da)=S_OK then
             begin
              d0:=stackIndex+1;
              if dz=nil then dz:=JSON;
             end;
         end;
        if i=bsonArray then
         begin
          Push;
          IsArray:=true;
          a1:=ai;
          at:=varEmpty;//used to detect same type elements array
          dRead(@i,4);//array document length
          //TODO: keep value and check if correct?
         end;
       end;
      bsonBinary:
       begin
        dRead(@i,4);
        dRead(@o[11],1);
        //TODO: store value somewhere?
        case o[11] of
          bsonBinaryGeneric,bsonBinaryOldBinary,bsonBinaryUserDefined:
           begin
            v:=VarArrayCreate([BSONArrayBaseIndex,i-1+BSONArrayBaseIndex],varByte);
            pp:=VarArrayLock(v);
            try
              dRead(pp,i);
            finally
              VarArrayUnlock(v);
            end;
            SetValue(v);
           end;
          bsonBinaryFunction:
           begin
            //TODO
            raise EInvalidOperation.Create('bsonBinaryFunction: Not Implemented');
           end;
          bsonBinaryUUID:
           begin
            if i<>16 then
              raise EBSONException.Create('Unexpected UUID length ('+
                IntToStr(i)+') at offset '+IntToHex(lstart,8));
            dRead(@gg,16);
            //TODO try to rig into varStrArg
            SetValue(GUIDToString(gg));
           end;
          bsonBinaryMD5:
           begin
            //TODO
            raise EInvalidOperation.Create('bsonBinaryMD5: Not Implemented');
           end;
          else
            raise EBSONException.Create('Unknown BSON binary type '+
              IntToHex(o[11],2)+' at offset '+IntToHex(lstart,8));
        end;
       end;
      bsonObjectID:
       begin
        dRead(@o[0],12);
        SetValue(bsonObjectIDPrefix+
          hex[o[00] shr 4]+hex[o[00] and $F]+
          hex[o[01] shr 4]+hex[o[01] and $F]+
          hex[o[02] shr 4]+hex[o[02] and $F]+
          hex[o[03] shr 4]+hex[o[03] and $F]+
          hex[o[04] shr 4]+hex[o[04] and $F]+
          hex[o[05] shr 4]+hex[o[05] and $F]+
          hex[o[06] shr 4]+hex[o[06] and $F]+
          hex[o[07] shr 4]+hex[o[07] and $F]+
          hex[o[08] shr 4]+hex[o[08] and $F]+
          hex[o[09] shr 4]+hex[o[09] and $F]+
          hex[o[10] shr 4]+hex[o[10] and $F]+
          hex[o[11] shr 4]+hex[o[11] and $F]+
          bsonObjectIDSuffix);
       end;
      bsonBoolean:
       begin
        i:=0;
        dRead(@i,1);
        SetValue(boolean(i<>0));
       end;
      bsonUTCDateTime:
       begin
        dRead(@ii,8);
        SetValue(VarFromDateTime(ii/MSecsPerDay+UnixDateDelta));
       end;
      bsonNULL:
        SetValue(Null);
      bsonRegEx:
       begin
        {$IFDEF BSON_SUPPORT_REGEX}
        v:=dReadRegEx;
        {$ELSE}
        v:=bsonRegExPrefix+'/'+dReadCString+'/'+dReadCString;
        {$ENDIF}
        SetValue(v);
       end;
      bsonJavaScriptCode:
       begin
        v:=bsonJavaScriptCodePrefix+dReadString;//TODO: find active script interface?µ
        SetValue(v);
       end;
      bsonSymbol:
       begin
        v:=dReadString;//?
        SetValue(v);
       end;
      bsonJavaScriptCodeWithScope:
       begin
        //TODO
        raise EInvalidOperation.Create('bsonJavaScriptCodeWithScope: Not Implemented');
       end;
      bsonInt32:
       begin
        dRead(@i,4);
        SetValue(i);
       end;
      bsonTimestamp:
       begin
        dRead(@ii,8);
        SetValue(ii);//TODO: VarFromDateTime?
       end;
      bsonInt64:
       begin
        dRead(@ii,8);
        SetValue(ii);
       end;

      0://doc or array done
       begin
        if IsArray then
         begin
          if (da<>nil) and (d0=stackIndex) then
            v:=Null
          else
          if JSON_UseIJSONArray then
           begin
            aa:=TJSONArray.Create(ai-a1);
            i:=a1;
            j:=0;
            while i<ai do
             begin
              //aa[j]:=a[i];VarClear(a[i]);
              pv:=(aa as IJSONArray).v0(j);
              Move(a[i],pv^,SizeOf(TVarData));
              ZeroMemory(@a[i],SizeOf(TVarData));
              inc(i);
              inc(j);
             end;
            v:=aa as IJSONArray;
           end
          else
           begin
            if not(VarTypeIsValidArrayType(at)) then at:=varVariant;
            v:=VarArrayCreate([0,ai-a1-1],at);
            i:=a1;
            j:=0;
            while i<ai do
             begin
              v[j]:=a[i];
              VarClear(a[i]);
              inc(i);
              inc(j);
             end;
           end;
          ai:=a1;
         end
        else
          v:=Null;//assert d embedded document SetValue'd at start
        //pop from stack
        if stackIndex=0 then
         begin
          dec(stackIndex);//stackIndex:=-1;
         end
        else
         begin
          dec(stackIndex);
          if stack[stackIndex].d=nil then
           begin
            a1:=stack[stackIndex].k1;
            at:=stack[stackIndex].k2;
            IsArray:=true;
            if (da<>nil) and (d0=stackIndex) then
             begin
              da.AddJSON(dz.ToString);
              dz.Clear;
             end;
           end
          else
           begin
            d:=stack[stackIndex].d;
            key:=stack[stackIndex].key;
            stack[stackIndex].d:=nil;
            IsArray:=false;
            if (da<>nil) and (d0-1=stackIndex) then
              da:=nil;//done
           end;
         end;
        //set value
        if (TVarData(v).VType<>varNull) then SetValue(v);
       end;

      else
        raise EBSONException.Create('Unknown BSON element type '+
          IntToHex(i,2)+' at offset '+IntToHex(lstart,8));
    end;
   end;

  if stackIndex<>-1 then raise EBSONException.Create(
    'BSON with '+IntToStr(stackIndex+1)+' objects or arrays not closed');
  //if <>ltotal then raise?
end;

procedure SaveBSON(const Doc: IJSONDocument; Data: TStream);
var
  lstart,ltotal:integer;
  procedure dWrite(p:pointer;s:integer);
  begin
    if Data.Write(p^,s)<>s then
      raise EBSONException.Create('Failed to write data to stream');
    inc(ltotal,s);
  end;
  procedure dWriteCString(const s:WideString);
  var
    sx:UTF8String;
    sl:integer;
  begin
    sx:=UTF8Encode(s);
    sl:=Length(sx);
    //sx:=sx+#0;
    if sl=0 then
      dWrite(@sl,1)
    else
      dWrite(@sx[1],sl+1);
  end;
  procedure dWriteString(const s:WideString);
  var
    sx:UTF8String;
    sl:integer;
  begin
    sx:=UTF8Encode(s);
    sl:=Length(sx);
    inc(sl);
    dWrite(@sl,4);
    //sx:=sx+#0;
    if sl=1 then
     begin
      sl:=0;
      dWrite(@sl,1);
     end
    else
      dWrite(@sx[1],sl);
  end;

const
  stackGrowStep=$20;//not too much, not too little (?)
var
  e:IJSONEnumerator;
  IsArray:boolean;
  stack:array of record
    e:IJSONEnumerator;
    p:integer;//int64?
    IsArray:boolean;
  end;

  stackIndex,stackLength:integer;

  procedure Push(const NewEnum:IJSONEnumerator; NewIsArray:boolean);
  var
    i:integer;
  begin
    if NewIsArray then i:=bsonArray else i:=bsonEmbeddedDocument;
    dWrite(@i,1);
    dWriteCString(e.Key);
    i:=0;
    dWrite(@i,4);//don't know total length now, filled in later
    //push onto stack
    if stackIndex=stackLength then
     begin
      inc(stackLength,stackGrowStep);
      SetLength(stack,stackLength);
     end;
    stack[stackIndex].e:=e;
    stack[stackIndex].IsArray:=IsArray;
    stack[stackIndex].p:=ltotal-4;
    inc(stackIndex);
    e:=NewEnum;
    IsArray:=NewIsArray;
  end;
var
  uu:IUnknown;
  function TryWriteJSONDocument:boolean;
  var
    d:IJSONDocument;
  begin
    Result:=uu.QueryInterface(IID_IJSONDocument,d)=S_OK;
    if Result then Push(JSONEnum(d),false);
  end;
  function TryWriteBSONDocArray:boolean;
  var
    a:IBSONDocArray;
    p1,p2:int64;
  begin
    Result:=uu.QueryInterface(IID_IBSONDocArray,a)=S_OK;
    if Result then
     begin
      p1:=Data.Position;
      a.WriteAll(Data);
      p2:=Data.Position;
      inc(ltotal,p2-p1);
     end;
  end;
  function TryWriteJSONArray:boolean;
  var
    a:IJSONArray;
  begin
    Result:=uu.QueryInterface(IID_IJSONArray,a)=S_OK;
    if Result then Push(TJSONArrayEnumerator.Create(a),true);
  end;
  {$IFDEF BSON_SUPPORT_REGEX}
  function TryWriteRegExp:boolean;
  var
    i:integer;
    w:WideString;
    r:IRegExp2;
  begin
    Result:=uu.QueryInterface(IID_IRegExp2,r)=S_OK;
    if Result then
     begin
      i:=bsonRegEx;
      dWrite(@i,1);
      dWriteCString(e.Key);
      dWriteCString(r.Pattern);
      w:='';
      if r.Global then w:=w+'g';
      if r.IgnoreCase then w:=w+'i';
      if r.Multiline then w:=w+'m';
      //TODO: other regex flags
      dWriteCString(w);
     end;
  end;
  {$ENDIF}
  function TryWriteStream:boolean;
  const
    dSize=$10000;
    IID_IStream:TGUID='{0000000C-0000-0000-C000-000000000046}';
  var
    i:integer;
    p1,p2:{$IFDEF VER310}UInt64{$ELSE}Int64{$ENDIF};
    ss:IStream;
    d:array[0..dSize-1] of byte;
  begin
    Result:=uu.QueryInterface(IID_IStream,ss)=S_OK;
    if Result then
     begin
      i:=bsonBinary;
      dWrite(@i,1);
      dWriteCString(e.Key);
      //seek end to know full size
      OleCheck(ss.Seek(0,soFromEnd,p1));
      //TODO: check less than 2GB
      //seek start for copying
      OleCheck(ss.Seek(0,soFromBeginning,p2));
      dWrite(@p1,4);
      i:=bsonBinaryGeneric;
      dWrite(@i,1);
      inc(ltotal,p1);
      while p1<>0 do
       begin
        OleCheck(ss.Read(@d[0],dSize,@i));
        if i=0 then raise EBSONException.Create('Failed to read from IStream');
        if Data.Write(d[0],i)<>i then
          raise EBSONException.Create('Failed to write data to stream');
        p1:=p1-i;
       end;
     end;
  end;
  function TryWritePersistStream:boolean;
  const
    IID_IPersistStream:TGUID='{00000109-0000-0000-C000-000000000046}';
  var
    i:integer;
    p1,p2:int64;
    ps:IPersistStream;
  begin
    Result:=uu.QueryInterface(IID_IPersistStream,ps)=S_OK;
    if Result then
     begin
      i:=bsonBinary;
      dWrite(@i,1);
      dWriteCString(e.Key);
      p1:=Data.Position;
      i:=0;//fill in later
      dWrite(@i,4);
      i:=bsonBinaryGeneric;
      dWrite(@i,1);
      ps.Save(TStreamAdapter.Create(Data,soReference),false);
      //fill in length
      p2:=Data.Position;
      i:=p2-p1-5;
      Data.Position:=p1;
      if Data.Write(i,4)<>4 then
        raise EBSONException.Create('Failed to write data to stream');
      Data.Position:=p2;
     end;
  end;
  function StartsWith(const a,b:WideString):boolean;
  var
    i,l1,l2:integer;
  begin
    i:=1;
    l1:=Length(a);
    l2:=Length(b);
    while (i<=l1) and (i<=l2) and (a[i]=b[i]) do inc(i);
    Result:=i=l2+1;
  end;
var
  v:PVariant;
  vt:TVarType;
  w:WideString;
  i,j,wl:integer;
  o:array[0..15] of byte;
  ii:int64 absolute o;
  gg:TGUID absolute o;
  dd:double absolute o;
  pp:pointer absolute o;
  bb:byte;
begin
  stackIndex:=0;
  stackLength:=0;
  lstart:=Data.Position;//may not be 0!
  ltotal:=0;
  i:=0;//write now, fill in later
  dWrite(@i,4);

  e:=JsonEnum(Doc);
  IsArray:=false;
  while e<>nil do
    if e.Next then
     begin
      v:=PVariant(e.v0);
      vt:=TVarData(v^).VType;
      if (vt and varArray)=0 then
        case vt and varTypeMask of
          //varEmpty?
          varNull:
           begin
            i:=bsonNULL;
            dWrite(@i,1);
            dWriteCString(e.Key);
           end;
          varSmallint,varInteger,varShortInt,varByte,varWord,varLongWord:
           begin
            i:=bsonInt32;
            dWrite(@i,1);
            dWriteCString(e.Key);
            i:=v^;
            dWrite(@i,4);
           end;
          varInt64:
           begin
            i:=bsonInt64;
            dWrite(@i,1);
            dWriteCString(e.Key);
            ii:=v^;
            dWrite(@ii,8);
            //TODO: detect bsonTimestamp?
           end;
          varSingle,varDouble,varCurrency:
           begin
            i:=bsonDouble;
            dWrite(@i,1);
            dWriteCString(e.Key);
            dd:=v^;
            dWrite(@dd,8);
           end;
          varDate:
           begin
            i:=bsonUTCDateTime;
            dWrite(@i,1);
            dWriteCString(e.Key);
            ii:=Round((VarToDateTime(v^)-UnixDateDelta)*MSecsPerDay);
            dWrite(@ii,8);
           end;
          varOleStr,varString,$0102:
           begin
            //detect GUID //TODO try to rig varStrArg
            w:=VarToWideStr(v^);
            wl:=Length(w);
            if (wl=38) and (w[1]='{') and (w[38]='}')
              and (w[10]='-') and (w[15]='-')
              and (w[20]='-') and (w[25]='-') then //and the other are hex digits?
             begin
              //assume UUID
              gg:=StringToGUID(w);
              i:=bsonBinary;
              dWrite(@i,1);
              dWriteCString(e.Key);
              i:=16;//SizeOf(TGUID);
              dWrite(@i,4);
              i:=bsonBinaryUUID;
              dWrite(@i,1);
              dWrite(@gg,16);
             end
            else
            //detect objectID
            if (wl=bsonObjectID_L) and StartsWith(w,bsonObjectIDPrefix) then //and the other are hex digits?
             begin
              i:=bsonObjectID;
              dWrite(@i,1);
              dWriteCString(e.Key);
              j:=Length(bsonObjectIDPrefix)+1;
              for i:=0 to 11 do
               begin
                bb:=byte(AnsiChar(w[j+i*2]));
                if (bb and $F0)=$30 then o[i]:=bb shl 4 else o[i]:=(9+bb) shl 4;
                bb:=byte(AnsiChar(w[j+i*2+1]));
                if (bb and $F0)=$30 then inc(o[i],bb and $F) else inc(o[i],(9+bb) and $F);
               end;
              dWrite(@o[0],12);
             end
            else
            //detect javascript
            if StartsWith(w,bsonJavaScriptCodePrefix) then
             begin
              i:=bsonJavaScriptCode;
              dWrite(@i,1);
              dWriteCString(e.Key);
              dWriteString(Copy(w,Length(bsonJavaScriptCodePrefix)+1,Length(w)-Length(bsonJavaScriptCodePrefix)));
             end
            else
            //detect regex
            if StartsWith(w,bsonRegExPrefix) then
             begin
              i:=bsonRegEx;
              dWrite(@i,1);
              dWriteCString(e.Key);
              i:=Length(bsonRegExPrefix)+1;
              if (i<=Length(w)) and (w[i]='/') then inc(i);//TODO: support alternate regex delimiter?
              j:=i;
              while (j<=Length(w)) and (w[i]<>'/') do inc(j);
              dWriteCString(Copy(w,i,j-i));
              dWriteCString(Copy(w,j+1,Length(w)-j));
             end
            else
             begin
              i:=bsonString;
              dWrite(@i,1);
              dWriteCString(e.Key);
              dWriteString(w);
             end;
            //TODO: bsonJavaScriptCodeWithScope, bsonSymbol ?
           end;
          //TODO varStrArg as bsonBinaryUUID
          varBoolean:
           begin
            i:=bsonBoolean;
            dWrite(@i,1);
            dWriteCString(e.Key);
            if v^ then i:=1 else i:=0;
            dWrite(@i,1);
           end;
          //varVariant://TODO
          varDispatch,varUnknown:
           begin
            uu:=IUnknown(v^);
            if uu<>nil then
            if not TryWriteJSONDocument then
            if not TryWriteBSONDocArray then
            if not TryWriteJSONArray then
            {$IFDEF BSON_SUPPORT_REGEX}
            if not TryWriteRegExp then
            {$ENDIF}
            if not TryWriteStream then
            if not TryWritePersistStream then
              raise EBSONException.Create('No supported interface found on object "'+e.Key+'"');
           end;
          else raise EBSONException.Create('Unsupported variant type '+IntToHex(vt,4)+' "'+e.Key+'"');
        end
      else
      if (vt and varTypeMask)=varByte then
       begin
        i:=bsonBinary;
        dWrite(@i,1);
        dWriteCString(e.Key);
        j:=VarArrayHighBound(v^,1)-VarArrayLowBound(v^,1)+1;
        dWrite(@j,4);
        i:=bsonBinaryGeneric;
        dWrite(@i,1);
        pp:=VarArrayLock(v^);
        try
          dWrite(pp,j);
        finally
          VarArrayUnlock(v^);
        end;
       end
      else
        Push(TVarArrayEnumerator.Create(v),true);
     end
    else
     begin
      //terminator
      i:=0;
      dWrite(@i,1);
      //assert Data.Position=lstart+ltotal
      if stackIndex=0 then
       begin
        //done
        e:=nil;
        Data.Position:=lstart;
        j:=ltotal;
       end
      else
       begin
        //pop from stack
        dec(stackIndex);
        e:=stack[stackIndex].e;
        IsArray:=stack[stackIndex].IsArray;
        stack[stackIndex].e:=nil;
        Data.Position:=lstart+stack[stackIndex].p;
        j:=ltotal-stack[stackIndex].p;
       end;
      //write total length (see Seek's above)
      if Data.Write(j,4)<>4 then
        raise EBSONException.Create('Failed to write data to stream');
      //return to end position
      Data.Position:=lstart+ltotal;
     end;
end;

{ TBSONDocArray }

constructor TBSONDocArray.Create;
begin
  inherited Create;
  FData:=nil;
  FRefIndex:=0;
  FRefLength:=0;
end;

destructor TBSONDocArray.Destroy;
begin
  FData:=nil;
  VarClear(FCurrent);
  inherited;
end;

function TBSONDocArray.AddRef(FromSize: boolean): integer;
var
  p:integer;
begin
  Result:=FRefIndex;
  if FRefIndex=FRefLength then
   begin
    //grow
    inc(FRefLength,$100);
    SetLength(FRef,FRefLength);
   end;
  //TODO: if FData=nil then FData:=TMemoryStream.Create; (and Free on Destroy)?
  if FromSize then
   begin
    p:=FData.Size;
    FRef[FRefIndex]:=p;
    FData.Position:=p;
   end;
  inc(FRefIndex);
end;

function TBSONDocArray.Add(const Doc: IJSONDocument): integer;
begin
  Result:=AddRef(true);
  SaveBSON(Doc,FData);
end;

function TBSONDocArray.AddJson(const Data: WideString): integer;
begin
  Result:=AddRef(true);
  SaveBSON(JSON(Data),FData);
end;

procedure TBSONDocArray.Clear;
begin
  FData:=nil;//?
  FRefIndex:=0;
end;

function TBSONDocArray.Count: integer;
begin
  Result:=FRefIndex;
end;

function TBSONDocArray.Get_Item(Index: integer): Variant;
var
  d:IJSONDocument;
begin
  if (Index<0) or (Index>=FRefIndex) then
    raise ERangeError.Create('Out of range');
  FData.Position:=FRef[Index];
  d:=JSON;
  LoadBSON(FData,d);
  Result:=d;
end;

procedure TBSONDocArray.LoadItem(Index: integer; const Doc: IJSONDocument);
begin
  //Doc.Clear? let caller decide
  if (Index<0) or (Index>=FRefIndex) then
    raise ERangeError.Create('Out of range');
  FData.Position:=FRef[Index];
  LoadBSON(FData,Doc);
end;

function TBSONDocArray.GetJSON(Index: integer): WideString;
var
  Doc:IJSONDocument;
begin
  if (Index<0) or (Index>=FRefIndex) then
    raise ERangeError.Create('Out of range');
  FData.Position:=FRef[Index];
  Doc:=JSON;
  LoadBSON(FData,Doc);
  Result:=Doc.ToString;
end;

procedure TBSONDocArray.Set_Item(Index: integer; const Value: Variant);
var
  p:integer;
begin
  if (Index<0) or (Index>=FRefIndex) then
    raise ERangeError.Create('Out of range');
  p:=FData.Size;
  FData.Position:=p;
  SaveBSON(JSON(Value),FData);
  FRef[Index]:=p;
end;

function TBSONDocArray.JSONToString: WideString;
var
  i:integer;
  d:IJSONDocument;
begin
  Result:='';
  //TODO: $IFDEF JSONDOC_STOREINDENTING
  d:=JSON;
  for i:=0 to FRefIndex-1 do
   begin
    FData.Seek(FRef[i],soBeginning);
    d.Clear;
    LoadBSON(FData,d);
    Result:=Result+','+d.ToString;
   end;
  if Result='' then Result:='[' else Result[1]:='[';
  Result:=Result+']';
end;

function TBSONDocArray.v0(Index: integer): pointer;
var
  d:IJSONDocument;
begin
  if (Index<0) or (Index>=FRefIndex) then
    raise ERangeError.Create('Out of range');
  FData.Position:=FRef[Index];
  d:=JSON;
  LoadBSON(FData,d);
  FCurrent:=d;
  Result:=@FCurrent;
end;

procedure TBSONDocArray.ReadAll(Data: TStream);
var
  lstart,lmax,ltotal:cardinal;//int64?
  i,t:integer;
  c:AnsiChar;
  procedure lRead(p:pointer;s:integer);
  begin
    if Data.Read(p^,s)<>s then
      raise EBSONException.Create('Unexpected end of stream');
    inc(ltotal,s);
    if ltotal>lmax then
      raise EBSONException.Create('More BSON data than declared');
  end;
begin
  FData:=Data;
  lstart:=FData.Position;
  ltotal:=4;
  if FData.Read(lmax,4)<>4 then
    raise EBSONException.Create('Unexpected end of stream');
  t:=0;
  repeat
    //read element type
    lRead(@t,1);
    if t<>0 then
     begin
      if t<>bsonEmbeddedDocument then
        raise EBSONException.Create('IBSONDocArray: non-document element at offset '+
          IntToHex(lstart+ltotal,8));
      //read array index
      i:=0;
      repeat
        lRead(@c,1);
        if c<>#0 then
          if c in ['0'..'9'] then
            i:=i*10+(byte(c) and $F)
          else
            raise EBSONException.Create('Invalid array index at offset '+
              IntToHex(lstart+ltotal,8));
      until c=#0;
      if i<>FRefIndex then
        raise EBSONException.Create('Unexpected array index "'+IntToStr(i)+
          '"<>"'+IntToStr(FRefIndex)+'" at offset '+IntToHex(lstart+ltotal,8));
      //list item
      FRef[AddRef(false)]:=FData.Position;
      lRead(@i,4);
      FData.Seek(i-4,soCurrent);
      inc(ltotal,i-4);
      if ltotal>lmax then
        raise EBSONException.Create('More BSON data than declared');
     end;
  until t=0;
end;

procedure TBSONDocArray.WriteAll(Data: TStream);
const
  dSize=$10000;
var
  i,j,k,l:integer;
  d:array[0..dSize-1] of byte;
begin
  for i:=0 to FRefIndex-1 do
   begin
    FData.Position:=FRef[i];
    if FData.Read(l,4)<>4 then
      raise EJSONException.Create('Error reading BSONDocArray');
    if Data.Write(l,4)<>4 then
      raise EJSONException.Create('Error writing BSONDocArray');
    if l>=4 then dec(l,4);
    while l<>0 do
     begin
      if l>dSize then j:=dSize else j:=l;
      k:=FData.Read(d[0],j);
      if k=0 then
        raise EJSONException.Create('Error reading BSONDocArray');
      if Data.Write(d[0],k)<>k then
        raise EJSONException.Create('Error writing BSONDocArray');
      dec(l,k);
     end;
   end;
end;

end.
