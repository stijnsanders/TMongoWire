unit bsonDoc;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, SysUtils, WinTypes;

const
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

  IID_IBSONDocument : TGUID = '{42534F4E-0000-0001-C000-000000000001}';
  CLASS_BSONDocument : TGUID = '{42534F4E-0000-0002-C000-000000000002}';

  //special prefix with unassigned(?) unicode symbols from the Special range
  bsonJavaScriptCodePrefix:WideString=#$FFF1'bsonJavaScriptCode'#$FFF2;
  bsonRegExPrefix:WideString=#$FFF1'bsonRegEx'#$FFF2;
  bsonObjectIDPrefix:WideString='ObjectID("';
  bsonObjectIDSuffix:WideString='")';

type
  IBSONDocument = interface(IUnknown)
    ['{42534F4E-0000-0001-C000-000000000001}']
    function Get_Item(const Key: WideString): OleVariant; safecall;
    procedure Set_Item(const Key: WideString; Value: OleVariant); safecall;
    function ToVarArray:OleVariant; safecall;
    procedure Clear; safecall;
    property Item[const Key: WideString]: OleVariant read Get_Item write Set_Item; default;
  end;

  //TODO: ActiveX enumerator over elements

  //BSON document as interfaced object allows storage in a variant variable
  TBSONDocument = class(TInterfacedObject, IBSONDocument, IPersistStream)
  private
    FDirty:boolean;
    FElementIndex,FElementSize,FLastIndex:integer;
    FKeys:array of record
      Key:WideString;
      ValueIndex:integer;
    end;
    FValues:array of OleVariant;
    procedure GetKeyIndex(Key: WideString;var Index:integer; var Match:boolean);
  protected
    function Get_Item(const Key: WideString): OleVariant; safecall;
    procedure Set_Item(const Key: WideString; Value: OleVariant); safecall;
    function GetClassID(out classID: TCLSID): HResult; stdcall;
    function IsDirty: HResult; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; stdcall;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Load(const stm: IStream): HResult; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; stdcall;
    function ToVarArray:OleVariant; safecall;
    procedure Clear; safecall;
    //TODO: function toJSON:WideString;
    property Item[const Key: WideString]: OleVariant read Get_Item write Set_Item; default;
  end;

  EBSONException=class(Exception);

{
  BSON document builder
  pass an array of key/value-pairs,
  use value '[' to start an embedded document,
  and key ']' to close it.
  example:
    MongoWireQuery1.Query('mydb1.orders',BSON([
      'confirmed',false,
      '$orderby','[',
        'created',-1,
      ']'
    ]);
}
function BSON:IBSONDocument; overload;
function BSON(x:array of OleVariant):IBSONDocument; overload;

implementation

uses
  Classes,
  {$IFDEF BSON_SUPPORT_REGEX}
  VBScript_RegExp_55_TLB,
  {$ENDIF}
  Variants;

const
  BSONArrayBaseIndex=0;//1?
  BSONDetectVarArrayType=true;

procedure TBSONDocument.AfterConstruction;
begin
  inherited;
  FDirty:=false;
  FElementIndex:=0;
  FElementSize:=0;
  FLastIndex:=0;
end;

destructor TBSONDocument.Destroy;
var
  i:integer;
begin
  for i:=0 to FElementIndex-1 do VarClear(FValues[i]);
  inherited;
end;

function TBSONDocument.GetSizeMax(out cbSize: Largeint): HResult;
begin
  //TODO: calculate total size
  raise EInvalidOperation.Create('Not implemented');
end;

procedure TBSONDocument.GetKeyIndex(Key: WideString; var Index: integer;
  var Match: boolean);
var
  a,b,c,x:integer;
begin
  //case sensitivity?
  //check last getindex, speeds up set right after get
  if (FElementIndex<>0) and (CompareStr(Key,FKeys[FLastIndex].Key)=0) then
   begin
    Index:=FLastIndex;
    Match:=true;
   end
  else
   begin
    a:=0;
    b:=FElementIndex-1;
    Match:=false;//default
    while b>=a do
     begin
      c:=(a+b) div 2;
      //if c=a? c=b?
      x:=CompareStr(Key,FKeys[c].Key);
      if x=0 then
       begin
        a:=c;
        b:=c-1;
        Match:=true;
        FLastIndex:=c;
       end
      else
        if x<0 then
          if b=c then dec(b) else b:=c
        else
          if a=c then inc(a) else a:=c;
     end;
    Index:=a;
   end;
end;

function TBSONDocument.Get_Item(const Key: WideString): OleVariant;
var
  x:integer;
  m:boolean;
begin
  GetKeyIndex(Key,x,m);
  if m then x:=FKeys[x].ValueIndex;
  if m and not(VarIsEmpty(FValues[x])) then Result:=FValues[x] else Result:=Null;
end;

procedure TBSONDocument.Set_Item(const Key: WideString; Value: OleVariant);
var
  x,i:integer;
  m:boolean;
const
  GrowStep=$20;//not too much, not too little (?)
begin
  GetKeyIndex(Key,x,m);
  if not(m) then
   begin
    if FElementIndex=FElementSize then
     begin
      inc(FElementSize,GrowStep);
      SetLength(FKeys,FElementSize);
      SetLength(FValues,FElementSize);
     end;
    for i:=FElementIndex-1 downto x do FKeys[i+1]:=FKeys[i];
    FKeys[x].Key:=Key;
    FKeys[x].ValueIndex:=FElementIndex;
    inc(FElementIndex);
   end;
  FValues[FKeys[x].ValueIndex]:=Value;
  //TODO: if VarType(Value)=varEmpty then drop element
  FDirty:=true;
end;

function TBSONDocument.IsDirty: HResult;
begin
  if FDirty then Result:=S_OK else Result:=S_FALSE; 
end;

function TBSONDocument.Load(const stm: IStream): HResult;
var
  i,lstart,ltotal,ltmax:integer;
  procedure stmRead(p:pointer;s:integer);
  var
    l:integer;
  begin
    OleCheck(stm.Read(p,s,@l));
    if l<>s then raise EBSONException.Create('Unexpected end of stream');
    inc(ltotal,s);
    if ltotal>ltmax then raise EBSONException.Create('More BSON data than declared');
  end;
  function stmReadCString:WideString;
  var
    c:AnsiChar;
    s:AnsiString;
    sx,sl:integer;
  begin
    sx:=0;
    sl:=0;
    stmRead(@c,1);
    while c<>#0 do
     begin
      if sx=sl then
       begin
        inc(sl,$100);//GrowStep
        SetLength(s,sl);
       end;
      inc(sx);
      s[sx]:=c;
      stmRead(@c,1);
     end;
    SetLength(s,sx);
    Result:=UTF8Decode(s);
  end;
  function stmReadString:WideString;
  var
    l:integer;
    s:AnsiString;
  begin
    stmRead(@l,4);
    if l=1 then s:='' else
     begin
      SetLength(s,l-1);
      stmRead(@s[1],l-1);
     end;
    //read closing null
    l:=0;
    stmRead(@l,1);
    if l<>0 then raise EBSONException.Create('BSON string incorrectly terminated at offset '+IntToHex(lstart,8));
    Result:=UTF8Decode(s);
  end;
  {$IFDEF BSON_SUPPORT_REGEX}
  function stmReadRegEx:IRegExp2;
  var
    i:integer;
    s:string;
  begin
    Result:=CoRegExp.Create;
    REsult.Pattern:=stmReadCString;
    s:=stmReadCString;
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
  function stmReadBSONDocument:IBSONDocument;
  var
    p1,p2:int64;
    d:TBSONDocument;
  begin
    OleCheck(stm.Seek(0,soFromCurrent,p1));
    d:=TBSONDocument.Create;
    try
      d.Load(stm);
      Result:=(d as IBSONDocument);
    except //not finally!
      d.Free;
      raise;
    end;
    OleCheck(stm.Seek(0,soFromCurrent,p2));
    inc(ltotal,p2-p1);
  end;
var
  j:integer;
  v:OleVariant;
  k:WideString;
  o:array[0..11] of byte;
  ii:int64 absolute o;
  dd:double absolute o;
  pp:pointer absolute o;
  gg:TGUID absolute o;
  vindex:integer;
  vstack:array of record
    vkey:WideString;
    vv:array of OleVariant;
    vl,vi,vlength,vstart:integer;
    vt:TVarType;
  end;
  vArrayIsEmpty: Boolean;
const
  hex:array[0..15] of char='0123456789abcdef';
begin
  ltotal:=0;
  lstart:=0;
  ltmax:=4;
  stmRead(@ltMax,4);
  vindex:=-1;
  SetLength(vstack,0);

  while ltotal<ltmax do
   begin
    i:=0;
    lstart:=ltotal;
    stmRead(@i,1);
    if i=0 then //end of document
     begin
      if vindex=-1 then
       begin
        if ltotal<>ltmax then
          raise EBSONException.Create('Less BSON data than declared');
       end
      else
       begin
        if ltotal-vstack[vindex].vstart>vstack[vindex].vlength then
          raise EBSONException.Create('More BSON array data than declared at offset '+IntToHex(lstart,8));
        SetLength(vstack[vindex].vv,vstack[vindex].vi);
        //create variant array of variant (from pascal array of variant)
        v:=VarArrayCreate([BSONArrayBaseIndex,vstack[vindex].vi-1+BSONArrayBaseIndex],vstack[vindex].vt);
        for j:=0 to vstack[vindex].vi-1 do v[j]:=vstack[vindex].vv[j];
        //store item
        if vindex=0 then
        begin
          Set_Item(vstack[vindex].vkey,v);
        end
        else
        begin
          //Empty Embedded array
          vArrayIsEmpty := VarIsArray(vstack[vindex-1].vv) and (VarArrayHighBound(vstack[vindex-1].vv, 1) = -1);
          if vArrayIsEmpty then
          begin
            SetLength(vstack[vindex-1].vv, 1);
          end;
          vstack[vindex-1].vv[vstack[vindex-1].vi]:=v;

          if (vArrayIsEmpty) then
          begin
            vstack[vindex-1].vi := 1;
          end;
        end;
        //pop from array stack
        SetLength(vstack[vindex].vv,0);
        dec(vindex);
        //SetLength(vstack,vindex);
       end;
     end
    else
     begin
      k:=stmReadCString;
      v:=Null;//VarClear
      //TODO: store value somewhere?
      case i of
        bsonDouble:
         begin
          stmRead(@dd,8);
          v:=dd;
         end;
        bsonString:
          v:=stmReadString;
        bsonEmbeddedDocument:
          v:=stmReadBSONDocument;
        bsonArray:
         begin
          //push onto array stack
          inc(vindex);
          SetLength(vstack,vindex+1);
          vstack[vindex].vkey:=k;
          vstack[vindex].vi:=-1;
          vstack[vindex].vl:=0;
          vstack[vindex].vstart:=ltotal;
          vstack[vindex].vt:=varVariant;
          stmRead(@vstack[vindex].vlength,4);//array document length
         end;
        bsonBinary:
         begin
          stmRead(@i,4);
          stmRead(@o[11],1);
          //TODO: store value somewhere?
          case o[11] of
            bsonBinaryGeneric,bsonBinaryOldBinary,bsonBinaryUserDefined:
             begin
              //
              v:=VarArrayCreate([BSONArrayBaseIndex,i-1+BSONArrayBaseIndex],varByte);
              pp:=VarArrayLock(v);
              try
                stmRead(pp,i);
              finally
                VarArrayUnlock(v);
              end;
             end;
            bsonBinaryFunction:
             begin
              //TODO
              raise EInvalidOperation.Create('Not Implemented');
             end;
            bsonBinaryUUID:
             begin
              if i<>16 then raise EBSONException.Create('Unexpected UUID length ('+IntToStr(i)+') at offset '+IntToHex(lstart,8));
              stmRead(@gg,16);
              //TODO try to rig into varStrArg
              v:=GUIDToString(gg);
             end;
            bsonBinaryMD5:
             begin
              //TODO
              raise EInvalidOperation.Create('Not Implemented');
             end;
            else raise EBSONException.Create('Unknown BSON binary type '+IntToHex(o[11],2)+' at offset '+IntToHex(lstart,8));
          end;
         end;
        bsonObjectID:
         begin
          stmRead(@o[0],12);
          v:=bsonObjectIDPrefix+
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
            bsonObjectIDSuffix;
         end;
        bsonBoolean:
         begin
          i:=0;
          stmRead(@i,1);
          v:=boolean(i<>0);
         end;
        bsonUTCDateTime:
         begin
          stmRead(@ii,8);
          v:=VarFromDateTime(ii/MSecsPerDay+UnixDateDelta);
         end;
        bsonNULL:;//v:=Null;
        bsonRegEx:
          {$IFDEF BSON_SUPPORT_REGEX}
          v:=stmReadRegEx;
          {$ELSE}
          v:=bsonRegExPrefix+'/'+stmReadCString+'/'+stmReadCString;
          {$ENDIF}
        bsonJavaScriptCode:
          v:=bsonJavaScriptCodePrefix+stmReadString;//TODO: find active script interface?
        bsonSymbol:
          v:=stmReadString;//?
        bsonJavaScriptCodeWithScope:
         begin
          //TODO
          raise EInvalidOperation.Create('Not Implemented');
         end;
        bsonInt32:
         begin
          stmRead(@i,4);
          v:=i;
         end;
        bsonTimestamp:
         begin
          stmRead(@ii,8);
          v:=ii;//convert?
         end;
        bsonInt64:
         begin
          stmRead(@ii,8);
          v:=ii;
         end;
        else raise EBSONException.Create('Unknown BSON element type '+IntToHex(i,2)+' at offset '+IntToHex(lstart,8));
      end;
      //add the element
      if vindex=-1 then
        Set_Item(k,v)
      else
        if vstack[vindex].vi=-1 then
          vstack[vindex].vi:=0 //just starting
        else
         begin
          if vstack[vindex].vi=vstack[vindex].vl then
           begin
            inc(vstack[vindex].vl,$100);//growstep
            SetLength(vstack[vindex].vv,vstack[vindex].vl);
           end;
          if k<>IntToStr(vstack[vindex].vi) then raise EBSONException.Create('Unexpected BSON array index key: "'+k+'"<>"'+
              IntToStr(vstack[vindex].vi)+'" at offset '+IntToHex(lstart,8));
          vstack[vindex].vv[vstack[vindex].vi]:=v;
          if BSONDetectVarArrayType then
            if vstack[vindex].vi=0 then vstack[vindex].vt:=VarType(v) else
              if (vstack[vindex].vt<>varVariant) and (vstack[vindex].vt<>VarType(v)) then
                vstack[vindex].vt:=varVariant;
          inc(vstack[vindex].vi);
         end;
     end;
   end;

  FDirty:=false;
  Result:=S_OK;
  //TODO: try except Result:=E_?
end;

function TBSONDocument.Save(const stm: IStream;
  fClearDirty: BOOL): HResult;
var
  lstart,lx:Int64;
  ltotal,li,xi:integer;
  procedure stmWrite(p:pointer;s:integer);
  var
    l:integer;
  begin
    OleCheck(stm.Write(p,s,@l));
    if l<>s then raise EBSONException.Create('Failed to write data to stream');
    inc(ltotal,s);
  end;
  procedure stmWriteCString(s:WideString);
  var
    sx:UTF8String;
    sl:integer;
  begin
    sx:=UTF8Encode(s);
    sl:=Length(sx);
    //sx:=sx+#0;
    if sl=0 then
      stmWrite(@sl,1)
    else
      stmWrite(@sx[1],sl+1);
  end;
  procedure stmWriteString(s:WideString);
  var
    sx:UTF8String;
    sl:integer;
  begin
    sx:=UTF8Encode(s);
    sl:=Length(sx);
    inc(sl);
    stmWrite(@sl,4);
    //sx:=sx+#0;
    if sl=1 then
     begin
      sl:=0;
      stmWrite(@sl,1);
     end
    else
      stmWrite(@sx[1],sl);
  end;
var
  uu:IUnknown;
  key:WideString;
  function TryWriteBSONDocument:boolean;
  var
    i:integer;
    ii,jj:int64;
    di:IBSONDocument;
  begin
    Result:=uu.QueryInterface(IID_IBSONDocument,di)=S_OK;
    if Result then
     begin
      i:=bsonEmbeddedDocument;
      stmWrite(@i,1);
      stmWriteCString(key);
      OleCheck(stm.Seek(0,soFromCurrent,ii));
      OleCheck((di as IPersistStream).Save(stm,fClearDirty));
      OleCheck(stm.Seek(0,soFromCurrent,jj));
      inc(ltotal,jj-ii);
     end;
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
      stmWrite(@i,1);
      stmWriteCString(key);
      stmWriteCString(r.Pattern);
      w:='';
      if r.Global then w:=w+'g';
      if r.IgnoreCase then w:=w+'i';
      if r.Multiline then w:=w+'m';
      //TODO: other regex flags
      stmWriteCString(w);
     end;
  end;
  {$ENDIF}
  function TryWriteStream:boolean;
  const
    dSize=$10000;
    IID_IStream:TGUID='{0000000C-0000-0000-C000-000000000046}';
  var
    i,j:integer;
    ii,jj:int64;
    ss:IStream;
    d:array[0..dSize-1] of byte;
  begin
    Result:=uu.QueryInterface(IID_IStream,ss)=S_OK;
    if Result then
     begin
      i:=bsonBinary;
      stmWrite(@i,1);
      stmWriteCString(key);
      //seek end to know full size
      OleCheck(ss.Seek(0,soFromEnd,ii));
      //TODO: check less than 2GB
      //seek start for copying
      OleCheck(ss.Seek(0,soFromBeginning,jj));
      stmWrite(@ii,4);
      i:=bsonBinaryGeneric;
      stmWrite(@i,1);
      inc(ltotal,ii);
      while ii<>0 do
       begin
        OleCheck(ss.Read(@d[0],dSize,@i));
        if i=0 then raise EBSONException.Create('Failed to read from IStream');
        OleCheck(stm.Write(@d[0],i,@j));
        if i<>j then raise EBSONException.Create('Failed to write data to stream');
        dec(ii,i);
       end;
     end;
  end;
  function TryWritePersistStream:boolean;
  const
    IID_IPersistStream:TGUID='{00000109-0000-0000-C000-000000000046}';
  var
    i,j:integer;
    ii,jj:int64;
    ps:IPersistStream;
  begin
    Result:=uu.QueryInterface(IID_IPersistStream,ps)=S_OK;
    if Result then
     begin
      i:=bsonBinary;
      stmWrite(@i,1);
      stmWriteCString(key);
      OleCheck(stm.Seek(0,soFromCurrent,ii));
      i:=0;//fill in later
      stmWrite(@i,4);
      i:=bsonBinaryGeneric;
      stmWrite(@i,1);
      ps.Save(stm,false);
      //fill in length
      OleCheck(stm.Seek(0,soFromCurrent,jj));
      i:=jj-ii-5;
      OleCheck(stm.Seek(0,soFromBeginning,ii));
      OleCheck(stm.Write(@i,4,@j));
      //assert j=4
      OleCheck(stm.Seek(0,soFromBeginning,jj));
     end;
  end;
var
  vt:TVarType;
  i,j:integer;
  v:OleVariant;
  ii:int64;
  w:WideString;
  o:array[0..11] of byte;
  gg:TGUID absolute o;
  dd:double absolute o;
  pp:pointer absolute o;
  bb:byte;
  IsArray:boolean;
  vindex:integer;
  vstack:array of record
    vv:OleVariant;
    vstart,vi,v1,v2:integer;
  end;
begin
  OleCheck(stm.Seek(0,soFromCurrent,lstart));
  ltotal:=0;
  i:=0;//write now, fill in later
  stmWrite(@i,4);

  vindex:=-1;
  xi:=0;
  while (xi<FElementIndex) or (vindex>=0) do
   begin
    if vindex=-1 then
     begin
      key:=FKeys[xi].Key;
      v:=FValues[FKeys[xi].ValueIndex];
      inc(xi);
     end
    else
     begin
      key:=IntToStr(vstack[vindex].vi-vstack[vindex].v1);
      v:=vstack[vindex].vv[vstack[vindex].vi];
      inc(vstack[vindex].vi);
     end;

    vt:=VarType(v);
    IsArray:=(vt and varArray)<>0;
    if IsArray then
      if VarArrayDimCount(v)>1 then
        raise EInvalidOperation.Create('VarArray: multi-dimensional arrays not supported');//TODO:
    if (vt and varByRef)<>0 then
      raise EInvalidOperation.Create('VarByRef: not implemented');//TODO: arrays!!!

    if IsArray and ((vt and varTypeMask)=varByte) then
     begin
      i:=bsonBinary;
      stmWrite(@i,1);
      stmWriteCString(key);
      j:=VarArrayHighBound(v,1)-VarArrayLowBound(v,1);
      stmWrite(@j,4);
      i:=bsonBinaryGeneric;
      stmWrite(@i,1);
      pp:=VarArrayLock(v);
      try
        stmWrite(pp,j);
      finally
        VarArrayUnlock(v);
      end;
     end
    else
    if IsArray then
     begin
      //push onto array stack
      inc(vindex);
      SetLength(vstack,vindex+1);
      vstack[vindex].vv:=v;
      vstack[vindex].v1:=VarArrayLowBound(v,1);
      vstack[vindex].v2:=VarArrayHighBound(v,1);
      vstack[vindex].vi:=vstack[vindex].v1;
      i:=bsonArray;
      stmWrite(@i,1);
      stmWriteCString(key);
      vstack[vindex].vstart:=ltotal;
      i:=0;
      stmWrite(@i,4);//don't know total length now, filled in later
     end
    else
     begin

      //TODO: re-use stored bson type? if any?
      case vt and varTypeMask of
        //varEmpty?
        varNull:
         begin
          i:=bsonNULL;
          stmWrite(@i,1);
          stmWriteCString(key);
         end;
        varSmallint,varInteger,varShortInt,varByte,varWord,varLongWord:
         begin
          i:=bsonInt32;
          stmWrite(@i,1);
          stmWriteCString(key);
          i:=v;
          stmWrite(@i,4);
         end;
        varInt64:
         begin
          i:=bsonInt64;
          stmWrite(@i,1);
          stmWriteCString(key);
          ii:=v;
          stmWrite(@ii,8);
          //TODO: detect bsonTimestamp?
         end;
        varSingle,varDouble,varCurrency:
         begin
          i:=bsonDouble;
          stmWrite(@i,1);
          stmWriteCString(key);
          dd:=v;
          stmWrite(@dd,8);
         end;
        varDate:
         begin
          i:=bsonUTCDateTime;
          stmWrite(@i,1);
          stmWriteCString(key);
          ii:=Round((VarToDateTime(v)-UnixDateDelta)*MSecsPerDay);
          stmWrite(@ii,8);
         end;
        varOleStr:
         begin
          //detect GUID //TODO try to rig varStrArg
          w:=VarToWideStr(v);
          if (Length(W)=38) and (W[1]='{') and (w[38]='}') and (w[10]='-') and (w[15]='-') and (w[20]='-') and (w[25]='-') then //and the other are hex digits?
           begin
            //assume UUID
            gg:=StringToGUID(w);
            i:=bsonBinary;
            stmWrite(@i,1);
            stmWriteCString(key);
            i:=16;//SizeOf(TGUID);
            stmWrite(@i,4);
            i:=bsonBinaryUUID;
            stmWrite(@i,1);
            stmWrite(@gg,16);
           end
          else
          //detect objectID
		      if (Length(w)=Length(bsonObjectIDPrefix)+24+Length(bsonObjectIDSuffix)) then //and the other are hex digits?
           begin
            i:=bsonObjectID;
            stmWrite(@i,1);
            stmWriteCString(key);
			      j:=Length(bsonObjectIDPrefix)+1;
            for i:=0 to 11 do
             begin
              bb:=byte(AnsiChar(w[j+i*2]));
              if (bb and $F0)=$30 then o[i]:=bb shl 4 else o[i]:=(9+bb) shl 4;
              bb:=byte(AnsiChar(w[j+i*2+1]));
              if (bb and $F0)=$30 then inc(o[i],bb and $F) else inc(o[i],(9+bb) and $F);
             end;
            stmWrite(@o[0],12);
           end
          else
          //detect javascript
          if (Copy(w,1,Length(bsonJavaScriptCodePrefix))=bsonJavaScriptCodePrefix) then
           begin
            i:=bsonJavaScriptCode;
            stmWrite(@i,1);
            stmWriteCString(key);
            stmWriteString(Copy(w,Length(bsonJavaScriptCodePrefix)+1,Length(w)-Length(bsonJavaScriptCodePrefix)));
           end
          else
          //detect regex
          if (Copy(w,1,Length(bsonRegExPrefix))=bsonRegExPrefix) then
           begin
            i:=bsonRegEx;
            stmWrite(@i,1);
            stmWriteCString(key);
            i:=Length(bsonRegExPrefix)+1;
            if (i<=Length(w)) and (w[i]='/') then inc(i);//TODO: support alternate regex delimiter?
            j:=i;
            while (j<=Length(w)) and (w[i]<>'/') do inc(j);
            stmWriteCString(Copy(w,i,j-i));
            stmWriteCString(Copy(w,j+1,Length(w)-j));
           end
          else
           begin
            i:=bsonString;
            stmWrite(@i,1);
            stmWriteCString(key);
            stmWriteString(w);
           end;
          //TODO: bsonJavaScriptCodeWithScope, bsonSymbol ?
         end;
        //TODO varStrArg as bsonBinaryUUID
        varBoolean:
         begin
          i:=bsonBoolean;
          stmWrite(@i,1);
          stmWriteCString(key);
          if v then i:=1 else i:=0;
          stmWrite(@i,1);
         end;
        //varVariant://TODO
        varDispatch,varUnknown:
         begin
          uu:=IUnknown(v);
          if uu<>nil then
          if not TryWriteBSONDocument then
          {$IFDEF BSON_SUPPORT_REGEX}
          if not TryWriteRegExp then
          {$ENDIF}
          if not TryWriteStream then
          if not TryWritePersistStream then
            raise EBSONException.Create('No supported interface found on object "'+key+'"');
         end;
        else raise EBSONException.Create('Unsupported variant type '+IntToHex(vt,4)+' "'+key+'"');
      end;
     end;

    while (vindex>=0) and (vstack[vindex].vi>vstack[vindex].v2) do
     begin
      //terminator
      i:=0;
      stmWrite(@i,1);
      //write total length
      OleCheck(stm.Seek(lstart+vstack[vindex].vstart,soFromBeginning,lx));
      i:=ltotal-vstack[vindex].vstart;
      OleCheck(stm.Write(@i,4,@li));
      //return to end position
      OleCheck(stm.Seek(lstart+ltotal,soFromBeginning,lx));
      //pop from array stack
      vstack[vindex].vv:=Null;
      dec(vindex);
      //SetLength(vstack,vindex);
     end;

   end;
  //terminator
  i:=0;
  stmWrite(@i,1);

  //write total length
  OleCheck(stm.Seek(lstart,soFromBeginning,lx));
  OleCheck(stm.Write(@ltotal,4,@li));
  //assert(li=4);

  //return to end position
  OleCheck(stm.Seek(ltotal-4,soFromCurrent,lx));
  //assert(lx=lstart+ltotal);

  if fClearDirty then FDirty:=false;
  Result:=S_OK;
  //TODO: try except Result:=E_?
end;

function TBSONDocument.GetClassID(out classID: TCLSID): HResult;
begin
  classID:=CLASS_BSONDocument;
  Result:=S_OK;
end;

procedure TBSONDocument.Clear;
var
  i:integer;
begin
  FDirty:=false;//?
  for i:=0 to FElementIndex-1 do VarClear(FValues[i]);
  //TODO: if IBSONDocument then Clear?
  FElementIndex:=0;//?
  FLastIndex:=0;
end;

function TBSONDocument.ToVarArray: OleVariant;
var
  i:integer;
begin
  Result:=VarArrayCreate([0,FElementIndex-1,0,1],varVariant);
  for i:=0 to FElementIndex-1 do
   begin
    Result[i,0]:=FKeys[i].Key;
    Result[i,1]:=FValues[FKeys[i].ValueIndex];
   end;
end;

function BSON:IBSONDocument; //overload;
begin
  Result:=TBSONDocument.Create as IBSONDocument;
end;

function BSON(x:array of OleVariant):IBSONDocument; //overload;
var
  i,di,l:integer;
  d:array of IBSONDocument;
  key:WideString;
const
  GrowStep=8;
begin
  i:=0;
  l:=Length(x);
  di:=0;
  SetLength(d,8);
  d[di]:=TBSONDocument.Create as IBSONDocument;
  while i<l do
   begin
    //key
    key:=VarToStr(x[i]);
    if key=']' then
     begin
      //pop from stack
      d[di]:=nil;
      dec(di);
     end
    else
     begin
      if key='[' then raise Exception.Create('BSON builder: embedded document needs key at index '+IntToStr(i)); 
      //value
      inc(i);
      if (VarType(x[i])=varOleStr) and (x[i]='[') then
       begin
        //push on stack
        inc(di);
        if di=Length(d) then SetLength(d,di+GrowStep);
        d[di]:=TBSONDocument.Create as IBSONDocument;
        d[di-1].Item[key]:=d[di];
       end
      else
        if i<l then
          d[di].Item[key]:=x[i]
        else
          raise Exception.Create('BSON builder: last key is missing value');
     end;
    inc(i);
   end;
  //if di>0 then raise Exception.Create('BSON builder: '+IntToStr(di)+' closing brackets missing');?
  Result:=d[0];
end;

end.
