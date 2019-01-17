{

jsonDoc.pas

Copyright 2015-2019 Stijn Sanders
Made available under terms described in file "LICENSE"
https://github.com/stijnsanders/jsonDoc

v1.2.0

}
unit jsonDoc;

{$WARN SYMBOL_PLATFORM OFF}
{$D-}
{$L-}

{

Options:
Define here or in the project settings

  JSONDOC_JSON_STRICT
    to disallow missing quotes around key names

  JSONDOC_JSON_LOOSE
    to allow missing colons and comma's

  JSONDOC_JSON_PASCAL_STRINGS
    to allow pascal-style strings

  JSONDOC_P2
    to combine JSONDOC_JSON_LOOSE and JSONDOC_JSON_PASCAL_STRINGS

  JSONDOC_STOREINDENTING
    to make ToString write indentation EOL's and tabs

  JSONDOC_THREADSAFE
    to make IJSONDocument instances thread-safe

  JSONDOC_DEFAULT_USE_IJSONARRAY
    to set JSON_UseIJSONArray to true by default

}

interface

uses SysUtils;

const
  //COM GUID's
  IID_IJSONDocument
    : TGUID = '{4A534F4E-0001-0001-C000-000000000001}';
  CLASS_JSONDocument
    : TGUID = '{4A534F4E-0001-0002-C000-000000000002}';
  IID_IJSONEnumerator
    : TGUID = '{4A534F4E-0001-0003-C000-000000000003}';
  IID_IJSONEnumerable
    : TGUID = '{4A534F4E-0001-0004-C000-000000000004}';
  IID_IJSONArray
    : TGUID = '{4A534F4E-0001-0005-C000-000000000005}';
  IID_IJSONDocArray
    : TGUID = '{4A534F4E-0001-0006-C000-000000000006}';
  IID_IJSONDocWithReUse
    : TGUID = '{4A534F4E-0001-0007-C000-000000000007}';

type
{
  IJSONDocument interface
  the base JSON document interface that provides access to a set of
  key-value pairs.
  use AsString and Parse to convert JSON to and from string values.
  use AsVarArray to access the key-value pairs as a [x,2] variant array.
  use Clear to re-use a JSON doc for parsing or building a new similar
  document and keep the allocated memory for keys and values.
  see also: JSON function
}
  IJSONDocument = interface(IUnknown)
    ['{4A534F4E-0001-0001-C000-000000000001}']
    function Get_Item(const Key: WideString): Variant; stdcall;
    procedure Set_Item(const Key: WideString; const Value: Variant); stdcall;
    procedure Parse(const JSONData: WideString); stdcall;
    function ToString: WideString; stdcall;
    function ToVarArray: Variant; stdcall;
    procedure Clear; stdcall;
    procedure Delete(const Key: WideString); stdcall;
    property Item[const Key: WideString]: Variant
      read Get_Item write Set_Item; default;
    property AsString: WideString read ToString write Parse;
  end;

{
  IJSONEnumerator interface
  use IJSONEnumerator to enumerate a document's key-value pairs
  see also: JSONEnum function
}
  //TODO: IEnumVariant?
  IJSONEnumerator = interface(IUnknown)
    ['{4A534F4E-0001-0003-C000-000000000003}']
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
    property Key: WideString read Get_Key;
    property Value: Variant read Get_Value write Set_Value;
  end;

{
  IJSONEnumerable interface
  used to get a IJSONEnumerable instance for a document
  see also: JSONEnum function
}
  IJSONEnumerable = interface(IUnknown)
    ['{4A534F4E-0001-0004-C000-000000000004}']
    function NewEnumerator: IJSONEnumerator; stdcall;
  end;

{
  IJSONArray interface
  When using VarArrayOf (declared in Variants.pas), a SafeArray is
  used internally for storage, but as a variant value VarCopy is called
  with each use, creating duplicate (deep) copies of the SafeArray.
  Use IJSONArray (and the ja function) to create a IJSONArray instance
  that uses a single copy of the data. It is also reference counted,
  which automates memory clean-up.
}
  IJSONArray = interface(IUnknown)
    ['{4A534F4E-0001-0005-C000-000000000005}']
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function Count: integer; stdcall;
    function ToString: WideString; stdcall;
    function v0(Index: integer): pointer; stdcall;
    property Item[Idx: integer]: Variant read Get_Item write Set_Item; default;
  end;

{
  IJSONDocArray interface
  use IJSONDocArray to build an array of similar documents,
  ideally in combination with a single IJSONDocument instance and
  IJSONDocument.Clear to re-use the memory allocated for keys and values
  see also: JSONDocArr function
}
  IJSONDocArray = interface(IJSONArray)
    ['{4A534F4E-0001-0006-C000-000000000006}']
    function Add(const Doc: IJSONDocument): integer; stdcall;
    function AddJSON(const Data: WideString): integer; stdcall;
    procedure LoadItem(Index: integer; const Doc: IJSONDocument); stdcall;
    procedure Clear; stdcall;
    function GetJSON(Index: integer): WideString; stdcall;
  end;

{
  IJSONDocWithReUse interface
  used internally to enable re-use of allocated keys
  see also: TJSONDocument Parse and Clear
}
  IJSONDocWithReUse = interface(IUnknown)
    ['{4A534F4E-0001-0007-C000-000000000007}']
    function ReUse(const Key: WideString): Variant; stdcall;
  end;

{
  JSON function: JSON document factory
  call JSON without parameters do create a new blank document
}
function JSON: IJSONDocument; overload;

{
  JSON function: JSON document builder
  pass an array of alternately keys and values,
  suffix key with opening brace to start an embedded document,
  and key of a single closing brace to close it.
}
function JSON(const x: array of Variant): IJSONDocument; overload;

{
  JSON function: JSON document converter
  pass a single variant to have it converted to an IJSONDocument interface
  or a string with JSON parsed into a IJSONDocument
  or nil when VarIsNull
}
function JSON(const x: Variant): IJSONDocument; overload;

{
  JSONEnum function
  get a new enumerator to enumeratare the key-value pairs in the document
}
function JSONEnum(const x: IJSONDocument): IJSONEnumerator; overload; //inline;
function JSONEnum(const x: Variant): IJSONEnumerator; overload;
function JSON(const x: IJSONEnumerator): IJSONDocument; overload; //inline;
function JSONEnum(const x: IJSONEnumerator): IJSONEnumerator; overload; //inline;

{
  ja function
  create and populate a new IJSONArray instance
  or cast a Variant holding a JSONArray instance to the interface reference
}
function ja(const Items:array of Variant): IJSONArray; overload;
function ja(const Item:Variant): IJSONArray; overload;

{
  JSONDocArray function
  get a new IJSONDocArray instance
}
function JSONDocArray: IJSONDocArray; overload;
function JSONDocArray(const Items:array of IJSONDocument): IJSONDocArray; overload;


{
  JSON_UseIJSONArray
  switch JSON so it will create IJSONArray instances to hold arrays of values
  instead of VarArrayCreate, default false
  see also TJSONDocument.UseIJSONArray property
}
var
  JSON_UseIJSONArray: boolean;

{
  TJSONImplBaseObj
  common base object JSON implementation objects inherit
  don't use directly
}
type

{$IFDEF JSONDOC_THREADSAFE}
  TJSONImplBaseObj = class(TInterfacedObject)
  protected
    FLock:TRTLCriticalSection;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

{$ELSE}
  //thread-unsafe anyway, so avoid locking when reference counting:
  TJSONImplBaseObj = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

{$ENDIF}

{
  TJSONDocument class
  the default IJSONDocument implementation
  see also: JSON function
}
  TJSONDocument = class(TJSONImplBaseObj, IJSONDocument, IJSONEnumerable,
    IJSONDocWithReUse)
  private
    FElementIndex,FElementSize:integer;
    FElements:array of record
      SortIndex,LoadIndex:integer;
      Key:WideString;
      Value:Variant;
    end;
    FLoadIndex:integer;
    {$IFNDEF JSONDOC_THREADSAFE}
    FGotIndex,FGotSorted:integer;
    FGotMatch:boolean;
    {$ENDIF}
    FUseIJSONArray:boolean;
    function GetKeyIndex(const Key: WideString;
      var GotIndex: integer; var GotSorted: integer): boolean;
  protected
    function Get_Item(const Key: WideString): Variant; stdcall;
    procedure Set_Item(const Key: WideString; const Value: Variant); stdcall;
    function ReUse(const Key: WideString): Variant; stdcall;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Parse(const JSONData: WideString); stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONDocument.ToString=JSONToString;
    function ToVarArray: Variant; stdcall;
    procedure Clear; stdcall;
    function NewEnumerator: IJSONEnumerator; stdcall;
    procedure Delete(const Key: WideString); stdcall;
    property Item[const Key: WideString]: Variant
      read Get_Item write Set_Item; default;
    property AsString: WideString read JSONToString write Parse;
    property UseIJSONArray:boolean read FUseIJSONArray write FUseIJSONArray;
  end;

{
  TJSONEnumerator class
  the default IJSONEnumerator implementation
  see also: JSONEnum function
}
  TJSONEnumerator = class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:TJSONDocument;
    FIndex: integer;
  public
    constructor Create(Data: TJSONDocument);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  end;

{
  TJSONArray class
  Default ILightArray implementation
}
  TJSONArray = class(TJSONImplBaseObj, IJSONArray, IJSONEnumerable)
  private
    FData:array of Variant;
  protected
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function Count: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
    function NewEnumerator: IJSONEnumerator; stdcall;
  public
    constructor Create(Size: integer);
  end;

{
  TJSONArrayEnumerator
  an IJSONEnumerator implementation that iterates over a variant array
  used internally to convert to JSON
}
  TJSONArrayEnumerator= class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:IJSONArray;
    FIndex,FMax:integer;
  public
    constructor Create(const Data:IJSONArray);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  end;

{
  TJSONDocArray class
  the default IJSONDocArray implementation
  see also: JSONDocArray function
}
  TJSONDocArray = class(TJSONImplBaseObj, IJSONArray, IJSONDocArray)
  private
    FItems:array of WideString;
    FItemsCount,FItemsSize,FCurrentIndex:integer;
    FCurrent:Variant;
  protected
    //IJSONArray
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function Count: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
    //function IJSONArray.ToString=JSONToString;
    //IJSONDocArray
    function Add(const Doc: IJSONDocument): integer; stdcall;
    function AddJSON(const Data: WideString): integer; stdcall;
    procedure LoadItem(Index: integer; const Doc: IJSONDocument); stdcall;
    function IJSONDocArray.ToString=JSONToString;
    procedure Clear; stdcall;
    function GetJSON(Index: integer): WideString; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{
  TVarArrayEnumerator
  an IJSONEnumerator implementation that iterates over a variant array
  used internally to convert to JSON
}
  TVarArrayEnumerator = class(TJSONImplBaseObj, IJSONEnumerator)
  private
    FData:PVariant;
    FCurrent:Variant;
    FIndex,FMax,FCurrentIndex:integer;
  public
    constructor Create(const Data:PVariant);
    destructor Destroy; override;
    function EOF: boolean; stdcall;
    function Next: boolean; stdcall;
    function Get_Key: WideString; stdcall;
    function Get_Value: Variant; stdcall;
    procedure Set_Value(const Value: Variant); stdcall;
    function v0: pointer; stdcall;
  end;

{
  TVarJSONArray
  an IJSONArray implementation that works on an existing variant array
  used internally by the ja function
}
  TVarJSONArray = class(TJSONImplBaseObj, IJSONArray)
  private
    FData,FCurrent:Variant;
    v1,v2,FCurrentIndex:integer;
  protected
    function Get_Item(Index: integer): Variant; stdcall;
    procedure Set_Item(Index: integer; const Value: Variant); stdcall;
    function Count: integer; stdcall;
    function JSONToString: WideString; stdcall;
    function IJSONArray.ToString=JSONToString;
    function v0(Index: integer): pointer; stdcall;
  public
    constructor Create(const Data: Variant);
    constructor CreateNoCopy(var Data: Variant);
    destructor Destroy; override;
  end;

{
  EJSONException class types
  exception types thrown from TJSONDocument's Parse and ToString
}
  EJSONException=class(Exception);
  EJSONDecodeException=class(EJSONException);
  EJSONEncodeException=class(EJSONException);


implementation

uses Variants, Windows;

procedure VarMove(var Dest, Src: Variant);
begin
  //Dest:=Src;VarClear(Src);
  Move(Src,Dest,SizeOf(TVarData));
  ZeroMemory(@Src,SizeOf(TVarData));
end;

{ TJSONImplBaseObj }

{$IFDEF JSONDOC_THREADSAFE}

procedure TJSONImplBaseObj.AfterConstruction;
begin
  inherited;
  InitializeCriticalSection(FLock);
end;

destructor TJSONImplBaseObj.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

{$ELSE}

procedure TJSONImplBaseObj.AfterConstruction;
begin
  inherited;
  dec(FRefCount);//see constructor
end;

procedure TJSONImplBaseObj.BeforeDestruction;
begin
  inherited;
  if RefCount<>0 then System.Error(reInvalidPtr);
end;

class function TJSONImplBaseObj.NewInstance: TObject;
begin
  Result:=inherited NewInstance;
  //see AfterConstruction, prevent detroy while creating
  TJSONImplBaseObj(Result).FRefCount:=1;
end;

function TJSONImplBaseObj.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID,Obj) then Result:=0 else Result:=E_NOINTERFACE;
end;

function TJSONImplBaseObj._AddRef: Integer;
begin
  inc(FRefCount);
  Result:=FRefCount;
end;

function TJSONImplBaseObj._Release: Integer;
begin
  dec(FRefCount);
  Result:=FRefCount;
  if Result=0 then Destroy;
end;

{$ENDIF}

{ TJSONDocument }

procedure TJSONDocument.AfterConstruction;
begin
  inherited;
  FElementIndex:=0;
  FElementSize:=0;
  FLoadIndex:=0;
  {$IFNDEF JSONDOC_THREADSAFE}
  FGotIndex:=0;
  FGotSorted:=0;
  FGotMatch:=false;
  {$ENDIF}
  FUseIJSONArray:=JSON_UseIJSONArray;
end;

destructor TJSONDocument.Destroy;
var
  i:integer;
begin
  for i:=0 to FElementIndex-1 do VarClear(FElements[i].Value);
  inherited;
end;

function TJSONDocument.GetKeyIndex(const Key: WideString;
  var GotIndex: integer; var GotSorted: integer): boolean;
var
  a,b,c,d,x:integer;
begin
  //case sensitivity?
  {$IFNDEF JSONDOC_THREADSAFE}
  //check last getindex, speeds up set right after get
  if FGotMatch and (CompareStr(Key,FElements[FGotIndex].Key)=0) then
   begin
    //assert FGotIndex=FSorted[FGotSorted];
    GotIndex:=FGotIndex;
    GotSorted:=FGotSorted;
    Result:=true;
   end
  else
  {$ENDIF}
   begin
    a:=0;
    b:=FElementIndex-1;
    d:=FElementIndex;
    Result:=false;//default
    while b>=a do
     begin
      c:=(a+b) div 2;
      d:=FElements[c].SortIndex;
      //if c=a? c=b?
      x:=CompareStr(Key,FElements[d].Key);
      if x=0 then
       begin
        a:=c;
        b:=c-1;
        Result:=true;
       end
      else
        if x<0 then
          if b=c then dec(b) else b:=c
        else
          if a=c then inc(a) else a:=c;
     end;
    GotSorted:=a;
    GotIndex:=d;
    {$IFNDEF JSONDOC_THREADSAFE}
    FGotSorted:=a;
    FGotIndex:=d;
    FGotMatch:=Result;
    {$ENDIF}
   end;
end;

function TJSONDocument.Get_Item(const Key: WideString): Variant;
var
  GotIndex,GotSorted:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Self<>nil) and GetKeyIndex(Key,GotIndex,GotSorted)
      and (FElements[GotIndex].LoadIndex=FLoadIndex) then
      Result:=FElements[GotIndex].Value
    else
      Result:=Null;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.ReUse(const Key: WideString): Variant;
var
  GotIndex,GotSorted:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Self<>nil) and GetKeyIndex(Key,GotIndex,GotSorted) then
     begin
      FElements[GotIndex].LoadIndex:=FLoadIndex;
      Result:=FElements[GotIndex].Value;
     end
    else
      Result:=Null;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.Set_Item(const Key: WideString; const Value: Variant);
var
  i,GotIndex,GotSorted:integer;
const
  GrowStep=$20;//not too much, not too little (?)
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //if ((VarType(Value) and varArray)<>0) and (VarArrayDimCount(v)>1) then
    //  raise EJSONException.Create(
    //    'VarArray: multi-dimensional arrays not supported');
    if not GetKeyIndex(Key,GotIndex,GotSorted) then
     begin
      if FElementIndex=FElementSize then
       begin
        inc(FElementSize,GrowStep);
        SetLength(FElements,FElementSize);
       end;
      for i:=FElementIndex-1 downto GotSorted do
        FElements[i+1].SortIndex:=FElements[i].SortIndex;
      GotIndex:=FElementIndex;
      inc(FElementIndex);
      FElements[GotSorted].SortIndex:=GotIndex;
      FElements[GotIndex].Key:=Key;
     end;
    FElements[GotIndex].Value:=Value;
    FElements[GotIndex].LoadIndex:=FLoadIndex;
    //FDirty:=true;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{$IFDEF JSONDOC_P2}
{$DEFINE JSONDOC_JSON_LOOSE}
{$DEFINE JSONDOC_JSON_PASCAL_STRINGS}
{$ENDIF}

procedure TJSONDocument.Parse(const JSONData: WideString);
var
  i,l:integer;
  function SkipWhiteSpace:WideChar;
  begin
    while (i<=l) and (jsonData[i]<=' ') do inc(i);
    if i<=l then Result:=jsonData[i] else Result:=#0;
  end;
  function ExVicinity(di:integer):WideString;
  const
    VicinityExtent=12;
  var
    i:integer;
  begin
    if di<=VicinityExtent then
      Result:=#13#10'(#'+IntToStr(di)+')"'+Copy(jsonData,1,di-1)+
        ' >>> '+jsonData[di]+' <<< '+Copy(jsonData,di+1,VicinityExtent)+'"'
    else
      Result:=#13#10'(#'+IntToStr(di)+')"...'+
        Copy(jsonData,di-VicinityExtent,VicinityExtent)+
        ' >>> '+jsonData[di]+' <<< '+Copy(jsonData,di+1,VicinityExtent)+'"';
    for i:=1 to Length(Result) do
      if word(Result[i])<32 then Result[i]:='|';
  end;
  procedure Expect(c:WideChar;const msg:string);
  begin
    while (i<=l) and (jsonData[i]<=' ') do inc(i);
    if (i<=l) and (jsonData[i]=c) then
      inc(i)
    else
    {$IFDEF JSONDOC_JSON_LOOSE}
      ;
    {$ELSE}
      raise EJSONDecodeException.Create(msg+ExVicinity(i));
    {$ENDIF}
  end;
    procedure GetStringIndexes(var i1,i2:integer);
  begin
    //assert jsonData[i]='"'
    i1:=i;
    inc(i);
    while (i<=l) and (jsonData[i]<>'"') do
     begin
      if jsonData[i]='\' then inc(i);//just skip all to skip any '"'
      inc(i);
     end;
    i2:=i;
    inc(i);
  end;
  {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
  procedure GetPascalIndexes(var i1,i2:integer);
  begin
    i1:=i;
    while (i<=l) and ((jsonData[i]='''') or (jsonData[i]='#')) do
      if jsonData[i]='''' then
       begin
        inc(i);
        while (i<=l) and (jsonData[i]<>'''') do inc(i);
        if i<=l then inc(i);
       end
      else
       begin
        inc(i);
        if (i<=l) and (jsonData[i]='$') then
         begin
          inc(i);
          while (i<=l) and (word(jsonData[i]) in [$30..$39,$41..$5A,$61..$7A]) do inc(i);
         end
        else
          while (i<=l) and (word(jsonData[i]) in [$30..$39]) do inc(i);
       end;
    i2:=i;
  end;
  {$ENDIF}
  function GetStringValue(i1,i2:integer):WideString;
  var
    ii,di,u,v,w:integer;
  begin
    //assert i1<=l
    //assert i2<=l
    //assert i1<i2
    {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
    if (jsonData[i1]='''') or (jsonData[i1]='#') then
     begin
      SetLength(Result,i2-i1);
      ii:=1;
      di:=i1;
      while di<i2 do
       begin
        case AnsiChar(jsonData[di]) of
          '''':
           begin
            inc(di);
            u:=0;
            while (di<i2) and (u=0) do
             begin
              if jsonData[di]='''' then
               begin
                inc(di);
                if (di<=l) and (jsonData[di]='''') then
                 begin
                  Result[ii]:='''';
                  inc(ii);
                  inc(di);
                 end
                else
                  u:=1;
               end
              else
               begin
                Result[ii]:=jsonData[di];
                inc(ii);
                inc(di);
               end;
             end;
           end;
          '#':
           begin
            inc(di);
            if (di<i2) and (jsonData[di]='$') then
             begin
              w:=0;
              u:=0;
              inc(di);
              while (u<4) and (di<i2) and (word(jsonData[di]) in [$30..$39,$41..$5A,$61..$7A]) do
               begin
                if di=i2 then raise EJSONDecodeException.Create(
                  'JSON Incomplete espace sequence'+ExVicinity(di));
                v:=word(jsonData[di]);
                case v of
                  $30..$39:w:=(w shl 4) or (v and $F);
                  $41..$5A,$61..$7A:w:=(w shl 4) or ((v and $1F)+9);
                  else raise EJSONDecodeException.Create(
                    'JSON Invalid espace sequence'+ExVicinity(di));
                end;
                inc(di);
                inc(u);
               end;
              Result[ii]:=WideChar(w);
              inc(ii);
             end
            else
             begin
              w:=0;
              u:=0;
              while (u<5) and (di<i2) and (word(jsonData[di]) in [$30..$39]) do
               begin
                if di=i2 then raise EJSONDecodeException.Create(
                  'JSON Incomplete espace sequence'+ExVicinity(di));
                w:=w*10+(word(jsonData[di]) and $F);
                inc(di);
                inc(u);
               end;
              Result[ii]:=WideChar(w);
              inc(ii);
             end;
           end;
          else raise EJSONDecodeException.Create(
            'JSON Unknown pascal string syntax'+ExVicinity(di));
        end;
       end;
      SetLength(Result,ii-1);
     end
    else
    {$ENDIF}
     begin
      {$IFDEF JSONDOC_JSON_STRICT}
      //assert jsonData[i1]='"'
      //assert jsonData[i2]='"';
      inc(i1);
      {$ELSE}
      if jsonData[i1]='"' then inc(i1);
      {$ENDIF}
      SetLength(Result,i2-i1);
      ii:=1;
      di:=i1;
      while di<i2 do
       begin
        //assert ii<=Length(Result);
        if jsonData[di]='\' then
         begin
          inc(di);
          case AnsiChar(jsonData[di]) of
            '"','\','/':Result[ii]:=jsonData[di];
            'b':Result[ii]:=#8;
            't':Result[ii]:=#9;
            'n':Result[ii]:=#10;
            'f':Result[ii]:=#12;
            'r':Result[ii]:=#13;
            'x':
             begin
              inc(di);
              if di=i2 then raise EJSONDecodeException.Create(
                'JSON Incomplete espace sequence'+ExVicinity(di));
              v:=word(jsonData[di]);
              case v of
                $30..$39:w:=(v and $F) shl 4;
                $41..$5A,$61..$7A:w:=((v and $1F)+9) shl 4;
                else raise EJSONDecodeException.Create(
                  'JSON Invalid espace sequence'+ExVicinity(di));
              end;
              inc(di);
              if di=i2 then raise EJSONDecodeException.Create(
                'JSON Incomplete espace sequence'+ExVicinity(di));
              v:=word(jsonData[di]);
              case v of
                $30..$39:w:=w or (v and $F);
                $41..$5A,$61..$7A:w:=w or ((v and $1F)+9);
                else raise EJSONDecodeException.Create(
                  'JSON Invalid espace sequence'+ExVicinity(di));
              end;
              Result[ii]:=WideChar(w);
             end;
            'u':
             begin
              w:=0;
              for u:=0 to 3 do
               begin
                inc(di);
                if di=i2 then raise EJSONDecodeException.Create(
                  'JSON Incomplete espace sequence'+ExVicinity(di));
                v:=word(jsonData[di]);
                case v of
                  $30..$39:w:=(w shl 4) or (v and $F);
                  $41..$5A,$61..$7A:w:=(w shl 4) or ((v and $1F)+9);
                  else raise EJSONDecodeException.Create(
                    'JSON Invalid espace sequence'+ExVicinity(di));
                end;
               end;
              Result[ii]:=WideChar(w);
             end;
            else raise EJSONDecodeException.Create(
              'JSON Unknown escape sequence'+ExVicinity(di));
          end;
         end
        else
          Result[ii]:=jsonData[di];
        inc(di);
        inc(ii);
       end;
      SetLength(Result,ii-1);
     end;
  end;
const
  stackGrowStep=$20;//not too much, not too little (?)
  arrGrowStep=$20;
var
  IsArray:boolean;
  k1,k2,v1,v2,a1,ai,al:integer;
  d:IJSONDocument;
  a:array of Variant;
  at,vt:TVarType;
  procedure SetValue(const v:Variant);
  begin
    //assert da=nil
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
      case at of
        varEmpty:
          at:=vt;
        varShortInt,varByte://i1,u1
          case vt of
            varSmallInt,varInteger,varSingle,varDouble,
            varLongWord,varInt64,$0015:
              at:=vt;
            varShortInt:
              ;//at:=varShortInt;
            else
              at:=varVariant;
          end;
        varSmallint,varWord://i2,u2
          case vt of
            varInteger,varSingle,varDouble,varLongWord,varInt64,$0015:
              at:=vt;
            varSmallInt,
            varShortInt,varByte,varWord:
              ;//at:=varSmallInt;
            else
              at:=varVariant;
          end;
        varInteger,varLongWord://i4,u4
          case vt of
            varSingle,varDouble,varInt64,$0015:
              at:=vt;
            varSmallInt,varInteger,
            varShortInt,varByte,varWord,varLongWord:
              ;//at:=varInteger;
            else
              at:=varVariant;
          end;
        varInt64,$0015://i8
          case vt of
            varSingle,varDouble:
              at:=vt;
            varSmallInt,varInteger,
            varShortInt,varByte,varWord,varLongWord,varInt64,$0015:
              ;//at:=varInt64;
            else
              at:=varVariant;
          end;
        varSingle:
          case vt of
            varDouble:
              at:=vt;
            varSmallInt,varInteger,varSingle,
            varShortInt,varByte,varWord,varLongWord:
              ;//at:=varSingle
            else
              at:=varVariant;
          end;
        varDouble:
          case vt of
            varSmallInt,varInteger,varSingle,varDouble,
            varShortInt,varByte,varWord,varLongWord:
              ;//at:=varDouble
            else
              at:=varVariant;
          end;
        varVariant:
          ;//Already creating an VarArray of varVariant
        else
          if at<>vt then at:=varVariant;
      end;
      inc(ai);
     end
    else
      d[GetStringValue(k1,k2)]:=v
  end;
var
  firstItem,b:boolean;
  stack:array of record
    k1,k2:integer;
    d:IJSONDocument;
  end;
  stackIndex,stackSize:integer;
  ods:char;
  key:WideString;
  d1:IJSONDocument;
  dr:IJSONDocWithReUse;
  da:IJSONDocArray;
  aa:TJSONArray;
  da0,da1:integer;
  v:Variant;
  v64:int64;
  procedure CheckValue;
  begin
    //assert da<>nil
    if stackIndex=da0 then
      raise EJSONDecodeException.Create('IJSONDocArray: non-document element in array');
  end;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //Clear;? let caller decide.
    i:=1;
    l:=Length(jsonData);
    //object starts
    Expect('{','JSON doesn''t define an object, "{" expected.');
    stackSize:=0;
    stackIndex:=0;
    ai:=0;
    a1:=0;
    al:=0;
    da:=nil;
    da0:=0;
    da1:=0;
    IsArray:=false;
    firstItem:=true;

    {$if CompilerVersion >= 24}
    ods:=FormatSettings.DecimalSeparator;
    {$else}
    ods:=DecimalSeparator;
    {$ifend}

    try

      {$if CompilerVersion >= 24}
      FormatSettings.DecimalSeparator:='.';
      {$else}
      DecimalSeparator:='.';
      {$ifend}

      d:=Self;
      //main loop over key/values and nested objects/arrays
      while (i<=l) and (stackIndex<>-1) do
       begin
        if firstItem then firstItem:=false else
          Expect(',','JSON element not delimited by comma');
        if not(IsArray) and (SkipWhiteSpace<>'}') then
         begin
          //key string
          case AnsiChar(SkipWhiteSpace) of
            '"':
              GetStringIndexes(k1,k2);
            {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
            '''','#':
              GetPascalIndexes(k1,k2);
            {$ENDIF}
            else
            {$IFDEF JSONDOC_JSON_STRICT}
              raise EJSONDecodeException.Create(
                'JSON key string not enclosed in double quotes'+ExVicinity(i));
            {$ELSE}
             begin
              k1:=i;
              while (i<=l) and (jsonData[i]>' ') and not(
                (jsonData[i]=':') or (jsonData[i]='"')
                {$IFDEF JSONDOC_JSON_LOOSE}
                or (jsonData[i]='{') or (jsonData[i]='[')
                or (jsonData[i]='=') or (jsonData[i]=';')
                {$ENDIF}
                ) do inc(i);
              k2:=i;
             end;
            {$ENDIF}
          end;
          Expect(':','JSON key, value not separated by colon');
          {$IFDEF JSONDOC_JSON_LOOSE}
          if (i<=l) and (jsonData[i]='=') then inc(i);
          {$ENDIF}
         end;
        //value
        case AnsiChar(SkipWhiteSpace) of
          '{','['://object or array
           begin
            b:=IsArray;
            if jsonData[i]='{' then
             begin
              //an object starts
              if da=nil then
                if IsArray then
                 begin
                  if ai=al then
                   begin
                    inc(al,arrGrowStep);//not too much, not too little (?)
                    SetLength(a,al);
                   end;
                  v:=JSON;
                  a[ai]:=v;
                  //detect same type elements array
                  if at=varEmpty then at:=varUnknown else
                    if at<>varUnknown then at:=varVariant;
                  inc(ai);
                 end
                else
                 begin
                  key:=GetStringValue(k1,k2);
                  if d.QueryInterface(IID_IJSONDocWithReUse,dr)=S_OK then
                   begin
                    v:=dr.ReUse(key);
                    dr:=nil;
                   end
                  else
                    v:=Null;
                  if (TVarData(v).VType in [varDispatch,varUnknown]) and
                    (TVarData(v).VUnknown<>nil) and
                    (IUnknown(v).QueryInterface(IID_IJSONDocument,d1)=S_OK) then
                    d1:=nil
                  else
                   begin
                    v:=JSON;
                    d[key]:=v;
                   end;
                 end
              else
                if da0=stackIndex then da1:=i;
              IsArray:=false;
             end
            else
             begin
              //an array starts
              if da=nil then
                if d.QueryInterface(IID_IJSONDocWithReUse,dr)=S_OK then
                 begin
                  key:=GetStringValue(k1,k2);
                  v:=dr.ReUse(key);
                  dr:=nil;
                  if (TVarData(v).VType in [varDispatch,varUnknown]) and
                    (TVarData(v).VUnknown<>nil) and
                    (IUnknown(v).QueryInterface(IID_IJSONDocArray,da)=S_OK) then
                   begin
                    da0:=stackIndex+1;
                    da1:=0;//see first '{' above
                   end;
                 end;
              IsArray:=true;
             end;
            inc(i);
            //push onto stack
            if stackIndex=stackSize then
             begin
              inc(stackSize,stackGrowStep);
              SetLength(stack,stackSize);
             end;
            if b then //if WasArray then
             begin
              stack[stackIndex].k1:=a1;
              stack[stackIndex].k2:=at;
              stack[stackIndex].d:=nil;
             end
            else
             begin
              stack[stackIndex].k1:=k1;
              stack[stackIndex].k2:=k2;
              stack[stackIndex].d:=d;
             end;
            inc(stackIndex);
            firstItem:=true;
            if da=nil then
              if IsArray then
               begin
                a1:=ai;
                at:=varEmpty;//used to detect same type elements array
               end
              else
                d:=IUnknown(v) as IJSONDocument;
           end;

          '}',']':;//empty object or array, drop into close array below

          '"'://string
           begin
            GetStringIndexes(v1,v2);
            if da=nil then
              SetValue(GetStringValue(v1,v2))
            else
              CheckValue;
           end;

          {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
          '''','#'://pascal-style string
           begin
            GetPascalIndexes(v1,v2);
            if da=nil then
              SetValue(GetStringValue(v1,v2))
            else
              CheckValue;
           end;
          '$'://pascal-style hex digit
           begin
            inc(i);
            v1:=i;
            v64:=0;
            while (i<=l) and (word(jsonData[i]) in [$30..$39,$41..$5A,$61..$7A]) do
             begin
              case word(jsonData[i]) of
                $30..$39:v64:=(v64 shl 4) or (word(jsonData[i]) and $F);
                $41..$5A,$61..$7A:v64:=(v64 shl 4) or ((word(jsonData[i]) and $1F)+9);
                else raise EJSONDecodeException.Create(
                  'JSON Invalid espace sequence'+ExVicinity(i));
              end;
              inc(i);
             end;
            if i=v1 then
              raise EJSONDecodeException.Create(
                'JSON Unrecognized value type'+ExVicinity(i));
            if v64>=$80000000 then //int64
              SetValue(v64)
            else if v64>=$80 then //int32
              SetValue(integer(v64))
            else //int8
              SetValue(SmallInt(v64));
           end;
          {$ENDIF}

          '0'..'9','-'://number
           begin
            b:=jsonData[i]='-';
            v1:=i;
            if b then inc(i);
            if da=nil then
             begin
              v64:=0;
              while (i<=l) and (word(jsonData[i]) in [$30..$39]) do
               begin
                v64:=v64*10+(word(jsonData[i]) and $F);//TODO: detect overflow
                inc(i);
               end;
              if AnsiChar(jsonData[i]) in ['.','e','E'] then
               begin
                //float
                inc(i);
                while (i<=l) and (AnsiChar(jsonData[i]) in
                  ['0'..'9','-','+','e','E']) do inc(i);
                //try except EConvertError?
                SetValue(StrToFloat(Copy(jsonData,v1,i-v1)));
               end
              else
               begin
                //integer
                if v64>=$80000000 then //int64
                  if b then SetValue(-v64) else SetValue(v64)
                else if v64>=$80 then //int32
                  if b then SetValue(-integer(v64)) else SetValue(integer(v64))
                else //int8
                  if b then SetValue(-SmallInt(v64)) else SetValue(SmallInt(v64));
               end;
             end
            else
             begin
              //skip
              CheckValue;
              while (i<=l) and (word(jsonData[i]) in [$30..$39]) do inc(i);
              if AnsiChar(jsonData[i]) in ['.','e','E'] then
               begin
                inc(i);
                while (i<=l) and (AnsiChar(jsonData[i]) in
                  ['0'..'9','-','+','e','E']) do inc(i);
               end;
             end;
           end;

          {$IFDEF JSONDOC_JSON_LOOSE}
          ';':inc(i);
          else
           begin
            v1:=i;
            while (i<=l) and (jsonData[i]>' ') and not(
              (jsonData[i]=':') or (jsonData[i]=',') or (jsonData[i]='"')
              or (jsonData[i]='}') or (jsonData[i]=']')
              {$IFDEF JSONDOC_JSON_PASCAL_STRINGS}
              or (jsonData[i]='''')
              {$ENDIF}
              ) do inc(i);
            v2:=i;
            if v1=v2 then
              raise EJSONDecodeException.Create(
                'JSON Value expected'+ExVicinity(i));
            if da=nil then
              if (v2-v1=4) and (jsonData[v1]='t') and (jsonData[v1+1]='r') and
                (jsonData[v1+2]='u') and (jsonData[v1+3]='e') then
                SetValue(true)
              else
              if (v2-v1=5) and (jsonData[v1]='f') and (jsonData[v1+1]='a') and
                (jsonData[v1+2]='l') and (jsonData[v1+3]='s') and (jsonData[v1+4]='e') then
                SetValue(false)
              else
              if (v2-v1=4) and (jsonData[v1]='n') and (jsonData[v1+1]='u') and
                (jsonData[v1+2]='l') and (jsonData[v1+3]='l') then
                SetValue(Null)
              else
                SetValue(GetStringValue(v1,v2))
            else
              CheckValue;
           end;
          {$ELSE}

          't'://true
           begin
            inc(i);
            Expect('r','JSON true misspelled');
            Expect('u','JSON true misspelled');
            Expect('e','JSON true misspelled');
            if da=nil then SetValue(true) else CheckValue;
           end;
          'f'://false
           begin
            inc(i);
            Expect('a','JSON false misspelled');
            Expect('l','JSON false misspelled');
            Expect('s','JSON false misspelled');
            Expect('e','JSON false misspelled');
            if da=nil then SetValue(false) else CheckValue;
           end;
          'n'://null
           begin
            inc(i);
            Expect('u','JSON null misspelled');
            Expect('l','JSON null misspelled');
            Expect('l','JSON null misspelled');
            if da=nil then SetValue(Null) else CheckValue;
            //TODO: support null in IJSONDocArray
           end;

          else
            raise EJSONDecodeException.Create(
              'JSON Unrecognized value type'+ExVicinity(i));
          {$ENDIF}
        end;
        if not firstItem then
         begin
          b:=true;
          while b do
           begin
            v:=Null;
            if IsArray then
              if SkipWhiteSpace=']' then
               begin
                if da=nil then
                 begin
                  if FUseIJSONArray then
                   begin
                    aa:=TJSONArray.Create(ai-a1);
                    k1:=a1;
                    k2:=0;
                    while k1<ai do
                     begin
                      //aa[k2]:=a[k1];VarClear(a[k1]);
                      VarMove(aa.FData[k2],a[k1]);
                      inc(k1);
                      inc(k2);
                     end;
                    v:=aa as IJSONArray;
                   end
                  else
                   begin
                    if not(VarTypeIsValidArrayType(at)) then at:=varVariant;
                    v:=VarArrayCreate([0,ai-a1-1],at);
                    k1:=a1;
                    k2:=0;
                    while k1<ai do
                     begin
                      v[k2]:=a[k1];
                      VarClear(a[k1]);
                      inc(k1);
                      inc(k2);
                     end;
                   end;
                  ai:=a1;
                 end;
               end
              else
                b:=false
            else
              b:=SkipWhiteSpace='}';
            if b then
             begin
              inc(i);
              //pop from stack
              if stackIndex=0 then
               begin
                //EndIndex:=i;
                dec(stackIndex);//stackindex:=-1;
                b:=false;
               end
              else
               begin
                dec(stackIndex);
                if stack[stackIndex].d=nil then
                 begin
                  a1:=stack[stackIndex].k1;
                  at:=stack[stackIndex].k2;
                  IsArray:=true;
                 end
                else
                 begin
                  if da=nil then d:=stack[stackIndex].d;
                  k1:=stack[stackIndex].k1;
                  k2:=stack[stackIndex].k2;
                  stack[stackIndex].d:=nil;
                  IsArray:=false;
                 end;
                if da<>nil then
                  if stackIndex=da0 then
                    da.AddJSON(Copy(jsonData,da1,i-da1))
                  else
                    if stackIndex=da0-1 then
                      da:=nil;//done
               end;
              //set array
              if (da=nil) and (TVarData(v).VType<>varNull) then SetValue(v);
             end;
           end;
         end;
       end;
      {$IFNDEF JSONDOC_JSON_LOOSE}
      if stackIndex<>-1 then raise EJSONDecodeException.Create(
        'JSON with '+IntToStr(stackIndex+1)+' objects or arrays not closed');
      {$ENDIF}
      if (i<=l) and (SkipWhiteSpace<>#0) then raise EJSONDecodeException.Create(
        'JSON has unexpected data after root document '+ExVicinity(i));
    finally
      {$if CompilerVersion >= 24}
      FormatSettings.DecimalSeparator:=ods;
      {$else}
      DecimalSeparator:=ods;
      {$ifend}
    end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function JSONEncodeStr(const xx:WideString):WideString;
const
  resGrowStep=$100;
  hex:array[0..15] of WideChar=(
    '0','1','2','3','4','5','6','7',
    '8','9','A','B','C','D','E','F');
var
  i,j,k,l:integer;
  w:word;
begin
  l:=Length(xx);
  SetLength(Result,l+2);
  Result[1]:='"';
  i:=1;
  j:=1;
  k:=l+2;
  while i<=l do
   begin
    w:=word(xx[i]);
    case w of
      0..31,word('"'),word('\'):
       begin
        if j+3>k then
         begin
          k:=((k div resGrowStep)+1)*resGrowStep;
          SetLength(Result,k);
         end;
        inc(j);
        Result[j]:='\';
        inc(j);
        case w of
          8:Result[j]:='b';
          9:Result[j]:='t';
          10:Result[j]:='n';
          12:Result[j]:='f';
          13:Result[j]:='r';
          word('"'),word('\'):Result[j]:=xx[i];
          else
           begin
            Result[j]:='u';
            if j+4>k then
             begin
              k:=((k div resGrowStep)+1)*resGrowStep;
              SetLength(Result,k);
             end;
            inc(j);Result[j]:=hex[w shr 12];
            inc(j);Result[j]:=hex[w shr 8 and $F];
            inc(j);Result[j]:=hex[w shr 4 and $F];
            inc(j);Result[j]:=hex[w and $F];
           end;
        end;
       end;
      else
       begin
        if j+2>k then
         begin
          k:=((k div resGrowStep)+1)*resGrowStep;
          SetLength(Result,k);
         end;
        inc(j);
        Result[j]:=WideChar(w);
       end;
    end;
    inc(i);
   end;
  inc(j);
  Result[j]:='"';
  SetLength(Result,j);
end;

{
function JSONVarToStr(const v: Variant):WideString;
begin
  if (TVarData(v).VType and varArray)=0 then
    Result:=JSONVarToStr1(v)
  else
    .....
end;
}

function JSONVarToStr1(const v: Variant):WideString;
var
  uu:IUnknown;
  d:IJSONDocument;
  da:IJSONArray;
begin
  case TVarData(v).VType and varTypeMask of
    varNull:Result:='null';
    varSmallint,varInteger,varShortInt,
    varByte,varWord,varLongWord,varInt64:
      Result:=VarToWideStr(v);
    varSingle,varDouble,varCurrency:
      Result:=FloatToStr(v);//?
    varDate:
      //Result:=FloatToStr(VarToDateTime(v));//?
      Result:='"'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz',
        VarToDateTime(v))+'"';
    varOleStr,varString,$0102:
      Result:=JSONEncodeStr(VarToWideStr(v));
    varBoolean:
      if v then Result:='true' else Result:='false';
    varDispatch,varUnknown:
     begin
      uu:=IUnknown(v);
      if uu=nil then Result:='null'
      else
      if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
       begin
        //revert to ToString
        Result:=d.ToString;
        d:=nil;
       end
      else
      if uu.QueryInterface(IID_IJSONArray,da)=S_OK then
       begin
        //TODO: re-do indenting
        Result:=da.ToString;
        da:=nil;
       end
      else
      //IRegExp2? IStream? IPersistStream?
        raise EJSONEncodeException.Create(
          'No supported interface found on object');
     end;
    else raise EJSONEncodeException.Create(
      'Unsupported variant type '+IntToHex(TVarData(v).VType,4));
  end;
end;

function TJSONDocument.JSONToString: WideString;
const
  stackGrowStep=$20;
var
  e:IJSONEnumerator;
  IsArray,firstItem:boolean;
  stack:array of record
    e:IJSONEnumerator;
    IsArray:boolean;
  end;
  stackLength,stackIndex:integer;
  function ExTrace:string;
  var
    i:integer;
  begin
    if IsArray then
      Result:=' #'+e.Key
    else
      Result:=' "'+e.Key+'"';
    i:=stackIndex;
    while i<>0 do
     begin
      dec(i);
      if stack[i].IsArray then
        Result:=' #'+stack[i].e.Key+Result
      else
        Result:=' "'+stack[i].e.Key+'"'+Result;
     end;
  end;
const
  resultGrowStep=$4000;
var
  wi,wl:cardinal;
{$IFDEF JSONDOC_STOREINDENTING}
const
  tabs=#13#10#9#9#9#9#9#9#9#9#9#9#9#9#9#9;
var
  tabIndex:integer;
  procedure wr(const xx,yy,zz:WideString);
  var
    xi,xj,xk,xl,yi,yl,zl:cardinal;
  begin
    xi:=1;
    xl:=Length(xx);
    yl:=Length(yy);//assert <>0
    zl:=Length(zz);
    while xi<=xl do
     begin
      xj:=xi;
      yi:=0;
      while (xi<=xl) and (yi<yl) do
       begin
        if (xx[xi]=yy[1]) and (xi+yl<=xl) then
         begin
          while (yi<yl) and (xx[xi+yi]=yy[1+yi]) do inc(yi);
          if yi<yl then inc(xi);
         end
        else
          inc(xi);
       end;
      xk:=xi-xj;
      while wi+xk>wl do
       begin
        //grow
        inc(wl,resultGrowStep);
        SetLength(Result,wl);
       end;
      Move(xx[xj],Result[wi+1],xk*2);
      inc(wi,xk);
      if xi<=xl then
       begin
        inc(xi,yl);
        while wi+zl>wl do
         begin
          //grow
          inc(wl,resultGrowStep);
          SetLength(Result,wl);
         end;
        Move(zz[1],Result[wi+1],zl*2);
        inc(wi,zl);
       end;
     end;
  end;
{$ENDIF}
  procedure w(const xx:WideString);
  var
    xl:cardinal;
  begin
    xl:=Length(xx);
    while wi+xl>wl do
     begin
      //grow
      inc(wl,resultGrowStep);
      SetLength(Result,wl);
     end;
    Move(xx[1],Result[wi+1],xl*2);
    inc(wi,xl);
  end;
  procedure Push(const NewEnum:IJSONEnumerator;NewIsArray:boolean);
  begin
    if stackIndex=stackLength then
     begin
      inc(stackLength,stackGrowStep);
      SetLength(stack,stackLength);
     end;
    stack[stackIndex].e:=e;
    stack[stackIndex].IsArray:=IsArray;
    inc(stackIndex);
    e:=NewEnum;
    IsArray:=NewIsArray;
    if IsArray then w('[') else w('{');
    firstItem:=true;
    {$IFDEF JSONDOC_STOREINDENTING}
    inc(tabIndex);
    {$ENDIF}
  end;
var
  ods:char;
  vt:TVarType;
  uu:IUnknown;
  d:IJSONDocument;
  de:IJSONEnumerable;
  da1:IJSONArray;
  da:IJSONDocArray;
begin
  if Self=nil then
   begin
    Result:='null';
    Exit;
   end;
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    wi:=1;
    wl:=resultGrowStep;
    SetLength(Result,wl);
    Result[1]:='{';

    stackLength:=0;
    stackIndex:=0;
    e:=TJSONEnumerator.Create(Self);
    IsArray:=false;

    {$if CompilerVersion >= 24}
    ods:= FormatSettings.DecimalSeparator;
    {$else}
    ods:=DecimalSeparator;
    {$ifend}
    try
      {$if CompilerVersion >= 24}
      FormatSettings.DecimalSeparator:='.';
      {$else}
      DecimalSeparator:='.';
      {$ifend}

      //w('{');//see above
      firstItem:=true;
      {$IFDEF JSONDOC_STOREINDENTING}
      tabIndex:=3;
      {$ENDIF}
      while e<>nil do
        if e.Next then
         begin
          if firstItem then firstItem:=false else w(',');
          {$IFDEF JSONDOC_STOREINDENTING}
          w(Copy(tabs,1,tabIndex));
          {$ENDIF}
          if not IsArray then
           begin
            w(JSONEncodeStr(e.Key));
            {$IFDEF JSONDOC_STOREINDENTING}
            w(': ');
            {$ELSE}
            w(':');
            {$ENDIF}
           end;
          //write value
          vt:=TVarData(PVariant(e.v0)^).VType;
          //if (vt and varByRef)<>0 then
          //  raise EJSONEncodeException.Create('VarByRef: not implemented'+ExTrace);
          if (vt and varArray)=0 then
           begin
            //not an array, plain value
            //TODO: deduplicate with JSONVarToStr1(PVariant(e.v0)^);
            case vt and varTypeMask of
              varNull:w('null');
              varSmallint,varInteger,varShortInt,
              varByte,varWord,varLongWord,varInt64:
                w(VarToWideStr(PVariant(e.v0)^));
              varSingle,varDouble,varCurrency:
                w(FloatToStr(PVariant(e.v0)^));//?
              varDate:
               begin
                //w(FloatToStr(VarToDateTime(v)));//?
                w('"');
                //TODO:"yyyy-mm-dd hh:nn:ss.zzz"? $date?
                w(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz',VarToDateTime(PVariant(e.v0)^)));
                w('"');
               end;
              varOleStr,varString,$0102:
                w(JSONEncodeStr(VarToWideStr(PVariant(e.v0)^)));
              varBoolean:
                if PVariant(e.v0)^ then w('true') else w('false');
              varDispatch,varUnknown:
               begin
                uu:=IUnknown(PVariant(e.v0)^);
                if uu=nil then w('null')
                else
                if uu.QueryInterface(IID_IJSONEnumerable,de)=S_OK then
                 begin
                  Push(de.NewEnumerator,false);
                  de:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
                 begin
                  //revert to ToString
                  w(d.ToString);
                  d:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONDocArray,da)=S_OK then
                 begin
                  {$IFDEF JSONDOC_STOREINDENTING}
                  wr(da.ToString,#13#10,Copy(tabs,1,tabIndex));
                  {$ELSE}
                  w(da.ToString);
                  {$ENDIF}
                  da:=nil;
                 end
                else
                if uu.QueryInterface(IID_IJSONArray,da1)=S_OK then
                 begin
                  Push(TJSONArrayEnumerator.Create(da1),true);
                  da1:=nil;
                 end
                else
                //IRegExp2? IStream? IPersistStream?
                  raise EJSONEncodeException.Create(
                    'No supported interface found on object'+ExTrace);
               end;
              else raise EJSONEncodeException.Create(
                'Unsupported variant type '+IntToHex(vt,4)+ExTrace);
            end;
           end
          else
           begin
            //TODO: if (vt and varTypeMask)=varByte then BLOB?
            Push(TVarArrayEnumerator.Create(e.v0),true);
           end;
         end
        else
         begin
          {$IFDEF JSONDOC_STOREINDENTING}
          dec(tabIndex);
          if not firstItem then w(Copy(tabs,1,tabIndex));
          {$ENDIF}
          if IsArray then w(']') else w('}');
          firstItem:=false;
          if stackIndex=0 then
            e:=nil
          else
           begin
            //pop from stack
            dec(stackIndex);
            e:=stack[stackIndex].e;
            IsArray:=stack[stackIndex].IsArray;
            stack[stackIndex].e:=nil;
           end;
         end;

      SetLength(Result,wi);

    finally
      {$if CompilerVersion >= 24}
      FormatSettings.DecimalSeparator:=ods;
      {$else}
      DecimalSeparator:=ods;
      {$ifend}
    end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.ToVarArray: Variant;
var
  i,l:integer;
begin
  if Self=nil then
   begin
    Result:=Null;
    Exit;
   end;
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    l:=0;
    for i:=0 to FElementIndex-1 do
      if FElements[i].LoadIndex=FLoadIndex then inc(l);
        //and not(VarIsNull(FElements[i].Value))?
    Result:=VarArrayCreate([0,l-1,0,1],varVariant);
    l:=0;
    for i:=0 to FElementIndex-1 do
      if FElements[i].LoadIndex=FLoadIndex then
       begin
        Result[l,0]:=FElements[i].Key;
        Result[l,1]:=FElements[i].Value;
        inc(l);
       end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.Clear;
var
  i:integer;
  uu:IUnknown;
  d:IJSONDocument;
  da:IJSONDocArray;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ELSE}
    FGotMatch:=false;
  {$ENDIF}
    //FDirty:=false;
    for i:=0 to FElementIndex-1 do
      if TVarData(FElements[i].Value).VType=varUnknown then
       begin
        uu:=IUnknown(FElements[i].Value);
        if uu=nil then
          VarClear(FElements[i].Value)
        else
        if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
         begin
          d.Clear;
          d:=nil;
         end
        else
        if uu.QueryInterface(IID_IJSONDocArray,da)=S_OK then
         begin
          da.Clear;
          da:=nil;
         end
        else
          VarClear(FElements[i].Value);
       end
      else
        VarClear(FElements[i].Value);
    inc(FLoadIndex);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocument.Delete(const Key: WideString);
var
  GotIndex,GotSorted:integer;
  uu:IUnknown;
  d:IJSONDocument;
  da:IJSONDocArray;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if GetKeyIndex(Key,GotIndex,GotSorted) then
     begin
      if TVarData(FElements[GotIndex].Value).VType=varUnknown then
       begin
        uu:=IUnknown(FElements[GotIndex].Value);
        if uu=nil then
          VarClear(FElements[GotIndex].Value)
        else
        if uu.QueryInterface(IID_IJSONDocument,d)=S_OK then
         begin
          d.Clear;
          d:=nil;
         end
        else
        if uu.QueryInterface(IID_IJSONDocArray,da)=S_OK then
         begin
          da.Clear;
          da:=nil;
         end
        else
          VarClear(FElements[GotIndex].Value);
       end
      else
        VarClear(FElements[GotIndex].Value);
      FElements[GotIndex].LoadIndex:=FLoadIndex-1;
     end;
    //else raise?
    //FDirty:=true;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocument.NewEnumerator: IJSONEnumerator;
begin
  Result:=TJSONEnumerator.Create(Self);
end;

{ TJSONEnumerator }

constructor TJSONEnumerator.Create(Data: TJSONDocument);
begin
  inherited Create;
  FData:=Data;
  FIndex:=-1;
  //TODO: hook into TJSONDocument destructor?
end;

destructor TJSONEnumerator.Destroy;
begin
  FData:=nil;
  inherited;
end;

function TJSONEnumerator.EOF: boolean;
var
  i:integer;
begin
  if FData=nil then
    Result:=true
  else
   begin
    {$IFDEF JSONDOC_THREADSAFE}
    EnterCriticalSection(FData.FLock);
    try
    {$ENDIF}
      i:=FIndex;
      if i=-1 then i:=0;
      while (i<FData.FElementIndex) and
        (FData.FElements[i].LoadIndex<>FData.FLoadIndex) do
        inc(i);
      Result:=i>=FData.FElementIndex;
    {$IFDEF JSONDOC_THREADSAFE}
    finally
      LeaveCriticalSection(FData.FLock);
    end;
    {$ENDIF}
   end;
end;

function TJSONEnumerator.Next: boolean;
begin
  if FData=nil then
    Result:=false
  else
   begin
    {$IFDEF JSONDOC_THREADSAFE}
    EnterCriticalSection(FData.FLock);
    try
    {$ENDIF}
      inc(FIndex);
      while (FIndex<FData.FElementIndex) and
        (FData.FElements[FIndex].LoadIndex<>FData.FLoadIndex) do
        inc(FIndex);
      Result:=FIndex<FData.FElementIndex;
    {$IFDEF JSONDOC_THREADSAFE}
    finally
      LeaveCriticalSection(FData.FLock);
    end;
    {$ENDIF}
   end;
end;

function TJSONEnumerator.Get_Key: WideString;
begin
  if (FIndex<0) or (FData=nil) or (FIndex>=FData.FElementIndex) then
    raise ERangeError.Create('Out of range')
  else
    Result:=FData.FElements[FIndex].Key;
end;

function TJSONEnumerator.Get_Value: Variant;
begin
  if (FIndex<0) or (FData=nil) or (FIndex>=FData.FElementIndex) then
    raise ERangeError.Create('Out of range')
  else
    Result:=FData.FElements[FIndex].Value;
end;

procedure TJSONEnumerator.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FData.FLock);
  try
  {$ENDIF}
    if (FIndex<0) or (FData=nil) or (FIndex>=FData.FElementIndex) then
      raise ERangeError.Create('Out of range')
    else
      FData.FElements[FIndex].Value:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FData.FLock);
  end;
  {$ENDIF}
end;

function TJSONEnumerator.v0: pointer;
begin
  if (FIndex<0) or (FData=nil) or (FIndex>=FData.FElementIndex) then
    raise ERangeError.Create('Out of range')
  else
    Result:=@FData.FElements[FIndex].Value;
end;

{ TVarArrayEnumerator }

constructor TVarArrayEnumerator.Create(const Data: PVariant);
begin
  inherited Create;
  if VarArrayDimCount(Data^)<>1 then
    raise EJSONException.Create('VarArray: multi-dimensional arrays not supported');
  FData:=Data;
  VarClear(FCurrent);
  FIndex:=VarArrayLowBound(Data^,1);
  FMax:=VarArrayHighBound(Data^,1)+1;
  FCurrentIndex:=FIndex-1;
  if FIndex<FMax then dec(FIndex);//see Next
end;

destructor TVarArrayEnumerator.Destroy;
begin
  VarClear(FCurrent);
  FData:=nil;
  inherited;
end;

function TVarArrayEnumerator.EOF: boolean;
begin
  Result:=not(FIndex<FMax);
end;

function TVarArrayEnumerator.Get_Key: WideString;
begin
  Result:=IntToStr(FIndex);
end;

function TVarArrayEnumerator.Get_Value: Variant;
begin
  Result:=FData^[FIndex];
end;

function TVarArrayEnumerator.Next: boolean;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    inc(FIndex);
    Result:=FIndex<FMax;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TVarArrayEnumerator.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    FData^[FIndex]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TVarArrayEnumerator.v0: pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FCurrentIndex<>FIndex then
     begin
      FCurrent:=FData^[FIndex];//TODO: keep SafeArray locked for lifetime of TVarArrayEnumerator instance?
      FCurrentIndex:=FIndex;
     end;
    Result:=@FCurrent;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ TVarJSONArray }

constructor TVarJSONArray.Create(const Data: Variant);
begin
  inherited Create;
  if (TVarData(Data).VType and varArray)=0 then
    raise EJSONException.Create('TVarJSONArray: array variant expected');
  if VarArrayDimCount(Data)<>1 then
    raise EJSONException.Create('TVarJSONArray: multi-dimensional arrays not supported');
  FData:=Data;
  v1:=VarArrayLowBound(FData,1);
  v2:=VarArrayHighBound(FData,1)+1;
  VarClear(FCurrent);
  FCurrentIndex:=-1;
end;

constructor TVarJSONArray.CreateNoCopy(var Data: Variant);
begin
  inherited Create;
  if (TVarData(Data).VType and varArray)=0 then
    raise EJSONException.Create('TVarJSONArray: array variant expected');
  if VarArrayDimCount(Data)<>1 then
    raise EJSONException.Create('TVarJSONArray: multi-dimensional arrays not supported');
  VarMove(FData,Data);
  v1:=VarArrayLowBound(FData,1);
  v2:=VarArrayHighBound(FData,1)+1;
  VarClear(FCurrent);
  FCurrentIndex:=-1;
end;

destructor TVarJSONArray.Destroy;
begin
  VarClear(FCurrent);
  VarClear(FData);
  inherited;
end;

function TVarJSONArray.Count: integer;
begin
  Result:=v2-v1;
end;

function TVarJSONArray.Get_Item(Index: integer): Variant;
begin
  if (Index<0) or (Index>=v2-v1) then
    raise ERangeError.Create('Out of range');
  Result:=FData[Index+v1];
end;

procedure TVarJSONArray.Set_Item(Index: integer; const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=v2-v1) then
      raise ERangeError.Create('Out of range');
    FData[Index+v1]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TVarJSONArray.v0(Index: integer): pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  Result:=nil;//counter warning
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FCurrentIndex<>Index then
     begin
      if (Index<0) or (Index>=v2-v1) then
        raise ERangeError.Create('Out of range');
      FCurrent:=FData[Index+v1];
      FCurrentIndex:=Index;
     end;
    Result:=@FCurrent;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TVarJSONArray.JSONToString: WideString;
var
  i:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //TODO: indenting?
    Result:='';
    i:=v1;
    while (i<v2) do
     begin
      Result:=Result+','+JSONVarToStr1(FData[i]);
      inc(i);
     end;
    Result[1]:='[';
    Result:=Result+']';
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ TJSONArray }

constructor TJSONArray.Create(Size: integer);
begin
  inherited Create;
  SetLength(FData,Size);
end;

function TJSONArray.Count: integer;
begin
  Result:=Length(FData);
end;

function TJSONArray.Get_Item(Index: integer): Variant;
begin
  if (Index<0) or (Index>=Length(FData)) then
    raise ERangeError.Create('Out of range');
  Result:=FData[Index];
end;

procedure TJSONArray.Set_Item(Index: integer; const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=Length(FData)) then
      raise ERangeError.Create('Out of range');
    FData[Index]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONArray.v0(Index: integer): pointer;
begin
  if (Index<0) or (Index>=Length(FData)) then
    raise ERangeError.Create('Out of range');
  Result:=@FData[Index];
end;

function TJSONArray.JSONToString: WideString;
var
  i:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    //TODO: indenting?
    Result:='';
    i:=0;
    while (i<Length(FData)) do
     begin
      Result:=Result+','+JSONVarToStr1(FData[i]);
      inc(i);
     end;
    Result[1]:='[';
    Result:=Result+']';
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONArray.NewEnumerator: IJSONEnumerator;
begin
  Result:=TJSONArrayEnumerator.Create(Self);
end;

{ TJSONArrayEnumerator }

constructor TJSONArrayEnumerator.Create(const Data: IJSONArray);
begin
  inherited Create;
  FData:=Data;
  FMax:=FData.Count;
  if FMax=0 then FIndex:=0 else FIndex:=-1;//see Next;
end;

destructor TJSONArrayEnumerator.Destroy;
begin
  FData:=nil;
  inherited;
end;

function TJSONArrayEnumerator.EOF: boolean;
begin
  Result:=not(FIndex<FMax);
end;

function TJSONArrayEnumerator.Get_Key: WideString;
begin
  Result:=IntToStr(FIndex);
end;

function TJSONArrayEnumerator.Get_Value: Variant;
begin
  Result:=FData[FIndex];
end;

function TJSONArrayEnumerator.Next: boolean;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    inc(FIndex);
    Result:=FIndex<FMax;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONArrayEnumerator.Set_Value(const Value: Variant);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    FData[FIndex]:=Value;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONArrayEnumerator.v0: pointer;
begin
  Result:=FData.v0(FIndex);
end;

{ TJSONDocArray }

constructor TJSONDocArray.Create;
begin
  inherited Create;
  FItemsCount:=0;
  FItemsSize:=0;
end;

destructor TJSONDocArray.Destroy;
begin
  SetLength(FItems,0);
  inherited;
end;

function TJSONDocArray.Get_Item(Index: integer): Variant;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=FItemsCount) then
      raise ERangeError.Create('Index out of range');
    //parse from string here assuming this won't be needed much
    if FItems[Index]='null' then
      Result:=Null
    else
      Result:=JSON(FItems[Index]);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.v0(Index: integer): pointer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  Result:=nil;//counter warning
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FCurrentIndex<>Index then
     begin
      if (Index<0) or (Index>=FItemsCount) then
        raise ERangeError.Create('Index out of range');
      //parse from string here assuming this won't be needed much
      if FItems[Index]='null' then
        FCurrent:=Null
      else
        FCurrent:=JSON(FItems[Index]);
      FCurrentIndex:=Index;
     end;
    Result:=@FCurrent;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocArray.Set_Item(Index: integer; const Value: Variant);
var
  d:IJSONDocument;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=FItemsCount) then
      raise ERangeError.Create('Index out of range');
    case TVarData(Value).VType of
      varNull:
        FItems[Index]:='null';
      varUnknown:
        if (TVarData(Value).VUnknown<>nil) and
          (IUnknown(Value).QueryInterface(IID_IJSONDocument,d)=S_OK) then
          FItems[Index]:=d.ToString
        else raise EJSONEncodeException.Create(
          'JSONDocArray.Set_Item requires IJSONDocument instances');
      else raise EJSONEncodeException.Create(
        'JSONDocArray.Set_Item requires IJSONDocument instances');
    end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.Count: integer;
begin
  Result:=FItemsCount;
end;

procedure TJSONDocArray.Clear;
var
  i:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    for i:=0 to FItemsCount-1 do FItems[i]:='';
    FItemsCount:=0;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.Add(const Doc: IJSONDocument): integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FItemsCount=FItemsSize then
     begin
      inc(FItemsSize,$400);//grow
      SetLength(FItems,FItemsSize);
     end;
    //ToString here to save on persisting effort later
    if Doc=nil then
      FItems[FItemsCount]:='null'
    else
      FItems[FItemsCount]:=Doc.ToString;
    Result:=FItemsCount;
    inc(FItemsCount);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.AddJSON(const Data: WideString): integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FItemsCount=FItemsSize then
     begin
      inc(FItemsSize,$400);//grow
      SetLength(FItems,FItemsSize);
     end;
    //TODO: check valid JSON?
    FItems[FItemsCount]:=Data;
    Result:=FItemsCount;
    inc(FItemsCount);
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

procedure TJSONDocArray.LoadItem(Index: integer; const Doc: IJSONDocument);
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=FItemsCount) then
      raise ERangeError.Create('Index out of range');
    Doc.Clear;
    if FItems[Index]<>'null' then Doc.Parse(FItems[Index]);
    //else?
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.GetJSON(Index: integer): WideString; stdcall;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if (Index<0) or (Index>=FItemsCount) then
      raise ERangeError.Create('Index out of range');
    Result:=FItems[Index];
    //else?
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

function TJSONDocArray.JSONToString: WideString;
var
  i,x,l:integer;
begin
  {$IFDEF JSONDOC_THREADSAFE}
  EnterCriticalSection(FLock);
  try
  {$ENDIF}
    if FItemsCount=0 then
      Result:='[]'
    else
     begin
      l:=FItemsCount+1;
      for i:=0 to FItemsCount-1 do
        inc(l,Length(FItems[i]));
      SetLength(Result,l);
      i:=0;
      x:=1;
      while i<FItemsCount do
       begin
        Result[x]:=',';
        inc(x);
        l:=Length(FItems[i]);
        Move(FItems[i][1],Result[x],l*2);
        inc(x,l);
        inc(i);
       end;
      Result[1]:='[';
      Result[x]:=']';
     end;
  {$IFDEF JSONDOC_THREADSAFE}
  finally
    LeaveCriticalSection(FLock);
  end;
  {$ENDIF}
end;

{ JSON }

function JSON:IJSONDocument; //overload;
begin
  Result:=TJSONDocument.Create as IJSONDocument;
end;

function JSON(const x:array of Variant):IJSONDocument; //overload;
var
  i,l,si,sl:integer;
  s:array of TJSONDocument;
  d:TJSONDocument;
  key:WideString;
begin
  d:=TJSONDocument.Create;
  si:=0;
  sl:=0;
  i:=0;
  l:=Length(x);
  while i<l do
   begin
    key:=VarToWideStr(x[i]);
    inc(i);
    if (key<>'') and (key[1]='}') then
     begin
      while (key<>'') and (key[1]='}') do
       begin
        //pop from stack
        if si=0 then
          raise EJSONException.Create('JSON builder: closing more embedded documents than opened #'+IntToStr(i))
        else
         begin
          dec(si);
          d:=s[si];
          s[si]:=nil;
         end;
        key:=Copy(key,2,Length(key)-1);
       end;
      if key<>'' then
        raise EJSONException.Create('JSON builder: "}" not allowed as key prefix #'+IntToStr(i));
     end
    else
      if (key<>'') and (key[Length(key)]='{') then
       begin
        //push on stack
        if si=sl then
         begin
          inc(sl,8);//growstep
          SetLength(s,sl);
         end;
        s[si]:=d;
        d:=TJSONDocument.Create;
        s[si][Copy(key,1,Length(key)-1)]:=d as IJSONDocument;
        inc(si);
       end
      else
        if i=l then
          raise EJSONException.Create('JSON builder: last key is missing value')
        else
         begin
          d[key]:=x[i];
          inc(i);
         end;
   end;
  //any left open?
  if si=0 then Result:=d else Result:=s[si-1];
end;

function JSON(const x: Variant): IJSONDocument; overload;
begin
  case TVarData(x).VType of
    varNull,varEmpty:Result:=nil;//raise?
    varOleStr,varString,$0102:
     begin
      Result:=TJSONDocument.Create as IJSONDocument;
      Result.Parse(VarToWideStr(x));
     end;
    else
      Result:=IUnknown(x) as IJSONDocument;
  end;
end;

function JSONEnum(const x: IJSONDocument): IJSONEnumerator;
var
  je:IJSONEnumerable;
begin
  if x=nil then
    Result:=TJSONEnumerator.Create(nil)
  else
    //Result:=(x as IJSONEnumerable).NewEnumerator;
    if x.QueryInterface(IID_IJSONEnumerable,je)=S_OK then
      Result:=je.NewEnumerator
    else
      raise EJSONException.Create('IJSONDocument instance doesn''t implement IJSONEnumerable');
end;

function JSONEnum(const x: Variant): IJSONEnumerator;
var
  vt:TVarType;
  e:IJSONEnumerable;
begin
  vt:=TVarData(x).VType;
  if (vt and varArray)=0 then
    case vt of
      varNull,varEmpty:
        Result:=TJSONEnumerator.Create(nil);//has .EOF=true
      varUnknown:
        if (TVarData(x).VUnknown<>nil) and
          (IUnknown(x).QueryInterface(IID_IJSONEnumerable,e)=S_OK) then
          Result:=e.NewEnumerator
        else
          raise EJSONException.Create('No supported interface found on object');
      else
        raise EJSONException.Create('Unsupported variant type '+IntToHex(vt,4));
    end
  else
    Result:=TVarArrayEnumerator.Create(@x);
end;

function JSON(const x: IJSONEnumerator): IJSONDocument;
begin
  Result:=IUnknown(x.Value) as IJSONDocument;
end;

function JSONEnum(const x: IJSONEnumerator): IJSONEnumerator;
begin
  if (x=nil) or VarIsNull(x.Value) then
    Result:=TJSONEnumerator.Create(nil)
  else
    Result:=(IUnknown(x.Value) as IJSONEnumerable).NewEnumerator;
end;

function ja(const Items:array of Variant): IJSONArray;
var
  a:TJSONArray;
  i,l:integer;
begin
  l:=Length(Items);
  a:=TJSONArray.Create(l);
  i:=0;
  while i<>l do
   begin
    a.FData[i]:=Items[i];
    inc(i);
   end;
  Result:=a;
end;

function ja(const Item:Variant): IJSONArray;
begin
  if (TVarData(Item).VType=varUnknown) and
    (TVarData(Item).VUnknown<>nil) and
    (IUnknown(Item).QueryInterface(IID_IJSONArray,Result)=S_OK) then
    //ok!
  else
  if (TVarData(Item).VType and varArray)<>0 then
    Result:=TVarJSONArray.Create(Item)
  else
    raise EJSONException.Create('Variant is not IJSONArray');
end;

function JSONDocArray: IJSONDocArray;
begin
  Result:=TJSONDocArray.Create;
end;

function JSONDocArray(const Items:array of IJSONDocument): IJSONDocArray;
var
  i:integer;
begin
  Result:=TJSONDocArray.Create;
  for i:=0 to Length(Items)-1 do Result.Add(Items[i]);
end;

initialization
  {$IFDEF JSONDOC_DEFAULT_USE_IJSONARRAY}
  JSON_UseIJSONArray:=true;  //default, see TJSONDocument.Create
  {$ELSE}
  JSON_UseIJSONArray:=false; //default, see TJSONDocument.Create
  {$ENDIF}
end.

