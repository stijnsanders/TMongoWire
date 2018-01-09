unit simpleSock;

interface

uses SysUtils, Classes;

{$D-}
{$L-}

type
  PSocketAddress=^TSocketAddress;
  TSocketAddress=record
    family: word;
    port: word;
    data: array[0..11] of word;
  end;

  THostEntry=record
    h_name:PAnsiChar;
    h_aliases:^PAnsiChar;
    h_addrtype:word;
    h_length:word;
    h_addr:^PAnsiChar;
    //TODO: IPv6
  end;
  PHostEntry = ^THostEntry;

  TFDSet = record
    fd_count: cardinal;
    fd_array: array[0..63] of THandle;
  end;
  PFDSet = ^TFDSet;

  TTimeVal = record
    tv_sec: cardinal;
    tv_usec: cardinal;
  end;
  PTimeVal = ^TTimeVal;

const
  INVALID_SOCKET = THandle(not(0));
  AF_INET = 2;
  AF_INET6 = 23;
  SOCKET_ERROR = -1;
  SOCK_STREAM = 1;
  IPPROTO_IP = 0;
  SOMAXCONN = 5;
  SOL_SOCKET = $FFFF;
  SO_SNDTIMEO = $1005;
  SO_RCVTIMEO = $1006;
  SD_BOTH = 2;
  IPPROTO_TCP = 6;
  TCP_NODELAY = 1;

type
  TTcpSocket=class(TObject)
  private
    FSocket:THandle;
    FAddr:TSocketAddress;
    FConnected:boolean;
  protected
    constructor Create(family: word; ASocket:THandle); overload;
    function GetPort:word;
    function GetAddress:string;
    function GetHostName:string;
  public
    constructor Create(family: word= AF_INET); overload;
    destructor Destroy; override;
    procedure Connect(const Address:AnsiString;Port:word); virtual;
    procedure Disconnect; virtual;
    function ReceiveBuf(var Buf; Count: Integer): Integer; virtual;
    function SendBuf(const Buf; Count: LongInt): LongInt; virtual;
    property Handle:THandle read FSocket;
    property Connected:boolean read FConnected;
    property Port:word read GetPort;
    property Address:string read GetAddress;
    property HostName:string read GetHostName;
  end;

  TTcpServer=class(TObject)
  private
    FFamily: word;
    FSocket: THandle;
  public
    constructor Create(family: word= AF_INET);
    destructor Destroy; override;
    procedure Bind(const Address:AnsiString;Port:word);
    procedure Listen;
    procedure WaitForConnection;
    function Accept:TTcpSocket;
    property Handle:THandle read FSocket;
  end;

  ETcpSocketError=class(Exception);

function WSAStartup(wVersionRequired: word; WSData: pointer): integer; stdcall;
function WSACleanup: integer; stdcall;
function WSAGetLastError: integer; stdcall;
function htons(hostshort: word): word; stdcall;
function inet_addr(cp: PAnsiChar): cardinal; stdcall;
function inet_ntoa(inaddr: cardinal): PAnsiChar; stdcall;
function gethostbyaddr(addr: pointer; len, Struct: integer): PHostEntry; stdcall;
function gethostbyname(name: PAnsiChar): PHostEntry; stdcall;
//TODO: getaddrinfo
function socket(af, Struct, protocol: integer): THandle; stdcall;
function setsockopt(s: THandle; level, optname: integer; optval: PAnsiChar;
  optlen: integer): integer; stdcall;
function listen(socket: THandle; backlog: integer): integer; stdcall;
function bind(s: THandle; var addr: TSocketAddress; namelen: integer): integer; stdcall;
function accept(s: THandle; addr: PSocketAddress; addrlen: PInteger): THandle; stdcall;
function connect(s: THandle; var name: TSocketAddress; namelen: integer): integer; stdcall;
function recv(s: THandle; var Buf; len, flags: integer): integer; stdcall;
function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): integer; stdcall;
function send(s: THandle; var Buf; len, flags: integer): integer; stdcall;
function shutdown(s: THandle; how: integer): integer; stdcall;
function closesocket(s: THandle): integer; stdcall;
//function __WSAFDIsSet(s: THandle; var FDSet: TFDSet): Boolean; stdcall;

type
  TCredHandle=record
    dwLower:pointer;
    dwUpper:pointer;
  end;
  PCredHandle=^TCredHandle;

  TCtxtHandle=type TCredHandle;
  PCtxtHandle=^TCtxtHandle;

  TSChannelCred=record
    dwVersion: cardinal;
    cCreds: cardinal;
    paCred: pointer;//PCertContext;
    hRootStore: THandle;
    cMappers: cardinal;
    aphMappers: pointer;
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: PCardinal;
    grbitEnabledProtocols: cardinal;
    dwMinimumCipherStrength: cardinal;
    dwMaximumCipherStrength: cardinal;
    dwSessionLifespan: cardinal;
    dwFlags: cardinal;
    dwCredFormat: cardinal;
  end;
  PSChannelCred=^TSChannelCred;

  TSecBuffer=record
    cbBuffer: cardinal;
    BufferType: cardinal;
    pvBuffer: pointer;
  end;
  PSecBuffer=^TSecBuffer;

  TSecBufferDesc=record
    ulVersion: cardinal;
    cBuffers: cardinal;
    pBuffers: PSecBuffer;
  end;
  PSecBufferDesc=^TSecBufferDesc;

  TTimeStamp=record
    dwLowDateTime: cardinal;
    dwHighDateTime: cardinal;
  end;
  PTimeStamp=^TTimeStamp;

  TSecPkgContextStreamSizes=record
    cbHeader: cardinal;
    cbTrailer: cardinal;
    cbMaximumMessage: cardinal;
    cBuffers: cardinal;
    cbBlockSize: cardinal;
  end;
  PSecPkgContextStreamSizes=^TSecPkgContextStreamSizes;

  
function AcquireCredentialsHandle(pszPrincipal: PAnsiChar;
    pszPackage: PAnsiChar; fCredentialUse: cardinal; pvLogonID: PInt64;
    pAuthData: PSChannelCred; pGetKeyFn: pointer; pvGetKeyArgument: pointer;
    phCredential: PCredHandle; ptsExpiry: PTimeStamp): cardinal; stdcall;

function FreeCredentialsHandle (phCredential: PCredHandle): cardinal; stdcall;

function InitializeSecurityContext(phCredential: PCredHandle;
    phContext: PCtxtHandle; pszTargetName: PAnsiChar; fContextReq: cardinal;
    Reserved1: cardinal; TargetDataRep: cardinal; pInput: PSecBufferDesc;
    Reserved2: cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc;
    pfContextAttr: PCardinal; ptsExpiry: PTimeStamp): cardinal; stdcall;

function DeleteSecurityContext(phContext: PCtxtHandle): cardinal; stdcall;

function ApplyControlToken(phContext: PCtxtHandle;
    pInput: PSecBufferDesc): cardinal; stdcall;

function QueryContextAttributes(phContext: PCtxtHandle;
    ulAttribute: cardinal; pBuffer: pointer): cardinal; stdcall;

function FreeContextBuffer(pvContextBuffer: pointer): cardinal; stdcall;

function EncryptMessage(phContext: PCtxtHandle; fQOP: cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: cardinal): cardinal; stdcall;

function DecryptMessage(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: cardinal; pfQOP: PCardinal): cardinal; stdcall;

const
  SP_PROT_TLS1          = $0C0;
  SP_PROT_TLS1_SERVER   = $040;
  SP_PROT_TLS1_CLIENT   = $080;
  SP_PROT_TLS1_1        = $300;
  SP_PROT_TLS1_1_SERVER = $100;
  SP_PROT_TLS1_1_CLIENT = $200;
  SP_PROT_TLS1_2        = $C00;
  SP_PROT_TLS1_2_SERVER = $400;
  SP_PROT_TLS1_2_CLIENT = $800;

  SECPKG_CRED_INBOUND  = 1;
  SECPKG_CRED_OUTBOUND = 2;

  ISC_REQ_DELEGATE               = $00000001;
  ISC_REQ_MUTUAL_AUTH            = $00000002;
  ISC_REQ_REPLAY_DETECT          = $00000004;
  ISC_REQ_SEQUENCE_DETECT        = $00000008;
  ISC_REQ_CONFIDENTIALITY        = $00000010;
  ISC_REQ_USE_SESSION_KEY        = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS       = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS     = $00000080;
  ISC_REQ_ALLOCATE_MEMORY        = $00000100;
  ISC_REQ_USE_DCE_STYLE          = $00000200;
  ISC_REQ_DATAGRAM               = $00000400;
  ISC_REQ_CONNECTION             = $00000800;
  ISC_REQ_CALL_LEVEL             = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED      = $00002000;
  ISC_REQ_EXTENDED_ERROR         = $00004000;
  ISC_REQ_STREAM                 = $00008000;
  ISC_REQ_INTEGRITY              = $00010000;
  ISC_REQ_IDENTIFY               = $00020000;
  ISC_REQ_NULL_SESSION           = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_RESERVED1              = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT        = $00200000;

  SECURITY_NATIVE_DREP = $10;

  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY   = 0;
  SECBUFFER_DATA    = 1;
  SECBUFFER_TOKEN   = 2;
  SECBUFFER_EXTRA   = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER  = 7;

  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED        = $00090312;
  SEC_E_INCOMPLETE_MESSAGE     = $80090318;

  SECPKG_ATTR_STREAM_SIZES = 4;

  SCHANNEL_SHUTDOWN = 1;
  
type
  TTcpSecureSocket=class(TTcpSocket)
  private
    FCred:TCredHandle;
    FCtx:TCtxtHandle;
    FSizes:TSecPkgContextStreamSizes;
    FBuffer,FScratch:array of byte;
    FBStart,FBCount:integer;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Connect(const Address:AnsiString;Port:word); override;
    procedure Disconnect; override;
    function ReceiveBuf(var Buf; Count: Integer): Integer; override;
    function SendBuf(const Buf; Count: LongInt): LongInt; override;
  end;

implementation

var
  WSAData:record // !!! also WSDATA
    wVersion:word;
    wHighVersion:word;
    szDescription:array[0..256] of AnsiChar;
    szSystemStatus:array[0..128] of AnsiChar;
    iMaxSockets:word;
    iMaxUdpDg:word;
    lpVendorInfo:PAnsiChar;
  end;

procedure RaiseLastWSAError;
begin
  raise ETcpSocketError.Create(SysErrorMessage(WSAGetLastError));
end;

procedure PrepareSockAddr(var addr: TSocketAddress; family, port: word;
  const host: AnsiString);
var
  e:PHostEntry;
  i:integer;
begin
  addr.family:=family;//AF_INET
  addr.port:=htons(port);
  for i:=0 to 11 do addr.data[i]:=0;
  if host<>'' then
    if AnsiChar(host[1]) in ['0'..'9'] then
      PCardinal(@addr.data[0])^:=inet_addr(PAnsiChar(host))
    else
     begin
      //TODO: getaddrinfo
      e:=gethostbyname(PAnsiChar(host));
      if e=nil then RaiseLastWSAError;
      addr.family:=e.h_addrtype;
      Move(e.h_addr^[0],addr.data[0],e.h_length);
     end;
end;

{ TTcpSocket }

procedure TTcpSocket.Connect(const Address: AnsiString; Port: word);
begin
  PrepareSockAddr(FAddr,FAddr.family,Port,Address);
  if simpleSock.connect(FSocket,FAddr,SizeOf(TSocketAddress))=SOCKET_ERROR then
    RaiseLastWSAError
  else
    FConnected:=true;
end;

constructor TTcpSocket.Create(family: word);
begin
  inherited Create;
  FConnected:=false;
  FillChar(FAddr,SizeOf(TSocketAddress),#0);
  FAddr.family:=family;//AF_INET
  FSocket:=socket(family,SOCK_STREAM,IPPROTO_IP);
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
end;

constructor TTcpSocket.Create(family: word; ASocket: THandle);
var
  i:integer;
begin
  inherited Create;
  FAddr.family:=family;
  FSocket:=ASocket;
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  i:=1;
  if setsockopt(FSocket,IPPROTO_TCP,TCP_NODELAY,@i,4)<>0 then
    RaiseLastWSAError;
  FConnected:=true;//?
end;

destructor TTcpSocket.Destroy;
begin
  //Disconnect;?
  closesocket(FSocket);
  inherited;
end;

procedure TTcpSocket.Disconnect;
begin
  if FConnected then
   begin
    FConnected:=false;
    shutdown(FSocket,SD_BOTH);
   end;
end;

function TTcpSocket.GetPort: word;
begin
  Result:=FAddr.port;
end;

function TTcpSocket.GetAddress: string;
begin
  Result:=string(inet_ntoa(PCardinal(@FAddr.data[0])^));
end;

function TTcpSocket.GetHostName: string;
var
  e:PHostEntry;
  i:integer;
begin
  e:=gethostbyaddr(@FAddr.data[0],SizeOf(TSocketAddress),FAddr.family);
  if e=nil then
    //inet_ntop?
    if FAddr.family=AF_INET6 then
     begin
      i:=3;
      if FAddr.data[i]=0 then Result:=':' else
        Result:=Result+IntToHex(FAddr.data[i],4)+':';
      while (i<10) do
       begin
        while (i<10) and (FAddr.data[i]=0) do inc(i);
        if i=10 then Result:=Result+':' else
          Result:=Result+':'+IntToHex(FAddr.data[i],4);
        inc(i);
       end;
     end
    else
      Result:=string(inet_ntoa(PCardinal(@FAddr.data[0])^))
  else
    Result:=string(e.h_name);
end;

function TTcpSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
begin
  Result:=recv(FSocket,Buf,Count,0);
  if Result=SOCKET_ERROR then
   begin
    Disconnect;
    RaiseLastWSAError;
   end;
end;

function TTcpSocket.SendBuf(const Buf; Count: LongInt): LongInt;
var
  p:pointer;
begin
  p:=@Buf;
  Result:=send(FSocket,p^,Count,0);
  if Result=SOCKET_ERROR then
   begin
    Disconnect;
    RaiseLastWSAError;
   end;
end;

{ TTcpServer }

constructor TTcpServer.Create(family: word);
begin
  inherited Create;
  FFamily:=family;//AF_INET
  FSocket:=socket(FFamily,SOCK_STREAM,IPPROTO_IP);
end;

destructor TTcpServer.Destroy;
begin
  closesocket(FSocket);
  inherited;
end;

procedure TTcpServer.Bind(const Address: AnsiString; Port: word);
var
  a:TSocketAddress;
begin
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  PrepareSockAddr(a,FFamily,Port,Address);
  if simpleSock.bind(FSocket,a,SizeOf(TSocketAddress))=SOCKET_ERROR then
    RaiseLastWSAError;
end;

procedure TTcpServer.Listen;
begin
  //call bind first!
  if simpleSock.listen(FSocket,SOMAXCONN)=SOCKET_ERROR then
    RaiseLastWSAError;
end;

procedure TTcpServer.WaitForConnection;
var
  r,x:TFDSet;
begin
  r.fd_count:=1;
  r.fd_array[0]:=FSocket;
  x.fd_count:=1;
  x.fd_array[0]:=FSocket;
  if select(FSocket+1,@r,nil,@x,nil)=SOCKET_ERROR then RaiseLastWSAError;
  if x.fd_count=1 then //if __WSAFDIsSet(FSocket,x) then
    raise ETcpSocketError.Create('Socket in error state');//?
  if r.fd_count=0 then //if not __WSAFDIsSet(FSocket,r) then
    raise ETcpSocketError.Create('Select without error nor result');//??
end;

function TTcpServer.Accept: TTcpSocket;
var
  a:TSocketAddress;
  l:integer;
begin
  l:=SizeOf(TSocketAddress);
  FillChar(a,l,#0);
  Result:=TTcpSocket.Create(FFamily,simpleSock.accept(FSocket,@a,@l));
  Result.FAddr:=a;
end;

const
  winsockdll='wsock32.dll';

function WSAStartup; external winsockdll;
function WSACleanup; external winsockdll;
function WSAGetLastError; external winsockdll;
function htons; external winsockdll;
function inet_addr; external winsockdll;
function inet_ntoa; external winsockdll;
function gethostbyaddr; external winsockdll;
function gethostbyname; external winsockdll;
function socket; external winsockdll;
function setsockopt; external winsockdll;
function listen; external winsockdll;
function bind; external winsockdll;
function accept; external winsockdll;
function connect; external winsockdll;
function recv; external winsockdll;
function select; external winsockdll;
function send; external winsockdll;
function shutdown; external winsockdll;
function closesocket; external winsockdll;
//function __WSAFDIsSet; external winsockdll;

const
  SecurityDLL='secur32.dll'; //'sspicli.dll'

function AcquireCredentialsHandle; external SecurityDLL name 'AcquireCredentialsHandleA';
function FreeCredentialsHandle; external SecurityDLL name 'FreeCredentialsHandle';
function InitializeSecurityContext; external SecurityDLL name 'InitializeSecurityContextA';

function DeleteSecurityContext; external SecurityDLL name 'DeleteSecurityContext';

function ApplyControlToken; external SecurityDLL name 'ApplyControlToken';

function QueryContextAttributes; external SecurityDLL name 'QueryContextAttributesA';

function FreeContextBuffer; external SecurityDLL name 'FreeContextBuffer';

function EncryptMessage; external SecurityDLL name 'EncryptMessage';

function DecryptMessage; external SecurityDLL name 'DecryptMessage';

{ TTcpSecureSocket }

procedure TTcpSecureSocket.AfterConstruction;
begin
  inherited;
  FCred.dwLower:=nil;
  FCred.dwUpper:=nil;
  FCtx.dwLower:=nil;
  FCtx.dwUpper:=nil;
  FBStart:=0;
  FBCount:=0;
end;

destructor TTcpSecureSocket.Destroy;
begin
  if FCred.dwLower<>nil then
   begin
    FreeCredentialsHandle(@FCred);
    FCred.dwLower:=nil;
    FCred.dwUpper:=nil;
   end;
  if FCtx.dwLower<>nil then
   begin
    DeleteSecurityContext(@FCtx);
    FCtx.dwLower:=nil;
    FCtx.dwUpper:=nil;
   end;
  FBStart:=0;
  FBCount:=0;
  SetLength(FBuffer,0);
  SetLength(FScratch,0);
  inherited;
end;

const
  rqFlags=
    ISC_REQ_SEQUENCE_DETECT or
    ISC_REQ_REPLAY_DETECT or
    ISC_REQ_CONFIDENTIALITY or
    ISC_REQ_EXTENDED_ERROR or
    ISC_REQ_ALLOCATE_MEMORY or
    ISC_REQ_STREAM;

procedure TTcpSecureSocket.Connect(const Address: AnsiString; Port: word);
var
  r,f:cardinal;
  i,j:integer;
  d1,d2:TSecBufferDesc;
  d:array of TSecBuffer;
  x:array of byte;
begin
{
  FillChar(a,SizeOf(TSChannelCred),#0);
  a.dwVersion:=SCHANNEL_CRED_VERSION;
  //a.hRootStore:=CertOpenSystemStore(nil,'MY');
  a.grbitEnabledProtocols:=
    SP_PROT_TLS1_CLIENT or
    SP_PROT_TLS1_1_CLIENT or
    SP_PROT_TLS1_2_CLIENT;
  a.dwFlags:=
    SCH_CRED_NO_DEFAULT_CREDS;//?
}

  if FCred.dwLower=nil then
    if AcquireCredentialsHandle(nil,'Microsoft Unified Security Protocol Provider',
      SECPKG_CRED_OUTBOUND,nil,
      //@a,
      nil,

      nil,nil,@FCred,nil)<>0 then
      RaiseLastOSError;

  inherited; //does connect
  //assert Connected

  SetLength(d,3);

  d1.ulVersion:=SECBUFFER_VERSION;
  d1.cBuffers:=2;
  d1.pBuffers:=@d[0];

  d[0].cbBuffer:=0;
  d[0].BufferType:=SECBUFFER_TOKEN;
  d[0].pvBuffer:=nil;

  d[1].cbBuffer:=0;
  d[1].BufferType:=SECBUFFER_EMPTY;
  d[1].pvBuffer:=nil;

  d2.ulVersion:=SECBUFFER_VERSION;
  d2.cBuffers:=1;
  d2.pBuffers:=@d[2];

  d[2].cbBuffer:=0;
  d[2].BufferType:=SECBUFFER_TOKEN;
  d[2].pvBuffer:=nil;

  r:=InitializeSecurityContext(@FCred,nil,PAnsiChar(Address),rqFlags,
    0,SECURITY_NATIVE_DREP,nil,0,@FCtx,@d2,@f,nil);
  if r<>SEC_I_CONTINUE_NEEDED then
    raise ETcpSocketError.Create(SysErrorMessage(r));

  j:=send(Handle,d[2].pvBuffer^,d[2].cbBuffer,0);
  if j=SOCKET_ERROR then
   begin
    Disconnect;
    RaiseLastWSAError;
   end;
  if j=0 then
    raise ETcpSocketError.Create('Unauthenticated connection closed');

  i:=0;
  SetLength(x,$10000);

  while (r=SEC_I_CONTINUE_NEEDED) or (r=SEC_E_INCOMPLETE_MESSAGE) do
   begin
    if r<>SEC_E_INCOMPLETE_MESSAGE then i:=0;

    j:=recv(Handle,x[i],$10000-i,0);
    if j=SOCKET_ERROR then
     begin
      Disconnect;
      RaiseLastWSAError;
     end;
    if j=0 then
      raise ETcpSocketError.Create('Authenticating connection closed');
    inc(i,j);

    d[0].cbBuffer:=i;
    d[0].BufferType:=SECBUFFER_TOKEN;
    d[0].pvBuffer:=@x[0];

    r:=InitializeSecurityContext(@FCred,@FCtx,nil,rqFlags,
      0,SECURITY_NATIVE_DREP,@d1,0,@FCtx,@d2,@f,nil);

    if (r=SEC_E_OK) or (r=SEC_I_CONTINUE_NEEDED)
      or ((f and ISC_REQ_EXTENDED_ERROR)<>0) then
     begin
      if (d[2].cbBuffer<>0) and (d[2].pvBuffer<>nil) then
       begin
        j:=send(Handle,d[2].pvBuffer^,d[2].cbBuffer,0);
        if j=SOCKET_ERROR then
         begin
          Disconnect;
          RaiseLastWSAError;
         end;
        if j=0 then
          raise ETcpSocketError.Create('Authentication connection closed');
        FreeContextBuffer(d[2].pvBuffer);//??
        d[2].pvBuffer:=nil;
       end;
     end;

   end;

  if r=SEC_E_OK then
   begin
    i:=QueryContextAttributes(@FCtx,SECPKG_ATTR_STREAM_SIZES,@FSizes);
    if i<>SEC_E_OK then
      raise ETcpSocketError.Create(SysErrorMessage(i));
    SetLength(FBuffer,FSizes.cbMaximumMessage);
    SetLength(FScratch,FSizes.cbHeader+FSizes.cbMaximumMessage+FSizes.cbTrailer);
    FBStart:=0;
    FBCount:=0;

    if d[1].BufferType=SECBUFFER_EXTRA then
     begin
      FBCount:=d[1].cbBuffer;
      Move(d[1].pvBuffer^,FBuffer[0],FBCount);
      //FreeContextBuffer(d[1].pvBuffer);//?
     end;

   end
  else
    raise Exception.Create(SysErrorMessage(r));

end;

procedure TTcpSecureSocket.Disconnect;
var
  d1:TSecBufferDesc;
  d:TSecBuffer;
  dt,r,f:cardinal;
begin
  if Connected then
   begin
    d1.ulVersion:=SECBUFFER_VERSION;
    d1.cBuffers:=1;
    d1.pBuffers:=@d;
    try
      d.cbBuffer:=4;
      d.BufferType:=SECBUFFER_TOKEN;
      d.pvBuffer:=@dt;
      dt:=SCHANNEL_SHUTDOWN;
      r:=ApplyControlToken(@FCtx,@d1);
      if r<>SEC_E_OK then
        raise ETcpSocketError.Create(SysErrorMessage(r));
      d.cbBuffer:=0;
      d.BufferType:=SECBUFFER_TOKEN;
      d.pvBuffer:=nil;
      r:=InitializeSecurityContext(@FCred,@FCtx,nil,rqFlags,
        0,SECURITY_NATIVE_DREP,nil,0,@FCtx,@d1,@f,nil);
      if r<>SEC_E_OK then
        raise ETcpSocketError.Create(SysErrorMessage(r));
      send(Handle,d.pvBuffer^,d.cbBuffer,0);
      FreeContextBuffer(d.pvBuffer);
    except
      on ETcpSocketError do ;//silent, disconnecting anyway
    end;
   end;
  inherited;
end;

function TTcpSecureSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
var
  d1:TSecBufferDesc;
  d:array of TSecBuffer;
  r:cardinal;
  i,j:integer;
begin
  Result:=0;//default
  if FBCount=0 then
   begin
    i:=0;
    SetLength(d,4);
    d1.ulVersion:=SECBUFFER_VERSION;
    d1.cBuffers:=4;
    d1.pBuffers:=@d[0];
    d[0].cbBuffer:=0;//see below
    d[0].BufferType:=SECBUFFER_DATA;
    d[0].pvBuffer:=@FScratch[0];
    d[1].cbBuffer:=0;
    d[1].BufferType:=SECBUFFER_EMPTY;
    d[1].pvBuffer:=nil;
    d[2].cbBuffer:=0;
    d[2].BufferType:=SECBUFFER_EMPTY;
    d[2].pvBuffer:=nil;
    d[3].cbBuffer:=0;
    d[3].BufferType:=SECBUFFER_EMPTY;
    d[3].pvBuffer:=nil;
    r:=SEC_E_INCOMPLETE_MESSAGE;
    while r=SEC_E_INCOMPLETE_MESSAGE do
     begin
      j:=inherited ReceiveBuf(FScratch[i],Length(FScratch)-i);
      if j=0 then Exit;//raise?
      inc(i,j);
      d[0].cbBuffer:=i;
      r:=DecryptMessage(@FCtx,@d1,0,nil);
     end;
    if r<>SEC_E_OK then raise ETcpSocketError.Create(SysErrorMessage(r));
    //TODO: SEC_I_RENEGOTIATE
    //assert d[0].BufferType=SECBUFFER_STREAM_HEADER
    //assert d[2].BufferType=SECBUFFER_STREAM_TRAILER
    if d[1].BufferType=SECBUFFER_DATA then
      if d[1].cbBuffer>cardinal(Count) then
       begin
        FBStart:=0;
        FBCount:=d[1].cbBuffer;
        Move(d[1].pvBuffer^,FBuffer[0],FBCount);
        if d[3].BufferType=SECBUFFER_EXTRA then
         begin
          i:=d[3].cbBuffer;
          Move(d[3].pvBuffer^,FBuffer[FBCount],i);
          //FreeContextBuffer(d[3].pvBuffer);//?
          inc(FBCount,i);
         end;
       end
      else
       begin
        Result:=d[1].cbBuffer;
        Move(d[1].pvBuffer^,Buf,d[1].cbBuffer);
        if d[3].BufferType=SECBUFFER_EXTRA then
         begin
          FBStart:=0;
          FBCount:=d[3].cbBuffer;
          Move(d[3].pvBuffer^,FBuffer[0],FBCount);
          //FreeContextBuffer(d[3].pvBuffer);//?
         end;
       end
    else
      raise ETcpSocketError.Create('Unexpected DecryptMessage result');
   end;
  if FBCount<>0 then
    if Count>FBCount then
     begin
      Result:=FBCount;
      Move(FBuffer[FBStart],Buf,Result);
      FBStart:=0;
      FBCount:=0;
     end
    else
     begin
      Result:=Count;
      Move(FBuffer[FBStart],Buf,Result);
      inc(FBStart,Result);
      dec(FBCount,Result);
     end;
end;

function TTcpSecureSocket.SendBuf(const Buf; Count: Integer): LongInt;
var
  d1:TSecBufferDesc;
  d:array of TSecBuffer;
  r,l,l1,c,c1:cardinal;
  p:pointer;
begin
  Result:=Count;
  SetLength(d,4);
  d1.ulVersion:=SECBUFFER_VERSION;
  d1.cBuffers:=4;
  d1.pBuffers:=@d[0];
  p:=@Buf;
  c:=cardinal(Count);
  while c<>0 do
   begin
    if c>FSizes.cbMaximumMessage then
      c1:=FSizes.cbMaximumMessage
    else
      c1:=c;
    Move(p^,FScratch[FSizes.cbHeader],c1);
    l1:=FSizes.cbHeader+c1;
    l:=l1+FSizes.cbTrailer;
    d[0].cbBuffer:=FSizes.cbHeader;
    d[0].BufferType:=SECBUFFER_STREAM_HEADER;
    d[0].pvBuffer:=@FScratch[0];
    d[1].cbBuffer:=c1;
    d[1].BufferType:=SECBUFFER_DATA;
    d[1].pvBuffer:=@FScratch[FSizes.cbHeader];
    d[2].cbBuffer:=FSizes.cbTrailer;
    d[2].BufferType:=SECBUFFER_STREAM_TRAILER;
    d[2].pvBuffer:=@FScratch[l1];
    d[3].cbBuffer:=0;
    d[3].BufferType:=SECBUFFER_EMPTY;
    d[3].pvBuffer:=nil;
    r:=EncryptMessage(@FCtx,0,@d1,0);
    if r<>SEC_E_OK then
      raise ETcpSocketError.Create(SysErrorMessage(r));
    l1:=inherited SendBuf(FScratch[0],l);
    if l1=0 then
     begin
      Disconnect;//raise?
      Result:=0;
      c:=0;//end loop
     end
    else
     begin
      if l1<>l then //TODO: loop until all sent?
        raise ETcpSocketError.Create('Error sending block');
      inc(cardinal(p),c1);
      dec(c,c1);
     end;
   end;
end;

initialization
  WSAStartup($0101,@WSAData);
finalization
  WSACleanup;
end.

