{--------------------------------------------------------------}

{  SynHttpSrv.pas - HTTP server over Synapse                   }

 {  Author:     Semi                                            }
 {  Started:    070528                                          }

{--------------------------------------------------------------}
unit SynHttpSrv;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
 {$IFDEF MSWINDOWS}
 Windows,
 {$ELSE}
 SynaUtil,
 {$ENDIF}
 ssl_openssl,
 SysUtils,
 Classes,
 synsock,
 blcksock,
 SynSrv;
//-------------------------------------------------------------

{$undef DEBUG}
//{$define DEBUG}

type
 // Result: True=found/stop, False=continue
 THeaderEnum = function(const Value: string; LParam: NativeUInt): boolean of object;

 THeaderList = class(TStringList)
 private
  function GetValueByName(const Name: string): string;
  procedure SetValueByName(const Name, Value: string);
  function GetNameByIndex(Index: integer): string;
  function GetValueByIndex(Index: integer): string;
  function CheckHttpFindValue(const Value: string; LParam: NativeUInt): boolean;
  function GetSubValue(const Name, SubName: string): string;
  procedure SetSubValue(const Name, SubName, Value: string);
 protected
  procedure Put(Index: integer; const S: string); override;
 public
  property Values[const Name: string]: string Read GetValueByName Write SetValueByName; default;
  //
  property Names[Index: integer]: string Read GetNameByIndex;
  property ValuesByIndex[Index: integer]: string Read GetValueByIndex;
  property SubValues[const Name, SubName: string]: string Read GetSubValue Write SetSubValue;
  // for 'ContentType: text/html; charset="Windows-1250"', SubValues['Content-Type','charset']
  //
  function IndexOfName(const Name: string): integer; reintroduce;
  procedure AddValue(const Name, Value: string); // add (possibly duplicate) value...
  function RemoveValue(const Name: string): boolean; // used also by writing Values[Name]:='';
  //
  // Enumerates duplicated or comma-separated headers:
  procedure EnumHeaders(const Name: string; const Enum: THeaderEnum; const Sep: char; LParam: NativeUInt = 0);
  function HasValue(const Name, Value: string): boolean; // Connection: upgrade, close
  function Add(const S: string): integer; override;
  procedure Insert(Index: integer; const S: string); override;
 end;

 THttpCookie = class(TCollectionItem)
 private
  FName:    string;
  FValue:   string;
  FDomain:  string;
  FPath:    string;
  FExpires: string;
  FVersion: string;
  FMaxAge:  string;
  FComment: string;
  FSecure:  boolean;
  FSameSite:boolean;
  function GetText: string;
 public
  property Name: string Read FName Write FName;
  property Value: string Read FValue Write FValue;
  property Text: string Read GetText;
  //
  property Domain: string Read FDomain Write FDomain;
  property Path: string Read FPath Write FPath;
  property Version: string Read FVersion Write FVersion;
  property MaxAge: string Read FMaxAge Write FMaxAge;
  property Comment: string Read FComment Write FComment;
  property Secure: boolean Read FSecure Write FSecure;
  property SameSite: boolean Read FSameSite Write FSameSite;
  property Expires: string Read FExpires Write FExpires; // obsolette...
  //
  procedure DeleteCookie; // set MaxAge:='0'; so that client will delete the cookie...
  //
  procedure Assign(Source: TPersistent); override;
  //
  function GetServerCookie: string; // Set-Cookie: format... (for sending server->client)
  function GetClientCookie: string; // Cookie:     format... (for sending client->server)
  function ParseValue(Line: string; Version: NativeUInt): boolean;
  // parse either Cookie: or SetCookie: header part, 1 cookie at a time...
  function MatchPath(const aPath: string): boolean; // is it cookie for this path?
 end;

 { THttpCookies }

 THttpCookies = class(TCollection)
 private
  function GetCookieItem(Index: integer): THttpCookie;
  function AddCookieValue(const Value: string; LParam: NativeUInt): boolean;
  function GetValue(const Name: string): string;
  procedure SetValue(const Name, Value: string);
  function GetCommaText: string;
 public
  constructor Create;
  //
  property Cookies[Index: integer]: THttpCookie Read GetCookieItem; default;
  function IndexOf(const Name: string): integer;
  function Find(const Name: string): THttpCookie;
  //
  // Load cookies from client, used in server... (Cookie: headers)
  procedure LoadClientCookies(Headers: THeaderList);
  // Save cookies to client, used in server...
  procedure SaveServerCookies(Headers: THeaderList; const DefaultDomain, DefaultPath: string);
  //
  // Load cookies from server, used in client... (Set-Cookie: headers)
  procedure LoadServerCookies(Headers: THeaderList);
  // Save cookies to server, used in client...
  procedure SaveClientCookies(Headers: THeaderList; const Path: string);
  //
  // Other client-side functions:
  procedure MergeCookies(Source: THttpCookies);
  procedure SetDefaultPath;
  procedure SetSameSite;

  property Values[const Name: string]: string Read GetValue Write SetValue;

  property CommaText: string Read GetCommaText;
 end;

 // HTTP request and response object

 { THttpRequest }

 THttpRequest = class(TPersistent)
 private
  FHeaders:      THeaderList;
  FCookies:      THttpCookies;
  FParams:       TStringList;
  FPostStream:   TStream;
  FUrl:          string;
  FMethod:       string;
  FProtocol:     string;
  FContent:      string;
  //FContentStream: TStream;
  FStatusCode:   integer;
  FStatusMsg:    string;
  FConnection:   TObject;
  FFlags:        integer;
  FResponseSent: boolean;
  FCharSet:      string;
  FDocument:     string;
  procedure SetHeaders(Value: THeaderList);
  procedure SetCookies(Value: THttpCookies);
  procedure SetStatusCode(Value: integer);
  function GetFlagBool(Index: integer): boolean;
  procedure SetFlagBool(Index: integer; Value: boolean);
  function GetStrProp(Index: integer): string;
  procedure SetStrProp(Index: integer; const Value: string);
  function GetDateProp(Index: integer): TDateTime;
  procedure SetDateProp(Index: integer; const Value: TDateTime);
  //
  procedure ApplyHeaders(bnIsServer: boolean); virtual;
  // parse Cookies and possibly other things from Headers... used by TSynHttpServer.ReadRequest
  function AddMultiPartFormItem(Headers: THeaderList; const FieldName, Content: string): boolean;
  procedure SetCharSet(const Value: string);
 public
  constructor Create;
  destructor Destroy; override;
  procedure Assign(Source: TPersistent); override;
  //
  property Headers: THeaderList Read FHeaders Write SetHeaders;  // Set assigns copy...
  //
  property Cookies: THttpCookies Read FCookies Write SetCookies; // Set assigns copy...
  //
  property Url: string Read FUrl;                 // '/index.html'
  property Document: string Read FDocument;
  property Method: string Read FMethod;        // 'GET'
  property Protocol: string Read FProtocol;  // 'HTTP/1.1'
  // also MUST include Headers['Host'] value...
  //
  property StatusCode: integer Read FStatusCode Write SetStatusCode; // 200
  property StatusMsg: string Read FStatusMsg Write FStatusMsg;       // 'OK'
  //
  property Content: string Read FContent Write FContent;
  //property ContentStream: TStream Read FContentStream Write FContentStream; // stream is owned by the Request...
  property SendChunked: boolean index 1 Read GetFlagBool Write SetFlagBool;
  // set to True to prevent asking Stream.Size and send in chunked mode (without Content-length)
  //
  property Connection: TObject Read FConnection Write FConnection; // TSynTcpSrvConnection usually...
  //
  // Params contain 'Name=Value' for parameters in ?params in url and for POST params inside content:
  // When posting files, Params does NOT contain file data, only FileName, use GetPostFormParam to retrieve file data...
  property Params: TStringList Read FParams; // use  Request.Params.Values[ParamName]
  property PostStream: TStream Read FPostStream Write FPostStream;
  function GetPostFormParam(const ParamName: string; var ParamData: string): boolean;
  // get 1 param from multipart/form-data or application/x-www-form-urlencoded...
  //
  // Common operations for application for making reply:
  procedure ServeFile(const LocalFileName: string);
  // open file in ContentStream, set Last-Modified, Content-Length, Content-Type
  procedure Redirect(const aUrl: string); // set 302 redirection and Location: header
  //
  // Functions used by server/client:
  procedure ParseFirstRequestLine(Line: string);  // parse:  'GET /index.html HTTP/1.1'  // used by server
  procedure ParseFirstResponseLine(Line: string); // parse:  'HTTP/1.1 200 OK'           // used by client
  function GetFirstResponseLine: string;         // format: 'HTTP/1.1 200 OK'           // used by server
  function GetFirstRequestLine: string;          // format: 'GET /index.html HTTP/1.1'  // used by client
  procedure ParsePostFormData;
  // parse Content string into Params, used usually by Server (for POST requests with propper Content-Type)
  //
  function MatchTag(Etags: string): boolean;
  // Etags may have multiple tags, comma-separated... returns True, if some of them is identical with Etag...
  //
  // Common Header properties:
  property ContentType: string index 0 Read GetStrProp Write SetStrProp;  // 'text/html; charset="Windows-1250"'
  property BaseContentType: string index 1 Read GetStrProp;               // 'text/html'
  property CharSet: string Read FCharSet Write SetCharSet;
  property ContentDisposition: string index 2 Read GetStrProp Write SetStrProp;
  // 'attachment; filename=targetfile.html'
  property TargetFileName: string index 3 Read GetStrProp Write SetStrProp;
  // name, by which this should be saved by client (in Content-Disposition)
  property Location: string index 4 Read GetStrProp Write SetStrProp;       // Location: header
  property Etag: string index 5 Read GetStrProp Write SetStrProp;
  // Etag is used for caches, so that they may know, that their copy is exactly identical with current data (having same Etag for same URL means it is exactly identical...)
  property Host: string index 6 Read GetStrProp Write SetStrProp;           // must be in Request
  property Referer: string index 7 Read GetStrProp Write SetStrProp;
  property UserAgent: string index 8 Read GetStrProp Write SetStrProp;
  property Vary: string index 9 Read GetStrProp Write SetStrProp;
  // list of headers, for which the response varies... used by caches...
  property WwwAuthenticate: string index 10 Read GetStrProp Write SetStrProp;
  // authentication challenge, used with 401 status-code... see RFC2617...
  property Authorization: string index 11 Read GetStrProp Write SetStrProp; // Authorization: value, sent by client
  property Boundary: string index 12 Read GetStrProp Write SetStrProp;
  // Content-Type: multipart/any; boundary=0123456789
  property ContentEncoding: string index 13 Read GetStrProp Write SetStrProp;
  property CacheControl: string index 14 Read GetStrProp Write SetStrProp;
  property Pragma: string index 15 Read GetStrProp Write SetStrProp;
  property ServerSoftware: string index 16 Read GetStrProp Write SetStrProp;
  property AcceptEncoding: string index 17 Read GetStrProp Write SetStrProp;
  property ContentLength: string index 18 Read GetStrProp Write SetStrProp;
  property TransferEncoding: string index 19 Read GetStrProp Write SetStrProp;
  //
  property Date: TDateTime index 0 Read GetDateProp Write SetDateProp;
  // local date of serving the request (is converted to UTC)    (filled by Server)
  property LastModified: TDateTime index 1 Read GetDateProp Write SetDateProp;
  // local date of file modification (is converted to UTC)   (filled by ServeFile method)
  property LastModifiedUTC: TDateTime index 2 Read GetDateProp Write SetDateProp;
  // UTC date of file modification   (filled by ServeFile method)
  property Expires: TDateTime index 3 Read GetDateProp Write SetDateProp;
  // UTC date of expiration (for caches, allows caching of otherwise-non-cacheable responses)
  property ResponseSent: boolean Read FResponseSent Write FResponseSent;
 end;

 TSynOnHttpGet = procedure(Sender: TObject; Connection: TSynTcpSrvConnection;
  Request, Response: THttpRequest) of object;
 TSynOnHttpExpect = procedure(Sender: TObject; Request: THttpRequest; var bnContinue: boolean) of object;
 TSynHTTPCreatePostStream = procedure(Sender: TObject; Request: THttpRequest; var PostStream: TStream) of object;

 // Virtual HTTP server.
 // This level does some RFC2616 stuff for you,
 // but it does NOT resolve URL->filename, which must be done in OnHttpGet method.

 { TSynHttpServer }

 TSynHttpServer = class(TSynTcpServer)
 private
  FOnCreatePostStream: TSynHTTPCreatePostStream;
  FOnHttpGet:  TSynOnHttpGet;
  FOnExpect:   TSynOnHttpExpect;
  FCertFile:   string;
  FKeyFile:    string;
  FKeyPass:    string;
  FCaCertFile: string;
  procedure HandleClientCommand(Connection: TSynTcpSrvConnection; Command: string);
  procedure CreatePostStream(Request: THttpRequest);
 protected
  procedure ReadRequest(Connection: TSynTcpSrvConnection; Request, Reply: THttpRequest; Command: string); virtual;
  procedure DoHttpGet(Connection: TSynTcpSrvConnection; Request, Reply: THttpRequest); virtual;
  procedure SetActive(Value: boolean); override;
 public
  constructor Create(AOwner: TComponent); override;
  //
  procedure InitHttps(const CertFile, KeyFile, KeyPassword, CaCertFile: string);
  procedure SendReply(Connection: TSynTcpSrvConnection; Request, Reply: THttpRequest); virtual;
  //
 published
  property Port;//default '80';
  //
  property OnHttpGet: TSynOnHttpGet Read FOnHttpGet Write FOnHttpGet;
  property OnExpect: TSynOnHttpExpect Read FOnExpect Write FOnExpect;
  property OnCreatePostStream: TSynHTTPCreatePostStream Read FOnCreatePostStream Write FOnCreatePostStream;
 end;

var
 // Value for Server: header...
 ServerValue: string = 'SynHttpSrv/1.0';

function ReadHeadersFromSocket(Socket: TTCPBlockSocket; Headers: THeaderList; LineTimeout: integer = 0): boolean;

function SendSocketStream(Socket: TTcpBlockSocket; Stream: TStream; MaxSize: int64 = -1;
 bnHttpChunked: boolean = False): boolean;

const
 cProtoHttp10 = 'HTTP/1.0';
 cProtoHttp11 = 'HTTP/1.1';

function GetHttpStatusMsg(StatusCode: integer; var StatusMsg: string): boolean;

 //-----------------------------------------------------------------------------
 // string utility functions:

// Trim(Copy(S,Pos,Count));
function TrimCopy(const S: string; Pos, Count: integer): string;
// trim inplace:
procedure DoTrim(var S: string);
// remove first token, no quoting:
function FetchToken(var Line: string; const Sep: string; bnTrim: boolean): string;
// "Quote value, using \" and \\ inside..."
function QuoteValue(const Value: string): string;
// remove first comma-separated value, possibly quoted
function FetchQSepValue(var Line: string; const Sep: string): string;
// for parsing: remove first  Name="Value", separators either ";" or ","
function FetchDequoted(var Line: string; out Name, Value: string): boolean;
// get value from Name="Value" in multi-prop header value:  (from 'text/html; charset="Windows-1250"' can extract charset...)
function GetHeaderSubValue(Header: string; const Name: string): string;
procedure ReplaceHeaderSubValue(var Header: string; const Name, Value: string);
function CombineStrings(Strings: TStrings; const Separator: string): string;
// SameHead == SameText(Copy(Str,1,Length(SHead)),SHead)
function SameHead(const Str, SHead: string): boolean;
// multipart parsing...
type
 // Result: True=found/stop, False=continue
 TMultipartEnumCallback = function(Headers: THeaderList; const FieldName, Content: string): boolean of object;

procedure EnumMultiPart(ContentData, Boundary: string; const Enum: TMultipartEnumCallback);

// Date - in HTTP (RFC2616), all dates MUST be in GMT (utc) format...
function FormatHttpDate(LocalDate: TDateTime; bnIsLocal: boolean): string;
function ParseHttpDate(Str: string; out DateTime: TDateTime): boolean;
function LocalToUtcDateTime(LocalDate: TDateTime): TDateTime;
function UtcToLocalDateTime(UtcDate: TDateTime): TDateTime;
function TimeZoneBiasTime: TDateTime;
function GetFileDateUtc(const FileName: string): TDateTime;

// Content-Type detection used by THttpRequest.ServeFile
function DetectContentType(const FileName: string): string;
function GetContentTypeByExt(const Ext: string): string;
// RegisterContentType can be used to register content-types by extension from user configuration:
procedure RegisterContentType(const Ext, ContentType: string);
{$ifdef MSWINDOWS}
// Automatically register content-types for all file extensions from registry...
procedure RegisterContentTypesFromRegistry;
{$endif MSWINDOWS}

// convert 'Documents%20and%20Settings'  to 'Documents and Settings', also handles utf8 encoded in %C4%8D...
function ConvertUrlChars(Url: string): string;
procedure TryDecodeUtf8(var Url: string); // used by ConvertUrlChars...

var
 // location of /error.html file, used by THttpRequest.ServerFile:
 Error404Url:     string;
 // contents of 404 error doc, used by THttpRequest.ServerFile, only if Error404Url is empty:
 Error404DocText: string;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('Samples', [TSynHttpServer]);
end;

function SendSocketStream(Socket: TTcpBlockSocket; Stream: TStream; MaxSize: int64; bnHttpChunked: boolean): boolean;
var
 Buffer: array[0..16383] of char;
 BlockSize, Size: integer;
label
 _Complete;
begin
 if (MaxSize < 0) then
  MaxSize := $10000000000; // 16Gb...
 //
 // Send Stream, without asking its Size... This allows sending from TDecompressionStream etc...
 BlockSize := Socket.SendMaxChunk;
 if (BlockSize > SizeOf(Buffer)) then
  BlockSize := SizeOf(Buffer); // no real need to send >4k packets...
 //
 while True do
 begin
  if (BlockSize > MaxSize) then
  begin
   // Last block...
   if (MaxSize = 0) then
   begin
    Result := True;
    goto _Complete;
   end;
   BlockSize := MaxSize;
  end;
  //
  Size := Stream.Read(Buffer[0], BlockSize);
  if (Size <= 0) then
  begin
   // EOF
   Result := (Size = 0); // stream complete...
   _Complete:
    if Result and bnHttpChunked then
    begin
     Socket.SendString('0'#13#10#13#10);
     Result := True;
    end;
   exit;
  end;
  //
  if bnHttpChunked then
   Socket.SendString(UTF8Encode(Format('%x'#13#10, [Size])));
  //
  Socket.SendBuffer(@Buffer, Size);
  if (Socket.LastError <> 0) then
   break;
 end;
 // Failed due to LastError
 Result := False;
end;

// read header lines until empty line is received...
function ReadHeadersFromSocket(Socket: TTCPBlockSocket; Headers: THeaderList; LineTimeout: integer): boolean;
var
 Line: string;
begin
 if (LineTimeout = 0) then
  LineTimeout := SynSrv.cDefLineTimeout; // default 2 minutes...
 //
 while True do
 begin
  Line := string(Socket.RecvString(LineTimeout));
  if (Line = '') then
  begin
   if (Socket.LastError <> 0) then
   begin
    // error (either timeout or client disconnected)
    Result := False;
    exit;
   end;
   // Headers complete (terminated by empty line)
      {$ifdef DEBUG}
      Debug('Request headers:'#13#10'%s',[Headers.Text]);
      {$endif DEBUG}
   Result := True;
   exit;
  end;
  Headers.Add(Line);
 end;
end;

function TrimCopy(const S: string; Pos, Count: integer): string;
var
 len, maxlen: integer;
begin
 //Result:=Trim(Copy(S,Pos,Count));
 // Optimized - trim before allocating result:
 len := Length(S);
 while (Pos <= len) and (S[Pos] <= ' ') do
  Inc(Pos);
 if (Pos <= len) then
 begin
  maxlen := len - Pos + 1;
  if (Count > maxlen) then
   Count := maxlen;
  while (Count > 0) and (S[Pos + Count - 1] <= ' ') do
   Dec(Count);
 end;
 Result := Copy(S, Pos, Count);
end;

procedure DoTrim(var S: string);
var
 len: integer;
begin
 len := Length(S);
 if (len > 0) and ((S[1] <= ' ') or (S[len] <= ' ')) then
  S := Trim(S);
end;

function FetchToken(var Line: string; const Sep: string; bnTrim: boolean): string;
var
 p: integer;
begin
 p := Pos(Sep, Line);
 if (p > 0) then
 begin
  // give part until separator:
  if bnTrim then
  begin
   Result := TrimCopy(Line, 1, p - 1);
   Delete(Line, 1, p + Length(Sep) - 1);
   DoTrim(Line);
  end else
  begin
   Result := Copy(Line, 1, p - 1);
   Delete(Line, 1, p + Length(Sep) - 1);
  end;
 end else
 begin
  // give all rest:
  Result := Line;
  Line := '';
  if bnTrim then
   DoTrim(Result);
 end;
end;

procedure AdjustHeaderLine(var Line: string);
var
 p, len: integer;
 Name: string;
begin
 // Right-trim:
 len := Length(Line);
 if (len = 0) then
  exit;
 if (Line[1] <= ' ') then
  Line := Trim(Line)
 else
 if (Line[len] <= ' ') then
  Line := TrimRight(Line);
 // Normalize arround ":"...
 p := Pos(':', Line);
 if (p > 1) then
  if (Line[p - 1] <= ' ') or not (Line[p + 1] <= ' ') or (Line[p + 2] <= ' ') then
  begin
   // Needs normalize...
   Name := FetchToken(Line, ':', True);
   //
   Line := Name + ': ' + Line;
  end;
end;

 // for parsing: remove first  Name="Value", separators either ";" or ","
 // Value may be quoted, but does not need to be quoted
 // Name may be missing (if no "=" is found, whole is Value)
function FetchDequoted(var Line: string; out Name, Value: string): boolean;
var
 len, startname, lenname, startvalue, lenvalue, Skip, rest, p: integer;
 bnName, bnSlash: boolean;
begin
 len := Length(Line);
 // LTrim name:
 startname := 1;
 while (startname <= len) and (Line[startname] <= ' ') do
  Inc(startname);
 startvalue := startname;
 //
 if (startname > len) then
 begin
  // Line was empty (or blank)...
  Line  := '';
  Name  := '';
  Value := '';
  Result := False;
  exit;
 end;
 //
 // Seek end of name:
 bnName  := False;
 lenname := 0;
 lenvalue := 0;
 while (startname + lenname <= len) do
 begin
  case Line[startname + lenname] of
   ';', ',', '"': break;
   '=':
   begin
    // End of name:
    startvalue := startname + lenname + 1;
    bnName := True;
    break;
   end;
  end;
  Inc(lenname);
 end;
 if not bnName then
 begin
  // no name...
  //startvalue:=startname; // already...
  lenvalue := lenname;
  lenname  := 0;
 end;
 Name := TrimCopy(Line, startname, lenname);
 //
 Skip := 0;
 bnSlash := False;
 if (lenvalue = 0) then
 begin
  // ltrim:
  while (startvalue <= len) and (Line[startvalue] <= ' ') do
   Inc(startvalue);
  lenvalue := 0;
  if (Line[startvalue] = '"') then
  begin
   // quoted:
   Inc(startvalue);
   lenvalue := 0;
   while (startvalue + lenvalue <= len) do
   begin
    case Line[startvalue + lenvalue] of
     '\':
     begin
      bnSlash := True;
      Inc(lenvalue);
     end;
     '"':
     begin
      // end-quote...
      Skip := 1;
      break;
     end;
    end;
    Inc(lenvalue);
   end;
  end else
   while (startvalue + lenvalue <= len) do
   begin
    case Line[startvalue + lenvalue] of
     ';', ',': break;
    end;
    Inc(lenvalue);
   end// separated:
  ;
 end;
 Value := TrimCopy(Line, startvalue, lenvalue);
 //
 rest  := startvalue + lenvalue + Skip;
 while (rest <= len) and (Line[rest] <= ' ') do
  Inc(rest);
 if (rest <= len) and (CharInSet(Line[rest], [';', ','])) then
  Inc(rest);
 Line := TrimCopy(Line, rest, Length(Line) - rest + 1);
 //
 if bnSlash then
 begin
  // Remove middle quoting markup:
  len := Length(Value);
  p := 1;
  while (p <= len) do
  begin
   if (Value[p] = '\') then
   begin
    Delete(Value, p, 1);
    Dec(len);
   end;
   Inc(p);
  end;
 end;
 //
 Result := True;
end;

function GetHeaderSubValue(Header: string; const Name: string): string;
var
 S: string;
begin
 Result := '';
 while (Header <> '') do
 begin
  FetchDequoted(Header, S, Result);
  if SameText(S, Name) then
   break;//exit;
  Result := '';
 end;
end;

procedure ReplaceHeaderSubValue(var Header: string; const Name, Value: string);
var
 Parts: TStringList;
 S, S2: string;
 ls2: integer;
begin
 // find existing Name="Value", value may be quoted and may be not quoted, Name= may occur inside other quoted value so may not use simple Pos()...
 Parts := TStringList.Create;
 try
  S2  := Name + '=';
  ls2 := Length(S2);
  //
  while (Header <> '') do
  begin
   S := Trim(FetchQSepValue(Header, ';'));
   //
   if (S <> '') and (ls2 >= Length(S)) and (S[ls2] = '=') and SameHead(S, S2)
   //and SameText(Copy(S,1,ls2),S2)
   then
   begin
    // Replace this:
    S := S2 + QuoteValue(Value);
    ls2 := 0;
   end;
   //
   Parts.Add(S);
  end;
  //
  if (ls2 > 0) then
   Parts.Add(S2 + QuoteValue(Value))// was not found...
  ;
  //
  // Combine into string:
  Header := CombineStrings(Parts, '; ');
  //
 finally
  Parts.Free;
 end;
end;

function CombineStrings(Strings: TStrings; const Separator: string): string;
var
 S: string;
 i: integer;
begin
 Result := '';
 for i := 0 to Strings.Count - 1 do
 begin
  S := Strings[i];
  if (i > 0) then
   Result := Result + Separator + S
  else
   Result := Result + S;
 end;
end;

function SameHead(const Str, SHead: string): boolean;
begin
 Result := SameText(Copy(Str, 1, Length(SHead)), SHead);
end;

const
 // SysUtils.ShortDayNames may be translated with resources... here use constants:
 UsShortDayNames: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
 UsShortMonthNames: array[1..12] of string =
  ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

function FormatHttpDate(LocalDate: TDateTime; bnIsLocal: boolean): string;
var
 UtcDate: TDateTime;
 d, m, y, h, n, s, z: word;
begin
 if (LocalDate <= 1) then
 begin
  Result := '';
  exit;
 end;
 // This format is recomended by RFC2616. it MUST be in GMT time-zone...
 // Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 if bnIsLocal then
  UtcDate := LocalToUtcDateTime(LocalDate)
 else
  UtcDate := LocalDate;
 DecodeDate(UtcDate, y, m, d);
 DecodeTime(UtcDate, h, n, s, z);
 Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT', [UsShortDayNames[DayOfWeek(UtcDate)],
  d, UsShortMonthNames[m], y, h, n, s]);
end;

function LocalToUtcDateTime(LocalDate: TDateTime): TDateTime;
begin
 // UTC = local_time + bias
 if (LocalDate <> 0) then
  Result := LocalDate + TimeZoneBiasTime()
 else
  Result := 0;
end;

function UtcToLocalDateTime(UtcDate: TDateTime): TDateTime;
begin
 // local_time = UTC - bias
 if (UtcDate <> 0) then
  Result := UtcDate - TimeZoneBiasTime()
 else
  Result := 0;
end;

const
 cMinuteToDateTime = 1 / (24 * 60);

{$undef WIN32FILETIME}
{$undef WIN32TZ}
{$ifdef MSWINDOWS} {$ifndef CIL}

{$define WIN32TZ}
function TimeZoneBiasTime: TDateTime;
var
 tzi:  TTimeZoneInformation;
 Bias: integer;
begin
 case GetTimeZoneInformation(tzi) of
  TIME_ZONE_ID_UNKNOWN: Bias  := tzi.Bias;
  TIME_ZONE_ID_STANDARD: Bias := tzi.Bias + tzi.StandardBias;
  TIME_ZONE_ID_DAYLIGHT: Bias := tzi.Bias + tzi.DaylightBias;
  else
   Bias := 0;
 end;
 if (Bias <> 0) then
  Result := Bias * cMinuteToDateTime
 else
  Result := 0;
end;

{$define WIN32FILETIME}
function FileTimeToUtcDateTime(const FileTime: TFileTime): TDateTime;
var
 Sys: TSystemTime;
begin
 if FileTimeToSystemTime(FileTime, Sys) then
  Result := EncodeDate(Sys.wYear, Sys.wMonth, Sys.wDay) + EncodeTime(Sys.wHour, Sys.wMinute,
   Sys.wSecond, Sys.wMilliseconds)
 else
  Result := 0;
end;

{$endif}{$endif}
//
{$ifndef WIN32TZ} // fallback for dotnet & linux:
//const
//  cMinuteToDateTime=1/(24*60);

function TimeZoneBiasTime: TDateTime;
begin
 Result := SynaUtil.TimeZoneBias*cMinuteToDateTime;
end;
{$endif}

function GetFileDateUtc(const FileName: string): TDateTime;
var
 SR: TSearchRec;
begin
 // This could work on linux also?
 if (FindFirst(FileName, faAnyFile, SR) = 0) then
 begin
  FindClose(SR);
  //
    {$ifdef WIN32FILETIME}// WIN32
  // Here we have directly UTC date-time:
  Result := FileTimeToUtcDateTime(SR.FindData.ftLastWriteTime);
    {$else ->fallback}
    Result:=LocalToUtcDateTime(FileDateToDateTime(SR.Time));
    {$endif}
 end else
  Result := 0;
end;

function ParseShortMonthName(const Token: string): integer;
var
 i: integer;
begin
 for i := 1 to 12 do
  if SameText(Token, UsShortMonthNames[i]) then
  begin
   Result := i;
   exit;
  end;
 Result := 0;
end;

function ParseHttpDate(Str: string; out DateTime: TDateTime): boolean;
var
 Token: string;
 Int, y, m, d, h, n, s, tzh, tzm, tokencount: integer;
 TzOffset: double;
begin
 DateTime := 0;
 // This format is recomended by RFC2616. it MUST be in GMT time-zone...
 // Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 // These formats are also possible:
 // Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
 // Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
 // Anyhow due to robustness we will parse +0000 and -0000 timezones also...
 y := 0;
 m := 0;
 d := 0;
 h := 0;
 n := 0;
 s := 0;
 tokencount := 0;
 TzOffset := 0;
 while (Str <> '') do
 begin
  Token := FetchToken(Str, ' ', True);
  if (Token = '') then
   continue;
  //
  Inc(tokencount);
  if (tokencount > 31) then
   break;
  //
  Int := -1;
  if (CharInSet(Token[1], ['0'..'9'])) then
   Int := StrToIntDef(Token, -1);
  //
  case Length(Token) of
   1, 2: if (d = 0) and (Int > 0) then
     d := Int;// Day...

   3: if (m = 0) and (Int < 0) then
     m := ParseShortMonthName(Token);// Sun, GMT, Nov

   4: if (y = 0) and (Int >= 1900) and (Int <= 2200) then
     y := Int;// 1999

   5: if (CharInSet(Token[1], ['-', '+'])) and (CharInSet(Token[2], ['0'..'2'])) then
    begin
     // +0200, -0200
     tzh := StrToIntDef(Copy(Token, 2, 2), -1);
     tzm := StrToIntDef(Copy(Token, 4, 2), -1);
     if (tzh >= 0) and (tzm >= 0) then
     begin
      TzOffset := (tzh * (1 / 24)) + (tzm * (1 / (24 * 60)));
      if (Token[1] = '+') then
       TzOffset := -TzOffset;
     end;
    end;
   else
    if (Pos(':', Token) > 0) then
    begin
     // Time...
     h := StrToIntDef(FetchToken(Token, ':', True), 0);
     n := StrToIntDef(FetchToken(Token, ':', True), 0);
     s := StrToIntDef(FetchToken(Token, ':', True), 0);
    end else
    if (d = 0) and (Pos('-', Token) > 0) then
    begin
     // 06-Nov-94
     d := StrToIntDef(FetchToken(Token, '-', True), 0);
     m := ParseShortMonthName(FetchToken(Token, '-', True));
     if (m <> 0) then
     begin
      y := StrToIntDef(Token, -1);
      if (y >= 0) then
       if (y > 50) then
        Inc(y, 1900)
       else
        Inc(y, 2000);
     end;
    end;
  end;
 end;
 //
 if (m > 0) and (m <= 12) and (y >= 1900) and (d > 0) and (d <= MonthDays[IsLeapYear(y), m]) then
 begin
  // Valid date...
  DateTime := EncodeDate(y, m, d);
  // Check time:
  if (h >= 0) and (h <= 23) and (n >= 0) and (n <= 59) and (s >= 0) and (s <= 59) then
   DateTime := DateTime + EncodeTime(h, n, s, 0) + TzOffset;
  Result := True;
 end else
  Result := False;
end;


{$ifdef MSWINDOWS} {$ifndef CIL} {$define LOCALUTF} {$endif}{$endif}

{$ifdef LOCALUTF}
//For compatibility with Delphi5, use our and kernel functions...

 //U+00000000 - U+0000007F   0xxxxxxx
 //U+00000080 - U+000007FF   110xxxxx 10xxxxxx
 //U+00000800 - U+0000FFFF   1110xxxx 10xxxxxx 10xxxxxx
 //U+00010000 - U+001FFFFF   11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 //U+00200000 - U+03FFFFFF   111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
 //U+04000000 - U+7FFFFFFF   1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx

function GetUtfCharLen(pc: PChar): integer;
var
 b: byte;
begin
 b := Ord(pc[0]);
 case b and $C0 of
  0, $40: Result := 1;
  $C0: case b and $30 of
    $00, $10: if (Ord(pc[1]) and $C0 = $80) then
      Result := 2
     else
      Result := 0;// 2 bytes:

    $20: if (Ord(pc[1]) and $C0 = $80) and (Ord(pc[2]) and $C0 = $80) then
      Result := 3
     else
      Result := 0;// 3 bytes:

    else
     Result := 0;
     // Longer than UCS-2 (unicode >$FFFF) not supported...
   end;// Start multi-byte:

  else
   Result := 0; // illegal
 end;
end;

function IsUtf8(pc: PChar): boolean;
var
 bn80: boolean;
 Len:  integer;
begin
 bn80 := False;
 while (pc^ <> #0) do
  if (byte(pc^) and $80 <> 0) then
  begin
   bn80 := True;
   Len  := GetUtfCharLen(pc);
   if (Len > 0) then
    Inc(pc, Len)
   else
   begin
    // illegal bytes...
    Result := False;
    exit;
   end;
  end else
   Inc(pc);
 Result := bn80;
end;

function FromUtf8ToWin(const S: string): string;
var
 WideBuf: PWideChar;
 Len, WideLen: integer;
begin
 Len := Length(S);
 WideBuf := AllocMem(Len * 2 + 16);
 try
  WideLen := MultiByteToWideChar(CP_UTF8, 0, Pointer(S), Len, WideBuf, Len);
  if (WideLen = 0) then
  begin
   Result := '';
   exit;
  end;
  SetString(Result, PChar(nil), WideLen);
  Len := WideCharToMultiByte(0, 0, WideBuf, WideLen, Pointer(Result), WideLen, '@', nil);
  if (Len < WideLen) then
   SetLength(Result, WideLen);
 finally
  FreeMem(WideBuf);
 end;
end;

procedure TryDecodeUtf8(var Url: string);
begin
 if IsUtf8(PChar(Url)) then
  Url := FromUtf8ToWin(Url);
end;

{$else ->Delphi7+}

// For Delphi7+ can use function in pascal System unit...
procedure TryDecodeUtf8(var Url: string);
var S: string;
begin
  S:=UTF8ToString(Url); // returns empty, if not valid Utf8...
  if (S<>'') then
    Url:=S;
end;
{$endif}

function ValHex(const S: string; var Value: integer): boolean;
var
 code: integer;
begin
 Val('$' + S, Value, Code);
 Result := Code = 0;
end;

function ConvertUrlChars(Url: string): string;
var
 p, len, code: integer;
 bnUtf: boolean;
 buff:  ansistring;
begin
 // convert 'Documents%20and%20Settings'  to 'Documents and Settings'
 // and A+B to A B
 Url := Url.Replace('+', ' ');
 p := Pos('%', Url);
 if (p = 0) then
  Exit(Url);
 //
 // Exit(TIdURI.URLDecode(Url));
 buff := ansistring(Url);
 bnUtf := False;
 len := Length(buff);
 while (p <= len) do
 begin
  if (buff[p] = '%') then
   if ValHex(Copy(string(buff), p + 1, 2), code) then
   begin
    Delete(buff, p + 1, 2);
    Dec(len, 2);
    buff[p] := ansichar(code);
    if (code > $80) then
     bnUtf := True;
   end;
  Inc(p);
 end;
 //
 if bnUtf then
  Result := UTF8ToString(RawByteString(buff))
 else
  Result := string(buff);
end;

{ THeaderList }

function THeaderList.Add(const S: string): integer;
var
 Index: integer;
 Line:  string;
begin
 // No empty lines:
 Line := S;
 if (Line = '') then
  Exit(-1);
 // Check multi-line headers:
 if (Line[1] <= ' ') then
 begin
  Index := Count - 1;
  if (Index >= 0) then
  begin
   // Append to last line:
   Strings[Index] := Strings[Index] + #13#10 + Line; // line includes leading blank...
   Exit(Index);
  end;
 end;
 // Common adjustment (trim and normalize arround ":")
 AdjustHeaderLine(Line);
 //
 Result := inherited Add(Line);
end;

procedure THeaderList.Insert(Index: integer; const S: string);
var
 S2: string;
begin
 S2 := S;
 if (S2 <> '') then
 begin
  // Common adjustment (trim and normalize arround ":")
  AdjustHeaderLine(S2);
  //
  inherited Insert(Index, S2);
 end;
end;

procedure THeaderList.Put(Index: integer; const S: string);
var
 S2: string;
begin
 S2 := S;
 if (S2 <> '') then
 begin
  // Common adjustment (trim and normalize arround ":")
  AdjustHeaderLine(S2);
  //
  inherited Put(Index, S2);
 end;
end;

procedure THeaderList.AddValue(const Name, Value: string);
var
 S: string;
begin
 if (Name <> '') and (Value <> '') then
 begin
  S := Name + ': ' + Value;
  AdjustHeaderLine(S);
  inherited Add(S);
 end;
end;

function THeaderList.IndexOfName(const Name: string): integer;
var
 i, len: integer;
 S: string;
begin
 Result := -1;
 len := Length(Name);
 if (len > 0) then
  for i := 0 to Count - 1 do
  begin
   S := Strings[i];
   if (Length(S) > len) and (S[len + 1] = ':') and SameHead(S, Name) //and SameText(Copy(S,1,len),Name)
   then
   begin
    Result := i;
    break;//exit;
   end;
  end;
end;

function IsName(const Line, Name: string): boolean;
var
 len: integer;
begin
 len := Length(Name);
 if (len > 0) and (Length(Line) > len) and (Line[len + 1] = ':') and SameHead(Line, Name)
 //and SameText(Copy(Line,1,len),Name)
 then
  Result := True
 else
  Result := False;
end;

procedure LStrDel(var S: string; Index, Count: integer);
begin
 Delete(S, Index, Count);
end;

// returns pos after quote...
function StrSkipQuoted(const S: string; pquote: integer): integer;
var
 Q: char;
 p, len: integer;
begin
 p := pquote;
 Q := S[p];
 Inc(p);
 len := Length(S);
 while (p <= len) do
  if (S[p] = Q) then
  begin
   // Found...
   Inc(p);
   Exit(p);
  end else
  if (S[p] = '\') then
   Inc(p, 2)
  else
   Inc(p);
 Result := 0;
end;

// seek next occurence after this pos:
function StrSeek(const S: string; C: char; StartPos: integer): integer;
var
 p, len: integer;
begin
 len := Length(S);
 p := StartPos;
 if (p <= 0) then
  p := 1;
 while (p <= len) do
 begin
  if (S[p] = C) then
   Exit(p);
  Inc(p);
 end;
 Result := len + 1;
end;

// remove first comma-separated value
function FetchQSepValue(var Line: string; const Sep: string): string;
var
 pcomma, pquote, p, len: integer;
begin
 // values are separated by "," but there may be another such in quotes...
 pcomma := Pos(Sep, Line);
 if (pcomma = 0) then
 begin
  // whole line is last part:
  Result := Trim(Line);
  Line := '';
  Exit;
 end;
 // skip quoted content:
 pquote := Pos('"', Line);
 while (pquote > 0) and (pquote < pcomma) do
 begin
  // May be quoted, may have multiple quoted parts...
  p := StrSkipQuoted(Line, pquote);
  pquote := StrSeek(Line, '"', p);
  pcomma := StrSeek(Line, Sep[1], p);
  if (pcomma = 0) then
  begin
   // whole line is last part:
   Result := Trim(Line);
   Line := '';
   exit;
  end;
 end;
 // Extract part:
 Result := TrimCopy(Line, 1, pcomma - 1);
 // Remove part, comma and spaces:
 len := Length(Line);
 p := pcomma;
 while (p < len) and ((Line[p + 1] <= ' ') or (Line[p + 1] = Sep[1])) do
  Inc(p);
 Delete(Line, 1, p);
end;

// according to RFC2616, comma-separated headers may be also duplicated...
procedure THeaderList.EnumHeaders(const Name: string; const Enum: THeaderEnum; const Sep: char;
 LParam: NativeUInt = 0);
var
 i, Index, Cnt: integer;
 Line, Value: string;
begin
 Index := IndexOfName(Name);
 if (Index >= 0) then
 begin
  i := Index;
  Line := Strings[i];
  while True do
  begin
   // Process this line:
   LStrDel(Line, 1, Length(Name) + 2); // remove 'Name: '
   Line := Trim(Line);
   //
   while (Line <> '') do
   begin
    Value := FetchQSepValue(Line, Sep);
    if (Value <> '') then
     if Enum(Value, LParam) then
      Exit;
   end;
   // find next...
   Inc(i);
   Cnt := Count;
   while (i < Cnt) do
   begin
    Line := Strings[i];
    if IsName(Line, Name) then
     break;
    Inc(i);
   end;
   if (i >= Cnt) then
    break;
  end;
 end;
end;

{$ifndef CIL}
// Simple pascal:
type
 PHeaderFinder = ^THeaderFinder;

 THeaderFinder = record
  FindValue: PString;
  Found: boolean;
 end;

{$else ->dotnet is more complicated}
type
  THeaderFinder=class(TObject)
  public
    FindValue: string;
    Found: Boolean;
    function CheckHttpFindValue(const Value: string; LParam: Longint): Boolean;
  end;
  PHeaderFinder=THeaderFinder;

function THeaderFinder.CheckHttpFindValue(const Value: string; LParam: Longint): Boolean;
var S: string;
begin
  S:=Value;
  if SameText(FetchToken(S,'=',True),FindValue) then begin
    Found:=True;
    Result:=True; // stop.
  end else
    Result:=False; // continue...
end;
{$endif}

function THeaderList.CheckHttpFindValue(const Value: string; LParam: NativeUInt): boolean;
{$ifndef CIL}
var
 S: string;
 Finder: PHeaderFinder;
{$endif}
begin
  {$ifndef CIL}
 Finder := PHeaderFinder(LParam);
 S := Value;
 if SameText(FetchToken(S, '=', True), Finder.FindValue^) then
 begin
  Finder.Found := True;
  Result := True; // stop.
 end else
  {$endif}
  Result := False; // continue...
end;

function THeaderList.HasValue(const Name, Value: string): boolean;
var
 Finder: THeaderFinder;
begin
  {$ifndef CIL}
 // Simple:
 Finder.FindValue := @Value;
 Finder.Found := False;
 //
 EnumHeaders(Name, Self.CheckHttpFindValue, ',', NativeUInt(@Finder));
 Result := Finder.Found;
 //
  {$else ->dotnet, little more complicated}
  //
  Finder:=THeaderFinder.Create;
  Finder.FindValue:=Value;
  Finder.Found:=False;
  EnumHeaders(Name,Finder.CheckHttpFindValue,0);
  Result:=Finder.Found;
  Finder.Free;
  {$endif}
end;

function THeaderList.GetValueByName(const Name: string): string;
var
 Index, p: integer;
begin
 Index := IndexOfName(Name);
 if (Index >= 0) then
 begin
  //Result:=GetValueByIndex(Index);
  Result := Strings[Index];
  //System.Delete(Result,Length(Name)+2); // remove 'Name: '
  p := Length(Name) + 2;
  Result := Copy(Result, p + 1, Length(Result) - p);
 end else
  Result := '';
end;

procedure THeaderList.SetValueByName(const Name, Value: string);
var
 Index: integer;
 S: string;
begin
 if (Name <> '') then
  if (Value <> '') then
  begin
   Index := IndexOfName(Name);
   S := Trim(Name) + ': ' + Trim(Value);
   if (Index >= 0) then
    inherited Put(Index, S) //Strings[Index]:=S
   else
    inherited Add(S);
  end else
   RemoveValue(Name);
end;

function THeaderList.RemoveValue(const Name: string): boolean;
var
 Index, Count: integer;
begin
 Result := False;
 Index  := IndexOfName(Name);
 if (Index >= 0) then
 begin
  Delete(Index);
  Result := True;
  //
  // Remove all occurences:
  Count  := Self.Count;
  while (Index < Count) do
   if IsName(Strings[Index], Name) then
   begin
    Delete(Index);
    Dec(Count);
   end else
    Inc(Index);
 end;
end;

function THeaderList.GetNameByIndex(Index: integer): string;
var
 p: integer;
begin
 Result := Strings[Index];
 p := Pos(':', Result);
 if (p > 0) then
  Result := Copy(Result, 1, p - 1)
 else
  Result := '';
end;

function THeaderList.GetValueByIndex(Index: integer): string;
var
 p: integer;
begin
 Result := Strings[Index];
 p := Pos(':', Result);
 if (p > 0) then
 begin
  Inc(p); // remove space after colon also...
  Result := TrimCopy(Result, p + 1, Length(Result) - p);
 end;
end;

function THeaderList.GetSubValue(const Name, SubName: string): string;
begin
 Result := Values[Name];
 if (Result <> '') then
  Result := GetHeaderSubValue(Result, SubName);
end;

procedure THeaderList.SetSubValue(const Name, SubName, Value: string);
var
 S: string;
 Index: integer;
begin
 Index := IndexOfName(Name);
 if (Index >= 0) then
  S := ValuesByIndex[Index]//Values[Name];
 else
  S := '';
 //
 if (S <> '') then
  ReplaceHeaderSubValue(S, SubName, Value)// Replace in existing header:
 else
  S := Format('%s=%s', [SubName, QuoteValue(Value)]);
 //
 S := Trim(Name) + ': ' + Trim(S);
 //
 if (Index >= 0) then
  inherited Put(Index, S)
 else
  inherited Add(S);
end;

{ THttpRequest }

constructor THttpRequest.Create;
begin
 inherited Create;
 FHeaders := THeaderList.Create;
 FParams  := TStringList.Create;
 FCookies := THttpCookies.Create;
end;

destructor THttpRequest.Destroy;
begin
 FreeAndNil({FContentStream}FPostStream);
 FreeAndNil(FHeaders);
 FreeAndNil(FParams);
 FreeAndNil(FCookies);
 inherited;
end;

procedure THttpRequest.Assign(Source: TPersistent);
var
 Req: THttpRequest;
 Lines: TStrings;
 Temp2: TStringList;
 i, Count: integer;
 S: string;
begin
 if (Source is THttpRequest) then
 begin
  Req := THttpRequest(Source);
  FHeaders.Assign(Req.FHeaders);
  FCookies.Assign(Req.FCookies);
  FUrl := Req.FUrl;
  FMethod := Req.FMethod;
  FProtocol := Req.FProtocol;
  FContent := Req.FContent;
  //FContentStream := Req.FContentStream;
  //Req.FContentStream := nil; // only 1 request may own the content stream...
  FPostStream := Req.FPostStream;
  Req.FPostStream := nil; // only 1 request may own the content stream...
  FStatusCode := Req.FStatusCode;
  FStatusMsg := Req.FStatusMsg;
  FConnection := Req.FConnection;
  FFlags := Req.FFlags;
 end else
 if (Source is TStrings) then
 begin
  Lines := TStrings(Source);
  Temp2 := nil;
  try
   // Load headers:
   Headers.Clear;
   i := 0;
   Count := Lines.Count;
   while (i < Count) do
   begin
    S := Lines[i];
    if (S = '') then
    begin
     // End of headers...
     Inc(i);
     break;
    end;
    Headers.Add(S);
    Inc(i);
   end;
   //
   if (i < Count) then
   begin
    // Load content:
    // It is usually much faster to copy strings to new list than to delete items from start...
    Temp2 := TStringList.Create;
    Temp2.Capacity := Count - i;
    while (i < Count) do
    begin
     Temp2.Add(Lines[i]);
     Inc(i);
    end;
    FreeAndNil(Temp2);
    FContent := Temp2.Text;
   end;
  finally
   FreeAndNil(Temp2);
  end;
 end else
  inherited;
end;

procedure THttpRequest.SetHeaders(Value: THeaderList);
begin
 if (Value <> nil) then
  FHeaders.Assign(Value)
 else
  FHeaders.Clear;
end;

procedure THttpRequest.SetCookies(Value: THttpCookies);
begin
 if (Value <> nil) then
  FCookies.Assign(Value)
 else
  FCookies.Clear;
end;

type
 THttpStatusMsg = record
  Code: integer;
  Msg:  string;
 end;

const
 // status codes defined in RFC2616:
 HttpStatusMsgs: array[0..39] of THttpStatusMsg = (
  // Common codes:
  (Code: 200; Msg: 'OK'),
  (Code: 403; Msg: 'Forbidden'),
  (Code: 404; Msg: 'Not Found'),
  (Code: 401; Msg: 'Unauthorized'),
  (Code: 500; Msg: 'Internal Server Error'),
  (Code: 302; Msg: 'Found'), // use this for redirection
  (Code: 304; Msg: 'Not Modified'),
  (Code: 206; Msg: 'Partial Content'),
  //
  (Code: 100; Msg: 'Continue'),
  (Code: 101; Msg: 'Switching Protocols'),
  (Code: 201; Msg: 'Created'),
  (Code: 202; Msg: 'Accepted'),
  (Code: 203; Msg: 'Non-Authoritative Information'),
  (Code: 204; Msg: 'No Content'),
  (Code: 205; Msg: 'Reset Content'),
  (Code: 300; Msg: 'Multiple Choices'),  //also possible for redirection...
  (Code: 301; Msg: 'Moved Permanently'), //also possible for redirection...
  (Code: 303; Msg: 'See Other'),         //also possible for redirection...
  (Code: 305; Msg: 'Use Proxy'),         //also possible for redirection...
  (Code: 307; Msg: 'Temporary Redirect'),//also possible for redirection...
  (Code: 400; Msg: 'Bad Request'),
  (Code: 402; Msg: 'Payment Required'),
  (Code: 405; Msg: 'Method Not Allowed'),
  (Code: 406; Msg: 'Not Acceptable'),
  (Code: 407; Msg: 'Proxy Authentication Required'),
  (Code: 408; Msg: 'Request Timeout'),
  (Code: 409; Msg: 'Conflict'),
  (Code: 410; Msg: 'Gone'),
  (Code: 411; Msg: 'Length Required'),
  (Code: 412; Msg: 'Precondition Failed'),
  (Code: 413; Msg: 'Request Entity Too Large'),
  (Code: 414; Msg: 'Request-URI Too Long'),
  (Code: 415; Msg: 'Unsupported Media Type'),
  (Code: 416; Msg: 'Requested Range Not Satisfiable'),
  (Code: 417; Msg: 'Expectation Failed'),
  (Code: 501; Msg: 'Not Implemented'),
  (Code: 502; Msg: 'Bad Gateway'),
  (Code: 503; Msg: 'Service Unavailable'),
  (Code: 504; Msg: 'Gateway Timeout'),
  (Code: 505; Msg: 'HTTP Version Not Supported')
  );

procedure THttpRequest.SetStatusCode(Value: integer);
begin
 FStatusCode := Value;
 GetHttpStatusMsg(FStatusCode, FStatusMsg);
end;

function GetHttpStatusMsg(StatusCode: integer; var StatusMsg: string): boolean;
var
 i: integer;
begin
 for i := Low(HttpStatusMsgs) to High(HttpStatusMsgs) do
  if (HttpStatusMsgs[i].Code = StatusCode) then
  begin
   StatusMsg := HttpStatusMsgs[i].Msg;
   Result := True;
   exit;
  end;
 Result := False;
end;

function THttpRequest.GetFlagBool(Index: integer): boolean;
var
 Mask: integer;
begin
 Mask := 1 shl Index;
 Result := (FFlags and Mask <> 0);
end;

procedure THttpRequest.SetFlagBool(Index: integer; Value: boolean);
var
 Mask: integer;
begin
 Mask := 1 shl Index;
 if Value then
  FFlags := FFlags or Mask
 else
  FFlags := FFlags and not Mask;
end;

procedure THttpRequest.ApplyHeaders(bnIsServer: boolean);
var
 S: string;
 p: integer;
begin
 if bnIsServer then
  Cookies.LoadClientCookies(Headers)
 else
  Cookies.LoadServerCookies(Headers);
 //
 // Parse parameters in URL:
 FParams.Clear;
 p := Pos('?', Url);
 if (p > 0) then
 begin
  S := Copy(Url, p + 1, Length(Url) - p);
  while (S <> '') do
   FParams.Add(ConvertUrlChars(Trim(FetchQSepValue(S, '&'))));
 end;
end;

{Sample from RFC1867:

Content-type: multipart/form-data, boundary=AaB03x

--AaB03x
content-disposition: form-data; name="field1"

Joe Blow
--AaB03x
content-disposition: form-data; name="pics"; filename="file1.txt"
Content-Type: text/plain

 ... contents of file1.txt ...
--AaB03x--
{}

procedure THttpRequest.ParsePostFormData;
var
 S: string;
 //p: integer;
begin
 if Content.StartsWith('--') then
  EnumMultiPart(Content, Boundary, AddMultiPartFormItem)
 else
 begin
  S := Content;
  while (S <> '') do
   FParams.Add(ConvertUrlChars(Trim(FetchQSepValue(S, '&'))));
 end;
end;

function THttpRequest.AddMultiPartFormItem(Headers: THeaderList; const FieldName, Content: string): boolean;
var
 S: string;
begin
 S := Headers.SubValues['Content-Disposition', 'filename'];
 if (S <> '') then // will add FieldName=filename
 else
  S := Content// will add FieldName=Content
 ;
 //
 if (FieldName <> '') then
  FParams.Add(FieldName + '=' + S)
 else
  FParams.Add(S);
 //
 Result := False; // all...
end;

type
 TGetPostParamInfo = class(TObject)
 public
  ParamName: string;
  ParamData: string;
  bnFound: boolean;
  function FindParamEnum(Headers: THeaderList; const FieldName, Content: string): boolean;
 end;

function TGetPostParamInfo.FindParamEnum(Headers: THeaderList; const FieldName, Content: string): boolean;
begin
 if SameText(FieldName, ParamName) then
 begin
  ParamData := Content;
  bnFound := True;
  Result  := True; // stop.
 end else
  Result := False; // continue...
end;

function THttpRequest.GetPostFormParam(const ParamName: string; var ParamData: string): boolean;
var
 Info: TGetPostParamInfo;
begin
 Info := TGetPostParamInfo.Create;
 try
  Info.ParamName := ParamName;
  Info.ParamData := ParamData;
  EnumMultiPart(FContent, Boundary, Info.FindParamEnum);
  ParamData := Info.ParamData;
  Result := Info.bnFound;
 finally
  Info.Free;
 end;
end;

procedure EnumMultiPart(ContentData, Boundary: string; const Enum: TMultipartEnumCallback);

 function FetchLine(var Rest: string): string;
 begin
  Result := FetchToken(Rest, #13#10, False);
 end;

var
 Line: string;
 Headers: THeaderList;
 p, lbound: integer;
 bnTerm, bnPart: boolean;
begin
 // cannot use TStringList, since it would damage binary parts (uploaded files)?!
 // could consume leading part of ContentData, but it can be very slow on large uploads... well, dotnet is slow anyway...
 //
 if (Boundary = '') then
 begin
  // autodetect boundary:
  while (ContentData <> '') do
  begin
   Line := FetchLine(ContentData);
   if (Line <> '') and (Line[1] = '-') and (Line[2] = '-') then
   begin
    //Delete(Line,1,2); Boundary:='--'+Line;
    Boundary := Line; // contains leading '--'
    break;
   end;
  end;
 end else
 begin
  // Seek leading boundary:
  Insert('--', Boundary, 1);
  while (ContentData <> '') do
  begin
   Line := FetchLine(ContentData);
   if (Line = '') then
    continue;
   if (Line = Boundary) then
    break;
   if (Line = Boundary + '--') then
    exit;
  end;
 end;
 lbound  := Length(Boundary);
 //
 Headers := THeaderList.Create;
 try
  while (ContentData <> '') do
  begin
   // Parse part headers:
   Headers.Clear;
   while (ContentData <> '') do
   begin
    Line := FetchLine(ContentData);
    if (Line = '') then
     break;
    Headers.Add(Line);
   end;
   // Parse part body:
   bnTerm := False;
   bnPart := False;
   p := 1;
   while (p < Length(ContentData)) do
   begin
    if (ContentData[p] = #13) and (ContentData[p + 1] = #10) and (ContentData[p + 2] = '-') and
     (ContentData[p + 3] = '-') and CharInSet(ContentData[p + 2 + lbound], [#13, '-']) and
     CharInSet(ContentData[p + 3 + lbound], [#10, '-']) then
    begin
     Line := Copy(ContentData, p + 2, lbound);
     if (Line = Boundary) then
     begin
      // End of part body here:
      Line := Copy(ContentData, 1, p - 1);
      Inc(p, 2); // skip #13#10
      Inc(p, lbound); // skip '--Boundary'
      bnTerm := (ContentData[p] = '-');
      Inc(p, 2); // skip either #13#10 or '--'
      Delete(ContentData, 1, p);
      //
      if Enum(Headers, Headers.SubValues['Content-Disposition', 'name'], Line) then
       exit;
      Line := '';
      //
      bnPart := True;
      break;
     end;
    end;
    Inc(p);
   end;
   //
   if bnTerm then // final boundary reached...
    break;
   if not bnPart then // input was incomplete, no boundary after data was found...
    break;
  end;
 finally
  Headers.Free;
 end;
end;

const
 StrPropNames: array[0..19] of string = (
  'Content-Type',             // 0
  'Content-Type',             // 1
  'Content-Disposition',      // 2
  'Content-Disposition',      // 3
  'Location',                 // 4
  'Etag',                     // 5
  'Host',                     // 6
  'Referer',                  // 7
  'User-Agent',               // 8
  'Vary',                     // 9
  'WWW-Authenticate',         //10   //!!!TODO
  'Authorization',            //11
  'Content-Type',             //12
  'Content-Encoding',         //13
  'Cache-control',            //14
  'Pragma',                   //15
  'Server',                   //16
  'Accept-Encoding',          //17
  'Content-Length',           //18
  'Transfer-Encoding'         //19
  );

function THttpRequest.GetStrProp(Index: integer): string;
var
 p: integer;
begin
 Result := '';
 if (Index >= 0) and (Index <= High(StrPropNames)) then
 begin
  Result := Headers[StrPropNames[Index]];
  //
  case Index of
   1:
   begin
    // BaseContentType... remove sub-type...
    p := Pos(';', Result);
    if (p > 0) then
     Result := TrimCopy(Result, 1, p - 1);
   end;
   3: Result  := GetHeaderSubValue(Result, 'filename');// TargetFileName, extract it:
   // Content-Disposition: attachment; filename="Filename"    also works without the "attachment"...
   12: Result := GetHeaderSubValue(Result, 'boundary');// Boundary:
  end;
 end;
end;

procedure THttpRequest.SetStrProp(Index: integer; const Value: string);
var
 i: integer;
begin
 if (Index >= 0) and (Index <= High(StrPropNames)) then
  case Index of
   3:
    Headers.SubValues[StrPropNames[Index], 'filename'] := Value;// TargetFileName:
   // Content-Disposition: attachment; filename="Filename"    also works without the "attachment"...
   12:
   begin
    // Boundary:
    if (Headers.Values[StrPropNames[Index]] = '') then
     Headers.Values[StrPropNames[Index]] := 'multipart/mixed';
    //
    Headers.SubValues[StrPropNames[Index], 'boundary'] := Value;
   end;
   18:
   begin
    i := 0;
    if TryStrToInt(Value, i) and (i > 0) then
     Headers[StrPropNames[Index]] := Value;
   end
   else
    Headers[StrPropNames[Index]] := Value;
  end;
end;

const
 DatePropNames: array[0..3] of string = (
  'Date',
  'Last-Modified',
  'Last-Modified',
  'Expires'
  );
 DatePropIsLocal: array[0..3] of boolean = (
  True,
  True,
  False,
  False
  );

procedure THttpRequest.SetCharSet(const Value: string);
begin
 FCharSet := Value;
 Headers.SubValues['Content-Type', 'charset'] := Value;
end;

function THttpRequest.GetDateProp(Index: integer): TDateTime;
begin
 if (Index >= 0) and (Index <= High(DatePropNames)) and ParseHttpDate(Headers[DatePropNames[Index]], Result) then
 begin
  if DatePropIsLocal[Index] then
   Result := UtcToLocalDateTime(Result);
  exit;
 end;
 //
 Result := 0;
end;

procedure THttpRequest.SetDateProp(Index: integer; const Value: TDateTime);
var
 bnIsLocal: boolean;
begin
 if (Index >= 0) and (Index <= High(DatePropNames)) then
 begin
  bnIsLocal := DatePropIsLocal[Index];
  Headers[DatePropNames[Index]] := FormatHttpDate(Value, bnIsLocal);
 end;
end;

procedure THttpRequest.ServeFile(const LocalFileName: string);
begin
 FreeAndNil({FContentStream}FPostStream);
 //
 if FileExists(LocalFileName) then
 begin
  //LastModified:=GetFileDateUtc(LocalFileName); // LastModified property is in LOCAL time, converting to UTC!
  Headers[DatePropNames[1]{'Last-Modified'}] := FormatHttpDate(GetFileDateUtc(LocalFileName), False);
  //
  //FreeAndNil(FContentStream);
  {ContentStream}PostStream := TFileStream.Create(LocalFileName, fmOpenRead or fmShareDenyWrite);
  //
  ContentType := DetectContentType(LocalFileName);
  //
  StatusCode  := 200; // OK
  //
 end else
 begin
  // File not found:
  StatusCode := 404; // Not Found
  // Give some message:
  if (Error404Url <> '') then
   Redirect(Error404Url)
  else
  if (Error404DocText <> '') then
  begin
   Content := Error404DocText;
   ContentType := 'text/html';
  end else
  begin
   // Fallback:
   Content := '404 - not found.';
   ContentType := 'text/plain';
  end;
 end;
end;

function DetectContentType(const FileName: string): string;
var
 Ext: string;
begin
 // By file extension:
 Ext := ExtractFileExt(FileName);
 Result := GetContentTypeByExt(Ext);
 //if (Result <> '') then
 // Exit;
 //
 // Auto-detect by contents?
 // Not here...
 //Result := '';
end;

var
 ContentTypes: TStringList;

procedure RegisterContentType(const Ext, ContentType: string);
var
 S, Prev: string;
 Index: integer;
begin
 S := Ext + '=' + ContentType;
 if (S[1] = '=') then
  exit;
 if (S[1] <> '.') then
  Insert('.', S, 1);
 //
 //
 Index := 0;
 ContentTypes.Find(S, Index);
 if (Index < ContentTypes.Count) then
 begin
  Prev := ContentTypes[Index];
  if SameText(FetchToken(Prev, '=', True), Ext) then
  begin
   ContentTypes[Index] := S;
   S := '';//exit;
  end;
 end;
 if (S <> '') then
  ContentTypes.Add(S);
end;

procedure RegisterInternalContentTypes;
begin
 // register some basic content types...
 // other get registered from configuration or from registry:
 RegisterContentType('.htm', 'text/html');
 RegisterContentType('.html', 'text/html');
 RegisterContentType('.xml', 'text/xml');
 RegisterContentType('.json', 'application/json');
 RegisterContentType('.txt', 'text/plain');
 RegisterContentType('.jpg', 'image/jpeg');
 RegisterContentType('.gif', 'image/gif');
 RegisterContentType('.png', 'image/png');
 RegisterContentType('.css', 'text/css');
 RegisterContentType('.ico', 'image/x-icon');
 RegisterContentType('.bmp', 'image/bmp');
 RegisterContentType('.htc', 'text/x-component');
 RegisterContentType('.js', 'text/javascript');
end;

function GetContentTypeByExt(const Ext: string): string;
var
 Index: integer;
begin
 Result := '';
 ContentTypes.Find(Ext + '=', Index);
 if (Index < ContentTypes.Count) then
 begin
  Result := ContentTypes[Index];
  if not SameText(FetchToken(Result, '=', True), Ext) then
   Result := '';
 end;
end;

{$ifdef MSWINDOWS}
 // Win32 specific...
 //[HKEY_CLASSES_ROOT\.xsl]
 //@="xslfile"
 //"Content Type"="text/xml"

procedure RegisterContentTypesFromRegistry;
var
 Key, SubKey: HKEY;
 KeyIndex: integer;
 CbName, CbData: DWORD;
 KeyName, Value: string;
begin
 KeyIndex := 0;
 Key := HKEY_CLASSES_ROOT;
 CbName := 128;//x080922: 16;
 SetLength(KeyName, CbName);
 while (RegEnumKeyEx(Key, KeyIndex, @KeyName[1], CbName, nil, nil, nil, nil) = 0) do
 begin
  SetLength(KeyName, CbName);
  if (KeyName <> '') and (KeyName[1] = '.') and (RegOpenKeyEx(Key, PChar(KeyName), 0, KEY_READ, SubKey) = 0) then
  begin
   CbData := 64;
   SetLength(Value, CbData + 8);
   if (RegQueryValueEx(SubKey, 'Content Type', nil, nil, @Value[1], @CbData) = 0) and (Value <> '') then
   begin
    SetLength(Value, CbData);
    RegisterContentType(KeyName, Value);
   end;
   RegCloseKey(SubKey);
  end;
  //
  Inc(KeyIndex);
  CbName := 128;//x080922: 16;
  SetLength(KeyName, CbName);
 end;
end;

{$endif MSWINDOWS}

procedure THttpRequest.Redirect(const aUrl: string);
begin
 //StatusCode := 302; // there are other 30x codes, but some HTTP/1.0 browsers do not understand them and understand only 302...
 Self.Location := aUrl;
end;

// parse:  'GET /index.html HTTP/1.1'  // used by server
procedure THttpRequest.ParseFirstRequestLine(Line: string);
var
 p: integer;
begin
 Self.FMethod := FetchToken(Line, ' ', True); // this trims Command...
 p := Length(Line) - 7;
 if (p > 0) and SameText(Copy(Line, p, 4), 'HTTP') then
 begin
  Self.FProtocol := Copy(Line, p, 8);
  Self.FUrl := TrimCopy(Line, 1, p - 1);
 end else
 begin
  Self.FUrl := FetchToken(Line, ' ', True);
  Self.FProtocol := Line;
 end;
 p := Pos('?', Url) - 1;
 if p > 0 then
  FDocument := Copy(Url, 1, p)
 else
  FDocument := Url;
end;

// parse:  'HTTP/1.1 200 OK'           // used by client
procedure THttpRequest.ParseFirstResponseLine(Line: string);
begin
 FProtocol  := FetchToken(Line, ' ', True);
 FStatusCode := StrToIntDef(FetchToken(Line, ' ', True), 0);
 FStatusMsg := Line;
end;

// format: 'HTTP/1.1 200 OK'           // used by server
function THttpRequest.GetFirstResponseLine: string;
begin
 if (FProtocol = '') then
  FProtocol := 'HTTP/1.0';
 if (FStatusCode = 0) then
  StatusCode := 500; // Internal server error - did not set StatusCode...?
 //
 Result := Format('%s %d %s', [FProtocol, StatusCode, StatusMsg]);
end;

// format: 'GET /index.html HTTP/1.1'  // used by client
function THttpRequest.GetFirstRequestLine: string;
begin
 if (FMethod = '') then
  FMethod := 'GET';
 if (FUrl = '') then
  FUrl := '/';
 if (FProtocol = '') then
  FProtocol := 'HTTP/1.0';
 //
 Result := Format('%s %s %s', [FMethod, FUrl, FProtocol]);
end;

function THttpRequest.MatchTag(Etags: string): boolean;
var
 E: string;
begin
 Result := False;
 // If-Match header may specify more tags, comma-separated...
 while (Etags <> '') do
 begin
  E := FetchQSepValue(Etags, ',');
  if (E = '') then
   continue;
  if (E = '*') then
  begin
   Result := (Self.Etag <> '');
   break;//exit;
  end;
  if (E = Self.Etag) then
  begin
   Result := True;
   break;//exit;
  end;
 end;
end;

{ THttpCookies }

constructor THttpCookies.Create;
begin
 inherited Create(THttpCookie);
end;

function THttpCookies.GetCommaText: string;
var
 i: integer;
begin
 Result := '';
 for i := 0 to Count - 1 do
  Result := Result + Cookies[i].Name + '=' + Cookies[i].Value + ',';
 System.Delete(Result, High(Result), 1);
end;

function THttpCookies.GetCookieItem(Index: integer): THttpCookie;
begin
 Result := THttpCookie(inherited Items[Index]);
end;

function THttpCookies.GetValue(const Name: string): string;
var
 Cookie: THttpCookie;
begin
 Cookie := Find(Name);
 if Assigned(Cookie) then
  Result := Cookie.Value
 else
  Result := '';
end;

function THttpCookies.IndexOf(const Name: string): integer;
var
 i: integer;
begin
 for i := 0 to Count - 1 do
  if SameText(Cookies[i].Name, Name) then
  begin
   Result := i;
   exit;
  end;
 Result := -1;
end;

function THttpCookies.Find(const Name: string): THttpCookie;
var
 Index: integer;
begin
 Index := IndexOf(Name);
 if (Index >= 0) then
  Result := Cookies[Index]
 else
  Result := nil;
end;

procedure THttpCookies.LoadClientCookies(Headers: THeaderList);
begin
 Clear;
 Headers.EnumHeaders('Cookie', AddCookieValue, ';', 1);
 //x: Headers.EnumHeaders('Cookie2',AddCookieValue,2);
end;

procedure THttpCookies.SaveServerCookies(Headers: THeaderList; const DefaultDomain, DefaultPath: string);
var
 i: integer;
 Cookie: THttpCookie;
begin
 Headers.RemoveValue('Set-Cookie');
 for i := 0 to Count - 1 do
 begin
  Cookie := Cookies[i];
  if (Cookie.Domain = '') then
   Cookie.Domain := Copy(DefaultDomain, 1, Pos(':', DefaultDomain) - 1);
  if (Cookie.Path = '') then
   Cookie.Path := DefaultPath;
  Headers.AddValue('Set-Cookie', Cookie.GetServerCookie);
 end;
end;

procedure THttpCookies.SetDefaultPath;
var
 i: integer;
begin
 for i := 0 to Count - 1 do
  if Cookies[i].Path.IsEmpty then
   Cookies[i].Path := '/';
end;

procedure THttpCookies.SetSameSite;
var
 i: integer;
begin
 for i := 0 to Count - 1 do
 begin
  Cookies[i].Secure := True;
  Cookies[i].SameSite := True;
 end;
end;

procedure THttpCookies.SetValue(const Name, Value: string);
var
 Cookie: THttpCookie;
begin
 Cookie := Find(Name);
 if Assigned(Cookie) then
  Cookie.Value := Value
 else
 begin
  Cookie := THttpCookie(Add);
  Cookie.Name := Name;
  Cookie.Value := Value;
 end;
end;

procedure THttpCookies.LoadServerCookies(Headers: THeaderList);
begin
 Clear;
 Headers.EnumHeaders('Set-Cookie', AddCookieValue, ',', 1);
 Headers.EnumHeaders('Set-Cookie2', AddCookieValue, ',', 2);
end;

procedure THttpCookies.SaveClientCookies(Headers: THeaderList; const Path: string);
var
 i: integer;
 Cookie: THttpCookie;
begin
 Headers.RemoveValue('Cookie');
 for i := 0 to Count - 1 do
 begin
  Cookie := Cookies[i];
  if (Path = '') or Cookie.MatchPath(Path) then
   Headers.AddValue('Cookie', Cookie.GetClientCookie);
 end;
end;

function THttpCookies.AddCookieValue(const Value: string; LParam: NativeUInt): boolean;
var
 Cookie: THttpCookie;
begin
 Cookie := THttpCookie.Create(nil);
 if Cookie.ParseValue(Value, LParam) then
  Cookie.Collection := Self
 else
  Cookie.Free;
 //
 Result := False; // all...
end;

procedure THttpCookies.MergeCookies(Source: THttpCookies);
var
 i: integer;
 Src, Dst: THttpCookie;
begin
 for i := 0 to Source.Count - 1 do
 begin
  Src := Source[i];
  Dst := Self.Find(Src.Name);
  if (Dst = nil) then
   Dst := THttpCookie.Create(nil);
  Dst.Assign(Src);
  Dst.Collection := Self;
 end;
end;

{ THttpCookie }

procedure THttpCookie.Assign(Source: TPersistent);
begin
 if (Source is THttpCookie) then
 begin
  FName  := THttpCookie(Source).FName;
  FValue := THttpCookie(Source).FValue;
  FDomain := THttpCookie(Source).FDomain;
  FPath  := THttpCookie(Source).FPath;
  FExpires := THttpCookie(Source).FExpires;
  FSecure := THttpCookie(Source).FSecure;
  FMaxAge := THttpCookie(Source).FMaxAge;
 end else
  inherited;
end;

procedure THttpCookie.DeleteCookie;
begin
 // RFC2109:
 //Optional.  The Max-Age attribute defines the lifetime of the
 //cookie, in seconds.  The delta-seconds value is a decimal non-
 //negative integer.  After delta-seconds seconds elapse, the client
 //should discard the cookie.  A value of zero means the cookie
 //should be discarded immediately.
 FMaxAge := '0';
end;

function QuoteValue(const Value: string): string;
var
 p, len: integer;
begin
 Result := Value;
 //
 len := Length(Result);
 p := 1;
 while (p <= len) do
 begin
  case Result[p] of
   '"', '\':
   begin
    Insert('\', Result, p);
    Inc(p);
    Inc(len);
   end;
  end;
  Inc(p);
 end;
 //
 Result := '"' + Result + '"';
end;

function NeedsCookieValueQuoting(const S: string): boolean;
var
 p: integer;
begin
 if (S = '') then
 begin
  Result := True;
  exit;
 end;
 //
 p := Length(S);
 while (p > 0) do
 begin
  case S[p] of
   '"', '=', ';', ',', #1..' ':
   begin
    Result := True;
    exit;
   end;
  end;
  Dec(p);
 end;
 Result := False;
end;

function AddCookieProp(const Cookie, Name, Value: string; bnQuoted: boolean): string;
var
 Sep, QVal: string;
begin
 Result := Cookie;
 if (Value <> '') then
 begin
  Sep := '';
  if (Result <> '') then
   Sep := '; ';
  //
  QVal := Value;
  // values may be quoted, but do not need to be quoted...
  if bnQuoted and NeedsCookieValueQuoting(Value) then
   QVal := QuoteValue(Value);
  //
  Result := Result + Sep + Name + '=' + QVal;
 end;
end;

function THttpCookie.GetServerCookie: string; // Set-Cookie: format... (for sending server->client)
begin
 Result := AddCookieProp('', FName, FValue, True);
 Result := AddCookieProp(Result, 'Version', FVersion, True);
 Result := AddCookieProp(Result, 'Path', FPath, True);
 Result := AddCookieProp(Result, 'Domain', FDomain, True);
 Result := AddCookieProp(Result, 'Max-Age', FMaxAge, True);
 Result := AddCookieProp(Result, 'Comment', FComment, True);
 if FSameSite then
  Result := AddCookieProp(Result, 'SameSite', 'none', False);
 //
 //Expires= is in this format:  Wdy, DD-Mon-YY HH:MM:SS GMT
 //in Netscape format, also must not use quotes or spaces elsewhere than in Expires...
 //
 if FSecure then
  Result := Result + '; secure';
 if FValue = '' then
  Result := FName + '=;' + Result;
end;

function THttpCookie.GetText: string;
begin
 Result := FName + '=' + ConvertUrlChars(FValue.Replace('\', ''));
end;

function THttpCookie.GetClientCookie: string; // Cookie:     format... (for sending client->server)
begin
 if (Version <> '') then
  Result :=
   AddCookieProp(AddCookieProp(AddCookieProp(AddCookieProp('', '$Version', Version, True), FName, FValue, True),
   '$Path', FPath, True), '$Domain', FDomain, True)// RFC2109 format... should have Version='1'
 // Cookie: $Version="1"; Name="Value"; $Path="Path", $Domain="Domain"
 else
  Result := AddCookieProp('', FName, FValue, False)// Simple Netscape format, just Name=Value, no quoting
 //Result:=FName+'='+FValue;
 ;
end;

function THttpCookie.ParseValue(Line: string; Version: NativeUInt): boolean;
var
 Value, Name: string;
 bnFirst, bnSpecial: boolean;
begin
 bnFirst := True;
 while (Line <> '') do
 begin
  Value := FetchQSepValue(Line, ';');
  if (Value <> '') then
  begin
   Name := FetchToken(Value, '=', True);
   //
   if (Name <> '') and (Name[1] = '$') then
   begin
    bnSpecial := True;
    Delete(Name, 1, 1);
   end else
    bnSpecial := False;
   //
   if bnFirst and not bnSpecial then
   begin
    FName  := Name;
    FValue := Value;
    bnFirst := False;
   end else if SameText(Name, 'path') then // do not localize...
    FPath := Value
   else
   if SameText(Name, 'expires') then
    FExpires := Value
   else
   if SameText(Name, 'domain') then
    FDomain := Value
   else
   if SameText(Name, 'secure') then
    FSecure := True
   else
   if SameText(Name, 'version') then
    FVersion := Value// other values:
   ;
  end;
 end;
 Result := not bnFirst;
end;

function THttpCookie.MatchPath(const aPath: string): boolean;
var
 Len: integer;
begin
 Len := Length(Self.Path);
 //
 if (Length(aPath) >= Len) and SameHead(aPath, Self.Path) //and SameText(Copy(aPath,1,Len),Self.Path)
 then
  Result := True
 else
  Result := False;
end;

{ TSynHttpServer }

constructor TSynHttpServer.Create(AOwner: TComponent);
begin
 inherited;
 Port := '80';
 //
 //FConnClass:=TSynTcpSrvConnection; // we are using generic connection class...
 //
 if not (csDesigning in ComponentState) then
  OnCommand := HandleClientCommand;
end;

procedure TSynHttpServer.SetActive(Value: boolean);
begin
  {$ifdef DEBUG}
  if (Value=Self.Active) then
    exit;
  if Value then
    Debug('%s http server on port %s',['Starting',Port])
  else
    Debug('%s http server on port %s',['Stoping',Port]);
  {$endif DEBUG}
 //
 inherited;
 //
  {$ifdef DEBUG}
  Debug('Done.');
  {$endif DEBUG}
end;

(*function GetStreamSize(Stream: TStream): int64;
var
 Pos: int64;
begin
 Pos := Stream.Position;
 Result := Stream.Size;
 //
  {$ifdef MSWINDOWS}
  // Workarround for Delphi 5, where stream does not return Int64...
  if (Stream is TFileStream) then begin
    LARGE_INTEGER(Pos).HighPart:=0;
    LARGE_INTEGER(Pos).LowPart:=SetFilePointer(TFileStream(Stream).Handle,0,@LARGE_INTEGER(Pos).HighPart,FILE_CURRENT);
    LARGE_INTEGER(Result).HighPart:=0;
    LARGE_INTEGER(Result).LowPart:=SetFilePointer(TFileStream(Stream).Handle,0,@LARGE_INTEGER(Result).HighPart,FILE_END);
    //
    SetFilePointer(TFileStream(Stream).Handle,LARGE_INTEGER(Pos).LowPart,@LARGE_INTEGER(Pos).HighPart,FILE_BEGIN);
  end;
  {$endif}
 //
 Result := Result - Pos;
end;

procedure StreamSeek(Stream: TStream; Offset: int64);
var
 This: longint;
begin
 // Workarround for Delphi 5, where TStream cannot seek by Int64...
 while (Offset > 0) do
 begin
  if (Offset < $20000000) then
   This := Offset
  else
   This := $20000000;
  Dec(Offset, This);
  Stream.Seek(This, soFromCurrent);
 end;
end;*)

function ParseRangeRequest(S: string; var RangeStart, RangeLength: int64; const ContentSize: int64;
 bnSizeKnown: boolean): boolean;
var
 p: integer;
 S1, S2: string;
 RangeEnd: int64;
begin
 Result := False;
 // bytes=0-1000
 // bytes=1000-
 // bytes=-1000
 // bytes=0-1000,2000-3000   this form is not parsed here and is ignored... this way we can avoid sending multipart/byte-ranges response...
 //
 if SameHead(S, 'bytes') //if SameText(Copy(S,1,5),'bytes')
 then
 begin
  Delete(S, 1, 5);
  DoTrim(S); // can have space: bytes = ...
  if (S <> '') and (S[1] = '=') then
  begin
   Delete(S, 1, 1);
   DoTrim(S);
  end;
  //
  p := Pos('-', S);
  if (p = 0) then
   exit;
  //
  S1 := TrimCopy(S, 1, p - 1);
  S2 := TrimCopy(S, p + 1, 63);
  //
  RangeStart := StrToInt64Def(S1, -1);
  RangeEnd := StrToInt64Def(S2, -1);
  //
  if (S1 = '') then
  begin
   if (S2 = '') or not bnSizeKnown or (RangeEnd < 0) then
    exit;
   // bytes=-tailsize
   RangeStart := ContentSize - RangeEnd;
   RangeLength := RangeEnd;
   Result := True;
  end else
  if (S2 = '') then
  begin
   // bytes=startpos-
   if (RangeStart < 0) or not bnSizeKnown then
    exit;
   RangeLength := ContentSize - RangeStart;
   Result := True;
  end else
  if (RangeStart >= 0) and (RangeEnd >= 0) then
  begin
   // bytes=startpos-endpos
   RangeLength := RangeEnd - RangeStart + 1;
   Result := True;
  end;
 end;
end;

// this function is the body of http request handling:
procedure TSynHttpServer.HandleClientCommand(Connection: TSynTcpSrvConnection; Command: string);
var
 Request, Reply: THttpRequest;
begin
 // Command is first line of request:   GET /index.html HTTP/1.1
 Request := THttpRequest.Create;
 Reply := THttpRequest.Create;
 try
  ReadRequest(Connection, Request, Reply, Command);
  DoHttpGet(Connection, Request, Reply);
  //-------------------------------------------------------------------------
  // Pass to application:
  if (Reply = nil) then
   Exit;// There is a chance for application to send reply, free it and give us NIL instead, to prevent further processing...
  //-------------------------------------------------------------------------
  SendReply(Connection, Request, Reply);
  //
 finally
  Reply.Free;
  Request.Free;
 end;
end;

procedure TSynHttpServer.CreatePostStream(Request: THttpRequest);
begin
 if Assigned(OnCreatePostStream) then
  OnCreatePostStream(Self, Request, Request.FPostStream);
end;

procedure TSynHttpServer.ReadRequest(Connection: TSynTcpSrvConnection; Request, Reply: THttpRequest; Command: string);
var
 bnContinue, bnHttp11: boolean;
 S: string;

 function PreparePostStream: boolean;
 var
  i, Size: integer;
 begin
  Result := False;
  if (Request.TransferEncoding <> '') and (not SameText(Request.TransferEncoding, 'identity')) then
  begin
   if Pos('chunked', LowerCase(Request.TransferEncoding)) = 0 then
   begin
    Reply.StatusCode := 400; // bad request
    //Reply.WriteHeader;
    Connection.Terminate;
    Exit;
   end;
   CreatePostStream(Request);
   if Request.FPostStream = nil then
    Request.FPostStream := TMemoryStream.Create;
   Request.PostStream.Position := 0;
   repeat
    S := string(Connection.Socket.RecvString(cDefLineTimeout));
    if (Connection.Socket.LastError <> 0) then
     Exit;
    i := Pos(';', S); {do not localize}
    if i > 0 then
     S := Copy(S, 1, i - 1);
    Size := StrToIntDef('$' + Trim(S), 0);      {do not localize}
    if Size = 0 then
     Break;
    Connection.Socket.RecvStreamSize(Request.PostStream, cDefLineTimeout, Size);
    Connection.Socket.RecvString(cDefLineTimeout); // CRLF at end of chunk data
   until False;
   // skip trailer headers
   repeat
   until Connection.Socket.RecvString(cDefLineTimeout) = '';
   Request.PostStream.Position := 0;
  end
  else if Request.ContentLength <> '' then
  begin
   CreatePostStream(Request);
   if Request.FPostStream = nil then
    Request.FPostStream := TMemoryStream.Create;
   Request.PostStream.Position := 0;
   if Request.ContentLength > '0' then
   begin
    Size := StrToIntDef(Request.ContentLength, 0);
    Connection.Socket.RecvStreamSize(Request.PostStream, cDefLineTimeout, Size);
    Request.PostStream.Position := 0;
   end;
  end
  // If HTTP Pipelining is used by the client, bytes may exist that belong to
  // the NEXT request!  We need to look at the CURRENT request and only check
  // for misreported body data if a body is actually expected.  GET and HEAD
  // requests do not have bodies...
  else if SameText(Request.Method, 'POST') or SameText(Request.Method, 'PUT') then
  begin
   // TODO: need to handle the case where the ContentType is 'multipart/...',
   // which is self-terminating and does not strictly require the above headers...
   if Connection.Socket.LineBuffer = '' then
    Connection.Socket.CanReadEx(cDefLineTimeout);
   if Connection.Socket.LineBuffer <> '' then
   begin
    Reply.StatusCode := 411; // length required
    Connection.Terminate;
    Exit;
   end;
  end;
  Result := True;
 end;

begin
 //
 // Connect objects:
 Request.FConnection := Connection;
 Reply.FConnection := Connection;
 //
 // Parse first line:
  {$ifdef DEBUG} Debug('Command:'#13#10'%s',[Command]); {$endif}
 Request.ParseFirstRequestLine(Command);
 //
 // Read rest of headers:
 if not ReadHeadersFromSocket(Connection.Socket, Request.Headers,
  {Connection.Socket.GetRecvTimeout}cDefLineTimeout) then
 begin
  Connection.Terminate;
  Exit;
 end;
 Request.ApplyHeaders(True);
 //
 if (Request.Protocol >= 'HTTP/1.1') and SameHead(Request.Protocol, 'HTTP')
 //and SameText(Copy(Request.Protocol,1,4),'HTTP')
 then
 begin
  bnHttp11 := True;
  Reply.FProtocol := 'HTTP/1.1'; // we are compliant...
  //
  S := Request.Headers['Expect'];
  if (S <> '') then
  begin
   // RFC2616:
   //A server that does not understand or is unable to comply with any of
   //the expectation values in the Expect field of a request MUST respond
   //with appropriate error status. The server MUST respond with a 417
   //(Expectation Failed) status if any of the expectations cannot be met
   //or, if there are other problems with the request, some other 4xx
   //status.
   bnContinue := SameText(S, '100-continue'); // we understand only this Expect value...
   if Assigned(FOnExpect) then
    FOnExpect(Self, Request, bnContinue);
   //
   if bnContinue then
   begin
    Reply.StatusCode := 100; // 100 continue
    Connection.Socket.SendString(UTF8Encode(Reply.GetFirstResponseLine + #13#10#13#10));
   end else
   begin
    // RFC2616:
    //If it responds with a final status
    //code, it MAY close the transport connection
    Reply.StatusCode := 417; // Expectation failed
    Connection.Socket.SendString(UTF8Encode(Reply.GetFirstResponseLine + #13#10#13#10));
    Connection.Terminate;
    Exit;
   end;
  end;
 end else
 if (Request.Protocol = 'HTTP/1.0') then
 begin
  Reply.FProtocol := 'HTTP/1.0';
  bnHttp11 := False;
 end else
 begin
  // Do not serve just any non-sense, written to our port...
  // Chance for getting HTTP/0.9 request is very small,
  // but chance for getting for ex. SMTP communication into the server port is much better...
  Connection.Terminate;
  Exit;
 end;
 //
 // Read body:
 if not PreparePostStream then
  Exit;
 if Assigned(Request.PostStream) and SameText(Request.Method, 'POST') then
 begin
  S := Request.ContentType;
  if S.StartsWith('application/x-www-form-urlencoded', True) {or S.StartsWith('multipart/form-data', True)} then
   with TStringStream.Create do
   begin
    CopyFrom(Request.PostStream, Request.PostStream.Size);
    Request.FContent := DataString;
    Free;
   end;
 end;
 //
 // Set some defaults:
 Reply.StatusCode := 404; // default to Not-found...
 if bnHttp11 then
 begin
  // HTTP/1.1 clients should default to keep-alive (rfc2616):
  if not Request.Headers.HasValue('Connection', 'close') then
   Reply.Headers['Connection'] := 'keep-alive'
  else
   Reply.Headers['Connection'] := 'close';
 end else if Request.Headers.HasValue('Connection', 'keep-alive') then
  Reply.Headers['Connection'] := 'keep-alive'
 else
  Reply.Headers['Connection'] := 'close'// HTTP/1.0 clients should default to close (rfc2616):
 ;
 //
 // Cookies:
 //??? Reply.Cookies.Assign(Request.Cookies);
 //
 // POST parameters:
 if Request.FContent <> '' then
  Request.ParsePostFormData;
 //
 Reply.Headers['Server'] := ServerValue;
end;

procedure TSynHttpServer.DoHttpGet(Connection: TSynTcpSrvConnection; Request, Reply: THttpRequest);
begin
 if Assigned(FOnHttpGet) then
  FOnHttpGet(Self, Connection, Request, Reply);
end;

function IsWithin(Value, Min, Max: integer): boolean;
begin
 Result := (Value >= Min) and (Value <= Max);
end;

function ExtractUrlPath(const Url: string): string;
var
 p: integer;
 bnFound: boolean;
begin
 Result := Url;
 p := Pos('://', Result);
 if (p > 0) then
 begin
  Delete(Result, 1, p + 2); // remove http://
  p := Pos('/', Result);
  if (p > 0) then
   Delete(Result, 1, p); // remove hostname
 end;
 //
 p := Pos('?', Result);
 if (p = 0) then
  p := Length(Result) + 1;
 bnFound := False;
 while (p > 1) do
 begin
  Dec(p);
  if (Result[p] = '/') then
  begin
   SetLength(Result, p - 1);
   bnFound := True;
   break;
  end;
 end;
 //
 if not bnFound or (Result = '') then
  Result := '/';
end;

procedure TSynHttpServer.SendReply(Connection: TSynTcpSrvConnection; Request, Reply: THttpRequest);
var
 bnBody, bnSize: boolean;
 S: string;
 Size, RangeStart, RangeLength: int64;
 Date, Date2: TDateTime;

 function AlwaysUpdate(const Url: string): boolean;
 begin
  Result := (Url = '/') or (Url.ToLower.Contains('.html'));
 end;

begin
 if Reply.ResponseSent then
  Exit;
 // Adjust Reply:
 //
 // Cookies:
 Reply.Cookies.SetDefaultPath;
 if FHTTPSEnabled then
  Reply.Cookies.SetSameSite;
 Reply.Cookies.SaveServerCookies(Reply.Headers, Request.Host, ExtractUrlPath(Request.Url));
 //
 // Fill other values:
 if (Reply.Headers['Date'] = '') then
  Reply.Headers['Date'] := FormatHttpDate(Now, True);
 //
 // Content-Length and Transfer-Encoding:
 if Reply.SendChunked then
 begin
  Reply.ContentLength := '';
  Reply.TransferEncoding := 'chunked';
  Size := -1;
  bnSize := False;
 end else
 begin
  S := Reply.ContentLength;
  if (S = '') then
  begin
   // Fill Content-Length:
   if (Reply.PostStream <> nil) then
   begin
    //Size:=Reply.ContentStream.Size;
    Size := Reply.PostStream.Size;
    bnSize := True;
   end else
   if (Reply.Content <> '') then
   begin
    Size := Length(UTF8Encode(Reply.Content));
    bnSize := True;
   end else
   begin
    Size := 0;
    bnSize := False;
   end;
   //
   Reply.ContentLength := IntToStr(Size);
   //
  end else
  begin
   // Content-Length was filled by application:
   Size := StrToInt64Def(S, -1);
   bnSize := (Size >= 0);
  end;
 end;
 //
 //? if (Reply.StatusCode=404) then Reply.Headers['Connection']:='close';
 //
 if IsWithin(Reply.StatusCode, 200, 299) and not AlwaysUpdate(Request.Url) then
 begin
  //
  // Check If-Modified-Since:
  S := Request.Headers['If-Modified-Since'];
  if (S <> '') and ParseHttpDate(S, Date) then
  begin
      {$ifdef DEBUG}
      Debug('If-Modified-Since: %s',[S]);
      Debug('Last-Modified: %s',[Reply.Headers['Last-Modified']]);
      {$endif DEBUG}
   //
   Date2 := Reply.LastModifiedUtc;
   if (Date2 <> 0) and (Date2 > Date) then // is modified...
{$ifdef DEBUG}
{$endif DEBUG}
   else
   begin
    // Is not modified...
    Reply.StatusCode := 304; // Not Modified
    //!!!TODO/bug
    // mozilla hangs in transfer, when it gets the 304 responses??
    //if (Copy(Request.Headers['User-Agent'], 1, 7) = 'Mozilla') then
    // Reply.Headers['Connection'] := 'close';
   end;
  end else
  begin
      {$ifdef DEBUG}
      if (S<>'') then
        Debug('Failed parse date "%s"',[S]);
      {$endif DEBUG}
   //
   S := Request.Headers['If-Unmodified-Since'];
   if (S <> '') and ParseHttpDate(S, Date) then
   begin
    Date2 := Reply.LastModifiedUtc;
    if (Date2 <> 0) and (Date2 > Date) then
     Reply.StatusCode := 412// is modified
    // Precondition Failed
    ;
   end;
  end;
 end;
 //
 if IsWithin(Reply.StatusCode, 200, 299) then
 begin
  //
  // Check If-Range - if the condition fails, we will ignore Range: header...
  S := Request.Headers['If-Range'];
  if (S <> '') then
   if (CharInSet(S[1], ['w', 'W'])) and (S[2] = '/') // W/"tag"
    or (S[1] = '"')                // "tag"
   then
   begin
    if not Request.Headers.HasValue('Etag', S) then
     Request.Headers['Range'] := ''; // does not have this Etag...
   end else if ParseHttpDate(S, Date) then
   begin
    Date2 := Reply.LastModifiedUtc;
    if (Date2 = 0) or (Date2 <= Date) then // is not modified since...
    else
    begin
     // was modified since...
     Request.Headers['Range'] := ''; // will send whole...
    end;
   end else
    Request.Headers['Range'] := ''// Http-date:  like If-Unmodified-Since...
  // we do not understand If-Range header, so we will send whole body...
  // If-Range = "If-Range" ":" ( entity-tag | HTTP-date )
  ;
  //
  // Check Range: header
  RangeStart := 0;
  RangeLength := 0;
  S := Request.Headers['Range'];
  if (S <> '') and ParseRangeRequest(S, RangeStart, RangeLength, Size, bnSize) then
   if (bnSize and (RangeStart >= Size)) or (RangeLength <= 0) then
   begin
    Reply.StatusCode := 416; // Requested Range Not Satisfiable
    Reply.ContentLength := '';
    if bnSize then
     Reply.Headers['Content-Range'] := Format('*/%d', [Size]); // we SHOULD send this with 416 code...
    Size := 0; // do not send body... //we will not send body, filtered also below...
   end else
   begin
    // Valid range:
    if bnSize then
     S := IntToStr(Size)
    else
     S := '*';
    Reply.StatusCode := 206; // Partial Content
    Reply.Headers['Content-Range'] :=
     Format('bytes %d-%d/%s', [RangeStart, RangeStart + RangeLength - 1, S]);
    if bnSize then
     Reply.ContentLength := IntToStr(RangeLength);
    //
    if (RangeStart <> 0) then
     if (Reply.PostStream <> nil) then
      Reply.PostStream.Seek(RangeStart, soCurrent)
     else
     if (Reply.Content <> '') then
      Delete(Reply.FContent, 1, RangeStart);
    //
    if (RangeLength <> 0) then
    begin
     Size := RangeLength;
     if (Reply.PostStream = nil) and (Reply.Content <> '') and (Size < Length(Reply.Content)) then
      SetLength(Reply.FContent, Size);
    end;
   end//
  ;
 end;
 //
 if IsWithin(Reply.StatusCode, 200, 299) then
 begin
  // Check Etag headers (If-Match, If-None-Match)
  S := Request.Headers['If-Match'];
  if (S <> '') then
   if not Reply.MatchTag(S) then
    Reply.StatusCode := 412// Precondition Failed
  // reply may have more tags, comma-separated, some week...
  // also If-Match may specify more tags...
  ;
  S := Request.Headers['If-None-Match'];
  if (S <> '') then
   if Reply.MatchTag(S) then
    Reply.StatusCode := 412// Precondition Failed
  ;
 end;
 //
 //-------------------------------------------------------------------------
 // Write reply to client:
 S := Reply.GetFirstResponseLine + #13#10 + Reply.Headers.Text + #13#10; // include 1 empty line after headers...
  {$ifdef DEBUG}Debug('Response headers:'#13#10'%s',[S]);{$endif}
 Connection.Socket.SendString(UTF8Encode(S));
 Reply.ResponseSent := True;
 if (Connection.Socket.LastError <> 0) then
 begin
  Connection.Terminate;
  Exit;
 end;
 //
 bnBody := True;
 if SameText(Request.Method, 'HEAD') then
  bnBody := False // MUST NOT send entity body with HEAD, but should send Content-Length...
 else
  case Reply.StatusCode of
   412, // this is not in RFC, but we will not send entity body with 412 precondition failed anyway...
   416, // this is not in RFC, but we will not send entity body with 416 code (Requested Range Not Satisfiable) anyway...
   100..199, 204, 304:
   begin
    bnBody := False; // we MUST NOT send entity body with these status-codes...
    // Do not send Content-Length and Content-Type fields
    Reply.Headers['Content-Length'] := '';
    Reply.Headers['Content-Type'] := '';
   end;
  end;
 //
 if bnBody then
 begin
  // Send body:
  if (Reply.PostStream <> nil) then
   SendSocketStream(Connection.Socket, Reply.PostStream, Size, Reply.SendChunked)
  //x: we cannot use this, since it uses Stream.Size: Connection.Socket.SendStreamRaw(Reply.ContentStream);
  else
  if (Reply.Content <> '') then
   if not Reply.SendChunked then
    Connection.Socket.SendString(UTF8Encode(Reply.Content))
   else
   begin
    // Send 1 chunk:
    Connection.Socket.SendString(UTF8Encode(Format('%x'#13#10, [Length(Reply.Content)])));
    if (Connection.Socket.LastError = 0) then
     Connection.Socket.SendString(UTF8Encode(Reply.Content));
    if (Connection.Socket.LastError = 0) then
     Connection.Socket.SendString('0'#13#10#13#10);
   end;
  //
  if (Connection.Socket.LastError <> 0) then
  begin
   Connection.Terminate;
   exit;
  end;
 end;
 //
 if Reply.Headers.HasValue('Connection', 'close') then
  Connection.Terminate;
end;

procedure TSynHttpServer.InitHttps(const CertFile, KeyFile, KeyPassword, CaCertFile: string);
begin
 if not FileExists(CertFile) or not FileExists(KeyFile) then
  Exit;
 FCertFile := CertFile;
 FKeyFile  := KeyFile;
 FKeyPass  := KeyPassword;
 FCaCertFile := CaCertFile;
 //FSynapseServer.Socket.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle.pem';
 FSynapseServer.Socket.SSL.CertificateFile := FCertFile;
 FSynapseServer.Socket.SSL.PrivateKeyFile := FKeyFile;
 FSynapseServer.Socket.SSL.KeyPassword := FKeyPass;
 FSynapseServer.Socket.SSL.VerifyCert := True;
 //
end;

initialization
 ContentTypes := TStringList.Create;
 ContentTypes.Sorted := True;
 RegisterInternalContentTypes;

finalization
 FreeAndNil(ContentTypes);
end.
