{--------------------------------------------------------------}
{                                                              }
{  SynSrv.pas - generic TCP server over Synapse library        }
{                                                              }
{  Author:     Semi                                            }
{  Started:    070528                                          }
{                                                              }
{--------------------------------------------------------------}
unit SynSrv;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
 SysUtils,
 Classes,
 synsock,
 blcksock,
 Generics.Collections;
//-------------------------------------------------------------

const
 // Default timeout to receive 1 line from connection:
 cDefLineTimeout = 120000; // default 2 minutes...

type
 TSynTcpSrvConnection = class;
 TSynTcpServer        = class;

 { TListenerThread }

 TListenerThread = class(TThread)
 private
  FThreadList: TObjectList<TSynTcpSrvConnection>;
  FSocket:     TTCPBlockSocket;
  FPort:       string;
  FHost:       string;
  FTcpServer:  TSynTcpServer;
  procedure ClearFinishedThreads;
  procedure BindSocket;
 protected
  procedure Execute; override;
 public
  constructor Create(ASuspended: boolean; ATcpServer: TSynTcpServer);
  destructor Destroy; override;
  property Host: string Read FHost Write FHost;
  property Port: string Read FPort Write FPort;
  property Socket: TTCPBlockSocket Read FSocket;
 end;

 TSynTcpSrvConnection = class(TThread)
 private
  FTcpServer: TSynTcpServer;
  FFinished:  boolean;
  FSocket:    TTCPBlockSocket;
  function GetClientAddress: string;
  function GetClientPort: integer;
 protected
  procedure Execute; override;
 public
  destructor Destroy; override;
  constructor Create(ASuspended: boolean; ASocket: TSocket; ATcpServer: TSynTcpServer);
  property Socket: TTCPBlockSocket Read FSocket Write FSocket; // client socket
  property ClientAddress: string Read GetClientAddress;        // '123.45.67.89'
  property ClientPort: integer Read GetClientPort;
 end;

 TCommandHandler = procedure(Connection: TSynTcpSrvConnection; Command: string) of object;

 // TSynTcpServer - Generic TCP server component
 TSynTcpServer = class(TComponent)
 protected
  FActive:        boolean;
  FPort:          string;
  FHost:          string;
  FHTTPSEnabled:  boolean;
  //
  FOnCommand:     TCommandHandler;
  //
  FSynapseServer: TListenerThread;
  procedure SetPort(const Value: string);
  procedure SetLocalAddr(const Value: string);
  procedure SetActive(Value: boolean); virtual;
 public
  constructor Create(AOwner: TComponent); override;
  //
  //
 published
  // Host may be assigned to 'localhost' to serve only on localhost interface...
  property Host: string Read FHost Write FHost;
  //
  // Port must be assigned.
  property Port: string Read FPort Write SetPort; // MUST assign port...
  //
  // Set Active:=True to start server, set Active:=False to stop server
  property Active: boolean Read FActive Write SetActive default False;
  //
  // Or assign OnCommand to parse commands (text lines) from connection:
  // (this is used by TSynHttpServer and TSynFtpServer etc...)
  property OnCommand: TCommandHandler Read FOnCommand Write FOnCommand;
  property HTTPSEnabled: boolean Read FHTTPSEnabled Write FHTTPSEnabled;
 end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

{ TSynTcpServer }

constructor TSynTcpServer.Create(AOwner: TComponent);
begin
 inherited;
 //
 FHost := '0.0.0.0';
end;

procedure TSynTcpServer.SetPort(const Value: string);
begin
 SetActive(False);
 FPort := Value;
end;

procedure TSynTcpServer.SetLocalAddr(const Value: string);
begin
 SetActive(False);
 FHost := Value;
end;

procedure TSynTcpServer.SetActive(Value: boolean);
begin
 if (csDesigning in ComponentState) then
 begin
  // No real server at design-time...
  FActive := Value;
  Exit;
 end;
 if (csLoading in ComponentState) then
  Exit;
 //
 if (FActive <> Value) then
 begin
  FActive := Value;
  if FActive then
  begin
   if (FPort = '') then
    raise ESynapseError.Create('Missing server Port');
   FSynapseServer := TListenerThread.Create(True, Self);
   FSynapseServer.Port := FPort;
   FSynapseServer.Host := FHost;
   try
    FSynapseServer.BindSocket
   except
    FreeAndNil(FSynapseServer);
    FActive := False;
    raise ESocketBindError.Create(Format('Couldnt bind socket on %s port', [FPort]));
   end;
   FSynapseServer.Start;
  end
  else
  if Assigned(FSynapseServer) then
  begin
   FSynapseServer.Terminate;
   FSynapseServer.WaitFor;
   FreeAndNil(FSynapseServer);
   //StopAllSessions;
  end;
 end;
end;

{ TListenerThread }

procedure TListenerThread.ClearFinishedThreads;
var
 i: integer;
begin
 for i := FThreadList.Count - 1 downto 0 do
  if FThreadList[i].FFinished then
   FThreadList.Remove(FThreadList[i]);
end;

procedure TListenerThread.BindSocket;
var
 e: ESynapseError;
begin
 FSocket.CreateSocket;
 FSocket.Bind(FHost, FPort);
 if FSocket.LastError = 0 then
 begin
  FSocket.EnableReuse(True);
  FSocket.Listen;
 end
 else
 begin
  e := ESynapseError.Create(Format('ListenThreadException %d: %s', [FSocket.LastError, FSocket.LastErrorDesc]));
  e.ErrorCode := FSocket.LastError;
  e.ErrorMessage := FSocket.LastErrorDesc;
  raise e;
 end;
end;

constructor TListenerThread.Create(ASuspended: boolean; ATcpServer: TSynTcpServer);
begin
 FSocket := TTCPBlockSocket.Create;
 FThreadList := TObjectList<TSynTcpSrvConnection>.Create;
 FTcpServer := ATcpServer;
 inherited Create(ASuspended);
end;

destructor TListenerThread.Destroy;
var
 i: integer;
begin
 FSocket.CloseSocket;
 for i := 0 to FThreadList.Count - 1 do
 begin
  FThreadList[i].Terminate;
  FThreadList[i].Socket.CloseSocket;
 end;
 ClearFinishedThreads;
 FreeAndNil(FThreadList);
 FreeAndNil(FSocket);
 inherited;
end;

procedure TListenerThread.Execute;
var
 SynapseConnect: TSynTcpSrvConnection;
begin
 inherited;
 repeat
  if FSocket.CanRead(100) then
  begin
   SynapseConnect := TSynTcpSrvConnection.Create(True, FSocket.Accept, FTcpServer);
   FThreadList.Add(SynapseConnect);
   SynapseConnect.Start;
  end;
  ClearFinishedThreads;
 until Terminated;
 try
  FSocket.CloseSocket;
 except
 end;
end;

{ TSynTcpSrvConnection }

constructor TSynTcpSrvConnection.Create(ASuspended: boolean; ASocket: TSocket; ATcpServer: TSynTcpServer);
begin
 inherited Create(ASuspended);
 FSocket := TTCPBlockSocket.Create;
 FSocket.Owner := Self;
 FSocket.SSL.CertificateFile:= ATcpServer.FSynapseServer.FSocket.SSL.CertificateFile;
 FSocket.SSL.PrivateKeyFile:= ATcpServer.FSynapseServer.FSocket.SSL.PrivateKeyFile;
 FSocket.SSL.KeyPassword:= ATcpServer.FSynapseServer.FSocket.SSL.KeyPassword;
 FSocket.SSL.VerifyCert:= ATcpServer.FSynapseServer.FSocket.SSL.VerifyCert;
 FTcpServer := ATcpServer;
 if ASocket <> INVALID_SOCKET then
 begin
  FSocket.Socket := ASocket;
  FSocket.GetSins;
 end;
end;

destructor TSynTcpSrvConnection.Destroy;
begin
 FSocket.CloseSocket;
 inherited;
 FreeAndNil(FSocket);
end;

procedure TSynTcpSrvConnection.Execute;
var
 Command: string;
begin
 inherited;
 if FSocket.SSL.VerifyCert then
  try
   if (not FSocket.SSLAcceptConnection) or (FSocket.SSL.LastError <> 0) then
   begin
    FFinished := True;
   end;
  except
   FFinished := True;
  end;
 if not FFinished then
 try
  while not Terminated do
  begin
   Command := string(FSocket.RecvString({FSocket.GetRecvTimeout)}cDefLineTimeout));
   // Disconnect on timeout:
   if (Command = '') and (FSocket.LastError <> 0) then
    Break;
   //
   if Assigned(FTcpServer.FOnCommand) then // could be de-assigned?
    FTcpServer.FOnCommand(Self, Command)
   else
    Break;
  end;
 finally
  FFinished := True;
 end;
end;

function TSynTcpSrvConnection.GetClientAddress: string;
begin
 Result := FSocket.GetRemoteSinIP;
end;

function TSynTcpSrvConnection.GetClientPort: integer;
begin
 Result := FSocket.GetRemoteSinPort;
end;

end.
