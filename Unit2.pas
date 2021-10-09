unit Unit2;

interface

uses
 Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
 Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynHttpSrv, Vcl.StdCtrls, SynSrv;

type
 TForm2 = class(TForm)
  Button1: TButton;
  procedure Button1Click(Sender: TObject);
 private
  FSynHttpServer: TSynHttpServer;
  { Private declarations }
  procedure SynHttpServer1HttpGet(Sender: TObject; Connection: TSynTcpSrvConnection;
   ARequestInfo, AResponseInfo: THttpRequest);
 public
  { Public declarations }
 end;

var
 Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);

 procedure TryToOpenWebPort;
 var
  s: string;
 begin
  try
   if not FSynHttpServer.Active then
    FSynHttpServer.Active := True;
  except
   on E: Exception do
   begin
    s := Format('Exception %s occurred while trying activate http or https connection. Message:"%s".',
     [E.ClassName, E.Message]);
    //Log(s);
   end;
  end;
 end;

begin
 FSynHttpServer := TSynHttpServer.Create(Self);
 FSynHttpServer.OnHttpGet := SynHttpServer1HttpGet;
 FSynHttpServer.Port := '8080';
 TryToOpenWebPort;
end;

procedure TForm2.SynHttpServer1HttpGet(Sender: TObject; Connection: TSynTcpSrvConnection;
 ARequestInfo, AResponseInfo: THttpRequest);

 procedure WriteData;
 begin
  try
   FSynHttpServer.SendReply(Connection, ARequestInfo, AResponseInfo);
  except
   On E: Exception do
    if (Pos('10054', E.Message) = 0) and (Pos('10053', E.Message) = 0) then
     {Log('Error; Exception occured. ' + E.Message)};
  end;
 end;

 procedure RespString(const Str: string; const CharSet: string = '');
 begin
  if Str.IsEmpty then
   AResponseInfo.Content := ' '
  else
   AResponseInfo.Content := Str;
  AResponseInfo.ContentType := 'text/html';
  AResponseInfo.CharSet := CharSet;
  AResponseInfo.StatusCode := 200;
  WriteData;
 end;

begin
 if ARequestInfo.Params.Values['Ping'] = 'Ping' then
  RespString('Pong');
end;

end.
