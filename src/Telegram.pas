unit Telegram;

interface

uses
Horse,
REST.Json,
System.StrUtils,
System.Variants,
System.Classes,
Telegram.Request,
Telegram.Returns,
Telegram.Returns.Pooling,
System.SysUtils;

type
TOnWebhookStatus  = procedure(const AStatus: string) of object;
TOnMessage        = procedure(ARetMessage: TRetMessage) of object;
TOnMessagePooling = procedure(ARetMessage: TRetMessagePooling) of object;
TOnError          = procedure(const AError: string) of object;

TConfig = class(TComponent)
private
FTokenBot: string;
published
property TokenBot: string read FTokenBot write FTokenBot;
end;

TWebhook = class(TComponent)
 private
 FHost: string;
 FPort: Integer;
 procedure SetHost(const Value: string);
 procedure SetPort(const Value: Integer);
 published
 property Host: string  read FHost write SetHost;
 property Port: Integer read FPort write SetPort;
end;

TTelegram4D = class(TComponent)
 private
 FAbout  : string;
 FConfig : TConfig;
 FWebhook: TWebhook;
 FOnWebhookStatus: TOnWebhookStatus;
 FOnMessage: TOnMessage;
 FOnMessagePooling: TOnMessagePooling;
 FOnError: TOnError;
 procedure WhMessage(Req: THorseRequest; Res: THorseResponse);
 published
 property About           : string            read FAbout;
 property Config          : TConfig           read FConfig           write FConfig;
 property OnError         : TOnError          read FOnError          write FOnError;
 property Webhook         : TWebhook          read FWebhook          write FWebhook;
 property OnWebhookStatus : TOnWebhookStatus  read FOnWebhookStatus  write FOnWebhookStatus;
 property OnMessage       : TOnMessage        read FOnMessage        write FOnMessage;
 property OnMessagePooling: TOnMessagePooling read FOnMessagePooling write FOnMessagePooling;
 public
 procedure SendMessage(const AChatID: string; AMessage: string);
 procedure ReadMessage(const AUpdateId: Integer);
 procedure GetUpdate;

 procedure StartWebhook;
 procedure StopWebhook;

 constructor Create(AOwner: TComponent); override;
 destructor Destroy; override;
end;

procedure register;

implementation

{$R .\TTelegram4D.dcr}

 procedure register;
 begin
  RegisterComponents('BMCoder', [TTelegram4D]);
 end;

{ TWebhook }

procedure TWebhook.SetHost(const Value: string);
begin
 FHost := Value;
end;

procedure TWebhook.SetPort(const Value: Integer);
begin
 FPort := Value;
end;

{ TTelegram4D }

constructor TTelegram4D.Create(AOwner: TComponent);
begin
  inherited;
 FConfig       := TConfig.Create(Self);
 FWebhook      := TWebhook.Create(Self);
 FConfig.Name  := 'Config';
 FWebhook.Name := 'Webhook';
 FWebhook.FHost:= 'http://127.0.0.1:9000';
 FWebhook.FPort:= 9000;
 FAbout        := 'Discord: bmcoder#3620';

 FConfig.SetSubComponent(True);
 FWebhook.SetSubComponent(True);
end;

destructor TTelegram4D.Destroy;
begin

  inherited;
end;

procedure TTelegram4D.GetUpdate;
begin
 try
  if Assigned(FOnMessagePooling) then
  FOnMessagePooling(TTelegramRequest.GetUpdate(FConfig.TokenBot));
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.ReadMessage(const AUpdateId: Integer);
begin
 try
  TTelegramRequest.ReadMessage(FConfig.TokenBot,AUpdateId);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.WhMessage(Req: THorseRequest; Res: THorseResponse);
begin
 try
  if Assigned(FOnMessage) then
  begin
   FOnMessage(TJson.JsonToObject<TRetMessage>(Req.Body));
  end;

  Res.Status(200);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.SendMessage(const AChatID: string; AMessage: string);
begin
 try
  TTelegramRequest.SendMessage(FConfig.TokenBot,AChatID,AMessage);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.StartWebhook;
begin
 THorse.Post('/wbtelegram',  WhMessage);
 {$IF DEFINED(HORSE_CGI)}
 THorse.Listen;
 {$ELSE}
 if not THorse.IsRunning = true then
 THorse.Listen(FWebhook.Port);
 {$ENDIF}
 THorse.Listen(9000);
end;

procedure TTelegram4D.StopWebhook;
begin
 {$IF DEFINED(HORSE_CGI)}
 THorse.StopListen;
 {$ELSE}
 if THorse.IsRunning = true then
 THorse.StopListen;
 {$ENDIF}
end;

end.
