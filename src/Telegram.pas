unit Telegram;

interface

uses
Horse,
Rest.Json.Types,
REST.Json,
System.StrUtils,
System.Variants,
System.Classes,
Telegram.Consts,
Telegram.Request,
Telegram.Returns,
Telegram.ReadMsg,
Telegram.Returns.Pooling,
System.SysUtils;

type
TOnWebhookStatus  = procedure(const AStatus: Boolean = False) of object;
TOnMessage        = procedure(ARetMessage: TRespMessage) of object;
TOnMessagePooling = procedure(ARetMessage: TRespMessagePooling) of object;
TOnError          = procedure(const AError: string) of object;

TConfig = class(TComponent)
private
FTokenBot   : string;
FTypeReceive: TReceiveMessage;
procedure SetTokenBot(const Value: string);
procedure SetTypeReceive(const Value: TReceiveMessage);
published
property TokenBot   : string          read FTokenBot    write SetTokenBot;
property TypeReceive: TReceiveMessage read FTypeReceive write SetTypeReceive default rmPooling;
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
 FThread : TUnReadMsg;
 FRespMessage: TRespMessage;
 FRetMessagePooling: TRespMessagePooling;
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
 procedure SendButton(const AChatID: string; AMessage: string; AReplyMarkup: string; ATypeReplyMarkup: TReplyMarkupType = rmInline);
 procedure SendPoll(const AChatID: string; AQuestion: string; AOptions: string; Anonymous: Boolean = False; AMultiple: Boolean = false);
 procedure SendLocation(const AChatID: string; ALatitude: Extended; ALongitude: Extended);
 procedure SendDocument(const AChatID: string; const ADocumentUrl: string; const ACaption: string);
 procedure ReadMessage(const AUpdateId: Integer);
 procedure GetUpdate;
 procedure StartPooling;
 procedure StartWebhook;
 procedure StopWebhook;
 procedure ShutDown;

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
 FRetMessagePooling.ClearObjects;
 FRespMessage.ClearObjects;

 if Assigned(FThread) then
 begin
  FThread.Terminate;
 end;
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
  FRespMessage.ClearObjects;
  FRespMessage.RetType := rtNull;
  if Pos('callback_query',Req.Body) = 0 then
  begin
   FRespMessage.RetType    := rtNormal;
   FRespMessage.RetMessage := TJson.JsonToObject<TRetMessage>(Req.Body);
   if Assigned(FOnMessage) then
   FOnMessage(FRespMessage);
  end;

  if Pos('callback_query',Req.Body) > 0 then
  begin
   FRespMessage.RetType        := rtCallback;
   FRespMessage.RetMsgCallback := TJson.JsonToObject<TRetMsgCallback>(Req.Body);
   if Assigned(FOnMessage) then
   FOnMessage(FRespMessage);
  end;

  Res.Status(200);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.SendButton(const AChatID: string; AMessage, AReplyMarkup: string; ATypeReplyMarkup: TReplyMarkupType);
begin
 try
  TTelegramRequest.SendButton(FConfig.TokenBot, AChatID, AMessage, AReplyMarkup, ATypeReplyMarkup);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.SendDocument(const AChatID, ADocumentUrl, ACaption: string);
begin
 try
  TTelegramRequest.SendDocument(FConfig.TokenBot,AChatID,ADocumentUrl,ACaption);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.SendLocation(const AChatID: string; ALatitude, ALongitude: Extended);
begin
 try
  TTelegramRequest.SendLocation(FConfig.TokenBot,AChatID,ALatitude,ALongitude);
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

procedure TTelegram4D.SendPoll(const AChatID: string; AQuestion, AOptions: string; Anonymous, AMultiple: Boolean);
begin
 try
  TTelegramRequest.SendPoll(FConfig.TokenBot, AChatID, AQuestion, AOptions, Anonymous, AMultiple);
 except
  on E:Exception do
  if Assigned(FOnError) then
  FOnError(E.Message);
 end;
end;

procedure TTelegram4D.StartWebhook;
begin
 if FConfig.TypeReceive = rmPooling then
 begin
  if Assigned(FOnError) then
  FOnError('Select receipt type for webhook');
  Exit;
 end;

 try
  if TTelegramRequest.GetInfoWebhook(FConfig.TokenBot) = False then
  TTelegramRequest.SetWebhook(FConfig.TokenBot,FWebhook.Host+'/wbtelegram');

  THorse.Post('/wbtelegram',  WhMessage);
  {$IF DEFINED(HORSE_CGI)}
  THorse.Listen;
  {$ELSE}
  if not THorse.IsRunning = true then
  THorse.Listen(FWebhook.Port);
  {$ENDIF}
 finally
  if Assigned(FOnWebhookStatus) then
  FOnWebhookStatus(True);
 end;
end;

procedure TTelegram4D.StopWebhook;
begin
try
 try
  if TTelegramRequest.GetInfoWebhook(FConfig.TokenBot) then
  TTelegramRequest.DeleteWebhook(FConfig.TokenBot);

  {$IF DEFINED(HORSE_CGI)}
  THorse.StopListen;
  {$ELSE}
  if THorse.IsRunning = true then
  THorse.StopListen;
  {$ENDIF}
 finally
  if Assigned(FOnWebhookStatus) then
  FOnWebhookStatus(False);
 end;
except
 on E:Exception do
 if Assigned(FOnError) then
 FOnError(E.Message);
end;
end;

procedure TTelegram4D.StartPooling;
 begin
  if FConfig.TypeReceive = rmWebhook then
  begin
   if Assigned(FOnError) then
   FOnError('Select receipt type for pooling');
   Exit;
  end;

  FThread := TUnReadMsg.Create(
  procedure
  var I: Integer;
  begin

   if FConfig.TypeReceive = rmPooling then
    begin
      try
       if FThread.CheckTerminated then
       Exit;

       FRetMessagePooling := TTelegramRequest.GetUpdate(FConfig.TokenBot);

       if FRetMessagePooling.RetType = rtNormal then
       if Length(FRetMessagePooling.RetMessagePooling.Result) > 0 then
       begin
        if Assigned(FOnMessagePooling) then
        FOnMessagePooling(FRetMessagePooling);

        for I := Low(FRetMessagePooling.RetMessagePooling.Result) to High(FRetMessagePooling.RetMessagePooling.Result) do
        begin
         TTelegramRequest.ReadMessage(FConfig.TokenBot,FRetMessagePooling.RetMessagePooling.Result[I].UpdateId);
        end;
       end;

       if FRetMessagePooling.RetType = rtCallback then
       if Length(FRetMessagePooling.RetMessageCallback.Result) > 0 then
       begin
        if Assigned(FOnMessagePooling) then
        FOnMessagePooling(FRetMessagePooling);

        for I := Low(FRetMessagePooling.RetMessageCallback.Result) to High(FRetMessagePooling.RetMessageCallback.Result) do
        begin
         TTelegramRequest.ReadMessage(FConfig.TokenBot,FRetMessagePooling.RetMessageCallback.Result[I].UpdateId);
        end;
       end;
      except
       on E:Exception do
       if Assigned(FOnError) then
       FOnError(E.Message);
      end;
    end;
  end,2000);
  FThread.Start;
 end;

procedure TTelegram4D.ShutDown;
begin
  if FThread <> nil then
  begin
   FThread.Terminate;
   FThread.WaitFor;
   FThread.Free;
  end;
end;
{ TConfig }

procedure TConfig.SetTokenBot(const Value: string);
begin
 FTokenBot := Value;
end;

procedure TConfig.SetTypeReceive(const Value: TReceiveMessage);
begin
 FTypeReceive := Value;
end;

end.
