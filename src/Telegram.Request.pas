unit Telegram.Request;

interface

uses
j4dl,
REST.Json,
System.StrUtils,
System.Variants,
System.SysUtils,
Telegram.Consts,
Telegram.Returns.Pooling,
RESTRequest4D;

type
TTelegramRequest = class
 public
 class procedure SendMessage(const ATokenBot: string; const AChatID: string; AMessage: string);
 class procedure SendImage(const ATokenBot: string; const AChatID: string; const AImageUrl: string; const ACaption: string);
 class function GetUpdate(const ATokenBot: string): TRetMessagePooling;
 class procedure ReadMessage(const ATokenBot: string; const AUpdateId: Integer);
 class procedure SetWebhook(const ATokenBot: string; const AUrl: string);
 class procedure DeleteWebhook(const ATokenBot: string);
end;

implementation

//LIBERA A MENSAGEM
class procedure TTelegramRequest.ReadMessage(const ATokenBot: string; const AUpdateId: Integer);
var
  LResponse: IResponse;
  URL      : string;
begin
  var UpdateNew := AUpdateId + 1;
  URL := GET_UPDATE;

  URL := StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);
  LResponse := TRequest.New.BaseURL(URL)
  .AddParam('offset', IntToStr(UpdateNew))
  .Accept('application/json')
  .Get;
end;

class function TTelegramRequest.GetUpdate(const ATokenBot: string): TRetMessagePooling;
var
  LResponse: IResponse;
  URL : string;
  LJson: j4dl.TJson;
  UpdateId: Integer;
begin
  URL := GET_UPDATE;
  URL := StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

  LResponse := TRequest.New.BaseURL(URL)
  .AddParam('timeout', '100')
  .Accept('application/json')
  .Get;

 if LResponse.StatusCode = 200 then
 begin
  try
   LJson := j4dl.TJson.Create;
   LJson.Parse(LResponse.Content);
   if LJson.JsonObject.Values['result'].AsArray.Count > 0 then
   begin
    Result := TJson.JsonToObject<TRetMessagePooling>(LResponse.Content);
   end;
  finally
   FreeAndNil(LJson);
  end;
 end;
end;

class procedure TTelegramRequest.SendMessage(const ATokenBot: string; const AChatID: string; AMessage: string);
var
  LResponse: IResponse;
  URL: string;
begin
   URL := SEND_MESSAGE;
   URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

   LResponse := TRequest.New.BaseURL(URL)
  .AddParam('chat_id', AChatID)
  .AddParam('text', AMessage)
  .AddParam('parse_mode','markdown')
  .Accept('application/json')
  .Get;
end;

class procedure TTelegramRequest.SendImage(const ATokenBot: string; const AChatID: string; const AImageUrl: string; const ACaption: string);
var
  LResponse: IResponse;
  URL: string;
begin
   URL := SEND_PHOTO;
   URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

   LResponse := TRequest.New.BaseURL(URL)
  .AddParam('chat_id', AChatID)
  .AddParam('caption', ACaption)
  .AddParam('photo', AImageUrl)
  .AddParam('parse_mode','markdown')
  .Accept('application/json')
  .Get;
end;

class procedure TTelegramRequest.SetWebhook(const ATokenBot: string; const AUrl: string);
var
  LResponse: IResponse;
  URL: string;
begin
   URL := SET_WEBHOOK;
   URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

   LResponse := TRequest.New.BaseURL(URL)
  .AddParam('url', AUrl)
  .Accept('application/json')
  .Get;
end;

class procedure TTelegramRequest.DeleteWebhook(const ATokenBot: string);
var
  LResponse: IResponse;
  URL: string;
begin
   URL := DELETE_WEBHOOK;
   URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

   LResponse := TRequest.New.BaseURL(URL)
  .Accept('application/json')
  .Get;
end;


end.
