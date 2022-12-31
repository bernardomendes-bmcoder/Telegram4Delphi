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
 class procedure SendButton(const ATokenBot: string; const AChatID: string; AMessage: string; AReplyMarkup: string; ATypeReplyMarkup: TReplyMarkupType = rmInline);
 class procedure SendPoll(const ATokenBot: string; const AChatID: string; AQuestion: string; AOptions: string; Anonymous: Boolean = False; AMultiple: Boolean = false);
 class procedure SendImage(const ATokenBot: string; const AChatID: string; const AImageUrl: string; const ACaption: string);
 class procedure SendLocation(const ATokenBot: string; const AChatID: string; ALatitude: Extended; ALongitude: Extended);
 class procedure SendDocument(const ATokenBot: string; const AChatID: string; const ADocumentUrl: string; const ACaption: string);
 class function GetUpdate(const ATokenBot: string): TRespMessagePooling;
 class function GetInfoWebhook(ATokenBot: string): Boolean;
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

class function TTelegramRequest.GetUpdate(const ATokenBot: string): TRespMessagePooling;
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
   Result.RetType := rtNull;

   if Pos('callback_query',LJson.Stringify) = 0 then
   begin
    if LJson.JsonObject.Values['result'].AsArray.Count > 0 then
    begin
     Result.RetType           := rtNormal;
     Result.RetMessagePooling := TJson.JsonToObject<TRetMessagePooling>(LResponse.Content);
    end;
   end else
   begin
    if LJson.JsonObject.Values['result'].AsArray.Count > 0 then
    begin
     Result.RetType            := rtCallback;
     Result.RetMessageCallback := TJson.JsonToObject<TRetMessageCallback>(LResponse.Content);
    end;
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

class procedure TTelegramRequest.SendLocation(const ATokenBot: string; const AChatID: string; ALatitude: Extended; ALongitude: Extended);
var
  LResponse: IResponse;
  URL: string;
begin
 URL := SEND_MESSAGE;
 URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

 LResponse := TRequest.New.BaseURL(URL)
 .AddParam('chat_id', AChatID)
 .AddParam('latitude', FloatToStr(ALatitude))
 .AddParam('longitude', FloatToStr(ALongitude))
 .AddParam('parse_mode','markdown')
 .Accept('application/json')
 .Get;
end;

class procedure TTelegramRequest.SendPoll(const ATokenBot: string; const AChatID: string; AQuestion: string; AOptions: string; Anonymous: Boolean; AMultiple: Boolean);
var
  LResponse: IResponse;
  URL: string;
begin
 URL := SEND_POLL;
 URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

 LResponse := TRequest.New.BaseURL(URL)
 .AddParam('chat_id', AChatID)
 .AddParam('question', AQuestion)
 .AddParam('options', AOptions)
 .AddParam('is_anonymous', BoolToStr(Anonymous))
 .AddParam('allows_multiple_answers', BoolToStr(AMultiple))
 .AddParam('parse_mode','markdown')
 .Accept('application/json')
 .Get;
end;

class procedure TTelegramRequest.SendButton(const ATokenBot: string; const AChatID: string; AMessage: string; AReplyMarkup: string; ATypeReplyMarkup: TReplyMarkupType);
var
  LResponse: IResponse;
  URL: string;
  Json: j4dl.TJson;
  JsonString: string;
begin
 try
   Json := j4dl.TJson.Create;
   if ATypeReplyMarkup = rmInline then
   begin
    Json.Parse(JSON_INLINE);
    Json['inline_keyboard'].Parse(AReplyMarkup);
   end;

   if ATypeReplyMarkup = rmKeyboard then
   begin
    Json.Parse(JSON_KEYBOARD);
    Json['keyboard'].Parse(AReplyMarkup);
   end;

   JsonString := Json.Stringify;

   URL  := SEND_MESSAGE;
   URL  :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);
   LResponse := TRequest.New.BaseURL(URL)
   .AddParam('chat_id', AChatID)
   .AddParam('text', AMessage)
   .AddParam('reply_markup', JsonString)
   .AddParam('parse_mode','markdown')
   .Accept('application/json')
   .Get;
 finally
  FreeAndNil(Json);
 end;
end;

class procedure TTelegramRequest.SendDocument(const ATokenBot, AChatID, ADocumentUrl, ACaption: string);
var
  LResponse: IResponse;
  URL: string;
begin
 URL := SEND_DOCUMENT;
 URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);

 LResponse := TRequest.New.BaseURL(URL)
 .AddParam('chat_id', AChatID)
 .AddParam('caption', ACaption)
 .AddParam('document', ADocumentUrl)
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

class function TTelegramRequest.GetInfoWebhook(ATokenBot: string): Boolean;
var
  LResponse: IResponse;
  URL: string;
  LJson: j4dl.TJson;
begin
 URL := INFO_WEBHOOK;
 URL :=StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);
try
 try
  LResponse := TRequest.New.BaseURL(URL)
  .Accept('application/json')
  .Get;

  if LResponse.StatusCode = 200 then
  begin
   LJson := j4dl.TJson.Create;
   LJson.Parse(LResponse.Content);
   if LJson.JsonObject.Values['result'].AsObject.Values['url'].AsString = '' then
   Result := False;

   if LJson.JsonObject.Values['result'].AsObject.Values['url'].AsString <> '' then
   Result := True;
  end;
 finally
  FreeAndNil(LJson);
 end;
except
 Result := False;
end;
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
