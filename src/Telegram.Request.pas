unit Telegram.Request;

interface

uses
Json,
REST.Json.Types,
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
 class procedure SendButton(const ATokenBot: string; const AChatID: string; AMessage: string; AReplyMarkup: array of string; ATypeReplyMarkup: TReplyMarkupType = rmInline);
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

var
FRespPooling: TRespMessagePooling;

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
  LJson: TJSONObject;
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
   LJson := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
   FRespPooling.ClearObjects;
   FRespPooling.RetType := rtNull;

   if Pos('callback_query',LJson.ToString) = 0 then
   begin
    if (LJson.GetValue('result') as TJSONArray).Count > 0 then
    begin
     FRespPooling.RetType           := rtNormal;
     FRespPooling.RetMessagePooling := TJson.JsonToObject<TRetMessagePooling>(LResponse.Content);
     Result := FRespPooling;
    end;
   end else
   begin
    if (LJson.GetValue('result') as TJSONArray).Count > 0 then
    begin
     FRespPooling.RetType            := rtCallback;
     FRespPooling.RetMessageCallback := TJson.JsonToObject<TRetMessageCallback>(LResponse.Content);
     Result := FRespPooling;
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

class procedure TTelegramRequest.SendButton(const ATokenBot: string; const AChatID: string; AMessage: string; AReplyMarkup: array of string; ATypeReplyMarkup: TReplyMarkupType);
var
  LResponse: IResponse;
  URL: string;
  JsonObj: TJSONObject;
  JsonArray: TJSONArray;
  JsonString: string;
  i: Integer;
begin
  try
    JsonObj := TJSONObject.Create;
    if ATypeReplyMarkup = rmInline then
    begin
      JsonArray := TJSONArray.Create;
      for i := 0 to High(AReplyMarkup) do
      begin
        var ButtonObj := TJSONObject.Create;
        ButtonObj.AddPair('text', AReplyMarkup[i]);
        JsonArray.AddElement(ButtonObj);
      end;

      JsonObj.AddPair('inline_keyboard', TJSONArray.Create(JsonArray));
    end;

    if ATypeReplyMarkup = rmKeyboard then
    begin
      JsonArray := TJSONArray.Create;
      for i := 0 to High(AReplyMarkup) do
        JsonArray.AddElement(TJSONObject.Create(TJSONPair.Create('text', AReplyMarkup[i])));
      JsonObj.AddPair('keyboard', TJSONArray.Create(JsonArray));
    end;

    JsonString := JsonObj.ToString;

    URL  := SEND_MESSAGE;
    URL  := StringReplace(URL,'<token>',ATokenBot,[rfReplaceAll]);
    LResponse := TRequest.New.BaseURL(URL)
    .AddParam('chat_id', AChatID)
    .AddParam('text', AMessage)
    .AddParam('reply_markup', JsonString)
    .AddParam('parse_mode','markdown')
    .Accept('application/json')
    .Get;
  finally
    FreeAndNil(JsonObj);
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
  LJson: TJSONObject;
  LResult: TJSONValue;
  LUrl: TJSONValue;
begin
  Result := False;
  URL := INFO_WEBHOOK;
  URL := StringReplace(URL, '<token>', ATokenBot, [rfReplaceAll]);
  try
    LResponse := TRequest.New.BaseURL(URL)
      .Accept('application/json')
      .Get;

    if LResponse.StatusCode = 200 then
    begin
      LJson := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONObject;
      if Assigned(LJson) then
      begin
        LResult := LJson.GetValue('result');
        if Assigned(LResult) and (LResult is TJSONObject) then
        begin
          LUrl := TJSONObject(LResult).GetValue('url');
          if Assigned(LUrl) and (LUrl.Value <> '') then
            Result := True;
        end;
      end;
    end;
  finally
    FreeAndNil(LJson);
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
