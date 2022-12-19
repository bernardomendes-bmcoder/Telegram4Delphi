unit Telegram.Message;

interface

uses
RESTRequest4D,
JsonsBM,
System.StrUtils,
System.Variants,
System.SysUtils,
Telegram.Consts;


type
TMessage = class
 private
 FBotToken: string;
 FChatID  : string;
 procedure SetBotToken(const Value: string);
 procedure SetChatID(const Value: string);
 procedure FreeUpdate(AUpdateId: Integer);
 public
 procedure Send(AChatID: string; AMessage: string);
 function GetUpdate: string;

 property BotToken: string read FBotToken write SetBotToken;
 property ChatID  : string read FChatID   write SetChatID;

end;

implementation


{ TMessage }

procedure TMessage.FreeUpdate(AUpdateId: Integer);
var
  LResponse: IResponse;
  URL      : string;
begin
  var UpdateNew := AUpdateId + 1;
  URL := 'https://api.telegram.org/bot{token}/getUpdates';
  URL := StringReplace(URL,'{token}',FBotToken,[rfReplaceAll]);
  LResponse := TRequest.New.BaseURL(URL)
  .AddParam('offset', IntToStr(UpdateNew))
  .Accept('application/json')
  .Get;
end;

function TMessage.GetUpdate: string;
var
  LResponse: IResponse;
  URL : string;
  Json: TJson;
  UpdateId: Integer;
begin
  URL := 'https://api.telegram.org/bot{token}/getUpdates';
  URL := StringReplace(URL,'{token}',FBotToken,[rfReplaceAll]);
  LResponse := TRequest.New.BaseURL(URL)
  .AddParam('limit', '1')
  .AddParam('timeout', '100')
  .Accept('application/json')
  .Get;

 if LResponse.StatusCode = 200 then
 begin
  try
   Json := TJson.Create;
   Json.Parse(LResponse.Content);
   if Json.JsonObject.Values['result'].AsArray.Count > 0 then
   begin
    Result   := IntToStr(Json.JsonObject.Values['result'].AsArray.Items[0].AsObject.Values['message'].AsObject.Values['chat'].AsObject.Values['id'].AsInteger);
    UpdateId := Json.JsonObject.Values['result'].AsArray.Items[0].AsObject.Values['update_id'].AsInteger;
    FreeUpdate(UpdateId);
   end;
  finally
   FreeAndNil(Json);
  end;
 end;
end;

procedure TMessage.Send(AChatID, AMessage: string);
var
  LResponse: IResponse;
  URL: string;
begin
   URL := 'https://api.telegram.org/bot{token}/sendMessage';
   URL :=StringReplace(URL,'{token}',FBotToken,[rfReplaceAll]);
  LResponse := TRequest.New.BaseURL(URL)
  .AddParam('chat_id', AChatID)
  .AddParam('text', AMessage)
  .AddParam('parse_mode','markdown')
  .Accept('application/json')
  .Get;
end;

procedure TMessage.SetBotToken(const Value: string);
begin
 FBotToken := Value;
end;

procedure TMessage.SetChatID(const Value: string);
begin
 FChatID := Value;
end;

end.
