unit Telegram.Consts;

interface

 uses
 Telegram.Returns.Pooling;

 const
 SEND_MESSAGE   = 'https://api.telegram.org/bot<token>/sendMessage';
 SEND_POLL      = 'https://api.telegram.org/bot<token>/sendPoll';
 SEND_PHOTO     = 'https://api.telegram.org/bot<token>/sendPhoto';
 SEND_DOCUMENT  = 'https://api.telegram.org/bot<token>/sendDocument';
 SEND_LOCATION  = 'https://api.telegram.org/bot<token>/sendLocation';
 GET_UPDATE     = 'https://api.telegram.org/bot<token>/getUpdates';
 SET_WEBHOOK    = 'https://api.telegram.org/bot<token>/setWebhook';
 INFO_WEBHOOK   = 'https://api.telegram.org/bot<token>/getWebhookInfo';
 DELETE_WEBHOOK = 'https://api.telegram.org/bot<token>/deleteWebhook';

 JSON_INLINE =
 '{'+
   '"resize_keyboard":true,'+
   '"inline_keyboard":['+

   ']'+
'}';

 JSON_KEYBOARD =
'{'+
   '"one_time_keyboard":true,'+
   '"resize_keyboard":true,'+
   '"keyboard":['+

   ']'+
'}';

 type
 TReceiveMessage  = (rmWebhook, rmPooling);
 TReplyMarkupType = (rmInline, rmKeyboard);

 TRespMessage = record
  CallBackId: string;
  ChatId    : Integer;
  MessageId : Integer;
  Text      : string;
  Data      : string;
 end;

TRespMessagePooling = record
 RetType           : string;
 RetMessagePooling : TRetMessagePooling;
 RetMessageCallback: TRetMessageCallback;

 procedure ClearObjects;
end;

implementation

{ TRespMessagePooling }

procedure TRespMessagePooling.ClearObjects;
begin
 if Assigned(RetMessagePooling) then
 RetMessagePooling.Free;

 if Assigned(RetMessageCallback) then
 RetMessageCallback.Free;
end;

end.
