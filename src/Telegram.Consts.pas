unit Telegram.Consts;

interface

 uses
 Telegram.Returns,
 Telegram.Returns.Pooling,
 System.SysUtils;

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
 TRetType         = (rtNull, rtNormal, rtCallback);

 TReturnMessage = record
  CallBackId: string;
  ChatId    : Integer;
  MessageId : Integer;
  Text      : string;
  Data      : string;
 end;

TRespMessagePooling = record
 RetType           : TRetType;
 RetMessagePooling : TRetMessagePooling;
 RetMessageCallback: TRetMessageCallback;

 procedure ClearObjects;
end;

TRespMessage = record
 RetType           : TRetType;
 RetMessage        : TRetMessage;
 RetMsgCallback    : TRetMsgCallback;

 procedure ClearObjects;
end;

implementation

{ TRespMessagePooling }

procedure TRespMessagePooling.ClearObjects;
begin
 if Assigned(RetMessagePooling) then
 FreeAndNil(RetMessagePooling);

 if Assigned(RetMessageCallback) then
 FreeAndNil(RetMessageCallback);
end;

{ TRespMessage }

procedure TRespMessage.ClearObjects;
begin
 if Assigned(RetMessage) then
 FreeAndNil(RetMessage);

 if Assigned(RetMsgCallback) then
 FreeAndNil(RetMsgCallback);
end;

end.
