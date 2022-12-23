unit Telegram.Consts;

interface

 const
 SEND_MESSAGE   = 'https://api.telegram.org/bot<token>/sendMessage';
 SEND_PHOTO     = 'https://api.telegram.org/bot<token>/sendPhoto';
 GET_UPDATE     = 'https://api.telegram.org/bot<token>/getUpdates';
 SET_WEBHOOK    = 'https://api.telegram.org/bot<token>/setWebhook';
 DELETE_WEBHOOK = 'https://api.telegram.org/bot<token>/deleteWebhook';

 type
 TReceiveMessage = (rmWebhook, rmPooling);

implementation

end.
