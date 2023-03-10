unit Telegram.Returns;

interface

uses
 REST.Json.Types;

{$M+}

type
  TChat = class;
  TFrom = class;

  TChat = class
  private
    [JSONName('first_name')]
    FFirstName: string;
    FId: Integer;
    [JSONName('last_name')]
    FLastName: string;
    FType: string;
    FUsername: string;
  published
    property FirstName: string read FFirstName write FFirstName;
    property Id: Integer read FId write FId;
    property LastName: string read FLastName write FLastName;
    property &Type: string read FType write FType;
    property Username: string read FUsername write FUsername;
  end;

  TFrom = class
  private
    [JSONName('first_name')]
    FFirstName: string;
    FId: Integer;
    [JSONName('is_bot')]
    FIsBot: Boolean;
    [JSONName('language_code')]
    FLanguageCode: string;
    [JSONName('last_name')]
    FLastName: string;
    FUsername: string;
  published
    property FirstName: string read FFirstName write FFirstName;
    property Id: Integer read FId write FId;
    property IsBot: Boolean read FIsBot write FIsBot;
    property LanguageCode: string read FLanguageCode write FLanguageCode;
    property LastName: string read FLastName write FLastName;
    property Username: string read FUsername write FUsername;
  end;

  TMessage = class
  private
    FChat: TChat;
    FDate: Integer;
    FFrom: TFrom;
    [JSONName('message_id')]
    FMessageId: Integer;
    FText: string;
  published
    property Chat: TChat read FChat;
    property Date: Integer read FDate write FDate;
    property From: TFrom read FFrom;
    property MessageId: Integer read FMessageId write FMessageId;
    property Text: string read FText write FText;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCallbackQuery = class
  private
    [JSONName('chat_instance')]
    FChatInstance: string;
    FData: string;
    FFrom: TFrom;
    FId: string;
    FMessage: TMessage;
  published
    property ChatInstance: string read FChatInstance write FChatInstance;
    property Data: string read FData write FData;
    property From: TFrom read FFrom;
    property Id: string read FId write FId;
    property Message: TMessage read FMessage;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRetMsgCallback = class
  private
    [JSONName('callback_query')]
    FCallbackQuery: TCallbackQuery;
    [JSONName('update_id')]
    FUpdateId: Integer;
  published
    property CallbackQuery: TCallbackQuery read FCallbackQuery;
    property UpdateId: Integer read FUpdateId write FUpdateId;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRetMessage = class
  private
    FMessage: TMessage;
    [JSONName('update_id')]
    FUpdateId: Integer;
  published
    property Message: TMessage read FMessage;
    property UpdateId: Integer read FUpdateId write FUpdateId;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TMessage }

constructor TMessage.Create;
begin
 inherited;
 FFrom := TFrom.Create;
 FChat := TChat.Create;
end;

destructor TMessage.Destroy;
begin
 FFrom.Free;
 FChat.Free;
 inherited;
end;

{ TRetMessage }

constructor TRetMessage.Create;
begin
 inherited;
 FMessage := TMessage.Create;
end;

destructor TRetMessage.Destroy;
begin
 FMessage.Free;
 inherited;
end;

constructor TCallbackQuery.Create;
begin
  inherited;
  FFrom := TFrom.Create;
  FMessage := TMessage.Create;
end;

destructor TCallbackQuery.Destroy;
begin
  FFrom.Free;
  FMessage.Free;
  inherited;
end;

{ TRetMsgCallback }

constructor TRetMsgCallback.Create;
begin
  inherited;
  FCallbackQuery := TCallbackQuery.Create;
end;

destructor TRetMsgCallback.Destroy;
begin
  FCallbackQuery.Free;
  inherited;
end;

end.
