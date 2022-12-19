unit Telegram.Returns.Pooling;

interface

uses
  REST.Json.Types;

{$M+}

type
  TChat = class;
  TFrom = class;
  TMessage = class;

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

  TResult = class
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

  TRetMessagePooling = class
  private
    FOk: Boolean;
    [JSONName('result'), JSONMarshalled(False)]
    FResult: TArray<TResult>;

  published
    property Ok: Boolean read FOk write FOk;
  public
    property Result: TArray<TResult> read FResult write FResult;
    procedure ResultClear;
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

{ TResult }

constructor TResult.Create;
begin
  inherited;
  FMessage := TMessage.Create;
end;

destructor TResult.Destroy;
begin
  FMessage.Free;
  inherited;
end;

{ TRetMessagePooling }

procedure TRetMessagePooling.ResultClear;
var
  ObjectIndex : Integer;
begin
   for ObjectIndex := Low(FResult) to High(FResult) do
  begin
    FResult[ObjectIndex].Free;
    FResult[ObjectIndex] := nil;
  end;
  SetLength(FResult, 0);
end;

constructor TRetMessagePooling.Create;
begin
  ResultClear;
end;

destructor TRetMessagePooling.Destroy;
begin
  ResultClear;
  inherited;
end;


end.
