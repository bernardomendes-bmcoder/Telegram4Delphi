unit j4dl;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  {$ifdef fpc}
  Classes, SysUtils, LazUTF8;
  {$else}
  System.Classes, System.SysUtils;
  {$endif}

type
  TJsonValueType = (jvNone, jvNull, jvString, jvInteger, jvNumber, jvBoolean, jvObject, jvArray);
  TJsonStructType = (jsNone, jsArray, jsObject);
  TJsonNull = (null);
  TJsonEmpty = (empty);

type

  { TJsonBase }

  TJsonBase = class(TObject)
  private
    FOwner: TJsonBase;
    function GetOwner: TJsonBase;
  protected
    function GetOwnerName: String;
    procedure RaiseError(const Msg: String);
    procedure RaiseParseError(const JsonString: String);
    procedure RaiseAssignError(Source: TJsonBase);

    function GetDecimalSeparator : Char;
  public
    constructor Create(AOwner: TJsonBase);
    destructor Destroy; override;

    procedure Parse(JsonString: String); virtual; abstract;
    function Stringify: String; virtual; abstract;

    procedure Assign(Source: TJsonBase); virtual; abstract;

    function Encode(const S: String): String;
    function Decode(const S: String): String;

    procedure Split(const S: String; const Delimiter: Char; Strings: TStrings);

    function IsJsonObject(const S: String): Boolean;
    function IsJsonArray(const S: String): Boolean;
    function IsJsonString(const S: String): Boolean;
    function IsJsonInteger(const S: String): Boolean;
    function IsJsonNumber(const S: String): Boolean;
    function IsJsonBoolean(const S: String): Boolean;
    function IsJsonNull(const S: String): Boolean;

    function AnalyzeJsonValueType(const S: String): TJsonValueType;
  public
    property Owner: TJsonBase read GetOwner;
  end;

  TJsonObject = class;
  TJsonArray = class;
  TJsonValue = class(TJsonBase)
  private
    FValueType: TJsonValueType;
    FStringValue: String;
    FIntegerValue: Int64;
    FNumberValue: Extended;
    FBooleanValue: Boolean;
    FObjectValue: TJsonObject;
    FArrayValue: TJsonArray;

    function GetAsArray: TJsonArray;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Int64;
    function GetAsNumber: Extended;
    function GetAsObject: TJsonObject;
    function GetAsString: String;
    function GetIsNull: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetIsEmpty(const Value: Boolean);
    procedure SetAsInteger(const Value: Int64);
    procedure SetAsNumber(const Value: Extended);
    procedure SetAsString(const Value: String);
    procedure SetIsNull(const Value: Boolean);
    procedure SetAsArray(const Value: TJsonArray);
    procedure SetAsObject(const Value: TJsonObject);
    function GetIsEmpty: Boolean;
  protected
    procedure RaiseValueTypeError(const AsValueType: TJsonValueType);
  public
    constructor Create(AOwner: TJsonBase);
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TJsonBase); override;

    procedure Clear;
  public
    property ValueType: TJsonValueType read FValueType;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsNumber: Extended read GetAsNumber write SetAsNumber;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TJsonObject read GetAsObject write SetAsObject;
    property AsArray: TJsonArray read GetAsArray write SetAsArray;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsEmpty: Boolean read GetIsEmpty write SetIsEmpty;
  end;

  { TJsonArray }

  TJsonArray = class(TJsonBase)
  private
    FList: TList;
    function GetItems(Index: Integer): TJsonValue;
    function GetCount: Integer;
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TJsonBase); override;
    procedure Merge(Addition: TJsonArray);

    function Add: TJsonValue;
    function Insert(const Index: Integer): TJsonValue;

    function Put(const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Value: TJsonNull): TJsonValue; overload;
    function Put(const Value: Boolean): TJsonValue; overload;

    function Put(const Value: Int64): TJsonValue; overload;
    function Put(const Value: Extended): TJsonValue; overload;
    function Put(const Value: String): TJsonValue; overload;
    function Put(const Value: TJsonArray): TJsonValue; overload;
    function Put(const Value: TJsonObject): TJsonValue; overload;
    function Put(const Value: TJsonValue): TJsonValue; overload;

    procedure Delete(const Index: Integer);
    procedure Clear;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJsonValue read GetItems; default;
  end;

  TJsonPair = class(TJsonBase)
  private
    FName: String;
    FValue: TJsonValue;

    procedure SetName(const Value: String);
  public
    constructor Create(AOwner: TJsonBase; const AName: String = '');
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TJsonBase); override;
  public
    property Name: String read FName write SetName;
    property Value: TJsonValue read FValue;
  end;

  TJsonObject = class(TJsonBase)
  private
    FList: TList;
    FAutoAdd: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJsonPair;
    function GetValues(Name: String): TJsonValue;
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TJsonBase); override;
    procedure Merge(Addition: TJsonObject);

    function Add(const Name: String = ''): TJsonPair;
    function Insert(const Index: Integer; const Name: String = ''): TJsonPair;

    function Put(const Name: String; const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonNull): TJsonValue; overload;
    function Put(const Name: String; const Value: Boolean): TJsonValue; overload;

    function Put(const Name: String; const Value: Int64): TJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TJsonValue; overload;
    function Put(const Name: String; const Value: String): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonArray): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonObject): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonValue): TJsonValue; overload;
    function Put(const Value: TJsonPair): TJsonValue; overload;

    function Find(const Name: String): Integer;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJsonPair read GetItems;
    property Values[Name: String]: TJsonValue read GetValues; default;
    property AutoAdd: Boolean read FAutoAdd write FAutoAdd;
  end;

  TJson = class(TJsonBase)
  private
    FStructType: TJsonStructType;
    FJsonArray: TJsonArray;
    FJsonObject: TJsonObject;

    function GetCount: Integer;
    function GetJsonArray: TJsonArray;
    function GetJsonObject: TJsonObject;
    function GetValues(Name: String): TJsonValue;
  protected
    procedure CreateArrayIfNone;
    procedure CreateObjectIfNone;

    procedure RaiseIfNone;
    procedure RaiseIfNotArray;
    procedure RaiseIfNotObject;

    procedure CheckJsonArray;
    procedure CheckJsonObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TJsonBase); override;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;

    function Get(const Index: Integer): TJsonValue; overload; //for both
    function Get(const Name: String): TJsonValue; overload; //for JsonObject

    //for JsonArray
    function Put(const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Value: TJsonNull): TJsonValue; overload;
    function Put(const Value: Boolean): TJsonValue; overload;
    function Put(const Value: Int64): TJsonValue; overload;
    function Put(const Value: Extended): TJsonValue; overload;
    function Put(const Value: String): TJsonValue; overload;
    function Put(const Value: TJsonArray): TJsonValue; overload;
    function Put(const Value: TJsonObject): TJsonValue; overload;
    function Put(const Value: TJsonValue): TJsonValue; overload;
    function Put(const Value: TJson): TJsonValue; overload;

    //for JsonObject
    function Put(const Name: String; const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonNull): TJsonValue; overload;
    function Put(const Name: String; const Value: Boolean): TJsonValue; overload;
    function Put(const Name: String; const Value: Int64): TJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TJsonValue; overload;
    function Put(const Name: String; const Value: String): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonArray): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonObject): TJsonValue; overload;
    function Put(const Name: String; const Value: TJsonValue): TJsonValue; overload;
    function Put(const Name: String; const Value: TJson): TJsonValue; overload;
    function Put(const Value: TJsonPair): TJsonValue; overload;
  public
    property StructType: TJsonStructType read FStructType;
    property JsonObject: TJsonObject read GetJsonObject;
    property JsonArray: TJsonArray read GetJsonArray;

    property Count: Integer read GetCount;
    property Values[Name: String]: TJsonValue read GetValues; default; //for JsonObject
  end;

  function FixedFloatToStr(const Value: Extended): string;
  function FixedTryStrToFloat(const S: string; out Value: Extended): Boolean;
  function FixedStrToFloat(const S: string): Extended;

  Const
    GLB_JSON_STD_DECIMALSEPARATOR = '.';

  Var
    JsonsUtils_GLB_DECIMALSEPARATOR : Char;

implementation

function FixedFloatToStr(const Value: Extended): string;
var
  lS: string;
begin
  lS := FloatToStr(Value);
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := LS;
  end
  else
  begin
    Result := StringReplace( lS,
                             JsonsUtils_GLB_DECIMALSEPARATOR,
                             GLB_JSON_STD_DECIMALSEPARATOR,
                             [rfReplaceAll]);
  end;
end;

function FixedTryStrToFloat(const S: string; out Value: Extended): Boolean;
var
  FixedS: string;
begin
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := TryStrToFloat(S, Value);
  end
  else
  begin
    FixedS := StringReplace( S,
                             GLB_JSON_STD_DECIMALSEPARATOR,
                             JsonsUtils_GLB_DECIMALSEPARATOR,
                             [rfReplaceAll]);
    Result := TryStrToFloat(FixedS, Value);
  end;
end;

function FixedStrToFloat(const S: string): Extended;
var
  FixedS: string;
begin
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := StrToFloat(S);
  end
  else
  begin
    FixedS := StringReplace( S,
                             GLB_JSON_STD_DECIMALSEPARATOR,
                             JsonsUtils_GLB_DECIMALSEPARATOR,
                             [rfReplaceAll]);
    Result := StrToFloat(FixedS);
  end;
end;

{ TJsonBase }

function TJsonBase.AnalyzeJsonValueType(const S: String): TJsonValueType;
var
  lValor: string;
  Len: Integer;
  lInteger: Integer;
  lInt64: Int64;
  Number: Extended;
begin
  lValor := Trim(S);

  Result := jvNone;
  Len := Length(lValor);
  if Len >= 2 then
  begin
    if (lValor[1] = '{') and (lValor[Len] = '}') then Result := jvObject
    else if (lValor[1] = '[') and (lValor[Len] = ']') then Result := jvArray
    else if (lValor[1] = '"') and (lValor[Len] = '"') then Result := jvString
    else if SameText(lValor, 'null') then Result := jvNull
    else if SameText(lValor, 'true') or SameText(lValor, 'false') then Result := jvBoolean

    else if TryStrToInt(lValor, lInteger) or TryStrToInt64(lValor, lInt64) then Result := jvInteger

    else if FixedTryStrToFloat(lValor, Number) then Result := jvNumber;
  end
  else if TryStrToInt(lValor, lInteger) or TryStrToInt64(lValor, lInt64) then Result := jvInteger
  else if FixedTryStrToFloat(lValor, Number) then Result := jvNumber;
end;

constructor TJsonBase.Create(AOwner: TJsonBase);
begin
  FOwner := AOwner;
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;
end;

function TJsonBase.Decode(const S: String): String;
  function HexValue(C: Char): Byte;
  begin
    case C of
      '0'..'9':  Result :=  Byte(C) - Byte('0');
      'a'..'f':  Result := (Byte(C) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(C) - Byte('A')) + 10;
      else raise Exception.Create('Illegal hexadecimal characters "' + C + '"');
    end;
  end;
var
  I      : Integer;
  lChar  : {$ifdef fpc}UnicodeChar{$else}Char{$endif};
  lUbuf  : Integer;
  lStream: TStringStream;
begin
  lStream := TStringStream.Create();
  try
    I := 1;
    while (I <= {$ifdef fpc}UTF8Length(S){$else}Length(S){$endif}) do
    begin
      lChar := {$ifdef fpc}UnicodeString(S)[I]{$else}S[I]{$endif};
      Inc(I);
      if (lChar = '\') then
      begin
        lChar := {$ifdef fpc}UnicodeString(S)[I]{$else}S[I]{$endif};
        Inc(I);
        case lChar of
          'b': lStream.WriteString(#8);
          't': lStream.WriteString(#9);
          'n': lStream.WriteString(#10);
          'f': lStream.WriteString(#12);
          'r': lStream.WriteString(#13);
          'u': begin
                 if not TryStrToInt('$' + Copy({$ifdef fpc}String(S){$else}S{$endif}, I, 4), lUbuf) then
                   raise Exception.Create(format('Invalid unicode \u%s',[Copy({$ifdef fpc}UnicodeString(S){$else}S{$endif}, I, 4)]));

                 lStream.WriteString({$ifdef fpc}UnicodeChar{$else}WideChar{$endif}(lUbuf));
                 Inc(I, 4);
               end;
        else
          lStream.WriteString({$ifdef fpc}UnicodeChar(lChar){$else}lChar{$endif});
        end;
      end
      else
        lStream.WriteString({$ifdef fpc}UnicodeChar(lChar){$else}lChar{$endif});
    end;
    Result := lStream.DataString;
  finally
    lStream.Free;
  end;
end;

destructor TJsonBase.Destroy;
begin
  inherited Destroy;
end;

function TJsonBase.Encode(const S: String): String;
var
  I,
  lUnicodeValue: Integer;
  lChar        : {$ifdef fpc}UnicodeChar{$else}Char{$endif};
  lStream      : TStringStream;
begin
  lStream := TStringStream.Create;
  try
    for I := 1 to {$ifdef fpc}UTF8Length(S){$else}Length(S){$endif} do
    begin
      lChar := {$ifdef fpc}UnicodeString(S)[I]{$else}S[I]{$endif};
      case lChar of
        '"': lStream.WriteString('\' + lChar);
        '\': lStream.WriteString('\' + lChar);
        '/': lStream.WriteString('\' + lChar);
         #8: lStream.WriteString('\b');
         #9: lStream.WriteString('\t');
        #10: lStream.WriteString('\n');
        #12: lStream.WriteString('\f');
        #13: lStream.WriteString('\r');
      else
        if (lChar < WideChar(32)) or (lChar > WideChar(127)) then
        begin
          lStream.WriteString('\u');
          lUnicodeValue := Ord(lChar);
          lStream.WriteString( lowercase( IntToHex((lUnicodeValue and 61440) shr 12, 1) ));
          lStream.WriteString( lowercase( IntToHex((lUnicodeValue and 3840) shr 8, 1) ));
          lStream.WriteString( lowercase( IntToHex((lUnicodeValue and 240) shr 4, 1) ));
          lStream.WriteString( lowercase( IntToHex((lUnicodeValue and 15), 1)));
        end
        else
          lStream.WriteString(lChar);
      end;
    end;
    Result := lStream.DataString;
  finally
    lStream.Free;
  end;
end;

function TJsonBase.GetDecimalSeparator: Char;
  {$ifdef fpc}
var
  LFormatSettings: TFormatSettings;
  {$endif}
begin
  {$ifndef fpc}
  Result :=  FormatSettings.DecimalSeparator;
  {$else}
  LFormatSettings := DefaultFormatSettings;
  Result :=  LFormatSettings.DecimalSeparator;
  {$endif}
end;

function TJsonBase.GetOwner: TJsonBase;
begin
  Result := FOwner;
end;

function TJsonBase.GetOwnerName: String;
var
  TheOwner: TJsonBase;
begin
  Result := '';
  TheOwner := Owner;
  while True do
  begin
    if not Assigned(TheOwner) then Break
    else if TheOwner is TJsonPair then
    begin
      Result := (TheOwner as TJsonPair).Name;
      Break;
    end
    else TheOwner := TheOwner.Owner;
  end;
end;

function TJsonBase.IsJsonArray(const S: String): Boolean;
var
  lValor: string;
  Len: Integer;
begin
  lValor := Trim(S);
  Len := Length(lValor);
  Result := (Len >= 2) and (lValor[1] = '[') and (lValor[Len] = ']');
end;

function TJsonBase.IsJsonBoolean(const S: String): Boolean;
begin
  Result := SameText(lowercase(S), 'true') or SameText(lowercase(S), 'false');
end;

function TJsonBase.IsJsonNull(const S: String): Boolean;
begin
  Result := SameText(S, 'null');
end;

function TJsonBase.IsJsonInteger(const S: String): Boolean;
var
  lInteger: Integer;
  lInt64: Int64;
begin
  Result := (TryStrToInt(S, lInteger) or TryStrToInt64(S, lInt64));
end;

function TJsonBase.IsJsonNumber(const S: String): Boolean;
var
  Number: Extended;
begin
  Result := FixedTryStrToFloat(S, Number);
end;

function TJsonBase.IsJsonObject(const S: String): Boolean;
var
  lValor: string;
  Len: Integer;
begin
  lValor := Trim(S);
  Len := Length(lValor);
  Result := (Len >= 2) and (lValor[1] = '{') and (lValor[Len] = '}');
end;

function TJsonBase.IsJsonString(const S: String): Boolean;
var
  lValor: string;
  Len: Integer;
begin
  lValor := Trim(S);
  Len := Length(lValor);
  Result := (Len >= 2) and (lValor[1] = '"') and (lValor[Len] = '"');
end;

procedure TJsonBase.RaiseAssignError(Source: TJsonBase);
var
  SourceClassName: String;
begin
  if Source is TObject then SourceClassName := Source.ClassName
  else SourceClassName := 'nil';
  RaiseError(Format('assign error: %s to %s', [SourceClassName, ClassName]));
end;

procedure TJsonBase.RaiseError(const Msg: String);
var
  S: String;
begin
  S := Format('<%s>%s', [ClassName, Msg]);
  raise Exception.Create(S);
end;

procedure TJsonBase.RaiseParseError(const JsonString: String);
begin
  RaiseError(Format('"%s" parse error: %s', [GetOwnerName, JsonString]));
end;

procedure TJsonBase.Split(const S: String; const Delimiter: Char;
  Strings: TStrings);

  function IsPairBegin(C: Char): Boolean;
  begin
    Result := (C = '{') or (C = '[') or (C = '"');
  end;

  function GetPairEnd(C: Char): Char;
  begin
    case C of
      '{': Result := '}';
      '[': Result := ']';
      '"': Result := '"';
      else Result := #0;
    end;
  end;

  function MoveToPair(P: PChar): PChar;
  var
    PairBegin, PairEnd: Char;
    C: Char;
  begin
    PairBegin := P^;
    PairEnd := GetPairEnd(PairBegin);
    Result := P;
    while Result^ <> #0 do
    begin
      Inc(Result);
      C := Result^;
      if C = PairEnd then Break
      else if (PairBegin = '"') and (C = '\') then Inc(Result)
      else if (PairBegin <> '"') and IsPairBegin(C) then Result := MoveToPair(Result);
    end;
  end;

var
  PtrBegin, PtrEnd: PChar;
  C: Char;
  StrItem: String;
begin
  PtrBegin := PChar(S);
  PtrEnd := PtrBegin;
  while PtrEnd^ <> #0 do
  begin
    C := PtrEnd^;
    if C = Delimiter then
    begin
      StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
      Strings.Add(StrItem);
      PtrBegin := PtrEnd + 1;
      PtrEnd := PtrBegin;
      Continue;
    end
    else if IsPairBegin(C) then PtrEnd := MoveToPair(PtrEnd);
    Inc(PtrEnd);
  end;
  StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
  if StrItem <> '' then
    Strings.Add(StrItem);
end;

{ TJsonValue }

procedure TJsonValue.Assign(Source: TJsonBase);
var
  Src: TJsonValue;
begin
  Clear;
  if not(Source is TJsonValue) and not(Source is TJsonObject) and not(Source is TJsonArray) then
    RaiseAssignError(Source);
  if Source is TJsonObject then
  begin
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
    FObjectValue.Assign(Source);
  end
  else if Source is TJsonArray then
  begin
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
    FArrayValue.Assign(Source);
  end
  else if Source is TJsonValue then
  begin
    Src := Source as TJsonValue;
    FValueType := Src.FValueType;
    case FValueType of
      jvNone, jvNull: ;
      jvString: FStringValue := Src.FStringValue;
      jvInteger: FIntegerValue := Src.FIntegerValue;
      jvNumber: FNumberValue := Src.FNumberValue;
      jvBoolean: FBooleanValue := Src.FBooleanValue;
      jvObject:
        begin
          FObjectValue := TJsonObject.Create(Self);
          FObjectValue.Assign(Src.FObjectValue);
        end;
      jvArray:
        begin
          FArrayValue := TJsonArray.Create(Self);
          FArrayValue.Assign(Src.FArrayValue);
        end;
    end;
  end;
end;

procedure TJsonValue.Clear;
begin
  case FValueType of
    jvNone, jvNull: ;
    jvString: FStringValue := '';
    jvInteger: FIntegerValue := 0;
    jvNumber: FNumberValue := 0;
    jvBoolean: FBooleanValue := False;
    jvObject:
      begin
        FObjectValue.Free;
        FObjectValue := nil;
      end;
    jvArray:
      begin
        FArrayValue.Free;
        FArrayValue := nil;
      end;
  end;
  FValueType := jvNone;
end;

constructor TJsonValue.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FStringValue := '';
  FIntegerValue := 0;
  FNumberValue := 0;
  FBooleanValue := False;
  FObjectValue := nil;
  FArrayValue := nil;
  FValueType := jvNone;
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;
end;

destructor TJsonValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJsonValue.GetAsArray: TJsonArray;
begin
  if IsEmpty then
  begin
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
  end;
  if FValueType <> jvArray then RaiseValueTypeError(jvArray);
  Result := FArrayValue;
end;

function TJsonValue.GetAsBoolean: Boolean;
begin
  Result := False;
  case FValueType of
    jvNone, jvNull: Result := False;
    jvString: Result := SameText(lowercase(FStringValue), 'true');
    jvInteger: Result := (FIntegerValue <> 0);
    jvNumber: Result := (FNumberValue <> 0);
    jvBoolean: Result := FBooleanValue;
    jvObject, jvArray: RaiseValueTypeError(jvBoolean);
  end;
end;

function TJsonValue.GetAsInteger: Int64;
begin
  Result := 0;

  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := StrToInt64(FStringValue);
    jvInteger: Result := FIntegerValue;
    //jvNumber: Result := FNumberValue;
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TJsonValue.GetAsNumber: Extended;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := FixedStrToFloat(FStringValue);
    jvInteger: Result := FIntegerValue;
    jvNumber: Result := FNumberValue;
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TJsonValue.GetAsObject: TJsonObject;
begin
  if IsEmpty then
  begin
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
  end;
  if FValueType <> jvObject then RaiseValueTypeError(jvObject);
  Result := FObjectValue;
end;

function TJsonValue.GetAsString: String;
const
  BooleanStr: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := '';
    jvString: Result := FStringValue;
    jvInteger: Result := IntToStr( FIntegerValue );
    jvNumber: Result := FixedFloatToStr(FNumberValue);
    jvBoolean: Result := BooleanStr[FBooleanValue];
    jvObject, jvArray: RaiseValueTypeError(jvString);
  end;
end;

function TJsonValue.GetIsEmpty: Boolean;
begin
  Result := (FValueType = jvNone);
end;

function TJsonValue.GetIsNull: Boolean;
begin
  Result := (FValueType = jvNull);
end;

procedure TJsonValue.Parse(JsonString: String);
var
  lValor: string;
begin
  lValor := Trim(JsonString);

  Clear;
  FValueType := AnalyzeJsonValueType(lValor);
  case FValueType of
    jvNone: RaiseParseError(lValor);
    jvNull: ;
    jvString: FStringValue := Decode(Copy(lValor, 2, Length(lValor) - 2));
    jvInteger: FIntegerValue :=  StrToInt64(lValor);
    jvNumber: FNumberValue := FixedStrToFloat(lValor);
    jvBoolean: FBooleanValue := SameText(lValor, 'true');
    jvObject:
      begin
        FObjectValue := TJsonObject.Create(Self);
        FObjectValue.Parse(lValor);
      end;
    jvArray:
      begin
        FArrayValue := TJsonArray.Create(Self);
        FArrayValue.Parse(lValor);
      end;
  end;
end;

procedure TJsonValue.RaiseValueTypeError(const AsValueType: TJsonValueType);
const
  StrJsonValueType: array[TJsonValueType] of String = ('jvNone', 'jvNull', 'jvString', 'jvInteger', 'jvNumber', 'jvBoolean', 'jvObject', 'jvArray');
var
  S: String;
begin
  S := Format('"%s" value type error: %s to %s', [GetOwnerName, StrJsonValueType[FValueType], StrJsonValueType[AsValueType]]);
  RaiseError(S);
end;

procedure TJsonValue.SetAsArray(const Value: TJsonArray);
begin
  if FValueType <> jvArray then
  begin
    Clear;
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
  end;
  FArrayValue.Assign(Value);
end;

procedure TJsonValue.SetAsBoolean(const Value: Boolean);
begin
  if FValueType <> jvBoolean then
  begin
    Clear;
    FValueType := jvBoolean;
  end;
  FBooleanValue := Value;
end;
procedure TJsonValue.SetAsInteger(const Value: Int64);
begin
  if (FValueType <> jvInteger) then
  begin
    Clear;
    FValueType := jvInteger;
  end;
  FIntegerValue := Value;
end;

procedure TJsonValue.SetAsNumber(const Value: Extended);
begin
  if (FValueType <> jvNumber) then
  begin
    Clear;
    FValueType := jvNumber;
  end;
  FNumberValue := Value;
end;

procedure TJsonValue.SetAsObject(const Value: TJsonObject);
begin
  if FValueType <> jvObject then
  begin
    Clear;
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
  end;
  FObjectValue.Assign(Value);
end;

procedure TJsonValue.SetAsString(const Value: String);
begin
  if FValueType <> jvString then
  begin
    Clear;
    FValueType := jvString;
  end;
  FStringValue := Value;
end;

procedure TJsonValue.SetIsEmpty(const Value: Boolean);
const
  EmptyValueType: array[Boolean] of TJsonValueType = (jvNull, jvNone);
begin
  if FValueType <> EmptyValueType[Value] then
  begin
    Clear;
    FValueType := EmptyValueType[Value];
  end;
end;

procedure TJsonValue.SetIsNull(const Value: Boolean);
const
  NullValueType: array[Boolean] of TJsonValueType = (jvNone, jvNull);
begin
  if FValueType <> NullValueType[Value] then
  begin
    Clear;
    FValueType := NullValueType[Value];
  end;
end;

function TJsonValue.Stringify: String;
const
  StrBoolean: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := 'null';
    jvString: Result := '"' + Encode(FStringValue) + '"';
    jvInteger: Result := IntToStr(FIntegerValue);
    jvNumber: Result := FixedFloatToStr(FNumberValue);
    jvBoolean: Result := StrBoolean[FBooleanValue];
    jvObject: Result := FObjectValue.Stringify;
    jvArray: Result := FArrayValue.Stringify;
  end;
end;

{ TJsonArray }

function TJsonArray.Add: TJsonValue;
begin
  Result := TJsonValue.Create(Self);
  FList.Add(Result);
end;

procedure TJsonArray.Assign(Source: TJsonBase);
var
  Src: TJsonArray;
  I: Integer;
begin
  Clear;
  if not(Source is TJsonArray) then RaiseAssignError(Source);
  Src := Source as TJsonArray;
  for I := 0 to Src.Count - 1 do Add.Assign(Src[I]);
end;

procedure TJsonArray.Clear;
var
  I: Integer;
  Item: TJsonValue;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonValue(FList[I]);
    Item.Free;
  end;
  FList.Clear;
end;

constructor TJsonArray.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;
end;

procedure TJsonArray.Delete(const Index: Integer);
var
  Item: TJsonValue;
begin
  Item := TJsonValue(FList[Index]);
  Item.Free;
  FList.Delete(Index);
end;

destructor TJsonArray.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TJsonArray.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJsonArray.GetItems(Index: Integer): TJsonValue;
begin
  Result := TJsonValue(FList[Index]);
end;

function TJsonArray.Insert(const Index: Integer): TJsonValue;
begin
  Result := TJsonValue.Create(Self);
  FList.Insert(Index, Result);
end;

procedure TJsonArray.Merge(Addition: TJsonArray);
var
  I: Integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition[I]);
end;

procedure TJsonArray.Parse(JsonString: String);
var
  lValor: string;
  I: Integer;
  S: String;
  List: TStringList;
  Item: TJsonValue;
begin
  Clear;
  lValor := Trim(JsonString);
  if not IsJsonArray(lValor) then RaiseParseError(lValor);
  S := Trim(Copy(lValor, 2, Length(lValor) - 2));
  List := TStringList.Create;
  try
    Split(S, ',', List);
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    List.Free;
  end;
end;

function TJsonArray.Put(const Value: Boolean): TJsonValue;
begin
  Result := Add;
  Result.AsBoolean := Value;
end;

function TJsonArray.Put(const Value: TJsonEmpty): TJsonValue;
begin
  Result := Add;
  Result.IsEmpty := True;
end;

function TJsonArray.Put(const Value: TJsonNull): TJsonValue;
begin
  Result := Add;
  Result.IsNull := True;
end;
function TJsonArray.Put(const Value: Int64): TJsonValue;
begin
  Result := Add;
  Result.AsInteger := Value;
end;

function TJsonArray.Put(const Value: Extended): TJsonValue;
begin
  Result := Add;
  Result.AsNumber := Value;
end;

function TJsonArray.Put(const Value: TJsonObject): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Put(const Value: TJsonValue): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Put(const Value: String): TJsonValue;
begin
  Result := Add;
  Result.AsString := Value;
end;

function TJsonArray.Put(const Value: TJsonArray): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Stringify: String;
var
  I: Integer;
  Item: TJsonValue;
begin
  Result := '[';
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonValue(FList[I]);
    if I > 0 then Result := Result + ',';
    Result := Result + Item.Stringify;
  end;
  Result := Result + ']';
end;

{ TJsonPair }

procedure TJsonPair.Assign(Source: TJsonBase);
var
  Src: TJsonPair;
begin
  if not(Source is TJsonPair) then RaiseAssignError(Source);
  Src := Source as TJsonPair;
  FName := Src.FName;
  FValue.Assign(Src.FValue);
end;

constructor TJsonPair.Create(AOwner: TJsonBase; const AName: String);
begin
  inherited Create(AOwner);
  FName := AName;
  FValue := TJsonValue.Create(Self);
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;
end;

destructor TJsonPair.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

procedure TJsonPair.Parse(JsonString: String);
var
  lValor: string;
  List: TStringList;
  StrName: String;
begin
  lValor := Trim(JsonString);

  List := TStringList.Create;
  try
    Split(lValor, ':', List);
    if List.Count <> 2 then RaiseParseError(lValor);
    StrName := List[0];
    if not IsJsonString(StrName) then RaiseParseError(StrName);

    FName := Decode(Copy(StrName, 2, Length(StrName) - 2));

    FValue.Parse(List[1]);
  finally
    List.Free;
  end;
end;

procedure TJsonPair.SetName(const Value: String);
begin
  FName := Value;
end;

function TJsonPair.Stringify: String;
begin
  Result := Format('"%s":%s', [Encode(FName), FValue.Stringify]);
end;

{ TJsonObject }

function TJsonObject.Add(const Name: String): TJsonPair;
begin
  Result := TJsonPair.Create(Self, Name);
  FList.Add(Result);
end;

procedure TJsonObject.Assign(Source: TJsonBase);
var
  Src: TJsonObject;
  I: Integer;
begin
  Clear;
  if not(Source is TJsonObject) then RaiseAssignError(Source);
  Src := Source as TJsonObject;
  for I := 0 to Src.Count - 1 do Add.Assign(Src.Items[I]);
end;

procedure TJsonObject.Clear;
var
  I: Integer;
  Item: TJsonPair;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonPair(FList[I]);
    Item.Free;
  end;
  FList.Clear;
end;

constructor TJsonObject.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FAutoAdd := True;
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;
end;

procedure TJsonObject.Delete(const Index: Integer);
var
  Item: TJsonPair;
begin
  Item := TJsonPair(FList[Index]);
  Item.Free;
  FList.Delete(Index);
end;

procedure TJsonObject.Delete(const Name: String);
var
  Index: Integer;
begin
  Index := Find(Name);
  if Index < 0 then RaiseError(Format('"%s" not found', [Name]));
  Delete(Index);
end;

destructor TJsonObject.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TJsonObject.Find(const Name: String): Integer;
var
  I: Integer;
  Pair: TJsonPair;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
  begin
    Pair := TJsonPair(FList[I]);
    if SameText(Name, Pair.Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TJsonObject.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJsonObject.GetItems(Index: Integer): TJsonPair;
begin
  Result := TJsonPair(FList[Index]);
end;

function TJsonObject.GetValues(Name: String): TJsonValue;
var
  Index: Integer;
  Pair: TJsonPair;
begin
  Index := Find(Name);
  if (Index < 0) then
  begin
    if not FAutoAdd then
      RaiseError(Format('%s not found', [Name]));
    Pair := Add(Name);
  end
  else
    Pair := TJsonPair(FList[Index]);

  Result := Pair.Value;
end;

function TJsonObject.Insert(const Index: Integer;
  const Name: String): TJsonPair;
begin
  Result := TJsonPair.Create(Self, Name);
  FList.Insert(Index, Result);
end;

procedure TJsonObject.Merge(Addition: TJsonObject);
var
  I: Integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition.Items[I]);
end;

procedure TJsonObject.Parse(JsonString: String);
var
  lValor: string;
  I: Integer;
  S: String;
  List: TStringList;
  Item: TJsonPair;
begin
  Clear;
  lValor := Trim(JsonString);

  if not IsJsonObject(lValor) then RaiseParseError(lValor);
  S := Trim(Copy(lValor, 2, Length(lValor) - 2));
  List := TStringList.Create;
  try
    Split(S, ',', List);
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    List.Free;
  end;
end;

function TJsonObject.Put(const Name: String;
  const Value: Int64): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsInteger := Value;
end;

function TJsonObject.Put(const Name: String;
  const Value: Extended): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsNumber := Value;
end;

function TJsonObject.Put(const Name: String;
  const Value: Boolean): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsBoolean := Value;
end;

function TJsonObject.Put(const Name: String;
  const Value: TJsonEmpty): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsEmpty := True;
end;

function TJsonObject.Put(const Name: String;
  const Value: TJsonNull): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsNull := True;
end;

function TJsonObject.Put(const Name: String;
  const Value: TJsonValue): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Put(const Value: TJsonPair): TJsonValue;
var
  Pair: TJsonPair;
begin
  Pair := Add;
  Pair.Assign(Value);
  Result := Pair.Value;
end;

function TJsonObject.Put(const Name: String;
  const Value: TJsonObject): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Put(const Name, Value: String): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsString := Value;
end;

function TJsonObject.Put(const Name: String;
  const Value: TJsonArray): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Stringify: String;
var
  I: Integer;
  Item: TJsonPair;
begin
  Result := '{';
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonPair(FList[I]);
    if (I > 0) then
      Result := Result + ',';
    Result := Result + Item.Stringify;
  end;
  Result := Result + '}';
end;

{ TJson }

procedure TJson.Assign(Source: TJsonBase);
begin
  Clear;
  if Source is TJson then
  begin
    case (Source as TJson).FStructType of
      jsNone: ;
      jsArray:
        begin
          CreateArrayIfNone;
          FJsonArray.Assign((Source as TJson).FJsonArray);
        end;                       
      jsObject:
        begin
          CreateObjectIfNone;
          FJsonObject.Assign((Source as TJson).FJsonObject);
        end;
    end;
  end
  else if Source is TJsonArray then
  begin
    CreateArrayIfNone;
    FJsonArray.Assign(Source);
  end
  else if Source is TJsonObject then
  begin
    CreateObjectIfNone;
    FJsonObject.Assign(Source);
  end
  else if Source is TJsonValue then
  begin
    if (Source as TJsonValue).ValueType = jvArray then
    begin
      CreateArrayIfNone;
      FJsonArray.Assign((Source as TJsonValue).AsArray);
    end
    else if (Source as TJsonValue).ValueType = jvObject then
    begin
      CreateObjectIfNone;
      FJsonObject.Assign((Source as TJsonValue).AsObject);
    end
    else RaiseAssignError(Source);
  end
  else RaiseAssignError(Source);
end;

procedure TJson.CheckJsonArray;
begin
  CreateArrayIfNone;
  RaiseIfNotArray;
end;

procedure TJson.CheckJsonObject;
begin
  CreateObjectIfNone;
  RaiseIfNotObject;
end;

procedure TJson.Clear;
begin
  case FStructType of
    jsNone: ;
    jsArray:
      begin
        FJsonArray.Free;
        FJsonArray := nil;
      end;
    jsObject:
      begin
        FJsonObject.Free;
        FJsonObject := nil;
      end;
  end;
  FStructType := jsNone;
end;

constructor TJson.Create;
begin
  inherited Create(nil);
  FStructType := jsNone;
  FJsonArray := nil;
  FJsonObject := nil;
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;
end;

procedure TJson.CreateArrayIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsArray;
    FJsonArray := TJsonArray.Create(Self);
  end;
end;

procedure TJson.CreateObjectIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsObject;
    FJsonObject := TJsonObject.Create(Self);
  end;
end;

procedure TJson.Delete(const Index: Integer);
begin
  RaiseIfNone;
  case FStructType of
    jsArray: FJsonArray.Delete(Index);
    jsObject: FJsonObject.Delete(Index);
  end;
end;

procedure TJson.Delete(const Name: String);
begin
  RaiseIfNotObject;
  FJsonObject.Delete(Name);
end;

destructor TJson.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJson.Get(const Index: Integer): TJsonValue;
begin
  Result := nil;
  RaiseIfNone;
  case FStructType of
    jsArray: Result := FJsonArray.Items[Index];
    jsObject: Result := FJsonObject.Items[Index].Value;
  end;
end;

function TJson.Get(const Name: String): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Values[Name];
end;

function TJson.GetCount: Integer;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Count;
    jsObject: Result := FJsonObject.Count;
    else Result := 0;
  end;
end;

function TJson.GetJsonArray: TJsonArray;
begin
  CheckJsonArray;
  Result := FJsonArray;
end;

function TJson.GetJsonObject: TJsonObject;
begin
  CheckJsonObject;
  Result := FJsonObject;
end;

function TJson.GetValues(Name: String): TJsonValue;
begin
  Result := Get(Name);
end;

procedure TJson.Parse(JsonString: String);
var
  lValor: string;
begin
  Clear;
  lValor := Trim(JsonString);

  if IsJsonArray(lValor) then
  begin
    CreateArrayIfNone;
    FJsonArray.Parse(lValor);
  end
  else if IsJsonObject(lValor) then
  begin
    CreateObjectIfNone;
    FJsonObject.Parse(lValor);
  end
  else RaiseParseError(lValor);
end;

function TJson.Put(const Value: Int64): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: Extended): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: Boolean): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonEmpty): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonNull): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: String): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonValue): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonObject): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonArray): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Name: String; const Value: Int64): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: Extended): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: Boolean): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String;
  const Value: TJsonEmpty): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String; const Value: TJsonNull): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String;
  const Value: TJsonValue): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TJsonPair): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Value);
end;

function TJson.Put(const Name: String;
  const Value: TJsonObject): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name, Value: String): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: String;
  const Value: TJsonArray): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TJson): TJsonValue;
begin
  CheckJsonArray;
  case Value.FStructType of
    jsArray: Result := Put(Value.FJsonArray);
    jsObject: Result := Put(Value.FJsonObject);
    else Result := nil;
  end;
end;

function TJson.Put(const Name: String; const Value: TJson): TJsonValue;
begin
  CheckJsonObject;
  case Value.FStructType of
    jsArray: Result := Put(Name, Value.FJsonArray);
    jsObject: Result := Put(Name, Value.FJsonObject);
    else Result := nil;
  end;
end;

procedure TJson.RaiseIfNone;
begin
  if FStructType = jsNone then RaiseError('json struct type is jsNone');
end;

procedure TJson.RaiseIfNotArray;
begin
  if FStructType <> jsArray then RaiseError('json struct type is not jsArray');
end;

procedure TJson.RaiseIfNotObject;
begin
  if FStructType <> jsObject then RaiseError('json struct type is not jsObject');
end;

function TJson.Stringify: String;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Stringify;
    jsObject: Result := FJsonObject.Stringify;
    else Result := '';
  end;
end;

end.
