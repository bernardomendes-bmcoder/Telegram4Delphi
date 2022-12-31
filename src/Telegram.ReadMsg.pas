unit Telegram.ReadMsg;

interface

Uses System.Classes, System.SysUtils;

Type
TUnReadMsg = class(TThread)
 private
 FUnThreadProc: TProc;
 FSleepTime   : Integer;
 public
 constructor Create(AThreadProc: TProc; const ASleepTime: Integer = 1000);
 protected
 procedure Execute; override;
end;

implementation

{ TUnReadMsg }
constructor TUnReadMsg.Create(AThreadProc: TProc; const ASleepTime: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Priority        := tpLower;
  FUnThreadProc   := AThreadProc;
  FSleepTime      := ASleepTime;
end;

procedure TUnReadMsg.Execute;
begin
  inherited;
 While not Terminated do
 begin
  Sleep(FSleepTime);
  FUnThreadProc;
 end;
end;
end.
