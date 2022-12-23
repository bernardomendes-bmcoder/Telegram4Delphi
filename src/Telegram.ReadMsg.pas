unit Telegram.ReadMsg;

interface

Uses System.Classes, System.SysUtils;

Type
TUnReadMsg = class(TThread)
 private
 FUnThreadProc: TProc;
 public
 constructor Create(const ThreadProc: TProc);
 procedure Execute; override;
end;

implementation

{ TUnReadMsg }
constructor TUnReadMsg.Create(const ThreadProc: TProc);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FUnThreadProc   := ThreadProc;
end;

procedure TUnReadMsg.Execute;
begin
  inherited;
  FUnThreadProc;
end;
end.
