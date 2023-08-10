unit udemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Telegram, Telegram.Consts, Telegram.Returns.Pooling,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Mask;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Button1: TButton;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    TabSheet3: TTabSheet;
    LabeledEdit3: TLabeledEdit;
    ComboBox1: TComboBox;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    Button2: TButton;
    Button4: TButton;
    Button3: TButton;
    Button5: TButton;
    Telegram4D1: TTelegram4D;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Telegram4D1MessagePooling(ARetMessage: TRespMessagePooling);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
 if  Telegram4D1.Config.TokenBot = '' then
 begin
  raise Exception.Create('insira o token bot');
  Exit;
 end;

 Telegram4D1.StartPooling;
 Label1.Caption := 'Status: Pooling Connected';
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
 if  Telegram4D1.Webhook.Host = '' then
 begin
  raise Exception.Create('insira o host');
  Exit;
 end;

 Telegram4D1.StartWebhook;
 Label1.Caption := 'Status: Webhook Connected';
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
 if  Telegram4D1.Config.TokenBot = '' then
 begin
  raise Exception.Create('insira o token bot');
  Exit;
 end;

 Telegram4D1.SendMessage(LabeledEdit2.Text,LabeledEdit1.Text);
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
 Telegram4D1.Webhook.Host := LabeledEdit4.Text;
 Telegram4D1.Webhook.Port := StrToInt(LabeledEdit5.Text);
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
 Telegram4D1.Config.TokenBot := LabeledEdit3.Text;
end;

procedure TForm3.ComboBox1CloseUp(Sender: TObject);
begin
 if ComboBox1.Text = 'Pooling' then
 Telegram4D1.Config.TypeReceive := rmPooling;

 if ComboBox1.Text = 'Webhook' then
 Telegram4D1.Config.TypeReceive := rmWebhook;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Telegram4D1.ShutDown;
end;

procedure TForm3.Telegram4D1MessagePooling(ARetMessage: TRespMessagePooling);
begin
  ARetMessage.RetType := rtNormal;
  Memo1.Lines.Add(ARetMessage.RetMessagePooling.Result[0].Message.Text);
end;

end.
