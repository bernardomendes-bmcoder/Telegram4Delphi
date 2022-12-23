object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Demo Telegram4Delphi'
  ClientHeight = 535
  ClientWidth = 1014
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 502
    Width = 1014
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    Color = 15772444
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 424
    ExplicitWidth = 740
    object Label1: TLabel
      Left = 8
      Top = 7
      Width = 58
      Height = 19
      Caption = 'Status:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1014
    Height = 502
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Message'
      ExplicitLeft = 8
      ExplicitTop = 22
      object Label2: TLabel
        Left = 66
        Top = 21
        Width = 114
        Height = 13
        Caption = 'Received Messages:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Memo1: TMemo
        Left = 3
        Top = 40
        Width = 241
        Height = 337
        TabOrder = 0
      end
      object Button1: TButton
        Left = 54
        Top = 383
        Width = 129
        Height = 25
        Caption = 'Start Pooling'
        TabOrder = 1
        OnClick = Button1Click
      end
      object LabeledEdit1: TLabeledEdit
        Left = 280
        Top = 112
        Width = 169
        Height = 21
        EditLabel.Width = 42
        EditLabel.Height = 13
        EditLabel.Caption = 'Message'
        TabOrder = 2
      end
      object LabeledEdit2: TLabeledEdit
        Left = 280
        Top = 58
        Width = 169
        Height = 21
        EditLabel.Width = 34
        EditLabel.Height = 13
        EditLabel.Caption = 'ChatID'
        TabOrder = 3
      end
      object Button2: TButton
        Left = 54
        Top = 414
        Width = 129
        Height = 25
        Caption = 'Start Webhook'
        TabOrder = 4
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 315
        Top = 147
        Width = 97
        Height = 25
        Caption = 'Send Message'
        TabOrder = 5
        OnClick = Button3Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Config'
      ImageIndex = 1
      ExplicitLeft = 8
      ExplicitTop = 22
      object LabeledEdit3: TLabeledEdit
        Left = 16
        Top = 32
        Width = 361
        Height = 21
        EditLabel.Width = 54
        EditLabel.Height = 13
        EditLabel.Caption = 'TokenBot'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -11
        EditLabel.Font.Name = 'Tahoma'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = '2049443060:AAEHxhr0ByYj2b4ecyXobkAjt92qfCu8LvY'
      end
      object ComboBox1: TComboBox
        Left = 16
        Top = 80
        Width = 165
        Height = 21
        ItemIndex = 0
        TabOrder = 1
        Text = 'Pooling'
        OnCloseUp = ComboBox1CloseUp
        Items.Strings = (
          'Pooling'
          'Webhook')
      end
      object Button5: TButton
        Left = 216
        Top = 78
        Width = 121
        Height = 25
        Caption = 'Carregar configura'#231#227'o'
        TabOrder = 2
        OnClick = Button5Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Webhook'
      ImageIndex = 2
      object LabeledEdit4: TLabeledEdit
        Left = 16
        Top = 32
        Width = 233
        Height = 21
        EditLabel.Width = 26
        EditLabel.Height = 13
        EditLabel.Caption = 'Host'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -11
        EditLabel.Font.Name = 'Tahoma'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        TabOrder = 0
      end
      object LabeledEdit5: TLabeledEdit
        Left = 16
        Top = 96
        Width = 73
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Port'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -11
        EditLabel.Font.Name = 'Tahoma'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        TabOrder = 1
        Text = '9000'
      end
      object Button4: TButton
        Left = 112
        Top = 94
        Width = 121
        Height = 25
        Caption = 'Carregar configura'#231#227'o'
        TabOrder = 2
        OnClick = Button4Click
      end
    end
  end
  object Telegram4D: TTelegram4D
    Config.TypeReceive = rmPooling
    Webhook.Host = 'http://127.0.0.1:9000'
    Webhook.Port = 9000
    OnMessagePooling = Telegram4DMessagePooling
    Left = 512
    Top = 216
  end
end
