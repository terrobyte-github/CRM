object frmConf: TfrmConf
  Left = 279
  Top = 180
  Width = 653
  Height = 537
  Caption = #1050#1086#1085#1092#1080#1075#1091#1088#1072#1090#1086#1088
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 645
    Height = 419
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 419
    Width = 645
    Height = 72
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 21
      Width = 18
      Height = 13
      Caption = 'ID:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 344
      Top = 21
      Width = 48
      Height = 13
      Caption = #1057#1077#1088#1074#1077#1088':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Edit2: TEdit
      Left = 32
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object Button6: TButton
      Left = 168
      Top = 41
      Width = 75
      Height = 25
      Caption = 'HotKey/Str'
      TabOrder = 1
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 256
      Top = 41
      Width = 75
      Height = 25
      Caption = #1050#1072#1088#1090#1080#1085#1082#1080
      TabOrder = 2
      OnClick = Button7Click
    end
    object Edit1: TEdit
      Left = 32
      Top = 45
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object Button4: TButton
      Left = 168
      Top = 9
      Width = 75
      Height = 25
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      TabOrder = 4
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 256
      Top = 9
      Width = 75
      Height = 25
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      TabOrder = 5
      OnClick = Button5Click
    end
    object ComboBox1: TComboBox
      Left = 400
      Top = 13
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 6
      Items.Strings = (
        'localhost'
        'erop')
    end
    object Button1: TButton
      Left = 560
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Con/Discon'
      TabOrder = 7
      OnClick = Button1Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 491
    Width = 645
    Height = 19
    Panels = <
      item
        Text = #1057#1077#1088#1074#1077#1088':'
        Width = 150
      end
      item
        Text = #1057#1090#1072#1090#1091#1089':'
        Width = 150
      end
      item
        Text = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100':'
        Width = 200
      end>
  end
end
