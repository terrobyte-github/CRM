object frmAddCell: TfrmAddCell
  Left = 284
  Top = 143
  Width = 306
  Height = 207
  Caption = #1044#1086#1073#1072#1074#1083#1077#1085#1080#1077
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 30
    Height = 13
    Caption = 'CfgID:'
  end
  object Label2: TLabel
    Left = 8
    Top = 88
    Width = 74
    Height = 13
    Caption = #1058#1080#1087' '#1101#1083#1077#1084#1077#1085#1090#1072':'
  end
  object Label3: TLabel
    Left = 8
    Top = 48
    Width = 25
    Height = 13
    Caption = #1048#1084#1103':'
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 161
    Height = 21
    TabOrder = 0
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 104
    Width = 161
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object RadioGroup1: TRadioGroup
    Left = 176
    Top = 16
    Width = 105
    Height = 65
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    ItemIndex = 0
    Items.Strings = (
      #1058#1080#1087
      #1069#1083#1077#1084#1077#1085#1090' '#1090#1080#1087#1072)
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object Button1: TButton
    Left = 40
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 176
    Top = 136
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 8
    Top = 64
    Width = 161
    Height = 21
    TabOrder = 5
  end
end
