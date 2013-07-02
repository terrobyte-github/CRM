object fProjects: TfProjects
  Left = 283
  Top = 85
  BorderStyle = bsDialog
  Caption = #1055#1088#1086#1077#1082#1090#1099
  ClientHeight = 384
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 174
    Top = 8
    Width = 40
    Height = 13
    Caption = #1055#1088#1086#1077#1082#1090':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 174
    Top = 48
    Width = 57
    Height = 13
    Caption = #1043#1077#1085#1077#1088#1072#1094#1080#1103':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 174
    Top = 88
    Width = 63
    Height = 13
    Caption = #1041#1080#1073#1083#1080#1086#1090#1077#1082#1072':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 390
    Top = 88
    Width = 123
    Height = 13
    Caption = #1060#1091#1085#1082#1094#1080#1103'/ProgID/'#1050#1083#1072#1089#1089':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 390
    Top = 8
    Width = 68
    Height = 13
    Caption = #1041#1072#1079#1072' '#1076#1072#1085#1085#1099#1093':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 390
    Top = 48
    Width = 48
    Height = 13
    Caption = #1044#1088#1072#1081#1074#1077#1088':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 174
    Top = 200
    Width = 62
    Height = 13
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 8
    Top = 8
    Width = 90
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1087#1088#1086#1077#1082#1090#1086#1074':'
  end
  object Label9: TLabel
    Left = 174
    Top = 128
    Width = 76
    Height = 13
    Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 390
    Top = 128
    Width = 41
    Height = 13
    Caption = #1055#1072#1088#1086#1083#1100':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lNote: TLabel
    Left = 176
    Top = 328
    Width = 274
    Height = 13
    Caption = #1042' '#1087#1072#1088#1072#1084#1077#1090#1088#1072#1093' '#1084#1086#1078#1085#1086' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1084#1072#1082#1088#1086#1089#1099': %s '#1080' %s'
  end
  object Edit1: TEdit
    Tag = 1
    Left = 174
    Top = 24
    Width = 195
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object Edit2: TEdit
    Tag = 3
    Left = 174
    Top = 104
    Width = 195
    Height = 21
    TabOrder = 3
    OnExit = Edit1Exit
  end
  object Edit3: TEdit
    Tag = 4
    Left = 390
    Top = 104
    Width = 195
    Height = 21
    TabOrder = 8
    OnExit = Edit1Exit
  end
  object ComboBox1: TComboBox
    Tag = 2
    Left = 174
    Top = 64
    Width = 195
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnExit = Edit1Exit
    Items.Strings = (
      #1092#1091#1085#1082#1094#1080#1103
      'COM'
      #1082#1083#1072#1089#1089)
  end
  object CheckBox1: TCheckBox
    Tag = 7
    Left = 174
    Top = 176
    Width = 195
    Height = 17
    Caption = 'Windows '#1072#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103
    TabOrder = 5
    OnExit = Edit1Exit
  end
  object Edit4: TEdit
    Tag = 5
    Left = 390
    Top = 24
    Width = 195
    Height = 21
    TabOrder = 6
    OnExit = Edit1Exit
  end
  object Edit5: TEdit
    Tag = 6
    Left = 390
    Top = 64
    Width = 195
    Height = 21
    TabOrder = 7
    OnExit = Edit1Exit
  end
  object CheckBox2: TCheckBox
    Tag = 8
    Left = 390
    Top = 176
    Width = 195
    Height = 17
    Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
    TabOrder = 10
    OnClick = CheckBox2Click
    OnExit = Edit1Exit
  end
  object Memo1: TMemo
    Tag = 9
    Left = 174
    Top = 216
    Width = 411
    Height = 105
    ScrollBars = ssBoth
    TabOrder = 11
    OnExit = Edit1Exit
  end
  object Button2: TButton
    Left = 472
    Top = 352
    Width = 75
    Height = 25
    Caption = #1054#1082
    ModalResult = 1
    TabOrder = 15
  end
  object ListBox1: TListBox
    Left = 8
    Top = 24
    Width = 153
    Height = 321
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Button3: TButton
    Left = 48
    Top = 352
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 12
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 168
    Top = 352
    Width = 75
    Height = 25
    Caption = #1059#1076#1072#1083#1080#1090#1100
    TabOrder = 13
    OnClick = Button4Click
  end
  object Edit6: TEdit
    Tag = 10
    Left = 174
    Top = 144
    Width = 195
    Height = 21
    TabOrder = 4
    OnExit = Edit1Exit
  end
  object Edit7: TEdit
    Tag = 11
    Left = 390
    Top = 144
    Width = 195
    Height = 21
    PasswordChar = '*'
    TabOrder = 9
    OnExit = Edit1Exit
  end
  object Button1: TButton
    Left = 352
    Top = 352
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 14
  end
end
