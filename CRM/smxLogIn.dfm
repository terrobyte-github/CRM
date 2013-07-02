object fLogIn: TfLogIn
  Left = 191
  Top = 124
  BorderStyle = bsDialog
  Caption = #1042#1093#1086#1076' '#1074' '#1087#1088#1086#1075#1088#1072#1084#1084#1091
  ClientHeight = 104
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 46
    Top = 76
    Width = 40
    Height = 13
    Caption = #1055#1088#1086#1077#1082#1090':'
  end
  object Label2: TLabel
    Left = 45
    Top = 44
    Width = 41
    Height = 13
    Caption = #1055#1072#1088#1086#1083#1100':'
  end
  object Label1: TLabel
    Left = 10
    Top = 12
    Width = 76
    Height = 13
    Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100':'
  end
  object cbProject: TComboBox
    Left = 88
    Top = 72
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbProjectChange
    OnKeyDown = eUserNameKeyDown
  end
  object ePassword: TEdit
    Left = 88
    Top = 40
    Width = 153
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    OnKeyDown = eUserNameKeyDown
  end
  object eUserName: TEdit
    Left = 88
    Top = 8
    Width = 153
    Height = 21
    TabOrder = 0
    OnKeyDown = eUserNameKeyDown
  end
  object bEnter: TButton
    Left = 248
    Top = 6
    Width = 75
    Height = 25
    Caption = #1042#1074#1086#1076
    ModalResult = 1
    TabOrder = 3
    OnKeyDown = eUserNameKeyDown
  end
  object bCancel: TButton
    Left = 248
    Top = 38
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 4
    OnKeyDown = eUserNameKeyDown
  end
  object bProjects: TButton
    Left = 248
    Top = 70
    Width = 75
    Height = 25
    Caption = #1055#1088#1086#1077#1082#1090#1099'...'
    TabOrder = 5
    OnClick = bProjectsClick
    OnKeyDown = eUserNameKeyDown
  end
end
