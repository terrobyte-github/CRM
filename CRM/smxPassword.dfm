object fPassword: TfPassword
  Left = 212
  Top = 148
  BorderStyle = bsDialog
  Caption = #1042#1074#1086#1076' '#1087#1072#1088#1086#1083#1103
  ClientHeight = 101
  ClientWidth = 242
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
    Left = 10
    Top = 12
    Width = 41
    Height = 13
    Caption = #1055#1072#1088#1086#1083#1100':'
  end
  object ePassword: TEdit
    Left = 10
    Top = 29
    Width = 223
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 32
    Top = 64
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
  end
  object Button2: TButton
    Left = 136
    Top = 64
    Width = 75
    Height = 25
    Caption = #1054#1082
    ModalResult = 1
    TabOrder = 2
  end
end
