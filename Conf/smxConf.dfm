object frmConf: TfrmConf
  Left = 279
  Top = 180
  Width = 754
  Height = 544
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
    Width = 746
    Height = 391
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 391
    Width = 746
    Height = 107
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 42
      Top = 53
      Width = 37
      Height = 13
      Caption = 'CfgID:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 390
      Top = 21
      Width = 48
      Height = 13
      Caption = #1055#1088#1086#1077#1082#1090':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 23
      Top = 21
      Width = 56
      Height = 13
      Caption = #1069#1083#1077#1084#1077#1085#1090':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 85
      Width = 71
      Height = 13
      Caption = #1048#1085#1090#1077#1088#1092#1077#1081#1089':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Edit2: TEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object Button6: TButton
      Left = 576
      Top = 73
      Width = 75
      Height = 25
      Caption = 'HotKey/Str'
      TabOrder = 1
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 664
      Top = 73
      Width = 75
      Height = 25
      Caption = #1050#1072#1088#1090#1080#1085#1082#1080
      TabOrder = 2
      OnClick = Button7Click
    end
    object Edit1: TEdit
      Left = 440
      Top = 77
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object Button4: TButton
      Left = 216
      Top = 41
      Width = 75
      Height = 25
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      TabOrder = 4
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 304
      Top = 41
      Width = 75
      Height = 25
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      TabOrder = 5
      OnClick = Button5Click
    end
    object ComboBox1: TComboBox
      Left = 440
      Top = 12
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      OnChange = ComboBox1Change
      Items.Strings = (
        '')
    end
    object Button1: TButton
      Left = 576
      Top = 9
      Width = 75
      Height = 25
      Caption = #1055#1088#1086#1077#1082#1090#1099'...'
      TabOrder = 7
      OnClick = Button1Click
    end
    object ComboBox2: TComboBox
      Left = 80
      Top = 12
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 8
      Text = 'cell'
      OnChange = ComboBox2Change
      Items.Strings = (
        'cell'
        'state')
    end
    object ComboBox3: TComboBox
      Left = 80
      Top = 76
      Width = 297
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      Items.Strings = (
        'cell'
        'state')
    end
    object CheckBox1: TCheckBox
      Left = 216
      Top = 16
      Width = 73
      Height = 17
      Caption = 'All'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
      OnClick = CheckBox1Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 498
    Width = 746
    Height = 19
    Panels = <
      item
        Text = #1055#1088#1086#1077#1082#1090':'
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
