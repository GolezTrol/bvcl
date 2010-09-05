object Form1: TForm1
  Left = 368
  Top = 278
  Width = 1088
  Height = 750
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = TURKISH_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel1: TBevel
    Left = 160
    Top = 48
    Width = 50
    Height = 50
  end
  object Button1: TButton
    Left = 304
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 304
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 224
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Left = 144
    Top = 240
  end
  object BigMRU1: TBigMRU
    Storage = BigMRURegistryStorage1
    Controller = BigMRUMenuController1
    Left = 336
    Top = 216
  end
  object MainMenu1: TMainMenu
    Left = 528
    Top = 360
    object ergerg1: TMenuItem
      Caption = 'ergerg'
    end
  end
  object BigMRUMenuController1: TBigMRUMenuController
    ParentMenuItem = ergerg1
    Left = 368
    Top = 216
  end
  object BigMRURegistryStorage1: TBigMRURegistryStorage
    KeyName = '\Software\BigVCL\MRU'
    ValueName = 'MRUTest'
    Left = 400
    Top = 216
  end
end
