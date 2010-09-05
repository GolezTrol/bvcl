object BigProgressForm: TBigProgressForm
  Left = 438
  Top = 187
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'BigProgressForm'
  ClientHeight = 68
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    392
    68)
  PixelsPerInch = 96
  TextHeight = 13
  object InfoLabel: TLabel
    Left = 8
    Top = 48
    Width = 377
    Height = 13
    AutoSize = False
    Caption = 'InfoLabel'
  end
  object TitleLabel: TLabel
    Left = 8
    Top = 8
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 24
    Width = 377
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
