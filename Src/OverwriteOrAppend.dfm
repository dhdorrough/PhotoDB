object frmOverWriteOrAppend: TfrmOverWriteOrAppend
  Left = 772
  Top = 437
  Width = 488
  Height = 253
  Caption = 'OverWrite Or Append'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    472
    215)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCaption: TLabel
    Left = 16
    Top = 8
    Width = 84
    Height = 13
    Caption = 'File Already Exists'
    WordWrap = True
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 81
    Width = 439
    Height = 96
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'How to handle'
    Items.Strings = (
      'Overwrite'
      'Append')
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 360
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 272
    Top = 184
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
  end
end
