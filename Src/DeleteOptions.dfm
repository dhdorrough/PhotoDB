object frmDeleteOptions: TfrmDeleteOptions
  Left = 278
  Top = 550
  Width = 546
  Height = 170
  Caption = 'Delete Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 13
    Width = 68
    Height = 13
    Caption = 'Log File Name'
  end
  object edtLogFileName: TEdit
    Left = 93
    Top = 11
    Width = 343
    Height = 21
    TabOrder = 0
    Text = 'edtLogFileName'
  end
  object btnDeleteOldest: TRadioButton
    Left = 64
    Top = 47
    Width = 136
    Height = 17
    Caption = 'Delete &Oldest Update'
    TabOrder = 1
    OnClick = btnDeleteOldestClick
  end
  object btnDeleteNewest: TRadioButton
    Left = 64
    Top = 72
    Width = 136
    Height = 17
    Caption = 'Delete &Newest Update'
    TabOrder = 2
    OnClick = btnDeleteNewestClick
  end
  object btnOk: TButton
    Left = 370
    Top = 103
    Width = 75
    Height = 25
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 452
    Top = 103
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnBrowse: TButton
    Left = 450
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 5
    OnClick = btnBrowseClick
  end
  object SaveDialog1: TSaveDialog
    Left = 508
    Top = 39
  end
end
