object frmShowParams: TfrmShowParams
  Left = 1130
  Top = 670
  Width = 607
  Height = 294
  Caption = 'ProShow Parameters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    591
    255)
  PixelsPerInch = 96
  TextHeight = 13
  object leShowFile: TLabeledEdit
    Left = 16
    Top = 24
    Width = 481
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = 'Show Filename'
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 504
    Top = 22
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object rgDateFormat: TRadioGroup
    Left = 16
    Top = 72
    Width = 185
    Height = 73
    Caption = 'Date Format'
    ItemIndex = 0
    Items.Strings = (
      'YYYYMMDD'
      'MM/DD/YYYY')
    TabOrder = 2
  end
  object Button2: TButton
    Left = 504
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Button3: TButton
    Left = 416
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
  end
end
