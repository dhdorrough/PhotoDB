object frmReplaceFieldsInSelectedRecords: TfrmReplaceFieldsInSelectedRecords
  Left = 734
  Top = 306
  Width = 520
  Height = 308
  Caption = 'Replace Field in Selected Records'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    512
    277)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 101
    Height = 13
    Caption = 'Select Field to Modify'
  end
  object Label2: TLabel
    Left = 24
    Top = 237
    Width = 68
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Replace With '
  end
  object lbFieldNames: TListBox
    Left = 152
    Top = 16
    Width = 333
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbFieldNamesClick
  end
  object btnExpressionParser: TButton
    Left = 104
    Top = 231
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Expression Builder'
    TabOrder = 1
    OnClick = btnExpressionParserClick
  end
  object btnBegin: TButton
    Left = 327
    Top = 231
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Begin'
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 416
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
