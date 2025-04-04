object frmReplaceDialog: TfrmReplaceDialog
  Left = 642
  Top = 367
  Width = 385
  Height = 241
  Caption = 'Replace Dialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    369
    202)
  PixelsPerInch = 96
  TextHeight = 13
  object leFindText: TLabeledEdit
    Left = 32
    Top = 32
    Width = 317
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Find'
    TabOrder = 0
  end
  object leReplaceText: TLabeledEdit
    Left = 32
    Top = 72
    Width = 317
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'Replace With'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 232
    Top = 157
    Width = 115
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Replace All'
    ModalResult = 1
    TabOrder = 3
  end
  object cbConfirmEachRecord: TCheckBox
    Left = 32
    Top = 104
    Width = 129
    Height = 17
    Caption = 'Confirm Each Record'
    TabOrder = 2
  end
end
