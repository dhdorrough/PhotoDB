object frmDeletionConfirmation: TfrmDeletionConfirmation
  Left = 780
  Top = 371
  Width = 436
  Height = 190
  Caption = 'Confirm Delete'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    428
    159)
  PixelsPerInch = 96
  TextHeight = 14
  object lblFileName: TLabel
    Left = 16
    Top = 16
    Width = 393
    Height = 37
    AutoSize = False
    Caption = 'FileName'
    WordWrap = True
  end
  object cbDeleteTheRecord: TCheckBox
    Left = 16
    Top = 64
    Width = 129
    Height = 17
    Caption = 'Delete the &Record'
    TabOrder = 0
  end
  object cbDeleteFile: TCheckBox
    Left = 16
    Top = 88
    Width = 145
    Height = 17
    Caption = 'Delete &Associated File'
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 242
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 330
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
