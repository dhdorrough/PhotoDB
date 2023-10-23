object frmYesNoDontAskAgain: TfrmYesNoDontAskAgain
  Left = 668
  Top = 398
  BorderStyle = bsDialog
  Caption = 'Confirm'
  ClientHeight = 199
  ClientWidth = 326
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
  object lblPrompt: TLabel
    Left = 28
    Top = 8
    Width = 265
    Height = 53
    AutoSize = False
    Caption = 'lblPrompt'
    WordWrap = True
  end
  object btnYes: TButton
    Left = 32
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Yes'
    ModalResult = 6
    TabOrder = 0
  end
  object btnNo: TButton
    Left = 128
    Top = 72
    Width = 75
    Height = 25
    Caption = 'No'
    Default = True
    ModalResult = 7
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 216
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnNoAndDontAskAgain: TButton
    Left = 32
    Top = 112
    Width = 257
    Height = 25
    Caption = 'NO to all'
    ModalResult = 9
    TabOrder = 3
  end
  object btnYesAndDontAskAgain: TButton
    Left = 31
    Top = 144
    Width = 257
    Height = 25
    Caption = 'YES to all'
    ModalResult = 10
    TabOrder = 4
  end
end
