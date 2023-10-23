object frmExport2: TfrmExport2
  Left = 593
  Top = 376
  Width = 576
  Height = 232
  Caption = 'Export Selected Records'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    568
    201)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus01: TLabel
    Left = 24
    Top = 106
    Width = 52
    Height = 13
    Caption = 'lblStatus01'
  end
  object lblStatus02: TLabel
    Left = 24
    Top = 130
    Width = 52
    Height = 13
    Caption = 'lblStatus02'
  end
  object btnBrowse: TButton
    Left = 446
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 0
    OnClick = btnBrowseClick
  end
  object leFileName: TLabeledEdit
    Left = 24
    Top = 24
    Width = 415
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 82
    EditLabel.Height = 13
    EditLabel.Caption = 'Output File Name'
    TabOrder = 1
    Text = 'C:\temp\Export.mdb'
  end
  object btnBegin: TButton
    Left = 374
    Top = 164
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Begin'
    Default = True
    TabOrder = 2
    OnClick = btnBeginClick
  end
  object btnCancel: TButton
    Left = 462
    Top = 164
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cbOverwriteExisting: TCheckBox
    Left = 184
    Top = 56
    Width = 169
    Height = 17
    Caption = 'Overwrite Existing Documents'
    Enabled = False
    TabOrder = 5
  end
  object cbUpdateRecords: TCheckBox
    Left = 24
    Top = 80
    Width = 145
    Height = 17
    Caption = 'Update Records'
    TabOrder = 6
  end
  object cbCopyDocuments: TCheckBox
    Left = 24
    Top = 56
    Width = 113
    Height = 17
    Caption = 'Copy Documents'
    TabOrder = 4
    OnClick = cbCopyDocumentsClick
  end
end
