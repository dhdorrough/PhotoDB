object frmScanForDuplicatesOptions: TfrmScanForDuplicatesOptions
  Left = 968
  Top = 338
  Width = 465
  Height = 376
  Caption = 'Scan For Duplicates Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    457
    345)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 131
    Height = 13
    Caption = 'Sort Records based on'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 68
    Width = 110
    Height = 13
    Caption = 'Required for Match'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 215
    Top = 142
    Width = 8
    Height = 13
    Caption = '%'
  end
  object btnOk: TButton
    Left = 276
    Top = 302
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 362
    Top = 302
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbDeleteDuplicateRecords: TCheckBox
    Left = 24
    Top = 174
    Width = 177
    Height = 17
    Anchors = [akBottom]
    Caption = 'Delete Duplicate Records'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = cbDeleteDuplicateRecordsClick
  end
  object cbComparisonMethod: TComboBox
    Left = 24
    Top = 35
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object rbDeleteOldest: TRadioButton
    Left = 40
    Top = 197
    Width = 136
    Height = 17
    Caption = 'Delete &Oldest Update'
    Enabled = False
    TabOrder = 4
  end
  object rbDeleteNewest: TRadioButton
    Left = 40
    Top = 217
    Width = 136
    Height = 17
    Caption = 'Delete &Newest Update'
    Enabled = False
    TabOrder = 5
  end
  object rbDeleteMatchingPattern: TRadioButton
    Left = 40
    Top = 278
    Width = 161
    Height = 17
    Caption = 'Delete Matching Pattern'
    Enabled = False
    TabOrder = 8
  end
  object leFileName: TLabeledEdit
    Left = 200
    Top = 251
    Width = 121
    Height = 21
    EditLabel.Width = 79
    EditLabel.Height = 13
    EditLabel.Caption = 'Filename Pattern'
    TabOrder = 9
  end
  object cbSameDate: TCheckBox
    Left = 48
    Top = 92
    Width = 97
    Height = 17
    Caption = 'Same Date'
    TabOrder = 10
    OnClick = cbSameDateClick
  end
  object cbSameFileName: TCheckBox
    Left = 48
    Top = 116
    Width = 97
    Height = 17
    Caption = 'Same File Name'
    TabOrder = 11
    OnClick = cbSameFileNameClick
  end
  object cbSameFileSize: TCheckBox
    Left = 48
    Top = 140
    Width = 97
    Height = 17
    Caption = 'Same File Size'
    TabOrder = 12
    OnClick = cbSameFileSizeClick
  end
  object ovcTolerance: TOvcNumericField
    Left = 160
    Top = 137
    Width = 51
    Height = 21
    Cursor = crIBeam
    Hint = '0-25%'
    DataType = nftDouble
    CaretOvr.Shape = csBlock
    Controller = OvcController1
    EFColors.Disabled.BackColor = clWindow
    EFColors.Disabled.TextColor = clGrayText
    EFColors.Error.BackColor = clRed
    EFColors.Error.TextColor = clBlack
    EFColors.Highlight.BackColor = clHighlight
    EFColors.Highlight.TextColor = clHighlightText
    Enabled = False
    Options = [efoArrowIncDec]
    PictureMask = '###.###'
    TabOrder = 13
    RangeHigh = {73B2DBB9838916F2FE43}
    RangeLow = {73B2DBB9838916F2FEC3}
  end
  object rbEarliestAdd: TRadioButton
    Left = 40
    Top = 237
    Width = 113
    Height = 17
    Caption = 'Earliest Add'
    Enabled = False
    TabOrder = 6
  end
  object rbLatestAdd: TRadioButton
    Left = 40
    Top = 257
    Width = 113
    Height = 17
    Caption = 'Latest Add'
    Enabled = False
    TabOrder = 7
  end
  object cbDisplayPath: TCheckBox
    Left = 216
    Top = 40
    Width = 153
    Height = 17
    Caption = 'Display PATH\FileName'
    TabOrder = 14
  end
  object rbForNonExistantFile: TRadioButton
    Left = 40
    Top = 300
    Width = 113
    Height = 17
    Caption = 'for Non-Existant file'
    Enabled = False
    TabOrder = 15
  end
  object SaveDialog1: TSaveDialog
    Left = 400
    Top = 24
  end
  object OvcController1: TOvcController
    EntryCommands.TableList = (
      'Default'
      True
      ()
      'WordStar'
      False
      ()
      'Grid'
      False
      ())
    Epoch = 2000
    Left = 216
    Top = 176
  end
end
