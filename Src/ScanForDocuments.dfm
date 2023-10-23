object frmScanForDocuments: TfrmScanForDocuments
  Left = 828
  Top = 419
  Width = 737
  Height = 346
  Caption = 'Scan for Documents/Photos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    721
    307)
  PixelsPerInch = 96
  TextHeight = 14
  object lblProcessed: TLabel
    Left = 15
    Top = 239
    Width = 52
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'Processed'
  end
  object Label5: TLabel
    Left = 15
    Top = 224
    Width = 64
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Processed:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblThumbCount: TLabel
    Left = 15
    Top = 253
    Width = 60
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'ThumbCount'
  end
  object lblRecsUpdated: TLabel
    Left = 15
    Top = 268
    Width = 65
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'RecsUpdated'
  end
  object lblElapsedTime: TLabel
    Left = 16
    Top = 283
    Width = 60
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'ElapsedTime'
  end
  object lblFileOptions: TLabel
    Left = 128
    Top = 130
    Width = 579
    Height = 94
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = '(NONE)'
    WordWrap = True
  end
  object lblError: TLabel
    Left = 678
    Top = 248
    Width = 34
    Height = 14
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'lblError'
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 68
    Height = 14
    Caption = 'Starting folder'
  end
  object btnBegin: TButton
    Left = 553
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Begin'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnBeginClick
  end
  object edtFolder: TEdit
    Left = 88
    Top = 13
    Width = 581
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'C:\My Documents\My Pictures'
    OnChange = edtFolderChange
    OnExit = edtFolderExit
  end
  object btnBrowse: TButton
    Left = 679
    Top = 13
    Width = 30
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnBrowseClick
  end
  object cbIsRootDir: TCheckBox
    Left = 17
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Is root directory'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnViewLog: TButton
    Left = 466
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'View Log File'
    Enabled = False
    TabOrder = 4
    OnClick = btnViewLogClick
  end
  object btnScanningOptions: TButton
    Left = 16
    Top = 129
    Width = 105
    Height = 25
    Caption = 'Scanning Options'
    TabOrder = 5
    OnClick = btnScanningOptionsClick
  end
  object cbProcessSubFolders: TCheckBox
    Left = 17
    Top = 86
    Width = 119
    Height = 17
    Caption = 'Process Sub-folders'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object cbIncludeEmpty: TCheckBox
    Left = 16
    Top = 108
    Width = 257
    Height = 17
    Caption = 'Include folder in DB even if contains no media'
    TabOrder = 7
  end
  object pnlDBStuff: TPanel
    Left = 176
    Top = 48
    Width = 522
    Height = 60
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvLowered
    TabOrder = 8
    object Label6: TLabel
      Left = 191
      Top = 25
      Width = 24
      Height = 14
      Caption = 'days'
    end
    object rbUpdateRecentOnly: TRadioButton
      Left = 27
      Top = 23
      Width = 137
      Height = 17
      Caption = 'Files updated within last'
      TabOrder = 0
    end
    object rbAllFiles: TRadioButton
      Left = 27
      Top = 40
      Width = 113
      Height = 14
      Caption = 'All Files'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object ovcDayCount: TOvcNumericField
      Left = 163
      Top = 21
      Width = 23
      Height = 22
      Cursor = crIBeam
      DataType = nftLongInt
      CaretOvr.Shape = csBlock
      Controller = OvcController1
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      PictureMask = 'iii'
      TabOrder = 2
      RangeHigh = {E7030000000000000000}
      RangeLow = {00000000000000000000}
    end
    object cbUpdateDB: TCheckBox
      Left = 12
      Top = 7
      Width = 97
      Height = 17
      Caption = 'Update DB'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbUpdateDBClick
    end
  end
  object btnCancel: TButton
    Left = 640
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 9
    OnClick = btnCancelClick
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
    Top = 32
  end
end
