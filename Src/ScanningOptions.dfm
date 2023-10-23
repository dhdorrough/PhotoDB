object frmScanningOptions: TfrmScanningOptions
  Left = 881
  Top = 243
  BorderStyle = bsDialog
  Caption = 'Scanning Options'
  ClientHeight = 506
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    531
    506)
  PixelsPerInch = 96
  TextHeight = 14
  object Label2: TLabel
    Left = 273
    Top = 12
    Width = 58
    Height = 13
    Caption = 'Other Info'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnOk: TButton
    Left = 358
    Top = 469
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 446
    Top = 469
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlLocation: TGroupBox
    Left = 23
    Top = 165
    Width = 449
    Height = 224
    Caption = 'Location Info'
    TabOrder = 7
    object cbSaveUpdatedTrackLog: TCheckBox
      Left = 23
      Top = 57
      Width = 161
      Height = 14
      Caption = 'Save Updated Track Log'
      Enabled = False
      TabOrder = 2
    end
    object cbScanSelectedSubFolders: TCheckBox
      Left = 23
      Top = 74
      Width = 177
      Height = 17
      Caption = 'Scan selected GPX Sub Folders'
      Color = clBtnFace
      Enabled = False
      ParentColor = False
      TabOrder = 3
      OnClick = cbScanSelectedSubFoldersClick
    end
    object leSelectedSubFolders: TLabeledEdit
      Left = 23
      Top = 111
      Width = 401
      Height = 22
      EditLabel.Width = 104
      EditLabel.Height = 14
      EditLabel.Caption = 'Selected Sub-Folders'
      Enabled = False
      TabOrder = 4
    end
    object leMinutesToAdd: TLabeledEdit
      Left = 31
      Top = 150
      Width = 49
      Height = 22
      EditLabel.Width = 162
      EditLabel.Height = 14
      EditLabel.Caption = 'Minutes to add to Photo Date/Time'
      Enabled = False
      TabOrder = 5
      Text = '0'
    end
    object leDefaultArea: TLabeledEdit
      Left = 23
      Top = 192
      Width = 306
      Height = 22
      EditLabel.Width = 127
      EditLabel.Height = 14
      EditLabel.Caption = 'Default Area for Locations'
      Enabled = False
      TabOrder = 6
    end
    object cbUseSavedTracksLogFile: TCheckBox
      Left = 22
      Top = 18
      Width = 209
      Height = 17
      Caption = 'Use Saved Tracks Log File'
      TabOrder = 0
      OnClick = cbUseSavedTracksLogFileClick
    end
    object cbUseGPXLogsForLocation: TCheckBox
      Left = 22
      Top = 37
      Width = 177
      Height = 17
      Caption = 'Use GPX logs for location'
      TabOrder = 1
      OnClick = cbUseGPXLogsForLocationClick
    end
    object leDefaultState: TLabeledEdit
      Left = 339
      Top = 192
      Width = 41
      Height = 22
      EditLabel.Width = 62
      EditLabel.Height = 14
      EditLabel.Caption = 'Default State'
      Enabled = False
      MaxLength = 2
      TabOrder = 7
    end
  end
  object cbUseExifDateForPhotoDate: TCheckBox
    Left = 281
    Top = 36
    Width = 185
    Height = 17
    Caption = 'Use EXIF for PhotoDate/Lat/Long'
    TabOrder = 2
    OnClick = cbUseExifDateForPhotoDateClick
  end
  object cbUseFileCreationDateForPhotoDate: TCheckBox
    Left = 281
    Top = 88
    Width = 220
    Height = 17
    Caption = 'Use File Creation Date for PhotoDate'
    TabOrder = 5
  end
  object cbUseFileModificationDateForPhotoDate: TCheckBox
    Left = 280
    Top = 62
    Width = 228
    Height = 17
    Caption = 'Use File Modification Date for PhotoDate'
    TabOrder = 3
  end
  object cbUseFileMediaInfoForPhotoDate: TCheckBox
    Left = 281
    Top = 108
    Width = 228
    Height = 27
    Caption = 'Use File MediaInfo for PhotoDate (for audio/video - may be slow)'
    TabOrder = 6
    WordWrap = True
  end
  object pnlFileNameInfo: TPanel
    Left = 16
    Top = 3
    Width = 249
    Height = 158
    BevelOuter = bvNone
    TabOrder = 4
    object Label1: TLabel
      Left = 1
      Top = 8
      Width = 158
      Height = 13
      Caption = 'Info Contained in File Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbExtractLatLon: TCheckBox
      Left = 6
      Top = 93
      Width = 237
      Height = 17
      Caption = 'Extract {Lat, Lon} to Latitude, Longitude fields'
      TabOrder = 3
    end
    object cbCheckForDate: TCheckBox
      Left = 6
      Top = 114
      Width = 177
      Height = 17
      Caption = 'Check File Name for Photo Date'
      TabOrder = 4
    end
    object cbExtractComments: TCheckBox
      Left = 6
      Top = 72
      Width = 205
      Height = 17
      Caption = 'Extract [comments] to Comments field'
      TabOrder = 2
    end
    object cbIncludeFileName: TCheckBox
      Left = 6
      Top = 51
      Width = 185
      Height = 17
      Caption = 'Include File Name in KeyWords'
      TabOrder = 1
    end
    object cbIncludeFilePath: TCheckBox
      Left = 6
      Top = 30
      Width = 169
      Height = 17
      Caption = 'Include File Path in KeyWords'
      TabOrder = 0
    end
    object cbExtractCustomKey: TCheckBox
      Left = 6
      Top = 136
      Width = 243
      Height = 17
      Caption = 'Extract "Custom Key=" to Custom Key Field'
      TabOrder = 5
    end
  end
  object pnlOwner: TPanel
    Left = 32
    Top = 462
    Width = 185
    Height = 38
    BevelOuter = bvNone
    TabOrder = 8
    object Label3: TLabel
      Left = 1
      Top = 1
      Width = 34
      Height = 14
      Caption = 'Owner'
    end
    object cbPrivate: TCheckBox
      Left = 76
      Top = 19
      Width = 73
      Height = 17
      Caption = 'Private'
      TabOrder = 1
    end
    object leOwner: TMaskEdit
      Left = 0
      Top = 16
      Width = 61
      Height = 22
      EditMask = '>aaaaa;1;_'
      MaxLength = 5
      TabOrder = 0
      Text = '     '
    end
  end
  object leDefaultKeyWords: TLabeledEdit
    Left = 32
    Top = 435
    Width = 473
    Height = 22
    Anchors = [akLeft, akRight, akBottom]
    EditLabel.Width = 91
    EditLabel.Height = 14
    EditLabel.Caption = 'Default Key Words'
    TabOrder = 9
  end
end
