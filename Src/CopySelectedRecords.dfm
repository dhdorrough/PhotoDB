object frmCopySelectedFiles: TfrmCopySelectedFiles
  Left = 1024
  Top = 300
  Width = 722
  Height = 474
  Caption = 'Copy Selected Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    714
    443)
  PixelsPerInch = 96
  TextHeight = 14
  object leDestinationFilePath: TLabeledEdit
    Left = 16
    Top = 24
    Width = 605
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 96
    EditLabel.Height = 14
    EditLabel.Caption = 'Destination File Path'
    TabOrder = 0
    OnChange = leDestinationFilePathChange
  end
  object btnBrowse: TButton
    Left = 631
    Top = 22
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object cbRetainFolderStructure: TCheckBox
    Left = 16
    Top = 56
    Width = 137
    Height = 17
    Caption = 'Retain Folder Structure'
    Enabled = False
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 627
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object btnBegin: TBitBtn
    Left = 539
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Begin'
    Enabled = False
    TabOrder = 4
    Kind = bkOK
  end
  object cbGenNames: TCheckBox
    Left = 16
    Top = 80
    Width = 665
    Height = 41
    Caption = 
      'Generate File Names Based on pattern (%m=month, %d=day, %y=year,' +
      ' %t=time, %f=foldername, %n=filename, %i=folder#, %k=index no, %' +
      'L=Location Description, %X=Location, %C=Comment, %W=Key Words, %' +
      'S=Sequential nnnnnnnn)'
    TabOrder = 5
    WordWrap = True
    OnClick = cbGenNamesClick
  end
  object leFileNamePattern: TLabeledEdit
    Left = 109
    Top = 132
    Width = 255
    Height = 22
    EditLabel.Width = 83
    EditLabel.Height = 14
    EditLabel.Caption = 'File Name Pattern'
    LabelPosition = lpLeft
    LabelSpacing = 10
    TabOrder = 6
    Text = '%Y%M%D [%N]'
    Visible = False
    OnChange = leFileNamePatternChange
  end
  object cbCreateLink: TCheckBox
    Left = 16
    Top = 247
    Width = 185
    Height = 17
    Caption = 'Create Link rather than copy file'
    TabOrder = 8
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 271
    Width = 249
    Height = 129
    Caption = 'Overwrite Mode'
    TabOrder = 9
    object cbAskToOverwriteDuplicateFiles: TRadioButton
      Left = 12
      Top = 93
      Width = 169
      Height = 17
      Caption = 'Ask to overwite duplicate files'
      TabOrder = 3
    end
    object cbChangeFileNameRatherThanOverwriting: TRadioButton
      Left = 12
      Top = 21
      Width = 225
      Height = 17
      Caption = 'Change file name rather than overwriting'
      TabOrder = 0
    end
    object cbAlwaysOverwrite: TRadioButton
      Left = 12
      Top = 69
      Width = 110
      Height = 17
      Caption = 'Always Overwrite'
      TabOrder = 2
    end
    object cbNeverOverwrite: TRadioButton
      Left = 12
      Top = 45
      Width = 97
      Height = 17
      Caption = 'Never overwrite'
      TabOrder = 1
    end
  end
  object cbCopyAssociatedWAVFile: TCheckBox
    Left = 16
    Top = 225
    Width = 185
    Height = 17
    Caption = 'Copy Associated WAV File'
    TabOrder = 7
  end
  object cbCopyOnlyPhotoFiles: TCheckBox
    Left = 216
    Top = 225
    Width = 129
    Height = 17
    Caption = 'Only copy photo files'
    TabOrder = 10
  end
  object cbDeleteAfterCopying: TCheckBox
    Left = 16
    Top = 407
    Width = 185
    Height = 17
    Caption = 'Delete the Record after copying'
    TabOrder = 11
  end
  object cbChangeFileDateToPhotoDAte: TCheckBox
    Left = 368
    Top = 225
    Width = 169
    Height = 17
    Caption = 'Change FileDate To PhotoDate'
    TabOrder = 12
  end
  object pnlFileNameBasedOnLatLon: TPanel
    Left = 136
    Top = 160
    Width = 289
    Height = 54
    BevelInner = bvLowered
    TabOrder = 13
    OnClick = pnlFileNameBasedOnLatLonClick
    object cbBasedOnLatitudeStoN: TRadioButton
      Left = 22
      Top = 7
      Width = 113
      Height = 17
      Caption = 'Latitude (S to N)'
      TabOrder = 0
      OnClick = pnlFileNameBasedOnLatLonClick
    end
    object cbBasedOnLatitudeNtoS: TRadioButton
      Left = 22
      Top = 31
      Width = 113
      Height = 17
      Caption = 'Latitude (N to S)'
      TabOrder = 1
      OnClick = pnlFileNameBasedOnLatLonClick
    end
    object cbBasedOnLongitudeWtoE: TRadioButton
      Left = 150
      Top = 31
      Width = 113
      Height = 17
      Caption = 'Longitude (W to E)'
      TabOrder = 2
      OnClick = pnlFileNameBasedOnLatLonClick
    end
    object cbBasedOnLongitudeEtoW: TRadioButton
      Left = 150
      Top = 7
      Width = 113
      Height = 17
      Caption = 'Longitude (E to W)'
      TabOrder = 3
      OnClick = pnlFileNameBasedOnLatLonClick
    end
  end
end
