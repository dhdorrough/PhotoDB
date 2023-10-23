object frmReportOptions: TfrmReportOptions
  Left = 749
  Top = 319
  Width = 709
  Height = 436
  Caption = 'Report Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    693
    397)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 13
    Width = 68
    Height = 13
    Caption = 'Log File Name'
  end
  object edtLogFileName: TEdit
    Left = 93
    Top = 11
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edtLogFileName'
  end
  object btnOk: TButton
    Left = 533
    Top = 370
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Begin'
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 615
    Top = 370
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnBrowse: TButton
    Left = 610
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object cbIncludePhotoDate: TCheckBox
    Left = 16
    Top = 48
    Width = 129
    Height = 17
    Caption = 'Include Photo Date'
    TabOrder = 4
    OnClick = cbIncludePhotoDateClick
  end
  object cbIncludeFileName: TCheckBox
    Left = 16
    Top = 72
    Width = 129
    Height = 17
    Caption = 'Include FileName'
    TabOrder = 5
    OnClick = cbIncludeFileNameClick
  end
  object cbIncludeFilePath: TCheckBox
    Left = 16
    Top = 225
    Width = 129
    Height = 17
    Caption = 'Include FilePath'
    TabOrder = 6
    OnClick = cbIncludeFilePathClick
  end
  object cbIncludeKeyWords: TCheckBox
    Left = 16
    Top = 249
    Width = 129
    Height = 17
    Caption = 'Include Key Words'
    TabOrder = 7
  end
  object cbIncludeLocationInfo: TCheckBox
    Left = 16
    Top = 273
    Width = 97
    Height = 17
    Caption = 'Location Info'
    TabOrder = 8
  end
  object leFileNamePattern: TLabeledEdit
    Left = 130
    Top = 129
    Width = 255
    Height = 21
    EditLabel.Width = 84
    EditLabel.Height = 13
    EditLabel.Caption = 'File Name Pattern'
    LabelPosition = lpLeft
    LabelSpacing = 10
    TabOrder = 10
    Text = '%Y%M%D [%N]'
    Visible = False
    OnChange = leFileNamePatternChange
  end
  object cbGenNames: TCheckBox
    Left = 16
    Top = 87
    Width = 665
    Height = 41
    Caption = 
      'Generate File Names Based on pattern (%m=month, %d=day, %y=year,' +
      ' %t=time, %f=foldername, %n=filename, %i=folder#, %k=index no, %' +
      'L=Location Description, %X=Location)'
    TabOrder = 11
    WordWrap = True
    OnClick = cbGenNamesClick
  end
  object pnlFileNameBasedOnLatLon: TPanel
    Left = 136
    Top = 160
    Width = 289
    Height = 54
    BevelInner = bvLowered
    TabOrder = 12
    object cbBasedOnLatitudeStoN: TRadioButton
      Left = 22
      Top = 7
      Width = 113
      Height = 17
      Caption = 'Latitude (S to N)'
      TabOrder = 0
      OnClick = leFileNamePatternChange
    end
    object cbBasedOnLatitudeNtoS: TRadioButton
      Left = 22
      Top = 31
      Width = 113
      Height = 17
      Caption = 'Latitude (N to S)'
      TabOrder = 1
      OnClick = leFileNamePatternChange
    end
    object cbBasedOnLongitudeWtoE: TRadioButton
      Left = 150
      Top = 31
      Width = 113
      Height = 17
      Caption = 'Longitude (W to E)'
      TabOrder = 2
      OnClick = leFileNamePatternChange
    end
    object cbBasedOnLongitudeEtoW: TRadioButton
      Left = 150
      Top = 7
      Width = 113
      Height = 17
      Caption = 'Longitude (E to W)'
      TabOrder = 3
      OnClick = leFileNamePatternChange
    end
  end
  object cbIncludeCopyrightOwner: TCheckBox
    Left = 16
    Top = 296
    Width = 145
    Height = 17
    Caption = 'Include Copyright Owner'
    TabOrder = 9
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'TXT'
    Left = 508
    Top = 39
  end
end
