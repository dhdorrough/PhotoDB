object frmPhotoDBOptions: TfrmPhotoDBOptions
  Left = 807
  Top = 260
  Width = 622
  Height = 531
  Caption = 'Photo DB Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  Position = poScreenCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    606
    492)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 441
    Top = 465
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 527
    Top = 465
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 606
    Height = 413
    ActivePage = tsPhotoDB
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PopupMenu1
    TabOrder = 2
    object tsPhotoDB: TTabSheet
      Caption = 'PhotoDB'
      PopupMenu = PopupMenu1
      DesignSize = (
        598
        385)
      object Label1: TLabel
        Left = 13
        Top = 56
        Width = 114
        Height = 13
        Caption = 'Path to Photo Database'
      end
      object Label3: TLabel
        Left = 13
        Top = 11
        Width = 91
        Height = 13
        Caption = 'Path to Documents'
      end
      object Label4: TLabel
        Left = 13
        Top = 147
        Width = 105
        Height = 13
        Caption = 'Photo Editing Program'
      end
      object lblPathToHikingMDB: TLabel
        Left = 13
        Top = 101
        Width = 116
        Height = 13
        Caption = 'Path to Hiking Database'
      end
      object Label5: TLabel
        Left = 13
        Top = 245
        Width = 101
        Height = 13
        Caption = 'Path to Import/Export'
      end
      object Label6: TLabel
        Left = 13
        Top = 196
        Width = 150
        Height = 13
        Caption = 'Alternate Photo Editing Program'
      end
      object edtPhotoMDB: TEdit
        Left = 14
        Top = 74
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object btnBrowseToPhotoDB: TButton
        Left = 522
        Top = 72
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 3
        TabStop = False
        OnClick = btnBrowseToPhotoDBClick
      end
      object edtRoot: TEdit
        Left = 14
        Top = 32
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object btnBrowseToRoot: TButton
        Left = 522
        Top = 30
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 1
        TabStop = False
        OnClick = btnBrowseToRootClick
      end
      object edtPhotoEditingProgram: TEdit
        Left = 14
        Top = 166
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
      end
      object btnBrowseToPhotoEditingProgram: TButton
        Left = 522
        Top = 168
        Width = 58
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 7
        TabStop = False
        OnClick = btnBrowseToPhotoEditingProgramClick
      end
      object edtHikingMDB: TEdit
        Left = 14
        Top = 120
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
      end
      object btnBrowseToHikingDB: TButton
        Left = 522
        Top = 118
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 5
        TabStop = False
        OnClick = btnBrowseToHikingDBClick
      end
      object leCopyRight: TLabeledEdit
        Left = 14
        Top = 312
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'CopyRight'
        TabOrder = 11
      end
      object leCopyRightID: TLabeledEdit
        Left = 14
        Top = 357
        Width = 43
        Height = 21
        EditLabel.Width = 63
        EditLabel.Height = 13
        EditLabel.Caption = 'CopyRight ID'
        MaxLength = 4
        TabOrder = 12
      end
      object edtImportExportFolder: TEdit
        Left = 14
        Top = 266
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 9
      end
      object btnImportExportFolder: TButton
        Left = 522
        Top = 265
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 10
        TabStop = False
        OnClick = btnImportExportFolderClick
      end
      object edtAlternatePhotoEditingProgram: TEdit
        Left = 14
        Top = 215
        Width = 499
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
      end
      object btnBrowseToAlternatePhotoEditingProgram: TButton
        Left = 522
        Top = 215
        Width = 58
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 13
        TabStop = False
        OnClick = btnBrowseToAlternatePhotoEditingProgramClick
      end
    end
    object tsMakeHTML: TTabSheet
      Caption = 'MakeHTML'
      ImageIndex = 1
      PopupMenu = PopupMenu1
      DesignSize = (
        598
        385)
      object Label2: TLabel
        Left = 12
        Top = 23
        Width = 71
        Height = 13
        Caption = 'Next Folder No'
      end
      object lblStatus: TLabel
        Left = 192
        Top = 48
        Width = 30
        Height = 13
        Caption = 'Status'
      end
      object edtFolderNo: TEdit
        Left = 12
        Top = 44
        Width = 86
        Height = 21
        TabOrder = 0
        Text = 'edtFolderNo'
      end
      object btnScan: TButton
        Left = 104
        Top = 42
        Width = 75
        Height = 25
        Caption = 'Scan'
        TabOrder = 1
        OnClick = btnScanClick
      end
      object cbReferenceLocal: TCheckBox
        Left = 12
        Top = 73
        Width = 145
        Height = 17
        Caption = 'Reference .css Locally'
        State = cbGrayed
        TabOrder = 2
        OnClick = cbReferenceLocalClick
      end
      object leLocalCSS: TLabeledEdit
        Left = 12
        Top = 111
        Width = 511
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 103
        EditLabel.Height = 13
        EditLabel.Caption = 'Local .CSS File Name'
        TabOrder = 3
      end
      object leRemoteCSS: TLabeledEdit
        Left = 12
        Top = 151
        Width = 515
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 114
        EditLabel.Height = 13
        EditLabel.Caption = 'Remote .CSS File Name'
        TabOrder = 4
      end
      object btnLocalCSS: TButton
        Left = 533
        Top = 108
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 5
        OnClick = btnLocalCSSClick
      end
    end
    object tsLocation: TTabSheet
      Caption = 'Location'
      ImageIndex = 2
      PopupMenu = PopupMenu1
      DesignSize = (
        598
        385)
      object Label7: TLabel
        Left = 8
        Top = 80
        Width = 383
        Height = 13
        Caption = 
          'i.e.: F:\NDAS-I\Shared Documents\Delorme\DeLorme Docs\Transfer F' +
          'iles\*.gpx'
      end
      object leTrackDataLfn: TLabeledEdit
        Left = 10
        Top = 18
        Width = 501
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 73
        EditLabel.Height = 13
        EditLabel.Caption = 'Track Data File'
        TabOrder = 0
      end
      object btnLocateTrackData: TBitBtn
        Left = 517
        Top = 16
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnLocateTrackDataClick
      end
      object leGPXFolder: TLabeledEdit
        Left = 10
        Top = 56
        Width = 501
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 111
        EditLabel.Height = 13
        EditLabel.Caption = 'Path Filter for .GPX files'
        TabOrder = 2
      end
      object btnGPXFolder: TButton
        Left = 517
        Top = 54
        Width = 58
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse'
        TabOrder = 3
        OnClick = btnGPXFolderClick
      end
      object RadioGroup1: TRadioGroup
        Left = 16
        Top = 116
        Width = 361
        Height = 137
        Caption = 'Location Kind'
        TabOrder = 4
      end
      object rbLatLong: TRadioButton
        Left = 24
        Top = 141
        Width = 113
        Height = 17
        Caption = 'Latitude Longitude'
        TabOrder = 5
        OnClick = rbLatLongClick
      end
      object rbLastWord: TRadioButton
        Left = 24
        Top = 165
        Width = 113
        Height = 17
        Caption = 'Last Word'
        TabOrder = 6
        OnClick = rbLastWordClick
      end
      object rbSpecifiedValue: TRadioButton
        Left = 24
        Top = 189
        Width = 113
        Height = 17
        Caption = 'Specified Value'
        TabOrder = 7
        OnClick = rbSpecifiedValueClick
      end
      object rbHikingLogAreaForTheDate: TRadioButton
        Left = 24
        Top = 213
        Width = 169
        Height = 17
        Caption = 'Hiking Log Area for the Date'
        TabOrder = 8
        Visible = False
        OnClick = rbHikingLogAreaForTheDateClick
      end
      object edtSpecifiedValue: TEdit
        Left = 136
        Top = 189
        Width = 121
        Height = 21
        TabOrder = 9
        Text = 'edtSpecifiedValue'
      end
    end
    object tsInternet: TTabSheet
      Caption = 'Internet'
      ImageIndex = 3
      object leWebSiteURL: TLabeledEdit
        Left = 16
        Top = 24
        Width = 369
        Height = 21
        EditLabel.Width = 64
        EditLabel.Height = 13
        EditLabel.Caption = 'Website URL'
        TabOrder = 0
      end
      object leWebsiteUserID: TLabeledEdit
        Left = 16
        Top = 64
        Width = 121
        Height = 21
        EditLabel.Width = 80
        EditLabel.Height = 13
        EditLabel.Caption = 'WebSite User ID'
        TabOrder = 1
      end
      object leWebsitePassword: TLabeledEdit
        Left = 16
        Top = 104
        Width = 121
        Height = 21
        EditLabel.Width = 90
        EditLabel.Height = 13
        EditLabel.Caption = 'WebSite Password'
        TabOrder = 2
      end
      object leWebSiteFilePath: TLabeledEdit
        Left = 16
        Top = 144
        Width = 121
        Height = 21
        EditLabel.Width = 85
        EditLabel.Height = 13
        EditLabel.Caption = 'WebSite File Path'
        TabOrder = 3
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.mdb'
    Filter = 'MS Access DB (*.mdb)|*.mdb'
    Left = 409
    Top = 19
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = '.ini'
    Filter = 'Settings File Name (*.ini)|*.ini'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 424
    Top = 56
  end
  object odLocalCSS: TOpenDialog
    DefaultExt = ',css'
    Filter = 'Cascading Style Sheet (*.css)|*.css'
    Left = 393
    Top = 217
  end
  object odSynonymsFileName: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Synonyms File (*.txt)|*.txt'
    Left = 393
    Top = 322
  end
  object SaveDialog3: TSaveDialog
    DefaultExt = '.mdb'
    Filter = 'MS Access DB (*.mdb)|*.mdb'
    Left = 369
    Top = 51
  end
  object SaveDialog4: TSaveDialog
    DefaultExt = '.ini'
    Filter = 'Settings File Name (*.ini)|*.ini'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 424
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = ',css'
    Filter = 'Cascading Style Sheet (*.css)|*.css'
    Left = 321
    Top = 329
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Synonyms File (*.txt)|*.txt'
    Left = 393
    Top = 322
  end
  object PopupMenu1: TPopupMenu
    Left = 244
    Top = 568
    object IsDHD1: TMenuItem
      Caption = 'Is DHD'
      OnClick = IsDHD1Click
    end
  end
end
