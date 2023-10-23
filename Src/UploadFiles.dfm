object frmUploadFiles: TfrmUploadFiles
  Left = 520
  Top = 243
  Width = 896
  Height = 486
  Caption = 'Upload Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    880
    448)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 16
    Top = 400
    Width = 40
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
  end
  object btnUpload: TButton
    Left = 775
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Upload'
    Enabled = False
    TabOrder = 7
    OnClick = btnUploadClick
  end
  object mmoLog: TMemo
    Left = 16
    Top = 144
    Width = 848
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmoLog')
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 420
    Width = 749
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 8
  end
  object leWebSiteURL: TLabeledEdit
    Left = 16
    Top = 24
    Width = 369
    Height = 21
    EditLabel.Width = 45
    EditLabel.Height = 13
    EditLabel.Caption = 'FTP URL'
    TabOrder = 0
  end
  object leWebsiteUserID: TLabeledEdit
    Left = 400
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 59
    EditLabel.Height = 13
    EditLabel.Caption = 'FTP User ID'
    TabOrder = 1
  end
  object leWebsitePassword: TLabeledEdit
    Left = 544
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 69
    EditLabel.Height = 13
    EditLabel.Caption = 'FTP Password'
    TabOrder = 2
  end
  object leWebSiteFilePath: TLabeledEdit
    Left = 400
    Top = 64
    Width = 289
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'FTP File Path'
    TabOrder = 4
  end
  object leLogFileName: TLabeledEdit
    Left = 16
    Top = 104
    Width = 737
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 68
    EditLabel.Height = 13
    EditLabel.Caption = 'Log File Name'
    TabOrder = 5
  end
  object leLocalFilePath: TLabeledEdit
    Left = 16
    Top = 64
    Width = 281
    Height = 21
    EditLabel.Width = 70
    EditLabel.Height = 13
    EditLabel.Caption = 'Local File Path'
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 768
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 9
    OnClick = btnBrowseClick
  end
  object cbUploadPhotos: TCheckBox
    Left = 760
    Top = 24
    Width = 97
    Height = 17
    Caption = 'Upload Photos'
    TabOrder = 10
    OnClick = cbUploadPhotosClick
  end
  object cbUploadHTML: TCheckBox
    Left = 760
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Upload HTML'
    TabOrder = 11
    OnClick = cbUploadHTMLClick
  end
  object IpFtpClient1: TIpFtpClient
    DebugLog.BufferSize = 65536
    DebugLog.WriteMode = wmOverwrite
    DebugLog.Enabled = False
    DebugLog.FileName = 'debug.log'
    DefaultPort = 21
    EventLog.DateTimeFormat = 'yyyy.mm.dd hh:nn:ss'
    EventLog.Enabled = False
    EventLog.FileName = 'event.log'
    FileType = ftBinary
    IdleTimeout = 0
    PassiveMode = False
    SocksServer.Port = 1080
    SocksServer.SocksVersion = svSocks4
    TransferTimeout = 0
    OnError = IpFtpClient1Error
    OnFtpConnected = IpFtpClient1FtpConnected
    OnFtpDisconnected = IpFtpClient1FtpDisconnected
    OnFtpError = IpFtpClient1FtpError
    OnFtpLoginError = IpFtpClient1FtpLoginError
    OnFtpStatus = IpFtpClient1FtpStatus
    Left = 640
    Top = 24
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 808
    Top = 80
  end
end
