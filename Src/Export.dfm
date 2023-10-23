object frmExport: TfrmExport
  Left = 638
  Top = 323
  Width = 683
  Height = 516
  Caption = 'Export Selected Records'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    667
    478)
  PixelsPerInch = 96
  TextHeight = 13
  object leFileName: TLabeledEdit
    Left = 24
    Top = 24
    Width = 521
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 82
    EditLabel.Height = 13
    EditLabel.Caption = 'Output File Name'
    TabOrder = 0
    Text = 'C:\temp\Export.mdb'
  end
  object btnBegin: TButton
    Left = 480
    Top = 445
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Begin'
    Default = True
    TabOrder = 1
    OnClick = btnBeginClick
  end
  object btnCancel: TButton
    Left = 568
    Top = 445
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnBrowse: TButton
    Left = 552
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object GroupBox1: TGroupBox
    Left = 368
    Top = 56
    Width = 105
    Height = 145
    Caption = 'Output Format'
    TabOrder = 4
    object rbMSAccess: TRadioButton
      Left = 10
      Top = 20
      Width = 113
      Height = 17
      Caption = 'MS Access'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbMSAccessClick
    end
    object rbdBase: TRadioButton
      Left = 10
      Top = 44
      Width = 113
      Height = 17
      Caption = 'dBase'
      TabOrder = 1
      OnClick = rbdBaseClick
    end
    object rbExcel: TRadioButton
      Left = 10
      Top = 68
      Width = 113
      Height = 17
      Caption = 'Excel'
      TabOrder = 2
      OnClick = rbExcelClick
    end
    object rbDelimited: TRadioButton
      Left = 10
      Top = 94
      Width = 113
      Height = 17
      Caption = 'Delimited'
      TabOrder = 3
      OnClick = rbDelimitedClick
    end
    object rbFoxpro: TRadioButton
      Left = 10
      Top = 118
      Width = 113
      Height = 17
      Caption = 'Foxpro'
      Enabled = False
      TabOrder = 4
      OnClick = rbFoxproClick
    end
  end
  object CheckListBox1: TCheckListBox
    Left = 24
    Top = 56
    Width = 321
    Height = 368
    Anchors = [akLeft, akTop, akBottom]
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 5
  end
  object btnClearAll: TButton
    Left = 32
    Top = 445
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear All'
    TabOrder = 6
    OnClick = btnClearAllClick
  end
  object btnSetAll: TButton
    Left = 120
    Top = 445
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Set All'
    TabOrder = 7
    OnClick = btnSetAllClick
  end
  object SMExportToAccess1: TSMExportToAccess
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = False
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDataSet
    FileName = 'C:\D7\Bin\SMExport.MDB'
    CharacterSet = csANSI_WINDOWS
    Left = 456
    Top = 56
  end
  object SMExportToDBF1: TSMExportToDBF
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = True
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDataSet
    FileName = 'C:\D7\Bin\SMExport.DBF'
    AddTitle = False
    CharacterSet = csANSI_WINDOWS
    DBFVersion = dBase4Memo
    Left = 424
    Top = 56
  end
  object SMExportToBDE1: TSMExportToBDE
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = False
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDataSet
    TableType = ttDefault
    FileName = 'C:\D7\Bin\SMExport'
    CharacterSet = csANSI_WINDOWS
    Left = 392
    Top = 56
  end
  object SMExportToText1: TSMExportToText
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = False
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDataSet
    FileName = 'C:\D7\Bin\SMExport.TXT'
    AddTitle = True
    CharacterSet = csANSI_WINDOWS
    TextQualifying = tqStringOnly
    Fixed = False
    Left = 360
    Top = 56
  end
  object SMExportToExcel1: TSMExportToExcel
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = False
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDBGrid
    FileName = 'C:\D7\Bin\SMExport.XLS'
    AddTitle = False
    CharacterSet = csANSI_WINDOWS
    ExportStyle.Style = esNormal
    ExportStyle.OddColor = clBlack
    ExportStyle.EvenColor = clBlack
    Left = 488
    Top = 56
  end
  object SMExportToFoxpro1: TSMExportToDBF
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = False
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDBGrid
    FileName = 'C:\D7\Bin\SMExport.DBF'
    AddTitle = False
    CharacterSet = csANSI_WINDOWS
    DBFVersion = VFP3
    DBFMemoType = dmFPT
    Left = 328
    Top = 56
  end
  object SMExportToXLS1: TSMExportToXLS
    AnimatedStatus = True
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = True
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = '$'
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    KeyGenerator = 'SMExport 4.77'
    SelectedRecord = False
    BlankIfZero = False
    RightToLeft = False
    Statistic.TotalCount = 0
    Statistic.Result = erInProgress
    Columns = <>
    Bands = <>
    ColumnSource = csDBGrid
    FileName = 'C:\D7\Bin\SMExport.XLS'
    AddTitle = True
    CharacterSet = csANSI_WINDOWS
    ExportStyle.Style = esNormal
    ExportStyle.OddColor = clBlack
    ExportStyle.EvenColor = clBlack
    Left = 520
    Top = 56
  end
  object OpenDialog1: TSaveDialog
    Left = 592
    Top = 56
  end
end
