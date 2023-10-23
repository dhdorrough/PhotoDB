object frmHandleMissingFile: TfrmHandleMissingFile
  Left = 512
  Top = 284
  Width = 1154
  Height = 533
  Caption = 'Handle Missing File'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    1146
    502)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMsg: TLabel
    Left = 12
    Top = 4
    Width = 1122
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'File "%s" does not exist.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 9
    Top = 461
    Width = 106
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '%n occurrences found'
  end
  object lblStatus2: TLabel
    Left = 8
    Top = 477
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus2'
  end
  object Label1: TLabel
    Left = 8
    Top = 136
    Width = 54
    Height = 13
    Caption = 'Found Files'
  end
  object rbSearch: TRadioButton
    Left = 16
    Top = 37
    Width = 1120
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Search for file %s'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object rbDelete: TRadioButton
    Left = 16
    Top = 85
    Width = 1120
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Delete file "%s"'
    TabOrder = 3
  end
  object rbIgnore: TRadioButton
    Left = 16
    Top = 110
    Width = 158
    Height = 17
    Caption = '&Ignore it'
    TabOrder = 4
  end
  object btnOk: TButton
    Left = 1039
    Top = 461
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    Default = True
    TabOrder = 5
    OnClick = btnOkClick
  end
  object StringGrid1: TStringGrid
    Left = 11
    Top = 160
    Width = 1111
    Height = 292
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    ScrollBars = ssVertical
    TabOrder = 6
    ColWidths = (
      809
      64)
  end
  object btnUpdateDB: TButton
    Left = 922
    Top = 461
    Width = 104
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = 'Update DB'
    TabOrder = 7
    OnClick = btnUpdateDBClick
  end
  object rbSearchForSubString: TRadioButton
    Left = 16
    Top = 61
    Width = 169
    Height = 17
    Caption = 'Search for String in &FileName'
    TabOrder = 1
    OnClick = rbSearchForSubStringClick
  end
  object edtSubString: TEdit
    Left = 192
    Top = 59
    Width = 281
    Height = 21
    Enabled = False
    TabOrder = 2
    Text = 'SubString'
  end
end
