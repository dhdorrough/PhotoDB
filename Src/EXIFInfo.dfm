object frmExifInfo: TfrmExifInfo
  Left = 566
  Top = 231
  Width = 551
  Height = 614
  Caption = 'EXIF Info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  DesignSize = (
    543
    563)
  PixelsPerInch = 96
  TextHeight = 14
  object lblFileName: TLabel
    Left = 16
    Top = 519
    Width = 413
    Height = 29
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'lblFileName'
    WordWrap = True
  end
  object StringGrid1: TStringGrid
    Left = 16
    Top = 16
    Width = 501
    Height = 493
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 20
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
    ColWidths = (
      64
      342)
  end
  object btnClose: TButton
    Left = 436
    Top = 525
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object MainMenu1: TMainMenu
    Left = 88
    object Edit1: TMenuItem
      Caption = 'Edit'
      object CopyLocationInfo1: TMenuItem
        Caption = 'Copy Location Info'
        OnClick = CopyLocationInfo1Click
      end
    end
  end
end
