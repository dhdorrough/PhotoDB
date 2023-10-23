inherited frmFilterLocation: TfrmFilterLocation
  Left = 874
  Top = 278
  Caption = 'Filter Location'
  Menu = MainMenu1
  PixelsPerInch = 96
  TextHeight = 13
  inherited GroupBox1: TGroupBox [0]
    Top = 16
    Height = 376
    TabOrder = 1
    inherited leHighDate: TLabeledEdit
      Visible = False
    end
    inherited leLowDate: TLabeledEdit
      Visible = False
    end
    inherited leFilter: TLabeledEdit
      Left = 136
      Top = 336
    end
    inherited btnExpression: TButton
      Left = 391
      Top = 334
    end
    inherited btnClearAll: TButton
      Top = 395
    end
    object leState: TLabeledEdit
      Left = 16
      Top = 237
      Width = 36
      Height = 21
      EditLabel.Width = 31
      EditLabel.Height = 13
      EditLabel.Caption = 'State'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'MS Sans Serif'
      EditLabel.Font.Style = [fsBold]
      EditLabel.ParentFont = False
      MaxLength = 2
      TabOrder = 5
    end
    object pnlLatLon: TPanel
      Left = 1
      Top = 84
      Width = 272
      Height = 133
      BevelOuter = bvNone
      TabOrder = 6
      object Label1: TLabel
        Left = 15
        Top = 22
        Width = 19
        Height = 13
        Caption = 'Lat'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 145
        Top = 22
        Width = 29
        Height = 13
        Caption = 'Long'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 20
        Top = 73
        Width = 78
        Height = 13
        Caption = 'Max Distance'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ovcLatitude: TOvcNumericField
        Left = 16
        Top = 42
        Width = 97
        Height = 21
        Cursor = crIBeam
        DataType = nftDouble
        CaretOvr.Shape = csBlock
        EFColors.Disabled.BackColor = clWindow
        EFColors.Disabled.TextColor = clGrayText
        EFColors.Error.BackColor = clRed
        EFColors.Error.TextColor = clBlack
        EFColors.Highlight.BackColor = clHighlight
        EFColors.Highlight.TextColor = clHighlightText
        Options = []
        PictureMask = '####.#######'
        TabOrder = 0
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object ovcLongitude: TOvcNumericField
        Left = 151
        Top = 42
        Width = 97
        Height = 21
        Cursor = crIBeam
        DataType = nftDouble
        CaretOvr.Shape = csBlock
        EFColors.Disabled.BackColor = clWindow
        EFColors.Disabled.TextColor = clGrayText
        EFColors.Error.BackColor = clRed
        EFColors.Error.TextColor = clBlack
        EFColors.Highlight.BackColor = clHighlight
        EFColors.Highlight.TextColor = clHighlightText
        Options = []
        PictureMask = '####.#######'
        TabOrder = 1
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object cbMaxDist: TComboBox
        Left = 16
        Top = 93
        Width = 126
        Height = 21
        ItemHeight = 13
        TabOrder = 2
      end
    end
  end
  inherited btnOK: TButton [1]
    Top = 412
    TabOrder = 2
  end
  inherited btnCancel: TButton [2]
    Top = 412
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 328
    Top = 48
    object e1: TMenuItem
      Caption = 'Edit'
      object PasteLatitudeLongitude1: TMenuItem
        Caption = 'Paste Latitude/Longitude'
        OnClick = PasteLatitudeLongitude1Click
      end
    end
  end
end
