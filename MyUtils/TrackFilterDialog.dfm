inherited frmTrackFilterOptions: TfrmTrackFilterOptions
  Left = 691
  Top = 330
  Caption = 'Track Filtering Options'
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TButton
    TabOrder = 2
  end
  inherited btnCancel: TButton
    TabOrder = 0
  end
  inherited GroupBox1: TGroupBox
    Left = 16
    Top = 16
    TabOrder = 1
    inherited leHighDate: TLabeledEdit
      Top = 104
    end
    inherited leLowDate: TLabeledEdit
      Top = 104
    end
    object leUpperLeft: TLabeledEdit
      Left = 35
      Top = 44
      Width = 195
      Height = 21
      EditLabel.Width = 17
      EditLabel.Height = 13
      EditLabel.Caption = 'UL'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'MS Sans Serif'
      EditLabel.Font.Style = [fsBold]
      EditLabel.ParentFont = False
      LabelPosition = lpLeft
      TabOrder = 5
    end
    object leBottomRight: TLabeledEdit
      Left = 261
      Top = 44
      Width = 195
      Height = 21
      EditLabel.Width = 18
      EditLabel.Height = 13
      EditLabel.Caption = 'BR'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'MS Sans Serif'
      EditLabel.Font.Style = [fsBold]
      EditLabel.ParentFont = False
      LabelPosition = lpLeft
      TabOrder = 6
    end
    object btnLoad: TButton
      Left = 491
      Top = 43
      Width = 75
      Height = 25
      Caption = 'Load'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
      OnClick = btnLoadClick
    end
  end
end
