inherited frmHTMLForSelectedFiles: TfrmHTMLForSelectedFiles
  Left = 754
  Top = 380
  Width = 621
  Height = 430
  Caption = 'HTML for Selected Files'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblStatus: TLabel
    Top = 370
  end
  inherited lblProgressInfo: TLabel
    Left = 14
    Top = 353
    Anchors = [akLeft, akBottom]
  end
  object ListBox1: TCheckListBox [3]
    Left = 16
    Top = 106
    Width = 572
    Height = 215
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 17
    ParentFont = False
    TabOrder = 9
    OnMouseDown = ListBox1MouseDown
    OnMouseMove = ListBox1MouseMove
  end
  inherited btnBegin: TButton
    Left = 433
    Top = 358
  end
  inherited btnCancel: TButton
    Left = 521
    Top = 358
  end
  inherited rgProtocol: TRadioGroup
    Width = 180
    Height = 33
  end
  inherited ProgressBar1: TProgressBar
    Width = 579
  end
  inherited leRowCount: TLabeledEdit
    Left = 271
    Top = 81
    TabOrder = 10
  end
  inherited leColCount: TLabeledEdit
    Left = 338
    Top = 81
    TabOrder = 11
  end
end
