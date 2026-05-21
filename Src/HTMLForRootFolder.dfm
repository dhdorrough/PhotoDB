inherited frmHTMLForSelectedRootFolder: TfrmHTMLForSelectedRootFolder
  Left = 704
  Top = 338
  Width = 685
  Height = 284
  Caption = 'HTML For Selected Root Folder'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblStatus: TLabel
    Top = 224
  end
  inherited lblProgressInfo: TLabel
    Top = 205
    Anchors = [akLeft, akBottom]
  end
  object leRootFolder: TLabeledEdit [3]
    Left = 13
    Top = 116
    Width = 559
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 55
    EditLabel.Height = 13
    EditLabel.Caption = 'Root Folder'
    TabOrder = 9
    OnChange = leRootFolderChange
  end
  object btnBrowse: TButton [4]
    Left = 582
    Top = 114
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 11
    OnClick = btnBrowseClick
  end
  object btnCountFoldersToBeProcessed: TButton [5]
    Left = 16
    Top = 144
    Width = 169
    Height = 25
    Caption = 'Count Folders to be Processed'
    TabOrder = 10
    OnClick = btnCountFoldersToBeProcessedClick
  end
  inherited btnBegin: TButton
    Left = 490
    Top = 212
  end
  inherited btnCancel: TButton
    Left = 578
    Top = 212
  end
  inherited rgProtocol: TRadioGroup
    Height = 33
  end
  inherited ProgressBar1: TProgressBar
    Top = 183
    Width = 637
  end
  inherited leRowCount: TLabeledEdit
    Left = 271
    Top = 82
    TabOrder = 12
  end
  inherited leColCount: TLabeledEdit
    Left = 338
    Top = 82
    TabOrder = 13
  end
end
