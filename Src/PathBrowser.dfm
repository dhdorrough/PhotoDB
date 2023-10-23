inherited frmPathBrowser: TfrmPathBrowser
  Left = 261
  Top = 328
  Caption = 'Path Browser'
  PixelsPerInch = 96
  TextHeight = 13
  object btnNewPath: TButton [1]
    Left = 312
    Top = 331
    Width = 75
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = 'Add path'
    TabOrder = 3
    OnClick = btnNewPathClick
  end
  inherited DBNavigator1: TDBNavigator
    Hints.Strings = ()
  end
end
