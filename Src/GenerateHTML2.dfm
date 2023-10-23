object HTMLGenerator: THTMLGenerator
  Left = 576
  Top = 227
  Width = 641
  Height = 502
  Caption = 'Generate HTML Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    633
    471)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 16
    Top = 432
    Width = 40
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
  end
  object cbIsRootDir: TCheckBox
    Left = 17
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Is root directory'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object cbProcessSubFolders: TCheckBox
    Left = 17
    Top = 38
    Width = 119
    Height = 17
    Caption = 'Process Sub-folders'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object cbIncludeEmpty: TCheckBox
    Left = 16
    Top = 60
    Width = 257
    Height = 17
    Caption = 'Include folder in DB even if contains no media'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object Button1: TButton
    Left = 437
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Button2: TButton
    Left = 525
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cbDeleteOldHTML: TCheckBox
    Left = 16
    Top = 80
    Width = 129
    Height = 17
    Caption = 'Delete Old HTML'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object ListBox1: TCheckListBox
    Left = 16
    Top = 104
    Width = 583
    Height = 310
    OnClickCheck = ListBox1ClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 6
  end
end
