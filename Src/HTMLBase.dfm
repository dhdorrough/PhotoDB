object frmHTMLBase: TfrmHTMLBase
  Left = 750
  Top = 220
  Width = 623
  Height = 434
  Caption = 'frmHTMLBase'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    607
    395)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 14
    Top = 374
    Width = 40
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
  end
  object lblProgressInfo: TLabel
    Left = 16
    Top = 357
    Width = 69
    Height = 13
    Caption = 'lblProgressInfo'
  end
  object Label1: TLabel
    Left = 272
    Top = 0
    Width = 21
    Height = 13
    Caption = 'CSS'
  end
  object cbProcessSubFolders: TCheckBox
    Left = 17
    Top = 18
    Width = 119
    Height = 17
    Caption = 'Process Sub-folders'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object cbDeleteOldHTML: TCheckBox
    Left = 16
    Top = 45
    Width = 119
    Height = 17
    Caption = 'Delete Old HTML'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object edtLocalCSS: TEdit
    Left = 272
    Top = 18
    Width = 321
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = cl3DLight
    TabOrder = 2
    Text = 'edtLocalCSS'
  end
  object edtRemoteCSS: TEdit
    Left = 272
    Top = 43
    Width = 321
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'edtRemoteCSS'
  end
  object btnBegin: TButton
    Left = 435
    Top = 362
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Begin'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnBeginClick
  end
  object btnCancel: TButton
    Left = 523
    Top = 362
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object rgProtocol: TRadioGroup
    Left = 18
    Top = 66
    Width = 223
    Height = 57
    Caption = 'Protocol'
    Columns = 2
    Items.Strings = (
      'HTTP://'
      'HTTPS://')
    TabOrder = 6
  end
  object ProgressBar1: TProgressBar
    Left = 14
    Top = 333
    Width = 581
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 7
  end
  object rgFileReference: TRadioGroup
    Left = 144
    Top = 13
    Width = 118
    Height = 54
    Caption = 'File References'
    Items.Strings = (
      'Local File'
      'Remote File')
    TabOrder = 8
    OnClick = rgFileReferenceClick
  end
  object leRowCount: TLabeledEdit
    Left = 256
    Top = 88
    Width = 56
    Height = 21
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Row Count'
    TabOrder = 9
    OnExit = leRowCountExit
  end
  object leColCount: TLabeledEdit
    Left = 323
    Top = 88
    Width = 56
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Col Count'
    TabOrder = 10
    OnExit = leColCountExit
  end
end
