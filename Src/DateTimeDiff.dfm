object frmCalcTimeDiff: TfrmCalcTimeDiff
  Left = 1317
  Top = 342
  Width = 400
  Height = 290
  Caption = 'Calculate Date/Time Difference'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    392
    259)
  PixelsPerInch = 96
  TextHeight = 14
  object lblDays: TLabel
    Left = 24
    Top = 104
    Width = 35
    Height = 14
    Caption = 'lblDays'
  end
  object lblHours: TLabel
    Left = 24
    Top = 128
    Width = 39
    Height = 14
    Caption = 'lblHours'
  end
  object leDate1: TLabeledEdit
    Left = 24
    Top = 24
    Width = 209
    Height = 22
    EditLabel.Width = 31
    EditLabel.Height = 14
    EditLabel.Caption = 'Date 1'
    TabOrder = 0
    Text = '3/18/2014 12:43:38'
  end
  object leDate2: TLabeledEdit
    Left = 24
    Top = 64
    Width = 209
    Height = 22
    EditLabel.Width = 31
    EditLabel.Height = 14
    EditLabel.Caption = 'Date 2'
    TabOrder = 1
    Text = '4/15/2014 12:39:53'
  end
  object edtDiffInMinutes: TLabeledEdit
    Left = 24
    Top = 176
    Width = 209
    Height = 22
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 123
    EditLabel.Height = 14
    EditLabel.Caption = 'Date 2 - Date 1 in Minutes'
    TabOrder = 2
  end
  object btnCalculate: TButton
    Left = 291
    Top = 210
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Calculate'
    TabOrder = 3
    OnClick = btnCalculateClick
  end
end
