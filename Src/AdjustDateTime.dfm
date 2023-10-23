object frmAdjustDateTime: TfrmAdjustDateTime
  Left = 556
  Top = 313
  Width = 514
  Height = 537
  Caption = 'Adjust Photo Date(s)/Time(s)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    506
    506)
  PixelsPerInch = 96
  TextHeight = 14
  object Label2: TLabel
    Left = 240
    Top = 30
    Width = 168
    Height = 14
    Caption = 'Minutes to add to Photo Date / Time'
  end
  object Label1: TLabel
    Left = 24
    Top = 56
    Width = 186
    Height = 14
    Caption = 'Adjust the date/time of this photo from '
  end
  object lblOldTime: TLabel
    Left = 24
    Top = 88
    Width = 41
    Height = 14
    Caption = 'Old Time'
  end
  object lblNewTime: TLabel
    Left = 192
    Top = 88
    Width = 48
    Height = 14
    Caption = 'New Time'
  end
  object Label5: TLabel
    Left = 166
    Top = 88
    Width = 9
    Height = 14
    Caption = 'to'
  end
  object lblFileName: TLabel
    Left = 96
    Top = 8
    Width = 53
    Height = 14
    Caption = 'lblFileName'
  end
  object Label3: TLabel
    Left = 24
    Top = 8
    Width = 61
    Height = 13
    Caption = 'File Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ovcMinutesToAdd: TOvcNumericField
    Left = 133
    Top = 30
    Width = 81
    Height = 22
    Cursor = crIBeam
    DataType = nftLongInt
    CaretOvr.Shape = csBlock
    EFColors.Disabled.BackColor = clWindow
    EFColors.Disabled.TextColor = clGrayText
    EFColors.Error.BackColor = clRed
    EFColors.Error.TextColor = clBlack
    EFColors.Highlight.BackColor = clHighlight
    EFColors.Highlight.TextColor = clHighlightText
    Options = []
    PictureMask = 'iiiiiiiiiii'
    TabOrder = 0
    OnChange = ovcMinutesToAddChange
    RangeHigh = {FFFFFF7F000000000000}
    RangeLow = {00000080000000000000}
  end
  object btnNo: TButton
    Left = 402
    Top = 454
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'No'
    ModalResult = 7
    TabOrder = 1
  end
  object btnYes: TButton
    Left = 313
    Top = 454
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Yes'
    Default = True
    ModalResult = 6
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 136
    Top = 456
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object pnlPhoto: TPanel
    Left = 24
    Top = 112
    Width = 457
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    OnResize = pnlPhotoResize
    object DBImage1: TRotateImage
      Left = 1
      Top = 1
      Width = 455
      Height = 327
      Align = alClient
    end
  end
  object OvcSpinner1: TOvcSpinner
    Left = 216
    Top = 27
    Width = 16
    Height = 25
    AutoRepeat = True
    Delta = 1.000000000000000000
    FocusedControl = ovcMinutesToAdd
  end
  object btnYesToAll: TButton
    Left = 224
    Top = 456
    Width = 75
    Height = 25
    Caption = 'Yes To All'
    ModalResult = 10
    TabOrder = 3
  end
end
