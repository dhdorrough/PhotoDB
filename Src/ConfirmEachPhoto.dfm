object frmConfirmEachPhoto: TfrmConfirmEachPhoto
  Left = 943
  Top = 323
  Width = 784
  Height = 569
  Caption = 'Confirm Each Record'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    768
    530)
  PixelsPerInch = 96
  TextHeight = 14
  object lblQuestion: TLabel
    Left = 16
    Top = 16
    Width = 53
    Height = 14
    Caption = 'lblQuestion'
  end
  object lblCaption1: TLabel
    Left = 24
    Top = 432
    Width = 717
    Height = 49
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Caption1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblCaption2: TLabel
    Left = 24
    Top = 496
    Width = 77
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'lblCaption2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object btnOk: TButton
    Left = 584
    Top = 494
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Yes'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 670
    Top = 494
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'No'
    ModalResult = 7
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 16
    Top = 40
    Width = 725
    Height = 377
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'No Photo Available'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object DBImage1: TRotateImage
      Left = 1
      Top = 1
      Width = 723
      Height = 375
      Align = alClient
      Center = True
      Proportional = True
    end
  end
  object Button1: TButton
    Left = 492
    Top = 496
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
