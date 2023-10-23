object frmFullSize: TfrmFullSize
  Left = 524
  Top = 273
  Width = 870
  Height = 640
  Caption = 'Full Size Window'
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object DBImage1: TRotateImage
    Left = 0
    Top = 0
    Width = 862
    Height = 609
    Align = alClient
    Center = True
    PopupMenu = PopupMenu1
    Proportional = True
  end
  object PopupMenu1: TPopupMenu
    Left = 320
    Top = 168
    object UseFullScreen1: TMenuItem
      Caption = 'Use Full Screen'
      OnClick = UseFullScreen1Click
    end
    object SlideShow1: TMenuItem
      Caption = 'Slide Show'
      ShortCut = 16467
    end
    object Interval1: TMenuItem
      Caption = 'Interval'
      OnClick = Interval1Click
      object N1sec1: TMenuItem
        Caption = '1 sec'
        GroupIndex = 99
        RadioItem = True
        OnClick = N1sec1Click
      end
      object N2sec1: TMenuItem
        Caption = '2 sec'
        GroupIndex = 99
        RadioItem = True
        OnClick = N2sec1Click
      end
      object N4sec1: TMenuItem
        Caption = '4 sec'
        GroupIndex = 99
        RadioItem = True
        OnClick = N4sec1Click
      end
      object N8sec1: TMenuItem
        Caption = '8 sec'
        GroupIndex = 99
        RadioItem = True
        OnClick = N8sec1Click
      end
      object N16sec1: TMenuItem
        Caption = '16 sec'
        GroupIndex = 99
        RadioItem = True
        OnClick = N16sec1Click
      end
      object N32sec1: TMenuItem
        Caption = '32 sec'
        GroupIndex = 99
        RadioItem = True
        OnClick = N32sec1Click
      end
    end
  end
end
