inherited frmBrowseScenes: TfrmBrowseScenes
  Left = 504
  Top = 385
  Width = 1318
  Height = 279
  Caption = 'Browse Scenes'
  Position = poDefaultPosOnly
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 14
  inherited lblStatus: TLabel
    Left = 1255
    Top = 202
  end
  object Label1: TLabel [1]
    Left = 345
    Top = 202
    Width = 32
    Height = 14
    Caption = 'Label1'
  end
  inherited DBGrid1: TDBGrid
    Width = 1397
    Height = 188
    Columns = <
      item
        Expanded = False
        FieldName = 'PhotoDate'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StartInSeconds'
        Title.Caption = 'Start(sec)'
        Width = 53
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StartFrame'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LengthInSeconds'
        Title.Caption = 'Length(sec)'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LengthInFrames'
        Title.Caption = 'Frames'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'KEY_WORDS'
        Title.Caption = 'Key Words'
        Width = 344
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AlsoSee'
        Width = 91
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Comment'
        Width = 49
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PhotoDateTime'
        Width = 79
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Year'
        Width = 41
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Month'
        Title.Caption = 'Mon'
        Width = 30
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Day'
        Width = 27
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Updated'
        Width = 115
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Added'
        Width = 92
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FileName_Key'
        Title.Caption = 'Key'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end>
  end
  inherited DBNavigator1: TDBNavigator
    Top = 197
    Hints.Strings = ()
  end
  inherited btnClose: TButton
    Left = 1334
    Top = 209
  end
  inherited MainMenu1: TMainMenu
    inherited File1: TMenuItem
      object Export1: TMenuItem [0]
        Caption = 'Export'
        object toCSV1: TMenuItem
          Caption = 'to .CSV...'
          OnClick = toCSV1Click
        end
      end
    end
    inherited Edit1: TMenuItem
      Caption = '&Edit'
      object N1: TMenuItem
        Caption = '-'
      end
      object CopyRecord1: TMenuItem
        Caption = 'Copy Record...'
      end
      object AppendandPasteRecord1: TMenuItem
        Caption = 'Append and Paste Record'
      end
    end
    inherited Navigate1: TMenuItem
      object N5: TMenuItem
        Caption = '-'
      end
      object Orderby1: TMenuItem
        Caption = 'Order by'
        object PhhotoDate1: TMenuItem
          Caption = 'Photo Date'
          OnClick = PhhotoDate1Click
        end
        object PhtoKey1: TMenuItem
          Caption = 'Photo Key'
          OnClick = PhtoKey1Click
        end
        object Updated1: TMenuItem
          Caption = 'Updated'
          OnClick = Updated1Click
        end
        object Added1: TMenuItem
          Caption = 'Added'
          OnClick = Added1Click
        end
      end
    end
    object Utilities1: TMenuItem
      Caption = '&Utilities'
      object CopyScenesfromAnotherPhotoTableRecord1: TMenuItem
        Caption = 'Copy Scenes from Another PhotoTable Record...'
        OnClick = CopyScenesfromAnotherPhotoTableRecord1Click
      end
      object RecalculateSceneLengths1: TMenuItem
        Caption = 'Recalculate Scene Lengths'
        OnClick = RecalculateSceneLengths1Click
      end
      object CalculateSceneStartsBasedsonSceneLengths1: TMenuItem
        Caption = 'Calculate Scene Starts Based on Scene Lengths (sorted by date)'
        OnClick = CalculateSceneStartsBasedsonSceneLengths1Click
      end
      object SetVideoLength1: TMenuItem
        Caption = 'Set Video Length...'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'CSV'
    FileName = 'Scenes.csv'
    InitialDir = 'c:\temp'
    Left = 16
    Top = 24
  end
end
