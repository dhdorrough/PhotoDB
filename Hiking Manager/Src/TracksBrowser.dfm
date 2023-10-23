inherited frmTracksBrowser: TfrmTracksBrowser
  Left = 231
  Top = 300
  Width = 1438
  Caption = 'Tracks Browser'
  PixelsPerInch = 96
  TextHeight = 13
  inherited DBGrid1: TDBGrid
    Width = 1419
    Height = 323
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StartDate'
        Width = 128
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EndDate'
        Width = 128
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StartLatitude'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EndLatitude'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StartLongitude'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EndLongitude'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HikeID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FileName'
        Width = 200
        Visible = True
      end
      item
        DropDownRows = 9
        Expanded = False
        FieldName = 'GPS Device'
        PickList.Strings = (
          'unknown, '
          'Garmin Geko'
          'Garmin eTrex Legend'
          'Garmin GPSMap CS60X'
          'FujiFilm 550EXR'
          'iPhone 4S')
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRTop'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRLeft'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRRight'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRBottom'
        Width = 150
        Visible = True
      end>
  end
  inherited DBNavigator1: TDBNavigator
    Top = 330
    Width = 220
    Hints.Strings = ()
  end
  inherited btnClose: TButton
    Left = 1343
  end
  inherited MainMenu1: TMainMenu
    inherited Edit1: TMenuItem
      object N1: TMenuItem
        Caption = '-'
      end
      object CopyStartLocation1: TMenuItem
        Caption = 'Copy Start Location'
        OnClick = CopyStartLocation1Click
      end
      object CopyEndLocation1: TMenuItem
        Caption = 'Copy End Location'
        OnClick = CopyEndLocation1Click
      end
      object CopyBRUpperLeft1: TMenuItem
        Caption = 'Copy BR Upper Left'
        OnClick = CopyBRUpperLeft1Click
      end
      object CopyBRBottomright1: TMenuItem
        Caption = 'Copy BR Bottom Right'
        OnClick = CopyBRBottomright1Click
      end
    end
    object Utilities1: TMenuItem
      Caption = 'Utilities'
      object MoveselectedTracks1: TMenuItem
        Caption = 'Move selected Tracks...'
        OnClick = MoveselectedTracks1Click
      end
    end
  end
end
