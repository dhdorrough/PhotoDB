inherited frmBoundariesBrowser: TfrmBoundariesBrowser
  Left = 476
  Top = 155
  Width = 1392
  Caption = 'Boundaries Browser'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblStatus: TLabel
    Left = 1249
  end
  inherited DBGrid1: TDBGrid
    Width = 1373
    Columns = <
      item
        Expanded = False
        FieldName = 'Abbrev'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 200
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRTop'
        Title.Caption = 'BRTop (Lat)'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRLeft'
        Title.Caption = 'BRLeft (Lon)'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRBottom'
        Title.Caption = 'BRBottom (Lat)'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BRRight'
        Title.Caption = 'BRRight (Lon)'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateUpdated'
        Title.Caption = 'Date Updated'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateAdded'
        Title.Caption = 'Date Added'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FileNames'
        Visible = True
      end>
  end
  inherited DBNavigator1: TDBNavigator
    Width = 220
    Hints.Strings = ()
  end
  inherited btnClose: TButton
    Left = 1297
    Top = 331
  end
  inherited MainMenu1: TMainMenu
    inherited Edit1: TMenuItem
      object N2: TMenuItem
        Caption = '-'
      end
      object CopyTopLeftLocation1: TMenuItem
        Caption = 'Copy Top, Left Location'
        OnClick = CopyTopLeftLocation1Click
      end
      object CopyBottomRightLocation1: TMenuItem
        Caption = 'Copy Bottom, Right Location'
        OnClick = CopyBottomRightLocation1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object PastetoTopLeftLocation1: TMenuItem
        Caption = 'Paste to Top, Left Location'
        OnClick = PastetoTopLeftLocation1Click
      end
      object PastetoBottomRightLocation1: TMenuItem
        Caption = 'Paste to Bottom, Right Location'
        OnClick = PastetoBottomRightLocation1Click
      end
    end
    object EditBoundaries: TMenuItem
      Caption = '&Edit'
      object CopyUpperLeftLatLong1: TMenuItem
        Caption = 'Copy Upper Left Lat/Long'
        OnClick = CopyUpperLeftLatLong1Click
      end
      object CopyBottomRightLatLong1: TMenuItem
        Caption = 'Copy Bottom Right Lat/Long'
        OnClick = CopyBottomRightLatLong1Click
      end
      object CopyDescription1: TMenuItem
        Caption = 'Copy Description'
        OnClick = CopyDescription1Click
      end
      object PasteUpperLeftLatLong1: TMenuItem
        Caption = 'Paste Upper Left Lat/Long'
        OnClick = PasteUpperLeftLatLong1Click
      end
      object PasteBottomRightLatLong1: TMenuItem
        Caption = 'Paste Bottom Right Lat/Long'
        OnClick = PasteBottomRightLatLong1Click
      end
      object PasteDescription1: TMenuItem
        Caption = 'Paste Description'
        OnClick = PasteDescription1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CopyRecordFields1: TMenuItem
        Caption = 'Copy Record Fields'
        OnClick = CopyRecordFields1Click
      end
      object PasteFieldsasNewRecord1: TMenuItem
        Caption = 'Paste Fields as New Record'
        OnClick = PasteFieldsasNewRecord1Click
      end
    end
  end
end
