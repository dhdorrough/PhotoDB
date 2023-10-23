inherited frmLocationsBrowser: TfrmLocationsBrowser
  Left = 506
  Top = 342
  Width = 1083
  Height = 391
  Caption = ' Locations Browser'
  PixelsPerInch = 96
  TextHeight = 14
  inherited lblStatus: TLabel
    Left = 1024
    Top = 316
  end
  object lblDistanceAway: TLabel [1]
    Left = 241
    Top = 313
    Width = 82
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'lblDistanceAway'
  end
  inherited DBGrid1: TDBGrid
    Left = 1
    Top = 2
    Width = 1163
    Height = 296
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'State'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Latitude'
        Width = 96
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Longitude'
        Width = 110
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 264
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateAdded'
        Width = 121
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateUpdated'
        Width = 121
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PrivateLocation'
        Title.Caption = 'Private'
        Width = 42
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'RefCnt'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LocationSource'
        Title.Caption = 'Source'
        Visible = True
      end>
  end
  inherited DBNavigator1: TDBNavigator
    Top = 309
    Width = 210
    Hints.Strings = ()
  end
  inherited btnClose: TButton
    Left = 1081
    Top = 329
    Caption = '&OK'
    Default = True
  end
  inherited MainMenu1: TMainMenu
    inherited Navigate1: TMenuItem
      object N1: TMenuItem
        Caption = '-'
      end
      object OrderBy1: TMenuItem
        Caption = 'Order By'
        object ID1: TMenuItem
          Caption = 'ID'
          RadioItem = True
          OnClick = ID1Click
        end
        object State1: TMenuItem
          Caption = 'State'
          RadioItem = True
          OnClick = State1Click
        end
        object Latitude1: TMenuItem
          Caption = 'Latitude'
          RadioItem = True
          OnClick = Latitude1Click
        end
        object Longitude1: TMenuItem
          Caption = 'Longitude'
          RadioItem = True
          OnClick = Longitude1Click
        end
        object Description1: TMenuItem
          Caption = 'Description'
          RadioItem = True
          OnClick = Description1Click
        end
        object DateAdded1: TMenuItem
          Caption = 'Date Added'
          RadioItem = True
          OnClick = DateAdded1Click
        end
        object DateUpdated1: TMenuItem
          Caption = 'Date Updated'
          RadioItem = True
          OnClick = DateUpdated1Click
        end
        object RefCount1: TMenuItem
          Caption = 'Ref Count'
          RadioItem = True
          OnClick = RefCount1Click
        end
      end
    end
    object Utilities1: TMenuItem
      Caption = 'Utilities'
      object DeleteSelectedRecords1: TMenuItem
        Caption = 'Delete Selected Records'
        OnClick = DeleteSelectedRecords1Click
      end
      object FillFieldinSelectedRecords1: TMenuItem
        Caption = 'Fill Field in Selected Records...'
        OnClick = FillFieldinSelectedRecords1Click
      end
    end
  end
end
