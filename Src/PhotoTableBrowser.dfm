inherited frmPhotoTableBrowser: TfrmPhotoTableBrowser
  Caption = 'Photo Table Browser'
  PixelsPerInch = 96
  TextHeight = 13
  inherited DBGrid1: TDBGrid
    Columns = <
      item
        Expanded = False
        FieldName = 'Key'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FILE_NAME'
        Title.Caption = 'File Name'
        Width = 153
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PHOTODATE'
        Title.Caption = 'PhotoDate'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PhotoDateTime'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDED'
        Title.Caption = 'Added'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'UPDATED'
        Title.Caption = 'Updated'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LocationID'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FILE_SIZE'
        Title.Caption = 'File Size'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'KEY_WORDS'
        Title.Caption = 'Key Words'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Width'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Height'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HasSound'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Distance'
        Title.Caption = 'Distance (MIles)'
        Width = 93
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Latitude'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Longitude'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TextInFile'
        Title.Caption = 'Text in File'
        Visible = True
      end>
  end
  inherited DBNavigator1: TDBNavigator
    Hints.Strings = ()
  end
end
