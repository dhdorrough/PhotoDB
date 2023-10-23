inherited frmFuncKeyBrowser: TfrmFuncKeyBrowser
  Caption = 'Function Key Values'
  PixelsPerInch = 96
  TextHeight = 13
  inherited DBGrid1: TDBGrid
    Columns = <
      item
        Expanded = False
        FieldName = 'KeyName'
        ReadOnly = True
        Title.Caption = 'Key'
        Width = 57
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'KeyValue'
        Title.Caption = 'Value'
        Width = 539
        Visible = True
      end>
  end
  inherited DBNavigator1: TDBNavigator
    Hints.Strings = ()
  end
end
