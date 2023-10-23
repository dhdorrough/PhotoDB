unit TableCreation;

interface

uses
  ADODB, ADOX_TLB, MyTables, MyTables_Decl, DB;

type
  TDBCreator = class
  private
    fConnectionString: string;
    fConnection: TADOConnection;
    fADOXCatalog: TADOXCatalog;
    fADOCommand: TADOCommand;
  public
    procedure CreateDatabase(var FileName: string; DBVersion: TDBVersion);
    procedure CreateTable(TableName: string; DataSet: TDataSet);
    Destructor Destroy; override;
  end;


implementation

uses
  MyUtils, SysUtils;

procedure TDBCreator.CreateDatabase(var FileName: string; DBVersion: TDBVersion);
begin { TDBCreator.CreateDatabase }
  case DBVersion of
    dv_Access2000:
      begin
        FileName          := ForceExtension(FileName, MDB_EXT);
        fConnectionString := Format(ACCESS_2000_CONNECTION_STRING, [ACCESS_2000_PROVIDER, FileName]);
      end;
    dv_Access2007:
      begin
        FileName          := ForceExtension(FileName, ACCDB_EXT);
        fConnectionString := Format(ACCESS_2007_CONNECTION_STRING, [ACCESS_2007_PROVIDER, FileName]);
      end;
    dv_Access2019:
      begin
        FileName          := ForceExtension(FileName, ACCDB_EXT);
        fConnectionString := Format(ACCESS_2019_CONNECTION_STRING, [ACCESS_2007_PROVIDER, FileName]);
      end;
  end;

  fADOXCatalog := TADOXCatalog.Create(nil);
  fADOCommand  := TADOCommand.Create(nil);

  fADOXCatalog.Create1(fConnectionString);
  fConnection := MyConnection(FileName);
  fADOCommand.Connection := fConnection;

  fConnection.Close;
end;  { TDBCreator.CreateDatabase }


procedure TDBCreator.CreateTable(TableName: string; DataSet: TDataSet);
const
  CRLF = #13#10;
var
  cs: string;
  i: integer;
  FldName: string;
  DataType: TDataType;
  DataTypeString: string;
  Line: string;
  nlines: integer;
begin { TDBCreator.CreateTable }
  cs := '';
  nlines := 0;
  for i := 0 to DataSet.Fields.Count - 1 do
    begin
      FldName  := Format('[%s]', [DataSet.Fields[i].FieldName]);
      DataType := DataSet.Fields[i].DataType;
      case DataType of
        ftWideString,
        ftString:
          DataTypeString := Format('Text(%d)', [DataSet.Fields[i].Size]);

        ftSmallint,
        ftInteger,
        ftWord,
        ftLargeint,
        ftAutoInc:
          DataTypeString := 'INT';

        ftDate,
        ftTime,
        ftDateTime:
          DataTypeString := 'DATETIME';

        ftBoolean:
          DataTypeString := 'YESNO';

        ftMemo,
        ftBlob,
        ftFmtMemo:
          DataTypeString := 'MEMO';

        ftFloat,
        ftCurrency:
          DataTypeString := 'FLOAT';

        else
          AlertFmt('System error: Unhandled datatype: %s', [ord(DataType)]);
      end;

      Line := Format('%s %s', [FldName, DataTypeString]);

      if cs = '' then
        cs := Format('CREATE TABLE %s (', [TableName]);

      if nlines = 0 then
        cs := cs + Line
      else
        cs := cs + ',' + CRLF +
              Line;

      inc(nlines);
    end;
  cs := cs + ')';

  fADOCommand.CommandText := cs;
  fADOCommand.Execute;
end;  { TDBCreator.CreateTable }


destructor TDBCreator.Destroy;
begin
  FreeAndNil(fADOXCatalog);
  FreeAndNil(fADOCommand);
  inherited;
end;

end.
