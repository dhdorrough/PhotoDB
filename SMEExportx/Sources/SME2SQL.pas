{ SMExport suite
  TSMExportToSQL component

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2SQL;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

type
  TSMESQLTypes = class(TPersistent)
  private
    { Private declarations }
    FftString: string;
    FftSmallint: string;
    FftInteger: string;
    FftWord: string;
    FftBoolean: string;
    FftFloat: string;
    FftCurrency: string;
    FftBCD: string;
    FftDate: string;
    FftTime: string;
    FftDateTime: string;
    FftAutoInc: string;
    FftBlob: string;
    FftMemo: string;
    FftGraphic: string;
    FftLargeint: string;

    FCheckRequired: Boolean;
    FAddPrimaryKey: Boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property ftString: string read FftString write FftString;
    property ftSmallint: string read FftSmallint write FftSmallint;
    property ftInteger: string read FftInteger write FftInteger;
    property ftWord: string read FftWord write FftWord;
    property ftBoolean: string read FftBoolean write FftBoolean;
    property ftFloat: string read FftFloat write FftFloat;
    property ftCurrency: string read FftCurrency write FftCurrency;
    property ftBCD: string read FftBCD write FftBCD;
    property ftDate: string read FftDate write FftDate;
    property ftTime: string read FftTime write FftTime;
    property ftDateTime: string read FftDateTime write FftDateTime;
    property ftAutoInc: string read FftAutoInc write FftAutoInc;
    property ftBlob: string read FftBlob write FftBlob;
    property ftMemo: string read FftMemo write FftMemo;
    property ftGraphic: string read FftGraphic write FftGraphic;
    property ftLargeint: string read FftLargeint write FftLargeint;

    property CheckRequired: Boolean read FCheckRequired write FCheckRequired default True;
    property AddPrimaryKey: Boolean read FAddPrimaryKey write FAddPrimaryKey default False;
  end;

  TGetCreateTable = procedure(Sender: TObject; var SQL, Prefix, Suffix: string) of object;
  TFieldCharCase = (ecNormal, ecUpperCase, ecLowerCase, ecProper);

  TSMExportToSQL = class(TSMExportToCellFile)
  private
    { Private declarations }
    RecInBatch: Integer;

    strFieldNames,

    FSQLTerm: string;
    FSQLQuote: Char;
    FCommitTerm: string;
    FBatchRecCount: Integer;

    FAddCreateTable: Boolean;
    FFieldNameCharCase: TFieldCharCase;

    FOnGetCreateTable: TGetCreateTable;

    FSQLTypes: TSMESQLTypes;
    procedure SetSQLTypes(Value: TSMESQLTypes);
  protected
    { Protected declarations }
    function MergeIsSupported: Boolean; override;
    function TitleIsSupported: Boolean; override;

    function GetFieldStr(Field: TField): string; override;
    function GetFieldName(const Value: string): string;
  public
    { Public declarations }
    procedure WriteCommitTerm; virtual;
    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;
    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;

    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Extension: string; override;
  published
    { Published declarations }
    property ActionAfterExport;
    property FileName;

    property AddCreateTable: Boolean read FAddCreateTable write FAddCreateTable;
    property CommitTerm: string read FCommitTerm write FCommitTerm;
    property BatchRecCount: Integer read FBatchRecCount write FBatchRecCount default MaxInt;
    property FieldNameCharCase: TFieldCharCase read FFieldNameCharCase write FFieldNameCharCase default ecNormal;
    property SQLQuote: Char read FSQLQuote write FSQLQuote;
    property SQLTerm: string read FSQLTerm write FSQLTerm;
    property SQLTypes: TSMESQLTypes read FSQLTypes write SetSQLTypes;

    property TableName;

    property OnGetCreateTable: TGetCreateTable read FOnGetCreateTable write FOnGetCreateTable;

    property AddTitle;
    property CharacterSet;

    property Header;
    property Footer;
    property OnGetCellParams;
  end;

const
  CreateTableTemplate: string = 'CREATE TABLE %s (%s)%s';

implementation

uses SysUtils;

{ TSMESQLTypes }
constructor TSMESQLTypes.Create;
begin
  inherited Create;

  FftString := 'CHAR(%d)';
  FftSmallint := 'INTEGER';
  FftInteger := 'INTEGER';
  FftWord := 'INTEGER';
  FftBoolean := 'INTEGER';
  FftFloat := 'DECIMAL';
  FftCurrency := 'MONEY';
  FftBCD := 'DECIMAL';
  FftDate := 'DATETIME';
  FftTime := 'DATETIME';
  FftDateTime := 'DATETIME';
  FftAutoInc := 'IDENTITY';
  FftBlob := 'BLOB';
  FftMemo := 'BLOB';
  FftGraphic := 'IMAGE';
  FftLargeint:= 'INTEGER';

  FCheckRequired := True;
  FAddPrimaryKey := False;
end;

procedure TSMESQLTypes.Assign(Source: TPersistent);
var
  sql: TSMESQLTypes;
begin
  if Source is TSMESQLTypes then
  begin
    sql := TSMESQLTypes(Source);

    ftString := sql.ftString;
    ftSmallint := sql.ftSmallint;
    ftInteger := sql.ftInteger;
    ftWord := sql.ftWord;
    ftBoolean := sql.ftBoolean;
    ftFloat := sql.ftFloat;
    ftCurrency := sql.ftCurrency;
    ftBCD := sql.ftBCD;
    ftDate := sql.ftDate;
    ftTime := sql.ftTime;
    ftDateTime := sql.ftDateTime;
    ftAutoInc := sql.ftAutoInc;
    ftBlob := sql.ftBlob;
    ftMemo := sql.ftMemo;
    ftGraphic := sql.ftGraphic;
    ftLargeint:= sql.ftLargeint;
  end
  else
    inherited Assign(Source);
end;

{ TSMExportToSQL }
constructor TSMExportToSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSQLTypes := TSMESQLTypes.Create;

  Options := Options - [soBlankRowAfterCaptions];
  FAddCreateTable := False;
  FSQLTerm := ';';
  FSQLQuote := '"';
  FCommitTerm := 'COMMIT;';
  FBatchRecCount := MaxInt;
  TableType := teSQL;
  FFieldNameCharCase := ecNormal;
end;

destructor TSMExportToSQL.Destroy;
begin
  FSQLTypes.Free;

  inherited Destroy;
end;

procedure TSMExportToSQL.SetSQLTypes(Value: TSMESQLTypes);
begin
  FSQLTypes.Assign(Value)
end;

function TSMExportToSQL.MergeIsSupported: Boolean;
begin
  Result := True
end;

function TSMExportToSQL.TitleIsSupported: Boolean;
begin
  Result := False
end;

function TSMExportToSQL.Extension: string;
begin
  Result := '.SQL'
end;

function TSMExportToSQL.GetFieldStr(Field: TField): string;
begin
  if BlankIfZero and Field.IsNull then
    Result := 'NULL'
  else
  begin
    case Field.DataType of
      ftDate: Result := FormatDateTime(DataFormats.GetDateFormat, Field.AsDateTime);
      ftTime: Result := FormatDateTime(DataFormats.GetTimeFormat, Field.AsDateTime);
      ftDateTime: Result := FormatDateTime(DataFormats.GetDateTimeFormat, Field.AsDateTime);
      ftBoolean: begin
                   if Field.AsBoolean then
                     Result := DataFormats.BooleanTrue
                   else
                     Result := DataFormats.BooleanFalse
                 end
    else
      if (Field is TNumericField) and Field.IsNull then
        Result := '0'
      else
      begin
        Result := inherited GetFieldStr(Field);//Field.AsString;
        if (Field is TNumericField) and BlankIfZero and (Result = '') then
          Result := '0'
      end;
    end;
  end;
end;

function TSMExportToSQL.GetFieldName(const Value: string): string;
begin
  case FieldNameCharCase of
    ecUpperCase: Result := AnsiUpperCase(Value);
    ecLowerCase: Result := AnsiLowerCase(Value);
    ecProper: begin
                Result := AnsiLowerCase(Value);
                if (Result <> '') then
                  Result[1] := AnsiUpperCase(Result[1])[1];
              end
  else //ecNormal
    Result := Value;
  end;
end;

procedure TSMExportToSQL.WriteFileBegin;
var
  strPK: string;

  function FieldType2SSQL(const strFieldName: string; AField: TField): string;

    function GetSQLType(const Value: string): string;
    begin
      if (Pos('%d', Value) > 0) then
      begin
        if (AField is TFloatField) then
          Result := Format(Value, [AField.DataSize, TFloatField(AField).Precision])
        else
          Result := Format(Value, [AField.DataSize])
      end
      else
        Result := Value;
    end;

  begin
    case AField.DataType of
      ftSmallint: Result := GetSQLType(SQLTypes.ftSmallint);
      ftWord: Result := GetSQLType(SQLTypes.ftWord);
      ftInteger: Result := GetSQLType(SQLTypes.ftInteger);
      {$IFDEF SMForDelphi4}
      ftLargeint: Result := GetSQLType(SQLTypes.ftLargeint);
      {$ENDIF}

      ftBoolean: Result := GetSQLType(SQLTypes.ftBoolean);

      ftAutoInc: Result := GetSQLType(SQLTypes.ftAutoInc);

      ftString: Result := GetSQLType(SQLTypes.ftString);

      ftFloat: Result := GetSQLType(SQLTypes.ftFloat);
      ftBCD: Result := GetSQLType(SQLTypes.ftBCD);
      ftCurrency: Result := GetSQLType(SQLTypes.ftCurrency);

      ftDate: Result := GetSQLType(SQLTypes.ftDate);
      ftTime: Result := GetSQLType(SQLTypes.ftTime);
      ftDateTime: Result := GetSQLType(SQLTypes.ftDateTime);

      ftGraphic: Result := GetSQLType(SQLTypes.ftGraphic);
    else
      if AField.IsBlob then
      begin
        if (AField.DataType in [ftMemo, ftFmtMemo]) then
          Result := GetSQLType(SQLTypes.ftMemo)
        else
          Result := GetSQLType(SQLTypes.ftBlob);
      end
      else
        Result := '#UNKNOWN#';
    end;

    if SQLTypes.CheckRequired then
    begin
      if AField.Required then
        Result := Result + ' NOT NULL'
      else
        Result := Result + ' NULL'
    end;
    if AField.Required then
    begin
      if (strPK = '') then
        strPK := strFieldName
      else
        strPK := strPK + ', ' + strFieldName
    end
  end;

var
  i: Integer;
  s, strCreateTable, strPrefix, strSuffix: string;
begin
  inherited;

  RecInBatch := 0;

  {fill a field names}
  strFieldNames := '';
  strCreateTable := '';
  strPK := '';
  for i := 0 to Columns.Count-1 do
    if Columns[i].Visible then
    begin
      if (soUseFieldNameAsCaption in Options) then
        s := GetFieldName(Columns[i].Title.Caption)
      else
        s := GetFieldName(Columns[i].FieldName);
      if (FSQLQuote <> #0) and (Pos(' ', s) > 0) then
        s := FSQLQuote + s + FSQLQuote;
      strFieldNames := strFieldNames + ', ' + s;
      if AddCreateTable then
        strCreateTable := strCreateTable + #13#10'   ' + s + ' ' + FieldType2SSQL(s, SourceDataEngine.FindFieldByColumn(Columns[i])) + ', ';
    end;

  if AddCreateTable and
     SQLTypes.AddPrimaryKey and (strPK <> '') then
    strCreateTable := strCreateTable + #13#10'   PRIMARY KEY (' + strPK + '), ';

  if (strFieldNames <> '') then
    Delete(strFieldNames, 1, 2);
  if AddCreateTable and
     (strCreateTable <> '') then
    strCreateTable := Copy(strCreateTable, 1, Length(strCreateTable)-2);

  {fill a table name}
  if (TableName = '') then
    TableName := TblName(GetDS);
  if (TableName = '') then
    TableName := Name;

  if AddCreateTable then
  begin
    strPrefix := '';
    strSuffix := '';
    if Assigned(OnGetCreateTable) then
      OnGetCreateTable(Self, strCreateTable, strPrefix, strSuffix);
    strCreateTable := strPrefix + Format(CreateTableTemplate, [TableName, strCreateTable, FSQLTerm]) + strSuffix + #13#10#13#10;

    WriteString(strCreateTable)
  end;
end;

procedure TSMExportToSQL.WriteFileEnd;
begin
  inherited;

  WriteCommitTerm;
end;

procedure TSMExportToSQL.WriteRowStart(IsAddedTitle: Boolean);
begin
  inherited;

  if IsDataArea then
    WriteString('INSERT INTO ' + TableName + #13#10 +
                '  (' + strFieldNames + ')' + #13#10 +
                'VALUES' + #13#10 +
                '  (');
end;

procedure TSMExportToSQL.WriteRowEnd(IsAddedTitle: Boolean);
begin
  inherited;

  if IsDataArea then
  begin
    WriteString(')' + FSQLTerm + #13#10#13#10);

    {batch is completed and the COMMIT is required?}
    if not IsAddedTitle then
      WriteCommitTerm;
  end
  else
    WriteString(#13#10);
end;

procedure TSMExportToSQL.WriteCommitTerm;
begin
  Inc(RecInBatch);
  if (RecInBatch >= FBatchRecCount) then
  begin
    WriteString(FCommitTerm + #13#10#13#10);
    RecInBatch := 0;
  end;
end;

procedure TSMExportToSQL.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  s: string;
  i: Integer;
  IsCommaSeparator: Boolean;
begin
  inherited;

  s := '';
  IsCommaSeparator := False;
  if (FSQLQuote <> #0) then
  begin
    for i := 1 to Length(AString) do
    begin
      if (AString[i] = FSQLQuote) then
        s := s + FSQLQuote + FSQLQuote
      else
        s := s + AString[i];
      if (AString[i] = DecimalSeparator) then
        IsCommaSeparator := True;
    end;
  end;

  if IsDataArea then
  begin
    if (FSQLQuote <> #0) and (not BlankIfZero or (BlankIfZero and (s <> 'NULL'))) then
      if Assigned(fld) then
      begin
        if (not (fld is TNumericField) and not (fld.DataType = ftBoolean)) or
           ((fld is TNumericField) and IsCommaSeparator and (DecimalSeparator = ',')) and
           (CellType <> ctBlank) then
          AString := FSQLQuote + s + FSQLQuote; 
      end
      else
      begin
        if (CellType in [ctString, ctDateTime, ctDate, ctCurrency, ctTime, ctMEMO]) or
            ((CellType in [ctDouble, ctCurrency]) and (DecimalSeparator = ',') and IsCommaSeparator) then
           AString := FSQLQuote + s + FSQLQuote;
      end;

    if (ACol < LastExportColumnIndex-1) then
      AString := AString + ', ';
  end
  else
    AString := '/* ' + AString + ' */';
  WriteString(AString)
end;

end.

