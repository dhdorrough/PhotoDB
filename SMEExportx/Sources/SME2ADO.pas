{ SMExport suite
  TSMExportToADO component.
  PS: requires ADO/MS Jet

  Copyright (C) 2000-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2ADO;

interface
{$I SME.inc}

uses
  Windows, Classes, Graphics, DB, ExportDS, SME2OLE, SMEEngine
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TSMExportToADO = class(TSMExportToADODOA)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure InitializeConnection; override;
    procedure FinilizeConnection; override;
    procedure OpenDatabase(Value: string); override;
    function TableExists(const Value: string): Boolean; override;
    procedure DeleteTable(const Value: string); override;
    procedure CreateNewTable; override;
    procedure CreateNewTable1;
    procedure OpenRecordset; override;
    procedure AddNewRecord; override;
    procedure UpdateField(ACol: Integer; fld: TField; Value: string); override;
    procedure PostRecord; override;
  public
    { Public declarations }
    connection, recordset: Variant;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;
  published
    { Published declarations }
  end;

function PromptDataSource(ParentHandle: THandle; InitialString: WideString): WideString;
procedure GetADOTableNames(lst: TStrings; const ConnectionString: string; SystemTables: Boolean);
procedure GetADOTableFields(lst: TStrings; const ConnectionString, TableName: string);

implementation

uses ExCnst, SysUtils, ActiveX, OLECtrls, ComObj;

type
  IDataInitialize = interface(IUnknown)
    ['{2206CCB1-19C1-11D1-89E0-00C04FD7A829}']
    function GetDataSource(const pUnkOuter: IUnknown; dwClsCtx: DWORD;
      pwszInitializationString: POleStr; const riid: TIID;
      var DataSource: IUnknown): HResult; stdcall;
    function GetInitializationString(const DataSource: IUnknown;
      fIncludePassword: Boolean; out pwszInitString: POleStr): HResult; stdcall;
    function CreateDBInstance(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function CreateDBInstanceEx(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      pServerInfo: PCoServerInfo; cmq: ULONG; rgmqResults: PMultiQI): HResult; stdcall;
    function LoadStringFromStorage(pwszFileName: POleStr;
      out pwszInitializationString: POleStr): HResult; stdcall;
    function WriteStringToStorage(pwszFileName, pwszInitializationString: POleStr;
      dwCreationDisposition: DWORD): HResult; stdcall;
  end;

  DBPROMPTOPTIONS = TOleEnum;

  DBSOURCETYPE = DWORD;
  PDBSOURCETYPE = ^DBSOURCETYPE;

  IDBPromptInitialize = interface(IUnknown)
    ['{2206CCB0-19C1-11D1-89E0-00C04FD7A829}']
    function PromptDataSource(const pUnkOuter: IUnknown; hWndParent: HWND;
      dwPromptOptions: DBPROMPTOPTIONS; cSourceTypeFilter: ULONG;
      rgSourceTypeFilter: PDBSOURCETYPE; pszProviderFilter: POleStr;
      const riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function PromptFileName(hWndParent: HWND; dwPromptOptions: DBPROMPTOPTIONS;
      pwszInitialDirectory, pwszInitialFile: POleStr;
      var ppwszSelectedFile: POleStr): HResult; stdcall;
  end;

const
  CLSID_DATALINKS: TGUID = '{2206CDB2-19C1-11D1-89E0-00C04FD7A829}'; {DataLinks}

  DBPROMPTOPTIONS_PROPERTYSHEET = $2;

function PromptDataSource(ParentHandle: THandle; InitialString: WideString): WideString;
var
//  DataInit, DBPrompt: OLEVariant;
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: PWideChar;
begin
  Result := InitialString;
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  if InitialString <> '' then
    DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER,
      PWideChar(InitialString), IUnknown, DataSource);
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;
  if Succeeded(DBPrompt.PromptDataSource(nil, ParentHandle,
    DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown, DataSource)) then
  begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr);
    Result := InitStr;
  end;
end;

procedure LoadADO(var connection, recordset: Variant; const SourceFileName, SQL: string);
const
  adOpenForwardOnly = $00000000;
  adOpenStatic = $00000003;
  adLockReadOnly = $00000001;
  adCmdText = $00000001;
  adCmdTable = $00000002;

var
{$IFNDEF SMForDelphi5}
  EmptyParam: OleVariant;
{$ENDIF SMForDelphi5}
  Options: Integer;
begin
{$IFNDEF SMForDelphi5}
  TVarData(EmptyParam).VType := varError;
  TVarData(EmptyParam).VError := $80020004; {DISP_E_PARAMNOTFOUND}
{$ENDIF SMForDelphi5}

  {load ADO connection}
  try
    connection := CreateOleObject('ADODB.Connection');
  except
    connection := NULL
  end;

  {open a database}
  try
    connection.Open(SourceFileName);

    if (SQL <> '') then
    begin
      recordset := CreateOLEObject('ADODB.Recordset');
      if (Copy(UpperCase(SQL), 1, 7) = 'SELECT ') then
        Options := adCmdText
      else
        Options := adCmdTable;
      recordset.Open(SQL, connection, adOpenStatic{adOpenForwardOnly}, adLockReadOnly, Options);
//      recordset := connection.Execute(GetSelectSQLByTableName(SQL), EmptyParam, -1);
    end;
  except
    recordset := NULL;
    connection := NULL;
  end;
end;

procedure UnLoadADO(var connection, recordset: Variant);
begin
  if not VarIsNull(recordset) and
     not VarIsEmpty(recordset) then
  begin
    recordset.Close;
    recordset := UnAssigned;
  end;

  if not VarIsNull(connection) and
     not VarIsEmpty(connection) then
    connection := UnAssigned;
end;

procedure GetADOTableNames(lst: TStrings; const ConnectionString: string; SystemTables: Boolean);
var
  connection, recordset: Variant;
  i, intTypeField, intNameField: Integer;
begin
  if ConnectionString = '' then
    raise Exception.Create(Format(strPropertyError, ['ConnectionString']));

  lst.Clear;
  try
    LoadADO(connection, recordset, ConnectionString, '');

    {if database exists, then open it}
    try
      {load tables}
      recordset := connection.OpenSchema(20{siTables}{, EmptyParam, EmptyParam});
      for i := 0 to recordset.Fields.Count-1 do
      begin
        if (recordset.Fields.Item[i].Name = 'TABLE_TYPE') then
          intTypeField := i
        else
        if (recordset.Fields.Item[i].Name = 'TABLE_NAME') then
          intNameField := i
      end;
      while not recordset.Eof do
      begin
        if (recordset.Fields.Item[intTypeField].Value = 'TABLE') or
           (recordset.Fields.Item[intTypeField].Value = 'VIEW') or
           (SystemTables and (recordset.Fields.Item[intTypeField].Value = 'SYSTEM TABLE')) then         { do not localize }
          lst.Add(recordset.Fields.Item[intNameField].Value);
        recordset.MoveNext
      end;
    except
    end;
  finally
    UnLoadADO(connection, recordset);
  end;
end;

procedure GetADOTableFields(lst: TStrings; const ConnectionString, TableName: string);
var
  connection, recordset: Variant;
  i: Integer;
begin
  if ConnectionString = '' then
    raise Exception.Create(Format(strPropertyError, ['ConnectionString']));
  if (TableName = '') then
    raise Exception.Create(Format(strPropertyError, ['TableName']));

  lst.Clear;
  try
    LoadADO(connection, recordset, ConnectionString, TableName);

    {if database exists, then open it}
    try
      for i := 0 to recordset.Fields.Count-1 do
        lst.Add(recordset.Fields.Item[i].Name);
    except
    end;
  finally
    UnLoadADO(connection, recordset);
  end;
end;

{ TSMExportToADO }
constructor TSMExportToADO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teADO;
end;

function TSMExportToADO.Extension: string;
begin
  Result := '';
end;

function FieldTypeToADOType(const FieldType: TFieldType): TOleEnum;
const
  adEmpty = $00000000;
  adTinyInt = $00000010;
  adSmallInt = $00000002;
  adInteger = $00000003;
  adBigInt = $00000014;
  adUnsignedTinyInt = $00000011;
  adUnsignedSmallInt = $00000012;
  adUnsignedInt = $00000013;
  adUnsignedBigInt = $00000015;
  adSingle = $00000004;
  adDouble = $00000005;
  adCurrency = $00000006;
  adDecimal = $0000000E;
  adNumeric = $00000083;
  adBoolean = $0000000B;
  adError = $0000000A;
  adUserDefined = $00000084;
  adVariant = $0000000C;
  adIDispatch = $00000009;
  adIUnknown = $0000000D;
  adGUID = $00000048;
  adDate = $00000007;
  adDBDate = $00000085;
  adDBTime = $00000086;
  adDBTimeStamp = $00000087;
  adBSTR = $00000008;
  adChar = $00000081;
  adVarChar = $000000C8;
  adLongVarChar = $000000C9;
  adWChar = $00000082;
  adVarWChar = $000000CA;
  adLongVarWChar = $000000CB;
  adBinary = $00000080;
  adVarBinary = $000000CC;
  adLongVarBinary = $000000CD;
  adChapter = $00000088;
  adFileTime = $00000040;
  adDBFileTime = $00000089;
  adPropVariant = $0000008A;
  adVarNumeric = $0000008B;
begin
  case FieldType of
    ftUnknown: Result := adEmpty;
    {$IFDEF SMForDelphi4}
    ftWideString,
    {$ENDIF}
    ftString: Result := adVarChar;
    ftSmallint: Result := adSmallint;
    ftInteger, ftAutoInc: Result := adInteger;
    ftWord: Result := adUnsignedSmallInt;
    ftBoolean: Result := adBoolean;
    ftFloat: Result := adDouble;
    ftCurrency, ftBCD: Result := adCurrency;
    ftDate: Result := adDBDate;
    ftTime: Result := adDBTime;
    ftDateTime: Result := adDBTimeStamp;
    ftBytes: Result := adBinary;
    ftVarBytes: Result := adVarBinary;
    ftMemo: Result := adLongVarChar;
    ftBlob, ftGraphic..ftTypedBinary: Result := adLongVarBinary;
    {$IFDEF SMForDelphi4}
    ftFixedChar: Result := adChar;
    ftLargeint: Result := adBigInt;
    {$ENDIF}
    {$IFDEF SMForDelphi5}
    ftVariant: Result := adVariant;
    ftInterface: Result := adIUnknown;
    ftIDispatch: Result := adIDispatch;
    ftGuid: Result := adGUID;
    {$ENDIF}
  else
    Result := adVarChar;
  end;
end;

procedure TSMExportToADO.InitializeConnection;
begin
  inherited;

  if UseCurrentInstance then
  begin
    IsNewestCom := False;
    try
      connection := GetActiveOleObject('ADODB.Connection');
    except
      connection := CreateOleObject('ADODB.Connection');
      IsNewestCom := True;
    end
  end
  else
  begin
    connection := CreateOleObject('ADODB.Connection');
    IsNewestCom := True;
  end;
end;

procedure TSMExportToADO.FinilizeConnection;
begin
  UnLoadADO(connection, recordset)
end;

procedure TSMExportToADO.OpenDatabase(Value: string);
begin
  inherited;

  connection.Open(Value, UserName, Password);
end;

function TSMExportToADO.TableExists(const Value: string): Boolean;
var
  i, intTypeField, intNameField: Integer;
begin
  Result := inherited TableExists(Value);

  {load tables}
  recordset := connection.OpenSchema(20{siTables}{, EmptyParam, EmptyParam});
  for i := 0 to recordset.Fields.Count-1 do
  begin
    if (recordset.Fields.Item[i].Name = 'TABLE_TYPE') then
      intTypeField := i
    else
    if (recordset.Fields.Item[i].Name = 'TABLE_NAME') then
      intNameField := i
  end;
  while not recordset.Eof do
  begin
    if (CompareText(recordset.Fields.Item[intNameField].Value, TableName) = 0) and
       ((recordset.Fields.Item[intTypeField].Value = 'TABLE') or
        (recordset.Fields.Item[intTypeField].Value = 'VIEW') or
        (recordset.Fields.Item[intTypeField].Value = 'SYSTEM TABLE')) then         { do not localize }
    begin
      Result := True;
      break
    end;
    recordset.MoveNext
  end;
  recordset.Close;
end;

procedure TSMExportToADO.DeleteTable(const Value: string);
const
  adExecuteNoRecords = $00000080;

  adCmdText = $00000001;
//var
//  v: OLEVariant;
begin
//  connection.Execute('DROP TABLE ' + Value, v, adCmdText + adExecuteNoRecords)
end;

procedure TSMExportToADO.CreateNewTable;

  function FieldType2SSQL(ft: TFieldType; Size, Precision: Integer; Required: Boolean): string;
  begin
    case ft of
      ftSmallint,
      ftWord,
      ftInteger: Result := 'INTEGER';
      {$IFDEF SMForDelphi4}
      ftLargeint: Result := 'INTEGER';
      {$ENDIF}

      ftBoolean: Result := 'INTEGER';

      ftAutoInc: Result := 'IDENTITY';

      ftString: Result := 'CHAR(' + IntToStr(Size) + ')';

      ftFloat,
      ftBCD: begin
               if Precision = 0 then
                 Result := 'NUMBER'
               else
                 Result := 'DECIMAL(' + IntToStr(Size) + ',' + IntToStr(Precision) +')';
             end;

      ftCurrency: Result := 'MONEY';

      ftDate: Result := 'DATE';
      ftTime: Result := 'TIME';
      ftDateTime: Result := 'DATETIME';

      ftMemo,
      ftFmtMemo: Result := 'TEXT';
      ftBlob: Result := 'BLOB';
      ftGraphic: Result := 'IMAGE';
    else
      Result := '#UNKNOWN#';
    end;

    if Required then
      Result := Result + ' NOT NULL'
    else
      Result := Result + ' NULL'
  end;

const
  adExecuteNoRecords = $00000080;

  adCmdText = $00000001;
var
  v: OLEVariant;

  intCurColumn: Integer;
  strText, strCreateTable: string;
  fieldUser: TField;
  ft: TFieldType;
begin
  inherited;

  strCreateTable := '';
  for intCurColumn := 0 to Columns.Count-1 do
  begin
    with Columns[intCurColumn] do
    begin
      if Visible then
      begin
        with Columns[intCurColumn].Title do
        begin
          if (soUseFieldNameAsCaption in Options) then
            strText := FieldName
          else
            strText := Caption;
          if (Pos(' ', strText) > 0) then
            strText := '"' + strText + '"';
        end;

        if (soFieldMask in Options) then
          ft := ftString
        else
        begin
          if (Columns[intCurColumn].ColumnKind = ckField) then
            fieldUser := SourceDataEngine.FindFieldByColumn(Columns[intCurColumn])
          else
            fieldUser := nil;
          if Assigned(fieldUser) then
            ft := fieldUser.DataType
          else
            ft := CellType2FieldType(DataType);
        end;
        strCreateTable := strCreateTable + ' ' + strText + ' ' +
                          FieldType2SSQL(ft, Width, Precision,
                                         SourceDataEngine.RequiredByColumn(Columns[intCurColumn])) + ', ';
      end
    end
  end;
  if (strCreateTable <> '') then
  begin
    strCreateTable := Copy(strCreateTable, 1, Length(strCreateTable)-2);
    connection.Execute('CREATE TABLE ' + TableName + '(' + strCreateTable + ')', v, adCmdText + adExecuteNoRecords)
  end
end;

procedure TSMExportToADO.CreateNewTable1;
const
  adFldIsNullable = $00000020;
  adFldMayBeNull = $00000040;

  adOpenUnspecified = -1;
  adLockUnspecified = -1;
var
  j, intCurColumn, optFlags, intSize: Integer;

  strText: string;
  colorUser: TColor;
  alignUser: TAlignment;
  fieldUser: TField;
  ct: TCellType;

{$IFNDEF SMForDelphi5}
  EmptyParam: OleVariant;
{$ENDIF SMForDelphi5}
begin
  inherited;

  recordset := CreateOLEObject('ADODB.Recordset');
//  recordset.CursorLocation := adUseClient;

  for intCurColumn := 0 to Columns.Count-1 do
    with Columns[intCurColumn] do
      if Visible then
      begin
        with Columns[intCurColumn].Title do
        begin
          strText := FieldName;
          if (soUseFieldNameAsCaption in Options) then
            strText := FieldName
          else
            strText := Caption;
          colorUser := Color;
          fontUser.Assign(Font);
          alignUser := Alignment;
        end;

        if Assigned(OnGetCellParams) then
        begin
          ct := Columns[intCurColumn].DataType;
          OnGetCellParams(Self, nil, strText, fontUser, alignUser, colorUser, ct);
        end;

        case CharacterSet of
          csASCII_MSDOS: strText := UECode(True, strText);
          csEBCDIC: strText := EBCDICCode(strText);
        end;

        if (Columns[intCurColumn].ColumnKind = ckField) then
          fieldUser := SourceDataEngine.FindFieldByColumn(Columns[intCurColumn])
        else
          fieldUser := nil;
        if SourceDataEngine.RequiredByColumn(Columns[intCurColumn]) then
          optFlags := adFldIsNullable + adFldMayBeNull
        else
          optFlags := 0;
        if (Columns[intCurColumn].DataType in [ctMEMO, ctGraphic]) or
           (Assigned(fieldUser) and fieldUser.IsBlob) then
          intSize := High(Integer)
        else
          intSize := Columns[intCurColumn].Width;
        if (soFieldMask in Options) then
          j := FieldTypeToADOType(ftString)
        else
        if Assigned(fieldUser) then
          j := FieldTypeToADOType(fieldUser.DataType)
        else
          j := FieldTypeToADOType(CellType2FieldType(ct));
        recordset.Fields.Append(strText, j, intSize, optFlags);
      end;

{$IFNDEF SMForDelphi5}
  TVarData(EmptyParam).VType := varError;
  TVarData(EmptyParam).VError := $80020004; {DISP_E_PARAMNOTFOUND}
{$ENDIF SMForDelphi5}
  recordset.ActiveConnection := connection;
  recordset.Open(EmptyParam, EmptyParam, adOpenUnspecified, adLockUnspecified, 0);
  recordset.Close;
  recordset := UnAssigned;
end;

procedure TSMExportToADO.OpenRecordset;
const
  adCmdText = $00000001;
  adCmdTable = $00000002;

  adOpenKeyset = $00000001;
  adLockOptimistic = $00000003;

  adExecuteNoRecords = $00000080;
var
  optFlags: Integer;
begin
  inherited;

  recordset := CreateOLEObject('ADODB.Recordset');
  if (Copy(UpperCase(TableName), 1, 7) = 'SELECT ') then
    optFlags := adCmdText
  else
    optFlags := adCmdTable;
  recordset.Open(TableName, connection, adOpenKeyset, adLockOptimistic, optFlags {+ adExecuteNoRecords});
end;

procedure TSMExportToADO.AddNewRecord;
begin
  inherited;

  recordset.AddNew;
end;

procedure TSMExportToADO.UpdateField(ACol: Integer; fld: TField; Value: string);
var
  curField, v: OLEVariant;
  i: Integer;

  P, pData: PChar;
begin
  inherited;

  curField := recordset.Fields[Statistic.CurrentCol];
  if VarIsNull(Value) or (VarToStr(Value) = '') then
    curField.Value := NULL
  else
  if ((Assigned(fld) and
      fld.IsBlob and
      (fld.DataType <> ftMemo))) or
     (Columns[ACol].DataType in [ctMEMO, ctGraphic])  then
  begin
    i := Length(Value);
//    if (i > curField.FieldSize) then
//      i := curField.FieldSize;
    v := VarArrayCreate([0, i-1], VarByte);
    P := VarArrayLock(v);
    pData := PChar(Value);
    Move(pData[0], P[0], i);
    VarArrayUnlock(v);
//    strText := inttostr(curfield.fieldname);
    curField.Value := v;
  end
  else
    curField.Value := Value;
end;

procedure TSMExportToADO.PostRecord;
const
  dbUpdateRegular = 1;

  adEditInProgress = $00000001;
  adEditAdd = $00000002;
{$IFNDEF SMForDelphi5}
var
  EmptyParam: OleVariant;
{$ENDIF SMForDelphi5}
begin
  inherited;
  
{$IFNDEF SMForDelphi5}
  TVarData(EmptyParam).VType := varError;
  TVarData(EmptyParam).VError := $80020004; {DISP_E_PARAMNOTFOUND}
{$ENDIF SMForDelphi5}

  if (recordset.EditMode * (adEditInProgress + adEditAdd)) <> 0 then
    recordset.Update(EmptyParam, EmptyParam);
end;

end.

