{ SMExport suite
  TSMExportToBDE and TSMExportMonitor components.

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2BDE;

interface

{$I sme.inc}

uses
  SysUtils, Classes, DB,
  {$IFDEF BDE_SUPPORT} DBTables, {$ENDIF BDE_SUPPORT}
  ExportDS, SME2DBF;

type
  TSMExportToBDE = class(TSMExportBaseComponent)
  private
    { Private declarations }
  {$IFDEF BDE_SUPPORT}
    FTableType: TTableType;
  {$ENDIF BDE_SUPPORT}
  protected
    { Protected declarations }
    procedure Prepare; override;
  public
    { Public declarations }
    function Extension: string; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
  {$IFDEF BDE_SUPPORT}
    property TableType: TTableType read FTableType write FTableType;
  {$ENDIF BDE_SUPPORT}

    property ActionAfterExport;
    property FileName;

    property CharacterSet;
//    property OnGetCellParams;
  end;

  TSMExportMonitor = class(TSMExportBaseComponent)
  private
    { Private declarations }
    FFormats: TExportFormatTypes;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ExportDataSet;
    procedure ExportToDBF;
    procedure ExportToText;
    procedure ExportToExcel;
    procedure ExportToWord;
    procedure ExportToCellFile;
    procedure ExportToAccess;
    procedure ExportToADO;

    procedure Execute(withSetParam: Boolean);
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Formats: TExportFormatTypes read FFormats write FFormats;

    property TextQualifier;
    property Separator;
    property RecordSeparator;

    property Fixed;
    property TableName;
    property TableType;

    property ActionAfterExport;
    property FileName;
    property RowsPerFile;
    property Layout;

    property AddTitle;
    property CharacterSet;

    property ExportStyle;

    property Header;
    property Footer;

    property OnGetCellParams;
  end;

function GetAvailableExportFormats: TExportFormatTypes;


implementation

uses BDE, ExCnst, SMEMonitor, SMEStat, SME2Cell, SMEEngine,
     SME2TXT, SME2HTML, SME2OLE, SME2XLS,
     SME2SYLK, SME2DIF, SME2WKS, SME2WQ, SME2SQL,
     SME2XML, SME2CLP, SME2RTF, SME2SPSS, SME2PDF, SME2LDIF, SME2ADO
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

function IsBDEAvailable: Boolean;
begin
    Result := True
{  try
    Check(dbiInit(nil));
    Result := True
  except
    Result := False
  end;
}end;

function GetAvailableExportFormats: TExportFormatTypes;
begin
  Result := AllFormats;
  if not IsBDEAvailable() then
    Result := Result - [teParadox{, teDBase}];
end;

{ TSMExportToBDE }
function TSMExportToBDE.Extension: string;
begin
  {$IFDEF BDE_SUPPORT}
  case TableType of
    ttParadox: Result := '.DB';
    ttDBase: Result := '.DBF';
    ttASCII: Result := '.TXT';
  end;
  {$ENDIF BDE_SUPPORT}
end;

procedure TSMExportToBDE.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  {$IFDEF BDE_SUPPORT}
  if (Source is TSMExportToBDE) then
    TableType := TSMExportToBDE(Source).TableType;
  {$ENDIF BDE_SUPPORT}
end;

procedure TSMExportToBDE.Prepare;
{$IFDEF BDE_SUPPORT}
var
  j, k: Integer;
  DestTable: TTable;
  arrFields: Variant;

  fld: TField;
  Accept, IsAborted: Boolean;
  strFieldName, strFileName: string;
  ft: TFieldType;
{$ENDIF BDE_SUPPORT}
begin
{$IFDEF BDE_SUPPORT}
  DestTable := TTable.Create(Self);
  try
    DestTable.TableName := FileName;
    DestTable.TableType := TableType;

    strFileName := FileName;
    if Assigned(OnGetFileName) then
      OnGetFileName(Self, 0, strFileName);

    if (soMergeData in Options) and
       FileExists(strFileName) then
    else
    begin
      for j := 0 to Columns.Count-1 do
        if Columns[j].Visible then
        begin
          if (Columns[j].ColumnKind = ckField) then
            fld := SourceDataEngine.FindFieldByColumn(Columns[j])
          else
            fld := nil;

          if (soUseFieldNameAsCaption in Options) then
            strFieldName := Columns[j].Title.Caption
          else
            strFieldName := Columns[j].FieldName;
          if Assigned(fld) then
          begin
    {$IFDEF SMForDelphi4}
            if fld.DataType = ftWideString then
              ft := ftString
            else
    {$ENDIF}
              ft := fld.DataType;
            DestTable.FieldDefs.Add(strFieldName, ft, fld.Size, fld.Required)
          end
          else
          if (Columns[j].ColumnKind in [ckConstant, ckSysVar]) then
            DestTable.FieldDefs.Add(strFieldName,
                                    CellType2FieldType(Columns[j].DataType),
                                    Columns[j].Width,
                                    False)
          else
            DestTable.FieldDefs.Add(strFieldName,
                                    SourceDataEngine.DataTypeByColumn(Columns[j]),
                                    Columns[j].Width,
                                    SourceDataEngine.RequiredByColumn(Columns[j]));
        end;
      DestTable.CreateTable;
    end;

    DestTable.Open;
    arrFields := VarArrayCreate([0, Columns.Count-1], varOLEStr);
    k := 0;
    for j := 0 to Columns.Count-1 do
      if Columns[j].Visible then
      begin
        fld := DestTable.FindField(Columns[j].FieldName);
        if not Assigned(fld) and
           (k < DestTable.FieldCount) then
          fld := DestTable.Fields[k];
        if Assigned(fld) then
          arrFields[j] := fld.FieldName;
        Inc(k);
      end;

    SourceDataEngine.DisableControls;
    SourceDataEngine.SavePosition;
    SourceDataEngine.First;

    Statistic.TotalCount := 0;
    Statistic.CurrentRow := 0;
    Statistic.CurrentSection := esData;
    while (not ProgressCanceled) and
          IsTrialCount(Statistic.TotalCount) and
          (not SourceDataEngine.EOF) do
    begin
      if AnimatedStatus then
        UpdateProgressDlg(GetLanguageString(8), Statistic.TotalCount);

      Accept := True;
      if Assigned(OnBeforeRecord) then
        OnBeforeRecord(Self, Accept);
      if Accept then
      begin
        if Assigned(OnProgress) then
        begin
          IsAborted := False;
          OnProgress(Self, Statistic.TotalCount, SourceDataEngine.RecordCount, IsAborted);
          if IsAborted then
          begin
            SetProgressCanceled(True);
            break
          end
        end;

        Statistic.TotalCount := Statistic.TotalCount+1;

        DestTable.Append;
        Statistic.CurrentCol := -1;
        for j := 0 to Columns.Count-1 do
          if Columns[j].Visible then
          begin
            Statistic.CurrentCol := Statistic.CurrentCol + 1;
            with DestTable.FieldByName(arrFields[j]) do
            begin
              if DataType = ftAutoInc then continue;

              if (Columns[j].ColumnKind = ckField) then
                Value := SourceDataEngine.GetFieldValue(Columns[j])
              else
                Value := Columns[j].FieldName;
            end
          end;
        DestTable.Post;
      end;
      SourceDataEngine.Next;
      Statistic.CurrentRow := Statistic.CurrentRow + 1;

      IsAborted := False;
      if Assigned(OnAfterRecord) then
        OnAfterRecord(Self, IsAborted);
      if IsAborted then
        SetProgressCanceled(True)
    end;
    SourceDataEngine.RestorePosition;
    SourceDataEngine.EnableControls;
  finally
    DestTable.Free;
  end;
  {$ELSE}
  raise Exception.Create('SMExport compiled with disabled BDE_SUPPORT directive')
  {$ENDIF BDE_SUPPORT}
end;

{ TSMExportMonitor }
constructor TSMExportMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Formats := GetAvailableExportFormats();
  TableType := teText;
end;

procedure TSMExportMonitor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if (Source is TSMExportMonitor) then
    Formats := TSMExportMonitor(Source).Formats;
end;

procedure TSMExportMonitor.ExportDataSet;
begin
  with TSMExportToBDE.Create(Self) do
    try
      Assign(Self);

  {$IFDEF BDE_SUPPORT}
      case Self.TableType of
//        teDBase: TableType := ttDBase;
        teParadox: TableType := ttParadox
      else
        TableType := ttDefault;
      end;
  {$ENDIF BDE_SUPPORT}

      Execute;
      Self.Statistic.Assign(Statistic)
    finally
      Free
    end;
end;

procedure TSMExportMonitor.ExportToDBF;
begin
  with TSMExportToDBF.Create(Self) do
    try
      Assign(Self);

      Execute;
      Self.Statistic.Assign(Statistic)
    finally
      Free
    end;
end;

procedure TSMExportMonitor.ExportToText;
var
  sme: TSMExportToText;
begin
  case TableType of
    teClipboard: sme := TSMExportToClipboard.Create(Self);
  else
    sme := TSMExportToText.Create(Self);
  end;

  try
    sme.Assign(Self);

    sme.Execute;
    Self.Statistic.Assign(sme.Statistic)
  finally
    sme.Free
  end;
end;

procedure TSMExportMonitor.ExportToExcel;
begin
  with TSMExportToExcel.Create(Self) do
    try
      Assign(Self);

      Execute;
      Self.Statistic.Assign(Statistic)
    finally
      Free
    end;
end;

procedure TSMExportMonitor.ExportToWord;
begin
  with TSMExportToWord.Create(Self) do
    try
      Assign(Self);

      Execute;
      Self.Statistic.Assign(Statistic)
    finally
      Free
    end;
end;

procedure TSMExportMonitor.ExportToCellFile;
var
  sme: TSMExportToCellFile;
begin
  case TableType of
    teXLS: sme := TSMExportToXLS.Create(Self);
    teSYLK: sme := TSMExportToSYLK.Create(Self);
    teDIF: sme := TSMExportToDIF.Create(Self);
    teWKS: sme := TSMExportToWKS.Create(Self);
    teQuattro: sme := TSMExportToQuattro.Create(Self);
    teHTML: sme := TSMExportToHTML.Create(Self);
    teXML: sme := TSMExportToXML.Create(Self);
    teSQL: sme := TSMExportToSQL.Create(Self);
    teSPSS: sme := TSMExportToSPSS.Create(Self);
    teLDIF: sme := TSMExportToLDIF.Create(Self);
    tePDF: sme := TSMExportToPDF.Create(Self);
    teRTF: sme := TSMExportToRTF.Create(Self);
  else
    exit
  end;

  try
    sme.Assign(Self);

    sme.Execute;
    Self.Statistic.Assign(sme.Statistic)
  finally
    sme.Free
  end;
end;

procedure TSMExportMonitor.ExportToAccess;
begin
  with TSMExportToAccess.Create(Self) do
    try
      Assign(Self);

      Execute;
      Self.Statistic.Assign(Statistic)
    finally
      Free
    end;
end;

procedure TSMExportMonitor.ExportToADO;
begin
  with TSMExportToADO.Create(Self) do
    try
      Assign(Self);

      Execute;
      Self.Statistic.Assign(Statistic)
    finally
      Free
    end;
end;

procedure TSMExportMonitor.Execute(withSetParam: Boolean);
var
  i: Integer;
  strFileName, strTableName: string;
  ttTableType: TTableTypeExport;
  csCharacterSet: TCharacterSet;
  chSeparator: Char;
  WithAddTitle, IsFixed, IsBlankIfZero: Boolean;
begin
  if (ColumnSource = csDataSet) then
    i := 2
  else
  begin
    if SelectedRecord then
    begin
      if IsWWDBGrid then
        i := 2
      else
        i := 1
    end
    else
      i := 0;
  end;

  strFileName := FileName;
  if (TableName = '') then
    strTableName := TblName(GetDS)
  else
    strTableName := TableName;
  ttTableType := TableType;
  csCharacterSet := CharacterSet;
  chSeparator := Separator;
  WithAddTitle := AddTitle;
  IsFixed := Fixed;
  IsBlankIfZero := BlankIfZero;
  if (not withSetParam) or
     (withSetParam and SetExportParam(Self,
                                      strFileName,
                                      strTableName,
                                      ttTableType,
                                      csCharacterSet,
                                      chSeparator,
                                      WithAddTitle,
                                      IsFixed, IsBlankIfZero, i,
                                      Formats)) then
  begin
    TableType := ttTableType;
    FileName := strFileName;
    TableName := strTableName;

    CharacterSet := csCharacterSet;
    Separator := chSeparator;
    AddTitle := WithAddTitle;
    Fixed := IsFixed;
    BlankIfZero := IsBlankIfZero;
    SelectedRecord := (i > 0);//(i = 1);
    case TableType of
      teDBase: ExportToDBF;

      teText,
      teClipboard: ExportToText;

      teExcel: ExportToExcel;
      teWord: ExportToWord;
      teHTML,
      teXML,
      teSQL,
      teXLS,
      teSYLK,
      teDIF,
      teWKS,
      teQuattro,
      teSPSS,
      teLDIF,
      tePDF,
      teRTF: ExportToCellFile;

      teAccess: ExportToAccess;
      teADO: ExportToADO;
    else
      ExportDataSet;
    end;
  end
end;

end.

