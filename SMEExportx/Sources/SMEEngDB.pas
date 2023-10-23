{ SMExport suite
  TSMECustomDBDataEngine: basic engine with navigation thru dataset
  TSMEDatasetDataEngine: published component for TDataset exporting
  TSMEDBGridDataEngine: published component for TDBGrid exporting

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMEEngDB;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB, DBGrids, SMEEngine
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TSMECustomDBDataEngine = class(TSMECustomDataEngine)
  private
    { Private declarations }
    bkmark: TBookmark;
  protected
    { Protected declarations }
    function GetDataset: TDataset; virtual;
  public
    { Public declarations }
    procedure EnableControls; override;
    procedure DisableControls; override;

    procedure SavePosition; override;
    procedure RestorePosition; override;

    procedure FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean); override;

    function Eof: Boolean; override;
    procedure First; override;
    procedure Next; override;
    function GetFieldValue(Column: TSMEColumn): Variant; override;
    function RecordCount: Integer; override;

    function DataTypeByColumn(Column: TSMEColumn): TFieldType; override;
    function RequiredByColumn(Column: TSMEColumn): Boolean; override;
    function ReadOnlyByColumn(Column: TSMEColumn): Boolean; override;
    function FindFieldByColumn(Column: TSMEColumn): TField;  override;

    property Dataset: TDataset read GetDataset;
  published
    { Published declarations }
  end;

  TSMEDatasetDataEngine = class(TSMECustomDBDataEngine)
  private
    { Private declarations }
    FDataSet: TDataSet;

    procedure SetDataset(Value: TDataset);
  protected
    { Protected declarations }
    function GetDataset: TDataset; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
  published
    { Published declarations }
    property Dataset: TDataset read GetDataset write SetDataset;
  end;

  TSMEDBGridDataEngine = class(TSMECustomDBDataEngine)
  private
    { Private declarations }
    intCurrentSelectedRow: Integer;

    FDBGrid: TCustomDBGrid;

    function GetDBGrid: TCustomDBGrid;
    procedure SetDBGrid(AValue: TCustomDBGrid);
  protected
    { Protected declarations }
    function GetDataset: TDataset; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    function SelectedRecordIsSupported: Boolean; override;

    procedure FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean); override;

    function Eof: Boolean; override;
    procedure First; override;
    procedure Next; override;
    function RecordCount: Integer; override;
  published
    { Published declarations }
    property DBGrid: TCustomDBGrid read GetDBGrid write SetDBGrid;
  end;

implementation

{ TSMECustomDBDataEngine }
function TSMECustomDBDataEngine.GetDataset: TDataset;
begin
  Result := nil
end;

procedure TSMECustomDBDataEngine.EnableControls;
begin
  inherited EnableControls;

  Dataset.EnableControls;
end;

procedure TSMECustomDBDataEngine.DisableControls;
begin
  inherited DisableControls;

  Dataset.DisableControls;
end;

procedure TSMECustomDBDataEngine.SavePosition;
begin
  inherited SavePosition;

  bkmark := Dataset.GetBookmark;
end;

procedure TSMECustomDBDataEngine.RestorePosition;
begin
  inherited RestorePosition;

  Dataset.GotoBookmark(bkmark);
  Dataset.FreeBookmark(bkmark);
end;

procedure TSMECustomDBDataEngine.FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean);
var
  i, intStartIndex, intCount: Integer;
begin
  inherited FillColumns(Columns, Bands, RightToLeft);

  if not Assigned(Dataset) then
    exit;

  intCount := Dataset.FieldCount-1;
  if RightToLeft then
    intStartIndex := intCount
  else
    intStartIndex := 0;

  i := intStartIndex;
  repeat
    with Columns.Add do
    begin
      Alignment := Dataset.Fields[i].Alignment;
      FieldName := Dataset.Fields[i].FieldName;
      Color := clWhite;
      Width := Dataset.Fields[i].DisplayWidth;
      Visible := Dataset.Fields[i].Visible;
      if (Dataset.Fields[i] is TFloatField) then
      begin
        Precision := TFloatField(Dataset.Fields[i]).Precision;
        if (Precision = 15) then
          Precision := 0;
      end;

      DataType := GetValueType(Dataset.Fields[i].DataType, Dataset.Fields[i].Value, False);

      Title.Caption := Dataset.Fields[i].DisplayLabel;
      Title.Alignment := Dataset.Fields[i].Alignment;
      Title.Font.Style := Title.Font.Style + [fsBold];
      Title.Color := clSilver;
    end;

    if RightToLeft then
      Dec(i)
    else
      Inc(i)
  until (i < 0) or (i >= intCount+1);
end;

function TSMECustomDBDataEngine.Eof: Boolean;
begin
  Result := (inherited Eof) and Dataset.Eof
end;

procedure TSMECustomDBDataEngine.First;
begin
  inherited First;

  Dataset.First;
end;

procedure TSMECustomDBDataEngine.Next;
begin
  inherited Next;

  Dataset.Next;
end;

function TSMECustomDBDataEngine.GetFieldValue(Column: TSMEColumn): Variant;
var
  fld: TField;
begin
  fld := FindFieldByColumn(Column);
  if Assigned(fld) then
    Result := fld.Value
  else
    Result := NULL
end;

function TSMECustomDBDataEngine.RecordCount: Integer;
begin
  Result := Dataset.RecordCount;
end;

function TSMECustomDBDataEngine.DataTypeByColumn(Column: TSMEColumn): TFieldType;
//var
//  fld: TField;
begin
//  fld := FindFieldByColumn(Column);
//  if Assigned(fld) then
//    Result := fld.DataType
//  else
    Result := inherited DataTypeByColumn(Column)
//    Result := ftString
end;

function TSMECustomDBDataEngine.RequiredByColumn(Column: TSMEColumn): Boolean;
var
  fld: TField;
begin
  fld := FindFieldByColumn(Column);
  if Assigned(fld) then
    Result := fld.Required
  else
    Result := False
end;

function TSMECustomDBDataEngine.ReadOnlyByColumn(Column: TSMEColumn): Boolean;
var
  fld: TField;
begin
  fld := FindFieldByColumn(Column);
  if Assigned(fld) then
    Result := fld.ReadOnly
  else
    Result := False
end;

function TSMECustomDBDataEngine.FindFieldByColumn(Column: TSMEColumn): TField;
begin
  if Assigned(Column) and Assigned(Dataset) then
    Result := Dataset.FindField(Column.FieldName)
  else
    Result := nil
end;


{ TSMEDatasetDataEngine }
function TSMEDatasetDataEngine.GetDataset: TDataset;
begin
  Result := FDataSet
end;

procedure TSMEDatasetDataEngine.SetDataset(Value: TDataset);
begin
  FDataSet := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TSMEDatasetDataEngine.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FDataSet) then
      FDataSet := nil;
  end;
end;


{ TSMEDBGridDataEngine }
function TSMEDBGridDataEngine.SelectedRecordIsSupported: Boolean;
begin
  Result := True;
end;

type
  TSMEHackGrid = class(TCustomDBGrid);

function TSMEDBGridDataEngine.GetDataset: TDataset;
begin
  if Assigned(DBGrid) and
     Assigned(TSMEHackGrid(DBGrid).DataSource) and
     Assigned(TSMEHackGrid(DBGrid).DataSource.Dataset) then
    Result := TSMEHackGrid(DBGrid).DataSource.Dataset
  else
    Result := inherited GetDataset
end;

function TSMEDBGridDataEngine.GetDBGrid: TCustomDBGrid;
begin
  Result := FDBGrid;
end;

procedure TSMEDBGridDataEngine.SetDBGrid(AValue: TCustomDBGrid);
begin
  FDBGrid := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TSMEDBGridDataEngine.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FDBGrid) then
      FDBGrid := nil
  end;
end;

function TSMEDBGridDataEngine.Eof: Boolean;
begin
  Result := inherited Eof;

  if SelectedRecords then
    Result := {Result and} (intCurrentSelectedRow > TSMEHackGrid(DBGrid).SelectedRows.Count-1)
end;

procedure TSMEDBGridDataEngine.First;
begin
  if SelectedRecords then
  begin
    intCurrentSelectedRow := 0;
    if (intCurrentSelectedRow < TSMEHackGrid(DBGrid).SelectedRows.Count) then
      Dataset.Bookmark := TSMEHackGrid(DBGrid).SelectedRows[intCurrentSelectedRow];
  end
  else
    inherited First
end;

procedure TSMEDBGridDataEngine.Next;
begin
  if SelectedRecords then
  begin
    Inc(intCurrentSelectedRow);
    if not Eof then
      Dataset.Bookmark := TSMEHackGrid(DBGrid).SelectedRows[intCurrentSelectedRow];
  end
  else
    inherited Next
end;

function TSMEDBGridDataEngine.RecordCount: Integer;
begin
  if SelectedRecords then
    Result := TSMEHackGrid(DBGrid).SelectedRows.Count
  else
    Result := inherited RecordCount
end;

procedure TSMEDBGridDataEngine.FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean);
var
  i, intStartIndex, intCount: Integer;
  fld: TField;
begin
  Columns.Clear;
  Bands.Clear;
//  inherited FillColumns(Columns, RightToLeft);

  if not Assigned(DBGrid) then
    exit;

  intCount := TSMEHackGrid(DBGrid).Columns.Count-1;
  if RightToLeft then
    intStartIndex := intCount
  else
    intStartIndex := 0;

  i := intStartIndex;
  repeat
    with Columns.Add do
    begin
      FieldName := TSMEHackGrid(DBGrid).Columns[i].FieldName;
      Alignment := TSMEHackGrid(DBGrid).Columns[i].Alignment;

      fld := TSMEHackGrid(DBGrid).Columns[i].Field;// Dataset.FindField(FieldName);
      if Assigned(fld) then
        Width := fld.DisplayWidth
      else
        Width := 10;

      if (fld is TFloatField) then
      begin
        Precision := TFloatField(fld).Precision;
        if (Precision = 15) then
          Precision := 0;
      end;

      DataType := GetValueType(fld.DataType, fld.Value, False);

      Color := TSMEHackGrid(DBGrid).Columns[i].Color;
      Font.Assign(TSMEHackGrid(DBGrid).Columns[i].Font);

      {$IFDEF SMForDelphi4}
      Visible := TSMEHackGrid(DBGrid).Columns[i].Visible;
      {$ELSE}
      if Assigned(fld) then
        Visible := fld.Visible
      else
        Visible := True;
      {$ENDIF}

      Title.Caption := TSMEHackGrid(DBGrid).Columns[i].Title.Caption;
      Title.Alignment := TSMEHackGrid(DBGrid).Columns[i].Title.Alignment;
      Title.Font.Assign(TSMEHackGrid(DBGrid).Columns[i].Title.Font);
      Title.Color := TSMEHackGrid(DBGrid).Columns[i].Title.Color;
    end;

    if RightToLeft then
      Dec(i)
    else
      Inc(i)
  until (i < 0) or (i >= intCount+1);
end;

end.

