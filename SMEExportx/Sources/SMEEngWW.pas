{ SMExport suite
  TSMEwwDBGridDataEngine component: data engine for navigation thru TwwDBGrid from InfoPower
  TSMEDBAdvStringGridDataEngine component: data engine for navigation thru TDBAdvStringGridDataEngine from TMS Software

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMEEngWW;

interface

{$I sme.inc}

uses
  Classes, Controls, Graphics, DB, SMEEngine, SMEEngDB
  {$IFDEF INFOPOWER_SUPPORT} , Wwdbigrd, Wwdbgrid {$ENDIF}
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TSMEwwDBGridDataEngine = class(TSMECustomDBDataEngine)
  private
    { Private declarations }
    intCurrentRow: Integer;

    FDBGrid: TCustomControl;
    FExportFooters: Boolean;

    function GetDBGrid: TCustomControl;
    procedure SetDBGrid(AValue: TCustomControl);
  protected
    { Protected declarations }
    function GetDataset: TDataset; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    function SelectedRecordIsSupported: Boolean; override;

    procedure FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean); override;

    function IsDataRow(Index: Integer): Boolean; override;
    function Eof: Boolean; override;
    procedure First; override;
    procedure Next; override;
    function RecordCount: Integer; override;
    function GetFieldValue(Column: TSMEColumn): Variant; override;
  published
    { Published declarations }
    property DBGrid: TCustomControl read GetDBGrid write SetDBGrid;
    property ExportFooters: Boolean read FExportFooters write FExportFooters default False;
  end;

  TSMEDBAdvStringGridDataEngine = class(TSMECustomDBDataEngine)
  private
    { Private declarations }
    FDBGrid: TCustomControl;

    function GetDBGrid: TCustomControl;
    procedure SetDBGrid(AValue: TCustomControl);
  protected
    { Protected declarations }
    function GetDataset: TDataset; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    procedure FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean); override;
  published
    { Published declarations }
    property DBGrid: TCustomControl read GetDBGrid write SetDBGrid;
  end;

function GetPropAsDataSet(comp: TComponent): TDataSet;
function PropIsDBGrid(comp: TComponent; var IsWWDBGrid, IsTMSDBGrid: Boolean): Boolean;
function GetPropAsSelected(comp: TComponent): TStrings;
function GetPropAsColumns(comp: TComponent): TCollection;

procedure FillGridList(comp: TComponent; lst: TStrings);

implementation
uses SysUtils, TypInfo, ExCnst
  {$IFDEF SMForDelphi5} , Forms {$ENDIF};

function GetPropAsDataSet(comp: TComponent): TDataSet;
var
  PropInfoDataSource: PPropInfo;
  DataSource: TDataSource;
begin
  Result := nil;
  if (comp is TCustomControl) then
  begin
    PropInfoDataSource := GetPropInfo(comp.ClassInfo, 'DataSource');
    {if such property exists}
    if Assigned(PropInfoDataSource) and
       (PropInfoDataSource.PropType^.Kind = tkClass) then
    begin
      DataSource := TDataSource(GetOrdProp(comp, PropInfoDataSource));
      if Assigned(DataSource) then
        if Assigned(DataSource.DataSet) then
          Result := DataSource.DataSet;
    end
  end
end;

function PropIsDBGrid(comp: TComponent; var IsWWDBGrid, IsTMSDBGrid: Boolean): Boolean;
var
  PropInfoDataSource, PropInfoColumns1, PropInfoColumns2, PropInfoColumns3: PPropInfo;
  columns: TCollection;
  i: LongInt;
begin
  Result := False;
  if (comp is TCustomControl {TCustomGrid}) then
  begin
    PropInfoDataSource := GetPropInfo(comp.ClassInfo, 'DataSource');
    PropInfoColumns1 := GetPropInfo(comp.ClassInfo, 'Columns');
    PropInfoColumns2 := GetPropInfo(comp.ClassInfo, 'Selected');
    PropInfoColumns3 := GetPropInfo(comp.ClassInfo, 'Fields');

    {if such properties exists}
    if Assigned(PropInfoDataSource) and
       (PropInfoDataSource.PropType^.Kind = tkClass) then
      if Assigned(PropInfoColumns1) and
         (PropInfoColumns1.PropType^.Kind = tkClass) then
      begin
        {check: the Columns property have the FieldName property?}
        try
          i := GetOrdProp(comp, PropInfoColumns1);
          if TObject(i) is TCollection then
          begin
            columns := TCollection(i);
            Result := Assigned(GetPropInfo(columns.ItemClass.ClassInfo, 'FieldName'));
            IsWWDBGrid := False;
            IsTMSDBGrid := False;
          end
        except
        end
      end
      else
        if Assigned(PropInfoColumns2) and
           (PropInfoColumns2.PropType^.Kind = tkClass) then
        begin
          Result := True;
          IsWWDBGrid := True;
          IsTMSDBGrid := False;
        end
        else
          if Assigned(PropInfoColumns3) and
             (PropInfoColumns3.PropType^.Kind = tkClass) then
          begin
            Result := True;
            IsTMSDBGrid := True;
          end
  end;
end;

function GetPropAsSelected(comp: TComponent): TStrings;
var
  PropInfoDataSource, PropInfoColumns: PPropInfo;
begin
  Result := nil;
  if (comp is TCustomControl) then
  begin
    PropInfoDataSource := GetPropInfo(comp.ClassInfo, 'DataSource');
    PropInfoColumns := GetPropInfo(comp.ClassInfo, 'Selected');

    {if such properties exists}
    if Assigned(PropInfoDataSource) and
       (PropInfoDataSource.PropType^.Kind = tkClass) and
       Assigned(PropInfoColumns) and
       (PropInfoColumns.PropType^.Kind = tkClass) then
      Result := TStrings(GetOrdProp(comp, PropInfoColumns));
  end
end;

function GetPropAsColumns(comp: TComponent): TCollection;
var
  PropInfoDataSource, PropInfoColumns: PPropInfo;
begin
  Result := nil;
  if (comp is TCustomControl) then
  begin
    PropInfoDataSource := GetPropInfo(comp.ClassInfo, 'DataSource');
    PropInfoColumns := GetPropInfo(comp.ClassInfo, 'Columns');
    if not Assigned(PropInfoColumns) then //TMS grid
      PropInfoColumns := GetPropInfo(comp.ClassInfo, 'Fields');

    {if such properties exists}
    if Assigned(PropInfoDataSource) and
       (PropInfoDataSource.PropType^.Kind = tkClass) and
       Assigned(PropInfoColumns) and
       (PropInfoColumns.PropType^.Kind = tkClass) then
      Result := TCollection(GetOrdProp(comp, PropInfoColumns));
  end
end;

procedure FillGridList(comp: TComponent; lst: TStrings);
var
  i: Integer;
  IsWWDBGrid, IsTMSDBGrid: Boolean;
  s: string;
  {$IFDEF SMForDelphi5}
//  prnt: TComponent;
  {$ENDIF}
begin
  for i := 0 to comp.ComponentCount-1 do
  begin
    if PropIsDBGrid(comp.Components[i], IsWWDBGrid, IsTMSDBGrid) then
    begin
      s := comp.Components[i].Name;
      {$IFDEF SMForDelphi5}
      if (comp is TFrame) then
        s := comp.Name + '.' + s;
{      prnt := comp;
      while Assigned(prnt) do
      begin
        if (prnt is TFrame) then
          s := prnt.Name + '.' + s;
        if (prnt is TWinControl) then
          prnt := TWinControl(prnt).Parent
      end;
}      {$ENDIF}
      lst.Add(s);
    end;
    if (comp.Components[i] is TWinControl) then
      FillGridList(comp.Components[i], lst)
  end;
end;


{ TSMEwwDBGridDataEngine }
function TSMEwwDBGridDataEngine.SelectedRecordIsSupported: Boolean;
begin
  Result := True;
end;

function TSMEwwDBGridDataEngine.GetDataset: TDataset;
begin
  Result := GetPropAsDataSet(DBGrid)
end;

function TSMEwwDBGridDataEngine.GetDBGrid: TCustomControl;
begin
  Result := FDBGrid;
end;

procedure TSMEwwDBGridDataEngine.SetDBGrid(AValue: TCustomControl);
var
  IsOK, IsWWDBGrid, IsTMSDBGrid: Boolean;
begin
  IsOK := (AValue = nil);
  if not IsOK and
     PropIsDBGrid(AValue, IsWWDBGrid, IsTMSDBGrid) then
    IsOK := IsWWDBGrid;
  if IsOK then
  begin
    FDBGrid := AValue;
    if AValue <> nil then
      AValue.FreeNotification(Self);
  end
  else
    raise Exception.Create(Format(strPropertyError, [AValue.Name]));
end;

procedure TSMEwwDBGridDataEngine.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FDBGrid) then
      FDBGrid := nil
  end;
end;

function TSMEwwDBGridDataEngine.IsDataRow(Index: Integer): Boolean;
begin
  if ExportFooters and (Index >= RecordCount-1) then
    Result := False
  else
    Result := inherited IsDataRow(Index)
end;

function TSMEwwDBGridDataEngine.Eof: Boolean;
begin
//  Result := (intCurrentRow >= RecordCount);
//  exit;

  Result := inherited Eof;

  {$IFDEF INFOPOWER_SUPPORT}
  if SelectedRecords then
    Result := (intCurrentRow > TwwDBGrid(DBGrid).SelectedList.Count-1)
  {$ENDIF}
end;

procedure TSMEwwDBGridDataEngine.First;
begin
  intCurrentRow := 0;
  {$IFDEF INFOPOWER_SUPPORT}
  if SelectedRecords then
    Dataset.GotoBookmark(TwwDBGrid(DBGrid).SelectedList.Items[intCurrentRow])
  else
  {$ENDIF}
    inherited First;
end;

procedure TSMEwwDBGridDataEngine.Next;
begin
  Inc(intCurrentRow);
  {$IFDEF INFOPOWER_SUPPORT}
  if SelectedRecords then
  begin
    if not Eof then
      Dataset.GotoBookmark(TwwDBGrid(DBGrid).SelectedList.Items[intCurrentRow]);
  end
  else
  {$ENDIF}
    inherited Next
end;

function TSMEwwDBGridDataEngine.RecordCount: Integer;
begin
  {$IFDEF INFOPOWER_SUPPORT}
  if SelectedRecords then
    Result := TwwDBGrid(DBGrid).SelectedList.Count
  else
  {$ENDIF}
    Result := inherited RecordCount;

  if ExportFooters then
  begin
    {$IFDEF INFOPOWER_SUPPORT}
    if (dgShowFooter in TwwDBGrid(DBGrid).Options) then
      Inc(Result)
  {$ENDIF}
  end;
end;

function TSMEwwDBGridDataEngine.GetFieldValue(Column: TSMEColumn): Variant; 
var
  IsProcessed: Boolean;
begin
  IsProcessed := False;
  if ExportFooters then
  begin
    {$IFDEF INFOPOWER_SUPPORT}
    if dgShowFooter in TwwDBGrid(DBGrid).Options then
    begin
      if {Dataset.Eof and} not IsDataRow(intCurrentRow) then
      begin
        Result := TwwDBGrid(DBGrid).ColumnByName(Column.FieldName).FooterValue;
        IsProcessed := True;
      end
    end
  {$ENDIF}
  end;
  if not IsProcessed then
    Result := inherited GetFieldValue(Column)
end;

procedure TSMEwwDBGridDataEngine.FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean);
var
  i, intStartIndex, intCount: Integer;
  lstSelected: TStrings;
  PropInfo: PPropInfo;
  fld: TField;
begin
  Columns.Clear;
  Bands.Clear;
//  inherited FillColumns(Columns, RightToLeft);

  lstSelected := GetPropAsSelected(DBGrid);
  if lstSelected.Count = 0 then
    for i := 0 to Dataset.FieldCount-1 do
      if Dataset.Fields[i].Visible then
        lstSelected.Add(Dataset.Fields[i].FieldName + #9 +
                        IntToStr(Dataset.Fields[i].DisplayWidth) + #9 +
                        Dataset.Fields[i].DisplayLabel);

  intCount := lstSelected.Count-1;
  if RightToLeft then
    intStartIndex := intCount
  else
    intStartIndex := 0;

  i := intStartIndex;
  repeat
    with Columns.Add do
    begin
      FieldName := Copy(lstSelected[i], 1, Pos(#9, lstSelected[i])-1);
      fld := Dataset.FindField(FieldName);
      Alignment := fld.Alignment;
      Width := fld.DisplayWidth;

      PropInfo := GetPropInfo(DBGrid.ClassInfo, 'Color');
      {if such property exists}
      if Assigned(PropInfo) then
        Color := TColor(GetOrdProp(DBGrid, PropInfo));

      PropInfo := GetPropInfo(DBGrid.ClassInfo, 'Font');
      {if such property exists}
      if Assigned(PropInfo) then
        Font.Assign(TFont(GetOrdProp(DBGrid, PropInfo)));

      Visible := fld.Visible;
      Title.Caption := fld.DisplayLabel;
      Title.Alignment := fld.Alignment;

      PropInfo := GetPropInfo(DBGrid.ClassInfo, 'TitleFont');
      {if such property exists}
      if Assigned(PropInfo) then
        Title.Font.Assign(TFont(GetOrdProp(DBGrid, PropInfo)));

      Title.Color := clSilver;
    end;

    if RightToLeft then
      Dec(i)
    else
      Inc(i);
  until (i < 0) or (i >= intCount+1);
end;


{ TSMEDBAdvStringGridDataEngine }
function TSMEDBAdvStringGridDataEngine.GetDataset: TDataset;
begin
  Result := GetPropAsDataSet(DBGrid)
end;

function TSMEDBAdvStringGridDataEngine.GetDBGrid: TCustomControl;
begin
  Result := FDBGrid;
end;

procedure TSMEDBAdvStringGridDataEngine.SetDBGrid(AValue: TCustomControl);
var
  IsOK, IsWWDBGrid, IsTMSDBGrid: Boolean;
begin
  IsOK := (AValue = nil);
  if not IsOK and
     PropIsDBGrid(AValue, IsWWDBGrid, IsTMSDBGrid) then
    IsOK := IsTMSDBGrid;
  if IsOK then
  begin
    FDBGrid := AValue;
    if AValue <> nil then
      AValue.FreeNotification(Self);
  end
  else
    raise Exception.Create(Format(strPropertyError, [AValue.Name]));
end;

procedure TSMEDBAdvStringGridDataEngine.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FDBGrid) then
      FDBGrid := nil
  end;
end;

procedure TSMEDBAdvStringGridDataEngine.FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean);
var
  i, intStartIndex, intCount: Integer;
  cols: TCollection;
  PropInfo: PPropInfo;
  fld: TField;
  ColTitle: TObject;
begin
  Columns.Clear;
  Bands.Clear;
//  inherited FillColumns(Columns, RightToLeft);

  cols := GetPropAsColumns(DBGrid);
  if cols.Count<1 then
  begin
    inherited;
    exit;
  end;

  intCount := cols.Count-1;
  if RightToLeft then
    intStartIndex := intCount
  else
    intStartIndex := 0;

  i := intStartIndex;
  repeat
    with Columns.Add do
    begin
      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'FieldName');
      {if such property exists}
      if Assigned(PropInfo) then
        FieldName := GetStrProp(cols.Items[i], PropInfo);

      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'Alignment');
      {if such property exists}
      if Assigned(PropInfo) then
        Alignment := TAlignment(GetOrdProp(cols.Items[i], PropInfo));

//      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'Width');
        {if such property exists}
//      if Assigned(PropInfo) then
//        Width := GetOrdProp(cols.Items[i], PropInfo);
      fld := Dataset.FindField(FieldName);
      if Assigned(fld) then
        Width := fld.DisplayWidth
      else
        Width := 10;

      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'Color');
      {if such property exists}
      if Assigned(PropInfo) then
        Color := TColor(GetOrdProp(cols.Items[i], PropInfo));

      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'Font');
      {if such property exists}
      if Assigned(PropInfo) then
        Font.Assign(TFont(GetOrdProp(cols.Items[i], PropInfo)));

      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'Visible');
      {if such property exists}
      if Assigned(PropInfo) then
        Visible := Boolean(GetOrdProp(cols.Items[i], PropInfo));

      PropInfo := GetPropInfo(cols.ItemClass.ClassInfo, 'Title');
      {if such properties exists}
      if Assigned(PropInfo) then
      begin
        if (PropInfo.PropType^.Kind = tkClass) then
        begin
          ColTitle := TObject(GetOrdProp(cols.Items[i], PropInfo));

          PropInfo := GetPropInfo(ColTitle.ClassInfo, 'Caption');
          {if such property exists}
          if Assigned(PropInfo) then
            Title.Caption := GetStrProp(ColTitle, PropInfo);

          PropInfo := GetPropInfo(ColTitle.ClassInfo, 'Alignment');
          {if such property exists}
          if Assigned(PropInfo) then
            Title.Alignment := TAlignment(GetOrdProp(ColTitle, PropInfo));

          PropInfo := GetPropInfo(ColTitle.ClassInfo, 'Font');
          {if such property exists}
          if Assigned(PropInfo) then
            Title.Font.Assign(TFont(GetOrdProp(ColTitle, PropInfo)));

          PropInfo := GetPropInfo(ColTitle.ClassInfo, 'Color');
          {if such property exists}
          if Assigned(PropInfo) then
            Title.Color := TColor(GetOrdProp(ColTitle, PropInfo));
        end
        else //TMS grid
          if (PropInfo.PropType^.Kind in [tkString, tkLString, tkWString]) then
            Title.Caption := GetStrProp(cols.Items[i], PropInfo);
      end
    end;

    if RightToLeft then
      Dec(i)
    else
      Inc(i)
  until (i < 0) or (i >= intCount+1);
end;

end.

