{ SMExport suite
  TSMECustomDataEngine: basic data engine for navigation thru exported control

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMEEngine;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TSMEColumnBand = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FCaption: string;
    FVisible: Boolean;
    FColor: TColor;
    FFont: TFont;

    procedure SetFont(Value: TFont);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default taCenter;
    property Caption: string read FCaption write FCaption;
    property Visible: Boolean read FVisible write FVisible default True;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor default clSilver;
  end;

  TSMEColumnBands = class(TCollection)
  private
    FSMEControl: TComponent;

    function GetColumnBand(Index: Integer): TSMEColumnBand;
    procedure SetColumnband(Index: Integer; Value: TSMEColumnBand);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(SMECntrl: TComponent);
    function Add: TSMEColumnBand;

    property Items[Index: Integer]: TSMEColumnBand read GetColumnBand write SetColumnBand; default;
    property SMEControl: TComponent read FSMEControl;
  end;


  TSMEColumnTitle = class(TPersistent)
  private
    { Private declarations }
    FCaption: string;
    FColor: TColor;
    FAlignment: TAlignment;
    FFont: TFont;

    procedure SetFont(Value: TFont);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Caption: string read FCaption write FCaption;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor default clSilver;
  end;

  TSMEColumnKind = (ckField, ckConstant, ckSysVar);
  TCellType = (ctBlank, ctInteger, ctDouble, ctString, ctDateTime, ctDate, ctCurrency, ctTime, ctMEMO, ctGraphic);
  TSMEColumn = class(TCollectionItem)
  private
    { Private declarations }
    FFieldName: string;
    FColor: TColor;
    FWidth: Integer;
    FPrecision: Integer;
    FFont: TFont;
    FAlignment: TAlignment;
    FVisible: Boolean;
    FTitle: TSMEColumnTitle;
    FDataType: TCellType;
    FColumnKind: TSMEColumnKind;
    FBandIndex: Integer;

    procedure SetFont(Value: TFont);
    procedure SetTitle(Value: TSMEColumnTitle);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property Color: TColor read FColor write FColor default clWhite;
    property FieldName: string read FFieldName write FFieldName;
    property Font: TFont read FFont write SetFont;
    property Width: Integer read FWidth write FWidth default 0;
    property Precision: Integer read FPrecision write FPrecision default 0;
    property Visible: Boolean read FVisible write FVisible;
    property Title: TSMEColumnTitle read FTitle write SetTitle;
    property DataType: TCellType read FDataType write FDataType default ctString;
    property ColumnKind: TSMEColumnKind read FColumnKind write FColumnKind default ckField;
    property BandIndex: Integer read FBandIndex write FBandIndex default -1;
  end;

  TSMEColumns = class(TCollection)
  private
    FSMEControl: TComponent;

    function GetColumn(Index: Integer): TSMEColumn;
    procedure SetColumn(Index: Integer; Value: TSMEColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(SMECntrl: TComponent);
    function Add: TSMEColumn;
    function ColumnByFieldName(const AFieldName: string): Integer;
    function MergedColStart(BandIndex: Integer): Integer;
    function MergedColCount(BandIndex, StartColIndex: Integer): Integer;

    property Items[Index: Integer]: TSMEColumn read GetColumn write SetColumn; default;
    property SMEControl: TComponent read FSMEControl;
  end;

  TSMEMeasure = (emPoint, emInch, emCentimeters, emPicas);
  TSMEOrientation = (emDefault, emPortrait, emLandscape);

  TSMEPageSetup = class(TPersistent)
  private
    { Private declarations }
    FMeasure: TSMEMeasure;

    FUseDefault: Boolean;
    FTopMargin: Single;
    FBottomMargin: Single;
    FLeftMargin: Single;
    FRightMargin: Single;

    FTableWidth: Single;
    FOrientation: TSMEOrientation;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Measure: TSMEMeasure read FMeasure write FMeasure default emPoint;

    property UseDefault: Boolean read FUseDefault write FUseDefault default True;
    property TopMargin: Single read FTopMargin write FTopMargin;
    property BottomMargin: Single read FBottomMargin write FBottomMargin;
    property LeftMargin: Single read FLeftMargin write FLeftMargin;
    property RightMargin: Single read FRightMargin write FRightMargin;

    property TableWidth: Single read FTableWidth write FTableWidth;
    property Orientation: TSMEOrientation read FOrientation write FOrientation default emDefault;
  end;

  TSMEGetNext = procedure(Sender: TObject; var Abort: Boolean) of object;
  TSMEGetCount = procedure(Sender: TObject; var Count: LongInt) of object;
  TSMEGetValue = procedure(Sender: TObject; Column: TSMEColumn; var Value: Variant) of object;

  TSMECustomDataEngine = class(TComponent)
  private
    { Private declarations }
    FSelectedRecords: Boolean;

    FOnFirst: TNotifyEvent;
    FOnNext: TSMEGetNext;
    FOnCount: TSMEGetCount;

    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;

    FOnFillColumns: TNotifyEvent;
    FOnGetValue: TSMEGetValue;
  protected
    { Protected declarations }
    IsCanceled: Boolean;
  public
    { Public declarations }
    function SelectedRecordIsSupported: Boolean; virtual;

    procedure EnableControls; virtual;
    procedure DisableControls; virtual;

    procedure SavePosition; virtual;
    procedure RestorePosition; virtual;

    procedure FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean); virtual;

    function IsDataRow(Index: Integer): Boolean; virtual;
    function Eof: Boolean; virtual;
    procedure First; virtual;
    procedure Next; virtual;
    function GetFieldValue(Column: TSMEColumn): Variant; virtual;
    procedure ApplyCellColors(ARow, ACol: Integer; var al: TAlignment; var fnt: TFont; var AColor: TColor); virtual;
    function RecordCount: Integer; virtual;

    function DataTypeByColumn(Column: TSMEColumn): TFieldType; virtual;
    function RequiredByColumn(Column: TSMEColumn): Boolean; virtual;
    function ReadOnlyByColumn(Column: TSMEColumn): Boolean; virtual;

    function FindFieldByColumn(Column: TSMEColumn): TField; virtual;

    property SelectedRecords: Boolean read FSelectedRecords write FSelectedRecords;
  published
    { Published declarations }
    property OnFirst: TNotifyEvent read FOnFirst write FOnFirst;
    property OnNext: TSMEGetNext read FOnNext write FOnNext;
    property OnCount: TSMEGetCount read FOnCount write FOnCount;

    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;

    property OnFillColumns: TNotifyEvent read FOnFillColumns write FOnFillColumns;
    property OnGetValue: TSMEGetValue read FOnGetValue write FOnGetValue;
  end;

  TSMEVirtualDataEngine = class(TSMECustomDataEngine)
  private
    { Private declarations }
    intCurRow: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure First; override;
    procedure Next; override;
    function Eof: Boolean; override;
    function GetFieldValue(Column: TSMEColumn): Variant; override;
  published
    { Published declarations }
  end;

function GetValueType(DataType: TFieldType; Value: Variant; BlankIfZero: Boolean): TCellType;
function CellType2FieldType(ct: TCellType): TFieldType;

implementation

function GetValueType(DataType: TFieldType; Value: Variant; BlankIfZero: Boolean): TCellType;
begin
  Result := ctString;
  case DataType of
    ftSmallint,
    ftInteger,
    ftWord,
    {$IFDEF SMForDelphi4}
    ftLargeint,
    {$ENDIF}
    ftAutoInc: if BlankIfZero then
               begin
                 if VarIsNull(Value) or VarIsEmpty(Value) or (Value = '0') then
                   Result := ctBlank
               end
               else
                 Result := ctInteger;

    ftFloat,
    {$IFDEF SMForDelphi6}
    ftFMTBcd,
    {$ENDIF}
//    ftCurrency,
    ftBCD: if BlankIfZero then
           begin
             if VarIsNull(Value) or VarIsEmpty(Value) or (Value = '0') then
               Result := ctBlank
           end
           else
             Result := ctDouble;

    ftCurrency: if BlankIfZero then
                begin
                  if VarIsNull(Value) or VarIsEmpty(Value) or (Value = '0') then
                    Result := ctBlank
                end
                else
                  Result := ctCurrency;

    ftDate,
    ftTime,
    ftDateTime: if BlankIfZero then
                begin
                  if VarIsNull(Value) or VarIsEmpty(Value) or (Value = '0') then
                    Result := ctBlank
                end
                else
                  case DataType of
                    ftDate: Result := ctDate;
                    ftTime: Result := ctTime;
                  else
                    Result := ctDateTime;
                  end;

    ftMemo,
    ftBLOB,
    ftFmtMemo,
    ftParadoxOle,
    ftDBaseOle,
    ftTypedBinary: Result := ctMEMO;

    ftGraphic: Result := ctGraphic;
  else
    Result := ctString
  end;
end;

function CellType2FieldType(ct: TCellType): TFieldType;
begin
  case ct of
    ctInteger: Result := ftInteger;
    ctDouble: Result := ftFloat;
    ctCurrency: Result := ftCurrency;
    ctDateTime: Result := ftDateTime;
    ctDate: Result := ftDate;
    ctTime: Result := ftTime;
    ctMEMO: Result := ftMemo;
    ctGraphic: Result := ftGraphic;
  else
    Result := ftString
  end;
end;

{ TSMEColumnBand }
constructor TSMEColumnBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FAlignment := taLeftJustify;
  FCaption := '';
  FVisible := True;
  FFont := TFont.Create;
  FColor := clSilver;
end;

destructor TSMEColumnBand.Destroy;
begin
  FFont.Free;

  inherited Destroy;
end;

procedure TSMEColumnBand.SetFont(Value: TFont);
begin
  FFont.Assign(Value)
end;

procedure TSMEColumnBand.Assign(Source: TPersistent);
var
  smb: TSMEColumnBand;
begin
  if Source is TSMEColumnBand then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      smb := TSMEColumnBand(Source);
      Alignment := smb.Alignment;
      Caption := smb.Caption;
      Visible := smb.Visible;
      Font.Assign(smb.Font);
      Color := smb.Color;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

{ TSMEColumnBands }
constructor TSMEColumnBands.Create(SMECntrl: TComponent);
begin
  inherited Create(TSMEColumnBand);
  FSMEControl := SMECntrl;
end;

function TSMEColumnBands.Add: TSMEColumnBand;
begin
  Result := TSMEColumnBand(inherited Add);
end;

function TSMEColumnBands.GetOwner: TPersistent;
begin
  Result := FSMEControl;
end;

function TSMEColumnBands.GetColumnBand(Index: Integer): TSMEColumnBand;
begin
  Result := TSMEColumnBand(inherited Items[Index]);
end;

procedure TSMEColumnBands.SetColumnBand(Index: Integer; Value: TSMEColumnBand);
begin
  Items[Index].Assign(Value);
end;

{ TSMEColumnTitle }
constructor TSMEColumnTitle.Create;
begin
  inherited;

  FFont := TFont.Create;
  FColor := clSilver;
  FAlignment := taLeftJustify;
end;

destructor TSMEColumnTitle.Destroy;
begin
  FFont.Free;

  inherited
end;

procedure TSMEColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value)
end;

procedure TSMEColumnTitle.Assign(Source: TPersistent);
var
  smeTitle: TSMEColumnTitle;
begin
  if Source is TSMEColumnTitle then
  begin
    smeTitle := TSMEColumnTitle(Source);
    Caption := smeTitle.Caption;
    Alignment := smeTitle.Alignment;
    Font.Assign(smeTitle.Font);
    Color := smeTitle.Color;
  end
  else
    inherited Assign(Source);
end;

{ TSMEColumn }
constructor TSMEColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FFont := TFont.Create;
  FTitle := TSMEColumnTitle.Create;
  FFieldName := '';
  FVisible := True;
  FColor := clWhite;
  FAlignment := taLeftJustify;
  FDataType := ctString;
  FPrecision := 0;
  FWidth := 0;
  FColumnKind := ckField;
  FBandIndex := -1;
end;

destructor TSMEColumn.Destroy;
begin
  FFont.Free;
  FTitle.Free;

  inherited Destroy;
end;

procedure TSMEColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value)
end;

procedure TSMEColumn.SetTitle(Value: TSMEColumnTitle);
begin
  FTitle.Assign(Value)
end;

procedure TSMEColumn.Assign(Source: TPersistent);
var
  smeCol: TSMEColumn;
begin
  if Source is TSMEColumn then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      smeCol := TSMEColumn(Source);
      FieldName := smeCol.FieldName;
      Font.Assign(smeCol.Font);
      Color := smeCol.Color;
      Width := smeCol.Width;
      Precision := smeCol.Precision;
      Alignment := smeCol.Alignment;
      Visible := smeCol.Visible;
      DataType := smeCol.DataType;
      ColumnKind := smeCol.ColumnKind;
      BandIndex := smeCol.BandIndex;
      Title.Caption := smeCol.Title.Caption;
      Title.Alignment := smeCol.Title.Alignment;
      Title.Font.Assign(smeCol.Title.Font);
      Title.Color := smeCol.Title.Color;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;


{ TSMEColumns }
constructor TSMEColumns.Create(SMECntrl: TComponent);
begin
  inherited Create(TSMEColumn);
  FSMEControl := SMECntrl;
end;

function TSMEColumns.Add: TSMEColumn;
begin
  Result := TSMEColumn(inherited Add);
end;

function TSMEColumns.GetOwner: TPersistent;
begin
  Result := FSMEControl;
end;

function TSMEColumns.GetColumn(Index: Integer): TSMEColumn;
begin
  Result := TSMEColumn(inherited Items[Index]);
end;

procedure TSMEColumns.SetColumn(Index: Integer; Value: TSMEColumn);
begin
  Items[Index].Assign(Value);
end;

function TSMEColumns.ColumnByFieldName(const AFieldName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].FieldName = AFieldName then
    begin
      Result := i;
      break
    end
end;

function TSMEColumns.MergedColStart(BandIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Visible and (Items[i].BandIndex = BandIndex) then
    begin
      Result := i;
      break
    end
end;

function TSMEColumns.MergedColCount(BandIndex, StartColIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := StartColIndex to Count-1 do
    if Items[i].Visible and (Items[i].BandIndex = BandIndex) then
      Inc(Result)
    else
      break;
end;

{ TSMEPageSetup }
procedure TSMEPageSetup.Assign(Source: TPersistent);
var
  page: TSMEPageSetup;
begin
  if Source is TSMEPageSetup then
  begin
    page := TSMEPageSetup(Source);

    Measure := page.Measure;
    UseDefault := page.UseDefault;
    TopMargin := page.TopMargin;
    BottomMargin := page.BottomMargin;
    LeftMargin := page.LeftMargin;
    RightMargin := page.RightMargin;

    TableWidth := page.TableWidth;
    Orientation := page.Orientation;
  end
  else
    inherited Assign(Source)
end;

{ TSMEDataEngine }
function TSMECustomDataEngine.SelectedRecordIsSupported: Boolean;
begin
  Result := False;
end;

procedure TSMECustomDataEngine.EnableControls;
begin
end;

procedure TSMECustomDataEngine.DisableControls;
begin
end;

procedure TSMECustomDataEngine.SavePosition;
begin
  if Assigned(OnBeforeExecute) then
    OnBeforeExecute(Self);
end;

procedure TSMECustomDataEngine.RestorePosition;
begin
  if Assigned(OnAfterExecute) then
    OnAfterExecute(Self);
end;

procedure TSMECustomDataEngine.FillColumns(Columns: TSMEColumns; Bands: TSMEColumnBands; RightToLeft: Boolean);
begin
  Columns.Clear;
  Bands.Clear;

  if Assigned(OnFillColumns) then
    OnFillColumns(Columns);
end;

function TSMECustomDataEngine.IsDataRow(Index: Integer): Boolean;
begin
  Result := True
end;

function TSMECustomDataEngine.Eof: Boolean;
begin
  Result := True
end;

procedure TSMECustomDataEngine.First;
begin
  if Assigned(OnFirst) then
    OnFirst(Self);
end;

procedure TSMECustomDataEngine.Next;
begin
  if Assigned(OnNext) then
    OnNext(Self, IsCanceled);
end;

function TSMECustomDataEngine.GetFieldValue(Column: TSMEColumn): Variant;
begin
  Result := NULL;

{  if Assigned(OnGetValue) then
    OnGetValue(Self, Column, Result)
}
end;

procedure TSMECustomDataEngine.ApplyCellColors(ARow, ACol: Integer; var al: TAlignment; var fnt: TFont; var AColor: TColor);
begin
end;

function TSMECustomDataEngine.RecordCount: Integer;
begin
  Result := 0;

  if Assigned(OnCount) then
    OnCount(Self, Result);
end;

function TSMECustomDataEngine.DataTypeByColumn(Column: TSMEColumn): TFieldType;
begin
  Result := CellType2FieldType(Column.DataType)//ctString
end;

function TSMECustomDataEngine.RequiredByColumn(Column: TSMEColumn): Boolean;
begin
  Result := False
end;

function TSMECustomDataEngine.ReadOnlyByColumn(Column: TSMEColumn): Boolean;
begin
  Result := False
end;

function TSMECustomDataEngine.FindFieldByColumn(Column: TSMEColumn): TField;
begin
  Result := nil
end;

{ TSMEVirtualDataEngine }
procedure TSMEVirtualDataEngine.First;
begin
  inherited First;

  intCurRow := 0
end;

procedure TSMEVirtualDataEngine.Next;
begin
  inherited Next;

  Inc(intCurRow)
end;

function TSMEVirtualDataEngine.Eof: Boolean;
begin
  Result := (inherited Eof) and (intCurRow >= RecordCount)
end;

function TSMEVirtualDataEngine.GetFieldValue(Column: TSMEColumn): Variant;
begin
  inherited GetFieldValue(Column);

  if Assigned(OnGetValue) then
    OnGetValue(Self, Column, Result)
end;

end.

