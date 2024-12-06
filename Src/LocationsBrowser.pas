unit LocationsBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BrowserUnit, Menus, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids,
  DB, ovcbase, ovcef, ovcpb, ovcnf, ovcsf, LocationUtils, FillLocationInfo,
  FilterLocations, FilterOptions;

type
  TfrmLocationsBrowser = class(TfrmDataSetBrowser)
    Utilities1: TMenuItem;
    DeleteSelectedRecords1: TMenuItem;
    N1: TMenuItem;
    OrderBy1: TMenuItem;
    ID1: TMenuItem;
    State1: TMenuItem;
    Latitude1: TMenuItem;
    Longitude1: TMenuItem;
    Description1: TMenuItem;
    DateAdded1: TMenuItem;
    DateUpdated1: TMenuItem;
    RefCount1: TMenuItem;
    FillFieldinSelectedRecords1: TMenuItem;
    lblDistanceAway: TLabel;
    N2: TMenuItem;
    CopyLocation1: TMenuItem;
    Button1: TButton;
    procedure CopyLatitudeLongitude1Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FilterOptions1Click(Sender: TObject);
    procedure DeleteSelectedRecords1Click(Sender: TObject);
    procedure ID1Click(Sender: TObject);
    procedure State1Click(Sender: TObject);
    procedure Latitude1Click(Sender: TObject);
    procedure Longitude1Click(Sender: TObject);
    procedure Description1Click(Sender: TObject);
    procedure DateAdded1Click(Sender: TObject);
    procedure DateUpdated1Click(Sender: TObject);
    procedure RefCount1Click(Sender: TObject);
    procedure FillFieldinSelectedRecords1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ClearFilterOptions1Click(Sender: TObject);
    procedure CopyLocation1Click(Sender: TObject);
//  procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    fSavedOnScroll: TDataSetNotifyEvent;
    fSavedAfterPost: TDataSetNotifyEvent;
    fSavedAfterRefresh: TDataSetNotifyEvent;

    fFilterDescription: string;
    fFilterMaxDistanceUnits: TMeasurementUnits;
    fFilterMaxDistanceText: string;
    fFilterLatitude: double;
    fFilterLongitude: double;
    fFilterLatitudeText: string;
    fFilterLongitudeText: string;
//  fTotalRecordCount: integer;
    fRecordsFound: integer;
    fFilterState: string;
    fLocationSearchType: TLocationSearchType;
    fLocationID: integer;
    fBaseLatitude: double;
    fBaseLongitude: double;
    fMaxDistanceNumeric: double;
    procedure UpdateDisplayedDistance(DataSet: TDataSet);
    procedure SetFilterDescription(const Value: string);
    procedure SetFilterLatitude(const Value: double);
    procedure SetFilterLongitude(const Value: double);
    procedure UpdateHint;
    function GetFilterMaxDistanceText: string;
    procedure SetKey_Words(const Value: string);
    procedure SetFilterMaxDistanceText(const Value: string);
    function GetFilterLatitude: double;
    function GetFilterLongitude: double;
    function GetFilterMaxDistanceNumeric: double;
    procedure SetLocationSearchType(const Value: TLocationSearchType);
    procedure SetLocationID(const Value: integer);
    function GetFilterDescription: string;
    function LocationsFilter: TfrmFilterOptions;
    procedure Update_Status(msg: string; Color: TColor = clBlack);
    procedure ResetProcessedCount(DataSet: TDataSet);
  protected
    function AllowFiltering: boolean; override;
    function CalcDescriptionString: string; override;
    procedure ClearFilterOptions; override;
    procedure SaveFilterParams; override;
  public
    { Public declarations }
    property FilterLatitude: double
             read GetFilterLatitude
             write SetFilterLatitude;
    property FilterLongitude: double
             read GetFilterLongitude
             write SetFilterLongitude;
    property FilterMaxDistanceText: string
             read GetFilterMaxDistanceText
             write SetFilterMaxDistanceText;
    property FilterMaxDistanceNumeric: double
             read GetFilterMaxDistanceNumeric;
    property FilterDescription: string
             read GetFilterDescription
             write SetFilterDescription;
    property FilterMaxDistanceUnits: TMeasurementUnits
             read fFilterMaxDistanceUnits
             write fFilterMaxDistanceUnits;
    property FilterState: string
             read fFilterState
             write fFilterState;
    property Key_Words: string
             write SetKey_Words;
    property LocationSearchType: TLocationSearchType
             read fLocationSearchType
             write SetLocationSearchType;
    property DataSet: TDataSet
             read fDataSet
             write fDataSet;
    property LocationID: integer
             read fLocationID
             write SetLocationID;
    Constructor Create(aOwner: TComponent; aDataSet: TDataSet; BaseLatitude, BaseLongitude: double; BaseDescription: string); reintroduce;
    Destructor Destroy; override;
    procedure FilterRecord(Dataset: TDataSet; var Accept: boolean); override;
  end;

implementation

uses
  PDB_Decl, PDBTables, CopySelectedRecords, MyUtils,
  Clipbrd, ReplaceFieldsInSelectedRecords, HikingSettingsUnit, ParseExpr;

{$R *.dfm}

{ TfrmLocationsBrowser }

procedure TfrmLocationsBrowser.UpdateHint;
begin
  case LocationSearchType of
    st_Normal:
      if (FilterLatitude <> 0.0) and (FilterLongitude <> 0.0) then
        lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [FilterLatitude, FilterLongitude, FilterDescription])
      else
        lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [fBaseLatitude, fBaseLongitude, FilterDescription]);

    st_Description:
        lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [fBaseLatitude, fBaseLongitude, FilterDescription]);

    st_Location:
        lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [FilterLatitude, FilterLongitude, FilterDescription]);
  end;
  lblDistanceAway.ShowHint := true;
end;


procedure TfrmLocationsBrowser.ResetProcessedCount(DataSet: TDataSet);
begin
  fRecordsFound           := 0;
end;


constructor TfrmLocationsBrowser.Create(aOwner: TComponent; aDataSet: TDataSet; BaseLatitude, BaseLongitude: double; BaseDescription: string);
begin
  inherited Create(aOwner, aDataSet, 'Locations Browser');
  fBaseLatitude         := BaseLatitude;
  fBaseLongitude        := BaseLongitude;
  FilterLatitude        := BaseLatitude;
  FilterLongitude       := BaseLongitude;
  FilterDescription     := BaseDescription;
  fDataSet              := aDataSet;
  fSavedOnScroll        := fDataSet.AfterScroll;
  fSavedAfterPost       := fDataset.AfterPost;
  fSavedAfterRefresh    := fDataSet.AfterRefresh;
  fDataSet.AfterScroll  := UpdateDisplayedDistance;
  fDataSet.AfterPost    := ResetProcessedCount;
  fDataSet.AfterRefresh := ResetProcessedCount;
  UpdateDisplayedDistance(fDataSet);
end;

procedure TfrmLocationsBrowser.UpdateDisplayedDistance(DataSet: TDataSet);    // Called by AfterScroll
var
  DistanceInFeet, DistanceInMiles: double;
begin
  with fDataSet as TLocationsTable do
    begin
      DistanceInFeet := 0.0;
      case LocationSearchType of
        st_Normal:
          if (FilterLatitude <> 0.0) and (FilterLongitude <> 0.0) then
            DistanceInFeet := Distance(FilterLatitude, FilterLongitude, fldLatitude.AsFloat, fldLongitude.AsFloat, muFeet)
          else
            DistanceInFeet := Distance(fBaseLatitude, fBaseLongitude, fldLatitude.AsFloat, fldLongitude.AsFloat, muFeet);

        st_Description:
          DistanceInFeet := Distance(fBaseLatitude, fBaseLongitude, fldLatitude.AsFloat, fldLongitude.AsFloat, muFeet);

        st_Location:
          DistanceInFeet := Distance(FilterLatitude, FilterLongitude, fldLatitude.AsFloat, fldLongitude.AsFloat, muFeet);
      end;
      DistanceInMiles := 0.0;
      if DistanceInFeet > FEETPERMILE then
        begin
          DistanceInMiles := DistanceInFeet / FEETPERMILE;
          DistanceInFeet  := -1;
        end;
      if (DistanceInMiles > 0) and (DistanceInMiles < 12000) then
        lblDistanceAway.Caption := Format('Distance: %7.2f miles', [DistanceInMiles]) else
      if DistanceInFeet >= 0 then
        lblDistanceAway.Caption := Format('Distance: %d feet', [Trunc(DistanceInFeet)])
      else
        lblDistanceAway.Caption := 'Distance: Unknown';
    end;
end;


destructor TfrmLocationsBrowser.Destroy;
begin
  fDataSet.AfterScroll  := fSavedOnScroll;
  fDataSet.AfterPost    := fSavedAfterPost;
  fDataSet.AfterRefresh := fSavedAfterRefresh;
  inherited;
end;

function TfrmLocationsBrowser.GetFilterDescription: string;
begin
  case LocationSearchType of
    st_Normal:
      if (FilterLatitude <> 0.0) and (FilterLongitude <> 0.0) then
        result := 'Specified Latitude, Longitude'
      else
        result := fFilterDescription;

    st_Description:
      result := fFilterDescription;

    st_Location:
      result := 'Specified Latitude, Longitude'
  end;
end;

procedure TfrmLocationsBrowser.SetFilterDescription(const Value: string);
begin
  fFilterDescription := Value;
  UpdateHint;
end;

procedure TfrmLocationsBrowser.SetFilterLatitude(const Value: double);
begin
  fFilterLatitude := Value;
  UpdateHint;
end;

procedure TfrmLocationsBrowser.SetFilterLongitude(const Value: double);
begin
  fFilterLongitude := Value;
  UpdateHint;
end;

procedure TfrmLocationsBrowser.FilterRecord(
  DataSet: TDataSet; var Accept: boolean);
var
  Dist: double;
begin
  inherited FilterRecord(DataSet, Accept);
  if Accept then
    begin
      with DataSet as TLocationsTable do
        begin
          if (FilterLatitude <> 0) or (FilterLongitude <> 0) then
            begin
              Dist := Distance(FilterLatitude, FilterLongitude, fldLatitude.AsFloat, fldLongitude.AsFloat, FilterMaxDistanceUnits);

              Accept := (FilterMaxDistanceNumeric = 0) or (Dist <= FilterMaxDistanceNumeric);
            end;
          if Accept then
            if not Empty(fFilterState) then
              Accept := SameText(fFilterState, fldState.AsString);
          if Accept then
            Inc(fRecordsFound);
        end;
    end;
end;

function TfrmLocationsBrowser.GetFilterMaxDistanceText: string;
begin
  result := fFilterMaxDistanceText;
end;

procedure TfrmLocationsBrowser.SetKey_Words(const Value: string);
begin
end;

procedure TfrmLocationsBrowser.SetFilterMaxDistanceText(const Value: string);
var
  aMaxDistance: double;
  aMeasurementUnits: TMeasurementUnits;
begin
  fFilterMaxDistanceText := Value;
  if ParseDistance(fFilterMaxDistanceText, aMaxDistance, aMeasurementUnits) then
    begin
      fMaxDistanceNumeric       := aMaxDistance;
      fFilterMaxDistanceUnits   := aMeasurementUnits;
    end
end;

procedure TfrmLocationsBrowser.CopyLatitudeLongitude1Click(
  Sender: TObject);
begin
  inherited;
  with fDataSet as TLocationsTable do
    Clipboard.AsText := Format('%11.7f %11.7f', [fldLatitude.AsFloat, fldLongitude.AsFloat]);
end;

function TfrmLocationsBrowser.GetFilterLatitude: double;
begin
  result := fFilterLatitude;
end;

function TfrmLocationsBrowser.GetFilterLongitude: double;
begin
  result := fFilterLongitude;
end;

function TfrmLocationsBrowser.GetFilterMaxDistanceNumeric: double;
var
  MaxDistance: double;
  MaxDistanceUnits: TMeasurementUnits;
begin
  if fMaxDistanceNumeric > 0 then
    result := fMaxDistanceNumeric
  else
    if ParseDistance(fFilterMaxDistanceText, MaxDistance, MaxDistanceUnits) then
      begin
        fMaxDistanceNumeric       := MaxDistance;
        result                    := fMaxDistanceNumeric;
        fFilterMaxDistanceUnits   := MaxDistanceUnits;
      end
    else
      result := 0.0;
end;

procedure TfrmLocationsBrowser.SetLocationSearchType(
  const Value: TLocationSearchType);
begin
  fLocationSearchType := Value;
  case Value of
    st_Normal:
      lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [fBaseLatitude, fBaseLongitude, 'Home']);
    st_Description:
      lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [fBaseLatitude, fBaseLongitude, 'Home']);
    st_Location:
      lblDistanceAway.Hint     := Format('(%10.6f, %10.6f) %s', [FilterLatitude, FilterLongitude, FilterDescription]);
  end;
end;

procedure TfrmLocationsBrowser.btnCloseClick(Sender: TObject);
begin
  inherited;
  fLocationID := fDataSet.FieldByName(cID).AsInteger;
  ModalResult := mrOK;
end;

procedure TfrmLocationsBrowser.SetLocationID(const Value: integer);
begin
  fLocationID := Value;
  if not fDataSet.Locate(cID, Value, []) then
    fDataSet.First;
end;

function TfrmLocationsBrowser.AllowFiltering: boolean;
begin
  result := (inherited AllowFiltering) or (FilterLatitude <> 0.0) or (FilterLongitude <> 0.0) or (not Empty(FilterState));
end;

function TfrmLocationsBrowser.CalcDescriptionString: string;
begin
  result := inherited CalcDescriptionString;
  
  if not Empty(fFilterLatitudeText) then
    AddClause(result, 'Lat', fFilterLatitudeText);
  if not Empty(fFilterLongitudeText) then
    AddClause(result, 'Lon', fFilterLongitudeText);
  if not Empty(fFilterMaxDistanceText) then
    AddClause(result, 'MaxDist', fFilterMaxDistanceText);
end;

procedure TfrmLocationsBrowser.ClearFilterOptions;
begin
  fFilterLatitude        := 0.0;
  fFilterLongitude       := 0.0;
  fFilterMaxDistanceText := '';
  fFilterState           := '';

  inherited ClearFilterOptions;
end;

procedure TfrmLocationsBrowser.SaveFilterParams;
begin
  with ffrmFilterOptions as TfrmFilterLocation do
    begin
      fFilterLatitude        := ovcLatitude.AsFloat;
      fFilterLongitude       := ovcLongitude.AsFloat;
      FilterMaxDistanceText  := cbMaxDist.Text;
      fFilterState           := leState.text;
    end;
  inherited SaveFilterParams;
end;

function TfrmLocationsBrowser.LocationsFilter: TfrmFilterOptions;
begin
  if not Assigned(ffrmFilterOptions) then
    ffrmFilterOptions := TfrmFilterLocation.Create(self, DataSet);
  with ffrmFilterOptions as TfrmFilterLocation do
    begin
      ovcLatitude.AsFloat  := fFilterLatitude;
      ovcLongitude.AsFloat := fFilterLongitude;
      TheLocationSearchType   := fLocationSearchType;
      with cbMaxDist do
        ItemIndex := Items.IndexOf(fFilterMaxDistanceText);
    end;
  result := ffrmFilterOptions;
end;

procedure TfrmLocationsBrowser.FilterOptions1Click(Sender: TObject);
begin
  if LocationsFilter.ShowModal = mrOk then
    ResetFilter;
end;

procedure TfrmLocationsBrowser.DeleteSelectedRecords1Click(
  Sender: TObject);
var
  Saved_RecNo: integer;
begin
  inherited;
  with DataSet do
    begin
      First;
      Saved_Recno := RecNo;
      while not eof do
        begin
          Delete;
          if RecNo = Saved_RecNo then
            Next;
        end;
    end;
end;

procedure TfrmLocationsBrowser.ID1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := cID;
  ID1.Checked := true;
end;

procedure TfrmLocationsBrowser.State1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'State';
  State1.Checked := true;
end;

procedure TfrmLocationsBrowser.Latitude1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'Latitude';
  Latitude1.Checked := true;
end;

procedure TfrmLocationsBrowser.Longitude1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'Longitude';
  Longitude1.Checked := true;
end;

procedure TfrmLocationsBrowser.Description1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'Description';
  Description1.Checked := true;
end;

procedure TfrmLocationsBrowser.DateAdded1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'DateAdded';
  DateAdded1.Checked := true;
end;

procedure TfrmLocationsBrowser.DateUpdated1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'DateUpdated';
  DateUpdated1.Checked := true;
end;

procedure TfrmLocationsBrowser.RefCount1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TLocationsTable).IndexFieldNames := 'RefCnt';
  RefCount1.Checked := true;
end;

procedure TfrmLocationsBrowser.Update_Status(msg: string; Color: TColor = clBlack);
begin
  lblStatus.Caption := msg;
  if Color <> clBlack then
    lblstatus.Color := Color;
  Application.ProcessMessages;
end;


procedure TfrmLocationsBrowser.FillFieldinSelectedRecords1Click(
  Sender: TObject);
var
  TempLocationsTable: TLocationsTable;
  FieldName: string;
  Field: TField;
  frmReplaceFieldsInSelectedRecords: TfrmReplaceFieldsInSelectedRecords;
  Count: integer;
  ValueParser: TParser;
  ErrorMsg: string;
  SavedRecNo: integer;
begin
  frmReplaceFieldsInSelectedRecords := TfrmReplaceFieldsInSelectedRecords.Create(self, DataSet);
  try
    with frmReplaceFieldsInSelectedRecords do
      begin
        if ShowModal = mrOk then
          begin
            Update_Status('Now processing');
            Count              := 0;
            TempLocationsTable     := TLocationsTable.Create( self,
                                                      HikingSettings.PhotoDBDatabaseFileName,
                                                      cLOCATIONS,
                                                      []);
            with TempLocationsTable do
              begin
                OnFilterRecord := FilterRecord;
                AddFields;

                Filtered       := true;

                Active         := true;
                SetSelectivityParserExpression(Expression);
                Field          := FindField(frmReplaceFieldsInSelectedRecords.FieldToModify);
                if Assigned(Field) then
                  begin
                    try
                      ValueParser    := TParser.Create;
                      if ValueParser.Valid_Expr(frmReplaceFieldsInSelectedRecords.Expression, TempLocationsTable, ErrorMsg) then
                        begin
                          First;
                          while not Eof do
                            begin
                              SavedRecNo := RecNo;
                              ValueParser.Eval_Tree.EvaluateTree;
                              if not SameText(Field.AsString, ValueParser.Eval_Tree.AsString) then
                                begin
                                  Edit;
                                  Field.AsString := ValueParser.Eval_Tree.AsString;
                                  Post;
                                end;

                              if RecNo = SavedRecNo then // previous record may have been removed from the selection set
                                Next;

                              inc(Count);
                              Update_Status( Format('Now processing #%d', [Count]));
                              Application.ProcessMessages;
                            end;
                          Update_Status( Format('Processing complete. %d records updated', [Count]));
                          DataSet.Refresh;
                          Application.ProcessMessages;
                        end
                      else
                        AlertFmt('Invalid expression: %s [%s]', [frmReplaceFieldsInSelectedRecords.Expression, ErrorMsg]);
                    except
                      on e:Exception do
                        Alert(e.Message);
                    end;
                  end
                else
                  AlertFmt('Field "%s" does not exist in the dataset', [FieldName]);
              end;
          end;
      end;
  finally
    FreeAndNil(frmReplaceFieldsInSelectedRecords);
  end;
end;

procedure TfrmLocationsBrowser.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
//  inherited;  // Don't do the inherited which will clear the filter and reposition the table
  Action := caHide;
end;

procedure TfrmLocationsBrowser.FormShow(Sender: TObject);
begin
  inherited;
  fMaxDistanceNumeric := 0.0;
end;

procedure TfrmLocationsBrowser.ClearFilterOptions1Click(Sender: TObject);
begin
  inherited;
//
end;

(* 8/24/2023 - DOES NOT WORK
procedure TfrmLocationsBrowser.FormResize(Sender: TObject);
begin
  inherited;
  EnableScrollBar(DBGrid1.Handle, SB_BOTH, ESB_ENABLE_BOTH);
  ShowScrollBar(DbGrid1.Handle, SB_BOTH, True);
end;
*)

procedure TfrmLocationsBrowser.CopyLocation1Click(Sender: TObject);
begin
  inherited;

  with DataSet as TLocationsTable do
    begin
      gLocationID  := fldID.AsInteger;
      gLatitude    := fldLatitude.AsFloat;
      gLongitude   := fldLongitude.AsFloat;
    end;
  Clipboard.AsText := Format('%11.7f %11.7f', [gLatitude, gLongitude]);
end;

end.
