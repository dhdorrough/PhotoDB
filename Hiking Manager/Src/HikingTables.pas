unit HikingTables;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  ADODB,
  Classes, DB, MyTables_Decl, MyTables;

const
  HIKINGLOGQUERY = 'HikingLog Query';
  cTRACKS        = 'Tracks';
  BOUNDING_RECTANGLES = 'Bounding Rectangles';
  HIKING_LOG_TRACK_QUERY_NAME = 'HikingLog - Track';
  HIKE_SEGMENTS_ON_A_DATE = 'Hike Segments on a Date';
  cFILENAME = 'FileName';
  cID       = 'ID';

type
  THikingLogTable = class(TMyTable)
  protected
    procedure DoBeforePost; override;
  public
    fldID: TField;
    fldTrail: TField;
    fldArea: TField;
    fldState: TField;
    fldSegment: TField;
    fldHikeDate: TField;
    fldEndDate: TField;
    fldTotal_Distance: TField;
    fldMainTrailNewMiles: TField;
    fldBike_Miles: TField;
    fldComments: TField;
    fldPhotos: TField;
    fldBoth_Ways: TField;
    fldPrivate: TField;
    fldDateAdded: TField;
    fldDateUpdated: TField;
    fldOtherHikers: TField;

    constructor Create( aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions); override;
    procedure DoAfterOpen; override;
  end;

  TTracksTable = class(TMyTable)
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforePost; override;
  public
    fldID: TField;
    fldStartDate: TField;
    fldEndDate: TField;
    fldStartLatitude: TField;
    fldEndLatitude: TField;
    fldStartLongitude: TField;
    fldEndLongitude: TField;
    fldFileName: TField;
    fldHikeID: TField;
    fldDateAdded: TField;
    fldDateUpdated: TField;
    fldBRLeft: TField;
    fldBRTop: TField;
    fldBRRight: TField;
    fldBRBottom: TField;

    constructor Create( aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions); reintroduce;
    Destructor Destroy; override;
  end;

  TBoundariesTable = class(TADOTable)
  private
    fOptions: TPhotoTableOptions;
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforePost; override;
  public
    fldID: TField;
    fldDescription: TField;
    fldULLeft: TField;
    fldULTop: TField;
    fldBRRight: TField;
    fldBRBottom: TField;
    fldDateAdded: TField;
    fldDateUpdated: TField;
    fldFileNames: TField;
    fldAbbrev: TField;
    constructor Create( aOwner: TComponent;
                        aTableName: string;
                        Options: TPhotoTableOptions); reintroduce; overload;
    constructor Create( aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions); reintroduce; overload;
    Destructor Destroy; override;
  end;

  THikingLogTracksQuery = class(TMyTable)
  private
  protected
    procedure DoAfterOpen; override;
//  procedure DoBeforeOpen; override;
  public
    fldID: TField;
    fldTrail: TField;
    fldArea: TField;
    fldState: TField;
    fldSegment: TField;
    fldHikeDate: TField;
    fldTotal_Distance: TField;
    fldNCT_Miles: TField;
    fldBike_Miles: TField;
    fldComments: TField;
    fldPhotos: TField;
    fldBoth_Ways: TField;
    fldStartDate: TField;
    fldEndDate: TField;
    fldStartLatitude: TField;
    fldEndLatitude: TField;
    fldStartLongitude: TField;
    fldEndLongitude: TField;
    fldFileName: TField;
    fldHikeID: TField;
    fldBRTop: TField;
    fldBRLeft: TField;
    fldBRBottom: TField;
    fldBRRight: TField;

    constructor Create(aOwner: TComponent;
      aDBFilePathName, aTableName: string; Options: TPhotoTableOptions); override;
    procedure UpdateCalculatedFields(Dataset: TDataSet);
  end;

  THikingLogHikesOnADateQuery = class(TADOQuery)
    constructor Create(aOwner: TComponent); override;
  end;

var
  gBoundariesTable: TBoundariesTable;
  gHikingLogTracksQuery: THikingLogTracksQuery;

function HikingLogTracksQuery: THikingLogTracksQuery;


implementation

uses
  SysUtils, {PhotoDBCommonSettings,} MyUtils, HikingSettingsUnit;


{ THikingLogTable }

constructor THikingLogTable.Create(aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions);
begin
  inherited;
end;

procedure THikingLogTable.DoAfterOpen;
begin
  inherited;
  fldID             := FieldByName(cID);
  fldTrail          := FieldByName('Trail');
  fldArea           := FieldByName('Area');
  fldState          := FieldByName('State');
  fldSegment        := FieldByName('Segment');
  fldHikeDate       := FieldByName('HikeDate');
  fldEndDate        := FieldByName('EndDate');
  fldTotal_Distance := FieldByName('Total_Distance');
  fldMainTrailNewMiles := FieldByName('NCT_Miles');
  fldBike_Miles     := FieldByName('Bike_Miles');
  fldComments       := FieldByName('Comments');
  fldPhotos         := FieldByName('Photos');
  fldBoth_Ways      := FieldByName('Both_Ways');
  fldPrivate        := FieldByName('Private');
  fldDateAdded      := FieldByName('DateAdded');
  fldDateUpdated    := FieldByName('DateUpdated');
  fldOtherHikers    := FieldByName('Other Hikers');
end;

procedure THikingLogTable.DoBeforePost;
begin
  inherited;
  if Assigned(fldDateAdded) and
     (Empty(fldDateAdded.AsString) or fldDateAdded.IsNull) then
    fldDateAdded.AsDateTime := Now;
  if Assigned(fldDateUpdated) then
    fldDateUpdated.AsDateTime := Now;
end;

{ TTracksTable }

constructor TTracksTable.Create(aOwner: TComponent; aDBFilePathName, aTableName: string;
  Options: TPhotoTableOptions);
begin
  inherited;
end;

destructor TTracksTable.Destroy;
begin

  inherited;
end;

procedure TTracksTable.DoAfterOpen;
begin
  inherited;
  fldID             := FieldByName(cID);
  fldStartDate      := FieldByName('StartDate');
  fldEndDate        := FieldByName('EndDate');
  fldStartLatitude  := FieldByName('StartLatitude');
  fldEndLatitude    := FieldByName('EndLatitude');
  fldStartLongitude := FieldByName('StartLongitude');
  fldEndLongitude   := FieldByName('EndLongitude');
  fldFileName       := FieldByName(cFILENAME);
  fldHikeID         := FieldByName('HikeID');
  fldDateAdded      := FieldByName('DateAdded');
  fldDateUpdated    := FieldByName('DateUpdated');
  fldBRLeft         := FieldByName('BRLeft');
  fldBRTop          := FieldByName('BRTop');
  fldBRRight        := FieldByName('BRRight');
  fldBRBottom       := FieldByName('BRBottom');
end;

procedure TTracksTable.DoBeforePost;
begin
  inherited;
  if Assigned(fldDateAdded) and
     (Empty(fldDateAdded.AsString) or fldDateAdded.IsNull) then
    fldDateAdded.AsDateTime := Now;
  if Assigned(fldDateUpdated) then
    fldDateUpdated.AsDateTime := Now;
end;

{ THikingLogTracksQuery }

constructor THikingLogTracksQuery.Create(aOwner: TComponent;
  aDBFilePathName, aTableName: string; Options: TPhotoTableOptions);
begin
  inherited Create(aOwner, aDBFilePathName, aTableName, Options);
  OnCalcFields      := UpdateCalculatedFields;
end;

procedure THikingLogTracksQuery.DoAfterOpen;
begin
  inherited;
  fldTrail          := FieldByName('Trail');
  fldArea           := FieldByName('Area');
  fldState          := FieldByName('State');
  fldSegment        := FieldByName('Segment');
  fldHikeDate       := FieldByName('HikeDate');
  fldEndDate        := FieldByNAme('EndDate');
  fldTotal_Distance := FieldByName('Total_Distance');
  fldNCT_Miles      := FieldByName('NCT_Miles');
  fldBike_Miles     := FieldByName('Bike_Miles');
  fldComments       := FieldByName('Comments');
//fldPhotos         := FieldByName('Photos');
//fldBoth_Ways      := FieldByName('Both_Ways');
  fldStartDate      := fldHikeDate;
  fldEndDate        := FieldByName('EndDate');
  fldStartLatitude  := FieldByName('StartLatitude');
  fldEndLatitude    := FieldByName('EndLatitude');
  fldStartLongitude := FieldByName('StartLongitude');
  fldEndLongitude   := FieldByName('EndLongitude');
  fldFileName       := FieldByName(cFILENAME);
  fldBRLeft         := FieldByName('BRLeft');
  fldBRTop          := FieldByName('BRTop');
  fldBRRight        := FieldByName('BRRight');
  fldBRBottom       := FieldByName('BRBottom');
end;

procedure THikingLogTracksQuery.UpdateCalculatedFields(Dataset: TDataSet);
begin
  if Active then
    FieldByName('SegmentX').AsString := FieldByName('Segment').AsString;
end;

{ TBoundariesTable }

constructor TBoundariesTable.Create(aOwner: TComponent; aTableName: string;
  Options: TPhotoTableOptions);
begin
  Create(aOwner, HikingSettings.HikingMdbPathName, aTableName, Options);
end;

constructor TBoundariesTable.Create(aOwner: TComponent; aDBFilePathName,
  aTableName: string; Options: TPhotoTableOptions);
begin
  inherited Create(aOwner);

  Connection        := MyConnection(aDBFilePathName);
  TableName         := aTableName;
  fOptions          := Options;
  TableDirect       := true;

  if optUseClient in Options then
    CursorLocation  := clUseClient
  else
    CursorLocation  := clUseServer;  // enabled 2/16/2005- trying to speed up

  if optReadOnly in Options then
    LockType          := ltReadOnly;
end;

destructor TBoundariesTable.Destroy;
begin

  inherited;
end;

procedure TBoundariesTable.DoAfterOpen;
begin
  inherited;
  fldID             := FieldByName(cID);
  fldDateAdded      := FieldByName('DateAdded');
  fldDescription    := FieldByName('Description');
  fldDateUpdated    := FieldByName('DateUpdated');
  fldULLeft         := FieldByName('BRLeft');
  fldULTop          := FieldByName('BRTop');
  fldBRRight        := FieldByName('BRRight');
  fldBRBottom       := FieldByName('BRBottom');
  fldFileNames      := FieldByName('FileNames');
  fldAbbrev         := FieldByName('Abbrev');
end;

procedure TBoundariesTable.DoBeforePost;
begin
  inherited;
  if Assigned(fldDateAdded) and
     (Empty(fldDateAdded.AsString) or fldDateAdded.IsNull) then
    fldDateAdded.AsDateTime := Now;
  if Assigned(fldDateUpdated) then
    fldDateUpdated.AsDateTime := Now;
end;

{ THikingLogHikesOnADateQuery }

constructor THikingLogHikesOnADateQuery.Create(aOwner: TComponent);
begin
  inherited;
  Connection        := MyConnection(HikingSettings.HikingMdbPathName);
end;

function HikingLogTracksQuery: THikingLogTracksQuery;
begin
  if not Assigned(gHikingLogTracksQuery) then
    begin
      gHikingLogTracksQuery := THikingLogTracksQuery.Create(nil, HikingSettings.HikingMdbPathName, HIKING_LOG_TRACK_QUERY_NAME, []);
      gHikingLogTracksQuery.Open;
    end;
  result := gHikingLogTracksQuery;
end;

initialization
finalization
  FreeAndNil(gHikingLogTracksQuery);
end.
