{$Define MemCheck}
{$Undef debugging}
{$Undef DumpRange}

unit GPSTrackingInfoUnit;

interface

uses
  MyUtils, Classes, LocationUtils, GPX, GPXFile, PDB_Decl
{$IfDef dhd}
  , HikingTables
{$EndIf}
  ;

const
  ONE_HOUR   = 1 / 24;
  ONE_MINUTE = ONE_HOUR / 60;
  MAX_TIME_DIFFERENCE_IN_MINUTES = 15;
  MAX_TIME_DIFFERENCE    = MAX_TIME_DIFFERENCE_IN_MINUTES * ONE_MINUTE;


type

  TWayPointInfo = record
    Lat: double;
    Lon: double;
    DateTime: TDateTime;
    ID: integer;
  end;

  PWayPointInfo = ^TWayPointInfo;

  TPointArray = array of TWayPointInfo;

  TTrackDataSource = (tds_Unknown, tds_GPXLogs, tds_csvTrackFile, tds_GPXLogsAndTrackFile);

  TGPSTrackingInfo = class(TObject)
  private
    fCurrentIndex      : integer;
    fChanged           : boolean;
    fDSTStart,
    fDSTEnd            : TDateTime;
    fFileNames         : TStringList;
    fLastYear          : word;
    fGPXdata           : TGPX;
    fGPXPathFilter     : string;
    fGPXFilesLoaded    : boolean;
    fGPXf              : TGPXFIle;
    fOnUpdateStatus    : TUpdateStatusProc;
    fSavedTrackDataLfn : string;
    fTotalWayPoints    : integer;
    fTrackDataLoaded   : boolean;
//  fTrackDataSource   : TTrackDataSource;
{$IfDef dhd}
    fTracksTable       : TTracksTable;
{$EndIf}    
    procedure QuickSort(L, R: Integer);
    procedure ClearTrackData;
    function GetGPXf: TGPXFIle;
    procedure SetGPXf(const Value: TGPXFIle);
    procedure SetSavedTrackDataLfn(const Value: string);
    procedure SetOnUpdateStatus(const Value: TUpdateStatusProc);
{$IfDef dhd}
    function GetFileID(const FileName: string): integer;
    function TracksTable: TTracksTable;
    function FileNameFromID(anID: integer): string;
{$EndIf}
    procedure SetCurrentIndex(const Value: integer);
    procedure DumpRange(const FileName: string; const Msg: string; L, R: integer;
      DoAppend: boolean);
  public
    FileCount1, FileCount2: integer;
    FolderCount1, FolderCount2: integer;
    Points   : TPointArray;
    LowestDateFound, HighestDateFound: TDateTime;

    function CountOfPoints: integer;
    constructor Create(const aGPXPathFilter: string; const aSavedTrackDataLfn: string); reintroduce;
    Destructor Destroy; override;
    property CurrentIndex: integer
             read fCurrentIndex
             write SetCurrentIndex;
    function EndLatitude: double;
    function EndLongitude: double;
    function FileNames: TStringList;

    function FindNearestPointInTracks(Latitude, Longitude: double; var ShortestDistanceInFeet: double; var FileName: string): TWayPointInfo;
    function FindClosestMatchingDateInTracks( ExifDate: TDateTime;
                                              var FoundMatch: boolean;
                                              MinutesToAdd: single): TWayPointInfo;
    function GetFarthestPointFrom(StartLatitude, StartLongitude: double;
                                  var FarthestLatitude, FarthestLongitude: double;
                                  Units: TMeasurementUnits): double;

    function GetLatitudeLongitudeFromGPXLogs( var Latitude, Longitude: double;
                                                           UpdateInfo: TUpdateInfo;
                                                           LocationInfo: TLocationInfo): boolean;
    property GlobalGPXf     : TGPXFIle
             read GetGPXf
             write SetGPXf;

    function HighestDate: TDateTime;

    procedure LoadSelectedTrackData( LocationInfo: TLocationInfo);

    function LoadGPXFile( const FileName: string;
                          LowestDateToInclude, HighestDateToInclude: TDateTime;
                          MinDistanceInFeet: double = 0.0;
                          MinTimeInSeconds: double = 0.0;
                          ShowStatus: boolean = true): integer;

    procedure LoadGPXFiles( PathFilter: string;
                            LowestDateToInclude, HighestDateToInclude: TDateTime;
                            LocationInfo: TLocationInfo;
                            MinDistanceInFeet: double = 0.0;
                            MinTimeInSeconds: double = 0.0); virtual;

    function LoadTrackData(var Lfn: string; LowestDateToInclude, HighestDateToInclude: TDateTime): TDateTime; virtual;
    function LowestDate: TDateTime;
    property OnUpdateStatus: TUpdateStatusProc
             read fOnUpdateStatus
             write SetOnUpdateStatus;
    procedure ResetTrackingInfo;
    function SaveTrackData(Lfn: string): boolean; virtual;
    procedure SortWayPoints(var NrPointsDeleted: integer);
    property SavedTrackDataLfn: string
             read fSavedTrackDataLfn
             write SetSavedTrackDataLfn;
    function StartLatitude: double;
    function StartLongitude: double;
    property TotalWayPoints: integer
             read fTotalWayPoints
             write fTotalWayPoints;
    procedure UpdateStatus(const Msg: string; LineNo: integer = 1); virtual;
    function WaypointsInFile(const FileName: string): integer;
    property TrackDataLoaded: boolean
             read fTrackDataLoaded
             write fTrackDataLoaded;
  end;

var
  gGPSTrackingInfo: TGPSTrackingInfo;

function CreateGPSTrackingInfo( const DefaultGPXFilter, SavedTrackDataLfn: string;
                                    anUpdateStatusProc: TUpdateStatusProc = nil): TGPSTrackingInfo;

implementation

uses
  SysUtils, MyDelimitedParser, DateUtils, PDBUtils,
  Math, HikingsettingsUnit, OverwriteOrAppend, PhotoDBCommonSettings;

function CreateGPSTrackingInfo(const DefaultGPXFilter, SavedTrackDataLfn: string;
                                   anUpdateStatusProc: TUpdateStatusProc = nil): TGPSTrackingInfo;
begin
  if not Assigned(gGPSTrackingInfo) then
    gGPSTrackingInfo := TGPSTrackingInfo.Create(DefaultGPXFilter, SavedTrackDataLfn);

  gGPSTrackingInfo.OnUpdateStatus := anUpdateStatusProc;
  result := gGPSTrackingInfo;
end;

{ TGPSTrackingInfo }


  procedure TGPSTrackingInfo.DumpRange(const FileName: string; const Msg: string; L, R: integer; DoAppend: boolean);
{$IfDef DumpRange}
  var
    DebugFile: TextFile;
    i: integer;
    LastDateTime: double;
{$EndIf}
  begin { DumpRange }
{$IfDef DumpRange}
    AssignFile(DebugFile, FileName);
    if DoAppend then
      Append(DebugFile)
    else
      ReWrite(DebugFile);

    WriteLn(DebugFile, '======================================================');
    Writeln(DebugFile, Msg, '':10, DateTimeToStr(Now));
    WriteLn(DebugFile, '------------------------------------------------------');
    try
      LastDateTime := 0;
      for i := L to R do
        try
          with Points[i] do
            begin
              Write(DebugFile, I:5, ': ', DateTimeToStr(DateTime), ' ', Lat:10:7, ' ', Lon:10:7);
              if DateTime < LastDateTime then
                Write(DebugFile, ' <==== OUT OF ORDER');
              WriteLn(DebugFile);
              LastDateTime := DateTime;
            end;
        except
          UpdateStatus(Format('Invalid date/time at line %i', [i]))
        end;
    finally
      Writeln(DebugFile);
      CloseFile(DebugFile);
    end;
{$EndIf}
  end;  { DumpRange }

procedure TGPSTrackingInfo.QuickSort(L, R: Integer);
var
  L0: Integer;
  R0: Integer;
  M: Integer;
  P, Pivot: TWayPointInfo;
begin { QuickSort }
  if L < R then
    begin
      L0     := L;
      R0     := R;
      M      := (L+R) shr 1;
      Pivot  := Points[M];
      repeat
        while Points[L0].DateTime < Pivot.DateTime do
          Inc(L0);
        while Points[R0].DateTime > Pivot.DateTime do
          Dec(R0);
        if L0 <= R0 then
        begin
          P          := Points[L0];
          Points[L0] := Points[R0];
          Points[R0] := P;
          Inc(L0);
          Dec(R0);
        end;
      until L0 > R0;
      if R0 > L then QuickSort(L, R0);
      if L0 < R then QuickSort(L0, R) ;
    end;
end;  { QuickSort }

{$IfDef dhd}
function TGPSTrackingInfo.GetFileID(const FileName: string): integer;
var
  idx: integer;
  aFileName: string;
begin
  aFileName := ExtractFileName(FileName);
  Idx       := FileNames.IndexOf(aFileName);
  if Idx >= 0 then
    result := integer(FileNames.Objects[Idx])
  else
    if TracksTable.Locate(cFILENAME, aFileName, []) then
      begin
        result := TracksTable.fldID.AsInteger;
        FileNames.AddObject(aFileName, TObject(result));
      end
    else
      result := -1;
end;
{$EndIf}


function TGPSTrackingInfo.LoadGPXFile( const FileName: string;
                                       LowestDateToInclude, HighestDateToInclude: TDateTime;
                                       MinDistanceInFeet: double = 0.0;
                                       MinTimeInSeconds: double = 0.0;
                                       ShowStatus: boolean = true): integer;
const
  SECONDS_PER_DAY = 24 * 3600;
var
  i, j, k: integer;
  Track: TGPXtrk;
  TrackSeg: TGPXTrkseg;
  GPXf     : TGPXFIle;
  GPXData  : TGPX;
  GoodDate, GoodDistance, GoodTime: boolean;
  PrevDateTime, CurrentDateTime: TDateTime;
  DistanceFromLastInFeet, TimeFromLastInSeconds: double;

  // Adjust DateTime for Longitude and Daylight Savings Time
  function AdjustDateTime(DateTime: TDateTime; Lon: double): TDateTime;
  var
    Offset: integer;
    Year, Month, Day: word;
  begin
    DecodeDate(DateTime, Year, Month, Day);

//  Trying to adjust for DayLight Savings Time

    if Year <> fLastYear then
      begin
        fDSTStart := FindDSTDateForYear(Year, 3, 2);  // begin date of DST
        fDSTEnd   := FindDSTDateForYear(Year, 11, 1); // end date of DST
        fLastYear := Year;
      end;

    offset := Round(-1 * lon * 24 / 360); // rough approximation of the time zone offset

    if (DateTime >= fDSTStart) and (DateTime <= fDSTEnd) then
      result := DateTime - ((offset-1) * ONE_HOUR)
    else
      result := DateTime - (offset * ONE_HOUR);

    // needs an adjustment for latitude
  end;

begin { LoadGPXFile }
  result    := 0;
  GPXf      := TGPXFile.Create;
  GPXData   := TGPX.Create;
  GPXf.GPXd := GPXData;
  try
    if FileExists(FileName) then
      begin
        if ShowStatus then
          UpdateStatus(Format('Processing %s', [FileName]));
        GPXf.FileName := FileName;
        GPXf.ImportGPXFile(GPXf.FileName);
        for i := 0 to GPXf.GPXd.trk.Count-1 do
          begin
            Track := GPXf.GPXd.trk.Tracks[i];
            for j := 0 to Track.Count-1 do
              begin
                TrackSeg := Track.trkseg[j];
                for k := 0 to TrackSeg.Count - 1 do
                  begin
{$IfDef MemCheck}
                    if (fCurrentIndex < 0) or (fCurrentIndex >= Length(Points)) then
                      AlertFmt('Attempted access beyond length of array at index = %d', [fCurrentIndex]);
{$EndIf}
                    with Points[fCurrentIndex] do
                      begin
{$IfDef dhd}
                        ID         := GetFileID(FileName);
{$EndIf}                        

                        DateTime   := AdjustDateTime(TrackSeg.trkpt[k].DateTime, TrackSeg.trkpt[k].Longitude);
                        Lat        := TrackSeg.trkpt[k].Latitude;
                        Lon        := TrackSeg.trkpt[k].Longitude;

                        if LowestDateToInclude <> BAD_DATE then
                          GoodDate   := (Trunc(DateTime) >= LowestDateToInclude)
                        else
                          GoodDate := true;

                        if GoodDate and (HighestDateToInclude <> BAD_DATE) then
                          GoodDate := (Trunc(DateTime) <= HighestDateToInclude);

                        GoodDistance := false;
                        GoodTime     := false;

                        if GoodDate then
                          if MinDistanceInFeet > 0 then
                            begin
                              if (fCurrentIndex > 0) and (fCurrentIndex < Pred(Length(Points))) then
                                DistanceFromLastInFeet := Distance( Points[fCurrentIndex-1].Lat,   // last saved location
                                                                    Points[fCurrentIndex-1].Lon,
                                                                    TrackSeg.trkpt[k].Latitude,                 // current location
                                                                    TrackSeg.trkpt[k].Longitude,
                                                                    muFeet)
                              else
                                DistanceFromLastInFeet := MAXINT;   // always include the first and the last

                              GoodDistance := DistanceFromLastInFeet >= MinDistanceInFeet;
                            end
                          else
                            GoodDistance := true;

                        if GoodDate and GoodDistance then
                          if MinTimeInSeconds > 0.0 then
                            begin
                              if (fCurrentIndex > 0)  and (fCurrentIndex < Pred(Length(Points))) then
                                begin
                                  PrevDateTime := Points[fCurrentIndex-1].DateTime;

                                  with TrackSeg.trkpt[k] do
                                    CurrentDateTime := AdjustDateTime(DateTime, Longitude);

                                  TimeFromLastInSeconds := (CurrentDateTime - PrevDateTime) * SECONDS_PER_DAY;
                                end
                              else
                                TimeFromLastInSeconds := MAXINT;

                              GoodTime              := TimeFromLastInSeconds >= MinTimeInSeconds;
                            end
                          else
                            GoodTime     := true;

                        if GoodDate and GoodDistance and GoodTime then
                          begin
                            CurrentDateTime := Points[fCurrentIndex].DateTime;
                            if CurrentDateTime > 0 then
                              if CurrentDateTime < LowestDateFound then
                                LowestDateFound := CurrentDateTime;
                              if CurrentDateTime > HighestDateFound then
                                HighestDateFound := CurrentDateTime;

                            Inc(fCurrentIndex);
                            Inc(Result);
                          end;
                      end;
                  end;
              end;
          end;
      end;
  finally
    FreeAndNil(GPXData);
    FreeAndNil(GPXf);
  end;
end;  { LoadGPXFile }

function TGPSTrackingInfo.WaypointsInFile(const FileName: string): integer;
var
  i, j: integer;
  Track: TGPXtrk;
  TrackSeg: TGPXTrkseg;
  TempGPXf: TGPXFIle;
  TempGPXData: TGPX;
begin { WaypointsInFile }
  result := 0;
  TempGPXf := TGPXFile.Create;
  TempGPXData  := TGPX.Create;
  TempGPXf.GPXd := TempGPXData;
  try
    if FileExists(FileName) then
      begin
//      UpdateStatus(Format('Counting waypoints in %s', [FileName]));
        TempGPXf.FileName := FileName;
        TempGPXf.ImportGPXFile(TempGPXf.FileName);
        for i := 0 to TempGPXf.GPXd.trk.Count-1 do
          begin
            Track := TempGPXf.GPXd.trk.Tracks[i];
            for j := 0 to Track.Count-1 do
              begin
                TrackSeg := Track.trkseg[j];
                result := result + TrackSeg.Count;
              end;
          end;
      end;
  finally
    TempGPXData.Free;
    TempGPXf.Free;
  end;
end;  { WaypointsInFile }

procedure TGPSTrackingInfo.LoadGPXFiles( PathFilter: string;
                                         LowestDateToInclude, HighestDateToInclude: TDateTime;
                                         LocationInfo: TLocationInfo;
                                         MinDistanceInFeet: double = 0.0;
                                         MinTimeInSeconds: double = 0.0);
var
  DOSErr: integer;
  FileName: string;
  WayPointCount1, WayPointCount2: integer;
  cp, n: integer;
  SubpathFilter, AllFilesFilter, FileMask, SubFolder: string;
  SelectedSubFoldersList: TStringList;
  NrPointsDeleted: integer;
  SelectedSubFolders: string;

//*****************************************************************************
//   Function Name     :
//   Useage            :
//   Function Purpose  :
//   Assumptions       : assume parameter is in the form "2017-04-02"
//   Parameters        :
//   Return Value      :
//*******************************************************************************}

  function GarminDate2DateTime(const YYYY_MM_DD: string): TDateTime;
  var
    YYYY, MM, DD: word;
  begin { GarminDate2DateTime }
    try
      if MatchesPattern(YYYY_MM_DD, '9999-99-99?????????') then
        begin
          YYYY   := StrToInt(Copy(YYYY_MM_DD, 1, 4));
          MM     := StrToInt(Copy(YYYY_MM_DD, 6, 2));
          DD     := StrToInt(Copy(YYYY_MM_DD, 9, 2));
          result := EncodeDate(YYYY, MM, DD);
        end else
      if MatchesPattern(YYYY_MM_DD, 'Track_9999-99-99???????') then
        begin
          YYYY   := StrToInt(Copy(YYYY_MM_DD, 7, 4));
          MM     := StrToInt(Copy(YYYY_MM_DD, 12, 2));
          DD     := StrToInt(Copy(YYYY_MM_DD, 15, 2));
          result := EncodeDate(YYYY, MM, DD);
        end
      else
        result := BAD_DATE;
    except
      result := BAD_DATE;
    end;
  end;  { GarminDate2DateTime }

  function OkDate(FileName: string): boolean;
  var
    YYYYMMDD: string;
    DateFromFileName, FileDate: TDateTime;
  begin { OkDate }
    YYYYMMDD         := Copy(ExtractFileBase(FileName), 1, 8);
    DateFromFileName := PhotoDate2DateTime(YYYYMMDD, dd_NoDate);

    if DateFromFileName <> BAD_DATE then
      result := (DateFromFileName >= LowestDateToInclude) and (DateFromFileName <= HighestDateToInclude)
    else
      begin
        DateFromFileName := GarminDate2DateTime(ExtractFileBase(FileName));
        if DateFromFileName <> BAD_DATE then
          result := (DateFromFileName >= LowestDateToInclude) and
                    (DateFromFileName <= HighestDateToInclude)
        else
          begin // should probably actually open the file and scan for dates from the GPX track
            FileDate := FileDateToDateTime(FileAge(FileName));
            result   := (FileDate >= LowestDateToInclude) and (FileDate <= HighestDateToInclude);
          end;
      end;
  end;  { OkDate }

//*****************************************************************************
//   Function Name     : CountWayPoints
//   Useage            : CountWayPoints('c:\temp\*.gpx')
//   Function Purpose  : Count the number of waypoints in the filtered location
//   Assumptions       :
//   Parameters        : PathFilter = path to the folder containing the waypoints
//   Return Value      : Updates the global variables:
//                         WayPointCount, FileCount, FolderCount
//*******************************************************************************}

  procedure CountWayPoints(PathFilter: string);
  var
    SearchRec: TSearchRec;
    temp: string;
  begin { CountWayPoints }
    // count the total number of waypoints
    AllFilesFilter := ExtractFilePath(PathFilter) + '*.*';
    FileMask       := ExtractFileName(PathFilter);
    DosErr         := FindFirst(AllFilesFilter, faAnyFile, SearchRec);

    try
      while DosErr = 0 do
        begin
          if (SearchRec.Attr and faDirectory) <> 0 then  // this is a directory
            begin
              if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
                begin
//                if LocationInfo.ScanAllSubFolders then
//                  begin
//                    SubPathFilter := ExtractFilePath(PathFilter) + SearchRec.Name + '\' + ExtractFileName(PathFilter);
//                    CountWayPoints(SubPathFilter);
//                  end else
                  if LocationInfo.ScanSelectedSubFolders then
                    begin
                      if SelectedSubFoldersList.IndexOf(SearchRec.Name) >= 0 then
                        begin
                          SubPathFilter := ExtractFilePath(PathFilter) + SearchRec.Name + '\' + ExtractFileName(PathFilter);
                          CountWayPoints(SubPathFilter);
                        end;
                    end;
                end
            end
          else
            begin
              if Wild_Match(pchar(SearchRec.Name), pchar(FileMask), '*', '?', false) then
                begin
                  FileName         := ExtractFilePath(PathFilter) + SearchRec.Name;

                  if OkDate(FileName) then
                    begin
                      WayPointCount1  := WayPointCount1 + WaypointsInFile(FileName);
                      inc(FileCount1);
                      temp := Format('Counting GPX %s %d files, %d folders, %0.n points',
                                     [FileName, FileCount1, FolderCount1, WayPointCount1*1.0]);
                      UpdateStatus(temp, -2 { clYellow });
                    end;
                end;
            end;
          DosErr := FindNext(SearchRec);
        end;
      Inc(FolderCount1);
    finally
      FindClose(SearchRec);
    end;
  end;  { CountWayPoints }

  procedure LoadWayPoints(PathFilter: string; ShowStatus: boolean);
  var
    SearchRec: TSearchRec;
  begin { LoadWayPoints }
    // now load the data into memory
    AllFilesFilter := ExtractFilePath(PathFilter) + '*.*';
    FileMask       := ExtractFileName(PathFilter);
    DosErr         := FindFirst(AllFilesFilter, faAnyFile, SearchRec);
    if ShowStatus then
      UpdateStatus(Format('Loading waypoints in %s', [PathFilter]));
    try
      while (DosErr = 0) and (WayPointCount2 <= fTotalWayPoints) do
        begin
          if (SearchRec.Attr and faDirectory) <> 0 then
            begin
              if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
                begin
//                if LocationInfo.ScanAllSubFolders then
//                  begin
//                    SubPathFilter := ExtractFilePath(PathFilter) + SearchRec.Name + '\' + ExtractFileName(PathFilter);
//                    LoadWayPoints(SubPathFilter, true);
//                  end else
                  if LocationInfo.ScanSelectedSubFolders then
                    begin
                      if SelectedSubFoldersList.IndexOf(SearchRec.Name) >= 0 then
                        begin
                          SubPathFilter := ExtractFilePath(PathFilter) + SearchRec.Name + '\' + ExtractFileName(PathFilter);
                          LoadWayPoints(SubPathFilter, true);
                        end;
                    end;
                end
            end
          else
            begin
              if Wild_Match(pchar(SearchRec.Name), pchar(FileMask), '*', '?', false) then
                begin
                  FileName       := ExtractFilePath(PathFilter) + SearchRec.Name;

                  if OkDate(FileName) then
                    begin
                      if FileNames.Count > 0 then
                        UpdateStatus(Format('Loading GPX %s\%s: %d/%d (%6.0f%%)',
                                            [PathFilter, SearchRec.Name,
                                            FileCount2, FileCount1,
                                            (FileCount2 / FileCount1) * 100]), -2 {yellow});
                      try
                        n := LoadGPXFile(FileName, LowestDateToInclude, HighestDateToInclude, MinDistanceInFeet, MinTimeInSeconds);
                        Inc(FileCount2);
                        Inc(WayPointCount2, n);
                      except
                        on e:Exception do
                          raise Exception.CreateFmt('Error [%s] while loading file "%s"', [e.message, FileName]);
                      end;
                    end;
                end;
            end;
          DosErr := FindNext(SearchRec);
        end;
    finally
      UpdateStatus(Format('Loading GPX complete: %d/%d (%6.0f%%)',
                          [FileCount2, FileCount1,
                          (FileCount2 / FileCount1) * 100]), 2 { clBtnFace });
      FindClose(SearchRec);
    end;
  end;  { LoadWayPoints }

begin { TGPSTrackingInfo.LoadGPXFiles }
  SelectedSubFoldersList := nil;
  SelectedSubFolders     := LocationInfo.SelectedSubFolders;
  try
    if not Empty(PathFilter) then
      begin
        WayPointCount1  := 0;
        WayPointCount2  := 0;
        FileCount1      := 0;
        FileCount2      := 0;
        FolderCount1    := 0;
        FolderCount2    := 0;

        if LocationInfo.ScanSelectedSubFolders and (not Empty(SelectedSubFolders)) then
          begin
            SelectedSubFoldersList := TStringList.Create;
            SelectedSubFoldersList.Sorted := true;
            cp    := PosCH(';,', SelectedSubFolders);
            repeat
              if cp > 0 then
                begin
                  SubFolder            := Copy(SelectedSubFolders, 1, cp-1);
                  SelectedSubFoldersList.Add(SubFolder);
                  SelectedSubFolders   := Copy(SelectedSubFolders, cp+1, Length(SelectedSubFolders) - cp);
                  cp                   := PosCH(';,', SelectedSubFolders);
                end;
            until cp <= 0;
            if not Empty(SelectedSubFolders) then
              SelectedSubFoldersList.Add(SelectedSubFolders);
          end;

        CountWayPoints(PathFilter);

        if WayPointCount1 > 0 then
          begin
            fCurrentIndex   := fTotalWayPoints;

            fTotalWayPoints := fTotalWayPoints + WayPointCount1;

            UpdateStatus(Filename + #13#10 + Format('Scanning complete. %d/%d files, %d/%d folders, %d/%d points',
                                                    [FileCount2, FileCount1,
                                                     FolderCount2, FolderCount1,
                                                     WayPointCount2, WayPointCount1]), 2);

            SetLength(Points, fTotalWayPoints);  // resize the array to allow for additional data
            WayPointCount2   := 0;
            LoadWayPoints(PathFilter, true);

            fTotalWayPoints := fCurrentIndex;  // may have skipped some waypoints lacking a date
            SetLength(Points, fTotalWayPoints);
            SortWayPoints(NrPointsDeleted);
          end
        else
          AlertFmt('No data was found in the location: %s', [PathFilter]);

        UpdateStatus(Format('Loading complete. %0.n/%0.n points loaded. %0.n duplicates were removed', // %0.n total points deleted',
                     [fTotalWayPoints*1.0, WayPointCount1*1.0, NrPointsDeleted*1.0]), 2 { back to clBtnFace });

        fGPXFilesLoaded := true;
        fChanged        := true;
      end;
  finally
    SelectedSubFoldersList.Free;
  end;
end;  { TGPSTrackingInfo.LoadGPXFiles }

procedure TGPSTrackingInfo.SortWayPoints(var NrPointsDeleted: integer);
const
  ONE_HUNDRED_FEET = 0.00025; // Roughly at the equator
  EQUALPOINTSFILENAME = 'C:\TEMP\Equal Points.txt';
var
  Src, Dst: integer;
  CurrentItem: TWayPointInfo;
{$IfDef debugging}
  OutFile: textfile;
  temp: string;
  I: integer;
  NrEqPoints: integer;
  LastItem: TWayPointInfo;
{$EndIf}

  function Equal(Item1, Item2: TWayPointInfo): boolean; overload;
  begin
    result := (ApproxEqual(Item1.DateTime, Item2.DateTime, ONESECOND * 5.0) and   // within 5 seconds
               ApproxEqual(Item1.Lat,      Item2.Lat, ONE_HUNDRED_FEET) and       // within 100 feet
               ApproxEqual(Item1.Lon,      Item2.Lon, ONE_HUNDRED_FEET))          // within 100 feet
  end;

  function ProperlySorted(LowIndex, HighIndex: integer): boolean;
  var
    Src: integer;
    LastTime: TDateTime;
  begin { ProperlySorted }
    result := true;
    if Length(Points) >= 2 then
      begin
        LastTime := Points[LowIndex].DateTime;
        for src := LowIndex+1 to HighIndex do
          if Points[src].DateTime < LastTime then
            begin
              result := false;
              break  // exit noting not properly sorted
            end
          else
            LastTime := Points[src].DateTime;
      end;
  end;  { ProperlySorted }

begin { TGPSTrackingInfo.SortWayPoints }
  if not ProperlySorted(0, fTotalWayPoints-1) then
    begin
      DumpRange('c:\temp\Points Before Sort.txt', 'Points Before Sort', 0, fTotalWayPoints-1, false);

      // sort the points by date time

      QuickSort(0, fTotalWayPoints-1);

      // validate that the sort worked

      if not ProperlySorted(0, fTotalWayPoints-1) then
        raise Exception.Create('System error: data not properly sorted');

      UpdateStatus(Format('Sorting complete. %0.n points sorted', [fTotalWayPoints*1.0]), 1 { clBtnFace });
      DumpRange('c:\temp\Points After Sort.txt', 'Points After Sort', 0, fTotalWayPoints-1, false);
    end;

{$IfDef debugging}
  AssignFile(OutFile, EQUALPOINTSFILENAME);
  ReWrite(OutFile);
  WriteLn(OutFile, 'Selected points @ ', DateTimeToStr(Now)); //  ' for Lowestdate: ', DateTimeToStr(), ' to HighestDate: ', DateTimeToStr());
  NrEqPoints := 0; LastItem := Points[0];
  for i :=0 to fTotalWaypoints-1 do
    with points[i] do
      begin
        Write(OutFile,
               i:5, ': ', Lat:10:5, Lon:10:5, '   ', DateTimeToStr(DateTime):25);
        if i > 0 then
          if Equal(LastItem, Points[i]) then
            begin
              Write(OutFile, '  EQ ', NrEqPoints+1:3);
              Inc(NrEqPoints);
            end;
        WriteLn(OutFile);
        LastItem := Points[i];
      end;
  CloseFile(OutFile);
//  AlertFmt('%0.n pairs of Equal points were found out of %0.n total', [NrEqPoints*1.0, fTotalWaypoints*1.0]);
//temp := Format('Notepad.exe %s', [EQUALPOINTSFILENAME]);
//FileExecute(temp, false);
{$EndIf}

  // DELETE DUPLICATE WAYPOINTS

  Dst := 0;
  Src := 0;
  if fTotalWayPoints > 1 then
    begin
      if not ProperlySorted(0, fTotalWayPoints-1) then
        Alert('Not properly sorted before duplicate deletion');

      While Src < Length(Points) do
        begin
          CurrentItem   := Points[Src];
          Points[Dst]   := CurrentItem;
          Inc(Dst);

          repeat { to always increment src pointer at least once }
            inc(Src);
          until not Equal(Points[Src], CurrentItem) // (Points[Src] <> currentvalue)
             or (Src >= Length(Points));
        end;

      NrPointsDeleted := fTotalWayPoints-Dst;
      fTotalWayPoints := Dst;
      SetLength(Points, fTotalWayPoints);
      if not ProperlySorted(0, fTotalWaypoints-1) then
        Alert('Not properly sorted after duplicate deletion');

      DumpRange('c:\temp\After duplicate deletion.txt', 'After duplicate deletion', 0, fTotalWayPoints-1, false);
    end;
end;  { TGPSTrackingInfo.SortWayPoints }


//*****************************************************************************
//   Function Name     : TGPSTrackingInfo.LoadTrackData
//   Function Purpose  : Load track data for a file
//   Parameters        : Lfn = file path/name for file to load
//   Return Value      : Highest Date read
//*******************************************************************************}

function TGPSTrackingInfo.LoadTrackData( var Lfn: string;
                                             LowestDateToInclude, HighestDateToInclude: TDateTime): TDateTime;
{$IfDef debugging}
const
  DEBUGLfn = 'c:\temp\debugfile.txt';
{$EndIf}
var
  Line: string;
  FieldCount: integer;
  Delimited_Info: TDelimited_Info;
  Fields: TFieldArray;
  WayPointsList: textfile;
  i, WayPointCount: integer;
  LineNr, MaxLineNr: integer;
  Pass2PointCount: integer;
  DateTime, TheDate, TimePart: TDateTime;
  ValidTime: boolean;
  NrInvalidTime: integer;
  fLastTime: TDateTime;
  RecsSkipped: integer;
  NrPointsDeleted: integer;

{$IfDef debugging}
  DebugList: TextFile;
{$EndIf}

  procedure UpdateStats;
  begin { UpdateStats }
    UpdateStatus(Format('Loading/Reading record # %0.n/%0.n, Adding %0.n/%0.n,  [%s --> %s]. NrInvalidTime = %0.n',
                                                       [LineNr*1.0, MaxLineNr*1.0,
                                                        Pass2PointCount*1.0, WayPointCount*1.0,
                                                        DateToStr(LowestDateFound),
                                                        DateToStr(HighestDateFound),
                                                        NrInvalidTime*1.0]), -2 { clYellow });
  end;  { UpdateStats }

begin {  TGPSTrackingInfo.LoadTrackData }
  result := 0;
  if not fTrackDataLoaded then
    begin
      if Empty(Lfn) or (not FileExists(Lfn)) then
        if not BrowseForFile('Track data file', Lfn, '.csv') then
          exit;

      if not Empty(Lfn) then
        begin
          UpdateStatus(Format('Loading track data file %s', [Lfn]), -1 { clYellow });
          AssignFile(WaypointsList, Lfn);
          RecsSkipped := 0; NrInvalidTime := 0;
          Reset(WaypointsList);
          try
            // count the number of points
            WayPointCount := 0;
            Delimited_Info.Field_Seperator := ',';
            Delimited_Info.QuoteChar       := '"';
            fLastTime := Now;
            LineNr  := 0;
            LowestDateFound := MAXINT;
            HighestDateFound := -MAXINT;

            ReadLn(WayPointsList, Line);
            Parse_Delimited_Line(Line, Fields, FieldCount, Delimited_Info);
            if Fields[0] = 'Date' then  // skip header line
              begin
                ReadLn(WayPointsList, Line);
                inc(LineNr);
              end;

            while not Eof(WayPointsList) do
              begin
                Parse_Delimited_Line(Line, Fields, FieldCount, Delimited_Info);

                DateTime := StrToDateTime(Fields[0]);

                TheDate  := Trunc(DateTime);  // get rid of the "time" portion of the date
                TimePart := Frac(DateTime);
                ValidTime   := TimePart > 0.0;

                inc(LineNr);
                if (TheDate > 0) and ValidTime then
                  begin
                    if (TheDate >= LowestDateToInclude) and (TheDate <= HighestDateToInclude) then
                      inc(WayPointCount);
                  end;
                if ((LineNr mod 1000) = 0) or (MillisecondsBetween(Now, fLastTime) > 1000) then
                  begin
                    UpdateStatus(Format('Counting record # %0.n/%0.n.',
                                        [WayPointCount*1.0, LineNr*1.0]), -2 { clYellow });
                    fLastTime := Now;
                  end;
                ReadLn(WayPointsList, Line);
              end;

            // now load the data
            fTotalWayPoints                := fTotalWayPoints + WayPointCount;
            SetLength(Points, fTotalWaypoints);

            Reset(WayPointsList);
            MaxLineNr           := LineNr;
            LineNr              := 0;
            Pass2PointCount     := 0;

            ReadLn(WayPointsList, Line);
            Inc(LineNr);
            Parse_Delimited_Line(Line, Fields, FieldCount, Delimited_Info);
            if Fields[0] = 'Date' then  // header line- ignore it
              begin
                ReadLn(WayPointsList, Line);
                Inc(LineNr);
              end;

{$IfDef debugging}
            AssignFile(DebugList, DEBUGLfn);
            ReWrite(DebugList);
{$EndIf}
            RecsSkipped := 0; NrInvalidTime := 0;
            while not Eof(WayPointsList) do
              begin
                Parse_Delimited_Line(Line, Fields, FieldCount, Delimited_Info);
                DateTime := StrToDateTime(Fields[0]);
                TheDate  := Trunc(DateTime);
                TimePart := Frac(DateTime);
                ValidTime   := TimePart > 0.0;
                if not ValidTime then
                  inc(NrInvalidTime);
                if (TheDate > 0) and
                   ValidTime { If it doesn't have a time part, it is useless.
                                    Note: this could skip events happening at midnght. } then
                  begin
                    if (TheDate >= LowestDateToInclude) and (TheDate <= HighestDateToInclude) then
                      begin
                        Inc(Pass2PointCount);
                        if TheDate < LowestDatefound then
                          LowestDateFound := TheDate;
                        if TheDate > HighestDateFound then
                          HighestDateFound := TheDate;

{$IfDef MemCheck}
                        if (fCurrentIndex < 0) or (fCurrentIndex >= Length(Points)) then
                          Raise Exception.CreateFmt('Attempted access beyond length of array at index = %d', [fCurrentIndex]);
{$EndIf}
                        Points[fCurrentIndex].Lat := StrToFloat(Fields[1]);
                        Points[fCurrentIndex].Lon := StrToFloat(Fields[2]);
                        Points[fCurrentIndex].ID  := StrToInt(Fields[3]);
                        Points[fCurrentIndex].DateTime := DateTime;
{$IfDef debugging}
                        WriteLn(DebugList,  fCurrentIndex:6, ': ', Fields[0]:25);
{$EndIf}
                        inc(fCurrentIndex);
                        if ((LineNr mod 1000) = 0) or (MillisecondsBetween(Now, fLastTime) > 1000) then
                          begin
                            UpdateStats;
                            fLastTime := Now;
                          end;
                      end
                    else
                      begin
                        Inc(RecsSkipped);
                        if ((LineNr mod 1000) = 0) or (MillisecondsBetween(Now, fLastTime) > 1000) then
                          begin
                            UpdateStats;
                            fLastTime := Now;
                          end;
                      end;
                  end;
                ReadLn(WayPointsList, Line);
                Inc(LineNr);
              end;
            Assert(Pass2PointCount = WayPointCount, 'System error in LoadTrackData. Point Count mismatch');
            fTotalWayPoints := fCurrentIndex;
            SetLength(Points, fTotalWayPoints);
            SortWayPoints(NrPointsDeleted);
            UpdateStatus(Format('%s --> %s (%0.n points deleted)',
                   [DateTimeToStr(LowestDateFound), DateTimeToStr(HighestDateFound), NrPointsDeleted*1.0]), 1 { clBtnFace });
          finally
            CloseFile(WaypointsList);
            UpdateStatus(Format('Track data file %s loaded [%0.n points, %0.n skipped]',
                         [Lfn, fTotalWayPoints*1.0, RecsSkipped*1.0]), 1 { clBtnFace });
{$IfDef debugging}
            CloseFile(DebugList);
            MessageFmt('%0.n lines written to %s', [fCurrentIndex*1.0, DEBUGLfn]);
{$EndIf}
            result := HighestDateFound;
            fTrackDataLoaded := true;
          end;
        end
    end
  else
    begin
      result := 0;
      WayPointCount := Length(Points);
      for i := 0 to WayPointCount-1 do
        begin
          if Points[i].DateTime > result then
            begin
              result := Points[i].DateTime;
              HighestDateFound := result;
            end;
        end;
    end;
end;  {  TGPSTrackingInfo.LoadTrackData }

function TGPSTrackingInfo.SaveTrackData(Lfn: string): boolean;
var
  WayPointsList: TextFile;
  i: integer;
  DateStr: string;
  mr: integer;
begin
  result := false;
  if not Empty(Lfn) then
    begin
      if FileExists(Lfn) then
        mr := OverwriteItOrAppend(Format('The tracking data file [%s] already exists. Overwrite it? ', [Lfn]))
      else
        mr := ooaOverwrite;

      if mr in [ooaOverwrite, ooaAppend] then
        begin
          AssignFile(WaypointsList, Lfn);
          if mr = ooaOverwrite then
            begin
              ReWrite(WaypointsList);
              WriteLn(WayPointsList, 'Date, Lat, Lon, Track ID');
            end
          else
            Append(WaypointsList);

          try
            for i := 0 to fTotalWayPoints-1 do
              with Points[i] do
                begin
                  if DateTime > 0 then
                    begin
                      DateStr := DateTimeToStr(DateTime);
                      WriteLn(WaypointsList, DateStr, ',', Lat:10:7, ',', Lon:10:7, ',', ID);
                    end;
                end;

          finally
            CloseFile(WaypointsList);
            UpdateStatus(Format('%0.n points saved to Track Data file: %s', [fTotalWayPoints*1.0, Lfn]), 1 { clBtnFace });
            result := true;
            fChanged := false;
          end;
        end;
    end;
end;

{$IfDef dhd}
function TGPSTrackingInfo.FileNameFromID(anID: integer): string;
begin
  if TracksTable.Locate(cID, anID, []) then
    result := TracksTable.fldFileName.AsString
  else
    result := '';
end;
{$EndIf}


procedure TGPSTrackingInfo.UpdateStatus(const Msg: string; LineNo: integer = 1);
begin
  if Assigned(fOnUpdateStatus) then
    fOnUpdateStatus(Msg, LineNo);
end;

function TGPSTrackingInfo.FindNearestPointInTracks(
           Latitude, Longitude: double;
           var ShortestDistanceInFeet: double;
           var FileName: string): TWayPointInfo;
const
  NO_WAY_POINT: TWayPointInfo = (Lat:-1; Lon: -1);
var
  i, IndexOfShortest: integer;
  aDistance: double;
begin
  ShortestDistanceInFeet := MAXINT;
  IndexOfShortest  := -1;
  if fTotalWayPoints > 0 then
    begin
      for i := 0 to fTotalWayPoints - 1 do
        begin
          aDistance := Distance(Latitude, Longitude, Points[i].Lat, Points[i].Lon, muFeet);
          if aDistance < ShortestDistanceInFeet then
            begin
              ShortestDistanceInFeet := aDistance;
              IndexOfShortest  := i;
            end;
        end;
      if IndexOfShortest >= 0 then
        begin
          result   := Points[IndexOfShortest];
{$IfDef dhd}
          FileName := FileNameFromID(result.ID);
{$Else}
          FileName := 'No tracks table';          
{$EndIf}
        end;
    end
  else
    result := NO_WAY_POINT;
end;

function TGPSTrackingInfo.FindClosestMatchingDateInTracks(ExifDate: TDateTime;
                                              var FoundMatch: boolean;
                                              MinutesToAdd: single): TWayPointInfo;
var
  mode: TSearch_Type; // SEARCHING, SEARCH_FOUND, NOT_FOUND
  t, b, m: integer;
  IndexOfSmallest: integer;
  Difference, SmallestAbsDifference, AbsDifference, Time_Correction: double;
begin { FindClosestMatchingDateInTracks }
  // do a binary search
  foundmatch := false;
  SmallestAbsDifference := MAXINT;
  IndexOfSmallest       := -1; // force exception if nothing found and this is used
  t := 0;
  m := t;
  b := fTotalWayPoints - 1;
  mode := SEARCHING;
  Time_Correction := MinutesToAdd * ONE_MINUTE;  // Convert MinutesToAdd to fraction of a day
  repeat
    if t <= b then
      begin
        m := (t + b) div 2;
        Difference    := ExifDate + Time_Correction - Points[m].DateTime;
        AbsDifference := Abs(Difference);
        if AbsDifference < SmallestAbsDifference then
          begin
            SmallestAbsDifference := AbsDifference;
            IndexOfSmallest       := m;
          end;
        if AbsDifference = 0 then // this will probably never happen
          mode := SEARCH_FOUND else
        if Difference > 0 then // below us
          t := m + 1 else
        if Difference < 0 then // above us
          b := m - 1
      end
    else
      mode := NOT_FOUND;
  until mode <> SEARCHING;

  if mode = SEARCH_FOUND then // unlikely but appears to be an exact match
    begin
      FoundMatch := true;
      result := Points[m];
    end
  else
    begin
      FoundMatch := SmallestAbsDifference <= MAX_TIME_DIFFERENCE;
      if FoundMatch then  // close enough
        result := Points[IndexOfSmallest];
    end;
end;  { FindClosestMatchingDateInTracks }

destructor TGPSTrackingInfo.Destroy;
begin
  SetLength(Points, 0);
  FreeAndNil(fGPXdata);
  FreeAndNil(fGPXf);
{$IfDef dhd}
  FreeAndNil(fTracksTable);
{$EndIf}  
  FreeAndNil(fFileNames);

  fChanged := false;  // superflous

  inherited;
end;

constructor TGPSTrackingInfo.Create( const aGPXPathFilter: string;
                                     const aSavedTrackDataLfn: string);
begin
  fGPXPathFilter     := aGPXPathFilter;
  fSavedTrackDataLfn := aSavedTrackDataLfn;
  fTotalWayPoints    := 0;
  fCurrentIndex      := 0;
  LowestDateFound    := MAXINT;
  HighestDateFound   := - MAXINT;
  fGPXdata           := TGPX.Create;
end;

procedure TGPSTrackingInfo.ClearTrackData;
begin
  SetLength(Points, 0);
  fTotalWayPoints    := 0;
  fCurrentIndex      := 0;
  LowestDateFound    := MAXINT;
  HighestDateFound   := - MAXINT;
end;

procedure TGPSTrackingInfo.LoadSelectedTrackData( LocationInfo: TLocationInfo);
var
  NrPointsDeleted: integer;
begin
  if (not (fTrackDataLoaded or fGPXFilesLoaded)) then
    begin
      if LocationInfo.UseGPXLogsForLocation and LocationInfo.UseSavedTracksLogFile then
        begin
          ClearTrackData;
          LoadTrackData(fSavedTrackDataLfn, LocationInfo.LowestDateToScan, Now);

          // assume that everything older than 30 days has already been loaded - 9/9/2019 - BAD ASSUMPTION!

//          if HighestDateFound > 30 then
//            LowestDateToInclude := HighestDateFound - 30;

          LoadGPXFiles( fGPXPathFilter,
                        LocationInfo.LowestDateToScan,
                        Now,
                        LocationInfo);   // only loading those added AFTER those already included in the track data
        end else
      if LocationInfo.UseGPXLogsForLocation then
        LoadGPXFiles(fGPXPathFilter, LocationInfo.LowestDateToScan, Now, LocationInfo)
      else
      if LocationInfo.UseSavedTracksLogFile then
        LoadTrackData(fSavedTrackDataLfn, LocationInfo.LowestDateToScan, Now);

      SortWayPoints(NrPointsDeleted);
      UpdateStatus(Format('%s --> %s (%0.n points deleted)',
                   [DateTimeToStr(LowestDateFound), DateTimeToStr(HighestDateFound), NrPointsDeleted*1.0]), 1 { clBtnFace });
    end;
end;

function TGPSTrackingInfo.GetLatitudeLongitudeFromGPXLogs( var Latitude, Longitude: double;
                                                           UpdateInfo: TUpdateInfo;
                                                           LocationInfo: TLocationInfo): boolean;
var
  WayPointInfo: TWayPointInfo;
begin { GetLatitudeLongitudeFromGPXLogs }
  LoadSelectedTrackData( LocationInfo);

  WayPointInfo := FindClosestMatchingDateInTracks( UpdateInfo.PhotoDateTime,
                                                   result,
                                                   UpdateInfo.MinutesToAdd);
  if result then
    begin
      Latitude := WayPointInfo.Lat;
      Longitude := WayPointInfo.Lon;
    end;
end;  { GetLatitudeLongitudeFromGPXLogs }

function TGPSTrackingInfo.HighestDate: TDateTime;
var
  i: integer;
begin
  result := - MAXINT;
  for i := 0 to CountOfPoints-1 do
    if Points[i].DateTime > result then
      result := Points[i].DateTime;
end;

function TGPSTrackingInfo.LowestDate: TDateTime;
var
  i: integer;
begin
  result := MAXINT;
  for i := 0 to CountOfPoints-1 do
    if Points[i].DateTime < result then
      result := Points[i].DateTime;
end;

function TGPSTrackingInfo.GetFarthestPointFrom(StartLatitude, StartLongitude: double;
                                               var FarthestLatitude, FarthestLongitude: double;
                                               Units: TMeasurementUnits): double;
var
  i: integer;
  aDistance: double;
begin
  result := 0;
  for i := 0 to CountOfPoints-1 do
    begin
      aDistance := Distance(StartLatitude, StartLongitude, Points[i].Lat, Points[i].Lon, Units);
      if aDistance > result then
        result := aDistance;
    end;
end;

function TGPSTrackingInfo.CountOfPoints: integer;
begin
  result := Length(Points);
end;

function TGPSTrackingInfo.EndLatitude: double;
begin
  if CountOfPoints > 0 then
    result := Points[Pred(CountOfPoints)].Lat
  else
    result := 0;
end;

function TGPSTrackingInfo.EndLongitude: double;
begin
  if CountOfPoints > 0 then
    result := Points[Pred(CountOfPoints)].Lon
  else
    result := 0;
end;

function TGPSTrackingInfo.StartLatitude: double;
begin
  if CountOfPoints > 0 then
    result := Points[0].Lat
  else
    result := 0;
end;

function TGPSTrackingInfo.StartLongitude: double;
begin
  if CountOfPoints > 0 then
    result := Points[0].Lon
  else
    result := 0;
end;

function TGPSTrackingInfo.GetGPXf: TGPXFIle;
begin
  if not Assigned(fGPXf) then
    begin
      fGPXf := TGPXFIle.Create;
      fGPXf.GPXd := fGPXdata       ;
    end;
  result := fGPXf;
end;

procedure TGPSTrackingInfo.SetGPXf(const Value: TGPXFIle);
begin

end;

procedure TGPSTrackingInfo.ResetTrackingInfo;
begin
  FreeAndNil(fGPXf);
  CurrentIndex     := 0;  // reset the index pointer
  TotalWayPoints   := 0;
  SetLength(Points, 0);
  fGPXFilesLoaded  := false;
  fTrackDataLoaded := false;
  fChanged         := false;
  UpdateStatus('');
end;

procedure TGPSTrackingInfo.SetSavedTrackDataLfn(const Value: string);
begin
  fSavedTrackDataLfn := Value;
end;

procedure TGPSTrackingInfo.SetOnUpdateStatus(
  const Value: TUpdateStatusProc);
begin
  fOnUpdateStatus := Value;
end;

{$IfDef dhd}
function TGPSTrackingInfo.TracksTable: TTracksTable;
begin
  if not Assigned(fTracksTable) then
    begin
      fTracksTable := TTracksTable.Create(nil, CommonPhotoSettings.HikingDBDatabaseFileName, cTRACKS, []);
      fTracksTable.Active := true;
    end;
  result := fTracksTable;
end;
{$EndIf}

function TGPSTrackingInfo.FileNames: TStringList;
begin
  if not Assigned(fFileNames) then
    begin
      fFileNames := TStringList.Create;
      fFileNames.Sorted := true;
    end;
  result := fFileNames;
end;

procedure TGPSTrackingInfo.SetCurrentIndex(const Value: integer);
begin
  fCurrentIndex := Value;
end;

initialization
  FreeAndNil(gGPSTrackingInfo);
end.
