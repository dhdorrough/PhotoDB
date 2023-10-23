unit Hiking_Decl;

interface

uses Classes;

const
{$IfDef DHD}
  DEF_SAVEDTRACKSFILE  = 'F:\NDAS-I\Shared Programs\PhotoDB\PhotoDB\SavedTracks.csv';
  DEF_GPX_FILTER       = 'F:\NDAS-I\Documents\Delorme\DeLorme Docs\Transfer Files\*.gpx';
{$Else}
  DEF_SAVEDTRACKSFILE  = '';
  DEF_GPX_FILTER       = '';
{$EndIf}
type
  TLocation = record
    Latitude: double;
    Longitude: double;
  end;

  TGPSLogStats = record
    Title: string;
    GroupName: string;
    GroupNumber: integer;
    PointCount: integer;
    LowestDate: TDateTime;
    HighestDate: TDateTime;
    StartLocation: TLocation;
    EndLocation: TLocation;
    FarthestLocation: TLocation;
    TopLeft: TLocation;
    BottomRight: TLocation;
    FarthestDistance: double;
    TotalDistance: double;
    AverageLocation: TLocation;
    ItemsRolledUp: integer
  end;

  TOptionsSelectable = (os_Count, os_LowestDateTime, os_HighestDateTime,
                        os_NumberOfDates, os_Location, os_FarthestDistance,
                        os_BoundingRectangle, os_TotalDistance);

  TOptionsSelected = set of TOptionsSelectable;

  TAddLine      = procedure {name}(const Msg: string) of object;
  TClearOutput  = procedure {name} of object;
  TStatusUpdate = procedure {name}(const Msg: string; LineNo: integer = 0) of object;

  TGPXFileAnalyzer = class
  private
    fIndent: integer;
    fOnStatusUpdate: TStatusUpdate;
    fOnAddLine: TAddLine;
    fOnClearOutput: TClearOutput;
  public
    OptionsSelected: TOptionsSelected;
    procedure AddLine(Msg: string); virtual;
    procedure AnalyzeOne(FileNumber: integer; const FileName: string; var FileStats: TGPSLogStats);
    procedure CalcAverageLocation(var Stats: TGPSLogStats);
    procedure ClearOutput;
    procedure InitStats(var Stats: TGPSLogStats; PointCount: integer; GroupName: string; GroupNumber: integer);
    procedure RollUp(var InnerStats, OuterStats: TGPSLogStats);
    procedure StatusUpdate(const Msg: string); virtual;
    procedure WriteHeader(const Stats: TGPSLogStats; anIndent: integer);
    procedure WriteStats(const Stats: TGPSLogStats; anIndent: Integer; OptionsSelected: TOptionsSelected);

    property OnAddLine: TAddLine
             read fOnAddLine
             write fOnAddLine;
    property OnClearOutput: TClearOutput
             read fOnClearOutput
             write fOnClearOutput;
    property OnStatusUpdate: TStatusUpdate
             read fOnStatusUpdate
             write fOnStatusUpdate;
    constructor Create;
  end;

(*
var
  gDefaultGPXFilter          : string;
  gFolderNo                  : integer;
  gGPX_EditorPath            : string;
  gHikingMdbPathName         : string;
  gMapFolder                 : string;
  gPhotoDBDatabaseFileName   : string;
  gSavedTrackDataLfn         : string;
*)

implementation

uses
  SysUtils, GPX, GPXFile, LocationUtils;

procedure TGPXFileAnalyzer.AddLine(Msg: string);
begin
  if Assigned(fOnAddLine) then
    fOnAddLine(Msg);
end;

procedure TGPXFileAnalyzer.AnalyzeOne(FileNumber: integer; const FileName: string; var FileStats: TGPSLogStats);
var
  TrackSeg: TGPXTrkseg;
  Track: TGPXtrk;
  i, j, k: integer;
  GPXf: TGPXFile;
  SegmentStats, TrackStats: TGPSLogStats;
  DateTime: TDateTime;
  Year, Month, Day: word;
  CurrentLatitude, CurrentLongitude, PrevLatitude, PrevLongitude, DistanceToPrevious: double;
  CurrentDistance: double;
  GPXData: TGPX;
begin { TfrmHikeManager.AnalyzeOne }
  StatusUpdate('Processing: ' + FileName);
  if FileExists(FileName) then
    begin
      fIndent := 0;
      AddLine(FileName);
      GPXf := TGPXFile.Create;
      GPXData := TGPX.Create;
      try
        GPXf.FileName := FileName;
        GPXF.GPXd     := GPXData;
        GPXf.ImportGPXFile(FileName);
        InitStats(FileStats, 0, 'File', FileNumber+1);
        WriteHeader(FileStats, 2);
        for i := 0 to GPXf.GPXd.trk.Count-1 do
          begin
            Track := GPXf.GPXd.trk.Tracks[i];
            InitStats(TrackStats, 0, 'Track', i+1);
            WriteHeader(TrackStats, 4);
            for j := 0 to Track.Count-1 do
              begin
                TrackSeg := Track.trkseg[j];
                InitStats(SegmentStats, TrackSeg.Count, 'Segment', j+1);
                WriteHeader(SegmentStats, 6);

                PrevLatitude      := TrackSeg.trkpt[0].Latitude;
                PrevLongitude     := TrackSeg.trkpt[0].Longitude;

                for k := 0 to TrackSeg.Count - 1 do
                  begin
                    DateTime   := TrackSeg.trkpt[k].DateTime;
                    if DateTime <> 0 then
                     with SegmentStats do
                      begin
                        DecodeDate(DateTime, Year, Month, Day);
                        if DateTime < LowestDate then
                          LowestDate := DateTime;
                        if DateTime > HighestDate then
                          HighestDate := DateTime;

                        CurrentLatitude      := TrackSeg.trkpt[k].Latitude;
                        CurrentLongitude     := TrackSeg.trkpt[k].Longitude;

                        Inc(ItemsRolledUp);
                        with AverageLocation do
                          begin
                            Latitude  := Latitude + CurrentLatitude;
                            Longitude := Longitude + CurrentLongitude;
                          end;

                        with StartLocation do
                          if (Latitude = 0) and (Longitude = 0) then // only store the first
                            begin
                              Latitude  := CurrentLatitude;
                              Longitude := CurrentLongitude;
                            end;

                        with EndLocation do // keep only the last
                          begin
                            Latitude  := CurrentLatitude;
                            Longitude := CurrentLongitude;
                          end;

                        if k > 0 then
                          begin
                            DistanceToPrevious := Distance(PrevLatitude, PrevLongitude, CurrentLatitude, CurrentLongitude, muFeet);
                            TotalDistance      := TotalDistance + DistanceToPrevious;
                          end;
                          
                        PrevLatitude   := CurrentLatitude;
                        PrevLongitude  := CurrentLongitude;

                        with StartLocation do
                          if (Latitude <> 0) and (Longitude <> 0) and
                             (CurrentLatitude <> 0) and (CurrentLongitude <> 0) then
                            begin
                              CurrentDistance := Distance(Latitude, Longitude, CurrentLatitude, CurrentLongitude, muMiles);
                              if CurrentDistance > FarthestDistance then
                                begin
                                  FarthestDistance := CurrentDistance;
                                  with FarthestLocation do
                                    begin
                                      Latitude  := CurrentLatitude;
                                      Longitude := CurrentLongitude;
                                    end;
                                end;
                            end;

                        // Find top-left corner
                        if CurrentLatitude > TopLeft.Latitude then
                          TopLeft.Latitude := CurrentLatitude;
                        if CurrentLongitude < TopLeft.Longitude then
                          TopLeft.Longitude := CurrentLongitude;

                        // find bottom-right corner
                        if CurrentLatitude < BottomRight.Latitude then
                          BottomRight.Latitude := CurrentLatitude;
                        if CurrentLongitude > BottomRight.Longitude then
                          BottomRight.Longitude := CurrentLongitude;
                      end;
                  end;
                if SegmentStats.ItemsRolledUp > 0 then
                  begin
                    CalcAverageLocation(SegmentStats);
                    WriteStats(SegmentStats, 6, OptionsSelected);
                    RollUp(SegmentStats, TrackStats);
                  end;
              end;
            if TrackStats.ItemsRolledUp > 0 then
              begin
                CalcAverageLocation(TrackStats);
                WriteStats(TrackStats, 4, OptionsSelected);
                RollUp(TrackStats, FileStats);
              end;
          end;
        if FileStats.ItemsRolledUp > 0 then
          begin
            CalcAverageLocation(FileStats);
            WriteStats(FileStats, 2, OptionsSelected);
          end;
      finally
        GPXData.Free;
        GPXf.Free;
      end;
    end;
end;  { TfrmHikeManager.AnalyzeOne }

procedure TGPXFileAnalyzer.CalcAverageLocation(var Stats: TGPSLogStats);
begin
  with Stats, AverageLocation do
    if ItemsRolledUp <> 0 then
      begin
        Latitude  := Latitude  / ItemsRolledUp;
        Longitude := Longitude / ItemsRolledUp;
      end;
end;

procedure TGPXFileAnalyzer.ClearOutput;
begin
  if Assigned(fOnClearOutput) then
    fOnClearOutput;
end;

constructor TGPXFileAnalyzer.Create;
begin
  OptionsSelected := [Low(TOptionsSelectable)..High(TOptionsSelectable)];
end;

procedure TGPXFileAnalyzer.InitStats( var Stats: TGPSLogStats;
                         PointCount: integer;
                         GroupName: string;
                         GroupNumber: integer);
begin { InitStats }
  FillChar(Stats, SizeOf(Stats), 0);
  if GroupNumber > 0 then
    Stats.Title := Format('%s %d', [GroupName, GroupNumber])
  else
    Stats.Title := GroupName;
  Stats.GroupName   := GroupName;
  Stats.GroupNumber := GroupNumber;
  Stats.PointCount  := PointCount;
  Stats.LowestDate  := MAXINT;
  Stats.HighestDate := -MAXINT;
  Stats.FarthestDistance := -MAXINT;
  Stats.TotalDistance := 0;
  with Stats.TopLeft do
    begin
      Latitude  := -MAXINT;
      Longitude := MAXINT;
    end;
  with Stats.BottomRight do
    begin
      Latitude  := MAXINT;
      Longitude := -MAXINT;
    end;
end;  { InitStats }

procedure TGPXFileAnalyzer.RollUp(var InnerStats, OuterStats: TGPSLogStats);
begin { RollUp }
  OuterStats.PointCount := OuterStats.PointCount + InnerStats.PointCount;
  if InnerStats.LowestDate < OuterStats.LowestDate then
    OuterStats.LowestDate := InnerStats.LowestDate;
  if InnerStats.HighestDate > OuterStats.HighestDate then
    OuterStats.HighestDate := InnerStats.HighestDate;

  if InnerStats.TopLeft.Latitude > OuterStats.TopLeft.Latitude then
    OuterStats.TopLeft.Latitude := InnerStats.TopLeft.Latitude;
  if InnerStats.TopLeft.Longitude < OuterStats.TopLeft.Longitude then
    OuterStats.TopLeft.Longitude := InnerStats.TopLeft.Longitude;
  if InnerStats.BottomRight.Latitude < OuterStats.BottomRight.Latitude then
    OuterStats.BottomRight.Latitude := InnerStats.BottomRight.Latitude;
  if InnerStats.BottomRight.Longitude > OuterStats.BottomRight.Longitude then
    OuterStats.BottomRight.Longitude := InnerStats.BottomRight.Longitude;

  OuterStats.TotalDistance := OuterStats.TotalDistance + InnerStats.TotalDistance;

  OuterStats.ItemsRolledUp := OuterStats.ItemsRolledUp + 1;
  OuterStats.AverageLocation.Latitude  := OuterStats.AverageLocation.Latitude  + InnerStats.AverageLocation.Latitude;
  OuterStats.AverageLocation.Longitude := OuterStats.AverageLocation.Longitude + InnerStats.AverageLocation.Longitude;
  if (OuterStats.StartLocation.Latitude = 0) and (OuterStats.StartLocation.Longitude = 0) then
    OuterStats.StartLocation := InnerStats.StartLocation;
  OuterStats.EndLocation := InnerStats.EndLocation;
  if InnerStats.FarthestDistance > OuterStats.FarthestDistance then
    begin
      OuterStats.FarthestDistance := InnerStats.FarthestDistance;
      OuterStats.FarthestLocation := InnerStats.FarthestLocation;
    end;
end;  { RollUp }

procedure TGPXFileAnalyzer.StatusUpdate(const Msg: string);
begin
  if Assigned(fOnStatusUpdate) then
    fOnStatusUpdate(Msg);
end;

procedure TGPXFileAnalyzer.WriteHeader(const Stats: TGPSLogStats;
  anIndent: integer);
begin
  fIndent := anIndent;
  with Stats do
    AddLine(Format('%s', [Title]));
end;

procedure TGPXFileAnalyzer.WriteStats(const Stats: TGPSLogStats; anIndent: Integer; OptionsSelected: TOptionsSelected);
var
  NumberOfDates: integer;
begin { WriteStats }
  fIndent := anIndent;
  with Stats do
    begin
      try
        NumberOfDates := Trunc(HighestDate) - Trunc(LowestDate) + 1;
      except
        NumberOfDates := -1;
      end;
// os_Count, os_LowestDateTime, os_HighestDateTime, os_NumberOfDates, os_Location, os_FarthestDistance, os_BoundingRectangle
      if NumberOfDates > 0 then
        if GroupNumber > 0 then
          begin
            if os_Count in OptionsSelected then
              AddLine(Format('  %-10s %3d Points: %d',              [GroupName, GroupNumber, PointCount]));
            if os_LowestDateTime in OptionsSelected then
              AddLine(Format('  %-10s %3d Lowest Date/Time:  %s',   [GroupName, GroupNumber, DateTimeToStr(LowestDate)]));
            if os_HighestDateTime in OptionsSelected then
              AddLine(Format('  %-10s %3d Highest Date/Time: %s',   [GroupName, GroupNumber, DateTimeToStr(HighestDate)]));
            if os_NumberOfDates in OptionsSelected then
              AddLine(Format('  %-10s %3d Number of Dates: %d',     [GroupName, GroupNumber, NumberOfDates]));
            if os_Location in OptionsSelected then
              AddLine(Format('  %-10s %3d Location: (%10.6f %10.6f)', [GroupName, GroupNumber, AverageLocation.Latitude, AverageLocation.Longitude]));
            if os_FarthestDistance in OptionsSelected then
              AddLine(Format('  %-10s %3d Farthest Distance: %6.1f miles', [GroupName, GroupNumber, FarthestDistance]));
            if os_BoundingRectangle in OptionsSelected then
              AddLine(Format('  %-10s %3d Bounding Rectangle: (%10.6f %10.6f) x (%10.6f %10.6f)', [GroupName, GroupNumber,
                                                                                              TopLeft.Latitude, TopLeft.Longitude,
                                                                                              BottomRight.Latitude, BottomRight.Longitude]));
            if os_TotalDistance in OptionsSelected then
              Addline(Format('  %-10s %3d Total Distance:    %6.1f miles',    [GroupName, GroupNumber, TotalDistance / 5280.0]));
          end
        else
          begin
            if os_Count in OptionsSelected then
              AddLine(Format('  %-10s     Points: %d',              [GroupName, PointCount]));
            if os_LowestDateTime in OptionsSelected then
              AddLine(Format('  %-10s     Lowest Date:  %s',        [GroupName, DateToStr(LowestDate)]));
            if os_HighestDateTime in OptionsSelected then
              AddLine(Format('  %-10s     Highest Date: %s',        [GroupName, DateToStr(HighestDate)]));
            if os_NumberOfDates in OptionsSelected then
              AddLine(Format('  %-10s     Number of Dates: %d',     [GroupName, NumberOfDates]));
            if os_Location in OptionsSelected then
              AddLine(Format('  %-10s     Location: (%10.6f %10.6f)', [GroupName, AverageLocation.Latitude, AverageLocation.Longitude]));
            if os_FarthestDistance in OptionsSelected then
              AddLine(Format('  %-10s     Farthest Distance: %6.1f miles', [GroupName, FarthestDistance]));
            if os_BoundingRectangle in OptionsSelected then
              AddLine(Format('  %-10s     Bounding Rectangle: (%10.6f %10.6f) x (%10.6f %10.6f)', [GroupName,
                                                                                           TopLeft.Latitude, TopLeft.Longitude,
                                                                                           BottomRight.Latitude, BottomRight.Longitude]));
            if os_TotalDistance in OptionsSelected then
              Addline(Format('  %-10s     Total Distance:    %6.1f miles',    [GroupName, TotalDistance / 5280]));
          end;
    end;
end;  { WriteStats }

end.
