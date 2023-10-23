unit GPS;

// GPS.pas is used in some other coordinate-related projects, so any changes
// made here should be carefully tested and checked for backwards compatibility

interface

uses
  Fastcode, Classes, SysUtils, Math, gnugettext;

type
  TTimeZoneOffset = (tzo_GMT, tzo_EST, tzo_EDT, tzo_CST, tzo_MST, tzo_PST, tzo_IS,
                     tzo_ROME); // DHD

  // Base class for storing 3D + Time data
  // Level is used for temporary value storing,
  // don't trust it's content if you didn't put it yourself
  TPoint4D2 = Class
    private
      function GetLatitudeStr() : String;
      function GetLongitudeStr() : String;
      function GetLatitudeOZStr() : String;
      function GetLongitudeOZStr() : String;
      function GetLatitudeSimpleStr() : String;
      function GetLongitudeSimpleStr() : String;
      function GetAltitudeStr() : String;
      function GetDateTimeStr() : String;
      function GetCoordStr() : String;
      function GetLatLonGmapStr() : String;
    public
      Latitude : Double;    // WGS84 Degrees
      Longitude : Double;   // WGS84 Degrees
      Altitude : Double;    // WGS84 meter
      DateTime : TDateTime; // Zulu time
      Level : Byte;
      enabled : Boolean;
      property LatitudeStr : String read GetLatitudeStr;
      property LongitudeStr : String read GetLongitudeStr;
      property LatitudeOZStr : String read GetLatitudeOZStr;
      property LongitudeOZStr : String read GetLongitudeOZStr;
      property Lat : String read GetLatitudeSimpleStr;
      property Lon : String read GetLongitudeSimpleStr;
      property LatLonGMapStr : String read GetLatLonGMapStr;
      property AltitudeStr : String read GetAltitudeStr;
      property DateTimeStr : String read GetDateTimeStr;
      property Coord : String read GetCoordStr;
      procedure CopyFrom(Point4D : TPoint4D2);
      function WGS84Distance(lon,lat : Double; Almostnul : boolean = false) : double; overload;
      function WGS84Distance(Point4D : TPoint4D2; Almostnul : boolean = false) : double; overload;
      function IsInBox(E,O,S,N : double) : Boolean;
{$IfDef DHD}
      function GetDateTimeStrO(tzo: TTimeZoneOffset = tzo_GMT) : String;
{$EndIf}
      constructor Create;
  end;

  // Bounding box class, used in pointlist
  TBBox = Class(TObject)
    N,S,E,O : Double;
    Rm, Rx, Ry : Double;
    Complete : Boolean;
    procedure Clear;
    procedure Copy(BBox : TBBox);
    procedure Add(BBox : TBBox);
    function GetCenter() : TPoint4D2;
    constructor Create;
    destructor Destroy; override;
  end;

  // List of TPoint4D
  // Filled property should not be used anymore, only left here for compatibility purpooses
  TPointList2 = Class(TList)
    private
      penabled  : Boolean;
      function GetPoint(Index: Integer) : TPoint4D2;
      procedure SetPoint(Index: Integer; Point : TPoint4D2);
      procedure SetEnabled(Value : Boolean);
    public
      BBox : TBBox;
      filled : Boolean;
      property Points [Index: Integer]: TPoint4D2 read GetPoint write SetPoint;
      property Enabled : Boolean read penabled write SetEnabled;
      Function FindClosestIndex(lon,lat : Double) : integer;
      Function FindClosestPoint(lon,lat : Double) : TPoint4D2;
      Function FindFurtherPoint(lon,lat : Double) : Tpoint4D2;
      function GetOldestDateTime() : TDateTime;
      function GetNewestDateTime() : TDateTime;
      Procedure SortByDate;
      Constructor Create;
      Procedure ClearPointList;
      Procedure FreeAndDelete(Index : integer);
      Destructor Destroy; override;
      procedure CalcBBox(Reset : Boolean = True);
      procedure CalcBBoxFromTo(pFrom,pTo : integer;Reset : Boolean = True);
      procedure CopyBBoxFrom(BBox : TBBox); overload;
      function GetSpeedOf(point : TPoint4D2) : Double;
{$IfDef DHD}
      function GetDirectionOf(point: TPoint4D2): string;
{$EndIf}
  end;

  // A TPointList2 with a name
  TTrack2 = Class(TPointList2)
    Name : String;
    procedure CopyFrom(Track : TTrack2);
    procedure CopyMeta(Track : TTrack2);
    procedure CopyBBoxFrom(Track : TTrack2); overload;
  end;

  // A list of TTrack2
  TTrackList2 = Class(TList)
    private
      function GetTrack(Index: Integer) : TTrack2;
      procedure SetTrack(Index: Integer; Track : TTrack2);
    public
      Name : String;
      Procedure SortByDate;
      property Tracks [Index: Integer]: TTrack2 read GetTrack write SetTrack;
      function FindClosest(lo,la : double) : TTrack2;
      constructor Create;
      destructor Destroy; override;
      Procedure ClearTracks(FreeTracks : Boolean = True);
      procedure FreeAndDelete(Index : integer);
      function ResetBBoxes : Boolean;
  end;

  TTimeZoneInfo = record
                    Abbrev: string;
                    Desc:   string;
                  end;

  // faster than delphi Max & Min
  function MaxFP(const A, B : Double) : Double; //taken from FastCode Challenge
  function MinFP(const A, B : Double) : Double; //taken from FastCode Challenge

const
  // Used to set locales for StrToFloat and such function
  // we use '.' as decimal separator in strings even in language where it
  // isn't the default, so it's the same as in the GPX file (and much easier to
  // work with
  SORT_DEFAULT                         = $0;     { sorting default }
  SUBLANG_SYS_DEFAULT                  = $02;    { system default }
  LANG_NEUTRAL                         = $00;
  LANG_SYSTEM_DEFAULT   = (SUBLANG_SYS_DEFAULT shl 10) or LANG_NEUTRAL;
  LOCALE_SYSTEM_DEFAULT = (SORT_DEFAULT shl 16) or LANG_SYSTEM_DEFAULT;

  // Just a big number to compare to, the data is always less than this
  BIG_ONE = 2147483647;

  MAXDATETIME = 4579200000; //200*265*24*60*60 Year 2100, aproximaetely

var
  TimeZones: array [TTimeZoneOffset] of TTimeZoneInfo = (
              {tzo_GMT}     (Abbrev: 'GMT'; Desc: 'Greenwich Standard Time'),
              {tzo_EST}     (Abbrev: 'EST'; Desc: 'Eastern Standard Time'),
              {tzo_EDT}     (Abbrev: 'EDT'; Desc: 'Eastern Daylight Time'),
              {tzo_CST}     (Abbrev: 'CST'; Desc: 'Central Standard Time'),
              {tzo_MST}     (Abbrev: 'MST'; Desc: 'Mountain Standard Time'),
              {tzo_PST}     (Abbrev: 'PST'; Desc: 'Pacific Standard Time'),
              {tzo_IS}      (Abbrev: 'IS';  Desc: 'Iceland Time'),
              {tzo_ROME}    (Abbrev: 'ROME';Desc: 'Rome Time')
             );

{$IfDef dhd}
function TimeZoneFromAbbrev(Abbrev: string): TTimeZoneOffset;  // DHD?
{$EndIf}


implementation

{$IfDef dhd}
uses
  LocationUtils;

const
  cNO_ANGLE=-999;
  NR_OCTANTS = 16;
  ONEOCTANT  = 360 / NR_OCTANTS;  // N, NNE, NE, ENE, etc.
  HALFOCTANT = ONEOCTANT / 2;
  ONE_HOUR   = 1 / 24;

type
  OctantInfo = record
                 sa: double;
                 desc: string;
               end;

var
  Octants: array[0..15] of OctantInfo = (
                                     (sa:0.0;          desc:'N'),
                                     (sa:OneOctant;    desc:'NNE'),
                                     (sa:2*OneOctant;  desc:'NE'),
                                     (sa:3*OneOctant;  desc:'ENE'),
                                     (sa:4*OneOctant;  desc:'E'),
                                     (sa:5*OneOctant;  desc:'ESE'),
                                     (sa:6*OneOctant;  desc:'SE'),
                                     (sa:7*OneOctant;  desc:'SSE'),
                                     (sa:8*OneOctant;  desc:'S'),
                                     (sa:9*OneOctant;  desc:'SSW'),
                                     (sa:10*OneOctant; desc:'SW'),
                                     (sa:11*OneOctant; desc:'WSW'),
                                     (sa:12*OneOctant; desc:'W'),
                                     (sa:13*OneOctant; desc:'WNW'),
                                     (sa:14*OneOctant; desc:'NW'),
                                     (sa:15*OneOctant; desc:'NNW')
                                      );
{$EndIf}                                      

function MaxFP(const A, B : Double) : Double;
asm
   fld     A
   fld     B
   fcomi   st(0), st(1)
   fcmovb  st(0), st(1)
   ffree   st(1)
end;

function MinFP(const A, B : Double) : Double;
asm
  fld     A
  fld     B
  fcomi   st(0), st(1)
  fcmovnb st(0), st(1)
  ffree   st(1)
end;

procedure TBBox.Clear;
begin
  Self.N := 0;
  Self.S := 0;
  Self.E := 0;
  Self.O := 0;
  Self.Rx := 0;
  Self.Ry := 0;
  Self.Complete := False;
end;

procedure TBBox.Copy(BBox : TBBox);
begin
  Self.N := BBox.N;
  Self.S := BBox.S;
  Self.E := BBox.E;
  Self.O := BBox.O;
  Self.Rx := BBox.Rx;
  Self.Ry := BBox.Ry;
  Self.Rm := BBox.Rm;
end;

procedure TBBox.Add(BBox : TBBox);
begin
  if not Self.Complete then
    Self.Copy(BBox)
  else
  begin
    Self.N := MaxFP(Self.N,BBox.N);
    Self.S := MinFP(Self.S,BBox.S);
    Self.E := MaxFP(Self.E,BBox.E);
    Self.O := MinFP(Self.O,BBox.O);
  end;
  if (Self.N <> 0) and (Self.S <> 0) and (Self.E <> 0) and (Self.O <> 0) then
    Self.Complete := True;
end;

constructor TBBox.Create;
begin
  inherited;
  Self.Clear;
end;

destructor TBBox.Destroy;
begin
  inherited;
end;

function TPointList2.GetPoint(Index: Integer) : TPoint4D2;
begin
  Result := TPoint4D2(Self.Items[Index]);
end;

procedure TPointList2.SetPoint(Index: Integer; Point : TPoint4D2);
begin
  Self.Points[Index].Free;
  Self.Items[Index] := Pointer(Point);
end;

function TTrackList2.GetTrack(Index: Integer) : TTrack2;
begin
  Result := TTrack2(Self.Items[Index]);
end;

procedure TTrackList2.SetTrack(Index: Integer; Track : TTrack2);
begin
  Self.Tracks[Index].Free;
  Self.Items[Index] := Pointer(Track);
end;

function TPoint4D2.GetCoordStr() : String;
begin
  Result := Self.LatitudeStr + ', ' + Self.LongitudeStr;
end;

procedure TPointList2.CalcBBox(Reset : Boolean = True);
begin
  CalcBBoxFromTo(0,self.Count-1,Reset);
end;

// go from pFrom points to pTo points and include them in the bounding box
procedure TPointList2.CalcBBoxFromTo(pFrom,pTo : integer;Reset : Boolean = True);
var
  N,S,E,O : Double;
  Point4D : TPoint4D2;
  i : integer;
begin
  Self.BBox.Complete := False;
  if Self.Count < 2 then
    Exit;

  //if not Self.Enabled then
//    Exit;

  if Reset then //or (Self.Count = 1)
  begin
    Point4D := Self.Points[pFrom];
    while (not Point4D.enabled) and (pFrom+1 < pTo) do
    begin
      pFrom := pFrom + 1;
      Point4D := Self.Points[pFrom];
    end;
    if not Point4D.Enabled then
      Exit;
    N := Point4D.Latitude;
    S := Point4D.Latitude;
    E := Point4D.Longitude;
    O := Point4D.Longitude;
  end
  else
  begin
    N := Self.BBox.N;
    S := Self.BBox.S;
    E := Self.BBox.E;
    O := Self.BBox.O;
  end;

  For i := pFrom to pTo do
  begin
    Point4D := Self.Points[i];
    if Point4D.enabled then
    begin
      N := MaxFP(Point4D.Latitude,N);
      S := MinFP(Point4D.Latitude,S);
      E := MaxFP(Point4D.Longitude,E);
      O := MinFP(Point4D.Longitude,O);
    end;
  end;

  Self.BBox.N := N;
  Self.BBox.S := S;
  Self.BBox.E := E;
  Self.BBox.O := O;
  Self.BBox.Complete := True;
end;

procedure Tpoint4D2.CopyFrom(Point4D : TPoint4D2);
begin
  Self.Latitude := Point4D.Latitude;
  Self.Longitude := Point4D.Longitude;
  Self.Altitude := Point4D.Altitude;
  Self.DateTime := Point4D.DateTime;
end;

// distance in meters onto the WGS84 geoid between two Coordinates
// some data are exageretely precise (way beyond GPS's capacity) so I
// added the AlmostNul switch, as arccos(1) returns an error when two points
// are too close (and therefore distance is 0)
function TPoint4D2.WGS84Distance(lon,lat : Double; Almostnul : boolean = false) : double;
var
  d,s,c : Double;
  lat1 : Double;
  lon1 : Double;
  lat2 : Double;
  lon2 : Double;
  almost1 : double;
Const
  R = 6378137; // Medium earth radius in meter
begin
  // d = R x arcos [ sin(lat1) x sin(lat2) + cos(lat1) x cos(lat2) x cos(lon2-lon1) ]
  lat1 := DegToRad(Self.Latitude);
  lon1 := DegToRad(Self.Longitude);
  lat2 := DegToRad(lat);
  lon2 := DegToRad(lon);
  if (lon1 = lon2) AND
     (lat1 = lat2) then
    d := 0
  else
  begin
    s := sin(lat1)*sin(lat2);
    c := cos(lat1)*cos(lat2)*cos(lon2-lon1);

    if Almostnul then
      almost1 := round((s+c)*100000000)/100000000
    else
      almost1 := (s+c);

    if almost1 <> 1 then
      d := R*arccos(s+c)
    else
      d := 0;
  end;
  WGS84Distance := d;
end;

function TPoint4D2.WGS84Distance(Point4D : TPoint4D2; Almostnul : boolean = false) : double;
begin
  Result := Self.WGS84Distance(Point4D.Longitude, Point4D.Latitude,Almostnul);
end;

Function TPointList2.FindClosestIndex(lon,lat : Double) : integer;
var
  i,c : integer;
  d,m : double;
  Point4D : TPoint4D2;
begin
  d := BIG_ONE;
  c := -1;
  for i := 0 to Self.Count-1 do
  begin
    Point4D := Self.Points[i];
    m := Point4D.WGS84Distance(lon,lat);
    if m < d then
    begin
      d := m;
      c := i;
    end;
  end;
  Result := c;
end;

Function TPointList2.FindClosestPoint(lon,lat : Double) : TPoint4D2;
begin
  Result := Self.Points[Self.FindClosestIndex(lon,lat)];
end;

function SortPOIByDate(Point1,Point2 : Pointer) : integer;
begin
  if TPoint4D2(Point1).DateTime > TPoint4D2(Point2).DateTime then
    SortPOIByDate := 1
  else if TPoint4D2(Point1).DateTime < TPoint4D2(Point2).DateTime then
    SortPOIByDate := -1
  else
    SortPOIByDate := 0;
end;

Procedure TPointList2.SortByDate;
begin
  Self.Sort(@SortPOIByDate);
end;

procedure TPointList2.CopyBBoxFrom(BBox : TBBox);
begin
  Self.BBox.Copy(BBox);
end;

procedure TTrack2.CopyBBoxFrom(Track : TTrack2);
begin
  Self.CopyBBoxFrom(Track.BBox);
end;

constructor TPoint4D2.Create;
begin
  inherited;
  Self.Level := 0;
  Self.enabled := True;
end;

constructor TPointList2.Create;
begin
  inherited;
  BBox := TBBox.Create;
end;

constructor TTrackList2.Create;
begin
  inherited;
end;

function TTrackList2.FindClosest(lo, la : double) : TTrack2;
var
  ClosestTrack,Track : TTrack2;
  ClosestDistance : double;
  Point4D : TPoint4D2;
  i,j : integer;
Const
  R = 6378137; // Medium earth radius in meter
begin
  ClosestTrack := Self.Tracks[0];
  ClosestDistance := r*2; //just to set the maximum

  for i := 0 to Self.Count-1 do
  begin
    Track := Self.Tracks[i];
    for j := 0 to Track.Count-1 do
    begin
      Point4D := Track.Points[j];
      if Point4D.WGS84Distance(lo,la) < ClosestDistance then
      begin
        ClosestDistance := Point4D.WGS84Distance(lo,la);
        ClosestTrack := Track;
      end;
    end;
  end;

  Result := ClosestTrack;
end;

procedure TPointList2.ClearPointList;
var
  i,c : integer;
begin
  c := Self.Count-1;
  if c >= 0 then
  begin
    for i := c downto 0 do
    begin
      Self.FreeAndDelete(i);
    end;
  end;
  Self.Clear;
end;

destructor TPointList2.Destroy;
begin
  Self.ClearPointList;
  Self.BBox.Free;
  inherited;
end;

procedure TTrackList2.ClearTracks;
var
  i,c : integer;
begin
  c := Self.Count;
  if c > 0 then
  begin
    for i := c-1 downto 0 do
    begin
      Self.FreeAndDelete(i);
    end;
  end;
  Self.Clear;
end;

destructor TTrackList2.Destroy;
begin
  Self.ClearTracks;
  inherited;
end;

function SortTrackByDate(Track1,Track2 : Pointer) : integer;
var
  t1,t2 : TTrack2;
begin
  t1 := TTrack2(Track1);
  t2 := TTrack2(Track2);

  if t1.Points[0].DateTime > t2.Points[0].DateTime then
    SortTrackByDate := 1
  else if t1.Points[0].DateTime < t2.Points[0].DateTime then
    SortTrackByDate := -1
  else
    SortTrackByDate := 0;
end;

Procedure TTrackList2.SortByDate;
begin
  Self.Sort(@SortTrackByDate);
end;

function TTrackList2.ResetBBoxes : Boolean;
var
  i,c : integer;
  Track1, Track2 : TTrack2;
begin
  Result := False;
  c := -1;
  if Self.Count = 0 then
    Exit;

  Track1 := nil;
  for i := 0 to Self.Count-1 do
  begin
    Track1 := Self.Tracks[i];
    c := i;
    break;
  end;

  if c <> -1 then
  begin
    Track1.CalcBBox(True);

    for i := c+1 to Self.Count-1 do
    begin
      Track2 := Self.Tracks[i];
      Track2.CopyBBoxFrom(Track1);
      Track2.CalcBBox(False);
      Track1 := Track2;
    end;

    for i := 0 to Self.Count-1 do
    begin
      Track2 := Self.Tracks[i];
      Track2.CopyBBoxFrom(Track1);
    end;

    Result := True;
  end;
end;

procedure TTrack2.CopyFrom(Track : TTrack2);
var
  i : integer;
  Point4D : TPoint4D2;
begin
  Self.CopyMeta(Track);
  for i := 0 to Track.Count-1 do
  begin
    Point4D := TPoint4D2.Create;
    Point4D.CopyFrom(Track.Points[i]);
    Self.Add(Point4D);
  end;
end;

procedure TTrackList2.FreeAndDelete(Index : integer);
begin
  Self.Tracks[Index].Free;
  Self.Delete(Index);
end;

Procedure TPointList2.FreeAndDelete(Index : integer);
begin
  Self.Points[Index].Free;
  Self.Delete(Index);
end;

procedure TTrack2.CopyMeta(Track : TTrack2);
begin
  Self.Name := Track.Name;
  Self.CopyBBoxFrom(Track);
end;

function TPoint4D2.GetLatitudeStr() : String;
var
  FmtS : TFormatSettings;
  d : Double;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  d := Self.Latitude;
  s := FloatToStrF(Abs(d),ffFixed, 99, 6,FmtS);
  if d < 0 then
    s := s + '° '+_('S') // South
  else
    s := s + '° '+_('N'); // North

  Result := s;
end;

// used for OZIExplorer wpt or rte format
function TPoint4D2.GetLatitudeOZStr() : String;
var
  FmtS : TFormatSettings;
  d : Double;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  d := Abs(Self.Latitude);
  s := IntToStr(Trunc(d)) + ',';
  s := s + FloatToStrF((d-Trunc(d))*60,ffFixed, 99, 5,FmtS);
  if Self.Latitude < 0 then
    s := s + ',S' // West
  else
    s := s + ',N'; //East

  Result := s;
end;

function TPoint4D2.GetLongitudeStr() : String;
var
  FmtS : TFormatSettings;
  d : Double;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  d := Self.Longitude;
  s := FloatToStrF(Abs(d),ffFixed, 99, 6,FmtS);
  if d < 0 then
    s := s + '° '+_('W') // West
  else
    s := s + '° '+_('E'); //East

  Result := s;
end;

// used for OZIExplorer wpt or rte format
function TPoint4D2.GetLongitudeOZStr() : String;
var
  FmtS : TFormatSettings;
  d : Double;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  d := Abs(Self.Longitude);
  s := IntToStr(Trunc(d)) + ',';
  s := s + FloatToStrF((d-Trunc(d))*60,ffFixed, 99, 5,FmtS);
  if Self.Longitude < 0 then
    s := s + ',W' // West
  else
    s := s + ',E'; //East

  Result := s;
end;

function TPoint4D2.GetLatLonGmapStr() : String;
var
  FmtS : TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  Result := FloatToStr(Self.Latitude, FmtS) + ', ' +  FloatToStr(Self.Longitude, FmtS)
end;

function TPoint4D2.GetAltitudeStr() : String;
var
  FmtS : TFormatSettings;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';
  s := FloatToStrF(Self.Altitude,ffFixed, 99, 1,FmtS) + ' ' + _('m'); // GetAltitudeStr
  Result := s;
end;

Function TPointList2.FindFurtherPoint(lon,lat : Double) : TPoint4D2;
var
  i,c : integer;
  d,m : double;
  Point4D : TPoint4D2;
begin
  d := 0;
  c := -1;
  for i := 0 to Self.Count-1 do
  begin
    Point4D := Self.Points[i];
    m := Point4D.WGS84Distance(lon,lat);
    if m > d then
    begin
      d := m;
      c := i;
    end;
  end;
  Result := Self.Points[c];
end;

function TPoint4D2.GetDateTimeStr() : String;
var
  FmtS : TFormatSettings;
  s : String;
  nh,nm,ns,nms : word;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  if Self.DateTime = 0 then
    s := ''
  else
  begin
    s := DateToStr(Self.DateTime,FmtS);
    DecodeTime(Self.DateTime,nh,nm,ns,nms);
    if nms > 0 then
      s := s + ' ' + Format('%.2d:%.2d:%.2d.%.3d',[nh,nm,ns,nms])
    else
      s := s + ' ' + Format('%.2d:%.2d:%.2d',[nh,nm,ns]);
    end;
  Result := s;
end;

{$IfDef DHD}
function TPoint4D2.GetDateTimeStrO(tzo: TTimeZoneOffset = tzo_GMT): String;
var
  FmtS : TFormatSettings;
  s : String;
  nh,nm,ns,nms : word;
  offset: extended;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  if Self.DateTime = 0 then
    s := ''
  else
  begin
    if tzo = tzo_GMT then
      s := DateTimeToStr(Self.DateTime, FmtS)
    else
      begin
        offset := 0;
        case tzo of
          tzo_ROME: offset := 1 * ONE_HOUR;
          tzo_IS:  offset := 0 * ONE_HOUR;
          tzo_EST: offset := 4 * ONE_HOUR;
          tzo_EDT: offset := 5 * ONE_HOUR;
          tzo_CST: offset := 5 * ONE_HOUR;
          tzo_MST: offset := 6 * ONE_HOUR;
          tzo_PST: offset := 7 * ONE_HOUR;
        end;
        s := DateTimeToStr(Self.DateTime - offset, FmtS);
        DecodeTime(Self.DateTime - offset, nh, nm, ns, nms);
        if nms > 0 then
          s := s + ' ' + Format('%.2d:%.2d:%.2d.%.3d',[nh,nm,ns,nms])
        else
          s := s + ' ' + Format('%.2d:%.2d:%.2d',[nh,nm,ns]);
        end;
      end;
  Result := s;
end;
{$EndIf}

function TPoint4D2.IsInBox(E,O,S,N : double) : Boolean;
var
  mE,mO,mS,mN : double;
  r : Boolean;
begin
  mE := Max(E,O);
  mO := Min(E,O);
  mS := Max(N,S);
  mN := Min(N,S);

  r := (Self.Latitude < mS);
  r := r and (Self.Latitude > mN);
  r := r and (Self.Longitude < mE);
  r := r and (Self.Longitude > mO);
  Result := r;
end;

{$IfDef DHD}
function TPointList2.GetSpeedOf(point: TPoint4D2): double;      // DHD version in mph
const
  NRSECONDSINADAY = 24 { HOURS } * 3600 { Seconds in an hour };
var
  ItemIndex : integer;
  DistanceInFeet, FractionOfADay, TimeInSeconds: double;
  ThisPoint, LastPoint: TPoint4D2;
begin
  result := -1;
  ItemIndex := IndexOf(point);
  if (Itemindex < 1) or (Itemindex > Count-1) then
    Exit;
  if (Points[itemIndex-1].DateTime = 0) or
     (Points[itemIndex].DateTime = 0) then
    Exit;

  LastPoint := Points[itemIndex-1];
  ThisPoint := Points[ItemIndex];
  Assert(ThisPoint = Point, 'Bad assumption');

  FractionOfADay := (ThisPoint.DateTime - LastPoint.DateTime);
  TimeInSeconds  := FractionOfADay * NRSECONDSINADAY;
  DistanceInFeet := Distance(LastPoint.Latitude, LastPoint.Longitude,
                             ThisPoint.Latitude, ThisPoint.Longitude,
                             muFeet);
  if TimeInSeconds > 0 then
    result := (DistanceInFeet / TimeInSeconds) * 3600 { seconds / hour } / 5280 { feet / mile }
  else
    result := 0;
end;
{$EndIf}

{$IfNDef DHD}
// speed in km/h
function TPointList2.GetSpeedOf(point : TPoint4D2) : Double;
var
  ItemIndex : integer;
  d : double;
  t : double;
begin
  Result := -1;
  ItemIndex := Self.IndexOf(point);
  if (Itemindex < 1) or (Itemindex > Self.Count-1) then
    Exit;
  if (Self.Points[itemIndex-1].DateTime = 0) or
     (Self.Points[itemIndex].DateTime = 0) then
    Exit;
  d := Self.Points[itemIndex-1].WGS84Distance(Self.Points[itemIndex]);
  t := Abs(Self.Points[itemIndex].DateTime - Self.Points[itemIndex-1].DateTime);
  if t = 0 then
    Exit;
  Result := (d/1000) / (t*24);
end;
{$EndIf}

{$IfDef DHD}
function getAngleBetweenPoints(X1,Y1, X2,Y2:double):double;
var
  dx,dy:double;
begin
  dx := X2 - X1;
  dy := Y2 - Y1;

  if (dx > 0) then  result := (Pi*0.5) - ArcTan(dy/dx)   else
  if (dx < 0) then  result := (Pi*1.5) - ArcTan(dy/dx)   else
  if (dy > 0) then  result := 0                          else
  if (dy < 0) then  result := Pi                         else
                    result := cNO_ANGLE; // the 2 points are equal

  result := RadToDeg(result);
end;
{$EndIf}

{$IfDef DHD}
function TPointList2.GetDirectionOf(point: TPoint4D2): string;
var
  ItemIndex : integer;
  Degrees: double;
  i: integer;

  function IsLocation(point: TPoint4D2): boolean;
  begin
    result := (point.Latitude <> 0) and (point.Longitude <> 0);
  end;

begin
  Result := '';
  ItemIndex := IndexOf(point);
  if (Itemindex < 1) or (Itemindex > Count-1) then
    Exit;
  if not (IsLocation(Points[itemIndex-1]) and IsLocation(Points[itemIndex]))  then
    Exit;
  Degrees := getAngleBetweenPoints( Points[ItemIndex-1].Longitude, Points[ItemIndex-1].Latitude,
                                    Points[ItemIndex].Longitude,   Points[ItemIndex].Latitude);
  if Degrees <> cNO_ANGLE then
    begin
      for i := 0 to NR_OCTANTS do
        if (Degrees >= (Octants[i].sa - HALFOCTANT)) and (Degrees < (Octants[i].sa + HALFOCTANT)) then
          begin
            result := Octants[i].desc;
            break;
          end;
      if result = '' then
        if Degrees >= (360 - HALFOCTANT) then  // "North"
          result := Octants[0].desc;
    end;
end;
{$EndIf}

{$IfDef DHD}
function TimeZoneFromAbbrev(Abbrev: string): TTimeZoneOffset;
var
  tzo: TTimeZoneOffset;
begin
  for tzo := Low(TTimeZoneOffset) to High(TTimeZoneOffset) do
    if TimeZones[tzo].Abbrev = Abbrev then
      begin
        result := tzo;
        exit;
      end;
  result := tzo_GMT;
end;
{$EndIf}

procedure TPointList2.SetEnabled(Value : Boolean);
var
  i : integer;
begin
  Self.penabled := Value;
  For i := 0 to Self.Count-1 do
    Self.Points[i].enabled := Value;
end;

function TBBox.GetCenter() : TPoint4D2;
begin
  Result := Tpoint4D2.Create;
  Result.Latitude := Self.S+((Self.N-Self.S)/2);
  Result.Longitude := Self.O+((Self.E-Self.O)/2);
end;

function TPoint4D2.GetLatitudeSimpleStr() : String;
var
  FmtS : TFormatSettings;
  d : Double;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  d := Self.Latitude;
  s := FloatToStrF(d,ffFixed, 99, 6,FmtS);
  Result := s;
end;

function TPoint4D2.GetLongitudeSimpleStr() : String;
var
  FmtS : TFormatSettings;
  d : Double;
  s : String;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';

  d := Self.Longitude;
  s := FloatToStrF(d,ffFixed, 99, 6,FmtS);
  Result := s;
end;

function TPointList2.GetOldestDateTime() : TDateTime;
var
  t : TDateTime;
  i : integer;
begin
  t := 0;
  try
    for i := 0 to Self.Count-1 do
      if t < Self.Points[i].DateTime then
        if Self.Points[i].DateTime > 0 then
          t := Self.Points[i].DateTime;
  finally
    Result := t;
  end;
end;

function TPointList2.GetNewestDateTime() : TDateTime;
var
  t : TDateTime;
  i : integer;
begin
  t := MAXDATETIME;
  try
    for i := 0 to Self.Count-1 do
      if t > Self.Points[i].DateTime then
        if Self.Points[i].DateTime > 0 then
          t := Self.Points[i].DateTime;
  finally
    Result := t;
  end;
end;

end.

