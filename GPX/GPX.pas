unit GPX;

// Base GPX unit, contains evrything needed to represent a GPX file in memory
// All the fields are derived from the GPX 1.1 documentation at
// http://www.topografix.com/gpx/1/1/
//  Limitation 1 : only supports 1 link by object
//  Limitation 2 : extensions are not interpreted, only stored as string;

interface

uses
  Fastcode, Classes, GPS, GPX_Style;

type
  PGPXwptList = ^TGPXwptList;
  PGPXrteList = ^TGPXrteList;
  PGPXtrkList = ^TGPXtrkList;

  TGPXOptions = Class
    GPX_Style : Boolean;
    Compress : Boolean;
    FileHistory : integer;
    ClipBoardStyle : integer;
    ProxyType : Byte;
    ProxyHttpIP : String;
    ProxyHttpPort : String;
    ProxySocksIP : String;
    ProxySocksPort : String;
    ProxyMultiIP : String;
    ProxyMultiPort : String;
    ElevationDatabase : byte;
    GeoNamesUsername : String;
    User_Agent : String;
    IgnoreXMLVersion : Boolean;
    KeepXMLCRLF : Boolean;
    DeleteNullData : Boolean;
    ClearUndoBeforeSave : Boolean;
    SortbydatebeforeRemovingDuplicate : Boolean;
    PreserveXMLNS : Boolean;
    DefaultFileName : String;
    DefaultFileName2 : String;
    DefaultFileName3 : String;
    DefaultFileName4 : String;
    DefaultFileNameNum : Byte;
    UseDefaultFileName : Boolean;
    procedure CopyFrom(GPXOptions : TGPXOptions);
    constructor Create;
    destructor Destroy; override;
  end;

  TGPXcopyright = Class
    //author="xsd:string [1] ?">
    author : String;
    //<year> xsd:gYear </year> [0..1] ?
    year : integer;
    //<license> xsd:anyURI </license> [0..1] ?
    license : String;
    filled : Boolean;
    procedure CopyFrom(Copyright : TGPXcopyright);
    constructor Create;
  end;

  TGPXLink = Class
    //href="xsd:anyURI [1] ?">
    href : String;
    //<text> xsd:string </text> [0..1] ?
    text : String;
    //<type> xsd:string </type> [0..1] ?
    ltype : String;
    filled : Boolean;
    procedure CopyFrom(Link : TGPXLink);
    constructor Create;
  end;

  TGPXemail = Class
    id : String;
    domain : String;
    filled : Boolean;
    procedure CopyFrom(email : TGPXemail);
    constructor Create;
    destructor Destroy; override;
  end;

  TGPXperson = Class
    //<name> xsd:string </name> [0..1] ?
    name : String;
    //<email> emailType </email> [0..1] ?
    email : TGPXemail;
    //<link> linkType </link> [0..1] ?
    link : TGPXlink;
    filled : Boolean;
    procedure CopyFrom(Person : TGPXperson);
    constructor Create;
    destructor Destroy; override;
  end;

  TGPXbounds = Class
    //minlat="latitudeType [1] ?"
    minlat : Double;
    //minlon="longitudeType [1] ?"
    minlon : Double;
    //maxlat="latitudeType [1] ?"
    maxlat : Double;
    //maxlon="longitudeType [1] ?"/>
    maxlon : Double;
    filled : Boolean;
    procedure CopyFrom(Bounds : TGPXbounds);
    constructor Create;
  end;

  TGPXmetadata = Class
    //<name> xsd:string </name> [0..1] ?
    name : String;
    //<desc> xsd:string </desc> [0..1] ?
    desc : String;
    //<author> personType </author> [0..1] ?
    author : TGPXperson;
    //<copyright> copyrightType </copyright> [0..1] ?
    copyright : TGPXcopyright;
    //<link> linkType </link> [0..*] ?
    link : TGPXLink;
    //<time> xsd:dateTime </time> [0..1] ?
    mtime : TDateTime;
    //<keywords> xsd:string </keywords> [0..1] ?
    keywords : String;
    //<bounds> boundsType </bounds> [0..1] ?
    bounds : TGPXBounds;
    //<extensions> extensionsType </extensions> [0..1] ?
    extensions : String;
    filled : Boolean;
    procedure CopyFrom(Metadata : TGPXMetadata);
    constructor Create;
    destructor Destroy; override;
  end;

  TGPXwpt = Class(TPoint4D2)
    //<magvar> degreesType </magvar> [0..1] ?
    magvar : Double;
    //<geoidheight> xsd:decimal </geoidheight> [0..1] ?
    geoidheight : Double;
    //<name> xsd:string </name> [0..1] ?
    name : String;
    //<cmt> xsd:string </cmt> [0..1] ?
    cmt : String;
    //<desc> xsd:string </desc> [0..1] ?
    desc : String;
    //<src> xsd:string </src> [0..1] ?
    src : String;
    //<link> linkType </link> [0..*] ?
    link : TGPXLink;
    //<sym> xsd:string </sym> [0..1] ?
    sym : String;
    //<type> xsd:string </type> [0..1] ?
    wtype : String;
    //<fix> fixType </fix> [0..1] ?
    fix : String;
    //<sat> xsd:nonNegativeInteger </sat> [0..1] ?
    sat : Byte;
    //<hdop> xsd:decimal </hdop> [0..1] ?
    hdop : Double;
    //<vdop> xsd:decimal </vdop> [0..1] ?
    vdop : Double;
    //<pdop> xsd:decimal </pdop> [0..1] ?
    pdop : Double;
    //<ageofdgpsdata> xsd:decimal </ageofdgpsdata> [0..1] ?
    ageofdgpsdata : Double;
    //<dgpsid> dgpsStationType </dgpsid> [0..1] ?
    dgpsid : Integer;
    //<extensions> extensionsType </extensions> [0..1] ?
    extensions : String;
    procedure CopyFrom(WayPoint : TGPXwpt); overload;
    function IsIdentical(WayPoint : TGPXwpt; FilterMode : byte = 0) : Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TGPXwptList = Class(TPointList2)
    private
      function GetWaypoint(Index: Integer) : TGPXwpt;
      procedure SetWayPoint(Index: Integer; WayPoint : TGPXwpt);
    public
      property WayPoints [Index: Integer]: TGPXwpt read GetWaypoint write SetWayPoint;
      procedure CopyFrom(wptlist : TGPXwptList);
      procedure SortByDate();
      procedure ClearPointList; overload;
      procedure RemoveDuplicates(FilterMode : byte = 0; SortBefore : boolean = True);
      constructor Create;
      destructor Destroy; override;
  end;

  TGPXrte = Class(TGPXwptList)
    private
      function GetRtePoint(Index: Integer) : TGPXwpt;
      procedure SetRtePoint(Index: Integer; RtePoint : TGPXwpt);
    public
      //<name> xsd:string </name> [0..1] ?
      name : String;
      //<cmt> xsd:string </cmt> [0..1] ?
      cmt : String;
      //<desc> xsd:string </desc> [0..1] ?
      desc : String;
      //<src> xsd:string </src> [0..1] ?
      src : String;
      //<link> linkType </link> [0..*] ?
      link : TGPXLink;
      //<number> xsd:nonNegativeInteger </number> [0..1] ?
      number : integer;
      //<type> xsd:string </type> [0..1] ?
      rtype : String;
      //<extensions> extensionsType </extensions> [0..1] ?
      extensions : String;
      // xmlns="http://www.topografix.com/GPX/gpx_style/0/2"
      LineStyle : TGPX_StyleLine;
      //<rtept> wptType </rtept> [0..*] ?
      property rtept [Index: Integer]: TGPXwpt read GetRtePoint write SetRtePoint;
      procedure CopyFrom(Route : TGPXrte);
      constructor Create;
      destructor Destroy; override;
  end;

  TGPXtrkseg = Class(TGPXwptList)
    private
      function GetTrkPoint(Index: Integer) : TGPXwpt;
      procedure SetTrkPoint(Index: Integer; Point : TGPXwpt);
    public
    //<extensions> extensionsType </extensions> [0..1] ?
    extensions : String;
    //<trkpt> wptType </trkpt> [0..*] ?
    property trkpt [Index: Integer]: TGPXwpt read GetTrkPoint write SetTrkPoint;
    procedure CopyFrom(TrackSeg : TGPXtrkseg);
    constructor Create;
    destructor Destroy; override;
  end;

  TGPXtrk = Class(TList)
    private
      penabled : Boolean;
      function GetTrkSeg(Index: Integer) : TGPXtrkseg;
      procedure SetTrkSeg(Index: Integer; tseg : TGPXTrkseg);
      procedure SetEnabled(Value : Boolean);
    public
      //<name> xsd:string </name> [0..1] ?
      name : String;
      //<cmt> xsd:string </cmt> [0..1] ?
      cmt : String;
      //<desc> xsd:string </desc> [0..1] ?
      desc : String;
      //<src> xsd:string </src> [0..1] ?
      src : String;
      //<link> linkType </link> [0..*] ?
      link : TGPXLink;
      //<number> xsd:nonNegativeInteger </number> [0..1] ?
      number : integer;
      //<type> xsd:string </type> [0..1] ?
      ttype : String;
      //<extensions> extensionsType </extensions> [0..1] ?
      extensions : String;
      // xmlns="http://www.topografix.com/GPX/gpx_style/0/2"
      LineStyle : TGPX_StyleLine;
      //<rtept> wptType </rtept> [0..*] ?
      property enabled : Boolean read penabled write SetEnabled;
      property TrkSeg [Index: Integer]: TGPXTrkseg read GetTrkSeg write SetTrkSeg;
      procedure CopyFrom(trk : TGPXtrk);
      function Cuttrkseg(Which : TGPXTrkseg; Where : TGPXwpt) : TGPXtrkseg;
      procedure SortByDate;
      function GetOldestDateTime() : TDateTime;
      function GetNewestDateTime() : TDateTime;
      constructor Create;
      destructor Destroy; override;
  end;

  TGPXrteList = Class(TList)
    private
      penabled : Boolean;
      function GetRoute(Index: Integer) : TGPXrte;
      procedure SetRoute(Index: Integer; Route : TGPXrte);
      procedure SetEnabled(Value : Boolean);
    public
      filled : Boolean;
      property enabled : Boolean read penabled write SetEnabled;
      property Routes [Index: Integer]: TGPXrte read GetRoute write SetRoute;
      procedure CopyFrom(rtelist : TGPXrteList);
      function CutRoute(Which : TGPXrte; Where : TGPXwpt) : TGPXrte;
      function BindRoutes(Route1,Route2 : TGPXrte) : TGPXrte;
      procedure SortByDate();
      function GetOldestDateTime() : TDateTime;
      function GetNewestDateTime() : TDateTime;
      constructor Create;
      destructor Destroy; override;
  end;

  TGPXtrkList = Class(TList)
    private
      penabled : Boolean;
      function GetTrack(Index: Integer) : TGPXtrk;
      procedure SetTrack(Index: Integer; Route : TGPXtrk);
      procedure SetEnabled(Value : Boolean);
    public
      filled : Boolean;
      property enabled : Boolean read penabled write SetEnabled;
      property Tracks [Index: Integer]: TGPXtrk read GetTrack write SetTrack;
      procedure CopyFrom(trkList : TGPXtrkList);
      function GetTrackOfTrackSeg(TrackSeg : TGPXtrkseg) : TGPXtrk;
      function BindTrackSeg(TrackSeg1,TrackSeg2 : TGPXtrkseg) : TGPXtrkseg;
      function GetOldestDateTime() : TDateTime;
      function GetNewestDateTime() : TDateTime;
      constructor Create;
      destructor Destroy; override;
  end;

  TGPXSelection = Class(TList)
    Parent : Pointer;
    function IsInSelection(Truc : TObject) : Boolean;
    function APointListIsSelected() : Boolean;
    procedure copyFrom(selection : TGPXSelection);
  end;

  TGPXExtension = Class
    Text : String;
    procedure CopyFrom(extension : TGPXExtension);
    constructor Create;
    destructor Destroy; override;
  end;

  TGPX = Class
    private
      penabled : Boolean;
      version : String;
      procedure SetEnabled(Value : Boolean);
    public
      creator : String;
      xmlns : String;
      metadata : TGPXMetadata;
      wpt : TGPXwptList;
      rte : TGPXrteList;
      trk : TGPXtrkList;
      extension : TGPXExtension;
      Selection : TGPXSelection;
      procedure CopyFrom(GPXd : TGPX);
      procedure ComputeMetaBounds;
      procedure Clear;
      function BindSelection : TObject;
      constructor Create;
      destructor Destroy; override;
      property enabled : Boolean read penabled write SetEnabled;
      function GetWaypointParent(waypoint : TGPXwpt) : TGPXwptList;
  end;


implementation

constructor TGPXperson.Create;
begin
  inherited;
  Self.email := TGPXemail.Create;
  Self.link := TGPXLink.Create;
end;

destructor TGPXperson.Destroy;
begin
  Self.link.Free;
  Self.email.Free;
  inherited;
end;

constructor TGPXMetadata.Create;
begin
  inherited;
  Self.author := TGPXperson.Create;
  Self.copyright := TGPXCopyright.Create;
  Self.link := TGPXLink.Create;
  Self.bounds := TGPXBounds.Create;
end;

destructor TGPXMetadata.Destroy;
begin
  Self.author.Free;
  Self.copyright.Free;
  Self.link.Free;
  Self.bounds.Free;
  inherited;
end;

procedure TGPXwpt.CopyFrom(WayPoint : TGPXwpt);
begin
  inherited CopyFrom(TPoint4D2(WayPoint));
  Self.magvar := WayPoint.magvar;
  Self.geoidheight := WayPoint.geoidheight;
  Self.name := WayPoint.name;
  Self.cmt := WayPoint.cmt;
  Self.desc := WayPoint.desc;
  Self.src := WayPoint.src;
  Self.link.Free;
  Self.link := TGPXLink.Create;
  Self.link.copyFrom(WayPoint.link);
  Self.sym := WayPoint.sym;
  Self.wtype := WayPoint.wtype;
  Self.fix := WayPoint.fix;
  Self.sat := WayPoint.sat;
  Self.hdop := WayPoint.hdop;
  Self.vdop := WayPoint.vdop;
  Self.pdop := WayPoint.pdop;
  Self.ageofdgpsdata := WayPoint.ageofdgpsdata;
  Self.dgpsid := WayPoint.dgpsid;
  Self.extensions := WayPoint.extensions;
end;

constructor TGPXwpt.Create;
begin
  inherited;
  Self.link := TGPXLink.Create;
  Self.DateTime := 0;
  Self.sat := 255;
  Self.dgpsid := -1;

  Self.hdop := -1;
  Self.vdop := -1;
  Self.pdop := -1;
  Self.ageofdgpsdata := -1;
end;

destructor TGPXwpt.Destroy;
begin
  Self.link.Free;
  inherited;
end;


function TGPXrte.GetRtePoint(Index: Integer) : TGPXwpt;
begin
  Result := TGPXwpt(Self.Items[Index]);
end;

procedure TGPXrte.SetRtePoint(Index: Integer; RtePoint : TGPXwpt);
begin
  Self.rtept[Index].Free;
  Self.Items[Index] := Pointer(RtePoint);
end;

constructor TGPXrte.Create;
begin
  inherited;
  Self.link := TGPXLink.Create;
  Self.number := -1;
  Self.LineStyle := TGPX_StyleLine.Create;

  Self.enabled := True;
end;

destructor TGPXrte.Destroy;
begin
  Self.ClearPointList;
  Self.link.Free;
  Self.LineStyle.Free;
//  Self.Clear;
  inherited;
end;

function TGPXtrkseg.GetTrkPoint(Index: Integer) : TGPXwpt;
begin
  Result := TGPXwpt(Self.Items[Index]);
end;

procedure TGPXtrkseg.SetTrkPoint(Index: Integer; Point : TGPXwpt);
begin
  Self.trkpt[Index].Free;
  Self.Items[Index] := Pointer(Point);
end;

constructor TGPXtrkseg.Create;
begin
  Self.enabled := True;

  inherited;
end;

destructor TGPXtrkseg.Destroy;
begin
  Self.ClearPointList;
//  Self.Clear;
  inherited;
end;

function TGPXtrk.GetTrkSeg(Index: Integer) : TGPXtrkseg;
begin
  Result := TGPXtrkseg(Self.Items[Index]);
end;

procedure TGPXtrk.SetTrkSeg(Index: Integer; tseg : TGPXTrkseg);
begin
  Self.trkseg[Index].Free;
  Self.Items[Index] := Pointer(tseg);
end;

constructor TGPXtrk.Create;
begin
  inherited;
  Self.link := TGPXLink.Create;
  Self.number := -1;
  Self.LineStyle := TGPX_StyleLine.Create;

  Self.enabled := True;
end;

destructor TGPXtrk.Destroy;
var
  i,c : integer;
begin
  c := Self.Count-1;
  if c >= 0 then
  begin
    for i := c downto 0 do
    begin
      Self.trkseg[i].Free;
      Self.Delete(i);
    end;
  end;
  Self.Clear;
  Self.link.Free;
  Self.LineStyle.Free;
  inherited;
end;

function TGPXwptList.GetWaypoint(Index: Integer) : TGPXwpt;
begin
  Result := TGPXwpt(Self.Items[Index]);
end;

procedure TGPXwptList.SetWaypoint(Index: Integer; WayPoint : TGPXwpt);
begin
  Self.WayPoints[Index].Free;
  Self.Items[Index] := Pointer(WayPoint);
end;

constructor TGPXwptList.Create;
begin
  inherited;
  Self.enabled := True;
end;

destructor TGPXwptList.Destroy;
var
  i,c : integer;
begin
  c := Self.Count-1;
  if c >= 0 then
  begin
    for i := c downto 0 do
    begin
      Self.WayPoints[i].Free;
    end;
  end;
  Self.Clear;
  inherited;
end;

function TGPXrteList.GetRoute(Index: Integer) : TGPXrte;
begin
  Result := TGPXrte(Self.Items[Index]);
end;

procedure TGPXrteList.SetRoute(Index: Integer; Route : TGPXrte);
begin
  Self.Routes[Index].Free;
  Self.Items[Index] := Pointer(Route);
end;

constructor TGPXrteList.Create;
begin
  inherited;
  Self.enabled := True;
end;

destructor TGPXrteList.Destroy;
var
  i,c : integer;
begin
  c := Self.Count-1;
  if c >= 0 then
  begin
    for i := c downto 0 do
    begin
      Self.Routes[i].Free;
    end;
  end;
  Self.Clear;
  inherited;
end;

function TGPXtrkList.GetTrack(Index: Integer) : TGPXtrk;
begin
  Result := TGPXtrk(Self.Items[Index]);
end;

procedure TGPXtrkList.SetTrack(Index: Integer; Route : TGPXtrk);
begin
  Self.Tracks[Index].Free;
  Self.Items[Index] := Pointer(Route);
end;

constructor TGPXtrkList.Create;
begin
  inherited;
  Self.enabled := True;
end;

destructor TGPXtrkList.Destroy;
var
  i,c : integer;
begin
  c := Self.Count-1;
  if c >= 0 then
  begin
    for i := c downto 0 do
    begin
      Self.Tracks[i].Free;
    end;
  end;
  Self.Clear;
  inherited;
end;

constructor TGPX.Create;
begin
  inherited;
  Self.metadata := TGPXMetadata.Create;
  Self.wpt := TGPXwptList.Create;
  Self.rte := TGPXrteList.Create;
  Self.trk := TGPXtrkList.Create;
  Self.extension := TGPXExtension.Create;
  Self.Selection := TGPXSelection.Create;
  Self.Selection.Parent := Self;
  Self.penabled := True;
end;

destructor TGPX.Destroy;
begin
  Self.Selection.Free;
  Self.metadata.Free;
  Self.wpt.Free;
  Self.rte.Free;
  Self.trk.Free;
  Self.extension.Free;
  inherited;
end;

function TGPXSelection.IsInSelection(Truc : TObject) : Boolean;
var
  r : Boolean;
  i : integer;
//  Track : TGPXtrk;
begin
  r := False;
  for i := 0 to Self.Count-1 do
  begin
    if Self.Items[i] = Truc then
    begin
      r := True;
      Break;
    end;
  end;
  {
  if Truc.ClassNameIs('TGPXtrkseg') then
  begin
    if Assigned(Parent) then
    begin
      for i := 0 to TGPX(Parent).trk.Count-1 do
      begin
        Track := TGPX(Parent).trk.Tracks[i];
        if Track.IndexOf(Truc)<> -1 then
        begin
          r := True;
          Break;
        end;
      end;
    end;
  end;
  }
  Result := r;
end;

procedure TGPXLink.CopyFrom(Link : TGPXLink);
begin
  Self.href := Link.href;
  Self.text := Link.text;
  Self.ltype := Link.ltype;
  Self.filled := Link.filled;
end;

procedure TGPXrte.CopyFrom(Route : TGPXrte);
var
  i : integer;
  WP : TGPXwpt;
begin
  Self.name := Route.name;
  Self.cmt := Route.cmt;
  Self.desc := Route.desc;
  Self.src := Route.src;
  Self.link.Free;
  Self.link := TGPXLink.Create;
  Self.link.copyFrom(Route.link);
  Self.number := Route.number;
  Self.rtype := Route.rtype;
  Self.extensions := Route.extensions;
  Self.LineStyle.CopyFrom(Route.LineStyle);

  for i := Self.Count-1 downto 0 do
  begin
    Self.rtept[i].Free;
    Self.Delete(i);
  end;
  Self.Clear;
  for i := 0 to Route.Count-1 do
  begin
    WP := TGPXwpt.Create;
    WP.CopyFrom(Route.WayPoints[i]);
    Self.Add(WP);
  end;
end;

procedure TGPXtrkseg.CopyFrom(TrackSeg : TGPXtrkseg);
var
  i : integer;
  WP : TGPXwpt;
begin
  Self.extensions := TrackSeg.extensions;
  for i := Self.Count-1 downto 0 do
  begin
    Self.trkpt[i].Free;
  end;
  Self.Clear;
  for i := 0 to TrackSeg.Count-1 do
  begin
    WP := TGPXwpt.Create;
    WP.CopyFrom(TrackSeg.WayPoints[i]);
    Self.Add(WP);
  end;
end;

procedure TGPX.CopyFrom(GPXd : TGPX);
begin
  Self.version := GPXd.version;
  Self.creator := GPXd.creator;
  Self.xmlns   := GPXd.xmlns;
  Self.metadata.CopyFrom(GPXd.metadata);
  Self.wpt.copyFrom(GPXd.wpt);
  Self.rte.copyFrom(GPXd.rte);
  Self.trk.copyFrom(GPXd.trk);
  Self.extension.CopyFrom(GPXd.extension);
  Self.Selection.copyFrom(GPXd.Selection);
end;

procedure TGPXwptList.CopyFrom(wptlist : TGPXwptList);
var
  i : integer;
  WP : TGPXwpt;
begin
  for i := 0 to wptlist.Count-1 do
  begin
    WP := TGPXwpt.Create;
    WP.CopyFrom(wptlist.WayPoints[i]);
    Self.Add(WP);
  end;
  Self.filled := wptList.filled;
end;

procedure TGPXrteList.CopyFrom(rtelist : TGPXrteList);
var
  i : integer;
  RTE : TGPXrte;
begin
  Self.filled := rtelist.filled;
  for i := 0 to rtelist.Count-1 do
  begin
    RTE := TGPXrte.Create;
    RTE.copyFrom(rtelist.Routes[i]);
    Self.Add(RTE);
  end;
end;

procedure TGPXtrk.CopyFrom(trk : TGPXtrk);
var
  i : integer;
  TRKSeg : TGPXtrkseg;
begin
  Self.name := trk.name;
  Self.cmt := trk.cmt;
  Self.desc := trk.desc;
  Self.src := trk.src;
  Self.link.Free;
  Self.link := TGPXLink.Create;
  Self.link.copyFrom(trk.link);
  Self.number := trk.number;
  Self.ttype := trk.ttype;
  Self.extensions := trk.extensions;
  Self.LineStyle.CopyFrom(trk.LineStyle);

  for i := 0 to trk.Count-1 do
  begin
    TRKSeg := TGPXtrkseg.Create;
    TRKSeg.copyFrom(trk.trkseg[i]);
    Self.Add(TRKSeg);
  end;
end;

procedure TGPXtrkList.CopyFrom(trkList : TGPXtrkList);
var
  i : integer;
  TRK : TGPXtrk;
begin
  Self.filled := trkList.filled;
  for i := 0 to trkList.Count-1 do
  begin
    TRK := TGPXtrk.Create;
    TRK.copyFrom(trkList.Tracks[i]);
    Self.Add(TRK);
  end;
end;

procedure TGPXSelection.CopyFrom(selection : TGPXSelection);
//var
//  i : integer;
begin
  if Assigned(selection.Parent) then
    Self.Parent := selection.Parent;

  Self.Clear;

{  for i := 0 to selection.Count-1 do
  begin
    Self.Add(selection.Items[i]);
  end; }
end;

function TGPXrteList.CutRoute(Which : TGPXrte; Where : TGPXwpt) : TGPXrte;
var
  i : integer;
  NewRoute : TGPXrte;
  sp : integer;
begin
  Result := nil;
  sp := Which.IndexOf(Where);
  if sp = -1 then
    Exit;
  if sp < 3 then
    Exit;
  if sp > Which.Count-3 then
    Exit;

  NewRoute := TGPXrte.Create;
  NewRoute.copyFrom(Which);
  for i := Which.Count-1 downto sp+1 do
    Which.Delete(i);
  for i := 0 to sp-1 do
    NewRoute.Delete(0);

  NewRoute.name := NewRoute.name + ' (part 2)';

  Self.Insert(Self.IndexOf(Which)+1,NewRoute);
  Result := NewRoute;
end;

function TGPXTrk.Cuttrkseg(Which : TGPXTrkseg; Where : TGPXwpt) : TGPXTrkseg;
var
  i : integer;
  NewTrackSeg : TGPXTrkseg;
  sp : integer;
begin
  Result := nil;
  sp := Which.IndexOf(Where);
  if sp = -1 then
    Exit;
  if sp < 3 then
    Exit;
  if sp > Which.Count-3 then
    Exit;

  NewTrackSeg := TGPXTrkseg.Create;
  NewTrackSeg.copyFrom(Which);
  for i := Which.Count-1 downto sp+1 do
    Which.Delete(i);
  for i := 0 to sp-1 do
    NewTrackSeg.Delete(0);

  Self.Insert(Self.IndexOf(Which)+1,NewTrackSeg);
  Result := NewTrackSeg;
end;

function TGPXTrkList.GetTrackOfTrackSeg(TrackSeg : TGPXtrkseg) : TGPXtrk;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
  begin
    if Self.Tracks[i].indexof(TrackSeg) <> -1 then
    begin
      Result := Self.Tracks[i];
      Break;
    end;
  end;
end;

function TGPXSelection.APointListIsSelected() : Boolean;
var
  i : integer;
begin
  Result := False;
  for i := 0 to Self.Count-1 do
  begin
    if Assigned(Self.Items[i]) then
      if TObject(Self.Items[i]).ClassNameIs('TGPXrte') or
         TObject(Self.Items[i]).ClassNameIs('TGPXtrkseg') then
      begin
        Result := True;
        Break;
      end;
  end;
end;

function TGPX.BindSelection : TObject;
var
  FirstObject : TObject;
  SecondRoute : TGPXrte;
  SecondTrackSeg : TGPXtrkseg;
begin
  Result := nil;
  if not Assigned(Selection) then
    Exit;
  if Selection.Count <> 2 then
    Exit;

  FirstObject := Selection.Items[0];

  if not Assigned(FirstObject) then
    Exit;

  if not Assigned(Selection.Items[1]) then
    Exit;


  if FirstObject.ClassNameIs('TGPXrte') then
  begin
    if not TObject(Selection.Items[1]).ClassNameIs('TGPXrte') then
      Exit;
    SecondRoute := TGPXrte(Selection.Items[1]);
    rte.BindRoutes(TGPXrte(FirstObject),SecondRoute);
    Result := FirstObject;
  end
  else if FirstObject.ClassNameIs('TGPXtrkseg') then
  begin
    if not TObject(Selection.Items[1]).ClassNameIs('TGPXtrkseg') then
      Exit;
    SecondTrackSeg := TGPXtrkseg(Selection.Items[1]);
    trk.BindTrackSeg(TGPXtrkseg(FirstObject),SecondTrackSeg);
    Result := FirstObject;
  end;
end;

function TGPXrteList.BindRoutes(Route1,Route2 : TGPXrte) : TGPXrte;
var
  P1, P2 : TGPXwpt;
  FirstIs1 : Boolean;
  SamePoint : Boolean;
  i,si : integer;
begin
//  Result := nil;
  P1 := Route1.WayPoints[Route1.Count-1];
  P2 := Route2.WayPoints[0];
  SamePoint := (P1.Longitude = P2.Longitude) and
               (P1.Latitude  = P2.Latitude ) and
               (P1.Altitude  = P2.Altitude );
  FirstIs1 := SamePoint or (P1.DateTime < P2.DateTime);
  if not FirstIs1 then
    FirstIs1 := Self.IndexOf(Route1) < Self.IndexOf(Route2);

  if FirstIs1 then
  begin
    if SamePoint then
      si := 1
    else
      si := 0;
  end
  else
  begin
    P1 := Route1.WayPoints[0];
    P2 := Route2.WayPoints[Route2.Count-1];
    SamePoint := (P1.Longitude = P2.Longitude) and
                 (P1.Latitude  = P2.Latitude ) and
                 (P1.Altitude  = P2.Altitude );
    if SamePoint then
      si := 1
    else
      si := 0;
  end;

  if FirstIs1 then
  begin
    for i := si to Route2.Count-1 do
    begin
      P1 := TGPXwpt.Create;
      P1.CopyFrom(Route2.WayPoints[i]);
      Route1.Add(P1);
    end;
  end
  else
  begin
    for i := 0 to Route2.Count-(1+si) do
    begin
      P1 := TGPXwpt.Create;
      P1.CopyFrom(Route2.WayPoints[i]);
      Route1.Insert(i,P1);
    end
  end;
  Self.Delete(Self.IndexOf(Route2));
  Result := Route1;
end;

function TGPXtrkList.BindTrackSeg(TrackSeg1,TrackSeg2 : TGPXtrkseg) : TGPXtrkseg;
var
  P1, P2 : TGPXwpt;
  FirstIs1 : Boolean;
  SamePoint : Boolean;
  i,si : integer;
  Track : TGPXtrk;
begin
//  Result := nil;

  P1 := TrackSeg1.WayPoints[TrackSeg1.Count-1];
  P2 := TrackSeg2.WayPoints[0];
  SamePoint := (P1.Longitude = P2.Longitude) and
               (P1.Latitude  = P2.Latitude ) and
               (P1.Altitude  = P2.Altitude );
  FirstIs1 := SamePoint or (P1.DateTime < P2.DateTime);
  if not FirstIs1 then
    FirstIs1 := Self.IndexOf(TrackSeg1) < Self.IndexOf(TrackSeg2);

  if FirstIs1 then
  begin
    if SamePoint then
      si := 1
    else
      si := 0;
  end
  else
  begin
    P1 := TrackSeg1.WayPoints[0];
    P2 := TrackSeg2.WayPoints[TrackSeg2.Count-1];
    SamePoint := (P1.Longitude = P2.Longitude) and
                 (P1.Latitude  = P2.Latitude ) and
                 (P1.Altitude  = P2.Altitude );
    if SamePoint then
      si := 1
    else
      si := 0;
  end;

  if FirstIs1 then
  begin
    for i := si to TrackSeg2.Count-1 do
    begin
      P1 := TGPXwpt.Create;
      P1.CopyFrom(TrackSeg2.WayPoints[i]);
      TrackSeg1.Add(P1);
    end;
  end
  else
  begin
    for i := 0 to TrackSeg2.Count-(1+si) do
    begin
      P1 := TGPXwpt.Create;
      P1.CopyFrom(TrackSeg2.WayPoints[i]);
      TrackSeg1.Insert(i,P1);
    end
  end;
  Track := Self.GetTrackOfTrackSeg(TrackSeg2);
  if Track.Count = 1 then
    Self.Delete(Self.IndexOf(Track))
  else
    Track.Delete(Track.IndexOf(TrackSeg2));

  Result := TrackSeg1;
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

procedure TGPXwptList.SortByDate();
begin
  Self.Sort(@SortPOIByDate);
end;

function SortPointListByDate(PL1,PL2 : Pointer) : integer;
var
  t1,t2 : TPointList2;
begin
  t1 := TPointList2(PL1);
  t2 := TPointList2(PL2);

  if t1.Points[0].DateTime > t2.Points[0].DateTime then
    SortPointListByDate := 1
  else if t1.Points[0].DateTime < t2.Points[0].DateTime then
    SortPointListByDate := -1
  else
    SortPointListByDate := 0;
end;

Procedure TGPXrteList.SortByDate;
begin
  Self.Sort(@SortPointListByDate);
end;

procedure TGPXtrk.SortByDate;
begin
  Self.Sort(@SortPointListByDate);
end;

procedure TGPXMetadata.CopyFrom(Metadata : TGPXMetadata);
begin
  Self.name := Metadata.name;
  Self.desc := Metadata.desc;
  Self.author.Free;
  Self.author := TGPXperson.Create;
  Self.author.CopyFrom(Metadata.author);
  Self.copyright.Free;
  Self.copyright := TGPXCopyright.Create;
  Self.copyright.CopyFrom(Metadata.copyright);
  Self.link.Free;
  Self.link := TGPXLink.Create;
  Self.link.copyFrom(Metadata.link);
  Self.mtime := Metadata.mtime;
  Self.keywords := Metadata.keywords;
  Self.bounds.Free;
  Self.bounds := TGPXBounds.Create;
  Self.bounds.CopyFrom(Metadata.bounds);
  Self.extensions := Metadata.extensions;
  Self.filled := Metadata.filled;
end;

procedure TGPXperson.CopyFrom(Person : TGPXperson);
begin
  Self.name := Person.name;
  Self.email.Free;
  Self.email := TGPXemail.Create;
  Self.email.CopyFrom(Person.email);
  Self.link.Free;
  Self.link := TGPXLink.Create;
  Self.link.CopyFrom(Person.link);
  Self.filled := Person.filled;
end;

procedure TGPXcopyright.CopyFrom(Copyright : TGPXcopyright);
begin
  Self.author := Copyright.author;
  Self.year := Copyright.year;
  Self.license := Copyright.license;
  Self.filled := Copyright.filled;
end;

procedure TGPXbounds.CopyFrom(Bounds : TGPXbounds);
begin
  Self.minlat := Bounds.minlat;
  Self.minlon := Bounds.minlon;
  Self.maxlat := Bounds.maxlat;
  Self.maxlon := Bounds.maxlon;
  Self.filled := Bounds.filled;
end;

procedure TGPXemail.CopyFrom(email : TGPXemail);
begin
  Self.id := email.id;
  Self.domain := email.domain;
  Self.filled := email.filled;
end;

constructor TGPXcopyright.Create;
begin
  inherited;
  Self.year := -1;
  Self.filled := False;
end;

constructor TGPXLink.Create;
begin
  inherited;
  Self.filled := False;
end;

constructor TGPXemail.Create;
begin
  inherited;
  Self.filled := False;
end;

destructor TGPXemail.Destroy;
begin
  inherited;
end;

constructor TGPXbounds.Create;
begin
  inherited;
  Self.filled := False;
end;

procedure TGPX.ComputeMetaBounds;
var
  BBox : TBBox;
  i,j :integer;
begin
  BBox := TBBox.Create;

  Self.wpt.CalcBBox(True);
  BBox.Add(Self.wpt.BBox);

  for i := 0 to Self.rte.Count-1 do
  begin
    Self.rte.Routes[i].CalcBBox(True);
    if Self.rte.Routes[i].BBox.Complete then
      BBox.Add(Self.rte.Routes[i].BBox);
  end;

  for i := 0 to Self.trk.Count-1 do
  begin
    for j := 0 to Self.trk.Tracks[i].Count-1 do
    begin
      Self.trk.Tracks[i].trkseg[j].CalcBBox(True);
      if Self.trk.Tracks[i].trkseg[j].BBox.Complete then
        BBox.Add(Self.trk.Tracks[i].trkseg[j].BBox);
    end;
  end;

  if BBox.E = BBox.O then
    Exit;

  if BBox.N = BBox.S then
    Exit;

  Self.metadata.bounds.minlat := MinFP(BBox.N,BBox.S);
  Self.metadata.bounds.maxlat := MaxFP(BBox.N,BBox.S);
  Self.metadata.bounds.minlon := MinFP(BBox.E,BBox.O);
  Self.metadata.bounds.maxlon := MaxFP(BBox.E,BBox.O);

  Self.metadata.filled := True;
  Self.metadata.bounds.filled := True;

  BBox.Free;
end;

procedure TGPXExtension.CopyFrom(extension : TGPXExtension);
begin
  Self.Text := extension.Text;
end;

constructor TGPXExtension.Create;
begin
  inherited;
  Self.Text := '';
end;

destructor TGPXExtension.Destroy;
begin
  inherited;
end;

procedure TGPX.Clear;
begin
  if Assigned(Self.Selection) then
  begin
    Self.Selection.Clear;
  end;
  If assigned(Self.wpt) then
  begin
    Self.wpt.Clear;
    Self.wpt.filled := False;
  end;
  If assigned(Self.rte) then
  begin
    Self.rte.Clear;
    Self.rte.filled := False;
  end;
  If assigned(Self.trk) then
  begin
    Self.trk.Clear;
    Self.trk.filled := False;
  end;
end;

procedure TGPXOptions.CopyFrom(GPXOptions : TGPXOptions);
begin
  Self.GPX_Style := GPXOptions.GPX_Style;
  Self.Compress := GPXOptions.Compress;
  Self.FileHistory := GPXOptions.FileHistory;
  Self.ClipBoardStyle := GPXOptions.ClipBoardStyle;
  Self.ProxyType := GPXOptions.ProxyType;
  Self.ProxyHttpIP := GPXOptions.ProxyHttpIP;
  Self.ProxyHttpPort := GPXOptions.ProxyHttpPort;
  Self.ProxySocksIP := GPXOptions.ProxySocksIP;
  Self.ProxySocksPort := GPXOptions.ProxySocksPort;
  Self.ProxyMultiIP := GPXOptions.ProxyMultiIP;
  Self.ProxyMultiPort := GPXOptions.ProxyMultiPort;
  Self.ElevationDatabase := GPXOptions.ElevationDatabase;
  Self.GeoNamesUsername := GPXOptions.GeoNamesUsername;
  Self.IgnoreXMLVersion := GPXOptions.IgnoreXMLVersion;
  Self.KeepXMLCRLF := GPXOptions.KeepXMLCRLF;
  Self.DeleteNullData := GPXOptions.DeleteNullData;
  Self.ClearUndoBeforeSave := GPXOptions.ClearUndoBeforeSave;
  Self.PreserveXMLNS := GPXOptions.PreserveXMLNS;
  Self.DefaultFileName  := GPXOptions.DefaultFileName;
  Self.DefaultFileName2 := GPXOptions.DefaultFileName2;
  Self.DefaultFileName3 := GPXOptions.DefaultFileName3;
  Self.DefaultFileName4 := GPXOptions.DefaultFileName4;
  Self.DefaultFileNameNum := GPXOptions.DefaultFileNameNum;
  Self.UseDefaultFileName := GPXOptions.UseDefaultFileName;
end;

constructor TGPXOptions.Create;
begin
  inherited;
  Self.GPX_Style := False;
  Self.Compress := False;
  Self.FileHistory := 8;
  Self.ClipBoardStyle := 0;
  Self.ProxyType := 0;
  Self.ElevationDatabase := 2;
  Self.IgnoreXMLVersion := False;
  Self.KeepXMLCRLF := False;
  Self.UseDefaultFileName := False;
end;

destructor TGPXOptions.Destroy;
begin
  inherited;
end;

procedure TGPXwptList.ClearPointList;
var
  i,c : integer;
begin
  c := Self.Count-1;
  if c >= 0 then
  begin
    for i := c downto 0 do
    begin
      if Assigned(Self.WayPoints[i]) then
        Self.WayPoints[i].Free;
      Self.Delete(i);
//      .FreeAndDelete(i);
    end;
  end;
  Self.Clear;
end;

procedure TGPXtrk.SetEnabled(Value : Boolean);
var
  i : integer;
begin
  Self.penabled := Value;
  For i := 0 to Self.Count-1 do
    Self.trkseg[i].enabled := Value;
end;

procedure TGPXrteList.SetEnabled(Value : Boolean);
var
  i : integer;
begin
  Self.penabled := Value;
  For i := 0 to Self.Count-1 do
    Self.Routes[i].enabled := Value;
end;

procedure TGPXtrkList.SetEnabled(Value : Boolean);
var
  i : integer;
begin
  Self.penabled := Value;
  For i := 0 to Self.Count-1 do
    Self.Tracks[i].enabled := Value;
end;

procedure TGPX.SetEnabled(Value : Boolean);
begin
  Self.penabled := Value;
  Self.wpt.enabled := Value;
  Self.rte.enabled := Value;
  Self.trk.enabled := Value;
end;

function TGPX.GetWaypointParent(waypoint : TGPXwpt) : TGPXwptList;
var
  i, j : integer;
begin
  Result := nil;
  if Self.wpt.IndexOf(waypoint)>=0 then
  begin
    Result := Self.wpt;
    Exit;
  end;
  For i := 0 to Self.rte.Count-1 do
  begin
    if Self.rte.Routes[i].IndexOf(waypoint)>=0 then
    begin
      Result := Self.rte.Routes[i];
      Exit;
    end;
  end;
  For i := 0 to Self.trk.Count-1 do
  begin
    for j := 0 to Self.trk.Tracks[i].Count-1 do
    begin
      if Self.trk.Tracks[i].trkseg[j].IndexOf(waypoint)>=0 then
      begin
        Result := Self.trk.Tracks[i].trkseg[j];
        Exit;
      end;
    end;
  end;
end;

procedure TGPXWptList.RemoveDuplicates(FilterMode : byte = 0; SortBefore : boolean = True);
var
  i : integer;
  wpt1,wpt2 : TGPXwpt;
begin
  if Sortbefore then
    Self.SortByDate;

  for i := Self.Count-1 downto 1 do
  begin
    wpt1 := Self.WayPoints[i];
    wpt2 := Self.WayPoints[i-1];

    if wpt1.IsIdentical(wpt2,FilterMode) then
    begin
      Self.Delete(i);
      wpt1.Free;
    end;
  end;
end;

function TGPXWpt.IsIdentical(WayPoint : TGPXwpt; FilterMode : byte = 0) : Boolean;
begin
  Result := True;
  if Self.Latitude <> WayPoint.Latitude then
  begin
    Result := False;
    Exit;
  end;
  if Self.Longitude  <> WayPoint.Longitude then
  begin
    Result := False;
    Exit;
  end;
  if Self.Altitude <> WayPoint.Altitude then
  begin
    Result := False;
    Exit;
  end;

  if FilterMode >= 1  then
    Exit;

  if Self.DateTime <> WayPoint.DateTime then
  begin
    Result := False;
    Exit;
  end;

  if FilterMode >= 2  then
    Exit;

  if Self.magvar <> WayPoint.magvar then
  begin
    Result := False;
    Exit;
  end;
  if Self.geoidheight <> WayPoint.geoidheight then
  begin
    Result := False;
    Exit;
  end;
  if Self.name <> WayPoint.name then
  begin
    Result := False;
    Exit;
  end;
  if Self.cmt <> WayPoint.cmt then
  begin
    Result := False;
    Exit;
  end;
  if Self.desc <> WayPoint.desc then
  begin
    Result := False;
    Exit;
  end;
  if Self.src <> WayPoint.src then
  begin
    Result := False;
    Exit;
  end;
  {
  if Self.link <> WayPoint.link then
  begin
    Result := False;
    Exit;
  end;
    link : TGPXLink;
  }
  if Self.sym <> WayPoint.sym then
  begin
    Result := False;
    Exit;
  end;
  if Self.wtype <> WayPoint.wtype then
  begin
    Result := False;
    Exit;
  end;
  if Self.fix <> WayPoint.fix then
  begin
    Result := False;
    Exit;
  end;
  if Self.sat <> WayPoint.sat then
  begin
    Result := False;
    Exit;
  end;
  if Self.hdop <> WayPoint.hdop then
  begin
    Result := False;
    Exit;
  end;
  if Self.vdop <> WayPoint.vdop then
  begin
    Result := False;
    Exit;
  end;
  if Self.pdop <> WayPoint.pdop then
  begin
    Result := False;
    Exit;
  end;
  if Self.ageofdgpsdata <> WayPoint.ageofdgpsdata then
  begin
    Result := False;
    Exit;
  end;
  if Self.dgpsid <> WayPoint.dgpsid then
  begin
    Result := False;
    Exit;
  end;
  if Self.extensions <> WayPoint.extensions then
  begin
    Result := False;
    Exit;
  end;
end;

function TGPXTrk.GetOldestDateTime() : TDateTime;
var
  t,d : TDateTime;
  i : integer;
begin
  t := 0;
  try
    for i := 0 to Self.Count-1 do
    begin
      d := Self.TrkSeg[i].GetOldestDateTime;
      if t < d then
        if d > 0 then
          t := d;
    end;
  finally
    Result := t;
  end;
end;

function TGPXTrk.GetNewestDateTime() : TDateTime;
var
  t,d : TDateTime;
  i : integer;
begin
  t := MAXDATETIME; //200*265*24*60*60
  try
    for i := 0 to Self.Count-1 do
    begin
      d := Self.TrkSeg[i].GetNewestDateTime;
      if t > d then
        if d > 0 then
          t := d;
    end;
  finally
    Result := t;
  end;
end;

function TGPXTrkList.GetOldestDateTime() : TDateTime;
var
  t,d : TDateTime;
  i : integer;
begin
  t := 0;
  try
    for i := 0 to Self.Count-1 do
    begin
      d := Self.Tracks[i].GetOldestDateTime;
      if t < d then
        if d > 0 then
          t := d;
    end;
  finally
    Result := t;
  end;
end;

function TGPXTrkList.GetNewestDateTime() : TDateTime;
var
  t,d : TDateTime;
  i : integer;
begin
  t := MAXDATETIME; //200*265*24*60*60
  try
    for i := 0 to Self.Count-1 do
    begin
      d := Self.Tracks[i].GetNewestDateTime;
      if t > d then
        if d > 0 then
          t := d;
    end;
  finally
    Result := t;
  end;
end;

function TGPXRteList.GetOldestDateTime() : TDateTime;
var
  t,d : TDateTime;
  i : integer;
begin
  t := 0;
  try
    for i := 0 to Self.Count-1 do
    begin
      d := Self.Routes[i].GetOldestDateTime;
      if t < d then
        if d > 0 then
          t := d;
    end;
  finally
    Result := t;
  end;
end;

function TGPXRteList.GetNewestDateTime() : TDateTime;
var
  t,d : TDateTime;
  i : integer;
begin
  t := MAXDATETIME; //200*265*24*60*60
  try
    for i := 0 to Self.Count-1 do
    begin
      d := Self.Routes[i].GetNewestDateTime;
      if t > d then
        if d > 0 then
          t := d;
    end;
  finally
    Result := t;
  end;
end;

end.
