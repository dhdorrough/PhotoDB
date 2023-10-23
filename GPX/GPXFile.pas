{$R-}
unit GPXFile;

// Contains all that is needed to load a gpxfile into a TGPX object,
// and to save such object into a file

interface

uses
  Classes, SysUtils, Graphics, StdCtrls,
  XpBase, XpDOM, Fastcode, StrUtils,
  GPS, GPX, GPX_Style, DateUtils, gnugettext;

type
  TFilterFunc = procedure {name}(anObject: TObject; var Accept: boolean) of object;

  TGPXFile = Class(Tobject)
  private
    fFilterFunc: TFilterFunc;
    procedure ImportGPXfDOM(var fDOM : TXpObjModel);
    function ImportGPXlink(XNode : TXpNode) : TGPXLink;
    function ImportGPXperson(XNode : TXpNode) : TGPXperson;
    function ImportGPXemail(XNode : TXpNode) : TGPXemail;
    function ImportGPXcopyright(XNode : TXpNode) : TGPXcopyright;
    function ImportGPXbounds(XNode : TXpNode) : TGPXbounds;

    procedure ImportXMLNS(XNode : TXpNode);
    procedure ImportGPXmetadata(XNode : TXpNode);
    procedure ImportGPXWPT(XNode : TXpNode; ToList : TPointList2);
    procedure ImportGPXRTE(XNode : TXpNode);
    procedure ImportGPXTRK(XNode : TXpNode);
    procedure ImportGPXextensions(XNode : TXpNode);

    function ExportGPXmetadata(meta : TGPXmetadata) : String;
    function ExportGPXWPT(WayPoint : TGPXwpt; Header : String ='wpt'; TabCount : integer = 0) : String;
    function ExportGPXRTE(Route : TGPXrte) : String;
    function ExportGPXTRK(Track : TGPXtrk) : String;

    function ExportGPXlink(link : TGPXlink; TabCount : integer = 0) : String;

    procedure AddLog(Text : String);
  published
    GPXd : TGPX;
    Function GetLastNodeText(Parent : TXpNode; NodeName : String) : String;
    Function GetExtensionNodeContent(Parent : TXpNode; NodeName : String) : String;
    function DateTimeToGPXTime(Value : TDateTime) : String;
    Function ISO8601ToDateTime(Value : String) : TDateTime;
    Function XMLise(s : String): String;
    function ExportGPXWPTList(WayptList : TGPXwptList) : String;
    function ExportGPXRTEList(RouteList : TGPXrteList) : String;
    function ExportGPXTRKList(TrackList : TGPXtrkList) : String;
    function WriteText(Text,Tag : string; TabCount : integer = 0; Ext : Boolean = False) : String;
    function WriteInt(Value : integer; Tag : string; Default : integer = 0; TabCount : integer = 0) : String;
    function WriteTime(Value : TDateTime; Tag : string; Default : integer = 0; TabCount : integer = 0) : String;
    function WriteDouble(Value : double; Tag : string; Default : integer = 0; TabCount : integer = 0) : String;
  public
    FileName : String;
    Loaded : Boolean;
    Modified : Boolean;

    mLog : TStringList;
    FmtS : TFormatSettings;

    GPXOptions : TGPXOptions;

    procedure CopyFrom(GPXFile : TGPXFile);
    constructor Create;
    destructor Destroy; override;

    procedure ImportGPXFile(FileName : String);
    procedure ImportGPXString(GPXString : String);
    procedure ExportGPX(Filename : String);

    property OnFilterFunc: TFilterFunc
             read fFilterFunc
             write fFilterFunc;
  end;

var
  CRLF : String; //= #13#10;
  TAB : String; //= #09;

implementation

procedure TGPXFile.AddLog(Text : String);
begin
  mLog.Add(Text);
end;

function TGPXFile.GetLastNodeText(Parent : TXpNode; NodeName : String) : String;
var
  XList : TXpNodeList;
begin
  Result := '';
  XList := Parent.SelectNodes(NodeName);
  if XList.Length > 0 then
  begin
    Result := Xlist.Last.Text;
  end;
  XList.Free;
end;

Function TGPXFile.GetExtensionNodeContent(Parent : TXpNode; NodeName : String) : String;
var
  XList : TXpNodeList;
  s : String;
begin
  Result := '';
    XList := Parent.SelectNodes(NodeName);
    if XList.Length > 0 then
    begin
      s := Trim(Xlist.Last.XmlDocument);
      if length(s) > 0 then
      begin
        if pos('<'+NodeName+'>',s) > 0 then
        begin
          s := copy(s,pos('<'+NodeName+'>',s)+length('<'+NodeName+'>'),length(s));
          s := copy(s,1,pos('</'+NodeName+'>',s)-1);
        end;
      end;
      Result := s;
    end;
    XList.Free;
end;

Function TGPXFile.ISO8601ToDateTime(Value : String) : TDateTime;
var
  prt : string;
  dpart, tpart, tzpart : String;
  Quand : TDateTime;
  y,m,d,h,n,s,ms : word;
  w : integer;
  tzh,tzm : integer;  // time zone bias if not zulu time
  tzpositive : boolean;
  dcm : Char;
  f : Double;
begin
  // 2004-11-26T14:42:38Z
  // 2004-11-26 14:42:38+0100
  // 2004-W19-2 14:42:38.010+03:30
  // See http://en.wikipedia.org/wiki/ISO_8601

  Result := 0;
  if length(Value) < 8 then
    Exit;

  y := 1900;
  m := 1;
  d := 1;
  n := 0;
  s := 1;
  ms := 0;
  tzh := 0;
  tzm := 0;
  tzpositive := True;

  Value := Trim(Value);

  if pos('T',Value)>0 then
  begin
    dpart := copy(Value,1,pos('T',Value)-1);
    tpart := copy(Value,pos('T',Value)+1,length(Value));
  end
  else if pos(' ',Value)>0 then
  begin
    dpart := copy(Value,1,pos(' ',Value)-1);
    tpart := copy(Value,pos(' ',Value)+1,length(Value));
  end
  else
  begin
    dpart := Value;
    tpart := '';
  end;

  if length(dpart) < 4 then
    Exit;

  if pos('Z',tpart)>0 then
  begin
    tpart := copy(tpart,1,pos('Z',tpart)-1);
    tzpart := 'Z';
  end else if pos('+',tpart)>0 then
  begin
    tzpart := copy(tpart,pos('+',tpart),length(tpart));
    tpart := copy(tpart,1,pos('+',tpart)-1);
  end else if pos('-',tpart)>0 then
  begin
    tzpart := copy(tpart,pos('-',tpart),length(tpart));
    tpart := copy(tpart,1,pos('-',tpart)-1);
  end else
  begin
    tzpart := '';
  end;

  dcm := FmtS.DecimalSeparator;

  if pos(',',tpart) > 0 then
    tpart := StringReplace(tpart,',',dcm,[]);

  //the standard also permits the expansion of the year representation [±YYYYY], but only by prior agreement between the sender and the receiver.
  // so in GPX it will ALWAYS begin with YYYY

  prt := copy(dpart,1,4);
  y := StrToIntDef(prt,1900);
  dpart := copy(dpart,5,length(dpart));
  if length(dpart) > 0 then // there is more than just a year
  begin
    if dpart[1] = '-' then
      dpart := copy(dpart,2,length(dpart)-1);

    if (length(dpart) = 3) and (dpart[1]<>'W') then // ordinal date YYYYDDD & YYYY-DDD
    begin
      d := StrTointDef(dpart,1);
      Quand := EncodeDateDay(y,d);
      DecodeDate(Quand,y,m,d);
    end else if (dpart[1]='W') then // Week date YYYYWww or YYYY-Www & YYYY-Www-D or YYYYWwwD
    begin
      dpart := copy(dpart,2,length(dpart)-1);
      prt := copy(dpart,1,2);
      w := StrToIntDef(prt,1);
      d := 1;
      if length(dpart) > length(prt) then // YYYY-Www-D or YYYYWwwD
      begin
        dpart := copy(dpart,3,length(dpart));
        if dpart[1] = '-' then
          dpart := copy(dpart,2,length(dpart)-1);
        d := StrToIntDef(dpart,1);
      end;
      Quand := EncodeDateWeek(y,w,d);
      DecodeDate(Quand,y,m,d);
    end else // YYYY-MM-DD or YYYYMMDD or YYYY-MM
    begin
      prt := copy(dpart,1,2);
      m := StrToIntDef(prt,1);
      if length(dpart) > length(prt) then // YYYY-MM-DD or YYYYMMDD
      begin
        dpart := copy(dpart,3,length(dpart));
        if dpart[1] = '-' then
          dpart := copy(dpart,2,length(dpart)-1);
        d := StrToIntDef(dpart,1);
      end;
    end
  end;

  prt := copy(tpart,1,2);
  h := StrToIntDef(prt,0);
  tpart := copy(tpart,3,length(tpart));
  if length(tpart)>1 then // hh:mm:ss or hhmmss & hh:mm or hhmm & hh & hh.xxxx
  begin
    if tpart[1] = ':' then
      tpart := copy(tpart,2,length(tpart)-1);

    if tpart[1] = dcm then // hh.xxxx
    begin
      f := StrToFloatDef('0'+tpart,0,FmtS);
      f := f * 60;
      n := trunc(f);
      f := f - n;
      f := f * 60;
      s := trunc(f);
      f := f - s;
      f := f *1000;
      ms := trunc(f);
    end else
    begin
      prt := copy(tpart,1,2);
      n := StrToIntDef(prt,0);
      tpart := copy(tpart,3,length(tpart));

      if length(tpart)>1 then // hh:mm:ss or hhmmss & hh:mm.xxxx or hhmm.xxxx
      begin
        if tpart[1] = ':' then
          tpart := copy(tpart,2,length(tpart)-1);

        if tpart[1] = dcm then // hh:mm.xxxx
        begin
          f := StrToFloatDef('0'+tpart,0,FmtS);
          f := f * 60;
          s := trunc(f);
          f := f - s;
          f := f *1000;
          ms := trunc(f);
        end else
        begin
          prt := copy(tpart,1,2);
          s := StrToIntDef(prt,0);
          tpart := copy(tpart,3,length(tpart));

          if length(tpart)>1 then // hh:mm:ss.xxx or hhmmss.xxx
          begin
            if tpart[1] = dcm then // hh.xxxx
            begin
              f := StrToFloatDef('0'+tpart,0,FmtS);
              f := f *1000;
              ms := trunc(f);
            end;
          end;
        end;
      end;
    end;
  end;

  if length(tzpart) < 3 then // nothing or Z
  begin
    tzpart := '';
  end
  else // ±hh:mm or ±hhmm & ±hh
  begin
    tzpositive := not (tzpart[1] = '-');

    tzpart := copy(tzpart,2,length(tzpart)-1);
    prt := copy(tzpart,1,2);
    tzh := StrToIntDef(prt,0);
    if length(tzpart) > length(prt) then // ±hh:mm or ±hhmm
    begin
      tzpart := copy(tzpart,3,length(tzpart)-1);
      if tzpart[1] = ':' then
        tzpart := copy(tzpart,2,length(tzpart)-1);
      tzm := StrToIntDef(tzpart,0);
    end;
  end;

  if tzpositive then
    Quand := EncodeDate(y,m,d) + EncodeTime(h,n,s,ms) + EncodeTime(tzh,tzm,0,0)
  else
    Quand := EncodeDate(y,m,d) + EncodeTime(h,n,s,ms) - EncodeTime(tzh,tzm,0,0);

  Result := Quand;
end;

Function TGPXFile.XMLise(s : String): String;
begin
  s := FastcodeAnsiStringReplace(s,'&','&amp;',[rfReplaceAll, rfIgnoreCase]);
  s := FastcodeAnsiStringReplace(s,'<','&lt;',[rfReplaceAll, rfIgnoreCase]);
  s := FastcodeAnsiStringReplace(s,'>','&gt;',[rfReplaceAll, rfIgnoreCase]);
  XMLise := s;
end;

function TGPXFile.WriteText(Text,Tag : string; TabCount : integer = 0; Ext : Boolean = False) : String;
var
  TabC : String;
begin
  if Length(TAB) > 0 then
    TabC := StringOfChar(TAB[1],TabCount)
  else
    TabC := '';
  Result := '';
  if length(Text) > 0 then
    if Ext then
      Result := TabC + '<'+Tag+'>'+Text+'</'+Tag+'>'+CRLF
    else
      Result := TabC + '<'+Tag+'>'+XMLise(Text)+'</'+Tag+'>'+CRLF;
end;

function TGPXFile.WriteInt(Value : integer; Tag : string; Default : integer = 0; TabCount : integer = 0) : String;
var
  TabC : String;
begin
  if Length(TAB) > 0 then
    TabC := StringOfChar(TAB[1],TabCount)
  else
    TabC := '';
  Result := '';
  if Value <> Default then
    Result := TabC + '<'+Tag+'>'+IntToStr(Value)+'</'+Tag+'>'+CRLF;
end;

function TGPXFile.WriteTime(Value : TDateTime; Tag : string; Default : integer = 0; TabCount : integer = 0) : String;
var
  TabC : String;
begin
  if Length(TAB) > 0 then
    TabC := StringOfChar(TAB[1],TabCount)
  else
    TabC := '';
  Result := '';
  if Value <> Default then
    Result := TabC + '<'+Tag+'>'+DateTimeToGPXTime(Value)+'</'+Tag+'>'+CRLF;
end;

function TGPXFile.WriteDouble(Value : double; Tag : string; Default : integer = 0; TabCount : integer = 0) : String;
var
  TabC : String;
begin
  if Length(TAB) > 0 then
    TabC := StringOfChar(TAB[1],TabCount)
  else
    TabC := '';
  Result := '';
  if Value <> Default then
    Result := TabC + '<'+Tag+'>'+FloatToStr(Value,FmtS)+'</'+Tag+'>'+CRLF;
end;

constructor TGPXFile.Create;
begin
  inherited;
  mLog := TStringList.Create;
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';
  Modified := False;
  GPXOptions := TGPXOptions.Create;
  CRLF := #13#10;
  TAB := #09;
end;

destructor TGPXFile.Destroy;
begin
  mLog.Free;
  GPXd := nil;
  GPXOptions.Free;
  inherited;
end;

procedure TGPXFile.ImportGPXFile(FileName : String);
var
  fDOM : TXpObjModel;
begin
  fDOM := TXpObjModel.Create(nil);
  fDOM.IgnoreCase := True;
  fDOM.LineBreakMode := lbmDefault;
  fDOM.KeepLF := Self.GPXOptions.KeepXMLCRLF;
  fDOM.IgnoreXMLVersion := Self.GPXOptions.IgnoreXMLVersion;
  try
    fDOM.LoadDataSource(FileName);
  except
    on E: Exception do
    begin
      AddLog(E.Message + ' (LoadDataSource' + IntToStr(E.HelpContext) + ').');
      fDom.Free;
      Exit;
    end;
  end;

  ImportGPXfDOM(fDOM);
end;

procedure TGPXFile.ImportGPXString(GPXString : String);
var
  fDOM : TXpObjModel;
  SS : TStringStream;
begin
  fDOM := TXpObjModel.Create(nil);
  fDOM.IgnoreCase := True;
  fDOM.LineBreakMode := lbmDefault;
  fDOM.KeepLF := Self.GPXOptions.KeepXMLCRLF;
  fDOM.IgnoreXMLVersion := Self.GPXOptions.IgnoreXMLVersion;
  SS := TStringStream.Create(GPXString);
  try
    fDOM.LoadStream(SS);
  except
    on E: Exception do
    begin
      AddLog(E.Message + ' (' + IntToStr(E.HelpContext) + ').');
      fDom.Free;
      SS.Free;
      Exit;
    end;
  end;
  SS.Free;
  ImportGPXfDOM(fDOM);
end;

procedure TGPXFile.ImportGPXfDOM(var fDOM : TXpObjModel);
var
  XRoot, XNode, XGPXRoot : TXpNode;
  i,j,c,t : integer;
  NodeName : String;
begin
  XRoot := fDOM.Document;
  XGPXRoot := nil;

  for i := 0 to XRoot.ChildNodes.Length - 1 do
  begin
    XNode := XRoot.ChildNodes.Item(i);
    //Log(XNode.NodeName);
    if Lowercase(XNode.NodeName) = 'gpx' then
      XGPXRoot := XNode;
  end;

  if XGPXRoot = nil then
  begin
    AddLog(trim(fDOM.Errors.Text));
    AddLog(_('File not loaded')); // LOG ImportGPX no GPXRoot after load
    fDOM.Free;
    Exit;
  end;

  ImportXMLNS(XGPXRoot);

  for i := 0 to XGPXRoot.ChildNodes.Length - 1 do
  begin
    XNode := XGPXRoot.ChildNodes.Item(i);
    NodeName := Lowercase(XNode.NodeName);
    //Log(XNode.NodeName);
    if NodeName = 'metadata' then
      ImportGPXmetadata(XNode)
    else if NodeName = 'wpt' then
      ImportGPXwpt(XNode, Self.GPXd.wpt)
    else if NodeName = 'rte' then
      ImportGPXrte(XNode)
    else if NodeName = 'trk' then
      ImportGPXtrk(XNode)
    else if NodeName = 'extensions' then
      ImportGPXextensions(XNode);
  end;

  fDOM.Free;

  AddLog(IntToStr(Self.GPXd.wpt.Count) + ' '+_('waypoints'));  // LOG ImportGPX waypoint count
  c := 0;
  for i := 0 to Self.GPXd.rte.Count-1  do
  begin
    c := c + Self.GPXd.rte.Routes[i].Count;
  end;
  AddLog(IntToStr(Self.GPXd.rte.Count) + ' '+_('routes')+' ('+IntToStr(c)+' '+('points)')+')');  // LOG ImportGPX route count
  c := 0;
  t := 0;
  for i := 0 to Self.GPXd.trk.Count-1  do
  begin
    t := t + Self.GPXd.trk.Tracks[i].Count;
    for j := 0 to Self.GPXd.trk.Tracks[i].Count-1 do
    begin
      c := c + Self.GPXd.trk.Tracks[i].trkseg[j].Count;
    end;
  end;
  AddLog(IntToStr(Self.GPXd.trk.Count) + ' '+_('tracks')+' ('+IntToStr(c)+' '+_('points in')+' '+IntToStr(t)+' '+_('track segments')+')');  // LOG ImportGPX track count
  Self.Loaded := True;
end;

function TGPXFile.ImportGPXbounds(XNode : TXpNode) : TGPXbounds;
var
  Bounds : TGPXbounds;
begin
  Bounds := TGPXbounds.Create;
  try
    if Assigned(XNode.Attributes) then
    begin
      if Assigned(XNode.Attributes.GetNamedItem('minlat')) then
        Bounds.minlat := StrToFloatDef(XNode.Attributes.GetNamedItem('minlat').NodeValue,0,FmtS);
      if Assigned(XNode.Attributes.GetNamedItem('minlon')) then
        Bounds.minlon := StrToFloatDef(XNode.Attributes.GetNamedItem('minlon').NodeValue,0,FmtS);
      if Assigned(XNode.Attributes.GetNamedItem('maxlat')) then
        Bounds.maxlat := StrToFloatDef(XNode.Attributes.GetNamedItem('maxlat').NodeValue,0,FmtS);
      if Assigned(XNode.Attributes.GetNamedItem('maxlon')) then
        Bounds.maxlon := StrToFloatDef(XNode.Attributes.GetNamedItem('maxlon').NodeValue,0,FmtS);
      Bounds.filled := True;
    end;
  finally
    Result := Bounds;
  end;
end;

function TGPXFile.ImportGPXcopyright(XNode : TXpNode) : TGPXcopyright;
var
  Copyright : TGPXcopyright;
begin
  Copyright := TGPXcopyright.Create;
  try
    if Assigned(XNode.Attributes) then
      if Assigned(XNode.Attributes.GetNamedItem('author')) then
        Copyright.author := XNode.Attributes.GetNamedItem('author').NodeValue;
    Copyright.year := StrToIntDef(GetLastNodeText(Xnode,'year'),-1);
    Copyright.license := GetLastNodeText(Xnode,'license');
    Copyright.filled := True;
  finally
    Result := Copyright;
  end;
end;

function TGPXFile.ImportGPXemail(XNode : TXpNode) : TGPXemail;
var
  Email : TGPXemail;
begin
  Email := TGPXemail.Create;
  try
    Email.id := GetLastNodeText(Xnode,'id');
    Email.domain := GetLastNodeText(Xnode,'domain');
    Email.filled := True;
  finally
    Result := Email;
  end;
end;

function TGPXFile.ImportGPXperson(XNode : TXpNode) : TGPXperson;
var
  Person : TGPXperson;
  XnList : TXpNodeList;
begin
  Person := TGPXperson.Create;
  try
    Person.name := GetLastNodeText(Xnode,'name');
    if (Length(Person.name)=0) and (length(XNode.Text)>0) then
      Person.name := XNode.Text;
    XnList := XNode.SelectNodes('email');
    if XnList.Length > 0 then
    begin
      if Assigned(Person.email) then
        Person.email.Free;
      Person.email := ImportGPXemail(XnList.Last);
    end;
    XnList.Free;
    XnList := XNode.SelectNodes('link');
    if XnList.Length > 0 then
    begin
      if Assigned(Person.link) then
        Person.link.Free;
      Person.link := ImportGPXLink(XnList.Last);
    end;
    XnList.Free;
    Person.filled := True;
  finally
    Result := Person;
  end;
end;

procedure TGPXFile.ImportGPXmetadata(XNode : TXpNode);
var
  Meta : TGPXmetadata;
  XnList : TXpNodeList;
begin
  Meta := TGPXmetadata.Create;
  try
    Meta.name := GetLastNodeText(Xnode,'name');
    Meta.desc := GetLastNodeText(Xnode,'desc');
    XnList := XNode.SelectNodes('author');
    if XnList.Length > 0 then
    begin
      if Assigned(Meta.author) then
        Meta.author.Free;
      Meta.author := ImportGPXperson(XnList.Last);
    end;
    XnList.Free;
    XnList := XNode.SelectNodes('copyright');
    if XnList.Length > 0 then
    begin
      if Assigned(Meta.copyright) then
        Meta.copyright.Free;
      Meta.copyright := ImportGPXcopyright(XnList.Last);
    end;
    XnList.Free;
    XnList := XNode.SelectNodes('link');
    if XnList.Length > 0 then
    begin
      if Assigned(Meta.link) then
        Meta.link.Free;
      Meta.link := ImportGPXLink(XnList.Last);
    end;
    XnList.Free;
    Meta.mtime := ISO8601ToDateTime(GetLastNodeText(Xnode,'time'));
    Meta.keywords := GetLastNodeText(Xnode,'keywords');
    XnList := XNode.SelectNodes('bounds');
    if XnList.Length > 0 then
    begin
      if Assigned(Meta.bounds) then
        Meta.bounds.Free;
      Meta.bounds := ImportGPXbounds(XnList.Last);
    end;
    XnList.Free;
    Meta.extensions := GetExtensionNodeContent(Xnode,'extensions');
    Meta.filled := true;
  finally
    if Assigned(Self.GPXd.metadata) then
      Self.GPXd.metadata.Free;
    Self.GPXd.metadata := Meta;
  end;
end;

function TGPXFile.ImportGPXLink(XNode : TXpNode) : TGPXLink;
var
  URI : String;
  Link : TGPXLink;
begin
  Link := TGPXLink.Create;
  try
    URI := '';
    if Assigned(XNode.Attributes) then
      URI := XNode.Attributes.GetNamedItem('href').NodeValue;
    if length(URI)>0 then
      Link.href := URI
    else
      Link.href := XNode.Text;
    Link.text := GetLastNodeText(Xnode,'text');
    Link.ltype := GetLastNodeText(Xnode,'type');
    Link.filled := True;
  finally
    Result := Link;
  end;
end;

procedure TGPXFile.ImportGPXWPT(XNode : TXpNode; ToList : TPointList2);
var
  WayPoint : TGPXwpt;
  s : String;
  XLnList : TXpNodeList;
begin
  WayPoint := TGPXwpt.Create;
  try
    if Assigned(XNode.Attributes) then
    begin
      s := XNode.Attributes.GetNamedItem('lat').NodeValue;
      WayPoint.Latitude := StrToFloatDef(s,0,FmtS);
      s := XNode.Attributes.GetNamedItem('lon').NodeValue;
      WayPoint.Longitude := StrToFloatDef(s,0,FmtS);
    end;

    WayPoint.DateTime := ISO8601ToDateTime(GetLastNodeText(Xnode,'time'));
    WayPoint.Altitude := StrToFloatDef(GetLastNodeText(Xnode,'ele'),0,FmtS);

    WayPoint.magvar := StrToFloatDef(GetLastNodeText(Xnode,'magvar'),0);
    WayPoint.geoidheight := StrToFloatDef(GetLastNodeText(Xnode,'geoidheight'),0);
    WayPoint.name := GetLastNodeText(Xnode,'name');
    WayPoint.cmt := GetLastNodeText(Xnode,'cmt');
    WayPoint.desc := GetLastNodeText(Xnode,'desc');
    WayPoint.src := GetLastNodeText(Xnode,'src');
    XLnList := XNode.SelectNodes('link');
    if XLnList.Length > 0 then
    begin
      if Assigned(WayPoint.link) then
        WayPoint.link.Free;
      WayPoint.link := ImportGPXLink(XLnList.Last);
    end;
    XLnList.Free;
    WayPoint.sym := GetLastNodeText(Xnode,'sym');
    WayPoint.wtype := GetLastNodeText(Xnode,'type');
    WayPoint.fix := LowerCase(GetLastNodeText(Xnode,'fix'));
    WayPoint.sat := StrToIntDef(GetLastNodeText(Xnode,'sat'),-1);
    WayPoint.hdop := StrToFloatDef(GetLastNodeText(Xnode,'hdop'),-1,FmtS);
    if (WayPoint.hdop = 0) and Self.GPXOptions.DeleteNullData then
      WayPoint.hdop := -1;
    WayPoint.vdop := StrToFloatDef(GetLastNodeText(Xnode,'vdop'),-1,FmtS);
    if (WayPoint.vdop = 0) and Self.GPXOptions.DeleteNullData then
      WayPoint.vdop := -1;
    WayPoint.pdop := StrToFloatDef(GetLastNodeText(Xnode,'pdop'),-1,FmtS);
    if (WayPoint.pdop = 0) and Self.GPXOptions.DeleteNullData then
      WayPoint.pdop := -1;
    WayPoint.ageofdgpsdata := StrToFloatDef(GetLastNodeText(Xnode,'ageofdgpsdata'),-1,FmtS);
    WayPoint.dgpsid := StrToIntDef(GetLastNodeText(Xnode,'dgpsid'),-1);
    WayPoint.extensions := GetExtensionNodeContent(Xnode,'extensions');
  finally
    ToList.Add(WayPoint);
    if ToList.Count > 0 then
        ToList.filled := true;
  end;
end;

procedure TGPXFile.ImportGPXRTE(XNode : TXpNode);
var
  Route : TGPXrte;
  XPtList : TXpNodeList;
  XnList : TXpNodeList;
  SubList : TXpNodeList;
  rtept : TXpNode;
  SubNode : TXpNode;
  i : integer;
  s : String;
begin
  Route := TGPXrte.Create;
  try
    Route.name := GetLastNodeText(Xnode,'name');
    Route.cmt := GetLastNodeText(Xnode,'cmt');
    Route.desc := GetLastNodeText(Xnode,'desc');
    Route.src := GetLastNodeText(Xnode,'src');
    XnList := XNode.SelectNodes('link');
    if XnList.Length > 0 then
    begin
      If Assigned(Route.link) then
        Route.link.Free;
      Route.link := ImportGPXLink(XnList.Last);
    end;
    XnList.Free;
    Route.number := StrToIntDef(GetLastNodeText(Xnode,'number'),-1);
    Route.rtype :=  GetLastNodeText(Xnode,'type');

    //GPX 1.0  deprecated format - SHOULD NOT BE USED
    Route.LineStyle.ColorStr := GetLastNodeText(Xnode,'topografix:color');

    SubList := XNode.SelectNodes('extensions');
    if SubList.Length > 0 then
    begin
      Route.extensions  := '';
      SubNode := SubList.Last;
      SubList.Free;
      SubList := SubNode.ChildNodes;
      for i := 0 to SubList.Length -1 do
      begin
        SubNode := SubList.Item(i);
        if SubNode.NodeName = 'line' then
        begin
          s := GetLastNodeText(SubNode,'color');
          if length(s)>0 then
            Route.LineStyle.ColorStr := s;
          s := GetLastNodeText(SubNode,'opacity');
          if length(s)>0 then
            Route.LineStyle.OpacityStr := s;
        end
        else
        begin
          s := SubNode.XmlDocument;
          Route.extensions := Route.extensions + CRLF + s;
        end;
      end;
      Route.extensions := Trim(Route.extensions);
    end
    else
      SubList.Free;
    {
    if GPXOptions.GPX_Style and ( length(GetLastNodeText(Xnode,'extensions/line')) > 0 ) then
    begin
      s := GetLastNodeText(Xnode,'extensions/line/color');
      if length(s)>0 then
        Route.LineStyle.ColorStr := s;
      s := GetLastNodeText(Xnode,'extensions/line/opacity');
      if length(s)>0 then
        Route.LineStyle.OpacityStr := s;
    end
    else
      Route.extensions := GetExtensionNodeContent(Xnode,'extensions');
    }
    if (length(Route.Name) = 0) and (length(Route.Desc) > 0) then
      Route.Name := Route.Desc;

    XPtList := XNode.SelectNodes('rtept');
    for i := 0 to XPtList.Length -1 do
    begin
      rtept := XPtList.Item(i);
      ImportGPXwpt(rtept, Route);
    end;
    XPtList.Free;

  finally
    Self.GPXd.rte.add(Route);
    if Self.GPXd.rte.Count > 0 then
        Self.GPXd.rte.filled := true;
  end;
end;

procedure TGPXFile.ImportGPXTRK(XNode : TXpNode);
var
  Track : TGPXtrk;
  trkseg : TGPXtrkseg;
  XtrksegList, XptList : TXpNodeList;
  XnList : TXpNodeList;
  tsNode,tpNode : TXpNode;
  i,j : integer;
  s : String;
  SubList : TXpNodeList;
  SubNode : TXpNode;
  Accept: boolean;
begin
  Track := TGPXtrk.Create;
  try
    Track.name := GetLastNodeText(Xnode,'name');
    Track.cmt := GetLastNodeText(Xnode,'cmt');
    Track.desc := GetLastNodeText(Xnode,'desc');
    Track.src := GetLastNodeText(Xnode,'src');
    XnList := XNode.SelectNodes('link');
    if XnList.Length > 0 then
    begin
      if Assigned(Track.link) then
        Track.link.Free;
      Track.link := ImportGPXLink(XnList.Last);
    end;
    XnList.Free;
    Track.number := StrToIntDef(GetLastNodeText(Xnode,'number'),-1);
    Track.ttype :=  GetLastNodeText(Xnode,'type');

    //GPX 1.0  deprecated format - SHOULD NOT BE USED
    Track.LineStyle.ColorStr := GetLastNodeText(Xnode,'topografix:color');

    SubList := XNode.SelectNodes('extensions');
    if SubList.Length > 0 then
    begin
      Track.extensions  := '';
      SubNode := SubList.Last;
      SubList.Free;
      SubList := SubNode.ChildNodes;
      for i := 0 to SubList.Length -1 do
      begin
        SubNode := SubList.Item(i);
        if SubNode.NodeName = 'line' then
        begin
          s := GetLastNodeText(SubNode,'color');
          if length(s)>0 then
            Track.LineStyle.ColorStr := s;
          s := GetLastNodeText(SubNode,'opacity');
          if length(s)>0 then
            Track.LineStyle.OpacityStr := s;
        end
        else
        begin
          s := SubNode.XmlDocument;
          Track.extensions := Track.extensions + CRLF + s;
        end;
      end;
      Track.extensions := Trim(Track.extensions);
    end
    else
      SubList.Free;

    {
    if GPXOptions.GPX_Style and ( length(GetLastNodeText(Xnode,'extensions/line')) > 0 ) then
    begin
      s := GetLastNodeText(Xnode,'extensions/line/color');
      if length(s)>0 then
        Track.LineStyle.ColorStr := s;
      s := GetLastNodeText(Xnode,'extensions/line/opacity');
      if length(s)>0 then
        Track.LineStyle.OpacityStr := s;
    end
    else
    Track.extensions := GetExtensionNodeContent(Xnode,'extensions');
    }

    if (length(Track.Name) = 0) and (length(Track.Desc) > 0) then
      Track.Name := Track.Desc;

    XtrksegList := XNode.SelectNodes('trkseg');
    for i := 0 to XtrksegList.Length -1 do
    begin
      tsNode := XtrksegList.Item(i);
      trkseg := TGPXtrkseg.Create;
      trkseg.extensions := GetExtensionNodeContent(tsNode,'extensions');
      XPtList := tsNode.SelectNodes('trkpt');
      for j := 0 to XPtList.Length -1 do
      begin
        tpNode := XPtList.Item(j);
        ImportGPXwpt(tpNode, trkseg);
      end;
      XPtList.Free;
//    Track.Add(trkseg);
      Accept := true;                // dhd
      if Assigned(fFilterFunc) then  // dhd
        fFilterFunc(trkSeg, Accept); // dhd
      if Accept then                 // dhd
        Track.Add(trkseg)            // dhd
      else                           // dhd
        trkseg.Free;                 // dhd
    end;
    XtrksegList.Free;

  finally
//  Self.GPXd.trk.add(Track);        // dhd
    if Assigned(fFilterFunc) then
      fFilterFunc(Track, Accept);
    if Accept then
      Self.GPXd.trk.add(Track)
    else
      Track.Free;

    if Self.GPXd.trk.Count > 0 then
        Self.GPXd.trk.filled := true;
  end;
end;

procedure TGPXFile.ImportGPXextensions(XNode : TXpNode);
var
  s : String;
const
  tag = 'extensions';
begin
  s := Trim(Xnode.XmlDocument);
  if s = '<extensions><extensions/></extensions>' then
    s := '';
  if length(s) > 0 then
  begin
    if pos('<'+tag+'>',s) > 0 then
    begin
      s := copy(s,pos('<'+tag+'>',s)+length('<'+tag+'>'),length(s));
      s := copy(s,1,pos('</'+tag+'>',s)-1);
    end;
  end;
  Self.GPXd.extension.Text := s;
end;

function TGPXFile.ExportGPXWPTList(WayptList : TGPXwptList) : String;
var
  s : String;
  i : integer;
  WayPoint : TGPXwpt;
begin
  s := '';
  for i := 0 to WayptList.Count-1 do
  begin
    WayPoint := WayptList.WayPoints[i];
    s := s + ExportGPXWPT(WayPoint);
  end;
  Result := s;
end;

function TGPXFile.ExportGPXRTEList(RouteList : TGPXrteList) : String;
var
  s : String;
  i : integer;
  Route : TGPXrte;
begin
  s := '';
  for i := 0 to RouteList.Count-1 do
  begin
    Route := RouteList.Routes[i];
    s := s + ExportGPXRTE(Route);
  end;
  Result := s;
end;

function TGPXFile.ExportGPXTRKList(TrackList : TGPXtrkList) : String;
var
  s : String;
  i : integer;
  Track : TGPXtrk;
begin
  s := '';
  for i := 0 to TrackList.Count-1 do
  begin
    Track := TrackList.Tracks[i];
    s := s + ExportGPXTRK(Track);
  end;
  Result := s;
end;

procedure TGPXFile.ExportGPX(Filename : String);
var
  s : WideString; //String;
  u : String;
  GPXFS : TFileStream;
const
  BASE_XMLNS = 'xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"';
begin
  if FileExists(FileName) then
    DeleteFile(FileName);

  if GPXOptions.Compress then
  begin
    CRLF := '';
    TAB := '';
  end
  else
  begin
    //CRLF := #13#10; // Conforming to XML spec (2.11)
    CRLF := #10;
    TAB := #09;
  end;

  GPXFS := TFileStream.Create(FileName,fmCreate	OR fmOpenWrite OR fmShareExclusive);

  GPXFS.Size := 0;

  s := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'+CRLF;
  s := s + '<gpx version="1.1" creator="'+Self.GPXd.creator+'" ';
  if (length(Self.GPXd.xmlns) > 0) and GPXOptions.PreserveXMLNS then
    s := s + Self.GPXd.xmlns
  else
    s := s + BASE_XMLNS;

  s := trim(s) + '>'+CRLF;

  u := UTF8Encode(s);
  GPXFS.WriteBuffer(u[1],length(u));

  if Self.GPXd.metadata.filled then
  begin
    try
      Self.GPXd.ComputeMetaBounds;
    finally
    end;
    s := ExportGPXmetadata(Self.GPXd.metadata);
    u := UTF8Encode(s);
    GPXFS.WriteBuffer(u[1],length(u));
  end;

  s := ExportGPXWPTList(Self.GPXd.wpt);
  if Self.GPXd.wpt.Count > 0 then
  begin
    s := s + CRLF;
    u := UTF8Encode(s);
    GPXFS.WriteBuffer(u[1],length(u));
  end;

  s := ExportGPXRTEList(Self.GPXd.rte);
  if Self.GPXd.rte.Count > 0 then
  begin
    s := s + CRLF;
    u := UTF8Encode(s);
    GPXFS.WriteBuffer(u[1],length(u));
  end;

  s := ExportGPXTRKList(Self.GPXd.trk);
  if Self.GPXd.trk.Count > 0 then
  begin
    s := s + CRLF;
    u := UTF8Encode(s);
    GPXFS.WriteBuffer(u[1],length(u));
  end;

  s := '';
  if length(Trim(Self.GPXd.extension.Text))>0 then
  begin
    s := WriteText(Self.GPXd.extension.Text,'extensions',0,True);
    u := UTF8Encode(s);
    GPXFS.WriteBuffer(u[1],length(u));
  end;

  s := '</gpx>';
  u := UTF8Encode(s);
  GPXFS.WriteBuffer(u[1],length(u));

  Self.Modified := False;

  //Log(ExtractFileName(Filename) +  ' saved');

  GPXFS.Free;
end;

function TGPXFile.DateTimeToGPXTime(Value : TDateTime) : String;
var
  d : String;
  nh,nm,ns,nd,ny,nms : word;
begin
  DecodeDate(Value,ny,nm,nd);
  d := Format('%.4d-%.2d-%.2d',[ny,nm,nd]);
  DecodeTime(Value,nh,nm,ns,nms);
  if nms > 0 then
    d := d + 'T' + Format('%.2d:%.2d:%.2d.%.3d',[nh,nm,ns,nms])
  else
    d := d + 'T' + Format('%.2d:%.2d:%.2d',[nh,nm,ns]);
  d := d + 'Z';

  Result := d;
end;


function TGPXFile.ExportGPXWPT(WayPoint : TGPXwpt;Header : String ='wpt'; TabCount : integer = 0) : String;
var
  s : String;
  TabC : String;
begin
  if Length(TAB) > 0 then
    TabC := StringOfChar(TAB[1],TabCount)
  else
    TabC := '';
  s := TabC + '<'+Header+' lat="'+FloatToStr(WayPoint.Latitude,FmtS)
       +'" lon="'+FloatToStr(WayPoint.Longitude,FmtS)+'">'+CRLF;

  s := s + WriteDouble(WayPoint.Altitude,'ele',0,TabCount+1);
  s := s + WriteTime(WayPoint.DateTime,'time',0,TabCount+1);

  s := s + WriteText(WayPoint.name,'name',TabCount+1);
  s := s + WriteText(WayPoint.cmt,'cmt',TabCount+1);
  s := s + WriteText(WayPoint.desc,'desc',TabCount+1);
  s := s + WriteText(WayPoint.src,'src',TabCount+1);
  if WayPoint.link.filled then
    s := s + ExportGPXlink(WayPoint.link);
  s := s + WriteText(WayPoint.sym,'sym',TabCount+1);
  s := s + WriteText(WayPoint.wtype,'type',TabCount+1);
  s := s + WriteText(WayPoint.fix,'fix',TabCount+1);

  s := s + WriteInt(WayPoint.sat,'sat',255,TabCount+1);
  s := s + WriteInt(WayPoint.dgpsid,'dgpsid',-1,TabCount+1);

  s := s + WriteDouble(WayPoint.magvar,'magvar',0,TabCount+1);
  s := s + WriteDouble(WayPoint.geoidheight,'geoidheight',0,TabCount+1);
  s := s + WriteDouble(WayPoint.hdop,'hdop',-1,TabCount+1);
  s := s + WriteDouble(WayPoint.vdop,'vdop',-1,TabCount+1);
  s := s + WriteDouble(WayPoint.pdop,'pdop',-1,TabCount+1);
  s := s + WriteDouble(WayPoint.ageofdgpsdata,'ageofdgpsdata',-1,TabCount+1);

  s := s + WriteText(WayPoint.extensions,'extensions',TabCount+1,True);

  s := s + TabC + '</'+Header+'>'+CRLF;
  Result := s;
end;

function TGPXFile.ExportGPXRTE(Route : TGPXrte) : String;
var
  s : String;
  i : integer;
  WP : TGPXwpt;
  HasStyle : Boolean;
begin
  s := '<rte>'+CRLF;
  s := s + WriteText(Route.name,'name',1);
  s := s + WriteText(Route.cmt,'cmt',1);
  s := s + WriteText(Route.desc,'desc',1);
  s := s + WriteText(Route.src,'src',1);
  if Route.link.filled then
    s := s + ExportGPXlink(Route.link);
  s := s + WriteInt(Route.number,'number',-1,1);
  s := s + WriteText(Route.rType,'type',1);
  HasStyle := not Route.LineStyle.IsDefaultColor;
  HasStyle := HasStyle or (Abs(Route.LineStyle.Opacity) <1);
  HasStyle := HasStyle and Self.GPXOptions.GPX_Style;
  if HasStyle or (length(Route.extensions)>0) then
  begin
    s := s+TAB+'<extensions>' + CRLF;
    if HasStyle then
    begin
      s := s+TAB+TAB+'<line xmlns="http://www.topografix.com/GPX/gpx_style/0/2">' + CRLF;
      if not Route.LineStyle.IsDefaultColor then
      begin
        s := s + WriteText(Route.LineStyle.ColorStr,'color',3);
      end;
      if Abs(Route.LineStyle.Opacity) < 1 then
      begin
        s := s + WriteText(Route.LineStyle.OpacityStr,'opacity',3);
      end;
      s := s+TAB+TAB+'</line>' + CRLF;
    end;
    s := s + Route.extensions;
    s := s+TAB+'</extensions>' + CRLF;
  end
  else
    s := s + WriteText(Route.extensions,'extensions',1,True);

  for i := 0 to Route.Count-1 do
  begin
    WP := Route.rtept[i];
    s := s + ExportGPXWPT(WP,'rtept',1);
  end;
  s := s + '</rte>'+CRLF;
  Result := s;
end;

function TGPXFile.ExportGPXTRK(Track : TGPXtrk) : String;
var
  s : String;
  i,j : integer;
  TS : TGPXtrkseg;
  WP : TGPXwpt;
  HasStyle : Boolean;
begin
  s := '<trk>'+CRLF;
  s := s + WriteText(Track.name,'name',1);
  s := s + WriteText(Track.cmt,'cmt',1);
  s := s + WriteText(Track.desc,'desc',1);
  s := s + WriteText(Track.src,'src',1);
  if Track.link.filled then
    s := s + ExportGPXlink(Track.link);
  s := s + WriteInt(Track.number,'number',-1,1);
  s := s + WriteText(Track.tType,'type',1);
  HasStyle := not Track.LineStyle.IsDefaultColor;
  HasStyle := HasStyle or (Abs(Track.LineStyle.Opacity) <1);
  HasStyle := HasStyle and Self.GPXOptions.GPX_Style;
  if HasStyle or (length(Track.extensions)>0) then
  begin
    s := s+TAB+'<extensions>' + CRLF;
    if HasStyle then
    begin
      s := s+TAB+TAB+'<line xmlns="http://www.topografix.com/GPX/gpx_style/0/2">' + CRLF;
      if not Track.LineStyle.IsDefaultColor then
      begin
        s := s + WriteText(Track.LineStyle.ColorStr,'color',3);
      end;
      if Abs(Track.LineStyle.Opacity) < 1 then
      begin
        s := s + WriteText(Track.LineStyle.OpacityStr,'opacity',3);
      end;
      s := s+TAB+TAB+'</line>' + CRLF;
    end;
    s := s + Track.extensions;
    s := s+TAB+'</extensions>' + CRLF;
  end
  else
    s := s + WriteText(Track.extensions,'extensions',1,True);

  for i := 0 to Track.Count-1 do
  begin
    TS := Track.trkseg[i];
    s := s + TAB + '<trkseg>'+CRLF;
    for j := 0 to TS.Count-1 do
    begin
      WP := TS.trkpt[j];
      s := s + ExportGPXWPT(WP,'trkpt',2);
    end;
    s := s + WriteText(TS.extensions,'extensions',1,True);
    s := s + TAB + '</trkseg>'+CRLF;
  end;
  s := s + '</trk>'+CRLF;
  Result := s;
end;

function TGPXFile.ExportGPXlink(link : TGPXlink; TabCount : integer = 0) : String;
var
  s : String;
  TabC : String;
begin
  TabC := StringOfChar(Char(9),TabCount);
  s := TabC+'<link';
  if length(link.href)>0 then
    s := s + ' href="'+link.href+'"';
  s := s + '>'+CRLF;
  s := s + WriteText(link.text,'text',TabCount+1);
  s := s + WriteText(link.ltype,'type',TabCount+1);
  s := s + TabC+'</link>'+CRLF;
  Result := s;
end;

function TGPXFile.ExportGPXmetadata(meta : TGPXmetadata) : String;
var
  s : String;
  TabC : String;
const
  TabCount = 1;
begin
  TabC := StringOfChar(Char(9),TabCount);
  s := '<metadata>'+CRLF;
  s := s + WriteText(meta.name,'name',TabCount);
  s := s + WriteText(meta.desc,'desc',TabCount);
  if meta.author.filled then
  begin
    s := s + TabC+'<author>'+CRLF;
    s := s + WriteText(meta.author.name,'name',TabCount+1);
    if meta.author.email.filled then
    begin
      s := s + TabC+TAB+'<email>'+CRLF;
      s := s + WriteText(meta.author.email.id,'id',TabCount+2);
      s := s + WriteText(meta.author.email.domain,'domain',TabCount+2);
      s := s + TabC+TAB+'</email>'+CRLF;
    end;
    if meta.author.link.filled then
      s := s + ExportGPXLink(meta.author.link,TabCount+1);
    s := s + '</author>'+CRLF;
  end;
  if meta.copyright.filled then
  begin
    s := s + TabC+'<copyright';
    if length(meta.copyright.author)>0 then
      s := s + ' author="'+meta.copyright.author+'"';
    s := s + '>'+CRLF;
    s := s + Writeint(meta.copyright.year,'year',-1,TabCount+1);
    s := s + WriteText(meta.copyright.license,'license',TabCount+1);
    s := s + TabC+'</copyright>'+CRLF;
  end;
  if meta.link.filled then
    s := s + ExportGPXLink(meta.link,TabCount);
  s := s + WriteTime(meta.mtime,'time',0,TabCount);
  s := s + WriteText(meta.keywords,'keywords',TabCount);
  if meta.bounds.filled then
  begin
    s := s + TabC+'<bounds';
    s := s + ' minlat="' + FloatToStr(meta.bounds.minlat,FmtS) + '"';
    s := s + ' minlon="' + FloatToStr(meta.bounds.minlon,FmtS) + '"';
    s := s + ' maxlat="' + FloatToStr(meta.bounds.maxlat,FmtS) + '"';
    s := s + ' maxlon="' + FloatToStr(meta.bounds.maxlon,FmtS) + '"';
    s := s + ' />'+CRLF;
  end;
  s := s + WriteText(meta.extensions,'extensions',TabCount,True);
  s := s + '</metadata>'+CRLF;
  Result := s;
end;

procedure TGPXFile.CopyFrom(GPXFile : TGPXFile);
begin
  GPXd.CopyFrom(GPXFile.GPXd);
  FileName := GPXFile.FileName;
  Loaded := GPXFile.Loaded;
  Modified := GPXFile.Modified;

  mLog := GPXFile.mLog;
  FmtS := GPXFile.FmtS;
end;

procedure TGPXFile.ImportXMLNS(XNode : TXpNode);
var
  i : integer;
  s, sName : String;
  sNode : TXpNode;
begin
  s := '';

  try
    if Assigned(XNode.Attributes) then
    begin
      for i := 0 to XNode.Attributes.Length-1 do
      begin
        sNode := XNode.Attributes.Item(i);
        sName := sNode.NodeName;
        if (pos('xsi',sName) = 1   ) or
           (pos('xmlns',sName) = 1 ) then
        begin
          s := s + sName + '="' + snode.NodeValue + '" ';
        end;
      end;
    end;
  finally
    GPXd.xmlns := trim(s);
  end;
end;



end.
