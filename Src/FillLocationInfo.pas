unit FillLocationInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ovcsf, ovcbase, ovcef, ovcpb, ovcnf, PDB_Decl,
  ExtCtrls, LocationUtils, Menus, ovcsc;

type
  TLocationSearchType = (st_Normal, st_Description, st_Location, st_LatLon,
                         st_CreateTrackFile, st_NearNess);
  
  TfrmFillLocationInfo = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    cbUpdateExistingLocationID: TCheckBox;
    lbLocationSource: TComboBox;
    pnlLatLon: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ovcLatitude: TOvcNumericField;
    ovcLongitude: TOvcNumericField;
    ovcLatDegMin: TOvcSimpleField;
    ovcLongDegMin: TOvcSimpleField;
    pnlDesc: TPanel;
    Label3: TLabel;
    ovcDescription: TOvcSimpleField;
    ovcMaxDistanceFeet: TOvcNumericField;
    lblMaxDistanceFeet: TLabel;
    MainMenu1: TMainMenu;
    Edit1: TMenuItem;
    PasteLocationInfo1: TMenuItem;
    CopyLocationInfo1: TMenuItem;
    cbPrivateLocation: TCheckBox;
    PopupMenu1: TPopupMenu;
    PasteSpecial1: TMenuItem;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    ovcState: TOvcSimpleField;
    Label6: TLabel;
    pnlRefCount: TPanel;
    Label9: TLabel;
    lblRefCount: TLabel;
    pnlStartDirection: TGroupBox;
    ovcStartDirection: TOvcNumericField;
    OvcSpinner1: TOvcSpinner;
    Label10: TLabel;
    pnlDates: TPanel;
    Label7: TLabel;
    lblDateAdded: TLabel;
    Label8: TLabel;
    lblDateUpdated: TLabel;
    DefaulttoLastLocation1: TMenuItem;
    procedure ovcLatitudeChange(Sender: TObject);
    procedure ovcLongitudeChange(Sender: TObject);
    procedure ovcLatDegMinChange(Sender: TObject);
    procedure ovcLongDegMinChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure PasteLocationInfo1Click(Sender: TObject);
    procedure CopyLocationInfo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure PasteSpecial1Click(Sender: TObject);
    procedure ovcDescriptionExit(Sender: TObject);
    procedure DefaulttoLastLocation1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    fInhibitLatLon, fInhibitDegMin: boolean;
    fLocationSearchType: TLocationSearchType;
    fLocationID: integer;
    fMeasurementUnits: TMeasurementUnits;
    fRefCount: integer;
    function GetLocationSource: TLocationSource;
    procedure SetLocationSource(const Value: TLocationSource);
    function GetLatitude: double;
    procedure SetLatitude(const Value: double);
    function GetLongitude: double;
    procedure SetLongitude(const Value: double);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetMaxDistance: double;
    procedure SetMaxDistance(const Value: double);
    procedure SetMaxDescSize(const Value: integer);
    procedure SetLocationID(const Value: integer);
    function GetPrivateLocation: boolean;
    procedure SetPrivateLocation(const Value: boolean);
    function GetState: string;
    procedure SetState(const Value: string);
    function GetMaxDistanceUnits: TMeasurementUnits;
    procedure SetMaxDistanceUnits(const Value: TMeasurementUnits);
    procedure SetRefCount(const Value: integer);
    function GetAzimuth: double;
    procedure SetAzimuth(const Value: double);
  protected
    procedure SetLocationSearchType(const Value: TLocationSearchType); virtual;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    property Azimuth: double
             read GetAzimuth
             write SetAzimuth;
    property LocationSource: TLocationSource
             read GetLocationSource
             write SetLocationSource;
    property LocationID: integer
             write SetLocationID;
    property Latitude: double
             read GetLatitude
             write SetLatitude;
    property Longitude: double
             read GetLongitude
             write SetLongitude;
    property MaxDistance: double
             read GetMaxDistance
             write SetMaxDistance;
    property MaxDistanceUnits: TMeasurementUnits
             read GetMaxDistanceUnits
             write SetMaxDistanceUnits;
    property Description: string
             read GetDescription
             write SetDescription;
    property State: string
             read GetState
             write SetState;
    property LocationSearchType: TLocationSearchType
             read fLocationSearchType
             write SetLocationSearchType;
    property MaxDescSize: integer
             write SetMaxDescSize;
    property PrivateLocation: boolean
             read GetPrivateLocation
             write SetPrivateLocation;
    property RefCount: integer
             read fRefCount
             write SetRefCount;
  end;

var
  gLatitude: double;
  gLongitude: double;
  gDescription: string;
  gState: string;
  gLocationID: integer;
  gLocationSource: TLocationSource;
  gPrivateLocation: boolean;
  gDateAdded: TDateTime;
  gDateUpdated: TDateTime;
  gRefCount: integer;

  fLatitude: double;
  fLongitude: double;
  fDescription: string;
  fState: string;
  fLocationSource: TLocationSource;
  fPrivateLocation: boolean;
  fDateAdded: TDateTime;
  fDateUpdated: TDateTime;
  fRefCount: integer;

  frmFillLocationInfo: TfrmFillLocationInfo;


implementation

uses Clipbrd, MyUtils, StStrL, StateNamesX;

{$R *.dfm}

procedure TfrmFillLocationInfo.ovcLatitudeChange(Sender: TObject);
begin
  if not fInhibitDegMin then
    try
      fInhibitLatLon := true;
      ovcLatDegMin.AsString := DecimalDegreesToDegMin(ovcLatitude.AsFloat, llLat);
    finally
      fInhibitLatLon := false;
    end;
end;

procedure TfrmFillLocationInfo.ovcLongitudeChange(Sender: TObject);
begin
  if not fInhibitDegMin then
    try
      fInhibitLatLon := true;
      ovcLongDegMin.AsString := DecimalDegreesToDegMin(ovcLongitude.AsFloat, llLon);
    finally
      fInhibitLatLon := false;
    end;
end;

procedure TfrmFillLocationInfo.ovcLatDegMinChange(Sender: TObject);
begin
  if not fInhibitLatLon then
    try
      fInhibitDegMin := true;
      try
        ovcLatitude.AsFloat := DegMinToDecimalDegrees(ovcLatDegMin.AsString);
      except
        on e:EInvalidLocation do
          { nothing }
        else
          raise;
      end;
    finally
      fInhibitDegMin := false;
    end;
end;

procedure TfrmFillLocationInfo.ovcLongDegMinChange(Sender: TObject);
begin
  if not fInhibitLatLon then
    try
      fInhibitDegMin := true;
      try
      ovcLongitude.AsFloat := DegMinToDecimalDegrees(ovcLongDegMin.AsString);
      except
        on EInvalidLocation do
          { nothing }
        else
          raise;
      end;
    finally
      fInhibitDegMin := false;
    end;
end;

constructor TfrmFillLocationInfo.Create(aOwner: TComponent);
var
  ls: TLocationSource;
begin
  inherited;
  LocationSearchType        := st_Normal;
  lbLocationSource.Clear;
  for ls := Low(TLocationSource) to High(TLocationSource) do
    lbLocationSource.Items.AddObject(LocationSourceStrings[ls], TObject(ls));
  MaxDistanceUnits          := muFeet;
end;

destructor TfrmFillLocationInfo.Destroy;
begin

  inherited;
end;

procedure TfrmFillLocationInfo.btnOkClick(Sender: TObject);
begin
  ovcDescriptionExit(nil);
  gLatitude        := ovcLatitude.AsFloat;
  gLongitude       := ovcLongitude.AsFloat;
  gDescription     := ovcDescription.AsString;
  gState           := ovcState.AsString;
  gLocationSource  := LocationSource;
  gPrivateLocation := cbPrivateLocation.Checked;

  fLatitude        := ovcLatitude.AsFloat;
  fLongitude       := ovcLongitude.AsFloat;
  fDescription     := ovcDescription.AsString;
  fState           := ovcState.AsString;
  fLocationSource  := LocationSource;
  fPrivateLocation := cbPrivateLocation.Checked;
  if IsValidDate(lblDateAdded.Caption) then
    fDateAdded       := StrToDateTime(lblDateAdded.Caption);
end;

function TfrmFillLocationInfo.GetLocationSource: TLocationSource;
begin
  with lbLocationSource do
    if ItemIndex >= 0 then
      result := TLocationSource(Items.Objects[ItemIndex])
    else
      result := ls_GPSLogs;
end;

procedure TfrmFillLocationInfo.SetLocationSource(
  const Value: TLocationSource);
begin
  with lbLocationSource do
    ItemIndex := Items.IndexOfObject(TObject(Value));
end;

function TfrmFillLocationInfo.GetLatitude: double;
begin
  result := ovcLatitude.AsFloat;
end;

procedure TfrmFillLocationInfo.SetLatitude(const Value: double);
begin
  ovcLatitude.AsFloat := value;
  ovcLatDegMin.AsString := DecimalDegreesToDegMin(value, llLat);
end;

function TfrmFillLocationInfo.GetLongitude: double;
begin
  result := ovcLongitude.AsFloat;
end;

procedure TfrmFillLocationInfo.SetLongitude(const Value: double);
begin
  ovcLongitude.AsFloat := Value;
  ovcLongDegMin.AsString := DecimalDegreesToDegMin(Value, llLon);
end;

function TfrmFillLocationInfo.GetDescription: string;
begin
  result := ovcDescription.AsString;
end;

procedure TfrmFillLocationInfo.SetDescription(const Value: string);
begin
  ovcDescription.AsString := value;
end;

procedure TfrmFillLocationInfo.SetLocationSearchType(const Value: TLocationSearchType);
begin
  pnlDesc.Visible   := Value in [st_Description, st_Normal, st_Nearness];
  pnlDesc.Enabled   := Value in [st_Description, st_Normal];
  pnlLatLon.Visible := Value in [st_Location, st_Normal, st_LatLon, st_Nearness];
  ovcMaxDistanceFeet.Visible := Value in [st_Location, st_LatLon, st_Nearness];
  lblMaxDistanceFeet.Visible := Value in [st_Location, st_LatLon, st_Nearness];
  pnlRefCount.Visible        := Value in [st_Normal];
  lbLocationSource.Visible   := Value in [st_Normal];
  cbPrivateLocation.Visible  := Value in [st_Normal];
  pnlStartDirection.Visible  := Value in [st_Nearness];
  pnlDates.Visible           := Value in [st_Normal];
  cbUpdateExistingLocationID.Visible := Value in [st_Normal];
  fLocationSearchType := Value;
end;

function TfrmFillLocationInfo.GetMaxDistance: double;
begin
  result := ovcMaxDistanceFeet.AsFloat;
end;

procedure TfrmFillLocationInfo.SetMaxDistance(const Value: double);
begin
  ovcMaxDistanceFeet.AsFloat := Value;
end;

procedure TfrmFillLocationInfo.SetMaxDescSize(const Value: integer);
begin
  ovcDescription.MaxLength := Value;
end;

procedure TfrmFillLocationInfo.PasteLocationInfo1Click(Sender: TObject);
var
  Latitude, Longitude: double;
begin
  if ParseLatitudeLongitude(Clipboard.AsText, Latitude, Longitude) then
    begin
      ovcLatitude.AsFloat  := Latitude;
      ovcLongitude.AsFloat := Longitude;
    end;
end;

procedure TfrmFillLocationInfo.CopyLocationInfo1Click(Sender: TObject);
begin
  Clipboard.AsText := Format('%11.7f %11.7f', [ovcLatitude.AsFloat, ovcLongitude.AsFloat]);
end;

procedure TfrmFillLocationInfo.SetLocationID(const Value: integer);
begin
  Caption := Format('Fill location ID [%d]', [Value]);
  fLocationID := Value;
end;

function TfrmFillLocationInfo.GetPrivateLocation: boolean;
begin
  result := cbPrivateLocation.Checked;
end;

procedure TfrmFillLocationInfo.SetPrivateLocation(const Value: boolean);
begin
  cbPrivateLocation.Checked := Value;
end;

procedure TfrmFillLocationInfo.Cut1Click(Sender: TObject);
begin
  ovcDescription.SelectedText := '';
end;

procedure TfrmFillLocationInfo.Copy1Click(Sender: TObject);
begin
  Clipboard.AsText := ovcDescription.SelectedText;
end;

procedure TfrmFillLocationInfo.Paste1Click(Sender: TObject);
begin
  ovcDescription.SelectedText := ClipBoard.AsText;
end;

procedure TfrmFillLocationInfo.PasteSpecial1Click(Sender: TObject);
begin
  ovcDescription.SelectedText := CleanUpString(ClipBoard.AsText, [#32..#127], ' ');
end;

function TfrmFillLocationInfo.GetState: string;
begin
  result := ovcState.AsString;
end;

procedure TfrmFillLocationInfo.SetState(const Value: string);
begin
  ovcState.AsString := Value;
end;

function TfrmFillLocationInfo.GetMaxDistanceUnits: TMeasurementUnits;
begin
  result := fMeasurementUnits;
end;

procedure TfrmFillLocationInfo.SetMaxDistanceUnits(
  const Value: TMeasurementUnits);
begin
  fMeasurementUnits := Value;
  lblMaxDistanceFeet.Caption := Format('Max Distance (%s)', [MeasurementUnitsArray[fMeasurementUnits]]);
end;

procedure TfrmFillLocationInfo.ovcDescriptionExit(Sender: TObject);
const
  DELIMS = ' ';
var
  wc: cardinal;
  i, ps, TempStWrd: integer;
  aword: string;
  description, Abbrev: string;
  OK: boolean;
  PossibleStates: TStateSet;
begin
  Description := ovcDescription.AsString;
  wc := WordCountL(Description, DELIMS);
  for i := 1 to wc do
    begin
      aWord := ExtractWordL(i, Description, DELIMS);
      OK    := ValidStateAbbreviation(aWord);
      if OK then
        Abbrev := aWord
      else
        begin
          ps := FindPossibleStates(aWord, PossibleStates);
          if ps > 0 then
            begin
              TempStWrd := i;
              Abbrev    := FindBestMatchingState(Description,
                             TempStWrd,
                             PossibleStates,
                             DELIMS);
              OK        := not Empty(Abbrev);
            end;
        end;

      if OK then
        if not SameText(ovcState.AsString, Abbrev) then
          begin
            if (not Empty(ovcState.AsString)) then
              OK := YesFmt('Change state from "%s" to "%s" for "%s"?', [ovcState.AsString, Abbrev, Description]);

            if OK then
              ovcState.AsString := Abbrev;
          end
    end;
end;

procedure TfrmFillLocationInfo.SetRefCount(const Value: integer);
begin
  lblRefCount.Caption := IntToStr(Value);
  fRefCount           := Value;
end;

function TfrmFillLocationInfo.GetAzimuth: double;
begin
  result := ovcStartDirection.AsVariant;
end;

procedure TfrmFillLocationInfo.SetAzimuth(const Value: double);
begin
  ovcStartDirection.AsVariant := Value
end;

procedure TfrmFillLocationInfo.DefaulttoLastLocation1Click(
  Sender: TObject);
begin
  ovcDescription.AsString   := fDescription;  // just the descriptive stuff
  ovcState.AsString         := fState;
  cbPrivateLocation.Checked := fPrivateLocation;
  lblDateUpdated.Caption    := DateTimeToStr(Now);
  lblDateAdded.Caption      := DateTimeToStr(fDateAdded);
end;

procedure TfrmFillLocationInfo.FormShow(Sender: TObject);
begin
  lblDateAdded.Caption   := DateTimeToStr(gDateAdded);
  lblDateUpdated.Caption := DateTimeToStr(gDateUpdated);
end;

initialization
  gLatitude  := 0.0;
  gLongitude := 0.0;
  gDescription := '';
  gState       := '';
end.
