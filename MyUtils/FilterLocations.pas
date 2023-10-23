unit FilterLocations;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FilterOptions, StdCtrls, ExtCtrls, ovcbase, ovcef, ovcpb, ovcnf,
  DB, Menus, ClipBrd, FillLocationInfo;

type
  TfrmFilterLocation = class(TfrmFilterOptions)
    MainMenu1: TMainMenu;
    e1: TMenuItem;
    PasteLatitudeLongitude1: TMenuItem;
    leState: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    ovcLatitude: TOvcNumericField;
    ovcLongitude: TOvcNumericField;
    pnlLatLon: TPanel;
    Label4: TLabel;
    cbMaxDist: TComboBox;
    procedure PasteLatitudeLongitude1Click(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
  private
    procedure SetLocationSearchType(const Value: TLocationSearchType);
    { Private declarations }
  protected
    procedure ClearFilterOptions; override;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; DataSet: TDataSet); reintroduce;
    property TheLocationSearchType: TLocationSearchType
             write SetLocationSearchType;
  end;

  TMaxDist = ({0} md_500ft, {1} md_1000ft, {2} md_2500ft, {3} md_1Mile, {4} md_2Miles, {5} md_5Miles, {6} md_10Miles, {7} md_25Miles,
              {8} md_50Miles, {9} md_125Miles, {10} md_250Miles, {11} md_500Miles, {12} md_1000Miles, {13} md_2500Miles);

var
  MAX_DISTANCE: Array[TMaxDist] of string =
(
 {md_500ft}       '500 ft',
 {md_1000ft}      '1000 ft',
 {md_2500ft}      '2500 ft',
 {md_1Mile}       '1 Mile',
 {md_2Miles}      '2 Miles',
 {md_5Miles}      '5 Miles',
 {md_10Miles}     '10 Miles',
 {md_25Miles}     '25 Miles',
 {md_50Miles}     '50 Miles',
 {md_125Miles}    '125 Miles',
 {md_250Miles}    '250 Miles',
 {md_500Miles}    '500 Miles',
 {md_1000Miles}   '1000 Miles',
 {md_2500Miles)}  '2500 Miles'
 );

implementation

uses LocationUtils;

{$R *.dfm}

{ TfrmFilterLocation }

constructor TfrmFilterLocation.Create(aOwner: TComponent; DataSet: TDataSet);
var
  md: TMaxDist;
begin
  inherited;
  for md := Low(TMaxDist) to High(TMaxDist) do
    cbMaxDist.AddItem(MAX_DISTANCE[md], TObject(md));
end;

procedure TfrmFilterLocation.PasteLatitudeLongitude1Click(Sender: TObject);
var
  Latitude, Longitude: double;
begin
  inherited;
  if ParseLatitudeLongitude(Clipboard.AsText, Latitude, Longitude) then
    begin
      ovcLatitude.AsFloat  := Latitude;
      ovcLongitude.AsFloat := Longitude;
//    FilterInfoChanged;
    end;
end;

procedure TfrmFilterLocation.btnClearAllClick(Sender: TObject);
begin
  inherited;
  ClearFilterOptions;
end;

procedure TfrmFilterLocation.ClearFilterOptions;
begin
  inherited;
  ovcLatitude.AsString  := '';
  ovcLongitude.AsString := '';
  cbMaxDist.ItemIndex   := -1;
  leState.Text          := '';
end;

procedure TfrmFilterLocation.SetLocationSearchType(
  const Value: TLocationSearchType);
begin
(*
  case Value of
    st_Description:
      begin
        pnlLatLon.Visible := false;
        leState.Visible   := true;
      end;
    st_Location:
      begin
        pnlLatLon.Visible := true;
        leState.Visible   := false;
      end;
    else
      raise Exception.Create('System error. Unexpected Location Search Type');
  end;
*)
end;

end.
