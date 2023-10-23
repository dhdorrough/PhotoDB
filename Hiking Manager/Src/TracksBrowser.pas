unit TracksBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BrowserUnit, Menus, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, DB,
  FilterOptions;

type
  TfrmTracksBrowser = class(TfrmDataSetBrowser)
    Utilities1: TMenuItem;
    MoveselectedTracks1: TMenuItem;
    CopyStartLocation1: TMenuItem;
    CopyEndLocation1: TMenuItem;
    CopyBRUpperLeft1: TMenuItem;
    CopyBRBottomright1: TMenuItem;
    N1: TMenuItem;
    procedure CopyStartLocation1Click(Sender: TObject);
    procedure CopyEndLocation1Click(Sender: TObject);
    procedure CopyBRUpperLeft1Click(Sender: TObject);
    procedure FilterOptions1Click(Sender: TObject);
    procedure MoveselectedTracks1Click(Sender: TObject);
    procedure CopyBRBottomright1Click(Sender: TObject);
  private
    { Private declarations }
    fUpperLeft, fBottomRight: string;
    fUpperLeftLatitude, fUpperLeftLongitude: double;
    fBottomRightLatitude, fBottomRightLongitude: double;
  protected
    function frmFilterOptions: TfrmFilterOptions; override;
    function CalcDescriptionString: string; override;
    function AllowFiltering: boolean; override;
    procedure ClearFilterOptions; override;
    procedure SaveFilterParams; override;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; DataSet: TDataSet; DataSetName: string); override;
    procedure FilterOnBoundingRectangle(DataSet: TDataSet; var Accept: Boolean);
    procedure FilterRecord(Dataset: TDataSet; var Accept: boolean); override;
  end;

implementation

uses HikingTables, LocationUtils, MyUtils, Math, BoundariesBrowser,
  ClipBrd, TrackFilterDialog, PhotoDBCommonSettings;

{$R *.dfm}

{ TfrmTracksBrowser }

constructor TfrmTracksBrowser.Create(aOwner: TComponent; DataSet: TDataSet; DataSetName: string);
begin
  inherited;
end;

procedure TfrmTracksBrowser.FilterOnBoundingRectangle(DataSet: TDataSet;
  var Accept: Boolean);
var
  BRTop, BRLeft, BRBottom, BRRight: double;   // Bounding rectangle
  IRTop, IRLeft, IRBottom, IRRight: double;   // Intersection rectangle
begin
  if Accept then
    begin
      if (not Empty(fUpperLeft)) and (not Empty(fBottomRight)) then
        with Dataset as TTracksTable do
          begin
            BRLeft   := fldBRLeft.AsFloat;
            BRRight  := fldBRRight.AsFloat;
            BRTop    := fldBRTop.AsFloat;
            BRBottom := fldBRBottom.AsFloat;

            IRLeft   := Max(BRLeft,   fUpperLeftLongitude);
            IRTop    := Min(BRTop,    fUpperLeftLatitude);
            IRRight  := Min(BRRight,  fBottomRightLongitude);
            IRBottom := Max(BRBottom, fBottomRightLatitude);

            Accept   := (IRLeft < IRRight) and (IRTop > IRBottom);  // The intersection rectangle cannot be imaginary
          end;
    end;
end;

function TfrmTracksBrowser.AllowFiltering: boolean;
begin
  result := (inherited AllowFiltering) or ((not Empty(fUpperLeft)) or (not Empty(fBottomRight)));
end;

procedure TfrmTracksBrowser.CopyStartLocation1Click(Sender: TObject);
begin
  inherited;
  with Dataset as TTracksTable do
    if not (Eof and Bof) then
      ClipBoard.AsText := CalcLocationStringLong(fldStartLatitude.AsFloat, fldStartLongitude.AsFloat);
end;

procedure TfrmTracksBrowser.CopyEndLocation1Click(Sender: TObject);
begin
  inherited;
  with Dataset as TTracksTable do
    if not (Eof and Bof) then
      ClipBoard.AsText := CalcLocationStringLong(fldEndLatitude.AsFloat, fldEndLongitude.AsFloat);
end;

procedure TfrmTracksBrowser.CopyBRUpperLeft1Click(Sender: TObject);
begin
  inherited;
  with Dataset as TTracksTable do
    if not (Eof and Bof) then
      ClipBoard.AsText := CalcLocationStringLong(fldBRTop.AsFloat, fldBRLeft.AsFloat);
end;

procedure TfrmTracksBrowser.FilterRecord(Dataset: TDataSet;
  var Accept: boolean);
begin
  inherited FilterRecord(DataSet, Accept);
  if Accept then
    FilterOnBoundingRectangle(DataSet, Accept);
end;

procedure TfrmTracksBrowser.ClearFilterOptions;
begin
  fUpperLeft   := '';
  fBottomRight := '';
  inherited ClearFilterOptions;
end;

function TfrmTracksBrowser.frmFilterOptions: TfrmFilterOptions;
begin
  if not Assigned(ffrmFilterOptions) then
    ffrmFilterOptions := TfrmTrackFilterOptions.Create(self, DataSet);
  result := ffrmFilterOptions;
end;

procedure TfrmTracksBrowser.SaveFilterParams;
begin
  inherited;
  with ffrmFilterOptions as TfrmTrackFilterOptions do
    begin
      fUpperLeft   := leUpperLeft.Text;
      fBottomRight := leBottomRight.Text;
      ParseLatitudeLongitude(fUpperLeft,   fUpperLeftLatitude,   fUpperLeftLongitude);
      ParseLatitudeLongitude(fBottomRight, fBottomRightLatitude, fBottomRightLongitude);
    end;
end;

procedure TfrmTracksBrowser.FilterOptions1Click(Sender: TObject);
begin
  if frmFilterOptions.ShowModal = mrOk then
    begin
      fDataSet.Filtered    := false;
      SaveFilterParams;
      fDataSet.Filtered    := fWasFiltering or AllowFiltering;
    end;
end;

function TfrmTracksBrowser.CalcDescriptionString: string;
begin
  result := (inherited CalcDescriptionString);
  if not Empty(fUpperLeft) then
    AddClause(result, 'UL', fUpperLeft);
  if not Empty(fBottomRight) then
    AddClause(result, 'BR', fBottomRight);
end;

procedure TfrmTracksBrowser.MoveSelectedTracks1Click(Sender: TObject);
var
  OldFilePath, NewFilePath, Temp: string;
  Ok: boolean;
  Count, Failed, Skipped: integer;
begin
  with Dataset as TTracksTable do
    begin
      Temp   := ExtractFilePath(CommonPhotoSettings.DefaultGPXFilter);
      if BrowseForFolder('Locate destination folder', Temp) then
        begin
          DisableControls;
          Count   := 0;
          Failed  := 0;
          Skipped := 0;
          try
            while not Eof do
              begin
                OldFilePath := ExtractFilePath(CommonPhotoSettings.DefaultGPXFilter) + fldFileName.AsString;
                if FileExists(OldFilePath) then
                  begin
                    NewFilePath := Temp + fldFileName.AsString;
                    if FileExists(NewFilePath) then
                      Ok := YesFmt('File "%s" already exists. Do you want to overwrite it?', [NewFilePath])
                    else
                      Ok := true;

                    if Ok then
                      if not RenameFile(OldFilePath, NewFilePath) then
                        begin
                          AlertFmt('Unable to rename "%s" to "%s"', [OldFilePath, NewFilePath]);
                          Inc(Failed);
                        end
                      else
                        Inc(Count)
                    else
                      Inc(Skipped);
                  end;
                Next;
              end;
          finally
            AlertFmt('%d track files were moved. %d moves failed. %d files were skipped.', [Count, Failed, Skipped]);
            EnableControls;
          end;
        end;
    end;
end;

procedure TfrmTracksBrowser.CopyBRBottomright1Click(Sender: TObject);
begin
  inherited;
  with Dataset as TTracksTable do
    if not (Eof and Bof) then
      ClipBoard.AsText := CalcLocationStringLong(fldBRBottom.AsFloat, fldBRRight.AsFloat);
end;

end.
