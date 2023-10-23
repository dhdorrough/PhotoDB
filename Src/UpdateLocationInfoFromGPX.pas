unit UpdateLocationInfoFromGPX;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, GPX, GPXFile, ovcbase, ovcef,
  ovcpb, ovcnf, GPSTrackingInfoUnit, ovcpf, PDB_Decl, PDBTables;

type
  TfrmUpdateLocationInfo = class(TForm)
    lblStatus: TLabel;
    btnUpdateSelected: TButton;
    pnlGPX: TPanel;
    leGpxFilter: TLabeledEdit;
    btnLocate: TBitBtn;
    btnLoadFromGPX: TButton;
    pnlCSV: TPanel;
    leTrackDataLfn: TLabeledEdit;
    btnLocateTrackData: TBitBtn;
    btnLoadTrackData: TButton;
    rbLoadFromGPX: TRadioButton;
    rbLoadFromCSV: TRadioButton;
    OpenDialog1: TOpenDialog;
    ovcLowestDate: TOvcPictureField;
    Label1: TLabel;
    cbAdjustPhotoTime: TCheckBox;
    ovcMinutesToAdd: TOvcNumericField;
    Label2: TLabel;
    lblCount: TLabel;
    lblLowestDate: TLabel;
    cbSimulate: TCheckBox;
    rgUseDateFrom: TGroupBox;
    cbEXIFData: TCheckBox;
    cbDataBase: TCheckBox;
    ovcHighestDate: TOvcPictureField;
    Label3: TLabel;
    btnClearPreviouslyLoadedTracks: TButton;
    btnSaveTrackData: TButton;
    btnSaveTrackDataAs: TButton;
    cbScanSelectedSubFolders: TCheckBox;
    leSelectedFolders: TLabeledEdit;
    Label4: TLabel;
    leDefaultLocationDescription: TLabeledEdit;
    leDefaultLocationState: TLabeledEdit;
    cbConfirmEachUpdate: TCheckBox;
    procedure btnLocateClick(Sender: TObject);
    procedure btnLoadFromGPXClick(Sender: TObject);
    procedure btnUpdateSelectedClick(Sender: TObject);
    procedure rbLoadFromGPXClick(Sender: TObject);
    procedure btnLocateTrackDataClick(Sender: TObject);
    procedure btnLoadTrackDataClick(Sender: TObject);
    procedure leGpxFilterChange(Sender: TObject);
    procedure cbSimulateClick(Sender: TObject);
    procedure ovcLowestDateChange(Sender: TObject);
    procedure ovcHighestDateChange(Sender: TObject);
    procedure cbEXIFDataClick(Sender: TObject);
    procedure cbDataBaseClick(Sender: TObject);
    procedure btnClearPreviouslyLoadedTracksClick(Sender: TObject);
    procedure btnSaveTrackDataClick(Sender: TObject);
    procedure btnSaveTrackDataAsClick(Sender: TObject);
    procedure cbScanSelectedSubFoldersClick(Sender: TObject);
  private
    { Private declarations }
    fMainForm: TForm;
    fLocationInfo: TLocationInfo;  {*}
    fTempPhotoTable: TPhotoTable;
    procedure Enable_Buttons;
    procedure EnableControls(Control: TWinControl; Enable: boolean);
    procedure UpdateStatus(const Msg: string; LineNo: integer = 1);
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
  end;

var
  frmUpdateLocationInfo: TfrmUpdateLocationInfo;

implementation

uses
  MyUtils, dEXIF, uPhotoDB, LocationUtils, PDBUtils, DateUtils,
  PhotoDBCommonSettings, PhotoUtils, Hiking_Decl, ConfirmEachPhoto,
{$IfDef dExif2}
  dMetadata
{$endIf}
  ;

{$R *.dfm}

procedure TfrmUpdateLocationInfo.btnLocateClick(Sender: TObject);
var
  Folder: string;
begin
  Folder := ExtractFilePath(leGpxFilter.Text);
  if BrowseForFolder('Locate folder containing .gpx files', Folder) then
    leGpxFilter.Text := Folder + '*.gpx';
end;

function GPSTrackingInfo: TGPSTrackingInfo;
begin
  if not Assigned(gGPSTrackingInfo) then
    raise Exception.Create('System error; global gGPSTrackingInfo not assigned')
  else
    result := gGPSTrackingInfo;
end;

procedure TfrmUpdateLocationInfo.btnLoadFromGPXClick(Sender: TObject);
begin
  if not Assigned(gGPSTrackingInfo) then
    gGPSTrackingInfo := TGPSTrackingInfo.Create( CommonPhotoSettings.DefaultGPXFilter,
                                                 CommonPhotoSettings.SavedTrackDataLfn);

  try
    with GPSTrackingInfo do
      begin
        OnUpdateStatus        := self.UpdateStatus;
        lblCount.Caption      := '';
        lblLowestDate.Caption := '';
        UpdateStatus('', 1);
        Application.ProcessMessages;
        fLocationInfo.ScanSelectedSubFolders  := cbScanSelectedSubFolders.Checked;
        fLocationInfo.SelectedSubFolders      := leSelectedFolders.Text;
        LoadGPXFiles( leGpxFilter.Text, ovcLowestDate.AsDateTime, ovcHighestDate.AsDateTime,
                      fLocationInfo);
        lblCount.Caption := Format('Points loaded = %0.n', [Length(Points)*1.0]);
        lblLowestDate.Caption := Format('%s --> %s', [DateTimeToStr(LowestDate), DateTimeToStr(HighestDate)]);
      end;
    Enable_Buttons;
  except
    on e:Exception do
      Alert(e.Message);
  end;
end;

constructor TfrmUpdateLocationInfo.Create(aOwner: TComponent);
begin
  inherited;
  fMainForm := aOwner as TForm;
{$IfDef DUMP}
  leFolderName.Text := TempPath;
{$EndIf}
  leTrackDataLfn.Text := IIF(not Empty(CommonPhotoSettings.SavedTrackDataLfn), CommonPhotoSettings.SavedTrackDataLfn, DEF_SAVEDTRACKSFILE);
  leGpxFilter.Text    := IIF(not Empty(CommonPhotoSettings.DefaultGPXFilter),  CommonPhotoSettings.DefaultGPXFilter, DEF_GPX_FILTER);
  rbLoadFromGPX.Checked := true;
  Enable_Buttons;
  ovcLowestDate.AsDateTime  := Now - 30;
  ovcHighestDate.AsDateTime := Now;
  lblCount.Caption          := '';
  lblLowestDate.Caption     := '';
  lblStatus.Caption         := '';
  UpdateStatus('', 1 { clBtnFace });
end;

procedure TfrmUpdateLocationInfo.Enable_Buttons;
begin
  if Assigned(gGPSTrackingInfo) then
    begin
      btnUpdateSelected.Enabled := GPSTrackingInfo.TotalWayPoints > 0;
      btnClearPreviouslyLoadedTracks.Enabled := true;
      btnClearPreviouslyLoadedTracks.Caption := Format('Clear %0.n loaded points', [GPSTrackingInfo.TotalWayPoints*1.0]);
    end
  else
    begin
      btnUpdateSelected.Enabled := false;
      btnClearPreviouslyLoadedTracks.Enabled := false;
      btnClearPreviouslyLoadedTracks.Caption := 'Clear loaded points';
    end;
end;


procedure TfrmUpdateLocationInfo.btnUpdateSelectedClick(Sender: TObject);
const
  CRLF = #13#10;
var
  Lfn: string; Count, MaxCount, Updated, Added: integer;
  ImgData: TimgData;
  OldRecNo: integer;
  temp, Msg: string;
//ftempPhotoTable: TPhotoTable;
//Year, Month, Day, Hr, Min, Sec: word;
  DateToUse: TDateTime;
  Point: TWayPointInfo;
  Latitude, Longitude, OldLatitude, OldLongitude: double;
  FoundMatch, HasOldLocation: boolean;
  DistanceAway: double;
  LocationID: integer;
  mb:  TMsgDlgBtn; mr: integer;
  OK: boolean;
  LogFileName: string;
  OutFile: TextFile;
  FoundDesc, OldLoc, NewLoc: string;
  OldID, NewID: integer;
//TempDateTime: TDateTime;
begin { TfrmUpdateLocationInfo.btnUpdateSelectedClick }
  fTempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  ImgData := TImgData.Create();
  if not cbSimulate.Checked then
    LogFileName := CalcLogFileName('UpdateLocationInfo.txt')
  else
    LogFileName := CalcLogFileName('UpdateLocationInfo [SIMULATED].txt');

  AssignFile(OutFile, LogFileName);
  ReWrite(OutFile);

  if cbSimulate.Checked then
    begin
      WriteLn(OutFile, 'SIMULATED LOCATIONS UPDATE');
      WriteLn(OutFile);
    end;

  if cbConfirmEachUpdate.Checked then
    if not Assigned(frmConfirmEachPhoto) then
      frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update');

  Updated        := 0;
  try
    with fTempPhotoTable do
      begin
        OnFilterRecord := (fMainForm as TfrmPhotoDataBase).PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression((fMainForm as TfrmPhotoDataBase).Expression);

        LocationsTable.InitLocationInfo( fLocationInfo,
                                         ls_GPSLogs,
                                         leDefaultLocationDescription.Text,
                                         leDefaultLocationState.Text,
                                         MAXDISTANCEINFEET,
                                         TRUE { OkToAdd });

//      Count the number of records to be processed
        MaxCount        := 0;
        while not Eof do
          begin
            if (MaxCount mod 100) = 0 then
              self.UpdateStatus(Format('Counting %d',
                                      [MaxCount]), -1 { clYellow });
            Inc(MaxCount);
            Next;
          end;
        First;
        Count          := 0;
        Added          := 0;
        OK             := false;
        mb             := mbHelp;  // unused default value

//      Now, actually process the records
        First;
        while not Eof do
          begin
            OldRecNo := RecNo;
            Lfn       := PathAndFileName;
            DateToUse := BAD_DATE;

            if cbEXIFData.Checked then
              begin
                // Parse the EXIF data to get a date
                OK := ImgData.ProcessFile(Lfn);
                if OK then
                  OK := ImgData.HasEXIF;
                if OK then
                  with ImgData.ExifObj do
                    begin
                      DateToUse := ImgData.ExifObj.GetImgDateTime;
//                    OK := DecodeExifDate(Temp, Year, Month, Day, Hr, Min, Sec);
//                    if OK then
//                      DateToUse := EncodeDateTime(Year, Month, Day, Hr, Min, Sec, 0);
                    end;
              end;

            if cbDataBase.Checked and (DateToUse = BAD_DATE) then
              begin
                OK := not fldPhotoDateTime.IsNull;
                if OK then
                  DateToUse := fldPhotoDateTime.AsDateTime;
              end;

            if OK then
              begin
                Point    := GPSTrackingInfo.FindClosestMatchingDateInTracks(
                                DateToUse,
                                FoundMatch,
                                ovcMinutesToAdd.AsFloat);
                if FoundMatch then
                  begin
                    Latitude    := Point.Lat;
                    Longitude   := Point.Lon;
                    if (Latitude <> 0) or (Longitude <> 0) then
                      begin
                        temp := ExtractFileName(RemoveTrailingBackSlash(ExtractFilePath(Lfn)));  // default location to last word in path
                        // Get the old lat, long (if any)
                        if (not fldLocationID.IsNull or (fldLocationID.AsInteger > 0)) then
                          begin
                            OldID          := fldLocationID.AsInteger;
                            HasOldLocation := LocationsTable.GetLatLon(OldID, OldLatitude, OldLongitude);
                            OldLoc         := LocationsTable.fldDescription.AsString;
                          end
                        else
                          begin
                            OldID          := 0;
                            HasOldLocation := false;
                            OldLoc         := '';
                          end;

//                      LocationsTable.InitLocationInfo( fLocationInfo,
//                                                       ls_GPSLogs,
//                                                       leDefaultLocationDescription.Text,
//                                                       leDefaultLocationState.Text,
//                                                       MAXDISTANCEINFEET,
//                                                       TRUE { OkToAdd });

                        FoundDesc   := fLocationInfo.DefaultLocation; // Set the default default
                        LocationID  := LocationsTable.FindLocationID( Latitude,
                                                                      Longitude,
                                                                      fLocationInfo,
                                                                      Added,
                                                                      FoundDesc {was: fLocationInfo.DefaultLocation});
                        NewID       := LocationID;
                        NewLoc      := LocationsTable.fldDescription.AsString;
                        if not cbSimulate.Checked then
                          if not (mb in [mbYesToAll, mbNoToAll]) then
                            if HasOldLocation or cbConfirmEachUpdate.Checked then
                              begin
                                DistanceAway := Distance(OldLatitude, OldLongitude, Latitude, Longitude, muFeet);
                                if (LocationID <> fldLocationID.AsInteger) or cbConfirmEachUpdate.Checked then
                                  begin
                                    if cbConfirmEachUpdate.Checked then
                                      begin
                                        Msg := Format('Set the location of (%s) to ID:%d %s?', [fldFILE_NAME.AsString,
                                                                                                LocationID,
                                                                                                LocationsTable.fldDescription.AsString]);
                                        if OldID = 0 then
                                          OldLoc := '(none)';
                                        with frmConfirmEachPhoto do
                                          begin
                                            Caption1 := Format('Change the location in record key [%d] ' + CRLF +
                                                                'from [%d:%s]' + CRLF +
                                                                'to   [%d:%s]?',
                                                               [fldKey.AsInteger,
                                                                OldID, OldLoc,
                                                                NewID, NewLoc]);
                                            Caption2 := Format('Updated/processed: %d/%d', [Updated, Count]);
                                            Question      := Msg;
                                            PhotoFileName := fTempPhotoTable.PathAndFileName;
                                            mr            := ShowModal;
                                          end
                                      end
                                    else
                                      begin
                                        Msg := Format('The Photo record (%s) already has a Location ID (%d). Overwrite it with the new Location ID (%d) ' + CRLF +
                                                    'which is %0.n feet away?',
                                                    [fldFILE_NAME.AsString,
                                                     fldLocationID.AsInteger, LocationID,
                                                     DistanceAway]);
                                        mr := MessageDlg(Msg, mtConfirmation, [mbYes, mbNo, mbCancel, mbNoToAll, mbYesToAll], 0);
                                      end;

                                    case mr of
                                      mrYes:      mb := mbYes;
                                      mrNo:       mb := mbNo;
                                      mrCancel:   mb := mbCancel;
                                      mrNoToAll:  mb := mbNoToAll;
                                      mrYesToAll: mb := mbYesToAll;
                                    end;
                                  end
                                else
                                  mb := mbIgnore;

                                if mb = mbCancel then
                                  break;
                              end
                            else
                              if mb <> mbYesToAll then
                                mb := mbYes;

                        if not cbSimulate.Checked then
                          if mb in [mbYes, mbYesToAll, mbIgnore] then
                            if fldLocationID.AsInteger <> LocationID then
                              begin
                                Edit;
                                WriteLn(OutFile,
                                        Format('%3d. %20s %5d ---> %5d',
                                               [Updated+1, fldFILE_NAME.AsString, fldLocationID.AsInteger, LocationID]));
                                fldUpdated.AsDateTime   := Now;
                                fldLocationID.AsInteger := LocationID;
                                fldLatitude.AsFloat     := Latitude;
                                fldLongitude.AsFloat    := Longitude;
                                Inc(Updated);
                                Post;
                              end
                            else
                              WriteLn(OutFile,
                                      Format('     %20s %5d skipped. No change',
                                             [fldFILE_NAME.AsString, fldLocationID.AsInteger]))

                          else
                            WriteLn(OutFile,
                                    Format('     %20s %5d skipped by user',
                                           [fldFILE_NAME.AsString, fldLocationID.AsInteger]))
                        else
                          if HasOldLocation then
                            begin
                              if (fldLocationID.AsInteger <> LocationID) then
                                begin
                                  Inc(Updated);  // assume that it would have been updated
                                  if LocationID > 0 then
                                    WriteLn(OutFile,
                                            Format('%3d. %20s %5d ---> %5d',
                                                   [Updated, fldFILE_NAME.AsString, fldLocationID.AsInteger, LocationID]))
                                  else
                                    begin
                                      WriteLn(OutFile,
                                            Format('%3d. %20s %5d ---> Would be added (%10.6n, %10.6n)',
                                                   [Updated, fldFILE_NAME.AsString, fldLocationID.AsInteger, Latitude, Longitude]));
                                      Inc(Added);
                                    end;
                                end;
                            end
                          else
                            begin
                              WriteLn(OutFile,
                                      Format('%3d. %20s 0 ---> Would be added (%10.6n, %10.6n)',
                                             [Added, fldFILE_NAME.AsString, Latitude, Longitude]));
                              Inc(Added);
                            end;
                      end;
                  end;
              end; // end

            if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
              Next;

            if (Count mod 100) = 0 then
              self.UpdateStatus(Format('Processed %d/%d, (updated=%d, added=%d)',
                                      [Count, MaxCount, Updated, Added]), -1 { clYellow });
            inc(Count);
          end;
      end; // with fTempPhotoTable
    UpdateStatus(Format('COMPLETE. Processed %d/%d, (updated=%d, added=%d)',
                         [Count, MaxCount, Updated, Added]), 1 { clBtnFace });
  finally
    fTempPhotoTable.Active := false;
    fTempPhotoTable.Free;
    ImgData.Free;

    CloseFile(OutFile);
    if (Updated + Added) > 0 then
      begin
        temp := Format('Notepad.exe %s', [LogFileName]);
        FileExecute(temp, false);
      end
    else
      Message('No records were updated and no locations were added');
  end;
end;  { TfrmUpdateLocationInfo.btnUpdateSelectedClick }

procedure TfrmUpdateLocationInfo.EnableControls(Control: TWinControl; Enable: boolean);
var
  i: integer;
begin
  Control.Enabled := Enable;
  for i := 0 to Control.ControlCount-1 do
    if Control.Controls[i] is TWinControl then
      EnableControls(Control.Controls[i] as TWinControl, Enable);
end;

procedure TfrmUpdateLocationInfo.rbLoadFromGPXClick(Sender: TObject);
begin
  EnableControls(pnlGPX, rbLoadFromGPX.Checked);
  EnableControls(pnlCSV, rbLoadFromCSV.Checked);
//cbSaveTrackData.Checked := rbLoadFromGPX.Checked;
end;

procedure TfrmUpdateLocationInfo.btnLocateTrackDataClick(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      FileName := leTrackDataLfn.Text;
      if Execute then
        leTrackDataLfn.Text := FileName;
    end;
end;

procedure TfrmUpdateLocationInfo.btnLoadTrackDataClick(Sender: TObject);
var
  Lfn: string;
  NrPointsDeleted: integer;
begin
  if not Assigned(gGPSTrackingInfo) then
    gGPSTrackingInfo := TGPSTrackingInfo.Create
                          (CommonPhotoSettings.DefaultGPXFilter,
                           CommonPhotoSettings.SavedTrackDataLfn);
  with GPSTrackingInfo do
    begin
      OnUpdateStatus        := self.UpdateStatus;
//    ResetTrackingInfo;
      Lfn := CommonPhotoSettings.SavedTrackDataLfn;
      UpdateStatus(Format('Loading %s', [Lfn]), 1 { clYellow });
      LoadTrackData(Lfn, ovcLowestDate.AsDateTime, ovcHighestDate.AsDateTime);
      CommonPhotoSettings.SavedTrackDataLfn := Lfn;
      if TotalWayPoints > 0 then
        SortWayPoints(NrPointsDeleted)
      else
        AlertFmt('No data was found in the date range: %s to %s',
                 [ovcLowestDate.AsString, ovcHighestDate.AsString]);   //
      lblCount.Caption := Format('Points loaded = %0.n, Points deleted from original = %0.n',
        [TotalWayPoints*1.0, NrPointsDeleted*1.0]);
      CurrentIndex := TotalWayPoints;
      lblLowestDate.Caption := Format('%s --> %s',
                                      [DateToStr(LowestDate), DateToStr(HighestDate)]);
    end;
  Enable_Buttons;
end;

procedure TfrmUpdateLocationInfo.leGpxFilterChange(Sender: TObject);
begin
//cbSaveTrackData.Checked := Pos('*', leGpxFilter.Text) > 0;
end;

procedure TfrmUpdateLocationInfo.UpdateStatus(const Msg: string; LineNo: integer = 1);
begin
  if LineNo < 0 then  // highlight in yellow
    lblStatus.Color := clYellow
  else
    if lblStatus.Color = clYellow then
      lblStatus.Color := clBtnFace;
  lblStatus.Caption := Msg;
  Application.ProcessMessages;
end;

destructor TfrmUpdateLocationInfo.Destroy;
begin
  FreeAndNil(gGPSTrackingInfo);

  inherited;
end;

procedure TfrmUpdateLocationInfo.cbSimulateClick(Sender: TObject);
begin
  if cbSimulate.Checked then
    btnUpdateSelected.Caption := 'Count Selected'
  else
    btnUpdateSelected.Caption := 'Update Selected Records';
end;

procedure TfrmUpdateLocationInfo.ovcLowestDateChange(Sender: TObject);
begin
  if Assigned(gGPSTrackingInfo) then
    GPSTrackingInfo.TrackDataLoaded := false;
  Enable_Buttons;
end;

procedure TfrmUpdateLocationInfo.ovcHighestDateChange(Sender: TObject);
begin
  if Assigned(gGPSTrackingInfo) then
    GPSTrackingInfo.TrackDataLoaded := false;
  Enable_Buttons;
end;

procedure TfrmUpdateLocationInfo.cbEXIFDataClick(Sender: TObject);
begin
  if Assigned(gGPSTrackingInfo) then
    GPSTrackingInfo.TrackDataLoaded := false;
  Enable_Buttons;
end;

procedure TfrmUpdateLocationInfo.cbDataBaseClick(Sender: TObject);
begin
  if Assigned(gGPSTrackingInfo) then
    GPSTrackingInfo.TrackDataLoaded := false;
  Enable_Buttons;
end;

procedure TfrmUpdateLocationInfo.btnClearPreviouslyLoadedTracksClick(
  Sender: TObject);
begin
  FreeAndNil(gGPSTrackingInfo);
  Enable_Buttons;
end;

procedure TfrmUpdateLocationInfo.btnSaveTrackDataClick(Sender: TObject);
begin
  if Assigned(gGPSTrackingInfo) then
    with gGPSTrackingInfo do
      SaveTrackData(SavedTrackDataLfn);
end;

procedure TfrmUpdateLocationInfo.btnSaveTrackDataAsClick(Sender: TObject);
var
  FilePath: string;
begin
  if Assigned(gGPSTrackingInfo) then
    with gGPSTrackingInfo do
      begin
        FilePath := SavedTrackDataLfn;
        if BrowseForFile('Save Track Data As', FilePath, 'CSV') then
          SaveTrackData(FilePath);
      end;
end;

procedure TfrmUpdateLocationInfo.cbScanSelectedSubFoldersClick(Sender: TObject);
begin
  leSelectedFolders.Enabled := cbScanSelectedSubFolders.Checked;
end;

end.
