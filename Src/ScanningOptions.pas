unit ScanningOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PDB_Decl, Mask;

type
  TDisplayOption = (doNone, doNoFileNameInfo);

  TDisplayOptions = set of TDisplayOption;
  
  TfrmScanningOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    pnlLocation: TGroupBox;
    cbSaveUpdatedTrackLog: TCheckBox;
    cbScanSelectedSubFolders: TCheckBox;
    leSelectedSubFolders: TLabeledEdit;
    leMinutesToAdd: TLabeledEdit;
    leDefaultArea: TLabeledEdit;
    cbUseExifDateForPhotoDate: TCheckBox;
    cbUseFileCreationDateForPhotoDate: TCheckBox;
    cbUseSavedTracksLogFile: TCheckBox;
    cbUseGPXLogsForLocation: TCheckBox;
    Label2: TLabel;
    cbUseFileModificationDateForPhotoDate: TCheckBox;
    cbUseFileMediaInfoForPhotoDate: TCheckBox;
    leDefaultState: TLabeledEdit;
    pnlFileNameInfo: TPanel;
    cbExtractLatLon: TCheckBox;
    cbCheckForDate: TCheckBox;
    cbExtractComments: TCheckBox;
    cbIncludeFileName: TCheckBox;
    cbIncludeFilePath: TCheckBox;
    Label1: TLabel;
    pnlOwner: TPanel;
    cbPrivate: TCheckBox;
    leOwner: TMaskEdit;
    Label3: TLabel;
    cbExtractCustomKey: TCheckBox;
    leDefaultKeyWords: TLabeledEdit;
    procedure cbScanSelectedSubFoldersClick(Sender: TObject);
    procedure cbUseGPXLogsForLocationClick(Sender: TObject);
    procedure cbUseSavedTracksLogFileClick(Sender: TObject);
    procedure cbUseExifDateForPhotoDateClick(Sender: TObject);
  private
    function GetDefaultCopyID: string;
    procedure SetDefaultCopyID(const Value: string);
    procedure Enable_Buttons;
    function GetPrivacy: boolean;
    procedure SetPrivacy(const Value: boolean);
    { Private declarations }
  public
    { Public declarations }
    property DefaultCopyID: string
             read GetDefaultCopyID
             write SetDefaultCopyID;
    property Privacy: boolean
             read GetPrivacy
             write SetPrivacy;
    constructor Create(aOwner: TComponent; DisplayOptions: TDisplayOptions); reintroduce; 
  end;

//function GetScanningOptions(var FileNameInfo: TFileNameInfo): boolean;
function GetScanningOptions( var UpdateInfo: TUpdateInfo;
                             var LocationInfo: TLocationInfo;
                             const DisplayOptions: TDisplayOptions): boolean;

implementation

uses PDBTables, PhotoDBCommonSettings, MyUtils;

{$R *.dfm}

function TfrmScanningOptions.GetDefaultCopyID: string;
begin
  result := Trim(leOwner.Text);
end;

//function GetScanningOptions(var FileNameInfo: TFileNameInfo): boolean;
function GetScanningOptions( var UpdateInfo: TUpdateInfo;
                             var LocationInfo: TLocationInfo;
                             const DisplayOptions: TDisplayOptions): boolean;
var
  frm: TfrmScanningOptions;
begin
  frm := TfrmScanningOptions.Create(nil, DisplayOptions);
  with frm do
    begin
      DefaultCopyID := CommonPhotoSettings.CopyRightID;
      with UpdateInfo.FileNameInfo do
        begin
          cbIncludeFilePath.Checked         := IncludeFilePath;
          cbIncludeFileName.Checked         := IncludeFileName;
          cbCheckForDate.Checked            := dk_From_FileName in DateKinds;
          cbUseExifDateForPhotoDate.Checked := dk_From_EXIF in DateKinds;
          cbUseFileCreationDateForPhotoDate.Checked := dk_Date_Created in DateKinds;
          cbUseFileModificationDateForPhotoDate.Checked := dk_Date_Modified in DateKinds;
          cbUseFileMediaInfoForPhotoDate.Checked := dk_MediaInfo in DateKinds;
          cbExtractComments.Checked         := ExtractComments;
          cbExtractLatLon.Checked           := LocationInfo.ExtractLatLonFromFileName;
          cbExtractCustomKey.Checked        := ExtractCustomKey;
          cbUseGPXLogsForLocation.Checked   := LocationInfo.UseGPXLogsForLocation;
          cbUseSavedTracksLogFile.Checked   := LocationInfo.UseSavedTracksLogFile;
          leDefaultArea.Text                := LocationInfo.DefaultLocation;
          leDefaultKeyWords.Text            := LocationInfo.DefaultKeyWords;
          leDefaultState.Text               := LocationInfo.DefaultState;

//        cbScanAllSubFolders.Checked       := LocationInfo.ScanAllSubFolders;
          cbScanSelectedSubFolders.Checked  := LocationInfo.ScanSelectedSubFolders;
          cbSaveUpdatedTrackLog.Checked     := LocationInfo.SaveUpdatedTrackLog;
          leSelectedSubFolders.Text         := LocationInfo.SelectedSubFolders;
          DefaultCopyID                     := UpdateInfo.PhotoOwner;
          Privacy                           := UpdateInfo.isPrivate;
          leMinutesToAdd.Text               := IntToStr(UpdateInfo.MinutesToAdd);
        end;

      result := ShowModal = mrOk;
      if result then
        with UpdateInfo.FileNameInfo do
          begin
            IncludeFilePath         := cbIncludeFilePath.Checked;
            IncludeFileName         := cbIncludeFileName.Checked;
            if cbCheckForDate.Checked then
              DateKinds := DateKinds + [dk_From_FileName];
            if cbUseExifDateForPhotoDate.Checked then
              DateKinds := DateKinds + [dk_From_EXIF];
            if cbUseFileCreationDateForPhotoDate.Checked then
              DateKinds := DateKinds + [dk_Date_Created];
            if cbUseFileModificationDateForPhotoDate.Checked then
              DateKinds := DateKinds + [dk_Date_Modified];
            if cbUseFileMediaInfoForPhotoDate.Checked then
              DateKinds := DateKinds + [dk_MediaInfo];
            ExtractComments         := cbExtractComments.Checked;
            ExtractCustomKey        := cbExtractCustomKey.Checked;
            LocationInfo.ExtractLatLonFromFileName           := cbExtractLatLon.Checked;
            UpdateInfo.PhotoOwner   := DefaultCopyID;
            UpdateInfo.isPrivate    := Privacy;
            LocationInfo.UseGPXLogsForLocation   := cbUseGPXLogsForLocation.Checked;
            LocationInfo.UseSavedTracksLogFile   := cbUseSavedTracksLogFile.Checked;
            LocationInfo.DefaultLocation := leDefaultArea.Text;
            LocationInfo.DefaultState := leDefaultState.Text;
            if not Empty(LocationInfo.DefaultLocation) then
              LocationInfo.DefaultLocationKind := dl_SpecifiedValue;

            LocationInfo.SaveUpdatedTrackLog     := cbSaveUpdatedTrackLog.Checked;
//          LocationInfo.ScanSubFolders          := cbScanSubFolders.Checked;
            LocationInfo.ScanSelectedSubFolders  := cbScanSelectedSubFolders.Checked;
            LocationInfo.SelectedSubFolders      := leSelectedSubFolders.Text;
            LocationInfo.MaxDistanceInFeet       := MAXDISTANCEINFEET;
            LocationInfo.DefaultKeyWords         := leDefaultKeyWords.Text;
            try
              UpdateInfo.MinutesToAdd := StrToInt(leMinutesToAdd.Text);
            except
              AlertFmt('Invalid value [%s] for minutes to add', [leMinutesToAdd.Text]);
              UpdateInfo.MinutesToAdd := 0;
            end;
          end;
    end;
  FreeAndNil(frm);
end;

procedure TfrmScanningOptions.SetDefaultCopyID(const Value: string);
begin
  leOwner.Text := Trim(value);
end;

procedure TfrmScanningOptions.cbScanSelectedSubFoldersClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmScanningOptions.cbUseGPXLogsForLocationClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmScanningOptions.Enable_Buttons;
var
  b: boolean;
begin
  b                                := cbUseGPXLogsForLocation.Checked or cbUseSavedTracksLogFile.Checked;
  cbSaveUpdatedTrackLog.Enabled    := cbUseGPXLogsForLocation.Checked;
  cbScanSelectedSubFolders.Enabled := cbUseGPXLogsForLocation.Checked;
  leSelectedSubFolders.Enabled     := cbUseGPXLogsForLocation.Checked and cbScanSelectedSubFolders.Checked;
  leMinutesToAdd.Enabled           := b;
  leDefaultArea.Enabled            := b or cbUseExifDateForPhotoDate.Checked;
  leDefaultState.Enabled           := leDefaultArea.Enabled;
end;

procedure TfrmScanningOptions.cbUseSavedTracksLogFileClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

function TfrmScanningOptions.GetPrivacy: boolean;
begin
  result := cbPrivate.Checked;
end;

procedure TfrmScanningOptions.SetPrivacy(const Value: boolean);
begin
  cbPrivate.Checked := Value;
end;

constructor TfrmScanningOptions.Create(aOwner: TComponent;
  DisplayOptions: TDisplayOptions);
var
  b: boolean;
begin
  inherited Create(aOwner);

  b := not (doNoFileNameInfo in DisplayOptions);

  pnlFileNameInfo.Visible := b;
  pnlOwner.Visible        := b;

  if not b then
    Caption := 'Location Search Info';
end;

procedure TfrmScanningOptions.cbUseExifDateForPhotoDateClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

end.
