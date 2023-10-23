unit uPhotoDBOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus, PDB_Decl;

type
  TfrmPhotoDBOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    tsPhotoDB: TTabSheet;
    tsMakeHTML: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtPhotoMDB: TEdit;
    btnBrowseToPhotoDB: TButton;
    edtRoot: TEdit;
    btnBrowseToRoot: TButton;
    edtPhotoEditingProgram: TEdit;
    btnBrowseToPhotoEditingProgram: TButton;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    odLocalCSS: TOpenDialog;
    odSynonymsFileName: TOpenDialog;
    SaveDialog3: TSaveDialog;
    SaveDialog4: TSaveDialog;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    edtFolderNo: TEdit;
    Label2: TLabel;
    btnScan: TButton;
    cbReferenceLocal: TCheckBox;
    leLocalCSS: TLabeledEdit;
    leRemoteCSS: TLabeledEdit;
    btnLocalCSS: TButton;
    tsLocation: TTabSheet;
    leTrackDataLfn: TLabeledEdit;
    btnLocateTrackData: TBitBtn;
    lblStatus: TLabel;
    lblPathToHikingMDB: TLabel;
    edtHikingMDB: TEdit;
    btnBrowseToHikingDB: TButton;
    leGPXFolder: TLabeledEdit;
    btnGPXFolder: TButton;
    leCopyRight: TLabeledEdit;
    PopupMenu1: TPopupMenu;
    IsDHD1: TMenuItem;
    leCopyRightID: TLabeledEdit;
    tsInternet: TTabSheet;
    leWebSiteURL: TLabeledEdit;
    leWebsiteUserID: TLabeledEdit;
    leWebsitePassword: TLabeledEdit;
    leWebSiteFilePath: TLabeledEdit;
    RadioGroup1: TRadioGroup;
    rbLatLong: TRadioButton;
    rbLastWord: TRadioButton;
    rbSpecifiedValue: TRadioButton;
    rbHikingLogAreaForTheDate: TRadioButton;
    edtSpecifiedValue: TEdit;
    edtImportExportFolder: TEdit;
    btnImportExportFolder: TButton;
    Label5: TLabel;
    edtAlternatePhotoEditingProgram: TEdit;
    Label6: TLabel;
    btnBrowseToAlternatePhotoEditingProgram: TButton;
    Label7: TLabel;
    procedure btnBrowseToPhotoDBClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnBrowseToPhotoEditingProgramClick(Sender: TObject);
    procedure btnBrowseToRootClick(Sender: TObject);
    procedure btnLocalCSSClick(Sender: TObject);
    procedure cbReferenceLocalClick(Sender: TObject);
    procedure btnLocateTrackDataClick(Sender: TObject);
    procedure btnBrowseToHikingDBClick(Sender: TObject);
    procedure btnGPXFolderClick(Sender: TObject);
    procedure IsDHD1Click(Sender: TObject);
    procedure rbLatLongClick(Sender: TObject);
    procedure rbLastWordClick(Sender: TObject);
    procedure rbSpecifiedValueClick(Sender: TObject);
    procedure rbHikingLogAreaForTheDateClick(Sender: TObject);
    procedure btnImportExportFolderClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnBrowseToAlternatePhotoEditingProgramClick(
      Sender: TObject);
  private
    fIsDHD: boolean;
    procedure SetIsDHD(const Value: boolean);
    procedure Enable_Buttons;
    function GetDefaultLocationKind: TDefaultLocationKind;
    procedure SetDefaultLocationKind(const Value: TDefaultLocationKind);
    { Private declarations }
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    property IsDHD: boolean
             read fIsDHD
             write SetIsDHD;
    property DefaultLocationKind: TDefaultLocationKind
             read GetDefaultLocationKind
             write SetDefaultLocationKind;
  end;

implementation

{$R *.DFM}

uses
  PDBTables, DB, PDBUtils, FileCtrl, MyUtils, PhotoDBCommonSettings,
  GPSTrackingInfoUnit, HikingSettingsUnit, Hiking_Decl, UploadFiles_Decl;

procedure TfrmPhotoDBOptions.btnBrowseToPhotoDBClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      InitialDir := edtRoot.Text;
      FileName   := edtPhotoMDB.Text;
      DefaultExt := '.mdb';
{$If MSACCESS_VERSION = 2000}
      Filter     := 'MS Access DB (*.mdb)|*.mdb';
{$IfEnd}
{$If MSACCESS_VERSION = 2007}
      Filter     := 'MS Access DB (*.mdb)|*.mdb|MS Access 2007 DB (*.accdb)|*.accdb';
{$IfEnd}
      if Execute then
        begin
          edtPhotoMDB.Text := FileName;
//        AdjustRelativePaths;
        end;
    end;
end;

procedure TfrmPhotoDBOptions.btnScanClick(Sender: TObject);
var
  HighestPath_No: integer;
  DatabaseFileName: string;
begin
  if FileExists(edtPhotoMDB.Text) then
    DatabaseFileName := edtPhotoMDB.Text
  else
    DatabaseFileName := CommonPhotoSettings.PhotoDBDatabaseFileName;

  lblStatus.Caption := 'Now scanning';
  HighestPath_No    := ScanForHighestFolderNumber(CommonPhotoSettings.FolderNo, DatabaseFileName);
  edtFolderNo.Text  := IntToStr(HighestPath_No);
  lblStatus.Caption := 'Complete';
end;

constructor TfrmPhotoDBOptions.Create(aOwner: TComponent);
begin
  inherited;

  if not Empty(gRootPath) then
    edtRoot.Text := RemoveTrailingBackSlash(gRootPath)
  else
    edtRoot.Text := RemoveTrailingBackSlash(gExePath);
    
  if DirectoryExists(edtRoot.Text) then
    ChDir(edtRoot.Text);

  edtPhotoMDB.Text           := CommonPhotoSettings.PhotoDBDatabaseFileName;
{$IfDef DHD}
  edtHikingMDB.Visible       := true;
  edtHikingMDB.Text          := CommonPhotoSettings.HikingDBDatabaseFileName;
  lblPathToHikingMDB.Visible := true;
  tsInternet.TabVisible      := true;
{$Else}
  edtHikingMDB.Visible       := false;
  edtHikingMDB.Text          := CommonPhotoSettings.HikingDBDatabaseFileName;
  lblPathToHikingMDB.Visible := false;
  tsInternet.TabVisible      := false;
  btnBrowseToHikingDB.Visible := false;
{$EndIf}
  edtImportExportFolder.Text := CommonPhotoSettings.ImportExportFolder;
  leGPXFolder.Text           := CommonPhotoSettings.DefaultGPXFilter;
  leCopyRight.Text           := CommonPhotoSettings.CopyRight;
  leCopyRightID.Text         := CommonPhotoSettings.CopyRightID;

  if not Empty(CommonPhotoSettings.PhotoEditingProgram) then
    edtPhotoEditingProgram.Text := CommonPhotoSettings.PhotoEditingProgram
  else
    edtPhotoEditingProgram.Text := cPhotoEditingProgram;

  edtFolderNo.Text := IntToStr(CommonPhotoSettings.FolderNo);
  leLocalCSS.Text  := CommonPhotoSettings.LocalCssLfn;
  leRemoteCSS.Text := CommonPhotoSettings.RemoteCssLfn;
  DefaultLocationKind     := CommonPhotoSettings.DefaultLocationKind;

  if Empty(CommonPhotoSettings.SavedTrackDataLfn) then
    leTrackDataLfn.Text := DEF_SAVEDTRACKSFILE
  else
    leTrackDataLfn.Text := CommonPhotoSettings.SavedTrackDataLfn;

  if Empty(CommonPhotoSettings.WebSiteURL) then
    leWebsiteURL.Text := FTP_URL
  else
    leWebsiteURL.Text := CommonPhotoSettings.WebsiteURL;

  if Empty(CommonPhotoSettings.WebSiteUserID) then
    leWebsiteUserID.Text := FTP_USERNAME
  else
    leWebsiteUserID.Text := CommonPhotoSettings.WebSiteUserID;

  if Empty(CommonPhotoSettings.WebsitePassword) then
    leWebSitePassword.Text := FTP_PASSWORD
  else
    leWebSitePassWord.Text := CommonPhotoSettings.WebSitePassword;

  if Empty(CommonPhotoSettings.WebSiteFilePath) then
    leWebSiteFilePath.Text := REMOTE_FILE_PATH
  else
    leWebSiteFilePath.Text := CommonPhotoSettings.WebSiteFilePath;

  cbReferenceLocal.Checked := CommonPhotoSettings.UseLocalCSS;
end;

procedure TfrmPhotoDBOptions.btnOkClick(Sender: TObject);
begin
  gRootPath := RemoveTrailingBackSlash(edtRoot.Text);  // never have trailing backslash on gRootPath

  CommonPhotoSettings.PhotoDBFolder            := gRootPath;

  CommonPhotoSettings.PhotoDBDatabaseFileName  := RemoveTrailingBackSlash(edtPhotoMDB.Text);  // gPhotoDBDatabaseFileName
  CommonPhotoSettings.HikingDBDatabaseFileName := edtHikingMDB.Text;
  CommonPhotoSettings.ImportExportFolder       := edtImportExportFolder.Text;
  CommonPhotoSettings.DefaultGPXFilter         := leGPXFolder.Text;
  CommonPhotoSettings.CopyRight                := leCopyRight.Text;
  CommonPhotoSettings.CopyRightID              := leCopyRightID.Text;

  CommonPhotoSettings.LocalCssLfn              := leLocalCSS.Text;   // gLocalCssLfn
  CommonPhotoSettings.RemoteCssLfn             := leRemoteCSS.Text;
  CommonPhotoSettings.PhotoEditingProgram      := edtPhotoEditingProgram.Text;
  CommonPhotoSettings.UseLocalCss              := cbReferenceLocal.Checked;

  CommonPhotoSettings.DefaultGPXFilter         := leGPXFolder.Text;
  CommonPhotoSettings.SavedTrackDataLfn        := leTrackDataLfn.Text;

  CommonPhotoSettings.WebsiteURL               := leWebsiteURL.Text;
  CommonPhotoSettings.WebSiteUserID            := leWebsiteUserID.Text;
  CommonPhotoSettings.WebSitePassword          := leWebSitePassWord.Text;
  CommonPhotoSettings.WebSiteFilePath          := leWebSiteFilePath.Text;

  CommonPhotoSettings.DefaultLocationKind      := DefaultLocationKind;

//  := edtSpecifiedValue.Text;

  try
    CommonPhotoSettings.FolderNo := StrToInt(edtFolderNo.Text);
  except
    raise Exception.CreateFmt('Invalid Path Number: %s', [edtFolderNo.Text]);
  end;
end;

procedure TfrmPhotoDBOptions.btnBrowseToPhotoEditingProgramClick(
  Sender: TObject);
begin
  with SaveDialog1 do
    begin
      FileName    := edtPhotoEditingProgram.Text;
      DefaultExt := '.exe';
      Filter     := 'Executable (*.exe)|*.exe';
      if Execute then
        edtPhotoEditingProgram.Text := FileName;
    end;
end;

procedure TfrmPhotoDBOptions.btnBrowseToRootClick(Sender: TObject);
var
  temp: string;
begin
  Temp := edtRoot.Text;
  if BrowseForFolder('Root Path', temp) then
    begin
      edtRoot.Text := temp;
//    AdjustRelativePaths;
    end;
end;

procedure TfrmPhotoDBOptions.btnLocalCSSClick(Sender: TObject);
begin
  with odLocalCSS do
    begin
      FileName    := leLocalCSS.Text;
      DEfaultExt  := '.css';
      Filter      := 'Cascading Style Sheet (*.css)|*.css';
      if Execute then
        leLocalCSS.Text := FileName;
    end;
end;

procedure TfrmPhotoDBOptions.cbReferenceLocalClick(Sender: TObject);
begin
  leLocalCSS.Enabled  := cbReferenceLocal.Checked;
  btnLocalCSS.Enabled := cbReferenceLocal.Checked;
  leRemoteCSS.Enabled := not cbReferenceLocal.Checked;
end;

procedure TfrmPhotoDBOptions.btnLocateTrackDataClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      FileName   := leTrackDataLfn.Text;
      DefaultExt := '.csv';
      Filter     := 'Track Data File (*.csv)|*.csv';
      if Execute then
        leTrackDataLfn.Text := FileName;
    end;
end;

procedure TfrmPhotoDBOptions.btnBrowseToHikingDBClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      FileName   := edtHikingMDB.Text;
      DefaultExt := '.mdb';
{$If MSACCESS_VERSION = 2000}
      Filter     := 'MS Access DB (*.mdb)|*.mdb';
{$IfEnd}
{$If MSACCESS_VERSION = 2007}
      Filter     := 'MS Access DB (*.mdb)|*.mdb|MS Access 2007 DB (*.accdb)|*.accdb';
{$IfEnd}
      if Execute then
        edtHikingMDB.Text := FileName;
    end;
end;

procedure TfrmPhotoDBOptions.btnGPXFolderClick(Sender: TObject);
var
  Temp: string;
begin
  if BrowseForFolder('Path to .GPX folder', temp) then
    leGPXFolder.Text := ForceBackSlash(temp) + '*.gpx';
end;

procedure TfrmPhotoDBOptions.SetIsDHD(const Value: boolean);
begin
  fIsDHD                     := Value;
  IsDHD1.Checked             := Value;
  lblPathToHikingMDB.Visible := fIsDHD;
  edtHikingMDB.Visible       := fIsDHD;
  leTrackDataLfn.Visible     := fIsDHD;
  leGPXFolder.Visible        := fIsDHD;
  leRemoteCSS.Visible        := fIsDHD;
  leLocalCSS.Visible         := fIsDHD;
  cbReferenceLocal.Visible   := fIsDHD;
end;

procedure TfrmPhotoDBOptions.IsDHD1Click(Sender: TObject);
begin
  IsDHD := not IsDHD;
end;

procedure TfrmPhotoDBOptions.rbLatLongClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmPhotoDBOptions.Enable_Buttons;
begin
  edtSpecifiedValue.Enabled := rbSpecifiedValue.Checked;
end;


procedure TfrmPhotoDBOptions.rbLastWordClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmPhotoDBOptions.rbSpecifiedValueClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmPhotoDBOptions.rbHikingLogAreaForTheDateClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

function TfrmPhotoDBOptions.GetDefaultLocationKind: TDefaultLocationKind;
begin
  if rbLatLong.Checked then
    result := dl_LatLong else
  if rbLastWord.Checked then
    result := dl_LastWord else
  if rbSpecifiedValue.Checked then
    result := dl_SpecifiedValue else
    result := dl_LastWord;
end;

procedure TfrmPhotoDBOptions.SetDefaultLocationKind(
  const Value: TDefaultLocationKind);
begin
  case Value of
    dl_LatLong:
      rbLatLong.Checked := true;
    dl_LastWord:
      rbLastWord.Checked := true;
    dl_SpecifiedValue:
      rbSpecifiedValue.Checked := true;
(*
    dl_UseHikingLogArea:
      rbHikingLogAreaForTheDate.Checked := true;
*)
  end;
end;

procedure TfrmPhotoDBOptions.btnImportExportFolderClick(Sender: TObject);
var
  temp: string;
begin
  Temp := edtImportExportFolder.Text;
  if BrowseForFolder('Import/Export Path', temp) then
    begin
      edtImportExportFolder.Text := temp;
//    AdjustRelativePaths;
    end;
end;

procedure TfrmPhotoDBOptions.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

  procedure CheckForValidFile(lfn: string; TabSheet: TTabSheet; MustExist: boolean = false);
  begin
    if not FileExists(Lfn) then
      begin
        if MustExist then
          begin
            PageControl1.ActivePage := TabSheet;
            AlertFmt('The specified file "%s" does not exist', [Lfn]);
            CanClose := false;
          end
        else
          if not Empty(Lfn) then
            begin
              MessageFmt('The file "%s" does not exist', [Lfn]);
              CanClose := true;
            end;
      end;
  end;

begin
  if ModalResult = mrOk then
    begin
      CheckForValidFile(leTrackDataLfn.Text, tsLocation);
      CheckForValidFile(edtPhotoMDB.Text,    tsPhotoDB, true);
      CheckForValidFile(edtHikingMDB.Text,   tsPhotoDB);
//    CheckForValidFile(edtPhotoEditingProgram.Text, tsPhotoDB, true);
      CheckForValidFile(edtAlternatePhotoEditingProgram.Text, tsPhotoDB);
      CheckForValidFile(leLocalCSS.Text, tsMakeHTML);
    end;
//  CheckForValidFile(leGPXFolder.Text);
end;

procedure TfrmPhotoDBOptions.btnBrowseToAlternatePhotoEditingProgramClick(
  Sender: TObject);
begin
  with SaveDialog1 do
    begin
      FileName    := edtAlternatePhotoEditingProgram.Text;
      DefaultExt := '.exe';
      Filter     := 'Executable (*.exe)|*.exe';
      if Execute then
        edtAlternatePhotoEditingProgram.Text := FileName;
    end;

end;

end.
