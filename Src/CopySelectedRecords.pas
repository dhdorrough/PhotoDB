unit CopySelectedRecords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, LocationUtils;

const
  LOC_BASED_PATTERN = '%X';  

type
  TOverWriteMode = (om_ChangeFileNameRatherThanOverwriting, om_NeverOverwrite,
                    om_AlwaysOverwrite, om_AskToOverwriteDuplicateFiles);

  TBasedOnLatLon = record
    FileNameBasedOnLatLon: boolean;
    Direction: TDirection;
  end;

  TfrmCopySelectedFiles = class(TForm)
    leDestinationFilePath: TLabeledEdit;
    btnBrowse: TButton;
    cbRetainFolderStructure: TCheckBox;
    BitBtn1: TBitBtn;
    btnBegin: TBitBtn;
    cbGenNames: TCheckBox;
    leFileNamePattern: TLabeledEdit;
    cbCreateLink: TCheckBox;
    GroupBox1: TGroupBox;
    cbAskToOverwriteDuplicateFiles: TRadioButton;
    cbChangeFileNameRatherThanOverwriting: TRadioButton;
    cbAlwaysOverwrite: TRadioButton;
    cbNeverOverwrite: TRadioButton;
    cbCopyAssociatedWAVFile: TCheckBox;
    cbCopyOnlyPhotoFiles: TCheckBox;
    cbDeleteAfterCopying: TCheckBox;
    cbChangeFileDateToPhotoDAte: TCheckBox;
    pnlFileNameBasedOnLatLon: TPanel;
    cbBasedOnLatitudeStoN: TRadioButton;
    cbBasedOnLatitudeNtoS: TRadioButton;
    cbBasedOnLongitudeWtoE: TRadioButton;
    cbBasedOnLongitudeEtoW: TRadioButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure cbGenNamesClick(Sender: TObject);
    procedure leDestinationFilePathChange(Sender: TObject);
    procedure leFileNamePatternChange(Sender: TObject);
    procedure pnlFileNameBasedOnLatLonClick(Sender: TObject);
  private
    fOverwriteMode: TOverwriteMode;
    function GetFilePath: string;
    procedure SetFilePath(const Value: string);
    function GetGenFileNames: boolean;
    procedure SetGenFileNames(const Value: boolean);
    function GetFileNamePattern: string;
    procedure SetFileNamePattern(const Value: string);
    function GetCreateLinks: boolean;
    procedure SetCreateLinks(const Value: boolean);
    function GetOverWriteMode: TOverWriteMode;
    procedure SetOverWriteMode(const Value: TOverWriteMode);
    function GetCopyAssociatedWAVFile: boolean;
    procedure SetCopyAssociatedWAVFile(const Value: boolean);
    function GetCopyOnlyPhotoFiles: boolean;
    procedure SetCopyOnlyPhotoFiles(const Value: boolean);
    procedure SetChangeFileDateToPhotoDate(const Value: boolean);
    function GetChangeFileDateToPhotoDate: boolean;
    function GetDeleteAfterCopying: boolean;
    procedure SetDeleteAfterCopying(const Value: boolean);
    function GetBasedOnLatLon: TBasedOnLatLon;
    procedure SetBasedOnLatLon(const Value: TBasedOnLatLon);
    function LocationInfoIsComplete: boolean;
    function LocationInfoNeeded: boolean;
    procedure Enable_Buttons;
    { Private declarations }
  public
    { Public declarations }
    property FilePath: string
             read GetFilePath
             write SetFilePath;
    property GenFileNames: boolean
             read GetGenFileNames
             write SetGenFileNames;
    property FileNamePattern: string
             read GetFileNamePattern
             write SetFileNamePattern;
    property CreateLinks: boolean
             read GetCreateLinks
             write SetCreateLinks;
    property CopyAssociatedWAVFile: boolean
             read GetCopyAssociatedWAVFile
             write SetCopyAssociatedWAVFile;
    property OverWriteMode: TOverWriteMode
             read GetOverWriteMode
             write SetOverWriteMode;
    property CopyOnlyPhotoFiles: boolean
             read GetCopyOnlyPhotoFiles
             write SetCopyOnlyPhotoFiles;
    property ChangeFileDateToPhotoDate: boolean
             read GetChangeFileDateToPhotoDate
             write SetChangeFileDateToPhotoDate;
    property DeleteAfterCopying: boolean
             read GetDeleteAfterCopying
             write SetDeleteAfterCopying;
    property BasedOnLatLon: TBasedOnLatLon
             read GetBasedOnLatLon
             write SetBasedOnLatLon;
    Constructor Create(aOwner: TComponent); override;
  end;

var
  frmCopySelectedFiles: TfrmCopySelectedFiles;

implementation

uses MyUtils;

{$R *.dfm}

procedure TfrmCopySelectedFiles.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := leDestinationFilePath.Text;
  if BrowseForFolder('Locate destination folder', Temp) then
    leDestinationFilePath.Text := Temp;
end;

function TfrmCopySelectedFiles.GetFilePath: string;
begin
  result := leDestinationFilePath.Text;
end;

procedure TfrmCopySelectedFiles.SetFilePath(const Value: string);
begin
  leDestinationFilePath.Text := Value;
end;

function TfrmCopySelectedFiles.GetGenFileNames: boolean;
begin
  result := cbGenNames.Checked;
end;

procedure TfrmCopySelectedFiles.SetGenFileNames(const Value: boolean);
begin
  cbGenNames.Checked := Value;
end;

function TfrmCopySelectedFiles.GetFileNamePattern: string;
begin
  if cbGenNames.Checked then
    result := UpperCase(leFileNamePattern.Text)
  else
    result := '%f%y%m%d';
end;

procedure TfrmCopySelectedFiles.SetFileNamePattern(const Value: string);
begin
  leFileNamePattern.Text := Value;
end;

function TfrmCopySelectedFiles.GetCreateLinks: boolean;
begin
  result := cbCreateLink.Checked;
end;

procedure TfrmCopySelectedFiles.SetCreateLinks(const Value: boolean);
begin
  cbCreateLink.Checked := Value;
end;

procedure TfrmCopySelectedFiles.cbGenNamesClick(Sender: TObject);
begin
  leFileNamePattern.Visible := cbGenNames.Checked;
  if cbGenNames.Checked then
    begin
      pnlFileNameBasedOnLatLon.Visible := false;
    end;
end;

function TfrmCopySelectedFiles.GetOverWriteMode: TOverWriteMode;
begin
  if cbChangeFileNameRatherThanOverwriting.Checked then
    result := om_ChangeFileNameRatherThanOverwriting else
  if cbNeverOverwrite.Checked then
    result := om_NeverOverwrite else
  if cbAlwaysOverwrite.Checked then
    result := om_AlwaysOverwrite
  else
    result := om_AskToOverwriteDuplicateFiles;
end;

procedure TfrmCopySelectedFiles.SetOverWriteMode(
  const Value: TOverWriteMode);
begin
  case fOverwriteMode of
    om_ChangeFileNameRatherThanOverwriting:
      cbChangeFileNameRatherThanOverwriting.Checked := false;
    om_NeverOverwrite:
      cbNeverOverwrite.Checked := false;
    om_AlwaysOverwrite:
      cbAlwaysOverwrite.Checked := false;
    om_AskToOverwriteDuplicateFiles:
      cbAskToOverwriteDuplicateFiles.Checked := false;
  end;
  case Value of
    om_ChangeFileNameRatherThanOverwriting:
      cbChangeFileNameRatherThanOverwriting.Checked := true;
    om_NeverOverwrite:
      cbNeverOverwrite.Checked := true;
    om_AlwaysOverwrite:
      cbAlwaysOverwrite.Checked := true;
    om_AskToOverwriteDuplicateFiles:
      cbAskToOverwriteDuplicateFiles.Checked := true;
  end;
  fOverwriteMode := Value;
end;

constructor TfrmCopySelectedFiles.Create(aOwner: TComponent);
begin
  inherited;
  OverWriteMode := om_ChangeFileNameRatherThanOverwriting;
end;

function TfrmCopySelectedFiles.GetCopyAssociatedWAVFile: boolean;
begin
  result := cbCopyAssociatedWAVFile.Checked;
end;

procedure TfrmCopySelectedFiles.SetCopyAssociatedWAVFile(
  const Value: boolean);
begin
  cbCopyAssociatedWAVFile.Checked := Value;
end;

function TfrmCopySelectedFiles.GetCopyOnlyPhotoFiles: boolean;
begin
  result := cbCopyOnlyPhotoFiles.Checked;
end;

procedure TfrmCopySelectedFiles.SetCopyOnlyPhotoFiles(
  const Value: boolean);
begin
  cbCopyOnlyPhotoFiles.Checked := Value;
end;

procedure TfrmCopySelectedFiles.SetChangeFileDateToPhotoDate(
  const Value: boolean);
begin
  cbChangeFileDateToPhotoDate.Checked := Value;
end;

function TfrmCopySelectedFiles.GetChangeFileDateToPhotoDate: boolean;
begin
  result := cbChangeFileDateToPhotoDate.Checked;
end;

function TfrmCopySelectedFiles.GetDeleteAfterCopying: boolean;
begin
  result := cbDeleteAfterCopying.Checked;
end;

procedure TfrmCopySelectedFiles.SetDeleteAfterCopying(
  const Value: boolean);
begin
  cbDeleteAfterCopying.Checked := Value;
end;

function TfrmCopySelectedFiles.GetBasedOnLatLon: TBasedOnLatLon;
begin
  result.FileNameBasedOnLatLon := Pos(LOC_BASED_PATTERN, UpperCase(leFileNamePattern.text)) > 0;
  if cbBasedOnLatitudeStoN.Checked then
    result.Direction := td_S_to_N else
  if cbBasedOnLatitudeNtoS.Checked then
    result.Direction := td_N_to_S else
  if cbBasedOnLongitudeEtoW.Checked then
    result.Direction := td_E_to_W else
  if cbBasedOnLongitudeWtoE.Checked then
    result.Direction := td_W_to_E
  else
    result.Direction := td_Unknown;
end;

procedure TfrmCopySelectedFiles.SetBasedOnLatLon(
  const Value: TBasedOnLatLon);
begin

end;

procedure TfrmCopySelectedFiles.leDestinationFilePathChange(
  Sender: TObject);
begin
  Enable_Buttons;
end;

function TfrmCopySelectedFiles.LocationInfoIsComplete: boolean;
begin
  if LocationInfoNeeded then
    result := BasedOnLatLon.Direction <> td_Unknown
  else
    result := true;
end;

function TfrmCopySelectedFiles.LocationInfoNeeded: boolean;
begin
  result := Pos(LOC_BASED_PATTERN, UpperCase(leFileNamePattern.text)) > 0;
end;



procedure TfrmCopySelectedFiles.leFileNamePatternChange(Sender: TObject);
begin
  pnlFileNameBasedOnLatLon.Visible := LocationInfoNeeded;
  Enable_Buttons;
end;

procedure TfrmCopySelectedFiles.pnlFileNameBasedOnLatLonClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmCopySelectedFiles.Enable_Buttons;
begin
  btnBegin.Enabled := not Empty(leDestinationFilePath.Text) and
                          LocationInfoIsComplete;
end;


end.
