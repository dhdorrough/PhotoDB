unit HTMLForRootFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HTMLBase, ComCtrls, StdCtrls, ExtCtrls;

type
  TfrmHTMLForSelectedRootFolder = class(TfrmHTMLBase)
    leRootFolder: TLabeledEdit;
    btnBrowse: TButton;
    btnCountFoldersToBeProcessed: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure leRootFolderChange(Sender: TObject);
    procedure btnCountFoldersToBeProcessedClick(Sender: TObject);
    procedure btnBeginClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    fCancelIt: boolean;
    function GetRootFolder: string;
    procedure SetRootFolder(const Value: string);
    function SubFolderCount(const RootFolderName: string): integer;
    { Private declarations }
  public
    { Public declarations }
    function CountFoldersToBeProcessed: integer; override;
    Constructor Create(Aowner: TComponent); override;
    procedure EnableButtons; override;
    procedure FinalizeProcess; override;
//  procedure GenerateHTMLForSelectedFolders; override;
    property RootFolder: string
             read GetRootFolder
             write SetRootFolder;
  end;

var
  frmHTMLForSelectedRootFolder: TfrmHTMLForSelectedRootFolder;

implementation

uses MyUtils, PhotoDBCommonSettings, PDBTables;

{$R *.dfm}

procedure TfrmHTMLForSelectedRootFolder.btnBrowseClick(Sender: TObject);
var
  FilePath: string;
begin
  inherited;
  if BrowseForFolder('Root Folder to Process', FilePath) then
    begin
      RootFolder := FilePath;
      EnableButtons;
    end;
end;

procedure TfrmHTMLForSelectedRootFolder.EnableButtons;
begin
  inherited;
  btnBegin.Enabled := DirectoryExists(RootFolder);
end;

function TfrmHTMLForSelectedRootFolder.GetRootFolder: string;
begin
  result := RemoveTrailingBackSlash(leRootFolder.text);
end;

procedure TfrmHTMLForSelectedRootFolder.SetRootFolder(const Value: string);
begin
 leRootFolder.text := Value;
end;

procedure TfrmHTMLForSelectedRootFolder.leRootFolderChange(
  Sender: TObject);
begin
  inherited;
  EnableButtons;
end;

function TfrmHTMLForSelectedRootFolder.CountFoldersToBeProcessed: integer;
begin
  result := SubFolderCount(leRootFolder.Text);
end;


function TfrmHTMLForSelectedRootFolder.SubFolderCount(const RootFolderName: string): integer;
var
  SearchRec: TSearchRec;
  DosErr: integer;
  SubFolderName: string;
begin
  result := 0;
  if not fCancelIt then
    begin
      if (FolderCount mod 100) = 0 then
        UpdateProgressInfo('Counted', FolderCount);

      FolderCount := FolderCount + 1;
      DosErr      := FindFirst(RootFolderName + '\*.*', faDirectory, SearchRec);
      result      := 1;   // we have at least the current directory
      try
        while DosErr = 0 do
          begin
            if (not (SearchRec.Name = '.')) and
               (not (SearchRec.Name = '..')) and
               (not SameText(SearchRec.Name, 'TN')) then
              if (SearchRec.Attr and faDirectory) <> 0 then
                begin
                  SubFolderName := RootFolderName + '\' + SearchRec.Name;
                  result := result + SubFolderCount(SubFolderName);
                end;
            DosErr := FindNext(SearchRec);
          end;
      finally
        FindClose(SearchRec);
      end;
    end;
end;

procedure TfrmHTMLForSelectedRootFolder.btnCountFoldersToBeProcessedClick(Sender: TObject);
begin
  inherited;
  UpdateStatus('Counting folders to be processed');
  try
    FolderCount := 0;
    btnBegin.Enabled := false;
    btnCountFoldersToBeProcessed.Enabled := false;
    TotalFoldersToProcess := CountFoldersToBeProcessed;
    lblProgressInfo.Caption := Format('%d (or more) folders to process', [TotalFoldersToProcess]);
    UpdateStatus('Count completed');
  finally
    btnCountFoldersToBeProcessed.Enabled := true;
    btnBegin.Enabled := true;
    fCancelIt        := false;
  end;
end;

procedure TfrmHTMLForSelectedRootFolder.btnBeginClick(Sender: TObject);
begin
  inherited;
  btnBegin.Enabled := false;
  fCancelIt        := false;
  UpdateProgressInfo('Processing', FolderCount);

  ProcessFolder(ExtractFilePath(RootFolder), ExtractFileName(RootFolder), RootFolder, '*.*');
end;

procedure TfrmHTMLForSelectedRootFolder.btnCancelClick(Sender: TObject);
begin
  inherited;
  fCancelIt := true;
end;

procedure TfrmHTMLForSelectedRootFolder.FinalizeProcess;
begin
  inherited;
end;

constructor TfrmHTMLForSelectedRootFolder.Create(Aowner: TComponent);
begin
  inherited;
  InitParams;
  RootFolder := CommonPhotoSettings.PhotoDBFolder;
end;

end.
