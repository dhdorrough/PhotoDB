unit HTMLForSelectedFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HTMLBase, StdCtrls, CheckLst, ComCtrls, ExtCtrls;

const

  HTML_REPORT_NAME = 'HTMLGenerated';

type

  TfrmHTMLForSelectedFiles = class(TfrmHTMLBase)
    ListBox1: TCheckListBox;
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnBeginClick(Sender: TObject);
    procedure ListBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    fFolderList: TStringList;
    fLogFile: TextFile;
    { Private declarations }
  protected
    procedure AfterFolderProcessed(FolderName: string); override;
    procedure GenerateHTMLforSelectedFolders; override;
    procedure Log(const Message: string; LineNo: integer = 0); override;
  public
    { Public declarations }
    function  CountFoldersToBeProcessed: integer; override;
    procedure FinalizeProcess; override;
    procedure InitializeProcess; override;
    Constructor Create(AOwner: TComponent); override;
  end;

var
  frmHTMLForSelectedFiles: TfrmHTMLForSelectedFiles;

implementation

uses PDBTables, PhotoDBCommonSettings, PDB_Decl, PDBUtils, MyUtils;

Procedure TfrmHTMLForSelectedFiles.Log(const Message: string; LineNo: integer = 0);
begin
  WriteLn(fLogFile, Message);
end;

procedure TfrmHTMLForSelectedFiles.InitializeProcess;
var
  aFullFilePath: string;
  aMsg: string;
  i: integer;
  LoopCount: integer;
  OK: boolean;
begin
  inherited;

  UpdateStatus('Building List of Folders to Process');

  fFolderList := TStringList.Create;
  fFolderList.Sorted := true;

  // Build a list of the folders currently selected for processing
  with TempPhotoTable2 do
    begin
      OnUpdateStatus := Log;
      First;
      while not eof do
        begin
//        aFullFilePath := Format('%5d: %s', [fFolderList.Count, FullFilePath]);
          if (fFolderList.IndexOf(FullFilePath) < 0) and (Pos(NONEXISTANTFOLDER, FullFilePath) <= 0) then
            fFolderList.Add(FullFilePath);
          Next;
        end;
    end;

  // if the log file cannot be opened, try a different file name
  LoopCount    := 0;
  OK           := FALSE;
  fLogFileName := HTML_REPORT_NAME;
  repeat
    try
      AssignFile(fLogFile, fLogFileName);
      ReWrite(fLogFile);
      OK := true;
    except
      fLogFileName := UniqueFileName(HTML_REPORT_NAME);
      LoopCount    := LoopCount + 1;
    end;
  until OK or (LoopCount >= 10);

  cbProcessSubFolders.Checked := true;    // force sub-folders to be processed
  cbDeleteOldHTML.Checked     := true;
  for i := 0 to fFolderList.Count-1 do
    begin
      aFullFilePath := Format('%5d: (%4d) %s', [i, SubFolderCount(fFolderList[i]), fFolderList[i]]);
      ListBox1.Items.Add(aFullFilePath);
      ListBox1.Checked[i] := true;
    end;

  Log('');
  aMsg := 'HTML Generated for these folders ';
  if ProcessSubFolders then
    aMsg := aMsg + '(and subfolders)- ';
  Log(aMsg + DateTimeToStr(Now));
  Log('');

  // Delete the unselected items

  for i := ListBox1.Count-1 downto 0 do  // assumes a 1-to-1 relation between ListBox1.Items and fFolderList
    if not ListBox1.Checked[i] then
      fFolderList.Delete(i);

  TotalFoldersToProcess := CountFoldersToBeProcessed;
  UpdateProgressInfo('Counting', 0);

  UpdateStatus('List complete');
end;  { InitializeProcess }


procedure TfrmHTMLForSelectedFiles.GenerateHTMLforSelectedFolders;
var
  i: integer;
  FolderName, ParentFolderPath, FolderPath: string;
begin { GenerateHTMLforSelectedFolders }
  // now process each of the folders selected

  InitParams;
  
  TempPhotoTable2.Filtered         := false;
  TempPhotoTable2.OnFilterRecord   := nil;
  TempPhotoTable2.IndexName        := FILE_NAME;

  i := fFolderList.Count - 1;
  while i >= 0  do
    begin
      FolderPath       := RemoveTrailingBackSlash(fFolderList[i]);
      FolderName       := ExtractFileName(FolderPath);
      ParentFolderPath := GetParentFolderPath(FolderPath);
      try
        try
          ProcessFolder(ParentFolderPath, FolderName, FolderPath, '*.*');
        finally
          UpdateStatus(Format('Processing: %d folders successfully processed', [FolderCount]));
          Log(Format('%s', [FolderPath]));
        end;
      except
        on e:Exception do
          begin
            Log(Format('"%s" error while processing %s', [e.message, FolderPath]));
            ErrorCount := ErrorCount + 1;
          end;
      end;
      Dec(i);
    end;
  UpdateStatus(Format('COMPLETE. %d folders successfully processed', [FolderCount]));
end;  { GenerateHTMLforSelectedFolders }

function TfrmHTMLForSelectedFiles.CountFoldersToBeProcessed: integer;
var
  i: integer;
  TotalFoldersCount, FoldersInThisFolder: integer;
  FileName: string;
begin
  TotalFoldersCount := 0;
  for i := ListBox1.Count-1 downto 0 do
    begin
      FileName                  := fFolderList[i];
      FoldersInThisFolder       := SubFolderCount(FileName) ;
      ListBox1.Items.Objects[i] := TObject(FoldersInThisFolder);
      if ListBox1.Checked[i] then
        TotalFoldersCount   := TotalFoldersCount + FoldersInThisFolder;
    end;
  result := TotalFoldersCount;
end;


{$R *.dfm}

procedure TfrmHTMLForSelectedFiles.AfterFolderProcessed(FolderName: string);
var
  Idx: integer;
begin
  FolderName := FolderName + '\';
  idx := fFolderList.IndexOf(FolderName);
  if idx >= 0 then
    fFolderList.Delete(Idx);
end;


procedure TfrmHTMLForSelectedFiles.FinalizeProcess;
var
  Temp: string;
begin
  inherited;
  FreeAndNil(fFolderList);
  if AbortIt then
    Log('Process cancelled by operator');
    
  if FolderCount > 0 then
    begin
      CloseFile(fLogFile);
      temp := Format('Notepad.exe %s', [fLogFileName]);
      FileExecute(temp, false);
    end;
end;

procedure TfrmHTMLForSelectedFiles.ListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  TheFolderCount: integer;
begin
  inherited;
  Index            := ListBox1.ItemAtPos(Point(X, Y), true);
  TheFolderCount   := Integer(ListBox1.Items.Objects[Index]);
  if ListBox1.Checked[Index] then    // we're going to uncheck it
    Dec(TotalFoldersToProcess, TheFolderCount)
  else                               // we're going to check it
    Inc(TotalFoldersToProcess, TheFolderCount);

  lblProgressInfo.Caption := Format('Total Folders to Process %d folders', [TotalFoldersToProcess]);
end;

procedure TfrmHTMLForSelectedFiles.btnBeginClick(Sender: TObject);
begin
  inherited;
  GenerateHTMLForSelectedFolders;
end;

constructor TfrmHTMLForSelectedFiles.Create(AOwner: TComponent);
begin
  inherited;
  InitParams;
end;

procedure TfrmHTMLForSelectedFiles.ListBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  HoverIndex: Integer;
  MousePoint: TPoint;
begin
  inherited;
  // Convert current X, Y to a TPoint
  MousePoint.X := X;
  MousePoint.Y := Y;

  // Find the item index at this point
  HoverIndex := ListBox1.ItemAtPos(MousePoint, True);

  if HoverIndex <> -1 then
    begin
      // display the number of folders contained
      ListBox1.Hint := Format('(%d) Subfolders: %d', [HoverIndex, Integer(ListBox1.Items.Objects[HoverIndex])]);
      lblStatus.Caption := ListBox1.Hint;
      ListBox1.ShowHint := True;
    end
  else
    ListBox1.ShowHint := False;
end;

end.
