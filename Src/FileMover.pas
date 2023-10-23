unit FileMover;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, PDBTables, StdCtrls, ImgList, Menus;

type
  TNodeType = (nt_File, nt_Folder);

  TNodeFunction = function {ProcessOneNode} (Node: TTreeNode): boolean of object;

  TMyNodeData = class
    Path_NO: integer;
    Parent_No: integer;
    PathName: string;
    Key: integer;
    NodeType: TNodeType;
    Changed: boolean;
  end;

  TfrmFileMover = class(TForm)
    Panel3: TPanel;
    Panel2: TPanel;
    TreeView1: TTreeView;
    ListView1: TListView;
    lblFolderName: TLabel;
    lblStatus: TLabel;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    miSaveSubTreeToFile: TMenuItem;
    PopupMenu2: TPopupMenu;
    RenameFolder1: TMenuItem;
    RenameFile1: TMenuItem;
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListView1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure RenameFile1Click(Sender: TObject);
    procedure SelectThisPhoto1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure miSaveSubTreeToFileClick(Sender: TObject);
    procedure ListView1Deletion(Sender: TObject; Item: TListItem);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    fFilePathsTable: TFilePathsTable;
    fFilesWereRenamed: boolean;
    fFilesWereMoved: boolean;
    fAlreadyScanned: boolean;
    fIndent: integer;
    fSavedPause: integer;
    fSelectedBranch: TMyNodeData;
    fSelectedListItem: TListItem;
    fPhotoTableQuery: TPhotoTableQuery;
    fTempPhotoTable: TPhotoTable;
    fMyNodeDataList: TList;
    fSelectedPhotoKey: integer;
    fOutputFile: TextFile;
    procedure SetSelectedListItem(Sender: TObject);
    function ProcessNode(Node: TTreeNode;
      aFunction: TNodeFunction): boolean;
    function WriteSubTree(Node: TTreeNode): boolean;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    procedure FreeListViewItems;
    procedure FreeTreeViewItems;
    function PhotoTableQuery: TPhotoTableQuery;
    function FilePathsTable: TFilePathsTable;
    function TempPhotoTable: TPhotoTable;
    property SelectedPhotoKey: integer
             read fSelectedPhotoKey
             write fSelectedPhotoKey;
    property FilesWereRenamed: boolean
             read fFilesWereRenamed;
    property FilesWereMoved: boolean
             read fFilesWereMoved;
    procedure RebuildTree;
  end;

var
  frmFileMover: TfrmFileMover;

implementation

uses
  PDB_Decl, MyUtils, StStrL, ADODB, uGetString, uPhotoDB,
  PhotoDBCommonSettings, MyTables_Decl, PDBUtils;

{$R *.dfm}

const
  BACKSLASH = '\';

type
  EFileRenameError = class(Exception);

{ TfrmFileMover }

function RemoveLeadingBackSlash(const s: string): string;
begin
  result := s;
  if (Length(s) > 0) and (s[1] = BACKSLASH) then
    begin
      result := Copy(s, 2, Length(s)-1);
    end;
end;

procedure TfrmFileMover.RebuildTree;
var
  Saved_Cursor: TCursor;
  PathPieces: array[1..100] of string;
  PathPieceCount, i: integer;
  aFilePath: string;
  RootNode: TTreeNode;
  aPath_No: integer;
  MyNodeData: TMyNodeData;
  StartTime, Elapsed: double;

  procedure MyAddNode(CurrentNode: TTreeNode; MyLevel: integer; aPath_No: integer);
  var
    aNode: TTreeNode;
    mode: TSearch_Type; // (SEARCHING, SEARCH_FOUND, NOT_FOUND);

    function BuildPathName(Level: integer): string;
    var
      i: integer;
    begin
      result := '';
      for i := 1 to Level do
        result := result + '\' + PathPieces[i];
      result := ForceBackSlash(result);
    end;

  begin { MyAddNode }
    // See if the "word" at this level exists within the current node
    if MyLevel <= PathPieceCount then
      with CurrentNode do
        begin
          aNode := GetFirstChild{ as TMyTreeNode};
          mode  := SEARCHING;
          repeat
            if aNode = nil then
              mode := NOT_FOUND
            else
              if SameText(aNode.Text, PathPieces[MyLevel]) then
                mode := SEARCH_FOUND
              else
                aNode := GetNextChild(aNode){ as TMyTreeNode};
          until mode <> SEARCHING;

          if mode = SEARCH_FOUND then
            begin
              if MyLevel < PathPieceCount then
                MyAddNode(aNode, MyLevel+1, aPath_No) // try for a match at a lower level
              else
                TMyNodeData(aNode.Data).Path_No := aPath_No;
            end
          else  // mode = NOT_FOUND --> add as a child
            begin
              aNode        := Treeview1.Items.AddChild(CurrentNode, PathPieces[MyLevel]){ as TMyTreeNode};
              aNode.Data   := TMyNodeData.Create;
              with TMyNodeData(aNode.Data) do
                begin
                  if MyLevel = PathPieceCount then
                    Path_NO := aPath_No;
                  PathName := BuildPathName(MyLevel);
                end;
              // Are there any possible lower levels?
              if MyLevel < PathPieceCount then
                MyAddNode(aNode, MyLevel+1, aPath_No);  // add remainder
            end;
        end;
    Application.ProcessMessages;
  end;  { MyAddNode }

begin { TfrmFileMover.RebuildTree }
  Saved_Cursor      := Screen.Cursor;
  StartTime         := GetCurrentTime;
  try
    // cleanup saved node list
    for i := 0 to fMyNodeDataList.Count - 1 do
      begin
        MyNodeData := fMyNodeDataList[i];
        MyNodeData.Free;
        fMyNodeDataList[i] := nil;
      end;

    TreeView1.Items.Clear;
    RootNode := TreeView1.Items.Add(nil, '\');

    with FilePathsTable do
      begin
        Active := true;
        First;
        while not Eof do
          begin
            aPath_No   := fldPATH_NO.AsInteger;
            MyNodeData := TMyNodeData.Create;
            with MyNodeData do
              begin
                PathName      := fldFILE_PATH.AsString;
                Key           := fldSQL_KEY.AsInteger;
                Path_No       := aPath_No;
                Parent_No     := fldPARENT_NO.AsInteger;
                NodeType      := nt_Folder;
              end;
            fMyNodeDataList[aPath_No] := MyNodeData;

            aFilePath      := RemoveLeadingBackSlash(RemoveTrailingBackSlash(fldFILE_PATH.AsString));
            PathPieceCount := WordCountL(aFilePath, BACKSLASH);
            if PathPieceCount > 0 then
              begin
                for i := 1 to PathPieceCount do
                  PathPieces[i] := ExtractWordL(i, aFilePath, BACKSLASH);

                MyAddNode(RootNode, 1, fldPATH_NO.AsInteger);
              end;
            Next;
          end;
        RootNode.AlphaSort(true);
        RootNode.Expanded := true;
        TreeView1Change(nil, RootNode);
      end;
  finally
    Elapsed := GetCurrentTime - StartTime;
    lblStatus.Caption := Format('%0.n seconds elapsed', [Elapsed / 1000.0]);
    Screen.Cursor := Saved_Cursor;
  end;
end;  { TfrmFileMover.RebuildTree }

destructor TfrmFileMover.Destroy;
var
  i: integer;
begin
  FreeAndNil(fFilePathsTable);
  FreeAndNil(fTempPhotoTable);
  for i := 0 to fMyNodeDataList.Count - 1 do
    TMyNodeData(fMyNodeDataList[i]).Free;
  fMyNodeDataList.Clear;
  FreeAndNil(fMyNodeDataList);
  inherited;
end;

function TfrmFileMover.TempPhotoTable: TPhotoTable;
begin
  if not Assigned(fTempPhotoTable) then
    begin
      fTempPhotoTable := TPhotoTable.Create( self,
                                             CommonPhotoSettings.PhotoDBDatabaseFileName,
                                             cFILENAMES,
                                             [optNoSyncFilePathTable, optUseClient,
                                              optNoUpdateDate,
                                              optNoCopyRightsTable]);
      fTempPhotoTable.Active := true;
    end;
  result := fTempPhotoTable;
end;

function TfrmFileMover.FilePathsTable: TFilePathsTable;
begin
  if not Assigned(fFilePathsTable) then
    begin
      fFilePathsTable := TFilePathsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, cFILEPATHS, []);
      fFilePathsTable.Active := true;
    end;
  result := fFilePathsTable;
end;

constructor TfrmFileMover.Create(aOwner: TComponent);
var
  MDBDate, iniDate: TDateTime;
begin
  inherited;
  lblStatus.Caption := '';
  lblFolderName.Caption := '';
  fSavedPause := Application.HintPause;
  Application.HintPause := 0;

  MDBDate := FileDateToDateTime(FileAge(CommonPhotoSettings.PhotoDBDatabaseFileName));
  iniDate := CommonPhotoSettings.PhotoTableLastUsed;
  if (iniDate = 0) or
     (Abs(MDBDate - iniDate) > 0.0001) or
     (CommonPhotoSettings.FolderNo <= 0) {or
     (not fAlreadyScanned)} then
    begin
      lblStatus.Caption   := 'Scanning FilePaths table to find highest folder number';
      Application.ProcessMessages;
      CommonPhotoSettings.FolderNo := PDBTables.ScanForHighestFolderNumber(CommonPhotoSettings.FolderNo, CommonPhotoSettings.PhotoDBDatabaseFileName);
      lblStatus.Caption   := '';
      fAlreadyScanned     := true;
      Application.ProcessMessages;
    end;

  fMyNodeDataList   := TList.Create;
  fMyNodeDataList.Count := CommonPhotoSettings.FolderNo + 1;
  RebuildTree;
end;

{ TMyTreeNode }

(*
destructor TMyTreeNode.Destroy;
begin
  TMyNodeData(Data).Free;
  inherited;
end;
*)

function TfrmFileMover.PhotoTableQuery: TPhotoTableQuery;
begin
  if not Assigned(fPhotoTableQuery) then
    fPhotoTableQuery := TPhotoTableQuery.Create(self);
  result := fPhotoTableQuery;
end;

procedure TfrmFileMover.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  aListItem: TListItem;
  i: integer; ChildNode: TTreeNode;
  mt: TMediaType;
  mc: TMediaClass;
begin
  if Assigned(Node) then
    begin
      fSelectedBranch := TMyNodeData(Node.Data);
      if Assigned(fSelectedBranch) then
        begin
          with PhotoTableQuery do
            begin
              FreeListViewItems;
//            TreeView1.Items.Clear;
              for i := 0 to Node.Count-1 do
                begin
                  aListItem := ListView1.Items.Add;
                  aListItem.Data := TMyNodeData.Create;
                  with TMyNodeData(aListItem.Data) do
                    begin
                      Path_NO   := TMyNodeData(Node[i].Data).Path_NO;
                      Parent_No := TMyNodeData(Node[i].Data).Parent_No;
                      PathName  := TMyNodeData(Node[i].Data).PathName;
                      NodeType  := nt_Folder;
                    end;
                  ChildNode := Node.Item[i];
                  with aListItem do
                    begin
                      Caption    := Format('%s', [ChildNode.Text]);
                      ImageIndex := 0;
                    end;
                end;

              lblFolderName.Caption := Format('%d: %s', [fSelectedBranch.Path_NO, fSelectedBranch.PathName]);
              SQL.Clear;
              SQL.AddObject(Format('SELECT * FROM %s WHERE Path_NO = %d ORDER BY %s',
                                   ['FileNames', fSelectedBranch.Path_NO, FILE_NAME]),
                            Nil);
              ExecSQL;
              Active := true;
              try
                First;
                while not Eof do
                  begin
                    aListItem := ListView1.Items.Add;
                    aListItem.Data := TMyNodeData.Create;
                    with TMyNodeData(aListItem.Data) do
                      begin
                        Path_NO   := fSelectedBranch.Path_NO;
                        Parent_No := fSelectedBranch.Parent_No;
                        Key       := fldKey.AsInteger;
                        NodeType  := nt_File;
                      end;
                    with aListItem do
                      begin
                        Caption := fldFILE_NAME.AsString;
                        mt := MediaTypeFromExtension(MyExtractFileExt(Caption));
                        mc := MediaInfoArray[mt].MediaClass;
                        case mc of
                            mc_Photos:
                              ImageIndex := 1;
                            mc_Video:
                              ImageIndex := 2;
                            mc_Audio:
                              ImageIndex := 3;
                            mc_Document:
                              ImageIndex := 4;
                        end;
                        SubItems.Add(fldPHOTODATE.AsString);
                        SubItems.Add(Format('%0.0n', [fldFILE_SIZE.AsInteger*1.0]));
                      end;
                    Next;
                  end;
              finally
                Active := false;
              end;
            end;
        end;
    end;
end;

procedure TfrmFileMover.TreeView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeView1.GetNodeAt(X, Y);
  Accept   := TreeNode <> nil;
  TreeView1.Refresh;
end;

procedure TfrmFileMover.TreeView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  TreeNode: TTreeNode;
  SrcListItem: TListItem;
  MyDstNodeData: TMyNodeData;
  NewFolderName, OldFolderName,
  OldThumbNailFileName, NewThumbNailFileName: string;
  ListView: TListView;
  i: integer;
  MovedFileCount, MovedFolderCount: integer;
  SrcPathNo, DstPathNo: integer;
  FileCounts, FolderCounts, Temp: string;

  procedure Move1Item(SrcListItem: TListItem);
  var
    NodeData: TMyNodeData;
    temp1, temp2, BaseName: string;

    procedure MoveFile;
    var
      SrcKey: integer;
      BaseName: string;
      OldFileName, NewFileName: string;
    begin { MoveFile }
      SrcKey        := NodeData.Key;
      BaseName      := SrcListItem.Caption;
      OldFileName   := MyUtils.ForceBackSlash(OldFolderName) + BaseName;
      NewFileName   := MyUtils.ForceBackSlash(NewFolderName) + BaseName;
      if FileExists(NewFileName) then
        if YesFmt('A file named "%s" already exists. Overwrite it?',
                       [NewFileName]) then
          DeleteFile(NewFileName)
        else
          Exit;

        // Copy the file

        if RenameFile(OldFileName, NewFileName) then
          begin
            // Update the DB

            with TempPhotoTable do
              begin
                Active := true;
                if Locate('Key', SrcKey, []) then
                  begin
                    Edit;
                    fldPATH_NO.AsInteger := DstPathNo;
                    fldUpdateReason.AsInteger := integer(ur_FileMover);  // tested
                    Post;
                  end
                else
                  raise Exception.CreateFmt('Unable to locate key = %d in FileNames table',
                                            [SrcKey]);
                // Move the ThumbNail also
                OldThumbNailFileName := ThumbNailPathAndName(OldFileName);

                if FileExists(OldThumbNailFileName) then
                  begin
                    NewThumbNailFileName := ThumbNailPathAndName(NewFileName);

                    if not FileExists(NewThumbNailFileName) then // this ThumbNail doesn't already exist
                      begin
                        if not ReNameFile(OldThumbNailFileName, NewThumbNailFileName) then
                          AlertFmt('Unable to rename ThumbNail file from [%s] to [%s]',
                                   [OldThumbNailFileName, NewThumbNailFileName]);
                      end
                    else
                      AlertFmt('The destination ThumbNail [%s] already existed and was not overwritten',
                               [NewThumbNailFileName]);
                  end;
              end;
            Inc(MovedFileCount);
          end
        else
          AlertFmt('Could NOT rename "%s" to "%s"', [OldFileName, NewFileName]);
    end;  { MoveFile }

    procedure MoveFolder(FolderNo, OldParentNo, NewParentNo: integer);
    var
      BaseName: string;
      MyNodeData: TMyNodeData;
      i: integer;
    begin { MoveFolder }
      // Change the parent number of the source folder from the old parent to the new
      MyNodeData := TMyNodeData(fMyNodeDataList[FolderNo]);
      with MyNodeData do
        begin
          Parent_No           := NewParentNo;
          BaseName            := ExtractFileBase(PathName);
          PathName            := TMyNodeData(fMyNodeDataList[NewParentNo]).PathName + '\' + BaseName;
          Changed             := true;
        end;

      // Point all of the child folders in the old parent folder to the new parent folder
      for i := 0 to fMyNodeDataList.Count-1 do
        begin
          MyNodeData := TMyNodeData(fMyNodeDataList[i]);
          if Assigned(MyNodeData) and (MyNodeData.Parent_No = FolderNo) then
            with MyNodeData do
              MoveFolder(Path_NO, Parent_No, Parent_No);
        end;
      inc(MovedFolderCount);
    end;  { MoveFolder }

  begin { Move1Item }
    NodeData := TMyNodeData(SrcListItem.Data);
    case NodeData.NodeType of
      nt_File:
        MoveFile;
      nt_Folder:
        begin
          BaseName := ExtractFileBase(RemoveTrailingBackSlash(NodeData.PathName));
          temp1    := OldFolderName + BaseName;
          temp2    := NewFolderName + BaseName;
          if RenameFile(temp1, temp2) then
            MoveFolder(NodeData.Path_NO, SrcPathNo, DstPathNo)
          else
            raise EFileRenameError.CreateFmt('Unable to rename folder "%s" to "%s"', [temp1, temp2]);
        end;
    end;
  end;  { Move1Item }

  function GetFileCountString(const what: string; HowMany: integer): string;
  begin { GetFileCountString }
    if HowMany > 0 then
      FileCounts := Format('%d %s%s',   [HowMany, what, IIF(HowMany=1, '', 's')])
    else
      FileCounts := '';
  end;  { GetFileCountString }

  procedure PrintReport;
  const
    NWID = 9; PWID = 30;
  var
    i: integer;
    MyNodeData: TMyNodeData;
    Outfile: TextFile;
    Lfn, Temp: string;
  begin { PrintReport }
    Lfn := CalcLogFileName('MovedPhotos.txt');
    AssignFile(OutFile, Lfn);
    ReWrite(OutFile);
    WriteLn(OutFile, 'Path_No':NWID, ' ',
                     'Parent_No':NWID, ' ',
                     'Pathname', '':PWID-Length('Pathname'));
    try
      for i := 0 to fMyNodeDataList.Count-1 do
        begin
          MyNodeData := TMyNodeData(fMyNodeDataList[i]);
          if Assigned(MyNodeData) and (MyNodeData.Changed) then
            with MyNodeData do
              begin
                if FilePathsTable.Locate(SQL_KEY, Key, []) then
                  begin
                    WriteLn(OutFile, FilePathsTable.fldPATH_NO.AsInteger:NWID, ' ',
                                     FilePathsTable.fldPARENT_NO.AsInteger:NWID, ' ',
                                     FilePathsTable.fldFILE_PATH.AsString, '':PWID-Length(FilePathsTable.fldFILE_PATH.AsString), ' ');
                    WriteLn(OutFile, Path_No:NWID, ' ',
                                     Parent_No:NWID, ' ',
                                     PathName, '':PWID-Length(PathName), ' ');
                    WriteLn(OutFile);
                  end;
              end;
        end;
    finally
      CloseFile(OutFile);
      temp := Format('Notepad.exe %s', [Lfn]);
      FileExecute(temp, false);
    end;
  end;  { PrintReport }

  procedure UpdateDataBase;
  var
    i: integer;
    MyNodeData: TMyNodeData;
  begin { UpdateDataBase }
    for i := 0 to fMyNodeDataList.Count-1 do
      begin
        MyNodeData := TMyNodeData(fMyNodeDataList[i]);
        if Assigned(MyNodeData) and (MyNodeData.Changed) then
          with MyNodeData do
            begin
              if FilePathsTable.Locate(SQL_KEY, Key, []) then
                begin
                  FilePathsTable.Edit;
                  FilePathsTable.fldPATH_NO.AsInteger := Path_No;
                  FilePathsTable.fldPARENT_NO.AsInteger := Parent_No;
                  FilePathsTable.fldFILE_PATH.AsString := PathName;
                  FilePathsTable.Post;
                end;
            end;
      end;
  end;  { UpdateDataBase }

begin { TfrmFileMover.TreeView1DragDrop }
  TreeNode      := TTreeNode(TreeView1.GetNodeAt(X, Y));   // get the drop target
  MyDstNodeData := TMyNodeData(TreeNode.Data);
  DstPathNo     := MyDstNodeData.Path_NO;
  SrcPathNo     := fSelectedBranch.Path_NO;
  fFilesWereMoved := true;

  if Source is TListView then
    begin
      ListView      := Source as TListView;
      with FilePathsTable do
        begin
          if Locate('Path_No', SrcPathNo, []) then
            OldFolderName := FixPath(fldFILE_PATH.AsString)
          else
            raise Exception.CreateFmt('Unable to locate Source Path_NO = %s in FilePathsTable',
                                      [SrcPathNo]);

          if Locate('Path_NO', DstPathNo, []) then
            NewFolderName := FixPath(fldFILE_PATH.AsString)
          else
            raise Exception.CreateFmt('Unable to locate Destination Path_NO = %s in FilePathsTable',
                                      [DstPathNo]);
          MovedFileCount := 0; MovedFolderCount := 0;

          if YesFmt('Move files/folders from "%s" to "%s"?', [OldFolderName,
                                                              NewFolderName]) then
            begin
              try
                for i := ListView.Items.Count-1 downto 0 do
                  if ListView.Items[i].Selected then
                    Move1Item(ListView.Items[i]);
                for i := ListView.Items.Count-1 downto 0 do
                  if ListView.Items[i].Selected then
                    begin
                      SrcListItem := ListView.Items[i];
                      SrcListItem.Free;   // remove item from right-side pane
                    end;
              except
                on e:EFileRenameError do
                  Alert(e.Message);
              end;
            end;
        end;

      if MovedFolderCount > 0 then
        begin
          UpdateDatabase;
          PrintReport;
          RebuildTree;
        end;

      FileCounts   := GetFileCountString('file',   MovedFileCount);
      FolderCounts := GetFileCountString('folder', MovedFolderCount);
      if (MovedFileCount + MovedFolderCount) > 0 then
        begin
          Temp         := Format('Moved %s %s from %s to "%s". WEBSITE NEEDS TO BE UPDATED.',
                                      [FileCounts, FolderCounts, OldFolderName, NewFolderName]);
          lblStatus.Caption := Temp;
          Alert(Temp);
        end
      else
        Alert('No files or folder were moved');
    end;
end;  { TfrmFileMover.TreeView1DragDrop }

procedure TfrmFileMover.SetSelectedListItem(Sender: TObject);
var
  Point: TPoint;
begin
  Assert(Sender = ListView1);
  with Sender as TListView do
    begin
      Point    := ScreenToClient(mouse.CursorPos);  // moving the mouse in the debugger can change mouse.CursorPos
      fSelectedListItem := GetItemAt(Point.x, Point.y);
    end;
end;

procedure TfrmFileMover.ListView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  SetSelectedListItem(Sender);
end;

procedure TfrmFileMover.PopupMenu2Popup(Sender: TObject);
begin
  fSelectedListItem     := ListView1.Selected;
  with TMyNodeData(fSelectedListItem.Data) do
    begin
      case NodeType of
        nt_File:
          begin
            RenameFile1.Caption := Format('Rename file "%s"...',   [fSelectedListItem.Caption]);
            RenameFile1.Visible := true;
            RenameFolder1.Visible := false;
          end;
        nt_Folder:
          begin
            RenameFolder1.Caption := Format('Rename folder "%s"...', [fSelectedListItem.Caption]);
            RenameFile1.Visible   := false;
            RenameFolder1.Visible := true;
            RenameFolder1.Enabled := false;  // DEBUG
          end;
      end;
    end;
end;

procedure TfrmFileMover.RenameFile1Click(Sender: TObject);
var
  OldFileName, NewFileName: string;
  SrcPathNo: integer;
  FolderName, Temp: string;
begin
  SrcPathNo := fSelectedBranch.Path_NO;
  with FilePathsTable do
    begin
      if Locate('Path_No', SrcPathNo, []) then
        FolderName := FixPath(fldFILE_PATH.AsString)
      else
        raise Exception.CreateFmt('Unable to locate Source Path_NO = %s in FilePathsTable',
                                  [SrcPathNo]);
    end;
  Temp        := fSelectedListItem.Caption;
  OldFileName := FolderName + temp;
  if GetString('Rename File', 'New FileName', temp) then
    begin
      NewFileName := FolderName + temp;
      if ReNameFile(OldFileName, NewFileName) then
        begin
          with TMyNodeData(fSelectedListItem.Data) do
            begin
              fSelectedPhotoKey := Key;
              with TempPhotoTable do
                begin
                  if Locate('Key', Key, []) then
                    begin
                      Edit;
                      fldFILE_NAME.AsString := temp;
                      fldUpdateReason.AsInteger := integer(ur_FileMoverRenameFile);  // tested
                      Post;
                      fSelectedListItem.Caption := temp;
                      fFilesWereRenamed := true;
                    end;
                end;
            end;
        end;
    end;
end;

procedure TfrmFileMover.SelectThisPhoto1Click(Sender: TObject);
begin
  fSelectedListItem     := ListView1.Selected;
  if Assigned(fSelectedListItem) then
    SelectedPhotoKey := TMyNodeData(fSelectedListItem.Data).Key;
end;

procedure TfrmFileMover.FormShow(Sender: TObject);
begin
  fFilesWereRenamed := false;
  fFilesWereMoved   := false;
end;

procedure TfrmFileMover.ListView1Click(Sender: TObject);
begin
  fSelectedListItem := ListView1.Selected;
  with TMyNodeData(fSelectedListItem.Data) do
    begin
      lblFolderName.Caption := Format('%d: %s', [Path_NO, fSelectedListItem.Caption]);
    end;
end;

function TfrmFileMover.ProcessNode(Node: TTreeNode; aFunction: TNodeFunction): boolean;
var
  ChildNode: TTreeNode;
begin
  result := false;
  if Assigned(Node) then
    begin
      try
        result := aFunction(Node);
        if not result then
          begin
            try
              inc(fIndent, 2);
              ChildNode := Node.getFirstChild{ as TMyTreeNode};
              while (ChildNode <> nil) and (not result) do
                begin
                  result := ProcessNode(ChildNode, aFunction);
                  if not result then
                    ChildNode := Node.getNextChild(ChildNode){ as TMyTreeNode}
                end;
            finally
              Dec(fIndent, 2);
            end;
          end;
      finally
      end;
    end;
end;

function TfrmFileMover.WriteSubTree(Node: TTreeNode): boolean;
begin
  Writeln(fOutputFile, '':fIndent, Node.Text);
  result := false;
end;

procedure TfrmFileMover.miSaveSubTreeToFileClick(Sender: TObject);
var
  FilePath, Temp: string;
  SelectedNode: TTreeNode;
begin
  SelectedNode := TreeView1.Selected{ as TMYTreeNode};
  if Assigned(SelectedNode) then
    begin
      FilePath := CalcLogFileName(Format('Treeview[%s].txt', [SelectedNode.Text]));
      if BrowseForFile('Output file', FilePath, 'txt') then
        begin
          AssignFile(fOutputFile, FilePath);
          ReWrite(fOutputFile);
          WriteLn(fOutputFile, Format('TreeView of "%s" at %s',
                                      [CommonPhotoSettings.PhotoDBFolder, DateTimeToStr(Now)]));
          Writeln(fOutputFile);
          try
            fIndent := 0;
            ProcessNode(SelectedNode, WriteSubTree);
          finally
            CloseFile(fOutputFile);
            temp := Format('Notepad.exe %s', [FilePath]);
            FileExecute(temp, false);
          end;
        end;
    end
  else
    Alert('No node is selected');
end;

procedure TfrmFileMover.ListView1Deletion(Sender: TObject;
  Item: TListItem);
var
  MyNodeData: TMyNodeData;
begin
  MyNodeData := Item.Data;
  FreeAndNil(MyNodeData);
end;

procedure TfrmFileMover.TreeView1Deletion(Sender: TObject;
  Node: TTreeNode);
var
  MyNodeData: TMyNodeData;
begin
   MyNodeData := Node.Data;
   MyNodeData.Free;
end;

procedure TfrmFileMover.FreeListViewItems;
var
  i, j: integer;
  aListItem: TListItem;
  anObject: TObject;
begin
  for i := 0 to ListView1.Items.Count-1 do
    begin
      aListItem := ListView1.Items[i];
      for j := 0 to aListItem.SubItems.Count - 1 do
        begin
          anObject := TObject(aListItem.SubItems.Objects[j]);
          anObject.Free;
        end;
    end;

  ListView1.Items.Clear;
end;

procedure TfrmFileMover.FreeTreeViewItems;
begin
  TreeView1.Items.Clear;
end;


end.
