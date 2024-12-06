{$define NoAudio}
unit ScanForDocuments;

// 02/07/2014 6 Converted ftom MakeHTML program
// 04/17/2011 5 Kludge around problem of reference to file via alternate network path "\\Gamer\My Pictures"
// 12/10/2010 4 Include photo width and height in DB
// 04/20/2010 3 Wasn't generating a page if no media in folder (might exist in lower folder)
// 10/01/2009 2 LogFile getting written to wrong file
// 10/01/2009 1 modified code to generate references to .css file

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OvcSC, OvcBase, OvcEF, OvcPB, OvcNF,
  PDBTables, ExtCtrls, PDBUtils, PDB_Decl, LookupsTableUnit;

const
  FATTR = faAnyFile-faDirectory-faHidden-faSysFile-faVolumeID;
  FATTR2 = faAnyFile-faHidden-faSysFile-faVolumeID;

type
  EAborted = class(Exception);

  TMediaList = class(TStringList)
  public
    Destructor Destroy; override;
    function HtmlMediaCount: integer;
  end;

  EExceptionWhileUpdating = class(Exception);

  TImageInfo = class
  public
    Key: integer;                                                           
    KeyWords: string;
    CopyRightID: string[4];
    PhotoDate: string[8];
    PhotoDateTime: TDateTime;
    CopyRightOwner: string;
    IsPrivate: boolean;
    MediaType: TMediaType;
  end;

  TProcessKind = (pk_Unknown, pk_ScanForMissingPhotos, pk_ImportPhotos);

  TfrmScanForDocuments = class(TForm)
    btnBegin: TButton;
    edtFolder: TEdit;
    btnBrowse: TButton;
    OvcController1: TOvcController;
    lblProcessed: TLabel;
    Label5: TLabel;
    cbIsRootDir: TCheckBox;
    lblThumbCount: TLabel;
    lblRecsUpdated: TLabel;
    btnViewLog: TButton;
    lblElapsedTime: TLabel;
    btnScanningOptions: TButton;
    lblFileOptions: TLabel;
    cbProcessSubFolders: TCheckBox;
    cbIncludeEmpty: TCheckBox;
    pnlDBStuff: TPanel;
    Label6: TLabel;
    rbUpdateRecentOnly: TRadioButton;
    rbAllFiles: TRadioButton;
    ovcDayCount: TOvcNumericField;
    cbUpdateDB: TCheckBox;
    btnCancel: TButton;
    lblError: TLabel;
    Label1: TLabel;
    procedure btnBeginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnViewLogClick(Sender: TObject);
    procedure btnScanningOptionsClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbUpdateDBClick(Sender: TObject);
    procedure edtFolderChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edtFolderExit(Sender: TObject);
  private
    fCancelled: boolean;
//  fFileNameInfo: TFileNameInfo;
    fUpdateInfo: TUpdateInfo;
    fOnOrAfter: TDateTime;
    fDBRecsUpdated: integer;
    fFilesProcessed: integer;
    fFilesMissing: integer;
    fFolderCount: integer;
    fNoiseWordsTable: TLookupsTable;
    fLogFile: TextFile;
    fLogFileName: string;
    fProcessKind: TProcessKind;
    fStartTime: double;
    fThumbCount: integer;
    fTreeList: TStringList;
    fUpdateRecentOnly: boolean;
    tblPhotoTable: TPhotoTable;
    fTempFilePathsTable: TFilePathsTable;
    fLocationInfo: TLocationInfo;
    procedure ProcessFolder(CallerTitle, FolderName, aPath: string;
                                              UsingWildName: boolean);
    procedure Log(const s: string);
    function GetFileNameInfoString(const UpdateInfo: TUpdateInfo; DelimString: string = ''): string;

    procedure ShowFileNameInfo(const InfoString: string);
    procedure UpdateStatusProc(const Msg: string; LineNo: integer = 0);
    procedure LogFmt(const s: string; Args: array of const);
    procedure SetProcessKind(const Value: TProcessKind);
    function NoiseWordsTable: TLookupsTable;
    procedure ValidatePath;
    procedure Enable_Buttons;
    { Private declarations }
  public
    { Public declarations }
    Year: integer;
    constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    property ProcessKind: TProcessKind
             read fProcessKind
             write SetProcessKind;
  end;

var
  frmScanForDocuments: TfrmScanForDocuments;

implementation

{$R *.DFM}

uses
  Math, JPeg, JpegConv, FileCtrl, StStrL,
  MyUtils, ShlObj, ActiveX, ComObj, DateUtils, ScanningOptions,
  PhotoDBCommonSettings, uPhotoDBOptions, GPSTrackingInfoUnit, MyTables_Decl,
  PhotoUtils, GenerateHTML2{, uMakeHTML};

const
  SMALLFONTSTR = '<font face="Verdana, Arial, Helvetica, Sans-Serif" size="-2">';
  GOODCHARS    = ['A'..'Z', 'a'..'z', '0'..'9', '-', ',', ' ', '''', '.', ';'];
  HTML_MEDIA   = [mt_Jpeg, mt_JPG, mt_RM, mt_RV, mt_RAD, mt_WMV, mt_MOV];

function FixSpecialChars(const s: string): string;
  var
    i: integer;
begin
  result := '';
  for i := 1 to Length(s) do
    begin
      case s[i] of
        ' ': result := result + '&nbsp;';
        '-': result := result + '&nbsp;';
      else
        result := result + s[i];
      end;
    end;
end;


procedure TfrmScanForDocuments.Log(const s: string);
begin
  WriteLn(fLogFile, s);
end;

procedure TfrmScanForDocuments.LogFmt(const s: string; Args: array of const);
begin
  Log(Format(s, args));
end;

procedure TfrmScanForDocuments.ProcessFolder( CallerTitle,
                                              FolderName,
                                              aPath: string;
                                              UsingWildName: boolean);
var
  ChildList: TStringList;
  MediaList: TMediaList;
  i: integer;
  FileName: string;
  yPath: string;

  function IsMediaFile(const FileName: string; var MediaType: TMediaType): boolean;
    var
      Ext: string;
  begin { IsMediaFile }
    Ext       := ExtractFileExt(FileName);
    MediaType := MediaTypeFromExtension(Ext);
    result    := MediaType <> mt_Unk;
  end;  { IsMediaFile }

  function IsInDateRange(const FileName: string): boolean;
  var
    FileDateTimeStamp: integer;
    FileDateTime: TDateTime;
  begin { IsInDateRange }
    if fUpdateRecentOnly then
      begin
        FileDateTimeStamp := FileAge(FileName);
        if FileDateTimeStamp >= 0 then
          begin
            FileDateTime      := FileDateToDateTime(FileDateTimeStamp);
            result            := FileDateTime >= fOnOrAfter;
          end
        else
          result := false;
      end
    else
      result := true;
  end;  { IsInDateRange }

  // Count known media types or sub-folders only. Basically determine if the specified folder
  // should be processed or not. Probably should be done recursively to be really correct.
  function FilesInChildFolder(Path, ChildName: string): integer;
    var
    DirInfo: TSearchRec;
    DosError: integer;
    IsADirectory: boolean;
    Temp, PathName: string;
    Lfn: string;
    mt: TMediaType;
  begin { FilesInChildFolder }
    result := 0;
    PathName := path + '\' + ChildName;
    Temp     := PathName + '\*.*';
    DosError := FindFirst(temp, faAnyFile, DirInfo);
    while DosError = 0 do
      begin
        IsADirectory := ((DirInfo.Attr and faDirectory) <> 0) and
                         not ((DirInfo.Name = '.') or (DirInfo.Name = '..'));
        Lfn := UpperCase(DirInfo.Name);

        if (IsMediaFile(Lfn, mt) and IsInDateRange(PathName + '\' + DirInfo.Name)) or IsADirectory then
          inc(result);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { FilesInChildFolder }

  procedure GetListOfChildFolders(ChildNames: TStringList);
  var
    DirInfo: TSearchRec;
    DosError: integer;
  begin { GetListOfChildFolders }
    ChildNames.Clear;
(*
    if UsingWildName then
      DosError := FindFirst(aPath + '\*.*', faAnyFile, DirInfo)
    else
      DosError := FindFirst(ExtractFilePath(aPath) + '\*.*', faAnyFile, DirInfo);
*)
    // 10/2/2018 - working on projects scan -
    // was starting one level too high in folder
    DosError := FindFirst(aPath + '\*.*', faAnyFile, DirInfo);
    while DosError = 0 do
      begin
        if (DirInfo.Name <> '.') and
           (DirInfo.Name <> '..') and
           (CompareText(DirInfo.Name, 'TN') <> 0) and
           (DirInfo.Name[1] <> '_') and
           ((DirInfo.Attr and faDirectory) <> 0) then
          ChildNames.Add(DirInfo.Name);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { GetListOfChildFolders }

  procedure GetListOfHTMLFiles(HTMLList: TStringList);
    var
      DOSError: integer;
      DirInfo: TSearchRec;
  begin
    HTMLList.Clear;
    DosError := FindFirst(yPath + '\*.htm', faAnyFile, DirInfo);
    while DosError = 0 do
      begin
        if (DirInfo.Name <> '.') and
           (DirInfo.Name <> '..') then
          HTMLList.Add(DirInfo.Name);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;

  procedure BuildMediaList(MediaList: TStringList);
  var
    DirInfo: TSearchRec;
    DosError: integer;
    FilePath: string;
    mt: TMediaType;
    ImageInfo: TImageInfo;
  begin { BuildMediaList }
    MediaList.Clear;
    // List the media in this folder
    if UsingWildName then
      FilePath := aPath
    else
      FilePath := Format('%s\*.*', [yPath]);

    DosError := FindFirst(FilePath, FATTR2, DirInfo);
    while DosError = 0 do
      begin
        if IsMediaFile(DirInfo.Name, mt) and IsInDateRange(yPath + '\' + DirInfo.Name) then
          begin
            ImageInfo := TImageInfo.Create;
            ImageInfo.MediaType := mt;
            MediaList.AddObject(DirInfo.Name, ImageInfo);
          end;
        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { BuildMediaList }

  procedure UpdateDataBase( const Path: string;
                            ImageIndex: integer;
                            FileNameInfo: TFileNameInfo);
  var
    OkToCheck: boolean;
    SoundPathName: string;
    KeyWords, Msg, Temp: string;
    UpdateInfo: TUpdateInfo;
    mt: TMediaType;
  begin { UpdateDataBase }
    Finalize(UpdateInfo);
//  FillChar(UpdateInfo, SizeOf(UpdateInfo), 0);  // Why not this?
    UpdateInfo := fUpdateInfo;
    UpdateInfo.ImagePathName := Path + '\' + MediaList[ImageIndex];
    if fUpdateRecentOnly then
      OkToCheck := (FileDateToDateTime(FileAge(UpdateInfo.ImagePathName)) >= fOnOrAfter)
    else
      OkToCheck := true;

    if OkToCheck then
      begin
        UpdateInfo.FileName     := ExtractFileName(UpdateInfo.ImagePathName);
        UpdateInfo.FilePath     := ExtractFilePath(UpdateInfo.ImagePathName);
        UpdateInfo.FileNameInfo := FileNameInfo;

        if not Empty(fLocationInfo.DefaultLocation) then
          fLocationInfo.DefaultLocationKind := dl_SpecifiedValue
        else
          fLocationInfo.DefaultLocationKind := dl_LastWord;

        UpdateInfo.FilePath     := RemoveTrailingBackSlash(UpdateInfo.FilePath);

        UpdateInfo.RecordExists := tblPhotoTable.MyLocateRecord(UpdateInfo.FileName, UpdateInfo.FilePath);

        case ProcessKind of
          pk_ImportPhotos:
            begin
              if UpdateInfo.RecordExists then
                begin
                  KeyWords            := tblPhotoTable.fldKEY_WORDS.AsString;
                  UpdateInfo.PhotoDateYYYYMMDD     := tblPhotoTable.fldPhotoDate.AsString;
                  UpdateInfo.PhotoDateTime := tblPhotoTable.fldPhotoDateTime.AsDateTime;
                end
              else
                if CommonPhotoSettings.UpdateDB then
                  begin
                    mt  := MediaTypeFromExtension(ExtractFileExt(UpdateInfo.ImagePathName));
                    if IsPhotoMedia(mt) then
                      begin
                        SoundPathName           := ForceExtensionL(UpdateInfo.ImagePathName, 'WAV');
                        UpdateInfo.HasSoundFile := FileExists(SoundPathName);
                      end;
                    if not GetPhotoInfo(UpdateInfo) then
                      begin
                        Msg := Format('Unable to GetPhotoInfo for file: %s', [UpdateInfo.ImagePathName]);
                        Alert(Msg);
                        Log(Msg);
                      end;
//                  UpdateInfo.FileNameInfo.MinutesToAdd   := 0; // 7/1/2019: Why was I setting this to zero?
                                                                 // Perhaps user actually wanted a time change?
                    UpdateInfo.UpdateIt       := true;
                    try
                      tblPhotoTable.UpdateProc(UpdateInfo, fLocationInfo, KeyWords, UpdateStatusProc);
                      inc(fDBRecsUpdated);
                      lblRecsUpdated.Caption := Format('%d recs added', [fDBRecsUpdated]);
                      Application.ProcessMessages;
{$IfNDef debugging}
                      Log('Record Added: ' + UpdateInfo.FilePath + '\' + UpdateInfo.FileName);
{$else}
                      Log('SIMULATED Record Add (debugging mode): ' + UpdateInfo.FilePath + '\' + UpdateInfo.FileName);
{$EndIf}
                    except
                      on e:Exception do
                        begin
                          with UpdateInfo do
                            Temp := Format('Error while updating record for %s\%s (%s)',
                                         [FilePath, FileName, e.Message]);
                          Log(Temp);
                          raise EMyUpdateError.Create(Temp);
                        end;
                    end;
                  end;
            end;
        end;
      end;
  end;  { UpdateDataBase }

  procedure UpdateFolderList(const Path: string;
                            FileNameInfo: TFileNameInfo);
  var
    ImagePathName: string;
    KeyWords: string;
  begin { UpdateFolderList }
    ImagePathName := Path;
    with fTempFilePathsTable do
      begin
        FindFolderNo( ImagePathName,
                      KeyWords,
                      true);   // OkToAdd
      end;
  end;  { UpdateFolderList }

  procedure GenerateThumbNail(const FileName, FileExt: string);
  const
    DELIMS = ' \:.?"''';
  var
    ThumbNailPathName,
    ErrorMsg: string;
    tps: TThumbnailProcessingStatus;
    RotateBy: integer;
  begin { GenerateThumbNail }
    ThumbNailPathName := ThumbNailPathAndName(FileName);
    RotateBy := RotationNeeded(FileName);
    tps := MyCreateThumbNail(FileName, ThumbNailPathName, ErrorMsg, THUMBNAILWIDTH, THUMBNAILHEIGHT, false, RotateBy);
    case tps of
      tps_CreatedUpdated:
        begin
          Inc(fThumbCount);
          lblThumbCount.Caption := Format('Thumbnails Created/Updated: %d', [fThumbCount]);
          Log('Created/Updated thumbnail: ' + ThumbnailPathName);
        end;
      tps_Error:
        LogFmt('Unable to create/update thumbnail for file "%s" [%s]: ', [FileName, ErrorMsg]);
    end;
  end;  { GenerateThumbNail }

begin { TfrmImportPhotos.ProcessFolder }
  ChildList        := TStringList.Create;
  ChildList.Sorted := true;
  MediaList        := TMediaList.Create;
  fTreeList.Add(FolderName);
  
  if UsingWildName then
    yPath := RemoveTrailingBackslash(ExtractFilePath(aPath))
  else
    yPath := aPath;

  try
    GetListOfChildFolders(ChildList);
    BuildMediaList(MediaList);
    for i := 0 to MediaList.Count-1 do
      try
        Inc(fFilesProcessed);
        lblProcessed.Caption := Format('Now processing: %d: %s', [fFilesProcessed, MediaList[i]]);
        Application.ProcessMessages;
        case ProcessKind of
          pk_ImportPhotos:
            begin
              UpdateDataBase(yPath, i, fUpdateInfo.FileNameInfo);
              GenerateThumbNail(ForceBackSlash(yPath) + MediaList[i], GENERAL_MEDIA_FILENAME_EXT);
            end;
          pk_ScanForMissingPhotos:
            begin
              FileName := yPath + '\' + MediaList[i];

              if not tblPhotoTable.MyLocateRecord(MediaList[i], yPath) then
                begin
                  Log(FileName);
                  Inc(fFilesMissing);
                  lblThumbCount.Caption  := Format('%d missing files: %s', [fFilesMissing, FileName]);
                end;
              lblElapsedTime.Caption := HHMMSS(DWORD(GetCurrentTime) - fStartTime);
              Application.ProcessMessages;
            end;
        end;
        if fCancelled then
          break;
      except
        on e:Exception do
          ErrorFmt('Exception while updating DB: %s', [e.Message]);
      end;
    if cbIncludeEmpty.Checked and (ProcessKind = pk_ImportPhotos) then
      UpdateFolderList(yPath, fUpdateInfo.FileNameInfo);
    if cbProcessSubFolders.Checked then
      for i := 0 to ChildList.Count - 1 do
        begin
          if fCancelled then
            break;
          if FilesInChildFolder(yPath, ChildList[i]) > 0 then
            ProcessFolder(FolderName, ChildList[i], aPath + '\' + ChildList[i], UsingWildName) else
          if cbIncludeEmpty.Checked then
            ProcessFolder(FolderName, ChildList[i], aPath + '\' + ChildList[i], UsingWildName);
        end;
    inc(fFolderCount);
  finally
    ChildList.Free;
    MediaList.Free;
    fTreeList.Delete(fTreeList.Count-1);
  end;
end;  { TfrmImportPhotos.ProcessFolder }

procedure TfrmScanForDocuments.UpdateStatusProc(const Msg: string; LineNo: integer);
var
  aColor: TColor;
begin
  if LineNo < 0 then
    begin
      aColor := clYellow;
      LineNo := - LineNo;
    end
  else
    aColor := clBtnFace;

  case LineNo of
    0, 1:
      begin
        lblProcessed.Caption := Msg;
        lblProcessed.Color   := aColor;
      end;
    2: begin
         lblThumbCount.Caption   := Msg;
         lblThumbCount.Color := aColor;
       end;
    3: begin
         lblRecsUpdated.Caption  := Msg;
         lblRecsUpdated.Color := aColor;
       end;
    4: begin
         lblElapsedTime.Caption  := Msg;
         lblElapsedTime.Color := aColor;
       end;
  end;
  Application.ProcessMessages;
end;

function TfrmScanForDocuments.NoiseWordsTable: TLookupsTable;
begin
  if not Assigned(fNoiseWordsTable) then
    begin
      fNoiseWordsTable := TLookupsTable.Create( nil,
                                                CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                cLOOKUPS,
                                                [],
                                                lc_NoiseWords);
      fNoiseWordsTable.Active := true;
    end;
  result := fNoiseWordsTable;
end;


procedure TfrmScanForDocuments.btnBeginClick(Sender: TObject);
const
  CRLF = #13#10;
var
  FolderName, InfoString: string;
  PathName: string;
  UsingWildName: boolean;
  Saved_Cursor: TCursor;
  Ok: boolean;

  function GetParentFolderName(FolderName: string): string;
    var
      OldDir: string;
  begin { GetParentFolderName }
    GetDir(0, OldDir);
    ChDir(RemoveTrailingBackSlash(FolderName)+'\..');
    GetDir(0, result);
    ChDir(OldDir);
  end;  { GetParentFolderName }

  function RemoveSlash(const s: string): string;
  begin
    if s[Length(s)] = '\' then
      result := copy(s, 1, Length(s)-1)
    else
      result := s;
  end;

begin { TfrmImportPhotos.btnImportClick }
  btnCancel.Enabled := true;
  fCancelled        := false;
  InfoString        := '';
  case ProcessKind of
    pk_ScanForMissingPhotos:
      Ok := true;
    pk_ImportPhotos:
      begin
        Ok := GetScanningOptions(fUpdateInfo, fLocationInfo, []);
        if Ok then
          begin
            InfoString := GetFileNameInfoString(fUpdateInfo);
            ShowFileNameInfo(InfoString);
          end;
      end;
    else
      begin
        AlertFmt('Unexpected ProcessKind = %d', [ord(ProcessKind)]);
        Ok := false;
      end;
  end;

  if Ok then
    begin
      Application.ProcessMessages;
      btnViewLog.Enabled := false;
      lblProcessed.Caption := ''; lblThumbCount.Caption := ''; lblRecsUpdated.Caption := ''; lblElapsedTime.Caption := '';
      case ProcessKind of
        pk_ScanForMissingPhotos:
          fLogFileName := CalcLogFileName('MissingFiles.txt');
        pk_ImportPhotos:
          fLogFileName := CalcLogFileName('ImportedFiles.txt');
      end;
      AssignFile(fLogFile, fLogFileName);
      Rewrite(fLogFile);
      Log(Format('PhotoDB Log File: %s', [DateToStr(Now)]));
      Log(Format('Folder:           %s', [edtFolder.Text]));
      Log(Format('Database Name:    %s', [CommonPhotoSettings.PhotoDBDatabaseFileName]));
      Log(Format('File Options:     %s', [InfoString]));
      if ProcessKind = pk_ScanForMissingPhotos then
        Log('The following files do not have records in the database:');
      InfoString := GetFileNameInfoString(fUpdateInfo, CRLF);
      Log(InfoString + CRLF);
      try
        fFolderCount   := 0;
        fThumbCount    := 0;
        fDBRecsUpdated := 0;
        PathName       := RemoveSlash(edtFolder.Text);
        UsingWildName  := IsWildName(PathName);

        CommonPhotoSettings.RecentOnly    := rbUpdateRecentOnly.Checked;
        CommonPhotoSettings.DayCount      := ovcDayCount.AsInteger;
        btnBegin.Enabled := false;

        if cbIsRootDir.Checked then
          begin
            FolderName := '';
            GenerateIndexPage( ExtractFileName(PathName), PathName);
          end
        else
          begin
            FolderName := ExtractFileName(GetParentFolderName(edtFolder.Text));
            fTreeList.Add(FolderName);
          end;

        tblPhotoTable := TPhotoTable.Create( self,
                                             CommonPhotoSettings.PhotoDBDatabaseFileName,
                                             cFILENAMES, []);
        with tblPhotoTable do
          begin
            OnUpdateStatus := UpdateStatusProc;
            Active        := true;
          end;

        Saved_Cursor   := Screen.Cursor;
        try
          fStartTime := DWORD(GetCurrentTime);

          if cbIncludeEmpty.Checked then
            begin
              fTempFilePathsTable := TFilePathsTable.Create( self,
                                                             CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                             cFILEPATHS,
                                                             []);
              fTempFilePathsTable.Active := true;
            end;

          fFilesProcessed := 0; fFilesMissing := 0;

          with fLocationInfo do
            begin
              OkToAdd          := true;
              LowestDateToScan := 0;    // no telling wat the lowest photo date might be
              if (UseGPXLogsForLocation or UseSavedTracksLogFile) then
                begin
                  CreateGPSTrackingInfo( CommonPhotoSettings.DefaultGPXFilter,
                                       CommonPhotoSettings.SavedTrackDataLfn,
                                       UpdateStatusProc);
                  if Assigned(gGPSTrackingInfo) then
                    gGPSTrackingInfo.OnUpdateStatus := UpdateStatusProc;
                end;
            end;

          ProcessFolder( FolderName,
                         ExtractFileName(PathName),
                         PathName,
                         UsingWildName);
          if (not fCancelled)
              and fLocationInfo.SaveUpdatedTrackLog
              and Assigned(gGPSTrackingInfo) then
            begin
              with gGPSTrackingInfo do
                begin
                  SaveTrackData(SavedTrackDataLfn);
                  AlertFmt('%d records of Track Data saved to file: %s',
                           [TotalWayPoints, SavedTrackDataLfn]);
                end;
            end;

        finally
          tblPhotoTable.Active   := false;
          tblPhotoTable.Filtered := false;
          FreeandNil(tblPhotoTable);
          FreeAndNil(fTempFilePathsTable);

          if fTreeList.Count > 0 then
            fTreeList.Delete(fTreeList.Count-1);

          lblProcessed.Caption := Format('Complete. %d processed, %d folders processed', [fFilesProcessed, fFolderCount]);
          lblElapsedTime.Caption := HHMMSS(DWORD(GetCurrentTime) - fStartTime);
          Log(lblProcessed.Caption);
          CloseFile(fLogFile);
          Screen.Cursor       := Saved_Cursor;
          btnViewLog.Enabled  := true;
          btnBegin.Enabled    := true;
          btnCancel.Enabled   := false;
          Beep;
          Beep;
          FileExecute(Format('NotePad.exe %s', [fLogFileName]), false)
        end;

      except
        on e:EAborted do
          ;
        on e:Exception do
          Application.HandleException(e);
      end;
    end;
end;  { TfrmImportPhotos.btnImportClick }

(*
function TfrmScanForDocuments.ValidPath(const FolderName: string; var ErrorMsg: string): boolean;
var
  Temp: string;
begin
  Temp       := Copy(FolderName, 1, Length(gRootPath));
  result     := Sametext(gRootPath, Temp);
  if not result then
    ErrorMsg := Format('The path "%s"is not a sub-folder of "%s"', [FolderName, gRootPath])
end;
*)

procedure TfrmScanForDocuments.ValidatePath;
var
  ErrorMsg: string;
begin
  if not ValidPath(edtFolder.Text, ErrorMsg) then
    Error(ErrorMsg);
end;


procedure TfrmScanForDocuments.btnBrowseClick(Sender: TObject);
var
  FolderName: string;
begin
  FolderName := edtFolder.Text;
  if BrowseForFolder('Browse for picture folder', FolderName) then
    edtFolder.Text := FolderName;
  cbIsRootDir.Checked := CompareText(RemoveTrailingBackSlash(FolderName), gRootPath) = 0;
  ValidatePath;
end;

constructor TfrmScanForDocuments.Create(aOwner: TComponent);
var
  MDBDate, iniDate: TDateTime;
begin
  inherited Create(aOwner);
  CommonPhotoSettings.LoadSettings;  // 10/13/2017- Is this going to overwrite currently unsaved settings?
  Init_NoiseWords(NoiseWordsTable);

  MDBDate := FileDateToDateTime(FileAge(CommonPhotoSettings.PhotoDBDatabaseFileName));
  iniDate := CommonPhotoSettings.PhotoTableLastUsed;
  if (iniDate = 0) or (Abs(MDBDate - iniDate) > 0.0001) then
    begin
      lblProcessed.Caption   := 'Scanning FilePaths table to find highest folder number';
      Application.ProcessMessages;
      CommonPhotoSettings.FolderNo := PDBTables.ScanForHighestFolderNumber(CommonPhotoSettings.FolderNo, CommonPhotoSettings.PhotoDBDatabaseFileName);
      lblProcessed.Caption   := '';
      Application.ProcessMessages;
    end;

  fTreeList         := TStringList.Create;
  edtFolder.Text    := CommonPhotoSettings.PhotoDBFolder;
  Enable_Buttons;
  Year              := YearOf(Now);
end;

destructor TfrmScanForDocuments.Destroy;
begin
  FreeAndNil(fTreeList);
  FreeAndNil(gGPSTrackingInfo);  // This should ask to save the data
  FreeAndNil(fNoiseWordsTable);
  inherited;
end;

procedure TfrmScanForDocuments.btnViewLogClick(Sender: TObject);
begin
  FileExecute(Format('NotePad.exe %s', [fLogFileName]), false)
end;

procedure TfrmScanForDocuments.FormCreate(Sender: TObject);
var
  InfoString: string;
begin
  lblProcessed.Caption     := '';
  lblThumbCount.Caption    := '';
  lblRecsUpdated.Caption   := '';
  lblElapsedTime.Caption   := '';
  fUpdateInfo.PhotoOwner   := CommonPhotoSettings.CopyRightID;
  fUpdateInfo.isPrivate    := false;
  InfoString               := GetFileNameInfoString(fUpdateInfo);
  ShowFileNameInfo(InfoString);

  // set default thumbnail size
  if FileExists(CommonPhotoSettings.PhotoDBDatabaseFileName) then
    begin
      pnlDBStuff.Visible    := true;
      ovcDayCount.AsInteger := 10;
    end
  else
    pnlDBStuff.Visible := false;

  rbUpdateRecentOnly.Checked := CommonPhotoSettings.RecentOnly;
  rbAllFiles.Checked         := not CommonPhotoSettings.RecentOnly;
  ovcDayCount.AsInteger      := CommonPhotoSettings.DayCount;
end;


{ TMediaList }

destructor TMediaList.Destroy;
  var                                             
    i: integer;
begin
  for i := 0 to Count-1 do
    Objects[i].Free;

  inherited;
end;                 

//function TfrmScanForDocuments.GetFileNameInfoString(FileNameInfo: TFileNameInfo; DelimString: string = ''): string;
function TfrmScanForDocuments.GetFileNameInfoString(const UpdateInfo: TUpdateInfo; DelimString: string = ''): string;
begin
//with FileNameInfo do
  with UpdateInfo.FileNameInfo do
    begin
      result := '';
      if IncludeFilePath then
        result := Result + DelimString + ' (Include File Path)';
      if IncludeFileName then
        result := Result + DelimString + ' (Include File Name)';
      if dk_From_FileName in DateKinds then
        result := Result + DelimString + ' (Scan for Date)';
      if dk_From_EXIF in DateKinds {UseExifForPhotoDateAndLocation} then
        result := Result + DelimString + ' (Use EXIF for PhotoDate and Location)';
      if dk_Date_Modified in DateKinds {UseFileDateForPhotoDate} then
        result := Result + DelimString + ' (Use FileDate for PhotoDate)';
      if fLocationInfo.UseGPXlogsForLocation then
        result := result + DelimString + ' (Use GPX logs for location)';
      if fLocationInfo.UseSavedTracksLogFile then
        result := result + DelimString + ' (Use saved log file for location)';
      if ExtractComments then
        result := result + DelimString + ' (Extract [comments] to field)';
      if fLocationInfo.ExtractLatLonFromFileName then
        result := result + DelimString + ' (Extract {Lat, Lon} to fields)';
      if fLocationInfo.ScanSelectedSubFolders and (not Empty(fLocationInfo.SelectedSubFolders)) then
        result := result + DelimString + Format(' (Scan selected sub-folders: %s)', [fLocationInfo.SelectedSubFolders]);
      if not Empty(fLocationInfo.DefaultLocation) then
        result := result + DelimString + Format(' (Default location: %s)', [fLocationInfo.DefaultLocation]);
      if not Empty(fLocationInfo.DefaultKeyWords) then
        result := result + DelimString + Format(' (Default Key Words: %s)', [fLocationInfo.DefaultKeyWords]);
      if fLocationInfo.SaveUpdatedTrackLog then
        result := result + DelimString + ' (Save Updated Track Log)';
      if UpdateInfo.PhotoOwner <> CommonPhotoSettings.CopyRight then
        result := Result + DelimString + Format(' (Owner: %s)', [UpdateInfo.PhotoOwner]);
      if UpdateInfo.isPrivate then
        result := result + ' PRIVATE';
    end;
end;

procedure TfrmScanForDocuments.btnScanningOptionsClick(Sender: TObject);
var
  InfoString: string;
begin
  GetScanningOptions(fUpdateInfo, fLocationInfo, []);
  InfoString := GetFileNameInfoString(fUpdateInfo);
  ShowFileNameInfo(InfoString);
end;

procedure TfrmScanForDocuments.btnOptionsClick(Sender: TObject);
begin
  with TfrmPhotoDBOptions.Create(self) do
    begin
      PageControl1.ActivePage := tsMakeHTML;
      if ShowModal = mrOk then
        CommonPhotoSettings.SaveSettings(CommonSettingsFileName(true));
      Free;
    end;
end;

procedure TfrmScanForDocuments.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  with CommonPhotoSettings do
    begin
      PhotoTableLastUsed := FileDateToDateTime(FileAge(PhotoDBDatabaseFileName));
      CommonPhotoSettings.UpdateDB := cbUpdateDB.Checked;  // this is obsolete
      SaveSettings(CommonSettingsFileName(true));
    end;
end;

procedure KillIt;
begin
  TerminateProcess(GetCurrentProcess, 0);
end;


// count the number of items that can be rendered
function TMediaList.HtmlMediaCount: integer;
var
  i: integer;
  ImageInfo: TImageInfo;
begin
  result := 0;
  for i := 0 to Pred(Count) do
    begin
      ImageInfo := TImageInfo(Objects[i]);
      if ImageInfo.MediaType in HTML_MEDIA then
        Inc(result);
    end;
end;

procedure TfrmScanForDocuments.cbUpdateDBClick(Sender: TObject);
begin
//CommonPhotoSettings.UpdateDB := cbUpdateDB.Checked;   // commented out to defer until form gets closed
end;

procedure TfrmScanForDocuments.SetProcessKind(const Value: TProcessKind);
begin
  fProcessKind := Value;
  case fProcessKind of
    pk_ImportPhotos:
      begin
        Caption            := 'Import Documents/Photos';
        btnBegin.Caption   := 'Import';
        pnlDBStuff.Visible := true;
        cbIncludeEmpty.Visible := true;
        btnScanningOptions.Visible := true;
        cbUpdateDB.Checked := true; // always default to true
        cbUpdateDB.Visible := true;
      end;
    pk_ScanForMissingPhotos:
      begin
        Caption            := 'Scan for Documents/Photos not in DB';
        btnBegin.Caption   := 'Scan';
        pnlDBStuff.Visible := false;
        cbIncludeEmpty.Visible := false;
        btnScanningOptions.Visible := false;
        cbUpdateDB.Checked := false; // always default to false
        cbUpdateDB.Visible := false;
      end;
  end;
end;

procedure TfrmScanForDocuments.Enable_Buttons;
var
  b: boolean;
  ErrorMsg: string;
begin
  b := ValidPath(edtFolder.Text, ErrorMsg);
  btnBegin.Enabled := b;
  if not b then
    begin
      lblError.Caption := ErrorMsg;
      lblError.Visible := true;
    end
  else
    lblError.Visible := false;
end;

procedure TfrmScanForDocuments.edtFolderChange(Sender: TObject);
begin
  cbIsRootDir.Checked := false;
  Enable_Buttons;
end;

procedure TfrmScanForDocuments.btnCancelClick(Sender: TObject);
begin
  fCancelled := true;
end;

procedure TfrmScanForDocuments.ShowFileNameInfo(const InfoString: string);
begin
  lblFileOptions.Caption      := InfoString;
  btnScanningOptions.Hint     := InfoString;
  btnScanningOptions.ShowHint := not Empty(InfoString);
end;

procedure TfrmScanForDocuments.edtFolderExit(Sender: TObject);
begin
  ValidatePath;
end;

initialization
  ExitProcessProc := Killit;
finalization
end.
