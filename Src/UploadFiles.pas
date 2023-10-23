unit UploadFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IpUtils, IpSock, IpFtp, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmUploadFiles = class(TForm)
    IpFtpClient1: TIpFtpClient;
    btnUpload: TButton;
    mmoLog: TMemo;
    lblStatus: TLabel;
    ProgressBar1: TProgressBar;
    leWebSiteURL: TLabeledEdit;
    leWebsiteUserID: TLabeledEdit;
    leWebsitePassword: TLabeledEdit;
    leWebSiteFilePath: TLabeledEdit;
    leLogFileName: TLabeledEdit;
    leLocalFilePath: TLabeledEdit;
    btnBrowse: TButton;
    SaveDialog1: TSaveDialog;
    cbUploadPhotos: TCheckBox;
    cbUploadHTML: TCheckBox;
    procedure IpFtpClient1FtpError(Sender: TObject; ErrorCode: Integer;
      const Error: String);
    procedure IpFtpClient1FtpConnected(Sender: TObject);
    procedure IpFtpClient1FtpDisconnected(Sender: TObject);
    procedure IpFtpClient1FtpLoginError(Sender: TObject;
      ErrorCode: Integer; const Error: String);
    procedure btnUploadClick(Sender: TObject);
    procedure IpFtpClient1FtpStatus(Sender: TObject;
      StatusCode: TIpFtpStatusCode; const Info: String);
    procedure IpFtpClient1Error(Sender: TObject; Socket: Cardinal;
      ErrCode: Integer; const ErrStr: String);
    procedure btnBrowseClick(Sender: TObject);
    procedure cbUploadPhotosClick(Sender: TObject);
    procedure cbUploadHTMLClick(Sender: TObject);
  private
    { Private declarations }
    fCancelled            : boolean;
    fLastStatusCode       : TIpFtpStatusCode;
    fLogFile              : TextFile;
    fFileIsOpen           : boolean;
    fFilesUploaded        : integer;
    fLogPathFileName      : string;
    fErrorCode            : integer;
    fErrorMessage         : string;
    fLocalFileName        : string;
    fNrFilesToProcess     : integer;
    fOverallStartTime     : longint;
    fRemoteFileName       : string;
    fStartANewUpload      : boolean;
    fTotalNrFiles         : integer;
    fUploadingFiles       : boolean;
    fWebSitePassWord      : string;
    function GetFtpUrl    : string;
    function GetFtpPassWord: string;
    function GetFtpUserName: string;
    procedure Enable_Buttons;
    procedure DisplayStatus(const Line: string);
  public
    { Public declarations }
    function UploadFile(const Dst, Src: string; var ErrorMessage: string; var UpLoadTime: longint): boolean;
    Constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  MyUtils, PDBTables, PhotoDBCommonSettings, PDB_Decl, MyTables, MyTables_Decl,
  uPhotoDB, PDBUtils, UploadFiles_Decl, uGetString;

const
  BADCHARS = [#1..#31, #127..#255];

function TfrmUploadFiles.GetFtpUrl: string;
begin
  result := leWebSiteURL.Text;
end;

function TfrmUploadFiles.GetFtpUserName: string;
begin
  result := leWebsiteUserID.Text;
end;

function TfrmUploadFiles.GetFtpPassWord: string;
begin
  if Empty(fWebSitePassWord) then
    if Empty(leWebsitePassword.Text) then
      begin
        if not GetString('WebSite Password', 'PassWord', fWebSitePassWord, 10) then
          Exit;
      end
    else
      fWebSitePassWord := leWebsitePassword.Text;
  result := fWebSitePassword;
end;

function TfrmUploadFiles.UploadFile(const Dst, Src: string; var ErrorMessage: string; var UpLoadTime: longint): boolean;
var
  StartTime: longint;
begin
  fErrorCode      := 0;
  fLocalFileName  := Src;
  fRemoteFileName := Dst;
  fStartANewUpload := true;
  StartTime        := GetCurrentTime;

  repeat
    Application.ProcessMessages;
    Sleep(0);
  until (fLastStatusCode in [fscLogin, fscTransferOk, fscClose]);

  if IpFtpClient1.Store (fRemoteFileName, fLocalFileName, smReplace, 0) then
    begin
      // wait until the transfer starts
      repeat
        Application.ProcessMessages;
        Sleep(0);
      until IpFtpClient1.InProgress or fCancelled;

      // wait until the transfer finishes
      if not fCancelled then
        repeat
          Application.ProcessMessages;
          Sleep(0);
        until (not IpFtpClient1.InProgress) and
        ((fLastStatusCode in [fscTransferOK]) or (fErrorCode <> 0));
    end;

  if not fCancelled then
    begin
      result := (fErrorCode = 0) or (fErrorCode = 226) {"transfer complete"};
      if result then
        begin
          UploadTime := (GetCurrentTime - StartTime);
          DisplayStatus(Format('          Uploaded in %s', [HHMMSS(UploadTime)]));
          Inc(fFilesUploaded);
        end
      else
        ErrorMessage := fErrorMessage;
    end
  else
    begin
      result := false;
      ErrorMessage := 'Cancelled by user';
      DisplayStatus(ErrorMessage);
    end;
end;

procedure TfrmUploadFiles.IpFtpClient1FtpError(Sender: TObject;
  ErrorCode: Integer; const Error: String);
begin
  fErrorMessage := Format('Error # %d: %s',
                          [ErrorCode,
                           RemoveBadChars(Error, BADCHARS)]);
  fErrorCode    := ErrorCode;
end;

procedure TfrmUploadFiles.DisplayStatus(const Line: string);
var
  aLine: string;
begin
  if Assigned(mmoLog) and (Line <> '') then
    begin
      aLine := DateTimeToStr(Now) + ': ' + Line;
      mmoLog.Lines.Add(aLine);
      if fFileIsOpen then
        WriteLn(fLogFile, aLine);
    end;
end;

procedure TfrmUploadFiles.IpFtpClient1FtpConnected(Sender: TObject);
begin
  DisplayStatus('Connected')
end;

procedure TfrmUploadFiles.IpFtpClient1FtpDisconnected(Sender: TObject);
begin
  DisplayStatus('DisConnected')
end;

procedure TfrmUploadFiles.IpFtpClient1FtpLoginError(Sender: TObject;
  ErrorCode: Integer; const Error: String);
begin
  DisplayStatus('FTP Login Error')
end;

procedure TfrmUploadFiles.btnUploadClick(Sender: TObject);
var
  Temp, Ext: string;
  mt: TMediaType;
  TempPhotoTable: TPhotoTable;
  frmPhotoDataBase: TfrmPhotoDataBase;

  procedure UploadPhotos;
  var
    Src, Dst, ThumbSrc, ThumbDst, Ext: string;
    mt: TMediaType;
    ErrorMessage: string;
    UploadTime: longint;

    procedure DisplayStatus(const Line: string);
    var
      aLine: string;
      TimePerFile: double;
      TotalElapsedTime: double;
      EstimatedTimeRemaining: double;
    begin
      if Assigned(mmoLog) and (Line <> '') then
        begin
          aLine := DateTimeToStr(Now) + ': ' + Line;
          mmoLog.Lines.Add(aLine);
          ProgressBar1.Position := fFilesUploaded;
          TotalElapsedTime      := GetCurrentTime - fOverAllStartTime;
          if fFilesUploaded > 0 then
            begin
              TimePerFile           := TotalElapsedTime / fFilesUploaded;
              EstimatedTimeRemaining := (fNrFilesToProcess - fFilesUploaded) * TimePerFile;
              lblStatus.Caption := Format('Total elapsed time; %s, %d/%d files, Estimated Time Remaining: %s',
                                          [HHMMSS(TotalElapsedTime), fFilesUploaded, fNrFilesToProcess,
                                           HHMMSS(EstimatedTimeRemaining)]);
            end;

          if fFileIsOpen then
            WriteLn(fLogFile, aLine);
        end;
    end;

  begin { UploadPhotos }
    fNrFilesToProcess := fTotalNrFiles * 2;  // Each file also includes a ThumbNail
    ProgressBar1.Max  := fNrFilesToProcess;
    ProgressBar1.Position := 0;

    fLastStatusCode := fscClose;
    with TempPhotoTable do
      begin
        First;
        while not (eof or fCancelled) do
          begin
            Src := PathAndFileName;

            Dst := leWebSiteFilePath.Text + ForceBackSlash(TempPhotoTable.tblFilePathsFILE_PATH.AsString) + fldFILE_NAME.AsString;

            Dst := ReplaceAll(Dst, '\', '/');

            Ext := ExtractFileExt(Src);
            mt  := MediaTypeFromExtension(Ext);
            if IsPhotoMedia(mt) and (not fldPrivate.AsBoolean) then
              begin
                DisplayStatus(Format('Uploading file #%d/%d: %s', [fFilesUploaded+1,
                                                                   fNrFilesToProcess,
                                                                   Dst]));

                if not UploadFile(Dst, Src, ErrorMessage, UploadTime) then
                  DisplayStatus(Format('Unable to upload to "%s" [%s]', [Dst, ErrorMessage]))
                else
                  begin
                    ThumbSrc := ThumbNailPathAndName(Src);

                    ThumbDst := ForceBackSlash(TempPhotoTable.tblFilePathsFILE_PATH.AsString) + fldFILE_NAME.AsString;
                    ThumbDst := ThumbNailPathAndName(ThumbDst);
                    ThumbDst := leWebSiteFilePath.Text + ReplaceAll(ThumbDst, '\', '/');

                    DisplayStatus(Format('Uploading file #%d/%d: %s', [fFilesUploaded+1,
                                                                       fNrFilesToProcess,
                                                                       Dst]));

                    if not UploadFile(ThumbDst, ThumbSrc, ErrorMessage, UploadTime) then
                      DisplayStatus(Format('Unable to upload to "%s" [%s]', [ThumbDst, ErrorMessage]))
                  end;
              end;

            Next;
          end;
        if fCancelled then
          DisplayStatus('Cancel acknowledged');
      end;
  end;  { UploadPhotos }

  procedure UploadHTML;
  var
    FolderList: TStringList;
    aFullFilePath: string;
    i: integer;
    NrFoldersToProcess: integer;
    NrFoldersProcessed: integer;

    procedure DisplayStatus(const Line: string);
    var
      aLine: string;
      TimePerFolder: double;
      TotalElapsedTime: double;
      EstimatedTimeRemaining: double;
    begin
      if Assigned(mmoLog) and (Line <> '') then
        begin
          aLine := DateTimeToStr(Now) + ': ' + Line;
          mmoLog.Lines.Add(aLine);
          ProgressBar1.Position := NrFoldersProcessed;
          TotalElapsedTime      := GetCurrentTime - fOverAllStartTime;
          if NrFoldersProcessed > 0 then
            begin
              TimePerFolder          := TotalElapsedTime / NrFoldersProcessed;
              EstimatedTimeRemaining := (NrFoldersToProcess - NrFoldersProcessed) * TimePerFolder;
              lblStatus.Caption      := Format('Total elapsed time; %s, %d/%d folders, Estimated Time Remaining: %s',
                                          [HHMMSS(TotalElapsedTime), NrFoldersProcessed, NrFoldersToProcess,
                                           HHMMSS(EstimatedTimeRemaining)]);
            end;

          if fFileIsOpen then
            WriteLn(fLogFile, aLine);
        end;
    end;

    procedure ProcessFolder(FolderPath: string);
    var
      SearchRec: TSearchRec;
      Err: integer;
      Filter: string;
      Src, Dst: string;
      ErrorMessage: string;
      UploadTime: longint;
    begin  { ProcessFolder }
      FolderPath := ForceBackSlash(FolderPath);
      DisplayStatus('Processing Folder: ' + FolderPath);
      Filter     := FolderPath + '*.htm';
      Err        := FindFirst(Filter, faAnyFile - (faHidden + faSysFile + faVolumeID + faDirectory), SearchRec);
      try
        while (Err = 0) and (not fCancelled) do
          begin
            Src := FolderPath + SearchRec.Name;
            Dst := leWebSiteFilePath.Text + RemoveRootString(src);
            Dst := ReplaceAll(Dst, '\', '/');

            DisplayStatus(Format('Uploading file #%d: %s', [fFilesUploaded+1,
                                                            SearchRec.Name]));

            if not UploadFile(Dst, Src, ErrorMessage, UploadTime) then
              DisplayStatus(Format('Unable to upload to "%s" [%s]', [Dst, ErrorMessage]));

            Err := FindNext(SearchRec);
          end;
        inc(NrFoldersProcessed);
      finally
        FindClose(SearchRec);
      end;
    end;  { ProcessFolder }

  begin { UploadHTML }
    fFilesUploaded := 0;
    NrFoldersProcessed := 0;

    fLastStatusCode := fscClose;

    FolderList := TStringList.Create;
    FolderList.Sorted := true;
    // Build a list of the folders currently selected
    try
      with tempPhotoTable do
        begin
          First;
          while not (eof or fCancelled) do
            begin
              aFullFilePath := FullFilePath;
              if FolderList.IndexOf(aFullFilePath) < 0 then
                FolderList.AddObject(aFullFilePath, TObject(nil));
              Next;
            end;
        end;

      NrFoldersToProcess    := FolderList.Count;
      ProgressBar1.Max      := fNrFilesToProcess;
      ProgressBar1.Position := 0;
      fOverAllStartTime     := GetCurrentTime;

      DisplayStatus(Format('List of Selected Folders: %s', [DateTimeToStr(Now)]));
      for i := 0 to FolderList.Count-1 do
        if not fCancelled then
          ProcessFolder(FolderList[i])
        else
          break;

      if fCancelled then
        DisplayStatus('Cancel acknowledged');

    finally
      FreeAndNil(FolderList);
    end;
  end;  { UploadHTML }

begin  { TfrmUploadFiles.btnUploadClick }
  if not fUploadingFiles then
    begin
      fUploadingFiles := true;
      btnUpload.Caption := 'Cancel';
      fWebSitePassWord := GetFtpPassWord;
      if Empty(fWebSitePassWord) then
        exit;
      if IpFtpClient1.Login( GetFtpUrl,
                             GetFtpUserName,
                             fWebSitePassWord,
                             '') then
        begin
          frmPhotoDataBase := Owner as TfrmPhotoDataBase;
          TempPhotoTable := TPhotoTable.Create( self,
                                                CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                cFILENAMES,
                                                [optReadOnly, optNoCopyRightsTable]);
          try
            fLogPathFileName := CalcLogFileName('Uploaded Files.txt');
            AssignFile(fLogFile, fLogPathFileName);
            fFileIsOpen := true; fFilesUploaded := 0; fOverallStartTime := GetCurrentTime;
            leLogFileName.Text := fLogPathFileName;
            ReWrite(fLogFile);

            try
              with TempPhotoTable do
                begin
                  OnFilterRecord := frmPhotoDataBase.PhotoTableFilterRecord;
                  Filtered       := true;
                  Active := true;
                  SetSelectivityParserExpression(frmPhotoDataBase.Expression);

                  // Count the number of photo files to be processed
                  fTotalNrFiles := 0;
                  First;
                  while not Eof do
                    begin
                      Ext := ExtractFileExt(PathAndFileName);
                      mt  := MediaTypeFromExtension(Ext);
                      if IsPhotoMedia(mt) then
                        inc(fTotalNrFiles);
                      Next;
                    end;

                  if cbUploadPhotos.Checked and (fTotalNrFiles > 0) and (not fCancelled) then
                    UpLoadPhotos;

                  if cbUploadHTML.Checked and (fTotalNrFiles > 0) and (not fCancelled) then
                    UploadHTML;
                end;
            finally
              DisplayStatus(Format('%d files uploaded in %s', [fFilesUploaded, HHMMSS(GetCurrentTime - fOverAllStartTime)]));
              if fFileIsOpen then
                CloseFile(fLogFile);
              fFileIsOpen := false;
              temp := Format('Notepad.exe %s', [fLogPathFileName]);
              FileExecute(temp, false);
            end;
          finally
            begin
              if IpFtpClient1.UserLoggedIn then
                begin
                  if IpFtpClient1.Logout then
                    DisplayStatus('Logging out')
                  else
                    DisplayStatus('FTP Logout failed');
                end;
              FreeAndNil(TempPhotoTable);
              fUploadingFiles := false;
              btnUpload.Caption := 'Upload';
            end;
          end
        end
      else
        begin
          AlertFmt('Unable to login to %s', [GetFtpUrl]);
          exit;
        end;
    end
  else
    begin  // It is now a 'Cancel' button rather than an 'UpLoad' button
      fCancelled := true;
      DisplayStatus('Cancel requested');
      if not IpFtpClient1.InProgress then
        begin
          if IpFtpClient1.UserLoggedIn then
            if IpFtpClient1.Logout then
              DisplayStatus('Logged out')
            else
              DisplayStatus('Unable to Log Out');
        end;
      btnUpload.Caption := 'UpLoad';
      fUploadingFiles   := false;
    end;
end;  { TfrmUploadFiles.btnUploadClick }

procedure TfrmUploadFiles.IpFtpClient1FtpStatus(Sender: TObject;
  StatusCode: TIpFtpStatusCode; const Info: String);
begin
  fLastStatusCode := StatusCode;
  case StatusCode of
    fscLogin:
      DisplayStatus(Format('Logged into %s', [GetFtpUrl]));
    fscClose:
      DisplayStatus('Connection Closed');
    fscTimeout:
      DisplayStatus(Format('TIMEOUT: %s', [Info]));
    fscInfo:
      lblStatus.Caption := Info;
    else
      if not Empty(Info) then
        DisplayStatus(Info);
  end;
  Sleep(100); // works better with this?
end;

constructor TfrmUploadFiles.Create(aOwner: TComponent);
begin
  inherited;
  mmoLog.Clear;
  leLocalFilePath.Text      := CommonPhotoSettings.PhotoDBFolder;;
  leWebSiteFilePath.Text    := CommonPhotoSettings.WebSiteFilePath;
  leWebsiteURL.Text         := CommonPhotoSettings.WebsiteURL;
  leWebsiteUserID.Text      := CommonPhotoSettings.WebSiteUserID;
  leWebSitePassWord.Text    := CommonPhotoSettings.WebSitePassword;
  leWebSiteFilePath.Text    := CommonPhotoSettings.WebSiteFilePath;
  leLogFileName.Text        := CalcLogFileName('Uploaded Files.txt');

  lblStatus.Caption         := '';

end;

procedure TfrmUploadFiles.IpFtpClient1Error(Sender: TObject;
  Socket: Cardinal; ErrCode: Integer; const ErrStr: String);
var
  Line: string;
begin
  Line := Format('FTP Client Error %d: %s', [ErrCode, ErrStr]);
  DisplayStatus(Line);
  lblStatus.Caption := Line;
end;

procedure TfrmUploadFiles.btnBrowseClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      FileName   := leLogFileName.Text;
      DefaultExt := ExtractFileExt(FileName);
      InitialDir := ExtractFilePath(FileName);
      if Execute then
        begin
          leLogFileName.Text := FileName;
        end;
    end;
end;

procedure TfrmUploadFiles.Enable_Buttons;
begin
  btnUpload.Enabled := cbUploadPhotos.Checked or cbUploadHTML.Checked;
end;


procedure TfrmUploadFiles.cbUploadPhotosClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmUploadFiles.cbUploadHTMLClick(Sender: TObject);
begin
  Enable_Buttons;
end;

end.
