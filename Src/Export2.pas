unit Export2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PDBTables, MyTables, PDB_Decl;

type
  TImportExportMode = (iem_Import, iem_Export);

  TfrmExport2 = class(TForm)
    btnBrowse: TButton;
    leFileName: TLabeledEdit;
    btnBegin: TButton;
    btnCancel: TButton;
    lblStatus01: TLabel;
    lblStatus02: TLabel;
    cbOverwriteExisting: TCheckBox;
    cbUpdateRecords: TCheckBox;
    cbCopyDocuments: TCheckBox;
    procedure btnBeginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure cbCopyDocumentsClick(Sender: TObject);
  private
    { Private declarations }
    fInProgress, fDoAbort: boolean;
    fImportExportMode: TImportExportMode;
    fLocationInfo: TLocationInfo;
    fLogFile: TextFile;
    fLogPathFileName: string;
    fMainForm: TForm;
//  fLocationsTable : TLocationsTable;
    procedure ExportSelectedRecords;
    procedure ImportSelectedRecords;
    function CopyPhotoTable(SrcTable, DstTable: TPhotoTable): integer;
    procedure CopyRecord(SrcTable, DstTable: TMyTable; FieldsToIgnore: string = '');
    function UpdatePhotoTable(SrcTable, DstTable: TPhotoTable; Var Updated, Added, Skipped: integer): integer;
    procedure Update_Status(const Msg: string; LineNo: integer = 1);
    procedure Update_StatusFmt(const Msg: string; Args: array of const; LineNo: integer = 1);
    procedure CopyAFile(cnt: integer; const SrcFileName, DstFileName: string);
    procedure CopyErrorLogs(var cnt: integer; SrcPath, DstPath: string);
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; ImportExportMode: TImportExportMode = iem_Export); reintroduce;
  end;

implementation

uses TableCreation, MyUtils, uPhotoDB, DB,
     MyTables_Decl, PhotoDBCommonSettings, PDBUtils, ConfirmEachPhoto;

{$R *.dfm}

(*
procedure TfrmExport2.CopyLocationsInfo(SrcLocatonsTable, DstLocationsTable: TLocationsTable);
var
  i: integer;
  SrcFld, DstFld: TField;
begin
  for i := 0 to SrcLocatonsTable.Fields.Count-1 do
    begin
      SrcFld := SrcLocatonsTable.Fields[i];
      DstFld := DstLocationsTable.FieldByName(SrcFld.FieldName);
      if Assigned(DstFld) then
        begin
          if (not ((DstFld.DataType in [ftAutoInc]) or
                  (SameText(FldName, 'ID')))) then
            DstFld.AsVariant := SrcFld.AsVariant
        end
      else
        raise Exception.CreateFmt('Field [%s] could not be found in destination table [%s]',
                                  [SrcFld.FieldName, DstTable.TableName]);
    end;
end;
*)

procedure TfrmExport2.CopyRecord(SrcTable, DstTable: TMyTable; FieldsToIgnore: string = '');
var
  i: integer;
  SrcFld, DstFld: TField;
  OK: boolean;
begin { CopyRecord }
  for i := 0 to SrcTable.Fields.Count-1 do
    begin
      SrcFld := SrcTable.Fields[i];
      if FieldsToIgnore <> '' then
        OK := not ContainsWords( FieldsToIgnore,
                                 SrcFld.FieldName,
                                 true {MatchWholeWordsOnly},
                                 false {MustContainAll})
      else
        OK := true;

      if OK then
        begin
          DstFld := DstTable.FieldByName(SrcFld.FieldName);
          if Assigned(DstFld) then
            begin
              if (not (DstFld.DataType in [ftAutoInc])) then
                DstFld.AsVariant := SrcFld.AsVariant
            end
          else
            raise Exception.CreateFmt('Field [%s] could not be found in destination table [%s]',
                                      [SrcFld.FieldName, DstTable.TableName]);
        end;
    end;
  if DstTable is TPhotoTable then
    (DstTable as TPhotoTable).fldUpdateReason.AsInteger := ord(ur_Imported);
end;  { CopyRecord }

function TfrmExport2.CopyPhotoTable(SrcTable, DstTable: TPhotoTable): integer;
var
  KeyFields: string;
begin { TfrmExport2.CopyPhotoTable }
  result := 0;
  lblStatus01.Caption := Format('Copying table "%s"', [SrcTable.TableName]);
  SrcTable.First;

  while not SrcTable.Eof do
    begin
      if fDoAbort then
        break;
      KeyFields := 'FILE_NAME;PHOTODATE';
      if not DstTable.Locate( FILE_NAME,
                          VarArrayOf([SrcTable.fldFILE_NAME.AsString, SrcTable.fldPhotoDate.AsString]),
                          []) then // a record with this filename does not already exists
        begin
          DstTable.Append;
          CopyRecord(SrcTable, DstTable);
          DstTable.Post;

          // Add the corresponding FilePath record, if it dosen't already exist
          if not DstTable.FilePathsTable.Locate(PATH_NO, SrcTable.fldPATH_NO.AsInteger, []) then
            begin
              DstTable.FilePathsTable.Append;
              CopyRecord(SrcTable.FilePathsTable, DstTable.FilePathsTable); // This sometimes gets an MSAccess error
              DstTable.FilePathsTable.Post;
            end;

          // If the CopyRight record exists for the source record then
          //   add the corresponding CopyRight record, if it dosen't already exist
          if SrcTable.CopyRightsTable.Eof then
            WriteLn(fLogFile, 'No CopyRight record exists for COPYR_ID = ', SrcTable.fldCOPYR_ID.AsString)
          else
            if not DstTable.CopyRightsTable.Locate(cCOPYR_ID, SrcTable.fldCOPYR_ID.AsString, [loCaseInsensitive]) then
              begin
                DstTable.CopyRightsTable.Append;
                CopyRecord(SrcTable.CopyRightsTable, DstTable.CopyRightsTable);
                DstTable.CopyRightsTable.Post;
              end;

          // Add the locations record
          if SrcTable.fldLocationID.AsInteger > 0 then
            if SrcTable.LocationsTable.Locate(cID, SrcTable.fldLocationID.AsInteger, []) then
              if not DstTable.LocationsTable.Locate(cID, DstTable.fldLocationID.AsInteger, []) then
                begin
                  DstTable.LocationsTable.Append;
                  CopyRecord(SrcTable.LocationsTable, DstTable.LocationsTable);
                  DstTable.LocationsTable.Post;
                end;

          SrcTable.Next;
          Inc(result);
          lblStatus02.Caption := Format('Copying record # %d', [result]);
          Application.ProcessMessages;
        end;
      lblStatus01.Caption := 'Export Database Created';
      lblStatus02.Caption := Format('%d records added', [result]);
      Application.ProcessMessages;
    end;
end;  { TfrmExport2.CopyPhotoTable }

function TfrmExport2.UpdatePhotoTable(SrcTable, DstTable: TPhotoTable; Var Updated, Added, Skipped: integer): integer;
var
  OK: boolean;
  frmConfirmEachPhoto: TfrmConfirmEachPhoto;
  Msg: string;
  mt: TMediaType;
  DocumentFile, Ext, PathNoStr, CopyRightStr, DocFileKeyMsg, Desc: string;
  KeyFields: string;
  mr: integer;
  ChangedField: string;

  procedure UpdateLogFile(TableName, FileName: string; Status: string);
  var
    Msg: string;
  begin { UpdateLogFile }
    Msg := Format('%-10s %-25s %s', [TableName, Status, FileName]);
    WriteLn(fLogFile, Msg);
    Update_Status(Msg);
  end;  { UpdateLogFile }

  function SameFields(SrcTable, DstTable: TMyTable; var ChangedField: string; FieldsToIgnore: string = ''): boolean;
  var
    fn: integer;
    SrcFld, DstFld: TField;
    OK: boolean;
  begin { SameFields }
    result := true; // Assume true until it is not
    for fn := 0 to SrcTable.FieldCount-1 do
      begin
        SrcFld  := SrcTable.Fields[fn];
        if FieldsToIgnore <> '' then
          OK := not ContainsWords( FieldsToIgnore,
                                   SrcFld.FieldName,
                                   true {MatchWholeWordsOnly},
                                   false {MustContainAll})
        else
          OK := true;

        if Ok then
          begin
            with DstTable do
              DstFld  := FieldByName(SrcFld.FieldName);
            if Assigned(DstFld) then
              if (not (DstFld.DataType in [ftAutoInc])) then
                if SrcFld.AsString <> DstFld.AsString then
                  begin
                    result := false;
                    ChangedField := SrcFld.FieldName;
                    break;
                  end;
          end;
      end;
  end;  { SameFields }

  procedure CopyLocationInfo;
  var
    ChangedField: string;
  begin
    if SrcTable.LocationsTable.Locate(cID, SrcTable.fldLocationID.AsInteger, []) then
            // The location record is available for import
      begin // either find or add the corresponding record in the destination locations record and link it
        DstTable.Edit;
        DstTable.fldLocationID.AsInteger := DstTable.LocationsTable.FindLocationID(
                                               SrcTable.fldLatitude.AsFloat,
                                               SrcTable.fldLongitude.AsFloat,
                                               fLocationInfo, Added, Desc);
        if DstTable.LocationsTable.Locate(cID, DstTable.fldLocationID.AsInteger, []) then // should always be true
          if not SameFields(SrcTable.LocationsTable, DstTable.LocationsTable, ChangedField, 'ID,DateAdded,DateUpdated') then
            begin  // because we found it or added it
              DstTable.LocationsTable.Edit;
              CopyRecord(SrcTable.LocationsTable, DstTable.LocationsTable);
              DstTable.LocationsTable.Post;
            end;
        DstTable.Post;
      end;
    Inc(Updated);
    UpdateLogFile('FileNames', DocFileKeyMsg, 'Updated');
  end;

begin { TfrmExport2.UpdatePhotoTable }
  Assert(false, 'Import is not working');
  result  := 0;
  Updated := 0;
  Added   := 0;
  Skipped := 0;
  lblStatus01.Caption := Format('Updating table "%s"', [SrcTable.TableName]);
  Application.ProcessMessages;
  SrcTable.First;

  frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update');

  try
    while not SrcTable.Eof do
      begin
        if fDoAbort then
          break;

        KeyFields := 'FILE_NAME;PHOTODATE';
        if DstTable.Locate( KeyFields,
                            VarArrayOf([SrcTable.fldFILE_NAME.AsString,
                                        SrcTable.fldPhotoDate.AsString]),
                            []) then // a record with this filename and date already exists. Update it maybe...?
          begin
            mr := mrCancel;
            DocFileKeyMsg := Format('Key     = %d', [SrcTable.fldKey.AsInteger]);
            if cbUpdateRecords.Checked then
              begin
                OK := SrcTable.fldUPDATED.AsDateTime >= DstTable.fldUPDATED.AsDateTime;
                if not OK then
                  begin
                    DocumentFile := DstTable.fldFILE_NAME.AsString;
                    Msg := Format('The source record for the file (%s) has a earlier update date (%s) than the destination record (%s). Update the destination anyway?',
                               [DocumentFile,
                                DateTimeToStr(SrcTable.fldUPDATED.AsDateTime),
                                DateTimeToStr(DstTable.fldUPDATED.AsDateTime)]);
                    Ext := ExtractFileExt(DocumentFile);
                    mt  := MediaTypeFromExtension(Ext);
                    if IsPhotoMedia(mt) then
                      with frmConfirmEachPhoto do
                        begin
                          Caption1      := 'Update the record?';

                          PhotoFileName := SrcTable.PathAndFileName;
                          mr            := ShowModal;
                          OK            := mr in [mrOK, mrYes];
                          fDoAbort      := mr = mrCancel;
                        end
                    else
                      begin
                        OK := Yes(Msg);
                        if OK then
                          mr := mrYes
                        else
                          mr := mrNo;
                      end;
                  end;

                if OK then
                  begin
//                  DstTable.Edit;
                    CopyLocationInfo;
                  end
                else
                  begin
                    UpdateLogFile('FileNames', DocFileKeyMsg, 'Skipped');
                    inc(Skipped);
                    if mr = mrCancel then
                      break
                  end;
              end
            else
              begin
                UpdateLogFile('FileNames', DocFileKeyMsg, 'Skipped');
                Inc(Skipped);
              end;
          end
        else // otherwise, add it as a new record
          begin
            DstTable.Append;
            CopyRecord(SrcTable, DstTable);
            DstTable.Post;

            CopyLocationInfo;

            DocFileKeyMsg := Format('Key = %d', [DstTable.fldKey.AsInteger]);
            UpdateLogFile('FileNames', DocFileKeyMsg, 'Added');
          end;

        // Add the corresponding FilePath record, if it dosen't already exist
        PathNoStr := Format('Path_No = %d', [SrcTable.fldPATH_NO.AsInteger]);
        if not DstTable.FilePathsTable.Locate(PATH_NO, SrcTable.fldPATH_NO.AsInteger, []) then
          begin // It does not currently exist in the FilePathsTable
            DstTable.FilePathsTable.Append;   // So add it with the same path number
            CopyRecord(SrcTable.FilePathsTable, DstTable.FilePathsTable);
            DstTable.FilePathsTable.Post;
            UpdateLogFile('FilePaths', PathNoStr, 'Added');
          end
        else // a record with that path number exists in the destination FilePaths table
          begin
            if not cbUpdateRecords.Checked then
              UpdateLogFile('FilePaths', PathNoStr, 'Skipped (not updating') else
            if SameFields(SrcTable.FilePathsTable, DstTable.FilePathsTable, ChangedField) then
              UpdateLogFile('FilePaths', PathNoStr, 'Skipped (no changes)') else
          { Something has changed and an update might be OK }
            if (SrcTable.FilePathsTable.fldUPDATED.AsDateTime <= DstTable.FilePathsTable.fldUPDATED.AsDateTime) then
              UpdateLogFile('FilePaths', PathNoStr, 'Skipped (Dest is Newer}')
            else { Source record is newer. Ask if they want to update the destination }
              if YesFmt('Update FilePath record: %s %s --> %s',
                       [PathNoStr,
                        SrcTable.FilePathsTable.fldFILE_PATH.AsString,
                        DstTable.FilePathsTable.fldFILE_PATH.AsString]) then
                begin
                  DstTable.FilePathsTable.Edit;
                  CopyRecord(SrcTable.FilePathsTable, DstTable.FilePathsTable);
                  DstTable.FilePathsTable.Post;
                  Msg := Format('FilePaths %s --> %s', [DstTable.FilePathsTable.fldFILE_PATH.AsString]);
                  UpdateLogFile(Msg, PathNoStr, 'Updated');
                end
          end;

        // Add the corresponding CopyRight record, if it dosen't already exist
        CopyRightStr := Format('Copy_ID = %s', [SrcTable.fldCOPYR_ID.AsString]);

        if SameText(SrcTable.CopyRightsTable.fldCOPYR_ID.AsString, SrcTable.fldCOPYR_ID.AsString) then// photo record is linked to a CopyRights record
          if not DstTable.CopyRightsTable.Locate(cCOPYR_ID, SrcTable.fldCOPYR_ID.AsString, [loCaseInsensitive]) then
            begin
              DstTable.CopyRightsTable.Append;
              CopyRecord(SrcTable.CopyRightsTable, DstTable.CopyRightsTable);
              DstTable.CopyRightsTable.Post;
              UpdateLogFile('CopyRights', CopyRightStr, 'Dest Added');
            end
          else
            begin
              if cbUpdateRecords.Checked and
                  (not SameFields(SrcTable.CopyRightsTable, DstTable.CopyRightsTable, ChangedField)) then
                begin
                  DstTable.CopyRightsTable.Edit;
                  CopyRecord(SrcTable.CopyRightsTable, DstTable.CopyRightsTable);
                  DstTable.CopyRightsTable.Post;
                  UpdateLogFile('CopyRights', CopyRightStr, 'Dest Updated');
                end
              else
                UpdateLogFile('CopyRights', CopyRightStr, 'Dest Exists - Skipped');
            end
        else
          UpdateLogFile('CopyRights', CopyRightStr, 'Not found in Source');

        SrcTable.Next;
        Inc(result);
        lblStatus02.Caption := Format('%d updated; %d added, %d skipped', [Updated, Added, Skipped]);
        Application.ProcessMessages;
      end;
    lblStatus01.Caption := 'Database Updated';
    lblStatus02.Caption := Format('%d updated; %d added, %d skipped', [Updated, Added, Skipped]);
    Application.ProcessMessages;
  finally
    FreeAndNil(frmConfirmEachPhoto);
  end;
end;  { TfrmExport2.UpdatePhotoTable }

procedure TfrmExport2.Update_Status(const Msg: string; LineNo: integer = 1);
begin
  case LineNo of
    1: lblStatus01.Caption := Msg;
    2: lblStatus02.Caption := Msg;
  end;
  Application.ProcessMessages;
end;

procedure TfrmExport2.Update_StatusFmt(const Msg: string; Args: array of const; LineNo: integer = 1);
begin
  Update_Status(Format(Msg, Args), LineNo);
end;


procedure TfrmExport2.CopyAFile(cnt: integer; const SrcFileName, DstFileName: string);
var
  ErrMessage, Temp: string;
begin { CopyAFile }
  try
    if FileExists(SrcFileName) then
      begin
        if cbOverwriteExisting.Checked and FileExists(DstFileName) then
          Temp := ' OverWrote '
        else
          Temp := ' Copied    ';

        if CopyFile(pchar(SrcFileName), pchar(DstFileName), not cbOverwriteExisting.Checked) then
          WriteLn(fLogFile, Cnt+1:6, Temp:11, ExtractFileName(SrcFileName), ' to ', ExtractFilePath(DstFileName))
        else
          RaiseLastOSError;
      end;

  except
    on e:Exception do
      begin
        ErrMessage := PrintableOnly2(e.Message);
        WriteLn(fLogFile, Cnt+1:6, ' FAILED:  ':11, ExtractFileName(SrcFileName), ' to ', ExtractFilePath(DstFileName), ' [',
                                   ErrMessage, ']');
        Update_Status(ErrMessage, 2);
      end;
  end;
end;  { CopyAFile }

procedure TfrmExport2.CopyErrorLogs(var cnt: integer; SrcPath, DstPath: string);
var
  SrcFileName, DstFileName, ErrorLogName: string;
begin { CopyErrorLogs }
  ErrorLogName := ExtractFileBase(ParamStr(0)) + '.elf';
  Update_Status('Copying error logs');
  SrcFileName  := ForceBackSlash(SrcPath) + ErrorLogName;
  DstFileName  := ForceBackSlash(DstPath) + ErrorLogName;
  CopyAFile(cnt, SrcFileName, DstFileName);
  Inc(Cnt);
end;  { CopyErrorLogs }

procedure TfrmExport2.ExportSelectedRecords;
var
  FileName: string;
  ExportFolder: string;
  SrcFileName, DstFileName: string;
  DBCreator : TDBCreator;
  OK: boolean;
  DestPhotoTable: TPhotoTable;
  Options: TPhotoTableOptions;
  Temp: string;
  Cnt, RecCnt: integer;

begin { TfrmExport2.ExportSelectedRecords }
  Update_Status('Export initializing');
  Options   := [optNoFilePathsTable, optNoCopyRightsTable, optNoUpdateDate, optUseClient];
  FileName  := leFileName.Text;
  if FileExists(FileName) then
    begin
      OK := YesFmt('Database "%s" already exists. OK to overwrite it?', [FileName]);
      if OK then
        begin
          OK := MyDeleteFile(FileName);
          if not OK then
            AlertFmt('Unable to delete database "%s"', [FileName]);
        end
    end
  else
    OK := true;

  if OK then
    begin
      fLogPathFileName := CalcLogFileName('ExportSelected.txt');
      AssignFile(fLogFile, fLogPathFileName);
      Rewrite(fLogFile);

      Cnt := 0;
      ExportFolder := RemoveTrailingBackSlash(ExtractFilePath(FileName));

      Update_StatusFmt('Copying error logs to %s', [ExportFolder]);
      CopyErrorLogs(cnt, gExePath, ExportFolder);

      DBCreator := TDBCreator.Create;
      try
        with DBCreator do
          begin
            Update_StatusFmt('Creating %s', [FileName]);
            CreateDatabase(FileName, dv_Access2000);

            with fMainForm as TfrmPhotoDataBase do
              begin
                Update_StatusFmt('Creating tables in %s', [FileName]);
                CreateTable(cFILENAMES, tblPhotoTable);
                CreateTable(cFILEPATHS, tblPhotoTable.FilePathsTable);
                CreateTable(cCOPYRIGHTS, tblPhotoTable.CopyRightsTable);
                CreateTable(cLOCATIONS,  tblPhotoTable.LocationsTable);

                try
                  Update_StatusFmt('Opening table %s in %s', [cFILENAMES, FileName]);
                  DestPhotoTable := TPhotoTable.Create(self, FileName, cFILENAMES, Options);
                  DestPhotoTable.Open;

                  Update_StatusFmt('Opening table %s in %s', [cFILEPATHS, FileName]);
                  DestPhotoTable.FilePathsTable := TFilePathsTable.Create(self,
                                                                          FileName,
                                                                          cFILEPATHS,
                                                                          Options);
                  DestPhotoTable.FilePathsTable.Open;

                  Update_StatusFmt('Opening table %s in %s', [cCOPYRIGHTS, FileName]);
                  DestPhotoTable.CopyRightsTable := TCopyRightsTable.Create( self,
                                                                             FileName,
                                                                             cCOPYRIGHTS,
                                                                             Options);
                  DestPhotoTable.CopyRightsTable.Open;

                  Update_StatusFmt('Opening table %s in %s', [cFILENAMES,
                                                          CommonPhotoSettings.PhotoDBDatabaseFileName]);
                  tempPhotoTable := TPhotoTable.Create( self,
                                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                        cFILENAMES,
                                                        [optNoUpdateDate, optUseClient]);
                  with tempPhotoTable do
                    begin
                      IndexFieldNames := Order[CurrentOrder].IndexName;
                      OnFilterRecord  := PhotoTableFilterRecord;
                      Filtered        := true;
                      Active          := true;
                      SetSelectivityParserExpression(Expression);
                    end;

                  RecCnt := CopyPhotoTable(tempPhotoTable, DestPhotoTable);

                  // now, copy the actual documents to the destination folder
                  try
                    with tempPhotoTable do
                      begin
                        First;
                        while not eof do
                          begin
                            if fDoAbort then
                              Break;

                            SrcFileName  := PathAndFileName;
                            DstFileName  := ExportFolder + FilePathsTable.fldFILE_PATH.AsString + '\' + fldFile_Name.AsString;
                            ForceDirectories(RemoveTrailingBackSlash(ExtractFilePath(DstFileName)));

                            lblStatus01.Caption := 'Copying file';
                            lblStatus02.Caption := Format('%d/%d: %s', [Cnt, RecCnt, SrcFileName]);
                            Application.ProcessMessages;

                            CopyAFile(cnt, SrcFileName, DstFileName);

                            Next;
                            Inc(Cnt);
                          end;
                        lblStatus01.Caption := 'File copy completed';
                        lblStatus02.Caption := Format('%d/%d files processed', [Cnt, RecCnt]);
                      end;
                  finally
                  end;

                finally
                  tempPhotoTable.Close;
                  FreeAndNil(tempPhotoTable);

                  DestPhotoTable.Close;
                  FreeAndNil(DestPhotoTable);
                end;
              end;
          end;
      finally
        if fDoAbort then
          WriteLn(fLogFile, 'Process aborted by operator');
        CloseFile(fLogFile);
        temp := Format('Notepad.exe %s', [fLogPathFileName]);
        FileExecute(temp, false);
        FreeAndNil(DBCreator);
      end;
    end;
end;  { TfrmExport2.ExportSelectedRecords }

constructor TfrmExport2.Create(aOwner: TComponent; ImportExportMode: TImportExportMode);
begin
  inherited Create(aOwner);
  fMainForm := aOwner as TForm;
  fImportExportMode := ImportExportMode;
  leFileName.Text   := ForceBackSlash(CommonPhotoSettings.ImportExportFolder) + EXPORTDB;
  case fImportExportMode of
    iem_Export:
      begin
        Caption := 'Export Selected Records';
        leFileName.EditLabel.Caption := 'Output File Name';
      end;
    iem_Import:
      begin
        Caption := 'Import Selected Records';
        leFileName.EditLabel.Caption := 'Input File Name';
      end;
  end;
  cbUpdateRecords.Visible := ImportExportMode = iem_Import;
end;

procedure TfrmExport2.FormCreate(Sender: TObject);
begin
  lblStatus01.Caption := '';
  lblStatus02.Caption := '';
end;

procedure TfrmExport2.btnBeginClick(Sender: TObject);
begin
  if not fInprogress then
    begin
      fInProgress := true;
      fDoAbort    := false;
      case fImportExportMode of
        iem_Export: ExportSelectedRecords;
        iem_Import: ImportSelectedRecords;
      end;
      fInProgress := false;
    end
  else
    fDoAbort := true;
end;

procedure TfrmExport2.ImportSelectedRecords;
var
  FileName       : string;
  ExportFolder   : string;
  SrcFileName,
  DstFileName    : string;
  SrcPhotoTable,
  DstPhotoTable  : TPhotoTable;
  SrcOptions,
  DstOptions     : TPhotoTableOptions;
  Temp           : string;
  Cnt, RecCnt, Updated, Added, Skipped: integer;
begin { TfrmExport2.ImportSelectedRecords }
  lblStatus01.Caption := 'Import initializing';
  Application.ProcessMessages;

  SrcOptions   := [optReadOnly, optNoUpdateDate, optUseClient, optLevel12];
  DstOptions   := [optUseClient, optNoSyncFilePaths, optNoSyncCopyrights,
                   optNoUpdateDate, optLevel12];
  FileName     := leFileName.Text;
  ExportFolder := RemoveTrailingBackSlash(ExtractFilePath(FileName));

  fLogPathFileName := CalcLogFileName('ImportedRecords.txt');
  AssignFile(fLogFile, fLogPathFileName);
  Rewrite(fLogFile);

  if cbOverWriteExisting.Checked then
    WriteLn(fLogFile, 'Document files overwrite disabled');

  (* input files *)
  SrcPhotoTable := TPhotoTable.Create(self, FileName, cFILENAMES, SrcOptions);
  SrcPhotoTable.Open;

  (* Output files *)
  Update_StatusFmt('Opening table %s in %s', [cFILENAMES, CommonPhotoSettings.PhotoDBDatabaseFileName]);
  DstPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        DstOptions);
  DstPhotoTable.Active := true;
  DstPhotoTable.Inhibited := true;  // allow Year to be changed without forcing changes to Month, Day

  FillChar(fLocationInfo, SizeOf(TLocationInfo), 0);
  DstPhotoTable.LocationsTable.InitLocationInfo( fLocationInfo,
                                                 ls_Unknown,
                                                 CommonPhotoSettings.DefaultLocation);

  if cbUpdateRecords.Checked then
    RecCnt := UpdatePhotoTable(SrcPhotoTable, DstPhotoTable, Updated, Added, Skipped)
  else
    begin
      RecCnt := SrcPhotoTable.RecordCount;
      WriteLn(fLogFile, 'PhotoDB Record update disabled');
    end;

  with fMainForm as TfrmPhotoDataBase do
    begin
      try
        Update_StatusFmt('Opening table %s in %s', [cFILEPATHS, CommonPhotoSettings.PhotoDBDatabaseFileName]);

        Update_StatusFmt('Opening table %s in %s', [cCOPYRIGHTS, CommonPhotoSettings.PhotoDBDatabaseFileName]);
        DstPhotoTable.CopyRightsTable := TCopyRightsTable.Create( self,
                                                                  CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                                  cCOPYRIGHTS,
                                                                  DstOptions);
        DstPhotoTable.CopyRightsTable.Open;

        // now, copy the actual documents to the destination folder
        if cbCopyDocuments.Checked then
          begin
            CopyErrorLogs(cnt, ExportFolder, gExePath);
            
            with SrcPhotoTable do
              begin
                Cnt := 0;
                First;
                while not eof do
                  begin
                    if fDoAbort then
                      Break;
                    DstFileName  := ForceBackSlash(RemoveTrailingBackSlash(gRootPath) + // get destination folder from settings
                                    FilePathsTable.fldFILE_PATH.AsString) +             // get the path from the source folder
                                    fldFile_Name.AsString;     // get filename from the Source filename
                    SrcFileName  := ExportFolder +             // get Source root folder from user
                                    FilePathsTable.fldFILE_PATH.AsString + '\' +  // get the path from the source folder
                                    fldFile_Name.AsString;     // get filename from the Source filename
                    ForceDirectories(RemoveTrailingBackSlash(ExtractFilePath(DstFileName)));

                    lblStatus01.Caption := 'Copying file';
                    lblStatus02.Caption := Format('%d/%d: %s', [Cnt, RecCnt, SrcFileName]);
                    Application.ProcessMessages;

                    CopyAFile(cnt, SrcFileName, DstFileName);

                    Next;
                    Inc(Cnt);
                  end;
                lblStatus01.Caption := 'File copy completed';
                Temp := Format('%d/%d files copied', [Cnt, RecCnt]);
                lblStatus02.Caption := Temp;
              end;
          end;

      finally
        if fDoAbort then
          WriteLn(fLogFile, 'Process aborted by operator');
        WriteLn(fLogFile, Temp);
        CloseFile(fLogFile);
        temp := Format('Notepad.exe %s', [fLogPathFileName]);
        FileExecute(temp, false);

        DstPhotoTable.Close;
        FreeAndNil(DstPhotoTable);

        SrcPhotoTable.Close;
        FreeAndNil(SrcPhotoTable);

        ClosePhotoTable;          // force complete refresh
        OpenPhotoTable;
      end;
    end;
end;  { TfrmExport2.ImportSelectedRecords }

procedure TfrmExport2.btnBrowseClick(Sender: TObject);
var
  FilePathAndName: string;
begin
  FilePathAndName := leFileName.Text;
  if BrowseForFile('Browse for output', FilePathAndName, MDB_EXT) then
    leFileName.Text := FilePathAndName;
end;

procedure TfrmExport2.cbCopyDocumentsClick(Sender: TObject);
begin
  cbOverwriteExisting.Enabled := cbCopyDocuments.Checked;
end;

end.
