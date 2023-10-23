unit BrowseScenes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PhotoTableBrowser, Menus, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, DB,
  BrowserUnit, PDB_Decl;

type
  TFieldData = record
    FieldName: string;
    FieldValue: variant;
  end;

  TfrmBrowseScenes = class(TfrmPhotoTableBrowser)
    Utilities1: TMenuItem;
    CopyScenesfromAnotherPhotoTableRecord1: TMenuItem;
    RecalculateSceneLengths1: TMenuItem;
    Label1: TLabel;
    N5: TMenuItem;
    Orderby1: TMenuItem;
    PhhotoDate1: TMenuItem;
    PhtoKey1: TMenuItem;
    Updated1: TMenuItem;
    Added1: TMenuItem;
    N1: TMenuItem;
    CopyRecord1: TMenuItem;
    AppendandPasteRecord1: TMenuItem;
    Export1: TMenuItem;
    toCSV1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SetVideoLength1: TMenuItem;
    CalculateSceneStartsBasedsonSceneLengths1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CopyScenesfromAnotherPhotoTableRecord1Click(Sender: TObject);
    procedure CopyRecord1Click(Sender: TObject);
    procedure PasteRecord1Click(Sender: TObject);
    procedure RecalculateSceneLengths1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PhhotoDate1Click(Sender: TObject);
    procedure PhtoKey1Click(Sender: TObject);
    procedure Updated1Click(Sender: TObject);
    procedure Added1Click(Sender: TObject);
    procedure toCSV1Click(Sender: TObject);
    procedure CalculateSceneStartsBasedsonSceneLengths1Click(
      Sender: TObject);
  private
    { Private declarations }
    fSavedRecord: Array of TFieldData;
    fSavedKey: integer;
    fPhotoKey: Integer;
    fPhotoKeyWords: string;
    procedure DebugAfterScroll(DataSet: TDataSet);
    procedure SetKeyWords(const Value: string);
    procedure SetPhotoKey(const Value: Integer);
  protected
    function AllowFiltering: boolean; override;
    function CalcDescriptionString: string; override;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; DataSet: TDataSet; DataSetName: string = ''); override;
    Destructor Destroy; override;
    procedure FilterRecord(Dataset: TDataSet; var Accept: boolean); override;
    procedure ClearPhotoKey;
    procedure ScanForDate(DateType: TDateTypes; LowDate, HighDate: string);
    procedure ScanForKeyWords(KeyWords: string; PhotoKeyWords: string);

    property PhotoKeyWords: string
             read fPhotoKeyWords
             write fPhotoKeyWords;
    property KeyWords: string
             write SetKeyWords;
    property PhotoKey: integer
             read fPhotoKey
             write SetPhotoKey;
  end;

implementation

{$R *.dfm}

uses
  PDBTables, MyTables_Decl, MyUtils, PDBUtils, uGetString, VideoStuff2;

procedure TfrmBrowseScenes.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caHide;
  (Owner as TForm).Perform(WM_HideScenes, 0, 0)
end;

function AlsoSeeStr(CurrentAlso: Variant; NewAlso: string): string;
begin { AlsoSeeStr }
  if VarIsNull(CurrentAlso) or (CurrentAlso = '') then
    result := NewAlso
  else
    result := Trim(CurrentAlso) + ', ' + NewAlso;
end;  { AlsoSeeStr }

procedure TfrmBrowseScenes.CopyScenesfromAnotherPhotoTableRecord1Click(
  Sender: TObject);
var
  TempPhotoTable, ParentPhotoTable: TPhotoTable;
  PhotoTableBrowser: TfrmPhotoTableBrowser;
  TheFileName_Key: integer;
  CurrentScenesTable, TempScenesTable: TScenesTable;

  procedure CopyScenesRecord(Dst, Src: TScenesTable; const NewKey, OldKey: integer);
  begin { CopyScenesRecord }
    Dst.Append;
    Dst.fldFILENAME_KEY.AsVariant    := NewKey;
    Dst.fldKEY_WORDS.AsVariant       := Src.fldKEY_WORDS.AsVariant;
//  Dst.fldADDED.AsVariant           := Src.fldADDED.AsVariant;         // set during the posting process
//  Dst.fldUPDATED.AsVariant         := Src.fldUPDATED.AsVariant;
    Dst.fldPhotoDateTime.AsVariant   := Src.fldPhotoDateTime.AsVariant;
    Dst.fldPhotoDate.AsVariant       := Src.fldPhotoDate.AsVariant;
//  Dst.fldYear.AsVariant            := Src.fldYear.AsVariant;          // set during the posting process
//  Dst.fldMonth.AsVariant           := Src.fldMonth.AsVariant;         // set during the posting process
//  Dst.fldDay.AsVariant             := Src.fldDay.AsVariant;           // set during the posting process
    Dst.fldComment.AsVariant         := Src.fldComment.AsVariant;
    Dst.fldStartInSeconds.AsVariant  := Src.fldStartInSeconds.AsVariant;
//  Dst.fldStartFrame.AsVariant      := Src.fldStartFrame.AsVariant;    // set during the posting process
    Dst.fldLengthInSeconds.AsVariant := Src.fldLengthInSeconds.AsVariant;
//  Dst.fldLengthInFrames.AsVariant  := Src.fldLengthInFrames.AsVariant;   // set during the posting process
    Dst.fldAlsoSee.AsString          := AlsoSeeStr(Dst.fldAlsoSee.AsString, IntToStr(OldKey));   // link the new to the old
    Dst.Post;
  end;  { CopyScenesRecord }

begin
  inherited;
  ParentPhotoTable := DataSet.Owner as TPhotoTable;
  TempPhotoTable   := TPhotoTable.Create( self,
                                          ParentPhotoTable.DBFilePathName,
                                          cFILENAMES,
                                          [optReadOnly,
                                           optNoUpdateDate,
                                           optNoFilePathsTable,
                                           optNoCopyRightsTable]);
  try
    TempPhotoTable.Active := true;
    PhotoTableBrowser := TfrmPhotoTableBrowser.Create(self, TempPhotoTable);
    if PhotoTableBrowser.ShowModal = mrOK then
      begin
        TheFileName_Key    := TempPhotoTable.fldKey.AsInteger;
        CurrentScenesTable := DataSet as TScenesTable;
        TempScenesTable    := TScenesTable.Create(self, ParentPhotoTable.DBFilePathName, cSCENES, [optUseClient]);
        try
          TempScenesTable.IndexName := FILENAME_KEY;
          TempScenesTable.Active    := true;
          if TempScenesTable.Locate(FILENAME_KEY, TheFileName_Key, [] ) then
            begin
              while (TempScenesTable.fldFILENAME_KEY.AsInteger = TheFileName_Key) and (not TempScenesTable.Eof) do
                begin
                  CopyScenesRecord(CurrentScenesTable, TempScenesTable, ParentPhotoTable.fldKey.AsInteger, TheFileName_Key);
                  TempScenesTable.Next;
                end;

              // Link the Old to the New
              with TempScenesTable do
                begin
                  First;
                  while not eof do
                    begin
                      Edit;
                      fldAlsoSee.AsString := AlsoSeeStr(fldAlsoSee.AsString, IntToStr(TheFileName_Key));
                      Post;
                      Next;
                    end;
                end;
            end;
        finally
          FreeAndNil(TempScenesTable);
        end;
      end;
  finally
    FreeAndNil(PhotoTableBrowser);
    FreeAndNil(TempPhotoTable);
  end;
end;

procedure TfrmBrowseScenes.CopyRecord1Click(Sender: TObject);
var
  i: integer;
  Field: TField;
  ParentPhotoTable: TPhotoTable;
begin
  inherited;
  ParentPhotoTable := DataSet.Owner as TPhotoTable;
  fSavedKey        := ParentPhotoTable.fldKey.AsInteger;

  SetLength(fSavedRecord, 0);
  SetLength(fSavedRecord, DataSet.Fields.Count);
  for i := 0 to DataSet.Fields.Count-1 do
    with fSavedRecord[i] do
      begin
        Field      := DataSet.Fields[i];
        FieldName  := Field.FieldName;
        FieldValue := Field.AsVariant;
      end;
end;

procedure TfrmBrowseScenes.DebugAfterScroll(DataSet: TDataSet);
begin
  //
end;

constructor TfrmBrowseScenes.Create(aOwner: TComponent; DataSet: TDataSet;
  DataSetName: string);
begin
  inherited;
  SetLength(fSavedRecord, DataSet.Fields.Count);
  DataSet.AfterScroll  := DebugAfterScroll;
  DataSet.AfterRefresh := DebugAfterScroll;
end;

destructor TfrmBrowseScenes.Destroy;
begin

  inherited;
end;

procedure TfrmBrowseScenes.PasteRecord1Click(Sender: TObject);
var
  i: integer;
  Field: TField;
  TheFileName_Key: integer;
  ParentPhotoTable: TPhotoTable;
begin
  inherited;
  ParentPhotoTable := DataSet.Owner as TPhotoTable;
  TheFileName_Key  := ParentPhotoTable.fldKey.AsInteger;
  DataSet.Append;
  for i := 0 to Length(fSavedRecord)-1 do
    begin
      Field := DataSet.FindField(fSavedRecord[i].FieldName);
      if Assigned(Field) and (not (Field.DataType in [ftAutoInc])) then
        if SameText(fSavedRecord[i].FieldName, FILENAME_KEY) then
          Field.AsInteger := TheFileName_Key else
        if SameText(fSavedRecord[i].FieldName, cALSOSEE) then
          Field.AsString := AlsoSeeStr(fSavedRecord[i].FieldValue, IntToStr(fSavedKey))
        else
          if not VarIsNull(fSavedRecord[i].FieldValue) then
            Field.AsVariant := fSavedRecord[i].FieldValue;
    end;
  DataSet.Post;
end;

procedure TfrmBrowseScenes.FilterRecord(Dataset: TDataSet;
  var Accept: boolean);
begin
  inherited FilterRecord(DataSet, Accept);
  with DataSet as TScenesTable do
    if PhotoKey > 0 then  // accept everything
      Accept := Accept and (fldFILENAME_KEY.AsInteger = PhotoKey);
end;

function TfrmBrowseScenes.AllowFiltering: boolean;
begin
  result := (inherited AllowFiltering) or (PhotoKey > 0);
end;

procedure TfrmBrowseScenes.ClearPhotoKey;
begin
  PhotoKey := -1;
  ResetFilter;
end;

procedure TfrmBrowseScenes.SetPhotoKey(const Value: Integer);
begin
  fPhotoKey := Value;
  ResetFilter;
end;

function TfrmBrowseScenes.CalcDescriptionString: string;
begin
  result := inherited CalcDescriptionString;
  
  if PhotoKey > 0 then
    AddClause(result, 'PhotoKey', IntToStr(PhotoKey));
end;

procedure TfrmBrowseScenes.FormHide(Sender: TObject);
begin
  inherited;
  fDataSet.Filtered       := false;
  fDataset.OnFilterRecord := fSavedFilter;
  fDataSet.Filtered       := fWasFiltering;
end;

procedure TfrmBrowseScenes.PhhotoDate1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TScenesTable).IndexFieldNames := PHOTODATE;
end;

procedure TfrmBrowseScenes.PhtoKey1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TScenesTable).IndexFieldNames := FILENAME_KEY;
end;

procedure TfrmBrowseScenes.Updated1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TScenesTable).IndexFieldNames := DATE_UPDATED;
end;

procedure TfrmBrowseScenes.Added1Click(Sender: TObject);
begin
  inherited;
  (DataSet as TScenesTable).IndexFieldNames := DATE_ADDED;
end;


procedure TfrmBrowseScenes.SetKeyWords(const Value: string);
begin
end;

procedure TfrmBrowseScenes.ScanForDate(DateType: TDateTypes; LowDate, HighDate: string);
var
  FoundOne: boolean;
  FoundField1, FoundField2: TField;
begin
  with DataSet as TScenesTable do
    begin
      FoundOne := false;
      First;
      while not eof do
        begin
          if CheckLowDate (DateType, LowDate,  fldPhotoDate, fldAdded, fldUpdated, fldPhotoDateTime, FoundField1) and
             CheckHighDate(DateType, HighDate, fldPhotoDate, fldAdded, fldUpdated, fldPhotoDateTime, FoundField2) then
            begin
              FoundOne := true;
              break;
            end;
          Next;
        end;
      if FoundOne then
        begin
          if Assigned(FoundField1) then
            DBGrid1.SelectedField := FoundField1
          else if Assigned(FoundField2) then
            DBGrid1.SelectedField := FoundField2;
        end
      else
        First;
    end;
end;

procedure TfrmBrowseScenes.ScanForKeyWords(KeyWords, PhotoKeyWords: string);
var
  FoundSome, FoundAll: TBookMark;
begin
  FoundSome := nil;
  Foundall  := nil;

  with DataSet as TScenesTable do
    begin
      First;
      while not eof do
        begin
          if ContainsWords( KeyWords,
                            fldKey_Words.AsString,
                            true,
                            false  { one or more for a match} ) then
            FoundSome := GetBookMark;

          if ContainsWords( KeyWords,
                            PhotoKeyWords + ' ' + fldKey_Words.AsString,
                            true,
                            true { all must match }) then
            begin
              FoundAll := GetBookMark;
              Break;
            end;
          Next;
        end;

      if Assigned(FoundAll) then
        begin
          GotoBookMark(FoundAll);
          DBGrid1.SelectedField := fldKey_Words;
        end else
      if Assigned(FoundSome) then
        begin
          GotoBookMark(FoundSome);
          DBGrid1.SelectedField := fldKey_Words;
        end
      else
        First;
    end;
end;

procedure TfrmBrowseScenes.toCSV1Click(Sender: TObject);
var
  ScenesTable: TScenesTable;
  OutFile: TextFile;
  Line, Temp: string;
  i: integer;

  procedure AddCSVField(FieldVal: string);
  begin { AddCSVField }
    if ContainsAny(FieldVal, [',', ';', #9]) then
      FieldVal := '"' + FieldVal + '"';

    if Line = '' then
      Line := FieldVal
    else
      Line := Line + ',' + FieldVal;
  end;  { AddCSVField }

begin { TfrmBrowseScenes.toCSV1Click }
  inherited;
  with OpenDialog1 do
    begin
      if Execute then
        begin
          AssignFile(OutFile, FileName);
          ReWrite(OutFile);
          try
            ScenesTable   := DataSet as TScenesTable;
            with ScenesTable do
              begin
                DisableControls;
                try
                  Line := '';
                  for i := 0 to Fields.Count-1 do
                    AddCSVField(Fields[i].FieldName);
                  WriteLn(OutFile, Line);
                  First;
                  while not eof do
                    begin
                      Line := '';
                      for i := 0 to Fields.Count-1 do
                        AddCSVField(Fields[i].AsString);
                      WriteLn(OutFile, Line);
                      Next;
                    end;
                finally
                  EnableControls;
                end;
              end;
          finally
            CloseFile(OutFile);
            temp := Format('Notepad.exe %s', [FileName]);
            FileExecute(temp, false);
          end;
        end;
    end;
end;  { TfrmBrowseScenes.toCSV1Click }

procedure TfrmBrowseScenes.RecalculateSceneLengths1Click(Sender: TObject);
var
  NrScenes, aPhotoKey: integer;
  ScenesTable: TScenesTable;
  SceneStartsInSeconds: array of double;
  VideoLength, SceneLength: double;
  aResult: string;
  ParentPhotoTable: TPhotoTable;
begin
  inherited;
  ScenesTable      := DataSet as TScenesTable;
  with ScenesTable do
    begin
      MyDisableControls;
      if State in [dsEdit, dsInsert] then
        Post;
      try
        // 1st pass: accumulate the scene starts
        First;
        ParentPhotoTable := DataSet.Owner as TPhotoTable;
        NrScenes := 0; VideoLength := 0;
        while not eof do
          begin
            SetLength(SceneStartsInSeconds, NrScenes+1);
            if fldStartInSeconds.AsInteger > 0 then
              SceneStartsInSeconds[NrScenes] := fldStartInSeconds.AsInteger
            else
              if not Empty(fldStartFrame.AsString) then
                SceneStartsInSeconds[NrScenes] := FrameToSeconds(fldStartFrame.AsString);
            Next;
            inc(NrScenes);
          end;

        // second pass: set the scene lengths
        aPhotoKey := 0;
        First;
        while not Eof do
          begin
            Edit;
            fldLengthInFrames.Clear;
            fldStartFrame.Clear;
            if aPhotoKey < (NrScenes-1) then
              begin
                SceneLength := SceneStartsInSeconds[aPhotoKey+1] - SceneStartsInSeconds[aPhotoKey];
                fldLengthInSeconds.AsFloat  := SceneLength;
                VideoLength := VideoLength + SceneLength;
              end
            else
              if aPhotoKey = (NrScenes-1) then
                begin
                  aResult := FloatToStr(FrameToSeconds(ParentPhotoTable.fldMEDIA_LENGTH.AsString));
                  if GetString( 'Enter Video Length', 'Length (sec)', aResult) then
                    begin
                      try
                        VideoLength := StrToFloat(aResult);
                      except
                        ErrorFmt('Length "%s" must be an integer', [aResult]);
                      end;
                    end;
                  SceneLength                 := VideoLength - SceneStartsInSeconds[aPhotoKey];
                  fldLengthInSeconds.AsFloat  := SceneLength;
                end;
            Post;
            Next;
            inc(aPhotoKey);
          end;

      finally
        MyEnableControls;
      end;
    end;
end;

procedure TfrmBrowseScenes.CalculateSceneStartsBasedsonSceneLengths1Click(
  Sender: TObject);
var
  ScenesTable: TScenesTable;
  StartTimeInSeconds: integer;
  SavedIndexName: string;
begin
  inherited;
  ScenesTable      := DataSet as TScenesTable;
  with ScenesTable do
    begin
      MyDisableControls;
      SavedIndexName  := IndexFieldNames;
      IndexFieldNames := PHOTODATETIME;
      if State in [dsEdit, dsInsert] then
        Post;
      try
        // 1st pass: if record has length in seconds but no starting time, set the starting time
        StartTimeInSeconds := 0;
        First;
        while not eof do
          begin
            if FieldIsEmpty(fldStartInSeconds, true) then
              begin
                Edit;
                fldStartInSeconds.AsInteger := StartTimeInSeconds;
                Post;
              end;
            if not fldStartInSeconds.IsNull then
              StartTimeInSeconds :=   StartTimeInSeconds + fldLengthInSeconds.AsInteger;
            Next;
          end;

      finally
        MyEnableControls;
        IndexFieldNames := SavedIndexName;
      end;
    end;
end;

end.
