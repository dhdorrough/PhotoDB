unit PDBTables;

interface

uses
  SysUtils, DBTables, DB, Classes, ADODB, PDB_Decl, PDBUtils, GPSTrackingInfoUnit,
  MyUtils, ParseExpr, MyTables, MyTables_Decl, DeletionConfirmation,
  LookupsTableUnit;

const
  FILE_NAME       = 'File_Name';
  PATH_NO         = 'PATH_NO';
  PARENT_NO       = 'Parent_NO';
  cCOPYR_ID       = 'Copyr_ID';
  cOWNER          = 'Owner';
  KEY_WORDS       = 'Key_words';
  EXT_JPG         = '*.jpg';
  PHOTODATE       = 'PhotoDate';
  PHOTODATETIME   = 'PhotoDateTime';
  FILE_TYPE       = 'File_Type';
  MEDIA_LENGTH    = 'Media_Length';
  DATE_UPDATED    = 'Updated';
  DATE_ADDED      = 'Added';
  cKEY            = 'Key';
  FILE_PATH       = 'File_Path';
  KEY_WORD        = 'Key_Word';
  SQL_KEY         = 'SQL_Key';
  cDATEISAGUESS   = 'DateIsAGuess';
  FILENAME_KEY    = 'FileName_Key';
  STARTINSECONDS  = 'StartInSeconds';
  SCENESINDEX     = 'ScenesInVideo';  // FileName_Key + StartInSeconds
  TIMEZONE_OFFSET = 'TimeZoneOffset';
  TIMEZONE        = 'TimeZone';
  cALSOSEE        = 'AlsoSee';
  cWidth          = 'Width';
  cHeight         = 'Height';
  cWasScanned     = 'WasScanned';
  cComment        = 'Comment';
  cDistance       = 'Distance';
  cLatitude       = 'Latitude';
  cLongitude      = 'Longitude';
  cHasSceneInfo   = 'HasSceneInfo';
  cYEAR           = 'Year';
  cMONTH          = 'Month';
  cDAY            = 'Day';
  cUpdateReason   = 'UpdateReason';
  cFILE_SIZE      = 'File_Size';
  cPrivate        = 'Private';
  cHasSound       = 'HasSound';
  cLocationID     = 'LocationID';
  cRating         = 'Rating';
  cTextInFile     = 'TextInFile';
  cID             = 'ID';
  cCUSTOM_KEY     = 'CUSTOM_KEY';

  NONEXISTANTFOLDER = 'NON-EXISTANT FOLDER';

  MAX_INDEX_LEN = 100;
  MAXDISTANCEINFEET = 150;  // require location to be at least this close

//  DEF_COPY_ID   = '?';

  // Rating values
  RATING_EXCELLENT = 2;
  RATING_GOOD      = 1;
  RATING_AVERAGE   = 0;
  RATING_POOR      = -1;
  RATING_TERRIBLE  = -2;

type

  TOrderInfo = record
                 SQL: string;
                 IndexName: string;
               end;

  TFilePathsTable = class(TMyTable)
  private
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforePost; override;
    procedure DoAfterScroll; override;
  public
    fldPATH_NO: TField;
    fldPARENT_NO: TField;
    fldFILE_PATH: TField;
    fldKEY_WORD: TField;
    fldSQL_KEY: TField;
    fldUPDATED: TField;

    Constructor Create( aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions); override;
    function FindFolderNo( aFolderPath: string;
                           var KeyWords: string;
                           OkToAdd: boolean;
                           AllowPartialKey: boolean = false): integer;
    function FindParentNo( aFolderPath: string; OkToAdd: boolean): integer;
    procedure InitFieldPtrs; override;
    procedure LocateUnusedPathNo;
  end;

  TLocationsTable = class(TMyTable)
  private
    fBoundariesDataSet: TDataSet;
    fBoundingRectangle : TRectangle;
    fLastLocationID: integer;
    fLastLocationState: string;
    fLastLatitude: double;
    fLastLongitude: double;
    fLastLocationSource: integer;
    procedure GetInBoundingRectangle(Eval_Tree: TEval_Tree);
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforePost; override;
    procedure DoAfterScroll; override;
  public
    fldID: TField;
    fldLatitude: TField;
    fldLongitude: TField;
    fldLatLongKey: TField;
    fldDescription: TField;
    fldLocationSource: TField;
    fldDateAdded: TField;
    fldDateUpdated: TField;
    fldPrivateLocation: TField;
    fldRefCnt: TField;
    fldState: TField;
    procedure AddOptionalFunctionsToParser(aParser: TParser); override;
    procedure AddFields; override;
    function BoundariesDataSet: TDataSet;

    procedure InitLocationInfo(var LocationInfo: TLocationInfo;
                               LocationSource: TLocationSource = ls_Unknown;
                               DefaultLocation: string = '';
                               DefaultState: string = '';
                               MaxDistanceInFeet: integer = MAXDISTANCEINFEET;
                               OkToAdd: boolean = true;
                               AlwaysAdd: boolean = false;
                               Simulate: boolean = false);
                               
    function FindLocationID( Latitude, Longitude: double;
                             LocationInfo: TLocationInfo;
                             var Added: integer;
                             var FoundDesc: string): integer; 
    function GetLocationDescription(LocationID: integer): string;
    function GetLatitude(LocationID: integer): double;
    function GetLongitude(LocationID: integer): double;
    function GetLatLon(LocationID: integer; var Lat, Lon: double): boolean;
    procedure InitFieldPtrs; override;
    function LocationSource(LocationID: integer): integer;
    function LocationState(LocationID: integer): string;
    class function HasLocationInfoInEXIF(const FileName: string): boolean;
    constructor Create( aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions); override;
    procedure DecrementRefCnt; overload;
    procedure DecrementRefCnt(aLocationID: integer); overload;
    procedure IncrementRefCnt; overload;
    procedure IncrementRefCnt(aLocationID: integer); overload;
    property OnCalcFields;
    Destructor Destroy; override;
  end;

  TCopyRightsTable = class(TMyTable)
  public
    fldCopyR_ID: TField;
    fldCopyRightsOwner: TField;
  protected
    procedure DoAfterOpen; override;
  end;

  TMemorabiliaTable = class(TMyTable)
  public
    fldID: TField;
    fldDateYYYYMMDD: TField;
    fldObjectType: TField;
    fldDescription: TField;
    fldFilePath: TField;
    fldFileName: TField;
    fldAuthor: TField;
    fldKeyWords: TField;
    fldDate: TField;
    fldYear: TField;
    fldMonth: TField;
    fldDay: TField;
    fldComment: TField;
    procedure DoAfterOpen; override;
  end;

  TGetBoundariesTableFunc = function {name}: TDataSet of object;

  TScenesTable = class(TMyTable)
  private
    fLastDateTime: string;
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure DoAfterInsert; override;
    procedure DoAfterScroll; override;
  public
    fldID: TField;
    fldFILENAME_KEY: TField;
    fldKEY_WORDS: TField;
    fldADDED: TField;
    fldUPDATED: TField;
    fldYear: TField;
    fldMonth: TField;
    fldDay: TField;
    fldPhotoDate: TField;
    fldPhotoDateTime: TField;
    fldComment: TField;
    fldStartInSeconds: TField;
    fldStartFrame: TField;
    fldLengthInFrames: TField;
    fldLengthInSeconds: TField;
    fldAlsoSee: TField;
    function FieldIsEmpty(Field: TField; ZeroIsEmpty: boolean = false): boolean;
    procedure InitFieldPtrs; override;
  end;

  TPhotoTable = class(TMyTable)
  private
    fBoundingRectangle : TRectangle;
    fCopy_ID           : string;
    fCopyRightsTable   : TCopyRightsTable;
    fCurrentOrder      : TCurrentOrder;
    fDeleteFilePending : boolean;
    fFilePathsTable    : TFilePathsTable;
    fFoldersList       : TStringList;  // used by the GetFileExistsInFolder function
    fFolderName        : string;       // used by the GetFileExistsInFolder function
    fLastLocationID    : integer;
    fLastLatitude      : double;
    fLastLongitude     : double;
    fLocationKindIsOk  : boolean;
    fLocationsTable    : TLocationsTable;
    fOnGetBoundariesTable: TGetBoundariesTableFunc;
    fOnUpdateStatus    : TUpdateStatusProc;
    fPhotoTableSource  : TDataSource;
    fSavedFileName     : string;
    fScenesTable       : TScenesTable;
    fTempPhotoTable    : TPhotoTable;
    fTempFilePathsTable: TFilePathsTable;
    fTotalRecordCount  : integer;
    frmDeletionConfirmation: TfrmDeletionConfirmation;

    function GetTempFilePathsTable: TFilePathsTable;
    procedure UpdatePhotoDateYear(sender: TField);
    procedure UpdatePhotoDateMonth(sender: TField);
    procedure UpdatePhotoDateDay(sender: TField);
    function GetPhotoDay: integer;
    procedure UpdatePhotoDate;
    function GetLocationsTable: TLocationsTable;
    procedure GetDistanceToLocationID(Eval_Tree: TEval_Tree);
    procedure GetEXIFInfo(Eval_Tree: TEval_Tree);
    procedure GetFilePath(Eval_Tree: TEval_Tree);
    procedure GetLatitude(Eval_Tree: TEval_Tree); overload;
    procedure GetLocationDescription(Eval_Tree: TEval_Tree);
    procedure GetLocationSource(Eval_Tree: TEval_Tree);
    procedure GetLocationState(Eval_Tree: TEval_Tree);
    procedure GetLongitude(Eval_Tree: TEval_Tree); overload;
    procedure GetInBoundingRectangle(Eval_Tree: TEval_Tree);
    procedure GetHasEXIF(Eval_Tree: TEval_Tree);
    procedure GetPathAndFileName(Eval_Tree: TEval_Tree);
    procedure GetThumbNailPathAndFileName(Eval_Tree: TEval_Tree);
    procedure GetThumbNailExists(Eval_Tree: TEval_Tree);
    function GetBoundariesTable: TDataSet;
    function GetScenesTableInUse: boolean;
    procedure SetScenesTableInUse(const Value: boolean);
    procedure GetFileExistsInFolder(Eval_Tree: TEval_Tree);
    procedure GetMediaTypeInSeconds(Eval_Tree: TEval_Tree);
    procedure SetOnGetBoundariesTable(
      const Value: TGetBoundariesTableFunc);
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforeClose; override;
    procedure DoAfterClose; override;
    procedure DoAfterScroll; override;
    procedure DoBeforePost; override;
    procedure DoAfterInsert; override;
    procedure DoAfterDelete; override;
    procedure KeyChanged(sender: TField);
  public
    fldKEY_WORDS: TField;
    fldPATH_NO: TField;
    fldFILE_NAME: TField;
    fldFILE_TYPE: TField;
    fldMEDIA_LENGTH: TField;
    fldADDED: TField;
    fldUPDATED: TField;
    fldYear: TField;
    fldMonth: TField;
    fldDay: TField;
    fldPhotoDate: TField;
    fldPhotoDateTime: TField;
    fldDateIsAGuess: TField;
    fldFile_Size: TField;
    fldCOPYR_ID: TField;
    fldPRIVATE: TField;
    fldHasSound: TField;
    fldLatLongKey: TField;
    fldLocationID: TField;
    fldRating: TField;
    fldKey: TField;
    fldWidth: TField;
    fldHeight: TField;
    fldWasScanned: TField;
    fldComment: TField;
    fldDistance: TField;
    fldLatitude: TField;
    fldLongitude: TField;
    fldTextInFile: TField;
    fldHasSceneInfo: TField;
    fldUpdateReason: TField;
    fldCustom_Key: TField;

    Inhibited: boolean;

    tblFilePathsPATH_NO: TField;
    tblFilePathsFILE_PATH: TField;
    tblFilePathsKEY_WORD: TField;
    fldCopyRightTable_CopyID: TField;
    fldFileNamesTable_CopyID: TField;
    fldOwner: TField;

    procedure AddOptionalFunctionsToParser(aParser: TParser); override;
    procedure ClearParser; override;
    function  CopyRightOwner: string;
    function  DateIsAGuess: boolean;
    function  DeletePhotoRecord(var WasEof: boolean): boolean;
    function  FullFilePath: string;
    function  GetLatitude: double; overload;
    function  GetLongitude: double; overload;
    procedure InitFieldPtrs; override;
    procedure SetOrder(anOrder: TCurrentOrder);
    function  MyLocateRecord(aFileName, aFolderPath: string): boolean;
    function  TotalRecordCount: Integer;
    procedure MyDisableControls; override;
    procedure MyEnableControls; override;
    property CurrentOrder: TCurrentOrder
             read fCurrentOrder
             write fCurrentOrder;
    constructor Create( aOwner: TComponent;
                        aDBFilePathName, aTableName: string;
                        Options: TPhotoTableOptions); override;
    destructor Destroy; override;
    property BoundariesTable: TDataSet
             read GetBoundariesTable;
    function PathAndFileName: string;
    function ScenesTable: TScenesTable;
    function ShortFilePath: string;
    property OnUpdateStatus: TUpdateStatusProc
             read fOnUpdateStatus
             write fOnUpdateStatus;

    procedure UpdateProc(    UpdateInfo: TUpdateInfo;
                             LocationInfo: TLocationInfo;
                         var KeyWords: string;
                             anUpdateStatusProc: TUpdateStatusProc);
    property FilePathsTable: TFilePathsTable
             read fFilePathsTable
             write fFilePathsTable;
    property LocationsTable: TLocationsTable
             read GetLocationsTable;
    property CopyRightsTable: TCopyRightsTable
             read fCopyRightsTable
             write fCopyRightsTable;
    property TempFilePathsTable: TFilePathsTable
             read GetTempFilePathsTable;
    property Copy_ID: string
             read fCopy_ID
             write fCopy_ID;
    property PhotoDay: integer
             read GetPhotoDay;
    property OnGetBoundariesTable: TGetBoundariesTableFunc
             read fOnGetBoundariesTable
             write SetOnGetBoundariesTable;
    property ScenesTableInUse: boolean
             read GetScenesTableInUse
             write SetScenesTableInUse;
  end;

  TPhotoTableQuery = class(TADOQuery)
    procedure DoAfterOpen; override;
  public
    fldKEY_WORDS: TField;
    fldPATH_NO: TField;
    fldPARENT_NO: TField;
    fldFILE_NAME: TField;
    fldFILE_SIZE: TField;
    fldFILE_TYPE: TField;
    fldADDED: TField;
    fldUPDATED: TField;
    fldYear: TField;
    fldMonth: TField;
    fldDay: TField;
    fldPhotoDate: TField;
    fldPhotoDateTime: TField;
    fldDateIsAGuess: TField;
    fldUpdateReason: TField;
    fldCOPYR_ID: TField;
    fldPRIVATE: TField;
    fldHasSound: TField;
    fldLocationID: TField;
    fldLatLongKey: TField;
    fldRating: TField;
    fldKey: TField;
    fldWidth: TField;
    fldHeight: TField;
    fldWasScanned: TField;
    fldComment: TField;
    fldDistance: TField;
    fldLatitude: TField;
    fldLongitude: TField;
    fldTextInFile: TField;
    fldHasSceneInfo: TField;
    fldMEDIA_LENGTH: TField;
    fldCustom_Key: TField;

    constructor Create(aOwner: TComponent); override;
  end;

  TFilePathsQuery = class(TADOQuery)
  protected
    procedure DoAfterOpen; override;
    procedure DoBeforePost; override;
  public
    fldPATH_NO: TField;
    fldPARENT_NO: TField;
    fldFILE_PATH: TField;
    fldKEY_WORD: TField;
    fldSQL_KEY: TField;
    fldUPDATED: TField;
    Constructor Create(aOwner: TComponent); override;
  end;

  EInvalidLocation = class(Exception);
  EMyUpdateError   = class(Exception);

function RemoveRootString(const Path: string): string;
function ScanForHighestFolderNumber(Starting_Number: integer; const DatabaseFileName: string): integer;

var
  Order: array[TCurrentOrder] of TOrderInfo =
         ({coNone}          (SQL: '';            IndexName: ''),
          {coFileName}      (SQL: 'File_Name';   IndexName: 'File_Name'),
          {coPathName}      (SQL: 'FilePaths.FILE_PATH, FileNames.FILE_NAME';  // This will only work with optUseClient
                                                 IndexName: 'PathName'),
          {coPhotoDate}     (SQL: 'Photo Date; FILE_NAME';  IndexName: 'PhotoDate'),
          {coPhotoDateTime} (SQL: 'Photo Date Time';  IndexName: 'PhotoDateTime'),
          {coFileSize}      (SQL: 'File_Size';   IndexName: 'File_Size'),
          {coDateAdded}     (SQL: 'Added';       IndexName: 'Added'),
          {coDateUpdated}   (SQL: 'Updated';     IndexName: 'Updated'),
          {DistancefromLocation}
                            (SQL: 'Distance';    IndexName: 'Distance'),
          {coLatitude}      (SQL: 'Latitude';    IndexName: 'Latitude'),
          {coLatitude}      (SQL: 'Longitude';   IndexName: 'Longitude'),
          {coNearness}      (SQL: 'Distance';    IndexName: 'Distance'),
          {coLatitudeDec}   (SQL: 'Latitude DESC';    IndexName: 'LatitudeDec'),
          {coLongitudeDec}  (SQL: 'Longitude DESC';   IndexName: 'LongitudeDec'),
          {coPhotoDateDesc} (SQL: 'PhotoDate DESC';   IndexName: 'PhotoDateDesc'),
          {coPhotoDateTimeDesc}
                            (SQL: 'PhotoDateTime DESC'; IndexName: 'PhotoDateTimeDec'),
          {coMediaLength}   (SQL: 'Media Length';     IndexName: 'Media_Length'),
          {coCustomKey}     (SQL: 'Custom Key';   IndexName: 'Custom_Key')
          );

function FixPath(const Path: string): string;
procedure Init_NoiseWords(NoiseWordsTable: TLookupsTable);

implementation

uses
  StStrL, Variants, DateUtils, PhotoDBCommonSettings,
  Controls, Forms, dExif, LocationUtils,
  HikingTables,
  PhotoUtils, dIPTC, Math, VideoStuff2
{$IfDef dExif2}
  , dMetadata, dGlobal
{$endIf}
  ;

function RemoveRootString(const Path: string): string;
var
  uRootPath: string;
  Len: integer;
begin
  uRootPath := WideUpperCase(gRootPath);
  Len       := Length(uRootPath);
  if WideUpperCase(Copy(Path, 1, len)) = uRootPath then
    result := Copy(Path, Len+1, Length(Path)-Len)
  else
    raise EInvalidLocation.CreateFmt('The file location "%s" is NOT valid. ' + CRLF+
                    'It must be located in the folder (or sub-folder) of "%s".', [Path, gRootPath]);
end;

{ TPhotoTable }

procedure TPhotoTable.InitFieldPtrs;
begin
  inherited;
  fldFILE_NAME      := FieldByName(FILE_NAME);
  fldKEY_WORDS      := FieldByName(KEY_WORDS);
  fldPATH_NO        := FieldByName(PATH_NO);
  fldFILE_TYPE      := FieldByName(FILE_TYPE);
  fldUPDATED        := FieldByName(DATE_UPDATED);
  fldADDED          := FieldByName(DATE_ADDED);
  fldYear           := FieldByName(cYEAR);
  fldYear.OnChange  := UpdatePhotoDateYear;
  fldMonth          := FieldByName(cMONTH);
  fldDay            := FindField(cDAY); // FieldByName('DAY');  - may not exist in older version of the DB
  fldMonth.OnChange := UpdatePhotoDateMonth;
  if Assigned(fldDay) then
    fldDay.OnChange   := UpdatePhotoDateDay;
  fldPhotoDate      := FindField(PHOTODATE);       // May not exist in older versions of the DB
  fldPhotoDateTime  := FindField(PHOTODATETIME);   // may not exist in older versions of the DB
  fldDateIsAGuess   := FindField(cDATEISAGUESS);   // May not exist in older versions of the DB
  fldUpdateReason   := FindField(cUpdateReason);   // may not exist in older versions of the DB
  fldFile_Size      := FindField(cFILE_SIZE);      // may not exist in older versions of the DB
  fldCOPYR_ID       := FindField(cCOPYR_ID);       // may not exist in older versions of the DB
  fldPRIVATE        := FindField(cPrivate);        // may not exist in older versions of the DB
  fldHasSound       := FindField(cHasSound);       // may not exist in older versions of the DB
  fldLocationID     := FindField(cLocationID);     // may not exist in older versions of the DB
  fldRating         := FindField(cRating);         // may not exist in older versions of the DB
  fldKey            := FieldByName(cKEY);
  fldWidth          := FindField(cWidth);          // may not exist in older versions of the DB
  fldHeight         := FindField(cHeight);         // may not exist in older versions of the DB
  fldWasScanned     := FindField(cWasScanned);     // may not exist in older versions of the DB
  fldComment        := FindField(cComment);        // may not exist in older versions of the DB
  fldDistance       := FindField(cDistance);       // may not exist in older versions of the DB
  fldLatitude       := FindField(cLatitude);       // may not exist in older versions of the DB
  fldLongitude      := FindField(cLongitude);      // may not exist in older versions of the DB
  fldHasSceneInfo   := FindField(cHasSceneInfo);   // may not exist in older versions of the DB
  fldKey.OnChange   := KeyChanged;
  fldKey.OnValidate := KeyChanged;
  fldTextInFile     := FindField(cTextInFile);     // may not exist in older versions of the DBR
  fldMEDIA_LENGTH   := FindField(MEDIA_LENGTH);    // does not exist in newer versions of the DB
  fldCustom_Key     := FindField(cCUSTOM_KEY);     // does not exist in older versions of the DB

  fldFileNamesTable_CopyID := FieldByName(cCOPYR_ID);

//  tblFilePathsPATH_NO   := FieldByName('FilePaths.PATH_NO');
  tblFilePathsPATH_NO   := nil;
  tblFilePathsFILE_PATH := nil;
  tblFilePathsKEY_WORD  := nil;
  if not (optNoFilePathsTable in fOptions) then
    with fFilePathsTable do
      if Active then
        begin
          tblFilePathsPATH_NO   := FieldByName('PATH_NO');
          tblFilePathsFILE_PATH := FieldByName(FILE_PATH);
          tblFilePathsKEY_WORD  := FieldByName(KEY_WORD);
        end;

  fldCopyRightTable_CopyID := nil;
  fldOwner                 := nil;
  if not (optNoCopyRightsTable in fOptions) then
    with fCopyRightsTable do
      if Active then
        begin
          fldCopyRightTable_CopyID := FieldByName(cCOPYR_ID);
          fldOwner                 := FieldByName(cOWNER);
        end;
end;

(*
procedure TPhotoTable.Debugger(DataSet: TDataSet);
var
  temp: string;
begin
  Assert(DataSet = fFilePathsTable);
  with fFilePathsTable do
    begin
      if Assigned(fldFILE_PATH) then
        temp := fldFILE_PATH.AsString
      else
        temp := '';
    end;
end;
*)

constructor TPhotoTable.Create( aOwner: TComponent;
                                aDBFilePathName, aTableName: string;
                                Options: TPhotoTableOptions);
begin
{$IfDef debugging}
  Message('Entering TPhotoTable.Create');
{$endIf}
  inherited;

  fPhotoTableSource := TDataSource.Create(self);
  fPhotoTableSource.DataSet := self;

  if not (optNoFilePathsTable in Options) then
    begin
      fFilePathsTable   := TFilePathsTable.Create(self, aDBFilePathName, cFILEPATHS, Options);
      if not (optNoSyncFilePaths in Options) then
        begin
          fFilePathsTable.MasterSource := fPhotoTableSource;
          fFilePathsTable.MasterFields := PATH_NO;
        end;
    end;

  if not (optNoCopyRightsTable in Options) then
    begin
      fCopyRightsTable  := TCopyRightsTable.Create(self, aDBFilePathName, cCOPYRIGHTS, Options);
      if not (optNoSyncCopyrights in Options) then
        begin
          fCopyRightsTable.MasterSource := fPhotoTableSource;
          fCopyRightsTable.MasterFields := cCOPYR_ID;
        end;
    end;

//  fCopy_ID := DEF_COPY_ID;
{$IfDef debugging}
  Message('Exiting TPhotoTable.Create');
{$endIf}
end;

destructor TPhotoTable.Destroy;
begin
  if Assigned(fTempPhotoTable) then
    FreeAndNil(fTempPhotoTable);
  if Assigned(fFilePathsTable) then
    FreeAndNil(fFilePathsTable);
  if Assigned(fLocationsTable) then
    FreeAndNil(fLocationsTable);
  if Assigned(fPhotoTableSource) then
    FreeAndNil(fPhotoTableSource);
  if Assigned(frmDeletionConfirmation) then
    FreeAndNil(frmDeletionConfirmation);
  FreeAndNil(fFoldersList);
  inherited;
end;

procedure TPhotoTable.DoAfterClose;
begin
  inherited;
  if not (optNoFilePathsTable in Options) then
    fFilePathsTable.Active  := false;

  if not (optNoCopyRightsTable in Options) then
    fCopyRightsTable.Active := false;

  if Assigned(fLocationsTable) then
    fLocationsTable.Active  := false;

  fldFILE_NAME := nil;
  fldKEY_WORDS := nil;
  fldPATH_NO   := nil;
  fldMEDIA_LENGTH := nil;
  fldFILE_TYPE := nil;
  fldUPDATED   := nil;
  fldADDED     := nil;
  fldYear      := nil;
  fldMonth     := nil;
  fldDay       := nil;
  fldPhotoDate := nil;
  fldPhotoDateTime := nil;
  fldUpdateReason := nil;
  fldFile_Size := nil;
  fldCOPYR_ID  := nil;
  fldPRIVATE   := nil;
  fldHasSound  := nil;
  fldLocationID   := nil;
  fldLatitude     := nil;
  fldLongitude    := nil;
  fldCopyRightTable_CopyID  := nil;
  fldFileNamesTable_CopyID  := nil;
  fldOwner                  := nil;
  fldTextInFile     := nil;
  fldCUSTOM_KEY     := nil;

  tblFilePathsPATH_NO   := nil;
  tblFilePathsFILE_PATH := nil;
//tblFilePathsKEY_WORD  := nil;
end;

procedure TPhotoTable.UpdatePhotoDate;
var
  HasChanged: boolean;
  Temp, OldDateString: string;
  YYYY, MM, DD, HH, Min, SS, MSec: word;
  PhotoDateNew, PhotoDateOld: TDateTime;
begin
  OldDateString := fldPhotoDate.AsString;
  YYYY := 0; MM := 0; DD := 0;
  PhotoDateToYearMonthDay(OldDateString, YYYY, MM, DD);
  HasChanged    := ((fldYear.AsInteger <> 0) and (YYYY <> fldYear.AsInteger)) or
                   ((fldMonth.AsInteger <> 0) and (MM <> fldMonth.AsInteger)) or
                   ((fldDay.AsInteger <> 0) and (DD <> fldDay.AsInteger));
  if Empty(fldPhotoDate.AsString) or HasChanged then
    begin
      temp := CalcPhotoDateString(fldYear.AsInteger, fldMonth.AsInteger, fldDay.AsInteger, HasChanged);
      fldPhotoDate.AsString := temp;
    end else
  if (fldYear.AsInteger = 0) and (fldMonth.AsInteger = 0) and (fldDay.AsInteger = 0) then
    fldPhotoDate.Clear;

  try
//  PhotoDateNew := BAD_DATE;
//  if (fldYear.AsInteger <> 0) and (fldMonth.AsInteger <> 0) and (fldDay.AsInteger <> 0) then
      PhotoDateNew := EncodeDate(fldYear.AsInteger, fldMonth.AsInteger, fldDay.AsInteger);
//  DecodeDateTime(fldPhotoDateTime.AsDateTime, YYYY, MM, DD, HH, Min, SS, MSec);
    if (fldPhotoDateTime.IsNull or (fldPhotoDateTime.AsDateTime = 0)) and (PhotoDateNew <> BAD_DATE) then
      fldPhotoDateTime.AsDateTime := PhotoDateNew
    else
      begin
        DecodeDateTime(fldPhotoDateTime.AsDateTime, YYYY, MM, DD, HH, Min, SS, MSec);
        PhotoDateOld := Trunc(fldPhotoDateTime.AsDateTime);   // if the date has changed (ignore the time)
        if PhotoDateOld <> PhotoDateNew then
          begin
            PhotoDateNew := EncodeDateTime( fldYear.AsInteger,  // use currently stored date
                                            fldMonth.AsInteger,
                                            fldDay.AsInteger,
                                            HH, Min, SS, 0);    // but old time
            fldPhotoDateTime.AsDateTime := PhotoDateNew;
          end;
      end;
  except
    on e:Exception do
      fldPhotoDateTime.Clear;
  end;
end;


procedure TPhotoTable.DoAfterOpen;
begin
  inherited;
  fTotalRecordCount := - 1;

//  InitFieldPtrs;
end;

procedure TPhotoTable.DoBeforeClose;
begin
  Filtered := false;
  OnFilterRecord := nil;

  inherited;
end;

procedure TPhotoTable.DoBeforeOpen;
begin
  inherited;
  if not (optNoFilePathsTable in fOptions) then
    fFilePathsTable.Active    := true;

  if not (optNoCopyRightsTable in fOptions) then
    fCopyRightsTable.Active   := true;
end;

function TPhotoTable.PathAndFileName: string;
begin
  if Assigned(tblFilePathsFILE_PATH) then
    begin
      result := FullFilePath + fldFILE_NAME.AsString;
      if Pos('\\', result) > 1 then
        raise Exception.CreateFmt('System error in PathAndFileName: %s', [result]);
    end
  else
    result := '';
end;

function FixPath(const Path: string): string;
begin
  result := ForceBackSlash(gRootPath + Path);
//  result := ForceBackSlash(RemoveTrailingBackSlash(gRootPath) + Path); // 10/11/2017 was get double \ after gRootPath
end;

procedure Init_NoiseWords(NoiseWordsTable: TLookupsTable);
  var
    i, j: integer; temp: string;
begin
  if Assigned(gNoiseWords) then  // perhaps table has been changed since last use
    FreeAndNil(gNoiseWords);

  gNoiseWords := TStringList.Create;
  with NoiseWordsTable do
    begin
      First;
      while not Eof do
        begin
          gNoiseWords.Add(fldLookupName.AsString);
          Next;
        end;
    end;

  for i := 0 to gNoiseWords.Count-2 do
    for j := i+1 to gNoiseWords.Count-1 do
      if gNoiseWords[i] > gNoiseWords[j] then
        begin
          temp           := gNoiseWords[i];
          gNoiseWords[i] := gNoiseWords[j];
          gNoiseWords[j] := temp;
        end;
end;

//*****************************************************************************
//   Function Name     : TPhotoTable.LocateRecord
//   Function Purpose  : Locate record with specified file name and path
//   Assumptions       :
//   Parameters        : aFileName (Like: 'PC100001.jpg')
//                     : aFolderPath = should include the full path string
//   Return Value      : True if record found
//*******************************************************************************}

function TPhotoTable.MyLocateRecord(aFileName, aFolderPath: string): boolean;
var
  mode: TSearch_Type;
  Temp: string;
begin
  try
    aFolderPath     := RemoveTrailingBackSlash(RemoveRootString(aFolderPath));
    IndexFieldNames := FILE_NAME;
    if Locate(FILE_NAME, aFileName, [loCaseInsensitive]) then
      begin
        mode := SEARCHING;
        while mode = SEARCHING do
          begin
            if Eof then
              mode := NOT_FOUND else
            if AnsiCompareText(aFileName, fldFILE_NAME.AsString) <> 0 then
              mode := NOT_FOUND // identical names will be grouped
            else
              begin
                if TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []) then
                  begin
                    Temp := TempFilePathsTable.fldFILE_PATH.AsString;
                    if SameText(aFolderPath, Temp) then
                      mode := SEARCH_FOUND
                    else
                      Next;   // phototable record
                  end;
              end;
          end;
        result := mode = SEARCH_FOUND;
      end
    else
      result := false;
  except
    on e: Exception do
      raise Exception.CreateFmt('Exception while locating "%s\%s"' + CRLF + '%s.',
                                [aFolderPath, aFileName, e.Message]);
//  on e:Exception do
//    result := false;
  end;
end;

function TPhotoTable.DateIsAGuess: boolean;
const
  cDateIsAGuessString = 'date is a guess';
  cDateIsAGuessString2 = 'unsure of date';

  function FileNameHasTilde(const FileName: string): boolean;
  var
    PartialDate: string;
    CharIdx, Idx: integer;
  begin { FileNameHasTilde }
    result := false;
    if FileNameContainsPartialDate(FileName, PartialDate, [dk_From_FileName], CharIdx) then  // 12/8/2016
      begin
        idx := Length(PartialDate) + CharIdx;
        if Idx <= Length(FileName) then
          if FileName[Idx] in ['~', 's', 'S'] then
            result := true;
      end;
  end;  { FileNameHasTilde }

  function UnsureNote(const DateIsAGuessString: string): boolean;
  begin
    result := (Pos(DateIsAGuessString, LowerCase(fldKey_Words.AsString)) > 0) or
              (Pos(DateIsAGuessString, LowerCase(fldComment.AsString)) > 0);
  end;


begin { DateIsAGuess }
  result := UnsureNote(cDateIsAGuessString) or
            UnsureNote(cDateIsAGuessString2) or
            (FileNameHasTilde(fldFile_Name.AsString));
end;  { DateIsAGuess }

procedure TPhotoTable.UpdateProc(    UpdateInfo: TUpdateInfo;
                                     LocationInfo: TLocationInfo;
                                 var KeyWords: string;
                                     anUpdateStatusProc: TUpdateStatusProc);
  const
    Delims = ' ,\-.()';
    LATLONDELIMS = ', ';
  var
    FolderNo: integer;
    Temp, TempKeyWords: string;
    aYear, aMonth, aDay: word;
    PathKeyWords, FileBase, Comments, CustomKey: string;
    LocID, lb, rb, eq: integer;
    LatLonStr, LatStr, LonStr: string;
    Latitude, Longitude: double;
    Added: integer;
    Ext: string;
    mt: TMediaType;
    MediaLengthS: string;
    MediaFrames, MediaLengthL, MediaWidth, MediaHeight: longint;
    DateTime: TDateTime;

  function GetPathKeyWords(aFilePath: string; FileNameInfo: TFileNameInfo): string;
  var
    i, wc: integer;
    aPhrase: string;
  begin { GetPathKeyWords }
    result := '';
    if FileNameInfo.IncludeFilePath then
      begin
        wc     := WordCountL(aFilePath, '\');
        for i := 1 to wc do
          begin
            aPhrase := ExtractWordL(i, aFilePath, '\');
            aPhrase := StripNoiseWords(aPhrase, FileNameInfo);
            if (not Empty(aPhrase)) then
              if result = '' then
                result := aPhrase
              else
                result := result + '; ' + aPhrase;
          end;
      end;
  end;  { GetPathKeyWords }

  function GetLocationDescription(const LocationInfo: TLocationInfo): string;
  var
    temp: string;
    idx, Len: cardinal;

    function RemoveLeadingDate(const temp: string): string;
    var
      word1: string;
      len: Cardinal;
    begin
      word1  := ExtractWordL(1, temp, DELIMS);
      if IsPureNumeric(word1) and (Length(Word1) = 8) then   // assume that the word is a date in the format yyyymmdd and just ignore it
        begin
          len    := Length(temp);
          result := Copy(temp, 9, Len-8);
        end
      else
        result := temp;
    end;

  begin { GetLocationDescription }
    case LocationInfo.DefaultLocationKind of
      dl_LatLong:
        begin
          result := CalcLocationStringLong(Latitude, Longitude);
          if not fLocationKindIsOk then
            begin
              fLocationKindIsOk := Yes('DefaultLocationKind is set to dl_LatLon. OK to proceed?');
              if not fLocationKindIsOk then
                raise Exception.Create('Location kind is set to dl_LatLon');
            end;
        end;

      dl_LastWord:
        begin
          temp   := ExtractFileName(RemoveTrailingBackSlash(ExtractFilePath(UpdateInfo.ImagePathName)));
          result := RemoveLeadingDate(temp);
        end;

      dl_SpecifiedValue:
        begin
          idx := pos('%p', LocationInfo.DefaultLocation);
          if idx > 0 then
            begin
              temp   := ExtractFileName(RemoveTrailingBackSlash(ExtractFilePath(UpdateInfo.ImagePathName)));
              temp   := RemoveLeadingDate(temp);
              len    := Length(LocationInfo.DefaultLocation);
              result := Copy(LocationInfo.DefaultLocation, 1, idx-1) + temp + Copy(LocationInfo.DefaultLocation, idx+2, Len - idx);
            end
          else
            result := LocationInfo.DefaultLocation;
        end;
    end;
  end;  { GetLocationDescription }

  function GetLocationIDFromEXIF( const Lfn: string;  var Latitude, Longitude: double): integer;
  var
    Dummy: integer;
    DummyDesc: string;
  begin { GetLocationIDFromEXIF }
    result := 0;
    if GetLatitudeLongitudeFromEXIF(Lfn, Latitude, Longitude) then
      begin
        LocationInfo.LocationSource  := ls_PhotoEXIF;
        LocationInfo.DefaultLocation := GetLocationDescription(LocationInfo);

        result      := LocationsTable.FindLocationID( Latitude, Longitude,
                                                      LocationInfo,
                                                      dummy,
                                                      DummyDesc);
      end;
  end;  { GetLocationIDFromEXIF }

  function GetLocationIDFromGPXLogs( var Latitude, Longitude: double;
                                         UpdateInfo: TUpdateInfo;
                                         LocationInfo: TLocationInfo;
                                         UpdateStatusProc: TUpdateStatusProc): integer;
  var
    Dummy: integer;
    Desc: string;
  begin { GetLocationIDFromGPXLogs }
    result := -1;
    with CommonPhotoSettings do
      begin
        CreateGPSTrackingInfo(DefaultGPXFilter, SavedTrackDataLfn, UpdateStatusProc);

        if gGPSTrackingInfo.GetLatitudeLongitudeFromGPXLogs(
                              Latitude, Longitude,
                              UpdateInfo,
                              LocationInfo) then
          begin
            result      := LocationsTable.FindLocationID( Latitude, Longitude,
                                                          LocationInfo,
                                                          Dummy,
                                                          Desc);
          end;
      end;
  end;  { GetLocationIDFromGPXLogs }

begin { UpdateProc }
  self.fOnUpdateStatus := AnUpdateStatusProc;

  if UpdateInfo.RecordExists then
    begin
      Assert(CompareText(UpdateInfo.FileName, fldFILE_NAME.AsString) = 0,
             'System error: Filename mismatch in Updateproc');
      Edit
    end
  else
    Append;

  fldFILE_NAME.AsString   := UpdateInfo.FileName;
  FolderNo                := TempFilePathsTable.FindFolderNo(UpdateInfo.FilePath,
                                                             PathKeyWords,
                                                             true);
  fldPATH_NO.AsInteger    := FolderNo;
  if not UpdateInfo.RecordExists then
    begin
      try
        Temp         := RemoveRootString(UpdateInfo.FilePath);
        TempKeyWords := GetPathKeyWords(Temp, UpdateInfo.FileNameInfo);
        if not Empty(TempKeyWords) then
          PathKeyWords := TempKeyWords;
        FileBase     := ExtractFileBase(UpdateInfo.FileName);

        with UpdateInfo.FileNameInfo do
          begin
(*
            up := Pos('_', FileBase);
            if up > 0 then  // remove trailing "_0000n" from filename
              begin
                Contents := Copy(FileBase, up+1, Length(FileBase)-up);
                if IsPureNumeric(Contents) then
                  FileBase := Copy(FileBase, 1, up-1)
                else
                  FileBase[up] := ' ';  // "_" can cause parsing problems
              end;
*)
            CustomKey := '';
            if ExtractCustomKey then    // remove leading custom key and move to db field
              begin
                eq := Pos('=', FileBase);
                if eq > 0 then
                  begin
                    CustomKey := Copy(FileBase, 1, eq-1);
                    FileBase  := Copy(FileBase, eq+1, Length(FileBase)-eq);
                  end;
              end;

            if ExtractComments then  // remove [comments] from BaseName (KeyWords) and move to comments field
              begin
                lb       := Pos('[', FileBase);
                if lb > 0 then
                  begin
                    rb := Pos(']', FileBase);
                    if rb = 0 then // ] is missing
                      raise EMyUpdateError.CreateFmt('"[" does not have matching "]" in filename'+#13#10+' %s',
                                                     [UpdateInfo.FileName]);
                    if rb > lb then
                      begin
                        Comments := Trim(Copy(FileBase, lb+1, rb-lb-1));
                        FileBase := Trim(Copy(FileBase, 1, lb-1)) + ';' + Copy(FileBase, rb+1, Length(FileBase)-rb);
                      end
                    else
                      raise EMyUpdateError.CreateFmt('"[" does not have matching "]" in filename'+#13#10+' %s',
                                                     [UpdateInfo.FileName]);
                  end;
              end;

            if LocationInfo.ExtractLatLonFromFileName then  // remove {Lat Lon} from BaseName (KeyWords) and move to latitude, longitude, location_id field
              begin
                Assert(true, 'Extract {Lat, Lon} code has not been tested');
                lb       := Pos('{', FileBase);
                if lb > 0 then
                  begin
                    rb := Pos('}', FileBase);
                    if rb = 0 then // ] is missing
                      raise EMyUpdateError.CreateFmt('"{" does not have matching "}" in filename'+#13#10+'%s',
                                                     [UpdateInfo.FileName]);
                    if rb > lb then
                      begin
                        LatLonStr := Trim(Copy(FileBase, lb+1, rb-lb-1));
                        LatStr    := ExtractWordL(1, LatLonStr, LATLONDELIMS);
                        Latitude  := StrToFloat(LatStr);
                        LonStr    := ExtractWordL(2, LatLonStr, LATLONDELIMS);
                        Longitude := StrToFloat(LonStr);
                        FileBase  := Trim(Copy(FileBase, 1, lb-1)) + ';' + Copy(FileBase, rb+1, Length(FileBase)-rb);
                        fldLatitude.AsFloat     := Latitude;
                        fldLongitude.AsFloat    := Longitude;

                        LocationInfo.LocationSource := ls_FileName;
                        LocID     := LocationsTable.FindLocationID( Latitude,
                                                                    Longitude,
                                                                    LocationInfo,
                                                                    Added,       // number that were added
                                                                    Temp);   // description for this location
                        if LocID > 0 then
                          begin
                            fldLocationId.AsInteger     := LocID;
                          end;
                      end
                    else
                      raise EMyUpdateError.CreateFmt('"{" does not have matching "}" in filename'+#13#10+': %s',
                                                      [UpdateInfo.FileName]);
                  end;
              end;

            if IncludeFilePath and IncludeFileName then // Both path and FileBase
              KeyWords := PathKeyWords + '; ' + FileBase else
            if IncludeFilePath then
              KeyWords := PathKeyWords else
            if IncludeFileName then
              KeyWords := StripNoiseWords(FileBase, UpdateInfo.FileNameInfo);
            if Empty(KeyWords) then
              KeyWords := LocationInfo.DefaultKeyWords;
          end;

        if not Empty(KeyWords) then
          fldKEY_WORDS.AsString := KeyWords
        else
          if not Empty(PathKeyWords) then
            fldKEY_WORDS.AsString := PathKeyWords  // got nothing better, use the path
          else
            fldKEY_WORDS.AsString := Temp;

        Ext                     := MyExtractFileExt(UpdateInfo.ImagePathName);
        fldFILE_TYPE.AsString   := Ext;

        mt                      := MediaTypeFromExtension(Ext);

        if IsVideoMedia(mt) or IsAudioMedia(mt) then
          begin
            if GetMediaProperties( UpdateInfo.ImagePathName,
                                   MediaFrames,
                                   MediaLengthL,
                                   MediaWidth,
                                   MediaHeight,
                                   MediaLengthS,
                                   DateTime) then
              begin
                if not Empty(MediaLengthS) then
                  fldMEDIA_LENGTH.AsString := MediaLengthS;

                fldHeight.AsInteger := MediaHeight;
                fldWidth.AsInteger  := MediaWidth;
              end;
          end;

        fldFile_Size.AsFloat    := FileSize64(UpdateInfo.ImagePathName);
        fldHasSound.AsBoolean   := UpdateInfo.HasSoundFile;
        LocID                   := -1;
        with UpdateInfo.FileNameInfo do
          begin
            if dk_From_EXIF in DateKinds then
              begin
                LocationInfo.LocationSource := ls_PhotoEXIF;
                LocID := GetLocationIDFromEXIF(UpdateInfo.ImagePathName, Latitude, Longitude);
                if LocID > 0 then
                  begin
                    fldLocationID.AsInteger := LocID;
                    fldLatitude.AsFloat     := Latitude;
                    fldLongitude.AsFloat    := Longitude;
                  end;
              end;

            if (LocID <= 0) and
               (LocationInfo.UseGPXLogsForLocation or LocationInfo.UseSavedTracksLogFile) and
               (UpdateInfo.PhotoDateTime > 0) then
              begin
                LocationInfo.LocationSource := ls_GPSLogs;
                LocID := GetLocationIDFromGPXLogs( Latitude, Longitude,
                                                   UpdateInfo,
                                                   LocationInfo,
                                                   fOnUpdateStatus);
                if LocID > 0 then  // the location is OK
                  begin
                    fldLocationID.AsInteger := LocID;
                    fldLatitude.AsFloat     := Latitude;
                    fldLongitude.AsFloat    := Longitude;
                  end;
              end;
          end;

        if UpdateInfo.PixelWidth > 0 then
          fldWidth.AsInteger      := UpdateInfo.PixelWidth;
        if UpdateInfo.PixelHeight > 0 then
          fldHeight.AsInteger     := UpdateInfo.PixelHeight;
        if not UpdateInfo.RecordExists then
          begin
            Inhibited := true;
            try
              fldADDED.AsDateTime   := Now;

              if {(not Empty(UpdateInfo.PhotoDateYYYYMMDD)) and} (UpdateInfo.PhotoDateTime > 0) then
                fldPhotoDateTime.AsDateTime := UpdateInfo.PhotoDateTime
                                                + (UpdateInfo.MinutesToAdd * ONE_MINUTE);

              if not Empty(UpdateInfo.PhotoDateYYYYMMDD) then
                begin
                  aYear := 0; aMonth := 0; aDay := 0;
                  PhotoDateToYearMonthDay(UpdateInfo.PhotoDateYYYYMMDD, aYear, aMonth, aDay);
                  fldPhotoDate.AsString := UpdateInfo.PhotoDateYYYYMMDD;

                  if aYear > 0 then
                    fldYear.AsInteger     := aYear;

                  if aMonth > 0 then
                    fldMonth.AsInteger    := aMonth;

                  if aDay > 0 then
                    fldDay.AsInteger      := aDay;
                end;

              fldCOPYR_ID.AsString  := UpdateInfo.PhotoOwner;
              fldPRIVATE.AsBoolean  := UpdateInfo.isPrivate; // 9/9/2019 - Always obey UpdateInfo settings
            finally
              Inhibited := false;
            end;
          end;

        if not Empty(Comments) then
          fldComment.AsString := Comments;

        if (not Empty(CustomKey)) and Assigned(fldCustom_Key) then
          fldCustom_Key.AsString := CustomKey;

        if Assigned(fldDateIsAGuess) then
          fldDateIsAGuess.AsBoolean := DateIsAGuess;

{$IfNDef debugging}
        if UpdateInfo.UpdateIt then
          Post
        else
{$EndIf}
          Cancel;

      except
        on e:Exception do
          begin
            Cancel;
            raise;
          end;
      end;
    end;
end;  { UpdateProc }

function TPhotoTable.DeletePhotoRecord(var WasEof: boolean): boolean;
var
  CurRecord, NextRecord: TBookmark;
begin
  CurRecord := GetBookMark;
  WasEof    := Eof;
  if not WasEof then
    Begin
      Next;
      NextRecord := GetBookMark;  // Cannot do a 'Next' after deleting a record
      GotoBookMark(CurRecord);
    end;
  try
    Delete;
    GotoBookMark(NextRecord);
    result := true;
  except
    result := false;
  end;
end;

procedure TPhotoTable.SetOrder(anOrder: TCurrentOrder);
  var
    BookMark: pointer;
    Cursor: TCursor;
begin
  fCurrentOrder   := anOrder;
  Cursor          := Screen.Cursor;
  Screen.Cursor   := crSQLWait;
  try
    BookMark        := GetBookmark;
    try
      case anOrder of
        coNone         : IndexName := '';
        coLatitudeDec  : IndexName := 'LatitudeDec';
        coLongitudeDec : IndexName := 'LongitudeDec';
        coPhotoDateTimeDesc  : IndexName := 'PhotoDateTimeDec';
        coPhotoDateDesc: IndexName := 'PhotoDateDesc';
        else
          IndexFieldNames := Order[anOrder].IndexName;
      end;

      try
        Refresh;
        GotoBookmark(BookMark);
      except
        on EDataBaseError do
          First;
        on Exception do
          raise;
      end;
    finally
      FreeBookMark(BookMark);
    end;
  finally
    Screen.Cursor := Cursor;
  end;
end;

function TPhotoTable.TotalRecordCount: Integer;
var
  aRecNo: integer;
begin
  if fTotalRecordCount > 0 then
    result := fTotalRecordCount
  else
    begin
      aRecNo := RecNo;
      try
        fTotalRecordCount := RecordCount;
        result := fTotalRecordCount;
      finally
        RecNo := aRecNo;
      end;
    end;
end;

function TPhotoTable.FullFilePath: string;
begin
  if Assigned(tblFilePathsFILE_PATH) then
    if not FilePathsTable.Eof then
      result := FixPath(tblFilePathsFILE_PATH.AsString)
    else
      result := Format('%s\%s\', [gRootPath, NONEXISTANTFOLDER])
  else
    result := '';
end;

function TPhotoTable.GetTempFilePathsTable: TFilePathsTable;
begin
  if not Assigned(fTempFilePathsTable) then
    begin
      fTempFilePathsTable := TFilePathsTable.Create(self, fDBFilePathName, cFILEPATHS, Options);
      fTempFilePathsTable.Active := true;
    end;
  result := fTempFilePathsTable;
end;

procedure TPhotoTable.DoBeforePost;
var
  LocationID: integer;
  YYYY, MM, DD: word;
begin
  inherited;

{$IfDef dhd}
  if fldKey.AsInteger = 21 then   // spring the mouse trap
    if not Yes('About to update record key = 21 ("128K Mac.jpg"). Go ahead and update? ') then
      raise Exception.Create('Update has been aborted');
{$EndIF}

  if Empty(fldFileNamesTable_CopyID.AsString) then
    fldFileNamesTable_CopyID.AsString := fCopy_ID;
  if fldWasScanned.IsNull then
    fldWasScanned.AsBoolean := fldPhotoDate.AsString < '19980701'; // assume that it was scanned if the photodate is before when I got a digital camera

  LocationID := fldLocationID.AsInteger;
  if (LocationID > 0) and (fldLatitude.IsNull or fldLongitude.IsNull) then
    begin
      if LocationID = fLastLocationID then
        begin
          fldLatitude.AsFloat    := fLastLatitude;
          fldLongitude.AsFloat   := fLastLongitude;
        end
      else
        if LocationsTable.Locate(cID, LocationID, []) then
          begin
            fLastLatitude          := LocationsTable.fldLatitude.AsFloat;
            fLastLongitude         := LocationsTable.fldLongitude.AsFloat;
            fLastLocationID        := LocationID;
            fldLatitude.AsFloat    := fLastLatitude;
            fldLongitude.AsFloat   := fLastLongitude;
          end
    end;

  if (not fldPhotoDateTime.IsNull) and (MyUtils.IsValidDate(fldPhotoDateTime.AsString)) then
    begin
      if Empty(fldPhotoDate.AsString) then
        begin
          fldPhotoDate.AsString := YYYYMMDD(fldPhotoDateTime.AsDateTime);
          DecodeDate(fldPhotoDateTime.AsDateTime, YYYY, MM, DD);
          fldYear.AsInteger  := YYYY;
          fldMonth.AsInteger := MM;
          fldDay.AsInteger   := DD;
        end;
    end;
    
  if not (optNoUpdateDate in Options) then
    begin
      if fldUpdateReason.IsNull then
        fldUpdateReason.AsInteger := integer(ur_GeneralEditing);
      if State = dsEdit then
        fldUpdated.AsDateTime := Now;  // changed 12/8/2016 to prevent photos from staying "unprocessed"
    end;
end;

function TPhotoTable.GetPhotoDay: integer;
  var
    dd: string;
begin
  result := 0;
  if Length(fldPhotoDate.AsString) >= 8 then
    begin
      dd := Copy(fldPhotoDate.AsString, 7, 2);
      try
        result := StrToInt(dd);
      except
        result := 0;
      end;
    end;
end;

function TPhotoTable.CopyRightOwner: string;
begin
  result := CopyRightsTable.fldCopyRightsOwner.AsString;
end;

function TPhotoTable.ShortFilePath: string;
begin
  if Assigned(tblFilePathsFILE_PATH) then
    if not fFilePathsTable.Eof then
      result := tblFilePathsFILE_PATH.AsString
    else
      result := Format('%s\%s\', [gRootPath, NONEXISTANTFOLDER])
  else
    result := '';
end;

procedure TPhotoTable.UpdatePhotoDateDay(sender: TField);
begin
  if not Inhibited then
    UpdatePhotoDate;
end;

procedure TPhotoTable.KeyChanged(sender: TField);       // KeyChanged
begin
//
end;


procedure TPhotoTable.UpdatePhotoDateMonth(sender: TField);
begin
  if not Inhibited then
    begin
      fldDay.Clear;
      UpdatePhotoDate;
    end;
end;

procedure TPhotoTable.UpdatePhotoDateYear(sender: TField);
begin
  if not Inhibited then
    begin
      fldMonth.Clear;
      fldDay.Clear;
      UpdatePhotoDate;
    end;
end;

function TPhotoTable.GetLocationsTable: TLocationsTable;
begin
  if not Assigned(fLocationsTable) then
    begin
      fLocationsTable   := TLocationsTable.Create(self, fDBFilePathName, cLOCATIONS, Options);
      fLocationsTable.Open;
      fLocationsTable.SelectivityParser; // force the selectivity parser to be created
      fLocationsTable.IndexFieldNames := 'LOCATION';
    end;
  result := fLocationsTable;
end;

procedure TPhotoTable.DoBeforeDelete;
var
  Temp: string;
begin
  inherited;
  if not (optNoConfirmDelete in Options) then
    begin
      Temp := PathAndFileName;
      if not Assigned(frmDeletionConfirmation) then
        frmDeletionConfirmation := TfrmDeletionConfirmation.Create(self);
      with frmDeletionConfirmation do
        begin
          FileName := temp;
          if ShowModal = mrOk then
            begin
              if cbDeleteFile.Checked then
                begin
                  fSavedFileName := temp;
                  fDeleteFilePending := true;
                end;
              if not cbDeleteTheRecord.Checked then // don't delete the record?
                SysUtils.Abort;
            end
          else
            Sysutils.Abort;  // user changed mind about deleting
        end;
    end;
end;

procedure TPhotoTable.DoAfterScroll;
begin
  inherited;

  // if file delete is pending, do it now
  if fDeleteFilePending then
    if (not Empty(fSavedFileName)) and FileExists(fSavedFileName) then
      begin
        if not MyDeleteFile(fSavedFileName, true, true) then
          AlertFmt('Unable to delete file: %s', [fSavedFileName]);
      end
    else
      AlertFmt('File does not exist: %s', [fSavedFileName]);
  fDeleteFilePending := false;
  fSavedFileName := '';
  if Assigned(fScenesTable) then
    fScenesTable.fLastDateTime   := fldPhotoDate.AsString;
end;

procedure TPhotoTable.AddOptionalFunctionsToParser(aParser: TParser);
begin
  inherited;
  with aParser do
    begin
      InitFunc(funcExtra,   'EXIFinfo',        ftString,  1, 1,  symStrRef,   symUnknown, symUnknown, symUnknown, 'EXIFInfo([EXIFKey])', GetEXIFInfo);
      InitFunc(funcExtra,   'HasEXIF',         ftBoolean, 0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetHasEXIF);
      InitFunc(funcExtra,   'FilePath',        ftString,  0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetFilePath );
      InitFunc(funcExtra,   'Latitude',        ftFloat,   0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetLatitude);
      InitFunc(funcExtra,   'Longitude',       ftFloat,   0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetLongitude);
      InitFunc(funcExtra,   'LocationSource',  ftInteger, 0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetLocationSource);
      InitFunc(funcExtra,   'LocationState',   ftString,  0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetLocationState);
      InitFunc(funcExtra,   'DistanceToLocationID',
                                               ftFloat,   2, 2,  symIntRef,   symIntRef, symUnknown,  symUnknown, 'DistanceToLocationID([LocationID], [iUnits{1=muFeet,3=muMiles}])', GetDistanceToLocationID);
      InitFunc(funcExtra,   'LocationDescription',
                                               ftString,  0, 0,  symUnknown,  symUnknown, symUnknown, symUnknown, '', GetLocationDescription);
{$IfDef dhd}
      InitFunc(funcExtra,   'InBoundingRectangle',
                                               ftBoolean, 3, 3,  symStrRef,  symFloatRef, symFloatRef, symUnknown, 'InBoundingRectangle([Abbrev],[Latitude],[Longitude])', GetInBoundingRectangle);
{$endIf}
      InitFunc(funcExtra,   'PathAndFileName', ftString,  0, 0,  symUnknown, symUnknown, symUnknown, symUnknown, '',  GetPathAndFileName);
      InitFunc(funcExtra,   'ThumbNailPathAndFileName',
                                               ftString,  0, 0,  symUnknown, symUnknown, symUnknown, symUnknown, '',  GetThumbNailPathAndFileName);
      InitFunc(funcExtra,   'FileExistsInFolder', ftBoolean, 3, 3, symStrRef,  symUnknown, symUnknown, symUnknown, 'FileExistsInFolder([FileName, WildName, InclSubFolders])', GetFileExistsInFolder);
      InitFunc(funcExtra,   'ThumbNailExists', ftBoolean, 0, 0,  symUnknown, symUnknown, symUnknown, symUnknown, '',  GetThumbNailExists);
      InitFunc(funcExtra,   'MediaLengthInSeconds',
                                               ftFloat,   0, 0,  symUnknown, symUnknown, symUnknown, symUnknown, 'MediaLengthInSeconds()', GetMediaTypeInSeconds);
    end;
end;

procedure TPhotoTable.GetEXIFInfo(Eval_Tree: TEval_Tree);
var
  ImgData: TImgData;
  Temp, TagName: string;
  TagEntry: TTagEntry;
begin
  with Eval_Tree do
    begin
      AsString := '';
      if Arg_Count = 1 then
        begin
          ImgData := TImgData.Create;
          try
            if TempFilePathsTable.Locate('Path_No', fldPATH_NO.AsInteger, []) then
              begin
                temp := FixPath(TempFilePathsTable.fldFILE_PATH.AsString) + fldFILE_NAME.AsString;
                if ImgData.ProcessFile(temp) then
                  if ImgData.HasEXIF then
                    begin
                      TagName  := Arg[1].AsString;
{$IfNDef dExif2}   // Use the old version of dExif
                      TagEntry := ImgData.ExifObj.Data[TagName];
{$else}
                      TagEntry := ImgData.ExifObj.TagByName[TagName];
{$EndIf dExif2}
                      if TagEntry.TType <> 0 then
                        AsString := TagEntry.Data;
                    end;
              end;
          finally
            ImgData.Free;
          end;
        end;
    end;
end;

procedure TPhotoTable.GetHasEXIF(Eval_Tree: TEval_Tree);
var
  ImgData: TImgData;
  Temp: string;
begin
  with Eval_Tree do
    begin
      AsBoolean := false;
      if Arg_Count = 0 then
        begin
          ImgData := TImgData.Create;
          try
            if TempFilePathsTable.Locate('Path_No', fldPATH_NO.AsInteger, []) then
              begin
                temp := FixPath(TempFilePathsTable.fldFILE_PATH.AsString) + fldFILE_NAME.AsString;
                if ImgData.ProcessFile(temp) then
                  AsBoolean := ImgData.HasEXIF;
              end;
          finally
            ImgData.Free;
          end;
        end;
    end;
end;


procedure TPhotoTable.GetFilePath(Eval_Tree: TEval_Tree);
var
  temp: string;
begin
  with Eval_Tree do
    begin
      if TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []) then
        begin
          Temp := ForceBackSlash(TempFilePathsTable.fldFILE_PATH.AsString) + fldFILE_NAME.AsString;
          AsString := temp;
        end
      else
        AsString := ''
    end;
end;

procedure TPhotoTable.GetLatitude(Eval_Tree: TEval_Tree);
begin
  Eval_Tree.AsFloat := GetLatitude;
end;

procedure TPhotoTable.GetLongitude(Eval_Tree: TEval_Tree);
begin
  Eval_Tree.AsFloat := GetLongitude;
end;

procedure TPhotoTable.GetLocationSource(Eval_Tree: TEval_Tree);
begin
  with Eval_Tree do
    begin
      if not (fldLocationID.IsNull or (fldLocationID.AsInteger <= 0)) then
        AsInteger := LocationsTable.LocationSource(fldLocationId.AsInteger)
      else
        AsInteger := 0;
    end;
end;

procedure TPhotoTable.GetLocationState(Eval_Tree: TEval_Tree);
begin
  with Eval_Tree do
    begin
      if not (fldLocationID.IsNull or (fldLocationID.AsInteger <= 0)) then
        AsString := LocationsTable.LocationState(fldLocationId.AsInteger)
      else
        AsString := '';
    end;
end;

procedure TPhotoTable.GetDistanceToLocationID(Eval_Tree: TEval_Tree);
var
  mu: TMeasurementUnits;
  Lat2, Lon2: double;
begin
  with Eval_Tree do
    begin
      if not (fldLocationID.IsNull or (fldLocationID.AsInteger <= 0)) then
        with LocationsTable do
          begin
            if not HasLocation then
              if Arg[1].Operator = symUnsNum then  // then this is a constant and will not change when reevaluated
                HasLocation := GetLatLon(Arg[1].AsInteger, fLat, fLon);

            if HasLocation or GetLatLon(Arg[1].AsInteger, fLat, fLon) then
              begin
                if Arg_Count >= 2 then
                  mu := TMeasurementUnits(Arg[2].AsInteger)
                  // 0=muInches, 1=muFeet, 2=muYards, 3=muMiles, 4=muMillimeters, 5=muCentimeters, 6=muMeters, 7=muKilometers
                else
                  mu := muFeet;

                if fldLocationID.AsInteger = LastLocationID then  // same record as before - skip the Locate()
                  begin
                    Lat2 := fldLatitude.AsFloat;
                    Lon2 := fldLongitude.AsFloat;
                  end else
                if GetLatLon(fldLocationID.AsInteger, Lat2, Lon2) then
                  begin
                    AsFloat := Distance(fLat, fLon,
                                      Lat2,
                                      Lon2,
                                      mu);
                    LastLocationID := fldLocationID.AsInteger;
                  end
                else
                  AsFloat := MAXINT;
              end
            else
              AsFloat := MAXINT;
          end
      else
        AsFloat := MAXINT;
    end;
end;

procedure TPhotoTable.GetLocationDescription(Eval_Tree: TEval_Tree);
var
  OK: boolean;
begin
  with Eval_Tree do
    begin
      if not (fldLocationID.IsNull or (fldLocationID.AsInteger <= 0)) then
        begin
          if fldLocationID.AsInteger = LastLocationID then
            OK := true
          else
            begin
              OK := LocationsTable.Locate(cID, fldLocationID.AsInteger, []);
              if OK then
                LastLocationID := fldLocationID.AsInteger;
            end;

          if OK then
            AsString := LocationsTable.fldDescription.AsString
          else
            AsString := '';
        end
      else
        AsString := '';
    end;
end;

procedure TPhotoTable.GetInBoundingRectangle(Eval_Tree: TEval_Tree);
var
  Abbrev: string;
  Lat2, Lon2: double;
  BoundariesTable: TBoundariesTable;
begin
  with Eval_Tree do
    begin
      Abbrev  := Arg[1].AsString;
      Lat2    := Arg[2].AsFloat;
      Lon2    := Arg[3].AsFloat;
      if not SameText(Abbrev, fAbbrev) then // avoid opening the Boundaries table when not necessary
        begin
          BoundariesTable := (Table as TPhotoTable).BoundariesTable as TBoundariesTable;
          if BoundariesTable.Locate('ABBREV', Abbrev, []) then
            with fBoundingRectangle, BoundariesTable do
              begin
                Top     := fldULTop.AsFloat;
                Left    := fldULLeft.AsFloat;
                Bottom  := fldBRBottom.AsFloat;
                Right   := fldBRRight.AsFloat;
                fAbbrev := Abbrev;
              end
          else
            raise ERecordNotFound.CreateFmt('Bounding rectangle "%s" not found in Boundaries Table', [Abbrev]);
        end;
      if (Lat2 <> 0) and (Lon2 <> 0) then
        with fBoundingRectangle do
          AsBoolean := (Left <= Lon2) and (Lon2 <= Right) and (Top >= Lat2) and (Bottom <= Lat2)
      else
        AsBoolean := false;
    end;
end;

procedure TPhotoTable.GetPathAndFileName(Eval_Tree: TEval_Tree);
begin
  if TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []) then
    Eval_Tree.AsString := FixPath(TempFilePathsTable.fldFILE_PATH.AsString) + fldFILE_NAME.AsString
  else
    Eval_Tree.AsString := '';
end;

procedure TPhotoTable.GetMediaTypeInSeconds(Eval_Tree: TEval_Tree);
begin
  try
    if not Empty(fldMEDIA_LENGTH.AsString) then
      Eval_Tree.AsFloat := FrameToSeconds(fldMEDIA_LENGTH.AsString)
    else
      Eval_Tree.AsFloat := 0;
  except
    Eval_Tree.AsFloat := 0;
  end;
end;

procedure TPhotoTable.GetThumbNailPathAndFileName(Eval_Tree: TEval_Tree);
var
  FileName, ThumbNailName: string;
begin
  if TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []) then
    begin
      FileName := FixPath(TempFilePathsTable.fldFILE_PATH.AsString) + fldFILE_NAME.AsString;
      ThumbNailName := ExtractFilePath(FileName) + 'tn\tn_' + ExtractFileName(FileName);
      Eval_Tree.AsString := ThumbNailName
    end
  else
    Eval_Tree.AsString := '';
end;

procedure TPhotoTable.GetThumbNailExists(Eval_Tree: TEval_Tree); // this seems to be VERY slow
var
  FileName, ThumbNailName: string;
begin
  if TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []) then
    begin
      FileName := FixPath(TempFilePathsTable.fldFILE_PATH.AsString) + fldFILE_NAME.AsString;
      ThumbNailName := ExtractFilePath(FileName) + 'tn\tn_' + ExtractFileName(FileName);
      Eval_Tree.AsBoolean := SysUtils.FileExists(ThumbNailName);
    end
  else
    Eval_Tree.AsBoolean := false;
end;


procedure TPhotoTable.GetFileExistsInFolder(Eval_Tree: TEval_Tree);
var
  FileName, FolderPath, PathAndName: string;
  IncludeSubFolders: boolean;

  procedure AddFilesInTree(const FolderPath: string);
  var
    SearchRec: TSearchRec;
    Err: integer;
    PathAndName: string;
  begin { AddFilesInTree }
    // Add all the files in this folder first
    Err := SysUtils.FindFirst(FolderPath + '*.*', faAnyFile - faDirectory, SearchRec);
    while (err = 0) do
      begin
        fFoldersList.Add(SearchRec.Name);

        Err := SysUtils.FindNext(SearchRec);
      end;

    // now add filenames in all of the sub-folders
    Err := SysUtils.FindFirst(FolderPath + '*.*', faDirectory, SearchRec);
    while (Err = 0) do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           ((SearchRec.Attr and faDirectory) = faDirectory) then
          begin
            PathAndName := ForceBackSlash(FolderPath + SearchRec.Name);
            AddFilesInTree(PathAndName);
          end;
        Err := SysUtils.FindNext(SearchRec)
      end;
    FindClose(SearchRec);
  end;  { AddFilesInTree }

begin { GetFileExistsInFolder }
  FileName    := Eval_Tree.Arg[1].AsString;
  FolderPath  := ForceBackSlash(Eval_Tree.Arg[2].AsString);
  IncludeSubFolders := Eval_Tree.Arg[3].AsBoolean;
  if IncludeSubFolders then
    begin
      if (not Assigned(fFoldersList)) or (not SameText(FolderPath, fFolderName)) then
        begin
          FreeAndNil(fFoldersList);
          fFoldersList := TStringList.Create;
          AddFilesInTree(FolderPath);
          fFoldersList.Sorted := true;
          fFolderName := FolderPath;
        end;
      Eval_Tree.AsBoolean := fFoldersList.IndexOf(FileName) >= 0;
    end
  else
    begin
      PathAndName := FolderPath + FileName;
      Eval_Tree.AsBoolean := FileExists(PathAndName);
    end;
end;  { GetFileExistsInFolder }



procedure TPhotoTable.DoAfterInsert;
begin
  inherited;
  Inc(fTotalRecordCount);
end;

procedure TPhotoTable.DoAfterDelete;
begin
  inherited;
  Dec(fTotalRecordCount);
end;

function TPhotoTable.GetLatitude: double;
begin
  if not fldLatitude.IsNull then
    result := fldLatitude.AsFloat else
  if not (fldLocationID.IsNull or (fldLocationID.AsInteger <= 0)) then
    if fldLocationID.AsInteger = fLastLocationID then
      result := fLastLatitude
    else
      begin
        if LocationsTable.GetLatLon(fldLocationId.AsInteger, fLastLatitude, fLastLongitude) then
          fLastLocationID := fldLocationID.AsInteger;
        result := fLastLatitude;
      end
  else
    result := 0.0;
end;

function TPhotoTable.GetLongitude: double;
begin
  if not fldLongitude.IsNull then
    result := fldLongitude.AsFloat else
  if not (fldLocationID.IsNull or (fldLocationID.AsInteger <= 0)) then
    if fldLocationID.AsInteger = fLastLocationID then
      result := fLastLongitude
    else
      begin
        if LocationsTable.GetLatLon(fldLocationId.AsInteger, fLastLatitude, fLastLongitude) then
          fLastLocationID := fldLocationID.AsInteger;
        result := fLastLongitude;
      end
  else
    result := 0.0;
end;

function TPhotoTable.GetBoundariesTable: TDataSet;
begin
  if Assigned(fOnGetBoundariesTable) then  
    result := fOnGetBoundariesTable
  else
    raise Exception.Create('System Error: Boundaries table cannot be defined');
end;

function TPhotoTable.ScenesTable: TScenesTable;
begin
  if not Assigned(fScenesTable) then
    begin
      fScenesTable := TScenesTable.Create( self,
                                         CommonPhotoSettings.PhotoDBDatabaseFileName,
                                         cSCENES,
                                         [optUseClient]);
      fScenesTable.IndexFieldNames := 'FileName_Key;StartInSeconds';
      fScenesTable.fLastDateTime   := fldPhotoDate.AsString;
      fScenesTable.Tag := 1; //
      fScenesTable.Active          := true;
    end;
  result := fScenesTable;
end;


function TPhotoTable.GetScenesTableInUse: boolean;
begin
  result := Assigned(fScenesTable);
end;

procedure TPhotoTable.SetScenesTableInUse(const Value: boolean);
begin
  if Value then
    ScenesTable
  else
    FreeAndNil(fScenesTable);
end;

procedure TPhotoTable.MyDisableControls;
begin
  inherited;
  if Assigned(fScenesTable) then
    fScenesTable.DisableControls;
end;

procedure TPhotoTable.MyEnableControls;
begin
  inherited;
  if Assigned(fScenesTable) then
    fScenesTable.EnableControls;
end;

procedure TPhotoTable.ClearParser;
begin
  inherited;
  FreeAndNil(fFoldersList);
  fFolderName := '';
end;

procedure TPhotoTable.SetOnGetBoundariesTable(
  const Value: TGetBoundariesTableFunc);
begin
  fOnGetBoundariesTable := Value;
end;

{ TCopyRightsTable }

procedure TCopyRightsTable.DoAfterOpen;
begin
  inherited;

  fldCopyRightsOwner := FieldByName(cOWNER);
  fldCopyR_ID        := FieldByName(cCOPYR_ID);
end;

{ TFilePathsTable }

constructor TFilePathsTable.Create( aOwner: TComponent;
                                    aDBFilePathName, aTableName: string;
                                    Options: TPhotoTableOptions);
begin
  inherited;
end;

procedure TFilePathsTable.InitFieldPtrs;
begin
  inherited;
  fldPATH_NO   := FieldByName(PATH_NO);
  fldPARENT_NO := FieldByName(PARENT_NO);
  fldFILE_PATH := FieldByName(FILE_PATH);
  fldKEY_WORD  := FieldByName(KEY_WORD);
  fldSQL_KEY   := FieldByName(SQL_KEY);
  fldUPDATED   := FieldByName(DATE_UPDATED);
end;


procedure TFilePathsTable.DoAfterOpen;
begin
  inherited;
//InitFieldPtrs;
end;

procedure TFilePathsTable.DoBeforePost;
begin
  inherited;
  if not (optNoUpdateDate in Options) then
    fldUpdated.AsDateTime := Now;
end;

//*****************************************************************************
//   Function Name     : TFilePathsTable.FindFolderNo
//   Useage            :
//   Function Purpose  :
//   Assumptions       :
//   Parameters        :
//   Return Value      :
//*******************************************************************************}

function TFilePathsTable.FindFolderNo( aFolderPath: string;
                                       var KeyWords: string;
                                       OkToAdd: boolean;
                                       AllowPartialKey: boolean = false): integer;
const
  DELIMS = ' _-';
var
  RelFolderPath: string;
  ParentPathNO: integer;
begin
  result := 0;

  RelFolderPath := RemoveTrailingBackSlash(RemoveRootString(aFolderPath));

  if not Empty(RelFolderPath) then
    begin
      if Locate(FILE_PATH, RelFolderPath, [loCaseInsensitive]) then
        begin
          result   := FieldByName('PATH_NO').AsInteger;
          KeyWords := FieldByName('KEY_WORD').AsString;
        end
      else
        if OkToAdd then
          begin
            ParentPathNO := FindParentNo(aFolderPath, OkToAdd);  // make sure that parent folder has a record
            Append;
            FieldByName(FILE_PATH).AsString  := RelFolderPath;
            result   := CommonPhotoSettings.FolderNo;
            CommonPhotoSettings.FolderNo     := CommonPhotoSettings.FolderNo + 1;
            FieldByName(PATH_NO).AsInteger   := result;
            FieldByName(PARENT_NO).AsInteger := ParentPathNO;
            KeyWords := '';
            KeyWords := ExtractFileName(aFolderPath);
            FieldByName(KEY_WORD).AsString := KeyWords;
            try
              Post;
            except
              on e:Exception do
                begin
                  AlertFmt('Error when trying to append a new FilePaths record. [%s] Try re-scanning for highest key value',
                         [e.Message]);
                  raise;
                end;
            end;
          end;
    end;
end;

//*****************************************************************************
//   Function Name     : TFilePathsTable.FindParentNo
//   Useage            : FindParentNo('', true)
//   Function Purpose  : Find the parent's PATH_NO for a given FilePath
//   Assumptions       :
//   Parameters        : aFolderPath: FILE_PATH value of the record whose parent is to be found (full physical path)
//                       OkToAdd: true is it is OK to add record(s)
//   Return Value      : PATH_NO of the targeted record
//*******************************************************************************}

function TFilePathsTable.FindParentNo(aFolderPath: string;
  OkToAdd: boolean): integer;
var
  ParentFolderPath, KeyWords: string;
begin
  result   := -1;
  if FindFolderNo(aFolderPath, KeyWords, false) > 0 then // we found the child
    if (not fldParent_No.IsNull) and (not fldParent_No.AsInteger <= 0) then // and it already has the Parent_No stored
      result := fldParent_No.AsInteger;

  if result = -1 then // the child doesn't exist yet
    begin
      ParentFolderPath := ExtractFilePath(RemoveTrailingBackSlash(aFolderPath));  // calculate the path to the parent
      result           := FindFolderNo(ParentFolderPath, KeyWords, OkToAdd);      // find/add the parent
      FindFolderNo(aFolderPath, KeyWords, false);  // go back to the child record, if any
    end;
end;

procedure TFilePathsTable.LocateUnusedPathNo;
begin
  Active := true;
  Last;
  repeat
    if fldPath_NO.AsInteger >= CommonPhotoSettings.FolderNo then
      CommonPhotoSettings.FolderNo := fldPath_NO.AsInteger + 1;
    Next;
  until Eof;
end;


procedure TFilePathsTable.DoAfterScroll;
begin
  inherited;

end;

{ TPhotoTableQuery }

constructor TPhotoTableQuery.Create(aOwner: TComponent);
begin
  inherited;
//ConnectionString  := Format(c_CnStr, [CommonPhotoSettings.PhotoDBDatabaseFileName]);
  Connection        := MyConnection(CommonPhotoSettings.PhotoDBDatabaseFileName);
end;

procedure TPhotoTableQuery.DoAfterOpen;
begin
  inherited;

  fldFILE_NAME      := FieldByName(FILE_NAME);
  fldKEY_WORDS      := FieldByName(KEY_WORDS);
  fldPATH_NO        := FieldByName('Path_NO');
  fldFILE_TYPE      := FieldByName(FILE_TYPE);
  fldMEDIA_LENGTH   := FieldByName(MEDIA_LENGTH);
  fldUPDATED        := FieldByName(DATE_UPDATED);
  fldADDED          := FieldByName(DATE_ADDED);
  fldYear           := FieldByName('Year');
  fldMonth          := FieldByName('Month');
  fldDay            := FieldByName('Day');
  fldPhotoDate      := FieldByName('PhotoDate');
  fldPhotoDateTime  := FieldByName('PhotoDateTime');
  fldFile_Size      := FieldByName('FILE_SIZE');
  fldCOPYR_ID       := FieldByName('COPYR_ID');
  fldPRIVATE        := FieldByName('Private');
  fldHasSound       := FieldByName('HasSound');
  fldLocationID     := FieldByName('LocationID');
  fldLatitude       := FieldByName('Latitude');
  fldLongitude      := FieldByName('Longitude');
  fldTextInFile     := FindField('TextInFile');
  fldRating         := FieldByName('Rating');
  fldKey            := FieldByName(CKEY);
  fldWidth          := FieldByName('Width');
  fldHeight         := FieldByName('Height');
end;

{ TLocationsTable }

(*  this slows things way down
procedure TLocationsTable.UpdateCalculatedFields(Dataset: TDataSet);
var
  fldLongitude: TField;
  Zone: integer;
begin
  if Active then
    begin
      fldLongitude := FieldByName('Longitude');
      if not fldLongitude.IsNull then
        begin
          Zone := Round(fldLongitude.AsFloat * 24 / 360);
          FieldByName(TIMEZONE_OFFSET).AsInteger := Zone;
          case Zone of
            -4: FieldByName(TIMEZONE).AsString := 'AST';
            -5: FieldByName(TIMEZONE).AsString := 'EST';
            -6: FieldByName(TIMEZONE).AsString := 'CST';
            -7: FieldByName(TIMEZONE).AsString := 'MST';
            -8: FieldByName(TIMEZONE).AsString := 'PST';
            -9: FieldByName(TIMEZONE).AsString := 'Alaska';
            -10: FieldByName(TIMEZONE).AsString := 'Hawaii';
            -11: FieldByName(TIMEZONE).AsString := 'Samoa';
            else
              FieldByName(TIMEZONE).AsString   := Format('Zone = %d', [Zone]);
          end;
        end;
    end;
end;
*)


constructor TLocationsTable.Create(aOwner: TComponent; aDBFilePathName, aTableName: string;
  Options: TPhotoTableOptions);
begin
  inherited;
//  OnCalcFields := UpdateCalculatedFields;

//  inherited Create(aOwner, aTableName, Options);
end;

procedure TLocationsTable.InitFieldPtrs;
begin
  inherited;
  fldID              := FieldByName(cID);
  fldLatitude        := FieldByName('Latitude');
  fldLongitude       := FieldByName('Longitude');
  fldLatLongKey      := FieldByName('Location');
  fldDescription     := FieldByName('Description');
  fldDateAdded       := FieldByName('DateAdded');
  fldDateUpdated     := FieldByName('DateUpdated');
  fldLocationSource  := FieldByName('LocationSource');
  fldPrivateLocation := FieldByName('PrivateLocation');
  fldRefCnt          := FieldByName('RefCnt');
  fldState           := FieldByName('State');
end;


procedure TLocationsTable.DoAfterOpen;
begin
  inherited;
// InitFieldPtrs;
end;

procedure TLocationsTable.DoBeforePost;
var
  Latitude, Longitude: double;
begin
  inherited;
  Latitude             := fldLatitude.AsFloat;
  Longitude            := fldLongitude.AsFloat;
  fldLatLongKey.AsString := CalcLocationString(Latitude, Longitude);

  if not (optNoUpdateDate in self.Options) then
    begin
      if fldDateAdded.IsNull then
        fldDateAdded.AsDateTime := Now;
      fldDateUpdated.AsDateTime := Now;
    end;
end;

procedure TLocationsTable.InitLocationInfo(var LocationInfo: TLocationInfo;
                               LocationSource: TLocationSource = ls_Unknown;
                               DefaultLocation: string = '';
                               DefaultState: string = '';
                               MaxDistanceInFeet: integer = MAXDISTANCEINFEET;
                               OkToAdd: boolean = true;
                               AlwaysAdd: boolean = false;
                               Simulate: boolean = false);
begin
  LocationInfo.MaxDistanceInFeet := MaxDistanceInFeet;
  LocationInfo.DefaultLocation   := DefaultLocation;
  LocationInfo.DefaultState      := DefaultState;
  LocationInfo.OkToAdd           := OkToAdd;
  LocationInfo.AlwaysAdd         := AlwaysAdd;
  LocationInfo.LocationSource    := LocationSource;
  LocationInfo.Simulate          := Simulate;
end;

function TLocationsTable.FindLocationID( Latitude, Longitude: double;
                                         LocationInfo: TLocationInfo;
                                         var Added: integer;
                                         var FoundDesc: string): integer;
var
  LatLongKey: string;
  Dist, ShortestDistInFeet: double;
  IDOfShortest: integer;
  idx: integer;
  temp: string;
  year, month, day: word;
begin
  ShortestDistInFeet := MAXINT;
  IDOfShortest := -1;
  result       := 0;
  FoundDesc    := LocationInfo.DefaultLocation;

  with LocationInfo do
    begin
      temp         := LeadingNumericString(DefaultLocation, idx);
      if not Empty(temp) then
        begin
          if IsYYYYMMDD(temp, year, month, day) then
            DefaultLocation := Trim(Copy(DefaultLocation, idx, Length(DefaultLocation)-idx+1));
        end;
      LatLongKey   := CalcLocationString(Latitude, Longitude);
      if not (Bof and Eof) then
        if Locate('Location', LatLongKey, [loCaseInsensitive, loPartialKey]) then
          begin
            // find the location nearest to the target

            repeat
              Dist := Distance(Latitude, Longitude, fldLatitude.AsFloat, fldLongitude.AsFloat, muFeet);
              if Dist < ShortestDistInFeet then
                begin
                  ShortestDistInFeet := Dist;
                  IDOfShortest       := fldID.AsInteger;
                  FoundDesc          := fldDescription.AsString;
                end;
              Next;
            until Eof or (LatLongKey <> fldLatLongKey.AsString) or (ShortestDistInFeet = 0);
            if ShortestDistInFeet <= MaxDistanceInFeet then
              result := IDOfShortest;
          end;

      if result <> 0 then  // found it. Make it the current record
        begin
          if not Locate(cID, result, []) then
            raise Exception.CreateFmt('System error. FindLocationID (ID=%d) was not found', [result]);
        end else
      if ((result = 0) { couldn't find it } and OkToAdd) or (OkToAdd and AlwaysAdd) then
        begin
          Append;
          fldLatitude.AsFloat         := Latitude;
          fldLongitude.AsFloat        := Longitude;
          fldLatLongKey.AsString      := LatLongKey;
          fldDescription.AsString     := DefaultLocation;
          fldState.AsString           := DefaultState;
          FoundDesc                   := DefaultLocation;
          fldLocationSource.AsInteger := ord(LocationSource);

          if not Simulate then
            begin
              Post;
              result := fldID.AsInteger;
            end
          else
            begin
              Cancel;
              result := -2;  // meaning "it would have been added"
            end;

          inc(Added);
        end;
    end;
end;

function TLocationsTable.GetLocationDescription(
  LocationID: integer): string;
begin
  if Locate(cID, LocationID, []) then
    result := fldDescription.AsString
  else
    result := '';
end;

class function TLocationsTable.HasLocationInfoInEXIF(
  const FileName: string): boolean;
var
  ImgData: TImgData;
  Latitude, Longitude: double;
begin   
  result := false;
  ImgData := TImgData.Create();

  try
    if ImgData.ProcessFile(FileName) then
      if ImgData.HasExif then
        begin
          with ImgData.ExifObj do
            begin
{$IfNDef dExif2}   // Use the old version of dExif
              Latitude    := GetDecimalDegrees(Data['GPSLatitudeRef'].Data, Data['GPSLatitude'].Data);
              Longitude   := GetDecimalDegrees(Data['GPSLongitudeRef'].Data, Data['GPSLongitude'].Data);
{$else}
              Latitude    := GetDecimalDegrees(TagByName['GPSLatitudeRef'].Data, TagByName['GPSLatitude'].Data);
              Longitude   := GetDecimalDegrees(TagByName['GPSLongitudeRef'].Data, TagByName['GPSLongitude'].Data);
{$endIf dExif2}
              result      := (Latitude <> 0) or (Longitude <> 0);
            end;
        end;
  finally
    ImgData.Free;
  end;
end;

function TLocationsTable.GetLatitude(LocationID: integer): double;
begin
  if LocationID = fLastLocationID then
    result := fLastLatitude
  else
    if Locate(cID, LocationID, []) then
      begin
        fLastLatitude          := fldLatitude.AsFloat;
        fLastLongitude         := fldLongitude.AsFloat;
        result                 := fLastLatitude;
        fLastLocationID        := LocationID;
      end
    else
      result := 0;
end;

function TLocationsTable.LocationSource(LocationID: integer): integer;
begin
  if LocationID = fLastLocationID then
    result := fLastLocationSource
  else
    if Locate(cID, LocationID, []) then
      begin
        fLastLocationSource  := fldLocationSource.AsInteger;
        result               := fLastLocationSource;
        fLastLatitude        := fldLatitude.AsFloat;
        fLastLongitude       := fldLongitude.AsFloat;
        fLastLocationID      := LocationID;
      end
    else
      result := 0;
end;

function TLocationsTable.GetLongitude(LocationID: integer): double;
begin
  if LocationID = fLastLocationID then
    result := fLastLongitude
  else
    if Locate(cID, LocationID, []) then
      begin
        fLastLatitude   := fldLatitude.AsFloat;
        fLastLongitude  := fldLongitude.AsFloat;
        result          := fLastLongitude;
        fLastLocationID := LocationID;
      end
    else
      result := 0;
end;

function TLocationsTable.GetLatLon(LocationID: integer; var Lat, Lon: double): boolean;
begin
  result := Locate(cID, LocationID, []);
  if result then
    begin
      Lat := fldLatitude.AsFloat;
      Lon := fldLongitude.AsFloat;
    end;
end;

procedure TLocationsTable.DecrementRefCnt;
begin
  if fldRefCnt.AsInteger > 0 then
    begin
      Edit;
      fldRefCnt.AsInteger := fldRefCnt.AsInteger - 1;
      if fldRefCnt.AsInteger <= 0 then
        fldRefCnt.Clear;
      Post;
    end;
end;

procedure TLocationsTable.IncrementRefCnt;
begin
  Edit;
  fldRefCnt.AsInteger := fldRefCnt.AsInteger + 1;
  Post;
end;

procedure TLocationsTable.DecrementRefCnt(aLocationID: integer);
begin
  if aLocationID > 0 then
    if Locate(cID, aLocationID, []) then
      DecrementRefCnt;
end;

procedure TLocationsTable.IncrementRefCnt(aLocationID: integer);
begin
  if aLocationID > 0 then
    if Locate(cID, aLocationID, []) then
      IncrementRefCnt;
end;

procedure TLocationsTable.AddFields;  // this slows things way down
begin
  inherited;
  AddField(self, TIMEZONE_OFFSET, TIMEZONE_OFFSET, TIntegerField,  fkCalculated, 0, 0);
  AddField(self, TIMEZONE,        TIMEZONE,        TStringField,   fkCalculated, 10, 10);
end;

function TLocationsTable.LocationState(LocationID: integer): string;
begin
  if LocationID = fldID.AsInteger then
    result := fLastLocationState
  else
    if Locate(cID, LocationID, []) then
      begin
        fLastLocationState   := fldState.AsString;
        result               := fLastLocationState;
      end
    else
      result := '';
end;

{ TMemorabiliaTable }

procedure TMemorabiliaTable.DoAfterOpen;
begin
  inherited;
  fldID            := FieldByName(cID);
  fldDateYYYYMMDD  := FieldByName('DateYYYYMMDD');
  fldObjectType    := FieldByName('Object Type');
  fldDescription   := FieldByName('Description');
  fldFilePath      := FieldByName('FilePath');
  fldFileName      := FieldByName(cFILENAME);
  fldAuthor        := FieldByName('Author');
  fldKeyWords      := FieldByName('KeyWords');
  fldDate          := FieldByName('Date');
  fldYear          := FieldByName('Year');
  fldMonth         := FieldByName('Month');
  fldDay           := FieldByName('Day');
  fldComment       := FieldByName('Comment');
end;

{ TFilePathsQuery }

constructor TFilePathsQuery.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
//  ConnectionString  := Format(c_CnStr, [CommonPhotoSettings.PhotoDBDatabaseFileName]);
  Connection        := MyConnection(CommonPhotoSettings.PhotoDBDatabaseFileName);
end;

procedure TFilePathsQuery.DoAfterOpen;
begin
  inherited;
  fldPATH_NO   := FieldByName(PATH_NO);
  fldPARENT_NO := FieldByName(PARENT_NO);
  fldFILE_PATH := FieldByName(FILE_PATH);
  fldKEY_WORD  := FieldByName(KEY_WORD);
  fldSQL_KEY   := FieldByName(SQL_KEY);
  fldUPDATED   := FieldByName(DATE_UPDATED);
end;

procedure TFilePathsQuery.DoBeforePost;
begin
  inherited;
  fldUpdated.AsDateTime := Now;
end;

procedure TLocationsTable.GetInBoundingRectangle(Eval_Tree: TEval_Tree);
var
  Abbrev: string;
  Lat2, Lon2: double;
  BoundariesTable: TBoundariesTable;
begin
  with Eval_Tree do
    begin
      Abbrev  := Arg[1].AsString;
      Lat2    := Arg[2].AsFloat;
      Lon2    := Arg[3].AsFloat;
      if not SameText(Abbrev, fAbbrev) then // avoid opening the Boundaries table when not necessary
        begin
          BoundariesTable := BoundariesDataSet as TBoundariesTable;
          if BoundariesTable.Locate('ABBREV', Abbrev, []) then
            with fBoundingRectangle, BoundariesTable do
              begin
                Top     := fldULTop.AsFloat;
                Left    := fldULLeft.AsFloat;
                Bottom  := fldBRBottom.AsFloat;
                Right   := fldBRRight.AsFloat;
                fAbbrev := Abbrev;
              end
          else
            raise ERecordNotFound.CreateFmt('Bounding rectangle "%s" not found in Boundaries Table', [Abbrev]);
        end;
      if (Lat2 <> 0) and (Lon2 <> 0) then
        with fBoundingRectangle do
          AsBoolean := (Left <= Lon2) and (Lon2 <= Right) and (Top >= Lat2) and (Bottom <= Lat2)
      else
        AsBoolean := false;
    end;
end;


function TLocationsTable.BoundariesDataSet: TDataSet;
begin
  if not Assigned(fBoundariesDataSet) then
    begin
      fBoundariesDataSet := TBoundariesTable.Create(nil, BOUNDING_RECTANGLES, [optReadOnly]);
      fBoundariesDataSet.Active := true;
    end;
  result := fBoundariesDataSet;
end;

procedure TLocationsTable.AddOptionalFunctionsToParser(aParser: TParser);
begin
  inherited;
  with aParser {was fParser as of 10/3/2016} do
    begin
      InitFunc(funcExtra,
               'InBoundingRectangle',
               ftBoolean, 3, 3,  symStrRef,  symFloatRef, symFloatRef, symUnknown,
               'InBoundingRectangle(Abbrev,Latitude,Longitude)',
               GetInBoundingRectangle);
    end;
end;

function ScanForHighestFolderNumber(Starting_Number: integer; const DatabaseFileName: string): integer;
var
  aFilePathsTable: TFilePathsTable;
  tblFilePathsPATH_NO: TField;
  Path_No: integer;
begin { ScanForHighestFolderNumber }
  aFilePathsTable := TFilePathsTable.Create(nil, DatabaseFileName, cFILEPATHS, []);
  result  := Starting_Number;
  try
    with aFilePathsTable do
      begin
        Active := true;
        tblFilePathsPATH_NO := FieldByName('PATH_NO');
        First;
        while not Eof do
          begin
            Path_No := tblFilePathsPATH_NO.AsInteger;
            if Path_No >= result then
              result := Path_No + 1;
            Next;
          end;
      end;
  finally
    aFilePathsTable.Active := false;
    aFilePathsTable.Free;
  end;
end;  { ScanForHighestFolderNumber }

{ TScenesTable }

procedure TScenesTable.DoAfterInsert;
var
  PhotoTable: TPhotoTable;
begin
  inherited;
  PhotoTable := Owner as TPhotoTable;
  fldFILENAME_KEY.AsInteger := PhotoTable.fldKey.AsInteger;  // set a link to the parent PhotoTable record
  fldPhotoDate.AsString     := fLastDateTime;
  PhotoTable.Edit;
  PhotoTable.fldHasSceneInfo.AsBoolean := true;  // mark this record as having scene info
end;

procedure TScenesTable.DoAfterOpen;
begin
  inherited;

end;


procedure TScenesTable.DoAfterPost;
begin
  inherited;
  if not FieldIsEmpty(fldPhotoDate) then
    fLastDateTime := fldPhotoDate.AsString;
end;

function TScenesTable.FieldIsEmpty(Field: TField; ZeroIsEmpty: boolean = false): boolean;
begin
  result := Field.IsNull or (Field.AsString = '');
  if not result then
    if ZeroIsEmpty then
      begin
        try
          result := Field.AsInteger = 0;
        except
          result := false;
        end
      end;
end;

procedure TScenesTable.DoAfterScroll;
begin
//
end;

procedure TScenesTable.DoBeforePost;
var
  Year, Month, Day: word;
  TheDate: TDateTime;

begin
  inherited;
  if fldAdded.IsNull then
    fldAdded.AsDateTime := Now
  else
    fldUpdated.AsDateTime := Now;

  if FieldIsEmpty(fldStartInSeconds, true) and (not FieldIsEmpty(fldStartFrame)) then
    fldStartInSeconds.AsFloat := FrameToSeconds(fldStartFrame.AsString)
  else
    if (not FieldIsEmpty(fldStartInSeconds, true)) and (FieldIsEmpty(fldStartFrame)) then
      fldStartFrame.AsString := SecondsToFrame(fldStartInSeconds.AsFloat);

  if FieldIsEmpty(fldLengthInSeconds, true) and (not FieldIsEmpty(fldLengthInFrames)) then
    begin
      fldLengthInSeconds.AsFloat := FrameToSeconds(fldLengthInFrames.AsString);
      fldLengthInFrames.AsString := SecondsToFrame(fldLengthInSeconds.AsFloat);  // normalize it
    end
  else
    if (not FieldIsEmpty(fldLengthInSeconds, true)) and (FieldIsEmpty(fldLengthInFrames)) then
      fldLengthInFrames.AsString := SecondsToFrame(fldLengthInSeconds.AsFloat);

  if FieldIsEmpty(fldPhotoDate) and (not FieldIsEmpty(fldPhotoDateTime)) then
    begin
      DecodeDate(fldPhotoDateTime.AsDateTime, Year, Month, Day);
      fldPhotoDate.AsString := YearMonthDayToPhotoDate(Year, Month, Day);
    end else
  if FieldIsEmpty(fldPhotoDateTime) and (not FieldIsEmpty(fldPhotoDate)) and (Length(fldPhotoDate.AsString) = 8) then
    begin
      TheDate := PhotoDate2DateTime(fldPhotoDate.AsString, dd_NoDate);
      if TheDate <> BAD_DATE then
        fldPhotoDateTime.AsDateTime := TheDate;
    end;

  if not FieldIsEmpty(fldPhotoDate) then
    begin
      PhotoDateToYearMonthDay(fldPhotoDate.AsString, Year, Month, Day);
      if FieldIsEmpty(fldYear) and (Year > 0) then
        fldYear.AsInteger := Year;
      if FieldIsEmpty(fldMonth) and (month > 0) then
        fldMonth.AsInteger := month;
      if FieldIsEmpty(fldDay) and (day > 0) then
        fldDay.AsInteger := day;
    end;
end;

procedure TScenesTable.InitFieldPtrs;
begin
  inherited;
  fldID               := FieldByNAme(cID);
  fldFILENAME_KEY     := FieldByName(FILENAME_KEY);
  fldKEY_WORDS        := FieldByName(KEY_WORDS);
  fldADDED            := FieldByName(DATE_ADDED);
  fldUPDATED          := FieldByName(DATE_UPDATED);
  fldYear             := FieldByName('Year');
  fldMonth            := FieldByName('Month');
  fldDay              := FieldByName('Day');
  fldPhotoDate        := FieldByName(PHOTODATE);
  fldPhotoDateTime    := FieldByName('PhotoDateTime');
  fldComment          := FieldByName('Comment');
  fldStartInSeconds   := FieldByName(STARTINSECONDS);
  fldStartFrame       := FieldByName('StartFrame');
  fldLengthInFrames   := FieldByName('LengthInFrames');
  fldLengthInSeconds  := FieldByName('LengthInSeconds');
  fldAlsoSee          := FieldByName(cALSOSEE);
end;

destructor TLocationsTable.Destroy;
begin

  inherited;
end;

procedure TLocationsTable.DoAfterScroll;
begin
  inherited;
end;

initialization
finalization
  FreeAndNil(gNoiseWords);
end.
