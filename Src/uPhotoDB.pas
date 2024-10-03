// Program was compiled with Delphi 7.0 (Build 8.1)
//
// Libraries used:
//         Orpheus Version 4.06 Release 1.11 (https://sourceforge.net/projects/tporpheus/)
//         Turbo Power Systools Version 4.01 library (available on SourceForge)
//         Drag and Drop Component Suite Version 5.2
//              Released 17-aug-2010
//              © 1997-2010 Anders Melander
//              http://melander.dk/delphi/dragdrop/
//         FastCode libxl-3.2.4 Charalabos Michael <chmichael@creationpower.com>
//         FastMM  http://fastmm.sourceforge.net copyright Professional Software Development / Pierre le Riche
//         XMLPartner from TurboPower https://sourceforge.net/projects/tpxmlpartner/
//         GPX - see the GPXEditor version 1.3.83 https://sourceforge.net/projects/gpxeditor/
//         RotateImages (possible source?) https://codebus.net/d-9gfX.html

//  The Access Database Engine is automatically installed when MS Access is installed.
//  However, you may need to install the 32-bit Access Database Engine:
//  https://download.microsoft.com/download/2/4/3/24375141-E08D-4803-AB0E-10F2E3A07AAA/AccessDatabaseEngine.exe

unit uPhotoDB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, DBCtrls, StdCtrls, Db, Mask, ADODB, DBTables, Grids,
  DBGrids, ComCtrls, PDBTables, Jpeg, StBase, PathBrowser, PDBUtils,
  ActnList, PDB_Decl, ParseExpr, Synonyms, ovcbase, ovcef, ovcpb, ovcnf,
  DragDropFile, GPSTrackingInfoUnit, FillLocationInfo,
  LocationsBrowser, dEXIF, CheckLst, DragDrop, DropTarget,
  Expression, RecordAudio, ScanForDuplicatesOptions,
{$IfDef dhd}
  HikingTables,
  BoundariesBrowser,
  TracksBrowser,
{$EndIf}
  BrowserUnit, BrowseScenes, ovcdbnf, Buttons,
  CopySelectedRecords, GenerateHTML2, LookupsTableUnit, RotImg;

const
// 61 dhd 11/21/2013 Jumped version to 1.61
// 32 dhd 10/18/2012 Implemented Thumbnails Tab Sheet
// 31 dhd 02/14/2012 Implemented ThumbNails for videos
// 30 dhd 09/12/2011 Added Distance(), Latitude() and Longitude() functions
// 29 dhd 07/29/2011 remove need for wild cards
// 28 dhd 07/28/2011 additional work on location data
// 27 dhd 07/11/2011 Stored location data
// 26 dhd 05/12/2011 Create Browser for copyright table
// 25 dhd 01/03/2011 Ability to drag-drop a replacement photo
// 24 dhd 12/30/2010 Ability to position by key value
// 23 dhd 12/21/2010 Implement the FilePaths() function
// 22 dhd 12/20/2010 Implemented the ability to move a file from one folder to another
// 21 dhd 12/10/2010 Include photo width and height in DB
// 20 dhd 11/24/2010 Ability to automatically rate photos
// 19 dhd 11/23/2010 use default values for PhotoDateToYearMonthDay
// 18 dhd 10/22/2010 Fill in PhotoDateTIME when filling from EXIF
// 17 dhd 10/15/2010 Ability to scan for duplicate file sizes
// 16 dhd 10/13/2010 Added Year(), Month() and Day() functions
//                   Added ability to fill PhotoDateTime from PhotoDate
//                   Fixed selectivity in fill PhotoDateTime from Exif
//                   Added ability to sort on date added and date updated
// 15 dhd 09/20/2010 Ability to select fields in report
// 14 dhd 08/05/2010 Wasn't changing internal field pointers when referencing a different dataset
// 13 dhd 07/29/2010 Added ability to create shortcuts rather than copy files
// 12 dhd 07/21/2010 Added duplicate sizes report
// 11 dhd 02/09/2010 Made changes to better handle video files
// 10 dhd 01/11/2010 Implemented ability to use expression parser
// 09 dhd 11/30/2009 Fixes for slideshow & studio
// 08 dhd 09/28/2009 Changes to scan for duplicates: auto-open NotePad.exe, etc
// 06 dhd 03/19/2009 Full screen and slideshow
// 05 dhd 11/06/2008 Disable ability to change file name or path
// 04 dhd 10/30/2008 Use of PhotoDate string rather than TDateTime
// 03 dhd 04/21/2008 Added ability to copy selected records
// 02 dhd 10/15/2007 Added ability to filter on file paths
// 01 dhd 04/04/2006 Allow Shift, and Ctrl-Click insertions

// Source Code Sources:

// DragDropFile:                 The Drag and Drop Component Suite for Delphi; Anders Melander, anders@melander.dk, http://melander.dk
// DropTarget:                   The Drag and Drop Component Suite for Delphi; Anders Melander, anders@melander.dk, http://melander.dk
// GPX.pas                       GPS_Editor 1.6.16 (SourceForge)
// GPSFile.pas                   (ditto)
// GPS.pas                       (ditto)
// XML Partner                   (ditto)
// FastCode                      (ditto)
// FastMM                        SourceForge
// SMExport suite  (optional)    http://www.scalabium.com

  cVersion = '2.00';
  cCopyright = 'Copyright (c) 2023, R && D Systems';

  MAXKEY  = 10;

  KEYWORDDELIMS = ', -._[];()?:''"@#$%^&*+={}|\<>/'#13#10;

  MIN_STRSIMILARITY = 65;

type
  TUpdateProc = procedure {name} (UpdateInfo: TUpdateInfo; var KeyWords: string) of object;
  TMoveProc   = procedure of object;

  TShiftMode = (smNone, smShift, smCtrl);

  TLastSearchKind = (sk_KeyWord, sk_String);

  TPhotoList = class(Tlist)
  public
    Destructor Destroy; override;
  end;

  TChangeReason = (cr_ExpressionChanged, cr_SynonymChange, cr_MediaClassChange,
                   cr_ParameterChanged, cr_FilePathNoChange, cr_FilterChanged,
                   cr_HasSceneInfoChanged);

  TSetOfChangeReasons = set of TChangeReason;

  EOperatorAbort = class(Exception);

  TfrmPhotoDataBase = class(TForm)
    MainMenu1: TMainMenu;
    miMoveFile: TMenuItem;
    Edit2: TMenuItem;
    Exit1: TMenuItem;
    PageControl1: TPageControl;
    tabBrowse: TTabSheet;
    tabPhoto: TTabSheet;
    dsPhotoTable: TDataSource;
    dsFilePaths: TDataSource;
    Navigate1: TMenuItem;
    First1: TMenuItem;
    Prev1: TMenuItem;
    Next1: TMenuItem;
    Last1: TMenuItem;
    MarkNext1: TMenuItem;
    Utilities1: TMenuItem;
    BuildInitialDB1: TMenuItem;
    InsertText1: TMenuItem;
    InsertText2: TMenuItem;
    F10: TMenuItem;
    F9: TMenuItem;
    F8: TMenuItem;
    F7: TMenuItem;
    F6: TMenuItem;
    F5: TMenuItem;
    F4: TMenuItem;
    F3: TMenuItem;
    F2: TMenuItem;
    F1: TMenuItem;
    LastKeyWords1: TMenuItem;
    SaveDialog1: TSaveDialog;
    RecalcKeyWords1: TMenuItem;
    CountSelectedRecords1: TMenuItem;
    N2: TMenuItem;
    Order1: TMenuItem;
    FileName1: TMenuItem;
    PathName1: TMenuItem;
    PhotoDate1: TMenuItem;
    puImage: TPopupMenu;
    FileSize1: TMenuItem;
    dsCopyRight: TDataSource;
    SaveAs1: TMenuItem;
    Help1: TMenuItem;
    AboutPhotoDB1: TMenuItem;
    InsertShiftText1: TMenuItem;
    InsertCtrlText1: TMenuItem;
    ShiftF1: TMenuItem;
    ShiftF2: TMenuItem;
    ShiftF3: TMenuItem;
    ShiftF4: TMenuItem;
    ShiftF5: TMenuItem;
    ShiftF6: TMenuItem;
    ShiftF7: TMenuItem;
    ShiftF8: TMenuItem;
    ShiftF9: TMenuItem;
    ShiftF10: TMenuItem;
    CtrlF1: TMenuItem;
    CtrlF2: TMenuItem;
    CtrlF3: TMenuItem;
    CtrlF4: TMenuItem;
    CtrlF5: TMenuItem;
    CtrlF6: TMenuItem;
    CtrlF7: TMenuItem;
    CtrlF8: TMenuItem;
    CtrlF9: TMenuItem;
    CtrlF10: TMenuItem;
    N3: TMenuItem;
    PrintKeyAssignments1: TMenuItem;
    AddKeyWordToSelectedPhotos1: TMenuItem;
    CopySelectedRecords1: TMenuItem;
    PhotoEditor1: TMenuItem;
    UseFullSizeWindow1: TMenuItem;
    puKeyWords: TPopupMenu;
    CaptureText1: TMenuItem;
    capF1: TMenuItem;
    capF2: TMenuItem;
    capF3: TMenuItem;
    capF4: TMenuItem;
    capF5: TMenuItem;
    capF6: TMenuItem;
    capF7: TMenuItem;
    capF8: TMenuItem;
    capF9: TMenuItem;
    capF10: TMenuItem;
    Panel2: TPanel;
    pnlPhoto: TPanel;
    Panel1: TPanel;
    Label4: TLabel;
    edtRootPath: TEdit;
    lblFilePath: TLabel;
    dbFilePath: TDBEdit;
    Label1: TLabel;
    dbFileName: TDBEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label18: TLabel;
    dbYear: TDBEdit;
    dbMonth: TDBEdit;
    dbPhotoDate: TDBEdit;
    Label13: TLabel;
    Label23: TLabel;
    dbUpdated: TDBEdit;
    dbAdded: TDBEdit;
    Label16: TLabel;
    Label20: TLabel;
    dbCopyCode: TDBEdit;
    cbPrivate: TDBCheckBox;
    Label2: TLabel;
    dbKeyWords: TDBMemo;
    Label19: TLabel;
    dbComments: TDBMemo;
    cbUseFullSizeWindow: TCheckBox;
    imgPhoto: TRotateImage;
    DBRadioGroup1: TDBRadioGroup;
    Panel3: TPanel;
    Label21: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label22: TLabel;
    lblLowYear: TLabel;
    lblHighYear: TLabel;
    Label12: TLabel;
    edHighMonth: TLabel;
    Label9: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    edtStringInPath: TEdit;
    edtFileNames: TEdit;
    edtLowDate: TEdit;
    edtHighDate: TEdit;
    cbDateType: TComboBox;
    edtLowYear: TEdit;
    edtHighYear: TEdit;
    edtLowMonth: TEdit;
    edtHighMonth: TEdit;
    edtKeyWords: TEdit;
    cbMatchWholeWordsOnly: TCheckBox;
    cbHasSound: TCheckBox;
    edtCopyCode: TMaskEdit;
    cbNot: TCheckBox;
    cbUnprocessedOnly: TCheckBox;
    btnApply: TButton;
    btnClear: TButton;
    Panel4: TPanel;
    dbGrid1: TDBGrid;
    RecNo1: TMenuItem;
    Delete1: TMenuItem;
    Reports1: TMenuItem;
    ScanforMissingFiles1: TMenuItem;
    ScanforMissngFilesandUpdate1: TMenuItem;
    ScanforDuplicates1: TMenuItem;
    ListSelectedRecords1: TMenuItem;
    DuplicateFileSizeReport1: TMenuItem;
    OpenFuncKeytxtforediting1: TMenuItem;
    Label5: TLabel;
    dbDay: TDBEdit;
    dbPhotoDateTime: TDBEdit;
    Label24: TLabel;
    Updated1: TMenuItem;
    Added1: TMenuItem;
    cbUseSynonyms: TCheckBox;
    PhotoDateTime1: TMenuItem;
    lblStatus: TLabel;
    DBNavigator1: TDBNavigator;
    dbKey: TDBText;
    Label25: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    ovcKey: TOvcNumericField;
    cbLocationID: TCheckBox;
    cbHasLocationInfoInEXIF: TCheckBox;
    Label14: TLabel;
    lblFileSize: TLabel;
    Label26: TLabel;
    DBText1: TDBText;
    Label30: TLabel;
    DBText2: TDBText;
    Label27: TLabel;
    lblLocation: TMemo;
    Browse1: TMenuItem;
    BrowseFilePaths1: TMenuItem;
    BrowseCopyrights1: TMenuItem;
    miFindKeyWord: TMenuItem;
    puLocationInfo: TPopupMenu;
    DeleteLocationInfo1: TMenuItem;
    CopyLocationInfo1: TMenuItem;
    PasteLocationInfo1: TMenuItem;
    AddLocationInfo1: TMenuItem;
    EditLocationInfo1: TMenuItem;
    N4: TMenuItem;
    CopyLatitude1: TMenuItem;
    CopyLongitude1: TMenuItem;
    CreatefromScratch1: TMenuItem;
    BasedonDateTime1: TMenuItem;
    FindLocation1: TMenuItem;
    ByDescription1: TMenuItem;
    ByLocation1: TMenuItem;
    BrowseLocations1: TMenuItem;
    FindAgain1: TMenuItem;
    dbWasScanned: TDBCheckBox;
    Label31: TLabel;
    ShowEXIFInfo1: TMenuItem;
    ReplaceTextinSelected1: TMenuItem;
    FindString1: TMenuItem;
    clbMediaClasses: TCheckListBox;
    OpenMedia: TMenuItem;
    ChangetoProperCase1: TMenuItem;
    AddPhoto1: TMenuItem;
    CopyfullFileName1: TMenuItem;
    N5: TMenuItem;
    ShowThumbnailsratherthanfullsizephoto1: TMenuItem;
    AdjustPhotoDatesTimes1: TMenuItem;
    Bynumber1: TMenuItem;
    tabMover: TTabSheet;
    AddFilepath1: TMenuItem;
    InsertLocationatCursor1: TMenuItem;
    tabThumbNails: TTabSheet;
    ScrollBox1: TScrollBox;
    puThumbNail: TPopupMenu;
    SelectThisPhoto1: TMenuItem;
    btnRefresh: TButton;
    edtMaxPhotos: TEdit;
    Label32: TLabel;
    udMaxPhotos: TUpDown;
    RenameFile1: TMenuItem;
    PhotoEditor2: TMenuItem;
    AddThumbnail1: TMenuItem;
    btnExpression: TButton;
    Ratethisphoto1: TMenuItem;
    Excellent1: TMenuItem;
    Good1: TMenuItem;
    Ok1: TMenuItem;
    Fair1: TMenuItem;
    Poor1: TMenuItem;
    lblStatus2: TLabel;
    OpenSettings1: TMenuItem;
    SaveProjectAs1: TMenuItem;
    N6: TMenuItem;
    NewProject1: TMenuItem;
    PlayAudio1: TMenuItem;
    Label3: TLabel;
    dbHasSound: TDBCheckBox;
    btnAudioRecorder: TButton;
    btnLoadFilter: TButton;
    btnSaveFilter: TButton;
    UpdateFieldinSelectedRecords1: TMenuItem;
    CreateThumbnail1: TMenuItem;
    UpdateThumbnailsforSelectedRecords1: TMenuItem;
    BrowseBoundaries1: TMenuItem;
    Label33: TLabel;
    edtFilePathNo: TEdit;
    Post1: TMenuItem;
    Fill1: TMenuItem;
    FillYearMonthDayPhotoDate1: TMenuItem;
    fromFileCreationTime1: TMenuItem;
    fromFileDate1: TMenuItem;
    FillPhotoDateTimefromPhotoDate1: TMenuItem;
    FillLocation1: TMenuItem;
    ClearLocationIDforselectedrecords1: TMenuItem;
    fromGPXdatafiles1: TMenuItem;
    FillFileSize1: TMenuItem;
    FillDateAddedfromFileDate1: TMenuItem;
    ImportPhotos1: TMenuItem;
    SameLocationasPreviousPost1: TMenuItem;
    SameKeyWordsasLastPost1: TMenuItem;
    Miscelaneous1: TMenuItem;
    DateTimeCalculation1: TMenuItem;
    DistancefromLocation1: TMenuItem;
    FillLatitudeLongitudefromLocationID1: TMenuItem;
    Latitude1: TMenuItem;
    Longitude1: TMenuItem;
    DeleteTrailingDates1: TMenuItem;
    RatePhotos1: TMenuItem;
    UpdateHeightWidthforSelectedPhotos1: TMenuItem;
    cbFileIsMissing: TCheckBox;
    FillTextInFilewithTextFromFile1: TMenuItem;
    cbDisplayTextInFile: TCheckBox;
    cbScanComments: TCheckBox;
    cbScanFile: TCheckBox;
    PopupMenu2: TPopupMenu;
    IsDHD1: TMenuItem;
    MatchSelectedFilestoFilesinFolder1: TMenuItem;
    SetFileDateTimetoPhotoDateTimeforselectedrecords1: TMenuItem;
    ScanforFilesnotinDataBase1: TMenuItem;
    GenerateHTMLforSelectedFolders1: TMenuItem;
    lblPathNo: TLabel;
    fromExifDateTime1: TMenuItem;
    DropFileTarget1: TDropFileTarget;
    cbAllowStrSimilarity: TCheckBox;
    meSS: TOvcNumericField;
    cbIncludeSubFolders: TCheckBox;
    ReplaceCurrentPhoto1: TMenuItem;
    FromEXIFInfo1: TMenuItem;
    BrowseScenes1: TMenuItem;
    cbShowScenes: TCheckBox;
    N7: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    UnDo1: TMenuItem;
    cbScanSceneKeyWords: TCheckBox;
    cbHasSceneInfo: TCheckBox;
    OvcDbNumericField1: TOvcDbNumericField;
    OvcDbNumericField2: TOvcDbNumericField;
    cbScanSceneDates: TCheckBox;
    ScanSelectedforMissingThumbnail1: TMenuItem;
    UploadSelectedFiles1: TMenuItem;
    btnFillDate: TBitBtn;
    cbDateKind: TComboBox;
    DeleteSelectedRecords1: TMenuItem;
    BrowseFileNames1: TMenuItem;
    FillLocationFromEnteredData1: TMenuItem;
    FillLocationfromEXIF1: TMenuItem;
    FillWasScannedfromEXIF1: TMenuItem;
    fromEnteredDateTime1: TMenuItem;
    btnOpenFile: TButton;
    N8: TMenuItem;
    GoToKey1: TMenuItem;
    N1: TMenuItem;
    btnLastRecord: TButton;
    BrowseTracks1: TMenuItem;
    EditThumbnail1: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    Print1: TMenuItem;
    Label34: TLabel;
    edtLowDay: TEdit;
    Label35: TLabel;
    edtHighDay: TEdit;
    FillYearMonthDay1: TMenuItem;
    FromPhotoDateTime1: TMenuItem;
    FromPhotoDate1: TMenuItem;
    BrowseLookups1: TMenuItem;
    FunctionKeys1: TMenuItem;
    Filters1: TMenuItem;
    Expressions1: TMenuItem;
    All1: TMenuItem;
    Synonyms1: TMenuItem;
    CalculateFileSize1: TMenuItem;
    N11: TMenuItem;
    EvaluateExpression2: TMenuItem;
    EditPhoto1: TMenuItem;
    DeletePhoto1: TMenuItem;
    FileDateIsAGuess1: TMenuItem;
    dbDateIsAGuess: TDBCheckBox;
    BrowseNoiseWords1: TMenuItem;
    InOrderbyLatLon1: TMenuItem;
    GenerateCSVFile1: TMenuItem;
    lblStatus1: TLabel;
    N12: TMenuItem;
    Ratethisphoto2: TMenuItem;
    Terrible1: TMenuItem;
    Poor2: TMenuItem;
    Ok2: TMenuItem;
    Good2: TMenuItem;
    Excellent2: TMenuItem;
    GenerateFilenamesList1: TMenuItem;
    ExportSelectedRecords1: TMenuItem;
    ImportRecords1: TMenuItem;
    Label37: TLabel;
    DBText3: TDBText;
    FillMediaLengthfromFiles1: TMenuItem;
    Ascending1: TMenuItem;
    Descending1: TMenuItem;
    Ascending2: TMenuItem;
    Decsending2: TMenuItem;
    Ascending3: TMenuItem;
    Descending2: TMenuItem;
    AScending4: TMenuItem;
    Descending3: TMenuItem;
    MediaLength1: TMenuItem;
    fromMediaDate1: TMenuItem;
    BrowseReportsFolder1: TMenuItem;
    dbCustom_Key: TDBEdit;
    Label38: TLabel;
    AlternatePhotoEditor1: TMenuItem;
    RotateImage1: TRotateImage;
    cbAllowRotation: TCheckBox;
    FillLocationfromLatLon1: TMenuItem;
    MoveFile1: TMenuItem;
    CustomKey1: TMenuItem;
    Refresh1: TMenuItem;
    N13: TMenuItem;
    Rotate1: TMenuItem;
    RotateRight901: TMenuItem;
    N14: TMenuItem;
    Rotate1801: TMenuItem;
    Image1: TMenuItem;
    RotateRight902: TMenuItem;
    RotateLeft901: TMenuItem;
    Rotate1802: TMenuItem;
    N15: TMenuItem;
    ShowEXIFMediaInfo1: TMenuItem;
    Options1: TMenuItem;
    RenameFilesinFoldertoDateTimeTaken1: TMenuItem;
    procedure BuildInitialDB1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure First1Click(Sender: TObject);
    procedure Prev1Click(Sender: TObject);
    procedure Next1Click(Sender: TObject);
    procedure Last1Click(Sender: TObject);
    procedure MarkNext1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FillMonthYear1Click(Sender: TObject);
    procedure F1Click(Sender: TObject);
    procedure puKeyWordsPopup(Sender: TObject);
    procedure F2Click(Sender: TObject);
    procedure F3Click(Sender: TObject);
    procedure F4Click(Sender: TObject);
    procedure F5Click(Sender: TObject);
    procedure F6Click(Sender: TObject);
    procedure F7Click(Sender: TObject);
    procedure F8Click(Sender: TObject);
    procedure F9Click(Sender: TObject);
    procedure F10Click(Sender: TObject);
    procedure capF1Click(Sender: TObject);
    procedure capF2Click(Sender: TObject);
    procedure capF3Click(Sender: TObject);
    procedure capF4Click(Sender: TObject);
    procedure capF5Click(Sender: TObject);
    procedure capF6Click(Sender: TObject);
    procedure capF7Click(Sender: TObject);
    procedure capF8Click(Sender: TObject);
    procedure capF9Click(Sender: TObject);
    procedure capF10Click(Sender: TObject);
    procedure ScanforMissingFiles1Click(Sender: TObject);
    procedure LastKeyWords1Click(Sender: TObject);
    procedure dbFileNameEnter(Sender: TObject);
    procedure tabPhotoResize(Sender: TObject);
    procedure BrowseFilePaths1Click(Sender: TObject);
    procedure RecalcKeyWords1Click(Sender: TObject);
    procedure ScanandUpdate1Click(Sender: TObject);
    procedure CountSelectedRecords1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure FileName1Click(Sender: TObject);
    procedure PathName1Click(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure FillFileSize1Click(Sender: TObject);
    procedure FileSize1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure AboutPhotoDB1Click(Sender: TObject);
    procedure imgPhotoStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure imgPhotoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgPhotoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShiftF1Click(Sender: TObject);
    procedure ShiftF2Click(Sender: TObject);
    procedure ShiftF3Click(Sender: TObject);
    procedure ShiftF4Click(Sender: TObject);
    procedure ShiftF5Click(Sender: TObject);
    procedure ShiftF6Click(Sender: TObject);
    procedure ShiftF7Click(Sender: TObject);
    procedure ShiftF8Click(Sender: TObject);
    procedure ShiftF9Click(Sender: TObject);
    procedure ShiftF10Click(Sender: TObject);
    procedure CtrlF1Click(Sender: TObject);
    procedure CtrlF2Click(Sender: TObject);
    procedure CtrlF3Click(Sender: TObject);
    procedure CtrlF4Click(Sender: TObject);
    procedure CtrlF5Click(Sender: TObject);
    procedure CtrlF6Click(Sender: TObject);
    procedure CtrlF7Click(Sender: TObject);
    procedure CtrlF8Click(Sender: TObject);
    procedure CtrlF9Click(Sender: TObject);
    procedure CtrlF10Click(Sender: TObject);
    procedure PrintKeyAssignments1Click(Sender: TObject);
    procedure SetFileDatefromEXIF1Click(Sender: TObject);
    procedure dbCopyCodeChange(Sender: TObject);
    procedure AddKeyWordToSelectedPhotos1Click(Sender: TObject);
    procedure CopySelectedRecords1Click(Sender: TObject);
    procedure DeleteTrailingDates1Click(Sender: TObject);
    procedure dbFileNameKeyPress(Sender: TObject; var Key: Char);
    procedure dbFilePathKeyPress(Sender: TObject; var Key: Char);
    procedure OpenFullSizeWindowExecute(Sender: TObject);
    procedure cbUseFullSizeWindowClick(Sender: TObject);
    procedure UseFullSizeWindow1Click(Sender: TObject);
    procedure btnExpressionClick(Sender: TObject);
    procedure cbCenterPhotoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RecNo1Click(Sender: TObject);
    procedure DuplicateFileSizeReport1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure ListSelectedRecords1Click(Sender: TObject);
    procedure ScanforMissngFilesandUpdate1Click(Sender: TObject);
    procedure ScanforDuplicates1Click(Sender: TObject);
    procedure ScanforDuplicatesandDelete1Click(Sender: TObject);
    procedure OpenFuncKeytxtforediting1Click(Sender: TObject);
    procedure Updated1Click(Sender: TObject);
    procedure Added1Click(Sender: TObject);
    procedure FillPhotoDateTimefromPhotoDate1Click(Sender: TObject);
    procedure cbUseSynonymsClick(Sender: TObject);
    procedure PhotoDateTime1Click(Sender: TObject);
    procedure ScanforDuplicateFileSizes1Click(Sender: TObject);
    procedure MoveFile1Click(Sender: TObject);
    procedure ovcKeyAfterExit(Sender: TObject);
    procedure BrowseCopyrights1Click(Sender: TObject);
    procedure FillLocationfromEXIF1Click(Sender: TObject);
    procedure FillLocationinfofromGPXdatafiles1Click(Sender: TObject);
    procedure FillLocationFromEnteredData1Click(Sender: TObject);
    procedure miFindKeyWordClick(Sender: TObject);
    procedure DeleteLocationInfo1Click(Sender: TObject);
    procedure CopyLocationInfo1Click(Sender: TObject);
    procedure PasteLocationInfo1Click(Sender: TObject);
    procedure ClearLocationIDforselectedrecords1Click(Sender: TObject);
    procedure FromPhotoDate1Click(Sender: TObject);
    procedure EditLocationInfo1Click(Sender: TObject);
    procedure CopyLatitude1Click(Sender: TObject);
    procedure CopyLongitude1Click(Sender: TObject);
    procedure CreatefromScratch1Click(Sender: TObject);
    procedure BasedonDateTime1Click(Sender: TObject);
    procedure SearchforLatLon1Click(Sender: TObject);
    procedure FindLocation1Click(Sender: TObject);
    procedure dbPhotoDateTimeExit(Sender: TObject);
    procedure ByLocation1Click(Sender: TObject);
    procedure BrowseLocations1Click(Sender: TObject);
    procedure FindAgain1Click(Sender: TObject);
    procedure FillWasScannedfromEXIF1Click(Sender: TObject);
    procedure ShowEXIFInfo1Click(Sender: TObject);
    procedure ReplaceTextinSelected1Click(Sender: TObject);
    procedure FindString1Click(Sender: TObject);
    procedure clbMediaClassesClickCheck(Sender: TObject);
    procedure puImagePopup(Sender: TObject);
    procedure ChangetoProperCase1Click(Sender: TObject);
    procedure AddPhoto1Click(Sender: TObject);
    procedure CopyfullFileName1Click(Sender: TObject);
    procedure ShowThumbnailsratherthanfullsizephoto1Click(Sender: TObject);
    procedure PhotoEditor1Click(Sender: TObject);
    procedure AdjustPhotoDatesTimes1Click(Sender: TObject);
    procedure Bynumber1Click(Sender: TObject);
    procedure AddFilepath1Click(Sender: TObject);
    procedure tabMoverResize(Sender: TObject);
    procedure dbCopyCodeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure InsertLocationatCursor1Click(Sender: TObject);
    procedure SelectThisPhoto1Click(Sender: TObject);
    procedure puThumbNailPopup(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure RenameFile1Click(Sender: TObject);
    procedure PhotoEditor2Click(Sender: TObject);
    procedure AddThumbnail1Click(Sender: TObject);
    procedure Excellent1Click(Sender: TObject);
    procedure Good1Click(Sender: TObject);
    procedure Ok1Click(Sender: TObject);
    procedure Fair1Click(Sender: TObject);
    procedure Poor1Click(Sender: TObject);
    procedure Ratethisphoto1Click(Sender: TObject);
    procedure OpenSettings1Click(Sender: TObject);
    procedure SaveProjectAs1Click(Sender: TObject);
    procedure PlayAudio1Click(Sender: TObject);
    procedure btnAudioRecorderClick(Sender: TObject);
    procedure btnSaveFilterClick(Sender: TObject);
    procedure btnLoadFilterClick(Sender: TObject);
    procedure UpdateFieldinSelectedRecords1Click(Sender: TObject);
    procedure ParameterChanged(Sender: TObject);
    procedure CreateThumbnail1Click(Sender: TObject);
    procedure UpdateThumbnailsforSelectedRecords1Click(Sender: TObject);
    procedure FillDateAddedfromFileDate1Click(Sender: TObject);
    procedure Post1Click(Sender: TObject);
    procedure edtMaxPhotosChange(Sender: TObject);
    procedure tabThumbNailsResize(Sender: TObject);
    procedure ImportPhotos1Click(Sender: TObject);
    procedure SameKeyWordsasLastPost1Click(Sender: TObject);
    procedure SameLocationasPreviousPost1Click(Sender: TObject);
    procedure DateTimeCalculation1Click(Sender: TObject);
    procedure DistancefromLocation1Click(Sender: TObject);
    procedure FillLatitudeLongitudefromLocationID1Click(Sender: TObject);
    procedure dbGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CopySelectedFiles1Click(Sender: TObject);
    procedure UpdateHeightWidthforSelectedPhotos1Click(Sender: TObject);
    procedure FillTextInFilewithTextFromFile1Click(Sender: TObject);
    procedure dbGrid1DblClick(Sender: TObject);
    procedure cbDisplayTextInFileClick(Sender: TObject);
    procedure IsDHD1Click(Sender: TObject);
    procedure MatchSelectedFilestoFilesinFolder1Click(Sender: TObject);
    procedure SetFileDateTimetoPhotoDateTimeforselectedrecords1Click(
      Sender: TObject);
    procedure ScanforFilesnotinDataBase1Click(Sender: TObject);
    procedure GenerateHTMLforSelectedFolders1Click(Sender: TObject);
    procedure fromFileDate1Click(Sender: TObject);
    procedure fromFileCreationTime1Click(Sender: TObject);
    procedure fromExifDateTime1Click(Sender: TObject);
    procedure cbAllowStrSimilarityClick(Sender: TObject);
    procedure edtFilePathNoChange(Sender: TObject);
    procedure ReplaceCurrentPhoto1Click(Sender: TObject);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    procedure FromEXIFInfo1Click(Sender: TObject);
    procedure BrowseScenes1Click(Sender: TObject);
    procedure cbShowScenesClick(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure UnDo1Click(Sender: TObject);
    procedure cbHasSceneInfoClick(Sender: TObject);
    procedure ScanSelectedforMissingThumbnail1Click(Sender: TObject);
    procedure UploadSelectedFiles1Click(Sender: TObject);
    procedure btnFillDateClick(Sender: TObject);
    procedure ExportSelectedRecords1Click(Sender: TObject);
    procedure DeleteSelectedRecords1Click(Sender: TObject);
    procedure BrowseFileNames1Click(Sender: TObject);
    procedure fromEnteredDateTime1Click(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure GoToKey1Click(Sender: TObject);
//  procedure GoToRecNo1Click(Sender: TObject);
    procedure btnLastRecordClick(Sender: TObject);
    procedure BrowseTracks1Click(Sender: TObject);
    procedure EditThumbnail1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure FromPhotoDateTime1Click(Sender: TObject);
    procedure cbScanCommentsClick(Sender: TObject);
    procedure Help2Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure Expressions1Click(Sender: TObject);
    procedure Filters1Click(Sender: TObject);
    procedure FunctionKeys1Click(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure Synonyms1Click(Sender: TObject);
    procedure CalculateFileSize1Click(Sender: TObject);
    procedure EvaluateExpression2Click(Sender: TObject);
    procedure EditPhoto1Click(Sender: TObject);
    procedure DeletePhoto1Click(Sender: TObject);
    procedure FileDateIsAGuess1Click(Sender: TObject);
    procedure BrowseBoundaries1Click(Sender: TObject);
    procedure BrowseNoiseWords1Click(Sender: TObject);
    procedure InOrderbyLatLon1Click(Sender: TObject);
    procedure GenerateCSVFile1Click(Sender: TObject);
    procedure Excellent2Click(Sender: TObject);
    procedure Good2Click(Sender: TObject);
    procedure Ok2Click(Sender: TObject);
    procedure Poor2Click(Sender: TObject);
    procedure Terrible1Click(Sender: TObject);
    procedure GenerateFilenamesList1Click(Sender: TObject);
    procedure ImportRecords1Click(Sender: TObject);
    procedure RatePhotos1Click(Sender: TObject);
    procedure DBRadioGroup1Click(Sender: TObject);
    procedure OpenMediaClick(Sender: TObject);
    procedure FillMediaLengthfromFiles1Click(Sender: TObject);
    procedure Ascending1Click(Sender: TObject);
    procedure Descending1Click(Sender: TObject);
    procedure Ascending2Click(Sender: TObject);
    procedure Decsencing1Click(Sender: TObject);
    procedure Ascending3Click(Sender: TObject);
    procedure Descending2Click(Sender: TObject);
    procedure AScending4Click(Sender: TObject);
    procedure Descending3Click(Sender: TObject);
    procedure Browse1Click(Sender: TObject);
    procedure MediaLength1Click(Sender: TObject);
    procedure fromMediaDate1Click(Sender: TObject);
    procedure BrowseReportsFolder1Click(Sender: TObject);
    procedure AlternatePhotoEditor1Click(Sender: TObject);
    procedure RotateLeft01Click(Sender: TObject);
    procedure Rotate1801Click(Sender: TObject);
    procedure cbAllowRotationClick(Sender: TObject);
    procedure FillLocationfromLatLon1Click(Sender: TObject);
    procedure RotateRight901Click(Sender: TObject);
    procedure miMoveFileClick(Sender: TObject);
    procedure CustomKey1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure RenameFilesinFoldertoDateTimeTaken1Click(Sender: TObject);
  private
    procedure CopySelectedFiles;
    function GetForm_Expression: TFrmExpression;
    function GetForm_Expression2: TFrmExpression;
    procedure UpdateFormCaption;
    procedure RecordingChanged(const AudioFileName: string);
    function UpdateThumbnail(PhotoTable: TPhotoTable;
      var Msg: string;
      OverWrite: boolean): TThumbnailProcessingStatus;
    procedure CreateThumbNail(var Msg: string; OverWrite: boolean);
    procedure Enable_Buttons;
    function BuildDistanceIndexTable: boolean;
    procedure SetOrder(NewOrder: TCurrentOrder);
    procedure ImageDoubleClick(Sender: TObject);
{$IfDef dhd2}
    procedure SetIsDHD(value: boolean);
    function GetIsDHD: boolean;
{$EndIf}
{$IfDef dhd}
    function TracksTable: TTracksTable;
    function GetTracksBrowser: TfrmTracksBrowser;
    function MyBoundariesBrowser: TfrmBoundariesBrowser;
    function GetBoundariesTable: TDataset {TBoundariesTable};
{$EndIf}
    function GetDateType: TDateTypes;
    procedure SetDateType(const Value: TDateTypes);
    procedure PhotoTableAfterRefresh(Dataset: TDataSet);
    procedure PhotoTableBeforeRefresh(Dataset: TDataSet);
    procedure FillPhotoDateTime(FromWhat: TFromWhat);
    function AcceptThisFile(const FileName: string): boolean;
    procedure Log(const Message: string; LineNo: integer = 0);
    function IsInASubFolderOfSelectedFolder(CurrentFolder: integer): boolean;
    procedure CreateSubFoldersList(const FolderNumber: string);
    function GenerateTheFileName(FileNamePattern: string; const FilePath,
      FileName: string; FolderNumber: integer; PhotoDate, PhotoKey: string;
      PhotoDateTime: TDateTime;
      LocationsDescription: string;
      BasedOnLatLon: TBasedOnLatLon;
      Latitude, Longitude: double;
      Comment: string;
      Key_Words: string;
      SequenceNo: integer): string;
    function GetScenesBrowser: TfrmBrowseScenes;
    function GetScenesBrowser2: TfrmBrowseScenes;
    procedure CheckMarkMenuOrder(NewOrder: TCurrentOrder);
    function CurrentDateType: TDateTypes;
    procedure RemoveProcessedFolder(FolderName: string);
    procedure PhotoTableBeforeScroll(DataSet: TDataSet);
    procedure BrowseLookupsTable(LookupCategory: TLookupCategory);
    function GetSynonymsTable: TSynonymsTable;
    function GetEvaluationParser: TParser;
    function NoiseWordsTable: TLookupsTable;
    function BuildIndexByNearness: boolean;
    procedure PhotoTableAfterPost(DataSet: TDataSet);
    procedure HandleMissingFileOuter(Lfn: string);
    procedure SaveRotatedImageToFile(imgPhoto: TRotateImage);
    procedure UpdatePhotoInfo(BitMap: Graphics.TBitMap);
    procedure MoveFile(SrcKey: integer; const BaseName, OldFolderName, NewFolderName: string);
    procedure UpdateDates(GoingToDateTime: boolean; OldDateFieldName, NewDateFieldName: string);
//  procedure TempAfterScroll(Dataset: TDataSet);
  private
    fAllowRotation: boolean;
    fAlreadyDragging: boolean;
    fAlternateEditor: string;
{$IfDef dhd2}
    fIsDHD: boolean;
{$EndIf}
{$IfDef dhd}
    fTracksTable: TTracksTable;
    fTracksBrowser: TfrmTracksBrowser;
    fBoundariesBrowser: TfrmBoundariesBrowser;
    fBoundariesTable: TBoundariesTable;
{$EndIf}
    fBrowseFilePaths: TForm;
    fBrowseCopyRights: TForm;
    fBrowseLocations: TfrmLocationsBrowser;
    fBuildingIndexTable: boolean;
    fCurrentOrder: TCurrentOrder;
    fCurrentRecordNumber: integer;
    fDragPoint: TPoint;
    fEvaluationParser: TParser;
    fExpression: string;
//  fFileNameInfo, fSavedFileNameInfo: TFileNameInfo;
    fUpdateInfo: TUpdateInfo;
    fLocationInfo: TLocationInfo;
    fSavedLocationInfo: TLocationInfo;
    fFileNamesBrowser: TfrmDataSetBrowser;
    fFillLocationInfo: TfrmFillLocationInfo;
    fFolderList: TStringList;
    fForm_Expression: TFrmExpression;
    fForm_Expression2: TFrmExpression;
    fFrmAudioRecorder: TfrmAudioRecorder;
    fHTMLGenerator: THTMLGenerator;
    fIgnoreMissingFile: boolean;
    fLastPathNo: integer;
    fLastSearchKind: TLastSearchKind;
    fMaxDistance: double;
    fMaxPhotosChanged: boolean;
    fNoiseWordsTable: TLookupsTable;
    fNrFiltered: integer;
    fSavedKey: integer;
    fScenesBrowser: TfrmBrowseScenes;
    fScenesBrowser2: TfrmBrowseScenes;
    fSynonymsTable: TSynonymsTable;
    fThumbNailsShowing: integer;
    fLastCopyr_ID: string;
    fLastSearch: string;
    fRatingItems: array[-2..+2] of TMenuItem;
    fRatingPhotos: boolean;
    fReDisplayingThumbnails: boolean;
    fSaveFolder: string;
    fSubFoldersList: TList;
    fOldFileName: string;
    fChanged: TSetOfChangeReasons;
    fRefreshThumbnails: boolean;
    fComponent: TComponent;
    fLastKeywords: string;
    fLastLocationID: integer;
    fLastLatitude: double;
    fLastLongitude: double;
    fMediaFileName: string;
    fPhotoList: TPhotoList;
    fPrevKey: integer;
    fSelectedImage: TImage;
    fSelectedPhotoKey: integer;
    fTimer: TTimer;
    fFullSizeWindowsInfo: TMyWindowInfo;
    fTempFilePathsTable: TFilePathsTable;
    fLogFile: TextFile;
    fLogPathFileName: string;
    FKMenuItems: array[TShiftMode, 1..MAXKEY] of TMenuItem;
    fCount: integer;
    fSynonymsList: TSynonymObject;
    ftblLocationsTable: TLocationsTable;
    fTempSynonymsList: TSynonymObject;
    fTempCopyrightTable: TAdoTable;
    fTempScenesTable: TScenesTable;

    procedure ProcessFolder(const FolderPath: string;
                            UpdateProc: TUpdateProc;
                            Update: boolean);
    procedure AnUpdateProc(UpdateInfo: TUpdateInfo; var KeyWords: string);
    procedure PhotoTableAfterScroll(DataSet: TDataSet);
    procedure PhotoTableBeforePost(Dataset: TDataSet);
    procedure PostAndMove(MoveWhere: TMoveProc);
    procedure InsertText(aComponent: TComponent; sm: TShiftMode; n: integer);
    procedure SetFuncKey(sm: TShiftMode; n: integer; Txt: string);
    procedure CaptureText(aComponent: TComponent; n: integer);
    procedure PhotoTableBeforeInsert(Dataset: TDataSet);
    procedure ScanDuplicates(ScanOrder: TCurrentOrder; DeleteWhat: TDeleteWhich);
    procedure ScanForMissing(Update: boolean);
    procedure PhotoTableBeforeOpen(DataSet: TDataset);
    procedure EditPhoto; overload;
//  procedure EditPhoto(const FileName: string); overload;
    procedure OpenFullSizeWindow(OpenIt: boolean);
    function GetTimer: TTimer;
    function GetOptionsFromUser: boolean;
    procedure ShowCalcFields(DataSet: TDataSet);
    procedure ScanDuplicateFileSizes(aFileName: string);
    procedure LoadFuncKeys;
    procedure GetAndSetPhotoDate(PathAndName: string; FileNameInfo: TFileNameInfo);
    function ContainsWords( Target, Data: string;
                            MatchWholeWordsOnly: boolean;
                            MustContainAll: boolean = true;
                            AllowSynonyms: boolean = false;
                            aSynonymsList: TSynonymObject = nil;
                            AllowStrSimilarity: boolean = false;
                            MinSimilarity: integer = MIN_STRSIMILARITY): boolean;
    function TempFilePathsTable: TFilePathsTable;
    function TempCopyrightTable: TDataSet;
    function TempScenesTable: TScenesTable;
    procedure EditLocationInfo(CreateIt: boolean);
    procedure Update_Status(const Msg: string; LineNo: integer = 1);
    procedure FindLocation(aLocationSearchType: TLocationSearchType);
    function GetBrowseLocations: TfrmLocationsBrowser;
    function FindKeyWords(SearchString: string): boolean;
    procedure FillWasScannedFromExif;
    procedure ShowExifInfo(const aCurrentFileName: string);
    function GetFillLocationInfo: TfrmFillLocationInfo;
    procedure ResetMediaClasses;
    procedure FileMover;
    procedure ReDisplayThumbnails(ReScan: boolean);
    function GetPhotoList: TPhotoList;
    function FindString(SearchFor: String): boolean;
    procedure FindNextString;
    procedure FindNextKeyWords;
    procedure RateThisPhoto(Rating, Key: integer; Advance: boolean = false);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    property Form_Expression: TFrmExpression
             read GetForm_Expression;
    property Form_Expression2: TFrmExpression
             read GetForm_Expression2;
    property EvaluationParser: TParser
             read GetEvaluationParser;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMHideScenes(Var Message: TMessage); message WM_HideScenes;
  public
    { Public declarations }
    SavedText: array[TShiftMode, 1..MAXKEY] of string;
    tblPhotoTable: TPhotoTable;
    tempPhotoTable: TPhotoTable;

    procedure ClosePhotoTable;
    function  OpenPhotoTable: boolean;
    function SynonymsList: TSynonymObject;
    Destructor Destroy; override;
    procedure LogError(const Msg: string; const F1, F2: string);
    procedure SizePhoto;
    function tblLocationsTable: TLocationsTable;

    property CurrentOrder: TCurrentOrder
             read fCurrentOrder
             write fCurrentOrder;
    procedure SetInterval(Interval: integer);
    property SynonymsTable: TSynonymsTable
             read GetSynonymsTable;
    property Timer: TTimer
             read GetTimer;
    property LocationsBrowser: TfrmLocationsBrowser
             read GetBrowseLocations;
    property ScenesBrowser: TfrmBrowseScenes
             read GetScenesBrowser;
    property ScenesBrowser2: TfrmBrowseScenes
             read GetScenesBrowser2;
    Constructor Create(aOwner: TComponent); override;
    property PhotoList: TPhotoList
             read GetPhotoList;
    procedure PhotoTableFilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
    property FillLocationInfo: TfrmFillLocationInfo
             read GetFillLocationInfo;
{$IfDef dhd2}
    property IsDHD: boolean
             read GetIsDHD
             write SetIsDHD;
{$EndIf}
{$IfDef dhd}
    property TracksBrowser: TfrmTracksBrowser
             read GetTracksBrowser;
{$EndIf}
    property DateType: TDateTypes
             read GetDateType
             write SetDateType;
    property Expression: string
             read fExpression;
  end;

  TGetPDFText = function {GetPDFText}( const FileName: PWideChar;
                                             opt: LongInt;
                                             hw: LongInt;
                                             fast: LongInt;
                                             target: PWideChar;
                                             lspaces: LongInt;
                                             ptitel: PWideChar;
                                             pos: LongInt;
                                             page: LongInt;
                                             clock: LongInt;
                                             blank: LongInt;
                                             ende: LongInt;
                                             wlist: LongInt): PWideChar; stdcall;

  TGetPDFPageCount = function {GetPDFPageCount}(const FileName: PWideChar): LongInt; stdcall;

  function GetPDFText(                 const FileName:
                                             PWideChar;
                                             opt: LongInt;
                                             hw: LongInt;
                                             fast: LongInt;
                                             target: PWideChar;
                                             lspaces: LongInt;
                                             ptitel: PWideChar;
                                             pos: LongInt;
                                             page: LongInt;
                                             clock: LongInt;
                                             blank: LongInt;
                                             ende: LongInt;
                                             wlist: LongInt): PWideChar; stdcall;

  function GetPDFPageCount(const FileName: PWideChar): LongInt; stdcall;

var
  frmPhotoDataBase: TfrmPhotoDataBase;

implementation

{$R *.DFM}

uses
  StStrL, DeleteOptions, ShellAPI, uPhotoDBOptions,
  uHandleMissingFile, MyUtils, About, MyUtils2, uGetString, FullSize, DeletionConfirmation,
  ReportOptions, DateUtils, ShlObj, FileMover, PhotoDBCommonSettings,
  dIPTC, LocationUtils, UpdateLocationInfoFromGPX,
  Clipbrd, EXIFInfo, MyReplaceDialog, ThumbNailUnit, AdjustDateTime,
  FilterSettingsUnit, LookupBrowser,
  ReplaceFieldsInSelectedRecords, MyTables_Decl, MyTables, PhotoUtils,
  ScanForDocuments, DateTimeDiff, Math, {JpegErrors,} uBrowseMemo,
  DropSource, BrowseFuncKey, StrCompareX, PDBMisc,
{$IfDef UploadFiles}
  UploadFiles,
{$EndIf}
  ScanningOptions,
{$IfDef SMEExport}
  Export,
{$else}
  Export2,
{$EndIf}
  HikingSettingsUnit, ConfirmEachPhoto, Variants, VideoStuff2
{$IfDef dExif2}
  , dMetadata
{$endIf}  ;

const
  SCROLLBOX_TOP = 40;
  MAX_THUMBNAILS_TO_SHOW = 100;
  BORDER_WIDTH = 10;

type
  TKeyNames = array[1..MAXKEY] of string;
var

  FuncKeyNames: array[TShiftMode] of TKeyNames = (
    ({smNone}  'F01',  'F02',  'F03',  'F04',  'F05',  'F06',  'F07',  'F08',  'F09',  'F10'),
    ({smShift} 'SF01', 'SF02', 'SF03', 'SF04', 'SF05', 'SF06', 'SF07', 'SF08', 'SF09', 'SF10'),
    ({smCtrl}  'CF01', 'CF02', 'CF03', 'CF04', 'CF05', 'CF06', 'CF07', 'CF08', 'CF09', 'CF10')
  );

type
  EFilterError = class(Exception);

function GetPDFText(const FileName: PWideChar; opt: LongInt; hw: LongInt; fast: LongInt; target: PWideChar; lspaces: LongInt; ptitel: PWideChar; pos: LongInt; page: LongInt; clock: LongInt; blank: LongInt; ende: LongInt; wlist: LongInt): PWideChar; stdcall;
  external 'PDFtext.dll';

function GetPDFPageCount(const FileName: PWideChar): LongInt; stdcall;
  external 'PDFtext.dll';

procedure UNTESTED;
begin
  if not Yes('Code has not been tested with FileNameInfo. Do you want to proceed?') then
    SysUtils.Abort;
end;


procedure TfrmPhotoDataBase.anUpdateProc(UpdateInfo: TUpdateInfo; var KeyWords: string);
begin
  UNTESTED;
  tempPhotoTable.UpdateProc(UpdateInfo, fLocationInfo, KeyWords, Update_Status);
end;

procedure TfrmPhotoDataBase.ProcessFolder(const FolderPath: string;
                                          UpdateProc: TUpdateProc;
                                          Update: boolean);
var
  SearchRec: TSearchRec; dosError: integer;
  PathFilter, Path: string; Attr: integer;
  Accept: boolean;
  Prefix: string;
  UpdateInfo: TUpdateInfo;
  KeyWords: string;
begin
  Update_Status(FolderPath);
  Application.ProcessMessages;
  // process files in this folder first
  Path       := AddBackSlashL(FolderPath);
  PathFilter := Path + EXT_JPG;
  Attr       := faAnyFile - faHidden - faSysFile - faVolumeID - faDirectory;

  dosError   := FindFirst(PathFilter, Attr, SearchRec);
  while dosError = 0 do
    begin
      Prefix := UpperCase(Copy(SearchRec.Name, 1, 3));

      if Prefix <> PREFIX_TN then
        begin
          UNTESTED;
          UpdateInfo.FileName     := SearchRec.Name;
          UpdateInfo.FileNameInfo := fUpdateInfo.FileNameInfo;
          if not GetPhotoInfo(UpdateInfo) then
            { do something? what? };
          if not Empty(fLocationInfo.DefaultLocation) then
            fLocationInfo.DefaultLocationKind := dl_SpecifiedValue
          else
            begin
              fLocationInfo.DefaultLocation     := CommonPhotoSettings.DefaultLocation;;
              fLocationInfo.DefaultLocationKind := CommonPhotoSettings.DefaultLocationKind;
            end;

          if tempPhotoTable.MyLocateRecord( SearchRec.Name, FolderPath) then
            begin
              Accept := true;
              PhotoTableFilterRecord(tempPhotoTable, Accept);
              if Accept then  // record exists and user wants to process it
                begin
                  UpdateInfo.FileName        := SearchRec.Name;
                  UpdateInfo.FilePath        := FolderPath;
                  UpdateInfo.UpdateIt        := Update;
                  UpdateInfo.RecordExists    := true;
                  try
                    UpdateProc(UpdateInfo, KeyWords);
                  except
                    on e:exception do
                      Alert(e.message);
                  end;
                end;
            end
          else // record dosen't exist
            begin
              UpdateInfo.FileName := SearchRec.Name;
              UpdateInfo.FilePath := FolderPath;
              UpdateInfo.UpdateIt := Update;
              try
                UpdateProc(UpdateInfo, KeyWords);
              except
                on e:Exception do
                  Alert(e.message);
              end;
            end;
        end;
      dosError := FindNext(SearchRec);
    end;

  // then process child folders
  PathFilter := Path + '*.*';
  dosError   := FindFirst(PathFilter, faAnyFile, SearchRec);
  while dosError = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) > 0)
         and (SearchRec.Name <> '.')
         and (SearchRec.Name <> '..')
         and (CompareText(SearchRec.Name, 'tn') <> 0) then
        ProcessFolder(Path+SearchRec.Name, UpdateProc, Update);
      dosError := FindNext(SearchRec);
    end;
end;


procedure TfrmPhotoDataBase.BuildInitialDB1Click(Sender: TObject);
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        [optNoSyncFilePathTable]);
  with tempPhotoTable do
    begin
      BeforeInsert   := PhotoTableBeforeInsert;
      Active         := true;
      try
        ProcessFolder(edtRootPath.Text, anUpdateProc, true);
        Update_Status('COMPLETE');
      finally
        Filtered := false;
        Active := false;
        Free;
      end;
    end;
end;

procedure TfrmPhotoDataBase.Exit1Click(Sender: TObject);
begin
  Close;
end;

function TfrmPhotoDataBase.CurrentDateType: TDateTypes;
begin
  with cbDateType do
    if ItemIndex >= 0 then
      result := TDateTypes(Items.Objects[ItemIndex])
    else
      result := dt_PhotoDate;
end;


(*
function TfrmPhotoDataBase.GetBestDate(const s: string; HighLow: THighLow): TDateTime;
var
  Year: integer;
  DateType: TDateTypes;
begin { GetBestDate }
  result := BAD_DATE;
  try
    with cbDateType do
      if ItemIndex >= 0 then
        begin
          DateType := TDateTypes(Items.Objects[ItemIndex]);
          case DateType of
            dt_PhotoDateTime:
              result := StrToDateTime(s);
            else
              result := StrToDate(s);
          end;
        end;
  except
    if Length(s) = 4 then
      if IsPureNumeric(s) then
        begin
          Year := StrToInt(s);
          if (Year >= 1900) and (Year <= 2100) then
            case HighLow Of
              hl_UseLow:
                result := EncodeDate(Year, 1, 1);
              hl_UseHigh:
                result := EncodeDate(Year, 12, 31);
            end;
        end;
  end;
end;  { GetBestDate }
*)

function TfrmPhotoDataBase.IsInASubFolderOfSelectedFolder(CurrentFolder: integer): boolean;
var
  t, b, m: integer;
  Middle: integer;
begin
  result := false;
  if Assigned(fSubFoldersList) then
    begin
      t := 0;
      b := fSubFoldersList.Count-1;
      while t <= b do
        begin
          m      := (t + b) div 2;
          Middle := Integer(fSubFoldersList[m]);
          if CurrentFolder < Middle then
            b := m - 1 else
          if CurrentFolder > Middle then
            t := m + 1
          else // CurrentFolder = Middle
            begin
              result := true;
              Exit;
            end;
        end;
    end
  else
    Error('System error: fSubFoldersList is not assigned');
end;


procedure TfrmPhotoDataBase.PhotoTableFilterRecord(DataSet: TDataSet; var Accept: Boolean);
  var
    b: boolean; aPathName: string;
    mt: TMediaType;
    mc: TMediaClass;
    Temp1, Temp2: string;
    OK: boolean;
    TempScenesKey: integer;
    CriticalMismatches: integer;
    KeyWords, KeyComments: string;
    Ext: string;
    Dummy: TField;

  procedure IsCritical(Accept: boolean);  // When filtering on SceneInfo, some fields may be matched later
  begin { IsCritical }
    if not Accept then
      Inc(CriticalMismatches);
  end;  { IsCritical }

begin { TfrmPhotoDataBase.PhotoTableFilterRecord }
  Accept := true;
  CriticalMismatches := 0;

  with DataSet as TPhotoTable do
    begin
      if not Assigned(fldUPDATED) then
        Exit;   // We may get called before field pointers are assigned in DoAfterOpen

      if Accept then
        if not Empty(edtFileNames.Text) then
          begin
            Accept := (Pos(UpperCase(edtFileNames.Text), UpperCase(fldFILE_NAME.AsString)) > 0);
            IsCritical(Accept);
          end;

      if Accept then
        Accept := CheckLowDate(CurrentDateType, edtLowDate.Text, fldPhotoDate, fldAdded, fldUpdated, fldPhotoDateTime, Dummy);

      if Accept then
        Accept := CheckHighDate(CurrentDateType, edtHighDate.Text, fldPhotoDate, fldAdded, fldUpdated, fldPhotoDateTime, Dummy);

      if Accept then
        Accept := CheckParsedDates( edtLowYear.Text,  edtHighYear.Text,
                                    edtLowMonth.Text, edtHighMonth.Text,
                                    edtLowDay.Text,   edtHighDay.Text,
                                    fldYear, fldMonth, fldDay);

      if Accept then
        if (not Empty(edtFilePathNo.Text)) and IsAllNumeric(edtFilePathNo.Text) then
          if not cbIncludeSubFolders.Checked then
            begin
              Accept := (StrToInt(edtFilePathNo.Text) = fldPATH_NO.AsInteger);
              IsCritical(Accept);
            end
          else
            begin
              Accept := IsInASubFolderOfSelectedFolder(fldPATH_NO.AsInteger);
              IsCritical(Accept);
            end;

      if Accept then
        if not Empty(edtKeyWords.Text) then
          begin
            Accept := ContainsWords( edtKeyWords.Text,
                                     fldKEY_WORDS.AsString,
                                     cbMatchWholeWordsOnly.Checked,
                                     true, // must match all
                                     cbUseSynonyms.Checked,
                                     fTempSynonymsList,
                                     cbAllowStrSimilarity.Checked,
                                     meSS.AsInteger);
            if not Accept then
              if cbScanComments.Checked then // if not found in the key words field, look in the comments
                begin
                  Accept := ContainsWords( edtKeyWords.Text,
                                             fldComment.AsString,
                                             cbMatchWholeWordsOnly.Checked,
                                             true, // must match all
                                             cbUseSynonyms.Checked,
                                             fTempSynonymsList,
                                             cbAllowStrSimilarity.Checked,
                                             meSS.AsInteger);
                end;

            if not Accept then // if not found in the key words field or the comments fields, look in the text contained in the file
              if cbScanFile.Checked then
                begin
                  Accept := ContainsWords( edtKeyWords.Text,
                                             fldTextInFile.AsString,
                                             cbMatchWholeWordsOnly.Checked,
                                             true, // must match all
                                             cbUseSynonyms.Checked,
                                             fTempSynonymsList,
                                             cbAllowStrSimilarity.Checked,
                                             meSS.AsInteger);
                end;
          end;

      if Accept then
        if cbUnprocessedOnly.Checked then
          begin
            Accept := ((fldUPDATED.IsNull) or (fldUPDATED.AsDateTime = 0));
            IsCritical(Accept);
          end;

      if Accept then
        if (not Empty(edtCopyCode.Text)) or (cbNot.Checked) then
          begin
            b := SameText(Trim(fldFileNamesTable_CopyID.AsString),
                          Trim(edtCopyCode.Text));
            if cbNot.Checked then
              b := not b;
            Accept := b;
            IsCritical(Accept);
          end;

      if Accept then
        if cbHasSound.Checked then
          begin
            Accept := fldHasSound.AsBoolean;
            IsCritical(Accept);
          end;

      if Accept then
        if cbLocationID.Checked then
          begin
            Accept := fldLocationID.AsInteger > 0;
            IsCritical(Accept);
          end;

      if Accept then
       if clbMediaClasses.Items.Count > 0 then
        with clbMediaClasses do
          begin
            if not Checked[0] then // we're not including everything
              begin
//              mt := MediaTypeFromExtension(fldFILE_TYPE.AsString);
                Ext := ExtractFileExt(fldFILE_NAME.AsString);
                mt  := MediaTypeFromExtension(Ext);
                for mc := Succ(Low(TMediaClass)) to High(TMediaClass) do
                  begin
                    if Checked[ord(mc)] then
                      begin
                        Accept := mc = MediaInfoArray[mt].MediaClass;
                        if Accept then
                          break;
                      end;
                  end;
                IsCritical(Accept);
              end;
          end;

      if Accept then
        if cbHasLocationInfoInEXIF.Checked then
          begin
            Accept := LocationsTable.HasLocationInfoInEXIF(PathAndFileName);
            IsCritical(Accept);
          end;

      if Accept then
        if HasParser and Assigned(SelectivityParser.Eval_Tree) then
          begin
            SelectivityParser.Eval_Tree.Evaluate(DataSet, false);
            try
              Accept := SelectivityParser.Eval_Tree.AsBoolean;
              IsCritical(Accept);;
            except
              Accept := false;  // if the expression can't be converted to boolean, skip the record
            end;
          end;

      if Accept then
        if fCurrentOrder in [coDistanceFromLocation{, coNearness}] then
          begin
            Accept := (not fldLocationID.IsNull) and (fldLocationID.AsInteger > 0);
            // If we're sorting on distance, then ignore records that don't have a distance

            if Accept then
              if (not fBuildingIndexTable) and (fMaxDistance > 0.0) then
                begin
                  Accept := fldDistance.AsFloat <= fMaxDistance;
                  IsCritical(Accept);
                end;
          end;

      if Accept then
        if not Empty(edtStringInPath.Text) then
          begin
            OK := (fldPATH_NO.AsInteger = fLastPathNo);
            if not OK then
              Ok := TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []);
            if OK then
              begin
                fLastPathNo := fldPATH_NO.AsInteger;
                aPathName := TempFilePathsTable.fldFILE_PATH.AsString;
                Accept := (Pos(UpperCase(edtStringInPath.Text), UpperCase(aPathName)) > 0);
                IsCritical(Accept);
              end
            else
              Accept := false;
          end;

      if Accept then
        if cbFileIsMissing.Checked then
          begin
            OK := (fldPATH_NO.AsInteger = fLastPathNo);
            if not OK then
              Ok := TempFilePathsTable.Locate(PATH_NO, fldPATH_NO.AsInteger, []);
            if OK then
              begin
                fLastPathNo := fldPATH_NO.AsInteger;
                Temp1       := FixPath(TempFilePathsTable.fldFILE_PATH.AsString);
                Temp2       := fldFile_Name.AsString;
                aPathName   := Temp1 + Temp2;
                Accept      := not FileExists(aPathName);
                IsCritical(Accept);
              end;
          end;

      if Accept then
        if cbHasSceneInfo.Checked then
          begin
            Accept := tblPhotoTable.fldHasSceneInfo.AsBoolean;
            IsCritical(Accept);
          end;

      if (cbScanSceneKeyWords.Checked or cbScanSceneDates.Checked) and (not Accept) and (CriticalMismatches = 0) then // maybe the info we are looking for is in the scene info
        begin
          if fldHasSceneInfo.AsBoolean then  // this record has scene info
            begin
              TempScenesKey := fldKey.AsInteger;  // scan the scenes table for records linked to this video

              KeyWords    := fldKEY_WORDS.AsString + ' ';  // Must include key words from photo record
              KeyComments := fldComment.AsString + ' ';

              with TempScenesTable do
                begin
                 if Locate(FILENAME_KEY, TempScenesKey, []) then
                  while (not Eof) and (fldFileName_Key.AsInteger = TempScenesKey) do  // look until we find a matching record, or until the end of the scenes info
                    begin
                      Accept   := true;

                      if cbScanSceneDates.Checked then
                        begin
                          if Accept then
                            Accept := CheckLowDate(CurrentDateType, edtLowDate.Text, fldPhotoDate, fldAdded, fldUpdated, fldPhotoDateTime, Dummy);

                          if Accept then
                            Accept := CheckHighDate(CurrentDateType, edtHighDate.Text, fldPhotoDate, fldAdded, fldUpdated, fldPhotoDateTime, Dummy);

                          if Accept then
                            Accept := CheckParsedDates( edtLowYear.Text, edtHighYear.Text,
                                                        edtLowMonth.Text, edtHighMonth.Text,
                                                        edtLowDay.Text,   edtHighDay.Text,
                                                        fldYear, fldMonth, fldDay);
                        end;

                      if cbScanSceneKeyWords.Checked then
                        if Accept then
                          if not Empty(edtKeyWords.Text) then
                            begin
                              Accept := ContainsWords( edtKeyWords.Text,
                                                       KeyWords + fldKEY_WORDS.AsString,
                                                       cbMatchWholeWordsOnly.Checked,
                                                       true, // must match all
                                                       cbUseSynonyms.Checked,
                                                       fTempSynonymsList,
                                                       cbAllowStrSimilarity.Checked,
                                                       meSS.AsInteger);

                              if not Accept then
                                if cbScanComments.Checked then // if not found in the key words field, look in the comments
                                  begin
                                    Accept := ContainsWords( edtKeyWords.Text,
                                                             KeyComments + fldComment.AsString,
                                                             cbMatchWholeWordsOnly.Checked,
                                                             true, // must match all
                                                             cbUseSynonyms.Checked,
                                                             fTempSynonymsList,
                                                             cbAllowStrSimilarity.Checked,
                                                             meSS.AsInteger);
                                  end;
                            end;

                      if Accept then
                        Break;

                      Next;
                    end;
                end;
            end;
        end;

    end;
  Inc(fNrFiltered);
  if (fNrFiltered mod 100) = 0 then
    begin
      lblStatus1.Caption := IntToStr(fNrFiltered);
      Application.ProcessMessages;
    end;
end;  { TfrmPhotoDataBase.PhotoTableFilterRecord }

procedure TfrmPhotoDataBase.HandleMissingFileOuter(Lfn: string);
var
  NewFileName: string;      // just the path
  mr: integer;
  Path_No: integer;
  dummy: string;
begin { TfrmPhotoDataBase.HandleMissingFileOuter }
  imgPhoto.Visible := false;
  NewFileName      := Lfn;
  mr := HandleMissingFile(self, gRootPath, tblPhotoTable.fldKEY_WORDS.AsString, NewFileName);
  case mr of
    mrUpdateDBRec:
      begin
        with tblPhotoTable do
          begin
            lfn     := ExtractFilePath(NewFileName);
            Path_No := TempFilePathsTable.FindFolderNo( lfn,
                                                        dummy,
                                                        true);
            Edit;
            fldFILE_NAME.AsString := ExtractFileName(NewFileName);
            fldPATH_NO.AsInteger  := Path_No;
            fldUpdateReason.AsInteger := integer(ur_WasMissingNowFound);  // tested
            Post;
          end;
      end;

    mrDeleteDBRec:
      tblPhotoTable.Delete;

    mrIgnore:
      fIgnoreMissingFile := true;
  end;
end;  { TfrmPhotoDataBase.HandleMissingFileOuter }


procedure TfrmPhotoDataBase.SizePhoto;
const
  cUSEBUTTON = ' [Open File] button below';
var
  Lfn, Ext: string;
  mt: TMediaType; mc: TMediaClass;
  ThumbnailPathName: string;

  procedure ShowPicture(mc: TMediaClass);
  var
    WasEdited: boolean;
  begin { ShowPicture }
    if not Empty(tblPhotoTable.fldFILE_NAME.AsString) then
      begin
        if FileExists(Lfn) then  // using the SLOW version for a debug test
          begin
            if cbUseFullSizeWindow.Checked and (Assigned(frmFullSize)) then
              begin
                imgPhoto.Visible := false;
                InnerSizePhoto( Lfn,
                                frmFullSize.DBImage1,
                                frmFullSize,
                                mc,
                                fAllowRotation,
                                CommonPhotoSettings.PhotoEditingProgram,
                                WasEdited);
                if WasEdited then
                  UpdatePhotoInfo(ImgPhoto.RotatedBitMap);

                frmFullSize.Caption := tblPhotoTable.fldKEY_WORDS.AsString;
              end
            else
              begin
                imgPhoto.Visible := true;
                InnerSizePhoto( Lfn,
                                imgPhoto,
                                pnlPhoto,
                                mc,
                                fAllowRotation,
                                CommonPhotoSettings.PhotoEditingProgram,
                                WasEdited)
              end;
          end
        else
          HandleMissingFileOuter(Lfn);
      end;
  end;  { ShowPicture }

begin { TfrmPhotoDataBase.SizePhoto }
  if fIgnoreMissingFile or (tblPhotoTable.bof and tblPhotoTable.eof) then
    begin
      imgPhoto.Visible := false;
      exit;
    end;

  Lfn := tblPhotoTable.PathAndFileName;
  Ext := MyExtractFileExt(Lfn);
  mt  := MediaTypeFromExtension(Ext);
  mc  := MediaInfoArray[mt].MediaClass;

  if ShowThumbnailsRatherThanFullSizePhoto1.Checked and (mc in [mc_Photos, mc_Video]) then // show thumbnail rather than full size photo
    begin
      ThumbNailPathName := ThumbNailPathAndName(Lfn);
      Lfn               := ThumbNailPathName;
    end;

  with tblPhotoTable do
    Update_Status( Format('%d/%d: %s', [Recno, TotalRecordCount, Lfn]));

  fMediaFileName   := Lfn;
  case mc of
    mc_Photos, mc_Video{, mc_Audio, mc_Document}:
      begin
        ShowPicture(mc);
        pnlPhoto.Caption := '';
      end
    else
      begin
        imgPhoto.Visible := false;
        with pnlPhoto do
          begin
            case mc of
              mc_DVD:          Caption := 'This is an .iso file which can be burned to a DVD or mounted';
              mc_Powerpoint:   Caption := 'This is a PowerPoint document.'+cUSEBUTTON;
              mc_Spreadsheet:  Caption := 'This is a SpreadSheet.'+cUSEBUTTON;
              mc_Document:     Caption := 'This is a Document.'+cUSEBUTTON;
              mc_Audio:        Caption := 'This is an audio file.'+cUSEBUTTON;
              mc_VideoProject: Caption := 'This is a video project file.'+cUSEBUTTON
            else
              Caption := '';
            end;
            if (not FileExists(Lfn)) then  // Warning: the FileExists function sometimes returns false when a file DOES exist!
              begin
                HandleMissingFileOuter(Lfn);
                Color   := clYellow;
                if (mc in [mc_DVD, mc_Powerpoint, mc_Spreadsheet, mc_Document, mc_Audio, mc_VideoProject]) then
                  Caption := Caption + ' (Not found)'
                else
                  Caption := Caption + Format(' (File %s not found. MediaClass = %d', [Lfn, ord(mc)]);
              end
            else
              Color := clBtnFace;
          end;
      end;
  end;
end;  { TfrmPhotoDataBase.SizePhoto }

procedure TfrmPhotoDataBase.PhotoTableBeforeScroll(DataSet: TDataSet);
begin
  fPrevKey := tblPhotoTable.fldKey.AsInteger;
end;

procedure TfrmPhotoDataBase.PhotoTableAfterPost(DataSet: TDataSet);
begin
  PhotoTableAfterScroll(DataSet);
end;

procedure TfrmPhotoDataBase.PhotoTableAfterScroll(DataSet: TDataSet);
begin
  fIgnoreMissingFile := false;
  with tblPhotoTable do
    if not ControlsDisabled then
      begin
        ShowCalcFields(DataSet);
        SizePhoto;
        if Assigned(FrmExifInfo) then
          ShowExifInfo(PathAndFileName);
          
        if Assigned(fFrmAudioRecorder) then
          fFrmAudioRecorder.AudioFileName := ExtractFilePath(PathAndFileName) + ExtractFileBase(PathAndFileName) + '.wav';

        if ScenesTableInUse and cbShowScenes.Checked then
          if Assigned(fScenesBrowser) then
            with ScenesBrowser do
              begin
                PhotoKey      := fldKey.AsInteger;
                if cbScanSceneKeyWords.Checked then
                  ScanForKeyWords(edtKeyWords.Text, fldKey_Words.AsString) else
                if cbScanSceneDates.Checked then
                  ScanForDate(CurrentDateType, edtLowDate.Text, edtHighDate.Text);
              end;

        if Assigned(fForm_Expression2) and Assigned(fEvaluationParser) then
          begin
            EvaluationParser.Eval_Tree.EvaluateTree;
            fForm_Expression2.edtResult.Text := EvaluationParser.Eval_Tree.AsString;
          end;
      end;
end;

procedure TfrmPhotoDataBase.PhotoTableBeforePost(Dataset: TDataSet);
begin
  with DataSet as TPhotoTable do
    begin
      fLastKeywords    := fldKEY_WORDS.AsString;
      fLastLocationID  := fldLocationID.AsInteger;
      fLastLatitude    := fldLatitude.AsFloat;

      if (not (optNoUpdateDate in Options)) and (State in [dsEdit, dsInsert]) then
        fldUPDATED.AsDateTime := Now;
    end;
end;

procedure TfrmPhotoDataBase.PhotoTableBeforeInsert(Dataset: TDataSet);
begin
  with tblPhotoTable do
    begin
      if State in [dsEdit, dsInsert] then
        fldADDED.AsDateTime := Now;
    end;
end;

procedure TfrmPhotoDataBase.SetFuncKey(sm: TShiftMode; n: integer; Txt: string);
begin
  SavedText[sm, n] := Txt;
  FKMenuItems[sm, n].Caption    := Txt;
//CapFKMenuItems[n].Caption := Txt;
end;

procedure TfrmPhotoDataBase.PhotoTableBeforeOpen(DataSet: TDataset);
begin
  DataSet.OnFilterRecord   := PhotoTableFilterRecord;
  DataSet.Filtered         := false;
end;

procedure TfrmPhotoDataBase.ClosePhotoTable;
begin
  if Assigned(tblPhotoTable) then
    begin
//    fRatingPhotos := false;
      tblPhotoTable.Close;
      FreeAndNil(tblPhotoTable);
    end;
end;

function TfrmPhotoDataBase.NoiseWordsTable: TLookupsTable;
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

function TfrmPhotoDataBase.OpenPhotoTable: boolean;
{$IfDef dhd}
var
  b1, b2: boolean;
{$EndIf}
begin
{$IfDef debugging}
  Message('Entering TfrmPhotoDataBase.OpenPhotoTable');
{$endIf}
  if not Assigned(tblPhotoTable) then
    tblPhotoTable := TPhotoTable.Create( self, CommonPhotoSettings.PhotoDBDatabaseFileName, cFILENAMES, []);
  with tblPhotoTable do
    begin
{$IfDef dhd}
      if SameText(ComputerName, 'Studio') then
        begin
          Copy_ID          := 'DHD'; // set default copyright owner
          b1 := SameText('\\XPS-8930\ndas-i\Shared Programs\PhotoDB\PhotoDB.AccDb',
                         CommonPhotoSettings.PhotoDBDatabaseFileName);
          b2 := SameText('\\XPS-8930\Ndas-i\My Pictures',
                         CommonPhotoSettings.PhotoDBFolder);
          if b1 and b2 then
            Color := clBtnFace else
          if not b1 then
            Color := clYellow
          else
            Color := clFuchsia;
        end;

      tblPhotoTable.onGetBoundariesTable := GetBoundariesTable;
{$EndIf}
      AfterScroll      := PhotoTableAfterScroll;
      AfterPost        := PhotoTableAfterPost;
      BeforeScroll     := PhotoTableBeforeScroll;
      BeforePost       := PhotoTableBeforePost;
      BeforeOpen       := PhotoTableBeforeOpen;
      AfterRefresh     := PhotoTableAfterRefresh;
      BeforeRefresh    := PhotoTableBeforeRefresh;
      try
{$IfDef debugging}
  MessageFmt('before Active := true. DBName = %s', [CommonPhotoSettings.PhotoDBDatabaseFileName]);
{$endIf}
        Active           := true;
{$IfDef debugging}
  Message('After Active := true');
{$endIf}

        dsPhotoTable.DataSet := tblPhotoTable;
        dsFilePaths.DataSet  := tblPhotoTable.FilePathsTable;
        dsCopyRight.DataSet  := tblPhotoTable.CopyRightsTable;
        Init_NoiseWords(NoiseWordsTable);
        FreeAndNil(fNoiseWordsTable);
        edtRootPath.Text     := gRootPath;
        result               := true;
      except
        on e:Exception do
          begin
            ErrorFmt('Cannot open photo database [%s]' + CRLF +
                     'Settings File Name = %s' + CRLF +
                     'Message = %s',
                     [CommonPhotoSettings.PhotoDBDatabaseFileName, gCommonSettingsFileName, e.Message]);
            result := false;
//          if not GetOptionsFromUser then
//            Exit;
          end;
      end;
    end;
{$IfDef debugging}
  Message('Exiting TfrmPhotoDataBase.OpenPhotoTable');
{$endIf}
end;

procedure TfrmPhotoDataBase.LoadFuncKeys;
const
  CFUNCKEYS = 'F01,F02,F03,F04,F05,F06,F07,F08,F09,F10';
var
  n: integer;
  sm: TShiftMode;
  FuncKeyTable: TLookupsTable;

  function DecodeFuncKey( KeyName: string;
                           var sm: TShiftMode;
                           var n: integer): boolean;
  var
    c1: char;
  begin { DecodeFuncKey }
    result := false;
    if Not Empty(KeyName) then
      begin
        c1 := KeyName[1];
        case c1 of
          'F': begin
                 sm := smNone;
                 n  := StrToInt(Copy(KeyName, 2, 2));
               end;
          'S': begin
                 sm := smShift;
                 n  := StrToInt(Copy(KeyName, 3, 2));
               end;
          'C': begin
                 sm := smCtrl;
                 n  := StrToInt(Copy(KeyName, 3, 2));
               end;
        end;
        result := true;
      end
  end;  { DecodeFuncKey }

  procedure AddKeys(Prefix: string; KeyList: string);
  var
    cp: integer;
    KeyName: string;
  begin { AddKeys }
    cp := Pos(',', KeyList);
    repeat
      if cp > 0 then
        begin
          KeyName := Copy(KeyList, 1, cp-1);
          KeyList := Copy(KeyList, cp+1, Length(KeyList)-cp);
        end
      else
        begin
          KeyName := KeyList;
          KeyList := '';
        end;
      cp := Pos(',', KeyList);
      with FuncKeyTable do
        begin
          Append;
          fldLookupCategory.AsString := Prefix + KeyName;
          Post;
        end;
    until cp = 0;
  end;  { AddKeys }

begin { LoadFuncKeys }
  FuncKeyTable := TLookupsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, cLOOKUPS, [], lc_FuncKey);
  FuncKeyTable.Active := true;
  with FuncKeyTable do
    begin
      First;
      while not Eof do
        begin
          if DecodeFuncKey(fldLookupName.AsString, sm, n) then
            SetFuncKey(sm, n, fldLookupValue.AsString);
          Next;
        end;
    end;
  FreeAndNil(FuncKeyTable);
end;  { LoadFuncKeys }

procedure TfrmPhotoDataBase.FormCreate(Sender: TObject);
  var
    n: integer;
    sm: TShiftMode;
    FolderPath: string;
    DateType: TDateTypes;
begin { TfrmPhotoDataBase.FormCreate }
{$IfDef debugging}
  Message('Entering FormCreate');
{$endIf}
  FolderPath := RemoveTrailingBackSlash(ExtractFilePath(ParamStr(0)));
  edtRootPath.Text := gRootPath;

  PageControl1.ActivePage := tabBrowse;
  tblPhotoTable := TPhotoTable.Create( self, CommonPhotoSettings.PhotoDBDatabaseFileName, cFILENAMES, []);
  if not OpenPhotoTable then
    OpenPhotoTable;         // try again after user (maybe) has set options

  FKMenuItems[smNone, 1]  := F1;
  FKMenuItems[smNone, 2]  := F2;
  FKMenuItems[smNone, 3]  := F3;
  FKMenuItems[smNone, 4]  := F4;
  FKMenuItems[smNone, 5]  := F5;
  FKMenuItems[smNone, 6]  := F6;
  FKMenuItems[smNone, 7]  := F7;
  FKMenuItems[smNone, 8]  := F8;
  FKMenuItems[smNone, 9]  := F9;
  FKMenuItems[smNone, MAXKEY] := F10;

  FKMenuItems[smShift, 1]  := ShiftF1;
  FKMenuItems[smShift, 2]  := ShiftF2;
  FKMenuItems[smShift, 3]  := ShiftF3;
  FKMenuItems[smShift, 4]  := ShiftF4;
  FKMenuItems[smShift, 5]  := ShiftF5;
  FKMenuItems[smShift, 6]  := ShiftF6;
  FKMenuItems[smShift, 7]  := ShiftF7;
  FKMenuItems[smShift, 8]  := ShiftF8;
  FKMenuItems[smShift, 9]  := ShiftF9;
  FKMenuItems[smShift, MAXKEY] := ShiftF10;

  FKMenuItems[smCtrl, 1]  := CtrlF1;
  FKMenuItems[smCtrl, 2]  := CtrlF2;
  FKMenuItems[smCtrl, 3]  := CtrlF3;
  FKMenuItems[smCtrl, 4]  := CtrlF4;
  FKMenuItems[smCtrl, 5]  := CtrlF5;
  FKMenuItems[smCtrl, 6]  := CtrlF6;
  FKMenuItems[smCtrl, 7]  := CtrlF7;
  FKMenuItems[smCtrl, 8]  := CtrlF8;
  FKMenuItems[smCtrl, 9]  := CtrlF9;
  FKMenuItems[smCtrl, MAXKEY] := CtrlF10;

  for sm := Low(TShiftMode) to High(TShiftMode) do
    for n := 1 to MAXKEY do SavedText[sm, n] := '';

  cbDateType.Clear;
  for DateType := Low(TDateTypes) to High(TDAteTypes) do
    cbDateType.Items.AddObject(DateTypesArray[DateType].Desc, TObject(DateType));

{$IfDef DHD}
  SetFuncKey(smNone, 1, 'Dan Dorrough');
  SetFuncKey(smNone, 2, 'Ruth Dorrough');
{$EndIf}
  LoadFuncKeys;
  cbDateType.ItemIndex := 0;
  Width := (Screen.Width * 2) div 3;
  Height := (Screen.Height * 2) div 3;
{$IfDef debugging}
  Message('Exiting FormCreate');
{$endIf}
end;  { TfrmPhotoDataBase.FormCreate }

procedure TfrmPhotoDataBase.ShowCalcFields(DataSet: TDataSet);
var
  Desc: string;
  bytes: Extended;
  mt: TMediaType;
  SrcFileName, Ext: string;
  UpdateReason: TUpdateReason;
  ReasonIsKnown, HasBeenUpdated: boolean;
{$IfDef DebugIndexBuild}
  azimuth, dist: DOUBLE;
{$EndIf}
begin
  with DataSet as TPhotoTable do
    if not ControlsDisabled then
      begin
        if not Assigned(fldFile_Size) then
          Exit;

        Bytes := fldFile_Size.AsFloat;

        lblFileSize.Caption := ScaledSize(Bytes);

        dbFilePath.Hint      := Format('Path_No=%d', [fldPath_No.AsInteger]);
        dbFilePath.ShowHint  := true;
        lblFilePath.Hint     := dbFilePath.Hint;
        lblFilePath.ShowHint := true;
        lblPathNo.Caption    := IntToStr(fldPath_No.AsInteger);
        if fldTextInFile.IsNull then
          cbDisplayTextInFile.Color := clBtnFace
        else
          cbDisplayTextInFile.Color := clYellow;

        dbUpdated.ShowHint := true;
        HasBeenUpdated := not (fldUpdated.IsNull);
        if HasBeenUpdated then
          begin
            ReasonIsKnown  := (not (fldUpdateReason.IsNull)) and
                              (fldUpdateReason.AsInteger <= Integer(High(TUpdateReason)));

            if ReasonIsKnown then
              begin
                UpdateReason   := TUpdateReason(fldUpdateReason.AsInteger);
                dbUpdated.Hint := Format('%3d: %s', [fldUpdateReason.AsInteger,
                                                     UpdateReasons[UpdateReason]]);
              end
            else
              dbUpdated.Hint := 'Update Reason not known';
          end
        else
          dbUpdated.Hint := 'Record Has not been updated';

        SrcFileName  := fldFILE_NAME.AsString;
        Ext          := ExtractFileExt(SrcFileName);
        mt           := MediaTypeFromExtension(Ext);
        cbShowScenes.Enabled := IsVideoMedia(mt) or IsAudioMedia(mt);

        if fldHasSceneInfo.AsBoolean then
          cbShowScenes.Color := clYellow
        else
          cbShowScenes.Color := clBtnFace;

        if fldLocationID.AsInteger > 0 then
          with LocationsTable do
            begin
              Desc := GetLocationDescription(fldLocationID.AsInteger); // get location description and position the table
              lblLocation.Text := Format('%d: [%d] %s',
                                       [fldLocationID.AsInteger,
                                        fldLocationSource.AsInteger,
                                        Desc]);
              if not Empty(fldState.AsString) then
                lblLocation.Text := lblLocation.Text + ', ' + fldState.AsString;

              lblLocation.Color := clBtnFace;
              lblLocation.Hint  := Format('(%10.7f,%10.7f)', [fldLatitude.AsFloat,    // coming from the Locations table
                                                              fldLongitude.AsFloat]);
              lblLocation.ShowHint := true;
            end
        else
          begin
            if (fldLatitude.AsFloat <> 0) or (fldLongitude.AsFloat <> 0) then
              begin
                lblLocation.Text := Format('(%10.7f,%10.7f)', [fldLatitude.AsFloat,    // coming from the photo table
                                                               fldLongitude.AsFloat]);
                lblLocation.Color := clYellow;
                lblLocation.Hint  := 'Location ID is not being used';
                lblLocation.ShowHint := true;
              end
            else
              begin
                lblLocation.Text := '';
                lblLocation.Color := clBtnFace;
                lblLocation.ShowHint := false;
              end;
          end;

        btnOpenFile.Enabled   := IsOpenableFile(mt); // fldHasSound.AsBoolean;
        if btnOpenFile.Enabled then
          btnOpenFile.Caption   := Format('Open %s file', [MediaInfoArray[mt].Desc]);

        btnLastRecord.Enabled := fPrevKey > 0;
        btnLastRecord.Hint    := Format('Go To Key =%d', [fPrevKey]);
        btnLastRecord.Enabled := true;
{$IfDef DebugIndexBuild}
        Dist                  := RhumbDistance(fLastLatitude,fLastLongitude,fldLatitude.AsFloat, fldLongitude.AsFloat, muMiles,
                                               Azimuth);
        lblStatus1.Caption       := Format('DEBUG: Dist key = %6.1n, Direction from previous = %6.1n (%s), Distance from previous = %6.1n',
                                 [fldDistance.AsFloat, Azimuth, AzimuthCode(Azimuth), Dist]);
        if Dist > 10.0 then
          lblStatus1.Color := clYellow
        else
          lblStatus1.Color := clBtnFace;
        fLastLatitude         := fldLatitude.AsFloat;
        fLastLongitude        := fldLongitude.AsFloat;
{$else}
        lblStatus1.Caption       := '';
{$EndIf}
      end;
end;

function CompareFunc(Item1, Item2: Pointer): Integer;
begin
  result := Integer(Item1) - Integer(Item2);
end;

procedure TfrmPhotoDataBase.CreateSubFoldersList(const FolderNumber: string);
var
  FolderNo: integer;
  TempFilePathsTable: TFilePathsTable;

  procedure AddSubFoldersOf(aParentNo: integer);
  var
    StartingIndex, NumberInFolder, idx: integer;
  begin { AddSubFoldersOf }
    with TempFilePathsTable do
      begin
        if Locate(PARENT_NO, aParentNo, []) then
          begin
            NumberInFolder := 0;
            StartingIndex  := fSubFoldersList.Count;
            while (not eof) and (fldPARENT_NO.AsInteger = aParentNo) do
              begin
                fSubFoldersList.Add(Pointer(fldPATH_NO.AsInteger));
                inc(NumberInFolder);
                Next;
              end;
            for idx := StartingIndex to StartingIndex+NumberInFolder-1 do
              AddSubFoldersOf(integer(fSubFoldersList[idx]));
          end;
      end;
  end;  { AddSubFoldersOf }

(*$IfDef FoldersList*)
  procedure DumpSubFoldersList;
  var
    i: integer;
    Temp: string;
    Lfn: string;
    OutFile: TextFile;
  begin
    Lfn := Format('c:\temp\SubFoldersOf-%d.txt', [FolderNo]);
    AssignFile(OutFile, Lfn);
    ReWrite(OutFile);
    WriteLn(OutFile, '   Idx: Path#: Path');
    for i := 0 to fSubFoldersList.Count-1 do
      with TempFilePathsTable do
        begin
          if Locate(PATH_NO, Integer(fSubFoldersList[i]), []) then
            Writeln(OutFile, i:6, ': ', Integer(fSubFoldersList[i]):5, ': ', fldFILE_PATH.AsString);
        end;
    CloseFile(OutFile);
    temp := Format('Notepad.exe %s', [Lfn]);
    FileExecute(temp, false);
  end;
(*$EndIf*)

begin { TfrmPhotoDataBase.CreateSubFoldersList }
  if (not Empty(FolderNumber)) and IsAllNumeric(FolderNumber) then
    begin
      try
        try
          FolderNo := StrToInt(edtFilePathNo.Text);
        except
          AlertFmt('Invalid folder number: %s', [FolderNumber]);
          Exit;
        end;
        FreeAndNil(fSubFoldersList);
        fSubFoldersList := TList.Create;
        fSubFoldersList.Add(Pointer(FolderNo));   // include the root folder

        TempFilePathsTable := TFilePathsTable.Create( self,
                                                      CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                      cFILEPATHS,
                                                      [optUseClient]);    // Use client side so that the index can reference two fields
        TempFilePathsTable.Active := true;

//      TempFilePathsTable.IndexFieldNames := Format('%s;%s', [PARENT_NO, PATH_NO]);
        // dhd 6/10/2019 - changed to the following because not all sub-folders were being found 
        TempFilePathsTable.IndexFieldNames := PARENT_NO;
        AddSubFoldersOf(FolderNo);

(*$IfDef FoldersList*)
        DumpSubFoldersList;
{$EndIf}        

        fSubFoldersList.Sort(CompareFunc);
      except
        on e:Exception do
          AlertFmt('Exception = %s', [e.message]);
      end;
    end;
end;  { TfrmPhotoDataBase.CreateSubFoldersList }


procedure TfrmPhotoDataBase.btnApplyClick(Sender: TObject);
var
  Cursor: TCursor;
  LowDate, HighDate: TDateTime;
  LowYear, HighYear: integer;
  KeyWords, Word1: string;
  wc, i: integer;
  aList: TStringList;
begin
  try
    LowDate := 0; HighDate := 0;
    LowYear := 0; HighYear := 0;
    fNrFiltered := 0;

    if not Empty(edtLowDate.Text) then
      begin
        Lowdate   := GetBestDate(CurrentDateType, edtLowDate.Text, hl_UseLow);
        if LowDate < 0 then
          raise EFilterError.CreateFmt('Invalid low date: %s (%10.5f)',
                                       [edtLowDate.Text, StrToDate(edtLowDate.Text)]);
      end;

    if not Empty(edtHighDate.Text) then
      begin
        HighDate  := GetBestDate(CurrentDateType, edtHighDate.Text, hl_UseHigh);
        if HighDate < 0 then
          raise EFilterError.CreateFmt('Invalid high date: %s', [edtHighDate.Text]);
      end;

    if (LowDate > 0) and (HighDate > 0) then
      begin
        if LowDate > HighDate then
          raise EFilterError.CreateFmt('Low date [%s] not be greater than high date [%s]', [edtLowDate.Text, edtHighDate.Text]);
      end;

    if not Empty(edtLowYear.Text) then
      begin
        LowYear := StrToInt(edtLowYear.Text);
        if LowYear < 100 then
          raise EFilterError.CreateFmt('Invalid low year: %d. It must be greater than 1900', [LowYear]);
      end;

    if not Empty(edtHighYear.Text) then
      begin
        HighYear := StrToInt(edtHighYear.Text);
        if HighYear < 100 then
          raise EFilterError.CreateFmt('Invalid high year: %d. It must be greater than 1900', [HighYear]);
      end;

    if (LowYear > 0) and (HighYear > 0) then
      begin
        if LowYear > HighYear then
          raise EFilterError.CreateFmt('Low year [%s] not be greater than high year [%s]', [edtLowYear.Text, edtHighYear.Text]);
      end;

    FreeAndNil(fTempSynonymsList);
    if cbUseSynonyms.Checked then
      begin
        fTempSynonymsList := TSynonymObject.Create(SynonymsTable);
        KeyWords          := edtKeyWords.Text;
        wc                := WordCountL(KeyWords, KEYWORDDELIMS);
        aList             := TStringList.Create;
        try
          for i := 1 to wc do
            begin
              Word1 := ExtractWordL(i, KeyWords, KEYWORDDELIMS);
              SynonymsList.SynonymsOf(Word1, aList);
              fTempSynonymsList.AddSynonyms(Word1, aList);
              aList.Clear;
            end;
        finally
          aList.Free;
          fTempSynonymsList.Sorted := true;
        end;
      end;

    if cbIncludeSubFolders.Checked and (not Empty(edtFilePathNo.Text))  then
      CreateSubFoldersList(edtFilePathNo.Text)
    else
      FreeAndNil(fSubFoldersList);

    Cursor := Screen.Cursor;
    try
      Screen.Cursor := crSQLWait;
      tblPhotoTable.MyDisableControls;
      tblPhotoTable.SetOrder(fCurrentOrder);
      tblPhotoTable.Filtered := true;
      CheckMarkMenuOrder(fCurrentOrder);
    finally
      tblPhotoTable.MyEnableControls;
      PhotoTableAfterScroll(tblPhotoTable);  // DEBUGGING - trying to get the ScenesTable to have up-to-date info  3/30/2016
      Screen.Cursor := Cursor;
    end;

    Enable_Buttons;

    fSelectedPhotoKey := 0;
    fChanged := [];
    fRefreshThumbnails := true;

  except
    on e:Exception do
      Alert(e.Message);
  end;
end;

procedure TfrmPhotoDataBase.Enable_Buttons;
begin
  with btnExpression.Font do
    if not Empty(fExpression) then
      Style := Style + [fsBold]
    else
      Style := Style - [fsBold];

  btnExpression.Hint := fExpression;
end;


procedure TfrmPhotoDataBase.First1Click(Sender: TObject);
begin
  tblPhotoTable.First;
end;

procedure TfrmPhotoDataBase.PostAndMove(MoveWhere: TMoveProc);
begin
  MoveWhere;
end;

(*
procedure TfrmPhotoDataBase.PostAndMove(MoveWhere: TMoveProc);
var
  Saved_RecNo: integer;
begin
  with tblPhotoTable do
    begin
//    Saved_RecNo := RecNo;
      if State in [dsEdit, dsInsert] then
        begin
          try
            Post;
          {  MoveWhere; } { 10/18/2017- still getting a double "Next" }
          { if RecNo = Saved_RecNo then }  { 11/30/2016- was forcing the "Next" button to be pressed twice
              MoveWhere
          { else
              SysUtils.Beep;} { 10/25/2016 I don't know why the RecNo has changed }
                              { 10/18/2017 Do RecNo get changed after a Post? }
          except
            on e:Exception do
              begin
                Cancel;
                Alert(e.Message);
              end;
          end;
        end
      else
        MoveWhere
    end;
end;
*)

procedure TfrmPhotoDataBase.Prev1Click(Sender: TObject);
begin
  if (PageControl1.ActivePage = tabPhoto) or (PageControl1.ActivePage = tabBrowse) then
    PostAndMove(tblPhotoTable.Prior) else
  if PageControl1.ActivePage = tabThumbNails then
//  ScrollBox1.ScrollBy(0, THUMBNAILHEIGHT+BORDER_WIDTH);  // changed to below 10/24/2017
    with ScrollBox1.VertScrollBar do
      Position := Position - (THUMBNAILHEIGHT+BORDER_WIDTH)
end;

procedure TfrmPhotoDataBase.Next1Click(Sender: TObject);
begin
  if (PageControl1.ActivePage = tabPhoto) or (PageControl1.ActivePage = tabBrowse) then
    PostAndMove(tblPhotoTable.Next) else
  if PageControl1.ActivePage = tabThumbNails then
//    ScrollBox1.ScrollBy(0, -THUMBNAILHEIGHT-BORDER_WIDTH);  // changed to below 10/24/2017
    with ScrollBox1.VertScrollBar do
      Position := Position + (THUMBNAILHEIGHT+BORDER_WIDTH);
end;

function TfrmPhotoDataBase.GetTimer: TTimer;
begin
  if not Assigned(fTimer) then
    fTimer := TTimer.Create(self);
  result := fTimer;
end;


procedure TfrmPhotoDataBase.Last1Click(Sender: TObject);
begin
  tblPhotoTable.Last;
end;

procedure TfrmPhotoDataBase.MarkNext1Click(Sender: TObject);
var
  Saved_RecNo: integer;
begin
  with tblPhotoTable do
    begin
      Saved_RecNo := RecNo;
      if State = dsBrowse then
        Edit;
      fldUPDATED.AsDateTime := Now;
      Post;
      if RecNo = Saved_RecNo then  // record we just posted may have been removed from the selection set moving us to the next record
        Next
      else
        PhotoTableAfterScroll(tblPhotoTable);
    end;
end;

procedure TfrmPhotoDataBase.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$IfDef debugging}
  Message('Entering FormClose');
{$endIf}
  if Assigned(tblPhotoTable) then
    with tblPhotoTable do
      begin
        if CopyRightsTable.State in [dsEdit, dsInsert] then
          CopyRightsTable.Post;

        if State in [dsEdit, dsInsert] then
          Post;

        Active := false;
      end;

  if FileExists(CommonPhotoSettings.PhotoDBDatabaseFileName) then
    CommonPhotoSettings.PhotoTableLastUsed := FileDateToDateTime(FileAge(CommonPhotoSettings.PhotoDBDatabaseFileName));
{$IfDef debugging}
  Message('Exiting FormClose');
{$endIf}
end;

procedure TfrmPhotoDataBase.InsertText(aComponent: TComponent; sm: TShiftMode; n: integer);
begin
  with tblPhotoTable do
    if not (State in [dsEdit]) then
      Edit;
  if aComponent is TCustomEdit then
    with aComponent as TCustomEdit do
      if n = 11 then
        SelText := fLastKeywords else
      if n = 12 then
        begin
          UNTESTED;
          SelText := GetKeyList(tblPhotoTable.PathAndFileName, fUpdateInfo.FileNameInfo);
        end
      else
        SelText := SavedText[sm, n] + '; ';
end;

procedure TfrmPhotoDataBase.CaptureText(aComponent: TComponent; n: integer);
begin
  if aComponent is TCustomEdit then
    with aComponent as TCustomEdit do
      begin
        SavedText[smNone, n] := SelText;
        SetFuncKey(smNone, n, SelText);
      end;
end;



procedure TfrmPhotoDataBase.F1Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 1);
end;

procedure TfrmPhotoDataBase.puKeyWordsPopup(Sender: TObject);
begin
  if Sender is TPopupMenu then
    fComponent := (Sender as TPopupMenu).PopupComponent
  else
    fComponent := nil;  // force an exception if used
end;

procedure TfrmPhotoDataBase.F2Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 2);
end;

procedure TfrmPhotoDataBase.F3Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 3);
end;

procedure TfrmPhotoDataBase.F4Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 4);
end;

procedure TfrmPhotoDataBase.F5Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 5);
end;

procedure TfrmPhotoDataBase.F6Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 6);
end;

procedure TfrmPhotoDataBase.F7Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 7);
end;

procedure TfrmPhotoDataBase.F8Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 8);
end;

procedure TfrmPhotoDataBase.F9Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 9);
end;

procedure TfrmPhotoDataBase.F10Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, MAXKEY);
end;

procedure TfrmPhotoDataBase.capF1Click(Sender: TObject);
begin
  CaptureText(fComponent, 1);
end;

procedure TfrmPhotoDataBase.capF2Click(Sender: TObject);
begin
  CaptureText(fComponent, 2);
end;

procedure TfrmPhotoDataBase.capF3Click(Sender: TObject);
begin
  CaptureText(fComponent, 3);
end;

procedure TfrmPhotoDataBase.capF4Click(Sender: TObject);
begin
  CaptureText(fComponent, 4);
end;

procedure TfrmPhotoDataBase.capF5Click(Sender: TObject);
begin
  CaptureText(fComponent, 5);
end;

procedure TfrmPhotoDataBase.capF6Click(Sender: TObject);
begin
  CaptureText(fComponent, 6);
end;

procedure TfrmPhotoDataBase.capF7Click(Sender: TObject);
begin
  CaptureText(fComponent, 7);
end;

procedure TfrmPhotoDataBase.capF8Click(Sender: TObject);
begin
  CaptureText(fComponent, 8);
end;

procedure TfrmPhotoDataBase.capF9Click(Sender: TObject);
begin
  CaptureText(fComponent, 9);
end;

procedure TfrmPhotoDataBase.capF10Click(Sender: TObject);
begin
  CaptureText(fComponent, MAXKEY);
end;

procedure TfrmPhotoDataBase.LogError(const Msg: string; const F1, F2: string);
begin
  Writeln(fLogFile, Msg);
  Writeln(fLogFile, '':10, F1);
  Writeln(fLogFile, '':10, F2);
end;

procedure TfrmPhotoDataBase.ScanForMissing(Update: boolean);
var
  Last_Path_No: integer; Lfn: string; Options: TPhotoTableOptions;
  temp: string; WasDeleted, WasEof: boolean; Missing, NrDeleted: integer;
  ConfirmEach, OK: boolean; mr: integer;
begin
  Options := [];
  if not Update then
    begin
      Options := Options + [optReadOnly];
      ConfirmEach := true; // keep compiler happy and be safe
    end
  else
    begin
      ConfirmEach := Yes('Confirm each deletion?');
      if ConfirmEach then
        begin
          FreeAndNil(frmConfirmEachPhoto);
          frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Delete')
        end;
    end;
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        Options);
  fLogPathFileName := CalcLogFileName('MissingFiles.txt');
  AssignFile(fLogFile, fLogPathFileName);
  Rewrite(fLogFile);
  NrDeleted := 0; Missing := 0;
  try
    fCount := 0;
    Update_Status('Scan beginning');

    with tempPhotoTable do
      begin
        OnFilterRecord  := PhotoTableFilterRecord;
        Filtered        := true;
        Active    := true;
        SetSelectivityParserExpression(fExpression);
        Last_Path_No := -1;
        First;
        Update_Status( tblFilePathsFILE_PATH.AsString);
        WriteLn(fLogFile, 'Non existant files as of ', DateTimeToStr(Now));
        WriteLn(fLogFile);
        WriteLn(fLogFile, 'Photo Folder: ', gRootPath);
        WriteLn(fLogFile, 'DataBase File: ', CommonPhotoSettings.PhotoDBDatabaseFileName);
        WriteLn(fLogFile);
        Writeln(fLogFile, 'Key':4, ': File Name');
        WasEof := false;
        while not (Eof or WasEof) do
          begin
            inc(fCount);
            Lfn := PathAndFileName;
            if not FileExists(Lfn) then
              begin
                Inc(Missing);
                if Update then
                  begin
                    WasDeleted := false;

                    if ConfirmEach then
                      with frmConfirmEachPhoto do
                        begin
                          PhotoFileName := PathAndFileName;
                          mr := ShowModal;
                          OK := mr = mrYes;
                          if mr = mrCancel then
                            break;
                        end
                    else
                      OK := true;

                    if OK then
                      begin
                        WasDeleted := DeletePhotoRecord(WasEof);
                        if WasDeleted then
                          begin
                            Writeln(fLogFile, Lfn);
                            inc(NrDeleted);
                          end
                        else
                          WriteLn(fLogFile, 'Unable to delete record: ', Lfn);
                      end;

                    if not (WasEof or WasDeleted) then
                      Next;
                  end
                else
                  begin
                    Writeln(fLogFile, fldKey.AsInteger:4, ': ', Lfn);
                    Next;
                  end;
              end
            else
              Next;
            if fldPATH_NO.AsInteger <> Last_Path_No then
              Update_Status( tblFilePathsFILE_PATH.AsString);
            Last_Path_No := fldPATH_NO.AsInteger;
          end;
        Update_Status('Scan 2 Complete');
      end;

  finally
    FreeAndNil(tempPhotoTable);
    FreeAndNil(frmConfirmEachPhoto);
    CloseFile(fLogFile);
    if (Missing > 0) or (NrDeleted > 0) then
      begin
        MessageFmt('%d missing records, %d deleted records', [Missing, NrDeleted]);
        temp := Format('Notepad.exe %s', [fLogPathFileName]);
        FileExecute(temp, false);
      end
    else
      if Update then
        MessageFmt('%d records scanned. No records or files were deleted', [fCount])
      else
        MessageFmt('%d records scanned. No missing files were found', [fCount]);
  end;
end;

procedure TfrmPhotoDataBase.ScanforMissingFiles1Click(Sender: TObject);
begin
  ScanForMissing(false);
end;

procedure TfrmPhotoDataBase.LastKeyWords1Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 11);
end;

procedure TfrmPhotoDataBase.GetAndSetPhotoDate(PathAndName: string; FileNameInfo: TFileNameInfo);
var
  Year, Month, Day: word;
  HasChanged: boolean;
  temp: string;
  UpdateInfo: TUpdateInfo;
  OK: boolean;
  creationTime, lastAccessTime,
  lastModificationTime: TDateTime;
begin
  Finalize(UpdateInfo);
  FillChar(UpdateInfo, SizeOf(UpdateInfo), 0);
  with tblPhotoTable do
    begin
      UpdateInfo.ImagePathName := PathAndName;
      UpdateInfo.FileName      := ExtractFileName(PathAndName);
      if fldWasScanned.AsBoolean then
        OK := YesFmt('File was from a scan not from a digital photo. Proceed?', [UpdateInfo.ImagePathName])
      else
        OK := TRUE;

      if OK then
        begin
          UpdateInfo.FileNameInfo  := FileNameInfo;
          with FileNameInfo do
            begin
(*
              if UseFileDateForPhotoDate then
                UpdateInfo.PhotoDateTime := FileDateToDateTime(FileAge(PathAndName)) else
              if UseModifiedDateForPhotoDate then
                begin
                  if GetFileTimes(PathAndName, creationTime, lastAccessTime,
                      lastModificationTime) then
                    UpdateInfo.PhotoDateTime := lastModificationTime
                end;
*)
              if (DateKinds * [dk_Date_Modified, dk_Date_Created, dk_Last_Access]) <> [] then
                begin
                  if GetFileTimes(PathAndName, creationTime, lastAccessTime,
                      lastModificationTime) then
                    begin
                      if dk_Date_Modified in DateKinds then
                        UpdateInfo.PhotoDateTime := lastModificationTime else
                      if dk_Date_Created in DateKinds then
                        UpdateInfo.PhotoDateTime := creationTime else
                      if dk_Last_Access in DateKinds then
                        UpdateInfo.PhotoDateTime := lastAccessTime;
                    end;
                end;
            end;
//        UpdateInfo.FileNameInfo.MinutesToAdd  := 0;  // don't know if this should be reset here
          if GetPhotoInfo(UpdateInfo) then
            begin
              Year := 0; Month := 0; Day := 0;
              PhotoDateToYearMonthDay(UpdateInfo.PhotoDateYYYYMMDD, Year, Month, Day);
              if not (State in [dsEdit]) then
                Edit;
              fldYear.AsInteger  := Year;
              fldMonth.AsInteger := Month;
              fldDay.AsInteger   := Day;
              if Day <> 0 then
                begin
                  temp := CalcPhotoDateString(Year, Month, Day, HasChanged);
                  if HasChanged then
                    begin
                      fldPhotoDate.AsString := Temp;
                      fldPhotoDateTime.AsDateTime := UpdateInfo.PhotoDateTime;
                    end;
                end;
            end;
        end;
    end;
end;


procedure TfrmPhotoDataBase.dbFileNameEnter(Sender: TObject);
begin
  fOldFileName := dbFileName.Text;
end;

procedure TfrmPhotoDataBase.tabPhotoResize(Sender: TObject);
begin
  SizePhoto;
end;

procedure TfrmPhotoDataBase.ScanDuplicates(ScanOrder: TCurrentOrder; DeleteWhat: TDeleteWhich);
var
  frmScanForDuplicatesOptions: TfrmScanForDuplicatesOptions;
  FileNamePattern: string;
  LogFileName: string;
  MatchName, MatchSize, MatchDate: boolean;
  NameIsOK, SizeIsOK, DateIsOK: boolean;
  Tolerance: Double;

  procedure ScanTheFile(aFileName: string; DeleteWhat: TDeleteWhich; DisplayPath: boolean);
  const
    cUPDATED = 12;
    cADDED   = 12;
    cLASTKEY = 6;
    cEXISTS = 3;
//  cLASTFILENAME = 30;
    cLASTFILESIZE = 10;
  var
    IsDuplicate: boolean;
    OutFile, DeletedRecList: TextFile;
    LastFileName, LastPathAndFileName: string;
    DeletedRecListFileName: string;
    LastFileNameLen: integer;
    fn1, fn2: string;
    LastFileSize: integer;
    LastRecno, SavedRecno: integer;
    LastKey: integer;
    Count, MaxCount, Fixed, NrToProcess: integer;
    DeletedRecordCount: integer;
    LastDateUpdate, LastDateAdded, LastDateTime: TDateTime;
    LastKeyWords: string;
    PhotoTableOptions: TPhotoTableOptions;

    procedure DeleteRecord;
    begin { DeleteRecord }
      with TempPhotoTable do
        begin
          WriteLn(DeletedRecList, fldKey.AsInteger:6, ' ', PathAndFileName);
          Delete;
          inc(DeletedRecordCount);
        end;
    end;  { DeleteRecord }

  begin { ScanTheFile }
    Update_Status('Preparing to scan for duplicates');
    AssignFile(Outfile, aFileName);
    ReWrite(OutFile);
    if DeleteWhat <> dwNone then
      begin
        DeletedRecListFileName := ExtractFilePath(aFileName) + 'DeletedRecs.txt';
        AssignFile(DeletedRecList, DeletedRecListFileName);
        ReWrite(DeletedRecList);
        WriteLn(DeletedRecList, 'Key':6, ' ', 'File path and name');
      end;

    Fixed           := 0;
    try
      PhotoTableOptions := [optNoSyncFilePathTable, optUseClient, optNoConfirmDelete];
      if (DeleteWhat = dwNone) then
        Include(PhotoTableOptions, optReadOnly);

      FreeAndNil(tempPhotoTable);
      tempPhotoTable := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            PhotoTableOptions);
      with tempPhotoTable do
        begin
          IndexFieldNames := Order[ScanOrder].IndexName;
          OnFilterRecord  := PhotoTableFilterRecord;
          Filtered        := true;
          Active          := true;
          SetSelectivityParserExpression(fExpression);
          try
            First;
            LastFileName    := '';
//          LastFolderNo    := -1;
            LastRecno       := -1;
            LastKey         := -1;
            Count           := 0;
            LastFileSize    := 0;
            LastDateUpdate  := -1;
            LastDateTime    := BAD_DATE;
            LastDateAdded   := BAD_DATE;
            LastKeyWords    := '';
            DeletedRecordCount := 0;
            MaxCount        := TotalRecordCount;
            if DisplayPath then
              LastFileNameLen := 90
            else
              LastFileNameLen := 30;
              
            writeln(OutFile, 'Updated':cUPDATED,
                             'Added':cADDED,
                             'Key#':cLASTKEY, ' ',
                             'EX': cEXISTS, ' ',
                             Padr('File Name', LastFileNameLen), ' ',
                             Padr('File Size', cLASTFILESIZE), ' ',
                             'Key Words');
            NrToProcess := 0;
            First;
            while not Eof do
              begin
                if (NrToProcess mod 100) = 0 then
                  Update_Status( Format('Duplicate Scan: Counting %d/%d', [NrToProcess, MaxCount]));
                Inc(NrToProcess);
                Next;
              end;

            First;

            while not Eof do
              begin
                if MatchName then
                  NameIsOK := (CompareText(fldFILE_NAME.AsString, LastFileName) = 0)
                else
                  NameIsOK := true;

                if MatchDate then
                  begin
                    if (Frac(fldPhotoDateTime.AsDateTime) = 0.0) or (Frac(LastDateTime) = 0.0) then
                      DateIsOK := false // do nothing
                    else  // If only the date (but not the time) is specified, skip this record
                      DateIsOK := fldPhotoDateTime.AsDateTime = LastDateTime;
                  end
                else
                  DateIsOK := true;

                if MatchSize then
                  if LastFileSize > 0 then
                    SizeIsOK := ((Abs(fldFile_Size.AsInteger - LastFileSize) / LastFileSize) * 100) < Tolerance
                  else
                    SizeIsOk := false
                else
                  SizeIsOK := true;

                IsDuplicate := NameIsOK and DateIsOK and SizeIsOK;

                if IsDuplicate then // this is a duplicate
                  begin
                    Inc(Fixed);
                    if DisplayPath then
                      begin
                        fn1 := LastPathAndFileName;
                        fn2 := PathAndFileName
                      end
                    else
                      begin
                        fn1 := LastFileName;
                        fn2 := fldFILE_NAME.AsString;
                      end;

                    writeln(OutFile, DateToStr(LastDateUpdate):cUPDATED,
                                     DateToStr(LastDateAdded):cADDED,
                                     LastKey:cLASTKEY, ' ',
                                     TF(FileExists(LastPathAndFileName)): cEXISTS, ' ',
                                     Padr(fn1, LastFileNameLen), ' ',
                                     LastFileSize:cLASTFILESIZE, ' ',
                                     PrintableOnly2(LastKeyWords));
                    writeln(OutFile, DateToStr(fldUPDATED.AsDateTime):cUPDATED,
                                     DateToStr(fldADDED.AsDateTime):cADDED,
                                     fldKey.AsInteger:cLASTKEY, ' ',
                                     TF(FileExists(PathAndFileName)): cEXISTS, ' ',
                                     Padr(fn2, LastFileNameLen), ' ',
                                     fldFILE_SIZE.AsInteger:cLASTFILESIZE, ' ',
                                     PrintableOnly2(fldKEY_WORDS.AsString));
                    writeln(OutFile);

                    SavedRecno := Recno;

                    case DeleteWhat of
                      dwOldestUpdate:
                        begin
                          if LastDateUpdate < fldUPDATED.AsDateTime then
                            begin
                              Recno := LastRecno;
                              DeleteRecord;
                            end
                          else
                            Delete;
                        end;

                      dwNewestUpdate:
                        begin
                          if fldUPDATED.AsDateTime >= LastDateUpdate then
                            begin
                              DeleteRecord;
                            end
                          else
                            begin
                              Recno := LastRecno;
                              DeleteRecord;
                            end;
                        end;

                      dwEarliestAdd:
                        if fldAdded.AsDateTime < LastDateAdded then
                          begin
                            DeleteRecord
                          end
                        else
                          begin
                            RecNo := LastRecno;
                            DeleteRecord;
                          end;

                      dwLatestAdd:
                        if fldAdded.AsDateTime >= LastDateAdded then
                          DeleteRecord
                        else
                          begin
                            RecNo := LastRecNo;
                            DeleteRecord;
                          end;

                      dwPattern:
                        begin
                          if Wild_Match(pchar(LastFileName), pchar(FileNamePattern), '*', '?', false) then
                            begin
                              RecNo := LastRecNo;
                              DeleteRecord;
                              RecNo := SavedRecNo;
                            end else
                          if Wild_Match(pchar(fldFILE_NAME.AsString), pchar(FileNamePattern), '*', '?', false) then
                            DeleteRecord
                        end;

                      dwForNonExistantFile:
                        begin
                          if not FileExists(PathAndFileName) then
                            DeleteRecord;

                          if not FileExists(LastPathAndFileName) then
                            begin
                              RecNo := LastRecNo;
                              DeleteRecord;
                            end;

                          RecNo := SavedRecNo;
                        end;
                    end;

                    if DeleteWhat <> dwNone then
                      begin                         // This dosen't make sense???
                        Recno := SavedRecno;        // back to where we were before the delete
                        if Recno <> SavedRecno then // if it was deleted
                          Prior;                    // move to the previous record
                      end;

                  end;
                Inc(Count);
                if (Count mod 10) = 0 then
                  Update_Status( Format('Duplicate Scan: Processed %d/%d', [Count, NrToProcess]));
                LastFileName    := fldFILE_NAME.AsString;
                LastPathAndFileName := PathAndFileName;
//              LastFolderNo    := fldPATH_NO.AsInteger;
                LastDateUpdate  := fldUPDATED.AsDateTime;
                LastDateAdded   := fldAdded.AsDateTime;
                LastKeyWords    := fldKEY_WORDS.AsString;
                LastDateTime    := fldPhotoDateTime.AsDateTime;
                LastFileSize    := fldFILE_SIZE.AsInteger;
                LastKey         := fldKey.AsInteger;
                LastRecno       := Recno;

                Next;
              end;
          finally
            FreeAndNil(tempPhotoTable);
          end;
        end;
    finally
      if Fixed > 0 then
        begin
          WriteLn(OutFile, Fixed, ' groups of duplicates were found');
          Case DeleteWhat of
            dwOldestUpdate:
              WriteLn(OutFile, 'The records with the OLDest update dates were deleted');
            dwNewestUpdate:
              WriteLn(OutFile, 'The records with the NEWest update dates were deleted');
            dwEarliestAdd:
              WriteLn(OutFile, 'The records with the EARLYiest add dates were deleted');
            dwLatestAdd:
              WriteLn(OutFile, 'The records with the LATEst add dates were deleted');
            dwPattern:
              WriteLn(OutFile, 'The records matching the pattern were deleted');
            dwForNonExistantFile:
              WriteLn(OutFile, 'The records for non-existant files were deleted');
          else
            WriteLn(OutFile, 'No records were deleted');
          end;
        end
      else
        WriteLn(OutFile, 'No duplicates were found');

      CloseFile(OutFile);
      if (Fixed > 0) and (DeleteWhat <> dwNone) then
        begin
          WriteLn(DeletedRecList, DeletedRecordCount, ' records were deleted');
          CloseFile(DeletedRecList);
          EditTextFile(DeletedRecListFileName);
        end;

      if Fixed > 0 then
        EditTextFile(aFileName)
      else
        Alert('No duplicates were found');
    end;

  end;  { ScanTheFile }

begin { TfrmPhotoDataBase.ScanDuplicates }
  frmScanForDuplicatesOptions := TfrmScanForDuplicatesOptions.Create(self);
  with frmScanForDuplicatesOptions do
    try
      TheScanOrder       := ScanOrder;
      TheFileNamePattern := 'img_*.jpg';
      DoDeleteRecords := dwNone;
      if ShowModal = mrOk then
        begin
          ScanOrder       := frmScanForDuplicatesOptions.TheScanOrder;
          FileNamePattern := frmScanForDuplicatesOptions.TheFileNamePattern;
          MatchName       := frmScanForDuplicatesOptions.cbSameFileName.Checked;
          MatchSize       := frmScanForDuplicatesOptions.cbSameFileSize.Checked;
          Tolerance       := frmScanForDuplicatesOptions.ovcTolerance.AsFloat;
          MatchDate       := frmScanForDuplicatesOptions.cbSameDate.Checked;
          LogFileName     := CalcLogFileName('Duplicates.txt');
          ScanTheFile(LogFileName, DoDeleteRecords, frmScanForDuplicatesOptions.cbDisplayPath.Checked);
        end;
    finally
      Free;
    end;
end;  { TfrmPhotoDataBase.ScanDuplicates }

procedure TfrmPhotoDataBase.ScanDuplicateFileSizes;
const
  WID_UPDATE   = 10;
  WID_Key      = 6;
  WID_FILENAME = 30;
  WID_PATH     = 30;
  WID_FILESIZE = 10;
var
  OutFile: TextFile;
  LastFileSize: integer;
  LastFileName: string;
  LastFilePath: string;
  LastKey: integer;
  Count, MaxCount, Fixed: integer;
  LastDateUpdate: TDateTime;
  LastKeyWords: string;
  PhotoTableOptions: TPhotoTableOptions;
  temp: string;
begin
  AssignFile(Outfile, aFileName);
  ReWrite(OutFile);
  Fixed           := 0;
  try
    PhotoTableOptions := [optNoSyncFilePathTable{, optUseClient}];
    Include(PhotoTableOptions, optReadOnly);
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          PhotoTableOptions);
    with tempPhotoTable do
      begin
        OnFilterRecord  := PhotoTableFilterRecord;
        Filtered        := true;
        Active          := true;
        SetSelectivityParserExpression(fExpression);
        SetOrder(coFileSize);
        try
          First;
          LastFileSize    := -1;
          Count           := 0;
          LastDateUpdate  := -1;
          LastKeyWords    := '';
          LastKey         := -1;
          MaxCount        := TotalRecordCount;
          writeln(OutFile, 'Updated':WID_UPDATE,
                           'Key#':WID_KEY, ' ',
                           Padr('File Path', WID_PATH), ' ',
                           Padr('File Name', WID_FILENAME), ' ',
                           'File Size':WID_FILESIZE, ' ',
                           'Key Words');
          while not Eof do
            begin
              if fldFile_Size.AsInteger = LastFileSize then // this is a duplicate
                begin
                  Inc(Fixed);
                  writeln(OutFile, DateToStr(LastDateUpdate):WID_UPDATE,
                                   LastKey:WID_KEY, ' ',
                                   Padr(LastFilePath, WID_PATH), ' ',
                                   Padr(LastFileName, WID_FILENAME), ' ',
                                   LastFileSize:WID_FILESIZE, ' ',
                                   LastKeyWords);
                  writeln(OutFile, DateToStr(fldUPDATED.AsDateTime):WID_UPDATE,
                                   fldKey.AsInteger:WID_KEY, ' ',
                                   Padr(ShortFilePath, WID_PATH), ' ',
                                   Padr(fldFILE_NAME.AsString, WID_FILENAME), ' ',
                                   FLDFile_Size.AsInteger:WID_FILESIZE, ' ',
                                   fldKEY_WORDS.AsString);
                  writeln(OutFile);

                end;
              Inc(Count);
              if (Count mod 100) = 0 then
                Update_Status( Format('Processed %d/%d', [Count, MaxCount]));

              LastFileName    := fldFILE_NAME.AsString;
              LastFileSize    := fldFile_Size.AsInteger;
              LastFilePath    := ShortFilePath;
              LastDateUpdate  := fldUPDATED.AsDateTime;
              LastKeyWords    := fldKEY_WORDS.AsString;
              LastKey         := fldKey.AsInteger;
              Next;
            end;
        finally
          FreeAndNil(tempPhotoTable);
        end;
      end;
  finally
    if Fixed > 0 then
      WriteLn(OutFile, Fixed, ' groups of duplicates were found')
    else
      WriteLn(OutFile, 'No duplicates were found');

    CloseFile(OutFile);

    if Fixed > 0 then
      begin
        temp := Format('Notepad.exe %s', [aFileName]);
        FileExecute(temp, false);
      end
    else
      Alert('No duplicates were found');
  end;
end;

function TfrmPhotoDataBase.TempFilePathsTable: TFilePathsTable;
begin
  if not Assigned(fTempFilePathsTable) then
    begin
      fTempFilePathsTable := TFilePathsTable.Create( self,
                                                     CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                     cFILEPATHS,
                                                     [optSortByPathName]);
      fTempFilePathsTable.Active := true;
    end;
  result := fTempFilePathsTable;
end;

procedure TfrmPhotoDataBase.BrowseFilePaths1Click(Sender: TObject);
begin
  if not Assigned(fBrowseFilePaths) then
    fBrowseFilePaths := TfrmPathBrowser.Create(self, TempFilePathsTable);
  fBrowseFilePaths.Show;
end;

destructor TfrmPhotoDataBase.Destroy;
begin
  CommonPhotoSettings.SaveSettings(CommonSettingsFileName(not gUsingTemporarySettingsFile));
  FreeAndNil(fTimer);
  FreeAndNil(fForm_Expression);
  FreeAndNil(fForm_Expression2);
  FreeAndNil(fTempFilePathsTable);
  FreeAndNil(tempPhotoTable);
  FreeAndNil(gGPSTrackingInfo);
  FreeAndNil(frmFileMover);
  FreeAndNil(fPhotoList);
  FreeAndNil(fFrmAudioRecorder);
  FreeAndNil(fSynonymsList);
  FreeAndNil(fTempSynonymsList);
  FreeAndNil(frmScanForDocuments);
  FreeAndNil(frmCopySelectedFiles);
{$IfDef dhd}
  FreeAndNil(fTracksBrowser);
  FreeAndNil(fTracksTable);
{$EndIf}
  FreeAndNil(fSubFoldersList);
  freeAndNil(fSynonymsTable);
  FreeAndNil(fEvaluationParser);
  FreeAndNil(fNoiseWordsTable);

  inherited;
end;

procedure TfrmPhotoDataBase.RecalcKeyWords1Click(Sender: TObject);
begin
  InsertText(ActiveControl, smNone, 12);
end;

(*
procedure TfrmPhotoDataBase.SetFileDate(const aFileName, aFolderPath: string;
                                              aDateTime: TDateTime;
                                              RecordExists: boolean;
                                              Update: boolean);
  var
    Lfn, FileBase: string;
    DOSError: integer; Year, Month, Day: word;
begin
  Lfn := AddBackSlashL(aFolderPath) + aFileName;
  if FileExists(Lfn) then
    begin
      try
        try
          DOSError := FileSetDate(Lfn, DateTimeToFileDate(fDateTime));
          if DOSError <> 0 then
            AlertFmt('Unable to set date for file %s [DOS Error=%d]', [lfn, DOSError])
          else
            begin
              if RecordExists then
                with tempPhotoTable do
                  begin
                    Edit;
                    fldTableDATE.AsDateTime := fDateTime;
                    if IsPhotoName(FileBase) then // from the camera
                      begin
                        DecodeDate(fDateTime, Year, Month, Day);
                        fldYear.AsInteger  := Year;
                        fldMonth.AsInteger := Month;
                      end;
                    if Update then
                      Post
                    else
                      Cancel;
                  end;
            end;
        except
          on e:Exception do
            AlertFmt('Unable to set date for file %s [%s]', [lfn, e.Message]);
        end;
      finally
      end;
    end
  else
    AlertFmt('File dosen''t exist: %s', [Lfn]);
end;

procedure TfrmPhotoDataBase.SetFileDates1Click(Sender: TObject);
  var
    DateStr: string;
begin
  if fDateTime <> 0 then
    DateStr := DateToStr(fDateTime)
  else
    DateStr := '';

  DateStr := GetString('Date to set', DateStr);
  if not Empty(DateStr) then
    begin
      try
        fDateTime                     := StrToDateTime(DateStr);
        tempPhotoTable                := TPhotoTable.Create( self,
                                                             gPhotoDB,
                                                             cFILENAMES,
                                                             [optNoSyncFilePathTable]);
        tempPhotoTable.IndexName      := FILE_NAME;
        tempPhotoTable.Active         := true;
        tempPhotoTable.OnFilterRecord := PhotoTableFilterRecord;
        tempPhotoTable.Filtered       := true;
        ProcessFolder(edtRootPath.Text, SetFileDate, true);
        lblStatus.Caption             := 'COMPLETE';
        tempPhotoTable.Active         := false;
        FreeAndNil(tempPhotoTable);
      except
        AlertFmt('Invalid date: %s', [DateStr]);
      end;
    end;
end;
*)

procedure TfrmPhotoDataBase.ScanandUpdate1Click(Sender: TObject);
begin
  ScanForMissing(true);
end;

procedure TfrmPhotoDataBase.CountSelectedRecords1Click(Sender: TObject);
var
  Count: integer;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
begin
  Update_Status('Counting records');
  SavedCursor := Screen.Cursor;
  Cursor      := crSQLWait;
  try
    Application.ProcessMessages;
    StartTime         := Now;
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          [optNoSyncFilePathTable, optReadOnly]);
    with tempPhotoTable do
      begin
{$IfDef dhd}
        onGetBoundariesTable := GetBoundariesTable;
{$EndIf}
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        First;
        Count          := 0;
        while not Eof do
          begin
            Inc(Count);
            Next;
          end;
        Application.ProcessMessages;
        EndTime := Now;
        ElapsedTime := (EndTime - StartTime);
        Update_Status( Format('Complete. %d records scanned. %s', [Count, ElapsedTimeToStr(ElapsedTime)]));
        Active         := false;
      end;
    FreeAndNil(tempPhotoTable);
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TfrmPhotoDataBase.CalculateFileSize1Click(Sender: TObject);
const
  CLUSTERSIZE = 4096;
  ONEKB = 1024;
  ONEMB = 1024 * ONEKB;
  ONEGB = 1024 * ONEMB;
var
  Count: integer;
  TotalFileSize, FilesSize: int64;
  FileName: string;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
begin
  Update_Status('Counting records');
  SavedCursor := Screen.Cursor;
  Cursor      := crSQLWait;
  try
    Application.ProcessMessages;
    StartTime         := Now;
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          [optNoSyncFilePathTable, optReadOnly]);
    TotalFileSize := 0;
    with tempPhotoTable do
      begin
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        First;
        Count          := 0;
        while not Eof do
          begin
            FileName  := PathAndFileName;
            FilesSize := ((FileSize64(FileName) + CLUSTERSIZE - 1) div CLUSTERSIZE) * CLUSTERSIZE;
            TotalFileSize := TotalFileSize + FilesSize;
            Inc(Count);
            if (Count mod 1000) = 0 then
              begin
                ElapsedTime := (Now - StartTime);
                Update_Status( Format('%d records scanned, %0.n bytes, %s',
                            [Count, TotalFileSize * 1.0, ElapsedTimeToStr(ElapsedTime)]));
              end;
            Next;
          end;
        Application.ProcessMessages;
        EndTime := Now;
        ElapsedTime := (EndTime - StartTime);
        Update_Status( Format('COMPLETE. %d records scanned, %0.n bytes, %s',
                    [Count, TotalFileSize * 1.0, ElapsedTimeToStr(ElapsedTime)]));
        Active         := false;
      end;
    FreeAndNil(tempPhotoTable);
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

(*
procedure TfrmPhotoDataBase.EditPhoto(const FileName: string);
  var
    FotoCanvasEXE, Params: string;
    temp: string;
begin
  FotoCanvasEXE := CommonPhotoSettings.PhotoEditingProgram;
  Params        := Format('"%s"', [FileName]);
  Temp          := FotoCanvasEXE + ' ' + Params;
  FileExecute(temp, true);
  SizePhoto;
end;
*)

procedure TfrmPhotoDataBase.EditPhoto;
begin
  with tblPhotoTable do
    begin
      EditPhotoUtil(PathAndFileName, CommonPhotoSettings.PhotoEditingProgram);
      SizePhoto;
      try
        UpdatePhotoInfo(ImgPhoto.RotatedBitMap);
      except
        on e:Exception do
          Alert(e.Message);
      end;
    end;
end;

function TfrmPhotoDataBase.GetOptionsFromUser: boolean;
var
  Saved_Cursor: TCursor;
begin
  with TfrmPhotoDBOptions.Create(self) do
    begin
{$IfDef dhd2}
      IsDHD := fIsDHD;
{$EndIf}
      PageControl1.ActivePage := tsPhotoDB;
      Caption := 'Photo DB Options - ' + gCommonSettingsFileName;
      result := ShowModal = mrOk;
      if result then
        begin
          try
            edtRootPath.Text := gRootPath;  // Show the new Root path
            CommonPhotoSettings.FolderNo := StrToInt(edtFolderNo.text);
            CommonPhotoSettings.SaveSettings(CommonSettingsFileName(true));
            UpdateFormCaption;
            Saved_Cursor  := screen.Cursor;
            screen.Cursor := crHourGlass;
            Application.ProcessMessages;
            try
              ClosePhotoTable;
              OpenPhotoTable;   // open using the new settings
              tblPhotoTable.MyDisableControls;
              btnApplyClick(nil);  // re-apply the filters
              tblPhotoTable.MyEnableControls;
            finally
              screen.Cursor := Saved_cursor;
            end;
          except
            on e:Exception do
              begin
                AlertFmt('Error while changing settings: %s', [e.Message]);
                result := false;
              end;
          end;
        end;
      Free;
    end;
end;


procedure TfrmPhotoDataBase.Options1Click(Sender: TObject);
begin
  GetOptionsFromUser;
end;

procedure TfrmPhotoDataBase.CheckMarkMenuOrder(NewOrder: TCurrentOrder);
begin
  case NewOrder of
    coNone:          RecNo1.Checked         := true;
    coFileName:      FileName1.Checked      := true;
    coPathName:      PathName1.Checked      := true;
    coPhotoDate:     PhotoDate1.Checked     := true;
    coPhotoDateTime: PhotoDateTime1.Checked := true;
    coFileSize:      FileSize1.Checked      := true;
    coDateAdded:     Added1.Checked         := true;
    coDateUpdated:   Updated1.Checked       := true;
    coLatitude:      begin
                       Latitude1.Checked    := true;
                       Ascending1.Checked   := true;
                     end;
    coLongitude:     begin
                       Longitude1.Checked   := true;
                       Ascending2.Checked   := true;
                     end;
    coLatitudeDec:   begin
                       Latitude1.Checked    := true;
                       Descending1.Checked  := true;
                     end;
    coLongitudeDec:  begin
                       Longitude1.Checked   := true;
                       Decsending2.Checked  := true;
                     end;
    coDistanceFromLocation: DistancefromLocation1.Checked := true;
    coNearness:      InOrderbyLatLon1.Checked := true;
  end;
end;

procedure TfrmPhotoDataBase.SetOrder(NewOrder: TCurrentOrder);
begin
  fCurrentOrder := NewOrder;
  if fChanged <> [] then
    btnApplyClick(nil);

  CheckMarkMenuOrder(NewOrder);

  tblPhotoTable.SetOrder(fCurrentOrder);
end;


procedure TfrmPhotoDataBase.FileName1Click(Sender: TObject);
begin
  SetOrder(coFileName);
end;

procedure TfrmPhotoDataBase.PathName1Click(Sender: TObject);
begin
  SetOrder(coPathName);
end;

procedure TfrmPhotoDataBase.CopytoClipboard1Click(Sender: TObject);
begin
  Assert(false, 'not implemented TRotatePicture.CopyToClipboard');
//  imgPhoto.CopyToClipboard;
end;

procedure TfrmPhotoDataBase.FillMonthYear1Click(Sender: TObject);
  const
    Delims = ' ,\-.()_';
  var
    Lfn: string; Count, MaxCount, Fixed: integer;
    aYear, aMonth, aDay: word; PathAndLfn: string;
    Year, Month, Day: integer;
    wc: integer;
    aWord, temp: string;
    HasChanged: boolean; i: integer;
    Age: integer; aDateTime: TDateTime;
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Month          := 0;
      Day            := 0;
      Fixed          := 0;
      First;
      while not eof do
        begin
          Lfn       := PathAndFileName;
          Age       := FileAge(Lfn);
          aDateTime := FileDateToDateTime(Age);

          Edit;

          DecodeDate(aDateTime, aYear, aMonth, aDay);

          // Look through the file name to see if it appears to contain a year and/or month
          HasChanged := false;
          wc := WordCountL(Lfn, DELIMS);
          if wc > 1 then
            begin
              for i := 1 to wc do
                begin
                  aWord := ExtractWordL(i, PathAndLfn, DELIMS);
                  if IsYear(aWord) then
                    begin
                      aYear := YearNumber(aWord);
                      if fldYear.AsInteger = 0 then
                        begin
                          fldYear.AsInteger := aYear;
                          HasChanged        := true;
                        end;
                    end else
                  if IsMonth(aWord) then
                    begin
                      Month              := MonthNumber(aWord);
                      if fldMonth.AsInteger = 0 then
                        begin
                          fldMonth.AsInteger := Month;
                          HasChanged         := true;
                        end;
                    end;
                end;
            end;

          if not HasChanged then
            begin
              UNTESTED;
//            if IsNameContainingDate(Lfn, Year, Month, Day, fFileNameInfo) then // from the camera
              if IsNameContainingDate(Lfn, Year, Month, Day, fUpdateInfo.FileNameInfo) then // from the camera
                begin
                  if fldYear.asInteger = 0 then
                    begin
                      fldYear.AsInteger  := aYear;
                      HasChanged := true;
                    end;
                  if fldMonth.asInteger = 0 then
                    begin
                      fldMonth.AsInteger := Month;
                      HasChanged := true;
                    end;
                  if fldDay.AsInteger = 0 then
                    begin
                      fldDay.AsInteger := Day;
                      HasChanged := true;
                    end;
                end;
            end;

          temp := CalcPhotoDateString(fldYear.AsInteger,
                                      fldMonth.AsInteger,
                                      fldDay.AsInteger,
                                      HasChanged);
          if HasChanged then
            fldPhotoDate.AsString := temp;

          if HasChanged then
            begin
              Post;
              inc(Fixed);
            end
          else
            Cancel;

          Next;
          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
        end;
    end;
  try
    Update_Status( Format('COMPLETE. %d recs updated', [Fixed]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
  end;
end;

procedure TfrmPhotoDataBase.FillFileSize1Click(Sender: TObject);
  var
    Lfn: string; FileSize, Count, MaxCount, Fixed: integer;
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Fixed          := 0;
      First;
      while not eof do
        begin
          Lfn      := PathAndFileName;
          FileSize := FileSize32(Lfn);
          if (fldFile_Size.AsInteger <> FileSize) then
            begin
              Edit;
              fldFile_Size.AsInteger := FileSize;
              fldUpdateReason.AsInteger := integer(ur_FillFileSize); // tested
              Post;
              inc(Fixed);
            end;
          Next;
          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
        end;
    end;
  try
    Update_Status( Format('COMPLETE. %d recs updated', [Fixed]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    tblPhotoTable.ReFresh;
  end;
end;

procedure TfrmPhotoDataBase.FileSize1Click(Sender: TObject);
begin
(*
  fCurrentOrder := coFileSize;
  tblPhotoTable.SetOrder(fCurrentOrder);
*)
  SetOrder(coFileSize); // changed from above on 8/12/2017
end;

procedure TfrmPhotoDataBase.ResetMediaClasses;
var
  mc: TMediaClass;
begin
  clbMediaClasses.Checked[0] := true; // select all media
  for mc := Succ(Low(TMediaClass)) to High(TMediaClass) do
    clbMediaClasses.Checked[ord(mc)] := false;
end;


procedure TfrmPhotoDataBase.btnClearClick(Sender: TObject);
begin
  edtStringInPath.Text := '';
  edtFileNames.Text  := '';
  edtLowDate.Text    := '';
  edtHighDate.Text   := '';
  edtLowYear.Text    := '';
  edtHighYear.Text   := '';
  edtLowMonth.Text   := '';
  edtHighMonth.Text  := '';
  edtLowDay.Text     := '';
  edtHighDay.Text    := '';
  edtKeyWords.Text   := '';
  edtCopyCode.Text   := '';
  edtFilePathNo.Text := '';
  cbNot.Checked      := false;
  cbHasSound.Checked := false;
  cbUnprocessedOnly.Checked := false;
  cbHasLocationInfoInEXIF.Checked  := false;
  cbLocationID.Checked             := false;
  cbFileIsMissing.Checked          := false;
  cbScanComments.Checked           := false;
  cbScanFile.Checked               := false;
  cbHasSceneInfo.Checked           := false;
  cbScanSceneKeyWords.Checked      := false;
  cbScanSceneDates.Checked         := false;
  ResetMediaClasses;
  cbDateType.ItemIndex := 0;
  tblPhotoTable.ClearParser;
  FreeAndNil(fForm_Expression);
  FreeAndNil(fSubFoldersList);
  fExpression         := '';
  Enable_Buttons;
  fChanged            := [];
end;

procedure TfrmPhotoDataBase.FileMover;
begin { FileMover }
  frmFileMover := TfrmFileMover.CreateParented(tabMover.Handle);
  with frmFileMover do
    begin
      Top    := 0;
      Left   := 0;
      Width  := tabMover.Width;
      Height := tabMover.Height;
      Show;
    end;
end;  { FileMover }

procedure TfrmPhotoDataBase.ReDisplayThumbnails(ReScan: boolean);
var
  FileName,
  ThumbNailFileName: string;
  CurrentTop, CurrentLeft, RightMostEdge: integer;
  TallestPhotoInRow, MaxPhotosPerRow, PhotosInCurrentRow: integer;
  SavedCursor: TCursor;

  function PageFull: boolean;
  begin
    result := (fThumbNailsShowing >= udMaxPhotos.Position);
  end;

  procedure SlowWay;

    procedure AddThumbNail(Key: integer; FileName, PhotoDate, KeyWords, ThumbNailFileName: string);
    var
      Image: TImage;
    begin { AddThumbNail }
      if FileExists(ThumbNailFileName) then
        begin
          Image           := TImage.Create(self);
          Image.Parent    := ScrollBox1;
          try
            Image.Picture.LoadFromFile(ThumbNailFileName);
            Image.Width       := THUMBNAILWIDTH;
            Image.Height      := THUMBNAILHEIGHT;
            Image.Hint        := Format('%d: [%s][%s]: %s', [Key, FileName, PhotoDate, KeyWords]);
            Image.ShowHint    := true;
            Image.PopupMenu   := puThumbNail;
            Image.Tag         := Key;
            Image.Top         := CurrentTop;
            Image.Left        := CurrentLeft;
            Image.OnMouseDown := ImageMouseDown;
            Image.OnMouseMove := ImageMouseMove;
            Image.OnDblClick  := ImageDoubleClick;
            PhotoList.Add(Image);
            Inc(PhotosInCurrentRow);

            Inc(CurrentLeft, THUMBNAILWIDTH + BORDER_WIDTH);
            if CurrentLeft > RightMostEdge then
              RightMostEdge := CurrentLeft;
            if Image.Picture.Height > TallestPhotoInRow then  // changed to lines below because some video thumbnails were too tall
              TallestPhotoInRow := Image.Picture.Height;

            if (CurrentLeft + THUMBNAILWIDTH) >= ScrollBox1.ClientWidth then
              begin
                if PhotosInCurrentRow > MaxPhotosPerRow then
                  MaxPhotosPerRow := PhotosInCurrentRow;

                PhotosInCurrentRow := 0;
                if TallestPhotoInRow > 0 then
                  Inc(CurrentTop, TallestPhotoInRow + BORDER_WIDTH)
                else
                  Inc(CurrentTop, THUMBNAILHEIGHT + BORDER_WIDTH);

                CurrentLeft       := 0;
                TallestPhotoInRow := 0;

                if CurrentTop > tabThumbNails.ClientHeight then
    //              tabThumbNails.ClientHeight := CurrentTop
                else
                  Application.ProcessMessages;
              end;

            inc(fThumbNailsShowing);
          except
            on e:Exception do
              begin
                AlertFmt('Unable to load file: "%s" [%s]', [ThumbNailFileName, e.Message]);
                Image.Free;
              end;
          end;
        end
      else
        AlertFmt('ThumbNail file "%s" not found', [ThumbNailFileName]);
    end;  { AddThumbNail }

  begin { SlowWay }
    if not fReDisplayingThumbnails then
      begin
        fReDisplayingThumbnails := true;
        SavedCursor   := Screen.Cursor;
        Screen.Cursor := crSQLWait;
        try
          fThumbNailsShowing := 0; CurrentTop := BORDER_WIDTH; CurrentLeft := 0; PhotosInCurrentRow := 0;
          TempPhotoTable := TPhotoTable.Create( self,
                                                CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                cFILENAMES,
                                                [optNoSyncFilePathTable, optReadOnly, optNoCopyRightsTable]);
{$IfDef dhd}
          TempPhotoTable.onGetBoundariesTable := GetBoundariesTable;
{$EndIf}
          TempPhotoTable.Active := true;
          TempPhotoTable.SetSelectivityParserExpression(fExpression);
          TempPhotoTable.SetOrder(fCurrentOrder);
          try
            FreeAndNil(fPhotoList);
            ScrollBox1.Height := tabThumbNails.ClientHeight;  // initialize height to tab height
            ScrollBox1.Width  := tabThumbNails.ClientWidth;   // initialize height to tab Width
            with TempPhotoTable do
              begin
                OnFilterRecord := PhotoTableFilterRecord;
                Filtered       := true;
                TallestPhotoInRow := 0;
                RightMostEdge     := 0;
                MaxPhotosPerRow   := 0;
                First;
                while (not (Eof or PageFull)) do
                  begin
                    ThumbNailFileName := ThumbNailPathAndName(TempPhotoTable.PathAndFileName);
                    FileName          := TempPhotoTable.fldFILE_NAME.AsString;
                    if FileExists(ThumbNailFileName) then
                      AddThumbNail(fldKey.AsInteger, FileName, fldPhotoDate.AsString, fldKey_Words.AsString, ThumbNailFileName);
                    Next;
                  end;
                lblStatus2.Caption := Format('Eof=%d, Bof=%d, PageFull=%d, Displayed=%d',
                                             [ord(Eof), ord(Bof),
                                              ord(PageFull), fThumbNailsShowing]);
              end;
          finally
            FreeAndNil(TempPhotoTable);
          end;
        finally
          Screen.Cursor := SavedCursor;
          fRedisplayingThumbnails := false;
        end;
      end;
  end;  { SlowWay }

  procedure FastWay;
  var
    i: integer;

    procedure RepositionThumbNail(Image: TImage);
    begin { RepositionThumbNail }
        begin
          Image.Top       := CurrentTop;
          Image.Left      := CurrentLeft;
          Inc(PhotosInCurrentRow);

          Inc(CurrentLeft, THUMBNAILWIDTH + BORDER_WIDTH);
          if CurrentLeft > RightMostEdge then
            RightMostEdge := CurrentLeft;
          if Image.Picture.Height > TallestPhotoInRow then
            TallestPhotoInRow := Image.Picture.Height;

          if (CurrentLeft + THUMBNAILWIDTH) >= ScrollBox1.ClientWidth then
            begin
              if PhotosInCurrentRow > MaxPhotosPerRow then
                MaxPhotosPerRow := PhotosInCurrentRow;

              PhotosInCurrentRow := 0;
              if TallestPhotoInRow > 0 then
                Inc(CurrentTop, TallestPhotoInRow + BORDER_WIDTH)
              else
                Inc(CurrentTop, THUMBNAILHEIGHT + BORDER_WIDTH);

              CurrentLeft       := 0;
              TallestPhotoInRow := 0;

              if CurrentTop > tabThumbNails.ClientHeight then
  //              tabThumbNails.ClientHeight := CurrentTop
              else
                Application.ProcessMessages;
            end;

          inc(fThumbNailsShowing);
        end;
    end;  { RepositionThumbNail }

  begin { Fastway }
    if not fReDisplayingThumbnails then
      begin
        fReDisplayingThumbnails := true;
        SavedCursor   := Screen.Cursor;
        Screen.Cursor := crSQLWait;

        fThumbNailsShowing := 0; CurrentTop := BORDER_WIDTH; CurrentLeft := 0; PhotosInCurrentRow := 0;
        try
          ScrollBox1.Height := tabThumbNails.ClientHeight;  // initialize height to tab Height
          ScrollBox1.Width  := tabThumbNails.ClientWidth;   // initialize height to tab Width
//          Scrollbox1.VertScrollBar.Position := 0; // 10/20/2016, 10/24/2017
            begin
              TallestPhotoInRow := 0;
              RightMostEdge     := 0;
              MaxPhotosPerRow   := 0;

              for i := 0 to PhotoList.Count-1 do
                RepositionThumbNail(TImage(fPhotoList[i]));

              lblStatus2.Caption := Format('PageFull=%d, Displayed=%d',
                                           [ord(PageFull), fThumbNailsShowing]);
            end;

        finally
          Screen.Cursor := SavedCursor;
          fRedisplayingThumbnails := false;
        end;
      end;
  end;  { FastWay }

begin { TfrmPhotoDataBase.ReDisplayThumbnails }
  if ReScan then
    SlowWay
  else
    FastWay;

  fMaxPhotosChanged  := false;
  fRefreshThumbnails := false;
end;  { TfrmPhotoDataBase.ReDisplayThumbnails }

procedure TfrmPhotoDataBase.PageControl1Change(Sender: TObject);
var
  CompleteRefresh: boolean;
  MaxPhotosStr: string;
begin
  if Assigned(frmFileMover) then
    begin
      if frmFileMover.SelectedPhotoKey <> 0 then
        fSelectedPhotoKey := frmFileMover.SelectedPhotoKey;
      with frmFileMover do
        begin
          FreeListViewItems;
          FreeTreeViewItems;
          if FilesWereRenamed or FilesWereMoved then
            begin
              lblStatus.Caption := 'Files were moved or renamed. Refreshing';
              Application.ProcessMessages;
              tblPhotoTable.Refresh;
            end;
        end;
      FreeAndNil(frmFileMover);  // recreate it each time that we use it
    end;

  if PageControl1.ActivePage = tabPhoto then
    begin
      if fChanged <> [] then
        btnApplyClick(nil);

      if fSelectedPhotoKey > 0 then
        if not tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then
          begin
            AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
            Exit;
          end;
      fSelectedPhotoKey := 0;
      fSelectedImage    := nil;

      SizePhoto;
    end else
  if PageControl1.ActivePage = tabMover then
    FileMover else
  if PageControl1.ActivePage = tabThumbNails then
    begin
      fSelectedPhotoKey := 0;
      ScrollBox1.Height := tabThumbNails.Height;  // initialize height to tab height
      ScrollBox1.Width  := tabThumbNails.Width;   // initialize height to tab width
      ScrollBox1.Top    := SCROLLBOX_TOP;

      CompleteRefresh := false;
      if fRefreshThumbnails or (fChanged <> []) then
        begin
          MaxPhotosStr  := edtMaxPhotos.Text;
          if edtMaxPhotos.Text <> '100' then
            CompleteRefresh := Yes('Refresh the thumbnails?')
          else
            if GetString('Max Photos to Display', 'Max Photos', MaxPhotosStr, 5) then
              begin
                edtMaxPhotos.Text := MaxPhotosStr;
                CompleteRefresh := true; // Yes('Refresh the thumbnails?');
              end
        end
      else
        CompleteRefresh := false;

      ReDisplayThumbNails(CompleteRefresh);
    end;
end;

procedure TfrmPhotoDataBase.SaveAs1Click(Sender: TObject);
  var
    ExistingFileName: string;
begin
  ExistingFileName := tblPhotoTable.PathAndFileName;
  With SaveDialog1 do
    begin
      FileName := ExtractFileName(ExistingFileName);
      Filter   := 'Image Files (*.jpg)|*.jpg';
      DefaultExt := 'JPG';
      InitialDir := fSaveFolder;
      Options    := [ofOverwritePrompt];
      if Execute then
        begin
          if FileExists(FileName) then
            begin
              if YesFmt('%s already exists. Do want to overwrite it?', [FileName]) then
                DeleteFile(FileName)
              else
                Exit;
            end;
          if CopyFile(PAnsiChar(ExistingFileName), PAnsiChar(FileName), false) then
            begin
              MessageFmt('File saved to %s', [FileName]);
              fSaveFolder := ExtractFilePath(FileName);
            end;
        end;
    end;
end;

procedure TfrmPhotoDataBase.AboutPhotoDB1Click(Sender: TObject);
begin
  with TAboutBox.Create(self) do
    begin
      ShowModal;
      Free;
    end;
end;

procedure TfrmPhotoDataBase.imgPhotoStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  Assert(false, 'imgPhotoStartDrag');
(*
  DropFileSource := TDropFileSource.Create(self);
  try
    DropFileSource.Files.Add(tblPhotoTable.PathAndFileName);
    DropFileSource.Execute;
  finally
    DropFileSource.Free;
  end;
*)
end;

procedure TfrmPhotoDataBase.imgPhotoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  DropFileSource: TDropFileSource;
begin
  if fAlreadyDragging then exit;
  Shift := (Shift * [ssLeft,ssRight]);

  //Make sure a mouse button is pressed before starting the drag ...
  //and the mouse has moved at least 10 pixels
  //(DragPoint is set in the ListView1MouseDown event)
  if  (Shift = []) or
      ((abs(fDragPoint.X - X) < 10) and (abs(fDragPoint.Y - Y) < 10)) then
    exit;

  with tblPhotoTable do
    if (Eof and Bof) then exit;

  DropFileSource := TDropFileSource.Create(self);
  try
    //Fill DropSource1.Files with selected files in ListView1
    DropFileSource.Files.Add(tblPhotoTable.PathAndFileName);
    fAlreadyDragging := true;

    //Start the dragdrop...
    //Note: DropFileSource1.DragTypes = [dtCopy]
    DropFileSource.execute;
    //Note: Execute does not return until the drop has finished -
    //either sucessfully with a copy, move or link operation or cancelled.

    fAlreadyDragging := false;
  finally
    DropFileSource.Free;
  end;
end;

procedure TfrmPhotoDataBase.imgPhotoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fDragPoint := Point(X,Y);
end;

procedure TfrmPhotoDataBase.ShiftF1Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 1);
end;

procedure TfrmPhotoDataBase.ShiftF2Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 2);
end;

procedure TfrmPhotoDataBase.ShiftF3Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 3);
end;

procedure TfrmPhotoDataBase.ShiftF4Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 4);
end;

procedure TfrmPhotoDataBase.ShiftF5Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 5);
end;

procedure TfrmPhotoDataBase.ShiftF6Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 6);
end;

procedure TfrmPhotoDataBase.ShiftF7Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 7);
end;

procedure TfrmPhotoDataBase.ShiftF8Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 8);
end;

procedure TfrmPhotoDataBase.ShiftF9Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 9);
end;

procedure TfrmPhotoDataBase.ShiftF10Click(Sender: TObject);
begin
  InsertText(ActiveControl, smShift, 10);
end;

procedure TfrmPhotoDataBase.CtrlF1Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 1);
end;

procedure TfrmPhotoDataBase.CtrlF2Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 2);
end;

procedure TfrmPhotoDataBase.CtrlF3Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 3);
end;

procedure TfrmPhotoDataBase.CtrlF4Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 4);
end;

procedure TfrmPhotoDataBase.CtrlF5Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 5);
end;

procedure TfrmPhotoDataBase.CtrlF6Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 6);
end;

procedure TfrmPhotoDataBase.CtrlF7Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 7);
end;

procedure TfrmPhotoDataBase.CtrlF8Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 8);
end;

procedure TfrmPhotoDataBase.CtrlF9Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, 9);
end;

procedure TfrmPhotoDataBase.CtrlF10Click(Sender: TObject);
begin
  InsertText(ActiveControl, smCtrl, MAXKEY);
end;

procedure TfrmPhotoDataBase.PrintKeyAssignments1Click(Sender: TObject);
const
  FW = 25;
  FW0 = FW-2;
var
  n: integer;
  OutFile: TextFile;
  lfn, temp: string;

  procedure WriteField(sm: TShiftMode; n: integer);
  begin
    Write(OutFile, Padr(SavedText[sm, n], FW0), '  ');
  end;

begin
  lfn := TempPath + 'Keys.txt';
  AssignFile(OutFile, lfn);
  ReWrite(OutFile);
  Writeln(OutFile, '':4,
                   Padr('F1', FW0), '  ',
                   Padr('Shift+F1', FW0), '  ',
                   Padr('Ctrl+F1', FW0), '  ');
  try
    for n := 1 to 10 do
      begin
        Write(OutFile, n:2, ': ');
        WriteField(smNone, n);
        WriteField(smShift, n);
        WriteField(smCtrl, n);
        WriteLn(OutFile);
      end;
  finally
    CloseFile(OutFile);
  end;
  temp := Format('Notepad.exe "%s"', [Lfn]);
  FileExecute(temp, false);
end;

procedure TfrmPhotoDataBase.SetFileDatefromEXIF1Click(Sender: TObject);
var
  Lfn: string; Count, MaxCount, Fixed: integer;
//temp: string;
  ImgData: TimgData;
  aDateTime: TDateTime;
  aFileDate: integer;
  OldRecNo: integer;
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  ImgData := TImgData.Create();
  try
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Fixed          := 0;
      First;
      while not eof do
        begin
          OldRecNo := RecNo;
          Lfn       := PathAndFileName;

          // Parse the EXIF data to get a date
          if ImgData.ProcessFile(Lfn) then
            if ImgData.HasEXIF then
              begin
//              Temp := ImgData.ExifObj.DateTime;
//              aDateTime := ExifDateToDateTime(Temp);
                aDateTime := ImgData.ExifObj.GetImgDateTime;
                if aDateTime <> BAD_DATE then
                  begin
                    aFileDate := DateTimeToFileDate(aDateTime);
                    FileSetDate(Lfn, aFileDate);
                    inc(Fixed);
                  end;
              end;

          if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
            Next;

          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
        end;
    end;
    Update_Status( Format('COMPLETE. %d file dates updated', [Fixed]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    ImgData.Free;
  end;
end;

procedure TfrmPhotoDataBase.dbCopyCodeChange(Sender: TObject);
{$IfDef dhd}
var
  b: boolean;
{$EndIf}
begin
{$IfDef dhd}
  if dbCopyCode.Modified then
    begin
      b := SameText(Trim(dbCopyCode.Text), 'DHD') or
           SameText(Trim(dbCopyCode.Text), 'DBD');
      tblPhotoTable.fldPRIVATE.AsBoolean := NOT b;
    end;
{$EndIf}
end;

procedure TfrmPhotoDataBase.AddKeyWordToSelectedPhotos1Click(
  Sender: TObject);
  const
    Delims = ' ,\-.()_';
  var
    Count, MaxCount, Fixed: integer;
    ReqKeyWord, KeyWords: string;
    wc: integer;
    aWord: string;
    HasChanged, HasKeyWord: boolean; i: integer;
    LogFileName, Temp: string;
begin
  // force every selected photo to include the specified key word
  if not GetString('Get String Value', 'Required Key Word', ReqKeyWord) then
    exit;

  FreeAndNil(tempPhotoTable);
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  LogFileName := CalcLogFileName('KeyWord Changes.txt');
  AssignFile(fLogfile, LogFileName);
  ReWrite(fLogFile);
  WriteLn(fLogFile, 'KeyWord Changes');
  Count          := 0;
  Fixed          := 0;
  try
    with TempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        MaxCount       := TotalRecordCount;
        First;
        while not eof do
          begin
            KeyWords := TempPhotoTable.fldKEY_WORDS.AsString;
            Edit;

            HasChanged := false;
            wc := WordCountL(KeyWords, DELIMS);
            HasKeyWord := false;
            if wc > 1 then
              begin
                for i := 1 to wc do
                  begin
                    aWord := ExtractWordL(i, KeyWords, DELIMS);
                    if SameText(aWord, ReqKeyWord) then
                      begin
                        HasKeyWord := true;
                        break;
                      end;
                  end;
              end;

            if not HasKeyWord then
              HasKeyWord := Pos(UpperCase(ReqKeyWord), UpperCase(KeyWords)) > 0;

            if not HasKeyWord then
              begin
                KeyWords   := KeyWords + '; ' + ReqKeyWord;
                HasChanged := true;
              end;

            if HasChanged then
              begin
                WriteLn(fLogFile, 'Before: ', fldKey_Words.AsString);
                Writeln(fLogFile, 'After:  ', KeyWords);
                WriteLn(fLogFile);

                fldKEY_WORDS.AsString := KeyWords;
                fldUpdateReason.AsInteger := integer(ur_KeywordsChanged); // tested
                Post;
                inc(Fixed);
              end
            else
              Cancel;

            Next;
            inc(Count);
            Update_Status( Format('Processed/Fixed/Max %d/%d/%d', [Count, Fixed, MaxCount]));
          end;
      end;
  finally
    Update_Status( Format('COMPLETE. %d records processed, %d recs updated', [Count, Fixed]));
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    tblPhotoTable.Refresh;

    CloseFile(fLogFile);
    temp := Format('Notepad.exe "%s"', [LogFileName]);
    FileExecute(temp, false);
  end;
end;

function TfrmPhotoDataBase.GenerateTheFileName( FileNamePattern: string;
                              const FilePath, FileName: string;
                              FolderNumber: integer;
                              PhotoDate, PhotoKey: string;
                              PhotoDateTime: TDateTime;
                              LocationsDescription: string;
                              BasedOnLatLon: TBasedOnLatLon;
                              Latitude, Longitude: double;
                              Comment: string;
                              Key_Words: string;
                              SequenceNo: integer): string;

var
  i: integer;
  temp: string;
  ch: char;
  FormatSettings: TFormatSettings;

begin { GenerateTheFileName }
  FileNamePattern := UpperCase(FileNamePattern);
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.LongTimeFormat := 'hhnnss';
  result := '';
  i := 0;
  while i < Length(FileNamePattern) do
    begin
      inc(i);
      ch := FileNamePattern[i];
      if ch = '%' then
        begin
          inc(i);
          ch := FileNamePattern[i];
          case ch of
            'Y': { photo year }
                 temp := PadR(Copy(PhotoDate, 1, 4), 4, '0');
            'M': { photo month }
                 temp := PadR(Copy(PhotoDate, 5, 2), 2, '0');
            'D': { photoday }
                 temp := PadR(Copy(PhotoDate, 7, 2), 2, '0');
            'K': { key }
                 temp := PhotoKey;
            'F': { folder name }
                 temp := ExtractFileName(RemoveTrailingBackSlash(FilePath));
            'N': { file name }
                 temp := ExtractFileBase(FileName);
            'I': { folder number }
                 temp := Rzero(FolderNumber, 4);
            'T': { time as HHHHMMSS }
                 begin                                // hhnnss
                   if Frac(PhotoDateTime) <> 0 then
                     temp := TimeToStr(PhotoDateTime, FormatSettings)
                   else
                     temp := '000000';
                 end;
             'L': { location description }
                 temp := LocationsDescription;
             'X': begin { based on Latitude/Longitude }
                    case BasedOnLatLon.Direction of
                      td_S_to_N:
                        temp    := LatLonToCode(Latitude, td_S_to_N);
                      td_N_to_S:
                        temp    := LatLonToCode(Latitude, td_N_to_S);
                      td_E_to_W:
                        temp    := LatLonToCode(Longitude, td_E_to_W);
                      td_W_to_E:
                        temp    := LatLonToCode(Longitude, td_W_to_E);
                      td_Unknown:
                        raise Exception.Create('System error: unknown location basis');
                    end
                  end;
             'C': temp := ProperCase(Comment);
             'W': temp := Key_Words;
             'S': temp := RZero(SequenceNo, 8);
             else
               temp := '%' + ch;
          end;
          result := result + temp;
        end
      else
        result := result + ch;
    end;
  result := RemoveBadChars(Result, BAD_FILENAME_CHARS);
end;  { GenerateTheFileName }

//{$R+}
procedure TfrmPhotoDataBase.CopySelectedRecords1Click(Sender: TObject);
var
  tempPhotoTable: TPhotoTable;
  Count, AudioCount, MaxCount, GroupNr: integer;
  DestPath: string;
  SrcFileName, DstFileName, FileNamePattern, RootFileName, Ext, Delta, AudioSrcName, AudioDstName, FileBase,
  PartialDate: string;
  GenFileNames: boolean;
  BasedOnLatLon: TBasedOnLatLon;

  CreateLinks, OK, CopyAssociatedWAVFile, CopyOnlyPhotoFiles, DeleteAfterCopying, ChangeFileDateToPhotoDate: boolean;
  FileDate, Err: integer;
  mt: TMediaType;
  Saved_RecNo: integer;
  PhotoDateTime: TDateTime;
  NeedLocationInfo, NeedLeadingDate: boolean;
  LocationDescription: string;
  SequenceNo: integer;

  function StripLeadingDate(const SrcFileName: string): string;
  var
    SrcFileBase, StrippedName, SrcFileExt: string;
    idx: integer;
  begin { StripLeadingDate }
    SrcFileBase  := ExtractFileBase(SrcFileName);
    SrcFileExt   := ExtractFileExt(SrcFileName);
    if NeedLeadingDate and StringContainsPartialDate(SrcFileBase, PartialDate, Idx) and (Idx = 1) then
      begin
        StrippedName := Trim(Copy(SrcFileBase, Length(PartialDate)+1, Length(SrcFileBase) - Length(PartialDate)));
        result       := StrippedName + SrcFileExt;
      end
    else
      result := SrcFileName;
  end;  { StripLeadingDate }

begin { TfrmPhotoDataBase.CopySelectedRecords1Click }
  if not Assigned(frmCopySelectedFiles) then
    frmCopySelectedFiles := TfrmCopySelectedFiles.Create(self);

  if frmCopySelectedFiles.ShowModal = mrOk then
    begin
      Update_Status('Initializing copy process');
      DestPath           := ForceBackSlash(frmCopySelectedFiles.FilePath);
      GenFileNames       := frmCopySelectedFiles.GenFileNames;
      BasedOnLatLon      := frmCopySelectedFiles.BasedOnLatLon;

      FileNamePattern    := frmCopySelectedFiles.FileNamePattern;
      NeedLocationInfo   := (Pos('%L', FileNamePattern) > 0) or
                            (Pos('%l', FileNamePattern) > 0) or
                            BasedOnLatLon.FileNameBasedOnLatLon;
      NeedLeadingDate    := Pos('%Y%M%D', FileNamePattern) > 0;
      CreateLinks        := frmCopySelectedFiles.CreateLinks;
      CopyAssociatedWAVFile := frmCopySelectedFiles.CopyAssociatedWAVFile;
      CopyOnlyPhotoFiles := frmCopySelectedFiles.CopyOnlyPhotoFiles;
      DeleteAfterCopying := frmCopySelectedFiles.DeleteAfterCopying;
      ChangeFileDateToPhotoDate := frmCopySelectedFiles.ChangeFileDateToPhotoDate;

      tempPhotoTable     := TPhotoTable.Create( self,
                                                CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                cFILENAMES,
                                                []);
      try
        Count := 0; AudioCount := 0;
        with TempPhotoTable do
          begin
            OnFilterRecord := PhotoTableFilterRecord;

            Filtered       := true;
            Active         := true;
            SetSelectivityParserExpression(fExpression);
            SetOrder(fCurrentOrder);

            // count the records to be processed
            Update_Status('Counting records to be processed');
            First;
            MaxCount := 0;
            while not Eof do
              begin
                inc(MaxCount);
                Next;
              end;

            First; SequenceNo := 0;
            while not Eof do
              begin
                Saved_RecNo  := RecNo;
                SrcFileName  := PathAndFileName;

                Ext := ExtractFileExt(SrcFileName);
                mt  := MediaTypeFromExtension(Ext);
                if CopyOnlyPhotoFiles then
                  OK := IsPhotoMedia(mt)
                else
                  OK := true;

                if OK then
                  begin
                    GroupNr := 0;
                    repeat
                      Delta := Rzero(GroupNr, 4);
                      if GenFileNames then
                        begin
                          if NeedLocationInfo then
                            if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
                              begin
                                LocationDescription := LocationsTable.fldDescription.AsString;
                                LocationDescription := RemoveBadChars(LocationDescription, BAD_FILENAME_CHARS);
                              end
                            else
                              LocationDescription := RemoveBadChars(fldKEY_WORDS.AsString, BAD_FILENAME_CHARS);

                          RootFileName := GenerateTheFileName( FileNamePattern,
                                                               FullFilePath,
                                                               StripLeadingDate(fldFILE_NAME.AsString),
                                                               fldPath_NO.AsInteger,
                                                               fldPhotoDate.AsString,
                                                               fldKey.AsString,
                                                               fldPhotoDateTime.AsDateTime,
                                                               LocationDescription,
                                                               BasedOnLatLon,
                                                               fldLatitude.AsFloat,
                                                               fldLongitude.AsFloat,
                                                               fldComment.AsString,
                                                               fldKey_Words.AsString,
                                                               SequenceNo);
                          inc(SequenceNo);

                          if GroupNr > 0 then
                            DstFileName := DestPath + Format('%s - %s%s',
                                                  [RootFileName, Delta, Ext])
                          else
                            DstFileName := DestPath + RootFileName + Ext;
                        end else
(*
                      if BasedOnLatLon.FileNameBasedOnLatLon then
                        begin
                        end
                      else
*)
                        begin
                          FileBase     := ExtractFileBase(SrcFileName);
                          RootFileName := DestPath + FileBase;
                          if GroupNr > 0 then
                            DstFileName := Format('%s - %s%s',
                                                  [RootFileName, Delta, Ext])
                          else
                            DstFileName := Format('%s%s', [RootFileName, Ext]);
                        end;


                      if not CreateLinks then
                        OK := not FileExists(DstFileName)
                      else
                        Ok := not FileExists(DstFileName + '.lnk');

                      if not OK then
                        case frmCopySelectedFiles.OverWriteMode of
                          om_ChangeFileNameRatherThanOverwriting:
                            Inc(GroupNr);
                          om_NeverOverwrite:
                            break;
                          om_AlwaysOverwrite:
                            OK := true;
                          om_AskToOverwriteDuplicateFiles:
                            begin
                              OK := YesFmt('OK to overwrite "%s"', [DstFileName]);
                              if not OK then
                                break;
                            end;
                        end;
                    until OK;

                    if OK then
                      if CreateLinks then
                        begin
                          CreateLink(SrcFileName, DstFileName+'.lnk', 'Link to '+SrcFileName);
                          inc(Count);
                          if CopyAssociatedWAVFile then
                            begin
                              AudioSrcName := ExtractFilePath(SrcFileName) + ExtractFileBase(SrcFileName) + WAV_FILE_EXT;
                              if FileExists(AudioSrcName) then
                                begin
                                  AudioDstName := ExtractFilePath(DstFileName) + ExtractFileBase(DstFileName) + WAV_FILE_EXT;
                                  CreateLink(AudioSrcName, AudioDstName+'.lnk', 'Link to '+SrcFileName);
                                  inc(AudioCount);
                                end;
                            end;
                        end
                      else
                        begin
                          if IsVideoMedia(mt) then
                            Update_Status( Format('Processing %d/%d: %s', [Count+1, MaxCount, DstFileName]));
                          if CopyFile(pchar(SrcFileName), pchar(DstFileName), false) then
                            begin
                              if ChangeFileDateToPhotoDate and (not Empty(fldPhotoDate.AsString)) then
                                begin
                                  FileDate      := DateTimeToFileDate(fldPhotoDateTime.AsDateTime);
                                  PhotoDateTime := PhotoDate2DateTime(fldPhotoDate.AsString, dd_UseLow); // if only a partial date - default to start of month
                                  if PhotoDateTime <> BAD_DATE then
                                    try
                                      Err           := FileSetDate(DstFileName, FileDate);
                                      if Err <> 0 then
                                        begin
                                          FileDate      := DateTimeToFileDate(PhotoDateTime+1);  // +1 is a kludge - don't know why its needed
                                          Err           := FileSetDate(DstFileName, FileDate);
                                          if Err <> 0 then
                                            AlertFmt('Unable to set date on file "%s" to %s. Err = %d',
                                                     [DstFileName, fldPhotoDate.AsString, Err]);
                                        end;
                                    except
                                      on e:ERangeError do
                                        AlertFmt('Exception raised in CopySelectedRecords1Click. File = "%s}, Date = {%s}, Error = %s',
                                                 [DstFileName, DateToStr(PhotoDateTime), e.Message]);
                                    end;
                                end;
                              inc(count);
                            end;
                          if CopyAssociatedWAVFile then
                            begin
                              AudioSrcName := ExtractFilePath(SrcFileName) + ExtractFileBase(SrcFileName) + WAV_FILE_EXT;
                              if FileExists(AudioSrcName) then
                                begin
                                  AudioDstName := ExtractFilePath(DstFileName) + ExtractFileBase(DstFileName) + WAV_FILE_EXT;
                                  if CopyFile(pchar(AudioSrcName), pchar(AudioDstName), false) then
                                    inc(AudioCount);
                                end;
                            end;
                          if DeleteAfterCopying then
                            Delete;
                        end;
                  end;
                if RecNo = Saved_RecNo then
                  Next;

                Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
              end;
            Update_Status( Format('COMPLETE. %d/%d recs Copied', [Count, MaxCount]));
            if AudioCount > 0 then
              Update_Status( lblStatus.Caption + Format(' + %d audio files copied', [AudioCount]));
          end;
      finally
        tempPhotoTable.Active := false;
        FreeAndNil(tempPhotoTable);
      end;
    end;
end;  { TfrmPhotoDataBase.CopySelectedRecords1Click }
//{$R-}

procedure TfrmPhotoDataBase.DeleteTrailingDates1Click(Sender: TObject);
const
  Delims = ' ,\-.()_';
var
  Count, MaxCount, Fixed: integer;
  temp: string;
  PartialDate: string;
  CharIdx: integer;
  FileNameInfo: TFileNameInfo;
  Saved_RecNo: integer;
begin
  with FileNameInfo do
    begin
      IncludeFileName         := true;
      IncludeFilePath         := false;
      DateKinds               := [dk_From_FileName];
    end;

  FreeAndNil(tempPhotoTable);
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  Count          := 0;
  Fixed          := 0;
  try
    with TempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        MaxCount       := TotalRecordCount;
        First;
        while not Eof do
          begin
            temp := fldKEY_WORDS.AsString;
            Saved_RecNo := RecNo;
            if FileNameContainsPartialDate(temp, PartialDate, [dk_From_FileName], CharIdx) then
              begin
                temp := Copy(Temp, 1, CharIdx-1);
                Edit;
                fldKEY_WORDS.AsString := temp;
                fldUPDATED.AsDateTime := Now;
                fldUpdateReason.AsInteger := integer(ur_RemoveTrailingDate);
                Post;
                inc(Fixed);
              end;

            if Saved_Recno = RecNo then
              Next;

            inc(Count);
            if (Count mod 100) = 0 then
              Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
          end;
      end;
  finally
    Update_Status( Format('COMPLETE. %d/%d recs fixed', [Fixed, Count]));
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    tblPhotoTable.Refresh;
  end;
end;

procedure TfrmPhotoDataBase.dbFileNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  SysUtils.Beep;
end;

procedure TfrmPhotoDataBase.dbFilePathKeyPress(Sender: TObject;
  var Key: Char);
begin
  Sysutils.Beep;
end;

procedure TfrmPhotoDataBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent.Name <> '') then
    if (Operation = opRemove) then
      begin
        if (aComponent = frmFullSize) then
          begin
            SaveWindowInfo(frmFullSize, fFullSizeWindowsInfo);
            frmFullSize := nil;
            if Assigned(cbUseFullSizeWindow) then
              begin
                cbUseFullSizeWindow.Checked := false;
                SizePhoto;
              end;
          end else
        if (aComponent = fFrmAudioRecorder) then
          fFrmAudioRecorder := nil else
        if (aComponent = frmBrowseMemo) then
          begin
            frmBrowseMemo := nil;
            cbDisplayTextInFile.Checked := false;
          end else
        if aComponent = fScenesBrowser then
          begin
            fScenesBrowser := nil;
            if Assigned(cbShowScenes) then
              cbShowScenes.Checked := false;
          end
      end
    else
      if Operation = opInsert then
        if aComponent = fScenesBrowser then
          begin
            fScenesBrowser.Show;
            cbShowScenes.Checked := true;
          end;
end;

procedure TfrmPhotoDataBase.OpenFullSizeWindow(OpenIt: boolean);
begin
  UseFullSizeWindow1.Checked  := OpenIt;
  cbUseFullSizeWindow.Checked := OpenIt;
  if Openit then
    begin
      if (not Assigned(frmFullSize)) then
        begin
          frmFullSize := TFrmFullSize.Create(self);
          frmFullSize.FreeNotification(self);
        end;
      with frmFullSize do
        begin
          with fFullSizeWindowsInfo do
            if (Size.x > 0) and (size.y > 0) then
              SetWindowInfo(frmFullSize, fFullSizeWindowsInfo);
          Show;
        end;
      SizePhoto;
    end
  else
    begin
      FreeAndNil(frmFullSize);
      SizePhoto;
    end;
end;

procedure TfrmPhotoDataBase.OpenFullSizeWindowExecute(Sender: TObject);
begin
  OpenFullSizeWindow(true);
end;

procedure TfrmPhotoDataBase.cbUseFullSizeWindowClick(Sender: TObject);
begin
  OpenFullSizeWindow(cbUseFullSizeWindow.Checked);
end;

procedure TfrmPhotoDataBase.UseFullSizeWindow1Click(Sender: TObject);
begin
  with UseFullSizeWindow1 do
    Checked := not Checked;
  OpenFullSizeWindow(UseFullSizeWindow1.Checked);
end;

(*
procedure TfrmPhotoDataBase.SlideShow1Click(Sender: TObject);
begin
  with SlideShow1 do
    begin
      Checked := not Checked;
      SlideShow2.Checked := Checked;
      DoSlideShow(Checked);
    end;
end;
*)

constructor TfrmPhotoDataBase.Create(aOwner: TComponent);
var
  mc: TMediaClass;
{$IfDef dhd2}
  temp: string;
  b: boolean;
{$EndIf}

  procedure TryAgain(const FileName, Msg: string);
  begin { TryAgain }
    AlertFmt('Error while loading File "%s" [%s]',
             [FileName, Msg]);
    if GetOptionsFromUser then
      begin
        CommonPhotoSettings.LoadSettings;  // try with the new settings
        UpdateFormCaption;
      end
    else
      raise EOperatorAbort.CreateFmt('Unable to load file "%s"', [FileName]);
  end;  { TryAgain }

begin { TfrmPhotoDataBase.Create }
  inherited;
{$IfDef debugging}
  Message('Entering TfrmPhotoDataBase.Create');
{$endif}
{$IfDef dhd2}
  Temp   := ComputerName;
  b      := SameText(Temp, 'xps-8930') (* or
            SameText(Temp, 'Zino') or
            SameText(Temp, 'DellLaptop') or
            SameText(Temp, 'SurfacePro' *);  // omit DellLaptop, SurfacePro for real-world testing of non DHD
  SetIsDHD(b);
  GenerateFilenamesList1.Visible := true;
  BrowseBoundaries1.Visible := true;
{$else}
  BrowseTracks1.Visible     := false;
  GenerateFilenamesList1.Visible := false;
{$EndIf}
  gEXEPath   := AddBackSlashL(ExtractFilePath(ParamStr(0)));

  with clbMediaClasses do
    begin
      for mc := Low(TMediaClass) to High(TMediaClass) do
        Items.AddObject(MediaClassInfoArray[mc].MediaClassName, TObject(mc));
      Assert(Items.Count = ord(High(TMediaClass))+1);
    end;
  ResetMediaClasses;

  with DBRadioGroup1 do
    begin
      with Buttons[0] do
        begin
          Caption := '&V Good';
          ShowHint := true;
          Hint     := 'Interesting AND high-quality picture';
        end;

      with Buttons[1] do
        begin
          Caption := '&Good';
          ShowHint := true;
          Hint     := 'Interesting OR high-quality picture';
        end;

      with Buttons[2] do
        begin
          Caption := '&Ok';
          ShowHint := true;
          Hint     := 'Average picture';
        end;

      with Buttons[3] do
        begin
          Caption := 'Poor';
          ShowHint := true;
          Hint     := 'UNInteresting OR low-quality picture';
        end;

      with Buttons[4] do
        begin
          Caption := '&Terrible';
          ShowHint := true;
          Hint     := 'UNInteresting AND low-quality picture';
        end;
    end;

  fRatingItems[2]  := Excellent1;
  fRatingItems[1]  := Good1;
  fRatingItems[0]  := Ok1;
  fRatingItems[-1] := Fair1;
  fRatingItems[-2] := Poor1;

  udMaxPhotos.Position := MAX_THUMBNAILS_TO_SHOW;
  meSS.AsInteger       := MIN_STRSIMILARITY;

  try
    CommonPhotoSettings.LoadSettings;
    UpdateFormCaption;

  except
    on EOperatorAbort do
      Exit;
    on e:Exception do
      TryAgain(CommonSettingsFileName(false), e.Message);
  end;

  if not SameText(CommonPhotoSettings.ExePath, gExePath) then
    TryAgain(gExePath, 'Executable location has changed');
  CommonPhotoSettings.ExePath := gExePath;
  cbUseSynonyms.Checked := CommonPhotoSettings.UseSynonymsByDefault;

  if not FileExists(CommonPhotoSettings.PhotoDBDatabaseFileName) then
    TryAgain(CommonPhotoSettings.PhotoDBDatabaseFileName, 'Database File could not be found');
(*
  if not DirectoryExists(CommonPhotoSettings.PhotoDBFolder) then
    TryAgain(CommonPhotoSettings.PhotoDBFolder, 'Path to documents could not be found');
*)
  fAllowRotation := true;
{$IfDef debugging}
  Message('Exiting TfrmPhotoDataBase.Create');
{$endif}
end;  { TfrmPhotoDataBase.Create }

(*
procedure TfrmPhotoDataBase.SlideShow2Click(Sender: TObject);
begin
  with SlideShow2 do
    begin
      Checked := not Checked;
      SlideShow1.Checked := Checked;
      DoSlideShow(Checked);
    end;
end;
*)

procedure TfrmPhotoDataBase.SetInterval(Interval: integer);
begin
  CommonPhotoSettings.SlideShowInterval := Interval;
  if Assigned(Timer) then
    Timer.Interval := Interval;
end;

procedure TfrmPhotoDataBase.btnExpressionClick(Sender: TObject);
var
  OldExpression: string;
begin
  OldExpression := fExpression;
  Form_Expression.Expression := fExpression;
  if Form_Expression.ShowModal = mrOk then
    begin
      fExpression := Form_Expression.Expression;
      tblPhotoTable.SetSelectivityParserExpression(fExpression);
      if fExpression <> OldExpression then
        Include(fChanged, cr_ParameterChanged);
      Enable_Buttons;
    end;
end;

procedure TfrmPhotoDataBase.cbCenterPhotoClick(Sender: TObject);
begin
  SizePhoto;
end;

procedure TfrmPhotoDataBase.FormResize(Sender: TObject);
begin
  DbGrid1.Width := Panel4.Width - DbGrid1.Left - 10;   // 20220818 - Tyring to keep the right side of DBGrid1 visible
end;

procedure TfrmPhotoDataBase.RecNo1Click(Sender: TObject);
begin
(*
  fCurrentOrder := coNone;
  tblPhotoTable.SetOrder(fCurrentOrder);
  RecNo1.Checked := true;
*)
  SetOrder(coNone); // changed from above on 8/12/2017  
end;

procedure TfrmPhotoDataBase.DuplicateFileSizeReport1Click(Sender: TObject);
var
  FilePath: string;
begin
  FilePath := CalcLogFileName('DupSizes.txt');
  if BrowseForFile('Duplicate file size report name', FilePath, '.txt') then
    begin
      fLogPathFileName := FilePath;
      ScanDuplicateFileSizes(fLogPathFileName);
    end;
end;

procedure TfrmPhotoDataBase.Delete1Click(Sender: TObject);
begin
  tblPhotoTable.Delete;
end;

procedure TfrmPhotoDataBase.ListSelectedRecords1Click(Sender: TObject);
var
  frmReportOptions: TfrmReportOptions;
  Ok: boolean;
  Count: integer;
  mc: TMediaClass;
  mediatype, temp, GeneratedName: string;
  Cursor: TCursor;
  NeedLocationInfo: boolean;
  LocationDescription: string;
  Pattern, ID, OwnerName: string;
  BasedOnLatLon: TBasedOnLatLon;
begin
  BasedOnLatLon.FileNameBasedOnLatLon := false;  // not implemented yet
  Cursor := Screen.Cursor;
  Screen.Cursor := crSQLWait;
  try
    frmReportOptions := TfrmReportOptions.Create(self);
    with frmReportOptions do
      begin
        edtLogFileName.Text     := CalcLogFileName('SelectedRecords.txt');
        btnOk.Enabled           := true;

        Ok := ShowModal = mrOk;

        BasedOnLatLon.Direction     := frmReportOptions.GetDirection;
      end;

    if Ok then
      try
        fLogPathFileName := frmReportOptions.edtLogFileName.Text;
        AssignFile(fLogFile, fLogPathFileName);
        Rewrite(fLogFile);

        MediaType := '';
        with clbMediaClasses do
          begin
            if Checked[0] then
              MediaType := '(All Media Classes)'
            else
              begin
                MediaType := '(';
                for mc := Succ(Low(TMediaClass)) to High(TMediaClass) do
                  begin
                    if Checked[ord(mc)] then
                      if MediaType = '' then
                        MediaType := MediaClassInfoArray[mc].MediaClassName
                      else
                        MediaType := MediaType + ', ' + MediaClassInfoArray[mc].MediaClassName;
                  end;
                MediaType := MediaType + ')';
              end;
          end;

        WriteLn(fLogFile, 'Listing of ', MediaType, ' records. Keywords = ', edtKeyWords.Text);
        WriteLn(fLogFile);
        tempPhotoTable := TPhotoTable.Create( self,
                                              CommonPhotoSettings.PhotoDBDatabaseFileName,
                                              cFILENAMES,
                                              [optNoSyncFilePathTable, optReadOnly]);
        with tempPhotoTable do
          begin
{$IfDef dhd}
            onGetBoundariesTable := GetBoundariesTable;
{$EndIf}
  //        Active         := true;
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            Active         := true;
            SetSelectivityParserExpression(fExpression);
            SetOrder(fCurrentOrder);
            Pattern         := UpperCase(frmReportOptions.FileNamePattern);
            NeedLocationInfo :=  (Pos('%L', Pattern) > 0) or
                                 (Pos('%X', Pattern) > 0);
            First;
            Count          := 0;
            while not Eof do
              begin
                Write(fLogFile, Count+1:5, ' ');
                if frmReportOptions.IncludePhotoDate then
                  Write(fLogFile, Padr(tempPhotoTable.fldPhotoDate.AsString, 8), ' ');
                if frmReportOptions.IncludeFileName then
                  Write(fLogFile, Padr(tempPhotoTable.fldFILE_NAME.AsString, 40), ' ');
                if frmReportOptions.GenerateFileName then
                  begin
                    if NeedLocationInfo then
                      if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
                        begin
                          LocationDescription := LocationsTable.fldDescription.AsString;
                          LocationDescription := RemoveBadChars(LocationDescription, BAD_FILENAME_CHARS);
                        end
                      else
                        LocationDescription := RemoveBadChars(fldKEY_WORDS.AsString, BAD_FILENAME_CHARS);

                    GeneratedName := GenerateTheFileName( frmReportOptions.FileNamePattern,
                                                           FullFilePath,
                                                           fldFILE_NAME.AsString,
                                                           fldPath_NO.AsInteger,
                                                           fldPhotoDate.AsString,
                                                           fldKey.AsString,
                                                           fldPhotoDateTime.AsDateTime,
                                                           LocationDescription,
                                                           BasedOnLatLon,
                                                           fldLatitude.AsFloat,
                                                           fldLongitude.AsFloat,
                                                           fldComment.AsString,
                                                           fldKey_Words.AsString,
                                                           Count);
                    Write(fLogFile, Padr(GeneratedName, 40), ' ');
                  end;
                if frmReportOptions.IncludeFilePath then
                  Write(fLogFile, Padr(tempPhotoTable.ShortFilePath, 60), ' ');
                if frmReportOptions.IncludeKeyWords then
                  Write(fLogFile, MyUtils.Left(tempPhotoTable.fldKEY_WORDS.AsString, 65), ' ');
                if frmReportOptions.IncludeLocationInfo and (fldLocationID.AsInteger > 0) then
                  begin
                    if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
                      Write(fLogFile, tempPhotoTable.LocationsTable.fldDescription.AsString);
                  end;
                if frmReportOptions.IncludeCopyrightOwner then
                  begin
                    ID := tempPhotoTable.fldCOPYR_ID.AsString;
                    if TempCopyrightTable.Locate('COPYR_ID', ID, []) then
                      begin
                        OwnerName := TempCopyrightTable.FieldByName('Owner').AsString;
                        Write(fLogFile, Padr(ID, 4), ' ', Padr(OwnerName,30));
                      end
                    else
                      begin
                        Write(fLogFile, Padr(ID, 4), ' ', Padr('(not in Copyrights table)',30));
                      end;
                  end;
                Writeln(fLogFile);
                Inc(Count);
                Next;
              end;
            Update_Status( Format('%d records scanned', [Count]));
            Active         := false;
          end;
      finally
        CloseFile(fLogFile);
        temp := Format('Notepad.exe "%s"', [fLogPathFileName]);
        FileExecute(temp, false);
        FreeAndNil(tempPhotoTable);
      end;
    FreeAndNil(frmReportOptions);
  finally
    Screen.Cursor := Cursor;
  end;
end;

procedure TfrmPhotoDataBase.ScanforMissngFilesandUpdate1Click(
  Sender: TObject);
begin
  ScanForMissing(true);
end;

procedure TfrmPhotoDataBase.ScanforDuplicates1Click(Sender: TObject);
begin
  ScanDuplicates(coFileName, dwNone);
end;

procedure TfrmPhotoDataBase.ScanforDuplicatesandDelete1Click(
  Sender: TObject);
begin
  ScanDuplicates(coFileName, dwNewestUpdate)
end;

procedure TfrmPhotoDataBase.Updated1Click(Sender: TObject);
begin
  fCurrentOrder := coDateUpdated;
  tblPhotoTable.SetOrder(fCurrentOrder);
end;

procedure TfrmPhotoDataBase.Added1Click(Sender: TObject);
begin
(*
  fCurrentOrder := coDateAdded;
  tblPhotoTable.SetOrder(fCurrentOrder);
*)
  SetOrder(coDateAdded);
end;

procedure TfrmPhotoDataBase.FillPhotoDateTimefromPhotoDate1Click(
  Sender: TObject);
var
  MaxCount, Count, Fixed: integer;
  PhotoDateTime, OldPhotoDateTime: TDateTime;
  YYYY, MM, DD, HH, Min, Sec, Msec: word;
  OldRecNo: integer;
begin
  if Yes('This will replace the PhotoDateTime with the PhotoDate which may cause the TIME to be lost. Procede?') then
    begin
      tempPhotoTable := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            [optNoUpdateDate]);
      try
      with TempPhotoTable do
        begin
          OnFilterRecord := PhotoTableFilterRecord;
          Filtered       := true;
          Active         := true;
          SetSelectivityParserExpression(fExpression);
          MaxCount       := TotalRecordCount;
          Count          := 0;
          Fixed          := 0;
          First;
          while not eof do
            begin
              OldRecNo := RecNo;
              YYYY := 0; MM := 0; DD := 0;
              if PhotoDateToYearMonthDay(fldPhotoDate.AsString, YYYY, MM, DD) then
                try
                  OldPhotoDateTime := fldPhotoDateTime.AsDateTime;
                  DecodeTime(OldPhotoDateTime, HH, Min, Sec, Msec); // if it already had time value, use it instead of leaving blank
                  PhotoDateTime := EncodeDateTime(YYYY, MM, DD, HH, Min, Sec, MSec);
                  if OldPhotoDateTime <> PhotoDateTime then
                    begin
                      Edit;
                      fldPhotoDateTime.AsDateTime := PhotoDateTime;
                      fldUpdateReason.AsInteger   := integer(ur_DateTimeFromPhotoDate); // tested
                      Post;
                      Inc(Fixed);
                    end;
                except
                  on e:Exception do
                    Alert(e.Message);
                end;

              if OldRecNo = RecNo then
                Next;

              inc(Count);
              if (Count mod 100) = 0 then
                Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
            end;
        end;
        tblPhotoTable.Refresh;
        Update_Status( Format('COMPLETE. %d/%d file dates updated', [Fixed, Count]));
      finally
        tempPhotoTable.Active := false;
        FreeAndNil(tempPhotoTable);
        tblPhotoTable.Refresh;
      end;
    end;
end;

procedure TfrmPhotoDataBase.cbUseSynonymsClick(Sender: TObject);
begin
  SynonymsList;  // create the list, if necessary
  Include(fChanged, cr_SynonymChange);
end;

function TfrmPhotoDataBase.GetSynonymsTable: TSynonymsTable;
begin
  if not Assigned(fSynonymsTable) then
    begin
      fSynonymsTable := TSynonymsTable.Create(self,
                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                        cLOOKUPS,
                        [],
                        lc_Syn);
      fSynonymsTable.Active := true;
    end;
  result := fSynonymsTable;
end;


function TfrmPhotoDataBase.SynonymsList: TSynonymObject;
begin
  if not Assigned(fSynonymsList) then
    fSynonymsList := TSynonymObject.Create(SynonymsTable);
  result := fSynonymsList;
end;

function TfrmPhotoDataBase.ContainsWords( Target, Data: string;
                                          MatchWholeWordsOnly: boolean;
                                          MustContainAll: boolean = true;
                                          AllowSynonyms: boolean = false;
                                          aSynonymsList: TSynonymObject = nil;
                                          AllowStrSimilarity: boolean = false;
                                          MinSimilarity: integer = MIN_STRSIMILARITY): boolean;
var
  wcD, wcT, TW, DW: integer;
  aWord: string;
  DataWords: array of string;
  MatchedOne: boolean;

  function DataContainsWord(const aWord: string): boolean;
  var
    DW: integer;
  begin { DataContainsWord }
    result := false;
    for DW := 0 to wcD-1 do
      begin
        if aWord = DataWords[DW] then
          begin
            result := true;
            exit;
          end;
        if AllowStrSimilarity then
//        if Soundex(aWord)  Soundex(Datawords[DW])) then
          if similar100(aWord, Datawords[DW]) > MinSimilarity then
            begin
              result := true;
              exit;
            end;
      end;
  end; { DataContainsWord }

  function DataContainsSynonymFor(const aWord: string): boolean;
  var
    DW: integer;
  begin
    result := false;
    if AllowSynonyms and (aSynonymsList <> nil) then
      begin
        for DW := 0 to wcD-1 do
          if aSynonymsList.IsSynonymOf(aWord, DataWords[DW]) then
            begin
              result := true;
              exit;
            end;
      end
    else
      result := false;
  end;

begin { ContainsWords }
  Target     := UpperCase(Target);
  Data       := UpperCase(Data);
  MatchedOne := false;

  if MatchWholeWordsOnly then
    begin
      wcD := WordCountL(Data, KEYWORDDELIMS);
      SetLength(DataWords, wcD);
      for DW := 1 to wcD do
        DataWords[DW-1] := ExtractWordL(DW, Data, KEYWORDDELIMS);
      wcT := WordCountL(Target, KEYWORDDELIMS);
      // try to match each word of the target to any word in the data
      for TW := 1 to wcT do
        begin
          aWord := ExtractWordL(TW, Target, KEYWORDDELIMS);

          if (not (DataContainsWord(aWord) or DataContainsSynonymFor(aWord))) then
            begin
              if MustContainAll then
                begin
                  result := false;
                  exit;
                end;
            end
          else
            MatchedOne := true;
        end;
      result := MatchedOne; // iff required number of matches made
    end
  else
    result := Pos(Target, Data) > 0;
end;  { ContainsWords }

(*
procedure TfrmPhotoDataBase.EditSynonymsFile1Click(Sender: TObject);
var
  temp: string;
begin
  temp := Format('Notepad.exe "%s"', [CommonPhotoSettings.SynonymsFileName]);
  FileExecute(temp, true);
  FreeAndNil(fSynonymsList);
  SynonymsList;   // reload it
end;
*)

procedure TfrmPhotoDataBase.PhotoDateTime1Click(Sender: TObject);
begin
(*
  fCurrentOrder := coPhotoDateTime;
  tblPhotoTable.SetOrder(fCurrentOrder);  // changed to below on 8/12/2017
*)
end;

procedure TfrmPhotoDataBase.ScanforDuplicateFileSizes1Click(
  Sender: TObject);
begin
  ScanDuplicates(coFileSize, dwNone);
end;

(*
procedure TfrmPhotoDataBase.FillWidthHeightfromEXIF1Click(Sender: TObject);
begin
  FillWidthHeightFromExif;
  tblPhotoTable.Refresh;
end;
*)

procedure TfrmPhotoDataBase.MoveFile1Click(Sender: TObject);
begin
  frmFileMover := TfrmFileMover.Create(self);
  frmFileMover.ShowModal;
  FreeAndNil(frmFileMover);
end;

procedure TfrmPhotoDataBase.ovcKeyAfterExit(Sender: TObject);
begin
  if not tblPhotoTable.Locate(CKEY, ovcKey.AsInteger, []) then
    AlertFmt('Key value "%d" not found in current selection.', [ovcKey.AsInteger]);
end;

procedure TfrmPhotoDataBase.DropFileTarget1Drop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  OldLfn, NewLfn, OldExt, NewExt: string;
  UpdateInfo: TUpdateInfo;
  ThumbnailPathName: string;
  ErrorMsg: string;
begin
  OldLfn := tblPhotoTable.PathAndFileName;
  if DropFileTarget1.Files.Count > 1 then
    Alert('Only 1 file at a time') else
  if DropFileTarget1.Files.Count <= 0 then
    Alert('No files?')
  else
    begin
      NewLfn := DropFileTarget1.Files[0];
      NewExt := ExtractFileExt(NewLfn);
      OldExt := ExtractFileExt(OldLfn);
      if not SameText(OldExt, NewExt) then
        begin
          AlertFmt('Change in file type (%s->%s) not permitted', [OldExt, NewExt]);
          Exit;
        end;

      If YesFmt('Replace file ' + CRLF +
                '  %s' + CRLF +
                'with file ' + CRLF +
                '  %s' + CRLF +
                '?', [OldLfn, NewLfn]) then
        begin
          if not CopyFile(pchar(NewLfn), pchar(OldLfn), false) then
            AlertFmt('Unable to replace file "%s" with file "%s"', [OldLfn, NewLfn])
          else
            begin
              with tblPhotoTable do
                begin
                  Edit;
                  fldFile_Size.AsFloat := FileSize64(OldLfn);
                  Finalize(UpdateInfo);
                  FillChar(UpdateInfo, SizeOf(UpdateInfo), 0);
                  with UpdateInfo do
                    begin
                      FileName      := OldLfn;
                      ImagePathName := OldLfn;
//                    FileNameInfo.UseExifForPhotoDateAndLocation := true;
                      with FileNameInfo do
                        DateKinds := DateKinds + [dk_From_EXIF, dk_Date_Modified,
                                                  dk_Date_Created, dk_Last_Access,
                                                  dk_MediaInfo];
                    end;
                  if GetPhotoInfo(UpdateInfo) then
                    begin
                      fldWidth.AsInteger  := UpdateInfo.PixelWidth;
                      fldHeight.AsInteger := UpdateInfo.PixelHeight;
                    end;
                  fldUPDATED.AsDateTime := Now;
                  fldUpdateReason.AsInteger := integer(ur_DropTarget);  // tested
                  Post;

                  // update the thumbnail
                  case MyCreateThumbNail(OldLfn, ErrorMsg) of
                    tps_Error:
                      AlertFmt('Unable to create/update thumbnail "%s" [%s]: ', [ExtractFileName(ThumbnailPathName), ErrorMsg]);
                  end;
                end;
              ShowCalcFields(tblPhotoTable);
              SizePhoto;
              MyDeleteFile(NewLfn, true, true);
            end;
        end;
    end;
end;

function TfrmPhotoDataBase.TempCopyrightTable: TDataSet;
begin
  if not Assigned(fTempCopyrightTable) then
    begin
      fTempCopyrightTable := TADOTable.Create(self);
      with fTempCopyRightTable do
        begin
//        ConnectionString  := Format(c_CnStr, [gPhotoDBDatabaseFileName]);
          Connection        := MyConnection(CommonPhotoSettings.PhotoDBDatabaseFileName);
          TableName         := 'COPYRIGHTS';
          TableDirect       := true;
          IndexFieldNames   := 'COPYR_ID';

          CursorLocation    := clUseClient;
          Active            := true;
        end;
    end;
  result := fTempCopyrightTable;
end;

procedure TfrmPhotoDataBase.BrowseCopyrights1Click(Sender: TObject);
begin
  if not Assigned(fBrowseCopyRights) then
    fBrowseCopyRights := TfrmDataSetBrowser.Create(self, TempCopyrightTable, 'Copyrights Table');
  fBrowseCopyRights.Show;
end;

procedure TfrmPhotoDataBase.FillLocationfromEXIF1Click(Sender: TObject);
var
  Lfn: string; Count, MaxCount, Updated: integer;
  ImgData: TimgData;
  OldRecNo: integer;
  Latitude, Longitude: double;
  LocationID: integer;
  temp: string;
  dummy: integer;
  FoundDesc: string;
  OverWrite: boolean;
begin { TfrmPhotoDataBase.FillLocationfromEXIF1Click }
  OverWrite := Yes('If a location is already specified, do you want to overwrite it?');
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  ImgData := TImgData.Create();
  try
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Updated        := 0;
      First;
      while not Eof do
        begin
          OldRecNo := RecNo;
          Lfn      := PathAndFileName;

          // Parse the EXIF data to get location
          if ImgData.ProcessFile(Lfn) then
            if ImgData.HasEXIF then
              begin
                with ImgData.ExifObj do
                  begin
{$IfNDef dExif2}   // Use the old version of dExif
                    Latitude    := GetDecimalDegrees(Data['GPSLatitudeRef'].Data, Data['GPSLatitude'].Data);
                    Longitude   := GetDecimalDegrees(Data['GPSLongitudeRef'].Data, Data['GPSLongitude'].Data);
{$else}
                    Latitude    := GetDecimalDegrees(TagByName['GPSLatitudeRef'].Data, TagByName['GPSLatitude'].Data);
                    Longitude   := GetDecimalDegrees(TagByName['GPSLongitudeRef'].Data, TagByName['GPSLongitude'].Data);
{$EndIf dExif2}
                    if (Latitude <> 0) or (Longitude <> 0) then
                      begin
                        case CommonPhotoSettings.DefaultLocationKind of
                          dl_LatLong:
                            temp := CalcLocationStringLong(Latitude, Longitude);
                          dl_LastWord:
                            temp := ExtractFileName(RemoveTrailingBackSlash(ExtractFilePath(Lfn)));  // default location to last word in path
                          dl_SpecifiedValue:
                            temp := CommonPhotoSettings.DefaultLocation;
                        end;

                        LocationsTable.InitLocationInfo( fLocationInfo,
                                                         ls_PhotoEXIF,
                                                         temp);
                        LocationID  := LocationsTable.FindLocationID( Latitude,
                                                                      Longitude,
                                                                      fLocationInfo,
                                                                      dummy { ignore count of added records },
                                                                      FoundDesc);


                        if LocationID > 0 then
                          if fldLocationID.IsNull or (fldLocationID.AsInteger = 0) or OverWrite then
                            begin
                              Edit;
                              fldLocationID.AsInteger := LocationID;
                              fldLatitude.AsFloat     := Latitude;
                              fldLongitude.AsFloat    := Longitude;
                              fldUpdateReason.AsInteger := integer(ur_LocationFromEXIF);  // tested
                              Post;
                              Inc(Updated);
                            end;
                      end;
                  end;
              end;

          if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
            Next;

          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d, (updated=%d)', [Count, MaxCount, Updated]));
        end;
    end;
    Update_Status( Format('COMPLETE. Processed %d/%d, (updated=%d)', [Count, MaxCount, Updated]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    ImgData.Free;
    tblPhotoTable.Refresh;
  end;
end;  { TfrmPhotoDataBase.FillLocationfromEXIF1Click }

procedure TfrmPhotoDataBase.FillLocationinfofromGPXdatafiles1Click(
  Sender: TObject);
begin
  if not Assigned(frmUpdateLocationInfo) then
    frmUpdateLocationInfo := TfrmUpdateLocationInfo.Create(self);
  with CommonPhotoSettings do
    CreateGPSTrackingInfo(DefaultGPXFilter, SavedTrackDataLfn);
  frmUpdateLocationInfo.ShowModal;
end;

procedure TfrmPhotoDataBase.FillLocationFromEnteredData1Click(
  Sender: TObject);
var
  Lfn: string; Count, MaxCount, Updated, Added: integer;
  OldRecNo: integer;
  Latitude, Longitude: double;
  LocationID: integer;
  FoundDesc: string;
begin
  if FillLocationInfo.ShowModal = mrOk then
    begin
      tempPhotoTable := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            []);
      try
        with TempPhotoTable do
          begin
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            Active         := true;
            SetSelectivityParserExpression(fExpression);
            MaxCount       := TotalRecordCount;
            Count          := 0;
            Updated        := 0;
            Added          := 0;
            First;
            while not eof do
              begin
                OldRecNo := RecNo;
                Lfn      := PathAndFileName;

                Latitude    := FillLocationInfo.ovcLatitude.AsFloat;
                Longitude   := FillLocationInfo.ovcLongitude.AsFloat;
                if (Latitude <> 0) or (Longitude <> 0) then
                  begin
                    LocationsTable.InitLocationInfo( fLocationInfo,
                                                     ls_Manual,
                                                     FillLocationInfo.ovcDescription.AsString,
                                                     FillLocationInfo.ovcState.AsString);

                    LocationID  := LocationsTable.FindLocationID( Latitude,
                                                                  Longitude,
                                                                  fLocationInfo,
                                                                  Added { count of added records },
                                                                  FoundDesc);

                    if (fldLocationID.AsInteger <= 0) { no previous location specified }
                        or (FillLocationInfo.cbUpdateExistingLocationID.Checked) then
                      begin
                        Edit;
                        fldLocationID.AsInteger := LocationID;
                        fldLatitude.AsFloat     := Latitude;
                        fldLongitude.AsFloat    := Longitude;
                        fldUpdateReason.AsInteger := integer(ur_EnteredLocation);  // tested
                        Post;
                        Inc(Updated);
                      end;
                  end;

                if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
                  Next;

                inc(Count);
                if (Count mod 100) = 0 then
                  Update_Status( Format('Processed %d/%d, (updated=%d, added=%d)', [Count, MaxCount, Updated, Added]));
              end;
          end;
        Update_Status( Format('COMPLETE. Processed %d/%d, (updated=%d, added=%d)', [Count, MaxCount, Updated, Added]));
      finally
        tempPhotoTable.Active := false;
        FreeAndNil(tempPhotoTable);
      end;
    end;
  tblPhotoTable.Refresh;
end;

procedure TfrmPhotoDataBase.EditLocationInfo(CreateIt: boolean);
var
  aLatitude, aLongitude: double;
  LocationID: integer;
  WasAdded: integer;
  OkToSave: boolean;
  FoundDesc: string;
begin
  FillLocationInfo.cbUpdateExistingLocationID.Checked := true;
  if CreateIt then
    begin
      FillLocationInfo.LocationID      := 0;
      FillLocationInfo.Description     := '';
      FillLocationInfo.State           := '';
      FillLocationInfo.RefCount        := 0;
    end
  else
    begin
      FillLocationInfo.LocationID      := gLocationID;
      FillLocationInfo.Latitude        := gLatitude;
      FillLocationInfo.Longitude       := gLongitude;
      FillLocationInfo.Description     := gDescription;
      FillLocationInfo.State           := gState;
      FillLocationInfo.RefCount        := gRefCount;
    end;
  FillLocationInfo.PrivateLocation := gPrivateLocation;
  FillLocationInfo.LocationSource  := gLocationSource;

  if FillLocationInfo.ShowModal = mrOk then
    with tblPhotoTable do
      begin
        aLatitude    := FillLocationInfo.ovcLatitude.AsFloat;
        aLongitude   := FillLocationInfo.ovcLongitude.AsFloat;
        if (aLatitude <> 0) or (aLongitude <> 0) then
          begin
            WasAdded := 0;
            LocationsTable.InitLocationInfo(fLocationInfo,
                               FillLocationInfo.LocationSource,
                               FillLocationInfo.Description,
                               FillLocationInfo.State,
                               round(FillLocationInfo.MaxDistance),
                               true,          { OK to create it if it doesn't exist }
                               CreateIt);     { add is mandatory if we are creating }

            LocationID  := LocationsTable.FindLocationID( aLatitude, aLongitude,
                                fLocationInfo,
                                WasAdded,
                                FoundDesc);
            OkToSave := true;
            if CreateIT and (WasAdded = 0) then
              OkToSave := YesFmt('A location with this same Lat/Lon [%s] was found. Is it OK to update it?',
                                 [FoundDesc]);

            if OkToSave then
              with LocationsTable do
                begin
                  if Locate(cID, LocationID, []) then
                    begin
                      Edit;
                      fldLatitude.AsFloat     := aLatitude;
                      fldLongitude.AsFloat    := aLongitude;
                      fldDescription.AsString := FillLocationInfo.ovcDescription.AsString;
                      fldState.AsString       := FillLocationInfo.ovcState.AsString;
                      Post;
                    end;
                end;

            if (fldLocationID.AsInteger <= 0) { no previous location specified }
                or (FillLocationInfo.cbUpdateExistingLocationID.Checked) then
              begin
                LocationsTable.DecrementRefCnt(gLocationID);

                Edit;
                fldLocationID.AsInteger := LocationID;
                fldLatitude.AsFloat     := aLatitude;
                fldLongitude.AsFloat    := aLongitude;
                fldUpdateReason.AsInteger := integer(ur_LocationEdited);  // tested

                LocationsTable.IncrementRefCnt(LocationID);

                Update_Status('Location ID Updated');
                gLatitude        := aLatitude;
                gLongitude       := aLongitude;
                gDescription     := FillLocationInfo.ovcDescription.AsString;
                gState           := FillLocationInfo.ovcState.AsString;
                gLocationID      := LocationID;
                gRefCount        := FillLocationInfo.RefCount;
                gPrivateLocation := FillLocationInfo.PrivateLocation;
                PhotoTableAfterScroll(tblPhotoTable);
              end;
          end;
      end;
end;

procedure TfrmPhotoDataBase.FindNextKeyWords;
begin
  tblPhotoTable.MyDisableControls;
  try
    tblPhotoTable.Next;
    if not FindKeyWords(fLastSearch) then
      begin
        if YesFmt('"%s" not found. Search from the top?', [fLastSearch]) then
          begin
            tblPhotoTable.First;
            if not FindKeyWords(fLastSearch) then
              AlertFmt('"%s" not found in current selection set', [fLastSearch]);
          end;
      end;
  finally
    tblPhotoTable.MyEnableControls;
    tblPhotoTable.AfterScroll(tblPhotoTable);
  end;
end;


function TfrmPhotoDataBase.FindKeyWords(SearchString: string): boolean;
var
  mode: TSearch_Type; // (SEARCHING, SEARCH_FOUND, NOT_FOUND)
  Saved_RecNo: integer;
  OK: boolean;
  Saved_Cursor: TCursor;
begin
  Saved_Cursor  := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    with tblPhotoTable do
      begin
//      tblPhotoTable.MyDisableControls;
        Saved_RecNo := RecNo;
        mode := SEARCHING;
        try
          while (not Eof) and (mode = SEARCHING) do
            begin
              OK := ContainsWords( SearchString,
                              tblPhotoTable.fldKEY_WORDS.AsString,
                              true, // its a key word search so must match all
                              true, // must match all
                              cbUseSynonyms.Checked);

              if OK then
                mode := SEARCH_FOUND
              else
                if not Eof then
                  Next
                else
                  mode := NOT_FOUND
            end;
        finally
          if mode = NOT_FOUND then
            tblPhotoTable.RecNo := Saved_RecNo;
//        tblPhotoTable.MyEnableControls;
//        tblPhotoTable.AfterScroll(tblPhotoTable);
          fLastSearchKind := sk_KeyWord;
          result := mode = SEARCH_FOUND;
        end;
      end;
  finally
    Screen.Cursor := Saved_Cursor;
  end;
end;



procedure TfrmPhotoDataBase.miFindKeyWordClick(
  Sender: TObject);
begin
  if GetString('Search for KeyWord', 'Key Word(s)', fLastSearch) then
    begin
      tblPhotoTable.MyDisableControls;
      try
        if not FindKeyWords(fLastSearch) then
          if YesFmt('"%s" not found. Search from the top?', [fLastSearch]) then
            begin
              tblPhotoTable.First;
              if not FindKeyWords(fLastSearch) then
                AlertFmt('"%s" not found in current selection set', [fLastSearch]);
            end;
      finally
        tblPhotoTable.MyEnableControls;
        tblPhotoTable.AfterScroll(tblPhotoTable);
      end;
    end;
end;

procedure TfrmPhotoDataBase.DeleteLocationInfo1Click(Sender: TObject);
var
  LocationID: integer;
begin
  with tblPhotoTable do
    begin
      Edit;
      LocationID := fldLocationID.AsInteger;
      fldLocationID.Clear;
      fldLatitude.Clear;
      fldLongitude.Clear;
      fldUpdateReason.AsInteger := integer(ur_LocationDeleted); // tested
      Post;

      LocationsTable.DecrementRefCnt(LocationID);

      Update_Status('Location ID Deleted');
      PhotoTableAfterScroll(tblPhotoTable);
    end;
end;

procedure TfrmPhotoDataBase.CopyLocationInfo1Click(Sender: TObject);
begin
  with tblPhotoTable do
    begin
      gLatitude  := fldLatitude.AsFloat;
      gLongitude := fldLongitude.AsFloat;
      if fldLocationID.AsInteger > 0 then
        begin
          if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
            with LocationsTable do
              begin
                gLatitude    := fldLatitude.AsFloat;
                gLongitude   := fldLongitude.AsFloat;
                gDescription := fldDescription.AsString;
                gState       := fldState.AsString;
                gLocationID  := fldLocationID.AsInteger;
                gRefCount    := fldRefCnt.AsInteger;
                Clipboard.AsText := Format('%11.7f %11.7f', [gLatitude, gLongitude]);
              end;
        end
      else
        if (fldLatitude.AsFloat <> 0) or (fldLongitude.AsFloat <> 0) then
          begin
            gLatitude    := fldLatitude.AsFloat;
            gLongitude   := fldLongitude.AsFloat;
            gDescription := '';
            gState       := '';
            gLocationID  := -1;
            gRefCount    := 0;
            Clipboard.AsText := Format('%11.7f %11.7f', [gLatitude, gLongitude]);
          end
        else
          SysUtils.Beep;
    end;
end;

procedure TfrmPhotoDataBase.PasteLocationInfo1Click(Sender: TObject);
var
  Ok: boolean;
begin
  if gLocationID <> 0 then
    begin
      with tblPhotoTable do
        begin
          Ok := true;
          if fldLocationID.AsInteger > 0 then
            Ok := YesFmt('Ok to overwrite current location ID (%d) info with info from Location ID %d?',
                         [fldLocationID.AsInteger, gLocationID]);
          if Ok then
            begin
              LocationsTable.DecrementRefCnt(fldLocationID.AsInteger);

              Edit;
              fldLocationID.AsInteger := gLocationID;
              fldLatitude.AsFloat     := gLatitude;
              fldLongitude.AsFloat    := gLongitude;
//            Post;
              PhotoTableAfterScroll(tblPhotoTable);

              LocationsTable.IncrementRefCnt(gLocationID);
            end;
        end;
    end
  else
    Alert('No location ID');
end;

procedure TfrmPhotoDataBase.ClearLocationIDforselectedrecords1Click(
  Sender: TObject);
var
  Updated, Count, OldRecNo: integer;
begin
  TempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  Count          := 0;
  Updated        := 0;
  try
    with TempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        First;
        while not eof do
          begin
            OldRecNo := RecNo;

            if fldLocationID.AsInteger > 0 then
              begin
                Edit;
                fldLocationID.Clear;
                fldLatitude.Clear;
                fldLongitude.Clear;
                fldUpdateReason.AsInteger := integer(ur_ClearLocationForSelected);  // tested
                Post;
                Inc(Updated);
              end;

            if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
              Next;

            inc(Count);
            if (Count mod 100) = 0 then
              Update_Status( Format('Processed %d, Updated=%d', [Count, Updated]));
          end;
      end;
  finally
    FreeAndNil(TempPhotoTable);
    Update_Status( Format('Complete. Processed %d, Updated=%d', [Count, Updated]));
    tblPhotoTable.Refresh;
  end;
end;

(*
procedure TfrmPhotoDataBase.FillYearMonthDayfromPhotoDate1Click(
  Sender: TObject);
var
  Updated, Count, OldRecNo: integer;
  YYYY, MM, DD: word;
  OverWrite: boolean;
begin
  FreeAndNil(TempPhotoTable);
  Overwrite := Yes('Overwrite pre-existing values in Year, Month and Day?');
  TempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  try
    with TempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        Count          := 0;
        Updated        := 0;
        First;
        while not eof do
          begin
            OldRecNo := RecNo;

            YYYY := 0;
            MM   := 0;
            DD   := 0;

            if (not Empty(fldPhotoDate.AsString)) and
                PhotoDateToYearMonthDay(fldPhotoDate.AsString, YYYY, MM, DD) and
                (Overwrite or
                 (fldYear.AsInteger = 0) or
                 (fldMonth.AsInteger = 0) or
                 (fldDay.AsInteger = 0)) then
              begin
                Edit;
                if YYYY > 0 then
                  fldYear.AsInteger  := YYYY
                else
                  fldYear.Clear;

                if MM > 0 then
                  fldMonth.AsInteger := MM
                else
                  fldMonth.Clear;

                if DD > 0 then
                  fldDay.AsInteger   := DD
                else
                  fldDay.Clear;

                fldUpdateReason.AsInteger := integer(ur_FillYearMonthDayFromPhotoDate);  // tested
                Post;
                Inc(Updated);
              end;

            if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
              Next;

            inc(Count);
            if (Count mod 100) = 0 then
              Update_Status( Format('Processed %d, Updated=%d', [Count, Updated]));
          end;
        Update_Status( Format('Complete. Processed %d, Updated=%d', [Count, Updated]));
        tblPhotoTable.Refresh;
      end;
  finally
    FreeAndNil(TempPhotoTable);
  end;
end;
*)

procedure TfrmPhotoDataBase.EditLocationInfo1Click(Sender: TObject);
begin
  if tblPhotoTable.fldLocationID.AsInteger > 0 then
    begin
      if tblPhotoTable.LocationsTable.Locate(cID, tblPhotoTable.fldLocationID.AsInteger, []) then
        with tblPhotoTable.LocationsTable do
          begin
            gLatitude        := fldLatitude.AsFloat;
            gLongitude       := fldLongitude.AsFloat;
            gDescription     := fldDescription.AsString;
            gState           := fldState.AsString;
            gLocationID      := tblPhotoTable.fldLocationID.AsInteger;
            gRefCount        := fldRefCnt.AsInteger;
            gLocationSource  := TLocationSource(fldLocationSource.AsInteger);
            gPrivateLocation := fldPrivateLocation.AsBoolean;
            gDateAdded       := fldDateAdded.AsDateTime;
            gDateUpdated     := fldDateUpdated.AsDateTime;
            FillLocationInfo.MaxDescSize     := tblPhotoTable.LocationsTable.fldDescription.Size;
            FillLocationInfo.Latitude        := gLatitude;
            FillLocationInfo.Longitude       := gLongitude;
            FillLocationInfo.Description     := gDescription;
            FillLocationInfo.State           := gState;
            FillLocationInfo.LocationSource  := gLocationSource;
            FillLocationInfo.LocationID      := gLocationID;
            FillLocationInfo.PrivateLocation := gPrivateLocation;
            FillLocationInfo.cbUpdateExistingLocationID.Visible := false;
            FillLocationInfo.RefCount        := gRefCount;

            if FillLocationInfo.ShowModal = mrOk then
              begin
                tblPhotoTable.Edit;
                tblPhotoTable.fldLatitude.AsFloat  := FillLocationInfo.Latitude;
                tblPhotoTable.fldLongitude.AsFloat := FillLocationInfo.Longitude;
                if Empty(tblPhotoTable.fldKEY_WORDS.AsString) then
                  begin // avoid RTE which lead to data loss
                    Alert('Key words may not be blank');
                    exit;
                  end;

                tblPhotoTable.Post;

                Edit;

                fldLatitude.AsFloat          := FillLocationInfo.Latitude;
                fldLongitude.AsFloat         := FillLocationInfo.Longitude;
                fldDescription.AsString      := FillLocationInfo.Description;
                fldState.AsString            := FillLocationInfo.State;
                fldLocationSource.AsInteger  := ord(FillLocationInfo.LocationSource);
                fldPrivateLocation.AsBoolean := FillLocationInfo.PrivateLocation;

//              Post;      // The LocationsTable gets scrolled soon which forces a post
//              PhotoTableAfterScroll(tblPhotoTable);
                ShowCalcFields(tblPhotoTable);
              end;
          end;
    end
  else
    SysUtils.Beep;
end;

procedure TfrmPhotoDataBase.CopyLatitude1Click(Sender: TObject);
begin
  with tblPhotoTable do
    begin
      if not fldLatitude.IsNull then
        Clipboard.AsText := FloatToStr(fldLatitude.AsFloat) else
      if fldLocationID.AsInteger > 0 then
        begin
          if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
            with LocationsTable do
              Clipboard.AsText := FloatToStr(fldLatitude.AsFloat)
          else
            SysUtils.Beep;
        end;
    end;
end;

procedure TfrmPhotoDataBase.CopyLongitude1Click(Sender: TObject);
begin
  with tblPhotoTable do
    begin
      if not fldLongitude.IsNUll then
        Clipboard.Astext := FloatToStr(fldLongitude.AsFloat) else
      if fldLocationID.AsInteger > 0 then
        begin
          if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
            with LocationsTable do
              Clipboard.AsText := FloatToStr(fldLongitude.AsFloat)
          else
            SysUtils.Beep;
        end;
    end;
end;

procedure TfrmPhotoDataBase.CreatefromScratch1Click(Sender: TObject);
begin
  gLocationSource := ls_Manual;
  EditLocationInfo(true);
end;

procedure TfrmPhotoDataBase.BasedOnDateTime1Click(Sender: TObject);
var
  Latitude, Longitude: double;
  dummy: integer;
  Temp, Dummys: string;
begin
  if GetScanningOptions(fUpdateInfo, fLocationInfo, [doNoFileNameInfo]) then
    with fLocationInfo do
      try
        if (fSavedLocationInfo.UseGPXLogsForLocation <> UseGPXLogsForLocation) or
           (fSavedLocationInfo.UseSavedTracksLogFile <> UseSavedTracksLogFile) then
          FreeAndNil(gGPSTrackingInfo);

        if (not Assigned(gGPSTrackingInfo)) then
          gGPSTrackingInfo := TGPSTrackingInfo.Create( CommonPhotoSettings.DefaultGPXFilter,
                                                       CommonPhotoSettings.SavedTrackDataLfn);
        gGPSTrackingInfo.OnUpdateStatus := self.Update_Status;

        fSavedLocationInfo := fLocationInfo;

        fUpdateInfo.PhotoDateTime := tblPhotoTable.fldPhotoDateTime.AsDateTime;
        if gGPSTrackingInfo.GetLatitudeLongitudeFromGPXLogs( Latitude,
                                                             Longitude,
                                                             fUpdateInfo,
                                                             fLocationInfo) then
          begin
            if not Empty(fLocationInfo.DefaultLocation) then
              temp := fLocationInfo.DefaultLocation
            else
              temp := '(Newly created)';

            tblPhotoTable.LocationsTable.InitLocationInfo( fLocationInfo,
                                             ls_GPSLogs,
                                             temp,
                                             fLocationInfo.DefaultState);

            gLocationID := tblPhotoTable.LocationsTable.FindLocationID( Latitude, Longitude,
                                                          fLocationInfo,
                                                          Dummy,
                                                          Dummys);
            gLatitude       := Latitude;
            gLongitude      := Longitude;
            with tblPhotoTable.LocationsTable do
              begin
                if Locate(cID, gLocationID, []) then
                  begin
                    gDescription := fldDescription.AsString;
                    gState       := fldState.AsString;
                    gLocationSource := TLocationSource(fldLocationSource.AsInteger);
                  end
                else
                  raise Exception.CreateFmt('System error: unable to locate LocationID = %d', [gLocationID]);
              end;
            EditLocationInfo(false);
          end
        else
          AlertFmt('Could not find any GPS log entries within %d minutes of %s',
                   [MAX_TIME_DIFFERENCE_IN_MINUTES, DateTimeToStr(tblPhotoTable.fldPhotoDateTime.AsDateTime+fUpdateInfo.MinutesToAdd * ONE_MINUTE)]);
      finally
        Update_Status('', 1); // clear the status messages
        Update_Status('', 2);
      end;

end;

procedure TfrmPhotoDataBase.Update_Status(const Msg: string; LineNo: integer = 1);
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
        lblStatus.Color    := aColor;
        lblStatus.Caption  := Msg;
      end;
    2: begin
        lblStatus1.Color   := aColor;
        lblStatus1.Caption := Msg;
       end;
  end;
  Application.ProcessMessages;
end;


procedure TfrmPhotoDataBase.SearchforLatLon1Click(Sender: TObject);
var
  ShortestDistanceInFeet: double;
  WayPointInfo: TWayPointInfo;
  FileName, TimeStamp: string;
  aFillLocationInfo: TfrmFillLocationInfo;
begin 
  if Assigned(gGPSTrackingInfo) then
    gGPSTrackingInfo.Free;  // start over

  gGPSTrackingInfo := TGPSTrackingInfo.Create( CommonPhotoSettings.DefaultGPXFilter,
                                               CommonPhotoSettings.SavedTrackDataLfn);
  gGPSTrackingInfo.OnUpdateStatus := Update_Status;
  gGPSTrackingInfo.LoadSelectedTrackData( fLocationInfo);

  aFillLocationInfo := TfrmFillLocationInfo.Create(self);
  try
    aFillLocationInfo.MaxDescSize := tblPhotoTable.LocationsTable.fldDescription.Size;
    with aFillLocationInfo do
      begin
        LocationSearchType                 := st_Location;
        ovcDescription.Visible             := false;
        ovcState.Visible                   := false;
        lbLocationSource.Visible           := false;
        cbUpdateExistingLocationID.Visible := false;
        Caption := 'Enter coordinates to be found';
        if ShowModal = mrOk then
          begin
            WayPointInfo := gGPSTrackingInfo.FindNearestPointInTracks(Latitude, Longitude, ShortestDistanceInFeet, FileName);
            if ShortestDistanceInFeet < FEETPERMILE then
              begin
                TimeStamp := DateTimeToStr(WayPointInfo.DateTime);
                AlertFmt('The nearest found point was %8.2f feet away and came from file %s. The time stamp was %s',
                       [ShortestDistanceInFeet, FileName, TimeStamp]);
              end
            else
              begin
                Alert('There were no points found that were less than a mile away');
              end;
          end;
      end;
  finally
    aFillLocationInfo.Free;
  end;
end;

procedure TfrmPhotoDataBase.FindLocation1Click(Sender: TObject);
begin
  FindLocation(st_Description);
end;

procedure TfrmPhotoDataBase.dbPhotoDateTimeExit(Sender: TObject);
var
  PhotoDateTime: TDateTime;
  YYYY, MM, DD, Hr, Min, Sec, MSec: word;
begin
  if tblPhotoTable.State in [dsEdit, dsInsert] then
    begin
      PhotoDateTime := tblPhotoTable.fldPhotoDateTime.AsDateTime;
      DecodeDate(PhotoDateTime, YYYY, MM, DD);
      DecodeTime(PhotoDateTime, Hr, Min, Sec, MSec);
      tblPhotoTable.Edit;
      tblPhotoTable.fldYear.AsInteger     := YYYY;
      tblPhotoTable.fldMonth.AsInteger    := MM;
      tblPhotoTable.fldDay.AsInteger      := DD;
      tblPhotoTable.fldPhotoDate.AsString := YearMonthDayToPhotoDate(YYYY, MM, DD);
      tblPhotoTable.fldPhotoDateTime.AsDateTime := PhotoDateTime;  // debug - prevent it from getting overwritten
    end;
end;

procedure TfrmPhotoDataBase.FindLocation( aLocationSearchType: TLocationSearchType);
var
  LocationsBrowser: TfrmLocationsBrowser;
  LocationsTable  : TLocationsTable;
begin
  LocationsTable := TLocationsTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName, cLOCATIONS,
                                            [{optReadOnly,} optUseClient]);
//  LocationsTable.AddFields;   // This slows things way down
  LocationsTable.SelectivityParser; // force the selectivity parser to be created
  LocationsTable.Active := true;
  LocationsBrowser := TfrmLocationsBrowser.Create(self, LocationsTable, HOME_LATITUDE, HOME_LONGITUDE, HOME_DESCRIPTION);
  try
    with LocationsBrowser do
      begin
        LocationSearchType := aLocationSearchType;
        case aLocationSearchType of
          st_Description:
            begin
              FilterLatitude    := 0;
              FilterLongitude   := 0;
              Key_Words         := '';
              FilterMaxDistanceText := '12000 miles';
            end;
          st_Location:
            begin
              if tblPhotoTable.fldLatitude.AsFloat <> 0 then
                FilterLatitude := tblPhotoTable.fldLatitude.AsFloat else
              if (tblPhotoTable.fldLocationID.AsInteger > 0) and
                 (tblPhotoTable.LocationsTable.fldLatitude.AsFloat <> 0.0) then
                FilterLatitude := tblPhotoTable.LocationsTable.fldLatitude.AsFloat
              else
                FilterLatitude := gLatitude;

              if tblPhotoTable.fldLongitude.AsFloat <> 0 then
                FilterLongitude := tblPhotoTable.fldLongitude.AsFloat else
              if (tblPhotoTable.fldLocationID.AsInteger > 0) and
                 (tblPhotoTable.LocationsTable.fldLongitude.AsFloat <> 0.0) then
                FilterLongitude := tblPhotoTable.LocationsTable.fldLongitude.AsFloat
              else
                FilterLongitude := gLongitude;

              Key_Words         := '';
              FilterDescription := tblPhotoTable.LocationsTable.fldDescription.AsString;
              FilterMaxDistanceText := '1 mile';
            end;
          st_Normal:
            Assert(false, 'Unexpected LocationSearchType: st_Normal');
        end;
        LocationsTable.Locate(cID, tblPhotoTable.fldLocationID.AsInteger, []);
        if ShowModal = mrOk then
          begin
            with LocationsTable do
              begin
                gLatitude    := fldLatitude.AsFloat;
                gLongitude   := fldLongitude.AsFloat;
                gDescription := fldDescription.AsString;
                gState       := fldState.AsString;
                gLocationID  := fldID.AsInteger;
                gRefCount    := fldRefCnt.AsInteger;
              end;
          end;
      end;
  finally
    LocationsBrowser.Free;
    LocationsTable.Free;
  end;

end;


procedure TfrmPhotoDataBase.ByLocation1Click(Sender: TObject);
begin
  FindLocation(st_Location);
end;

procedure TfrmPhotoDataBase.BrowseLocations1Click(Sender: TObject);
begin
  tblLocationsTable.Filtered := false;
  tblLocationsTable.Locate(cID, tblPhotoTable.fldLocationID.AsInteger, []);
  with LocationsBrowser do
    begin
      Key_Words         := '';
      FilterLatitude    := 0.0;
      FilterLongitude   := 0.0;
      FilterMaxDistanceText := '12000 miles';
      LocationSearchType := st_Normal;
      ShowModal;      // changing to "Show" may cause an AV 
    end;
end;

function TfrmPhotoDataBase.GetBrowseLocations: TfrmLocationsBrowser;
begin
  if not Assigned(fBrowseLocations) then
    fBrowseLocations := TfrmLocationsBrowser.Create(self, tblLocationsTable, HOME_LATITUDE, HOME_LONGITUDE, HOME_DESCRIPTION);
  result := fBrowseLocations;
end;

procedure TfrmPhotoDataBase.FindNextString;
begin
  tblPhotoTable.MyDisableControls;
  try
    tblPhotoTable.Next;
    if not FindString(fLastSearch) then
      begin
        if YesFmt('"%s" not found. Search from the top?', [fLastSearch]) then
          begin
            tblPhotoTable.First;
            if not FindString(fLastSearch) then
              AlertFmt('"%s" not found in selected records', [fLastSearch]);
          end;
      end;
  finally
    tblPhotoTable.MyEnableControls;
    tblPhotoTable.AfterScroll(tblPhotoTable);    
  end;
end;


procedure TfrmPhotoDataBase.FindAgain1Click(Sender: TObject);
begin
  case fLastSearchKind of
    sk_KeyWord:
      FindNextKeyWords;
    sk_String:
      FindNextString;
  end;
end;

procedure TfrmPhotoDataBase.FillWasScannedFromExif;
var
  Lfn: string; Count, MaxCount, Fixed: integer;
  ImgData: TimgData;
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  ImgData := TImgData.Create();
  try
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Fixed          := 0;
      First;
      while not Eof do
        begin
          Lfn       := PathAndFileName;

          if ImgData.ProcessFile(Lfn) and ImgData.HasEXIF then
            begin
              Edit;
              fldWasScanned.AsBoolean := (Empty(ImgData.ExifObj.CameraMake) and Empty(ImgData.ExifObj.CameraModel))
                                           or (ImgData.ExifObj.CameraModel = 'CanoScan 8800F')
                                           or (fldPhotoDate.AsString < '19980701');
              Inc(Fixed);
              fldUpdateReason.AsInteger := integer(ur_WasScanned);  // tested
              Post;
            end
          else      // strange? No EXIF? Assume that it was scanned.
            begin
              Edit;
              fldWasScanned.AsBoolean := true;
              fldUpdateReason.AsInteger := integer(ur_WasScanned);
              Post;
            end;

          Next;
          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
        end;
    end;
    Update_Status( Format('COMPLETE. %d recs updated', [Fixed]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    ImgData.Free;
  end;
end;


procedure TfrmPhotoDataBase.FillWasScannedfromEXIF1Click(Sender: TObject);
var
  Saved_RecNo: integer;
begin
  Saved_RecNo := tblPhotoTable.RecNo;
  try
    FillWasScannedFromExif;
    tblPhotoTable.Refresh;
  finally
    tblPhotoTable.RecNo := Saved_RecNo;
  end;
end;

procedure TfrmPhotoDataBase.ShowExifInfo(const aCurrentFileName: string);
begin
  if not Assigned(FrmExifInfo) then
    begin
      FrmExifInfo := TFrmExifInfo.Create(self);
      FrmExifInfo.Show;
    end;
  FrmExifInfo.CurrentFileName := aCurrentFileName;
  if not FrmExifInfo.Showing then
    FrmExifInfo.Show;
end;


procedure TfrmPhotoDataBase.ShowEXIFInfo1Click(Sender: TObject);
begin
  ShowExifInfo(tblPhotoTable.PathAndFileName);
end;

(*
procedure TfrmPhotoDataBase.fromEXIF1Click(Sender: TObject);
begin
  FillPhotoDateTimeFromExif;
  tblPhotoTable.Refresh;
end;
*)

procedure TfrmPhotoDataBase.ReplaceTextinSelected1Click(Sender: TObject);
const
  Delims = ' ,\-.()_';
var
  Count, MaxCount, Updated: integer;
  KeyWords: string;
  n, Saved_RecNo, OrgRecNo, mr: integer;
  LeftPart, RightPart: string;
  ConfirmEachRecord, OK: boolean;
  NewKeyWords: string;
begin
  frmReplaceDialog  := TfrmReplaceDialog.Create(self);
  ConfirmEachRecord := false;
  if frmReplaceDialog.ShowModal = mrOk then
    try
      if frmReplaceDialog.cbConfirmEachRecord.Checked then
        begin
          ConfirmEachRecord  := true;
          FreeAndNil(frmConfirmEachPhoto);
          frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update');
        end;

      OrgRecNo       := tblPhotoTable.RecNo;
      tempPhotoTable := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            []);
      with TempPhotoTable do
        begin
          OnFilterRecord := PhotoTableFilterRecord;
          Filtered       := true;
          Active         := true;
          SetSelectivityParserExpression(fExpression);
          MaxCount       := TotalRecordCount;
          Count          := 0;
          Updated        := 0;
          First;
          while not eof do
            begin
              Saved_RecNo := RecNo;
              KeyWords := TempPhotoTable.fldKEY_WORDS.AsString;

              n  := Pos(frmReplaceDialog.FindText, KeyWords);
              OK := n > 0;
              if OK then
                begin
                  LeftPart     := Copy(KeyWords, 1, n-1);
                  RightPart    := Copy(KeyWords, n + Length(frmReplaceDialog.FindText), Length(KeyWords) +1 - n - Length(frmReplaceDialog.FindText));
                  NewKeyWords  := LeftPart + frmReplaceDialog.ReplaceText + RightPart;

                  if ConfirmEachRecord then
                    with frmConfirmEachPhoto do
                      begin
                        Caption1 := Format('Change key words in record key [%d] ' + CRLF +
                                            'from [%s]' + CRLF +
                                            'to   [%s]?',
                                           [fldKey.AsInteger,
                                            KeyWords,
                                            NewKeyWords]);
                        Caption2 := Format('Updated/processed: %d/%d', [Updated, Count]);
                        PhotoFileName := tempPhotoTable.PathAndFileName;
                        mr := ShowModal;
                        OK := mr = mrYes;
                        if mr = mrCancel then
                          break;
                      end
                  else
                    OK := true;
                end;

              if OK then
                begin
                  Edit;
                  TempPhotoTable.fldKEY_WORDS.AsString := NewKeyWords;
                  fldUpdateReason.AsInteger := integer(ur_KeywordsReplaced);   // tested
                  Post;
                  inc(Updated);
                end;

              if Saved_RecNo = RecNo then
                Next;

              inc(Count);
              if (Count mod 100) = 0 then
                Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
            end;
        end;
      try
        Update_Status( Format('COMPLETE. %d recs updated', [Updated]));
      finally
        tempPhotoTable.Active := false;
        FreeAndNil(tempPhotoTable);
        tblPhotoTable.Refresh;
        tblPhotoTable.RecNo := OrgRecNo;
      end;
    finally
      FreeAndNil(frmConfirmEachPhoto);
      FreeAndNil(frmReplaceDialog);
    end;
end;

function TfrmPhotoDataBase.GetFillLocationInfo: TfrmFillLocationInfo;
begin
  if not Assigned(fFillLocationInfo) then
    begin
      fFillLocationInfo := TfrmFillLocationInfo.Create(self);
      fFillLocationInfo.MaxDescSize := tblPhotoTable.LocationsTable.fldDescription.Size;
    end;
  result := fFillLocationInfo;
end;

function TfrmPhotoDataBase.FindString(SearchFor: String): boolean;
var
  modeO, modeI: TSearch_Type; // (SEARCHING, SEARCH_FOUND, NOT_FOUND);
  Saved_RecNo, fn: integer;
  Saved_Cursor: TCursor;
begin
  Saved_Cursor  := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    SearchFor := UpperCase(SearchFor);
    with tblPhotoTable do
      begin
//      MyDisableControls;
        Saved_RecNo := RecNo;
        modeO := SEARCHING;
        try
          while (not Eof) and (modeO = SEARCHING) do
            begin
              modeI := SEARCHING;
              fn   := 0;
              repeat
                if fn >= Fields.Count then
                  modeI := NOT_FOUND
                else
                  begin
                    if Pos(SearchFor, UpperCase(Fields[fn].AsString)) > 0 then
                      modeI := SEARCH_FOUND
                    else
                      inc(fn);
                  end;
              until modeI <> SEARCHING;

              if modeI = SEARCH_FOUND then
                modeo := SEARCH_FOUND
              else
                if not Eof then
                  Next
                else
                  modeo := NOT_FOUND
            end;
        finally
          if modeo = NOT_FOUND then
            RecNo := Saved_RecNo;
//        MyEnableControls;
//        AfterScroll(tblPhotoTable);
          fLastSearchKind := sk_String;
          result := modeO = SEARCH_FOUND;
        end;
      end;
  finally
    Screen.Cursor := Saved_Cursor;
  end;
end;


procedure TfrmPhotoDataBase.FindString1Click(Sender: TObject);
begin
  if GetString('Search for String', 'String', fLastSearch) then
    FindNextString;
end;

procedure TfrmPhotoDataBase.clbMediaClassesClickCheck(Sender: TObject);
var
  mc: TMediaClass;
  AnyChecked: boolean;
begin
  AnyChecked := false;
  for mc := Succ(Low(TMediaClass)) to High(TMediaClass) do
    if clbMediaClasses.Checked[ord(mc)] then
      begin
        AnyChecked := true;
        break;
      end;
  clbMediaClasses.Checked[0] := not AnyChecked;
  Include(fChanged, cr_MediaClassChange);
end;

procedure TfrmPhotoDataBase.puImagePopup(Sender: TObject);
var
  Ext: string; mt: TMediaType; mc: TMediaClass;
  EditorKnown: boolean;
begin
  Ext := MyExtractFileExt(fMediaFileName);
  mt  := MediaTypeFromExtension(Ext);
  mc  := MediaInfoArray[mt].MediaClass;

//CopytoClipboard1.Enabled   := true;
  UseFullSizeWindow1.Enabled := mc = mc_Photos;
//  SlideShow1.Enabled         := mc in [mc_Photos, mc_Video];
  ShowEXIFInfo1.Enabled      := mc in [mc_Photos, mc_Video];
  OpenMedia.Visible          := mc in [mc_Audio, mc_Video, mc_Document];
  if OpenMedia.Visible then
    OpenMedia.Caption        := Format('Open %s file', [MediaInfoArray[mt].Desc]);
  EditorKnown                := not Empty(CommonPhotoSettings.PhotoEditingProgram);
  PhotoEditor2.Enabled       := (mc = mc_Photos) and EditorKnown;
  EditThumbnail1.Enabled     := (mc in [mc_Photos, mc_Video]) and EditorKnown;
  PlayAudio1.Enabled         := tblPhotoTable.fldHasSound.AsBoolean;
  Rotate1.Enabled            := mc = mc_Photos;
end;

procedure TfrmPhotoDataBase.ChangetoProperCase1Click(Sender: TObject);
begin
  with tblPhotoTable do
    begin
      Edit;
      fldKEY_WORDS.AsString := ProperCase(fldKEY_WORDS.AsString);
    end;
end;

procedure TfrmPhotoDataBase.AddPhoto1Click(Sender: TObject);
var
  UpdateInfo: TUpdateInfo;
  KeyWords: string;
  OpenDialog: TOpenDialog;
  ErrorMsg: string;
  BookMark: TBookMark;
begin
  btnApplyClick(nil);  // apply the filters
  OpenDialog := TOpenDialog.Create(self);
  BookMark   := tblPhotoTable.GetBookmark;
  tblPhotoTable.MyDisableControls;
  try
    try
      with OpenDialog do
        begin
          DefaultExt := 'JPG';
          InitialDir := ForceBackSlash(gRootPath);
          FileName   := InitialDir + '*.jpg';
//        Filter     := 'Data files (*.*)|*.*';
          Filter     := BuildMediaFilter; // 'JPEG files (*.jpg)|*.jpg';
          UpdateInfo := fUpdateInfo;
          if Execute then
            with UpdateInfo do
              begin
                FileName        := OpenDialog.FileName;
                with FileNameInfo do
                  begin
                    DateKinds                      := [];
                    IncludeFilePath                := false;
                    IncludeFileName                := false;
                    PhotoOwner                     := gCommonPhotoSettings.CopyRightID;
                    ImagePathName                  := OpenDialog.FileName;
                    ExtractComments                := false;
                    ExtractCustomKey               := false;
                  end;

                fLocationInfo.ExtractLatLonFromFileName      := false;

                if not GetScanningOptions(UpdateInfo, fLocationInfo, []) then
                  Exit;

                PixelWidth          := 0;
                PixelHeight         := 0;
                if not GetPhotoInfo(UpdateInfo) then
                  AlertFmt('Unable to GetPhotoInfo for "%s"', [OpenDialog.FileName]);
                fLocationInfo.LowestDateToScan := Trunc(UpdateInfo.PhotoDateTime); // Just the date, not the time
                FileName        := ExtractFileName(OpenDialog.FileName);
                FilePath        := ExtractFilePath(OpenDialog.FileName);
                if tblPhotoTable.MyLocateRecord( FileName, FilePath) then
                  begin
                    UpdateIt        := false;
                    RecordExists    := true;
                    tblPhotoTable.FreeBookmark(BookMark);  // free memory used
                    BookMark        := tblPhotoTable.GetBookmark;
                    AlertFmt('The file "%s" already exists in the database', [FileName]);
                  end
                else // record dosen't exist
                  begin
                    UpdateIt        := true;
                    RecordExists    := false;
                    try
                      tblPhotoTable.UpdateProc(UpdateInfo, fLocationInfo, KeyWords, Update_Status);
                      tblPhotoTable.FreeBookmark(BookMark);  // free memory used
                      BookMark        := tblPhotoTable.GetBookmark;
                    except
                      raise;
                    end;
                  end;

                case MyCreateThumbnail(OpenDialog.FileName, ErrorMsg) of
                  tps_CreatedUpdated:
                    Update_Status('Thumbnail Created/Updated');
                  tps_Error:
                    ErrorFmt('Error creating thumbnail [%s]', [ErrorMsg]);
                  tps_Ignored:
                    Update_Status('Thumbnail already existed.');
                end;

              end;
        end;
    finally
      OpenDialog.Free;
      tblPhotoTable.MyEnableControls;
      tblPhotoTable.GotoBookmark(BookMark);
      tblPhotoTable.FreeBookmark(BookMark);
//    PhotoTableAfterScroll(tblPhotoTable);
    end;
  except
    on e:Exception do
      Alert(e.Message);
  end;
end;

procedure TfrmPhotoDataBase.CopyfullFileName1Click(Sender: TObject);
begin
  Clipboard.AsText :=  tblPhotoTable.PathAndFileName;
end;

procedure TfrmPhotoDataBase.ShowThumbnailsratherthanfullsizephoto1Click(
  Sender: TObject);
begin
  ShowThumbnailsratherthanfullsizephoto1.Checked := not ShowThumbnailsratherthanfullsizephoto1.Checked;
  SizePhoto;
end;

procedure TfrmPhotoDataBase.PhotoEditor1Click(Sender: TObject);
begin
  EditPhoto;
end;

procedure TfrmPhotoDataBase.AdjustPhotoDatesTimes1Click(Sender: TObject);
var
  Count, MaxCount, Fixed: integer;
  Saved_RecNo, OrgRecNo: integer;
  mr: integer;
  PhotoDateTime: TDateTime;
  YYYY, MM, DD, Hr, Min, Sec, MSec: word;
  YesToAll: boolean;
  Ext: string;
begin
  frmAdjustDateTime := TfrmAdjustDateTime.Create(self);
  try
    OrgRecNo       := tblPhotoTable.RecNo;
    FreeAndNil(tempPhotoTable);
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          []);
    frmAdjustDateTime.PhotoTable := TempPhotoTable;
    with TempPhotoTable do
      begin
        Inhibited      := true;
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        MaxCount       := TotalRecordCount;
        Count          := 0;
        Fixed          := 0;
        YesToAll       := false;
        First;
        while not Eof do
          begin
            Saved_RecNo := RecNo;
//          if IsPhotoMedia(MediaTypeFromExtension(fldFILE_TYPE.AsString)) then
            Ext         := ExtractFileExt(fldFILE_NAME.AsString);
            if IsPhotoMedia(MediaTypeFromExtension(Ext)) then
              begin
                if YesToAll then
                  mr := mrYes
                else
                  begin
                    mr := frmAdjustDateTime.ShowModal;
                    YesToAll := mr = mrYesToAll;
                  end;

                if mr = mrYes then
                  begin
                    PhotoDateTime         := frmAdjustDateTime.NewTime;
                    DecodeDate(PhotoDateTime, YYYY, MM, DD);
                    DecodeTime(PhotoDateTime, Hr, Min, Sec, MSec);

                    Edit;
                    fldYear.AsInteger     := YYYY;
                    fldMonth.AsInteger    := MM;
                    fldDay.AsInteger      := DD;
                    fldPhotoDate.AsString := YearMonthDayToPhotoDate(YYYY, MM, DD);
                    fldPhotoDateTime.AsDateTime := PhotoDateTime;  // debug - prevent it from getting overwritten
                    fldUpdateReason.AsInteger := integer(ur_DatesTimesAdjusted);   // tested
                    Post;
                    inc(Fixed);

                    inc(Count);
                    if (Count mod 100) = 0 then
                      Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
                  end else
                if mr = mrCancel then
                  break;
              end;
            if RecNo = Saved_RecNo then  // current record was not removed from the dataset by selectivity
              Next;
          end;
      end;
    try
      Update_Status( Format('COMPLETE. %d recs updated', [Fixed]));
    finally
      tempPhotoTable.Inhibited := false;
      tempPhotoTable.Active := false;
      FreeAndNil(tempPhotoTable);
      tblPhotoTable.Refresh;
      tblPhotoTable.RecNo := OrgRecNo;
    end;
  finally
    FreeAndNil(frmAdjustDateTime);
  end;
end;

procedure TfrmPhotoDataBase.Bynumber1Click(Sender: TObject);
var
  LocationID: integer;
  Value: string;
  CurrentlyUsed: boolean;
begin
  if GetString('Location ID', cID, Value) then
    begin
      try
        LocationID := StrToInt(Value);
        with tblPhotoTable do
          begin
            if LocationsTable.Locate(cID, LocationID, []) then // it exists
              begin
                CurrentlyUsed := {(not fldLocationID.IsNull) or} (fldLocationID.AsInteger > 0);
                if Not CurrentlyUsed or
                   YesFmt('Do you want to overwrite the current LocationID [%d]?', [fldLocationID.AsInteger]) then
                  begin
                    if CurrentlyUsed then
                      LocationsTable.DecrementRefCnt(fldLocationID.AsInteger);

                    Edit;
                    fldLocationID.AsInteger := LocationID;
                    fldUpdateReason.AsInteger := integer(ur_LocationChangedByNumber); // tested
                    Post;

                    LocationsTable.IncrementRefCnt(LocationID);
                    Update_Status('Location ID Updated');
                    gLatitude         := LocationsTable.fldLatitude.AsInteger;
                    gLongitude        := LocationsTable.fldLongitude.AsInteger;
                    gDescription      := LocationsTable.fldDescription.AsString;
                    gState            := LocationsTable.fldState.AsString;
                    gLocationID       := LocationID;
                    gRefCount         := LocationsTable.fldRefCnt.AsInteger;          
                    gPrivateLocation  := LocationsTable.fldPrivateLocation.AsBoolean;
                    PhotoTableAfterScroll(tblPhotoTable);
                  end;
              end
            else
              AlertFmt('LocationID %d was not found in the Locations table', [LocationID]);
          end;
      except
        on e:EConvertError do
          AlertFmt('%s is not a valid integer', [Value]);
      end;
    end;
end;


procedure TfrmPhotoDataBase.AddFilepath1Click(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  FolderName, KeyWords: string;
  FolderNo: integer;
begin
  OpenDialog := TopenDialog.Create(self);
  try
    with OpenDialog do
      begin
        FolderName := CommonPhotoSettings.PhotoDBFolder;
        if BrowseForFolder('Select folder to add to FilePaths table', FolderName) then
          begin
            FolderNo   := tempFilePathsTable.FindFolderNo(FolderName, KeyWords, true, false);
            MessageFmt('Added folder # %d [%s]', [FolderNo, FolderName]);
          end;
      end;
  finally
    FreeAndNil(OpenDialog);
  end;
end;

procedure TfrmPhotoDataBase.tabMoverResize(Sender: TObject);
begin
  if Assigned(frmFileMover) then
    with frmFileMover do
      begin
        Top    := 0;
        Left   := 0;
        Width  := tabMover.Width;
        Height := tabMover.Height;
      end;
end;

procedure TfrmPhotoDataBase.dbCopyCodeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  temp: string;
begin
  temp := self.tblPhotoTable.fldCOPYR_ID.AsString;
  if not SameText(temp, fLastCopyr_ID) then
    begin
      fLastCopyR_ID := temp;
      dbCopyCode.Hint := Trim(tblPhotoTable.CopyRightsTable.fldCopyRightsOwner.AsString);
      dbCopyCode.ShowHint := true;
    end;
end;

procedure TfrmPhotoDataBase.InsertLocationatCursor1Click(Sender: TObject);
var
  temp: string;
begin
  with tblPhotoTable do
    if fldLocationID.AsInteger > 0 then
      begin
        with LocationsTable do
          if Locate(cID, fldLocationID.AsInteger, []) then
            begin
              if not Empty(fldState.AsString) then
                Temp := Format('%s, %s', [fldDescription.AsString, fldState.AsString])
              else
                Temp := Format('%s', [fldDescription.AsString]);

              if not (tblPhotoTable.State in [dsEdit, dsInsert]) then
                tblPhotoTable.Edit;
                
              dbKeyWords.SelText := Temp;

              Assert(dbKeyWords.DataSource.DataSet = tblPhotoTable, 'System error'); 

//            tblPhotoTable.Post;
            end;
      end
    else
      SysUtils.Beep;
end;

function TfrmPhotoDataBase.GetPhotoList: TPhotoList;
begin
  if not Assigned(fPhotoList) then
    fPhotoList := TPhotoList.Create;
  result := fPhotoList;
end;

{ TPhotoList }

destructor TPhotoList.Destroy;
var
  I: integer;
begin
  for I := Pred(Count) downto 0 do
    TImage(Items[i]).Free;
  inherited;
end;

procedure TfrmPhotoDataBase.SelectThisPhoto1Click(Sender: TObject);
var
  Rating: integer;
begin
  fSelectedPhotoKey := fSelectedImage.Tag;
  with tblPhotoTable do
    begin
      if Locate('Key', fSelectedPhotoKey, []) then
        begin
          Rating := fldRating.AsInteger;
          if (Rating >= -2) and (Rating <= +2) then
            fRatingItems[Rating].Checked := true
          else
            fRatingItems[0].Checked := true; // set it to "Ok"
        end
      else
        AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
    end;
end;

procedure TfrmPhotoDataBase.puThumbNailPopup(Sender: TObject);
begin
  fSelectedImage := ((Sender as TPopupMenu).PopupComponent) as TImage;
end;

procedure TfrmPhotoDataBase.btnRefreshClick(Sender: TObject);
begin
  ReDisplayThumbnails(true);
end;

procedure TfrmPhotoDataBase.RenameFile1Click(Sender: TObject);
var
  OldFileName, NewFileName, FilePath, OldPathName, NewPathName, OldThumbNailName,
  NewThumbNailName: string;
begin
  with tblPhotoTable do
    begin
      OldFileName := fldFILE_NAME.AsString;
      NewFileName := OldFileName;
      if GetString('Rename File', 'New File Name', NewFileName) then
        begin
          if NewFileName <> OldFileName then
            begin
              FilePath    := tblPhotoTable.FullFilePath;
              OldPathName := FilePath + OldFileName;
              NewPathName := FilePath + NewFileName;
              if RenameFile(OldPathName, NewPathName) then
                begin
                  Edit;
                  fldFILE_NAME.AsString := NewFileName;
                  fldUpdateReason.AsInteger := integer(ur_FileRenamed);  // tested
                  Post;
                  AlertFmt('Renamed "%s" --> "%s"', [OldFileName, NewFileName]);
                  OldThumbNailName := ThumbNailPathAndName(OldPathName);
                  NewThumbNailName := ThumbNailPathAndName(NewPathName);
                  if not RenameFile(OldThumbNailName, NewThumbNailName) then
                    AlertFmt('Unable to rename thumbnail "%s" to "%s"',
                             [OldThumbNailName, NewThumbNailName]);
                end
              else
                AlertFmt('Unable to rename file "%s" to "%s"',
                         [OldPathName, NewPathName]);
            end;
        end;
    end;
end;

procedure TfrmPhotoDataBase.PhotoEditor2Click(Sender: TObject);
begin
  EditPhoto;
end;

procedure TfrmPhotoDataBase.AddThumbnail1Click(Sender: TObject);
var
  ErrorMsg, ThumbNailName: string;
  OverWrite: Boolean;
  RotateBy: integer;
begin
  ThumbNailName := ThumbNailPathAndName(tblPhotoTable.PathAndFileName);
  OverWrite := false;
  if FileExists(ThumbNailName) then
    OverWrite := YesFmt('The Thumbnail file "%s" already exists. Do you want to delete it?',
                   [ThumbNailName]);

  RotateBy := RotationNeeded(tblPhotoTable.PathAndFileName);

  case MyCreateThumbNail( tblPhotoTable.PathAndFileName,
                          ThumbNailName,
                          ErrorMsg,
                          THUMBNAILWIDTH,
                          THUMBNAILHEIGHT,
                          OverWrite,
                          RotateBy) of
    tps_CreatedUpdated:
      begin
        PhotoTableAfterScroll(tblPhotoTable); 
        lblStatus.Caption := Format('Thumbnail "%s" Created/Updated', [ThumbNailName]);
      end;
    tps_Error:
      AlertFmt('Error creating thumbnail [%s]: %s', [ThumbNailName, ErrorMsg]);
    tps_Ignored:
      MessageFmt('Thumbnail "%s" is up to date', [ThumbNailName]);
  end;
end;

procedure TfrmPhotoDataBase.RateThisPhoto(Rating, Key: integer; Advance: boolean = false);
var
  Saved_RecNo: integer;
  
  procedure DeleteImage(Index: integer);
  var
    i: integer;
  begin { DeleteImage }
    for i := 0 to PhotoList.Count-1 do
      if (PhotoList[i] <> nil) and (TImage(PhotoList[i]).Tag = fSelectedPhotoKey) then
        begin
          TImage(PhotoList[i]).Free;
          PhotoList[i] := nil;
        end;
  end;  { DeleteImage }

begin { TfrmPhotoDataBase.RateThisPhoto }
  fSelectedPhotoKey := Key;
  with tblPhotoTable do
    begin
      if Locate('Key', fSelectedPhotoKey, []) then
        begin
          Saved_RecNo := RecNo;
          Edit;
          fldRating.AsInteger := Rating;
          fldUpdateReason.AsInteger := integer(ur_RatingChanged);  // tested
          Post;

          if Advance and (RecNo = Saved_RecNo) then
            Next
          else
            try
              if not tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then // if the photo is no longer in the selection set...
                DeleteImage(fSelectedPhotoKey); // remove the photo from the display
            except
              // handle it silently
              DeleteImage(fSelectedPhotoKey); // remove the photo from the display
            end;
        end
      else
        AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
    end;
end;  { TfrmPhotoDataBase.RateThisPhoto }


procedure TfrmPhotoDataBase.Excellent1Click(Sender: TObject);
begin
  RateThisPhoto(+2, fSelectedImage.Tag);
end;

procedure TfrmPhotoDataBase.Good1Click(Sender: TObject);
begin
  RateThisPhoto(+1, fSelectedImage.Tag);
end;

procedure TfrmPhotoDataBase.Ok1Click(Sender: TObject);
begin
  RateThisPhoto(0, fSelectedImage.Tag);
end;

procedure TfrmPhotoDataBase.Fair1Click(Sender: TObject);
begin
  RateThisPhoto(-1, fSelectedImage.Tag);
end;

procedure TfrmPhotoDataBase.Poor1Click(Sender: TObject);
begin
  RateThisPhoto(-2, fSelectedImage.Tag);
end;

procedure TfrmPhotoDataBase.Ratethisphoto1Click(Sender: TObject);
var
  Rating: integer;
begin
  if Assigned(fSelectedImage) then
    begin
      fSelectedPhotoKey := fSelectedImage.Tag;
      with tblPhotoTable do
        begin
          if tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then
            begin
              Rating := fldRating.AsInteger;
              fRatingItems[Rating].Checked := true;
            end
          else
            AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
        end;
    end;
end;

procedure TfrmPhotoDataBase.ImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  MINMOVE = 5;
var
  DropFileSource: TDropFileSource;
  dr: TDragResult;
  Msg: string;
  Image: TImage;
begin
  if fAlreadyDragging then
    exit;

  Shift := (Shift * [ssLeft,ssRight]);

  // Make sure a mouse button is pressed before starting the drag ...
  // and the mouse has moved at least 10 pixels
  // (DragPoint is set in the ListView1MouseDown event)

  if  (Shift = []) or
      ((abs(fDragPoint.X - X) < MINMOVE) and (abs(fDragPoint.Y - Y) < MINMOVE)) then
    exit;

  with tblPhotoTable do
    if (Eof and Bof) then exit;

  if Assigned(Sender) and (Sender is TImage) then
    begin
      Image := Sender as TImage;
      fSelectedPhotoKey := Image.Tag;
      tblPhotoTable.MyDisableControls;
      try
        if tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then
          begin
            DropFileSource := TDropFileSource.Create(self);
            try
              DropFileSource.Files.Add(tblPhotoTable.PathAndFileName);
              fAlreadyDragging := true;

              //Start the dragdrop...
              //Note: DropFileSource1.DragTypes = [dtCopy]
              dr := DropFileSource.Execute;
              //Note: Execute does not return until the drop has finished -
              //either sucessfully with a copy, move or link operation or cancelled.
              case dr of
                drDropCopy: Msg := 'DropCopy';
                drDropMove: Msg := 'DropMove';
                drDropLink: Msg := 'DropLink';
                drCancel  : Msg := 'Cancel';
                drOutMemory: Msg := 'OutMemory';
                drAsync   : Msg := 'Async';
                drUnknown : Msg := 'Unknown';
              end;
              lblStatus.Caption := Format('DragAndDrop %s: %s', [Msg, tblPhotoTable.PathAndFileName]);
              lblStatus.Color   := clWhite;
              Application.ProcessMessages;

              fAlreadyDragging := false;
            finally
              DropFileSource.Free;
            end;
          end;
      finally
        tblPhotoTable.MyEnableControls;
      end;
    end;
end;

procedure TfrmPhotoDataBase.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fDragPoint        := Point(X,Y);
  fSelectedImage    := Sender as TImage;
  fSelectedPhotoKey := fSelectedImage.Tag;
  lblStatus.Caption := 'ImageMuoseDown';
end;

procedure TfrmPhotoDataBase.ImageDoubleClick(Sender: TObject);
begin
  fSelectedImage    := Sender as TImage;
  fSelectedPhotoKey := fSelectedImage.Tag;
  if fSelectedPhotoKey > 0 then
    begin
      if not tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then
        begin
          AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
          Exit;
        end;
      EditPhoto;
    end;
end;


function TfrmPhotoDataBase.GetForm_Expression: TFrmExpression;
begin
  if not Assigned(fForm_Expression) then
    fForm_Expression := TFrmExpression.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, tblPhotoTable, tblPhotoTable.SelectivityParser);
  result := fForm_Expression;
end;

// Currently not useful since settings are ALWAYS loaded from / saved to PhotoDB.ini
procedure TfrmPhotoDataBase.OpenSettings1Click(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(self);
  try
    with OpenDialog do
      begin
        FileName := gCommonSettingsFileName;
        DefaultExt := _INI;
        Filter     := Format('Project file (*.%s)|*.%s', [_INI, _INI]);
        if OpenDialog.Execute then
          begin
            gCommonSettingsFileName := FileName;
            gUsingTemporarySettingsFile  := true;
            tblPhotoTable.Close;
            FreeAndNil(tblPhotoTable);
            CommonPhotoSettings.LoadSettings;
            UpdateFormCaption;
            OpenPhotoTable;
            LoadFuncKeys;

            Update_Status('Now scanning for highest folder number');
            CommonPhotoSettings.FolderNo         := ScanForHighestFolderNumber( CommonPhotoSettings.FolderNo,
                                                                                CommonPhotoSettings.PhotoDBDatabaseFileName);
            Update_Status('Scan Complete');
          end;
      end;
  finally
    OpenDialog.Free;
  end;
end;

{$Include FastMM4Options.inc}
procedure TfrmPhotoDataBase.UpdateFormCaption;
begin
  Caption := Format('PhotoDB [%s] / [%s]', [gCommonSettingsFileName, CommonPhotoSettings.PhotoDBDatabaseFileName]);
{$If Defined(EnableMemoryLeakReporting)}
  Caption := Caption + ' ===> MEMORY LEAK REPORTING IS ENABLED <====';
{$IfEnd}
end;

// Currently not useful since settings are ALWAYS loaded from / saved to PhotoDB.ini
procedure TfrmPhotoDataBase.SaveProjectAs1Click(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  INIExt, TempFileName: string;
begin
  SaveDialog := TSaveDialog.Create(self);
  try
    with SaveDialog do
      begin
        FileName   := gCommonSettingsFileName;
        INIExt     := _INI;
        DefaultExt := INIExt;
        Filter     := Format('Project file (*.%s)|*.%s', [INIExt, INIExt]);
        if SaveDialog.Execute then
          begin
            TempFileName := FileName;
            if GetOptionsFromUser then
              CommonPhotoSettings.SaveSettings(TempFileName);
            gCommonSettingsFileName := TempFileName;
            UpdateFormCaption;
            LoadFuncKeys;
          end;
      end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TfrmPhotoDataBase.PlayAudio1Click(Sender: TObject);
var
  AudioFileName, FullFileName: string;
begin
  FullFileName  := tblPhotoTable.PathAndFileName;
  AudioFileName := ExtractFilePath(FullFileName) + ExtractFileBase(FullFileName) + '.wav';
  if not FileExists(AudioFileName) then
    AlertFmt('Sound file [%s] could not be found', [AudioFileName])
  else
    if (not ExecAndWait(AudioFileName, '', false)) then
      AlertFmt('Error playing %s', [AudioFileName]);
end;

procedure TfrmPhotoDataBase.btnAudioRecorderClick(Sender: TObject);
begin
  if not Assigned(fFrmAudioRecorder) then
    fFrmAudioRecorder := TfrmAudioRecorder.Create(self);
  with tblPhotoTable do
    begin
      fFrmAudioRecorder.AudioFileName := ExtractFilePath(PathAndFileName) + ExtractFileBase(PathAndFileName) + WAV_FILE_EXT;
      fSavedKey := fldKey.AsInteger;
    end;
  fFrmAudioRecorder.OnRecordingChanged := RecordingChanged;
  fFrmAudioRecorder.Show;
end;

procedure TfrmPhotoDataBase.RecordingChanged(const AudioFileName: string);
begin
  with tblPhotoTable do
    begin
      if fSavedKey = fldKey.AsInteger then
        begin
          Edit;
          fldHasSound.AsBoolean := FileExists(AudioFileName);
          fldUpdateReason.AsInteger := integer(ur_HasSound);  // tested
          Post;
        end
      else
        raise Exception.CreateFmt('System error: Key changed: %d --> ^%d',
                                  [fSavedKey, fldKey.AsInteger]);;
    end;
end;


procedure TfrmPhotoDataBase.btnSaveFilterClick(Sender: TObject);
var
  FilterSettings: TFilterSettings;
  FilterName: string;
  MemoStream: TStream;
  LookupsTable: TLookupsTable;
  AlreadyExists: boolean;
  mc: TMediaClass;
  Cat: string;
begin
  FilterSettings := TFilterSettings.Create(self);
  try
    with FilterSettings do
      begin
        if not Empty(edtStringInPath.Text) then
          StringInPath :=  edtStringInPath.Text;
        if not Empty(edtFileNames.Text) then
          StringInFileName := edtFileNames.Text;
        if (not Empty(edtLowDate.Text)) or (not Empty(edtHighDate.Text)) then
          begin
            if not Empty(edtLowDate.Text) then
              LowDate := edtLowDate.Text;
            if not Empty(edtHighDate.Text) then
              HighDate := edtHighDate.Text;
            if cbDateType.ItemIndex >= 0 then
              DateType := self.DateType;
          end;
        if not Empty(edtLowYear.Text) then
          LowYear := edtLowYear.Text;
        if not Empty(edtHighYear.Text) then
          HighYear := edtHighYear.Text;
        if not Empty(edtLowMonth.Text) then
          LowMonth := edtLowMonth.Text;
        if not Empty(edtHighMonth.Text) then
          HighMonth := edtHighMonth.Text;
        if not Empty(edtCopyCode.Text) then
          begin
            CopyCode := edtCopyCode.Text;
            if cbNot.Checked then
              NotCopyCode           := cbNot.Checked;
          end;
        if not Empty(edtKeyWords.Text) then
          KeyWords := edtKeyWords.Text;
        MatchWholeWordsOnly := cbMatchWholeWordsOnly.Checked;
        if not Empty(fExpression) then
          FilterExpression := fExpression;

        if cbUnprocessedOnly.Checked then
          UnprocessedOnly       := true;
        if cbHasSound.Checked then
          HasSound              := true;
        if cbLocationID.Checked then
          HasLocationID         := true;
        if cbHasLocationInfoInEXIF.Checked then
          HasLocationInfoInEXIF := true;
        if cbHasSceneInfo.Checked then
          HasSceneInfo := true;
        if cbUseSynonyms.Checked then
          UseSynonyms           := true;
        if cbAllowStrSimilarity.Checked then
          AllowStrSimilarity            := true;
        MinStrSimilarity        := meSS.AsInteger;
        if cbFileIsMissing.Checked then
          FileIsMissing         := true;
        if cbScanComments.Checked then
          ScanComments          := true;
        if cbScanFile.Checked then
          ScanFile              := true;
        if cbScanSceneKeyWords.Checked then
          ScanSceneKeyWords := true;
        if cbScanSceneDates.Checked then
          ScanSceneDates        := true;

        MediaClasses := [];
        with clbMediaClasses do
          begin
            if not Checked[0] then // we're not including everything
              begin
                for mc := Succ(Low(TMediaClass)) to High(TMediaClass) do
                  begin
                    if Checked[ord(mc)] then
                      MediaClasses := MediaClasses + [mc];
                  end;
              end
            else
              MediaClasses := ALLMEDIACLASSES;
          end;

        if not Empty(edtFilePathNo.Text) then
          begin
            SelectedFolder    := edtFilePathNo.Text;
            IncludeSubFolders := cbIncludeSubFolders.Checked;
          end;

        CurrentSortOrder :=   fCurrentOrder;

        if GetString('Save Filter', 'Filter Name', FilterName) then
          begin
            LookupsTable := TLookupsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, cLOOKUPS, [], lc_Fil);
            LookupsTable.Open;
            try
              Cat           := LookupInfoArray[lc_Fil].Cat;
              AlreadyExists := LookupsTable.Locate('LookupCategory;LookupName',
                                                   VarArrayOf([Cat, FilterName]), []);
              if AlreadyExists then
                if not YesFmt('A filter named [%s] already exists. Do you want to overwrite it? ', [FilterName]) then
                  Exit;

              if AlreadyExists then
                LookupsTable.Edit
              else
                begin
                  LookupsTable.Append;
                  LookupsTable.fldLookupName.AsString     := FilterName;
//                LookupsTable.fldLookupCategory.AsString := Cat;   // set in DoBeforePost event of the table
                end;

              MemoStream   := LookupsTable.CreateBlobStream(LookupsTable.fldLookupValue, bmWrite);
              FilterSettings.SaveToStream(MemoStream);
              MemoStream.Free;     // copy the memo stream to the field
              LookupsTable.Post;
            finally
              LookupsTable.Free;
            end;
          end;
      end;
  finally
    FilterSettings.Free;
  end;
end;

procedure TfrmPhotoDataBase.btnLoadFilterClick(Sender: TObject);
var
  FilterSettings: TFilterSettings;
  MemoStream: TStream;
  LookupsTable: TLookupsTable;
  LookupsBrowser: TfrmLookupBrowser;
  mc: TMediaClass;
begin
  FilterSettings := TFilterSettings.Create(self);
  try
    with FilterSettings do
      begin
        LookupsTable := TLookupsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, cLOOKUPS, [], lc_Fil);
        LookupsTable.Open;
        LookupsTable.IndexFieldNames := 'LookupName';
        LookupsBrowser := TfrmLookupBrowser.Create(self, LookupsTable, 'Saved Filters', lc_Fil);
        try
          if LookupsBrowser.ShowModal = mrOk then
            begin
              try
                MemoStream   := LookupsTable.CreateBlobStream(LookupsTable.fldLookupValue, bmRead);
                try
                  with FilterSettings do
                    begin
                      if MemoStream.Size > 0 then
                        begin
                          btnClearClick(nil);  // clear out field data
                          try
                            LoadFromStream(MemoStream);
                          except
                            on e:Exception do
                              ErrorFmt('Error loading from saved settings [%s]', [e.message]);
                          end;

                          if not Empty(StringInPath) then
                            edtStringInPath.Text := StringInPath;
                          if not Empty(StringInFileName) then
                            edtFileNames.Text := StringInFileName;
                          if not Empty(LowDate) then
                            edtLowDate.Text := LowDate;
                          if not Empty(HighDate) then
                            edtHighDate.Text := HighDate;
                          if cbDateType.ItemIndex >= 0 then
                            self.DateType := DateType;
                          if not Empty(LowYear) then
                            edtLowYear.Text := LowYear;
                          if not Empty(HighYear) then
                            edtHighYear.Text := HighYear;
                          if not Empty(LowMonth) then
                            edtLowMonth.Text := LowMonth;
                          if not Empty(HighMonth) then
                            edtHighMonth.Text := HighMonth;
                          if not Empty(CopyCode) then
                            begin
                              edtCopyCode.Text := CopyCode;
                              cbNot.Checked    := NotCopyCode;
                            end;
                          if not Empty(KeyWords) then
                            edtKeyWords.Text := KeyWords;
                          if not Empty(FilterExpression) then
                            begin
                              fExpression := FilterExpression;
                              tblPhotoTable.SetSelectivityParserExpression(fExpression);
                              Enable_Buttons;
                            end;

                          cbUnprocessedOnly.Checked  := UnprocessedOnly;
                          cbHasSound.Checked         := HasSound;
                          cbLocationID.Checked       := HasLocationID;
                          cbHasLocationInfoInEXIF.Checked := HasLocationInfoInEXIF;
                          cbHasSceneInfo.Checked          := HasSceneInfo;
                          cbMatchWholeWordsOnly.Checked := MatchWholeWordsOnly;
                          cbAllowStrSimilarity.Checked  := AllowStrSimilarity;
                          meSS.AsInteger                := MinStrSimilarity;
                          cbFileIsMissing.Checked       := FileIsMissing;
                          cbScanComments.Checked        := ScanComments;
                          cbScanFile.Checked            := ScanFile;
                          cbScanSceneKeyWords.Checked   := ScanSceneKeyWords;
                          cbScanSceneDates.Checked      := ScanSceneDates;
                          edtFilePathNo.Text            := SelectedFolder;
                          cbIncludeSubFolders.Checked   := IncludeSubFolders;

                          fCurrentOrder                 := CurrentSortOrder;

                          with clbMediaClasses do
                            begin
                              if MediaClasses = ALLMEDIACLASSES then // all
                                Checked[0] := true
                              else
                                begin
                                  Checked[0] := false;
                                  for mc := Succ(Low(TMediaClass)) to High(TMediaClass) do
                                    Checked[ord(mc)] := mc in MediaClasses;
                                end;
                            end;
                          Include(fChanged, cr_FilterChanged);
                        end
                      else
                        Alert('MemoStream was empty!');
                    end;
                finally
                  MemoStream.Free;
                end;

              finally
                LookupsTable.Free;
              end;
            end;
        finally
          LookupsBrowser.Free;
        end;
      end;
  finally
    FilterSettings.Free;
  end;
end;

procedure TfrmPhotoDataBase.UpdateFieldinSelectedRecords1Click(
  Sender: TObject);
var
  tempPhotoTable: TPhotoTable;
  FieldName: string;
  Field: TField;
  frmReplaceFieldsInSelectedRecords: TfrmReplaceFieldsInSelectedRecords;
  ConfirmEachRecord: boolean;
  Count, Updated: integer;
  ValueParser: TParser;
  ErrorMsg, Temp: string;
  SavedRecNo: integer;
  mr: integer;
  OK: boolean;
begin
  frmReplaceFieldsInSelectedRecords := TfrmReplaceFieldsInSelectedRecords.Create(self, tblPhotoTable);
  try
    ConfirmEachRecord  := Yes('Do you want to confirm each record?');
    if ConfirmEachRecord then
      begin
        FreeAndNil(frmConfirmEachPhoto);
        frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update')
      end;

    with frmReplaceFieldsInSelectedRecords do
      begin
        if ShowModal = mrOk then
          begin
            Update_Status('Now processing');
            Count              := 0;
            Updated            := 0;
            tempPhotoTable     := TPhotoTable.Create( self,
                                                      CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                      cFILENAMES,
                                                      []);
            fLogPathFileName := CalcLogFileName('ReplacedFieldInRecords.txt');
            AssignFile(fLogFile, fLogPathFileName);
            Rewrite(fLogFile);
            try
              with tempPhotoTable do
                begin
                  OnFilterRecord := PhotoTableFilterRecord;

                  Filtered       := true;

                  Active         := true;
                  SetSelectivityParserExpression(fExpression);
                  Field          := FindField(frmReplaceFieldsInSelectedRecords.FieldToModify);
                  if Assigned(Field) then
                    begin
                      try
                        ValueParser    := TParser.Create;
                        try
                          tempPhotoTable.AddOptionalFunctionsToParser(ValueParser);
                          if ValueParser.Valid_Expr(frmReplaceFieldsInSelectedRecords.Expression, tempPhotoTable, ErrorMsg) then
                            begin
                              WriteLn(fLogFile, 'Updating the field [',
                                                Field.FieldName,
                                                '] using the expression: ',
                                                frmReplaceFieldsInSelectedRecords.Expression);
                              WriteLn(fLogFile, 'Selectivity: ', fExpression);
                              WriteLn(fLogFile);

                              First;
                              while not Eof do
                                begin
                                  SavedRecNo := RecNo;
                                  ValueParser.Eval_Tree.EvaluateTree;
                                  if Field.AsString <> ValueParser.Eval_Tree.AsString then
                                    begin
                                      if ConfirmEachRecord then
                                        with frmConfirmEachPhoto do
                                          begin
                                            Caption1 := Format('Update record key [%s] from [%s] to [%s]?',
                                                               [tempPhotoTable.fldKey.AsString,
                                                                Field.AsString,
                                                                ValueParser.Eval_Tree.AsString]);
                                            Caption2 := Format('Updated/processed: %d/%d', [Updated, Count]);
                                            PhotoFileName := tempPhotoTable.PathAndFileName;
                                            mr := ShowModal;
                                            OK := mr = mrYes;
                                            if mr = mrCancel then
                                              break;
                                          end
                                      else
                                        OK := true;

                                      if OK then
                                        begin
                                          WriteLn(fLogFile, 'Updated the record key: ',
                                                            tempPhotoTable.fldKey.AsString);
                                          Edit;
                                          Writeln(fLogFile, 'Before:  ', Field.AsString);
                                          Writeln(fLogFile, 'After:   ', ValueParser.Eval_Tree.AsString);
                                          WriteLn(fLogFile);
                                          Field.AsString := ValueParser.Eval_Tree.AsString;
                                          fldUpdateReason.AsInteger := integer(ur_UpdateFieldInSelectedRecords); // tested
                                          Post;
                                          inc(Updated);
                                        end;
                                    end;

                                  if RecNo = SavedRecNo then // previous record may have been removed from the selection set
                                    Next;

                                  inc(Count);
                                  Update_Status( Format('Now processing #%d', [Count]));
                                  Application.ProcessMessages;
                                end;
                              Update_Status( Format('Processing complete. %d records updated', [Count]));
                              tblPhotoTable.Refresh;
                              Application.ProcessMessages;
                            end
                          else
                            AlertFmt('Invalid expression: %s [%s]', [frmReplaceFieldsInSelectedRecords.Expression, ErrorMsg]);
                        finally
                          ValueParser.Free;
                        end;
                      except
                        on e:Exception do
                          Alert(e.Message);
                      end;
                    end
                  else
                    AlertFmt('Field "%s" does not exist in the dataset', [FieldName]);
                end; //
            finally
              CloseFile(fLogFile);
              FreeAndNil(tempPhotoTable);
              tblPhotoTable.Refresh;
              if Updated > 0 then
                begin
                  temp := Format('Notepad.exe %s', [fLogPathFileName]);
                  FileExecute(temp, false);
                end
              else
                Message('No records were updated');
            end;
          end;
      end;
  finally
    FreeAndNil(frmReplaceFieldsInSelectedRecords);
  end;
end;

procedure TfrmPhotoDataBase.ParameterChanged(Sender: TObject);
begin
  Include(fChanged, cr_ParameterChanged);
end;

procedure TfrmPhotoDataBase.CreateThumbNail(var Msg: string; OverWrite: boolean);
var
  mt: TMediaType;
begin
  mt := MediaTypeFromExtension(ExtractFileExt(tblPhotoTable.fldFILE_NAME.AsString));
  if IsPhotoMedia(mt) then
    case UpdateThumbNail(tblPhotoTable, Msg, OverWrite) of
      tps_Ignored:
        Msg := 'The current thumbnail is up to date and was not changed';
      tps_CreatedUpdated:
        Msg := 'The thumbnail was created/updated';
      tps_Error:
        { Message already set };
    end
  else
    Msg := Format('Files of type %s do not have thumbnails', [MediaInfoArray[mt].Desc]);
end;


procedure TfrmPhotoDataBase.CreateThumbnail1Click(Sender: TObject);
var
  Message: string;
begin
  CreateThumbNail(Message, true);
  Update_Status( Message);
end;

function TfrmPhotoDataBase.UpdateThumbnail(PhotoTable: TPhotoTable; var Msg: string;
                                           OverWrite: boolean): TThumbnailProcessingStatus;
var
  ErrorMsg, ThumbNailName: string;
  RotateBy: integer;
begin
  with PhotoTable do
    begin
      ThumbNailName := ThumbNailPathAndName(PathAndFileName);
      RotateBy := RotationNeeded(PhotoTable.PathAndFileName);

      result        := MyCreateThumbNail( PathAndFileName,
                                          ThumbNailName,
                                          ErrorMsg,
                                          THUMBNAILWIDTH,
                                          THUMBNAILHEIGHT,
                                          OverWrite,
                                          RotateBy);
      case result of
        tps_Ignored:
          Msg := Format('Thumbnail [%s] appears to be up to date', [ThumbNailName]);
        tps_CreatedUpdated:
          Msg := Format('Thumbnail [%s] was created or updated', [ThumbNailName]);
        tps_Error:
          Msg := Format('Unable to create/update thumbnail "%s" [%s]: ', [ThumbNailName, ErrorMsg]);
      end;
    end;
end;


procedure TfrmPhotoDataBase.UpdateThumbnailsforSelectedRecords1Click(
  Sender: TObject);
var
  Count, ErrCount: integer;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
  OutFile: textfile;
  OutFileName: string;
  ErrorMsg, Temp: string;
  mt: TMediaType;
  OverWrite, DoAll: boolean;
begin
  Update_Status('Updating thumbnails for selected records');
  SavedCursor := Screen.Cursor;
  Cursor      := crSQLWait;
  OutFileName := CalcLogFileName('UpdatedThumbNails.txt');
  AssignFile(OutFile, OutFileName);
  ReWrite(OutFile);
  WriteLn(OutFile, 'Created/Updated ThumbNails for the following files [', DateTimeToStr(Now), ']:');
  WriteLn(OutFile);
  DoAll     := Yes('New thumbnail for all selected? ');
  if DoAll then
    OverWrite := true
  else
    OverWrite := Yes('If thumbnail already exists, overwrite it?');

  try
    Application.ProcessMessages;
    StartTime         := Now;
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          [optNoSyncFilePathTable, optReadOnly]);
    with tempPhotoTable do
      begin
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        First;
        Count          := 0;
        ErrCount       := 0;
        while not Eof do
          begin
            mt := MediaTypeFromExtension(ExtractFileExt(fldFILE_NAME.AsString));
            if IsPhotoMedia(mt) or IsVideoMedia(mt) then
              case UpdateThumbNail(tempPhotoTable, ErrorMsg, OverWrite) of
                tps_Error:
                  begin
                    Inc(ErrCount);
                    WriteLn(OutFile, ErrCount:3, ': ', ErrorMsg);
                    Update_Status( Format('%d errors. %s [%s]',
                                                [ErrCount,
                                                 ElapsedTimeToStr(Now - StartTime),
                                                 ErrorMsg]));
                    Application.ProcessMessages;
                  end;
                tps_CreatedUpdated:
                  begin
                    Inc(Count);
                    WriteLn(OutFile, Count:3, ': ', PathAndFileName);
                    Update_Status( Format('%d thumbnails updated. %s', [Count, ElapsedTimeToStr(Now - StartTime)]));
                  end;
              end;
            Next;
          end;
        EndTime := Now;
        ElapsedTime := (EndTime - StartTime);
        Update_Status( Format('Complete. %d thumbnails updated. %s', [Count, ElapsedTimeToStr(ElapsedTime)]));
        Active         := false;
      end;
    FreeAndNil(tempPhotoTable);
  finally
    Screen.Cursor := SavedCursor;
    CloseFile(OutFile);
    temp := Format('Notepad.exe %s', [OutFileName]);
    FileExecute(temp, false);
  end;
end;

procedure TfrmPhotoDataBase.FillDateAddedfromFileDate1Click(
  Sender: TObject);
var
  Count, ErrCount: integer;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
  FileName: string;
  OkIfNotBlank: boolean;
  Saved_RecNo: integer;
  LogPathFileName, Temp: string;
begin
  OkIfNotBlank := Yes('Is it OK to update records with non-blank date?');
  Update_Status('Filling "Date Added" from File Date for selected records');
  SavedCursor := Screen.Cursor;
  Cursor      := crSQLWait;
  ErrCount    := 0;
  try
    Application.ProcessMessages;
    StartTime         := Now;
    LogPathFileName   := CalcLogFileName('DateAddedFromFileDate.txt');
    AssignFile(fLogFile, LogPathFileName);
    Rewrite(fLogFile);
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          []);
    with tempPhotoTable do
      begin
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        First;
        Count          := 0;
        while not Eof do
          begin
            Saved_RecNo := RecNo;
            if OkIfNotBlank or (fldAdded.IsNull) or (fldAdded.AsDateTime = 0) then
              begin
                Edit;
                FileName := PathAndFileName;
                try
                  fldAdded.AsDateTime := FileDateToDateTime(FileAge(FileName));
                except
                  on e:Exception do
                    begin
                      Inc(ErrCount);
                      WriteLn(fLogFile, 'Unable to calculate DateTime for file ', FileName, ' (', e.Message, ')');
                    end;
                end;
                fldUpdateReason.AsInteger := integer(ur_DateAddedFromFileDate);  // tested
                Post;
                Inc(Count);
              end;
            if Saved_RecNo = RecNo then
              Next; // otherwise the record is no longer in the selection set
          end;
        EndTime := Now;
        ElapsedTime := (EndTime - StartTime);
        Update_Status( Format('Complete. %d records updated. %d errors. Time=%s', [Count, ErrCount, ElapsedTimeToStr(ElapsedTime)]));
        Active         := false;
      end;
  finally
    FreeAndNil(tempPhotoTable);
    tblPhotoTable.Refresh;
    CloseFile(fLogFile);
    if ErrCount > 0 then
      begin
        temp := Format('Notepad.exe %s', [LogPathFileName]);
        FileExecute(temp, false);
      end;
    Screen.Cursor := SavedCursor;
  end;
end;

{$IfDef dhd}
function TfrmPhotoDataBase.GetBoundariesTable: TDataset {TBoundariesTable};
var
  lfn, Msg: string;
begin
  if not Assigned(fBoundariesTable) then
    begin
      if not FileExists(CommonPhotoSettings.HikingDBDatabaseFileName) then
        begin
          Lfn := CommonPhotoSettings.HikingDBDatabaseFileName;
          Msg := Format('GetBoundariesTable: Hiking table [%s] does not exist', [Lfn]);
          if BrowseForFile(Msg, Lfn, MDB_EXT) then
            CommonPhotoSettings.HikingDBDatabaseFileName := lfn
          else
            raise Exception.Create(Msg);
        end;
      fBoundariesTable := TBoundariesTable.Create(self, CommonPhotoSettings.HikingDBDatabaseFileName, BOUNDING_RECTANGLES, []);
      fBoundariesTable.Active := true;
    end;
  result := fBoundariesTable;
end;

function TfrmPhotoDataBase.MyBoundariesBrowser: TfrmBoundariesBrowser;
begin
  if not Assigned(fBoundariesBrowser) then
    fBoundariesBrowser := TfrmBoundariesBrowser.Create(nil, GetBoundariesTable, (GetBoundariesTable as TBoundariesTable).TableName);
  result := fBoundariesBrowser;
end;
{$EndIf}

procedure TfrmPhotoDataBase.BrowseBoundaries1Click(Sender: TObject);
begin
{$IfDef dhd}
  MyBoundariesBrowser.Show;
{$EndIf}
end;

procedure TfrmPhotoDataBase.Post1Click(Sender: TObject);
begin
  with tblPhotoTable do
    if State in [dsInsert, dsEdit] then
      Post;
end;

procedure TfrmPhotoDataBase.edtMaxPhotosChange(Sender: TObject);
begin
  fMaxPhotosChanged := true;
end;

procedure TfrmPhotoDataBase.tabThumbNailsResize(Sender: TObject);
begin
  ReDisplayThumbnails(fThumbNailsShowing = 0);
end;

procedure TfrmPhotoDataBase.ImportPhotos1Click(Sender: TObject);
var
  OrgRecNo: integer;
begin
  OrgRecNo := tblPhotoTable.RecNo;
  if not Assigned(frmScanForDocuments) then
    frmScanForDocuments := TfrmScanForDocuments.Create(self);
  frmScanForDocuments.ProcessKind := pk_ImportPhotos;
  frmScanForDocuments.ShowModal;
  try
    tblPhotoTable.Refresh;
    if OrgRecNo > 0 then
      tblPhotoTable.RecNo := OrgRecNo;
  except
  end;
end;

procedure TfrmPhotoDataBase.SameKeyWordsasLastPost1Click(Sender: TObject);
begin
  with tblPhotoTable do
    begin
      Edit;
      fldKEY_WORDS.AsString := fLastKeywords;
    end;
end;

procedure TfrmPhotoDataBase.SameLocationasPreviousPost1Click(
  Sender: TObject);
var
  OK: boolean;
begin
  with tblPhotoTable do
    begin
      Edit;
      Ok := true;
      if fldLocationID.AsInteger > 0 then
        Ok := YesFmt('Ok to overwrite current location ID (%d) info with info from Location ID %d?', [fldLocationID.AsInteger, fLastLocationID]);
      if Ok then
        begin
          fldLocationID.AsInteger := fLastLocationID;
          fldLatitude.AsFloat     := fLastLatitude;
          fldLongitude.asFloat    := fLastLongitude;
          PhotoTableAfterScroll(tblPhotoTable);
        end;
    end;
end;

procedure TfrmPhotoDataBase.DateTimeCalculation1Click(Sender: TObject);
begin
  with TfrmCalcTimeDiff.Create(self) do
    begin
      ShowModal;
      Free;
    end;
end;

function TfrmPhotoDataBase.BuildDistanceIndexTable: boolean;
var
  Count: integer;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
  frmFillLocationInfo: TfrmFillLocationInfo;
  OriginLatitude: double;
  OriginLongitude: double;
  theDistance: double;
  Lat, Lon: double;

  procedure ClearDistance;
  begin
    with tempPhotoTable do
      begin
        Edit;
        fldDistance.Clear;
        fldUpdateReason.AsInteger := integer(ur_ClearDistance);   // tested
        Post;
      end;
  end;

begin { TfrmPhotoDataBase.BuildDistanceIndexTable }
  Update_Status('Building distance index table');
  SavedCursor := Screen.Cursor;
  frmFillLocationInfo := TfrmFillLocationInfo.Create(self);
  fBuildingIndexTable := true;
  try
    frmFillLocationInfo.LocationSearchType := st_Location;
    frmFillLocationInfo.Latitude           := tblPhotoTable.GetLatitude;
    frmFillLocationInfo.Longitude          := tblPhotoTable.GetLongitude;
    frmFillLocationInfo.MaxDistance        := fMaxDistance;
    frmFillLocationInfo.MaxDistanceUnits   := muMiles;
    result                                 := frmFillLocationInfo.ShowModal = mrOK;
    if result then
      begin
        try
          Cursor          := crSQLWait;
          StartTime       := Now;
          OriginLatitude  := frmFillLocationInfo.Latitude;
          OriginLongitude := frmFillLocationInfo.Longitude;
          fMaxDistance    := frmFillLocationInfo.MaxDistance;
          if Assigned(tempPhotoTable) then
            FreeAndNil(tempPhotoTable);
          tempPhotoTable  := TPhotoTable.Create( self,
                                                CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                cFILENAMES,
                                                [optNoSyncFilePathTable,
                                                 optNoFilePathsTable,
                                                 optNoCopyRightsTable,
                                                 optNoUpdateDate]);
          with tempPhotoTable do
            begin
              Active         := true;
              SetSelectivityParserExpression(fExpression);
              OnFilterRecord := PhotoTableFilterRecord;
              Filtered       := true;
              First;
              Count          := 0;
              while not Eof do
                begin
                  Lat := GetLatitude;
                  Lon := GetLongitude;
                  if (Lat <> 0) or (lon <> 0) then
                    begin
                      theDistance := RoundTo( Distance( OriginLatitude,
                                                        OriginLongitude,
                                                        Lat,
                                                        Lon,
                                                        frmFillLocationInfo.MaxDistanceUnits),
                                              -4);
                      Edit;
                      fldDistance.AsFloat    := theDistance;
                      fldUpdateReason.AsInteger    := integer(ur_SettingDistance);  // tested
                      Post;
                    end
                  else
                    ClearDistance;

                  Inc(Count);
                  Next;
                  ElapsedTime := (Now - StartTime);
                  Update_Status( Format('%d records scanned. %s', [Count, ElapsedTimeToStr(ElapsedTime)]));
                end;
              EndTime := Now;
              ElapsedTime := (EndTime - StartTime);
              Update_Status( Format('%d records scanned. %s', [Count, ElapsedTimeToStr(ElapsedTime)]));
              Active         := false;
            end;
          FreeAndNil(tempPhotoTable);
        finally
          Screen.Cursor := SavedCursor;
        end;
      end;
  finally
    frmFillLocationInfo.Free;
    fBuildingIndexTable := false;
  end;
end;  { TfrmPhotoDataBase.BuildDistanceIndexTable }


procedure TfrmPhotoDataBase.FillLatitudeLongitudefromLocationID1Click(
  Sender: TObject);
var
  MaxCount, Count, Fixed: integer;
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Fixed          := 0;
      First;
      while not eof do
        begin
          if (fldLocationID.AsInteger > 0) then
            begin
              if LocationsTable.Locate(cID, fldLocationID.AsInteger, []) then
                begin
                  Edit;
                  fldLatitude.AsFloat  := LocationsTable.fldLatitude.AsFloat;
                  fldLongitude.AsFloat := LocationsTable.fldLongitude.AsFloat;
                  fldUpdateReason.AsInteger := integer(ur_LatLonFromLocation);   // tested
                  Post;
                end;
              inc(Fixed);
            end;
          Next;
          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
        end;
    end;
  try
    Update_Status( Format('COMPLETE. %d recs updated', [Fixed]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
  end;
end;

procedure TfrmPhotoDataBase.CopySelectedFiles;
var
  i: integer;
  Folder: string;
  SrcFileName, DstFileName: string;
  OK: boolean;
begin
  with tblPhotoTable do
    if (Eof and Bof) then exit;

  if MyUtils.BrowseForFolder('Select Output Location', Folder) then
    begin
      with dbGrid1 do
        if SelectedRows.Count > 0 then
          begin
            for i := 0 to SelectedRows.Count-1 do
              begin
                tblPhotoTable.Bookmark := SelectedRows[i];
                SrcFileName := tblPhotoTable.PathAndFileName;
                DstFileName := ForceBackSlash(Folder) + ExtractFileName(SrcFileName);
                OK          := not FileExists(DstFileName);
                if not OK then
                  OK := YesFmt('The destination file "%s" already exists. Do you want to overwrite it?', [DstFileName]);
                if OK then
                  if not CopyFile(pChar(SrcFileName), pChar(DstFileName), true) then
                    AlertFmt('Unable to copy "%s" to "%s"', [SrcFileName, DstFileName]);
              end;
          end;
    end;
end;

procedure TfrmPhotoDataBase.dbGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fDragPoint := Point(X,Y);
end;

procedure TfrmPhotoDataBase.CopySelectedFiles1Click(Sender: TObject);
begin
  CopySelectedFiles;
end;

procedure TfrmPhotoDataBase.UpdateHeightWidthforSelectedPhotos1Click(
  Sender: TObject);
var
  Count, ErrCount: integer;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
  OutFile: textfile;
  OutFileName: string;
  ErrorMsg, Temp: string;
  mt: TMediaType;
  UpdateInfo: TUpdateInfo;
begin
  Update_Status('Updating height/width for selected records');
  SavedCursor := Screen.Cursor;
  Cursor      := crSQLWait;
  OutFileName := CalcLogFileName('UpdatedHeightWidth.txt');
  AssignFile(OutFile, OutFileName);
  ReWrite(OutFile);
  WriteLn(OutFile, 'Updated Height/Width for the following files [', DateTimeToStr(Now), ']:');
  try
    Application.ProcessMessages;
    StartTime         := Now;
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          []);
    with tempPhotoTable do
      begin
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        First;
        Count          := 0;
        ErrCount       := 0;
        while not Eof do
          begin
            mt := MediaTypeFromExtension(ExtractFileExt(fldFILE_NAME.AsString));
            if IsPhotoMedia(mt) or IsVideoMedia(mt) then
              begin
                UpdateInfo.ImagePathName := PathAndFileName;
                UpdateInfo.FileName      := fldFILE_NAME.AsString;
                if GetPhotoInfo(UpdateInfo) then
                  begin
                    Edit;
                    fldWidth.AsInteger  := UpdateInfo.PixelWidth;
                    fldHeight.AsInteger := UpdateInfo.PixelHeight;
                    fldUpdateReason.AsInteger := integer(ur_HeightWidthForSelectedPhotos); // tested
                    Post;
                    Inc(Count);
                    WriteLn(OutFile, Count:3, ': ', PathAndFileName);
                    Update_Status( Format('%d records updated. %s', [Count, ElapsedTimeToStr(Now - StartTime)]));
                  end
                else
                  begin
                    Inc(ErrCount);
                    WriteLn(OutFile, ErrCount:3, ': ', ErrorMsg);
                    Update_Status( Format('%d errors. %s [%s]',
                                                [ErrCount,
                                                 ElapsedTimeToStr(Now - StartTime),
                                                 ErrorMsg]));
                    Application.ProcessMessages;
                  end;
              end;
            Next;
          end;
        EndTime := Now;
        ElapsedTime := (EndTime - StartTime);
        Update_Status( Format('Complete. %d thumbnails updated. %s', [Count, ElapsedTimeToStr(ElapsedTime)]));
        Active         := false;
      end;
    FreeAndNil(tempPhotoTable);
  finally
    Screen.Cursor := SavedCursor;
    CloseFile(OutFile);
    temp := Format('Notepad.exe %s', [OutFileName]);
    FileExecute(temp, false);
  end;
end;

procedure TfrmPhotoDataBase.FillTextInFilewithTextFromFile1Click(
  Sender: TObject);
const
  MAXBLOBSIZE = 32768;
var
  Count, ErrCount: integer;
  StartTime, EndTime, ElapsedTime: double;
  SavedCursor: TCursor;
  OutFile: textfile;
  OutFileName: string;
  TheFileName, Temp: string;
  mt: TMediaType;
//UpdateInfo: TUpdateInfo;
  PUNCT_CRLF: TSetOfChar;

  GetPdfText: TGetPdfText;
  GetPDFTextFuncPtr: TFarProc;

  GetPDFPageCount: TGetPDFPageCount;
  GetPDFPageCountFuncPtr: TFarProc;

  DLLHandle: THandle;

  procedure FilterStream(InputStream, OutputStream: TStream);
  var
    InBuffer, OutBuffer, ip, op: pchar;
    Len: Longint;
    i: integer;
    Temp: string;
    SkippingBlanksMode: (sbNotABlank, sbFirstBlank, sbSuccessiveBlanks);
  begin
    Len := InputStream.Size;
    if Len > MAXBLOBSIZE then
      begin
        Len := MAXBLOBSIZE;
        Temp := Format('Truncated the file %s from %d bytes to %d bytes', [InputStream.Size, MAXBLOBSIZE]);
        WriteLn(Outfile, Temp);
        lblStatus.Color := clYellow;
        Update_Status( Temp);
        Inc(ErrCount);
      end;
    GetMem(InBuffer, Len);
    GetMem(OutBuffer, Len);
    ip := InBuffer;
    op := OutBuffer;
    OutputStream.Size := 0;
    OutputStream.Position := 0;
    InputStream.Read(InBuffer^, Len);
    if Len > 0 then
      begin
        SkippingBlanksMode := sbNotABlank;

        for i := 0 to Len - 1 do
          begin
            if ip^ in ALPHA_UPPER then
              op^ := chr(ord(ip^) - ord('A') + ord('a')) else
            if ip^ in PUNCT_CRLF then
              op^ := ' '
            else
              op^ := iP^;

            case SkippingBlanksMode of
              sbNotABlank:
                begin
                  if op^ = ' ' then
                    SkippingBlanksMode := sbFirstBlank;
                  inc(op);
                end;
              sbFirstBlank:
                begin
                  if op^ = ' ' then
                    SkippingBlanksMode := sbSuccessiveBlanks
                  else
                    begin
                      SkippingBlanksMode := sbNotABlank;
                      inc(op);
                    end;
                end;
              sbSuccessiveBlanks:
                begin
                  if op^ <> ' ' then
                    begin
                      SkippingBlanksMode := sbNotABlank;
                      inc(op);
                    end;
                end;
            end;

            inc(ip);
          end;
      end;
    Len := op - OutBuffer;
    OutputStream.Write(OutBuffer^, Len);
    FreeMem(InBuffer);
    FreeMem(OutBuffer);
  end;


  procedure LoadTextFromTextFile(const FileName: string);
  var
    FileStream: TFileStream;
    OutputStream: TStream;
  begin
    TheFileName  := FileName;
    FileStream   := TFileStream.Create(FileName, fmOpenRead	+ fmShareDenyWrite);
    try
      tempPhotoTable.Edit;
      OutputStream := tempPhotoTable.CreateBlobStream(tempPhotoTable.fldTextInFile, bmWrite);

      try
        FilterStream(FileStream, OutputStream);
      finally
        FileStream.Free;
        OutputStream.Free;
        TempPhotoTable.fldUpdateReason.AsInteger := integer(ur_LoadTextFromFile);
        TempPhotoTable.Post;
        inc(Count);
        WriteLn(OutFile, Count: 5, '. ', ExtractFileName(FileName));
      end;
    except
      Update_Status( Format('Unable to load file: %s', [FileName]));
      WriteLn(OutFile, 'Unable to load file: ', FileName);
      ErrCount := ErrCount + 1;
    end;
  end;

  procedure LoadTextFromPDFFile(const FileName: string);
  var
    FileContents: string;
    StringStream: TStringStream;
    OutputStream: TStream;
  begin { LoadTextFromPDFFile }
    // dynamic load for the dll and textextraction
    if DLLHandle = 0 then
      begin
        DLLHandle := LoadLibrary(PChar('PDFtext.dll'));
        if DLLHandle <> 0 then
          begin
            GetPDFTextFuncPtr   := GetProcAddress(DLLHandle, 'GetPDFText');
            @GetPdfText := GetPDFTextFuncPtr;

            GetPDFPageCountFuncPtr := GetProcAddress(DLLHandle, 'GetPDFPageCount');
            @GetPDFPageCount := GetPDFPageCountFuncPtr;
          end
        else
          raise Exception.CreateFmt('Unable to load DLL: %s', ['PDFtext.dll']);
      end;

    try
      FileContents := GetPdfText(PWideChar(WideString(FileName)),
                                3  {opt},
                                0  {hw},
                                1  {fast},
                                PWideChar(WideString('')),
                                0  {lspaces- don't delete leading spaces},
                                PWideChar(WideString('')),
                                0  {pos},
                                0  {page},
                                0  {clock},
                                0  {blank},
                                0  {end},
                                0  {wlist});
      if FileContents <> '9' then
        begin
          StringStream := TStringStream.Create(FileContents);

          tempPhotoTable.Edit;
          OutputStream := tempPhotoTable.CreateBlobStream(tempPhotoTable.fldTextInFile, bmWrite);
          try
            FilterStream(StringStream, OutputStream);
          finally
            StringStream.Free;
            OutputStream.Free;
            TempPhotoTable.fldUpdateReason.AsInteger := integer(ur_LoadTextFromPDF);
            TempPhotoTable.Post;
            inc(Count);
            WriteLn(OutFile, Count: 5, '. ', ExtractFileName(FileName));
          end;
        end
      else
        AlertFmt('No text contained in "%s"', [FileName]);

    except
      Update_Status( Format('Unable to load file: %s', [FileName]));
      WriteLn(OutFile, 'Unable to load file: ', FileName);
      ErrCount := ErrCount + 1;
    end;
  end;  { LoadTextFromPDFFile }

begin
  Update_Status('Loading Text from selected records');
  lblStatus.Color   := clBtnFace;
  SavedCursor := Screen.Cursor;
  Cursor      := crSQLWait;
  OutFileName := CalcLogFileName('LoadedTextFiles.txt');
  AssignFile(OutFile, OutFileName);
  ReWrite(OutFile);
  WriteLn(OutFile, 'Loaded text from the following files [', DateTimeToStr(Now), ']:');
  PUNCT_CRLF := PUNCT + [#13, #10];
  try
    Application.ProcessMessages;
    StartTime         := Now;
    tempPhotoTable := TPhotoTable.Create( self,
                                          CommonPhotoSettings.PhotoDBDatabaseFileName,
                                          cFILENAMES,
                                          []);
    with tempPhotoTable do
      begin
        Active         := true;
        FillTextInFilewithTextFromFile1.Enabled := Assigned(fldTextInFile);
        SetSelectivityParserExpression(fExpression);
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        First;
        Count          := 0;
        ErrCount       := 0;
        while not Eof do
          begin
            mt := MediaTypeFromExtension(ExtractFileExt(fldFILE_NAME.AsString));
            case mt of
              mt_TXT:
                LoadTextFromTextFile(PathAndFileName);
              mt_PDF:
                LoadTextFromPDFFile(PathAndFileName);
            end;
            if (Count mod 25) = 0 then
              Update_Status( Format('Processing: %s', [fldFile_Name.AsString]));
            Next;
          end;
        EndTime := Now;
        ElapsedTime := (EndTime - StartTime);
        Update_Status( Format('Complete. %d records updated. %s', [Count, ElapsedTimeToStr(ElapsedTime)]));
        Active         := false;
      end;
    FreeAndNil(tempPhotoTable);
  finally
    if DLLHAndle <> 0 then
      begin
        GetPDFTextFuncPtr := nil;
        GetPDFPageCountFuncPtr := nil;
        FreeLibrary(DLLHandle);
      end;
    Screen.Cursor := SavedCursor;
    CloseFile(OutFile);
    temp := Format('Notepad.exe %s', [OutFileName]);
    FileExecute(temp, false);
  end;
end;

procedure TfrmPhotoDataBase.dbGrid1DblClick(Sender: TObject);
begin
  if Sender is TDBGrid then
    with Sender as TDBGrid do
      begin
        if not Assigned(frmBrowseMemo) then
          frmBrowseMemo := TfrmBrowseMemo.Create( self,
                                                  {TDBGrid.}DataSource,
                                                  SelectedField,
                                                  bhAsText);
        frmBrowseMemo.Show;
      end;
end;

procedure TfrmPhotoDataBase.cbDisplayTextInFileClick(Sender: TObject);
begin
  if cbDisplayTextInFile.Checked then
    with tblPhotoTable do
      begin
        if not Assigned(frmBrowseMemo) then
          begin
            frmBrowseMemo := TfrmBrowseMemo.Create( self,
                                                  dsPhotoTable,
                                                  fldTextInFile,
                                                  bhAsText);
            frmBrowseMemo.FreeNotification(self);
            frmBrowseMemo.TheCloseAction := caFree;
          end;
        frmBrowseMemo.Show;
      end
  else
    FreeAndNil(frmBrowseMemo);
end;

{$IfDef dhd2}
procedure TfrmPhotoDataBase.SetIsDHD(value: boolean);
begin
  if Value <> fIsDHD then
    begin
      BrowseBoundaries1.Visible := Value;
      IsDHD1.Checked := Value;
      fIsDHD         := Value;
    end;
end;
{$EndIf}


procedure TfrmPhotoDataBase.IsDHD1Click(Sender: TObject);
begin
{$IfDef dhd2}
  IsDHD := not IsDHD;
{$EndIf}
end;

{$IfDef dhd2}
function TfrmPhotoDataBase.GetIsDHD: boolean;
begin
  result := IsDHD1.Checked;
end;
{$EndIf}

function TfrmPhotoDataBase.GetDateType: TDateTypes;
begin
  with cbDateType do
    if ItemIndex >= 0 then
      result := TDateTypes(Items.Objects[ItemIndex])
    else
      result := dt_PhotoDate;
end;

procedure TfrmPhotoDataBase.SetDateType(const Value: TDateTypes);
begin
  with cbDateType do
    ItemIndex := Items.IndexOfObject(TObject(Value));
end;

procedure TfrmPhotoDataBase.MatchSelectedFilestoFilesinFolder1Click(
  Sender: TObject);
type
  TFileRecord = record
                  FileName: string;
                  FileDateTime: TDateTime;
                  FileSize: integer;
                  NearestMatchingIndex: smallint;
                  MatchingFileFound: boolean;
                end;
  TFileList = array of TFileRecord;
var
  tempPhotoTable: TPhotoTable;
  NumberOfSelectedRecords, NumberOfFilesToSearch: integer;
  SelectedFiles: TFileList;
  FilesToSearch: TFileList;
  SearchRec: TSearchRec;
  FolderForFilesToCompare: string;
  DOSErr: integer;
  TempFileRecord: TFileRecord;
  done: boolean;
  Count, i, j, t, b: integer;
//Year, Month, Day, Hr, Min, Sec: word;
//TempDateStr: string;
//ValidExifDate: boolean;
  ImgData: TImgData;
  OutFile: TextFile;
  TempFileName: string;
  temp: string;
begin
  try
    Update_Status('Now processing');
    ImgData            := TImgData.Create();

    // Get the name of the folder which contains the files to be matched
    FolderForFilesToCompare := 'C:\temp';
    if not BrowseForFolder('Select folder containing the files to be matched', FolderForFilesToCompare) then
      Exit;

    tempPhotoTable     := TPhotoTable.Create( self,
                                              CommonPhotoSettings.PhotoDBDatabaseFileName,
                                              cFILENAMES,
                                              []);
    with tempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;

        Filtered       := true;

        Active         := true;
        SetSelectivityParserExpression(fExpression);

        // count the selected files
        NumberOfSelectedRecords           := 0;
        First;
        while not eof do
          begin
            inc(NumberOfSelectedRecords);
            Next;
          end;

        // make a list of them
        SetLength(SelectedFiles, NumberOfSelectedRecords);

        Count := 0;
        First;
        while not Eof do
          begin
            with SelectedFiles[Count] do
              begin
                FileName     := fldFile_Name.AsString;
                FileSize     := fldFile_Size.AsInteger;
                FileDateTime := fldPhotoDateTime.AsDateTime;
                NearestMatchingIndex := -1;
                MatchingFileFound    := false;
              end;
            Next;
            inc(Count);
          end;
        Assert(Count = NumberOfSelectedRecords, 'System error: count <> NumberOfSelectedRecords');

        // Count the number of files to be searched
        FolderForFilesToCompare := ForceBackSlash(FolderForFilesToCompare);

        DOSErr := SysUtils.FindFirst(FolderForFilesToCompare + '*.*', 0, SearchRec);
        NumberOfFilesToSearch := 0;
        while DOSErr = 0 do
          begin
            inc(NumberOfFilesToSearch);
            DOSErr := SysUtils.FindNext(SearchRec);
          end;

        // make a list of the files to search

        SetLength(FilesToSearch, NumberOfFilesToSearch);
        DOSErr := SysUtils.FindFirst(FolderForFilesToCompare + '*.*', 0, SearchRec);
        Count := 0;
        while DOSErr = 0 do
          with FilesToSearch[Count] do
            begin
              FileName     := SearchRec.Name;
              FileSize     := SearchRec.Size;
              FileDateTime := FileDateToDateTime(SearchRec.Time);
              // Parse the EXIF data to get a date
              if ImgData.ProcessFile(FolderForFilesToCompare + FileName) then
                if ImgData.HasEXIF then
                  begin
//                  TempDateStr      := ImgData.ExifObj.DateTime;
//                  ValidExifDate := DecodeExifDate(TempDateStr, Year, Month, Day, Hr, Min, Sec);
//                  if ValidExifDate then
//                    FileDateTime := EncodeDateTime(Year, Month, Day, Hr, Min, Sec, 0);
                    FileDateTime := ImgData.ExifObj.GetImgDateTime;
                  end;
              NearestMatchingIndex := -1;
              MatchingFileFound    := false;
              inc(Count);
              DOSErr := SysUtils.FindNext(SearchRec);
            end;
        SysUtils.FindClose(SearchRec);
        Assert(Count = NumberOfFilesToSearch, 'System error: count <> NumberOfFileToSearch');

        // sort the list of files to search by date/time
        for i := 0 to count-2 do
          for j := i + 1 to count-1 do
            if FilesToSearch[i].FileDateTime > FilesToSearch[j].FileDateTime then
              begin
                TempFileRecord   := FilesToSearch[i];
                FilesToSearch[i] := FilesToSearch[j];
                FilesToSearch[j] := TempFileRecord;
              end;

        // find the closest matching file for each of the selected files
        for i := 0 to NumberOfSelectedRecords-1 do
          begin
            Update_Status( Format('Now trying to match "%s"', [SelectedFiles[i].FileName]));
            t := 0;
            b := NumberOfFilesToSearch-1;
            done := false;
            repeat
              j := (t + b) div 2;
              if b < 0 then
                done := true else
              if t >= NumberOfFilesToSearch then
                done := true else
              if t > b then
                done := true else
              if SelectedFiles[i].FileDateTime < FilesToSearch[j].FileDateTime then // it should be above j
                b := j - 1 else
              if SelectedFiles[i].FileDateTime > FilesToSearch[j].FileDateTime then // it should be below us
                t := j + 1 else
              if Abs(SecondSpan(SelectedFiles[i].FileDateTime, FilesToSearch[j].FileDateTime)) < 10 then // we found a file with the same date/time
                begin
                  done := true;
                  SelectedFiles[i].MatchingFileFound    := true;
                  SelectedFiles[i].NearestMatchingIndex := j;
                  FilesToSearch[j].MatchingFileFound    := true;
                  FilesToSearch[j].NearestMatchingIndex := i;
                end;
            until done;
          end;

        // print the report
        count := 0;    // count the matching files
        for i := 0 to NumberOfFilesToSearch-1 do
          if FilesToSearch[i].MatchingFileFound then
            inc(count);

        MessageFmt('There were %d files in the folder.'+#13+#10+
                   '  %d/%d files were matched'+#13#10+
                   '  %d/%d files were NOT matched',
                   [NumberOfFilesToSearch,
                    count, NumberOfFilesToSearch,
                    NumberOfFilesToSearch-count, NumberOfFilesToSearch]);
        TempFileName := CalcLogFileName('MissingFiles.txt');
        AssignFile(OutFile, TempFileName);
        ReWrite(OutFile);
        for j := 0 to NumberOfFilesToSearch-1 do
          if not FilesToSearch[j].MatchingFileFound then
            WriteLn(OutFile, FilesToSearch[j].FileName);
        CloseFile(OutFile);
        temp := Format('Notepad.exe %s', [TempFileName]);
        FileExecute(temp, false);
        if Count > 0 then  // if there were any matching files
          if Yes('Delete the files that were matched?') then
            begin
              for j := 0 to NumberOfFilesToSearch-1 do
                if FilesToSearch[j].MatchingFileFound then
                  begin
                    TempFileName := FolderForFilesToCompare + FilesToSearch[j].FileName;
                    if not DeleteFile(FolderForFilesToCompare + FilesToSearch[j].FileName) then
                      AlertFmt('Could not delete "%s"', []);
                  end;
            end;

        if Yes('Correct the filedate for the files that were not matched?') then
          begin
            for j := 0 to NumberOfFilesToSearch-1 do
              if not FilesToSearch[j].MatchingFileFound then
                begin
                  TempFileName := FolderForFilesToCompare + FilesToSearch[j].FileName;
                  FileSetDate(TempFileName, DateTimeToFileDate(FilesToSearch[j].FileDateTime));
                end;
          end;
      end;
  finally
    FreeAndNil(TempPhotoTable);
    FreeAndNil(ImgData);
  end;
end;

procedure TfrmPhotoDataBase.PhotoTableAfterRefresh(Dataset: TDataSet);
begin
  if fCurrentRecordNumber > 0 then
    (DataSet as TPhotoTable).RecNo := fCurrentRecordNumber;
end;

procedure TfrmPhotoDataBase.PhotoTableBeforeRefresh(Dataset: TDataSet);
begin
  fCurrentRecordNumber := (DataSet as TPhotoTable).RecNo;
end;

procedure TfrmPhotoDataBase.SetFileDateTimetoPhotoDateTimeforselectedrecords1Click(
  Sender: TObject);
var
  tempPhotoTable: TPhotoTable;
  Count: integer;
  FileName: string;
  ErrNo, ErrCount: integer;
begin
  Update_Status('Now processing');
  Count              := 0;
  ErrCount           := 0;
  tempPhotoTable     := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            []);
  with tempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;

      Filtered       := true;

      Active         := true;
      SetSelectivityParserExpression(fExpression);
      while not eof do
        begin
          FileName := PathAndFileName;
          ErrNo := FileSetDate(FileName, DateTimeToFileDate(fldPhotoDateTime.AsDateTime));
          if ErrNo <> 0 then
            begin
              AlertFmt('FileSetDate error = %d on file: %s', [ErrNo, FileName]);
              inc(ErrCount);
            end
          else
            Inc(Count);
          Update_Status( Format('%d errors occurred. %d records successfully processed', [ErrCount, Count]));
          Next;
        end;
    end;
  MessageFmt('%d errors occurred. %d records successfully processed', [ErrCount, Count]);
end;

function TfrmPhotoDataBase.tblLocationsTable: TLocationsTable;
begin
  if not Assigned(ftblLocationsTable) then
    begin
      ftblLocationsTable := TLocationsTable.Create( self,
                                                    CommonPhotoSettings.PhotoDBDatabaseFileName, cLOCATIONS,
                                                    []);
//    ftblLocationsTable.AddFields;  // This slows things way down
      ftblLocationsTable.Active := true;
      ftblLocationsTable.Tag := 666;
    end;
  result := ftblLocationsTable;
end;

procedure TfrmPhotoDataBase.ScanforFilesnotinDataBase1Click(
  Sender: TObject);
var
  OrgRecNo: integer;
begin
  OrgRecNo := tblPhotoTable.RecNo;
  if not Assigned(frmScanForDocuments) then
    frmScanForDocuments := TfrmScanForDocuments.Create(self);
  frmScanForDocuments.ProcessKind := pk_ScanForMissingPhotos;
  frmScanForDocuments.ShowModal;
  try
    tblPhotoTable.Refresh;
    tblPhotoTable.RecNo := OrgRecNo;
  except
  end;
end;

// This should be made smarter. Right now it is generating HTML only for photo files
function TfrmPhotoDataBase.AcceptThisFile(const FileName: string): boolean;
begin
  result := IsPhotoMedia(MediaTypeFromExtension(ExtractFileExt(FileName)));
end;

Procedure TfrmPhotoDataBase.Log(const Message: string; LineNo: integer = 0);
begin
  WriteLn(fLogFile, Message);
end;

procedure TfrmPhotoDataBase.RemoveProcessedFolder(FolderName: string);
var
  idx: integer;
begin { RemoveProcessedFolder }
  FolderName := FolderName + '\';
  idx := fFolderList.IndexOf(FolderName);
  if idx >= 0 then
    fFolderList.Delete(Idx);
  Log(Format('%5d. %s', [fHTMLGenerator.FolderCount, FolderName]));
end;  { RemoveProcessedFolder }

procedure TfrmPhotoDataBase.GenerateHTMLforSelectedFolders1Click(
  Sender: TObject);
var
  tempPhotoTable: TPhotoTable;
  aFullFilePath: string;
  LogFileName: string;
  Temp: string;
  i: integer;
  FolderName, ParentFolderPath, FolderPath: string;
  Saved_Cursor: TCursor;

  function GetParentFolderPath(FolderPath: string): string;
  var
    OldDir, NewDir: string;
  begin { GetParentFolderPath }
    GetDir(0, OldDir);
    NewDir := FolderPath+'\..';
    ChDir(NewDir);
    GetDir(0, result);
    ChDir(OldDir);
  end;  { GetParentFolderPath }

begin { TfrmPhotoDataBase.GenerateHTMLforSelectedFolders1Click }
  Update_Status('Building List of Folders to Process');
  FreeAndNil(tempPhotoTable);
  tempPhotoTable     := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            []);
  fFolderList := TStringList.Create;
  fFolderList.Sorted := true;

  // Build a list of the folders currently selected for processing
  try
    Saved_Cursor  := Screen.Cursor;
    Screen.Cursor := crSQLWait;
    try
      with tempPhotoTable do
        begin
          OnFilterRecord := PhotoTableFilterRecord;

          Filtered       := true;

          Active         := true;
          SetSelectivityParserExpression(fExpression);
          First;
          while not eof do
            begin
              aFullFilePath := FullFilePath;
              if (fFolderList.IndexOf(aFullFilePath) < 0) and (Pos(NONEXISTANTFOLDER, aFullFilePath) <= 0) then
                fFolderList.AddObject(aFullFilePath, TObject(nil));
              Next;
            end;
        end;
    finally
      Screen.Cursor := Saved_Cursor;
    end;

    LogFileName := CalcLogFileName('HTMLGenerated.txt');
    AssignFile(fLogFile, LogFileName);
    ReWrite(fLogFile);

    fHTMLGenerator := THTMLGenerator.Create(self);
    try
      with fHTMLGenerator do
        begin
          cbIncludeEmpty.Checked      := false;   // should come from CommonSettings
          cbProcessSubFolders.Checked := true;    // force sub-folders to be processed
          cbDeleteOldHTML.Checked     := true;
          for i := 0 to fFolderList.Count-1 do
            begin
              ListBox1.Items.Add(fFolderList[i]);
              ListBox1.Checked[i] := true;
            end;

          if ShowModal = mrOK then
            begin
              Log('');
              Log('HTML Generated for these folders: ' + DateTimeToStr(Now));
              Log('');

              // Delete the unselected items

              for i := ListBox1.Count-1 downto 0 do  // assumes a 1-to-1 relation between ListBox1.Items and fFolderList
                if not ListBox1.Checked[i] then
                  fFolderList.Delete(i);;

              // now process each of the folders selected

              RowCount          := 2;        // should come from CommonSettings
              ColCount          := 3;        // should come from CommonSettings
              UpdateRecentOnly  := false;    // should come from CommonSettings
              IncludeEmpty      := cbIncludeEmpty.Checked;     // should come from CommonSettings
              ProcessSubFolders := cbProcessSubFolders.Checked;    // force sub-folders to be processed
              DeleteOldHTML     := cbDeleteOldHTML.Checked;
              DayCount          := 0;        // irrelevent if UpdateRecentOnly is false
//            FileNameInfo      := fFileNameInfo;
              FileAcceptor      := AcceptThisFile;
              AfterFolderProcessed := RemoveProcessedFolder;
              StartTime         := DWORD(GetCurrentTime);
              FolderCount       := 0;
              ThumbCount        := 0;
              OnUpdateStatus    := Update_Status;
              OnUpdateThumbStatus := Update_Status;
              OnUpdateLogFile   := self.Log;

              OnOrAfter         := Now - DayCount;

              i := 0;
              while i < fFolderList.Count do
                begin
                  FolderPath       := RemoveTrailingBackSlash(fFolderList[i]);
                  FolderName       := ExtractFileName(FolderPath);
                  ParentFolderPath := GetParentFolderPath(FolderPath);
                  ProcessFolder(ParentFolderPath, FolderName, FolderPath, '*.*');
                  Update_Status(Format('Processing: %d folders successfully processed', [fHTMLGenerator.FolderCount]));
                  inc(i);
                end;
            end;
        end;
    finally
      Update_Status(Format('COMPLETE. %d folders successfully processed', [fHTMLGenerator.FolderCount]));
      if fHTMLGenerator.FolderCount > 0 then
        begin
          CloseFile(fLogFile);
          temp := Format('Notepad.exe %s', [LogFileName]);
          FileExecute(temp, false);
        end;
      fHTMLGenerator.Free;
    end;

  finally
    FreeAndNil(tempPhotoTable);
    FreeAndNil(fFolderList);
  end;
end;  { TfrmPhotoDataBase.GenerateHTMLforSelectedFolders1Click }

procedure TfrmPhotoDataBase.FillPhotoDateTime(FromWhat: TFromWhat);
var
  Updated, Count, OldRecNo: integer;
  FileName: string;
  DateTime, EnteredDateTime: TDateTime;
  ImgData: TImgData;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: word;
  OK, ConfirmEach: boolean;
  Temp, Ext: string;
  mt: TMediaType;
  mc: TMediaClass;
  mr: integer;
  MediaFrames, MediaLength, MediaWidth, MediaHeight: Longint;
  MediaLengths: string; RecordedDate: TDateTime;
begin
  EnteredDateTime := BAD_DATE;
  OK              := false;
  if FromWhat = fw_EnteredDateTime then
    begin
      try
        repeat
          OK := GetString('File Date/Time from entered date', 'DateTime', Temp);
          if OK then
            begin
              EnteredDateTime := StrToDateTime(Temp);
              Ok := true;
            end
          else
            Exit;
        until OK;
      except
        AlertFmt('%s is not a valid Date/Time', [Temp]);
      end;
    end
  else
    OK := true;

  if OK then
    begin
      ConfirmEach := Yes('Confirm each update?');
      if ConfirmEach then
        begin
          FreeAndNil(frmConfirmEachPhoto);
          frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update')
        end;

      TempPhotoTable := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            []);
      ImgData := TImgData.Create;
      try
        with TempPhotoTable do
          begin
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            Active         := true;
            SetSelectivityParserExpression(fExpression);
            Count          := 0;
            Updated        := 0;
            First;
            while not eof do
              begin
                OldRecNo := RecNo;
                DateTime := BAD_DATE;

                FileName := TempPhotoTable.PathAndFileName;

//              if Ok then
                  Ok := false;
                  case FromWhat of
                    fw_FileDateTime:
                      begin
                        try
                          DateTime := FileDateToDateTime(FileAge(FileName));
                          Ok := true;
                        except
                          Ok := false;
                        end;
                      end;

                    fw_FileCreationDateTime:
                      begin
                        DateTime := GetFileCreationTime(FileName);
                        Ok       := true;
                      end;

                    fw_ExifInfo,
                    fw_MediaInfo:
                      begin
                        Ok  := false;
                        Ext := MyExtractFileExt(FileName);
                        mt  := MediaTypeFromExtension(Ext);
                        mc  := MediaInfoArray[mt].MediaClass;
                        if mc in [mc_Video, mc_Audio] then
                          begin
                            if GetMediaProperties(FileName,
                                                  MediaFrames, MediaLength, MediaWidth, MediaHeight,
                                                  MediaLengths,
                                                  RecordedDate) then
                              begin
                                DateTime := RecordedDate;
                                OK       := true;
                              end;
                          end;
                        if mc = mc_Photos then
                          if ImgData.ProcessFile(FileName) then
                            begin
                              try
//                              Temp := ImgData.ExifObj.DateTime;
//                              Ok   := DecodeExifDate(Temp, aYear, aMonth, aDay, AHour, AMinute, ASecond);
//                              if Ok then
//                                DateTime := EncodeDateTime(aYear, aMonth, aDay, aHour, aMinute, aSecond, aMilliSecond)
                                DateTime := ImgData.ExifObj.GetImgDateTime;
                              except
                                Ok := false;
                              end;
                            end;
                      end;

                    fw_EnteredDateTime:
                      begin
                        DateTime := EnteredDateTime;
                        OK       := true;
                      end;
                  end;

                if (DateTime <> BAD_DATE) and (fldPhotoDateTime.AsDateTime <> DateTime) then
                  if OK and ConfirmEach then
                    with frmConfirmEachPhoto do
                      begin
                        PhotoFileName := FileName;
                        lblCaption1.Caption := Format('Change the date from [%s] to [%s]?',
                                                      [fldPhotoDateTime.AsString, DateTimeToStr(DateTime)]);
                        mr := ShowModal;
                        OK := mr in [mrYes, mrOk];
                        if mr = mrCancel then
                          break;
                      end
                  else
                    OK := true;

                if Ok then
                  begin
                    DecodeDateTime(DateTime, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
                    Edit;
                    fldYear.AsInteger  := aYear;
                    fldMonth.AsInteger := aMonth;
                    fldDay.AsInteger   := aDay;
                    fldPhotoDateTime.AsDateTime := DateTime;
                    fldUpdateReason.AsInteger   := integer(ur_FillPhotoDateTime);  // tested
                    Post;
                    Inc(Updated);
                  end;

                if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
                  Next;

                inc(Count);
                if (Count mod 100) = 0 then
                  Update_Status( Format('Processed %d, Updated=%d', [Count, Updated]));
              end;
            Update_Status( Format('Complete. Processed %d, Updated=%d', [Count, Updated]));
            tblPhotoTable.Refresh;
          end;
      finally
        FreeAndNil(frmConfirmEachPhoto);
        FreeAndNil(TempPhotoTable);
        FreeAndNil(ImgData);
        tblPhotoTable.Refresh;
      end;
    end;

end;


procedure TfrmPhotoDataBase.fromFileDate1Click(Sender: TObject);
begin
  FillPhotoDateTime(fw_FileDateTime);
end;

procedure TfrmPhotoDataBase.fromFileCreationTime1Click(Sender: TObject);
begin
  FillPhotoDateTime(fw_FileCreationDateTime);
end;

procedure TfrmPhotoDataBase.fromExifDateTime1Click(Sender: TObject);
begin
  FillPhotoDateTime(fw_ExifInfo);
end;

procedure TfrmPhotoDataBase.fromEnteredDateTime1Click(Sender: TObject);
begin
  FillPhotoDateTime(fw_EnteredDateTime);
end;

procedure TfrmPhotoDataBase.cbAllowStrSimilarityClick(Sender: TObject);
begin
  meSS.Enabled := cbAllowStrSimilarity.Checked;
end;

procedure TfrmPhotoDataBase.edtFilePathNoChange(Sender: TObject);
begin
  Include(fChanged, cr_FilePathNoChange);
  cbIncludeSubFolders.Visible := not Empty(edtFilePathNo.Text);
end;

procedure TfrmPhotoDataBase.ReplaceCurrentPhoto1Click(Sender: TObject);
var
  OldLfn, NewLfn, OldExt, NewExt: string;
  UpdateInfo: TUpdateInfo;
  ThumbnailPathName: string;
  ErrorMsg: string;
begin
  OldLfn := tblPhotoTable.PathAndFileName;
  NewLfn := OldLfn;
  if BrowseForFile('Locate replacement file', NewLfn, JPG_FILE_EXT) then
    begin
      NewExt := ExtractFileExt(NewLfn);
      OldExt := ExtractFileExt(OldLfn);
      if not SameText(OldExt, NewExt) then
        begin
          AlertFmt('Change in file type (%s->%s) not permitted', [OldExt, NewExt]);
          Exit;
        end;

      If YesFmt('Replace file "%s" with file "%s"?', [OldLfn, NewLfn]) then
        begin
          if not CopyFile(pchar(NewLfn), pchar(OldLfn), false) then
            AlertFmt('Unable to replace file "%s" with file "%s"', [OldLfn, NewLfn])
          else
            begin
              with tblPhotoTable do
                begin
                  Edit;
                  fldFile_Size.AsFloat := FileSize64(OldLfn);
                  Finalize(UpdateInfo);
                  FillChar(UpdateInfo, SizeOf(UpdateInfo), 0);
                  with UpdateInfo do
                    begin
                      FileName      := OldLfn;
                      ImagePathName := OldLfn;
//                    FileNameInfo.UseExifForPhotoDateAndLocation := true;
                      with FileNameInfo do
                        DateKinds    := DateKinds + [dk_from_EXIF, dk_Date_Modified,
                                                     dk_Date_Created, dk_Last_Access,
                                                     dk_MediaInfo];
                    end;
                  if GetPhotoInfo(UpdateInfo) then
                    begin
                      fldWidth.AsInteger  := UpdateInfo.PixelWidth;
                      fldHeight.AsInteger := UpdateInfo.PixelHeight;
                    end;
                  fldUPDATED.AsDateTime := Now;
                  fldUpdateReason.AsInteger := integer(ur_ReplaceCurrentPhoto);
                  Post;

                  // update the thumbnail
                  case MyCreateThumbNail(OldLfn, ErrorMsg) of
                    tps_Error:
                      AlertFmt('Unable to create/update thumbnail "%s" [%s]: ', [ExtractFileName(ThumbnailPathName), ErrorMsg]);
                  end;
                end;
              ShowCalcFields(tblPhotoTable);
              SizePhoto;
              MyDeleteFile(NewLfn, true, true);
            end;
        end;
    end;
end;

procedure TfrmPhotoDataBase.FromEXIFInfo1Click(Sender: TObject);
var
  Latitude, Longitude: double;
  LocID: integer;
  Added, NonNumericIndex: integer;
  FoundDesc: string;
  ImgData: TImgData;
  FileName, Description, Temp : string;
begin
  ImgData := TImgData.Create();

  try
    FileName := tblPhotoTable.PathAndFileName;
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
{$EndIf dExif2}
              if (Latitude <> 0) or (Longitude <> 0) then
                with tblPhotoTable do
                  begin
                    Description := ExtractFileName(RemoveTrailingBackslash(ExtractFilePath(FileName)));  // use the last word of the path for the description
                    Temp := LeadingNumericString(Description, NonNumericIndex);
                    if (Length(Temp) > 0) and (NonNumericIndex > 0) then
                      Description := Copy(Description, NonNumericIndex+1, Length(Description)-NonNumericIndex);

                    LocationsTable.InitLocationInfo( fLocationInfo,
                                                     ls_PhotoEXIF,
                                                     temp);

                    LocID := LocationsTable.FindLocationID( Latitude, Longitude,
                                               fLocationInfo,
                                               Added,
                                               FoundDesc);
                    if LocID > 0 then
                      begin
                        Edit;
                        fldLatitude.AsFloat := Latitude;
                        fldLongitude.AsFloat := Longitude;
                        fldLocationID.AsInteger := LocID;
                        ShowCalcFields(tblPhotoTable);
                      end;
                  end;
            end;
        end;
  finally
    ImgData.Free;
  end;
end;

function TfrmPhotoDataBase.GetScenesBrowser: TfrmBrowseScenes;
begin
  if not Assigned(fScenesBrowser) then
    fScenesBrowser := TfrmBrowseScenes.Create(self, tblPhotoTable.ScenesTable);
  result := fScenesBrowser;
end;

function TfrmPhotoDataBase.GetScenesBrowser2: TfrmBrowseScenes;      // not linked to the photos table
begin
  if not Assigned(fScenesBrowser2) then
    fScenesBrowser2 := TfrmBrowseScenes.Create(self, tblPhotoTable.ScenesTable);
  result := fScenesBrowser2;
end;

procedure TfrmPhotoDataBase.BrowseScenes1Click(Sender: TObject);
begin
  ScenesBrowser2.ClearPhotoKey;
  ScenesBrowser2.Show;
end;

procedure TfrmPhotoDataBase.cbShowScenesClick(Sender: TObject);
begin
  if cbShowScenes.Checked then
    begin
      ScenesBrowser.PhotoKey := tblPhotoTable.fldKey.AsInteger;
      ScenesBrowser.Top      := self.Top + self.Height;
      ScenesBrowser.Left     := self.Left;
      ScenesBrowser.Width    := self.Width;
      ScenesBrowser.Show;
    end
  else
    begin
      if Assigned(fScenesBrowser) then
        begin
          ScenesBrowser.Hide;
          ScenesBrowser.ClearPhotoKey;
        end;
    end;
end;

procedure TfrmPhotoDataBase.Cut1Click(Sender: TObject);
begin
  dbKeyWords.CutToClipboard;
end;

procedure TfrmPhotoDataBase.Copy1Click(Sender: TObject);
begin
  dbKeyWords.CopyToClipboard;
end;

procedure TfrmPhotoDataBase.Paste1Click(Sender: TObject);
begin
  dbKeyWords.PasteFromClipboard;
end;

procedure TfrmPhotoDataBase.UnDo1Click(Sender: TObject);
begin
  dbKeyWords.Undo;
end;

function TfrmPhotoDataBase.TempScenesTable: TScenesTable;
begin
  if not Assigned(fTempScenesTable) then
    begin
      fTempScenesTable := TScenesTable.Create( self,
                                         CommonPhotoSettings.PhotoDBDatabaseFileName,
                                         cSCENES,
                                         [optUseClient]);
      fTempScenesTable.Tag := 2;
      fTempScenesTable.IndexFieldNames := FILENAME_KEY;
      fTempScenesTable.Active := true;
    end;
  result := fTempScenesTable;
end;

procedure TfrmPhotoDataBase.WMHideScenes(var Message: TMessage);
begin
  cbShowScenes.Checked := false;
end;

procedure TfrmPhotoDataBase.cbHasSceneInfoClick(Sender: TObject);
begin
  Include(fChanged, cr_HasSceneInfoChanged);
end;

procedure TfrmPhotoDataBase.ScanSelectedforMissingThumbnail1Click(
  Sender: TObject);
var
  Count, MissingThumbNails: integer;
  temp: string;
  FullFileName, Ext, FileName, FilePath, ThumbNailFileName: string;
  mt: TMediaType;
begin
  FilePath := CalcLogFileName('MissingThumbNails.txt');
  if BrowseForFile('Missing Thumb Nails report name', FilePath, '.txt') then
    begin
      fLogPathFileName := FilePath;
      AssignFile(fLogFile, fLogPathFileName);
      Rewrite(fLogFile);
      try
        WriteLn(fLogFile, 'Listing of Missing Thumbnail Files:');
        WriteLn(fLogFile);
        tempPhotoTable := TPhotoTable.Create( self,
                                              CommonPhotoSettings.PhotoDBDatabaseFileName,
                                              cFILENAMES,
                                              [optNoSyncFilePathTable, optReadOnly]);
        with tempPhotoTable do
          begin
            Active         := true;
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            SetSelectivityParserExpression(fExpression);
            SetOrder(fCurrentOrder);
            First;
            Count          := 0; MissingThumbNails := 0;
            while not Eof do
              begin
                FullFileName      := TempPhotoTable.PathAndFileName;
                Ext               := ExtractFileExt(FullFileName);
                mt                := MediaTypeFromExtension(Ext);
                if IsVideoMedia(mt) or IsPhotoMedia(mt) then
                  begin
                    FileName          := ExtractFileName(FullFileName);
                    FilePath          := ExtractFilePath(FullFileName);
                    ThumbNailFileName := ThumbNailPathAndName(FullFileName);

                    if not FileExists(ThumbNailFileName) then
                      begin
                        Log(ThumbNailFileName);
                        inc(MissingThumbNails);
                        lblStatus.Caption := Format('%d files scanned. %d Missing ThumbNails',
                                                    [Count, MissingThumbNails]);
                        Application.ProcessMessages;
                      end
                    else
                      if (Count mod 100) = 0 then
                        begin
                          lblStatus.Caption := Format('%d files scanned. %d Missing ThumbNails',
                                                      [Count, MissingThumbNails]);
                          Application.ProcessMessages;
                        end;
                  end;

                Next;
                Inc(Count);
              end;
          end;
      finally
        CloseFile(fLogFile);
        temp := Format('Notepad.exe %s', [fLogPathFileName]);
        FileExecute(temp, false);
      end;
    end;
end;

procedure TfrmPhotoDataBase.UploadSelectedFiles1Click(Sender: TObject);
{$IfDef UpLoadFiles}
var
  frmUploadFiles: TfrmUploadFiles;
{$EndIf}  
begin
{$IfDef UpLoadFiles}
  frmUploadFiles := TfrmUploadFiles.Create(self);
  try
    frmUploadFiles.ShowModal;
  finally
    FreeAndNil(frmUploadFiles);
  end;
{$EndIf}
end;

procedure TfrmPhotoDataBase.btnFillDateClick(Sender: TObject);
var
  FileNameInfo: TFileNameInfo;
begin
  with cbDateKind do
    begin
      FillChar(FileNameInfo, SizeOf(TFileNameInfo), 0);
      with FileNameInfo do
      case ItemIndex of
        0: { unknown };
        1: DateKinds := DateKinds + [dk_From_EXIF];
        2: DateKinds := DateKinds + [dk_From_FileName];
        3: DateKinds := DateKinds + [dk_Date_Modified];
        4: DateKinds := DateKinds + [dk_Date_Created];
        5: DateKinds := DateKinds + [dk_Last_Access];
        6: DateKinds := DateKinds + [dk_MediaInfo];
      end;
      GetAndSetPhotoDate(tblPhotoTable.PathAndFileName, FileNameInfo);
    end;
end;

{$IfDef SMEExport}
procedure TfrmPhotoDataBase.ExportSelectedRecords1Click(Sender: TObject);
var
  frmExport: TfrmExport;
begin
  frmExport := TFrmExport.Create(self);
  try
  finally
    frmExport.ShowModal;
    frmExport.Free;
  end;
end;
{$else}
procedure TfrmPhotoDataBase.ExportSelectedRecords1Click(Sender: TObject);
var
  frmExport: TfrmExport2;
begin
  frmExport := TFrmExport2.Create(self);
  try
    frmExport.ShowModal;
  finally
    frmExport.Free;
  end;
end;
{$EndIF}

procedure TfrmPhotoDataBase.DeleteSelectedRecords1Click(Sender: TObject);
var
  Count, MaxCount, FilesDeleted, RecsDeleted: integer;
  Saved_RecNo, OrgRecNo: integer;
  frmDeletionConfirmation: TfrmDeletionConfirmation;
  aFileName,LogFileName, Temp: string;
begin
  frmDeletionConfirmation := TfrmDeletionConfirmation.Create(self);
  if frmDeletionConfirmation.ShowModal = mrOk then
    begin
      LogFileName := CalcLogFileName('Recs&FilesDeleted');
      AssignFile(fLogFile, LogFileName);
      ReWrite(fLogFile);
      OrgRecNo       := tblPhotoTable.RecNo;
      FreeAndNil(tempPhotoTable);
      tempPhotoTable := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            [optNoConfirmDelete]);
      try
        with TempPhotoTable do
          begin
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            Active         := true;
            SetSelectivityParserExpression(fExpression);
            MaxCount       := TotalRecordCount;
            Count          := 0;
            FilesDeleted   := 0;
            RecsDeleted    := 0;
            First;
            while not eof do
              begin
                Saved_RecNo := RecNo;
                with frmDeletionConfirmation do
                  begin
                    aFileName := PathAndFileName;
                    if cbDeleteFile.Checked then
                      begin
                        if not DeleteFile(aFileName) then
                          WriteLn(fLogFile, FilesDeleted:5, ': Could not delete file: ', aFileName)
                        else
                          WriteLn(fLogFile, FilesDeleted:5, ': Deleted file:          ', aFileName);
                        Inc(FilesDeleted);
                      end;

                    if cbDeleteTheRecord.Checked then
                      begin
                        Delete;
                        inc(RecsDeleted);
                        WriteLn(fLogFile,   RecsDeleted:5, ': Deleted record:        ', aFileName);
                      end;
                  end;

                if Saved_RecNo = RecNo then
                  Next;

                inc(Count);
                if (Count mod 100) = 0 then
                  Update_Status( Format('Processed %d/%d: %d recs deleted. %d files deleted.',
                                        [Count, MaxCount, RecsDeleted, FilesDeleted]));
              end;
          end;
        Update_Status( Format('COMPLETE. %d recs deleted. %d files deleted.', [RecsDeleted, FilesDeleted]));
      finally
        CloseFile(fLogFile);
        temp := Format('Notepad.exe %s', [LogFileName]);
        FileExecute(temp, false);

        tempPhotoTable.Active := false;
        FreeAndNil(tempPhotoTable);
        ClosePhotoTable;
        OpenPhotoTable;
        tblPhotoTable.RecNo := OrgRecNo;
        FreeAndNil(frmDeletionConfirmation);
      end;
    end;
end;

procedure TfrmPhotoDataBase.BrowseFileNames1Click(Sender: TObject);
begin
  FreeAndNil(TempPhotoTable);
  tempPhotoTable := TPhotoTable.Create(self,
                                      CommonPhotoSettings.PhotoDBDatabaseFileName,
                                      cFILENAMES,
                                      [optNoFilePathsTable, optNoCopyRightsTable]);
  tempPhotoTable.Active := true;

  if not Assigned(fFileNamesBrowser) then
    fFileNamesBrowser := TfrmDataSetBrowser.Create(self, tempPhotoTable, cFILENAMES);
  fFileNamesBrowser.Show;
end;

procedure TfrmPhotoDataBase.btnOpenFileClick(Sender: TObject);
var
  mt: TMediaType;
  mc: TMediaClass;
begin
  with tblPhotoTable do
    begin
      mt := MediaTypeFromExtension(ExtractFileExt(tblPhotoTable.fldFILE_NAME.AsString));
      mc := MediaInfoArray[mt].MediaClass;
      if IsOpenableFile(mt) then
        begin
          case mc of
            mc_Photos:
              EditPhoto;
            mc_Video, mc_Document, mc_Audio, mc_Powerpoint, mc_Spreadsheet, mc_VideoProject:
              OpenMediaClick(nil);
          end;
        end;
    end;
end;

procedure TfrmPhotoDataBase.GoToKey1Click(Sender: TObject);
var
  Key_Value: string;
  Key: integer;
begin
  if GetString('Go To Key', 'Key Value', Key_Value, 6) then
    begin
      try
        Key := StrToInt(Trim(Key_Value));
        if not tblPhotoTable.Locate(CKEY, Key, []) then
          AlertFmt('Key value "%d" not found in current selection.', [Key]);
      except
        on e:Exception do
          AlertFmt('Key "%s" is not found in current selection set [%s]',
                   [Key_Value, e.Message]);
      end;
    end;
end;

(*
procedure TfrmPhotoDataBase.GoToRecNo1Click(Sender: TObject);
var
  RecNo_Value: string;
  RecNo: integer;
begin
  if GetString('Go To RecNo', 'RecNo', RecNo_Value, 6) then
    begin
      try
        RecNo := StrToInt(Trim(RecNo_Value));
        tblPhotoTable.RecNo := RecNo;
        if tblPhotoTable.RecNo <> RecNo then
          AlertFmt('RecNo value "%s" not found.', [RecNo_Value]);
      except
        AlertFmt('RecNo "%s" is not numeric', [RecNo_Value]);
      end;
    end;
end;
*)

procedure TfrmPhotoDataBase.btnLastRecordClick(Sender: TObject);
begin
  if not tblPhotoTable.Locate(CKEY, fPrevKey, []) then
    AlertFmt('Key value "%d" not found in current selection.', [fPrevKey]);
end;

(*
procedure TfrmPhotoDataBase.BrowseLookups1Click(Sender: TObject);
var
  LookupsTable: TLookupsTable;
  LookupsBrowser: TfrmDataSetBrowser;
begin
  LookupsTable := TLookupsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, 'Lookups', [], lc_All);
  LookupsTable.Open;
  LookupsBrowser := TfrmDataSetBrowser.Create(self, LookupsTable, 'Lookups');
  try
    LookupsBrowser.ShowModal;
  finally
    FreeAndNil(LookupsBrowser);
    FreeAndNil(LookupsTable);
  end;
end;
*)

{$IfDef dhd}
function TfrmPhotoDataBase.TracksTable: TTracksTable;
begin
  if not Assigned(fTracksTable) then
    begin
      fTracksTable := TTracksTable.Create(nil, HikingSettings.HikingMdbPathName, cTRACKS, []);
      fTracksTable.Active := true;
    end;
  result := fTracksTable;
end;

function TfrmPhotoDataBase.GetTracksBrowser: TfrmTracksBrowser;
begin
  if not Assigned(fTracksBrowser) then
    fTracksBrowser := TfrmTracksBrowser.Create(self, TracksTable, cTRACKS);
  result := fTracksBrowser;
end;
{$EndIf}

procedure TfrmPhotoDataBase.BrowseTracks1Click(Sender: TObject);
begin
{$IfDef dhd}
  TracksBrowser.Show;
{$EndIf}
end;

procedure TfrmPhotoDataBase.EditThumbnail1Click(Sender: TObject);
var
  ThumbNailName, Message: string;
begin
  ThumbNailName := ThumbNailPathAndName(tblPhotoTable.PathAndFileName);
  Message       := '';
  
  if not FileExists(ThumbNailName) then
    CreateThumbNail(Message, true);

  EditPhotoUtil(ThumbNailName, CommonPhotoSettings.PhotoEditingProgram);

  Update_Status( Message);
end;

procedure TfrmPhotoDataBase.Print1Click(Sender: TObject);
begin
  Print;
end;

procedure TfrmPhotoDataBase.FromPhotoDateTime1Click(Sender: TObject);
begin
  with TempPhotoTable do
    UpdateDates(false, PHOTODATETIME, PHOTODATE);
end;

procedure TfrmPhotoDataBase.FromPhotoDate1Click(Sender: TObject);
begin
  with TempPhotoTable do
    UpdateDates(true, PHOTODATE, PHOTODATETIME);
end;

procedure TfrmPhotoDataBase.UpdateDates(GoingToDateTime: boolean; OldDateFieldName, NewDateFieldName: string);
var
  Updated, Count, OldRecNo: integer;
  OldYYYY, OldMM, OldDD: word;
  NewYYYY, NewMM, NewDD: word;
  OverWrite,
  ConfirmEachRecord: boolean;
  mr: integer;
  OldDateField, NewDateField: TField;

  procedure UpdateThisRecord(NewYYYY, NewMM, NewDD: integer);
  begin { UpdateThisRecord }
    with TempPhotoTable do
      begin
        Edit;

        fldYear.AsInteger     := NewYYYY;
        fldMonth.AsInteger    := NewMM;
        fldDay.AsInteger      := NewDD;

        if GoingToDateTime then
          NewDateField.AsDateTime := EncodeDate(NewYYYY, NewMM, NewDD)
        else   // going to just date
          NewDateField.AsString   := YearMonthDayToPhotoDate(NewYYYY, NewMM, NewDD);

        fldUpdateReason.AsInteger := integer(ur_FillYearMonthDayFromPhotoDateTime);  // tested
        Post;
(*      Cancel;   // DEBUGGING !!!! *)
        Inc(Updated);
      end;
  end; { UpdateThisRecord }

  function OkToDoThisRecord(OldYYYY, OldMM, OldDD, NewYYYY, NewMM, NewDDD: integer): integer;
  begin { OkToDoThisRecord }
    with frmConfirmEachPhoto do
      begin
//      Question      := 'Do you want to change';
        Caption1      := Format(' Year = %4d, Month = %2d, Day = %2d to', [OldYYYY, OldMM, OldDD]) + CRLF +
                         Format(' Year = %4d, Month = %2d, Day = %2d?',   [NewYYYY, NewMM, NewDD]);
//      Caption2      := TempPhotoTable.fldKey_Words.AsString;
        PhotoFileName := TempPhotoTable.PathAndFileName;
        result        := ShowModal;
      end;
  end; { OkToDoThisRecord }

begin { UpdateDates }
  FreeAndNil(TempPhotoTable);
  OverWrite := false;

  ConfirmEachRecord  := Yes('Do you want to confirm each update?');
  if ConfirmEachRecord then
    begin
      FreeAndNil(frmConfirmEachPhoto);
      frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update');
    end
  else
    Overwrite := Yes('Overwrite pre-existing values in Year, Month and Day?');

  TempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        [optNoSyncDateFields]);
  try
    with TempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);
        Count          := 0;
        Updated        := 0;
        OldDateField   := FieldByName(OldDateFieldName);
        NewDateField   := FieldByName(NewDateFieldName);
        First;
        while not eof do
          begin
            OldRecNo := RecNo;

            if (not Empty(OldDateField.AsString)) then
              begin
                OldYYYY := fldYear.AsInteger;
                OldMM   := fldMonth.AsInteger;
                OldDD   := FldDay.AsInteger;

                if GoingToDateTime then
                  PhotoDateToYearMonthDay(OldDateField.AsString, NewYYYY, NewMM, NewDD)
                else
                  DecodeDate(OldDateField.AsDateTime, NewYYYY, NewMM, NewDD);

                mr := mrNone;
                
                if ConfirmEachRecord then
                  mr := OkToDoThisRecord(OldYYYY, OldMM, OldDD, NewYYYY, NewMM, NewDD)
                else
                  begin
                    if OverWrite then
                      mr := mrOK else
                    if (OldYYYY = 0) or (oldMM = 0) or (OldDD = 0) or
                       (fldPhotoDate.AsString = '') or (fldPhotoDate.IsNull) then
                      mr := mrOK;
                  end;

                if mr = mrCancel then
                  Break
                else
                  if not ConfirmEachRecord then
                    UpdateThisRecord(NewYYYY, NewMM, NewDD)
                  else if mr in [mrYes, mrOk] then
                    UpdateThisRecord(NewYYYY, NewMM, NewDD)
              end;

            if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
              Next;

            inc(Count);
            if (Count mod 100) = 0 then
              Update_Status( Format('Processed %d, Updated=%d', [Count, Updated]));
          end;
        Update_Status( Format('Complete. Processed %d, Updated=%d', [Count, Updated]));
        tblPhotoTable.Refresh;
      end;
  finally
    FreeAndNil(TempPhotoTable);
  end;
end;  { UpdateDates }

procedure TfrmPhotoDataBase.cbScanCommentsClick(Sender: TObject);
begin
  if cbScanComments.Checked then
    cbScanComments.Color := clYellow
  else
    cbScanComments.Color := clBtnFace;
end;

(*
procedure TfrmPhotoDataBase.AddFuncToLookupTable(fp: TAvailable_Function);
var
  LookupsTable: TLookupsTable;
begin
  LookupsTable := TLookupsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, 'Lookups', [], lc_Fil);
  try
    with LookupsTable do
      begin
        Open;
        Append;
        fldLookupName.AsString     := fp.fn;
        fldLookupValue.AsString    := '';
        fldLookupCategory.AsString := LookupInfoArray[lc_Help].Cat;
        Post;
        Close;
      end;
  finally
    LookupsTable.Free;
  end;
end;

procedure TfrmPhotoDataBase.FillLookupsforExpressionFunctions1Click(
  Sender: TObject);
var
  Func: TAvailable_Function_Calls;
  Parser: TParser;
begin
  Parser := TParser.Create;
  try
    Parser.RootNode.TraverseSubTree(AddFuncToLookupTable);
  finally
    Parser.Free;
  end;
end;
*)

procedure TfrmPhotoDataBase.BrowseLookupsTable(LookupCategory: TLookupCategory);
var
  LookupsTable: TLookupsTable;
  LookupsBrowser: TfrmLookupBrowser;
begin
  LookupsTable := TLookupsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, cLOOKUPS, [], LookupCategory);
  LookupsTable.IndexName := 'LookupName';
  LookupsTable.Open;
  LookupsBrowser := TfrmLookupBrowser.Create(self, LookupsTable, cLOOKUPS, LookupCategory);
  try
    LookupsBrowser.ShowModal;
  finally
    FreeAndNil(LookupsBrowser);
    FreeAndNil(LookupsTable);
  end;
end;

procedure TfrmPhotoDataBase.All1Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_All);
end;

procedure TfrmPhotoDataBase.Expressions1Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_Exp);
end;

procedure TfrmPhotoDataBase.Filters1Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_Fil);
end;

procedure TfrmPhotoDataBase.Help2Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_Help);
end;

procedure TfrmPhotoDataBase.FunctionKeys1Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_FuncKey);
  LoadFuncKeys;
end;

procedure TfrmPhotoDataBase.OpenFuncKeytxtforediting1Click(
  Sender: TObject);
begin
  BrowseLookupsTable(lc_FuncKey);
  LoadFuncKeys;
end;

procedure TfrmPhotoDataBase.Edit2Click(Sender: TObject);
var
  mt: TMediaType;
begin
  mt := MediaTypeFromExtension(ExtractFileExt(tblPhotoTable.fldFILE_NAME.AsString));
  PhotoEditor1.Enabled := IsPhotoMedia(mt);
end;

procedure TfrmPhotoDataBase.Synonyms1Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_Syn);
end;

function TfrmPhotoDataBase.GetEvaluationParser: TParser;
begin
  if not Assigned(fEvaluationParser) then
    begin
      fEvaluationParser := TParser.Create;
      tblPhotoTable.AddOptionalFunctionsToParser(fEvaluationParser);
    end;
  result := fEvaluationParser;
end;


function TfrmPhotoDataBase.GetForm_Expression2: TFrmExpression;
begin
  if not Assigned(fForm_Expression2) then
    fForm_Expression2 := TFrmExpression.Create( self,
                                               CommonPhotoSettings.PhotoDBDatabaseFileName,
                                               tblPhotoTable,
                                               EvaluationParser,
                                               'Evaluate',
                                               [eo_NonModal]);
  result := fForm_Expression2;
end;

procedure TfrmPhotoDataBase.EvaluateExpression2Click(Sender: TObject);
begin
  Form_Expression2.Show;
end;

procedure TfrmPhotoDataBase.EditPhoto1Click(Sender: TObject);
var
  ThumbNailFileName: string;
  UpdateInfo: TUpdateInfo;
begin
  if Assigned(fSelectedImage) then
    begin
      fSelectedPhotoKey := fSelectedImage.Tag;
      with tblPhotoTable do
        begin
          if tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then
            begin
              EditPhoto;
              Edit;
              fldFile_Size.AsFloat    := FileSize64(UpdateInfo.FileName);

              ThumbNailFileName := ThumbNailPathAndName(tblPhotoTable.PathAndFileName);
              if FileExists(ThumbNailFileName) then
                fSelectedImage.Picture.LoadFromFile(ThumbNailFileName);
            end
          else
            AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
        end;
    end;
end;

procedure TfrmPhotoDataBase.DeletePhoto1Click(Sender: TObject);
var
  Idx: integer;
begin
  if Assigned(fSelectedImage) then
    begin
      fSelectedPhotoKey := fSelectedImage.Tag;
      with tblPhotoTable do
        begin
          if tblPhotoTable.Locate('Key', fSelectedPhotoKey, []) then
            begin
              tblPhotoTable.Delete;
              Idx := PhotoList.IndexOf(fSelectedImage);
              if Idx >= 0 then
                begin
                  PhotoList.Delete(Idx);
                  FreeAndNil(fSelectedImage);
                end;
            end
          else
            AlertFmt('Unable to select photo key %d', [fSelectedPhotoKey]);
        end;
    end;
end;

procedure TfrmPhotoDataBase.FileDateIsAGuess1Click(Sender: TObject);
var
  MaxCount, Count, Fixed: integer;
  FileNameInfo: TFileNameInfo;
begin
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Fixed          := 0;
      FillChar(FileNameInfo, sizeof(TFileNameInfo), 0);
      FileNameInfo.DateKinds := [dk_From_FileName];

      First;
      while not eof do
        begin
          if DateIsAGuess then
            begin
              Edit;
              if Assigned(fldDateIsAGuess) then
                fldDateIsAGuess.AsBoolean := true;
              fldUpdateReason.AsInteger := ord(ur_FileDateIsAGuess);
              Post;
              inc(Fixed);
            end;
          Next;
          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d', [Count, MaxCount]));
        end;
    end;

  try
    Update_Status( Format('COMPLETE. %d recs updated', [Fixed]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
  end;
end;

procedure TfrmPhotoDataBase.BrowseNoiseWords1Click(Sender: TObject);
begin
  BrowseLookupsTable(lc_NoiseWords);
end;

procedure TfrmPhotoDataBase.DistancefromLocation1Click(Sender: TObject);
var
  OK: boolean;
begin
  // Build and index based on the distance from a given Lat, Lon (or should I use a specific Location record?
  if Yes('Rebuild index for currently selected records?') then
    begin
      OK := BuildDistanceIndexTable;
      if not OK then
        Message('Operation aborted');
    end;
  SetOrder(coDistanceFromLocation);
end;

procedure TfrmPhotoDataBase.InOrderbyLatLon1Click(Sender: TObject);
var
  OK: boolean;
begin
  // Build and index based on the distance from a given Lat, Lon
  if Yes('Rebuild index for currently selected records?') then
    begin
      OK := BuildIndexByNearness;
      if not OK then
        Message('Operation aborted');
    end
  else
    OK := true;

  if OK then
    SetOrder(coNearness);
end;

function TfrmPhotoDataBase.BuildIndexByNearness: boolean;
type
  TWayPoint = record
                TheKey: integer;
                Used: boolean;
                Latitude, Longitude: double;
{$IfDef DebugIndexBuild}
                Desc: string;
{$EndIf}                
              end;
var
  Count, NrToProcess, i: integer;
  StartTime: double;
  SavedCursor: TCursor;
  frmFillLocationInfo: TfrmFillLocationInfo;
  OriginLatitude: double;
  OriginLongitude: double;
  CurrentLatitude: double;
  CurrentLongitude: double;
  ShortestDistance: double;
  StartAzimuth, CurrentAzimuth: double;
  StartingIndex, OutputIndex, CurrentIndex: integer;
  Lat, Lon: double;
  InPoints, Outpoints: array of TWayPoint;
{$IfDef DebugIndexBuild}
  LogFileName: string;
{$EndIf}

  function FindNearestPointInAzimuth(    StartLat, StartLon, DesiredAzimuth: double;
                                       var ActualAzimuth, ShortestDistance: double): integer;
  var
    TheDistance: double;

    function FindNearestPoint(CheckAzimuth: boolean): integer;
    var
      i: integer;
      CalcedAzimuth: double;
    begin { FindNearestPoint }
      result  := -1;
      ShortestDistance := MAXINT;
      ActualAzimuth    := DesiredAzimuth;
      for i := 0 to NrToProcess-1 do
        with InPoints[i] do
          begin
            if not Used then
              begin
//              if ApproxEqual(39.851, Latitude, 0.005) and
//                 ApproxEqual(-84.173, Longitude, 0.005) then
//                   J := J;
                TheDistance := RhumbDistance( StartLat, StartLon,
                                              Latitude, Longitude,
                                              frmFillLocationInfo.MaxDistanceUnits,
                                              CalcedAzimuth);
                if CheckAzimuth then
                  begin
                    if TheDistance > 0.01 then
                      begin
                        if InArc(CalcedAzimuth, DesiredAzimuth) then
                          if TheDistance < ShortestDistance then
                            begin
                              ShortestDistance := TheDistance;
                              if ShortestDistance > 0.01 then
                                ActualAzimuth    := CalcedAzimuth;
                              result := i;
                            end;
                      end
                    else
                      begin // should I compare something else like "previous date"?
                        ShortestDistance := 0.0;
                        ActualAzimuth    := DesiredAzimuth;
                        result           := i;
                        Exit;
                      end
                  end
                else
                  begin
                    if TheDistance > 0 then
                      begin
                        if TheDistance < ShortestDistance then
                          begin
                            ShortestDistance := TheDistance;
                            if ShortestDistance > 0.01 then
                              ActualAzimuth    := CalcedAzimuth;
                            result := i;
                          end;
                      end
                    else
                      begin // should I compare something else like "previous date"?
                        ShortestDistance := 0.0;
                        ActualAzimuth    := DesiredAzimuth;
                        result           := i;
                        Exit;
                      end
                  end;
              end;
          end;
    end;  { FindNearestPoint }

  begin { FindNearestPointInAzimuth }
    ActualAzimuth := DesiredAzimuth; // starting assumption
    result        := FindNearestPoint(true);    // considering Azimuth
    if (result < 0) or (ShortestDistance > frmFillLocationInfo.MaxDistance) then
      result := FindNearestPoint(false)  // not considering Azimuth
  end;  { FindNearestPointInAzimuth }

  procedure WriteLogEntry(CurrentIndex, OutputIndex: integer);
  begin
{$IfDef DebugIndexBuild}
    with tempPhotoTable, Outpoints[OutputIndex] do
      begin
//      Locate(cKey, TheKey, []);
        WriteLn(fLogFile, OutputIndex: 4,
                          CurrentIndex:4,
                          ShortestDistance:7:2,
                          AzimuthCode(CurrentAzimuth):5,
                          ' (', CurrentAzimuth:6:1, '): ',
                          CurrentLatitude:10:6, ' ', CurrentLongitude:10:6, '  ',   // ***
                          '[', fldKey.AsInteger:6, ']', ' ',
                          PrintableOnly2(Copy(Desc, 1, 60)));
      end;
{$EndIf}
  end;

begin { TfrmPhotoDataBase.BuildIndexByNearness }
  Update_Status('Building distance index table');
  SavedCursor := Screen.Cursor;
  frmFillLocationInfo := TfrmFillLocationInfo.Create(self);
  fBuildingIndexTable := true;
  try
    frmFillLocationInfo.LocationSearchType := st_Nearness;
    frmFillLocationInfo.Latitude           := tblPhotoTable.GetLatitude;
    frmFillLocationInfo.Longitude          := tblPhotoTable.GetLongitude;
    frmFillLocationInfo.MaxDistanceUnits   := muMiles;
    frmFillLocationInfo.MaxDistance        := 1;
    frmFillLocationInfo.Caption            := 'Specify starting location';
    frmFillLocationInfo.ovcDescription.AsString := tblPhotoTable.LocationsTable.fldDescription.AsString;
    frmFillLocationInfo.ovcState.AsString  := tblPhotoTable.LocationsTable.fldState.AsString;
{$IfDef DebugIndexBuild}
    frmFillLocationInfo.Azimuth            := 270;
{$EndIf}
    result                                 := frmFillLocationInfo.ShowModal = mrOK;
    if result then
      begin
        try
          Cursor          := crSQLWait;
          StartTime       := Now;
          OriginLatitude  := frmFillLocationInfo.Latitude;
          OriginLongitude := frmFillLocationInfo.Longitude;
          StartAzimuth    := frmFillLocationInfo.Azimuth;
          fMaxDistance    := frmFillLocationInfo.MaxDistance;
          tempPhotoTable  := TPhotoTable.Create( self,
                                                CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                cFILENAMES,
                                                [optNoSyncFilePathTable,
                                                 optNoFilePathsTable,
                                                 optNoCopyRightsTable,
                                                 optNoUpdateDate]);
          with tempPhotoTable do
            begin
{$IfDef dhd}
              tempPhotoTable.OnGetBoundariesTable := GetBoundariesTable;
{$EndIf}
              Active         := true;
              SetSelectivityParserExpression(fExpression);
              OnFilterRecord := PhotoTableFilterRecord;
              Filtered       := true;

              // Count the number of records to process
              First;
              NrToProcess := 0;
              while not Eof do
                begin
                  if (NrToProcess mod 10) = 0 then
                    Update_Status( Format('Presort Scan: Counting %d at %s',
                                          [NrToProcess, ElapsedTimeToStr(Now - StartTime)]));
                  Inc(NrToProcess);
                  Next;
                end;

              SetLength(InPoints, NrToProcess);
              Count := 0;
              First;
              while not Eof do
                begin
                  Lat := GetLatitude;
                  Lon := GetLongitude;
                  if (Lat <> 0) or (lon <> 0) then
                    begin
                      with InPoints[Count] do
                        begin
                          Latitude  := Lat;
                          Longitude := Lon;
                          TheKey    := fldKey.AsInteger;
{$IfDef DebugIndexBuild}
                          Desc      := fldKey_Words.AsString;
{$EndIf}
                          Used      := false;
                        end;
                    end;
                  Inc(Count);
                  Next;

                  if (Count mod 10) = 0 then
                    Update_Status( Format('%d/%d records loaded at %s',
                                          [Count, NrToProcess, ElapsedTimeToStr(Now - StartTime)]));
                end;
              NrToProcess := Count;  // may have skipped some records
              SetLength(InPoints, NrToProcess);
              SetLength(Outpoints, NrToProcess);

{$IfDef DebugIndexBuild}
              AssignFile(fLogFile, 'C:\temp\PointList.txt');
              ReWrite(fLogFile);
              WriteLn(fLogFile, '#':4,
                                'Key':7,
                                'Latitude':10, ' ',
                                'Longitude':10);
              for i := 0 to NrToProcess-1 do
                with Inpoints[i] do
                  WriteLn(fLogFile, i:4,
                                    TheKey:7,
                                    Latitude:10:6, ' ',
                                    Longitude:10:6, ' ',
                                    Desc);
              CloseFile(fLogFile);
              LogFileName := UniqueFileName('c:\temp\LogFile.txt');
              AssignFile(fLogfile, LogFileName);
              ReWrite(fLogFile);
              WriteLn(fLogFile, 'LogFile Created at ', DateTimeToStr(Now));
              WriteLn(fLogFile, '#': 4,
                                'Org':4,
                                'Dist':7,
                                'Dir':5,
                                'Deg':11,
                                'Lat':10, ' ', 'Lon':10, // ***
                                'Key':8, ' ',
                                'Key Words');
{$EndIf}
(* *)
              // Now locate the nearest point in the given Azimuth from the starting location
              Update_Status('Building Key List');
              OutputIndex := 0;
              StartingIndex := FindNearestPointInAzimuth( OriginLatitude,
                                                          OriginLongitude,
                                                          StartAzimuth,
                                                          CurrentAzimuth,
                                                          ShortestDistance);
              if StartingIndex >= 0 then
                with InPoints[StartingIndex] do
                  begin
                    if Locate(CKEY, TheKey, []) then
                      if YesFmt('The nearest record to the location (%10.6n, %10.6n) in the Azimuth %s has key %d (index=%d), Description: %s. Proceed?',
                                 [OriginLatitude, OriginLongitude, AzimuthCode(StartAzimuth), TheKey, StartingIndex, fldKey_Words.AsString]) then
                        // then all is well
                      else
                        Exit
                    else
                      raise Exception.CreateFmt('System error: Key = %d could not be found', [TheKey]);

                    CurrentLatitude  := Latitude;
                    CurrentLongitude := Longitude;
                    Inpoints[StartingIndex].Used := true;
                    Outpoints[OutputIndex] := Inpoints[StartingIndex];

                    WriteLogEntry(StartingIndex, OutputIndex);

                    Inc(Outputindex);
                  end
              else
                raise Exception.CreateFmt('No records could be found near the location (%12.8f, %12.8f) in the Azimuth %s (%5d) could be found',
                         [OriginLatitude, OriginLongitude, AzimuthCode(StartAzimuth), Round(StartAzimuth)]);

              // add records by nearness that are in the correct arc
              StartTime := Now;
              repeat
                if (OutputIndex mod 10) = 0 then
                  Update_Status(Format('Indexing %d/%d at %s', [ OutputIndex,
                                                                 NrToProcess,
                                                                 ElapsedTimeToStr(Now - StartTime)]));

                CurrentIndex := FindNearestPointInAzimuth( CurrentLatitude,
                                                           CurrentLongitude,
                                                           CurrentAzimuth,
                                                           CurrentAzimuth,
                                                           ShortestDistance);
                if CurrentIndex >= 0 then
                  with InPoints[CurrentIndex] do
                    if (Latitude <> 0) or (Longitude <> 0) then
                      begin
                        CurrentLatitude  := Latitude;
                        CurrentLongitude := Longitude;
                        Inpoints[CurrentIndex].Used := true;
                        Outpoints[OutputIndex] := Inpoints[CurrentIndex];
                        WriteLogEntry(CurrentIndex, OutputIndex);
                        Inc(Outputindex);
                      end
                    else
                      begin
                        ErrorFmt('Point %d has location (0,0)?', [CurrentIndex]);
                        CurrentIndex := -1;
                      end;
              until CurrentIndex < 0;

{$IfDef DebugIndexBuild}
              CloseFile(fLogFile);
{$EndIf}              

              Update_Status( Format('%d/%d records found', [Outputindex, NrToProcess]));

              // clear the sort order field in all records
              i := 0;
              First;
              while not eof do
                begin
                  if (i mod 100) = 0 then
                    Update_Status( Format('%d/%d records initialized at %s',
                                          [ i,
                                            NrToProcess,
                                            ElapsedTimeToStr(Now - StartTime)]));

                  Edit;
                  fldDistance.Clear;
                  Post;
                  Next;
                  inc(i);
                end;

              Update_Status('Setting sort order field in selected record');

              // now set the sort order field in the selected records
              SetLength(OutPoints, OutputIndex);
              for i := 0 to OutputIndex-1 do
                with Outpoints[i] do
                  begin
                    if Locate(cKey, TheKey, []) then
                      begin
                        Edit;
                        fldDistance.AsInteger := i;
                        Post;
                      end
                    else
                      ErrorFmt('System error: Key = %d could not be found', [TheKey]);

                    if (i mod 100) = 0 then
                      Update_Status( Format('%d records indexed at %s', [i, ElapsedTimeToStr(Now - StartTime)]));
                  end;

              Update_Status( Format('Complete. %d records indexed in %s',
                                    [Outputindex,
                                     ElapsedTimeToStr(Now - StartTime)]));

              Active         := false;
            end;
          FreeAndNil(tempPhotoTable);
        finally
          Screen.Cursor := SavedCursor;
        end;
      end;
  finally
    frmFillLocationInfo.Free;
    fBuildingIndexTable := false;
  end;
end;  { TfrmPhotoDataBase.BuildIndexByNearness }

procedure TfrmPhotoDataBase.GenerateCSVFile1Click(Sender: TObject);
var
  tempPhotoTable: TPhotoTable;
  OutFile: TextFile;
  OutFileName: string;
  POINr: integer;
  DontAskAgain: boolean;
begin
  DontAskAgain := false;
  OutFileName := 'c:\temp\POI.csv';
  if BrowseForFile('POI filename', OutFileName, 'csv') then
    begin
      AssignFile(OutFile, OutFileName);
      ReWrite(OutFile);
      tempPhotoTable  := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            [optNoSyncFilePathTable,
                                             optNoFilePathsTable,
                                             optNoCopyRightsTable,
                                             optNoUpdateDate]);
      try
        with tempPhotoTable do
          begin
            Active         := true;
            SetSelectivityParserExpression(fExpression);
            IndexName      := 'Distance';
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            POINr          := 0;
            while not Eof do
              begin
                if (POINr mod 100) = 0 then
                  Update_Status(Format('Writing Record %d', [POINr]));

                if (not DontAskAgain) and (POINr <> fldDistance.AsInteger) then
                  if Yes('POI field (distance) may not be in sequential order. Proceed anyway? ') then
                    DontAskAgain := true
                  else
                    Exit;
                    
                WriteLn(OutFile, fldLongitude.AsFloat:10:7, ',',
                                 fldLatitude.AsFloat:10:7, ',',
                                 'P' + RZero(fldDistance.AsInteger, 4),  ',', // This is actually the POI number
                                 Format('[%5d] %s', [fldKey.AsInteger,
                                                   PrintableOnly2(Copy(fldKey_Words.AsString, 1, 80))]));
                Next;
                Inc(POINr);
              end;
            Update_Status(Format('Complete. %d records written to %s', [POINr, OutFileName]));
          end;
      finally
        FreeAndNil(tempPhotoTable);
        CloseFile(OutFile);
      end;
    end;

end;

procedure TfrmPhotoDataBase.Excellent2Click(Sender: TObject);
begin
  RateThisPhoto(+2, tblPhotoTable.fldKey.AsInteger, true);
end;

procedure TfrmPhotoDataBase.Good2Click(Sender: TObject);
begin
  RateThisPhoto(+1, tblPhotoTable.fldKey.AsInteger, true);
end;

procedure TfrmPhotoDataBase.Ok2Click(Sender: TObject);
begin
  RateThisPhoto(0, tblPhotoTable.fldKey.AsInteger, true);
end;

procedure TfrmPhotoDataBase.Poor2Click(Sender: TObject);
begin
  RateThisPhoto(-1, tblPhotoTable.fldKey.AsInteger, true);
end;

procedure TfrmPhotoDataBase.Terrible1Click(Sender: TObject);
begin
  RateThisPhoto(-2, tblPhotoTable.fldKey.AsInteger, true);
end;

procedure TfrmPhotoDataBase.GenerateFilenamesList1Click(Sender: TObject);
var
  tempPhotoTable: TPhotoTable;
  OutFile: TextFile;
  OutFileName: string;
  POINr: integer;
  DontAskAgain: boolean;
  Desc: string;
  Lat, Lon: double;
  LocationBased: boolean;
begin
  LocationBased := (fCurrentOrder in [coDistanceFromLocation, coNearness]); // only relevent when doing location based sorts

  OutFileName := 'C:\Users\dhdor\Videos\Video Projects\FileNames.csv';
  if BrowseForFile('FileNames List filename', OutFileName, 'csv') then
    begin
      AssignFile(OutFile, OutFileName);
      ReWrite(OutFile);
      WriteLn(OutFile, 'POI #',  ',', // This is actually the POI number
                       'Key #', ',',
                       'FileName', ',',
                       'Location', ',',
                       'Latitude', ',', 'Longitude', ',',
                       'Key Words',
                       'Photographer',
                       'DateTime');
      tempPhotoTable  := TPhotoTable.Create( self,
                                            CommonPhotoSettings.PhotoDBDatabaseFileName,
                                            cFILENAMES,
                                            [optNoUpdateDate]);
      try
        with tempPhotoTable do
          begin
{$IfDef dhd}
            onGetBoundariesTable := GetBoundariesTable;
{$EndIf}
            Active         := true;
            SetSelectivityParserExpression(fExpression);

            SetOrder(fCurrentOrder);
            if LocationBased then
              if not Yes('Current Sort Order <> Nearness. Proceed anyway') then
                raise Exception.Create('Operate abort. Sort order <> "Nearness"')
              else
                DontAskAgain := true
            else
              DontAskAgain := true;
                
            OnFilterRecord := PhotoTableFilterRecord;
            Filtered       := true;
            POINr          := 0;
            while not Eof do
              begin
                if (not fldDistance.IsNull) or (not LocationBased) then
                  begin
                    if (POINr mod 10) = 0 then
                      Update_Status(Format('Writing Record %d', [POINr]));

                    if LocationBased and (not DontAskAgain) and (POINr <> fldDistance.AsInteger) then
                      if Yes('POI field (distance) may not be in sequential order. Proceed anyway? ') then
                        DontAskAgain := true
                      else
                        Exit;

                    if fldLocationID.AsInteger > 0 then
                      with LocationsTable do
                        Desc := GetLocationDescription(fldLocationID.AsInteger) // get location description and position the table
                    else
                      Desc := '';

                    Lat := GetLatitude;
                    Lon := GetLongitude;
                    if (not LocationBased) or ((Lat <> 0.0) or (Lon <> 0.0)) then
                      WriteLn(OutFile, 'P' + RZero(fldDistance.AsInteger, 4),  ',', // This is actually the POI number
                                       fldKey.AsInteger, ',',
                                       Quoted(PathAndFileName), ',',
                                       Quoted(Desc), ',',
                                       Lat:10:6, ',', Lon:10:6, ',',
                                       Quoted(PrintableOnly2(Copy(fldKey_Words.AsString, 1, 80))), ',', 
                                       Quoted(Trim(CopyRightOwner)), ',',
                                       fldPhotoDateTime.AsString);
                  end;
                Next;
                Inc(POINr);
              end;
            Update_Status(Format('Complete. %d records written to %s', [POINr, OutFileName]));
          end;
      finally
        FreeAndNil(tempPhotoTable);
        CloseFile(OutFile);
      end;
    end;
end;

procedure TfrmPhotoDataBase.ImportRecords1Click(Sender: TObject);
var
  frmExport: TfrmExport2;
begin
  frmExport := TFrmExport2.Create(self, iem_Import);
  try
    frmExport.ShowModal;
  finally
    frmExport.Free;
  end;
end;

procedure TfrmPhotoDataBase.RatePhotos1Click(Sender: TObject);
begin
  if not fRatingPhotos then
    begin
      fRatingPhotos := true;
      DBRadioGroup1.Color := clLime;
      RatePhotos1.Caption := 'Stop rating photos';
    end
  else
    begin
      fRatingPhotos := false;
      DBRadioGroup1.Color := clBtnFace;
      RatePhotos1.Caption := 'Rate photos';
    end;
end;

procedure TfrmPhotoDataBase.DBRadioGroup1Click(Sender: TObject);
begin
  if fRatingPhotos then
    begin
      tblPhotoTable.Post;
      tblPhotoTable.Next;
      DBNavigator1.SetFocus;
    end;
end;

procedure TfrmPhotoDataBase.OpenMediaClick(Sender: TObject);
var
  temp: string;
  mt: TMediaType;
  mc: TMediaClass;
begin
  mt := MediaTypeFromExtension(MyExtractFileExt(fMediaFileName));
  mc := MediaInfoArray[mt].MediaClass;
  case mc of
    mc_Video, mc_Document, mc_Audio, mc_Powerpoint, mc_Spreadsheet, mc_VideoProject:
      if FileExists(fMediaFileName) then
        begin
          if IsDelphiRunning and (mt = mt_MOV) then  // QuickTime Pro won't work in debugger
            begin
              Clipboard.AsText := fMediaFileName;
              MessageFmt('QuickTime Pro won''t execute in the debugger. Filename "%s" has been saved to the ClipBoard',
                         [fMediaFileName]);
            end
          else
            begin
              temp := fMediaFileName;
              if not ExecAndWait(temp, '') then
                AlertFmt('Error executing %s', [temp]);
            end;
        end;
    mc_Photos:
      EditPhoto;
  end;
end;

procedure TfrmPhotoDataBase.FillMediaLengthfromFiles1Click(
  Sender: TObject);
const
  COL_KEY = 6;
  COL_LEN = 12;
  COL_LFN = 35;
var
  Lfn, Ext: string; Count, MaxCount, Skipped, Saved_RecNo: integer;
  MediaLengthS, Temp: string;
  mt: TMediaType;
  MediaFrames, MediaLengthL, MediaWidth, MediaHeight: Longint;
  DateTime: TDateTime;
  ConfirmEachRecord: boolean;
  mr: integer;
  OK: boolean;

  procedure WasSkipped(Action: string);
  begin { WasSkipped }
    inc(Skipped);
    with TempPhotoTable do
      WriteLn(fLogFile, fldKey.AsInteger:COL_KEY, ' ',
                        Action:COL_LEN, ' ',
                        Padr(fldFile_Name.AsString, COL_LFN),
                        FullFilePath);
  end; { WasSkipped }

begin
  ConfirmEachRecord  := Yes('Do you want to confirm each record?');
  if ConfirmEachRecord then
    begin
      FreeAndNil(frmConfirmEachPhoto);
      frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update');
    end;

  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  fLogPathFileName  := CalcLogFileName('VideoAudioLengths.txt');
  AssignFile(fLogFile, fLogPathFileName);
  Rewrite(fLogFile);
  WriteLn(fLogFile, 'Key':COL_KEY, ' ',
                    'Length':COL_LEN, ' ',
                    Padr('FileName', COL_LFN),
                    'FilePath');

  Count          := 0;
  Skipped        := 0;
  MaxCount       := 0;

  try
    with TempPhotoTable do
      begin
        OnFilterRecord := PhotoTableFilterRecord;
        Filtered       := true;
        Active         := true;
        SetSelectivityParserExpression(fExpression);

        Update_Status('Counting records to be processed');
        First;
        MaxCount          := 0;
        while not Eof do
          begin
            Inc(MaxCount);
            if (MaxCount mod 100) = 0 then
              Update_Status(Format('Counting records to be processed %d', [MaxCount]));
            Next;
          end;

        First;
        while not eof do
          begin
            Saved_RecNo := RecNo;
            Lfn      := PathAndFileName;
            Update_Status(Lfn, 1);
            Ext      := ExtractFileExt(Lfn);
            mt       := MediaTypeFromExtension(Ext);
            if IsVideoMedia(mt) or IsAudioMedia(mt) then
              begin
                try
                  if GetMediaProperties( Lfn,
                                         MediaFrames,
                                         MediaLengthL,
                                         MediaWidth,
                                         MediaHeight,
                                         MediaLengthS,
                                         DateTime) then
                    if not Empty(MediaLengthS) then
                      begin
                        if ConfirmEachRecord then
                          with frmConfirmEachPhoto do
                            begin
//                            Question := Format('Update this record [%s]?', [PathAndFileName]);
                              Caption1 := Format('Change date (frames, length, width, height) in record key [%d] ' + CRLF +
                                                  'from [Date:%-22s, Length:%-12s, Width:%4d, Height:%4d]' + CRLF +
                                                  'to   [Date:%-22s, Length:%-12s, Width:%4d, Height:%4d]?',
                                                 [fldKey.AsInteger,
                                                  fldPhotoDateTime.AsString, fldMEDIA_LENGTH.AsString,  fldWidth.AsInteger, fldHeight.AsInteger,
                                                  DateTimeToStr(DateTime),   MediaLengthS,              MediaWidth,         MediaHeight]);
                              Caption2 := Format('Processed %d, Skipped %d, Total %d', [Count, Skipped, MaxCount]);
                              PhotoFileName := tempPhotoTable.PathAndFileName;
                              mr := ShowModal;
                              OK := mr = mrYes;
                              if mr = mrCancel then
                                break;
                            end
                        else
                          OK := true;

                        if OK then
                          begin
                            Edit;
                            fldMEDIA_LENGTH.AsString  := MediaLengthS;
                            fldWidth.AsInteger        := MediaWidth;
                            fldHeight.AsInteger       := MediaHeight;
                            fldUpdateReason.AsInteger := integer(ur_MediaLength);
                            Post;
                            WriteLn(fLogFile, fldKey.AsInteger:COL_KEY, ' ',
                                              fldMedia_Length.AsString:COL_LEN, ' ',
                                              Padr(fldFile_Name.AsString, COL_LFN),
                                              FullFilePath);
                          end
                        else
                          WasSkipped('Skipped')
                      end
                    else
                      WasSkipped('No Length')
                  else
                    WasSkipped('NoProperties');
                except
                  on e:Exception do
                    begin
                      Update_Status(Format('Unable to determine length of "%s" [%s]',
                                           [Lfn, e.Message]));
                      WriteLn(fLogFile, fldKey.AsInteger:COL_KEY, ' ',
                                        e.Message:COL_LEN, ' ',
                                        Padr(fldFile_Name.AsString, COL_LFN),
                                        FullFilePath);
                    end;
                end;
              end;

            if RecNo = Saved_RecNo then   // record we just posted may have been removed from the selection set moving us to the next record
              Next;
            inc(Count);
            Update_Status(Format('Processed %d, Skipped %d, Total %d', [Count, Skipped, MaxCount]));
          end;
      end;
  finally
    Writeln(fLogFile);
    WriteLn(fLogFile, Format('Processed %d, Skipped %d, Total %d', [Count, Skipped, MaxCount]));
    CloseFile(fLogFile);
    if (Count > 0) or (Skipped > 0) then
      begin
        temp := Format('Notepad.exe %s', [fLogPathFileName]);
        FileExecute(temp, false);
      end;
    Update_Status( Format('COMPLETE. Processed %d, Skipped %d, Total %d', [Count, Skipped, MaxCount]));
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    FreeAndNil(frmConfirmEachPhoto);
    tblPhotoTable.ReFresh;
  end;
end;

procedure TfrmPhotoDataBase.Ascending1Click(Sender: TObject);
begin
(*
  fCurrentOrder := coLatitude;
  tblPhotoTable.SetOrder(fCurrentOrder);
*)
  SetOrder(coLatitude); // changed from above on 8/12/2017
end;

procedure TfrmPhotoDataBase.Descending1Click(Sender: TObject);
begin
  SetOrder(coLatitudeDec); // changed from above on 8/12/2017
end;

procedure TfrmPhotoDataBase.Ascending2Click(Sender: TObject);
begin
(*
  fCurrentOrder := coLongitude;
  tblPhotoTable.SetOrder(fCurrentOrder);
*)
  SetOrder(coLongitude);
end;

procedure TfrmPhotoDataBase.Decsencing1Click(Sender: TObject);
begin
  SetOrder(coLongitudeDec);
end;

procedure TfrmPhotoDataBase.Ascending3Click(Sender: TObject);
begin
  SetOrder(coPhotoDate);
end;

procedure TfrmPhotoDataBase.Descending2Click(Sender: TObject);
begin
  SetOrder(coPhotoDateDesc);
end;

procedure TfrmPhotoDataBase.AScending4Click(Sender: TObject);
begin
  SetOrder(coPhotoDateTime);
end;

procedure TfrmPhotoDataBase.Descending3Click(Sender: TObject);
begin
  SetOrder(coPhotoDateTimeDesc);
end;

procedure TfrmPhotoDataBase.Browse1Click(Sender: TObject);
begin
{$IfDef dhd}
  BrowseBoundaries1.Visible := true;
{$else}
  BrowseBoundaries1.Visible := false;
{$EndIf}
end;

procedure TfrmPhotoDataBase.MediaLength1Click(Sender: TObject);
begin
  SetOrder(coMediaLength); // changed from above on 8/12/2017
end;

procedure TfrmPhotoDataBase.fromMediaDate1Click(Sender: TObject);
begin
  FillPhotoDateTime(fw_MediaInfo);
end;

procedure TfrmPhotoDataBase.BrowseReportsFolder1Click(Sender: TObject);
var
  temp: string;
begin
  Temp := ReportsPath;
  temp := temp + '*.TXT';
  if BrowseForFile('Select report to open', temp, 'TXT') then
    begin
      temp := Format('Notepad.exe %s', [temp]);
      FileExecute(temp, false);
    end;
end;

procedure TfrmPhotoDataBase.UpdatePhotoInfo(BitMap: Graphics.TBitMap);
var
  Msg: string;
begin
  with tblPhotoTable do
    begin
      Edit;

      fldFile_Size.AsFloat      := FileSize64(PathAndFileName);  // May have changed
      fldWidth.AsInteger        := BitMap.Width;                 // (ditto)
      fldHeight.AsInteger       := BitMap.Height;                // (ditto)
      fldUpdateReason.AsInteger := integer(ur_ReplaceCurrentPhoto);

      Post;
      CreateThumbNail(Msg, true);
      Update_Status(Msg);
    end;
end;


procedure TfrmPhotoDataBase.AlternatePhotoEditor1Click(Sender: TObject);
var
  Temp: string;
begin (* TfrmPhotoDataBase.AlternatePhotoEditor1Click *)
  if Empty(fAlternateEditor) then
    fAlternateEditor := gCommonPhotoSettings.AlternatePhotoEditingProgram;

  with tblPhotoTable do
    begin
      Temp := Format('"%s" "%s"', [fAlternateEditor, PathAndFileName]);
      if FileExecute(temp, false) then
        UpdatePhotoInfo(ImgPhoto.RotatedBitmap)
      else
        AlertFmt('Could not edit %', [tblPhotoTable.PathAndFileName]);
    end;
end;  { TfrmPhotoDataBase.AlternatePhotoEditor1Click }

procedure TfrmPhotoDataBase.SaveRotatedImageToFile(imgPhoto: TRotateImage);
{$IfDef dExif2}
var
  FilePath: string;
  OutStream: TMemoryStream;
  Jpg: TJPEGImage;
  ImgData: TImgData;
{$EndIf dExif2}
begin
{$IfNDef dExif2}   // Use the old version of dExif
  AlertFmt('Cannot save orientation: %s', ['Horizontal (normal)']);
{$else}
  FilePath   := tblPhotoTable.PathAndFileName;
  ImgData    := TImgData.Create;
  Jpg        := TJPEGImage.Create;
  if ImgData.ProcessFile(FilePath) then
    begin
      OutStream := TMemoryStream.Create;
      try
        Jpg.Assign(imgPhoto.RotatedBitmap);
        Jpg.SaveToStream(OutStream);
        ImgData.ExifObj.TagValue['Orientation'] := 'Horizontal (normal)';
        // should Height, Width also get updated?
        ImgData.WriteEXIFJpeg(OutStream, FilePath);
      finally
        Jpg.Free;
        ImgData.Free;
        OutStream.Free;
      end;
    end;

  UpdatePhotoInfo(imgPhoto.RotatedBitmap); // Must come after the "free" to prevent locked file
{$EndIf dExif2}
end;

procedure TfrmPhotoDataBase.RotateRight901Click(Sender: TObject);
begin
  imgPhoto.Angle := imgPhoto.Angle - 90;
  SaveRotatedImageToFile(imgPhoto);
end;

procedure TfrmPhotoDataBase.RotateLeft01Click(Sender: TObject);
begin
  imgPhoto.Angle := imgPhoto.Angle + 90;
  SaveRotatedImageToFile(imgPhoto);
end;

procedure TfrmPhotoDataBase.Rotate1801Click(Sender: TObject);
begin
  imgPhoto.Angle := imgPhoto.Angle + 180;
  SaveRotatedImageToFile(imgPhoto);
end;

procedure TfrmPhotoDataBase.cbAllowRotationClick(Sender: TObject);
begin
  fAllowRotation := cbAllowRotation.Checked;
  SizePhoto;
end;

procedure TfrmPhotoDataBase.FillLocationfromLatLon1Click(Sender: TObject);
var
  Lfn: string; Count, MaxCount, Updated: integer;
  OldRecNo: integer;
  Latitude, Longitude: double;
  LocationID: integer;
  temp: string;
  dummy, mr: integer;
  FoundDesc: string;
  OverWrite: boolean;
begin { TfrmPhotoDataBase.FillLocationfromLatLon1Click }
  OverWrite := Yes('If a location is already specified, do you want to overwrite it?');
  tempPhotoTable := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        []);
  fLocationInfo.OkToAdd := false;  // Don't add any new records
  if not Assigned(frmConfirmEachPhoto) then
    frmConfirmEachPhoto := TfrmConfirmEachPhoto.Create(self, 'Update');
  try
  with TempPhotoTable do
    begin
      OnFilterRecord := PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(fExpression);
      MaxCount       := TotalRecordCount;
      Count          := 0;
      Updated        := 0;
      First;
      while not Eof do
        begin
          OldRecNo := RecNo;
          Lfn      := PathAndFileName;

          Latitude    := fldLatitude.AsFloat;
          Longitude   := fldLongitude.AsFloat;
          if (Latitude <> 0) or (Longitude <> 0) then
            begin
              case CommonPhotoSettings.DefaultLocationKind of
                dl_LatLong:
                  temp := CalcLocationStringLong(Latitude, Longitude);
                dl_LastWord:
                  temp := ExtractFileName(RemoveTrailingBackSlash(ExtractFilePath(Lfn)));  // default location to last word in path
                dl_SpecifiedValue:
                  temp := CommonPhotoSettings.DefaultLocation;
              end;

              LocationsTable.InitLocationInfo( fLocationInfo,
                                               ls_PhotoEXIF,
                                               temp);
              LocationID  := LocationsTable.FindLocationID( Latitude,
                                                            Longitude,
                                                            fLocationInfo,
                                                            dummy { ignore count of added records },
                                                            FoundDesc);


              if (LocationID > 0) and (LocationID <> fldLocationID.AsInteger) then
                if fldLocationID.IsNull or (fldLocationID.AsInteger = 0) or OverWrite then
                  begin
                    with frmConfirmEachPhoto do
                      begin
                        Question      := 'Do you want to set the location for this photo to %d';
                        PhotoFileName := PathAndFileName;
                        Caption1      := Format('ID=%d: %s', [LocationID, FoundDesc]);
                        Caption2      := fldKey_Words.AsString;
                        mr            := ShowModal;
                        if mr in [mrYes, mrOk] then
                          begin
                            Edit;
                            fldLocationID.AsInteger := LocationID;
                            fldLatitude.AsFloat     := Latitude;
                            fldLongitude.AsFloat    := Longitude;
                            fldUpdateReason.AsInteger := integer(ur_LocationFromEXIF);  // tested
                            Post;
                            Inc(Updated);
                          end else
                        if mr = mrCancel then
                          break;
                      end;
                  end;
            end;

          if RecNo = OldRecNo then  // selectivity hasn't deleted record from set
            Next;

          inc(Count);
          if (Count mod 100) = 0 then
            Update_Status( Format('Processed %d/%d, (updated=%d)', [Count, MaxCount, Updated]));
        end;
    end;
    Update_Status( Format('COMPLETE. Processed %d/%d, (updated=%d)', [Count, MaxCount, Updated]));
  finally
    tempPhotoTable.Active := false;
    FreeAndNil(tempPhotoTable);
    tblPhotoTable.Refresh;
  end;
end;  { TfrmPhotoDataBase.FillLocationfromLatLon1Click }

procedure TfrmPhotoDataBase.miMoveFileClick(Sender: TObject);
var
  BaseName, SrcFileName, DestFileName, SrcFolderName, DestFolderName, ErrorMsg: string;
begin
  with tblPhotoTable do
    begin
      BaseName       := fldFile_Name.AsString;
      SrcFileName    := PathAndFileName;
      SrcFolderName  := FullFilePath;
      DestFolderName := gRootPath;
      if BrowseForFolder('Destination folder', DestFolderName) then
        begin
          DestFileName := DestFolderName + BaseName;
          if SameText(SrcFolderName, DestFolderName) then
            Alert('Source folder is the same as destination folder')
          else
            begin
              if not ValidPath(DestFolderName, ErrorMsg) then
                raise Exception.Create(ErrorMsg);
              try
                MoveFile(fldKey.AsInteger, BaseName, SrcFolderName, DestFolderName);
                tblPhotoTable.Refresh;
              except
                on e:Exception do
                  Alert(e.Message);
              end;
            end;
        end;
    end;
end;

(*
procedure TfrmPhotoDataBase.TempAfterScroll(Dataset: TDataSet);
begin
  AlertFmt('Scrolled %s', [(DataSet as TMyTable).TableName]);
end;
*)

procedure TfrmPhotoDataBase.MoveFile(SrcKey: integer; const BaseName, OldFolderName, NewFolderName: string);
var
  OldFileName, NewFileName, Dummy, OldThumbNailFileName, NewThumbNailFileName: string;
  DstPathNo: integer;
  TempFilePathsTable: TFilePathsTable;
  result: boolean;
begin { MoveFile }
  NewFileName := NewFolderName + BaseName;
  OldFileName := OldFolderName + BaseName;
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

        TempPhotoTable := TPhotoTable.Create( self,
                                              CommonPhotoSettings.PhotoDBDatabaseFileName,
                                              cFILENAMES,
                                              [optNoSyncFilePathTable {does not seem to work?}]);
        TempFilePathsTable := TFilePathsTable.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, cFILEPATHS, []);
        TempFilePathsTable.Active := true;
        try
          TempPhotoTable.Active      := true;
          DstPathNo   := TempFilePathsTable.FindFolderNo(NewFolderName, dummy, false, false);

          if DstPathNo > 0 then
            with TempPhotoTable do
              begin
                if Locate('Key', SrcKey, []) then
                  begin
                    Edit;
                    fldPATH_NO.AsInteger := DstPathNo;
                    fldUpdateReason.AsInteger := integer(ur_FileMover);
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
                        result := ReNameFile(OldThumbNailFileName, NewThumbNailFileName);
                        if not result then
                          raise Exception.CreateFmt('Unable to rename ThumbNail file from [%s] to [%s]',
                                   [OldThumbNailFileName, NewThumbNailFileName]);
                      end
                    else
                      raise Exception.CreateFmt('The destination ThumbNail [%s] already existed and was not overwritten',
                               [NewThumbNailFileName]);
                  end;
              end
          else
            raise Exception.CreateFmt('Folder "%s" could not be found in the FilePaths table', [NewFolderName]);
        finally
          FreeAndNil(TempFilePathsTable);
          FreeAndNil(TempPhotoTable);
        end;
      end
    else
      AlertFmt('Could NOT rename "%s" to "%s"', [OldFileName, NewFileName]);
end;  { MoveFile }


procedure TfrmPhotoDataBase.CustomKey1Click(Sender: TObject);
begin
  SetOrder(coCustomKey);
end;

procedure TfrmPhotoDataBase.Refresh1Click(Sender: TObject);
var
  BookMark: TBookMark;
begin
  BookMark   := tblPhotoTable.GetBookmark;
  try
    tblPhotoTable.Refresh;
    tblPhotoTable.GotoBookmark(BookMark);
  finally
    tblPhotoTable.FreeBookmark(BookMark);  // free memory used
  end;
end;

(*
procedure TfrmPhotoDataBase.tabPhotoDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
//
end;

procedure TfrmPhotoDataBase.tabPhotoDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  lblStatus.Caption := 'Dragging over tabPhoto';
  Accept := true;
end;

procedure TfrmPhotoDataBase.tabPhotoEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
//
end;

procedure TfrmPhotoDataBase.Panel1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  lblStatus.Caption := 'Dragging over Panel1';
  Accept := true;
end;

procedure TfrmPhotoDataBase.Panel1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
//
end;

procedure TfrmPhotoDataBase.FormDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  lblStatus.Caption := 'Dragging over Form';
  Accept := true;
end;

procedure TfrmPhotoDataBase.FormDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
//
end;

procedure TfrmPhotoDataBase.DropFileTarget1DragOver(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin
  lblStatus.Caption := 'Dragging over DropFileTarget1?';
end;
*)

procedure TfrmPhotoDataBase.RenameFilesinFoldertoDateTimeTaken1Click(
  Sender: TObject);
const
  RENAMED = 'Renamed';
  MINPERDAY = 24 * 60;  
type
  TResultType = (rt_Unknown, rt_Succeeded, rt_Failed);
var
  DOSErr              : integer;
  Attr                : integer;
  OldDir              : string;
  OldPath             : string;
  NewPath             : string;
  PathFilter          : string;
  SearchRec           : TSearchRec;
  ImgData             : TimgData;
  aDateTime           : TDateTime;
  AFileDate           : Integer;
  FilesRenamed        : TextFile;
  FilesRenamedFileName: string;
  Ext                 : string;
  Msg                 : string;
  mt                  : TMediaType;
  mc                  : TMediaClass;
  fn                  : integer;
  OldFileName, NewFileName: {wide}string;
  TempStr             : string;
  Result              : TResultType;
  FailedList          : TStringList;
  DateChangedCount, FailedCount, RenamedCount: integer;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  File_Modified_Date_Local: TDateTime;
  Encoded_Date        : TDateTime;
  File_Modified_Date  : TDateTime;
  File_Created_Date_Local: TDateTime;
  File_Created_Date   : TDateTime;
  MinutesChanged      : single;

  procedure Log(const Result: TResultType; OldFileName, NewFileName: string; const ErrMsg: string = '');
  var
    Msg: string;
    ResultStr, Op: string;
  begin
    case result of
      rt_Unknown:   begin ResultStr := 'Unknown  '; Op := '???' end;
      rt_Succeeded: begin ResultStr := 'Succeeded'; Op := '-->' end;
      rt_Failed:    begin ResultStr := 'Failed   '; Op := '???' end;
    end;
    Msg := Format('%8s %32s %3s %-32s %s', [ResultStr, OldFileName, Op, NewFileName, ErrMsg]);
    WriteLn(FilesRenamed, Msg);
  end;

  procedure RenameTheFile(aDateTime: TDateTime; const Ext: string);
  var
    FractionOfADay: single;
  begin
    DecodeDateTime(aDateTime, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);

    NewFileName := Format('%s%4.4d%2.2d%2.2d-%2.2d%2.2d%2.2d%s',
                          [NewPath, aYear, aMonth, aDay, aHour, aMinute, aSecond, Ext]);
    if RenameFile(OldFileName, NewFileName) then
      begin
        Result := rt_Succeeded;
        Log(Result, OldFileName, NewFileName);
        Inc(RenamedCount);

        if MinutesChanged <> 0 then
          begin
            FractionOfADay := MinutesChanged / MINPERDAY;
            aFileDate      := DateTimeToFileDate(aDateTime + FractionOfADay);
            FileSetDate(NewFileName, aFileDate);
            inc(DateChangedCount);
          end;
      end
    else
      begin
        Msg := SysErrorMessage(GetLastError);
        Result := rt_Failed;
        Log(Result, OldFileName, NewFileName, Msg);
        Inc(FailedCount);
        FailedList.Add(OldFileName);
      end;
  end;

begin
  OldPath := CommonPhotoSettings.ImportExportFolder;
  if BrowseForFolder('Folder to Process', OldPath) then
    begin
      NewPath    := ForceBackSlash(RENAMED);
      AlertFmt('This function will rename/move ALL of the JPG files in the folder "%s" to the folder "%s".',
               [OldPath, NewPath]);

      GetDir(0, OldDir);
      ChDir(OldPath);
      PathFilter := OldPath + '*.*';
      if YesFmt('Move renamed files to "%s"?', [NewPath]) then
        begin
          if not FileExists(RENAMED) then
            begin
              try
                MkDir(NewPath);
              except
                on e: exception do
                  begin
                    Msg := SysErrorMessage(GetLastError);
                    Msg := Format('MkDir failed: %s %s', [NewPath, Msg]);
                    WriteLn(FilesRenamed, Msg);
                    Exit;
                  end;
              end
            end;
        end
      else
        Exit;

      if GetString('Time adjustment', 'Minutes to add to date/time', TempStr) then
        begin
          MinutesChanged := 0.0;
          if IsAllNumeric(TempStr) then
            MinutesChanged := StrToFloat(TempStr);
        end;

      Attr       := faAnyFile - faHidden - faSysFile - faVolumeID - faDirectory;

      DOSErr     := FindFirst(PathFilter, Attr, SearchRec);

      ImgData    := TImgData.Create();
      DateChangedCount := 0;
      RenamedCount     := 0;
      FailedCount      := 0;
      FilesRenamedFileName := UniqueFileName(ReportsPath + 'RenamedFiles.txt');
      AssignFile(FilesRenamed, FilesRenamedFileName);
      Rewrite(FilesRenamed);
      FailedList := TStringList.Create;
      FailedList.Sorted := true;
      try
        while DOSErr = 0 do
          begin
            OldFileName := SearchRec.Name;
            if FailedList.IndexOf(OldFileName) < 0 then // not a previously failed rename
              begin
                Ext         := ExtractFileExt(OldFileName);
                mt          := MediaTypeFromExtension(Ext);
                mc          := MediaInfoArray[mt].MediaClass;

                Result   := rt_Unknown;
                if (mc = mc_Photos) and ImgData.ProcessFile(OldFileName) and ImgData.HasEXIF then
                  begin
                    aDateTime := ImgData.ExifObj.GetImgDateTime;
                    if aDateTime <> BAD_DATE then
                      RenameTheFile(aDateTime, Ext);
                  end else
                 if (mc = mc_Video) then
                   begin
                     if GetMediaDates( OldFileName,
                                       File_Modified_Date_Local,
                                       Encoded_Date,
                                       File_Modified_Date,
                                       File_Created_Date_Local,
                                       File_Created_Date) then
                       if Encoded_Date <> BAD_DATE then
                         RenameTheFile(Encoded_Date, Ext) else
                       if File_Modified_Date_Local <> BAD_DATE then
                         RenameTheFile(File_Modified_Date_Local, Ext)
                       else
                         result := rt_Failed;
                   end;

                if Result = rt_Succeeded then
                  begin
                    // After a sucessful RenameFile (i.e. move) operation,
                    // the SearchRec may no longer valid and must be re-initialized.
                    FindClose(SearchRec);
                    DOSErr := FindFirst(PathFilter, Attr, SearchRec);
                  end
                else
                  DOSErr := FindNext(SearchRec);
              end
            else
              DOSErr := FindNext(SearchRec);  // just skip over it
          end;

      finally
        FindClose(SearchRec);
        ImgData.Free;
        Msg := Format('%d Files Renamed, %d Rename Fails, %d Date Changed',
                   [RenamedCount, FailedCount, DateChangedCount]);
        WriteLn(FilesRenamed, Msg);
        CloseFile(FilesRenamed);
        Message(Msg);
        EditTextFile(FilesRenamedFileName);

        try
          if (FailedList.Count > 0) and YesFmt('Do you want to delete the %d files that were fails?',
                                               [FailedList.Count]) then
            for fn := 0 to FailedList.Count-1 do
              DeleteFile(FailedList[fn]);
        finally
          FreeAndNil(FailedList);
        end;

        ChDir(OldDir);
      end;
    end;
end;

end.



