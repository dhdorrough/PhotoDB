program PhotoDB;

{%ToDo 'PhotoDB.todo'}
{%File '..\..\GPX Editor\GPX_Editor 1-6-16\lib\FastMM\FastMM4Options.inc'}

uses
  FastMM4,
  Forms,
  uPhotoDB in 'uPhotoDB.pas' {frmPhotoDataBase},
  PDB_Decl in 'PDB_Decl.pas',
  PDBTables in 'PDBTables.pas',
  uHandleMissingFile in 'uHandleMissingFile.pas' {frmHandleMissingFile},
  uPhotoDBOptions in 'uPhotoDBOptions.pas' {frmPhotoDBOptions},
  MyUtils in '..\..\MyUtils\MyUtils.pas',
  About in 'About.pas' {AboutBox},
  PDBUtils in 'PDBUtils.pas',
  CopySelectedRecords in 'CopySelectedRecords.pas' {frmCopySelectedFiles},
  SettingsFiles in '..\..\MyUtils\SettingsFiles.pas',
  PhotoDBCommonSettings in 'PhotoDBCommonSettings.pas',
  FullSize in 'FullSize.pas' {frmFullSize},
  KillIt in '..\..\MyUtils\KillIt.pas',
  DeletionConfirmation in 'DeletionConfirmation.pas' {frmDeletionConfirmation},
  ReportOptions in 'ReportOptions.pas' {frmReportOptions},
  Synonyms in 'Synonyms.pas',
  FileMover in 'FileMover.pas' {frmFileMover},
  DropSource in '..\..\DragDrop\Source\DropSource.pas',
  DragDropFile in '..\..\DragDrop\Source\DragDropFile.pas',
  ParseExpr in '..\..\MyUtils\ParseExpr.pas',
  PathBrowser in 'PathBrowser.pas' {frmPathBrowser},
  LocationUtils in '..\..\MyUtils\LocationUtils.pas',
  UpdateLocationInfoFromGPX in 'UpdateLocationInfoFromGPX.pas' {frmUpdateLocationInfo},
  MyDelimitedParser in '..\..\MyUtils\MyDelimitedParser.pas',
  GPSTrackingInfoUnit in 'GPSTrackingInfoUnit.pas',
  FillLocationInfo in 'FillLocationInfo.pas' {frmFillLocationInfo},
  LocationsBrowser in 'LocationsBrowser.pas' {frmLocationsBrowser},
  EXIFInfo in 'EXIFInfo.pas' {frmExifInfo},
  MyReplaceDialog in 'MyReplaceDialog.pas' {frmReplaceDialog},
  ThumbNailUnit in '..\..\MyUtils\ThumbNailUnit.pas',
  ExtractImage2 in '..\..\MyUtils\ExtractImage2.pas',
  uExtractImg in '..\..\ThumbNails\uExtractImg.pas',
  TracksBrowser in '..\..\Hiking Manager\Src\TracksBrowser.pas' {frmTracksBrowser},
  AdjustDateTime in 'AdjustDateTime.pas' {frmAdjustDateTime},
  JpegConv in '..\..\MakeHTML\JpegConv.pas',
  BoundariesBrowser in 'BoundariesBrowser.pas' {frmBoundariesBrowser},
  uGetString in '..\..\MyUtils\uGetString.pas' {frmGetString},
  LookupsTableUnit in '..\..\MyUtils\LookupsTableUnit.pas',
  FilterOptions in '..\..\MyUtils\FilterOptions.pas' {frmFilterOptions},
  FilterLocations in '..\..\MyUtils\FilterLocations.pas' {frmFilterLocation},
  RecordAudio in 'RecordAudio.pas' {frmAudioRecorder},
  LookupBrowser in '..\..\MyUtils\LookupBrowser.pas' {frmLookupBrowser},
  FilterSettingsUnit in 'FilterSettingsUnit.pas',
  ReplaceFieldsInSelectedRecords in '..\..\NFIMR\Src\ReplaceFieldsInSelectedRecords.pas' {frmReplaceFieldsInSelectedRecords},
  MyTables in '..\..\MyUtils\MyTables.pas',
  PhotoUtils in '..\..\MyUtils\PhotoUtils.pas',
  HikingTables in '..\..\Hiking Manager\Src\HikingTables.pas',
  Expression in '..\..\MyUtils\Expression.pas' {frmExpression},
  ScanForDocuments in 'ScanForDocuments.pas' {frmScanForDocuments},
  DateTimeDiff in 'DateTimeDiff.pas' {frmCalcTimeDiff},
  ScanForDuplicatesOptions in 'ScanForDuplicatesOptions.pas' {frmScanForDuplicatesOptions},
  uBrowseMemo in '..\..\MyUtils\uBrowseMemo.pas' {frmBrowseMemo},
  BrowseFuncKey in 'BrowseFuncKey.pas' {frmFuncKeyBrowser},
  StrCompareX in '..\..\MyUtils\StrCompareX.pas',
  HikingSettingsUnit in '..\..\Hiking Manager\Src\HikingSettingsUnit.pas',
  Hiking_Decl in '..\..\Hiking Manager\Src\Hiking_Decl.pas',
  BrowseScenes in 'BrowseScenes.pas' {frmBrowseScenes},
  BrowserUnit in '..\..\MyUtils\BrowserUnit.pas' {frmDataSetBrowser},
  PhotoTableBrowser in 'PhotoTableBrowser.pas' {frmPhotoTableBrowser},
  GenerateHTML2 in 'GenerateHTML2.pas' {HTMLGenerator},
  ConfirmEachPhoto in 'ConfirmEachPhoto.pas' {frmConfirmEachPhoto},
  OverwriteOrAppend in 'OverwriteOrAppend.pas' {frmOverWriteOrAppend},
  TableCreation in '..\..\MyUtils\TableCreation.pas',
  Export2 in 'Export2.pas' {frmExport2},
  ScanningOptions in 'ScanningOptions.pas' {frmScanningOptions},
  MediaInfoDll in '..\..\MyUtils\MediaInfoDll.pas',
  VideoStuff2 in 'VideoStuff2.pas',
  PathConversions in '..\..\MyUtils\PathConversions.pas',
  RotImg in '..\..\RotateImages\Src\RotImg.pas',
  YesNoDontAskAgain in '..\..\MyUtils\YesNoDontAskAgain.pas' {frmYesNoDontAskAgain},
  dEXIF in '..\..\dExif2\dexif-master\dEXIF.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'PhotoDB- Document Database';
  Application.CreateForm(TfrmPhotoDataBase, frmPhotoDataBase);
  Application.CreateForm(TfrmReplaceDialog, frmReplaceDialog);
  Application.Run;
end.
