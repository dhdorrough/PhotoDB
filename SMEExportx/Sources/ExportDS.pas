{ SMExport suite
  TSMExportBaseComponent basic type

  Copyright (C) 1998-2007, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
// A3971-235CF
}
unit ExportDS;

interface

{$I sme.inc}

uses
  Windows, Classes, Controls, Graphics, SMEEngine, SMEStat, DB;

type
  TTableTypeExport = (teParadox, teDBase, teText, teHTML, teXLS, teExcel, teWord, teSYLK, teDIF, teWKS, teQuattro, teSQL, teXML, teAccess, teClipboard, teRTF, teSPSS, tePDF, teLDIF, teADO);
  TExportFormatTypes = set of TTableTypeExport;
  TCharacterSet = (csANSI_WINDOWS, csASCII_MSDOS, csEBCDIC {IBM mainframe});

const
  AllFormats: TExportFormatTypes = [{$IFDEF BDE_SUPPORT} teParadox, {$ENDIF BDE_SUPPORT}
                                    teDBase, teText, teHTML, teXLS, teExcel, teWord, teSYLK, teDIF, teWKS, teQuattro, teSQL, teXML, teAccess, teClipboard, teRTF, teSPSS, tePDF, teLDIF, teADO];
  GeneratorVer: string = 'SMExport 4.77';

type
  TSMEStyle = (esNormal, esPriceList, esMSMoney, esBrick, esDesert, esEggplant, esLilac, esMaple, esMarine, esRose, esSpruce, esWheat);
  TSMExportStyle = class(TPersistent)
  private
    FStyle: TSMEStyle;
    FOddColor: TColor;
    FEvenColor: TColor;

    procedure SetStyle(Value: TSMEStyle);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TSMEStyle read FStyle write SetStyle;
    property OddColor: TColor read FOddColor write FOddColor;
    property EvenColor: TColor read FEvenColor write FEvenColor;
  end;

  {custom "regional settings"}
  TSMEDateOrder = (doMDY, doDMY, doYMD, doYDM, doDYM, doMYD);
  TSMEDataFormats = class(TPersistent)
  private
    { Private declarations }
    FDateOrder: TSMEDateOrder;
    FDateSeparator: Char;
    FTimeSeparator: Char;
    FFourDigitYear: Boolean;
    FLeadingZerosInDate: Boolean;
    FThousandSeparator: Char;
    FDecimalSeparator: Char;
    FCurrencyString: string;

    FCustomDateTimeFormat: string;

    FBooleanTrue: string;
    FBooleanFalse: string;

    FUseRegionalSettings: Boolean;
  protected
    { Protected declarations }
    function GetDTSeparator(Value: Char): string;
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;

    procedure LoadRegionalSettings;

    function GetDateFormat: string;
    function GetTimeFormat: string;
    function GetDateTimeFormat: string;
  published
    { Published declarations }
    property DateOrder: TSMEDateOrder read FDateOrder write FDateOrder default doMDY;
    property DateSeparator: Char read FDateSeparator write FDateSeparator;
    property TimeSeparator: Char read FTimeSeparator write FTimeSeparator;
    property FourDigitYear: Boolean read FFourDigitYear write FFourDigitYear;
    property LeadingZerosInDate: Boolean read FLeadingZerosInDate write FLeadingZerosInDate;
    property ThousandSeparator: Char read FThousandSeparator write FThousandSeparator;
    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
    property CurrencyString: string read FCurrencyString write FCurrencyString;

    property CustomDateTimeFormat: string read FCustomDateTimeFormat write FCustomDateTimeFormat;

    property BooleanTrue: string read FBooleanTrue write FBooleanTrue;
    property BooleanFalse: string read FBooleanFalse write FBooleanFalse;

    property UseRegionalSettings: Boolean read FUseRegionalSettings write FUseRegionalSettings;
  end;

  TSMESendTo = class(TPersistent)
  private
    { Private declarations }
    FEMailRecipient: string;
    FEMailCC: string;
    FEMailBCC: string;
    FEMailSubject: string;
    FEMailBody: string;
    FEMailOpen: Boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property EMailRecipient: string read FEMailRecipient write FEMailRecipient;
    property EMailCC: string read FEMailCC write FEMailCC;
    property EMailBCC: string read FEMailBCC write FEMailBCC;
    property EMailSubject: string read FEMailSubject write FEMailSubject;
    property EMailBody: string read FEMailBody write FEMailBody;
    property EMailOpenBeforeSend: Boolean read FEMailOpen write FEMailOpen default True;
  end;

  TSMEAbout = string;//[10];

  TSMEBeforeRecordEvent = procedure(Sender: TObject; var Accept: Boolean) of object;
  TSMEAfterRecordEvent = procedure(Sender: TObject; var Abort: Boolean) of object;

  TGetCellParamsEvent = procedure(Sender: TObject; Field: TField; var Text: string; AFont: TFont; var Alignment: TAlignment; var Background: TColor; var CellType: TCellType) of object;
  TSMEGetLanguageStringEvent = procedure(Sender: TObject; id: Integer; var Text: string) of object;

  TSMEGetNextSelected = procedure(Sender: TObject; Index: Integer; var Selected: TBookmark) of object;
  TSMEGetSelectedCount = procedure(Sender: TObject; var Count: LongInt) of object;

  TSMESpecificationEvent = procedure(Sender: TObject; var AFileName: string) of object;
  TSMEGetFileName = procedure(Sender: TObject; CurFileNumber: Integer; var AFileName: string) of object;
  TSMEProgress = procedure(Sender: TObject; CurValue, MaxValue: Integer; var Abort: Boolean) of object;

  TColumnSource = (csDBGrid, csDataSet, csDataEngine);
  TSMELayout = (elColumnar, elReversedColumnar, elTabularForm);
  TActionAfterExport = (aeNone, aeOpenView, aeEMail);

  TSMOption = (soFieldMask, soShowMessage, soBlankRowAfterCaptions, soMergeData, soWaitCursor, soDisableControls, soUseFieldNameAsCaption, soColLines, soRowLines, soColorsFonts, soExportBlankValues, soExportBands);
  TSMOptions = set of TSMOption;

  TSMEResult = (erInProgress, erCompleted, erCanceled, erFailed);
  TSMESection = (esHeader, esBand, esTitle, esData, esFooter);

  TSMEStatistic = class(TPersistent)
  private
    { Private declarations }
    FTotalCount: LongInt;
    FTotalErrors: LongInt;

    FUpdateStep: LongInt;

    FResult: TSMEResult;

    FCurrentRow: LongInt;
    FCurrentCol: LongInt;
    FCurrentSection: TSMESection;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;

    property CurrentRow: LongInt read FCurrentRow write FCurrentRow;
    property CurrentCol: LongInt read FCurrentCol write FCurrentCol;
    property CurrentSection: TSMESection read FCurrentSection write FCurrentSection;
  published
    { Published declarations }
    property TotalCount: LongInt read FTotalCount write FTotalCount;
    property TotalErrors: LongInt read FTotalErrors write FTotalErrors stored False;

    property UpdateStep: LongInt read FUpdateStep write FUpdateStep default 1;

    property Result: TSMEResult read FResult write FResult;
  end;

  TSMECustomBaseComponent = class(TComponent)
  private
    { Private declarations }
    FAbout: TSMEAbout;
    FOptions: TSMOptions;

    FCharacterSet: TCharacterSet;
    FRowsPerFile: LongInt;

    FRightToLeft: Boolean;
    FSelectedRecord: Boolean;
    FAddTitle: Boolean;
    FKeyGenerator: string;
    FAnimatedStatus: Boolean;

    FBlankIfZero: Boolean;

    FHeader: TStrings;
    FFooter: TStrings;

    FExportStyle: TSMExportStyle;
    FDataFormats: TSMEDataFormats;

    FSendTo: TSMESendTo;
    FExportIfEmpty: Boolean;

    FPageSetup: TSMEPageSetup;

    FOnBeforeRecord: TSMEBeforeRecordEvent;
    FOnAfterRecord: TSMEAfterRecordEvent;

    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetLanguageString: TSMEGetLanguageStringEvent;

    FOnBeforeExecute: TNotifyEvent;
    FOnAfterExecute: TNotifyEvent;

    FOnGetFileName: TSMEGetFileName;
    FOnProgress: TSMEProgress;

    FSeparator: Char;
    FTextQualifier: Char;
    FRecordSeparator: string;
    FFixed: Boolean;

    FTableName: string;
    FTableType: TTableTypeExport;

    FStatistic: TSMEStatistic;

    procedure SetPageSetup(Value: TSMEPageSetup);
    procedure SetExportStyle(Value: TSMExportStyle);
    procedure SetSendTo(Value: TSMESendTo);

    function GetHeader: TStrings;
    procedure SetHeader(Value: TStrings);
    function GetFooter: TStrings;
    procedure SetFooter(Value: TStrings);

    function GetExportedRecordCount: Integer;
    function GetExportResult: TSMEResult;

    function Translate(const s: string; IsEncrypt: Boolean): string;
    procedure SetAbout(const Value: TSMEAbout);
  protected
    { Protected declarations }
//    FExportedRecordCount: Integer;
//    FExportResult: TSMEResult;

    procedure SetTableType(Value: TTableTypeExport); virtual;

    function GetFieldType(Field: TField; BlankIfZero: Boolean): TCellType;
    function MappingVersion: Integer; virtual;
    function GetSysValue(const AName: string): Variant;

    procedure AssignStyle(ARow: LongInt; var color: TColor);

    procedure Prepare; virtual;
    function AddZeros(const Source: string; Len: Integer; IsLeading: Boolean): string;
    function GetFieldStr(Field: TField): string; virtual;

    function GetLanguageString(id: Integer): string;

    property Header: TStrings read GetHeader write SetHeader;
    property Footer: TStrings read GetFooter write SetFooter;
    property RowsPerFile: LongInt read FRowsPerFile write FRowsPerFile default 0;
    property TableType: TTableTypeExport read FTableType write SetTableType;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AboutSME;
    function SendMail(const AFileName, body: string): Integer;

    function Extension: string; virtual;
    function GetTitleRowCount: Integer; 

    property AddTitle: Boolean read FAddTitle write FAddTitle;
    property CharacterSet: TCharacterSet read FCharacterSet write FCharacterSet;
    property OnGetCellParams: TGetCellParamsEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnGetCellParams {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnGetCellParams;

    property ExportedRecordCount: Integer read GetExportedRecordCount;
    property ExportResult: TSMEResult read GetExportResult;

    property PageSetup: TSMEPageSetup read FPageSetup write SetPageSetup;
    property ExportStyle: TSMExportStyle read FExportStyle write SetExportStyle;

    property TableName: string read FTableName write FTableName;

    property TextQualifier: Char read FTextQualifier write FTextQualifier default #0;
    property Separator: Char read FSeparator write FSeparator default #9;
    property RecordSeparator: string read FRecordSeparator write FRecordSeparator;

    property Fixed: Boolean read FFixed write FFixed;
  published
    { Published declarations }
    property About: TSMEAbout read FAbout write SetAbout stored False;
    property AnimatedStatus: Boolean read FAnimatedStatus write FAnimatedStatus;

    property DataFormats: TSMEDataFormats read FDataFormats write FDataFormats;

    property KeyGenerator: string read FKeyGenerator write FKeyGenerator;
    property SelectedRecord: Boolean read FSelectedRecord write FSelectedRecord;
    property BlankIfZero: Boolean read FBlankIfZero write FBlankIfZero;
    property Options: TSMOptions read FOptions write FOptions default [soShowMessage, soWaitCursor, soDisableControls, soColLines, soRowLines];

    property RightToLeft: Boolean read FRightToLeft write FRightToLeft;
    property SendTo: TSMESendTo read FSendTo write SetSendTo;
    property ExportIfEmpty: Boolean read FExportIfEmpty write FExportIfEmpty default True;

    property Statistic: TSMEStatistic read FStatistic write FStatistic;

    property OnGetLanguageString: TSMEGetLanguageStringEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnGetLanguageString {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnGetLanguageString;

    property OnBeforeRecord: TSMEBeforeRecordEvent read FOnBeforeRecord write FOnBeforeRecord;
    property OnAfterRecord: TSMEAfterRecordEvent read FOnAfterRecord write FOnAfterRecord;

    property OnBeforeExecute: TNotifyEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnBeforeExecute {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnAfterExecute {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnAfterExecute;
    property OnGetFileName: TSMEGetFileName {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnGetFileName {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnGetFileName;
    property OnProgress: TSMEProgress {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnProgress {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnProgress;
  end;

  TSMExportBaseComponent = class(TSMECustomBaseComponent)
  private
    { Private declarations }
    FActionAfterExport: TActionAfterExport;

    FDBGrid: TCustomControl{TCustomGrid};
    FDataSet: TDataSet;
    FDataEngine: TSMECustomDataEngine;
    FColumnSource: TColumnSource;

    FFileName: string;

    FOnGetNextSelected: TSMEGetNextSelected;
    FOnGetSelectedCount: TSMEGetSelectedCount;

    FOnBeforeLoadSpecification: TSMESpecificationEvent;
    FOnAfterLoadSpecification: TSMESpecificationEvent;
    FOnBeforeSaveSpecification: TSMESpecificationEvent;
    FOnAfterSaveSpecification: TSMESpecificationEvent;

    FSpecificationDir: string;

    FColumns: TSMEColumns;
    FBands: TSMEColumnBands;

    FLayout: TSMELayout;

    FLastExportColumnIndex: Integer;

    intRecordCount: LongInt;
    IsCustomColumns, IsAborted: Boolean;

    oldDecimalSeparator,
    oldDateSeparator,
    oldTimeSeparator,
    oldThousandSeparator: Char;
    oldCurrencyString: string;

    function GetColumns: TSMEColumns;
    procedure SetColumns(Value: TSMEColumns);

    function GetBands: TSMEColumnBands;
    procedure SetBands(Value: TSMEColumnBands);

    function GetDBGrid: TCustomControl;
    procedure SetDBGrid(AValue: TCustomControl);

    function GetDataSet: TDataSet;
    procedure SetDataSet(AValue: TDataSet);

    function GetDataEngine: TSMECustomDataEngine;
    procedure SetDataEngine(AValue: TSMECustomDataEngine);
  protected
    { Protected declarations }
    {animated status dialog}
    boolAborted: Boolean;
    frmSMEProcess: TfrmSMEProcess;

    FIsWWDBGrid: Boolean;
    FIsTMSDBGrid: Boolean;

    {dataengine with data for exporting
    PS: require for compartibility with oldest versions where DataEngine is not present - DBGrid+DataSet only}
    SourceDataEngine: TSMECustomDataEngine;
    procedure CreateSourceDataEngine; virtual;
    procedure DestroySourceDataEngine; virtual;

    procedure CreateDirIfNotExists(const s: string);

    {animated status dialog}
    function CreateProgressDlg(strCaption, strMessage, strBtnCaption: string; MinValue, MaxValue, Progress: Longint): Boolean;
    function DestroyProgressDlg: Boolean;
    function UpdateProgressDlg(strMessage: string; Progress: Longint): Boolean;
    function ProgressCanceled: Boolean;
    procedure SetProgressCanceled(Value: Boolean);

    procedure InternalBeforeProcess; virtual;
    procedure InternalAfterProcess; virtual;

    procedure SetTableType(Value: TTableTypeExport); override;
    procedure AfterExport; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetNextSelected(i: Integer): TBookmark;
    function GetSelectedCount: Integer;

    function IsMemoColumnExist: Boolean;
    function IsTrialCount(i: Integer): Boolean;

    property Layout: TSMELayout read FLayout write FLayout default elColumnar;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure BuildDefaultColumns;
    procedure Execute;

    function GetDS: TDataSet;

//    procedure FillColumns;

    class function GetDefExt(intIndex: Integer): string; {$IFDEF CLR} static; {$ENDIF}

    procedure LoadSpecification(strFileName: string);
    procedure SaveSpecification(SpecName, FileName: string; ShowDialog: Boolean);
    procedure LoadSpecificationFromStream(Stream: TStream);
    procedure SaveSpecificationToStream(Stream: TStream; const SpecName: string);

    property IsWWDBGrid: Boolean read FIsWWDBGrid;
    property IsTMSDBGrid: Boolean read FIsTMSDBGrid;

    property ActionAfterExport: TActionAfterExport read FActionAfterExport write FActionAfterExport default aeNone;
    property FileName: string read FFileName write FFileName;

    property LastExportColumnIndex: Integer read FLastExportColumnIndex;
  published
    { Published declarations }
    property Columns: TSMEColumns read GetColumns write SetColumns;
    property Bands: TSMEColumnBands read GetBands write SetBands;

    property DBGrid: TCustomControl read GetDBGrid write SetDBGrid;
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property DataEngine: TSMECustomDataEngine read GetDataEngine write SetDataEngine;
    property ColumnSource: TColumnSource read FColumnSource write FColumnSource;

    property SpecificationDir: string read FSpecificationDir write FSpecificationDir;

    property OnGetNextSelected: TSMEGetNextSelected {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnGetNextSelected {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnGetNextSelected;
    property OnGetSelectedCount: TSMEGetSelectedCount {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnGetSelectedCount {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnGetSelectedCount;

    property OnBeforeLoadSpecification: TSMESpecificationEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnBeforeLoadSpecification {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnBeforeLoadSpecification;
    property OnAfterLoadSpecification: TSMESpecificationEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnAfterLoadSpecification {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnAfterLoadSpecification;
    property OnBeforeSaveSpecification: TSMESpecificationEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnBeforeSaveSpecification {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnBeforeSaveSpecification;
    property OnAfterSaveSpecification: TSMESpecificationEvent {$IFDEF CLR}add{$ELSE}read{$ENDIF} FOnAfterSaveSpecification {$IFDEF CLR}remove{$ELSE}write{$ENDIF} FOnAfterSaveSpecification;
  end;

const
   arrStyleColor: array[TSMEStyle] of array[Boolean] of TColor = (
      {Odd/Even colores}
      (clWindow, clWindow), {esNormal}
      (cl3DLight, clWindow), {esPriceList}
      (TColor($FFFBF0), clWindow), {esMSMoney}
      (TColor($CFDFDF), TColor($A0BFBF)), {esBrick}
      (TColor($B8CCD8), TColor($688CA0)), {esDesert}
      (TColor($A8B090), TColor($788058)), {esEggplant}
      (TColor($D8A8B0), TColor($B05058)), {esLilac}
      (TColor($A8D8EB), TColor($48A8C8)), {esMaple}
      (TColor($B8C088), TColor($889048)), {esMarine}
      (TColor($B8B0D0), TColor($7060A0)), {esRose}
      (TColor($A8C8A0), TColor($689858)), {esSpruce}
      (TColor($A0E0E0), TColor($40BCC0)) {esWheat}
     );

procedure AboutSMExport;
function UECode(WinToDOS: Boolean; s: string): string;
function EBCDICCode(s: string): string;
function IsStringField(Field: TField): Boolean;

function PadLSpace(strStr: string; intLen: Integer): string;
function PadRSpace(strStr: string; intLen: Integer): string;
function IsNumericField(Field: TField): Boolean;

function GetPropAsSelectedList(comp: TComponent): TList;

function ReplaceHTMLSystemChars(const s: string): string;
function TblName(DS: TDataset): string;

function GetFileNameByGraphic(const FileName: string; intCurPicFile: Integer; pic: TPicture): string;

const
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($FFFBF0);

implementation

uses SysUtils, TypInfo, ExCnst, MAPI, Forms, Dialogs,
     ShellAPI, DBGrids, SMESave, SMEUtils, IniFiles,
     SMEEngDB, SMEEngWW, SMEMIME
     {$IFDEF SM_USE_JPEG_FOR_HTML} , jpeg {$ENDIF}
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

const
  strRegInfo = 'For additional tech information or support ask Mike Shkolnik, Scalabium:' + #13#10 +
               'http://www.scalabium.com' + #13#10 +
               'e-mail: support@scalabium.com, mshkolnik@scalabium.com, mshkolnik@yahoo.com' + #13#10#13#10 +
               'You can order a product at:' + #13#10 +
               '1. http://www.regsoft.com/cgi-bin/reg_it_offsite.pl?13585' + #13#10 +
               '2. https://secure.element5.com/register.html?productid=131572';

procedure AboutSMExport;
begin
  Application.MessageBox('SMExport suite is a components for data export from any DataSet/DBGrid:' + #13#10 +
                         '  - TSMExportToXLS export into MS Excel file (without OLE!!)' + #13#10 +
                         '  - TSMExportToExcel export into MS Excel file (using OLE)' + #13#10 +
                         '  - TSMExportToWord export into MS Word document (using OLE)' + #13#10 +
                         '  - TSMExportToRTF export into RichText format' + #13#10 +
                         '  - TSMExportToAccess export into table of MS Access database (using DAO)' + #13#10 +
                         '  - TSMExportToHTML export into HTML file' + #13#10 +
                         '  - TSMExportToXML export into XML file' + #13#10 +
                         '  - TSMExportToText export into text file without BDE' + #13#10 +
                         '  - TSMExportToSYLK export into SYLK (Symbolic Link) file' + #13#10 +
                         '  - TSMExportToDIF export into DIF (Data Interchange Format) file' + #13#10 +
                         '  - TSMExportToWKS export into Lotus 1-2-3 spreadsheet' + #13#10 +
                         '  - TSMExportToQuattro export into QuattroPro spreadsheet' + #13#10 +
                         '  - TSMExportToBDE export into Paradox/DBase/ASCII' + #13#10 +
                         '  - TSMExportToSQL generate the SQL script with data dump' + #13#10 +
                         '  - TSMExportToClipboard copy a data into MS Windows Clipboard' + #13#10 +
                         '  - TSMExportToSPSS copy a data into SPSS data file' + #13#10 +
                         '  - TSMExportToPDF copy a data into Adobe Acrobat Document' + #13#10 +
                         '  - TSMExportToADO export into any ADO datasource' + #13#10 +
                         '  - TSMExportToDataset copy a data into some other dataset' + #13#10 +
                         #13#10 +
                         '  - TSMExportMonitor is a compound component for data exporting with user dialog possibility' + #13#10 +
                         '  - TSMEWizardDlg is a wizard dialog component for data exporting' + #13#10 +
                         #13#10 + strRegInfo,
                         PChar('About ' + GeneratorVer), MB_OK);
end;

function GetFileNameByGraphic(const FileName: string; intCurPicFile: Integer; pic: TPicture): string;
var
  TempPath, TempFileName: array[0..MAX_PATH] of Char;
begin
  if FileName = '' then
  begin
    {create a temporary file with picture}
    GetTempPath(MAX_PATH, @TempPath);
    GetTempFileName(@TempPath, 'SME', 0, @TempFileName);
    DeleteFile(TempFileName);
    Result := ChangeFileExt(StrPas(@TempFileName), '')
  end
  else
    Result := ExtractFilePath(FileName) + 'img' + IntToStr(intCurPicFile);

  if Assigned(pic.Bitmap) then
    Result := Result + '.bmp'
  else
  if Assigned(pic.Metafile) then
  begin
    if pic.Metafile.Enhanced then
      Result := Result + '.emf'
    else
      Result := Result + '.wmf';
  end
  else
  if Assigned(pic.Icon) then
    Result := Result + '.ico'
  else
  {$IFDEF SM_USE_JPEG_FOR_HTML}
  if Assigned(pic.Graphic) and
     (pic.Graphic is TJPEGImage) then
    Result := Result + '.jpg'
  else
  {$ENDIF}
    Result := Result + '.bmp';
end;

function GetPropAsSelectedList(comp: TComponent): TList;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  if (comp is TCustomControl {TCustomGrid}) then
  begin
    PropInfo := GetPropInfo(comp.ClassInfo, 'SelectedRows');

    {if such properties exists}
    if Assigned(PropInfo) and
       (PropInfo.PropType^.Kind = tkClass) then
      Result := TList(GetOrdProp(comp, PropInfo));
  end
end;

function GetPropAsString(comp: TComponent): string;
var
  PropInfo: PPropInfo;
begin
  Result := '';
  PropInfo := GetPropInfo(comp.ClassInfo, 'TableName');

  {if such properties exists}
  if Assigned(PropInfo) then
    Result := GetStrProp(comp, PropInfo);
end;

function GetPropAsTStrings(comp: TComponent): string;
var
  PropInfo: PPropInfo;
begin
  Result := '';
  PropInfo := GetPropInfo(comp.ClassInfo, 'SQL');

  {if such properties exists}
  if Assigned(PropInfo) and
     (PropInfo.PropType^.Kind = tkClass) then
    Result := TStrings(GetOrdProp(comp, PropInfo)).Text;
end;

function StrToFloatDef(s: string; DefValue: Double): Double;
begin
  Result := DefValue;
  if Trim(s) <> '' then
    try
      Result := StrToFloat(s)
    except
    end;
end;

function ReplaceHTMLSystemChars(const s: string): string;
var
  i: Integer;
  ch: Char;
  strNew: string;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    ch := s[i];
    case ch of
      '|': strNew := '&brvbar;';
      '°': strNew := '&deg;';
      '±': strNew := '&plusmn;';
      '¶': strNew := '&para;';
      '&': strNew := '&amp;';
      '<': strNew := '&lt;';
      '>': strNew := '&gt;';
      '©': strNew := '&copy;';
      '"': strNew := '&quot;';
      '''': strNew := '&apos;';
      '™': strNew := '&trade;';
      #10: strNew := '&car;';
      #13: strNew := '&ret;';
    else
      strNew := ch
    end;
    Result := Result + strNew
  end
end;

function TblName(DS: TDataset): string;
const
  Brackets = ['(',')','[',']','{','}'];
  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;
var
  s: string;
  i: Integer;
begin
  Result := '';
  if Assigned(DS) then
  begin
    Result := GetPropAsString(DS);
    if Result = '' then
    begin
//      s := UpperCase(GetPropAsTStrings(DS));
      s := GetPropAsTStrings(DS);
      if s <> '' then
      begin
        i := Pos('FROM ', s);
        if (i > 0) then
          s := Copy(s, i+5, Length(s)-i-5);
        i := 1;
        while not (s[i] in StdWordDelims) and (i <= Length(s)) do
          Inc(i);
        Result := Copy(s, 1, i-1);
      end;
    end;
    i := Pos('.', Result);
    if i > 0 then
      Result := Copy(Result, 1, i-1);
  end;
end;

function IsNumericField(Field: TField): Boolean;
begin
  Result := (Field.DataType in [ftSmallint, ftInteger, ftWord,
  {$IFDEF SMForDelphi4}
        ftLargeInt,
  {$ENDIF}
                                ftFloat, ftCurrency, ftBCD, ftAutoInc]);
end;

function IsStringField(Field: TField): Boolean;
begin
  Result := (Field.DataType in [ftString, ftMemo, ftFmtMemo]) or
            Field.IsBlob;
end;

{function ExportAsciiField(Field: TField): Boolean;
begin
  Result := Field.Visible and not (Field.DataType in
            [ftUnknown, ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic,
             ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary]);
end;
}


{function OemToAnsiStr(const s: string): string;
begin
  SetLength(Result, Length(s));
  if Length(Result) > 0 then
    OemToChar(PChar(s), PChar(Result));
end;
}
function AnsiToOemStr(const s: string): string;
begin
  SetLength(Result, Length(s));
  if Length(Result) > 0 then
    CharToOem(PChar(s), PChar(Result));
end;
{
function StringToWideStringEx(const S: String; CodePage: Word): WideString;
var
  L: Integer;
begin
  L:= MultiByteToWideChar(CodePage, 0, PChar(S), -1, nil, 0);
  SetLength(Result, L-1);
  MultiByteToWideChar(CodePage, 0, PChar(S), -1, PWideChar(Result), L - 1);
end;

function WideStringToStringEx(const WS: WideString; CodePage: Word): String;
var
  L: Integer;
begin
  L := WideCharToMultiByte(CodePage, 0, PWideChar(WS), -1, nil, 0, nil, nil);
  SetLength(Result, L-1);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), -1, PChar(Result), L - 1, nil, nil);
end;
}
function UECode(WinToDOS: Boolean; s: string): string;
var
  i: Integer;
  arrW: array[0..MAX_PATH] of WideChar;
begin
  if WinToDOS then
    Result := AnsiToOemStr(s)
  else
    Result := '';
  exit;


  if WinToDOS then
//    Result := {WideStringToStringEx(}StringToWideStringEx(st, 866){, 866});
    MultiByteToWideChar(866{CP_ACP}{CP_MACCP}{CP_OEMCP}, 0, PChar(s), -1, arrW, MAX_PATH);
  Result := '';
  for i := 1 to Length(arrW) do
    Result := Result + IntToStr(Ord(arrW[i])) + '_';
//  Result := arrW;
  exit;
end;

function EBCDICCode(s: string): string;
var
  i, intLen: Integer;
begin
  intLen := Length(s);
  SetLength(Result, intLen);
  for i := 1 to intLen do
    Result[i] := Chr(ASCII2EBCDIC(s[i]))
end;

function PadLSpace(strStr: string; intLen: Integer): string;
begin
  FmtStr(Result, '%*s', [intLen, strStr]);
end;

function PadRSpace(strStr: string; intLen: Integer): string;
begin
  FmtStr(Result, '%-*s', [intLen, strStr]);
end;

{ TSMExportStyle }
procedure TSMExportStyle.Assign(Source: TPersistent);
var
  SMEStyle: TSMExportStyle;
begin
  if Source is TSMExportStyle then
  begin
    SMEStyle := TSMExportStyle(Source);
    Style := SMEStyle.Style;
    OddColor := SMEStyle.OddColor;
    EvenColor := SMEStyle.EvenColor;
  end
  else
    inherited Assign(Source);
end;

procedure TSMExportStyle.SetStyle(Value: TSMEStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;

    OddColor := arrStyleColor[FStyle][False];
    EvenColor := arrStyleColor[FStyle][True];
  end;
end;

{ TSMEStatistic }
procedure TSMEStatistic.Assign(Source: TPersistent);
var
  Statistic: TSMEStatistic;
begin
  if (Source is TSMEStatistic) then
  begin
    Statistic := TSMEStatistic(Source);

    TotalCount := Statistic.TotalCount;
    TotalErrors := Statistic.TotalErrors;

    UpdateStep := Statistic.UpdateStep;

    Result := Statistic.Result;
  end
  else
    inherited Assign(Source);
end;

{ TSMEDataFormats }
procedure TSMEDataFormats.Assign(Source: TPersistent);
var
  Formats: TSMEDataFormats;
begin
  if (Source is TSMEDataFormats) then
  begin
    Formats := TSMEDataFormats(Source);

    DateOrder := Formats.DateOrder;
    DateSeparator := Formats.DateSeparator;
    TimeSeparator := Formats.TimeSeparator;
    FourDigitYear := Formats.FourDigitYear;
    LeadingZerosInDate := Formats.LeadingZerosInDate;
    ThousandSeparator := Formats.ThousandSeparator;
    DecimalSeparator := Formats.DecimalSeparator;
    CurrencyString := Formats.CurrencyString;

    CustomDateTimeFormat := Formats.CustomDateTimeFormat;

    BooleanTrue := Formats.BooleanTrue;
    BooleanFalse := Formats.BooleanFalse;

    UseRegionalSettings := Formats.UseRegionalSettings;
  end
  else
    inherited Assign(Source);
end;

procedure TSMEDataFormats.LoadRegionalSettings;
var
  s: string;
  c1, c2, c3: Char;
  i: Integer;
begin
  DateSeparator := SysUtils.DateSeparator;
  TimeSeparator := SysUtils.TimeSeparator;
  DateOrder := doMDY;
  s := UpperCase(SysUtils.ShortDateFormat);
  if (s <> '') then
  begin
    {detect date order}
    c1 := s[1];
    i := Pos(DateSeparator, s);
    if (i > 0) then
    begin
      Delete(s, 1, i);
      c2 := s[1];

      i := Pos(DateSeparator, s);
      if (i > 0) then
      begin
        Delete(s, 1, i);
        c3 := s[1];

        if (c1 = 'D') and (c2 = 'M') and (c3 = 'Y') then
          DateOrder := doDMY
        else
        if (c1 = 'D') and (c2 = 'Y') and (c3 = 'M') then
          DateOrder := doDYM
        else
        if (c1 = 'M') and (c2 = 'D') and (c3 = 'Y') then
          DateOrder := doMDY
        else
        if (c1 = 'M') and (c2 = 'Y') and (c3 = 'D') then
          DateOrder := doMYD
        else
        if (c1 = 'Y') and (c2 = 'M') and (c3 = 'D') then
          DateOrder := doYMD
        else
        if (c1 = 'Y') and (c2 = 'D') and (c3 = 'M') then
          DateOrder := doYDM
      end;
    end;
  end;

  DecimalSeparator := SysUtils.DecimalSeparator;
  ThousandSeparator := SysUtils.ThousandSeparator;
  CurrencyString := SysUtils.CurrencyString;

  s := UpperCase(SysUtils.ShortDateFormat);
  FourDigitYear := (Pos('YYYY', s) > 0);
  LeadingZerosInDate := (Pos('DD', s) > 0);
  CustomDateTimeFormat := '';
end;

function TSMEDataFormats.GetDTSeparator(Value: Char): string;
begin
  if (Value = #0) then
    Result := ''
  else
    Result := Value
end;

function TSMEDataFormats.GetDateFormat: string;

    function GetDTPrefix(const Value: string): string;
    begin
      if LeadingZerosInDate then
        Result := Value + Value
      else
        Result := Value;
      if (Value = 'Y') and FourDigitYear then
        while Length(Result) < 4 do
          Result := Result + Value
    end;

begin
  if CustomDateTimeFormat <> '' then
  begin
    Result := UpperCase(CustomDateTimeFormat);
    exit;
  end;

  case DateOrder of
    doMDY: Result := GetDTPrefix('M') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('D') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('Y');
    doDMY: Result := GetDTPrefix('D') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('M') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('Y');
    doYMD: Result := GetDTPrefix('Y') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('M') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('D');
    doYDM: Result := GetDTPrefix('Y') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('D') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('M');
    doDYM: Result := GetDTPrefix('D') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('Y') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('M');
    doMYD: Result := GetDTPrefix('M') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('Y') + GetDTSeparator(DateSeparator) +
                     GetDTPrefix('D');
  end;
end;

function TSMEDataFormats.GetTimeFormat: string;
begin
  Result := 'HH' + GetDTSeparator(TimeSeparator) +
            'NN' + GetDTSeparator(TimeSeparator) +
            'SS';
end;

function TSMEDataFormats.GetDateTimeFormat: string;
begin
  Result := GetDateFormat + ' ' + GetTimeFormat
end;

{ TSMESendTo }
procedure TSMESendTo.Assign(Source: TPersistent);
var
  SendTo: TSMESendTo;
begin
  if (Source is TSMESendTo) then
  begin
    SendTo := TSMESendTo(Source);

    EMailRecipient := SendTo.EMailRecipient;
    EMailCC := SendTo.EMailCC;
    EMailBCC := SendTo.EMailBCC;
    EMailSubject := SendTo.EMailSubject;
    EMailBody := SendTo.EMailBody;
    EMailOpenBeforeSend := SendTo.EMailOpenBeforeSend;
  end
  else
    inherited Assign(Source);
end;


{ TSMECustomBaseComponent }
constructor TSMECustomBaseComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHeader := TStringList.Create;
  FFooter := TStringList.Create;

  FSendTo := TSMESendTo.Create;
  FSendTo.EMailOpenBeforeSend := True;

  FExportStyle := TSMExportStyle.Create;

  FPageSetup := TSMEPageSetup.Create;
  FPageSetup.Measure := emPoint;
  FPageSetup.UseDefault := True;
  FPageSetup.TopMargin := 0;
  FPageSetup.BottomMargin := 0;
  FPageSetup.LeftMargin := 0;
  FPageSetup.RightMargin := 0;
  FPageSetup.TableWidth := 0;
  FPageSetup.Orientation := emDefault;

  FStatistic := TSMEStatistic.Create;
  FStatistic.UpdateStep := 1;

  FRowsPerFile := 0;
  FKeyGenerator := GeneratorVer;
  FAnimatedStatus := True;
  FOptions := [soShowMessage, soWaitCursor, soDisableControls, soColLines, soRowLines];
  FStatistic.TotalCount := 0;
  FExportIfEmpty := True;

  FTextQualifier := #0;
  FSeparator := #9;
  FRecordSeparator := #13#10;

  FDataFormats := TSMEDataFormats.Create;
  FDataFormats.LoadRegionalSettings;

  FDataFormats.FBooleanTrue := strTrue;
  FDataFormats.FBooleanFalse := strFalse;
end;

destructor TSMECustomBaseComponent.Destroy;
begin
  if Assigned(FStatistic) then
  begin
    FStatistic.Free;
    FStatistic := nil;
  end;

  FPageSetup.Free;
  FDataFormats.Free;
  FExportStyle.Free;

  FHeader.Free;
  FFooter.Free;

  FSendTo.Free;

  inherited;
end;

procedure TSMECustomBaseComponent.SetAbout(const Value: TSMEAbout);
begin
  FAbout := ''
end;

procedure TSMECustomBaseComponent.SetPageSetup(Value: TSMEPageSetup);
begin
  FPageSetup.Assign(Value);
end;

procedure TSMECustomBaseComponent.SetExportStyle(Value: TSMExportStyle);
begin
  FExportStyle.Assign(Value);
end;

procedure TSMECustomBaseComponent.SetSendTo(Value: TSMESendTo);
begin
  FSendTo.Assign(Value)
end;

procedure TSMECustomBaseComponent.SetTableType(Value: TTableTypeExport);
begin
  FTableType := Value;
end;

function TSMECustomBaseComponent.GetLanguageString(id: Integer): string;
begin
  case id of
     0: Result := strPropertyError; // 'Property %s wasn''t specified.'
     1: Result := strIsNotDBGrid; // 'Assigned DBGrid (%s) have the not supported type'
     2: Result := strExportFinished; // 'The data export to the %s file successfuly is finished.'
     3: Result := strExportError; // 'Error of data export.'
     4: Result := strExportOk; // 'Data export is successfully completed.'
     5: Result := strFileCreateError; // 'It is impossible to create a file %s.'
     6: Result := strHTMLTitle; // 'The Exported data by SMExport generator'
     7: Result := strFilePrompt; // 'File %s already exists. Do you want to replace it?'

     8: Result := strExporting; // 'Exporting...'
     9: Result := strLoadError; // 'Can''t create a process dialog'
    10: Result := strSetupDlgCaption; // 'Setup of the data export'
    11: Result := strBtnOk; // '&OK'
    12: Result := strBtnCancel; // '&Cancel'
    13: Result := strBtnBack; // '< &Back'
    14: Result := strBtnNext; // '&Next >'
    15: Result := strExecute; // 'Execute'
    16: Result := strExportCanceled; // 'Data exporting was canceled by user.'
    17: Result := strSMEWizardCaption; // 'Export Wizard'
    18: Result := strStepDisplay; // 'Step %d of %d'
    19: Result := strWizardAbort; // 'Wizard is not complete. If you quit the Wizard now, the new export will not be created. Abort wizard?'

    20: Result := strLblStep1; // 'This wizard allows you to specify details on how should export your data. Which export format would you like?'
    21: Result := strLblStep2; // ''
    22: Result := strLblStep3; // 'What delimiter separates your fields? Select the appropriate delimiter.'
    23: Result := strLblStep5; // 'You can define a custom properties of exported columns.'
    24: Result := strLblStep6; // 'You can add a custom header and footer into generated file.'
    25: Result := strLblStep8; // 'That''s all information the wizard needs to export your data.'

    26: Result := strHeader; // 'Header'
    27: Result := strFooter; // 'Footer'

    28: Result := strTableType; // ' Table type '
    29: Result := strFormat1; // 'Paradox file (*.db)'
    30: Result := strFormat2; // 'DBase file (*.dbf)'
    31: Result := strFormat3; // 'Text file (*.txt)'
    32: Result := strFormat4; // 'HTML file (*.htm)'
    33: Result := strFormat5; // 'Excel spreadsheet (*.xls)'
    34: Result := strFormat6; // 'Excel file (*.xls)'
    35: Result := strFormat7; // 'Word file (*.doc)';
    36: Result := strFormat8; // 'SYLK (Symbolic Link) (*.slk)';
    37: Result := strFormat9; // 'DIF (Data Interchange Format) (*.dif)';
    38: Result := strFormatA; // 'Lotus 1-2-3 file (*.wk1)';
    39: Result := strFormatB; // 'QuattroPro file (*.wq1)';
    40: Result := strFormatC; // 'SQL script file (*.sql)';
    41: Result := strFormatD; // 'XML file (*.xml)';
    42: Result := strFormatE; // 'MS Access database (*.mdb)';
    43: Result := strFormatF; // 'MS Windows clipboard';
    44: Result := strFormatG; // 'Rich Text format (*.rtf)';
    45: Result := strFormatH; // 'SPSS format (*.sav)';
    46: Result := strFormatI; // 'Adobe Acrobat Document (*.pdf)';
    47: Result := strFormatJ; // 'LDAP/Lightweight Data Interchange Format (*.ldif)';
    48: Result := strFormatK; // 'ADO connection'
    
    50: Result := strFileOrigin; // 'File Origin:';
    51: Result := strSelectedOnly; // 'selected records only';
    52: Result := strAddCaption; // 'include a column titles';
    53: Result := strAddTitle; // 'Include Field Names on First Row';
    54: Result := strBlankIfZero; // 'blank if zero';
    55: Result := strActionAfterExport; // 'Action after exporting';
    56: Result := strAENone; // 'none';
    57: Result := strAEOpenView; // 'open for file view';
    58: Result := strAEEMail; // 'e-mail with file attachment';

    59: Result := strSeparatorType; // ' Field delimiter ';
    60: Result := strDelimiter1; // 'Tab';
    61: Result := strDelimiter2; // 'Semicolon';
    62: Result := strDelimiter3; // 'Comma';
    63: Result := strDelimiter4; // 'Space';
    64: Result := strDelimiter5; // 'Other symbol:';
    65: Result := strFixed; // 'fixed length of the columns';
    66: Result := strText1; // '&Delimited - Characters such as comma or tab separate each field';
    67: Result := strText2; // 'Fixed &Width - Fields are aligned in columns with spaces between each field';

    68: Result := strFileName; // 'Export to File:';
    69: Result := strSaveDlgTitle; // 'Select a file for data exporting';
    70: Result := strAllFiles; // 'All files (*.*)|*.*';
    71: Result := strTableName; // 'Table name (MS Access):';

    72: Result := SgbTitle; // ' Title ';
    73: Result := SgbData; // ' Data ';
    74: Result := STitleCaption; // 'Caption:';
    75: Result := STitleAlignment; // 'Alignment:';
    76: Result := STitleColor; // 'Background:';
    77: Result := STitleFont; // 'Font:';
    78: Result := SWidth; // 'Width:';
    79: Result := SWidthFix; // 'characters';
    80: Result := SAlignLeft; // 'left';
    81: Result := SAlignRight; // 'right';
    82: Result := SAlignCenter; // 'center';

    83: Result := strESpecs;// Specifications...

    84: Result := strERecordSeparator;// Record separator
    85: Result := strETextQualifier;// Text qualifier
    86: Result := strENone;// none

    87: Result := strERowsPerFile; //Records per each file
    88: Result := strLblStep7; // You can select desired layout of exported data

    89: Result := strLayout; //' Layout ';
    90: Result := strColumnar; // 'columnar';
    91: Result := strReversedColumnar; // 'reversed columnar';
    92: Result := strTabularForm; // 'tabular form';
    93: Result := strColorStyle; // 'color style'

    94: Result := strSelectAll; // 'Select All'
    95: Result := strUnSelectAll; // 'Unselect All'
    96: Result := strRevertSelection; // 'Revert selection'

    97: Result := strAddBlankRow; // 'add a blank row after field names';
    98: Result := strPreviewLayout; //'Preview'

    99: Result := strDateNumFormat;
    100: Result := strDateOrder;
    101: Result := strMDY;
    102: Result := strDMY;
    103: Result := strYMD;
    104: Result := strYDM;
    105: Result := strDYM;
    106: Result := strMYD;
    107: Result := strDateDelimiter;
    108: Result := strTimeDelimiter;
    109: Result := strFourDigitYears;
    110: Result := strLeadingZerosInDate;
    111: Result := strDecimalSymbol;
    112: Result := strThousandSymbol;
    113: Result := strCurrencyString;
    114: Result := strLogicalValues;
    115: Result := strLblStep4; // 'That''s all information the wizard needs to export your data.'
    116: Result := strMergeFile;

    117: Result := strStepHeader1; //'File Format';
    118: Result := strStepHeader2; //'Data Origin';
    119: Result := strStepHeader3; // 'Text Settings';
    120: Result := strStepHeader4; // 'Data Formats';
    121: Result := strStepHeader5; // 'Columns';
    122: Result := strStepHeader6; // 'Header and Footer';
    123: Result := strStepHeader7; // 'Layout';
    124: Result := strStepHeader8; // 'File Name';
    125: Result := strExportColorsFonts; // 'export colors/fonts';
    126: Result := strExportDataOnly; //'export data only';

    127: Result := strExportOriginalValueTypes; //'export the original value types';
    128: Result := strSheetName; //'Sheet name:';

    129: Result := strOrientation; //' Orientation '
    130: Result := strDefault; //'Default'
    131: Result := strPortrait; //'Portrait'
    132: Result := strLandscape; //'Landscape'

    133: Result := strMargins; //' Default &Margins '
    134: Result := strLeft; //'Left'
    135: Result := strTop; //'Top'
    136: Result := strRight; //'Right'
    137: Result := strBottom; //'Bottom'

    138: Result := strStepHeader6A; //'Page Setup'
    139: Result := strLblStep6A; // 'Customize settings for page'

    140: Result := strInsertSysVar;
    141: Result := strSVRowNo;
    142: Result := strSVColNo;
    143: Result := strSVRecCount;
    144: Result := strSVErrCount;
    145: Result := strSVSheetName;
  else
    Result := ''
  end;

  if Assigned(FOnGetLanguageString) then
    FOnGetLanguageString(Self, id, Result);
end;

procedure TSMECustomBaseComponent.Assign(Source: TPersistent);
var
  sme: TSMECustomBaseComponent;
begin
  if Source is TSMECustomBaseComponent then
  begin
    sme := TSMECustomBaseComponent(Source);

    Header.Assign(sme.Header);
    Footer.Assign(sme.Footer);
    RowsPerFile := sme.RowsPerFile;
    TableType := sme.TableType;

    AnimatedStatus := sme.AnimatedStatus;
    DataFormats.Assign(sme.DataFormats);
    KeyGenerator := sme.KeyGenerator;
    SelectedRecord := sme.SelectedRecord;
    BlankIfZero := sme.BlankIfZero;
    Options := sme.Options;
    RightToLeft := sme.RightToLeft;
    SendTo.Assign(sme.SendTo);
    Statistic.Assign(sme.Statistic);

    OnGetLanguageString := sme.OnGetLanguageString;
    OnBeforeRecord := sme.OnBeforeRecord;
    OnAfterRecord := sme.OnAfterRecord;

    OnBeforeExecute := sme.OnBeforeExecute;
    OnAfterExecute := sme.OnAfterExecute;
    OnGetFileName := sme.OnGetFileName;
    OnProgress := sme.OnProgress;

    AddTitle := sme.AddTitle;
    CharacterSet := sme.CharacterSet;
    OnGetCellParams := sme.OnGetCellParams;
    ExportStyle.Assign(sme.ExportStyle);
    TableName := sme.TableName;
    TextQualifier := sme.TextQualifier;
    Separator := sme.Separator;
    RecordSeparator := sme.RecordSeparator;
    Fixed := sme.Fixed;

    PageSetup.Assign(sme.PageSetup);
  end
  else
    inherited Assign(Source);
end;

procedure TSMECustomBaseComponent.AboutSME;
begin
  AboutSMExport;
end;

function TSMECustomBaseComponent.GetHeader: TStrings;
begin
  Result := FHeader;
end;

procedure TSMECustomBaseComponent.SetHeader(Value: TStrings);
begin
  FHeader.Assign(Value);
end;

function TSMECustomBaseComponent.GetFooter: TStrings;
begin
  Result := FFooter;
end;

procedure TSMECustomBaseComponent.SetFooter(Value: TStrings);
begin
  FFooter.Assign(Value);
end;

function TSMECustomBaseComponent.AddZeros(const Source: string; Len: Integer; IsLeading: Boolean): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to (Len-Length(Source)) do
    if IsLeading then
      Result := '0' + Result
    else
      Result := Result + '0';
end;

function TSMECustomBaseComponent.GetFieldStr(Field: TField): string;
begin
  if not Assigned(Field) then
  begin
    Result := '';
    exit;
  end;

  if (soFieldMask in Options) then
  begin
    if (Field.DataType = ftBoolean) then
    begin
      if Field.AsBoolean then
        Result := DataFormats.BooleanTrue
      else
        Result := DataFormats.BooleanFalse
    end
    else
      Result := Field.DisplayText
  end
  else
    Result := Field.AsString; 
  if BlankIfZero then
    case Field.DataType of
      ftSmallint,
      ftInteger,
      ftWord,
      ftAutoInc,
      ftFloat,
      ftCurrency,
  {$IFDEF SMForDelphi4}
        ftLargeInt,
  {$ENDIF}
      ftBCD: with TNumericField(Field) do
               if AsFloat = 0 then
                 Result := '';
      ftDate,
      ftTime,
      ftDateTime: with TDateTimeField(Field) do
                    if AsDateTime = 0 then
                      Result := '';
    else
      if Field.IsNull then
        Result := ''
    end;
end;

procedure TSMECustomBaseComponent.AssignStyle(ARow: LongInt; var color: TColor);
begin
  if (ExportStyle.Style <> esNormal) then
  begin
    if (ARow mod 2 = 1) then
      color := ExportStyle.OddColor
    else
      color := ExportStyle.EvenColor;
  end;
end;

procedure TSMECustomBaseComponent.Prepare;
begin
end;

function TSMECustomBaseComponent.GetFieldType(Field: TField; BlankIfZero: Boolean): TCellType;
begin
  if Assigned(Field) then
    Result := GetValueType(Field.DataType, Field.Value, BlankIfZero)
  else
    Result := ctString;
end;

function TSMECustomBaseComponent.GetSysValue(const AName: string): Variant;
begin
  if (CompareText(AName, 'ROWNO') = 0) then
  begin
    Result := Statistic.CurrentRow+1 - Header.Count;
    if AddTitle then
    begin
      Result := Result-1;
      if (soBlankRowAfterCaptions in Options) then
        Result := Result-1;
    end;
  end
  else
  if (CompareText(AName, 'COLNO') = 0) then
    Result := Statistic.CurrentCol
  else
  if (CompareText(AName, 'COUNT') = 0) then
    Result := Statistic.TotalCount
  else
  if (CompareText(AName, 'ERRCOUNT') = 0) then
    Result := Statistic.TotalErrors
  else
  if (CompareText(AName, 'GENERATOR') = 0) then
    Result := KeyGenerator
end;

function TSMECustomBaseComponent.MappingVersion: Integer;
begin
  Result := 0
end;

function TSMECustomBaseComponent.SendMail(const AFileName, body: string): Integer;
var
  lstRecipients: TStrings;

  procedure SplitRecipients(EMail, Prefix: string);
  var
    intPos: Integer;
  begin
    EMail := Trim(EMail);

    while (EMail <> '') do
    begin
      intPos := Pos(';', EMail);
      if (intPos = 0) then
        intPos := Length(EMail)+1;
      lstRecipients.Add(Prefix + Copy(EMail, 1, intPos-1));
      Delete(EMail, 1, intPos)
    end;
  end;

const
  RECIP_MAX  = MaxInt div SizeOf(TMapiRecipDesc);
type
  TRecipAccessArray = array [0..(RECIP_MAX - 1)] of TMapiRecipDesc;
  TlpRecipArray     = ^TRecipAccessArray;

  TszRecipName   = array[0..256] of Char;
  TlpszRecipName = ^TszRecipName;

var
  Message: TMapiMessage;
  FileAttach: TMapiFileDesc;
//  Recipient: TMapiRecipDesc;
  lpRecipArray: TlpRecipArray;

  SM: TFNMapiSendMail;
  MAPIModule: hModule;
  Flags: dWord;
  i: Integer;
  s: string;
begin
  FillChar(Message, SizeOf(Message), 0);
  with Message do
  begin
    if (FSendTo.EMailSubject = '') then
      lpszSubject := PChar(KeyGenerator)
    else
      lpszSubject := PChar(FSendTo.EMailSubject);
    if (body = '') then
    begin
      FillChar(FileAttach, SizeOf(FileAttach), 0);
      FileAttach.nPosition := Cardinal($FFFFFFFF); //ULONG(-1);
      FileAttach.lpszPathName := PChar(AFileName);

      nFileCount := 1;
      lpFiles := @FileAttach;

      lpszNoteText := PChar(FSendTo.EMailBody);
    end
    else
    begin
      lpszNoteText := PChar(FSendTo.EMailBody + #13#10 + body);
      nFileCount := 0;
      lpFiles := nil;
    end;
    lpRecips := nil;
  end;

  {fill recipient(s)}
  lstRecipients := TStringList.Create;
  try
    SplitRecipients(FSendTo.EMailRecipient, '');
  except
  end;
  try
    SplitRecipients(FSendTo.EMailCC, 'CC:');
  except
  end;
  try
    SplitRecipients(FSendTo.EMailBCC, 'BCC:');
  except
  end;

  lpRecipArray := nil;

  if (lstRecipients.Count > 0) then
  begin
    lpRecipArray := TlpRecipArray(StrAlloc(lstRecipients.Count*SizeOf(TMapiRecipDesc)));
    FillChar(lpRecipArray^, StrBufSize(PChar(lpRecipArray)), 0);

    for i := 0 to lstRecipients.Count-1 do
    begin
      s := lstRecipients[i];
      if (UpperCase(Copy(s, 1, 3)) = 'CC:') then
      begin
        lpRecipArray^[i].ulRecipClass := MAPI_CC;
        Delete(s, 1, 3);
      end
      else
      if (UpperCase(Copy(s, 1, 4)) = 'BCC:') then
      begin
        lpRecipArray^[i].ulRecipClass := MAPI_BCC;
        Delete(s, 1, 4);
      end
      else
        lpRecipArray^[i].ulRecipClass := MAPI_TO;
      lpRecipArray^[i].lpszName := StrCopy(new(TlpszRecipName)^, PChar(s));
      lpRecipArray^[i].lpszAddress := StrCopy(new(TlpszRecipName)^, PChar(s));
    end;

    Message.nRecipCount := lstRecipients.Count;
    Message.lpRecips := @lpRecipArray^;

{    FillChar(Recipient, SizeOf(Recipient), 0);
    Recipient.lpszName := PChar(FSendTo.EMailRecipient);
    Recipient.lpszAddress := PChar(FSendTo.EMailRecipient);
    Recipient.ulRecipClass := MAPI_TO;

    Message.nRecipCount := 1;
    Message.lpRecips := @Recipient;
}  end;

  MAPIModule := LoadLibrary(PChar(MAPIDLL));
  if MAPIModule = 0 then
    Result := -1
  else
    try
      @SM := GetProcAddress(MAPIModule, 'MAPISendMail');
      if @SM <> nil then
      begin
        if FSendTo.EMailOpenBeforeSend or
           (FSendTo.EMailRecipient = '') then
          Flags := MAPI_DIALOG
        else
          Flags := 0;
        Result := SM(0, Application.Handle{0}, Message, Flags or MAPI_LOGON_UI {or MAPI_NEW_SESSION}, 0);

//  Result := MapiSendMail(0, 0, Message, MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
{               MError := MapiSendMail(0, Application.Handle, MapiMessage,
                                      MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
//               MError := MAPISendDocuments(0, ';', PChar(FileName), nil, 0);
}
      end
      else
        Result := 1;
    finally
      FreeLibrary(MAPIModule);
    end;

  {to free the memory}
  if Assigned(lstRecipients) and (lstRecipients.Count > 0) then
  begin
    for i := 0 to Message.nRecipCount-1 do
    begin
      if Assigned(lpRecipArray^[i].lpszName) then
        Dispose(lpRecipArray^[i].lpszName);

      if Assigned(lpRecipArray^[i].lpszAddress) then
        Dispose(lpRecipArray^[i].lpszAddress);
    end;
    StrDispose(PChar(lpRecipArray));
    lstRecipients.Free
  end;

  if Result <> 0 then
    if (soShowMessage in Options) then
      MessageDlg('Error sending mail (' + IntToStr(Result) + ')', mtError, [mbOK], 0);
end;

function TSMECustomBaseComponent.Extension: string;
begin
  Result := '.EXP'
end;

function TSMECustomBaseComponent.GetTitleRowCount: Integer; 
begin
  if AddTitle then
  begin
    if (soBlankRowAfterCaptions in Options) then
      Result := 3
    else
      Result := 2;
  end
  else
    Result := 1;
  Inc(Result, Header.Count)
end;

function TSMECustomBaseComponent.GetExportedRecordCount: Integer;
begin
  Result := Statistic.TotalCount
end;

function TSMECustomBaseComponent.GetExportResult: TSMEResult;
begin
  Result := Statistic.Result
end;

function TSMECustomBaseComponent.Translate(const s: string; IsEncrypt: Boolean): string;
var
  i: Integer;
begin
  Result := '';
  if IsEncrypt then
    for i := 1 to Length(s) do
      Result := Result + IntToHex(Ord(s[i]), 2)
  else
    for i := 1 to (Length(s) div 2) do
      Result := Result + Chr(StrToInt('$' + s[2*i-1] + s[2*i]))
end;

{ TSMExportBaseComponent }
constructor TSMExportBaseComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

//  FDBGrid := nil;
//  FDataSet := nil;
//  FDataEngine := nil;
  FColumnSource := csDBGrid;
  FIsWWDBGrid := False;
  FIsTMSDBGrid := False;

  FColumns := TSMEColumns.Create(Self);
  FBands := TSMEColumnBands.Create(Self);

  if TableType = teClipboard then
    FileName := ''
  else
    FileName := ExtractFilePath(Application.ExeName) + 'SMExport' + Extension;
  FActionAfterExport := aeNone;
  FLayout := elColumnar;
end;

destructor TSMExportBaseComponent.Destroy;
begin
  FColumns.Free;
  FBands.Free;

  inherited;
end;

class function TSMExportBaseComponent.GetDefExt(intIndex: Integer): string; 
begin
  case intIndex of
    0: Result := '.DB';
    1: Result := '.DBF';
    2: Result := '.TXT';
    3: Result := '.HTM';
    4: Result := '.XLS';
    5: Result := '.XLS';
    6: Result := '.DOC';
    7: Result := '.SLK';
    8: Result := '.DIF';
    9: Result := '.WK1';
   10: Result := '.WQ1';
   11: Result := '.SQL';
   12: Result := '.XML';
   13: Result := '.MDB';
   15: Result := '.RTF';
   16: Result := '.SAV';
   17: Result := '.PDF';
   18: Result := '.LDIF';
  else
    Result := ''
  end;
end;

procedure TSMExportBaseComponent.SetTableType(Value: TTableTypeExport);
begin
  if (FileName <> '') then
    FileName := ChangeFileExt(FileName, GetDefExt(Ord(Value)));

  inherited SetTableType(Value);
end;

function TSMExportBaseComponent.GetColumns: TSMEColumns;
begin
  Result := FColumns
end;

procedure TSMExportBaseComponent.SetColumns(Value: TSMEColumns);
begin
  FColumns.Assign(Value)
end;

function TSMExportBaseComponent.GetBands: TSMEColumnBands;
begin
  Result := FBands
end;

procedure TSMExportBaseComponent.SetBands(Value: TSMEColumnBands);
begin
  FBands.Assign(Value)
end;

function TSMExportBaseComponent.GetDBGrid: TCustomControl {TCustomGrid};
begin
  Result := FDBGrid;
end;

procedure TSMExportBaseComponent.SetDBGrid(AValue: TCustomControl{TCustomGrid});
begin
  if (AValue = nil) or PropIsDBGrid(AValue, FIsWWDBGrid, FIsTMSDBGrid) then
  begin
    FDBGrid := AValue;
    if AValue <> nil then
      AValue.FreeNotification(Self);
  end
  else
    raise Exception.Create(Format(GetLanguageString(1), [AValue.Name]));
end;

function TSMExportBaseComponent.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TSMExportBaseComponent.SetDataSet(AValue: TDataSet);
begin
  FDataSet := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

function TSMExportBaseComponent.GetDataEngine: TSMECustomDataEngine;
begin
  Result := FDataEngine;
end;

procedure TSMExportBaseComponent.SetDataEngine(AValue: TSMECustomDataEngine);
begin
  FDataEngine := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

function TSMExportBaseComponent.GetDS: TDataSet;
begin
  if Assigned(SourceDataEngine) and
     (SourceDataEngine is TSMECustomDBDataEngine) then
    Result := TSMECustomDBDataEngine(SourceDataEngine).Dataset
  else
  if (ColumnSource = csDataSet) then
    Result := DataSet
  else
    Result := GetPropAsDataSet(DBGrid);
end;

type
  TSMEHackGrid = class(TCustomDBGrid);
  
function TSMExportBaseComponent.GetNextSelected(i: Integer): TBookmark;
begin
  if Assigned(OnGetNextSelected) then
    OnGetNextSelected(Self, i, Result)
  else
  begin
    if (not IsWWDBGrid) and (not IsTMSDBGrid) and (DBGrid is TCustomDBGrid) then
      Result := Pointer(TSMEHackGrid(FDBGrid).SelectedRows[i])
    else
      Result := nil
  end;
end;

function TSMExportBaseComponent.GetSelectedCount: Integer;
begin
  if Assigned(OnGetSelectedCount) then
    OnGetSelectedCount(Self, Result)
  else
  begin
    if (not IsWWDBGrid) and (not IsTMSDBGrid) and (DBGrid is TCustomDBGrid) then
      Result := TSMEHackGrid(FDBGrid).SelectedRows.Count
    else
      Result := 0
//  Result := GetPropAsSelectedList(DBGrid).Count;
  end;
end;

function TSMExportBaseComponent.IsMemoColumnExist: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Columns.Count-1 do
    if Columns[i].Visible and
       (Columns[i].DataType = ctMemo) then
    begin
      Result := True;
      break;
    end
end;


{limit of processed record count in trial version}
function TSMExportBaseComponent.IsTrialCount(i: Integer): Boolean;
begin
  Result := True
end;

procedure TSMExportBaseComponent.AfterExport;
begin
  case ActionAfterExport of
    aeOpenView: ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_SHOWNORMAL);
    aeEMail: SendMail(FileName, '');
  else
  end;
end;

procedure TSMExportBaseComponent.CreateSourceDataEngine;
begin
  case ColumnSource of
    csDBGrid: begin
                if not Assigned(DBGrid) then
                  raise Exception.Create(Format(GetLanguageString(0), ['DBGrid']));

                if IsWWDBGrid then
                begin
                  SourceDataEngine := TSMEwwDBGridDataEngine.Create(Self);
                  with TSMEwwDBGridDataEngine(SourceDataEngine) do
                    DBGrid := Self.DBGrid;
                end
                else
                if IsTMSDBGrid then
                begin
                  SourceDataEngine := TSMEDBAdvStringGridDataEngine.Create(Self);
                  with TSMEDBAdvStringGridDataEngine(SourceDataEngine) do
                    DBGrid := (Self.DBGrid as TCustomDBGrid);
                end
                else
                begin
                  SourceDataEngine := TSMEDBGridDataEngine.Create(Self);
                  with TSMEDBGridDataEngine(SourceDataEngine) do
                  begin
                    DBGrid := (Self.DBGrid as TCustomDBGrid);
                  end
                end
              end;
    csDataSet: begin
                 if not Assigned(Dataset) then
                   raise Exception.Create(Format(GetLanguageString(0), ['Dataset']));

                 SourceDataEngine := TSMEDatasetDataEngine.Create(Self);
                 TSMEDatasetDataEngine(SourceDataEngine).Dataset := Dataset;
               end;
  else
    if not Assigned(DataEngine) then
      raise Exception.Create(Format(GetLanguageString(0), ['DataEngine']));
    SourceDataEngine := FDataEngine;
  end;
  SourceDataEngine.SelectedRecords := SelectedRecord;
end;

procedure TSMExportBaseComponent.DestroySourceDataEngine;
begin
  case ColumnSource of
    csDBGrid: SourceDataEngine.Free;
    csDataSet: SourceDataEngine.Free;
  else
  end;
  SourceDataEngine := nil;
end;

procedure TSMExportBaseComponent.Assign(Source: TPersistent);
var
  sme: TSMExportBaseComponent;
begin
  inherited Assign(Source);

  if (Source is TSMExportBaseComponent) then
  begin
    sme := TSMExportBaseComponent(Source);

    ActionAfterExport := sme.ActionAfterExport;
    FileName := sme.FileName;
    Columns.Assign(sme.Columns);
    Bands.Assign(sme.Bands);
    DBGrid := sme.DBGrid;
    DataSet := sme.DataSet;
    DataEngine := sme.DataEngine;
    ColumnSource := sme.ColumnSource;

    OnGetNextSelected := sme.OnGetNextSelected;
    OnGetSelectedCount := sme.OnGetSelectedCount;
    OnBeforeLoadSpecification := sme.OnBeforeLoadSpecification;
    OnAfterLoadSpecification := sme.OnAfterLoadSpecification;
    OnBeforeSaveSpecification := sme.OnBeforeSaveSpecification;
    OnAfterSaveSpecification := sme.OnAfterSaveSpecification;
    Layout := sme.Layout;
    ExportIfEmpty := sme.ExportIfEmpty;
  end
end;

function TSMExportBaseComponent.CreateProgressDlg(strCaption, strMessage, strBtnCaption: string; MinValue, MaxValue, Progress: Longint): Boolean;
begin
  Result := False;

  try
    DestroyProgressDlg;
    frmSMEProcess := TfrmSMEProcess.Create(Application);

    Application.ProcessMessages;
    with frmSMEProcess do
    begin
      Caption := strCaption;
      lblStatus.Caption := strMessage;
      btnCancel.Caption := strBtnCaption;
      ProgressBar.Min := MinValue;
      ProgressBar.Max := MaxValue;
      ProgressBar.Position := Progress;
      ProgressBar.Visible := not ((MinValue = 0) and (MaxValue = 0));
      {$IFDEF LINUX}
      {$ELSE}
      Animate.Active := True;
      {$ENDIF}

      Show;
      Application.ProcessMessages;
    end;
    Result := True;
  except
    MessageDlg(strLoadError, mtError, [mbOK], 0 );
    try
      frmSMEProcess.Free;
    finally
      frmSMEProcess := nil;
    end;
  end;
end;

function TSMExportBaseComponent.DestroyProgressDlg: Boolean;
begin
  Result := False;

  if Assigned(frmSMEProcess) then
    try
      {$IFDEF LINUX}
      {$ELSE}
      frmSMEProcess.Animate.Active := False;
      {$ENDIF}
      frmSMEProcess.Free;
      frmSMEProcess := nil;
      Application.ProcessMessages;
      Result := True;
    except
    end;
end;

function TSMExportBaseComponent.UpdateProgressDlg(strMessage: string; Progress: Longint): Boolean;
begin
  if (Statistic.UpdateStep > 0) and not (Progress mod Statistic.UpdateStep = 0) then
  begin
    Result := True;
    exit;
  end;

  Result := False;

  if Assigned(frmSMEProcess) then
    with frmSMEProcess do
    begin
      lblStatus.Caption := strMessage + ' (' + IntToStr(Progress) + '/' + IntToStr(ProgressBar.Max) + ')';
      ProgressBar.Position := Progress;
      Update;

      Application.ProcessMessages;

      Result := True;
    end;
end;

function TSMExportBaseComponent.ProgressCanceled: Boolean;
begin
  if Assigned(frmSMEProcess) then
    Result := frmSMEProcess.CancelPressed
  else
    Result := boolAborted;
end;

procedure TSMExportBaseComponent.SetProgressCanceled(Value: Boolean);
begin
  if Assigned(frmSMEProcess) then
    frmSMEProcess.CancelPressed := Value
  else
    boolAborted := Value
end;

procedure TSMExportBaseComponent.BuildDefaultColumns;
begin
  try
    CreateSourceDataEngine;
    if Assigned(SourceDataEngine) then
      SourceDataEngine.FillColumns(Columns, Bands, RightToLeft);
  finally
    DestroySourceDataEngine;
  end
end;

procedure TSMExportBaseComponent.InternalBeforeProcess;
var
  i: Integer;
  strExt: string;
begin
  Statistic.Result := erInProgress;
  Statistic.CurrentRow := 0;
  Statistic.CurrentCol := 0;

  if Assigned(OnBeforeExecute) then
    OnBeforeExecute(Self);

  CreateSourceDataEngine;
  intRecordCount := SourceDataEngine.RecordCount;

  if DataFormats.UseRegionalSettings then
    FDataFormats.LoadRegionalSettings;

  {save a previous and set the custom regional settings/formats}
  oldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := DataFormats.DecimalSeparator;

  oldDateSeparator := DateSeparator;
  DateSeparator := DataFormats.DateSeparator;

  oldTimeSeparator := TimeSeparator;
  TimeSeparator := DataFormats.TimeSeparator;

  oldThousandSeparator := ThousandSeparator;
  ThousandSeparator := DataFormats.ThousandSeparator;

  oldCurrencyString := CurrencyString;
  CurrencyString := DataFormats.CurrencyString;

  if (soDisableControls in Options) then
    SourceDataEngine.DisableControls;

  if (soWaitCursor in Options) then
    Screen.Cursor := crHourGlass;
  boolAborted := False;
  if AnimatedStatus then
  begin
    {the Midware dataset from return the noncorrect value (=-1).
     So we must hide the ProgressBar}
    if intRecordCount < 0 then
      intRecordCount := 0;
    CreateProgressDlg(KeyGenerator, GetLanguageString(8), GetLanguageString(12), 0, intRecordCount, 0);
  end;

  {fill the defaul columns if user didn't defined a custom}
  IsCustomColumns := (Columns.Count > 0);
  if not IsCustomColumns then
    SourceDataEngine.FillColumns(Columns, Bands, RightToLeft);

  {generate the export}
  Statistic.TotalCount := 0;

  {calculate an index of last visible column}
  FLastExportColumnIndex := 0;
  for i := 0 to Columns.Count-1 do
    if Columns[i].Visible then
      Inc(FLastExportColumnIndex);
//      FLastExportColumnIndex := i;

  {replace empty extension with default one}
  strExt := ExtractFileExt(FileName);
  if (strExt = '') then
    FileName := ChangeFileExt(FileName, GetDefExt(Ord(TableType)));
end;

procedure TSMExportBaseComponent.InternalAfterProcess;
var
  str: string;
begin
  {clear a default columns info}
  if not IsCustomColumns then
  begin
    Columns.Clear;
    Bands.Clear;
  end;

  if (Statistic.Result <> erFailed) then
  begin
    if ProgressCanceled then
    begin
      str := GetLanguageString(16);
      Statistic.Result := erCanceled
    end
    else
    begin
      Statistic.Result := erCompleted;
      str := Format(GetLanguageString(2), [FFileName])
    end;
  end;
  
  if Assigned(OnProgress) then
    OnProgress(Self, intRecordCount, intRecordCount, IsAborted);

  if AnimatedStatus then
  begin
    UpdateProgressDlg(str, intRecordCount);
    DestroyProgressDlg;
  end;
  if (soShowMessage in Options) and
     (str <> '') and 
     ((not AnimatedStatus) or (Statistic.Result = erFailed)) then
    MessageDlg(str, mtInformation, [mbOk], 0);

  if (Statistic.Result <> erFailed) then
    AfterExport;

  {restore saved regional settings}
  DecimalSeparator := oldDecimalSeparator;
  DateSeparator := oldDateSeparator;
  TimeSeparator := oldTimeSeparator;
  ThousandSeparator := oldThousandSeparator;
  CurrencyString := oldCurrencyString;

  if (soWaitCursor in Options) then
    Screen.Cursor := crDefault;

  if (soDisableControls in Options) then
    SourceDataEngine.EnableControls;

  DestroySourceDataEngine;

  if Assigned(OnAfterExecute) then
    OnAfterExecute(Self);
end;

procedure TSMExportBaseComponent.Execute;
begin
  try
    try
      InternalBeforeProcess;

      Prepare;

    except
      on E: Exception do
      begin
        Statistic.Result := erFailed;

        if (soShowMessage in Options) then
          MessageDlg(GetLanguageString(3) + ': ' + E.Message, mtInformation, [mbOk], 0);
      end
    end
  finally
    InternalAfterProcess;
  end;
end;

procedure TSMExportBaseComponent.CreateDirIfNotExists(const s: string);
var
  strDir: string;
begin
  strDir := ExtractFilePath(s);
  if not DirectoryExists(strDir) then
    ForceDirectories(strDir)
end;

function Font2String(AAlignment: TAlignment; AColor: TColor; smFont: TFont): string;
begin
  Result := IntToStr(Ord(AAlignment)) + ',' +
            IntToStr(AColor) + ',' +
            smFont.Name + ',' +
            IntToStr(smFont.CharSet) + ',' +
            IntToStr(smFont.Color) + ',' +
            IntToStr(smFont.Size) + ',' +
            IntToStr(Byte(smFont.Style));
end;

procedure String2Font(var AAlignment: TAlignment; var AColor: TColor; smFont: TFont; s: string);
var
  Data: string;
  i: Integer;
begin
  try
    i := Pos(',', s);
    if (i > 0) then
    begin
      {alignment}
      Data := Trim(Copy(s, 1, i-1));
      if (Data <> '') then
        AAlignment := TAlignment(StrToIntDef(Data, Ord(AAlignment)));
      Delete(s, 1, i);

      i := Pos(',', s);
      if (i > 0) then
      begin
        {color}
        Data := Trim(Copy(s, 1, i-1));
        if (Data <> '') then
          AColor := TColor(StrToIntDef(Data, AColor));
        Delete(s, 1, i);

        i := Pos(',', s);
        if (i > 0) then
        begin
          {font name}
          Data := Trim(Copy(s, 1, i-1));
          if (Data <> '') then
            smFont.Name := Data;
          Delete(s, 1, i);

          i := Pos(',', s);
          if (i > 0) then
          begin
            {CharSet}
            Data := Trim(Copy(s, 1, i-1));
            if (Data <> '') then
              smFont.Charset := TFontCharSet(StrToIntDef(Data, smFont.Charset));
            Delete(s, 1, i);

            i := Pos(',', s);
            if (i > 0) then
            begin
              {Color}
              Data := Trim(Copy(s, 1, i-1));
              if (Data <> '') then
                smFont.Color := TColor(StrToIntDef(Data, smFont.Color));
              Delete(s, 1, i);

              i := Pos(',', s);
              if (i > 0) then
              begin
                {Size}
                Data := Trim(Copy(s, 1, i-1));
                if (Data <> '') then
                  smFont.Size := StrToIntDef(Data, smFont.Size);
                Delete(s, 1, i);
                {Style}
                Data := Trim(s);
                if (Data <> '') then
                  smFont.Style := TFontStyles(Byte(StrToIntDef(Data, Byte(smFont.Style))));
              end
            end
          end
        end
      end
    end;
  except
  end;

end;

procedure TSMExportBaseComponent.SaveSpecification(SpecName, FileName: string; ShowDialog: Boolean);
var
  s: string;
  strStream: TStringStream;
begin
  if ShowDialog then
  begin
    with TfrmSMESaveSpec.Create(nil) do
      try
        Caption := strESpecCaption;
        lblSpecName.Caption := strESpecCaption;
        if (SpecName = '') then
          edSpecName.Text := strESpecCaption
        else
          edSpecName.Text := SpecName;
        lblFileName.Caption := strFileName;
        if (FileName = '') then
        begin
          if (SpecificationDir = '') then
            edFileName.Text := ExtractFilePath(Application.ExeName)
          else
            edFileName.Text := SpecificationDir;

          edFileName.Text := edFileName.Text + ChangeFileExt(ExtractFileName(FileName), '.sme')
        end
        else
          edFileName.Text := FileName;

        btnOk.Caption := strBtnOk;
        btnCancel.Caption := strBtnCancel;

        Filter := strESpecCaption + '|*.SME|' + strAllFiles;

        if (ShowModal = mrOk) then
        begin
          FileName := edFileName.Text;
          SpecName := edSpecName.Text
        end
      finally
        Free
      end
  end;

  if (FileName = '') or (SpecName = '') then
    exit;

  if Assigned(OnBeforeSaveSpecification) then
    OnBeforeSaveSpecification(Self, FileName);

  strStream := TStringStream.Create('');
  try
    SaveSpecificationToStream(strStream, SpecName);
    s := strStream.DataString;
  finally
    strStream.Free
  end;

  CreateDirIfNotExists(FileName);
  with TFileStream.Create(FileName, fmCreate) do
    try
      Write(s[1], Length(s))
    finally
      Free
    end;

  if Assigned(OnAfterSaveSpecification) then
    OnAfterSaveSpecification(Self, FileName);
end;

procedure TSMExportBaseComponent.LoadSpecification(strFileName: string);
var
  fs: TFileStream;
  s: string;
begin
  if (strFileName = '') then
  begin
    s := '';
    if SMEOpenSaveFileDialog(Application.Handle, 'sme', strESpecCaption + '|*.SME|' + strAllFiles, '', '', s, True) then
      strFileName := s;
  end;

  if (strFileName = '') or not FileExists(strFileName) then
    Exit;
  if Assigned(OnBeforeLoadSpecification) then
    OnBeforeLoadSpecification(Self, strFileName);

  fs := TFileStream.Create(strFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadSpecificationFromStream(fs);
  finally
    fs.Free
  end;

  if Assigned(OnAfterLoadSpecification) then
    OnAfterLoadSpecification(Self, strFileName);
end;

procedure TSMExportBaseComponent.LoadSpecificationFromStream(Stream: TStream);
var
  lstSpec: TStrings;

  strDef: string;
  i, j{, intCount}: Integer;
  AAlignment: TAlignment;
  AColor: TColor;
  smFont: TFont;
begin
  lstSpec := TStringList.Create;
  try
    lstSpec.LoadFromStream(Stream);

    {read a general information}
    FileName := lstSpec.Values['SourceFileName'];
    TableName := lstSpec.Values['TableName'];

    Options := TSMOptions(Word(StrToIntDef(lstSpec.Values['Options'], Word(Options))));

    ExportIfEmpty := (lstSpec.Values['ExportIfEmpty'] = '1');
    AddTitle := (lstSpec.Values['AddTitle'] = '1');
    BlankIfZero := (lstSpec.Values['BlankIfZero'] = '1');
    SelectedRecord := (lstSpec.Values['SelectedRecord'] = '1');
    RowsPerFile := StrToIntDef(lstSpec.Values['RowsPerFile'], 0);

    CharacterSet := TCharacterSet(StrToIntDef(lstSpec.Values['CharacterSet'], Ord(CharacterSet)));
    ActionAfterExport := TActionAfterExport(StrToIntDef(lstSpec.Values['ActionAfterExport'], Ord(ActionAfterExport)));

    strDef := lstSpec.Values['TableType'];
    TableType := TTableTypeExport(GetEnumValue(TypeInfo(TTableTypeExport), 'te' + strDef));

    {load the export style}
    ExportStyle.Style := TSMEStyle(StrToIntDef(lstSpec.Values['Style'], Ord(ExportStyle.Style)));
    ExportStyle.OddColor := StrToIntDef(lstSpec.Values['OddColor'], ExportStyle.OddColor);
    ExportStyle.EvenColor := StrToIntDef(lstSpec.Values['EvenColor'], ExportStyle.EvenColor);
    Layout := TSMELayout(StrToIntDef(lstSpec.Values['Layout'], Ord(Layout)));

    {load the page setup}
    PageSetup.Measure := TSMEMeasure(StrToIntDef(lstSpec.Values['Measure'], Ord(PageSetup.Measure)));
    PageSetup.UseDefault := (lstSpec.Values['UseDefault'] = '1');
    PageSetup.TopMargin := StrToFloatDef(lstSpec.Values['TopMargin'], PageSetup.TopMargin);
    PageSetup.BottomMargin := StrToFloatDef(lstSpec.Values['BottomMargin'], PageSetup.BottomMargin);
    PageSetup.LeftMargin := StrToFloatDef(lstSpec.Values['LeftMargin'], PageSetup.LeftMargin);
    PageSetup.RightMargin := StrToFloatDef(lstSpec.Values['RightMargin'], PageSetup.RightMargin);
    PageSetup.TableWidth := StrToFloatDef(lstSpec.Values['TableWidth'], PageSetup.TableWidth);
    PageSetup.Orientation := TSMEOrientation(StrToIntDef(lstSpec.Values['Orientation'], Ord(PageSetup.Orientation)));

    {load mail settings}
    SendTo.EMailRecipient := lstSpec.Values['Recipient'];
    SendTo.EMailSubject := lstSpec.Values['Subject'];
    SendTo.EMailOpenBeforeSend := (lstSpec.Values['OpenDialog'] = '1');

    {read columns/mappings}
    Columns.Clear;
    Bands.Clear;
    i := 0;
    while True do
    begin
      strDef := lstSpec.Values['Map' + IntToStr(i)];
      if (strDef <> '') and (strDef <> ' = ') then
      begin
        with Columns.Add do
        begin
          j := Pos(' = ', strDef);
          if j < 1 then
            FieldName := Trim(strDef)
          else
          begin
            FieldName := Trim(Copy(strDef, 1, j));
            Title.Caption := Trim(Copy(strDef, j + 3, Length(strDef)-j-2));
          end;
          if (Title.Caption = '') then
          begin
            Title.Caption := FieldName;
            Visible := False
          end;

          {colors/alignments/fonts}
          AAlignment := Alignment;
          AColor := Color;
          smFont := Font;
          String2Font(AAlignment, AColor, smFont, lstSpec.Values['Col' + IntToStr(i)]);
          Alignment := AAlignment;
          Color := AColor;
          Font := smFont;

          AAlignment := Title.Alignment;
          AColor := Title.Color;
          smFont := Title.Font;
          String2Font(AAlignment, AColor, smFont, lstSpec.Values['ColTitle' + IntToStr(i)]);
          Title.Alignment := AAlignment;
          Title.Color := AColor;
          Title.Font := smFont;

          Width := StrToIntDef(lstSpec.Values['Width' + IntToStr(i)], Width);
        end;
      end
      else
      begin
        if (i > 150) then
          break;
      end;
      Inc(i)
    end;

    {read a Header}
    Header.Clear;
    i := 0;
    while True do
    begin
      strDef := lstSpec.Values['Header' + IntToStr(i)];
      if strDef <> '' then
        Header.Add(strDef)
      else
        break;
      Inc(i)
    end;

    {read a Footer}
    Footer.Clear;
    i := 0;
    while True do
    begin
      strDef := lstSpec.Values['Footer' + IntToStr(i)];
      if strDef <> '' then
        Footer.Add(strDef)
      else
        break;
      Inc(i)
    end;

    {read the additional text settings}
    strDef := Translate(lstSpec.Values['FieldDelimiter'], False);
    if strDef <> '' then
      Separator := strDef[1];
    strDef := Translate(lstSpec.Values['TextQualifier'], False);
    if strDef <> '' then
      TextQualifier := strDef[1];
    RecordSeparator := Translate(lstSpec.Values['RecordSeparator'], False);
    Fixed := (lstSpec.Values['Fixed'] = '1');

    DataFormats.DateOrder := TSMEDateOrder(StrToIntDef(lstSpec.Values['DateOrder'], Ord(DataFormats.DateOrder)));
    strDef := Translate(lstSpec.Values['DateSeparator'], False);
    if (strDef <> '') then
      DataFormats.DateSeparator := strDef[1];
    strDef := Translate(lstSpec.Values['TimeSeparator'], False);
    if (strDef <> '') then
      DataFormats.TimeSeparator := strDef[1];
    strDef := Translate(lstSpec.Values['ThousandSeparator'], False);
    if (strDef <> '') then
      DataFormats.ThousandSeparator := strDef[1];
    strDef := Translate(lstSpec.Values['DecimalSeparator'], False);
    if (strDef <> '') then
      DataFormats.DecimalSeparator := strDef[1];
    DataFormats.CurrencyString := lstSpec.Values['CurrencyString'];
    DataFormats.FourDigitYear := (lstSpec.Values['FourDigitYear'] = '1');
    DataFormats.LeadingZerosInDate := (lstSpec.Values['LeadingZerosInDate'] = '1');

    DataFormats.CustomDateTimeFormat := lstSpec.Values['CustomDateTimeFormat'];
    DataFormats.UseRegionalSettings := (lstSpec.Values['UseRegionalSettings'] = '1');

    DataFormats.BooleanTrue := lstSpec.Values['BooleanTrue'];
    DataFormats.BooleanFalse := lstSpec.Values['BooleanFalse'];
  finally
    lstSpec.Free
  end
end;

procedure TSMExportBaseComponent.SaveSpecificationToStream(Stream: TStream; const SpecName: string);

  procedure WriteString(Value: string);
  begin
    Value := Value+#13#10;
    Stream.Write(Value[1], Length(Value));
  end;

var
  i, intCurVisible: Integer;
  s, strColSections: string;
begin
  {save a general information}
  WriteString('[Common]');

  WriteString('App=' + Application.Title);
  WriteString('Description=SMExport: specification'); //do not translate!
  WriteString('Specification=' + SpecName);
  s := GetEnumName(TypeInfo(TTableTypeExport), Ord(TableType));
  WriteString('TableType=' + Copy(s, 3, Length(s)-2));

  {save the items of options}
  if AddTitle then
    WriteString('AddTitle=1')
  else
    WriteString('AddTitle=0');
  if BlankIfZero then
    WriteString('BlankIfZero=1')
  else
    WriteString('BlankIfZero=0');
  WriteString('SourceFileName=' + FileName);
  if SelectedRecord then
    WriteString('SelectedRecord=1')
  else
    WriteString('SelectedRecord=0');
  WriteString('TableName=' + TableName);
  WriteString('CharacterSet=' + IntToStr(Ord(CharacterSet)));
  WriteString('SourceFileName=' + FileName);
  WriteString('Options=' + IntToStr(Word(Options)));
  if ExportIfEmpty then
    WriteString('ExportIfEmpty=1')
  else
    WriteString('ExportIfEmpty=0');
  WriteString('RowsPerFile=' + IntToStr(RowsPerFile));

  WriteString('ActionAfterExport=' + IntToStr(Ord(ActionAfterExport)));

  {save the export style}
  WriteString(#13#10'[ExportStyle]');
  WriteString('Style=' + IntToStr(Ord(ExportStyle.Style)));
  WriteString('OddColor=' + IntToStr(ExportStyle.OddColor));
  WriteString('EvenColor=' + IntToStr(ExportStyle.EvenColor));
  WriteString('Layout=' + IntToStr(Ord(Layout)));

  {save the page setup}
  WriteString(#13#10'[PageSetup]');
  WriteString('Measure=' + IntToStr(Ord(PageSetup.Measure)));
  if PageSetup.UseDefault then
    WriteString('UseDefault=1')
  else
    WriteString('UseDefault=0');
  WriteString('TopMargin=' + FloatToStr(PageSetup.TopMargin));
  WriteString('BottomMargin=' + FloatToStr(PageSetup.BottomMargin));
  WriteString('LeftMargin=' + FloatToStr(PageSetup.LeftMargin));
  WriteString('RightMargin=' + FloatToStr(PageSetup.RightMargin));
  WriteString('TableWidth=' + FloatToStr(PageSetup.TableWidth));
  WriteString('Orientation=' + IntToStr(Ord(PageSetup.Orientation)));

  {save mail-settings}
  WriteString(#13#10'[SendTo]');
  WriteString('Recipient=' + SendTo.EMailRecipient);
  WriteString('Subject=' + SendTo.EMailSubject);
  if SendTo.EMailOpenBeforeSend then
    WriteString('OpenDialog=1')
  else
    WriteString('OpenDialog=0');

  {save columns/mappings}
  WriteString(#13#10'[Mappings]');
  intCurVisible := 0;
  strColSections := '';
  WriteString('Count=' + IntToStr(Columns.Count));
  for i := 0 to Columns.Count-1 do
  begin
    if Columns[i].Visible then
    begin
      case MappingVersion() of
        1: //fixed text
           s := strTextFieldName + IntToStr(intCurVisible) + '-' + IntToStr(Columns[i].Width);
        2: //csv
           s := strField + IntToStr(intCurVisible);
      else
        s := Columns[i].Title.Caption;
      end;
//      s := Columns[i].FieldName + ' = ' + s;
      Inc(intCurVisible)
    end
    else
      s := '';
    s := Columns[i].FieldName + ' = ' + s;

    WriteString('Map' + IntToStr(i) + '=' + s);

    {colors/alignments/fonts}
    strColSections := strColSections +
                      #13#10'Col' + IntToStr(i) + '=' + Font2String(Columns[i].Alignment, Columns[i].Color, Columns[i].Font) +
                      #13#10'ColTitle' + IntToStr(i) + '=' + Font2String(Columns[i].Title.Alignment, Columns[i].Title.Color, Columns[i].Title.Font) +
                      #13#10'Width' + IntToStr(i) + '=' + IntToStr(Columns[i].Width);
  end;
  if strColSections <> '' then
    WriteString(#13#10'[COLUMNS]' + strColSections);

  {save a Header}
  WriteString(#13#10'[Header]');
  WriteString('Count=' + IntToStr(Header.Count));
  for i := 0 to Header.Count-1 do
    WriteString('Header' + IntToStr(i) + '=' + Header[i]);

  {save a Footer}
  WriteString(#13#10'[Footer]');
  WriteString('Count=' + IntToStr(Footer.Count));
  for i := 0 to Footer.Count-1 do
    WriteString('Footer' + IntToStr(i) + '=' + Footer[i]);

  {save the additional text settings}
  WriteString(#13#10'[Text]');
  WriteString('FieldDelimiter=' + Translate(Separator, True));
  WriteString('TextQualifier=' + Translate(TextQualifier, True));
  WriteString('RecordSeparator=' + Translate(RecordSeparator, True));
  if Fixed then
    WriteString('Fixed=1')
  else
    WriteString('Fixed=0');

  i := Header.Count;
  if AddTitle then
  begin
    if (soBlankRowAfterCaptions in Options) then
      Inc(i, 2)
    else
      Inc(i)
  end;
  WriteString('RowFirst=' + IntToStr(i));
  WriteString('RowLast=' + IntToStr(MaxInt{RowLast}));

  WriteString(#13#10'[DataFormat]');
  WriteString('DateOrder=' + IntToStr(Ord(FDataFormats.DateOrder)));
  WriteString('DateSeparator=' + Translate(FDataFormats.DateSeparator, True));
  WriteString('TimeSeparator=' + Translate(FDataFormats.TimeSeparator, True));
  WriteString('ThousandSeparator=' + Translate(FDataFormats.ThousandSeparator, True));
  WriteString('DecimalSeparator=' + Translate(FDataFormats.DecimalSeparator, True));
  WriteString('CurrencyString=' + FDataFormats.CurrencyString);
  if FDataFormats.FourDigitYear then
    WriteString('FourDigitYear=1')
  else
    WriteString('FourDigitYear=0');
  if FDataFormats.LeadingZerosInDate then
    WriteString('LeadingZerosInDate=1')
  else
    WriteString('LeadingZerosInDate=0');
  WriteString('CustomDateTimeFormat=' + FDataFormats.CustomDateTimeFormat);
  WriteString('BooleanTrue=' + FDataFormats.BooleanTrue);
  WriteString('BooleanFalse=' + FDataFormats.BooleanFalse);
  if FDataFormats.UseRegionalSettings then
    WriteString('UseRegionalSettings=1')
  else
    WriteString('UseRegionalSettings=0');
end;

procedure TSMExportBaseComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FDBGrid) then
      FDBGrid := nil
    else
      if (AComponent = FDataSet) then
        FDataSet := nil
      else
        if (AComponent = FDataEngine) then
          FDataEngine := nil;
  end;
end;


end.