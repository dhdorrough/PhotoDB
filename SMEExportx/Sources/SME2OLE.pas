{ SMExport suite
  TSMExportToExcel, TSMExportToWord and TSMExportToAccess OLE components

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2OLE;

interface

{$I sme.inc}

uses
  Windows,
  {$IFDEF SMForDelphi3 }
  SysUtils,
  {$ENDIF SMForDelphi3}
  Classes, Graphics, DB, ExportDS, SME2Cell, SMEEngine
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TMSExcelVersion = (evExcelAuto, evExcel2, evExcel2FarEast, evExcel3, evExcel4, evExcel5, evExcel7, evExcel9597);

var
  DefaultExcelVersion: TMSExcelVersion = evExcelAuto;

type
  TSMExportToExcel = class(TSMExportToCellFile)
  private
    { Private declarations }
    FExcelVersion: TMSExcelVersion;
    FTemplateFile: string;
    IsNewestCom: Boolean;

    FAutoFitColumns: Boolean;
    FUseCurrentInstance: Boolean;

    FStartRow: Integer;
    FFreezeFixed: Boolean;

    FPassword: string;
    strLastFileName: string;
    FIsMemoColumnExist: Boolean;
    arrData: Variant;
    colorDefault: TColor;

    function IsVersionStore: Boolean;
    procedure DrawBorder(Range: Variant; IsVertical: Boolean);
  protected
    { Protected declarations }
    intMergedSheetRows: Integer;

    procedure InternalFileCreate(const AFileName: string); override;
    function GetFormat(ft: TFieldType; Precision: Integer): string; virtual;
    function IsFormula(const Value: string): Boolean;
  public
    { Public declarations }
    xls, xlw: Variant;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;

    procedure CloseFileStream; override;

    procedure WriteHeader; override;
    procedure WriteFooter; override;

    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;
  published
    { Published declarations }
    property AutoFitColumns: Boolean read FAutoFitColumns write FAutoFitColumns default False;
    property UseCurrentInstance: Boolean read FUseCurrentInstance write FUseCurrentInstance default False;
    property ExcelVersion: TMSExcelVersion read FExcelVersion write FExcelVersion stored IsVersionStore default evExcelAuto;
    property TemplateFile: string read FTemplateFile write FTemplateFile;
    property FreezeFixed: Boolean read FFreezeFixed write FFreezeFixed default False;
    property Password: string read FPassword write FPassword;
    property PageSetup;

    property ActionAfterExport;
    property FileName;

    property StartRow: Integer read FStartRow write FStartRow default 0;
    property AddTitle;
    property CharacterSet;

    property ExportStyle;

    property Header;
    property Footer;

    property OnGetCellParams;
  end;

  TSMExportToWord = class(TSMExportToCellFile)
  private
    { Private declarations }
    IsNewestCom: Boolean;

    FUseCurrentInstance: Boolean;
    FTemplateFile: string;

    strLastFileName: string;

    arrColWidths: Variant;
    intTblRowCount, intTblColCount: Integer;

    FAutoFitColumns: Boolean;
    FPassword: string;

    function GetColorIndex(color: TColor): Integer;
    function GetMeasureMargin(Measure: TSMEMeasure; Value: Single): Single;
  protected
    { Protected declarations }
    intTotalMergedCols: Integer;
//    procedure Prepare; override;
    procedure InternalFileCreate(const AFileName: string); override;
  public
    { Public declarations }
    word, document, table: Variant;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;

    procedure CloseFileStream; override;

    procedure WriteDataBegin; override;
    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;
  published
    { Published declarations }
    property UseCurrentInstance: Boolean read FUseCurrentInstance write FUseCurrentInstance default False;
    property TemplateFile: string read FTemplateFile write FTemplateFile;
    property AutoFitColumns: Boolean read FAutoFitColumns write FAutoFitColumns default False;
    property Password: string read FPassword write FPassword;
    property PageSetup;

    property ActionAfterExport;
    property FileName;

    property AddTitle;
    property CharacterSet;

    property ExportStyle;
    property Header;
    property Footer;

    property OnGetCellParams;
  end;


  TSMExportToADODOA = class(TSMExportBaseComponent)
  private
    { Private declarations }
    FUseCurrentInstance: Boolean;

    FUserName: string;
    FPassword: string;
  protected
    { Protected declarations }
    IsNewestCom: Boolean;
    fontUser: TFont;

    procedure InitializeConnection; virtual;
    procedure FinilizeConnection; virtual;
    procedure OpenDatabase(Value: string); virtual;
    function TableExists(const Value: string): Boolean; virtual;
    procedure DeleteTable(const Value: string); virtual;
    procedure CreateNewTable; virtual;
    procedure OpenRecordset; virtual;
    procedure AddNewRecord; virtual;
    procedure UpdateField(ACol: Integer; fld: TField; Value: string); virtual;
    procedure PostRecord; virtual;

    procedure Prepare; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property UseCurrentInstance: Boolean read FUseCurrentInstance write FUseCurrentInstance default False;

    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;

    property ActionAfterExport;
    property FileName;
    property TableName;

    property CharacterSet;
    property OnGetCellParams;
  end;

  TMSAccessVersion = (avAuto, avAccess97, avAccess2000);

var
  DefaultAccessVersion: TMSAccessVersion = avAuto;

type
  TSMExportToAccess = class(TSMExportToADODOA)
  private
    { Private declarations }
    FMSAccessVersion: TMSAccessVersion;

    function IsVersionStore: Boolean;
  protected
    { Protected declarations }
    function GetFieldStr(Field: TField): string; override;
    procedure InitializeConnection; override;
    procedure FinilizeConnection; override;
    procedure OpenDatabase(Value: string); override;
    function TableExists(const Value: string): Boolean; override;
    procedure DeleteTable(const Value: string); override;
    procedure CreateNewTable; override;
    procedure OpenRecordset; override;
    procedure AddNewRecord; override;
    procedure UpdateField(ACol: Integer; fld: TField; Value: string); override;
    procedure PostRecord; override;
  public
    { Public declarations }
    access, db, recordset: OLEVariant;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;
  published
    { Published declarations }
    property MSAccessVersion: TMSAccessVersion read FMSAccessVersion write FMSAccessVersion stored IsVersionStore default avAuto;
  end;

var
  DefaultExportAsRealHeaderFooter: Boolean = False;

procedure GetAccessTableNames(lst: TStrings; const DBName: string);

function MSExcelIsInstalled: Boolean;
function MSWordIsInstalled: Boolean;
function MSAccessIsInstalled: Boolean;
function MSJETIsInstalled: Boolean;

implementation

uses SMEStat, ExCnst, OleConst
     {$IFDEF SMForDelphi3}, ActiveX, ComObj {$ELSE}, Ole2, OleAuto {$ENDIF};

{$IFDEF SMForDelphi3}
const
  MaxDispArgs = 32;
{$ENDIF SMForDelphi3}

const
  EnglishLocale = (LANG_ENGLISH + SUBLANG_DEFAULT * 1024) + (SORT_DEFAULT shl 16);

function AppIsInstalled(strOLEObject: string): Boolean;
var
  ClassID: TCLSID;
begin
  Result := (CLSIDFromProgID(PWideChar(WideString(strOLEObject)), ClassID) = S_OK)
end;

function MSExcelIsInstalled: Boolean;
begin
  Result := AppIsInstalled('Excel.Application');
end;

function MSWordIsInstalled: Boolean;
begin
  Result := AppIsInstalled('Word.Application');
end;

function MSAccessIsInstalled: Boolean;
begin
  Result := AppIsInstalled('DAO.DBEngine.35');
  if not Result then
    Result := AppIsInstalled('DAO.DBEngine.36');
end;

function MSJETIsInstalled: Boolean;
begin
  Result := AppIsInstalled('ADODB.Connection');
end;

{ TSMExportToExcel }
constructor TSMExportToExcel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teExcel;
  FUseCurrentInstance := False;
  ExcelVersion := DefaultExcelVersion;

  FFreezeFixed := False;
  FStartRow := 0;
end;

function TSMExportToExcel.IsVersionStore: Boolean;
begin
  Result := (ExcelVersion <> DefaultExcelVersion)
end;

function TSMExportToExcel.Extension: string;
begin
  Result := '.XLS'
end;

function TSMExportToExcel.GetFormat(ft: TFieldType; Precision: Integer): string;
begin
  Result := '';
  case ft of
    ftSmallint,
    ftInteger,
    ftWord,
    ftAutoInc: Result := '0';

    ftFloat,
    ftBCD: begin
//             Result := '';//'0,00';
             if Precision = 0 then
               Result := ''
             else
               Result := AddZeros('0.0', 2+Precision, False);
//               Result := AddZeros('0' + DataFormats.DecimalSeparator + '0', 2+Precision, False);
//               Result := '#'
           end;

    ftCurrency: begin
//                  Result := '';//'0,00' ; '0,00$';
                  Result := '0' + DataFormats.DecimalSeparator + '0';//'0.00'
                  if Precision = 0 then
                  else
                    Result := AddZeros(Result, 2+Precision, False);
                  Result := Result + DataFormats.CurrencyString;
                end;

    ftDate: Result := '';//DataFormats.GetDateFormat;
    ftTime: Result := '';//DataFormats.GetTimeFormat;
    ftDateTime: Result := '';//DataFormats.GetDateTimeFormat;

  else
    Result := '@';
  end;
end;

function GetSpreadSheetColByID(ID: Integer): string;
const
  ZARange = Ord('Z') - Ord('A');
var
  i: Integer;
begin
  Result:= '';
  i := ID;
  while i > 0 do
  begin
    Result := Char(Ord('A') + Pred(i) mod Succ(ZARange)) + Result;
    i := Pred(i) div Succ(ZARange);
  end
end;

const
  AXLSAlignment: array[TAlignment] of Integer = (-4131{xlLeft}, -4152{xlRight}, -4108 {xlCenter});
  xlExcel2 = $00000010;
  xlExcel2FarEast = $0000001B;
  xlExcel3 = $0000001D;
  xlExcel4 = $00000021;
  xlExcel5 = $00000027;
  xlExcel7 = $00000027;
  xlExcel9795 = $0000002B;
  xlExcel4Workbook = $00000023;

procedure TSMExportToExcel.DrawBorder(Range: Variant; IsVertical: Boolean);
const
  xlInsideHorizontal = $0000000C;
  xlInsideVertical = $0000000B;
  xlDiagonalDown = $00000005;
  xlDiagonalUp = $00000006;
  xlEdgeBottom = $00000009;
  xlEdgeLeft = $00000007;
  xlEdgeRight = $0000000A;
  xlEdgeTop = $00000008;

  xlContinuous = $00000001;
begin
{  if (soColLines in Options) or
     (soRowLines in Options) then
    Range.Borders.LineStyle := 1; //xlContinuous
  exit;
}
  if IsVertical and (soColLines in Options) then
  begin
    Range.Borders.Item[xlEdgeLeft].LineStyle := xlContinuous;
    Range.Borders.Item[xlEdgeRight].LineStyle := xlContinuous;
//    Range.Borders.Item[xlInsideVertical].LineStyle := xlContinuous;
  end;

  if (not IsVertical) and (soRowLines in Options) then
  begin
    Range.Borders.Item[xlEdgeTop].LineStyle := xlContinuous;
    Range.Borders.Item[xlEdgeBottom].LineStyle := xlContinuous;
//    Range.Borders.Item[xlInsideHorizontal].LineStyle := xlContinuous;
  end
end;

procedure TSMExportToExcel.InternalFileCreate(const AFileName: string);
var
  strFileName: string;
  j: Integer;

  sheet: Variant;
  SheetIsFound: Boolean;
begin
  {DON'T CALL INHERITED!}

  if VarIsEmpty(xls) or
     VarIsNull(xls) then
  begin
    if UseCurrentInstance then
    begin
      try
        xls := GetActiveOleObject('Excel.Application');
        IsNewestCom := False;
      except
        xls := CreateOleObject('Excel.Application');
        IsNewestCom := True
      end
    end
    else
    begin
      xls := CreateOleObject('Excel.Application');
      IsNewestCom := True
    end;

    xls.DisplayAlerts := False;
  end;

  strFileName := AFileName;
  if Assigned(OnGetFileName) then
    OnGetFileName(Self, 0, strFileName);

  if (soMergeData in Options) and
     (FileExists(strFileName)) then
  begin
    xlw := xls.WorkBooks.Open(strFileName)
  end
  else
  begin
    if (TemplateFile = '') then
      xlw := xls.WorkBooks.Add
    else
      xlw := xls.WorkBooks.Add(Template := FTemplateFile);
  end;


  {iterate thru sheet collection and find the sheet by name}
  SheetIsFound := False;
  intMergedSheetRows := 0;
  for j := 1 to xlw.Sheets.Count do
  begin
    if (xlw.Sheets.Item[j].Name = KeyGenerator) then
    begin
      SheetIsFound := True;
      xlw.Sheets.Item[j].Activate;
      intMergedSheetRows := xls.ActiveSheet.UsedRange.Rows.Count{+2};
    end;
  end;
  Inc(intMergedSheetRows, StartRow);
  if not SheetIsFound then
  begin
    sheet := xlw.Sheets.Add;
    sheet.Name := KeyGenerator;
    sheet := UnAssigned;
  end;

  if not PageSetup.UseDefault then
  begin
    xls.ActiveSheet.PageSetup.LeftMargin := PageSetup.LeftMargin;
    xls.ActiveSheet.PageSetup.RightMargin := PageSetup.RightMargin;
    xls.ActiveSheet.PageSetup.TopMargin := PageSetup.TopMargin;
    xls.ActiveSheet.PageSetup.BottomMargin := PageSetup.BottomMargin;
  end;

  case PageSetup.Orientation of
    emPortrait: xls.ActiveSheet.PageSetup.Orientation := 1;
    emLandscape: xls.ActiveSheet.PageSetup.Orientation := 2;
  end;
{TODO}  colorDefault := clWhite;//xls.ActiveSheet.Columns[0].Interior.Color;

  strLastFileName := strFileName;
end;

procedure TSMExportToExcel.CloseFileStream;
var
  i: Integer;
begin
  inherited;

  if not VarIsEmpty(xls) and
     not VarIsNull(xls) then
  begin
    WriteFooter;

    {unhide columns (in some MS Excel column which is Visible := False can be hidden)}
    for i := 1 to Columns.Count do
//      xls.ActiveSheet.Columns[GetSpreadSheetColByID(i)].Hidden := False;
      xls.ActiveSheet.Columns[i].Hidden := False;

    if AutoFitColumns then
      xls.ActiveSheet.Columns.AutoFit;

    if FreezeFixed and AddTitle then
    begin
      xls.ActiveSheet.Rows[intMergedSheetRows+1].Select;
      xls.ActiveWindow.FreezePanes := True
    end;

    if (Password <> '') then
      xls.ActiveSheet.Protect(Password := Self.Password);

    case ExcelVersion of
      evExcel2: xlw.SaveAs(strLastFileName, xlExcel2);
      evExcel2FarEast: xlw.SaveAs(strLastFileName, xlExcel2FarEast);
      evExcel3: xlw.SaveAs(strLastFileName, xlExcel3);
      evExcel4: xlw.SaveAs(strLastFileName, xlExcel4);
      evExcel5: xlw.SaveAs(strLastFileName, xlExcel5);
      evExcel7: xlw.SaveAs(strLastFileName, xlExcel7);
      evExcel9597: xlw.SaveAs(strLastFileName, xlExcel9795);
//      evExcel2000: ;
    else
      xlw.SaveAs(strLastFileName);
    end;
    xlw.Close;
    xlw := UnAssigned;

    if IsNewestCom then
      xls.Quit;
    xls := UnAssigned;
  end;
end;

procedure TSMExportToExcel.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited;

  FIsMemoColumnExist := IsMemoColumnExist;
  if not FIsMemoColumnExist then
    arrData := VarArrayCreate([1, 1, 1, intColCount], varVariant);
end;

procedure TSMExportToExcel.WriteColWidth(intCurColumn, intColWidth: Integer);
var
  vColumn: Variant;
begin
  if (intColWidth < 1) then exit;

  vColumn := xls.ActiveSheet.Columns[intCurColumn+1];
  if not (soFieldMask in Options) then
    vColumn.NumberFormat := GetFormat(SourceDataEngine.DataTypeByColumn(Columns[intCurColumn]), Columns[intCurColumn].Precision);

  if intColWidth > 50 then
    vColumn.ColumnWidth := 50
  else
    vColumn.ColumnWidth := intColWidth;
end;

procedure TSMExportToExcel.WriteRowStart(IsAddedTitle: Boolean);
var
  i: Integer;
begin
  inherited;

  if not FIsMemoColumnExist then
    for i := VarArrayLowBound(arrData, 2) to VarArrayHighBound(arrData, 2) do
      arrData[1, i] := NULL;
end;

procedure TSMExportToExcel.WriteRowEnd(IsAddedTitle: Boolean);
var
  fValue: Variant;
begin
  fValue := xls.ActiveSheet.Range['A'+IntToStr(intMergedSheetRows+Statistic.CurrentRow+1),
                                  GetSpreadSheetColByID(Statistic.CurrentCol+1)+IntToStr(intMergedSheetRows+Statistic.CurrentRow+1)];
  if not FIsMemoColumnExist then
    fValue.Value := arrData;
  DrawBorder(fValue, False);

  inherited
end;

procedure TSMExportToExcel.WriteHeader;
begin
  inherited;

  {copy header/footer to page header/footer  - will be printed on each page}
  if DefaultExportAsRealHeaderFooter then
  begin
    if (Header.Count > 0) then
      xls.ActiveSheet.PageSetup.LeftHeader := Header.Text;
  end;
end;

procedure TSMExportToExcel.WriteFooter;
begin
  inherited;

  {copy header/footer to page header/footer  - will be printed on each page}
  if DefaultExportAsRealHeaderFooter then
  begin
    if (Footer.Count > 0) then
      xls.ActiveSheet.PageSetup.LeftFooter := Footer.Text;
  end;
end;

function TSMExportToExcel.IsFormula(const Value: string): Boolean;
const
  arrFormulas: array[0..30] of string = (
     'SUM',
     'COUNT',
     'AVERAGE',
     'MIN',
     'MAX',
     'ROW',
     'COLUMN',
     'NPV',
     'STDEV',
     'SIN',
     'ASIN',
     'COS',
     'ACOS',
     'TAN',
     'ATAN',
     'ATAN2',
     'PI',
     'SQRT',
     'EXP',
     'LN',
     'LOG',
     'LOG10',
     'ABS',
     'INT',
     'SIGN',
     'ROUND',
     'MID',
     'LEN',
     'TREND',
     'IF',
     'RAND'
     );
var
  i: Integer;
  s: string;
begin
  Result := False;
  if (Value <> '') and ((Value[1] = '=') or (Value[1] = '-')) then
  begin
    for i := Low(arrFormulas) to High(arrFormulas) do
    begin
      s := arrFormulas[i] + '(';
      if (UpperCase(Copy(Value, 2, Length(s))) = s) then
      begin
        Result := True;
        break
      end
    end
  end;
end;

procedure TSMExportToExcel.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  PicFileName: string;
  pic: TPicture;
  fValue: Variant;

  strmPic: TStringStream;
  bmp: TBitmap;
//  j: Integer;
begin
  inherited;

  if (AString <> '') then
  begin
    if (Assigned(fld) and
        fld.IsBlob and
        not fld.IsNull and
        (TBlobField(fld).BlobType = ftGraphic)) or
      (CellType = ctGraphic) then
    begin
      pic := TPicture.Create;
      try
        if Assigned(fld) then
          pic.Assign(TBlobField(fld))
        else
        begin
          {for constant column kind we must load bitmap from string}
          strmPic := TStringStream.Create(AString);
          bmp := TBitmap.Create;
          try
            strmPic.Seek(0, soFromBeginning);
            bmp.LoadFromStream(strmPic);
            pic.Assign(bmp);
          finally
            bmp.Free;
            strmPic.Free
          end
        end;

        PicFileName := GetFileNameByGraphic('', 0, pic);
        xls.ActiveSheet.Rows[intMergedSheetRows+ARow+1].RowHeight := pic.Height;
        pic.SaveToFile(PicFileName)
      finally
        pic.Free
      end;
      {load file to xls}
      xls.ActiveSheet.Cells[intMergedSheetRows+ARow+1, ACol+1].Select;
      xls.ActiveSheet.Pictures.Insert(PicFileName);
      {destroy temporary file}
      DeleteFile(PicFileName);
      {Range("B5").Select
       xls.ActiveSheet.Pictures.Insert(FileName)
       OR
       xls.ActiveSheet.Shapes.AddPicture(Dir + 'Technolgy.bmp' , // FileName (included in sample)
                    True , // LinkToFile
                    True , // SaveWithDocument
                    10 , // Left
                    90 , // Top
                    90 , // Width
                    90); // Height
       OR
       xls.ActiveSheet.Shapes.AddPicture('C:\Pictures\Hills.Bmp',
                    EmptyParam, EmptyParam, 10, 10,
                    EmptyParam, EmptyParam
       }
    end
    else
    begin
      if (soColorsFonts in Options) then
      begin
        fValue := xls.ActiveSheet.Cells[intMergedSheetRows+ARow+1, ACol+1];

        if colorDefault <> ColorToRGB(Color) then
          fValue.Interior.Color := ColorToRGB(color);
        fValue.HorizontalAlignment := AXLSAlignment[al];
        fValue.Font.Name := font.Name;
        fValue.Font.Size := font.Size;
        fValue.Font.Color := ColorToRGB(font.Color);
        if fsBold in font.Style then
          fValue.Font.Bold := True;
        if fsItalic in font.Style then
          fValue.Font.Italic := True;
        if fsUnderline in font.Style then
          fValue.Font.Underline := True;
      end;

      if (CellType in [ctString, ctMEMO]) then
      begin
        if IsFormula(AString) then
          AString := ' ' + AString;
      end
      else
      begin
{        for j := 1 to Length(AString) do
          if AString[j] = ',' then
            AString[j] := '.';
}
        if not (soFieldMask in Options) and
           (CellType <> Columns[Statistic.CurrentCol].DataType) then
          xls.ActiveSheet.Cells[intMergedSheetRows+ARow+1, Statistic.CurrentCol+1].NumberFormat := GetFormat(CellType2FieldType(CellType), Columns[Statistic.CurrentCol].Precision);
      end;
      if FIsMemoColumnExist then
        xls.ActiveSheet.Cells[intMergedSheetRows+ARow+1, ACol+1].Value := AString
      else
        arrData[1, ACol+1]{xls.ActiveSheet.Cells[intCurLine, k+1].Value} := AString;
    end
  end;
end;

procedure TSMExportToExcel.WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  strRange: string;
begin
  inherited;

  if (AMergeCols > 1) then
  begin
    strRange := IntToStr(intMergedSheetRows+ARow+1);
    strRange := GetSpreadSheetColByID(ACol+1) + strRange + ':' + GetSpreadSheetColByID(ACol+AMergeCols) + strRange;
    xls.ActiveSheet.Range[strRange].Merge;
  end;
end;

{ TSMExportToWord }
constructor TSMExportToWord.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teWord;
  FUseCurrentInstance := False;
end;

function TSMExportToWord.Extension: string;
begin
  Result := '.DOC'
end;

function TSMExportToWord.GetMeasureMargin(Measure: TSMEMeasure; Value: Single): Single;
begin
  {There are:
    1440 twips to 1 inch
    567 twips to 1 cm
    72 twips to 1 point}
  case Measure of
    emInch: Result := Value*1440/72;//word.InchesToPoints(Value);
    emCentimeters: Result := Value*567/72;//word.CentimetersToPoints(Value);
    emPicas: Result := word.PicasToPoints(Value);
  else
    Result := Value
  end;
end;

procedure TSMExportToWord.InternalFileCreate(const AFileName: string);
const
  wdOrientPortrait = $00000000;
  wdOrientLandscape = $00000001;
var
  strFileName: string;
begin
  {DON'T CALL INHERITED!}

  if VarIsEmpty(word) or
     VarIsNull(word) then
  begin
    if UseCurrentInstance then
    begin
      try
        word := GetActiveOleObject('Word.Application');
        IsNewestCom := False;
      except
        word := CreateOleObject('Word.Application');
        IsNewestCom := True
      end
    end
    else
    begin
      word := CreateOleObject('Word.Application');
      IsNewestCom := True
    end;

    word.Visible := False;//True;
    word.ScreenUpdating := False;
    word.Options.CheckSpellingAsYouType := False;
    word.Options.CheckGrammarAsYouType := False;
//    word.CommandBars.ActiveMenuBar.FindControl(10).Execute;
  end;

  strFileName := AFileName;
  if Assigned(OnGetFileName) then
    OnGetFileName(Self, 0, strFileName);

  if (soMergeData in Options) and
     (FileExists(strFileName)) then
  begin
    word.Documents.Open(strFileName)
  end
  else
  begin
    if (TemplateFile = '') then
      word.Documents.Add
    else
      word.Documents.Open(TemplateFile)
  end;
  document := word.ActiveDocument;
  if not PageSetup.UseDefault then
  begin
    if PageSetup.TopMargin > 0 then
      document.PageSetup.TopMargin := GetMeasureMargin(PageSetup.Measure, PageSetup.TopMargin);
    if PageSetup.BottomMargin > 0 then
      document.PageSetup.BottomMargin := GetMeasureMargin(PageSetup.Measure, PageSetup.BottomMargin);
    if PageSetup.LeftMargin > 0 then
      document.PageSetup.LeftMargin := GetMeasureMargin(PageSetup.Measure, PageSetup.LeftMargin);
    if PageSetup.RightMargin > 0 then
      document.PageSetup.RightMargin := GetMeasureMargin(PageSetup.Measure, PageSetup.RightMargin);

    case PageSetup.Orientation of
      emPortrait: document.PageSetup.Orientation := wdOrientPortrait;
      emLandscape: document.PageSetup.Orientation := wdOrientLandscape;
    end;
  end;

  strLastFileName := strFileName;
end;

procedure TSMExportToWord.CloseFileStream;
begin
  inherited;

  if not VarIsEmpty(table) and
     not VarIsNull(table) then
  begin
    if AutoFitColumns then
      table.Columns.AutoFit;
  end;

  if not VarIsEmpty(document) and
     not VarIsNull(document) then
  begin
    WriteFooter;

    if (Password <> '') then
      document.Protect(Password := Self.Password);
      
    document.SaveAs(strLastFileName);
    document.Close;

    table := UnAssigned;
    document := UnAssigned;

    word.ScreenUpdating := True;
    if IsNewestCom then
      word.Quit;

    word := UnAssigned;
  end;
end;

procedure TSMExportToWord.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited;

  arrColWidths := VarArrayCreate([0, intColCount-1], varInteger);

  intTblRowCount := intRowCount;
  intTblColCount := intColCount;
end;

const
  wdAuto = $00000000;

  wdAlignParagraphLeft = $00000000;
  wdAlignParagraphRight = $00000002;
  wdAlignParagraphCenter = $00000001;

  ADOCAlignment: array[TAlignment] of Integer = (wdAlignParagraphLeft, wdAlignParagraphRight, wdAlignParagraphCenter);

procedure TSMExportToWord.WriteDataBegin;
const
  wdAdjustNone = $00000000;

  wdLineStyleNone = $00000000;
  wdLineStyleSingle = $00000001;

  wdBorderTop = $FFFFFFFF;
  wdBorderLeft = $FFFFFFFE;
  wdBorderBottom = $FFFFFFFD;
  wdBorderRight = $FFFFFFFC;
  wdBorderHorizontal = $FFFFFFFB;
  wdBorderVertical = $FFFFFFFA;
var
  i, intWidth, intTableWidth: Integer;

{  ColorIndex: Integer;
  vRange: Variant;
}begin
  inherited;

  if (intTblRowCount < 1) or
     ((intTblColCount < 2) and (Layout <> elTabularForm)) then exit;

  if intTblColCount > 63 then
    intTblColCount := 63;

  intTotalMergedCols := 0;
  table := document.Range.Tables.Add(Range := document.Range(document.Content.End-1, document.Content.End-1),
                                     NumRows := 1, //intTblRowCount,
                                     NumColumns := intTblColCount-1);

  if not (soColLines in Options) and
     not (soRowLines in Options) then
  begin
    table.Borders.Item(wdBorderLeft).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderRight).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderVertical).LineStyle := wdLineStyleNone;

    table.Borders.Item(wdBorderTop).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderBottom).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderHorizontal).LineStyle := wdLineStyleNone;
  end
  else
  if not (soColLines in Options) and
     (soRowLines in Options) then
  begin
    table.Borders.Item(wdBorderLeft).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderRight).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderVertical).LineStyle := wdLineStyleNone;

    table.Borders.Item(wdBorderTop).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderBottom).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderHorizontal).LineStyle := wdLineStyleSingle;
  end
  else
  if (soColLines in Options) and
     not (soRowLines in Options) then
  begin
    table.Borders.Item(wdBorderLeft).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderRight).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderVertical).LineStyle := wdLineStyleSingle;

    table.Borders.Item(wdBorderTop).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderBottom).LineStyle := wdLineStyleNone;
    table.Borders.Item(wdBorderHorizontal).LineStyle := wdLineStyleNone;
  end
  else
  begin
    table.Borders.Item(wdBorderLeft).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderRight).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderVertical).LineStyle := wdLineStyleSingle;

    table.Borders.Item(wdBorderTop).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderBottom).LineStyle := wdLineStyleSingle;
    table.Borders.Item(wdBorderHorizontal).LineStyle := wdLineStyleSingle;
  end;

  if (PageSetup.TableWidth <> 0) then
    table.Columns.Width := GetMeasureMargin(PageSetup.Measure, PageSetup.TableWidth);

  {recalculate widths (autofit)}
  if AutoFitColumns then
  begin
    intWidth := 0;
    for i := VarArrayLowBound(arrColWidths, 1) to VarArrayHighBound(arrColWidths, 1)-1 do
    begin
      if VarIsEmpty(arrColWidths[i]) or
         VarIsNull(arrColWidths[i]) then
        arrColWidths[i] := 0;

      intWidth := intWidth + arrColWidths[i];
    end;
    intTableWidth := table.Columns.Width;
    if (intWidth <> 0) and
       (intTableWidth < intWidth) then
    begin
      for i := VarArrayLowBound(arrColWidths, 1) to VarArrayHighBound(arrColWidths, 1)-2 do
        arrColWidths[i] := arrColWidths[i]*intTableWidth/intWidth;
    end;
  end;

  for i := VarArrayLowBound(arrColWidths, 1) to VarArrayHighBound(arrColWidths, 1)-1 do
    if not VarIsEmpty(arrColWidths[i]) and
       not VarIsNull(arrColWidths[i]) and
       (arrColWidths[i] <> 0) then
    begin
//      table.Columns.Item(i+1).Width := arrColWidths[i];
      try
        table.Columns.Item(i+1).SetWidth(arrColWidths[i], wdAdjustNone);
      except
      end
    end;

{  if (Layout = elColumnar) then
  begin
    for i := 0 to Columns.Count-1 do
    begin
      vRange := table.Columns.Item(i+1);
      if (fsBold in Columns[i].Font.Style) then
        vRange.Bold := 1;
      if (fsItalic in Columns[i].Font.Style) then
        vRange.Italic := 1;
      if (fsUnderline in Columns[i].Font.Style) then
        vRange.Underline := 1;

      if (vRange.Cells.Alignment <> ADOCAlignment[Columns[i].Alignment]) then
        vRange.Cells.Alignment := ADOCAlignment[Columns[i].Alignment];
      ColorIndex := GetColorIndex(Columns[i].Font.Color);
      if (ColorIndex <> wdAuto) then
        vRange.Cells.Font.ColorIndex := ColorIndex;
      if (vRange.Cells.Font.Name <> Columns[i].Font.Name) then
        vRange.Cells.Font.Name := Columns[i].Font.Name;

      ColorIndex := GetColorIndex(Columns[i].Color);
      if (ColorIndex <> wdAuto) then
        vRange.Cells.HighlightColorIndex := ColorIndex;
    end;
  end;
}end;

procedure TSMExportToWord.WriteColWidth(intCurColumn, intColWidth: Integer);
begin
  if (intColWidth < 1) then exit;
  if intCurColumn > 63 then exit;

  intColWidth := 4*intColWidth;
//  if intColWidth > 50 then
//    intColWidth := 50;
  arrColWidths[intCurColumn] := intColWidth;
end;

function TSMExportToWord.GetColorIndex(color: TColor): Integer;
const
  wdBlack = $00000001;
  wdBlue = $00000002;
  wdTurquoise = $00000003;
  wdBrightGreen = $00000004;
  wdPink = $00000005;
  wdRed = $00000006;
  wdYellow = $00000007;
  wdWhite = $00000008;
  wdDarkBlue = $00000009;
  wdTeal = $0000000A;
  wdGreen = $0000000B;
  wdViolet = $0000000C;
  wdDarkRed = $0000000D;
  wdDarkYellow = $0000000E;
  wdGray50 = $0000000F;
  wdGray25 = $00000010;
begin
  case ColorToRGB(color) of
    clBlack: Result := wdBlack;
    clBlue: Result := wdBlue;
    clAqua: Result := wdTurquoise;
    clLime: Result := wdBrightGreen;
    clFuchsia: Result := wdPink;
    clRed: Result := wdRed;
    clYellow: Result := wdYellow;
    clWhite: Result := wdWhite;
    clNavy: Result := wdDarkBlue;
    clTeal: Result := wdTeal;
    clGreen: Result := wdGreen;
    clPurple: Result := wdViolet;
    clMaroon: Result := wdDarkRed;
    clOlive: Result := wdDarkYellow;
    clGray: Result := wdGray50;
    clSilver: Result := wdGray25;
  else
    Result := wdAuto
  end;
end;

procedure TSMExportToWord.WriteRowStart(IsAddedTitle: Boolean);
begin
  inherited;

//  if IsDataArea then
//    table.Rows.Add
end;

procedure TSMExportToWord.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  ColorIndex, intRowNum: LongInt;
  PicFileName: string;
  pic: TPicture;
  vRange: Variant;

  strmPic: TStringStream;
  bmp: TBitmap;
begin
  if ACol > 62 then exit;

  inherited;

  if IsDataArea and not VarIsEmpty(table) then
  begin
    if ACol < 1 then
      table.Rows.Add;

    if (AString <> '') then
    begin
      intRowNum := ARow - Header.Count + 1;

      if (Assigned(fld) and
          fld.IsBlob and
          not fld.IsNull and
          (TBlobField(fld).BlobType = ftGraphic)) or
        (CellType = ctGraphic) then
      begin
        pic := TPicture.Create;
        try
          if Assigned(fld) then
            pic.Assign(TBlobField(fld))
          else
          begin
            {for constant column kind we must load bitmap from string}
            strmPic := TStringStream.Create(AString);
            bmp := TBitmap.Create;
            try
              strmPic.Seek(0, soFromBeginning);
              bmp.LoadFromStream(strmPic);
              pic.Assign(bmp);
            finally
              bmp.Free;
              strmPic.Free
            end
          end;

          PicFileName := GetFileNameByGraphic('', 0, pic);
          pic.SaveToFile(PicFileName)
        finally
          pic.Free
        end;
        {load file to xls}
        document.InlineShapes.AddPicture(FileName := PicFileName,
                                         LinkToFile := False,
                                         SaveWithDocument := True,
                                         Range := Table.Cell(intRowNum, ACol+1).Range);
        {destroy temporary file}
        DeleteFile(PicFileName);
      end
      else
      begin
//        vRange := Table.Rows.Last.Cells.Item({intRowNum, }ACol+1).Range;
        vRange := Table.Cell(intRowNum, ACol+1).Range;

        if (soColorsFonts in Options) then
        begin
          if (fsBold in font.Style) then
            vRange.Bold := 1;
          if (fsItalic in font.Style) then
            vRange.Italic := 1;
          if (fsUnderline in font.Style) then
            vRange.Underline := 1;

          ColorIndex := GetColorIndex(font.color);
          if (ColorIndex <> wdAuto) then
            vRange.Font.ColorIndex := ColorIndex;
          if (vRange.Font.Name <> font.Name) then
            vRange.Font.Name := font.Name;

          ColorIndex := GetColorIndex(Color);
          if (ColorIndex <> wdAuto) then
            vRange.HighlightColorIndex := ColorIndex;
        end;

        if (vRange.ParagraphFormat.Alignment <> ADOCAlignment[al]) then
          vRange.ParagraphFormat.Alignment := ADOCAlignment[al];

        vRange.InsertAfter({Text := }AString)
      end
    end
  end
  else
  begin
    document.Range.ParagraphFormat.Alignment := ADOCAlignment[al];
    document.Range.InsertAfter(AString);
    document.Range.InsertParagraphAfter;
  end;
end;

procedure TSMExportToWord.WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
  inherited;

  if (AMergeCols > 1) then
  begin
    Table.Cell(ARow-Header.Count+1, ACol+1-intTotalMergedCols).Merge(MergeTo := Table.Cell(ARow-Header.Count+1, ACol+AMergeCols-intTotalMergedCols));
    Inc(intTotalMergedCols, AMergeCols-1);
  end;
end;


{ TSMExportToADODOA }
constructor TSMExportToADODOA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUseCurrentInstance := False;
end;

procedure TSMExportToADODOA.InitializeConnection;
begin
end;

procedure TSMExportToADODOA.OpenDatabase(Value: string);
begin
end;

function TSMExportToADODOA.TableExists(const Value: string): Boolean;
begin
  Result := False;
end;

procedure TSMExportToADODOA.DeleteTable(const Value: string);
begin
end;

procedure TSMExportToADODOA.CreateNewTable;
begin
end;

procedure TSMExportToADODOA.OpenRecordset;
begin
end;

procedure TSMExportToADODOA.AddNewRecord;
begin
end;

procedure TSMExportToADODOA.UpdateField(ACol: Integer; fld: TField; Value: string);
begin
end;

procedure TSMExportToADODOA.PostRecord;
begin
end;

procedure TSMExportToADODOA.FinilizeConnection;
begin
end;

procedure TSMExportToADODOA.Prepare;
var
  intCurColumn: Integer;

  strText, strFName: string;
  colorUser: TColor;
  alignUser: TAlignment;
  fieldUser: TField;
  ct: TCellType;

  Accept, IsAborted: Boolean;
begin
  if TableName = '' then
    raise Exception.Create(Format(GetLanguageString(0), ['TableName']));

  InitializeConnection;

  fontUser := TFont.Create;
  try
    strFName := FileName;
    if Assigned(OnGetFileName) then
      OnGetFileName(Self, 0, strFName);

    OpenDatabase(strFName);

    if not TableExists(TableName) then
    begin
      {if table exists, then we must delete it.}
      DeleteTable(TableName);

      {we must create a new table in database}
      CreateNewTable;
    end;

    {open the created table in database}
    OpenRecordset;

    SourceDataEngine.DisableControls;
    SourceDataEngine.SavePosition;
    SourceDataEngine.First;

    Statistic.TotalCount := 0;
    Statistic.CurrentRow := 0;
    Statistic.CurrentSection := esData;
    while (not ProgressCanceled) and
          IsTrialCount(Statistic.TotalCount) and
          (not SourceDataEngine.EOF) do
    begin
      if AnimatedStatus then
        UpdateProgressDlg(GetLanguageString(8), Statistic.TotalCount);

      Accept := True;
      if Assigned(OnBeforeRecord) then
        OnBeforeRecord(Self, Accept);
      if Accept then
      begin
        if Assigned(OnProgress) then
        begin
          IsAborted := False;
          OnProgress(Self, Statistic.TotalCount, SourceDataEngine.RecordCount, IsAborted);
          if IsAborted then
          begin
            SetProgressCanceled(True);
            break
          end
        end;

        Statistic.TotalCount := Statistic.TotalCount+1;

        {append the new record in opened table}
        AddNewRecord;

        Statistic.CurrentCol := 0;
        for intCurColumn := 0 to Columns.Count-1 do
          if Columns[intCurColumn].Visible then
          begin
            with Columns[intCurColumn] do
            begin
              colorUser := Color;
              fontUser.Assign(Font);
              alignUser := Alignment;
              if (ColumnKind = ckField) and SourceDataEngine.IsDataRow(Statistic.TotalCount-1) then
                fieldUser := SourceDataEngine.FindFieldByColumn(Columns[intCurColumn])
              else
                fieldUser := nil;
            end;
            if Assigned(fieldUser) then
              strText := GetFieldStr(fieldUser)
            else
            if (Columns[intCurColumn].ColumnKind = ckConstant) then
              strText := Columns[intCurColumn].FieldName
            else
            if (Columns[intCurColumn].ColumnKind = ckSysVar) then
              strText := GetSysValue(Columns[intCurColumn].FieldName)
            else
              strText := SourceDataEngine.GetFieldValue(Columns[intCurColumn]);

            if Assigned(OnGetCellParams) then
            begin
              ct := Columns[intCurColumn].DataType;
              OnGetCellParams(Self, fieldUser, strText, fontUser, alignUser, colorUser, ct);
            end;

            case CharacterSet of
              csASCII_MSDOS: strText := UECode(True, strText);
              csEBCDIC: strText := EBCDICCode(strText);
            end;

            {change the field value}
            UpdateField(Statistic.CurrentCol, fieldUser, strText);
            Statistic.CurrentCol := Statistic.CurrentCol + 1
          end;
        {post the new record}
        PostRecord;
      end;
      SourceDataEngine.Next;
      Statistic.CurrentRow := Statistic.CurrentRow + 1;

      IsAborted := False;
      if Assigned(OnAfterRecord) then
        OnAfterRecord(Self, IsAborted);
      if IsAborted then
        SetProgressCanceled(True);
    end;
    SourceDataEngine.RestorePosition;
    SourceDataEngine.EnableControls;
  finally
    fontUser.Free;
    FinilizeConnection;
  end;
end;

{ TSMExportToAccess }
procedure GetAccessTableNames(lst: TStrings; const DBName: string);

  function IsCorrectFile(FileName: string): Boolean;
  var
    Handle: THandle;
    FindData: TWin32FindData;
  begin
    Handle := FindFirstFile(PChar(FileName), FindData);
    Result := (Handle <> INVALID_HANDLE_VALUE);
    if Result then
      Windows.FindClose(Handle);
  end;

const
  dbSystemObject = -2147483646;
var
  access, db, td: Variant;
  i: Integer;
begin
  if DBName = '' then
    raise Exception.Create(Format(strPropertyError, ['DatabaseName']));

  lst.Clear;
  if not IsCorrectFile(DBName) then exit;

  try
    access := CreateOleObject('DAO.DBEngine.36');
  except
    access := CreateOleObject('DAO.DBEngine.35');
  end;

{  try
    access := GetActiveOleObject('DAO.DBEngine');
  except
    access := CreateOleObject('DAO.DBEngine');
  end;
}
  try
    {if database exists, then open it}
    try
      db := access.OpenDatabase(DBName);
      td := db.TableDefs;
      for i := 0 to td.Count-1 do
        if (td.Item[i].Attributes and dbSystemObject = 0) then
          lst.Add(td.Item[i].Name);
    except
    end;
  finally
    db.Close;
    access := UnAssigned;
  end;
end;

constructor TSMExportToAccess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MSAccessVersion := DefaultAccessVersion;
  TableType := teAccess;
end;

function TSMExportToAccess.Extension: string;
begin
  Result := '.MDB'
end;

function TSMExportToAccess.IsVersionStore: Boolean;
begin
  Result := (MSAccessVersion <> DefaultAccessVersion)
end;

function TSMExportToAccess.GetFieldStr(Field: TField): string;
var
  intSize: Integer;
begin
  if Assigned(Field) and
     not Field.IsNull then
  begin
    if (Field.DataType = ftBoolean) then
      Result := VarToStr(Field.AsBoolean)
    else
    if (Field.DataType in [ftDate, ftTime, ftDateTime]) then
      Result := VarFromDateTime(Field.AsDateTime)
    else
      if Field.IsBlob then
      begin
        with DataSet.CreateBlobStream(Field, bmRead) do
          try
            intSize := Size - Position;
            SetString(Result, nil, intSize);
            Read(Result[1], intSize);
//            Read(Variant(Pointer(Result)^), intSize);
          finally
            Free
          end
      end
      else
        Result := inherited GetFieldStr(Field)
  end
  else
    Result := inherited GetFieldStr(Field)
end;

procedure TSMExportToAccess.InitializeConnection;
begin
  inherited;

  case MSAccessVersion of
    avAccess97: begin
                  if UseCurrentInstance then
                  begin
                    try
                      access := GetActiveOleObject('DAO.DBEngine.35');
                      IsNewestCom := False;
                    except
                      try
                        access := CreateOleObject('DAO.DBEngine.35');
                        IsNewestCom := True;
                      except
                      end;
                    end
                  end
                  else
                  begin
                    try
                      access := CreateOleObject('DAO.DBEngine.35');
                      IsNewestCom := True;
                    except
                    end;
                  end;
                end;
    avAccess2000: begin
                    if UseCurrentInstance then
                    begin
                      try
                        access := GetActiveOleObject('DAO.DBEngine.36');
                        IsNewestCom := False;
                      except
                        try
                          access := CreateOleObject('DAO.DBEngine.36');
                          IsNewestCom := True;
                        except
                        end;
                      end
                    end
                    else
                    begin
                      try
                        access := CreateOleObject('DAO.DBEngine.36');
                        IsNewestCom := True;
                      except
                      end;
                    end;
                  end;
    else //avAuto
    begin
      if UseCurrentInstance then
      begin
        IsNewestCom := False;
        try
          access := GetActiveOleObject('DAO.DBEngine.36');
        except
          try
            access := GetActiveOleObject('DAO.DBEngine.35');
          except
            try
              access := CreateOleObject('DAO.DBEngine.36');
            except
              access := CreateOleObject('DAO.DBEngine.35');
            end;
            IsNewestCom := True;
          end;
        end
      end
      else
      begin
        try
          access := CreateOleObject('DAO.DBEngine.36');
        except
          access := CreateOleObject('DAO.DBEngine.35');
        end;
        IsNewestCom := True;
      end
    end
  end;
{  try
    access := GetActiveOleObject('DAO.DBEngine');
    IsNewestCom := False;
  except
    access := CreateOleObject('DAO.DBEngine');
    IsNewestCom := True;
  end;
}
end;

procedure TSMExportToAccess.OpenDatabase(Value: string);
var
  IsNeedCreate: Boolean;
  strConnection: string;
begin
  {if database exists, then open it.
   Else we must create a new database}
  IsNeedCreate := FileExists(Value);
  if IsNeedCreate then
  begin
    try
      if (Password = '') then
        db := access.OpenDatabase(Value)
      else
        db := access.OpenDatabase(Name := Value, Connect := ';PWD=' + FPassword);
      IsNeedCreate := True;
    except
      IsNeedCreate := False;
    end
  end;
  if not IsNeedCreate then
  begin
    strConnection := ';LANGID=0x0409;CP=' + IntToStr(GetACP()) + ';COUNTRY=0';
//    s := ';LANGID=0x0409;CP=1252;COUNTRY=0';
    db := access.CreateDatabase(Value, strConnection, 0);
  end;
end;

function TSMExportToAccess.TableExists(const Value: string): Boolean;
var
  j: Integer;
begin
  Result := inherited TableExists(Value);

  if (soMergeData in Options) then
  begin
    for j := 0 to db.TableDefs.Count-1 do
      if CompareText(db.TableDefs.Item[j].Name, TableName) = 0 then
      begin
        Result := True;
        break
      end;
  end;
end;

procedure TSMExportToAccess.DeleteTable(const Value: string);
var
  j: Integer;
begin
  inherited;

  for j := 0 to db.TableDefs.Count-1 do
    if CompareText(db.TableDefs.Item[j].Name, Value) = 0 then
    begin
      db.TableDefs.Delete(Value);
      break
    end;

{  try
    db.TableDefs.Delete(Value);
    except
  end;
}
end;

procedure TSMExportToAccess.CreateNewTable;
const
  dbFixedField = 1;
  dbVariableField = 2;

  arrMDBTypes: array[TFieldType] of Integer{Byte} =

    ({dbText} 10 {ftUnknown},
     {dbText} 10 {ftString},
     {dbInteger} 3{2} {ftSmallint},
     {dbLong} 4 {ftInteger},
     {dbInteger} 3 {ftWord},
     {dbBoolean} 1 {ftBoolean},
     {dbDouble} 7 {ftFloat},
     {dbCurrency} 5 {ftCurrency},
     {dbDouble} 7 {ftBCD},
     {dbDate} 8 {ftDate},
     {dbTime} 8{22} {ftTime},
     {dbDate} 8 {ftDateTime},
     {dbLongBinary} 11 {ftBytes},
     {dbLongBinary} 11 {ftVarBytes},
     {dbLong} 4{3} {ftAutoInc},
     {dbLongBinary} 11 {ftBlob},
     {dbMemo} 12 {ftMemo},
     {dbLongBinary} 11 {ftGraphic},
     {dbMemo} 12 {ftFmtMemo},
     {dbLongBinary} 11 {ftParadoxOle},
     {dbLongBinary} 11 {ftDBaseOle},
     {dbBinary} 9 {ftTypedBinary},
     {dbText} 10 {ftCursor}
    {$IFDEF SMForDelphi4}
     ,
     {dbText} 10 {ftFixedChar},
     {dbText} 10 {ftWideString},
     {dbBigInt} {4}16 {ftLargeint},
     {dbText} 10 {ftADT},
     {dbText} 10 {ftArray},
     {dbText} 10 {ftReference},
     {dbText} 10 {ftDataSet}

    {$IFDEF SMForDelphi5}
     ,
     {dbLongBinary} 11 {ftOraBlob},
     {dbLongBinary} 11 {ftOraClob},
     {dbText} 10 {ftVariant},
     {dbText} 10 {ftInterface},
     {dbText} 10 {ftIDispatch},
     {dbGUID} 15 {ftGuid}

    {$IFDEF SMForDelphi6}
     ,
     {dbText} 10 {ftTimeStamp},
     {dbText} 10 {ftFMTBcd}
    {$ENDIF}

    {$IFDEF SMForDelphi2006}
     ,
     {dbText} 10 {ftFixedWideChar},
     {dbMemo} 12 {ftWideMemo},
     {dbDate} 8 {ftOraTimeStamp},
     {dbText} 10 {ftOraInterval}
    {$ENDIF}

    {$ENDIF}
    {$ENDIF}
    );

var
  td, tf: OLEVariant;

  strText: string;
  colorUser: TColor;
  alignUser: TAlignment;
  fieldUser: TField;
  ct: TCellType;

  j, intCurColumn: Integer;
begin
  inherited;

  td := db.CreateTableDef(TableName, 0, '', '');
  for intCurColumn := 0 to Columns.Count-1 do
  begin
    with Columns[intCurColumn] do
    begin
      if Visible then
      begin
        with Columns[intCurColumn].Title do
        begin
          if (soUseFieldNameAsCaption in Options) then
            strText := FieldName
          else
            strText := Caption;
          colorUser := Color;
          fontUser.Assign(Font);
          alignUser := Alignment;
        end;

        if Assigned(OnGetCellParams) then
        begin
          ct := Columns[intCurColumn].DataType;
          OnGetCellParams(Self, nil, strText, fontUser, alignUser, colorUser, ct);
        end;

        case CharacterSet of
          csASCII_MSDOS: strText := UECode(True, strText);
          csEBCDIC: strText := EBCDICCode(strText);
        end;

        if (Columns[intCurColumn].ColumnKind = ckField) then
          fieldUser := SourceDataEngine.FindFieldByColumn(Columns[intCurColumn])
        else
          fieldUser := nil;
        if (Columns[intCurColumn].DataType in [ctMEMO, ctGraphic]) or
           (Assigned(fieldUser) and fieldUser.IsBlob) then
        begin
          if Assigned(fieldUser) and
             (fieldUser.DataType = ftMemo) then
            tf := td.CreateField(strText, 12{dbMemo}, 0) 
          else
          if Assigned(fieldUser) and
             (fieldUser.DataType = ftGraphic) then
            tf := td.CreateField(strText, 11{dbLongBinary}, 0)
          else
            tf := td.CreateField(strText, 9{dbBinary}{j}, 0)
        end
        else
        begin
          if (soFieldMask in Options) then
            j := arrMDBTypes[ftString]
          else
          if Assigned(fieldUser) then
            j := arrMDBTypes[fieldUser.DataType]
          else
            j := arrMDBTypes[CellType2FieldType(ct)];
          tf := td.CreateField(strText, j, Columns[intCurColumn].Width);

          if Assigned(fieldUser) and (fieldUser.DataType = ftBoolean) then
          begin
//            tf.Format := '"True";"False"';
//            v := tf.CreateProperty('Format', 10{dbText}, '"True";"False"');
//            tf.Properties.Append(v);
          end
        end;
        tf.Required := SourceDataEngine.RequiredByColumn(Columns[intCurColumn]);
{
        if Assigned(fieldUser) then
        begin
          if (soFieldMask in Options) then
            j := arrMDBTypes[ftString]
          else
            j := arrMDBTypes[fieldUser.DataType];
          if fieldUser.IsBlob then
          begin
            tf := td.CreateField(strText, 11, 0);
            tf.Attributes := dbVariableField
          end
          else
            tf := td.CreateField(strText, j, fieldUser.Size);
            tf.Required := fieldUser.Required;
        end
        else
        if (Columns[intCurColumn].ColumnKind = ckConstant) then
        begin
          if (Columns[intCurColumn].DataType in [ctMEMO, ctGraphic]) then
          begin
            tf := td.CreateField(Columns[intCurColumn].FieldName, 11, 0);
            tf.Attributes := dbVariableField
          end
          else
            tf := td.CreateField(strText,
                                 arrMDBTypes[CellType2FieldType(Columns[intCurColumn].DataType)],
                                 Columns[intCurColumn].Width);
            tf.Required := False;
        end
        else
        if (Columns[intCurColumn].ColumnKind = ckSysVar) then
        begin
          tf := td.CreateField(strText,
                               arrMDBTypes[CellType2FieldType(Columns[intCurColumn].DataType)],
                               Columns[intCurColumn].Width);
          tf.Required := False;
        end
        else
        begin
          tf := td.CreateField(strText,
                               arrMDBTypes[SourceDataEngine.DataTypeByColumn(Columns[intCurColumn])],
                               Columns[intCurColumn].Width);
          tf.Required := SourceDataEngine.RequiredByColumn(Columns[intCurColumn]);
        end;
}        td.Fields.Append(tf);
      end
    end
  end;
  db.TableDefs.Append(td);
end;

procedure TSMExportToAccess.OpenRecordset;
begin
  inherited;

  recordset := db.OpenRecordset(TableName, 1{db_OpenTable}, 0, 2 {db_Pessimistic});
end;

procedure TSMExportToAccess.AddNewRecord;
begin
  inherited;

  recordset.AddNew;
end;

procedure TSMExportToAccess.UpdateField(ACol: Integer; fld: TField; Value: string);
var
  curField, v: OLEVariant;
  i: Integer;

  P, pData: PChar;
begin
  inherited;

  curField := recordset.Fields[Statistic.CurrentCol];
  if VarIsNull(Value) or (VarToStr(Value) = '') then
    curField.Value := NULL
  else
  if ((Assigned(fld) and
      fld.IsBlob and
      (fld.DataType <> ftMemo))) or
     (not Assigned(fld) and (Columns[ACol].DataType in [ctMEMO, ctGraphic]))  then
  begin
    i := Length(Value);
//    if (i > curField.FieldSize) then
//      i := curField.FieldSize;
    v := VarArrayCreate([0, i-1], VarByte);
    P := VarArrayLock(v);
    pData := PChar(Value);
    Move(pData[0], P[0], i);
    VarArrayUnlock(v);
//    strText := inttostr(curfield.fieldname);
    curField.Value := v;
  end
  else
    curField.Value := Value;
end;

procedure TSMExportToAccess.PostRecord;
const
  dbUpdateRegular = 1;
begin
  inherited;

  recordset.Update(dbUpdateRegular, False);
end;

procedure TSMExportToAccess.FinilizeConnection;
begin
  if not VarIsEmpty(recordset) then
    recordset.Close;
  if not VarIsEmpty(db) then
    db.Close;
  access := UnAssigned;
end;

{$IFDEF SMForDelphi3 }
procedure GetIDsOfNamesFix(const Dispatch: IDispatch; Names: PChar;
  NameCount: Integer; DispIDs: PDispIDList);

  procedure Error;
  begin
    raise EOleError.CreateFmt(SNoMethod, [Names]);
  end;

type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..MaxDispArgs - 1] of PWideChar;
var
  N, SrcLen, DestLen: Integer;
  Src: PChar;
  Dest: PWideChar;
  NameRefs: PNamesArray;
  StackTop: Pointer;
  Temp: Integer;
begin
  Src := Names;
  N := 0;
  asm
    MOV  StackTop, ESP
    MOV  EAX, NameCount
    INC  EAX
    SHL  EAX, 2  // sizeof pointer = 4
    SUB  ESP, EAX
    LEA  EAX, NameRefs
    MOV  [EAX], ESP
  end;
  repeat
    SrcLen := StrLen(Src);
    DestLen := MultiByteToWideChar(0, 0, Src, SrcLen, nil, 0) + 1;
    asm
      MOV  EAX, DestLen
      ADD  EAX, EAX
      ADD  EAX, 3      // round up to 4 byte boundary
      AND  EAX, not 3
      SUB  ESP, EAX
      LEA  EAX, Dest
      MOV  [EAX], ESP
    end;
    if N = 0 then NameRefs[0] := Dest else NameRefs[NameCount - N] := Dest;
    MultiByteToWideChar(0, 0, Src, SrcLen, Dest, DestLen);
    Dest[DestLen-1] := #0;
    Inc(Src, SrcLen+1);
    Inc(N);
  until N = NameCount;
{
  Original Borland code

  Temp := Dispatch.GetIDsOfNames(GUID_NULL, NameRefs, NameCount,
    GetThreadLocale, DispIDs);
}
  Temp := Dispatch.GetIDsOfNames(GUID_NULL, NameRefs, NameCount,
    EnglishLocale, DispIDs);

  if Temp = DISP_E_UNKNOWNNAME then Error else OleCheck(Temp);
  asm
    MOV  ESP, StackTop
  end;
end;
{$ELSE}
procedure GetIDsOfNamesFix(Dispatch: IDispatch; Names: PChar;
  NameCount: Integer; DispIDs: PDispIDList);
var
  I, N: Integer;
  Ch: WideChar;
  P: PWideChar;
  NameRefs: array[0..MaxDispArgs - 1] of PWideChar;
  WideNames: array[0..1023] of WideChar;
begin
  I := 0;
  N := 0;
  repeat
    P := @WideNames[I];
    if N = 0 then NameRefs[0] := P else NameRefs[NameCount - N] := P;
    repeat
      Ch := WideChar(Names[I]);
      WideNames[I] := Ch;
      Inc(I);
    until Char(Ch) = #0;
    Inc(N);
  until N = NameCount;
{
  Original Borland code

  if Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
    LOCALE_SYSTEM_DEFAULT, DispIDs) <> 0 then
}
  if Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
       EnglishLocale, DispIDs) <> 0 then
    raise EOleError.CreateResFmt(SNoMethod, [Names]);
end;
{$ENDIF}

procedure VarDispInvokeFix(Result: PVariant; const Instance: Variant;
  CallDesc: PCallDesc; Params: Pointer); cdecl;
var
  Dispatch: {$IFDEF SMForDelphi3} Pointer {$ELSE} IDispatch {$ENDIF};
  DispIDs: array[0..MaxDispArgs - 1] of Integer;
begin
{$IFDEF SMForDelphi3}
  if TVarData(Instance).VType = varDispatch then
    Dispatch := TVarData(Instance).VDispatch
  else if TVarData(Instance).VType = (varDispatch or varByRef) then
    Dispatch := Pointer(TVarData(Instance).VPointer^)
  else
    raise EOleError.Create(SVarNotObject);
  GetIDsOfNamesFix(IDispatch(Dispatch), @CallDesc^.ArgTypes[CallDesc^.ArgCount],
    CallDesc^.NamedArgCount + 1, @DispIDs);
  if Result <> nil then VarClear(Result^);
  DispatchInvoke(IDispatch(Dispatch), CallDesc, @DispIDs, @Params, Result);
{$ELSE}
  Dispatch := VarToInterface(Instance);
  GetIDsOfNamesFix(Dispatch, @CallDesc^.ArgTypes[CallDesc^.ArgCount],
    CallDesc^.NamedArgCount + 1, @DispIDs);
  if Result <> nil then VarClear(Result^);
  DispInvoke(Dispatch, CallDesc, @DispIDs, @Params, Result);
{$ENDIF}
end;

initialization
//  VarDispProc := @VarDispInvokeFix;

end.

