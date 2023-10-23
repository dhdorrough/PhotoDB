{ SMExport suite
  TSMExportToXLS component: data export into XLS-file without MS Excel OLE

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2XLS;

interface

{$I sme.inc}

uses
  Classes, Windows, Graphics, DB, ActiveX, SysUtils, SME2Cell, ExportDS, SMEEngine;

type
  TSMExportXLSVersion = (exvExcel2, exvExcel3, exvExcel4, exvExcel5, exvExcel7, exvExcel8, exvExcel9);

  TXLSObjectKind = (okUnknown, okHyperlink, okNote, okPicture, okImage, okGraphic, okMergedCells);

  TXLSObject = class
  private
    { Private declarations }
    FObjectKind: TXLSObjectKind;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property ObjectKind: TXLSObjectKind read FObjectKind write FObjectKind default okUnknown;
  end;

  TXLSPicture = class(TXLSObject)
  private
    { Private declarations }
    FPicture: TPicture;
    FSize: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    function GetPictureSize: Integer;
    function GetPictureType: Integer;
    procedure WriteToStream(strm: TStream);
  published
    { Published declarations }
    property Picture: TPicture read FPicture write FPicture;
    property Size: Integer read FSize write FSize;
  end;

  TXLSMergeCells = class(TXLSObject)
  private
    { Private declarations }
    FFirstCol: Word;
    FFirstRow: Word;
    FLastCol: Word;
    FLastRow: Word;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property FirstCol: Word read FFirstCol write FFirstCol;
    property FirstRow: Word read FFirstRow write FFirstRow;
    property LastCol: Word read FLastCol write FLastCol;
    property LastRow: Word read FLastRow write FLastRow;
  end;

  TSMExportToXLS = class(TSMExportToCellFile)
  private
    { Private declarations }
    FTempFontStream: TMemoryStream;
    lstFonts: TList;
    lstObjects: TList;

    FVersion: TSMExportXLSVersion;
    FSheetIndex: Integer;

    APos: Integer;

    function GetFontFromTable(const Font: TFont): Integer;

    function GetFontStream: TMemoryStream;
    function GetXLSIndexByColor(color: TColor): Integer;
  protected
    { Protected declarations }
    OStream: IStream;
    pBuf: PByteArray;

    procedure WriteFormats;

    procedure InternalBeforeProcess; override;
    procedure InternalAfterProcess; override;
  public
    { Public declarations }
    procedure WriteRecordHeader(Stream: TStream; RecType, Size: Integer);

    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;
    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteFont(fn: TFont); virtual;
    procedure WriteRowStart(IsAddedTitle: Boolean); override;

    procedure WriteStyle(const Style: string);
    procedure WriteFormat(Index: Integer; Format: string; IsFormat: Boolean); virtual;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;

    procedure SaveToWorkbook;

    property TempFontStream: TMemoryStream read GetFontStream;

    property Version: TSMExportXLSVersion read FVersion write FVersion default exvExcel5;//4
//    SheetIndex: Integer read FSheetIndex write FSheetIndex default 0;
  published
    { Published declarations }
    property ExportStyle;
  end;

implementation

uses ComObj;

{XLS record types}
const
  XLSR_BOF              = $0009;
  XLSR_BIFF_EOF         = $000A;

  XLSR_BIT_BIFF2        = $0000;
  XLSR_BOF_BIFF2        = XLSR_BOF or XLSR_BIT_BIFF2;

  XLSR_BIT_BIFF3        = $0200;
  XLSR_BOF_BIFF3        = XLSR_BOF or XLSR_BIT_BIFF3;

  XLSR_BIT_BIFF4        = $0400;
  XLSR_BOF_BIFF4        = XLSR_BOF or XLSR_BIT_BIFF4;

  XLSR_BIT_BIFF5        = $0800;
  XLSR_BOF_BIFF5        = XLSR_BOF or XLSR_BIT_BIFF5;

  XLSR_BIT_BIFF9        = $1C00;
  XLSR_BOF_BIFF9        = XLSR_BOF or XLSR_BIT_BIFF9;

  XLSR_SHEET            = $0010;
  XLSR_CHART            = $0020;
  XLSR_MACROSHEET       = $0040;

  XLSR_DIMENSIONS       = $0200;
  XLSR_DOCTYPE_XLS      = $0010; //MS Excel 2.0 file format
  XLSR_LEN_RECORDHEADER = 4;
  XLSR_COLWIDTH         = $0024;
  XLSR_BLANK            = $01;
  XLSR_INTEGER          = $02;
  XLSR_NUMBER           = $03;
  XLSR_LABEL            = $04; {BIFF2}
  XLSR_LABEL3           = $0204; {BIFF3-7}
  XLSR_LABELSST         = $00FD; {BIFF8}
  XLSR_BOOLERR          = $05;

  XLSR_FONT             = $0031;
  XLSR_FONT3            = $0231;

  XLSR_FORMAT           = $001E;
  XLSR_FORMAT4          = $041E;
  XLSR_XF2              = $0043;
  XLSR_XF3              = $0243;
  XLSR_XF4              = $0443;
  XLSR_XF5              = $00E0;
  XLSR_STYLE            = $0293;

  XLSR_CODEPAGE         = $0042;
  XLSR_WRITEACCESS      = $005C;
  XLSR_FNGROUPCOUNT     = $009C;
  XLSR_EXTERNCOUNT      = $0016;
  XLSR_EXTERNSHEET      = $0017;

  {This record contains a boolean value determining whether the GUI should make backups of the file or not.}
  XLSR_BACKUP           = $0040;
  XLSR_HIDEOBJ          = $008D;
  XLSR_1904             = $0022;
  XLSR_PRECISION        = $000E;
  XLSR_REFRESHALL       = $01B7;
  {This record contains a boolean value determining whether to save values linked from external workbooks}
  XLSR_BOOKBOOL         = $00DA;

  XLSR_BOUNDSHEET       = $0085;
  XLSR_COUNTRY          = $008C;

  {width for columns in range
  Note: also specify collapse/level/hidden}
  XLSR_COLINFO          = $007D;

  {MSODRAWING (images, charts etc)}
  XLSR_OBJ              = $5D;
  XLSR_BITMAP           = $00E9; {background}
  XLSR_MSO_DRAWINGGROUP = $00EB;
  XLSR_MSO_DRAWING      = $00EC;

  XLSR_MSO_DGGCONTAINER    = $F000;
  XLSR_MSO_BSTORECONTAINER = $F001;
  XLSR_MSO_DGCONTAINER     = $F002;
  XLSR_MSO_SPGRCONTAINER   = $F003;
  XLSR_MSO_SPCONTAINER     = $F004;
  XLSR_MSO_DGG             = $F006;
  XLSR_MSO_BSE             = $F007;
  XLSR_MSO_DG              = $F008;
  XLSR_MSO_SPGR            = $F009;
  XLSR_MSO_SP              = $F00A;
  XLSR_MSO_OPT             = $F00B;
  XLSR_MSO_CLIENTTEXTBOX   = $F00D;
  XLSR_MSO_CLIENTANCHOR    = $F010;
  XLSR_MSO_CLIENTDATA      = $F011;
  XLSR_MSO_BLIP            = $F018;
  XLSR_MSO_SPLITMENUCOLORS = $F11E;


  MSO_SizeOf = 2*SizeOf(Word)+SizeOf(dWord);
  MSO_DGG_SizeOf = 6*SizeOf(dWord);
  MSO_BSE_SizeOf = 18+SizeOf(Word)+3*SizeOf(dWord)+2+SizeOf(Word);
  MSO_OPTValue_SizeOf = SizeOf(Word)+SizeOf(dWord);
  MSO_SplitMenuColors_SizeOf = 4*SizeOf(dWord);

  Skip_BitmapHeader_Size = 4*SizeOf(Word) + 6; {=SizeOf(TBitmapFileHeader)}

{ TXLSPicture }
constructor TXLSPicture.Create;
begin
  inherited;

  FPicture := TPicture.Create;
end;

destructor TXLSPicture.Destroy;
begin
  FPicture.Free;
  FPicture := nil;

  inherited;
end;

procedure TXLSPicture.WriteToStream(strm: TStream);
var
  p: Pointer;
  intSize: Integer;
begin
  {TODO: for Bitmap we must skip the header (Skip_BitmapHeader_Size)}
  if Assigned(Picture.Bitmap) then
  begin
    intSize := GetPictureSize;
    GetMem(p, intSize);
    Move(Picture, p^, intSize);
    strm.Write(p^, intSize);
    FreeMem(p)
  end
  else
  if Assigned(Picture.Metafile) then
    Picture.Metafile.SaveToStream(strm);
end;

function TXLSPicture.GetPictureSize: Integer;
begin
  if Assigned(Picture) then
  begin
    if Assigned(Picture.Bitmap) then
      Result := Size - Skip_BitmapHeader_Size
    else
      Result := Size
  end
  else
    Result := 0
end;

function TXLSPicture.GetPictureType: Integer;
begin
  {picture type
     0: Undefined
     1: Undefined2
     2: WMF
     3: EMF
     4: PICT
     5: JPEG
     6: PNG
     7: DIB
  }
  if not Assigned(Picture) or Picture.Graphic.Empty then
    Result := 0
  else
  if Assigned(Picture.Bitmap) then
    Result := 7
  else
  if Assigned(Picture.Metafile) then
  begin
    if Picture.Metafile.Enhanced then
      Result := 3
    else
      Result := 2
  end
  else
    Result := 0
end;

{ TSMExportToXLS }
constructor TSMExportToXLS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teXLS;
  FVersion := exvExcel5;//exvExcel4{5};
  FSheetIndex := 0;
end;

function TSMExportToXLS.Extension: string;
begin
  Result := '.XLS'
end;

function TSMExportToXLS.GetFontStream: TMemoryStream;
begin
  if not Assigned(FTempFontStream) then
    FTempFontStream := TMemoryStream.Create;
  Result := FTempFontStream;
end;

procedure TSMExportToXLS.WriteFormats;
begin
  {add default internal formats
  PS: http://support.microsoft.com/default.aspx?scid=%2Fservicedesks%2Fbin%2Fkbsearch.asp%3FArticle%3D147942}
  WriteFormat($00, 'General', True); {general}
  WriteFormat($01, '0', True); {decimal}
  WriteFormat($02, '0.00', True); {decimal}
  WriteFormat($03, '#,##0', True); {decimal}
  WriteFormat($04, '#,##0.00', True); {decimal}
  WriteFormat($05, '($#,##0_);($#,##0)', True); {currency $33,333}
  WriteFormat($06, '($#,##0_);[Red]($#,##0)', True);

  case CurrencyFormat of
    0: {'$1'}
       WriteFormat($07, CurrencyString + '00.00', True);
    2: {'$ 1'}
       WriteFormat($07, CurrencyString + ' 00.00', True);
    3: {'1 $'}
       WriteFormat($07, '00.00 ' + CurrencyString, True);
  else
//    1: {'1$'}
    WriteFormat($07, '00.00' + CurrencyString, True);
  end;
//  WriteFormat('($#,##0.00_);($#,##0.00)', True); {currency $33,333.33}

//  WriteFormat('($#,##0.00_);[Red]($#,##0.00)', True);
  WriteFormat($08, '$#,##0.00_);[Red]($#,##0.00)', True);

  WriteFormat($09, '0%', True); {percent}
  WriteFormat($0A, '0.00%', True); {percent}
  WriteFormat($0B, '0.00E+00', True); {scientific}
  WriteFormat($0C, '# ?/?', True); {fraction}
  WriteFormat($0D, '# ??/??', True); {fraction}
  WriteFormat($0E, ShortDateFormat{'m/d/yy'}, True); {date}
  WriteFormat($0F, 'd-mmm-yy', True); {date}
  WriteFormat($10, 'd-mmm', True); {date}
  WriteFormat($11, 'mmm-yy', True); {date}
  WriteFormat($12, ShortTimeFormat{'h:mm AM/PM'}, True); {time}
  WriteFormat($13, 'h:mm:ss AM/PM', True); {time}
  WriteFormat($14, 'h:mm', True); {time}
  WriteFormat($15, 'h:mm:ss', True); {time}
  WriteFormat($16, ShortDateFormat{'m/d/yy h:mm'}, True); {date/time}
  WriteFormat($25, '(#,##0_);(#,##0)', True); {currency}
  WriteFormat($26, '(#,##0_);[Red](#,##0)', True); {currency}
  WriteFormat($27, '(#,##0.00_);(#,##0.00)', True); {currency}
  WriteFormat($28, '(#,##0.00_);[Red](#,##0.00)', True); {currency}
  WriteFormat($29, '_(* #,##0_);_(* (#,##0);_(* "-"_);_(@_)', True);
  WriteFormat($2A, '_($* #,##0_);_($* (#,##0);_($* "-"_);_(@_)', True);
  WriteFormat($2B, '_(* #,##0.00_);_(* (#,##0.00);_(* "-"??_);_(@_)', True);
  WriteFormat($2C, '_($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)', True);
  WriteFormat($2D, 'mm:ss', True); {time}
  WriteFormat($2E, '[h]:mm:ss', True); {time}
  WriteFormat($2F, 'mm:ss.0', True); {time}
  WriteFormat($30, '##0.0E+0', True); {scientific}
  WriteFormat($31, '@', True); {text}


{ http://support.microsoft.com/directory/article.asp?ID=KB;EN-US;Q147942&LN=EN-US&rnk=3&SD=msdn&FR=0&qry=BIFF&src=DHCS_MSPSS_msdn_SRCH&SPR=MSALL&
Index     Format String
-------------------------------------
0x00      General
0x01      0
0x02      0.00
0x03      #,##0
0x04      #,##0.00
0x05      ($#,##0_);($#,##0)
0x06      ($#,##0_);[Red]($#,##0)
0x07      ($#,##0.00_);($#,##0.00)
0x08      ($#,##0.00_);[Red]($#,##0.00)
0x09      0%
0x0a      0.00%
0x0b      0.00E+00
0x0c      # ?/?
0x0d      # ??/??
0x0e      m/d/yy
0x0f      d-mmm-yy
0x10      d-mmm
0x11      mmm-yy
0x12      h:mm AM/PM
0x13      h:mm:ss AM/PM
0x14      h:mm
0x15      h:mm:ss
0x16      m/d/yy h:mm
0x25      (#,##0_);(#,##0)
0x26      (#,##0_);[Red](#,##0)
0x27      (#,##0.00_);(#,##0.00)
0x28      (#,##0.00_);[Red](#,##0.00)
0x29      _(* #,##0_);_(* (#,##0);_(* "-"_);_(@_)
0x2a      _($* #,##0_);_($* (#,##0);_($* "-"_);_(@_)
0x2b      _(* #,##0.00_);_(* (#,##0.00);_(* "-"??_);_(@_)
0x2c      _($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)
0x2d      mm:ss
0x2e      [h]:mm:ss
0x2f      mm:ss.0
0x30      ##0.0E+0
0x31      @
}

  {write default extended formats (XF) for workbook}

  {write default styles for workbook}
//  WriteStyle(#$10#$80#$04#$FF); {currency}
//  WriteStyle(#$11#$80#$07#$FF); {currency[0]}
//  WriteStyle(#$00#$80#$00#$FF); {general}
//  WriteStyle(#$12#$80#$05#$FF); {percent}
//  WriteStyle(#$13#$80#$03#$FF); {finance}
//  WriteStyle(#$14#$80#$06#$FF); {finance[0]}
end;

procedure TSMExportToXLS.WriteFileBegin;
var
  Buffer: array[0..4] of Word;
begin
  inherited;

  case FVersion of
    exvExcel2: WriteRecordHeader(OutputStream, XLSR_BOF_BIFF2, 6);
    exvExcel3: WriteRecordHeader(OutputStream, XLSR_BOF_BIFF3, 6);
    exvExcel4: WriteRecordHeader(OutputStream, XLSR_BOF_BIFF4, 6);
    exvExcel5: WriteRecordHeader(OutputStream, XLSR_BOF_BIFF5, 6);
    exvExcel9: WriteRecordHeader(OutputStream, XLSR_BOF_BIFF9, 6);
  end;

  Buffer[0] := 0;
  Buffer[1] := XLSR_DOCTYPE_XLS;
  Buffer[2] := $0DBB; {build identifier}
  OutputStream.Write(Buffer, 6);

{  Buffer[0] := 0;
  Buffer[1] := XLSR_SHEET;
  Buffer[2] := 0;
  Buffer[3] := 0;
  OutputStream.Write(Buffer, 8);
}
  WriteFormats;
end;

procedure TSMExportToXLS.WriteFileEnd;
var
  i: Integer;
begin
  inherited;

  {flush pictures}
  if Assigned(lstObjects) and
     (lstObjects.Count > 0) then
  begin
    {MSO_SPContainer
       MSO_SP
       MSO_OPT
       MSO_ClientAnchor
       MSO_ClientData
     MSO_DRAWING
       MSO_DGContainer
       object write
         OBJ
         CMO
         CF
         PIOGRBIT
         END
    }
    {flush MSO DRAWING record}
//    WriteRecordHeader(TempFontStream, XLSR_MSO_DRAWING, ?);

    {SP Container}
//    WriteMSOHeader(XLSR_MSO_SPCONTAINER, $0F, $00, ?);
  end;

  {flush fonts}
  if Assigned(lstFonts) then
  begin
    for i := 0 to lstFonts.Count-1 do
      WriteFont(TObject(lstFonts[i]) as TFont);
//    for i := 0 to lstFonts.Count-1 do
//      WriteFormat(Chr(i+1) + #0 + Chr(i+1+$40) + #0, False);
  end;

  if Assigned(lstFonts) then
  begin
    for i := lstFonts.Count-1 downto 0 do
      TFont(lstFonts[i]).Free;
    lstFonts.Clear;
  end;

  {write MSODrawing if any pictures stored}
  if Assigned(lstObjects) and
     (lstObjects.Count > 0) then
  begin
    {Escher file stream (Office Drawing Layer)
     MSO_DRAWING:
       MSO_DGGContainer
         MSO_DGG
         MSO_CLSID (don't need)
         MSO_OPT
         MSO_ColorMRU (don't need)
         MSO_SplitMenuColors
         MSO_BstoreContainer
           MSO_BSE
           MSO_Blip
       MSO_DGContainer (don't need)
         MSO_DG
         MSO_RegroupItems
         MSO_ColorScheme
         MSO_SpgrContainer
         MSO_SpContainer
           MSO_Spgr
           MSO_Sp
           MSO_OPT
           MSO_TextBox
           MSO_ClientTextBox
           MSO_Anchor
           MSO_ChildAnchor
           MSO_ClientAnchor
           MSO_ClientData
           MSO_OLEObject
           MSO_DeletedPspl
         MSO_SolverContainer
           MSO_ConnectorRule
           MSO_AlignRule
           MSO_ArcRule
           MSO_ClientRule
           MSO_CalloutRule
       MSO_Selection (don't need)
}

    for i := lstObjects.Count-1 downto 0 do
      TXLSObject(lstObjects[i]).Free;
    lstObjects.Clear;
  end;

  if Assigned(TempFontStream) and
     (TempFontStream.Size > 0) then
    TempFontStream.SaveToStream(OutputStream);

  WriteRecordHeader(OutputStream, XLSR_BIFF_EOF, 0);
end;

procedure TSMExportToXLS.WriteStyle(const Style: string);
var
  intLength: Integer;
begin
  intLength := Length(Style);
  WriteRecordHeader(OutputStream, XLSR_STYLE, intLength);

  if Assigned(OutputStream) and (intLength > 0) then
    OutputStream.Write(Style[1], intLength);
end;

procedure TSMExportToXLS.WriteFormat(Index: Integer; Format: string; IsFormat: Boolean);
var
  intLength, APos: Integer;
begin
  intLength := Length(Format);

  if IsFormat then
  begin
{    if Assigned(OStream) then
    begin
      WriteRecordHeader(OutputStream, XLSR_XF5, intLength+3)
    end
    else
}    begin
      if (FVersion >= exvExcel4) then
        WriteRecordHeader(OutputStream, XLSR_FORMAT4, intLength+3)
      else
        WriteRecordHeader(OutputStream, XLSR_FORMAT, intLength+1)
    end
  end
  else
    WriteRecordHeader(OutputStream, XLSR_XF2, intLength);

  if Assigned(OutputStream) then
  begin
    if IsFormat then
    begin
      if (FVersion >= exvExcel4) then
        OutputStream.Write(Index, 2);
      OutputStream.Write(intLength, 1);
    end;
    OutputStream.Write(Format[1], intLength);
  end
  else
  begin
    if IsFormat then
    begin

      OleCheck(OStream.Write(@Index, 2, @APos));
      OleCheck(OStream.Write(@intLength, 1, @APos));
      OleCheck(OStream.Write(PChar(Format), intLength, @APos));
    end
  end
end;

procedure TSMExportToXLS.WriteDimensions(intRowCount, intColCount: Integer);
var
  Buffer: array[0..4] of Word;
begin
  inherited;

  if intColCount > 255 then
    intColCount := 255;

  if Assigned(OStream) then
    WriteRecordHeader(nil, XLSR_DIMENSIONS, 10)
  else
    WriteRecordHeader(OutputStream, XLSR_DIMENSIONS, 10);
  Buffer[0] := 0;
  Buffer[1] := intRowCount;
  Buffer[2] := 0;
  Buffer[3] := intColCount-1;
  Buffer[4] := 0;
  if Assigned(OStream) then
    OleCheck(OStream.Write(@Buffer, 10, @APos))
  else
    OutputStream.Write(Buffer, 10);
end;

procedure TSMExportToXLS.WriteRecordHeader(Stream: TStream; RecType, Size: Integer);
var
  Buffer: array[0..1] of Word;
  Pos: Integer;
begin
  Buffer[0] := RecType;
  Buffer[1] := Size;

  if Assigned(Stream) then
  begin
//  if (FVersion = exvExcel4) then
    Stream.Write(Buffer, SizeOf(Buffer))
  end
  else
  if Assigned(OStream) then
  begin //exvExcel7
    OleCheck(OStream.Write(@Buffer, SizeOf(Buffer), @Pos));
  end;
end;

procedure TSMExportToXLS.WriteColWidth(intCurColumn, intColWidth: Integer);
var
  bufCols: array[0..1] of Byte;
begin
  if (intColWidth < 1) then exit;
  if (intCurColumn > 255) then exit;

  if (intColWidth = 10) then
    intColWidth := 12;

  bufCols[0] := intCurColumn;
  bufCols[1] := intCurColumn;

  WriteRecordHeader(OutputStream, XLSR_COLWIDTH, 4);
  OutputStream.Write(bufCols, 2);
  intColWidth := 256*intColWidth;
  OutputStream.Write(intColWidth, 2);
end;

procedure TSMExportToXLS.WriteRowStart(IsAddedTitle: Boolean);
begin
  inherited;

  if Statistic.CurrentRow > 65536 then
    raise Exception.Create('MS Excel spreadsheet can not contain more than 65536 rows');
end;

procedure TSMExportToXLS.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
const
  arrAlign: array[TAlignment] of Byte = ($01, $03, $02);
  BorderLeft = $04;
  BorderRight = $08;
  BorderTop = $10;
  BorderBottom = $20;
var
  Attribute: array[0..2] of Byte;
  Buffer: array[0..1] of Word;
  Size: Word;
  aInt, intError: Integer;
  aDbl: Double;
  Data: Pointer;
  strValue: ShortString;

  pic: TXLSPicture;
  bmp: TBitmap;
  strmPic: TStringStream;
begin
  if (ACol > 255) then exit;

  inherited;

  if (Assigned(fld) and
     fld.IsBlob and
     not fld.IsNull and
     (TBlobField(fld).BlobType = ftGraphic)) or
    (CellType = ctGraphic) then
  begin
    pic := TXLSPicture.Create;
    try
      if Assigned(fld) then
      begin
        pic.Picture.Assign(TBlobField(fld));
        pic.Size := Length(AString);
      end
      else
      begin
        {we must load bitmap from string}
        strmPic := TStringStream.Create(AString);
        bmp := TBitmap.Create;
        try
          strmPic.Seek(0, soFromBeginning);
          bmp.LoadFromStream(strmPic);
          pic.Picture.Assign(bmp);
          pic.Size := strmPic.Size;
        finally
          bmp.Free;
          strmPic.Free
        end
      end;

      lstObjects.Add(pic);
    except
    end;
    exit;
  end;

  Buffer[0] := ARow;
  Buffer[1] := ACol;
  if (soFieldMask in Options) then
    CellType := ctString
  else
  begin
    if (AString = '') then
      CellType := ctBlank
    else
      if (CellType = ctInteger) then
      begin
        aInt := StrToIntDef(AString, 0);
        if (aInt > $FF) or (aInt < 0) then
          CellType := ctDouble;
      end
  end;
  if not (CellType in [ctBlank, ctInteger, ctDouble, ctDateTime, ctDate, ctCurrency, ctTime]) then
    strValue := AString;

  if (CellType = ctBlank) and
     not (soExportBlankValues in Options) then exit;

  Size := 0;
  Data := nil;
  case CellType of
    ctBlank: WriteRecordHeader(OutputStream, XLSR_BLANK, 7);

    ctInteger,
    ctDate: begin
              WriteRecordHeader(OutputStream, XLSR_INTEGER, 9);
              Size := 2;

              case CellType of
                ctInteger: begin
                             Val(AString, aInt, intError);
                             if intError <> 0 then
                               aInt := 0;
                           end;
                ctDate: aInt := Trunc(StrToDate(AString));
              end;
              Data := @aInt;
            end;

    ctDouble,
    ctCurrency,
    ctTime,
    ctDateTime: begin
                  WriteRecordHeader(OutputStream, XLSR_NUMBER, 15);
                  Size := 8;
                  aDbl := 0;
                  if AString <> '' then
                  try
                    case CellType of
                      ctDouble,
                      ctCurrency: aDbl := StrToFloat(AString);
                      ctDateTime: aDbl := StrToDateTime(AString);
                      ctTime: aDbl := StrToTime(AString);
                    end;
                  except
                  end;
                  Data := @aDbl;
                end;

  else // ctString
    WriteRecordHeader(OutputStream, XLSR_LABEL, Length(strValue) + 8);
//    Exit;
  end;

  if Assigned(OStream) then
    OleCheck(OStream.Write(@Buffer, SizeOf(Buffer), @APos))
  else
    OutputStream.Write(Buffer, SizeOf(Buffer));
  Attribute[0] := 0;
  Attribute[1] := 0;
  Attribute[2] := 0;

  if (soColorsFonts in Options) then
  begin
    intError := GetFontFromTable(Font);
    if (intError > -1) then
      Attribute[1] := $40*intError
    else
      intError := 0;
  end
  else
    intError := 0;

  case CellType of
//    ctInteger: Attribute[1] := Attribute[1] + 1;
    ctDate: Attribute[1] := Attribute[1] + {1}14;
    ctTime: Attribute[1] := Attribute[1] + {2}18;
    ctDateTime: Attribute[1] := Attribute[1] + {3}22;
    ctCurrency: Attribute[1] := Attribute[1] + {4}{8}7;
    ctDouble: Attribute[1] := Attribute[1] + {0}2;
    ctInteger: Attribute[1] := Attribute[1] + {0}1;
  else
//    if intError > 0 then
//      Attribute[1] := Attribute[1] + 6{49} + intError;//5
  end;

  Attribute[2] := {BorderBottom or BorderTop or BorderRight or BorderLeft + }arrAlign[al];

{111  Attribute[1] := 0;
111  Attribute[2] := 0;
}
  if Assigned(OStream) then
    OleCheck(OStream.Write(@Attribute, SizeOf(Attribute), @APos))
  else
    OutputStream.Write(Attribute, SizeOf(Attribute));

  if (CellType in [ctBlank, ctInteger, ctDouble, ctDateTime, ctDate, ctCurrency, ctTime]) then
  begin
    if Size > 0 then
    begin
      if Assigned(OStream) then
        OleCheck(OStream.Write(Data, Size, @APos))
      else
        OutputStream.Write(Data^, Size);
    end
  end
  else
  begin
    if Assigned(OStream) then
    begin
      Size := Length(AString);
      Move(AString[1], pBuf[0], Size);
      OleCheck(OStream.Write(@Size, 1, @APos));
      OleCheck(OStream.Write(pBuf, Size, @APos))
//      OleCheck(OStream.Write(Pointer(AString), Length(strValue)+1, @APos))
    end
    else
      OutputStream.Write(strValue, Length(strValue) + 1);
  end
end;

const
  wdAuto = $00000000;
  wdBlack = $00000001;

function TSMExportToXLS.GetXLSIndexByColor(color: TColor): Integer;
const
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
  case color of
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
    Result := wdAuto;
  end;
end;

procedure TSMExportToXLS.WriteFont(fn: TFont);
var
  intFontLength, intStyle, d: Word;
  b: Byte;
  str: string;
begin
  str := fn.Name;
  intFontLength := Length(str);

  if (FVersion in [exvExcel3, exvExcel4]) then
    WriteRecordHeader(TempFontStream, XLSR_FONT3, intFontLength + 7)
  else
    WriteRecordHeader(TempFontStream, XLSR_FONT, intFontLength + 5);
  intStyle := fn.Size*20;
  TempFontStream.Write(intStyle, 2);

  intStyle := 0;
  if (fsBold in fn.Style) then
    intStyle := intStyle or 1;
  if (fsItalic in fn.Style) then
    intStyle := intStyle or 2;
  if (fsUnderline in fn.Style) then
    intStyle := intStyle or 4;
  if (fsStrikeout in fn.Style) then
    intStyle := intStyle or 8;
  TempFontStream.Write(intStyle, 2);

  if (FVersion in [exvExcel3, exvExcel4]) then
  begin
    d := GetXLSIndexByColor(ColorToRGB(fn.Color));
    if d in [wdAuto, wdBlack] then
    begin
      b := $FF;
      TempFontStream.Write(b, 1);
      b := $7F;
      TempFontStream.Write(b, 1);
    end
    else
      TempFontStream.Write(d, 2);
  end;

  TempFontStream.Write(intFontLength, 1);
  TempFontStream.Write(str[1], intFontLength);
end;

function TSMExportToXLS.GetFontFromTable(const Font: TFont): Integer;
var
  i, j: Integer;
  fn: TFont;
begin
  j := -1;
  if Assigned(Font) then
  begin
    {add a default font}
    if (lstFonts.Count = 0) then
    begin
      fn := TFont.Create;
      fn.Name := 'Arial';
      fn.Size := 8;
      lstFonts.Add(fn)
    end;

    for i := 0 to lstFonts.Count-1 do
    begin
      fn := TFont(lstFonts[i]);
      if (fn.Name = Font.Name) and
         (fn.Style = Font.Style) and
         (fn.Size = Font.Size) {and
         (fn.Color = Font.Color)} then
      begin
        j := i;
        break;
      end;
    end;
    if (j = -1) and (lstFonts.Count < 4) then
    begin
      fn := TFont.Create;
      fn.Assign(Font);
      j := lstFonts.Add(fn)
    end
  end;
  Result := j
end;

procedure TSMExportToXLS.InternalBeforeProcess;
begin
  inherited;

  lstFonts := TList.Create;
  lstObjects := TList.Create;
end;

procedure TSMExportToXLS.InternalAfterProcess;
begin
  lstFonts.Free;
  lstFonts := nil;

  lstObjects.Free;
  lstObjects := nil;

  if Assigned(FTempFontStream) then
  begin
    FTempFontStream.Free;
    FTempFontStream := nil;
  end;

  inherited;
end;

procedure TSMExportToXLS.SaveToWorkbook;

  procedure WriteBuffer(buf: PByteArray; Len: Word);
  begin
    if Len > 0 then
      OleCheck(OStream.Write(buf, Len, @APos));
  end;

var
  Storage: IStorage;
  WC: PWideChar;

  Buffer: array[0..5] of Word;
  i, j, intLen: Integer;
  d: Word;

  strSheetName: string;
begin
  OutputStream := nil;

  if FVersion < exvExcel5 then
    raise Exception.Create('Version number for MS Excel workbook must be greater than 5');

  GetMem(WC, 512);
  try
    OleCheck(StgCreateDocfile(StringToWideChar(FileName, WC, 512),
             STGM_DIRECT or STGM_READWRITE or STGM_CREATE or STGM_SHARE_EXCLUSIVE, 0, Storage));
  finally
    FreeMem(WC);
  end;
  if FVersion = exvExcel5 then
    WC := 'Book'
  else
    WC := 'Workbook';
  OleCheck(Storage.CreateStream(WC,
                                STGM_DIRECT or STGM_WRITE or STGM_CREATE or STGM_SHARE_EXCLUSIVE, 0, 0, OStream));

  if FVersion > exvExcel7 then
    pBuf := AllocMem(8224)
  else
    pBuf := AllocMem(2080);

  {write BOF record for workbook}
  if FVersion = exvExcel5 then
  begin
    WriteRecordHeader(nil, XLSR_BOF_BIFF5, 6);
    {version number}
    Buffer[0] := $0500;
  end
  else
  begin
    WriteRecordHeader(nil, XLSR_BOF_BIFF9, 6);
    {version number}
    Buffer[0] := $0600;
  end;
  {stream type (workbook)}
  Buffer[1] := $0005;
  {build identifier}
  Buffer[2] := 0;
  {build year}
  Buffer[3] := 0;
  WriteBuffer(@Buffer, 6{SizeOf(Buffer)});

  if FVersion > exvExcel5 then
  begin
    {history flags}
    Buffer[0] := 0;
    Buffer[1] := 0;

    {low biff}
    Buffer[2] := $0006;
    Buffer[3] := 0;
    WriteBuffer(@Buffer, SizeOf(Buffer));
  end;

  if FVersion = exvExcel5 then
  begin
    FillChar(pBuf^, 31, ' ');
    pBuf[0] := 0;

    WriteRecordHeader(nil, XLSR_WRITEACCESS, 32);
    WriteBuffer(pBuf, 32);

    {codepage}
    WriteRecordHeader(nil, XLSR_CODEPAGE, 2);
    Buffer[0] := GetACP();
    OleCheck(OStream.Write(@Buffer, 2, @APos));

    WriteRecordHeader(nil, XLSR_FNGROUPCOUNT, 2);
    Buffer[0] := $000E;
    OleCheck(OStream.Write(@Buffer, 2, @APos));

    WriteRecordHeader(nil, XLSR_BACKUP, 2);
    d := 0;
    WriteBuffer(@d, 2);

    WriteRecordHeader(nil, XLSR_HIDEOBJ, 2);
    d := 0;
    WriteBuffer(@d, 2);

    WriteRecordHeader(nil, XLSR_1904, 2);
    d := 0;
    WriteBuffer(@d, 2);

    WriteRecordHeader(nil, XLSR_PRECISION, 2);
    d := 1;
    WriteBuffer(@d, 2);

    WriteRecordHeader(nil, XLSR_REFRESHALL, 2);
    d := 0;
    WriteBuffer(@d, 2);

    WriteRecordHeader(nil, XLSR_BOOKBOOL, 2);
    d := 0;
    WriteBuffer(@d, 2);

    {TODO: fonts (FONT)}

    WriteFormats;

    {TODO: write extended formats (XF)}
    {TODO: styles (STYLE)}

    for i := 0 to 1{Sheets.Count}-1 do
    begin
      strSheetName := 'Sheet' + IntToStr(i+1); {Sheets[i].Caption}
      intLen := Length(strSheetName);

      {BoundSheet}
      if (FVersion > exvExcel5) then
      begin
        strSheetName := #0 + strSheetName;
        Move(Pointer(strSheetName)^, pBuf[0], intLen+1);
        WriteRecordHeader(nil, XLSR_BOUNDSHEET, 7+intLen+1);
      end
      else
      begin
        Move(Pointer(strSheetName)^, pBuf[0], intLen);
        WriteRecordHeader(nil, XLSR_BOUNDSHEET, 7+intLen);
      end;

      {position for BOF record}
      Buffer[0] := 0;
      Buffer[1] := 0;

      {options for sheet}
      Buffer[2] := 0;
      OleCheck(OStream.Write(@Buffer, 6, @APos));

      {length for sheet caption}
      OleCheck(OStream.Write(@intLen, 1, @APos));

      {caption string}
      if (FVersion > exvExcel5) then
        WriteBuffer(pBuf, intLen+1)
      else
        WriteBuffer(pBuf, intLen)
    end;


    WriteRecordHeader(nil, XLSR_COUNTRY, 2*SizeOf(Word));
    Buffer[0] := 1; { default country }
    Buffer[1] := 1; { Country from MS Windows }
    OleCheck(OStream.Write(@Buffer, 2*SizeOf(Word), @APos));

    {write EOF-record}
    WriteRecordHeader(nil, XLSR_BIFF_EOF, 0);

    {write all sheet cells}
    for i := 0 to 1{Sheets.Count}-1 do
    begin
      {write BOF-record for worksheet}
      if (FVersion = exvExcel5) then
      begin
        WriteRecordHeader(nil, XLSR_BOF_BIFF5, 6);
        {version number}
        Buffer[0] := $0500;
      end
      else
      begin
        WriteRecordHeader(nil, XLSR_BOF_BIFF9, 6);
        {version number}
        Buffer[0] := $0600;
      end;
      {stream type (worksheet)}
      Buffer[1] := $0010;
      {build identifier}
      Buffer[2] := 0;
      {build year}
      Buffer[3] := 0;
      WriteBuffer(@Buffer, 6{SizeOf(Buffer)});

      {write widths for columns in range}
      for j := 0 to 3{Sheet[i].Columns.Count}-1 do
      begin
        WriteRecordHeader(nil, XLSR_COLINFO, 12);
        Buffer[0] := j; //Col1
        Buffer[1] := j; //Col2
        Buffer[2] := 200{Sheet[i].Columns[j].ColWidth};
        Buffer[3] := 0; //Format index
        Buffer[4] := 0; //options (collapse/level/hidden)
        Buffer[5] := 0; //reserved
        WriteBuffer(@Buffer, 12);
      end;

      {write dimensions for sheet cells}
      WriteDimensions(2{Sheet[i].Rows.Count}, 3{Sheet[i].Columns.Count});

      {write cell values}
      WriteData(nil, ctString, 0, 0, 'aaaaa', taLeftJustify, nil, clWhite);
      WriteData(nil, ctDouble, 1, 0, '12,45', taRightJustify, nil, clWhite);
      WriteData(nil, ctInteger, 1, 1, '2', taRightJustify, nil, clWhite);
      WriteData(nil, ctString, 0, 1, 'bbbbbb', taLeftJustify, nil, clWhite);

      {write EOF-record}
      WriteRecordHeader(nil, XLSR_BIFF_EOF, 0);
    end
  end;
end;

end.

