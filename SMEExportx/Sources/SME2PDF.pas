{ SMExport suite
  TSMExportToPDF component: data export into Adobe Acrobat Document (PDF-file)

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2PDF;

interface

{$I sme.inc}

uses
  Classes, Windows, Graphics, DB, ExportDS, SME2Cell, SMEEngine
  {$IFDEF SME_USE_ZLIB} , ZLib {$ENDIF}
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TPDFFilterDecode = (fdASCIIHex, fdASCII85, fdLZW, fdDCT, fdFlate);

const
  arrPDFFilter: array[TPDFFilterDecode] of string = ('ASCIIHexDecode', 'ASCII85Decode', 'LZWDecode', 'DCTDecode', 'FlateDecode');

type
  TPDFFont = class
  public
    RefIndex: string;
    BaseFont: string;
    FontName: string;
    Name: TFontName;
    CharSet: TFontCharset;
    Style: TFontStyles;

    IsTrueType: Boolean;
    IsEmbeddable: Boolean;
    Flags: Integer;

    procedure FontParameters(Font: TFont);
    function GetFontName: string;
    function IsStandardFont: Boolean;
    function EncodingString: string;
  end;

  TSMExportToPDF = class;

  TPDFFonts = class(TList)
  private
    function FontFamily(Font: TFont; Charset: TFontCharset): string;
    function GetFontCharset(AValue: TFontCharset): TFontCharset;

    function GetItemByFont(const BaseFont: string; Charset: TFontCharset): Integer;

    function FontFromTable(PDFEngine: TSMExportToPDF; Font: TFont): Integer;
    procedure WriteFontDifferences(PDFEngine: TSMExportToPDF; fnt: TPDFFont);
  public
    function GetFont(Index: Integer): TPDFFont;

    procedure Clear;
         {$IFDEF VER110} override;
         {$ELSE}
           {$IFDEF SMForDelphi4} override;
           {$ENDIF}
         {$ENDIF}

    procedure WriteFonts(PDFEngine: TSMExportToPDF);
  end;

  TSMExportToPDF = class(TSMExportToCellFile)
  private
    { Private declarations }
    arrColWidths: Variant;

    MaxKids: Integer;
    FLastUsedRefIndex: Integer;

    CrossRef: TStringList;
    tblFont: TPDFFonts;
    tblImage: TStrings;
    lstPages: TStrings;

    intTotalPages,
    StreamSize,
    FilePos,
    intLastMergeCol: Integer;

    BodyRefIndex, ResRefIndex,
    StreamRefIndex, ProcRefIndex: string;

    intCurY, intCurRowHeight, intCurYPos, FOneSheetWidth, PageWidth, PageHeight: Integer;

    {to write in memory stream or in output stream}
    IsMemStream: Boolean;

    IsDataStarted: Boolean;

    function GetRowTop(ARow: Integer): Integer;
  protected
    { Protected declarations }
    MemStream: TMemoryStream;

    function MergeIsSupported: Boolean; override;

    function DateToPDFString(dt: TDateTime): string;
    function Occurs(Value: string; ch: Char): Integer;
    procedure ReplaceChar(var Source: string; chOld, chNew: Char);
    function PDFColor(Value: TColor; IsBackground: Boolean): string;

    procedure AddRef(RefIndex: string);
    function GetNextRefIndex: string;

    procedure WriteCrossRef;
  public
    { Public declarations }
    procedure WriteString(const s: string); override;

    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;

    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;
    procedure WriteHeader; override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;

    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteLine(Left, Top, Width, Height: Integer; ForeColor, BackColor: TColor);
    procedure WriteBitmap(Index: Integer; ImageRefIndex: string; bmp: TBitmap);

    procedure WritePageBegin; virtual;
    procedure WritePageEnd; virtual;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Extension: string; override;
  published
    { Published declarations }
    property PageSetup;
  end;

implementation

uses SysUtils, Printers;

const
  {A4 format}
  A4PageWidth = 612;//808;//2100/2.6
  A4PageHeight = 792;//1115;//2900/2.6
  RowHeight = 20;

function IntPower(Base: Extended; Exponent: Integer): Extended;
asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
end;

function EncodeString(const s: string; Filter: TPDFFilterDecode): string;
var
  i, j, intLen, ip: Integer;
  strOut: string;

  dblGroup: Extended;
  b: Byte;

  {$IFDEF SME_USE_ZLIB}
  src, trg: TStringStream;
  {$ENDIF}
begin
  intLen := Length(s);
  case Filter of
    fdASCIIHex: begin
                  SetLength(Result, 2*intLen);
                  for i := 1 to intLen do
                  begin
                    strOut := IntToHex(Ord(s[i]), 2);
                    Result[2*(i-1)+1] := strOut[1];
                    Result[2*(i-1)+2] := strOut[2];
                  end;
                end;

    fdASCII85: begin
  {This filter decodes data that has been encoded in the ASCII base-85 encoding and
   produces binary data.
   ASCII base-85 encoding produces five ASCII printing characters from every four
   bytes of binary data. Each group of four binary bytes (b1 b2 b3 b4 ) is converted to a
   group of five encoded characters (c1 c2 c3 c4 c5 ) using the relation:

   (b1*256^3)+(b2*256^2)+(b3*256)+b4 = (c1*85^4)+(c2*85^3)+(c3*85^2)+(c4*85)+c5

   The five "digits" of the encoded base-85 number are converted to printable ASCII
   characters by adding 33 (the ASCII code for !) to each. The resulting data contains
   only printable ASCII characters with codes in the range 33 (!) to 117 (u).

   Two special cases occur during encoding. First, if all five encoded digits are zero,
   they are represented by the character code 122 (z), instead of by a series of five
   exclamation points (!!!!!). In addition, if the length of the binary data to be
   encoded is not a multiple of four bytes, the last partial 4-tuple is used to produce a
   last, partial output 5-tuple. Given n (1, 2, or 3) bytes of binary data, the encoding
   first appends 4 - n zero bytes to make a complete 4-tuple. This 4-tuple is encoded
   in the usual way, but without applying the special z case. Finally, only the first
   n + 1 characters of the resulting 5-tuple are written out. Those characters are
   immediately followed by the EOD marker, which is the two-character sequence
   ~>.

   The following conditions are errors during decoding:
   - The value represented by a 5-tuple is greater than 2 32 - 1.
   - A z character occurs in the middle of a 5-tuple.
   - A final partial 5-tuple contains only one character.

   These conditions never occur in the output produced from a correctly encoded byte
   sequence.}

                  Result := '';
                  i := 1;
                  while (i <= intLen) do
                  begin
                    dblGroup := 0;
                    for j := 0 to 3 do
                    begin
                      dblGroup := dblGroup + Ord(s[i])*Trunc(IntPower(256, 3-j));
                      Inc(i);
                    end;

                    if (dblGroup = 0) and (i <= intLen) then
                      Result := Result + Chr(122)
                    else
                    begin
                      for j := 1 to 5 do
                      begin
                        ip := Trunc(IntPower(85, 5-j));
                        b := Trunc(dblGroup/ip);
                        if (b <> 0) then
                          dblGroup := dblGroup - (b * ip);
                        Result := Result + Chr(b + 33)
                      end;
                    end;

                  end;
                  Result := Result + '~>'
               end;

    {$IFDEF SME_USE_ZLIB}
    fdFlate: begin
               src := TStringStream.Create(s);
               trg := TStringStream.Create('');
               try
                 with TCompressionStream.Create(TCompressionLevel(CompressionMethod), trg) do
                   try
                     CopyFrom(src, 0)
                   finally
                     Free
                   end;
                 Result := trg.DataString
               finally
                 src.Free;
                 trg.Free;
               end;
             end;
    {$ENDIF}
  else
    Result := s
  end;
end;

{ TPDFFont }
function TPDFFont.GetFontName: string;
begin
  if (Charset <> ANSI_CHARSET) then
  begin
    case Charset of
      EASTEUROPE_CHARSET: Result := BaseFont + '-EastEurope';
      RUSSIAN_CHARSET: Result := BaseFont + '-Russian';
      GREEK_CHARSET: Result := BaseFont + '-Greek';
      TURKISH_CHARSET: Result := BaseFont + '-Turkish';
      BALTIC_CHARSET: Result := BaseFont + '-Baltic';
      MAC_CHARSET: Result := BaseFont + '-MAC';
      OEM_CHARSET: Result := BaseFont + '-OEM';
      SYMBOL_CHARSET: Result := BaseFont + '-Symbol';
    else
      Result := BaseFont + '-' + IntToStr(Charset)
    end
  end
  else
    Result := BaseFont;
end;

procedure TPDFFont.FontParameters(Font: TFont);
var
  DC: hDC;
  TM: TTextMetric;
  OTM: TOutlineTextMetric;
  oldFont: hFont;
begin
  Name := Font.Name;
  Style := Font.Style;

  DC := GetDC(0);
  oldFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, TM);
//  IsTrueType := {True;//}False;
  IsTrueType := (TM.tmPitchAndFamily and TMPF_TRUETYPE) = TMPF_TRUETYPE;
//  IsTrueType := True;

  Flags := $20;
  if (TM.tmPitchAndFamily and FF_ROMAN) = FF_ROMAN then
    Flags := Flags or $02;
  if (TM.tmPitchAndFamily and FF_SCRIPT) = FF_SCRIPT then
    Flags := Flags or $02;
  if TM.tmItalic <> 0 then
    Flags := Flags or $40;

//  IsEmbeddable := True;//False;
  GetOutlineTextMetrics(DC, SizeOf(TOutlineTextMetric), @OTM);
  {author allows you to embedd it in documents}
  IsEmbeddable := ((OTM.otmfsType and 1) = 0);

  SelectObject(DC, oldFont);
  ReleaseDC(0, DC);
end;

function TPDFFont.IsStandardFont: Boolean;
var
  s: string;
begin
  s := GetFontName; //BaseFont
  if (CompareText(s, 'Helvetica') = 0) or
     (CompareText(s, 'Courier') = 0) or
     (CompareText(s, 'Times') = 0) or
     (CompareText(s, 'Symbol') = 0) or
     (CompareText(s, 'ZapfDingbats') = 0) then
    Result := True
  else
    Result := False
end;

function TPDFFont.EncodingString: string;
begin
  if (CompareText(Name, 'ZapfDingbats') = 0) or
     (CompareText(Name, 'Symbol') = 0) or
     (CharSet = SYMBOL_CHARSET) then
    Result := 'StandardEncoding'
  else
  if (CharSet = MAC_CHARSET) then
    Result := 'MacRomanEncoding'
  else
    Result := 'WinAnsiEncoding';
end;

{ TPDFFonts }
function TPDFFonts.GetItemByFont(const BaseFont: string; Charset: TFontCharset): Integer;
var
  i: Integer;
  fnt: TPDFFont;
begin
  Result := -1;
  for i := 0 to Count-1 do
  begin
    fnt := TPDFFont(Items[i]);
    if Assigned(fnt) and
       (fnt.BaseFont = BaseFont) and (fnt.Charset = Charset) then
    begin
      Result := i;
      break
    end;
  end
end;

function TPDFFonts.FontFamily(Font: TFont; Charset: TFontCharset): string;
var
  strFontName: string;
begin
  if Charset in [128, 129, 136, 134] then
  begin
    case Charset of
      128: Result := 'JapanFont';
      129: Result := 'KoreanFont';
      136: Result := 'ChineseTraditionalFont';
    else
      Result := 'ChineseSimplifiedFont';
    end;
    exit;
  end;

  Result := 'Times-Roman';
  strFontName := UpperCase(Font.Name);

  if (Pos('ARIAL', strFontName) <> 0) or
     (Pos('MS SANS SERIF', strFontName) <> 0) then
  begin
    Result := 'Helvetica';
    if (fsBold in Font.Style) then
    begin
      Result := Result + '-Bold';
      if (fsItalic in Font.Style) then
        Result := Result + 'Oblique'
    end
    else
    begin
      if (fsItalic in Font.Style) then
        Result := Result + '-Oblique'
    end
  end;

  if Pos('COURIER', strFontName) <> 0 then
  begin
    Result := 'Courier';
    if (fsBold in Font.Style) then
    begin
      Result := Result + '-Bold';
      if (fsItalic in Font.Style) then
        Result := Result + 'Oblique'
      else
    end
    else
    begin
      if (fsItalic in Font.Style) then
        Result := Result + '-Oblique'
    end;
  end;

  if Pos('TIMES', strFontName) <> 0 then
  begin
    Result := 'Times-';
    if (fsBold in Font.Style) then
    begin
      Result := Result + 'Bold';
      if (fsItalic in Font.Style) then
        Result := Result + 'Italic'
    end
    else
    begin
      if (fsItalic in Font.Style) then
        Result := Result + 'Italic'
      else
        Result := Result + 'Roman';
    end
  end;

  if Pos('SYMBOL', strFontName) <> 0 then
    Result := 'Symbol'
  else
  if (Pos('ZAPFDINGBATS', strFontName) <> 0) or
     (Pos('WINGDINGS', strFontName) <> 0) then
    Result := 'ZapfDingbats'
  else
end;

function TPDFFonts.GetFontCharset(AValue: TFontCharset): TFontCharset;
begin
  if AValue = DEFAULT_CHARSET then
  begin
    case GetACP() of
       874: Result := THAI_CHARSET;
       932: Result := SHIFTJIS_CHARSET;
       936: Result := GB2312_CHARSET;
       950: Result := CHINESEBIG5_CHARSET;
      1250: Result := EASTEUROPE_CHARSET;
      1251: Result := RUSSIAN_CHARSET;
      1252: Result := ANSI_CHARSET;
      1253: Result := GREEK_CHARSET;
      1254: Result := TURKISH_CHARSET;
      1255: Result := HEBREW_CHARSET;
      1256: Result := ARABIC_CHARSET;
      1257: Result := BALTIC_CHARSET;
      1258: Result := VIETNAMESE_CHARSET;
    else
      Result := ANSI_CHARSET;
    end;
  end
  else
    Result := AValue;
end;

function TPDFFonts.FontFromTable(PDFEngine: TSMExportToPDF; Font: TFont): Integer;
var
  i: Integer;
  FontStr: string;
  fnt: TPDFFont;
  Charset: TFontCharset;
begin
  Charset := GetFontCharset(Font.Charset);
  FontStr := FontFamily(Font, Charset);

  i := GetItemByFont(FontStr, Charset);
  if (i < 0) then
  begin
    fnt := TPDFFont.Create;
    fnt.BaseFont := FontStr;
    fnt.RefIndex := PDFEngine.GetNextRefIndex();
    fnt.CharSet := Charset;
    fnt.FontParameters(Font);

    i := Add(fnt);
  end;
  Result := i;
end;

function TPDFFonts.GetFont(Index: Integer): TPDFFont;
begin
  if (Index > -1) and (Index < Count) then
    Result := TPDFFont(Items[Index])
  else
    Result := nil
end;

procedure TPDFFonts.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    TPDFFont(Items[i]).Free;

  inherited;
end;

{character glyphs for all non-standard font/charset}
type
  TPDFGlyph = record
    CharID: Byte;
    Name: string;
  end;

const
  {Greek map}
  arrGreekMap: array[0..71] of TPDFGlyph =
    ((CharID: 161; Name: 'dieresistonos'),     (CharID: 162; Name: 'Alphatonos'),
     (CharID: 175; Name: 'afii00208'),         (CharID: 180; Name: 'tonos'),
     (CharID: 184; Name: 'Epsilontonos'),      (CharID: 185; Name: 'Etatonos'),
     (CharID: 186; Name: 'Iotatonos'),         (CharID: 188; Name: 'Omicrontonos'),
     (CharID: 190; Name: 'Upsilontonos'),      (CharID: 191; Name: 'Omegatonos'),
     (CharID: 192; Name: 'iotadieresistonos'), (CharID: 193; Name: 'Alpha'),
     (CharID: 194; Name: 'Beta'),              (CharID: 195; Name: 'Gamma'),
     (CharID: 196; Name: 'Delta'),             (CharID: 197; Name: 'Epsilon'),
     (CharID: 198; Name: 'Zeta'),              (CharID: 199; Name: 'Eta'),
     (CharID: 200; Name: 'Theta'),             (CharID: 201; Name: 'Iota'),
     (CharID: 202; Name: 'Kappa'),             (CharID: 203; Name: 'Lambda'),
     (CharID: 204; Name: 'Mu'),                (CharID: 205; Name: 'Nu'),
     (CharID: 206; Name: 'Xi'),                (CharID: 207; Name: 'Omicron'),
     (CharID: 208; Name: 'Pi'),                (CharID: 209; Name: 'Rho'),
     (CharID: 211; Name: 'Sigma'),             (CharID: 212; Name: 'Tau'),
     (CharID: 213; Name: 'Upsilon'),           (CharID: 214; Name: 'Phi'),
     (CharID: 215; Name: 'Chi'),               (CharID: 216; Name: 'Psi'),
     (CharID: 217; Name: 'Omega'),             (CharID: 218; Name: 'Iotadieresis'),
     (CharID: 219; Name: 'Upsilondieresis'),   (CharID: 220; Name: 'alphatonos'),
     (CharID: 221; Name: 'epsilontonos'),      (CharID: 222; Name: 'etatonos'),
     (CharID: 223; Name: 'iotatonos'),         (CharID: 224; Name: 'upsilondieresistonos'),
     (CharID: 225; Name: 'alpha'),             (CharID: 226; Name: 'beta'),
     (CharID: 227; Name: 'gamma'),             (CharID: 228; Name: 'delta'),
     (CharID: 229; Name: 'epsilon'),           (CharID: 230; Name: 'zeta'),
     (CharID: 231; Name: 'eta'),               (CharID: 232; Name: 'theta'),
     (CharID: 233; Name: 'iota'),              (CharID: 234; Name: 'kappa'),
     (CharID: 235; Name: 'lambda'),            (CharID: 236; Name: 'mu'),
     (CharID: 237; Name: 'nu'),                (CharID: 238; Name: 'xi'),
     (CharID: 239; Name: 'omicron'),           (CharID: 240; Name: 'pi'),
     (CharID: 241; Name: 'rho'),               (CharID: 242; Name: 'sigma1'),
     (CharID: 243; Name: 'sigma'),             (CharID: 244; Name: 'tau'),
     (CharID: 245; Name: 'upsilon'),           (CharID: 246; Name: 'phi'),
     (CharID: 247; Name: 'chi'),               (CharID: 248; Name: 'psi'),
     (CharID: 249; Name: 'omega'),             (CharID: 250; Name: 'iotadieresis'),
     (CharID: 251; Name: 'upsilondieresis'),   (CharID: 252; Name: 'omicrontonos'),
     (CharID: 253; Name: 'upsilontonos'),      (CharID: 254; Name: 'omegatonos'));

  arrEastEuropeMap: array[0..52] of TPDFGlyph =
    ((CharID: 140; Name: 'Sacute'),        (CharID: 141; Name: 'Tcaron'),
     (CharID: 143; Name: 'Zacute'),        (CharID: 156; Name: 'sacute'),
     (CharID: 157; Name: 'tcaron'),        (CharID: 159; Name: 'zacute'),
     (CharID: 161; Name: 'caron'),         (CharID: 162; Name: 'breve'),
     (CharID: 163; Name: 'Lslash'),        (CharID: 165; Name: 'Aogonek'),
     (CharID: 170; Name: 'Scedilla'),      (CharID: 175; Name: 'Zdotaccent'),
     (CharID: 178; Name: 'ogonek'),        (CharID: 179; Name: 'lslash'),
     (CharID: 185; Name: 'aogonek'),       (CharID: 186; Name: 'scedilla'),
     (CharID: 188; Name: 'Lcaron'),        (CharID: 189; Name: 'hungarumlaut'),
     (CharID: 190; Name: 'lcaron'),        (CharID: 191; Name: 'zdotaccent'),
     (CharID: 192; Name: 'Racute'),        (CharID: 195; Name: 'Abreve'),
     (CharID: 197; Name: 'Lacute'),        (CharID: 198; Name: 'Cacute'),
     (CharID: 200; Name: 'Ccaron'),        (CharID: 202; Name: 'Eogonek'),
     (CharID: 204; Name: 'Ecaron'),        (CharID: 207; Name: 'Dcaron'),
     (CharID: 208; Name: 'Dcroat'),        (CharID: 209; Name: 'Nacute'),
     (CharID: 210; Name: 'Ncaron'),        (CharID: 213; Name: 'Ohungarumlaut'),
     (CharID: 216; Name: 'Rcaron'),        (CharID: 217; Name: 'Uring'),
     (CharID: 219; Name: 'Uhungarumlaut'), (CharID: 222; Name: 'Tcedilla'),
     (CharID: 224; Name: 'racute'),        (CharID: 227; Name: 'abreve'),
     (CharID: 229; Name: 'lacute'),        (CharID: 230; Name: 'cacute'),
     (CharID: 232; Name: 'ccaron'),        (CharID: 234; Name: 'eogonek'),
     (CharID: 236; Name: 'ecaron'),        (CharID: 239; Name: 'dcaron'),
     (CharID: 240; Name: 'dcroat'),        (CharID: 241; Name: 'nacute'),
     (CharID: 242; Name: 'ncaron'),        (CharID: 245; Name: 'ohungarumlaut'),
     (CharID: 248; Name: 'rcaron'),        (CharID: 249; Name: 'uring'),
     (CharID: 251; Name: 'uhungarumlaut'), (CharID: 254; Name: 'tcedilla'),
     (CharID: 255; Name: 'dotaccent'));

  arrRussianMap: array[0..95] of TPDFGlyph =
    ((CharID: 128; Name: 'afii10051'),     (CharID: 129; Name: 'afii10052'),
     (CharID: 131; Name: 'afii10100'),     (CharID: 136; Name: 'Euro'),
     (CharID: 138; Name: 'afii10058'),     (CharID: 140; Name: 'afii10059'),
     (CharID: 141; Name: 'afii10061'),     (CharID: 142; Name: 'afii10060'),
     (CharID: 143; Name: 'afii10145'),     (CharID: 144; Name: 'afii10099'),
     (CharID: 154; Name: 'afii10106'),     (CharID: 156; Name: 'afii10107'),
     (CharID: 157; Name: 'afii10109'),     (CharID: 158; Name: 'afii10108'),
     (CharID: 159; Name: 'afii10193'),     (CharID: 161; Name: 'afii10062'),
     (CharID: 162; Name: 'afii10110'),     (CharID: 163; Name: 'afii10057'),
     (CharID: 165; Name: 'afii10050'),     (CharID: 168; Name: 'afii10023'),
     (CharID: 170; Name: 'afii10053'),     (CharID: 175; Name: 'afii10056'),
     (CharID: 178; Name: 'afii10055'),     (CharID: 179; Name: 'afii10103'),
     (CharID: 180; Name: 'afii10098'),     (CharID: 184; Name: 'afii10071'),
     (CharID: 185; Name: 'afii61352'),     (CharID: 186; Name: 'afii10101'),
     (CharID: 188; Name: 'afii10105'),     (CharID: 189; Name: 'afii10054'),
     (CharID: 190; Name: 'afii10102'),     (CharID: 191; Name: 'afii10104'),
     (CharID: 192; Name: 'afii10017'),     (CharID: 193; Name: 'afii10018'),
     (CharID: 194; Name: 'afii10019'),     (CharID: 195; Name: 'afii10020'),
     (CharID: 196; Name: 'afii10021'),     (CharID: 197; Name: 'afii10022'),
     (CharID: 198; Name: 'afii10024'),     (CharID: 199; Name: 'afii10025'),
     (CharID: 200; Name: 'afii10026'),     (CharID: 201; Name: 'afii10027'),
     (CharID: 202; Name: 'afii10028'),     (CharID: 203; Name: 'afii10029'),
     (CharID: 204; Name: 'afii10030'),     (CharID: 205; Name: 'afii10031'),
     (CharID: 206; Name: 'afii10032'),     (CharID: 207; Name: 'afii10033'),
     (CharID: 208; Name: 'afii10034'),     (CharID: 209; Name: 'afii10035'),
     (CharID: 210; Name: 'afii10036'),     (CharID: 211; Name: 'afii10037'),
     (CharID: 212; Name: 'afii10038'),     (CharID: 213; Name: 'afii10039'),
     (CharID: 214; Name: 'afii10040'),     (CharID: 215; Name: 'afii10041'),
     (CharID: 216; Name: 'afii10042'),     (CharID: 217; Name: 'afii10043'),
     (CharID: 218; Name: 'afii10044'),     (CharID: 219; Name: 'afii10045'),
     (CharID: 220; Name: 'afii10046'),     (CharID: 221; Name: 'afii10047'),
     (CharID: 222; Name: 'afii10048'),     (CharID: 223; Name: 'afii10049'),
     (CharID: 224; Name: 'afii10065'),     (CharID: 225; Name: 'afii10066'),
     (CharID: 226; Name: 'afii10067'),     (CharID: 227; Name: 'afii10068'),
     (CharID: 228; Name: 'afii10069'),     (CharID: 229; Name: 'afii10070'),
     (CharID: 230; Name: 'afii10072'),     (CharID: 231; Name: 'afii10073'),
     (CharID: 232; Name: 'afii10074'),     (CharID: 233; Name: 'afii10075'),
     (CharID: 234; Name: 'afii10076'),     (CharID: 235; Name: 'afii10077'),
     (CharID: 236; Name: 'afii10078'),     (CharID: 237; Name: 'afii10079'),
     (CharID: 238; Name: 'afii10080'),     (CharID: 239; Name: 'afii10081'),
     (CharID: 240; Name: 'afii10082'),     (CharID: 241; Name: 'afii10083'),
     (CharID: 242; Name: 'afii10084'),     (CharID: 243; Name: 'afii10085'),
     (CharID: 244; Name: 'afii10086'),     (CharID: 245; Name: 'afii10087'),
     (CharID: 246; Name: 'afii10088'),     (CharID: 247; Name: 'afii10089'),
     (CharID: 248; Name: 'afii10090'),     (CharID: 249; Name: 'afii10091'),
     (CharID: 250; Name: 'afii10092'),     (CharID: 251; Name: 'afii10093'),
     (CharID: 252; Name: 'afii10094'),     (CharID: 253; Name: 'afii10095'),
     (CharID: 254; Name: 'afii10096'),     (CharID: 255; Name: 'afii10097'));

  arrHebrewMap: array[0..53] of TPDFGlyph =
    ((CharID: 164; Name: 'afii57636'),     (CharID: 170; Name: 'multiply'),
     (CharID: 186; Name: 'divide'),        (CharID: 192; Name: 'afii57799'),
     (CharID: 193; Name: 'afii57801'),     (CharID: 194; Name: 'afii57800'),
     (CharID: 195; Name: 'afii57802'),     (CharID: 196; Name: 'afii57793'),
     (CharID: 197; Name: 'afii57794'),     (CharID: 198; Name: 'afii57795'),
     (CharID: 199; Name: 'afii57798'),     (CharID: 200; Name: 'afii57797'),
     (CharID: 201; Name: 'afii57806'),     (CharID: 203; Name: 'afii57796'),
     (CharID: 204; Name: 'afii57807'),     (CharID: 205; Name: 'afii57839'),
     (CharID: 206; Name: 'afii57645'),     (CharID: 207; Name: 'afii57841'),
     (CharID: 208; Name: 'afii57842'),     (CharID: 209; Name: 'afii57804'),
     (CharID: 210; Name: 'afii57803'),     (CharID: 211; Name: 'afii57658'),
     (CharID: 212; Name: 'afii57716'),     (CharID: 213; Name: 'afii57717'),
     (CharID: 214; Name: 'afii57718'),     (CharID: 224; Name: 'afii57664'),
     (CharID: 225; Name: 'afii57665'),     (CharID: 226; Name: 'afii57666'),
     (CharID: 227; Name: 'afii57667'),     (CharID: 228; Name: 'afii57668'),
     (CharID: 229; Name: 'afii57669'),     (CharID: 230; Name: 'afii57670'),
     (CharID: 231; Name: 'afii57671'),     (CharID: 232; Name: 'afii57672'),
     (CharID: 233; Name: 'afii57673'),     (CharID: 234; Name: 'afii57674'),
     (CharID: 235; Name: 'afii57675'),     (CharID: 236; Name: 'afii57676'),
     (CharID: 237; Name: 'afii57677'),     (CharID: 238; Name: 'afii57678'),
     (CharID: 239; Name: 'afii57679'),     (CharID: 240; Name: 'afii57680'),
     (CharID: 241; Name: 'afii57681'),     (CharID: 242; Name: 'afii57682'),
     (CharID: 243; Name: 'afii57683'),     (CharID: 244; Name: 'afii57684'),
     (CharID: 245; Name: 'afii57685'),     (CharID: 246; Name: 'afii57686'),
     (CharID: 247; Name: 'afii57687'),     (CharID: 248; Name: 'afii57688'),
     (CharID: 249; Name: 'afii57689'),     (CharID: 250; Name: 'afii57690'),
     (CharID: 253; Name: 'afii299'),       (CharID: 254; Name: 'afii300'));

  arrBalticMap: array[0..57] of TPDFGlyph =
    ((CharID: 141; Name: 'dieresis'),     (CharID: 142; Name: 'caron'),
     (CharID: 143; Name: 'cedilla'),      (CharID: 157; Name: 'macron'),
     (CharID: 158; Name: 'ogonek'),       (CharID: 168; Name: 'Oslash'),
     (CharID: 170; Name: 'Rcommaaccent'), (CharID: 175; Name: 'AE'),
     (CharID: 184; Name: 'oslash'),       (CharID: 186; Name: 'rcommaaccent'),
     (CharID: 191; Name: 'ae'),           (CharID: 192; Name: 'Aogonek'),
     (CharID: 193; Name: 'Iogonek'),      (CharID: 194; Name: 'Amacron'),
     (CharID: 195; Name: 'Cacute'),       (CharID: 198; Name: 'Eogonek'),
     (CharID: 199; Name: 'Emacron'),      (CharID: 200; Name: 'Ccaron'),
     (CharID: 202; Name: 'Zacute'),       (CharID: 203; Name: 'Edotaccent'),
     (CharID: 204; Name: 'Gcommaaccent'), (CharID: 205; Name: 'Kcommaaccent'),
     (CharID: 206; Name: 'Imacron'),      (CharID: 207; Name: 'Lcommaaccent'),
     (CharID: 208; Name: 'Scaron'),       (CharID: 209; Name: 'Nacute'),
     (CharID: 210; Name: 'Ncommaaccent'), (CharID: 212; Name: 'Omacron'),
     (CharID: 216; Name: 'Uogonek'),      (CharID: 217; Name: 'Lslash'),
     (CharID: 218; Name: 'Sacute'),       (CharID: 219; Name: 'Umacron'),
     (CharID: 221; Name: 'Zdotaccent'),   (CharID: 222; Name: 'Zcaron'),
     (CharID: 224; Name: 'aogonek'),      (CharID: 225; Name: 'iogonek'),
     (CharID: 226; Name: 'amacron'),      (CharID: 227; Name: 'cacute'),
     (CharID: 230; Name: 'eogonek'),      (CharID: 231; Name: 'emacron'),
     (CharID: 232; Name: 'ccaron'),       (CharID: 234; Name: 'zacute'),
     (CharID: 235; Name: 'edotaccent'),   (CharID: 236; Name: 'gcommaaccent'),
     (CharID: 237; Name: 'kcommaaccent'), (CharID: 238; Name: 'imacron'),
     (CharID: 239; Name: 'lcommaaccent'), (CharID: 240; Name: 'scaron'),
     (CharID: 241; Name: 'nacute'),       (CharID: 242; Name: 'ncommaaccent'),
     (CharID: 244; Name: 'omacron'),      (CharID: 248; Name: 'uogonek'),
     (CharID: 249; Name: 'lslash'),       (CharID: 250; Name: 'sacute'),
     (CharID: 251; Name: 'umacron'),      (CharID: 253; Name: 'zdotaccent'),
     (CharID: 254; Name: 'zcaron'),       (CharID: 255; Name: 'dotaccent'));

  arrTurkishMap: array[0..5] of TPDFGlyph =
    ((CharID: 208; Name: 'Gbreve'),       (CharID: 221; Name: 'Idotaccent'),
     (CharID: 222; Name: 'Scedilla'),     (CharID: 240; Name: 'gbreve'),
     (CharID: 253; Name: 'dotlessi'),     (CharID: 254; Name: 'scedilla'));

  arrArabicMap: array[0..60] of TPDFGlyph =
    ((CharID: 129; Name: 'afii57506'),    (CharID: 138; Name: 'afii57511'),
     (CharID: 141; Name: 'afii57507'),    (CharID: 142; Name: 'afii57508'),
     (CharID: 143; Name: 'afii57512'),    (CharID: 144; Name: 'afii57509'),
     (CharID: 154; Name: 'afii57513'),    (CharID: 157; Name: 'afii61664'),
     (CharID: 158; Name: 'afii301'),      (CharID: 159; Name: 'afii57514'),
     (CharID: 161; Name: 'afii57388'),    (CharID: 186; Name: 'afii57403'),
     (CharID: 191; Name: 'afii57407'),    (CharID: 193; Name: 'afii57409'),
     (CharID: 194; Name: 'afii57410'),    (CharID: 195; Name: 'afii57411'),
     (CharID: 196; Name: 'afii57412'),    (CharID: 197; Name: 'afii57413'),
     (CharID: 198; Name: 'afii57414'),    (CharID: 199; Name: 'afii57415'),
     (CharID: 200; Name: 'afii57416'),    (CharID: 201; Name: 'afii57417'),
     (CharID: 202; Name: 'afii57418'),    (CharID: 203; Name: 'afii57419'),
     (CharID: 204; Name: 'afii57420'),    (CharID: 205; Name: 'afii57421'),
     (CharID: 206; Name: 'afii57422'),    (CharID: 207; Name: 'afii57423'),
     (CharID: 208; Name: 'afii57424'),    (CharID: 209; Name: 'afii57425'),
     (CharID: 210; Name: 'afii57426'),    (CharID: 211; Name: 'afii57427'),
     (CharID: 212; Name: 'afii57428'),    (CharID: 213; Name: 'afii57429'),
     (CharID: 214; Name: 'afii57430'),    (CharID: 216; Name: 'afii57431'),
     (CharID: 217; Name: 'afii57432'),    (CharID: 218; Name: 'afii57433'),
     (CharID: 219; Name: 'afii57434'),    (CharID: 220; Name: 'afii57440'),
     (CharID: 221; Name: 'afii57441'),    (CharID: 222; Name: 'afii57442'),
     (CharID: 223; Name: 'afii57443'),    (CharID: 225; Name: 'afii57444'),
     (CharID: 227; Name: 'afii57445'),    (CharID: 228; Name: 'afii57446'),
     (CharID: 229; Name: 'afii57470'),    (CharID: 230; Name: 'afii57448'),
     (CharID: 236; Name: 'afii57449'),    (CharID: 237; Name: 'afii57450'),
     (CharID: 240; Name: 'afii57451'),    (CharID: 241; Name: 'afii57452'),
     (CharID: 242; Name: 'afii57453'),    (CharID: 243; Name: 'afii57454'),
     (CharID: 245; Name: 'afii57455'),    (CharID: 246; Name: 'afii57456'),
     (CharID: 248; Name: 'afii57457'),    (CharID: 250; Name: 'afii57458'),
     (CharID: 253; Name: 'afii299'),      (CharID: 254; Name: 'afii300'),
     (CharID: 255; Name: 'afii57519'));

procedure TPDFFonts.WriteFontDifferences(PDFEngine: TSMExportToPDF; fnt: TPDFFont);
var
  i: Integer;
begin
  if not Assigned(PDFEngine) then exit;

  {an array of character codes and glyph names}
  if fnt.CharSet = GREEK_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 71 do
      PDFEngine.WriteString(IntToStr(arrGreekMap[i].CharID) + ' /' + arrGreekMap[i].Name);
    PDFEngine.WriteString(']');
  end
  else

  if fnt.CharSet = EASTEUROPE_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 52 do
      PDFEngine.WriteString(IntToStr(arrEastEuropeMap[i].CharID) + ' /' + arrEastEuropeMap[i].Name);
    PDFEngine.WriteString(']');
  end
  else

  if fnt.CharSet = RUSSIAN_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 95 do
      PDFEngine.WriteString(IntToStr(arrRussianMap[i].CharID) + ' /' + arrRussianMap[i].Name);
    PDFEngine.WriteString(']');


{    PDFEngine.WriteString('/Differences [129 /afii10052');
    PDFEngine.WriteString('/quotesinglbase/afii10100/quotedblbase/ellipsis/dagger/daggerdbl/Euro/perthousand/afii10058/guilsinglleft/afii10059/afii10061/afii10060/afii10145/afii10099/quoteleft');
    PDFEngine.WriteString('/quoteright/quotedblleft/quotedblright/bullet/endash/emdash/space/trademark/afii10106/guilsinglright/afii10107/afii10109/afii10108/afii10193/space/afii10062');
    PDFEngine.WriteString('/afii10110/afii10057/currency/afii10050/brokenbar/section/afii10023/copyright/afii10053/guillemotleft/logicalnot/hyphen/registered/afii10056/degree/plusminus');
    PDFEngine.WriteString('/afii10055/afii10103/afii10098/mu/paragraph/periodcentered/afii10071/afii61352/afii10101/guillemotright/afii10105/afii10054/afii10102/afii10104/afii10017/afii10018');
    PDFEngine.WriteString('/afii10019/afii10020/afii10021/afii10022/afii10024/afii10025/afii10026/afii10027/afii10028/afii10029/afii10030/afii10031/afii10032/afii10033/afii10034/afii10035');
    PDFEngine.WriteString('/afii10036/afii10037/afii10038/afii10039/afii10040/afii10041/afii10042/afii10043/afii10044/afii10045/afii10046/afii10047/afii10048/afii10049/afii10065/afii10066');
    PDFEngine.WriteString('/afii10067/afii10068/afii10069/afii10070/afii10072/afii10073/afii10074/afii10075/afii10076/afii10077/afii10078/afii10079/afii10080/afii10081/afii10082/afii10083');
    PDFEngine.WriteString('/afii10084/afii10085/afii10086/afii10087/afii10088/afii10089/afii10090/afii10091/afii10092/afii10093/afii10094/afii10095/afii10096/afii10097/space]');
}  end
  else

  if fnt.CharSet = HEBREW_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 95 do
      PDFEngine.WriteString(IntToStr(arrHebrewMap[i].CharID) + ' /' + arrHebrewMap[i].Name);
    PDFEngine.WriteString(']');
  end
  else

  if fnt.CharSet = BALTIC_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 57 do
      PDFEngine.WriteString(IntToStr(arrBalticMap[i].CharID) + ' /' + arrBalticMap[i].Name);
    PDFEngine.WriteString(']');
  end
  else

  if fnt.CharSet = TURKISH_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 5 do
      PDFEngine.WriteString(IntToStr(arrTurkishMap[i].CharID) + ' /' + arrTurkishMap[i].Name);
    PDFEngine.WriteString(']');
  end
  else

  if fnt.CharSet = ARABIC_CHARSET then
  begin
    PDFEngine.WriteString('/Differences [');
    for i := 0 to 60 do
      PDFEngine.WriteString(IntToStr(arrArabicMap[i].CharID) + ' /' + arrArabicMap[i].Name);
    PDFEngine.WriteString(']');
  end;
end;

procedure TPDFFonts.WriteFonts(PDFEngine: TSMExportToPDF);
type
  TarrABC = array[0..255] of TABC;
  ParrABC = ^TarrABC;
var
  arrABC: TarrABC;
  i, j: Integer;
  intFontDataLen: dWord;
  fnt: TPDFFont;
  EncodingRef, FontDescriptorRef,
  FontRefIndex, FontData: string;

  bmp: TBitmap;
  TM: TTextMetric;
  s: string;

  fd: TPDFFilterDecode;
begin
  for i := 0 to Count-1 do
  begin
    fnt := TPDFFont(Items[i]);

    PDFEngine.AddRef(fnt.RefIndex);
    PDFEngine.WriteString(fnt.RefIndex + ' 0 obj');
    PDFEngine.WriteString('<< /Type /Font ');
//    PDFEngine.WriteString('/Subtype /Type1 ');
    PDFEngine.WriteString('/Name /F' + IntToStr(i) + ' ');
    PDFEngine.WriteString('/BaseFont /' + fnt.BaseFont + ' ');

    {write encoding}
    if not fnt.IsStandardFont then
    begin
      EncodingRef := PDFEngine.GetNextRefIndex();
      PDFEngine.WriteString('/Encoding ' + EncodingRef + ' 0 R');
    end
    else
      PDFEngine.WriteString('/Encoding /' + fnt.EncodingString + ' ');

    if not fnt.IsStandardFont {}and
       fnt.IsTrueType {}then
    begin
      if (fnt.CharSet = CHINESEBIG5_CHARSET) then
        PDFEngine.WriteString('/Subtype /Type0 ')
      else
        PDFEngine.WriteString('/Subtype /TrueType ');
//        PDFEngine.WriteString('/Subtype /TrueType ');

      {write all widths for saved font}
      bmp := TBitmap.Create;
      try
        bmp.Canvas.Font.Name := fnt.Name;
        bmp.Canvas.Font.Charset := fnt.CharSet;
        bmp.Canvas.Font.Style := fnt.Style;
        bmp.Canvas.Font.Height := -1000;

        GetTextMetrics(bmp.Canvas.Handle, TM);

        PDFEngine.WriteString('/FirstChar ' + IntToStr(Ord(TM.tmFirstChar)));
        PDFEngine.WriteString('/LastChar ' + IntToStr(Ord(TM.tmLastChar)));

        PDFEngine.WriteString('/Widths [');
        s := '';
        if GetCharABCWidths(bmp.Canvas.Handle, dWord(TM.tmFirstChar), dWord(TM.tmLastChar), arrABC) then
        begin
          for j := 0 to dWord(TM.tmLastChar)-dWord(TM.tmFirstChar) do
          begin
            s := s + IntToStr(dWord(arrABC[j].abcA) + arrABC[j].abcB + dWord(arrABC[j].abcC)) + ' ';
            if Length(s) > 80 then
            begin
              PDFEngine.WriteString(s);
              s := '';
            end
          end
        end;

        PDFEngine.WriteString(s + ']');
      finally
        bmp.Free;
      end;

//      if fnt.IsTrueType then
      begin
        FontDescriptorRef := PDFEngine.GetNextRefIndex();
        PDFEngine.WriteString('/FontDescriptor ' + FontDescriptorRef + ' 0 R >>');
      end
    end
    else
      PDFEngine.WriteString('/Subtype /Type1 >>');

    if not fnt.IsStandardFont then
    begin
      PDFEngine.WriteString('endobj');
      PDFEngine.AddRef(EncodingRef);
      PDFEngine.WriteString(EncodingRef + ' 0 obj');
      PDFEngine.WriteString('<</Type /Encoding ');
      PDFEngine.WriteString('/BaseEncoding /' + fnt.EncodingString + ' ');

      {write font differences for extended charsets}
      WriteFontDifferences(PDFEngine, fnt);

      PDFEngine.WriteString('>>');
    end;
    PDFEngine.WriteString('endobj');

    if not fnt.IsStandardFont and
       fnt.IsTrueType then
    begin
      {write font descriptor}
      PDFEngine.AddRef(FontDescriptorRef);
      PDFEngine.WriteString(FontDescriptorRef + ' 0 obj');
      PDFEngine.WriteString('<< /Type /FontDescriptor ');
      PDFEngine.WriteString('/FontName /' + fnt.GetFontName);
      PDFEngine.WriteString('/FontBBox [ 0 0 0 0 ] ');

      PDFEngine.WriteString('/Flags ' + IntToStr(fnt.Flags));
      PDFEngine.WriteString('/StemV ' + IntToStr(50 + Round(sqr(TM.tmWeight / 65))));
      PDFEngine.WriteString('/ItalicAngle 0');
      PDFEngine.WriteString('/CapHeight ' + IntToStr(TM.tmHeight));
      PDFEngine.WriteString('/Ascent 1000');
      PDFEngine.WriteString('/Descent -250');
      PDFEngine.WriteString('/MaxWidth 1000');

      FontRefIndex := '';
      if {fnt.IsTrueType and }fnt.IsEmbeddable then
      begin
        if (fnt.CharSet <> ANSI_CHARSET) then
        begin
          FontRefIndex := PDFEngine.GetNextRefIndex();
          PDFEngine.WriteString('/FontFile2 ' + FontRefIndex + ' 0 R');
        end;
      end;

      PDFEngine.WriteString('>>');
      PDFEngine.WriteString('endobj');

      if FontRefIndex <> '' then
      begin
        {write font contents to file}
        {load font metric data into string}
        bmp := TBitmap.Create;
        try
          bmp.Canvas.Font.Name := fnt.Name;
          bmp.Canvas.Font.Charset := fnt.CharSet;
          bmp.Canvas.Font.Style := fnt.Style;
          bmp.Canvas.Font.Size := Round(72000/bmp.Canvas.Font.PixelsPerInch);
          intFontDataLen := GetFontData(bmp.Canvas.Handle, 0, 0, nil, 1);
          if intFontDataLen = GDI_ERROR then
            intFontDataLen := 0
          else
          begin
            SetLength(FontData, intFontDataLen);
            if GetFontData(bmp.Canvas.Handle, 0, 0, PChar(FontData), intFontDataLen) = GDI_ERROR then
              FontData := '';
          end
        finally
          bmp.Free;
        end;

        PDFEngine.AddRef(FontRefIndex);
        PDFEngine.WriteString(FontRefIndex + ' 0 obj');

        {Because the stream containing Type 1 or TrueType font data may include binary
        data, it may be desirable to convert this data to ASCII using either the ASCII
        hexadecimal or ASCII base-85 encoding.}
        fd := {fdASCII85; //} fdASCIIHex;
        j := intFontDataLen; //length for original font data
        FontData := EncodeString(FontData, fd);
        intFontDataLen := Length(FontData);
        PDFEngine.WriteString('<< /Filter /' + arrPDFFilter[fd] + ' ');
        PDFEngine.WriteString('/Length ' + IntToStr(intFontDataLen) + ' /Length1 ' + IntToStr(j));
        PDFEngine.WriteString('>>');

        PDFEngine.WriteString('stream');
        PDFEngine.WriteString(FontData);
        PDFEngine.WriteString('endstream');
        PDFEngine.WriteString('endobj');
      end;
    end;
  end;
end;

{ TSMExportToPDF }
constructor TSMExportToPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MemStream := TMemoryStream.Create;
  TableType := tePDF;
end;

destructor TSMExportToPDF.Destroy;
begin
  MemStream.Free;

  inherited Destroy;
end;

function TSMExportToPDF.Extension: string;
begin
  Result := '.PDF'
end;

function TSMExportToPDF.MergeIsSupported: Boolean;
begin
  Result := False
end;

procedure TSMExportToPDF.WriteString(const s: string);
var
  intLength: Integer;
  str: string;
begin
  str := s + #13#10;
  intLength := Length(str);
  if IsMemStream then
  begin
    MemStream.Write(str[1], intLength);

    {to increase a stream size for written data}
    Inc(StreamSize, intLength);
  end
  else
    OutputStream.Write(str[1], intLength);

//  if IsDataStarted then
    Inc(FilePos, intLength);
end;

function TSMExportToPDF.DateToPDFString(dt: TDateTime): string;
begin
  Result := FormatDateTime('yyyymmddhhnnss', dt)
end;

function TSMExportToPDF.Occurs(Value: string; ch: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Value) do
    if (Value[i] = ch) then
      Inc(Result)
end;

procedure TSMExportToPDF.ReplaceChar(var Source: string; chOld, chNew: Char);
var
  i: Integer;
begin
  for i := 1 to Length(Source) do
    if (Source[i] = chOld) then
      Source[i] := chNew
end;

function TSMExportToPDF.PDFColor(Value: TColor; IsBackground: Boolean): string;
var
  Color: Integer;
begin
  Color  := ColorToRGB(Value);
  Result := FormatFloat('0.000', GetRValue(Color) / 255) + ' ' + FormatFloat('0.000', GetGValue(Color) / 255) + ' ' + FormatFloat('0.000', GetBValue(Color) / 255);
  if IsBackground then
    Result := Result + ' RG'
  else
    Result := Result + ' rg';
  ReplaceChar(Result, DecimalSeparator, '.');
end;

procedure TSMExportToPDF.AddRef(RefIndex: string);
begin
  CrossRef.Add(AddZeros(RefIndex, 6, True) + ' ' + AddZeros(IntToStr(FilePos-StreamSize), 10, True));
end;

function TSMExportToPDF.GetNextRefIndex: string;
begin
  Result := IntToStr(FLastUsedRefIndex);
  Inc(FLastUsedRefIndex);
end;

procedure TSMExportToPDF.WriteCrossRef;
var
  i: Integer;
begin
  CrossRef.Sort;

  WriteString('xref');
  WriteString('0 ' + IntToStr(CrossRef.Count + 1));
  WriteString('0000000000 65535 f');
  for i := 0 to CrossRef.Count - 1 do
    WriteString(Copy(CrossRef[i], 8, 10) + ' 00000 n');
end;

procedure TSMExportToPDF.WriteFileBegin;
begin
  CrossRef := TStringList.Create;

  tblFont := TPDFFonts.Create;

  tblImage := TStringList.Create;
  lstPages := TStringList.Create;
  intTotalPages := 0;

  FLastUsedRefIndex := 4;
  MaxKids := 10;
  FilePos := 0;

  intLastMergeCol := -1;
  WriteString('%PDF-1.2');

  AddRef('1');

  WriteString('1 0 obj');
  WriteString('<< /Type /Catalog ');
  WriteString('/Pages 3 0 R >> ');
  WriteString('endobj');
end;

procedure TSMExportToPDF.WriteFileEnd;
var
  i: Integer;
  XRefPos: Integer;
  Obj, Buffer: string;
  bmp: TBitmap;
begin
  WritePageEnd;

  tblFont.WriteFonts(Self);

  {export saved bitmaps}
  if (tblImage.Count > 0) then
  begin
    for i := 0 to tblImage.Count-1 do
    begin
      bmp := TBitmap(tblImage.Objects[i]);
      WriteBitmap(i, tblImage[i], bmp);
      bmp.Free
    end;
  end;

  AddRef('2');

  WriteString('2 0 obj');
  WriteString('<< /Creator (Scalabium SMExport suite)');
  WriteString('/CreationDate (D:' + DateToPDFString(Now()) + ')');
  WriteString('/Title (' + KeyGenerator + ')');
  WriteString('/Author (SMExport)');
  WriteString('/Producer (Scalabium SMExport suite, http://www.scalabium.com)');
  WriteString('/Keywords (' {+ Keywords} + ')');
  WriteString('/Subject (' {+ Subject} + ') >>');
  WriteString('endobj');

  AddRef('3');

  WriteString('3 0 obj');
  WriteString('<< /Type /Pages ');
  Buffer := '/Kids [ ';
  for i := 0 to lstPages.Count - 1 do
    Buffer := Buffer + Trim(Copy(lstPages[i], 1, 6)) + ' 0 R ';

  Buffer := Buffer + ' ] ';
  WriteString(Buffer);
  WriteString('/Count ' + IntToStr(intTotalPages) + ' >> ');
  WriteString('endobj');

  {page refs}
  for i := 0 to lstPages.Count-1 do
  begin
    Obj := Trim(Copy(lstPages[i], 1, 6));
    AddRef(Obj);
    WriteString(Obj + ' 0 obj');
    WriteString('<< /Type /Pages ');
    WriteString('/Kids [ ' + Copy(lstPages[i], 7, Length(lstPages[i]) - 7) + ' ] ');
    WriteString('/Count ' + IntToStr(Occurs(lstPages[i], 'R')) + ' ');
    WriteString('/Parent 3 0 R >> ');
    WriteString('endobj');
  end;

  XRefPos := FilePos;
  WriteCrossRef;

  WriteString('trailer');
  WriteString('<< /Root 1 0 R ');
  WriteString('/Info 2 0 R ');
  WriteString('/Size ' + IntToStr(CrossRef.Count + 1) + ' >> ');
  WriteString('startxref');
  WriteString(IntToStr(XRefPos));

  WriteString('%%EOF');

  tblFont.Clear;
  tblFont.Free;

  tblImage.Free;
  CrossRef.Free;
  lstPages.Free;
end;

procedure TSMExportToPDF.WritePageBegin;
var
  PageObj: string;
begin
  intCurY := 0;
  intCurYPos := RowHeight-10;;
  StreamSize := 0;

  if (intTotalPages mod MaxKids = 0) then
    lstPages.Add(Copy(GetNextRefIndex() + '      ', 1, 6) + ' ');
  Inc(intTotalPages);

  PageObj := GetNextRefIndex();
  lstPages[lstPages.Count-1] := lstPages[lstPages.Count-1] + PageObj + ' 0 R ';

  AddRef(PageObj);
  WriteString(PageObj + ' 0 obj');
  WriteString('<< /Type /Page ');
  WriteString('/Parent ' + Trim(Copy(lstPages[lstPages.Count-1], 1, 6)) + ' 0 R ');

  BodyRefIndex := GetNextRefIndex();
  StreamRefIndex := GetNextRefIndex();
  ProcRefIndex := GetNextRefIndex();
  ResRefIndex := GetNextRefIndex();

  WriteString('/MediaBox [0 0 ' + IntToStr(PageWidth) + ' ' + IntToStr(PageHeight) + '] ');

  WriteString('/Resources ' + ResRefIndex + ' 0 R');
  WriteString('/Contents ' + BodyRefIndex + ' 0 R >> ');
  WriteString('endobj');

  AddRef(ResRefIndex);

  WriteString(ResRefIndex + ' 0 obj');

  StreamSize := 0;

  {now every streaming are in memory because we must collect font table}
  IsMemStream := True;
  IsDataStarted := True;
end;

procedure TSMExportToPDF.WritePageEnd;
var
  str, StreamLen: string;
  j: Integer;
begin
  IsMemStream := False;

  {flush fonts}
  WriteString('<<');
  if (tblFont.Count > 0) then
  begin
    str := '/Font << ';
    for j := 0 to tblFont.Count - 1 do
      str := str + '/F' + IntToStr(j) + ' ' + tblFont.GetFont(j).RefIndex + ' 0 R ';
    WriteString(str + '>>');
  end;

  {flush images}
  if (tblImage.Count > 0) then
  begin
    str := '/XObject << ';
    for j := 0 to tblImage.Count - 1 do
      str := str + '/Im' + IntToStr(j+1) + ' ' + tblImage[j] + ' 0 R ';
    WriteString(str + '>>');
  end;

  WriteString('/ProcSet ' + ProcRefIndex + ' 0 R');
  WriteString('>>');
  WriteString('endobj');
  AddRef(BodyRefIndex);
  WriteString(BodyRefIndex + ' 0 obj');
  WriteString('<< /Length ' + StreamRefIndex + ' 0 R >> ');
  WriteString('stream');

  StreamLen := IntToStr(StreamSize-1);

  {copy exported data from MemStream}
  OutputStream.CopyFrom(MemStream, 0);
  MemStream.Clear;
  StreamSize := 0;

  WriteString('endstream');
  WriteString('endobj');
  AddRef(StreamRefIndex);
  WriteString(StreamRefIndex + ' 0 obj');
  WriteString(StreamLen);
  WriteString('endobj');

  AddRef(ProcRefIndex);
  WriteString(ProcRefIndex + ' 0 obj');
  if (tblImage.Count > 0) then
    {ImageB is "grayscale images or image masks"
     ImageC is "color image"
     ImageI is "indexed images (also called color-table images)"
    }
    WriteString('[ /PDF /Text /ImageC ] ')
  else
    WriteString('[ /PDF /Text ] ');
  WriteString('endobj');
end;

procedure TSMExportToPDF.WriteRowStart(IsAddedTitle: Boolean);
begin
  intCurRowHeight := RowHeight;
end;

function TSMExportToPDF.GetRowTop(ARow: Integer): Integer;
begin
  Result := RowHeight*(ARow+1)-10;
end;

procedure TSMExportToPDF.WriteRowEnd(IsAddedTitle: Boolean);
begin
  {horizontal gridline}
  if (soRowLines in Options) then
  begin
    if (Layout <> elReversedColumnar) or
       (Statistic.CurrentRow < Columns.Count) then
      WriteLine(10, PageHeight-intCurYPos-5, PageWidth-2*10, 1, clBlack, clSilver);
  end;

  Inc(intCurY);
  Inc(intCurYPos, intCurRowHeight);

  if (intCurYPos >= PageHeight) then
  begin
    WritePageEnd;
    WritePageBegin;
  end;
end;

procedure TSMExportToPDF.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  i, j, intWidth, intTop, intLastCol: Integer;
  bmp: TBitmap;

  IsFontColorChanged: Boolean;
  strmBMP: TStringStream;
  s: string;
begin
  {rectangle with backcolor}
  if (intLastMergeCol > -1) then
    intLastCol := ACol+intLastMergeCol
  else
    intLastCol := ACol;
  if (intLastCol < LastExportColumnIndex-1) and (intLastCol < VarArrayHighBound(arrColWidths, 1)) then
    intWidth := arrColWidths[intLastCol+1]
  else
    intWidth := PageWidth - 10;
  if (soColorsFonts in Options) then
    WriteLine(arrColWidths[ACol], PageHeight - intCurYPos - 5, intWidth - arrColWidths[ACol]+1, intCurRowHeight{RowHeight}{-10}, Color, {clWhite}Color);


//  if (soColLines in Options) then
//  else
//    WriteLine(arrColWidths[ACol], PageHeight - GetRowTop(intCurY) - 5, intWidth - arrColWidths[ACol]+1, RowHeight{-10}, clBlack, clWhite);
//  ;

  if (Layout = elReversedColumnar) then
    intTop := GetRowTop(ARow)
  else
    intTop := intCurYPos;
  if (Assigned(fld) and
      fld.IsBlob and
      (TBlobField(fld).BlobType = ftGraphic)) or
     (CellType = ctGraphic) then
  begin
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24Bit;
    if Assigned(fld) then
      bmp.Assign(TBlobField(fld))
    else
    begin
      {for constant column kind we must load bitmap from string}
      strmBMP := TStringStream.Create(AString);
      try
        strmBMP.Seek(0, soFromBeginning);
        bmp.LoadFromStream(strmBMP)
      finally
        strmBMP.Free
      end;
    end;
    bmp.PixelFormat := pf24Bit;

    tblImage.AddObject(GetNextRefIndex(), bmp);

    if intCurRowHeight < bmp.Height then
      intCurRowHeight := bmp.Height;
    WriteString('q');
    WriteString(IntToStr(bmp.Width) + ' 0 0 ' +
                IntToStr(intCurRowHeight{RowHeight}{bmp.Height}) + ' ' +
                IntToStr(arrColWidths[ACol]+1) + ' ' +
                IntToStr(PageHeight{-RowHeight}{-bmp.Height}-intTop-5) + ' cm');
    WriteString('/Im' + IntToStr(tblImage.Count) + ' Do');
    WriteString('Q');
  end
  else
  begin
    {text}
    WriteString('BT');
    IsFontColorChanged := False;

{    if (ColorToRGB(color) <> clWhite) then
      WriteString(PDFColor(color, True));
}    if Assigned(Font) then
    begin
      IsFontColorChanged := True;

      if (soColorsFonts in Options) then
        if (ColorToRGB(Font.Color) <> clBlack) then
          WriteString(PDFColor(Font.Color, False));

      WriteString('/F' + IntToStr(tblFont.FontFromTable(Self, Font)) + ' ' + IntToStr(Font.Size) + ' Tf');
      WriteString(IntToStr(Font.Size + 2) + ' TL');
    end;
    {if '\' char is in string, convert}
    i := Pos('\', AString);
    if (i > 0) then
    begin
      s := Copy(AString, 1, i-1);
      for j := i to Length(AString) do
      begin
        if (AString[j] = '\') then
          s := s + '\\'
        else
          s := s + AString[j];
      end;
      AString := s
    end;

    WriteString(IntToStr(arrColWidths[ACol]+1) + ' ' + IntToStr(PageHeight - intTop) + ' Td (' + AString + ') Tj ');

    if IsFontColorChanged then
    begin
      if (soColorsFonts in Options) then
        if (ColorToRGB(Font.Color) <> clBlack) then
          WriteString(PDFColor(clBlack, False));
{    if (ColorToRGB(color) <> clWhite) then
      WriteString(PDFColor(clWhite, True));
}
    end;

    WriteString('ET');
  end;

  {draw vertical line between columns}
  if (soColLines in Options) then
  begin
    if (Layout = elReversedColumnar) then
      intTop := GetRowTop(ARow)
    else
      intTop := intCurYPos;

    if (intLastMergeCol > -1) then
      intLastMergeCol := -1
    else
      WriteLine(arrColWidths[ACol], PageHeight - intTop - 4, 1, intCurRowHeight+1, clSilver{clBlack}, clSilver);
  end
end;

procedure TSMExportToPDF.WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
  if (AMergeCols > 1) then
    intLastMergeCol := AMergeCols
  else
    intLastMergeCol := -1;

  inherited;
end;

procedure TSMExportToPDF.WriteBitmap(Index: Integer; ImageRefIndex: string; bmp: TBitmap);
var
  RowInBitmap: PByteArray;
  BytesPerPixel: ShortInt;
  Mask: dWord;//LongInt;

  function GetRGBPixelFromBitmap(RowID: Integer): Integer;
  var
    i: Byte;
    ByteNo: Word;
    ShiftBy: Byte;
  begin
    Result := 0;
    if BytesPerPixel > 0 then
    begin
      ByteNo := RowID*BytesPerPixel;
      for i := 0 to BytesPerPixel-1 do
        Result := Result + LongInt(RowInBitmap^[i + ByteNo]) shl (8*i);
    end
    else
    begin
      BytesPerPixel := -BytesPerPixel;
      ByteNo := RowID div BytesPerPixel;
      ShiftBy := (BytesPerPixel-(RowID mod BytesPerPixel)-1) * (8 div BytesPerPixel);
      Result := RowInBitmap^[ByteNo] shr ShiftBy;
    end;
    Result := Result and Mask;
  end;

var
  Buffer, LenObj: string;
  i, j, PixelColor, intLen, StreamLen: Integer;
begin
  LenObj := GetNextRefIndex();
  AddRef(ImageRefIndex);

  WriteString(ImageRefIndex + ' 0 obj');
  WriteString('<< ');
  WriteString('/Type /XObject');
  WriteString('/Subtype /Image');
  WriteString('/Name /Im' + IntToStr(Index+1));
  WriteString('/Width ' + IntToStr(bmp.Width));
  WriteString('/Height ' + IntToStr(bmp.Height));
  WriteString('/BitsPerComponent 8');
  WriteString('/ColorSpace /DeviceRGB');
  WriteString('/Filter /' + arrPDFFilter[fdASCIIHex] + ' ');
  WriteString('/Length ' + LenObj + ' 0 R >> ');
  WriteString('stream');

  {for row scan we need bytes per pixel}
  case bmp.PixelFormat of
    pf1bit: begin
              BytesPerPixel := -8;
              Mask := 1;
            end;
    pf4bit: begin
              BytesPerPixel := -2;
              Mask := $F;
            end;
    pf8bit: begin
              BytesPerPixel := 1;
              Mask := $FF;
            end;
    pf15bit: begin
               BytesPerPixel := 2;
               Mask := $7FFF;
             end;
    pf16bit: begin
               BytesPerPixel := 2;
               Mask := $FFFF;
             end;
    pf24bit: begin
               BytesPerPixel := 3;
               Mask := $FFFFFF;
             end;
    pf32bit: begin
               BytesPerPixel := 4;
               Mask := $FFFFFFFF;
             end;
  end;

  Buffer := '';
  StreamSize := 0;
  for i := 0 to bmp.Height-1 do
  begin
    RowInBitmap := PByteArray(bmp.ScanLine[i]);
    for j := 0 to bmp.Width-1 do
    begin
      PixelColor := GetRGBPixelFromBitmap(j);

      Buffer := Buffer +
                IntToHex(GetBValue(PixelColor), 2) + {extract a red line}
                IntToHex(GetGValue(PixelColor), 2) + {extract a green line}
                IntToHex(GetRValue(PixelColor), 2);  {extract a blue line}
      intLen := Length(Buffer);
      if intLen > 77 then
      begin
        WriteString(Buffer);
        Buffer := '';
      end;
    end;
  end;

  WriteString(Buffer + '>');
  StreamLen := StreamSize;

  WriteString('endstream');
  WriteString('endobj');

  AddRef(LenObj);
  WriteString(LenObj + ' 0 obj');
  WriteString(IntToStr(StreamLen));
  WriteString('endobj');
end;

procedure TSMExportToPDF.WriteLine(Left, Top, Width, Height: Integer; ForeColor, BackColor: TColor);
begin
  if (ColorToRGB(ForeColor) <> clBlack) then
    WriteString(PDFColor(ForeColor, False));
  if (ColorToRGB(BackColor) <> clWhite) then
    WriteString(PDFColor(BackColor, True));
  WriteString(IntToStr(Left) + ' ' + IntToStr(Top) + ' ' + IntToStr(Width-1) + ' ' + IntToStr(Height-1) + ' re');
  WriteString('B');
  if (ColorToRGB(BackColor) <> clWhite) then
    WriteString(PDFColor(clWhite, True));
  if (ColorToRGB(ForeColor) <> clBlack) then
    WriteString(PDFColor(clBlack, False));
end;

procedure TSMExportToPDF.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited WriteDimensions(intRowCount, intColCount);

  arrColWidths := VarArrayCreate([0, intColCount-1], varInteger);

  if (PageSetup.Orientation = emPortrait) or
     ((PageSetup.Orientation = emDefault) and (Printer.Orientation = poPortrait)) then
  begin
    PageWidth := A4PageWidth;
    PageHeight := A4PageHeight;
  end
  else
  begin
    PageWidth := A4PageHeight;
    PageHeight := A4PageWidth;
  end;
  FOneSheetWidth := PageWidth;
end;

procedure TSMExportToPDF.WriteColWidth(intCurColumn, intColWidth: Integer); 
var
  i, j, intPrev: Integer;
begin
  inherited WriteColWidth(intCurColumn, intColWidth);

  {must use TextWidth('W') instead const}
  arrColWidths[intCurColumn] := 8*intColWidth;

  {calculate a page width}
  if (intCurColumn = VarArrayHighBound(arrColWidths, 1)-1) then
  begin
    j := 10;
    for i := VarArrayLowBound(arrColWidths, 1) to VarArrayHighBound(arrColWidths, 1) do
      if not VarIsEmpty(arrColWidths[i]) and
         not VarIsNull(arrColWidths[i]) then
      begin
        intPrev := arrColWidths[i];
        arrColWidths[i] := j;
        Inc(j, intPrev);
      end;

    {add a new sheet/vertical page}
    PageWidth := FOneSheetWidth;
    while (j > PageWidth) do
      PageWidth := PageWidth + FOneSheetWidth;
  end
end;

procedure TSMExportToPDF.WriteHeader;
begin
  WritePageBegin;

  inherited WriteHeader;
end;

end.

