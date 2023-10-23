{ SMExport suite
  TSMExportToRTF component

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2RTF;

interface

{$I sme.inc}

uses
  Windows, Classes, Graphics, DB, ExportDS, SME2Cell, SMEEngine
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TSMExportToRTF = class(TSMExportToCellFile)
  private
    { Private declarations }
    lstFonts,
    lstColors: TStrings;

    arrColWidths: Variant;

    {to write in memory stream or in output stream}
    IsMemStream: Boolean;

    function GetTwips(AValue: Single): Integer;
  protected
    { Protected declarations }
    MemStream: TMemoryStream;

    function MergeIsSupported: Boolean; override;

    function DateTimeToRTF(Value: TDateTime): string;
    function GetFontStyle(fs: TFontStyles): string;
    function GetFromTable(lstTable: TStrings; intShift: Integer; const Value: string): string;
    function WriteBitmap(bmp: TBitmap): string;
  public
    { Public declarations }
    procedure WriteString(const s: string); override;

    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;

    procedure WriteDataBegin; override;
    procedure WriteDataEnd; override;

    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;

    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Extension: string; override;
  published
    { Published declarations }
    property PageSetup;
  end;


implementation

uses SysUtils;

{ TSMExportToRTF }
constructor TSMExportToRTF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MemStream := TMemoryStream.Create;
  TableType := teRTF;
end;

destructor TSMExportToRTF.Destroy;
begin
  MemStream.Free;

  inherited Destroy;
end;

function TSMExportToRTF.Extension: string;
begin
  Result := '.RTF'
end;

function TSMExportToRTF.DateTimeToRTF(Value: TDateTime): string;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(Value, Year, Month, Day);

  Result := 'yr' + IntToStr(Year) + '\mo' + IntToStr(Month) + '\dy' + IntToStr(Day);
  if (Frac(Value) <> 0) then
  begin
    DecodeTime(Value, Hour, Min, Sec, MSec);
    Result := Result + '\hr' + IntToStr(Hour) + '\min' + IntToStr(Min);
  end
end;

function TSMExportToRTF.GetFontStyle(fs: TFontStyles): string;
begin
  Result := '';
  if (fsItalic in fs) then
    Result := '\i';
  if (fsBold in fs) then
    Result := Result + '\b';
  if (fsUnderline in fs) then
    Result := Result + '\ul';
end;

function TSMExportToRTF.GetFromTable(lstTable: TStrings; intShift: Integer; const Value: string): string;
var
  i, j: Integer;
begin
  i := lstTable.IndexOf(Value);
  if i <> -1 then
    j := i
  else
    j := lstTable.Add(Value);
  if intShift <> 0 then
    Inc(j, intShift);
  Result := IntToStr(j)
end;

function TSMExportToRTF.MergeIsSupported: Boolean;
begin
  Result := False
end;

procedure TSMExportToRTF.WriteString(const s: string);
var
  intLength: Integer;
begin
  intLength := Length(s);
  if IsMemStream then
    MemStream.Write(s[1], intLength)
  else
    OutputStream.Write(s[1], intLength);
end;

function TSMExportToRTF.GetTwips(AValue: Single): Integer;
begin
  Result := Round(AValue);
  case PageSetup.Measure of
    emInch: Result := 1440*Result;
    emCentimeters: Result := 567*Result;
    emPicas: Result := 72*Result;
  else // emPoint
  end;
end;

procedure TSMExportToRTF.WriteFileBegin;
begin
  lstFonts := TStringList.Create;
  lstColors := TStringList.Create;

  IsMemStream := False;
  WriteString('{\rtf1\ansi\ansicpg' + IntToStr(GetACP()) + '\uc1' + #13#10);

  {add a header (KeyGenerator)}
  IsMemStream := True;

  WriteString('{\*\generator ' + KeyGenerator + ';}');
  WriteString('{\info{\operator ' + KeyGenerator + '}');
  WriteString('{\*\company Scalabium Software}');
  WriteString('{\creatim\' + DateTimeToRTF(Now) + '}');
  WriteString('{\title ' + KeyGenerator + '}');
  WriteString('}'#13#10);

  WriteString('{\header \pard\plain {' + KeyGenerator + ' \par }}' + #13#10);

  {write the PageSetup margins and paper orientation}
  if not PageSetup.UseDefault then
  begin
    WriteString('\margl' + IntToStr(GetTwips(PageSetup.LeftMargin)) +
                '\margr' + IntToStr(GetTwips(PageSetup.RightMargin)) +
                '\margt' + IntToStr(GetTwips(PageSetup.TopMargin)) +
                '\margb' + IntToStr(GetTwips(PageSetup.BottomMargin)) +
                #13#10);
  end;

  case PageSetup.Orientation of
    emPortrait: WriteString('\paperwpaperw11907\paperh16840' + #13#10);
    emLandscape: WriteString('\paperwpaperw16840\paperh11907' + #13#10);
  end;

  inherited WriteFileBegin;
end;

procedure TSMExportToRTF.WriteFileEnd;
var
  s, str: string;
  j, intColor: Integer;
begin
  IsMemStream := True;
  WriteString('\par}');

  IsMemStream := False;
  s := '{\fonttbl';
  for j := 0 to lstFonts.Count - 1 do
  begin
    str := '{\f' + IntToStr(j) + ' ' + lstFonts[j] + '}';
    if Length(s + str) < 255 then
      s := s + str
    else
    begin
      WriteString(s + #13#10);
      s := str;
    end;
  end;
  WriteString(s + '}' + #13#10);

  s := '{\colortbl;';
  for j := 0 to lstColors.Count - 1 do
  begin
    intColor := {ColorToRGB(}StrToInt(lstColors[j]){)};
    str := '\red' + IntToStr(GetRValue(intColor)) +
           '\green' + IntToStr(GetGValue(intColor)) +
           '\blue' + IntToStr(GetBValue(intColor)) + ';';
    if Length(s + str) < 255 then
      s := s + str
    else
    begin
      WriteString(s + #13#10);
      s := str;
    end;
  end;
  WriteString(s + '}' + #13#10);

  OutputStream.CopyFrom(MemStream, 0);

  lstFonts.Free;
  lstColors.Free;

  inherited WriteFileEnd;
end;

procedure TSMExportToRTF.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited WriteDimensions(intRowCount, intColCount);

  arrColWidths := VarArrayCreate([0, intColCount-1], varInteger);
end;

procedure TSMExportToRTF.WriteColWidth(intCurColumn, intColWidth: Integer);
begin
  inherited WriteColWidth(intCurColumn, intColWidth);

  arrColWidths[intCurColumn] := 13*intColWidth;
end;

procedure TSMExportToRTF.WriteDataBegin;
var
  i, intShiftX: Integer;
begin
  inherited WriteDataBegin;

  IsMemStream := True;

  {write a table header}
  WriteString('\par'#13#10 +
              '\trowd\irow\trautofit1\trql\trgaph0\trleft36'#13#10);
  intShiftX := 36;
  for i := VarArrayLowBound(arrColWidths, 1) to VarArrayHighBound(arrColWidths, 1)-1 do
  begin
    if not VarIsEmpty(arrColWidths[i]) and
       not VarIsNull(arrColWidths[i]) then
    begin
      intShiftX := intShiftX + arrColWidths[i]*20 + 10;
      WriteString('\clbrdrl\brdrth \clbrdrr\brdrth \clbrdrt\brdrth \clbrdrb\brdrth \cellx' + IntToStr(intShiftX) + #13#10);
    end
  end;
end;

procedure TSMExportToRTF.WriteDataEnd;
begin
  IsMemStream := True;
  WriteString('\pard' + #13#10);

  inherited WriteDataEnd
end;

procedure TSMExportToRTF.WriteRowStart(IsAddedTitle: Boolean);
begin
end;

procedure TSMExportToRTF.WriteRowEnd(IsAddedTitle: Boolean);
begin
  IsMemStream := True;

  if IsDataArea then
    WriteString('\row' + #13#10)
  else
    WriteString(' \par' + #13#10)
end;

function TSMExportToRTF.WriteBitmap(bmp: TBitmap): string;
var
  bi, bb, rtf: string;
  bis, bbs: dWord;
  achar: ShortString;
  hexpict: string;
  i: Integer;
begin
  GetDIBSizes(bmp.Handle, bis, bbs);
  SetLength(bi, bis);
  SetLength(bb, bbs);
  GetDIB(bmp.Handle, bmp.Palette, PChar(bi)^, PChar(bb)^);
  rtf := '{\pict\dibitmap ';
  SetLength(hexpict,(Length(bb) + Length(bi)) * 2);
  i := 2;
  for bis := 1 to Length(bi) do
  begin
    achar := Format('%x', [Integer(bi[bis])]);
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[i-1] := achar[1];
    hexpict[i] := achar[2];
    Inc(i, 2);
  end;
  for bbs := 1 to Length(bb) do
  begin
    achar := Format('%x', [Integer(bb[bbs])]);
    if Length(achar) = 1 then
      achar := '0' + achar;
    hexpict[i-1] := achar[1];
    hexpict[i] := achar[2];
    Inc(i, 2);
  end;
  rtf := rtf + hexpict + ' }';
  Result := rtf;
end;

procedure TSMExportToRTF.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
const
  arrAlignment: array[TAlignment] of string = ('', '\qc', '\qr');
var
  s: string;

  pic: TPicture;
  bmp: TBitmap;
  strmBMP: TStringStream;
begin
  IsMemStream := True;

  if IsDataArea then
  begin
    if (Assigned(fld) and
        fld.IsBlob and
        not fld.IsNull and
        (TBlobField(fld).BlobType = ftGraphic)) or
       (CellType = ctGraphic) then
    begin
      {load a picture from BLOB}
      pic := TPicture.Create;
      try
        if Assigned(fld) then
          pic.Assign(TBlobField(fld));

        {copy a bitmap to bitmap}
        bmp := TBitmap.Create;
        try
          if Assigned(fld) then
            bmp.Assign(pic.Bitmap)
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
          s := WriteBitmap(bmp);
          WriteString(s);
        finally
          bmp.Free;
        end;
      finally
        pic.Free;
      end;
    end
    else
    begin
      s := '\pard\intbl\li30\ri30' + arrAlignment[al] + '{';
      if (soColorsFonts in Options) then
      begin
        if Assigned(font) then
          s := s + '\f' + GetFromTable(lstFonts, 0, font.Name) +
               '\fs' + IntToStr(font.Size * 2) +
               GetFontStyle(font.Style);
        if (ColorToRGB(color) <> clWhite) then
//          s := s + '\cb' + GetFromTable(lstColors, 1, IntToStr(ColorToRGB(color)));
          s := s + '\highlight' + GetFromTable(lstColors, 1, IntToStr(ColorToRGB(color)));
        if Assigned(font) then
          if (ColorToRGB(font.Color) <> clWhite) then
            s := s + '\cf' + GetFromTable(lstColors, 1, IntToStr(ColorToRGB(font.Color)));
      end;
      
      WriteString(s + ' ' + AString + '}\cell');
    end
  end
  else
    WriteString(AString)
end;

end.