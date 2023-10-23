{ SMExport suite
  TSMExportToHTML component

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2HTML;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;


type
  TSMEGetExtHTMLTableParamsEvent = procedure(Sender: TObject; var ExtTableText: string) of object;
  TSMEGetExtHTMLCellParamsEvent = procedure(Sender: TObject; Column: TSMEColumn; var ExtTDText: string) of object;

  TSMExportToHTML = class(TSMExportToCellFile)
  private
    { Private declarations }
    IsCustomHTMLTemplate: Boolean;
    FHTMLPattern: TStrings;

    intCurLineHTMLPattern, intCurColumn: Integer;

    arrWidthByColumns: Variant;
    FHeaderFooterWithHTML: Boolean;
    FOnGetExtHTMLCellParamsEvent: TSMEGetExtHTMLCellParamsEvent;
    FOnGetExtHTMLTableParamsEvent: TSMEGetExtHTMLTableParamsEvent;

    procedure SetHTMLPattern(Value: TStrings);

    function GetHTMLColor(cl: TColor; IsBackColor: Boolean): string;
    function GetHTMLFontSize(Size: Integer): string;
    function GetHTMLTextWithFont(s: string; fn: TFont): string;
  protected
    { Protected declarations }
    intCurPicFile: Integer;
    strDefTDTag: string;

    FLastColWritten: Integer;

    function GetFieldStr(Field: TField): string; override;
  public
    { Public declarations }
    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;

    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;

    procedure WriteDataBegin; override;
    procedure WriteDataEnd; override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Extension: string; override;
  published
    { Published declarations }
    property HeaderFooterWithHTML: Boolean read FHeaderFooterWithHTML write FHeaderFooterWithHTML;
    property HTMLPattern: TStrings read FHTMLPattern write SetHTMLPattern;

    property OnGetExtHTMLCellParamsEvent: TSMEGetExtHTMLCellParamsEvent read FOnGetExtHTMLCellParamsEvent write FOnGetExtHTMLCellParamsEvent;
    property OnGetExtHTMLTableParamsEvent: TSMEGetExtHTMLTableParamsEvent read FOnGetExtHTMLTableParamsEvent write FOnGetExtHTMLTableParamsEvent;

    property ActionAfterExport;
    property FileName;

    property AddTitle;
    property CharacterSet;

    property ExportStyle;

    property Header;
    property Footer;

    property OnGetCellParams;
  end;

implementation

uses Windows, SysUtils
     {$IFDEF SM_USE_JPEG_FOR_HTML} , JPEG{$ENDIF}
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

{ TSMExportToHTML }
constructor TSMExportToHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHTMLPattern := TStringList.Create;
  AddTitle := True;
  TableType := teHTML;
end;

destructor TSMExportToHTML.Destroy;
begin
  FHTMLPattern.Free;

  inherited Destroy;
end;

procedure TSMExportToHTML.SetHTMLPattern(Value: TStrings);
begin
  FHTMLPattern.Assign(Value)
end;

function TSMExportToHTML.Extension: string;
begin
  Result := '.HTM'
end;

function TSMExportToHTML.GetHTMLColor(cl: TColor; IsBackColor: Boolean): string;
var
  str: string;
  colRGB: TColorRef;
begin
  colRGB := ColorToRGB(cl) ;
  str := Format('%.2x%.2x%.2x',
                [GetRValue(colRGB), GetGValue(colRGB), GetBValue(colRGB)]);

//  str := IntToHex(ColorToRGB(cl), 6);
  if str = '000000' then
    Result := ''
  else
  begin
    if IsBackColor then
      Result := 'bg'
    else
      Result := '';
//    Result := Result + 'color="#' + Copy(str, 5, 2) + Copy(str, 3, 2) + Copy(str, 1, 2) + '"';
    Result := Result + 'color="#' + str + '"';
  end;
end;

function TSMExportToHTML.GetHTMLFontSize(Size: Integer): string;
begin
  case Size of
    6, 7: Result := '1';
    8, 9: Result := '2';
   14..17: Result := '4';
   18..23: Result := '5';
   24..35: Result := '6'
  else
    Result := '-1';
  end;
end;

function TSMExportToHTML.GetHTMLTextWithFont(s: string; fn: TFont): string;
var
  str: string;
begin
  if not Assigned(fn) or
     not (soColorsFonts in Options) then
    Result := s
  else
  begin
    if fsUnderline in fn.Style then
      s := '<u>' + s + '</u>';
    if fsItalic in fn.Style then
    s := '<i>' + s + '</i>';
    if fsBold in fn.Style then
      s := '<b>' + s + '</b>';
    Result := '<font ';
    if fn.Name <> 'MS Sans Serif' then
      Result := Result + 'face="' + fn.Name + '" ';
    str := GetHTMLFontSize(fn.Size);
    if str <> '-1' then
      Result := Result + 'size=' + str;

    str := GetHTMLColor(fn.Color, False);
    if str = '' then
      Result := Result + '>'
    else
      Result := Result + ' ' + str + '>';

    Result := Result + s + '</font>';
  end
end;

function TSMExportToHTML.GetFieldStr(Field: TField): string;
var
  dt: TDateTime;
begin
  Result := inherited GetFieldStr(Field);

  if (soFieldMask in Options) then
  begin
    case Field.DataType of
      ftDate: Result := FormatDateTime(DataFormats.GetDateFormat, Field.AsDateTime);
      ftTime: Result := FormatDateTime(DataFormats.GetTimeFormat, Field.AsDateTime);
      ftDateTime: begin
                    dt := Field.AsDateTime;
                    if (Frac(dt) = 0) then
                      Result := FormatDateTime(DataFormats.GetDateFormat, dt)
                    else
                      Result := FormatDateTime(DataFormats.GetDateTimeFormat, dt);
                  end
    end;
  end
end;

procedure TSMExportToHTML.WriteFileBegin;
var
  Buffer: PChar;
begin
  inherited;

  FLastColWritten := -1;
  IsCustomHTMLTemplate := (FHTMLPattern.Count = 0);
  if IsCustomHTMLTemplate then
    FHTMLPattern.Add('<html>'#13#10'<head>'#13#10+
                     '<meta http-equiv="Content-Type"'#13#10+
                     'content="text/html; charset=windows-' + IntToStr(GetACP()) + '">'#13#10+
                     '<meta name="GENERATOR" content="'+GeneratorVer +'">'#13#10+
                     '<title>' + GetLanguageString(6) + '</title>'#13#10+
                     '</head>'#13#10#13#10+
                     '<body bgcolor="#FFFFFF">'#13#10+
                     '</body>'#13#10+
                     '</html>');

  strDefTDTag := '';
  intCurPicFile := 1;
  {parse a header of HTML-pattern and flush it}
  intCurColumn := 0;
  intCurLineHTMLPattern := 0;
  while (intCurColumn = 0) and (intCurLineHTMLPattern < FHTMLPattern.Count) do
  begin
    intCurColumn := Pos('</BODY>', AnsiUpperCase(FHTMLPattern.Strings[intCurLineHTMLPattern]));
    if intCurColumn > 0 then
      Buffer := PChar(Copy(FHTMLPattern.Strings[intCurLineHTMLPattern], 1, intCurColumn-1) + #13#10)
    else
    begin
      Buffer := PChar(FHTMLPattern.Strings[intCurLineHTMLPattern] + #13#10);
      Inc(intCurLineHTMLPattern);
    end;
    OutputStream.Write(Buffer[0], StrLen(Buffer));
  end;
end;

procedure TSMExportToHTML.WriteFileEnd;
var
  Buffer: PChar;
begin
  inherited;

  {flush a footer of HTML-pattern}
  while (intCurLineHTMLPattern < FHTMLPattern.Count) do
  begin
    intCurColumn := Pos('</BODY>', AnsiUpperCase(FHTMLPattern.Strings[intCurLineHTMLPattern]));
    if intCurColumn > 0 then
      Buffer := PChar(Copy(FHTMLPattern.Strings[intCurLineHTMLPattern], intCurColumn, Length(FHTMLPattern.Strings[intCurLineHTMLPattern])) + #13#10)
    else
      Buffer := PChar(FHTMLPattern.Strings[intCurLineHTMLPattern] + #13#10);
    Inc(intCurLineHTMLPattern);
    OutputStream.Write(Buffer[0], StrLen(Buffer));
  end;

  if IsCustomHTMLTemplate then
    FHTMLPattern.Clear;
end;

procedure TSMExportToHTML.WriteDataBegin;
var
  ExtTableText: string;
begin
  inherited;

  if (soColLines in Options) or
     (soRowLines in Options) then
    ExtTableText := 'border="1"'
  else
    ExtTableText := 'border="0"';

  if Assigned(OnGetExtHTMLTableParamsEvent) then
    OnGetExtHTMLTableParamsEvent(Self, ExtTableText);
  WriteString('<table ' + ExtTableText + '>'#13#10);
end;

procedure TSMExportToHTML.WriteDataEnd;
begin
  inherited;

  WriteString('</table>'#13#10#13#10);
end;

procedure TSMExportToHTML.WriteRowStart(IsAddedTitle: Boolean);
begin
  inherited WriteRowStart(IsAddedTitle);

  if IsDataArea then
    WriteString('<tr>'#13#10)
end;

procedure TSMExportToHTML.WriteRowEnd(IsAddedTitle: Boolean);
begin
  inherited WriteRowEnd(IsAddedTitle);

  if IsDataArea then
    WriteString('</tr>'#13#10)
end;

procedure TSMExportToHTML.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited;

  arrWidthByColumns := VarArrayCreate([0, intColCount-1], varInteger);
end;

procedure TSMExportToHTML.WriteColWidth(intCurColumn, intColWidth: Integer);
begin
  inherited;

  arrWidthByColumns[intCurColumn] := intColWidth;
end;

procedure TSMExportToHTML.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
const
  AlignHTML: array[TAlignment] of string =
            ('', ' align="right"', ' align="center"');
var
  str, PicFileName: string;
  pic: TPicture;
  ExtTDText: string;
begin
  inherited;

  if IsDataArea then
    FLastColWritten := ACol;

  ExtTDText := '';

  if {IsDataArea or} not HeaderFooterWithHTML then
  begin
    if (AString = '') then
      AString := '&nbsp'
    else
      AString := ReplaceHTMLSystemChars(AString);
    str := GetHTMLTextWithFont(AString, font);
  end
  else
    str := AString;


  if (color = -1) then
  else
    if (soColorsFonts in Options) then
      ExtTDText := GetHTMLColor(color, True) + ' ';

  if IsDataArea and
     (arrWidthByColumns[ACol] > 0) then
    ExtTDText := ExtTDText + 'width="' + IntToStr(16*arrWidthByColumns[ACol]) + '"';

  if (strDefTDTag <> '') then
  begin
    ExtTDText := ExtTDText + ' ' + strDefTDTag;
    strDefTDTag := '';
  end;
    
  if Assigned(OnGetExtHTMLCellParamsEvent) then
    OnGetExtHTMLCellParamsEvent(Self, Columns[ACol], ExtTDText);

  if IsDataArea then
  begin
    if Assigned(fld) and
       fld.IsBlob and
       not fld.IsNull and
       (TBlobField(fld).BlobType = ftGraphic) then
    begin
      pic := TPicture.Create;
      try
        pic.{Bitmap.}Assign(TBlobField(fld));

        PicFileName := GetFileNameByGraphic(FileName, intCurPicFile, pic);
        Inc(intCurPicFile);

        {$IFDEF SM_USE_JPEG_FOR_HTML}
        with TJpegImage.Create do
          try
            PicFileName := ChangeFileExt(PicFileName, '.jpg');
            Assign(pic.Bitmap);
            SaveToFile(PicFileName);
          finally
            Free
          end;
        {$ELSE}
        pic.SaveToFile(PicFileName);
        {$ENDIF}

        WriteString('  <td ' + ExtTDText + AlignHTML[al] + '>' +
                    '<img src="' + PicFileName + '" width="' + IntToStr(pic.Width) + '" height="' + IntToStr(pic.Height) + '"></td>'#13#10)
      finally
        pic.Free
      end;
    end
    else
    begin
      WriteString('  <td ' + ExtTDText + AlignHTML[al] + '>' +
                  str +
                  '</td>'#13#10)
    end
  end
  else
  begin
    if HeaderFooterWithHTML then
      WriteString(str + #13#10)
    else
      WriteString('<p ' + ExtTDText + AlignHTML[al] + '>' +
                  str + '</p>' + #13#10)
  end
end;

procedure TSMExportToHTML.WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
//  while FLastColWritten+1 < ACol do
//    WriteData(nil, ctBlank, ARow, FLastColWritten+1, '', taLeftJustify, nil, clNone);

  if (AMergeCols > 1) then
    strDefTDTag := 'colspan="' + IntToStr(AMergeCols) + '"'
  else
    strDefTDTag := '';

  inherited;

  FLastColWritten := FLastColWritten + AMergeCols;
end;

end.
