{ SMExport suite
  TSMExportToWKS component: data export into Lotus WKS-file
  without Lotus 1-2-3 and Symphony OLE

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2WKS;

interface

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

{ WKS: release 1A
  WK1: release 2
  FMT: format of release 2 (wysiwyg)
  WK3: release 3
  FM3: format of release 3 (wysiwyg)
  WK4: release 4 and 5 (includes format (wysiwyg))
  123: release 97
}

type
  TSMExportToWKS = class(TSMExportToCellFile)
  private
    { Private declarations }
    procedure WriteRecordHeader(RecType, Size: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;
    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;
  published
    { Published declarations }
  end;

implementation

uses SysUtils;

{WKS record types}
const
  WKSR_BOF              = $0000;
  WKSR_EOF              = $0001;
  WKSR_RANGE            = $0006;
  WKSR_DOCTYPE_WKS      = $0404; //$0404: 1-2-3 file, $0405: Symphony file
//  WKSR_COLWIDTH         = $0024;
  WKSR_BLANK            = $0C;
  WKSR_INTEGER          = $0D;
  WKSR_NUMBER           = $0E;
  WKSR_LABEL            = $0F;

{ TSMExportToWKS }
constructor TSMExportToWKS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teWKS;
end;

function TSMExportToWKS.Extension: string;
begin
  Result := '.WK1'
end;

procedure TSMExportToWKS.WriteFileBegin;
var
  Buffer: Word;
begin
  inherited;

  WriteRecordHeader(WKSR_BOF, 2);
  Buffer := WKSR_DOCTYPE_WKS;
  OutputStream.Write(Buffer, 2);
end;

procedure TSMExportToWKS.WriteFileEnd;
begin
  inherited;

  WriteRecordHeader(WKSR_EOF, 0);
end;

procedure TSMExportToWKS.WriteDimensions(intRowCount, intColCount: Integer);
var
  Buffer: array[0..3] of Word;
begin
  inherited;

  if intColCount > 255 then
    intColCount := 255;

  WriteRecordHeader(WKSR_RANGE, 8);
  Buffer[0] := 0;
  Buffer[1] := 0;
  Buffer[2] := intColCount-1;
  Buffer[3] := intRowCount-1;
  OutputStream.Write(Buffer, 8);
end;

procedure TSMExportToWKS.WriteRecordHeader(RecType, Size: Integer);
var
  Buffer: array[0..1] of Word;
begin
  Buffer[0] := RecType;
  Buffer[1] := Size;
  OutputStream.Write(Buffer, SizeOf(Buffer));
end;

procedure TSMExportToWKS.WriteColWidth(intCurColumn, intColWidth: Integer);
//var bufCols: array[0..1] of Byte;
begin
{
  if (intColWidth < 1) then exit;
  if (intCurColumn > 255) then exit;

  if (intColWidth = 10) then
    intColWidth := 12;

  bufCols[0] := intCurColumn;
  bufCols[1] := intCurColumn;

  WriteRecordHeader(WKSR_COLWIDTH, 4);
  OutputStream.Write(bufCols, 2);
  intColWidth := 256*intColWidth;
  OutputStream.Write(intColWidth, 2);
}end;

procedure TSMExportToWKS.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
const
//  arrAlign: array[TAlignment] of Byte = ($27, $22, $5E); //'^"
  arrAlign: array[TAlignment] of Char = ('''', '^', '"');
var
  Buffer: array[0..1] of Word;
//  bufString: array[0..239] of Char;
  Format: Byte;
  Size: Word;
  aInt, intError: Integer;
  aDbl: Double;
  Data: Pointer;
  s: ShortString;
begin
  if (ACol > 255) then exit;

  inherited;

  Buffer[0] := ACol;
  Buffer[1] := ARow;

  if (soFieldMask in Options) then
    CellType := ctString
  else
  begin
    aInt := StrToIntDef(AString, 0);
    if (CellType = ctInteger) and
       ((aInt < -32767) or (aInt >32767)) then
      CellType := ctDouble;
  end;

  Size := 0;
  Data := nil;
  case CellType of
    ctBlank: begin
               Format := $22;
               WriteRecordHeader(WKSR_BLANK, 5);
             end;
    ctInteger: begin
                 Format := $82;
                 WriteRecordHeader(WKSR_INTEGER, 7);
                 Size := 2;
                 Val(AString, aInt, intError);
                 if intError <> 0 then
                   aInt := 0;
                 Data := @aInt;
               end;
    ctDouble,
    ctCurrency: begin
                Format := $F1;
                WriteRecordHeader(WKSR_NUMBER, 13);
                Size := 8;
                aDbl := 0;
                if AString <> '' then
                  try
                    aDbl := StrToFloat(AString);
                  except
                  end;
                Data := @aDbl;
              end;
  else // ctString
    Format := $F1; //general
    AString := AString + #0;
    WriteRecordHeader(WKSR_LABEL, SizeOf(Format) + SizeOf(Buffer) + Length(AString) + 1);
//    Exit;
  end;

  OutputStream.Write(Format, SizeOf(Format));
  OutputStream.Write(Buffer, SizeOf(Buffer));

  if (CellType in [ctBlank, ctInteger, ctDouble]) then
  begin
    if Size > 0 then
      OutputStream.Write(Data^, Size);
  end
  else
  begin
    s := arrAlign[al] + AString;
    OutputStream.Write(s[1], Length(s))
//    StrPCopy(bufString, arrAlign[al] + AString);
//    Stream.Write(bufString, Length(bufString)+1);
  end;

{



  WriteRecordHeader(Stream, WKSR_LABEL, 2*SizeOf(Format) + SizeOf(Buffer) + Length(AString) + 1);

  Stream.Write(Format, SizeOf(Format));
  Stream.Write(Buffer, SizeOf(Buffer));
  StrPCopy(bufString, arrAlign[al] + AString);
  Stream.Write(bufString, Length(bufString)+1);
}end;

end.

