{ SMExport suite
  TSMExportToQuattro component: data export into QuattroPro WQ1-file
  without QuattroPro OLE

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2WQ;

interface

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

type
  TSMExportToQuattro = class(TSMExportToCellFile)
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

{WQ record types}
const
  WQR_BOF              = $0000;
  WQR_EOF              = $0001;
  WQR_DIMENSION        = $0006;
  WQR_DOCTYPE_WQ       = $5120; //$1001: WB1, $1002: WB2 (QuattroPro for Windows)
  WQR_COLWIDTH         = $08; //column size
  WQR_BLANK            = $0C;
  WQR_INTEGER          = $0D;
  WQR_NUMBER           = $0E;
  WQR_LABEL            = $0F;

{ TSMExportToQuattro }
constructor TSMExportToQuattro.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teQuattro;
end;

function TSMExportToQuattro.Extension: string;
begin
  Result := '.WQ1'
end;

procedure TSMExportToQuattro.WriteFileBegin;
var
  Buffer: Word;
begin
  inherited;

  WriteRecordHeader(WQR_BOF, 2);
  Buffer := WQR_DOCTYPE_WQ;
  OutputStream.Write(Buffer, 2);
end;

procedure TSMExportToQuattro.WriteFileEnd;
begin
  inherited;

  WriteRecordHeader(WQR_EOF, 0);
end;

procedure TSMExportToQuattro.WriteDimensions(intRowCount, intColCount: Integer);
var
  Buffer: Byte;
  intRow: Word;
begin
  inherited;

  if intColCount > 255 then
    intColCount := 255;

  WriteRecordHeader(WQR_DIMENSION, 8);
  {top-left cell: column-page-row}
  Buffer := 0;
  OutputStream.Write(Buffer, 1);
  Buffer := 0;
  OutputStream.Write(Buffer, 1);
  {intRow=0}
//  intRow := 0;
  Buffer := 0;
  OutputStream.Write(Buffer, 1);
  Buffer := 0;
  OutputStream.Write(Buffer, 1);
//  OutputStream.Write(intRow, SizeOf(intRow));

  {bottom-right cell: column-page-row}
  Buffer := intColCount-1;
  OutputStream.Write(Buffer, 1);
  Buffer := 0;
  OutputStream.Write(Buffer, 1);
  intRow := intRowCount; //-1;
//  OutputStream.Write(intRow, SizeOf(intRow));
  intRow := Hi(intRowCount);
  OutputStream.Write(intRow, 1);
  intRow := Lo(intRowCount);
  OutputStream.Write(intRow, 1);
end;

procedure TSMExportToQuattro.WriteRecordHeader(RecType, Size: Integer);
var
  Buffer: array[0..1] of Word;
begin
  Buffer[0] := RecType;
  Buffer[1] := Size;
  OutputStream.Write(Buffer, SizeOf(Buffer));
end;

procedure TSMExportToQuattro.WriteColWidth(intCurColumn, intColWidth: Integer);
var
  bufCols: array[0..2] of Byte;
begin
  if (intColWidth < 1) then exit;
  if (intCurColumn > 255) then exit;

  if (intColWidth = 10) then
    intColWidth := 12;

  bufCols[0] := intCurColumn;
  bufCols[1] := 0;
  bufCols[2] := intColWidth;

  WriteRecordHeader(WQR_COLWIDTH, 3);
  OutputStream.Write(bufCols, SizeOf(bufCols));
//  intColWidth := 256*intColWidth;
//  Stream.Write(intColWidth, 2);
end;

procedure TSMExportToQuattro.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
const
  arrAlign: array[TAlignment] of Char = ('''', '"', '^');
var
  Buffer: array[0..4] of Byte;
  Size: Word;
  aInt, intError: Integer;
  aDbl: Double;
  Data: Pointer;
  s: ShortString;
begin
  if (ACol > 255) then exit;

  inherited;

  Buffer[0] := 0;
  Buffer[1] := ACol;
  Buffer[2] := 0;
  Buffer[3] := Lo(ARow);
  Buffer[4] := Hi(ARow);

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
               WriteRecordHeader(WQR_BLANK, 5);
             end;
    ctInteger: begin
                 WriteRecordHeader(WQR_INTEGER, 7);
                 Size := 2;
                 Val(AString, aInt, intError);
                 if intError <> 0 then
                   aInt := 0;
                 Data := @aInt;
               end;
    ctDouble,
    ctCurrency: begin
                WriteRecordHeader(WQR_NUMBER, 13);
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
    WriteRecordHeader(WQR_LABEL, 7 + Length(AString));
//    Exit;
  end;

  OutputStream.Write(Buffer, SizeOf(Buffer));

  if (CellType in [ctBlank, ctInteger, ctDouble]) then
  begin
    if Size > 0 then
      OutputStream.Write(Data^, Size);
  end
  else
  begin
    OutputStream.Write(arrAlign[al], 1);
    s := AString;
    OutputStream.Write(s, Length(s) + 1)
  end;
end;

end.

