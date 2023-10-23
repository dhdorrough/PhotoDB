{ SMExport suite
  TSMExportToDBF component

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2DBF;

interface

{$I sme.inc}

uses
  Windows, Classes, Graphics, DB, ExportDS, SMEEngine, SME2Cell
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TDBFField = packed record
    FieldName: array[0..10] of Char;
    FieldType: Char;     { type of field - C,N,D,L,M }
    Address: LongInt;    { not used }
    Length: Byte;        { total width of this field }
    Decimals: Byte;      { digits to right of decimal }
    Reserved1: array[0..1] of Byte;
    WordAreaID: Byte;    { dBaseIV work area ID }
    MultiUserDBase: Word;
    SetFields: Byte;
    Reserved2: array[0..7] of Byte;
    MDXFlag: Byte;       { Field is part of production index - 0x01 else 0x00 }
  end;
  TDBFFields = array[0..255] of TDBFField;

  TDBFVersion = (dBase3, dBase3Memo, FoxPro, FoxProMemo, dBase4, dBase4Memo, VFP3);
  TDBFMemoType = (dmDBT, dmFPT);

const
  arrDBFVersion: array[TDBFVersion] of Byte = ($03, $83, $05, $F5, $03, $8B, $30);

type
  TDBFHeader = packed record
    VersionNumber: Byte;
    LastUpdateYear: Byte;
    LastUpdateMonth: Byte;
    LastUpdateDay: Byte;
    NumberOfRecords: LongInt; { number of record in database }
    HeaderSize: Word;         { number of bytes in header }
    RecordSize: Word;         { number of bytes in record }
    Reserve1: array[0..1] of Char; {fill $00}
    IncompleteTransaction: Byte; { flag for dBase IV:
                                   Begin Transaction sets it to 0x01
                                   End Transaction or RollBack reset it to 0x00 }
    Encrypted: Byte;          { Encryption flag, encrypted 0x01 else 0x00
                                Changing the flag does not encrypt or decrypt the records }
    Reserved: array[0..11] of Char; { dBaseIV multi-user environment use }
    MDXIsExist: Byte;         { Production index exists - 0x01 else 0x00 }
    LanguageID: Byte;         { dBaseIV language driver ID:
                                0x01    codepage  437 DOS USA        FoxPro
                                0x02    codepage  850 DOS Multi ling FoxPro
                                0x03    codepage 1251 Windows ANSI   FoxPro
                                0xC8    codepage 1250 Windows EE     FoxPro
                                0x00    ignored                      FlagShip, dBaseIII+, FoxBase, FoxPro, Clipper}
    Reserved2: array[0..1] of Char; { Reserved fill with 0x00 }

    NumberOfFields: LongInt;
    Fields: TDBFFields;
  end;

  TSMEDBTHeader = packed record
    NextBlock: Longint;
    Dummy: array [4..7] of Byte;
    DbfFile: array [0..7] of Byte;   // 8..15
    Version: Byte;                   // 16
    Dummy2: array [17..19] of Byte;
    BlockLen: Word;                   // 20..21
    Dummy3: array [22..511] of Byte;
  end;

  TSMEFPTHeader = packed record
    NextBlock: Longint;
    Dummy: array [4..5] of Byte;
    BlockLen: Word;                   // 20..21
    Dummy3: array [8..511] of Byte;
  end;

  PSMEBlockHeader = ^TSMEBlockHeader;
  TSMEBlockHeader = record
    MemoType: LongInt;
    MemoSize: LongInt;
  end;

  TSMExportToDBF = class(TSMExportToCellFile)
  private
    { Private declarations }
    DBFHeader: TDBFHeader;
    MEMOHeader: TSMEDBTHeader;

    FDBFVersion: TDBFVersion;
    FDBFMemoType: TDBFMemoType;

    MemoIsExists: Boolean;
    MemoFileName: string;
  protected
    { Protected declarations }
    MEMOStream: TStream;

    function MergeIsSupported: Boolean; override;
    procedure InternalBeforeProcess; override;
    procedure InternalFileCreate(const AFileName: string); override;

    procedure WriteMemo(Value: string);
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;

    procedure WriteDataBegin; override;
    procedure CloseFileStream; override;

    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;

    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;
  published
    { Published declarations }
    property DBFVersion: TDBFVersion read FDBFVersion write FDBFVersion default dBase3;
    property DBFMemoType: TDBFMemoType read FDBFMemoType write FDBFMemoType default dmDBT;
  end;


implementation

uses SysUtils, ExCnst;

{ TSMExportToDBF }
constructor TSMExportToDBF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teDBase;

  FDBFVersion := dBase3;
  FDBFMemoType := dmDBT;
end;

function TSMExportToDBF.Extension: string;
begin
  Result := '.DBF'
end;

function TSMExportToDBF.MergeIsSupported: Boolean;
begin
  Result := False
end;

procedure TSMExportToDBF.Assign(Source: TPersistent);
begin
  inherited;

  AddTitle := False;
end;

procedure TSMExportToDBF.InternalBeforeProcess;
begin
  inherited;

  Header.Clear;
  Footer.Clear;
  AddTitle := False;
end;

procedure TSMExportToDBF.WriteDataBegin;
var
  i: Integer;
begin
  inherited WriteDataBegin;

  {write a header of DBF-file}
  DBFHeader.HeaderSize := 33 + (32*(DBFHeader.NumberOfFields-1));
  OutputStream.WriteBuffer(DBFHeader, 32);
  for i := 0 to DBFHeader.NumberOfFields-2 do
    OutputStream.WriteBuffer(DBFHeader.Fields[i], 32);
  WriteString(#$0D);
end;

procedure TSMExportToDBF.WriteDimensions(intRowCount, intColCount: Integer);
var
  intYear, intMonth, intDay: Word;
  i: Integer;
begin
  inherited WriteDimensions(intRowCount, intColCount);

  MemoIsExists := False;
  for i := 0 to Columns.Count-1 do
  begin
    if (Columns[i].DataType in [ctMEMO, ctGraphic]) then
    begin
      MemoIsExists := True;
      break
    end
  end;

  if MemoIsExists then
  begin
    {change dbf-version}
    case DBFVersion of
      dBase3: DBFVersion := dBase3Memo;
      FoxPro: DBFVersion := FoxProMemo;
      dBase4: DBFVersion := dBase4Memo;
    end;
  end;
  DBFHeader.VersionNumber := arrDBFVersion[DBFVersion];

  DecodeDate(Date(), intYear, intMonth, intDay);
  DBFHeader.LastUpdateYear := intYear - 1900;
  DBFHeader.LastUpdateMonth := intMonth;
  DBFHeader.LastUpdateDay := intDay;

  DBFHeader.NumberOfRecords:= intRowCount{+1};
  DBFHeader.NumberOfFields := intColCount;
  FillChar(DBFHeader.Reserved, SizeOf(DBFHeader.Reserved){14}, #0);
  DBFHeader.MDXIsExist := 0;
  FillChar(DBFHeader.Reserved2, SizeOf(DBFHeader.Reserved2){3}, #0);
end;

procedure TSMExportToDBF.WriteColWidth(intCurColumn, intColWidth: Integer);

  function UniqueFieldName(FieldName: string; intFieldCount: Integer): string;
  var
    i: Integer;
    ch: Char;
    strTrail: string;
  begin
    for i := 1 to Length(FieldName) do
      if (FieldName[i] = ' ') then
        FieldName[i] := '_';

    Result := FieldName;
    for i := 0 to intFieldCount{DBFHeader.NumberOfFields}-1 do
      if (DBFHeader.Fields[i].FieldName = Result) then
      begin
        strTrail := '';
        while Result <> '' do
        begin
          ch := Result[Length(Result)];
          if ch in ['0'..'9'] then
          begin
            strTrail := ch + strTrail;
            Delete(Result, Length(Result), 1);
          end
          else
          begin
            if strTrail = '' then
              strTrail := '0';
            strTrail := IntToStr(StrToInt(strTrail)+1);
            Result := Copy(Result, 1, 10-Length(strTrail)) + strTrail;
            Result := UniqueFieldName(Result, intFieldCount);

            break;
          end
        end;
        break
      end;
  end;

var
  i, intLen: Integer;
  dt: TFieldType;
  col: TSMEColumn;
  s: string;
begin
  inherited WriteColWidth(intCurColumn, intColWidth);

  col := nil;
  intLen := 0;
  for i := 0 to Columns.Count-1 do
    if Columns[i].Visible then
    begin
      if (intLen = intCurColumn) then
      begin
        col := Columns[i];
        break
      end;
      Inc(intLen)
    end;

{  intLen := Length(col.FieldName);
  if (intLen > 10) then
    intLen := 10;
  for i := 0 to intLen-1 do
    DBFHeader.Fields[intCurColumn].FieldName[i] := col.FieldName[i+1];
}
//  FillChar(DBFHeader.Fields[intCurColumn], SizeOf(DBFHeader.Fields[intCurColumn]), #0);

  if (soUseFieldNameAsCaption in Options) then
    s := col.Title.Caption
  else
    s := col.FieldName;
  s := UniqueFieldName(Copy(s, 1, 10), intCurColumn);
  for i := 0 to Length(s)-1 do
    DBFHeader.Fields[intCurColumn].FieldName[i] := s[i+1];
  if intColWidth = 0 then
    intColWidth := 10;
  if intColWidth > 255 then
    intColWidth := 255;
  DBFHeader.Fields[intCurColumn].Length := intColWidth;
  DBFHeader.Fields[intCurColumn].Decimals := 0;

  {define field types}
  if (soFieldMask in Options) then
    DBFHeader.Fields[intCurColumn].FieldType := 'C'
  else
  begin
    dt := SourceDataEngine.DataTypeByColumn(col);
    case dt of
      ftSmallint,
      ftWord: begin
                DBFHeader.Fields[intCurColumn].FieldType := 'N';
                DBFHeader.Fields[intCurColumn].Decimals := 0;
//                if (intColWidth = 10) then
                  DBFHeader.Fields[intCurColumn].Length := 5;
              end;

      ftInteger,
      ftAutoInc: begin
                   DBFHeader.Fields[intCurColumn].FieldType := 'N';
                   DBFHeader.Fields[intCurColumn].Decimals := 0;
                   if (intColWidth = 10) or (intColWidth = 0) then
                     DBFHeader.Fields[intCurColumn].Length := 11
                   else
                     DBFHeader.Fields[intCurColumn].Length := intColWidth
                 end;

      {$IFDEF SMForDelphi4}
      ftLargeint: begin
                    DBFHeader.Fields[intCurColumn].FieldType := 'N';
                    DBFHeader.Fields[intCurColumn].Decimals := 0;
                    if (intColWidth = 10) or (intColWidth = 0) then
                      DBFHeader.Fields[intCurColumn].Length := 18
                    else
                      DBFHeader.Fields[intCurColumn].Length := intColWidth
                  end;
      {$ENDIF}

      ftBoolean: begin
                   DBFHeader.Fields[intCurColumn].FieldType := 'L';
                   DBFHeader.Fields[intCurColumn].Length := 1;
                 end;
      ftFloat,
      ftCurrency,
      ftBCD: begin
               DBFHeader.Fields[intCurColumn].FieldType := 'N';
               DBFHeader.Fields[intCurColumn].Decimals := col.Precision; //2
               if ((intColWidth = 10) and (not (col.ColumnKind in [ckConstant, ckSysVar]))) or (intColWidth = 0) then
                 DBFHeader.Fields[intCurColumn].Length := 15
               else
                 DBFHeader.Fields[intCurColumn].Length := intColWidth
             end;

      ftDate,
      ftDateTime: begin
                    DBFHeader.Fields[intCurColumn].FieldType := 'D';
                    DBFHeader.Fields[intCurColumn].Length := 8; //YYYYMMDD
                  end;
      ftTime: begin
                DBFHeader.Fields[intCurColumn].FieldType := 'C';
                DBFHeader.Fields[intCurColumn].Length := 11;//'hh:nn:ss AM'
              end;

      ftVarBytes,
      ftBlob,
      ftMemo,
      ftGraphic,
      ftFmtMemo,
      ftParadoxOle,
      ftDBaseOle,
      ftTypedBinary,
      ftCursor: begin
                  { MEMOs are not supported now. Convert to C(255) now }
                  DBFHeader.Fields[intCurColumn].FieldType := 'M';
                  DBFHeader.Fields[intCurColumn].Length := 10;
{                  DBFHeader.Fields[intCurColumn].FieldType := 'C';
                  DBFHeader.Fields[intCurColumn].Length := 255;
}                end;
    else
      DBFHeader.Fields[intCurColumn].FieldType := 'C';
    end;
  end;
  FillChar(DBFHeader.Fields[intCurColumn].Reserved1, SizeOf(DBFHeader.Fields[intCurColumn].Reserved1){2}, #0);
  DBFHeader.Fields[intCurColumn].WordAreaID := 0;
  DBFHeader.Fields[intCurColumn].MultiUserDBase := 0;
  DBFHeader.Fields[intCurColumn].SetFields := 0;
  FillChar(DBFHeader.Fields[intCurColumn].Reserved2, SizeOf(DBFHeader.Fields[intCurColumn].Reserved2){8}, #0);
  DBFHeader.Fields[intCurColumn].MDXFlag := 0;

  if (intCurColumn = 0) then
    DBFHeader.Fields[intCurColumn].Address := 1
  else
    DBFHeader.Fields[intCurColumn].Address := DBFHeader.Fields[intCurColumn-1].Address + DBFHeader.Fields[intCurColumn-1].Length;
  DBFHeader.RecordSize := DBFHeader.Fields[intCurColumn].Address + DBFHeader.Fields[intCurColumn].Length;
end;

procedure TSMExportToDBF.WriteRowStart(IsAddedTitle: Boolean);
begin
  {mark for deleted records}
  WriteString(' ');
end;

procedure TSMExportToDBF.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);

  function PadChar(const Value: string; intLength: Integer; ch: Char; AddToLeft: Boolean): string;
  var
    intStrLength: LongInt;
  begin
    Result := Value;
    intStrLength := Length(Result);
    if (intStrLength > intLength) then
    begin
      if AddToLeft then
        Delete(Result, 1, intStrLength-intLength)
      else
        Result := Copy(Result, 1, {intStrLength-}intLength)
    end
    else
    begin
      while (intStrLength < intLength) do
      begin
        if AddToLeft then
          Result := ch + Result
        else
          Result := Result + ch;
        intStrLength := Length(Result);
      end;
    end
  end;

  function ValueToDBF: string;
  var
    i: Integer;
    intYear, intMonth, intDay: Word;
    strFormat: string;
    dt: TDateTime;
  begin
    Result := AString;
    case DBFHeader.Fields[ACol].FieldType of
      'N': begin
             Result := Trim(Result);

             if (Result <> '') or not BlankIfZero then
             begin
               if (Result = '') then
                 Result := '0';

               if (DBFHeader.Fields[ACol].Decimals > 0) then
               begin
                 {must replace DecimalSeparator to '.'}
                 strFormat := '0.' + PadChar('', DBFHeader.Fields[ACol].Decimals, '0', True);
                 Result := FormatFloat(strFormat, StrToFloat(Result));
                 if (DecimalSeparator <> '.') then
                 begin
                   i := Pos(DecimalSeparator, Result);
                   if (i > 0) then
                     Result[i] := '.'
                 end
               end
             end;
             Result := PadChar(Result, DBFHeader.Fields[ACol].Length, ' ', True);
           end;

      'D': begin
             {YYYYMMDD format}
             if (AString = '') then
               Result := '        '
             else
             begin
               dt := VarToDateTime(AString);
               DecodeDate(dt, intYear, intMonth, intDay);
               Result := PadChar(IntToStr(intYear), 4, '0', True) +
                         PadChar(IntToStr(intMonth), 2, '0', True) +
                         PadChar(IntToStr(intDay), 2, '0', True)
             end
           end;

      'L': begin
             if (AString = strTrue) then
               Result := 'T'
             else
             if (AString = strFalse) then
               Result := 'F'
             else
               Result := ' '
           end;

    else
      if (CellType = ctBlank) then
        Result := '';
      Result := PadChar(Result, DBFHeader.Fields[ACol].Length, ' ', False);
    end;
  end;

var
  s: string;
begin
  if IsDataArea then
  begin
    if (Assigned(fld) and fld.IsBlob) or
       (CellType in [ctMemo, ctGraphic]) then
    begin
      if (Assigned(fld) and fld.IsNull) or (Trim(AString) = '')then
        WriteString('          ')
      else
      begin
        s := IntToStr(MEMOHeader.NextBlock);
        while Length(s) < 10 do
          s := ' ' + s;
        WriteString(s);
        if Assigned(fld) then
          WriteMemo(fld.AsString)
        else
          WriteMemo(AString)
      end;
    end
    else
      WriteString(ValueToDBF());
  end
end;

procedure TSMExportToDBF.InternalFileCreate(const AFileName: string);
var
  s: string;
begin
  inherited InternalFileCreate(AFileName);

  {create a stream for MEMO}
  if (DBFVersion in [FoxPro, FoxProMemo]) or
     (DBFMemoType = dmFPT) then
    s := '.fpt'
  else
    s := '.dbt';

  MemoFileName := ChangeFileExt(FileName, s);
  {TODO: process a merge support if file is exists}
  MEMOStream := TFileStream.Create(MemoFileName, fmCreate);
  FillChar(MEMOHeader, SizeOf(TSMEDBTHeader), 0);
  MEMOHeader.NextBlock := 1;
  {TODO: add support for custom length of block in MEMO}
  MEMOHeader.BlockLen := 512;
  MEMOHeader.Version := arrDBFVersion[DBFVersion];

  MEMOStream.Seek(0, soFromBeginning);
  MEMOStream.Write(MEMOHeader, SizeOf(TSMEDBTHeader));
end;

procedure TSMExportToDBF.CloseFileStream;
begin
  inherited CloseFileStream;

  {flush a MEMO file}
  if Assigned(MEMOStream) then
  begin
    {update a DBT/FPT header}
    MEMOStream.Seek(0, soFromBeginning);
//    MEMOStream.Position := 0;
    MEMOStream.Write(MEMOHeader, SizeOf(TSMEDBTHeader));

    MEMOStream.Free;
    MEMOStream := nil;

    if not MemoIsExists then
      DeleteFile(MemoFileName)
  end
end;

function SwapInt(const Value: Integer): Integer;
begin
  PByteArray(@Result)[0] := PByteArray(@Value)[3];
  PByteArray(@Result)[1] := PByteArray(@Value)[2];
  PByteArray(@Result)[2] := PByteArray(@Value)[1];
  PByteArray(@Result)[3] := PByteArray(@Value)[0];
end;

procedure TSMExportToDBF.WriteMemo(Value: string);
var
  i, intTotalSize, intLenInBlock: Integer;
  s: string;
  block: TSMEBlockHeader;
begin
//  MemoIsExists := True;

  if (DBFVersion in [dBase4, dBase4Memo, FoxPro, FoxProMemo]) then
  begin
    {we must add FFh FFh 08h 00h +length in start of memo}
    intTotalSize := Length(Value) + SizeOf(TSMEBlockHeader);

    if (DBFVersion in [FoxPro, FoxProMemo]) or
       (DBFMemoType = dmFPT) then
    begin
      block.MemoType := $0008FFFF;
      block.MemoSize := intTotalSize;
    end
    else
    begin
      block.MemoType := $01000000;
      block.MemoSize := SwapInt(intTotalSize);
    end;
  end
  else
  begin
    {we must add 1Ah + 1Ah in end of memo}
    Value := Value + #$1A#$1A;
    intTotalSize := Length(Value);
  end;

  {go to end of memo-file}
  MEMOStream.Seek(0, soFromEnd);
  if (DBFVersion in [dBase3, dBase3Memo]) then
    intLenInBlock := MEMOHeader.BlockLen
  else
  begin
    MEMOStream.Write(block.MemoType, SizeOf(LongInt));
    MEMOStream.Write(block.MemoSize, SizeOf(LongInt));
    intLenInBlock := MEMOHeader.BlockLen-2*SizeOf(LongInt);
  end;
  {flush every block in 512 bytes}
  i := 0;
  while (intTotalSize > 0) do
  begin
    SetLength(s, intLenInBlock);
    FillChar(s[1], intLenInBlock, #0);
    if (intTotalSize < intLenInBlock) then
      Move(Value[i+1], s[1], intTotalSize)
    else
      Move(Value[i+1], s[1], intLenInBlock);
//    s := Copy(Value, i+1, intLenInBlock);

    { write to disk }
    MEMOStream.Write(s[1], intLenInBlock);
    MEMOHeader.NextBlock := MEMOHeader.NextBlock+1;
    Dec(intTotalSize, intLenInBlock);
    Inc(i, intLenInBlock);
    intLenInBlock := MEMOHeader.BlockLen;
  end;
end;

end.