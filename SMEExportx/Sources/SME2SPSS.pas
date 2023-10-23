{ SMExport suite
  TSMExportToSPSS component: data export into statistical SPSS format
  (very popular in statistic and education environments)

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2SPSS;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine, Windows
  {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TSMExportToSPSS = class(TSMExportToCellFile)
  private
    { Private declarations }
  protected
    { Protected declarations }
    CaseCluster: array[0..9*8-1] of Char;

    CurrentPosInOctet,
    CurrentPosInCluster: dWord;

    procedure ClearCluster;
    procedure FlushCluster;
  public
    { Public declarations }
    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteDataBegin; override;
    procedure WriteFileEnd; override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;

    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;
  published
    { Published declarations }
  end;

implementation

uses SysUtils;

const
  CompressionBIAS = 100;
  
  arrFormat: array[TCellType] of Byte = (
    0, //ctBlank
    6, //ctInteger
    5, //ctDouble
    1, //ctString
    22, //ctDateTime
    20, //ctDate
    5, //ctCurrency
    21, //ctTime
    1,
    1
    );

  {DON'T LOCALIZE}
  arrMonths: array[1..12] of string[3] = (
             'JAN', 'FEB', 'MAR',
             'APR', 'MAY', 'JUN',
             'JUL', 'AUG', 'SEP',
             'OCT', 'NOV', 'DEC');

type
  TSPSSHeader = packed record
    RecordID: array[0..3] of Char; {always `$FL2'}
    ProductName: array[0..59] of Char;
    LayoutCode: dWord; {always 2
                        PS reads this value in order to determine the file's endianness}
    CaseSize: dWord; {number of data elements per case.  This is the number of variables,
                      except that long string variables add extra data elements (one for
                      every 8 characters after the first 8}
    Compressed: dWord; {set to 1 if the data in the file is compressed, 0 otherwise}
    WeightIndex: dWord; {if one of the variables in the data set is used as a weighting
                         variable, set to the index of that variable.  Otherwise, set to 0.}
    nCases: Comp; {set to the number of cases in the file if it is known, or -1 otherwise
                   This number must include records even for second and subsequent elements of string variable}
    bias: dWord; {compression bias
                  only numbers between (bias-1) and (251-bias) can be compressed}
    CreationDate: array[0..8] of Char; {creation date in 'dd MMM yy' format, english only}
    CreationTime: array[0..7] of Char; {creation time in 'hh:mm:ss' format, 24-hour clock}
    FileLabel: array[0..63] of Char; {any custom label for file}
    Padding: array[0..2] of Byte; {always #0#0#0}
  end;

  TSPSSVariable = packed record
    RecordID: dWord; {always 2}
    VariableType: dWord; {Variable type code.
                          Set to 0 for a numeric variable.  For a short
                          string variable or the first part of a long string variable, this
                          is set to the width of the string.  For the second and subsequent
                          parts of a long string variable, set to -1, and the remaining
                          fields in the structure are ignored.}
    HasVarLabel: dWord; {if this variable has a variable label, set to 1; otherwise, set to 0.}
    nMissingValues: dWord; {if the variable has no missing values, set to 0.  If the variable
                            has one, two, or three discrete missing values, set to 1, 2, or 3,
                            respectively.  If the variable has a range for missing variables,
                            set to -2; if the variable has a range for missing variables plus
                            a single discrete value, set to -3.}
    PrintFormat: dWord; {Print format for this variable}
    WriteFormat: dWord; {Write format for this variable}
    Name: array[0..7] of Char; {Variable name.
                                The variable name must begin with a capital letter
                                or the at-sign (`@').  Subsequent characters may also be
                                octothorpes (`#'), dollar signs (`$'), underscores (`_'), or full
                                stops (`.').  The variable name is padded on the right with spaces}
  end;

  TSPSSMachine32Info = packed record
    RecordID: dWord; {always 7}
    SubTypeID: dWord; {always 3}
    Size: dWord; {size of each piece of data in data part. Always 4}
    Count: dWord; {number of pieces of data in the data part. Always 8}

    MajorVersion: dWord; {major version number (x in x.y.z)}
    MinorVersion: dWord; {minor version number (y in x.y.z)}
    RevisionVersion: dWord; {revision version number (z in x.y.z)}
    MachineCode: dWord; {-1 by default but other values may appear}
    FloatingPointRep: dWord; {floating point representation code.
                              1 for IEEE 754
                              2 for IBM 370
                              3 for DEC VAX E}
    CompressionCode: dWord; {always 1}
    Endianness: dWord; {machine endianness:
                        1 for big-endian
                        2 for little-endiane}
    CharacterCode: dWord; {character code:
                           1 for EBCDIC
                           2 for 7-bit ASCII
                           3 for 8-bit ASCII
                           4 for DEC Kanji}
  end;

  TSPSSDictionaryTermination = packed record
    RecordID: dWord; {always 999}
    Filler: dWord; {Ignored padding. Should be set to 0}
  end;

  TSPSSEndOfFile = packed record
    RecordID: dWord; {always $FC}
    Filler: dWord; {Ignored padding. Should be set to 0}
  end;

{ TSMExportToSPSS }
constructor TSMExportToSPSS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teSPSS;
end;

function TSMExportToSPSS.Extension: string;
begin
  Result := '.SAV'
end;

function PadRSpace(const s: string; Len: Integer): string;
var
  i, j: Integer;
begin
  i := Len - Length(s);
  if (i < 0) then
    Result := Copy(s, 1, Len)
  else
  begin
    Result := s;

    if (i > 0) then
      for j := 1 to i do
        Result := Result + ' '
  end
end;

function DateToSPSSString(dt: TDateTime): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(dt, Year, Month, Day);
  if Day < 10 then
    Result := '0' + IntToStr(Day)
  else
    Result := IntToStr(Day);
  Result := Result + ' ' + arrMonths[Month] + ' ' + Copy(IntToStr(Year), 3, 2);
//  Result := IntToStr(Day) + '/' + arrMonths[Month] + '/' + IntToStr(Year);
end;

procedure MoveStringToArray(var arr: array of Char; const s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    arr[i-1] := s[i]
end;

procedure TSMExportToSPSS.WriteDimensions(intRowCount, intColCount: Integer);
var
  ACol: Integer;
  CellType: TCellType;

  Header: TSPSSHeader;
begin
  inherited;

  {file header}
  Header.RecordID := '$FL2';
  MoveStringToArray(Header.ProductName, PadRSpace('@(#) SPSS DATA FILE SMExport http://www.scalabium.com ' + KeyGenerator, 60));
  Header.LayoutCode := 2;

  Header.CaseSize := intColCount-1;
  for ACol := 0 to Columns.Count-1 do
    if Columns[ACol].Visible then
    begin
      CellType := GetFieldType(SourceDataEngine.FindFieldByColumn(Columns[ACol]), BlankIfZero);
      if (CellType in [ctString, ctMEMO]) then
        {long string variables add extra data elements (one for every 8 characters after first 8)}
        Header.CaseSize := Header.CaseSize + dWord((Columns[ACol].Width-1) div 8);
    end;

  Header.Compressed := 1;
  Header.WeightIndex := 0;
  Header.nCases := intRowCount;
  if AddTitle then
  begin
    if (soBlankRowAfterCaptions in Options) then
      Header.nCases := Header.nCases-1
  end;
  Header.bias := {0}CompressionBIAS;
  MoveStringToArray(Header.CreationDate, DateToSPSSString(Date()));
  MoveStringToArray(Header.CreationTime, FormatDateTime('hh:nn:ss', Time()));
  MoveStringToArray(Header.FileLabel, PadRSpace('', 64));
  FillChar(Header.Padding, 3, 0);
  OutputStream.Write(Header, SizeOf(Header));
end;

function AddPadRToOctet(const Value: string; PadLen: Integer): string;
begin
  Result := Value;
  while Length(Result) mod PadLen <> 0 do
    Result := Result + ' ';
end;

function GetUndefinedWidth(Value: Integer): Integer;
begin
  Result := Value;
  if Result = 0 then
    Result := 10;
end;

function UniqueFieldName(Columns: TSMEColumns; FieldName: string; Len, intFieldCount: Integer): string;
var
  i: Integer;
  ch: Char;
  strTrail: string;
begin
  for i := 1 to Length(FieldName) do
    if (FieldName[i] = ' ') then
      FieldName[i] := '_';

  Result := FieldName;
  for i := 0 to intFieldCount{Columns.Count}-1 do
    if (Columns[i].FieldName = Result) then
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
          Result := Copy(Result, 1, Len-Length(strTrail)) + strTrail;
          Result := UniqueFieldName(Columns, Result, Len, intFieldCount);

          break;
        end
      end;
      break
    end;
end;

{write column descriptions}
procedure TSMExportToSPSS.WriteDataBegin;
var
  i, ACol: Integer;
  CellType: TCellType;
  Buffer: dWord;
  strCaption: string;

  Variable: TSPSSVariable;
  Machine32Info: TSPSSMachine32Info;
  DictionaryTermination: TSPSSDictionaryTermination;
begin
  inherited;

  for ACol := 0 to Columns.Count-1 do
    if Columns[ACol].Visible then
    begin
      FillChar(Variable, SizeOf(TSPSSVariable), #0);
      Variable.RecordID := 2;

      CellType := GetFieldType(SourceDataEngine.FindFieldByColumn(Columns[ACol]), BlankIfZero);
      if (CellType in [ctString, ctMEMO]) then
        Variable.VariableType := GetUndefinedWidth(Columns[ACol].Width)
      else
        Variable.VariableType := 0;

      Variable.HasVarLabel := 0; {1}
      Variable.nMissingValues := 0;

      Variable.PrintFormat := 0;
      {precision}
      Variable.PrintFormat := 0;
      {column width}
      if (CellType in [ctDouble, ctCurrency]) then
        Variable.PrintFormat := 2+256*8 {8 is for width}
      else
      if (CellType = ctDateTime) then
        Variable.PrintFormat := 256*12
      else
      if (CellType in [ctString, ctMEMO]) then
        Variable.PrintFormat := 256*GetUndefinedWidth(Columns[ACol].Width);
      {format type}
      Variable.PrintFormat := Variable.PrintFormat + 256*256*arrFormat[CellType];

      Variable.WriteFormat := Variable.PrintFormat;
      {variable's name}
      MoveStringToArray(Variable.Name, PadRSpace(UpperCase(UniqueFieldName(Columns, Columns[ACol].FieldName, 8, ACol)), 8));
      OutputStream.Write(Variable, SizeOf(TSPSSVariable));

      {variable's label/description}
      if Variable.HasVarLabel = 1 then
      begin
        if (soUseFieldNameAsCaption in Options) then
          strCaption := Columns[ACol].FieldName
        else
          strCaption := Columns[ACol].Title.Caption;
        {max 120 characters supported}
        strCaption := AddPadRToOctet(Copy(strCaption, 1, 120), 4);
        Buffer := Length(strCaption);
        OutputStream.Write(Buffer, SizeOf(Buffer));
        WriteString(strCaption);
      end;

      if (CellType in [ctString, ctMEMO]) then
      begin
        for i := 0 to ((GetUndefinedWidth(Columns[ACol].Width)-1) div 8)-1 do
        begin
          {flush all elements for long string variables}
          Variable.VariableType := $FFFFFFFF;
          Variable.HasVarLabel := 0;
          Variable.PrintFormat := 256*GetUndefinedWidth(Columns[ACol].Width);
          Variable.WriteFormat := Variable.PrintFormat;
          MoveStringToArray(Variable.Name, PadRSpace('', 8));
          OutputStream.Write(Variable, SizeOf(TSPSSVariable));
        end
      end;

      {missing values}
      {empty}
    end;

  {save machine information}
  Machine32Info.RecordID := 7;
  Machine32Info.SubTypeID := 3;
  Machine32Info.Size := 4;
  Machine32Info.Count := 8;
  Machine32Info.MajorVersion := 7;
  Machine32Info.MinorVersion := 5;
  Machine32Info.RevisionVersion := 0;
  Machine32Info.MachineCode := $02D0;
  Machine32Info.FloatingPointRep := 1; {IEEE 754}
  Machine32Info.CompressionCode := 1;
  Machine32Info.Endianness := 2;
  Machine32Info.CharacterCode := 2;
  OutputStream.Write(Machine32Info, SizeOf(TSPSSMachine32Info));

  {we must export "end of dictionary" record}
  DictionaryTermination.RecordID := 999;
  DictionaryTermination.Filler := 0;
  OutputStream.Write(DictionaryTermination, SizeOf(TSPSSDictionaryTermination));

  ClearCluster;
{  Buffer := $FDFDFDFD;
  OutputStream.Write(Buffer, SizeOf(Buffer));
  Buffer := $FDFDFDFD;
  OutputStream.Write(Buffer, SizeOf(Buffer));
}end;

procedure TSMExportToSPSS.WriteFileEnd;
var
  EndOfFile: TSPSSEndOfFile;
begin
  if CurrentPosInOctet > 0 then
    FlushCluster;

  {end of file}
  EndOfFile.RecordID := $FC;
  EndOfFile.Filler := 0;
  OutputStream.Write(EndOfFile, SizeOf(TSPSSEndOfFile));

  inherited
end;

procedure TSMExportToSPSS.ClearCluster;
var
  i: Integer;
begin
  {clear cluster for case}
  FillChar(CaseCluster, SizeOf(CaseCluster), #$20);
  {cluster header}
  for i := 0 to 7 do
    CaseCluster[i] := #$FD;
  CurrentPosInOctet := 0;
  CurrentPosInCluster := 8;
end;

procedure TSMExportToSPSS.FlushCluster;
begin
  {flush cluster to stream}
  OutputStream.Write(CaseCluster, CurrentPosInCluster{-1});

  ClearCluster;
end;

procedure TSMExportToSPSS.WriteRowEnd(IsAddedTitle: Boolean);
begin
//  FlushCluster;

  inherited
end;

procedure TSMExportToSPSS.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  Size: Word;
  aInt, intError,
  i, intStart, intWidth: Integer;
  aDbl: Double;
  strValue, strPart: ShortString;
begin
//exit;

  inherited;

  if (soFieldMask in Options) then
    CellType := ctString
  else
  begin
    if (AString = '') and
       (CellType <> ctString) then
      CellType := ctBlank
  end;
  if not (CellType in [ctBlank, ctInteger, ctDouble, ctDateTime, ctDate, ctCurrency, ctTime]) then
//  if (CellType = ctString) then
    strValue := AString;

  Size := 0;
  aInt := -1;
  case CellType of
    ctBlank: ;
    ctInteger: begin
                 Size := 8;

                 Val(AString, aInt, intError);
                 if intError <> 0 then
                   aInt := 0;
               end;

    ctDouble,
    ctCurrency: begin
                  Size := 8;
                  aDbl := 0;
                  if AString <> '' then
                  try
                    case CellType of
                      ctDouble,
                      ctCurrency: begin
                                    aDbl := StrToFloat(AString);
{                                    if Frac(aDbl) = 0 then
                                    begin
                                      aInt := Trunc(aDbl);
                                      CellType := ctInteger
                                    end
}                                  end;
                    end;
                  except
                  end;
                end;

    ctDate,
    ctTime,
    ctDateTime: begin
                  {Dates are stored internally as the number of seconds
                   from the beginning of the Gregorian calendar (midnight, 14th October 1582!)

                  So all Gregorian calendar dates between the years 1582 and 9999 are handled correctly by SPSS}

                  Size := 8;
                  aDbl := 0;
                  if AString <> '' then
                  try
                    case CellType of
                      ctDate: aDbl := Trunc(StrToDate(AString));
                      ctDateTime: aDbl := StrToDateTime(AString);
                      ctTime: aDbl := StrToTime(AString);
                    end;
//                    Move(aDbl, aInt, 8);
//                    aDbl := aDbl +  693594
                    aDbl := aDbl - EncodeDate(1582, 10, 14);
                    aDbl := aDbl*86400{secs per day: 24*3600}
                  except
                  end;
                end;

  else // ctString
  end;

  {export value}
  if (CellType in [ctString, ctMEMO]) then
  begin
    {add right spaces if required}
    if Assigned(fld) then
    begin
      i := Columns.ColumnByFieldName(fld.FieldName);
      if i < 0 then
        intStart := fld.Size
      else
        intStart := Columns[i].Width
    end
    else
      intStart := Columns[ACol].Width;

    intStart := GetUndefinedWidth(intStart);
    strValue := AddPadRToOctet(PadRSpace(strValue, intStart), 8);
    intWidth := Length(strValue);

    {move string to cluster}
    for intStart := 1 to (intWidth div 8) do
    begin
      strPart := Copy(strValue, (intStart-1)*8+1, 8);
      if Trim(strPart) = '' then
      begin
        {mark as blank value}
        CaseCluster[CurrentPosInOctet] := #$FE;
      end
      else
      begin
        CaseCluster[CurrentPosInOctet] := #$FD;

        Move(strPart[1], CaseCluster[CurrentPosInCluster], 8);
        {shift current position in cluster}
        Inc(CurrentPosInCluster, 8);
      end;

      {shift current position in cluster}
      Inc(CurrentPosInOctet);

      if (CurrentPosInOctet > 7) or (CurrentPosInCluster >= SizeOf(CaseCluster)) then
        FlushCluster;
    end;
  end
  else
  begin
    if (Size > 0) or
       (aInt <> -1) then
    begin
      if (CellType = ctInteger) and
         (aInt >= 1 - CompressionBIAS) and
         (aInt <= 251 - CompressionBIAS) then
      begin
        CaseCluster[CurrentPosInOctet] := Chr(aInt + CompressionBIAS);
      end
      else
      begin
        {move value to cluster}
        if aInt <> -1 then
          Move(aInt, CaseCluster[CurrentPosInCluster], 8{Size})
        else
          Move(aDbl, CaseCluster[CurrentPosInCluster], 8{Size});
        {shift current position in cluster}
        Inc(CurrentPosInCluster, 8{Size});
      end
    end
    else
      {mark as missed value}
      CaseCluster[CurrentPosInOctet] := #$FE;
  end;
  {shift current position in octet/header of cluster}
  if (CellType in [ctString, ctMEMO]) then
  else
    Inc(CurrentPosInOctet);

  if (CurrentPosInOctet > 7) or (CurrentPosInCluster >= SizeOf(CaseCluster)) then
    FlushCluster;
end;

end.

