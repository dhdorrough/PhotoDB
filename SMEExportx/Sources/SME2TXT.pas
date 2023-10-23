{ SMExport suite
  TSMExportToText component

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2TXT;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

type
  TSMETextQualifying = (tqAny, tqStringOnly, tqNoNumber);

var
  defTextQualifying: TSMETextQualifying = tqStringOnly;

type
  TSMExportToText = class(TSMExportToCellFile)
  private
    { Private declarations }
    arrWidthByColumns: Variant;

    FTextQualifying: TSMETextQualifying;
    FAddCtrlZ: Boolean;
  protected
    { Protected declarations }
    function MappingVersion: Integer; override;

    function MergeIsSupported: Boolean; override;
    function GetFieldStr(Field: TField): string; override;
  public
    { Public declarations }
    procedure WriteDimensions(intRowCount, intColCount: Integer); override;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); override;

    procedure WriteRowEnd(IsAddedTitle: Boolean); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;
    procedure WriteFooter; override;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;

    procedure GenerateOraControlFile(const AFileName: string);
  published
    { Published declarations }
    property TextQualifying: TSMETextQualifying read FTextQualifying write FTextQualifying;
    property AddCtrlZ: Boolean read FAddCtrlZ write FAddCtrlZ default False;

    property TextQualifier;
    property Separator;
    property RecordSeparator;
    property Fixed;

    property ActionAfterExport;

    property AddTitle;
    property CharacterSet;

    property Header;
    property Footer;

    property OnGetCellParams;
  end;

implementation

uses SysUtils
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

{ TSMCustomExportToText }
constructor TSMExportToText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Options := Options - [soBlankRowAfterCaptions];
  FTextQualifying := defTextQualifying;
  TableType := teText;
  FAddCtrlZ := False;
end;

function TSMExportToText.Extension: string;
begin
  if Fixed then
    Result := '.TXT'
  else
    Result := '.CSV'
end;

function TSMExportToText.MappingVersion: Integer;
begin
  if Fixed then
    Result := 1
  else
    Result := 2
end;

function TSMExportToText.MergeIsSupported: Boolean;
begin
  Result := True
end;

procedure TSMExportToText.WriteFooter;
begin
  inherited;

  if AddCtrlZ then
    WriteString(#26)
end;

procedure TSMExportToText.WriteRowEnd(IsAddedTitle: Boolean);
begin
  inherited;

  WriteString(RecordSeparator)
end;

procedure TSMExportToText.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited;

  arrWidthByColumns := VarArrayCreate([0, intColCount-1], varInteger);
end;

procedure TSMExportToText.WriteColWidth(intCurColumn, intColWidth: Integer);
begin
  inherited;

  arrWidthByColumns[intCurColumn] := intColWidth;
end;

{function ModifyTextQualifier(const s: string; chQualifier: Char): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    Result := Result + s[i];
    if (s[i] = chQualifier) then
      Result := Result + chQualifier;
  end
end;
}
function TSMExportToText.GetFieldStr(Field: TField): string;
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

procedure TSMExportToText.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
  inherited;

  if Fixed then
  begin
    if Assigned(fld) and IsNumericField(fld) then
      AString := PadLSpace(AString, arrWidthByColumns[ACol])
    else
      AString := PadRSpace(AString, arrWidthByColumns[ACol])
  end;

//  if (TextQualifier <> #0) then
  if {(AString <> '') and }(TextQualifier <> #0) then
  begin
    if (TextQualifying = tqAny) or
       (not Assigned(fld) and (CellType = ctString) and (TextQualifying in [tqStringOnly, tqNoNumber])) or
       (Assigned(fld) and
        ((IsStringField(fld) and (TextQualifying = tqStringOnly)) or
         (not (fld is TNumericField) and (TextQualifying = tqNoNumber)) or
         ((fld is TNumericField) and (Pos(Separator, AString) > 0)))) then
    begin
      {for every TextQualifier in AString, we must add a new one (must be twice)}
      AString := AnsiQuotedStr(AString, TextQualifier)
//      AString := TextQualifier + ModifyTextQualifier(AString, TextQualifier) + TextQualifier;
    end
  end;

  if (ACol < LastExportColumnIndex-1) and
     (Separator <> #0) and
     IsDataArea and
     not (AddTitle and (soBlankRowAfterCaptions in Options) and (ARow = Header.Count + 1)) then
    AString := AString + Separator;
  WriteString(AString)
end;

procedure TSMExportToText.GenerateOraControlFile(const AFileName: string);
{http://www.orafaq.com/faqloadr.htm
Sample1:
          load data
          infile 'c:\data\mydata.csv'
		  into table emp
          fields terminated by "," optionally enclosed by '"'
          ( empno, empname, sal, deptno )

10001,"Scott Tiger", 1000, 40
10002,"Frank Naude", 500, 20

Sample2:
          load data
          infile *
          replace
          into table departments
          (  dept     position (02:05) char(4),
             deptname position (08:27) char(20)
          )
        begindata
          COSC  COMPUTER SCIENCE
          ENGL  ENGLISH LITERATURE
          MATH  MATHEMATICS
          POLY  POLITICAL SCIENCE
}

const
  CRLF = #13#10;

var
  fs: TFileStream;
  strCTL: string;
  i, intCurPos: Integer;
  IsFirstField: Boolean;
  fld: TField;
begin
  strCTL := 'LOAD DATA' + CRLF +
            'INFILE *' + CRLF +
            'INTO TABLE ' + tablename + CRLF;
  if AddTitle then
  begin
    if (soBlankRowAfterCaptions in Options) then
      strCTL := strCTL + 'SKIP 2' + CRLF
    else
      strCTL := strCTL + 'SKIP 1' + CRLF;
  end;

  IsFirstField := True;
  if Fixed then
  begin
    intCurPos := 0;
    strCTL := strCTL +
              '(';
    {build field list}
    for i := 0 to Columns.Count-1 do
      if Columns[i].Visible then
      begin
        if IsFirstField then
          strCTL := strCTL + '( '
        else
          strCTL := strCTL + ', ' + CRLF;

        strCTL := strCTL + Columns[i].FieldName +
                  ' POSITION (' + AddZeros(IntToStr(intCurPos), 2, True) + ':' + AddZeros(IntToStr(Columns[i].Width), 2, True) + ') ' +
                  '';
        fld := SourceDataEngine.FindFieldByColumn(Columns[i]);
        if Assigned(fld) then
        begin
          if fld is TNumericField then
            strCTL := strCTL + 'NUMERIC'
          else
          if fld is TDateTimeField then
            strCTL := strCTL + 'DATE'
          else
            strCTL := strCTL + 'CHAR(' + IntToStr(Columns[i].Width) + ')';
        end
        else
          strCTL := strCTL + 'CHAR(' + IntToStr(Columns[i].Width) + ')';

        IsFirstField := False;
      end
  end
  else
  begin
    strCTL := strCTL +
              'FIELDS TERMINATED BY "' + Separator + '" OPTIONALLY ENCLOSED BY ''' + TextQualifier + '''' + CRLF +
              '(';

    {build field list}
    for i := 0 to Columns.Count-1 do
      if Columns[i].Visible then
      begin
        if not IsFirstField then
          strCTL := strCTL + ', ';
        strCTL := strCTL + Columns[i].FieldName;
        IsFirstField := False;
      end;
    strCTL := strCTL + ')' + CRLF;
  end;

  CreateDirIfNotExists(AFileName);
  fs := TFileStream.Create(AFileName, fmCreate);
  try
  finally
    fs.Free
  end;
end;

end.