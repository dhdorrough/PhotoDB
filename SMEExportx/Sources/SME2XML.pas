{ SMExport suite
  TSMExportToXML component

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2XML;

interface

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

type
  TSMEXMLFormat = (xmlIE, xmlIECompact, xmlClientDataset, xmlElement, xmlMSSQL);

  TSMEXMLTags = class(TPersistent)
  private
    { Private declarations }
    FRecordsTag: string;
    FRecordTag: string;
    FRowTag: string;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property RecordsTag: string read FRecordsTag write FRecordsTag;
    property RecordTag: string read FRecordTag write FRecordTag;
    property RowTag: string read FRowTag write FRowTag;
  end;

  TSMEBLOBEncoding = (beNone, beEncode64);

  TSMExportToXML = class(TSMExportToCellFile)
  private
    { Private declarations }
    FFormat: TSMEXMLFormat;
    FGenerateXSL: Boolean;
    FBLOBEncoding: TSMEBLOBEncoding;

    FXMLTags: TSMEXMLTags;
    FQuoteTerm: Char;

    procedure SetXMLTags(Value: TSMEXMLTags);
  protected
    { Protected declarations }
    function TitleIsSupported: Boolean; override;
    function GetFieldStr(Field: TField): string; override;
  public
    { Public declarations }
    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteRowEnd(IsAddedTitle: Boolean); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Extension: string; override;
  published
    { Published declarations }
    property Format: TSMEXMLFormat read FFormat write FFormat;
    property GenerateXSL: Boolean read FGenerateXSL write FGenerateXSL;

    property BLOBEncoding: TSMEBLOBEncoding read FBLOBEncoding write FBLOBEncoding default beEncode64;
    property XMLTags: TSMEXMLTags read FXMLTags write SetXMLTags;
    property QuoteTerm: Char read FQuoteTerm write FQuoteTerm default '"';
  end;

var
  DefaultXMLFormat: TSMEXMLFormat = xmlIE;

implementation

uses Windows, SysUtils, SMEMIME;

const
  xslClientDataset = '<?xml version="1.0"?>'#13#10+
                     '<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">'#13#10+
                     '  <xsl:template match="/">'#13#10+
                     '    <xsl:apply-templates/>'#13#10+
                     '  </xsl:template>'#13#10+
                     #13#10+
                     '  <xsl:template match="DATAPACKET">'#13#10+
                     '    <table border="1">'#13#10+
                     '    <xsl:apply-templates select="METADATA/FIELDS"/>'#13#10+
                     '    <xsl:apply-templates select="ROWDATA/ROW"/>'#13#10+
                     '    </table>'#13#10+
                     '  </xsl:template>'#13#10+
                     #13#10+
                     '  <xsl:template match="FIELDS">'#13#10+
                     '    <tr>'#13#10+
                     '    <xsl:apply-templates/>'#13#10+
                     '    </tr>'#13#10+
                     '  </xsl:template>'#13#10+
                     #13#10+
                     '  <xsl:template match="FIELD">'#13#10+
                     '    <th>'#13#10+
                     '    <xsl:value-of select="@attrname"/>'#13#10+
                     '    </th>'#13#10+
                     '  </xsl:template>'#13#10+
                     #13#10+
                     '  <xsl:template match="ROWDATA/ROW">'#13#10+
                     '    <tr>'#13#10+
                     '    <xsl:for-each select="@*">'#13#10+
                     '      <td>'#13#10+
                     '      <xsl:value-of/>'#13#10+
                     '      </td>'#13#10+
                     '    </xsl:for-each>'#13#10+
                     '    </tr>'#13#10+
                     '  </xsl:template>'#13#10+
                     #13#10+
                     '</xsl:stylesheet>'#13#10;

  xslIE = '<?xml version="1.0"?>'#13#10+
          '<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">'#13#10+
          '  <xsl:template match="/">'#13#10+
          '    <xsl:apply-templates/>'#13#10+
          '  </xsl:template>'#13#10+
          #13#10+
          '  <xsl:template match="RECORDS">'#13#10+
          '    <table border="1">'#13#10+
          '    <xsl:apply-templates select="METADATA/FIELDS"/>'#13#10 +
          '    <xsl:apply-templates select="RECORD/ROW"/>'#13#10+
          '    </table>'#13#10+
          '  </xsl:template>'#13#10+
          #13#10+
          '  <xsl:template match="FIELDS">'#13#10+
          '    <tr>'#13#10+
          '    <xsl:apply-templates/>'#13#10+
          '    </tr>'#13#10+
          '  </xsl:template>'#13#10+
          #13#10+
          '  <xsl:template match="FIELD">'#13#10+
          '    <th>'#13#10+
          '    <xsl:value-of select="@attrname"/>'#13#10+
          '    </th>'#13#10+
          '  </xsl:template>'#13#10+
          #13#10+
          '  <xsl:template match="ROW">'#13#10+
          '    <tr>'#13#10+
          '    <xsl:for-each select="@*">'#13#10+
          '      <td>'#13#10+
          '      <xsl:value-of/>'#13#10+
          '      </td>'#13#10+
          '    </xsl:for-each>'#13#10+
          '    </tr>'#13#10+
          '  </xsl:template>'#13#10+
          #13#10+
          '</xsl:stylesheet>'#13#10;
  xslElement = '<?xml version="1.0"?>'#13#10+
               '<xsl:stylesheet xmlns:xsl="http://www.w3.org/TR/WD-xsl">'#13#10+
               '  <xsl:template match="/">'#13#10+
               '    <xsl:apply-templates/>'#13#10+
               '  </xsl:template>'#13#10+
               #13#10+
               '  <xsl:template match="RECORDS">'#13#10+
               '    <table border="1">'#13#10+
               '    <xsl:apply-templates select="METADATA/FIELDS"/>'#13#10 +
               '    <xsl:apply-templates select="RECORD"/>'#13#10+
               '    </table>'#13#10+
               '  </xsl:template>'#13#10+
               #13#10+
               '  <xsl:template match="FIELDS">'#13#10+
               '    <tr>'#13#10+
               '    <xsl:apply-templates/>'#13#10+
               '    </tr>'#13#10+
               '  </xsl:template>'#13#10+
               #13#10+
               '  <xsl:template match="FIELD">'#13#10+
               '    <th>'#13#10+
               '    <xsl:value-of select="@attrname"/>'#13#10+
               '    </th>'#13#10+
               '  </xsl:template>'#13#10+
               #13#10+
               '  <xsl:template match="RECORD">'#13#10+
               '    <tr>'#13#10+
               '    <xsl:for-each>'#13#10+
               '      <td>'#13#10+
               '      <xsl:value-of/>'#13#10+
               '      </td>'#13#10+
               '    </xsl:for-each>'#13#10+
               '    </tr>'#13#10+
               '  </xsl:template>'#13#10+
               #13#10+
               '</xsl:stylesheet>'#13#10;


{ TSMEXMLTags }
procedure TSMEXMLTags.Assign(Source: TPersistent);
var
  tags: TSMEXMLTags;
begin
  if Source is TSMEXMLTags then
  begin
    tags := TSMEXMLTags(Source);

    RecordsTag := tags.RecordsTag;
    RecordTag := tags.RecordTag;
    RowTag := tags.RowTag;
  end
  else
    inherited Assign(Source);
end;

{ TSMExportToXML }
constructor TSMExportToXML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FXMLTags := TSMEXMLTags.Create;
  FXMLTags.RecordsTag := 'RECORDS';
  FXMLTags.RecordTag := 'RECORD';
  FXMLTags.RowTag := 'ROW';

  FBLOBEncoding := beEncode64;

  FQuoteTerm := '"';
  TableType := teXML;
  Format := DefaultXMLFormat;

  DataFormats.DateOrder := doYMD;
  DataFormats.FourDigitYear := True;
  DataFormats.LeadingZerosInDate := True;
  DataFormats.CustomDateTimeFormat := 'yyyymmddThhnnsszzz'
end;

destructor TSMExportToXML.Destroy;
begin
  FXMLTags.Free;

  inherited Destroy;
end;

function TSMExportToXML.Extension: string;
begin
  Result := '.XML'
end;

function TSMExportToXML.TitleIsSupported: Boolean;
begin
  Result := False
end;

procedure TSMExportToXML.SetXMLTags(Value: TSMEXMLTags);
begin
  FXMLTags.Assign(Value);
end;

function TSMExportToXML.GetFieldStr(Field: TField): string;

  function GetDig(i, j: Word): string;
  begin
    Result := IntToStr(i);
    while (Length(Result) < j) do
      Result := '0' + Result;
  end;

var
  Hour, Min, Sec, MSec: Word;
begin
  Result := inherited GetFieldStr(Field);

  if not Assigned(Field) or (BlankIfZero and Field.IsNull) then
  begin
    if (FQuoteTerm <> #0) then
      Result := FQuoteTerm + FQuoteTerm//'""'
    else
      Result := '';
    exit
  end
  else
  begin
    case Field.DataType of
      ftBoolean: begin
                   {Result := UpperCase(Field.AsString);}
                   if Field.AsBoolean then
                     Result := DataFormats.BooleanTrue
                   else
                     Result := DataFormats.BooleanFalse
                 end;
      ftDate: if Field.IsNull then
                Result := ''
              else
                Result := FormatDateTime('yyyymmdd', Field.AsDateTime);
      ftTime: if Field.IsNull then
                Result := ''
              else
                Result := FormatDateTime('hhnnss', Field.AsDateTime);
      ftDateTime: begin
                    if Field.IsNull then
                      Result := ''
                    else
                    begin
                      Result := FormatDateTime('yyyymmdd', Field.AsDateTime);
                      DecodeTime(Field.AsDateTime, Hour, Min, Sec, MSec);
                      if (Hour <> 0) or (Min <> 0) or (Sec <> 0) or (MSec <> 0) then
                        Result := Result + 'T' + GetDig(Hour, 2) + ':' + GetDig(Min, 2) + ':' + GetDig(Sec, 2) + GetDig(MSec, 3);
                    end
                  end;
    else
    end;
  end;

  if Assigned(Field) and Field.IsBlob then
  begin
    if (Result <> '') then
    begin
      if (FBLOBEncoding = beEncode64) then
        Result := Encode64(Result);
      if (Format = xmlClientDataset) then
//        Result := '<![CDATA[' + s + ']]>'
      else
        Result := '[CDATA[' + Result + ']]'
    end
  end
  else
    Result := ReplaceHTMLSystemChars(Result);
end;

procedure ConvertSpaces(var s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    if (s[i] in [' ', '(', ')', '%']) then
      s[i] := '_'
end;

function ReplaceTags(const S, OldTag, NewTag: string): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if (OldTag = NewTag) then
  begin
    Result := S;
    exit;
  end;

  SearchStr := S;
  Patt := OldTag;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewTag;
    NewStr := Copy(NewStr, Offset + Length(OldTag), MaxInt);
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

procedure TSMExportToXML.WriteFileBegin;

  function XMLFieldType(DataType: TFieldType; DataSize: Integer; Required, ReadOnly: Boolean): string;
  begin
    if Format = xmlMSSQL then
    begin
      if DataType = ftMemo then
        DataSize := 2147483647;
      case DataType of
        ftString,
        ftMemo: Result := 'minOccurs="0">'#13#10 +
                          '									<xsd:simpleType>'#13#10+
                          '										<xsd:restriction base="xsd:string">'#13#10+
                          '											<xsd:maxLength value="' + IntToStr(DataSize) + '"/>'#13#10+
                          '										</xsd:restriction>'#13#10+
                          '									</xsd:simpleType>'#13#10+
                          '								</xsd:element>';

        ftSmallint: Result := 'type="xsd:int" minOccurs="0"/>';
        ftInteger: Result := 'type="xsd:int" minOccurs="0"/>';
        ftWord: Result := 'type="xsd:int" minOccurs="0"/>';
        ftBoolean: Result := 'type="xsd:boolean" minOccurs="0"/>';
        ftAutoInc: Result := 'type="xsd:autoinc" minOccurs="0"/>';
        ftFloat: Result := 'type="xsd:r8" minOccurs="0"/>';
        ftCurrency: Result := 'type="xsd:money" minOccurs="0"/>';
        ftBCD: Result := 'type="xsd:r8" minOccurs="0"/>'; //??
        ftDate: Result := 'type="xsd:date" minOccurs="0"/>';
        ftTime: Result := 'type="xsd:time" minOccurs="0"/>'; //??
        ftDateTime: Result := 'type="xsd:datetime" minOccurs="0"/>';
      end
    end  
    else
    begin
      case DataType of
        ftString: Result := '"string" WIDTH="' + IntToStr(DataSize) + '"';
        ftSmallint: Result := '"i2"';
        ftInteger: Result := '"i4"';
        ftWord: Result := '"ui2"';
        ftBoolean: Result := '"boolean"';
        ftAutoInc: Result := '"i4" SUBTYPE="Autoinc"';
        ftFloat: Result := '"r8"';
        ftCurrency: Result := '"r8" SUBTYPE="Money"';
        ftBCD: Result := '"r8"'; //??
        ftDate: Result := '"date"';
        ftTime: Result := '"time"'; //??
        ftDateTime: Result := '"datetime"';
        ftMemo: Result := '"blob" SUBTYPE="Text"';
      else
        Result := '"string" WIDTH="' + IntToStr(DataSize) + '"';
      end;
      if Required then
        Result := Result + ' required="true"';
      if Readonly then
        Result := Result + ' readonly="true"';
    end
  end;

var
  i, intCount: Integer;
  s, strFieldName, strXSLFileName, strXSLBody: string;
begin
  inherited;

  strXSLFileName := ChangeFileExt(FileName, '.xsl');

  {generate XSL-file for XML}
  if GenerateXSL then
  begin
    case Format of
      xmlClientDataset: strXSLBody := xslClientDataset;
      xmlIECompact,
      xmlIE: strXSLBody := xslIE;
      xmlElement: strXSLBody := xslElement;
    end;

    if (strXSLBody <> '') then
    begin
        CreateDirIfNotExists(strXSLFileName);

        with TFileStream.Create(strXSLFileName, fmCreate) do
        try
          {replace any standard tag to custom defined}
          ReplaceTags(strXSLBody, 'RECORDS', XMLTags.RecordsTag);
          ReplaceTags(strXSLBody, 'RECORD', XMLTags.RecordTag);
          ReplaceTags(strXSLBody, 'ROW', XMLTags.RowTag);

          {write a scheme to file}
          Write(strXSLBody[1], Length(strXSLBody));
        finally
          Free
        end
    end
  end;

  s := '<?xml version="1.0" ';
//  if Format <> xmlClientDataset then
    s := s + 'encoding="windows-' + IntToStr(GetACP()) + '" ';
  s := s + 'standalone="yes"?>';

  if GenerateXSL and (strXSLBody <> '') then
    s := s + #13#10 + '<?xml-stylesheet type="text/xsl" href="' + ExtractFileName(strXSLFileName) + '"?>';

  if (Format <> xmlClientDataset) then
    s := s + '<!-- Generated by ' + KeyGenerator + '-->';

  case Format of
    xmlClientDataset: s := s + '  <DATAPACKET Version="2.0">';
    xmlIECompact: s := s + '  <' + FXMLTags.RecordsTag + '>';
    xmlIE: s := s + '  ' + #13#10'<' + FXMLTags.RecordsTag + '>'#13#10;
    xmlElement: s := s + '  ' + #13#10'<' + FXMLTags.RecordsTag + '>'#13#10;
    xmlMSSQL: s := s + '<VFPData>'#13#10;
  end;
  WriteString(s);

  if AddTitle then
  begin
    if Format = xmlMSSQL then
      WriteString('	<xsd:schema id="VFPData" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:msdata="urn:schemas-microsoft-com:xml-msdata">'#13#10+
                  '		<xsd:element name="VFPData" msdata:IsDataSet="true">'#13#10+
                  '			<xsd:complexType>'#13#10+
                  '				<xsd:choice maxOccurs="unbounded">'#13#10+
                  '					<xsd:element name="' + FXMLTags.RecordTag + '">'#13#10+
                  '						<xsd:complexType>'#13#10 +
                  '							<xsd:sequence>'#13#10)
    else
      WriteString('<METADATA><FIELDS>');

    {write th metadata}
    intCount := Columns.Count;

    for i := 0 to intCount-1 do
      if Columns[i].Visible then
      begin
        strFieldName := Columns[i].FieldName;
        ConvertSpaces(strFieldName);
        if Format = xmlMSSQL then
        begin
          s := '								<xsd:element name="' + strFieldName +
               '" ' + XMLFieldType(SourceDataEngine.DataTypeByColumn(Columns[i]),
                                        Columns[i].Width,
                                        SourceDataEngine.RequiredByColumn(Columns[i]),
                                        SourceDataEngine.ReadOnlyByColumn(Columns[i])) + '/>';
          WriteString(s+#13#10)
        end
        else
        begin
          if (strFieldName <> Columns[i].FieldName) then
            s := 'fieldname="' + Columns[i].FieldName + '" '
          else
            s := '';
          WriteString('<FIELD ' + s + 'attrname="' +
                      strFieldName +
                      '" fieldtype=' +
                      XMLFieldType(SourceDataEngine.DataTypeByColumn(Columns[i]),
                                   Columns[i].Width,
                                   SourceDataEngine.RequiredByColumn(Columns[i]),
                                   SourceDataEngine.ReadOnlyByColumn(Columns[i])) +
                      '/>');
        end
      end;
    if Format = xmlMSSQL then
      s := '							</xsd:sequence>'#13#10+
           '						</xsd:complexType>'#13#10+
           '					</xsd:element>'#13#10+
           '				</xsd:choice>'#13#10+
           '			</xsd:complexType>'#13#10+
           '		</xsd:element>'#13#10+
           '	</xsd:schema>'
    else
      s := '</FIELDS>' +
           '<PARAMS DEFAULT_ORDER="1" PRIMARY_KEY="1" LCID="1033"/>' +
           '</METADATA>';
    if (Format in [xmlIE, xmlElement, xmlMSSQL]) then
      s := s + #13#10;
    WriteString(s);
  end;
  if Format = xmlClientDataset then
    WriteString('<ROWDATA>');
end;

procedure TSMExportToXML.WriteFileEnd;
begin
  inherited;

  if Format = xmlClientDataset then
    WriteString('</ROWDATA></DATAPACKET>')
  else
  if Format = xmlMSSQL then
    WriteString('</VFPData>')
  else
    WriteString('</' + FXMLTags.RecordsTag + '>');
end;

procedure TSMExportToXML.WriteRowStart(IsAddedTitle: Boolean);
begin
  inherited;

//  if not IsAddedTitle then
//  if IsDataArea and
//     not (AddTitle and (intCurLine <= AdditionalHeaderCount)) then
    case Format of
      xmlClientDataset: WriteString('<' + FXMLTags.RowTag);
      xmlIECompact: WriteString('<' + FXMLTags.RecordTag + '><' + FXMLTags.RowTag);
      xmlElement: WriteString('<' + FXMLTags.RecordTag + '>');
      xmlMSSQL: WriteString('	<' + FXMLTags.RecordTag + '>');
    else
      WriteString('  <' + FXMLTags.RecordTag + '>'#13#10'    <' + FXMLTags.RowTag)
    end;
end;

procedure TSMExportToXML.WriteRowEnd(IsAddedTitle: Boolean);
begin
  inherited;

//  if not IsAddedTitle then
//  if IsDataArea and
//     not (AddTitle and (intCurLine <= AdditionalHeaderCount)) then
    case Format of
      xmlClientDataset: WriteString('/>');
      xmlIECompact: WriteString('/></' + FXMLTags.RecordTag + '>');
      xmlElement: WriteString(#13#10'</' + FXMLTags.RecordTag + '>'#13#10);
      xmlMSSQL: WriteString(#13#10'	</' + FXMLTags.RecordTag + '>'#13#10);
    else
      WriteString(#13#10'    />'#13#10'  </' + FXMLTags.RecordTag + '>'#13#10)
    end
end;

procedure TSMExportToXML.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
var
  strTag: string;
begin
  inherited;

//  if AddTitle and (ARow = 0) then exit;
  
  if (AString <> '') or (soExportBlankValues in Options) then
  begin
    if Assigned(fld) then
      strTag := fld.DisplayLabel //fld.FieldName
    else
      if IsDataArea then
      begin
        if (soUseFieldNameAsCaption in Options) then
          strTag := Columns[ACol].Title.Caption
        else
          strTag := Columns[ACol].FieldName
      end
      else
        strTag := 'LINE' + IntToStr(ARow);
    ConvertSpaces(strTag);

    if (FQuoteTerm <> #0) then
      AString := FQuoteTerm + AString + FQuoteTerm;
    case Format of
      xmlClientDataset: WriteString(' ' + strTag + '=' + AString);
      xmlElement: WriteString(#13#10'      <' + strTag + '>' + AString + '</' + strTag + '>');
      xmlMSSQL: WriteString(#13#10'		<' + strTag + '>' + AString + '</' + strTag + '>');
    else
      if Format = xmlIE then
        WriteString(#13#10'     ');
      WriteString(' ' + strTag + '=' + AString);
    end
  end
end;

end.

