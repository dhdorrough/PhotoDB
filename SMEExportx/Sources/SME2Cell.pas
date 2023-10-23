{ SMExport suite
  TSMExportToCellFile basic type

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2Cell;

interface

{$I sme.inc}

uses
  Classes, Graphics, DB, ExportDS, SMEEngine;

type
  TSMExportToCellFile = class(TSMExportBaseComponent)
  private
    { Private declarations }
    FRecordCount: Integer;
    FCurInBatch: Integer;

    al: TAlignment;
    fValue: string;
    ct: TCellType;
    fld: TField;
    fontUser: TFont;
    colorUser: TColor;

    IsMyFileStream: Boolean;
  protected
    { Protected declarations }
    AdditionalHeaderCount: Integer;
    IsDataArea: Boolean;

    procedure Prepare; override;

    function MergeIsSupported: Boolean; virtual;
    function TitleIsSupported: Boolean; virtual;
    procedure InternalFileCreate(const AFileName: string); virtual;
  public
    { Public declarations }
    OutputStream: TStream;

    procedure WriteHeader; virtual;
    procedure WriteFooter; virtual;

    procedure WriteString(const s: string); virtual;
    procedure WriteFileBegin; virtual;
    procedure WriteFileEnd; virtual;
    procedure WriteDimensions(intRowCount, intColCount: Integer); virtual;
    procedure WriteColWidth(intCurColumn, intColWidth: Integer); virtual;

    procedure WriteDataBegin; virtual;
    procedure WriteDataEnd; virtual;
    procedure WriteRowStart(IsAddedTitle: Boolean); virtual;
    procedure WriteRowEnd(IsAddedTitle: Boolean); virtual;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); virtual;
    procedure WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); virtual;
//    procedure WriteFont(fn: TFont); virtual;

    procedure CreateFileStream(const AFileName: string; intCurFileNum: Integer); virtual;
    procedure CloseFileStream; virtual;
    procedure SaveToStream(Stream: TStream);
  published
    { Published declarations }
    property ActionAfterExport;
    property FileName;
    property AddTitle;
    property CharacterSet;
    property Header;
    property Footer;
    property RowsPerFile;
    property Layout;
    
    property OnGetCellParams;
  end;


implementation

uses SysUtils, SMEStat, Windows
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

{ TSMExportToCellFile }
function TSMExportToCellFile.MergeIsSupported: Boolean;
begin
  Result := False
end;

function TSMExportToCellFile.TitleIsSupported: Boolean;
begin
  Result := True
end;

procedure TSMExportToCellFile.WriteFileBegin;
begin
  FCurInBatch := 0;
  Statistic.CurrentRow := 0;
end;

procedure TSMExportToCellFile.WriteFileEnd;
begin
end;

procedure TSMExportToCellFile.WriteDimensions(intRowCount, intColCount: Integer);
begin
end;

procedure TSMExportToCellFile.WriteColWidth(intCurColumn, intColWidth: Integer);
begin
end;

procedure TSMExportToCellFile.WriteDataBegin;
begin
  IsDataArea := True;
end;

procedure TSMExportToCellFile.WriteDataEnd;
begin
  IsDataArea := False;
end;

procedure TSMExportToCellFile.WriteRowStart(IsAddedTitle: Boolean);
begin
end;

procedure TSMExportToCellFile.WriteRowEnd(IsAddedTitle: Boolean);
begin
  Inc(FCurInBatch);
  if Assigned(OutputStream) and
     (OutputStream is TFileStream) then
  begin
    if (FCurInBatch > 1024) then
    begin
      FlushFileBuffers(TFileStream(OutputStream).Handle);
      FCurInBatch := 0;
    end
  end;
  Statistic.CurrentRow := Statistic.CurrentRow + 1;
end;

procedure TSMExportToCellFile.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
  Statistic.CurrentCol := ACol;
end;

procedure TSMExportToCellFile.WriteMergeData(CellType: TCellType; ARow, ACol, AMergeCols: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
  WriteData(nil, CellType, ARow, ACol, AString, al, font, color);
end;

procedure TSMExportToCellFile.WriteString(const s: string);
begin
  if Assigned(OutputStream) then
    OutputStream.Write(s[1], Length(s));
end;

procedure TSMExportToCellFile.InternalFileCreate(const AFileName: string);
begin
  if not IsMyFileStream or (AFileName = '') then exit;

  if (soMergeData in Options) and
     (FileExists(AFileName)) and
     MergeIsSupported then
  begin
    OutputStream := TFileStream.Create(AFileName, fmOpenWrite);
    OutputStream.Seek(0, soFromEnd);
  end
  else
  begin
    CreateDirIfNotExists(AFileName);
    OutputStream := TFileStream.Create(AFileName, fmCreate);
  end
end;

procedure TSMExportToCellFile.CreateFileStream(const AFileName: string; intCurFileNum: Integer);
var
  strFileName: string;
  k, intMaxWidth, intBand, //intColByBand,
  intMergeCols, intCurRow, intCurColumn: Integer;
begin
  if intCurFileNum > 0 then
    CloseFileStream;

//  if (AFileName <> '') then
  begin
    if (intCurFileNum > 0) then
    begin
      strFileName := IntToStr(intCurFileNum);
      if (intCurFileNum < 10) then
        strFileName := '00' + strFileName
      else
      if (intCurFileNum < 100) then
        strFileName := '0' + strFileName;
      strFileName := ChangeFileExt(AFileName, strFileName + ExtractFileExt(AFileName))
//      strFileName := ExtractFilePath(AFileName) + ExtractFileName(AFileName) + IntToStr(intCurFileNum) + ExtractFileExt(AFileName)
    end
    else
      strFileName := AFileName;

    if Assigned(OnGetFileName) then
      OnGetFileName(Self, intCurFileNum, strFileName);

    InternalFileCreate(strFileName);
  end;

  WriteFileBegin;
  if (RowsPerFile < 1) then
    k := AdditionalHeaderCount + FRecordCount//FExportedRecordCount
  else
    k := (AdditionalHeaderCount + FRecordCount){FExportedRecordCount} mod RowsPerFile*(intCurFileNum+1);
  case Layout of
    elReversedColumnar: begin
                          WriteDimensions(LastExportColumnIndex{Columns.Count}, k+1);

                          {calculate a maximum width within Columns}
                          intMaxWidth := 0;
                          for intCurColumn := 0 to Columns.Count-1 do
                            if Columns[intCurColumn].Visible then
                              if (intMaxWidth < Columns[intCurColumn].Width) then
                                intMaxWidth := Columns[intCurColumn].Width;

                          {set a column width}
                          for intCurColumn := 0 to k do
                            WriteColWidth(intCurColumn, intMaxWidth);
                        end;
    elTabularForm: begin
                     WriteDimensions(k*(LastExportColumnIndex){Columns.Count}, AdditionalHeaderCount+2{1}{3});

                     {calculate a maximum width within Columns}
                     intMaxWidth := 0;
                     for intCurColumn := 0 to Columns.Count-1 do
                       if Columns[intCurColumn].Visible then
                         if (intMaxWidth < Columns[intCurColumn].Width) then
                           intMaxWidth := Columns[intCurColumn].Width;

                     for intCurColumn := 0 to AdditionalHeaderCount do
                       WriteColWidth(intCurColumn, intMaxWidth);
                   end;
  else //elColumnar
    WriteDimensions(k, LastExportColumnIndex+1{Columns.Count});

    {set a column width}
    k := 0;
    for intCurColumn := 0 to Columns.Count-1 do
      if Columns[intCurColumn].Visible then
      begin
        WriteColWidth(k{intCurColumn}, Columns[intCurColumn].Width);
        Inc(k)
      end;
    end;


  WriteHeader;

  WriteDataBegin;
  if AddTitle and
     TitleIsSupported and
     (Layout <> elTabularForm) then
  begin
    intCurRow := Header.Count;
    if (soExportBands in Options) and (Bands.Count > 0) then
    begin
      Statistic.CurrentSection := esBand;

      {write band row}
      WriteRowStart(True);
      k := 0;

      intCurColumn := 0;
      while (intCurColumn < Columns.Count) do
      begin
        if not Columns[intCurColumn].Visible then
        begin
          Inc(intCurColumn);
          continue;
        end;

        intBand := Columns[intCurColumn].BandIndex;
        if (intBand >= 0) and
           (intBand < Bands.Count) and
           Bands[intBand].Visible then
        begin
          with Bands[intBand] do
          begin
            colorUser := Color;
            fontUser.Assign(Font);
            al := Alignment;
            fValue := Caption;
            intMergeCols := Columns.MergedColCount(intBand, intCurColumn);
          end;

          AssignStyle(intCurRow+1, colorUser);
          ct := ctString;
          if Assigned(OnGetCellParams) then
            OnGetCellParams(Self, nil, fValue, fontUser, al, colorUser, ct);

          case CharacterSet of
            csASCII_MSDOS: fValue := UECode(True, fValue);
            csEBCDIC: fValue := EBCDICCode(fValue);
          end;

          case Layout of
            elReversedColumnar: WriteData(nil, ct, k, intCurRow, fValue, al, fontUser, colorUser);
          else //elColumnar
            WriteMergeData(ct, intCurRow, intCurColumn{k}, intMergeCols, fValue, al, fontUser, colorUser);
          end;
          Inc(intCurColumn, intMergeCols);
        end
        else
        begin
          case Layout of
            elReversedColumnar: WriteData(nil, ctBlank, k, intCurRow, '', Columns[intCurColumn].Alignment, Columns[intCurColumn].Font, Columns[intCurColumn].Color);
          else //elColumnar
            WriteData(nil, ctBlank, intCurRow, k, '', Columns[intCurColumn].Alignment, Columns[intCurColumn].Font, Columns[intCurColumn].Color);
          end;
          Inc(intCurColumn);
        end;
        Inc(k);
      end;
      {write the end of row}
      WriteRowEnd(True);
      Inc(intCurRow);
    end;

    Statistic.CurrentSection := esTitle;
    {write a title row}
    WriteRowStart(True);
    k := 0;
    for intCurColumn := 0 to Columns.Count-1 do
      if Columns[intCurColumn].Visible then
      begin
        with Columns[intCurColumn] do
        begin
          colorUser := Title.Color;
          fontUser.Assign(Title.Font);
          al := Title.Alignment;
          if (soUseFieldNameAsCaption in Options) then
            fValue := FieldName
          else
            fValue := Title.Caption;
        end;

        AssignStyle(intCurRow+1, colorUser);
        ct := ctString;
        if Assigned(OnGetCellParams) then
          OnGetCellParams(Self, nil, fValue, fontUser, al, colorUser, ct);

        case CharacterSet of
          csASCII_MSDOS: fValue := UECode(True, fValue);
          csEBCDIC: fValue := EBCDICCode(fValue);
        end;

        case Layout of
          elReversedColumnar: WriteData(nil, ct, k, intCurRow, fValue, al, fontUser, colorUser);
        else //elColumnar
          WriteData(nil, ct, intCurRow, k, fValue, al, fontUser, colorUser);
        end;
        Inc(k)
      end;
    {write the end of row}
    WriteRowEnd(True);
    Inc(intCurRow);

    {write a null row after title before data cells}
    if (soBlankRowAfterCaptions in Options) then
    begin
      WriteRowStart(True);
      k := 0;
      AssignStyle(intCurRow + 1, colorUser);
      for intCurColumn := 0 to Columns.Count-1 do
        if Columns[intCurColumn].Visible then
        begin
          case Layout of
            elReversedColumnar: WriteData(nil, ctBlank, k, Statistic.CurrentRow - 1, '', taCenter, nil, colorUser);
          else //elColumnar
            WriteData(nil, ctBlank, Statistic.CurrentRow, k, '', taCenter, nil, colorUser);
          end;
          Inc(k);
        end;
      {write the end of row}
      WriteRowEnd(True);
    end
  end;
end;

procedure TSMExportToCellFile.CloseFileStream;
begin
  if Assigned(OutputStream) then
  begin
    WriteFooter;

    if IsMyFileStream then
    begin
      OutputStream.Free;
      OutputStream := nil;
    end;
  end;
end;

procedure TSMExportToCellFile.WriteHeader;
var
  k: Integer;
begin
  Statistic.CurrentSection := esHeader;
{  if AddTitle and
     (Layout <> elTabularForm) then
  begin
    if (soBlankRowAfterCaptions in Options) then
      Statistic.CurrentRow := 2
    else
      Statistic.CurrentRow := 1
  end
  else
    Statistic.CurrentRow := 0;
  Statistic.CurrentRow := Statistic.CurrentRow + Header.Count;
}
  {write a Header}
  for k := 0 to Header.Count-1 do
  begin
    {write a title row}
    WriteRowStart(True);

    colorUser := clWhite;//Black;
    al := taLeftJustify;
    fValue := Header[k];
    AssignStyle(k, colorUser);
    ct := ctString;
    if Assigned(OnGetCellParams) then
      OnGetCellParams(Self, nil, fValue, fontUser, al, colorUser, ct);

    case CharacterSet of
      csASCII_MSDOS: fValue := UECode(True, fValue);
      csEBCDIC: fValue := EBCDICCode(fValue);
    end;

    WriteData(nil, ct, k, 1, fValue, al, fontUser, colorUser);

    {write the end of row}
    WriteRowEnd(True);
  end;
end;

procedure TSMExportToCellFile.WriteFooter;
var
  k: Integer;
begin
  Statistic.CurrentSection := esFooter;
  WriteDataEnd;

  {write a Footer}
  for k := 0 to Footer.Count-1 do
  begin
    {write a footer/summary row}
    WriteRowStart(True);

    colorUser := clWhite;//Black;
    al := taLeftJustify;
    fValue := Footer[k];
    ct := ctString;
    if Assigned(OnGetCellParams) then
      OnGetCellParams(Self, nil, fValue, fontUser, al, colorUser, ct);

    case CharacterSet of
      csASCII_MSDOS: fValue := UECode(True, fValue);
      csEBCDIC: fValue := EBCDICCode(fValue);
    end;

    WriteData(nil, ct, Statistic.CurrentRow + k, 1, fValue, al, fontUser, colorUser);

    {write the end of row}
    WriteRowEnd(True);
  end;
  WriteFileEnd;
end;

procedure TSMExportToCellFile.Prepare;
begin
  IsMyFileStream := True;
  SaveToStream(nil);
  IsMyFileStream := False;
end;

procedure TSMExportToCellFile.SaveToStream(Stream: TStream);
var
  i, k, intCurColumn, intLinNo: Integer;
  FCurFileNumber: Integer;
  IsCustomStream, Accept, IsAborted: Boolean;
  strColCaption: string;
begin
  IsCustomStream := Assigned(Stream);
  if IsCustomStream then
  begin
    InternalBeforeProcess;
//    ShowModalCallBack(nil);
  end;

  FCurFileNumber := 0;
  OutputStream := Stream;

  fontUser := TFont.Create;
  try
//    SourceDataEngine.DisableControls;
    SourceDataEngine.SavePosition;
    SourceDataEngine.First;

    FRecordCount := SourceDataEngine.RecordCount{-1};
    if ((FRecordCount = 0) and ExportIfEmpty) or (FRecordCount <> 0) then
    begin
      if AddTitle and
         TitleIsSupported then
      begin
        if (soBlankRowAfterCaptions in Options) then
          AdditionalHeaderCount := 2
        else
          AdditionalHeaderCount := 1;
        if (soExportBands in Options) and (Bands.Count > 0) then
          Inc(AdditionalHeaderCount);
      end
      else
        AdditionalHeaderCount := 0;

      Statistic.TotalCount := Header.Count + AdditionalHeaderCount + FRecordCount + Footer.Count;
      i := 0;
      CreateFileStream(FileName, FCurFileNumber);
      Statistic.CurrentSection := esData;
      intLinNo := Statistic.CurrentRow;
      while (not ProgressCanceled) and
            IsTrialCount(i) and
            (not SourceDataEngine.EOF) do
      begin
        if AnimatedStatus then
          UpdateProgressDlg(GetLanguageString(8), i);

        Accept := True;
        if Assigned(OnBeforeRecord) then
          OnBeforeRecord(Self, Accept);
        if Accept then
        begin
          if Assigned(OnProgress) then
          begin
            IsAborted := False;
            OnProgress(Self, i, SourceDataEngine.RecordCount, IsAborted);
            if IsAborted then
            begin
              SetProgressCanceled(True);
              break
            end
          end;

          if {(i = 0) or}
             ((RowsPerFile > 0) and (i > 0) and (i mod RowsPerFile = 0)) then
          begin
            Inc(FCurFileNumber);
            if Assigned(Stream) then //memory stream
            begin
//              if (i = 0) then
//                CreateFileStream('', FCurFileNumber)
            end
            else //file stream
              CreateFileStream(FileName, FCurFileNumber);
          end;

          Inc(i);

          if (Layout <> elTabularForm) then
            WriteRowStart(False);
          k := 0;
          for intCurColumn := 0 to Columns.Count-1 do
          begin
            if Columns[intCurColumn].Visible then
            begin
              with Columns[intCurColumn] do
              begin
                colorUser := Color;
                fontUser.Assign(Font);
                al := Alignment;
                if (ColumnKind = ckField) and SourceDataEngine.IsDataRow(i-1) then
                  fld := SourceDataEngine.FindFieldByColumn(Columns[intCurColumn])
                else
                  fld := nil;
              end;
              if Assigned(fld) then
              begin
                fValue := GetFieldStr(fld);
                ct := GetFieldType(fld, BlankIfZero);
              end
              else
              if (Columns[intCurColumn].ColumnKind = ckConstant) then
              begin
                fValue := Columns[intCurColumn].FieldName;
                ct := Columns[intCurColumn].DataType
              end
              else
              if (Columns[intCurColumn].ColumnKind = ckSysVar) then
              begin
                fValue := GetSysValue(Columns[intCurColumn].FieldName);
                ct := Columns[intCurColumn].DataType
              end
              else
              begin
                fValue := VarToStr(SourceDataEngine.GetFieldValue(Columns[intCurColumn]));
                ct := GetValueType(SourceDataEngine.DataTypeByColumn(Columns[intCurColumn]), fValue, BlankIfZero);
              end;
              AssignStyle(Header.Count + AdditionalHeaderCount + i, colorUser);

              if Assigned(OnGetCellParams) then
                OnGetCellParams(Self, fld, fValue, fontUser, al, colorUser, ct);

              if (CharacterSet = csASCII_MSDOS) then
                fValue := UECode(True, fValue);

              case Layout of
                elTabularForm: begin
                                 WriteRowStart(False);
                                 {column caption}
                                 with Columns[intCurColumn] do
                                 begin
                                   if (soUseFieldNameAsCaption in Options) then
                                     strColCaption := FieldName
                                   else
                                     strColCaption := Title.Caption;

                                   if AddTitle then
                                   begin
                                     WriteData(nil, ctString, intLinNo, 0, strColCaption, Title.Alignment, Title.Font, Title.Color);
                                     if (soBlankRowAfterCaptions in Options) then
                                       WriteData(nil, ctBlank, intLinNo, 1, '', Title.Alignment, Title.Font, Title.Color);
                                   end;
                                 end;
                                 {data cell}
                                 WriteData(fld, ct, intLinNo, AdditionalHeaderCount{ + 1}, fValue, al, fontUser, colorUser);

                                 {write the end of row}
                                 WriteRowEnd(False);
                                 Inc(intLinNo)
                               end;
                elReversedColumnar: WriteData(fld, ct, k, Statistic.CurrentRow, fValue, al, fontUser, colorUser);
              else //elColumnar

                SourceDataEngine.ApplyCellColors(Statistic.CurrentRow-GetTitleRowCount+1, k, al, fontUser, colorUser);
                WriteData(fld, ct, Statistic.CurrentRow, k, fValue, al, fontUser, colorUser);
              end;
              Inc(k)
            end
          end;
          {write the end of row}
          if (Layout <> elTabularForm) then
            WriteRowEnd(False)
          else
          begin
            if (soBlankRowAfterCaptions in Options) and
               (TableType in [teText, teClipboard]) then
            begin
              {add empty row}
              WriteRowStart(False);
              WriteRowEnd(False);
              Inc(intLinNo);
            end;
          end;

        end;
        SourceDataEngine.Next;
        Inc(intLinNo);

        IsAborted := False;
        if Assigned(OnAfterRecord) then
          OnAfterRecord(Self, IsAborted);
        if IsAborted then
          SetProgressCanceled(True)
      end;
    end;

    SourceDataEngine.RestorePosition;
//    SourceDataEngine.EnableControls;
  finally
    CloseFileStream;
    fontUser.Free;
  end;

  if IsCustomStream then
    InternalAfterProcess;
end;

end.