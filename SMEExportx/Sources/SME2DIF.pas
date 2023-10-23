{ SMExport suite
  TSMExportToDIF component: data export into DIF-file (Data Interchange Format)

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2DIF;

interface

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

type
  TSMExportToDIF = class(TSMExportToCellFile)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure WriteFileBegin; override;
    procedure WriteFileEnd; override;
    procedure WriteDimensions(intRowCount, intColCount: Integer); override;

    procedure WriteRowStart(IsAddedTitle: Boolean); override;
    procedure WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor); override;

    constructor Create(AOwner: TComponent); override;
    function Extension: string; override;
  published
    { Published declarations }
  end;

implementation

uses SysUtils;

{ TSMExportToDIF }
constructor TSMExportToDIF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teDIF;
end;

function TSMExportToDIF.Extension: string;
begin
  Result := '.DIF'
end;

procedure TSMExportToDIF.WriteFileBegin;
begin
  inherited;

  WriteString('TABLE'#13#10'0,1'#13#10 + '"' + KeyGenerator + '"'#13#10);
end;

procedure TSMExportToDIF.WriteFileEnd;
begin
  inherited;

  WriteString('-1,0'#13#10'EOD'#13#10);
end;

procedure TSMExportToDIF.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited;

  {amount of the rows}
  WriteString('VECTORS'#13#10 + '0,' + IntToStr(intRowCount+1) + #13#10'""'#13#10);
  {amount of the columns}
  WriteString('TUPLES'#13#10 + '0,' + IntToStr(intColCount+1) + #13#10'""'#13#10);

  {data start}
  WriteString('DATA'#13#10'0,0'#13#10'""'#13#10);
end;

procedure TSMExportToDIF.WriteRowStart(IsAddedTitle: Boolean);
begin
  inherited;

  WriteString('-1,0'#13#10'BOT'#13#10);
end;

procedure TSMExportToDIF.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
begin
  inherited;

  WriteString('1,0'#13#10'"' + AString + '"'#13#10);
end;

end.

