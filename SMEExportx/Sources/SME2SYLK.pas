{ SMExport suite
  TSMExportToSYLK component: data export into SYLK-file (Symbolic Link)

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2SYLK;

interface

uses
  Classes, Graphics, DB, SME2Cell, ExportDS, SMEEngine;

type
  TSMExportToSYLK = class(TSMExportToCellFile)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure WriteString(const s: string); override;

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

{ TSMExportToSYLK }
constructor TSMExportToSYLK.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableType := teSYLK;
end;

function TSMExportToSYLK.Extension: string;
begin
  Result := '.SLK'
end;

procedure TSMExportToSYLK.WriteString(const s: string);
begin
  inherited WriteString(s + #13#10);
end;

procedure TSMExportToSYLK.WriteFileBegin;
begin
  inherited;

  WriteString('ID;P' + KeyGenerator + ';N;E');
end;

procedure TSMExportToSYLK.WriteFileEnd;
begin
  inherited;

  WriteString('E');
end;

procedure TSMExportToSYLK.WriteDimensions(intRowCount, intColCount: Integer);
begin
  inherited;

  WriteString('B;Y' + IntToStr(intRowCount+1) + ';X' + IntToStr(intColCount+1));
end;

procedure TSMExportToSYLK.WriteColWidth(intCurColumn, intColWidth: Integer);
begin
  inherited;

  WriteString('F;W' + IntToStr(intCurColumn+1) + ' ' + IntToStr(intCurColumn+1) + ' ' + IntToStr(intColWidth));
end;

procedure TSMExportToSYLK.WriteData(fld: TField; CellType: TCellType; ARow, ACol: Integer; AString: {Short}String; al: TAlignment; font: TFont; color: TColor);
const
  ASylkAlign: array[TAlignment] of Char = ('L', 'R', 'C');
begin
  inherited;

  {write a cell coordinates and string}
  WriteString('C;Y' + IntToStr(ARow+1) + ';X' + IntToStr(ACol+1) +
              ';K"' + AString + '"');

  {write an alignment}
  WriteString('F;' + {'P0;' + }'FG0' + ASylkAlign[al]);
end;

end.

