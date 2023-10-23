{ SMExport suite
  TSMExportToClipboard component: data export into Windows Clipboard

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SME2CLP;

interface

uses
  Classes, Graphics, DB, SME2Txt;

type
  TSMExportToClipboard = class(TSMExportToText)
  private
    { Private declarations }
    strClipboard: string;
  protected
    { Protected declarations }
    procedure AfterExport; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure WriteString(const s: string); override;
    procedure WriteFileBegin; override;

    procedure CreateFileStream(const AFileName: string; intCurFileNum: Integer); override;
    procedure CloseFileStream; override;
  published
    { Published declarations }
  end;

implementation

uses ExportDS, Clipbrd, Windows, ShellAPI
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

{ TSMExportToClipboard }
constructor TSMExportToClipboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Separator := #9;
  TableType := teClipboard;
end;

procedure TSMExportToClipboard.WriteFileBegin;
begin
  inherited;

  strClipboard := '';
end;

procedure TSMExportToClipboard.CreateFileStream(const AFileName: string; intCurFileNum: Integer); 
begin
  inherited CreateFileStream('', intCurFileNum);
end;

procedure TSMExportToClipboard.CloseFileStream;
begin
  if (soMergeData in Options) then
    Clipboard.AsText := Clipboard.AsText + strClipboard
  else
    Clipboard.AsText := strClipboard;

  inherited CloseFileStream;

//  DeleteFile(PChar(FileName))
end;

procedure TSMExportToClipboard.WriteString(const s: string);
begin
  inherited WriteString(s);

  strClipboard := strClipboard + s;
end;

procedure TSMExportToClipboard.AfterExport;
begin
  case ActionAfterExport of
    aeOpenView: ShellExecute(0, 'open', 'clipbrd.exe', nil, nil, SW_SHOWNORMAL);
    aeEMail: SendMail('', Clipboard.AsText);
  else
    inherited
  end;
end;

end.

