{ SMExport suite

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMEStat;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type
  TfrmSMEProcess = class(TForm)
    ProgressBar: TProgressBar;
    btnCancel: TButton;
    Bevel: TBevel;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    {$IFDEF LINUX}
    {$ELSE}
    Animate: TAnimate;
    {$ENDIF}
    CancelPressed: Boolean;
  end;

implementation

{$R *.DFM}

{ TfrmSMEProcess }
procedure TfrmSMEProcess.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  {$ELSE}
  Animate := TAnimate.Create(Self);
  with Animate do
  begin
    Left := 8;
    Top := 4;
    Width := 427;
    Height := 45;
    Parent := Self;
    Active := False;
    AutoSize := False;
    CommonAVI := aviCopyFiles;
    StopFrame := 34;
    ParentColor := True;
    Transparent := True;
  end;
  {$ENDIF}
  CancelPressed := False;
end;

procedure TfrmSMEProcess.btnCancelClick(Sender: TObject);
begin
  CancelPressed := True;
end;

procedure TfrmSMEProcess.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CancelPressed := True;
end;

end.
