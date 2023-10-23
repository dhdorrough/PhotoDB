{ SMExport suite
  Dialog for specification saving.

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMESave;

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls;

type
  TfrmSMESaveSpec = class(TForm)
    bvlButtons: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    lblSpecName: TLabel;
    edSpecName: TEdit;
    lblFileName: TLabel;
    edFileName: TEdit;
    btnFileName: TButton;
    procedure btnFileNameClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Filter: string;
  end;

var
  frmSMESaveSpec: TfrmSMESaveSpec;

implementation

{$R *.DFM}
uses SMEUtils;

procedure TfrmSMESaveSpec.btnFileNameClick(Sender: TObject);
var
  strFileName: string;
begin
  strFileName := edFileName.Text;
  if SMEOpenSaveFileDialog(Self.Handle, 'sme', Filter, '', '', strFileName, False) then
    edFileName.Text := strFileName
end;

end.
