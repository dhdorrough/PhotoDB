unit OverwriteOrAppend;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

const
  ooaOverwrite = 1;
  ooaAppend    = 2;
  ooaCancel    = 3;  

type
  TfrmOverWriteOrAppend = class(TForm)
    RadioGroup1: TRadioGroup;
    lblCaption: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

Function OverwriteItOrAppend(const Prompt: string): integer;

var
  frmOverWriteOrAppend: TfrmOverWriteOrAppend;

implementation

{$R *.dfm}

function OverwriteItOrAppend(const Prompt: string): integer;
begin
  if not Assigned(frmOverWriteOrAppend) then
    begin
      frmOverWriteOrAppend := TfrmOverWriteOrAppend.Create(nil);
      frmOverWriteOrAppend.lblCaption.Caption := Prompt;
    end;
  result := frmOverWriteOrAppend.ShowModal;
end;

procedure TfrmOverWriteOrAppend.btnOKClick(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0:   ModalResult := ooaOverwrite;
    1:   ModalResult := ooaAppend;
  end;
end;

initialization
finalization
  FreeAndNil(frmOverWriteOrAppend);
end.
