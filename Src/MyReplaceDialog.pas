unit MyReplaceDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmReplaceDialog = class(TForm)
    leFindText: TLabeledEdit;
    leReplaceText: TLabeledEdit;
    OK: TButton;
    cbConfirmEachRecord: TCheckBox;
  private
    function GetFindText: string;
    function GetReplaceText: string;
    procedure SetFindText(const Value: string);
    procedure SetReplaceText(const Value: string);
    { Private declarations }
  public
    property FindText: string
             read GetFindText
             write SetFindText;
    property ReplaceText: string
             read GetReplaceText
             write SetReplaceText;
    { Public declarations }
  end;

var
  frmReplaceDialog: TfrmReplaceDialog;

implementation

{$R *.dfm}

{ TfrmReplaceDialog }

function TfrmReplaceDialog.GetFindText: string;
begin
  result := leFindText.Text;
end;

function TfrmReplaceDialog.GetReplaceText: string;
begin
  result := leReplaceText.Text;
end;

procedure TfrmReplaceDialog.SetFindText(const Value: string);
begin
  leReplaceText.Text := Value;
end;

procedure TfrmReplaceDialog.SetReplaceText(const Value: string);
begin

end;

end.
