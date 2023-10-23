unit DeletionConfirmation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmDeletionConfirmation = class(TForm)
    cbDeleteTheRecord: TCheckBox;
    cbDeleteFile: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    lblFileName: TLabel;
  private
    procedure SetFileName(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    property FileName: string
             write SetFileName;
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrmDeletionConfirmation }

constructor TfrmDeletionConfirmation.Create(aOwner: TComponent);
begin
  inherited;
 
end;

destructor TfrmDeletionConfirmation.Destroy;
begin
  inherited;
end;

procedure TfrmDeletionConfirmation.SetFileName(const Value: string);
begin
  lblFileName.Caption  := Value;
  if FileExists(Value) then
    cbDeleteFile.Enabled := true
  else
    begin
      cbDeleteFile.Enabled := false;
      cbDeleteFile.Checked := false;
    end;

end;

end.
