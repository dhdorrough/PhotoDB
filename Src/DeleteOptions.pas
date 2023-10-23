unit DeleteOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmDeleteOptions = class(TForm)
    Label1: TLabel;
    edtLogFileName: TEdit;
    btnDeleteOldest: TRadioButton;
    btnDeleteNewest: TRadioButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnBrowse: TButton;
    SaveDialog1: TSaveDialog;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDeleteOldestClick(Sender: TObject);
    procedure btnDeleteNewestClick(Sender: TObject);
  private
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetDeleteNewest: boolean;
    function GetDeleteOldest: boolean;
    procedure SetDeleteNewest(const Value: boolean);
    procedure SetDeleteOldest(const Value: boolean);
    procedure Enable_Buttons;
    { Private declarations }
  public
    { Public declarations }
    Property FileName: string
             read GetFileName
             write SetFileName;
    Property DeleteOldest: boolean
             read GetDeleteOldest
             write SetDeleteOldest;
    Property DeleteNewest: boolean
             read GetDeleteNewest
             write SetDeleteNewest;
  end;

implementation

{$R *.DFM}

procedure TfrmDeleteOptions.btnBrowseClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      DefaultExt := 'TXT';
      FileName   := edtLogFileName.Text;
      if Execute then
        edtLogFileName.Text := FileName;
    end;
end;

function TfrmDeleteOptions.GetDeleteNewest: boolean;
begin
  result := btnDeleteNewest.Checked;
end;

function TfrmDeleteOptions.GetDeleteOldest: boolean;
begin
  result := btnDeleteOldest.Checked;
end;

function TfrmDeleteOptions.GetFileName: string;
begin
  result := edtLogFileName.Text;
end;

procedure TfrmDeleteOptions.SetDeleteNewest(const Value: boolean);
begin
  btnDeleteNewest.Checked := Value;
end;

procedure TfrmDeleteOptions.SetDeleteOldest(const Value: boolean);
begin
  btnDeleteOldest.Checked := Value;
end;

procedure TfrmDeleteOptions.SetFileName(const Value: string);
begin
  edtLogFileName.Text  := Value;
end;

procedure TfrmDeleteOptions.btnDeleteOldestClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmDeleteOptions.btnDeleteNewestClick(Sender: TObject);
begin
  Enable_Buttons;
end;


procedure TfrmDeleteOptions.Enable_Buttons;
begin
  btnOk.Enabled := btnDeleteOldest.Checked or btnDeleteNewest.Checked;
end;

end.
