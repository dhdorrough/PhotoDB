unit ProShowParams;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmShowParams = class(TForm)
    leShowFile: TLabeledEdit;
    btnBrowse: TButton;
    rgDateFormat: TRadioGroup;
    Button2: TButton;
    Button3: TButton;
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmShowParams: TfrmShowParams;

implementation

uses MyUtils;

{$R *.dfm}

procedure TfrmShowParams.btnBrowseClick(Sender: TObject);
var
  OutFileName : string;
begin
  OutFileName := leShowFile.Text;
  if BrowseForFile('FileNames List filename', OutFileName, 'csv') then
    leShowFile.Text := OutFileName;
end;

end.
