unit About;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    OKButton: TButton;
    lblCompiled: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses uPhotoDB;

{$R *.dfm}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  lblVersion.Caption  := 'Version: ' + cVERSION;
  lblCompiled.Caption := Format('Compiled: %s',
                                [DateTimeToStr(FileDateToDateTime(FileAge(ParamStr(0))))]);
  lblCopyright.Caption := cCOPYRIGHT;
end;

end.

