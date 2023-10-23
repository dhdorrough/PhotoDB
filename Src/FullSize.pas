unit FullSize;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DBCtrls, Menus, PDB_Decl, ExtCtrls, RotImg;

type
  TfrmFullSize = class(TForm)
    DBImage1: TRotateImage;
    PopupMenu1: TPopupMenu;
    UseFullScreen1: TMenuItem;
    SlideShow1: TMenuItem;
    Interval1: TMenuItem;
    N1sec1: TMenuItem;
    N2sec1: TMenuItem;
    N4sec1: TMenuItem;
    N8sec1: TMenuItem;
    N16sec1: TMenuItem;
    N32sec1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure UseFullScreen1Click(Sender: TObject);
    procedure Interval1Click(Sender: TObject);
    procedure N1sec1Click(Sender: TObject);
    procedure N2sec1Click(Sender: TObject);
    procedure N4sec1Click(Sender: TObject);
    procedure N8sec1Click(Sender: TObject);
    procedure N16sec1Click(Sender: TObject);
    procedure N32sec1Click(Sender: TObject);
  private
    { Private declarations }
    fDoingSlideShow: boolean;
    fOwner: TComponent;
    fMyWindowInfo: TMyWindowInfo;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    procedure SetSlideShowMode(TurnOn: boolean);
  end;

var
  frmFullSize: TfrmFullSize;

implementation

uses
  uPhotoDB, PhotoDBCommonSettings;

{$R *.dfm}

(*
procedure TfrmFullSize.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Escape then
    SlideShow1Click(nil);  // Same as ^S
end;
*)

procedure TfrmFullSize.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmFullSize.FormResize(Sender: TObject);
begin
  with (fOwner as TfrmPhotoDataBase) do
    SizePhoto;
end;

constructor TfrmFullSize.Create(aOwner: TComponent);
begin
  inherited;
  fOwner := aOwner;
end;

procedure TfrmFullSize.UseFullScreen1Click(Sender: TObject);
begin
  with UseFullScreen1 do
    begin
      Checked := not Checked;
      if Checked then
        begin
          SaveWindowInfo(self, fMyWindowInfo);
          WindowState := wsMaximized;
          BorderStyle := bsNone;
        end
      else
        begin
          WindowState := wsNormal;
          BorderStyle := bsSizeable;
          SetWindowInfo(self, fMyWindowInfo);
        end;
    end;

end;

procedure TfrmFullSize.SetSlideShowMode(TurnOn: boolean);
begin
  if TurnOn <> fDoingSlideShow then
    begin
      SlideShow1.Checked := TurnOn;

      if TurnOn then
        begin
          SaveWindowInfo(self, fMyWindowInfo);
          WindowState := wsMaximized;
          BorderStyle := bsNone;
        end
      else
        begin
          WindowState := wsNormal;
          BorderStyle := bsSizeable;
          SetWindowInfo(self, fMyWindowInfo);
        end;

      fDoingSlideShow := TurnOn;
    end;
end;


(*
procedure TfrmFullSize.SlideShow1Click(Sender: TObject);
begin
  with SlideShow1 do
    begin
      Checked := not Checked;
      SetSlideShowMode(Checked);
      with fOwner as TfrmPhotoDataBase do
        DoSlideShow(Checked);
    end;
end;
*)

procedure TfrmFullSize.Interval1Click(Sender: TObject);
begin
  case CommonPhotoSettings.SlideShowInterval of
    1000: n1sec1.Checked := true;
    2000: n2sec1.Checked := true;
    4000: n4sec1.Checked := true;
    8000: n8sec1.Checked := true;
    16000: n16Sec1.Checked := true;
    32000: n32sec1.Checked := true;
  end;
end;

procedure TfrmFullSize.N1sec1Click(Sender: TObject);
begin
  (fOwner as TfrmPhotoDataBase).SetInterval(1000);
end;

procedure TfrmFullSize.N2sec1Click(Sender: TObject);
begin
  (fOwner as TfrmPhotoDataBase).SetInterval(2000);
end;

procedure TfrmFullSize.N4sec1Click(Sender: TObject);
begin
  (fOwner as TfrmPhotoDataBase).SetInterval(4000);
end;

procedure TfrmFullSize.N8sec1Click(Sender: TObject);
begin
  (fOwner as TfrmPhotoDataBase).SetInterval(8000);
end;

procedure TfrmFullSize.N16sec1Click(Sender: TObject);
begin
  (fOwner as TfrmPhotoDataBase).SetInterval(16000);
end;

procedure TfrmFullSize.N32sec1Click(Sender: TObject);
begin
  (fOwner as TfrmPhotoDataBase).SetInterval(32000);
end;

end.
