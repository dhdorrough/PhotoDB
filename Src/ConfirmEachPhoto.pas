unit ConfirmEachPhoto;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBCtrls, ExtCtrls, RotImg;

type
  TfrmConfirmEachPhoto = class(TForm)
    lblQuestion: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    lblCaption1: TLabel;
    lblCaption2: TLabel;
    Panel1: TPanel;
    DBImage1: TRotateImage;
    Button1: TButton;
  private
    fAllowRotation: boolean;
    fProcessType: string;
    procedure SetCaption1(const Value: string);
    procedure SetCaption2(const Value: string);
    procedure SetPhotoFileName(const Value: string);
    procedure SetQuestion(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    property Question: string
             write SetQuestion;
    property PhotoFileName: string
             write SetPhotoFileName;
    property Caption1: string
             write SetCaption1;
    property Caption2: string
             write SetCaption2;
    constructor Create(aOwner: TComponent; const ProcessType: string; AllowRotation: boolean = true); reintroduce;
  end;

var
  frmConfirmEachPhoto: TfrmConfirmEachPhoto;

implementation

uses PhotoUtils, PDB_Decl, PhotoDBCommonSettings;

{$R *.dfm}

{ TfrmConfirmEachPhoto }

constructor TfrmConfirmEachPhoto.Create(aOwner: TComponent; const ProcessType: string; AllowRotation: boolean = true);
begin
  inherited Create(aOwner);

  fProcessType := ProcessType;
  fAllowRotation := AllowRotation;
  Caption1     := Format('%s this record?', [ProcessType]);
  Caption2     := '';
end;

procedure TfrmConfirmEachPhoto.SetCaption1(const Value: string);
begin
  lblCaption1.Caption := Value;
end;

procedure TfrmConfirmEachPhoto.SetCaption2(const Value: string);
begin
  lblCaption2.Caption := Value;
end;

procedure TfrmConfirmEachPhoto.SetPhotoFileName(const Value: string);
var
  mc: TMediaClass;
  mt: TMediaType;
  Ext: string;
  WasEdited: boolean;
begin
  Ext := ExtractFileExt(Value);
  mt  := MediaTypeFromExtension(Ext);
  mc  := MediaInfoArray[mt].MediaClass;
  Question := Format('Process this record [%s]?', [Value]);
  if mc in [mc_Photos, mc_Video] then
    begin
      DBImage1.Visible := true;
      InnerSizePhoto( Value,
                      DBImage1,
                      Panel1,
                      mc,
                      true,
                      CommonPhotoSettings.PhotoEditingProgram,
                      WasEdited)
    end
  else
    DBImage1.Visible := false;
end;

procedure TfrmConfirmEachPhoto.SetQuestion(const Value: string);
begin
  lblQuestion.Caption := Value;
end;

end.
