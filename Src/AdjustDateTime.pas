unit AdjustDateTime;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ovcbase, ovcef, ovcpb, ovcnf, DBCtrls,
  PDB_Decl, PDBTables, DB, ExtCtrls, ovcsc, RotImg;

type
  TfrmAdjustDateTime = class(TForm)
    ovcMinutesToAdd: TOvcNumericField;
    Label2: TLabel;
    Label1: TLabel;
    lblOldTime: TLabel;
    lblNewTime: TLabel;
    Label5: TLabel;
    btnNo: TButton;
    btnYes: TButton;
    btnCancel: TButton;
    pnlPhoto: TPanel;
    DBImage1: TRotateImage;
    OvcSpinner1: TOvcSpinner;
    btnYesToAll: TButton;
    lblFileName: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ovcMinutesToAddChange(Sender: TObject);
    procedure pnlPhotoResize(Sender: TObject);
  private
    { Private declarations }
    fOwner: TComponent;
    fFileName: string;
    fPhotoTable: TPhotoTable;
    fDataSource: TDataSource;
    function GetNewTime: TDateTime;
    function GetOldTime: TDateTime;
    procedure SetNewTime(const Value: TDateTime);
    procedure SetOldTime(const Value: TDateTime);
    procedure SetPhotoTable(const Value: TPhotoTable);
    procedure ShowTimes;
  public
    { Public declarations }
    property OldTime: TDateTime
             read GetOldTime
             write SetOldTime;
    property NewTime: TDateTime
             read GetNewTime
             write SetNewTime;
    property PhotoTable: TPhotoTable
             read fPhotoTable
             write SetPhotoTable;
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
  end;

var
  frmAdjustDateTime: TfrmAdjustDateTime;

implementation

uses PhotoUtils, DateUtils, PhotoDBCommonSettings;


{$R *.dfm}

{ TfrmAdjustDateTime }

constructor TfrmAdjustDateTime.Create(aOwner: TComponent);
begin
  inherited;
  fOwner := aOwner;
  fDataSource := TDataSource.Create(self);
end;

destructor TfrmAdjustDateTime.Destroy;
begin
  FreeAndNil(fDataSource);
  inherited;
end;

function TfrmAdjustDateTime.GetNewTime: TDateTime;
begin
  result := IncMinute(OldTime, ovcMinutesToAdd.AsInteger);
end;

function TfrmAdjustDateTime.GetOldTime: TDateTime;
begin
  result := PhotoTable.fldPhotoDateTime.AsDateTime;
end;

procedure TfrmAdjustDateTime.SetPhotoTable(const Value: TPhotoTable);
begin
  fPhotoTable := Value;
  fDataSource.DataSet := Value;
//  DBImage1.DataSource := fDataSource;
end;

procedure TfrmAdjustDateTime.SetNewTime(const Value: TDateTime);
begin

end;

procedure TfrmAdjustDateTime.SetOldTime(const Value: TDateTime);
begin

end;

procedure TfrmAdjustDateTime.ShowTimes;
begin
  lblOldTime.Caption := DateTimeToStr(OldTime);
  lblNewTime.Caption := DateTimeToStr(NewTime);
end;


procedure TfrmAdjustDateTime.FormShow(Sender: TObject);
var
  WasEdited: boolean;
begin
  ShowTimes;
  fFileName := PhotoTable.fldFILE_NAME.AsString;
  lblFileName.Caption := fFileName;
  InnerSizePhoto( PhotoTable.PathAndFileName,
                  DBImage1,
                  pnlPhoto,
                  mc_Photos, // this ought to be gotten from the file name
                  false,
                  CommonPhotoSettings.PhotoEditingProgram,
                  WasEdited);
end;

procedure TfrmAdjustDateTime.ovcMinutesToAddChange(Sender: TObject);
begin
  ShowTimes;
end;

procedure TfrmAdjustDateTime.pnlPhotoResize(Sender: TObject);
var
  WasEdited: boolean;
begin
  InnerSizePhoto( PhotoTable.PathAndFileName,
                  DBImage1,
                  pnlPhoto,
                  mc_Photos, // this ought to be gotten from the file name
                  false,
                  CommonPhotoSettings.PhotoEditingProgram,
                  WasEdited);
end;

end.
