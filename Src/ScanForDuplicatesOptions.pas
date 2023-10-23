unit ScanForDuplicatesOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PDBTAbles, ExtCtrls, PDB_Decl, ovcbase, ovcef, ovcpb,
  ovcnf;


type
  TDeleteWhich = (dwNone, dwOldestUpdate, dwNewestUpdate, dwPattern, dwEarliestAdd, dwLatestAdd, dwForNonExistantFile);

  TfrmScanForDuplicatesOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    cbDeleteDuplicateRecords: TCheckBox;
    cbComparisonMethod: TComboBox;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    rbDeleteOldest: TRadioButton;
    rbDeleteNewest: TRadioButton;
    rbDeleteMatchingPattern: TRadioButton;
    leFileName: TLabeledEdit;
    Label2: TLabel;
    cbSameDate: TCheckBox;
    cbSameFileName: TCheckBox;
    cbSameFileSize: TCheckBox;
    ovcTolerance: TOvcNumericField;
    OvcController1: TOvcController;
    Label3: TLabel;
    rbEarliestAdd: TRadioButton;
    rbLatestAdd: TRadioButton;
    cbDisplayPath: TCheckBox;
    rbForNonExistantFile: TRadioButton;
    procedure cbDeleteDuplicateRecordsClick(Sender: TObject);
    procedure cbSameDateClick(Sender: TObject);
    procedure cbSameFileNameClick(Sender: TObject);
    procedure cbSameFileSizeClick(Sender: TObject);
  private
    fDefaultLogFileName: string;
    fDeletedRecordsLogFileName: string;
    function GetScanOrder: TCurrentOrder;
    procedure SetScanOrder(const Value: TCurrentOrder);
    function GetDoDeleteRecords: TDeleteWhich;
    procedure SetDoDeleteRecords(const Value: TDeleteWhich);
    function GetFileNamePattern: string;
    procedure SetFileNamePattern(const Value: string);
    procedure Enable_Buttons;
    { Private declarations }
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    property TheScanOrder: TCurrentOrder
             read GetScanOrder
             write SetScanOrder;
    property DoDeleteRecords: TDeleteWhich
             read GetDoDeleteRecords
             write SetDoDeleteRecords;
    property TheFileNamePattern: string
             read GetFileNamePattern
             write SetFileNamePattern;
  end;

implementation

{$R *.dfm}

uses
  StStrL, PDBUtils;

const
  AllowableSortOrders = [coFileName, coPhotoDateTime, coFileSize];

{ TfrmScanForDuplicatesOptions }

constructor TfrmScanForDuplicatesOptions.Create(aOwner: TComponent);
// coNone, coFileName, coPathName, coPhotoDate, coPhotoDateTime, coFileSize, coDateAdded, coDateUpdated, coDistanceFromLocation, coLatitude, coLongitude
var
  co: TCurrentOrder;
begin
  inherited;
  with cbComparisonMethod do
    begin
      for co := Succ(coNone) to High(TCurrentOrder) do
        if co in AllowableSortOrders then
          with Order[co] do
            Items.AddObject(SQL, TObject(co));
    end;
  DoDeleteRecords            := dwNone;
  fDefaultLogfileName        := CalcLogFileName('Duplicates.txt');
  fDeletedRecordsLogFileName := CalcLogFileName('Deleted Records');
end;

function TfrmScanForDuplicatesOptions.GetScanOrder: TCurrentOrder;
begin
  with cbComparisonMethod do
    if ItemIndex >= 0 then
      result := TCurrentOrder(Items.Objects[ItemIndex])
    else
      result := coNone;
end;

procedure TfrmScanForDuplicatesOptions.SetScanOrder(
  const Value: TCurrentOrder);
begin
  with cbComparisonMethod do
    if Value in AllowableSortOrders then
      ItemIndex := Items.IndexOfObject(TObject(Value));
end;

procedure TfrmScanForDuplicatesOptions.cbDeleteDuplicateRecordsClick(
  Sender: TObject);
var
  b: boolean;
begin
  b := cbDeleteDuplicateRecords.Checked;
  rbDeleteOldest.Enabled := b;
  rbDeleteNewest.Enabled := b;
  rbDeleteMatchingPattern.Enabled := b;
  rbEarliestAdd.Enabled           := b;
  rbLatestAdd.Enabled             := b;
  rbForNonExistantFile.Enabled    := b;
end;

function TfrmScanForDuplicatesOptions.GetDoDeleteRecords: TDeleteWhich;
begin
  if cbDeleteDuplicateRecords.Checked then
    begin
      if rbDeleteOldest.Checked then
        result := dwOldestUpdate else
      if rbDeleteNewest.Checked then
        result := dwNewestUpdate else
      if rbDeleteMatchingPattern.Checked then
        result := dwPattern else
      if rbEarliestAdd.Checked then
        result := dwEarliestAdd else
      if rbLatestAdd.Checked then
        result := dwLatestAdd else
      if rbForNonExistantFile.Checked then
        result := dwForNonExistantFile
      else
        result := dwNone;
    end
  else
    result := dwNone;
end;

procedure TfrmScanForDuplicatesOptions.SetDoDeleteRecords(
  const Value: TDeleteWhich);
begin
  case Value of
    dwNone:
      cbDeleteDuplicateRecords.Checked := false;

    dwOldestUpdate:
      rbDeleteOldest.Checked := true;

    dwNewestUpdate:
      rbDeleteNewest.Checked := true;

    dwEarliestAdd:
      rbEarliestAdd.Checked := true;

    dwLatestAdd:
      rbLatestAdd.Checked := true;

    dwForNonExistantFile:
      rbForNonExistantFile.Checked := true;

    dwPattern:
      rbDeleteMatchingPattern.Checked := true;
  end;
end;

function TfrmScanForDuplicatesOptions.GetFileNamePattern: string;
begin
  result := leFileName.Text;
end;

procedure TfrmScanForDuplicatesOptions.SetFileNamePattern(
  const Value: string);
begin
  leFileName.Text := Value;
end;

procedure TfrmScanForDuplicatesOptions.Enable_Buttons;
begin
  btnOk.Enabled := (cbSameDate.Checked or cbSameFileName.Checked or cbSameFileSize.Checked) and (TheScanOrder in AllowableSortOrders);
  ovcTolerance.Enabled := cbSameFileSize.Checked;
end;

procedure TfrmScanForDuplicatesOptions.cbSameDateClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmScanForDuplicatesOptions.cbSameFileNameClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmScanForDuplicatesOptions.cbSameFileSizeClick(
  Sender: TObject);
begin
  Enable_Buttons;
end;

end.
