unit TrackFilterDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FilterOptions, StdCtrls, ExtCtrls, BoundariesBrowser, HikingTables;

type
  TfrmTrackFilterOptions = class(TfrmFilterOptions)
    leUpperLeft: TLabeledEdit;
    leBottomRight: TLabeledEdit;
    btnLoad: TButton;
    procedure btnLoadClick(Sender: TObject);
  private
    fBoundariesBrowser: TfrmBoundariesBrowser;
    function MyBoundariesBrowser: TfrmBoundariesBrowser;
    function BoundariesTable: TBoundariesTable;
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

implementation

uses LocationUtils;

{$R *.dfm}

function TfrmTrackFilterOptions.BoundariesTable: TBoundariesTable;
begin
  if not Assigned(gBoundariesTable) then
    gBoundariesTable := TBoundariesTable.Create(self, BOUNDING_RECTANGLES, []);
  result := gBoundariesTable;
end;

function TfrmTrackFilterOptions.MyBoundariesBrowser: TfrmBoundariesBrowser;
begin
  if not Assigned(fBoundariesBrowser) then
    fBoundariesBrowser := TfrmBoundariesBrowser.Create(nil, BoundariesTable, BoundariesTable.TableName);
  result := fBoundariesBrowser;
end;

procedure TfrmTrackFilterOptions.btnLoadClick(Sender: TObject);
begin
  inherited;
  MyBoundariesBrowser.ShowModal;
  with BoundariesTable do
    if not (Eof and Bof) then
      begin
        leUpperLeft.Text   := CalcLocationStringLong(fldULTop.AsFloat,    fldULLeft.AsFloat);
        leBottomRight.Text := CalcLocationStringLong(fldBRBottom.AsFloat, fldBRBottom.AsFloat);
      end;
end;

end.
