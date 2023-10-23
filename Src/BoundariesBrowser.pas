unit BoundariesBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BrowserUnit, Menus, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids,
  DB;

type
  TfrmBoundariesBrowser = class(TfrmDataSetBrowser)
    EditBoundaries: TMenuItem;
    CopyUpperLeftLatLong1: TMenuItem;
    CopyBottomRightLatLong1: TMenuItem;
    PasteUpperLeftLatLong1: TMenuItem;
    PasteBottomRightLatLong1: TMenuItem;
    CopyDescription1: TMenuItem;
    PasteDescription1: TMenuItem;
    N1: TMenuItem;
    CopyRecordFields1: TMenuItem;
    PasteFieldsasNewRecord1: TMenuItem;
    N2: TMenuItem;
    CopyTopLeftLocation1: TMenuItem;
    CopyBottomRightLocation1: TMenuItem;
    N3: TMenuItem;
    PastetoTopLeftLocation1: TMenuItem;
    PastetoBottomRightLocation1: TMenuItem;
    procedure CopyUpperLeftLatLong1Click(Sender: TObject);
    procedure CopyBottomRightLatLong1Click(Sender: TObject);
    procedure CopyDescription1Click(Sender: TObject);
    procedure PasteUpperLeftLatLong1Click(Sender: TObject);
    procedure PasteBottomRightLatLong1Click(Sender: TObject);
    procedure PasteDescription1Click(Sender: TObject);
    procedure CopyRecordFields1Click(Sender: TObject);
    procedure PasteFieldsasNewRecord1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CopyTopLeftLocation1Click(Sender: TObject);
    procedure CopyBottomRightLocation1Click(Sender: TObject);
    procedure PastetoTopLeftLocation1Click(Sender: TObject);
    procedure PastetoBottomRightLocation1Click(Sender: TObject);
  private
    fOneRecord            : array of variant;
    procedure Enable_Buttons;
    { Private declarations }
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; DataSet: TDataSet; DataSetName: string = ''); override;
  end;

implementation

uses
  HikingTables, ClipBrd, LocationUtils, MyUtils;

{$R *.dfm}

procedure TfrmBoundariesBrowser.CopyUpperLeftLatLong1Click(Sender: TObject);
begin
  inherited;
  with fDataSet as TBoundariesTable do
    Clipboard.AsText := Format('%11.7f %11.7f', [fldULTop.AsFloat, fldULLeft.AsFloat]);
end;

procedure TfrmBoundariesBrowser.CopyBottomRightLatLong1Click(
  Sender: TObject);
begin
  inherited;
  with fDataSet as TBoundariesTable do
    Clipboard.AsText := Format('%11.7f %11.7f', [fldBRBottom.AsFloat, fldBRRight.AsFloat]);
end;

procedure TfrmBoundariesBrowser.CopyDescription1Click(Sender: TObject);
begin
  inherited;
  with fDataSet as TBoundariesTable do
    Clipboard.AsText := fldDescription.AsString;
end;

procedure TfrmBoundariesBrowser.PasteUpperLeftLatLong1Click(
  Sender: TObject);
var
  Latitude, Longitude: double;
begin
  inherited;
  if ParseLatitudeLongitude(Clipboard.AsText, Latitude, Longitude) then
    with fDataSet as TBoundariesTable do
      begin
        Edit;
        fldULTop.AsFloat  := Latitude;
        fldULLeft.AsFloat := Longitude;
      end;
end;

procedure TfrmBoundariesBrowser.PasteBottomRightLatLong1Click(
  Sender: TObject);
var
  Latitude, Longitude: double;
begin
  inherited;
  if ParseLatitudeLongitude(Clipboard.AsText, Latitude, Longitude) then
    with fDataSet as TBoundariesTable do
      begin
        Edit;
        fldBRBottom.AsFloat  := Latitude;
        fldBRRight.AsFloat   := Longitude;
      end;
end;

procedure TfrmBoundariesBrowser.PasteDescription1Click(Sender: TObject);
begin
  inherited;
    with fDataSet as TBoundariesTable do
      begin
        Edit;
        fldDescription.AsString  := Clipboard.AsText;
      end;
end;

procedure TfrmBoundariesBrowser.CopyRecordFields1Click(Sender: TObject);
var
  i: integer;
begin
  inherited;
  with fDataSet as TBoundariesTable do
    begin
      SetLength(fOneRecord, Fields.Count);
      for i := 0 to Fields.Count-1 do
        fOneRecord[i] := Fields[i].AsVariant;
    end;
  Enable_Buttons;
end;

procedure TfrmBoundariesBrowser.Enable_Buttons;
begin
  if Assigned(fDataSet) then
    with fDataSet as TBoundariesTable do
      PasteFieldsasNewRecord1.Enabled := Fields.Count = Length(fOneRecord)
end;


procedure TfrmBoundariesBrowser.PasteFieldsasNewRecord1Click(
  Sender: TObject);
var
  i: integer;

  function Adjusted(NewValue: string): string;
  var
    LastCh: char;
  begin { Adjusted }
    if Length(NewValue) > 0 then
      begin
        LastCh := NewValue[Length(NewValue)];
        if LastCh in Digits then
          begin
            inc(LastCh);
            result := Copy(NewValue, 1, Length(NewValue)-1) + LastCh;
          end
        else
          result := NewValue + '1';
      end;
  end;  { Adjusted }

begin { TfrmBoundariesBrowser.PasteFieldsasNewRecord1Click }
  inherited;
  with fDataSet as TBoundariesTable do
    begin
      if Fields.Count = Length(fOneRecord) then
        begin
          Append;
          try
            for i := 0 to Fields.Count-1 do
              begin
                if Fields[i].FieldName = 'Abbrev' then
                  Fields[i].AsVariant := Adjusted(fOneRecord[i]) else
                if Fields[i].FieldName = 'Description' then
                  Fields[i].AsVariant := Adjusted(fOneRecord[i]) else
                if Fields[i].FieldName = 'DateAdded' then
                  Fields[i].AsVariant := Now else
                if Fields[i].FieldName = 'DateUpdated' then
                  Fields[i].AsVariant := Now
                else
                  if not Fields[i].ReadOnly then
                    Fields[i].AsVariant := fOneRecord[i];
              end;
            Post;
          except
            on e:Exception do
              AlertFmt('Unable to paste record [%s]', [e.message])
          end;
        end
      else
        Alert('System error: Copy Record has not been performed');
    end;
end;  { TfrmBoundariesBrowser.PasteFieldsasNewRecord1Click }

constructor TfrmBoundariesBrowser.Create(aOwner: TComponent;
  DataSet: TDataSet; DataSetName: string);
begin
  inherited;
  Enable_Buttons;
end;

procedure TfrmBoundariesBrowser.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if fDataSet.State in [dsEdit, dsInsert] then
    fDataSet.Post;
end;

procedure TfrmBoundariesBrowser.CopyTopLeftLocation1Click(Sender: TObject);
begin
  inherited;
  with DataSet as TBoundariesTable do
    Clipboard.AsText := Format('%10.6f %10.6f', [fldULTop.AsFloat, fldULLeft.AsFloat]);
end;

procedure TfrmBoundariesBrowser.CopyBottomRightLocation1Click(
  Sender: TObject);
begin
  inherited;
  with DataSet as TBoundariesTable do
    Clipboard.AsText := Format('%10.6f %10.6f', [fldBRBottom.AsFloat, fldBRRight.AsFloat]);
end;

procedure TfrmBoundariesBrowser.PastetoTopLeftLocation1Click(
  Sender: TObject);
var
  Latitude, Longitude: double;
begin
  inherited;
  if ParseLatitudeLongitude(Clipboard.AsText, Latitude, Longitude) then
    with DataSet as TBoundariesTable do
      begin
        Edit;
        fldULTop.AsFloat    := Latitude;
        fldULLeft.AsFloat   := Longitude;
      end;
end;

procedure TfrmBoundariesBrowser.PastetoBottomRightLocation1Click(
  Sender: TObject);
var
  Latitude, Longitude: double;
begin
  inherited;
  if ParseLatitudeLongitude(Clipboard.AsText, Latitude, Longitude) then
    with DataSet as TBoundariesTable do
      begin
        Edit;
        fldBRBottom.AsFloat := Latitude;
        fldBRRight.AsFloat  := Longitude;
      end;
end;

initialization
finalization
end.
