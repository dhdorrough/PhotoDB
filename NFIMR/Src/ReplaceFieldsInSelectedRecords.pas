unit ReplaceFieldsInSelectedRecords;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, Expression, ParseExpr;

type
  TfrmReplaceFieldsInSelectedRecords = class(TForm)
    Label1: TLabel;
    lbFieldNames: TListBox;
    Label2: TLabel;
    btnExpressionParser: TButton;
    btnBegin: TButton;
    btnCancel: TButton;
    procedure btnExpressionParserClick(Sender: TObject);
    procedure lbFieldNamesClick(Sender: TObject);
  private
    { Private declarations }
    fDataSet: TDataSet;
    fExpression: string;
    fForm_Expression: TFrmExpression;
    fFieldToModify: string;
    fParser: TParser;
    function GetForm_Expression: TFrmExpression;
    function Parser: TParser;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; DataSet: TDataSet); reintroduce;
    Destructor Destroy; override;
    property Form_Expression: TfrmExpression
             read GetForm_Expression;
    property Expression: string
             read fExpression
             write fExpression;
    property FieldToModify: string
             read fFieldToModify
             write fFieldToModify;
  end;

implementation

uses MyTables, MyUtils, PhotoDBCommonSettings;

{$R *.dfm}

{ TfrmReplaceFieldsInSelectedRecords }

constructor TfrmReplaceFieldsInSelectedRecords.Create(aOwner: TComponent;
  DataSet: TDataSet);
var
  i: integer;
begin
  inherited Create(aOwner);
  fDataSet := DataSet;
  lbFieldNames.Clear;
  for i := 0 to DataSet.Fields.Count-1 do
    lbFieldNames.Items.AddObject(DataSet.Fields[i].FieldName, DataSet.Fields[i]);
  lbFieldNames.Sorted := true;
end;

procedure TfrmReplaceFieldsInSelectedRecords.btnExpressionParserClick(
  Sender: TObject);
begin
  if Form_Expression.ShowModal = mrOk then
    begin
      fExpression := Form_Expression.Expression;
      btnExpressionParser.Hint := fExpression;
      btnExpressionParser.ShowHint := true;
      btnBegin.Enabled := not Empty(fExpression);
    end;
end;



function TfrmReplaceFieldsInSelectedRecords.GetForm_Expression: TFrmExpression;
begin
  if not Assigned(fForm_Expression) then
    with fDataSet as TMyTable do
      begin
        Parser;  // force the parser to be created before the form is created
        AddOptionalFunctionsToParser(Parser);
        fForm_Expression := TFrmExpression.Create(self, CommonPhotoSettings.PhotoDBDatabaseFileName, fDataSet, Parser);
        if not Empty(FieldToModify) then
          fForm_Expression.Caption := Format('Update field [%s]', [FieldToModify])
        else
          fForm_Expression.Caption := 'Updating unknown field';
        fForm_Expression.Expression := '';
      end;
  result := fForm_Expression;
end;

procedure TfrmReplaceFieldsInSelectedRecords.lbFieldNamesClick(
  Sender: TObject);
begin
  with lbFieldNames do
    begin
      if ItemIndex >= 0 then
        fFieldToModify := Items[ItemIndex]
      else
        fFieldToModify := '';
    end;
end;

function TfrmReplaceFieldsInSelectedRecords.Parser: TParser;
begin
  if not Assigned(fParser) then
    fParser := TParser.Create;
  result := fParser;
end;

destructor TfrmReplaceFieldsInSelectedRecords.Destroy;
begin
  FreeAndNil(fParser);
  inherited;
end;

end.
                                    