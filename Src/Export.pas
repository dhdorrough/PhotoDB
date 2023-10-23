unit Export;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExportDS, SME2BDE, PDB_Decl, PDBTables,
  SME2OLE, SME2Cell, SME2DBF, SME2TXT, SME2XLS, ExCnst, CheckLst;

type
  TfrmExport = class(TForm)
    leFileName: TLabeledEdit;
    btnBegin: TButton;
    btnCancel: TButton;
    SMExportToAccess1: TSMExportToAccess;
    SMExportToDBF1: TSMExportToDBF;
    SMExportToBDE1: TSMExportToBDE;
    SMExportToText1: TSMExportToText;
    SMExportToExcel1: TSMExportToExcel;
    SMExportToFoxpro1: TSMExportToDBF;
    SMExportToXLS1: TSMExportToXLS;
    btnBrowse: TButton;
    OpenDialog1: TSaveDialog;
    GroupBox1: TGroupBox;
    rbMSAccess: TRadioButton;
    rbdBase: TRadioButton;
    rbExcel: TRadioButton;
    rbDelimited: TRadioButton;
    rbFoxpro: TRadioButton;
    CheckListBox1: TCheckListBox;
    btnClearAll: TButton;
    btnSetAll: TButton;
    procedure btnBeginClick(Sender: TObject);
    procedure rbMSAccessClick(Sender: TObject);
    procedure rbExcelClick(Sender: TObject);
    procedure rbDelimitedClick(Sender: TObject);
    procedure rbdBaseClick(Sender: TObject);
    procedure rbFoxproClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnSetAllClick(Sender: TObject);
  private
    { Private declarations }
    fExporter: TSMExportBaseComponent;
    fTempPhotoTable : TPhotoTable;
    fTableTypeExport: TTableTypeExport;
    fRecordsProcessed: integer;
    procedure SetTableTypeExport(aTableTypeExport: TTableTypeExport);
    function GetTableTypeExport: TTableTypeExport;
    procedure SMExportAfterRecord(Sender: TObject; var Abort: Boolean);
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    property TableTypeExport: TTableTypeExport
             read GetTableTypeExport
             write SetTableTypeExport;
  end;

  TExportTypeInfo = record
                     Ext: string;
                     FilterStr: string;
                   end;

var
  ExportTypeInfoArray: array[TTableTypeExport] of TExportTypeInfo = (
                        ({teParadox}   Ext:'db'),
                        ({teDBase}     Ext:'dbf'),
                        ({teText}      Ext:'csv'),
                        ({teHTML}      Ext:'html'),
                        ({teXLS}       Ext:'xls'),
                        ({teExcel}     Ext:'xls'),
                        ({teWord}      Ext:'doc'),
                        ({teSYLK}      Ext:''),
                        ({teDIF}       Ext:'dif'),
                        ({teWKS}       Ext:'wks'),
                        ({teQuattro}   Ext:''),
                        ({teSQL}       Ext:'sql'),
                        ({teXML}       Ext:'xml'),
                        ({teAccess}    Ext:'mdb'),
                        ({teClipboard} Ext:''),
                        ({teRTF}       Ext:'rtf'),
                        ({teSPSS}      Ext:'spss'),
                        ({tePDF}       Ext:'pdf'),
                        ({teLDIF}      Ext:''),
                        ({teADO}       Ext:'')
    );

  ExportableTypes: TExportFormatTypes = [teDBase, teText, teXLS, teAccess];


implementation

uses PhotoDBCommonSettings, uPhotoDB, MyTables, MyTables_Decl, MyUtils,
  SMEEngine, DB;

{$R *.dfm}

procedure TfrmExport.btnBeginClick(Sender: TObject);
var
  aColumn: TSMEColumn;
  i: integer;
  ft: TFieldType;
  Field: TField;

  function FtToCt(ft: TFieldType; TableTypeExport: TTableTypeExport): TCellType;

    procedure DoAccess;
    begin { DoAccess }
      case ft of
        ftSmallint, ftInteger, ftWord, ftLargeint, ftBoolean:
          result := ctInteger;

        ftFloat:
          result := ctDouble;

        ftString:
          result := ctString;

        ftDateTime:
          result := ctDateTime;

        ftDate:
          result := ctDate;

        ftCurrency:
          result := ctCurrency;

        ftTime:
          result := ctTime;

        ftMemo:
          result := ctMEMO;

        ftGraphic:
          result := ctGraphic;
      end;
    end;  { DoAccess }

    procedure DoDBase;
    begin
      case ft of
        ftSmallint, ftInteger, ftWord, ftLargeint, ftBoolean:
          result := ctInteger;

        ftFloat:
          result := ctString;

        ftString:
          result := ctString;

        ftDateTime:
          result := ctDateTime;

        ftDate:
          result := ctDate;

        ftCurrency:
          result := ctCurrency;

        ftTime:
          result := ctTime;

        ftMemo:
          result := ctMEMO;

        ftGraphic:
          result := ctGraphic;
      end;
    end;

    procedure DoXLS;
    begin
      case ft of
        ftSmallint, ftInteger, ftWord, ftLargeint, ftBoolean:
          result := ctInteger;

        ftFloat:
          result := ctDouble;

        ftString:
          result := ctString;

        ftDateTime:
          result := ctDateTime;

        ftDate:
          result := ctDate;

        ftCurrency:
          result := ctCurrency;

        ftTime:
          result := ctTime;

        ftMemo:
          result := ctMEMO;

        ftGraphic:
          result := ctGraphic;
      end;
    end;

  begin { FtToCt }
    result := ctString;

    case TableTypeExport of
      teDBase:    DoDbase;
      teText:     DoAccess;
      teXLS:      DoXLS;
      teAccess:   DoAccess;
    end;

  end;  { FtToCt }

begin { TfrmExport.btnBeginClick }
  case TableTypeExport of
    teAccess: fExporter                 := SMExportToAccess1;
    teXLS:    fExporter                 := SMExportToXLS1;
    teText:   fExporter                 := SMExportToText1;
    teDBase:  fExporter                 := SMExportToDBF1;
//  teDBase:  fExporter                 := SMExportToFoxpro1;
  end;

  fExporter.DataSet         := fTempPhotoTable;
  fExporter.ColumnSource    := csDataset;

  {target MS Access table}
  fExporter.FileName        := leFileName.Text;
  fExporter.TableName       := 'PhotoTable';
  fExporter.AnimatedStatus  := true;
  fExporter.OnAfterRecord   := SMExportAfterRecord;

  {export with original field types}
  fExporter.Options         := [];

(*
    ftUnknown, , ,
    ftBoolean, , , ftBCD, , , ,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, , ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    , ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd
*)

  {create the selected columns}
  fExporter.Columns.Clear;

  with CheckListBox1 do
    for i := 0 to Count - 1 do
      if Checked[i] then
        begin
          aColumn               := fExporter.Columns.Add;
          aColumn.FieldName     := Items[i];
          aColumn.Title.Caption := Items[i];
          Field                 := fTempPhotoTable.Fields[Integer(Items.Objects[i])];
          ft                    := Field.DataType;
          aColumn.DataType      := FtToCt(ft, fTableTypeExport);
          aColumn.Width         := Field.Size;
        end;

  if FileExists(leFileName.Text) then
    begin
      if YesFmt('File "%s" already exists? Do you want to delete it?', [leFileName.Text]) then
        MyDeleteFile(leFileName.Text)
      else
        Exit;
    end;

  {start process}

  fRecordsProcessed := 0;

  fExporter.Execute;

  MessageFmt('%d records exported. %d errors occurred',
             [fRecordsProcessed, self.fExporter.Statistic.TotalErrors]);
end;  { TfrmExport.btnBeginClick }

constructor TfrmExport.Create(aOwner: TComponent);
var
  tt: TTableTypeExport;
  First: boolean;
  i: integer;
  frmPhotoDataBase: TfrmPhotoDataBase;
begin
  inherited;
  frmPhotoDataBase := Owner as TfrmPhotoDataBase;

  fExporter := SMExportToAccess1;
  ExportTypeInfoArray[teDBase].FilterStr := strFormat2;
  ExportTypeInfoArray[teText].FilterStr  := strFormat3;
  ExportTypeInfoArray[teXLS].FilterStr   := strFormat5;
  ExportTypeInfoArray[teExcel].FilterStr := strFormat5;
  ExportTypeInfoArray[teAccess].FilterStr := strFormatE;

  First := true;
  with OpenDialog1 do
    begin
      DefaultExt := ExportTypeInfoArray[teAccess].Ext;   // default to msAccess
      Filter     := '';
      for tt := low(TTableTypeExport) to high(TTableTypeExport) do
        if tt in ExportableTypes then
          with ExportTypeInfoArray[tt] do
            if First then
              begin
                Filter := Format('%s|*.%s', [FilterStr, Ext]);
                First  := false;
              end
            else
              Filter := Filter + '|' + Format('%s|*.%s', [FilterStr, Ext]);
    end;

  fTempPhotoTable  := TPhotoTable.Create( self,
                                        CommonPhotoSettings.PhotoDBDatabaseFileName,
                                        cFILENAMES,
                                        [optReadOnly, optNoCopyRightsTable]);
  with fTempPhotoTable do
    begin
      OnFilterRecord := frmPhotoDataBase.PhotoTableFilterRecord;
      Filtered       := true;
      Active         := true;
      SetSelectivityParserExpression(frmPhotoDataBase.Expression);
    end;

  CheckListBox1.Items.Clear;
  with fTempPhotoTable do
    for i := 0 to Fields.Count- 1 do
      CheckListBox1.Items.AddObject(Fields[i].FieldName, TObject(i));

  with CheckListBox1 do
    for i := 0 to Pred(Items.Count) do
      Checked[i] := true;
end;

procedure TfrmExport.SetTableTypeExport(aTableTypeExport: TTableTypeExport);
begin
  leFileName.Text  := ForceExtension(leFileName.Text, ExportTypeInfoArray[aTableTypeExport].Ext);
  fTableTypeExport := aTableTypeExport;
end;


procedure TfrmExport.rbMSAccessClick(Sender: TObject);
begin
  SetTableTypeExport(teAccess);
end;

procedure TfrmExport.rbExcelClick(Sender: TObject);
begin
  SetTableTypeExport(teXLS);
end;

procedure TfrmExport.rbDelimitedClick(Sender: TObject);
begin
  SetTableTypeExport(teText);
end;

procedure TfrmExport.rbdBaseClick(Sender: TObject);
begin
  SetTableTypeExport(teDBase);
end;

procedure TfrmExport.rbFoxproClick(Sender: TObject);
begin
  SetTableTypeExport(teDBase);
end;

procedure TfrmExport.btnBrowseClick(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      FileName   := leFileName.Text;
      InitialDir := ExtractFilePath(FileName);
      DefaultExt := ExtractFileExt(FileName);
      if execute then
        begin
          leFileName.Text := FileName;
        end;
    end;
end;

function TfrmExport.GetTableTypeExport: TTableTypeExport;
begin
  result := fTableTypeExport;
end;

destructor TfrmExport.Destroy;
begin
  fTempPhotoTable.Free;
  inherited;
end;

procedure TfrmExport.SMExportAfterRecord(Sender: TObject;
  var Abort: Boolean);
begin
  inc(fRecordsProcessed);
end;

procedure TfrmExport.btnClearAllClick(Sender: TObject);
var
  i: integer;
begin
  with CheckListBox1 do
    for i := 0 to Items.Count-1 do
      Checked[i] := false;
end;

procedure TfrmExport.btnSetAllClick(Sender: TObject);
var
  i: integer;
begin
  with CheckListBox1 do
    for i := 0 to Items.Count-1 do
      Checked[i] := true;
end;

end.
