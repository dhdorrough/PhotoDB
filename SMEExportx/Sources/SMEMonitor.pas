{ SMExport suite
  TSMExportMonitor dialog (simple alternative to wizard)

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMEMonitor;

interface

{$I sme.inc}

uses
  Windows, Classes, Graphics, Buttons, StdCtrls, ExtCtrls, Forms, Dialogs,
  ExportDS, DB, Controls;

type
  TfrmExportDlg = class(TForm)
    rgTableType: TRadioGroup;
    rgSeparator: TRadioGroup;
    edSymbol: TEdit;
    cbAddTitle: TCheckBox;
    lblCharacterSet: TLabel;
    cbCharacterSet: TComboBox;
    bvlBtn: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    lblFileName: TLabel;
    edFileName: TEdit;
    btnOpenDlg: TSpeedButton;
    cbFixed: TCheckBox;
    SaveDlg: TSaveDialog;
    cbSelectedRecord: TCheckBox;
    cbBlankIfZero: TCheckBox;
    procedure rgTableTypeClick(Sender: TObject);
    procedure edFileNameChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure rgSeparatorClick(Sender: TObject);
    procedure btnOpenDlgClick(Sender: TObject);
  private
    { Private declarations }
    TitleSaveDlg: string;
    procedure SetDefaultExt;
  public
    { Public declarations }
    strAccessTable: string;
    strFPrompt: string;
    strTName: string;
  end;

function SetExportParam(sme: TSMExportBaseComponent; var FileName, TableName: string; var TabType: TTableTypeExport;
  var csCharacterSet: TCharacterSet; var chSeparator: Char;
  var boolAddTitle, boolFixed, boolBlankIfZero: Boolean; var intSelectedRecord: Integer;
  Formats: TExportFormatTypes): Boolean;

implementation
{$R *.DFM}

uses SysUtils, TypInfo, ExCnst, MAPI,
     ShellAPI, DBGrids, SMESave, SMEUtils, IniFiles,
     SMEEngDB, SMEEngWW
     {$IFDEF SM_USE_JPEG_FOR_HTML} , jpeg {$ENDIF}
     {$IFDEF SMForDelphi6} , Variants {$ENDIF};

type
  TsmeHack = class(TSMExportBaseComponent);

function SetExportParam(sme: TSMExportBaseComponent; var FileName, TableName: string; var TabType: TTableTypeExport;
  var csCharacterSet: TCharacterSet; var chSeparator: Char;
  var boolAddTitle, boolFixed, boolBlankIfZero: Boolean; var intSelectedRecord: Integer;
  Formats: TExportFormatTypes): Boolean;
var
  i, j: Integer;
  smeHack: TsmeHack;
begin
  Result := False;
  smeHack := TsmeHack(sme);
  with TfrmExportDlg.Create(Application) do
    try
      strFPrompt := smeHack.GetLanguageString(7);
      strTName := smeHack.GetLanguageString(65);

      Caption := smeHack.GetLanguageString(10);
      TitleSaveDlg := smeHack.GetLanguageString(69);

      rgTableType.Caption := smeHack.GetLanguageString(28);
      for i := Ord(Low(TTableTypeExport)) to Ord(High(TTableTypeExport)) do
        if (TTableTypeExport(i) in Formats) then
        begin
          j := rgTableType.Items.Add(smeHack.GetLanguageString(29 + i));
          TRadioButton(rgTableType.Controls[j]).Tag := i;
        end;

//      rgTableType.ItemIndex := Ord(TabType);
      { Check to see if there are any formats listed }
      if rgTableType.Items.Count = 0 then exit;

      { Set the default to the first format element in the list }
      rgTableType.ItemIndex := 0;
      { Loop through the format list }
      for i := 0 to rgTableType.Items.Count - 1 do
      begin
        { If the TabType equals the Tag number }
        if TRadioButton(rgTableType.Controls[i]).Tag = Ord(TabType) then
        begin
          { Set the ItemIndex and exit }
          rgTableType.ItemIndex := i;
          break;
        end;
      end;

      lblCharacterSet.Caption := smeHack.GetLanguageString(50);
      cbCharacterSet.ItemIndex := 0;
      cbSelectedRecord.Caption := smeHack.GetLanguageString(51);
      cbAddTitle.Caption := smeHack.GetLanguageString(52);
      cbFixed.Caption := smeHack.GetLanguageString(65);
      lblFileName.Caption := smeHack.GetLanguageString(68);
      cbBlankIfZero.Caption := smeHack.GetLanguageString(54);

      rgSeparator.Caption := smeHack.GetLanguageString(59);
      for i := 60 to 64 do
        rgSeparator.Items.Add(smeHack.GetLanguageString(i));

      btnOk.Caption := smeHack.GetLanguageString(11);
      btnCancel.Caption := smeHack.GetLanguageString(12);

      if (FileName <> '') then
        edFileName.Text := FileName;
      strAccessTable := TableName;
      cbCharacterSet.ItemIndex := Ord(csCharacterSet);

      case chSeparator of
        #9: rgSeparator.ItemIndex := 0;
        ';': rgSeparator.ItemIndex := 1;
        ',': rgSeparator.ItemIndex := 2;
        ' ': rgSeparator.ItemIndex := 3;
      else
        rgSeparator.ItemIndex := 4;
        if (chSeparator <> '') then
          edSymbol.Text := chSeparator;
      end;

      cbAddTitle.Checked := boolAddTitle;
      cbFixed.Checked := boolFixed;

      rgTableTypeClick(Application);
      rgSeparatorClick(Application);

      cbSelectedRecord.Checked := (intSelectedRecord = 1);
//      cbSelectedRecord.Enabled := (intSelectedRecord < 2);
      cbBlankIfZero.Checked := boolBlankIfZero;


      Result := (ShowModal = mrOk);
      if Result then
      begin
        FileName := edFileName.Text;
        TableName := strAccessTable;
        TabType := TTableTypeExport(TRadioButton(rgTableType.Controls[rgTableType.ItemIndex]).Tag);

        csCharacterSet := TCharacterSet(cbCharacterSet.ItemIndex);
        case rgSeparator.ItemIndex of
          0: chSeparator := #9;
          1: chSeparator := ';';
          2: chSeparator := ',';
          3: chSeparator := ' ';
        else
          if (edSymbol.Text = '') then
            chSeparator := #0//' '
          else
            chSeparator := edSymbol.Text[1];
        end;

        boolAddTitle := cbAddTitle.Checked;
        boolFixed := cbFixed.Checked;
        boolBlankIfZero := cbBlankIfZero.Checked;

        if cbSelectedRecord.Enabled then
          if cbSelectedRecord.Checked then
            intSelectedRecord := 1
          else
            intSelectedRecord := 0;
      end;
    finally
      Free;
    end;
end;


{ TfrmExportDlg }
procedure TfrmExportDlg.rgTableTypeClick(Sender: TObject);
var
  intCurExportType: Integer;
begin
  intCurExportType := TRadioButton(rgTableType.Controls[rgTableType.ItemIndex]).Tag;

  edFileName.Enabled := (intCurExportType <> 14);
  btnOpenDlg.Enabled := edFileName.Enabled;

  rgSeparator.Enabled := (intCurExportType in [2, 14]);
  cbAddTitle.Enabled := (intCurExportType > 1);
  cbFixed.Enabled := rgSeparator.Enabled and
                     (intCurExportType = 2);
  edSymbol.Enabled := rgSeparator.Enabled and
                      (rgSeparator.ItemIndex = 4);

  cbCharacterSet.Enabled := (intCurExportType > 1);
//  cbSelectedRecord.Enabled := (intCurExportType > 1);
  SetDefaultExt;
end;

procedure TfrmExportDlg.rgSeparatorClick(Sender: TObject);
var
  intCurExportType: Integer;
begin
  intCurExportType := TRadioButton(rgTableType.Controls[rgTableType.ItemIndex]).Tag;

  cbFixed.Enabled := rgSeparator.Enabled and
                     (intCurExportType = 2);
  edSymbol.Enabled := rgSeparator.Enabled and
                     (rgSeparator.ItemIndex {intCurExportType} = 4);
end;

procedure TfrmExportDlg.SetDefaultExt;
var
  intCurExportType: Integer;
begin
  if (edFileName.Text <> '') then
  begin
    intCurExportType := TRadioButton(rgTableType.Controls[rgTableType.ItemIndex]).Tag;
    edFileName.Text := ChangeFileExt(edFileName.Text, TSMExportBaseComponent.GetDefExt(intCurExportType))
  end
end;

procedure TfrmExportDlg.edFileNameChange(Sender: TObject);
begin
  btnOk.Enabled := (edFileName.Text <> '');
end;

procedure TfrmExportDlg.btnOpenDlgClick(Sender: TObject);
var
  intCurExportType: Integer;
  strFileName, DefExt: string;
begin
  intCurExportType := TRadioButton(rgTableType.Controls[rgTableType.ItemIndex]).Tag;

  DefExt := TSMExportBaseComponent.GetDefExt(intCurExportType);
  strFileName := edFileName.Text;
  if SMEOpenSaveFileDialog(Self.Handle, DefExt, rgTableType.Items[rgTableType.ItemIndex] + '|*' + SaveDlg.DefaultExt, '', TitleSaveDlg, strFileName, False) then
    edFileName.Text := strFileName;
//    edFileName.Text := ChangeFileExt(edFileName.Text, DefExt)
end;

procedure TfrmExportDlg.btnOkClick(Sender: TObject);
var
  intCurExportType: Integer;
begin
  intCurExportType := TRadioButton(rgTableType.Controls[rgTableType.ItemIndex]).Tag;

  if (intCurExportType = 13) then // Access
  begin
    if InputQuery(strTName, strTableName, strAccessTable) then
      ModalResult := mrOk;
  end
  else
    if (not FileExists(edFileName.Text)) or
       (MessageDlg(Format(strFPrompt, [edFileName.Text]),
                   mtWarning, [mbYes, mbNo], 0) = mrYes) then
      ModalResult := mrOk;
end;

end.