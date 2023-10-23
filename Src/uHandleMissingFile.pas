unit uHandleMissingFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, YesNoDontAskAgain;

const
  mrUpdateDBRec = 101;
  mrDeleteDBRec = 102;
  mrIgnore      = 103;
  mrSearchForSubString = 104;

type
  TfrmHandleMissingFile = class(TForm)
    lblMsg: TLabel;
    rbSearch: TRadioButton;
    rbDelete: TRadioButton;
    rbIgnore: TRadioButton;
    btnOk: TButton;
    StringGrid1: TStringGrid;
    lblStatus: TLabel;
    btnUpdateDB: TButton;
    lblStatus2: TLabel;
    Label1: TLabel;
    rbSearchForSubString: TRadioButton;
    edtSubString: TEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnUpdateDBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbSearchForSubStringClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    frmYesNoDontAskAgain: TfrmYesNoDontAskAgain;
    fLfn: string;
    fRootPath: string;
    fNrFound: integer;
    procedure SearchForOccurrences(SearchFolder: string;
      SearchForSubString: boolean; SubStringToSearchFor: string);
    function YesNoDontAsk(Msg: string; Args: array of const): integer;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent; const RootPath, aLfn, KeyWords: string); reintroduce;
    Destructor Destroy; override;
  end;

function HandleMissingFile(aOwner: TComponent; const RootPath, KeyWords: string; var aLfn: string): integer;

implementation

{$R *.DFM}

uses
  StStrL, MyUtils, PDB_Decl;

var
  gCorrectedFileName: string;
  gNoToAll: boolean;

function HandleMissingFile(aOwner: TComponent; const RootPath, KeyWords: string; var aLfn: string): integer;
begin
  with TfrmHandleMissingFile.Create(nil, RootPath, aLfn, KeyWords) do
    begin
      result := ShowModal;
      aLfn   := gCorrectedFileName;
      Free;
    end;
end;


{ TfrmHandleMissingFile }

constructor TfrmHandleMissingFile.Create(aOwner: TComponent;
  const RootPath, aLfn, KeyWords: string);
begin
  inherited Create(aOwner);
  fLfn             := aLfn;
  fRootPath        := RootPath;
  lblMsg.Caption   := Format('File "%s" [KeyWords: %s] does not exist', [fLfn, KeyWords]);
  rbSearch.Caption := Format('Search for file "%s"', [ExtractFileName(fLfn)]);
  edtSubString.Text := ExtractFileName(fLfn);
  rbDelete.Caption := Format('Delete record "%s" from DB', [fLfn]);
end;

procedure TfrmHandleMissingFile.SearchForOccurrences(SearchFolder: string; SearchForSubString: boolean; SubStringToSearchFor: string);
  var
    FileNameAndExt: string;
    SearchRec: TSearchRec; dosError: integer;
    PathFilter, Path: string;
    RowNr: integer;
    OK: boolean;
begin
  lblStatus.Caption := Format('%d found', [fNrFound]);
  lblStatus2.Caption := 'Searching: ' + ExtractFileName(SearchFolder);
  Application.ProcessMessages;
  // process files in this folder first
  Path           := AddBackSlashL(SearchFolder);
  FileNameAndExt := ExtractFileName(fLfn);
//if SearchForSubString then
//  PathFilter := Path + '*.*'
//else
//  PathFilter := Path + FileNameAndExt;

  PathFilter := Path + '*.*';  // 11/16/2018 - changed from above because it was not finding some files (such as:)
                               // \\xps-8930\Users\dhdor\Music\Ahmet M. Özgül\Best Of Anatolia_Melodies (Volume 1)\01 Kütahyanin Pinarlari.mp3

  dosError    := FindFirst(PathFilter, faAnyFile, SearchRec);
  try
    while dosError = 0 do
      begin
        if SearchRec.Attr and (faDirectory+faVolumeID+faSysFile+faHidden) = 0 then
          begin
            if SearchForSubString then
              OK := Assigned(MyStrPos(pchar(SearchRec.Name), pchar(SubStringToSearchFor), Length(SearchRec.Name)+1, true))
            else
              OK := AnsiCompareText(SearchRec.Name, FileNameAndExt) = 0;

            if OK then
              begin
                if fNrFound > 0 then
                  begin
                    RowNr := fNrFound;
                    StringGrid1.RowCount := StringGrid1.RowCount + 1;
                  end
                else
                  RowNr := 0;
                inc(fNrFound);
                lblStatus.Caption := Format('%d found', [fNrFound]);
                StringGrid1.Cells[0, RowNr] := Path + SearchRec.Name;
                StringGrid1.Cells[1, RowNr] := DateToStr(FileDateToDateTime(SearchRec.Time));
                Application.ProcessMessages;
              end;
          end;
        dosError := FindNext(SearchRec);
      end;

    // then process child folders
    PathFilter := Path + '*.*';
    dosError   := FindFirst(PathFilter, faAnyFile, SearchRec);
    while dosError = 0 do
      begin
        if ((SearchRec.Attr and faDirectory) > 0)
           and (SearchRec.Name <> '.')
           and (SearchRec.Name <> '..')
           and (AnsiCompareText(Copy(SearchRec.Name, 1, 2), 'TN') <> 0) then
          SearchForOccurrences(Path+SearchRec.Name, SearchForSubString, SubStringToSearchFor);
        dosError := FindNext(SearchRec);
      end;
  finally
    FindClose(SearchRec);
  end;
end;

function TfrmHandleMissingFile.YesNoDontAsk(Msg: string; Args: array of const): integer;
begin
  if not Assigned(frmYesNoDontAskAgain) then
    frmYesNoDontAskAgain := TfrmYesNoDontAskAgain.Create(self, '', '');
  frmYesNoDontAskAgain.lblPrompt.Caption := Format(Msg, Args);
  result := frmYesNoDontAskAgain.ShowModal;
end;


procedure TfrmHandleMissingFile.btnOkClick(Sender: TObject);
var
  mr: integer;
begin { TfrmHandleMissingFile.btnOkClick }
  if rbSearch.Checked then
    begin
      fNrFound := 0; mr := mrNone;
      if not Empty(gCorrectedFileName) then
        SearchForOccurrences(ExtractFilePath(gCorrectedFileName), false, ''); // 1st try where last file was found

      if fNrFound = 0 then                          // Nothing found in quick search
        SearchForOccurrences(fRootPath, false, '')  // so try complete search.
      else                                          // Already found one. Continue looking for more?
        if not gNoToAll then
          mr := YesNoDontAsk('%d occurrences found. Continue searching? ', [fNrFound])
        else
          mr := mrNo;

      case mr of
        mrYes:
          SearchForOccurrences(fRootPath, false, '');
        mrNo:
          { search is complete };
        mrCancel:
          begin
            lblStatus.Caption := 'Search aborted';
            Exit;
          end;
        mrNoToAll:
          gNoToAll := true;
      end;

      lblStatus2.Caption := 'Search complete';
      if fNrFound > 0 then
        btnUpdateDB.Enabled := true
      else
        rbIgnore.Checked    := true;
    end else
  if rbSearchForSubString.Checked then
    begin
      fNrFound := 0;
      SearchForOccurrences(fRootPath, true, edtSubString.Text);
      lblStatus2.Caption := 'Search complete';
      if fNrFound > 0 then
        btnUpdateDB.Enabled := true
      else
        rbIgnore.Checked    := true;
    end else
  if rbDelete.Checked then
    ModalResult := mrDeleteDBRec else
  if rbIgnore.Checked then
    ModalResult := mrIgnore
  else
    ModalResult := mrOk;
    { Ignore It }
end;  { TfrmHandleMissingFile.btnOkClick }


procedure TfrmHandleMissingFile.btnUpdateDBClick(Sender: TObject);
begin
  with StringGrid1 do
    gCorrectedFileName := Cells[0, Row];
  ModalResult := mrUpdateDBRec;
end;

procedure TfrmHandleMissingFile.FormCreate(Sender: TObject);
begin
  StringGrid1.ColWidths[0] := 800;
  StringGrid1.ColWidths[1] := 100;
end;

procedure TfrmHandleMissingFile.rbSearchForSubStringClick(Sender: TObject);
begin
  edtSubString.Enabled := rbSearchForSubString.Checked;
end;

destructor TfrmHandleMissingFile.Destroy;
begin

  inherited;
end;

procedure TfrmHandleMissingFile.FormShow(Sender: TObject);
begin
  btnUpdateDB.Enabled := false;
end;

initialization
  gCorrectedFileName := '';
  gNoToAll           := false;
end.
