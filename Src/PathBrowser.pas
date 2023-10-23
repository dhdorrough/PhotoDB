unit PathBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BrowserUnit, Menus, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids,
  PDBTables;

type
  TfrmPathBrowser = class(TfrmDataSetBrowser)
    btnNewPath: TButton;
    procedure btnNewPathClick(Sender: TObject);
  private
    { Private declarations }
    fFilePathsTable: TFilePathsTable;
  public
    { Public declarations }
    Constructor Create(aOwner: TComponent; aFilePathsTable: TFilePathsTable); reintroduce;
  end;

var
  frmPathBrowser: TfrmPathBrowser;

implementation

{$R *.dfm}

uses
  PDB_Decl, MyUtils, PDBUtils;

procedure TfrmPathBrowser.btnNewPathClick(Sender: TObject);
var
  aPath: string;
//  FileNameInfo: TFileNameInfo;
  KeyWords: string;
begin
  inherited;
  aPath := gRootPath;

  if BrowseForFolder('Select folder to add', aPath) then
    fFilePathsTable.FindFolderNo( aPath, KeyWords, true);
end;

constructor TfrmPathBrowser.Create(aOwner: TComponent;
  aFilePathsTable: TFilePathsTable);
begin
  inherited Create(aOwner, aFilePathsTable, 'File Paths Table');
  fFilePathsTable := aFilePathsTable;
end;

end.
