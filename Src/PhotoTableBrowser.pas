unit PhotoTableBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BrowserUnit, Menus, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids;

type
  TfrmPhotoTableBrowser = class(TfrmDataSetBrowser)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmPhotoTableBrowser.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
//  inherited;  // skip the inherited stuff which will mis-position the dataset
  CloseMemos;
end;

end.
