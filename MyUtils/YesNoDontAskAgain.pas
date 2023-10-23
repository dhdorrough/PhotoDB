unit YesNoDontAskAgain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverWrite_Decl;

type
  TfrmYesNoDontAskAgain = class(TForm)
    btnYes: TButton;
    btnNo: TButton;
    btnCancel: TButton;
    btnNoAndDontAskAgain: TButton;
    lblPrompt: TLabel;
    btnYesAndDontAskAgain: TButton;
  private
    { Private declarations }
    fObjectType: string;
  public
    { Public declarations }
    NoAndDontAskAgain: boolean;
    YesAndDontAskAgain: boolean;
    function OkToOverWrite(const What: string; OverWriteOptions: TOverWriteOptions): boolean;
    constructor Create(aOwner: TComponent; const ObjectType: string; const ParentType: string = ''); reintroduce;
  end;

(*
var
  frmYesNoDontAskAgain: TfrmYesNoDontAskAgain;
*)

implementation

{$R *.dfm}

constructor TfrmYesNoDontAskAgain.Create(aOwner: TComponent; const ObjectType: string; const ParentType: string = '');
begin
  inherited Create(aOwner);
  
  fObjectType := ObjectType;
  btnNoAndDontAskAgain.Caption  := Format('No to all %s in %s', [ObjectType, ParentType]);
  btnYesAndDontAskAgain.Caption := Format('Yes to all %s in %s', [ObjectType, ParentType]);
end;

function TfrmYesNoDontAskAgain.OkToOverWrite(const What: string; OverWriteOptions: TOverWriteOptions): boolean;
  var
    mr: integer;
  begin
    result := false;
    case OverWriteOptions of
      ooOkToOverWrite:  result := true;
      ooDoNotOverWrite: result := false;
      ooAskToOverWrite:
        begin
          if YesAndDontAskAgain then
            result := true else
          if NoAndDontAskAgain then
            result := false
          else
            begin
              lblPrompt.Caption := Format('OK to update %s?', [What]);
              mr := ShowModal;
              case mr of
                mrYes: result := true;
                mrNo:  result := false;
                mrYesToAll:
                  begin
                    result := true;
                    YesAndDontAskAgain := true;
                  end;
                mrNoToAll:
                  begin
                    result := false;
                    NoAndDontAskAgain := true;
                  end;
                mrCancel:
                  SysUtils.Abort; //  Exception.Create('Operator abort');
              end;
            end;
        end;
    end;
  end;
end.
