unit DateTimeDiff;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmCalcTimeDiff = class(TForm)
    leDate1: TLabeledEdit;
    leDate2: TLabeledEdit;
    edtDiffInMinutes: TLabeledEdit;
    btnCalculate: TButton;
    lblDays: TLabel;
    lblHours: TLabel;
    procedure btnCalculateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCalcTimeDiff: TfrmCalcTimeDiff;

implementation

{$R *.dfm}

uses
  DateUtils;

procedure TfrmCalcTimeDiff.btnCalculateClick(Sender: TObject);
var
  Date1, Date2: TDateTime;
  Days, Hours: double;
  Minutes: int64;
begin
  Date1 := StrToDateTime(leDate1.Text);
  Date2 := StrToDateTime(leDate2.Text);
  Minutes := MinutesBetween(Date1, Date2);
  Days    := DaysBetween(Date1, Date2);
  lblDays.Caption          := Format('%10.2f days', [Days]);
  Hours   := HoursBetween(Date1, Date2);
  lblHours.Caption      := Format('%10.2f hours', [Hours]);
  edtDiffInMinutes.Text := Format('%d', [Minutes]);
end;

end.
