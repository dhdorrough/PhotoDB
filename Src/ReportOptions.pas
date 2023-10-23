unit ReportOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, LocationUtils;

type
  TfrmReportOptions = class(TForm)
    Label1: TLabel;
    edtLogFileName: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    btnBrowse: TButton;
    SaveDialog1: TSaveDialog;
    cbIncludePhotoDate: TCheckBox;
    cbIncludeFileName: TCheckBox;
    cbIncludeFilePath: TCheckBox;
    cbIncludeKeyWords: TCheckBox;
    cbIncludeLocationInfo: TCheckBox;
    leFileNamePattern: TLabeledEdit;
    cbGenNames: TCheckBox;
    pnlFileNameBasedOnLatLon: TPanel;
    cbBasedOnLatitudeStoN: TRadioButton;
    cbBasedOnLatitudeNtoS: TRadioButton;
    cbBasedOnLongitudeWtoE: TRadioButton;
    cbBasedOnLongitudeEtoW: TRadioButton;
    cbIncludeCopyrightOwner: TCheckBox;
    procedure btnBrowseClick(Sender: TObject);
    procedure cbIncludePhotoDateClick(Sender: TObject);
    procedure cbIncludeFileNameClick(Sender: TObject);
    procedure cbIncludeFilePathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure leFileNamePatternChange(Sender: TObject);
    procedure cbGenNamesClick(Sender: TObject);
  private
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetIncludePhotoDate: boolean;
    procedure SetIncludePhotoDate(const Value: boolean);
    function GetIncludeFileName: boolean;
    procedure SetIncludeFileName(const Value: boolean);
    function GetIncludeFilePath: boolean;
    procedure SetIncludeFilePath(const Value: boolean);
    procedure Enable_Buttons;
    function GetIncludeKeyWords: boolean;
    procedure SetIncludeKeyWords(const Value: boolean);
    function GetIncludeLocationInfo: boolean;
    procedure SetIncludeLocationInfo(const Value: boolean);
    function GetGenerateFileName: boolean;
    procedure SetGenerateFileName(const Value: boolean);
    function GetFileNamePattern: string;
    procedure SetFileNamePattern(const Value: string);
    function LocationInfoNeeded: boolean;
    function LocationInfoIsComplete: boolean;
    function GetIncludeCopyrightOwner: boolean;
    procedure SetIncludeCopyrightOwner(const Value: boolean);
    { Private declarations }
  public
    { Public declarations }
    function GetDirection: TDirection;
    Property FileName: string
             read GetFileName
             write SetFileName;
    property IncludePhotoDate: boolean
             read GetIncludePhotoDate
             write SetIncludePhotoDate;
    property IncludeFileName: boolean
             read GetIncludeFileName
             write SetIncludeFileName;
    property IncludeFilePath: boolean
             read GetIncludeFilePath
             write SetIncludeFilePath;
    property IncludeKeyWords: boolean
             read GetIncludeKeyWords
             write SetIncludeKeyWords;
    property IncludeLocationInfo: boolean
             read GetIncludeLocationInfo
             write SetIncludeLocationInfo;
    property GenerateFileName: boolean
             read GetGenerateFileName
             write SetGenerateFileName;
    property FileNamePattern: string
             read GetFileNamePattern
             write SetFileNamePattern;
    property IncludeCopyrightOwner: boolean
             read GetIncludeCopyrightOwner
             write SetIncludeCopyrightOwner;
  end;

implementation

uses MyUtils;

{$R *.DFM}

const
  LOC_BASED_PATTERN = '%X';


procedure TfrmReportOptions.btnBrowseClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      DefaultExt := 'TXT';
      FileName   := edtLogFileName.Text;
      if Execute then
        edtLogFileName.Text := FileName;
    end;
end;

function TfrmReportOptions.GetFileName: string;
begin
  result := edtLogFileName.Text;
end;

procedure TfrmReportOptions.SetFileName(const Value: string);
begin
  edtLogFileName.Text  := Value;
end;

function TfrmReportOptions.GetIncludePhotoDate: boolean;
begin
  result := cbIncludePhotoDate.Checked;
end;

procedure TfrmReportOptions.SetIncludePhotoDate(const Value: boolean);
begin
  cbIncludePhotoDate.Checked := Value;
end;

function TfrmReportOptions.GetIncludeFileName: boolean;
begin
  result := cbIncludeFileName.Checked;
end;

procedure TfrmReportOptions.SetIncludeFileName(const Value: boolean);
begin
  cbIncludeFileName.Checked := Value;
end;

function TfrmReportOptions.GetIncludeFilePath: boolean;
begin
  result := cbIncludeFilePath.Checked;
end;

procedure TfrmReportOptions.SetIncludeFilePath(const Value: boolean);
begin
  cbIncludeFilePath.Checked := Value;
end;

procedure TfrmReportOptions.cbIncludePhotoDateClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmReportOptions.cbIncludeFileNameClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmReportOptions.cbIncludeFilePathClick(Sender: TObject);
begin
  Enable_Buttons;
end;

procedure TfrmReportOptions.Enable_Buttons;
var
  b1, b2: boolean;
begin
  b1 := cbIncludePhotoDate.Checked or
        cbIncludeFileName.Checked or
        cbIncludeFilePath.Checked;
  b2 := not Empty(edtLogFileName.Text) and LocationInfoIsComplete;
  btnOK.Enabled := b1 or b2;
end;


procedure TfrmReportOptions.FormCreate(Sender: TObject);
begin
  btnOk.Enabled := true;
end;

function TfrmReportOptions.GetIncludeKeyWords: boolean;
begin
  result := cbIncludeKeyWords.Checked;
end;

procedure TfrmReportOptions.SetIncludeKeyWords(const Value: boolean);
begin
  cbIncludeKeyWords.Checked := Value;
end;

function TfrmReportOptions.GetIncludeLocationInfo: boolean;
begin
  result := cbIncludeLocationInfo.Checked;
end;

procedure TfrmReportOptions.SetIncludeLocationInfo(const Value: boolean);
begin
  cbIncludeLocationInfo.Checked := Value;
end;

function TfrmReportOptions.GetGenerateFileName: boolean;
begin
  result := cbGenNames.Checked;
end;

procedure TfrmReportOptions.SetGenerateFileName(const Value: boolean);
begin
  cbGenNames.Checked := Value;
end;

function TfrmReportOptions.GetFileNamePattern: string;
begin
  result := leFileNamePattern.Text;
end;

procedure TfrmReportOptions.SetFileNamePattern(const Value: string);
begin
  leFileNamePattern.Text := Value;
end;

function TfrmReportOptions.GetDirection: TDirection;
begin
  if cbBasedOnLatitudeStoN.Checked then
    result := td_S_to_N else
  if cbBasedOnLatitudeNtoS.Checked then
    result := td_N_to_S else
  if cbBasedOnLongitudeEtoW.Checked then
    result := td_E_to_W else
  if cbBasedOnLongitudeWtoE.Checked then
    result := td_W_to_E
  else
    result := td_Unknown;
end;

function TfrmReportOptions.LocationInfoIsComplete: boolean;
begin
  if LocationInfoNeeded then
    result := GetDirection <> td_Unknown
  else
    result := true;
end;

function TfrmReportOptions.LocationInfoNeeded: boolean;
begin
  result := Pos(LOC_BASED_PATTERN, UpperCase(leFileNamePattern.text)) > 0;
end;

procedure TfrmReportOptions.leFileNamePatternChange(Sender: TObject);
begin
  pnlFileNameBasedOnLatLon.Visible := LocationInfoNeeded;
  Enable_Buttons;
end;

procedure TfrmReportOptions.cbGenNamesClick(Sender: TObject);
begin
  leFileNamePattern.Visible := cbGenNames.Checked;
end;

function TfrmReportOptions.GetIncludeCopyrightOwner: boolean;
begin
  result := cbIncludeCopyrightOwner.Checked;
end;

procedure TfrmReportOptions.SetIncludeCopyrightOwner(const Value: boolean);
begin
  cbIncludeCopyrightOwner.Checked := Value;
end;

end.
