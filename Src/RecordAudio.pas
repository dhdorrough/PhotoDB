unit RecordAudio;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs,MPlayer,MMSystem,StdCtrls, ComCtrls, Spin, ExtCtrls,
  Buttons, OvcBase, ovcfsc, JPeg;

const
  MAXLEVELS = 25;
  FATTR = faAnyFile{-faDirectory}-faHidden-faSysFile-faVolumeID;

type
  TMyDirPos = record
    FileIndex: integer;
    FolderName: string;
  end;

  TRecordingChangedEvent = procedure {name}(const FileName: string) of object;

  TfrmAudioRecorder = class(TForm)
    btnStop: TButton;
    MediaPlayer1: TMediaPlayer;
    pgBar: TProgressBar;
    edtFileNameWAV: TEdit;
    btnBrowse: TButton;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    GroupBox6: TGroupBox;
    Panel6: TPanel;
    Panel7: TPanel;
    rbMono: TRadioButton;
    rbStereo: TRadioButton;
    Panel12: TPanel;
    Panel17: TPanel;
    rb11: TRadioButton;
    rb22: TRadioButton;
    rb44: TRadioButton;
    Panel18: TPanel;
    rb8: TRadioButton;
    rb16: TRadioButton;
    rb8k: TRadioButton;
    btnClose: TButton;
    btnStart: TBitBtn;
    lblStatus: TLabel;
    lblSize: TLabel;
    btnDeleteWAV: TButton;
    OpenDialog1: TOpenDialog;
    lblDeviceID: TLabel;
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AppException(Sender: TObject; E: Exception);
    procedure SpinEdit1Change(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteWAVClick(Sender: TObject);
    procedure edtFileNameWAVMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure edtFileNameWAVMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MediaPlayer1Notify(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtFileNameWAVChange(Sender: TObject);
  private
    { Private declarations }
    fAlreadyDragging: boolean;
    FDeviceID: Word;
    fDragPoint: TPoint;
    fOnRecordingChanged: TRecordingChangedEvent;
    fStartTime: double;
    procedure CheckForWAVFile(lfn: string);
    function GetAudioFileName: string;
    procedure SetAudioFileName(const Value: string);
  public
    procedure OpenMedia;
    procedure RecordMedia;
    procedure StopMedia;
    procedure SaveMedia;
    procedure CloseMedia;
    procedure SetWaveParameters;
    property AudioFileName: string
             read GetAudioFileName
             write SetAudioFileName;
    property OnRecordingChanged: TRecordingChangedEvent
             read fOnRecordingChanged
             write fOnRecordingChanged;
  end;


implementation

uses MyUtils, DragDropFile;

{$R *.DFM}

var
  MyError, Flags: Longint;

  function FileSize64(FileName: string): Int64;
  var
    Find: THandle;
    FindData: TWin32FindData;
  begin
    Result:= 0;
    Find:= FindFirstFile(pChar(FileName), FindData);
    if Find<>INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Find);
      with LARGE_INTEGER(Result) do
      begin
        LowPart:= FindData.nFileSizeLow;
        HighPart:= FindData.nFileSizeHigh;
      end;
    end;
  end;

  procedure TfrmAudioRecorder.OpenMedia;
  var
    MyOpenParms: TMCI_Open_Parms;
  begin
    Flags := mci_Wait or mci_Open_Element or mci_Open_Type;
    with MyOpenParms do
      begin
        dwCallback       := Handle; // TForm1.Handle
        lpstrDeviceType  := PChar('WaveAudio');
        lpstrElementName := PChar('');
      end;
    MyError := mciSendCommand(0, mci_Open, Flags, Longint(@MyOpenParms));
    if MyError = 0 then
      FDeviceID := MyOpenParms.wDeviceID;
  end;

  procedure TfrmAudioRecorder.RecordMedia;
  var
    MyRecordParms: TMCI_Record_Parms;
  begin
    Flags := mci_Notify;
    with MyRecordParms do
      begin
        dwCallback := Handle;  // TForm1.Handle
        dwFrom     := 0;
        dwTo       := 10000; // should this be SpinEdit1.Value * 1000;
        pgBar.max  := SpinEdit1.value;
      end;
    MyError := mciSendCommand(FDeviceID, mci_Record, Flags, Longint(@MyRecordParms));
  end;

  procedure TfrmAudioRecorder.StopMedia;
  var
    MyGenParms: TMCI_Generic_Parms;
  begin
    if FDeviceID <> 0 then
      begin
        Flags := mci_Wait;
        MyGenParms.dwCallback := Handle;  // TForm1.Handle
        MyError := mciSendCommand(FDeviceID, mci_Stop, Flags, Longint(@MyGenParms));
      end;
  end;

  procedure TfrmAudioRecorder.SaveMedia;
  type    // not implemented by Delphi
    PMCI_Save_Parms = ^TMCI_Save_Parms;
    TMCI_Save_Parms = record
      dwCallback: DWord;
      lpstrFileName: PAnsiChar;  // name of file to save
    end;
  var
    MySaveParms: TMCI_Save_Parms;
  begin
    if FDeviceID <> 0 then
      begin
          // save the file...
        Flags := mci_Save_File or mci_Wait;
        with MySaveParms do
          begin
            dwCallback    := Handle;
            lpstrFileName := PChar(edtFileNameWAV.text);
            //lpstrFileName:=PChar('c:\message.wav');
          end;
        MyError := mciSendCommand(FDeviceID, mci_Save, Flags, Longint(@MySaveParms));
      end;
  end;

  procedure TfrmAudioRecorder.CloseMedia;
  var
    MyGenParms: TMCI_Generic_Parms;
  begin
    if FDeviceID <> 0 then
      begin
        Flags := 0;
        MyGenParms.dwCallback := Handle; // TForm1.Handle
        MyError := mciSendCommand(FDeviceID, mci_Close, Flags, Longint(@MyGenParms));
        if MyError = 0 then
          FDeviceID := 0;
      end;
  end;

  procedure TfrmAudioRecorder.btnStopClick(Sender: TObject);
  begin
    StopMedia;
    SaveMedia;
    CloseMedia;
    if Assigned(fOnRecordingChanged) then
      fOnRecordingChanged(edtFileNameWAV.text);

    btnStop.enabled  := false;
    btnStart.enabled := true;
    timer1.enabled:= false;
    pgBar.position := 0;
    with mediaplayer1 do
      begin
        filename := edtFileNameWAV.text;
        enabled  := true;
        open;
      end;
    edtFileNameWAV.Color := clYellow;
    btnDeleteWAV.Enabled := true;
    lblSize.Caption      := ScaledSize(FileSize32(edtFileNameWAV.text));
    if Assigned(fOnRecordingChanged) then
      fOnRecordingChanged(edtFileNameWAV.text);
  end;

  procedure TfrmAudioRecorder.CheckForWAVFile(lfn: string);
  begin { TfrmImageSound.CheckForWAVFile }
//  edtFileNameWAV.text := ChangeFileExt(lfn, '.wav');
    if FileExists(edtFileNameWAV.text) then
      begin
        edtFileNameWAV.Color := clYellow;
        btnDeleteWAV.Enabled := true;
        edtFileNameWAV.Hint  := DateTimeToStr(FileDateToDateTime(FileAge(edtFileNameWAV.text)));
        edtFileNameWAV.ShowHint := true;
        with mediaplayer1 do
          begin
            Enabled := true;
            filename := edtFileNameWAV.text;
            mediaplayer1.open;
//          OpenMedia;
          end;
      end
    else
      begin
        edtFileNameWAV.Color := clWindow;
        btnDeleteWAV.Enabled := false;
        edtFileNameWAV.ShowHint := false;
      end;

    edtFileNameWAV.Enabled := true;
    btnBrowse.Enabled      := true;
  end;  { TfrmImageSound.CheckForWAVFile }

  procedure TfrmAudioRecorder.FormCreate(Sender: TObject);
  begin
    Application.OnException := AppException;
    mediaplayer1.width := 307;

    lblStatus.Caption := '';
    lblSize.Caption   := '';
  end;

  procedure TfrmAudioRecorder.AppException(Sender: TObject; E: Exception);
  begin
    CloseMedia;
  end;

procedure TfrmAudioRecorder.SpinEdit1Change(Sender: TObject);
begin
  pgBar.max := SpinEdit1.value;
end;

procedure TfrmAudioRecorder.btnBrowseClick(Sender: TObject);
begin
  SaveDialog1.filename := edtFileNameWAV.text;
  if SaveDialog1.execute then
    edtFileNameWAV.text := SaveDialog1.filename;
end;

procedure TfrmAudioRecorder.Timer1Timer(Sender: TObject);
var
  temp: string;
begin
  with pgBar do
    begin
      if position < Max then
        begin
          DateTimeToString(temp, 'ss:zzz', Time-fStartTime);
          lblStatus.Caption := Format('%s', [temp]);
          Application.ProcessMessages;
        end
      else
        begin
          btnStopClick(self);
          position := 0;
          exit;
        end;
      position := position + 1;
    end;
end;

procedure TfrmAudioRecorder.SetWaveParameters;
const
  cMONO    = 1;
  cSTEREO  = 2;
  sRATE8   = 8000;  // Supported by PCM wave format (as long as I've known)
  sRATE11  = 11025;
  sRATE22  = 22050;
  sRATE44  = 44100;
  bBIT8    = 8;
  bBIT16   = 16;
var
  MyWaveParms   : TMCI_Wave_Set_Parms;
  SampPerSec    : DWORD;
  Channels      : WORD;
  BitsSample    : WORD;
begin
  SampPerSec := 0;
  if rbMono.checked then Channels := cMONO else Channels := cSTEREO;
  if rb8k.checked then SampPerSec := sRATE8;
  if rb11.checked then SampPerSec := sRATE11;
  if rb22.checked then SampPerSec := sRATE22;
  if rb44.checked then SampPerSec := sRATE44;
  if rb8.checked then BitsSample := bBIT8 else BitsSample := bBIT16;
  if FDeviceId <> 0 then  // Channel must be already open
    begin
      lblDeviceID.Caption := Format('Device ID = %d', [fDeviceID]);
      Flags := MCI_WAVE_SET_ANYINPUT or
               MCI_WAVE_SET_ANYOUTPUT or
     	       MCI_WAVE_SET_BITSPERSAMPLE or
               MCI_WAVE_SET_AVGBYTESPERSEC or
               MCI_WAVE_SET_BLOCKALIGN or
               MCI_WAVE_SET_SAMPLESPERSEC or
               MCI_WAVE_SET_CHANNELS or
               MCI_WAVE_SET_FORMATTAG;
      with MyWaveParms do
        begin
          wFormatTag 	  := WAVE_FORMAT_PCM;
          dwCallback	  := Handle;  // TForm1.Handle
          nChannels       := Channels;
          nSamplesPerSec  := SampPerSec;
          nAvgBytesPerSec := (BitsSample * Channels) div 8 * SampPerSec;
          wBitsPerSample  := BitsSample;
          nBlockAlign     := (BitsSample * Channels) div 8;
        end;
      MyError := mciSendCommand(fDeviceId, MCI_SET, Flags, longint(@MyWaveParms));
  end;
end;


procedure TfrmAudioRecorder.btnStartClick(Sender: TObject);
begin
  OpenMedia;
  fStartTime := Time;
  SetWaveParameters;
  RecordMedia;

  btnStop.enabled  := true;
  btnStart.enabled := false;
  timer1.enabled   := true;
end;

procedure TfrmAudioRecorder.btnCloseClick(Sender: TObject);
begin
  mediaplayer1.close;
  mediaplayer1.filename := '';
  mediaplayer1.enabled  := false;
end;

procedure Alert(msg: string);
begin
  Application.MessageBox(pchar(msg), 'Alert', idOK);
end;

procedure TfrmAudioRecorder.btnDeleteWAVClick(Sender: TObject);
begin
  btnCloseClick(nil);
  if FileExists(edtFileNameWAV.text) then
    if not DeleteFile(edtFileNameWAV.text) then
      Alert(Format('File %s could not be deleted',
                   [edtFileNameWAV.text]))
    else
      begin
        CheckForWAVFile(edtFileNameWAV.text);
        if Assigned(fOnRecordingChanged) then
          fOnRecordingChanged(edtFileNameWAV.text);
      end;
end;

function TfrmAudioRecorder.GetAudioFileName: string;
begin
  result := edtFileNameWAV.Text;
end;

procedure TfrmAudioRecorder.SetAudioFileName(const Value: string);
begin
  btnCloseClick(nil);
  edtFileNameWAV.Text := Value;
  CheckForWAVFile(edtFileNameWAV.Text);
end;

procedure TfrmAudioRecorder.edtFileNameWAVMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  DropFileSource: TDropFileSource;
begin
  if fAlreadyDragging then exit;
  Shift := (Shift * [ssLeft,ssRight]);

  //Make sure a mouse button is pressed before starting the drag ...
  //and the mouse has moved at least 10 pixels
  //(DragPoint is set in the ListView1MouseDown event)
  if  (Shift = []) or
      ((abs(fDragPoint.X - X) < 10) and (abs(fDragPoint.Y - Y) < 10)) then
    exit;

  DropFileSource := TDropFileSource.Create(self);
  try
    //Fill DropSource1.Files with selected files in ListView1
    DropFileSource.Files.Add(edtFileNameWAV.Text);
    fAlreadyDragging := true;

    //Start the dragdrop...
    //Note: DropFileSource1.DragTypes = [dtCopy]
    DropFileSource.execute;
    //Note: Execute does not return until the drop has finished -
    //either sucessfully with a copy, move of link operation or cancelled.

    fAlreadyDragging := false;
  finally
    DropFileSource.Free;
  end;
end;

procedure TfrmAudioRecorder.edtFileNameWAVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fDragPoint := Point(X,Y);
end;

procedure TfrmAudioRecorder.MediaPlayer1Notify(Sender: TObject);
begin
  with Sender as TMediaPlayer do
   begin
     case Mode of
       mpStopped: {DO something HERE};
     end;
     //must set to true to
     //enable next-time notification
     Notify := True;
   end;
end;

procedure TfrmAudioRecorder.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MediaPlayer1.Close;
  Action := caFree;
end;

procedure TfrmAudioRecorder.edtFileNameWAVChange(Sender: TObject);
begin
  CheckForWAVFile(edtFileNameWAV.Text);
end;

end.
