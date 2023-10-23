unit EXIFInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dEXIF, Grids, StdCtrls, Menus
{$IfDef dExif2}
  , dMetadata
{$EndIf}
  ;

type
  TfrmExifInfo = class(TForm)
    StringGrid1: TStringGrid;
    btnClose: TButton;
    MainMenu1: TMainMenu;
    Edit1: TMenuItem;
    CopyLocationInfo1: TMenuItem;
    lblFileName: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure CopyLocationInfo1Click(Sender: TObject);
  private
    fImgData: TimgData;
    fCurrentFileName: string;
    fLatitude: double;
    fLongitude: double;
    procedure AddRow(const ValueName, Value: string);
    procedure SetCurrentFileName(const Value: string);
    procedure ShowExifInfo(Const FileName: string);
    procedure AddRowN(ValueName: string; N: integer; AlwaysShow: boolean = FALSE);
    procedure AddRowF(ValueName: string; F: Double; AlwaysShow: boolean = FALSE);
    procedure ShowMediaInfo(FileName: string; MediaFrames, MediaLength,
      MediaWidth, MediaHeight: Integer; MediaLengths: string;
      RecordedDate: TDateTime);
    procedure FileChanged(Value: string);
    { Private declarations }
  public
    { Public declarations }
    property CurrentFileName: string
             read fCurrentFileName
             write SetCurrentFileName;
    destructor Destroy; override;
    Constructor Create(aOwner: TComponent); override;
  end;

var
  FrmExifInfo: TfrmExifInfo;

implementation

uses LocationUtils, Clipbrd, PDB_Decl, VideoStuff2;

{$R *.dfm}

const
  COL1_WIDTH = 150;

{ TfrmExifInfo }

procedure TfrmExifInfo.AddRow(const ValueName, Value: string);
var
  CurrentRow: integer;
begin
  if Value <> '' then
    begin
      CurrentRow := StringGrid1.RowCount;
      StringGrid1.RowCount := CurrentRow + 1;
      StringGrid1.Cells[0, CurrentRow] := ValueName;
      StringGrid1.Cells[1, CurrentRow] := Value;
    end;
end;

procedure TfrmExifInfo.AddRowN(ValueName: string; N: integer; AlwaysShow: boolean = FALSE);
begin
  if (N <> 0) or AlwaysShow then
    AddRow(ValueName, IntToStr(N));
end;

procedure TfrmExifInfo.AddRowF(ValueName: string; F: Double; AlwaysShow: boolean = FALSE);
begin
  if (F <> 0.0) or AlwaysShow then
    AddRow(ValueName, Format('%10.7f', [F]));
end;

procedure TfrmExifInfo.ShowMediaInfo(FileName: string;
                                     MediaFrames, MediaLength, MediaWidth, MediaHeight: longint;
                                     MediaLengths: string;
                                     RecordedDate: TDateTime);
begin
  Caption := Format('Media Info for %s', [ExtractFileName(FileName)]);
  lblFileName.Caption := FileName;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0, 0] := 'Name';
  StringGrid1.Cells[1, 0] := 'Value';
  AddRow('Length', IntToStr(MediaLength));
  AddRow('Frames', IntToStr(MediaFrames));
  AddRow('Width',  IntToStr(MediaWidth));
  AddRow('Height', IntToStr(MediaHeight));
  AddRow('Lengths', MediaLengths);
  AddRow('Recorded Date', DateTimeTostr(RecordedDate));
end;


procedure TfrmExifInfo.ShowExifInfo(Const FileName: string);
begin
  Caption := Format('EXIF Info for %s', [ExtractFileName(FileName)]);
  lblFileName.Caption := FileName;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0, 0] := 'Name';
  StringGrid1.Cells[1, 0] := 'Value';
  with fImgData.ExifObj do
    begin
      AddRow('EXIF Version',             exifVersion);
      AddRow('Make',                     CameraMake);
      AddRow('Model',                    CameraModel);
{$IfNDef dExif2}   // Use the old version of dExif
      AddRow('Software',                 Data['Software'].Data);
      AddRow('DateTime',                 DateTimeToStr(GetImgDateTime));
      AddRowN('Height',                  Height);
      AddRowN('Width',                   Width);
      if HasThumbnail then
        AddRow('HasThumbnail',           'TRUE');
      fLatitude  := GetDecimalDegrees(Data['GPSLatitudeRef'].Data, Data['GPSLatitude'].Data);
      AddRowF('Latitude',                fLatitude);
      fLongitude := GetDecimalDegrees(Data['GPSLongitudeRef'].Data, Data['GPSLongitude'].Data);
      AddRowF('Longitude',               fLongitude);
      AddRow('FocalLength',              Data['FocalLength'].Data);
      AddRow('FocalLengthIn35mmFilm',    Data['FocalLengthIn35mmFilm'].Data);
      AddRow('ExposureTime',             Data['ExposureTime'].Data);
      AddRow('FNumber',                  Data['FNumber'].Data);
      AddRow('ISOSpeedRatings',          Data['ISOSpeedRatings'].Data);
      AddRow('ShutterSpeedValue',        Data['ShutterSpeedValue'].Data);
      AddRow('AperatureValue',           Data['AperatureValue'].Data);
      AddRow('LightSource',              Data['LightSource'].Data);
      AddRow('MeteringMode',             Data['MeteringMode'].Data);
      AddRow('Flash',                    Data['Flash'].Data);
      AddRow('FileSource',               Data['FileSource'].Data);
      AddRow('SceneType',                Data['SceneType'].Data);
      AddRow('SceneCaptureType',         Data['SceneCaptureType'].Data);
      AddRow('ImageDescription',         Data['ImageDescription'].Data);
      AddRow('ExposureMode',             Data['ExposureMode'].Data);
      AddRow('XResolutionMode',          Data['XResolutionMode'].Data);
      AddRow('YResolutionMode',          Data['YResolutionMode'].Data);
      AddRow('ResolutionUnit',           Data['ResolutionUnit'].Data);
//    AddRow('DateTimeOriginal',         Data['DateTimeOriginal'].Data);
//    AddRow('DateTimeDigitized',        Data['DateTimeDigitized'].Data);
      AddRow('CompressedBitsPerPixel',   Data['CompressedBitsPerPixel'].Data);
      AddRow('Orientation',              Data['Orientation'].Data);
      AddRow('Altitude',                 Data['GPSAltitude'].Data);
      AddRow('AltitudeRef',              Data['GPSAltitudeRef'].Data);
      AddRow('GPSImgDirection',          Data['GPSImgDirection'].Data);
      AddRow('GPSImgDirectionRef',       Data['GPSImgDirectionRef'].Data);
      AddRow('GPSTimeStamp',             Data['GPSTimeStamp'].Data);
      AddRow('SubjectLocation',          Data['SubjectLocation'].Data);
      AddRow('SubjectDistanceRange',     Data['SubjectDistanceRange'].Data);
      AddRow('MakerNote',                Data['MakerNote'].Data);
{$else}
      AddRow('Software',                 TagByName['Software'].Data);
      AddRow('DateTime',                 DateTimeToStr(GetImgDateTime));
      AddRowN('Height',                  Height);
      AddRowN('Width',                   Width);
      if HasThumbnail then
        AddRow('HasThumbnail',           'TRUE');
      fLatitude  := GetDecimalDegrees(TagByName['GPSLatitudeRef'].Data, TagByName['GPSLatitude'].Data);
      AddRowF('Latitude',                fLatitude);
      fLongitude := GetDecimalDegrees(TagByName['GPSLongitudeRef'].Data, TagByName['GPSLongitude'].Data);
      AddRowF('Longitude',               fLongitude);
      AddRow('FocalLength',              TagByName['FocalLength'].Data);
      AddRow('FocalLengthIn35mmFilm',    TagByName['FocalLengthIn35mmFilm'].Data);
      AddRow('ExposureTime',             TagByName['ExposureTime'].Data);
      AddRow('FNumber',                  TagByName['FNumber'].Data);
      AddRow('ISOSpeedRatings',          TagByName['ISOSpeedRatings'].Data);
      AddRow('ShutterSpeedValue',        TagByName['ShutterSpeedValue'].Data);
      AddRow('AperatureValue',           TagByName['AperatureValue'].Data);
      AddRow('LightSource',              TagByName['LightSource'].Data);
      AddRow('MeteringMode',             TagByName['MeteringMode'].Data);
      AddRow('Flash',                    TagByName['Flash'].Data);
      AddRow('FileSource',               TagByName['FileSource'].Data);
      AddRow('SceneType',                TagByName['SceneType'].Data);
      AddRow('SceneCaptureType',         TagByName['SceneCaptureType'].Data);
      AddRow('ImageDescription',         TagByName['ImageDescription'].Data);
      AddRow('ExposureMode',             TagByName['ExposureMode'].Data);
      AddRow('XResolutionMode',          TagByName['XResolutionMode'].Data);
      AddRow('YResolutionMode',          TagByName['YResolutionMode'].Data);
      AddRow('ResolutionUnit',           TagByName['ResolutionUnit'].Data);
//    AddRow('DateTimeOriginal',         TagByName['DateTimeOriginal'].Data);
//    AddRow('DateTimeDigitized',        TagByName['DateTimeDigitized'].Data);
      AddRow('CompressedBitsPerPixel',   TagByName['CompressedBitsPerPixel'].Data);
      AddRow('Orientation',              TagByName['Orientation'].Data);
      AddRow('Altitude',                 TagByName['GPSAltitude'].Data);
      AddRow('AltitudeRef',              TagByName['GPSAltitudeRef'].Data);
      AddRow('GPSImgDirection',          TagByName['GPSImgDirection'].Data);
      AddRow('GPSImgDirectionRef',       TagByName['GPSImgDirectionRef'].Data);
      AddRow('GPSTimeStamp',             TagByName['GPSTimeStamp'].Data);
      AddRow('SubjectLocation',          TagByName['SubjectLocation'].Data);
      AddRow('SubjectDistanceRange',     TagByName['SubjectDistanceRange'].Data);
      AddRow('MakerNote',                TagByName['MakerNote'].Data);
{$EndIf dExif2}
    end;
  Application.ProcessMessages;
end;

procedure TfrmExifInfo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  FrmExifInfo := nil;
end;

procedure TfrmExifInfo.FileChanged(Value: string);
var
  mt: TMediaType;
  mc: TMediaClass;
  Ext: string;
  MediaFrames, MediaLength, MediaWidth, MediaHeight: longint;
  MediaLengths: string;
  RecordedDate: TDateTime;
begin
  Ext := ExtractFileExt(Value);
  mt  := MediaTypeFromExtension(Ext);
  mc  := MediaInfoArray[mt].MediaClass;
  if mc = mc_Photos then
    begin
      fImgData.Free;
      fImgData := TImgData.Create;

      StringGrid1.RowCount := 1;  // In case there is no EXIF info
      if fImgData.ProcessFile(Value) then
        if fImgData.HasEXIF then
          ShowExifInfo(Value);

    end else
  if mc = mc_Video then
    begin
      if GetMediaProperties(Value, MediaFrames, MediaLength, MediaWidth, MediaHeight, MediaLengths, RecordedDate) then
        ShowMediaInfo(Value, MediaFrames, MediaLength, MediaWidth, MediaHeight, MediaLengths, RecordedDate);
    end;
end;


procedure TfrmExifInfo.SetCurrentFileName(const Value: string);
begin
//if not SameText(fCurrentFileName, Value) then
//  begin
      FileChanged(Value);
      Caption := Format('EXIF info- %s', [Value]);
      fCurrentFileName := Value;
//  end;
end;

destructor TfrmExifInfo.Destroy;
begin
  FreeAndNil(fImgData);
  inherited;
end;

procedure TfrmExifInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TfrmExifInfo.Create(aOwner: TComponent);
begin
  inherited;
  StringGrid1.ColWidths[0] := COL1_WIDTH;
  StringGrid1.ColWidths[1] := StringGrid1.Width - COL1_WIDTH;
  lblFileName.Caption      := '';
end;

procedure TfrmExifInfo.CopyLocationInfo1Click(Sender: TObject);
begin
  Clipboard.AsText := Format('%11.7f %11.7f', [fLatitude, fLongitude]);
end;

initialization
finalization
//  FreeAndNil(FrmExifInfo);

end.
