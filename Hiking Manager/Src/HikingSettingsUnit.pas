unit HikingSettingsUnit;

interface

uses
  SettingsFiles, Classes;

const
  HIKINGMANAGER_INI     = 'HikingManager.ini';
{$IfDef DHD}
  DEF_GPX_EDITOR        = '\\XPS-8930\All Users\Shared Programs\GPX_Editor.exe';
  DEF_HIKINGMDBPATHNAME = 'C:\Users\Dan\Dropbox\Hiking.mdb';
{$Else}
  DEF_GPX_EDITOR        = '';
  DEF_HIKINGMDBPATHNAME = '';
{$EndIf}
type
  THikingSettings = class(TSettingsFile)
  private
    fGPX_Logs_Folder_Name: string;
    fHikingMdbPathName: string;
    fPhotoSettingsIniFileName: string;
    fDefaultGPXFilter: string;
    fGPX_EditorPath: string;
    fMapFolder: string;
    fSavedTrackDataLfn: string;
    fPhotoDBDatabaseFileName: string;
    function GetHikingMdbPathName: string;
    procedure SetDefaultGPXFilter(const Value: string);
    function GetMapFolder: string;
    procedure SetMapFolder(const Value: string);
  public
    class function HikingSettingsFileName: string;
    procedure LoadSettings; override;
    procedure SaveSettings(const SettingsFileName: string); override;
    constructor Create(aOwner: TComponent); override;
  published
    property GPX_Logs_Folder_Name: string
             read fGPX_Logs_Folder_Name
             write fGPX_Logs_Folder_Name;
    property HikingMdbPathName: string
             read GetHikingMdbPathName
             write fHikingMdbPathName;
    property PhotoDBDatabaseFileName: string
             read fPhotoDBDatabaseFileName
             write fPhotoDBDatabaseFileName;
    property PhotoSettingsIniFileName: string
             read fPhotoSettingsIniFileName
             write fPhotoSettingsIniFileName;
    property DefaultGPXFilter: string
             read fDefaultGPXFilter
             write SetDefaultGPXFilter;
    property GPX_EditorPath: string
             read fGPX_EditorPath
             write fGPX_EditorPath;
    property MapFolder: string
             read GetMapFolder
             write SetMapFolder;
    property SavedTrackDataLfn: string
             read fSavedTrackDataLfn
             write fSavedTrackDataLfn;
  end;

var
  gHikingSettings            : THikingSettings;
  gHikingSettingsFileName    : string;

function HikingSettings: THikingSettings;

implementation

uses MyUtils, PhotoDBCommonSettings, SysUtils, PDB_Decl, Hiking_Decl;

function HikingSettings: THikingSettings;
begin { HikingSettings }
  if not Assigned(gHikingSettings) then
    gHikingSettings := THikingSettings.Create(nil);
  result := gHikingSettings;
end;  { HikingSettings }

{ THikingSettings }

constructor THikingSettings.Create(aOwner: TComponent);
begin
  inherited;

end;

function THikingSettings.GetHikingMdbPathName: string;
begin
  Result := fHikingMdbPathName;
  if Empty(result) then
    Result := DEF_HIKINGMDBPATHNAME;
end;

function THikingSettings.GetMapFolder: string;
begin
  if not Empty(fMapFolder) then
    Result := fMapFolder
  else
    result := CommonPhotoSettings.PhotoDBFolder + '\Maps';
end;

class function THikingSettings.HikingSettingsFileName: string;
var
  Ext, FileName: string;
begin
  FileName := ParamStr(1);
  if ParamStr(1) <> '' then
    begin
      Ext := ExtractFileExt(FileName);
      if SameText(Ext, '.ini') then
        if FileExists(FileName) then
          gHikingSettingsFileName := FileName;
    end;

  if Empty(gHikingSettingsFileName) or (not FileExists(gHikingSettingsFileName)) then
    gHikingSettingsFileName := ExtractFilePath(ParamStr(0)) + HIKINGMANAGER_INI; // default to executable's folder

  result := gHikingSettingsFileName;
end;

procedure THikingSettings.LoadSettings;
begin
  if FileExists(HikingSettingsFileName) then
    LoadFromFile(HikingSettingsFileName);
  if Empty(MapFolder) and (not Empty(CommonPhotoSettings.PhotoDBFolder)) then
    MapFolder              := RemoveTrailingBackSlash(CommonPhotoSettings.PhotoDBFolder) + '\Maps';
end;

procedure THikingSettings.SaveSettings(const SettingsFileName: string);
begin
  SaveToFile(SettingsFileName);
end;

procedure THikingSettings.SetDefaultGPXFilter(const Value: string);
begin
  fDefaultGPXFilter := Value;
end;

procedure THikingSettings.SetMapFolder(const Value: string);
begin
  fMapFolder := Value;
end;

initialization
  gHikingSettings := nil;

finalization
  FreeAndNil(gHikingSettings);
end.
