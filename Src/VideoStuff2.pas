unit VideoStuff2;

interface

uses
  Classes, Forms, {MPlayer,} Controls;

const
  BytesPerSec = 1058694;   // This is really only an approximation for MP4 at 1920x1080, 30fps
                           // -- needs to be based on the actual video format

  function FrameToSeconds(Framecode: string): double;
  function LengthChangeInSeconds(FileSizeChange: int64): int64;
  function SecondsToFrame(Seconds: double): string;
  function SignificantFileSizeChange(SavedFileSize, CurrentFileSize: double; var FileSizeChange: int64): boolean;
  function GetMediaDates( const FileName: string;
                           var File_Modified_Date_Local: TDateTime;
                           var Encoded_Date: TDateTime;
                           var File_Modified_Date: TDateTime;
                           var File_Created_Date_Local: TDateTime;
                           var File_Created_Date: TDateTime): boolean;
  function GetMediaProperties(FileName: string;
                               var MediaFrames, MediaLength, MediaWidth, MediaHeight: Integer;
                               var MediaLengths: string;
                               var RecordedDate: TDateTime): boolean;
  function GetMediaLength(FileName: string): string;

implementation

uses
  StStrL, Math, MyUtils, Types, SysUtils, MediaInfoDLL, PDB_Decl, PDBUtils,
  YesNoDontAskAgain;

const
  LIBRARY_LOCATION  = 'MediaInfo.dll';

var
  gMediaInfoHandle: Cardinal = 0;
  gOpenFailed: boolean = false;
  gLibrary_Location: string;

  function SignificantFileSizeChange(SavedFileSize, CurrentFileSize: double; var FileSizeChange: int64): boolean;
  var
    ChangeInSeconds : int64;
  begin
     FileSizeChange        := Round(CurrentFileSize-SavedFileSize);
     ChangeInSeconds       := LengthChangeInSeconds(FileSizeChange);
     result                := Abs(ChangeInSeconds) > 6 { seconds};
  end;

  function LengthChangeInSeconds(FileSizeChange: int64): int64;
  begin
    result := Round(FileSizeChange / BytesPerSec);
  end;

  function FrameToSeconds(Framecode: string): double;
  // Assumes FrameCode in the format "hh;mm;ss;ff", "mm;ss;ff" or "ss;ff"
  const
    DELIMS = ';:';
  var
    wc: cardinal;
    ff, ss, mm, hh: variant;
  begin
    result := 0;
    hh := 0;
    mm := 0;
    ss := 0;
    ff := 0;

    wc := WordCountL(FrameCode, ';:');
    if wc > 0 then
      begin
        try
          ff := ExtractWordL(wc,   FrameCode, DELIMS);
          if wc > 1 then
            begin
              ss := ExtractwordL(wc-1, FrameCode, DELIMS);
              if wc > 2 then
                begin
                  mm := ExtractwordL(wc-2, FrameCode, DELIMS);
                  if wc > 3 then
                    hh := ExtractWordL(wc-3, FrameCode, DELIMS);
                end;
            end;
          result := RoundTo((hh * 3600) + (mm * 60) + ss + (ff / 29), -2);
        except
          result := 0;
        end;
      end;
  end;

  function SecondsToFrame(Seconds: double): string;
  var
    hh, mm, ss, ff: integer;
    Secs: integer;
  begin
    ff      := Round(Frac(Seconds) * 29);
    Secs    := Trunc(Int(Seconds));
    hh      := Secs div 3600;
    Secs    := Secs mod 3600;
    mm      := Secs div 60;
    Secs    := Secs mod 60;
    ss      := Secs;

    result   := RZero(hh,2) + ';' + RZero(mm,2) + ';' + RZero(ss, 2) + ';' + RZero(ff, 2);
  end;

  function StreamKind(mt: TMediaType): TMIStreamKind;
  begin
    if IsAudioMedia(mt) then
      result := Stream_Audio else
    if IsVideoMedia(mt) then
      result := Stream_Video
    else
      result := Stream_General;
  end;

  function GetMediaLength(FileName: string): string;
  var
    mt: TMediaType;
    Ext: String;
    Seconds: double;
  begin
    result := '';
      if FileExists(FileName) then
        begin
          Ext := ExtractFileExt(FileName);
          mt  := MediaTypeFromExtension(ext);

          if MediaInfoA_Open(gMediaInfoHandle, pchar(FileName)) = 1 then // opened correctly
            begin
              if mt = mt_Wav then
                result := MediaInfoA_Get(gMediaInfoHandle, Stream_Audio, 0, 'Duration/String3', Info_Text, Info_Name)
              else
                result := MediaInfoA_Get(gMediaInfoHandle, StreamKind(mt), 0, 'Duration/String4', Info_Text, Info_Name);

              if result = '' then
                begin
                  result := MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'Duration', Info_Text, Info_Name);
                  if (result <> '') and IsPureNumeric(result) then
                    begin
                      seconds := StrToFloat(result) / 1000.0;
                      result  := SecondsToFrame(seconds);
                    end;
                end;
//            MediaInfoA_Close(gMediaInfoHandle);
            end;
        end
      else
        result := 'Missing File';
  end;

  function MyLoadLibrary: boolean;
  var
    Temp: string;
  begin { MyLoadLibrary }
    result := (gMediaInfoHandle <> 0);
    if (not result) and (not gOpenfailed) then
      begin
        try
          result := MediaInfoDLL_Load(gLibrary_Location);
          if result then
            gMediaInfoHandle := MediaInfo_New()
          else
            begin
              if YesFmt('%s could not be found. Do you want to search for it?', [gLibrary_Location]) then
                begin
                  Temp := gLibrary_Location;
                  if BrowseForFile('Search for MediaInfo.dll (normally in same folder as .exe', Temp, 'DLL') then
                    begin
                      result := MediaInfoDLL_Load(Temp);
                      if result then
                        begin
                          gMediaInfoHandle  := MediaInfo_New();
                          gOpenFailed       := false;
                          gLibrary_Location := Temp;
                        end
                      else
                        begin
                          gMediaInfoHandle := 0;
                          gOpenFailed      := true;
                          gLibrary_Location := LIBRARY_LOCATION;
                        end;
                    end;
                end
              else
                begin
                  gOpenFailed       := false;
                  raise Exception.CreateFmt('Unable to locate "%s". Media lengths will not be set', [gLibrary_Location]);
                end;
            end;
        except
          result := false;
        end;
      end;
  end;  { MyLoadLibrary }

  function ToStandardDate(const s: string; Start, Len: integer): TDateTime;
  var
    Temp, DatePart, TimePart: string;
    Year, Month, Day: word;
    Hour, Min, Sec: word;
    Date, Time: TDateTime;
  begin { ToStandardDate }
    result := BAD_DATE;

    if s <> '' then
      begin
        temp     := Copy(s, Start, Len);
        DatePart := RemoveBadChars(Copy(temp, 1, 10), ['-']);
        TimePart := Copy(temp, 12, 8);
      end
    else
      temp := '';

    if temp <> '' then
      if IsYYYYMMDD(DatePart, year, month, day) then
        begin
          Date   := EncodeDate(Year, Month, Day);
          result := Date;
          if IsHH_MM_SS(TimePart, Hour, Min, Sec) then
            begin
              Time := EncodeTime(Hour, Min, Sec, 0);
              result := Date + Time;
            end;
        end;
  end;  { ToStandardDate }

  function GetMediaDates( const FileName: string;
                           var File_Modified_Date_Local : TDateTime;
                           var Encoded_Date             : TDateTime;
                           var File_Modified_Date       : TDateTime;
                           var File_Created_Date_Local  : TDateTime;
                           var File_Created_Date        : TDateTime): boolean;
  var
    r1, r2, r3, r4, r5: boolean;
  begin
    result := false;
    if MyLoadLibrary then
      if FileExists(FileName) then
        begin
          if MediaInfoA_Open(gMediaInfoHandle, pchar(FileName)) = 1 then
            begin
              File_Modified_Date_Local := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'File_Modified_Date_Local', Info_Text, Info_Name), 1, 19);
                          //  '2015-12-03 09:18:13.944'  (This seems to be the time that the video was taken in most cases-- No guarantees)

              Encoded_Date             := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'Encoded_Date', Info_Text, Info_Name), 5, 19);
                          //  'UTC 2006-05-23 16:54:23.674'

              File_Modified_Date       := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'File_Modified_Date', Info_Text, Info_Name), 5, 19);
                          // 'UTC 2011-02-06 13:57:01.168'

              File_Created_Date_Local  := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'File_Created_Date_Local', Info_Text, Info_Name), 1, 19);
                          // '2015-12-03 09:18:13.944'

              File_Created_Date        := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General,  0, 'File_Created_Date', Info_Text, Info_Name), 5, 19);
                          // 'UTC 2015-12-03 14:18:13.944'
              r1 := File_Modified_Date_Local <> BAD_DATE;
              r2 := Encoded_Date             <> BAD_DATE;
              r3 := File_Modified_Date       <> BAD_DATE;
              r4 := File_Created_Date_Local  <> BAD_DATE;
              r5 := File_Created_Date        <> BAD_DATE;

              result := r1 or r2 or r3 or r4 or r5; // true if we got at least one result
            end;
        end;
  end;

  function GetMediaProperties(FileName: string;
    var MediaFrames, MediaLength, MediaWidth, MediaHeight: Longint;
    var MediaLengths: string;
    var RecordedDate: TDateTime): boolean;
  var
    Temp{, Ext}: string;

    function GetMediaDate: TDateTime;
    begin { GetMediaDate }
      result := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'File_Modified_Date_Local', Info_Text, Info_Name), 1, 19);
                //  '2015-12-03 09:18:13.944'  (This seems to be the time that the video was taken in most cases-- No guarantees)

      if result = BAD_DATE then
        result := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'Encoded_Date', Info_Text, Info_Name), 5, 19);
                //  'UTC 2006-05-23 16:54:23.674'

      if result = BAD_DATE then
        result := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'File_Modified_Date', Info_Text, Info_Name), 5, 19);
                // 'UTC 2011-02-06 13:57:01.168'

      if result = BAD_DATE then
        result := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General, 0, 'File_Created_Date_Local', Info_Text, Info_Name), 1, 19);
                // '2015-12-03 09:18:13.944'

      if result = BAD_DATE then
        result := ToStandardDate(MediaInfoA_Get(gMediaInfoHandle, Stream_General,  0, 'File_Created_Date', Info_Text, Info_Name), 5, 19);
                // 'UTC 2015-12-03 14:18:13.944'
    end;  { GetMediaDate }

  begin { GetMediaProperties }
    result      := false;
    MediaFrames := 0;
    MediaLength := 0;
    MediaWidth  := 0;
    MediaHeight := 0;
    if MyLoadLibrary then
      if FileExists(FileName) then
        begin
//        Ext := ExtractFileExt(FileName);

          MediaLengthS := GetMediaLength(FileName);
          MediaLength  := Round(FrameToSeconds(MediaLengthS));

          if MediaInfoA_Open(gMediaInfoHandle, pchar(FileName)) = 1 then
            begin
              RecordedDate := GetMediaDate;

              temp := MediaInfoA_Get(gMediaInfoHandle, Stream_Video, 0, 'FrameRate_Nominal', Info_Text, Info_Name);
              if temp <> '' then
                MediaFrames := Round(StrToFloat(temp));

              temp := MediaInfoA_Get(gMediaInfoHandle, Stream_Video, 0, 'Height', Info_Text, Info_Name);
              if temp <> '' then
                MediaHeight := StrToInt(temp);

              temp := MediaInfoA_Get(gMediaInfoHandle, Stream_Video, 0, 'Width', Info_Text, Info_Name);
              if temp <> '' then
                MediaWidth := StrToInt(temp);

              result := true;
            end;
      end;
  end;  { GetMediaProperties }

initialization
  gMediaInfoHandle := 0;
  gLibrary_Location := LIBRARY_LOCATION;
finalization
  if gMediaInfoHandle <> 0 then
    MediaInfoA_Close(gMediaInfoHandle);

end.
