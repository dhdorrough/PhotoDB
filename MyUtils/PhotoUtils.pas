unit PhotoUtils;

interface

uses
  Controls, DbCtrls, PDB_Decl, Windows, dExif, ExtCtrls, RotImg;

function  DecodeExifDate( const DateString: string;
                          var aYear, aMonth, aDay: word;
                          var Hr, Min, Sec: word): boolean;
procedure EditPhotoUtil(const FileName, FotoCanvasEXE: string);
function  ExifDateToDateTime(const ExifDate: string): TDateTime;
function  GetPhotoInfo(var UpdateInfo: TUpdateInfo): boolean;
function  GetLatitudeLongitudeFromEXIF(const Lfn: string; var Latitude, Longitude: double): boolean;
function  RotationNeeded(const aFileName: string): integer;
procedure InnerSizePhoto( aLfn: string;
                          imgPhoto: TRotateImage;
                          aControl: TControl;
                          mc: TMediaClass;
                          AllowRotation: boolean;
                          const PhotoEditingProgram: string;
                          var WasEdited: boolean);
procedure RotatePhoto(const aFileName: string; ImgPhoto: TRotateImage; anAngle: integer);

implementation

uses
  ThumbNailUnit, SysUtils, MyUtils, PDBUtils, DateUtils, LocationUtils, Graphics, JPeg,
  VideoStuff2
{$IfDef dExif2}
  , dMetaData
{$endIf}
  ;

function DecodeEXIFDate( const DateString: string;
                         var aYear, aMonth, aDay: word;
                         var Hr, Min, Sec: word): boolean;
  var
    Temp: string;
begin
  // Like: '2002:11:11 08:52:58'
  result := (Length(DateString) >= 10)
            and (DateString[5] = ':')
            and (DateString[8] = ':');
  if result then
    begin
      temp   := Copy(DateString, 1, 4);
      result := IsAllNumeric(temp);
      if result then
        begin
          aYear  := MyStrToInt(temp);
          temp   := Copy(DateString, 6, 2);
          result := IsAllNumeric(temp);
          if result then
            begin
              aMonth := MyStrToInt(Temp);
              Temp   := Copy(DateString, 9, 2);
              result := IsAllNumeric(Temp);
              if result then
                begin
                  aDay := MyStrToInt(Temp);
                  Temp := Copy(DateString, 12, 2);
                  result := IsAllNumeric(Temp);
                  if result and (Length(DateString) >= 19) then
                    begin
                      Hr     := MyStrToInt(Temp);
                      Temp   := Copy(DateString, 15, 2);
                      result := IsAllNumeric(Temp);
                      if result then
                        begin
                          Min := MyStrToInt(Temp);
                          Temp := Copy(DateString, 18, 2);
                          result := IsAllNumeric(Temp);
                          if result then
                            begin
                              Sec    := MyStrToInt(Temp);
                              result := (aYear <> 0) and (aMonth <> 0) and (aDay <> 0);
                            end;
                        end;
                    end
                  else
                    begin
                      Hr := 12; Min := 0; Sec := 0;
                    end;
                end;
            end;
        end;
    end;
end;

function ExifDateToDateTime(const ExifDate: string): TDateTime;
  var
    Year, Month, Day, Hr, Min, Sec: word;
begin
  if DecodeExifDate(ExifDate, Year, Month, Day, Hr, Min, Sec) then
    result := EncodeDate(Year, Month, Day) + EncodeTime(Hr, Min, Sec, 0)
  else
    result := BAD_DATE;
end;

// Try to use the date recorded by the camera rather than the date that the
// file may have been updated.
function GetPhotoInfo(var UpdateInfo: TUpdateInfo): boolean;
var
  aYear, aMonth, aDay, aHr, aMin, aSec, aMs: word;
//Temp,
  PartialDate: string;
  aDateTime: TDateTime;
  mt: TMediaType;
  hBmp: HBITMAP;
  BitMap: TBitMap;
  JpegImage: TJpegImage;
  creationTime, lastAccessTime, lastModificationTime: TDateTime;
  ImgData: TImgData;
  MediaFrames, MediaLength, MediaWidth, MediaHeight: Longint;
  MediaLengths: string;
  RecordedDate, TempDateTime: TDateTime;

  procedure GetHeightWidthFromImage;
  var
    Image: Timage;
  begin { GetHeightWidthFromImage }
    Image := TImage.Create(nil);
    try
      Image.Picture.LoadFromFile(UpdateInfo.ImagePathName);
      UpdateInfo.PixelWidth    := Image.Picture.Width;
      UpdateInfo.PixelHeight   := Image.Picture.Height;
    finally
      Image.Free;
    end;
  end;  { GetHeightWidthFromImage }

begin { GetPhotoInfo }
  UpdateInfo.PhotoDateYYYYMMDD := '';
  result := true;

  // Look for a date in the File NAME if requested
  if (UpdateInfo.PhotoDateYYYYMMDD = '') and (dk_From_FileName in UpdateInfo.FileNameInfo.DateKinds) then
    begin
      if FileNameContainsPartialDate(UpdateInfo.FileName, PartialDate, UpdateInfo.FileNameInfo) then
        begin
          UpdateInfo.PhotoDateYYYYMMDD := PartialDate;
          UpdateInfo.PhotoDateTime := PhotoDate2DateTime( PartialDate,
                                               dd_NoDate { default to no date });
        end;
    end;

  mt := MediaTypeFromExtension(ExtractFileExt(UpdateInfo.FileName));
  // Parse the EXIF data to get a date
  if IsPhotoMedia(mt) then
    begin
      GetHeightWidthFromImage;
      // Look for a date in the EXIF if requested and not already set
      if (UpdateInfo.PhotoDateYYYYMMDD = '') and (dk_from_EXIF in UpdateInfo.FileNameInfo.DateKinds) then
        begin
          ImgData := TImgData.Create;

          try
            if ImgData.ProcessFile(UpdateInfo.ImagePathName) then
              if ImgData.HasEXIF then
                begin
//                Temp := ImgData.ExifObj.DateTime;
                  TempDateTime := ImgData.ExifObj.GetImgDateTime;
                  DecodeDate(TempDateTime, aYear, aMonth, aDay);
//                if DecodeExifDate(Temp, aYear, aMonth, aDay, aHr, aMin, aSec) then
                    if (aYear <> 0) and (aMonth <> 0) and (aDay <> 0) then
                      begin
                        UpdateInfo.PhotoDateYYYYMMDD := YearMonthDayToPhotoDate(aYear, aMonth, aDay);
                        if UpdateInfo.PixelWidth = 0 then
                          UpdateInfo.PixelWidth  := ImgData.ExifObj.Width;  // set by GetHeightWidthFromImage above
                        if UpdateInfo.PixelHeight = 0 then
                          UpdateInfo.PixelHeight := ImgData.ExifObj.Height;
//                      UpdateInfo.PhotoDateTime := EncodeDateTime(aYear, aMonth, aDay, aHr, aMin, aSec, 0);
                        UpdateInfo.PhotoDateTime := TempDateTime;
                      end
                    else
                      AlertFmt('Invalid date in EXIF: (%d/%d/%d)', [aYear, aMonth, aDay])
                end;
          finally
            FreeAndNil(ImgData);
          end;
        end;
    end else
  if IsVideoMedia(mt) then
    begin
      if Succeeded(GetThumbnail(UpdateInfo.ImagePathName, hBmp, THUMBNAILWIDTH, THUMBNAILHEIGHT)) then
        begin
          BitMap        := TBitMap.Create;
          JpegImage     := TJpegImage.Create;
          try
            Bitmap.Handle := hBmp;
            JpegImage.Assign(BitMap);
            if dk_MediaInfo in UpdateInfo.FileNameInfo.DateKinds then
              if GetMediaProperties( UpdateInfo.ImagePathName,
                                     MediaFrames, MediaLength, MediaWidth, MediaHeight,
                                     MediaLengths,
                                     RecordedDate) then
                begin
                  UpdateInfo.PixelWidth  := MediaWidth;
                  UpdateInfo.PixelHeight := MediaHeight;
                  UpdateInfo.PhotoDateTime := RecordedDate;
                  DecodeDate(RecordedDate, aYear, aMonth, aDay);
                  UpdateInfo.PhotoDateYYYYMMDD := YearMonthDayToPhotoDate(aYear, aMonth, aDay);
                end
              else
                begin
                  UpdateInfo.PixelWidth  := JpegImage.Width;
                  UpdateInfo.PixelHeight := JpegImage.Height;
                end
            else
              begin
                UpdateInfo.PixelWidth  := JpegImage.Width;
                UpdateInfo.PixelHeight := JpegImage.Height;
              end;
          finally
            FreeAndNil(JpegImage);
            FreeAndNil(BitMap);
          end;
        end;
    end else
  if IsAudioMedia(mt) then
    begin
      if dk_MediaInfo in UpdateInfo.FileNameInfo.DateKinds then
        if GetMediaProperties( UpdateInfo.ImagePathName,
                               MediaFrames, MediaLength, MediaWidth, MediaHeight,
                               MediaLengths,
                               RecordedDate) then
          begin
            UpdateInfo.PhotoDateTime := RecordedDate;
            UpdateInfo.MediaLength   := MediaLengthS;
            DecodeDate(RecordedDate, aYear, aMonth, aDay);
            UpdateInfo.PhotoDateYYYYMMDD := YearMonthDayToPhotoDate(aYear, aMonth, aDay);
          end
    end;

  // If no date, then default to the FileDate, if requested
  if (UpdateInfo.PhotoDateYYYYMMDD = '') and (([dk_Date_Modified, dk_Date_Created] * UpdateInfo.FileNameInfo.DateKinds) <> []) then
    begin
      aDateTime := BAD_DATE;

      if GetFileTimes(UpdateInfo.ImagePathName, creationTime, lastAccessTime, lastModificationTime) then
        if dk_Date_Modified in UpdateInfo.FileNameInfo.DateKinds then
          aDateTime := lastModificationTime else
        if dk_Date_Created in UpdateInfo.FileNameInfo.DateKinds then
          aDateTime := creationTime;

      if aDateTime <> BAD_DATE then
        begin
          DecodeDateTime(aDateTime, aYear, aMonth, aDay, aHr, aMin, aSec, aMs);
          UpdateInfo.PhotoDateYYYYMMDD := YearMonthDayToPhotoDate(aYear, aMonth, aDay);
          UpdateInfo.PhotoDateTime := aDateTime;
        end;
    end;

  // As a last resort, look for a date in the File PATH if requested
  if (UpdateInfo.PhotoDateYYYYMMDD = '') and (dk_From_FileName in UpdateInfo.FileNameInfo.DateKinds) then
    begin
      if FileNameContainsPartialDate(UpdateInfo.ImagePathName, PartialDate, UpdateInfo.FileNameInfo) then
        begin
          UpdateInfo.PhotoDateYYYYMMDD := PartialDate;
          UpdateInfo.PhotoDateTime := PhotoDate2DateTime( PartialDate,
                                               dd_NoDate { default to no date });
        end;
    end;

end;  { GetPhotoInfo }

// returns rotation needed in degrees
function RotationNeeded(const aFileName: string): integer;
var
  ImgData   : TImgData;
  Temp      : string;
  Raw       : string;
//V1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12: Variant;
begin
  result  := 0;
  ImgData := TImgData.Create;
  try
    if ImgData.ProcessFile(aFileName) then
      if ImgData.HasEXIF then
        begin
{$IfNDef dExif2}   // Use the old version of dExif
          Temp := ImgData.ExifObj.Data['Orientation'].Data;
          Raw  := ImgData.ExifObj.Data['Orientation'].Raw;
{$Else}
          Temp := ImgData.ExifObj.TagByName['Orientation'].Data;
          Raw  := ImgData.ExifObj.TagByName['Orientation'].Raw;
{$endIf dExif2}
(*        Attempting to find a more reliable method of determining orientation
          V1 := ImgData.ExifObj.TagByName['Orientation'].Data;
          V2 := ImgData.ExifObj.TagByName['Orientation'].TID;        // TagTableID - EXIF use
          V3 := ImgData.ExifObj.TagByName['Orientation'].TType;         // tag type  -- see FMT_XXXX constants
          V4 := ImgData.ExifObj.TagByName['Orientation'].Tag;           // primary key
          V5 := ImgData.ExifObj.TagByName['Orientation'].ParentID;      // Tag ID of the parent folder
          V6 := ImgData.ExifObj.TagByName['Orientation'].Count;     // count of TType elements
          V7 := ImgData.ExifObj.TagByName['Orientation'].Name;        // searchable
          V8 := ImgData.ExifObj.TagByName['Orientation'].Desc;        // translatable
          V9 := ImgData.ExifObj.TagByName['Orientation'].Code;        // decode capability
          V10 := ImgData.ExifObj.TagByName['Orientation'].Raw;         // unprocessed value  -- DO NOT CHANGE TO STRING !!!
          V11 := ImgData.ExifObj.TagByName['Orientation'].FormatS;     // Format string
          V12 := ImgData.ExifObj.TagByName['Orientation'].Size;       // used by ITPC module
*)
          if (Temp <> '') and (Temp <> 'Normal') and (Temp <> 'Horizontal (normal)' {#0#1}) then
            begin
              if (Temp = 'Rotated 180°') or (Temp = 'Rotate 180') then
                result := + 180 else
              if (Temp = 'CounterClockwise 90°') then
                result := - 90 else
              if (Temp = 'Rotate 270 CW') then
                result := + 90 else
              if (Temp = 'Clockwise 90°') then
                result := + 90 else
              if (Temp = 'Rotate 90 CW') {#0#6} then
                result := - 90
              else
                raise Exception.CreateFmt('Unexpected orientation "%s"', [Temp]);
            end;
        end;
  finally
    ImgData.Free;
  end;
end;

procedure RotatePhoto(const aFileName: string; ImgPhoto: TRotateImage; anAngle: integer);
begin
  if Assigned(imgPhoto.Picture) then
    with ImgPhoto do
      try
        AutoSize       := true;
        Center         := true;
        Stretch        := false;
        UniqueSize     := false;
        Proportional   := true;
        Angle          := anAngle;
        Picture.LoadFromFile(aFileName);
      except
        on e:Exception do
          AlertFmt('Could not load %s (%s)', [aFileName, e.Message]);
      end;
end;

procedure EditPhotoUtil(const FileName, FotoCanvasEXE: string);
  var
//  FotoCanvasEXE, COMMENTED OUT FOR DEBUG
    Params: string;
    temp: string;
begin
//FotoCanvasEXE := CommonPhotoSettings.PhotoEditingProgram; 
  Params        := Format('"%s"', [FileName]);
  Temp          := FotoCanvasEXE + ' ' + Params;
  FileExecute(temp, true);
//SizePhoto; 
end;

procedure InnerSizePhoto( aLfn: string;
                          imgPhoto: TRotateImage;
                          aControl: TControl;
                          mc: TMediaClass;
                          AllowRotation: boolean;
                          const PhotoEditingProgram: string;
                          var WasEdited: boolean);
(*
  procedure HandlePhoto(aFileName: string);
  var
    Ratio: extended;
  begin { HandlePhoto }
    if Assigned(imgPhoto.Picture) then
      try
        imgPhoto.Picture.LoadFromFile(aFileName);

        // Copy JPEG to bitmap:
        imgPhoto.Align  := alNone;
        imgPhoto.Center := False;

        if imgPhoto.Picture.Height > 0 then
          Ratio  := imgPhoto.Picture.Width / imgPhoto.Picture.Height
        else
          exit; // ??;

        // Resize Bitmap
        if (aControl.ClientHeight > 0) and (aControl.ClientWidth > 0) then
          if (aControl.ClientWidth  < imgPhoto.Picture.Width) or
             (aControl.ClientHeight < imgPhoto.Picture.Height) then
            begin
             // If width is the dominant side...
             // Since form width and form height are usually different,
             // this needs to be tested with the ratio:
             if (imgPhoto.Picture.Height/aControl.ClientHeight <
                 imgPhoto.Picture.Width/aControl.ClientWidth) then
               begin
                  // width gets the max length
                  imgPhoto.Width  := aControl.ClientWidth;

                  // and height is calculated
                  imgPhoto.Height := Round( imgPhoto.Width / Ratio );

                  // put the ctrl to the left
                  imgPhoto.Left := 0;
                  if CenterPhoto then
                    imgPhoto.Top := (aControl.ClientHeight - imgPhoto.Height) div 2
                  else
                    imgPhoto.Top := 0;
               end
             else
               begin // OTHERWISE
                  // height gets the max len
                  imgPhoto.Height := aControl.ClientHeight;

                  // and width is calculated
                  imgPhoto.Width  := Round( imgPhoto.Height * Ratio );

                  // put the ctrl on top
                  imgPhoto.Top  := 0;
                  if CenterPhoto then
                    imgPhoto.Left := (aControl.ClientWidth - imgPhoto.Width) div 2
                  else
                    imgPhoto.Left := 0; // strange positioning with large font sizes
               end; // if (iDiffWidth < iDifHeight) then

           // make sure the image adjusts to the ctrl new size
           imgPhoto.Stretch := True;
          end
        else
          begin
           // the ctrl should be the size of the control
           imgPhoto.Align := alClient;

           // make sure the image is presented in
           // its correct size
           imgPhoto.Stretch := False;
           imgPhoto.Center  := CenterPhoto;
          end;
      except
        on EInvalidGraphic do
          imgPhoto.Picture.Graphic := nil;
      end;
  end;  { HandlePhoto }
*)

  procedure HandlePhoto(aFileName: string; AllowRotation: boolean);
  const
    ONEMB = 1000000;
    MAXFILESIZE = 40 * ONEMB;
    MAXFILESIZEMEG = MAXFILESIZE DIV ONEMB;
  begin { HandlePhoto }
    if Assigned(imgPhoto.Picture) then
      with ImgPhoto do
        try
          AutoSize     := true;
          Center       := true;
          Stretch      := false;
          UniqueSize   := false; // true;
          Proportional := true;
          if AllowRotation then
            ImgPhoto.Angle := RotationNeeded(aFileName)
          else                 
            ImgPhoto.Angle := 0;

          WasEdited := false;
          if FileSize32(aFileName) > MAXFILESIZE then
            begin
              WasEdited := YesFmt('This photo (%s) is greater then %dMb. It may not be loadable at this size. Do you want to edit it first?',
                      [aFileName, MAXFILESIZEMEG]);
              if WasEdited then
                EditPhotoUtil(aFileName, PhotoEditingProgram);
            end;

          Picture.LoadFromFile(aFileName);
        except
          on e:Exception do
            raise Exception.CreateFmt('Could not load %s (%s)', [aFileName, e.Message]);
        end;
  end;  { HandlePhoto }

  procedure HandleMedia(aFileName: string; AllowRotation: boolean);
  var
    hBmp: HBITMAP;
    ThumbNailName: string;
  begin { HandleMedia }
    ThumbNailName := ThumbNailPathAndName(aFileName);
    if FileExists(ThumbNailName) then
      HandlePhoto(ThumbNailName, AllowRotation)
    else
      begin
        imgPhoto.Stretch := false;
        imgPhoto.Align   := alClient;
        if Succeeded(GetThumbnail(aFileName, hBmp, imgPhoto.Width, imgPhoto.Height)) then
          imgPhoto.Picture.Bitmap.Handle := hBmp
        else
          imgPhoto.Picture.Graphic := nil;
      end;
  end;  { HandleMedia }

begin { InnerSizePhoto }
  case mc of
    mc_Photos:
      HandlePhoto(aLfn, AllowRotation);
    mc_Video, mc_Audio:
      HandleMedia(aLfn, AllowRotation);
    mc_Document:
      imgPhoto.Picture.Graphic := nil;
  end;
end;  { InnerSizePhoto }

function GetLatitudeLongitudeFromEXIF(const Lfn: string; var Latitude, Longitude: double): boolean;
var
  ImgData: TImgData;
begin
  result := false;
  ImgData := TImgData.Create();
  try
    if ImgData.ProcessFile(Lfn) then
      if ImgData.HasExif then
        begin
          with ImgData.ExifObj do
            begin
{$IfNDef dExif2}   // Use the old version of dExif
              Latitude    := GetDecimalDegrees(Data['GPSLatitudeRef'].Data, Data['GPSLatitude'].Data);
              Longitude   := GetDecimalDegrees(Data['GPSLongitudeRef'].Data, Data['GPSLongitude'].Data);
{$else}
              Latitude    := GetDecimalDegrees(TagByName['GPSLatitudeRef'].Data, TagByName['GPSLatitude'].Data);
              Longitude   := GetDecimalDegrees(TagByName['GPSLongitudeRef'].Data, TagByName['GPSLongitude'].Data);
{$EndIf dExif2}
              result      := (Latitude <> 0) or (Longitude <> 0);
            end;
        end;
  finally
    ImgData.Free;
  end;
end;

end.
