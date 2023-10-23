unit JpegConv;

interface

uses Windows, Graphics, SysUtils, Classes;

type
  EOrientationNotSet = class(Exception);

procedure CreateThumbnail(InStream, OutStream: TStream;
  Width, Height: Integer; FillColor: TColor=clWhite); overload;
procedure CreateThumbnail(const InFileName, OutFileName: string;
  Width, Height: Integer; FillColor: TColor=clWhite); overload;

implementation

uses Jpeg, RotImg, PhotoUtils,
{$IfDef dExif2}
     dMetadata,
{$endIf}
     dExif, MyUtils;

procedure CreateThumbnail(InStream, OutStream: TStream;
  Width, Height: Integer; FillColor: TColor=clWhite);
var
  JpegImage: TJpegImage;
  Bitmap: TBitmap;
  Ratio: Double;
  ARect: TRect;
  AHeight: Integer;
  AWidth: Integer;
begin
//  Check for invalid parameters
  if Width<1 then
    raise Exception.Create('Invalid Width');

  if Height<1 then
    raise Exception.Create('Invalid Height');

  JpegImage := TJpegImage.Create;
  try
//  Load the image
    JpegImage.LoadFromStream(InStream);

//  Create bitmap, and calculate parameters
    Bitmap := TBitmap.Create;
    try
      Ratio := JpegImage.Width/JpegImage.Height;
      if Ratio > 1 then
        begin
          AHeight       := Round(Width/Ratio);
          AWidth        := Width;
        end
      else
        begin
          AWidth        := Round(Height*Ratio);
          AHeight       := Height;
        end;
      Bitmap.Width  := AWidth;
      Bitmap.Height := AHeight;
      Bitmap.Canvas.Brush.Color := FillColor;
      Bitmap.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
      ARect := Rect(0 {AWidthOffset},
                    0 {AHeightOffset},
                    AWidth{+AWidthOffset},
                    AHeight{+AHeightOffset});
      Bitmap.Canvas.StretchDraw(ARect, JpegImage);

// Assign back to the Jpeg, and save to the file
      JpegImage.Assign(Bitmap);
      JpegImage.SaveToStream(OutStream);
    finally
      Bitmap.Free;
    end;
  finally
    JpegImage.Free;
  end;
end;


procedure CreateThumbnail(const InFileName, OutFileName: string;
  Width, Height: Integer; FillColor: TColor=clWhite); overload;
var
  InStream, OutStream: TFileStream;
  OutStream2: TMemoryStream;
  Angle : integer;
  ImgPhoto: TRotateImage;
  Jpg: TJPEGImage;
  ImgData: TImgData;
begin
//IF POS('IMG_8357', InFileName) > 0 THEN  // DEBUG 8/16/2022 -
//BEGIN
    InStream := TFileStream.Create(InFileName,fmOpenRead);
    try
      OutStream := TFileStream.Create(OutFileName, fmOpenWrite or fmCreate);
      try
        CreateThumbnail(InStream, OutStream, Width, Height, FillColor);
      finally
        OutStream.Free;
      end;
    finally
      InStream.Free;
    end;

    // Big kludge: If the image needs to be rotated, do it after creating the thumbnail file
    //             rather than part of the same operation.
    Angle := RotationNeeded(InFileName);
    if Angle <> 0 then
      begin
        ImgPhoto := TRotateImage.Create(nil);
        try
  //      ImgPhoto.Picture.LoadFromFile(OutFileName);
          RotatePhoto(OutFileName, ImgPhoto, Angle);
          ImgData    := TImgData.Create;
          // Base rotation of thumbnail on the original image and copy its EXIF info
          if ImgData.ProcessFile(InFileName) and ImgData.HasEXIF then
            begin
              Jpg        := TJPEGImage.Create;
              Jpg.LoadFromFile(OutFileName);
              OutStream2 := TMemoryStream.Create;
              try
                Jpg.Assign(ImgPhoto.RotatedBitmap);
                Jpg.SaveToStream(OutStream2);
  {$IfNDef dExif2}   // Use the old version of dExif
                ImgData.WriteEXIFJpeg(OutFileName);
                raise EOrientationNotSet.CreateFmt('Orientation is not getting set for %s', [OutFileName]);
  {$Else}
                ImgData.ExifObj.TagValue['Orientation'] := 'Horizontal (normal)';
                ImgData.WriteEXIFJpeg(OutStream2, OutFileName);
  {$endIf dExif2}
              finally
                OutStream2.Free;
                Jpg.Free;
                ImgData.Free;
              end;
            end;
        finally
          ImgPhoto.Free;
        end;
      end;
//END;  // DEBUG
end;

end.

