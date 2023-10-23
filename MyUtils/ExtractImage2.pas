unit ExtractImage2;

interface

uses
  Windows, Types, ShlObj2, ActiveX, ComObj, Graphics;

type
  IExtractImage = interface
    ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}'] 
    function GetLocation( 
      pszPathBuffer: PWideChar;
      cch: DWORD; 
      var pdwPriority: DWORD; 
      var prgSize: TSize; 
      dwRecClrDepth: DWORD; 
      var pdwFlags: DWORD): HResult; stdcall; 
    function Extract( 
      var phBmpThumbnail: HBITMAP): HResult; stdcall; 
  end;

const
  IEIFLAG_OFFLINE     =$0008;      // if the extractor shouldn't hit the net to get any content needed for the rendering
  IEIFLAG_SCREEN      =$0020;      // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )

procedure ExtractThumbnail(Bitmap:TBitmap; FileName:string; DesiredWidth:integer=120; DesiredHeight:integer=120);

implementation

uses
  SysUtils;

procedure ExtractThumbnail(Bitmap:TBitmap; FileName:string; DesiredWidth:integer=120; DesiredHeight:integer=120);
var
  Malloc:IMalloc;
  DesktopFolder,SourceFolder:IShellFolder;
  eaten,flags,prio:cardinal;
  id:PItemIDList;
  ex:IExtractImage;
  s:TSize;
  h:HBITMAP;
  w:WideString;
begin
  try
    OleCheck(SHGetMalloc(Malloc));
    OleCheck(SHGetDesktopFolder(DesktopFolder));
    flags := 0;
    w := ExtractFilePath(FileName);
    OleCheck(DesktopFolder.ParseDisplayName(0, nil, PWideChar(w), eaten, id, flags));
    try
      OleCheck(DesktopFolder.BindToObject(id, nil, IShellFolder, SourceFolder));
    finally
      Malloc.Free(id);
    end;
    w := ExtractFileName(FileName);
    OleCheck(SourceFolder.ParseDisplayName(0, nil, PWideChar(w), eaten, id, flags));
    try
      OleCheck(SourceFolder.GetUIObjectOf(0, 1, id,IExtractImage, nil, ex));
    finally
      Malloc.Free(id);
    end;
    s.cx  := DesiredWidth;
    s.cy  := DesiredHeight;
    flags := IEIFLAG_SCREEN or IEIFLAG_OFFLINE;
    prio  := 0;
    SetLength(w,MAX_PATH);
    OleCheck(ex.GetLocation(PWideChar(w), Length(w)*2, prio, s, 32, flags));
    OleCheck(ex.Extract(h));
    Bitmap.Handle:=h;
  finally
    DesktopFolder := nil;
    SourceFolder  := nil;
    Malloc        := nil;
  end;
end;

end.
 