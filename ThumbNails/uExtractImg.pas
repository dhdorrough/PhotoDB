unit uExtractImg;

interface
uses
  classes, windows, graphics, activex, shlobj2, comObj;

type
  IExtractImage = interface
  ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}']
    function GetLocation(pszPathBuffer: PWideChar;
                                 cch: cardinal;
                                 var pdwPriority: cardinal;
                                 var prgSize: TSize;
                                 dwRecClrDepth: cardinal;
                                 var pdwFlags: cardinal): HResult; stdcall;
    function Extract(var phBmpThumbnail: HBITMAP): HResult; stdcall;
  end;

TExtractImgEvent=procedure (Sender:TObject;const Filename:string;
                  const Thumbnail:TBitmap) of object;

  TBaseExtractImage=class(TObject)
  private
    FWidth,FHeight:integer;
    FOnExtract:TExtractImgEvent;
  protected
    function GetTargetFolder(const
    aFolder:string):IShellFolder;
    procedure DoExtractImg(ImgExtractor:IExtractImage);
    procedure ExtractImg(const filename:string;
                                 const Thumbnail:TBitmap);virtual;
  public
     procedure Extract;virtual;abstract;
     property Width:integer read FWidth write FWidth;
     property Height:integer read FHeight write FHeight;
     property OnExtract:TExtractImgEvent read FOnExtract write FOnExtract;
  end;

  TExtractImage=class(TBaseExtractImage)
  private
    FFilename:string;
  public
    procedure Extract;override;
  published
    property Filename:string read FFilename write FFilename;
    property OnExtract;
  end;

  TExtractFolderImage=class(TBaseExtractImage)
  private
    FFolder:string;
  public
    procedure Extract;override;
  published
    property Folder:string read FFolder write FFolder;
    property OnExtract;
  end;

implementation

uses sysutils;

function TBaseExtractImage.GetTargetFolder(const aFolder:string):IShellFolder;
var
  MallocObj: IMalloc;
  DesktopFolder,TargetFolder: IShellFolder;
  Eaten,attr: cardinal;
  itemPIDL: PItemIDList;
  awidefolder: widestring;
begin
   aWideFolder := aFolder;
   SHGetMalloc(mallocObj);
   SHGetDesktopFolder(DesktopFolder);

   DesktopFolder.ParseDisplayName(0,nil,
                               PWideChar(aWideFolder),
                               Eaten, ItemPIDL, attr);
   try
     DesktopFolder.BindToObject(ItemPIDL,nil,
                                IShellFolder,TargetFolder);
     result := TargetFolder;
   finally
     mallocObj.Free(ItemPIDL);
   end;
end;

const
  IEIFLAG_ASYNC = $001;
  IEIFLAG_CACHE = $002;
  IEIFLAG_ASPECT = $004;
  IEIFLAG_OFFLINE = $008;
  IEIFLAG_GLEAM = $010;
  IEIFLAG_SCREEN = $020;
  IEIFLAG_ORIGSIZE = $040;
  IEIFLAG_NOSTAMP = $080;
  IEIFLAG_NOBORDER = $100;
  IEIFLAG_QUALITY = $200;

procedure TBaseExtractImage.DoExtractImg(imgExtractor: IExtractImage);
var Thumbnail: TBitmap;
     ThumbnailHandle: HBITMAP;
     Buf: array[0..MAX_PATH] of WideChar;
     ColorDepth, Priority, Flags: DWORD;
     size:TSize;
     res:HResult;
begin
   priority     := 0;      //normal priority
   colorDepth   := 32;     //32 bit color depth
   size.cx      := FWidth; //thumbnail size
   size.cy      := FHeight;
   //display as the screen and the only reply of
   flags        := IEIFLAG_SCREEN or IEIFLAG_OFFLINE;

   res          := imgExtractor.GetLocation(@Buf, sizeof(Buf), priority, size, colorDepth, Flags);
   if (res=NOERROR) or (res=E_PENDING) then
     begin
       ThumbnailHandle := 0;
       imgExtractor.Extract(ThumbnailHandle);
       if ThumbnailHandle <> 0 then
         begin
           Thumbnail := TBitmap.Create;
           try
             Thumbnail.ReleaseHandle;
             Thumbnail.Handle := ThumbnailHandle;
             ExtractImg(buf, thumbnail);
           finally
             Thumbnail.Free;
         end;
     end;
  end;
end;

procedure TBaseExtractImage.ExtractImg;
begin
   if Assigned(FOnExtract) then
      FOnExtract(self,Filename,Thumbnail);
end;

{TExtractImage}
procedure TExtractImage.Extract;
var
  TargetFolder  : IShellFolder;
  attr, eaten   : cardinal;
  itemPIDL      : PItemIDList;
  ImgExtractor  : IExtractImage;
  folder        : string;
  mallocObj     : IMalloc;
  afilename     : wideString;
begin
   folder := ExtractFilePath(FFilename);
   delete(folder, length(folder), 1);
   TargetFolder := GetTargetFolder(folder);
   if TargetFolder=nil then exit;
   afilename := ExtractFilename(FFilename);
   SHGetMalloc(mallocObj);

   TargetFolder.ParseDisplayName(0, nil, PWideChar(aFilename), eaten, ItemPIDL, attr);
   try
     TargetFolder.GetUIObjectOf(0,1,itemPIDL,IExtractImage, nil,ImgExtractor);
   finally
     mallocObj.Free(itemPIDL);
   end;

   DoExtractImg(ImgExtractor);
end;

{TExtractFolderImage}
procedure TExtractFolderImage.Extract;
var
  TargetFolder : IShellFolder;
  attr, eaten  : cardinal;
  itemPIDL     : PItemIDList;
  ImgExtractor : IExtractImage;
  mallocObj    : IMalloc;
  afilename    : widestring;
  searchRec    : TSearchRec;
begin
   TargetFolder := GetTargetFolder(FFolder);
   if TargetFolder=nil then exit;
   if FindFirst(FFolder+'\*.*', faAnyFile, SearchRec)=0 then
   begin
      SHGetMalloc(mallocObj);
      repeat
          afilename:=searchRec.Name;
          if (searchRec.Name<>'.') and (searchRec.Name<>'..') then
          begin
             TargetFolder.ParseDisplayName(0, nil, PWideChar(aFilename), eaten, ItemPIDL, attr);
             try
               TargetFolder.GetUIObjectOf(0, 1, itemPIDL, IExtractImage, nil,ImgExtractor);
             finally
               mallocObj.Free(itemPIDL);
             end;
             DoExtractImg(ImgExtractor);
          end;
       until (FindNext(searchRec)<>0);
       FindClose(searchRec);
    end;
end;

end.


