unit GenerateHTML2;

// 4/3/2015 Created from uMakeHTML today

interface

uses
  Classes, PDB_Decl, PDBTables, Forms, Controls, StdCtrls, CheckLst;

const
  HTML_MEDIA   = [mt_Jpeg, mt_JPG, mt_RM, mt_RV, mt_RAD
                  {, mt_WMV, mt_MOV, mt_MPG, mt_MP4, mt_M2TS}  // 12/14/2016 - removed these because they shouldn't be added to generated HTML?
                  ];

type
  TStatusUpdate = procedure {Update_Status}(const Msg: string; LineNo: integer = 0) of object;
  TFolderProcessed = procedure {FolderProcessed} (FolderName: string) of object;
  TFileAcceptor = function {AcceptThisFile}(const FileName: string): boolean of object;

  THTMLGenerator = class(TForm)
    cbProcessSubFolders: TCheckBox;
    btnBegin: TButton;
    Button2: TButton;
    cbDeleteOldHTML: TCheckBox;
    lblStatus: TLabel;
    ListBox1: TCheckListBox;
    procedure ListBox1ClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fDBRecsUpdated: integer;
    fColCount: integer;
    fDayCount: integer;
    fDeleteOldHTML: boolean;
    fFileNameInfo: TFileNameInfo;
    fFolderCount: integer;
    fIncludeEmpty: boolean;
    fOnOrAfter: TDateTime;
    fProcessSubFolders: boolean;
    fRowCount: integer;
    fStartTime: double;
    fTreeList: TStringList;
    fThumbCount: integer;
    fUpdateRecentOnly: boolean;
    fYear: integer;

    tblPhotoTable: TPhotoTable;
    fLevel: integer;
    fFoldersProcessed: integer;
    fTempFilePathsTable: TFilePathsTable;

    fFileAcceptor: TFileAcceptor;
    fOnUpdateStatus: TStatusUpdate;
    fOnUpdateLogFile: TStatusUpdate;
    fOnUpdateThumbStatus: TStatusUpdate;
    fAfterFolderProcessed: TFolderProcessed;

    procedure UpdateStatusProc(const Msg: string; LineNo: integer = 0);
    procedure SetIncludeEmpty(const Value: boolean);
    procedure SetProcessSubFolders(const Value: boolean);
    procedure SetStartTime(const Value: double);
  public
    procedure ProcessFolder(CallerTitle, FolderName, path, WildName: string);
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;

    property DBRecsUpdated: integer
             read fDBRecsUpdated
             write fDBRecsUpdated;
    property DayCount: integer
             read fDayCount
             write fDayCount;
    property ColCount: integer
             read fColCount
             write fColCount;
    property DeleteOldHTML: boolean
             read fDeleteOldHTML
             write fDeleteOldHTML;
    property FileNameInfo: TFileNameInfo
             read fFileNameInfo
             write fFileNameInfo;
    property FolderCount: integer
             read fFolderCount
             write fFolderCount;
    property IncludeEmpty: boolean
             read fIncludeEmpty
             write SetIncludeEmpty;
    property OnOrAfter: TDateTime
             read fOnOrAfter
             write fOnOrAfter;
    property ProcessSubFolders: boolean
             read fProcessSubFolders
             write SetProcessSubFolders;
    property AfterFolderProcessed: TFolderProcessed
             read fAfterFolderProcessed
             write fAfterFolderProcessed;
    property RowCount: integer
             read fRowCount
             write fRowCount;
    property TreeList: TStringList
             read fTreeList
             write fTreeList;
    property ThumbCount: integer
             read fThumbCount
             write fThumbCount;
    property UpdateRecentOnly: boolean
             read fUpdateRecentOnly
             write fUpdateRecentOnly;
    property StartTime: double
             read fStartTime
             write SetStartTime;
    property FileAcceptor: TFileAcceptor
             read fFileAcceptor
             write fFileAcceptor;
    property OnUpdateStatus: TStatusUpdate
             read fOnUpDateStatus
             write fOnUpDateStatus;
    property OnUpdateThumbStatus: TStatusUpdate
             read fOnUpdateThumbStatus
             write fOnUpdateThumbStatus;
    property OnUpdateLogFile: TStatusUpdate
             read fOnUpdateLogFile
             write fOnUpdateLogFile;
  end;

procedure GenerateIndexPage(const FolderName, path: string);

implementation

{$R *.DFM}

uses
  Windows, SysUtils, Math, MyUtils, PhotoDBCommonSettings, PDBUtils, StStrL, DateUtils,
  Types, PhotoUtils;

const
  FATTR = faAnyFile-faDirectory-faHidden-faSysFile-faVolumeID;
  FATTR2 = faAnyFile-faHidden-faSysFile-faVolumeID;

type

  TMediaList = class(TStringList)
  public
    Destructor Destroy; override;
    function HtmlMediaCount: integer;
  end;

  TImageInfo = class
  public
    Key: integer;
    KeyWords: string;
    CopyRight: string[4];
    PhotoDate: string[8];
    PhotoDateTime: TDateTime;
    CopyRightOwner: string;
    IsPrivate: boolean;
    MediaType: TMediaType;
    function IncludeThisPhoto: boolean;
  end;

  procedure GenerateIndexPage(const FolderName, path: string);
    var
      lfn: string;
      OutFile: textfile;
  begin { GenerateIndexPage }
    lfn   := Format('%s\%s.htm', [path, 'INDEX']);
    AssignFile(OutFile, lfn);
    ReWrite(OutFile);
    try
      WriteLn(outfile, '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
      WriteLn(outfile, '<html lang="en-US" xml:lang="en-US" xmlns="http://www.w3.org/1999/xhtml">');
      WriteLn(outfile, '<head><title>', FolderName,'</title>');

      if CommonPhotoSettings.UseLocalCSS then
        WriteLn(outfile, '<link rel="stylesheet" type="text/css" href="', CommonPhotoSettings.LocalCSSLfn, '" />')
      else
        WriteLn(outfile, '<link rel="stylesheet" type="text/css" href="', CommonPhotoSettings.RemoteCSSLfn, '" />');

      WriteLn(outfile, '</head>');

      WriteLn(outfile, '<body>');
      WriteLn(outfile, FolderName);

      WriteLn(outfile, '<br/><br/>');
      WriteLn(outfile, '<div class="footer">');
      WriteLn(outfile, CommonPhotoSettings.CopyRight, '<br/>');

      writeLn(outfile, 'Click on photo for larger view<br/><br/>');
      WriteLn(outfile, 'Last updated: ', DateToStr(Date()), ' at ', TimeToStr(Time()));
      WriteLn(outFile, '</div>');

      WriteLn(outfile, '</body>');
      WriteLn(outfile, '</html>');
    finally
      CloseFile(OutFile);
    end;
  end;  { GenerateIndexPage }

{ TMediaList }

destructor TMediaList.Destroy;
  var
    i: integer;
begin
  for i := 0 to Count-1 do
    Objects[i].Free;

  inherited;
end;

procedure THTMLGenerator.UpdateStatusProc(const Msg: string; LineNo: integer);
begin
  if Assigned(fOnUpdateStatus) then
    fOnUpdateStatus(Msg{, LineNo});
end;


constructor THTMLGenerator.Create(aOwner: TComponent);
begin
  inherited;
  fYear          := YearOf(Now);
  fDBRecsUpdated := 0;
  tblPhotoTable := TPhotoTable.Create( self,
                                       CommonPhotoSettings.PhotoDBDatabaseFileName,
                                       cFILENAMES, []);
  with tblPhotoTable do
    begin
      OnUpdateStatus := UpdateStatusProc;
      Active        := true;
    end;

  fTreeList         := TStringList.Create;
end;

procedure THTMLGenerator.ProcessFolder(CallerTitle, FolderName, path, WildName: string);
var
  ChildList: TStringList;
  HTMLList: TStringList;
  SiblingList: TStringList;
  MediaList: TMediaList;
  i: integer;
  ImageNr: integer;

  function IsMediaFile(const FileName: string; var MediaType: TMediaType): boolean;
    var
      Ext: string;
  begin { IsMediaFile }
    Ext       := ExtractFileExt(FileName);
    MediaType := MediaTypeFromExtension(Ext);
    result    := MediaType <> mt_Unk;
  end;  { IsMediaFile }

  function IsInDateRange(const FileName: string): boolean;
  var
    FileDateTimeStamp: integer;
    FileDateTime: TDateTime;
  begin { IsInDateRange }
    if UpdateRecentOnly then
      begin
        FileDateTimeStamp := FileAge(FileName);
        if FileDateTimeStamp >= 0 then
          begin
            FileDateTime      := FileDateToDateTime(FileDateTimeStamp);
            result            := FileDateTime >= fOnOrAfter;
          end
        else
          result := false;
      end
    else
      result := true;
  end;  { IsInDateRange }

  // Count known media types or sub-folders only. Basically determine if the specified folder
  // should be processed or not. Probably should be done recursively to be really correct.
  function FilesInChildFolder(Path, ChildName, WildName: string): integer;
    var
    DirInfo: TSearchRec;
    DosError: integer;
    IsADirectory: boolean;
    Temp, PathName, FullFileName: string;
    Lfn: string;
    mt: TMediaType;
  begin { FilesInChildFolder }
    result := 0;
    PathName := path + '\' + ChildName;
    Temp     := PathName + '\' + WildName;
    DosError := FindFirst(temp, faAnyFile, DirInfo);
    while DosError = 0 do
      begin
        IsADirectory := ((DirInfo.Attr and faDirectory) <> 0) and
                         not ((DirInfo.Name = '.') or (DirInfo.Name = '..'));
        Lfn := UpperCase(DirInfo.Name);
        FullFileName := PathName + '\' + DirInfo.Name;
        if (IsMediaFile(Lfn, mt) and IsInDateRange(FullFileName) and (fFileAcceptor(FullFileName))) or IsADirectory then
          inc(result);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { FilesInChildFolder }

  procedure GetListOfChildFolders(ChildNames: TStringList);
  var
    DirInfo: TSearchRec;
    DosError: integer;
  begin { GetListOfChildFolders }
    ChildNames.Clear;
    DosError := FindFirst(path + '\*.*', faAnyFile, DirInfo);
    while DosError = 0 do
      begin
        if (DirInfo.Name <> '.') and
           (DirInfo.Name <> '..') and
           (CompareText(DirInfo.Name, 'TN') <> 0) and
           (DirInfo.Name[1] <> '_') and
           ((DirInfo.Attr and faDirectory) <> 0) then
          ChildNames.Add(DirInfo.Name);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { GetListOfChildFolders }

  procedure GetListOfHTMLFiles(HTMLList: TStringList);
    var
      DOSError: integer;
      DirInfo: TSearchRec;
  begin
    HTMLList.Clear;
    DosError := FindFirst(path + '\*.htm', faAnyFile, DirInfo);
    while DosError = 0 do
      begin
        if (DirInfo.Name <> '.') and
           (DirInfo.Name <> '..') then
          HTMLList.Add(DirInfo.Name);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;

  procedure BuildSiblingList(SiblingList: TStringList);
  var
    PhotoCount: integer;
    Pages_Needed: integer;
    i: integer;
  begin { BuildSiblingList }
    // build the sibling list
    PhotoCount := MediaList.HtmlMediaCount;
    SiblingList.Clear;
{$IfDef debugging}
    Set8087CW(Default8087CW); // Clear "Invalid Floating Point Operation" that only occurs when debugging
{$endif}
    Pages_Needed := Ceil(PhotoCount / (fRowCount * ColCount));
    for i := 1 to Pages_Needed do
      SiblingList.Add(Format('INDEX-%2.2d', [i]));
  end;  { BuildSiblingList }

  procedure BuildMediaList(MediaList: TStringList; WildName: string);
  var
    DirInfo: TSearchRec;
    DosError: integer;
    FilePath, FullFileName: string;
    mt: TMediaType;
    ImageInfo: TImageInfo;
    RecordExists: boolean;
  begin { BuildMediaList }
    MediaList.Clear;
    // List the media in this folder
    FilePath := Format('%s\%s', [Path, WildName]);
    DosError := FindFirst(FilePath, FATTR2, DirInfo);
    while DosError = 0 do
      begin
        FullFileName := Path + '\' + DirInfo.Name;
        if IsMediaFile(DirInfo.Name, mt) and IsInDateRange(FullFileName) and fFileAcceptor(FullFileName) then
          begin
            RecordExists := tblPhotoTable.MyLocateRecord(DirInfo.Name, Path);
            if RecordExists then
              begin
                ImageInfo := TImageInfo.Create;
                with ImageInfo do
                  begin
                    MediaType     := mt;
                    KeyWords      := tblPhotoTable.fldKEY_WORDS.AsString;
                    CopyRight     := tblPhotoTable.fldCopyRightTable_CopyID.AsString;
                    PhotoDate     := tblPhotoTable.fldPhotoDate.AsString;
                    PhotoDateTime := tblPhotoTable.fldPhotoDateTime.AsDateTime;
                    IsPrivate     := tblPhotoTable.fldPrivate.AsBoolean;
                    Key           := tblPhotoTable.fldKey.AsInteger;
                  end;
                MediaList.AddObject(DirInfo.Name, ImageInfo);
              end;
          end;
        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { BuildMediaList }

  procedure GenerateSiblingPages(SiblingList: TStringList; WildName: string);
    var
      i: integer;

    function BuildString(aList: TStringList): string;
      var
        i: integer;
    begin { BuildString }
      for i := 0 to aList.Count-1 do
        if i > 0 then
          result := result + '; ' + aList[i]
        else
          result := aList[0];
    end;  { BuildString }

    procedure GeneratePage(PageNr: integer; PageFileName, WildName: string);
      type
        TEmbeddedSound = record
                          FileName: string;
                          SoundName: string;
                        end;
      var
        OutFile: TextFile;
        Lfn: string;
        ImageStart, ImageEnd, cn: integer;
        ImageInfo: TImageInfo;
        RowNr: integer;
        First, Last: boolean;
        PrevPage, NextPage: string;
        i, PhotoCount: integer;
        Temp: string;
        AccelChar: string;
        EmbedList: array of TEmbeddedSound;

      function Indent(Count: integer): string;
      begin
        result := PadR('', 4*count);
      end;

      procedure WriteCSSRef;
      begin { WriteCSSRef }
        if CommonPhotoSettings.UseLocalCSS then
          WriteLn(outfile, Indent(1), '<link rel="stylesheet" type="text/css" href="', CommonPhotoSettings.LocalCSSLfn, '" />')
        else
          WriteLn(outfile, Indent(1), '<link rel="stylesheet" type="text/css" href="', CommonPhotoSettings.RemoteCSSLfn, '" />');
      end;  { WriteCSSRef }

{$IfDef GenAudio}
      function GenSoundLink(Path, RelPath, JPEGName: string): string;
        var
          WAVFileName, WAVFilePathName, SoundName: string;
          len: integer;
      begin { GenSoundLink }
        WAVFileName     := ChangeFileExt(JPEGName, '.wav');
        if Path[Length(Path)] <> '\' then
          Path := Path + '\';
        WAVFilePathName := Path + WAVFileName;
        if FileExists(WAVFilePathName) then
          begin
            SoundName := PrintableOnly(ExtractFileBase(JPEGName));
            result    := Format('<a href=javascript:void('''') onMouseover="playSound(''%s'')"</a> (audio)',
                             [ExtractFileBase(SoundName)]);
            len := Length(EmbedList);
            SetLength(EmbedList, Len+1);
            EmbedList[len].FileName  := RelPath + WAVFileName;
            EmbedList[len].SoundName := SoundName;
          end
        else
          result := '';
      end;  { GenSoundLink }
{$EndIf}

      function BorderMargin(width: integer): string;
      begin
{$IfDef BorderMargin}
        result := Format(' border=%d', [width]);
{$else}
        result := '';
{$EndIf}
      end;

      procedure GenerateRow(StartIdx, EndIdx: integer);
      var
        i, PhotoCount: integer;
        ImageInfo: TImageInfo;

        procedure ForceForwardSlashes(var s: string);
          var
            i: integer;
        begin { ForceForwardSlashes }
          for i := 1 to length(s) do
            if s[i] = '\' then
              s[i] := '/';
        end;  { ForceForwardSlashes }

        procedure GenerateThumbNail(const FileName, FileExt: string; PhotoKey: integer);
        const
          DELIMS = ' \:.?"''';
        var
          ThumbNailName, ThumbNailPathAndName0, JPEGName, KeyWords,
          aWord, NewKeyWords, ErrorMsg: string;
          Count, i: integer;
        begin { GenerateThumbNail }
          ThumbNailPathAndName0 := ThumbNailPathAndName(Path + '\' + FileName);
          ThumbNailName         := ExtractFileName(ThumbNailPathAndName0);
          case MyCreateThumbNail(Path + '\' + FileName, ErrorMsg) of
            tps_CreatedUpdated:
              begin
                Inc(fThumbCount);
                if Assigned(fOnUpdateThumbStatus) then
                  fOnUpdateThumbStatus(Format('Thumbnails Created/Updated: %d', [fThumbCount]));
                if Assigned(fOnUpdateLogFile) then
                  fOnUpdateLogFile(Format('Created/Updated thumbnail: %s', [ThumbNailPathAndName0]));
              end;
            tps_Error:
              if Assigned(fOnUpdateLogFile) then
                fOnUpdateLogFile(Format('Unable to create/update thumbnail "%s" [%s]: ', [ThumbNailName, ErrorMsg]));
          end;

          if FileExists(ThumbNailPathAndName0) and (not ImageInfo.IsPrivate) then
            begin
              JpegName  := FileName;
              KeyWords  := ImageInfo.KeyWords;
              if Empty(KeyWords) then
                KeyWords := JPegName;
              Count := WordCountL(KeyWords, DELIMS);
              NewKeyWords := '';
              for i := 1 to Count do
                begin
                  aWord := ExtractWordL(I, KeyWords, DELIMS);

                  if NewKeyWords = '' then
                    NewKeyWords := aWord
                  else
                    NewKeyWords := NewKeyWords + ' ' + aWord;
                end;
              KeyWords := Trim(NewKeyWords);
              ForceForwardSlashes(ThumbNailName);
              Writeln(outfile, Indent(6),
                      Format('<td class="thumbnails" valign="top" width="%d">',
                             [THUMBNAILWIDTH+4]));
              WriteLn(OutFile, Indent(7), '<a href="http://ruthanddan.net/SinglePhoto.asp?Key=', PhotoKey, '"', '>');
              Writeln(OutFile, Indent(7),
                      '<img src="tn/', ThumbNailName, '"');
              Writeln(OutFile,
                    Indent(7),
                    ' alt="', KeyWords, '"></img></a><br/>');


{$IfDef CopyRightStuff}
              if ImageInfo.CopyRight <> DEF_COPY_ID then
                begin
                  WriteLn(OutFile,
                          Indent(7),
                          '<font color="blue">',
                          'Copyright (c) ', Year, ', ',
                          Trim(ImageInfo.CopyRightOwner),
                          '</font><br/>');
                end;
{$EndIf}

              Write(Outfile, Indent(7), KeyWords, ' ');
              if not Empty(ImageInfo.PhotoDate) then
                Write(Outfile, '[', ImageInfo.PhotoDate, ']');
              WriteLn(OutFile);

              Writeln(Outfile, Indent(6),
//                    '</A>',
{$IfDef GenAudio}
                      GenSoundLink(Path, '', JPEGName),
{$EndIf}
                      '</td>'
                      );
            end;
        end;  { GenerateThumbNail }

        procedure GenerateMediaReference(const Description: string);
          var
            MediaFileName, MediaPathName: string;
        begin { GenerateMediaReference }
          MediaFileName     := MediaList[i];    // TN/TN_1986-01.JPG
          MediaPathName     := Path + '\' + MediaFileName;
          if FileExists(MediaPathName) then
            begin
              Writeln(outfile, Indent(6),
                      Format('<td align="center" valign="bottom" width=%d>',
                             [THUMBNAILWIDTH+4]));
              Writeln(OutFile, Indent(7),
                      '<a href="', MediaFileName, '"', '>');
              Writeln(Outfile, Indent(7),
                      Description, '<br/>', ExtractFileBase(MediaFileName));
            end;
        end;  { GenerateMediaReference }

      begin { GenerateRow }
        PhotoCount := 0;
        for i := StartIdx to EndIdx do
          begin
            ImageInfo := TImageInfo(MediaList.Objects[i]);
            if ImageInfo.IncludeThisPhoto then
              Inc(PhotoCount);
          end;

        if (PhotoCount > 0) and (StartIdx <= EndIdx) then
          begin
            WriteLn(outfile, Indent(1), '<tr>');
            WriteLn(outfile, Indent(2), '<td align="center">');
            WriteLn(outfile, Indent(3), '<table', BorderMargin(3), '>');
            WriteLn(outfile, Indent(4), '<tr>');
            for i := StartIdx to EndIdx do
              begin
                ImageInfo := TImageInfo(MediaList.Objects[i]);
                if IsVideoMedia(ImageInfo.MediaType) then
                  begin
                    GenerateMediaReference(MediaInfoArray[ImageInfo.MediaType].Desc);
                    GenerateThumbNail(MediaList[i], GENERAL_MEDIA_FILENAME_EXT, ImageInfo.Key)
                  end else
                if IsPhotoMedia(ImageInfo.MediaType) then
                  GenerateThumbNail(MediaList[i], JPG_FILE_EXT, ImageInfo.Key);
              end;
            WriteLn(outfile, Indent(4), '</tr>');
            WriteLn(outfile, Indent(3), '</table>');
            WriteLn(outfile, Indent(2), '</td>');
            WriteLn(outfile, Indent(1), '</tr>');
          end;
      end;  { GenerateRow }

      procedure WriteChildLinks;
        const
          INDENT = '    ';
        var
          i: integer;
          AccelChar: string;

      begin { WriteChildLinks }
        if ChildList.Count > 0 then
          begin
            Writeln(outfile, INDENT, '<tr><td align="center"><br/><font face="Verdana, Arial, Helvetica, Sans-Serif"> &nbsp;&nbsp;</font>');

            for i := 0 to ChildList.Count-1 do
              if FilesInChildFolder(Path, ChildList[i], WildName) > 0 then
                begin
                  if i = 0 then  //
                    AccelChar := ' accesskey="D"'
                  else
                    AccelChar := '';

                  WriteLn(outfile, INDENT,
                    '  <a href="', ChildList[i], '/INDEX-01.htm"', AccelChar ,'>',
                    FixSpecialChars(ChildList[i]), '</a>&nbsp;&nbsp;&nbsp;');
                end;

            WriteLn(outfile, INDENT, '</td></tr>');
          end;
      end;  { WriteChildLinks }

      procedure WriteSiblingLinks;
        var
          i: integer;
          AccelChar: string;
      begin { WriteSiblingLinks }
        if SiblingList.Count > 1 then
          begin
            WriteLn(outfile, Indent(1), '<tr>');
            Writeln(outfile, Indent(2), '<td>');
            WriteLn(outfile, Indent(3), '<table', BorderMargin(1), ' align="center">');
            Writeln(outfile, Indent(4), '<tr>');
            WriteLn(outfile, Indent(5), '<td class="siblings"><br/>');
            WriteLn(Outfile, Indent(6), '<b>Page:</b>&nbsp;&nbsp;');

            for i := 0 to SiblingList.Count-1 do
              begin
                if i = 0 then  //
                  AccelChar := ' accesskey="F"' else
                if i = SiblingList.Count-1 then
                  AccelChar := ' accesskey="L"'
                else
                  AccelChar := '';

                if i <> PageNr then
                  WriteLn(outfile, Indent(6), '<a href="', SiblingList[i]+'.htm"', AccelChar, '>', i+1, '</a>&nbsp;')
                else
                  WriteLn(outfile, ' ':19, i+1, '&nbsp;');
              end;

            Writeln(outfile, Indent(5), '</td>');
            WriteLn(outfile, Indent(4), '</tr>');

            if (not First) or (not Last) then
              begin
                Writeln(outfile, Indent(4), '<tr>');
                WriteLn(outfile, Indent(5), '<td class="siblings">');

                if not First then
                  begin
                    PrevPage := SiblingList[PageNr-1]+'.htm';
                    WriteLn(outfile, Indent(6),
                                     '[ <a href="', PrevPage,'" accesskey="P">Prev</a> ]&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
                  end;

                if not Last then
                  begin
                    NextPage := SiblingList[PageNr+1]+'.htm';
                    WriteLn(outfile, Indent(6),
                                     '[ <a href="', NextPage,'" accesskey="N">Next</a> ]');
                  end;

                WriteLn(outfile, Indent(5), '</td>');
                WriteLn(outfile, Indent(4), '</tr>');
              end;

            WriteLn(outfile, Indent(3), '</table>');
            Writeln(outfile, Indent(2), '</td>');
            WriteLn(outfile, Indent(1), '</tr>');
          end;
      end;  { WriteSiblingLinks }

      function TreePath(Level: integer): string;
        var
          i: integer;
      begin { TreePath }
        result := '';
        for i := fTreeList.Count-1 downto Level+1 do
          if i > 0 then
            result := result + '../'
          else
            result := '../';
      end;  { TreePath }

{$IfDef GenAudio}
      procedure GenSoundScript;
      begin { GenSoundScript }
        WriteLn(outfile, '<script language=javascript type="text/javascript">');
        WriteLn(outfile, 'function playSound(SName) {');
        WriteLn(outfile, '  soundPlayer = document.embeds[SName]');
        WriteLn(outfile, '  if (navigator.appName == "Netscape") {');
        WriteLn(outfile, '    if (soundPlayer != null && soundPlayer.IsReady()) {');
        WriteLn(outfile, '      soundPlayer.play(false)');
        WriteLn(outfile, '    }');
        WriteLn(outfile, '  }');
        WriteLn(outfile, '  else {');
        WriteLn(outfile, '    if (soundPlayer != null) {');
        WriteLn(outfile, '      soundPlayer.Play()');
        WriteLn(outfile, '    }');
        WriteLn(outfile, '  }');
        WriteLn(outfile, '}');
        WriteLn(outfile, '</script>');
      end;  { GenSoundScript }
{$EndIf}

      procedure WriteHTMLLinks;
        var
          i: integer;
          temp: string;
      begin { WriteHTMLLinks }
        if HTMLList.Count > 0 then
          begin
            Writeln(outfile, INDENT(2), '<tr><td align="center"><br/><font face="Verdana, Arial, Helvetica, Sans-Serif"> &nbsp;&nbsp;</font>');

            for i := 0 to HTMLList.Count-1 do
              begin
                temp := ExtractFileBase(ExtractFileName(HTMLList[i]));
                WriteLn(outfile, INDENT(3),
                  '  <a href="', HTMLList[i], '">',
                  FixSpecialChars(temp), '</a>&nbsp;&nbsp;&nbsp;');
              end;

            WriteLn(outfile, INDENT(2), '</td></tr>');
          end;
      end;  { WriteHTMLLinks }

      procedure GenerateMetaKeywords;
      const
        DELIMS = ';,-';
      var
        i, j, ImageStart, ImageEnd, wc, cnt: integer;
        ImageInfo: TImageInfo;
        aWord, CleanedKeyWords: string;
        List: TStringList;
      begin { GenerateMetaKeywords }
        ImageStart := ImageNr;
        ImageEnd   := ImageNr + (ColCount * RowCount)-1;
        if ImageEnd > Pred(MediaList.Count) then
          ImageEnd := Pred(MediaList.Count);
        if ImageEnd >= ImageStart then
          begin
            List := TStringList.Create;
            try
              for i := ImageStart to ImageEnd do
                begin
                  ImageInfo := TImageInfo(MediaList.Objects[i]);
                  case ImageInfo.MediaType of
                    mt_Jpeg, mt_JPG:
                      with ImageInfo do
                        if not IsPrivate then
                          begin
                            CleanedKeyWords := StripNoiseWords(KeyWords, FileNameInfo);
                            wc := WordCountL(CleanedKeyWords, DELIMS);
                            for j := 1 to wc do
                              begin
                                aWord := Trim(ExtractWordL(j, CleanedKeyWords, DELIMS));
                                if List.IndexOf(aWord) = -1 then
                                  List.Add(aWord);
                              end;
                          end;
                  end;
                end;

              if List.Count > 0 then
                begin
                  write(outfile, Indent(1), '<meta name="keywords" content="');
                  cnt := 0;
                  for i := 0 to List.Count-1 do
                    if (not Empty(List[i])) and (not NoiseWord(List[i], FileNameInfo)) then
                      begin
                        if cnt > 0 then
                          write(outfile, ', ');
                        write(outfile, List[i]);
                        inc(cnt);
                      end;
                  WriteLn(outfile, '"/>');
                end;
            finally
              List.Free;
            end;
          end;
      end;  { GenerateMetaKeywords }

    begin { GeneratePage }
{$IfDef GenWithNoMedia}
      // put back in to force page generation even if no media at this level
      // (might exist at lower level)
      if MediaList.HtmlMediaCount = 0 then
        Exit;
{$EndIf}
      try
        SetLength(EmbedList, 0);
        First := PageNr = 0;
        Last  := PageNr >= SiblingList.Count-1;
        lfn   := Format('%s\%s.htm', [path, PageFileName]);
        AssignFile(OutFile, lfn);
        ReWrite(OutFile);
        try
          WriteLn(outfile, '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
          WriteLn(outfile, '<html lang="en-US" xml:lang="en-US" xmlns="http://www.w3.org/1999/xhtml">');
          WriteLn(outfile, '<head>' {'<LINK rel="stylesheet" type="text/css" href="http://RuthAndDan.net/MyStyle.css">'});
          GenerateMetaKeywords;
          WriteLn(outfile, Indent(1), '<meta http-equiv="Content-Type" content="text/html;charset=utf-8" />');
          WriteLn(outfile, Indent(1), '<meta name="description" content="', FolderName, '"/>');
          WriteLn(outfile, Indent(1), '<meta name="author" content="Dan Dorrough"/>');
          WriteLn(outfile, Indent(1), '<meta name="copyright" content="&copy; ', fYEAR, ' Dan and Ruth Dorrough"/>');
          WriteLn(outfile, Indent(1), '<meta name="viewport" content= "width = device-width"/>');
          WriteLn(outfile, Indent(1), '<title>', FolderName,'</title>');

          WriteCSSRef;

  {$IfDef GenAudio}
          GenSoundScript;
  {$EndIf}
          WriteLn(outfile, '</head>');

          WriteLn(outfile, '<body>');
          if CallerTitle <> '' then
            begin
              for i := 0 to fTreeList.Count-2 do
                begin
                  if i = fTreeList.Count-2 then  //
                    AccelChar := ' accesskey="U"'
                  else
                    AccelChar := '';

                  temp := Format('    <a class="tree" href="%sINDEX-01.htm"%s>%s</a>&nbsp;&nbsp;/&nbsp;&nbsp;',
                                 [Treepath(i), AccelChar, FixSpecialChars(fTreeList[i])]);
                  WriteLn(outfile, temp);
                end;
            end;
          WriteLn(outfile, '<table', BorderMargin(5), '>');
          WriteLn(outfile, '    <tr>');
          WriteLn(outfile, '      <td class="title">', Pretty(FolderName));
  {$IfDef GenAudio}
          WriteLn(outfile, GenSoundLink(ExtractFilePath(path), '../', FolderName));
  {$EndIf}
          WriteLn(outfile, '      </td>');
          WriteLn(outfile, '    </tr>');

          RowNr      := 0;

          if ImageNr < MediaList.Count then
            repeat
              // find the starting index
              i          := ImageNr;
              ImageStart := -1;
              repeat
                ImageInfo := TImageInfo(MediaList.Objects[i]);
                if ImageInfo.IncludeThisPhoto then
                  ImageStart := i;
                Inc(i);
              until (ImageStart >= 0) or (i >= MediaList.Count);
              if ImageStart < 0 then
                break;  // there are no photos

              ImageEnd   := ImageStart-1;
              cn         := 0;      // initialize count of columns used
              i          := ImageStart;

              // find the ending index
              repeat
                ImageInfo := TImageInfo(MediaList.Objects[i]);
                if ImageInfo.IncludeThisPhoto then
                  begin
                    Inc(ImageEnd);
                    Inc(cn);
                  end;
                Inc(i);
              until (cn >= ColCount) or (i >= MediaList.Count);

              if ImageEnd > Pred(MediaList.Count) then
                ImageEnd := Pred(MediaList.Count);
              PhotoCount := ImageEnd - ImageStart + 1;

              if PhotoCount > 0 then
                GenerateRow(ImageStart, ImageEnd);

              ImageNr := ImageEnd + 1;
              inc(RowNr);
            until (RowNr >= fRowCount) or (ImageNr >= MediaList.Count);

          WriteChildLinks;
          WriteSiblingLinks;
          WriteHTMLLinks;

          WriteLn(outfile, '</table>');

          WriteLn(outfile, '<br/><br/>');

          WriteLn(outfile, '<div class="footer">');
          WriteLn(outfile, 'Last updated: ', DateToStr(Date()), ' at ', TimeToStr(Time()), '<br/>');
          WriteLn(outfile, 'Click on photo for larger view');
          WriteLn(outfile, '<a href="http://RuthAndDan.net/">Home</a> &nbsp;');
          WriteLn(outfile, '<a href="http://RuthAndDan.net/SearchForm.asp">Search</a> &nbsp;', '<br/>');
(*
          WriteLn(outfile, 'Report errors to: <a href="mailto:dhdorrough@r-and-d-systems.com?subject=Error on RuthAndDan.net"> dhdorrough@r-and-d-systems.com</a><br/>');
          WriteLn(outfile, 'Family Web Site Developed by <a href="http://r-and-d-systems.com/">R&amp;D Systems</a>; Canandaigua, NY 14424<br/>');
*)
          WriteLn(outfile, CommonPhotoSettings.CopyRight);
          WriteLn(outfile, '</div>');
        
          WriteLn(outfile);

  {$IfDef GenAudio}
          for i := 0 to Length(EmbedList)-1 do
            WriteLn(outfile,
              Format('<EMBED SRC="%s" NAME="%s" HIDDEN=TRUE LOOP=FALSE AUTOSTART=FALSE MASTERSOUND>',
                     [EmbedList[i].FileName, EmbedList[i].SoundName]));
  {$EndIf}
          WriteLn(outfile, '</body>');
          WriteLn(outfile, '</html>');
        finally
          CloseFile(OutFile);
          SetLength(EmbedList, 0);
        end;
      except
        on e:Exception do
          UpdateStatusProc(Format('Unable to generate page %s (%s)', [Lfn, e.Message]));
      end;
    end;  { GeneratePage }

  begin { GenerateSiblingPages }
    UpdateStatusProc('Processing: ' + FolderName);

    ImageNr := 0;  // current global index into the array

    if SiblingList.Count > 0 then
      for i := 0 to SiblingList.Count-1 do
        GeneratePage(i, SiblingList[i], WildName)
    else
      GeneratePage(0, 'INDEX-01', WildName);

  end;  { GenerateSiblingPages }

  procedure DeleteOldHTMLFiles;
    var
      DOSError: integer;
      i: integer;
      OldHTMLFiles: TStringList;
      DirInfo: TSearchRec;
  begin { DeleteOldHTMLFiles }
    OldHTMLFiles := TStringList.Create;
    try
      OldHTMLFiles.Clear;
      // List the *.htm files in this folder
      DosError := FindFirst(path + '\INDEX*.htm', FATTR, DirInfo);
      while DosError = 0 do
        begin
          OldHTMLFiles.Add(DirInfo.Name);
          DosError := FindNext(DirInfo);
        end;
      FindClose(DirInfo);
      for i := OldHTMLFiles.Count-1 downto 0 do
        MyDeleteFile(path + '\' + OldHTMLFiles[i], true, false);
    finally
      OldHTMLFiles.Free;
    end;
  end;  { DeleteOldHTMLFiles }

  procedure xUpdateDataBase( const Path: string;
                            ImageIndex: integer;
                            FileNameInfo: TFileNameInfo);
  var
    OkToCheck: boolean;
    SoundPathName: string;
    KeyWords, Msg: string;
    UpdateInfo: TUpdateInfo;
    mt: TMediaType;
    ii: TImageInfo;
    LocationInfo: TLocationInfo;
  begin { UpdateDataBase }
    FillChar(UpdateInfo, SizeOf(UpDateInfo), 0);
    UpdateInfo.ImagePathName := Path + '\' + MediaList[ImageIndex];
    if UpdateRecentOnly then
      OkToCheck := (FileDateToDateTime(FileAge(UpdateInfo.ImagePathName)) >= fOnOrAfter)
    else
      OkToCheck := true;

    if OkToCheck then
      begin
        UpdateInfo.FileName     := ExtractFileName(UpdateInfo.ImagePathName);
        UpdateInfo.FilePath     := ExtractFilePath(UpdateInfo.ImagePathName);
        UpdateInfo.FileNameInfo := FileNameInfo;
//      if not Empty(UpdateInfo.FileNameInfo.DefaultArea) then
        if not Empty(LocationInfo.DefaultLocation) then
          LocationInfo.DefaultLocationKind := dl_SpecifiedValue
        else
          LocationInfo.DefaultLocationKind := CommonPhotoSettings.DefaultLocationKind;

        if UpdateInfo.FilePath[Length(UpdateInfo.FilePath)] = '\' then
          Delete(UpdateInfo.FilePath, Length(UpdateInfo.FilePath), 1);

        UpdateInfo.RecordExists := tblPhotoTable.MyLocateRecord(UpdateInfo.FileName, UpdateInfo.FilePath);
        if UpdateInfo.RecordExists then
          begin
            KeyWords            := tblPhotoTable.fldKEY_WORDS.AsString;
            UpdateInfo.PhotoDateYYYYMMDD     := tblPhotoTable.fldPhotoDate.AsString;
            UpdateInfo.PhotoDateTime := tblPhotoTable.fldPhotoDateTime.AsDateTime;
          end
        else if CommonPhotoSettings.UpdateDB then
          begin
            mt  := MediaTypeFromExtension(ExtractFileExt(UpdateInfo.ImagePathName));
            if IsPhotoMedia(mt) then
              begin
                SoundPathName           := ForceExtensionL(UpdateInfo.ImagePathName, 'WAV');
                UpdateInfo.HasSoundFile := FileExists(SoundPathName);
              end;
            if not GetPhotoInfo(UpdateInfo) then
              begin
                Msg := Format('Unable to GetPhotoInfo for file: %s', [UpdateInfo.ImagePathName]);
                Alert(Msg);
                if Assigned(fOnUpdateLogFile) then
                  fOnUpdateLogFile(Msg);
              end;
//          UpdateInfo.FileNameInfo.MinutesToAdd   := 0;
            UpdateInfo.MinutesToAdd   := 0;
            UpdateInfo.UpdateIt       := true;

            try
              tblPhotoTable.UpdateProc(UpdateInfo, LocationInfo, KeyWords, nil);
            except
              on e:Exception do
                raise Exception.CreateFmt('Error while updating table "%s" [%s]',
                                          [UpdateInfo.FileName, e.Message]);
            end;
            inc(fDBRecsUpdated);
            if Assigned(fOnUpdateLogFile) then
              fOnUpdateStatus(Format('%d recs added', [fDBRecsUpdated]));
{$IfDef debugging}
            if Assigned(fOnUpdateLogFile) then
              fOnUpdateLogFile('Record NOT Added (debugging): ' + UpdateInfo.FilePath + '\' + UpdateInfo.FileName);
{$Else}
            if Assigned(fOnUpdateLogFile) then
              fOnUpdateLogFile('Record Added: ' + UpdateInfo.FilePath + '\' + UpdateInfo.FileName);
{$EndIf}
          end
        else
          if Assigned(fOnUpdateStatus) then
            fOnUpdateStatus('gUpdateDb is False');
(**)  // This code had been deleted and might not work when called from uPhotoDB
        ii               := TImageInfo(MediaList.Objects[ImageIndex]);
        ii.KeyWords      := KeyWords;
        ii.CopyRight     := tblPhotoTable.fldCopyRightTable_CopyID.AsString;
        ii.PhotoDate     := UpdateInfo.PhotoDateYYYYMMDD;
        ii.PhotoDateTime := UpdateInfo.PhotoDateTime;
//      if not SameText(ii.CopyRight, DEF_COPY_ID) then
//        ii.CopyRightOwner := tblPhotoTable.CopyRightOwner;
        ii.IsPrivate     := tblPhotoTable.fldPrivate.AsBoolean;
        ii.Key           := tblPhotoTable.fldKey.AsInteger;
(**)
      end;
  end;  { UpdateDataBase }

  procedure UpdateFolderList(const Path: string;
                            FileNameInfo: TFileNameInfo);
  var
    ImagePathName: string;
    KeyWords: string;
  begin { UpdateFolderList }
    ImagePathName := Path;
    with fTempFilePathsTable do
      begin
        FindFolderNo( ImagePathName,
                      KeyWords,
                      true);   // OkToAdd
      end;
  end;  { UpdateFolderList }

begin { TForm_HTMLMaker.ProcessFolder }
  UpdateStatusProc(Format('Processing folder %d: Path: %s', [fFoldersProcessed, path]));
  inc(fLevel);
  inc(fFoldersProcessed);
  try
    ChildList        := TStringList.Create;
    ChildList.Sorted := true;
    SiblingList      := TStringList.Create;
    MediaList        := TMediaList.Create;
    HTMLList         := TStringList.Create;
    fTreeList.Add(FolderName);
    try
      if DeleteOldHTML then
        DeleteOldHTMLFiles;
      GetListOfChildFolders(ChildList);
      GetListOfHTMLFiles(HTMLList);
      BuildMediaList(MediaList, WildName);
      if fIncludeEmpty then
        UpdateFolderList(Path, FileNameInfo);  // Make sure that this folder exists in the database
      BuildSiblingList(SiblingList);
      GenerateSiblingPages(SiblingList, WildName);
      if fProcessSubFolders then  // was:     if cbProcessSubFolders.Checked then
        for i := 0 to ChildList.Count - 1 do
          if FilesInChildFolder(Path, ChildList[i], WildName) > 0 then
            ProcessFolder(FolderName, ChildList[i], path + '\' + ChildList[i], WildName) else
          if fIncludeEmpty then  // was:         if cbIncludeEmpty.Checked then
            ProcessFolder(FolderName, ChildList[i], path + '\' + ChildList[i], WildName);
      UpdateStatusProc(Format('COMPLETE. %d folders processed', [fFoldersProcessed]));


    finally
      ChildList.Free;
      SiblingList.Free;
      MediaList.Free;
      HTMLList.Free;
      fTreeList.Delete(fTreeList.Count-1);
      inc(fFolderCount);
      if Assigned(fAfterFolderProcessed) then
        fAfterFolderProcessed(Path);
    end;
  finally
    Dec(fLevel);
  end;
end;  { TForm_HTMLMaker.ProcessFolder }

// count the number of items that can be rendered
function TMediaList.HtmlMediaCount: integer;
var
  i: integer;
  ImageInfo: TImageInfo;
begin
  result := 0;
  for i := 0 to Pred(Count) do
    begin
      ImageInfo := TImageInfo(Objects[i]);
      if ImageInfo.IncludeThisPhoto then
        Inc(result);
    end;
end;

destructor THTMLGenerator.Destroy;
begin
  tblPhotoTable.Active   := false;
  tblPhotoTable.Filtered := false;
  FreeandNil(tblPhotoTable);
  FreeAndNil(fTempFilePathsTable);

  if fTreeList.Count > 0 then
    fTreeList.Delete(fTreeList.Count-1);
  fTreeList.Free;

  inherited;
end;

procedure THTMLGenerator.SetIncludeEmpty(const Value: boolean);
begin
  if fIncludeEmpty <> Value then
    begin
      fIncludeEmpty := Value;
      if fIncludeEmpty and (not Assigned(fTempFilePathsTable)) then
        begin
          fTempFilePathsTable := TFilePathsTable.Create( self,
                                                         CommonPhotoSettings.PhotoDBDatabaseFileName,
                                                         cFILEPATHS,
                                                         []);
          fTempFilePathsTable.Active := true;
        end;
    end;
end;

procedure THTMLGenerator.SetProcessSubFolders(const Value: boolean);
begin
  fProcessSubFolders := Value;
  fLevel := 0; fFoldersProcessed := 0;
end;

procedure THTMLGenerator.SetStartTime(const Value: double);
begin
  fStartTime := Value;
end;

{ TImageInfo }

function TImageInfo.IncludeThisPhoto: boolean;
begin
  result := (MediaType in HTML_MEDIA) and (not IsPrivate);
end;

procedure THTMLGenerator.ListBox1ClickCheck(Sender: TObject);
var
  i, NrChecked: integer;
begin
  NrChecked := 0;
  for i := 0 to ListBox1.Count-1 do
    if ListBox1.Checked[i] then
      Inc(NrChecked);
  lblStatus.Caption := Format('At least %d folders to process', [NrChecked]);
end;

procedure THTMLGenerator.FormShow(Sender: TObject);
begin
  ListBox1ClickCheck(nil);
end;

end.
