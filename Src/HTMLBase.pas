unit HTMLBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, PDB_Decl, PDBTables, Protocol_Decl,
  PhotoDBCommonSettings;

const
  HTML_MEDIA   = [mt_Jpeg, mt_JPG, mt_RM, mt_RV, mt_RAD
                  {, mt_WMV, mt_MOV, mt_MPG, mt_MP4, mt_M2TS}  // 12/14/2016 - removed these because they shouldn't be added to generated HTML?
                  ];

  FATTR = faAnyFile-faDirectory-faHidden-faSysFile-faVolumeID;
  FATTR2 = faAnyFile-faHidden-faSysFile-faVolumeID;

  CREATETHUMBS = TRUE;

type
  TFileAcceptor    = function {AcceptThisFile}(const FileName: string): boolean of object;
  TStatusUpdate    = procedure {Update_Status}(const Msg: string; LineNo: integer = 0) of object;

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

  TfrmHTMLBase = class(TForm)
    cbProcessSubFolders: TCheckBox;
    cbDeleteOldHTML: TCheckBox;
    edtLocalCSS: TEdit;
    edtRemoteCSS: TEdit;
    lblStatus: TLabel;
    btnBegin: TButton;
    btnCancel: TButton;
    rgProtocol: TRadioGroup;
    ProgressBar1: TProgressBar;
    lblProgressInfo: TLabel;
    rgFileReference: TRadioGroup;
    Label1: TLabel;
    leRowCount: TLabeledEdit;
    leColCount: TLabeledEdit;
//  procedure cbUseLocalCSSClick(Sender: TObject);
    procedure btnBeginClick(Sender: TObject);
    procedure rgFileReferenceClick(Sender: TObject);
    procedure leRowCountExit(Sender: TObject);
    procedure leColCountExit(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
//  procedure cbUseRemoteCSSClick(Sender: TObject);
  private
    fIncludeEmpty: boolean;
    fLevel: integer;
    fProcessSubFolders: boolean;
    fSavedCursor: TCursor;
    fTempFilePathsTable: TFilePathsTable;
    fTreeList: TStringList;
    fYear: integer;
    FileNameInfo: TFileNameInfo;
    fFolderCount: integer;
    fDeleteOldHTML: boolean;
    fOnOrAfter: TDateTime;
    fColCount: word;
    fRowCount: word;

    procedure SetIncludeEmpty(const Value: boolean);
    procedure SetLocalCSSLfn(const Value: string);
    procedure SetRemoteCSSLFN(const Value: string);
    procedure SetUseLocalCSS(const Value: boolean);
    function GetProtocol: TProtocol;
    procedure SetProtocol(const Value: TProtocol);
    procedure ShowEnabled(b: boolean; edt: TEdit);
    procedure ShowEnabledColors;
    procedure UpdateProgressBar(FoldersProcessed: integer);
    function FilesInChildFolder(Path, ChildName, WildName: string): integer;
    function IsMediaFile(const FileName: string;
      var MediaType: TMediaType): boolean;
    function IsInDateRange(const FileName: string): boolean;
    function GetFileLocations: TCSSLocale;
    procedure SetFileLocations(const Value: TCSSLocale);
    function GetUseLocalCSS: boolean;
    function GetUseRemoteCSS: boolean;
    procedure SetUseRemoteCSS(const Value: boolean);
    function GetRemoteCSSLFN: string;
    function GetLocalCSSLfn: string;
    procedure SetRowCount(const Value: word);
    procedure SetColCount(const Value: word);
  protected
    fLogFile: TextFile;
    fLogFileName: string;
    StartTime: TDateTime;
    fErrorCount: integer;
    procedure AfterFolderProcessed(FolderName: string); virtual; {abstract;}
    procedure EnableButtons; virtual; abstract;
    procedure FinalizeProcess; virtual;
    procedure GenerateHTMLforSelectedFolders; virtual; abstract;
    Procedure Log(const Message: string; LineNo: integer = 0); virtual; abstract;
  public
    AbortIt: boolean;
    TempPhotoTable2: TPhotoTable;
    TotalFoldersToProcess: integer;

    function AcceptThisFile(const FileName: string): boolean;
    function CountFoldersToBeProcessed: integer; virtual; abstract;
//  procedure GenerateIndexPage(const FolderName, path: string);
    procedure GetListOfChildFolders(const Path: string; ChildNames: TStringList);
    procedure InitializeProcess; virtual;
    procedure InitParams;
    procedure ProcessFolder(CallerTitle, FolderName, path, WildName: string);
    function ProtoColName: string;
    Constructor Create(Aowner: TComponent); override;
    Destructor Destroy; override;
    procedure UpdateProgressInfo(const ACaption: string; FoldersProcessed: integer);
    procedure UpdateStatus(const Msg: string);
    property IncludeEmpty: boolean
             read fIncludeEmpty
             write SetIncludeEmpty;
    property ColCount: word
             read fColCount
             write SetColCount;
    property DeleteOldHTML: boolean
             read fDeleteOldHTML
             write fDeleteOldHTML;
    property ErrorCount: integer
             read fErrorCount
             write fErrorCount;
    property FileLocations: TCSSLocale
             read GetFileLocations
             write SetFileLocations;
    property OnOrAfter: TDateTime
             read fOnOrAfter
             write fOnOrAfter;
    property ProcessSubFolders: boolean
             read fProcessSubFolders
             write fProcessSubFolders;
    property ProtoCol: TProtocol
             read GetProtoCol
             write SetProtoCol;
    property RowCount: word
             read fRowCount
             write SetRowCount;
    property FolderCount: integer
             read fFolderCount
             write fFolderCount;
    property UseLocalCSS: boolean
             read GetUseLocalCSS
             write SetUseLocalCSS;
    property UseRemoteCSS: boolean
             read GetUseRemoteCSS
             write SetUseRemoteCSS;
    property LocalCSSLfn: string
             read GetLocalCSSLfn
             write SetLocalCSSLfn;
    property RemoteCSSLFN: string
             read GetRemoteCSSLFN
             write SetRemoteCSSLFN;
  end;

  TMediaList = class(TStringList)
  public
    Destructor Destroy; override;
    function HtmlMediaCount: integer;
  end;

(*
var
  frmHTMLBase: TfrmHTMLBase;
*)
implementation

{$R *.dfm}

uses
  DateUtils, Math, MyUtils, PDBUtils, StStrL,
  PhotoUtils;

constructor TfrmHTMLBase.Create(Aowner: TComponent);
var
  p: TProtocol;
begin
  inherited Create(AOwner);
  fYear          := YearOf(Now);

  fTreeList      := TStringList.Create;
  cbProcessSubFolders.Checked := true;    // force sub-folders to be processed
  cbDeleteOldHTML.Checked     := true;

(*
  FileLocations               := CommonPhotoSettings.CSSLocale;
  edtLocalCSS.Text            := CommonPhotoSettings.LocalCssLfn;
  edtRemoteCSS.Text           := CommonPhotoSettings.RemoteCssLfn;
*)
  ShowEnabledColors;

  with rgProtocol do
    begin
      Items.Clear;
      for p := succ(Low(TProtocol)) to high(TProtocol) do
        Items.AddObject(ProtoColInfoArray[p].Value, TObject(p));
    end;
  ProtoCol := pHTTP; // DEFAULT_PROTOCOL
end;

procedure TfrmHTMLBase.ShowEnabled(b: boolean; edt: TEdit);
begin
  if b then
    edt.Color := clWindow
  else
    edt.Color := cl3DLight;
end;

procedure TfrmHTMLBase.ShowEnabledColors;
begin
  ShowEnabled(UseLocalCSS,  edtLocalCSS);
  ShowEnabled(UseRemoteCSS, edtRemoteCSS);
  Application.ProcessMessages;
end;


destructor TfrmHTMLBase.Destroy;
begin
  if fTreeList.Count > 0 then
    fTreeList.Delete(fTreeList.Count-1);
  fTreeList.Free;
  FreeAndNil(fTempFilePathsTable);

  FreeandNil(TempPhotoTable2);

  inherited;
end;

  function TfrmHTMLBase.IsMediaFile(const FileName: string; var MediaType: TMediaType): boolean;
    var
      Ext: string;
  begin { IsMediaFile }
    Ext       := ExtractFileExt(FileName);
    MediaType := MediaTypeFromExtension(Ext);
    result    := MediaType <> mt_Unk;
  end;  { IsMediaFile }

  procedure TfrmHTMLBase.GetListOfChildFolders(const Path: string; ChildNames: TStringList);
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

  function TfrmHTMLBase.IsInDateRange(const FileName: string): boolean;
//var
//  FileDateTimeStamp: integer;
//  FileDateTime: TDateTime;
  begin { IsInDateRange }
(*
    if UpdateRecentOnly then
      begin
        FileDateTimeStamp := FileAge(FileName);
        if FileDateTimeStamp >= 0 then
          begin
            FileDateTime      := FileDateToDateTime(FileDateTimeStamp);
            result            := FileDateTime >= OnOrAfter;
          end
        else
          result := false;
      end
    else
*)
      result := true;
  end;  { IsInDateRange }

  // Count known media types or sub-folders only. Basically determine if the specified folder
  // should be processed or not. Probably should be done recursively to be really correct.
  function TfrmHTMLBase.FilesInChildFolder(Path, ChildName, WildName: string): integer;
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
        if (IsMediaFile(Lfn, mt) and IsInDateRange(FullFileName) and (AcceptThisFile(FullFileName))) or IsADirectory then
          inc(result);

        DosError := FindNext(DirInfo);
      end;
    FindClose(DirInfo);
  end;  { FilesInChildFolder }

procedure TfrmHTMLBase.ProcessFolder(CallerTitle, FolderName, path, WildName: string);
var
  ChildList: TStringList;
  HTMLList: TStringList;
  SiblingList: TStringList;
  MediaList: TMediaList;
  i: integer;
  ImageNr: integer;

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
    Pages_Needed := Ceil(PhotoCount / (RowCount * ColCount));
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
    DosError := FindFirst(FilePath, FATTR, DirInfo);
    while DosError = 0 do
      begin
        FullFileName := Path + '\' + DirInfo.Name;
        if IsMediaFile(DirInfo.Name, mt) and IsInDateRange(FullFileName) and AcceptThisFile(FullFileName) then
          begin
            RecordExists := TempPhotoTable2.MyLocateRecord(DirInfo.Name, Path);
            if RecordExists then
              begin
                ImageInfo := TImageInfo.Create;
                with ImageInfo do
                  begin
                    ImageInfo.MediaType     := mt;
                    with TempPhotoTable2 do // Fill in the  ImageInfo
                      begin
                        KeyWords      := fldKEY_WORDS.AsString;
                        CopyRight     := fldCopyRightTable_CopyID.AsString;
                        PhotoDate     := fldPhotoDate.AsString;
                        PhotoDateTime := fldPhotoDateTime.AsDateTime;
                        IsPrivate     := fldPrivate.AsBoolean;
                        Key           := fldKey.AsInteger;
                      end;
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
        if UseLocalCSS then
          WriteLn(outfile, Indent(1), '<link rel="stylesheet" type="text/css" href="file:///', ForceForwardSlashes(LocalCSSLfn), '" />')
        else
          WriteLn(outfile, Indent(1), '<link rel="stylesheet" type="text/css" href="', RemoteCSSLfn, '" />');
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
//              Inc(fThumbCount);
//              if Assigned(OnUpdateThumbStatus) then
//                OnUpdateThumbStatus(Format('Thumbnails Created/Updated: %d', [fThumbCount]));
//              if Assigned(OnUpdateLogFile) then
//                OnUpdateLogFile(Format('Created/Updated thumbnail: %s', [ThumbNailPathAndName0]));
                Log(Format('Created/Updated thumbnail: %s', [ThumbNailPathAndName0]));  
              end;
            tps_Error:
//            if Assigned(OnUpdateLogFile) then
                Log(Format('Unable to create/update thumbnail "%s" [%s]: ', [ThumbNailName, ErrorMsg]));
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
              if UseRemoteCSS then
                WriteLn(OutFile, Indent(7), '<a href="', ProtocolName, '://ruthanddan.net/SinglePhoto.asp?Key=', PhotoKey, '"', '>') else
              if UseLocalCSS then
                WriteLn(OutFile, Indent(7), '<a href="file:///', FileName, '"', '>');

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
                    if CREATETHUMBS then
                      GenerateThumbNail(MediaList[i], GENERAL_MEDIA_FILENAME_EXT, ImageInfo.Key)
                  end
                else
                if IsPhotoMedia(ImageInfo.MediaType) then
                  if CREATETHUMBS then
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
          WriteLn(outfile, '<html lang="en-US" xml:lang="en-US" xmlns=', ProtoColName,
                            '"http://www.w3.org/1999/xhtml">');
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
            until (RowNr >= RowCount) or (ImageNr >= MediaList.Count);

          WriteChildLinks;
          WriteSiblingLinks;
          WriteHTMLLinks;

          WriteLn(outfile, '</table>');

          WriteLn(outfile, '<br/><br/>');

          WriteLn(outfile, '<div class="footer">');
          WriteLn(outfile, 'Last updated: ', DateToStr(Date()), ' at ', TimeToStr(Time()), '<br/>');
          WriteLn(outfile, 'Click on photo for larger view');
          if UseRemoteCSS then
            begin
              WriteLn(outfile, '<a href="', ProtocolName, '://RuthAndDan.net/">Home</a> &nbsp;');
              WriteLn(outfile, '<a href="', ProtocolName, '://RuthAndDan.net/SearchForm.asp">Search</a> &nbsp;', '<br/>');
            end else
          if UseLocalCSS then
            begin
              WriteLn(outfile, '<a href="file:///', CommonPhotoSettings.DefaultLocalWebPagePath, '">Home</a> &nbsp;');
            end;
{
          WriteLn(outfile, 'Report errors to: <a href="mailto:dhdorrough@r-and-d-systems.com?subject=Error on RuthAndDan.net"> dhdorrough@r-and-d-systems.com</a><br/>');
          WriteLn(outfile, 'Family Web Site Developed by <a href="http://r-and-d-systems.com/">R&amp;D Systems</a>; Canandaigua, NY 14424<br/>');
}
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
          begin
            ErrorCount := ErrorCount + 1;
            UpdateStatus(Format('Unable to generate page %s (%s)', [Lfn, e.Message]));
          end;
      end;
    end;  { GeneratePage }

  begin { GenerateSiblingPages }
    UpdateStatus('Processing: ' + FolderName);

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

begin { THTMLBase.ProcessFolder }
  if AbortIt then
    begin
      UpdateStatus('Canceling process');
      Exit
    end;

  UpdateStatus(Format('%d: Path: %s', [fFolderCount, path]));
  inc(fLevel);
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
      GetListOfChildFolders(Path, ChildList);
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
      UpdateStatus(Format('COMPLETE. %d folders processed', [fFolderCount]));

    finally
      FreeAndNil(ChildList);
      FreeAndNil(SiblingList);
      FreeAndNil(MediaList);
      FreeAndNil(HTMLList);
      fTreeList.Delete(fTreeList.Count-1);
      inc(fFolderCount);
      UpdateProgressInfo('Processing', fFolderCount);
      AfterFolderProcessed(Path);
    end;
  finally
    Dec(fLevel);
  end;
end;  { TfrmHTMLBase.ProcessFolder }


function TfrmHTMLBase.ProtocolName: string;
begin
  result := ProtocolInfoArray[Protocol].Value;
end;

procedure TfrmHTMLBase.SetIncludeEmpty(const Value: boolean);
begin
  Assert(false, 'SetIncludeEmpty');
end;

procedure TfrmHTMLBase.SetLocalCSSLfn(const Value: string);
begin
  edtLocalCSS.Text := Value;
end;

procedure TfrmHTMLBase.SetRemoteCSSLFN(const Value: string);
begin
  edtRemoteCSS.Text := Value;
end;

procedure TfrmHTMLBase.UpdateStatus(const Msg: string);
begin
  lblStatus.Caption := Msg;
  Application.ProcessMessages;
end;

function TfrmHTMLBase.GetProtocol: TProtocol;
begin
  result := pUnknown;
  with rgProtocol do
    if ItemIndex >= 0 then
      result := TProtocol(Items.Objects[ItemIndex]);
end;

procedure TfrmHTMLBase.SetProtocol(const Value: TProtocol);
begin
  with rgProtocol do
   begin
     ItemIndex := Items.IndexOfObject(TObject(Value))
   end;
end;

{ TMediaList }

destructor TMediaList.Destroy;
  var
    i: integer;
begin
  for i := 0 to Count-1 do
    Objects[i].Free;

  inherited;
end;

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

{ TImageInfo }

function TImageInfo.IncludeThisPhoto: boolean;
begin
  result := (MediaType in HTML_MEDIA) and (not IsPrivate);
end;

procedure TfrmHTMLBase.InitParams;
begin
  RowCount          := CommonPhotoSettings.RowCount;        // should come from CommonSettings
  ColCount          := CommonPhotoSettings.ColCount;        // should come from CommonSettings
  FileLocations     := CommonPhotoSettings.CSSLocale;
  edtLocalCSS.Text  := CommonPhotoSettings.LocalCssLfn;
  edtRemoteCSS.Text := CommonPhotoSettings.RemoteCssLfn;
//UpdateRecentOnly  := false;    // should come from CommonSettings
  ProcessSubFolders := cbProcessSubFolders.Checked;    // force sub-folders to be processed
  DeleteOldHTML     := cbDeleteOldHTML.Checked;
  StartTime         := DWORD(GetCurrentTime);
  FolderCount       := 0;
  AbortIt           := false;
end;


procedure TfrmHTMLBase.btnBeginClick(Sender: TObject);
begin
  FolderCount       := 0;

  // Remember the default settings
  CommonPhotoSettings.CSSLocale    := FileLocations;
  CommonPhotoSettings.LocalCssLfn  := LocalCSSLfn;
  CommonPhotoSettings.RemoteCssLfn := RemoteCSSLFN;
  CommonPhotoSettings.RowCount     := RowCount;
  CommonPhotoSettings.ColCount     := ColCount;
end;

procedure TfrmHTMLBase.AfterFolderProcessed(FolderName: string);
begin
end;

function TfrmHTMLBase.GetUseLocalCSS: boolean;
begin
  result := rgFileReference.ItemIndex = 0;
end;

function TfrmHTMLBase.GetUseRemoteCSS: boolean;
begin
  result := rgFileReference.ItemIndex = 1;
end;

procedure TfrmHTMLBase.SetUseRemoteCSS(const Value: boolean);
begin
  if Value then
    rgFileReference.ItemIndex := 1;
end;

procedure TfrmHTMLBase.SetUseLocalCSS(const Value: boolean);
begin
  if Value then
    rgFileReference.ItemIndex := 0;
end;



procedure TfrmHTMLBase.InitializeProcess;
begin
  fSavedCursor  := Screen.Cursor;
  Screen.Cursor := crSQLWait;
  ProgressBar1.Min      := 0;
  ProgressBar1.Max      := 100;
  ProgressBar1.Position := 0;
end;

procedure TfrmHTMLBase.FinalizeProcess;
begin
  Screen.Cursor := fSavedCursor;
  ProgressBar1.Position := 100;
  MessageFmt('Processed %d folders, %d errors occurred', [FolderCount, ErrorCount]);
  if AbortIt then
    Message('Process cancelled by operator'); 
end;


function TfrmHTMLBase.AcceptThisFile(const FileName: string): boolean;
begin
  result := IsPhotoMedia(MediaTypeFromExtension(ExtractFileExt(FileName)));
end;

(*
procedure TfrmHTMLBase.SetTotalFoldersToProcess(const Value: integer);
begin
  fTotalFoldersToProcess := Value;
end;
*)

procedure TfrmHTMLBase.UpdateProgressBar(FoldersProcessed: integer);
begin
  if TotalFoldersToProcess <> 0 then
    ProgressBar1.Position := Round((FolderCount {was:FoldersProcessed} / TotalFoldersToProcess) * 100.0);
end;

procedure TfrmHTMLBase.UpdateProgressInfo(const ACaption: string; FoldersProcessed: integer);
begin
  if TotalFoldersToProcess <> 0 then
    lblProgressInfo.Caption := Format('%d/%d Folders %s (%0.1n %%)',
                                      [FoldersProcessed,
                                       TotalFoldersToProcess,
                                       ACaption,
                                       (FoldersProcessed / TotalFoldersToProcess) * 100.0])
  else
    lblProgressInfo.Caption := Format('%d Folders %s', [FolderCount {was:FoldersProcessed}, ACaption]);
  UpdateProgressBar(FolderCount{FoldersProcessed});
  Application.ProcessMessages;
end;


function TfrmHTMLBase.GetFileLocations: TCSSLocale;
begin
  case rgFileReference.ItemIndex of
    0: result := cl_Local;
    1: result := cl_Remote;
    else
       result := cl_Unknown;
  end;
end;

procedure TfrmHTMLBase.SetFileLocations(const Value: TCSSLocale);
begin
  case Value of
    cl_Local:  rgFileReference.ItemIndex := 0;
    cl_Remote: rgFileReference.ItemIndex := 1;
  end;
end;

procedure TfrmHTMLBase.rgFileReferenceClick(Sender: TObject);
begin
  edtLocalCSS.Enabled    := UseLocalCSS;
  edtRemoteCSS.Enabled   := not UseLocalCSS;
  ShowEnabledColors;
end;

function TfrmHTMLBase.GetRemoteCSSLFN: string;
begin
  result := edtRemoteCSS.Text;
end;

function TfrmHTMLBase.GetLocalCSSLfn: string;
begin
  result := edtLocalCSS.Text;
end;

procedure TfrmHTMLBase.SetRowCount(const Value: word);
begin
  fRowCount := Value;
  leRowCount.Text := IntToStr(Value);
end;

procedure TfrmHTMLBase.SetColCount(const Value: word);
begin
  fColCount := Value;
  leColCount.Text := IntToStr(Value);
end;

procedure TfrmHTMLBase.leRowCountExit(Sender: TObject);
begin
  RowCount := StrToInt(leRowCount.Text);
end;

procedure TfrmHTMLBase.leColCountExit(Sender: TObject);
begin
  ColCount := StrToInt(leColCount.Text);
end;

procedure TfrmHTMLBase.btnCancelClick(Sender: TObject);
begin
  AbortIt := true;
end;

end.

