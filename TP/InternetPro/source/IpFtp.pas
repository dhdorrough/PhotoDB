{******************************************************************}
{*             IPFTP.PAS - FTP classes and components             *}
{******************************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

unit IpFtp;

interface

uses
  WinTypes,
  WinProcs,
  Classes,
  Forms,
  Messages,
  Controls,
  ComCtrls,
  ExtCtrls,
  ShellApi,
  SysUtils,
  StdCtrls,
{$IFDEF VERSION4}
  ImgList,
{$ENDIF}
  Graphics,
  IpSock,
  IpConst,
  IpUtils;


const {miscellaneous constants}
  MaxBuffer = 32768;
  MaxCmdStack = 128;

type {Ftp client enumerated types}
  TIpFtpRetrieveMode  = (rmAppend, rmReplace, rmRestart);
  TIpFtpStoreMode     = (smAppend, smReplace, smUnique, smRestart);
  TIpFtpFileType      = (ftAscii, ftBinary);
  TIpFtpProcessState  = (psClosed, psLogin, psIdle, psDir, psGet, psPut, psRen, psCmd{ , psNone });
  TIpFtpStatusCode    = ( fscClose, fscOpen, fscLogin, fscLogout,
                          fscComplete, fscCurrentDir, fscSystem, fscDirList, fscInfo,
                          fscProgress, fscTransferOK, fscTimeout);
  TIpFtpLogCode       = (lcClose, lcOpen, lcLogin, lcLogout, lcDelete,
                       lcRename, lcReceive, lcStore, lcComplete,
                       lcRestart, lcTimeout, lcUserAbort);

type {Ftp directory tree enumerated types}
  TIpFtpDirFileType  = (ftFile, ftDir, ftLink, ftUnknown);
  TIpFtpDirStyle     = (dsNone, dsUnix, dsDos, dsMultiNet, dsVms, dsUser);
  TIpFtpDirFileAttr  = (faRead, faWrite, faExecute);
  TIpFtpDirFileAttrs = set of TIpFtpDirFileAttr;
  TIpFtpDirFilePermissions = record
    Owner : TIpFtpDirFileAttrs;
    Group : TIpFtpDirFileAttrs;
    Other : TIpFtpDirFileAttrs;
  end;

type {Ftp event definitions}
  TIpFtpErrorEvent  = procedure(Sender : TObject;
                              ErrorCode : Integer;
                              const Error : string) of object;
  TIpFtpReplyEvent  = procedure(Sender : TObject;
                              ReplyCode : Integer;
                              const Reply : string) of object;
  TIpFtpStatusEvent = procedure(Sender : TObject;
                              StatusCode : TIpFtpStatusCode;
                              const Info : string) of object;


type {forwards}
  TIpFtpDirectoryTree = class;

     {custom ftp component}
  TIpCustomFtpClient = class(TIpCustomMultiClient)
  protected {private}
    CmdStack      : array[0..MaxCmdStack-1] of string;
    CmdsStacked   : Byte;
    ControlSocket : TSocket;
    DataSocket    : TSocket;                                           {!!.01}
    ListenSocket  : TSocket;                                           {!!.01}
    ListenSock    : TIpListenSocket;                                   {!!.01}
    MultiLine     : Boolean;
    MultiLineTerm : string;
    ReplyBuffer   : string;
    LocalStream   : TStream;
    ProcessState  : TIpFtpProcessState;

  protected {property variables}
    FAccount          : string;
    FBytesTransferred : Longint;
    FFileType         : TIpFtpFileType;
    FDirectoryTree    : TIpFtpDirectoryTree;
    FIdleTimeout      : DWORD;                                         {!!.01}
    FLocalFile        : string;
    FPassword         : string;
    FPassiveMode      : Boolean;
    FTransferTimeout  : DWORD;
    FRemoteFile       : string;
    FServerAddress    : string;
    FUserLoggedIn     : Boolean;
    FUserName         : string;

  protected {event variables}
    FOnFtpError        : TIpFtpErrorEvent;
    FOnFtpStatus       : TIpFtpStatusEvent;
    FOnFtpConnected    : TNotifyEvent;
    FOnFtpDisconnected : TNotifyEvent;  
    FOnFtpReply        : TIpFtpReplyEvent;
    FOnFtpLoginError   : TIpFtpErrorEvent;                             {!!.11}
  protected {methods}
    procedure AbortTransfer;                                           {!!.01}
    procedure ChangeState(NewState : TIpFtpProcessState);
    procedure CMIpFtpReply(var Msg : TMessage); message CM_IPFTPREPLY;
    procedure CMIpFtpStatus(var Msg : TMessage); message CM_IPFTPSTATUS;
    procedure CMIpFtpTimeout(var Msg : TMessage); message CM_IPFTPTIMEOUT;
    procedure ProcessReply (Reply : String; Code : Integer);           {!!.12}
    function  CreateListenSocket : string;                             {!!.01}
    function  DataConnectPASV(IP : string) : Boolean;                  {!!.01}
    procedure DataShutdown;
    procedure DoIdleTimeout(Socket : TSocket; var Reset : Boolean); override;
    procedure DoFtpError(Code : Integer; const Text : string);           {!!.02}
    procedure DoFtpStatus(Code : TIpFtpStatusCode; const Text : string); {!!.02}
    procedure DoListDir(const FtpRemotePathName : string; FullList : Boolean);
    procedure DoReadLine(Socket : TSocket; const Line : string); override;
    procedure DoStatus(Socket : TSocket;
                       Event : TIpStatusType;
                       Connection : TIpConnRec); override;
    function  GetConnected : Boolean;                                  {!!.02}
    function  GetIdleTimeout : DWord;                                  {!!.02}
    procedure SetIdleTimeout(Value : DWord);                           {!!.02}
    function  GetInProgress : Boolean;
    procedure Notification(AComponent : TComponent;
                           Operation : TOperation); override;
    function  PopCommand : string;
    procedure PostFtpStatus(Code : TIpFtpStatusCode; const Text : string); {!!.02}
    procedure PostFtpReply(Code : Integer; const Text : string);           {!!.02}
    procedure PushCommand(const Cmd : string);
    procedure scAccept(Socket : TSocket); override;                    {!!.01}
    procedure SendCommand(const Cmd : string);
    procedure SetPassiveMode(Value : Boolean);
    procedure DoError(Socket : TSocket; ErrCode : Integer;             {!!.11}
      const ErrStr : string); override;                                {!!.11}
  protected {properties}
    property DirectoryTree : TIpFtpDirectoryTree
      read FDirectoryTree write FDirectoryTree;
    property FileType : TIpFtpFileType
      read FFileType write FFileType;
    property IdleTimeout : DWORD                                       {!!.01}
      read GetIdleTimeout write SetIdleTimeout;                        {!!.02}
    property PassiveMode : Boolean
      read FPassiveMode write SetPassiveMode;
    property TransferTimeout : DWORD
      read FTransferTimeout write FTransferTimeout;

  protected {events}
    property OnFtpError : TIpFtpErrorEvent
      read FOnFtpError write FOnFtpError;
    property OnFtpConnected : TNotifyEvent                             {!!.12}
      read FOnFtpConnected write FOnFtpConnected;                      {!!.12}
    property OnFtpDisconnected : TNotifyEvent                          {!!.12}
      read FOnFtpDisconnected write FOnFtpDisconnected;                {!!.12}
    property OnFtpReply : TIpFtpReplyEvent
      read FOnFtpReply write FOnFtpReply;                              
    property OnFtpStatus : TIpFtpStatusEvent
      read FOnFtpStatus write FOnFtpStatus;
    property OnFtpLoginError : TIpFtpErrorEvent                        {!!.11}
      read FOnFtpLoginError write FOnFtpLoginError;                    {!!.11}
  public {run-time properties}
    property Account  : string
      read FAccount write FAccount;                                    {!!.02}
    property BytesTransferred : Longint
      read FBytesTransferred;
    property Connected : Boolean                                       {!!.02}
      read GetConnected;
    property InProgress : Boolean
      read GetInProgress;
    property Password : string
      read FPassword write FPassword;                                  {!!.02}
    property ServerAddress : string
      read FServerAddress;
    property UserLoggedIn : Boolean
      read FUserLoggedIn;
    property UserName : string
      read FUserName write FUserName;                                  {!!.02}

  public {methods}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    class function GetLogString(const S, D1, D2, D3 : DWORD) : string; override;

    function  Abort : Boolean;
    function  ChangeDir(const RemotePathName : string) : Boolean;
    procedure SetServerType( Reply : string );                         {!!.03}
    function  ServerSystem : Boolean;                                  {!!.03}
    function  CurrentDir : Boolean;
    function  Delete(const RemotePathName : string) : Boolean;
    procedure Disconnect;                                              {!!.12}
    function  Help(const Command : string) : Boolean;
    function  ListDir(const RemotePathName : string;
                      FullList : Boolean) : Boolean;
    function  Login(const URL, UserName, Password, Account : string) : Boolean;
    function  Logout : Boolean;
    function  MakeDir(const RemotePathName : string) : Boolean;
    function  Rename(const RemotePathName, NewPathName : string) : Boolean;
    function  Retrieve(const RemotePathName, LocalPathName : string;
                       RetrieveMode : TIpFtpRetrieveMode;
                       RestartAt : Longint) : Boolean;
    function  SendFtpCommand(const FtpCmd : string) : Boolean;
    function  Status(const RemotePathName : string) : Boolean;
    function  Store(const RemotePathName, LocalPathName : string;
                    StoreMode : TIpFtpStoreMode;
                    RestartAt : Longint) : Boolean;
    function  SystemName : Boolean;
  end;


  {Ftp component}
  TIpFtpClient = class(TIpCustomFtpClient)
  published {properties}
    property DebugLog;
    property DefaultPort;
    property DirectoryTree;
    property EventLog;
    property FileType;
    property IdleTimeout;                                              {!!.01}
    property PassiveMode;
    property SocksServer;                                              {!!.01}
    property TransferTimeout;
             {events}
    property OnError;                                                  {!!.12}
    property OnFtpConnected;                                           {!!.12}
    property OnFtpDisconnected;                                        {!!.12}
    property OnFtpError;
    property OnFtpLoginError;                                          {!!.12}
    property OnFtpReply;
    property OnFtpStatus;
    property OnIdleTimeout;                                            {!!.01}
  end;


  { TIpFtpFileInfo }
  TIpFtpFileInfo = class;

  TIpUserParseStyleEvent = procedure(Sender : TObject;
                                     const Info : string;
                                     FtpFileInfo : TIpFtpFileInfo) of object;

  TIpFtpFileInfo = class
  private
    function GetDateTime: TDateTime;
  protected {private}
    FFileName    : string;
    FFileType    : TIpFtpDirFileType;
    FGroup       : string;
    FLinkPath    : string;
    FOwner       : string;
    FPermissions : TIpFtpDirFilePermissions;
    FSize        : DWord;
    FTimeStamp   : string;

  protected {methods}
    procedure UnixStyleParse(Info : TStringList);
    procedure DosStyleParse(Info : TStringList);
    procedure MultiNetStyleParse(Info : TStringList);
    procedure VMSStyleParse(Info : TStringList);

  public {properties}
    property FileName : string
      read FFileName;
    property FileType : TIpFtpDirFileType
      read FFileType;
    property Group : string
      read FGroup;
    property LinkPath : string
      read FLinkPath;
    property Owner : string
      read FOwner;
    property Permissions : TIpFtpDirFilePermissions
      read FPermissions;
    property Size  : DWord
      read FSize;
    property TimeStamp : string
      read FTimeStamp;
    property DateTime: TDateTime
      read GetDateTime;

  public {methods}
    constructor Create(const Info : string; Style : TIpFtpDirStyle;
                       UserParse : TIpUserParseStyleEvent);
    destructor Destroy; override;
  end;


  { TIpFtpFileList }
  TIpFtpFileList = class(TList)
  public {methods}
    destructor Destroy; override;
    function Populate(Info : string; Style : TIpFtpDirStyle;
                      UserParse : TIpUserParseStyleEvent) : Boolean;
    procedure ClearItems;
  end;


  { TIpFtpDirectoryTree }
  TIpFtpDirectoryTree = class(TCustomTreeView)
  protected {private}
    ClearingItems    : Boolean;

  protected {property variables}
    FCurrentDir     : string;
    FDirectoryStyle : TIpFtpDirStyle;
    FFtpClient      : TIpCustomFtpClient;
    FFileList       : TIpFtpFileList;
    FListBox        : TListBox;
    FOnChanged      : TNotifyEvent;
    FOnParseStyle   : TIpUserParseStyleEvent;
    FUseDefaultImages : Boolean;

  protected {methods}
    procedure AddDirectoryNode(const Path : string);
    procedure Change(Node: TTreeNode); override;
    procedure GetImageIndex(Node: TTreeNode); {$IFNDEF VER100} override; {$ENDIF}
    procedure GetSelectedIndex(Node: TTreeNode); {$IFNDEF VER100} override; {$ENDIF}
    procedure Loaded; override;
    procedure Notification(AComponent : TComponent;
                           Operation: TOperation); override;
    procedure PopulateFileList(Text : string);
    procedure SetDirectoryStyle(Style : TIpFtpDirStyle);
    procedure SetOnParseStyle(Value : TIpUserParseStyleEvent);
    function GetVersion : string;
    procedure SetVersion(const Value : string);

  public {run-time properties}
    property FileList : TIpFtpFileList
      read FFileList;
    property CurrentDir : string
      read FCurrentDir;

  public {methods}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    function GetFullPath(Node : TTreeNode) : string;
    function GetVMSPath(Node : TTreeNode) : string;
    function FindDirectoryNode(const Path : string) : TTreeNode;

  published {properties and events}
    property FtpClient : TIpCustomFtpClient
      read FFtpClient write FFtpClient;
    property ListBox : TListBox
      read FListBox write FListBox;
    property DirectoryStyle : TIpFtpDirStyle
      read FDirectoryStyle write SetDirectoryStyle;
    property OnChanged : TNotifyEvent
      read FOnChanged write FOnChanged;
    property OnParseStyle : TIpUserParseStyleEvent
      read FOnParseStyle write SetOnParseStyle;
    property UseDefaultImages : Boolean
      read FUseDefaultImages write FUseDefaultImages;
    property Version : string
      read GetVersion write SetVersion stored False;

    { inherited properties }
    property ShowButtons;
    property BorderStyle;
    property DragCursor;
    property ShowLines;
    property ShowRoot;
    property ReadOnly;
    property DragMode;
    property HideSelection;
    property Indent;
    property OnEditing;
    property OnEdited;
    property OnExpanded;
    property OnCompare;
    property OnCollapsed;
    property OnChange;
    property OnDeletion;
    property Align;
    property Enabled;
    property Font;
    property Color;
    property ParentColor;
    property ParentCtl3D;
    property Ctl3D;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Images;
  end;


implementation

const {file data type constants}
  TypeChar  : array[TIpFtpFileType] of Char = ('A', 'I');

const {FTP commands}
  fcABOR = 'ABOR';
  fcACCT = 'ACCT';
  fcALLO = 'ALLO';
  fcAPPE = 'APPE';
  fcCDUP = 'CDUP';
  fcCWD  = 'CWD';
  fcDELE = 'DELE';
  fcHELP = 'HELP';
  fcLIST = 'LIST';
  fcMKD  = 'MKD';
  fcMODE = 'MODE';
  fcNLST = 'NLST';
  fcNOOP = 'NOOP';
  fcPASS = 'PASS';
  fcPASV = 'PASV';
  fcPORT = 'PORT';
  fcPWD  = 'PWD';
  fcQUIT = 'QUIT';
  fcREIN = 'REIN';
  fcREST = 'REST';
  fcRETR = 'RETR';
  fcRMD  = 'RMD';
  fcRNFR = 'RNFR';
  fcRNTO = 'RNTO';
  fcSITE = 'SITE';
  fcSIZE = 'SIZE';
  fcSMNT = 'SMNT';
  fcSTAT = 'STAT';
  fcSTOR = 'STOR';
  fcSTOU = 'STOU';
  fcSTRU = 'STRU';
  fcSYST = 'SYST';
  fcTYPE = 'TYPE';
  fcUSER = 'USER';

type {miscellaneous types}
  wParam = Longint;
  lParam = Longint;

var
  gFormatSettings: TFormatSettings;

{ Parse string into string list }
function Parse(const Info, Delim : string): TStringList;
var
  iPos : Integer;
  Remaining : string;
  Term : string;
begin
  Result := TStringList.Create;
  Remaining := Info;
  iPos := Pos(Delim, Remaining);
  while (iPos > 0) do begin
    Term := Copy(Remaining, 1, iPos - 1);
    if (Term <> '') then
      Result.Add(Trim(Term));
    Remaining := Copy(Remaining, iPos + 1, Length(Remaining));
    iPos := Pos(Delim, Remaining);
  end;
  if Length(Remaining) > 0 then
     Result.Add(Trim(Remaining));
end;

{ Change '/' to '\'}
function FtpToDOS(const Path : string) : string;
var
  i : Integer;                                                         {!!.02}
begin
  Result := Path;
  for i := 1 to Length(Result) do                                      {!!.02}
    if (Result[i] = '/') then                                          {!!.02}
      Result[i] := '\';                                                {!!.02}
end;

{ Change '\' to '/'}
function DOSToFtp(const Path : string) : string;
var
  i : Integer;                                                         {!!.02}
begin
  Result := Path;
  for i := 1 to Length(Result) do                                      {!!.02}
    if (Result[i] = '\') then                                          {!!.02}
      Result[i] := '/';                                                {!!.02}
end;


{ TIpCustomFtpClient }
constructor TIpCustomFtpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultPort := IpPort_Ftp;
  FPassiveMode := False;
  FIdleTimeout := 0;
  FTransferTimeout := 0;
  FUserLoggedIn := False;
  ProcessState := psClosed;
  MultiLine := False;
  ControlSocket := Invalid_Socket;
  DataSocket := Invalid_Socket;

end;

destructor TIpCustomFtpClient.Destroy;
begin
  {msCloseAll;}                                                        {!!.02}
  inherited Destroy;
end;

{ Terminate file transfer in progress }
function TIpCustomFtpClient.Abort : Boolean;
begin
  Result := (ProcessState > psIdle);
  if Result then begin
    AbortTransfer;
    SendCommand(fcABOR);
    EventLog.WriteEventString(sFtpUserAbort);
  end;
end;

{!!.01}
{ Local abort data transfer }
procedure TIpCustomFtpClient.AbortTransfer;
begin
  if (DataSocket <> Invalid_Socket) then begin                         {!!.03}
    msCloseSocket(DataSocket);
    DataSocket := Invalid_Socket;                                      {!!.03}
  end;                                                                 {!!.03}
end;

{ Change the current working directory }
function TIpCustomFtpClient.ChangeDir(const RemotePathName : string) : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psDir);
    if (RemotePathName <> '') then begin
//      PushCommand(fcPWD);                                            {Deleted !!.12}
      SendCommand(fcCWD + ' ' + DosToFtp(RemotePathName));             {!!.11}
    end else
      SendCommand(fcPWD);
  end;
end;

{ Change state variable and cleanup as necessary }
procedure TIpCustomFtpClient.ChangeState(NewState : TIpFtpProcessState);
begin
  ProcessState := NewState;
end;

{ Decoupling message handler to fire the OnFtpStatus event }
procedure TIpCustomFtpClient.CMIpFtpStatus(var Msg : TMessage);
var
  Code : TIpFtpStatusCode;
  pText : PChar;
begin
  Code := TIpFtpStatusCode(Msg.wParam);
  pText := Pointer(Msg.lParam);
  DoFtpStatus(Code, StrPas(pText));
  if (pText <> nil) then
    StrDispose(pText);
end;

{ Decoupling message handler to cleanup after timeout and fire the OnFtpStatus event }
procedure TIpCustomFtpClient.CMIpFtpTimeout(var Msg : TMessage);
var
  pText : PChar;
begin
  pText := Pointer(Msg.lParam);
  if (pText <> nil) then
    StrDispose(pText);
  AbortTransfer;                                                       {!!.01}
  if (ProcessState > psLogin) then
    ChangeState(psIdle)
  else
    ChangeState(psClosed);
  if Assigned(FOnFtpStatus) then
    FOnFtpStatus(Self, fscTimeout, '');
end;

procedure TIpCustomFtpClient.ProcessReply (Reply : String; Code : Integer);
  procedure DoError;
  begin
    case Code of
      221, 421 : ChangeState(psClosed);
    else
      DoFtpError(Code, Reply);
    end;
  end;
var
  Str   : string;
begin
  case Code of
    110 : DoError;
    120 : DoError;
    125 : ; {ignore for now}
    150 : ; {ignore for now}
    200 : if (PopCommand = '') then
            ChangeState(psIdle);
    202 : DoError;
    211 : DoFtpStatus(fscInfo, Copy( Reply, 4, Length( Reply )));      {!!.03}
    212 : DoFtpStatus(fscInfo, Copy( Reply, 4, Length( Reply )));      {!!.03}
    213 : DoFtpStatus(fscInfo, Copy( Reply, 4, Length( Reply )));      {!!.03}
    214 : DoFtpStatus(fscInfo, Copy( Reply, 4, Length( Reply )));      {!!.03}
    215 : SetServerType( Reply );                                      {!!.03}
    220 : if (FUserName <> '') then                                    {!!.02}
            SendCommand(fcUSER + ' ' + FUserName)
          else begin                                                   {!!.02}
            ChangeState(psIdle);                                       {!!.02}
            FUserLoggedIn := True;                                     {!!.02}
            DoFtpStatus(fscLogin, '');                                 {!!.02}
          end;                                                         {!!.02}
    221 : msCloseAll;                                                  {!!.02}
    225 : ChangeState(psIdle);
    226 : case ProcessState of
            psClosed : DoError;
            psIdle   : ; (* DataShutdown; *)                           {!!.02}
            psRen    : DoError;
            psCmd    : ChangeState(psIdle);
          end;
    227 : begin
            Str := Copy(Reply, CharPos('(', Reply) + 1, Length(Reply));  {!!.02}
            Str := Copy(Str, 1, CharPos(')', Str) - 1);                  {!!.02}
            DataConnectPASV(Str);                                        {!!.01}
            PopCommand;
          end;
    230 : begin
            ChangeState(psIdle);
            FUserLoggedIn := True;                                     {!!.02}
            DoFtpStatus(fscLogin, '');
          end;
    250 : begin
            if (ProcessState = psDir) then begin
              ChangeState(psIdle);
              CurrentDir;                                              {!!.01}
            end else if( ProcessState = psGet ) or                     {!!.03}
                       ( ProcessState = psPut )then begin              {!!.03}
              {ignore for now}                                         {!!.03}
            end else begin
              ChangeState(psIdle);
              DoFtpStatus(fscComplete, '');
            end;
          end;
    257 : begin
            Str := Copy(Reply, CharPos('"', Reply) + 1, Length(Reply));  {!!.02}
            Str := Copy(Str, 1, CharPos('"', Str) - 1);                  {!!.02}
            DoFtpStatus(fscCurrentDir, Str);                             {!!.02}
          end;
    331 : if ProcessState <> psCmd then                                {!!.03}
            SendCommand( fcPASS + ' ' + FPassword );
    332 : SendCommand(fcACCT + ' ' + FAccount);
    350 : PopCommand;
  else
    DoError;
  end;
end;
                     
{ Ftp state machine driven by decoupled reply from Ftp server }
procedure TIpCustomFtpClient.CMIpFtpReply(var Msg : TMessage);
var
  Code : Integer;
  pText : PChar;
  Reply : string;
begin
  Code := Msg.wParam;
  pText := Pointer(Msg.lParam);
  Reply := StrPas(pText);
  StrDispose(pText);

  if not MultiLine then begin
    ReplyBuffer := Reply;
    if (Reply[4] = '-') then begin
      MultiLine := True;
      MultiLineTerm := IntToStr(Code) + ' ';
      Exit;
    end;
  end else begin
    if (Pos(MultiLineTerm, Reply) <> 1) then begin
      ReplyBuffer := ReplyBuffer + Reply;
      Exit;
    end else
      MultiLine := False
  end;
  Reply := ReplyBuffer;
  DebugLog.WriteDebugString(Reply);

  ProcessReply (Reply, Code);

  if ProcessState = psCmd then                                         {!!.03}
    ChangeState( psIdle );
  if Assigned (FOnFtpReply) then
    FOnFtpReply(Self, Code, Reply);
end;

{ Create listen socket and return PortIP string }
function TIpCustomFtpClient.CreateListenSocket : string;
var
  SockName : TSockAddrIn;
  NameLen : Integer;
begin
  if Assigned(ListenSock) then
    IpSafeFree(ListenSock);
  ListenSock := nil;
  ListenSocket := Invalid_Socket;
  NameLen := SizeOf(SockName);
  IpWinSockAccess.GetSockName(ControlSocket, SockName, NameLen);
  SockName.sin_family := AF_INET;
  SockName.sin_port := IpWinSockAccess.htons(0);
  ListenSock := TIpListenSocket.Create(Self, Invalid_Socket, SockName, nil, nil);
  ListenSocket := ListenSock.SocketHandle;
  IpWinSockAccess.GetSockName(ListenSocket, SockName, NameLen);
  with SockName do
    Result := StrPas(IpWinSockAccess.INet_NtoA(sin_addr)) + '.' +
      IntToStr(Lo(sin_port)) + '.' + IntToStr(Hi(sin_port));
  while CharPos('.', Result) > 0 do                                    {!!.02}
    Result[CharPos('.', Result)]  := ',';                              {!!.02}
end;


{ Make a guess at the remote server type from it SYST response }       {!!.03}
procedure TIpCustomFtpClient.SetServerType( Reply : string );
var
  sl : TStringList;
begin
  // if the type has not been set then try to set it
  if Assigned(DirectoryTree) then begin                                {!!.10}
    if( DirectoryTree.DirectoryStyle = dsNone )then begin
      sl := Parse( Reply, ' ' );
      try                                                              {!!.10}
        sl[ 1 ] := UpperCase( sl[ 1 ]);
        if( AnsiPos( 'WIND', sl[ 1 ] ) <> 0 )then
          DirectoryTree.DirectoryStyle := dsDos
        else if( AnsiPos( 'UNIX', sl[ 1 ] ) <> 0 )then
          DirectoryTree.DirectoryStyle := dsUnix
        else if( AnsiPos( 'VMS', sl[ 1 ] ) <> 0 )then
          DirectoryTree.DirectoryStyle := dsVms
        else
          DirectoryTree.DirectoryStyle := dsNone;
      finally                                                          {!!.10}
        sl.Free;                                                       {!!.10}
      end;                                                             {!!.10}
    end;
  end;                                                                 {!!.10}
  ChangeState(psIdle);
  CurrentDir;
end;

{ Request server type }                                                {!!.03}
function TIpCustomFtpClient.ServerSystem : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState( psCmd  );
    SendCommand( fcSYST );
  end;
end;

{ Get the name of the current working directory }
function TIpCustomFtpClient.CurrentDir : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psCmd);
    SendCommand(fcPWD);
  end;
end;

{ Establish passive data connection to specified IP }
function TIpCustomFtpClient.DataConnectPASV(IP : string) : Boolean;
var
  RemoteURL : string;
  PortHi, PortLo : string;
  iPort, i, j : Integer;
begin

  { convert A1,A2,A3,A4,P1,P2 -> A1.A2.A3.A4:Port }
  RemoteURL := IP;
  for i := 1 to 4 do begin
    j := CharPos(',', RemoteURL);                                      {!!.02}
    if (j > 0) then
      case i of
        1,2,3 : RemoteURL[j]  := '.';
        4     : RemoteURL[j]  := ':';
      end;
  end;
  i := CharPos(':', RemoteURL);                                        {!!.02}
  j := CharPos(',', RemoteURL);                                        {!!.02}
  PortHi := System.Copy(RemoteURL, i+1, j-i-1);
  PortLo := System.Copy(RemoteURL, j+1, Length(RemoteURL));
  System.Delete(RemoteURL, i+1, Length(RemoteURL));
  iPort := (StrToIntDef(PortHi, 0) shl 8) + StrToIntDef(PortLo, 0);
  RemoteURL := RemoteURL + IntToStr(iPort);

  { establish data connection }
  DataSocket := mcConnectSocket(RemoteURL);
  Result := DataSocket <> Invalid_Socket;
  if Result then begin
    inherited IdleTimeout[DataSocket] := FTransferTimeout;
  end;
end;

{ Shutdown data connection }
procedure TIpCustomFtpClient.DataShutDown;
begin
  if Assigned(LocalStream) then begin
    AbortTransfer;                                                     {!!.03}
    LocalStream.Free;
    LocalStream := nil;
  end;

  if Assigned(ListenSock) then begin
    IpSafeFree(ListenSock);
    ListenSock := nil;
  end;
  ListenSocket := Invalid_Socket;
end;

{ Delete specified remote file or directory }
function TIpCustomFtpClient.Delete(const RemotePathName : string) : Boolean;
begin
  Result := (ProcessState = psIdle) and (RemotePathName <> '');
  if Result then begin
    ChangeState(psCmd);
    FRemoteFile := DosToFtp(RemotePathName);
    SendCommand(fcDELE + ' ' + FRemoteFile);
    EventLog.WriteEventString(sFtpDelete + FRemoteFile);
  end;
end;

{Begin !!.12}
{ Disconnect the FTP client. Should be used if detect a bogus connection to a
  bogus ftp server or if wanting to abruptly disconnect the ftp client from the
  ftp server. }
procedure TIpCustomFtpClient.Disconnect;
var
  Connection : TIpConnRec;
begin
  if DataSocket <> INVALID_SOCKET then
    DoStatus(DataSocket, stDisconnect, Connection);
  if ControlSocket <> INVALID_SOCKET then
    DoStatus(ControlSocket, stDisconnect, Connection);
end;
{End !!.12}
{ Handle IdleTimeout from socket }
procedure TIpCustomFtpClient.DoIdleTimeout(Socket : TSocket; var Reset : Boolean);
begin
  if (Socket = DataSocket) then begin
    CmdsStacked := 0;
    ChangeState(psIdle);
    DataShutdown;
    PostFtpStatus(fscTimeout, '');
  end else
    inherited DoIdleTimeout(Socket, Reset);
end;

{ List contents of remote directory }
procedure TIpCustomFtpClient.DoListDir(const FtpRemotePathName : string;
                                       FullList : Boolean);
var
  S : string;
begin
  if FullList then
    S := fcLIST
  else
   S := fcNLST;
  if (FtpRemotePathName <> '') then
    S := S + ' ' + FtpRemotePathName;
  PushCommand(S);
  LocalStream := TStringStream.Create('');                             {!!.02}
  FBytesTransferred := 0;
  if PassiveMode then begin
    PushCommand(fcPASV);
    SendCommand(fcType + ' ' + TypeChar[ftAscii]);
  end else begin
    PushCommand(fcType + ' ' + TypeChar[ftAscii]);
    SendCommand(fcPORT + ' ' + CreateListenSocket);
  end;
end;

{ Read in reply from server and queue it up }
procedure TIpCustomFtpClient.DoReadLine(Socket: TSocket; const Line: string);
var
  Code : Integer;
  pReply : PChar;
begin
  if (Socket = ControlSocket) then begin
    if (Line <> '') then begin
      Code := StrToIntDef(Copy(Line, 1, 3), 0);
      pReply := StrNew(PChar(Line));
      PostMessage(Self.Handle, CM_IPFTPREPLY, Code, Longint(pReply));
    end;
  end else
    inherited DoReadLine(Socket, Line);
end;

{ Process control socket status change }
procedure TIpCustomFtpClient.DoStatus(Socket : TSocket; Event : TIpStatusType;
  Connection : TIpConnRec);

  { local routine to obtain transfer progress byte count }
  function GetProgress : Cardinal;
  var
    Stats : TIpSockStatRec;
  begin
    scGetSocket(Socket, True).StatusSnapshot(Stats);
    if (ProcessState = psGet) then
      Result := Stats.BytesRcvd
    else
      Result := Stats.BytesSent;
  end;

begin
  if (Socket = ControlSocket) then
    begin
      if (Event = stConnect) then begin
        ChangeState(psLogin);
        if Assigned (FOnFtpConnected) then                               {!!.12}
          FOnFtpConnected (Self);                                        {!!.12}
      end else if (Event = stDisconnect) then begin
        DataShutDown;
        FUserLoggedIn := False;
        CmdsStacked := 0;
        ChangeState(psClosed);
        PostFtpStatus(fscClose, '');
        ControlSocket := Invalid_Socket;
        if Assigned (FOnFtpDisconnected) then                            {!!.12}
          FOnFtpDisconnected (Self);                                     {!!.12}
      end;
    end  {ControlSocket}
  else if (Socket = DataSocket) then
    begin
      if (Event = stConnect) then
        begin
          if (ProcessState = psPut) then begin
            scPutStream(DataSocket, LocalStream, True);
          end else
            begin
              ReceiveMode[DataSocket] := rmStream;
              ReceiveStream[DataSocket] := LocalStream;
            end;
        end
      else if (Event = stDisconnect) then
        begin
          if (ProcessState = psDir) then
            with (LocalStream as TStringStream) do begin
              Position := 0;
              if Assigned(FDirectoryTree) then
                FDirectoryTree.PopulateFileList(DataString);
              PostFtpStatus(fscDirList, DataString);
            end
          else if (ProcessState = psGet) or (ProcessState = psPut) then
            begin
              FBytesTransferred := GetProgress;
              PostFtpStatus(fscTransferOK, '');
            end;
          DataSocket := Invalid_Socket;
        end
      else if (Event = stProgress) and (ProcessState <> psDir) then
        begin
          FBytesTransferred := GetProgress;
          PostFtpStatus(fscProgress, '');
        end;
    end;
(*  inherited DoStatus(Socket, Event, Connection); *)                  {!!.02}
end;

{!!.02}
{ Connected property read access method }
function TIpCustomFtpClient.GetConnected : Boolean;
begin
  if (ControlSocket <> Invalid_Socket) then
    Result := inherited Connected[ControlSocket]
  else
    Result := False;
end;

{!!.02}
{ IdleTimeout property read access method }
function TIpCustomFtpClient.GetIdleTimeout : DWord;
begin
  if (ControlSocket <> Invalid_Socket) then
    FIdleTimeout := inherited IdleTimeout[ControlSocket];
  Result := FIdleTimeout;
end;

{!!.02}
{ IdleTimeout property write access method }
procedure TIpCustomFtpClient.SetIdleTimeout(Value : DWord);
begin
  FIdleTimeout := Value;
  if (ControlSocket <> Invalid_Socket) then
    inherited IdleTimeout[ControlSocket] := FIdleTimeout;
end;

{ InProgress property read access method }
function TIpCustomFtpClient.GetInProgress : Boolean;
begin
  Result := not ((ProcessState = psClosed) or (ProcessState = psIdle));
end;

{ Pass formatted log string back to debug log }
class function TIpCustomFtpClient.GetLogString(const S, D1, D2, D3: DWORD): string;
begin
  if (D1 = 1) then
    Result := ' A ' + IntToStr(D2) + '    ' + IntToStr(D3)
  else if (D1 = 2) then
    Result := ' D ' + IntToStr(D2) + '    ' + IntToStr(D3)
  else
    Result := '';
end;

{ Obtain help for the specified Ftp command }
function TIpCustomFtpClient.Help(const Command : string) : Boolean;
var
  Cmd : string;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psCmd);
    Cmd := fcHELP;
    if (Command <> '') then
      Cmd := Cmd + ' ' + Command;
    SendCommand(Cmd);
  end;
end;

{ List contents of remote directory }
function TIpCustomFtpClient.ListDir(const RemotePathName : string;
                                    FullList : Boolean) : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psDir);
    DoListDir(DosToFtp(RemotePathName), FullList);
  end;
end;

{ Log on to FTP server }
function TIpCustomFtpClient.Login(const URL, UserName, Password, Account : string) : Boolean;
begin
  Result := (ProcessState = psClosed);
  if Result then begin
    ChangeState(psLogin);
    FServerAddress := URL;
    FUserName := UserName;
    FPassword := Password;
    FAccount := Account;
    try                                                                {!!.01}
      ControlSocket := scCreateSocket;                                 {!!.02}
      Result := (ControlSocket <> Invalid_Socket);
      if Result then begin
        inherited IdleTimeout[ControlSocket] := FIdleTimeout;
        mcConnectThisSocket(ControlSocket, FServerAddress);            {!!.02}
      end else
        ChangeState(psClosed);
    except                                                             {!!.01}
      ChangeState(psClosed);                                           {!!.01}
      raise;                                                           {!!.01}
    end;                                                               {!!.01}
  end;
end;

{ Log off of FTP server}
function TIpCustomFtpClient.Logout : Boolean;
begin
  Result := (ProcessState <> psClosed);
  if Result then
    SendCommand(fcQUIT);
end;

{ Create specified remote directory }
function TIpCustomFtpClient.MakeDir(const RemotePathName : string) : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psCmd);
    SendCommand(fcMKD + ' ' + DosToFtp(RemotePathName));
  end;
end;

{ Notification of new or delete log and directory tree components }
procedure TIpCustomFtpClient.Notification(AComponent : TComponent;
                                     Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then begin
    if (AComponent = FDirectoryTree) then
      FDirectoryTree := nil;
  end else if (Operation = opInsert) then
    if (AComponent is TIpFtpDirectoryTree) then begin
      if not Assigned(FDirectoryTree) then
        if not Assigned(TIpFtpDirectoryTree(AComponent).FFtpClient) then begin
          FDirectoryTree := TIpFtpDirectoryTree(AComponent);
          FDirectoryTree.FtpClient := Self;
        end;
    end;
end;

{ Pop ftp command off of command stack }
function TIpCustomFtpClient.PopCommand : string;
begin
  if (CmdsStacked > 0) then begin
    Dec(CmdsStacked);
    Result := CmdStack[CmdsStacked];
    SendCommand(Result);
  end else
    Result := '';
end;

{!!.02}
{ Fire error event }
procedure TIpCustomFtpClient.DoFtpError(Code : Integer; const Text : string);
begin
  if (ProcessState > psIdle) then
    ChangeState(psIdle);
  if Assigned(FOnFtpError) then
    FOnFtpError(Self, Code, Text);
end;
{!!.02}

{ Queue up reply event }
procedure TIpCustomFtpClient.PostFtpReply(Code : Integer; const Text : string);
var
  pData : PChar;
begin
  pData := StrNew(PChar(Text));
  PostMessage(Self.Handle, CM_IPFTPREPLY, Code, Longint(pData));
end;

{ Queue up status event }
procedure TIpCustomFtpClient.PostFtpStatus(Code : TIpFtpStatusCode; const Text : string);
var
  pData : PChar;
begin
  if (Text <> '') then
    pData := StrNew(PChar(Text))                                       {!!.02}
  else
    pData := nil;
  PostMessage(Self.Handle, CM_IPFTPSTATUS, Integer(Code), Longint(pData));
end;

{ fire status event directly }
procedure TIpCustomFtpClient.DoFtpStatus(Code : TIpFtpStatusCode; const Text : string);
begin
  if (Code > fscLogin) and (Code <> fscProgress) then
    ChangeState(psIdle);
  if (Code = fscTransferOk) or (Code = fscDirList) then                {!!.02}
    DataShutDown;                                                      {!!.02}

  { Feed the directory tree if assigned }
  if Assigned(FDirectoryTree) then
    case Code of
      fscClose :
        DirectoryTree.Clear;
      fscLogin :
        ServerSystem;                                                  {!!.03}
      fscSystem     :                                                  {!!.03}
        CurrentDir;                                                    {!!.03}
      fscCurrentDir :
        begin
          DirectoryTree.AddDirectoryNode(FtpToDos(Text));
          ChangeState(psDir);
          DoListDir('', DirectoryTree.DirectoryStyle <> dsNone);
        end;
    end;

  { Fire the OnFtpStatus event }
  if Assigned(FOnFtpStatus) then
    FOnFtpStatus(Self, Code, Text);
end;

{ Push ftp command onto command stack - dont call from an event handler }
procedure TIpCustomFtpClient.PushCommand(const Cmd : string);
begin
  if (CmdsStacked < MaxCmdStack) then begin
    CmdStack[CmdsStacked] := Cmd;
    Inc(CmdsStacked);
  end else begin
    CmdsStacked := 0;
    raise Exception.Create('Fatal Error: Command stack full');
  end;
end;

{ Rename specified remote file or directory }
function TIpCustomFtpClient.Rename(const RemotePathName, NewPathName : string) : Boolean;
begin
  Result := (ProcessState = psIdle) and
            (RemotePathName <> '') and (NewPathName <> '');
  if Result then begin
    ChangeState(psRen);
    PushCommand(fcRNTO + ' ' + DosToFtp(NewPathName));
    FRemoteFile := DosToFtp(RemotePathName);
    EventLog.WriteEventString(sFtpRename + FRemoteFile);
    SendCommand(fcRNFR + ' ' + FRemoteFile);
  end;
end;

{ Transfer a file from the server }
function TIpCustomFtpClient.Retrieve(const RemotePathName, LocalPathName : string;
                                     RetrieveMode : TIpFtpRetrieveMode;
                                     RestartAt : Longint) : Boolean;
var
  FH : Integer;
begin
  Result := (ProcessState = psIdle) and
            (RemotePathName <> '') and (LocalPathName <> '');
  if Result then begin
    ChangeState(psGet);
    FRemoteFile := DosToFtp(RemotePathName);
    FLocalFile := LocalPathName;
    PushCommand(fcRETR + ' ' + FRemoteFile);
    if not FileExists(FLocalFile) then begin
      FH := FileCreate(FLocalFile);
      FileClose(FH);
    end;

{Begin !!.12}    
    { Make sure local stream is shutdown. }
    if Assigned(LocalStream) then                                     
      DataShutdown;
{End !!.12}

    if (RetrieveMode = rmReplace) then begin
      DeleteFile(FLocalFile);                                          {!!.11}
      LocalStream := TFileStream.Create(FLocalFile, fmCreate);         {!!.11}
      LocalStream.Position := 0;
      EventLog.WriteEventString(sFtpRetrieve + FRemoteFile);
    end else if (RetrieveMode = rmAppend) then begin
      LocalStream := TFileStream.Create(FLocalFile, fmOpenReadWrite);
      LocalStream.Position := LocalStream.Size;
      EventLog.WriteEventString(sFtpRetrieve + FRemoteFile);
    end else begin {RetrieveMode = rmRestart}
      LocalStream := TFileStream.Create(FLocalFile, fmOpenReadWrite);
      if (RestartAt > LocalStream.Size) or (RestartAt < 0) then
        LocalStream.Position := LocalStream.Size
      else
        LocalStream.Position := RestartAt;
      PushCommand(fcREST + ' ' + IntToStr(LocalStream.Position));
      EventLog.WriteEventString(sFtpRestart + FRemoteFile);
    end;
    FBytesTransferred := 0;
    if PassiveMode then begin
      PushCommand(fcPASV);
      SendCommand(fcType + ' ' + TypeChar[FFileType]);
    end else begin
      PushCommand(fcType + ' ' + TypeChar[FFileType]);
      SendCommand(fcPORT + ' ' + CreateListenSocket);
    end;
  end;
end;

{ Handle incoming Fd_Accept and create DataSocket for non-passive mode }
procedure TIpCustomFtpClient.scAccept(Socket : TSocket);
var
  NewSock : TSocket;
  Addr : TSockAddrIn;
  ErrCode, AddrLen : Integer;
  DataSock : TIpDataSocket;
begin
  if (Socket <> ListenSocket) then begin
    inherited scAccept(Socket);
    Exit;
  end else begin
    AddrLen := SizeOf(Addr);
    FillChar(Addr, AddrLen, #0);
    NewSock := IpWinSockAccess.Accept(Socket, Addr, AddrLen);

    if Integer(NewSock) = Socket_Error then begin
      ErrCode := GetLastError;
      if ErrCode <> wsaEWouldBlock then
        raise EIpWinSockError.CreateWsError(ErrCode, 'Accept');
    end else begin
      { Create socket instance and add to list }
      DataSock := TIpDataSocket.Create(Self, NewSock, scLocalSockAddr, SockInits, nil);
      msAddDataSocket(DataSock);
      DataSocket := DataSock.SocketHandle;
      inherited IdleTimeout[DataSocket] := FTransferTimeout;
      scConnect(DataSocket);
    end;
  end;
end;

{ Send FTP command string via control connection }
procedure TIpCustomFtpClient.SendCommand(const Cmd : string);
begin
  if (ControlSocket <> Invalid_Socket) then begin                      {!!.03}
    DebugLog.WriteDebugString(Cmd);
    scPutString(ControlSocket, Cmd + IpCRLF, False);
  end;
end;

{ Send any FTP command }
function TIpCustomFtpClient.SendFtpCommand(const FtpCmd : string) : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psCmd);
    SendCommand(FtpCmd);
  end;
end;

{ PassiveMode property write access method }
procedure TIpCustomFtpClient.SetPassiveMode(Value : Boolean);
begin
  if (ProcessState < psDir) and (FPassiveMode <> Value) then
    FPassiveMode := Value;
end;

{ Post login errors }
{!!.11}{ the whole procedure is new}
procedure TIpCustomFtpClient.DoError(Socket : TSocket; ErrCode : Integer;
  const ErrStr : string);
begin
  if (ProcessState = psLogin) then begin
    FUserLoggedIn := False;
    CmdsStacked := 0;
    ChangeState(psClosed);
    ControlSocket := Invalid_Socket;
    scClose(Socket);
    if Assigned( FOnFtpLoginError ) then
      FOnFtpLoginError(Self, ErrCode, ErrStr);
  end
  else
    inherited DoError(Socket, ErrCode, ErrStr);
end;

{ Obtain status of server or optional directory listing }
function TIpCustomFtpClient.Status(const RemotePathName : string) : Boolean;
var
  Cmd : string;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psCmd);
    Cmd := fcSTAT;
    if (RemotePathName <> '') then
      Cmd := Cmd + ' ' + DosToFtp(RemotePathName);
    SendCommand(Cmd);
  end;
end;

{ Transfer a file to the server }
function TIpCustomFtpClient.Store(const RemotePathName, LocalPathName : string;
                                  StoreMode : TIpFtpStoreMode;
                                  RestartAt : Longint) : Boolean;
begin
  Result := (ProcessState = psIdle) and
            (RemotePathName <> '') and FileExists(LocalPathName);
  if Result then begin
    ChangeState(psPut);
    FRemoteFile := DosToFtp(RemotePathName);
    FLocalFile := LocalPathName;
    if Assigned(LocalStream) then
      DataShutDown;
    LocalStream := TFileStream.Create(FLocalFile, fmOpenRead);
    LocalStream.Position := 0;
    if (StoreMode = smAppend) then begin
      PushCommand(fcAPPE + ' ' + FRemoteFile);
      EventLog.WriteEventString(sFtpStore + FLocalFile);
    end else if (StoreMode = smReplace) then begin
      PushCommand(fcSTOR + ' ' + FRemoteFile);
      EventLog.WriteEventString(sFtpStore + FLocalFile);
    end else if (StoreMode = smUnique) then begin
      PushCommand(fcSTOU + ' ' + FRemoteFile);
      EventLog.WriteEventString(sFtpStore + FLocalFile);
    end else begin {StoreMode = smReplace}
      if (RestartAt > LocalStream.Size) or (RestartAt < 0) then
        LocalStream.Position := 0
      else
        LocalStream.Position := RestartAt;
      PushCommand(fcSTOR + ' ' + FRemoteFile);
      PushCommand(fcREST + ' ' + IntToStr(LocalStream.Position));
      EventLog.WriteEventString(sFtpRestart + FRemoteFile);
    end;
    FBytesTransferred := 0;
    if PassiveMode then begin
      PushCommand(fcPASV);
      SendCommand(fcType + ' ' + TypeChar[FFileType]);
    end else begin
      PushCommand(fcType + ' ' + TypeChar[FFileType]);
      SendCommand(fcPORT + ' ' + CreateListenSocket);
    end;
  end;
end;

{ Obtain the name of the server's operating system }
function TIpCustomFtpClient.SystemName : Boolean;
begin
  Result := (ProcessState = psIdle);
  if Result then begin
    ChangeState(psCmd);
    SendCommand(fcSYST);
  end;
end;


{ TIpFtpFileInfo }
constructor TIpFtpFileInfo.Create(const Info : string; Style : TIpFtpDirStyle;
                                UserParse : TIpUserParseStyleEvent);
var
  SL : TStringList;
begin
  inherited Create;
  FFileType := ftUnknown;
  FPermissions.Owner := [];
  FPermissions.Group := [];
  FPermissions.Other := [];
  FLinkPath := '';
  FOwner := 'Unknown';
  FGroup := 'Unknown';
  FSize := 0;
  FTimeStamp := '';

  { let the user parse it if they want }
  if Assigned(UserParse) then begin
    UserParse(Self, Info, Self);
    Exit;
  end;

  { otherwise we'll do it }
  if (Style = dsNone) or (Style = dsUser) then
    FFileName := Info
  else begin
    FFileName := '';
    SL := Parse(Info, ' ');
    try
      if (SL.Count > 0) then
        case Style of
          dsUnix     : UnixStyleParse(SL);
          dsDos      : DosStyleParse(SL);
          dsMultiNet : MultiNetStyleParse(SL);
          dsVms      : VmsStyleParse(SL);
        end;
    finally
      SL.Free;
    end;
  end;
end;

destructor TIpFtpFileInfo.Destroy;
begin
  FOwner := '';
  FGroup := '';
  FFilename := '';
  FTimeStamp := '';
  inherited Destroy;
end;

{ Set properties from Dos style line of directory listing }
procedure TIpFtpFileInfo.DosStyleParse(Info : TStringList);
var
  cnt : integer;                                                       {!!.03}
begin
  {File Type, Size, Time Stamp, File Name}
  // if it has more than that it has a name with spaces
  if( Info.Count > 4 )then begin                                       {!!.03}
    if (UpperCase(Info[2]) = '<DIR>') then begin                       {!!.03}
      FFileType := ftDir;                                              {!!.03}
      FSize := 0;                                                      {!!.03}
      FTimeStamp := Info[0] + ' ' + Info[1];                           {!!.03}
      FFileName := '';                                                 {!!.03}
      for cnt := 3 to pred( Info.Count )do begin                       {!!.03}
        FFileName := FFileName + Info[ cnt ];                          {!!.03}
        if( cnt <> pred( Info.Count ))then                             {!!.03}
          FFileName := FFileName + ' ';                                {!!.03}
      end;                                                             {!!.03}
    end else begin                                                     {!!.03}
      if (StrToIntDef(Info[2], -1) > -1) then begin                    {!!.03}
        FFileType := ftFile;                                           {!!.03}
        FSize := StrToInt(Info[2]);                                    {!!.03}
        FTimeStamp := Info[0] + ' ' + Info[1];                         {!!.03}
        FFileName := '';                                               {!!.03}
        for cnt := 3 to pred( Info.Count )do begin                     {!!.03}
          FFileName := FFileName + Info[ cnt ];                        {!!.03}
          if( cnt <> pred( Info.Count ))then                           {!!.03}
            FFileName := FFileName + ' ';                              {!!.03}
        end;                                                           {!!.03}
      end;                                                             {!!.03}
    end;                                                               {!!.03}
  end else if (Info.Count = 4) then begin
    if (UpperCase(Info[2]) = '<DIR>') then begin
      FFileType := ftDir;
      FSize := 0;
      FTimeStamp := Info[0] + ' ' + Info[1];
      FFileName := Info[3];
    end else begin
      if (StrToIntDef(Info[2], -1) > -1) then begin
        FFileType := ftFile;
        FSize := StrToInt(Info[2]);
        FTimeStamp := Info[0] + ' ' + Info[1];
        FFileName := Info[3];
      end;
    end;
  end else if (Info.Count = 2) then begin
    if (StrToIntDef(Info[0], -1) > -1) then begin
      FFileType := ftFile;
      FSize := StrToInt(Info[0]);
      FTimeStamp := '';
      FFileName := Info[1];
    end;
  end;
end;

{ Set properties from Multinet style line of directory listing }
function TIpFtpFileInfo.GetDateTime: TDateTime;
begin
  result := StrToDateTime(FTimeStamp, gFormatSettings);
end;

procedure TIpFtpFileInfo.MultiNetStyleParse(Info : TStringList);
var
  SL : TStringList;
  S : string;
begin
  if CharPos(':', Info[0]) > 0 then                                    {!!.02}
    Exit;
  S := Info[0];
  if Pos('.DIR', UpperCase(Info[0])) > 0 then begin
    FFileType := ftDir;
    FFileName := Copy(Info[0], 1, CharPos('.', Info[0]) - 1);          {!!.02}
  end else if (CharPos(';', Info[0]) > 0) then begin                   {!!.02}
    FFileType := ftFile;
    FFileName := Copy(Info[0], 1, CharPos(';', Info[0]) - 1)           {!!.02}
  end;
  if (Info.Count > 2) then
    FTimeStamp := Info[1] + ' ' + Info[2];
  if (Info.Count > 3) then
    FSize := StrToIntDef(Info[3], 0);

  if (Info.Count > 4) then begin
    SL := Parse(Info[4], ',');
    if (SL.Count > 1) then begin
      if CharPos('R', UpperCase(SL[1])) > 0 then                       {!!.02}
        FPermissions.Owner := FPermissions.Owner + [faRead];
      if CharPos('W', UpperCase(SL[1])) > 0 then                       {!!.02}
        FPermissions.Owner := FPermissions.Owner + [faWrite];
      if CharPos('E', UpperCase(SL[1])) > 0 then                       {!!.02}
        FPermissions.Owner := FPermissions.Owner + [faExecute];
    end;
    if (SL.Count > 2) then begin
      if CharPos('R', UpperCase(SL[2])) > 0 then                       {!!.02}
        FPermissions.Group := FPermissions.Group + [faRead];
      if CharPos('W', UpperCase(SL[2])) > 0 then                       {!!.02}
        FPermissions.Group := FPermissions.Group + [faWrite];
      if CharPos('E', UpperCase(SL[2])) > 0 then                       {!!.02}
        FPermissions.Group := FPermissions.Group + [faExecute];
    end;
    SL.Free;
  end;
end;

{ Set properties from Unix style line of directory listing }
procedure TIpFtpFileInfo.UnixStyleParse(Info : TStringList);
var
  S, A : string;
  i : Integer;
begin

  {File Type}
  S := Info[0];
  case UpCase(S[1]) of
   '-' : FFileType := ftFile;
   'D' : FFileType := ftDir;
//   'L' : FFileType := ftLink;
   'L' : FFileType := ftDir;
  else
   FFileType := ftUnknown;
  end;

  {Owner Permissions}
  FPermissions.Owner := [];
  A := UpperCase(Copy(S, 2, 3));
  if CharPos('R', A) > 0 then                                          {!!.02}
    FPermissions.Owner := FPermissions.Owner + [faRead];
  if CharPos('W', A) > 0 then                                          {!!.02}
    FPermissions.Owner := FPermissions.Owner + [faWrite];
  if CharPos('X', A) > 0 then                                          {!!.02}
    FPermissions.Owner := FPermissions.Owner + [faExecute];

  {Group Permissions}
  FPermissions.Group := [];
  A := UpperCase(Copy(S, 5, 3));
  if CharPos('R', A) > 0 then                                          {!!.02}
    FPermissions.Group := FPermissions.Group + [faRead];
  if CharPos('W', A) > 0 then                                          {!!.02}
    FPermissions.Group := FPermissions.Group + [faWrite];
  if CharPos('X', A) > 0 then                                          {!!.02}
    FPermissions.Group := FPermissions.Group + [faExecute];

  {Other Permissions}
  FPermissions.Other := [];
  A := UpperCase(Copy(S, 8, 3));                                       {!!.02}
  if CharPos('R', A) > 0 then
    FPermissions.Other := FPermissions.Other + [faRead];
  if CharPos('W', A) > 0 then                                          {!!.02}
    FPermissions.Other := FPermissions.Other + [faWrite];
  if CharPos('X', A) > 0 then                                          {!!.02}
    FPermissions.Other := FPermissions.Other + [faExecute];

  {Owner}
  if (Info.Count > 2) then
    FOwner := Info[2];

  {Group}
  if (Info.Count > 3) then
    FGroup := Info[3];

  {Size}
  if (Info.Count > 4) then
    FSize  := StrToIntDef(Info[4], 0);

  {Time stamp}
  if (Info.Count > 7) then
  FTimeStamp  := Info[5] + ' ' + Info[6] + ' ' + Info[7];

  {File name}
  FFileName := '';
  if (Info.Count > 8) then
    for i := 8 to Pred(Info.Count) do begin
      if(Info[i] = '->')then
        break;
      if (FFileName <> '') then
        FFileName := FFileName + ' ';
      FFilename := FFileName + Info[i];
    end;

  {Symbolic Link}
  FLinkPath := '';
  if (FFileType = ftLink) then begin
    for i := 0 to Pred(Info.Count) do
      if (Info[i] = '->') then
        break;
    if (i <= (Info.Count - 2)) then
      FLinkPath := Info[i + 1];
  end;
end;

{ Set properties from VMS style line of directory listing }
procedure TIpFtpFileInfo.VMSStyleParse(Info : TStringList);
var
  SL : TStringList;
  S : string;
begin
  if CharPos(':', Info[0]) > 0 then                                    {!!.02}
    Exit;
  S := Info[0];
  if Pos('.DIR', UpperCase(Info[0])) > 0 then begin
    FFileType := ftDir;
    FFileName := Copy(Info[0], 1, CharPos('.', Info[0]) - 1);          {!!.02}
  end else if (CharPos(';', Info[0]) > 0) then begin                   {!!.02}
    FFileType := ftFile;
    FFileName := Copy(Info[0], 1, CharPos(';', Info[0]) - 1)           {!!.02}
  end;

  if (Info.Count > 1) then
    FSize :=
      StrToIntDef(Copy(Info[1], 1, CharPos('/', Info[1]) - 1), 0) * 512; {!!.11}
  if (Info.Count > 3) then
    FTimeStamp := Info[2] + ' ' + Info[3];
  if (Info.Count > 4) then begin
    SL := Parse(Info[4], ',');                                        {!!.11}
    if SL.Count > 0 then FOwner := SL[0];                             {!!.11}
    if SL.Count > 1 then Fgroup := SL[1];                             {!!.11}
  end;

  if (Info.Count > 5) then begin
    SL := Parse(Info[5], ',');
    if (SL.Count > 1) then begin
      if CharPos('R', UpperCase(SL[0])) > 0 then                      {!!.11}
        FPermissions.Owner := FPermissions.Owner + [faRead];
      if CharPos('W', UpperCase(SL[0])) > 0 then                      {!!.11}
        FPermissions.Owner := FPermissions.Owner + [faWrite];
      if CharPos('E', UpperCase(SL[0])) > 0 then                      {!!.11}
        FPermissions.Owner := FPermissions.Owner + [faExecute];
    end;
    if (SL.Count > 2) then begin
      if CharPos('R', UpperCase(SL[1])) > 0 then                      {!!.11}
        FPermissions.Group := FPermissions.Group + [faRead];
      if CharPos('W', UpperCase(SL[1])) > 0 then                      {!!.11}
        FPermissions.Group := FPermissions.Group + [faWrite];
      if CharPos('E', UpperCase(SL[1])) > 0 then                      {!!.11}
        FPermissions.Group := FPermissions.Group + [faExecute];
    end;
    if (SL.Count > 3) then begin                                      {!!.11}
      if CharPos('R', UpperCase(SL[2])) > 0 then                      {!!.11}
        FPermissions.Other := FPermissions.Other + [faRead];          {!!.11}
      if CharPos('W', UpperCase(SL[2])) > 0 then                      {!!.11}
        FPermissions.Other := FPermissions.Other + [faWrite];         {!!.11}
      if CharPos('E', UpperCase(SL[2])) > 0 then                      {!!.11}
        FPermissions.Other := FPermissions.Other + [faExecute];       {!!.11}
    end;
    SL.Free;
  end;
end;


{ TIpFtpFileList }
destructor TIpFtpFileList.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

{ Clear the file list }
procedure TIpFtpFileList.ClearItems;
begin
  while (Count > 0) do begin
    TIpFtpFileInfo(Items[0]).Free;
    Delete(0);
  end;
end;

{ Build file list from raw directory listing }
function TIpFtpFileList.Populate(Info : string; Style : TIpFtpDirStyle;
                               UserParse : TIpUserParseStyleEvent) : Boolean;
var
  SL : TStringList;
  i : Integer;
  FI : TIpFtpFileInfo;
begin
  ClearItems;
  SL := Parse(Info, IpCRLF);
  if (SL.Count > 0) then
    for i := 0 to Pred(SL.Count) do begin
      FI := TIpFtpFileInfo.Create(SL[i], Style, UserParse);
      if Assigned(FI) then begin
        if (FI.FileType = ftUnknown) and (Style <> dsNone) then
          FI.Free
        else if (Length(FI.Filename) > 0) then begin
          if (FI.Filename[1] = '.') then
            FI.Free
          else
            Add(FI);
        end;
      end;
    end;
  SL.Free;
  Result := (Count > 0);
end;


{ TIpFtpDirectoryTree }
constructor TIpFtpDirectoryTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileList := TIpFtpFileList.Create;
  FListBox := nil;
  FDirectoryStyle := dsUNIX;
  FUseDefaultImages := True;
  ClearingItems := False;
end;

destructor TIpFtpDirectoryTree.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

{ Add node to the directory tree }
procedure TIpFtpDirectoryTree.AddDirectoryNode(const Path : string);
var
  SL : TStringList;
  Node : TTreeNode;
  i, j : Integer;
begin
  if (Path <> '') then begin                                           {!!.01}
    if Path[Length(Path)] = '\' then                                   {!!.01}
      FCurrentDir := Copy(Path, 1, Length(Path) - 1)                   {!!.01}
    else                                                               {!!.01}
      FCurrentDir := Path;
  end;                                                                 {!!.01}

  if (FDirectoryStyle = dsVMS) or (FDirectoryStyle = dsMultiNet) then
    if CharPos(':', Path) > 0 then begin                               {!!.02}
      i := CharPos('[', Path) + 1;                                     {!!.02}
      j := CharPos(']', Path);                                         {!!.02}
      FCurrentDir := Copy(Path, i, j - i);                             {!!.02}
      i := CharPos('.', FCurrentDir);
      if (i > 0) then
        FCurrentDir := Copy(FCurrentDir, i+1, Length(FCurrentDir));
    end;

  if CharPos('\', FCurrentDir) = 0 then begin                          {!!.02}
    SL := TStringList.Create;                                          {!!.01}
    if FCurrentDir <> '' then                                          {!!.01}
      SL.Add(FCurrentDir);                                             {!!.01}
  end else
    SL := Parse(FCurrentDir, '\');

  if (SL.Count > 0) then begin
    Node := FindDirectoryNode(FCurrentDir);
    if not Assigned(Node) then
      i := 0
    else
      i := Node.Level + 1;
    if (i < SL.Count) then
      for j := i to Pred(SL.Count) do
        Node := Items.AddChild(Node, SL[j]);
  end;
  SL.Free;
end;

{ Selected node changed, get directory listing for new selected node }
procedure TIpFtpDirectoryTree.Change(Node: TTreeNode);
begin
  if Assigned(FFtpClient) then
    if (not ClearingItems) then begin
      if (FDirectoryStyle = dsVMS) or (FDirectoryStyle = dsMultiNet) then
        FFtpClient.ChangeDir(GetVMSPath(Node))
      else
        FFtpClient.ChangeDir(GetFullPath(Node));
      if Assigned(OnChange) then
        OnChange(Self, Node);
    end;
end;

{ Clear the directory tree }
procedure TIpFtpDirectoryTree.Clear;
begin
  ClearingItems := True;
  Items.Clear;
  if Assigned(FListBox) then
    FListBox.Clear;
  ClearingItems := False;
end;

{ Locate node that corresponds to given path }
function TIpFtpDirectoryTree.FindDirectoryNode(const Path : string) : TTreeNode;
var
  SL : TStringList;
  i : Integer;
  Node : TTreeNode;
begin
  Result := nil;
  if (FDirectoryStyle = dsVMS) or (FDirectoryStyle = dsMultiNet) then
    SL := Parse(Path, '.')
  else
    SL := Parse(Path, '\');
  if (SL.Count > 0) then begin
    Node := Items.GetFirstNode;
    for i := 0 to Pred(SL.Count) do begin
      while (Node <> nil) do begin
        if Node.Text = SL[i] then
          Break
        else
          Node := Node.GetNextSibling;
      end;
      if (Node = nil) then
        Break
      else begin
        Result := Node;
        Node := Node.GetFirstChild;
      end;
    end;
  end;
  SL.Free;
end;

{ Extract fully qualified Dos or Unix pathname of a given node }
function TIpFtpDirectoryTree.GetFullPath(Node : TTreeNode) : string;
var
  TN : TTreeNode;
begin
  Result := '';
  TN := Node;
  if (TN = nil) then
    Exit;
  repeat
    Result := '\' + TN.Text + Result;
    TN := TN.Parent;
  until (TN = nil);
end;

{ Extract fully qualified Vms pathname of a given node }
function TIpFtpDirectoryTree.GetVMSPath(Node : TTreeNode) : string;
var
  TN : TTreeNode;
  S : string;
begin
  Result := '';
  if (Node = nil) then
    Exit
  else if Node.Level = 0 then
    Exit;
  Result := Node.Text;
  TN := Node.Parent;
  if Assigned(TN) then
    while (TN.Level > 0) do begin
      Result := TN.Text + '\' + Result;
      TN := TN.Parent;
    end;

  if CharPos('\', Result) > 0 then                                     {!!.02}
    Result := '\' + Result
  else if (S = '') or (Length(S) < Length(FCurrentDir)) then
    if CharPos('.', FCurrentDir) > 0 then                              {!!.02}
      Result := '..';
end;

{ Handle the GetImageIndex event }
procedure TIpFtpDirectoryTree.GetImageIndex(Node: TTreeNode);
begin
  Node.SelectedIndex := Integer(Node.Selected);                        {!!.02}
end;

{ Handle the GetSelectedImageIndex event }
procedure TIpFtpDirectoryTree.GetSelectedIndex(Node: TTreeNode);
begin
  Node.SelectedIndex := Integer(Node.Selected);                        {!!.02}
end;

{ Initialize default folder images }
procedure TIpFtpDirectoryTree.Loaded;
var
  TempImages : TImageList;
  TempIcon : TIcon;
  SHFileInfo : TSHFileInfo;
  WinDir : array [0..MAX_PATH] of Char;
begin
  if not (csDesigning in ComponentState) then
    if (not Assigned(Images)) and UseDefaultImages then begin
      TempIcon := TIcon.Create;
      TempImages := TImageList.Create(nil);
      Images := TImageList.Create(Self);
      try
        TempImages.ShareImages := True;
        GetWindowsDirectory(WinDir, SizeOf(WinDir));
        TempImages.Handle := SHGetFileInfo(WinDir, 0, SHFileInfo,
          SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
        TempImages.GetIcon(SHFileInfo.iIcon, TempIcon);
        Images.AddIcon(TempIcon);
        TempIcon.ReleaseHandle;
        SHGetFileInfo(WinDir, 0, SHFileInfo, SizeOf(TSHFileInfo),
          SHGFI_SYSICONINDEX or SHGFI_OPENICON or SHGFI_SMALLICON);
        TempImages.GetIcon(SHFileInfo.iIcon, TempIcon);
        Images.AddIcon(TempIcon);
      finally
        TempIcon.Free;
        TempImages.Free;
      end;
    end;
  inherited Loaded;
end;

{ Notification of new/deleted Ftp client component }
procedure TIpFtpDirectoryTree.Notification(AComponent : TComponent;
                                           Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent = FFtpClient) then
      FFtpClient := nil;
  end else if (Operation = opInsert) then begin
    if (AComponent is TIpFtpClient) then begin
      if not Assigned(FFtpClient) then
        if not Assigned(TIpFtpClient(AComponent).DirectoryTree) then begin
          FFtpClient := TIpFtpClient(AComponent);
          TIpFtpClient(FFtpClient).DirectoryTree := Self;
        end;
    end;
  end;
end;

{ Refresh child nodes of the current directory node }
procedure TIpFtpDirectoryTree.PopulateFileList(Text : string);
var
  Info : TIpFtpFileInfo;
  i : Integer;
  Node : TTreeNode;
begin
  if Assigned(FListBox) then
    FListBox.Clear;
  FFileList.Populate(Text, FDirectoryStyle, FOnParseStyle);
  Node := FindDirectoryNode(FCurrentDir);
  if Assigned(Node) then
    Node.DeleteChildren
  else                                                                 {!!.01}
    Clear;                                                             {!!.01}
  for i := 0 to Pred(FFileList.Count) do begin
    Info := TIpFtpFileInfo(FFileList.Items[i]);
    if Assigned(Info) then begin
      if (Info.FileType = ftDir) or (FDirectoryStyle = dsNone) then
        if (Length(Info.FileName) > 0) then
          if (Info.FileName[1] <> '.') then
        Items.AddChild(Node, Info.Filename);
      if Assigned(FListBox) then
        FListBox.Items.AddObject(Info.Filename, Info);
    end;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ DirectoryStyle property write access method }
procedure TIpFtpDirectoryTree.SetDirectoryStyle(Style : TIpFtpDirStyle);
begin
  if FDirectoryStyle <> Style then
    FDirectoryStyle := Style;
end;

{ OnParseStyle event write access method }
procedure TIpFtpDirectoryTree.SetOnParseStyle(Value : TIpUserParseStyleEvent);
begin
  FOnParseStyle := Value;
  if Assigned(FOnParseStyle) then
    FDirectoryStyle := dsUser;
end;

function TIpFtpDirectoryTree.GetVersion : string;
begin
  Result := IpShortVersion;
end;

procedure TIpFtpDirectoryTree.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;


initialization
  GetLocaleFormatSettings(GetSystemDefaultLCID, gFormatSettings);
  gFormatSettings.DateSeparator := '-';
end.
