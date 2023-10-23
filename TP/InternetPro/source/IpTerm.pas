{******************************************************************}
{*     IPTERM.PAS - Telnet terminal and related classes           *}
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

{ Options required for this unit }
{$Z-}

{$IFOPT R+}
{$DEFINE UseRangeChecks}
{$ENDIF}

{.$DEFINE TermDebug}

{$R IPTRMVT1.R32}
{$R IPCHSVT1.R32}

unit IpTerm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  Forms, Dialogs, ClipBrd, IpUtils, IpConst, IpSock;

type
  TIpCaptureMode = ( { terminal data capture modes.. }
          cmOff,     { ..no capturing of data }
          cmOn,      { ..capture new data to new file }
          cmAppend); { ..capture data, appending to old file }

  TIpTerminalCursorMovedEvent =
     procedure (aSender : TObject; aRow, aCol : Integer) of object;

  TIpCursorType = (  { cursor types }
    ctNone,          { ..no cursor is visible }
    ctUnderline,     { ..underline cursor }
    ctBlock);        { ..block cursor }

  TIpVT100LineAttr = ( { special VT100 line attributes }
    ltNormal,          { ..normal, no special attrs }
    ltDblHeightTop,    { ..line is top half of double-height line }
    ltDblHeightBottom, { ..line is bottom half of double-height line }
    ltDblWidth);       { ..line is double-width }

const
  {Emulator commands}
  eNone               = 0;     { No command, ignore this char }
  eChar               = 1;     { No command, process the char }
  eGotoXY             = 2; {X} { Absolute goto cursor position call }
  eUp                 = 3; {X} { Cursor up }
  eDown               = 4; {X} { Cursor down }
  eRight              = 5; {X} { Cursor right }
  eLeft               = 6; {X} { Cursor left }
  eClearBelow         = 7; {R} { Clear screen below cursor }
  eClearAbove         = 8; {R} { Clear screen above cursor }
  eClearScreen        = 9; {R} { Clear entire screen }
  eClearEndofLine     = 10;{R} { Clear from cursor to end of line }
  eClearStartOfLine   = 11;{R} { Clear from cursor to the start of line }
  eClearLine          = 12;{R} { Clear entire line that cursor is on }
  eSetMode            = 13;{X} { Set video mode }
  eSetBackground      = 14;    { Set background attribute }
  eSetForeground      = 15;    { Set foreground attribute }
  eSetAttribute       = 16;{X} { Set video attribute (foreground and background) }
  eSaveCursorPos      = 17;    { Save cursor position }
  eRestoreCursorPos   = 18;    { Restore cursor position }
  eDeviceStatusReport = 19;{X} { Report device status or cursor position }
  eString             = 20;    { Pascal style string }
  eHT                 = 21;    { Horizontal Tab Character }
  eError              = 255;   { indicates a parser error }

  eAPC  { } = 30;                  { Application programming command }
  eBEL  {X} = 115;                 { Sound bell }
  eBS   {X} = 116;                 { Backspace }
  eCBT  {X} = 31;                  { Cursor backward tabulation }
  eCCH  { } = 32;                  { Cancel character }
  eCHA  {X} = 33;                  { Cursor horizontal absolute }
  eCHT  {X} = 34;                  { Cursor horizontal tabulation }
  eCNL  {X} = 35;                  { Cursor next line }
  eCPL  {X} = 36;                  { Cursor preceding line }
  eCPR  {X} = 37;                  { Cursor position report }
  eCR   {X} = 118;                 { Carriage return }
  eCRM  {.} = 38;                  { Control representation mode }
  eCTC  {X} = 39;                  { Cursor tabulation control }
  eCUB  {X} = eLeft;               { Cursor backward }
  eCUD  {X} = eDown;               { Cursor down }
  eCUF  {X} = eRight;              { Cursor forward }
  eCUP  {X} = eGotoXY;             { Cursor position }
  eCUU  {X} = eUp;                 { Cursor up }
  eCVT  {X} = 40;                  { Cursor vertical tabulation }
  eDA   {X} = 41;                  { Device attributes }
  eDAQ  { } = 42;                  { Define area qualification }
  eDCH  {X} = 43;                  { Delete character }
  eDCS  { } = 44;                  { Device control string }
  eDL   {X} = 45;                  { Delete line }
  eDMI  { } = 46;                  { Disable manual input }
  eDSR  {X} = eDeviceStatusReport; { Device status report }
  eEA   { } = 47;                  { Erase in area }
  eEBM  { } = 48;                  { Editing boundry mode }
  eECH  {X} = 49;                  { Erase character }
  eED   {X} = 50;                  { Erase in Display }
  eEF   { } = 51;                  { Erase in field }
  eEL   {X} = 52;                  { Erase in line }
  eEMI  { } = 53;                  { Enable manual input }
  eENQ  {X} = 114;                 { Enquiry request }
  eEPA  { } = 54;                  { End of protected mode }
  eERM  { } = 55;                  { Erasure mode }
  eESA  { } = 56;                  { End of selected area }
  eFEAM { } = 57;                  { Format effector action mode }
  eFETM { } = 58;                  { Format effector transfer mode }
  eFNT  { } = 59;                  { Font selection }
  eGATM { } = 60;                  { Guarded area transfer mode }
  eGSM  { } = 61;                  { Graphics size modification }
  eGSS  { } = 62;                  { Graphics size selection }
  eHEM  { } = 63;                  { Horizontal editing mode }
  eHPA  {X} = eCHA;                { Horizontal position absolute }
  eHPR  {X} = eCUF;                { Horizontal position relative }
  eHTJ  {X} = 64;                  { Horizontal tab with justification }
  eHTS  {X} = 65;                  { Horizontal tabulation set }
  eHVP  {X} = eCUP;                { Horizontal and vertical position }
  eICH  {X} = 66;                  { Insert character }
  eIL   {X} = 67;                  { Insert line }
  eIND  {X} = eCUD;                { Index }
  eIND2 {X} = 121;                 { Corrected eIND (<> eCUD, eDown) new term only }
  eINT  { } = 68;                  { Interrupt }
  eIRM  {.} = 69;                  { Inseration-Replacement mode }
  eJFY  { } = 70;                  { Justify }
  eKAM  {.} = 71;                  { Keyboard action mode }
  eLF   {X} = 117;                 { Line feed command }
  eLNM  {.} = 72;                  { Line feed new line mode }
  eMATM { } = 73;                  { Multiple area transfer mode }
  eMC   {.} = 74;                  { Media copy }
  eMW   {.} = 75;                  { Message waiting }
  eNEL  {X} = 76;                  { Next line }
  eNP   {.} = 77;                  { Next page }
  eOSC  { } = 78;                  { Operating system command }
  ePLD  { } = 79;                  { Partial line down }
  ePLU  { } = 80;                  { Partial line up }
  ePM   { } = 81;                  { Privacy message }
  ePP   {.} = 82;                  { Preceding page }
  ePU1  { } = 83;                  { Private use 1 }
  ePU2  { } = 84;                  { Private use 2 }
  ePUM  { } = 85;                  { Positioning unit mode }
  eQUAD { } = 86;                  { Quad }
  eREP  { } = 87;                  { Repeat }
  eRI   {X} = 88;                  { Reverse index }
  eRIS  {.} = 89;                  { Reset to initial state }
  eRM   {.} = 90;                  { Reset mode }
  eSATM { } = 91;                  { Selected area transfer mode }
  eSD   { } = 92;                  { Scroll down }
  eSEM  { } = 93;                  { selected editing extent mode }
  eSGR  {X} = eSetAttribute;       { Select graphics rendition }
  eSI   {X} = 120;                 { Invoke G0 charset }
  eSL   { } = 94;                  { Scroll left }
  eSM   {.} = eSetMode;            { Set Mode }
  eSO   {X} = 119;                 { Invoke G1 charset }
  eSPA  { } = 95;                  { Start of protected area }
  eSPI  { } = 96;                  { Spacing increment }
  eSR   { } = 97;                  { Scroll right }
  eSRM  { } = 98;                  { Send-Receive mode }
  eSRTM { } = 99;                  { Status report transfer mode }
  eSS2  { } = 100;                 { Single shift 2 }
  eSS3  { } = 101;                 { Single shift 3 }
  eSSA  { } = 102;                 { Start of selected area }
  eST   { } = 103;                 { String terminator }
  eSTS  { } = 104;                 { Set transmit state }
  eSU   { } = 105;                 { Scroll up }
  eTBC  {X} = 106;                 { Tabulation clear }
  eTSM  { } = 107;                 { Tabulation stop mode }
  eTSS  { } = 108;                 { Thin space specification }
  eTTM  { } = 109;                 { Transfer termination mode }
  eVEM  { } = 110;                 { Vertical editing mode }
  eVPA  {X} = 111;                 { Vertical position absolute }
  eVPR  {X} = eCUD;                { Vertical position relative }
  eVTS  {X} = 112;                 { vertical tabulation set }

  eDECALN      = 122;     { DEC PRIVATE-screen alignment display }
  eDECDHL      = 123;     { DEC PRIVATE-Double height line }
  eDECDWL      = 124;     { DEC PRIVATE-Double width line }
  eDECLL       = 125;     { DEC PRIVATE-load LEDs }
  eDECREQTPARM = 126;     { DEC PRIVATE-request terminal parameters }
  eDECSCS      = 129;     { DEC PRIVATE-select charset }
  eDECSTBM     = 113;     { DEC PRIVATE-set Top/Bottom margin }
  eDECSWL      = 127;     { DEC PRIVATE-single width line }
  eDECTST      = 128;     { DEC PRIVATE-Invoke confidence test }

const
  { Convenient character constants (and aliases) }
  cNul = #0;
  cSoh = #1;
  cStx = #2;
  cEtx = #3;
  cEot = #4;
  cEnq = #5;
  cAck = #6;
  cBel = #7;
  cBS  = #8;
  cTab = #9;
  cLF  = #10;
  cVT  = #11;
  cFF  = #12;
  cCR  = #13;
  cSO  = #14;
  cSI  = #15;
  cDle = #16;
  cDC1 = #17;       cXon  = #17;
  cDC2 = #18;
  cDC3 = #19;       cXoff = #19;
  cDC4 = #20;
  cNak = #21;
  cSyn = #22;
  cEtb = #23;
  cCan = #24;
  cEM  = #25;
  cSub = #26;
  cEsc = #27;
  cFS  = #28;
  cGS  = #29;
  cRS  = #30;
  cUS  = #31;

const
  { Buffer default property values }
  ipc_TermBufForeColor = clSilver;
  ipc_TermBufBackColor = clBlack;
  ipc_TermBufColCount = 80;
  ipc_TermBufRowCount = 24;
  ipc_TermBufScrollRowCount = 200;
  ipc_TermBufUseAbsAddress = True;    { use absolute rows/col values }
  ipc_TermBufUseAutoWrap = True;      { chars wrap at end of line }
  ipc_TermBufUseAutoWrapDelay = True; { ..but delay slightly }
  ipc_TermBufUseInsertMode = False;   { no insert mode }
  ipc_TermBufUseNewLineMode = False;  { LF char is a pure linefeed }
  ipc_TermBufUseScrollRegion = False; { no scroll region used }

const
  { default property values for the terminal }
  ipc_TermActive = True;
  ipc_TermBackColor = ipc_TermBufBackColor;
  ipc_TermBlinkTime = 500;
  ipc_TermBorderStyle = bsSingle;
  ipc_TermCapture = cmOff;
  ipc_TermCaptureFile = 'IPROTERM.CAP';
  ipc_TermColumnCount = ipc_TermBufColCount;
  ipc_TermCursorType = ctBlock;
  ipc_TermFontName = 'Terminal';
  ipc_TermForeColor = ipc_TermBufForeColor;
  ipc_TermHalfDuplex = False;
  ipc_TermHeight = 200;
  ipc_TermLazyByteDelay = 200;
  ipc_TermLazyTimeDelay = 100;
  ipc_TermRowCount = ipc_TermBufRowCount;
  ipc_TermScrollback = False;
  ipc_TermScrollRowCount = ipc_TermBufScrollRowCount;
  ipc_TermUseLazyDisplay = True;
  ipc_TermWantAllKeys = True;
  ipc_TermWidth = 300;
  ipc_FreezeScrollback = true;                                         {!!.03}

  { default property values for the VT100 emulator--'power-up' values }
  ipc_VT100ANSIMode = True;
  ipc_VT100Answerback = 'iPROterm';
  ipc_VT100AppKeyMode = False;
  ipc_VT100AppKeypadMode = False;
  ipc_VT100AutoRepeat = True;
  ipc_VT100Col132Mode = False;
  ipc_VT100G0CharSet = 0;
  ipc_VT100G1CharSet = 0;
  ipc_VT100GPOMode = False;
  ipc_VT100InsertMode = ipc_TermBufUseInsertMode;
  ipc_VT100Interlace = False;
  ipc_VT100NewLineMode = ipc_TermBufUseNewLineMode;
  ipc_VT100RelOriginMode = not ipc_TermBufUseAbsAddress;
  ipc_VT100RevScreenMode = False;
  ipc_VT100SmoothScrollMode = False;
  ipc_VT100WrapAround = ipc_TermBufUseAutoWrap;

type
  TIpSockControlFriend = class(TIpSockControl);

  PIptWordArray = ^TIptWordArray;
  TIptWordArray = array [0..MaxInt div sizeof(Word) - 1] of Word;

  PIptLongArray = ^TIptLongArray;
  TIptLongArray = array [0..MaxInt div sizeof(Longint) - 1] of Longint;

  TIpTerminalCharAttr = (  { character attributes }
         tcaBold,          { ..bold }
         tcaUnderline,     { ..underlined }
         tcaStrikethrough, { ..struck through as if deleted }
         tcaBlink,         { ..blinking }
         tcaReverse,       { ..back/foreground colors reversed }
         tcaInvisible,     { ..invisible }
         tcaSelected);     { ..selected }

  TIpTerminalCharAttrs = set of TIpTerminalCharAttr;

  TIpScrollRowsNotifyEvent = procedure (aSender : TObject;
    aCount, aTop, aBottom : Integer) of object;
    
  TIpOnCursorMovedEvent =                                              {!!.01}
     procedure (ASender  : TObject;                                    {!!.01}
                Row, Col : integer) of object;                         {!!.01}

  TIpTerminalArray = class
  private
    FActColCount : Integer;
    FColCount    : Integer;
    FDefaultItem : Longint;
    FItems       : PAnsiChar;
    FItemSize    : Integer;
    FRowCount    : Integer;
  protected
    procedure taSetColCount(aNewCount : Integer);
    procedure taSetRowCount(aNewCount : Integer);
    procedure taClearRows(aBuffer : PAnsiChar; aActColCount : Integer;
      aStartRow, aEndRow : Integer);
    procedure taGrowArray(aRowCount, aColCount, aActColCount : Integer);
  public
    constructor Create(aItemSize : Integer);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearItems(aRow : Integer; aFromCol, aToCol : Integer);
    procedure DeleteItems(aCount : Integer; aRow : Integer; aCol : Integer);
    function GetItemPtr(aRow, aCol : Integer) : Pointer;
    procedure InsertItems(aCount : Integer; aRow : Integer; aCol : Integer);
    procedure ReplaceItems(aOldItem : pointer; aNewItem : pointer);
    procedure SetDefaultItem(aDefaultItem : Pointer);
    procedure ScrollRows(aCount : Integer; aStartRow, aEndRow : Integer);
    procedure WriteItems(aItems : Pointer; aCount : Integer;
      aRow, aCol : Integer);
    procedure WriteDupItems(aItem  : Pointer; aCount : Integer;
      aRow, aCol : Integer);
    property ColCount : Integer read FColCount write taSetColCount;
    property ItemSize : Integer read FItemSize;
    property RowCount : Integer read FRowCount write taSetRowCount;
  end;

type

  TIpTerminalBuffer = class
  private
    FAttr         : TIpTerminalCharAttrs; { current attributes }
    FBackColor    : TColor;   { current background color }
    FBeyondMargin : Boolean;  { True if cursor's beyond right margin }
    FCharSet      : Byte;     { current charset }
    FColCount     : Integer;  { count of columns in both views }
    FCursorCol    : Integer;  { current internal cursor col position }
    FCursorMoved  : Boolean;  { True if cursor has moved }
    FCursorRow    : Integer;  { current internal cursor row position }
    FDefAnsiChar  : AnsiChar; { default ANSI character }
    FDefAttr      : TIpTerminalCharAttrs; { default attributes }
    FDefCharSet   : Byte;     { default charset }
    FDefBackColor : TColor;   { default background color }
    FDefForeColor : TColor;   { default foreground color }
    FDefWideChar  : WideChar; { default wide character }
    FDisplayOriginCol : Integer; { column origin of addressable area }
    FDisplayOriginRow : Integer; { row origin of addressable area }
    FDisplayColCount  : Integer; { column count in addressable area }
    FDisplayRowCount  : Integer; { row count in addressable area }
    FForeColor    : TColor;   { current foreground color }
    FHorzTabStops : PByteArray; { bitset of horizontal tab stops }
    FInvRectList  : Pointer;  { list of invalid rects }
    FOnScrollRows : TIpScrollRowsNotifyEvent;
    FRowCount     : Integer;  { count of rows in display view }
    FSRStartRow   : Integer;  { start row of scrolling region }
    FSREndRow     : Integer;  { end row of scrolling region }
    FSVRowCount   : Integer;  { count of rows in scrollback view }
    FUseAbsAddress: Boolean; { True if absolute values for row/col }
    FUseAutoWrap  : Boolean;  { True if chars wrap to next line }
    FUseAutoWrapDelay: Boolean; { True if cursor stays at last col }
    FUseInsertMode   : Boolean; { True if insert rather than replace }
    FUseNewLineMode  : Boolean; { True if LF means CR+LF }
    FUseScrollRegion : Boolean; { True if limit to scroll region }
    FUseWideChars : Boolean;  { True if expecting UNICODE chars }
    FVertTabStops : PByteArray; { bitset of vertical tab stops }

    FCharMatrix   : TIpTerminalArray;    { matrix of chars }
    FCharSetMatrix: TIpTerminalArray;    { matrix of charsets }
    FAttrMatrix   : TIpTerminalArray;    { matrix of attrs }
    FForeColorMatrix : TIpTerminalArray; { matrix of forecolors }
    FBackColorMatrix : TIpTerminalArray; { matrix of backcolors }

    FOnCursorMoved : TIpOnCursorMovedEvent; {Cursor moved event}       {!!.01}
    
  protected
    function tbGetCol : Integer;
    function tbGetOriginCol : Integer;
    function tbGetOriginRow : Integer;
    function tbGetRow : Integer;

    procedure tbSetBackColor(aValue : TColor);
    procedure tbSetCharSet(aValue : Byte);
    procedure tbSetDefAnsiChar(aValue : AnsiChar);
    procedure tbSetDefBackColor(aValue : TColor);
    procedure tbSetDefForeColor(aValue : TColor);
    procedure tbSetForeColor(aValue : TColor);
    procedure tbSetSVRowCount(aNewCount : Integer);
    procedure tbSetCol(aCol : Integer);
    procedure tbSetColCount(aNewCount : Integer);
    procedure tbSetRow(aRow : Integer);
    procedure tbSetRowCount(aNewCount : Integer);
    procedure tbSetUseScrollRegion(aValue : Boolean);

    procedure tbInvalidateRect(aFromRow, aFromCol, aToRow, aToCol : Integer);

    function tbCvtToInternalCol(aCol : Integer; aAbsolute : Boolean) : Integer;
    function tbCvtToInternalRow(aRow : Integer; aAbsolute : Boolean) : Integer;
    function tbCvtToExternalCol(aCol : Integer; aAbsolute : Boolean) : Integer;
    function tbCvtToExternalRow(aRow : Integer; aAbsolute : Boolean) : Integer;

    function tbAtLastColumn : Boolean;
    procedure tbMoveCursorLeftRight(aDirection : Integer;
      aWrap : Boolean; aScroll : Boolean);
    procedure tbMoveCursorUpDown(aDirection : Integer; aScroll : Boolean);
    procedure tbReallocBuffers(aNewRowCount : Integer; aNewColCount : Integer);
    procedure tbScrollRows(aCount, aTop, aBottom : Integer);

    procedure tbFireOnCursorMovedEvent;                                {!!.01}
    
  public
    constructor Create(aUseWideChars : Boolean);
    destructor Destroy; override;

    {---METHODS---}
    { character attributes }
    procedure GetCharAttrs(var aValue : TIpTerminalCharAttrs);
    procedure GetDefCharAttrs(var aValue : TIpTerminalCharAttrs);
    procedure SetDefCharAttrs(const aValue : TIpTerminalCharAttrs);
    procedure SetCharAttrs(const aValue : TIpTerminalCharAttrs);

    { cursor movement }
    procedure MoveCursorDown(aScroll : Boolean);
    procedure MoveCursorLeft(aWrap : Boolean; aScroll : Boolean);
    procedure MoveCursorRight(aWrap : Boolean; aScroll : Boolean);
    procedure MoveCursorUp(aScroll : Boolean);
    procedure SetCursorPosition(aRow, aCol : Integer);

    { insertion/deletion }
    procedure DeleteChars(aCount : Integer);
    procedure DeleteLines(aCount : Integer);
    procedure InsertChars(aCount : Integer);
    procedure InsertLines(aCount : Integer);

    { erasing }
    procedure EraseAll;
    procedure EraseChars(aCount : Integer);
    procedure EraseFromBOW;
    procedure EraseFromBOL;
    procedure EraseLine;
    procedure EraseScreen;
    procedure EraseToEOL;
    procedure EraseToEOW;

    { horizontal tab stop control }
    procedure SetHorzTabStop;
    procedure ClearHorzTabStop;
    procedure ClearAllHorzTabStops;
    procedure DoHorzTab;
    procedure DoBackHorzTab;

    { vertical tab stop control }
    procedure SetVertTabStop;
    procedure ClearVertTabStop;
    procedure ClearAllVertTabStops;
    procedure DoVertTab;
    procedure DoBackVertTab;

    { scrolling regions }
    procedure SetScrollRegion(aTopRow, aBottomRow : Integer);

    { write character/string }
    procedure WriteChar(aCh : char);
    procedure WriteString(const aSt : string);

    { miscellaneous special processing }
    procedure DoBackspace;
    procedure DoCarriageReturn;
    procedure DoLineFeed;
    procedure Reset;

    { get buffer information }
    function GetLineCharPtr(aRow : Integer) : Pointer;
    function GetLineAttrPtr(aRow : Integer) : Pointer;
    function GetLineForeColorPtr(aRow : Integer) : Pointer;
    function GetLineBackColorPtr(aRow : Integer) : Pointer;
    function GetLineCharSetPtr(aRow : Integer) : Pointer;

    { getting information about changes }
    function HasCursorMoved : Boolean;
    function HasDisplayChanged : Boolean;
    function GetInvalidRect(var aRect : TRect) : Boolean;

    {---PROPERTIES---}
    { color, charsets }
    property BackColor : TColor read FBackColor write tbSetBackColor;
    property CharSet : Byte read FCharSet write tbSetCharSet;
    property DefAnsiChar : AnsiChar read FDefAnsiChar write tbSetDefAnsiChar;
    property DefBackColor : TColor read FDefBackColor write tbSetDefBackColor;
    property DefCharSet : Byte read FDefCharSet write FDefCharSet;
    property DefForeColor : TColor read FDefForeColor write tbSetDefForeColor;
    property ForeColor : TColor read FForeColor write tbSetForeColor;

    { scrollback view extent }
    property SVRowCount : Integer read FSVRowCount write tbSetSVRowCount;

    { display view properties }
    property Col : Integer read tbGetCol write tbSetCol;
    property ColCount : Integer read FColCount write tbSetColCount;
    property OriginCol : Integer read tbGetOriginCol;
    property OriginRow : Integer read tbGetOriginRow;
    property Row : Integer read tbGetRow write tbSetRow;
    property RowCount : Integer read FRowCount write tbSetRowCount;

    property UseAbsAddress : Boolean
      read FUseAbsAddress write FUseAbsAddress;
    property UseAutoWrap : Boolean
      read FUseAutoWrap write FUseAutoWrap;
    property UseAutoWrapDelay : Boolean
      read FUseAutoWrapDelay write FUseAutoWrapDelay;
    property UseInsertMode : Boolean
      read FUseInsertMode write FUseInsertMode;
    property UseNewLineMode : Boolean
      read FUseNewLineMode write FUseNewLineMode;
    property UseScrollRegion : Boolean
      read FUseScrollRegion write tbSetUseScrollRegion;
    property UseWideChars : Boolean read FUseWideChars;
    property OnScrollRows : TIpScrollRowsNotifyEvent
      read FOnScrollRows write FOnScrollRows;

      { OnCursorMoved
        This property is used to notify the TAdTerminal component when
        the cursor moves.  It should not be used for your own purposes
        as that may cause unexpected behaviour in the terminal }       {!!.01}

      property OnCursorMoved : TIpOnCursorMovedEvent                   {!!.01}
                  read FOnCursorMoved write FOnCursorMoved;            {!!.01}
  end;

type
  PIpKeyString = ^TIpKeyString;
  TIpKeyString = string[63];

const
  DefaultFontName : string[9] = '<Default>';

type
  TIpKeyboardMapping = class
  private
    FTable : TList;
    FCount : Integer;
  protected
    function kbmFindPrim(const aKey : TIpKeyString; var aInx : Integer;
      var aNode : Pointer) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const aKey : TIpKeyString; const aValue : TIpKeyString) : Boolean;
    procedure Clear;
    function Get(const aKey : TIpKeyString) : TIpKeyString;
    procedure LoadFromFile(const aFileName : string);
    procedure LoadFromRes(aInstance : THandle; const aResName : string);
    procedure StoreToBinFile(const aFileName : string);
    {$IFDEF CompileDebugCode}
    procedure DebugPrint(const aFileName : string);
    {$ENDIF}
    property Count : Integer read FCount;
  end;

type
  TIpCharSetMapping = class
  private
    FTable     : TList;
    FCharQueue : Pointer;
    FCount     : Integer;
    FScript    : Pointer;
    FScriptEnd : Pointer;
    FScriptFreeList : Pointer;
  protected
    procedure csmAddScriptNode(aFont : PIpKeyString);
    function csmFindPrim(const aCharSet : TIpKeyString; aChar : AnsiChar;
      var aInx : Integer; var aNode : Pointer) : Boolean;
    procedure csmFreeScript;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const aCharSet : TIpKeyString; aFromCh : AnsiChar;
      aToCh : AnsiChar; const aFont : TIpKeyString; aGlyph : AnsiChar) : Boolean;
    procedure Clear;
    procedure GetFontNames(aList : TStrings);
    procedure GenerateDrawScript(const aCharSet : TIpKeyString; aText : PAnsiChar);
    function GetNextDrawCommand(var aFont : TIpKeyString; aText : PAnsiChar) : Boolean;
    procedure LoadFromFile(const aFileName : string);
    procedure LoadFromRes(aInstance : THandle; const aResName  : string);
    procedure StoreToBinFile(const aFileName : string);
    {$IFDEF CompileDebugCode}
    procedure DebugPrint(const aFileName : string);
    {$ENDIF}
    property Count : Integer read FCount;
  end;

type
  TIpParserCmdType = ( { Parser return command types... }
    pctNone,           { ..no command, unknown command or char ignored }
    pctChar,           { ..single displayable character }
    pct8bitChar,       { ..single character with bit 7 set }
    pctPending,        { ..command being built up }
    pctComplete);      { ..a complete command is ready }

  TIpVTParserState = (   { VT Parser states... }
    psIdle,              { ..nothing happening }
    psGotEscape,         { ..received escape char }
    psParsingANSI,       { ..parsing an ESC[ sequence }
    psParsingHash,       { ..parsing an ESC# sequence }
    psParsingLeftParen,  { ..parsing an ESC( sequence }
    psParsingRightParen, { ..parsing an ESC) sequence }
    psParsingCharSet,    { ..parsing ESC *, +, -, ., / sequence }
    psGotCommand,        { ..received complete command }
    psGotInterCommand,   { ..received complete intermediary command }
    psParsingCUP52);     { ..received VT52 position cursor command }

type
  PAdIntegerArray = ^TIpIntegerArray;
  TIpIntegerArray = array [0..pred(MaxInt div sizeof(Integer))] of Integer;

type
  TIpTerminalParser = class
  { the ancestor parser class }
  private
    FArgCount    : Integer;
    FCommand     : Byte;
    FUseWideChar : Boolean;
  protected
    function tpGetArgument(aInx : Integer) : Integer; virtual;
    function tpGetSequence : string; virtual;
  public
    constructor Create(aUseWideChar : Boolean);
    destructor Destroy; override;

    function ProcessChar(aCh : AnsiChar) : TIpParserCmdType; virtual;
    function ProcessWideChar(aCh : WideChar) :TIpParserCmdType; virtual;

    procedure Clear; virtual;

    property Argument [aInx : Integer] : Integer
       read tpGetArgument;
    property ArgumentCount : Integer read FArgCount;
    property Command : Byte read FCommand;
    property Sequence : string read tpGetSequence;
  end;

  TIpVT100Parser = class(TIpTerminalParser)
  { the VT100 terminal parser }
  private
    FArgCountMax : Integer;
    FArgs        : PAdIntegerArray;
    FInVT52Mode  : Boolean;
    FSavedSeq    : Pointer;
    FSavedState  : TIpVTParserState;
    FSequence    : Pointer;
    FState       : TIpVTParserState;
  protected
    function tpGetArgument(aInx : Integer) : Integer; override;
    function tpGetSequence : string; override;

    function vtpGetArguments : Boolean;
    function vtpParseANSISeq(aCh : char) : TIpParserCmdType;
    function vtpProcessVT52(aCh : char) : TIpParserCmdType;
    function vtpValidateArgsPrim(aMinArgs : Integer;
                                 aMaxArgs : Integer;
                                 aDefault : Integer) : Boolean;

    procedure vtpGrowArgs;
  public
    constructor Create(aUseWideChar : Boolean);
    destructor Destroy; override;

    function ProcessChar(aCh : AnsiChar) : TIpParserCmdType; override;
    function ProcessWideChar(aCh : WideChar) :TIpParserCmdType; override;

    procedure Clear; override;

    property InVT52Mode : Boolean read FInVT52Mode;
  end;

type
  TIpTerminalEmulator = class;

  TIpCustomTerminal = class(TIpBaseWinControl)
  private
    FActive           : Boolean;
    FBlinkTime        : Integer;
    FBlinkTimeCount   : Integer;
    FBlinkTextVisible : Boolean;
    FBorderStyle      : TBorderStyle;
    FByteQueue        : Pointer;
    FCapture          : TIpCaptureMode;
    FCaptureFile      : string;
    FCaptureStream    : TFileStream;
    FCanvas           : TCanvas;
    FCharHeight       : Integer;     { height of char cell in pixels }
    FCharWidth        : Integer;     { width of char cell in pixels }
    FClientCols       : Integer;     { client width in columns, incl part }
    FClientFullCols   : Integer;     { client width in *full* columns }
    FClientRows       : Integer;     { client height in rows, incl part }
    FClientFullRows   : Integer;     { client height in *full* rows }
    FCreatedCaret     : Boolean;
    FCursorType       : TIpCursorType;
    FDefEmulator      : TIpTerminalEmulator; { default = tty }
    FEmulator         : TIpTerminalEmulator;
    FHalfDuplex       : Boolean;
    FHaveSelection    : Boolean;
    FHeartbeat        : TTimer;
    FLazyByteCount    : Integer;
    FLazyTimeCount    : Integer;
    FLazyByteDelay    : Integer;
    FLazyTimeDelay    : Integer;
    FLButtonAnchor    : TPoint;
    FLButtonDown      : Boolean;
    FLButtonRect      : TRect;
    FOnCursorMoved    : TIpTerminalCursorMovedEvent;
    FOriginCol        : Integer;
    FOriginRow        : Integer;
    FScrollback       : Boolean;
    FFreezeScrollback : boolean;                                       {!!.03}
    FScrollHorzInfo   : TScrollInfo;
    FScrollVertInfo   : TScrollInfo;
    FShowingCaret     : Boolean;
    FSockControl      : TIpSockControl;
    FSocket           : TSocket;
    FUsedRect         : TRect;
    FUseLazyDisplay   : Boolean;
    FUnusedRightRect  : TRect;
    FUnusedBottomRect : TRect;
    FUseHScrollBar    : Boolean;
    FUseVScrollBar    : Boolean;
    FWantAllKeys      : Boolean;
  protected
    { get and set methods for properties }
    function tmGetAttributes(aRow, aCol : Integer) : TIpTerminalCharAttrs;
    function tmGetBackColor(aRow, aCol : Integer) : TColor;
    function tmGetCharSet(aRow, aCol : Integer) : Byte;
    function tmGetColumns : Integer;
    function tmGetEmulator : TIpTerminalEmulator;
    function tmGetForeColor(aRow, aCol : Integer) : TColor;
    function tmGetIsMasterTerminal : Boolean;
    function tmGetLine(aRow : Integer) : string;
    function tmGetRows : Integer;
    function tmGetScrollbackRows : Integer;
    function tmGetTelnetTermSize : string;
    procedure tmSetActive(aValue : Boolean);
    procedure tmSetAttributes(aRow, aCol : Integer; const aAttr : TIpTerminalCharAttrs);
    procedure tmSetBackColor(aRow, aCol : Integer; aValue : TColor);
    procedure tmSetBlinkTime(aValue : Integer);
    procedure tmSetBorderStyle(aBS : TBorderStyle);
    procedure tmSetCapture(aValue : TIpCaptureMode);
    procedure tmSetCaptureFile(const aValue : string);
    procedure tmSetCharSet(aRow, aCol : Integer; aValue : Byte);
    procedure tmSetColumns(aValue : Integer);
    procedure tmSetCursorType(aValue : TIpCursorType);
    procedure tmSetForeColor(aRow, aCol : Integer; aValue : TColor);
    procedure tmSetEmulator(aValue : TIpTerminalEmulator);
    procedure tmSetLazyByteDelay(aValue : Integer);
    procedure tmSetLazyTimeDelay(aValue : Integer);
    procedure tmSetLine(aRow : Integer; const aValue : string);
    procedure tmSetOriginCol(aValue : Integer);
    procedure tmSetOriginRow(aValue : Integer);
    procedure tmSetRows(aValue : Integer);
    procedure tmSetScrollback(aValue : Boolean);
    procedure tmSetScrollbackRows(aValue : Integer);
    procedure tmSetSockControl(aValue : TIpSockControl);
    procedure tmSetSocket(aValue : TSocket);
    procedure tmSetUseLazyDisplay(aValue : Boolean);
    procedure tmSetWantAllKeys(aValue : Boolean);
    procedure tmSetFreezeScrollback (v : boolean);                     {!!.03}

    { various miscellaneous methods }
    procedure tmAttachToSocket;
    procedure tmCalcExtent;
    procedure tmDetachFromSocket;
    procedure tmDrawDefaultText;
    procedure tmGetFontInfo;
    procedure tmInvalidateRow(aRow : Integer);
    procedure tmTermResize;

    { caret methods }
    procedure tmFreeCaret;
    procedure tmHideCaret;
    procedure tmMakeCaret;
    procedure tmPositionCaret;
    procedure tmShowCaret;

    { scrollbar stuff }
    procedure tmInitHScrollBar;
    procedure tmInitVScrollBar;
    procedure tmScrollHorz(aDist : Integer);
    procedure tmScrollVert(aDist : Integer);

    { selection stuff }
    procedure tmGrowSelect(X, Y : Integer);
    procedure tmMarkDeselected(aRow : Integer; aFromCol, aToCol : Integer);
    procedure tmMarkSelected(aRow : Integer; aFromCol, aToCol : Integer);
    function tmProcessClipboardCopy(var Msg : TWMKeyDown) : Boolean;
    procedure tmStartSelect(X, Y : Integer);

    { overridden ancestor methods }
    procedure KeyDown(var Key : Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState;
      X, Y : Integer); override;
    procedure PaintWindow(DC : HDC); override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    { message and event handlers }
    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Msg : TMessage); message CM_FONTCHANGED;
    procedure CMIpSocketStatus(var Msg : TCMIpSocketStatus); message CM_IPSOCKETSTATUS;
    procedure CMIpTermData(var Msg : TCMIpTermData); message CM_IPTERMDATA;
    procedure CMIpTermForceSize(var Msg : TMessage); message CM_IPTERMFORCESIZE;
    procedure CMIpTermStuff(var Msg : TMessage); message CM_IPTERMSTUFF;
    procedure CNKeyDown(var Msg : TWMKeyDown); message CN_KEYDOWN;
    procedure CNSysKeyDown(var Msg : TWMKeyDown); message CN_SYSKEYDOWN;
    procedure tmBeat(Sender: TObject);
    procedure WMCancelMode(var Msg : TMessage); message WM_CANCELMODE;
    procedure WMCopy(var Msg : TMessage); message WM_COPY;
    procedure WMEraseBkgnd(var Msg : TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg : TWMScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg : TWMScroll); message WM_VSCROLL;

    { TCustomControl emulation }
    procedure Paint;

  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    { overridden ancestor methods }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    { terminal methods }
    procedure Clear;
    procedure ClearAll;
    procedure CopyToClipboard;
    procedure HideSelection;
    procedure WriteChar(aCh : AnsiChar);
    procedure WriteString(const aSt : string);

    { public properties }
    property Attributes [aRow, aCol : Integer] : TIpTerminalCharAttrs
      read tmGetAttributes write tmSetAttributes;
    property BackColor [aRow, aCol : Integer] : TColor
      read tmGetBackColor write tmSetBackColor;
    property Canvas : TCanvas read FCanvas;
    property CharHeight : Integer read FCharHeight;
    property CharSet [aRow, aCol : Integer] : Byte
      read tmGetCharSet write tmSetCharSet;
    property CharWidth : Integer read FCharWidth;
    property ClientCols : Integer read FClientCols;
    property ClientRows : Integer read FClientRows;
    property ClientOriginCol : Integer read FOriginCol write tmSetOriginCol;
    property ClientOriginRow : Integer read FOriginRow write tmSetOriginRow;
    property ForeColor [aRow, aCol : Integer] : TColor
      read tmGetForeColor write tmSetForeColor;
    property HaveSelection : Boolean read FHaveSelection;
    property Line [aRow : Integer] : string read tmGetLine write tmSetLine;
    property Emulator : TIpTerminalEmulator
      read tmGetEmulator write tmSetEmulator;
    property Socket : TSocket
      read FSocket write tmSetSocket;
    property TelnetTermSize : string read tmGetTelnetTermSize;
  published
    property Active : Boolean
      read FActive write tmSetActive default ipc_TermActive;
    property BlinkTime : Integer
      read FBlinkTime write tmSetBlinkTime default ipc_TermBlinkTime;
    property BorderStyle : TBorderStyle
      read FBorderStyle write tmSetBorderStyle default ipc_TermBorderStyle;
    property Capture : TIpCaptureMode
      read FCapture write tmSetCapture default ipc_TermCapture;
    property CaptureFile : string
      read FCaptureFile write tmSetCaptureFile;
    property Columns : Integer
      read tmGetColumns write tmSetColumns default ipc_TermColumnCount;
    property CursorType : TIpCursorType
      read FCursorType write tmSetCursorType default ipc_TermCursorType;
    property HalfDuplex : Boolean
      read FHalfDuplex write FHalfDuplex default ipc_TermHalfDuplex;
    property IsMasterTerminal : Boolean read tmGetIsMasterTerminal;
    property LazyByteDelay : Integer
      read FLazyByteDelay write tmSetLazyByteDelay default ipc_TermLazyByteDelay;
    property LazyTimeDelay : Integer
      read FLazyTimeDelay write tmSetLazyTimeDelay default ipc_TermLazyTimeDelay;
    property Rows : Integer
      read tmGetRows write tmSetRows default ipc_TermRowCount;
    property ScrollbackRows : Integer
      read tmGetScrollbackRows write tmSetScrollbackRows default ipc_TermScrollRowCount;
    property Scrollback : Boolean
      read FScrollback write tmSetScrollback;
    property SockControl : TIpSockControl
      read FSockControl write tmSetSockControl;
    property UseLazyDisplay : Boolean
      read FUseLazyDisplay write tmSetUseLazyDisplay default ipc_TermUseLazyDisplay;
    property WantAllKeys : Boolean
      read FWantAllKeys write tmSetWantAllKeys default ipc_TermWantAllKeys;
    property OnCursorMoved : TIpTerminalCursorMovedEvent
      read FOnCursorMoved write FOnCursorMoved;
    property FreezeScrollBack : boolean                                {!!.03}
      read FFreezeScrollback write tmSetFreezeScrollback               {!!.03}
      default ipc_FreezeScrollback;                                    {!!.03}

    { publish ancestor's properties }
    property Align;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property Height;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;

    { publish ancestor's events }
    property OnClick;
    property OnDblClick;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TIpTerminalEmulator = class(TIpBaseComponent)
  private
    FTerminalBuffer  : TIpTerminalBuffer;
    FCharSetMapping  : TIpCharSetMapping;
    FIsDefault       : Boolean;
    FKeyboardMapping : TIpKeyboardMapping;
    FParser          : TIpTerminalParser;
    FTelnetTermType  : string;
    FTerminal        : TIpCustomTerminal;

    { saved cursor details }
    FSavedAttrs     : TIpTerminalCharAttrs;
    FSavedBackColor : TColor;
    FSavedCharSet   : Byte;
    FSavedCol       : Integer;
    FSavedForeColor : TColor;
    FSavedRow       : Integer;

  protected
    { property access methods }
    function teGetNeedsUpdate : Boolean; virtual;
    procedure teSetTerminal(aValue : TIpCustomTerminal); virtual;

    { overridden ancestor methods }
    procedure Notification(AComponent : TComponent;
      Operation : TOperation); override;

    { new virtual methods }
    procedure teClear; virtual;
    procedure teClearAll; virtual;
    procedure teSendChar(aCh : char; aCanEcho : Boolean); virtual;

    {Cursor Movement Methods}                                          {!!.01}
    procedure teHandleCursorMovement (Sender : TObject;                {!!.01}
                                      Row    : Integer;                {!!.01}
                                      Col    : Integer);               {!!.01}
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    procedure BlinkPaint(aVisible : Boolean); virtual;
    function HasBlinkingText : Boolean; virtual;
    procedure KeyDown(var Key : Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key : AnsiChar); virtual;
    procedure LazyPaint; virtual;
    procedure Paint; virtual;
    procedure ProcessBlock(aData : Pointer; aDataLen : Longint); virtual;

    { read-only properties for low-level objects }
    property Buffer : TIpTerminalBuffer read FTerminalBuffer;
    property CharSetMapping : TIpCharSetMapping read FCharSetMapping;
    property KeyboardMapping : TIpKeyboardMapping read FKeyboardMapping;
    property Parser : TIpTerminalParser read FParser;
    property NeedsUpdate : Boolean read teGetNeedsUpdate;
  published
    property Terminal : TIpCustomTerminal read FTerminal write teSetTerminal;
    property TelnetTermType : string read FTelnetTermType write FTelnetTermType;
  end;

  TIpTerminal = class(TIpCustomTerminal)
  published
    property Emulator;
  end;

type
  TIpTTYEmulator = class(TIpTerminalEmulator)
  private
    FCellWidths     : PAdIntegerArray;
    FDisplayStr     : PAnsiChar;
    FDisplayStrSize : Integer;
    FPaintFreeList  : Pointer;
    FRefresh        : Boolean;
  protected
    { property accessor methods }
    function teGetNeedsUpdate : Boolean; override;

    { overridden ancestor methods }
    procedure teClear; override;
    procedure teClearAll; override;
    procedure teSetTerminal(aValue : TIpCustomTerminal); override;

    { miscellaneous }
    procedure ttyDrawChars(aRow, aStartCol, aEndCol : Integer; aVisible : Boolean);
    procedure ttyProcessCommand(aCh : AnsiChar);

    { paint node methods }
    procedure ttyExecutePaintScript(aRow : Integer; aScript : Pointer);
    procedure ttyFreeAllPaintNodes;
    procedure ttyFreePaintNode(aNode : Pointer);
    function ttyNewPaintNode : Pointer;

  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    { overridden ancestor methods }
    procedure KeyPress(var Key : AnsiChar); override;
    procedure LazyPaint; override;
    procedure Paint; override;
    procedure ProcessBlock(aData : Pointer; aDataLen : Longint); override;
  end;

type
  TIpVT100Emulator = class(TIpTerminalEmulator)
  private
    FAnswerback     : string;
    FBlinkers       : Pointer;
    FBlinkFreeList  : Pointer;
    FCellWidths     : PAdIntegerArray;
    FDisplayStr     : PAnsiChar;
    FDisplayStrSize : Integer;
    FDispUpperASCII : Boolean;
    FLEDs           : Integer;
    FLineAttrArray  : TObject;
    FPaintFreeList  : Pointer;
    FRefresh        : Boolean;
    FSecondaryFont  : TFont;

    { modes }
    FANSIMode         : Boolean;
    FAppKeyMode       : Boolean;
    FAppKeypadMode    : Boolean;
    FAutoRepeat       : Boolean;
    FCol132Mode       : Boolean;
    FGPOMode          : Boolean; { graphics proc.option not supported }
    FInsertMode       : Boolean;
    FInterlace        : Boolean; { interlace chgs are not supported }
    FNewLineMode      : Boolean;
    FRelOriginMode    : Boolean;
    FRevScreenMode    : Boolean;
    FSmoothScrollMode : Boolean; { smooth scrolling is not supported }
    FWrapAround       : Boolean;

    { character sets }
    FUsingG1   : Boolean;                                   
    FG0CharSet : Integer;
    FG1CharSet : Integer;

  protected
    { property accessor methods }
    function teGetNeedsUpdate : Boolean; override;
    procedure vttSetCol132Mode(aValue : Boolean);
    procedure vttSetRelOriginMode(aValue : Boolean);
    procedure vttSetRevScreenMode(aValue : Boolean);

    { overridden ancestor methods }
    procedure teClear; override;
    procedure teClearAll; override;
    procedure teSetTerminal(aValue : TIpCustomTerminal); override;

    { miscellaneous }
    procedure vttDrawChars(aRow, aStartVal, aEndVal : Integer;
      aVisible : Boolean; aCharValues : Boolean);
    procedure vttProcessCommand;
    function vttGenerateDECREPTPARM(aArg : Integer) : string;
    procedure vttInvalidateRow(aRow : Integer);
    procedure vttProcess8bitChar(aCh : AnsiChar);
    procedure vttScrollRowsHandler(aSender : TObject; aCount, aTop, aBottom : Integer);
    procedure vttToggleNumLock;

    { blink node methods }
    procedure vttCalcBlinkScript;
    procedure vttClearBlinkScript;
    procedure vttDrawBlinkOffCycle(aRow, aStartCh, aEndCh : Integer);
    procedure vttFreeAllBlinkNodes;
    procedure vttFreeBlinkNode(aNode : Pointer);
    function vttNewBlinkNode : Pointer;

    { paint node methods }
    procedure vttExecutePaintScript(aRow : Integer; aScript : Pointer);
    procedure vttFreeAllPaintNodes;
    procedure vttFreePaintNode(aNode : Pointer);
    function vttNewPaintNode : Pointer;

  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    { overridden ancestor methods }
    procedure BlinkPaint(aVisible : Boolean); override;
    function HasBlinkingText : Boolean; override;
    procedure KeyDown(var Key : Word; Shift: TShiftState); override;
    procedure KeyPress(var Key : AnsiChar); override;
    procedure LazyPaint; override;
    procedure Paint; override;
    procedure ProcessBlock(aData : Pointer; aDataLen : Longint); override;

    { modes }
    property ANSIMode : Boolean read FANSIMode write FANSIMode;
    property AppKeyMode : Boolean read FAppKeyMode write FAppKeyMode;
    property AppKeypadMode : Boolean read FAppKeypadMode write FAppKeypadMode;
    property AutoRepeat : Boolean read FAutoRepeat write FAutoRepeat;
    property Col132Mode : Boolean read FCol132Mode write vttSetCol132Mode;
    property GPOMode : Boolean read FGPOMode write FGPOMode;
    property InsertMode : Boolean read FInsertMode write FInsertMode;
    property Interlace : Boolean read FInterlace write FInterlace;
    property NewLineMode : Boolean read FNewLineMode write FNewLineMode;
    property RelOriginMode : Boolean read FRelOriginMode write vttSetRelOriginMode;
    property RevScreenMode : Boolean read FRevScreenMode write vttSetRevScreenMode;
    property SmoothScrollMode : Boolean read FSmoothScrollMode write FSmoothScrollMode;
    property WrapAround : Boolean read FWrapAround write FWrapAround;

    { miscellaneous }
    property LEDs : Integer read FLEDs write FLEDs;

  published
    property Answerback : string read FAnswerback write FAnswerback;
    property DisplayUpperASCII : Boolean read FDispUpperASCII write FDispUpperASCII;
  end;

implementation

const
  BeatInterval = 100;

type
  PBlinkNode = ^TBlinkNode;
  TBlinkNode = packed record
    bnNext    : PBlinkNode;
    bnRow     : Integer;
    bnStartCh : Integer;
    bnEndCh   : Integer;
  end;

type
  PPaintNode = ^TPaintNode;
  TPaintNode = packed record
    pnNext : PPaintNode;
    pnStart: Integer;               { start column of range }
    pnEnd  : Integer;               { end column of range }
    pnFore : TColor;                { foreground color for range }
    pnBack : TColor;                { background color for range }
    pnAttr : TIpTerminalCharAttrs;  { attributes for range }
    pnCSet : Byte;                  { charset for range }
  end;

const
  VT52DeviceAttrs  = #27'/Z';                     { "VT100 acting as VT52" }
  VT100DeviceAttrs = #27'[?1;0c';                 { "Base VT100, no options" }
  VT100StatusRpt   = #27'[0n';                    { "terminal OK" }
  VT100CursorPos   = #27'[%d;%dR';                { "cursor is at row;col" }
  VT100ReportParm  = #27'[%d;%d;%d;%d;%d;%d;%dx'; { report terminal parameters }

const
  VT100CharSetNames : array [0..4] of string =
                      ('VT100-USASCII',           { charset 0 }
                       'VT100-UK',                { charset 1 }
                       'VT100-linedraw',          { charset 2 }
                       'VT100-ROM1',              { charset 3 }
                       'VT100-ROM2');             { charset 4 }


{===Terminal/Emulator links==========================================}
type
  PTermEmuLink = ^TTermEmuLink;
  TTermEmuLink = record
    telNext : PTermEmuLink;
    telTerm : TIpCustomTerminal;
    telEmu  : TIpTerminalEmulator;
  end;
{--------}
var
  TermEmuLink : PTermEmuLink;
  TermEmuLinkFreeList : PTermEmuLink;
{--------}
procedure AddTermEmuLink(aTerminal : TIpCustomTerminal;
                         aEmulator : TIpTerminalEmulator);
var
  Node : PTermEmuLink;
begin
  { if the link already exists, exit }
  Node := TermEmuLink;
  while (Node <> nil) do begin
    if (Node^.telTerm = aTerminal) then
      Exit;
    Node := Node^.telNext;
  end;
  { otherwise, add it }
  if (TermEmuLinkFreeList = nil) then
    New(Node)
  else begin
    Node := TermEmuLinkFreeList;
    TermEmuLinkFreeList := Node^.telNext;
  end;
  Node^.telTerm := aTerminal;
  Node^.telEmu := aEmulator;
  Node^.telNext := TermEmuLink;
  TermEmuLink := Node;
  { now update each object to point to the other }
  aTerminal.Emulator := aEmulator;
  aEmulator.Terminal := aTerminal;
end;
{--------}
procedure RemoveTermEmuLink(aTerminal : TIpCustomTerminal;
                            aNotify   : Boolean);
var
  Dad, Node : PTermEmuLink;
  Emulator  : TIpTerminalEmulator;
begin
  { remove the link }
  Emulator := nil;
  Dad := nil;
  Node := TermEmuLink;
  while (Node <> nil) do begin
    if (Node^.telTerm = aTerminal) then begin
      if (Dad = nil) then
        TermEmuLink := Node^.telNext
      else
        Dad^.telNext := Node^.telNext;
      Emulator := Node^.telEmu;
      Node^.telNext := TermEmuLinkFreeList;
      TermEmuLinkFreeList := Node;
      Break;
    end;
    Dad := Node;
    Node := Node^.telNext;
  end;
  {now update each object to point to nil instead of each other}
  if aNotify and (Emulator <> nil) then begin
    aTerminal.Emulator := nil;
    Emulator.Terminal := nil;
  end;
end;
{====================================================================}


{===Byte queue=======================================================}
{Notes: this byte queue was published in Algorithms Alfresco in The
        Delphi Magazine, December 1988. It is reproduced and used here
        with permission.
        Copyright (c) Julian M Bucknall, 1998. All Rights Reserved.}
type
  TaaByteQueue = class
  private
    bqHead      : PChar;
    bqTail      : PChar;
    bqQMidPoint : PChar;
    bqQueue     : PChar;
    bqQueueEnd  : PChar;
  protected
    function GetCapacity : Integer;
    function GetCount : Integer;
    procedure SetCapacity(aValue : Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AdvanceAfterPut(aDataLen : Integer);
    procedure Clear;
    (***** not used yet
    procedure Get(var aData; aDataLen : Integer);
    function IsEmpty : Boolean;
    *****)
    procedure Put(const aData; aDataLen : Integer);
    function PutPeek(aDataLen : Integer) : Pointer;
    function Peek(aDataLen : Integer) : Pointer;
    procedure Remove(aDataLen : integer);                              {!!.01} 

    property Capacity : Integer read GetCapacity write SetCapacity;
    property Count : Integer read GetCount;
  end;
{--------}
procedure NotEnoughDataError(aAvail, aReq : Integer);
begin
  raise Exception.Create(Format(SNotEnoughData, [aAvail, aReq]));
end;
{--------}
constructor TaaByteQueue.Create;
begin
  inherited Create;
  Capacity := 64;
end;
{--------}
destructor TaaByteQueue.Destroy;
begin
  if Assigned(bqQueue) then
    FreeMem(bqQueue, bqQueueEnd - bqQueue);
  inherited Destroy;
end;
{--------}
procedure TaaByteQueue.AdvanceAfterPut(aDataLen : Integer);
begin
  inc(bqTail, aDataLen);
end;
{--------}
procedure TaaByteQueue.Clear;
begin
  bqHead := bqQueue;
  bqTail := bqQueue;
end;
{--------}
(***** not used yet
procedure TaaByteQueue.Get(var aData; aDataLen : Integer);
var
  ByteCount : Integer;
begin
  {check for enough data}
  if (aDataLen > Count) then
    NotEnoughDataError(Count, aDataLen);
  {move the data}
  Move(bqHead^, aData, aDataLen);
  inc(bqHead, aDataLen);
  {if we've emptied the queue, move the head/tail pointers back}
  ByteCount := Count;
  if (ByteCount = 0) then begin
    bqHead := bqQueue;
    bqTail := bqQueue;
  end
  {if the head of the queue has moved into the overflow zone, move the
   data back, and reset the head/tail pointers}
  else if (bqHead >= bqQMidPoint) then begin
    Move(bqHead^, bqQueue^, ByteCount);
    bqHead := bqQueue;
    bqTail := bqHead + ByteCount;
  end;
end;
*****)
{--------}
function TaaByteQueue.getCapacity : Integer;
begin
  Result := (bqQueueEnd - bqQueue) div 2;
end;
{--------}
function TaaByteQueue.getCount : Integer;
begin
  Result := bqTail - bqHead;
end;
{--------}
(***** not used yet
function TaaByteQueue.IsEmpty : Boolean;
begin
  Result := bqHead = bqTail;
end;
*****)
{--------}
procedure TaaByteQueue.Put(const aData; aDataLen : Integer);
var
  ByteCount : Integer;
begin
  {if required, grow the queue by at least doubling its size}
  ByteCount := Count;
  while (ByteCount + aDataLen > Capacity) do
    Capacity := Capacity * 2;
  {we now have enough room, so add the new data}
  Move(aData, bqTail^, aDataLen);
  inc(bqTail, aDataLen);
end;
{--------}
function TaaByteQueue.PutPeek(aDataLen : Integer) : Pointer;
var
  ByteCount : Integer;
begin
  {if required, grow the queue by at least doubling its size}
  ByteCount := Count;
  while (ByteCount + aDataLen > Capacity) do
    Capacity := Capacity * 2;
  {just return the tail Pointer}
  Result := bqTail;
end;
{--------}
function TaaByteQueue.Peek(aDataLen : Integer) : Pointer;
begin
  {check for enough data}
  if (aDataLen > Count) then
    NotEnoughDataError(Count, aDataLen);
  {just return the head Pointer}
  Result := bqHead;
end;
{--------}                                                             {!!.01}
procedure TaaByteQueue.Remove(aDataLen : integer);                     {!!.01}
begin                                                                  {!!.01}
  {check for enough data}                                              {!!.01}
  if (aDataLen > Count) then                                           {!!.01}
    NotEnoughDataError(Count, aDataLen);                               {!!.01}
  {move the remaining data to the head}                                {!!.01}
  Move((bqHead+aDataLen)^, bqHead^, Count - aDataLen);                 {!!.01}
  bqTail := bqTail - aDataLen;                                         {!!.01}
end;                                                                   {!!.01}
{--------}
procedure TaaByteQueue.setCapacity(aValue : Integer);
var
  ByteCount : Integer;
  NewQueue  : PChar;
begin
  {don't allow data to be lost}
  ByteCount := Count;
  if (aValue < ByteCount) then
    aValue := ByteCount;
  {round the requested capacity to nearest 64 bytes}
  aValue := (aValue + 63) and $7FFFFFC0;
  {get a new buffer}
  GetMem(NewQueue, aValue * 2);
  {if we have data to transfer from the old buffer, do so}
  if (ByteCount <> 0) then
    Move(bqHead^, NewQueue^, ByteCount);
  {destroy the old buffer}
  if (bqQueue <> nil) then
    FreeMem(bqQueue, bqQueueEnd - bqQueue);
  {set the head/tail and other pointers}
  bqQueue := NewQueue;
  bqHead := NewQueue;
  bqTail := NewQueue + ByteCount;
  bqQueueEnd := NewQueue + (aValue * 2);
  bqQMidPoint := NewQueue + aValue;
end;
{====================================================================}


{===VT100 line attributes array======================================}
type
  TIpLineAttrArray = class
  private
    FArray          : PAnsiChar;
    FEmulator       : TIpVT100Emulator;
    FTotalLineCount : Integer;
  protected
    function laaGetAttr(aInx : Integer) : TIpVT100LineAttr;
    procedure laaSetAttr(aInx : Integer; aValue : TIpVT100LineAttr);
    procedure laaResize;
  public
    constructor Create(aEmulator : TIpVT100Emulator);
    destructor Destroy; override;
    procedure Scroll(aCount, aTop, aBottom : Integer);
    property Attr [aInx : Integer] : TIpVT100LineAttr
      read laaGetAttr write laaSetAttr;
  end;
{--------}
constructor TIpLineAttrArray.Create(aEmulator : TIpVT100Emulator);
begin
  inherited Create;
  FEmulator := aEmulator;
  FTotalLineCount := aEmulator.Buffer.SVRowCount;
  FArray := AllocMem(FTotalLineCount * sizeof(TIpVT100LineAttr));
end;
{--------}
destructor TIpLineAttrArray.Destroy;
begin
  if (FArray <> nil) then
    FreeMem(FArray, FTotalLineCount * sizeof(TIpVT100LineAttr));
  inherited Destroy;
end;
{--------}
function TIpLineAttrArray.laaGetAttr(aInx : Integer) : TIpVT100LineAttr;
begin
  if (FTotalLineCount <> FEmulator.Buffer.SVRowCount) then
    laaResize;
  aInx := aInx + FTotalLineCount - FEmulator.Buffer.RowCount - 1;
  if (aInx < 0) or (aInx >= FTotalLineCount) then
    Result := ltNormal
  else
    Result := TIpVT100LineAttr(FArray[aInx]);
end;
{--------}
procedure TIpLineAttrArray.laaResize;
var
  NewArray      : PAnsiChar;
  NewTotalCount : Integer;
begin
  {allocate the new array}
  NewTotalCount := FEmulator.Buffer.SVRowCount;
  NewArray := AllocMem(NewTotalCount * sizeof(TIpVT100LineAttr));
  {copy over the old array from the end, *not* the beginning}
  if (NewTotalCount < FTotalLineCount) then
    Move(FArray[FTotalLineCount - NewTotalCount], NewArray[0],
         NewTotalCount)
  else {FTotalLineCount <= NewTotalCount}
    Move(FArray[0], NewArray[FTotalLineCount - NewTotalCount],
         FTotalLineCount);
  {free the old array, save the new one}
  FreeMem(FArray, FTotalLineCount * sizeof(TIpVT100LineAttr));
  FArray := NewArray;
  FTotalLineCount := NewTotalCount;
end;
{--------}
procedure TIpLineAttrArray.laaSetAttr(aInx   : Integer;
                                      aValue : TIpVT100LineAttr);
begin
  if (FTotalLineCount <> FEmulator.Buffer.SVRowCount) then
    laaResize;
  aInx := aInx + FTotalLineCount - FEmulator.Buffer.RowCount - 1;
  if (0 <= aInx) and (aInx < FTotalLineCount) then
    FArray[aInx] := AnsiChar(aValue);
end;
{--------}
procedure TIpLineAttrArray.Scroll(aCount, aTop, aBottom : Integer);
var
  i     : Integer;
  ToInx : Integer;
begin
  aTop := aTop + FTotalLineCount - FEmulator.Buffer.RowCount - 1;
  aBottom := aBottom + FTotalLineCount - FEmulator.Buffer.RowCount - 1;
  if (aCount > 0) then {scroll upwards} begin
    ToInx := aTop;
    for i := (aTop + aCount) to aBottom do begin
      FArray[ToInx] := FArray[i];
      inc(ToInx);
    end;
    for i := ToInx to aBottom do
      FArray[i] := #0;
  end
  else if (aCount < 0) then {scroll downwards} begin
    ToInx := aBottom;
    for i := (aBottom + aCount) downto aTop do begin
      FArray[ToInx] := FArray[i];
      dec(ToInx);
    end;
    for i := ToInx downto aTop do
      FArray[i] := #0;
  end;
end;
{====================================================================}


{===Helper routines==================================================}
function BoundI(aLow, X, aHigh : Integer) : Integer;
begin
  if (aLow < aHigh) then
    if (X <= aLow) then
      Result := aLow
    else if (X >= aHigh) then
      Result := aHigh
    else
      Result := X
  else {aHigh is less than aLow}
    if (X <= aHigh) then
      Result := aHigh
    else if (X >= aLow) then
      Result := aLow
    else
      Result := X;
end;
{--------}


{===TIpTerminalArray=================================================}
constructor TIpTerminalArray.Create(aItemSize : Integer);
begin
  inherited Create;
  {save a valid item size}
  case aItemSize of
    1, 2, 4 : FItemSize := aItemSize;
  else
    FItemSize := 1;
  end;{case}
  {set the actual column count to -1, which means 'uncalculated'}
  FActColCount := -1;
end;
{--------}
destructor TIpTerminalArray.Destroy;
begin
  if (FItems <> nil) then begin
    FreeMem(FItems, RowCount * FActColCount * ItemSize);
  end;
  inherited Destroy;
end;

{ Clear the entire array, filling with DefaultItem }
procedure TIpTerminalArray.Clear;
begin
  if (FItems <> nil) then
    taClearRows(FItems, FActColCount, 0, pred(RowCount));
end;

{ Clear the row in between the two inclusive columns, filling }
{ with DefaultItem (equivalent to 'erase') }
procedure TIpTerminalArray.ClearItems(aRow : Integer;
                                      aFromCol, aToCol : Integer);
var
  Walker    : PAnsiChar;
  Value     : Longint;
  i         : Integer;
begin
  Walker := @FItems[((aRow * FActColCount) + aFromCol) * ItemSize];
  case ItemSize of
    1 : FillChar(Walker^, succ(aToCol - aFromCol), Byte(FDefaultItem));
    2 : begin
          Value := Word(FDefaultItem);
          for i := 0 to (aToCol - aFromCol) do begin
            PWord(Walker)^ := Value;
            inc(Walker, 2);
          end;
        end;
    4 : begin
          Value := FDefaultItem;
          for i := 0 to (aToCol - aFromCol) do begin
            PLongint(Walker)^ := Value;
            inc(Walker, 4);
          end;
        end;
  end;{case}
end;

{ Delete aCount default items at aRow , aCol; items currently beyond   }
{ these positions are pulled left, and their original positions filled }
{ with default items; this action does not extend beyond the row       }
procedure TIpTerminalArray.DeleteItems(aCount : Integer;
  aRow : Integer; aCol : Integer);
var
  ItemCount : Integer;
  Distance  : Integer;
  FromPtr   : PAnsiChar;
  ToPtr     : PAnsiChar;
  Value     : Longint;
  i         : Integer;
begin
  {$IFDEF UseRangeChecks}
  {Range check aRow and aCol}
  if (aRow < 0) or (aRow >= RowCount) or
     (aCol < 0) or (aCol >= ColCount) then
    raise Exception.Create(Format(SRowColOOR,
      ['TIpTerminalArray.DeleteItems', aRow, aCol]));
  {$ENDIF}
  Distance := ColCount - aCol;
  if (Distance > aCount) then
    Distance := aCount;
  ItemCount := ColCount - aCol - Distance;
  case ItemSize of
    1 : begin
          ToPtr := @FItems[(aRow * FActColCount) + aCol];
          if (ItemCount > 0) then begin
            FromPtr := ToPtr + Distance;
            Move(FromPtr^, ToPtr^, ItemCount);
          end;
          ToPtr := ToPtr + ItemCount;
          FillChar(ToPtr^, Distance, Byte(FDefaultItem));
        end;
    2 : begin
          ToPtr := @FItems[((aRow * FActColCount) + aCol) * 2];
          if (ItemCount > 0) then begin
            FromPtr := ToPtr + (Distance * 2);
            Move(FromPtr^, ToPtr^, ItemCount * 2);
          end;
          ToPtr := ToPtr + (ItemCount * 2);
          Value := Word(FDefaultItem);
          for i := 0 to pred(Distance) do begin
            PWord(ToPtr)^ := Value;
            inc(ToPtr, 2);
          end;
        end;
    4 : begin
          ToPtr := @FItems[((aRow * FActColCount) + aCol) * 4];
          if (ItemCount > 0) then begin
            FromPtr := ToPtr + (Distance * 4);
            Move(FromPtr^, ToPtr^, ItemCount * 4);
          end;
          ToPtr := ToPtr + (ItemCount * 4);
          Value := FDefaultItem;
          for i := 0 to pred(Distance) do begin
            PLongint(ToPtr)^ := Value;
            inc(ToPtr, 4);
          end;
        end;
  end;{case}
end;

{ Return a Pointer to the item at aRow, aCol; caller must properly }
{ dereference, and not memory overread }
function TIpTerminalArray.GetItemPtr(aRow, aCol : Integer) : Pointer;
begin
  {$IFDEF UseRangeChecks}
  {Range check aRow and aCol}
  if (aRow < 0) or (aRow >= RowCount) or
     (aCol < 0) or (aCol >= ColCount) then
    raise Exception.Create(Format(SRowColOOR, ['TIpTerminalArray.GetItemPtr', aRow, aCol]));
  {$ENDIF}
  if FItems = nil then
    Result := nil
  else begin
    case ItemSize of
      1 : Result := @FItems[(aRow * FActColCount) + aCol];
      2 : Result := @FItems[(aRow * FActColCount * 2) + (aCol * 2)];
      4 : Result := @FItems[(aRow * FActColCount * 4) + (aCol * 4)];
    else
      raise Exception.Create(SInvItemSize);
      Result := nil;
    end;{case}
  end;
end;

{ Insert aCount default items at aRow , aCol; items currently in these }
{ positions are pushed right, but not beyond the row boundary          }
procedure TIpTerminalArray.InsertItems(aCount : Integer;
                                       aRow   : Integer;
                                       aCol   : Integer);
var
  ItemCount : Integer;
  Distance  : Integer;
  FromPtr   : PAnsiChar;
  ToPtr     : PAnsiChar;
  Value     : Longint;
  i         : Integer;
begin
  {$IFDEF UseRangeChecks}
  {Range check aRow and aCol}
  if (aRow < 0) or (aRow >= RowCount) or (aCol < 0) or (aCol >= ColCount) then
    raise Exception.Create(
      Format(SRowColOOR, ['TIpTerminalArray.InsertItems', aRow, aCol]));
  {$ENDIF}
  Distance := ColCount - aCol;
  if (Distance > aCount) then
    Distance := aCount;
  ItemCount := ColCount - aCol - Distance;
  case ItemSize of
    1 : begin
          FromPtr := @FItems[(aRow * FActColCount) + aCol];
          if (ItemCount > 0) then begin
            ToPtr := FromPtr + Distance;
            Move(FromPtr^, ToPtr^, ItemCount);
          end;
          FillChar(FromPtr^, Distance, Byte(FDefaultItem));
        end;
    2 : begin
          FromPtr := @FItems[((aRow * FActColCount) + aCol) * 2];
          if (ItemCount > 0) then begin
            ToPtr := FromPtr + (Distance * 2);
            Move(FromPtr^, ToPtr^, ItemCount * 2);
          end;
          Value := Word(FDefaultItem);
          for i := 0 to pred(Distance) do begin
            PWord(FromPtr)^ := Value;
            inc(FromPtr, 2);
          end;
        end;
    4 : begin
          FromPtr := @FItems[((aRow * FActColCount) + aCol) * 4];
          if (ItemCount > 0) then begin
            ToPtr := FromPtr + (Distance * 4);
            Move(ToPtr^, FromPtr^, ItemCount * 4);
          end;
          Value := FDefaultItem;
          for i := 0 to pred(Distance) do begin
            PLongint(FromPtr)^ := Value;
            inc(FromPtr, 4);
          end;
        end;
  end;{case}
end;

procedure TIpTerminalArray.ReplaceItems(aOldItem : pointer; aNewItem : pointer);
var
  Walker    : PAnsiChar;
  OldValue  : Longint;
  NewValue  : Longint;
  Row       : Integer;
  I         : Integer;
begin
  case ItemSize of
    1 : begin
          OldValue := PByte(aOldItem)^;
          NewValue := PByte(aNewItem)^;
        end;
    2 : begin
          OldValue := PWord(aOldItem)^;
          NewValue := PWord(aNewItem)^;
        end;
    4 : begin
          OldValue := PLongint(aOldItem)^;
          NewValue := PLongint(aNewItem)^;
        end;
  else
    { dummy statements that will never get executed, however they fool }
    { the warning analyzer in the compiler }
    OldValue := 0;
    NewValue := 0;
  end;{ case }
  for Row := 0 to Pred(RowCount) do begin
    Walker := @FItems[(Row * FActColCount) * ItemSize];
    case ItemSize of
      1 : for I := 0 to pred(ColCount) do begin
            if (PByte(Walker)^ = OldValue) then
               PByte(Walker)^ := NewValue;
            Inc(Walker);
          end;
      2 : for I := 0 to pred(ColCount) do begin
            if (PWord(Walker)^ = OldValue) then
               PWord(Walker)^ := NewValue;
            Inc(Walker, 2);
          end;
      4 : for I := 0 to pred(ColCount) do begin
            if (PLongint(Walker)^ = OldValue) then
               PLongint(Walker)^ := NewValue;
            Inc(Walker, 4);
          end;
    end;{ case }
  end;
end;

{ Scroll the data by aCount rows, filling new rows with DefaultItem;  }
{ scroll just between aStartRow and aEndRow inclusive; if aCount is   }
{ positive it means scroll upwards (ie, the usual sense), if negative }
{ scroll downwards }
procedure TIpTerminalArray.ScrollRows(aCount : Integer;
                                      aStartRow, aEndRow : Integer);
var
  ThisRow : Integer;
  FromPtr : PAnsiChar;
  ToPtr   : PAnsiChar;
  i       : Integer;
begin
  {$IFDEF UseRangeChecks}
  {Range check aStartRow and aEndRow}
  if (aStartRow < 0) or (aStartRow >= RowCount) or
    (aEndRow < 0) or (aEndRow >= RowCount) then
    raise Exception.Create(Format(SRowRowOOR, [aStartRow, aEndRow]));
  {$ENDIF}
  if (FItems <> nil) and (aCount <> 0) then begin
    {make sure the end row is larger than the start row}
    if (aEndRow < aStartRow) then begin
      ThisRow := aEndRow;
      aEndRow := aStartRow;
      aStartRow := ThisRow;
    end;
    { split the code depending on whether we are scrolling upwards, }
    { aCount is +ve, or downwards, aCount is -ve }
    if (aCount > 0) then {scroll upwards} begin
      { if the number of rows to scroll is greater than the difference }
      { between the start and end rows, all we need to do is blank out }
      { all the rows between start and end inclusive, otherwise we have }
      { some scrolling to do }
      ThisRow := aStartRow;
      if (aCount <= (aEndRow - aStartRow)) then begin
        ToPtr := @FItems[ThisRow * FActColCount * ItemSize];
        FromPtr := @FItems[(ThisRow + aCount) * FActColCount * ItemSize];
        for i := 0 to (aEndRow - aStartRow - aCount) do begin
          Move(FromPtr^, ToPtr^, ColCount * ItemSize);
          inc(FromPtr, FActColCount * ItemSize);
          inc(ToPtr, FActColCount * ItemSize);
          inc(ThisRow);
        end;
      end;
      { now blank out everything from ThisRow to aEndRow }
      taClearRows(FItems, FActColCount, ThisRow, aEndRow);
    end
    else {scroll downwards} begin
      { if the number of rows to scroll is greater than the difference  }
      { between the start and end rows, all we need to do is blank out  }
      { all the rows between start and end inclusive, otherwise we have }
      { some scrolling to do }
      aCount := -aCount;
      ThisRow := aEndRow;
      if (aCount <= (aEndRow - aStartRow)) then begin
        ToPtr := @FItems[ThisRow * FActColCount * ItemSize];
        FromPtr := @FItems[(ThisRow - aCount) * FActColCount * ItemSize];
        for i := 0 to (aEndRow - aStartRow - aCount) do begin
          Move(FromPtr^, ToPtr^, ColCount * ItemSize);
          dec(FromPtr, FActColCount * ItemSize);
          dec(ToPtr, FActColCount * ItemSize);
          dec(ThisRow);
        end;
      end;
      {now blank out everything from aStartRow to ThisRow}
      taClearRows(FItems, FActColCount, aStartRow, ThisRow);
    end;
  end;
end;

{ Define the default item to be used to fill new items, eg, }
{ during scrolling, clearing or resizing }
procedure TIpTerminalArray.SetDefaultItem(aDefaultItem : Pointer);
begin
  case ItemSize of
    1 : FDefaultItem := PByte(aDefaultItem)^;
    2 : FDefaultItem := PWord(aDefaultItem)^;
    4 : FDefaultItem := PLongint(aDefaultItem)^;
  end;
end;
{--------}
procedure TIpTerminalArray.taClearRows(aBuffer : PAnsiChar;
                                       aActColCount : Integer;
                                       aStartRow, aEndRow : Integer);
var
  Walker     : PAnsiChar;
  Value      : Longint;
  DWORDCount : Integer;
  i          : Integer;
begin
  Walker := @aBuffer[aStartRow * aActColCount * ItemSize];
  if (ItemSize = 1) then
    FillChar(Walker^,
             succ(aEndRow - aStartRow) * aActColCount,
             Byte(FDefaultItem))
  else begin
    if (ItemSize = 2) then begin
      Value := (FDefaultItem shl 16) + Word(FDefaultItem);
      DWORDCount := (succ(aEndRow - aStartRow) * aActColCount) div 2;
    end
    else begin
      Value := FDefaultItem;
      DWORDCount := succ(aEndRow - aStartRow) * aActColCount;
    end;
    for i := 0 to pred(DWORDCount) do begin
      PLongint(Walker)^ := Value;
      inc(Walker, 4);
    end;
  end;
end;
{--------}
procedure TIpTerminalArray.taGrowArray(aRowCount,
                                       aColCount,
                                       aActColCount : Integer);
var
  NewArray : PAnsiChar;
  RowSize  : Integer;
  NumRows  : Integer;
  FromPtr  : PAnsiChar;
  ToPtr    : PAnsiChar;
  i        : Integer;
begin
  { make sure we have the new actual column count: this is the external }
  { column count rounded up so that the actual length of a row in bytes }
  { is a multiple of four--this makes fills and moves much faster       }
  if (aActColCount = -1) then begin
    case ItemSize of
      1 : aActColCount := ((aColCount + 3) div 4) * 4;
      2 : aActColCount := ((aColCount + 1) div 2) * 2;
      4 : aActColCount := aColCount;
    end;{case}
  end;
  { nothing to do if either the row or actual column count is zero }
  if (aRowCount = 0) or (aActColCount = 0) then
    Exit;
  { equally obvious, nothing to do if neither the row and actual column }
  { count have changed }
  if (aRowCount = RowCount) and (aActColCount = FActColCount) then
    Exit;
  { at this point we must allocate another array }
  GetMem(NewArray, aRowCount * aActColCount * ItemSize);
  { blank it all out using the current default item }
  taClearRows(NewArray, aActColCount, 0, pred(aRowCount));
  {if the old array existed, transfer over the data, row by row,
   starting at the bottom}
  if (FItems <> nil) then begin
    {calculate the number of bytes to copy per row}
    if (ColCount < aColCount) then
      RowSize := ColCount * ItemSize
    else
      RowSize := aColCount * ItemSize;
    {calculate the number of rows to copy}
    if (RowCount < aRowCount) then
      NumRows := RowCount
    else
      NumRows := aRowCount;
    {copy the rows}
    FromPtr := @FItems[RowCount * FActColCount * ItemSize];
    ToPtr := @NewArray[aRowCount * aActColCount * ItemSize];
    for i := pred(RowCount) downto (RowCount - NumRows) do begin
      dec(FromPtr, FActColCount * ItemSize);
      dec(ToPtr, aActColCount * ItemSize);
      Move(FromPtr^, ToPtr^, RowSize);
    end;
    {dispose of the old array}
    FreeMem(FItems, RowCount * FActColCount * ItemSize);
  end;
  {save the new array}
  FItems := NewArray;
  FActColCount := aActColCount;
end;
{--------}
procedure TIpTerminalArray.taSetColCount(aNewCount : Integer);
begin
  {$IFDEF UseRangeChecks}
  {Range check aNewCount}
  if (aNewCount < 0) then
    raise Exception.Create(
      Format(SLessZero, ['TIpTerminalArray.taSetColCount', aNewCount]));
  {$ENDIF}
  if (aNewCount <> ColCount) then begin
    taGrowArray(RowCount, aNewCount, -1);
    FColCount := aNewCount;
  end;
end;
{--------}
procedure TIpTerminalArray.taSetRowCount(aNewCount : Integer);
begin
  {$IFDEF UseRangeChecks}
  {Range check aNewCount}
  if (aNewCount < 0) then
    raise Exception.Create(
      Format(SLessZero, ['TIpTerminalArray.taSetColCount', aNewCount]));
  {$ENDIF}
  if (aNewCount <> RowCount) then begin
    taGrowArray(aNewCount, ColCount, FActColCount);
    FRowCount := aNewCount;
  end;
end;

{ Write aCount copies of aItem to aRow, aCol, without wrapping at }
{ the end of the row }
procedure TIpTerminalArray.WriteDupItems(aItem : Pointer;
  aCount : Integer; aRow, aCol : Integer);
var
  Walker    : PAnsiChar;
  Value     : Longint;
  i         : Integer;
  ItemCount : Integer;
begin
  {$IFDEF UseRangeChecks}
  {Range check aRow and aCol}
  if (aRow < 0) or (aRow >= RowCount) or (aCol < 0) or (aCol >= ColCount) then
    raise Exception.Create(
      Format(SRowColOOR, ['TIpTerminalArray.WriteDupItems', aRow, aCol]));
  {$ENDIF}
  if (FItems <> nil) then begin
    ItemCount := ColCount - aCol;
    if (ItemCount > aCount) then
      ItemCount := aCount;
    case ItemSize of
      1 : FillChar(FItems[(aRow * FActColCount) + aCol],
                   ItemCount, PByte(aItem)^);
      2 : begin
            Walker := @FItems[((aRow * FActColCount) + aCol) * 2];
            Value := PWord(aItem)^;
            for i := 0 to pred(ItemCount) do begin
              PWord(Walker)^ := Value;
              inc(Walker, 2);
            end;
          end;
      4 : begin
            Walker := @FItems[((aRow * FActColCount) + aCol) * 4];
            Value := PLongint(aItem)^;
            for i := 0 to pred(ItemCount) do begin
              PLongint(Walker)^ := Value;
              inc(Walker, 4);
            end;
          end;
    end;{case}
  end;
end;

{ Write aCount items to aRow, aCol, without wrapping at the end of the row }
procedure TIpTerminalArray.WriteItems(aItems : Pointer;
                                      aCount : Integer;
                                      aRow, aCol : Integer);
var
  ItemCount : Integer;
begin
  {$IFDEF UseRangeChecks}
  {Range check aRow and aCol}
  if (aRow < 0) or (aRow >= RowCount) or (aCol < 0) or (aCol >= ColCount) then
    raise Exception.Create(
      Format(SRowColOOR, ['TIpTerminalArray.WriteItems', aRow, aCol]));
  {$ENDIF}
  if (FItems <> nil) then begin
    ItemCount := ColCount - aCol;
    if (ItemCount > aCount) then
      ItemCount := aCount;
    case ItemSize of
      1 : Move(aItems^,
               FItems[(aRow * FActColCount) + aCol],
               ItemCount);
      2 : Move(aItems^,
               FItems[(aRow * FActColCount * 2) + (aCol * 2)],
               ItemCount * 2);
      4 : Move(aItems^,
               FItems[(aRow * FActColCount * 4) + (aCol * 4)],
               ItemCount * 4);
    end;{case}
  end;
end;
{====================================================================}


{===Bitset routines==================================================}
procedure IPTClearAllBits(aBitset : PByteArray; aBitCount : Integer);
begin
  FillChar(aBitset^, (aBitCount+7) shr 3, 0);
end;
{--------}
procedure IPTClearBit(aBitset : PByteArray; aBit : Integer);
var
  BS : PAnsiChar absolute aBitset;
  P  : PAnsiChar;
  M  : Byte;
begin
  P := BS + (aBit shr 3);
  M := 1 shl (Byte(aBit) and 7);
  P^ := char(Byte(P^) and not M);
end;
{--------}
function IPTIsBitSet(aBitset : PByteArray; aBit : Integer) : Boolean;
var
  BS : PAnsiChar absolute aBitset;
  P  : PAnsiChar;
  M  : Byte;
begin
  P := BS + (aBit shr 3);
  M := 1 shl (Byte(aBit) and 7);
  Result := (Byte(P^) and M) <> 0;
end;
{--------}
function IPTReallocBitset(aBitset      : PByteArray;
                          aOldBitCount : Integer;
                          aNewBitCount : Integer) : PByteArray;
var
  XferBitCount : Integer;
begin
  if (aNewBitCount = 0) then
    Result := nil
  else begin
    Result := AllocMem(aNewBitCount);
    if (aBitset <> nil) then begin
      if (aOldBitCount < aNewBitCount) then
        XferBitCount := aOldBitCount
      else
        XferBitCount := aNewBitCount;
      Move(aBitset^, Result^, (XferBitCount+7) shr 3);
    end;
  end;
  FreeMem(aBitset, (aOldBitCount+7) shr 3);
end;
{--------}
(***** not used yet
procedure IPTSetAllBits(aBitset : PByteArray; aBitCount : Integer);
begin
  FillChar(aBitset^, (aBitCount+7) shr 3, $FF);
end;
*****)
{--------}
procedure IPTSetBit(aBitset : PByteArray; aBit : Integer);
var
  BS : PAnsiChar absolute aBitSet;
  P  : PAnsiChar;
  M  : Byte;
begin
  P := BS + (aBit shr 3);
  M := 1 shl (Byte(aBit) and 7);
  P^ := char(Byte(P^) or M);
end;
{====================================================================}


{===Invalid rectangle routines==========================================}
const
  RectsPerPage = 200;
type
  PInvRect = ^TInvRect;
  TInvRect = packed record
    irNext : PInvRect;
    irRect : TRect;
  end;
  PInvRectPage = ^TInvRectPage;
  TInvRectPage = packed record
    irpNext  : PInvRectPage;
    irpRects : array [0.. pred(RectsPerPage)] of TInvRect;
  end;
var
  InvRectFreeList : PInvRect;
  InvRectPageList : PInvRectPage;
{--------}
procedure IPTFreeInvRect(P : PInvRect);
begin
  {push rect onto free list}
  P^.irNext := InvRectFreeList;
  InvRectFreeList := P;
end;
{--------}
procedure IPTAllocInvRectPage;
var
  NewPage : PInvRectPage;
  i       : Integer;
begin
  {alloc new page and add it to front of page list}
  New(NewPage);
  NewPage^.irpNext := InvRectPageList;
  InvRectPageList := NewPage;
  {add all rects on this page to free list}
  for i := 0 to pred(RectsPerPage) do
    IPTFreeInvRect(@NewPage^.irpRects[i]);
end;
{--------}
function IPTAllocInvRect : PInvRect;
begin
  {pop top rect from free list; if none, add a whole page's worth}
  if (InvRectFreeList = nil) then
    IPTAllocInvRectPage;
  Result := InvRectFreeList;
  InvRectFreeList := Result^.irNext;
end;
{--------}
procedure IPTFreeInvRectPages;
var
  Temp : PInvRectPage;
begin
  {dispose of all rect pages}
  while (InvRectPageList <> nil) do begin
    Temp := InvRectPageList;
    InvRectPageList := Temp^.irpNext;
    Dispose(Temp);
  end;
  {since all rects have now gone, force the rect free list to nil}
  InvRectFreeList := nil;
end;
{--------}
procedure IPTIpdInvalidRect(var aInvRectList : PInvRect;
                       const aRect        : TRect);
var
  NewRect : PInvRect;
begin
  NewRect := IPTAllocInvRect;
  NewRect^.irNext := aInvRectList;
  aInvRectList := NewRect;
  NewRect^.irRect := aRect;
end;
{--------}
function IPTRemoveInvalidRect(var aInvRectList : PInvRect;
                           var aRect        : TRect) : Boolean;
var
  TopRect : PInvRect;
begin
  if (aInvRectList = nil) then
    Result := False
  else begin
    Result := True;
    TopRect := aInvRectList;
    aInvRectList := TopRect^.irNext;
    aRect := TopRect^.irRect;
    IPTFreeInvRect(TopRect);
  end;
end;
{--------}
function IPTPeekInvalidRect(aInvRectList : PInvRect;
                     var aRect        : TRect) : Boolean;
begin
  if (aInvRectList = nil) then
    Result := False
  else begin
    Result := True;
    aRect := aInvRectList^.irRect;
  end;
end;
{--------}
procedure IPTMergeInvalidRects(aInvRectList : PInvRect);
var
  Temp    : PInvRect;
  Walker  : PInvRect;
  MinRect : TRect;
begin
  if (aInvRectList = nil) then
    Exit;
  {performs a simple merge of all the invalid rects in the list by
   working out the rect that just covers them all; free the rects from
   the list after we read them--leaving the first for our use}
  MinRect := aInvRectList^.irRect;
  Walker := aInvRectList^.irNext;
  while (Walker <> nil) do begin
    with Walker^.irRect do begin
      if Left < MinRect.Left then
        MinRect.Left := Left;
      if Top < MinRect.Top then
        MinRect.Top := Top;
      if Right > MinRect.Right then
        MinRect.Right := Right;
      if Bottom > MinRect.Bottom then
        MinRect.Bottom := Bottom;
    end;
    Temp := Walker;
    Walker := Walker^.irNext;
    IPTFreeInvRect(Temp);
  end;
  {MinRect now contains the smallest rectangle that covers all invalid
   rects in the list; set this minimum invalid rect into the first
   (and only) item in the list}
  aInvRectList^.irNext := nil;
  aInvRectList^.irRect := MinRect;
end;
{====================================================================}

{===TIpTerminalBuffer================================================}
constructor TIpTerminalBuffer.Create(aUseWideChars : Boolean);
var
  i : Integer;
begin
  inherited Create;

  {set the values of the properties that define defaults}
  FDefBackColor := ipc_TermBufBackColor;
  FDefForeColor := ipc_TermBufForeColor;
  FDefAnsiChar := ' ';
  FDefWideChar := ' ';
  FDefCharSet := 0;
  FDefAttr := [];

  {set the 'power-up' values}
  FBackColor := ipc_TermBufBackColor;
  FForeColor := ipc_TermBufForeColor;
  UseAbsAddress := ipc_TermBufUseAbsAddress;
  UseAutoWrap := ipc_TermBufUseAutoWrap;
  UseAutoWrapDelay := ipc_TermBufUseAutoWrapDelay;
  UseInsertMode := ipc_TermBufUseInsertMode;
  UseNewLineMode := ipc_TermBufUseNewLineMode;
  UseScrollRegion := ipc_TermBufUseScrollRegion;

  {set up all the matrices to hold the displayed data}
  {..character matrix}
  if aUseWideChars then begin
    FCharMatrix := TIpTerminalArray.Create(sizeof(WideChar));
    FCharMatrix.SetDefaultItem(@FDefWideChar);
  end
  else
  begin
    FCharMatrix := TIpTerminalArray.Create(sizeof(AnsiChar));
    FCharMatrix.SetDefaultItem(@FDefAnsiChar);
  end;
  FCharMatrix.ColCount := ipc_TermBufColCount;
  FCharMatrix.RowCount := ipc_TermBufScrollRowCount;

  {..character set matrix}
  FCharSetMatrix := TIpTerminalArray.Create(sizeof(Byte));
  FCharSetMatrix.SetDefaultItem(@FDefCharSet);
  FCharSetMatrix.ColCount := ipc_TermBufColCount;
  FCharSetMatrix.RowCount := ipc_TermBufScrollRowCount;

  {..character attributes matrix}
  FAttrMatrix := TIpTerminalArray.Create(sizeof(TIpTerminalCharAttrs));
  FAttrMatrix.SetDefaultItem(@FDefAttr);
  FAttrMatrix.ColCount := ipc_TermBufColCount;
  FAttrMatrix.RowCount := ipc_TermBufScrollRowCount;

  {..character foreground color matrix}
  FForeColorMatrix := TIpTerminalArray.Create(sizeof(TColor));
  FForeColorMatrix.SetDefaultItem(@FDefForeColor);
  FForeColorMatrix.ColCount := ipc_TermBufColCount;
  FForeColorMatrix.RowCount := ipc_TermBufScrollRowCount;

  {..character background color matrix}
  FBackColorMatrix := TIpTerminalArray.Create(sizeof(TColor));
  FBackColorMatrix.SetDefaultItem(@FDefBackColor);
  FBackColorMatrix.ColCount := ipc_TermBufColCount;
  FBackColorMatrix.RowCount := ipc_TermBufScrollRowCount;

  {initialize the terminal dimensions}
  FUseWideChars := aUseWideChars;
  SVRowCount := ipc_TermBufScrollRowCount;
  ColCount := ipc_TermBufColCount;
  RowCount := ipc_TermBufRowCount;

  ClearAllHorzTabStops;
  ClearAllVertTabStops;
  i := Col;
  while (i < ColCount) do begin
    Col := i;
    SetHorzTabStop;
    inc(i, 8);
  end;
  Row := 1;
  Col := 1;

  {set up the current cursor position}
  FCursorRow := SVRowCount - RowCount;
  FDisplayOriginRow := FCursorRow;
  FCursorCol := 0;

  { add the whole screen as an invalid rect }
  FCursorMoved := True;
  tbInvalidateRect(FCursorRow, 0,
                   pred(SVRowCount), pred(ColCount));
  tbFireOnCursorMovedEvent;                                            {!!.01}                   
end;
{--------}
destructor TIpTerminalBuffer.Destroy;
var
  OurRect : TRect;
begin
  { remove all of the invalid rects and discard them }
  while IPTRemoveInvalidRect(PInvRect(FInvRectList), OurRect) do { nothing };

  {free the tab stops}
  IPTReallocBitset(FVertTabStops, RowCount, 0);
  IPTReallocBitset(FHorzTabStops, ColCount, 0);

  { free all arrays }
  FBackColorMatrix.Free;
  FForeColorMatrix.Free;
  FAttrMatrix.Free;
  FCharSetMatrix.Free;
  FCharMatrix.Free;
  inherited Destroy;
end;
{--------}
procedure TIpTerminalBuffer.ClearAllHorzTabStops;
begin
  if (ColCount <> 0) then
    IPTClearAllBits(FHorzTabStops, ColCount);
end;
{--------}
procedure TIpTerminalBuffer.ClearAllVertTabStops;
begin
  if (RowCount <> 0) then
    IPTClearAllBits(FVertTabStops, RowCount);
end;
{--------}
procedure TIpTerminalBuffer.ClearHorzTabStop;
begin
  if (ColCount <> 0) then
    IPTClearBit(FHorzTabStops, FCursorCol);
end;
{--------}
procedure TIpTerminalBuffer.ClearVertTabStop;
begin
  if (RowCount <> 0) then
    IPTClearBit(FVertTabStops, FCursorRow);
end;
{--------}
procedure TIpTerminalBuffer.DeleteChars(aCount : Integer);
var
  CharCount : Integer;
begin
  FBeyondMargin := False;
  {$IFDEF UseRangeCheck}
  if (aCount <= 0) then
    raise Exception.Create(Format(SPosReqd, ['TIpTerminalBuffer.DeleteChars']));
  {$ENDIF}
  {the actual number of characters to delete is constrained by the
   current display region}
  CharCount := FDisplayOriginCol + FDisplayColCount - FCursorCol;
  if (CharCount > aCount) then
    CharCount := aCount;
  if (CharCount > 0) then begin
    FCharMatrix.DeleteItems(CharCount, FCursorRow, FCursorCol);
    FCharSetMatrix.DeleteItems(CharCount, FCursorRow, FCursorCol);
    FAttrMatrix.DeleteItems(CharCount, FCursorRow, FCursorCol);
    FForeColorMatrix.DeleteItems(CharCount, FCursorRow, FCursorCol);
    FBackColorMatrix.DeleteItems(CharCount, FCursorRow, FCursorCol);
    {the cursor does not move}
    tbInvalidateRect(FCursorRow, FCursorCol,
                     FCursorRow, FCursorCol+CharCount-1);
  end;
end;
{--------}
procedure TIpTerminalBuffer.DeleteLines(aCount : Integer);
var
  MaxRow : Integer;
begin
  FBeyondMargin := False;
  {deleting lines is equivalent to a scroll up to the current cursor
   position; we take account of any scroll region, of course}
  {$IFDEF UseRangeCheck}
  if (aCount <= 0) then
    raise Exception.Create(Format(SPosReqd, ['TIpTerminalBuffer.DeleteLines']));
  {$ENDIF}
  MaxRow := FDisplayOriginRow + FDisplayRowCount - 1;
  tbScrollRows(aCount, FCursorRow, MaxRow);
  {the cursor does not move}
  tbInvalidateRect(FCursorRow, FDisplayOriginCol,
                   MaxRow, FDisplayOriginCol+FDisplayColCount-1);
end;
{--------}
procedure TIpTerminalBuffer.DoBackHorzTab;
var
  NewCol : Integer;
begin
  if (ColCount > 0) then begin
    NewCol := FCursorCol;
    while (NewCol > FDisplayOriginCol) do begin
      dec(NewCol);
      if IPTIsBitSet(FHorzTabStops, NewCol) then begin
        FCursorMoved := FCursorMoved or (FCursorCol <> NewCol);
        FCursorCol := NewCol;
        tbFireOnCursorMovedEvent;                                      {!!.01}                   
        Exit;
      end;
    end;
    FCursorMoved := FCursorMoved or (FCursorCol <> FDisplayOriginCol);
    FCursorCol := FDisplayOriginCol;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.DoBackVertTab;
var
  NewRow : Integer;
begin
  FBeyondMargin := False;
  if (RowCount > 0) then begin
    NewRow := FCursorRow;
    while (NewRow > FDisplayOriginRow) do begin
      dec(NewRow);
      if IPTIsBitSet(FVertTabStops, NewRow) then begin
        FCursorMoved := FCursorMoved or (FCursorRow <> NewRow);
        FCursorRow := NewRow;
        tbFireOnCursorMovedEvent;                                      {!!.01}                   
        Exit;
      end;
    end;
    FCursorMoved := FCursorMoved or (FCursorRow <> FDisplayOriginRow);
    FCursorRow := FDisplayOriginRow;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.DoBackspace;
begin
  FBeyondMargin := False;
  if (FCursorCol > FDisplayOriginCol) then begin
    FCursorMoved := True;
    dec(FCursorCol);
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.DoCarriageReturn;
begin
  FBeyondMargin := False;
  FCursorMoved := FCursorMoved or (FCursorCol <> FDisplayOriginCol);
  FCursorCol := FDisplayOriginCol;
  tbFireOnCursorMovedEvent;                                            {!!.01}                   
end;
{--------}
procedure TIpTerminalBuffer.DoHorzTab;
var
  NewCol : Integer;
  MaxCol : Integer;
begin
  FBeyondMargin := False;
  if (ColCount > 0) then begin
    NewCol := FCursorCol;
    MaxCol := FDisplayOriginCol + FDisplayColCount - 1;
    while (NewCol < MaxCol) do begin
      inc(NewCol);
      if IPTIsBitSet(FHorzTabStops, NewCol) then begin
        FCursorMoved := FCursorMoved or (FCursorCol <> NewCol);
        FCursorCol := NewCol;
        tbFireOnCursorMovedEvent;                                      {!!.01}                   
        Exit;
      end;
    end;
    FCursorMoved := FCursorMoved or (FCursorCol <> MaxCol);
    FCursorCol := MaxCol;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.DoLineFeed;
begin
  FBeyondMargin := False;
  if UseNewLineMode then
    DoCarriageReturn;
  MoveCursorDown(True);
end;
{--------}
procedure TIpTerminalBuffer.DoVertTab;
var
  NewRow : Integer;
  MaxRow : Integer;
begin
  FBeyondMargin := False;
  if (RowCount > 0) then begin
    NewRow := FCursorRow;
    MaxRow := FDisplayOriginRow + FDisplayRowCount - 1;
    while (NewRow < MaxRow) do begin
      inc(NewRow);
      if IPTIsBitSet(FVertTabStops, NewRow) then begin
        FCursorMoved := FCursorMoved or (FCursorRow <> NewRow);
        FCursorRow := NewRow;
        tbFireOnCursorMovedEvent;                                      {!!.01}                   
        Exit;
      end;
    end;
    FCursorMoved := FCursorMoved or (FCursorRow <> MaxRow);
    FCursorRow := MaxRow;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.EraseAll;
begin
  {WARNING: this DOES NOT use the scroll region, if defined, it blanks
            out everything in the scrollback buffer}
  FBeyondMargin := False;
  FCharMatrix.Clear;
  FCharSetMatrix.Clear;
  FAttrMatrix.Clear;
  FForeColorMatrix.Clear;
  FBackColorMatrix.Clear;
  Row := 1;
  Col := 1;
  tbInvalidateRect(FCursorRow, 0,
                   pred(SVRowCount), pred(ColCount));
end;
{--------}
procedure TIpTerminalBuffer.EraseChars(aCount : Integer);
var
  CharCount : Integer;
  ToColNum  : Integer;
  CurRow    : Integer;
  CurCol    : Integer;
  MaxCol    : Integer;
  MaxRow    : Integer;
begin
  {WARNING: this uses the scroll region, if defined}

  FBeyondMargin := False;
  {$IFDEF UseRangeChecks}
  if (aCount <= 0) then
    raise Exception.Create(Format(SPosReqd, ['TIpTerminalBuffer.EraseChars']));
  {$ENDIF}
  { this is complicated by the need to erase chars on individual lines separately }
  CurRow := FCursorRow;
  CurCol := FCursorCol;
  MaxCol := FDisplayOriginCol + FDisplayColCount - 1;
  MaxRow := FDisplayOriginRow + FDisplayRowCount - 1;
  while (aCount > 0) and (CurRow <= MaxRow) do begin
    {calculate the number of characters to erase in this row}
    CharCount := MaxCol - CurCol + 1;
    if (CharCount > aCount) then
      CharCount := aCount;
    {calculate the final column number}
    ToColNum := CurCol + CharCount - 1;
    {erase}
    FCharMatrix.ClearItems(CurRow, CurCol, ToColNum);
    FCharSetMatrix.ClearItems(CurRow, CurCol, ToColNum);
    FAttrMatrix.ClearItems(CurRow, CurCol, ToColNum);
    FForeColorMatrix.ClearItems(CurRow, CurCol, ToColNum);
    FBackColorMatrix.ClearItems(CurRow, CurCol, ToColNum);
    tbInvalidateRect(CurRow, CurCol,
                     CurRow, ToColNum);
    {set up for the next loop}
    dec(aCount, CharCount);
    inc(CurRow);
    CurCol := FDisplayOriginCol;
  end;
end;
{--------}
procedure TIpTerminalBuffer.EraseFromBOL;
begin
  {WARNING: this uses the scroll region, if defined}

  FBeyondMargin := False;
  {set all characters from the beginning of the line, up to and
   including the cursor position, to blanks}
  FCharMatrix.ClearItems(FCursorRow, FDisplayOriginCol, FCursorCol);
  FCharSetMatrix.ClearItems(FCursorRow, FDisplayOriginCol, FCursorCol);
  FAttrMatrix.ClearItems(FCursorRow, FDisplayOriginCol, FCursorCol);
  FForeColorMatrix.ClearItems(FCursorRow, FDisplayOriginCol, FCursorCol);
  FBackColorMatrix.ClearItems(FCursorRow, FDisplayOriginCol, FCursorCol);
  tbInvalidateRect(FCursorRow, FDisplayOriginCol,
                   FCursorRow, FCursorCol);
end;
{--------}
procedure TIpTerminalBuffer.EraseFromBOW;
var
  DisplayStartRow : Integer;
begin
  {WARNING: this DOES NOT use the scroll region, if defined, it blanks
            out everything on the window up to and including the
            cursor position}

  FBeyondMargin := False;
  {set all characters from the beginning of the line, up to and
   including the cursor position, to blanks}
  FCharMatrix.ClearItems(FCursorRow, 0, FCursorCol);
  FCharSetMatrix.ClearItems(FCursorRow, 0, FCursorCol);
  FAttrMatrix.ClearItems(FCursorRow, 0, FCursorCol);
  FForeColorMatrix.ClearItems(FCursorRow, 0, FCursorCol);
  FBackColorMatrix.ClearItems(FCursorRow, 0, FCursorCol);
  tbInvalidateRect(FCursorRow, 0,
                   FCursorRow, FCursorCol);
  {now erase all previous lines, by scrolling them out of existence}
  DisplayStartRow := SVRowCount - RowCount;
  if (FCursorRow > DisplayStartRow) then begin
    tbScrollRows(FCursorRow - DisplayStartRow,
                 DisplayStartRow, pred(FCursorRow));
    tbInvalidateRect(DisplayStartRow, 0,
                     pred(FCursorRow), pred(ColCount));
  end;
end;
{--------}
procedure TIpTerminalBuffer.EraseLine;
var
  MaxCol : Integer;
begin
  {WARNING: this uses the scroll region, if defined}

  FBeyondMargin := False;
  {set all characters from the beginning to the end of the line to
   blanks}
  MaxCol := FDisplayOriginCol + FDisplayColCount - 1;
  FCharMatrix.ClearItems(FCursorRow, FDisplayOriginCol, MaxCol);
  FCharSetMatrix.ClearItems(FCursorRow, FDisplayOriginCol, MaxCol);
  FAttrMatrix.ClearItems(FCursorRow, FDisplayOriginCol, MaxCol);
  FForeColorMatrix.ClearItems(FCursorRow, FDisplayOriginCol, MaxCol);
  FBackColorMatrix.ClearItems(FCursorRow, FDisplayOriginCol, MaxCol);
  tbInvalidateRect(FCursorRow, FDisplayOriginCol,
                   FCursorRow, MaxCol);
end;
{--------}
procedure TIpTerminalBuffer.EraseScreen;
begin
  {WARNING: this DOES NOT use the scroll region, if defined, it blanks
            out everything on the window}

  FBeyondMargin := False;
  {scroll the entire scrollback view by RowCount lines: this will have
   the effect of clearing the display view and of setting up the
   scrollback buffer with the previous screen}
  tbScrollRows(RowCount, 0, pred(SVRowCount));
  tbInvalidateRect(SVRowCount - RowCount, 0,
                   pred(SVRowCount), pred(ColCount));
end;
{--------}
procedure TIpTerminalBuffer.EraseToEOL;
var
  MaxCol : Integer;
begin
  {WARNING: this uses the scroll region, if defined}

  FBeyondMargin := False;
  {set all characters from and including the cursor position to the
   end of the line to blanks}
  MaxCol := FDisplayOriginCol + FDisplayColCount - 1;
  FCharMatrix.ClearItems(FCursorRow, FCursorCol, MaxCol);
  FCharSetMatrix.ClearItems(FCursorRow, FCursorCol, MaxCol);
  FAttrMatrix.ClearItems(FCursorRow, FCursorCol, MaxCol);
  FForeColorMatrix.ClearItems(FCursorRow, FCursorCol, MaxCol);
  FBackColorMatrix.ClearItems(FCursorRow, FCursorCol, MaxCol);
  tbInvalidateRect(FCursorRow, FCursorCol,
                   FCursorRow, MaxCol);
end;
{--------}
procedure TIpTerminalBuffer.EraseToEOW;
begin
  {WARNING: this DOES NOT use the scroll region, if defined, it blanks
            out everything from and including the cursor position up
            to the end of the screen}

  FBeyondMargin := False;
  {set all characters from and including the cursor position to the
   end of the line to blanks}
  FCharMatrix.ClearItems(FCursorRow, FCursorCol, pred(ColCount));
  FCharSetMatrix.ClearItems(FCursorRow, FCursorCol, pred(ColCount));
  FAttrMatrix.ClearItems(FCursorRow, FCursorCol, pred(ColCount));
  FForeColorMatrix.ClearItems(FCursorRow, FCursorCol, pred(ColCount));
  FBackColorMatrix.ClearItems(FCursorRow, FCursorCol, pred(ColCount));
  tbInvalidateRect(FCursorRow, FCursorCol,
                   FCursorRow, pred(ColCount));
  {now erase all succeeding lines, by scrolling them out of existence}
  if (FCursorRow < pred(SVRowCount)) then begin
    tbScrollRows(pred(SVRowCount) - FCursorRow,
                 succ(FCursorRow), pred(SVRowCount));
    tbInvalidateRect(succ(FCursorRow), 0,
                     pred(SVRowCount), pred(ColCount));
  end;
end;
{--------}
procedure TIpTerminalBuffer.GetCharAttrs(var aValue : TIpTerminalCharAttrs);
begin
  aValue := FAttr;
end;
{--------}
procedure TIpTerminalBuffer.GetDefCharAttrs(var aValue : TIpTerminalCharAttrs);
begin
  aValue := FDefAttr;
end;
{--------}
function TIpTerminalBuffer.GetInvalidRect(var aRect : TRect) : Boolean;
begin
  if (FInvRectList = nil) then
    Result := False
  else begin
    {if there is more than one invalid rect, merge them all into one}
    if (PInvRect(FInvRectList)^.irNext <> nil) then
      IPTMergeInvalidRects(FInvRectList);
    {return the first invalid rect}
    Result := IPTRemoveInvalidRect(PInvRect(FInvRectList), aRect);
  end;
end;
{--------}
function TIpTerminalBuffer.GetLineAttrPtr(aRow : Integer) : Pointer;
var
  OurRow : Integer;
begin
  {normalize the row number to our internal system}
  OurRow := tbCvtToInternalRow(aRow, True);
  {$IFDEF UseRangeChecks}
  if (OurRow < 0) or (OurRow >= FSVRowCount) then
    raise Exception.Create(Format(SRowOOR, ['TIpTerminalBuffer.GetLineAttrPtr']));
  {$ENDIF}
  {return the Pointer into the matrix}
  Result := FAttrMatrix.GetItemPtr(OurRow, FDisplayOriginCol);
end;
{--------}
function TIpTerminalBuffer.GetLineBackColorPtr(aRow : Integer) : Pointer;
var
  OurRow : Integer;
begin
  {normalize the row number to our internal system}
  OurRow := tbCvtToInternalRow(aRow, True);
  {$IFDEF UseRangeChecks}
  if (OurRow < 0) or
     (OurRow >= FSVRowCount) then
    raise Exception.Create(Format(SRowOOR, ['TIpTerminalBuffer.GetLineBackColorPtr']));
  {$ENDIF}
  {return the Pointer into the matrix}
  Result := FBackColorMatrix.GetItemPtr(OurRow, FDisplayOriginCol);
end;
{--------}
function TIpTerminalBuffer.GetLineCharPtr(aRow : Integer) : Pointer;
var
  OurRow : Integer;
begin
  {normalize the row number to our internal system}
  OurRow := tbCvtToInternalRow(aRow, True);
  {$IFDEF UseRangeChecks}
  if (OurRow < 0) or (OurRow >= FSVRowCount) then
    raise Exception.Create(Format(SRowOOR, ['TIpTerminalBuffer.GetLineCharPtr']));
  {$ENDIF}
  {return the Pointer into the matrix}
  Result := FCharMatrix.GetItemPtr(OurRow, FDisplayOriginCol)
end;
{--------}
function TIpTerminalBuffer.GetLineCharSetPtr(aRow : Integer) : Pointer;
var
  OurRow : Integer;
begin
  {normalize the row number to our internal system}
  OurRow := tbCvtToInternalRow(aRow, True);
  {$IFDEF UseRangeChecks}
  if (OurRow < 0) or (OurRow >= FSVRowCount) then
    raise Exception.Create(Format(SRowOOR, ['TIpTerminalBuffer.GetLineCharSetPtr']));
  {$ENDIF}
  {return the Pointer into the matrix}
  Result := FCharSetMatrix.GetItemPtr(OurRow, FDisplayOriginCol)
end;
{--------}
function TIpTerminalBuffer.GetLineForeColorPtr(aRow : Integer) : Pointer;
var
  OurRow : Integer;
begin
  {normalize the row number to our internal system}
  OurRow := tbCvtToInternalRow(aRow, True);
  {$IFDEF UseRangeChecks}
  if (OurRow < 0) or (OurRow >= FSVRowCount) then
    raise Exception.Create(Format(SRowOOR, ['TIpTerminalBuffer.GetLineForeColorPtr']));
  {$ENDIF}
  {return the Pointer into the matrix}
  Result := FForeColorMatrix.GetItemPtr(OurRow, FDisplayOriginCol)
end;
{--------}
function TIpTerminalBuffer.HasCursorMoved : Boolean;
begin
  {return whether the cursor has moved since the last time this
   function was called; reset the internal variable}
  Result := FCursorMoved;
  FCursorMoved := False;
end;
{--------}
function TIpTerminalBuffer.HasDisplayChanged : Boolean;
var
  DummyRect : TRect;
begin
  Result := IPTPeekInvalidRect(PInvRect(FInvRectList), DummyRect);
end;
{--------}
procedure TIpTerminalBuffer.InsertChars(aCount : Integer);
var
  CharCount : Integer;
begin
  FBeyondMargin := False;
  {$IFDEF UseRangeCheck}
  if (aCount <= 0) then
    raise Exception.Create(Format(SPosReqd, ['TIpTerminalBuffer.InsertChars']));
  {$ENDIF}
  { the actual number of characters to delete is constrained by the }
  { current display region }
  CharCount := FDisplayOriginCol + FDisplayColCount - FCursorCol;
  if (CharCount > aCount) then
    CharCount := aCount;
  if (CharCount > 0) then begin
    FCharMatrix.InsertItems(CharCount, FCursorRow, FCursorCol);
    FCharSetMatrix.InsertItems(CharCount, FCursorRow, FCursorCol);
    FAttrMatrix.InsertItems(CharCount, FCursorRow, FCursorCol);
    FForeColorMatrix.InsertItems(CharCount, FCursorRow, FCursorCol);
    FBackColorMatrix.InsertItems(CharCount, FCursorRow, FCursorCol);
    tbInvalidateRect(FCursorRow, FCursorCol,
                     FCursorRow, pred(FCursorCol + CharCount));
  end;
end;
{--------}
procedure TIpTerminalBuffer.InsertLines(aCount : Integer);
var
  MaxRow : Integer;
begin
  FBeyondMargin := False;
  { inserting lines is equivalent to a scroll down from the current  }
  { cursor position; we take account of any scroll region, of course }
  {$IFDEF UseRangeCheck}
  if (aCount <= 0) then
    raise Exception.Create(Format(SPosReqd, ['TIpTerminalBuffer.InsertLines']));
  {$ENDIF}
  MaxRow := FDisplayOriginRow + FDisplayRowCount - 1;
  tbScrollRows(-aCount, FCursorRow, MaxRow);
  {the cursor position doesn't change as a result of inserting rows}
  tbInvalidateRect(FCursorRow, FDisplayOriginCol,
                   MaxRow, pred(FDisplayOriginCol + FDisplayColCount));
end;
{--------}
procedure TIpTerminalBuffer.MoveCursorDown(aScroll : Boolean);
begin
  FBeyondMargin := False;
  tbMoveCursorUpDown(1, aScroll);
end;
{--------}
procedure TIpTerminalBuffer.MoveCursorLeft(aWrap   : Boolean;
                                           aScroll : Boolean);
begin
  FBeyondMargin := False;
  tbMoveCursorLeftRight(-1, aWrap, aScroll);
end;
{--------}
procedure TIpTerminalBuffer.MoveCursorRight(aWrap   : Boolean;
                                            aScroll : Boolean);
begin
  FBeyondMargin := False;
  tbMoveCursorLeftRight(1, aWrap, aScroll);
end;
{--------}
procedure TIpTerminalBuffer.MoveCursorUp(aScroll : Boolean);
begin
  FBeyondMargin := False;
  tbMoveCursorUpDown(-1, aScroll);
end;
{--------}
procedure TIpTerminalBuffer.Reset;
var
  i : Integer;
begin
  {set the attributes, the colors, and the charset}
  FAttr := FDefAttr;
  FForeColor := FDefForeColor;
  FBackColor := FDefBackColor;
  FCharSet := FDefCharSet;

  {set the various matrices to their 'power-up' values}
  if UseWideChars then begin
    FCharMatrix.SetDefaultItem(@FDefWideChar);
  end
  else
  begin
    FCharMatrix.SetDefaultItem(@FDefAnsiChar);
  end;
  FCharSetMatrix.SetDefaultItem(@FDefCharSet);
  FAttrMatrix.SetDefaultItem(@FDefAttr);
  FForeColorMatrix.SetDefaultItem(@FDefForeColor);
  FBackColorMatrix.SetDefaultItem(@FDefBackColor);

  {clear the matrices}
  FCharMatrix.Clear;
  FCharSetMatrix.Clear;
  FAttrMatrix.Clear;
  FForeColorMatrix.Clear;
  FBackColorMatrix.Clear;

  {clear all tab stops, set horizontal ones to every 8 chars}
  ClearAllHorzTabStops;
  ClearAllVertTabStops;
  i := 1;
  while (i < ColCount) do begin
    Col := i;
    SetHorzTabStop;
    inc(i, 8);
  end;

  {set the buffer modes}
  UseAbsAddress := ipc_TermBufUseAbsAddress;
  UseAutoWrap := ipc_TermBufUseAutoWrap;
  UseAutoWrapDelay := ipc_TermBufUseAutoWrapDelay;
  UseInsertMode := ipc_TermBufUseInsertMode;
  UseNewLineMode := ipc_TermBufUseNewLineMode;
  UseScrollRegion := ipc_TermBufUseScrollRegion;

  {reset the cursor position}
  FBeyondMargin := False;
  Row := 1;
  Col := 1;
end;
{--------}
procedure TIpTerminalBuffer.SetCharAttrs(const aValue : TIpTerminalCharAttrs);
begin
  FAttr := aValue;
  FAttrMatrix.SetDefaultItem(@FAttr);
end;
{--------}
procedure TIpTerminalBuffer.SetDefCharAttrs(const aValue : TIpTerminalCharAttrs);
begin
  FDefAttr := aValue;
end;
{--------}
procedure TIpTerminalBuffer.SetCursorPosition(aRow, aCol : Integer);
begin
  FBeyondMargin := False;
  Row := aRow;
  Col := aCol;
end;
{--------}
procedure TIpTerminalBuffer.SetScrollRegion(aTopRow, aBottomRow : Integer);
var
  Temp : Integer;
begin
  FBeyondMargin := False;
  {if the top row is greater than the bottom row, they're out of
   order, so switch 'em round}
  if (aTopRow > aBottomRow) then begin
    Temp := aTopRow;
    aTopRow := aBottomRow;
    aBottomRow := Temp;
  end;
  {$IFDEF UseRangeChecks}
  if ((aBottomRow - aTopRow) < 1) or (aTopRow < 1) or (aTopRow > RowCount) or
    (aBottomRow < 1) or (aBottomRow > RowCount) then
    raise Exception.Create(SInvScrollRow);
  {$ENDIF}
  FSRStartRow := tbCvtToInternalRow(aTopRow, True);
  FSREndRow := tbCvtToInternalRow(aBottomRow, True);
  {force the scroll region to be used}
  if UseScrollRegion then
    UseScrollRegion := False;
  if (aTopRow <> 1) or (aBottomRow <> RowCount) then
    UseScrollRegion := True;
end;
{--------}
procedure TIpTerminalBuffer.SetHorzTabStop;
begin
  if (ColCount <> 0) then
    IPTSetBit(FHorzTabStops, FCursorCol);
end;
{--------}
procedure TIpTerminalBuffer.SetVertTabStop;
begin
  if (RowCount <> 0) then
    IPTSetBit(FVertTabStops, FCursorRow);
end;
{--------}
function TIpTerminalBuffer.tbAtLastColumn : Boolean;
var
  MaxCol : Integer;
begin
  MaxCol := FDisplayOriginCol + FDisplayColCount - 1;
  Result := FCursorCol = MaxCol;
end;
{--------}
function TIpTerminalBuffer.tbCvtToExternalCol(aCol : Integer;
                                              aAbsolute : Boolean) : Integer;
begin
  {aCol is an internal reference (ie. zero-based and absolute), we
   need the external value (ie, one-based and relative to the start of
   the addressable area)}
  if aAbsolute then
    Result := aCol + 1
  else
    Result := aCol - FDisplayOriginCol + 1;
end;
{--------}
function TIpTerminalBuffer.tbCvtToExternalRow(aRow : Integer;
                                              aAbsolute : Boolean) : Integer;
begin
  {aRow is an internal reference (ie. zero-based and absolute), we
   need the external value (ie, one-based and relative to the start of
   the addressable area)}
  if aAbsolute then
    Result := aRow - (SVRowCount - RowCount) + 1
  else
    Result := aRow - FDisplayOriginRow + 1;
end;
{--------}
function TIpTerminalBuffer.tbCvtToInternalCol(aCol : Integer;
                                              aAbsolute : Boolean) : Integer;
begin
  {aCol is an external reference (ie, one-based and relative to the
   start of the addressable area), we need the internal value (ie.
   zero-based and absolute)}
  if aAbsolute then
    Result := aCol - 1
  else
    Result := aCol - 1 + FDisplayOriginCol;
end;
{--------}
function TIpTerminalBuffer.tbCvtToInternalRow(aRow : Integer;
                                              aAbsolute : Boolean) : Integer;
begin
  {aRow is an external reference (ie, one-based and relative to the
   start of the addressable area), we need the internal value (ie.
   zero-based and absolute)}
  if aAbsolute then
    Result := aRow - 1 + (SVRowCount - RowCount)
  else
    Result := aRow - 1 + FDisplayOriginRow;
end;
{--------}                                                             {!!.01}
procedure TIpTerminalBuffer.tbFireOnCursorMovedEvent;                  {!!.01}
begin                                                                  {!!.01}
  if (assigned (FOnCursorMoved)) and (FCursorMoved) then               {!!.01}
    FOnCursorMoved (Self, Row, Col);                                   {!!.01}
end;                                                                   {!!.01}
{--------}
function TIpTerminalBuffer.tbGetCol : Integer;
begin
  Result := tbCvtToExternalCol(FCursorCol, UseAbsAddress);
end;
{--------}
function TIpTerminalBuffer.tbGetOriginCol : Integer;
begin
  Result := tbCvtToExternalCol(FDisplayOriginCol, True);
end;
{--------}
function TIpTerminalBuffer.tbGetOriginRow : Integer;
begin
  Result := tbCvtToExternalRow(FDisplayOriginRow, True);
end;
{--------}
function TIpTerminalBuffer.tbGetRow : Integer;
begin
  Result := tbCvtToExternalRow(FCursorRow, UseAbsAddress);
end;
{--------}
procedure TIpTerminalBuffer.tbInvalidateRect(aFromRow, aFromCol,
                                             aToRow, aToCol : Integer);
var
  OurRect : TRect;
begin
  {convert the row and column values to external values}
  OurRect.Left := aFromCol + 1;
  OurRect.Top := aFromRow + RowCount - SVRowCount + 1;
  OurRect.Right := aToCol + 1;
  OurRect.Bottom := aToRow + RowCount - SVRowCount + 1;
  IPTIpdInvalidRect(PInvRect(FInvRectList), OurRect);
end;
{--------}
procedure TIpTerminalBuffer.tbMoveCursorLeftRight(aDirection : Integer;
                                                  aWrap      : Boolean;
                                                  aScroll    : Boolean);
var
  MaxCol   : Integer;
  MaxRow   : Integer;
  StartRow : Integer;
begin
  { if wrap is off, we just advance or retard the cursor position by }
  { one without extending beyond the left or right margin; scrolling is }
  { not possible in this case }
  MaxCol := FDisplayOriginCol + FDisplayColCount - 1;
  if not aWrap then begin
    if (aDirection < 0) then begin
      if (FCursorCol > FDisplayOriginCol) then begin
        FCursorMoved := True;
        dec(FCursorCol);
        tbFireOnCursorMovedEvent;                                      {!!.01}
      end;
    end
    else begin
      if (FCursorCol < MaxCol) then begin
        FCursorMoved := True;
        inc(FCursorCol);
        tbFireOnCursorMovedEvent;                                      {!!.01}
      end;
    end;
    Exit;
  end;
  MaxRow := FDisplayOriginRow + FDisplayRowCount - 1;
  { otherwise it's a wrap, as it were                                   }
  { there are several cases here                                        }
  {   1. if the new cursor position is within the same line, just move  }
  {      it as above                                                    }
  {   2. if the current cursor position is 0 and we're moving left...   }
  {      a. if we're not on the top display row, move the cursor to     }
  {         the last column of the previous row                         }
  {      b. if aScroll is False and we're on the top row, leave         }
  {         the cursor where it is                                      }
  {      c. if aScroll is True and we're on the top row, scroll the     }
  {         display down and move the cursor to the last column of the  }
  {         new top row                                                 }
  {   3. if the current cursor position is on the last column and we're }
  {      moving right...                                                }
  {      a. if we're not on the bottom display row, move the cursor to  }
  {         the first column of the next row                            }
  {      b. if aScroll is False and we're on the bottom row, leave the  }
  {         cursor where it is                                          }
  {      c. if aScroll is True and we're on the bottom, scroll the      }
  {         display up and move the cursor to the first column of the   }
  {         new bottom row                                              }
  if (aDirection < 0) then begin { moving left }
    if (FCursorCol > FDisplayOriginCol) then begin
      FCursorMoved := True;
      dec(FCursorCol);
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end
    else if (FCursorRow > FDisplayOriginRow) then begin
      FCursorMoved := True;
      FCursorCol := MaxCol;
      dec(FCursorRow);
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end
    else if aScroll then begin
      tbScrollRows(-1, FDisplayOriginRow, MaxRow);
      tbInvalidateRect(FDisplayOriginRow, 0, MaxRow, pred(ColCount));
      FCursorMoved := True;
      FCursorCol := MaxCol;
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end;
  end
  else {Direction > 0} begin {moving right}
    if (FCursorCol < MaxCol) then begin
      FCursorMoved := True;
      inc(FCursorCol);
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end
    else if (FCursorRow < MaxRow) then begin
      FCursorMoved := True;
      FCursorCol := FDisplayOriginCol;
      inc(FCursorRow);
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end
    else if aScroll then begin
      if UseScrollRegion then
        StartRow := FDisplayOriginRow
      else
        StartRow := 0;
      tbScrollRows(1, StartRow, MaxRow);
      tbInvalidateRect(FDisplayOriginRow, 0, MaxRow, pred(ColCount));
      FCursorMoved := True;
      FCursorCol := FDisplayOriginCol;
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbMoveCursorUpDown(aDirection : Integer;
                                               aScroll    : Boolean);
var
  MaxRow   : Integer;
  StartRow : Integer;
begin
  MaxRow := FDisplayOriginRow + FDisplayRowCount - 1;
  if (aDirection < 0) then begin
    if (FCursorRow > FDisplayOriginRow) then begin
      FCursorMoved := True;
      dec(FCursorRow);
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end
    else if aScroll then begin
      tbScrollRows(-1, FDisplayOriginRow, MaxRow);
      tbInvalidateRect(FDisplayOriginRow, 0, MaxRow, pred(ColCount));
    end;
  end
  else {Direction > 0} begin
    if (FCursorRow < MaxRow) then begin
      FCursorMoved := True;
      inc(FCursorRow);
      tbFireOnCursorMovedEvent;                                        {!!.01}                   
    end
    else if aScroll then begin
      if UseScrollRegion then
        StartRow := FDisplayOriginRow
      else
        StartRow := 0;
      tbScrollRows(1, StartRow, MaxRow);
      tbInvalidateRect(FDisplayOriginRow, 0, MaxRow, pred(ColCount));
    end;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbReallocBuffers(aNewRowCount : Integer;
                                             aNewColCount : Integer);
begin
  {check for changes in row count}
  if (aNewRowCount <> SVRowCount) then begin
    FCharMatrix.RowCount := aNewRowCount;
    FCharSetMatrix.RowCount := aNewRowCount;
    FAttrMatrix.RowCount := aNewRowCount;
    FForeColorMatrix.RowCount := aNewRowCount;
    FBackColorMatrix.RowCount := aNewRowCount;
    FSVRowCount := aNewRowCount;
  end
  {otherwise it's a change in column count}
  else begin
    FCharMatrix.ColCount := aNewColCount;
    FCharSetMatrix.ColCount := aNewColCount;
    FAttrMatrix.ColCount := aNewColCount;
    FForeColorMatrix.ColCount := aNewColCount;
    FBackColorMatrix.ColCount := aNewColCount;
    FColCount := aNewColCount;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbScrollRows(aCount, aTop, aBottom : Integer);
begin
  FCharMatrix.ScrollRows(aCount, aTop, aBottom);
  FCharSetMatrix.ScrollRows(aCount, aTop, aBottom);
  FAttrMatrix.ScrollRows(aCount, aTop, aBottom);
  FForeColorMatrix.ScrollRows(aCount, aTop, aBottom);
  FBackColorMatrix.ScrollRows(aCount, aTop, aBottom);
  if Assigned(FOnScrollRows) then
    FOnScrollRows(Self, aCount,
                  aTop + 1 - (SVRowCount - RowCount),
                  aBottom + 1 - (SVRowCount - RowCount));
end;
{--------}
procedure TIpTerminalBuffer.tbSetBackColor(aValue : TColor);
begin
  if (aValue <> BackColor) then begin
    FBackColor := aValue;
    FBackColorMatrix.SetDefaultItem(@FBackColor);
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetCharSet(aValue : Byte);
begin
  if (aValue <> CharSet) then begin
    FCharSet := aValue;
    FCharSetMatrix.SetDefaultItem(@FCharSet);
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetCol(aCol : Integer);
var
  OurCol : Integer;
begin
  FBeyondMargin := False;
  if (aCol <> Col) then begin
    OurCol := tbCvtToInternalCol(aCol, UseAbsAddress);
    if (OurCol < 0) then                                               {!!.01}
      OurCol := 0                                                      {!!.01}
    else if (OurCol >= ColCount) then                                  {!!.01}
      OurCol := pred(ColCount);                                        {!!.01}
    FCursorMoved := True;
    FCursorCol := OurCol;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetColCount(aNewCount : Integer);
begin
  {only do something if the value changes}
  if (aNewCount <> ColCount) then begin
    {check the new value is sensible}
    if (aNewCount < 2) then
      raise Exception.Create(Format(SCountTooSmall,
        ['TIpTerminalBuffer.tbSetColCount']));
    {reallocate the tab positions bitset}
    FHorzTabStops :=
       IPTReallocBitset(FHorzTabStops, ColCount, aNewCount);
    {if the number of scrollback rows is zero, just make a note of the
     new value: there won't have been any allocations yet}
    if (SVRowCount = 0) then begin
      FColCount := aNewCount;
      FDisplayColCount  := aNewCount;
    end
    {otherwise we can allocate new buffers and transfer over the old}
    else begin
      tbReallocBuffers(SVRowCount, aNewCount);
      tbInvalidateRect(SVRowCount - RowCount, 0,
                       pred(SVRowCount), pred(aNewCount));
      if UseScrollRegion then
        UseScrollRegion := False
      else begin
        FCursorRow := SVRowCount - RowCount;
        FCursorCol := 0;
        FDisplayColCount  := aNewCount;
      end;
    end;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetDefAnsiChar(aValue : AnsiChar);
begin
  if (aValue <> DefAnsiChar) then begin
    FDefAnsiChar := aValue;
    FCharMatrix.SetDefaultItem(@FDefAnsiChar);
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetDefBackColor(aValue : TColor);
begin
  if (FDefBackColor <> aValue) then begin
    FBackColorMatrix.ReplaceItems(@FDefBackColor, @aValue);
    FDefBackColor := aValue;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetDefForeColor(aValue : TColor);
begin
  if (FDefForeColor <> aValue) then begin
    FForeColorMatrix.ReplaceItems(@FDefForeColor, @aValue);
    FDefForeColor := aValue;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetForeColor(aValue : TColor);
begin
  if (aValue <> ForeColor) then begin
    FForeColor := aValue;
    FForeColorMatrix.SetDefaultItem(@FForeColor);
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetRow(aRow : Integer);
var
  OurRow : Integer;
begin
  FBeyondMargin := False;
  if (aRow <> Row) then begin
    OurRow := tbCvtToInternalRow(aRow, UseAbsAddress);
    if (OurRow < 0) then                                               {!!.01}
      OurRow := 0                                                      {!!.01}
    else if (OurRow >= SVRowCount) then                                {!!.01}
      OurRow := pred(SVRowCount);                                      {!!.01}
    FCursorMoved := True;
    FCursorRow := OurRow;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetRowCount(aNewCount : Integer);
begin
  { only do something if the value changes, and changes to something }
  { not greater than the scrollback view count }
  if (aNewCount <> RowCount) and
     (aNewCount <= SVRowCount) then begin
    { check the new value is sensible }
    if (aNewCount < 2) then
      raise Exception.Create(Format(SCountTooSmall,
        ['TIpTerminalBuffer.tbSetRowCount']));
    { changing the row count resets the scroll region }
    if UseScrollRegion then
      UseScrollRegion := False;
    { reallocate the tab positions bitset }
    FVertTabStops :=
       IPTReallocBitset(FVertTabStops, RowCount, aNewCount);
    { set the display origin and the cursor position }
    FDisplayOriginRow := FSVRowCount - aNewCount;
    if (FCursorRow < FDisplayOriginRow) then
      FCursorRow := FDisplayOriginRow;
    { save the new value }
    FRowCount := aNewCount;
    FDisplayRowCount := aNewCount;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetSVRowCount(aNewCount : Integer);
begin
  { only do something if the value changes }
  if (aNewCount <> SVRowCount) then begin
    { check the new value is sensible }
    if (aNewCount < 2) then
      raise Exception.Create(Format(SCountTooSmall, ['TIpTerminalBuffer.tbSetSVRowCount']));
    { if the new scrollback view count is less than the display view }
    { count, reduce the display view count to match }
    if (aNewCount < RowCount) then begin
      FRowCount := aNewCount;
      FDisplayRowCount := aNewCount;
    end;
    {set the display origin and the cursor position}
    FDisplayOriginRow := aNewCount - RowCount;
    FCursorRow := FCursorRow - SVRowCount + aNewCount;
    { if the number of columns is zero, just make a note of the new }
    { value: there won't have been any allocations yet }
    if (ColCount = 0) then
      FSVRowCount := aNewCount
    { otherwise we can allocate new buffers and transfer over the old }
    else begin
      tbReallocBuffers(aNewCount, ColCount);
      tbInvalidateRect(SVRowCount - RowCount, 0,
                       pred(SVRowCount), pred(aNewCount));
    end;
  end;
end;
{--------}
procedure TIpTerminalBuffer.tbSetUseScrollRegion(aValue : Boolean);
begin
  if (aValue <> UseScrollRegion) then begin
    {calculate the limits beyond which the cursor cannot move}
    if aValue {limit to scroll region} then begin
      FDisplayOriginCol := 0;
      FDisplayOriginRow := FSRStartRow;
      FDisplayColCount  := ColCount;
      FDisplayRowCount  := FSREndRow - FSRStartRow + 1;
    end
    else {limit to full display area} begin
      FDisplayOriginCol := 0;
      FDisplayOriginRow := SVRowCount - RowCount;
      FDisplayColCount  := ColCount;
      FDisplayRowCount  := RowCount;
    end;
    {rest the cursor to the top left corner of the allowed region}
    FCursorMoved := True;
    FCursorCol := FDisplayOriginCol;
    FCursorRow := FDisplayOriginRow;
    tbFireOnCursorMovedEvent;                                          {!!.01}                   
    {save the property value}
    FUseScrollRegion := aValue;
  end;
end;
{--------}
procedure TIpTerminalBuffer.WriteChar(aCh : char);
begin
  {this is performed as
    - write the character to the current cursor
    - write the current attributes, colors and charset to the current
      cursor
    - advance the cursor
   the latter operation may not do anything if UseAutoWrap is off and
   the cursor is at the end of the line, otherwise, if it's on, a
   scroll will occur}
  if FBeyondMargin then begin
    MoveCursorRight(UseAutoWrap, False);
    FBeyondMargin := False;
  end;
  if UseInsertMode then begin
    FCharMatrix.InsertItems(1, FCursorRow, FCursorCol);
    FCharSetMatrix.InsertItems(1, FCursorRow, FCursorCol);
    FAttrMatrix.InsertItems(1, FCursorRow, FCursorCol);
    FForeColorMatrix.InsertItems(1, FCursorRow, FCursorCol);
    FBackColorMatrix.InsertItems(1, FCursorRow, FCursorCol);
  end;
  FCharMatrix.WriteItems(@aCh, 1, FCursorRow, FCursorCol);
  FCharSetMatrix.WriteItems(@FCharSet, 1, FCursorRow, FCursorCol);
  FAttrMatrix.WriteItems(@FAttr, 1, FCursorRow, FCursorCol);
  FForeColorMatrix.WriteItems(@FForeColor, 1, FCursorRow, FCursorCol);
  FBackColorMatrix.WriteItems(@FBackColor, 1, FCursorRow, FCursorCol);
  tbInvalidateRect(FCursorRow, FCursorCol,
                   FCursorRow, FCursorCol);
  if (not UseAutoWrapDelay) or (not tbAtLastColumn) then
    MoveCursorRight(UseAutoWrap, False)
  else
    FBeyondMargin := True;
end;
{--------}
procedure TIpTerminalBuffer.WriteString(const aSt : string);
var
  i : Integer;
begin
  for i := 1 to length(aSt) do begin
    WriteChar(aSt[i]);
  end;
end;

const
  {The hash table sizes: these are prime numbers that suit these
   particular implementations}
  KBHashTableSize = 57;    {keyboard mapping hash table size}
  CSHashTableSize = 199;   {charset mapping hash table size}

  OurSignature : Longint = $33544841;
    { Note: $33544841 = AHT3 = APRO Hash Table, version 3 }
    { Use the same sig for IPRO }


type
  PKBHashNode = ^TKBHashNode;   {hash table node for keyboard maps}
  TKBHashNode = packed record
    kbnNext  : PKBHashNode;
    kbnKey   : PIpKeyString;
    kbnValue : PIpKeyString;
  end;

type
  PCSHashNode = ^TCSHashNode;   {hash table node for charset maps}
  TCSHashNode = packed record
    csnNext    : PCSHashNode;
    csnCharSet : PIpKeyString;
    csnFont    : PIpKeyString;
    csnChar    : AnsiChar;
    csnGlyph   : AnsiChar;
  end;

  PScriptNode = ^TScriptNode;
  TScriptNode = packed record
    snNext : PScriptNode;
    snFont : PIpKeyString;
    snText : PAnsiChar;
  end;


{===TCharQueue=======================================================}
const
  CharQueueDelta = 32;
type
  TCharQueue = class
  private
    FSize : Longint;
    FLen  : Longint;
    FText : PAnsiChar;
  protected
    function cqGetDupText : PAnsiChar;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aCh : AnsiChar);
    procedure Clear;
    property DupText : PAnsiChar read cqGetDupText;
  end;
{--------}
constructor TCharQueue.Create;
begin
  inherited Create;
  {allocate a starter character queue}
  GetMem(FText, CharQueueDelta);
  FSize := CharQueueDelta;
  FText[0] := #0;
end;
{--------}
destructor TCharQueue.Destroy;
begin
  if (FText <> nil) then
    FreeMem(FText, FSize);
  inherited Destroy;
end;
{--------}
procedure TCharQueue.Add(aCh : AnsiChar);
var
  NewQ : PAnsiChar;
begin
  if (FLen = FSize-1) then begin
    GetMem(NewQ, FSize + CharQueueDelta);
    StrCopy(NewQ, FText);
    FreeMem(FText, FSize);
    inc(FSize, CharQueueDelta);
    FText := NewQ;
  end;
  FText[FLen] := aCh;
  inc(FLen);
  FText[FLen] := #0;
end;
{--------}
procedure TCharQueue.Clear;
begin
  FLen := 0;
  FText[0] := #0;
end;
{--------}
function TCharQueue.cqGetDupText : PAnsiChar;
begin
  GetMem(Result, FLen+1);
  StrCopy(Result, FText);
end;
{====================================================================}

{===Helper routines==================================================}
{Note: The ELF hash functions are described in "Practical Algorithms
       For Programmers" by Andrew Binstock and John Rex, Addison
       Wesley, with modifications in Dr Dobbs Journal, April 1996.
       They're modified to suit this implementation.}
function HashELF(const S : TIpKeyString) : Longint;
var
  G : Longint;
  i : Integer;
begin
  Result := 0;
  for i := 1 to length(S) do begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and Longint($F0000000);
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;
{--------}
function HashELFPlusChar(const S : TIpKeyString;
                               C : AnsiChar) : Longint;
var
  G : Longint;
  i : Integer;
begin
  Result := ord(C);
  G := Result and Longint($F0000000);
  if (G <> 0) then
    Result := Result xor (G shr 24);
  Result := Result and (not G);
  for i := 1 to length(S) do begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and Longint($F0000000);
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;
{--------}
function AllocKeyString(const aSt : TIpKeyString) : PIpKeyString;
begin
  GetMem(Result, succ(length(aSt)));
  Result^ := aSt;
end;
{--------}
procedure FreeKeyString(aKS : PIpKeyString);
begin
  if (aKS <> nil) then
    FreeMem(aKS, succ(length(aKS^)));
end;
{--------}
function ProcessCharSetLine(const aLine : ShortString;
                              var aCharSet : TIpKeyString;
                              var aFromCh  : AnsiChar;
                              var aToCh    : AnsiChar;
                              var aFontName: TIpKeyString;
                              var aGlyph   : AnsiChar) : Boolean;
var
  InWord    : Boolean;
  CharInx   : Integer;
  StartCh   : Integer;
  QuoteMark : AnsiChar;
  WordStart : array [0..4] of Integer;
  WordEnd   : array [0..4] of Integer;
  WordCount : Integer;
  WordLen   : Integer;
  Chars     : array [0..4] of AnsiChar;
  i         : Integer;
  AsciiCh   : Integer;
  ec        : Integer;
  TestSt    : string[3];
begin
  {assumption: the line has had trailing spaces stripped, the line is
   not the empty string, the line starts with a ' ' character

  {assume we'll fail to parse the line properly}
  Result := False;

  {extract out the 5 words; if there are not at least 5 words, exit}
  QuoteMark := ' '; {needed to fool the compiler}
  StartCh := 0;     {needed to fool the compiler}
  InWord := False;
  WordCount := 0;
  CharInx := 1;
  while CharInx <= length(aLine) do begin
    if InWord then begin
      if (QuoteMark = ' ') then begin
        if (aLine[CharInx] = ' ') then begin
          InWord := False;
          WordStart[WordCount] := StartCh;
          WordEnd[WordCount] := pred(CharInx);
          inc(WordCount);
          if (WordCount = 5) then
            Break;
        end
      end
      else {the quotemark is active} begin
        if (aLine[CharInx] = QuoteMark) then
          QuoteMark := ' ';
      end;
    end
    else {not in a word} begin
      if (aLine[CharInx] <> ' ') then begin
        InWord := True;
        StartCh := CharInx;
        QuoteMark := aLine[CharInx];
        if (QuoteMark <> '''') and (QuoteMark <> '"') then
          QuoteMark := ' ';
      end;
    end;
    inc(CharInx);
  end;
  {when we reach this point we know where the last word ended}
  if InWord then begin
    if (QuoteMark <> ' ') then
      Exit; {the last word had no close quote}
    WordStart[WordCount] := StartCh;
    WordEnd[WordCount] := pred(CharInx);
    inc(WordCount);
  end;
  if (WordCount <> 5) then
    Exit;
  {fix the quoted strings}
  for i := 0 to 4 do begin
    if (aLine[WordStart[i]] = '''') or
       (aLine[WordStart[i]] = '"') then begin
      inc(WordStart[i]);
      dec(WordEnd[i]);
      if (WordEnd[i] < WordStart[i]) then
        Exit; {the word was either '' or ""}
    end;
  end;
  {we now know where each word can be found; the only special words
   are words 1, 2, and 4 which must be single characters, or ASCII
   values of the form \xnn}
  for i := 1 to 4 do
    if (i <> 3) then begin
      WordLen := succ(WordEnd[i] - WordStart[i]);
      if (WordLen = 1) then
        Chars[i] := aLine[WordStart[i]]
      else if (WordLen = 4) then begin
        CharInx := WordStart[i];
        if (aLine[CharInx] <> '\') or
           (aLine[CharInx+1] <> 'x') then
          Exit;
        TestSt := Copy(aLine, CharInx+1, 3);
        TestSt[1]:= '$';
        Val(TestSt, AsciiCh, ec);
        if (ec <> 0) then
          Exit;
        Chars[i] := chr(AsciiCh);
      end
      else
        Exit; {unknown format}
    end;
  {return values}
  aFromCh := Chars[1];
  aToCh := Chars[2];
  aGlyph := Chars[4];
  aCharSet := Copy(aLine, WordStart[0], succ(WordEnd[0] - WordStart[0]));
  aFontName := Copy(aLine, WordStart[3], succ(WordEnd[3] - WordStart[3]));
  Result := True;
end;
{====================================================================}


{===TIpKeyboardMapping==================================================}
constructor TIpKeyboardMapping.Create;
begin
  inherited Create;
  FTable := TList.Create;
  FTable.Count := KBHashTableSize;
end;
{--------}
destructor TIpKeyboardMapping.Destroy;
begin
  if (FTable <> nil) then begin
    Clear;
    FTable.Destroy;
  end;
  inherited Destroy;
end;
{--------}
function TIpKeyboardMapping.Add(const aKey   : TIpKeyString;
                             const aValue : TIpKeyString) : Boolean;
var
  Inx  : Integer;
  Node : PKBHashNode;
begin
  if kbmFindPrim(aKey, Inx, Pointer(Node)) then
    Result := False
  else begin
    Result := True;
    New(Node);
    Node^.kbnNext := FTable[Inx];
    Node^.kbnKey := AllocKeyString(aKey);
    Node^.kbnValue := AllocKeyString(aValue);
    FTable[Inx] := Node;
    inc(FCount);
  end;
end;
{--------}
procedure TIpKeyboardMapping.Clear;
var
  i    : Integer;
  Node : PKBHashNode;
  Temp : PKBHashNode;
begin
  for i := 0 to pred(KBHashTableSize) do begin
    Node := FTable[i];
    while (Node <> nil) do begin
      Temp := Node;
      Node := Node^.kbnNext;
      FreeKeyString(Temp^.kbnKey);
      FreeKeyString(Temp^.kbnValue);
      Dispose(Temp);
    end;
    FTable[i] := nil;
  end;
  FCount := 0;
end;
{--------}
{$IFDEF CompileDebugCode}
procedure TIpKeyboardMapping.DebugPrint(const aFileName : string);
var
  F    : text;
  i    : Integer;
  Node : PKBHashNode;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  for i := 0 to pred(KBHashTableSize) do begin
    writeln(F, '---', i, '---');
    Node := FTable[i];
    while (Node <> nil) do begin
      writeln(F, Node^.kbnKey^:20, Node^.kbnValue^:20);
      Node := Node^.kbnNext;
    end;
  end;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count/CSHashTableSize:5:3, ')');

  System.Close(F);
end;
{$ENDIF}
{--------}
function TIpKeyboardMapping.Get(const aKey : TIpKeyString) : TIpKeyString;
var
  Inx  : Integer;
  Node : PKBHashNode;
begin
  if kbmFindPrim(aKey, Inx, Pointer(Node)) then
    Result := Node^.kbnValue^
  else
    Result := '';
end;
{--------}
function TIpKeyboardMapping.kbmFindPrim(const aKey  : TIpKeyString;
                                          var aInx  : Integer;
                                          var aNode : Pointer) : Boolean;
var
  Node : PKBHashNode;
begin
  {assume we won't find aKey}
  Result := False;
  aNode := nil;
  {calculate the index, ie hash, of the key}
  aInx := HashELF(aKey) mod KBHashTableSize;
  {traverse the linked list at this entry, looking for the key in each
   node we encounter--a case-sensitive comparison}
  Node := FTable[aInx];
  while (Node <> nil) do begin
    if (aKey = Node^.kbnKey^) then begin
      Result := True;
      aNode := Node;
      Exit;
    end;
    Node := Node^.kbnNext;
  end;
end;
{--------}
procedure TIpKeyboardMapping.LoadFromFile(const aFileName : string);
var
  Lines     : TStringList;
  ActualLen : Integer;
  i         : Integer;
  LineInx   : Integer;
  Word1Start: Integer;
  Word1End  : Integer;
  Word2Start: Integer;
  Word2End  : Integer;
  LookingForStart : Boolean;
  Line      : string[255];
begin
  {clear the hash table, ready for loading}
  Clear;
  {create the stringlist to hold the keymap script}
  Lines := TStringList.Create;
  try
    {load the keymap script}
    Lines.LoadFromFile(aFileName);
    for LineInx := 0 to pred(Lines.Count) do begin
      {get this line}
      Line := Lines[LineInx];
      {remove trailing spaces}
      ActualLen := length(Line);
      for i := ActualLen downto 1 do
        if (Line[i] = ' ') then
          dec(ActualLen)
        else
          Break;
      Line[0] := chr(ActualLen);
      {only process detail lines}
      if (Line <> '') and (Line[1] <> '*') then begin
        {identify the first 'word'}
        Word1Start := 0;
        Word1End := 0;
        LookingForStart := True;
        for i := 1 to ActualLen do begin
          if LookingForStart then begin
            if (Line[i] <> ' ') then begin
              Word1Start := i;
              LookingForStart := False;
            end;
          end
          else {looking for end} begin
            if (Line[i] = ' ') then begin
              Word1End := i - 1;
              Break;
            end;
          end;
        end;
        {if we've set Word1End then there are at least two words in
         the line, otherwise there was only one word (which we shall
         ignore)}
        if (Word1End <> 0) then begin
          {identify the second 'word'}
          Word2Start := 0;
          Word2End := 0;
          LookingForStart := True;
          for i := succ(Word1End) to ActualLen do begin
            if LookingForStart then begin
              if (Line[i] <> ' ') then begin
                Word2Start := i;
                LookingForStart := False;
              end;
            end
            else {looking for end} begin
              if (Line[i] = ' ') then begin
                Word2End := i - 1;
                Break;
              end;
            end;
          end;
          if (Word2End = 0) then
            Word2End := ActualLen;
          {add the key and value to the hash table}
          Add(System.Copy(Line, Word1Start, succ(Word1End-Word1Start)),
              System.Copy(Line, Word2Start, succ(Word2End-Word2Start)));
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;
{--------}
procedure TIpKeyboardMapping.LoadFromRes(aInstance : THandle;
                                const aResName  : string);
var
  MS        : TMemoryStream;
  ResInfo   : THandle;
  ResHandle : THandle;
  ResNameZ  : PAnsiChar;
  Res       : PByteArray;
  i         : Integer;
  Sig       : Longint;
  ResCount  : Longint;
  BytesRead : Longint;
  Key       : TIpKeyString;
  Value     : TIpKeyString;
begin
  {Note: this code has been written to work with all versions of
   Delphi, both 16-bit and 32-bit. Hence it does not make use of any
   of the features available in later compilers, ie, typecasting a
   string to a PAnsiChar, or TResourceStream)

  {clear the hash table, ready for loading}
  Clear;
  {get the resource info handle}
  GetMem(ResNameZ, succ(length(aResName)));
  try
    StrPCopy(ResNameZ, aResName);
    ResInfo := FindResource(aInstance, ResNameZ, RT_RCDATA);
  finally
    FreeMem(ResNameZ, succ(length(aResName)));
  end;
  if (ResInfo = 0) then
    Exit;
  {load and lock the resource}
  ResHandle := LoadResource(aInstance, ResInfo);
  if (ResHandle = 0) then
    Exit;
  Res := LockResource(ResHandle);
  if (Res = nil) then begin
    FreeResource(ResHandle);
    Exit;
  end;
  try
    {create a memory stream}
    MS := TMemoryStream.Create;
    try
      {copy the resource to our memory stream}
      MS.Write(Res^, SizeOfResource(aInstance, ResInfo));
      MS.Position := 0;
      {read the header signature, get out if it's not ours}
      BytesRead := MS.Read(Sig, sizeof(Sig));
      if (BytesRead <> sizeof(Sig)) or (Sig <> OurSignature) then
        Exit;
      {read the count of key/value string pairs in the resource}
      MS.Read(ResCount, sizeof(ResCount));
      {read that number of key/value string pairs and add them to the
       hash table}
      for i := 0 to pred(ResCount) do begin
        MS.Read(Key[0], 1);
        MS.Read(Key[1], ord(Key[0]));
        MS.Read(Value[0], 1);
        MS.Read(Value[1], ord(Value[0]));
        Add(Key, Value);
      end;
      {read the footer signature, clear and get out if it's not ours}
      BytesRead := MS.Read(Sig, sizeof(Sig));
      if (BytesRead <> sizeof(Sig)) or (Sig <> OurSignature) then begin
        Clear;
        Exit;
      end;
    finally
      MS.Free;
    end;
  finally
    UnlockResource(ResHandle);
    FreeResource(ResHandle);
  end;
end;
{--------}
procedure TIpKeyboardMapping.StoreToBinFile(const aFileName : string);
var
  aFS  : TFileStream;
  i    : Integer;
  Node : PKBHashNode;
begin
  {create a file stream}
  aFS := TFileStream.Create(aFileName, fmCreate);
  try
    {write our signature as header}
    aFS.Write(OurSignature, sizeof(OurSignature));
    {write the number of key/value string pairs}
    aFS.Write(FCount, sizeof(FCount));
    {write all the key/value string pairs}
    for i := 0 to pred(KBHashTableSize) do begin
      Node := FTable[i];
      while (Node <> nil) do begin
        aFS.Write(Node^.kbnKey^, succ(length(Node^.kbnKey^)));
        aFS.Write(Node^.kbnValue^, succ(length(Node^.kbnValue^)));
        Node := Node^.kbnNext;
      end;
    end;
    {write our signature as footer as a check}
    aFS.Write(OurSignature, sizeof(OurSignature));
  finally
    aFS.Free;
  end;
end;

{===TIpCharSetMapping================================================}
constructor TIpCharSetMapping.Create;
begin
  inherited Create;
  FTable := TList.Create;
  FTable.Count := CSHashTableSize;
  FCharQueue := Pointer(TCharQueue.Create);
end;
{--------}
destructor TIpCharSetMapping.Destroy;
var
  Temp, Walker : PScriptNode;
begin
  {free the hash table}
  if (FTable <> nil) then begin
    Clear;
    FTable.Destroy;
  end;
  {free the character queue}
  TCharQueue(FCharQueue).Free;
  {free the script node freelist}
  Walker := FScriptFreeList;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.snNext;
    Dispose(Temp);
  end;
  inherited Destroy;
end;
{--------}
function TIpCharSetMapping.Add(const aCharSet : TIpKeyString;
                                     aFromCh  : AnsiChar;
                                     aToCh    : AnsiChar;
                               const aFont    : TIpKeyString;
                                     aGlyph   : AnsiChar) : Boolean;
var
  Inx  : Integer;
  Node : PCSHashNode;
  Ch   : AnsiChar;
  Glyph: AnsiChar;
begin
  {we must do this in two stages: first, determine that we can add
   *all* the character mappings; second, do so}

  {stage one: check no mapping currently exists}
  Result := False;
  for Ch := aFromCh to aToCh do begin
    if csmFindPrim(aCharSet, Ch, Inx, Pointer(Node)) then
      Exit;
  end;
  {stage two: add all charset/char mappings}
  Result := True;
  Glyph := aGlyph;
  for Ch := aFromCh to aToCh do begin
    Inx := HashELFPlusChar(aCharSet, Ch) mod CSHashTableSize;
    New(Node);
    Node^.csnNext := FTable[Inx];
    Node^.csnCharSet := AllocKeyString(aCharSet);
    Node^.csnFont := AllocKeyString(aFont);
    Node^.csnChar := Ch;
    Node^.csnGlyph := Glyph;
    FTable[Inx] := Node;
    inc(FCount);
    inc(Glyph);
  end;
end;
{--------}
procedure TIpCharSetMapping.Clear;
var
  i    : Integer;
  Node : PCSHashNode;
  Temp : PCSHashNode;
begin
  {free the script: in a moment there's going to be no mapping}
  csmFreeScript;
  {clear out the hash table}
  for i := 0 to pred(CSHashTableSize) do begin
    Node := FTable[i];
    while (Node <> nil) do begin
      Temp := Node;
      Node := Node^.csnNext;
      FreeKeyString(Temp^.csnCharSet);
      FreeKeyString(Temp^.csnFont);
      Dispose(Temp);
    end;
    FTable[i] := nil;
  end;
  FCount := 0;
end;
{--------}
procedure TIpCharSetMapping.csmAddScriptNode(aFont : PIpKeyString);
var
  Node : PScriptNode;
begin
  {allocate and set up the new node}
  if (FScriptFreeList = nil) then
    New(Node)
  else begin
    Node := FScriptFreeList;
    FScriptFreeList := Node^.snNext;
  end;
  Node^.snNext := nil;
  Node^.snFont := aFont;
  Node^.snText := TCharQueue(FCharQueue).DupText;
  {add the node to the script}
  if (FScript <> nil) then
    PScriptNode(FScriptEnd)^.snNext := Node
  else
    FScript := Node;
  {update the tail Pointer}
  FScriptEnd := Node;
end;
{--------}
function TIpCharSetMapping.csmFindPrim(const aCharSet : TIpKeyString;
                                             aChar    : AnsiChar;
                                         var aInx     : Integer;
                                         var aNode    : Pointer) : Boolean;
var
  Node : PCSHashNode;
begin
  {assume we won't find aCharSet/aChar}
  Result := False;
  aNode := nil;
  {calculate the index, ie hash, of the charset/char}
  aInx := HashELFPlusChar(aCharSet, aChar) mod CSHashTableSize;
  {traverse the linked list at this entry, looking for the character
   in each node we encounter--a case-sensitive comparison--if we get a
   match, compare the character set name as well, again case-
   insensitive}
  Node := FTable[aInx];
  while (Node <> nil) do begin
    if (aChar = Node^.csnChar) then begin
      if (aCharSet = Node^.csnCharSet^) then begin
        Result := True;
        aNode := Node;
        Exit;
      end;
    end;
    Node := Node^.csnNext;
  end;
end;
{--------}
procedure TIpCharSetMapping.csmFreeScript;
var
  Walker, Temp : PScriptNode;
begin
  Walker := FScript;
  FScript := nil;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.snNext;
    FreeMem(Temp^.snText, StrLen(Temp^.snText));
    {NOTE: we do NOT free the font name: it's a copy of an allocated
     string in the mapping hash table}
    Temp^.snNext := FScriptFreeList;
    FScriptFreeList := Temp;
  end;
end;
{--------}
{$IFDEF CompileDebugCode}
procedure TIpCharSetMapping.DebugPrint(const aFileName : string);
var
  F    : text;
  i    : Integer;
  Node : PCSHashNode;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);

  for i := 0 to pred(CSHashTableSize) do begin
    writeln(F, '---', i, '---');
    Node := FTable[i];
    while (Node <> nil) do begin
      writeln(F, Node^.csnCharSet^:20,
                 ord(Node^.csnChar):4,
                 Node^.csnFont^:20,
                 ord(Node^.csnGlyph):4);
      Node := Node^.csnNext;
    end;
  end;

  writeln(F);
  writeln(F, 'Count: ', Count, ' (mean: ', Count/CSHashTableSize:5:3, ')');

  System.Close(F);
end;
{$ENDIF}
{--------}
procedure TIpCharSetMapping.GenerateDrawScript(const aCharSet : TIpKeyString;
                                                     aText    : PAnsiChar);
var
  i    : Integer;
  Inx  : Integer;
  TextLen  : Integer;
  Node     : PCSHashNode;
  Ch       : AnsiChar;
  CurFont  : PIpKeyString;
  ThisFont : PIpKeyString;
  ThisChar : AnsiChar;
begin
  {nothing to do if the string is empty}
  TextLen := StrLen(aText);
  if (TextLen = 0) then
    Exit;
  {destroy any current script}
  csmFreeScript;
  TCharQueue(FCharQueue).Clear;
  {we don't yet have a font name}
  CurFont := nil;
  {read the text, char by char}
  for i := 0 to pred(TextLen) do begin
    {look up this charset/char in the hash table}
    Ch := aText[i];
    if csmFindPrim(aCharSet, Ch, Inx, Pointer(Node)) then begin
      {found it, use the named font and glyph}
      ThisFont := Node^.csnFont;
      ThisChar := Node^.csnGlyph;
    end
    else begin
      {if not found, use the default font and glyph}
      ThisFont := @DefaultFontName;
      ThisChar := Ch;
    end;
    {if the font has changed, create a script node for the previous
     font}
    if (CurFont = nil) then
      CurFont := ThisFont;
    if (CurFont^ <> ThisFont^) then begin
      csmAddScriptNode(CurFont);
      CurFont := ThisFont;
      TCharQueue(FCharQueue).Clear;
    end;
    {add this character to the current string}
    TCharQueue(FCharQueue).Add(ThisChar);
  end;
  {add the final script node to finish off the string}
  csmAddScriptNode(CurFont);
  TCharQueue(FCharQueue).Clear;
end;
{--------}
procedure TIpCharSetMapping.GetFontNames(aList : TStrings);
var
  i    : Integer;
  Node : PCSHashNode;
  PrevFont : string;
begin
  aList.Clear;
  PrevFont := '';
  for i := 0 to pred(CSHashTableSize) do begin
    Node := FTable[i];
    while (Node <> nil) do begin
      if (CompareText(Node^.csnFont^, PrevFont) <> 0) then begin
        PrevFont := Node^.csnFont^;
        if (aList.IndexOf(PrevFont) = -1) then
          aList.Add(PrevFont);
      end;
      Node := Node^.csnNext;
    end;
  end;
end;
{--------}
function TIpCharSetMapping.GetNextDrawCommand(var aFont : TIpKeyString;
                                                  aText : PAnsiChar) : Boolean;
var
  Temp : PScriptNode;
begin
  {start off with the obvious case: there's no script}
  if (FScript = nil) then begin
    Result := False;
    Exit;
  end;
  {we'll definitely return something}
  Result := True;
  {return the data from the top node}
  aFont := PScriptNode(FScript)^.snFont^;
  StrCopy(aText, PScriptNode(FScript)^.snText);
  {unlink the top node}
  Temp := PScriptNode(FScript);
  FScript := Temp^.snNext;
  {free the unlinked top node}
  FreeMem(Temp^.snText, StrLen(Temp^.snText));
  {NOTE: we do NOT free the font name: it's a copy of an allocated
   string in the mapping hash table}
  Temp^.snNext := FScriptFreeList;
  FScriptFreeList := Temp;
end;
{--------}
procedure TIpCharSetMapping.LoadFromFile(const aFileName : string);
var
  Lines     : TStringList;
  ActualLen : Integer;
  i         : Integer;
  LineInx   : Integer;
  Line      : string[255];
  CharSet   : TIpKeyString;
  FontName  : TIpKeyString;
  FromCh    : AnsiChar;
  ToCh      : AnsiChar;
  Glyph     : AnsiChar;
begin
  {clear the hash table, ready for loading}
  Clear;
  {create the stringlist to hold the mapping script}
  Lines := TStringList.Create;
  try
    {load the mapping script}
    Lines.LoadFromFile(aFileName);
    for LineInx := 0 to pred(Lines.Count) do begin
      {get this line}
      Line := Lines[LineInx];
      {remove trailing spaces}
      ActualLen := length(Line);
      for i := ActualLen downto 1 do
        if (Line[i] = ' ') then
          dec(ActualLen)
        else
          Break;
      Line[0] := chr(ActualLen);
      {only process detail lines}
      if (Line <> '') and (Line[1] = ' ') then begin
        if ProcessCharSetLine(Line, CharSet, FromCh, ToCh, FontName, Glyph) then
          Add(CharSet, FromCh, ToCh, FontName, Glyph);
      end;
    end;
  finally
    Lines.Free;
  end;
end;
{--------}
procedure TIpCharSetMapping.LoadFromRes(aInstance : THandle;
                                  const aResName  : string);
var
  MS        : TMemoryStream;
  ResInfo   : THandle;
  ResHandle : THandle;
  ResNameZ  : PAnsiChar;
  Res       : PByteArray;
  i         : Integer;
  Sig       : Longint;
  ResCount  : Longint;
  BytesRead : Longint;
  CharSet   : TIpKeyString;
  Font      : TIpKeyString;
  Ch        : AnsiChar;
  Glyph     : AnsiChar;
begin
  {Note: this code has been written to work with all versions of
   Delphi, both 16-bit and 32-bit. Hence it does not make use of any
   of the features available in later compilers, ie, typecasting a
   string to a PChar, or TResourceStream)

  {clear the hash table, ready for loading}
  Clear;
  {get the resource info handle}
  GetMem(ResNameZ, succ(length(aResName)));
  try
    StrPCopy(ResNameZ, aResName);
    ResInfo := FindResource(aInstance, ResNameZ, RT_RCDATA);
  finally
    FreeMem(ResNameZ, succ(length(aResName)));
  end;
  if (ResInfo = 0) then
    Exit;
  {load and lock the resource}
  ResHandle := LoadResource(aInstance, ResInfo);
  if (ResHandle = 0) then
    Exit;
  Res := LockResource(ResHandle);
  if (Res = nil) then begin
    FreeResource(ResHandle);
    Exit;
  end;
  try
    {create a memory stream}
    MS := TMemoryStream.Create;
    try
      {copy the resource to our memory stream}
      MS.Write(Res^, SizeOfResource(aInstance, ResInfo));
      MS.Position := 0;
      {read the header signature, get out if it's not ours}
      BytesRead := MS.Read(Sig, sizeof(Sig));
      if (BytesRead <> sizeof(Sig)) or (Sig <> OurSignature) then
        Exit;
      {read the count of mappings in the resource}
      MS.Read(ResCount, sizeof(ResCount));
      {read that number of mappings and add them to the hash table}
      for i := 0 to pred(ResCount) do begin
        MS.Read(CharSet[0], 1);
        MS.Read(CharSet[1], ord(CharSet[0]));
        MS.Read(Font[0], 1);
        MS.Read(Font[1], ord(Font[0]));
        MS.Read(Ch, 1);
        MS.Read(Glyph, 1);
        Add(CharSet, Ch, Ch, Font, Glyph);
      end;
      {read the footer signature, clear and get out if it's not ours}
      BytesRead := MS.Read(Sig, sizeof(Sig));
      if (BytesRead <> sizeof(Sig)) or (Sig <> OurSignature) then begin
        Clear;
        Exit;
      end;
    finally
      MS.Free;
    end;
  finally
    UnlockResource(ResHandle);
    FreeResource(ResHandle);
  end;
end;
{--------}
procedure TIpCharSetMapping.StoreToBinFile(const aFileName : string);
var
  aFS  : TFileStream;
  i    : Integer;
  Node : PCSHashNode;
begin
  {create a file stream}
  aFS := TFileStream.Create(aFileName, fmCreate);
  try
    {write our signature as header}
    aFS.Write(OurSignature, sizeof(OurSignature));
    {write the number of mappings}
    aFS.Write(FCount, sizeof(FCount));
    {write all the mappings}
    for i := 0 to pred(CSHashTableSize) do begin
      Node := FTable[i];
      while (Node <> nil) do begin
        aFS.Write(Node^.csnCharSet^, succ(length(Node^.csnCharSet^)));
        aFS.Write(Node^.csnFont^, succ(length(Node^.csnFont^)));
        aFS.Write(Node^.csnChar, sizeof(AnsiChar));
        aFS.Write(Node^.csnGlyph, sizeof(AnsiChar));
        Node := Node^.csnNext;
      end;
    end;
    {write our signature as footer as a further check on reading}
    aFS.Write(OurSignature, sizeof(OurSignature));
  finally
    aFS.Free;
  end;
end;

{===TIpTerminalParser================================================}
constructor TIpTerminalParser.Create(aUseWideChar : Boolean);
begin
  inherited Create;
  FUseWideChar := aUseWideChar;
  FCommand := eNone;
end;
{--------}
destructor TIpTerminalParser.Destroy;
begin
  inherited Destroy;
end;
{--------}
procedure TIpTerminalParser.Clear;
begin
  {do nothing at this level}
end;
{--------}
function TIpTerminalParser.ProcessChar(aCh : AnsiChar) : TIpParserCmdType;
begin
  Result := pctNone;
end;
{--------}
function TIpTerminalParser.ProcessWideChar(aCh : WideChar) : TIpParserCmdType;
begin
  Result := pctNone;
end;
{--------}
function TIpTerminalParser.tpGetArgument(aInx : Integer) : Integer;
begin
  Result := 0;
end;
{--------}
function TIpTerminalParser.tpGetSequence : string;
begin
  Result := '';
end;
{====================================================================}


{====================================================================}
type
  PSeq = ^TSeq;
  TSeq = packed record
    sSize : Longint;
    sLen  : Longint;
    sText : array [1..10000] of AnsiChar;
  end;
{--------}
function ReAllocSeq(aSeq : PSeq; aSize : Longint) : PSeq;
var
  NewSeq : PSeq;
begin
  if (aSize = 0) then
    NewSeq := nil
  else begin
    GetMem(NewSeq, 2*sizeof(Longint) + aSize);
    NewSeq^.sSize := aSize;
    NewSeq^.sLen := 0;
    if (aSeq <> nil) then begin
      Move(aSeq^.sText, NewSeq^.sText, aSeq^.sLen);
      NewSeq^.sLen := aSeq^.sLen;
    end;
  end;
  if (aSeq <> nil) then
    FreeMem(aSeq, 2*sizeof(Longint) + aSeq^.sSize);
  Result := NewSeq;
end;
{--------}
procedure AddCharToSeq(var aSeq : PSeq; aCh : AnsiChar);
begin
  if (aSeq = nil) then
    aSeq := ReAllocSeq(aSeq, 64)
  else if (aSeq^.sSize = aSeq^.sLen) then
    aSeq := ReAllocSeq(aSeq, aSeq^.sSize + 64);
  inc(aSeq^.sLen);
  aSeq^.sText[aSeq^.sLen] := aCh;
end;
{--------}
procedure AssignSeqToChar(var aSeq : PSeq; aCh : AnsiChar);
begin
  if (aSeq <> nil) then
    aSeq^.sLen := 0;
  AddCharToSeq(aSeq, aCh);
end;
{--------}
procedure CopySeq(aFromSeq : PSeq; var aToSeq : PSeq);
begin
  if (aFromSeq = nil) then begin
    if (aToSeq <> nil) then
      aToSeq^.sLen := 0;
  end
  else begin
    if (aToSeq = nil) or
       (aToSeq^.sSize < aFromSeq^.sLen) then
      aToSeq := ReAllocSeq(aToSeq, aFromSeq^.sLen);
    if (aToSeq <> nil) then begin
      aToSeq^.sLen := aFromSeq^.sLen;
      Move(aFromSeq^.sText, aToSeq^.sText, aFromSeq^.sLen);
    end;
  end;
end;
{--------}
procedure DelCharFromSeq(aSeq : PSeq);
begin
  if (aSeq <> nil) and (aSeq^.sLen > 0) then
    dec(aSeq^.sLen);
end;
{--------}
procedure ClearSeq(aSeq : PSeq);
begin
  if (aSeq <> nil) then
    aSeq^.sLen := 0;
end;
{--------}
function GetSeqLength(aSeq : PSeq) : Integer;
begin
  Result := aSeq^.sLen;
end;
{--------}
function GetStringFromSeq(aSeq : PSeq) : string;
begin
  Result := '';
  if (aSeq <> nil) and (aSeq^.sLen > 0) then begin
    SetLength(Result, aSeq^.sLen);
    Move(aSeq^.sText, Result[1], aSeq^.sLen);
  end;
end;
{====================================================================}

const
  DECSCLseq : string[6] = ^['[61"p';

{===TIpVT100Parser===================================================}
constructor TIpVT100Parser.Create(aUseWideChar : Boolean);
begin
  inherited Create(aUseWideChar);
  FArgCount := 0;
  vtpGrowArgs;
  FInVT52Mode := False;
end;
{--------}
destructor TIpVT100Parser.Destroy;
begin
  FSequence := ReAllocSeq(FSequence, 0);
  FSavedSeq := ReAllocSeq(FSavedSeq, 0);
  if (FArgs <> nil) then begin
    FreeMem(FArgs, sizeof(Integer) * FArgCountMax);
    FArgs := nil;
    FArgCountMax := 0;
  end;
  inherited Destroy;
end;
{--------}
procedure TIpVT100Parser.Clear;
begin
  ClearSeq(FSequence);
  FCommand := eNone;
  if (FArgCount <> 0) then begin
    FillChar(FArgs^, sizeof(Integer) * FArgCount, 0);
    FArgCount := 0;
  end;
end;
{--------}
function TIpVT100Parser.ProcessChar(aCh : AnsiChar) : TIpParserCmdType;
begin
  {if the current state is psGotCommand, the previous character
   managed to complete a command. Before comtinuing we should clear
   all traces of the previous command and sequence}
  if (FState = psGotCommand) then begin
    FArgCount := 0;
    ClearSeq(FSequence);
    FCommand := eNone;
    FState := psIdle;
  end;

  {if the current state is psGotInterCommand, the previous character
   was non-displayable and a command; restore the previously saved
   state}
  if (FState = psGotInterCommand) then begin
    FArgCount := 0;
    FCommand := eNone;
    CopySeq(FSavedSeq, PSeq(FSequence));
    FState := FSavedState;
  end;

  {assume that the result is going to be that we are building up a
   command escape sequence}
  Result := pctPending;

  {add the character to the sequence string we're building up,
   although we may delete it later}
  AddCharToSeq(PSeq(FSequence), aCh);

  {if the character is non-displayable, process it immediately, even
   if we're in the middle of parsing some other command}
  if (aCh < ' ') then begin
    FSavedState := FState;
    DelCharFromSeq(FSequence);
    CopySeq(FSequence, PSeq(FSavedSeq));
    FState := psGotInterCommand;
    Result := pctComplete;
    case aCh of
      cENQ : begin {enquiry request}
               AssignSeqToChar(PSeq(FSequence), cENQ);
               FCommand := eENQ;
             end;
      cBel : begin {sound bell}
               AssignSeqToChar(PSeq(FSequence), cBel);
               FCommand := eBel;
             end;
      cBS  : begin {backspace}
               AssignSeqToChar(PSeq(FSequence), cBS);
               FCommand := eBS;
             end;
      cTab : begin {horizontal tab}
               AssignSeqToChar(PSeq(FSequence), cTab);
               FCommand := eCHT;
               FArgCount := 1;
               FArgs^[0] := 1; {ie a single tab}
             end;
      cLF  : begin
               AssignSeqToChar(PSeq(FSequence), cLF);
               FCommand := eLF;
             end;
      cVT  : begin
               AssignSeqToChar(PSeq(FSequence), cVT);
               FCommand := eCVT;
               FArgCount := 1;
               FArgs^[0] := 1; {ie a single tab}
             end;
      cFF  : begin {formfeed, equals clear screen}
               AssignSeqToChar(PSeq(FSequence), cFF);
               FCommand := eED;
               FArgCount := 1;
               FArgs^[0] := 2; {ie <esc>[2J}
             end;
      cCR  : begin {carriage return}
               AssignSeqToChar(PSeq(FSequence), cCR);
               FCommand := eCR;
             end;
      cSO  : begin {shift out character set, ie use G0}
               AssignSeqToChar(PSeq(FSequence), cSO);
               FCommand := eSO;
             end;
      cSI  : begin {shift in character set, ie use G1}
               AssignSeqToChar(PSeq(FSequence), cSI);
               FCommand := eSI;
             end;
      cCan,
      cSub : begin {abandon current escape sequence}
               Result := pctNone;
             end;
      cEsc : begin {start a new escape sequence}
               {abandon whatever escape sequence we're in}
               AssignSeqToChar(PSeq(FSequence), cEsc);
               FArgCount := 0;
               FState := psGotEscape;
               Result := pctPending;
             end;
    else
      {otherwise ignore the non-displayable char}
      DelCharFromSeq(FSequence);
      Result := pctNone;
    end;{case}
  end
  {otherwise parse the character}
  else begin
    case FState of
      psIdle :
        begin
          if (aCh < #127) then begin
            FState := psGotCommand;
            FCommand := eChar;
            Result := pctChar;
          end
          else {full 8-bit char} begin
            FState := psGotCommand;
            FCommand := eChar;
            Result := pct8bitChar;
          end;
        end;
      psGotEscape :
        if InVT52Mode then begin
          Result := vtpProcessVT52(aCh);
        end
        else {in VT100 mode} begin
          case aCh of
            '[' : FState := psParsingANSI;
            '(' : FState := psParsingLeftParen;
            ')' : FState := psParsingRightParen;
            '#' : FState := psParsingHash;
            '*', '+', '-', '.', '/' : FState := psParsingCharSet;
          else {it's a two character esc. seq.}
            FState := psGotCommand;
            Result := pctComplete;
            case aCh of
              '1' : begin {set graphics processor option on}
                      { NOT SUPPORTED }
                      FCommand := eNone;
                    end;
              '2' : begin {set graphics processor option off}
                      { NOT SUPPORTED }
                      FCommand := eNone;
                    end;
              '7' : begin {save cursor pos}
                      FCommand := eSaveCursorPos;
                    end;
              '8' : begin {restore cursor pos}
                      FCommand := eRestoreCursorPos;
                    end;
              '<' : begin {switch to ANSI--ie, do nothing}
                      FCommand := eNone;
                      Result := pctNone;
                    end;
              '=' : begin {set application keypad mode}
                      FCommand := eSM;
                      FArgCount := 2;
                      FArgs^[0] := -2;
                      FArgs^[1] := 999; {special iPRO code!}
                    end;
              '>' : begin {set numeric keypad mode}
                      FCommand := eRM;
                      FArgCount := 2;
                      FArgs^[0] := -2;
                      FArgs^[1] := 999; {special iPRO code!}
                    end;
              'D' : begin {index = cursor down + scroll}
                      FCommand := eIND2;
                    end;
              'E' : begin {next line}
                      FCommand := eNEL;
                    end;
              'H' : begin {set horx tab stop}
                      FCommand := eHTS;
                    end;
              'M' : begin {reverse index = cursor up + scroll}
                      FCommand := eRI;
                    end;

              'Z' : begin {device attributes}
                      FCommand := eDA;
                      FArgCount := 1;
                      FArgs^[0] := 0; {stands for VT100}
                    end;
              'c' : begin
                      FCommand := eRIS;
                    end;
            else
              {ignore the char & seq.--it's not one we know}
              Result := pctNone;
            end;{case}
          end;{case}
        end;
      psParsingANSI :
        begin
          if (#$40 <= aCh) and (aCh < #$7F) then begin
            {the command is now complete-see if we know about it}
            FState := psGotCommand;
            Result := vtpParseANSISeq(aCh);
          end;
          {otherwise, the next character has already been added to
           the sequence string, so there's nothing extra to do}
        end;
      psParsingLeftParen :
        begin
          if ('0' <= aCh) and (aCh <= '~') then begin
            {the command is complete}
            if (GetSeqLength(FSequence) = 3) then begin
              FState := psGotCommand;
              Result := pctComplete;
              FCommand := eDECSCS;
              FArgCount := 2;
              FArgs^[0] := 0; {0 = set G0 charset}
              case aCh of
                'A' : FArgs^[1] := ord('A');
                'B' : FArgs^[1] := ord('B');
                '0' : FArgs^[1] := 0;
                '1' : FArgs^[1] := 1;
                '2' : FArgs^[1] := 2;
              else
                {ignore the char & seq.--it's not one we know}
                FState := psGotCommand;
                Result := pctNone;
                FCommand := eNone;
                FArgCount := 0;
              end;{case}
            end
            else {sequence is too long} begin
              FState := psGotCommand;
              Result := pctNone;
              FCommand := eNone;
              FArgCount := 0;
            end;
          end;
        end;
      psParsingRightParen :
        begin
          if ('0' <= aCh) and (aCh <= '~') then begin
            {the command is complete}
            if (GetSeqLength(FSequence) = 3) then begin
              FState := psGotCommand;
              Result := pctComplete;
              FCommand := eDECSCS;
              FArgCount := 2;
              FArgs^[0] := 1; {0 = set G1 charset}
              case aCh of
                'A' : FArgs^[1] := ord('A');
                'B' : FArgs^[1] := ord('B');
                '0' : FArgs^[1] := 0;
                '1' : FArgs^[1] := 1;
                '2' : FArgs^[1] := 2;
              else
                {ignore the char & seq.--it's not one we know}
                FState := psGotCommand;
                Result := pctNone;
                FCommand := eNone;
                FArgCount := 0;
              end;{case}
            end
            else {sequence is too long} begin
              FState := psGotCommand;
              Result := pctNone;
              FCommand := eNone;
              FArgCount := 0;
            end;
          end;
        end;
      psParsingCharSet :
        begin
          {these are the VT200+ "switch charset" sequences: we ignore
           them after finding the first char in range $30..$7E}
          if ('0' <= aCh) and (aCh <= '~') then begin
            FState := psGotCommand;
            Result := pctNone;
            FCommand := eNone;
            FArgCount := 0;
          end;
        end;
      psParsingHash :
        begin
          FState := psGotCommand;
          Result := pctComplete;
          case aCh of
            '3' : begin
                    FCommand := eDECDHL;
                    FArgCount := 1;
                    FArgs^[0] := 0; {0 = top half}
                  end;
            '4' : begin
                    FCommand := eDECDHL;
                    FArgCount := 1;
                    FArgs^[0] := 1; {1 = bottom half}
                  end;
            '5' : begin
                    FCommand := eDECSWL;
                  end;
            '6' : begin
                    FCommand := eDECDWL;
                  end;
            '8' : begin
                    FCommand := eDECALN;
                  end;
          else
            {ignore the char & seq.--it's not one we know}
            FState := psGotCommand;
            Result := pctNone;
          end;{case}
        end;
      psParsingCUP52 :
        begin
          if (FArgCount = 0) then begin
            FArgs^[0] := ord(aCh) - $1F;
            inc(FArgCount);
          end
          else begin
            FState := psGotCommand;
            FCommand := eCUP;
            FArgs^[1] := ord(aCh) - $1F;
            inc(FArgCount);
            Result := pctComplete;
          end;
        end;
    else
      {invalid state?}
    end;{case}
  end;
end;
{--------}
function TIpVT100Parser.ProcessWideChar(aCh : WideChar) :TIpParserCmdType;
begin
  Result := pctNone;
end;
{--------}
function TIpVT100Parser.tpGetArgument(aInx : Integer) : Integer;
begin
  if (aInx < 0) or (aInx >= FArgCount) then
    Result := 0
  else
    Result := FArgs^[aInx];
end;
{--------}
function TIpVT100Parser.tpGetSequence : string;
begin
  if (FCommand <> eNone) then
    Result := GetStringFromSeq(FSequence)
  else
    Result := '';
end;
{--------}
function TIpVT100Parser.vtpGetArguments : Boolean;
var
  ChInx   : Integer;
  StartInx: Integer;
  Ch      : char;
  ec      : Integer;
  TempStr : string[255];
begin
  {for this parser, we assume
     1. arguments consist of numeric digits only
     2. arguments are separated by ';'
     3. the first argument can be ? (DEC VT100 special)
     4. argument parsing stops at the first character #$20 - #$2F, or
        #$40 - #$7E}

  {assume the sequence is badly formed}
  Result := False;

  {first check for the third character being ?}
  if (PSeq(FSequence)^.sText[3] = '?') then begin
    FArgCount := 1;
    FArgs^[0] := -2;
    StartInx := 4;
  end
  else
    StartInx := 3;

  {scan the rest of the characters until we reach a char in the range
   $20-$2F, or $40-$7E; look out for numeric digits and semi-colons}
  TempStr := '';
  for ChInx := StartInx to PSeq(FSequence)^.sLen do begin
    Ch := PSeq(FSequence)^.sText[ChInx];
    if ((#$20 <= Ch) and (Ch <= #$2F)) or
       ((#$40 <= Ch) and (Ch <= #$7E)) then
      Break;
    if (Ch = ';') then begin
      if (FArgCountMax = FArgCount) then
        vtpGrowArgs;
      if (TempStr = '') then begin
        FArgs^[FArgCount] := -1;
        inc(FArgCount);
      end
      else begin
        Val(TempStr, FArgs^[FArgCount], ec);
        if (ec <> 0) then
          Exit;
        inc(FArgCount);
        TempStr := '';
      end;
    end
    else if ('0' <= Ch) and (Ch <= '9') then begin
      TempStr := TempStr + Ch;
    end
    else {bad character}
      Exit;
  end;

  {convert the final argument}
  if (FArgCountMax = FArgCount) then
    vtpGrowArgs;
  if (TempStr = '') then begin
    FArgs^[FArgCount] := -1;
    inc(FArgCount);
  end
  else begin
    Val(TempStr, FArgs^[FArgCount], ec);
    if (ec <> 0) then
      Exit;
    inc(FArgCount);
  end;

  {if we got here, everything was all right}
  Result := True;
end;
{--------}
procedure TIpVT100Parser.vtpGrowArgs;
var
  NewMax   : Integer;
  NewArray : PAdIntegerArray;
begin
  {use a simple increase-by-half algorithm}
  if (FArgCountMax = 0) then
    NewMax := 16
  else
    NewMax := (FArgCountMax * 3) div 2;
  {alloc the new array, zeroed}
  NewArray := AllocMem(sizeof(Integer) * NewMax);
  {if there's any data in the old array copy it over, delete it}
  if (FArgs <> nil) then begin
    Move(FArgs^, NewArray^, sizeof(Integer) * FArgCount);
    FreeMem(FArgs, sizeof(Integer) * FArgCountMax);
  end;
  {remember the new details}
  FArgs := NewArray;
  FArgCountMax := NewMax;
end;
{--------}
function TIpVT100Parser.vtpParseANSISeq(aCh : char) : TIpParserCmdType;
begin
  {when this method is called FSequence has the full escape sequence,
   and FArgCount, FArgs, FCommand have to be set; for convenience aCh
   is the final character in FSequence--the command identifier--and
   FSequence must have at least three characters in it}

  {assume the sequence is invalid}
  Result := pctNone;

  {special case: DECSCL}
  if (GetStringFromSeq(FSequence) = DECSCLseq) then begin
    FCommand := eRIS;
    Result := pctComplete;
  end;

  {split out the arguments in the sequence, build up the FArgs array;
   note that an arg of -1 means 'default', and -2 means ? (a special
   DEConly parameter)}
  if not vtpGetArguments then
    Exit;

  {identify the command character}
  case aCh of
    '@' : begin {insert character--VT102}
            FCommand := eICH;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'A' : begin {Cursor up}
            FCommand := eCUU;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'B' : begin {Cursor down}
            FCommand := eCUD;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'C' : begin {Cursor right}
            FCommand := eCUF;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'D' : begin {cursor left}
            FCommand := eCUB;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'H' : begin {cursor position}
            FCommand := eCUP;
            {should have two parameters, both default of 1}
            if not vtpValidateArgsPrim(2, 2, 1) then Exit;
          end;
    'J' : begin {Erase in display}
            FCommand := eED;
            {should only have one parameter, default of 0}
            if not vtpValidateArgsPrim(1, 1, 0) then Exit;
          end;
    'K' : begin {Erase in line}
            FCommand := eEL;
            {should only have one parameter, default of 0}
            if not vtpValidateArgsPrim(1, 1, 0) then Exit;
          end;
    'L' : begin {Insert line--VT102}
            FCommand := eIL;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'M' : begin {Delete line--VT102}
            FCommand := eDL;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'P' : begin {delete character--VT102}
            FCommand := eDCH;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'X' : begin {erase character--VT102}
            FCommand := eECH;
            {should only have one parameter, default of 1}
            if not vtpValidateArgsPrim(1, 1, 1) then Exit;
          end;
    'c' : begin {Device attributes}
            FCommand := eDA;
            {should only have one parameter, default of 0}
            if not vtpValidateArgsPrim(1, 1, 0) then Exit;
          end;
    'f' : begin {cursor position}
            FCommand := eCUP;
            {should have two parameters, both default of 1}
            if not vtpValidateArgsPrim(2, 2, 1) then Exit;
          end;
    'g' : begin {clear horizontal tabs}
            FCommand := eTBC;
            {should only have one parameter, default of 0}
            if not vtpValidateArgsPrim(1, 1, 0) then Exit;
          end;
    'h' : begin {set mode}
            FCommand := eSM;
            {should have one parameter, or 2 if the first is ?, no
             defaults}
           end;
    'l' : begin {reset mode}
            FCommand := eRM;
            {should have one parameter, or 2 if the first is ?, no
             defaults}
            {we have to try and spot one command in particular: the
             switch to VT52 mode}
            if (FArgCount = 2) and
               (FArgs^[0] = -2) and (FArgs^[1] = 2) then
              FInVT52Mode := True; 
          end;
    'm' : begin
            FCommand := eSGR;
            {should have at least one parameter, default of 0 for all
             parameters}
            if not vtpValidateArgsPrim(1, 30000, 0) then Exit;
          end;
    'n' : begin {Device status report}
            FCommand := eDSR;
            {should only have one parameter, no default}
            if not vtpValidateArgsPrim(1, 1, -1) then Exit;
          end;
    'q' : begin {DEC PRIVATE-set/clear LEDs}
            FCommand := eDECLL;
            {should have at least one parameter, default of 0 for all
             parameters}
            if not vtpValidateArgsPrim(1, 30000, 0) then Exit;
          end;
    'r' : begin {DEC PRIVATE-set top/bottom margins}
            FCommand := eDECSTBM;
            {should have two parameters, first default of 1, second
             default unknowable by this class}
          end;
    's' : begin {save cursor pos - ANSI.SYS escape sequence}
            FCommand := eSaveCursorPos;
          end;
    'u' : begin {restore cursor pos - ANSI.SYS escape sequence}
            FCommand := eRestoreCursorPos;
          end;
    'x' : begin {DEC PRIVATE-request terminal parameters}
            FCommand := eDECREQTPARM;
            {should only have one parameter, no default}
            if not vtpValidateArgsPrim(1, 1, -1) then Exit;
          end;
    'y' : begin {DEC PRIVATE-invoke confidence test}
            FCommand := eDECTST;
            {should have two parameters, no default for first, second
             default to 0}
          end;
  else {the command letter is unknown}
    Exit;
  end;{case}

  {if we get here the sequence is valid and we've patched up the
   arguments list and count}
  Result := pctComplete;
end;
{--------}
function TIpVT100Parser.vtpProcessVT52(aCh : char) : TIpParserCmdType;
begin
  FState := psGotCommand;
  Result := pctComplete;
  case aCh of
    '<' : begin {switch to ANSI mode}
            FCommand := eSM;
            FArgCount := 2;  {pretend it's Esc[?2h}
            FArgs^[0] := -2;
            FArgs^[1] := 2;
            FInVT52Mode := False;
          end;
    '=' : begin {enter alternate keypad mode}
            FCommand := eSM;
            FArgCount := 2;
            FArgs^[0] := -2;
            FArgs^[1] := 999; {special iPRO code!}
          end;
    '>' : begin {leave alternate keypad mode}
            FCommand := eRM;
            FArgCount := 2;
            FArgs^[0] := -2;
            FArgs^[1] := 999; {special iPRO code!}
          end;
    'A' : begin {cursor up}
            FCommand := eCUU;
            FArgCount := 1;
            FArgs^[0] := 1;
          end;
    'B' : begin {cursor down}
            FCommand := eCUD;
            FArgCount := 1;
            FArgs^[0] := 1;
          end;
    'C' : begin {cursor right}
            FCommand := eCUF;
            FArgCount := 1;
            FArgs^[0] := 1;
          end;
    'D' : begin {cursor left}
            FCommand := eCUB;
            FArgCount := 1;
            FArgs^[0] := 1;
          end;
    'F' : begin {switch to graphics characters}
            FCommand := eSO;
          end;
    'G' : begin {switch to ASCII characters}
            FCommand := eSI;
          end;
    'H' : begin {move cursor home}
            FCommand := eCUP;
            FArgCount := 2;
            FArgs^[0] := 1;
            FArgs^[1] := 1;
          end;
    'I' : begin {reverse index = cursor up + scroll}
            FCommand := eRI;
          end;
    'J' : begin {erase to end of screen}
            FCommand := eED;
            FArgCount := 1;
            FArgs^[0] := 0; {ie <esc>[0J}
          end;
    'K' : begin {erase to end of line}
            FCommand := eEL;
            FArgCount := 1;
            FArgs^[0] := 0; {ie <esc>[0K}
          end;
    'Y' : begin {position cursor}
            FState := psParsingCUP52;
            FCommand := eCUP;
            Result := pctPending;
          end;
    'Z' : begin {device attributes, identify}
            FCommand := eDA;
            FArgCount := 1;
            FArgs^[0] := 52; {ie VT52 emulation}
          end;
  else
    Result := pctNone;
  end;{case}
end;
{--------}
function TIpVT100Parser.vtpValidateArgsPrim(aMinArgs : Integer;
                                            aMaxArgs : Integer;
                                            aDefault : Integer) : Boolean;
var
  i : Integer;
begin
  Result := False;
  {if we have too many arguments, something's obviously wrong}
  if (FArgCount > aMaxArgs) then
    Exit;
  {if we have too few, make the missing ones the default}
  while (FArgCount < aMinArgs) do begin
    if (FArgCountMax = FArgCount) then
      vtpGrowArgs;
    FArgs^[FArgCount] := aDefault;
    inc(FArgCount);
  end;
  {convert any -1 arguments to the default}
  for i := 0 to pred(FArgCount) do
    if (FArgs^[i] = -1) then
      FArgs^[i] := aDefault;
  {and we're done}
  Result := True;
end;

{===TIpTerminal======================================================}
constructor TIpCustomTerminal.Create(aOwner : TComponent);
begin
  { create the default emulator }
  FDefEmulator := TIpTTYEmulator.Create(nil);
  FDefEmulator.FIsDefault := True;
  FDefEmulator.Terminal := Self;
  if (FEmulator =  nil) then
    FEmulator := FDefEmulator;

  inherited Create(aOwner);

  { create our canvas }
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  { set up heartbeat timer }
  FHeartbeat := TTimer.Create(Self);
  FHeartbeat.Interval := BeatInterval;
  FHeartbeat.OnTimer := tmBeat;
  FHeartbeat.Enabled := False;

  { create a Byte queue to receive data }
  FByteQueue := TaaByteQueue.Create;

  { make sure the origin is at (1,1) }
  FOriginCol := 1;
  FOriginRow := 1;

  { set up the default values of the terminal's properties }
  Active := ipc_TermActive; { the set access method must be called }
  FBlinkTime := ipc_TermBlinkTime;
  FBorderStyle := ipc_TermBorderStyle;
  FCapture := ipc_TermCapture;
  FCaptureFile := ipc_TermCaptureFile;
  FCursorType := ipc_TermCursorType;
  FHalfDuplex := ipc_TermHalfDuplex;
  FLazyByteDelay := ipc_TermLazyByteDelay;
  FLazyTimeDelay := ipc_TermLazyTimeDelay;
  FUseLazyDisplay := ipc_TermUseLazyDisplay;
  FWantAllKeys := ipc_TermWantAllKeys;
  FFreezeScrollBack := ipc_FreezeScrollBack;                           {!!.03}

  { Initialize socket }
  FSocket := Invalid_Socket;

  Width := ipc_TermWidth;
  Height := ipc_TermHeight;
  Font.Name := ipc_TermFontName;
  Font.Size := 9;
  Font.Color := ipc_TermForeColor;
  Color := ipc_TermBackColor;

  { miscellaneous }
  with FScrollHorzInfo do
    cbSize := sizeof(FScrollHorzInfo);
  with FScrollVertInfo do
    cbSize := sizeof(FScrollVertInfo);

end;
{--------}
destructor TIpCustomTerminal.Destroy;
begin
  Emulator := nil;
  FEmulator := nil;
  FDefEmulator.Free;
  FCanvas.Free;
  FHeartbeat.Free;
  RemoveTermEmuLink(Self, False);
  TaaByteQueue(FByteQueue).Free;
  inherited Destroy;
end;
{--------}
procedure TIpCustomTerminal.CMIpSocketStatus(var Msg : TCMIpSocketStatus);
begin
  if not Assigned(SockControl) then Exit;

  case Msg.StatusEvent of
    IpssConnect :
      begin
        { Handle single-socket "attach to anything" situation }
        if (FSocket = Invalid_Socket) and (Msg.SingleSocket = 1) then
          { Go through SetSocket, so we attach to socket }
          Socket := Msg.Socket;
      end;

    IpssDisconnect :
      begin
        { Disconnect and set Socket to Invalid_Socket }
        if Msg.Socket = FSocket then
          Socket := Invalid_Socket;
      end;
  end;
end;
{--------}
procedure TIpCustomTerminal.CMIpTermForceSize(var Msg : TMessage);
begin
  RecreateWnd;
end;
{--------}
procedure TIpCustomTerminal.CMIpTermStuff(var Msg : TMessage);
var
  DataLen  : Integer;
  DataPtr  : Pointer;
  PaintRgn : HRGN;
begin
  { This message is sent by CMIpTermData, and the WriteChar/WriteString }
  { methods; note that if there are two or more CM_IPTERMSTUFF messages }
  { in the message queue, the first one to be accepted will clear the   }
  { byte queue, so the others need to take account of the fact that     }
  { there may be no data             }

  DataLen := TaaByteQueue(FByteQueue).Count;
  if (DataLen > 0) then begin
    DataPtr := TaaByteQueue(FByteQueue).Peek(DataLen);

    { if we are capturing, save the data to file }
    if (Capture = cmOn) then begin
      if (FCaptureStream <> nil) then
        FCaptureStream.Write(DataPtr^, DataLen);
    end;

    { otherwise get the emulator to process the block }
    FEmulator.ProcessBlock(DataPtr, DataLen);
    if (not UseLazyDisplay) and FEmulator.NeedsUpdate then begin
      { hide the caret }
      tmHideCaret;
      try
        PaintRgn := CreateRectRgn(0, 0,
                       ClientCols * CharWidth,
                       ClientRows * CharHeight);
        SelectClipRgn(Canvas.Handle, PaintRgn);
        DeleteObject(PaintRgn);
        FEmulator.LazyPaint;
      finally
        tmShowCaret;
      end;
    end;

    { increment the byte count for the lazy painting }
    inc(FLazyByteCount, DataLen);

    { now we've processed all the data, clear the byte queue }
    if (DataLen <> TaaByteQueue(FByteQueue).Count) then                {!!.01}
      TaaByteQueue(FByteQueue).Remove(DataLen)                         {!!.01}
    else                                                               {!!.01}
      TaaByteQueue(FByteQueue).Clear;
  end;
end;
{--------}
procedure TIpCustomTerminal.Clear;
begin
  FEmulator.teClear;
end;
{--------}
procedure TIpCustomTerminal.ClearAll;
begin
  FEmulator.teClearAll;
end;
{--------}
procedure TIpCustomTerminal.CMCtl3DChanged(var Msg : TMessage);
begin
  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;

  inherited;
end;
{--------}
procedure TIpCustomTerminal.CMFontChanged(var Msg : TMessage);
begin
  { make sure our ancestors do their stuff }
  inherited;
  { if we're loading, don't bother doing anything }
  if (csLoading in ComponentState) then
    Exit;
  { if we have no handle, there's nothing we can do }
  if not HandleAllocated then
    Exit;
  { otherwise, work out the character size }
  tmGetFontInfo;
end;
{--------}
procedure TIpCustomTerminal.CMIpTermData(var Msg : TCMIpTermData);
var
  QBuffer : Pointer;
begin
  try
    { get all the data available and buffer it elsewhere }
    QBuffer := TaaByteQueue(FByteQueue).PutPeek(Msg.BufSize);
    Move(Msg.Buffer^, QBuffer^, Msg.BufSize);
    TaaByteQueue(FByteQueue).AdvanceAfterPut(Msg.BufSize);
    { tell ourselves that we have more data }
    PostMessage(Handle, CM_IPTERMSTUFF, 0, 0);
  finally
    FreeMem(Msg.Buffer);
  end;
end;
{--------}
procedure TIpCustomTerminal.CNKeyDown(var Msg : TWMKeyDown);
var
  Shift  : TShiftState;
  EnhKey : Boolean;
  Key    : Word;
  NumLockState : TKeyboardState;
begin
  { if the left button is down (we're selecting) and the key is Escape, }
  { cancel the mode }
  if FLButtonDown and (Msg.CharCode = VK_ESCAPE) then begin
    Perform(WM_CANCELMODE, 0, 0);
    Msg.Result := 1;
    Exit;
  end;
  { if we have a selection and the key is Ctrl+C or Ctrl+Insert, copy }
  { the selection to the clipboard }
  if tmProcessClipboardCopy(Msg) then begin
    Msg.Result := 1;
    Exit;
  end;
  { process the keystroke }
  with Msg do begin
    { ignore all shift and supershift keys }
    if (CharCode = VK_SHIFT) or
       (CharCode = VK_CONTROL) or
       (CharCode = VK_MENU) then
      Exit;
    {calculate the shift state}
    Shift := KeyDataToShiftState(KeyData);
    {calculate whether this is an enhanced key}
    EnhKey := (KeyData and $1000000) <> 0;
    {get the key}
    Key := CharCode;
    {convert some keys}
    if EnhKey then begin
      {convert VK_RETURN to VK_EXECUTE for the Keypad}
      if (Key = VK_RETURN) then
        Key := VK_EXECUTE;
    end
    else {not an enhanced keyboard} begin
      {convert unNumLocked keys on Keypad to their Keypad values}
      GetKeyboardState(NumLockState);
      if ((NumLockState[VK_NUMLOCK] and $01) = 0) then
        case Key of
          VK_CLEAR  : Key := VK_NUMPAD5;
          VK_PRIOR  : Key := VK_NUMPAD9;
          VK_NEXT   : Key := VK_NUMPAD3;
          VK_END    : Key := VK_NUMPAD1;
          VK_HOME   : Key := VK_NUMPAD7;
          VK_LEFT   : Key := VK_NUMPAD4;
          VK_UP     : Key := VK_NUMPAD8;
          VK_RIGHT  : Key := VK_NUMPAD6;
          VK_DOWN   : Key := VK_NUMPAD2;
          VK_INSERT : Key := VK_NUMPAD0;
          VK_DELETE : Key := VK_DECIMAL;
        end;{case}
    end;
    { calculate whether this is an autorepeat }
    if ((KeyData and $40000000) <> 0) then
      Key := Key or $8000;
    { pass the key, etc, onto the emulator }
    FEmulator.KeyDown(Key, Shift);
    {if the emulator successfully processed the key, let the caller
     know that it's been processed}
    if (Key = 0) then
      Result := 1;
  end;
end;
{--------}
procedure TIpCustomTerminal.CNSysKeyDown(var Msg : TWMKeyDown);
begin
  CNKeyDown(Msg);
end;
{--------}
procedure TIpCustomTerminal.CopyToClipboard;
var
  i         : Integer;
  CharCount : Integer;
  StartCol  : Integer;
  ColCount  : Integer;
  CurInx    : Integer;
  RowText   : PAnsiChar;
  TextPtr   : PAnsiChar;
begin
  { calculate the amount of text in the selection; this equals the }
  { number of characters for each line, plus CR/LF per line }
  ColCount := FEmulator.Buffer.ColCount;
  CharCount := 0;
  StartCol := FLButtonRect.Left;
  for i := FLButtonRect.Top to pred(FLButtonRect.Bottom) do begin
    CharCount := CharCount + (ColCount - StartCol + 1) + 2;
    StartCol := 1;
  end;
  CharCount := CharCount + FLButtonRect.Right + 2;
  { allocate enough memory to store this set of characters, plus the }
  { terminating null }
  GetMem(TextPtr, CharCount + 1);
  { copy all the selected characters }
  CurInx := 0;
  StartCol := FLButtonRect.Left;
  for i := FLButtonRect.Top to pred(FLButtonRect.Bottom) do begin
    RowText := FEmulator.Buffer.GetLineCharPtr(i);
    Move(RowText[StartCol - 1], TextPtr[CurInx],
         ColCount - StartCol + 1);
    inc(CurInx, ColCount - StartCol + 1);
    TextPtr[CurInx] := ^M;
    TextPtr[CurInx+1] := ^J;
    inc(CurInx, 2);
    StartCol := 1;
  end;
  RowText := FEmulator.Buffer.GetLineCharPtr(FLButtonRect.Bottom);
  Move(RowText[StartCol - 1], TextPtr[CurInx],
       FLButtonRect.Right - StartCol + 1);
  inc(CurInx, FLButtonRect.Right - StartCol + 1);
  TextPtr[CurInx] := ^M;
  TextPtr[CurInx+1] := ^J;
  inc(CurInx, 2);
  TextPtr[CurInx] := #0;

  Clipboard.Open;
  Clipboard.SetTextBuf(TextPtr);
  Clipboard.Close;
end;
{--------}
procedure TIpCustomTerminal.CreateParams(var Params: TCreateParams);
const
  BorderStyles : array [TBorderStyle] of Longint =
                 (0, WS_BORDER);
begin
  inherited CreateParams(Params);

  with Params do begin
    Style := Longint(Style) or BorderStyles[FBorderStyle];
    if FUseHScrollBar then
      Style := Longint(Style) or WS_HSCROLL;
    if FUseVScrollBar or (ScrollbackRows > Rows) then
      Style := Longint(Style) or WS_VSCROLL;
  end;

  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
end;
{--------}
procedure TIpCustomTerminal.CreateWnd;

  { Search for an existing TIpSockControl }
  function FindSockControl(aForm : TWinControl) : TIpSockControl;
  var
    I : Integer;
  begin
    for i := 0 to pred(aForm.ComponentCount) do
      if aForm.Components[i] is TIpSockControl then begin
        Result := TIpSockControl(aForm.Components[i]);
        Exit;
      end;
    Result := nil;
  end;

var
  ParentForm : TWinControl;
begin
  { It is possible that the last message to be sent to the old window }
  { handle was a WM_SETFOCUS: this would have created the caret, so   }
  { free it: it's attached to a window handle that no longer exists   }
  tmFreeCaret;
  { call our ancestor to create the window }
  inherited CreateWnd;
  { Hopefully, a handle was created... }
  if HandleAllocated then begin
    { if we don't have a SockControl, go find one }
    if (SockControl = nil) then begin
      ParentForm := GetParentForm(Self);
      if (ParentForm <> nil) then
        FSockControl := FindSockControl(ParentForm);
    end;
    { check to see if we're attached to a SockControl, if so, register }
    if (SockControl <> nil) and not (csDesigning in ComponentState) then begin
      SockControl.RegisterTerminal(Handle);
      if (Socket <> Invalid_Socket) then
        tmAttachToSocket;
    end;
    { calculate the character sizes }
    tmGetFontInfo;
    { start the heartbeat }
    FHeartbeat.Enabled := True;
    if (csDesigning in ComponentState) then
      tmDrawDefaultText;
  end;
end;
{--------}
procedure TIpCustomTerminal.DestroyWnd;
begin
  if HandleAllocated then begin
    { check to see if we're attached to a socket, if so, deregister }
    if (SockControl <> nil) and not (csDesigning in ComponentState) then begin
      if (Socket <> Invalid_Socket) then
        tmDetachFromSocket;
      SockControl.DeregisterTerminal(Handle);
    end;
    {stop the heartbeat}
    FHeartbeat.Enabled := False;
    {destroy the caret}
    tmFreeCaret;
  end;
  inherited DestroyWnd;
end;
{--------}
procedure TIpCustomTerminal.HideSelection;
var
  i        : Integer;
  ColCount : Integer;
begin
  {clear the current selection}
  ColCount := FEmulator.Buffer.ColCount;
  tmMarkDeselected(FLButtonRect.Top, FLButtonRect.Left, ColCount);
  for i := succ(FLButtonRect.Top) to pred(FLButtonRect.Bottom) do
    tmMarkDeselected(i, 1, ColCount);
  tmMarkDeselected(FLButtonRect.Bottom, 1, FLButtonRect.Right);
  {initialize the selection variables}
  SetRectEmpty(FLButtonRect);
  FLButtonAnchor.X := 0;
  FLButtonAnchor.Y := 0;
  FHaveSelection := False;
end;
{--------}
procedure TIpCustomTerminal.KeyDown(var Key : Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (not WantAllKeys) and (Key <> 0) then
    FEmulator.KeyDown(Key, Shift);
end;
{--------}
procedure TIpCustomTerminal.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key <> #0) then
    FEmulator.KeyPress(Key);
end;
{--------}
procedure TIpCustomTerminal.MouseDown(Button : TMouseButton;
                                      Shift  : TShiftState;
                                      X, Y   : Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) then begin
    if not Focused and CanFocus then
      SetFocus;
    FLButtonDown := True;
    {force mouse position into bounds}
    X := BoundI(0, X, pred(ClientCols * CharWidth));
    Y := BoundI(0, Y, pred(ClientRows * CharHeight));
    {start the selection}
    tmStartSelect(X, Y);
  end;
end;
{--------}
procedure TIpCustomTerminal.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FLButtonDown then begin
    {force mouse position into bounds}
    X := BoundI(0, X, pred(ClientCols * CharWidth));
    Y := BoundI(0, Y, pred(ClientRows * CharHeight));
    {update the selection}
    tmGrowSelect(X, Y);
  end;
end;
{--------}
procedure TIpCustomTerminal.MouseUp(Button : TMouseButton;
                              Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FLButtonDown and (Button = mbLeft) then begin
    FLButtonDown := False;
  end;
end;
{--------}
procedure TIpCustomTerminal.Notification(AComponent : TComponent;
                                         Operation  : TOperation);
begin
  if (Operation = opRemove) then begin
    if (AComponent = Emulator) then
      Emulator := nil
    else if (AComponent = SockControl) then begin
      Active := False;
      SockControl := nil;
      Socket := Invalid_Socket;
    end;
  end else {Operation = opInsert} begin
    if (AComponent is TIpTerminalEmulator) then begin
      if (Emulator = nil) then
        Emulator := TIpTerminalEmulator(AComponent);
    end else if (AComponent is TIpSockControl) then begin
      if (SockControl = nil) then
        SockControl := TIpSockControl(AComponent);
    end;
  end;
  inherited Notification(AComponent, Operation);
end;
{--------}
function TermIntersectRect(var lprcDst : TRect;
                         const lprcSrc1, lprcSrc2: TRect) : Boolean;
begin
  Result := IntersectRect(lprcDst, lprcSrc1, lprcSrc2);
end;
{--------}
procedure TIpCustomTerminal.Paint;
var
  DirtyRect : TRect;
  PaintRect : TRect;
  PaintRgn  : HRGN;
begin
  {hide the caret}
  tmHideCaret;
  try
    {get the current clip rect--ie, the rect that needs painting}
    DirtyRect := Canvas.ClipRect;
    {paint any unused bits of that rect}
    if TermIntersectRect(PaintRect, DirtyRect, FUnusedRightRect) then begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(PaintRect);
    end;
    if TermIntersectRect(PaintRect, DirtyRect, FUnusedBottomRect) then begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(PaintRect);
    end;
    {find out if there is anything else to paint}
    if TermIntersectRect(PaintRect, DirtyRect, FUsedRect) then begin
      {create a clipping region and select it into the canvas, so that
       the emulator only gets to know about bits it can paint}
      with PaintRect do
        PaintRgn := CreateRectRgn(Left, Top, Right, Bottom);
      SelectClipRgn(Canvas.Handle, PaintRgn);
      DeleteObject(PaintRgn);
      {tell the emulator to paint what it has}
      FEmulator.Paint
    end;
  finally
    {show the caret again}
    tmShowCaret;
  end;
end;
{--------}
procedure TIpCustomTerminal.PaintWindow(DC : HDC);
begin
  Canvas.Handle := DC;
  try
    Paint;
  finally
    Canvas.Handle := 0;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmAttachToSocket;
begin
  { Assumptions: not designtime; SockControl <> nil; Active = True; }
  if TIpSockControlFriend(SockControl).scAttachTerminal(Socket, Handle) then begin

    { Set MasterTerminal if not currently set }
    if (SockControl.MasterTerminal[Socket] = nil) then
      SockControl.MasterTerminal[Socket] := Self;

  end else begin

    { Socket apparently doesn't exist }
    Socket := Invalid_Socket;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmBeat(Sender: TObject);
var
  PaintRgn : HRGN;
  MousePt  : TPoint;
  Msg      : TWMScroll;
begin
  {if the left mouse button is down we may need to scroll to help the
   user select text}
  if FLButtonDown then begin
    GetCursorPos(MousePt);
    MousePt := ScreenToClient(MousePt);
    if FUseHScrollBar then
      if (MousePt.X < 0) then begin
        Msg.ScrollCode := SB_LINELEFT;
        WMHScroll(Msg);
      end
      else if (MousePt.X >= ClientWidth) then begin
        Msg.ScrollCode := SB_LINERIGHT;
        WMHScroll(Msg);
      end;
    if (MousePt.Y < 0) then begin
      Msg.ScrollCode := SB_LINEUP;
      WMVScroll(Msg);
    end
    else if (MousePt.Y >= ClientHeight) then begin
      Msg.ScrollCode := SB_LINEDOWN;
      WMVScroll(Msg);
    end;
    MouseMove([ssLeft], MousePt.X, MousePt.Y);
  end;
  {only blink if the blink time is not zero}
  if (BlinkTime > 0) then begin
    inc(FBlinkTimeCount, BeatInterval);
    if (FBlinkTimeCount > BlinkTime) then begin
      FBlinkTextVisible := not FBlinkTextVisible;
      if FEmulator.HasBlinkingText then begin
        tmHideCaret;
        try
          PaintRgn := CreateRectRgn(0, 0,
                         ClientCols * CharWidth,
                         ClientRows * CharHeight);
          SelectClipRgn(Canvas.Handle, PaintRgn);
          DeleteObject(PaintRgn);
          FEmulator.BlinkPaint(FBlinkTextVisible);
        finally
          tmShowCaret;
        end;
      end;
      FBlinkTimeCount := 0;
    end;
  end;

  {only use the lazy display option if requested to}
  if UseLazyDisplay then begin
    {check for a lazy display time or byte count}
    inc(FLazyTimeCount, BeatInterval);
    if FEmulator.NeedsUpdate and
       ((FLazyTimeCount > FLazyTimeDelay) or
        (FLazyByteCount > FLazyByteDelay)) then begin
      tmHideCaret;
      try
        PaintRgn := CreateRectRgn(0, 0,
                       ClientCols * CharWidth,
                       ClientRows * CharHeight);
        SelectClipRgn(Canvas.Handle, PaintRgn);
        DeleteObject(PaintRgn);
        FEmulator.LazyPaint;
      finally
        tmShowCaret;
      end;
      FLazyTimeCount := 0;
      FLazyByteCount := 0;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmCalcExtent;
var
  WorkColCount : Integer;
  WorkRowCount : Integer;
begin
  { ASSUMPTION: there is a handle allocated, but an emulator may not be }
  {             attached. Since there is a handle, we've already        }
  {             calculated the character width and height.              }

  { first calculate the max number of columns and rows in the client area }
  if (FEmulator = nil) then begin
    FClientCols := 0;
    FClientRows := 0;
    Exit;
  end;

  WorkColCount := Columns;
  WorkRowCount := Rows;

  FClientCols := (ClientWidth + pred(FCharWidth)) div FCharWidth;
  FClientCols := IpMinInt(FClientCols, WorkColCount - ClientOriginCol + 1);
  FClientFullCols := ClientWidth div FCharWidth;
  FClientFullCols := IpMinInt(FClientFullCols, WorkColCount - ClientOriginCol + 1);

  FClientRows := (ClientHeight + pred(FCharHeight)) div FCharHeight;
  FClientRows := IpMinInt(FClientRows, WorkRowCount - ClientOriginRow + 1);
  FClientFullRows := ClientHeight div FCharHeight;
  FClientFullRows := IpMinInt(FClientFullRows, WorkRowCount - ClientOriginRow + 1);

  {if there is a handle for the terminal, we have to worry about
   whether the scrollbars are visible or not}
  if HandleAllocated then begin
    {if the width is too small, force a horizontal scroll bar; if too
     large, force the horizontal scroll bar off}
    if (FClientFullCols < WorkColCount) then begin
      if not FUseHScrollBar then begin
        FUseHScrollBar := True;
        PostMessage(Handle, CM_IPTERMFORCESIZE, 0, 0);
        Exit;
      end
    end
    else begin
      if FUseHScrollBar then begin
        FUseHScrollBar := False;
        PostMessage(Handle, CM_IPTERMFORCESIZE, 0, 0);
        Exit;
      end
    end;
    { if the height is too small, force a vertical scroll bar; if too }
    { large, force the vertical scroll bar off }
    if (FClientFullRows < WorkRowCount) then begin
      if not FUseVScrollBar then begin
        FUseVScrollBar := True;
        {note: if we have scrollback rows we already have a vertical
               scrollbar}
        if (ScrollbackRows <= Rows) then begin
          PostMessage(Handle, CM_IPTERMFORCESIZE, 0, 0);
          Exit;
        end
      end
    end
    else begin
      if FUseVScrollBar then begin
        FUseVScrollBar := False;
        PostMessage(Handle, CM_IPTERMFORCESIZE, 0, 0);
        Exit;
      end
    end;
  end;

  { now calculate the used and unused area rects }
  FUsedRect := Rect(0, 0, ClientWidth, ClientHeight);
  FUnusedRightRect := FUsedRect;
  FUnusedBottomRect := FUsedRect;

  FUsedRect.Right := IpMinInt(ClientCols * CharWidth, ClientWidth);
  FUsedRect.Bottom := IpMinInt(ClientRows * CharHeight, ClientHeight);

  FUnusedRightRect.Left := FUsedRect.Right;
  FUnusedRightRect.Bottom := FUsedRect.Bottom;
  FUnusedBottomRect.Top := FUsedRect.Bottom;

  {if we are using a horizontal scroll bar, set the range}
  if FUseHScrollBar then begin
    tmInitHScrollBar;
  end;

  {if we are using a vertical scroll bar, set the range}
  if FUseVScrollBar or (ScrollbackRows > Rows) then begin
    tmInitVScrollBar;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmDetachFromSocket;
begin
  if Assigned(SockControl) then begin
    if IsMasterTerminal then
      SockControl.MasterTerminal[Socket] := nil;

    TIpSockControlFriend(SockControl).scDetachTerminal(Socket, Handle);
  end;
end;
{--------}
procedure TIpCustomTerminal.tmDrawDefaultText;
const
  FormatMost = 'Design mode; line %.2d'^M^J;
  FormatLast = 'Design mode; line %.2d'^M;
  VT100Most = #27'[0;%dmDesign mode; line %.2d'^M^J;
  VT100Last = #27'[0;%dmDesign mode; line %.2d'^M;
var
  i  : Integer;
  S  : string;
  VTColor : Integer;
begin
  {ASSUMPTION: Handle is allocated}

  if (FByteQueue = nil) or
     (FEmulator.Buffer = nil) then
    Exit;

  for i := 1 to ScrollbackRows do
    WriteString(^M^J);

  if (FEmulator is TIpVT100Emulator) then begin
    VTColor := 31;
    for i := 1 to pred(Rows) do begin
      S := Format(VT100Most, [VTColor, i]);
      WriteString(S);
      inc(VTColor);
      if (VTColor = 38) then
        VTColor := 31;
    end;
    S := Format(VT100Last, [VTColor, Rows]);
    WriteString(S);
  end
  else begin
    for i := 1 to pred(Rows) do begin
      S := Format(FormatMost, [i]);
      WriteString(S);
    end;
    S := Format(FormatLast, [Rows]);
    WriteString(S);
  end;
  Invalidate;
end;
{--------}
procedure TIpCustomTerminal.tmFreeCaret;
begin
  if FCreatedCaret then begin
    DestroyCaret;
    FCreatedCaret := False;
  end;
end;
{--------}
function TIpCustomTerminal.tmGetAttributes(aRow, aCol : Integer) : TIpTerminalCharAttrs;
var
  AttrArray : PByteArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    AttrArray := FEmulator.Buffer.GetLineAttrPtr(aRow);
    Result := TIpTerminalCharAttrs(AttrArray^[pred(aCol)]);
  end
  else
    Result := [];
end;
{--------}
function TIpCustomTerminal.tmGetBackColor(aRow, aCol : Integer) : TColor;
var
  ColorArray : PIptLongArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    ColorArray := FEmulator.Buffer.GetLineBackColorPtr(aRow);
    Result := TColor(ColorArray^[pred(aCol)]);
  end
  else
    Result := ipc_TermBackColor;
end;
{--------}
function TIpCustomTerminal.tmGetCharSet(aRow, aCol : Integer) : Byte;
var
  CharSetArray : PByteArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    CharSetArray := FEmulator.Buffer.GetLineCharSetPtr(aRow);
    Result := CharSetArray^[pred(aCol)];
  end
  else
    Result := 0;
end;
{--------}
function TIpCustomTerminal.tmGetColumns : Integer;
begin
  if (FEmulator.Buffer <> nil) then
    Result := FEmulator.Buffer.ColCount
  else
    Result := 0;
end;
{--------}
function TIpCustomTerminal.tmGetEmulator : TIpTerminalEmulator;
begin
  if (FEmulator = FDefEmulator) then
    Result := nil
  else
    Result := FEmulator;
end;
{--------}
procedure TIpCustomTerminal.tmGetFontInfo;
var
  DC              : HDC;
  i               : Integer;
  SavedFontHandle : THandle;
  Metrics         : TTextMetric;
  SizeChanged     : Boolean;
  NewValue        : Integer;
  TempFont        : TFont;
  FontList        : TStringList;
  {------}
  procedure UpdateCharSizes;
  begin
    with Metrics do begin
      {calculate the height}
      NewValue := tmHeight + tmExternalLeading;
      if (NewValue > FCharHeight) then begin
        SizeChanged := True;
        FCharHeight := NewValue;
      end;
      {if it's a fixed pitch font, use the average char width}
      if ((tmPitchAndFamily and TMPF_FIXED_PITCH) <> 0) then
        NewValue := tmAveCharWidth
      {otherwise use the average char width, weighted towards the max
       char width}
      else
        NewValue := tmAveCharWidth +
                    (tmMaxCharWidth - tmAveCharWidth) div 2;
      if (NewValue > FCharWidth) then begin
        SizeChanged := True;
        FCharWidth := NewValue;
      end;
    end;
  end;
  {------}
begin
  if (FEmulator.CharSetMapping <> nil) then begin
    FontList := TStringList.Create;
    try
      FEmulator.CharSetMapping.GetFontNames(FontList);
    except
      FontList.Free;
      raise;
    end;
  end
  else
    FontList := nil;

  {get a DC, in order that we can get the font metrics for the current
   font; release the DC afterwards}
  DC := GetDC(0);
  SavedFontHandle := SelectObject(DC, Font.Handle);
  TempFont := nil;
  FCharWidth := 0;
  FCharHeight := 0;
  try
    {obtain the character cell height and width from the metrics}
    SizeChanged := False;
    if (FontList = nil) then begin
      GetTextMetrics(DC, Metrics);
      UpdateCharSizes;
    end
    else begin
      TempFont := TFont.Create;
      TempFont.Assign(Font);
      {$IFDEF Version3}
      TempFont.CharSet := DEFAULT_CHARSET;
      {$ENDIF}
      for i := 0 to pred(FontList.Count) do begin
        if (FontList[i] = DefaultFontName) then
          TempFont.Name := Font.Name
        else
          TempFont.Name := FontList[i];
        SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        UpdateCharSizes;
      end;
    end;
    {if either the width or height changed, invalidate the display}
    if HandleAllocated then begin
      if SizeChanged then begin
        tmCalcExtent;
        Invalidate;
      end;
    end;
  finally
    SelectObject(DC, SavedFontHandle);
    ReleaseDC(0, DC);
    FontList.Free;
    TempFont.Free;
  end;
end;
{--------}
function TIpCustomTerminal.tmGetForeColor(aRow, aCol : Integer) : TColor;
var
  ColorArray : PIptLongArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    ColorArray := FEmulator.Buffer.GetLineForeColorPtr(aRow);
    Result := TColor(ColorArray^[pred(aCol)]);
  end
  else
    Result := clWhite;
end;
{--------}
function TIpCustomTerminal.tmGetIsMasterTerminal : Boolean;
begin
  if Assigned(SockControl) and (Socket <> Invalid_Socket) then
    Result := (SockControl.MasterTerminal[Socket] = Self)
  else
    Result := False;
end;
{--------}
function TIpCustomTerminal.tmGetLine(aRow : Integer) : string;
var
  CharArray : PAnsiChar;
begin
  if (FEmulator.Buffer <> nil) then begin
    CharArray := FEmulator.Buffer.GetLineCharPtr(aRow);
    SetLength(Result, FEmulator.Buffer.ColCount);
    Move(CharArray^, Result[1], FEmulator.Buffer.ColCount);
  end
  else
    Result := '';
end;
{--------}
function TIpCustomTerminal.tmGetRows : Integer;
begin
  if (FEmulator.Buffer <> nil) then
    Result := FEmulator.Buffer.RowCount
  else
    Result := 0;
end;
{--------}
function TIpCustomTerminal.tmGetScrollbackRows : Integer;
begin
  if (FEmulator.Buffer <> nil) then
    Result := FEmulator.Buffer.SVRowCount
  else
    Result := 0;
end;
{--------}
function TIpCustomTerminal.tmGetTelnetTermSize : string;
begin
  Result := Result + AnsiChar(Hi(Columns));
  Result := Result + AnsiChar(Lo(Columns));
  Result := Result + AnsiChar(Hi(Rows));
  Result := Result + AnsiChar(Lo(Rows));
end;
{--------}
procedure TIpCustomTerminal.tmGrowSelect(X, Y : Integer);
var
  NewRow   : Integer;
  NewCol   : Integer;
  StartCol : Integer;
  ColCount : Integer;
  NewRgn   : TRect;
  i        : Integer;
begin
  {if this gets called, we have a selection}
  FHaveSelection := True;

  {get the column count as a local variable: we'll be using it a lot}
  ColCount := FEmulator.Buffer.ColCount;

  {X and Y are mouse coordinates on the terminal display; convert to a
   row/col position}
  NewCol := (X div CharWidth) + ClientOriginCol;
  NewCol := IpMinInt(IpMaxInt(NewCol, 1), ColCount);
  NewRow := (Y div CharHeight) + ClientOriginRow;
  with FEmulator.Buffer do
    NewRow := IpMinInt(IpMaxInt(NewRow, RowCount-SVRowCount), RowCount);

  {generate the new selected region}
  if (NewRow < FLButtonAnchor.Y) then begin
    NewRgn.Top := NewRow;
    NewRgn.Left := NewCol;
    NewRgn.Bottom := FLButtonAnchor.Y;
    NewRgn.Right := FLButtonAnchor.X;
  end
  else if (NewRow > FLButtonAnchor.Y) then begin
    NewRgn.Top := FLButtonAnchor.Y;
    NewRgn.Left := FLButtonAnchor.X;
    NewRgn.Bottom := NewRow;
    NewRgn.Right := NewCol;
  end
  else {NewRow and the anchor row are the same} begin
    NewRgn.Top := NewRow;
    NewRgn.Bottom := NewRow;
    if (NewCol <= FLButtonAnchor.X) then begin
      NewRgn.Left := NewCol;
      NewRgn.Right := FLButtonAnchor.X;
    end
    else begin
      NewRgn.Left := FLButtonAnchor.X;
      NewRgn.Right := NewCol;
    end;
  end;

  {check to see how the current selection grew (we need to mark the
   new bits as selected) or shrank (the new bits need to be
   deselected)}
  if (NewRgn.Top = NewRgn.Bottom) then begin
    StartCol := FLButtonRect.Left;
    for i := FLButtonRect.Top to pred(FLButtonRect.Bottom) do begin
      tmMarkDeselected(i, StartCol, ColCount);
      StartCol := 1;
    end;
    tmMarkDeselected(FLButtonRect.Bottom, 1, FLButtonRect.Right);
    tmMarkSelected(NewRgn.Top, NewRgn.Left, NewRgn.Right);
  end
  else begin
    if (NewRgn.Top < FLButtonRect.Top) then begin
      {the selection grew upwards}
      StartCol := NewRgn.Left;
      for i := NewRgn.Top to pred(FLButtonRect.Top) do begin
        tmMarkSelected(i, StartCol, ColCount);
        StartCol := 1;
      end;
      tmMarkSelected(FLButtonRect.Top, 1, FLButtonRect.Left);
    end
    else if (NewRgn.Top = FLButtonRect.Top) then begin
      {the selection might have changed on the top row}
      if (NewRgn.Left < FLButtonRect.Left) then
        tmMarkSelected(NewRgn.Top, NewRgn.Left, FLButtonRect.Left)
      else if (NewRgn.Left > FLButtonRect.Left) then
        tmMarkDeselected(NewRgn.Top, FLButtonRect.Left, NewRgn.Left);
    end
    else {new top > old top} begin
      StartCol := FLButtonRect.Left;
      for i := FLButtonRect.Top to pred(NewRgn.Top) do begin
        tmMarkDeselected(i, StartCol, ColCount);
        StartCol := 1;
      end;
      tmMarkDeselected(NewRgn.Top, 1, NewRgn.Left);
    end;
    if (NewRgn.Bottom > FLButtonRect.Bottom) then begin
      {the selection grew downwards}
      StartCol := FLButtonRect.Right;
      tmMarkSelected(FLButtonRect.Bottom, FLButtonRect.Right, ColCount);
      for i := FLButtonRect.Bottom to pred(NewRgn.Bottom) do begin
        tmMarkSelected(i, StartCol, ColCount);
        StartCol := 1;
      end;
      tmMarkSelected(NewRgn.Bottom, 1, NewRgn.Right);
    end
    else if (NewRgn.Bottom = FLButtonRect.Bottom) then begin
      {the selection might have changed on the bottom row}
      if (NewRgn.Right > FLButtonRect.Right) then
        tmMarkSelected(NewRgn.Bottom, FLButtonRect.Right, NewRgn.Right)
      else if (NewRgn.Right < FLButtonRect.Right) then
        tmMarkDeselected(NewRgn.Bottom, NewRgn.Right, FLButtonRect.Right);
    end
    else {new bottom < old bottom} begin
      StartCol := NewRgn.Right;
      for i := NewRgn.Bottom to pred(FLButtonRect.Bottom) do begin
        tmMarkDeselected(i, StartCol, ColCount);
        StartCol := 1;
      end;
      tmMarkDeselected(FLButtonRect.Bottom, 1, FLButtonRect.Right);
    end;
  end;
  FLButtonRect := NewRgn;
end;
{--------}
procedure TIpCustomTerminal.tmHideCaret;
begin
  if FShowingCaret then begin
    HideCaret(Handle);
    FShowingCaret := False;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmInitHScrollBar;
begin
  {Assumptions: FUseHScrollBar = True
                FClientFullCols < Columns}
  with FScrollHorzInfo do begin
    fMask := SIF_ALL;
    nMin := 0;
    nMax := Columns - 1;
    nPage := FClientFullCols;
    nPos := ClientOriginCol-1;
  end;
  SetScrollInfo(Handle, SB_HORZ, FScrollHorzInfo, True);
end;
{--------}
procedure TIpCustomTerminal.tmInitVScrollBar;
begin
  {Assumptions: FUseVScrollBar = True AND FClientFullRows < Rows
                OR ScrollbackRows > Rows}
  if FUseVScrollBar and (not Scrollback) then begin
    with FScrollVertInfo do begin
      fMask := SIF_ALL;
      nMin := 0;
      nMax := Rows - 1;
      nPage := FClientFullRows;
      nPos := ClientOriginRow-1;
    end;
  end
  else begin
    with FScrollVertInfo do begin
      fMask := SIF_ALL;
      nMin := Rows - ScrollbackRows;
      nMax := Rows - 1;
      nPage := FClientFullRows;
      nPos := ClientOriginRow-1;
    end;
  end;
  SetScrollInfo(Handle, SB_VERT, FScrollVertInfo, True);
end;
{--------}
procedure TIpCustomTerminal.tmMakeCaret;
begin
  FCreatedCaret := True;
  if (CursorType = ctUnderline) then
    CreateCaret(Handle, 0, CharWidth, 2)
  else if (CursorType = ctBlock) then
    CreateCaret(Handle, 0, CharWidth, CharHeight)
  else
    FCreatedCaret := False;
end;
{--------}
procedure TIpCustomTerminal.tmInvalidateRow(aRow : Integer);
var
  InvRect : TRect;
begin
  if HandleAllocated and (ClientOriginRow <= aRow) and
     (aRow < ClientOriginRow + ClientRows) then begin
    InvRect.Left := 0;
    InvRect.Top := (aRow - ClientOriginRow) * CharHeight;
    InvRect.Right := ClientWidth;
    InvRect.Bottom := InvRect.Top + CharHeight;
    InvalidateRect(Handle, @InvRect, False);
  end;
end;
{--------}
procedure TIpCustomTerminal.tmMarkDeselected(aRow : Integer;
                                             aFromCol, aToCol : Integer);
var
  AttrArray : PByteArray;
  i         : Integer;
  CharAttrs : TIpTerminalCharAttrs;
begin
  {get the attributes for this line}
  AttrArray := FEmulator.Buffer.GetLineAttrPtr(aRow);
  for i := pred(aFromCol) to pred(aToCol) do begin
    CharAttrs := TIpTerminalCharAttrs(AttrArray^[i]);
    Exclude(CharAttrs, tcaSelected);
    AttrArray^[i] := Byte(CharAttrs);
  end;
  tmInvalidateRow(aRow);
end;
{--------}
procedure TIpCustomTerminal.tmMarkSelected(aRow : Integer;
                                           aFromCol, aToCol : Integer);
var
  AttrArray : PByteArray;
  i         : Integer;
  CharAttrs : TIpTerminalCharAttrs;
begin
  {get the attributes for this line}
  AttrArray := FEmulator.Buffer.GetLineAttrPtr(aRow);
  for i := pred(aFromCol) to pred(aToCol) do begin
    CharAttrs := TIpTerminalCharAttrs(AttrArray^[i]);
    Include(CharAttrs, tcaSelected);
    AttrArray^[i] := Byte(CharAttrs);
  end;
  tmInvalidateRow(aRow);
end;
{--------}
procedure TIpCustomTerminal.tmPositionCaret;
var
  X, Y : Integer;
begin
  with FEmulator.Buffer do begin
    if UseAbsAddress then begin
      X := (Col - FOriginCol) * CharWidth;
      Y := (Row - FOriginRow) * CharHeight;
    end
    else begin
      X := (Col + OriginCol - 1 - FOriginCol) * CharWidth;
      Y := (Row + OriginRow - 1 - FOriginRow) * CharHeight;
    end;
  end;
  if (CursorType = ctUnderline) then
    inc(Y, CharHeight - 2);
  if (0 <= X) and (X < ClientWidth) and
     (0 <= Y) and (Y < ClientHeight) then begin
    ShowCaret(Handle);
    SetCaretPos(X, Y);
    FShowingCaret := True;
  end;
end;
{--------}
function TIpCustomTerminal.tmProcessClipboardCopy(
                                      var Msg : TWMKeyDown) : Boolean;
const
  VK_C = $43;
var
  Shift  : TShiftState;
begin
  {this method is called from the main keydown methods to check for
   Ctrl+C or Ctrl+Insert, so that the selected text can get copied to
   the clipboard}
  Result := False;
  {check whether we have a selection}
  if not FHaveSelection then
    Exit;
  {check whether the key is a C or an Insert}
  if (Msg.CharCode <> VK_C) and (Msg.CharCode <> VK_INSERT) then
    Exit;
  {see if the Ctrl key is down and the shift and Alt keys are not}
  Shift := KeyDataToShiftState(Msg.KeyData);
  if (not (ssCtrl in Shift)) or
     (ssShift in Shift) or (ssAlt in Shift) then
    Exit;
  {we have the required keystroke and a selection: copy to clipboard}
  Result := True;
  CopyToClipboard;
end;
{--------}
procedure TIpCustomTerminal.tmScrollHorz(aDist : Integer);
begin
  {aDist is +ve if we are scrolling to the right (ie the current
   window contents are moved to a position aDist pixels to the right),
   and is -ve for a leftward scroll}
  ScrollWindow(Handle, aDist, 0, nil, nil);
end;
{--------}
procedure TIpCustomTerminal.tmScrollVert(aDist : Integer);
begin
  { aDist is +ve if we are scrolling downwards (ie the current window   }
  { contents are moved to a position aDist pixels down), and is -ve for }
  { a upward scroll }
  ScrollWindow(Handle, 0, aDist, nil, nil);
end;
{--------}
procedure TIpCustomTerminal.tmSetActive(aValue : Boolean);
begin
  if (aValue <> Active) then begin
    FActive := aValue;
    { if we have a socket then either attach to or detach from it }
    if (SockControl <> nil) and (Socket <> Invalid_Socket) then begin
      if not (csDesigning in ComponentState) then
        if Active then
          tmAttachToSocket
        else
          tmDetachFromSocket;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetAttributes(aRow, aCol : Integer;
                               const aAttr      : TIpTerminalCharAttrs);
var
  AttrArray : PByteArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    AttrArray := FEmulator.Buffer.GetLineAttrPtr(aRow);
    TIpTerminalCharAttrs(AttrArray^[pred(aCol)]) := aAttr;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetBackColor(aRow, aCol : Integer;
                                    aValue     : TColor);
var
  ColorArray : PIptLongArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    ColorArray := FEmulator.Buffer.GetLineBackColorPtr(aRow);
    TColor(ColorArray^[pred(aCol)]) := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetBlinkTime(aValue : Integer);
begin
  if (aValue <> BlinkTime) then begin
    if (aValue <= 0) then
      aValue := 0
    else if (aValue <= 250) then
      aValue := 250;
    FBlinkTime := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetBorderStyle(aBS : TBorderStyle);
begin
  if (aBS <> BorderStyle) then begin
    FBorderStyle := aBS;
    RecreateWnd;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetCapture(aValue : TIpCaptureMode);
begin
  if (aValue <> Capture) then begin
    if (aValue = cmAppend) and (Capture = cmOn) then
      Exit;
    {if we're capturing now, close the stream}
    if (Capture = cmOn) then begin
      FCaptureStream.Free;
    end;
    {save the new value}
    FCapture := aValue;
    {if we should now be capturing, open the stream}
    if (Capture <> cmOff) then begin
      if (CaptureFile = '') then
        FCaptureFile := ipc_TermCaptureFile;
      if not (csDesigning in ComponentState) then begin
        if (Capture = cmOn) then
          FCaptureStream := TFileStream.Create(CaptureFile, fmCreate)
        else if (Capture = cmAppend) then begin
          FCaptureStream := TFileStream.Create(CaptureFile, fmOpenReadWrite);
          FCaptureStream.Seek(0, soFromEnd);
          FCapture := cmOn;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetCaptureFile(const aValue : string);
var
  WasCapturing : Boolean;
begin
  if (aValue <> CaptureFile) then begin
    {first turn off capturing if needed}
    if (Capture = cmOff) then
      WasCapturing := False
    else begin
      WasCapturing := True;
      Capture := cmOff;
    end;
    {save the new filename}
    FCaptureFile := aValue;
    {now turn capturing back on}
    if WasCapturing then
      Capture := cmOn;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetCharSet(aRow, aCol : Integer; aValue : Byte);
var
  CharSetArray : PByteArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    CharSetArray := FEmulator.Buffer.GetLineCharSetPtr(aRow);
    CharSetArray^[pred(aCol)] := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetColumns(aValue : Integer);
var
  OldValue : Integer;
begin
  if (FEmulator.Buffer <> nil) and
     (aValue > 0) then begin
    OldValue := FEmulator.Buffer.ColCount;
    if (OldValue <> aValue) then begin
      FEmulator.Buffer.ColCount := aValue;
      FOriginCol := 1;
      FOriginRow := 1;
      tmCalcExtent;
      tmTermResize;
      Invalidate;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetCursorType(aValue : TIpCursorType);
begin
  if (aValue <> CursorType) then begin
    tmHideCaret;
    tmFreeCaret;
    FCursorType := aValue;
    tmMakeCaret;
    tmShowCaret;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetEmulator(aValue : TIpTerminalEmulator);
var
  OldRowCount : Integer;
  OldColCount : Integer;
  OldSVRowCount : Integer;
begin
  if (aValue <> Emulator) then begin
    if HandleAllocated then
      FHeartbeat.Enabled := False;
    {if we were attached to an emulator, remove its link}
    OldSVRowCount := FEmulator.Buffer.SVRowCount;
    OldRowCount := FEmulator.Buffer.RowCount;
    OldColCount := FEmulator.Buffer.ColCount;
    if (Emulator <> nil) then begin
      FEmulator := nil; {to stop recursion with the next call}
      RemoveTermEmuLink(Self, True);
    end;
    {attach ourselves to the new emulator}
    if (aValue = nil) then
      FEmulator := FDefEmulator
    else begin
      FEmulator := aValue;
      AddTermEmuLink(Self, Emulator);
    end;
    {set the new emulator up}
    if (OldSVRowCount > 0) then begin
      FEmulator.Buffer.SVRowCount := OldSVRowCount;
      FEmulator.Buffer.RowCount := OldRowCount;
      FEmulator.Buffer.ColCount := OldColCount;
    end;
    if HandleAllocated then begin
      FHeartbeat.Enabled := True;
      if (csDesigning in ComponentState) then
        tmDrawDefaultText;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetForeColor(aRow, aCol : Integer;
                                     aValue     : TColor);
var
  ColorArray : PIptLongArray;
begin
  if (FEmulator.Buffer <> nil) then begin
    ColorArray := FEmulator.Buffer.GetLineForeColorPtr(aRow);
    TColor(ColorArray^[pred(aCol)]) := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetLazyByteDelay(aValue : Integer);
begin
  if (0 < aValue) and (aValue <= 1024) and
     (aValue <> LazyByteDelay) then begin
    FLazyByteDelay := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetLazyTimeDelay(aValue : Integer);
begin
  if (0 < aValue) and (aValue <= 1000) and
     (aValue <> LazyTimeDelay) then begin
    FLazyTimeDelay := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetLine(aRow : Integer; const aValue : string);
var
  CharArray : PAnsiChar;
  StLen     : Integer;
  i         : Integer;
begin
  if (FEmulator.Buffer <> nil) then begin
    CharArray := FEmulator.Buffer.GetLineCharPtr(aRow);
    StLen := length(aValue);
    if (StLen > FEmulator.Buffer.ColCount) then
      Move(aValue[1], CharArray[0], FEmulator.Buffer.ColCount)
    else begin
      Move(aValue[1], CharArray[0], StLen);
      for i := StLen to pred(FEmulator.Buffer.ColCount) do
        CharArray[i] := ' ';
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetOriginCol(aValue : Integer);
var
  MaxOriginCol : Integer;
  OldOrigin    : Integer;
begin
  if (aValue <> ClientOriginCol) then begin
    {work out the maximum value}
    MaxOriginCol := Columns - FClientFullCols + 1;
    if (MaxOriginCol < 1) then
      MaxOriginCol := 1;
    {save the old value}
    OldOrigin := FOriginCol;
    {set the new value}
    if (aValue < 1) then
      FOriginCol := 1
    else if (aValue > MaxOriginCol) then
      FOriginCol := MaxOriginCol
    else
      FOriginCol := aValue;
    {scroll the window, set the scrollbar position}
    if (OldOrigin <> ClientOriginCol) then begin
      tmScrollHorz((OldOrigin - ClientOriginCol) * CharWidth);
      tmCalcExtent;

      with FScrollHorzInfo do begin
        fMask := SIF_POS;
        nPos := ClientOriginCol-1;
      end;
      SetScrollInfo(Handle, SB_HORZ, FScrollHorzInfo, True);
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetOriginRow(aValue : Integer);
var
  MinOriginRow : Integer;
  MaxOriginRow : Integer;
  OldOrigin    : Integer;
begin
  if (aValue <> ClientOriginRow) then begin
    {work out the minimum value}
    if FUseVScrollBar and (not Scrollback) then
      MinOriginRow := 1
    else
      MinOriginRow := Rows - ScrollbackRows + 1;
    {work out the maximum value}
    MaxOriginRow := Rows - FClientFullRows + 1;
    if (MaxOriginRow < 1) then
      MaxOriginRow := 1;
    {save the old value}
    OldOrigin := FOriginRow;
    {set the new value}
    if (aValue < MinOriginRow) then
      FOriginRow := MinOriginRow
    else if (aValue > MaxOriginRow) then
      FOriginRow := MaxOriginRow
    else
      FOriginRow := aValue;
    {scroll the window, set the scrollbar position}
    if (OldOrigin <> ClientOriginRow) then begin
      tmScrollVert((OldOrigin - ClientOriginRow) * CharHeight);
      tmCalcExtent;

      with FScrollVertInfo do begin
        fMask := SIF_POS;
        nPos := ClientOriginRow-1;
      end;
      SetScrollInfo(Handle, SB_VERT, FScrollVertInfo, True);
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetRows(aValue : Integer);
var
  OldValue : Integer;
begin
  if (FEmulator.Buffer <> nil) and
     (aValue > 0) then begin
    OldValue := FEmulator.Buffer.RowCount;
    if (OldValue <> aValue) then begin
      FEmulator.Buffer.RowCount := aValue;
      tmCalcExtent;
      tmTermResize;
      Invalidate;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetScrollback(aValue : Boolean);
begin
  if (aValue <> Scrollback) then begin
    FScrollback := aValue;
    tmInitVScrollBar;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetScrollbackRows(aValue : Integer);
begin
  if (FEmulator.Buffer <> nil) and (aValue > 0) then
    FEmulator.Buffer.SVRowCount := aValue;
end;
{--------}
procedure TIpCustomTerminal.tmSetSockControl(aValue : TIpSockControl);
begin
  if (aValue <> SockControl) then begin
    { if we don't yet have a handle, just save the new value }
    if not HandleAllocated then begin
      { Make sure Socket is reset }
      tmDetachFromSocket;
      { Store new value }
      FSockControl := aValue;
    end else begin
    { otherwise we need to do some registration }
      { detach/deregister from the old socket & sockcontrol }
      if (SockControl <> nil) and not (csDesigning in ComponentState) then begin
        tmDetachFromSocket;
        SockControl.DeregisterTerminal(Handle);
      end;
      { save the new value }
      FSockControl := aValue;
      { register with the new SockControl }
      if (SockControl <> nil) and not (csDesigning in ComponentState) then
        SockControl.RegisterTerminal(Handle);
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetSocket(aValue : TSocket);
begin
  if (aValue <> Socket) then begin
    { Detach from old socket }
    tmDetachFromSocket;
    { Store value }
    FSocket := aValue;
    { Attach to new socket if necessary }
    if Active and not (csDesigning in ComponentState) then
      tmAttachToSocket;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetUseLazyDisplay(aValue : Boolean);
begin
  FUseLazyDisplay := aValue;
end;
{--------}
procedure TIpCustomTerminal.tmSetWantAllKeys(aValue : Boolean);
begin
  if (aValue <> WantAllKeys) then begin
    FWantAllKeys := aValue;
  end;
end;
{--------}
procedure TIpCustomTerminal.tmSetFreezeScrollback (v : boolean);       {!!.03}
begin                                                                  {!!.03}
  if (v <> FFreezeScrollback) then                                     {!!.03}
    FFreezeScrollback := v;                                            {!!.03}
end;                                                                   {!!.03}
{--------}
procedure TIpCustomTerminal.tmShowCaret;
begin
  if (not FShowingCaret) and FCreatedCaret and
     (not (csDesigning in ComponentState)) and
     Focused then
    tmPositionCaret;
end;
{--------}
procedure TIpCustomTerminal.tmStartSelect(X, Y : Integer);
begin
  { clear the current selection }
  if (FLButtonRect.Left > 0) and (FLButtonRect.Top > 0) and            {!!.01}
     (FLButtonRect.Bottom > 0) and (FLButtonRect.Right > 0) then       {!!.01}
    HideSelection;

  { X and Y are mouse coordinates on the terminal display; convert to a }
  { row/col position }
  FLButtonAnchor.X := (X div CharWidth) + ClientOriginCol;
  FLButtonAnchor.Y := (Y div CharHeight) + ClientOriginRow;

  { reset the current selection }
  FLButtonRect.Left := FLButtonAnchor.X;
  FLButtonRect.Top := FLButtonAnchor.Y;
  FLButtonRect.Right := FLButtonAnchor.X;
  FLButtonRect.Bottom := FLButtonAnchor.Y;
end;                                                      
{--------}
procedure TIpCustomTerminal.tmTermResize;
begin
  if Assigned(SockControl) and (Socket <> Invalid_Socket) then
    PostMessage(SockControl.Handle, CM_IPTERMRESIZE, Socket, 0);
end;
{--------}
procedure TIpCustomTerminal.WMCancelMode(var Msg : TMessage);
begin
  HideSelection;
  FLButtonDown := False;
end;
{--------}
procedure TIpCustomTerminal.WMCopy(var Msg : TMessage);
begin
  CopyToClipboard;
  Msg.Result := 1;
end;
{--------}
procedure TIpCustomTerminal.WMEraseBkgnd(var Msg : TMessage);
begin
  Msg.Result := 1
end;
{--------}
procedure TIpCustomTerminal.WMGetDlgCode(var Msg : TMessage);
begin
  {we want as many keys as we can get for the emulatation}
  if WantAllKeys then
    Msg.Result := DLGC_WANTALLKEYS +
                  DLGC_WANTARROWS +
                  DLGC_WANTCHARS +
                  DLGC_WANTMESSAGE +
                  DLGC_WANTTAB
  else
    inherited;
end;
{--------}
procedure TIpCustomTerminal.WMHScroll(var Msg : TWMScroll);
var
  PageSize : Integer;
  NewPos   : Integer;
  MaxOriginCol : Integer;
begin
  PageSize := FClientFullCols;
  MaxOriginCol := Columns - PageSize + 1;
  case Msg.ScrollCode of
    SB_LINELEFT      : NewPos := ClientOriginCol - 1;
    SB_LINERIGHT     : NewPos := ClientOriginCol + 1;
    SB_PAGELEFT      : NewPos := ClientOriginCol - PageSize;
    SB_PAGERIGHT     : NewPos := ClientOriginCol + PageSize;
    SB_THUMBPOSITION : NewPos := Msg.Pos + 1;
    SB_THUMBTRACK    : NewPos := Msg.Pos + 1;
  else
    Exit; {ignore it}
  end;
  if (NewPos < 1) then
    NewPos := 1
  else if (NewPos > MaxOriginCol) then
    NewPos := MaxOriginCol;
  if (NewPos <> ClientOriginCol) then
    ClientOriginCol := NewPos;
end;
{--------}
procedure TIpCustomTerminal.WMKillFocus(var Msg: TWMSetFocus);
begin
  tmHideCaret;
  tmFreeCaret;
  inherited;
end;
{--------}
procedure TIpCustomTerminal.WMPaint(var Msg : TWMPaint);
begin
  PaintHandler(Msg);
end;
{--------}
procedure TIpCustomTerminal.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if not FCreatedCaret then
    tmHideCaret;
    tmMakeCaret;
  if not FShowingCaret then
    tmShowCaret;
end;
{--------}
procedure TIpCustomTerminal.WMSize(var Msg : TWMSize);
begin
  if (CharWidth > 0) then begin
    { calculate the new values for the extent of the window }
    tmCalcExtent;
    { if the current origin column is not 1 and we can show more of the }
    { terminal if it were less, then do so }
    if (ClientOriginCol > 1) then begin
      if ((ClientCols * CharWidth) < ClientWidth) then
        ClientOriginCol := Columns - (ClientWidth div CharWidth) + 1;
    end;
    { if the current origin row is not 1 and we can show more of the }
    { terminal if it were less, then do so }
    if (ClientOriginRow > 1) then begin
      if ((ClientRows * CharHeight) < ClientHeight) then
        ClientOriginRow := Rows - (ClientHeight div CharHeight) + 1;
    end;
  end;
end;
{--------}
procedure TIpCustomTerminal.WMVScroll(var Msg : TWMScroll);
var
  PageSize : Integer;
  NewPos   : Integer;
  MinOriginRow : Integer;
  MaxOriginRow : Integer;
begin
  {if we've a vertical scrollbar because the window is smaller than
   the terminal screen...}
  if FUseVScrollBar then begin
    {if we're not currently going through the scrollback buffer and
     the current origin row is 1 and the user clicks the up arrow on
     the scrollbar, auto switch into scrollback mode}
    if (not Scrollback) then begin
      if (Msg.ScrollCode = SB_LINEUP) and (ClientOriginRow = 1) then
        Scrollback := True;
    end
    {if we're currently going through the scrollback buffer and the
     current origin row is greater than 1, auto switch into
     non-scrollback mode}
    else {Scrollback is True} begin
      if (ClientOriginRow > 1) then
        Scrollback := False;
    end;
  end;

  PageSize := FClientFullRows;
  if FUseVScrollBar and (not Scrollback) then
    MinOriginRow := 1
  else
    MinOriginRow := Rows - ScrollbackRows - 1;
  MaxOriginRow := Rows - PageSize + 1;
  if (MaxOriginRow < 1) then
    MaxOriginRow := 1;
  case Msg.ScrollCode of
    SB_LINEUP        : NewPos := ClientOriginRow - 1;
    SB_LINEDOWN      : NewPos := ClientOriginRow + 1;
    SB_PAGEUP        : NewPos := ClientOriginRow - PageSize;
    SB_PAGEDOWN      : NewPos := ClientOriginRow + PageSize;
    SB_THUMBPOSITION : NewPos := Msg.Pos + 1;
    SB_THUMBTRACK    : NewPos := Msg.Pos + 1;
  else
    Exit; {if anything else, ignore it}
  end;
  if (NewPos < MinOriginRow) then
    NewPos := MinOriginRow
  else if (NewPos > MaxOriginRow) then
    NewPos := MaxOriginRow;
  if (NewPos <> ClientOriginRow) then
    ClientOriginRow := NewPos;
end;
{--------}
procedure TIpCustomTerminal.WriteChar(aCh : AnsiChar);
begin
  {stuff the data into the byte queue}
  TaaByteQueue(FByteQueue).Put(aCh, sizeof(AnsiChar));
  {tell ourselves that we have more data}
  PostMessage(Handle, CM_IPTERMSTUFF, 0, 0);
end;
{--------}
procedure TIpCustomTerminal.WriteString(const aSt : string);
begin
  {stuff the data into the byte queue}
  TaaByteQueue(FByteQueue).Put(aSt[1], length(aSt));
  {tell ourselves that we have more data}
  PostMessage(Handle, CM_IPTERMSTUFF, 0, 0);
end;
{====================================================================}


{===TIpTerminalEmulator==============================================}
constructor TIpTerminalEmulator.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
end;
{--------}
destructor TIpTerminalEmulator.Destroy;
begin
  if not FIsDefault then
    Terminal := nil;
  inherited Destroy;
end;
{--------}
procedure TIpTerminalEmulator.BlinkPaint(aVisible : Boolean);
begin
  {do nothing}
end;
{--------}
function TIpTerminalEmulator.HasBlinkingText : Boolean;
begin
  Result := False;
end;
{--------}
procedure TIpTerminalEmulator.KeyDown(var Key : Word; Shift: TShiftState);
begin
  {do nothing}
end;
{--------}
procedure TIpTerminalEmulator.KeyPress(var Key : AnsiChar);
begin
  {do nothing}
end;
{--------}
procedure TIpTerminalEmulator.LazyPaint;
begin
  {do nothing}
end;
{--------}
procedure TIpTerminalEmulator.Notification(AComponent : TComponent;
                                           Operation  : TOperation);
begin
  if (Operation = opRemove) and (AComponent = Terminal) then begin
    FTerminal := nil;
  end;
  if (Operation = opInsert) and
     (AComponent is TIpTerminal) and
     (Terminal = nil) then begin
    Terminal := TIpTerminal(AComponent);      
  end;
  inherited Notification(AComponent, Operation);
end;
{--------}
procedure TIpTerminalEmulator.Paint;
begin
  {do nothing}
end;
{--------}
procedure TIpTerminalEmulator.ProcessBlock(aData : Pointer; aDataLen : Longint);
begin
  {do nothing}
end;
{--------}
procedure TIpTerminalEmulator.teClear;
begin
  {do nothing};
end;
{--------}
procedure TIpTerminalEmulator.teClearAll;
begin
  {do nothing};
end;
{--------}
function TIpTerminalEmulator.teGetNeedsUpdate : Boolean;
begin
  Result := Buffer.HasDisplayChanged or Buffer.HasCursorMoved;         {!!.01}
end;
{--------}                                                             {!!.01}
procedure TIpTerminalEmulator.teHandleCursorMovement (Sender : TObject;  {!!.01}
                                                      Row    : Integer;  {!!.01}
                                                      Col    : Integer); {!!.01}
{ HandleCursorMovement

  Method is called by the Terminal Buffer's OnCursorMoved event}
begin                                                                  {!!.01}
  if assigned (FTerminal) then begin                                   {!!.01}
    if assigned (FTerminal.OnCursorMoved) then                         {!!.01}
      FTerminal.OnCursorMoved (Sender, Row, Col);                      {!!.01}
  end;                                                                 {!!.01}
end;                                                                   {!!.01}                                                                            
{--------}
procedure TIpTerminalEmulator.teSendChar(aCh : char; aCanEcho : Boolean);
begin
  { Assumptions: aCh is a printable character or is one of the standard }
  { non-printable characters }
  if (Terminal <> nil) then begin
    if aCanEcho and Terminal.HalfDuplex then
      ProcessBlock(@aCh, 1);
      if (Terminal.SockControl <> nil) and (Terminal.Socket <> Invalid_Socket) then
        TIpSockControlFriend(Terminal.SockControl).scPutBlock(Terminal.Socket, aCh, SizeOf(aCh), False);
  end;
end;
{--------}
procedure TIpTerminalEmulator.teSetTerminal(aValue : TIpCustomTerminal);
var
  OldTerminal : TIpCustomTerminal;
begin
  if (aValue <> Terminal) then begin
    {if we were attached to a terminal, remove its link}
    if (Terminal <> nil) and (not FIsDefault) then begin
      OldTerminal := Terminal;
      FTerminal := nil; {to stop recursion in the following call}
      RemoveTermEmuLink(OldTerminal, True);
    end;
    FTerminal := aValue;
    {attach ourselves to the new terminal}
    if (Terminal <> nil) and (not FIsDefault) then begin
      if (Terminal.Emulator <> nil) and (Terminal.Emulator <> Self) then
        RemoveTermEmuLink(Terminal, True);
      AddTermEmuLink(Terminal, Self);
    end;
  end;
end;
{====================================================================}


{====================================================================}
constructor TIpTTYEmulator.Create(aOwner : TComponent);
begin
  {Note: the buffer *must* be created before the ancestor can perform
         initialization. The reason is that at design time dropping an
         emulator on the form will cause a series of Notification
         calls to take place. This in turn could cause a terminal's
         tmSetEmulator method to be called, which would then set up
         some default text in the emulator's buffer.}

  {create the buffer}
  FTerminalBuffer := TIpTerminalBuffer.Create(False);
  FTelnetTermType := 'dumb';

  {now let the ancestor do his stuff}
  inherited Create(aOwner);

  {set up the terminal buffer and ourselves}
  FDisplayStrSize := 256; {enough for standard terminals}
  GetMem(FDisplayStr, FDisplayStrSize);
  GetMem(FCellWidths, 255 * sizeof(Integer));
  FillChar(FCellWidths^, 255 * sizeof(Integer), 0);
  FTerminalBuffer.OnCursorMoved := teHandleCursorMovement;             {!!.01}
end;
{--------}
destructor TIpTTYEmulator.Destroy;
begin
  {free the paint node free list}
  ttyFreeAllPaintNodes;
  {free the cell widths array}
  if (FCellWidths <> nil) then
    FreeMem(FCellWidths, 255 * SizeOf(Integer));
  {free the display string}
  if (FDisplayStr <> nil) then
    FreeMem(FDisplayStr, FDisplayStrSize);
  {free the internal objects}
  FTerminalBuffer.Free;
  inherited Destroy;
end;
{--------}
procedure TIpTTYEmulator.KeyPress(var Key : AnsiChar);
begin
  {send it to the host}
  if (Key = ^H) then
    Key := #127;
  teSendChar(Key, True);
end;
{--------}
procedure TIpTTYEmulator.LazyPaint;
var
  DirtyRect : TRect;
  Row       : Integer;
begin
  { the LazyPaint method is called by the terminal, if and only if      }
  { either of the lazy timers expired; in other words, either a certain }
  { amount of data has been received by the terminal, or a certain time }
  { has elapsed since the last update                                   }

  if (Terminal.Color <> Buffer.BackColor) or
     (Terminal.Font.Color <> Buffer.ForeColor) then begin
    Buffer.DefBackColor := Terminal.Color;
    Buffer.DefForeColor := Terminal.Font.Color;
    Buffer.BackColor := Terminal.Color;
    Buffer.ForeColor := Terminal.Font.Color;
    FRefresh := True;
  end;

  { if we have to refresh the display, do it, and throw away all the }
  { 'dirty' data }
  if FRefresh then begin
    FRefresh := False;
    for Row := 1 to Buffer.RowCount do
      ttyDrawChars(Row, 1, Buffer.ColCount, True);
    while Buffer.GetInvalidRect(DirtyRect) do {nothing};
  end
  else begin
    { using the 'dirty' data in the buffer, draw the required characters }
    while Buffer.GetInvalidRect(DirtyRect) do begin
      if not Terminal.FreezeScrollBack then begin                      {!!.03}
        Inc (DirtyRect.Top, Terminal.FScrollVertInfo.nPos);            {!!.03}
        Inc (DirtyRect.Bottom, Terminal.FScrollVertInfo.nPos + 1);     {!!.03}
      end;                                                             {!!.03}
      for Row := DirtyRect.Top to DirtyRect.Bottom do
        ttyDrawChars(Row, DirtyRect.Left, DirtyRect.Right, True);
    end;
  end;
end;
{--------}
procedure TIpTTYEmulator.Paint;
var
  DirtyRect : TRect;
  Row       : Integer;
begin
  { the Paint method is called by the terminal, if and only if the }
  { terminal component received a WM_PAINT message }

  if (Terminal.Color <> Buffer.BackColor) or
     (Terminal.Font.Color <> Buffer.ForeColor) then begin
    Buffer.DefBackColor := Terminal.Color;
    Buffer.DefForeColor := Terminal.Font.Color;
    Buffer.BackColor := Terminal.Color;
    Buffer.ForeColor := Terminal.Font.Color;
    FRefresh := true;
  end;

  {repaint the clip region}
  DirtyRect := Terminal.Canvas.ClipRect;
  with Terminal do begin
    DirtyRect.Left :=
       (DirtyRect.Left div CharWidth) + ClientOriginCol;
    DirtyRect.Right :=
       (pred(DirtyRect.Right) div CharWidth) + ClientOriginCol;
    DirtyRect.Top :=
       (DirtyRect.Top div CharHeight) + ClientOriginRow;
    DirtyRect.Bottom :=
       (pred(DirtyRect.Bottom) div CharHeight) + ClientOriginRow;
  end;
  if not Terminal.FreezeScrollBack then begin                          {!!.03}
    Inc (DirtyRect.Top, Terminal.FScrollVertInfo.nPos);                {!!.03}
    Inc (DirtyRect.Bottom, Terminal.FScrollVertInfo.nPos + 1);         {!!.03}
  end;                                                                 {!!.03}
  for Row := DirtyRect.Top to DirtyRect.Bottom do
    ttyDrawChars(Row, DirtyRect.Left, DirtyRect.Right, True);
end;
{--------}
procedure TIpTTYEmulator.ProcessBlock(aData : Pointer; aDataLen : Longint);
var
  DataAsChar : PAnsiChar absolute aData;
  i          : Integer;
  Ch         : AnsiChar;
begin
  for i := 0 to pred(aDataLen) do begin
    Ch := DataAsChar[i];
    if (Ch < ' ') then
      ttyProcessCommand(Ch)
    else
      Buffer.WriteChar(DataAsChar[i]);
  end;
end;
{--------}
procedure TIpTTYEmulator.teClear;
begin
  Buffer.EraseScreen;
end;
{--------}
procedure TIpTTYEmulator.teClearAll;
begin
  Buffer.EraseAll;
end;
{--------}
function TIpTTYEmulator.teGetNeedsUpdate : Boolean;
begin
  Result := Buffer.HasDisplayChanged or FRefresh or                    {!!.01}
            Buffer.HasCursorMoved;                                     {!!.01}
end;
{--------}
procedure TIpTTYEmulator.teSetTerminal(aValue : TIpCustomTerminal);
begin
  inherited teSetTerminal(aValue);
end;
{--------}
procedure TIpTTYEmulator.ttyDrawChars(aRow, aStartCol, aEndCol : Integer;
                                      aVisible : Boolean);
var
  ColNum     : Integer;
  StartColNum: Integer;
  BackColor  : TColor;
  ForeColor  : TColor;
  Attr       : TIpTerminalCharAttrs;
  ForeColors : PIptLongArray;
  BackColors : PIptLongArray;
  Attrs      : PByteArray;
  Script     : PPaintNode;
  PaintNode  : PPaintNode;
begin
  {ASSUMPTION: aStartCol <= aEndCol}

  {avoid any drawing if the row simply is not visible}
  if (aRow < Terminal.ClientOriginRow) or
     (aRow >= Terminal.ClientOriginRow + Terminal.ClientRows) then
    Exit;
  {same if the range of columns is not visible}
  if (aEndCol < Terminal.ClientOriginCol) or
     (aStartCol >= Terminal.ClientOriginCol + Terminal.ClientCols) then
    Exit;

  {if this point is reached, we have to paint *something*}

  {force the parameter values in range}
  if (aStartCol < Terminal.ClientOriginCol) then
    aStartCol := Terminal.ClientOriginCol;
  if (aEndCol > Terminal.ClientOriginCol + Terminal.ClientCols) then
    aEndCol := Terminal.ClientOriginCol + Terminal.ClientCols;

  {this is the main processing: in general we'll be displaying text in
   this section; what will happen here, is that we first generate a
   script of drawing commands (which background color, which text
   color, which attributes, which text), and then we execute the
   script}

  {get the pointers to the colors, the attributes}
  BackColors := Buffer.GetLineBackColorPtr(aRow);
  ForeColors := Buffer.GetLineForeColorPtr(aRow);
  Attrs := Buffer.GetLineAttrPtr(aRow);

  {get the initial values for the display variables}
  BackColor := BackColors^[aStartCol-1];
  ForeColor := ForeColors^[aStartCol-1];
  Attr := TIpTerminalCharAttrs(Attrs^[aStartCol-1]);

  {make a note of the start column}
  StartColNum := aStartCol;
  ColNum := aStartCol;

  {build the script as a stack of paint commands}
  Script := nil;
  while (ColNum < aEndCol) do begin
    {look at the next column}
    inc(ColNum);
    {if any info has changed...}
    if (ForeColor <> ForeColors^[ColNum-1]) or
       (BackColor <> BackColors^[ColNum-1]) or
       (Attr <> TIpTerminalCharAttrs(Attrs^[ColNum-1])) then begin
      {get a new node, initialize it}
      PaintNode := ttyNewPaintNode;
      PaintNode^.pnStart := StartColNum;
      PaintNode^.pnEnd := pred(ColNum);
      PaintNode^.pnFore := ForeColor;
      PaintNode^.pnBack := BackColor;
      PaintNode^.pnAttr := Attr;
      PaintNode^.pnCSet := 0;
      {add it to the script}
      PaintNode^.pnNext := Script;
      Script := PaintNode;
      {save the new values of the variables}
      ForeColor := ForeColors^[ColNum-1];
      BackColor := BackColors^[ColNum-1];
      Attr := TIpTerminalCharAttrs(Attrs^[ColNum-1]);
      StartColNum := ColNum;
    end;
  end;
  {create the final paint command}
  PaintNode := ttyNewPaintNode;
  PaintNode^.pnStart := StartColNum;
  PaintNode^.pnEnd := aEndCol;
  PaintNode^.pnFore := ForeColor;
  PaintNode^.pnBack := BackColor;
  PaintNode^.pnAttr := Attr;
  PaintNode^.pnCSet := 0;
  {add it to the script}
  PaintNode^.pnNext := Script;
  Script := PaintNode;

  {now execute the paint script}
  ttyExecutePaintScript(aRow, Script);
end;
{--------}
procedure TIpTTYEmulator.ttyExecutePaintScript(aRow    : Integer;
                                               aScript : Pointer);
var
  Canvas       : TCanvas;
  Font         : TFont;
  Walker, Temp : PPaintNode;
  ForeColor    : TColor;
  BackColor    : TColor;
  CharWidth    : Integer;
  CharHeight   : Integer;
  OriginCol    : Integer;
  TextStrLen   : Integer;
  TextChars    : PAnsiChar;
  WorkRect     : TRect;
  Y            : Integer;
  R, G, B      : Integer;
  Reversed     : Boolean;
begin
  {get some values as local variables}
  Canvas := Terminal.Canvas;
  Font := Terminal.Font;
  TextChars := Buffer.GetLineCharPtr(aRow);
  CharHeight := Terminal.CharHeight;
  WorkRect.Top := (aRow - Terminal.ClientOriginRow) * CharHeight;
  WorkRect.Bottom := WorkRect.Top + CharHeight;
  CharWidth := Terminal.CharWidth;
  OriginCol := Terminal.ClientOriginCol;
  {set the cell widths}
  for Y := 0 to pred(Buffer.ColCount) do
    FCellWidths^[Y] := CharWidth;
  {process the script}
  Walker := PPaintNode(aScript);
  while (Walker <> nil) do begin
    {check for reverse}
    Reversed := (tcaReverse in Walker^.pnAttr) xor
                (tcaSelected in Walker^.pnAttr);
    if Reversed then begin
      ForeColor := Walker^.pnBack;
      BackColor := Walker^.pnFore;
    end
    else begin
      ForeColor := Walker^.pnFore;
      BackColor := Walker^.pnBack;
    end;
    {check for invisible}
    if (tcaInvisible in Walker^.pnAttr) then
      ForeColor := BackColor
    {check for bold}
    else if (tcaBold in Walker^.pnAttr) then begin
      if Reversed then begin
        R := IpMinInt(Integer(GetRValue(BackColor)) + $80, $FF);
        G := IpMinInt(Integer(GetGValue(BackColor)) + $80, $FF);
        B := IpMinInt(Integer(GetBValue(BackColor)) + $80, $FF);
        BackColor := RGB(R, G, B);
      end
      else begin
        R := IpMinInt(Integer(GetRValue(ForeColor)) + $80, $FF);
        G := IpMinInt(Integer(GetGValue(ForeColor)) + $80, $FF);
        B := IpMinInt(Integer(GetBValue(ForeColor)) + $80, $FF);
        ForeColor := RGB(R, G, B);
      end;
    end;

    {get the length of the text to display}
    TextStrLen := succ(Walker^.pnEnd - Walker^.pnStart);

    {move the required text to our display string}
    Move(TextChars[Walker^.pnStart - 1], FDisplayStr^, TextStrLen);
    FDisplayStr[TextStrLen] := #0;

    {set the correct background}
    Canvas.Brush.Color := BackColor;

    {calculate the left and right values for the rect}
    WorkRect.Left := (Walker^.pnStart - OriginCol) * CharWidth;
    WorkRect.Right := WorkRect.Left + (TextStrLen * CharWidth);

    {display the bit o'text}
    Canvas.Font := Font;
    Canvas.Font.Color := ForeColor;

    ExtTextOut(Canvas.Handle,
               WorkRect.Left,
               WorkRect.Top,
               ETO_OPAQUE,
               @WorkRect,
               FDisplayStr,
               TextStrLen,
               @FCellWidths^);

    {finally, draw the underline and/or strike through}
    Canvas.Pen.Color := ForeColor;
    if (tcaUnderline in Walker^.pnAttr) then begin
      Y := WorkRect.Bottom - 2;
      Canvas.MoveTo(WorkRect.Left, Y);
      Canvas.LineTo(WorkRect.Right, Y);
    end;
    if (tcaStrikeThrough in Walker^.pnAttr) then begin
      Y := WorkRect.Top + (WorkRect.Bottom - WorkRect.Top) div 2;
      Canvas.MoveTo(WorkRect.Left, Y);
      Canvas.LineTo(WorkRect.Right, Y);
    end;

    {walk to the next paint node, free this one}
    Temp := Walker;
    Walker := Walker^.pnNext;
    ttyFreePaintNode(Temp);
  end;
end;
{--------}
procedure TIpTTYEmulator.ttyFreeAllPaintNodes;
var
  Walker, Temp : PPaintNode;
begin
  Walker := FPaintFreeList;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.pnNext;
    Dispose(Temp);
  end;
  FPaintFreeList := nil;
end;
{--------}
procedure TIpTTYEmulator.ttyFreePaintNode(aNode : Pointer);
begin
  PPaintNode(aNode)^.pnNext := FPaintFreeList;
  FPaintFreeList := aNode;
end;
{--------}
function TIpTTYEmulator.ttyNewPaintNode : Pointer;
begin
  if (FPaintFreeList = nil) then
    New(PPaintNode(Result))
  else begin
    Result := FPaintFreeList;
    FPaintFreeList := PPaintNode(Result)^.pnNext;
  end;
end;
{--------}
procedure TIpTTYEmulator.ttyProcessCommand(aCh : AnsiChar);
begin
  {Assumption: aCh is less than space, i.e., is one of the unprintable
               characters}
  case aCh of
    ^G : {bell}
      begin
        MessageBeep(MB_ICONASTERISK);
      end;
    ^H : {backspace}
      begin
        Buffer.DoBackspace;
      end;
    ^I : {tab}
      begin
        Buffer.DoHorzTab;
      end;
    ^J : {linefeed}
      begin
        Buffer.DoLineFeed;
      end;
    ^M : {carriage return}
      begin
        Buffer.DoCarriageReturn;
      end;
  end;{case}
end;
{====================================================================}


{====================================================================}
constructor TIpVT100Emulator.Create(aOwner : TComponent);
begin
  {Note: the buffer *must* be created before the ancestor can perform
         initialization. The reason is that at design time dropping an
         emulator on the form will cause a series of Notification
         calls to take place. This in turn could cause a terminal's
         tmSetEmulator method to be called, which would then set up
         some default text in the emulator's buffer.}

  {create the buffer and parser and keyboard mapper}
  FTerminalBuffer := TIpTerminalBuffer.Create(False);
  FParser := TIpVT100Parser.Create(False);
  FKeyboardMapping := TIpKeyboardMapping.Create;
  FCharSetMapping := TIpCharSetMapping.Create;

  {now let the ancestor do his stuff}
  inherited Create(aOwner);

  {set up our default values}
  ANSIMode := ipc_VT100ANSIMode;
  AppKeyMode := ipc_VT100AppKeyMode;
  AppKeypadMode := ipc_VT100AppKeypadMode;
  AutoRepeat := ipc_VT100AutoRepeat;
  FCol132Mode := ipc_VT100Col132Mode;
  GPOMode := ipc_VT100GPOMode;
  InsertMode := ipc_VT100InsertMode;
  Interlace := ipc_VT100Interlace;
  NewLineMode := ipc_VT100NewLineMode;
  FRelOriginMode := ipc_VT100RelOriginMode;
  FRevScreenMode := ipc_VT100RevScreenMode;
  SmoothScrollMode := ipc_VT100SmoothScrollMode;
  WrapAround := ipc_VT100WrapAround;

  FAnswerback := ipc_VT100Answerback;
  FG0CharSet := ipc_VT100G0CharSet;
  FG1CharSet := ipc_VT100G1CharSet;
  FTelnetTermType := 'vt100';

  FDisplayStrSize := 133; {enough for both 80- and 132-column mode}
  GetMem(FDisplayStr, FDisplayStrSize);

  {make sure we're told about scrolling rows so we can track double
   height and double width lines}
  FLineAttrArray := TIpLineAttrArray.Create(Self);
  FTerminalBuffer.OnScrollRows := vttScrollRowsHandler;

  {make sure the mappers are initialized}
  FKeyboardMapping.LoadFromRes(hInstance, 'IPVT100KeyMap');            {!!.10}
  FCharSetMapping.LoadFromRes(hInstance, 'IPVT100CharSet');            {!!.10}

  {initialize the secondary font--a cache to help charset switches}
  FSecondaryFont := TFont.Create;
  if (Terminal <> nil) then
    FSecondaryFont.Assign(Terminal.Font);
  GetMem(FCellWidths, 132 * sizeof(Integer));
  FillChar(FCellWidths^, 132 * sizeof(Integer), 0);
  FTerminalBuffer.OnCursorMoved := teHandleCursorMovement;             {!!.01}
end;
{--------}
destructor TIpVT100Emulator.Destroy;
begin
  {clear the current blink script, free the blink node free list}
  vttClearBlinkScript;
  vttFreeAllBlinkNodes;
  {free the paint node free list}
  vttFreeAllPaintNodes;
  FreeMem(FCellWidths, 132 * sizeof(Integer));
  {free the display string}
  if (FDisplayStr <> nil) then
    FreeMem(FDisplayStr, FDisplayStrSize);
  {free the internal objects}
  FSecondaryFont.Free;
  FLineAttrArray.Free;
  FKeyboardMapping.Free;
  FCharSetMapping.Free;
  FParser.Free;
  FTerminalBuffer.Free;
  inherited Destroy;
end;
{--------}
procedure TIpVT100Emulator.BlinkPaint(aVisible : Boolean);
var
  Walker : PBlinkNode;
begin
  {the BlinkPaint method is called by the terminal, if and only if
   the blink timer expired; in other words, the blinking text must be
   displayed in the opposite sense, on or off}

  {read all the nodes in the blink linked list, redraw them}
  Walker := PBlinkNode(FBlinkers);
  while (Walker <> nil) do
    with Walker^ do begin
      vttDrawChars(bnRow, bnStartCh, bnEndCh, aVisible, True);
      Walker := bnNext;
    end;
end;
{--------}
function TIpVT100Emulator.HasBlinkingText : Boolean;
begin
  Result := FBlinkers <> nil;
end;
{--------}
procedure TIpVT100Emulator.KeyDown(var Key : Word; Shift: TShiftState);
  {------}
  function CvtHexChar(const S : string; aInx : Integer) : AnsiChar;
  var
    Hex : string[3];
    Value : Integer;
    ec    : Integer;
  begin
    Hex := '$  ';
    Hex[2] := S[aInx];
    Hex[3] := S[aInx+1];
    Val(Hex, Value, ec);
    if (ec = 0) then
      Result := chr(Value)
    else
      Result := '?';
  end;
  {------}
var
  VKKey     : string;
  VTKey     : string;
  VTKeyString : string;
  i         : Integer;
  EscChar   : Boolean;
  HexChar1  : Boolean;
  HexChar2  : Boolean;
  IsEscSeq  : Boolean;
  IsRepeat  : Boolean;
  IsNumLock : Boolean;
  Ch        : AnsiChar;
begin
  {check for a repeated key and AutoRepeat is off}
  IsRepeat := (Key and $8000) <> 0;
  if IsRepeat then begin
    if not AutoRepeat then begin
      Key := 0;
      Exit;
    end;
    Key := Key and $7FFF;
  end;
  {make a note in case we hit the numlock key}
  IsNumLock := Key = VK_NUMLOCK;
  {we need to convert this keystroke into a VT100 string to pass back
   to the server; this is a three step process...}
  {first, convert the keystroke to its name}
  VKKey := Format('\x%.2x', [Key]);
  VKKey := KeyboardMapping.Get(VKKey);
  {if we can continue, add the shift state in the order shift, ctrl,
   then alt}
  if (VKKey = '') then
    Exit;
  if ssAlt in Shift then
    VKKey := 'alt+' + VKKey;
  if ssCtrl in Shift then
    VKKey := 'ctrl+' + VKKey;
  if ssShift in Shift then
    VKKey := 'shift+' + VKKey;
  {now convert this into a DEC VT100 keyname}
  VTKey := KeyboardMapping.Get(VKKey);
  {if we managed to convert it to a VT100 key, modify the name to
   include the application/cursor key mode}
  if (VTKey = '') then
    Exit;
  {if this is a cursor key the name ends in 'c', replace it with 0, 1,
   or 2, depending on whether we're in VT52 mode, ANSI mode with
   cursor key mode reset, or ANSI mode with cursor key mode set}
  if (VTKey[length(VTKey)] = 'c') then begin
    if not ANSIMode then
      VTKey[length(VTKey)] := '0'
    else if not AppKeyMode then
      VTKey[length(VTKey)] := '1'
    else
      VTKey[length(VTKey)] := '2';
  end
  {if this is a Keypad key the name ends in 'k', replace it with 3, 4,
   5, or 6, depending on whether we're in VT52 numeric mode, VT52
   application mode, ANSI numeric mode, or ANSI application mode}
  else if (VTKey[length(VTKey)] = 'k') then begin
    if not ANSIMode then
      if not AppKeypadMode then
        VTKey[length(VTKey)] := '3'
      else
        VTKey[length(VTKey)] := '4'
    else
      if not AppKeypadMode then
        VTKey[length(VTKey)] := '5'
      else
        VTKey[length(VTKey)] := '6';
  end;
  {now get the string we need to send to the host for this key}
  VTKeyString := KeyboardMapping.Get(VTKey);
  if (VTKeyString <> '') then begin
    IsEscSeq := False;
    EscChar := False;
    HexChar1 := False;
    HexChar2 := False;
    {interpret the key string}
    for i := 1 to length(VTKeyString) do begin
      {get the next character}
      Ch := VTKeyString[i];
      {if the previous character was a '\' then we're reading an
       escaped character, either of the form \e or \xhh}
      if EscChar then begin
        if (Ch = 'e') then begin
          teSendChar(#27, False);
          IsEscSeq := True;
        end
        else if (Ch = 'x') then begin
          HexChar1 := True;
          Ch := CvtHexChar(VTKeyString, i+1);
          if (Ch = #13) then begin
            teSendChar(Ch, True);
            if NewLineMode then
              teSendChar(#10, True);
          end
          else if (Ch = #27) then begin
            teSendChar(#27, False);
            IsEscSeq := True;
          end
          else
            teSendChar(Ch, not IsEscSeq);
        end
        else {it's not \e or \xhh} begin
          teSendChar('\', True);
          teSendChar(Ch, True);
        end;
        EscChar := False;
      end
      {if the previous character was the x in \xhh, ignore this one:
       we've already interpreted it with the \x}
      else if HexChar1 then begin
        HexChar1 := False;
        HexChar2 := True;
      end
      {if the previous character was the first h in \xhh, ignore this
       one: we've already interpreted it with the \x}
      else if HexChar2 then begin
        HexChar2 := False;
      end
      {if this character is a '\' we're starting an escaped character}
      else if (Ch = '\') then
        EscChar := True
      {otherwise there's nothing special about this character}
      else
        teSendChar(Ch, not IsEscSeq);
    end;
    {we've now fully processed the key, so make sure it doesn't come
     back to bite us}
    Key := 0;
    {if this was the NumLock key, we'd better set it on again}
    if IsNumLock then begin
      vttToggleNumLock;
    end;
  end;
end;
{--------}
procedure TIpVT100Emulator.KeyPress(var Key : AnsiChar);
begin
  {on a key press, send the key to the host; for a CR character we
   either send CR (NewLineMode is OFF) or a CR/LF (NewLineMode is on}
  if (Key = #13) then begin
    teSendChar(Key, True);
    if NewLineMode then
      teSendChar(#10, True);
  end
  else
    teSendChar(Key, True);
end;
{--------}
procedure TIpVT100Emulator.LazyPaint;
var
  DirtyRect : TRect;
  Row       : Integer;
begin
  {the LazyPaint method is called by the terminal, if and only if
   either of the lazy timers expired; in other words, either a certain
   amount of data has been received by the terminal, or a certain time
   has elapsed since the last update}

  {if we have to refresh the display, do it, and throw away all the
   'dirty' data}
  if FRefresh then begin
    FRefresh := False;
    for Row := 1 to Buffer.RowCount do
      vttDrawChars(Row, 1, Buffer.ColCount, True, False);
    while Buffer.GetInvalidRect(DirtyRect) do {nothing};
  end
  else begin
    {using the 'dirty' data in the buffer, draw the required
     characters}
    while Buffer.GetInvalidRect(DirtyRect) do begin
      if not Terminal.FreezeScrollBack then begin                      {!!.03}
        Inc (DirtyRect.Top, Terminal.FScrollVertInfo.nPos);            {!!.03}
        Inc (DirtyRect.Bottom, Terminal.FScrollVertInfo.nPos + 1);     {!!.03}
      end;                                                             {!!.03}
      for Row := DirtyRect.Top to DirtyRect.Bottom do
        vttDrawChars(Row, DirtyRect.Left, DirtyRect.Right, True, False);
    end;
  end;
end;
{--------}
procedure TIpVT100Emulator.Paint;
var
  DirtyRect : TRect;
  Row       : Integer;
begin
  {the Paint method is called by the terminal, if and only if the
   terminal component received a WM_PAINT message}

  {repaint the clip region}
  DirtyRect := Terminal.Canvas.ClipRect;
  with Terminal do begin
    DirtyRect.Left :=
       (DirtyRect.Left div CharWidth) + ClientOriginCol;
    DirtyRect.Right :=
       (pred(DirtyRect.Right) div CharWidth) + ClientOriginCol;
    DirtyRect.Top :=
       (DirtyRect.Top div CharHeight) + ClientOriginRow;
    DirtyRect.Bottom :=
       (pred(DirtyRect.Bottom) div CharHeight) + ClientOriginRow;
  end;
  if not Terminal.FreezeScrollBack then begin                          {!!.03}
    Inc (DirtyRect.Top, Terminal.FScrollVertInfo.nPos);                {!!.03}
    Inc (DirtyRect.Bottom, Terminal.FScrollVertInfo.nPos + 1);         {!!.03}
  end;                                                                 {!!.03}
  for Row := DirtyRect.Top to DirtyRect.Bottom do
    vttDrawChars(Row, DirtyRect.Left, DirtyRect.Right, True, False);
end;
{--------}
procedure TIpVT100Emulator.ProcessBlock(aData : Pointer; aDataLen : Longint);
var
  DataAsChar : PChar absolute aData;
  i          : Integer;
begin
  for i := 0 to pred(aDataLen) do begin
    case Parser.ProcessChar(DataAsChar[i]) of
      pctNone :
        {ignore it};
      pctChar :
        begin
          {Terminal.ComPort.AddStringToLog(DataAsChar[i]);}
          Buffer.WriteChar(DataAsChar[i]);
        end;
      pct8BitChar:
        begin
          if DisplayUpperASCII then
            Buffer.WriteChar(DataAsChar[i])
          else
            vttProcess8bitChar(DataAsChar[i]);
        end;
      pctPending :
        {nothing to do yet};
      pctComplete :
        vttProcessCommand;
    end;
  end;
  vttCalcBlinkScript;
end;
{--------}
procedure TIpVT100Emulator.teClear;
begin
  Buffer.EraseScreen;
end;
{--------}
procedure TIpVT100Emulator.teClearAll;
begin
  Buffer.EraseAll;
end;
{--------}
function TIpVT100Emulator.teGetNeedsUpdate : Boolean;
begin
  Result := Buffer.HasDisplayChanged or FRefresh or                    {!!.01}
            Buffer.HasCursorMoved;                                     {!!.01}
end;
{--------}
procedure TIpVT100Emulator.teSetTerminal(aValue : TIpCustomTerminal);
begin
  inherited teSetTerminal(aValue);
  if (aValue <> nil) and (aValue.Font <> nil) and
     (FSecondaryFont <> nil) then
    FSecondaryFont.Assign(aValue.Font);
end;

{--------}
procedure TIpVT100Emulator.vttCalcBlinkScript;
var
  RowInx       : Integer;
  ColInx       : Integer;
  StartColInx  : Integer;
  Temp         : PBlinkNode;
  Attrs        : TIpTerminalCharAttrs;
  AttrArray    : PByteArray;
  InBlinkRegion: Boolean;
begin
  if Buffer.HasDisplayChanged then begin
    {clear the current script of blink regions}
    vttClearBlinkScript;
    {search for the current blink regions in the buffer}
    InBlinkRegion := False;
    StartColInx := 0;
    with Buffer do
      for RowInx := Terminal.ClientOriginRow to
                    pred(Terminal.ClientOriginRow + RowCount) do begin
        AttrArray := GetLineAttrPtr(RowInx);
        for ColInx := 0 to pred(ColCount) do begin
          Attrs := TIpTerminalCharAttrs(AttrArray^[ColInx]);
          if InBlinkRegion then begin
            if not (tcaBlink in Attrs) then begin
              InBlinkRegion := False;
              Temp := vttNewBlinkNode;
              Temp^.bnRow := RowInx;
              Temp^.bnStartCh := succ(StartColInx);
              Temp^.bnEndCh := ColInx;
              Temp^.bnNext := PBlinkNode(FBlinkers);
              FBlinkers := Temp;
            end;
          end
          else {not tracking blink region} begin
            if (tcaBlink in Attrs) then begin
              InBlinkRegion := True;
              StartColInx := ColInx;
            end;
          end;
        end;
      end;

    {close off the final blink region if there is one}
    if InBlinkRegion then begin
      Temp := vttNewBlinkNode;
      Temp^.bnRow := Buffer.RowCount;
      Temp^.bnStartCh := succ(StartColInx);
      Temp^.bnEndCh := Buffer.ColCount;
      Temp^.bnNext := PBlinkNode(FBlinkers);
      FBlinkers := Temp;
    end;
  end;
end;
{--------}
procedure TIpVT100Emulator.vttClearBlinkScript;
var
  Walker, Temp : PBlinkNode;
begin
  Walker := PBlinkNode(FBlinkers);
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.bnNext;
    vttFreeBlinkNode(Temp);
  end;
  FBlinkers := nil;
end;
{--------}
procedure TIpVT100Emulator.vttDrawBlinkOffCycle(
                                    aRow, aStartCh, aEndCh : Integer);
var
  ChNum      : Integer;
  BackColor  : TColor;
  BackColors : PIptLongArray;
  WorkRect   : TRect;
  CharWidth  : Integer;
  OriginCh   : Integer;
begin
  {get the character width}
  if (TIpLineAttrArray(FLineAttrArray).Attr[aRow] = ltNormal) then begin
    CharWidth := Terminal.CharWidth;
    OriginCh := Terminal.ClientOriginCol;
  end
  else begin
    CharWidth := Terminal.CharWidth * 2;
    OriginCh := ((Terminal.ClientOriginCol - 1) div 2) + 1;
  end;

  {get the pointers to the fore/background colors}
  if RevScreenMode then
    BackColors := Buffer.GetLineForeColorPtr(aRow)
  else
    BackColors := Buffer.GetLineBackColorPtr(aRow);

  {just paint the background}
  with Terminal.Canvas do begin
    {we have to display the background color in separate steps since
     different characters across the display may have different
     background colors; set to the loop variables}
    BackColor := BackColors^[aStartCh-1];
    ChNum := aStartCh;
    WorkRect :=
       Rect((aStartCh - OriginCh) * CharWidth,
            (aRow - Terminal.ClientOriginRow) * Terminal.CharHeight,
            0, {to be set in the loop}
            (aRow - Terminal.ClientOriginRow + 1) * Terminal.CharHeight);
    while (ChNum < aEndCh) do begin
      inc(ChNum);
      {if the background color changes, draw the rect in the old
       color, and then save the new color and start point}
      if (BackColor <> BackColors^[ChNum-1]) then begin
        WorkRect.Right := (ChNum - OriginCh) * CharWidth;
        Brush.Color := BackColor;
        FillRect(WorkRect);
        BackColor := BackColors^[ChNum-1];
        WorkRect.Left := WorkRect.Right;
      end;
    end;
    {finish off the last rect in the final color}
    WorkRect.Right := (aEndCh - OriginCh + 1) * CharWidth;
    Brush.Color := BackColor;
    FillRect(WorkRect);
  end;
end;
{--------}
procedure TIpVT100Emulator.vttDrawChars(aRow, aStartVal, aEndVal : Integer;
                                        aVisible : Boolean;
                                        aCharValues : Boolean);
var
  ChNum     : Integer;
  StartChNum : Integer;
  StartChar  : Integer;
  EndChar    : Integer;
  BackColor  : TColor;
  ForeColor  : TColor;
  Attr       : TIpTerminalCharAttrs;
  CharSet    : Byte;
  ForeColors : PIptLongArray;
  BackColors : PIptLongArray;
  Attrs      : PByteArray;
  CharSets   : PByteArray;
  Script     : PPaintNode;
  PaintNode  : PPaintNode;
  LineAttr   : TIpVT100LineAttr;
begin
  {ASSUMPTION: aStartVal <= aEndVal}

  {Notes: The terminal display can be viewed in two ways: either a
          certain number of characters across, or a certain number of
          columns across. When each character is exactly one column
          (or cell) in width it doesn't matter whether we talk about
          characters or columns. If the characters are double width,
          it *does* matter, since 1 character unit is then equivalent
          to two column units. aCharValues True means that aStartVal
          and aEndVal are character values; False means that they are
          column values. In the latter case, if the row consists of
          double-width characters then we'll have to convert the
          column values to character values.}

  {avoid any drawing if the row simply is not visible}
  if (aRow < Terminal.ClientOriginRow) or
     (aRow >= Terminal.ClientOriginRow + Terminal.ClientRows) then
    Exit;

  {get the line attribute}
  LineAttr := TIpLineAttrArray(FlineAttrArray).Attr[aRow];

  {if the start/end parameters are column values, convert to character
   values}
  if not aCharValues then begin
    if (LineAttr <> ltNormal) then begin
      aStartVal := ((aStartVal - 1) div 2) + 1;
      aEndVal := ((aEndVal - 1) div 2) + 1;
    end;
  end;

  {check to see whether the range of characters is visible, force
   values into visible range}
  if (LineAttr = ltNormal) then begin
    StartChar := Terminal.ClientOriginCol;
    EndChar := Terminal.ClientOriginCol + Terminal.ClientCols;
  end
  else {double-width} begin
    StartChar := ((Terminal.ClientOriginCol - 1) div 2) + 1;
    EndChar := ((Terminal.ClientOriginCol + Terminal.ClientCols - 1)
               div 2) + 1;
  end;
  if (aEndVal < StartChar) or (aStartVal >= EndChar) then
    Exit;
  aStartVal := IpMaxInt(aStartVal, StartChar);
  aEndVal := IpMinInt(aEndVal, EndChar);

  {if this point is reached, we have to paint *something*}

  {there will be calls to this method that are just drawing blinking
   characters for the 'off' cycle (aVisible = False), so make this a
   special case}
  if not aVisible then begin
    vttDrawBlinkOffCycle(aRow, aStartVal, aEndVal);
    Exit;
  end;

  {if this row is the bottom half of a double-height row we don't have
   to display any text since it is displayed with the top half}
  if (LineAttr = ltDblHeightBottom) then
    dec(aRow);

  {this is the main processing: in general we'll be displaying text in
   this section; what will happen here, is that we first generate a
   script of drawing commands (which background color, which text
   color, which attributes, which character set, which text), and then
   we execute the script}

  {get the pointers to the colors, the attributes, the charsets}
  BackColors := Buffer.GetLineBackColorPtr(aRow);
  ForeColors := Buffer.GetLineForeColorPtr(aRow);
  Attrs := Buffer.GetLineAttrPtr(aRow);
  CharSets := Buffer.GetLineCharSetPtr(aRow);

  {get the initial values for the display variables}
  BackColor := BackColors^[aStartVal-1];
  ForeColor := ForeColors^[aStartVal-1];
  Attr := TIpTerminalCharAttrs(Attrs^[aStartVal-1]);
  CharSet := CharSets^[aStartVal-1];

  {make a note of the start char number}
  StartChNum := aStartVal;
  ChNum := aStartVal;

  {build the script as a stack of paint commands}
  Script := nil;
  while (ChNum < aEndVal) do begin
    {look at the next char number}
    inc(ChNum);
    {if any info has changed...}
    if (ForeColor <> ForeColors^[ChNum-1]) or
       (BackColor <> BackColors^[ChNum-1]) or
       (Attr <> TIpTerminalCharAttrs(Attrs^[ChNum-1])) or
       (CharSet <> CharSets^[ChNum-1]) then begin
      {get a new node, initialize it}
      PaintNode := vttNewPaintNode;
      PaintNode^.pnStart := StartChNum;
      PaintNode^.pnEnd := pred(ChNum);
      PaintNode^.pnFore := ForeColor;
      PaintNode^.pnBack := BackColor;
      PaintNode^.pnAttr := Attr;
      PaintNode^.pnCSet := CharSet;
      {add it to the script}
      PaintNode^.pnNext := Script;
      Script := PaintNode;
      {save the new values of the variables}
      ForeColor := ForeColors^[ChNum-1];
      BackColor := BackColors^[ChNum-1];
      Attr := TIpTerminalCharAttrs(Attrs^[ChNum-1]);
      CharSet := CharSets^[ChNum-1];
      StartChNum := ChNum;
    end;
  end;
  {create the final paint command}
  PaintNode := vttNewPaintNode;
  PaintNode^.pnStart := StartChNum;
  PaintNode^.pnEnd := aEndVal;
  PaintNode^.pnFore := ForeColor;
  PaintNode^.pnBack := BackColor;
  PaintNode^.pnAttr := Attr;
  PaintNode^.pnCSet := CharSet;
  {add it to the script}
  PaintNode^.pnNext := Script;
  Script := PaintNode;

  {now execute the paint script}
  vttExecutePaintScript(aRow, Script);
end;
{--------}
procedure TIpVT100Emulator.vttExecutePaintScript(aRow    : Integer;
                                                 aScript : Pointer);
var
  Canvas       : TCanvas;
  Font         : TFont;
  Walker, Temp : PPaintNode;
  ForeColor    : TColor;
  BackColor    : TColor;
  CharWidth    : Integer;
  CharHeight   : Integer;
  OriginCol    : Integer;
  OffsetCol    : Integer;
  TextStrLen   : Integer;
  TextChars    : PAnsiChar;
  PartTextLen  : Integer;
  TextCol      : Integer;
  FontName     : TIpKeyString;
  WorkRect     : TRect;
  Y            : Integer;
  R, G, B      : Integer;
  Reversed     : Boolean;
  DblHeight    : Boolean;
begin
  case TIpLineAttrArray(FLineAttrArray).Attr[aRow] of
    ltNormal          : DblHeight := False;
    ltDblHeightTop    : DblHeight := True;
    ltDblHeightBottom : DblHeight := True;
    ltDblWidth        : DblHeight := False;
  else
    DblHeight := False;
  end;
  {get some values as local variables}
  Canvas := Terminal.Canvas;
  Font := Terminal.Font;
  TextChars := Buffer.GetLineCharPtr(aRow);
  CharHeight := Terminal.CharHeight;
  WorkRect.Top := (aRow - Terminal.ClientOriginRow) * CharHeight;
  if DblHeight then
    WorkRect.Bottom := WorkRect.Top + (CharHeight * 2)
  else
    WorkRect.Bottom := WorkRect.Top + CharHeight;
  if (TIpLineAttrArray(FLineAttrArray).Attr[aRow] = ltNormal) then begin
    CharWidth := Terminal.CharWidth;
    OriginCol := Terminal.ClientOriginCol;
    OffsetCol := 0;
  end
  else begin
    CharWidth := Terminal.CharWidth * 2;
    OriginCol := ((Terminal.ClientOriginCol - 1) div 2) + 1;
    if Odd(Terminal.ClientOriginCol) then
      OffsetCol := 0
    else
      OffsetCol := -Terminal.CharWidth;
  end;
  {set the cell widths}
  for Y := 0 to pred(Buffer.ColCount) do
    FCellWidths^[Y] := CharWidth;
  {process the script}
  Walker := PPaintNode(aScript);
  while (Walker <> nil) do begin
    {check for reverse}
    Reversed := RevScreenMode xor
                (tcaReverse in Walker^.pnAttr) xor
                (tcaSelected in Walker^.pnAttr);
    if Reversed then begin
      ForeColor := Walker^.pnBack;
      BackColor := Walker^.pnFore;
    end
    else begin
      ForeColor := Walker^.pnFore;
      BackColor := Walker^.pnBack;
    end;
    {check for invisible}
    if (tcaInvisible in Walker^.pnAttr) then
      ForeColor := BackColor
    {check for bold}
    else if (tcaBold in Walker^.pnAttr) then begin
      if Reversed then begin
        R := IpMinInt(Integer(GetRValue(BackColor)) + $80, $FF);
        G := IpMinInt(Integer(GetGValue(BackColor)) + $80, $FF);
        B := IpMinInt(Integer(GetBValue(BackColor)) + $80, $FF);
        BackColor := RGB(R, G, B);
      end
      else begin
        R := IpMinInt(Integer(GetRValue(ForeColor)) + $80, $FF);
        G := IpMinInt(Integer(GetGValue(ForeColor)) + $80, $FF);
        B := IpMinInt(Integer(GetBValue(ForeColor)) + $80, $FF);
        ForeColor := RGB(R, G, B);
      end;
    end;

    {get the length of the text to display}
    TextStrLen := succ(Walker^.pnEnd - Walker^.pnStart);

    {move the required text to our display string}
    Move(TextChars[Walker^.pnStart - 1], FDisplayStr^, TextStrLen);
    FDisplayStr[TextStrLen] := #0;

    {set the correct background}
    Canvas.Brush.Color := BackColor;

    {ask the charset mapping to create a draw script for this text}
    FCharSetMapping.GenerateDrawScript(
       VT100CharSetNames[Walker^.pnCSet], FDisplayStr);

    {while the charset mapping passes us back draw commands, draw}
    TextCol := Walker^.pnStart - OriginCol;
    while FCharSetMapping.GetNextDrawCommand(FontName, FDisplayStr) do begin
      {calculate the length of this bit o' text}
      PartTextLen := StrLen(FDisplayStr);

      { calculate the left and right values for the rect }
      WorkRect.Left := (TextCol * CharWidth) + OffsetCol;
      WorkRect.Right := WorkRect.Left + (PartTextLen * CharWidth);

      { display the bit o'text }
      if (FontName = DefaultFontName) then
        Canvas.Font := Font
      else begin
        FSecondaryFont.Name := FontName;
        Canvas.Font := FSecondaryFont;
      end;
      Canvas.Font.Color := ForeColor;
      {$IFDEF VERSION3}
      Canvas.Font.CharSet := DEFAULT_CHARSET;
      {$ENDIF}
      if DblHeight then
        Canvas.Font.Size := Canvas.Font.Size * 2;

      ExtTextOut(Canvas.Handle,
                 WorkRect.Left,
                 WorkRect.Top,
                 ETO_OPAQUE,
                 @WorkRect,
                 FDisplayStr,
                 PartTextLen,
                 @FCellWidths^);

      { finally, draw the underline and/or strike through }
      Canvas.Pen.Color := ForeColor;
      if (tcaUnderline in Walker^.pnAttr) then begin
        Y := WorkRect.Bottom - 2;
        Canvas.MoveTo(WorkRect.Left, Y);
        Canvas.LineTo(WorkRect.Right, Y);
      end;
      if (tcaStrikeThrough in Walker^.pnAttr) then begin
        Y := WorkRect.Top + (WorkRect.Bottom - WorkRect.Top) div 2;
        Canvas.MoveTo(WorkRect.Left, Y);
        Canvas.LineTo(WorkRect.Right, Y);
      end;

      {advance past this bit o'text}
      inc(TextCol, PartTextLen);
    end;

    {walk to the next paint node, free this one}
    Temp := Walker;
    Walker := Walker^.pnNext;
    vttFreePaintNode(Temp);
  end;
end;
{--------}
procedure TIpVT100Emulator.vttFreeAllBlinkNodes;
var
  Walker, Temp : PBlinkNode;
begin
  Walker := FBlinkFreeList;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.bnNext;
    Dispose(Temp);
  end;
  FBlinkFreeList := nil;
end;
{--------}
procedure TIpVT100Emulator.vttFreeAllPaintNodes;
var
  Walker, Temp : PPaintNode;
begin
  Walker := FPaintFreeList;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.pnNext;
    Dispose(Temp);
  end;
  FPaintFreeList := nil;
end;
{--------}
procedure TIpVT100Emulator.vttFreeBlinkNode(aNode : Pointer);
begin
  PBlinkNode(aNode)^.bnNext := FBlinkFreeList;
  FBlinkFreeList := aNode;
end;
{--------}
procedure TIpVT100Emulator.vttFreePaintNode(aNode : Pointer);
begin
  PPaintNode(aNode)^.pnNext := FPaintFreeList;
  FPaintFreeList := aNode;
end;
{--------}
function TIpVT100Emulator.vttGenerateDECREPTPARM(aArg : Integer) : string;
const
  par = 1; nbits = 1; xspd = 120; rspd = 120;
begin
  { Hard-coded values, since these really don't apply very well to WinSock }
  Result := Format(VT100ReportParm, [aArg+2, par, nbits, xspd, rspd, 1, 0]);
end;
{--------}
procedure TIpVT100Emulator.vttInvalidateRow(aRow : Integer);
var
  InvRect : TRect;
begin
  if Terminal.HandleAllocated and
     (Terminal.ClientOriginRow <= aRow) and
     (aRow < Terminal.ClientOriginRow + Terminal.ClientRows) then begin
    InvRect.Left := 0;
    InvRect.Top := (aRow - Terminal.ClientOriginRow) * Terminal.CharHeight;
    InvRect.Right := Terminal.ClientWidth;
    InvRect.Bottom := InvRect.Top + Terminal.CharHeight;
    InvalidateRect(Terminal.Handle, @InvRect, False);
  end;
end;
{--------}
function TIpVT100Emulator.vttNewBlinkNode : Pointer;
begin
  if (FBlinkFreeList = nil) then
    New(PBlinkNode(Result))
  else begin
    Result := FBlinkFreeList;
    FBlinkFreeList := PBlinkNode(Result)^.bnNext;
  end;
end;
{--------}
function TIpVT100Emulator.vttNewPaintNode : Pointer;
begin
  if (FPaintFreeList = nil) then
    New(PPaintNode(Result))
  else begin
    Result := FPaintFreeList;
    FPaintFreeList := PPaintNode(Result)^.pnNext;
  end;
end;
{--------}
procedure TIpVT100Emulator.vttProcess8bitChar(aCh : AnsiChar);
const
  FirstChar = #176;
  LastChar = #218;
  CvtChars : array [FirstChar..LastChar] of AnsiChar =
             'aaaxuuukkuxkjjjkmvwtqnttmlvwtqnvvwwmmllnnjl';
var
  OldCharSet : Byte;
begin
  { convert the line draw characters from the OEM character set to the }
  { VT100 linedraw charset }
  if (FirstChar <= aCh) and (aCh <= LastChar) then begin
    OldCharSet := Buffer.CharSet;
    Buffer.CharSet := 2;
    Buffer.WriteChar(CvtChars[aCh]);
    Buffer.CharSet := OldCharSet;
  end;
end;
{--------}
procedure TIpVT100Emulator.vttProcessCommand;
var
  i   : Integer;
  Arg : Integer;
  Arg2: Integer;
  OldDefChar : AnsiChar;
  Attrs      : TIpTerminalCharAttrs;
begin
  { assumption: this method is called immediately Parser.ProcessChar }
  {             returns pctComplete. Hence Parser.ArgumentCount,     }
  {             Parser.Argument, Parser.Command, Parser.Sequence     }
  {             are all set properly                                 }

  {$IFDEF TermDebug}
  if (Terminal.SockControl <> nil) and (Terminal.Socket <> Invalid_Socket) then
    Terminal.SockControl.DebugLog.WriteDebugString(Parser.Sequence);
  {$ENDIF}

  {WARNING: this case statement is in numeric order!}
  case Parser.Command of
    eGotoXY {or eCUP, eHVP} :
      begin
        if (Parser.ArgumentCount = 2) then
          Buffer.SetCursorPosition(Parser.Argument[0], Parser.Argument[1]);
      end;
    eUp {or eCUU} :
      begin
        if (Parser.ArgumentCount = 1) then begin
          Arg := Parser.Argument[0];
          if (Arg = 0) then
            Arg := 1;
          for i := 1 to Arg do
            Buffer.MoveCursorUp(False);
        end;
      end;
    eDown {or eCUD, eIND, eVPR} :
      begin
        if (Parser.ArgumentCount = 1) then begin
          Arg := Parser.Argument[0];
          if (Arg = 0) then
            Arg := 1;
          for i := 1 to Arg do
            Buffer.MoveCursorDown(False);
        end;
      end;
    eRight {or eCUF, eHPR} :
      begin
        if (Parser.ArgumentCount = 1) then begin
          Arg := Parser.Argument[0];
          if (Arg = 0) then
            Arg := 1;
          for i := 1 to Arg do
            Buffer.MoveCursorRight(False, False);
        end;
      end;
    eLeft {or eCUB} :
      begin
        if (Parser.ArgumentCount = 1) then begin
          Arg := Parser.Argument[0];
          if (Arg = 0) then
            Arg := 1;
          for i := 1 to Arg do
            Buffer.MoveCursorLeft(False, False);
        end;
      end;
    eSetMode {or eSM} :
      begin
        if (Parser.ArgumentCount = 1) then begin
          case Parser.Argument[0] of
             4 : begin
                   InsertMode := True;
                   Buffer.UseInsertMode := True;
                 end;
            20 : begin
                   NewLineMode := True;
                   Buffer.UseNewLineMode := True;
                 end;
          end;{case}
        end
        else if (Parser.ArgumentCount = 2) and
                (Parser.Argument[0] = -2) then begin
          case Parser.Argument[1] of
            1 : AppKeyMode := True;
            2 : ANSIMode := True;
            3 : begin
                  Col132Mode := True;
                end;
            4 : SmoothScrollMode := True;
            5 : begin
                  RevScreenMode := True;
                end;
            6 : begin
                  RelOriginMode := True;
                end;
            7 : begin
                  WrapAround := True;
                  Buffer.UseAutoWrap := True;
                end;
            8 : AutoRepeat := True;
            9 : Interlace := True;
            999 {special IPRO value} : AppKeypadMode := True;
          end;{case}
        end;
      end;
    eSetAttribute {or eSGR} :
      begin
        for i := 0 to pred(Parser.ArgumentCount) do begin
          case Parser.Argument[i] of
             0 : begin
                   Buffer.SetCharAttrs([]);
                   Buffer.ForeColor := Buffer.DefForeColor;
                   Buffer.BackColor := Buffer.DefBackColor;
                 end;
             1 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Include(Attrs, tcaBold);
                   Buffer.SetCharAttrs(Attrs);
                 end;
             4 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Include(Attrs, tcaUnderline);
                   Buffer.SetCharAttrs(Attrs);
                 end;
             5 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Include(Attrs, tcaBlink);
                   Buffer.SetCharAttrs(Attrs);
                 end;
             7 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Include(Attrs, tcaReverse);
                   Buffer.SetCharAttrs(Attrs);
                 end;
             8 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Include(Attrs, tcaInvisible);
                   Buffer.SetCharAttrs(Attrs);
                 end;
            22 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Exclude(Attrs, tcaBold);
                   Buffer.SetCharAttrs(Attrs);
                 end;
            24 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Exclude(Attrs, tcaUnderline);
                   Buffer.SetCharAttrs(Attrs);
                 end;
            25 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Exclude(Attrs, tcaBlink);
                   Buffer.SetCharAttrs(Attrs);
                 end;
            27 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Exclude(Attrs, tcaReverse);
                   Buffer.SetCharAttrs(Attrs);
                 end;
            28 : begin
                   Buffer.GetCharAttrs(Attrs);
                   Exclude(Attrs, tcaInvisible);
                   Buffer.SetCharAttrs(Attrs);
                 end;
            30 : Buffer.ForeColor := clBlack;
            31 : Buffer.ForeColor := clMaroon;
            32 : Buffer.ForeColor := clGreen;
            33 : Buffer.ForeColor := clOlive;
            34 : Buffer.ForeColor := clNavy;
            35 : Buffer.ForeColor := clPurple;
            36 : Buffer.ForeColor := clTeal;
            37 : Buffer.ForeColor := clSilver;
            40 : Buffer.BackColor := clBlack;
            41 : Buffer.BackColor := clMaroon;
            42 : Buffer.BackColor := clGreen;
            43 : Buffer.BackColor := clOlive;
            44 : Buffer.BackColor := clNavy;
            45 : Buffer.BackColor := clPurple;
            46 : Buffer.BackColor := clTeal;
            47 : Buffer.BackColor := clSilver;
          end;{case}
        end;
      end;
    eSaveCursorPos :
      begin
        Buffer.GetCharAttrs(FSavedAttrs);
        FSavedBackColor := Buffer.BackColor;
        FSavedCharSet := Buffer.CharSet;
        FSavedCol := Buffer.Col;
        FSavedForeColor := Buffer.ForeColor;
        FSavedRow := Buffer.Row;
      end;
    eRestoreCursorPos :
      begin
        Buffer.SetCharAttrs(FSavedAttrs);
        Buffer.BackColor := FSavedBackColor;
        Buffer.CharSet := FSavedCharSet;
        Buffer.Col := FSavedCol;
        Buffer.ForeColor := FSavedForeColor;
        Buffer.Row := FSavedRow;
      end;
    eDeviceStatusReport {or eDSR} :
      begin
        { we *only* issue a reply if we are the master terminal,   }
        { otherwise if there were several terminals on a form, all }
        { attached to the same comport, the host would get several }
        { responses }
        if Terminal.IsMasterTerminal then begin
          if (Parser.ArgumentCount = 1) then
            if (Parser.Argument[0] = 5) then
              TIpSockControlFriend(Terminal.SockControl).scPutString(Terminal.Socket,
                VT100StatusRpt, False)
            else if (Parser.Argument[0] = 6) then
              TIpSockControlFriend(Terminal.SockControl).scPutString(Terminal.Socket,
                Format(VT100CursorPos, [Buffer.Row, Buffer.Col]), False);
        end;
      end;
    eCHT :
      begin
        if (Parser.ArgumentCount = 1) then
          for i := 1 to Parser.Argument[0] do
            Buffer.DoHorzTab;
      end;
    eCVT :
      begin
        if (Parser.ArgumentCount = 1) then
          for i := 1 to Parser.Argument[0] do
            Buffer.DoVertTab;
      end;
    eDA :
      begin
        { we *only* issue a reply if we are the master terminal,   }
        { otherwise if there were several terminals on a form, all }
        { attached to the same comport, the host would get several }
        { responses }
        if Terminal.IsMasterTerminal then begin
          if (Parser.ArgumentCount = 1) then
            if (Parser.Argument[0] = 0) then
              TIpSockControlFriend(Terminal.SockControl).scPutString(Terminal.Socket,
                VT100DeviceAttrs, False)
            else if (Parser.Argument[0] = 52) then
              TIpSockControlFriend(Terminal.SockControl).scPutString(Terminal.Socket,
                VT52DeviceAttrs, False);
        end;
      end;
    eDCH :
      begin
        if (Parser.ArgumentCount > 0) then
          Buffer.DeleteChars(Parser.Argument[0]);
      end;
    eDL :
      begin
        if (Parser.ArgumentCount > 0) then
          Buffer.DeleteLines(Parser.Argument[0]);
      end;
    eECH :
      begin
        if (Parser.ArgumentCount > 0) then
          Buffer.EraseChars(Parser.Argument[0]);
      end;
    eED :
      begin
        if (Parser.ArgumentCount = 1) then
          case Parser.Argument[0] of
            0 : Buffer.EraseToEOW;
            1 : Buffer.EraseFromBOW;
            2 : Buffer.EraseScreen;
          end;
      end;
    eEL :
      begin
        if (Parser.ArgumentCount = 1) then
          case Parser.Argument[0] of
            0 : Buffer.EraseToEOL;
            1 : Buffer.EraseFromBOL;
            2 : Buffer.EraseLine;
          end;
      end;
    eHTS :
      begin
        Buffer.SetHorzTabStop;
      end;
    eICH :
      begin
        if (Parser.ArgumentCount > 0) then
          Buffer.InsertChars(Parser.Argument[0]);
      end;
    eIL :
      begin
        if (Parser.ArgumentCount > 0) then
          Buffer.InsertLines(Parser.Argument[0]);
      end;
    eNEL :
      begin
        Buffer.DoLineFeed;
        if not NewLineMode then
          Buffer.DoCarriageReturn;
      end;
    eRI :
      begin
        Buffer.MoveCursorUp(True);
      end;
    eRIS :
      begin
        Buffer.Reset; {also clears the screen} 

        ANSIMode := ipc_VT100ANSIMode;
        AppKeyMode := ipc_VT100AppKeyMode;
        AppKeypadMode := ipc_VT100AppKeypadMode;
        AutoRepeat := ipc_VT100AutoRepeat;
        Col132Mode := ipc_VT100Col132Mode;
        GPOMode := ipc_VT100GPOMode;
        InsertMode := ipc_VT100InsertMode;
        Interlace := ipc_VT100Interlace;
        NewLineMode := ipc_VT100NewLineMode;
        RelOriginMode := ipc_VT100RelOriginMode;
        RevScreenMode := ipc_VT100RevScreenMode;
        SmoothScrollMode := ipc_VT100SmoothScrollMode;
        WrapAround := ipc_VT100WrapAround;

        FUsingG1 := False;                                   
        FG0CharSet := ipc_VT100G0CharSet;
        FG1CharSet := ipc_VT100G1CharSet;
      end;
    eRM :
      begin
        if (Parser.ArgumentCount = 1) then begin
          case Parser.Argument[0] of
             4 : begin
                   InsertMode := False;
                   Buffer.UseInsertMode := False;
                 end;
            20 : begin
                   NewLineMode := False;
                   Buffer.UseNewLineMode := False;
                 end;
          end;{case}
        end
        else if (Parser.ArgumentCount = 2) and
                (Parser.Argument[0] = -2) then begin
          case Parser.Argument[1] of
            1 : AppKeyMode := False;
            2 : ANSIMode := False;
            3 : begin
                  Col132Mode := False;
                end;
            4 : SmoothScrollMode := False;
            5 : begin
                  RevScreenMode := False;
                end;
            6 : begin
                  RelOriginMode := False;
                end;
            7 : begin
                  WrapAround := False;
                  Buffer.UseAutoWrap := False;
                end;
            8 : AutoRepeat := False;
            9 : Interlace := False;
            999 {special IPRO value} : AppKeypadMode := False;
          end;{case}
        end;
      end;
    eTBC :
      begin
        if (Parser.Argument[0] = 0) then
          Buffer.ClearHorzTabStop
        else if (Parser.Argument[0] = 3) then
          Buffer.ClearAllHorzTabStops;
      end;
    eDECSTBM :
      begin
        Arg := Parser.Argument[0];
        if (Arg = -1) or (Arg = 0) then
          Arg := 1;
        Arg2 := Parser.Argument[1];
        if (Arg2 = -1) or (Arg2 = 0) then
          Arg2 := Buffer.RowCount;
        Buffer.SetScrollRegion(Arg, Arg2);
      end;
    eENQ :
      begin
        { we *only* issue a reply if we are the master terminal,   }
        { otherwise if there were several terminals on a form, all }
        { attached to the same comport, the host would get several }
        { responses }
        if Terminal.IsMasterTerminal then begin
          TIpSockControlFriend(Terminal.SockControl).scPutString(Terminal.Socket,
            Answerback, False);
        end;
      end;
    eBel :
      begin
        MessageBeep(MB_ICONASTERISK);
      end;
    eBS :
      begin
        Buffer.DoBackspace;
      end;
    eLF :
      begin
        Buffer.DoLineFeed;
      end;
    eCR :
      begin
        Buffer.DoCarriageReturn;
      end;
    eSO :
      begin
        if not ANSIMode then
          Buffer.CharSet := 2
        else
          Buffer.Charset := FG1CharSet;
        FUsingG1 := True;
      end;
    eSI :
      begin
        if not ANSIMode then
          Buffer.CharSet := 0
        else
          Buffer.Charset := FG0CharSet;
        FUsingG1 := False;                                   
      end;
    eIND2 :
      begin
        Buffer.MoveCursorDown(True);
      end;
    eDECALN :
      begin
        {scroll buffer up, fill screen with character E's}
        OldDefChar := Buffer.DefAnsiChar;
        Buffer.DefAnsiChar := 'E';
        Buffer.EraseScreen;
        Buffer.DefAnsiChar := OldDefChar;
      end;
    eDECDHL :
      begin
        if (Parser.ArgumentCount = 1) then begin
          with TIpLineAttrArray(FLineAttrArray) do
            if (Parser.Argument[0] = 0) then
              Attr[Buffer.Row] := ltDblHeightTop
            else
              Attr[Buffer.Row] := ltDblHeightBottom;
          vttInvalidateRow(Buffer.Row);
        end;
      end;
    eDECDWL :
      begin
        with TIpLineAttrArray(FLineAttrArray) do
          Attr[Buffer.Row] := ltDblWidth;
        vttInvalidateRow(Buffer.Row);
      end;
    eDECLL :
      begin
        for i := 0 to pred(Parser.ArgumentCount) do
          case Parser.Argument[i] of
            0 : LEDs := 0;
            1 : LEDs := LEDs or $1;
            2 : LEDs := LEDs or $2;
            3 : LEDs := LEDs or $4;
            4 : LEDs := LEDs or $8;
          end;{case}
      end;
    eDECREQTPARM :
      begin
        { we *only* issue a reply if we are the master terminal,   }
        { otherwise if there were several terminals on a form, all }
        { attached to the same comport, the host would get several }
        { responses }
        if Terminal.IsMasterTerminal then begin
          Arg := 1;
          if (Parser.ArgumentCount = 1) then begin
            Arg := Parser.Argument[0];
            if (Arg < 0) then
              Arg := 0
            else if (Arg > 1) then
              Arg := 1;
          end;
          TIpSockControlFriend(Terminal.SockControl).scPutString(Terminal.Socket,
            vttGenerateDECREPTPARM(Arg), False);
        end;
      end;
    eDECSWL :
      begin
        with TIpLineAttrArray(FLineAttrArray) do
          Attr[Buffer.Row] := ltNormal;
        vttInvalidateRow(Buffer.Row);
      end;
    eDECTST :
      begin
        {}
      end;
    eDECSCS :
      begin
        if Parser.ArgumentCount = 2 then
          case Parser.Argument[0] of
            0 :
              begin
                case Parser.Argument[1] of
                  0        : FG0CharSet := 2;
                  1        : FG0CharSet := 3;
                  2        : FG0CharSet := 4;
                  ord('A') : FG0CharSet := 1;
                  ord('B') : FG0CharSet := 0;
                end;
                if not FUsingG1 then
                  Buffer.Charset := FG0CharSet;
              end;
            1 :
              begin
                case Parser.Argument[1] of
                  0        : FG1CharSet := 2;
                  1        : FG1CharSet := 3;
                  2        : FG1CharSet := 4;
                  ord('A') : FG1CharSet := 1;
                  ord('B') : FG1CharSet := 0;
                end;
                if FUsingG1 then
                  Buffer.Charset := FG1CharSet;
              end;
          end{case}
      end;
  end;{case}
end;
{--------}
procedure TIpVT100Emulator.vttScrollRowsHandler(aSender : TObject;
                                     aCount, aTop, aBottom : Integer);
begin
  TIpLineAttrArray(FLIneAttrArray).Scroll(aCount, aTop, aBottom);
end;
{--------}
procedure TIpVT100Emulator.vttSetCol132Mode(aValue : Boolean);
begin
  if (aValue <> Col132Mode) then begin
    FCol132Mode := aValue;
    if Col132Mode then
      Terminal.Columns := 132
    else
      Terminal.Columns := 80;
  end;
end;
{--------}
procedure TIpVT100Emulator.vttSetRelOriginMode(aValue : Boolean);
begin
  if (aValue <> RelOriginMode) then begin
    FRelOriginMode := aValue;
    Buffer.UseAbsAddress := not aValue;
  end;
end;
{--------}
procedure TIpVT100Emulator.vttSetRevScreenMode(aValue : Boolean);
begin
  if (aValue <> RevScreenMode) then begin
    FRevScreenMode := aValue;
    FRefresh := True;
  end;
end;
{--------}
procedure TIpVT100Emulator.vttToggleNumLock;
var
  NumLockState : TKeyboardState;
begin
  GetKeyboardState(NumLockState);
  if ((NumLockState[VK_NUMLOCK] and $01) = 0) then begin
    NumLockState[VK_NUMLOCK] := NumLockState[VK_NUMLOCK] or $01;
  end
  else begin
    NumLockState[VK_NUMLOCK] := NumLockState[VK_NUMLOCK] and $FE;
  end;
  SetKeyboardState(NumLockState);
end;
{====================================================================}


{===Initialization/finalization======================================}
procedure IPTrmEmuDone; far;
var
  Node, Temp : PTermEmuLink;
begin
  {dispose of active links}
  Node := TermEmuLink;
  while (Node <> nil) do begin
    Temp := Node;
    Node := Node^.telNext;
    Dispose(Temp);
  end;
  {dispose of inactive links}
  Node := TermEmuLinkFreeList;
  while (Node <> nil) do begin
    Temp := Node;
    Node := Node^.telNext;
    Dispose(Temp);
  end;
end;

initialization
  TermEmuLink := nil;
  TermEmuLinkFreeList := nil;
  InvRectFreeList := nil;
  InvRectPageList := nil;

finalization
  IPTrmEmuDone;
  IPTFreeInvRectPages;
end.

