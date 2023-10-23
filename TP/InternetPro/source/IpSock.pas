{******************************************************************}
{*     IPSOCK.PAS - DLL Interfaces and Base Socket Components     *}
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
 * Markus Kaemmerer <mk@happyarts.de> SourceForge: mkaemmerer
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

{Options required for this unit}
{$B-}

{ Debug option to force WinSock 1 even if a newer WinSock is available }
{.$DEFINE ForceWinSock1}

unit IpSock;

interface

uses
  Messages,
  Windows,
  Classes,
  SysUtils,
  IpUtils,
  IpConst;

{ Hdc needs to be an Integer for BCB compatibility }                  {!!.12}
{$IFDEF CBuilder}                                                     {!!.12}
type                                                                  {!!.12}
  TIpHdc  = Integer;                                                  {!!.12}
  TIpHWND = Cardinal;                                                 {!!.12}
{$ELSE}                                                               {!!.12}
type                                                                  {!!.12}
  TIpHdc  = HDC;                                                      {!!.12}
  TIpHWND = HWND;                                                     {!!.12}
{$ENDIF}                                                              {!!.12}


const

  IPStrSize = 15;           { String size for a dotted-quad IP address }

  IpSockVers = 2;           { Number of WinSock versions we support, currently }
                            { two of them -- WinSock 1.1 and WinSock 2 }

  IpHashSize = 127;         { Size of hash table used in server components }

  MtuSize = 1460;           { Assumed size for TCP max transmittable unit }

  MaxDebugLog = 16000000;   { Max size of debug log buffer }

  MaxLineBuf = 1024 * 1024; { Max line buffer handled for ReadLine }

const

  { Telnet Commands }
  TELNET_IAC   = #255;  { Interpret as Command }
  TELNET_DONT  = #254;  { Stop performing, or not expecting him to perform }
  TELNET_DO    = #253;  { Perform, or expect him to perform }
  TELNET_WONT  = #252;  { Refusal to perform }
  TELNET_WILL  = #251;  { Desire to perform }
  TELNET_SB    = #250;  { What follows is sub-negotiation of indicated option }
  TELNET_GA    = #249;  { Go ahead signal }
  TELNET_EL    = #248;  { Erase Line function }
  TELNET_EC    = #247;  { Erase Character function }
  TELNET_AYT   = #246;  { Are You There function }
  TELNET_AO    = #245;  { Abort Output function }
  TELNET_IP    = #244;  { Interrupt Process function }
  TELNET_BRK   = #243;  { NVT break character }
  TELNET_DM    = #242;  { Data stream portion of a Synch (DATAMARK) }
  TELNET_NOP   = #241;  { No operation }
  TELNET_SE    = #240;  { End of sub-negotiation parameters }
  TELNET_EOR   = #239;  { End of record }
  TELNET_ABORT = #238;  { Abort process }
  TELNET_SUSP  = #237;  { Suspend current process }
  TELNET_EOF   = #236;  { End of file }

  TELNET_NULL  = #0;    { No operation }
  TELNET_BEL   = #7;    { Bell }
  TELNET_BS    = #8;    { Back Space }
  TELNET_HT    = #9;    { Horizontal Tab }
  TELNET_LF    = #10;   { Line Feed }
  TELNET_VT    = #11;   { Vertical Tab }
  TELNET_FF    = #12;   { Form Feed }
  TELNET_CR    = #13;   { Carriage Return }

  { Telnet Options }
  {? Means I couldn't find the "actual" spelling of the option identifier }
  { (probably because the doc covering the option is something other than }
  { an RFC) so I took a stab at it on my own -- the value and description }
  { is correct, but the identifier may not match the associated document. }
  TELNETOPT_FIRSTOPT        = #0;    { iPRO specific placeholder }
  TELNETOPT_BINARY          = #0;    { Transmit binary }
  TELNETOPT_ECHO            = #1;    { Echo mode }
  TELNETOPT_RECONN          = #2;    { Reconnection }
  TELNETOPT_SUPGA           = #3;    { Suppress Go-Ahead }
  TELNETOPT_AMS{?}          = #4;    { Approx Msg Size }
  TELNETOPT_STATUS          = #5;    { Status }
  TELNETOPT_MARK            = #6;    { Timing mark }
  TELNETOPT_RCTE            = #7;    { Remote Trans & Echo }
  TELNETOPT_OLW{?}          = #8;    { Output Line Width }
  TELNETOPT_OPS{?}          = #9;    { Output Page Size }
  TELNETOPT_NAOCRD          = #10;   { Output C/R Disp }
  TELNETOPT_NAOHTS          = #11;   { Output Horz Tabs }
  TELNETOPT_NAOHTD          = #12;   { Output Horz Tab Disp }
  TELNETOPT_NAOFFD          = #13;   { Output FF Disp }
  TELNETOPT_NAOVTS          = #14;   { Output Vert Tabs }
  TELNETOPT_NAOVTD          = #15;   { Output Vert Tab Disp }
  TELNETOPT_NAOLFD          = #16;   { Output Linefeed Disp }
  TELNETOPT_EXTEND_ASCII    = #17;   { Extended ASCII }
  TELNETOPT_LOGOUT          = #18;   { Logout }
  TELNETOPT_BM              = #19;   { Byte Macro }
  TELNETOPT_DET             = #20;   { Data entry terminal subcommands }
  TELNETOPT_SUPDUP          = #21;   { SUPDUP }
  TELNETOPT_SUPDUP_OUTPUT   = #22;   { SUPDUP Output }
  TELNETOPT_SEND_LOCATION   = #23;   { Send Location }
  TELNETOPT_TERM            = #24;   { Terminal Type }
  TELNETOPT_EOR             = #25;   { End of record }
  TELNETOPT_TUID            = #26;   { TACACS User ID }
  TELNETOPT_OUTMRK          = #27;   { Output Marking }
  TELNETOPT_TTYLOC          = #28;   { Terminal Loc Num }
  TELNETOPT_3270_REGIME     = #29;   { Telnet3270 Regime }
  TELNETOPT_X3PAD           = #30;   { X.3-Pad operations }
  TELNETOPT_NAWS            = #31;   { Negotiate about window size }
  TELNETOPT_SPEED           = #32;   { Terminal Speed }
  TELNETOPT_FLOW            = #33;   { Toggle Flow Control }
  TELNETOPT_LINEMODE        = #34;   { Line Mode }
  TELNETOPT_XDISPLOC        = #35;   { X Display Location }
  TELNETOPT_ENVIRON         = #36;   { Environment variables }
  TELNETOPT_AUTH            = #37;   { Authentication }
  TELNETOPT_ENCRYPT{?}      = #38;   { Encryption Option }
  TELNETOPT_NEWENVIRON      = #39;   { New environment variables }
  TELNETOPT_TN3270E         = #40;   { TN3270E }
  TELNETOPT_XAUTH           = #41;   { XAUTH }
  TELNETOPT_CHARSET         = #42;   { Character set }
  TELNETOPT_RSP             = #43;   { Telnet Remote Serial Port (RSP) }
  TELNETOPT_COM_PORT_OPTION = #44;   { Com Port Control Option }
  TELNETOPT_SUP_LOC_ECHO{?} = #45;   { Telnet Suppress Local Echo }
  TELNETOPT_START_TLS{?}    = #46;   { Telnet Start TLS }
  TELNETOPT_KERMIT          = #47;   { KERMIT }
  TELNETOPT_SEND_URL        = #48;   { SEND-URL }
  TELNETOPT_FORWARD_X       = #49;   { FORWARD_X }
  TELNETOPT_LASTOPT         = #49;   { iPRO specific placeholder -- increase as list grows }
  TELNETOPT_EXOPL           = #255;  { Extended options list }

type
  { The new type to be used in all instances which refer to sockets.   }
  { No, I don't *really* want Cardinal, this was originally a DWORD,   }
  { but Delphi 3 has DWORD defined as an *Integer* -- mutter, mutter...}
  TSocket = Cardinal;

  { Record for WSAAsync database lookup methods }
  TCMIpAsyncResult = record
    Msg : Cardinal;
    Handle : TIpHandle;
    AsyncBufLen : Word;
    AsyncError : Word;
    Result : Longint;
  end;

  { Record for socket events }
  TCMIpSockMessage = record
    Msg : Cardinal;
    Socket : TSocket;
    SelectEvent : Word;
    SelectError : Word;
    Result : Longint;
  end;

const
  IpssConnect = 1;
  IpssDisconnect = 2;

type

  { Record for status events (for terminals) }
  TCMIpSocketStatus = record
    Msg : Cardinal;
    Socket : TSocket;
    case Integer of
      0 : (StatusEvent : Word;
           SingleSocket : Word);
      1 : (LParam : Longint;
           Result : LongInt);
  end;

  { Record for terminal data }
  TCMIpTermData = record
    Msg : Cardinal;
    Buffer : Pointer;
    BufSize : DWORD;
    Result : Longint;
  end;

  { Record for read line data }
  PIpLineRec = ^TIpLineRec;
  TIpLineRec = record
    Count : Integer;
    Str : PChar;
  end;

  { Record for read line events }
  TCMIpLineMessage = record
    Msg : Cardinal;
    Socket : TSocket;
    Line : PIpLineRec;
    Result : Longint;
  end;

  { WinSock version stuff }
  TIpVerRec = record
    VerNum : Word;
    SoMaxConn : DWORD;
    ModuleName : array[0..12] of AnsiChar;
  end;

  { Order matters in this enumeration }
  TIpModuleVersion = (wvNone, wvWinSock1, wvWinSock2);

  TIpSockProtocol = (spTcp, spUdp);

  TIpSockOptions = (soBroadcast, soDebug, soDiscardReceived,
    soDontRoute, soEchoReceived, soEchoSent, soKeepAlive,
    soNoDelay, soReuseAddr, soStripHighBit, soTelnet);

  TIpSockOptionSet = set of TIpSockOptions;

var

  { This array is intentionally one-based }
  IpVerArray : array[1..IpSockVers] of TIpVerRec =
    ((VerNum : $0101; SoMaxConn : 5; ModuleName : 'wsock32.dll'),         { WinSock 1 }
     (VerNum : $0202; SoMaxConn : $7FFFFFFF; ModuleName : 'ws2_32.dll')); { WinSock 2 }

type
  TPublicEncryptionType  = Byte;
  TMessageEncryptionType = Byte;
  THashType = Byte;

  TUint8    = Byte;
  TUint16   = WORD;
  TUint24   = array [0..2] of Byte;
  TUint32   = DWORD;

const

  Fd_Setsize = 64;

  { Commands for ioctlsocket(), taken from the BSD file fcntl.h.  }
  { Ioctl's have the command encoded in the lower word, and the   }
  { size of any in or out parameters in the upper word. The high  }
  { 2 bits of the upper word are used to encode the in/out status }
  { of the parameter; for now we restrict parameters to at most   }
  { 128 bytes. }

const

  IocParm_Mask = $7F;                 { parameters must be < 128 bytes }
  Ioc_Void     = $20000000;           { no parameters }
  Ioc_Out      = $40000000;           { copy out parameters }
  Ioc_In       = DWORD($80000000);    { copy in parameters }
  Ioc_InOut    = (Ioc_In or Ioc_Out);

  { WinSock 2 extension -- manifest constants for WSAIoctl() }

  Ioc_Unix     = $00000000;
  Ioc_Ws2      = $08000000;
  Ioc_Protocol = $10000000;
  Ioc_Vendor   = $18000000;

  { Get # bytes to read }
  FIOnRead = Ioc_Out or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('f')) shl 8) or 127;

  { Set/Clear non-blocking I/O }
  FIOnBio  = Ioc_In or((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('f')) shl 8) or 126;

  { Set/Clear async I/O }
  FIOAsync = Ioc_In or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('f')) shl 8) or 125;

  { Socket I/O Controls }

  { Set High Watermark }
  SioCSHiWat = Ioc_In or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('s')) shl 8) or 0;

  { Get High Watermark }
  SioCGHiWat = Ioc_Out or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('s')) shl 8) or 1;

  { Set Low Watermark }
  SioCSLoWat = Ioc_In or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('s')) shl 8) or 2;

  { Get Low Watermark }
  SioCGLoWat = Ioc_Out or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('s')) shl 8) or 3;

  { At OOB Mark? }
  SioCAtMark = Ioc_Out or ((DWORD(SizeOf(DWORD)) and IocParm_Mask) shl 16) or
    (DWORD(Byte('s')) shl 8) or 7;

  { WinSock 2 extension -- manifest constants for WSAIoctl() }

  Sio_Associate_Handle               = (Ioc_In or Ioc_Ws2 or 1);
  Sio_Enable_Circular_Queueing       = (Ioc_Void or Ioc_Ws2 or 2);
  Sio_Find_Route                     = (Ioc_Out or Ioc_Ws2 or 3);
  Sio_Flush                          = (Ioc_Void or Ioc_Ws2 or 4);
  Sio_Get_Broadcast_Address          = (Ioc_Out or Ioc_Ws2 or 5);
  Sio_Get_Extension_Function_Pointer = (Ioc_InOut or Ioc_Ws2 or 6);
  Sio_Get_Qos                        = (Ioc_InOut or Ioc_Ws2 or 7);
  Sio_Get_Group_Qos                  = (Ioc_InOut or Ioc_Ws2 or 8);
  Sio_Multipoint_Loopback            = (Ioc_In or Ioc_Ws2 or 9);
  Sio_Multicast_Scope                = (Ioc_In or Ioc_Ws2 or 10);
  Sio_Set_Qos                        = (Ioc_In or Ioc_Ws2 or 11);
  Sio_Set_Group_Qos                  = (Ioc_In or Ioc_Ws2 or 12);
  Sio_Translate_Handle               = (Ioc_InOut or Ioc_Ws2 or 13);
  Sio_Routing_Interface_Query        = (Ioc_InOut or Ioc_Ws2 or 20);
  Sio_Routing_Interface_Change       = (Ioc_In or Ioc_Ws2 or 21);
  Sio_Address_List_Query             = (Ioc_Out or Ioc_Ws2 or 22);
  Sio_Address_List_Change            = (Ioc_Void or Ioc_Ws2 or 23);
  Sio_Query_Target_Pnp_Handle        = (Ioc_Out or Ioc_Ws2 or 24);

const

  { Constants and structures defined by the Internet system, per   }
  { RFC 790, September 1981, taken from the BSD file netinet/in.h. }

  { Protocols }

  IpProto_Ip    = 0;               { Dummy for IP }
  IpProto_Icmp  = 1;               { Control message protocol }
  IpProto_Igmp  = 2;               { Group management protocol }
  IpProto_Ggp   = 3;               { Gateway^2 (deprecated) }
  IpProto_Tcp   = 6;               { Tcp }
  IpProto_Pup   = 12;              { Pup }
  IpProto_Udp   = 17;              { User datagram protocol }
  IpProto_Idp   = 22;              { Xns idp }
  IpProto_Whois = 43;              { Who Is }
  IpProto_Nd    = 77;              { UNOFFICIAL net disk proto }

  IpProto_Raw  = 255;              { Raw IP packet }
  IpProto_Max  = 256;

const

  { Port/socket numbers: network standard functions }

  IpPort_Echo       = 7;
  IpPort_Discard    = 9;
  IpPort_SyStat     = 11;
  IpPort_Daytime    = 13;
  IpPort_NetStat    = 15;
  IpPort_Quote      = 17;
  IpPort_CharGen    = 19;
  IpPort_Ftp        = 21;
  IpPort_Telnet     = 23;
  IpPort_Smtp       = 25;
  IpPort_TimeServer = 37;
  IpPort_NameServer = 42;
  IpPort_WhoIs      = 43;
  IpPort_Mtp        = 57;
  IpPort_SntpServer = 123;

const

  { Port/socket numbers: host specific functions }

  IpPort_Tftp    = 69;
  IpPort_Rje     = 77;
  IpPort_Finger  = 79;
  IpPort_Http    = 80;
  IpPort_TtyLink = 87;
  IpPort_SupDup  = 95;

const

  { Unix TCP sockets }

  IpPort_ExecServer  = 512;
  IpPort_LoginServer = 513;
  IpPort_CmdServer   = 514;
  IpPort_EfsServer   = 520;

const

  { Unix UDP sockets }

  IpPort_BiffUdp     = 512;
  IpPort_WhoServer   = 513;
  IpPort_RouteServer = 520;    { 520+1 also used }

  { Ports < IpPort_Reserved are reserved for privileged processes (e.g. root). }

  IpPort_Reserved = 1024;

const

  { Link numbers }

  ImpLink_Ip        = 155;
  ImpLink_LowExper  = 156;
  ImpLink_HighExper = 158;

const

  { Definitions of bits in internet address integers. On subnets, }
  { the decomposition of addresses to host and net parts is done  }
  { according to subnet mask, not the masks here.                 }

  In_ClassA_Net    = DWORD($FF000000);
  In_ClassA_NShift = 24;
  In_ClassA_Host   = $00FFFFFF;
  In_ClassA_Max    = 128;

  In_ClassB_Net    = DWORD($FFFF0000);
  In_ClassB_NShift = 16;
  In_ClassB_Host   = $0000FFFF;
  In_ClassB_Max    = 65536;

  In_ClassC_Net    = DWORD($FFFFFF00);
  In_ClassC_NShift = 8;
  In_ClassC_Host   = $000000FF;

  { WinSock 2 }
  In_ClassD_Net    = DWORD($F0000000); { These ones aren't really }
  In_ClassD_NShift = 28;               { net and host fields, but }
  In_ClassD_Host   = $0FFFFFFF;        { routing needn't know.    }

const

  InAddr_Any       = $00000000;
  InAddr_LoopBack  = DWORD($7F000001);
  InAddr_Broadcast = DWORD($FFFFFFFF);
  InAddr_None      = DWORD($FFFFFFFF);

  { WinSock 2 }
  Addr_Any         = InAddr_Any;

  wsaDescription_Len = 256;
  wsaSys_Status_Len  = 128;

const

  { Options for use with (get/set)sockopt at the IP level. }

  Ip_Options         = 1;           { set/get IP per-packet options    }
  Ip_Multicast_If    = 2;           { set/get IP multicast interface   }
  Ip_Multicast_Ttl   = 3;           { set/get IP multicast timetolive  }
  Ip_Multicast_Loop  = 4;           { set/get IP multicast loopback    }
  Ip_Add_Membership  = 5;           { add  an IP group membership      }
  Ip_Drop_Membership = 6;           { drop an IP group membership      }
  Ip_Ttl             = 7;           { set/get IP Time To Live          }
  Ip_Tos             = 8;           { set/get IP Type Of Service       }
  Ip_DontFragment    = 9;           { set/get IP Don't Fragment flag   }

  Ip_Default_Multicast_Ttl  = 1;    { normally limit m'casts to 1 hop  }
  Ip_Default_Multicast_Loop = 1;    { normally hear sends if a member  }
  Ip_Max_Memberships        = 20;   { per socket; must fit in one mbuf }

  { This is used instead of -1, since the TSocket type is unsigned.}

  Invalid_Socket = TSocket(not(0));
  Socket_Error   = -1;

  { The following may be used in place of the address family,      }
  { socket type, or protocol in a call to WSASocket to indicate    }
  { that the corresponding value should be taken from the supplied }
  { WSAPROTOCOL_INFO structure instead of the parameter itself.    }

  { WinSock 2 }
  From_Protocol_Info = -1;

const

  { Types }

  Sock_Stream    = 1;               { stream socket }
  Sock_DGram     = 2;               { datagram socket }
  Sock_Raw       = 3;               { raw-protocol interface }
  Sock_Rdm       = 4;               { reliably-delivered message }
  Sock_SeqPacket = 5;               { sequenced packet stream }

const

  { Option flags per-socket. }

  So_Debug       = $0001;           { turn on debugging info recording }
  So_AcceptConn  = $0002;           { socket has had listen() }
  So_ReuseAddr   = $0004;           { allow local address reuse }
  So_KeepAlive   = $0008;           { keep connections alive }
  So_DontRoute   = $0010;           { just use interface addresses }
  So_Broadcast   = $0020;           { permit sending of broadcast msgs }
  So_UseLoopback = $0040;           { bypass hardware when possible }
  So_Linger      = $0080;           { linger on close if data present }
  So_OobInline   = $0100;           { leave received OOB data in line }

  So_DontLinger  = $FF7F;

const

  { WinSock 2 extension -- manifest constants for return }
  { values of the condition function }

  Cf_Accept      = $0000;
  Cf_Reject      = $0001;
  Cf_Defer       = $0002;

const

  { WinSock 2 extension -- manifest constants for shutdown() }

  Sd_Receive     = $0000;
  Sd_Send        = $0001;
  Sd_Both        = $0002;

const

  { Additional options. }

  So_SndBuf      = $1001;      { send buffer size }
  So_RcvBuf      = $1002;      { receive buffer size }
  So_SndLoWat    = $1003;      { send low-water mark }
  So_RcvLoWat    = $1004;      { receive low-water mark }
  So_SndTimeO    = $1005;      { send timeout }
  So_RcvTimeO    = $1006;      { receive timeout }
  So_Error       = $1007;      { get error status and clear }
  So_Type        = $1008;      { get socket type }

  { WinSock 2 extension -- new options }

  So_Group_Id       = $2001;   { ID of a socket group }
  So_Group_Priority = $2002;   { the relative priority within a group }
  So_Max_Msg_Size   = $2003;   { maximum message size }
  So_Protocol_InfoA = $2004;   { WSAPROTOCOL_INFOA structure }
  So_Protocol_InfoW = $2005;   { WSAPROTOCOL_INFOW structure }

  {$IFDEF IP_USE_UNICODE}
  So_Protocol_Info = So_Protocol_InfoW;
  {$ELSE}
  So_Protocol_Info = So_Protocol_InfoA;
  {$ENDIF}

  Pvd_Config       = $3001;    { configuration info for service provider }

const

  { Options for connect and disconnect data and options.  Used only by }
  { non-TCP/IP transports such as DecNet, Osi TP4, etc. }

  So_ConnData    = $7000;
  So_ConnOpt     = $7001;
  So_DiscData    = $7002;
  So_DiscOpt     = $7003;
  So_ConnDataLen = $7004;
  So_ConnOptLen  = $7005;
  So_DiscDataLen = $7006;
  So_DiscOptLen  = $7007;

  { Option for opening sockets for synchronous access. }

  So_OpenType    = $7008;

  So_Synchronous_Alert    = $10;
  So_Synchronous_NonAlert = $20;

  { Other NT-specific options. }

  So_MaxDG                 = $7009;
  So_MaxPathDG             = $700A;
  So_Update_Accept_Context = $700B;
  So_Connect_Time          = $700C;

const

  { TCP options. }

  Tcp_NoDelay   = $0001;
  Tcp_BsdUrgent = $7000;

const

  { Options for TransmitFile }

  Tf_Disconnect   = $01;
  Tf_Reuse_Socket = $02;
  Tf_Write_Behind = $04;

const

  { Address families. }

  Af_Unspec    = 0;                 { unspecified }
  Af_Unix      = 1;                 { local to host (pipes, portals) }
  Af_Inet      = 2;                 { internetwork: UDP, TCP, etc. }
  Af_ImpLink   = 3;                 { arpanet imp addresses }
  Af_Pup       = 4;                 { pup protocols: e.g. BSP }
  Af_Chaos     = 5;                 { MIT Chaos protocols }
  Af_Ns        = 6;                 { XEROX Ns protocols }
  Af_Ipx       = Af_Ns;             { IXP protocols: IPX, SPX, etc. }
  Af_Iso       = 7;                 { Iso protocols }
  Af_Osi       = Af_Iso;            { Osi is Iso }
  Af_Ecma      = 8;                 { european computer manufacturers }
  Af_DataKit   = 9;                 { datakit protocols }
  Af_Ccitt     = 10;                { Ccitt protocols, X.25 etc }
  Af_Sna       = 11;                { IBM Sna }
  Af_DecNet    = 12;                { DECnet }
  Af_Dli       = 13;                { Direct data link interface }
  Af_Lat       = 14;                { Lat }
  Af_HyLink    = 15;                { NSC Hyperchannel }
  Af_AppleTalk = 16;                { AppleTalk }
  Af_NetBios   = 17;                { NetBios-style addresses }
  Af_VoiceView = 18;                { VoiceView }
  Af_FireFox   = 19;                { Protocols from FireFox }
  Af_Unknown1  = 20;                { Somebody is using this! }
  Af_Ban       = 21;                { Banyan }

  { WinSock 2 }
  Af_Atm       = 22;                { Native ATM Services }
  Af_Inet6     = 23;                { Internetwork Version 6 }
  Af_Cluster   = 24;                { Microsoft Wolfpack }
  Af_12844     = 25;                { IEEE 1284.4 WG AF }

  Af_Max       = 26;

const

  { Protocol families, same as address families for now. }

  Pf_Unspec    = Af_Unspec;
  Pf_Unix      = Af_Unix;
  Pf_Inet      = Af_Inet;
  Pf_ImpLink   = Af_ImpLink;
  Pf_Pup       = Af_Pup;
  Pf_Chaos     = Af_Chaos;
  Pf_Ns        = Af_Ns;
  Pf_Ipx       = Af_Ipx;
  Pf_Iso       = Af_Iso;
  Pf_Osi       = Af_Osi;
  Pf_Ecma      = Af_Ecma;
  Pf_DataKit   = Af_DataKit;
  Pf_Ccitt     = Af_Ccitt;
  Pf_Sna       = Af_Sna;
  Pf_DecNet    = Af_DecNet;
  Pf_Dli       = Af_Dli;
  Pf_Lat       = Af_Lat;
  Pf_HyLink    = Af_HyLink;
  Pf_AppleTalk = Af_AppleTalk;
  Pf_VoiceView = Af_VoiceView;
  Pf_FireFox   = Af_FireFox;
  Pf_Unknown1  = Af_Unknown1;
  Pf_Ban       = Af_Ban;

  { WinSock 2 }
  Pf_Atm       = Af_Atm;
  Pf_Inet6     = Af_Inet6;
  Pf_Cluster   = Af_Cluster;
  Pf_12844     = Af_12844;

  Pf_Max       = Af_Max;

const

  { Level number for (get/set)sockopt() to apply to socket itself. }

  Sol_Socket = $FFFF;             { options for socket level }

const

  Msg_Oob       = $01;            { process out-of-band data }
  Msg_Peek      = $02;            { peek at incoming message }
  Msg_DontRoute = $04;            { send without using routing tables }

  { WinSock 2 extension -- new flags for WSASend(), WSASendTo(), }
  { WSARecv() and WSARecvFrom() }

  Msg_Interrupt = $10;            { send/recv in the interrupt context }

  Msg_MaxIovLen = 16;             { does this conflict with Msg_Interrupt??? }

  Msg_Partial   = $8000;          { partial send or recv for message xport }

  { Define constant based on rfc883, used by gethostbyxxxx() calls. }

  MaxGetHostStruct = 1024;

const

  { Define flags to be used with the wsaAsyncSelect() call. }
  { WinSock 2 extension -- bit values and indices for FD_XXX network events }

  Fd_Read_Bit      = 0;
  Fd_Write_Bit     = 1;
  Fd_Oob_Bit       = 2;
  Fd_Accept_Bit    = 3;
  Fd_Connect_Bit   = 4;
  Fd_Close_Bit     = 5;
  Fd_Qos_Bit       = 6;
  Fd_Group_Qos_Bit = 7;
  Fd_Routing_Interface_Change_Bit = 8;
  Fd_Address_List_Change_Bit = 9;
  Fd_Max_Events    = 10;

  Fd_Read       = ($01 shl Fd_Read_Bit);
  Fd_Write      = ($01 shl Fd_Write_Bit);
  Fd_Oob        = ($01 shl Fd_Oob_Bit);
  Fd_Accept     = ($01 shl Fd_Accept_Bit);
  Fd_Connect    = ($01 shl Fd_Connect_Bit);
  Fd_Close      = ($01 shl Fd_Close_Bit);

  { WinSock 2 }
  Fd_Qos        = ($01 shl Fd_Qos_Bit);
  Fd_Group_Qos  = ($01 shl Fd_Group_Qos_Bit);
  Fd_Routing_Interface_Change = ($01 shl Fd_Routing_Interface_Change_Bit);
  Fd_Address_List_Change = ($01 shl Fd_Address_List_Change_Bit);
  Fd_All_Events = (($01 shl Fd_Max_Events) - 1);

const

  { WinSock 2 }
  Sg_Unconstrained_Group = $01;
  Sg_Constrained_Group   = $02;

const

  { WinSock 2 }
  Max_Protocol_Chain = 7;

  { WinSock 2 }
  Base_Protocol = 1;
  Layered_Protocol = 0;

const

  WsaProtocol_Len = 255;

const

  { WinSock 2 }
  Lup_Deep                = $0001;
  Lup_Containers          = $0002;
  Lup_NoContainers        = $0004;
  Lup_Nearest             = $0008;
  Lup_Return_Name         = $0010;
  Lup_Return_Type         = $0020;
  Lup_Return_Version      = $0040;
  Lup_Return_Comment      = $0080;
  Lup_Return_Addr         = $0100;
  Lup_Return_Blob         = $0200;
  Lup_Return_Aliases      = $0400;
  Lup_Return_Query_String = $0800;
  Lup_Return_All          = $0FF0;
  Lup_Res_Service         = $8000;
  Lup_FlushCache          = $1000;
  Lup_FlushPrevious       = $2000;

const

  { All Windows Sockets error constants are biased by wsaBaseErr from the "normal" }

  wsaBaseErr = 10000;

const

  { Windows Sockets definitions of regular Microsoft C error constants }

  wsaEIntr  = (wsaBaseErr + 4);
  wsaEBadF  = (wsaBaseErr + 9);
  wsaEAcces = (wsaBaseErr + 13);
  wsaEFault = (wsaBaseErr + 14);
  wsaEInval = (wsaBaseErr + 22);
  wsaEMFile = (wsaBaseErr + 24);

const

  { Windows Sockets definitions of regular Berkeley error constants }

  wsaEWouldBlock     = (wsaBaseErr + 35);
  wsaEInProgress     = (wsaBaseErr + 36);
  wsaEAlReady        = (wsaBaseErr + 37);
  wsaENotSock        = (wsaBaseErr + 38);
  wsaEDestAddrReq    = (wsaBaseErr + 39);
  wsaEMsgSize        = (wsaBaseErr + 40);
  wsaEPrototype      = (wsaBaseErr + 41);
  wsaENoProtoOpt     = (wsaBaseErr + 42);
  wsaEProtoNoSupport = (wsaBaseErr + 43);
  wsaESocktNoSupport = (wsaBaseErr + 44);
  wsaEOpNotSupp      = (wsaBaseErr + 45);
  wsaEPfNoSupport    = (wsaBaseErr + 46);
  wsaEAfNoSupport    = (wsaBaseErr + 47);
  wsaEAddrInUse      = (wsaBaseErr + 48);
  wsaEAddrNotAvail   = (wsaBaseErr + 49);

  wsaENetDown        = (wsaBaseErr + 50);
  wsaENetUnreach     = (wsaBaseErr + 51);
  wsaENetReset       = (wsaBaseErr + 52);
  wsaEConnAborted    = (wsaBaseErr + 53);
  wsaEConnReset      = (wsaBaseErr + 54);
  wsaENoBufs         = (wsaBaseErr + 55);
  wsaEIsConn         = (wsaBaseErr + 56);
  wsaENotConn        = (wsaBaseErr + 57);
  wsaEShutDown       = (wsaBaseErr + 58);
  wsaETooManyRefs    = (wsaBaseErr + 59);
  wsaETimedOut       = (wsaBaseErr + 60);
  wsaEConnRefused    = (wsaBaseErr + 61);
  wsaELoop           = (wsaBaseErr + 62);
  wsaENameTooLong    = (wsaBaseErr + 63);
  wsaEHostDown       = (wsaBaseErr + 64);
  wsaEHostUnreach    = (wsaBaseErr + 65);
  wsaENotEmpty       = (wsaBaseErr + 66);
  wsaEProcLim        = (wsaBaseErr + 67);
  wsaEUsers          = (wsaBaseErr + 68);
  wsaEDQuot          = (wsaBaseErr + 69);
  wsaEStale          = (wsaBaseErr + 70);
  wsaERemote         = (wsaBaseErr + 71);

  { Extended Windows Sockets error constant definitions }

  wsaSysNotReady         = (wsaBaseErr + 91);
  wsaVerNotSupported     = (wsaBaseErr + 92);
  wsaNotInitialised      = (wsaBaseErr + 93);
  wsaEDiscOn             = (wsaBaseErr + 101);

  { WinSock 2}
  wsaENoMore             = (wsaBaseErr + 102);
  wsaECancelled          = (wsaBaseErr + 103);
  wsaEInvalidProcTable   = (wsaBaseErr + 104);
  wsaEInvalidProvider    = (wsaBaseErr + 105);
  wsaEProviderFailedInit = (wsaBaseErr + 106);
  wsaSysCallFailure      = (wsaBaseErr + 107);
  wsaService_Not_Found   = (wsaBaseErr + 108);
  wsaType_Not_Found      = (wsaBaseErr + 109);
  wsa_E_No_More          = (wsaBaseErr + 110);
  wsa_E_Cancelled        = (wsaBaseErr + 111);
  wsaERefused            = (wsaBaseErr + 112);

const

  { WinSock 2 extension -- new error codes and type definition }

  wsa_Io_Pending          = (ERROR_IO_PENDING);
  wsa_Io_Incomplete       = (ERROR_IO_INCOMPLETE);
  wsa_Invalid_Handle      = (ERROR_INVALID_HANDLE);
  wsa_Invalid_Parameter   = (ERROR_INVALID_PARAMETER);
  wsa_Not_Enough_Memory   = (ERROR_NOT_ENOUGH_MEMORY);
  wsa_Operation_Aborted   = (ERROR_OPERATION_ABORTED);

type

  TWsaEvent = TIpHandle;

const

  { WinSock 2 }
  wsa_Invalid_Event       = TWsaEvent(nil);
  wsa_Maximum_Wait_Events = (MAXIMUM_WAIT_OBJECTS);
  wsa_Wait_Failed         = DWORD(-1);
  wsa_Wait_Event_0        = (WAIT_OBJECT_0);
  wsa_Wait_Io_Completion  = (WAIT_IO_COMPLETION);
  wsa_Wait_Timeout        = (WAIT_TIMEOUT);
  wsa_Infinite            = (INFINITE);

  { Error return codes from gethostbyname() and gethostbyaddr()       }
  { (when using the resolver). Note that these errors are retrieved   }
  { via wsaGetLastError() and must therefore follow the rules for     }
  { avoiding clashes with error numbers from specific implementations }
  { or language run-time systems. For this reason the codes are based }
  { at wsaBaseErr+1001. Note also that [wsa]No_Address is defined     }
  { only for compatibility purposes. }

const

  { Authoritative Answer: Host not found }
  wsaHost_Not_Found = (wsaBaseErr + 1001);
  Host_Not_Found    = wsaHost_Not_Found;

  { Non-Authoritative: Host not found, or ServerFail }
  wsaTry_Again = (wsaBaseErr + 1002);
  Try_Again    = wsaTry_Again;

  { Non recoverable errors, FORMERR, REFUSED, NotIMP }
  wsaNo_Recovery = (wsaBaseErr + 1003);
  No_Recovery    = wsaNo_Recovery;

  { Valid name, no data record of requested type }
  wsaNo_Data = (wsaBaseErr + 1004);
  No_Data    = wsaNo_Data;

  { no address, look for MX record }
  wsaNo_Address = wsaNo_Data;
  No_Address    = wsaNo_Address;

const

  { Define QOS related error return codes - WinSock 2 }

  { at least one Reserve has arrived }
  wsa_Qos_Receivers = (wsaBaseErr + 1005);

  { at least one Path has arrived }
  wsa_Qos_Senders   = (wsaBaseErr + 1006);

  { there are no senders }
  wsa_Qos_No_Senders = (wsaBaseErr + 1007);

  { there are no receivers }
  wsa_Qos_No_Receivers = (wsaBaseErr + 1008);

  { Reserve has been confirmed }
  wsa_Qos_Request_Confirmed = (wsaBaseErr + 1009);

  { error due to lack of resources }
  wsa_Qos_Admission_Failure = (wsaBaseErr + 1010);

  { rejected for administrative reasons - bad credentials }
  wsa_Qos_Policy_Failure = (wsaBaseErr + 1011);

  { unknown or conflicting style }
  wsa_Qos_Bad_Style = (wsaBaseErr + 1012);

  { problem with filterspec or providerspecific buffer }
  wsa_Qos_Bad_Object = (wsaBaseErr + 1013);

  { problem with some part of the flowspec }
  wsa_Qos_Traffic_Ctrl_Error = (wsaBaseErr + 1014);

  { general error }
  wsa_Qos_Generic_Error = (wsaBaseErr + 1015);

const

  { Windows Sockets errors redefined as regular Berkeley error constants. }
  { These are commented out in Windows NT to avoid conflicts with errno.h. }
  { Use the wsa constants instead. }

  EWouldBlock     = wsaEWouldBlock;
  EInProgress     = wsaEInProgress;
  EAlready        = wsaEAlready;
  ENotSock        = wsaENotSock;
  EDestAddrReq    = wsaEDestAddrReq;
  EMsgSize        = wsaEMsgSize;
  EPrototype      = wsaEPrototype;
  ENoProtoOpt     = wsaENoProtoOpt;
  EProtoNoSupport = wsaEProtoNoSupport;
  ESocktNoSupport = wsaESocktNoSupport;
  EOpNotSupp      = wsaEOpNotSupp;
  EPfNoSupport    = wsaEPfNoSupport;
  EAfNoSupport    = wsaEAfNoSupport;
  EAddrInUse      = wsaEAddrInUse;
  EAddrNotAvail   = wsaEAddrNotAvail;
  ENetDown        = wsaENetDown;
  ENetUnreach     = wsaENetUnreach;
  ENetReset       = wsaENetReset;
  EConnAborted    = wsaEConnAborted;
  EConnReset      = wsaEConnReset;
  ENoBufs         = wsaENoBufs;
  EIsConn         = wsaEIsConn;
  ENotConn        = wsaENotConn;
  EShutDown       = wsaEShutDown;
  ETooManyRefs    = wsaETooManyRefs;
  ETimedOut       = wsaETimedOut;
  EConnRefused    = wsaEConnRefused;
  ELoop           = wsaELoop;
  ENameTooLong    = wsaENameTooLong;
  EHostDown       = wsaEHostDown;
  EHostUnreach    = wsaEHostUnreach;
  ENotEmpty       = wsaENotEmpty;
  EProcLim        = wsaEProcLim;
  EUsers          = wsaEUsers;
  EDQuot          = wsaEDQuot;
  EStale          = wsaEStale;
  ERemote         = wsaERemote;

const

  { Flag bit definitions for dwProviderFlags }

  { WinSock 2 }
  Pfl_Multiple_Proto_Entries   = $00000001;
  Pfl_Recommended_Proto_Entry  = $00000002;
  Pfl_Hidden                   = $00000004;
  Pfl_Matches_Protocol_Zero    = $00000008;

const

  { Flag bit definitions for dwServiceFlags1 }

  { WinSock 2 }
  Xp1_Connectionless           = $00000001;
  Xp1_Guaranteed_Delivery      = $00000002;
  Xp1_Guaranteed_Order         = $00000004;
  Xp1_Message_Oriented         = $00000008;
  Xp1_Pseudo_Stream            = $00000010;
  Xp1_Graceful_Close           = $00000020;
  Xp1_Expedited_Data           = $00000040;
  Xp1_Connect_Data             = $00000080;
  Xp1_Disconnect_Data          = $00000100;
  Xp1_Support_Broadcast        = $00000200;
  Xp1_Support_Multipoint       = $00000400;
  Xp1_Multipoint_Control_Plane = $00000800;
  Xp1_Multipoint_Data_Plane    = $00001000;
  Xp1_Qos_Supported            = $00002000;
  Xp1_Interrupt                = $00004000;
  Xp1_Uni_Send                 = $00008000;
  Xp1_Uni_Recv                 = $00010000;
  Xp1_Ifs_Handles              = $00020000;
  Xp1_Partial_Message          = $00040000;

  BigEndian                    = $0000;
  LittleEndian                 = $0001;

  Security_Protocol_None       = $0000;

const

  { WinSock 2 extension -- manifest constants for WSAJoinLeaf() }

  Jl_Sender_Only   = $01;
  Jl_Receiver_Only = $02;
  Jl_Both          = $04;

const

  { WinSock 2 extension -- manifest constants for WSASocket() }

  wsa_Flag_Overlapped        = $01;
  wsa_Flag_Multipoint_C_Root = $02;
  wsa_Flag_Multipoint_C_Leaf = $04;
  wsa_Flag_Multipoint_D_Root = $08;
  wsa_Flag_Multipoint_D_Leaf = $10;

const

  { WinSock 2 extension -- manifest constants for SIO_TRANSLATE_HANDLE ioctl }

  Th_NetDev = $00000001;
  Th_Tapi   = $00000002;

const

  { Service Install Flags }

  Service_Multiple = $00000001;

const

  { Return flags }

  Result_Is_Alias = $0001;

const

  { Name Spaces }

  Ns_All          = 0;

  Ns_Sap          = 1;
  Ns_Nds          = 2;
  Ns_Peer_Browse  = 3;

  Ns_Tcpip_Local  = 10;
  Ns_Tcpip_Hosts  = 11;
  Ns_Dns          = 12;
  Ns_NetBT        = 13;
  Ns_Wins         = 14;

  Ns_Nbp          = 20;

  Ns_Ms           = 30;
  Ns_Stda         = 31;
  Ns_Ntds         = 32;

  Ns_X500         = 40;
  Ns_Nis          = 41;
  Ns_NisPlus      = 42;

  Ns_Wrq          = 50;

const

  { Resolution flags for WSAGetAddressByName(). Note     }
  { these are also used by the 1.1 API GetAddressByName, }
  { so leave them around.                                }

  { WinSock 2 }
  Res_Unused_1    = $00000001;
  Res_Flush_Cache = $00000002;
  Res_Service     = $00000004;

const

  { Well known value names for Service Types }
  { These are the ANSI versions of these constants }

  { WinSock 2 }
  Service_Type_Value_IpxPort  = 'IpxSocket';
  Service_Type_Value_SapId    = 'SapId';
  Service_Type_Value_TcpPort  = 'TcpPort';
  Service_Type_Value_UdpPort  = 'UdpPort';
  Service_Type_Value_ObjectId = 'ObjectId';

const

  { Definitions for valued-based Service Type }
  { for each direction of data flow.          }

  ServiceType_NoTraffic           = $00000000;  { No data in this direction }
  ServiceType_BestEffort          = $00000001;  { Best Effort }
  ServiceType_ControlledLoad      = $00000002;  { Controlled Load }
  ServiceType_Guaranteed          = $00000003;  { Guaranteed }
  ServiceType_Network_Unavailable = $00000004;  { Used to notify change to user }
  ServiceType_General_Information = $00000005;  { "General Parameters" defined by IntServ }
  ServiceType_NoChange            = $00000006;  { no change from any previous one }
  ServiceType_NonConforming       = $00000009;  { Non-Conforming Traffic }
  ServiceType_Custom1             = $0000000A;  { Custom ServiceType 1 }
  ServiceType_Custom2             = $0000000B;  { Custom ServiceType 2 }
  ServiceType_Custom3             = $0000000C;  { Custom ServiceType 3 }
  ServiceType_Custom4             = $0000000D;  { Custom ServiceType 4 }

const

  { Definitions for bitmap-based Service Type }
  { for each direction of data flow.          }

  Service_BestEffort      = DWORD($80020000);
  Service_ControlledLoad  = DWORD($80040000);
  Service_Guaranteed      = DWORD($80080000);
  Service_Custom1         = DWORD($80100000);
  Service_Custom2         = DWORD($80200000);
  Service_Custom3         = DWORD($80400000);
  Service_Custom4         = DWORD($80800000);

const

  { Number of available Service Types. }

  Num_ServiceTypes = 8;

const

  { to turn on immediate traffic control, OR ( | ) this flag with the }
  { ServiceType field in the FLOWSPEC }

  Service_No_Traffic_Control = DWORD($81000000);

  { this flag can be used with the immediate traffic control   }
  { flag above to prevent any rsvp signaling messages from     }
  { being sent. Local traffic control will be invoked, but no  }
  { RSVP Path messages will be sent.This flag can also be used }
  { in conjunction with a receiving flowspec to suppress the   }
  { automatic generation of a Reserve message. The application }
  { would receive notification that a Path message had arrived }
  { and would then need to alter the QOS by issuing            }
  { WSAIoctl( SIO_SET_QOS ), to unset this flag and thereby    }
  { cause Reserve messages to go out. }

  Service_No_Qos_Signaling = $40000000;
  Status_Qos_Released      = $10101010;    { rsvp status code }

  { This value can be used in the FLOWSPEC structure to instruct   }
  { the Rsvp Service provider to derive the appropriate default    }
  { value for the parameter. Note that not all values in the       }
  { FLOWSPEC structure can be defaults. In the ReceivingFlowspec,  }
  { all parameters can be defaulted except the ServiceType. In the }
  { SendingFlowspec, the MaxSduSize and MinimumPolicedSize can be  }
  { defaulted. Other defaults may be possible. Refer to the        }
  { appropriate documentation. }

  Qos_Not_Specified = DWORD($FFFFFFFF);
  Null_Qos_Type     = DWORD($FFFFFFFD);

  { Define a value that can be used for the PeakBandwidth, which    }
  { will map into positive infinity when the FLOWSPEC is converted  }
  { into IntServ floating point format. We can't use (-1) because   }
  { that value was previously defined to mean "select the default". }

  Positive_Infinity_Rate = DWORD($FFFFFFFE);

const

  { General QOS objects start at this offset from the base and have }
  { a range of 1000 }

  Qos_General_Id_Base               = 2000;

  Qos_Object_Priority               = (Qos_General_Id_Base + 0);
  Qos_Object_End_Of_List            = (Qos_General_Id_Base + 1);
  Qos_Object_Sd_Mode                = (Qos_General_Id_Base + 2);
  Qos_Object_Traffic_Class          = (Qos_General_Id_Base + 3);
  Qos_Object_DestAddr               = (Qos_General_Id_Base + 4);
  Qos_Object_Shaper_Queue_Drop_Mode = (Qos_General_Id_Base + 5);
  Qos_Object_Shaper_Queue_Limit     = (Qos_General_Id_Base + 6);

const

  Tc_NonConf_Borrow      = 0;
  Tc_NonConf_Shape       = 1;
  Tc_NonConf_Discard     = 2;
  Tc_NonConf_Borrow_Plus = 3;

const

  Qos_Shaper_Drop_Incoming  = 0;
  Qos_Shaper_Drop_From_Head = 1;

type
  TWsaOverlapped = TOverlapped;

  PFDSet = ^TFDSet;
  TFDSet = packed record
    fd_count : Integer;
    fd_array : array[0..Pred(Fd_Setsize)] of TSocket;
  end;

  { Structure used in select() call, taken from the BSD file sys/time.h }

  PTimeVal = ^TTimeVal;
  TTimeVal = packed record
    tv_sec : DWORD;
    tv_usec : DWORD;
  end;

  { Structures returned by network data base library, taken from the }
  { BSD file netdb.h.  All addresses are supplied in host order, and }
  { returned in network order (suitable for use in system calls).    }

  SunB = packed record
    s_b1, s_b2, s_b3, s_b4 : Byte;
  end;

  SunW = packed record
    s_w1, s_w2 : Word;
  end;

  PInAddr = ^TInAddr;
  TInAddr = packed record
    case Integer of
      0 : (S_un_b : SunB);
      1 : (S_un_w : SunW);
      2 : (S_addr : LongInt);
  end;

  { Not in WinSock header -- created to ease access to lists }

const
  MaxAddrs = 256;
  MaxAlias = 256;

type
  PAddrArray = ^TAddrArray;
  TAddrArray = array[0..MaxAddrs-1] of PInAddr;

  PAliasArray = ^TAliasArray;
  TAliasArray = array[0..MaxAlias-1] of PAnsiChar;

  { Host Entity }

  PHostEnt = ^THostEnt;
  THostEnt = packed record
    h_name : PAnsiChar;                 { Official name of host }
    h_aliases : PAliasArray;            { Alias list }
    h_addrtype : SmallInt;              { Host address type }
    h_length : SmallInt;                { Length of address }
    h_addr_list : PAddrArray;           { List of addresses }
  end;

  { Network Entity }
  { It is assumed here that a network number fits in 32 bits. }

  PNetEnt = ^TNetEnt;
  TNetEnt = packed record
    n_name : PAnsiChar;                 { Official name of net }
    n_aliases : PAliasArray;            { Alias list }
    n_addrtype : SmallInt;              { Net address type }
    n_net : DWORD;                      { Network # }
  end;

  { Service Entity }

  PServEnt = ^TServEnt;
  TServEnt = packed record
    s_name : PAnsiChar;                 { Official service name }
    s_aliases : PAliasArray;            { Alias list }
    s_port : SmallInt;                  { Port # }
    s_proto : PAnsiChar;                { Protocol to use }
  end;

  { Protocol Entity }

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = packed record
    p_name : PAnsiChar;                 { Official protocol name }
    p_aliases : PAliasArray;            { Alias list }
    p_proto : SmallInt;                 { Protocol # }
  end;

  { s_addr = TInAddr.S_addr }         { can be used for most tcp & ip code }
  { s_host = TInAddr.S_un_b.s_b2 }    { host on imp }
  { s_net = TInAddr.S_un_b.s_b1 }     { network }
  { s_imp = TInAddr.S_un_w.s_w2 }     { imp }
  { s_impno = TInAddr.S_un_b.s_b4 }   { imp # }
  { s_lh = TInAddr.S_un_b.s_b3 }      { logical host }

  { Socket address, Internet style }

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = packed record
    case Integer of
      0 : (sin_family : Word;
           sin_port : Word;
           sin_addr : TInAddr;
           sin_zero : array[0..7] of AnsiChar);
      1 : (sa_family : Word;
           sa_data : array[0..13] of AnsiChar)
  end;

  PwsaData = ^TwsaData;
  TwsaData = packed record
    wVersion : Word;
    wHighVersion : Word;
    szDescription : array[0..wsaDescription_Len] of AnsiChar;
    szSystemStatus : array[0..wsaSys_Status_Len] of AnsiChar;
    iMaxSockets : Word;
    iMaxUdpDg : Word;
    lpVendorInfo : PAnsiChar;
  end;

  { Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP }

  TIpMreq = packed record
    ImrMultiAddr : TInAddr;   { IP multicast address of group }
    ImrInterface : TInAddr;   { Local IP address of interface }
  end;

  PTransmitFileBuffers = ^TTransmitFileBuffers;
  TTransmitFileBuffers = packed record
    Head : Pointer;
    HeadLength : DWORD;
    Tail : Pointer;
    TailLength : DWORD;
  end;

  { Structure used by kernel to store most addresses. }

  PSockAddr = ^TSockAddr;
  TSockAddr = TSockAddrIn;

  { Structure used by kernel to pass protocol information in raw sockets. }

  PSockProto = ^TSockProto;
  TSockProto = packed record
    sp_family : Word;
    sp_protocol : Word;
  end;

  { Structure used for manipulating linger option. }

  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff : Word;
    l_linger : Word;
  end;

  { WinSock data record }

  PWsDataRec = ^TWsDataRec;
  TWsDataRec = record
    WsSockAddr : TSockAddrIn;
    WsHostAddr : TSockAddrIn;
    WsIsClient : Boolean;
    WsIsTelnet : Boolean;
  end;

  PFlowSpec = ^TFlowSpec;
  TFlowSpec = record
    TokenRate : DWORD;                { In Bytes/sec }
    TokenBucketSize : DWORD;          { In Bytes }
    PeakBandwidth : DWORD;            { In Bytes/sec }
    Latency : DWORD;                  { In microseconds }
    DelayVariation : DWORD;           { In microseconds }
    ServiceType : DWORD;
    MaxSduSize : DWORD;               { In Bytes }
    MinimumPolicedSize : DWORD;       { In Bytes }
  end;

  { The provider specific structure can have a number of objects }
  { in it. Each next structure in the ProviderSpecific will be   }
  { the QOS_OBJECT_HDR struct that prefaces the actual data with }
  { a type and length for that object. This QOS_OBJECT struct    }
  { can repeat several times if there are several objects. This  }
  { list of objects terminates either when the buffer length has }
  { been reached ( WSABUF ) or an object of type QOS_END_OF_LIST }
  { is encountered. }

  PQosObjectHdr = ^TQosObjectHdr;
  TQosObjectHdr = record
    ObjectType : DWORD;
    ObjectLength : DWORD;  { Length of object buffer INCL this header }
  end;

  { This structure defines the absolute priorty of the flow. Priorities }
  { in the range of 0-7 are currently defined. Receive Priority is not  }
  { currently used, but may at some point in the future. }

  PQosPriority = ^TQosPriority;
  TQosPriority = record
    ObjectHdr : TQosObjectHdr;
    SendPriority : Byte;       { This gets mapped to layer 2 priority }
    SendFlags : Byte;          { There are none currently defined     }
    ReceivePriority : Byte;    { Could be used to decide who gets forwarded }
                               { up the stack first - not currently used    }
    Unused : Byte;
  end;

  { This structure is used to define the behaviour that the }
  { traffic control packet shaper will apply to the flow.   }

  { PS_NONCONF_BORROW - the flow will receive resources remaining  }
  { after all higher priority flows have been serviced. If a       }
  { TokenRate is specified, packets may be non-conforming and      }
  { will be demoted to less than best-effort priority.             }

  { PS_NONCONF_SHAPE - TokenRate must be specified. Non-conforming   }
  { packets will be retianed in the packet shaper until they become  }
  { conforming. }

  { PS_NONCONF_DISCARD - TokenRate must be specified. Non-conforming }
  { packets will be discarded. }

  PQosSdMode = ^TQosSdMode;
  TQosSdMode = record
    ObjectHdr : TQosObjectHdr;
    ShapeDiscardMode : DWORD;
  end;

  { This structure may carry an 802.1 TrafficClass parameter which has  }
  { been provided to the host by a layer 2 network, for example, in an  }
  { 802.1 extended RSVP RESV message. If this object is obtained from   }
  { the network, hosts will stamp the MAC headers of corresponding      }
  { transmitted packets, with the value in the object. Otherwise, hosts }
  { may select a value based on the standard Intserv mapping of         }
  { ServiceType to 802.1 TrafficClass. }

  PQosTrafficClass = ^TQosTrafficClass;
  TQosTrafficClass = record
    ObjectHdr : TQosObjectHdr;
    TrafficClass : DWORD;
  end;

  { This structure allows overriding of the default schema used to drop }
  { packets when a flow's shaper queue limit is reached.                }
  {   DropMethod -                                                      }
  {    QOS_SHAPER_DROP_FROM_HEAD - Drop packets from                    }
  {       the head of the queue until the new packet can be             }
  {       accepted into the shaper under the current limit.  This       }
  {       behavior is the default.                                      }
  {    QOS_SHAPER_DROP_INCOMING - Drop the incoming,                    }
  {       limit-offending packet.                                       }

  PQosShaperQueueLimitDropMode = ^TQosShaperQueueLimitDropMode;
  TQosShaperQueueLimitDropMode = record
    ObjectHdr : TQosObjectHdr;
    DropMode : DWORD;
  end;

  { This structure allows the default per-flow limit on the shaper }
  { queue size to be overridden. }
  { QueueSizeLimit - Limit, in bytes, of the size of the shaper queue }

  PQosShaperQueueLimit = ^TQosShaperQueueLimit;
  TQosShaperQueueLimit = record
    ObjectHdr : TQosObjectHdr;
    QueueSizeLimit : DWORD;
  end;

  { WinSock 2 extension -- WSABUF and QOS struct }

  PWsaBuf = ^TWsaBuf;
  TWsaBuf = record
    Len : DWORD;       { The length of the buffer }
    Buf : PAnsiChar;   { The pointer to the buffer }
  end;

  PQos = ^TQos;
  TQos = record
    SendingFlowspec : TFlowSpec;    { The flow spec for data sending }
    ReceivingFlowspec : TFlowSpec;  { The flow spec for data receiving }
    ProviderSpecific : TWsaBuf;     { Additional provider specific stuff }
  end;

  { WinSock 2 extension -- data type for WSAEnumNetworkEvents() }

  PWsaNetworkEvents = ^TWsaNetworkEvents;
  TWsaNetworkEvents = record
    NetworkEvents : LongInt;
    ErrorCode : array [0..Fd_Max_Events-1] of Integer;
  end;

  { WinSock 2 extension -- WSAPROTOCOL_INFO structure and associated }
  { manifest constants }

  PWsaProtocolChain = ^TWsaProtocolChain;
  TWsaProtocolChain = record
    ChainLen : Integer;    { The length of the chain, }
                           {   length = 0 means layered protocol, }
                           {   length = 1 means base protocol,    }
                           {   length > 1 means protocol chain    }
    { A list of dwCatalogEntryIds }
    ChainEntries : array[0..Max_Protocol_Chain-1] of DWORD;
  end;

  PWsaProtocolInfo = ^TWsaProtocolInfo;
  TWsaProtocolInfo = record
    ServiceFlags1 : DWORD;
    ServiceFlags2 : DWORD;
    ServiceFlags3 : DWORD;
    ServiceFlags4 : DWORD;
    ProviderFlags : DWORD;
    ProviderId : TGuid;
    CatalogEntryId : DWORD;
    ProtocolChain : TWsaProtocolChain;
    Version : Integer;
    AddressFamily : Integer;
    MaxSockAddr : Integer;
    MinSockAddr : Integer;
    SocketType : Integer;
    Protocol : Integer;
    ProtocolMaxOffset : Integer;
    NetworkByteOrder : Integer;
    SecurityScheme : Integer;
    MessageSize : DWORD;
    ProviderReserved : DWORD;
    Protocols : array[0..WsaProtocol_Len] of AnsiChar;
  end;

  { SockAddr Information }

  PSocketAddress = ^TSocketAddress;
  TSocketAddress = record
    Sockaddr : PSockAddr;
    SockaddrLength : Integer;
  end;

  { CSAddr Information }

  PCsAddrInfo = ^TCsAddrInfo;
  TCsAddrInfo = record
    LocalAddr : TSocketAddress;
    RemoteAddr : TSocketAddress;
    SocketType : Integer;
    Protocol : Integer;
  end;

  { Address list returned via SIO_ADDRESS_LIST_QUERY }

  PSocketAddressList = ^TSocketAddressList;
  TSocketAddressList = record
    AddressCount : Integer;
    Address : TSocketAddress;
  end;

  { Address Family/Protocol Tuples }

  PAfProtocols = ^TAfProtocols;
  TAfProtocols = record
    AddressFamily : Integer;
    Protocol : Integer;
  end;

  { Client Query API Typedefs }

  { The comparators }

  TWsaEComparator = (CompEqual, CompNotLess);

  PWsaVersion = ^TWsaVersion;
  TWsaVersion = record
    Version : DWORD;
    How : TWsaEComparator;
  end;

  PWsaQuerySet = ^TWsaQuerySet;
  TWsaQuerySet = record
    Size : DWORD;
    ServiceInstanceName : PAnsiChar;
    ServiceClassId : PGuid;
    Version : PWsaVersion;
    Comment : PAnsiChar;
    NameSpace : DWORD;
    NSProviderId : PGuid;
    Context : PAnsiChar;
    NumberOfProtocols : DWORD;
    Protocols : PAfProtocols;
    QueryString : PAnsiChar;
    NumberOfCsAddrs : DWORD;
    Buffer : PCsAddrInfo;
    OutputFlags : DWORD;
    LPBLOB : Pointer;
  end;

  { Service Address Registration and Deregistration Data Types }

  TWsaSetServiceOp = (RnrServiceRegister, RnrServiceDeregister, RnrServiceDelete);

  { Service Installation/Removal Data Types }

  PWsaNsClassInfo = ^TWsaNsClassInfo;
  TWsaNsClassInfo = record
    Name : PAnsiChar;
    NameSpace : DWORD;
    ValueType : DWORD;
    ValueSize : DWORD;
    Value : Pointer;
  end;

  PWsaServiceClassInfo = ^TWsaServiceClassInfo;
  TWsaServiceClassInfo = record
    ServiceClassId : PGuid;
    ServiceClassName : PAnsiChar;
    Count : DWORD;
    ClassInfos : PWsaNsClassInfo;
  end;

  PWsaNameSpaceInfo = ^TWsaNameSpaceInfo;
  TWsaNameSpaceInfo = record
    NSProviderId : TGuid;
    NameSpace : DWORD;
    Active : Bool;
    Version : DWORD;
    Identifier : PAnsiChar;
  end;

  { Microsoft Windows Extension function prototypes }

  { WinSock 2 extensions -- data types for the condition function in }
  { WSAAccept() and overlapped I/O completion routine }

  TIpConditionProc = function(var CallerId, CallerData : TWsaBuf;
    var SQos, GQos : TQos; var CalleeId, CalleeData : TWsaBuf;
    var Group : Cardinal; CallbackData : DWORD) : Integer; stdcall;

  TIpWsaOverlappedCompletionRoutine = procedure(Error, Transferred : DWORD;
    Overlapped : TWsaOverlapped; Flags : DWORD); stdcall;

  { Socket function prototypes }

  TIpAccept = function(S : TSocket; var Addr : TSockAddr;
    var Addrlen : Integer) : TSocket; stdcall;

  TIpBind = function(S : TSocket; var Name : TSockAddr;
    NameLen : Integer) : Integer; stdcall;

  TIpCloseSocket = function(S : TSocket) : Integer; stdcall;

  TIpConnect = function(S : TSocket; var Name : TSockAddr;
    NameLen : Integer) : Integer; stdcall;

  TIpIOCtlSocket = function(S : TSocket; Cmd : LongInt;
    var Arg : LongInt) : Integer; stdcall;

  TIpGetPeerName = function(S : TSocket; var Name : TSockAddr;
    var NameLen : Integer) : Integer; stdcall;

  TIpGetSockName = function(S : TSocket; var Name : TSockAddr;
    var NameLen : Integer) : Integer; stdcall;

  TIpGetSockOpt = function(S : TSocket; Level, OptName : Integer;
    var OptVal; var OptLen : Integer) : Integer; stdcall;

  TIphtonl = function(HostLong : DWORD) : DWORD; stdcall;

  TIphtons = function(HostShort : Word) : Word; stdcall;

  TIpINet_Addr = function(Cp : PAnsiChar) : LongInt; stdcall;

  TIpINet_NtoA = function(InAddr : TInAddr) : PAnsiChar; stdcall;

  TIpListen = function(S : TSocket; Backlog : Integer) : Integer; stdcall;

  TIpntohl = function(NetLong : DWORD) : DWORD; stdcall;

  TIpntohs = function(NetShort : Word) : Word; stdcall;

  TIpRecv = function(S : TSocket; var Buf;
    Len, Flags : Integer) : Integer; stdcall;

  TIpRecvFrom = function(S : TSocket; var Buf; Len, Flags : Integer;
    var From : TSockAddr; var FromLen : Integer) : Integer; stdcall;

  TIpSelect = function(Nfds : Integer; Readfds, Writefds,
    Exceptfds : PFDSet; Timeout : PTimeVal) : LongInt; stdcall;

  TIpSend = function(S : TSocket; var Buf;
    Len, Flags : Integer) : Integer; stdcall;

  TIpSendTo = function(S : TSocket; var Buf; Len, Flags : Integer;
    var AddrTo : TSockAddr; ToLen : Integer) : Integer; stdcall;

  TIpSetSockOpt = function(S : TSocket; Level, OptName : Integer;
    var OptVal; OptLen : Integer) : Integer; stdcall;

  TIpShutdown = function(S : TSocket; How : Integer) : Integer; stdcall;

  TIpSocket = function(Af, Struct, Protocol : Integer) : TSocket; stdcall;

  { Database function prototypes }

  TIpGetHostByAddr = function(var Addr; Len, Struct : Integer) : PHostEnt; stdcall;

  TIpGetHostByName = function(Name : PAnsiChar) : PHostEnt; stdcall;

  TIpGetHostName = function(Name : PAnsiChar; Len : Integer) : Integer; stdcall;

  TIpGetServByPort = function(Port : Integer; Proto : PAnsiChar) : PServEnt; stdcall;

  TIpGetServByName = function(Name, Proto : PAnsiChar) : PServEnt; stdcall;

  TIpGetProtoByNumber = function(Proto : Integer) : PProtoEnt; stdcall;

  TIpGetProtoByName = function(Name : PAnsiChar) : PProtoEnt; stdcall;

  { Microsoft Windows Extension function prototypes }

  TIpWsaStartup = function(VersionRequested : Word;
    var WSData : TWsaData) : Integer; stdcall;

  TIpWsaCleanup = function : Integer; stdcall;

  TIpWsaSetLastError = procedure(Error : Integer); stdcall;

  TIpWsaGetLastError = function : Integer; stdcall;

  TIpWsaIsBlocking = function : BOOL; stdcall;

  TIpWsaUnhookBlockingHook = function : Integer; stdcall;

  TIpWsaSetBlockingHook = function(BlockFunc : TFarProc) : TFarProc; stdcall;

  TIpWsaCancelBlockingCall = function : Integer; stdcall;

  TIpWsaAsyncGetServByName = function(HWindow : TIpHWND; Msg : Cardinal; {!!.12}
    Name, Proto, Buf : PAnsiChar; BufLen : Integer) : TIpHandle; stdcall;

  TIpWsaAsyncGetServByPort = function(HWindow : TIpHWND; Msg : Cardinal; {!!.12}
    Port : Integer; Proto, Buf : PAnsiChar; BufLen : Integer) : TIpHandle; stdcall;

  TIpWsaAsyncGetProtoByName = function(HWindow : TIpHWND; Msg : Cardinal; {!!.12}
    Name, Buf : PAnsiChar; BufLen : Integer) : TIpHandle; stdcall;

  TIpWsaAsyncGetProtoByNumber = function(HWindow : TIpHWND; Msg : Cardinal; {!!.12}
    Number : Integer; Buf : PAnsiChar; BufLen : Integer) : TIpHandle; stdcall;

  TIpWsaAsyncGetHostByName = function(HWindow : TIpHWND; Msg : Cardinal; {!!.12}
    Name, Buf : PAnsiChar; BufLen : Integer) : TIpHandle; stdcall;

  TIpWsaAsyncGetHostByAddr = function(HWindow : TIpHWND; Msg : Cardinal; {!!.12}
    Addr : PAnsiChar; Len, Struct : Integer; Buf : PAnsiChar;
    BufLen : Integer) : TIpHandle; stdcall;

  TIpWsaCancelAsyncRequest = function(AsyncTaskHandle : TIpHandle) : Integer; stdcall;

  TIpWsaAsyncSelect = function(S : TSocket; HWindow : TIpHWND; {!!.12}
    Msg : Cardinal; Event : LongInt) : Integer; stdcall;

  { WinSock 2 API new function prototypes }

  TIpWsaAccept = function(S : TSocket; var Addr : TSockAddr; var Addrlen : Integer;
    Condition : TIpConditionProc; dwCallbackData : DWORD) : TSocket; stdcall;

  TIpWsaCloseEvent = function(Event : TWsaEvent) : Bool; stdcall;

  TIpWsaConnect = function(S : TSocket; const Name : TSockAddr; NameLen : Integer;
    var CallerData, CalleeData : TWsaBuf; var SQos, GQos : TQos) : Integer; stdcall;

  TIpWsaCreateEvent = function : TWsaEvent; stdcall;

  TIpWsaDuplicateSocket = function(S : TSocket; ProcessId : DWORD;
    var ProtocolInfo : TWsaProtocolInfo) : Integer; stdcall;

  TIpWsaEnumNetworkEvents = function(S : TSocket; EventObject : TWsaEvent;
    var NetworkEvents : TWsaNetworkEvents) : Integer; stdcall;

  TIpWsaEnumProtocols = function(var Protocols : Integer;
    var ProtocolBuffer : TWsaProtocolInfo; var BufferLength : DWORD) : Integer; stdcall;

  TIpWsaEventSelect = function(S : TSocket; EventObject : TWsaEvent;
    NetworkEvents : LongInt) : Integer; stdcall;

  TIpWsaGetOverlappedResult = function(S : TSocket; var Overlapped : TWsaOverlapped;
    var Transfer : DWORD; Wait : Bool; var Flags : DWORD) : Bool; stdcall;

  TIpWsaGetQosByName = function(S : TSocket; var QOSName : TWsaBuf;
    var QOS : TQos) : Bool; stdcall;

  TIpWsaHtonl = function(S : TSocket; HostLong : DWORD;
    var NetLong : DWORD) : Integer; stdcall;

  TIpWsaHtons = function(S : TSocket; HostShort : Word;
    var NetShort : Word) : Integer; stdcall;

  TIpWsaIoctl = function(S : TSocket; IoControlCode : DWORD;
    InBuffer : Pointer; cbInBuffer : DWORD; lpvOutBuffer : Pointer;
    cbOutBuffer : DWORD; var cbBytesReturned : DWORD;
    var Overlapped : TWsaOverlapped;
    CompletionRoutine : TIpWsaOverlappedCompletionRoutine) : Integer; stdcall;

  TIpWsaJoinLeaf = function(S : TSocket; const Name : TSockAddr;
    NameLen : Integer; var CallerData, CalleeData : TWsaBuf;
    var SQos, GQos : TQos; Flags : DWORD) : TSocket; stdcall;

  TIpWsaNtohl = function(S : TSocket; NetLong : DWORD;
    var HostLong : DWORD) : Integer; stdcall;

  TIpWsaNtohs = function(S : TSocket; NetShort : Word;
    var HostShort : Word) : Integer; stdcall;

  TIpWsaRecv = function(S : TSocket; var Buffers : TWsaBuf;
    BufferCount : DWORD; var NumberOfBytesRecvd : DWORD;
    var Flags : DWORD; var Overlapped : TWsaOverlapped;
    CompletionRoutine : TIpWsaOverlappedCompletionRoutine) : Integer; stdcall;

  TIpWsaRecvDisconnect = function(S : TSocket;
    var InboundDisconnectData : TWsaBuf) : Integer; stdcall;

  TIpWsaRecvFrom = function(S : TSocket; var Buffers : TWsaBuf;
    BufferCount : DWORD; var NumberOfBytesRecvd : DWORD; var Flags : DWORD;
    var From : TSockAddr; var FromLen : Integer; var Overlapped : TWsaOverlapped;
    CompletionRoutine : TIpWsaOverlappedCompletionRoutine) : Integer; stdcall;

  TIpWsaResetEvent = function(Event : TWsaEvent) : Bool; stdcall;

  TIpWsaSend = function(S : TSocket; var Buffers : TWsaBuf;
    BufferCount : DWORD; var NumberOfBytesSent : DWORD;
    Flags : DWORD; var Overlapped : TWsaOverlapped;
    CompletionRoutine : TIpWsaOverlappedCompletionRoutine) : Integer; stdcall;

  TIpWsaSendDisconnect = function(S : TSocket;
    var OutboundDisconnectData : TWsaBuf) : Integer; stdcall;

  TIpWsaSendTo = function(S : TSocket; var Buffers : TWsaBuf;
    BufferCount : DWORD; var NumberOfBytesSent : DWORD; Flags : DWORD;
    const SendTo : TSockAddr; ToLen : Integer; var Overlapped : TWsaOverlapped;
    CompletionRoutine : TIpWsaOverlappedCompletionRoutine) : Integer; stdcall;

  TIpWsaSetEvent = function(Event : TWsaEvent) : Bool; stdcall;

  TIpWsaSocket = function(Af, Kind, Protocol : Integer;
    var ProtocolInfo : TWsaProtocolInfo; Group : Cardinal;
    Flags : DWORD) : TSocket; stdcall;

  TIpWsaWaitForMultipleEvents = function(NumEvents : DWORD; Events : TWsaEvent;
    WaitAll : Bool; Timeout : DWORD; Alertable : Bool) : DWORD; stdcall;

  TIpWsaAddressToString = function(var Address : TSockAddr;
    AddressLength : DWORD; const ProtocolInfo : TWsaProtocolInfo;
    AddressString : PAnsiChar; var AddressStringLength : DWORD) : Integer; stdcall;

  TIpWsaStringToAddress = function(AddressString : PAnsiChar;
    AddressFamily : Integer; const ProtocolInfo : TWsaProtocolInfo;
    var Address : TSockAddr; var AddressLength : Integer) : Integer; stdcall;

  { Registration and Name Resolution API functions }

  TIpWsaLookupServiceBegin = function(const Restrictions : TWsaQuerySet;
    ControlFlags : DWORD; var Lookup : TIpHandle) : Integer; stdcall;

  TIpWsaLookupServiceNext = function(Lookup : TIpHandle; ControlFlags : DWORD;
    var BufferLength : DWORD; var Results : TWsaQuerySet) : Integer; stdcall;

  TIpWsaLookupServiceEnd = function(Lookup : TIpHandle) : Integer; stdcall;

  TIpWsaInstallServiceClass =
    function(const ServiceClassInfo : TWsaServiceClassInfo) : Integer; stdcall;

  TIpWsaRemoveServiceClass = function(const ServiceClassId : TGuid) : Integer; stdcall;

  TIpWsaGetServiceClassInfo = function(const ProviderId : TGuid;
    const ServiceClassId : TGuid; var BufSize : DWORD;
    var ServiceClassInfo : TWsaServiceClassInfo) : Integer; stdcall;

  TIpWsaEnumNameSpaceProviders = function(var BufferLength : DWORD;
    const NspBuffer : TWsaNameSpaceInfo) : Integer; stdcall;

  TIpWsaGetServiceClassNameByClassId = function(const ServiceClassId : TGuid;
    ServiceClassName : PAnsiChar; var BufferLength : DWORD) : Integer; stdcall;

  TIpWsaSetService = function(const RegInfo : TWsaQuerySet;
    Operation : TWsaSetServiceOp; const ControlFlags : DWORD) : Integer; stdcall;

  TIpWsaProviderConfigChange =
    function(var NotificationHandle : TIpHandle; const Overlapped : TWsaOverlapped;
    CompletionRoutine : TIpWsaOverlappedCompletionRoutine) : Integer; stdcall;

{

BOOL
PASCAL FAR
AcceptEx (
    IN SOCKET sListenSocket,
    IN SOCKET sAcceptSocket,
    IN PVOID lpOutputBuffer,
    IN DWORD dwReceiveDataLength,
    IN DWORD dwLocalAddressLength,
    IN DWORD dwRemoteAddressLength,
    OUT LPDWORD lpdwBytesReceived,
    IN LPOVERLAPPED lpOverlapped
    );

VOID
PASCAL FAR
GetAcceptExSockaddrs (
    IN PVOID lpOutputBuffer,
    IN DWORD dwReceiveDataLength,
    IN DWORD dwLocalAddressLength,
    IN DWORD dwRemoteAddressLength,
    OUT struct sockaddr **LocalSockaddr,
    OUT LPINT LocalSockaddrLength,
    OUT struct sockaddr **RemoteSockaddr,
    OUT LPINT RemoteSockaddrLength
    );

}

  EIpWinSockError = class(EIpBaseException)
  public
    ErrorCode : DWORD;
    constructor CreateWsError(ErrCode : Integer; const API : string);
  end;

  EIpActiveError = class(EIpBaseException);

  EIpIndexError = class(EIpBaseException);

  EIpNoSocketError = class(EIpBaseException);

  EIpNotConnectedError = class(EIpBaseException);

  EIpProgrammerError = class(EIpBaseException);

  TIpWinSockAccess = class(TIpBaseAccess)
  private
    { Internal variables }
    waWsaData : TWsaData;
    { DLL function pointers }
    FAccept : TIpAccept;
    FBind : TIpBind;
    FCloseSocket : TIpCloseSocket;
    FConnect : TIpConnect;
    FGetHostByAddr : TIpGetHostByAddr;
    FGetHostByName : TIpGetHostByName;
    FGetHostName : TIpGetHostName;
    FGetPeerName : TIpGetPeerName;
    FGetProtoByNumber : TIpGetProtoByNumber;
    FGetProtoByName : TIpGetProtoByName;
    FGetServByName : TIpGetServByName;
    FGetServByPort : TIpGetServByPort;
    FGetSockName : TIpGetSockName;
    FGetSockOpt : TIpGetSockOpt;
    Fhtonl : TIphtonl;
    Fhtons : TIphtons;
    FINet_Addr : TIpINet_Addr;
    FINet_Ntoa : TIpINet_Ntoa;
    FIOCtlSocket : TIpIOCtlSocket;
    FListen : TIpListen;
    FLoadedModule : TIpModuleVersion;
    Fntohl : TIpntohl;
    Fntohs : TIpntohs;
    FRecv : TIpRecv;
    FRecvFrom : TIpRecvFrom;
    FSelect : TIpSelect;
    FSend : TIpSend;
    FSendTo : TIpSendTo;
    FSetSockOpt : TIpSetSockOpt;
    FShutdown : TIpShutdown;
    FSocket : TIpSocket;
    FWsaAsyncGetHostByAddr : TIpWsaAsyncGetHostByAddr;
    FWsaAsyncGetHostByName : TIpWsaAsyncGetHostByName;
    FWsaAsyncGetProtoByName : TIpWsaAsyncGetProtoByName;
    FWsaAsyncGetProtoByNumber : TIpWsaAsyncGetProtoByNumber;
    FWsaAsyncGetServByName : TIpWsaAsyncGetServByName;
    FWsaAsyncGetServByPort : TIpWsaAsyncGetServByPort;
    FWsaAsyncSelect : TIpWsaAsyncSelect;
    FWsaCancelAsyncRequest : TIpWsaCancelAsyncRequest;
    FWsaCancelBlockingCall : TIpWsaCancelBlockingCall;
    FWsaCleanup : TIpWsaCleanup;
    FWsaGetLastError : TIpWsaGetLastError;
    FWsaIsBlocking : TIpWsaIsBlocking;
    FWsaSetBlockingHook : TIpWsaSetBlockingHook;
    FWsaSetLastError : TIpWsaSetLastError;
    FWsaStartup : TIpWsaStartup;
    FWsaUnhookBlockingHook : TIpWsaUnhookBlockingHook;
    { WinSock 2 function pointers }
    FWsaAccept : TIpWsaAccept;
    FWsaCloseEvent : TIpWsaCloseEvent;
    FWsaConnect : TIpWsaConnect;
    FWsaCreateEvent : TIpWsaCreateEvent;
    FWsaDuplicateSocket : TIpWsaDuplicateSocket;
    FWsaEnumNetworkEvents : TIpWsaEnumNetworkEvents;
    FWsaEnumProtocols : TIpWsaEnumProtocols;
    FWsaEventSelect : TIpWsaEventSelect;
    FWsaGetOverlappedResult : TIpWsaGetOverlappedResult;
    FWsaGetQosByName : TIpWsaGetQosByName;
    FWsaHtonl : TIpWsaHtonl;
    FWsaHtons : TIpWsaHtons;
    FWsaIoctl : TIpWsaIoctl;
    FWsaJoinLeaf : TIpWsaJoinLeaf;
    FWsaNtohl : TIpWsaNtohl;
    FWsaNtohs : TIpWsaNtohs;
    FWsaRecv : TIpWsaRecv;
    FWsaRecvDisconnect : TIpWsaRecvDisconnect;
    FWsaRecvFrom : TIpWsaRecvFrom;
    FWsaResetEvent : TIpWsaResetEvent;
    FWsaSend : TIpWsaSend;
    FWsaSendDisconnect : TIpWsaSendDisconnect;
    FWsaSendTo : TIpWsaSendTo;
    FWsaSetEvent : TIpWsaSetEvent;
    FWsaSocket : TIpWsaSocket;
    FWsaWaitForMultipleEvents : TIpWsaWaitForMultipleEvents;
    FWsaAddressToString : TIpWsaAddressToString;
    FWsaStringToAddress : TIpWsaStringToAddress;
    { Registration and Name Resolution API functions }
    FWsaLookupServiceBegin : TIpWsaLookupServiceBegin;
    FWsaLookupServiceNext : TIpWsaLookupServiceNext;
    FWsaLookupServiceEnd : TIpWsaLookupServiceEnd;
    FWsaInstallServiceClass : TIpWsaInstallServiceClass;
    FWsaRemoveServiceClass : TIpWsaRemoveServiceClass;
    FWsaGetServiceClassInfo : TIpWsaGetServiceClassInfo;
    FWsaEnumNameSpaceProviders : TIpWsaEnumNameSpaceProviders;
    FWsaGetServiceClassNameByClassId : TIpWsaGetServiceClassNameByClassId;
    FWsaSetService : TIpWsaSetService;
    FWsaProviderConfigChange : TIpWsaProviderConfigChange;
    function GetAccept : TIpAccept;
    function GetBind : TIpBind;
    function GetCloseSocket : TIpCloseSocket;
    function GetConnect : TIpConnect;
    function GetGetHostByAddr : TIpGetHostByAddr;
    function GetGetHostByName : TIpGetHostByName;
    function GetGetHostName : TIpGetHostName;
    function GetGetPeerName : TIpGetPeerName;
    function GetGetProtoByName : TIpGetProtoByName;
    function GetGetProtoByNumber : TIpGetProtoByNumber;
    function GetGetServByName : TIpGetServByName;
    function GetGetServByPort : TIpGetServByPort;
    function GetGetSockName : TIpGetSockName;
    function GetGetSockOpt : TIpGetSockOpt;
    function Gethtonl : TIphtonl;
    function Gethtons : TIphtons;
    function GetINet_Addr : TIpINet_Addr;
    function GetINet_Ntoa : TIpINet_Ntoa;
    function GetIOCtlSocket : TIpIOCtlSocket;
    function GetListen : TIpListen;
    function Getntohl : TIpntohl;
    function Getntohs : TIpntohs;
    function GetRecv : TIpRecv;
    function GetRecvFrom : TIpRecvFrom;
    function GetSelect : TIpSelect;
    function GetSend : TIpSend;
    function GetSendTo : TIpSendTo;
    function GetSetSockOpt : TIpSetSockOpt;
    function GetShutdown : TIpShutdown;
    function GetSocket : TIpSocket;
    function GetWsaAsyncGetHostByAddr : TIpWsaAsyncGetHostByAddr;
    function GetWsaAsyncGetHostByName : TIpWsaAsyncGetHostByName;
    function GetWsaAsyncGetProtoByName : TIpWsaAsyncGetProtoByName;
    function GetWsaAsyncGetProtoByNumber : TIpWsaAsyncGetProtoByNumber;
    function GetWsaAsyncGetServByName : TIpWsaAsyncGetServByName;
    function GetWsaAsyncGetServByPort : TIpWsaAsyncGetServByPort;
    function GetWsaAsyncSelect : TIpWsaAsyncSelect;
    function GetWsaCancelAsyncRequest : TIpWsaCancelAsyncRequest;
    function GetWsaCancelBlockingCall : TIpWsaCancelBlockingCall;
    function GetWsaCleanup : TIpWsaCleanup;
    function GetWsaGetLastError : TIpWsaGetLastError;
    function GetWsaIsBlocking : TIpWsaIsBlocking;
    function GetWsaSetBlockingHook : TIpWsaSetBlockingHook;
    function GetWsaSetLastError : TIpWsaSetLastError;
    function GetWsaStartup : TIpWsaStartup;
    function GetWsaUnhookBlockingHook : TIpWsaUnhookBlockingHook;
    { WinSock 2 }
    function GetWsaAccept : TIpWsaAccept;
    function GetWsaCloseEvent : TIpWsaCloseEvent;
    function GetWsaConnect : TIpWsaConnect;
    function GetWsaCreateEvent : TIpWsaCreateEvent;
    function GetWsaDuplicateSocket : TIpWsaDuplicateSocket;
    function GetWsaEnumNetworkEvents : TIpWsaEnumNetworkEvents;
    function GetWsaEnumProtocols : TIpWsaEnumProtocols;
    function GetWsaEventSelect : TIpWsaEventSelect;
    function GetWsaGetOverlappedResult : TIpWsaGetOverlappedResult;
    function GetWsaGetQosByName : TIpWsaGetQosByName;
    function GetWsaHtonl : TIpWsaHtonl;
    function GetWsaHtons : TIpWsaHtons;
    function GetWsaIoctl : TIpWsaIoctl;
    function GetWsaJoinLeaf : TIpWsaJoinLeaf;
    function GetWsaNtohl : TIpWsaNtohl;
    function GetWsaNtohs : TIpWsaNtohs;
    function GetWsaRecv : TIpWsaRecv;
    function GetWsaRecvDisconnect : TIpWsaRecvDisconnect;
    function GetWsaRecvFrom : TIpWsaRecvFrom;
    function GetWsaResetEvent : TIpWsaResetEvent;
    function GetWsaSend : TIpWsaSend;
    function GetWsaSendDisconnect : TIpWsaSendDisconnect;
    function GetWsaSendTo : TIpWsaSendTo;
    function GetWsaSetEvent : TIpWsaSetEvent;
    function GetWsaSocket : TIpWsaSocket;
    function GetWsaWaitForMultipleEvents : TIpWsaWaitForMultipleEvents;
    function GetWsaAddressToString : TIpWsaAddressToString;
    function GetWsaStringToAddress : TIpWsaStringToAddress;
    { Registration and Name Resolution API functions }
    function GetWsaLookupServiceBegin : TIpWsaLookupServiceBegin;
    function GetWsaLookupServiceNext : TIpWsaLookupServiceNext;
    function GetWsaLookupServiceEnd : TIpWsaLookupServiceEnd;
    function GetWsaInstallServiceClass : TIpWsaInstallServiceClass;
    function GetWsaRemoveServiceClass : TIpWsaRemoveServiceClass;
    function GetWsaGetServiceClassInfo : TIpWsaGetServiceClassInfo;
    function GetWsaEnumNameSpaceProviders : TIpWsaEnumNameSpaceProviders;
    function GetWsaGetServiceClassNameByClassId : TIpWsaGetServiceClassNameByClassId;
    function GetWsaSetService : TIpWsaSetService;
    function GetWsaProviderConfigChange : TIpWsaProviderConfigChange;
  public
    constructor Create; override;
    destructor Destroy; override;
    { DLL function pointers }
    property Accept : TIpAccept
      read GetAccept;
    property Bind : TIpBind
      read GetBind;
    property CloseSocket : TIpCloseSocket
      read GetCloseSocket;
    property Connect : TIpConnect
      read GetConnect;
    property GetHostByAddr : TIpGetHostByAddr
      read GetGetHostByAddr;
    property GetHostByName : TIpGetHostByName
      read GetGetHostByName;
    property GetHostName : TIpGetHostName
      read GetGetHostName;
    property GetPeerName : TIpGetPeerName
      read GetGetPeerName;
    property GetProtoByNumber : TIpGetProtoByNumber
      read GetGetProtoByNumber;
    property GetProtoByName : TIpGetProtoByName
      read GetGetProtoByName;
    property GetServByName : TIpGetServByName
      read GetGetServByName;
    property GetServByPort : TIpGetServByPort
      read GetGetServByPort;
    property GetSockName : TIpGetSockName
      read GetGetSockName;
    property GetSockOpt : TIpGetSockOpt
      read GetGetSockOpt;
    property htonl : TIphtonl
      read Gethtonl;
    property htons : TIphtons
      read Gethtons;
    property INet_Addr : TIpINet_Addr
      read GetINet_Addr;
    property INet_Ntoa : TIpINet_Ntoa
      read GetINet_Ntoa;
    property IOCtlSocket : TIpIOCtlSocket
      read GetIOCtlSocket;
    property Listen : TIpListen
      read GetListen;
    property LoadedModule : TIpModuleVersion
      read FLoadedModule;
    property ntohl : TIpntohl
      read Getntohl;
    property ntohs : TIpntohs
      read Getntohs;
    property Recv : TIpRecv
      read GetRecv;
    property RecvFrom : TIpRecvFrom
      read GetRecvFrom;
    property Select : TIpSelect
      read GetSelect;
    property Send : TIpSend
      read GetSend;
    property SendTo : TIpSendTo
      read GetSendTo;
    property SetSockOpt : TIpSetSockOpt
      read GetSetSockOpt;
    property Shutdown : TIpShutdown
      read GetShutdown;
    property Socket : TIpSocket
      read GetSocket;
    property WsaAsyncGetHostByAddr : TIpWsaAsyncGetHostByAddr
      read GetWsaAsyncGetHostByAddr;
    property WsaAsyncGetHostByName : TIpWsaAsyncGetHostByName
      read GetWsaAsyncGetHostByName;
    property WsaAsyncGetProtoByName : TIpWsaAsyncGetProtoByName
      read GetWsaAsyncGetProtoByName;
    property WsaAsyncGetProtoByNumber : TIpWsaAsyncGetProtoByNumber
      read GetWsaAsyncGetProtoByNumber;
    property WsaAsyncGetServByName : TIpWsaAsyncGetServByName
      read GetWsaAsyncGetServByName;
    property WsaAsyncGetServByPort : TIpWsaAsyncGetServByPort
      read GetWsaAsyncGetServByPort;
    property WsaAsyncSelect : TIpWsaAsyncSelect
      read GetWsaAsyncSelect;
    property WsaCancelAsyncRequest : TIpWsaCancelAsyncRequest
      read GetWsaCancelAsyncRequest;
    property WsaCancelBlockingCall : TIpWsaCancelBlockingCall
      read GetWsaCancelBlockingCall;
    property WsaCleanup : TIpWsaCleanup
      read GetWsaCleanup;
    property WsaGetLastError : TIpWsaGetLastError
      read GetWsaGetLastError;
    property WsaIsBlocking : TIpWsaIsBlocking
      read GetWsaIsBlocking;
    property WsaSetBlockingHook : TIpWsaSetBlockingHook
      read GetWsaSetBlockingHook;
    property WsaSetLastError : TIpWsaSetLastError
      read GetWsaSetLastError;
    property WsaStartup : TIpWsaStartup
      read GetWsaStartup;
    property WsaUnhookBlockingHook : TIpWsaUnhookBlockingHook
      read GetWsaUnhookBlockingHook;
    { WinSock 2 }
    property WsaAccept : TIpWsaAccept
      read GetWsaAccept;
    property WsaCloseEvent : TIpWsaCloseEvent
      read GetWsaCloseEvent;
    property WsaConnect : TIpWsaConnect
      read GetWsaConnect;
    property WsaCreateEvent : TIpWsaCreateEvent
      read GetWsaCreateEvent;
    property WsaDuplicateSocket : TIpWsaDuplicateSocket
      read GetWsaDuplicateSocket;
    property WsaEnumNetworkEvents : TIpWsaEnumNetworkEvents
      read GetWsaEnumNetworkEvents;
    property WsaEnumProtocols : TIpWsaEnumProtocols
      read GetWsaEnumProtocols;
    property WsaEventSelect : TIpWsaEventSelect
      read GetWsaEventSelect;
    property WsaGetOverlappedResult : TIpWsaGetOverlappedResult
      read GetWsaGetOverlappedResult;
    property WsaGetQosByName : TIpWsaGetQosByName
      read GetWsaGetQosByName;
    property WsaHtonl : TIpWsaHtonl
      read GetWsaHtonl;
    property WsaHtons : TIpWsaHtons
      read GetWsaHtons;
    property WsaIoctl : TIpWsaIoctl
      read GetWsaIoctl;
    property WsaJoinLeaf : TIpWsaJoinLeaf
      read GetWsaJoinLeaf;
    property WsaNtohl : TIpWsaNtohl
      read GetWsaNtohl;
    property WsaNtohs : TIpWsaNtohs
      read GetWsaNtohs;
    property WsaRecv : TIpWsaRecv
      read GetWsaRecv;
    property WsaRecvDisconnect : TIpWsaRecvDisconnect
      read GetWsaRecvDisconnect;
    property WsaRecvFrom : TIpWsaRecvFrom
      read GetWsaRecvFrom;
    property WsaResetEvent : TIpWsaResetEvent
      read GetWsaResetEvent;
    property WsaSend : TIpWsaSend
      read GetWsaSend;
    property WsaSendDisconnect : TIpWsaSendDisconnect
      read GetWsaSendDisconnect;
    property WsaSendTo : TIpWsaSendTo
      read GetWsaSendTo;
    property WsaSetEvent : TIpWsaSetEvent
      read GetWsaSetEvent;
    property WsaSocket : TIpWsaSocket
      read GetWsaSocket;
    property WsaWaitForMultipleEvents : TIpWsaWaitForMultipleEvents
      read GetWsaWaitForMultipleEvents;
    property WsaAddressToString : TIpWsaAddressToString
      read GetWsaAddressToString;
    property WsaStringToAddress : TIpWsaStringToAddress
      read GetWsaStringToAddress;
    { Registration and Name Resolution API functions }
    property WsaLookupServiceBegin : TIpWsaLookupServiceBegin
      read GetWsaLookupServiceBegin;
    property WsaLookupServiceNext : TIpWsaLookupServiceNext
      read GetWsaLookupServiceNext;
    property WsaLookupServiceEnd : TIpWsaLookupServiceEnd
      read GetWsaLookupServiceEnd;
    property WsaInstallServiceClass : TIpWsaInstallServiceClass
      read GetWsaInstallServiceClass;
    property WsaRemoveServiceClass : TIpWsaRemoveServiceClass
      read GetWsaRemoveServiceClass;
    property WsaGetServiceClassInfo : TIpWsaGetServiceClassInfo
      read GetWsaGetServiceClassInfo;
    property WsaEnumNameSpaceProviders : TIpWsaEnumNameSpaceProviders
      read GetWsaEnumNameSpaceProviders;
    property WsaGetServiceClassNameByClassId : TIpWsaGetServiceClassNameByClassId
      read GetWsaGetServiceClassNameByClassId;
    property WsaSetService : TIpWsaSetService
      read GetWsaSetService;
    property WsaProviderConfigChange : TIpWsaProviderConfigChange
      read GetWsaProviderConfigChange;
    { Other properties }
    property StartupData : TWsaData read waWsaData;
  end;

  TIpReceiveMode = (rmLine, rmStream, rmAny);                          {!!.01}

  TIpRejectType = (rtCount, rtDenied);

  TIpReplyType = (rtAddress, rtName, rtPort, rtService);

  TIpStatusType = (stConnect, stDisconnect, stProgress);

  TIpStatusSet = set of TIpStatusType;

  TIpLogOptions = (loHighData, loLowData, loTelnet);                   {!!.01}
  TIpLogOptionSet = set of TIpLogOptions;                              {!!.01}

  TIpConnRec = record
    RemoteAddr : TInAddr;
    RemotePort : Word;
    LocalAddr : TInAddr;
    LocalPort : Word;
  end;

  PIpReplyBuf = ^TIpReplyBuf;
  TIpReplyBuf = record
    Handle : TIpHandle;
    ReplyType : TIpReplyType;
    Buffer : array[0..MaxGetHostStruct-1] of AnsiChar;
  end;

  TIpSockStatRec = record
    { Resetable stats }
    BytesRcvd : DWORD;
    BytesSent : DWORD;
    RawBytesRcvd : DWORD;
    RawBytesSent : DWORD;
    { Total stats (these remain for the duration of the connection) }
    TotalBytesRcvd : DWORD;
    TotalBytesSent : DWORD;
    TotalRawBytesRcvd : DWORD;
    TotalRawBytesSent : DWORD;
  end;

  { Event types }

  TIpAddressReplyEvent = procedure(Sender : TObject; Handle : Cardinal;
    const Address : TInAddr) of object;

  TIpRejectEvent = procedure(Sender : TObject; Reason : TIpRejectType;
    var Msg : string) of object;

  TIpErrorEvent = procedure (Sender : TObject; Socket : TSocket;
    ErrCode : Integer; const ErrStr : string) of object;

  TIpErrorReplyEvent = procedure(Sender : TObject; Handle : Cardinal;
    ErrCode : Integer; const ErrStr : string) of object;

  TIpIdleTimeoutEvent = procedure(Sender : TObject; Socket : TSocket;
    var Reset : Boolean) of object;

  TIpItemSentEvent = procedure(Sender : TObject; Socket : TSocket;
    Handle : Cardinal; Remaining : Cardinal) of object;

  TIpNameReplyEvent = procedure(Sender : TObject; Handle : Cardinal;
    const Name : string) of object;

  TIpPortReplyEvent = procedure(Sender : TObject; Handle : Cardinal;
    Port : Integer) of object;

  TIpReadLineEvent = procedure(Sender : TObject; Socket : TSocket;
    const Line : string) of object;

  TIpServiceReplyEvent = procedure(Sender : TObject; Handle : Cardinal;
    const Service : string) of object;

  TIpStatusEvent = procedure (Sender : TObject; Socket : TSocket;
    Event : TIpStatusType; const Connection : TIpConnRec;
    const StatRec : TIpSockStatRec) of object;

const
  { Debug log constants }
  deEnabled   = 1;
  deDisabled  = 2;
  deTelnetCmd = 3;                                                     {!!.01}

  deString    = DWORD($80000000);
  scReadData  = DWORD($80000001);
  scWriteData = DWORD($80000002);
  scRawRead   = DWORD($80000003);                                      {!!.01}
  scRawWrite  = DWORD($80000004);                                      {!!.01}

  { Supplementary debug log constants }
  slRead      = 1;                                                     {!!.01}
  slWrite     = 2;                                                     {!!.01}

const
  diFixedLineLength = 80;
  diIdleTimeout = 0;
  diLineTermChar = #10;
  diLineTerminator = ltCRLF;
  diLogOptions = [loHighData, loTelnet];                               {!!.01}
  diReceiveBufferSize = 4096;
  diReceiveMode = rmLine;
  diSendBufferSize = 4096;
  diStripLineTerminator = False;

type
  TIpSockControl = class;

  TIpBaseLog = class(TPersistent)
  private
    { Property variables }
    FEnabled : Boolean;
    FFileName : TFileName;
    { Internal variables }
    blLogCS : TRTLCriticalSection;
    blOwner : TIpSockControl;
    { Property methods }
    function GetFileName : TFileName;
  protected
    procedure LockLog;
    procedure UnlockLog;
    function GetEnabled : Boolean;
    procedure SetEnabled(const Value : Boolean); virtual;
    procedure SetFileName(const Value : TFileName); virtual;
  public
    constructor Create(Owner : TIpSockControl); virtual;
    destructor Destroy; override;
    { Properties }
    property Enabled : Boolean
      read GetEnabled write SetEnabled;
    property FileName : TFileName
      read GetFileName write SetFileName;
  end;

  TIpWriteMode = (wmOverwrite, wmAppend);

  { Record for log entries }
  PIpDebugRec = ^TIpDebugRec;
  TIpDebugRec = record
    drClass : TIpComponentClass;
    drTime : DWORD;
    drSckt : DWORD;
    drData1 : DWORD;
    drData2 : DWORD;
    drData3 : DWORD;
  end;

  PIpDebugBuffer = ^TIpDebugBuffer;
  TIpDebugBuffer = array[0..MaxDebugLog] of Byte;

  TIpDebugLog = class(TIpBaseLog)
  private
    { Property variables }
    FBufferSize : DWORD;
    FWriteMode : TIpWriteMode;
    { Private variables }
    dlBuffer : PIpDebugBuffer;
    dlBufferHead : DWORD;
    dlBufferTail : DWORD;
    dlTempBuffer : PByteArray;
    dlTempSize : DWORD;
    dlTimeBase : DWORD;
  protected
    { Property access methods }
    function GetBufferEmpty : Boolean;                                 {!!.01}
    function GetBufferFree : DWORD;
    function GetBufferSize : DWORD;
    function GetWriteMode : TIpWriteMode;
    procedure SetBufferSize(const Value : DWORD);
    procedure SetEnabled(const Value : Boolean); override;
    procedure SetWriteMode(const Value : TIpWriteMode);
    { Internal methods }
    procedure dlCheckTempSize(SizeReq : DWORD);
    function dlDoFileHeader : string;
    function dlPopDebugEntry(var DebugRec : TIpDebugRec) : Boolean;
    function dlTelnetString(const S, D2, D3 : DWORD) : string;         {!!.01}
    function dlTimeStamp(Mark : DWORD) : string;
  public
    { Public methods }
    constructor Create(Owner : TIpSockControl); override;
    destructor Destroy; override;
    procedure AddDebugEntry(const deClass : TIpComponentClass; const S, D1, D2, D3 : DWORD);
    procedure ClearBuffer;
    procedure DumpLog;
    procedure WriteDebugString(const DebugString : string);
    { Public properties }
    property BufferEmpty : Boolean read GetBufferEmpty;                {!!.01}
    property BufferFree : DWORD read GetBufferFree;
  published
    { Published properties }
    property BufferSize : DWORD read GetBufferSize write SetBufferSize;
    property WriteMode : TIpWriteMode read GetWriteMode write SetWriteMode;
    { Inherited properties }
    property Enabled;
    property FileName;
  end;

  TIpEventLog = class(TIpBaseLog)
  private
    FDateTimeFormat : string;
  protected
    { Property methods }
    function GetDateTimeFormat : string;
    procedure SetDateTimeFormat(const Value : string);
  public
    procedure WriteEventString(const EventString : string);
  published
    property DateTimeFormat : string
      read GetDateTimeFormat write SetDateTimeFormat;
    { Inherited properties }
    property Enabled;
    property FileName;
  end;

  TIpSocksVersion = (svSocks4, svSocks4a, svSocks5);
  TIpSocksState = (ssNone, ssProxyConnect, ssNegotiating, ssAuthenticating,
                   ssRequesting, ssConnected);

  TIpSocks4Rec = packed record
    Ver : Byte;
    Cmd : Byte;
    Port : Word;
    Addr : TInAddr;
  end;

  TIpSocks5Rec = packed record
    Ver : Byte;
    Cmd : Byte;
    Rsv : Byte;
    Atyp : Byte;
    Addr : TInAddr;
    Port : Word;
  end;

  TIpSocks5NegRec = packed record
    Ver : Byte;
    Meth : Byte;
  end;

  TIpSocksServerInfo = class(TIpBasePersistent)
  private
    FAddress : string;
    FPassword : string;
    FPort : Word;
    FSocksVersion : TIpSocksVersion;
    FUserCode : string;
  protected
    function GetAddress : string;
    function GetPassword : string;
    function GetPort : Word;
    function GetSocksVersion : TIpSocksVersion;
    function GetUserCode : string;
    procedure SetAddress(const Value : string);
    procedure SetPassword(const Value : string);
    procedure SetPort(const Value : Word);
    procedure SetSocksVersion(const Value : TIpSocksVersion);
    procedure SetUserCode(const Value : string);
  public
    constructor Create; override;
    procedure Assign(Source : TPersistent); override;
  published
    property Address : string read FAddress write FAddress;
    property Password : string read FPassword write FPassword;
    property Port : Word read FPort write FPort;
    property SocksVersion : TIpSocksVersion
      read FSocksVersion write FSocksVersion;
    property UserCode : string read FUserCode write FUserCode;
  end;

  TIpBaseInits = class(TPersistent)
  private
    FSocketOptions : TIpSockOptionSet;
  public
    constructor Create;
  published
    property SocketOptions : TIpSockOptionSet
      read FSocketOptions write FSocketOptions default [];
  end;

  TIpDataInits = class(TIpBaseInits)
  private
    FFixedLineLength : DWORD;
    FIdleTimeout : DWORD;
    FLineTermChar : AnsiChar;
    FLineTerminator : TIpLineTerminator;
    FLogOptions : TIpLogOptionSet;                                     {!!.01}
    FReceiveBufferSize : Integer;
    FReceiveMode : TIpReceiveMode;
    FSendBufferSize : Integer;
    FStripLineTerminator : Boolean;
  public
    constructor Create;
  published
    property FixedLineLength : DWORD
      read FFixedLineLength write FFixedLineLength
      default diFixedLineLength;
    property IdleTimeout : DWORD
      read FIdleTimeout write FIdleTimeout
      default diIdleTimeout;
    property LineTermChar : AnsiChar
      read FLineTermChar write FLineTermChar
      default diLineTermChar;
    property LineTerminator : TIpLineTerminator
      read FLineTerminator write FLineTerminator
      default diLineTerminator;
    property LogOptions : TIpLogOptionSet                              {!!.01}
      read FLogOptions write FLogOptions                               {!!.01}
      default diLogOptions;                                            {!!.01}
    property ReceiveBufferSize : Integer
      read FReceiveBufferSize write FReceiveBufferSize
      default diReceiveBufferSize;
    property ReceiveMode : TIpReceiveMode
      read FReceiveMode write FReceiveMode
      default diReceiveMode;
    property SendBufferSize : Integer
      read FSendBufferSize write FSendBufferSize
      default diSendBufferSize;
    property StripLineTerminator : Boolean
      read FStripLineTerminator write FStripLineTerminator
      default diStripLineTerminator;
  end;

  TIpBaseSocket = class
  private
    { Property variables }
    FConnected : Boolean;
    FSocketHandle : TSocket;
    FURL : string;
    { Local socket options }
    FDiscardReceived : Boolean;
    FEchoReceived : Boolean;
    FEchoSent : Boolean;
    FStripHighBit : Boolean;
    FTelnet : Boolean;
    { Internal variables }
    bsLocalSockAddr : TSockAddrIn;
    bsRemoteSockAddr : TSockAddrIn;
    bsSockCS : TRTLCriticalSection;
    bsSocks : TIpSocksServerInfo;
    bsSocksState : TIpSocksState;
    bsSockType : Integer;
    { Internal methods }
    function GetLocalAddress : TInAddr;
    function GetLocalPort : Word;
    function GetRemoteAddress : TInAddr;
    function GetRemotePort : Word;
    function GetSocketOptions : TIpSockOptionSet;
    function GetURL : string;
    procedure SetSocketOptions(const Value : TIpSockOptionSet);
  protected
    { Internal variables }
    bsOwner : TIpSockControl;
    { Internal methods }
    function bsGetLastError : Integer;
    procedure bsHandleSocksConnect; virtual;
    procedure bsInitSocket(var LocalAddress : TSockAddrIn;
      const Inits : TIpBaseInits);
    procedure bsSetAsyncOptions; virtual;
    procedure LockSock;
    procedure UnlockSock;
    { Methods for SockControl <-> Socket communication }
    procedure bsAccept; virtual;
    procedure bsClose; virtual;
    procedure bsConnect; virtual;
    procedure bsOob; virtual;
    procedure bsRead; virtual;
    procedure bsTimer(Interval : Integer); virtual;
    procedure bsWrite; virtual;
    { Methods for socket control }
    procedure bsCloseSocket; virtual;
    procedure bsConnectSocket(var RemoteAddr : TSockAddrIn;
      const URL : string); virtual;
  public
    constructor Create(Owner : TIpSockControl; Socket : TSocket;
      var LocalAddress : TSockAddrIn; const Inits : TIpBaseInits;
      const Socks : TIpSocksServerInfo); virtual;
    destructor Destroy; override;
    { Properties }
    property Connected : Boolean read FConnected;
    property LocalAddress : TInAddr read GetLocalAddress;
    property LocalPort : Word read GetLocalPort;
    property RemoteAddress : TInAddr read GetRemoteAddress;
    property RemotePort : Word read GetRemotePort;
    property SocketHandle : TSocket read FSocketHandle;
    property SocketOptions : TIpSockOptionSet
      read GetSocketOptions write SetSocketOptions;
    property SocketType : Integer read bsSockType;
    property URL : string read GetURL;
  end;

  PIpSendQueueItem = ^TIpSendQueueItem;
  TIpSendQueueItem = record
    Handle : TIpHandle;     { Queue handle for item }
    Stream : TStream;       { Pointer to stream if there is one }
    Buffer : PByteArray;    { Pointer to Buffer }
    BufferSize : Integer;   { Size of Buffer }
    BufferOffset : Integer; { Offset to start of data yet to be sent }
    ActualData : Integer;   { Size of data prior to processing (telnet, etc) }
    Disconnect : Boolean;   { Disconnect after sending this item? }
    Internal : Boolean;     { Item originated internally (echo, etc) }
  end;

  TIpInBuffer = array[0..8191] of Byte;

  TIpTelnetOptSet = set of AnsiChar;

  TIpDataSocket = class(TIpBaseSocket)
  private
    { Property variables }
    FFixedLineLength : DWORD;
    FIdleTime : DWORD;
    FIdleTimeout : DWORD;
    FLineTermChar : AnsiChar;
    FLineTerminator : TIpLineTerminator;
    FLogOptions : TIpLogOptionSet;                                     {!!.01}
    FMasterTerminal : TIpBaseWinControl;
    FReceiveMode : TIpReceiveMode;
    FReceiveStream : TStream;
    FStripLineTerminator : Boolean;
    { Internal variables }
    dsInBuf : TIpInBuffer;
    dsInHead : DWORD;
    dsInTail : DWORD;
    dsLineBuf : string;                                                {!!.03}
    dsNextItem : TIpDataSocket;
    dsSendQueue : TList;
    dsSockStats : TIpSockStatRec;
    dsSockStatsDirty : Boolean;
    dsTelnetCmd : PAnsiChar;
    dsTelnetCmdTail : DWORD;
    dsTerminalList : TList;
    dsQueueHandle : DWORD;
    dsWritable : Boolean;
    { Telnet Options }
    toHostAllowed : TIpTelnetOptSet;
    toHostEnabled : TIpTelnetOptSet;
    toHostNegotiating : TIpTelnetOptSet;
    toRemoteDesired : TIpTelnetOptSet;
    toRemoteEnabled : TIpTelnetOptSet;
    toRemoteNegotiating : TIpTelnetOptSet;
    { Property access methods }
    function GetFixedLineLength : DWORD;
    function GetIdleTime : DWORD;
    function GetLineTermChar : AnsiChar;
    function GetLineTerminator : TIpLineTerminator;
    function GetLogOptions : TIpLogOptionSet;                          {!!.01}
    function GetMasterTerminal : TIpBaseWinControl;
    function GetReceiveBufferSize : Integer;
    function GetReceiveMode : TIpReceiveMode;
    function GetReceiveStream : TStream;
    function GetSendBufferSize : Integer;
    function GetStripLineTerminator : Boolean;
    procedure SetFixedLineLength(const Value : DWORD);
    procedure SetIdleTimeout(const Value : DWORD);
    procedure SetLineTermChar(const Value : AnsiChar);
    procedure SetLineTerminator(const Value : TIpLineTerminator);
    procedure SetLogOptions(const Value : TIpLogOptionSet);            {!!.01}
    procedure SetMasterTerminal(const Value : TIpBaseWinControl);
    procedure SetReceiveBufferSize(const Value : Integer);
    procedure SetReceiveMode(const Value : TIpReceiveMode);
    procedure SetReceiveStream(const Value : TStream);
    procedure SetSendBufferSize(const Value : Integer);
    procedure SetStripLineTerminator(const Value : Boolean);
    { Internal methods }
    function dsCheckRead : Boolean;
    function dsCheckSendQueue : Boolean;
    procedure dsClearSendQueue;
    procedure dsEnableTelnetHostOption(Option : AnsiChar);
    function dsExtractLine(Force : Boolean) : Boolean;
    procedure dsFreeQueueItem(QItem : PIpSendQueueItem);
    function dsInBufAvail : DWORD;
    procedure dsIncIdleTime(Delta : Integer);
    function dsNextQueueHandle : DWORD;
    function dsReadData(Force : Boolean) : Integer;
    procedure dsRefreshQueueItemBuffer(QItem : PIpSendQueueItem);
    procedure dsResetIdleTime;
    function dsSendQueueItem(QItem : PIpSendQueueItem) : Boolean;
    procedure dsSendTelnet(Command, Option : AnsiChar);
    procedure dsSendTelnetStatus;
    procedure dsSendTerminal;
    procedure dsSendTerminalSize;
    procedure dsShutDown;
    procedure dsStripHighBit(var Buffer; BufLen : DWORD);
    function dsSocksRcv(var Buffer; BufLen : DWORD) : DWORD;
    function dsTelnetRcv(var Buffer; BufLen : DWORD) : DWORD;
    procedure dsTelnetSnd(var InBuf, OutBuf; InBufLen : Integer);
    procedure dsWriteTerm(var Buffer; BufLen : DWORD);
  protected

    { Methods for SockControl <-> Socket communication }
    procedure bsClose; override;
    procedure bsRead; override;
    procedure bsTimer(Interval : Integer); override;
    procedure bsWrite; override;
    { Internal methods }
    procedure bsCloseSocket; override;
    procedure bsConnectSocket(var RemoteAddr : TSockAddrIn;
      const URL : string); override;
    procedure bsHandleSocksConnect; override;
    procedure bsSetAsyncOptions; override;
    procedure dsAttachTerminal(Handle : TIpHWND); {!!.12}
    procedure dsDetachTerminal(Handle : TIpHWND); {!!.12}
    procedure dsIdleTimeout; virtual;
    function dsPutData(var Buffer; BufferSize : Integer; Stream : TStream;
      Disconnect, Internal, Cmd : Boolean) : TIpHandle; virtual;
    procedure dsHandleSubnegotiation(const Subnegotiation : string); virtual;
    function dsHandleTelnet(Command, Option : AnsiChar) : Boolean; virtual;
  public
    constructor Create(Owner : TIpSockControl; Socket : TSocket;
      var LocalAddress : TSockAddrIn; const Inits : TIpBaseInits;
      const Socks : TIpSocksServerInfo); override;
    destructor Destroy; override;
    function PutBlock(var Buffer; BufferSize : Integer; Disconnect : Boolean) : TIpHandle;
    function PutStream(const Stm : TStream; Disconnect : Boolean) : TIpHandle;
    function PutString(const Str : string; Disconnect : Boolean) : TIpHandle;
    procedure ResetStats;
    procedure StatusSnapshot(var StatRec : TIpSockStatRec);
    { Properties }
    property FixedLineLength : DWORD
      read GetFixedLineLength write SetFixedLineLength;
    property IdleTime : DWORD read GetIdleTime;
    property IdleTimeout : DWORD
      read FIdleTimeout write SetIdleTimeout;
    property LineTermChar : AnsiChar
      read GetLineTermChar write SetLineTermChar;
    property LineTerminator : TIpLineTerminator
      read GetLineTerminator write SetLineTerminator;
    property LogOptions : TIpLogOptionSet                              {!!.01}
      read GetLogOptions write SetLogOptions;                          {!!.01}
    property MasterTerminal : TIpBaseWinControl
      read GetMasterTerminal write SetMasterTerminal;
    property ReceiveBufferSize : Integer
      read GetReceiveBufferSize write SetReceiveBufferSize;
    property ReceiveMode : TIpReceiveMode
      read GetReceiveMode write SetReceiveMode;
    property ReceiveStream : TStream
      read GetReceiveStream write SetReceiveStream;
    property SendBufferSize : Integer
      read GetSendBufferSize write SetSendBufferSize;
    property StripLineTerminator : Boolean
      read GetStripLineTerminator write SetStripLineTerminator;
  end;

  TIpListenSocket = class(TIpBaseSocket)
  protected
    { Internal methods }
    procedure bsSetAsyncOptions; override;
  public
    constructor Create(Owner : TIpSockControl; Socket : TSocket;
      var LocalAddress : TSockAddrIn; const Inits : TIpBaseInits;
      const Socks : TIpSocksServerInfo); override;
  end;

  TIpSockControl = class(TIpBaseComponent)
  private
    { Property variables }
    FDebugLog : TIpDebugLog;
    FDefaultLocalAddress : Integer;
    FDirectReadLine : Boolean;                                         {!!.03}
    FEventLog : TIpEventLog;
    FHandle : TIpHWND;                                                  {!!.12}
    FSockInits : TIpDataInits;
    FSockProtocol : TIpSockProtocol;
    FStatusEvents : TIpStatusSet;
    FTimerInterval : DWORD;
    { Event variables }
    FOnAddressReply : TIpAddressReplyEvent;
    FOnError : TIpErrorEvent;
    FOnErrorReply : TIpErrorReplyEvent;
    FOnIdleTimeout : TIpIdleTimeoutEvent;
    FOnItemSent : TIpItemSentEvent;
    FOnNameReply : TIpNameReplyEvent;
    FOnPortReply : TIpPortReplyEvent;
    FOnReadLine : TIpReadLineEvent;
    FOnServiceReply : TIpServiceReplyEvent;
    FOnStatus : TIpStatusEvent;
    { Internal variables }
    scLastTime : DWORD;
    scProtoString : string;
    scProtoType : Integer;
    scReplyList : TThreadList;
    scTerminalList : TList;
    { Property access methods }
    function GetDefaultLocalAddress : Integer;
    function GetHostAddress(Index : Integer) : string;
    function GetHostAddressCount : Integer;
    function GetLastError : Integer;
    function GetLocalHost : string;
    function GetMasterTerminal(Socket : TSocket) : TIpBaseWinControl;
    function GetModuleVersion : TIpModuleVersion;
    function GetwsDescription : string;
    function GetwsHighVersion : Word;
    function GetwsMaxDatagram : Word;
    function GetwsMaxSockets : Word;
    function GetwsSystemStatus : string;
    function GetwsVersion : Word;
    procedure SetDefaultLocalAddress(const Value : Integer);
    procedure SetMasterTerminal(Socket : TSocket; const Value : TIpBaseWinControl);
    procedure SetTimerInterval(const Value : DWORD);
  protected
    { Internal variables }
    scLocalSockAddr : TSockAddrIn;
    { Internal methods }
    function GetDefaultLocalPort : Word;                               {!!.11}
    procedure SetDefaultLocalPort(const Value : Word);                 {!!.11}
    function scCreateSocket : TSocket; virtual; abstract;
    procedure scFreeSocket(Socket : TSocket); virtual; abstract;
    procedure scString2SockAddr(const Addr : string;
      var SockAddr : TSockAddrIn; DefPort : Word);
    { Virtual property methods }
    function GetActiveSockets : Boolean; virtual;
    procedure SetSockProtocol(const Value : TIpSockProtocol); virtual;
    { Virtual methods for events }
    procedure DoAddressReply(Handle : TIpHandle;
      const Address : TInAddr); virtual;
    procedure DoError(Socket : TSocket; ErrCode : Integer;
      const ErrStr : string); virtual;
    procedure DoErrorReply(Handle : TIpHandle; ErrCode : Integer;
      const ErrStr : string); virtual;
    procedure DoIdleTimeout(Socket : TSocket; var Reset : Boolean); virtual;
    procedure DoItemSent(Socket : TSocket; Handle : TIpHandle;
      Remaining : DWORD); virtual;
    procedure DoNameReply(Handle : TIpHandle; const Name : string); virtual;
    procedure DoPortReply(Handle : TIpHandle; Port : Integer); virtual;
    procedure DoReadLine(Socket : TSocket; const Line : string); virtual;
    procedure DoServiceReply(Handle : TIpHandle; const Service : string); virtual;
    procedure DoStatus(Socket : TSocket; Event : TIpStatusType;
      Connection : TIpConnRec); virtual;
    { Virtual methods to handle incoming socket messages }
    procedure scAccept(Socket : TSocket); virtual;
    procedure scClose(Socket : TSocket); virtual;
    procedure scConnect(Socket : TSocket); virtual;
    procedure scOob(Socket : TSocket); virtual;
    procedure scRead(Socket : TSocket); virtual;
    procedure scTimer; virtual;
    procedure scTimerBroadcast(Interval : Integer); virtual;
    procedure scWrite(Socket : TSocket); virtual;
    { Internal methods for descendant methods/properties }
    function scAttachTerminal(Socket : TSocket; Handle : TIpHWND) : Boolean; {!!.12} 
    procedure scDetachTerminal(Socket : TSocket; Handle : TIpHWND);          {!!.12}
    function scGetSocket(Socket : TSocket; Validate : Boolean) : TIpDataSocket; virtual;
    function scPutBlock(Socket : TSocket; var Buf; BufferSize : Integer;
      Disconnect : Boolean) : TIpHandle;
    function scPutStream(Socket : TSocket; const Stm : TStream;
      Disconnect : Boolean) : TIpHandle;
    function scPutString(Socket : TSocket; const Str : string;
      Disconnect : Boolean) : TIpHandle;
    procedure scResetStats(Socket : TSocket);
    procedure scStatusSnapshot(Socket : TSocket; var StatRec : TIpSockStatRec);
    { Message handlers }
    procedure CMIpAsyncResult(var Message : TCMIpAsyncResult); message CM_IPASYNCRESULT;
    procedure CMIpFreeSocket(var Message : TCMIpSockMessage); message CM_IPFREESOCKET;
    procedure CMIpLineMessage(var Message : TCMIpLineMessage); message CM_IPLINEMESSAGE;
    procedure CMIpSockMessage(var Message : TCMIpSockMessage); message CM_IPSOCKMESSAGE;
    procedure CMIpTermResize(var Message : TMessage); message CM_IPTERMRESIZE;
    procedure WndProc(var Message : TMessage); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    class function GetLogString(const S, D1, D2, D3 : DWORD) : string; override;
    { Utility routines }
    function ExtractAddress(HostEnt : PHostEnt; Index : Integer) : TInAddr;
    function ExtractAddressCount(HostEnt : PHostEnt) : Integer;
    function ExtractAlias(HostEnt : PHostEnt; Index : Integer) : string;
    function ExtractAliasCount(HostEnt : PHostEnt) : Integer;
    function ExtractName(HostEnt : PHostEnt) : string;
    function ExtractPort(ServEnt : PServEnt) : Integer;
    function ExtractService(ServEnt : PServEnt) : string;
    { Conversion methods }
    function htonl(HostLong : DWORD) : DWORD;
    function htons(HostShort : Word) : Word;
    function ntohl(NetLong : DWORD) : DWORD;
    function ntohs(NetShort : Word) : Word;
    function NetAddr2String(InAddr : TInAddr) : string;
    function String2NetAddr(const S : string) : TInAddr;
    { Lookup (Synchronous) routines }
    function LookupAddress(const Name : string) : TInAddr;
    function LookupName(InAddr : TInAddr) : string;
    function LookupPort(const Service : string) : Integer;
    function LookupService(Port : Word) : string;
    { Request (Asynchronous) routines }
    function RequestAddressLookup(const Name : string) : TIpHandle;
    function RequestNameLookup(InAddr : TInAddr) : TIpHandle;
    function RequestPortLookup(const Service : string) : TIpHandle;
    function RequestServiceLookup(Port : Word) : TIpHandle;
    { Registration routines }
    procedure RegisterTerminal(Handle : TIpHWND); virtual;             {!!.12}
    procedure DeRegisterTerminal(Handle : TIpHWND); virtual;           {!!.12}
    { Properties }
    property ActiveSockets : Boolean read GetActiveSockets;
    property DebugLog : TIpDebugLog
      read FDebugLog write FDebugLog;
    property DefaultLocalAddress : Integer
      read GetDefaultLocalAddress write SetDefaultLocalAddress;
    property DefaultLocalPort : Word
      read GetDefaultLocalPort write SetDefaultLocalPort;
    property DirectReadLine : Boolean                                  {!!.03}
      read FDirectReadLine write FDirectReadLine                       {!!.03}
      default False;                                                   {!!.03} 
    property EventLog : TIpEventLog
      read FEventLog write FEventLog;
    property Handle : TIpHWND read FHandle;                             {!!.12}
    property HostAddress[Index : Integer] : string read GetHostAddress;
    property HostAddressCount : Integer read GetHostAddressCount;
    property LastError : Integer read GetLastError;
    property LoadedModule : TIpModuleVersion read GetModuleVersion;
    property LocalHost : string read GetLocalHost;
    property MasterTerminal[Socket : TSocket] : TIpBaseWinControl
      read GetMasterTerminal write SetMasterTerminal;
    property SockInits : TIpDataInits
      read FSockInits write FSockInits;
    property SockProtocol : TIpSockProtocol
      read FSockProtocol write SetSockProtocol;
    property StatusEvents : TIpStatusSet
      read FStatusEvents write FStatusEvents;
    property TimerInterval : DWORD
      read FTimerInterval write SetTimerInterval;
    property wsDescription : string read GetwsDescription;
    property wsHighVersion : Word read GetwsHighVersion;
    property wsMaxDatagram : Word read GetwsMaxDatagram;
    property wsMaxSockets : Word read GetwsMaxSockets;
    property wsSystemStatus : string read GetwsSystemStatus;
    property wsVersion : Word read GetwsVersion;
    { Events }
    property OnAddressReply : TIpAddressReplyEvent
      read FOnAddressReply write FOnAddressReply;
    property OnError : TIpErrorEvent
      read FOnError write FOnError;
    property OnErrorReply : TIpErrorReplyEvent
      read FOnErrorReply write FOnErrorReply;
    property OnIdleTimeout : TIpIdleTimeoutEvent
      read FOnIdleTimeout write FOnIdleTimeout;
    property OnItemSent : TIpItemSentEvent
      read FOnItemSent write FOnItemSent;
    property OnNameReply : TIpNameReplyEvent
      read FOnNameReply write FOnNameReply;
    property OnPortReply : TIpPortReplyEvent
      read FOnPortReply write FOnPortReply;
    property OnReadLine : TIpReadLineEvent
      read FOnReadLine write FOnReadLine;
    property OnServiceReply : TIpServiceReplyEvent
      read FOnServiceReply write FOnServiceReply;
    property OnStatus : TIpStatusEvent
      read FOnStatus write FOnStatus;
  end;

  TIpCustomClient = class(TIpSockControl)
  private
    { Property variables }
    FDefaultPort : Word;
    FSocksServer : TIpSocksServerInfo;
    { Internal variables }
    ccClientSocket : TIpDataSocket;
    { Property methods }
    function GetConnected : Boolean;
    function GetFixedLineLength : DWORD;
    function GetIdleTime : DWORD;
    function GetIdleTimeout : DWORD;
    function GetLineTermChar : AnsiChar;
    function GetLineTerminator : TIpLineTerminator;
    function GetLocalAddress : TInAddr;
    function GetLocalPort : Word;
    function GetLogOptions : TIpLogOptionSet;                          {!!.01}
    function GetReceiveBufferSize : Integer;
    function GetReceiveMode : TIpReceiveMode;
    function GetReceiveStream : TStream;
    function GetRemoteAddress : TInAddr;
    function GetRemotePort : Word;
    function GetSendBufferSize : Integer;
    function GetSocketHandle : TSocket;
    function GetSocketOptions : TIpSockOptionSet;
    function GetStripLineTerminator : Boolean;
    function GetURL : string;
    procedure SetFixedLineLength(const Value : DWORD);
    procedure SetIdleTimeout(const Value : DWORD);
    procedure SetLineTermChar(const Value : AnsiChar);
    procedure SetLineTerminator(const Value : TIpLineTerminator);
    procedure SetLogOptions(const Value : TIpLogOptionSet);            {!!.01}
    procedure SetReceiveBufferSize(const Value : Integer);
    procedure SetReceiveMode(const Value : TIpReceiveMode);
    procedure SetReceiveStream(const Value : TStream);
    procedure SetSendBufferSize(const Value : Integer);
    procedure SetSocketOptions(const Value : TIpSockOptionSet);
    procedure SetStripLineTerminator(const Value : Boolean);
  protected
    { Virtual methods for events }
    procedure DoStatus(Socket : TSocket; Event : TIpStatusType;
      Connection : TIpConnRec); override;
    { Virtual property methods }
    function GetActiveSockets : Boolean; override;
    { Internal methods }
    procedure ccCloseSocket(Socket : TSocket); virtual;
    function ccConnectSocket(const RemoteURL : string) : TSocket; virtual;
    function scCreateSocket : TSocket; override;
    procedure scFreeSocket(Socket : TSocket); override;
    function scGetSocket(Socket : TSocket; Validate : Boolean) : TIpDataSocket; override;
    procedure scTimerBroadcast(Interval : Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RegisterTerminal(Handle : TIpHWND); override;             {!!.12}
    { Properties }
    property Connected : Boolean read GetConnected;
    property DefaultPort : Word read FDefaultPort write FDefaultPort;
    property FixedLineLength : DWORD
      read GetFixedLineLength write SetFixedLineLength;
    property IdleTime : DWORD read GetIdleTime;
    property IdleTimeout : DWORD read GetIdleTimeout write SetIdleTimeout;
    property LineTermChar : AnsiChar
      read GetLineTermChar write SetLineTermChar;
    property LineTerminator : TIpLineTerminator
      read GetLineTerminator write SetLineTerminator;
    property LocalAddress : TInAddr read GetLocalAddress;
    property LocalPort : Word read GetLocalPort;
    property LogOptions : TIpLogOptionSet                              {!!.01}
      read GetLogOptions write SetLogOptions;                          {!!.01}
    property ReceiveBufferSize : Integer
      read GetReceiveBufferSize write SetReceiveBufferSize;
    property ReceiveMode : TIpReceiveMode
      read GetReceiveMode write SetReceiveMode;
    property ReceiveStream : TStream
      read GetReceiveStream write SetReceiveStream;
    property RemoteAddress : TInAddr read GetRemoteAddress;
    property RemotePort : Word read GetRemotePort;
    property SendBufferSize : Integer
      read GetSendBufferSize write SetSendBufferSize;
    property SocketHandle : TSocket read GetSocketHandle;
    property SocketOptions : TIpSockOptionSet
      read GetSocketOptions write SetSocketOptions;
    property SocksServer : TIpSocksServerInfo
      read FSocksServer write FSocksServer;
    property StripLineTerminator : Boolean
      read GetStripLineTerminator write SetStripLineTerminator;
    property URL : string read GetURL;
  end;

  TIpClient = class(TIpCustomClient)
  public
    procedure CloseSocket;
    function ConnectSocket(const RemoteURL : string) : TSocket;
    function PutBlock(var Buf; BufferSize : Integer; Disconnect : Boolean) : TIpHandle;
    function PutStream(const Stm : TStream; Disconnect : Boolean) : TIpHandle;
    function PutString(const Str : string; Disconnect : Boolean) : TIpHandle;
    procedure ResetStats;
    procedure StatusSnapshot(var StatRec : TIpSockStatRec);
  published
    { Properties from TIpCustomClient }
    property DefaultPort;                                              {!!.01}
    { Properties from TIpSockControl }
    property DebugLog;
    property EventLog;
    property SockInits;
    property SockProtocol;
    property SocksServer;
    property StatusEvents;
    property TimerInterval;
    { Events from TIpSockControl }
    property OnAddressReply;
    property OnError;
    property OnErrorReply;
    property OnIdleTimeout;
    property OnItemSent;
    property OnNameReply;
    property OnPortReply;
    property OnReadLine;
    property OnServiceReply;
    property OnStatus;
  end;

  TIpSocketList = class
  private
    FList : TList;
    HashTable : array[0..IpHashSize-1] of TIpDataSocket;
    slListCS : TRTLCriticalSection;
  protected
    procedure DeleteIndexEntry(Index : Integer);
    function GenerateHash(S : TSocket) : LongInt;
    function Get(Index : Integer) : Pointer;
    function GetCount : Integer;
    procedure Put(Index : Integer; Item : Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item : Pointer);
    procedure BroadcastClose;
    procedure BroadcastTimer(Interval : Integer);
    procedure Delete(Index : Integer);
    function Find(S : TSocket) : Integer;
    function FindPtr(S : TSocket) : TIpDataSocket;
    procedure FreeSocket(S : TSocket);
    procedure LockList;
    procedure UnlockList;
    { Properties }
    property Count : Integer read GetCount;
    property Items[Index : Integer] : Pointer
      read Get write Put; default;
  end;

  TIpMultiSockControl = class(TIpSockControl)
  private
    { Internal variables }
    msSocketList : TIpSocketList;
    { Property methods }
    function GetConnected(Socket : TSocket) : Boolean;
    function GetFixedLineLength(Socket : TSocket) : DWORD;
    function GetIdleTime(Socket : TSocket) : DWORD;
    function GetIdleTimeout(Socket : TSocket) : DWORD;
    function GetLineTermChar(Socket : TSocket) : AnsiChar;
    function GetLineTerminator(Socket : TSocket) : TIpLineTerminator;
    function GetLocalAddress(Socket : TSocket) : TInAddr;
    function GetLocalPort(Socket : TSocket) : Word;
    function GetLogOptions(Socket : TSocket) : TIpLogOptionSet;        {!!.01}
    function GetReceiveBufferSize(Socket : TSocket) : Integer;
    function GetReceiveMode(Socket : TSocket) : TIpReceiveMode;
    function GetReceiveStream(Socket : TSocket) : TStream;
    function GetRemoteAddress(Socket : TSocket) : TInAddr;
    function GetRemotePort(Socket : TSocket) : Word;
    function GetSendBufferSize(Socket : TSocket) : Integer;
    function GetSocketCount : Integer;
    function GetSocketOptions(Socket : TSocket) : TIpSockOptionSet;
    function GetStripLineTerminator(Socket : TSocket) : Boolean;
    function GetURL(Socket : TSocket) : string;
    procedure SetFixedLineLength(Socket : TSocket; const Value : DWORD);
    procedure SetIdleTimeout(Socket : TSocket; const Value : DWORD);
    procedure SetLineTermChar(Socket : TSocket; const Value : AnsiChar);
    procedure SetLineTerminator(Socket : TSocket; const Value : TIpLineTerminator);
    procedure SetLogOptions(Socket : TSocket; const Value : TIpLogOptionSet); {!!.01}
    procedure SetReceiveBufferSize(Socket : TSocket; const Value : Integer);
    procedure SetReceiveMode(Socket : TSocket; const Value : TIpReceiveMode);
    procedure SetReceiveStream(Socket : TSocket; const Value : TStream);
    procedure SetSendBufferSize(Socket : TSocket; const Value : Integer);
    procedure SetSocketOptions(Socket : TSocket; const Value : TIpSockOptionSet);
    procedure SetStripLineTerminator(Socket : TSocket; const Value : Boolean);
  protected
    { Virtual methods for events }
    procedure DoStatus(Socket : TSocket; Event : TIpStatusType;
      Connection : TIpConnRec); override;
    { Virtual property methods }
    function GetActiveSockets : Boolean; override;
    { Internal methods }
    procedure msAddDataSocket(const DataSocket : TIpDataSocket);       {!!.01}
    procedure msCloseAll; virtual;
    procedure msCloseSocket(Socket : TSocket); virtual;
    function scCreateSocket : TSocket; override;
    procedure scFreeSocket(Socket : TSocket); override;
    function scGetSocket(Socket : TSocket; Validate : Boolean) : TIpDataSocket; override;
    procedure scTimerBroadcast(Interval : Integer); override;
  public
    { Public methods }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RegisterTerminal(Handle : TIpHWND); override;            {!!.12}
    procedure ResetStats(Socket : TSocket);
    procedure StatusSnapshot(Socket : TSocket; var StatRec : TIpSockStatRec);
    { Properties }
    property Connected[Socket : TSocket] : Boolean read GetConnected;
    property FixedLineLength[Socket : TSocket] : DWORD
      read GetFixedLineLength write SetFixedLineLength;
    property IdleTime[Socket : TSocket] : DWORD read GetIdleTime;
    property IdleTimeout[Socket : TSocket] : DWORD
      read GetIdleTimeout write SetIdleTimeout;
    property LineTermChar[Socket : TSocket] : AnsiChar
      read GetLineTermChar write SetLineTermChar;
    property LineTerminator[Socket : TSocket] : TIpLineTerminator
      read GetLineTerminator write SetLineTerminator;
    property LocalAddress[Socket : TSocket] : TInAddr read GetLocalAddress;
    property LocalPort[Socket : TSocket] : Word read GetLocalPort;
    property LogOptions[Socket : TSocket] : TIpLogOptionSet            {!!.01}
      read GetLogOptions write SetLogOptions;                          {!!.01}
    property ReceiveBufferSize[Socket : TSocket] : Integer
      read GetReceiveBufferSize write SetReceiveBufferSize;
    property ReceiveMode[Socket : TSocket] : TIpReceiveMode
      read GetReceiveMode write SetReceiveMode;
    property ReceiveStream[Socket : TSocket] : TStream
      read GetReceiveStream write SetReceiveStream;
    property RemoteAddress[Socket : TSocket] : TInAddr read GetRemoteAddress;
    property RemotePort[Socket : TSocket] : Word read GetRemotePort;
    property SendBufferSize[Socket : TSocket] : Integer
      read GetSendBufferSize write SetSendBufferSize;
    property SocketCount : Integer read GetSocketCount;
    property SocketOptions[Socket : TSocket] : TIpSockOptionSet
      read GetSocketOptions write SetSocketOptions;
    property StripLineTerminator[Socket : TSocket] : Boolean
      read GetStripLineTerminator write SetStripLineTerminator;
    property URL[Socket : TSocket] : string read GetURL;
    { Events }
  end;

  TIpCustomMultiClient = class(TIpMultiSockControl)
  private
    { Property variables }
    FDefaultPort : Word;
    FSocksServer : TIpSocksServerInfo;
  protected
    function mcConnectSocket(const RemoteURL : string) : TSocket; virtual;
    procedure mcConnectThisSocket(Socket : TSocket; const RemoteURL : string);
    function scCreateSocket : TSocket; override;
  public
    { Public methods }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    { Properties }
    property DefaultPort : Word read FDefaultPort write FDefaultPort;
    property SocksServer : TIpSocksServerInfo
      read FSocksServer write FSocksServer;
  end;

  TIpMultiClient = class(TIpCustomMultiClient)
  public
    procedure CloseAll;
    procedure CloseSocket(Socket : TSocket);
    function ConnectSocket(const RemoteURL : string) : TSocket;
    function PutBlock(Socket : TSocket; var Buf; BufferSize : Integer;
      Disconnect : Boolean) : TIpHandle;
    function PutStream(Socket : TSocket; const Stm : TStream;
      Disconnect : Boolean) : TIpHandle;
    function PutString(Socket : TSocket; const Str : string;
      Disconnect : Boolean) : TIpHandle;
  published
    { Properties from TIpCustomMultiClient }
    property DefaultPort;                                              {!!.01}
    { Properties from TIpSockControl }
    property DebugLog;
    property EventLog;
    property SockInits;
    property SockProtocol;
    property SocksServer;
    property StatusEvents;
    property TimerInterval;
    { Events from TIpSockControl }
    property OnAddressReply;
    property OnError;
    property OnErrorReply;
    property OnIdleTimeout;
    property OnItemSent;
    property OnNameReply;
    property OnPortReply;
    property OnReadLine;
    property OnServiceReply;
    property OnStatus;
  end;

const
  cdMaxClients = 10;

type

  TIpCustomServer = class(TIpMultiSockControl)
  private
    { Property variables }
    FActive : Boolean;
    FHostsAllow : TStringList;
    FHostsDeny : TStringList;
    FMaxClients : DWORD;
    { Event variables }
    FOnReject : TIpRejectEvent;
    { Internal variables }
    cdListenSocket : TIpListenSocket;
    { Property access methods }
    procedure SetHostsAllow(const Value : TStringList);
    procedure SetHostsDeny(const Value : TStringList);
  protected
    procedure Loaded; override;
    { Internal methods }
    procedure cdAcceptConnection; virtual;
    function cdMatchList(const S : string; const List : TStringList) : Boolean;
    procedure scFreeSocket(Socket : TSocket); override;
    { Virtual methods for events }
    procedure DoReject(Reason : TIpRejectType; var Msg : string); virtual;
    { Virtual property methods }
    function GetActiveSockets : Boolean; override;
    procedure SetActive(const Value : Boolean); virtual;
    { Virtual methods to handle incoming socket messages }
    procedure scAccept(Socket : TSocket); override;
    procedure scClose(Socket : TSocket); override;
    procedure scConnect(Socket : TSocket); override;
    procedure scRead(Socket : TSocket); override;
    procedure scWrite(Socket : TSocket); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    { Properties }
    property Active : Boolean
      read FActive write SetActive;
    property HostsAllow : TStringList
      read FHostsAllow write SetHostsAllow;
    property HostsDeny : TStringList
      read FHostsDeny write SetHostsDeny;
    property ListenPort : Word
      read GetDefaultLocalPort write SetDefaultLocalPort;
    property MaxClients : DWORD
      read FMaxClients write FMaxClients default cdMaxClients;
    { Events }
    property OnReject : TIpRejectEvent
      read FOnReject write FOnReject;
  end;

  TIpServer = class(TIpCustomServer)
  public
    procedure CloseAll;                                                {!!.01}
    procedure CloseSocket(Socket : TSocket);                           {!!.01}
    function PutBlock(Socket : TSocket; var Buf; BufferSize : Integer;
      Disconnect : Boolean) : TIpHandle;
    function PutStream(Socket : TSocket; const Stm : TStream;
      Disconnect : Boolean) : TIpHandle;
    function PutString(Socket : TSocket; const Str : string;
      Disconnect : Boolean) : TIpHandle;
  published
    { Properties from TIpSockControl }
    property DebugLog;
    property EventLog;
    property SockInits;
    property SockProtocol;
    property StatusEvents;
    property TimerInterval;
    { Events from TIpSockControl }
    property OnAddressReply;
    property OnError;
    property OnErrorReply;
    property OnIdleTimeout;
    property OnItemSent;
    property OnNameReply;
    property OnPortReply;
    property OnReadLine;
    property OnServiceReply;
    property OnStatus;
    { Properties from TIpCustomServer}
    property Active;
    property HostsAllow;
    property HostsDeny;
    property ListenPort;
    property MaxClients;
    { Events from TIpCustomServer}
    property OnReject;
  end;

  function WinSockErrorMessage(ErrorCode : Integer) : string;

var
  IpWinSockAccess : TIpWinSockAccess;

implementation

uses
  Forms, IpTerm;

const
  DefSockAddrIn : TSockAddrIn = (sa_family : Af_Inet;
    sa_data : #0#0#0#0#0#0#0#0#0#0#0#0#0#0);


{ Return string associated with a given reply type }
function AsyncAPIStr(ReplyType : TIpReplyType) : string;
begin
  case ReplyType of
    rtAddress : Result := 'WsaAsyncGetHostByName';
    rtName : Result := 'WsaAsyncGetHostByAddr';
    rtPort : Result := 'WsaAsyncGetServByName';
    rtService : Result := 'WsaAsyncGetServByPort';
  end;
end;

{ Return string associated with a given reply type }
function MsgAPIStr(SelectEvent : Integer) : string;
begin
  case SelectEvent of
    Fd_Read : Result := 'Fd_Read';
    Fd_Write : Result := 'Fd_Write';
    Fd_Oob : Result := 'Fd_Oob';
    Fd_Accept : Result := 'Fd_Accept';
    Fd_Connect : Result := 'Fd_Connect';
    Fd_Close : Result := 'Fd_Close';
  end;
end;

{ Returns WinSock error message string }
function WinSockErrorMessage(ErrorCode : Integer) : string;
begin
  case ErrorCode of
    wsaEIntr : Result := SwsaEIntr;
    wsaEBadF : Result := SwsaEBadF;
    wsaEAcces : Result := SwsaEAcces;
    wsaEFault : Result := SwsaEFault;
    wsaEInval : Result := SwsaEInval;
    wsaEMFile : Result := SwsaEMFile;
    wsaEWouldBlock : Result := SwsaEWouldBlock;
    wsaEInProgress : Result := SwsaEInProgress;
    wsaEAlReady : Result := SwsaEAlReady;
    wsaENotSock : Result := SwsaENotSock;
    wsaEDestAddrReq : Result := SwsaEDestAddrReq;
    wsaEMsgSize : Result := SwsaEMsgSize;
    wsaEPrototype : Result := SwsaEPrototype;
    wsaENoProtoOpt : Result := SwsaENoProtoOpt;
    wsaEProtoNoSupport : Result := SwsaEProtoNoSupport;
    wsaESocktNoSupport : Result := SwsaESocktNoSupport;
    wsaEOpNotSupp : Result := SwsaEOpNotSupp;
    wsaEPfNoSupport : Result := SwsaEPfNoSupport;
    wsaEAfNoSupport : Result := SwsaEAfNoSupport;
    wsaEAddrInUse : Result := SwsaEAddrInUse;
    wsaEAddrNotAvail : Result := SwsaEAddrNotAvail;
    wsaENetDown : Result := SwsaENetDown;
    wsaENetUnreach : Result := SwsaENetUnreach;
    wsaENetReset : Result := SwsaENetReset;
    wsaEConnAborted : Result := SwsaEConnAborted;
    wsaEConnReset : Result := SwsaEConnReset;
    wsaENoBufs : Result := SwsaENoBufs;
    wsaEIsConn : Result := SwsaEIsConn;
    wsaENotConn : Result := SwsaENotConn;
    wsaEShutDown : Result := SwsaEShutDown;
    wsaETooManyRefs : Result := SwsaETooManyRefs;
    wsaETimedOut : Result := SwsaETimedOut;
    wsaEConnRefused : Result := SwsaEConnRefused;
    wsaELoop : Result := SwsaELoop;
    wsaENameTooLong : Result := SwsaENameTooLong;
    wsaEHostDown : Result := SwsaEHostDown;
    wsaEHostUnreach : Result := SwsaEHostUnreach;
    wsaENotEmpty : Result := SwsaENotEmpty;
    wsaEProcLim : Result := SwsaEProcLim;
    wsaEUsers : Result := SwsaEUsers;
    wsaEDQuot : Result := SwsaEDQuot;
    wsaEStale : Result := SwsaEStale;
    wsaERemote : Result := SwsaERemote;
    wsaSysNotReady : Result := SwsaSysNotReady;
    wsaVerNotSupported : Result := SwsaVerNotSupported;
    wsaNotInitialised : Result := SwsaNotInitialised;
    wsaEDiscOn : Result := SwsaEDiscOn;
    wsaENoMore : Result := SwsaENoMore;
    wsaECancelled : Result := SwsaECancelled;
    wsaEInvalidProcTable : Result := SwsaEInvalidProcTable;
    wsaEInvalidProvider : Result := SwsaEInvalidProvider;
    wsaEProviderFailedInit : Result := SwsaEProviderFailedInit;
    wsaSysCallFailure : Result := SwsaSysCallFailure;
    wsaService_Not_Found : Result := SwsaService_Not_Found;
    wsaType_Not_Found : Result := SwsaType_Not_Found;
    wsa_E_No_More : Result := Swsa_E_No_More;
    wsa_E_Cancelled : Result := Swsa_E_Cancelled;
    wsaERefused : Result := SwsaERefused;
    wsaHost_Not_Found : Result := SwsaHost_Not_Found;
    wsaTry_Again : Result := SwsaTry_Again;
    wsaNo_Recovery : Result := SwsaNo_Recovery;
    wsaNo_Data : Result := SwsaNo_Data;
    wsa_Qos_Receivers : Result := Swsa_Qos_Receivers;
    wsa_Qos_Senders : Result := Swsa_Qos_Senders;
    wsa_Qos_No_Senders : Result := Swsa_Qos_No_Senders;
    wsa_Qos_No_Receivers : Result := Swsa_Qos_No_Receivers;
    wsa_Qos_Request_Confirmed : Result := Swsa_Qos_Request_Confirmed;
    wsa_Qos_Admission_Failure : Result := Swsa_Qos_Admission_Failure;
    wsa_Qos_Policy_Failure : Result := Swsa_Qos_Policy_Failure;
    wsa_Qos_Bad_Style : Result := Swsa_Qos_Bad_Style;
    wsa_Qos_Bad_Object : Result := Swsa_Qos_Bad_Object;
    wsa_Qos_Traffic_Ctrl_Error : Result := Swsa_Qos_Traffic_Ctrl_Error;
    wsa_Qos_Generic_Error : Result := Swsa_Qos_Generic_Error
    else Result := SysErrorMessage(ErrorCode);
  end;
end;

{ EIpWinSockError }

constructor EIpWinSockError.CreateWsError(ErrCode : Integer; const API : string);
begin
  inherited;
  ErrorCode := ErrCode;
  Message := Format(SWinSockErr, [ErrCode, WinSockErrorMessage(ErrCode), API]);
end;

{ TIpWinSockAccess }

constructor TIpWinSockAccess.Create;
var
  Err, Ver, I : Integer;
  OFile : TOFStruct;
  Code : HFile;
begin
  inherited;

  { Attempt to load WinSock module (in order of preference) }
  for I := IpSockVers downto 1 do begin

    {$IFDEF ForceWinSock1}
    if I <> 1 then Continue;
    {$ENDIF}

    { Check to see if the file exists before trying to load it }
    Code := OpenFile(IpVerArray[I].ModuleName, OFile, OF_EXIST);
    if Code = HFILE_ERROR then Continue;

    { If we get this far, we should have a good module -- load it }
    Module := LoadLibrary(IpVerArray[I].ModuleName);
    if Module <> 0 then begin
      Ver := I;
      FLoadedModule := TIpModuleVersion(Ver);
      Err := Self.WsaStartup(IpVerArray[Ver].VerNum, waWsaData);
      if Err <> 0 then
        raise EIpWinSockError.CreateWsError(Err, 'WSAStartup');
      Break;
    end;
  end;
end;

destructor TIpWinSockAccess.Destroy;
var
  TempHandle : TIpHandle;
begin
  if Module <> 0 then begin
    Self.WsaCleanup;
    TempHandle := Module;
    Module := 0;
    FreeLibrary(TempHandle);
  end;
  inherited;
end;

{ Return pointer for the Accept API }
function TIpWinSockAccess.GetAccept : TIpAccept;
const
  ProcName = 'accept';
begin
  LockProperties;
  try
    if not Assigned(FAccept) then
      @FAccept:= GetProcAddress(Module, ProcName);

    Result := FAccept;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Bind API }
function TIpWinSockAccess.GetBind : TIpBind;
const
  ProcName = 'bind';
begin
  LockProperties;
  try
    if not Assigned(FBind) then
      @FBind := GetProcAddress(Module, ProcName);

    Result := FBind;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the CloseSocket API }
function TIpWinSockAccess.GetCloseSocket : TIpCloseSocket;
const
  ProcName = 'closesocket';
begin
  LockProperties;
  try
    if not Assigned(FCloseSocket) then
      @FCloseSocket := GetProcAddress(Module, ProcName);

    Result := FCloseSocket;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Connect API }
function TIpWinSockAccess.GetConnect : TIpConnect;
const
  ProcName = 'connect';
begin
  LockProperties;
  try
    if not Assigned(FConnect) then
      @FConnect := GetProcAddress(Module, ProcName);

    Result := FConnect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetHostByAddr API }
function TIpWinSockAccess.GetGetHostByAddr : TIpGetHostByAddr;
const
  ProcName = 'gethostbyaddr';
begin
  LockProperties;
  try
    if not Assigned(FGetHostByAddr) then
      @FGetHostByAddr := GetProcAddress(Module, ProcName);

    Result := FGetHostByAddr;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetHostByName API }
function TIpWinSockAccess.GetGetHostByName : TIpGetHostByName;
const
  ProcName = 'gethostbyname';
begin
  LockProperties;
  try
    if not Assigned(FGetHostByName) then
      @FGetHostByName := GetProcAddress(Module, ProcName);

    Result := FGetHostByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetHostName API }
function TIpWinSockAccess.GetGetHostName : TIpGetHostName;
const
  ProcName = 'gethostname';
begin
  LockProperties;
  try
    if not Assigned(FGetHostName) then
      @FGetHostName := GetProcAddress(Module, ProcName);

    Result := FGetHostName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetPeerName API }
function TIpWinSockAccess.GetGetPeerName : TIpGetPeerName;
const
  ProcName = 'getpeername';
begin
  LockProperties;
  try
    if not Assigned(FGetPeerName) then
      @FGetPeerName := GetProcAddress(Module, ProcName);

    Result := FGetPeerName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetProtoByName API }
function TIpWinSockAccess.GetGetProtoByName : TIpGetProtoByName;
const
  ProcName = 'getprotobyname';
begin
  LockProperties;
  try
    if not Assigned(FGetProtoByName) then
      @FGetProtoByName := GetProcAddress(Module, ProcName);

    Result := FGetProtoByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetProtoByNumber API }
function TIpWinSockAccess.GetGetProtoByNumber : TIpGetProtoByNumber;
const
  ProcName = 'getprotobynumber';
begin
  LockProperties;
  try
    if not Assigned(FGetProtoByNumber) then
      @FGetProtoByNumber := GetProcAddress(Module, ProcName);

    Result := FGetProtoByNumber;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetServByName API }
function TIpWinSockAccess.GetGetServByName : TIpGetServByName;
const
  ProcName = 'getservbyname';
begin
  LockProperties;
  try
    if not Assigned(FGetServByName) then
      @FGetServByName := GetProcAddress(Module, ProcName);

    Result := FGetServByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetServByPort API }
function TIpWinSockAccess.GetGetServByPort : TIpGetServByPort;
const
  ProcName = 'getservbyport';
begin
  LockProperties;
  try
    if not Assigned(FGetServByPort) then
      @FGetServByPort := GetProcAddress(Module, ProcName);

    Result := FGetServByPort;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetSockName API }
function TIpWinSockAccess.GetGetSockName : TIpGetSockName;
const
  ProcName = 'getsockname';
begin
  LockProperties;
  try
    if not Assigned(FGetSockName) then
      @FGetSockName := GetProcAddress(Module, ProcName);

    Result := FGetSockName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the GetSockOpt API }
function TIpWinSockAccess.GetGetSockOpt : TIpGetSockOpt;
const
  ProcName = 'getsockopt';
begin
  LockProperties;
  try
    if not Assigned(FGetSockOpt) then
      @FGetSockOpt := GetProcAddress(Module, ProcName);

    Result := FGetSockOpt;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the htonl API }
function TIpWinSockAccess.Gethtonl : TIphtonl;
const
  ProcName = 'htonl';
begin
  LockProperties;
  try
    if not Assigned(Fhtonl) then
      @Fhtonl := GetProcAddress(Module, ProcName);

    Result := Fhtonl;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the htons API }
function TIpWinSockAccess.Gethtons : TIphtons;
const
  ProcName = 'htons';
begin
  LockProperties;
  try
    if not Assigned(Fhtons) then
      @Fhtons := GetProcAddress(Module, ProcName);

    Result := Fhtons;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the INet_Addr API }
function TIpWinSockAccess.GetINet_Addr : TIpINet_Addr;
const
  ProcName = 'inet_addr';
begin
  LockProperties;
  try
    if not Assigned(FINet_Addr) then
      @FINet_Addr := GetProcAddress(Module, ProcName);

    Result := FINet_Addr;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the INet_Ntoa API }
function TIpWinSockAccess.GetINet_Ntoa : TIpINet_Ntoa;
const
  ProcName = 'inet_ntoa';
begin
  LockProperties;
  try
    if not Assigned(FINet_Ntoa) then
      @FINet_Ntoa := GetProcAddress(Module, ProcName);

    Result := FINet_Ntoa;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the IOCtlSocket API }
function TIpWinSockAccess.GetIOCtlSocket : TIpIOCtlSocket;
const
  ProcName = 'ioctlsocket';
begin
  LockProperties;
  try
    if not Assigned(FIOCtlSocket) then
      @FIOCtlSocket := GetProcAddress(Module, ProcName);

    Result := FIOCtlSocket;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Listen API }
function TIpWinSockAccess.GetListen : TIpListen;
const
  ProcName = 'listen';
begin
  LockProperties;
  try
    if not Assigned(FListen) then
      @FListen := GetProcAddress(Module, ProcName);

    Result := FListen;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the ntohl API }
function TIpWinSockAccess.Getntohl : TIpntohl;
const
  ProcName = 'ntohl';
begin
  LockProperties;
  try
    if not Assigned(Fntohl) then
      @Fntohl := GetProcAddress(Module, ProcName);

    Result := Fntohl;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the ntohs API }
function TIpWinSockAccess.Getntohs : TIpntohs;
const
  ProcName = 'ntohs';
begin
  LockProperties;
  try
    if not Assigned(Fntohs) then
      @Fntohs := GetProcAddress(Module, ProcName);

    Result := Fntohs;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Recv API }
function TIpWinSockAccess.GetRecv : TIpRecv;
const
  ProcName = 'recv';
begin
  LockProperties;
  try
    if not Assigned(FRecv) then
      @FRecv := GetProcAddress(Module, ProcName);

    Result := FRecv;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the RecvFrom API }
function TIpWinSockAccess.GetRecvFrom : TIpRecvFrom;
const
  ProcName = 'recvfrom';
begin
  LockProperties;
  try
    if not Assigned(FRecvFrom) then
      @FRecvFrom := GetProcAddress(Module, ProcName);

    Result := FRecvFrom;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Select API }
function TIpWinSockAccess.GetSelect : TIpSelect;
const
  ProcName = 'select';
begin
  LockProperties;
  try
    if not Assigned(FSelect) then
      @FSelect := GetProcAddress(Module, ProcName);

    Result := FSelect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Send API }
function TIpWinSockAccess.GetSend : TIpSend;
const
  ProcName = 'send';
begin
  LockProperties;
  try
    if not Assigned(FSend) then
      @FSend := GetProcAddress(Module, ProcName);

    Result := FSend;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the SendTo API }
function TIpWinSockAccess.GetSendTo : TIpSendTo;
const
  ProcName = 'sendto';
begin
  LockProperties;
  try
    if not Assigned(FSendTo) then
      @FSendTo := GetProcAddress(Module, ProcName);

    Result := FSendTo;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the SetSockOpt API }
function TIpWinSockAccess.GetSetSockOpt : TIpSetSockOpt;
const
  ProcName = 'setsockopt';
begin
  LockProperties;
  try
    if not Assigned(FSetSockOpt) then
      @FSetSockOpt := GetProcAddress(Module, ProcName);

    Result := FSetSockOpt;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Shutdown API }
function TIpWinSockAccess.GetShutdown : TIpShutdown;
const
  ProcName = 'shutdown';
begin
  LockProperties;
  try
    if not Assigned(FShutdown) then
      @FShutdown := GetProcAddress(Module, ProcName);

    Result := FShutdown;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the Socket API }
function TIpWinSockAccess.GetSocket : TIpSocket;
const
  ProcName = 'socket';
begin
  LockProperties;
  try
    if not Assigned(FSocket) then
      @FSocket := GetProcAddress(Module, ProcName);

    Result := FSocket;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncGetHostByAddr API }
function TIpWinSockAccess.GetWsaAsyncGetHostByAddr : TIpWsaAsyncGetHostByAddr;
const
  ProcName = 'WSAAsyncGetHostByAddr';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncGetHostByAddr) then
      @FWsaAsyncGetHostByAddr := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncGetHostByAddr;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncGetHostByName API }
function TIpWinSockAccess.GetWsaAsyncGetHostByName : TIpWsaAsyncGetHostByName;
const
  ProcName = 'WSAAsyncGetHostByName';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncGetHostByName) then
      @FWsaAsyncGetHostByName := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncGetHostByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncGetProtoByName API }
function TIpWinSockAccess.GetWsaAsyncGetProtoByName : TIpWsaAsyncGetProtoByName;
const
  ProcName = 'WSAAsyncGetProtoByName';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncGetProtoByName) then
      @FWsaAsyncGetProtoByName := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncGetProtoByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncGetProtoByNumber API }
function TIpWinSockAccess.GetWsaAsyncGetProtoByNumber : TIpWsaAsyncGetProtoByNumber;
const
  ProcName = 'WSAAsyncGetProtoByNumber';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncGetProtoByNumber) then
      @FWsaAsyncGetProtoByNumber := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncGetProtoByNumber;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncGetServByName API }
function TIpWinSockAccess.GetWsaAsyncGetServByName : TIpWsaAsyncGetServByName;
const
  ProcName = 'WSAAsyncGetServByName';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncGetServByName) then
      @FWsaAsyncGetServByName := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncGetServByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncGetServByPort API }
function TIpWinSockAccess.GetWsaAsyncGetServByPort : TIpWsaAsyncGetServByPort;
const
  ProcName = 'WSAAsyncGetServByPort';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncGetServByPort) then
      @FWsaAsyncGetServByPort := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncGetServByPort;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAsyncSelect API }
function TIpWinSockAccess.GetWsaAsyncSelect : TIpWsaAsyncSelect;
const
  ProcName = 'WSAAsyncSelect';
begin
  LockProperties;
  try
    if not Assigned(FWsaAsyncSelect) then
      @FWsaAsyncSelect := GetProcAddress(Module, ProcName);

    Result := FWsaAsyncSelect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaCancelAsyncRequest API }
function TIpWinSockAccess.GetWsaCancelAsyncRequest : TIpWsaCancelAsyncRequest;
const
  ProcName = 'WSACancelAsyncRequest';
begin
  LockProperties;
  try
    if not Assigned(FWsaCancelAsyncRequest) then
      @FWsaCancelAsyncRequest := GetProcAddress(Module, ProcName);

    Result := FWsaCancelAsyncRequest;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaCancelBlockingCall API }
function TIpWinSockAccess.GetWsaCancelBlockingCall : TIpWsaCancelBlockingCall;
const
  ProcName = 'WSACancelBlockingCall';
begin
  LockProperties;
  try
    if not Assigned(FWsaCancelBlockingCall) then
      @FWsaCancelBlockingCall := GetProcAddress(Module, ProcName);

    Result := FWsaCancelBlockingCall;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaCleanup API }
function TIpWinSockAccess.GetWsaCleanup : TIpWsaCleanup;
const
  ProcName = 'WSACleanup';
begin
  LockProperties;
  try
    if not Assigned(FWsaCleanup) then
      @FWsaCleanup := GetProcAddress(Module, ProcName);

    Result := FWsaCleanup;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaGetLastError API }
function TIpWinSockAccess.GetWsaGetLastError : TIpWsaGetLastError;
const
  ProcName = 'WSAGetLastError';
begin
  LockProperties;
  try
    if not Assigned(FWsaGetLastError) then
      @FWsaGetLastError := GetProcAddress(Module, ProcName);

    Result := FWsaGetLastError;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaIsBlocking API }
function TIpWinSockAccess.GetWsaIsBlocking : TIpWsaIsBlocking;
const
  ProcName = 'WSAIsBlocking';
begin
  LockProperties;
  try
    if not Assigned(FWsaIsBlocking) then
      @FWsaIsBlocking := GetProcAddress(Module, ProcName);

    Result := FWsaIsBlocking;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSetBlockingHook API }
function TIpWinSockAccess.GetWsaSetBlockingHook : TIpWsaSetBlockingHook;
const
  ProcName = 'WSASetBlockingHook';
begin
  LockProperties;
  try
    if not Assigned(FWsaSetBlockingHook) then
      @FWsaSetBlockingHook := GetProcAddress(Module, ProcName);

    Result := FWsaSetBlockingHook;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSetLastError API }
function TIpWinSockAccess.GetWsaSetLastError : TIpWsaSetLastError;
const
  ProcName = 'WSASetLastError';
begin
  LockProperties;
  try
    if not Assigned(FWsaSetLastError) then
      @FWsaSetLastError := GetProcAddress(Module, ProcName);

    Result := FWsaSetLastError;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaStartup API }
function TIpWinSockAccess.GetWsaStartup : TIpWsaStartup;
const
  ProcName = 'WSAStartup';
begin
  LockProperties;
  try
    if not Assigned(FWsaStartup) then
      @FWsaStartup := GetProcAddress(Module, ProcName);

    Result := FWsaStartup;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaUnhookBlockingHook API }
function TIpWinSockAccess.GetWsaUnhookBlockingHook : TIpWsaUnhookBlockingHook;
const
  ProcName = 'WSAUnhookBlockingHook';
begin
  LockProperties;
  try
    if not Assigned(FWsaUnhookBlockingHook) then
      @FWsaUnhookBlockingHook := GetProcAddress(Module, ProcName);

    Result := FWsaUnhookBlockingHook;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAccept API }
function TIpWinSockAccess.GetWsaAccept : TIpWsaAccept;
const
  ProcName = 'WSAAccept';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaAccept) then
      @FWsaAccept := GetProcAddress(Module, ProcName);

    Result := FWsaAccept;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaCloseEvent API }
function TIpWinSockAccess.GetWsaCloseEvent : TIpWsaCloseEvent;
const
  ProcName = 'WSACloseEvent';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaCloseEvent) then
      @FWsaCloseEvent := GetProcAddress(Module, ProcName);

    Result := FWsaCloseEvent;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaConnect API }
function TIpWinSockAccess.GetWsaConnect : TIpWsaConnect;
const
  ProcName = 'WSAConnect';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaConnect) then
      @FWsaConnect := GetProcAddress(Module, ProcName);

    Result := FWsaConnect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaCreateEvent API }
function TIpWinSockAccess.GetWsaCreateEvent : TIpWsaCreateEvent;
const
  ProcName = 'WSACreateEvent';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaCreateEvent) then
      @FWsaCreateEvent := GetProcAddress(Module, ProcName);

    Result := FWsaCreateEvent;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaDuplicateSocket API }
function TIpWinSockAccess.GetWsaDuplicateSocket : TIpWsaDuplicateSocket;
const
  ProcName = 'WSADuplicateSocketA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaDuplicateSocket) then
      @FWsaDuplicateSocket := GetProcAddress(Module, ProcName);

    Result := FWsaDuplicateSocket;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaEnumNetworkEvents API }
function TIpWinSockAccess.GetWsaEnumNetworkEvents : TIpWsaEnumNetworkEvents;
const
  ProcName = 'WSAEnumNetworkEvents';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaEnumNetworkEvents) then
      @FWsaEnumNetworkEvents := GetProcAddress(Module, ProcName);

    Result := FWsaEnumNetworkEvents;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaEnumProtocols API }
function TIpWinSockAccess.GetWsaEnumProtocols : TIpWsaEnumProtocols;
const
  ProcName = 'WSAEnumProtocolsA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaEnumProtocols) then
      @FWsaEnumProtocols := GetProcAddress(Module, ProcName);

    Result := FWsaEnumProtocols;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaEventSelect API }
function TIpWinSockAccess.GetWsaEventSelect : TIpWsaEventSelect;
const
  ProcName = 'WSAEventSelect';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaEventSelect) then
      @FWsaEventSelect := GetProcAddress(Module, ProcName);

    Result := FWsaEventSelect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaGetOverlappedResult API }
function TIpWinSockAccess.GetWsaGetOverlappedResult : TIpWsaGetOverlappedResult;
const
  ProcName = 'WSAGetOverlappedResult';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaGetOverlappedResult) then
      @FWsaGetOverlappedResult := GetProcAddress(Module, ProcName);

    Result := FWsaGetOverlappedResult;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaGetQosByName API }
function TIpWinSockAccess.GetWsaGetQosByName : TIpWsaGetQosByName;
const
  ProcName = 'WSAGetQOSByName';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaGetQosByName) then
      @FWsaGetQosByName := GetProcAddress(Module, ProcName);

    Result := FWsaGetQosByName;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaHtonl API }
function TIpWinSockAccess.GetWsaHtonl : TIpWsaHtonl;
const
  ProcName = 'WSAHtonl';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaHtonl) then
      @FWsaHtonl := GetProcAddress(Module, ProcName);

    Result := FWsaHtonl;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaHtons API }
function TIpWinSockAccess.GetWsaHtons : TIpWsaHtons;
const
  ProcName = 'WSAHtons';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaHtons) then
      @FWsaHtons := GetProcAddress(Module, ProcName);

    Result := FWsaHtons;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaIoctl API }
function TIpWinSockAccess.GetWsaIoctl : TIpWsaIoctl;
const
  ProcName = 'WSAIoctl';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaIoctl) then
      @FWsaIoctl := GetProcAddress(Module, ProcName);

    Result := FWsaIoctl;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaJoinLeaf API }
function TIpWinSockAccess.GetWsaJoinLeaf : TIpWsaJoinLeaf;
const
  ProcName = 'WSAJoinLeaf';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaJoinLeaf) then
      @FWsaJoinLeaf := GetProcAddress(Module, ProcName);

    Result := FWsaJoinLeaf;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaNtohl API }
function TIpWinSockAccess.GetWsaNtohl : TIpWsaNtohl;
const
  ProcName = 'WSANtohl';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaNtohl) then
      @FWsaNtohl := GetProcAddress(Module, ProcName);

    Result := FWsaNtohl;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaNtohs API }
function TIpWinSockAccess.GetWsaNtohs : TIpWsaNtohs;
const
  ProcName = 'WSANtohs';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaNtohs) then
      @FWsaNtohs := GetProcAddress(Module, ProcName);

    Result := FWsaNtohs;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaRecv API }
function TIpWinSockAccess.GetWsaRecv : TIpWsaRecv;
const
  ProcName = 'WSARecv';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaRecv) then
      @FWsaRecv := GetProcAddress(Module, ProcName);

    Result := FWsaRecv;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaRecvDisconnect API }
function TIpWinSockAccess.GetWsaRecvDisconnect : TIpWsaRecvDisconnect;
const
  ProcName = 'WSARecvDisconnect';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaRecvDisconnect) then
      @FWsaRecvDisconnect := GetProcAddress(Module, ProcName);

    Result := FWsaRecvDisconnect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaRecvFrom API }
function TIpWinSockAccess.GetWsaRecvFrom : TIpWsaRecvFrom;
const
  ProcName = 'WSARecvFrom';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaRecvFrom) then
      @FWsaRecvFrom := GetProcAddress(Module, ProcName);

    Result := FWsaRecvFrom;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaResetEvent API }
function TIpWinSockAccess.GetWsaResetEvent : TIpWsaResetEvent;
const
  ProcName = 'WSAResetEvent';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaResetEvent) then
      @FWsaResetEvent := GetProcAddress(Module, ProcName);

    Result := FWsaResetEvent;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSend API }
function TIpWinSockAccess.GetWsaSend : TIpWsaSend;
const
  ProcName = 'WSASend';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaSend) then
      @FWsaSend := GetProcAddress(Module, ProcName);

    Result := FWsaSend;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSendDisconnect API }
function TIpWinSockAccess.GetWsaSendDisconnect : TIpWsaSendDisconnect;
const
  ProcName = 'WSASendDisconnect';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaSendDisconnect) then
      @FWsaSendDisconnect := GetProcAddress(Module, ProcName);

    Result := FWsaSendDisconnect;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSendTo API }
function TIpWinSockAccess.GetWsaSendTo : TIpWsaSendTo;
const
  ProcName = 'WSASendTo';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaSendTo) then
      @FWsaSendTo := GetProcAddress(Module, ProcName);

    Result := FWsaSendTo;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSetEvent API }
function TIpWinSockAccess.GetWsaSetEvent : TIpWsaSetEvent;
const
  ProcName = 'WSASetEvent';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaSetEvent) then
      @FWsaSetEvent := GetProcAddress(Module, ProcName);

    Result := FWsaSetEvent;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSocket API }
function TIpWinSockAccess.GetWsaSocket : TIpWsaSocket;
const
  ProcName = 'WSASocketA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaSocket) then
      @FWsaSocket := GetProcAddress(Module, ProcName);

    Result := FWsaSocket;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaWaitForMultipleEvents API }
function TIpWinSockAccess.GetWsaWaitForMultipleEvents : TIpWsaWaitForMultipleEvents;
const
  ProcName = 'WSAWaitForMultipleEvents';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaWaitForMultipleEvents) then
      @FWsaWaitForMultipleEvents := GetProcAddress(Module, ProcName);

    Result := FWsaWaitForMultipleEvents;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaAddressToString API }
function TIpWinSockAccess.GetWsaAddressToString : TIpWsaAddressToString;
const
  ProcName = 'WSAAddressToStringA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaAddressToString) then
      @FWsaAddressToString := GetProcAddress(Module, ProcName);

    Result := FWsaAddressToString;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaStringToAddress API }
function TIpWinSockAccess.GetWsaStringToAddress : TIpWsaStringToAddress;
const
  ProcName = 'WSAStringToAddressA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaStringToAddress) then
      @FWsaStringToAddress := GetProcAddress(Module, ProcName);

    Result := FWsaStringToAddress;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaLookupServiceBegin API }
function TIpWinSockAccess.GetWsaLookupServiceBegin : TIpWsaLookupServiceBegin;
const
  ProcName = 'WSALookupServiceBeginA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaLookupServiceBegin) then
      @FWsaLookupServiceBegin := GetProcAddress(Module, ProcName);

    Result := FWsaLookupServiceBegin;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaLookupServiceNext API }
function TIpWinSockAccess.GetWsaLookupServiceNext : TIpWsaLookupServiceNext;
const
  ProcName = 'WSALookupServiceNextA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaLookupServiceNext) then
      @FWsaLookupServiceNext := GetProcAddress(Module, ProcName);

    Result := FWsaLookupServiceNext;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaLookupServiceEnd API }
function TIpWinSockAccess.GetWsaLookupServiceEnd : TIpWsaLookupServiceEnd;
const
  ProcName = 'WSALookupServiceEnd';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaLookupServiceEnd) then
      @FWsaLookupServiceEnd := GetProcAddress(Module, ProcName);

    Result := FWsaLookupServiceEnd;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaInstallServiceClass API }
function TIpWinSockAccess.GetWsaInstallServiceClass : TIpWsaInstallServiceClass;
const
  ProcName = 'WSAInstallServiceClassA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaInstallServiceClass) then
      @FWsaInstallServiceClass := GetProcAddress(Module, ProcName);

    Result := FWsaInstallServiceClass;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaRemoveServiceClass API }
function TIpWinSockAccess.GetWsaRemoveServiceClass : TIpWsaRemoveServiceClass;
const
  ProcName = 'WSARemoveServiceClass';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaRemoveServiceClass) then
      @FWsaRemoveServiceClass := GetProcAddress(Module, ProcName);

    Result := FWsaRemoveServiceClass;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaGetServiceClassInfo API }
function TIpWinSockAccess.GetWsaGetServiceClassInfo : TIpWsaGetServiceClassInfo;
const
  ProcName = 'WSAGetServiceClassInfoA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaGetServiceClassInfo) then
      @FWsaGetServiceClassInfo := GetProcAddress(Module, ProcName);

    Result := FWsaGetServiceClassInfo;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaEnumNameSpaceProviders API }
function TIpWinSockAccess.GetWsaEnumNameSpaceProviders : TIpWsaEnumNameSpaceProviders;
const
  ProcName = 'WSAEnumNameSpaceProvidersA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaEnumNameSpaceProviders) then
      @FWsaEnumNameSpaceProviders := GetProcAddress(Module, ProcName);

    Result := FWsaEnumNameSpaceProviders;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaGetServiceClassNameByClassId API }
function TIpWinSockAccess.GetWsaGetServiceClassNameByClassId : TIpWsaGetServiceClassNameByClassId;
const
  ProcName = 'WSAGetServiceClassNameByClassIdA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaGetServiceClassNameByClassId) then
      @FWsaGetServiceClassNameByClassId := GetProcAddress(Module, ProcName);

    Result := FWsaGetServiceClassNameByClassId;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaSetService API }
function TIpWinSockAccess.GetWsaSetService : TIpWsaSetService;
const
  ProcName = 'WSASetServiceA';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaSetService) then
      @FWsaSetService := GetProcAddress(Module, ProcName);

    Result := FWsaSetService;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ Return pointer for the WsaProviderConfigChange API }
function TIpWinSockAccess.GetWsaProviderConfigChange : TIpWsaProviderConfigChange;
const
  ProcName = 'WSAProviderConfigChange';
begin
  LockProperties;
  try
    Result := nil;

    if FLoadedModule < wvWinSock2 then
      raise EIpAccessException.CreateFmt(SNoWinSock2Err, [ProcName]);

    if not Assigned(FWsaProviderConfigChange) then
      @FWsaProviderConfigChange := GetProcAddress(Module, ProcName);

    Result := FWsaProviderConfigChange;

    if not Assigned(Result) then
      raise EIpAccessException.CreateFmt(SAccessProcErr,
        [ProcName, SysErrorMessage(GetLastError)]);

  finally
    UnlockProperties;
  end;
end;

{ TIpBaseLog }

{ Create instance of TIpBaseLog }
constructor TIpBaseLog.Create(Owner : TIpSockControl);
begin
  inherited Create;
  blOwner := Owner;
  InitializeCriticalSection(blLogCS);
end;

{ Destroy instance of TIpBaseLog }
destructor TIpBaseLog.Destroy;
begin
  DeleteCriticalSection(blLogCS);
  inherited;
end;

{ Enters TIpBaseLog critical section }
procedure TIpBaseLog.LockLog;
begin
  if IsMultiThread then
    EnterCriticalSection(blLogCS);
end;

{ Leaves TIpBaseLog critical section }
procedure TIpBaseLog.UnlockLog;
begin
  if IsMultiThread then
    LeaveCriticalSection(blLogCS);
end;

{ Get Enabled property }
function TIpBaseLog.GetEnabled : Boolean;
begin
  LockLog;
  try
    Result := FEnabled;
  finally
    UnlockLog;
  end;
end;

{ Get FileName property }
function TIpBaseLog.GetFileName : TFileName;
begin
  LockLog;
  try
    Result := FFileName;
  finally
    UnlockLog;
  end;
end;

{ Set Enabled property }
procedure TIpBaseLog.SetEnabled(const Value : Boolean);
begin
  LockLog;
  try
    FEnabled := Value;
  finally
    UnlockLog;
  end;
end;

{ Set FileName property }
procedure TIpBaseLog.SetFileName(const Value : TFileName);
begin
  LockLog;
  try
    FFileName := Value;
  finally
    UnlockLog;
  end;
end;

{ TIpDebugLog }

constructor TIpDebugLog.Create(Owner : TIpSockControl);
begin
  inherited;
  FBufferSize := 65536;
  FFileName := 'debug.log';
  dlTimeBase := GetTickCount;
end;

destructor TIpDebugLog.Destroy;
begin
  FreeMem(dlBuffer);
  FreeMem(dlTempBuffer);

  inherited;
end;

{ AddDebugEntry notes:                                                }
{                                                                     }
{ This is designed to be a flexible and extendable debug logging      }
{ facility that allows new components to add their own customized     }
{ logging. It works together with the GetLogString class method       }
{ introduced in TIpBaseComponent.                                     }
{                                                                     }
{ deClass -- Class of component adding the entry (Self.ClassType)     }
{ When dumping the debug log to disk, this class will be expected to  }
{ return a string for the log based on the data contained in S, D1,   }
{ D2 and D3.                                                          }
{                                                                     }
{ S is intended to be used for the socket handle, which is probably   }
{ an important thing to include if the debug log may have entries     }
{ for multiple sockets (like a server, for instance). If the log      }
{ string is being generated by the component via GetLogString, S can  }
{ be used for "whatever" if a socket handle is not required (in other }
{ words, S is not treated internally in any special way by the log.   }
{                                                                     }
{ D1, D2, D3 are "info" fields to be used in deClass's overridden     }
{ GetLogString class method. deClass will then presumably use the     }
{ info to format a string and pass it back to the debug log.          }
{                                                                     }
{ Special case: If the high bit of D1 is set, D2 becomes a pointer    }
{ to data, and D3 is the size of the data. Make *sure* the high bit   }
{ isn't set unless you are using this special situation.              }
{                                                                     }
{ Obviously, every class that might get passed in for deClass needs   }
{ to have an overridden GetLogString method that understands the      }
{ values it's passing in for S, D1, D2 & D3                           }
{                                                                     }
{ If you just have a simple case for logging that probably won't get  }
{ used that often, consider adding entries with the WriteDebugString  }
{ method.                                                             }
procedure TIpDebugLog.AddDebugEntry(const deClass : TIpComponentClass; const S, D1, D2, D3 : DWORD);
var
  DebugEntry : TIpDebugRec;
  EntryPtr : PIpDebugRec;
  SizeReq, TimeMrk, ChunkSize : DWORD;
  HasData : Boolean;
begin
  LockLog;
  try
    { Bail if we're not logging }
    if not Enabled then Exit;

    TimeMrk := GetTickCount;

    { Determine size needed }
    SizeReq := SizeOf(TIpDebugRec);
    if (D1 and $80000000) = $80000000 then begin
      HasData := True;
      Inc(SizeReq, D3);
    end else begin
      HasData := False;
    end;

    { Bail if SizeReq is bigger than the whole buffer }
    if SizeReq > FBufferSize then Exit;

    { Make more room in buffer if necessary }
    while (SizeReq > BufferFree) and dlPopDebugEntry(DebugEntry) do ;

    { Do we need to wrap this entry? }
    if (dlBufferTail + SizeReq) <= FBufferSize then begin

      { Wrap not required, write directly to dlBuffer }
      EntryPtr := @dlBuffer[dlBufferTail];
      EntryPtr.drClass := deClass;
      EntryPtr.drTime := TimeMrk;
      EntryPtr.drSckt := S;
      EntryPtr.drData1 := D1;
      EntryPtr.drData2 := D2;
      EntryPtr.drData3 := D3;

      { Write add'l data if necessary }
      if HasData then begin
        Move(Pointer(D2)^, dlBuffer[dlBufferTail + SizeOf(TIpDebugRec)], D3);
      end;
      Inc(dlBufferTail, SizeReq);

      { Fix tail if necessary }
      if dlBufferTail = FBufferSize then
        dlBufferTail := 0;

    end else begin

      { Wrap required, use temp buffer }
      dlCheckTempSize(SizeReq);

      EntryPtr := @dlTempBuffer[0];
      EntryPtr.drClass := deClass;
      EntryPtr.drTime := TimeMrk;
      EntryPtr.drSckt := S;
      EntryPtr.drData1 := D1;
      EntryPtr.drData2 := D2;
      EntryPtr.drData3 := D3;

      { Write add'l data if necessary }
      if HasData then begin
        Move(Pointer(D2)^, dlTempBuffer[SizeOf(TIpDebugRec)], D3);
      end;

      { Move first half }
      ChunkSize := FBufferSize - dlBufferTail;
      Move(dlTempBuffer[0], dlBuffer[dlBufferTail], ChunkSize);

      { Move second half }
      Move(dlTempBuffer[ChunkSize], dlBuffer[0], SizeReq - ChunkSize);

      { Set tail }
      dlBufferTail := SizeReq - ChunkSize;
    end;
  finally
    UnlockLog;
  end;
end;

{ Clears all data from buffer (does not write data to disk) }
procedure TIpDebugLog.ClearBuffer;
begin
  LockLog;
  try
    dlBufferHead := 0;
    dlBufferTail := 0;
  finally
    UnlockLog;
  end;
end;

{ Verifies the size of the temp buffer }
procedure TIpDebugLog.dlCheckTempSize(SizeReq : DWORD);
begin
  if (SizeReq > dlTempSize) then begin
    ReallocMem(dlTempBuffer, SizeReq);
    dlTempSize := SizeReq;
  end;
end;

{ Returns string describing OS version }
function GetOSVersion : string;
var
  OSVersion : TOSVersionInfo;
begin
  OSVersion.dwOSVersionInfoSize := SizeOf(OSVersion);
  GetVersionEx(OSVersion);
  case OSVersion.dwPlatformID of
    VER_PLATFORM_WIN32s        : Result := 'Win32s on Windows ';
    VER_PLATFORM_WIN32_WINDOWS : Result := 'Win32 on Windows ';
    VER_PLATFORM_WIN32_NT      : Result := 'Windows NT ';
    else Result := 'Unknown';
  end;
  Result := Result + IntToStr(OSVersion.dwMajorVersion) + '.' +
    IntToStr(OSVersion.dwMinorVersion) + ' ' + StrPas(OSVersion.szCSDVersion);
end;

{ Generates a file header for the log }
function TIpDebugLog.dlDoFileHeader : string;
var Cmp : string;
begin
  { Compiler version }
  Cmp := 'Unknown Compiler';
  {$IFDEF CBuilder}
  {$IFDEF VER110} Cmp := 'C++ Builder 3'; {$ENDIF}
  {$IFDEF VER125} Cmp := 'C++ Builder 4'; {$ENDIF}
  {$IFDEF VER130} Cmp := 'C++ Builder 5'; {$ENDIF}
  {$ELSE}
  {$IFDEF VER100} Cmp := 'Delphi 3'; {$ENDIF}
  {$IFDEF VER120} Cmp := 'Delphi 4'; {$ENDIF}
  {$IFDEF VER130} Cmp := 'Delphi 5'; {$ENDIF}
  {$IFDEF VER140} Cmp := 'Delphi 6'; {$ENDIF}
  {$IFDEF VER150} Cmp := 'Delphi 7'; {$ENDIF}
  {$ENDIF}

  { Create / format header string }
  Result := 'TurboPower Internet Professional ' + IpShortVersion +
    ' compiled with ' + Cmp + IpCRLF +
    'Operating System : ' + GetOSVersion + IpCRLF +
    'Description : ' + blOwner.wsDescription + IpCRLF +
    Format('Negotiated Version : $%4.4x', [blOwner.wsVersion]) + IpCRLF +
    'System Status : ' + blOwner.wsSystemStatus + IpCRLF +
    '=============================================================================' +
    IpCRLF + IpCRLF;
end;

{ Pop debug record from log, return False if no record to return }
function TIpDebugLog.dlPopDebugEntry(var DebugRec : TIpDebugRec) : Boolean;
type
  BytesArray = array[0..SizeOf(TIpDebugRec)-1] of Byte;
var
  Bytes : BytesArray absolute DebugRec;
  ChunkSize : DWORD;
begin
  LockLog;
  try
    { Check for empty buffer }
    if (dlBufferHead = dlBufferTail) then begin
      Result := False;
      Exit;
    end else begin
      Result := True;
    end;

    { Check to see if debug record wraps }
    if (dlBufferHead + SizeOf(TIpDebugRec)) <= FBufferSize then begin

      { No wrap, copy directly over }
      Move(dlBuffer[dlBufferHead], DebugRec, SizeOf(DebugRec));
      Inc(dlBufferHead, SizeOf(DebugRec));

      { Fix head if needed }
      if (dlBufferHead = FBufferSize) then dlBufferHead := 0;
    end else begin

      { Need to deal with wrap -- copy first half }
      ChunkSize := (FBufferSize - dlBufferHead);
      Move(dlBuffer[dlBufferHead], Bytes[0], ChunkSize);

      { Copy second half }
      Move(dlBuffer[0], Bytes[ChunkSize], (SizeOf(DebugRec) - ChunkSize));
      dlBufferHead := SizeOf(DebugRec) - ChunkSize;
    end;

    { Do we have data? If so, deal with it }
    if (DebugRec.drData1 and $80000000) = $80000000 then begin

      { Check to see if debug data wraps }
      if (dlBufferHead + DebugRec.drData3) <= FBufferSize then begin

        { No wrap -- point D2 to buffer }
        DebugRec.drData2 := DWORD(@dlBuffer[dlBufferHead]);
        Inc(dlBufferHead, DebugRec.drData3);
      end else begin

        { Wrap -- copy first half to temp buffer }
        dlCheckTempSize(DebugRec.drData3);
        ChunkSize := (FBufferSize - dlBufferHead);
        Move(dlBuffer[dlBufferHead], dlTempBuffer[0], ChunkSize);

        { Copy second half }
        Move(dlBuffer[0], dlTempBuffer[ChunkSize], (DebugRec.drData3 - ChunkSize));
        DebugRec.drData2 := DWORD(@dlTempBuffer[0]);
        dlBufferHead := DebugRec.drData3 - ChunkSize;
      end;
    end

  finally
    UnlockLog;
  end;
end;

{!!.01 - added}
{ Return telnet string }
function TIpDebugLog.dlTelnetString(const S, D2, D3 : DWORD) : string;
type
  TCastRec = packed record
    A : AnsiChar;
    B : AnsiChar;
    C : AnsiChar;
    D : AnsiChar;
  end;
var
  Dir, Cmd, Opt : string;
  TelCmd, TelOpt : AnsiChar;
begin
  case D2 of
    slWrite : Dir := 'sent.';
    slRead  : Dir := 'received.';
  else Dir := Format('[%8.8x]', [D2]);
  end;

  TelCmd := TCastRec(D3).B;
  case TelCmd of
    TELNET_DONT : Cmd := 'DON''T';
    TELNET_DO   : Cmd := 'DO';
    TELNET_WONT : Cmd := 'WON''T';
    TELNET_WILL : Cmd := 'WILL';
  else Cmd := Format('[%d]', [Byte(TelCmd)]);
  end;

  TelOpt := TCastRec(D3).A;
  case TelOpt of
    TELNETOPT_BINARY          : Opt := 'transmit binary';
    TELNETOPT_ECHO            : Opt := 'echo';
    TELNETOPT_RECONN          : Opt := 'reconnect';
    TELNETOPT_SUPGA           : Opt := 'suppress go-ahead';
    TELNETOPT_AMS             : Opt := 'approx msg size';
    TELNETOPT_STATUS          : Opt := 'status';
    TELNETOPT_MARK            : Opt := 'timing mark';
    TELNETOPT_RCTE            : Opt := 'remote trans & echo';
    TELNETOPT_OLW             : Opt := 'output line width';
    TELNETOPT_OPS             : Opt := 'output page size';
    TELNETOPT_NAOCRD          : Opt := 'output C/R disp';
    TELNETOPT_NAOHTS          : Opt := 'output horz tabs';
    TELNETOPT_NAOHTD          : Opt := 'output horz tab disp';
    TELNETOPT_NAOFFD          : Opt := 'output FF disp';
    TELNETOPT_NAOVTS          : Opt := 'output vert tabs';
    TELNETOPT_NAOVTD          : Opt := 'output vert tab disp';
    TELNETOPT_NAOLFD          : Opt := 'output linefeed disp';
    TELNETOPT_EXTEND_ASCII    : Opt := 'extended ASCII';
    TELNETOPT_LOGOUT          : Opt := 'logout';
    TELNETOPT_BM              : Opt := 'byte macro';
    TELNETOPT_DET             : Opt := 'data entry terminal subcommands';
    TELNETOPT_SUPDUP          : Opt := 'SUPDUP';
    TELNETOPT_SUPDUP_OUTPUT   : Opt := 'SUPDUP output';
    TELNETOPT_SEND_LOCATION   : Opt := 'send location';
    TELNETOPT_TERM            : Opt := 'terminal type';
    TELNETOPT_EOR             : Opt := 'end of record';
    TELNETOPT_TUID            : Opt := 'TACACS user ID';
    TELNETOPT_OUTMRK          : Opt := 'output marking';
    TELNETOPT_TTYLOC          : Opt := 'terminal loc num';
    TELNETOPT_3270_REGIME     : Opt := 'Telnet3270 Regime';
    TELNETOPT_X3PAD           : Opt := 'X.3-Pad operations';
    TELNETOPT_NAWS            : Opt := 'negotiate about window size';
    TELNETOPT_SPEED           : Opt := 'terminal speed';
    TELNETOPT_FLOW            : Opt := 'toggle flow control';
    TELNETOPT_LINEMODE        : Opt := 'line mode';
    TELNETOPT_XDISPLOC        : Opt := 'X display location';
    TELNETOPT_ENVIRON         : Opt := 'environment variables';
    TELNETOPT_AUTH            : Opt := 'authentication';
    TELNETOPT_ENCRYPT         : Opt := 'encryption option';
    TELNETOPT_NEWENVIRON      : Opt := 'new environment variables';
    TELNETOPT_TN3270E         : Opt := 'TN3270E';
    TELNETOPT_XAUTH           : Opt := 'XAUTH';
    TELNETOPT_CHARSET         : Opt := 'character set';
    TELNETOPT_RSP             : Opt := 'telnet remote serial port (RSP)';
    TELNETOPT_COM_PORT_OPTION : Opt := 'com port control option';
    TELNETOPT_SUP_LOC_ECHO    : Opt := 'telnet suppress local echo';
    TELNETOPT_START_TLS       : Opt := 'telnet start TLS';
    TELNETOPT_KERMIT          : Opt := 'KERMIT';
    TELNETOPT_SEND_URL        : Opt := 'SEND-URL';
    TELNETOPT_FORWARD_X       : Opt := 'FORWARD_X';
  else Opt := Format('[%d]', [Byte(TelOpt)]);
  end;

  Result := Format('[%d] Telnet %s %s %s', [S, Cmd, Opt, Dir]);
end;

{ Return time stamp string }
function TIpDebugLog.dlTimeStamp(Mark : DWORD) : string;
begin
  Result := Format('%07.7d : ', [Mark - dlTimeBase]);
  Insert('.', Result, 5);
end;

{ Dumps log file to disk }
procedure TIpDebugLog.DumpLog;
var
  DR : TIpDebugRec;
  FS : TFileStream;
  S, T : string;
begin
  LockLog;

  try
    { Open file stream }
    if FileExists(FileName) and (WriteMode = wmAppend) then begin
      FS := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
      FS.Seek(soFromEnd, 0);
    end else begin
      FS := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    end;

    try
      { Do file header if appropriate }
      if (FS.Size = 0) then begin
        S := dlDoFileHeader;
        FS.Write(S[1], Length(S));
      end;

      { Cycle through all data }
      while dlPopDebugEntry(DR) do begin
        if DR.drClass <> nil then begin

          { It belongs to somone else, let them process it }
          S := DR.drClass.GetLogString(DR.drSckt, DR.drData1, DR.drData2, DR.drData3);
        end else begin

          { Something we're supposed to know about, deal with it }
          case DR.drData1 of

            { Logging enabled }
            deEnabled : S := '**** Logging Enabled' + IpCRLF;

            { Logging disabled }
            deDisabled : S := '**** Logging Disabled' + IpCRLF;

            {!!.01 - added}
            { Telnet Command entry }
            deTelnetCmd : S := dlTelnetString(DR.drSckt, DR.drData2, DR.drData3);

            { WriteDebugString entry }
            deString :
              begin
                SetLength(S, DR.drData3);
                Move(PByteArray(DR.drData2)[0], S[1], DR.drData3);
              end;

            {!!.01 - added}
            else
              S := Format('!!!! Unknown log entry : [%d][%8.8x][%8.8x][%8.8x]' + IpCRLF,
                [DR.drSckt, DR.drData1, DR.drData2, DR.drData3]);

          end;
        end;

        { Write time stamp }
        T := dlTimeStamp(DR.drTime);
        FS.Write(T[1], Length(T));

        { Write log string }
        FS.Write(S[1], Length(S));

        { Write trailing CRLF }
        FS.Write(IpCRLF[1], Length(IpCRLF));
      end;

    finally
      FS.Free;
    end;

  finally
    UnlockLog;
  end;
end;

{!!.01 - added}
{ Determines whether something is in the buffer }
function TIpDebugLog.GetBufferEmpty : Boolean;
begin
  LockLog;
  try
    Result := (dlBufferHead = dlBufferTail);
  finally
    UnlockLog;
  end;
end;

{ Calculates free space in the buffer }
function TIpDebugLog.GetBufferFree : DWORD;
begin
  LockLog;
  try
    if (dlBufferHead <= dlBufferTail) then
      { One less than actual, since we always leave one byte free }
      Result := Pred(FBufferSize - (dlBufferTail - dlBufferHead))
    else
      Result := Pred(dlBufferHead - dlBufferTail);
  finally
    UnlockLog;
  end;
end;

{ Retrieves buffer size }
function TIpDebugLog.GetBufferSize : DWORD;
begin
  LockLog;
  try
    Result := FBufferSize;
  finally
    UnlockLog;
  end;
end;

{ Retrieves write mode }
function TIpDebugLog.GetWriteMode : TIpWriteMode;
begin
  LockLog;
  try
    Result := FWriteMode;
  finally
    UnlockLog;
  end;
end;

{ Sets the size of the logging buffer }
procedure TIpDebugLog.SetBufferSize(const Value : DWORD);
begin
  LockLog;
  try
    if Value <> FBufferSize then begin
      FBufferSize := Value;
      ReallocMem(dlBuffer, Value);
      ClearBuffer;
    end;
  finally
    UnlockLog;
  end;
end;

{ Enables (or disables) logging }
procedure TIpDebugLog.SetEnabled(const Value : Boolean);
begin
  LockLog;
  try
    if (Value = True) then begin

      { Allocate buffer if not already done }
      if (dlBuffer = nil) then begin
        GetMem(dlBuffer, FBufferSize);
      end;

      { Init temp buffer if not already done }
      if (dlTempBuffer = nil) then begin
        dlTempSize := 1024;
        GetMem(dlTempBuffer, dlTempSize);
      end;
    end else begin
      AddDebugEntry(nil, 0, deDisabled, 0, 0);
    end;

  finally
    UnlockLog;
  end;

  inherited;

  if (Value = True) then
    AddDebugEntry(nil, 0, deEnabled, 0, 0);
end;

{ Write debug string to log buffer }
procedure TIpDebugLog.WriteDebugString(const DebugString : string);
begin
  AddDebugEntry(nil, Invalid_Socket, deString, DWORD(DebugString), Length(DebugString));
end;

procedure TIpDebugLog.SetWriteMode(const Value : TIpWriteMode);
begin
  LockLog;
  try
    FWriteMode := Value;
  finally
    UnlockLog;
  end;
end;

{ TIpEventLog }

{ Get DateTimeFormat property }
function TIpEventLog.GetDateTimeFormat : string;
begin
  LockLog;
  try
    Result := FDateTimeFormat;
  finally
    UnlockLog;
  end;
end;

{ Set DateTimeFormat property }
procedure TIpEventLog.SetDateTimeFormat(const Value : string);
begin
  LockLog;
  try
    FDateTimeFormat := Value;
  finally
    UnlockLog;
  end;
end;

{ Writes and commits event string to log }
procedure TIpEventLog.WriteEventString(const EventString : string);
var
  LogStr : string;
  FileStm : TFileStream;
  LogMode : Word;
begin
  LockLog;
  try
    { Bail if logging isn't turned on }
    if not FEnabled then Exit;

    { Create appropriate string for log }
    DateTimeToString(LogStr, FDateTimeFormat, Now);
    LogStr := LogStr + ' ' + EventString + IpCRLF;

    { Check whether file exists, set flags appropriately }
    if FileExists(FFileName) then
      LogMode := (fmOpenReadWrite or fmShareDenyWrite)
    else
      LogMode := (fmCreate or fmShareDenyWrite);

    { Open file, write string, close file }
    FileStm := TFileStream.Create(FFileName, LogMode);
    try
      FileStm.Seek(0, soFromEnd);
      FileStm.WriteBuffer(LogStr[1], Length(LogStr));
    finally
      FileStm.Free;
    end;

  finally
    UnlockLog;
  end;
end;

{ TIpSocksServerInfo }

{ Create new instance of TIpSocksServer }
constructor TIpSocksServerInfo.Create;
begin
  inherited;
  FPort := 1080;
end;

{ Copy data from Source object }
procedure TIpSocksServerInfo.Assign(Source : TPersistent);
begin
  if Source is TIpSocksServerInfo then begin
    LockProperties;
    try
      FAddress := TIpSocksServerInfo(Source).Address;
      FPassword := TIpSocksServerInfo(Source).Password;
      FPort := TIpSocksServerInfo(Source).Port;
      FSocksVersion := TIpSocksServerInfo(Source).SocksVersion;
      FUserCode := TIpSocksServerInfo(Source).UserCode;
    finally
      UnlockProperties;
    end;
  end else
    inherited Assign(Source);
end;

{ Get Address property }
function TIpSocksServerInfo.GetAddress : string;
begin
  LockProperties;
  try
    Result := FAddress;
  finally
    UnlockProperties;
  end;
end;

{ Get Password property }
function TIpSocksServerInfo.GetPassword : string;
begin
  LockProperties;
  try
    Result := FPassword;
  finally
    UnlockProperties;
  end;
end;

{ Get Port property }
function TIpSocksServerInfo.GetPort : Word;
begin
  LockProperties;
  try
    Result := FPort;
  finally
    UnlockProperties;
  end;
end;

{ Get SocksVersion property }
function TIpSocksServerInfo.GetSocksVersion : TIpSocksVersion;
begin
  LockProperties;
  try
    Result := FSocksVersion;
  finally
    UnlockProperties;
  end;
end;

{ Get UserCode property }
function TIpSocksServerInfo.GetUserCode : string;
begin
  LockProperties;
  try
    Result := FUserCode;
  finally
    UnlockProperties;
  end;
end;

{ Set Address property }
procedure TIpSocksServerInfo.SetAddress(const Value : string);
begin
  LockProperties;
  try
    FAddress := Value;
  finally
    UnlockProperties;
  end;
end;

{ Set Password property }
procedure TIpSocksServerInfo.SetPassword(const Value : string);
begin
  LockProperties;
  try
    FPassword := Value;
  finally
    UnlockProperties;
  end;
end;

{ Set Port property }
procedure TIpSocksServerInfo.SetPort(const Value : Word);
begin
  LockProperties;
  try
    FPort := Value;
  finally
    UnlockProperties;
  end;
end;

{ Set SocksVersion property }
procedure TIpSocksServerInfo.SetSocksVersion(const Value : TIpSocksVersion);
begin
  LockProperties;
  try
    FSocksVersion := Value;
  finally
    UnlockProperties;
  end;
end;

{ Set UserCode property }
procedure TIpSocksServerInfo.SetUserCode(const Value : string);
begin
  LockProperties;
  try
    FUserCode := Value;
  finally
    UnlockProperties;
  end;
end;

{ TIpBaseInits }

{ Creates instance of TIpBaseInits }
constructor TIpBaseInits.Create;
begin
  inherited;
  FSocketOptions := [];
end;

{ Creates instance of TIpDataInits }
constructor TIpDataInits.Create;
begin
  inherited;
  FFixedLineLength := diFixedLineLength;
  FIdleTimeout := diIdleTimeout;
  FLineTermChar := diLineTermChar;
  FLineTerminator := diLineTerminator;
  FLogOptions := diLogOptions;                                         {!!.01}
  FReceiveBufferSize := diReceiveBufferSize;
  FReceiveMode := diReceiveMode;
  FSendBufferSize := diSendBufferSize;
  FStripLineTerminator := diStripLineTerminator;
end;

{ TIpBaseSocket }

{ Creates instance of TIpBaseSocket }
constructor TIpBaseSocket.Create(Owner : TIpSockControl; Socket : TSocket;
  var LocalAddress : TSockAddrIn; const Inits : TIpBaseInits;
  const Socks : TIpSocksServerInfo);
var
  OptSize : Integer;
  SockName : TSockAddrIn;
  NameLen : Integer;
begin
  inherited Create;

  InitializeCriticalSection(bsSockCS);

  { Init proxy stuff }
  bsSocks := TIpSocksServerInfo.Create;

  if Assigned(Socks) then
    bsSocks.Assign(Socks);

  if bsSocks.FAddress <> '' then
    bsSocksState := ssProxyConnect;

  { Init socket field }
  FSocketHandle := Invalid_Socket;

  { Assign Owner }
  if Assigned(Owner) then
    bsOwner := Owner;

  { Assign socket if supplied, otherwise create one }
  if Socket <> Invalid_Socket then begin
    FSocketHandle := Socket;

    { Sync socket type }
    OptSize := SizeOf(bsSockType);
    if IpWinSockAccess.GetSockOpt(FSocketHandle, Sol_Socket,
      So_Type, bsSockType, OptSize) = Socket_Error then
        raise EIpWinSockError.CreateWsError(bsGetLastError, 'GetSockOpt');

    { Set Async options, just in case }
    bsSetAsyncOptions;

    { Set socket options }
    if Assigned(Inits) then
      SocketOptions := Inits.SocketOptions;

    { Set bsLocalSockAddr }
    NameLen := SizeOf(SockName);
    IpWinSockAccess.GetSockName(FSocketHandle, SockName, NameLen);
    Move(SockName, bsLocalSockAddr, SizeOf(TSockAddrIn));

    { Set bsRemoteSockAddr }
    NameLen := SizeOf(SockName);
    IpWinSockAccess.GetPeerName(FSocketHandle, SockName, NameLen);
    Move(SockName, bsRemoteSockAddr, SizeOf(TSockAddrIn));

  end else
    bsInitSocket(LocalAddress, Inits);
end;

{ Destroys instance }
destructor TIpBaseSocket.Destroy;
begin
  { If socket still open, close it }
  if (FSocketHandle <> Invalid_Socket) then begin
    IpWinSockAccess.CloseSocket(FSocketHandle);
    FSocketHandle := Invalid_Socket;
  end;

  IpSafeFree(bsSocks);
  DeleteCriticalSection(bsSockCS);
  inherited;
end;

{ Housekeeping associated with receiving FD_ACCEPT message }
procedure TIpBaseSocket.bsAccept;
begin
  { Do nothing; base socket shouldn't get this message anyway }
end;

{ Housekeeping associated with receiving FD_CLOSE message }
procedure TIpBaseSocket.bsClose;
begin
  LockSock;
  try
    FConnected := False;                                      
  finally
    UnlockSock;
  end;
end;

{ Close the socket connection, release descriptor, free thyself }
procedure TIpBaseSocket.bsCloseSocket;
var
  Connection : TIpConnRec;
begin
  LockSock;
  try
    if (FSocketHandle <> Invalid_Socket) then
      IpWinsockAccess.CloseSocket(FSocketHandle);

    Connection.RemoteAddr := bsRemoteSockAddr.sin_addr;
    Connection.RemotePort := IpWinsockAccess.ntohs(bsRemoteSockAddr.sin_port);
    Connection.LocalAddr := bsLocalSockAddr.sin_addr;
    Connection.LocalPort := IpWinsockAccess.ntohs(bsLocalSockAddr.sin_port);
    bsOwner.DoStatus(FSocketHandle, stDisconnect, Connection);

  finally
    UnlockSock;
  end;
  {bsOwner.scFreeSocket(FSocketHandle);}                               {!!.02}
end;

{ Housekeeping associated with received FD_CONNECT message }
procedure TIpBaseSocket.bsConnect;
var
  Connection : TIpConnRec;
  SockName : TSockAddrIn;
  NameLen : Integer;
begin
  LockSock;
  try

    { Set bsLocalSockAddr }
    NameLen := SizeOf(SockName);

    IpWinSockAccess.GetSockName(FSocketHandle, SockName, NameLen);
    Move(SockName, bsLocalSockAddr, SizeOf(TSockAddrIn));

    { Set connected. }
    FConnected := True;
    
    case bsSocksState of
    { If we're doing SOCKS... umm, do SOCKS }
      ssProxyConnect :
        begin
          bsSocksState := ssNegotiating;
          bsHandleSocksConnect;
        end;
      { Fire owner event }
      else begin
        Connection.RemoteAddr := bsRemoteSockAddr.sin_addr;
        Connection.RemotePort := IpWinsockAccess.ntohs(bsRemoteSockAddr.sin_port);
        Connection.LocalAddr := bsLocalSockAddr.sin_addr;
        Connection.LocalPort := IpWinsockAccess.ntohs(bsLocalSockAddr.sin_port);
        bsOwner.DoStatus(FSocketHandle, stConnect, Connection);
      end;
    end;
  finally
    UnlockSock;
  end;
end;

{ Establishes a connection with given remote address }
procedure TIpBaseSocket.bsConnectSocket(var RemoteAddr : TSockAddrIn;
  const URL : string);
var
  ErrCode : Integer;
  ErrStr : string;
  SocksAddr : TSockAddrIn;
  TempAddr : TInAddr;
begin
  LockSock;

  try
    FURL := URL;
    Move(RemoteAddr, bsRemoteSockAddr, SizeOf(TSockAddrIn));
  finally
    UnlockSock;
  end;

  if bsSocksState > ssNone then begin
    ErrStr := 'SocksConnect';
    Move(DefSockAddrIn, SocksAddr, SizeOf(TSockAddrIn));
    SocksAddr.sin_port := IpWinSockAccess.htons(bsSocks.Port);

    { Is the address a dotted quad? }
    TempAddr := bsOwner.String2NetAddr(bsSocks.Address);

    if DWORD(TempAddr.S_addr) <> InAddr_None then
      SocksAddr.sin_addr := TempAddr
    else
      SocksAddr.sin_addr := bsOwner.LookupAddress(bsSocks.Address);

    ErrCode := IpWinSockAccess.Connect(FSocketHandle,
      SocksAddr, SizeOf(TSockAddrIn));

  end else begin
    ErrStr := 'Connect';
    ErrCode := IpWinSockAccess.Connect(FSocketHandle,
      RemoteAddr, SizeOf(TSockAddrIn));
  end;

  if (ErrCode = Socket_Error) then begin
    ErrCode := bsGetLastError;

    if ErrCode <> wsaEWouldBlock then
      raise EIpWinSockError.CreateWsError(ErrCode, ErrStr);
  end;
end;

{ Returns last WinSock error }
function TIpBaseSocket.bsGetLastError : Integer;
begin
  Result := IpWinSockAccess.WSAGetLastError;
end;

{ Starts a SOCKS connection }
procedure TIpBaseSocket.bsHandleSocksConnect;
begin
  { Can't actually handle SOCKS here, because we can't send/receive }
  { Defer to descendant }
end;

{ Creates and initializes a socket }
procedure TIpBaseSocket.bsInitSocket(var LocalAddress : TSockAddrIn;
  const Inits : TIpBaseInits);
var
  Err : Integer;
begin
  { Get socket type }
  bsSockType := bsOwner.scProtoType;
  FSocketHandle := IpWinSockAccess.Socket(Af_Inet, bsSockType, 0);

  { Handle error if necessary }
  if FSocketHandle = Invalid_Socket then
    raise EIpWinSockError.CreateWsError(bsGetLastError, 'Socket');

  { Set Async options }
  bsSetAsyncOptions;

  { Set socket options }
  if Assigned(Inits) then
    SocketOptions := Inits.SocketOptions;

  { Associate host address and port if necessary }
  if IpCompStruct(LocalAddress, DefSockAddrIn, SizeOf(TSockAddrIn)) <> 0 then begin

    { Update bsLocalSockAddr }
    Move(LocalAddress, bsLocalSockAddr, SizeOf(TSockAddrIn));

    { Bind socket }
    Err := IpWinSockAccess.Bind(FSocketHandle, bsLocalSockAddr, SizeOf(TSockAddrIn));

    if Err = Socket_Error then begin
      Err := bsGetLastError;
      raise EIpWinSockError.CreateWsError(Err, 'Bind');
    end;
  end else

    { Init bsLocalSockAddr to default for now }
    Move(DefSockAddrIn, bsLocalSockAddr, SizeOf(TSockAddrIn));
end;

{ Housekeeping associated with received FD_OOB message }
procedure TIpBaseSocket.bsOob;
begin
  { Do nothing; OOB data is currently unsupported }
end;

{ Housekeeping associated with received FD_READ message }
procedure TIpBaseSocket.bsRead;
begin
  { Do nothing; base socket shouldn't get this message anyway }
end;

{ Sets Async Options -- when overridden in descendant classes, }
{ "Inherited" will typically *not* be called. }
procedure TIpBaseSocket.bsSetAsyncOptions;
var
  ErrCode : Integer;
begin
  if (FSocketHandle <> Invalid_Socket) then begin
    ErrCode := IpWinSockAccess.WsaAsyncSelect(FSocketHandle, bsOwner.Handle,
      CM_IPSOCKMESSAGE, (Fd_Close or Fd_Connect));

    if ErrCode = Socket_Error then begin
      ErrCode := bsGetLastError;
      raise EIpWinSockError.CreateWsError(ErrCode, 'WsaAsyncSelect');
    end;
  end;
end;

{ Housekeeping associated with received timer tickle }
procedure TIpBaseSocket.bsTimer(Interval : Integer);
begin
  { Do nothing at this level; for now at least }
end;

{ Housekeeping associated with received FD_WRITE message }
procedure TIpBaseSocket.bsWrite;
begin
  { Do nothing; base socket shouldn't get this message anyway }
end;

{ Retrieve local address for a connected socket }
function TIpBaseSocket.GetLocalAddress : TInAddr;
begin
  if not FConnected then                                  
    raise EIpNotConnectedError.Create(SwsaENotConn);

  Result := bsLocalSockAddr.sin_addr;
end;

{ Retrieve local port for a connected socket }
function TIpBaseSocket.GetLocalPort : Word;
begin
  if not FConnected then
    raise EIpNotConnectedError.Create(SwsaENotConn);

  Result := bsLocalSockAddr.sin_port;
end;

{ Retrieve remote address for a connected socket }
function TIpBaseSocket.GetRemoteAddress : TInAddr;
begin
  if not FConnected then                                    
    raise EIpNotConnectedError.Create(SwsaENotConn);

  Result := bsRemoteSockAddr.sin_addr;
end;

{ Retrieve remote port for a connected socket }
function TIpBaseSocket.GetRemotePort : Word;
begin
  if not FConnected then                                     
    raise EIpNotConnectedError.Create(SwsaENotConn);

  Result := bsRemoteSockAddr.sin_port;
end;

{ Retrieve socket options }
function TIpBaseSocket.GetSocketOptions : TIpSockOptionSet;
var
  OptSize : Integer;
  OptBool : Bool;

  function NoError(ErrCode : Integer) : Boolean;
  begin
    if ErrCode <> Socket_Error then
      Result := True
    else
      raise EIpWinSockError.CreateWsError(bsGetLastError, 'GetSockOpt');
  end;

begin
  LockSock;
  try
    Result := [];
    OptSize := SizeOf(OptBool);

    { Get the options that are stored internally: }
    { By the way, these are stored internally as booleans instead of  }
    { being in the set because they get tested a *lot* during data    }
    { transfer, and simple booleans should be slightly more efficient }

    if FDiscardReceived then
      Include(Result, soDiscardReceived);

    if FEchoReceived then
      Include(Result, soEchoReceived);

    if FEchoSent then
      Include(Result, soEchoSent);

    if FStripHighBit then
      Include(Result, soStripHighBit);

    if FTelnet then
      Include(Result, soTelnet);

    if bsSockType = Sock_DGram then begin
      { Check WinSock for status of soBroadcast (datagram only) }
      if NoError(IpWinSockAccess.GetSockOpt(FSocketHandle, Sol_Socket,
        So_Broadcast, OptBool, OptSize)) then Include(Result, soBroadcast);
    end;

    if bsSockType = Sock_Stream then begin
      { Check WinSock for status of soNoDelay (stream only) }
      if NoError(IpWinSockAccess.GetSockOpt(FSocketHandle, IpProto_Tcp,
        Tcp_NoDelay, OptBool, OptSize)) then Include(Result, soNoDelay);

      { Check WinSock for status of soKeepAlive (stream only) }
      if NoError(IpWinSockAccess.GetSockOpt(FSocketHandle, Sol_Socket,
        So_KeepAlive, OptBool, OptSize)) then Include(Result, soKeepAlive);
    end;

    { Check WinSock for status of soDebug }
    if NoError(IpWinSockAccess.GetSockOpt(FSocketHandle, Sol_Socket,
      So_Debug, OptBool, OptSize)) then Include(Result, soDebug);

    { Check WinSock for status of soDontRoute }
    if NoError(IpWinSockAccess.GetSockOpt(FSocketHandle, Sol_Socket,
      So_DontRoute, OptBool, OptSize)) then Include(Result, soDontRoute);

    { Check WinSock for status of soReuseAddr }
    if NoError(IpWinSockAccess.GetSockOpt(FSocketHandle, Sol_Socket,
      So_Broadcast, OptBool, OptSize)) then Include(Result, soReuseAddr);

  finally
    UnlockSock;
  end;
end;

{ Retrieve full URL that was used for connection }
function TIpBaseSocket.GetURL : string;
begin
  LockSock;
  try
    Result := FURL;
  finally
    UnlockSock;
  end;
end;

{ Enters TIpBaseSocket critical section }
procedure TIpBaseSocket.LockSock;
begin
  if IsMultiThread then
    EnterCriticalSection(bsSockCS);
end;

{ Updates socket options }
procedure TIpBaseSocket.SetSocketOptions(const Value : TIpSockOptionSet);
var
  OptSize : Integer;
  OptBool : Bool;

  procedure CheckError(ErrCode : Integer);
  begin
    if ErrCode = Socket_Error then
      raise EIpWinSockError.CreateWsError(bsGetLastError, 'SetSockOpt');
  end;

begin
  LockSock;
  try
    OptSize := SizeOf(OptBool);

    if bsSockType = Sock_DGram then begin
      { Check WinSock for status of soBroadcast (datagram only) }
      OptBool := (soBroadcast in Value);
      CheckError(IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
        So_Broadcast, OptBool, OptSize));
    end;

    if bsSockType = Sock_Stream then begin
      { Check WinSock for status of soNoDelay (stream only) }
      OptBool := (soNoDelay in Value);
      CheckError(IpWinSockAccess.SetSockOpt(FSocketHandle, IpProto_Tcp,
        Tcp_NoDelay, OptBool, OptSize));

      { Check WinSock for status of soKeepAlive (stream only) }
      OptBool := (soKeepAlive in Value);
      CheckError(IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
        So_KeepAlive, OptBool, OptSize));
    end;

    { Check WinSock for status of soDebug }
    OptBool := (soDebug in Value);
    CheckError(IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
      So_Debug, OptBool, OptSize));

    { Check WinSock for status of soDontRoute }
    OptBool := (soDontRoute in Value);
    CheckError(IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
      So_DontRoute, OptBool, OptSize));

    { Check WinSock for status of soReuseAddr }
    OptBool := (soReuseAddr in Value);
    CheckError(IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
      So_ReuseAddr, OptBool, OptSize));

    { Store the options that get handled locally }
    { By the way, these are stored internally as booleans instead of  }
    { being in the set because they get tested a *lot* during data    }
    { transfer, and simple booleans should be slightly more efficient }

    FDiscardReceived := (soDiscardReceived in Value);

    FEchoReceived := (soEchoReceived in Value);

    FEchoSent := (soEchoSent in Value);

    FStripHighBit := (soStripHighBit in Value);

    FTelnet := (soTelnet in Value);

  finally
    UnlockSock;
  end;
end;

{ Leaves TIpBaseSocket critical section }
procedure TIpBaseSocket.UnlockSock;
begin
  if IsMultiThread then
    LeaveCriticalSection(bsSockCS);
end;

{ TIpDataSocket }

{ Creates instance of TIpDataSocket }
constructor TIpDataSocket.Create(Owner : TIpSockControl; Socket : TSocket;
  var LocalAddress : TSockAddrIn; const Inits : TIpBaseInits;
  const Socks : TIpSocksServerInfo);
begin
  inherited;
  { we'll go ahead and create the send queue here since it will likely be used }
  { creation of the terminal list is deferred until it's actually needed       }
  dsSendQueue := TList.Create;

  { Initialize telnet stuff }

  { Things we want the remote to do }
  Include(toRemoteDesired, TELNETOPT_BINARY);
  Include(toRemoteDesired, TELNETOPT_ECHO);
  Include(toRemoteDesired, TELNETOPT_SUPGA);

  { Things we know how to handle }
  Include(toHostAllowed, TELNETOPT_BINARY);
  Include(toHostAllowed, TELNETOPT_SUPGA);
  Include(toHostAllowed, TELNETOPT_TERM);
  Include(toHostAllowed, TELNETOPT_STATUS);
  Include(toHostAllowed, TELNETOPT_NAWS);

  { Initialize property variables }
  FIdleTime := 0;
  if Assigned(Inits) then begin
    FFixedLineLength := TIpDataInits(Inits).FixedLineLength;
    FIdleTimeout := TIpDataInits(Inits).IdleTimeout;
    FLineTermChar := TIpDataInits(Inits).LineTermChar;
    FLineTerminator := TIpDataInits(Inits).LineTerminator;
    FLogOptions := TIpDataInits(Inits).LogOptions;                     {!!.01}
    FReceiveMode := TIpDataInits(Inits).ReceiveMode;
    FStripLineTerminator := TIpDataInits(Inits).StripLineTerminator;
    ReceiveBufferSize := TIpDataInits(Inits).ReceiveBufferSize;
    SendBufferSize := TIpDataInits(Inits).SendBufferSize;
  end else begin
    { nil sent in, so fake it }
    FFixedLineLength := diFixedLineLength;
    FIdleTimeout := diIdleTimeout;
    FLineTermChar := diLineTermChar;
    FLineTerminator := diLineTerminator;
    FLogOptions := diLogOptions;                                       {!!.01}
    FReceiveMode := diReceiveMode;
    FStripLineTerminator := diStripLineTerminator;
    ReceiveBufferSize := diReceiveBufferSize;
    SendBufferSize := diSendBufferSize;
  end;
end;

{ Destroys instance }
destructor TIpDataSocket.Destroy;
begin
  IpSafeFree(dsTerminalList);
  FreeMem(dsTelnetCmd);
  dsClearSendQueue;                                                    {!!.02}
  dsSendQueue.Free;                                                    {!!.02}
  dsSendQueue := nil;                                                  {!!.02}
  inherited;
end;

{ Housekeeping associated with received FD_CLOSE message }
procedure TIpDataSocket.bsClose;
begin
  if FConnected then bsCloseSocket;                                    {!!.02}
end;

{ Performs a graceful shutdown of socket }
procedure TIpDataSocket.bsCloseSocket;
begin
  dsShutdown;

  try
    { Read, rinse, repeat }
    while dsReadData(True) <> 0 do

      { Handle existing data in buffer }
      while dsCheckRead do ;

    { Grab last bit out of buffer }
    dsExtractLine(True);

  { make sure we call inherited }
  finally
    inherited;
  end;
end;

{ Connects socket to specified remote host }
procedure TIpDataSocket.bsConnectSocket(var RemoteAddr : TSockAddrIn;
  const URL : string);
begin
  LockSock;
  try
    { Zero the InBuffer }
    dsInHead := 0;
    dsInTail := 0;

    { Completely zero the stats }
    FillChar(dsSockStats, SizeOf(dsSockStats), #0);
    dsSockStatsDirty := False;
  finally
    UnlockSock;
  end;

  inherited;
end;

{ Starts a SOCKS connection }
procedure TIpDataSocket.bsHandleSocksConnect;
var
  Bytes : Pointer;
  AddrRec : TIpAddrRec;
  BytesSize : Integer;
  Socks5Array : array[0..3] of Byte;
begin
  case bsSocks.SocksVersion of
    svSocks4 :
      begin
        BytesSize := SizeOf(TIpSocks4Rec) + Length(bsSocks.UserCode)+1;
        Bytes := AllocMem(BytesSize);
        try
          { Set up the data }
          TIpSocks4Rec(Bytes^).Ver := 4;
          TIpSocks4Rec(Bytes^).Cmd := 1;
          TIpSocks4Rec(Bytes^).Port := bsRemoteSockAddr.sin_port;
          TIpSocks4Rec(Bytes^).Addr := bsRemoteSockAddr.sin_addr;
          Move(bsSocks.UserCode[1], PByteArray(Bytes)[SizeOf(TIpSocks4Rec)],
            Length(bsSocks.UserCode));

          { Send the data }
          dsPutData(Bytes^, BytesSize, nil, False, True, True);
        finally
          FreeMem(Bytes);
        end;
      end;

    svSocks4a :
      begin
        FillChar(AddrRec, SizeOf(AddrRec), #0);
        IpParseURL(URL, AddrRec);
        BytesSize := SizeOf(TIpSocks4Rec) + Length(bsSocks.UserCode)+1;
        Bytes := AllocMem(BytesSize);
        try
          { Set up the data }
          TIpSocks4Rec(Bytes^).Ver := 4;
          TIpSocks4Rec(Bytes^).Cmd := 1;
          TIpSocks4Rec(Bytes^).Port := bsRemoteSockAddr.sin_port;
          TIpSocks4Rec(Bytes^).Addr := bsRemoteSockAddr.sin_addr;
          Move(bsSocks.UserCode[1], PByteArray(Bytes)[SizeOf(TIpSocks4Rec)],
            Length(bsSocks.UserCode));

          { Send the data }
          dsPutData(Bytes^, BytesSize, nil, False, True, True);
        finally
          FreeMem(Bytes);
        end;
      end;

    svSocks5 :
      begin
        { Initial negotiation }
        Socks5Array[0] := 5;
        { We can handle no negotiation, or user/password }
        Socks5Array[1] := 2;
        Socks5Array[2] := 0;
        Socks5Array[3] := 2;

        { Send the data }
        dsPutData(Socks5Array, SizeOf(Socks5Array), nil, False, True, True);
      end;
  end;
end;

{ Housekeeping associated with received FD_READ message }
procedure TIpDataSocket.bsRead;
begin
  dsResetIdleTime;

  { Read, rinse, repeat }
  while dsReadData(False) <> 0 do

    { Handle existing data in buffer }
    while dsCheckRead do ;

  inherited;
end;

{ Sets Async Options -- when overridden in descendant classes, }
{ "Inherited" will typically *not* be called. }
procedure TIpDataSocket.bsSetAsyncOptions;
var
  ErrCode : Integer;
begin
  if (FSocketHandle <> Invalid_Socket) then begin
    ErrCode := IpWinSockAccess.WsaAsyncSelect(FSocketHandle, bsOwner.Handle,
      CM_IPSOCKMESSAGE, (Fd_Close or Fd_Connect or Fd_Read or Fd_Write));

    if ErrCode = Socket_Error then begin
      ErrCode := bsGetLastError;
      raise EIpWinSockError.CreateWsError(ErrCode, 'WsaAsyncSelect');
    end;
  end;
end;

{ Housekeeping associated with received timer tickle }
procedure TIpDataSocket.bsTimer(Interval : Integer);
var
  Connection : TIpConnRec;
begin
  LockSock;
  try
    { Send status message if appropriate }
    if dsSockStatsDirty then begin
      Connection.RemoteAddr := bsRemoteSockAddr.sin_addr;
      Connection.RemotePort := IpWinsockAccess.ntohs(bsRemoteSockAddr.sin_port);
      Connection.LocalAddr := bsLocalSockAddr.sin_addr;
      Connection.LocalPort := IpWinsockAccess.ntohs(bsLocalSockAddr.sin_port);
      bsOwner.DoStatus(FSocketHandle, stProgress, Connection);
      dsSockStatsDirty := False;                                       {!!.01}
    end;

    dsIncIdleTime(Interval);                                    {moved  !!.03}
  finally
    UnlockSock;
  end;

  inherited;
end;

{ Housekeeping associated with received FD_WRITE message }
procedure TIpDataSocket.bsWrite;
begin
  LockSock;
  try
    { Enable Send if appropriate }
    if FSocketHandle <> Invalid_Socket then                            {!!.02}
      dsWritable := True
    else
      Exit;
  finally
    UnlockSock;
  end;
  { See if there's anything waiting to be sent }
  while dsCheckSendQueue do;
end;

{ Adds terminal from socket's terminal list }
procedure TIpDataSocket.dsAttachTerminal(Handle : TIpHWND);            {!!.12}
begin
  LockSock;
  try
    { If we don't have a terminal list, create one }
    if not Assigned(dsTerminalList) then
      dsTerminalList := TList.Create;

    { Add terminal to the list }
    dsTerminalList.Add(Pointer(Handle));
  finally
    UnlockSock;
  end;
end;

{ Check received data -- handle appropriately }
function TIpDataSocket.dsCheckRead : Boolean;
var
  TempStr : string;
  StrSize : DWORD;
begin
  { Assume fail }
  Result := False;

  LockSock;
  try
    { Check for simple case -- nothing to process }
    if (dsInHead = dsInTail) then Exit;

    case FReceiveMode of

      rmLine :
        begin
          { Search for lines, send 'em along }
          Result := dsExtractLine(False);

          { Deal with dsLineBuf > MaxLineBuf }                         {!!.03}
          if Length(dsLineBuf) > MaxLineBuf then begin                 {!!.03}
            dsLineBuf := '';                                           {!!.03}
            bsOwner.DoError(SocketHandle, ReadLineErr, SReadLineErr);  {!!.03}
          end;                                                         {!!.03}

          { If buffer full, move some to dsLineBuf }                   {!!.03}
          if dsInBufAvail = 0 then begin                               {!!.03}
            { Grab all of buffer except the very end (to preserve }    {!!.03}
            { potential line terminator fragment) }                    {!!.03}
            StrSize := (dsInTail - dsInHead - 2);                      {!!.03}
            SetString(TempStr, PChar(@dsInBuf[dsInHead]), StrSize);    {!!.03}
            dsLineBuf := dsLineBuf + TempStr;                          {!!.03}
            Inc(dsInHead, StrSize);                                    {!!.03}
          end;                                                         {!!.03}

          { Make more room in buffer if necessary/possible }
          if (dsInHead <> 0) and (dsInTail > (SizeOf(dsInBuf) shr 1)) then begin
            Move(dsInBuf[dsInHead], dsInBuf[0], (dsInTail-dsInHead));
            dsInTail := (dsInTail-dsInHead);
            dsInHead := 0;
          end;
        end;

      rmStream :
        begin
          { Dump the whole lot to the stream }
          if Assigned(FReceiveStream) then begin
            FReceiveStream.WriteBuffer(dsInBuf[dsInHead], dsInTail - dsInHead);
            dsInHead := dsInTail;
          end else
            raise EIpProgrammerError.Create(SNoStreamErr);
        end;

      {!!.01 - added}
      rmAny :
        begin
          { Fire OnReadLine every time data is received }
          Result := dsExtractLine(True);
        end;
    end;
  finally
    UnlockSock;
  end;
end;

{ Check send queue, send if possible -- return True if there might be more to do }
function TIpDataSocket.dsCheckSendQueue : Boolean;
begin
  Result := False;
  LockSock;
  try
    { Protect against potential problems during shutdown }
    if FSocketHandle = Invalid_Socket then Exit;                       {!!.02}
    if Assigned(dsSendQueue) and dsWritable then begin                 {!!.03}

      if (dsSendQueue.Count > 0) and dsWritable then begin
        if dsSendQueueItem(PIpSendQueueItem(dsSendQueue.Items[0])) then begin

          { Entire item was sent, fire event }
          bsOwner.DoItemSent(FSocketHandle,
            PIpSendQueueItem(dsSendQueue.Items[0]).Handle, dsSendQueue.Count-1);

          { Are we supposed to disconnect? }
          if PIpSendQueueItem(dsSendQueue.Items[0]).Disconnect then begin

            { Shutdown process will free item }
            bsCloseSocket;
            PostMessage(bsOwner.Handle, CM_IPFREESOCKET, FSocketHandle, 0);  {!!.02}

          end else begin
            { Done with this item, free it }
            dsFreeQueueItem(PIpSendQueueItem(dsSendQueue.Items[0]));
            dsSendQueue.Delete(0);
          end;
        end;
        Result := True;
      end;
    end;                                                               {!!.02}
  finally
    UnlockSock;
  end;
end;

{ Clears the send queue of all pending items }
procedure TIpDataSocket.dsClearSendQueue;
var
  I : Integer;
begin
  if dsSendQueue <> nil then begin                                     {!!.14}
    LockSock;
    try
      for I := dsSendQueue.Count-1 downto 0 do begin
        dsFreeQueueItem(PIpSendQueueItem(dsSendQueue.Items[I]));
        dsSendQueue.Delete(I);
      end;
    finally
      UnlockSock;
    end;
  end;                                                                  {!!.14}
end;

{ Enables a given option -- gives us a chance to take action }
procedure TIpDataSocket.dsEnableTelnetHostOption(Option : AnsiChar);
begin
  Include(toHostEnabled, Option);
  if Option = TELNETOPT_NAWS then dsSendTerminalSize;
end;

{ Removes terminal from socket's terminal list }
procedure TIpDataSocket.dsDetachTerminal(Handle : TIpHWND);            {!!.12}
var
  I : Integer;
begin
  LockSock;
  try
    I := dsTerminalList.IndexOf(Pointer(Handle));
    if I <> -1 then
      dsTerminalList.Delete(I);
  finally
    UnlockSock;
  end;
end;

{ Extract a line from the input buffer }
function TIpDataSocket.dsExtractLine(Force : Boolean) : Boolean;
var
  Ch : AnsiChar;
  BufSize, LineLen, I : DWORD;
  TermSize, StrSize : DWORD;
  TempStr : string;
begin
  Result := False;
  LockSock;
  try
    BufSize := (dsInTail - dsInHead);
    if BufSize = 0 then                                                {!!.12}
      Exit;                                                            {!!.12}

    if Force then
      LineLen := BufSize
    else
      LineLen := FFixedLineLength;

    if (FLineTerminator = ltNone) or Force then begin

      { Handle fixed length line situations }
      if (LineLen <= (BufSize + DWORD(Length(dsLineBuf))))             {!!.03}
          and (LineLen <> 0) then begin
        SetString(TempStr, PChar(@dsInBuf[dsInHead]), LineLen);

        { Fire DoReadLine }
        if dsLineBuf = '' then                                         {!!.03}
          bsOwner.DoReadLine(FSocketHandle, TempStr)                   {!!.03}
        else begin                                                     {!!.03}
          bsOwner.DoReadLine(FSocketHandle, dsLineBuf + TempStr);      {!!.03}
        end;                                                           {!!.03}

        Inc(dsInHead, LineLen - DWORD(Length(dsLineBuf)));             {!!.03}
        dsLineBuf := '';                                               {!!.03}
        Result := True;
      end;

    end else begin

      { Handle other situations }
      TermSize := 1;
      case FLineTerminator of
        ltCRLF : begin
                   Ch := #10;
                   TermSize := 2;
                 end;
        ltLF : Ch := #10;
        ltCR : Ch := #13;
        ltOther : Ch := FLineTermChar;
      else
        raise Exception.Create('Bad terminator');
      end;

      { Adjust TermSize }
      if not FStripLineTerminator then
        TermSize := 0;

      { Search buffer for line terminator }
      for I := 0 to BufSize-1 do begin

        { Do we have a match? }
        if dsInBuf[dsInHead+I] = Byte(Ch) then begin

          { Handle ltCRLF special case }
          if FLineTerminator = ltCRLF then
            if (I = 0) or (dsInBuf[dsInHead+I-1] <> 13) then
              Continue;

          { Do the string thing }
          StrSize := I + 1 - TermSize;
          SetString(TempStr, PChar(@dsInBuf[dsInHead]), StrSize);

          { Fire DoReadLine }
          if dsLineBuf = '' then                                       {!!.03}
            bsOwner.DoReadLine(FSocketHandle, TempStr)                 {!!.03}
          else begin                                                   {!!.03}
            bsOwner.DoReadLine(FSocketHandle, dsLineBuf + TempStr);    {!!.03}
            dsLineBuf := '';                                           {!!.03}
          end;                                                         {!!.03}

          Inc(dsInHead, I+1);
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    UnlockSock;
  end;
end;

{ Free a queue item, disposing all memory }
{ This does *not* free the associated stream, if one exists }
procedure TIpDataSocket.dsFreeQueueItem(QItem : PIpSendQueueItem);
begin
  FreeMem(QItem.Buffer);
  FreeMem(QItem);
end;

{ Handle telnet subnegotiations }
procedure TIpDataSocket.dsHandleSubnegotiation(const Subnegotiation : string);
begin
  { Handle the subnegotiations we know about }
  case Subnegotiation[1] of
    TELNETOPT_TERM : if Subnegotiation[2] = #1 then dsSendTerminal;
    TELNETOPT_STATUS : if Subnegotiation[2] = #1 then dsSendTelnetStatus;
  end;
end;

{ Handle telnet commands -- returns True if command is option negotiation }
{ Dragons be here -- don't make any changes to telnet option negotiation  }
{ unless you've read and *fully* understand RFC 854 and 1143.             }
{ The negotiation process here does not perfectly map to the methods      }
{ described in RFC 1143 (obviously, anyway) but it's believed the intent  }
{ is met and the hazards discussed in RFC 1143 are avoided.               }
function TIpDataSocket.dsHandleTelnet(Command, Option : AnsiChar) : Boolean;
var
  LogCmd : DWORD;
begin
  Result := True;

  {!!.01 - added}
  { Log it }
  if (loTelnet in FLogOptions) then begin
    if (Command = TELNET_DONT) or
       (Command = TELNET_DO) or
       (Command = TELNET_WONT) or
       (Command = TELNET_WILL) then begin
      LogCmd := Byte(Command);
      LogCmd := (LogCmd shl 8) or Byte(Option);
      bsOwner.DebugLog.AddDebugEntry(nil, SocketHandle,
        deTelnetCmd, slRead, LogCmd);
    end;
  end;

  case Command of

    TELNET_DONT :
    if (Option in toHostNegotiating) then begin
      if (Option in toHostAllowed) then begin
        if (Option in toHostEnabled) then begin
          { Looks like we changed our mind -- renegotiate }
          Exclude(toHostEnabled, Option);
          dsSendTelnet(TELNET_WILL, Option);
        end else begin
          { We're getting refused, nice try }
          Exclude(toHostNegotiating, Option);
          Exclude(toHostEnabled, Option);
        end;
      end else begin
        { Looks like everyone's happy, update bits }
        Exclude(toHostNegotiating, Option);
        Exclude(toHostEnabled, Option);
      end;
    end else begin
      if (Option in toHostEnabled) then begin
        { Turn it off }
        Exclude(toHostEnabled, Option);
        dsSendTelnet(TELNET_WONT, Option);
      end else begin
        { It's already off-- ignore }
      end;
    end;

    TELNET_DO :
    if (Option in toHostNegotiating) then begin
      if (Option in toHostAllowed) then begin
        { Looks like everyone's happy, update bits }
        Exclude(toHostNegotiating, Option);
        dsEnableTelnetHostOption(Option);
      end else begin
        if (Option in toHostEnabled) then begin
          { Something stupid is going on, stop the insanity }
          Exclude(toHostNegotiating, Option);
          Exclude(toHostEnabled, Option);
        end else begin
          { Looks like we changed our mind -- renegotiate }
          dsEnableTelnetHostOption(Option);
          dsSendTelnet(TELNET_WONT, Option);
        end;
      end;
    end else begin
      if (Option in toHostEnabled) then begin
        { It's already on -- ignore }
      end else begin
        { Turn it on if we'll allow it }
        if (Option in toHostAllowed) then begin
          dsEnableTelnetHostOption(Option);
          dsSendTelnet(TELNET_WILL, Option);
        end else begin
          dsSendTelnet(TELNET_WONT, Option);
        end;
      end;
    end;

    TELNET_WONT :
    if (Option in toRemoteNegotiating) then begin
      if (Option in toRemoteDesired) then begin
        if (Option in toRemoteEnabled) then begin
          { Looks like we changed our mind -- renegotiate }
          Exclude(toRemoteEnabled, Option);
          dsSendTelnet(TELNET_DO, Option);
        end else begin
          { We're getting refused, nice try }
          Exclude(toRemoteNegotiating, Option);
          Exclude(toRemoteEnabled, Option);
        end;
      end else begin
        { Looks like everyone's happy, update bits }
        Exclude(toRemoteNegotiating, Option);
        Exclude(toRemoteEnabled, Option);
      end;
    end else begin
      if (Option in toRemoteEnabled) then begin
        { Turn it off }
        Exclude(toRemoteEnabled, Option);
        dsSendTelnet(TELNET_DONT, Option);
      end else begin
        { It's already off-- ignore }
      end;
    end;

    TELNET_WILL :
    if (Option in toRemoteNegotiating) then begin
      if (Option in toRemoteDesired) then begin
        { Looks like everyone's happy, update bits }
        Exclude(toRemoteNegotiating, Option);
        Include(toRemoteEnabled, Option);
      end else begin
        if (Option in toRemoteEnabled) then begin
          { Something stupid is going on, stop the insanity }
          Exclude(toRemoteNegotiating, Option);
          Exclude(toRemoteEnabled, Option);
        end else begin
          { Looks like we changed our mind -- renegotiate }
          Include(toRemoteEnabled, Option);
          dsSendTelnet(TELNET_DONT, Option);
        end;
      end;
    end else begin
      if (Option in toRemoteEnabled) then begin
        { It's already on -- ignore }
      end else begin
        { Turn it on if we want it }
        if (Option in toRemoteDesired) then begin
          Include(toRemoteEnabled, Option);
          dsSendTelnet(TELNET_DO, Option);
        end else begin
          dsSendTelnet(TELNET_DONT, Option);
        end;
      end;
    end;

    else begin
      { Not an option negotiation }
      Result := False;
    end;
  end;
end;
//------------------------------------------------------------------------------
{ Handle idle timeout }
procedure TIpDataSocket.dsIdleTimeout;
var
  Reset : Boolean;
begin
  Reset := False;
  bsOwner.DoIdleTimeout(FSocketHandle, Reset);
  if Reset then dsResetIdleTime;
end;

{ Increment the IdleCounter by Delta }
procedure TIpDataSocket.dsIncIdleTime(Delta : Integer);
begin
  LockSock;
  try
    { Increment IdleTime }
    Inc(FIdleTime, Delta);

    { Check for timeout and handle }
    if (FIdleTimeout <> 0) and (FIdleTime > FIdleTimeout) then
      dsIdleTimeout;

  finally
    UnlockSock;
  end;
end;

{ Return available space in local buffer }
function TIpDataSocket.dsInBufAvail : DWORD;
begin
  LockSock;
  try
    Result := SizeOf(dsInBuf) - dsInTail;
  finally
    UnlockSock;
  end;
end;

{ Return next queue handle }
function TIpDataSocket.dsNextQueueHandle : DWORD;
begin
  LockSock;
  try
    Inc(dsQueueHandle);
    Result := dsQueueHandle;
  finally
    UnlockSock;
  end;
end;

{ Read data from WinSock, process appropriately -- returns bytes read }
function TIpDataSocket.dsReadData(Force : Boolean) : Integer;
var
  ErrCode : Integer;
begin
  { Assume failure }
  Result := 0;

  LockSock;
  try

    { Quick idiot check(s) }
    if FSocketHandle = Invalid_Socket then Exit;                       {!!.02}
    if (not Force) and (not Connected) then Exit;

    { Zero buffer if possible }
    if (dsInHead >= dsInTail) then begin                               {!!.12} 
      dsInHead := 0;
      dsInTail := 0;
    end;

    { Read data from WinSock }
    Result := IpWinSockAccess.Recv(FSocketHandle, dsInBuf[dsInTail],
      dsInBufAvail, 0);

    {!!.01 - added}
    { Log raw incoming data - ignore if everything has been processed out }
    if (Result > 0) and (loLowData in FLogOptions) then
      bsOwner.DebugLog.AddDebugEntry(TIpSockControl, FSocketHandle, scRawRead,
        DWORD(@dsInBuf[dsInTail]), Result);

    { Check for errors }
    if Result = Socket_Error then begin

      { Raise exception if error is "unexpected" }
      { If the error results from attempting to Force the read, no worries }
      ErrCode := bsGetLastError;
      if (ErrCode = wsaEWouldBlock) or (ErrCode = wsaENotConn) or Force then {!!.10}
        Result := 0
      else
        raise EIpWinSockError.CreateWsError(ErrCode, 'Recv');

    end else begin

      { Record prefiltered bytes rec'd }
      Inc(dsSockStats.RawBytesRcvd, Result);
      Inc(dsSockStats.TotalRawBytesRcvd, Result);

      { The following filter operations must be done in the order shown }

      { Handle incoming Socks stuff }
      if (bsSocksState > ssNone) and (bsSocksState < ssConnected) then
        Result := dsSocksRcv(dsInBuf[dsInTail], Result);

      { Handle telnet processing on received data }
      if FTelnet then
        Result := dsTelnetRcv(dsInBuf[dsInTail], Result);

      { Strip the high bit of each received character if desired }
      if FStripHighBit then
        dsStripHighBit(dsInBuf[dsInTail], Result);

      { Echo the received data if desired }
      if FEchoReceived then
        dsPutData(dsInBuf[dsInTail], Result, nil, False, True, False);

      { Discard the received data if desired }
      if FDiscardReceived then
        Result := 0;

      { Log incoming data - ignore if everything has been processed out }
      if (Result > 0) and (loHighData in FLogOptions) then             {!!.01}
        bsOwner.DebugLog.AddDebugEntry(TIpSockControl, FSocketHandle, scReadData,
          DWORD(@dsInBuf[dsInTail]), Result);

      { Send processed data to any attached terminal(s) }
      dsWriteTerm(dsInBuf[dsInTail], Result);

      { Record postfiltered bytes rec'd }
      Inc(dsSockStats.BytesRcvd, Result);
      Inc(dsSockStats.TotalBytesRcvd, Result);
      dsSockStatsDirty := True;

      { Position tail appropriately }
      Inc(dsInTail, Result);
    end;

  finally
    UnlockSock;
  end;
end;

{ Fill internal buffer of queue item from stream }
procedure TIpDataSocket.dsRefreshQueueItemBuffer(QItem : PIpSendQueueItem);
var
  Buffer : array[0..1023] of AnsiChar;
  ReadSize, NewSize : DWORD;
begin
  LockSock;
  try

    { Telnet is handled differently }
    if FTelnet then begin

      { Get amount to read }
      ReadSize := IpMinInt(QItem.Stream.Size - QItem.Stream.Position,
        SizeOf(Buffer));
      QItem.Stream.ReadBuffer(Buffer, ReadSize);

      { Done with stream }
      if ReadSize < SizeOf(Buffer) then
        QItem.Stream := nil;

      { See how much room we're going to need in the buffer }
      NewSize := ReadSize + IpCharCount(Buffer, ReadSize, #255);

      { Make appropriate size buffer, update fields }
      ReallocMem(QItem.Buffer, NewSize);
      QItem.BufferSize := NewSize;
      QItem.ActualData := ReadSize;
      QItem.BufferOffset := 0;

      { Move data via telnet filter }
      dsTelnetSnd(Buffer, QItem.Buffer^, ReadSize);

    end else begin

      { Get amount to read }
      ReadSize := IpMinInt(QItem.Stream.Size - QItem.Stream.Position, MtuSize);

      { Make appropriate size buffer }
      ReallocMem(QItem.Buffer, ReadSize);
      QItem.BufferSize := ReadSize;
      QItem.ActualData := ReadSize;
      QItem.BufferOffset := 0;

      { Read data directly to buffer }
      QItem.Stream.ReadBuffer(QItem.Buffer^, ReadSize);

      { Done with stream? }
      if ReadSize < MtuSize then
        QItem.Stream := nil;

    end;

  finally
    UnlockSock;
  end;
end;

{ Resets the IdleTime counter }
procedure TIpDataSocket.dsResetIdleTime;
begin
  LockSock;
  try
    FIdleTime := 0;
  finally
    UnlockSock;
  end;
end;

{ Attempts to send queue item -- return True if item completely sent }
function TIpDataSocket.dsSendQueueItem(QItem : PIpSendQueueItem) : Boolean;
var
  ErrCode : Integer;
  BufSize : Integer;
begin
  { Assume failure }
  Result := False;

  LockSock;
  try
    { Send as much of item as possible }
    if dsWritable then begin

      BufSize := (QItem.BufferSize - QItem.BufferOffset);
      ErrCode := IpWinSockAccess.Send(FSocketHandle,
        QItem.Buffer[QItem.BufferOffset], BufSize, 0);

      {reset the idle time counter}                                    {!!.03}
      FIdleTime := 0;                                                   {!!.03}

      {!!.01 - added}
      { Log raw outgoing data  }
      if (ErrCode > 0) and (loLowData in FLogOptions) then
        bsOwner.DebugLog.AddDebugEntry(TIpSockControl, FSocketHandle,
          scRawWrite, DWORD(@QItem.Buffer[QItem.BufferOffset]), ErrCode);

      { Did we send everything? }
      if (ErrCode = BufSize) then begin

        { Record bytes sent }
        Inc(dsSockStats.RawBytesSent, QItem.BufferSize);
        Inc(dsSockStats.TotalRawBytesSent, QItem.BufferSize);
        if not QItem.Internal then begin
          Inc(dsSockStats.BytesSent, QItem.ActualData);
          Inc(dsSockStats.TotalBytesSent, QItem.ActualData);

          { Log outgoing data  }
          if (loHighData in FLogOptions) then                          {!!.01}
            bsOwner.DebugLog.AddDebugEntry(TIpSockControl, FSocketHandle,
              scWriteData, DWORD(@QItem.Buffer[0]), QItem.ActualData);
        end;
        dsSockStatsDirty := True;

        { Are we attached to a stream? }
        if QItem.Stream <> nil then

          { Refill buffer }
          dsRefreshQueueItemBuffer(QItem)
        else
          { We're done }
          Result := True;

      { Did we get an error? }
      end else if (ErrCode = Socket_Error) then begin

        ErrCode := bsGetLastError;

        { "Expected" error -- handle appropriately }
        if ErrCode = wsaEWouldBlock then

          { Set writable to False, and let it ride }
          dsWritable := False
        else
          { "Unexpected" error -- bummer }
          raise EIpWinSockError.CreateWsError(ErrCode, 'send');

      { Must be a partial send }
      end else begin
        Inc(QItem.BufferOffset, ErrCode);
      end;
    end;

  finally
    UnlockSock;
  end;
end;

{ Send telnet command }
procedure TIpDataSocket.dsSendTelnet(Command, Option : AnsiChar);
var
  TelnetCmd : array[0..2] of AnsiChar;
  LogCmd : DWORD;
begin
  { Verify we're in telnet mode }
  if not FTelnet then Exit;

  { Format and send command }
  TelnetCmd[0] := TELNET_IAC;
  TelnetCmd[1] := Command;
  TelnetCmd[2] := Option;
  dsPutData(TelnetCmd, SizeOf(TelnetCmd), nil, False, True, True);

  { Log it }
  {!!.01 - added}
  if (loTelnet in FLogOptions) then begin
    LogCmd := Byte(Command);
    LogCmd := (LogCmd shl 8) or Byte(Option);
    bsOwner.DebugLog.AddDebugEntry(nil, SocketHandle,
      deTelnetCmd, slWrite, LogCmd);
  end;
end;

{ Doubles any occurence of Char in the string }
function DoubleChar(const S : string; Char : AnsiChar) : string;
var
  Len, I : Integer;
begin
  if CharPos(Char, S) <> 0 then begin
    Len := Length(S);
    { Yeah, kinda ugly -- but this should rarely, if ever, be needed }
    for I := 1 to Len do begin
      Result := Result + S[I];
      if S[I] = Char then
        Result := Result + Char;
    end;
  end else begin
    Result := S;
  end;
end;

{ Send telnet status }
procedure TIpDataSocket.dsSendTelnetStatus;
const
  StatPrefix = TELNET_IAC + TELNET_SB + TELNETOPT_STATUS + #0;
  StatPostfix = TELNET_IAC + TELNET_SE;
var
  StatString : string;
  Opt : AnsiChar;
begin
  { Verify we're in telnet mode and Status is enabled }
  if not FTelnet then Exit;
  if not(TELNETOPT_STATUS in toHostEnabled) then Exit;

  { Build status string }
  for Opt := TELNETOPT_FIRSTOPT to TELNETOPT_LASTOPT do begin

    { If we've got the option enabled, append to string }
    if Opt in toHostEnabled then
      StatString := StatString + TELNET_WILL + Opt;

    { If the remote has the option enabled (as far as we know), append }
    if Opt in toRemoteEnabled then
      StatString := StatString + TELNET_DO + Opt;
  end;

  { Escape characters as necessary }
  StatString := DoubleChar(StatString, TELNET_IAC);
  StatString := DoubleChar(StatString, TELNET_SB);

  { Add Prefix and Postfix }
  StatString := StatPrefix + StatString + StatPostfix;

  { Send string }
  dsPutData(StatString[1], Length(StatString), nil, False, True, True);
end;

{ Send terminal type }
procedure TIpDataSocket.dsSendTerminal;
const
  TermPrefix = TELNET_IAC + TELNET_SB + TELNETOPT_TERM + #0;
  TermPostfix = TELNET_IAC + TELNET_SE;
var
  TermType : string;
begin
  { Verify we're in telnet mode and Term is enabled }
  if not FTelnet then Exit;
  if not(TELNETOPT_TERM in toHostEnabled) then Exit;

  { Get terminal type }
  if Assigned(FMasterTerminal) then
    TermType := TIpCustomTerminal(FMasterTerminal).Emulator.TelnetTermType
  else
    TermType := 'dumb';

  { Escape characters as necessary }
  TermType := DoubleChar(TermType, TELNET_IAC);

  { Prepare string }
  TermType := TermPrefix + TermType + TermPostfix;

  { Send string }
  dsPutData(TermType[1], Length(TermType), nil, False, True, True);
end;

{ Send Terminal Size }
procedure TIpDataSocket.dsSendTerminalSize;
const
  SizePrefix = TELNET_IAC + TELNET_SB + TELNETOPT_NAWS;
  SizePostfix = TELNET_IAC + TELNET_SE;
var
  TermSize : string;
begin
  { Verify we're in telnet mode and NAWS is enabled }
  if not FTelnet then Exit;
  if not(TELNETOPT_NAWS in toHostEnabled) then Exit;

  { Get terminal size }
  if Assigned(FMasterTerminal) then begin
    TermSize := TIpCustomTerminal(FMasterTerminal).TelnetTermSize;

    { Escape characters as necessary }
    TermSize := DoubleChar(TermSize, TELNET_IAC);

    { Prepare string }
    TermSize := SizePrefix + TermSize + SizePostfix;

    { Send string }
    dsPutData(TermSize[1], Length(TermSize), nil, False, True, True);
  end;
end;

{ Graceful shutdown of socket }
procedure TIpDataSocket.dsShutDown;
begin
  FConnected := False;                                                 {!!.02}

  dsWritable := False;                                                 {!!.03}
  dsClearSendQueue;
  {IpSafeFree(dsSendQueue);}                                           {!!.02}
  IpWinSockAccess.Shutdown(FSocketHandle, 1);
  {FConnected := False;}                                               {!!.02}
end;

{ Send data -- return zero if all sent, or queue handle }
{ Data in queue will have already been processed (telnet, etc) }
function TIpDataSocket.dsPutData(var Buffer; BufferSize : Integer;
  Stream : TStream; Disconnect, Internal, Cmd : Boolean) : TIpHandle;
var
  QItem : PIpSendQueueItem;
  NewSize : Integer;
begin
  LockSock;
  try

    { Create QItem }
    QItem := AllocMem(SizeOf(TIpSendQueueItem));

    { Init fields }
    QItem.Disconnect := Disconnect;
    QItem.Internal := Internal;
    QItem.Stream := Stream;

    if Stream = nil then begin

      { Regular buffer job -- check if telnet }
      if FTelnet and not(Cmd) then begin

        { telnet, handle appropriately }
        NewSize := BufferSize + Integer(IpCharCount(Buffer, BufferSize, #255));
        GetMem(QItem.Buffer, NewSize);
        dsTelnetSnd(Buffer, QItem.Buffer^, BufferSize);
        QItem.BufferSize := NewSize;
        QItem.ActualData := BufferSize;
      end else begin

        { No telnet, copy buffer straight over }
        GetMem(QItem.Buffer, BufferSize);
        Move(Buffer, QItem.Buffer^, BufferSize);
        QItem.BufferSize := BufferSize;
        QItem.ActualData := BufferSize;
      end;

    end else begin

      { Stream -- pass off to get buffer filled }
      dsRefreshQueueItemBuffer(QItem);
    end;

    { Add item to queue }
    if (dsSendQueue.Add(QItem) = 0) and dsWritable then begin

      repeat
        { We have a shot at sending item, give it a go }
        if dsSendQueueItem(QItem) then begin

          { Are we supposed to disconnect? }
          if PIpSendQueueItem(dsSendQueue.Items[0]).Disconnect then begin

            { Shutdown process will free item }
            bsCloseSocket;
            PostMessage(bsOwner.Handle, CM_IPFREESOCKET, FSocketHandle, 0);  {!!.02}

          end else begin
            { Done with this item, free it }
            dsFreeQueueItem(QItem);
            dsSendQueue.Delete(0);
          end;

          { Item sent, we're done }
          Result := 0;
          Exit;
        end;
      until not(dsWritable);
    end;

    { Item is being queued, set and return handle }
    QItem.Handle := dsNextQueueHandle;
    Result := QItem.Handle;

  finally
    UnlockSock;
  end;
end;

{ Strips the high bit of every byte in the buffer }
procedure TIpDataSocket.dsStripHighBit(var Buffer; BufLen : DWORD);
var
  I : DWORD;
begin
  for I := 0 to BufLen-1 do
    TByteArray(Buffer)[I] := TByteArray(Buffer)[I] and $7F;
end;

{ Socks processing for incoming data }
function TIpDataSocket.dsSocksRcv(var Buffer; BufLen : DWORD) : DWORD;
var
  Socks4Rec : TIpSocks4Rec;
  Socks5Neg : TIpSocks5NegRec;
  Socks5Rec : TIpSocks5Rec;
  SendBuf : Pointer;
  BufSize : Integer;
  ULen, PLen, BufInx : Integer;
begin
  Result := BufLen;
  case bsSocks.SocksVersion of
    svSocks4, svSocks4a :
      begin
        if BufLen >= SizeOf(TIpSocks4Rec) then begin
          Move(Buffer, Socks4Rec, SizeOf(TIpSocks4Rec));
          if Socks4Rec.Cmd = 90 then begin
            { Life is good }
            bsRemoteSockAddr.sin_port := Socks4Rec.Port;
            bsRemoteSockAddr.sin_addr := Socks4Rec.Addr;
            bsSocksState := ssConnected;
            bsConnect;
          end else begin
            { Socks error occurred }
            bsOwner.DoError(SocketHandle, Socks4Rec.Cmd,
              Format(SSocksErr, [Socks4Rec.Cmd]));
          end;
          Dec(Result, SizeOf(TIpSocks4Rec));
        end;
      end;

    svSocks5 :
      begin
        case bsSocksState of
          ssNegotiating :
            begin
              if BufLen >= SizeOf(TIpSocks5NegRec) then begin
                Move(Buffer, Socks5Neg, SizeOf(TIpSocks5NegRec));

                case Socks5Neg.Meth of
                  $00 :
                    begin
                      { Prepare data }
                      Socks5Rec.Ver := 5;
                      Socks5Rec.Cmd := 1;
                      Socks5Rec.Rsv := 0;
                      Socks5Rec.Atyp := 1;
                      Socks5Rec.Addr := bsRemoteSockAddr.sin_addr;
                      Socks5Rec.Port := bsRemoteSockAddr.sin_port;
                      { Send the data }
                      dsPutData(Socks5Rec, SizeOf(Socks5Rec), nil, False, True, True);
                      bsSocksState := ssRequesting;
                    end;

                  $02 :
                    begin
                      ULen := Length(bsSocks.UserCode);
                      if ULen > 255 then ULen := 255;
                      PLen := Length(bsSocks.Password);
                      if PLen > 255 then PLen := 255;
                      BufSize := ULen + PLen + 3;
                      SendBuf := AllocMem(BufSize);
                      try
                        PByteArray(SendBuf)[0] := 1;
                        PByteArray(SendBuf)[1] := ULen;
                        BufInx := 2;
                        Move(bsSocks.UserCode, PByteArray(SendBuf)[BufInx], ULen);
                        Inc(BufInx, ULen);
                        PByteArray(SendBuf)[BufInx] := PLen;
                        Inc(BufInx);
                        Move(bsSocks.Password, PByteArray(SendBuf)[BufInx], PLen);
                        { Send the data }
                        dsPutData(SendBuf^, BufSize, nil, False, True, True);
                      finally
                        FreeMem(SendBuf);
                      end;
                      bsSocksState := ssAuthenticating;
                    end;

                  $FF :
                    begin
                      { Socks error occurred }
                      bsOwner.DoError(SocketHandle, Socks5Neg.Meth,
                        Format(SSocksErr, [Socks5Neg.Meth]));
                    end;
                end;
                Dec(Result, SizeOf(TIpSocks5NegRec));
              end;
            end;

          ssAuthenticating :
            begin
              if BufLen >= SizeOf(TIpSocks5NegRec) then begin
                Move(Buffer, Socks5Neg, SizeOf(TIpSocks5NegRec));
                if Socks5Neg.Meth = 0 then begin
                  { Prepare data }
                  Socks5Rec.Ver := 5;
                  Socks5Rec.Cmd := 1;
                  Socks5Rec.Rsv := 0;
                  Socks5Rec.Atyp := 1;
                  Socks5Rec.Addr := bsRemoteSockAddr.sin_addr;
                  Socks5Rec.Port := bsRemoteSockAddr.sin_port;
                  { Send the data }
                  dsPutData(Socks5Rec, SizeOf(Socks5Rec), nil, False, True, True);
                  bsSocksState := ssRequesting;
                end else begin
                  { Socks error occurred }
                  bsOwner.DoError(SocketHandle, Socks5Neg.Meth,
                    Format(SSocksErr, [Socks5Neg.Meth]));
                end;
                Dec(Result, SizeOf(TIpSocks5NegRec));
              end;
            end;

          ssRequesting :
            begin
              if BufLen >= SizeOf(TIpSocks5Rec) then begin
                Move(Buffer, Socks5Rec, SizeOf(TIpSocks5Rec));
                if Socks5Rec.Cmd = 0 then begin
                  { Life is good }
                  bsRemoteSockAddr.sin_port := Socks5Rec.Port;
                  bsRemoteSockAddr.sin_addr := Socks5Rec.Addr;
                  bsSocksState := ssConnected;
                  bsConnect;
                end else begin
                  { Socks error occurred }
                  bsOwner.DoError(SocketHandle, Socks5Rec.Cmd,
                    Format(SSocksErr, [Socks5Rec.Cmd]));
                end;
                Dec(Result, SizeOf(TIpSocks5NegRec));
              end;
            end;
        end;
      end;
  end;
end;

{ Telnet processing for incoming data }
{ Returns final length -- should be same or shorter }
function TIpDataSocket.dsTelnetRcv(var Buffer; BufLen : DWORD) : DWORD;
var
  I, J, X, SubLen : DWORD;
  Found : Boolean;
  SubNeg : string;
begin
  Result := 0;
  Found := False;

  { Bail if nothing to do }
  if BufLen = 0 then Exit;

  LockSock;
  try
    if dsTelnetCmdTail = 0 then begin

      { Check for simple case (no telnet stuff) }
      for I := 0 to BufLen-1 do begin
        if (TIpCharArray(Buffer)[I] = TELNET_IAC) then begin
          Found := True;
          Break;
        end;
      end;

      { No telnet commands, bail }
      if not Found then begin
        Result := BufLen;
        Exit;
      end;
    end;

    { We know at this point we have telnet stuff to process... }

    { Append data to any partial command from previous pass }
    ReallocMem(dsTelnetCmd, BufLen+dsTelnetCmdTail);
    Move(Buffer, dsTelnetCmd[dsTelnetCmdTail], BufLen);
    Inc(dsTelnetCmdTail, BufLen);

    I := 0;
    J := 0;

    while True do begin

      { Process buffer }
      if dsTelnetCmd[I] <> TELNET_IAC then begin

        { ASCII mode, CR/NULL received -- strip the null per RFC 854 }
        if not (dsTelnetCmd[I+1] = TELNET_NULL) and
               (dsTelnetCmd[I] = TELNET_CR) and
               (TELNETOPT_BINARY in toRemoteEnabled) then begin
          TIpCharArray(Buffer)[J] := dsTelnetCmd[I];
          Inc(J);
          Inc(I,2);
        end else begin

          { Copy over verbatim }
          TIpCharArray(Buffer)[J] := dsTelnetCmd[I];
          Inc(I);
          Inc(J);
        end;
        if I >= dsTelnetCmdTail then Break;

      end else begin
        { Bail if at the end of the buffer }
        if I >= dsTelnetCmdTail then Break;

        { Deal with telnet commands apropriately }
        if dsTelnetCmd[I+1] = TELNET_IAC then begin

          { It's an escaped IAC -- copy a single $FF over }
          TIpCharArray(Buffer)[J] := dsTelnetCmd[I];
          Inc(J);
          Inc(I,2);

          if I >= dsTelnetCmdTail then Break;

        end else begin

          { If at the end of the buffer and we made it this far, exit loop }
          if I+2 >= dsTelnetCmdTail then Break;

          if dsTelnetCmd[I+1] <> TELNET_SB then begin

            { It's an actual command -- strip it and act on it }
            if dsHandleTelnet(dsTelnetCmd[I+1], dsTelnetCmd[I+2]) then
              Inc(I,3)
            else
              Inc(I,2);

            if I >= dsTelnetCmdTail then Break;

          end else begin
            { It's a subnegotation situation -- look for the end }
            X := I;
            while X <= dsTelnetCmdTail do begin

              { Look for the IAC }
              if dsTelnetCmd[X] <> TELNET_IAC then begin
                Inc(X);
                Continue;
              end;

              { If TELNET_SE is next, we've got it }
              if dsTelnetCmd[X+1] = TELNET_SE then begin
                SubLen := X-I-2;
                SetLength(SubNeg, SubLen);
                Move(dsTelnetCmd[I+2], SubNeg[1], SubLen);
                dsHandleSubnegotiation(SubNeg);
                I := X+2;
                Break;
              end;
              Inc(X);
            end;
          end;
        end;
      end;
    end;

    { Get any remaining data ready for next pass }
    if I < dsTelnetCmdTail then begin
      Move(dsTelnetCmd[I], dsTelnetCmd[0], dsTelnetCmdTail-I);
      dsTelnetCmdTail := dsTelnetCmdTail-I;
    end else begin
      dsTelnetCmdTail := 0;
    end;

    { J now reflects the data in the receive buffer }
    Result := J;

  finally
    UnlockSock;
  end;
end;

{ Telnet processing for outgoing data }
procedure TIpDataSocket.dsTelnetSnd(var InBuf, OutBuf; InBufLen : Integer);
var
  I, J : Integer;
begin
  I := 0;
  J := 0;
  while I < InBufLen do begin

    { Copy bytes over }
    TByteArray(OutBuf)[J] := TByteArray(InBuf)[I];

    { If it's an $FF, escape it }
    if TByteArray(OutBuf)[J] = $FF then begin
      Inc(J);
      TByteArray(OutBuf)[J] := $FF;
    end;

    { Inc pointers }
    Inc(I);
    Inc(J);
  end;
end;

{ Write data to all attached terminals }
procedure TIpDataSocket.dsWriteTerm(var Buffer; BufLen : DWORD);
var
  I : Integer;
  Buf : Pointer;
begin
  { Test bailout conditions }

  { No attached terminals }
  if not Assigned(dsTerminalList) then Exit;
  if (dsTerminalList.Count < 1) then Exit;

  { No data to send }
  if (BufLen = 0) then Exit;

  for I := 0 to dsTerminalList.Count-1 do begin
    { Get memory for data -- memory released in message handler }
    GetMem(Buf, BufLen);
    { Copy data }
    Move(Buffer, Buf^, BufLen);
    { Send data to terminal }
    PostMessage(TIpHWND(dsTerminalList[I]), CM_IPTERMDATA, LongInt(Buf), BufLen);  {!!.12}
  end;
end;

{ Retrieve the fixed line length for the socket }
function TIpDataSocket.GetFixedLineLength : DWORD;
begin
  LockSock;
  try
    Result := FFixedLineLength;
  finally
    UnlockSock;
  end;
end;

{ Retrieve the idle time for the socket }
function TIpDataSocket.GetIdleTime : DWORD;
begin
  LockSock;
  try
    Result := FIdleTime;
  finally
    UnlockSock;
  end;
end;

{ Retrieve the line term char for line mode reading }
function TIpDataSocket.GetLineTermChar : AnsiChar;
begin
  LockSock;
  try
    Result := FLineTermChar;
  finally
    UnlockSock;
  end;
end;

{ Retrieve the line delimiter for line mode reading }
function TIpDataSocket.GetLineTerminator : TIpLineTerminator;
begin
  LockSock;
  try
    Result := FLineTerminator;
  finally
    UnlockSock;
  end;
end;

{!!.01 - added}
{ Retrieve the log options }
function TIpDataSocket.GetLogOptions : TIpLogOptionSet;
begin
  LockSock;
  try
    Result := FLogOptions;
  finally
    UnlockSock;
  end;
end;

{ For internal use by the terminal }
function TIpDataSocket.GetMasterTerminal : TIpBaseWinControl;
begin
  LockSock;
  try
    Result := FMasterTerminal;
  finally
    UnlockSock;
  end;
end;

{ Retrieve receive buffer size }
function TIpDataSocket.GetReceiveBufferSize : Integer;
var
  ErrCode, OptSize : Integer;
begin
  OptSize := SizeOf(Result);
  LockSock;
  try
    ErrCode := IpWinSockAccess.GetSockOpt(FSocketHandle,
      Sol_Socket, So_RcvBuf, Result, OptSize);

    if ErrCode = Socket_Error then begin

      ErrCode := bsGetLastError;

      if (ErrCode = wsaENoProtoOpt) or (ErrCode = wsaEInval) then
        { "Expected" error; handle appropriately }
        Result := 0

      else
        { "Unexpected" error -- bummer }
        raise EIpWinSockError.CreateWsError(ErrCode, 'getsockopt');

    end;

  finally
    UnlockSock;
  end;
end;

{ Retrieve the current receive mode }
function TIpDataSocket.GetReceiveMode : TIpReceiveMode;
begin
  LockSock;
  try
    Result := FReceiveMode;
  finally
    UnlockSock;
  end;
end;

{ Retrieve handle to receive stream }
function TIpDataSocket.GetReceiveStream : TStream;
begin
  LockSock;
  try
    Result := FReceiveStream;
  finally
    UnlockSock;
  end;
end;

{ Retrieve send buffer size }
function TIpDataSocket.GetSendBufferSize : Integer;
var
  ErrCode, OptSize : Integer;
begin
  OptSize := SizeOf(Result);
  LockSock;
  try
    ErrCode := IpWinSockAccess.GetSockOpt(FSocketHandle,
      Sol_Socket, So_SndBuf, Result, OptSize);

    if ErrCode = Socket_Error then begin

      ErrCode := bsGetLastError;

      if (ErrCode = wsaENoProtoOpt) or (ErrCode = wsaEInval) then
        { "Expected" error; handle appropriately }
        Result := 0

      else
        { "Unexpected" error -- bummer }
        raise EIpWinSockError.CreateWsError(ErrCode, 'getsockopt');

    end;

  finally
    UnlockSock;
  end;
end;

{ Retrieve StripLineTerminator property }
function TIpDataSocket.GetStripLineTerminator : Boolean;
begin
  LockSock;
  try
    Result := FStripLineTerminator;
  finally
    UnlockSock;
  end;
end;

{ Queue block for send }
function TIpDataSocket.PutBlock(var Buffer; BufferSize : Integer;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := dsPutData(Buffer, BufferSize, nil, Disconnect, False, False);
end;

{ Queue stream for send }
function TIpDataSocket.PutStream(const Stm : TStream;
  Disconnect : Boolean) : TIpHandle;
var
  Temp : Pointer;
begin
  Temp := nil;
  Result := dsPutData(Temp, 0, Stm, Disconnect, False, False);
end;

{ Queue string for send }
function TIpDataSocket.PutString(const Str : string;
  Disconnect : Boolean) : TIpHandle;
var
  Temp : PChar;
begin
  Temp := PChar(Str);
  Result := dsPutData(Temp^, Length(Str), nil, Disconnect, False, False);
end;

{ Resets status record }
procedure TIpDataSocket.ResetStats;
begin
  LockSock;
  try
    { Zero all the resetable fields }
    dsSockStats.BytesRcvd := 0;
    dsSockStats.BytesSent := 0;
    dsSockStats.RawBytesRcvd := 0;
    dsSockStats.RawBytesSent := 0;
    dsSockStatsDirty := False;
  finally
    UnlockSock;
  end;
end;

{ Set value for fixed line length timeout }
procedure TIpDataSocket.SetFixedLineLength(const Value : DWORD);
begin
  LockSock;
  try
    FFixedLineLength := Value;
  finally
    UnlockSock;
  end;
end;

{ Set value for idle timeout }
procedure TIpDataSocket.SetIdleTimeout(const Value : DWORD);
begin
  LockSock;
  try
    FIdleTimeout := Value;
  finally
    UnlockSock;
  end;
end;

{ Set the line term char for line mode }
procedure TIpDataSocket.SetLineTermChar(const Value : AnsiChar);
begin
  LockSock;
  try
    FLineTermChar := Value;
  finally
    UnlockSock;
  end;
end;

{ Set the line delimiter for line mode }
procedure TIpDataSocket.SetLineTerminator(const Value : TIpLineTerminator);
begin
  LockSock;
  try
    FLineTerminator := Value;
  finally
    UnlockSock;
  end;
end;

{!!.01 - added}
{ Set the logging options }
procedure TIpDataSocket.SetLogOptions(const Value : TIpLogOptionSet);
begin
  LockSock;
  try
    FLogOptions := Value;
  finally
    UnlockSock;
  end;
end;

{ For internal use by the terminal }
procedure TIpDataSocket.SetMasterTerminal(const Value : TIpBaseWinControl);
begin
  LockSock;
  try
    FMasterTerminal := Value;
  finally
    UnlockSock;
  end;
end;

{ Set the size of the system receive buffer }
procedure TIpDataSocket.SetReceiveBufferSize(const Value : Integer);
var
  ErrCode, OptSize, TrySize : Integer;
begin
  TrySize := Value;
  OptSize := SizeOf(TrySize);

  LockSock;
  try
    while TrySize > MtuSize do begin

      { Give the requested size a whirl }
      ErrCode := IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
        So_RcvBuf, TrySize, OptSize);

      { Check for errors }
      if ErrCode = Socket_Error then begin

        ErrCode := bsGetLastError;

        { Changing buffer size apparently not supported, exit silently }
        if (ErrCode = wsaENoProtoOpt) or (ErrCode = wsaEInval) then Exit;

        { Reduce size and try again }
        TrySize := TrySize div 2;

      end else Exit;
    end;
  finally
    UnlockSock;
  end;
end;

{ Set the receive mode }
procedure TIpDataSocket.SetReceiveMode(const Value : TIpReceiveMode);
begin
  LockSock;
  try
    FReceiveMode := Value;
  finally
    UnlockSock;
  end;
end;

{ Set the receive stream }
procedure TIpDataSocket.SetReceiveStream(const Value : TStream);
begin
  LockSock;
  try
    FReceiveStream := Value;
  finally
    UnlockSock;
  end;
end;

{ Set the size of the system send buffer }
procedure TIpDataSocket.SetSendBufferSize(const Value : Integer);
var
  ErrCode, OptSize, TrySize : Integer;
begin
  TrySize := Value;
  OptSize := SizeOf(TrySize);

  LockSock;
  try
    while TrySize > MtuSize do begin

      { Give the requested size a whirl }
      ErrCode := IpWinSockAccess.SetSockOpt(FSocketHandle, Sol_Socket,
        So_SndBuf, TrySize, OptSize);

      { Check for errors }
      if ErrCode = Socket_Error then begin

        ErrCode := bsGetLastError;

        { Changing buffer size apparently not supported, exit silently }
        if (ErrCode = wsaENoProtoOpt) or (ErrCode = wsaEInval) then Exit;

        { Reduce size and try again }
        TrySize := TrySize div 2;

      end else Exit;
    end;
  finally
    UnlockSock;
  end;
end;

{ Set the StripLineTerminator mode }
procedure TIpDataSocket.SetStripLineTerminator(const Value : Boolean);
begin
  LockSock;
  try
    FStripLineTerminator := Value;
  finally
    UnlockSock;
  end;
end;

{ Return a snapshot of the current stats }
procedure TIpDataSocket.StatusSnapshot(var StatRec : TIpSockStatRec);
begin
  LockSock;
  try
    Move(dsSockStats, StatRec, SizeOf(StatRec));
  finally
    UnlockSock;
  end;
end;


{ TIpListenSocket }

{ Creates instance of TIpListenSocket }
constructor TIpListenSocket.Create(Owner : TIpSockControl; Socket : TSocket;
  var LocalAddress : TSockAddrIn; const Inits : TIpBaseInits;
  const Socks : TIpSocksServerInfo);
var
  ErrCode : Integer;
begin
  inherited;
  ErrCode := IpWinSockAccess.Listen(FSocketHandle, 5);
  if ErrCode = Socket_Error then begin
    ErrCode := bsGetLastError;
    raise EIpWinSockError.CreateWsError(ErrCode, 'Listen');
  end;
end;

{ Sets Async Options -- when overridden in descendant classes, }
{ "Inherited" will typically *not* be called. }
procedure TIpListenSocket.bsSetAsyncOptions;
var
  ErrCode : Integer;
begin
  if (FSocketHandle <> Invalid_Socket) then begin
    ErrCode := IpWinSockAccess.WsaAsyncSelect(FSocketHandle, bsOwner.Handle,
      CM_IPSOCKMESSAGE, (Fd_Accept or Fd_Read or Fd_Write or Fd_Close));

    if ErrCode = Socket_Error then begin
      ErrCode := bsGetLastError;
      raise EIpWinSockError.CreateWsError(ErrCode, 'WsaAsyncSelect');
    end;
  end;
end;

{ TIpSockControl }

{ Creates instance of TIpSockControl }
constructor TIpSockControl.Create(AOwner : TComponent);
begin
  inherited;
{$IFDEF Version6OrHigher}
  FHandle := Classes.AllocateHWnd(WndProc);
{$ELSE}
  FHandle := AllocateHWnd(WndProc);
{$ENDIF}
  FDebugLog := TIpDebugLog.Create(Self);
  FEventLog := TIpEventLog.Create(Self);
  FSockInits := TIpDataInits.Create;
  scReplyList := TThreadList.Create;
  scTerminalList := TList.Create;

  { Initialize event log }
  FEventLog.FDateTimeFormat := 'yyyy.mm.dd hh:nn:ss';
  FEventLog.FFileName := 'event.log';

  { Initialize scLocalSockAddr }
  Move(DefSockAddrIn, scLocalSockAddr, SizeOf(TSockAddrIn));

  { Initialize property through method to start timer }
  TimerInterval := 500;

  { Initialize other properties }
  FDefaultLocalAddress := 0;
  FStatusEvents := [stConnect, stDisconnect, stProgress];

  { For this one, go through the set method }
  SockProtocol := spTcp;
end;

{ Destroys instance of TIpSockControl }
destructor TIpSockControl.Destroy;
var
  I : Integer;
  LockedList : TList;
begin
  { Shut down timer }
  TimerInterval := 0;

  { If there are any outstanding async requests, kill them }
  LockedList := scReplyList.LockList;
  try
    for I := LockedList.Count-1 downto 0 do begin
      IpWinSockAccess.WsaCancelAsyncRequest(TIpReplyBuf(LockedList[I]^).Handle);
      FreeMem(LockedList[I]);
      LockedList.Delete(I);
    end;
  finally
    { Don't use LockedList beyond this point }
    scReplyList.UnlockList;
  end;

  { Dump debug log }
  if Assigned(FDebugLog) and not(csDesigning in ComponentState) then   {!!.01}
    if not(FDebugLog.BufferEmpty) then                                 {!!.01}
      FDebugLog.DumpLog;

  { Free everything else }
  IpSafeFree(scTerminalList);
  IpSafeFree(scReplyList);
  IpSafeFree(FSockInits);
  IpSafeFree(FEventLog);
  IpSafeFree(FDebugLog);
{$IFDEF Version6OrHigher}
  Classes.DeallocateHWnd(FHandle);
{$ELSE}
  DeallocateHWnd(FHandle);
{$ENDIF}
  inherited;
end;

{ Gives text representation of a block of data }
function HexifyBlock(var Buffer; BufferSize : Integer) : string;
const
  { Starting string to work with - this is directly written to by index }
  { below, so any positional changes here will also have to be made below. }
  StockString = '  %6.6x: 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 : 0000000000000000' + IpCRLF;
  HexDigits : array[0..$F] of AnsiChar = '0123456789ABCDEF';
var
  I, J, K, Lines : Integer;
  TempStr : string;
  Hex1, Hex2 : array[0..23] of AnsiChar;
  Ascii1, Ascii2 : array[0..7] of AnsiChar;
begin
  K := 0;
  Result := '';                                                        {!!.12}
  FillChar(Hex1, SizeOf(Hex1), #32);
  FillChar(Hex2, SizeOf(Hex2), #32);

  { Calculate number of lines required }
  Lines := BufferSize div 16;
  if (BufferSize mod 16) <> 0 then Inc(Lines);

  { Process and append lines }
  for I := 0 to Lines-1 do begin

    { Load string, add index marker }
    TempStr := Format(StockString, [I*16]);

    { Format data for first word }
    for J := 0 to 7 do begin
      if J+K >= BufferSize then begin
        Ascii1[J] := ' ';
        Hex1[J*3] := ' ';
        Hex1[J*3+1] := ' ';
      end else begin
        Ascii1[J] := TIpCharArray(Buffer)[J+K];
        Hex1[J*3] := HexDigits[Byte(Ascii1[J]) shr 4];
        Hex1[J*3+1] := HexDigits[Byte(Ascii1[J]) and $F];

        { Clamp Ascii to printable range }
        if (Ascii1[J] < #32) or (Ascii1[J] > #126) then Ascii1[J] := '.';
      end;
    end;
    Inc(K,8);

    { Format data for second word }
    for J := 0 to 7 do begin
      if J+K >= BufferSize then begin
        Ascii2[J] := ' ';
        Hex2[J*3] := ' ';
        Hex2[J*3+1] := ' ';
      end else begin
        Ascii2[J] := TIpCharArray(Buffer)[J+K];
        Hex2[J*3] := HexDigits[Byte(Ascii2[J]) shr 4];
        Hex2[J*3+1] := HexDigits[Byte(Ascii2[J]) and $F];
        { Clamp Ascii to printable range }
        if (Ascii2[J] < #32) or (Ascii2[J] > #126) then Ascii2[J] := '.';
      end;
    end;
    Inc(K,8);

    { Move data to existing temp string }
    Move(Hex1[0], TempStr[11], SizeOf(Hex1));
    Move(Hex2[0], TempStr[36], SizeOf(Hex2));

    Move(Ascii1[0], TempStr[62], SizeOf(Ascii1));
    Move(Ascii2[0], TempStr[70], SizeOf(Ascii2));

    { Append temp string to result }
    Result := Result + TempStr;
  end;
end;

{ Formats log strings that we know about }
class function TIpSockControl.GetLogString(const S, D1, D2, D3 : DWORD) : string;
var
  ByteStr : string;
begin
  { Awww, what the heck }
  if D3 = 1 then
    ByteStr := 'byte'
  else
    ByteStr := 'bytes';

  { Format and return appropriate string }
  case D1 of
    scReadData  :
      begin
        Result := Format('Read - %d %s read from socket %d' + IpCRLF, [D3, ByteStr, S]);
        Result := Result + HexifyBlock(PByteArray(D2)[0], D3);
      end;

    scWriteData :
      begin
        Result := Format('Write - %d %s written to socket %d' + IpCRLF, [D3, ByteStr, S]);
        Result := Result + HexifyBlock(PByteArray(D2)[0], D3);
      end;

    scRawRead   :
      begin
        Result := Format('[%d] Recv API: %d %s read' + IpCRLF, [S, D3, ByteStr]);
        Result := Result + HexifyBlock(PByteArray(D2)[0], D3);
      end;

    scRawWrite  :
      begin
        Result := Format('[%d] Send API: %d %s written' + IpCRLF, [S, D3, ByteStr]);
        Result := Result + HexifyBlock(PByteArray(D2)[0], D3);
      end;
  end;
end;

{ Message handler for async lookup replies }
procedure TIpSockControl.CMIpAsyncResult(var Message : TCMIpAsyncResult);
var
  I : Integer;
  TempBuf : PIpReplyBuf;
  LockedList : TList;
begin
  TempBuf := nil;
  try
    { Find the matching buffer }
    LockedList := scReplyList.LockList;
    try
      for I := 0 to LockedList.Count-1 do begin
        TempBuf := PIpReplyBuf(LockedList[I]);
        if TempBuf <> nil then begin
          if TempBuf.Handle = Message.Handle then begin
            { Copy the pointer to our local variable, delete from list }
            LockedList.Delete(I);
            Break;
          end;
        end;
      end;
    finally
      { Don't use LockedList beyond this point }
      scReplyList.UnlockList
    end;

    { Safety check }
    if TempBuf = nil then
      Exit;

    { Check for async error, firing event if appropriate }
    if Message.AsyncError <> 0 then begin
      DoErrorReply(TempBuf.Handle, Message.AsyncError,
        Format(SWinSockErr, [Message.AsyncError,
        WinSockErrorMessage(Message.AsyncError),
        AsyncAPIStr(TempBuf.ReplyType)]));
      Exit;
    end;

    { Determine reply type and handle accordingly }
    case TempBuf.ReplyType of
      rtAddress :
        begin
          { Fire OnAddressReply event if assigned }
          DoAddressReply(TempBuf.Handle, ExtractAddress(@TempBuf.Buffer, 0));
        end;

      rtName :
        begin
          { Fire OnNameReply event if assigned }
          DoNameReply(TempBuf.Handle, ExtractName(@TempBuf.Buffer));
        end;

      rtPort :
        begin
          { Fire OnPortReply event if assigned }
          DoPortReply(TempBuf.Handle, ExtractPort(@TempBuf.Buffer));
        end;

      rtService :
        begin
          { Fire OnServiceReply event if assigned }
          DoServiceReply(TempBuf.Handle, ExtractService(@TempBuf.Buffer));
        end;
    end;

  finally
    { We're done with the record, free it }
    FreeMem(TempBuf);
  end;
end;

{ Frees TIpBaseSocket instance associated with socket }
procedure TIpSockControl.CMIpFreeSocket(var Message : TCMIpSockMessage);
begin
  scFreeSocket(Message.Socket);
end;

{ Message handler for socket messages }
procedure TIpSockControl.CMIpLineMessage(var Message : TCMIpLineMessage);
var
  Str : string;
begin
  { Make the string }
  SetString(Str, Message.Line.Str, Message.Line.Count);

  { Free memory }
  FreeMem(Message.Line.Str);
  FreeMem(Message.Line);

  { Fire event if one exists }
  if Assigned(FOnReadLine) then begin
    FOnReadLine(Self, Message.Socket, Str);
  end;
end;

{ Message handler for socket messages }
procedure TIpSockControl.CMIpSockMessage(var Message : TCMIpSockMessage);
begin
  with Message do try                                                  {!!.10}
    if SelectError = 0 then begin
      case SelectEvent of
        Fd_Read    : scRead(Socket);
        Fd_Write   : scWrite(Socket);
        Fd_Accept  : scAccept(Socket);
        Fd_Connect : scConnect(Socket);
        Fd_Close   : scClose(Socket);
        Fd_Oob     : scOob(Socket);
      end;
    end else begin
      DoError(Socket, SelectError, Format(SWinSockErr,
        [SelectError, WinSockErrorMessage(SelectError),
        MsgAPIStr(SelectEvent)]));
      if (SelectEvent = Fd_Close) then                                 {!!.03}
        scClose(Socket);                                               {!!.03}
    end;
  except                                                               {!!.10}
    on E : EIpWinSockError do DoError(Socket, E.ErrorCode, E.Message); {!!.10}
  else                                                                 {!!.10}
    Application.HandleException(Self);                                 {!!.10}
  end;
end;

{ Sends notification of terminal resizing }
procedure TIpSockControl.CMIpTermResize(var Message : TMessage);
var
  Sckt : TIpDataSocket;
begin
  Sckt := scGetSocket(Message.WParam, False);
  Sckt.dsSendTerminalSize;
end;

{ Removes terminal from notification list }
procedure TIpSockControl.DeRegisterTerminal(Handle : TIpHWND);         {!!.12}
var
  I : Integer;
begin
  I := scTerminalList.IndexOf(Pointer(Handle));
  if I <> -1 then
    scTerminalList.Delete(I);
end;

{ Handle asynchronous address reply }
procedure TIpSockControl.DoAddressReply(Handle : TIpHandle;
  const Address : TInAddr);
begin
  if Assigned(FOnAddressReply) then
    FOnAddressReply(Self, Handle, Address);
end;

{ Handle general WinSock errors }
procedure TIpSockControl.DoError(Socket : TSocket; ErrCode : Integer;
  const ErrStr : string);
begin
  if Assigned(FOnError) then
    FOnError(Self, Socket, ErrCode, ErrStr);
end;

{ Handle asynchronous reply error }
procedure TIpSockControl.DoErrorReply(Handle : TIpHandle;
  ErrCode : Integer; const ErrStr : string);
begin
  if Assigned(FOnErrorReply) then
    FOnErrorReply(Self, Handle, ErrCode, ErrStr);
end;

{ Handle IdleTimeout from socket }
procedure TIpSockControl.DoIdleTimeout(Socket : TSocket; var Reset : Boolean);
begin
  if Assigned(FOnIdleTimeout) then
    FOnIdleTimeout(Self, Socket, Reset);
end;

{ Handle ItemSent }
procedure TIpSockControl.DoItemSent(Socket : TSocket; Handle : TIpHandle;
  Remaining : DWORD);
begin
  { Fire event if one exists }
  if Assigned(FOnItemSent) then
    FOnItemSent(Self, Socket, Handle, Remaining);
end;

{ Handle asynchronous name reply }
procedure TIpSockControl.DoNameReply(Handle : TIpHandle; const Name : string);
begin
  if Assigned(FOnNameReply) then
    FOnNameReply(Self, Handle, Name);
end;

{ Handle asynchronous port reply }
procedure TIpSockControl.DoPortReply(Handle : TIpHandle; Port : Integer);
begin
  if Assigned(FOnPortReply) then
    FOnPortReply(Self, Handle, Port);
end;

{ Handle ReadLine }
procedure TIpSockControl.DoReadLine(Socket : TSocket; const Line : string);
var
  StrRec : PIpLineRec;
begin
  if FDirectReadLine then begin                                        {!!.03}
    { Fire event if one exists (without posting message) }             {!!.03}
    if Assigned(FOnReadLine) then begin
      FOnReadLine(Self, Socket, Line);                                 {!!.03}
    end;
  end else begin                                                       {!!.03}
    { Fire event via posted message (the default way) }
    { Get memory for rec and string; will get freed in message handler }
    GetMem(StrRec, SizeOf(TIpLineRec));
    StrRec.Count := Length(Line);
    GetMem(StrRec.Str, StrRec.Count);

    { Copy data }
    Move(Line[1], StrRec.Str^, StrRec.Count);

    { Away she goes }
    PostMessage(FHandle, CM_IPLINEMESSAGE, Socket, LongInt(StrRec));
  end;                                                                 {!!.03}
end;

{ Handle asynchronous service reply }
procedure TIpSockControl.DoServiceReply(Handle : TIpHandle;
  const Service : string);
begin
  if Assigned(FOnServiceReply) then
    FOnNameReply(Self, Handle, Service);
end;

{ Handle status events }
procedure TIpSockControl.DoStatus(Socket : TSocket; Event : TIpStatusType;
  Connection : TIpConnRec);
var
  Stats : TIpSockStatRec;
  Local, Remote : string;
begin
  { Fire status event as appropriate }
  if Assigned(FOnStatus) and (Event in FStatusEvents) then begin
    scGetSocket(Socket, True).StatusSnapshot(Stats);
    FOnStatus(Self, Socket, Event, Connection, Stats);
  end;

  { Log connection and disconnection events }
  case Event of
    stConnect :
      begin
        { Stringify the address / port }
        Remote := NetAddr2String(Connection.RemoteAddr) +
          ':' + IntToStr(Connection.RemotePort);

        Local := NetAddr2String(Connection.LocalAddr) +
          ':' + IntToStr(Connection.LocalPort);

        { Log connection to event log }
        if Assigned(FEventLog) then
          FEventLog.WriteEventString(Format(SEventConnect, [Local, Remote]));
      end;

    stDisconnect :
      begin
        { Stringify the address / port }
        Remote := NetAddr2String(Connection.RemoteAddr) +
          ':' + IntToStr(Connection.RemotePort);

        Local := NetAddr2String(Connection.LocalAddr) +
          ':' + IntToStr(Connection.LocalPort);

        { Log disconnection to event log }
        if Assigned(FEventLog) then
          FEventLog.WriteEventString(Format(SEventDisconnect, [Local, Remote]));
      end;
  end;
end;

{ Extracts indexed address from a HostEnt }
function TIpSockControl.ExtractAddress(HostEnt : PHostEnt;
  Index : Integer) : TInAddr;
begin
  FillChar(Result, SizeOf(Result), #0);

  { Nil check }
  if HostEnt = nil then Exit;

  { Index out of range, raise exception }
  if (Index <> 0) and (Index >= ExtractAddressCount(HostEnt)) then
    raise EIpIndexError.Create(SIndexErr);

  { Look up indexed address }
  if Assigned(HostEnt) then
    Result.S_addr := HostEnt.h_addr_list[Index].S_addr;
end;

{ Extracts address count from a HostEnt }
function TIpSockControl.ExtractAddressCount(HostEnt : PHostEnt) : Integer;
var
  I : Integer;
begin
  Result := 0;

  { Nil check }
  if HostEnt = nil then Exit;

  { Iterate MaxAddrs -- if nil found, bail }
  for I := 0 to MaxAddrs-1 do begin
    if (HostEnt.h_addr_list[I] = nil) then begin
      Result := I;
      Exit;
    end;
  end;
end;

{ Extracts indexed alias from a HostEnt }
function TIpSockControl.ExtractAlias(HostEnt : PHostEnt;
  Index : Integer) : string;
begin
  { Nil check }
  if HostEnt = nil then Exit;

  { Index out of range, raise exception }
  if (Index <> 0) and (Index >= ExtractAliasCount(HostEnt)) then
    raise EIpIndexError.Create(SIndexErr);

  { Look up indexed address }
  if Assigned(HostEnt) then
    SetString(Result, HostEnt.h_aliases[Index],
      StrLen(HostEnt.h_aliases[Index]));
end;

{ Extracts alias count from a HostEnt }
function TIpSockControl.ExtractAliasCount(HostEnt : PHostEnt) : Integer;
var
  I : Integer;
begin
  Result := 0;

  { Nil check }
  if HostEnt = nil then Exit;

  { Iterate MaxAlias -- if nil found, bail }
  for I := 0 to MaxAlias-1 do begin
    if (HostEnt.h_aliases[I] = nil) then begin
      Result := I;
      Exit;
    end;
  end;
end;

{ Extracts official name from a HostEnt }
function TIpSockControl.ExtractName(HostEnt : PHostEnt) : string;
begin
  if Assigned(HostEnt) then
    SetString(Result, HostEnt.h_name, StrLen(HostEnt.h_name));
end;

{ Extracts port from a ServEnt }
function TIpSockControl.ExtractPort(ServEnt : PServEnt) : Integer;
begin
  if Assigned(ServEnt) then
    Result := ntohs(ServEnt.s_port)
  else
    Result := 0;
end;

{ Extracts service from a ServEnt }
function TIpSockControl.ExtractService(ServEnt : PServEnt) : string;
begin
  if Assigned(ServEnt) then
    SetString(Result, ServEnt.s_name, StrLen(ServEnt.s_name));
end;

{ Determine whether we have any active sockets }
function TIpSockControl.GetActiveSockets : Boolean;
begin
  { No sockets at this level }
  Result := False;
end;

{ Returns the index of the default local address }
function TIpSockControl.GetDefaultLocalAddress : Integer;
begin
  Result := FDefaultLocalAddress;
end;

{ Returns the default local port }
function TIpSockControl.GetDefaultLocalPort: Word;
begin
  Result := ntohs(scLocalSockAddr.sin_port);
end;

{ Returns host address(es) }
function TIpSockControl.GetHostAddress(Index : Integer) : string;
var
  HostStr : array[0..255] of AnsiChar;
begin
  Result := '';
  if IpWinSockAccess.GetHostName(HostStr, SizeOf(HostStr)) = 0 then
    Result := NetAddr2String(ExtractAddress(
      IpWinSockAccess.GetHostByName(HostStr), Index));
end;

{ Returns number of host address(es) }
function TIpSockControl.GetHostAddressCount : Integer;
var
  HostStr : array[0..255] of AnsiChar;
begin
  Result := 0;
  if IpWinSockAccess.GetHostName(HostStr, SizeOf(HostStr)) = 0 then
    Result := ExtractAddressCount(IpWinSockAccess.GetHostByName(HostStr));
end;

{ Returns last WinSock error }
function TIpSockControl.GetLastError : Integer;
begin
  Result := IpWinSockAccess.WSAGetLastError;
end;

{ Returns local host }
function TIpSockControl.GetLocalHost : string;
var
  HostStr : array[0..255] of AnsiChar;
begin
  Result := '';
  if IpWinSockAccess.GetHostName(HostStr, SizeOf(HostStr)) = 0 then begin
    Result := StrPas(HostStr);
  end;
end;

{ For internal use by the terminal }
function TIpSockControl.GetMasterTerminal(Socket : TSocket) : TIpBaseWinControl;
var
  Sckt : TIpDataSocket;
begin
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then
    Result := Sckt.MasterTerminal
  else
    Result := nil;
end;

{ Returns loaded module }
function TIpSockControl.GetModuleVersion : TIpModuleVersion;
begin
  Result := IpWinSockAccess.LoadedModule;
end;

{ Returns description of installed WinSock }
function TIpSockControl.GetwsDescription : string;
begin
  Result := StrPas(IpWinSockAccess.StartupData.szDescription);
end;

{ Returns high version of installed WinSock }
function TIpSockControl.GetwsHighVersion : Word;
begin
  Result := IpWinSockAccess.StartupData.wHighVersion;
end;

{ Returns maximum size of udp datagrams from startup info }
function TIpSockControl.GetwsMaxDatagram : Word;
begin
  Result := IpWinSockAccess.StartupData.iMaxUdpDg;
end;

{ Returns maximum number of sockets from startup info }
function TIpSockControl.GetwsMaxSockets : Word;
begin
  Result := IpWinSockAccess.StartupData.iMaxSockets;
end;

{ Returns system status from startup info }
function TIpSockControl.GetwsSystemStatus : string;
begin
  Result := StrPas(IpWinSockAccess.StartupData.szSystemStatus);
end;

{ Returns version of installed WinSock }
function TIpSockControl.GetwsVersion : Word;
begin
  Result := IpWinSockAccess.StartupData.wVersion;
end;

{ Converts host DWORD to network DWORD }
function TIpSockControl.htonl(HostLong : DWORD) : DWORD;
begin
  Result := IpWinSockAccess.htonl(HostLong);
end;

{ Converts host WORD to network WORD }
function TIpSockControl.htons(HostShort : Word) : Word;
begin
  Result := IpWinSockAccess.htons(HostShort);
end;

{ Blocking address look up }
function TIpSockControl.LookupAddress(const Name : string) : TInAddr;
begin
  Result := ExtractAddress(IpWinSockAccess.GetHostByName(PChar(Name)), 0);
end;

{ Blocking name look up }
function TIpSockControl.LookupName(InAddr : TInAddr) : string;
begin
  Result := ExtractName(IpWinSockAccess.GetHostByAddr(InAddr,
    SizeOf(InAddr), PF_INET));
end;

{ Blocking port look up }
function TIpSockControl.LookupPort(const Service : string) : Integer;
begin
  Result := ExtractPort(IpWinSockAccess.GetServByName(PChar(Service),
    PChar(scProtoString)));
end;

{ Blocking service look up }
function TIpSockControl.LookupService(Port : Word) : string;
begin
  Result := ExtractService(IpWinSockAccess.GetServByPort(htons(Port),
    PChar(scProtoString)));
end;

{ Converts TInAddr to a XXX.XXX.XXX.XXX string }
function TIpSockControl.NetAddr2String(InAddr : TInAddr) : string;
var
  TempStr : array[0..IPStrSize] of AnsiChar;
begin
  Result := '';
  StrCopy(TempStr, IpWinSockAccess.INet_NtoA(InAddr));
  Result := StrPas(TempStr);
end;

{ Converts network DWORD to host DWORD }
function TIpSockControl.ntohl(NetLong : DWORD) : DWORD;
begin
  Result := IpWinSockAccess.ntohl(NetLong);
end;

{ Converts network WORD to host WORD }
function TIpSockControl.ntohs(NetShort : Word) : Word;
begin
  Result := IpWinSockAccess.ntohs(NetShort);
end;

{ Registers terminal for socket notifications }
procedure TIpSockControl.RegisterTerminal(Handle : TIpHWND);           {!!.12}
begin
  { A "connect" message should be sent for all connected sockets }
  { when a terminal first connects -- to be handled in descendants }
  scTerminalList.Add(Pointer(Handle));
end;

{ Request asynchronous address lookup }
function TIpSockControl.RequestAddressLookup(const Name : string) : TIpHandle;
var
  TempBuf : PIpReplyBuf;
begin
  { Create buffer for async reply }
  TempBuf := AllocMem(SizeOf(TIpReplyBuf));
  TempBuf.ReplyType := rtAddress;

  Result := IpWinSockAccess.WsaAsyncGetHostByName(FHandle,
    CM_IPASYNCRESULT, PChar(Name), TempBuf.Buffer, SizeOf(TempBuf.Buffer));

  if Result <> 0 then begin
    { Async process successfully started, add to list }
    TempBuf.Handle := Result;
    scReplyList.Add(TempBuf);
  end else begin
    { Async process failed, free memory and raise error }
    FreeMem(TempBuf);
    raise EIpWinSockError.CreateWsError(LastError, AsyncAPIStr(rtAddress));
  end;
end;

{ Request asynchronous name lookup }
function TIpSockControl.RequestNameLookup(InAddr : TInAddr) : TIpHandle;
var
  TempBuf : PIpReplyBuf;
begin
  { Create buffer for async reply }
  TempBuf := AllocMem(SizeOf(TIpReplyBuf));
  TempBuf.ReplyType := rtName;

  Result := IpWinSockAccess.WsaAsyncGetHostByAddr(FHandle,
    CM_IPASYNCRESULT, @InAddr, SizeOf(InAddr), Af_Inet, TempBuf.Buffer,
    SizeOf(TempBuf.Buffer));

  if Result <> 0 then begin
    { Async process successfully started, add to list }
    TempBuf.Handle := Result;
    scReplyList.Add(TempBuf);
  end else begin
    { Async process failed, free memory and raise error }
    FreeMem(TempBuf);
    raise EIpWinSockError.CreateWsError(LastError, AsyncAPIStr(rtName));
  end;
end;

{ Request asynchronous port lookup }
function TIpSockControl.RequestPortLookup(const Service : string) : TIpHandle;
var
  TempBuf : PIpReplyBuf;
begin
  { Create buffer for async reply }
  TempBuf := AllocMem(SizeOf(TIpReplyBuf));
  TempBuf.ReplyType := rtPort;

  Result := IpWinSockAccess.WsaAsyncGetServByName(FHandle,
    CM_IPASYNCRESULT, PChar(Service), PChar(scProtoString),
    TempBuf.Buffer, SizeOf(TempBuf.Buffer));

  if Result <> 0 then begin
    { Async process successfully started, add to list }
    TempBuf.Handle := Result;
    scReplyList.Add(TempBuf);
  end else begin
    { Async process failed, free memory and raise error }
    FreeMem(TempBuf);
    raise EIpWinSockError.CreateWsError(LastError, AsyncAPIStr(rtPort));
  end;
end;

{ Request asynchronous service lookup }
function TIpSockControl.RequestServiceLookup(Port : Word) : TIpHandle;
var
  TempBuf : PIpReplyBuf;
begin
  { Create buffer for async reply }
  TempBuf := AllocMem(SizeOf(TIpReplyBuf));
  TempBuf.ReplyType := rtService;

  Result := IpWinSockAccess.WsaAsyncGetServByPort(FHandle,
    CM_IPASYNCRESULT, htons(Port), PChar(scProtoString),
    TempBuf.Buffer, SizeOf(TempBuf.Buffer));

  if Result <> 0 then begin
    { Async process successfully started, add to list }
    TempBuf.Handle := Result;
    scReplyList.Add(TempBuf);
  end else begin
    { Async process failed, free memory and raise error }
    FreeMem(TempBuf);
    raise EIpWinSockError.CreateWsError(LastError, AsyncAPIStr(rtService));
  end;
end;

{ Handle incoming Fd_Accept }
procedure TIpSockControl.scAccept(Socket : TSocket);
begin
  { Do nothing -- handled in descendant class(es) }
end;

{ Handle incoming Fd_Close }
procedure TIpSockControl.scClose(Socket : TSocket);
var
  Sckt : TIpDataSocket;
begin
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then begin
    Sckt.bsClose;
    scFreeSocket(Socket);                                              {!!.02}
  end;
end;

{ Attach terminal to designated data socket, return True for success }
function TIpSockControl.scAttachTerminal(Socket : TSocket; Handle : TIpHWND) : Boolean; {!!.12}
var
  Sckt : TIpDataSocket;
begin
  { Get appropriate socket }
  Sckt := scGetSocket(Socket, False);

  { Attach if it exists, return result }
  if Assigned(Sckt) then begin
    Sckt.dsAttachTerminal(Handle);
    Result := True;
  end else begin
    Result := False;
  end;
end;

{ Handle incoming Fd_Connect }
procedure TIpSockControl.scConnect(Socket : TSocket);
var
  Sckt : TIpDataSocket;
begin
  Sckt := scGetSocket(Socket, False);
  if not Assigned(Sckt) then Exit;

  { Inform socket it's connected }
  Sckt.bsConnect;
end;

{ Attach terminal to designated data socket }
procedure TIpSockControl.scDetachTerminal(Socket : TSocket; Handle : TIpHWND);  {!!.12}
var
  Sckt : TIpDataSocket;
begin
  { Get appropriate socket }
  Sckt := scGetSocket(Socket, False);

  { Detach if it exists, ignore if not }
  if Assigned(Sckt) then
    Sckt.dsDetachTerminal(Handle);
end;

{ Return pointer to socket }
function TIpSockControl.scGetSocket(Socket : TSocket; Validate : Boolean) : TIpDataSocket;
begin
  { We don't have a socket at this level }
  if Validate then
    raise EIpNoSocketError.Create(SNoSockErr)
  else
    Result := nil;
end;

{ Handle incoming Fd_Oob }
procedure TIpSockControl.scOob(Socket : TSocket);
var
  Sckt : TIpDataSocket;
begin
  { Inform socket it got an Fd_Oob message }
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then Sckt.bsOob;
end;

{ Send block out through appropriate socket }
function TIpSockControl.scPutBlock(Socket : TSocket; var Buf;
  BufferSize : Integer; Disconnect : Boolean) : TIpHandle;
begin
  { Send the block, returning handle if it was queued }
  Result := scGetSocket(Socket, True).PutBlock(Buf, BufferSize, Disconnect);
end;

{ Send contents of stream out through appropriate socket }
function TIpSockControl.scPutStream(Socket : TSocket; const Stm : TStream;
  Disconnect : Boolean) : TIpHandle;
begin
  { Send the stream, returning handle if it was queued }
  Result := scGetSocket(Socket, True).PutStream(Stm, Disconnect);
end;

{ Send string out through appropriate socket }
function TIpSockControl.scPutString(Socket : TSocket; const Str : string;
  Disconnect : Boolean) : TIpHandle;
begin
  { Send the stream, returning handle if it was queued }
  Result := scGetSocket(Socket, True).PutString(Str, Disconnect);
end;

{ Handle incoming Fd_Read }
procedure TIpSockControl.scRead(Socket : TSocket);
var
  Sckt : TIpDataSocket;
begin
  { Inform socket it got an Fd_Read message }
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then Sckt.bsRead;
end;

{ Resets stats for appropriate socket }
procedure TIpSockControl.scResetStats(Socket : TSocket);
begin
  scGetSocket(Socket, True).ResetStats;
end;

{ Return a snapshot of the current stats of the appropriate socket }
procedure TIpSockControl.scStatusSnapshot(Socket : TSocket; var StatRec : TIpSockStatRec);
begin
  scGetSocket(Socket, True).StatusSnapshot(StatRec);
end;

{ Creates a SockAddr based on string and default }
procedure TIpSockControl.scString2SockAddr(const Addr : string;
  var SockAddr : TSockAddrIn; DefPort : Word);
var
  AddrRec : TIpAddrRec;
  HostPort : Word;
  HostAddr : TInAddr;
begin
  { Set to default }
  SockAddr.sin_family := Af_Inet;

  FillChar(AddrRec, SizeOf(AddrRec), #0);

  { Parse Addr }
  IpParseURL(Addr, AddrRec);

  { Set port appropriately }
  if AddrRec.Port <> '' then begin
    HostPort := StrToInt(AddrRec.Port);
    SockAddr.sin_port := IpWinSockAccess.htons(HostPort);
  end else
    SockAddr.sin_port := IpWinSockAccess.htons(DefPort);

  { Handle no address error }
  if AddrRec.Authority = '' then
    raise EIpWinSockError.Create(SwsaEDestAddrReq);

  { Check for broadcast address, handle directly }                     {!!.03}
  if CompareStr(AddrRec.Authority, '255.255.255.255') = 0 then begin   {!!.03}
    DWORD(SockAddr.sin_addr) := InAddr_Broadcast;                      {!!.03}
    Exit;                                                              {!!.03}
  end;                                                                 {!!.03}

  { Is the address a dotted quad? }
  HostAddr := String2NetAddr(AddrRec.Authority);

  { Yes, easy case }
  if DWORD(HostAddr.S_addr) <> InAddr_None then
    SockAddr.sin_addr := HostAddr

  { No, we have to do a database lookup }
  else begin
    SockAddr.sin_addr := LookupAddress(AddrRec.Authority);
  end;
end;

{ Handle incoming timer message }
procedure TIpSockControl.scTimer;
var
  NewTime : DWORD;
  Delta : Integer;
begin
  NewTime := GetTickCount;
  if scLastTime > NewTime then begin

    { Looks like we wrapped, handle appropriately }
    Delta := High(DWORD) - scLastTime;
    Inc(Delta, NewTime);
  end else begin

    { Simple case }
    Delta := NewTime - scLastTime;
  end;

  { Update scLastTime }
  scLastTime := NewTime;

  { Do the broadcast }
  scTimerBroadcast(Delta);
end;

{ Broadcast timer tickle to all associated sockets }
procedure TIpSockControl.scTimerBroadcast(Interval : Integer);
begin
  { No sockets at this level }
end;

{ Handle incoming Fd_Write }
procedure TIpSockControl.scWrite(Socket : TSocket);
var
  Sckt : TIpDataSocket;
begin
  { Inform socket it got an Fd_Write message }
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then Sckt.bsWrite;
end;

{ Sets the default local address }
procedure TIpSockControl.SetDefaultLocalAddress(const Value : Integer);
begin
  if Value <> FDefaultLocalAddress then begin

    { Index out of range, raise exception }
    if Value >= HostAddressCount then
      raise EIpIndexError.Create(SIndexErr);

    { Set property variable }
    FDefaultLocalAddress := Value;

    { Set value in scLocalSockAddr }
    if FDefaultLocalAddress = 0 then
      scLocalSockAddr.sin_addr.S_addr := InAddr_Any
    else
      scLocalSockAddr.sin_addr := String2NetAddr(HostAddress[Value]);
  end;
end;

{ Sets the default local port }
procedure TIpSockControl.SetDefaultLocalPort(const Value: Word);
begin
  scLocalSockAddr.sin_port := htons(Value);
end;

{ Sets the socket type }
procedure TIpSockControl.SetSockProtocol(const Value : TIpSockProtocol);
begin
  { Cannot set this property if socket is active }
  if ActiveSockets then
    raise EIpActiveError.Create(SActiveErr);

  FSockProtocol := Value;
  case FSockProtocol of
    spTcp :
      begin
        scProtoString := 'tcp';
        scProtoType := Sock_Stream;
      end;

    spUdp :
      begin
        scProtoString := 'udp';
        scProtoType := Sock_DGram;
      end;
  end;
end;

{ Converts XXX.XXX.XXX.XXX string to a TInAddr }
function TIpSockControl.String2NetAddr(const S : string) : TInAddr;
var
  TempStr : array[0..IPStrSize] of AnsiChar;
begin
  FillChar(Result, SizeOf(Result), #0);
  StrPLCopy(TempStr, S, IPStrSize);
  Result.S_addr := IpWinSockAccess.INet_Addr(@TempStr);
end;

{ For internal use by the terminal }
procedure TIpSockControl.SetMasterTerminal(Socket : TSocket; const Value : TIpBaseWinControl);
var
  Sckt : TIpDataSocket;
begin
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then
    Sckt.MasterTerminal := Value;
end;

{ Sets the interval of the internal timer -- if 0, timer killed }
procedure TIpSockControl.SetTimerInterval(const Value: DWORD);
begin
  if Value <> FTimerInterval then begin
    FTimerInterval := Value;

    { Kill old timer }
    KillTimer(FHandle, 1);

    if (FTimerInterval <> 0) then

      { Start new one if desired }
      if SetTimer(FHandle, 1, FTimerInterval, nil) = 0 then
        raise EOutOfResources.Create(SNoTimerErr)
      else

        { Init scLastTime field }
        scLastTime := GetTickCount;
  end;
end;

{ Window procedure }
procedure TIpSockControl.WndProc(var Message : TMessage);
begin
  if Message.Msg = WM_TIMER then try
    scTimer;
  except
    TimerInterval := 0;
    if Assigned(Application) then
      Application.HandleException(Self);
    { The above code could very well change...                    }
    { I have mixed feelings about it. Some things to consider:    }
    {                                                             }
    { - Application is sometimes set to nil, so                   }
    {   Application.HandleException isn't a perfect solution      }
    { - An exception that happens once in a timer event is        }
    {   likely to happen again, creating a "multi exception from  }
    {   hell" situation like happens in TTimer's OnTimer event    }
    {                                                             }
    { The alternative is to blindly eat exceptions -- also bad    }
    {                                                             }
    { Bottom line is we need to be extra diligent about avoiding  }
    { exceptions in timer handler code, and handling them if they }
    { do occur.                                                   }


  end else if Message.Msg = WM_QUERYENDSESSION then try                {!!.01}
    Message.Result := 1;                                               {!!.01}
  except                                                               {!!.01}
    if Assigned(Application) then                                      {!!.01}
      Application.HandleException(Self);                               {!!.01}

  end else
     Dispatch(Message);
end;

{ TIpCustomClient }

constructor TIpCustomClient.Create(AOwner : TComponent);
begin
  inherited;
  FSocksServer := TIpSocksServerInfo.Create;
end;

destructor TIpCustomClient.Destroy;
begin
  IpSafeFree(FSocksServer);
  IpSafeFree(ccClientSocket);
  inherited;
end;

{ Close an existing socket }
procedure TIpCustomClient.ccCloseSocket(Socket : TSocket);
var
  Sckt : TIpDataSocket;
begin
  Sckt := scGetSocket(Socket, False);
  if Assigned(Sckt) then begin                                         {!!.02}
    Sckt.bsCloseSocket;
    scFreeSocket(Socket);                                              {!!.02}
  end;                                                                 {!!.02}
end;

{ Create (if necessary) and connect() a socket }
function TIpCustomClient.ccConnectSocket(const RemoteURL : string) : TSocket;
var
  Addr : TSockAddrIn;
begin
  { Do we already have a client socket? }
  if Assigned(ccClientSocket) then begin

    { Is it already connected? }
    if ccClientSocket.Connected then
      raise EIpWinSockError.Create(SwsaEIsConn);

  end else
    { Create the data socket if necessary }
    scCreateSocket;

  { Fill in Addr appropriately }
  scString2SockAddr(RemoteURL, Addr, FDefaultPort);
  { Connect the socket }
  ccClientSocket.bsConnectSocket(Addr, RemoteURL);
  Result := ccClientSocket.SocketHandle;
end;

{ Handle status events }
procedure TIpCustomClient.DoStatus(Socket : TSocket; Event : TIpStatusType;
  Connection : TIpConnRec);
var
  I : Integer;
  SockStat : TCMIpSocketStatus;
begin
  inherited;

  { Post terminal notification message if appropriate }
  case Event of
    stConnect :
      begin
        SockStat.StatusEvent := IpssConnect;
        SockStat.SingleSocket := 1;
        for I := 0 to scTerminalList.Count-1 do
          PostMessage(TIpHWND(scTerminalList[I]), CM_IPSOCKETSTATUS, Socket,
                                                       SockStat.LParam); {!!.12}
      end;

    stDisconnect :
      begin
        SockStat.StatusEvent := IpssDisconnect;
        SockStat.SingleSocket := 1;
        for I := 0 to scTerminalList.Count-1 do
          PostMessage(TIpHWND(scTerminalList[I]), CM_IPSOCKETSTATUS, Socket,
                                                       SockStat.LParam); {!!.12}
      end;
    end;
end;

{ Return whether we have any active sockets }
function TIpCustomClient.GetActiveSockets : Boolean;
begin
  Result := Assigned(ccClientSocket);
end;

{ Return whether client socket is connected to a remote }
function TIpCustomClient.GetConnected : Boolean;
begin
  if Assigned(ccClientSocket) then
    Result := ccClientSocket.Connected
  else
    Result := False;
end;

{ Retrieve the fixed line length for the socket }
function TIpCustomClient.GetFixedLineLength : DWORD;
begin
  { Make sure we have a client socket instance }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.FixedLineLength;
end;

{ Returns the idle time for a connected socket }
function TIpCustomClient.GetIdleTime : DWORD;
begin
  { Make sure we have a client socket instance }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.IdleTime;
end;

{ Returns the idle timeout for a socket }
function TIpCustomClient.GetIdleTimeout : DWORD;
begin
  if (csDesigning in ComponentState) then begin                        {!!.03}
    { Redirect from sock inits }                                       {!!.03}
    Result := FSockInits.IdleTimeout;                                  {!!.03}
  end else begin                                                       {!!.03}

    { Make sure we have a client socket instance }
    if not Assigned(ccClientSocket) then
      raise EIpNoSocketError.Create(SNoSockErr);

    Result := ccClientSocket.IdleTimeout;
  end;                                                                 {!!.03}
end;

{ Returns the line term char for a socket }
function TIpCustomClient.GetLineTermChar : AnsiChar;
begin
  { Make sure we have a client socket instance }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.LineTermChar;
end;

{ Returns the line delimiter for a socket }
function TIpCustomClient.GetLineTerminator : TIpLineTerminator;
begin
  { Make sure we have a client socket instance }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.LineTerminator;
end;

{ Returns the local address for a connected socket }
function TIpCustomClient.GetLocalAddress : TInAddr;
begin
  { Make sure we have a client socket instance }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.LocalAddress;
end;

{ Returns the local port for a connected socket }
function TIpCustomClient.GetLocalPort : Word;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.LocalPort;
end;

{!!.01 - added}
{ Returns the log options for a socket }
function TIpCustomClient.GetLogOptions : TIpLogOptionSet;
begin
  { Make sure we have a client socket instance }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.LogOptions;
end;

{ Returns the receive buffer size a socket }
function TIpCustomClient.GetReceiveBufferSize : Integer;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.ReceiveBufferSize;
end;

{ Returns the receive mode for a socket }
function TIpCustomClient.GetReceiveMode : TIpReceiveMode;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.ReceiveMode;
end;

{ Returns pointer to the receive stream }
function TIpCustomClient.GetReceiveStream : TStream;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.ReceiveStream;
end;

{ Returns the remote port for a connected socket }
function TIpCustomClient.GetRemoteAddress : TInAddr;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.RemoteAddress;
end;

{ Returns the remote port for a connected socket }
function TIpCustomClient.GetRemotePort : Word;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.RemotePort;
end;

{ Returns the send buffer size for the client socket }
function TIpCustomClient.GetSendBufferSize : Integer;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.SendBufferSize;
end;

{ Returns the socket handle for the client socket }
function TIpCustomClient.GetSocketHandle : TSocket;
begin
  if Assigned(ccClientSocket) then
    Result := ccClientSocket.SocketHandle
  else
    Result := Invalid_Socket;
end;

{ Returns the socket options for the client socket }
function TIpCustomClient.GetSocketOptions : TIpSockOptionSet;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.SocketOptions;
end;

{ Returns the StripLineTerminator mode for a socket }
function TIpCustomClient.GetStripLineTerminator : Boolean;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.StripLineTerminator;
end;

{ Returns the URL used for connection }
function TIpCustomClient.GetURL : string;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  Result := ccClientSocket.URL;
end;

{ Registers terminal for socket notifications }
procedure TIpCustomClient.RegisterTerminal(Handle : TIpHWND); {!!.12}
var
  SockStat : TCMIpSocketStatus;
begin
  { Post notification message if we're already connected }
  if Connected then begin
    SockStat.StatusEvent := IpssConnect;
    SockStat.SingleSocket := 1;
    PostMessage(Handle, CM_IPSOCKETSTATUS, SocketHandle, SockStat.LParam);
  end;

  inherited;
end;

{ Create a new socket }
function TIpCustomClient.scCreateSocket : TSocket;
begin
  { Create sock with default options }
  ccClientSocket := TIpDataSocket.Create(Self, Invalid_Socket,
    scLocalSockAddr, SockInits, SocksServer);

  { Return socket handle }
  Result := ccClientSocket.SocketHandle;
end;

{ Set ccClientSocket to nil, free socket }
procedure TIpCustomClient.scFreeSocket(Socket : TSocket);
begin
  { Verify our socket }
  scGetSocket(Socket, True);

  { Free it, setting ccClientSocket to nil }
  IpSafeFree(ccClientSocket);
end;

{ Return pointer to client socket }
function TIpCustomClient.scGetSocket(Socket : TSocket; Validate : Boolean) : TIpDataSocket;
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then begin

    if Validate then
      raise EIpNoSocketError.Create(SNoSockErr)
    else begin
      Result := nil;
      Exit;
    end;
  end;

  { Validate requested handle }
  if ccClientSocket.SocketHandle <> Socket then begin

    if Validate then
      raise EIpNoSocketError.Create(SNoSockErr)
    else
      Result := nil;

  end else
    { Return handle }
    Result := ccClientSocket;
end;

{ Broadcast timer tickle to all associated socks }
procedure TIpCustomClient.scTimerBroadcast(Interval : Integer);
begin
  { One socket at most, ignore if nil }
  if Assigned(ccClientSocket) then
    ccClientSocket.bsTimer(Interval);

  inherited;
end;

{ Sets the fixed line length for the client socket }
procedure TIpCustomClient.SetFixedLineLength(const Value : DWORD);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.FixedLineLength := Value;
end;

{ Sets the socket options for the client socket }
procedure TIpCustomClient.SetIdleTimeout(const Value : DWORD);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.IdleTimeout := Value;
end;

{ Sets the line term char for the client socket }
procedure TIpCustomClient.SetLineTermChar(const Value : AnsiChar);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.LineTermChar := Value;
end;

{ Sets the line delimiter for the client socket }
procedure TIpCustomClient.SetLineTerminator(const Value : TIpLineTerminator);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.LineTerminator := Value;
end;

{!!.01 - added}
{ Sets the log options for the client socket }
procedure TIpCustomClient.SetLogOptions(const Value : TIpLogOptionSet);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.LogOptions := Value;
end;

{ Sets the receive buffer size the client socket }
procedure TIpCustomClient.SetReceiveBufferSize(const Value : Integer);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.ReceiveBufferSize := Value;
end;

{ Sets the receive mode for the client socket }
procedure TIpCustomClient.SetReceiveMode(const Value : TIpReceiveMode);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.ReceiveMode := Value;
end;

{ Sets the receive stream for the client socket }
procedure TIpCustomClient.SetReceiveStream(const Value : TStream);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.ReceiveStream := Value;
end;

{ Sets the socket options for the client socket }
procedure TIpCustomClient.SetSendBufferSize(const Value : Integer);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.SendBufferSize := Value;
end;

{ Sets the socket options for the client socket }
procedure TIpCustomClient.SetSocketOptions(const Value : TIpSockOptionSet);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.SocketOptions := Value;
end;

{ Sets the StripLineTerminator mode the client socket }
procedure TIpCustomClient.SetStripLineTerminator(const Value : Boolean);
begin
  { Make sure we have a client socket class }
  if not Assigned(ccClientSocket) then
    raise EIpNoSocketError.Create(SNoSockErr);

  ccClientSocket.StripLineTerminator := Value;
end;

{ TIpClient }

{ Close the client socket if open }
procedure TIpClient.CloseSocket;
begin
  if Assigned(ccClientSocket) then
    ccCloseSocket(ccClientSocket.SocketHandle);
end;

{ Open and connect the socket to remote host }
function TIpClient.ConnectSocket(const RemoteURL : string) : TSocket;
begin
  Result := ccConnectSocket(RemoteURL);
end;

{ Sends or queues a block }
function TIpClient.PutBlock(var Buf; BufferSize : Integer;
  Disconnect : Boolean) : TIpHandle;
begin
  if Assigned(ccClientSocket) then
    Result := scPutBlock(ccClientSocket.SocketHandle, Buf, BufferSize, Disconnect)
  else
    raise EIpNoSocketError.Create(SNoSockErr);
end;

{ Sends or queues a stream }
function TIpClient.PutStream(const Stm : TStream;
  Disconnect : Boolean) : TIpHandle;
begin
  if Assigned(ccClientSocket) then
    Result := scPutStream(ccClientSocket.SocketHandle, Stm, Disconnect)
  else
    raise EIpNoSocketError.Create(SNoSockErr);
end;

{ Sends or queues a string }
function TIpClient.PutString(const Str : string;
  Disconnect : Boolean) : TIpHandle;
begin
  if Assigned(ccClientSocket) then
    Result := scPutString(ccClientSocket.SocketHandle, Str, Disconnect)
  else
    raise EIpNoSocketError.Create(SNoSockErr);
end;

{ Resets stat counter for client socket }
procedure TIpClient.ResetStats;
begin
  if Assigned(ccClientSocket) then
    scResetStats(ccClientSocket.SocketHandle)
  else
    raise EIpNoSocketError.Create(SNoSockErr);
end;

{ Return a snapshot of the current stats for the client socket }
procedure TIpClient.StatusSnapshot(var StatRec : TIpSockStatRec);
begin
  if Assigned(ccClientSocket) then
    scStatusSnapshot(ccClientSocket.SocketHandle, StatRec)
  else
    raise EIpNoSocketError.Create(SNoSockErr);
end;

{ TIpSocketList }

{ Create instance of TIpSocketList }
constructor TIpSocketList.Create;
begin
  inherited Create;
  InitializeCriticalSection(slListCS);
  FList := TList.Create;
end;

{ Destroy instance of TIpSocketList }
destructor TIpSocketList.Destroy;
begin
  BroadcastClose;
  IpSafeFree(FList);
  DeleteCriticalSection(slListCS);
  inherited Destroy;
end;

{ Add item to list and hash }
procedure TIpSocketList.Add(Item : Pointer);
var
  H : LongInt;
begin
  LockList;
  try
    H := GenerateHash(TIpDataSocket(Item).SocketHandle);
    TIpDataSocket(Item).dsNextItem := HashTable[H];
    HashTable[H] := TIpDataSocket(Item);
    FList.Add(Item);
  finally
    UnlockList;
  end;
end;

{ Close each socket in list }
procedure TIpSocketList.BroadcastClose;
var
  I : Integer;
begin
  LockList;
  try
    { Close and free each socket }
    for I := FList.Count-1 downto 0 do begin                           {!!.02}
      TIpDataSocket(FList[I]).bsCloseSocket;
      Delete(I);                                                       {!!.02}
    end;                                                               {!!.02}
  finally
    UnlockList;
  end;
end;

{!!.01 - rewritten }
{ Timer tickle each socket in list }
procedure TIpSocketList.BroadcastTimer(Interval : Integer);
var
  I : Integer;
begin
  LockList;
  try
    try
      I := 0;
      { Timer tickle each socket }
      while I < FList.Count do begin
        TIpDataSocket(FList[I]).bsTimer(Interval);
        Inc(I);
      end;
    except
      { There's a small chance we can get a list index out of bounds }
      { here if a socket disappears on us. If so, let it bounce us   }
      { out of the loop, then eat it }
      on EListError do ;
    end;
  finally
    UnlockList;
  end;
end;

{ Delete item from list and hash -- freeing it }
procedure TIpSocketList.Delete(Index : Integer);
begin
  LockList;
  try
    DeleteIndexEntry(Index);
    TObject(FList[Index]).Free;
    FList.Delete(Index);
  finally
    UnlockList;
  end;
end;

{ Delete index entry from hash }
procedure TIpSocketList.DeleteIndexEntry(Index : Integer);
var
  Look : TIpDataSocket;
  LastItem : TIpDataSocket;
  S : TSocket;
  Idx : Integer;
begin
  LockList;
  try
    S := TIpDataSocket(FList[Index]).SocketHandle;
    Idx := GenerateHash(S);
    LastItem := nil;
    Look := TIpDataSocket(HashTable[Idx]);
    while (Look <> nil) do begin
      if (Look.SocketHandle = S) then begin
        if LastItem = nil then
          TIpDataSocket(HashTable[Idx]) := Look.dsNextItem
        else
          LastItem.dsNextItem := Look.dsNextItem;
        Break;
      end;
      LastItem := Look;
      Look := Look.dsNextItem;
    end;
  finally
    UnlockList;
  end;
end;

{ Find item in hash, return index to list }
function TIpSocketList.Find(S : TSocket) : Integer;
var
  Look : TIpDataSocket;
begin
  LockList;
  try
    Look := HashTable[GenerateHash(S)];
    while Look <> nil do begin
      if Look.SocketHandle = S then begin
        Result := FList.IndexOf(Look);
        Exit;
      end;
      Look := Look.dsNextItem;
    end;
  finally
    UnlockList;
  end;
  Result := -1;
end;

{ Find item in hash, return pointer to item }
function TIpSocketList.FindPtr(S : TSocket) : TIpDataSocket;
begin
  LockList;
  try
    Result := HashTable[GenerateHash(S)];
    while Result <> nil do begin
      if Result.SocketHandle = S then
        Exit;
      Result := Result.dsNextItem;
    end;
  finally
    UnlockList;
  end;
end;

{ Remove socket from list and hash, free it }
procedure TIpSocketList.FreeSocket(S : TSocket);
var
  Index : Integer;
begin
  LockList;
  try
    Index := Find(S);
    if Index <> -1 then                                                {!!.02}
      Delete(Index);
  finally
    UnlockList;
  end;
end;

{ Generate hash for item }
function TIpSocketList.GenerateHash(S : TSocket) : LongInt;
begin
  Result := S mod IpHashSize;
end;

{ Returns item at index }
function TIpSocketList.Get(Index : Integer) : Pointer;
begin
  LockList;
  try
    if Index <> -1 then
      Result := FList[Index]
    else
      Result := nil;
  finally
    UnlockList;
  end;
end;

{ Returns number of items in list }
function TIpSocketList.GetCount : Integer;
begin
  LockList;
  try
    Result := FList.Count;
  finally
    UnlockList;
  end;
end;

{ Enter critical section for the list }
procedure TIpSocketList.LockList;
begin
  if IsMultiThread then
    EnterCriticalSection(slListCS);
end;

{ Leave critical section for the list }
procedure TIpSocketList.UnlockList;
begin
  if IsMultiThread then
    LeaveCriticalSection(slListCS);
end;

{ Put item at a specific position in list }
procedure TIpSocketList.Put(Index : Integer; Item : Pointer);
var
  H : LongInt;
begin
  LockList;
  try
    DeleteIndexEntry(Index);
    { Free old instance }
    TObject(FList[Index]).Free;
    { Add new index }
    H := GenerateHash(TIpDataSocket(Item).SocketHandle);
    TIpDataSocket(Item).dsNextItem := HashTable[H];
    HashTable[H] := TIpDataSocket(Item);
    { Replace pointer }
    FList[Index] := Item;
  finally
    UnlockList;
  end;
end;

{ TIpMultiSockControl }

constructor TIpMultiSockControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  msSocketList := TIpSocketList.Create;
end;

destructor TIpMultiSockControl.Destroy;
begin
  {if Assigned(msSocketList) then}                                     {!!.01}
    msSocketList.Free;
    {IpSafeFree(msSocketList);}                                        {!!.01}
  inherited;
end;

{ Handle status events }
procedure TIpMultiSockControl.DoStatus(Socket : TSocket; Event : TIpStatusType;
  Connection : TIpConnRec);
var
  I : Integer;
  SockStat : TCMIpSocketStatus;
begin
  inherited;

  { Post terminal notification message if appropriate }
  case Event of
    stConnect :
      begin
        SockStat.StatusEvent := IpssConnect;
        SockStat.SingleSocket := 0;
        for I := 0 to scTerminalList.Count-1 do
          PostMessage(TIpHWND(scTerminalList[I]), CM_IPSOCKETSTATUS, Socket,
                                                       SockStat.LParam); {!!.12}
      end;

    stDisconnect :
      begin
        SockStat.StatusEvent := IpssDisconnect;
        SockStat.SingleSocket := 0;
        for I := 0 to scTerminalList.Count-1 do
          PostMessage(TIpHWND(scTerminalList[I]), CM_IPSOCKETSTATUS, Socket,
                                                       SockStat.LParam); {!!.12}
      end;
    end;
end;

{ Return whether we have any active sockets }
function TIpMultiSockControl.GetActiveSockets : Boolean;
begin
  if Assigned(msSocketList) then
    Result := msSocketList.Count > 0
  else
    Result := False;
end;

{ Return whether referenced socket is connected to a remote }
function TIpMultiSockControl.GetConnected(Socket : TSocket) : Boolean;
begin
  Result := scGetSocket(Socket, True).Connected;
end;

{ Return the idle time for a connected socket }
function TIpMultiSockControl.GetFixedLineLength(Socket : TSocket) : DWORD;
begin
  Result := scGetSocket(Socket, True).FixedLineLength;
end;

{ Return the idle time for a connected socket }
function TIpMultiSockControl.GetIdleTime(Socket : TSocket) : DWORD;
begin
  Result := scGetSocket(Socket, True).IdleTime;
end;

{ Return the idle timeout for a socket }
function TIpMultiSockControl.GetIdleTimeout(Socket : TSocket) : DWORD;
begin
  if (csDesigning in ComponentState) then                              {!!.03}
    { Redirect from sock inits }                                       {!!.03}
    Result := FSockInits.IdleTimeout                                   {!!.03}
  else                                                                 {!!.03}
    Result := scGetSocket(Socket, True).IdleTimeout;
end;

{ Return the line term char for a socket }
function TIpMultiSockControl.GetLineTermChar(Socket : TSocket) : AnsiChar;
begin
  Result := scGetSocket(Socket, True).LineTermChar;
end;

{ Return the line delimiter for a socket }
function TIpMultiSockControl.GetLineTerminator(Socket : TSocket) : TIpLineTerminator;
begin
  Result := scGetSocket(Socket, True).LineTerminator;
end;

{ Return the local address for a connected socket }
function TIpMultiSockControl.GetLocalAddress(Socket : TSocket) : TInAddr;
begin
  Result := scGetSocket(Socket, True).LocalAddress;
end;

{ Return the local port for a connected socket }
function TIpMultiSockControl.GetLocalPort(Socket : TSocket) : Word;
begin
  Result := scGetSocket(Socket, True).LocalPort;
end;

{!!.01 - added}
{ Return the log options for a socket }
function TIpMultiSockControl.GetLogOptions(Socket : TSocket) : TIpLogOptionSet;
begin
  Result := scGetSocket(Socket, True).LogOptions;
end;

{ Return the receive buffer size for requested socket }
function TIpMultiSockControl.GetReceiveBufferSize(Socket : TSocket) : Integer;
begin
  Result := scGetSocket(Socket, True).ReceiveBufferSize;
end;

{ Return the receive mode for requested socket }
function TIpMultiSockControl.GetReceiveMode(Socket : TSocket) : TIpReceiveMode;
begin
  Result := scGetSocket(Socket, True).ReceiveMode;
end;

{ Return the receive stream for requested socket }
function TIpMultiSockControl.GetReceiveStream(Socket : TSocket) : TStream;
begin
  Result := scGetSocket(Socket, True).ReceiveStream;
end;

{ Return the remote address for requested connected socket }
function TIpMultiSockControl.GetRemoteAddress(Socket : TSocket) : TInAddr;
begin
  Result := scGetSocket(Socket, True).RemoteAddress;
end;

{ Return the remote port for requested connected socket }
function TIpMultiSockControl.GetRemotePort(Socket : TSocket) : Word;
begin
  Result := scGetSocket(Socket, True).RemotePort;
end;

{ Return send buffer size for requested socket }
function TIpMultiSockControl.GetSendBufferSize(Socket : TSocket) : Integer;
begin
  Result := scGetSocket(Socket, True).SendBufferSize;
end;

{ Return the number of sockets in the list }
function TIpMultiSockControl.GetSocketCount : Integer;
begin
  Result := msSocketList.Count;
end;

{ Return socket options for requested socket }
function TIpMultiSockControl.GetSocketOptions(Socket : TSocket) : TIpSockOptionSet;
begin
  Result := scGetSocket(Socket, True).SocketOptions;
end;

{ Return the StripLineTerminator mode for requested socket }
function TIpMultiSockControl.GetStripLineTerminator(Socket : TSocket) : Boolean;
begin
  Result := scGetSocket(Socket, True).StripLineTerminator;
end;

{ Return full URL for requested socket }
function TIpMultiSockControl.GetURL(Socket : TSocket) : string;
begin
  Result := scGetSocket(Socket, True).URL;
end;

{!!.01 - added}
{ Add data socket object to list }
procedure TIpMultiSockControl.msAddDataSocket(const DataSocket : TIpDataSocket);
begin
  if Assigned(msSocketList) and Assigned(DataSocket) then
    msSocketList.Add(DataSocket);
end;

{ Close socket, remove from list }
procedure TIpMultiSockControl.msCloseSocket(Socket : TSocket);
begin
  scGetSocket(Socket, True).bsCloseSocket;
  msSocketList.FreeSocket(Socket);                                     {!!.02}
end;

{ Close and free all sockets in list }
procedure TIpMultiSockControl.msCloseAll;
begin
  if Assigned(msSocketList) then
    msSocketList.BroadcastClose;
end;

{ Registers terminal for socket notifications }
procedure TIpMultiSockControl.RegisterTerminal(Handle : TIpHWND);      {!!.12}
begin
  { Well, we could throw a bunch of connect messages at the terminal here -- }
  { but why bother, since it's not going to connect to any of them anyway?   }
  inherited;
end;

{ Resets stat counter for the desired socket }
procedure TIpMultiSockControl.ResetStats(Socket : TSocket);
begin
  scResetStats(Socket);
end;

{ Return a snapshot of the current stats for the desired socket }
procedure TIpMultiSockControl.StatusSnapshot(Socket : TSocket; var StatRec : TIpSockStatRec);
begin
  scStatusSnapshot(Socket, StatRec);
end;

{ Create new socket }
function TIpMultiSockControl.scCreateSocket : TSocket;
begin
  { Return Invalid_Socket here, since this should not be used for servers -- }
  { the servers we know, at least }
  Result := Invalid_Socket;
end;

{ Remove from list, free it }
procedure TIpMultiSockControl.scFreeSocket(Socket : TSocket);
begin
  { Free the desired socket }
  msSocketList.FreeSocket(Socket);
end;

{ Return handle to requested socket }
function TIpMultiSockControl.scGetSocket(Socket : TSocket; Validate : Boolean) : TIpDataSocket;
begin
  Result := msSocketList.FindPtr(Socket);

  if (Result = nil) and Validate then
    { Raise exception if requested handle doesn't exist }
    raise EIpNoSocketError.Create(SNoSockErr);
end;


{ Broadcast timer tickle to all associated sockets }
procedure TIpMultiSockControl.scTimerBroadcast(Interval : Integer);
begin
  if Assigned(msSocketList) then
    msSocketList.BroadcastTimer(Interval);
  inherited;
end;

{ Set fixed line length for requested socket }
procedure TIpMultiSockControl.SetFixedLineLength(Socket : TSocket;
  const Value : DWORD);
begin
  scGetSocket(Socket, True).FixedLineLength := Value;
end;

{ Set socket options for requested socket }
procedure TIpMultiSockControl.SetIdleTimeout(Socket : TSocket;
  const Value : DWORD);
begin
  scGetSocket(Socket, True).IdleTimeout := Value;
end;

{ Set line term char for requested socket }
procedure TIpMultiSockControl.SetLineTermChar(Socket : TSocket;
  const Value : AnsiChar);
begin
  scGetSocket(Socket, True).LineTermChar := Value;
end;

{ Set line delimiter for requested socket }
procedure TIpMultiSockControl.SetLineTerminator(Socket : TSocket;
  const Value : TIpLineTerminator);
begin
  scGetSocket(Socket, True).LineTerminator := Value;
end;

{!!.01 - added}
{ Set log options for requested socket }
procedure TIpMultiSockControl.SetLogOptions(Socket : TSocket;
  const Value : TIpLogOptionSet);
begin
  scGetSocket(Socket, True).LogOptions := Value;
end;

{ Set receive buffer size for requested socket }
procedure TIpMultiSockControl.SetReceiveBufferSize(Socket : TSocket;
  const Value : Integer);
begin
  scGetSocket(Socket, True).ReceiveBufferSize := Value;
end;

{ Set receive mode for requested socket }
procedure TIpMultiSockControl.SetReceiveMode(Socket : TSocket;
  const Value : TIpReceiveMode);
begin
  scGetSocket(Socket, True).ReceiveMode := Value;
end;

{ Set receive stream for requested socket }
procedure TIpMultiSockControl.SetReceiveStream(Socket : TSocket;
  const Value : TStream);
begin
  scGetSocket(Socket, True).ReceiveStream := Value;
end;

{ Set send buffer size for requested socket }
procedure TIpMultiSockControl.SetSendBufferSize(Socket : TSocket;
  const Value : Integer);
begin
  scGetSocket(Socket, True).SendBufferSize := Value;
end;

{ Set socket options for requested socket }
procedure TIpMultiSockControl.SetSocketOptions(Socket : TSocket;
  const Value : TIpSockOptionSet);
begin
  scGetSocket(Socket, True).SocketOptions := Value;
end;

{ Set StripLineTerminator mode for requested socket }
procedure TIpMultiSockControl.SetStripLineTerminator(Socket : TSocket;
  const Value : Boolean);
begin
  scGetSocket(Socket, True).StripLineTerminator := Value;
end;

{ TIpCustomMultiClient }

{ Create TIpCustomMultiClient instance }
constructor TIpCustomMultiClient.Create(AOwner : TComponent);
begin
  inherited;
  FSocksServer := TIpSocksServerInfo.Create;
end;

{ Destroy instance }
destructor TIpCustomMultiClient.Destroy;
begin
  IpSafeFree(FSocksServer);
  inherited;
end;

{ Connect an existing socket }
procedure TIpCustomMultiClient.mcConnectThisSocket(Socket : TSocket;
                                             const RemoteURL : string);
var
  Addr : TSockAddrIn;
  ErrCode : Integer;                                                   {!!.12}
  TempSocket : TIpDataSocket;
begin
  { Does the requested socket exist? }
  TempSocket := scGetSocket(Socket, True);

  { Is it already connected? }
  if TempSocket.Connected then
    raise EIpWinSockError.Create(SwsaEIsConn);

  { Fill in Addr appropriately }
  scString2SockAddr(RemoteURL, Addr, FDefaultPort);
{Begin !!.12}
  if Addr.sin_addr.s_addr = 0 then begin
    ErrCode := TempSocket.bsGetLastError;
    DoError(Socket, ErrCode,
            Format(SWinSockErr,
                   [ErrCode, WinSockErrorMessage(ErrCode),
                    MsgAPIStr(fd_Connect)]));
  end
  else
    { Connect the socket }
    TempSocket.bsConnectSocket(Addr, RemoteURL);
{End !!.12}
end;

{ Create and connect socket }
function TIpCustomMultiClient.mcConnectSocket(const RemoteURL : string) : TSocket;
var
  Addr : TSockAddrIn;
begin
  { Create the data socket }
  Result := scCreateSocket;

  { Fill in Addr appropriately }
  scString2SockAddr(RemoteURL, Addr, FDefaultPort);

  { Connect the socket }
  scGetSocket(Result, True).bsConnectSocket(Addr, RemoteURL);
end;

function TIpCustomMultiClient.scCreateSocket : TSocket;
var
  NewSocket : TIpDataSocket;
begin
  { Create the data socket }
  NewSocket := TIpDataSocket.Create(Self, Invalid_Socket,
    scLocalSockAddr, SockInits, SocksServer);

  { Add it to the list }
  msSocketList.Add(NewSocket);

  { Return handle }
  Result := NewSocket.SocketHandle;
end;

{ Close a given connection }
procedure TIpMultiClient.CloseSocket(Socket : TSocket);
begin
  msCloseSocket(Socket);
end;

{ Closes all active connections }
procedure TIpMultiClient.CloseAll;
begin
  msCloseAll;
end;

{ Connects socket to desired server }
function TIpMultiClient.ConnectSocket(const RemoteURL : string) : TSocket;
begin
  Result := mcConnectSocket(RemoteURL);
end;

{ Sends a block of data }
function TIpMultiClient.PutBlock(Socket : TSocket; var Buf; BufferSize : Integer;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := scPutBlock(Socket, Buf, BufferSize, Disconnect);
end;

{ Sends a stream }
function TIpMultiClient.PutStream(Socket : TSocket; const Stm : TStream;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := scPutStream(Socket, Stm, Disconnect);
end;

{ Sends a string }
function TIpMultiClient.PutString(Socket : TSocket; const Str : string;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := scPutString(Socket, Str, Disconnect);
end;

{ TIpCustomServer }

{ Create instance }
constructor TIpCustomServer.Create(AOwner : TComponent);
begin
  inherited;
  { Property inits }
  FHostsAllow := TStringList.Create;
  FHostsAllow.Sorted := True;
  FHostsAllow.Duplicates := dupIgnore;
  FHostsAllow.Add('ALL');

  FHostsDeny := TStringList.Create;
  FHostsDeny.Sorted := True;
  FHostsDeny.Duplicates := dupIgnore;

  FMaxClients := cdMaxClients;
end;

{ Destroy instance }
destructor TIpCustomServer.Destroy;
begin
  IpSafeFree(FHostsAllow);
  IpSafeFree(FHostsDeny);
  IpSafeFree(cdListenSocket);
  inherited;
end;

{ Handle incoming connections appropriately }
procedure TIpCustomServer.cdAcceptConnection;
var
  RejectStr, IpStr : string;
  NewSock : TSocket;
  Addr : TSockAddrIn;
  ErrCode, AddrLen : Integer;
  DataSock : TIpDataSocket;
  {Connection : TIpConnRec;}                                           {!!.01}
begin
  RejectStr := '';

  AddrLen := SizeOf(Addr);
  FillChar(Addr, AddrLen, #0);

  { Accept the connection }
  NewSock := IpWinSockAccess.Accept(cdListenSocket.SocketHandle, Addr, AddrLen);

  { Check for errors }
  if Integer(NewSock) = Socket_Error then begin
    ErrCode := GetLastError;
    if ErrCode <> wsaEWouldBlock then
      raise EIpWinSockError.CreateWsError(ErrCode, 'Accept');
  end else begin

    { Create socket instance and add to list }
    DataSock := TIpDataSocket.Create(Self, NewSock, scLocalSockAddr, SockInits, nil);
    msSocketList.Add(DataSock);

    { Now, do we reject it? }

    { Check # connections vs. max connections }
    if DWORD(msSocketList.Count) > FMaxClients then begin
      DoReject(rtCount, RejectStr);
      DataSock.PutString(RejectStr, True);
      Exit;
    end;

    IpStr := NetAddr2String(Addr.sin_addr);

    { Check hosts allow list }
    if not cdMatchList(IpStr, FHostsAllow) then begin

      { Check hosts deny list }
      if cdMatchList(IpStr, FHostsDeny) then begin
        DoReject(rtDenied, RejectStr);
        DataSock.PutString(RejectStr, True);
        Exit;
      end;
    end;

    { Inform socket it's connected }
    DataSock.bsConnect;

    { Fill in connection data }
    {Connection.RemoteAddr := DataSock.RemoteAddress;}                 {!!.01}
    {Connection.RemotePort := ntohs(DataSock.RemotePort);}             {!!.01}
    {Connection.LocalAddr := DataSock.LocalAddress;}                   {!!.01}
    {Connection.LocalPort := ntohs(DataSock.LocalPort);}               {!!.01}

    { Pass along the message }
    {DoStatus(DataSock.SocketHandle, stConnect, Connection);}          {!!.01}
  end;
end;

{ Return whether string exists in list (or if 'ALL' exists in list) }
function TIpCustomServer.cdMatchList(const S : string; const List : TStringList) : Boolean;
var
  I : Integer;
  TempStr : string;
begin
  { Assume failure }
  Result := False;

  if List.Count = 0 then Exit;                                         {!!.03}

  { Check for 'ALL' }
  for I := 0 to List.Count-1 do begin
    if (CompareText(List[I], 'ALL') = 0) then begin
      Result := True;
      Exit;
    end;
  end;

  { Check list for exact IP match }
  if List.Find(S, I) then begin
    Result := True;
    Exit;

  { Check list for 'close enough' IP match }
  end else begin
    if I > 0 then begin
      TempStr := List[I-1];
      if TempStr[Length(TempStr)] = '.' then begin
        if Pos(TempStr, S) = 1 then begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

{ Fire rejection event }
procedure TIpCustomServer.DoReject(Reason : TIpRejectType; var Msg : string);
begin
  if Assigned(FOnReject) then
    FOnReject(Self, Reason, Msg);
end;

{ Return whether we have any active sockets }
function TIpCustomServer.GetActiveSockets : Boolean;
begin
  if Assigned(msSocketList) then
    Result := (msSocketList.Count > 0) or Assigned(cdListenSocket)
  else
    Result := Assigned(cdListenSocket);
end;

{ Housekeeping after component has been streamed in }
procedure TIpCustomServer.Loaded;
begin
  inherited;

  { If we're not designing, create a listen socket }
  if not (csDesigning in ComponentState) then begin

   if FActive and not Assigned(cdListenSocket) then
     { Create a listening socket }
     cdListenSocket := TIpListenSocket.Create(Self, Invalid_Socket,
       scLocalSockAddr, nil, nil);
  end;
end;

{ Handle incoming Fd_Accept }
procedure TIpCustomServer.scAccept(Socket : TSocket);
begin
  if Assigned(cdListenSocket) and (Socket = cdListenSocket.SocketHandle) then begin

    { Handle connection attempt appropriately }
    cdAcceptConnection;

    { Pass along to socket in case it cares }
    cdListenSocket.bsAccept;
  end else
    inherited;
end;

{ Handle incoming Fd_Close }
procedure TIpCustomServer.scClose(Socket : TSocket);
begin
  if Assigned(cdListenSocket) and (Socket = cdListenSocket.SocketHandle) then begin
    cdListenSocket.bsClose;
    IpSafeFree(cdListenSocket);
  end else
    inherited;
end;

{ Handle incoming Fd_Connect }
procedure TIpCustomServer.scConnect(Socket : TSocket);
begin
  if Assigned(cdListenSocket) and (Socket = cdListenSocket.SocketHandle) then
    cdListenSocket.bsConnect
  else
    inherited;
end;

{ Free specified socket }
procedure TIpCustomServer.scFreeSocket(Socket : TSocket);
begin
  if Assigned(cdListenSocket) and (Socket = cdListenSocket.SocketHandle) then
    IpSafeFree(cdListenSocket)
  else
    inherited;
end;

{ Handle incoming Fd_Read }
procedure TIpCustomServer.scRead(Socket : TSocket);
begin
  if Assigned(cdListenSocket) and (Socket = cdListenSocket.SocketHandle) then
    cdListenSocket.bsRead
  else
    inherited;
end;

{ Handle incoming Fd_Write }
procedure TIpCustomServer.scWrite(Socket : TSocket);
begin
  if Assigned(cdListenSocket) and (Socket = cdListenSocket.SocketHandle) then
    cdListenSocket.bsWrite
  else
    inherited;
end;

{ Set listening state }
procedure TIpCustomServer.SetActive(const Value : Boolean);
begin
  if (Value <> FActive) then begin

    { Don't create listen socket at design time }
    if not ((csDesigning in ComponentState) or
      (csLoading in ComponentState)) then begin

      if Value then begin

        { Create a listening socket }
        cdListenSocket := TIpListenSocket.Create(Self, Invalid_Socket,
          scLocalSockAddr, nil, nil);
      end else begin

        { Kill the listening socket }
        IpSafeFree(cdListenSocket);
      end;
    end;
    FActive := Value;
  end;
end;

procedure TIpCustomServer.SetHostsAllow(const Value : TStringList);
begin
  FHostsAllow.Assign(Value);
end;

procedure TIpCustomServer.SetHostsDeny(const Value : TStringList);
begin
  FHostsDeny.Assign(Value);
end;

{ IpServer }

{!!.01 - added}
{ Closes all active connections }
procedure TIpServer.CloseAll;
begin
  msCloseAll;
end;

{!!.01 - added}
{ Close a given connection }
procedure TIpServer.CloseSocket(Socket : TSocket);
begin
  msCloseSocket(Socket);
end;

{ Sends a block of data }
function TIpServer.PutBlock(Socket : TSocket; var Buf; BufferSize : Integer;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := scPutBlock(Socket, Buf, BufferSize, Disconnect);
end;

{ Sends a stream }
function TIpServer.PutStream(Socket : TSocket; const Stm : TStream;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := scPutStream(Socket, Stm, Disconnect);
end;

{ Sends a string }
function TIpServer.PutString(Socket : TSocket; const Str : string;
  Disconnect : Boolean) : TIpHandle;
begin
  Result := scPutString(Socket, Str, Disconnect);
end;


initialization
  IpWinSockAccess := TIpWinSockAccess.Create;

finalization
  IpSafeFree(IpWinSockAccess);

{
  $Log: IpSock.pas,v $
  Revision 1.3  2003/03/09 22:29:21  mkaemmerer
  - very little speed optimization (TIpSendQueueItem)

  Revision 1.2  2003/03/05 20:30:03  mkaemmerer
  - added Delphi 6 and Delphi 7 to Log-Header

}
end.
