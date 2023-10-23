unit MyAscii; 

interface

const
  NUL = #$00;
  SOH = #$01;
  STX = #$02;
  ETX = #$03;
  EOT = #$04;
  ENQ = #$05;
  ACK = #$06;
  BEL = #$07;
  BS  = #$08;
  HT  = #$09;
  LF  = #$0A;
  VT  = #$0B;
  FF  = #$0C;
  CR  = #$0D;
  SO  = #$0E;
  SI  = #$0F;
  DLE = #$10;
  DC1 = #$11;
  DC2 = #$12;
  DC3 = #$13;
  DC4 = #$14;
  NAK = #$15;
  SYN = #$16;
  ETB = #$17;
  CAN = #$18;
  EM  = #$19;
  SUB = #$1A;
  ESC = #$1B;
  FS  = #$1C;
  GS  = #$1D;
  RS  = #$1E;
  US  = #$1F;

function AsciiDisplayText(Value: Char): string;

implementation

function AsciiDisplayText(Value: Char): string;
begin
  case Value of
    NUL: Result:= 'NUL';
    SOH: Result:= 'SOH';
    STX: Result:= 'STX';
    ETX: Result:= 'ETX';
    EOT: Result:= 'EOT';
    ENQ: Result:= 'ENQ';
    ACK: Result:= 'ACK';
    BEL: Result:= 'BEL';
    BS:  Result:= 'BS';
    HT:  Result:= 'HT';
    LF:  Result:= 'LF';
    VT:  Result:= 'VT';
    FF:  Result:= 'FF';
    CR:  Result:= 'CR';
    SO:  Result:= 'SO';
    SI:  Result:= 'SI';
    DLE: Result:= 'DLE';
    DC1: Result:= 'DC1';
    DC2: Result:= 'DC2';
    DC3: Result:= 'DC3';
    DC4: Result:= 'DC4';
    NAK: Result:= 'NAK';
    SYN: Result:= 'SYN';
    ETB: Result:= 'ETB';
    CAN: Result:= 'CAN';
    EM:  Result:= 'EM';
    SUB: Result:= 'SUB';
    ESC: Result:= 'ESC';
    FS:  Result:= 'FS';
    GS:  Result:= 'GS';
    RS:  Result:= 'RS';
    US:  Result:= 'US';
  else
    Result:= Value;
  end;
end;

end.

