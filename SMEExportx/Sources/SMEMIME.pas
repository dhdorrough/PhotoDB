{ SMExport suite
  Encoding to/decoding from MIME

  Copyright (C) 1998-2005, written by Mike Shkolnik, Scalabium Software
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit SMEMIME;

interface

function Encode64(const Source: string): string;
function Decode64(const Source: string): string;

function ASCII2EBCDIC(Asc: Char): Byte;
function EBCDIC2ASCII(EBC: Byte): Char;

implementation

function Encode64(const Source: string): string;
const
  arrEncodeTable: array[0..63] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i, intLength: Integer;
  intOrd1, intOrd2, intOrd3: Byte;
begin
  Result := '';

  intLength := Length(Source);
  i := 1;
  while (i <= intLength) do
  begin
    intOrd1 := Ord(Source[i]);
    Inc(i);
    if (i <= intLength) then
    begin
      intOrd2 := Ord(Source[i]);
      Inc(i);
      if (i <= intLength) then
      begin
        intOrd3 := Ord(Source[i]);
        Inc(i);
        Result := Result + arrEncodeTable[intOrd1 shr 2] +
                           arrEncodeTable[(intOrd1 and 3) shl 4 or (intOrd2 shr 4)] +
                           arrEncodeTable[(intOrd2 and 15) shl 2 or (intOrd3 shr 6)] +
                           arrEncodeTable[intOrd3 and 63];
      end
      else
      begin
        Result := Result + arrEncodeTable[intOrd1 shr 2] +
                           arrEncodeTable[(intOrd1 and 3) shl 4 or (intOrd2 shr 4)] +
                           arrEncodeTable[(intOrd2 and 15) shl 2] +
                           '=';
      end
    end
    else
    begin
      Result := Result + arrEncodeTable[intOrd1 shr 2] +
                arrEncodeTable[intOrd1 and 3 shl 4] +
                '=' +
                '=';
    end;
  end;
end;

function Decode64(const Source: string): string;
var
  i, j, intResLen, intLength: Integer;
  arrEncoded: array[1..4] of Byte;
  arrDecoded: array[1..3] of Byte;
  arrDecodeTable: array[Byte] of Byte;
begin
  Result := '';

  {build the decode table}
  FillChar(arrDecodeTable, SizeOf(arrDecodeTable), 99);
  for i := Ord('A') to Ord('Z') do
    arrDecodeTable[i] := i - 65;
  for i := Ord('a') to Ord('z') do
    arrDecodeTable[i] := i - 71;
  for i := Ord('0') to Ord('9') do
    arrDecodeTable[i] := i + 4;
  arrDecodeTable[Ord('+')] := 62;
  arrDecodeTable[Ord('/')] := 63;
  arrDecodeTable[Ord('=')] := 64;

  intLength := Length(Source);
  for i := 1 to (intLength div 4) do
  begin
    j := 4*(i-1)+1;
    arrEncoded[1] := arrDecodeTable[Ord(Source[j])];
    arrEncoded[2] := arrDecodeTable[Ord(Source[j+1])];
    arrEncoded[3] := arrDecodeTable[Ord(Source[j+2])];
    arrEncoded[4] := arrDecodeTable[Ord(Source[j+3])];

    if (arrEncoded[1] <> 64) then
    begin
      intResLen := 3;
      if (arrEncoded[3] = 64) then
      begin
        arrEncoded[3] := 0;
        arrEncoded[4] := 0;
        intResLen := 1;
      end
      else
      if (arrEncoded[4] = 64) then
      begin
        arrEncoded[4] := 0;
        intResLen := 2;
      end;

      arrDecoded[1] := arrEncoded[1] * 4 + (arrEncoded[2] div 16);
      arrDecoded[2] := (arrEncoded[2] mod 16) * 16 + (arrEncoded[3] div 4);
      arrDecoded[3] := (arrEncoded[3] mod 4) * 64 + arrEncoded[4];

      for j := 1 to intResLen do
        Result := Result + Chr(arrDecoded[j]);
    end
  end;
end;


const
  arrASCII_to_EBCDIC: array [0..255] of Byte =
            (
            $00, $01, $02, $03, $04, $05, $06, $07, $08,
            $09, $0A, $0B, $0C, $0D, $0E, $0F,
            $10, $11, $12, $13, $14, $15, $16, $17, $18,
            $19, $1A, $1B, $1C, $1D, $1E, $1F,
            $40, $5A, $7F, $7B, $5B, $6C, $50, $7D, $4D,
            $5D, $5C, $4E, $6B, $60, $4B, $61,
            $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8,
            $F9, $7A, $5E, $4C, $7E, $6E, $6F,
            $7C, $C1, $C2, $C3, $C4, $C5, $C6, $C7, $C8,
            $C9, $D1, $D2, $D3, $D4, $D5, $D6,
            $D7, $D8, $D9, $E2, $E3, $E4, $E5, $E6, $E7,
            $E8, $E9, $AD, $E0, $BD, $5F, $6D,
            $7D, $81, $82, $83, $84, $85, $86, $87, $88,
            $89, $91, $92, $93, $94, $95, $96,
            $97, $98, $99, $A2, $A3, $A4, $A5, $A6, $A7,
            $A8, $A9, $C0, $6A, $D0, $A1, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B, $4B,
            $4B, $4B, $4B, $4B, $4B, $4B, $4B );


  arrEBCDIC_to_ASCII: array[0..255] of Byte =
            (
            $00, $01, $02, $03, $04, $05, $06, $07, $08,
            $09, $0A, $0B, $0C, $0D, $0E, $0F,
            $10, $11, $12, $13, $14, $15, $16, $17, $18,
            $19, $1A, $1B, $1C, $1D, $1E, $1F,
            $20, $21, $22, $23, $24, $25, $26, $27, $28,
            $29, $2A, $2B, $2C, $2D, $2E, $2F,
            $2E, $2E, $32, $33, $34, $35, $36, $37, $38,
            $39, $3A, $3B, $3C, $3D, $2E, $3F,
            $20, $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $2E, $2E, $3C, $28, $2B, $7C,
            $26, $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $21, $24, $2A, $29, $3B, $5E,
            $2D, $2F, $2E, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $7C, $2C, $25, $5F, $3E, $3F,
            $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $3A, $23, $40, $27, $3D, $22,
            $2E, $61, $62, $63, $64, $65, $66, $67, $68,
            $69, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $6A, $6B, $6C, $6D, $6E, $6F, $70, $71,
            $72, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $7E, $73, $74, $75, $76, $77, $78, $79,
            $7A, $2E, $2E, $2E, $5B, $2E, $2E,
            $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E,
            $2E, $2E, $2E, $2E, $5D, $2E, $2E,
            $7B, $41, $42, $43, $44, $45, $46, $47, $48,
            $49, $2E, $2E, $2E, $2E, $2E, $2E,
            $7D, $4A, $4B, $4C, $4D, $4E, $4F, $50, $51,
            $52, $2E, $2E, $2E, $2E, $2E, $2E,
            $5C, $2E, $53, $54, $55, $56, $57, $58, $59,
            $5A, $2E, $2E, $2E, $2E, $2E, $2E,
            $30, $31, $32, $33, $34, $35, $36, $37, $38,
            $39, $2E, $2E, $2E, $2E, $2E, $2E);

function ASCII2EBCDIC(Asc: Char): Byte;
begin
  ASCII2EBCDIC := arrASCII_to_EBCDIC[Ord(Asc)];
end;

function EBCDIC2ASCII(EBC: Byte): Char;
begin
  EBCDIC2ASCII := Chr(arrEBCDIC_to_ASCII[EBC]);
end;

end.
