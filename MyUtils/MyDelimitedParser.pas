unit MyDelimitedParser;

// 01/07/2004 dhd CR 11792- Lines ending with delimiter weren't counting the last field
// 06/17/2004 dhd CR 10415- Export via ADO

interface

  uses
    SysUtils, {DQ_Strings,} MyAscii;

  const
    CRLF = #13#10;

  type
    EImproperlyDelimited = class(Exception);

    TFieldArray = array of String;

    TIntegerArray = array of integer;

    TDelimited_Info = record
      Field_Seperator : string[2];
      QuoteChar       : char;
//    FieldSizes      : TIntegerArray;
    end;

    TSearch_Type = (SEARCHING, SEARCH_FOUND, NOT_FOUND);
    TDelimiter = (crd_CRLF, crd_LF, crd_CR, crd_UserDefined, crd_FormFeed);

    TDelimiterRec = record
                      Abbrev  : string;
                      RecDelim: string;
                    end;

  function  Contruct_Delimited_Line( Fields: TFieldArray;
                                     Delimited_Info: TDelimited_Info): string;
  procedure DeleteField(var Fields: TFieldArray; Index: integer);
  procedure Parse_Delimited_Line
                      ( var Line  : string;
                        var Fields: TFieldArray;
                        var count : integer;
                        const Delimited_Info : TDelimited_Info);
  function  MwStrPos( StartIdx: integer; Target: string; Buffer: string; BufLen: integer): integer;


implementation

  const
    MAX_DELIMITED_FIELDS = 255;

  function MwStrPos( StartIdx: integer;
                     Target  : string;
                     Buffer  : string;
                     BufLen  : integer): integer;
    var
      mode   : tSEARCH_TYPE;
      i      : integer;
      LenTar : Integer;
  begin { MwStrPos }
    LenTar := Length(Target);
    if LenTar > 0 then
      begin
        { look for the first character of the target }
        mode := SEARCHING;
        i    := StartIdx;
        repeat
          if i + LenTar - 1 > BufLen then
            mode := NOT_FOUND else
          if Buffer[i] = Target[1] then  { match on 1st char }
            if Target = Copy(Buffer, i, LenTar) then
              mode := SEARCH_FOUND
            else
              inc(i)
          else
            inc(i);
        until mode <> SEARCHING;
        if mode = SEARCH_FOUND then
          result := i
        else
          result := 0;
      end
    else
      result := 0;   { searching for null-string is bad luck }
  end;  { MwStrPos }

  function Contruct_Delimited_Line( Fields: TFieldArray;
                                    Delimited_Info: TDelimited_Info): string;
    var
      i: integer; aField: string;
  begin
    result := '';
    with Delimited_Info do
      for i := 0 to Length(Fields)-1 do
        begin
          aField := Fields[i];
          if pos(QuoteChar, aField) > 0 then
            aField := QuoteChar + aField + QuoteChar;
          if i = 0 then
            result := aField
          else
            result := result + Field_Seperator + aField;
        end;
  end;

  procedure DeleteField(var Fields: TFieldArray; Index: integer);
    var
      i, NewLength: integer;
  begin
    NewLength := Length(Fields) - 1;
    for i := Index to NewLength-1 do
      Fields[i] := Fields[i+1];
    SetLength(Fields, NewLength);
  end;

  procedure Parse_Delimited_Line
                      ( var Line  : string;
                        var Fields: TFieldArray;
                        var count : integer;
                        const Delimited_Info : TDelimited_Info);
    const
      EOL = #$ff;

    var
      idx       : integer;
      ch        : char;
      dummy     : String;
      fld_cnt   : integer;
      tot_cnt   : integer;
      len       : integer;
      i         : integer;
      BlankIsSpecial: boolean;
      SepLen    : integer;

      procedure Nextch;
      begin { NextCh }
        if idx < length(line) then
          begin
            inc(idx);
            ch := Line[idx]
          end
        else
          ch := EOL;
      end;  { NextCh }

      procedure Skip_Blanks;
      begin { Skip_Blanks }
        while (idx < Length(line)) and (Line[idx] = ' ') do
          NextCh;
      end;  { Skip_Blanks }

      {*****************************************************************************
      {   Function Name     : readstring
      {   Function Purpose  : Read quoted string and demand that character
      {                       following the string is the seperator character.
      {                       Per request from Jim Shawver dated 5/13/98
      {*******************************************************************************}

      procedure ReadString( var s: String;
                            Quote: string; // may represent either the Quote OR the Field Seperator
                            Seperator: char);
        var
          temp : string[255];
          len  : integer;
          qp   : integer;
      begin { ReadString }
        qp := MwStrPos(idx, Quote, line, length(line));

        { make sure next char is the seperator }
        if (qp > 0) and (Seperator <> #0) then
          begin
            while (qp < length(line)) and
                 not ((line[qp] = quote[1]) and (line[qp+1] in [seperator,#0])) do
              inc(qp);

            if qp > length(line) then
              qp := 0;
          end;

        if qp > 0 then
          begin
            len  := qp - idx;
            temp := copy(line, idx, len);
            Idx  := qp + Length(quote)-1;
            NextCh;
          end
        else
          begin
            temp := copy(line, idx, Length(Line)-idx+1);
            idx  := length(line)+1;
            ch   := EOL;
          end;

        s := temp;
      end;  { ReadString }

      {*****************************************************************************
      {   Function Name     : read_field
      {   Function Purpose  : read the next delimited field
      {   Assumptions       : After read_field, should be ready to read 1st
      {                       char of next field }
      {*******************************************************************************}

      procedure Read_Field(var s : String);
        var
          chs: string[1];
      begin { read_field }
        if not BlankIsSpecial then
          Skip_Blanks;
        with Delimited_Info do
          if ch = QuoteChar then
            begin
              Nextch;
              ReadString(s, QuoteChar, Field_Seperator[1]);
              if ch <> EOL then
                if ch = Field_Seperator[1] then
                  Nextch
                else
                  begin
                    chs := ' '; chs[1] := ch;
                    raise EImproperlyDelimited.CreateFmt('Unexpected Field Seperator: %s', [chs]);
                  end;
            end
          else if ch <> EOL then
            ReadString(s, Field_Seperator, #0);
      end;  { read_field }

  begin { Parse_Delimited_Line }
    idx     := 0;
    fld_cnt := 0;
    tot_cnt := 0;
    with Delimited_Info do
      BlankIsSpecial := (QuoteChar <> #0) and
                        ((QuoteChar = ' ') or (Pos(' ', Field_Seperator) > 0));

    { ignore trailing blanks }
    len := Length(line);
    with Delimited_Info do
      begin
        if not (Length(Field_Seperator) > 0) then // cr 8478
          raise EImproperlyDelimited.Create('Field seperator undefined');

        if (not (' ' in [QuoteChar, Field_Seperator[1]])) and (len > 0) then
          begin
            while (len > 0) and (line[len] = ' ') do
              dec(len);
            SetLength(Line, len);
          end;
      end;

    Nextch;
    if ch <> EOL then
      begin
        repeat
          if fld_cnt < MAX_DELIMITED_FIELDS then
            begin
              if fld_cnt >= Length(Fields) then
                SetLength(Fields, fld_cnt+1);
              Read_Field(Fields[fld_cnt]);
              inc(fld_cnt);
            end
          else
            Read_Field(dummy);
          inc(tot_cnt);
        until ch = EOL;

        // if line ends with a field seperator, then there is really one more field
        with Delimited_Info do
          begin
            SepLen  := Length(Field_Seperator);
            if Copy(Line, Length(Line)-SepLen+1, SepLen) = Field_Seperator then
              begin
                if fld_cnt >= Length(Fields) then
                  SetLength(Fields, fld_cnt+1);
                Fields[fld_cnt] := '';
                inc(fld_cnt);
              end;
          end;
      end;

    if fld_cnt > MAX_DELIMITED_FIELDS then
      raise EImproperlyDelimited.CreateFmt('Too many fields: %d (%d max)',
                                               [tot_cnt, MAX_DELIMITED_FIELDS]);

    { if file is badly formatted, this line might have fewer fields than
      the previous line }
    for i := fld_cnt to Length(Fields)-1 do
      Fields[i] := '';

    count := fld_cnt;
  end;  { Parse_Delimited_Line }

initialization
//  CSVInfo.Field_Seperator := ',';
//  CSVInfo.QuoteChar       := '"';
end.
