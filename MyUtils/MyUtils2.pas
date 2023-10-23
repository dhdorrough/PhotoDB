unit MyUtils2;

interface

function  CalcBMI(Feet, Inches, Pounds: extended): Extended;
function  CalcBMIMetric(Centimeters, Kilograms: extended): Extended;
(*
function  ContainsWords( Target: string;
                         Data: string;
                         MatchWholeWordsOnly: boolean): boolean;
*)
implementation

uses
  SysUtils, StStrL, MetricConversions;

function  CalcBMI(Feet, Inches, Pounds: extended): Extended;
var
  TotIn: Extended;
begin
  TotIn := ((Feet * 12.0) + Inches);
  if TotIn > 0 then
    result := (Pounds * 703.0) / (TotIn * TotIn)
  else
    result := 0;
end;

function  CalcBMIMetric(Centimeters, Kilograms: extended): Extended;
var
  Feet, Inches, Pounds: Extended;
begin
  CentimetersToFeetAndInches(Centimeters, Feet, Inches);
  Pounds   := (Kilograms * 2.2);
  result   := CalcBMI(Feet, Inches, Pounds);
end;

(*
function ContainsWords( Target: string;
                        Data: string;
                        MatchWholeWordsOnly: boolean): boolean;
  const
    Delims = ', -._[];()?:''"@#$%^&*+={}|\<>';
  var
    wcD, wcT, TW, DW: integer;
    aWord: string;
    DataWords: array of string;

  function DataContainsWord(const aWord: string): boolean;
    var
      DW: integer;
  begin { DataContainsWord }
    result := false;
    for DW := 0 to wcD-1 do
      if aWord = DataWords[DW] then
        begin
          result := true;
          exit;
        end;
  end; { DataContainsWord }

begin
  Target := UpperCase(Target);
  Data   := UpperCase(Data);
  if MatchWholeWordsOnly then
    begin
      wcD := WordCountL(Data, DELIMS);
      SetLength(DataWords, wcD);
      for DW := 1 to wcD do
        DataWords[DW-1] := ExtractWordL(DW, Data, DELIMS);
      wcT := WordCountL(Target, DELIMS);
      // try to match each word of the target to any word in the data
      for TW := 1 to wcT do
        begin
          aWord := ExtractWordL(TW, Target, DELIMS);
          if not DataContainsWord(aWord) then
            begin
              result := false;
              exit;
            end;
        end;
      result := true; // iff every word was matched
    end
  else
    result := Pos(Target, Data) > 0;
end;
*)

end.
