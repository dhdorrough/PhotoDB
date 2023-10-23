Unit StateNamesX;

//{$INCLUDE BccDefines.inc}

// 09/30/2008 dhd CR 15512- Rollback effects of this change
// 06/02/2008 dhd CR 15512- Ability to match state abbreviations containing spaces ("N Y")
// 06/02/2005 dhd CR 13092- Revert to weak state abbreviation matching
// 06/02/2008 dhd CR 15513- Ability to match StateNamesX with no spaces
// 05/28/2008 dhd CR 15154- Implement MatchState function
// 12/16/2005 dhd CR 13092- Ability to split csz when state name is spelled out

interface

uses
  Classes;

const
{$IfDef Kludge}
  KLUDGE = 46;
  TOTALSTATES  = 69+KLUDGE;
{$else}
  TOTALSTATES = 69;
{$EndIf}
  STATE_DELIMS = ', ';


type
  TStateInfo = record
                 Abbrev: string[2];
                 FullName: string;
  end;

  TStateSet = set of 0..TOTALSTATES;

  TMultiWordRef = record
                 LastWord: string;
                 Refs: TStateSet;
  end;

var
  SIdx: array of byte;  // alphabetical index into StateInfo

(*
  StateInfo: array[TStateNr] of TStateInfo = (
     ({ 0} Abbrev: 'NC'; FullName: 'North Carolina'),
     ({ 1} Abbrev: 'SC'; FullName: 'South Carolina'),
     ({ 2} Abbrev: 'NY'; FullName: 'New York'),
     ...etc
  );

  MultiWordRefs: array[TMultiWordStates] of TMultiWordRef = (
     (LastWord: 'Carolina';  NrWords: 2, [1,2]),
     (LastWord: 'York';      NrWords: 2, [3]),
     ...etc
  );
*)
  StateInfoArray: array of TStateInfo;
  MultiWordRefs: array of TMultiWordRef;
  StateNameAbbreviations: TStringList = nil;

function FindPossibleStates(const Word: string;
                             var PossibleStates: TStateSet): integer;
function FindBestMatchingState( const S: string;
                                var TempStWrd: integer;
                                PossibleStates: TStateSet;
                                Delims: String = STATE_DELIMS): string;
function MatchSqueezedState(const TempSt: string): string;
function MatchState(const Value: string; var StateIndex: integer): boolean;
procedure SortStatesByName;
//function StateAbbrevFromSingleLetters(const Words: string): string;
function ValidStateAbbreviation(const Abbrev: string): boolean;

implementation

uses
  SysUtils, StStrL{ BccStringsX, StrCompareX};

//const
//  MIN_SCORE_FOR_MATCH = 90;

function Cardinality(StateSet: TStateSet): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to TOTALSTATES-1 do
    if i in StateSet then
      inc(result);
end;

function MatchState(const Value: string; var StateIndex: integer): boolean;
var
  t, b, m, c: integer;
  mode: (SEARCHING, SEARCH_FOUND, NOT_FOUND);
begin
  t    := 0;
  b    := Length(StateInfoArray) - 1;
  mode := SEARCHING;

  // do a binary search on the state names
  repeat
    if t > b then
      mode := NOT_FOUND
    else
      begin
        m := (t + b) div 2;
        StateIndex := SIdx[m];
        c := CompareText(Value, StateInfoArray[StateIndex].FullName);
        if c > 0 then
          t := m + 1 else
        if c < 0 then
          b := m - 1
        else
          mode           := SEARCH_FOUND;
      end;
  until mode <> SEARCHING;
  result := mode = SEARCH_FOUND;
end;

function FindPossibleStates(const Word: string;
                            var PossibleStates: TStateSet): integer;
  var
    t, b, m, c: integer;
    mode: (SEARCHING, SEARCH_FOUND, NOT_FOUND);
    aPossibleStates: TStateSet;
    StateIndex: integer;
begin { FindPossibleStates }
  // See if the word matches the last word of a state name
  result         := 0;  // return number of possibilities
  PossibleStates := [];
  t      := 0;
  b      := Length(MultiWordRefs)-1;
  mode   := SEARCHING;
  repeat
    if t > b then
      mode := NOT_FOUND
    else
      begin
        m := (t + b) div 2;
        c   := CompareText(Word, MultiWordRefs[m].LastWord);
        if c > 0 then
          t := m + 1 else
        if c < 0 then
          b := m - 1
        else
          begin
            mode            := SEARCH_FOUND;
            aPossibleStates := MultiWordRefs[m].Refs; // return the list of possible states
            result          := Cardinality(aPossibleStates);
            PossibleStates  := aPossibleStates; // oompiler bug: 2-step assignment should not be necessary but IS!
          end;
      end;
  until mode <> SEARCHING;

  // There may also be a single entry match. If so, include it
  if MatchState(Word, StateIndex) then
    begin
      result         := result + 1;
      PossibleStates := PossibleStates + [StateIndex];
    end;
end;  { FindPossibleStates }

(*
function SimilarText(const Word1, Word2: string): boolean;
begin
  result := Similar100(UpperCase(Word1), UpperCase(Word2)) >= MIN_SCORE_FOR_MATCH;
end;
*)

function FindBestMatchingState( const S: string;
                                var TempStWrd: integer;  // 1-origin
                                PossibleStates: TStateSet;
                                Delims: String = STATE_DELIMS): string;
var
  StartIdx           : integer;
  MaxWordsStateIndex : integer;
  StateNr            : integer;
  StateInfo          : TStateInfo;
  j                  : integer;
  mode               : (SEARCHING, SEARCH_FOUND, NOT_FOUND);
  WordsMatched       : integer;
  MaxWordsMatched    : integer;
  Word1, Word2       : string;
  StateWordCount     : integer;
begin
  MaxWordsStateIndex := -1;
  MaxWordsMatched    := 0;
  result             := '';
  StartIdx           := TempStWrd;
  // Look through all of the possible matching states for the longest match
  for StateNr := 0 to TOTALSTATES-1 do
    if StateNr in PossibleStates then
      begin
        StateInfo      := StateInfoArray[StateNr];
        StateWordCount := WordCountL(StateInfoArray[StateNr].FullName, STATE_DELIMS);
        TempStWrd      := StartIdx - StateWordCount + 1;
        if TempStWrd > 0 then // ...there are enough preceding words for this to be a possibility
          begin
            j := 1;
            mode := SEARCHING;
            WordsMatched := 0;
            // See if all of the words match for this "State"
            repeat
              if j > StateWordCount then
                mode := SEARCH_FOUND
              else
                begin
                  Word1 := ExtractWordL(j, StateInfo.FullName, STATE_DELIMS);
                  Word2 := ExtractWordL(TempStWrd, S, Delims);
                  if not (SameText(Word1, Word2) {or SimilarText(Word1, Word2)}) then
                    mode := NOT_FOUND
                  else
                    begin
                      inc(j);
                      inc(TempStWrd);
                      inc(WordsMatched);
                    end;
                end;
            until mode <> searching;
            if mode = SEARCH_FOUND then // all of the words matched for this "state"
              begin
                if WordsMatched > MaxWordsMatched then
                  begin
                    MaxWordsMatched := WordsMatched;
                    MaxWordsStateIndex   := StateNr;
                  end;
              end;
          end;
      end;

  if MaxWordsStateIndex >= 0 then // found a match
    begin
      StateInfo      := StateInfoArray[MaxWordsStateIndex];
      result         := StateInfo.Abbrev;
      StateWordCount := WordCountL(StateInfo.FullName, STATE_DELIMS);
      TempStWrd      := StartIdx - StateWordCount + 1;
    end;
end;

function Filter(const S, Filters: string): string;
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if not CharExistsL(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  SetLength(Result, Len);
end;

//*****************************************************************************
//   Function Name     : MatchSqueezedState
//   Useage            : Abbrev := MatchSqueezedState('WESTVIRGINIA');
//   Function Purpose  : Searches through the state names for a match with blanks ignored
//   Assumptions       : Only necessary to compare names containing a blank
//   Parameters        : TempSt = string to search for
//   Return Value      : The abbreviated state name
//*******************************************************************************}

function MatchSqueezedState(const TempSt: string): string;
var
  i: integer;
  FullName, SqueezedState: string;
begin
  result := '';
  for i := 0 to Length(StateInfoArray) - 1 do
    begin
      FullName := StateInfoArray[i].FullName;
      if Pos(' ', FullName) > 0 then
        begin
          SqueezedState := Filter(FullName, ' ');
          if SameText(TempSt, SqueezedState) then
            begin
              result := StateInfoArray[i].Abbrev;
              break;
            end;
        end;
    end;
end;

procedure SortStatesByName;
  var
    i, j: integer;
    Temp: integer;
    MultiWordRef: TMultiWordRef;
    NrStates: integer;

  procedure InitStateInfo;

    procedure Add(const Abr, Full: string);
      var
        Len, wc, Index: integer; Word: string;

      function AlreadyInList(const aWord: string; var Index: integer): boolean;
        var
          i: integer;
      begin { AlreadyInList }
        result := true;
        for i := 0 to Length(MultiWordRefs)-1 do
          if CompareText(aWord, MultiWordRefs[i].LastWord) = 0 then
            begin
              Index := i;
              exit;
            end;
        result := false;
      end;  { AlreadyInList }

    begin { Add }
      if Length(StateInfoArray) <= (NrStates+1) then
        begin
          SetLength(StateInfoArray, NrStates+1);
          SetLength(SIdx, NrStates+1);
        end;

      with StateInfoArray[NrStates] do
        begin
          Abbrev   := Abr;
          FullName := Full;
        end;

      wc := WordCountL(Full, STATE_DELIMS);
      if wc > 1 then
        begin
          Word := ExtractWordL(wc, Full, STATE_DELIMS);
          if not AlreadyInList(Word, Index) then
            begin
              Len := Length(MultiWordRefs);
              SetLength(MultiWordRefs, Len+1);
              with MultiWordRefs[Len] do
                begin
                  LastWord := Word;
                  Include(Refs, NrStates);
                end;
            end
          else
            with MultiWordRefs[Index] do
              Include(Refs, NrStates);
        end;

      Inc(NrStates);
    end;  { Add }

  begin { InitStateInfo }
    NrStates := 0;
    SetLength(StateInfoArray, TOTALSTATES);
    SetLength(Sidx,           TOTALSTATES);

    // This table was adapted from the BCCStateAbbreviations.rpl file
    Add('AL', 'ALA');
    Add('AL', 'ALABAMA');
    Add('AK', 'ALASKA');
    Add('AZ', 'ARIZ');
    Add('AZ', 'ARIZONA');
    Add('AR', 'ARKANSAS');
    Add('CA', 'CALIF');
    Add('CA', 'CALIFORNIA');
    Add('CO', 'COLORADO');
    Add('CT', 'CONNECTICUT');
    Add('CT', 'CONNETICUT');
    Add('DE', 'DELAWARE');
    Add('DC', 'WASHINGTON DC');
    Add('FL', 'FLORIDA');
    Add('GA', 'GEORGIA');
    Add('HI', 'HAWAII');
    Add('ID', 'IDAHO');
    Add('IL', 'ILLINOIS');
    Add('IN', 'INDIANA');
    Add('IA', 'IOWA');
    Add('KS', 'KANSAS');
    Add('KY', 'KENTUCKY');
    Add('LA', 'LOUISIANA');
    Add('ME', 'MAINE');
    Add('MD', 'MARYLAND');
    Add('MA', 'MASSACHUSETTS');
    Add('MI', 'MICHIGAN');
    Add('MN', 'MINNESOTA');
    Add('MS', 'MISSISSIPPI');
    Add('MO', 'MISSOURI');
    Add('MT', 'MONTANA');
    Add('NC', 'N CAROLINA');
    Add('ND', 'N DAKOTA');
    Add('NH', 'N HAMPSHIRE');
    Add('NJ', 'N JERSEY');
    Add('NM', 'N MEXICO');
    Add('NY', 'N YORK');
    Add('NE', 'NEBRASKA');
    Add('NV', 'NEVADA');
    Add('NH', 'NEW HAMPSHIRE');
    Add('NJ', 'NEW JERSEY');
    Add('NM', 'NEW MEXICO');
    Add('NY', 'NEW YORK');
    Add('NC', 'NO CAROLINA');
    Add('NC', 'NORTH CAROLINA');
    Add('ND', 'NORTH DAKOTA');
    Add('OH', 'OHIO');
    Add('OK', 'OKLAHOMA');
    Add('OR', 'OREGON');
    Add('PA', 'PENNSYLVANIA');
    Add('PA', 'PENSYLVANIA');
    Add('PR', 'PUERTO RICO');
    Add('RI', 'RHODE ISLAND');
    Add('SC', 'S CAROLINA');
    Add('SD', 'S DAKOTA');
    Add('SC', 'SO CAROLINA');
    Add('SC', 'SOUTH CAROLINA');
    Add('SD', 'SOUTH DAKOTA');
    Add('TN', 'TENESSEE');
    Add('TN', 'TENNESEE');
    Add('TX', 'TEXAS');
    Add('UT', 'UTAH');
    Add('VT', 'VERMONT');
    Add('VA', 'VIRGINIA');
    Add('WV', 'W VIRGINIA');
    Add('WA', 'WASHINGTON');
    Add('WV', 'WEST VIRGINIA');
    Add('WI', 'WISCONSIN');
    Add('WY', 'WYOMING');

{$IfDef Kludge}
    Add('NY',  'ONANDAGA');
    Add('NY',  'FLT');
    Add('NY',  'CANANDAIGUA');
    Add('NY',  'ADIRONDACKS');
    Add('NY',  'Genesee Valley');
    Add('NY',  'Lehigh Valley');
    Add('NY',  'Erie Canal');
    Add('NY',  'Ganandogan');
    Add('NY',  'Ganondogan');
    Add('NY',  'Penn Yan');
    Add('NY',  'Canadice Lake');
    Add('NY',  'Bare Hill');
    Add('NY',  'Wesley Hill');
    Add('NY',  'Hi Tor');
    Add('NY',  'Onanda Park');
    Add('NY',  'Seneca Park');
    Add('NY',  'Rattlesnake Hill');
    Add('NY',  'Harriet Hollister');
    Add('NY',  'FLCC');
    Add('NY',  'Cutler Boy Scout Camp');
    Add('NY',  'Bergen Swamp');
    Add('NY',  'Great Eastern Trail');
    Add('NY',  'Fishers');
    Add('NY',  'Perinton');
    Add('NY',  'Ontario Pathways');
    Add('MN',  'Superior Hiking Trail');
    Add('MN',  'SHT');
    Add('NY',  'Black River');
    Add('OH',  'Buckeye Trail');
    Add('NY',  'Letchworth');
    Add('MN',  'Gooseberry Falls State Park');
    Add('MN',  'Duluth');
    Add('MN',  'Superior Lake');
    Add('MN',  'Lake Superior');
    Add('MI',  'Mosquito Falls');
    Add('MI',  'Mosquito River');
    Add('MI',  'Little Beaver Lake');
    Add('MI',  '12 Mile Beach');
    Add('MI',  'Twelve Mile Beach');
    Add('NY',  'ADK');
    Add('MN',  'Lorana Jenkerson''s Trail');
    Add('OH',  'Caesar''s Creek');
    Add('MI',  'Munising');

    Add('AK',  'Fairbanks');
    Add('AK',  'Anchorage');
    Add('AK',  'Coldfoot');
{$EndIf}
    Assert(TOTALSTATES = NrStates, Format('TOTALSTATES <> NrStates (%d,%d)',
                                          [TOTALSTATES, NrStates]));
  end;  { InitStateInfo }

begin { SortStatesByName }
  InitStateInfo;

  for i := 0 to NrStates-1 do
    SIdx[i] := i;

  // We actually sort the index, leaving the StateNamesX in their original position
  for i := 0 to pred(pred(NrStates)) do
    for j := i+1 to pred(NrStates) do
      if CompareText(StateInfoArray[SIdx[i]].FullName, StateInfoArray[SIdx[j]].FullName) > 0 then
        begin
          Temp      := SIdx[i];
          SIdx[i]   := SIdx[j];
          SIdx[j]   := Temp;
        end;

  // but we sort the MultiWordRefs
  for i := 0 to pred(pred(Length(MultiWordRefs))) do
    for j := i + 1 to pred(Length(MultiWordRefs)) do
      if CompareText(MultiWordRefs[i].LastWord, MultiWordRefs[j].LastWord) > 0 then
        begin
          MultiWordRef       := MultiWordRefs[i];
          MultiWordRefs[i]   := MultiWordRefs[j];
          MultiWordRefs[j]   := MultiWordRef;
        end;
end;  { SortStatesByName }

procedure BuildSortedAbbreviationList;
var
  i: integer;
  Abbrev: string;
begin
  StateNameAbbreviations := TStringList.Create;
  for i := 0 to TOTALSTATES-1 do
    begin
      Abbrev := StateInfoArray[i].Abbrev;
      if StateNameAbbreviations.IndexOf(Abbrev) < 0 then
        StateNameAbbreviations.Add(Abbrev);
    end;
  StateNameAbbreviations.Sorted := true;
end;

function ValidStateAbbreviation(const Abbrev: string{; StrongMatch: boolean}): boolean;
var
  i: integer;
begin
  result := (Length(Abbrev) = 2);
  if result then
    begin
      result := false;
      for i := 0 to Length(StateInfoArray) - 1 do
        if SameText(Abbrev, StateInfoArray[i].Abbrev) then
          begin
            result := true;
            exit;
          end;
    end;
end;

(*
function StateAbbrevFromSingleLetters(const Words: string): string;
var
  L1, L2: string;
  Abbrev: string;
begin
  result := '';
  L1 := ExtractWordL(1, Words, STATE_DELIMS);
  L2 := ExtractWordL(2, Words, STATE_DELIMS);
  if (Length(L1) = 1) and (Length(L2) = 1) then
    begin
      Abbrev := L1 + L2;
      if StateNameAbbreviations.IndexOf(Abbrev) >= 0 then
        result := UpperCase(Abbrev);
    end;
end;
*)

initialization
  SortStatesByName;
  BuildSortedAbbreviationList;
finalization
  StateNameAbbreviations.Free;
end.
