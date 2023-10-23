unit Synonyms;

interface

uses
  SysUtils, Classes, LookupsTableUnit;

type
  TSynonymObject = class
  private
    fList: TStringList;
    fLookupsTable: TLookupsTable;
    fReverseList: TStringList;
    fSorted: boolean;
    procedure SetSorted(const Value: boolean);
  public
    procedure AddSynonym(const Word1, Word2: string);
    procedure AddSynonyms(const word1: string; aList: TStringList);
    procedure LoadSynonyms(LookupsTable: TLookupsTable);
    function IsSynonymOf(const aWord1, aWord2: string): boolean;
    constructor Create(LookupsTable: TLookupsTable);
    destructor Destroy; override;
    procedure SynonymsOf(const aWord: string; List: TStringList);
    property Sorted: boolean
             read fSorted
             write SetSorted; 
  end;

implementation

uses
  StStrL, MyUtils;

{ TSynonymObject }

constructor TSynonymObject.Create(LookupsTable: TLookupsTable);
begin
  fLookupsTable := LookupsTable;
  fList         := TStringList.Create;
  fReverseList  := TStringList.Create;
  if Assigned(fLookupsTable) then
    LoadSynonyms(fLookupsTable);
end;

destructor TSynonymObject.Destroy;
begin
  FreeAndNil(fList);
  FreeAndNil(fReverseList);
  inherited;
end;

function TSynonymObject.IsSynonymOf(const aWord1, aWord2: string): boolean;
var
  idx: integer;
begin
  result := false;
  idx := fList.IndexOfName(aWord1);
  if idx >= 0 then
    begin
      result := SameText(aWord2, fList.ValueFromIndex[idx]);
      while (not result) and (idx < Pred(fList.Count)) do // there may be other synonyms
        begin
          inc(idx);
          if SameText(aWord1, fList.Names[idx]) then
            result := SameText(aWord2, fList.ValueFromIndex[idx])
          else
            break;  // no more possibilities
        end;
    end;

  if not result then
    begin
      idx := fReverseList.IndexOfName(aWord1);
      if idx >= 0 then
        begin
          result := SameText(aWord2, fReverseList.ValueFromIndex[idx]);
          while (not result) and (idx < Pred(fReverseList.Count)) do // there may be other synonyms
            begin
              inc(idx);
              if SameText(aWord1, fReverseList.Names[idx]) then
                result := SameText(aWord2, fReverseList.ValueFromIndex[idx])
              else
                break;  // no more possibilities
            end;
        end;
    end;
end;

procedure TSynonymObject.AddSynonym(const Word1, Word2: string);
var
  Line: string;
begin
  Line  := Format('%s=%s', [word1, word2]);
  fList.Add(Line);

  Line  := Format('%s=%s', [word2, word1]);
  fReverseList.Add(Line);
end;


procedure TSynonymObject.LoadSynonyms(LookupsTable: TLookupsTable);
var
  i: integer;
  Line, word1, word2: string;
begin
  // Load the forward list
  fList.Clear;
  with LookupsTable do
    begin
      First;
      while not Eof do
        begin
          fList.Add(fldLookupName.AsString);
          Next;
        end;
    end;

  // Load the backwards list
  for i := 0 to fList.Count-1 do
    begin
      Line  := fList[i];
      word1 := ExtractWordL(1, Line, '=');
      word2 := ExtractWordL(2, Line, '=');
      Line  := Format('%s=%s', [word2, word1]);
      fReverseList.Add(Line);
    end;

  // sort both lists for speed
  fList.Sorted := true;
  fReverseList.Sorted := true;
end;

procedure TSynonymObject.SynonymsOf(const aWord: string;
  List: TStringList);
var
  idx: integer;
begin
  idx := fList.IndexOfName(aWord);
  if idx >= 0 then  // it has at least one synonym
    begin
      List.Add(fList.ValueFromIndex[idx]);
      while (idx < Pred(fList.Count)) and (SameText(aWord, fList.Names[idx+1])) do
        begin
          inc(idx);
          List.Add(fList.ValueFromIndex[idx]);
        end;
    end;

  // also look for reverse synonyms
  idx := fReverseList.IndexOfName(aWord);
  if idx >= 0 then  // it has at least one synonym
    begin
      List.Add(fReverseList.ValueFromIndex[idx]);
      while (idx < Pred(fReverseList.Count)) and (SameText(aWord, fReverseList.Names[idx+1])) do
        begin
          inc(idx);
          List.Add(fReverseList.ValueFromIndex[idx]);
        end;
    end;

end;

procedure TSynonymObject.AddSynonyms(const word1: string;
  aList: TStringList);
var
  i: integer;
  Line: string;
begin
  for i := 0 to aList.Count-1 do
    begin
      Line := Format('%s=%s', [Word1, aList[i]]);
      fList.Add(Line);
    end;
end;

procedure TSynonymObject.SetSorted(const Value: boolean);
begin
  fSorted := Value;
  fList.Sorted := true;
  fReverseList.Sorted := true;
end;

end.
