unit PathConversions;

interface

uses
  Types, Windows, SysUtils;

function AbsToRel(const AbsPath, BasePath: string): string;

function RelToAbs(const RelPath, BasePath: string): string;

implementation

function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToA';

function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
         external 'shlwapi.dll' name 'PathCanonicalizeA';



function AbsToRel(const AbsPath, BasePath: string): string;
var
  Path: array[0..MAX_PATH-1] of char;
begin
  PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
  result := Path;
end;

function RelToAbs(const RelPath, BasePath: string): string;
var
  Dst: array[0..MAX_PATH-1] of char;
begin
  PathCanonicalize(@Dst[0], PChar(IncludeTrailingBackslash(BasePath) + RelPath));
  result := Dst;
end;

end.
