unit GPX_Style;

// Basic support for the GPX_Style extension documented at
// http://www.topografix.com/GPX/gpx_style/0/2/

interface

uses
  Windows, Classes, Graphics, SysUtils;

type

  TGPX_StyleLine = Class
    private
      fColor : TColor;
      fOpacity : Double;
      function GetStyleColor() : String;
      procedure SetStyleColor(Color : String);
      function GetIsDefaultColor : Boolean;
      function GetOpacity() : String;
      procedure SetOpacity(Opacity : String);
    public
      //<color> colorType </color> [0..1] ? <-- 3 octets, RGB (ff0000) -->
      property IsDefaultColor : Boolean read GetIsDefaultColor;
      property Color : TColor read fColor write fColor;
      property ColorStr : String read GetStyleColor write SetStyleColor;
      property Opacity : Double read fOpacity write fOpacity;
      property OpacityStr : String read GetOpacity write SetOpacity;
      //<opacity> opacityType </opacity> [0..1] ? 0.0 = completely transparent; 1.0 = completely opaque
      //<width> xsd:decimal </width> [0..1] ? Width, in millimeters, of the line
      //<pattern> xsd:string </pattern> [0..1] ? Dash, e.g.
      //<linecap> linecapType </linecap> [0..1] ?
      //<dasharray> dasharrayType </dasharray> [0..1] ?
      //<extensions> extensionsType </extensions> [0..1] ?
      procedure CopyFrom(GPX_StyleLine : TGPX_StyleLine);
      constructor Create;
      destructor Destroy; override;      
  end;

  function ColorToHtml(DColor:TColor):string;
  function HtmlToColor(const HTML: String): TColor;

implementation

function ColorToHtml(DColor:TColor):string;
var
//  tmpRGB : LongInt;
  R,G,B : Byte;
begin
//  tmpRGB := ColorToRGB(DColor) ;
  B := (DColor and $FF0000) shr 16;
  G := (DColor and $00FF00) shr 8;
  R := (DColor and $0000FF);
  Result:=Format('%.2x%.2x%.2x', [R,G,B]) ;
end;

function HtmlToColor(const HTML: String): TColor;
var
  s : String;
  c : Integer;
begin
  s := Trim(HTML);
  if length(s)>6 then
    s := copy(s,1,6)
  else if length(s) < 6 then
    s := StringOfChar('0',6-length(s)) + s;
  c := Integer(StrToIntDef('$' + Copy(s, 1, 2),0)) +
       Integer(StrToIntDef('$' + Copy(s, 3, 2),0)) shl 8 +
       Integer(StrToIntDef('$' + Copy(s, 5, 2),0)) shl 16;
  Result := c;
end;

function TGPX_StyleLine.GetStyleColor() : String;
begin
  Result := ColorToHtml(fColor);
end;

procedure TGPX_StyleLine.SetStyleColor(Color : String);
begin
  Self.fColor := HTMLToColor(Color);
end;

procedure TGPX_StyleLine.CopyFrom(GPX_StyleLine : TGPX_StyleLine);
begin
  fColor := GPX_StyleLine.Color;
  fOpacity := GPX_StyleLine.Opacity;
end;

constructor TGPX_StyleLine.Create;
begin
  inherited;
  fColor := $01000000;
  fOpacity := 1;
end;

destructor TGPX_StyleLine.Destroy;
begin
  inherited;
end;

function TGPX_StyleLine.GetIsDefaultColor : Boolean;
begin
  Result := ((fColor and $01000000) = $01000000);
end;

function TGPX_StyleLine.GetOpacity() : String;
var
  FmtS : TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';
  Result := FloatToStr(Abs(fOpacity),FmtS);
end;

procedure TGPX_StyleLine.SetOpacity(Opacity : String);
var
  FmtS : TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,FmtS);
  FmtS.DecimalSeparator := '.';
  fOpacity := Abs(StrToFloatDef(Opacity,1,FmtS));
end;

end.
