unit MetricConversions;

interface

const
  POUNDS_PER_KILOGRAM  = 2.2;
  CENTIMETERS_PER_INCH = 2.54;
  INCHES_PER_FOOT      = 12.0;
  GRAMS_PER_KILOGRAM   = 1000.0;
  OUNCES_PER_POUND     = 16.0;

procedure CentimetersToFeetAndInches(Centimeters: Extended; var Feet, Inches: Extended);
procedure GramsToPoundsAndOunces(Grams: Extended; var Pounds, Ounces: Extended);
function  FeetAndInchesToCentimeters(Feet, Inches: extended): Extended;
procedure KilogramsToPoundsAndOunces(Kilograms: Extended; var Pounds, Ounces: Extended);
function  PoundsAndOuncesToKilograms(Pounds, Ounces: Extended): Extended;
function  PoundsAndOuncesToGrams(Pounds, Ounces: Extended): Extended;

implementation

uses
  MyUtils;

procedure CentimetersToFeetAndInches(Centimeters: Extended; var Feet, Inches: Extended);
begin
  Inches := Centimeters / CENTIMETERS_PER_INCH;
  Feet   := Inches / INCHES_PER_FOOT;
  Inches := (Feet - Trunc(Feet)) * INCHES_PER_FOOT;
  Feet   := Trunc(Feet);
end;

function  FeetAndInchesToCentimeters(Feet, Inches: extended): Extended;
begin
  result := ((Feet * INCHES_PER_FOOT) + Inches) * CENTIMETERS_PER_INCH;
end;

procedure KilogramsToPoundsAndOunces(Kilograms: Extended; var Pounds, Ounces: Extended);
var
  Fraction: Extended;
begin
  Pounds   := (Kilograms * POUNDS_PER_KILOGRAM);
  Fraction := Pounds - Trunc(Pounds);
  Pounds   := Trunc(Pounds);
  Ounces   := Fraction * OUNCES_PER_POUND;
end;

procedure GramsToPoundsAndOunces(Grams: Extended; var Pounds, Ounces: Extended);
var
  Fraction: Extended;
begin
  Pounds   := (Grams / GRAMS_PER_KILOGRAM) * POUNDS_PER_KILOGRAM;
  Fraction := Pounds - Trunc(Pounds);
  Pounds   := Trunc(Pounds);
  Ounces   := Fraction * OUNCES_PER_POUND;
end;

function  PoundsAndOuncesToGrams(Pounds, Ounces: Extended): Extended;
begin
  result := (Rounder(POUNDS*GRAMS_PER_KILOGRAM/POUNDS_PER_KILOGRAM +
              (Ounces/OUNCES_PER_POUND)*GRAMS_PER_KILOGRAM/POUNDS_PER_KILOGRAM, 2))
end;

function  PoundsAndOuncesToKilograms(Pounds, Ounces: Extended): Extended;
begin
  result := PoundsAndOuncesToGrams(Pounds, Ounces) / GRAMS_PER_KILOGRAM
end;

end.




