{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Dec 2, 2006.
*******************************************************************************
- CorrectTextWrapping:
  Breaks text by inserting extra crlf's. Text will fit within the given width
  using the font settings of the given canvas.
******************************************************************************}
unit BigStrUtils;

interface

uses
  Graphics;

procedure CorrectTextWrapping(var AText: string; AWidth: Integer;
    out AHeight: Integer; ACanvas: TCanvas);

implementation

uses
  StrUtils, SysUtils;

procedure CorrectTextWrapping(var AText: string; AWidth: Integer;
    out AHeight: Integer; ACanvas: TCanvas);
// Breaks text correctly, fitting it in the provided width, using the font
// settings of the provided canvas. Returns altered text and text height.
var
  Src, Res, Line: string;
  i: Integer;
  LastSpace, LinePos: Integer;
  Height: Integer;
  BreakLine, IsEnter: Boolean;
begin
  LastSpace := 0;
  Height := 0;
  Res := '';
  Line := '';
  IsEnter := False;
  // Replace crlf with lf for easier checking
  Src := AdjustLineBreaks(AText, tlbsLF);
  i := 1;
  LinePos := 1;

  while i <= Length(Src) do
  begin
    // Break lines on ...
    if Src[i] in [' ', '-'] then
    begin
      LastSpace := i;
    end;

    // Skip spaces at start of line
    if not ((LinePos = 1) and (Src[i] = ' ')) then
      Line := Line + Src[i];

    // Check for linebreak
    IsEnter := Src[i] = #10;
    BreakLine := False;
    // Break line when it won't fit
    if (not IsEnter) and (ACanvas.TextWidth(Trim(Line)) > AWidth) then
    begin
      BreakLine := True;
      // If a space is found in this line, break on the space
      if LastSpace > 0 then
      begin
        Line := LeftStr(Line, Length(Line) - (i - LastSpace));
        i := LastSpace;
      end else
      begin
        // Shift the last character to the next line
        if Length(Line) > 1 then
        begin
          Dec(i);
          SetLength(Line, Length(Line) - 1);
        end;
      end;
    end;

    // Break line and reset variables.
    if IsEnter or BreakLine or (i = Length(Src)) then
    begin
      Inc(Height, ACanvas.TextHeight(Line));
      Res := Res + Trim(Line) + #13#10;
      Line := '';
      LinePos := 1;
      LastSpace := 0;
    end;

    Inc(i);
    Inc(LinePos);
  end;

  // Remove trailing crlf, but only if it was not part of the source text.
  if (Res <> '') and not IsEnter then
    SetLength(Res, Length(Res) - 2);
  // Return adjusted text and text height.
  AText := Res;
  AHeight := Height;
end;

end.
