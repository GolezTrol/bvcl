{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

*******************************************************************************
Mine Sweeper in a single control.
Current state: unknown
******************************************************************************}
unit BigMineSweeper;

interface

uses
  Windows, Forms, Messages, Classes, Types, Graphics, Controls;

type
  TBigMineSweeper = class(TCustomControl)
  private
    FField: array of array of Byte;
    FFieldSize: Integer;
    FCursorX, FCursorY: Integer;

    procedure Initialize(AWidth, AHeight: Integer; AMines: Integer);
    procedure PreCount;
    function MouseToField(var AX, AY: Integer): Boolean;

    procedure WMGETDLGCODE(var AMsg: TMessage); message WM_GETDLGCODE;
  protected
    procedure Paint; override;

    procedure OpenCell(x, y: Integer);
    procedure MarkCell(x, y: Integer);

    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
  end;

const
  ID_BIGMINESWEEPER = 'BMS';

  // Field masks
  FM_COUNT = $0F;
  FM_MINE  = $10;
  FM_OPEN  = $20;
  FM_FLAG  = $40;
  FM_DOUBT = $80;
  FM_MARK  = $C0;
  FM_DATA  = $F0;

implementation

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

{ TBigMineSweeper }

constructor TBigMineSweeper.Create(AOwner: TComponent);
begin
  inherited;

  Initialize(10, 10, 8);
end;

procedure TBigMineSweeper.Initialize(AWidth, AHeight, AMines: Integer);
var
  i, x, y, m, rx, ry: Integer;
  b: Byte;
begin
  // Set dimensions of the field
  SetLength(FField, AWidth, AHeight);

  // Initialize the field with a specified number of mines.
  i := 0;
  m := FM_MINE;
  for x := 0 to High(FField) do
    for y := 0 to High(FField[x]) do
    begin
      Inc(i);
      if i > AMines then
        m := 0;
      FField[x, y] := m
    end;

  if AMines = 0 then
    Exit;

  // Shuffle the mines
  for x := 0 to High(FField) do
    for y := 0 to High(FField[x]) do
    begin
      rx := Random(x); ry := Random(y);
      b := FField[x, y];
      FField[x, y] := FField[rx, ry];
      FField[rx, ry] := b;
    end;

  PreCount;
end;

procedure TBigMineSweeper.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
  begin
    if ssShift in Shift then
      MarkCell(FCursorX, FCursorY)
    else
      OpenCell(FCursorX, FCursorY);
  end;

  if Key = VK_LEFT then Dec(FCursorX);
  if Key = VK_RIGHT then Inc(FCursorX);
  if Key = VK_UP then Dec(FCursorY);
  if Key = VK_DOWN then Inc(FCursorY);

  FCursorX := (FCursorX + High(FField)) mod Length(FField);
  FCursorY := (FCursorY + High(FField[0])) mod Length(FField[0]);
  Invalidate;
end;

procedure TBigMineSweeper.LoadFromStream(AStream: TStream);
begin

end;

procedure TBigMineSweeper.MarkCell(x, y: Integer);
begin

end;

function TBigMineSweeper.MouseToField(var AX, AY: Integer): Boolean;
var
  x, y: Integer;
begin
  x := AX div FFieldSize;
  y := AY div FFieldSize;
  Result := (x <= High(FField)) and (y <= High(FField[0]));
  if not Result then
    Exit;
  AX := x;
  AY := y;
end;

procedure TBigMineSweeper.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if MouseToField(X, Y) then
  begin
    FCursorX := X;
    FCursorY := Y;
    if (Button = mbRight) or ((Button = mbLeft) and (ssShift in Shift)) then
      MarkCell(X, Y)
    else if (Button = mbLeft) then
      OpenCell(X, Y);
  end;
end;

procedure TBigMineSweeper.OpenCell(x, y: Integer);
begin
  Invalidate;
  if (x<0) or (y<0) or (x>High(FField)) or (y>High(FField[0])) or (FField[x, y] and FM_OPEN > 0) then
    Exit;
  FField[x, y] := FField[x, y] or FM_OPEN;
  Application.ProcessMessages;
  Sleep(10);
  OpenCell(x-1, y-1); OpenCell(x, y-1); OpenCell(x+1, y-1);
  OpenCell(x-1, y); OpenCell(x, y); OpenCell(x+1, y);
  OpenCell(x-1, y+1); OpenCell(x, y+1); OpenCell(x+1, y+1);
end;

procedure TBigMineSweeper.Paint;
var
  x, y: Integer;

  procedure DrawField(x, y: Integer);
  var
    r: TRect;
  begin
    // Draw the frame
    r := Rect(x * FFieldSize, y * FFieldSize, x * FFieldSize + FFieldSize - 1, y * FFieldSize + FFieldSize - 1);
    if (FField[x, y] and FM_OPEN) = 0 then
      Frame3D(Canvas, r, clBtnHighlight, clBtnShadow, 2)
    else
      Frame3D(Canvas, r, clBtnFace, clBtnShadow, 1);
    Dec(r.Right); Dec(r.Bottom);

    // Draw the focus rectangle
    if (FCursorX = x) and (FCursorY = y) then
      DrawFocusRect(Canvas.Handle, r);
  end;

begin
  inherited;

  // Don't draw if there are no cells
  if (Length(FField) = 0) or (Length(FField[0]) = 0) then
    Exit;

  // Determine Field Size (maximum fill)
  x := ClientWidth div (High(FField) + 1);
  y := ClientHeight div Length(FField[0]);
  if x < y then FFieldSize := x else FFieldSize := y;

  // Draw each cell
  for x := 0 to High(FField) do
    for y := 0 to High(FField[x]) do
      DrawField(x, y);


end;

procedure TBigMineSweeper.PreCount;
var
  x, y: Integer;
begin
  // Precount the number of mines surrounding each cell. Can't this be any shorter?
  for x := 0 to High(FField) do
    for y := 0 to High(FField[x]) do
      if FField[x, y] and FM_MINE > 0 then
      begin
        if x > 0 then
        begin
          if y > 0 then Inc(FField[x-1, y-1]);
          Inc(FField[x-1, y]);
          if y < High(FField[x]) then Inc(FField[x-1, y+1]);
        end;
        if y > 0 then Inc(FField[x, y-1]);
        if y < High(FField[x]) then Inc(FField[x, y+1]);
        if x < High(FField) then
        begin
          if y > 0 then Inc(FField[x+1, y-1]);
          Inc(FField[x+1, y]);
          if y < High(FField[x]) then Inc(FField[x+1, y+1]);
        end;
      end;
end;

procedure TBigMineSweeper.SaveToStream(AStream: TStream);
var
  i, x, y: Word;
  d: packed array of Byte;
  t: Boolean;

  procedure WriteInt(i: DWord);
  begin
    AStream.Write(i, SizeOf(i));
  end;

begin
  // Signature
  AStream.Write(ID_BIGMINESWEEPER, Length(ID_BIGMINESWEEPER));

  // Version
  WriteInt(1);
  // Meta info
  WriteInt(High(FField));
  if High(FField) = 0 then
    WriteInt(0)
  else
    WriteInt(High(FField[0]));

  // Determine the number of bytes needed for the field
  i := High(FField) * High(FField[0]);
  if (i and 1) = 1 then
    Inc(i);
  i := i div 2;
  SetLength(d, i);

  // Write field data, each field in a nibble
  t := False;
  i := 0;
  for x := 0 to High(FField) do
    for y := 0 to High(FField[0]) do
      if t then
      begin
        d[i] := d[i] or (FField[x,y] and FM_DATA);
        Inc(i);
      end
      else
        d[i] := (FField[x,y] and FM_DATA) shr 4;

  // Write the buffer to the stream
  AStream.Write(d, Length(d));
end;

procedure TBigMineSweeper.WMGETDLGCODE(var AMsg: TMessage);
begin
  inherited;

  AMsg.Result := AMsg.Result or DLGC_WANTARROWS;
end;

end.
