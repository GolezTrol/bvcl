{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2010 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jun 19, 2009.
http://www.nldelphi.com/forum/showthread.php?t=34082
*******************************************************************************
 A control to select a value/score by selecting a number of stars.
******************************************************************************}
unit BigRatingControl;

interface

uses
  Windows, Messages, Graphics, Classes, Controls, SysUtils, Math;

type
  TBigRatingDrawShapeEvent = procedure(Sender: TObject; ARect: TRect; AFilled: Boolean) of object;

  TBigRatingStyle = (rsStar, rsCircle, rsCustomDraw);

  TBigRating = class(TCustomControl)
  private
    FPen: TPen;
    FMax: Integer;
    FRating: Integer;
    FStyle: TBigRatingStyle;
    FOnDrawShape: TBigRatingDrawShapeEvent;
    FBrushFilled: TBrush;
    FBrushEmpty: TBrush;
    procedure SetPen(const Value: TPen);
    procedure SetBrushEmpty(const Value: TBrush);
    procedure SetBrushFilled(const Value: TBrush);
    procedure SetMax(const Value: Integer);
    procedure SetRating(const Value: Integer);
    procedure SetStyle(const Value: TBigRatingStyle);

    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

  protected
    procedure DrawShape(ARect: TRect; AFilled: Boolean); virtual;

    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure PenOrBrushChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property TabStop default True;
    // Draw style properties
    property Pen: TPen read FPen write SetPen;
    property BrushEmpty: TBrush read FBrushEmpty write SetBrushEmpty;
    property BrushFilled: TBrush read FBrushFilled write SetBrushFilled;
    // Max value (max number of stars)
    property Max: Integer read FMax write SetMax;
    // Rating (number of stars given)
    property Rating: Integer read FRating write SetRating;
    // Shape style
    property Style: TBigRatingStyle read FStyle write SetStyle;
    // Event for custom drawn shapes
    property OnDrawShape: TBigRatingDrawShapeEvent read FOnDrawShape write FOnDrawShape;
  end;

implementation

{ TBigRating }

constructor TBigRating.Create(AOwner: TComponent);
begin
  inherited;
  // Pen and brushes
  FPen := TPen.Create;
  FBrushEmpty := TBrush.Create;
  FBrushFilled := TBrush.Create;

  // Default
  FPen.Color := clBlack;
  FBrushEmpty.Color := clWhite;
  FBrushFilled.Color := clLime;

  // Make sure we're notified of changes.
  FPen.OnChange := PenOrBrushChanged;
  FBrushEmpty.OnChange := PenOrBrushChanged;
  FBrushFilled.OnChange := PenOrBrushChanged;

  // More defaults
  FMax := 5;
  FRating := 0;
  TabStop := True;
end;

destructor TBigRating.Destroy;
begin
  FPen.Free;
  FBrushEmpty.Free;
  FBrushFilled.Free;
  inherited;
end;

procedure TBigRating.DrawShape(ARect: TRect; AFilled: Boolean);

  procedure DrawStar(ARect: TRect; APoints: Integer);
  var
    i: Integer;
    x, y: Double;
    p: Integer;
    Points: array of TPoint;
    RadiusX, RadiusY: Integer;
  begin
    RadiusX := (ARect.Right - ARect.Left) div 2;
    RadiusY := (ARect.Bottom - ARect.Top) div 2;

    p := APoints * 2;
    SetLength(Points, p);
    for i := 0 to p - 1 do
    begin
      x := Sin(2*PI / p * i) * RadiusX;
      y := Cos(2*PI / p * i) * RadiusY;
      if (i and 1) = 0 then
      begin
        x := x / 2;
        y := y / 2;
      end;
      Points[i].X := ARect.Left + RadiusX + Trunc(SimpleRoundTo(x, 0));
      Points[i].Y := RadiusY + Trunc(SimpleRoundTo(y, 0));
    end;
    Canvas.Polygon(Points);
  end;

  procedure DrawCircle(ARect: TRect);
  begin
    Canvas.Ellipse(ARect);
  end;

begin
  // Set the pen and brush
  Canvas.Pen := FPen;
  if AFilled then
    Canvas.Brush := FBrushFilled
  else
    Canvas.Brush := FBrushEmpty;

  // Draw shape or call event for custom drawing
  case FStyle of
    rsStar: DrawStar(ARect, 5);
    rsCircle: DrawCircle(ARect);
    rsCustomDraw:
      if Assigned(FOnDrawShape) then
        FOnDrawShape(Self, ARect, AFilled);
  end;
end;

procedure TBigRating.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  case Key of
    VK_RIGHT: Rating := Rating + 1;
    VK_LEFT: Rating := Rating - 1;
  end;
end;

procedure TBigRating.Paint;
var
  Size: Integer;
  i: Integer;
  r: TRect;

begin
  inherited;

  // Try to draw shapes as big as possible
  Size := Width div FMax;
  if Size > Height then
    Size := Height;
  Size := Size;
  // Max size found, now define a rectangle
  r := Rect(0, 0, Size-1, Size-1);
  // Draw FMax shapes
  for i := 0 to FMax - 1 do
  begin
    DrawShape(r, i < FRating);
    OffsetRect(r, Size, 0);
  end;
end;

procedure TBigRating.PenOrBrushChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TBigRating.SetBrushEmpty(const Value: TBrush);
begin
  FBrushEmpty.Assign(Value);
end;

procedure TBigRating.SetBrushFilled(const Value: TBrush);
begin
  FBrushFilled.Assign(Value);
end;

procedure TBigRating.SetMax(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.Create('Max must be 1 or more');
  if FMax = Value then
    Exit;
  FMax := Value;
  if FRating > FMax then
    FRating := FMax;
  Invalidate;
end;

procedure TBigRating.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TBigRating.SetRating(const Value: Integer);
begin
  if (FRating = Value) or (Value < 0) or (Value > FMax) then
    Exit;
  FRating := Value;
  Invalidate;
end;

procedure TBigRating.SetStyle(const Value: TBigRatingStyle);
begin
  if FStyle = Value then
    Exit;
  FStyle := Value;
  Invalidate;
end;

procedure TBigRating.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  // We want arrows!
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
