{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Nov 14, 2002.
http://www.nldelphi.com/forum/showthread.php?p=30659#post30659
*******************************************************************************
 Unfinished: TBigButton is a small demo that shows how to create an owner drawn
 TButton descendant.

To do;
 Fix redrawing problems when moving the window out of and back into the screen.
******************************************************************************}
unit BigButton;

interface

uses
  Windows, Graphics, Messages, Classes, Controls, StdCtrls;

type
  TBigButton = class(TButton)
  private
    FCanvas: TCanvas;
    FDefault: Boolean;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Canvas: TCanvas read FCanvas;
  end;

implementation

{ TBigButton }

constructor TBigButton.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TCanvas.Create;
end;

procedure TBigButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Create an ownerdrawn button
  CreateSubClass(Params, 'BUTTON');
  Params.Style := Params.Style or BS_OWNERDRAW;
end;

destructor TBigButton.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TBigButton.Paint;
begin
  // Misschien niet de meest logische actie, maar als het OnPaint event is
  // toegekend, dan wordt die aangeroepen.. Anders tekenen we ons eigen
  // plaatje.. Dit soort gedrag mag je natuurlijk zelf verzinnen. :)
  if Assigned(FOnPaint) then
    FOnPaint(Self)
  else
    with FCanvas do
    begin

      Pen.Color := clBlack;
      if FDefault then
        Brush.Color := clRed // Geselecteerd = rood
      else
        Brush.Color := clGreen; // Anders groen
      Rectangle(0, 0, Width, Height);

      if Focused then
        DrawFocusRect(Rect(2, 2, Width-2, Height-2));
      //DrawFrameControl(FCanvas.Handle, FCanvas.ClipRect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_CHECKED);

    end;
end;

procedure TBigButton.PaintWindow(DC: HDC);
begin
  FCanvas.Handle := DC;
  Paint;
end;

procedure TBigButton.SetButtonStyle(ADefault: Boolean);
const
  BS_MASK = $000F;
var
  Style: Word;
begin
  // ADefault indicates if this is the default button or not. Selected buttons
  // are default as well.
  // Use the BS_OwnerDraw to be allowed to draw the button yourself.

  // A change in state will cause the button to be repainted.
  // TButton.Default property cannot be used, because it will be modified too
  // soon. Therefore keep an FDefault flag.
  if FDefault <> ADefault then
    Invalidate;

  FDefault := ADefault;

  // Borrowed and modified a piece from TButton;
  if HandleAllocated then
  begin
    Style := BS_OWNERDRAW;
    if GetWindowLong(Handle, GWL_STYLE) and BS_MASK <> Style then
      SendMessage(Handle, BM_SETSTYLE, Style, 1);
  end;
end;

procedure TBigButton.WMPaint(var Message: TWMPaint);
begin
  // Indicate custom painting or PaintWindow won't be executed at all.
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

end.

