{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Nov 20, 2008.
*******************************************************************************
 TSubtitleLabel shows its caption with a glow, making it more readable on a
 picture background.
******************************************************************************}
unit BigSubtitleLabel;

interface

uses
  Windows, Graphics, Classes, BigLabel;

type
  TBigCustomSubtitleLabel = class(TBigCustomLabel)
  private
    FGlowColor: TColor;
    procedure SetGlowColor(const Value: TColor);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Integer); override;
    property GlowColor: TColor read FGlowColor write SetGlowColor;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBigSubtitleLabel = class(TBigCustomSubtitleLabel)
  published
    // TBigCustomLabel
    property GlowColor;

    // TCustomLabel
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TBigCustomSubtitleLabel }

constructor TBigCustomSubtitleLabel.Create(AOwner: TComponent);
begin
  inherited;
  FGlowColor := clBlack;
  Font.Color := clWhite;

  //Width := 150;
end;

procedure TBigCustomSubtitleLabel.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: string;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;

  // 'Glow'
  Canvas.Font.Color := FGlowColor;

  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, 2, 0);
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, 0, 2);
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, -2, 0);
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);

  OffsetRect(Rect, 1, -1);
  // Front color
  Canvas.Font.Color := Font.Color;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);

  // A big more margin to prevent glow being clipped...
  InflateRect(Rect, 2, 2);
end;

procedure TBigCustomSubtitleLabel.SetGlowColor(const Value: TColor);
begin
  FGlowColor := Value;
  Invalidate;
end;

end.
