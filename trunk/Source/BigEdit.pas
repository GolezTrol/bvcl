{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Nov 25, 2008.
http://www.nldelphi.com/forum/showthread.php?p=263612#post263612
*******************************************************************************
 TBigCustomEdit is an edit that shows an example text as long a the control
 is empty and doesn't have focus.
******************************************************************************}

unit BigEdit;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, StdCtrls;

type
  TBigCustomEdit = class(TCustomEdit)
  private
    FCanvas: TCanvas;
    FHintText: string;
  protected
    procedure SetHintText(const Value: string); virtual;

    procedure Paint; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;

    property HintText: string read FHintText write SetHintText;

    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var Message); override;
  end;

  TBigEdit = class(TBigCustomEdit)
  published
    // TBigCustomEdit
    property HintText;

    // TCustomEdit
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TBigCustomEdit }

constructor TBigCustomEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TCanvas.Create;
end;
 
destructor TBigCustomEdit.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TBigCustomEdit.DefaultHandler(var Message);
begin
  inherited;

  if TMessage(Message).Msg = WM_PAINT then
  begin
    if HandleAllocated then
      FCanvas.Handle := GetDC(Handle);
 
    // The paint message has an optional Device Context that overrules the
    // default. This is used for double buffering etc.
    if TWMPAINT(Message).DC <> 0 then
      FCanvas.Handle := TWMPAINT(Message).DC;
 
   Paint;
  end;
end;
 
procedure TBigCustomEdit.DoEnter;
begin
  inherited;
  Invalidate;
end;
 
procedure TBigCustomEdit.DoExit;
begin
  inherited;
  Invalidate;
end;
 
procedure TBigCustomEdit.Paint;
begin
  inherited;

  // Make sure example is only drawn when the box is empty and doesn't
  // have focus.
  if (Text = '') and not Focused then
  begin
    // Pick a nice color. ToDo: Make this a property
    FCanvas.Font.Color := clGrayText;
    FCanvas.TextOut(0,0, FHintText);
  end;
end;
 
procedure TBigCustomEdit.SetHintText(const Value: string);
begin
  FHintText := Value;
  // Redraw
  Invalidate;
end;

end.
