{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 21, 2003.
http://www.nldelphi.com/forum/showthread.php?t=8221
*******************************************************************************
 Unfinished: TBigClipboardViewer is a control to display a clipboard history
 like the Microsoft Office clipboard viewer.
******************************************************************************}

unit BigClipboardViewer;

interface

uses
  Windows, Graphics, Classes, Controls;

type
  TBigClipboardDrawItemEvent =
    procedure(Sender: TObject; DC: HDC; Rect: TRect) of object;

  TBigClipboardViewer = class(TCustomControl)
  private
    FOnDrawItem: TBigClipboardDrawItemEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    property OnDrawItem: TBigClipboardDrawItemEvent
      read FOnDrawItem write FOnDrawItem;
  end;

implementation

{ TBigClipboardViewer }

constructor TBigClipboardViewer.Create(AOwner: TComponent);
begin
  inherited;
  Color := clWhite;
end;

destructor TBigClipboardViewer.Destroy;
begin

  inherited;
end;

procedure TBigClipboardViewer.Paint;
var
  Can: TCanvas;
  Org: TPoint;
  R: TRect;
  Rgn: HRgn;
  DC: HDC;
begin
  inherited;
  Can := TCanvas.Create;
  // Het stuk waarop ik wil tekenen moet relatief aan
  // het scherm worden bepaald.
  Org := GetClientOrigin;
  R := Rect(10, 10, 70, 40);
  OffsetRect(R, Org.x, Org.y);
  Rgn := CreateRectRgnIndirect(R);
  // Vraag een nieuwe Device Context op voor de
  // verkregen Region
  DC := GetDCEx(Handle, Rgn, DCX_CACHE or
          DCX_CLIPSIBLINGS or DCX_INTERSECTRGN);
  try
    if DC > 0 then
    begin
      // Canvas koppelen aan de device context en tekenen
      Can.Handle := DC;
      Can.Brush.Color := clRed;
      Can.Pen.Color := clBlue;
      Can.Pen.Width := 3;
      // En hieronder is het probleem:
      Can.FillRect(Rect(10, 10, 300, 300));
      Can.Rectangle(10,10, 300, 300);
    end;
  finally
    Can.Free;
    ReleaseDC(Handle, DC);
    DeleteObject(Rgn);
  end;
end;

end.
