{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Dec 2, 2006.
http://www.nldelphi.com/forum/showthread.php?t=26326
*******************************************************************************
 TBigLabel is a label that wraps text somewhat better than a regular TLabel.
 Text will break on spaces and dashes, but will also break if a word is too
 long to fit on a line. When AutoSize is True, it will also adjust the height
 to extactly match the corrected text height, without altering the width.
 Also, the label accepts an array of const, just like the format function. 
******************************************************************************}

unit BigLabel;

interface

{$INCLUDE BVCL.INC}

uses
  Windows, Graphics, Classes, SysUtils, StdCtrls, BigUtils, BigStrUtils;

type
  TBigCustomLabel = class(TCustomLabel)
  private
    FAutoSizing: Boolean;
    FArgs: TConstArray;
  protected
    procedure AdjustBounds; override;
    function GetLabelText: string; override;
  public
    destructor Destroy; override;
    procedure SetArgs(AArgs: array of const);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TBigLabel = class(TBigCustomLabel)
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

uses Controls;

{ TBigCustomLabel }

procedure TBigCustomLabel.AdjustBounds;
begin
  FAutoSizing := True;
  inherited;
  FAutoSizing := False;
end;

destructor TBigCustomLabel.Destroy;
begin
  FinalizeConstArray(FArgs);
  
  inherited;
end;

function TBigCustomLabel.GetLabelText: string;
var
  AHeight: Integer;
begin
  // Get the text and adjust it by inserting line breaks at the right places.
  Result := inherited GetLabelText;

  // Format the result with the given parameters.
  if Length(FArgs) > 0 then
  try
    Result := Format(Result, FArgs);
  except
    // If an error occurs while designing, display the error in the label
    // instead of raising an exception.
    on e: Exception do
    begin
      if csDesigning in ComponentState then
        Result := e.Message
      else
        raise;
    end;
  end;

  // If WordWrap is true, break up the string with returns.
  if WordWrap then
  begin
    CorrectTextWrapping(Result, Width, AHeight, Canvas);
  end;
end;

procedure TBigCustomLabel.SetArgs(AArgs: array of const);
begin
  FinalizeConstArray(FArgs);
  FArgs := CopyConstArray(AArgs);
end;

procedure TBigCustomLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Prevent the width from being modified while autosizing.
  if FAutoSizing then
    AWidth := Width;
  inherited;
end;

end.

