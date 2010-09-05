{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Nov 18, 2008.
http://www.nldelphi.com/forum/showthread.php?t=32638
*******************************************************************************
 TBigNumEdit is a numeric edit. It supports negative and floating point numbers.
 It supports left, central and right alignment of text.
 Illegal characters are filtered directly while typing, pasting or otherwise
 modifying the content of the edit.
******************************************************************************}

unit BigNumEdit;

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, BigEdit;

type
  TBigCustomNumEdit = class(TBigCustomEdit)
  private
    FAlignment: TAlignment;
    FAllowNegative: Boolean;
    FAllowFloatingPoint: Boolean;

    procedure SetAlignment(const Value: TAlignment);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetText(var Message: TWMGetDlgCode); message WM_SETTEXT;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    function ValidateContent: Boolean; virtual;
    procedure SetAllowFloatingPoint(const Value: Boolean);
    procedure SetAllowNegative(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taRightJustify;
    property AllowNegativeNumbers: Boolean read FAllowNegative write SetAllowNegative default True;
    property AllowFloatingPoint: Boolean read FAllowFloatingPoint write SetAllowFloatingPoint default False;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBigNumEdit = class(TBigCustomNumEdit)
  published
    // TBigCustomNumEdit
    property Alignment;
    property AllowNegativeNumbers;
    property AllowFloatingPoint;

    // TBigCustomEdit
    property HintText;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
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

uses StdCtrls;

{ TBigCustomNumEdit }

procedure TBigCustomNumEdit.Change;
begin
  // Only call inherited change when the text has validated, otherwise
  // the OnChange event is called twice.
  if ValidateContent then
    inherited;
end;

procedure TBigCustomNumEdit.CMEnter(var Message: TCMGotFocus);
begin
  inherited;
  // By default AutoSelect does not work for multiline edits, so we need to
  // override this one as well. 
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
end;

constructor TBigCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited;

  FAlignment := taRightJustify; // More usual for numedits

  FAllowNegative := True;
  FAllowFloatingPoint := False;

  Text := '0';
end;

procedure TBigCustomNumEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER),(ES_RIGHT, ES_LEFT, ES_CENTER));
begin
  // Inpired by TMemo
  inherited CreateParams(Params);
  // To allow right alignment, the edit must also use the MultiLine style.
  with Params do
  begin
    Style := Style and not ES_AUTOHSCROLL or ES_MULTILINE or
             Alignments[UseRightToLeftAlignment, FAlignment];
  end;
end;

procedure TBigCustomNumEdit.KeyPress(var Key: Char);
begin
  // Stolen from TMemo
  inherited KeyPress(Key);
  if (Key = Char(VK_RETURN)) then Key := #0;
end;

procedure TBigCustomNumEdit.SetAlignment(const Value: TAlignment);
begin
  // Stolen from TMemo
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TBigCustomNumEdit.SetAllowFloatingPoint(const Value: Boolean);
begin
  // Actually, validation is only needed when the property is switched from
  // True to False..
  if FAllowFloatingPoint <> Value then
  begin
    FAllowFloatingPoint := Value;
    ValidateContent;
  end;
end;

procedure TBigCustomNumEdit.SetAllowNegative(const Value: Boolean);
begin
  // Actually, validation is only needed when the property is switched from
  // True to False..
  if FAllowNegative <> Value then
  begin
    FAllowNegative := Value;
    ValidateContent;
  end;
end;

function TBigCustomNumEdit.ValidateContent: Boolean;
var
  i, j: Integer;
  t: string;
  ss, sl: Integer;
  SeparatorFound: Boolean;
begin
  j := 0; i := 0; ss := 0; sl := 0;
  t := Text;
  SeparatorFound := False;

  // Save selection only if handle is allocated. If it isn't, there isn't a
  // selection. But even more: trying to get or set the selection of an edit
  // without a parent results in an error, therefore just check if the handle
  // is allocated. If it is, we're safe.
  if HandleAllocated then
  begin
    ss := SelStart;
    sl := SelLength;
  end;

  // Loop the string and eliminate all unneeded characters
  while i < Length(t) do
  begin
    Inc(i);
    // Check if the character is a number or another allowed character.
    if (t[i] in ['0'..'9']) or
       ((t[i] = '-') and (j = 0) and FAllowNegative) or
       ((t[i] = DecimalSeparator) and (not SeparatorFound) and FAllowFloatingPoint)
      then
    begin
      // This character is valid. Move it to position j in the string.
      // j = i as long as all processed characters are valid.
      Inc(j);
      t[j] := t[i];
      // Allow only one decimal separator
      if t[i] = DecimalSeparator then
        SeparatorFound := True;
    end else
    begin
      // If this character is not valid, it is not copied and j is not adjusted.
      // Instead selstart and sellength are shifted if needed.
      if ss > j then
        Dec(ss)
      else if ss + sl > j then
        Dec(sl);
    end;
  end;

  // If j = i, then all characters were valid.
  Result := j = i;

  // Not quite waterproof, and I'm not sure how I want this to work. If the text
  // starts and/or ends with a decimal separator, we can add a zero in front of
  // and/or behind the text. In this case, we can shift the selection too.
  // For now, decided to skip this feature at all. After all, both '.1' and '1.'
  // will convert correctly with StrToFloat, just as if these 0's would have
  // been added.

  {if SeparatorFound and (t[j] = DecimalSeparator) then
  begin
    // If a zero is added behind the last decimal separator, we can
    // a) keep the pivot right behind the separator. The disadvantage is that
    //    after typing a separator, you can't backspace to empty the edit,
    //    because that extra 0 is behind the pivot. (Not a big problem, though)
    // b) like a, but select the 0. This way it will be overwritten when actual
    //    numbers are typed. The disadvantage is that you can't backspace past
    //    the separator, because removing it will add and select it again.
    t := t + '0';
    //if ss + sl = j then // uncomment these lines if b)
    //  Inc(sl);
    Inc(j);
    Result := False;
  end;

  if SeparatorFound and (t[1] = DecimalSeparator) then
  begin
    // If a zero is added before the decimal separator, there is not really a
    // problem. This works reasonable.
    Inc(j);
    Insert('0', t, 1);
    Inc(ss);
    Result := False;
  end;}

  if not Result then
  begin
    // If invalid characters were found, set the corrected text.
    SetLength(t, j);
    Text := t;
    // Restore the selection.
    if HandleAllocated then
    begin
      SelStart := ss;
      SelLength := sl;
    end;
  end;
end;

procedure TBigCustomNumEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  // Inspired by TMemo
  Message.Result := Message.Result and (not DLGC_WANTTAB);
  Message.Result := Message.Result and (not DLGC_WANTALLKEYS);
end;

procedure TBigCustomNumEdit.WMSetText(var Message: TWMGetDlgCode);
begin
  inherited;
  // I certainly wish I didn't need to, because in most cases, it is not needed.
  // However, when calling Perform with the WM_SETTEXT parameter, there is no
  // change event triggered. This would allow the text to be set to an invalid
  // value programmatically. Therefor, CM_TEXTCHANGED is sent, risking the
  // validation to be performed twice instead of once in other cases.
  Perform(CM_TEXTCHANGED, 0, 0);
end;

end.
