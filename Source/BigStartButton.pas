{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2002-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released May 02, 2002.
*******************************************************************************
 Finds the start button and allows to set some of it's properties.
*******************************************************************************
To do:
 - Turn this into a TComponent
 - Build a hook that allows to detect changes. Windows XP restores its
   startbutton frequently)
 - Test if this will work at all under Vista and 7 (no, it doesn't, does it?)
******************************************************************************}
unit BigStartButton;

interface

uses Windows, SysUtils;

type
  TBigStartButton = class
  private
    FHandle: HWND;
    function FindHandle: HWND;
  protected
    function GetHandle: HWND; virtual;

    function GetParent: HWND; virtual;
    function GetParentBorderWidth: Integer; virtual;
    function GetText: string; virtual;
    function GetVisible: Boolean; virtual;
    procedure SetParent(const Value: HWND); virtual;
    procedure SetText(const Value: string); virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    function GetHeight: Integer; virtual;
    function GetLeft: Integer; virtual;
    function GetTop: Integer; virtual;
    function GetWidth: Integer; virtual;
    procedure SetHeight(const Value: Integer); virtual;
    procedure SetLeft(const Value: Integer); virtual;
    procedure SetTop(const Value: Integer); virtual;
    procedure SetWidth(const Value: Integer); virtual;
  public
    constructor Create; virtual;

    property Parent: HWND read GetParent write SetParent;
    property Handle: HWND read GetHandle;
    property Text: string read GetText write SetText;

    property Visible: Boolean read GetVisible write SetVisible;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

implementation

uses Types;

{ TBigStartButton }

constructor TBigStartButton.Create;
begin
  FHandle := FindHandle; // Find it already
end;

function TBigStartButton.FindHandle: HWND;
// Find the actual handle of the window. Assumably this is the first BUTTON
// on the taskbar.
var
  Tray, Child: HWND;
  c: array[0..127] of Char;
begin
  Result := 0;
  Tray := FindWindow('Shell_TrayWnd', nil);
  Child := GetWindow(Tray, GW_CHILD);
  while Child <> 0 do
  begin
    if (GetClassName(Child, c, SizeOf(c)) > 0) and
        (UpperCase(c) = 'BUTTON') then
    begin
      Result := Child;
      Exit;
    end;
    Child := GetWindow(Child, GW_HWNDNEXT);
  end;
end;

function TBigStartButton.GetHandle: HWND;
// Check if previously found handle is still valid. If not, find again.
begin
  if not IsWindow(FHandle) then
    FHandle := FindHandle;
  Result := FHandle;
end;

function TBigStartButton.GetHeight: Integer;
var
  r: TRect;
begin
  Result := -1;
  if GetWindowRect(GetHandle, r) then
    Result := r.Bottom - r.Top;
end;

function TBigStartButton.GetLeft: Integer;
var
  r: TRect;
begin
  Result := -1;
  if GetWindowRect(GetHandle, r) then
    Result := r.Left;
  if GetParent <> 0 then
    if GetWindowRect(GetParent, r) then
      Result := Result - r.Left - GetParentBorderWidth;
end;

function TBigStartButton.GetParent: HWND;
// The Startbutton's parentwindow
begin
  Result := Windows.GetParent(GetHandle);
end;

function TBigStartButton.GetParentBorderWidth: Integer;
// The borderwidth of the parentwindow
var
  r,c: TRect;
begin
  Result := -1;
  if GetWindowRect(GetParent, r) then
    if GetClientRect(GetParent, c) then
      Result := ((r.Right-r.Left)-c.Right) div 2;
end;

function TBigStartButton.GetText: string;
// Retreive the caption
var
  c: array[0..127] of Char;
begin
  Result := '';
  if GetWindowText(GetHandle, c, SizeOf(c)) > 0 then
    Result := c;
end;

function TBigStartButton.GetTop: Integer;
var
  r: TRect;
begin
  Result := -1;
  if GetWindowRect(GetHandle, r) then
    Result := r.Top;
  if GetParent <> 0 then
    if GetWindowRect(GetParent, r) then
      Result := Result - r.Top - GetParentBorderWidth;
end;

function TBigStartButton.GetVisible: Boolean;
begin
  Result := IsWindowVisible(GetHandle);
end;

function TBigStartButton.GetWidth: Integer;
var
  r: TRect;
begin
  Result := -1;
  if GetWindowRect(GetHandle, r) then
    Result := r.Right - r.Left;
end;

procedure TBigStartButton.SetHeight(const Value: Integer);
begin
  SetWindowPos(GetHandle, 0, 0, 0, GetWidth, Value, SWP_NOZORDER or SWP_NOMOVE)
end;

procedure TBigStartButton.SetLeft(const Value: Integer);
begin
  SetWindowPos(GetHandle, 0, Value, GetTop, 0, 0, SWP_NOZORDER or SWP_NOSIZE)
end;

procedure TBigStartButton.SetParent(const Value: HWND);
begin
  Windows.SetParent(GetHandle, Value);
end;

procedure TBigStartButton.SetText(const Value: string);
// Set the text and update the window (needed for immediate show of change)
begin
  SetWindowText(GetHandle, PChar(Copy(Value, 1, 127)));
  InvalidateRect(GetHandle, nil, True)
end;

procedure TBigStartButton.SetTop(const Value: Integer);
begin
  SetWindowPos(GetHandle, 0, GetLeft, Value, 0, 0, SWP_NOZORDER or SWP_NOSIZE)
end;

procedure TBigStartButton.SetVisible(const Value: Boolean);
begin
  if Value then
    ShowWindow(GetHandle, SW_SHOW)
  else
    ShowWindow(GetHandle, SW_HIDE);
end;

procedure TBigStartButton.SetWidth(const Value: Integer);
begin
  SetWindowPos(GetHandle, 0, 0, 0, Value, GetHeight, SWP_NOZORDER or SWP_NOMOVE)
end;

end.
