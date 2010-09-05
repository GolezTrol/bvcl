{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 25, 2008.
- Jan 25, 2008: TBigFontDialog.
  A font dialog that allow selecting more than 16 colors
http://www.nldelphi.com/forum/showthread.php?t=30255

- Sep 08, 2008: MessageDlgTimer variations
  A series of messages boxes that close automatically after a period of time.
http://www.nldelphi.com/forum/showthread.php?t=32104
******************************************************************************}

unit BigDialogs;

interface

uses
  Windows, Forms, Controls, SysUtils, Graphics, Classes, Messages, Dialogs,
  ExtCtrls, CommDlg, Menus;

type
  TSampleType = (stText, stHTMLColor, stDelphiColor);

  TBigFontDialog = class(TFontDialog)
  private
    FColorCaption: THandle;
    FColorCombo: THandle;
    FColorButton: THandle;
    FColorButtonWndProc: Pointer;
    FColorButtonOldProc: Pointer;
    FSampleTextWndProc: Pointer;
    FSampleTextOldProc: Pointer;
    FSampleText: THandle;

    FChooseFont: PChooseFont;
    FPreviewText: string;
    FSampleType: TSampleType;
    FColorPicker: TColorDialog;
  protected
    function UpdateControlHandles: Boolean; virtual;
    procedure SetSampleText(const Value: string); virtual;
    procedure DoShow; override;
    procedure ColorButtonWndProc(var AMsg: TMessage);
    procedure SampleTextWndProc(var AMsg: TMessage);
    procedure PickColor;
    procedure UpdateColor;
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
    function GetPreviewText(AColor: TColor): string;
    procedure WndProc(var AMsg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SampleText: string read FPreviewText write SetSampleText;
    property SampleType: TSampleType read FSampleType write FSampleType default stText;
  end;

function GetColorComboHandleProc(AHandle: HWND; AItems: TStrings): Boolean; stdcall;

const
  WM_UPDATECOLOR = WM_USER + 1;

  FONTDIALOG_ID_FONTCAPTION       = $0440;
  FONTDIALOG_ID_FONTCOMBO         = $0470;
  FONTDIALOG_ID_FONTLIST          = $03E8;
  FONTDIALOG_ID_FONTEDIT          = $03E9;
  FONTDIALOG_ID_FONTSTYLECAPTION  = $0441;
  FONTDIALOG_ID_FONTSTYLECOMBO    = $0471;
  FONTDIALOG_ID_FONTSTYLELIST     = $03E8;
  FONTDIALOG_ID_FONTSTYLEEDIT     = $03E9;
  FONTDIALOG_ID_FONTSIZECAPTION   = $0442;
  FONTDIALOG_ID_FONTSIZECOMBO     = $0472;
  FONTDIALOG_ID_FONTSIZELIST      = $03E8;
  FONTDIALOG_ID_FONTSIZEEDIT      = $03E9;
  FONTDIALOG_ID_EFFECTSGROUP      = $0430;
  FONTDIALOG_ID_STRIKEOUTCHECKBOX = $0410;
  FONTDIALOG_ID_UNDERLINECHECKBOX = $0411;
  FONTDIALOG_ID_COLORCAPTION      = $0443;
  FONTDIALOG_ID_COLORBUTTON       = $0000;
  FONTDIALOG_ID_COLORCOMBO        = $0473;
  FONTDIALOG_ID_SAMPLEGROUP       = $0431;
  FONTDIALOG_ID_SAMPLETEXT        = $0444;
  FONTDIALOG_ID_UNKNOWNLABEL      = $0445;
  FONTDIALOG_ID_SCRIPTCAPTION     = $0446;
  FONTDIALOG_ID_SCRIPTCOMBO       = $0474;
  FONTDIALOG_ID_OKBUTTON          = $0001;
  FONTDIALOG_ID_CANCELBUTTON      = $0002;
  FONTDIALOG_ID_APPLYBUTTON       = $0402;
  FONTDIALOG_ID_HELPBUTTON        = $040E;

  COLORDIALOG_ID_BASICCOLORSCAPTION       = $FFFF;
  COLORDIALOG_ID_UNKNOWNLABEL1            = $02D0;
  COLORDIALOG_ID_CUSTOMCOLORSCAPTION      = $FFFF;
  COLORDIALOG_ID_UNKNOWNLABEL2            = $02D1;
  COLORDIALOG_ID_DEFINECUSTOMCOLORSBUTTON = $02CF;
  COLORDIALOG_ID_OKBUTTON                 = $0001;
  COLORDIALOG_ID_CANCELBUTTON             = $0002;
  COLORDIALOG_ID_HELPBUTTON               = $040E;
  COLORDIALOG_ID_UNKNOWNLABEL3            = $02C6;
  COLORDIALOG_ID_UNKNOWNLABEL4            = $02BE;
  COLORDIALOG_ID_UNKNOWNLABEL5            = $02C5;
  COLORDIALOG_ID_UNKNOWNLABELo            = $02C9;
  COLORDIALOG_ID_COLORCAPTION             = $02DA;
  COLORDIALOG_ID_SOLIDCAPTION             = $02DB;
  COLORDIALOG_ID_HUECAPTION               = $02D3;
  COLORDIALOG_ID_HUEEDIT                  = $02BF;
  COLORDIALOG_ID_SATCAPTION               = $02D4;
  COLORDIALOG_ID_SATEDIT                  = $02C0;
  COLORDIALOG_ID_LUMCAPTION               = $02D5;
  COLORDIALOG_ID_LUMEDIT                  = $02C1;
  COLORDIALOG_ID_REDCAPTION               = $02D6;
  COLORDIALOG_ID_REDEDIT                  = $02C2;
  COLORDIALOG_ID_GREENCAPTION             = $02D7;
  COLORDIALOG_ID_GREENEDIT                = $02C3;
  COLORDIALOG_ID_BLUECAPTION              = $02D8;
  COLORDIALOG_ID_BLUEEDIT                 = $02C4;
  COLORDIALOG_ID_ADDTOCUSTOMCOLORSBUTTON  = $02C8;


type
  TMessageBoxAutoClose = class(TComponent)
  private
    FTimer: TTimer;
    FDialog: TCustomForm;
    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
  protected
    property TimeOut: Integer read GetTimeOut write SetTimeOut;
    procedure DoTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function MessageDlgTimer(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint;
    ATimeOut: Integer = -1): Integer;
function MessageDlgPosTimer(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
    ATimeOut: Integer = -1): Integer;
function MessageDlgPosHelpTimer(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
    const HelpFileName: string; ATimeOut: Integer = -1): Integer;

procedure ShowMessageTimer(const Msg: string; ATimeOut: Integer = -1);
procedure ShowMessageFmtTimer(const Msg: string; Params: array of const;
    ATimeOut: Integer = -1);
procedure ShowMessagePosTimer(const Msg: string; X, Y: Integer;
    ATimeOut: Integer = -1);

implementation

function GetColorComboHandleProc(AHandle: HWND; AItems: TStrings): Boolean; stdcall;
// Can be used to retreive dialog item ids.
// From the dialogs OnShow event, execute something like the following line:
// EnumChildWindows(TBigFontDialog(Sender).Handle,
//                  @GetColorComboHandleProc, Integer(Memo1.Lines))
var
  cls, txt: string;
  id: Integer;
  p: Cardinal;
begin
   Result := True;

   id := GetDlgCtrlID(AHandle);

   SetLength(cls, 256);
   GetClassName(AHandle, PChar(cls), 255);
   SetLength(txt, GetWindowTextLength(AHandle) + 1);
   GetWindowText(AHandle, PChar(txt), Length(txt));
   cls := PChar(cls);
   p := GetWindowLong(AHandle, GWL_HWNDPARENT);

   AItems.Add(Format('Id: %x, Handle: %d, Parent: %d, Class: %s, Text: %s', [id, AHandle, p, PChar(cls), PChar(txt)]));
end;

{ TBigFontDialog }

procedure TBigFontDialog.ColorButtonWndProc(var AMsg: TMessage);
begin
  // Catch messages to see if button is clicked, spaces or entered.
  if ( AMsg.Msg = WM_LBUTTONUP) or
     ( (AMsg.Msg = WM_KEYUP) and (TWMKey(AMsg).CharCode in [VK_SPACE]) ) or
     ( (AMsg.Msg = WM_KEYDOWN) and (TWMKey(AMsg).CharCode in [VK_RETURN]) ) then
  begin
    PickColor;
  end
  else
  begin
    // Pass all button messages to the buttons original winproc
    AMsg.Result := CallWindowProc(FColorButtonOldProc, FColorButton,
      AMsg.Msg, AMsg.WParam, AMsg.LParam);

    // Tell windows that this buttons wants to process the Enter key
    if (AMsg.Msg = WM_GETDLGCODE) and (AMsg.WParam = VK_RETURN) then
      TWMGetDlgCode(AMsg).Result := TWMGetDlgCode(AMsg).Result or DLGC_WANTALLKEYS;

  end;
end;

function TBigFontDialog.GetPreviewText(AColor: TColor): string;
begin
  // Color to string, without using identifier. Could be adjusted to return
  // Graphics.GetPreviewText or use an array of HTML color names.
  case FSampleType of
    stText:
      Result := FPreviewText;
    stHTMLColor:
      Result := Format('%s%2.2x%2.2x%2.2x', ['#', AColor and $FF,
                    AColor shr 8 and $FF, AColor shr 16 and $FF]);
    stDelphiColor:
      Result := Format('%s%.8x', [HexDisplayPrefix, AColor]);
  end;
end;

constructor TBigFontDialog.Create(AOwner: TComponent);
begin
  inherited;

  FPreviewText := 'AaBbYyZz'; // Customizable

  // Allow methods to be used as WndProc
  FColorButtonWndProc := MakeObjectInstance(ColorButtonWndProc);
  FSampleTextWndProc := MakeObjectInstance(SampleTextWndProc);
  // Use a color dialog instead of the normal color combobox
  FColorPicker := TColorDialog.Create(Self);
end;

destructor TBigFontDialog.Destroy;
begin
  FColorPicker.Free;
  FreeObjectInstance(FSampleTextWndProc);
  FreeObjectInstance(FColorButtonWndProc);
  inherited;
end;

procedure TBigFontDialog.DoShow;
var
  Parent: THandle;
  Pos: TPoint;
  Caption: string;
  r: TRect;
  Width: Integer;
begin
  if UpdateControlHandles then
  begin
    // Set the text of the preview label, to overwrite the default 'AaBbYyZz'
    FSampleTextOldProc := Pointer(SetWindowLong(FSampleText, GWL_WNDPROC,
        Integer(FSampleTextWndProc)));
    SendDlgItemMessage(Self.Handle, FONTDIALOG_ID_SAMPLETEXT, WM_SETTEXT, 0, 0);

    // Get the caption of the color label and strip it's hotkey
    SetLength(Caption, GetWindowTextLength(FColorCaption));
    GetWindowText(FColorCaption, @Caption[1], Length(Caption));
    Caption := StripHotkey(Caption);

    // Get the parent of the color combo. The button will be inserted there
    Parent := GetWindowLong(FColorCombo, GWL_HWNDPARENT);

    // Get the position of the label and the width of the combo. These will
    // be used for the button.
    GetWindowRect(FColorCombo, r);
    Width := r.Right - r.Left;
    GetWindowRect(FColorCaption, r);
    Pos := Point(r.Left, r.Top);
    ScreenToClient(Parent, Pos);

    //Create a button and assign it a message handler
    FColorButton := CreateWindowEx(0, 'BUTTON', PChar(Caption),
        WS_TABSTOP or WS_CHILD,
        Pos.X, Pos.Y, Width, 23,
        Parent, 0, 0, nil);
    FColorButtonOldProc := Pointer(SetWindowLong(FColorButton, GWL_WNDPROC,
        Integer(FColorButtonWndProc)));

    // Position button correctly in the tab order
    SetWindowPos(FColorButton, FColorCaption,
                 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);

    // Hide the combo and its label and show the button
    ShowWindow(FColorCaption, SW_HIDE);
    ShowWindow(FColorCombo, SW_HIDE);
    ShowWindow(FColorButton, SW_SHOWNORMAL);

    UpdateColor;
  end;

  inherited;
end;

procedure TBigFontDialog.PickColor;
begin
  // Show color picker
  FColorPicker.Color := FChooseFont.rgbColors;

  if FColorPicker.Execute then
  begin

    // If the use picked a different color, store it.
    if Cardinal(FColorPicker.Color) <> FChooseFont.rgbColors then
    begin
      FChooseFont.rgbColors := FColorPicker.Color;

      UpdateColor;
    end;
  end;
end;

procedure TBigFontDialog.SampleTextWndProc(var AMsg: TMessage);
begin
  // Overrule WM_SETTEXT message. Always use the preview text
  if AMsg.Msg = WM_SETTEXT then
    TWMSetText(AMsg).Text := PChar(GetPreviewText(FChooseFont.rgbColors));

  AMsg.Result := CallWindowProc(FSampleTextOldProc, FSampleText,
    AMsg.Msg, AMsg.WParam, AMsg.LParam);
end;

procedure TBigFontDialog.SetSampleText(const Value: string);
begin
  FPreviewText := Value;
  // Send the new text
  SendDlgItemMessage(Handle, FONTDIALOG_ID_SAMPLETEXT, WM_SETTEXT, 0, 0);
end;

function TBigFontDialog.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
begin
  // Store a reference to the FontDialogData record
  FChooseFont := PChooseFont(@DialogData);

  Result := inherited TaskModalDialog(DialogFunc, DialogData);

  // Destroy our custom button
  SetWindowLong(FColorButton, GWL_WNDPROC, Integer(FColorButtonOldProc));
  DestroyWindow(FColorButton);
end;

function TBigFontDialog.UpdateControlHandles: Boolean;
begin
  // Get handles of relevant controls
  FColorCombo := GetDlgItem(Handle, FONTDIALOG_ID_COLORCOMBO);
  FColorCaption := GetDlgItem(Handle, FONTDIALOG_ID_COLORCAPTION);
  FSampleText := GetDlgItem(Handle, FONTDIALOG_ID_SAMPLETEXT);
  Result := (FColorCombo <> 0) and (FSampleText <> 0);
end;

procedure TBigFontDialog.WndProc(var AMsg: TMessage);
begin
  inherited;
end;

procedure TBigFontDialog.UpdateColor;
begin
  SendDlgItemMessage(Handle, FONTDIALOG_ID_SAMPLETEXT, WM_SETTEXT, 0, 0);

  // Now, hack into the hidden color combo to update the sample text
  // Select the first item in the color combobox
  SendDlgItemMessage(Self.Handle, FONTDIALOG_ID_COLORCOMBO, CB_SETCURSEL, 0, 0);
  // Change the color of the first item
  SendDlgItemMessage(Handle, FONTDIALOG_ID_COLORCOMBO, CB_SETITEMDATA, 0, FChooseFont.rgbColors);

  // TODO -cTidy: Find better way to update preview.
  // Sample text control won't respond to UpdateWindow etc.  Trick
  // the control by actually sending key messages to the hidden combobox
  SendDlgItemMessage(Handle, FONTDIALOG_ID_COLORCOMBO, WM_KEYDOWN, VK_DOWN, 0);
  SendDlgItemMessage(Handle, FONTDIALOG_ID_COLORCOMBO, WM_KEYDOWN, VK_UP, 0);
end;

{ TMessageBoxAutoClose }

constructor TMessageBoxAutoClose.Create(AOwner: TComponent);
begin
  inherited;

  if AOwner is TCustomForm then
    FDialog := TCustomForm(AOwner)
  else
    raise Exception.Create(Self.ClassName + '.Owner must be a form.');
  FTimer := TTimer.Create(Self);
  SetTimeOut(-1);
  FTimer.OnTimer := DoTimer;
end;

destructor TMessageBoxAutoClose.Destroy;
begin
  FTimer.Free;

  inherited;
end;

procedure TMessageBoxAutoClose.DoTimer(Sender: TObject);
begin
  FDialog.ModalResult := mrCancel;
end;

function TMessageBoxAutoClose.GetTimeOut: Integer;
begin
  if FTimer.Enabled then
    Result := FTimer.Interval
  else
    Result := -1;
end;

procedure TMessageBoxAutoClose.SetTimeOut(const Value: Integer);
begin
  if Value <= 0 then
  begin
    FTimer.Enabled := False;
  end
  else
  begin
    FTimer.Interval := Value;
    FTimer.Enabled := True
  end;
end;

{Start message dialogs}
function MessageDlgPosHelpTimer(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
    const HelpFileName: string; ATimeOut: Integer = -1): Integer;
var
  MessageForm: TForm;
begin
  MessageForm := CreateMessageDialog(Msg, DlgType, Buttons);
  with MessageForm do
  try
    TMessageBoxAutoClose.Create(MessageForm).TimeOut := ATimeOut;

    HelpContext := HelpCtx;
    HelpFile := HelpFileName;
    if X >= 0 then Left := X;
    if Y >= 0 then Top := Y;
    if (Y < 0) and (X < 0) then Position := poScreenCenter;
    Result := ShowModal;
  finally
    Free;
  end;
end;

function MessageDlgPosTimer(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
    ATimeOut: Integer = -1): Integer;
begin
  Result := MessageDlgPosHelpTimer(Msg, DlgType, Buttons, HelpCtx,
      X, Y, '', ATimeOut);
end;

function MessageDlgTimer(const Msg: string; DlgType: TMsgDlgType;
    Buttons: TMsgDlgButtons; HelpCtx: Longint;
    ATimeOut: Integer = -1): Integer;
begin
  Result := MessageDlgPosHelpTimer(Msg, DlgType, Buttons, HelpCtx,
      -1, -1, '', ATimeOut);
end;

procedure ShowMessagePosTimer(const Msg: string; X, Y: Integer;
    ATimeOut: Integer = -1);
begin
  MessageDlgPosTimer(Msg, mtCustom, [mbOK], 0, X, Y, ATimeOut);
end;

procedure ShowMessageFmtTimer(const Msg: string; Params: array of const;
  ATimeOut: Integer = -1);
begin
  ShowMessageTimer(Format(Msg, Params), ATimeOut);
end;

procedure ShowMessageTimer(const Msg: string; ATimeOut: Integer = -1);
begin
  ShowMessagePosTimer(Msg, -1, -1, ATimeOut);
end;
{End message dialogs}

end.
