{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jun 3, 2003.
http://www.nldelphi.com/forum/showthread.php?t=8221
*******************************************************************************
 TBigClipboardMonitor will trigger an event whenever the contents of the
 clipboard changes.
******************************************************************************}

unit BigClipboardMonitor;

interface

uses
  Windows, Messages, Classes;

type
  TClipboardChangeEvent = TNotifyEvent;

  { TBigCustomClipboardMonitor }
  TBigCustomClipboardMonitor = class(TComponent)
  private
    FMonitorWindow: THandle;
    FNextWindow: THandle;
    FMonitoring: Boolean;
    FOnClipboardChange: TClipboardChangeEvent;

    procedure PassMessage(Message: TMessage);
    procedure SetMonitoring(const Value: Boolean);
  protected
    procedure DoClipboardChange; virtual;
    procedure WndProc(var Message: TMessage); virtual;

    property OnClipboardChange: TClipboardChangeEvent
      read FOnClipboardChange write FOnClipboardChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Monitoring: Boolean
      read FMonitoring write Setmonitoring;
  end;

  TBigClipboardMonitor = class(TBigCustomClipboardMonitor)
  published
    property OnClipboardChange;
    property Monitoring;
  end;

implementation

{ TBigCustomClipboardMonitor }

constructor TBigCustomClipboardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMonitoring := False;
end;

destructor TBigCustomClipboardMonitor.Destroy;
begin
  SetMonitoring(False);
  inherited;
end;

procedure TBigCustomClipboardMonitor.DoClipboardChange;
begin
  if Assigned(FOnClipboardChange) then
    FOnClipboardChange(Self);
end;

procedure TBigCustomClipboardMonitor.PassMessage(Message: TMessage);
begin
  SendMessage(FNextWindow, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TBigCustomClipboardMonitor.SetMonitoring(const Value: Boolean);
begin
  if FMonitoring = Value then
    Exit;

  FMonitoring := Value;

  if not (csDesigning in ComponentState) then
  begin
    if Value then
    begin
      FMonitorWindow := AllocateHWnd(WndProc);
      FNextWindow := SetClipBoardViewer(FMonitorWindow);
    end
    else
    begin
      ChangeClipboardChain(FMonitorWindow, FNextWindow);
      DeallocateHWnd(FMonitorWindow);
    end;
  end;
end;

procedure TBigCustomClipboardMonitor.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_DRAWCLIPBOARD then
  begin
    // This Message is also triggered when starting or stopping to monitor
    // In these cases BigClipboard.FormatCount will always return 0 (??)
    try
      DoClipboardChange;
    finally
      PassMessage(Message);
    end;
  end;

  //wParam = HWNDRemove
  //lParam = HWNDNext
  if Message.Msg = WM_CHANGECBCHAIN then
  begin
    // If next window is the one to be removed, LParam is the new NextWindow.
    // Else pass the message to our NextWindow
    if FNextWindow = THandle(Message.WParam) then
      FNextWindow := Message.LParam
    else
      PassMessage(Message);
  end;

  // Thanks to Peter Gessner: The lines below are needed to correctly respond
  // to WM_QUERYENDSESSION message
  with Message do
    Result := DefWindowProc(FMonitorWindow, Msg, wParam, lParam);
end;

end.
