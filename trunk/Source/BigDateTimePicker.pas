{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released May 13, 2009.
http://www.nldelphi.com/forum/showthread.php?t=33863
*******************************************************************************
Provide OnUp and OnDown events for TDateTimepicker in dmUpDown datemode.

TODO: Catch KeyDown to redirect Up and Down arrows to events as well.
******************************************************************************}

unit BigDateTimePicker;

interface

uses
  Windows, Messages, Classes, SysUtils, ComCtrls, CommCtrl;

type
  TBigDateTimePicker = class(TDateTimePicker)
  private
    FOnDown: TNotifyEvent;
    FOnUp: TNotifyEvent;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure DoUp;
    procedure DoDown;
  published
    property OnUp: TNotifyEvent read FOnUp write FOnUp;
    property OnDown: TNotifyEvent read FOnDown write FOnDown;
  end;

implementation

{ TBigDateTimePicker }

procedure TBigDateTimePicker.DoDown;
begin
  if Assigned(FOnDown) then
    FOnDown(Self);
end;

procedure TBigDateTimePicker.DoUp;
begin
  if Assigned(FOnUp) then
    FOnUp(Self);
end;

procedure TBigDateTimePicker.WndProc(var Message: TMessage);
begin
  try

    if Message.Msg = WM_NOTIFY then
    begin
      if PNMHdr(Message.LParam).code = UDN_DELTAPOS then
      begin
        if PNMUpDown(Message.LParam).iDelta > 0 then
          DoUp;
        if PNMUpDown(Message.LParam).iDelta < 0 then
          DoDown;
      end;
    end;

  except
    on E: EAbort do
      Exit
    else
      raise;
  end;

  inherited;
end;

end.
