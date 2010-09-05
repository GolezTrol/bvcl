{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 09, 2009.
*******************************************************************************
-TBigProcHook allows to override a given function with another by inserting a
 JMP to the new function. In the new function you can disable the hook to call
 the old function again.
-TBigSemaphore implements wraps a nameless semaphore into a syncobject class.
******************************************************************************}

unit BigClasses;

interface

uses
  Windows, sysUtils, SyncObjs;

type
{ TBigProcHook allows to override a given function with another by inserting a
  JMP to the new function. In the new function you can disable the hook to call
  the old function again. }
  PHack = ^THook;
  THook = packed record
    OpCodeCall : Byte;
    OFFTo      : Integer;
    OpCodeRet  : Byte;
  end;
  TBackup = THook;

  TBigProcHook = class
  private
    FOldProc, FNewProc: Pointer;
    FBackupped: Boolean;
    FHooked: Boolean;
    FOriginal: TBackup;
    procedure SetHooked(const Value: Boolean);
  protected
    procedure InstallHook(Hook: THook);
    procedure OverwriteProc;
  public
    constructor Create(AOldProc, ANewProc: Pointer; Install: Boolean = True);
    property Hooked: Boolean read FHooked write SetHooked;
  end;

{ TBigSemaphore wraps a nameless semaphore into a syncobject class. }
  TSemaphore = class(TSynchroObject)
  private
    FSemaphore: THandle;
  public
    constructor Create(AMaxConnections: Integer); virtual;
    procedure Acquire; overload; override;
    function  Acquire(ATimeOutMilliseconds: Cardinal): Boolean; overload; dynamic;
    procedure Release; override;
  end;

implementation

{ TBigProcHook }

constructor TBigProcHook.Create(AOldProc, ANewProc: Pointer;
  Install: Boolean);
begin
  inherited Create;

  FOldProc := AOldProc;
  FNewProc := ANewProc;

  if Install then
    SetHooked(True);
end;

procedure TBigProcHook.InstallHook(Hook: THook);
var
  OldProtect: Cardinal;
begin
  // Change protection of oldproc memory
  if VirtualProtect(FOldProc, SizeOf(THook), PAGE_EXECUTE_READWRITE, OldProtect) then
  try
    if not FBackupped then
    begin
      Move(FOldProc^, FOriginal, SizeOf(THook));
      FBackupped := True;
    end;
    // Overwrite the old procedure
    Move(Hook, FOldProc^, SizeOf(THook));
  finally
    VirtualProtect(FOldProc, SizeOf(THook), OldProtect, OldProtect);
  end
  else
  begin
    RaiseLastOSError;
  end;
end;

procedure TBigProcHook.OverwriteProc;
// Overwrites the first few calls of OldProc with a call to NewProc and a Ret.
var
  Hook: THook;
begin
  // Create a tiny little redirection
  with Hook do begin
    OpCodeCall := $E8; // = CALL}
    OFFTo      := PAnsiChar(FNewProc) - PAnsiChar(FOldProc) - 5;
    OpCodeRet  := $C3; // = RET
  end;

  InstallHook(Hook);
end;

procedure TBigProcHook.SetHooked(const Value: Boolean);
begin
  // Toggle hook.
  if FHooked <> Value then
    if Value then
      OverwriteProc
    else
      InstallHook(FOriginal);

  FHooked := Value;
end;

{ TSemaphore }

procedure TSemaphore.Acquire;
begin
  Acquire(INFINITE);
end;

function TSemaphore.Acquire(ATimeOutMilliseconds: Cardinal): Boolean;
begin
  case WaitForSingleObject(FSemaphore, ATimeOutMilliseconds) of
    WAIT_OBJECT_0: Result := True;
    WAIT_TIMEOUT: Result := False;
    WAIT_ABANDONED: raise Exception.Create('Semaphore was abandoned');
    WAIT_FAILED: RaiseLastOSError
  else
    raise Exception.Create('Unknown exception while aquiring semaphore');
  end;
end;

constructor TSemaphore.Create(AMaxConnections: Integer);
begin
  inherited Create;

  FSemaphore := CreateSemaphore(nil, AMaxConnections, AMaxConnections, nil);
  if FSemaphore = 0 then
    RaiseLastOSError;
end;

procedure TSemaphore.Release;
begin
  if not ReleaseSemaphore(FSemaphore, 1, nil) then
    RaiseLastOSError;
end;

end.
