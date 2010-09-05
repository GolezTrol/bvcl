{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 10, 2009.
http://www.nldelphi.com/forum/showthread.php?t=32977
*******************************************************************************
 Uses a TBigProcHook to work around a bug in the VCL that causes an access
 violation at address 00000000 when AltGr is pressed. Problem occurs for
 non-admin users on Win2k and WinXP systems in Delphi 2005 to 2007 (and maybe
 others as well).
 To work around this problem, just include this unit in the project. This unit
 will automatically hook the IsAltGrPressed function and in-memory encapsulate
 it in a try..except block. If an error occurs in IsAltGrPressed, this error is
 ignored and the function will return False, like it should.
******************************************************************************}
unit BigAltGrCrashWorkaround;

interface

implementation

// Only in Delphi 9, 10, and 11 (2005 - 2007)
{$IFDEF DELPHI9UP} {$IFNDEF DELPHI12UP}

uses
  BigProcHook, Menus;

var
  FHook: TBigProcHook;

// The replacement function
function SafeIsAltGrPressed: Boolean;
begin
  try
    // Restore the original function
    FHook.Hooked := False;
    try
      // Call the original function
      Result := IsAltGRPressed;
    finally
      // Restore the hook
      FHook.Hooked := True;
    end;
  except
    // Exception: Error when reading keyboard layout dll.
    Result := False;
  end;
end;

initialization
  FHook := TBigProcHook.Create(@IsAltGRPressed, @SafeIsAltGrPressed);
finalization
  FHook.Hooked := False;
  FHook.Free;
{$ENDIF}{$ENDIF}
end.
