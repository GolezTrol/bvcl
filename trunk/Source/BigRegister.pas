{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Dec 14, 2008.
*******************************************************************************
 Registers visual controls of the Big Visual Component Library
******************************************************************************}
unit BigRegister;

{$I BVCL.inc}

interface

uses
  {$IFDEF DELPHI6UP}DesignIntf, DesignEditors,{$ELSE}Dsgnintf,{$ENDIF}
  Classes;

{$R dclBVCL_D7.dcr}

procedure Register;

implementation

uses
  BigButton, BigDialogs, BigEdit, BigLabel, BigNumEdit, BigStartButton,
  BigSubtitleLabel, BigClipboardMonitor, BigMiniGames, BigImageList,
  BigMRU, BigMRUMenuController, BigMRURegistryStorage;

procedure Register;
begin
  RegisterComponents('Big VCL', [
    TBigButton, TBigFontDialog, TBigLabel, TBigSubtitleLabel, TBigImageList,
    TBigEdit, TBigNumEdit, TBigClipboardMonitor,
    TBigMRU, TBigMRUMenuController, TBigMRURegistryStorage,
    TBigMiniGameHost
    ]);
end;

end.
