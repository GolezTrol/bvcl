{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2010 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jul 25, 2010.
http://www.nldelphi.com/forum/showthread.php?p=295893#post295893
*******************************************************************************
 Storage class to be used with TBigMRU to store a MRU list in the registry.
******************************************************************************}
unit BigMRURegistryStorage;

interface

uses
  Windows, Classes, BigMRU, Registry;

type
  TBigHKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine);

  TBigMRURegistryStorage = class(TBigCustomMRUStorage)
  private
    FRootKey: TBigHKey;
    FValueName: string;
    FKeyName: string;
  protected
    procedure Load(Subject: TBigCustomMRU); override;
    procedure Save(Subject: TBigCustomMRU); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RootKey: TBigHKey
             read FRootKey write FRootKey default hkCurrentUser;
    property KeyName: string read FKeyName write FKeyName;
    property ValueName: string read FValueName write FValueName;
  end;

implementation



{ TBigMRURegistryStorage }

constructor TBigMRURegistryStorage.Create(AOwner: TComponent);
begin
  inherited;

  FRootKey := hkCurrentUser;
end;

procedure TBigMRURegistryStorage.Load(Subject: TBigCustomMRU);
begin
  inherited;
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := DWord($80000000) or DWord(FRootKey);
    if OpenKeyReadOnly(FKeyName) then
      Subject.MRUList.CommaText := ReadString(FValueName);
  finally
    Free;
  end;
end;

procedure TBigMRURegistryStorage.Save(Subject: TBigCustomMRU);
begin
  inherited;
  with TRegistry.Create(KEY_WRITE) do
  try
    RootKey := DWord($80000000) or DWord(FRootKey);
    if OpenKey(FKeyName, True) then
      WriteString(FValueName, Subject.MRUList.CommaText);
  finally
    Free;
  end;
end;

end.
