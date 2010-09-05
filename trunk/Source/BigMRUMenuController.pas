{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2010 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jul 25, 2010.
http://www.nldelphi.com/forum/showthread.php?p=295893#post295893
*******************************************************************************
 Controller class to be used with TBigMRU to use a menu-item as MRU menu.
******************************************************************************}
unit BigMRUMenuController;

interface

uses
  Classes, BigMRU, SysUtils, Menus;

type
  TBigMRUMenuController = class(TBigCustomMRUController)
  private
    FMenuList: TStringList;
    FParentMenuItem: TMenuItem;
    procedure SetParentMenuItem(const Value: TMenuItem);
  protected
    procedure Refresh(Subject: TBigCustomMRU); override;
    procedure Clear;
    procedure ItemClick(Sender: TObject);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ParentMenuItem: TMenuItem
             read FParentMenuItem write SetParentMenuItem;
    property OnExecute;
    property OnGetCaption;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Big VCL', [
    TBigMRUMenuController
    ]);
end;

{ TBigMRUMenuController }

procedure TBigMRUMenuController.Clear;
var
  i: Integer;
begin
  for i := 0 to FMenuList.Count - 1 do
    FMenuList.Objects[i].Free;

  FMenuList.Clear;
end;

constructor TBigMRUMenuController.Create(AOwner: TComponent);
begin
  inherited;
  FMenuList := TStringList.Create;
end;

destructor TBigMRUMenuController.Destroy;
begin
  SetParentMenuItem(nil);

  Clear;
  FMenuList.Free;

  inherited;
end;

procedure TBigMRUMenuController.ItemClick(Sender: TObject);
var
  n: Integer;
begin
  n := FMenuList.IndexOfObject(Sender);
  if n = -1 then
    raise Exception.Create('Clicked item not found');

  Execute(FMenuList[n]);
end;

procedure TBigMRUMenuController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FParentMenuItem then
      SetParentMenuItem(nil);
end;

procedure TBigMRUMenuController.Refresh(Subject: TBigCustomMRU);
var
  i: Integer;
  Item: TMenuItem;
  FileName: String;
begin
  inherited;

  Clear;

  if Assigned(FParentMenuItem) then
  begin
    for i := 0 to Subject.MRUList.Count - 1 do
    begin
      FileName := Subject.MRUList[i];

      Item := TMenuItem.Create(nil);
      Item.OnClick := ItemClick;
      Item.Caption := GetCaption(FileName);

      FParentMenuItem.Add(Item);

      FMenuList.AddObject(FileName, Item);
    end;
  end;

end;

procedure TBigMRUMenuController.SetParentMenuItem(const Value: TMenuItem);
var
  i: Integer;
begin
  if FParentMenuItem <> Value then
  begin
    if Assigned(FParentMenuItem) then
    begin
      FParentMenuItem.RemoveFreeNotification(Self);

      if Assigned(FParentMenuItem) then
        for i := 0 to FMenuList.Count - 1 do
          FParentMenuItem.Remove(TMenuItem(FMenuList.Objects[i]));
    end;

    FParentMenuItem := Value;

    if Assigned(FParentMenuItem) then
    begin
      FParentMenuItem.FreeNotification(Self);

      if Assigned(FParentMenuItem) then
        for i := 0 to FMenuList.Count - 1 do
          FParentMenuItem.Add(TMenuItem(FMenuList.Objects[i]));
    end;
  end;
end;

end.
