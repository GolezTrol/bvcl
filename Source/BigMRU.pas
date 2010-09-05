{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2010 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jul 25, 2010.
http://www.nldelphi.com/forum/showthread.php?p=295893#post295893
*******************************************************************************
 Component that keeps a MRU list.
 Uses a storage class that can be overridden to implement various storage
 methods.
 Uses a controller class to display the MRU list and keep it in sync with the
 linked control (like a TMenu)
******************************************************************************}
unit BigMRU;

interface

uses
  Classes;

type
  // TBigCustomMRU
  // The main component that wraps the MRU list

  TBigCustomMRU = class;

  // TBigCustomMRUStorage
  // Base class for a storage component. Descendants can save the list to any
  // desired storage.
  TBigCustomMRUStorage = class(TComponent)
  protected
    procedure Load(Subject: TBigCustomMRU); virtual; abstract;
    procedure Save(Subject: TBigCustomMRU); virtual; abstract;
  end;

  // TBigCustomMRUController
  // Base class for a controller class that presents a UI for selecting files
  // from the MRU list.

  // The event that is called when a file is selected
  TBigMRUFileExecuteEvent =
      procedure(Sender: TObject; const AFile: string) of object;
  // The event that is called to return a caption for a file
  TBigMRUGetCaptionEvent  =
      procedure(Sender: TObject; const AFile: string; var ACaption: string) of object;

  TBigCustomMRUController = class(TComponent)
  private
    FOnExecute: TBigMRUFileExecuteEvent;
    FOnGetCaption: TBigMRUGetCaptionEvent;
  protected
    procedure Refresh(Subject: TBigCustomMRU); virtual;
    procedure Execute(const AFile: string); virtual;

    function GetCaption(FileName: string): string; virtual;

    property OnExecute: TBigMRUFileExecuteEvent
             read FOnExecute write FOnExecute;
    property OnGetCaption: TBigMRUGetCaptionEvent
             read FOnGetCaption write FOnGetCaption;
  end;

  TBigCustomMRU = class(TComponent)
  private
    FMRUList: TStringList;
    FSaveOnDestroy: Boolean;
    FSaveOnChange: Boolean;
    FMaxItems: Integer;
    FController: TBigCustomMRUController;
    FStorage: TBigCustomMRUStorage;
    procedure SetController(const Value: TBigCustomMRUController);
    procedure SetStorage(const Value: TBigCustomMRUStorage);
  protected
    procedure Loaded; override;

    property SaveOnChange: Boolean
             read FSaveOnChange write FSaveOnChange
             default True;
    property SaveOnDestroy: Boolean
             read FSaveOnDestroy write FSaveOnDestroy
             default False;
    property MaxItems: Integer
             read FMaxItems write FMaxItems
             default 10;

    property Storage: TBigCustomMRUStorage
             read FStorage write SetStorage;
    property Controller: TBigCustomMRUController
             read FController write SetController;

    procedure Changed;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure Truncate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MRUList: TStringList read FMRUList;

    procedure Add(const AFile: string); virtual;
    procedure Clear; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
  end;

  // Actual component to use, with published properties.
  TBigMRU = class(TBigCustomMRU)
  published
    property SaveOnChange;
    property SaveOnDestroy;
    property MaxItems;

    property Storage;
    property Controller;
  end;

procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('Big VCL', [
    TBigMRU
    ]);
end;

{ TBigCustomMRU }

procedure TBigCustomMRU.Add(const AFile: string);
var
  n: Integer;
begin
  inherited;

  // Put the new file on top of the list, except when it's already there.
  n := FMRUList.IndexOf(AFile);
  if n > 0 then
    FMRUList.Delete(n);
  if n <> 0 then
    FMRUList.Insert(0, AFile);

  Truncate;
  Changed;
end;

procedure TBigCustomMRU.Changed;
begin
  // Save the MRU list
  if FSaveOnChange then
    Save;

  // Refresh the MRU controller
  if Assigned(FController) then
    FController.Refresh(Self);
end;

procedure TBigCustomMRU.Clear;
begin
  // Clears the MRU list.
  FMRUList.Clear;
  
  Changed;
end;

constructor TBigCustomMRU.Create(AOwner: TComponent);
begin
  inherited;

  FMRUList := TStringList.Create;

  // Set property defaults
  FSaveOnChange := True;
  FMaxItems := 10;
end;

destructor TBigCustomMRU.Destroy;
begin
  // Save the MRU list before destroying the component
  if FSaveOnDestroy then
    Save;

  FMRUList.Free;

  inherited;
end;

procedure TBigCustomMRU.Load;
begin
  if not (csLoading in ComponentState) then
    if Assigned(FStorage) then
    begin
      FStorage.Load(Self);
      Truncate;
      Changed;
    end;
end;

procedure TBigCustomMRU.Loaded;
begin
  inherited;
  Load;
end;

procedure TBigCustomMRU.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  // Check if any of the linked components is freed. If so, break the connection.
  if Operation = opRemove then
  begin
    if AComponent = FController then
      SetController(nil);
    if AComponent = FStorage then
      SetStorage(nil);
  end;
end;

procedure TBigCustomMRU.Save;
begin
  // Silently ignore if no storage is linked.
  if Assigned(FStorage) then
    FStorage.Save(Self);
end;

procedure TBigCustomMRU.SetController(
  const Value: TBigCustomMRUController);
begin
  // If the controller is changed, unlink the old controller (if any) and
  // connect the new one
  if FController <> Value then
  begin
    if Assigned(FController) then
      FController.RemoveFreeNotification(Self);

    FController := Value;

    if Assigned(FController) then
    begin
      FController.FreeNotification(Self);
      FController.Refresh(Self);
    end;
  end;
end;

procedure TBigCustomMRU.SetStorage(const Value: TBigCustomMRUStorage);
begin
  // If the storage is changed, unlink the old storage (if any) and
  // connect the new one
  if FStorage <> Value then
  begin
    if Assigned(FStorage) then
      FStorage.RemoveFreeNotification(Self);

    FStorage := Value;

    if Assigned(FStorage) then
      FStorage.FreeNotification(Self);
  end;
  Load;
end;

procedure TBigCustomMRU.Truncate;
begin
  if (FMaxItems > 0) then
    while FMRUList.Count > FMaxItems do
      FMRUList.Delete(FMRUList.Count - 1);
end;

{ TBigCustomMRUController }

procedure TBigCustomMRUController.Execute(const AFile: string);
begin
  // Execute the OnExecute event to which the application can respond by opening
  // the given file.
  if Assigned(FOnExecute) then
    FOnExecute(Self, AFile);
end;

function TBigCustomMRUController.GetCaption(FileName: string): string;
begin
  // Return a caption for a filename. The default is the filename itself, but then
  // application can modify it in the OnGetCaption event.
  Result := FileName;

  if Assigned(FOnGetCaption) then
    FOnGetCaption(Self, FileName, Result);
end;

procedure TBigCustomMRUController.Refresh(Subject: TBigCustomMRU);
begin
  // Do nothing here. Descendant can choose to implement this method or not.
end;

end.
