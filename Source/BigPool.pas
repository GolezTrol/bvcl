{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2010 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

*******************************************************************************
 Concept for a generic, thread safe pool of objects. Could be used as a
 connection pool. Needs some serious testing, though.
******************************************************************************}
unit BigPool;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, BigClasses;

type
  EPoolException = class(Exception);

  TPool = class;

{ Interface that is stored in the pool }

  IPoolObject = interface(IUnknown)
    ['{9709BBA9-E5AC-4EF9-89EB-F83889E0EB88}']
    function GetLastAccess: TDateTime;
    function GetRefCount: Integer;

    property LastAccess: TDateTime read GetLastAccess;
    property RefCount: Integer read GetRefCount;
    procedure SetPool(APool: TPool);
  end;

{ Basic implementation for IPoolObject that can be used as a base or as example.
  Could have been inherited from TInterfacedObject, but most of its methods are
  not virtual nor dynamic }

  TPoolObject = class(TInterfacedObject, IInterface, IPoolObject)
  protected
    FLastAccess : TDateTime;
    FPool       : TPool;
    function _Release: Integer; virtual; stdcall;

    function GetRefCount: Integer;
    function GetLastAccess: TDateTime;
    procedure SetPool(APool: TPool);
  end;

{ Interface that refers to an object. This allows the object (any object) being returned by the interface }

  IObjectReference = interface
    function GetObject: TObject;
  end;

{ If you need, you can implement IObjectReference, or inherit from
  TPoolWrapObject to pool objects that do not implement IPoolObject themselves }

  TPoolWrapObject = class(TPoolObject, IObjectReference)
  protected
    function GetObject: TObject; virtual; abstract;
  end;

{ Base class for pool object factory}

  TPoolObjectFactory = class
    class function CreateInstance: IPoolObject; virtual; abstract;
    class procedure UpdateInstance(Instance: IPoolObject); virtual; abstract;
  end;
  TPoolObjectFactoryClass = class of TPoolObjectFactory;

{ Thread for creating and updating pool objects }
  TPoolObjectThread = class(TThread)
  private
    FPool: TPool;
    FPoolObject: IPoolObject;
  protected
    procedure Execute; override;
  end;

{ Actual pool implementation }

  TPool = class(TObject)
  private
    FObjects : TInterfaceList;
    FLock    : TCriticalSection;
    FQueue   : TSemaphore;
    FFactory : TPoolObjectFactoryClass;

    function CreateOrUpdatePoolObject( AObject              : IPoolObject;
                                       ATimeOutMilliseconds : Cardinal): IPoolObject;
  protected
    procedure Release(AObject: IPoolObject);
  public
    constructor Create( AFactory               : TPoolObjectFactoryClass;
                        APoolSize              : Cardinal = INFINITE;
                        APoolDecayMilliSeconds : Cardinal = 0;
                        ASafeCreate            : Boolean = True ); virtual;
    destructor Destroy; override;

    function Acquire(ATimeOutMilliseconds: Cardinal = INFINITE): IPoolObject;
  end;

implementation

uses DateUtils;

{ TPool }

function TPool.Acquire(ATimeOutMilliseconds: Cardinal = INFINITE): IPoolObject;
var
  StartTime: TDateTime;
  i: Integer;
  PoolObject: IPoolObject;
begin
  Result := nil;
  StartTime := Now;

  if Assigned(FQueue) and not FQueue.Acquire(ATimeOutMilliseconds) then
    raise EPoolException.Create('Timed out while waiting for queue');

  FLock.Acquire;
  try
    // Try to locate an unused object in the pool and return it.
    for i := 0 to FObjects.Count - 1 do
    begin
      if Assigned(FObjects[i]) then
      begin
        PoolObject := FObjects[i] as IPoolObject;
        if PoolObject.RefCount = 2 {PoolObject variable + FObjects list} then
        begin
          Result := PoolObject;

          CreateOrUpdatePoolObject(Result, ATimeOutMilliseconds - MilliSecondsBetween(StartTime, Now));
          Exit;
        end;
      end;
    end;

    // If no unused object is found, create a new object, add it to the list
    // and return it.
    if not Assigned(Result) then
    begin
      Result := CreateOrUpdatePoolObject(nil, ATimeOutMilliseconds - MilliSecondsBetween(StartTime, Now));
      FObjects.Add(Result);
      Exit;
    end;

  finally
    FLock.Release;
  end;

end;

constructor TPool.Create( AFactory               : TPoolObjectFactoryClass;
                          APoolSize              : Cardinal;
                          APoolDecayMilliseconds : Cardinal;
                          ASafeCreate            : Boolean);
begin
  inherited Create;

  FFactory := AFactory;
  FLock := TCriticalSection.Create;
  if APoolSize <> INFINITE then
    FQueue := TSemaphore.Create(APoolSize);

  FObjects := TInterfaceList.Create;
end;

function TPool.CreateOrUpdatePoolObject( AObject              : IPoolObject;
                                         ATimeOutMilliseconds : Cardinal): IPoolObject;
var
  t: TPoolObjectThread;
begin
  Result := nil;

  t := TPoolObjectThread.Create(True);
  t.FPool := Self;
  t.FPoolObject := AObject;
  t.Resume;

  if WaitForSingleObject(t.Handle, ATimeOutMilliseconds) = WAIT_OBJECT_0 then
  begin
    Result := t.FPoolObject;
    t.Free;
  end
  else
    t.FreeOnTerminate := True;
end;

destructor TPool.Destroy;
begin
  FObjects.Clear;

  FObjects.Free;
  FLock.Free;
  FQueue.Free;

  inherited;
end;

procedure TPool.Release(AObject: IPoolObject);
begin
  FQueue.Release;
end;

{ TPoolObject }

function TPoolObject.GetLastAccess: TDateTime;
begin
  Result := FLastAccess;
end;

function TPoolObject.GetRefCount: Integer;
begin
  Result := FRefCount;
end;

procedure TPoolObject.SetPool(APool: TPool);
begin
  FPool := APool;
end;

function TPoolObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);

  if Result = 0 then
    Destroy
  else
  begin
    FLastAccess := Now;
    if Result = 1 then
      FPool.Release(Self);
  end;
end;

{ TPoolObjectThread }

procedure TPoolObjectThread.Execute;
begin
  if FPoolObject = nil then
  begin
    FPoolObject := FPool.FFactory.CreateInstance;
    FPoolObject.SetPool(FPool);
  end
  else
    FPool.FFactory.UpdateInstance(FPoolObject);
end;

end.
