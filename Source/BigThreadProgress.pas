{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Sep 05, 2009.
http://www.nldelphi.com/forum/showthread.php?p=281131#post281131
*******************************************************************************
 TBigThreadProgress, an aggregation class that synchronises status updates to
 a given IBigThreadProgressMonitor.

 IBigThreadProgressMonitor, an interface that has methods for showing thread
 status. The methods are called in the main thread context.

 TBigCustomThreadProgressMonitor, an empty implementation thread monitor.

 TBigProgressThread, a base class for thread that need to show progress.
 Inherit from this class and implement Execute. Call Self.ThreadProgress methods
 to show progress.

*******************************************************************************
 Example:

procedure TForm1.Button1Click(Sender: TObject);
var
  Monitor: TBigCustomThreadProgressMonitor;
begin
  Monitor := TBigProgressBarThreadProgressMonitor.Create(ProgressBar1);

  with TDemoThread.Create(False, Monitor) do
    FreeOnTerminate := True;
end;

procedure TDemoThread.Execute;
var
  i: Integer;
begin
  inherited;
  ThreadProgress.SetMax(100);
  ThreadProgress.Show;

  for i := 0 to 100 do
  begin
    Sleep(50);  // Heavy duty code goes here
    ThreadProgress.SetPosition(i);
  end;

  ThreadProgress.Hide;
end;

******************************************************************************}
unit BigThreadProgress;

interface

uses
  Classes, SysUtils;

type
  // Thread monitor interface. This allow to write custom monitors, not descending form
  // TBigCustomThreadProgressMonitor.
  IBigThreadProgressMonitor = interface(IUnknown)
    procedure Show;
    procedure SetMax(AMax: Integer);
    procedure SetPosition(APosition: Integer);
    procedure StepBy(ADelta: Integer);
    procedure SetInfoText(AText: string);
    procedure Hide;
  end;

  // Base class for thread monitors.
  TBigCustomThreadProgressMonitor = class(TInterfacedObject, IBigThreadProgressMonitor)
  protected
    procedure Show; virtual;
    procedure SetMax(AMax: Integer); virtual;
    procedure SetPosition(APosition: Integer); virtual;
    procedure StepBy(ADelta: Integer); virtual;
    procedure SetInfoText(AText: string); virtual;
    procedure Hide; virtual;
  end;

  // Aggregation class that synchronises status updates to the monitor.
  TBigThreadProgress = class(TObject)
  private
    FMonitor: IBigThreadProgressMonitor;
    FInt: Integer;
    FStr: string;
    FThread: TThread;
    procedure SynchronisedShow;
    procedure SynchronisedSetMax;
    procedure SynchronisedSetPosition;
    procedure SynchronisedStepBy;
    procedure SynchronisedHide;
    procedure SynchronisedSetInfoText;
  public
    constructor Create(AThread: TThread; AMonitor: IBigThreadProgressMonitor);
    procedure Show;
    procedure SetMax(AMax: Integer);
    procedure SetPosition(APosition: Integer);
    procedure StepBy(ADelta: Integer);
    procedure SetInfoText(AText: string);
    procedure Hide;
  end;

  // Base class for threads implementing progress display
  TBigProgressThread = class(TThread)
  private
    FThreadProgress: TBigThreadProgress;
  protected
    procedure SetThreadProgressMonitor(const Value: IBigThreadProgressMonitor);
    property ThreadProgress: TBigThreadProgress read FThreadProgress;

    procedure DoTerminate; override;
  public
    constructor Create(CreateSuspended: Boolean;
        AThreadProgressMonitor: IBigThreadProgressMonitor); virtual;
  end;

implementation

{ TBigProgressThread }

constructor TBigProgressThread.Create(
    CreateSuspended: Boolean;
    AThreadProgressMonitor: IBigThreadProgressMonitor);
begin
  inherited Create(CreateSuspended);

  SetThreadProgressMonitor(AThreadProgressMonitor);
end;

procedure TBigProgressThread.DoTerminate;
begin
  inherited;
  if Assigned(FThreadProgress) then
    FreeAndNil(FThreadProgress);
end;

procedure TBigProgressThread.SetThreadProgressMonitor(
    const Value: IBigThreadProgressMonitor);
begin
  FreeAndNil(FThreadProgress);

  FThreadProgress := TBigThreadProgress.Create(Self, Value);
end;

{ TBigThreadProgress }

constructor TBigThreadProgress.Create(AThread: TThread;
  AMonitor: IBigThreadProgressMonitor);
begin
  inherited Create;
  FThread := AThread;
  FMonitor := AMonitor;
end;

procedure TBigThreadProgress.Hide;
begin
  if Assigned(FMonitor) then
    TBigProgressThread(FThread).Synchronize(SynchronisedHide);
end;

procedure TBigThreadProgress.SetInfoText(AText: string);
begin
  if Assigned(FMonitor) then
  begin
    FStr := AText;
    TBigProgressThread(FThread).Synchronize(SynchronisedSetInfoText);
  end;
end;

procedure TBigThreadProgress.SetMax(AMax: Integer);
begin
  if Assigned(FMonitor) then
  begin
    FInt := AMax;
    TBigProgressThread(FThread).Synchronize(SynchronisedSetMax);
  end;
end;

procedure TBigThreadProgress.SetPosition(APosition: Integer);
begin
  if Assigned(FMonitor) then
  begin
    FInt := APosition;
    TBigProgressThread(FThread).Synchronize(SynchronisedSetPosition);
  end;
end;

procedure TBigThreadProgress.Show;
begin
  if Assigned(FMonitor) then
    TBigProgressThread(FThread).Synchronize(SynchronisedShow);
end;

procedure TBigThreadProgress.StepBy(ADelta: Integer);
begin
  if Assigned(FMonitor) then
  begin
    FInt := ADelta;
    TBigProgressThread(FThread).Synchronize(SynchronisedStepBy);
  end;
end;

procedure TBigThreadProgress.SynchronisedHide;
begin
  FMonitor.Hide;
end;

procedure TBigThreadProgress.SynchronisedSetInfoText;
begin
  FMonitor.SetInfoText(FStr);
end;

procedure TBigThreadProgress.SynchronisedSetMax;
begin
  FMonitor.SetMax(FInt);
end;

procedure TBigThreadProgress.SynchronisedSetPosition;
begin
  FMonitor.SetPosition(FInt);
end;

procedure TBigThreadProgress.SynchronisedShow;
begin
  FMonitor.Show;
end;

procedure TBigThreadProgress.SynchronisedStepBy;
begin
  FMonitor.StepBy(FInt);
end;

{ TBigCustomThreadProgressMonitor }

// Empty implementations.
procedure TBigCustomThreadProgressMonitor.Hide; begin end;
procedure TBigCustomThreadProgressMonitor.SetInfoText(AText: string); begin end;
procedure TBigCustomThreadProgressMonitor.SetMax(AMax: Integer); begin end;
procedure TBigCustomThreadProgressMonitor.SetPosition(APosition: Integer); begin end;
procedure TBigCustomThreadProgressMonitor.Show; begin end;
procedure TBigCustomThreadProgressMonitor.StepBy(ADelta: Integer); begin end;

end.
