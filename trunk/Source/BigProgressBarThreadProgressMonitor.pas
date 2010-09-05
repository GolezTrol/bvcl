{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Sep 05, 2009.
http://www.nldelphi.com/forum/showthread.php?p=281131#post281131
*******************************************************************************
 TBigProgressBarThreadProgressMonitor is a wrapper around TProgressBar that can
 be passed to a TBigProgressThread.
******************************************************************************}

unit BigProgressBarThreadProgressMonitor;

interface

uses
  BigThreadProgress, ComCtrls;

type
  TBigProgressBarThreadProgressMonitor = class(TBigCustomThreadProgressMonitor)
  private
    FProgressBar: TProgressBar;
  protected
    procedure SetMax(AMax: Integer); override;
    procedure SetPosition(APosition: Integer); override;
    procedure StepBy(ADelta: Integer); override;
  public
    constructor Create(AProgressBar: TProgressBar);
  end;

implementation

{ TBigProgressBarThreadProgressMonitor }

constructor TBigProgressBarThreadProgressMonitor.Create(
  AProgressBar: TProgressBar);
begin
  FProgressBar := AProgressBar;
end;

procedure TBigProgressBarThreadProgressMonitor.SetMax(AMax: Integer);
begin
  FProgressBar.Max := AMax;
end;

procedure TBigProgressBarThreadProgressMonitor.SetPosition(APosition: Integer);
begin
  FProgressBar.Position := APosition;
end;

procedure TBigProgressBarThreadProgressMonitor.StepBy(ADelta: Integer);
begin
  FProgressBar.StepBy(ADelta);
end;

end.
