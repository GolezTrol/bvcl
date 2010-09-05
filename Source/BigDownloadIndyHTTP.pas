{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 14, 2010.
http://www.nldelphi.com/forum/showthread.php?p=287227#post287227
*******************************************************************************
An Indy HTTP implementation for the TBigDownload base class.
To be used by TBigDownloadManager.
******************************************************************************}
unit BigDownloadIndyHTTP;

interface

uses
  SysUtils, Classes,
  IdHTTP, IdComponent,
  BigDownloadManager;

type
  TBigDownloadIndyHTTP = class(TBigDownload)
  private
    FIdHTTP: TIdHTTP;
    procedure DoWorkBegin(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Integer);
    procedure DoWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure DoWork(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Integer);
  public
    destructor Destroy; override;
    procedure Execute; override;
    procedure Cancel; override;
  end;

implementation

{ TBigDownloadIndyHTTP }

procedure TBigDownloadIndyHTTP.Cancel;
begin
  FreeAndNil(FIdHTTP);
  inherited;
end;

destructor TBigDownloadIndyHTTP.Destroy;
begin
  FreeAndNil(FIdHTTP);
  
  inherited;
end;

procedure TBigDownloadIndyHTTP.DoWork(Sender: TObject;
  AWorkMode: TWorkMode; const AWorkCount: Integer);
begin
  PostFeedback(BDM_PROGRESS, AWorkCount);
end;

procedure TBigDownloadIndyHTTP.DoWorkBegin(Sender: TObject;
  AWorkMode: TWorkMode; const AWorkCountMax: Integer);
begin
  PostFeedback(BDM_SETSIZE, AWorkCountMax);
end;

procedure TBigDownloadIndyHTTP.DoWorkEnd(Sender: TObject;
  AWorkMode: TWorkMode);
begin
  SetState(dsFinished);
end;

procedure TBigDownloadIndyHTTP.Execute;
begin
  if Terminated then
    Exit;

  SetState(dsBusy);
  try
    FIdHTTP := TIdHTTP.Create(nil);
    try
      FIdHTTP.OnWorkBegin := Self.DoWorkBegin;
      FIdHTTP.OnWorkEnd := Self.DoWorkEnd;
      FIdHTTP.OnWork := Self.DoWork;

      FIdHTTP.Get(Uri, Stream);

      if Terminated or (FIdHTTP = nil) then
        SetState(dsCancelled)
      else if State <> dsFinished then // Download is finished, but state is not?
        SetState(dsError);
    finally
      FreeAndNil(FIdHTTP);
    end;
  except
    SetState(dsError);
  end;
end;

end.
