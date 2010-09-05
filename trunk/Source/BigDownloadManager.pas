{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 14, 2010.
http://www.nldelphi.com/forum/showthread.php?p=287227#post287227
*******************************************************************************
A simple threaded download manager.
******************************************************************************}

unit BigDownloadManager;

interface

uses
  Windows, Messages, SysUtils, Classes;

const
  // BigDownloadMessages are sent by the download to the manager.
  // Download is created and should be added to the list
  BDM_QUEUED          = WM_APP;
  // State of download has changed
  BDM_STATECHANGE     = BDM_QUEUED + 1;
  // Size of download is known
  BDM_SETSIZE         = BDM_QUEUED + 2;
  // Progress of the download, shows size in bytes
  BDM_PROGRESS        = BDM_QUEUED + 3;
  // Download is destroyed and should be removed from the list
  BDM_DESTROYED       = BDM_QUEUED + 4;

// TBigDownload: Abstract base class for download threads
// Descendants can implement the actual downloading in their own way. This base
// class implements state properties as well as communication with manager.
type
  TDownloadState = (dsQueued, dsBusy, dsFinished, dsCancelled, dsError);

  TBigDownload = class(TThread)
  private
    FUri: String;
    FHandle: THandle;
    FState: TDownloadState;
    FStream: TStream;
    FOwnsStream: Boolean;
  protected
    // Send feedback to download manager
    procedure PostFeedback(Msg: Cardinal; Value: Integer = 0);
    procedure SendFeedback(Msg: Cardinal; Value: Integer = 0);

    procedure SetState(AState: TDownloadState);
    procedure ThreadTerminate(Sender: TObject);
  public
    constructor Create(AHandle: THandle; AUri: String; AStream: TStream);
    destructor Destroy; override;

    procedure Cancel; virtual;
    property State: TDownloadState read FState;
    property Uri: String read FUri;
    property Stream: TStream read FStream;
  end;

  TBigDownloadClass = class of TBigDownload;


// TBigDownloadManager: Manages download threads. For each added uri, a thread
// is created. The thread registers and removes itself and posts status updates
// by sending or posting messages to the window handle of the manager.
type
  TDMMessage = packed record
    Msg: Cardinal;
    Sender: TBigDownload;
    Value: Integer;
    Result: Longint;
  end;

type
  TBigDownloadNotifyEvent =
    procedure(Sender: TBigDownload) of object;
  TBigDownloadStateChangeEvent =
    procedure(Sender: TBigDownload; State: TDownloadState) of object;
  TBigDownloadProgressEvent =
    procedure(Sender: TBigDownload; Progress: Integer) of object;

  TBigDownloadManager = class(TComponent)
  private
    FWindowHandle: THandle;
    FDownloadList: TList;

    FOnRemove: TBigDownloadNotifyEvent;
    FOnQueue: TBigDownloadNotifyEvent;
    FOnProgress: TBigDownloadProgressEvent;
    FOnStateChange: TBigDownloadStateChangeEvent;
    FDownloadClass: TBigDownloadClass;
    FOnSetSize: TBigDownloadProgressEvent;
  protected
    procedure WndProc(var Msg: TMessage);

    procedure DoStateChange(Sender: TBigDownload; State: TDownloadState);
    procedure DoProgress(Sender: TBigDownload; Progress: Integer);
    procedure DoQueue(Sender: TBigDownload);
    procedure DoRemove(Sender: TBigDownload);
    procedure DoSetSize(Sender: TBigDownload; Size: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Add(AUri: String; AStream: TStream): TBigDownload;
    property DownloadClass: TBigDownloadClass
             read FDownloadClass write FDownloadClass;

  published
    property OnStateChange: TBigDownloadStateChangeEvent
             read FOnStateChange write FOnStateChange;
    property OnSetSize: TBigDownloadProgressEvent
             read FOnSetSize write FOnSetSize;
    property OnProgress: TBigDownloadProgressEvent
             read FOnProgress write FOnProgress;
    property OnQueue: TBigDownloadNotifyEvent
             read FOnQueue write FOnQueue;
    property OnRemove: TBigDownloadNotifyEvent
             read FOnRemove write FOnRemove;
  end;

implementation

{ TBigDownloadManager }

constructor TBigDownloadManager.Create(AOwner: TComponent);
begin
  inherited;

  // Create a window handle to receive messages from download threads
  FWindowHandle := Classes.AllocateHWnd(WndProc);

  FDownloadList := TList.Create;
end;

destructor TBigDownloadManager.Destroy;
var
  i: Integer;
begin
  // Cancel all downloads
  for i := FDownloadList.Count - 1 downto 0 do
  begin
    TBigDownload(FDownloadList[i]).Cancel;
  end;

  FDownloadList.Free;

  Classes.DeallocateHWnd(FWindowHandle);

  inherited;
end;

function TBigDownloadManager.Add(AUri: String; AStream: TStream): TBigDownload;
begin
  // Instantiate a download thread and return it.
  if DownloadClass = nil then
    raise Exception.Create('Please assign a download class first');

  Result := DownloadClass.Create(FWindowHandle, AUri, AStream);
end;

procedure TBigDownloadManager.DoProgress(Sender: TBigDownload;
  Progress: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Progress);
end;

procedure TBigDownloadManager.DoQueue(Sender: TBigDownload);
begin
  // Called when a download thread is created.
  FDownloadList.Add(Sender);

  if Assigned(FOnQueue) then
    FOnQueue(Sender);
end;

procedure TBigDownloadManager.DoRemove(Sender: TBigDownload);
var
  i: Integer;
begin
  // Called when a download thread is removed.
  i := FDownloadList.IndexOf(Sender);
  Assert(i > -1, 'Removing a download that isn''t in the list');
  FDownloadList.Delete(i);

  if Assigned(FOnRemove) then
    FOnRemove(Sender);
end;

procedure TBigDownloadManager.DoSetSize(Sender: TBigDownload;
  Size: Integer);
begin
  if Assigned(FOnSetSize) then
    FOnSetSize(Sender, Size);
end;

procedure TBigDownloadManager.DoStateChange(Sender: TBigDownload;
  State: TDownloadState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Sender, State);
end;

procedure TBigDownloadManager.WndProc(var Msg: TMessage);
var
  m: TDMMessage;
begin
  // Messages received from download threads
  m := TDMMessage(Msg);
  case m.Msg of
    BDM_STATECHANGE:
      DoStateChange(m.Sender, TDownloadState(m.Value));
    BDM_PROGRESS:
      DoProgress(m.Sender, m.Value);
    BDM_QUEUED:
      DoQueue(m.Sender);
    BDM_DESTROYED:
      DoRemove(m.Sender);
    BDM_SETSIZE:
      DoSetSize(m.Sender, m.Value);
  end;
end;

{ TBigDownload }

constructor TBigDownload.Create(AHandle: THandle; AUri: String; AStream: TStream);
begin
  inherited Create(True);

  FUri := AUri;
  FHandle := AHandle;

  // If no stream is passed, create a memory stream to hold the downloaded data.
  FOwnsStream := AStream = nil;
  FStream := AStream;
  if FStream = nil then
    FStream := TMemoryStream.Create;

  // Notify the manager
  PostFeedback(BDM_QUEUED);
  // SetState(dsQueued); Default state

  // Handle own OnTerminate event
  OnTerminate := ThreadTerminate;
end;

destructor TBigDownload.Destroy;
begin
  // Notify the manager
  SendFeedback(BDM_DESTROYED);

  // Free the memory stream if it is ours.
  if FOwnsStream then
    FStream.Free;

  inherited;
end;

procedure TBigDownload.Cancel;
var
  DoFree: Boolean;
begin
  // Terminate the thread. Descendants can try to cancel the download.
  Terminate;

  // If the download is not started yet, free it as well.
  DoFree := FState = dsQueued;
  FState := dsCancelled;
  if DoFree then
    Free;
end;

procedure TBigDownload.PostFeedback(Msg: Cardinal; Value: Integer);
begin
  // Post a message (non blocking)
  PostMessage(FHandle, Msg, Integer(Self), Value);
end;

procedure TBigDownload.SendFeedback(Msg: Cardinal; Value: Integer);
begin
  // Send a message (blocking)
  SendMessage(FHandle, Msg, Integer(Self), Value);
end;

procedure TBigDownload.SetState(AState: TDownloadState);
begin
  FState := AState;
  PostFeedback(BDM_STATECHANGE, Integer(FState));
end;

procedure TBigDownload.ThreadTerminate(Sender: TObject);
begin
  // TODO: Send this message again? Or create a BDM_FINISHED message which
  // passes the state as well? (Finished, Cancelled or Error)
  //SendFeedback(BDM_STATECHANGE, Integer(FState));
end;

end.
