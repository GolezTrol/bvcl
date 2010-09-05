{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jun 3, 2003.
http://www.nldelphi.com/forum/showthread.php?t=8221
*******************************************************************************
 TBigClipboard is a TClipboard replacement that allows adding data in multiple
 clipboardformats, like rtf and plain text, simultaneously, so each program can
 select the best format they support.
 It also adds methods to get or set the contents of the clipboard into a stream.
******************************************************************************}

unit BigClipbrd;

interface

uses
  Windows, Messages, Graphics, Classes, SysUtils, Clipbrd;

type
  { TBigClipboard }
  TBigClipboard = class(TClipboard)
  protected
    // AddError raises an EConvertError. Used by Add()
    procedure AddError(Source: TPersistent); virtual;

    // Add procs for several types are called by Add().
    procedure AddGraphic(Source: TGraphic); virtual;
    procedure AddPicture(Source: TPicture); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Get data of specified format into stream instead of buffer
    procedure GetAsStream(Format: Integer; AStream: TStream); virtual;
    // Write stream data to clipboard
    procedure SetAsStream(Format: Integer; AStream: TStream); virtual;

    // Many Add methods provide the same functionionality as Set methods
    // in TClipboard, except for clearing clipboard contents first. Therefor
    // these functions are not explicitly clarified. The help of TClipBoard
    // should provide enough info about the use of these methods.
    procedure AddBuffer(Format: Word; const Buffer; Size: Integer); virtual;
    // Add, like Assign, but does not clear contents first
    procedure Add(Source: TPersistent); virtual;
    procedure AddComponent(Component: TComponent); virtual;
    procedure AddAsHandle(Format: Word; Value: THandle); virtual;
    procedure AddTextBuf(Buffer: PChar); virtual;
    procedure AddStream(Format: Integer; AStream: TStream); virtual;
  end;

function BigClipboard: TBigClipboard;

implementation

uses
  Forms,
  BigClipRes;

function BigClipboard: TBigClipboard;
begin
  // Replace regular Clipboard by a TBigClipboard.
  // Code that uses ClipBoard function instead of BigClipboard function should
  // still work fine.
  if not (Clipboard is TBigClipboard) then
    SetClipboard(TBigClipboard.Create).Free;
  Result := Clipboard as TBigClipboard;
end;

{ TBigClipboard }

procedure TBigClipboard.Add(Source: TPersistent);
begin
  if Source is TPicture then
    AddPicture(TPicture(Source))
  else if Source is TGraphic then
    AddGraphic(TGraphic(Source))
  else
    AddError(Source);
end;

procedure TBigClipboard.AddAsHandle(Format: Word; Value: THandle);
begin
  Open;
  try
    SetClipboardData(Format, Value);
  finally
    Close;
  end;
end;

procedure TBigClipboard.AddBuffer(Format: Word; const Buffer; Size: Integer);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(Buffer, DataPtr^, Size);
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Close;
  end;
end;

procedure TBigClipboard.AddComponent(Component: TComponent);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    MemStream.WriteComponent(Component);
    // Stolen from SetComponent, but could have used AddStream instead..
    AddBuffer(CF_COMPONENT, MemStream.Memory^, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;

procedure TBigClipboard.AddError(Source: TPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  raise EConvertError.CreateResFmt(@SAddError, [SourceName, ClassName]);
end;

procedure TBigClipboard.AddGraphic(Source: TGraphic);
var
  Data: THandle;
  Format: Word;
  Palette: HPALETTE;
begin
  Open;
  try
    Palette := 0;
    Source.SaveToClipboardFormat(Format, Data, Palette);
    SetClipboardData(Format, Data);
    if Palette <> 0 then
      SetClipboardData(CF_PALETTE, Palette);
  finally
    Close;
  end;
end;

procedure TBigClipboard.AddPicture(Source: TPicture);
var
  Data: THandle;
  Format: Word;
  Palette: HPALETTE;
begin
  Open;
  try
    Palette := 0;
    Source.SaveToClipboardFormat(Format, Data, Palette);
    SetClipboardData(Format, Data);
    if Palette <> 0 then SetClipboardData(CF_PALETTE, Palette);
  finally
    Close;
  end;
end;

procedure TBigClipboard.AddStream(Format: Integer; AStream: TStream);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, AStream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        AStream.Read(DataPtr^, AStream.Size);
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Close;
  end;
end;

procedure TBigClipboard.AddTextBuf(Buffer: PChar);
begin
  AddBuffer(CF_TEXT, Buffer^, StrLen(Buffer) + 1);
end;

constructor TBigClipboard.Create;
begin
  inherited;
end;

destructor TBigClipboard.Destroy;
begin
  inherited;
end;

procedure TBigClipboard.GetAsStream(Format: Integer; AStream: TStream);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if not Assigned(AStream) then
    Exit;
  AStream.Size := 0;
  Data := GetAsHandle(Format);
  if Data = 0 then
    Exit;
  DataPtr := GlobalLock(Data);
  try
    AStream.WriteBuffer(DataPtr^, GlobalSize(Data));
  finally
    GlobalUnlock(Data)
  end;
end;

procedure TBigClipboard.SetAsStream(Format: Integer; AStream: TStream);
begin
  // This is how the SetBlahblah in TClipboard should be implemented. :)
  Open;
  try
    Clear;
    AddStream(Format, AStream);
  finally
    Close;
  end;
end;

initialization
  // Replace Clipboard.
  //SetClipboard(TBigClipboard.Create).Free;
end.
