{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2003-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Aug 3, 2003.
http://www.nldelphi.com/forum/showthread.php?t=8221
*******************************************************************************
 TBigOfflineClipboard allows data from the clipboard to be stored in a clip
 object or even to a file. This way, clipboard contents can be saved,
 overwritten and later restored to the clipboard.
*******************************************************************************
ToDo:
- TBigClip.LoadFromStream: Save data for in case an exception occurs
- Find out what WM_RENDERFORMAT and WM_RENDERALLFORMATS is about. (Update data
    before saving?)
Currently implementing:
- TBigCustomClipCollection.DoClipboardChange - Duplicates / Max entries
******************************************************************************}

unit BigOfflineClipboard;

interface

uses
  Windows, Messages, Graphics, Classes, SysUtils, Clipbrd,
  BigClipbrd, BigClipboardMonitor;

const
  // Do not localize these constants!!

  // Default file extentions
  BigClipFileExt = 'bcf';            // BigClipFile
  BigClipCollectionFileExt = 'bcc';  // BigClipCollection

  BigClipDataHeader = 'BCD';         // Big Clipboard Data
  BigClipHeader = 'GTBCF';           // GolezTrol Big Clip File
  BigClipVersion = 1;                // Clip Stream Format
  BigClipCollectionHeader = 'GTBCC'; // GolezTrol Big Clip Collection
  BigClipCollectionVersion = 1;      // Internal format version of Clip Coll. file

  MaxFormatLength = 255;             // max length of clipboard format name

type
  { forward declarations }
  TBigClip = class;

  { TBigClipboardData: Record to keep data in specific format }
  TBigClipboardData = record
    Format: Integer;
    Data: Pointer;
    Size: Integer;
  end;
  PBigClipboardData = ^TBigClipboardData;

  TClipDuplicates = (cdIgnore, cdAllow, cdMoveUp);

  { General exception }
  EBigClipboardError = class(Exception);

  { WM_DRAWCLIPBOARD Message -> On Change event }
  TClipboardChangeEvent = TNotifyEvent;
  TClipboardCanCollectEvent = procedure(Sender: TObject; var Allow: Boolean) of object;

  { TBigClip }
  TBigClip = class(TPersistent)
  private
    FOfflineData: array of TBigClipboardData;
  protected
    function InternalLoadFormatFromStream(var ClipData: TBigClipboardData; AStream: TStream): Boolean;
    procedure InternalSaveFormatToStream(Index: Integer; AStream: TStream);

    function IsSameFormatAs(Source: TBigClip; CompareSizeToo: Boolean): Boolean; virtual;
    function IsSameAs(Source: TBigClip): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    procedure LoadFromClipboard; virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToClipboard; virtual;
    procedure SaveToStream(AStream: TStream); virtual;
  end;

  TBigCustomClipCollection = class(TBigCustomClipboardMonitor)
  private
    FList: TList;
    FOnCanCollect: TClipboardCanCollectEvent;
    FClipDuplicates: TClipDuplicates;
    FIgnoreNext: Boolean;

    function GetCount: Integer;
    procedure SetClipDuplicates(const Value: TClipDuplicates);
  protected
    function Add(var Clip: TBigClip): Integer;
    procedure FilterDuplicates; virtual;
    procedure DoClipboardChange; override;
    property OnCanCollect: TClipboardCanCollectEvent
      read FOnCanCollect write FOnCanCollect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure CopyToClipboard(Index: Integer);
    procedure CopyClip(var Clip: TBigClip; Index: Integer);
    property Count: Integer read GetCount;
    procedure Delete(Index: Integer);
    property Duplicates: TClipDuplicates
      read FClipDuplicates write SetClipDuplicates;

    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    property OnClipboardChange;
  end;

  TBigClipCollection = class(TBigCustomClipCollection)
  published
    property OnCanCollect;
    property Duplicates;
  end;

implementation

uses
  Forms, Contnrs,
  BigClipRes;

{ TBigClip }

procedure TBigClip.Assign(Source: TPersistent);
var
  i: Integer;
  Src: TBigClip;
begin
  if Source is TBigClip then
  begin
    Src := TBigClip(Source);
    Clear;
    SetLength(FOfflineData, Length(Src.FOfflineData));
    for i := 0 to High(Src.FOfflineData) do
    begin
      FOfflineData[i].Format := Src.FOfflineData[i].Format;
      FOfflineData[i].Size := Src.FOfflineData[i].Size;
      GetMem(FOfflineData[i].Data, FOfflineData[i].Size);
      CopyMemory(FOfflineData[i].Data, Src.FOfflineData[i].Data, FOfflineData[i].Size);
    end;
  end
  else
    inherited;
end;

procedure TBigClip.Clear;
var
  i: Integer;
begin
  for i := 0 to High(FOfflineData) do
    FreeMem(FOfflineData[i].Data, FOfflineData[i].Size);
  SetLength(FOfflineData, 0);
end;

constructor TBigClip.Create;
begin
  SetLength(FOfflineData, 0);
end;

destructor TBigClip.Destroy;
begin
  Clear;
  inherited;
end;

function TBigClip.InternalLoadFormatFromStream(
  var ClipData: TBigClipboardData; AStream: TStream): Boolean;
var
  Reader: TReader;
begin
  Result := False;
  try
    Reader := TReader.Create(AStream, 4096);
    try
      // First check the header
      if not (Reader.ReadString = BigClipDataHeader) then
        Exit;
      // ----------------------------------------------------------------------
      // As you see below, it's easy to add a version for the format streams
      // themselves, but I figured it wouldn't matter. A version for the
      // entire clip will do.
      // ----------------------------------------------------------------------
      // Next, check the version
      //case Reader.ReadInteger of
      //  1: begin
          // :: Version 1: <IsPredef><If IsPredef<formatname>else<Format>><size><data>
          if Reader.ReadBoolean then
            ClipData.Format := Reader.ReadInteger
          else
            ClipData.Format := RegisterClipboardFormat(PChar(Reader.ReadString));

          ClipData.Size := Reader.ReadInteger;
          GetMem(ClipData.Data, ClipData.Size);
          try
            Reader.Read(ClipData.Data^, ClipData.Size);
          except
            FreeMem(ClipData.Data, ClipData.Size);
            raise
          end;
        //end;
      //else
      //  raise EBigClipboardError.CreateRes(@SInvalidVersion);
      //end;
    finally
      Reader.Free;
    end;
    Result := True;
  except
    ClipData.Format := 0;
    ClipData.Data := nil;
    ClipData.Size := 0;
  end;
end;

procedure TBigClip.InternalSaveFormatToStream(Index: Integer;
  AStream: TStream);
// Writes OfflineData[Index] to stream in the format:
// <header><version><formatname><size><data stream>
var
  IsPredef: Boolean;
  Buffer: PChar;
  Writer: TWriter;
begin
  GetMem(Buffer, MaxFormatLength);
  try
    Writer := TWriter.Create(AStream, 4096);
    with FOfflineData[Index] do
    try
      Writer.WriteString(bigClipDataHeader);
      //Writer.WriteInteger(bigClipVersion);  // Skip the FormatStream version
      IsPredef := GetClipboardFormatName(Format, Buffer, MaxFormatLength) = 0;
      Writer.WriteBoolean(IsPredef);
      if IsPredef then
        Writer.WriteInteger(Format)
      else
        Writer.WriteString(string(Buffer));
      Writer.WriteInteger(Size);
      Writer.Write(Data^, Size);
    finally
      Writer.Free;
    end;
  finally
    FreeMem(Buffer, MaxFormatLength);
  end;
end;

function TBigClip.IsSameAs(Source: TBigClip): Boolean;
var
  i, j: Integer;
begin
  Result := IsSameFormatAs(Source, True);

  if not Result then
    Exit;

  // It might seem inefficient to do the same nested loop as in IsSameFormatAs,
  // but usually there aren't too many formats available, and comparing blocks
  // of memory takes lots of time as well, so it's better to check is all
  // formats are available before checking the contents of each blok.
  // An optimization might be to sort the blocks smallest size first.
  for i := 0 to Length(FOfflineData)-1 do
    for j := 0 to Length(FOfflineData)-1 do
      if FOfflineData[i].Format = Source.FOfflineData[j].Format then
        Result := Result and CompareMem(FOfflineData[i].Data, FOfflineData[j].Data, FOfflineData[i].Size);
end;

function TBigClip.IsSameFormatAs(Source: TBigClip;
  CompareSizeToo: Boolean): Boolean;
// Compare based on format and -optionally- on size for each format.
// The lists are not sorted by format, so we need a nested loop. This is no
// problem; usually there will be only a handful of items in the lists.
var
  i, j: Integer;
begin
  Result := Length(FOfflineData) = Length(Source.FOfflineData);
  if not Result then
    Exit;
  for i := 0 to Length(FOfflineData)-1 do
  begin
    Result := False;
    for j := 0 to Length(FOfflineData)-1 do
      if FOfflineData[i].Format = Source.FOfflineData[j].Format then
      begin
        // Check size, if needed.
        Result := (not CompareSizeToo) or (FOfflineData[i].Size = Source.FOfflineData[j].Size);
        Break;
      end;

    // If none of the formats in FOfflineData matches Source.FOfflineData,
    // the clips differ.
    if not Result then
      Break;
  end;
end;

procedure TBigClip.LoadFromClipboard;
var
  i: Integer;
  ClipData: THandle;
  ClipDataPtr: Pointer;
begin
  for i := 0 to High(FOfflineData) do
    FreeMem(FOfflineData[i].Data, FOfflineData[i].Size);

  SetLength(FOfflineData, BigClipboard.FormatCount);

  BigClipboard.Open;
  try
    for i := 0 to BigClipboard.FormatCount - 1 do
    begin
      ClipDataPtr := nil;
      ClipData := BigClipboard.GetAsHandle(BigClipboard.Formats[i]);
      if ClipData <> 0 then
        ClipDataPtr := GlobalLock(ClipData);

      if ClipDataPtr = nil then
      begin
        with FOfflineData[i] do
        begin
          Format := BigClipboard.Formats[i];
          Size := 0;
          Data := nil;
        end;
      end
      else
      try
        with FOfflineData[i] do
        begin
          Format := BigClipboard.Formats[i];

          if GlobalFlags(ClipData) = GMEM_INVALID_HANDLE then
            raise EBigClipboardError.CreateRes(@SInvalidHandle);

          Size := GlobalSize(ClipData);
          GetMem(Data, Size);
          CopyMemory(Data, ClipDataPtr, Size);
        end;
      finally
        GlobalUnlock(ClipData)
      end;
    end;
  finally
    BigClipboard.Close;
  end;
end;

procedure TBigClip.LoadFromStream(AStream: TStream);
var
  Reader: TReader;
  NewData: TBigClipboardData;
  Index: Integer;
begin
  // TODO -cPotential issue: Backup old data first?
  Clear;
  try
    Reader := TReader.Create(AStream, 4096);
    try
      Reader.Position := 0;
      // First check the header
      if not (Reader.ReadString = bigClipHeader) then
        Exit;
      case Reader.ReadInteger of  // Version
        1: begin
          Reader.ReadListBegin;
          while not Reader.EndOfList do
          begin
            Reader.FlushBuffer;
            SetLength(FOfflineData, High(FOfflineData) + 2);
            InternalLoadFormatFromStream(NewData, AStream);
            Index := High(FOfflineData);
            FOfflineData[Index].Format := NewData.Format;
            FOfflineData[Index].Data := NewData.Data;
            FOfflineData[Index].Size := NewData.Size;
            Reader.FlushBuffer;
          end;
          Reader.ReadListEnd;
        end;
      else
        raise EBigClipboardError.CreateRes(@SInvalidVersion)
      end;
    finally
      Reader.Free;
    end;
  except
    // Restore old data?
  end;
end;

procedure TBigClip.SaveToClipboard;
var
  i: Integer;
begin
  BigClipboard.Clear;
  for i := 0 to High(FOfflineData)do
  with FOfflineData[i] do
    BigClipboard.AddBuffer(Format, Data^, Size);
end;

procedure TBigClip.SaveToStream(AStream: TStream);
var
  i: Integer;
  Writer: TWriter;
  Mem: TMemoryStream;
begin
  Writer := TWriter.Create(AStream, 4096);
  try
    Writer.Position := 0;
    Writer.WriteString(bigClipHeader);
    Writer.WriteInteger(bigClipVersion);
    Writer.WriteListBegin;
    Writer.FlushBuffer;
    Mem := TMemoryStream.Create;
    try
      for i := 0 to High(FOfflineData) do
      begin
        Mem.Clear;
        InternalSaveFormatToStream(i, Mem);
        AStream.CopyFrom(Mem, 0);
      end;
    finally
      Mem.Free;
    end;
    Writer.WriteListEnd;
  finally
    Writer.Free;
  end;
end;

{ TBigCustomClipCollection }

function TBigCustomClipCollection.Add(var Clip: TBigClip): Integer;
// Adds Clip to the list of clips. Returns the index of the added item.
// if Duplicates = cdAllow then Clip is inserted at position 0 (zero).
// if Duplicates <> cdAllow and a similar clip already exists, then Clip is
// freed and the existing item is assigned to the Clip variable.
// If Duplicates = cdMoveUp, result is typically 0 (zero).
// If Dublicates = cdIgnore, then result is the index of Clip. If a similar
// clip existed, this result will be the index of that clip, else result will
// be 0 (zero);
// Anyhow, Self wil own the clip, so it must not be freed by the program!
var
  Exists: Boolean;
  i: Integer;
begin
  Result := 0;
  if Duplicates = cdAllow then
    FList.Insert(0, Clip)
  else
  begin
    Exists := False;
    for i := 0 to FList.Count - 1 do
      if TBigClip(FList[i]).IsSameAs(Clip) then
      begin
        Exists := True;
        if Duplicates = cdMoveUp then
          FList.Move(i, 0)
        else
          Result := i;
        Break;
      end;
    if Exists then
    begin
      Clip.Free;
      Clip := TBigClip(FList[Result]);
    end
    else
      FList.Insert(0, Clip)
  end
end;

procedure TBigCustomClipCollection.Clear;
begin
  while FList.Count > 0 do
    Delete(0);
end;

procedure TBigCustomClipCollection.CopyClip(var Clip: TBigClip;
  Index: Integer);
begin
  Clip.Assign(FList[Index]);
end;

procedure TBigCustomClipCollection.CopyToClipboard(Index: Integer);
begin
  try
    FIgnoreNext := True;
    TBigClip(FList[Index]).SaveToClipboard;
  finally
    FIgnoreNext := False;
  end;
end;

constructor TBigCustomClipCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FList := TList.Create;
  FIgnoreNext := False;
end;

procedure TBigCustomClipCollection.Delete(Index: Integer);
begin
  TBigClip(FList[Index]).Free;
  FList.Delete(Index);
end;

destructor TBigCustomClipCollection.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TBigCustomClipCollection.DoClipboardChange;
var
  Allow, Exists: Boolean;
  NewEntry: TBigClip;
  i: Integer;
begin
  // This change should be ignored
  if FIgnoreNext then
    Exit;

  // If any content is on the clipboard, collect it
  if bigClipBoard.FormatCount > 0 then
  begin
    // Allowed to collect?
    Allow := True;
    if Assigned(FOnCanCollect) then
      FOnCanCollect(Self, Allow);

    if Allow then
    begin
      NewEntry := TBigClip.Create;

      NewEntry.LoadFromClipboard;

      if Duplicates = cdAllow then
      begin
        // Add on top of the list
        FList.Insert(0, NewEntry)
      end
      else
      begin
        // Duplicates are not allowed. Check if this data already exists.
        Exists := False;

        for i := 0 to FList.Count - 1 do
          if TBigClip(FList[i]).IsSameAs(NewEntry) then
          begin
            Exists := True;

            if Duplicates = cdMoveUp then
              FList.Move(i, 0);
            Break;
          end;

        if not Exists then
          FList.Insert(0, NewEntry);
      end
    end;
  end;

  inherited;
end;

procedure TBigCustomClipCollection.FilterDuplicates;
var
  i, j: Integer;
begin
  i := 0;

  while i < FList.Count - 1 do
  begin
    j := i + 1;
    while j < FList.Count do
      if TBigClip(FList[j]).IsSameAs(TBigClip(FList[i])) then
        Delete(j)
      else
        Inc(j);
    Inc(i);
  end;
end;

function TBigCustomClipCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TBigCustomClipCollection.LoadFromStream(AStream: TStream);
var
  Reader: TReader;
  NewClip: TBigClip;
begin
  // TODO -cPotential issue: Backup old data first?
  Clear;

  try
    Reader := TReader.Create(AStream, 4096);
    try
      Reader.Position := 0;

      // First check the header
      if not (Reader.ReadString = bigClipCollectionHeader) then
        raise EBigClipboardError.CreateRes(@SInvalidHeader);

      case Reader.ReadInteger of  // Version
        1: begin
          Duplicates := TClipDuplicates(Reader.ReadInteger);
          Reader.ReadListBegin;
          while not Reader.EndOfList do
          begin
            Reader.FlushBuffer;
            NewClip := TBigClip.Create;
            NewClip.LoadFromStream(AStream);
            Add(NewClip);
            Reader.FlushBuffer;
          end;
          Reader.ReadListEnd;
        end;
      else
        raise EBigClipboardError.CreateRes(@SInvalidVersion)
      end;
    finally
      Reader.Free;
    end;
  except
    // Restore old data?
  end;
end;

procedure TBigCustomClipCollection.SaveToStream(AStream: TStream);
var
  i: Integer;
  Writer: TWriter;
  Mem: TMemoryStream;
begin
  Writer := TWriter.Create(AStream, 4096);
  try
    Writer.Position := 0;
    Writer.WriteString(bigClipCollectionHeader);
    Writer.WriteInteger(bigClipCollectionVersion);
    Writer.WriteInteger(Integer(Duplicates));
    Writer.WriteListBegin;
    Writer.FlushBuffer;
    Mem := TMemoryStream.Create;
    try
      for i := 0 to FList.Count-1 do
      begin
        Mem.Clear;
        TBigClip(FList[i]).SaveToStream(Mem);
        AStream.CopyFrom(Mem, 0);
      end;
    finally
      Mem.Free;
    end;
    Writer.WriteListEnd;
  finally
    Writer.Free;
  end;
end;

procedure TBigCustomClipCollection.SetClipDuplicates(
  const Value: TClipDuplicates);
begin
  if FClipDuplicates = Value then
    Exit;

  FClipDuplicates := Value;
  if FClipDuplicates <> cdAllow then
    FilterDuplicates;
end;

end.
