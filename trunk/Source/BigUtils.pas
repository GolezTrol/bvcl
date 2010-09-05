{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Sep 14, 2009
*******************************************************************************
- CopyConst, CopyConstArray, FinalizeConst, FinalizeConstArray.
  Helper functions for copying/saving arrays of const.
******************************************************************************}
unit BigUtils;

interface

type
{ typeless constant parameters are actually TVarRecs. Define an alias for
  TVarRec and for array of const, to be used for storing these values }

  TConst = TVarRec;
  TConstArray = array of TConst;

{ CopyConst copies a typeless const and returns the copy. Values are
  copied and refcounts to interfaces are increased. Some types, like object,
  cannot be copied. In this case, only the pointer is copied, without further
  action. }

procedure CopyConst(const AConst: TConst; out Result: TConst);

{ CopyConstArray copies an array of const and returns the copy. }

function CopyConstArray(const Arr: array of const): TConstArray;

{ FinalizeConst should be called for each constant that is returned by
  CopyConst. RefCounts of interfaces are decremented and strings are freed. }

procedure FinalizeConst(var Item: TConst);

{ FinalizeConstArray finalizes an array of consts that was returned by
  CopyConstArray by calling FinalizeConst on each item. }

procedure FinalizeConstArray(var Arr: TConstArray);

{ StrNewW copies a PWideChar very much in the same way as StrNew copies a
  PChar. }

function StrNewW(const Str: PWideChar): PWideChar;

{ SendTextMessage is able to send text messages. It works much like SendMessage,
  but unlike SendMessage, it can send text buffers to other processed using any
  message number other than WM_SETTEXT. }

function SendTextMessage(Handle: THandle; Msg: Integer; BufferLength: Integer;
    Buffer: Pointer): Cardinal;

{ PeriodBetween returns years, months, weeks, days, hours, minutes, seconds and
  milliseconds between two TDateTimes. The AParts set allows passing any of these
  values that needs to be returned. This way, for example, a period can be
  specified in only days and hours.  }
type
  TDateTimePart = (
      dpYears, dpMonths, dpWeeks, dpDays,
      dpHours, dpMinutes, dpSeconds, dpMilliseconds);
  TDateTimeParts = set of TDateTimePart;

procedure PeriodBetween(
    ANow, AThen: TDateTime;
    AParts: TDateTimeParts;
    out AYears, AMonths, AWeeks, ADays,
        AHours, AMinutes, ASeconds, AMilliSeconds: Int64);

implementation

uses Windows, Messages, SysUtils, DateUtils;

function StrNewW(const Str: PWideChar): PWideChar;
// Like StrNew, but for PWideChar
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := (Length(Str) + 1) * SizeOf(WideChar);
    GetMem(Result, Size);
    Move(Str^, Result^, Size);
  end;
end;

procedure CopyConst(const AConst: TConst; out Result: TConst);
// Copy a constant parameter
begin
  Result := AConst;

  case AConst.VType of
    vtExtended:
      begin
        New(Result.VExtended);
        Result.VExtended^ := AConst.VExtended^;
      end;
    vtString:
      begin
        New(Result.VString);
        Result.VString^ := AConst.VString^;
      end;
    vtPChar:
      Result.VPChar := StrNew(AConst.VPChar);
    vtPWideChar:
      Result.VPWideChar := StrNewW(AConst.VPWideChar);
    vtAnsiString:
      begin
        Result.VAnsiString := nil;
        AnsiString(Result.VAnsiString) := AnsiString(AConst.VAnsiString);
      end;
    vtCurrency:
      begin
        New(Result.VCurrency);
        Result.VCurrency^ := AConst.VCurrency^;
      end;
    vtVariant:
      begin
        New(Result.VVariant);
        Result.VVariant^ := AConst.VVariant^;
      end;
    vtInterface:
      begin
        Result.VInterface := nil;
        IInterface(Result.VInterface) := IInterface(AConst.VInterface);
      end;
    vtWideString:
      begin
        Result.VWideString := nil;
        WideString(Result.VWideString) := WideString(AConst.VWideString);
      end;
    vtInt64:
      begin
        New(Result.VInt64);
        Result.VInt64^ := AConst.VInt64^;
      end;
  end;
end;

function CopyConstArray(const Arr: array of const): TConstArray;
// Copy an open array of constants
var
  i: Integer;
begin
  SetLength(Result, Length(Arr));
  for i := Low(Arr) to High(Arr) do
    CopyConst(Arr[i], Result[i]);
end;

procedure FinalizeConst(var Item: TConst);
// Finalize the a variant record that was initialized with CopyConst 
begin
  case Item.VType of
    vtExtended: Dispose(Item.VExtended);
    vtString: Dispose(Item.VString);
    vtPChar: StrDispose(Item.VPChar);
    vtPWideChar: FreeMem(Item.VPWideChar);
    vtAnsiString: AnsiString(Item.VAnsiString) := '';
    vtCurrency: Dispose(Item.VCurrency);
    vtVariant: Dispose(Item.VVariant);
    vtInterface: IInterface(Item.VInterface) := nil;
    vtWideString: WideString(Item.VWideString) := '';
    vtInt64: Dispose(Item.VInt64);
  end;
  Item.VObject := nil;
end;

procedure FinalizeConstArray(var Arr: TConstArray);
// Finalize an open array of consts
var
  i: Integer;
begin
  for i := Low(Arr) to High(Arr) do
    FinalizeConst(Arr[i]);
  Finalize(Arr);
  Arr := nil;
end;

function SendTextMessage(Handle: THandle; Msg: Integer; BufferLength: Integer;
    Buffer: Pointer): Cardinal;
var
  ProcessHandle: THandle;
  ProcessId: Cardinal;
  VirtualBuffer: Pointer;
begin
  // Get the id of process to which the handle belongs.
  GetWindowThreadProcessID(Handle, @ProcessId);

  // If the given handle belongs to the current process, don't use virtual
  // buffers, but just call SendMessage instead. This also goes for the
  // WM_SETTEXT message, because this message already is able to send text
  // due to a special exception in Windows.
  if (GetCurrentProcessId = ProcessId) or (Msg = WM_SETTEXT) then
  begin
    Result := SendMessage(Handle, Msg, BufferLength, Integer(Buffer));
    Exit;
  end;

  ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, False, ProcessId);

  if ProcessHandle = 0 then
    RaiseLastOSError;

  // Allocate a virtual buffer in the target process
  VirtualBuffer := VirtualAllocEx(ProcessHandle, nil, BufferLength,
                           MEM_COMMIT, PAGE_READWRITE);
  if VirtualBuffer = nil then
    RaiseLastOSError;

  try
    // Send a message to the handle, passing the virtual pointer as a buffer
    Result := SendMessage(Handle, Msg, BufferLength, Integer(VirtualBuffer));

    // Read the resulting value from the virtual buffer into the given buffer
    if not ReadProcessMemory(ProcessHandle, VirtualBuffer, Buffer, Result, Result) then
      RaiseLastOSError;

  finally
    VirtualFreeEx(ProcessHandle, VirtualBuffer, BufferLength, MEM_RELEASE);
  end;
end;

procedure PeriodBetween(
    ANow, AThen: TDateTime;
    AParts: TDateTimeParts;
    out AYears, AMonths, AWeeks, ADays,
        AHours, AMinutes, ASeconds, AMilliSeconds: Int64);
var
  Swap, Delta, Temp: TDateTime;

  function IsBeforeLeapDate(ADate: TDateTime): Boolean;
  begin
    Result := (EncodeDate(YearOf(Adate), 3, 1) - 1) > ADate;
  end;

begin
  // Initialize all return variables
  AYears := 0;
  AMonths := 0;
  AWeeks := 0;
  ADays := 0;
  AMinutes := 0;
  ASeconds := 0;
  AMilliSeconds := 0;

  // Swap dates if Then is before Now
  if ANow > AThen then
  begin
    Swap := ANow;
    ANow := AThen;
    AThen := Swap;
  end;

  // Remaining days/time
  Delta := AThen - ANow;

  if dpYears in AParts then
  begin
    // Extract whole years

    while Delta >= 365 do
    begin
      // Check if there is a leap date between ANow and the same date next year.
      // If so, use 366 days (365 otherwise)
      Temp := IncYear(ANow, 1);
      if (IsInLeapYear(ANow) and IsBeforeLeapDate(ANow)) or
         (IsInLeapYear(Temp) and not IsBeforeLeapDate(Temp)) then
        Temp := 366
      else
        Temp := 365;

      // If remaining delta is more than number of days in this year, decrease
      // delta and increase year and repeat.
      // Else, quit calculating years.
      if Delta > Temp then
      begin
        ANow := ANow + Temp;
        Delta := Delta - Temp;
        Inc(AYears);
      end
      else
        Break;

    end;
  end;

  if dpMonths in AParts then
  begin
    // Change the date to the first day of the month to ease calculating month
    // delta (months could accidentally be skipped otherwise).
    ANow := ANow - DayOfTheMonth(ANow);

    // Extract whole months
    Temp := DaysInMonth(ANow);
    while Delta > Temp do
    begin
      ANow := ANow + Temp;
      Delta := Delta - Temp;
      Inc(AMonths);
      Temp := DaysInMonth(ANow);
    end;
  end;

  // From here, it's simply subtracting from delta
  if dpWeeks in AParts then
  begin
    AWeeks := Trunc(Delta) div 7;
    Delta := Delta - AWeeks * 7;
  end;

  if dpDays in AParts then
  begin
    ADays := Trunc(Delta);
    Delta := Frac(Delta);
  end;

  if dpHours in AParts then
  begin
    AHours := HoursBetween(0, Delta);
    Delta := IncHour(Delta, -AHours);
  end;

  if dpMinutes in AParts then
  begin
    AMinutes := MinutesBetween(0, Delta);
    Delta := IncMinute(Delta, -AMinutes);
  end;

  if dpSeconds in AParts then
  begin
    ASeconds := SecondsBetween(0, Delta);
    Delta := IncSecond(Delta, -ASeconds);
  end;

  if dpMilliSeconds in AParts then
    AMilliSeconds := MilliSecondsBetween(0, Delta);
end;

end.
