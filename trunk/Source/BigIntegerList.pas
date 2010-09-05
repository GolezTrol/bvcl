{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2005-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Aug 12, 2005.
http://www.nldelphi.com/forum/showthread.php?t=21535
*******************************************************************************
 This integer list is nice, because its implementation closely resembles
 TStringList implementation. It also allows you to add both strings and objects
 to an integer. The compare methods is slightly different. By setting a compare
 method, you can sort the list any way you like and still use fast binary search
 for finding items. 
******************************************************************************}


unit BigIntegerList;

interface

uses
{$IFNDEF COMPILER_6_UP}
  RtlConsts,
{$ELSE}
  Consts,      // Delphi 5
{$ENDIF}
  Classes, SysUtils;

type
  EIntegerListError = class(Exception);
  TIntegerList = class;

  PIntItem = ^TIntItem;
  TIntItem = record
    FInteger: Integer;
    FString: string;
    FObject: TObject;
  end;

  PIntItemList = ^TIntItemList;
  TIntItemList = array[0..MaxListSize] of TIntItem;
  //TIntegerListItemCompare = function(List: TIntegerList; Index1, Index2: Integer): Integer;
  TCompareIntegers = function(AValue1, AValue2: Integer): Integer;

  TIntegerList = class(TObject)
  private
    FUpdateCount: Integer;
    FCompareMethod: TCompareIntegers;
    FList: PIntItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TCompareIntegers);
    procedure InsertItem(Index: Integer; AValue: Integer);
    procedure SetSorted(Value: Boolean);
    procedure SetCompareMethod(const Value: TCompareIntegers);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function Get(Index: Integer): Integer; virtual;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual;
    function GetObject(Index: Integer): TObject; virtual;
    function GetString(Index: Integer): string; virtual;
    procedure Put(Index: Integer; AValue: Integer); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure PutString(Index: Integer; const AString: string); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    function SlowIndexOf(AValue: Integer): Integer; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(AValue: Integer): Integer; virtual;
    function AddObject(AValue: Integer; AObject: TObject): Integer; virtual;
    function AddString(AValue: Integer; const AString: string): Integer; virtual;
    function AddItem(AValue: Integer; const AString: string; AObject: TObject): Integer; virtual;
    procedure AddItems(AItems: TIntegerList); virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(AValue: Integer): Integer; virtual;
    function Find(AValue: Integer; var Index: Integer): Boolean; virtual;
    procedure Insert(Index: Integer; AValue: Integer); virtual;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TCompareIntegers); virtual;
    property CompareMethod: TCompareIntegers read FCompareMethod write SetCompareMethod;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property Count: Integer read GetCount;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: string read GetString write PutString;
    property Integers[Index: Integer]: Integer read Get write Put; default;
  end;

//function IntegerListCompare(List: TIntegerList; Index1, Index2: Integer): Integer;
function CompareIntegers(AValue1, AValue2: Integer): Integer;

implementation

uses
  Consts;

function CompareIntegers(AValue1, AValue2: Integer): Integer;
begin
  Result := AValue1 - AValue2;
end;

{function IntegerListCompare(List: TIntegerList; Index1, Index2: Integer): Integer;
begin
  Result := List.FList^[Index1].FInteger - List.FList^[Index2].FInteger;
end;}

{ TIntegerList }

destructor TIntegerList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TIntegerList.Add(AValue: Integer): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(AValue, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0); // TODO: Change text
      end;
  InsertItem(Result, AValue);
end;

procedure TIntegerList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TIntegerList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TIntegerList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TIntegerList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TIntItem));
  Changed;
end;

procedure TIntegerList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TIntegerList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PIntItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FInteger);
  Integer(Item1^.FInteger) := Integer(Item2^.FInteger);
  Integer(Item2^.FInteger) := Temp;
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TIntegerList.Find(AValue: Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := FCompareMethod(FList^[I].FInteger, AValue);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TIntegerList.Get(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FInteger;
end;

function TIntegerList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TIntegerList.GetCount: Integer;
begin
  Result := FCount;
end;

function TIntegerList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TIntegerList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TIntegerList.IndexOf(AValue: Integer): Integer;
begin
  if not Sorted then
    Result := SlowIndexOf(AValue)
  else
    if not Find(AValue, Result) then
      Result := -1;
end;

procedure TIntegerList.Insert(Index: Integer; AValue: Integer);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, AValue);
end;

procedure TIntegerList.InsertItem(Index: Integer; AValue: Integer);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TIntItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FInteger := AValue;
  end;
  Inc(FCount);
  Changed;
end;

procedure TIntegerList.Put(Index: Integer; AValue: Integer);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FInteger := AValue;
  Changed;
end;

procedure TIntegerList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TIntegerList.QuickSort(L, R: Integer; SCompare: TCompareIntegers);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(FList^[I].FInteger, FList^[P].FInteger) < 0 do Inc(I);
      while SCompare(FList^[J].FInteger, FList^[P].FInteger) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TIntegerList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TIntItem));
  FCapacity := NewCapacity;
end;

procedure TIntegerList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TIntegerList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

procedure TIntegerList.Sort;
begin
  CustomSort(FCompareMethod);
end;

procedure TIntegerList.CustomSort(Compare: TCompareIntegers);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

procedure TIntegerList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;

begin
  raise EIntegerListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TIntegerList.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TIntegerList.PutString(Index: Integer; const AString: string);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FString := AString;
  Changed;
end;

function TIntegerList.SlowIndexOf(AValue: Integer): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if Get(Result) = AValue then
      Exit;
  Result := -1;
end;

function TIntegerList.GetString(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

procedure TIntegerList.SetCompareMethod(const Value: TCompareIntegers);
begin
  if @FCompareMethod <> @Value then
  begin
    if @Value <> nil then
      FCompareMethod := Value
    else
      FCompareMethod := CompareIntegers;
    if Sorted then
      Sort;
  end;
end;

constructor TIntegerList.Create;
begin
  FCompareMethod := CompareIntegers;
end;

function TIntegerList.AddItem(AValue: Integer; const AString: string;
  AObject: TObject): Integer;
begin
  Result := Add(AValue);
  PutString(Result, AString);
  PutObject(Result, AObject);
end;

function TIntegerList.AddObject(AValue: Integer;
  AObject: TObject): Integer;
begin
  Result := Add(AValue);
  PutObject(Result, AObject);
end;

function TIntegerList.AddString(AValue: Integer;
  const AString: string): Integer;
begin
  Result := Add(AValue);
  PutString(Result, AString);
end;

procedure TIntegerList.AddItems(AItems: TIntegerList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AItems.Count - 1 do
      AddItem(AItems[I], AItems.Strings[i], AItems.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TIntegerList.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TIntegerList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

end.
