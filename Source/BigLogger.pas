{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

*******************************************************************************
Work in progress...

******************************************************************************}
unit BigLogger;

interface

uses
  SysUtils, DateUtils;

type
  TBigAbstractProfiler = class
  private
    FLevel: Integer;
  public
    procedure Log(const AMessage: String); overload; virtual;
    procedure Log(const ANumber: Integer); overload; virtual;
    procedure Log(const AName: String; AValue: Integer); overload;
    procedure Log(const AName: String; const AValue: Extended); overload;
    procedure Log(const AName: String; AValue: TDateTime); overload;
    procedure Log(const AName: String; AValue: Boolean); overload;
    procedure Log(const AName: String; const AValue: String); overload;
    procedure Log(E: Exception); overload;
    procedure Enter(const AFunction: String; const AComment: String = '');
    procedure Exit(const AFunction: String; const AComment: String = '');
  end;

implementation

{ TBigAbstractProfiler }

procedure TBigAbstractProfiler.Enter(const AFunction, AComment: String);
begin
Date
end;

procedure TBigAbstractProfiler.Exit(const AFunction, AComment: String);
begin

end;

procedure TBigAbstractProfiler.Log(const AName: String;
  const AValue: Extended);
begin

end;

procedure TBigAbstractProfiler.Log(const AName: String; AValue: Integer);
begin

end;

procedure TBigAbstractProfiler.Log(const ANumber: Integer);
begin

end;

procedure TBigAbstractProfiler.Log(const AName, AValue: String);
begin

end;

procedure TBigAbstractProfiler.Log(E: Exception);
begin

end;

procedure TBigAbstractProfiler.Log(const AName: String; AValue: TDateTime);
begin

end;

procedure TBigAbstractProfiler.Log(const AName: String; AValue: Boolean);
begin

end;

procedure TBigAbstractProfiler.Log(const AMessage: String);
begin

end;

end.
