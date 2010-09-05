{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2009 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

*******************************************************************************
Host control for various minigames. Concept will probably never be finished ;)
******************************************************************************}

unit BigMiniGames;

interface

uses
  Controls, Graphics;

type
  TBigMiniGameSize = (gsSmall, gsMedium, gsLarge);
  TBigMiniGameDifficulty = (gdEasy, gdMedium, gdHard);

  TBigMiniGameHost = class(TCustomControl)
  private
  public
    //property Difficulty: TBigMiniGameDifficulty read FDifficulty write SetDifficulty;
    //property Size: TBigMiniGameSize read FSize write SetSize;
    // Play
    // Save
    // Stop
    // Difficulty (easy, medium, hard, custom)
    // Size (small, medium, large, custom)
  end;

  TBigMiniGame = class
  public
    constructor Create; virtual; abstract;

    procedure Paint; virtual; abstract;
  published
  end;

{  TBigMineSweeper = class(TBigMiniGame)
  private
    //FFields: array of TButton;
    FWidth: Integer;
    FHeight: Integer;
  protected
    procedure SetFieldSize(AWidth, AHeight: Integer);
    function IsOpen(X, Y: Integer): Boolean;
    function IsBomb(X, Y: Integer): Boolean;

    procedure Paint; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;}

implementation

end.
