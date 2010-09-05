unit BigInstallActions;

interface

uses
  BigInstallBase;

type
  TInstallAction = class(TInterfacedObject)
    property Caption: string read GetCaption write SetCaption;
    procedure DoAction; virtual;
    procedure UndoAction; virtual;
  end;



implementation

end.
