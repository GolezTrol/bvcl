unit BigInstallBase;

interface

type
  IInstallAction = interface
    procedure DoAction;
    procedure UndoAction;
  end;

implementation

end.
