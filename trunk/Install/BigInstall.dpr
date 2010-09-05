program BigInstall;

uses
  Forms,
  BigInstallGUIMain in 'BigInstallGUIMain.pas' {Form1},
  BigInstallActions in 'BigInstallActions.pas',
  BigInstallBase in 'BigInstallBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
