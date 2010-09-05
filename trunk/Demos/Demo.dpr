program Demo;

uses
  Forms,
  fControls in 'fControls.pas' {Form1},
  BigUtils in '..\Source\BigUtils.pas',
  BigProgressFrm in '..\Source\BigProgressFrm.pas' {BigProgressForm},
  BigThreadProgress in '..\Source\BigThreadProgress.pas',
  BigAltGrCrashWorkaround in '..\Source\BigAltGrCrashWorkaround.pas',
  BigGraphics in '..\Source\BigGraphics.pas',
  BigPool in '..\Source\BigPool.pas',
  AgentObjects_TLB in 'C:\Program Files\Borland\Delphi7\Imports\AgentObjects_TLB.pas',
  BigMRU in '..\Source\BigMRU.pas',
  BigMRURegistryStorage in '..\Source\BigMRURegistryStorage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
