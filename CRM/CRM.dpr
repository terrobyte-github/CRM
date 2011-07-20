program CRM;

uses
  Forms,
  smxMain in 'smxMain.pas',
  smxFormManager in '..\Common\smxFormManager.pas',
  smxDBManager in '..\Common\smxDBManager.pas',
  smxLibManager in '..\Common\smxLibManager.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxCallBack in '..\Common\smxCallBack.pas',
  smxProcs in '..\Common\smxProcs.pas',
  smxCommonStorage in '..\Common\smxCommonStorage.pas';

{$R *.res}

begin
  Application.Initialize;

  Initialize;
  if LogIn and CreateMainForm then
    Application.Run else
    Application.Terminate;
end.
