program CRM;

uses
  Forms,
  smxMain in 'smxMain.pas',
  smxPConsts in 'smxPConsts.pas',
  smxLogIn in 'smxLogIn.pas' {fLogIn},
  smxProjects in 'smxProjects.pas' {fProjects},
  smxPassword in 'smxPassword.pas' {fPassword};

{$R *.res}

begin
  Application.Initialize;
  smxMain.Initialize;
  try
    if smxMain.LogIn {and smxMain.CreateMainForm} then
      Application.Run
    {else
      Application.Terminate};
  finally
    smxMain.Finalize;
  end;
end.
