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
    if smxMain.LogIn then
      try
        if smxMain.Start then
          try
            Application.Run;
          finally
            smxMain.Finish;
          end;
      finally
        smxMain.LogOut;
      end;
  finally
    smxMain.Finalize;
  end;
end.
