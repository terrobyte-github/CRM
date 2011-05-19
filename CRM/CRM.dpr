program CRM;

uses
  ShareMem,
  Forms,
  smxMain in 'smxMain.pas';

{$R *.res}

begin
  Application.Initialize;

  Initialize;
  if LogIn and CreateMainForm then
    Application.Run else
    Application.Terminate;
end.
