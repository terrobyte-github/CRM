program CRM;

uses
  ShareMem,
  Forms,
  smxMain in 'smxMain.pas';

{$R *.res}

begin
  Application.Initialize;

  {Initialize;
  if ConnectDatabase and CreateMainForm then
    Application.Run;}

  Initialize;
  if ConnectDatabase('CRM')
    ////and CheckUser
    and CreateMainForm then
  begin
    //CallBack[1] := Integer(Database);
    //LoadPackages;
    Application.Run;
    //UnLoadPackages;
    DisconnectDatabase;
  end;
end.
