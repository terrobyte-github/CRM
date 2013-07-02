library dADO;

uses
  ComServ,
  smxCoADODatabase in 'smxCoADODatabase.pas',
  smxCoDatabase in '..\Common\smxCoDatabase.pas',
  smxADODB in '..\Common\smxADODB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer{,
  LibInfo,
  NewADODatabase,
  DatabaseCLSID};

{$R *.RES}

begin
  //FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
