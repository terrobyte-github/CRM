library dADO;

uses
  ComServ,
  smxLibProcs in '..\Common\smxLibProcs.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxCoADODatabase in 'smxCoADODatabase.pas',
  smxProcs in '..\Common\smxProcs.pas',
  smxCoDatabase in '..\Common\smxCoDatabase.pas',
  smxDBIntf in '..\Common\smxDBIntf.pas',
  smxADODB in '..\Common\smxADODB.pas',
  smxConsts in '..\Common\smxConsts.pas',
  smxDBClasses in '..\Common\smxDBClasses.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  LibInfo,
  NewADODatabase,
  DatabaseCLSID;

{$R *.RES}

begin
  FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
