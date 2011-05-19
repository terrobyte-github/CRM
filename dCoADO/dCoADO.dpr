library dCoADO;

uses
  //ShareMem,
  ComServ,
  smxLibProcs in '..\CRMCommon\smxLibProcs.pas',
  smxLibTypes in '..\CRMCommon\smxLibTypes.pas',
  smxCoADODatabase in 'smxCoADODatabase.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  LibInfo,
  DatabaseCLSID;

{$R *.RES}

begin
  FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
