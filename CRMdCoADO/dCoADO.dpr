library dCoADO;

uses
  ShareMem,
  ComServ,
  smxCoADODatabase in 'smxCoADODatabase.pas',
  smxCoDatabase in '..\CRMBase\smxCoDatabase.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxADODB in '..\CRMBase\smxADODB.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxField in '..\CRMBase\smxField.pas',
  smxLibProcs in '..\CRMComn\smxLibProcs.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas',
  smxProcs in '..\CRMComn\smxProcs.pas';

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
