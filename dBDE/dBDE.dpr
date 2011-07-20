library dBDE;

uses
  ComServ,
  smxLibProcs in '..\Common\smxLibProcs.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxProcs in '..\Common\smxProcs.pas',
  smxCoDatabase in '..\Common\smxCoDatabase.pas',
  smxDBIntf in '..\Common\smxDBIntf.pas',
  smxConsts in '..\Common\smxConsts.pas',
  smxField in '..\Common\smxField.pas',
  smxBDEDB in '..\Common\smxBDEDB.pas',
  smxCoBDEDatabase in 'smxCoBDEDatabase.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  LibInfo,
  NewBDEDatabase,
  DatabaseCLSID;

{$R *.RES}

begin
  FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
