library dBDE;

uses
  ComServ,
  smxLibProcs,
  smxLibTypes,
  smxBDEDB in '..\Common\smxBDEDB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  LibInfo,
  NewDatabase,
  NewDataSet,
  DatabaseCLSID;

{$R *.RES}

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
