library dBDE;

uses
  ComServ,
  smxLibProcs,
  smxLibTypes,
  smxBDEDB in '..\Common\smxBDEDB.pas';

exports
  ComServ.DllGetClassObject,
  ComServ.DllCanUnloadNow,
  ComServ.DllRegisterServer,
  ComServ.DllUnregisterServer,
  smxLibProcs.LibInfo,
  smxBDEDB.NewDatabase,
  smxBDEDB.NewDataSet,
  smxBDEDB.DatabaseCLSID;

{$R *.RES}

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
