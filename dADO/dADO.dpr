library dADO;

uses
  ComServ,
  smxLibProcs,
  smxLibTypes,
  smxADODB in 'smxADODB.pas';

exports
  ComServ.DllGetClassObject,
  ComServ.DllCanUnloadNow,
  ComServ.DllRegisterServer,
  ComServ.DllUnregisterServer,
  smxLibProcs.LibInfo{,
  smxADODB.NewDatabase,
  smxADODB.NewDataSet,
  smxADODB.DatabaseCLSID};

{$R *.RES}

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
