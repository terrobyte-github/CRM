{**************************************}
{                                      }
{            SalesMan v1.0             }
{         ADO drivers library          }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

library smxADODrivers;

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
