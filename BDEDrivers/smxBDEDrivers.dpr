{**************************************}
{                                      }
{            SalesMan v1.0             }
{         BDE drivers library          }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

library smxBDEDrivers;

uses
  ComServ,
  smxLibProcs,
  smxLibTypes,
  smxBDEDB in 'smxBDEDB.pas';

exports
  ComServ.DllGetClassObject,
  ComServ.DllCanUnloadNow,
  ComServ.DllRegisterServer,
  ComServ.DllUnregisterServer,
  smxLibProcs.LibInfo,
  smxBDEDB.NewDatabase,
  smxBDEDB.NewQuery,
  smxBDEDB.NewProcedure,
  smxBDEDB.DatabaseCLSID;

{$R *.RES}

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltDatabaseIntf, ltCellClass]);
end.
