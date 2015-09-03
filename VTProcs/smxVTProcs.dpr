{**************************************}
{                                      }
{            SalesMan v1.0             }
{    VirtualTree procedures library    }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

library smxVTProcs;

uses
  smxLibProcs,
  smxLibTypes,
  smxVTAlgs in 'smxVTAlgs.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo,
  smxVTAlgs.ShowObjectProps;

begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib);
end.
 