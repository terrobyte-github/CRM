{**************************************}
{                                      }
{            SalesMan v1.0             }
{     VirtualTree classes library      }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

library smxVTClasses;

uses
  smxLibProcs,
  smxLibTypes,
  smxVTCells in 'smxVTCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
