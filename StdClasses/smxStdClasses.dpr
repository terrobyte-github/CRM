{**************************************}
{                                      }
{            SalesMan v1.0             }
{       Standard classes library       }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

library smxStdClasses;

uses
  smxLibProcs,
  smxLibTypes,
  smxStdCells in 'smxStdCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
