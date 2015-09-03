{**************************************}
{                                      }
{            SalesMan v1.0             }
{      Standard resources library      }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

library smxStdResources;

uses
  smxLibProcs,
  smxLibTypes;

{$R *.res}
{$R pic.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltResource]);
end.
