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
