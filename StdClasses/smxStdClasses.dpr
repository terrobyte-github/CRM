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
