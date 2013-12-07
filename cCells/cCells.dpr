library cCells;

uses
  smxLibProcs,
  smxLibTypes,
  smxCells in 'smxCells.pas',
  smxExtCells in 'smxExtCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
