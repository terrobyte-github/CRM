library smxVTCellCls;

uses
  smxLibProcs,
  smxLibTypes,
  smxVTCells in '..\VTCellsCls\smxVTCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
