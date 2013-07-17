library cCells;

uses
  SysUtils,
  Classes,
  smxLibProcs,
  smxLibTypes,
  smxCfgs in 'smxCfgs.pas',
  smxCells in 'smxCells.pas',
  smxExtCells in 'smxExtCells.pas',
  smxStdCtrls in 'smxStdCtrls.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib, [ltCellClass]);
end.
