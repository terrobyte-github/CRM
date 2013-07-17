library cCells;

uses
  SysUtils,
  Classes,
  smxLibProcs,
  smxLibTypes,
  smxCfgs in '..\Common\smxCfgs.pas',
  smxCells in '..\Common\smxCells.pas',
  smxExtCells in '..\Common\smxExtCells.pas',
  smxStdCtrls in '..\Common\smxStdCtrls.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib, [ltCellClass]);
end.
