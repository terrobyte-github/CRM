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
  LibInfo;

begin
  FillInfo(1, 0, InitLib, [ltCellClass]);
end.
