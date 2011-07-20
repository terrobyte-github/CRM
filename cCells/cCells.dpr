library cCells;

uses
  //ShareMem,
  SysUtils,
  Classes,
  smxLibProcs in '..\Common\smxLibProcs.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxCfgs in '..\Common\smxCfgs.pas',
  smxCells in '..\Common\smxCells.pas',
  smxFilterCells in '..\Common\smxFilterCells.pas';

{$R *.res}

exports
  LibInfo;

begin
  FillInfo(1, 0, InitLib, [ltCellClass]);
end.
