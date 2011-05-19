library cCells;

uses
  //ShareMem,
  SysUtils,
  Classes,
  smxLibProcs in '..\CRMCommon\smxLibProcs.pas',
  smxLibTypes in '..\CRMCommon\smxLibTypes.pas',
  smxCfgs in '..\CRMBase\smxCfgs.pas',
  smxCells in '..\CRMBase\smxCells.pas',
  smxFilterCells in '..\CRMBase\smxFilterCells.pas';

{$R *.res}

exports
  LibInfo;

begin
  FillInfo(1, 0, InitLib, [ltCellClass]);
end.
