library cCells;

uses
  smxLibProcs,
  smxLibTypes,
  smxCells in 'smxCells.pas',
  smxExtCells in 'smxExtCells.pas',
  smxStdCtrls in 'smxStdCtrls.pas',
  VirtualTrees in '..\VT\VirtualTrees.pas',
  MSAAIntf in '..\VT\MSAAIntf.pas',
  ThemeSrv in '..\VT\ThemeSrv.pas',
  UxTheme in '..\VT\UxTheme.pas',
  TmSchema in '..\VT\TmSchema.pas',
  VTAccessibilityFactory in '..\VT\VTAccessibilityFactory.pas',
  smxVTCells in 'smxVTCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
