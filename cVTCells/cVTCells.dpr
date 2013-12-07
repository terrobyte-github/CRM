library cVTCells;

uses
  smxLibProcs,
  smxLibTypes,
  MSAAIntf in 'MSAAIntf.pas',
  ThemeSrv in 'ThemeSrv.pas',
  TmSchema in 'TmSchema.pas',
  UxTheme in 'UxTheme.pas',
  VirtualTrees in 'VirtualTrees.pas',
  VTAccessibilityFactory in 'VTAccessibilityFactory.pas',
  smxVTCells in 'smxVTCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
