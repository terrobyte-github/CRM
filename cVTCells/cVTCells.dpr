library cVTCells;

uses
  smxLibProcs,
  smxLibTypes,
  MSAAIntf in '..\VT\MSAAIntf.pas',
  ThemeSrv in '..\VT\ThemeSrv.pas',
  TmSchema in '..\VT\TmSchema.pas',
  UxTheme in '..\VT\UxTheme.pas',
  VirtualTrees in '..\VT\VirtualTrees.pas',
  VTAccessibilityFactory in '..\VT\VTAccessibilityFactory.pas',
  smxVTCells in 'smxVTCells.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltCellClass]);
end.
