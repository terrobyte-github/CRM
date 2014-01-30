library aVTAlgs;

uses
  smxLibProcs,
  smxLibTypes,
  smxVTAlgs in 'smxVTAlgs.pas',
  VirtualTrees in '..\VT\VirtualTrees.pas',
  MSAAIntf in '..\VT\MSAAIntf.pas',
  ThemeSrv in '..\VT\ThemeSrv.pas',
  UxTheme in '..\VT\UxTheme.pas',
  TmSchema in '..\VT\TmSchema.pas',
  VTAccessibilityFactory in '..\VT\VTAccessibilityFactory.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo,
  smxVTAlgs.GetPropsTree,
  smxVTAlgs.SetPropsTree,
  smxVTAlgs.AfterEditPropTree,
  smxVTAlgs.BeforeEditPropTree;

begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib);
end.
 