library smxVTPrcAlg;

uses
  smxLibProcs,
  smxLibTypes,
  smxVTAlgs in 'smxVTAlgs.pas';

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
 