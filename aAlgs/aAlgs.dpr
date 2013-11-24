library aAlgs;

uses
  smxLibProcs,
  smxLibTypes,
  smxAlgs in 'smxAlgs.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo,
  smxAlgs.ApplyForm,
  smxAlgs.CancelForm,
  smxAlgs.PrepareForm,
  smxAlgs.RefreshForm,
  smxAlgs.ShowModalForm,
  smxAlgs.CloseModalForm,
  smxAlgs.GetPropsTree,
  smxAlgs.SetPropsTree,
  smxAlgs.CloseOkForm,
  smxAlgs.CloseCancelForm,
  smxAlgs.ShowMessageForm,
  smxAlgs.ChangeRowPropsGrid,
  smxAlgs.ClickColumnPropsGrid;
  
begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib);
end.
