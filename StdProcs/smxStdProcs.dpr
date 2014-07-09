library smxStdProcs;

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
  smxAlgs.ShowForm,
  smxAlgs.ShowUniForm,
  smxAlgs.ShowModalForm,
  smxAlgs.CloseModalForm,
  smxAlgs.CloseOkForm,
  smxAlgs.CloseCancelForm,
  smxAlgs.ShowMessageForm,
  smxAlgs.ChangeRowPropsGrid,
  smxAlgs.ClickColumnPropsGrid;
  
begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib);
end.
