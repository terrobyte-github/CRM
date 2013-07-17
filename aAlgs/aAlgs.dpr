library aAlgs;

uses
  smxLibProcs,
  smxLibTypes,
  smxAlgs in 'smxAlgs.pas';

{$R *.res}

exports
  smxLibProcs.LibInfo,
  smxAlgs.PrepareForm,
  smxAlgs.OpenModalForm;
  {OpenForm,
  OpenFormByEventID,
  OpenFormByProblemID,
  CloseForm,
  RefreshForm,
  ApplyForm,
  SelectRecord,
  UnSelectRecord,
  ChangeFilterValue,
  ChangeStateForm,
  SelectAndPerformRequestA,
  SelectAndPerformRequestF;}

begin
  smxLibProcs.FillInfo(1, 0, smxLibProcs.InitLib);
end.
