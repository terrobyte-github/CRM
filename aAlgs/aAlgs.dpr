library aAlgs;

uses
  Forms,
  smxAlgs in 'smxAlgs.pas',
  smxLibProcs in '..\Common\smxLibProcs.pas',
  smxLibFuncs in '..\Common\smxLibFuncs.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxProcs in '..\Common\smxProcs.pas';

{$R *.res}

exports
  LibInfo,
  OpenForm,
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
  SelectAndPerformRequestF;

begin
  FillInfo(1, 0, InitLib);
end.
