library aAlgs;

uses
  ShareMem,
  Forms,
  smxAlgs in 'smxAlgs.pas',
  smxLibProcs in '..\CRMCommon\smxLibProcs.pas';

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
