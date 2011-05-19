library aAlgs;

uses
  ShareMem,
  Forms,
  smxAlgs in 'smxAlgs.pas',
  smxClasses in '..\CRMBase\smxClasses.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxTypes in '..\CRMComn\smxTypes.pas',
  smxFuncs in '..\CRMComn\smxFuncs.pas',
  smxClassFuncs in '..\CRMComn\smxClassFuncs.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxLibFuncs in '..\CRMComn\smxLibFuncs.pas',
  smxLibProcs in '..\CRMComn\smxLibProcs.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxClassTypes in '..\CRMComn\smxClassTypes.pas';

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
  FillInfo(1, 0, LibInit);
end.
