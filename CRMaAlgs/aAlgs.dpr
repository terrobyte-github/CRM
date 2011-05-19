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
  smxCells in '..\CRMBase\smxCells.pas',
  smxCfgs in '..\CRMBase\smxCfgs.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxWheelDBGrid in '..\CRMBase\smxWheelDBGrid.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas',
  smxLibFuncs in '..\CRMComn\smxLibFuncs.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxGlobalStorage in '..\CRMBase\smxGlobalStorage.pas',
  smxLibManager in '..\CRMBase\smxLibManager.pas',
  smxFormManager in '..\CRMBase\smxFormManager.pas',
  smxGlobal in '..\CRMBase\smxGlobal.pas',
  smxADODB in '..\CRMBase\smxADODB.pas',
  smxField in '..\CRMBase\smxField.pas',
  smxLibProcs in '..\CRMComn\smxLibProcs.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxDBManager in '..\CRMBase\smxDBManager.pas',
  smxDBConnection in '..\CRMBase\smxDBConnection.pas';

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
  SelectOfFormSetFilterValue,
  PerformRequestFromForm,
  SelectOfFormPerformRequest,
  SelectOfFormPerformRequestFromAlgorithm,
  SelectOfFormPerformRequestFromForm;

begin
  FillInfo(1, 0, LibInit);
end.
