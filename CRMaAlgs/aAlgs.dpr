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
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxCells in '..\CRMBase\smxCells.pas',
  smxCfgs in '..\CRMBase\smxCfgs.pas',
  smxWheelDBGrid in '..\CRMBase\smxWheelDBGrid.pas',
  smxCommonStorage in '..\CRMBase\smxCommonStorage.pas',
  smxLibManager in '..\CRMBase\smxLibManager.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxFormManager in '..\CRMBase\smxFormManager.pas',
  smxCellTypes in '..\CRMComn\smxCellTypes.pas',
  smxGlobalVariables in '..\CRMBase\smxGlobalVariables.pas',
  smxDBConnection in '..\CRMBase\smxDBConnection.pas',
  smxDBManager in '..\CRMBase\smxDBManager.pas',
  smxLibFuncs in '..\CRMComn\smxLibFuncs.pas',
  smxLibProcs in '..\CRMComn\smxLibProcs.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxCellFuncs in '..\CRMComn\smxCellFuncs.pas';

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
