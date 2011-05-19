program Conf;

uses
  Forms,
  smxConf in 'smxConf.pas' {frmConf},
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxADODB in '..\CRMBase\smxADODB.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxClasses in '..\CRMBase\smxClasses.pas',
  smxTypes in '..\CRMComn\smxTypes.pas',
  smxFuncs in '..\CRMComn\smxFuncs.pas',
  smxCells in '..\CRMBase\smxCells.pas',
  smxCfgs in '..\CRMBase\smxCfgs.pas',
  smxWheelDBGrid in '..\CRMBase\smxWheelDBGrid.pas',
  smxField in '..\CRMBase\smxField.pas',
  smxMain in '..\CRMMain\smxMain.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxGlobalStorage in '..\CRMBase\smxGlobalStorage.pas',
  smxLibManager in '..\CRMBase\smxLibManager.pas',
  smxFormManager in '..\CRMBase\smxFormManager.pas',
  smxGlobal in '..\CRMBase\smxGlobal.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxDBManager in '..\CRMBase\smxDBManager.pas',
  smxDBConnection in '..\CRMBase\smxDBConnection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmConf, frmConf);
  Application.Run;
end.
