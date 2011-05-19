program Conf;

uses
  ShareMem,
  Forms,
  smxConf in 'smxConf.pas' {frmConf},
  smxProject in 'smxProject.pas' {frmProject},
  smxClasses in '..\CRMBase\smxClasses.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxTypes in '..\CRMComn\smxTypes.pas',
  smxFuncs in '..\CRMComn\smxFuncs.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxDBConnection in '..\CRMBase\smxDBConnection.pas',
  smxLibManager in '..\CRMBase\smxLibManager.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas',
  smxCommonStorage in '..\CRMBase\smxCommonStorage.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxDBManager in '..\CRMBase\smxDBManager.pas',
  smxProcs in '..\CRMComn\smxProcs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmConf, frmConf);
  Application.Run;
end.
