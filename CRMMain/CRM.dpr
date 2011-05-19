program CRM;

uses
  ShareMem,
  Forms,
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxADODB in '..\CRMBase\smxADODB.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxClasses in '..\CRMBase\smxClasses.pas',
  smxField in '..\CRMBase\smxField.pas',
  smxCfgs in '..\CRMBase\smxCfgs.pas',
  smxCells in '..\CRMBase\smxCells.pas',
  smxFltrCells in '..\CRMBase\smxFltrCells.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxFuncs in '..\CRMComn\smxFuncs.pas',
  smxTypes in '..\CRMComn\smxTypes.pas',
  smxWheelDBGrid in '..\CRMBase\smxWheelDBGrid.pas',
  smxParams in '..\CRMComn\smxParams.pas',
  smxMain in 'smxMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Initialize;
  if ConnectDatabase and CreateMainForm then
    Application.Run;
end.
