program CRM;

uses
  ShareMem,
  Forms,
  smxMain in 'smxMain.pas',
  smxClasses in '..\CRMBase\smxClasses.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxTypes in '..\CRMComn\smxTypes.pas',
  smxFuncs in '..\CRMComn\smxFuncs.pas',
  smxClassFuncs in '..\CRMComn\smxClassFuncs.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxCommonStorage in '..\CRMBase\smxCommonStorage.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas',
  smxLibManager in '..\CRMBase\smxLibManager.pas',
  smxDBManager in '..\CRMBase\smxDBManager.pas',
  smxDBConnection in '..\CRMBase\smxDBConnection.pas',
  smxFormManager in '..\CRMBase\smxFormManager.pas',
  smxClassTypes in '..\CRMComn\smxClassTypes.pas',
  smxGlobalVariables in '..\CRMBase\smxGlobalVariables.pas',
  smxCells in '..\CRMBase\smxCells.pas',
  smxCfgs in '..\CRMBase\smxCfgs.pas',
  smxWheelDBGrid in '..\CRMBase\smxWheelDBGrid.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxFilterCells in '..\CRMBase\smxFilterCells.pas';

{$R *.res}

begin
  Application.Initialize;

  {Initialize;
  if ConnectDatabase and CreateMainForm then
    Application.Run;}

  Initialize;
  if ConnectDatabase('CRM')
    //and CheckUser
    and CreateMainForm then
  begin
    //CallBack[1] := Integer(Database);
    //LoadPackages;
    Application.Run;
    //UnLoadPackages;
    DisconnectDatabase;
  end;
end.
