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
  smxFilterCells in '..\CRMBase\smxFilterCells.pas',
  smxFuncs in '..\CRMComn\smxFuncs.pas',
  smxTypes in '..\CRMComn\smxTypes.pas',
  smxWheelDBGrid in '..\CRMBase\smxWheelDBGrid.pas',
  smxMain in 'smxMain.pas',
  smxCallBack in '..\CRMBase\smxCallBack.pas',
  smxGlobalStorage in '..\CRMBase\smxGlobalStorage.pas',
  smxFormManager in '..\CRMBase\smxFormManager.pas',
  smxLibManager in '..\CRMBase\smxLibManager.pas',
  smxGlobal in '..\CRMBase\smxGlobal.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxDBManager in '..\CRMBase\smxDBManager.pas',
  smxDBConnection in '..\CRMBase\smxDBConnection.pas';

{$R *.res}

begin
  Application.Initialize;

  Initialize;
  if ConnectDatabase and CreateMainForm then
    Application.Run;

  {Initialize;
  if ConnectDatabase('CRM') then
  begin
    CallBack[1] := Integer(Database);
    if CreateMainForm then
      Application.Run;
    //DisconnectDatabase;
  end;}
end.
