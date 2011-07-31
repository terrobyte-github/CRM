program Conf;

uses
  Forms,
  smxConf in 'smxConf.pas' {frmConf},
  smxProjects in 'smxProjects.pas' {frmProjects},
  smxDBIntf in '..\Common\smxDBIntf.pas',
  smxTypes in '..\Common\smxTypes.pas',
  smxFuncs in '..\Common\smxFuncs.pas',
  smxClassFuncs in '..\Common\smxClassFuncs.pas',
  smxConsts in '..\Common\smxConsts.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxProcs in '..\Common\smxProcs.pas',
  smxCommonStorage in '..\Common\smxCommonStorage.pas',
  smxLibManager in '..\Common\smxLibManager.pas',
  smxCallBack in '..\Common\smxCallBack.pas',
  smxDBManager in '..\Common\smxDBManager.pas',
  smxAddCell in 'smxAddCell.pas' {frmAddCell};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmConf, frmConf);
  Application.Run;
end.
