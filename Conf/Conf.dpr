program Conf;

uses
  //ShareMem,
  Forms,
  smxConf in 'smxConf.pas' {frmConf},
  smxProjects in 'smxProjects.pas' {frmProjects},
  smxDBIntf in '..\CRMCommon\smxDBIntf.pas',
  smxTypes in '..\CRMCommon\smxTypes.pas',
  smxFuncs in '..\CRMCommon\smxFuncs.pas',
  smxClassFuncs in '..\CRMCommon\smxClassFuncs.pas',
  smxConsts in '..\CRMCommon\smxConsts.pas',
  smxLibTypes in '..\CRMCommon\smxLibTypes.pas',
  smxProcs in '..\CRMCommon\smxProcs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmConf, frmConf);
  Application.Run;
end.
