library dADO;

uses
  ShareMem,
  smxLibProcs in '..\CRMComn\smxLibProcs.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxField in '..\CRMBase\smxField.pas',
  smxADODB in '..\CRMBase\smxADODB.pas',
  smxLibTypes in '..\CRMComn\smxLibTypes.pas';

{$R *.res}

exports
  LibInfo,
  NewADODatabase;

begin
  FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
