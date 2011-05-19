library dADO;

uses
  ShareMem,
  smxLibProcs in '..\CRMComn\smxLibProcs.pas',
  smxProcs in '..\CRMComn\smxProcs.pas',
  smxADODB in '..\CRMBase\smxADODB.pas',
  smxDBIntf in '..\CRMComn\smxDBIntf.pas',
  smxBaseClasses in '..\CRMBase\smxBaseClasses.pas',
  smxConsts in '..\CRMComn\smxConsts.pas',
  smxField in '..\CRMBase\smxField.pas',
  smxTypes in '..\CRMComn\smxTypes.pas';

{$R *.res}

exports
  LibInfo,
  NewADODatabase;

begin
  FillInfo(1, 0, LibInit, [ltDatabaseIntf]);
end.
 