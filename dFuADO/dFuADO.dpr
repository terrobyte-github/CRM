library dFuADO;

uses
  //ShareMem,
  SysUtils,
  Classes,
  smxLibProcs in '..\CRMCommon\smxLibProcs.pas',
  smxLibTypes in '..\CRMCommon\smxLibTypes.pas',
  smxADODB in '..\CRMBase\smxADODB.pas';

exports
  LibInfo,
  NewADODatabase;

{$R *.res}

begin
  FillInfo(1, 0, nil, [ltDatabaseIntf]);
end.
 