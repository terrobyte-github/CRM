library rImages;

uses
  ShareMem,
  SysUtils,
  Classes,
  smxLibProcs in '..\CRMCommon\smxLibProcs.pas',
  smxLibTypes in '..\CRMCommon\smxLibTypes.pas',
  smxFuncs in '..\CRMCommon\smxFuncs.pas';

{$R *.res}
{$R ..\Resource\pic.res}

exports
  LibInfo,
  NewResource;

begin
  FillInfo(1, 0, nil, [ltResource]);
end.
