library rImages;

uses
  SysUtils,
  Classes,
  smxLibProcs in '..\Common\smxLibProcs.pas',
  smxLibTypes in '..\Common\smxLibTypes.pas',
  smxFuncs in '..\Common\smxFuncs.pas',
  smxProcs in '..\Common\smxProcs.pas';

{$R *.res}
{$R ..\Resource\pic.res}

exports
  LibInfo,
  NewResource;

begin
  FillInfo(1, 0, nil, [ltResource]);
end.
