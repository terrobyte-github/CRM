library rImages;

uses
  Classes,
  smxLibProcs,
  smxLibTypes,
  smxFuncs;

{$R *.res}
{$R ..\Resource\pic.res}

exports
  smxLibProcs.LibInfo{,
  NewResource};

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltResource]);
end.
