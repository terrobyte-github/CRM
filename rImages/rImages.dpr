library rImages;

uses
  Classes,
  smxLibProcs,
  smxLibTypes,
  smxFuncs;

{$R *.res}
{$R ..\Resource\pic.res}

exports
  LibInfo{,
  NewResource};

begin
  FillInfo(1, 0, nil, [ltResource]);
end.
