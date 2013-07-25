library rImages;

uses
  smxLibProcs,
  smxLibTypes;

{$R *.res}
{$R ..\Resource\pic.res}

exports
  smxLibProcs.LibInfo;

begin
  smxLibProcs.FillInfo(1, 0, nil, [ltResource]);
end.
