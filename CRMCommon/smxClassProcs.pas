unit smxClassProcs;

interface

uses
  smxBaseClasses;

procedure RegistrationClasses(AClasses: array of TsmxComponent);

implementation

uses
  Classes;

procedure RegistrationClasses(AClasses: array of TsmxComponent);
var i: Integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    RegisterClass(TPersistentClass(AClasses[i]));
end;

end.
