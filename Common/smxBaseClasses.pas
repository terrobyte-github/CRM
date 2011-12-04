unit smxBaseClasses;

interface

uses
  Classes, smxBaseIntf;

type
  { TsmxComponent }

  TsmxComponent = class(TComponent)
  private
    function GetVersion: String;
  public
    class function GetDescription: String; virtual;

    property Version: String read GetVersion;
  end;

  TsmxComponentClass = class of TsmxComponent;

  { TsmxInterfacedComponent }

  TsmxInterfacedComponent = class(TInterfacedObject, IsmxBaseInterface)
  protected
    function GetVersion: String; virtual;
  public
    function GetDescription: String; virtual;

    property Version: String read GetVersion;
  end;

//procedure RegistrationClasses(AClasses: array of TsmxComponentClass);
//procedure UnRegistrationClasses(AClasses: array of TsmxComponentClass);

implementation

{$I ..\Resource\smxVers.inc}

{procedure RegistrationClasses(AClasses: array of TsmxComponentClass);
var i: Integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    RegisterClass(AClasses[i]);
end;

procedure UnRegistrationClasses(AClasses: array of TsmxComponentClass);
var i: Integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    UnRegisterClass(AClasses[i]);
end;}

{ TsmxComponent }

class function TsmxComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxComponent.GetVersion: String;
begin
  Result := smxVersion;
end;

{ TsmxInterfacedComponent }

function TsmxInterfacedComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedComponent.GetVersion: String;
begin
  Result := smxIntfVersion;
end;

end.



