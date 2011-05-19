unit smxBaseClasses;

interface

uses
  Classes;

type
  { TsmxComponent }

  TsmxComponent = class(TComponent)
  private
    function GetVersion: String;
  public
    class function GetDescription: String; virtual;

    property Version: String read GetVersion;
  end;

  { TsmxInterfacedComponent }

  TsmxInterfacedComponent = class(TInterfacedObject)
  protected
    function GetVersion: String; virtual;
  public
    class function GetDescription: String; virtual;

    property Version: String read GetVersion;
  end;

implementation

{$I ..\Resource\smxVers.inc}

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

class function TsmxInterfacedComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedComponent.GetVersion: String;
begin
  Result := '';
end;

end.



