unit smxMain2;

interface

procedure Initialize;
function CreateMainForm: Boolean;
function LogIn: Boolean;

implementation

uses
  Windows;

var
  Handle: THandle = 0;

procedure Initialize;
type
  TsmxInitialize = procedure;
var
  Init: TsmxInitialize;
begin
  if Handle > 0 then
  begin
    @Init := GetProcAddress(Handle, 'Initialize');
    if Assigned(Init) then
      Init;
  end;
end;

function CreateMainForm: Boolean;
type
  TsmxCreateMainForm = function: Boolean;
var
  CreateForm: TsmxCreateMainForm;
begin
  Result := False;
  if Handle > 0 then
  begin
    @CreateForm := GetProcAddress(Handle, 'CreateMainForm');
    if Assigned(CreateForm) then
      Result := CreateForm;
  end;
end;

function LogIn: Boolean;
type
  TsmxLogIn = function: Boolean;
var
  Log: TsmxLogIn;
begin
  Result := False;
  if Handle > 0 then
  begin
    @Log := GetProcAddress(Handle, 'LogIn');
    if Assigned(Log) then
      Result := Log;
  end;
end;

initialization
  Handle := LoadLibrary('cManagers.dll');

finalization
  if Handle > 0 then
    FreeLibrary(Handle);

end.
