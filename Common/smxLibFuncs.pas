unit smxLibFuncs;

interface

uses
  Classes, smxManagerIntf;

function GetStorageManager: IsmxStorageManager;
function GetLibraryManager: IsmxLibraryManager;
function GetDatabaseManager: IsmxDatabaseManager;
function GetFormManager: IsmxFormManager;
function GetImageListManager: IsmxImageListManager;

implementation

uses
  smxLibProcs, smxConsts;

function GetStorageManager: IsmxStorageManager;
begin
  if Assigned(smxLibProcs.Call) then
    Result := IsmxStorageManager(Integer(smxLibProcs.Call(smxConsts.cStorageManager))) else
    Result := nil;
end;

function GetLibraryManager: IsmxLibraryManager;
begin
  if Assigned(smxLibProcs.Call) then
    Result := IsmxLibraryManager(Integer(smxLibProcs.Call(smxConsts.cLibraryManager))) else
    Result := nil;
end;

function GetDatabaseManager: IsmxDatabaseManager;
begin
  if Assigned(smxLibProcs.Call) then
    Result := IsmxDatabaseManager(Integer(smxLibProcs.Call(smxConsts.cDatabaseManager))) else
    Result := nil;
end;

function GetFormManager: IsmxFormManager;
begin
  if Assigned(smxLibProcs.Call) then
    Result := IsmxFormManager(Integer(smxLibProcs.Call(smxConsts.cFormManager))) else
    Result := nil;
end;

function GetImageListManager: IsmxImageListManager;
begin
  if Assigned(smxLibProcs.Call) then
    Result := IsmxImageListManager(Integer(smxLibProcs.Call(smxConsts.cImageListManager))) else
    Result := nil;
end;

end.
