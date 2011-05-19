unit smxLibFuncs;

interface

uses
  Windows, smxClasses, smxDBIntf;

function FindFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
function FindFormByHandleLib(AHandle: HWND): TsmxCustomForm;
function NewCellLib(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
function NewFormLib(const ADatabase: IsmxDatabase; ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;
function IsCellLib(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function FindDatabaseByNameLib(ADatabaseName: String): IsmxDatabase;

implementation

uses
  smxLibProcs, smxClassTypes, smxConsts, smxLibTypes;

function FindFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
var FuncFindFormByComboID: TsmxFuncFindFormByComboID;
begin
  FuncFindFormByComboID := TsmxFuncFindFormByComboID(Integer(Call(141)));
  if Assigned(FuncFindFormByComboID) then
    Result := FuncFindFormByComboID(ACfgID, AID) else
    Result := nil;
end;

function FindFormByHandleLib(AHandle: HWND): TsmxCustomForm;
var FuncFindFormByHandle: TsmxFuncFindFormByHandle;
begin
  FuncFindFormByHandle := TsmxFuncFindFormByHandle(Integer(Call(142)));
  if Assigned(FuncFindFormByHandle) then
    Result := FuncFindFormByHandle(AHandle) else
    Result := nil;
end;

function NewCellLib(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
var FuncNewCell: TsmxFuncNewCell;
begin
  FuncNewCell := TsmxFuncNewCell(Integer(Call(201)));
  if Assigned(FuncNewCell) then
    Result := FuncNewCell(nil, ADatabase, ACfgID) else
    raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function NewFormLib(const ADatabase: IsmxDatabase; ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;
var FuncNewForm: TsmxFuncNewForm;
begin
  FuncNewForm := TsmxFuncNewForm(Integer(Call(202)));
  if Assigned(FuncNewForm) then
    Result := FuncNewForm(nil, ADatabase, ACfgID, AIntfID, AID) else
    raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function IsCellLib(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
var FuncIsCell: TsmxFuncIsCell;
begin
  FuncIsCell := TsmxFuncIsCell(Integer(Call(203)));
  if Assigned(FuncIsCell) then
    Result := FuncIsCell(ACell, ACellClassName) else
    Result := False;
end;

function FindDatabaseByNameLib(ADatabaseName: String): IsmxDatabase;
var FuncFindDatabaseByName: TsmxFuncFindDatabaseByName;
begin
  FuncFindDatabaseByName := TsmxFuncFindDatabaseByName(Integer(Call(131)));
  if Assigned(FuncFindDatabaseByName) then
    Result := FuncFindDatabaseByName(ADatabaseName) else
    Result := nil;
end;

end.
