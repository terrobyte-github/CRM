unit smxLibFuncs;

interface

uses
  Classes, Windows, ImgList, smxClasses, smxDBIntf;

//function FindFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
//function FindFormByHandleLib(AHandle: HWND): TsmxCustomForm;
//function NewCellLib(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
//function NewFormLib(const ADatabase: IsmxDatabase; ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;
//function IsCellLib(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
//function FindDatabaseByNameLib(ADatabaseName: String): IsmxDatabase;
//function FindProcedureByNameLib(ALibName, AProcName: String): Pointer;
//function FindCommonParamByNameLib(AName: String): Variant;
function ComStorageLib: TsmxCustomCommonStorage;
function LibManagerLib: TsmxCustomLibraryManager;
function DBManagerLib: TsmxCustomDatabaseManager;
function FrmManagerLib: TsmxCustomFormManager;
function ImgListLib: TCustomImageList;
function GetFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
function GetDatabaseByNameLib(ADatabaseName: String): IsmxDatabase;
//function AddFormIntoManagerLib(AForm: TsmxCustomForm): Boolean;
//function DelFormFromManagerLib(AForm: TsmxCustomForm): Boolean;

implementation

uses
  Variants, smxLibProcs, smxConsts, smxLibTypes;

{function FindFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
var FuncFindFormByComboID: TsmxFuncFindFormByComboID;
begin
  if Assigned(Call) then
    FuncFindFormByComboID := TsmxFuncFindFormByComboID(Integer(Call(141))) else
    FuncFindFormByComboID := nil;
  if Assigned(FuncFindFormByComboID) then
    Result := FuncFindFormByComboID(ACfgID, AID) else
    Result := nil;
end;

function FindFormByHandleLib(AHandle: HWND): TsmxCustomForm;
var FuncFindFormByHandle: TsmxFuncFindFormByHandle;
begin
  if Assigned(Call) then
    FuncFindFormByHandle := TsmxFuncFindFormByHandle(Integer(Call(142))) else
    FuncFindFormByHandle := nil;
  if Assigned(FuncFindFormByHandle) then
    Result := FuncFindFormByHandle(AHandle) else
    Result := nil;
end;

function NewCellLib(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
var FuncNewCell: TsmxFuncNewCell;
begin
  if Assigned(Call) then
    FuncNewCell := TsmxFuncNewCell(Integer(Call(201))) else
    FuncNewCell := nil;
  if Assigned(FuncNewCell) then
    Result := FuncNewCell(nil, ADatabase, ACfgID) else
    raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function NewFormLib(const ADatabase: IsmxDatabase; ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;
var FuncNewForm: TsmxFuncNewForm;
begin
  if Assigned(Call) then
    FuncNewForm := TsmxFuncNewForm(Integer(Call(202))) else
    FuncNewForm := nil;
  if Assigned(FuncNewForm) then
    Result := FuncNewForm(nil, ADatabase, ACfgID, AIntfID, AID) else
    raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function IsCellLib(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
var FuncIsCell: TsmxFuncIsCell;
begin
  if Assigned(Call) then
    FuncIsCell := TsmxFuncIsCell(Integer(Call(203))) else
    FuncIsCell := nil;
  if Assigned(FuncIsCell) then
    Result := FuncIsCell(ACell, ACellClassName) else
    Result := False;
end;

function FindDatabaseByNameLib(ADatabaseName: String): IsmxDatabase;
var FuncFindDatabaseByName: TsmxFuncFindDatabaseByName;
begin
  if Assigned(Call) then
    FuncFindDatabaseByName := TsmxFuncFindDatabaseByName(Integer(Call(131))) else
    FuncFindDatabaseByName := nil;
  if Assigned(FuncFindDatabaseByName) then
    Result := FuncFindDatabaseByName(ADatabaseName) else
    Result := nil;
end;

function FindProcedureByNameLib(ALibName, AProcName: String): Pointer;
var FuncFindProcedureByName: TsmxFuncFindProcedureByName;
begin
  if Assigned(Call) then
    FuncFindProcedureByName := TsmxFuncFindProcedureByName(Integer(Call(121))) else
    FuncFindProcedureByName := nil;
  if Assigned(FuncFindProcedureByName) then
    Result := FuncFindProcedureByName(ALibName, AProcName) else
    Result := nil;
end;

function FindCommonParamByNameLib(AName: String): Variant;
var FuncFindCommonParamByName: TsmxFuncFindCommonParamByName;
begin
  if Assigned(Call) then
    FuncFindCommonParamByName := TsmxFuncFindCommonParamByName(Integer(Call(111))) else
    FuncFindCommonParamByName := nil;
  if Assigned(FuncFindCommonParamByName) then
    Result := FuncFindCommonParamByName(AName) else
    Result := Null;
end;}

function ComStorageLib: TsmxCustomCommonStorage;
begin
  if Assigned(Call) then
    Result := TsmxCustomCommonStorage(Integer(Call(1))) else
    Result := nil;
end;

function LibManagerLib: TsmxCustomLibraryManager;
begin
  if Assigned(Call) then
    Result := TsmxCustomLibraryManager(Integer(Call(2))) else
    Result := nil;
end;

function DBManagerLib: TsmxCustomDatabaseManager;
begin
  if Assigned(Call) then
    Result := TsmxCustomDatabaseManager(Integer(Call(3))) else
    Result := nil;
end;

function FrmManagerLib: TsmxCustomFormManager;
begin
  if Assigned(Call) then
    Result := TsmxCustomFormManager(Integer(Call(4))) else
    Result := nil;
end;

function ImgListLib: TCustomImageList;
begin
  if Assigned(Call) then
    Result := TCustomImageList(Integer(Call(5))) else
    Result := nil;
end;

function GetFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
var fm: TsmxCustomFormManager;
begin
  Result := nil;
  fm := FrmManagerLib;
  if Assigned(fm) then
    Result := fm.FindByComboID(ACfgID, AID);
end;

function GetDatabaseByNameLib(ADatabaseName: String): IsmxDatabase;
var dbm: TsmxCustomDatabaseManager; dbc: TsmxDBConnection;
begin
  Result := nil;
  dbm := DBManagerLib;
  if Assigned(dbm) then
  begin
    dbc := dbm.FindByName(ADatabaseName);
    if Assigned(dbc) then
      Result := dbc.Database;
  end;
end;

{function AddFormIntoManagerLib(AForm: TsmxCustomForm): Boolean;
var ProcAddFormIntoManager: TsmxProcAddFormIntoManager;
begin
  if Assigned(Call) then
    ProcAddFormIntoManager := TsmxProcAddFormIntoManager(Integer(Call(143))) else
    ProcAddFormIntoManager := nil;
  if Assigned(ProcAddFormIntoManager) then
  begin
    ProcAddFormIntoManager(AForm);
    Result := True;
  end else
    Result := False;
end;

function DelFormFromManagerLib(AForm: TsmxCustomForm): Boolean;
var ProcDelFormFromManager: TsmxProcDelFormFromManager;
begin
  if Assigned(Call) then
    ProcDelFormFromManager := TsmxProcDelFormFromManager(Integer(Call(144))) else
    ProcDelFormFromManager := nil;
  if Assigned(ProcDelFormFromManager) then
  begin
    ProcDelFormFromManager(AForm);
    Result := True;
  end else
    Result := False;
end;}

end.
