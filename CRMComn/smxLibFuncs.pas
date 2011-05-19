unit smxLibFuncs;

interface

uses
  Windows, smxClasses, smxFormManager, smxDBManager;

function FormManagerLib: TsmxFormManager;
function DBManagerLib: TsmxDBManager;
function FindFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
function FindFormByHandleLib(AHandle: HWND): TsmxBaseCell;
function NewCellLib(CfgID: Integer; ID: Integer = 0): TsmxBaseCell;
function IsCellLib(Cell: TsmxBaseCell; CellClassName: String): Boolean;
//function ShowInf(Msg: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
//function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;

implementation

uses
  smxLibProcs, smxFuncs, smxDBIntf, smxTypes, smxLibTypes, smxCellTypes,
  smxConsts;

function FormManagerLib: TsmxFormManager;
begin
  Result := TsmxFormManager(Integer(Call(4)));
end;

function DBManagerLib: TsmxDBManager;
begin
  Result := TsmxDBManager(Integer(Call(1)));
end;

function FindFormByComboIDLib(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
var FuncFindFormByComboID: TsmxFuncFindFormByComboID;
begin
  FuncFindFormByComboID := TsmxFuncFindFormByComboID(Integer(Call(141)));
  if Assigned(FuncFindFormByComboID) then
    Result := FuncFindFormByComboID(ACfgID, AID) else
    Result := nil;
end;

function FindFormByHandleLib(AHandle: HWND): TsmxBaseCell;
var FuncFindFormByHandle: TsmxFuncFindFormByHandle;
begin
  FuncFindFormByHandle := TsmxFuncFindFormByHandle(Integer(Call(142)));
  if Assigned(FuncFindFormByHandle) then
    Result := FuncFindFormByHandle(AHandle) else
    Result := nil;
end;

function NewCellLib(CfgID: Integer; ID: Integer = 0): TsmxBaseCell;
var FuncNewCell: TsmxFuncNewCell;
begin
  FuncNewCell := TsmxFuncNewCell(Integer(Call(201)));
  if Assigned(FuncNewCell) then
    //Result := FuncNewCell(nil, IsmxDatabase(Integer(Call(1))), CfgID, ID) else
    Result := FuncNewCell(nil, DBManagerLib.DBConnections[0].Database, CfgID, ID) else
    raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function IsCellLib(Cell: TsmxBaseCell; CellClassName: String): Boolean;
var FuncIsCell: TsmxFuncIsCell;
begin
  FuncIsCell := TsmxFuncIsCell(Integer(Call(202)));
  if Assigned(FuncIsCell) then
    Result := FuncIsCell(Cell, CellClassName) else
    Result := False;
end;

{function ShowInf(Msg: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
var FuncInf: TsmxFuncInf;
begin
  FuncInf := TsmxFuncInf(Integer(Call(151)));
  Result := FuncInf(Msg, uType);
end;}

{function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;
var fld: IsmxField; prm: IsmxParam; res: Integer; msg: String;
begin
  Result := False;
  with Request do
  begin
    if not Database.InTransaction then
      Database.StartTransaction;
    try
      res := -1; msg := '';
      Perform(Same);
      case CellDataSet.DataSetType of
        dstQuery:
        begin
          fld := FindFieldSense(fsResult);
          if Assigned(fld) then
            res := fld.Value;
          fld := FindFieldSense(fsMessage);
          if Assigned(fld) then
            msg := fld.Value;
        end;
        dstStoredProc:
        begin
          prm := FindParamLocation(plResult);
          if Assigned(prm) then
            res := prm.Value;
          prm := FindParamLocation(plMessage);
          if Assigned(prm) then
            msg := prm.Value;
        end;
      end;
      if res = 0 then
        Database.CommitTransaction else
        Database.RollbackTransaction;
      if msg <> '' then
        Inf(msg);
      Result := res = 0;
    except
      Database.RollbackTransaction;
    end;
  end;
end;}

end.
