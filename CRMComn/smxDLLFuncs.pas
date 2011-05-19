unit smxDLLFuncs;

interface

uses
  Classes, Windows, smxClasses, smxCells, smxParams, smxTypes;

function CreateCell(ID: Integer; Call: TsmxFuncCallBack): TsmxBaseCell;
function ClassCell(Cell: TsmxBaseCell; CellClassName: String; Call: TsmxFuncCallBack): Boolean;
function PerformRequest(Request: TsmxCustomRequest): Boolean;

implementation

uses
  SysUtils, smxFuncs, smxDLLTypes, smxDBIntf;

function CreateCell(ID: Integer; Call: TsmxFuncCallBack): TsmxBaseCell;
var FuncNewCell: TsmxFuncNewCell;
begin
  FuncNewCell := TsmxFuncNewCell(Integer(Call(101)));
  Result := FuncNewCell(nil, ID, Call);
end;

function ClassCell(Cell: TsmxBaseCell; CellClassName: String; Call: TsmxFuncCallBack): Boolean;
var FuncIsCell: TsmxFuncIsCell;
begin
  FuncIsCell := TsmxFuncIsCell(Integer(Call(111)));
  Result := FuncIsCell(Cell, CellClassName);
end;

function PerformRequest(Request: TsmxCustomRequest): Boolean;
var fld: IsmxField; res: Integer; msg: String;
begin
  Result := False;
  with Request do
  begin
    if not(Database.InTransaction) then
      Database.StartTrans;
    try
      res := -1; msg := '';
      Perform;
      case CellDataSet.DataSetType of
        dstQuery:
        begin
          fld := FindFieldSense(fsResult);
          if Assigned(fld) then
            res := fld.Value;
          fld := FindFieldSense(fsMsg);
          if Assigned(fld) then
            msg := fld.Value;
        end;
        dstStoredProc:
        begin
          res := StrToIntDef(RequestParams['@Return_value'], -1);
          msg := RequestParams['@Msg'];
        end;
      end;
      if res = 0 then
        Database.CommitTrans else
        Database.RollbackTrans;
      if msg <> '' then
        TsmxFuncInf(Integer(Call(151)))(msg);
      Result := res = 0;
    except
      Database.RollbackTrans;
    end;
  end;
end;

end.
