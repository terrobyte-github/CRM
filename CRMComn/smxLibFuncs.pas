unit smxLibFuncs;

interface

uses
  smxClasses;

function CreateCell(CfgID: Integer; ID: Integer = 0): TsmxBaseCell;
function ClassCell(Cell: TsmxBaseCell; CellClassName: String): Boolean;
//function ShowInf(Msg: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
//function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;

implementation

uses
  smxLibProcs, smxDBIntf, smxLibTypes;

function CreateCell(CfgID: Integer; ID: Integer = 0): TsmxBaseCell;
var FuncNewCell: TsmxFuncNewCell;
begin
  FuncNewCell := TsmxFuncNewCell(Integer(Call(101)));
  Result := FuncNewCell(nil, IsmxDatabase(Integer(Call(1))), CfgID, ID);
end;

function ClassCell(Cell: TsmxBaseCell; CellClassName: String): Boolean;
var FuncIsCell: TsmxFuncIsCell;
begin
  FuncIsCell := TsmxFuncIsCell(Integer(Call(111)));
  Result := FuncIsCell(Cell, CellClassName);
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
      Database.StartTrans;
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
        Database.CommitTrans else
        Database.RollbackTrans;
      if msg <> '' then
        ShowInf(msg);
      Result := res = 0;
    except
      Database.RollbackTrans;
    end;
  end;
end;}

end.
