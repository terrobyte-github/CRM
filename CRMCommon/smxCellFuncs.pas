unit smxCellFuncs;

interface

uses
  smxCells;

function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;

implementation

uses
  smxFuncs, smxDBIntf, smxTypes;

function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;
var fld: IsmxField; prm: IsmxParam; res: Integer; msg: String;
begin
  Result := False;
  if not Assigned(Request.Database) then
    Exit;
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
end;

end.
