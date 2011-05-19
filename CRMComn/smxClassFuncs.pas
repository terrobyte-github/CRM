unit smxClassFuncs;

interface

uses
  Classes, smxClasses, smxDBIntf;

function CfgIDToCfgClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
function CfgIDToCellClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
function NewCfg(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfg;
function NewCell(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
function NewForm(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;
function VarToParams(V: Variant): TsmxParams;
function ParamsToVar(Params: TsmxParams): Variant;
function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function ParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant;
function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;

implementation

uses
  Variants, smxFuncs, smxTypes, smxConsts;

function CfgIDToCfgClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ADatabase, ACfgID);
  t.Initialize;
  try
    Result := t.CfgClass;
  finally
    t.Free;
  end;
end;

function CfgIDToCellClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ADatabase, ACfgID);
  t.Initialize;
  try
    Result := t.CellClass;
  finally
    t.Free;
  end;
end;

function NewCfg(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfg;
var CfgClass: TsmxBaseCfgClass;
begin
  CfgClass := CfgIDToCfgClass(ADatabase, ACfgID);
  if Assigned(CfgClass) then
    Result := CfgClass.Create(AOwner, ADatabase, ACfgID) else
    raise EsmxCfgError(@SCfgBuildError);
end;

function NewCell(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
var CellClass: TsmxBaseCellClass;
begin
  CellClass := CfgIDToCellClass(ADatabase, ACfgID);
  if Assigned(CellClass) then
    Result := CellClass.Create(AOwner, ADatabase, ACfgID) else
    raise EsmxCellError(@SCellBuildError);
end;

function NewForm(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;
var CellClass: TsmxBaseCellClass; FormClass: TsmxCustomFormClass;
begin
  CellClass := CfgIDToCellClass(ADatabase, ACfgID);
  FormClass := nil;
  if Assigned(CellClass) then
    if CellClass.InheritsFrom(TsmxCustomForm) then
      FormClass := TsmxCustomFormClass(CellClass);
  if Assigned(FormClass) then
    Result := FormClass.CreateByIntfID(AOwner, ADatabase, ACfgID, AIntfID, AID) else
    raise EsmxCellError(@SCellBuildError);
end;

function VarToParams(V: Variant): TsmxParams;
var i: Integer;
begin
  Result := TsmxParams.Create(TsmxParam);
  if VarIsArray(V) and (VarArrayHighBound(V, 1) = 1) then
    if VarIsArray(V[0]) and VarIsArray(V[1])
      and (VarArrayHighBound(V[0], 1) = VarArrayHighBound(V[1], 1)) then
    begin
      for i := 0 to VarArrayHighBound(V[0], 1) do
        with Result.Add do
        begin
          ParamName := V[0][i];
          ParamValue := V[1][i];
        end;
    end;
end;

function ParamsToVar(Params: TsmxParams): Variant;
var i: Integer; v1, v2: Variant;
begin
  Result := Unassigned;
  if Params.Count > 0 then
  begin
    Result := VarArrayCreate([0, 1], varVariant);
    v1 := VarArrayCreate([0, Params.Count - 1], varVariant);
    v2 := VarArrayCreate([0, Params.Count - 1], varVariant);
    for i := 0 to Params.Count - 1 do
    begin
      v1[i] := Params[i].ParamName;
      v2[i] := Params[i].ParamValue;
    end;
    Result[0] := v1;
    Result[1] := v2;
  end;
end;

function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
var CellClass: TsmxBaseCellClass;
begin
  CellClass := TsmxBaseCellClass(FindClass(ACellClassName));
  Result := ACell is CellClass;
end;

function ParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant;
var p: TsmxParam;
begin
  Result := ADefValue;
  if Assigned(AParams) then
  begin
    p := AParams.FindByName(AName);
    if Assigned(p) then
      if not VarIsNull(p.ParamValue) then
        Result := p.ParamValue;
  end;
end;

function PerformRequest(Request: TsmxCustomRequest; Same: Boolean = False): Boolean;
var fld: IsmxField; prm: IsmxParam; res: Integer; msg: String;
begin
  Result := False;
  if not Assigned(Request) then
    Exit;
  if not Assigned(Request.Database) or not Assigned(Request.CellDataSet) then
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
 