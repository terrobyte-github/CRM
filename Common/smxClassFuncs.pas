unit smxClassFuncs;

interface

uses
  Classes, smxClasses, smxTypes, smxDBIntf;

function CfgIDToCfgClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
function CfgIDToCellClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
function NewCfg(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfg;
function NewCell(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
function NewForm(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AIntfID: Integer = 0; AID: Integer = 0): TsmxCustomForm;
function VarToParams(V: Variant): TsmxParams;
function ParamsToVar(Params: TsmxParams): Variant;
function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function ParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant;
function FieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  ADefValue: Variant): Variant;
function ParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  ADefValue: Variant): Variant;
function PerformRequest(ARequest: TsmxCustomRequest; ASame: Boolean = False): Boolean;
function RequestReturnKeyValue(ARequest: TsmxCustomRequest; var AKey, AValue: Variant): Boolean;

implementation

uses
  Variants, smxFuncs, smxConsts;

function CfgIDToCfgClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ADatabase, ACfgID);
  try
    t.Initialize;
    Result := t.CfgClass;
  finally
    t.Free;
  end;
end;

function CfgIDToCellClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ADatabase, ACfgID);
  try
    t.Initialize;
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
  ACfgID: Integer; AIntfID: Integer = 0; AID: Integer = 0): TsmxCustomForm;
var CellClass: TsmxBaseCellClass; FormClass: TsmxCustomFormClass;
begin
  CellClass := CfgIDToCellClass(ADatabase, ACfgID);
  FormClass := nil;
  if Assigned(CellClass) then
    if CellClass.InheritsFrom(TsmxCustomForm) then
      FormClass := TsmxCustomFormClass(CellClass);
  if Assigned(FormClass) then
  begin
    if AIntfID = 0 then
      Result := FormClass.Create(AOwner, ADatabase, ACfgID, AID) else
      Result := FormClass.CreateByIntfID(AOwner, ADatabase, ACfgID, AIntfID, AID);
  end else
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

function FieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  ADefValue: Variant): Variant;
var f: IsmxField;
begin
  Result := ADefValue;
  if Assigned(ARequest) then
  begin
    f := ARequest.FindFieldSense(ASense);
    if Assigned(f) then
      if not VarIsNull(f.Value) then
        Result := f.Value;
  end;
end;

function ParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  ADefValue: Variant): Variant;
var p: IsmxParam;
begin
  Result := ADefValue;
  if Assigned(ARequest) then
  begin
    p := ARequest.FindParamLocation(ALocation);
    if Assigned(p) then
      if not VarIsNull(p.Value) then
        Result := p.Value;
  end;
end;

function PerformRequest(ARequest: TsmxCustomRequest; ASame: Boolean = False): Boolean;
var res: Integer; msg: String;
begin
  Result := False;
  if not Assigned(ARequest) then
    Exit;
  if not Assigned(ARequest.Database) or not Assigned(ARequest.CellDataSet) then
    Exit;
  with ARequest do
  begin
    if not Database.InTransaction then
      Database.StartTransaction;
    try
      res := 1; msg := '';
      Perform(ASame);
      case CellDataSet.DataSetType of
        dstQuery:
        begin
          res := FieldSenseValueDef(ARequest, fsResult, 1);
          msg := FieldSenseValueDef(ARequest, fsMessage, '');
        end;
        dstStoredProc:
        begin
          res := ParamLocationValueDef(ARequest, plResult, 1);
          msg := ParamLocationValueDef(ARequest, plMessage, '');
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

function RequestReturnKeyValue(ARequest: TsmxCustomRequest; var AKey, AValue: Variant): Boolean;
begin
  Result := False;
  AKey := Null; AValue := Null;
  if not Assigned(ARequest) then
    Exit;
  if not Assigned(ARequest.Database) or not Assigned(ARequest.CellDataSet) then
    Exit;
  with ARequest do
  begin
    Perform;
    case CellDataSet.DataSetType of
      dstQuery:
      begin
        AKey := FieldSenseValueDef(ARequest, fsKey, Null);
        AValue := FieldSenseValueDef(ARequest, fsValue, Null);
      end;
      dstStoredProc:
      begin
        AKey := ParamLocationValueDef(ARequest, plKey, Null);
        AValue := ParamLocationValueDef(ARequest, plValue, Null);
      end;
    end;
    Result := True;
  end;
end;

end.
