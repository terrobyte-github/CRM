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
//function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function GetParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant;
  var AValue: Variant): Boolean; overload;
function GetParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant; overload;
function GetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  ADefValue: Variant; var AValue: Variant): Boolean; overload;
function GetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  ADefValue: Variant): Variant; overload;
function SetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  AValue: Variant): Boolean;
function GetParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  ADefValue: Variant; var AValue: Variant): Boolean; overload;
function GetParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  ADefValue: Variant): Variant; overload;
function SetParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  AValue: Variant): Boolean;
//function PerformRequest(ARequest: TsmxCustomRequest; ASame: Boolean = False): Boolean;
function GetRequestKeyAndValue(ARequest: TsmxCustomRequest; var AKey, AValue: Variant): Boolean;
function GetRequestValueByKey(ARequest: TsmxCustomRequest; AKey: Variant; var AValue: Variant): Boolean;
function SetRequestValueByKey(ARequest: TsmxCustomRequest; AKey, AValue: Variant): Boolean;

implementation

uses
  Variants, smxFuncs, smxConsts, smxClassProcs, smxProcs;

function GetTypeCfgIDByCfgID(ADatabase: IsmxDatabase; ACfgID: Integer): Integer;
var
  Cell: TsmxBaseCell;
  Request: IsmxDataSet;
begin
  Request := ADatabase.NewDataSet(smxClassProcs.SelectRequestCfg.DataSetType);
  try
  finally
    Request := nil;
  end;
end;

function CfgIDToCfgClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
var
  TypeCfg: TsmxTypeCfg;
  TypeCfgID: Integer;
begin
  //t := TsmxTypeCfg.Create(nil, ADatabase, ACfgID);

  TypeCfg := TsmxTypeCfg.Create(nil);
  try
    TypeCfg.CfgDatabase := ADatabase;
    TypeCfg.CfgID := ACfgID;
    //TypeCfg.UpdateSetting := smxProcs.UpdateSetting; // SelectCfgID;
    //TypeCfg.Initialize;
    TypeCfg.LoadCfg;
    TypeCfg.ReadCfg;
    Result := TypeCfg.CfgClass;
  finally
    TypeCfg.Free;
  end;
end;

function CfgIDToCellClass(const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
var
  TypeCfg: TsmxTypeCfg;
begin
  //t := TsmxTypeCfg.Create(nil, ADatabase, ACfgID);
  TypeCfg := TsmxTypeCfg.Create(nil);
  try
    TypeCfg.CfgDatabase := ADatabase;
    TypeCfg.CfgID := ACfgID;
    //TypeCfg.Initialize;
    TypeCfg.LoadCfg;
    TypeCfg.ReadCfg;
    Result := TypeCfg.CellClass;
  finally
    TypeCfg.Free;
  end;
end;

function NewCfg(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCfg;
var
  CfgClass: TsmxBaseCfgClass;
begin
  CfgClass := CfgIDToCfgClass(ADatabase, ACfgID);
  if Assigned(CfgClass) then
    //Result := CfgClass.Create(AOwner, ADatabase, ACfgID) else
  begin
    Result := CfgClass.Create(AOwner);
    Result.CfgDatabase := ADatabase;
    Result.CfgID := ACfgID;
  end else
    raise EsmxCfgError(@SCfgBuildError);
end;

function NewCell(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;
var
  CellClass: TsmxBaseCellClass;
begin
  CellClass := CfgIDToCellClass(ADatabase, ACfgID);
  if Assigned(CellClass) then
    Result := CellClass.Create(AOwner, ADatabase, ACfgID) else
    raise EsmxCellError(@SCellBuildError);
end;

function NewForm(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AIntfID: Integer = 0; AID: Integer = 0): TsmxCustomForm;
var
  CellClass: TsmxBaseCellClass;
  FormClass: TsmxCustomFormClass;
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

{function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
var
  CellClass: TsmxBaseCellClass;
begin
  CellClass := TsmxBaseCellClass(FindClass(ACellClassName));
  Result := ACell is CellClass;
end;}

function GetParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant;
  var AValue: Variant): Boolean;
var
  Param: TsmxParam;
begin
  Result := False;
  AValue := ADefValue;
  if Assigned(AParams) then
  begin
    Param := AParams.FindByName(AName);
    if Assigned(Param) then
    begin
      AValue := Param.ParamValue;
      Result := True;
    end;
  end;
end;

function GetParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant;
begin
  GetParamValueDef(AParams, AName, ADefValue, Result);
end;

function GetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  ADefValue: Variant; var AValue: Variant): Boolean;
var
  Field: IsmxField;
begin
  Result := False;
  AValue := ADefValue;
  if Assigned(ARequest) then
  begin
    Field := ARequest.FindFieldSense(ASense);
    if Assigned(Field) then
    begin
      AValue := Field.Value;
      Result := True;
    end;
  end;
end;

function GetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  ADefValue: Variant): Variant;
begin
  GetFieldSenseValueDef(ARequest, ASense, ADefValue, Result);
end;

function SetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  AValue: Variant): Boolean;
var
  Field: IsmxField;
begin
  Result := False;
  if Assigned(ARequest) then
  begin
    Field := ARequest.FindFieldSense(ASense);
    if Assigned(Field) then
    begin
      Field.Value := AValue;
      Result := True;
    end;
  end;
end;

function GetParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  ADefValue: Variant; var AValue: Variant): Boolean;
var
  Param: IsmxParam;
begin
  Result := False;
  AValue := ADefValue;
  if Assigned(ARequest) then
  begin
    Param := ARequest.FindParamLocation(ALocation);
    if Assigned(Param) then
    begin
      AValue := Param.Value;
      Result := True;
    end;
  end;
end;

function GetParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  ADefValue: Variant): Variant;
begin
  GetParamLocationValueDef(ARequest, ALocation, ADefValue, Result);
end;

function SetParamLocationValueDef(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  AValue: Variant): Boolean;
var
  Param: IsmxParam;
begin
  Result := False;
  if Assigned(ARequest) then
  begin
    Param := ARequest.FindParamLocation(ALocation);
    if Assigned(Param) then
    begin
      Param.Value := AValue;
      Result := True;
    end;
  end;
end;

{function PerformRequest(ARequest: TsmxCustomRequest; ASame: Boolean = False): Boolean;
var
  res: Integer;
  msg: String;
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
          res := GetFieldSenseValueDef(ARequest, fsResult, 1);
          msg := GetFieldSenseValueDef(ARequest, fsMessage, '');
        end;
        dstStoredProc:
        begin
          res := GetParamLocationValueDef(ARequest, plResult, 1);
          msg := GetParamLocationValueDef(ARequest, plMessage, '');
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

function GetRequestKeyAndValue(ARequest: TsmxCustomRequest; var AKey, AValue: Variant): Boolean;
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
        Result := GetFieldSenseValueDef(ARequest, fsKey, Null, AKey);
        Result := GetFieldSenseValueDef(ARequest, fsValue, Null, AValue) and Result;
      end;
      dstStoredProc:
      begin
        Result := GetParamLocationValueDef(ARequest, plKey, Null, AKey);
        Result := GetParamLocationValueDef(ARequest, plValue, Null, AValue) and Result;
      end;
    end;
  end;
end;

function GetRequestValueByKey(ARequest: TsmxCustomRequest; AKey: Variant; var AValue: Variant): Boolean;
begin
  Result := False;
  AValue := Null;
  if not Assigned(ARequest) then
    Exit;
  if not Assigned(ARequest.Database) or not Assigned(ARequest.CellDataSet) then
    Exit;
  if SetParamLocationValueDef(ARequest, plKey, AKey) then
    with ARequest do
    begin
      Perform;
      case CellDataSet.DataSetType of
        dstQuery: Result := GetFieldSenseValueDef(ARequest, fsValue, Null, AValue);
        dstStoredProc: Result := GetParamLocationValueDef(ARequest, plValue, Null, AValue);
      end;
    end;
end;

function SetRequestValueByKey(ARequest: TsmxCustomRequest; AKey, AValue: Variant): Boolean;
begin
  Result := False;
  if not Assigned(ARequest) then
    Exit;
  if not Assigned(ARequest.Database) or not Assigned(ARequest.CellDataSet) then
    Exit;
  Result := SetParamLocationValueDef(ARequest, plKey, AKey) and
    SetParamLocationValueDef(ARequest, plValue, AValue);
  if Result then
    ARequest.Perform;
end;

end.
