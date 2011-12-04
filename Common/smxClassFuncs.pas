unit smxClassFuncs;

interface

uses
  Classes, smxClasses, smxTypes, smxDBIntf;

function CfgIDToCfgClass(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfgClass;
function CfgIDToCellClass(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCellClass;
function NewCfg(AOwner: TComponent; const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfg;
function NewCell(AOwner: TComponent; const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCell;
function NewForm(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AIntfID: Integer = 0; AID: Integer = 0): TsmxCustomForm;
//function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function GetParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant;
  var AValue: Variant): Boolean; overload;
function GetParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant; overload;
{function GetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
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
  AValue: Variant): Boolean;}
//function PerformRequest(ARequest: TsmxCustomRequest; ASame: Boolean = False): Boolean;
//function GetRequestKeyAndValue(ARequest: TsmxCustomRequest; var AKey, AValue: Variant): Boolean;
//function GetRequestValueByKey(ARequest: TsmxCustomRequest; AKey: Variant; var AValue: Variant): Boolean;
//function SetRequestValueByKey(ARequest: TsmxCustomRequest; AKey, AValue: Variant): Boolean;

//function GetParamLocationCount(AParams: TsmxLocationParams; ALocation: TsmxParamLocation): Integer;

function GetParamLocation(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation): TsmxLocationParam;
function GetParamLocationValue(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  var AValue: Variant): Boolean;
function SetParamLocationValue(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  AValue: Variant): Boolean;
function GetFieldSense(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense): TsmxSenseField;
function GetFieldSenseValue(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  var AValue: Variant): Boolean;
function SetFieldSenseValue(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  AValue: Variant): Boolean;
function GetRequestValueByKey(ARequest: TsmxCustomRequest; AKey: Variant;
  var AValue: Variant): Boolean;
function SetRequestValueByKey(ARequest: TsmxCustomRequest; var AKey: Variant;
  AValue: Variant): Boolean;
function GetRequestForeignKeyByKey(ARequest: TsmxCustomRequest; AKey: Variant;
  var AForeignKey: Variant): Boolean;
function SetRequestForeignKeyByKey(ARequest: TsmxCustomRequest; var AKey: Variant;
  AForeignKey: Variant): Boolean;

implementation

uses
  Variants, smxFuncs, smxConsts, smxClassProcs, smxProcs, smxDBTypes;

function CfgIDToCfgClass(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfgClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
begin
  Result := nil;
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.SelectRequest;
  if Assigned(ACfgDatabase) and Assigned(ASelectRequest) and (ACfgID > 0) then
    if GetRequestForeignKeyByKey(ASelectRequest, ACfgID, ForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgDatabase := ACfgDatabase;
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectRequest := ASelectRequest;
        TypeCfg.Receive;
        Result := TypeCfg.CfgClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

function CfgIDToCellClass(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCellClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
begin
  Result := nil;
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.SelectRequest;
  if Assigned(ACfgDatabase) and Assigned(ASelectRequest) and (ACfgID > 0) then
    if GetRequestForeignKeyByKey(ASelectRequest, ACfgID, ForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgDatabase := ACfgDatabase;
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectRequest := ASelectRequest;
        TypeCfg.Receive;
        Result := TypeCfg.CellClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

function NewCfg(AOwner: TComponent; const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfg;
var
  CfgClass: TsmxBaseCfgClass;
begin
  CfgClass := CfgIDToCfgClass(ACfgDatabase, ACfgID, ASelectRequest);
  if Assigned(CfgClass) then
  begin
    Result := CfgClass.Create(AOwner);
    Result.CfgDatabase := ACfgDatabase;
    Result.CfgID := ACfgID;
    if not Assigned(ASelectRequest) then
      ASelectRequest := smxClassProcs.SelectRequest;
    Result.SelectRequest := ASelectRequest;
  end else
    raise EsmxCfgError.CreateResFmt(@SCfgBuildError, [ACfgID]);
end;

function NewCell(AOwner: TComponent; const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCell;
var
  CellClass: TsmxBaseCellClass;
begin
  CellClass := CfgIDToCellClass(ACfgDatabase, ACfgID, ASelectRequest);
  if Assigned(CellClass) then
    Result := CellClass.Create(AOwner, ACfgDatabase, ACfgID)
  else
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [ACfgID]);
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
      Result := FormClass.Create(AOwner, ADatabase, ACfgID) else
      Result := FormClass.CreateByID(AOwner, AID);
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

{function GetFieldSenseValueDef(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
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
end;}

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

{function GetRequestKeyAndValue(ARequest: TsmxCustomRequest; var AKey, AValue: Variant): Boolean;
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
end;}

{function GetParamLocationCount(AParams: TsmxLocationParams; ALocation: TsmxParamLocation): Integer;
var
  i: Integer;
begin
  Result := 0;
  if not Assigned(AParams) then
    Exit;
  for i := 0 to AParams.Count - 1 do
    if AParams[i].ParamLocation = ALocation then
      Inc(Result);
end;}

function GetParamLocation(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation): TsmxLocationParam;
var
  Params: TsmxLocationParamArray;
begin
  Result := nil;
  Params := nil;
  if Assigned(ARequest) then
    if ARequest.RequestParams.FindByLocation(ALocation, Params) > 0 then
    begin
      Result := Params[0];
      SetLength(Params, 0);
    end;
end;

function GetParamLocationValue(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  var AValue: Variant): Boolean;
var
  Param: TsmxLocationParam;
  //Str: String;
begin
  Result := False;
  AValue := Variants.Null;
  Param := GetParamLocation(ARequest, ALocation);
  if Assigned(Param) then
  begin
    AValue := ARequest.ParamValue[Param.ParamName];
    Result := True;
  end;
end;

function SetParamLocationValue(ARequest: TsmxCustomRequest; ALocation: TsmxParamLocation;
  AValue: Variant): Boolean;
var
  Param: TsmxLocationParam;
begin
  Result := False;
  Param := GetParamLocation(ARequest, ALocation);
  if Assigned(Param) then
  begin
    ARequest.ParamValue[Param.ParamName] := AValue;
    Result := True;
  end;
end;

function GetFieldSense(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense): TsmxSenseField;
var
  Fields: TsmxSenseFieldArray;
begin
  Result := nil;
  Fields := nil;
  if Assigned(ARequest) then
    if ARequest.RequestFields.FindBySense(ASense, Fields) > 0 then
    begin
      Result := Fields[0];
      SetLength(Fields, 0);
    end;
end;

function GetFieldSenseValue(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  var AValue: Variant): Boolean;
var
  Field: TsmxSenseField;
  //Str: String;
begin
  Result := False;
  AValue := Variants.Null;
  Field := GetFieldSense(ARequest, ASense);
  if Assigned(Field) then
  begin
    AValue := ARequest.FieldValue[Field.FieldName];
    Result := True;
  end;
end;

function SetFieldSenseValue(ARequest: TsmxCustomRequest; ASense: TsmxFieldSense;
  AValue: Variant): Boolean;
var
  Field: TsmxSenseField;
begin
  Result := False;
  Field := GetFieldSense(ARequest, ASense);
  if Assigned(Field) then
  begin
    ARequest.FieldValue[Field.FieldName] := AValue;
    Result := True;
  end;
end;

function GetRequestValueByKey(ARequest: TsmxCustomRequest; AKey: Variant;
  var AValue: Variant): Boolean;
begin
  Result := False;
  if SetParamLocationValue(ARequest, plKey, AKey) then
  begin
    ARequest.Perform;
    case ARequest.DataSetType of
      dstQuery: Result := GetFieldSenseValue(ARequest, fsValue, AValue);
      dstStoredProc: Result := GetParamLocationValue(ARequest, plValue, AValue);
    end;
  end;
end;

function SetRequestValueByKey(ARequest: TsmxCustomRequest; var AKey: Variant;
  AValue: Variant): Boolean;
begin
  Result := False;
  if SetParamLocationValue(ARequest, plKey, AKey) and
      SetParamLocationValue(ARequest, plValue, AValue) then
  begin
    ARequest.Perform;
    case ARequest.DataSetType of
      dstQuery: Result := GetFieldSenseValue(ARequest, fsKey, AKey);
      dstStoredProc: Result := GetParamLocationValue(ARequest, plKey, AKey);
    end;
  end;
end;

function GetRequestForeignKeyByKey(ARequest: TsmxCustomRequest; AKey: Variant;
  var AForeignKey: Variant): Boolean;
begin
  Result := False;
  if SetParamLocationValue(ARequest, plKey, AKey) then
  begin
    ARequest.Perform;
    case ARequest.DataSetType of
      dstQuery: Result := GetFieldSenseValue(ARequest, fsForeignKey, AForeignKey);
      dstStoredProc: Result := GetParamLocationValue(ARequest, plForeignKey, AForeignKey);
    end;
  end;
end;

function SetRequestForeignKeyByKey(ARequest: TsmxCustomRequest; var AKey: Variant;
  AForeignKey: Variant): Boolean;
begin
  Result := False;
  if SetParamLocationValue(ARequest, plKey, AKey) and
      SetParamLocationValue(ARequest, plForeignKey, AForeignKey) then
  begin
    ARequest.Perform;
    case ARequest.DataSetType of
      dstQuery: Result := GetFieldSenseValue(ARequest, fsKey, AKey);
      dstStoredProc: Result := GetParamLocationValue(ARequest, plKey, AKey);
    end;
  end;
end;

end.
