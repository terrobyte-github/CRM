unit smxDBFuncs;

interface

uses
  smxDBIntf, smxDBTypes, smxTypes;

function FindFieldBySense(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  var AFields: TsmxFieldArray): Integer;
function GetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  const ADefValues: Variant; var AValues: Variant): Boolean;
function SetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  const AValues: Variant): Boolean;
function FindParamByLocation(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  var AParams: TsmxParamArray): Integer;
function GetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  const ADefValues: Variant; var AValues: Variant): Boolean;
function SetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  const AValues: Variant): Boolean;
function GetValueByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  var AValues: Variant; APerformance: TsmxPerformanceMode;
  AKeySense: TsmxBaseSense = fsKey; AValueSense: TsmxBaseSense = fsValue): Boolean;
function SetValueByKey(const ADataSet: IsmxDataSet; var AKeys: Variant;
  const AValues: Variant; APerformance: TsmxPerformanceMode;
  AKeySense: TsmxBaseSense = fsKey; AValueSense: TsmxBaseSense = fsValue): Boolean;
function GetFieldNameList(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense): String;
function LocateByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  AKeySense: TsmxBaseSense = fsKey): Boolean;
function GetCurrentValue(const ADataSet: IsmxDataSet; var AValues: Variant;
  APerformance: TsmxPerformanceMode; AValueSense: TsmxBaseSense = fsValue): Boolean;
function SetCurrentValue(const ADataSet: IsmxDataSet; const AValues: Variant;
  APerformance: TsmxPerformanceMode; AValueSense: TsmxBaseSense = fsValue): Boolean;
function IsBlobType(ADataType: TsmxDataType; ALength: Integer = 0): Boolean;
function DataTypeToVarType(ADataType: TsmxDataType): Integer;

implementation

uses
  Variants, DB;

const
  SenseToLocation: Array[TsmxBaseSense] of TsmxParamLocation =
    (plKey, plValue, plResult, plMessage, plForeignKey);

function FindFieldBySense(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  var AFields: TsmxFieldArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AFields, 0);
  if Assigned(ADataSet) then
    for i := 0 to ADataSet.FieldCount - 1 do
      if ADataSet.Fields[i].FieldSense = ASense then
      begin
        Inc(Result);
        SetLength(AFields, Result);
        AFields[Result - 1] := ADataSet.Fields[i];
      end;
end;

function GetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  const ADefValues: Variant; var AValues: Variant): Boolean;
var
  Fields: TsmxFieldArray;
  i, Count: Integer;
begin
  Result := False;
  AValues := ADefValues;
  Count := FindFieldBySense(ADataSet, ASense, Fields);
  if Count > 0 then
  begin
    AValues := Variants.VarArrayCreate([0, Count - 1], varVariant);
    for i := 0 to Count - 1 do
      AValues[i] := Fields[i].Value;
    Result := True;
  end;
end;

function SetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  const AValues: Variant): Boolean;
var
  Fields: TsmxFieldArray;
  i, Count: Integer;
begin
  Result := False;
  Count := FindFieldBySense(ADataSet, ASense, Fields);
  if Variants.VarIsArray(AValues) then
    if Variants.VarArrayHighBound(AValues, 1) = Count - 1 then
    begin
      ADataSet.Edit;
      for i := 0 to Count - 1 do
        Fields[i].Value := AValues[i];
      ADataSet.Post;
      Result := True;
    end;
end;

function FindParamByLocation(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  var AParams: TsmxParamArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AParams, 0);
  if Assigned(ADataSet) then
    for i := 0 to ADataSet.ParamCount - 1 do
      if ADataSet.Params[i].ParamLocation = ALocation then
      begin
        Inc(Result);
        SetLength(AParams, Result);
        AParams[Result - 1] := ADataSet.Params[i];
      end;
end;

function GetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  const ADefValues: Variant; var AValues: Variant): Boolean;
var
  Params: TsmxParamArray;
  i, Count: Integer;
begin
  Result := False;
  AValues := ADefValues;
  Count := FindParamByLocation(ADataSet, ALocation, Params);
  if Count > 0 then
  begin
    AValues := Variants.VarArrayCreate([0, Count - 1], varVariant);
    for i := 0 to Count - 1 do
      AValues[i] := Params[i].Value;
    Result := True;
  end;
end;

function SetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  const AValues: Variant): Boolean;
var
  Params: TsmxParamArray;
  i, Count: Integer;
begin
  Result := False;
  Count := FindParamByLocation(ADataSet, ALocation, Params);
  if Variants.VarIsArray(AValues) then
    if Variants.VarArrayHighBound(AValues, 1) = Count - 1 then
    begin
      for i := 0 to Count - 1 do
        Params[i].Value := AValues[i];
      Result := True;
    end;
end;

function GetValueByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  var AValues: Variant; APerformance: TsmxPerformanceMode;
  AKeySense: TsmxBaseSense = fsKey; AValueSense: TsmxBaseSense = fsValue): Boolean;
begin
  Result := False;
  AValues := Variants.Null;
  if Assigned(ADataSet) then
    ADataSet.Close;
  if SetParamLocationValue(ADataSet, SenseToLocation[AKeySense], AKeys) then
  begin
    case APerformance of
      pmOpen:
      begin
        ADataSet.Open;
        Result := GetFieldSenseValue(ADataSet, AValueSense, AValues, AValues);
      end;
      pmExecute:
      begin
        ADataSet.Execute;
        Result := GetParamLocationValue(ADataSet, SenseToLocation[AValueSense], AValues, AValues);
      end;
    end;
  end;
end;

function SetValueByKey(const ADataSet: IsmxDataSet; var AKeys: Variant;
  const AValues: Variant; APerformance: TsmxPerformanceMode;
  AKeySense: TsmxBaseSense = fsKey; AValueSense: TsmxBaseSense = fsValue): Boolean;
begin
  Result := False;
  if Assigned(ADataSet) then
    ADataSet.Close;
  if SetParamLocationValue(ADataSet, SenseToLocation[AKeySense], AKeys)
      and SetParamLocationValue(ADataSet, SenseToLocation[AValueSense], AValues) then
  begin
    case APerformance of
      pmOpen:
      begin
        ADataSet.Open;
        Result := GetFieldSenseValue(ADataSet, AKeySense, AKeys, AKeys);
      end;
      pmExecute:
      begin
        ADataSet.Execute;
        Result := GetParamLocationValue(ADataSet, SenseToLocation[AKeySense], AKeys, AKeys);
      end;
    end;
  end;
end;

function GetFieldNameList(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense): String;
var
  Fields: TsmxFieldArray;
  i, Count: Integer;
begin
  Result := '';
  Count := FindFieldBySense(ADataSet, ASense, Fields);
  for i := 0 to Count - 1 do
    Result := Result + Fields[i].FieldName + ';';
end;

function LocateByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  AKeySense: TsmxBaseSense = fsKey): Boolean;
begin
  Result := ADataSet.Locate(GetFieldNameList(ADataSet, AKeySense), AKeys);
end;

function GetCurrentValue(const ADataSet: IsmxDataSet; var AValues: Variant;
  APerformance: TsmxPerformanceMode; AValueSense: TsmxBaseSense = fsValue): Boolean;
begin
  Result := False;
  if Assigned(ADataSet) then
    case APerformance of
      pmOpen: Result := GetFieldSenseValue(ADataSet, AValueSense, AValues, AValues);
      pmExecute: Result := GetParamLocationValue(ADataSet, SenseToLocation[AValueSense], AValues, AValues);
    end;
end;

function SetCurrentValue(const ADataSet: IsmxDataSet; const AValues: Variant;
  APerformance: TsmxPerformanceMode; AValueSense: TsmxBaseSense = fsValue): Boolean;
begin
  Result := False;
  if Assigned(ADataSet) then
    case APerformance of
      pmOpen: Result := SetFieldSenseValue(ADataSet, AValueSense, AValues);
      pmExecute: Result := SetParamLocationValue(ADataSet, SenseToLocation[AValueSense], AValues);
    end;
end;

function IsBlobType(ADataType: TsmxDataType; ALength: Integer = 0): Boolean;
begin
  Result := ((ADataType in [ftString, ftFixedChar]) and (ALength > 255)) or
    (ADataType in [ftBlob .. ftTypedBinary, ftOraBlob, ftOraClob]);
end;

function DataTypeToVarType(ADataType: TsmxDataType): Integer;
const
  VarTypeMap: array[TFieldType] of Integer = (varError, varOleStr, varSmallint,
    varInteger, varSmallint, varBoolean, varDouble, varCurrency, varCurrency,
    varDate, varDate, varDate, varOleStr, varOleStr, varInteger, varOleStr,
    varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varError,
    varOleStr, varOleStr, varError, varError, varError, varError, varError,
    varOleStr, varOleStr, varVariant, varUnknown, varDispatch, varOleStr, varOleStr, varOleStr);
begin
  Result := VarTypeMap[ADataType];
end;

end.
