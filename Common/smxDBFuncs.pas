unit smxDBFuncs;

interface

uses
  smxDBIntf, smxDBTypes, smxTypes;

function FindFieldBySense(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  var AFields: TsmxFieldArray): Integer;
function GetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const ADefValues: Variant; var AValues: Variant): Boolean;
function SetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const AValues: Variant): Boolean;
function FindParamBySense(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  var AParams: TsmxParamArray): Integer;
function GetParamSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const ADefValues: Variant; var AValues: Variant): Boolean;
function SetParamSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const AValues: Variant): Boolean;
{function FindParamByLocation(const ADataSet: IsmxDataSet; ALocation: TsmxDataLocation;
  var AParams: TsmxParamArray): Integer;
function GetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxDataLocation;
  const ADefValues: Variant; var AValues: Variant): Boolean;
function SetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxDataLocation;
  const AValues: Variant): Boolean;}
function GetValueByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  var AValues: Variant; {APerformance: TsmxPerformanceMode;}
  AKeySense: TsmxDataSense = dsKey; AValueSense: TsmxDataSense = dsValue): Boolean;
function SetValueByKey(const ADataSet: IsmxDataSet; var AKeys: Variant;
  const AValues: Variant; {APerformance: TsmxPerformanceMode;}
  AKeySense: TsmxDataSense = dsKey; AValueSense: TsmxDataSense = dsValue): Boolean;
function GetFieldNameList(const ADataSet: IsmxDataSet; ASense: TsmxDataSense): String;
function LocateByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  AKeySense: TsmxDataSense = dsKey): Boolean;
function GetCurrentValue(const ADataSet: IsmxDataSet; var AValues: Variant;
  {APerformance: TsmxPerformanceMode;} AValueSense: TsmxDataSense = dsValue): Boolean;
function SetCurrentValue(const ADataSet: IsmxDataSet; const AValues: Variant;
  {APerformance: TsmxPerformanceMode;} AValueSense: TsmxDataSense = dsValue): Boolean;
function IsBlobType(ADataType: TsmxDataType; ALength: Integer = 0): Boolean;
function DataTypeToVarType(ADataType: TsmxDataType): Integer;
function SetNumberOfRecord(const ADataSet: IsmxDataSet; ARecordNo: Integer): Boolean;

implementation

uses
  Variants, DB;

{const
  SenseToLocation: Array[TsmxDataSense] of TsmxDataLocation =
    (plKey, plValue, plResult, plMessage, plForeignKey);}

function FindFieldBySense(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  var AFields: TsmxFieldArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AFields, 0);
  if Assigned(ADataSet) then
    for i := 0 to ADataSet.FieldCount - 1 do
      if ADataSet.Fields[i].DataSense = ASense then
      begin
        Inc(Result);
        SetLength(AFields, Result);
        AFields[Result - 1] := ADataSet.Fields[i];
      end;
end;

function GetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const ADefValues: Variant; var AValues: Variant): Boolean;
var
  Fields: TsmxFieldArray;
  i, Count: Integer;
begin
  Result := False;
  AValues := ADefValues;
  Count := FindFieldBySense(ADataSet, ASense, Fields);
  if Count = 1 then
  begin
    AValues := Fields[0].Value;
    Result := True;
  end else
  if Count > 1 then
  begin
    AValues := Variants.VarArrayCreate([0, Count - 1], varVariant);
    for i := 0 to Count - 1 do
      AValues[i] := Fields[i].Value;
    Result := True;
  end;
end;

function SetFieldSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const AValues: Variant): Boolean;
var
  Fields: TsmxFieldArray;
  i, Count: Integer;
begin
  Result := False;
  Count := FindFieldBySense(ADataSet, ASense, Fields);
  if not Variants.VarIsArray(AValues) then
  begin
    if Count = 1 then
    begin
      ADataSet.Edit;
      Fields[0].Value := AValues;
      ADataSet.Post;
      Result := True;
    end
  end else
  if Variants.VarArrayHighBound(AValues, 1) = Count - 1 then
  begin
    ADataSet.Edit;
    for i := 0 to Count - 1 do
      Fields[i].Value := AValues[i];
    ADataSet.Post;
    Result := True;
  end;
end;

function FindParamBySense(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  var AParams: TsmxParamArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AParams, 0);
  if Assigned(ADataSet) then
    for i := 0 to ADataSet.ParamCount - 1 do
      if ADataSet.Params[i].DataSense = ASense then
      begin
        Inc(Result);
        SetLength(AParams, Result);
        AParams[Result - 1] := ADataSet.Params[i];
      end;
end;

function GetParamSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const ADefValues: Variant; var AValues: Variant): Boolean;
var
  Params: TsmxParamArray;
  i, Count: Integer;
begin
  Result := False;
  AValues := ADefValues;
  Count := FindParamBySense(ADataSet, ASense, Params);
  if Count = 1 then
  begin
    AValues := Params[0].Value;
    Result := True;
  end else
  if Count > 1 then
  begin
    AValues := Variants.VarArrayCreate([0, Count - 1], varVariant);
    for i := 0 to Count - 1 do
      AValues[i] := Params[i].Value;
    Result := True;
  end;
end;

function SetParamSenseValue(const ADataSet: IsmxDataSet; ASense: TsmxDataSense;
  const AValues: Variant): Boolean;
var
  Params: TsmxParamArray;
  i, Count: Integer;
begin
  Result := False;
  Count := FindParamBySense(ADataSet, ASense, Params);
  if not Variants.VarIsArray(AValues) then
  begin
    if Count = 1 then
    begin
      Params[0].Value := AValues;
      Result := True;
    end
  end else
  if Variants.VarArrayHighBound(AValues, 1) = Count - 1 then
  begin
    for i := 0 to Count - 1 do
      Params[i].Value := AValues[i];
    Result := True;
  end;
end;

{function FindParamByLocation(const ADataSet: IsmxDataSet; ALocation: TsmxDataLocation;
  var AParams: TsmxParamArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AParams, 0);
  if Assigned(ADataSet) then
    for i := 0 to ADataSet.ParamCount - 1 do
      if ADataSet.Params[i].DataLocation = ALocation then
      begin
        Inc(Result);
        SetLength(AParams, Result);
        AParams[Result - 1] := ADataSet.Params[i];
      end;
end;

function GetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxDataLocation;
  const ADefValues: Variant; var AValues: Variant): Boolean;
var
  Params: TsmxParamArray;
  i, Count: Integer;
begin
  Result := False;
  AValues := ADefValues;
  Count := FindParamByLocation(ADataSet, ALocation, Params);
  if Count = 1 then
  begin
    AValues := Params[0].Value;
    Result := True;
  end else
  if Count > 1 then
  begin
    AValues := Variants.VarArrayCreate([0, Count - 1], varVariant);
    for i := 0 to Count - 1 do
      AValues[i] := Params[i].Value;
    Result := True;
  end;
end;

function SetParamLocationValue(const ADataSet: IsmxDataSet; ALocation: TsmxDataLocation;
  const AValues: Variant): Boolean;
var
  Params: TsmxParamArray;
  i, Count: Integer;
begin
  Result := False;
  Count := FindParamByLocation(ADataSet, ALocation, Params);
  if not Variants.VarIsArray(AValues) then
    if Count = 1 then
    begin
      Params[0].Value := AValues;
      Result := True;
    end
  else
    if Variants.VarArrayHighBound(AValues, 1) = Count - 1 then
    begin
      for i := 0 to Count - 1 do
        Params[i].Value := AValues[i];
      Result := True;
    end;
end;}

function GetValueByKey(const ADataSet: IsmxDataSet; const AKeys: Variant;
  var AValues: Variant; {APerformance: TsmxPerformanceMode;}
  AKeySense: TsmxDataSense = dsKey; AValueSense: TsmxDataSense = dsValue): Boolean;
begin
  Result := False;
  AValues := Variants.Null;
  if Assigned(ADataSet) then
    ADataSet.Close;
  if SetParamSenseValue(ADataSet, {SenseToLocation[}AKeySense{]}, AKeys) then
  begin
    case ADataSet.PerformanceMode of
      pmOpen:
      begin
        ADataSet.Open;
        Result := GetFieldSenseValue(ADataSet, AValueSense, AValues, AValues);
      end;
      pmExecute:
      begin
        ADataSet.Execute;
        Result := GetParamSenseValue(ADataSet, {SenseToLocation[}AValueSense{]}, AValues, AValues);
      end;
    end;
  end;
end;

function SetValueByKey(const ADataSet: IsmxDataSet; var AKeys: Variant;
  const AValues: Variant; {APerformance: TsmxPerformanceMode;}
  AKeySense: TsmxDataSense = dsKey; AValueSense: TsmxDataSense = dsValue): Boolean;
begin
  Result := False;
  if Assigned(ADataSet) then
    ADataSet.Close;
  if SetParamSenseValue(ADataSet, {SenseToLocation[}AKeySense{]}, AKeys)
      and SetParamSenseValue(ADataSet, {SenseToLocation[}AValueSense{]}, AValues) then
  begin
    case ADataSet.PerformanceMode of
      pmOpen:
      begin
        ADataSet.Open;
        Result := GetFieldSenseValue(ADataSet, AKeySense, AKeys, AKeys);
      end;
      pmExecute:
      begin
        ADataSet.Execute;
        Result := GetParamSenseValue(ADataSet, {SenseToLocation[}AKeySense{]}, AKeys, AKeys);
      end;
    end;
  end;
end;

function GetFieldNameList(const ADataSet: IsmxDataSet; ASense: TsmxDataSense): String;
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
  AKeySense: TsmxDataSense = dsKey): Boolean;
begin
  Result := ADataSet.Locate(GetFieldNameList(ADataSet, AKeySense), AKeys);
end;

function GetCurrentValue(const ADataSet: IsmxDataSet; var AValues: Variant;
  {APerformance: TsmxPerformanceMode;} AValueSense: TsmxDataSense = dsValue): Boolean;
begin
  Result := False;
  if Assigned(ADataSet) then
    case ADataSet.PerformanceMode of
      pmOpen: Result := GetFieldSenseValue(ADataSet, AValueSense, AValues, AValues);
      pmExecute: Result := GetParamSenseValue(ADataSet, {SenseToLocation[}AValueSense{]}, AValues, AValues);
    end;
end;

function SetCurrentValue(const ADataSet: IsmxDataSet; const AValues: Variant;
  {APerformance: TsmxPerformanceMode;} AValueSense: TsmxDataSense = dsValue): Boolean;
begin
  Result := False;
  if Assigned(ADataSet) then
    case ADataSet.PerformanceMode of
      pmOpen: Result := SetFieldSenseValue(ADataSet, AValueSense, AValues);
      pmExecute: Result := SetParamSenseValue(ADataSet, {SenseToLocation[}AValueSense{]}, AValues);
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

function SetNumberOfRecord(const ADataSet: IsmxDataSet; ARecordNo: Integer): Boolean;
var
  CurRecordNo: Integer;
  RecordNo: Integer;
begin
  Result := False;
  if not Assigned(ADataSet) then
    Exit;
  if ADataSet.Active then
  begin
    CurRecordNo := ADataSet.RecordNo;
    if (CurRecordNo = -1) and (ADataSet.RecordCount <> 0) then
    begin
      RecordNo := 1;
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        if RecordNo = ARecordNo then
        begin
          Result := True;
          Break;
        end;
        Inc(RecordNo);
        ADataSet.Next;
      end;
    end else
    begin
      if ADataSet.RecordCount > ARecordNo then
      begin
        ADataSet.RecordNo := ARecordNo;
        Result := True;
      end;
    end;
  end;
end;

end.
