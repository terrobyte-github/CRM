unit smxDBFuncs;

interface

uses
  smxDBIntf, smxDBTypes, smxTypes;

{function GetParamsLocationCountDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation): Integer;
function GetParamsLocationDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  AParams: TsmxParamArray): Boolean;
function GetParamLocationDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation): IsmxParam;
function GetParamLocationValueDefDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  ADevValue: Variant; var AValue: Variant): Boolean; overload;
function GetParamLocationValueDefDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  ADevValue: Variant): Variant; overload;
function SetParamLocationValueDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  AValue: Variant): Boolean;
function GetFieldsSenseCountDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense): Integer;
function GetFieldsSenseDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  AFields: TsmxFieldArray): Boolean;
function GetFieldSenseDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense): IsmxField;
function GetFieldSenseValueDefDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  ADevValue: Variant; var AValue: Variant): Boolean; overload;
function GetFieldSenseValueDefDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  ADevValue: Variant): Variant; overload;
function GetValueByKeyDS(ADataSet: IsmxDataSet; AKey: Variant;
  var AValue: Variant; AMode: TsmxPerformanceMode = pmOpen): Boolean;
function SetValueByKeyDS(ADataSet: IsmxDataSet; var AKey: Variant;
  AValue: Variant; AMode: TsmxPerformanceMode = pmExecute): Boolean;}

implementation

uses
  Variants;

{function GetParamsLocationCountDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation): Integer;
var
  i: Integer;
begin
  Result := 0;
  if not Assigned(ADataSet) then
    Exit;
  for i := 0 to ADataSet.ParamCount - 1 do
    if ADataSet.Params[i].ParamLocation = ALocation then
      Inc(Result);
end;

function GetParamsLocationDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  AParams: TsmxParamArray): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  j := GetParamsLocationCountDS(ADataSet, ALocation);
  SetLength(AParams, j);
  if j > 0 then
  begin
    j := 0;
    for i := 0 to ADataSet.ParamCount - 1 do
      if ADataSet.Params[i].ParamLocation = ALocation then
      begin
        AParams[j] := ADataSet.Params[i];
        Inc(j);
      end;
    Result := True;
  end;
end;

function GetParamLocationDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation): IsmxParam;
var
  Params: TsmxParamArray;
begin
  Result := nil;
  Params := nil;
  if GetParamsLocationDS(ADataSet, ALocation, Params) then
  begin
    Result := Params[0];
    SetLength(Params, 0);
  end;
end;

function GetParamLocationValueDefDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  ADevValue: Variant; var AValue: Variant): Boolean;
var
  Param: IsmxParam;
begin
  Result := False;
  AValue := ADevValue;
  Param := GetParamLocationDS(ADataSet, ALocation);
  if Assigned(Param) then
  begin
    AValue := Param.Value;
    Result := True;
  end;
end;

function GetParamLocationValueDefDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  ADevValue: Variant): Variant;
begin
  GetParamLocationValueDefDS(ADataSet, ALocation, ADevValue, Result);
end;

function SetParamLocationValueDS(const ADataSet: IsmxDataSet; ALocation: TsmxParamLocation;
  AValue: Variant): Boolean;
var
  Param: IsmxParam;
begin
  Result := False;
  Param := GetParamLocationDS(ADataSet, ALocation);
  if Assigned(Param) then
  begin
    Param.Value := AValue;
    Result := True;
  end;
end;

function GetFieldsSenseCountDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense): Integer;
var
  i: Integer;
begin
  Result := 0;
  if not Assigned(ADataSet) then
    Exit;
  for i := 0 to ADataSet.FieldCount - 1 do
    if ADataSet.Fields[i].FieldSense = ASense then
      Inc(Result);
end;

function GetFieldsSenseDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  AFields: TsmxFieldArray): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  j := GetFieldsSenseCountDS(ADataSet, ASense);
  SetLength(AFields, j);
  if j > 0 then
  begin
    j := 0;
    for i := 0 to ADataSet.FieldCount - 1 do
      if ADataSet.Fields[i].FieldSense = ASense then
      begin
        AFields[j] := ADataSet.Fields[i];
        Inc(j);
      end;
    Result := True;
  end;
end;

function GetFieldSenseDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense): IsmxField;
var
  Fields: TsmxFieldArray;
begin
  Result := nil;
  Fields := nil;
  if GetFieldsSenseDS(ADataSet, ASense, Fields) then
  begin
    Result := Fields[0];
    SetLength(Fields, 0);
  end;
end;

function GetFieldSenseValueDefDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  ADevValue: Variant; var AValue: Variant): Boolean;
var
  Field: IsmxField;
begin
  Result := False;
  AValue := ADevValue;
  Field := GetFieldSenseDS(ADataSet, ASense);
  if Assigned(Field) then
  begin
    AValue := Field.Value;
    Result := True;
  end;
end;

function GetFieldSenseValueDefDS(const ADataSet: IsmxDataSet; ASense: TsmxFieldSense;
  ADevValue: Variant): Variant;
begin
  GetFieldSenseValueDefDS(ADataSet, ASense, ADevValue, Result);
end;

function GetValueByKeyDS(ADataSet: IsmxDataSet; AKey: Variant;
  var AValue: Variant; AMode: TsmxPerformanceMode = pmOpen): Boolean;
begin
  Result := False;
  AValue := Variants.Null;
  if SetParamLocationValueDS(ADataSet, plKey, AKey) then
  begin
    case AMode of
      pmOpen: ADataSet.Open;
      pmExecute: ADataSet.Execute;
    end;
    case ADataSet.DataSetType of
      dstQuery: Result := GetFieldSenseValueDefDS(ADataSet, fsValue, AValue, AValue);
      dstStoredProc: Result := GetParamLocationValueDefDS(ADataSet, plValue, AValue, AValue);
    end;
  end;
end;

function SetValueByKeyDS(ADataSet: IsmxDataSet; var AKey: Variant;
  AValue: Variant; AMode: TsmxPerformanceMode = pmExecute): Boolean;
begin
  Result := SetParamLocationValueDS(ADataSet, plKey, AKey) and
    SetParamLocationValueDS(ADataSet, plValue, AValue);
  if Result then
  begin
    case AMode of
      pmOpen: ADataSet.Open;
      pmExecute: ADataSet.Execute;
    end;
    case ADataSet.DataSetType of
      dstQuery: GetFieldSenseValueDefDS(ADataSet, fsKey, AKey, AKey);
      dstStoredProc: GetParamLocationValueDefDS(ADataSet, plKey, AKey, AKey);
    end;
  end;
end;}

end.
