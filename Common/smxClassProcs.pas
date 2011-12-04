unit smxClassProcs;

interface

uses
  Classes, smxClasses, smxDBIntf;

//procedure RegistrationClasses(AClasses: array of TsmxComponent);
procedure VarToParams(AValue: Variant; AParams: TsmxParams);
procedure ParamsToVar(AParams: TsmxParams; var AValue: Variant);

var
  //SelectRequestCfg: TsmxRequestCfg = nil;
  SelectRequest: TsmxCustomRequest = nil;
//function SelectRequest: TsmxCustomRequest;
//function DataStream: TStream;

implementation

uses
  Variants;

//var
  //_SelectRequest: TsmxCustomRequest = nil;
  //_DataStream: TStream = nil;

{function SelectRequest: TsmxCustomRequest;
begin
  Result := _SelectRequest;
end;}

{function DataStream: TStream;
begin
  Result := _DataStream;
end;}

{procedure RegistrationClasses(AClasses: array of TsmxComponent);
var i: Integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    RegisterClass(TPersistentClass(AClasses[i]));
end;}

procedure VarToParams(AValue: Variant; AParams: TsmxParams);
var
  i: Integer;
begin
  if not Assigned(AParams) then
    Exit;
  AParams.Clear;
  if Variants.VarIsArray(AValue) and (Variants.VarArrayHighBound(AValue, 1) = 1) then
    if Variants.VarIsArray(AValue[0]) and Variants.VarIsArray(AValue[1]) and
        (Variants.VarArrayHighBound(AValue[0], 1) = Variants.VarArrayHighBound(AValue[1], 1)) then
    begin
      for i := 0 to Variants.VarArrayHighBound(AValue[0], 1) do
        with AParams.Add do
        begin
          ParamName := AValue[0][i];
          ParamValue := AValue[1][i];
        end;
    end;
end;

procedure ParamsToVar(AParams: TsmxParams; var AValue: Variant);
var
  i: Integer;
  v1, v2: Variant;
begin
  if not Assigned(AParams) then
    Exit;
  AValue := Variants.Null;
  if AParams.Count > 0 then
  begin
    AValue := Variants.VarArrayCreate([0, 1], varVariant);
    v1 := Variants.VarArrayCreate([0, AParams.Count - 1], varVariant);
    v2 := Variants.VarArrayCreate([0, AParams.Count - 1], varVariant);
    for i := 0 to AParams.Count - 1 do
    begin
      v1[i] := AParams.Items[i].ParamName;
      v2[i] := AParams.Items[i].ParamValue;
    end;
    AValue[0] := v1;
    AValue[1] := v2;
  end;
end;

//initialization
  //_DataStream := TMemoryStream.Create;

//finalization
  //_DataStream.Free;

end.
