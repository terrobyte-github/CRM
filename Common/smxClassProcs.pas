unit smxClassProcs;

interface

uses
  Classes, smxClasses, smxDBIntf;

procedure VarToParams(const AValue: Variant; AParams: TsmxParams);
procedure ParamsToVar(AParams: TsmxParams; var AValue: Variant);
procedure AllCells(ACell: TsmxBaseCell; AList: TList;
  AClassList: array of TsmxBaseCellClass; AIsActive: Boolean = False);
procedure AllParents(ACell: TsmxBaseCell; AList: TList;
  AClassList: array of TsmxBaseCellClass);

var
  gSelectRequest: TsmxCustomRequest = nil;

implementation

uses
  Variants;

procedure VarToParams(const AValue: Variant; AParams: TsmxParams);
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

procedure AllCells(ACell: TsmxBaseCell; AList: TList;
  AClassList: array of TsmxBaseCellClass; AIsActive: Boolean = False);

  function IsClass(ACurCell: TsmxBaseCell): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(AClassList) to High(AClassList) do
      if ACurCell is AClassList[i] then
        Result := True;
  end;

  function IsActive(ACurCell: TsmxBaseCell): Boolean;
  begin
    Result := True;
    if ACurCell is TsmxControlCell then
      Result := TsmxControlCell(ACurCell).CellActive;
  end;

  procedure AddChilds(ACurCell: TsmxBaseCell; AIsEmpty: Boolean);
  var
    i: Integer;
  begin
    if (AIsEmpty or IsClass(ACurCell)) and (not AIsActive or IsActive(ACurCell)) then
      AList.Add(ACurCell);
    for i := 0 to ACurCell.CellCount - 1 do
      AddChilds(ACurCell.Cells[i], AIsEmpty);
  end;

var
  i: Integer;
begin
  if not Assigned(AList) then
    Exit;
  AList.Clear;
  for i := 0 to ACell.CellCount - 1 do
    AddChilds(ACell.Cells[i], Length(AClassList) = 0);
end;

procedure AllParents(ACell: TsmxBaseCell; AList: TList;
  AClassList: array of TsmxBaseCellClass);

  function IsClass(ACurCell: TsmxBaseCell): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(AClassList) to High(AClassList) do
      if ACurCell is AClassList[i] then
        Result := True;
  end;

var
  Cell: TsmxBaseCell;
  Empty: Boolean;
begin
  if not Assigned(AList) then
    Exit;
  AList.Clear;
  Empty := Length(AClassList) = 0;
  Cell := ACell.CellParent;
  while Assigned(Cell) do
  begin
    if Empty or IsClass(Cell) then
      AList.Add(Cell);
    Cell := Cell.CellParent;
  end;
end;

end.
