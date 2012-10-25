unit smxClassFuncs;

interface

uses
  Classes, smxClasses, smxTypes, smxBaseIntf, smxDBIntf;

function CfgIDToCfgClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfgClass;
function CfgIDToCellClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCellClass;
//function CfgIDToIntfClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxInterfacedComponentClass;
function NewCfg(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfg;
function NewCell(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCell;
//function NewIntf(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): IsmxBaseInterface;
function NewForm(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil; AID: Integer = 0): TsmxCustomForm;
function ExistsParent(ACell, ACellParent: TsmxBaseCell): Boolean;
function FindFilterOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
function FindColumnOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
function FindFilterOnForm(AForm: TsmxCustomForm; const AName: String;
  var AValue: Variant): Boolean;
function FindColumnOnForm(AForm: TsmxCustomForm; const AName: String;
  var AValue: Variant): Boolean;
function GetAccessoryForm(ACell: TsmxBaseCell): TsmxCustomForm;

implementation

uses
  Variants, smxFuncs, smxConsts, smxClassProcs, smxProcs, smxDBTypes, smxDBFuncs;

function CfgIDToCfgClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfgClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
begin
  Result := nil;
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  if Assigned(ASelectRequest) then
    if smxDBFuncs.GetValueByKey(ASelectRequest.DataSet, ACfgID, ForeignKey,
        ASelectRequest.PerformanceMode, fsKey, fsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := smxFuncs.GetSingleValue(ForeignKey, 0);
        TypeCfg.SelectRequest := ASelectRequest;
        TypeCfg.Receive;
        Result := TypeCfg.CfgClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

function CfgIDToCellClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCellClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
begin
  Result := nil;
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  if Assigned(ASelectRequest) then
    if smxDBFuncs.GetValueByKey(ASelectRequest.DataSet, ACfgID, ForeignKey,
        ASelectRequest.PerformanceMode, fsKey, fsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := smxFuncs.GetSingleValue(ForeignKey, 0);
        TypeCfg.SelectRequest := ASelectRequest;
        TypeCfg.Receive;
        Result := TypeCfg.CellClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

{function CfgIDToIntfClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxInterfacedComponentClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
begin
  Result := nil;
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.GSelectReques;
  if Assigned(ASelectRequest) then
    if smxDBFuncs.GetValueByKey(ASelectRequest.DataSet, ACfgID, ForeignKey,
        ASelectRequest.PerformanceMode, fsKey, fsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := smxFuncs.GetSingleValue(ForeignKey, 0);
        TypeCfg.SelectRequest := ASelectRequest;
        TypeCfg.Receive;
        Result := TypeCfg.IntfClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;}

function NewCfg(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfg;
var
  CfgClass: TsmxBaseCfgClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  CfgClass := CfgIDToCfgClass(ACfgID, ASelectRequest);
  if Assigned(CfgClass) then
  begin
    Result := CfgClass.Create(AOwner);
    Result.CfgID := ACfgID;
    Result.SelectRequest := ASelectRequest;
  end else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      ['nil', ACfgID, 'create'], ACfgID);
end;

function NewCell(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCell;
var
  CellClass: TsmxBaseCellClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  CellClass := CfgIDToCellClass(ACfgID, ASelectRequest);
  if Assigned(CellClass) then
  begin
    Result := CellClass.Create(AOwner);
    Result.CfgID := ACfgID;
    Result.SelectRequest := ASelectRequest;
  end else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellActionError,
      ['nil', 'create'], ACfgID);
end;

{function NewIntf(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): IsmxBaseInterface;
var
  IntfClass: TsmxInterfacedComponentClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.GSelectReques;
  IntfClass := CfgIDToIntfClass(ACfgID, ASelectRequest);
  if Assigned(IntfClass) then
    Result := IntfClass.Create as IsmxBaseInterface;
  else
    raise EsmxCfgError.CreateByCfgID(@SCfgActionError, ['nil', ACfgID, 'create'], ACfgID);
end;}

function NewForm(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil; AID: Integer = 0): TsmxCustomForm;
var
  CellClass: TsmxBaseCellClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  CellClass := CfgIDToCellClass(ACfgID, ASelectRequest);
  if CellClass.InheritsFrom(TsmxCustomForm) then
  begin
    if AID = 0 then
      Result := TsmxCustomFormClass(CellClass).Create(AOwner) else
      Result := TsmxCustomFormClass(CellClass).CreateByID(AOwner, AID);
    Result.CfgID := ACfgID;
    Result.SelectRequest := ASelectRequest;
  end else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellActionError,
      ['nil', 'create'], ACfgID);
end;

function ExistsParent(ACell, ACellParent: TsmxBaseCell): Boolean;
var
  List: TList;
  i: Integer;
begin
  Result := False;
  if not Assigned(ACell) or not Assigned(ACellParent) then
    Exit;
  List := TList.Create;
  try
    smxClassProcs.AllParents(ACell, List, []);
    for i := 0 to List.Count - 1 do
      if List[i] = ACellParent then
      begin
        Result := True;
        Break;
      end;
  finally
    List.Free;
  end;
end;

function FindFilterOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
var
  Filter: TsmxCustomFilter;
begin
  Result := False;
  AValue := Variants.Null;
  Filter := nil;
  if Assigned(ASection) then
    if Assigned(ASection.FilterDesk) then
      Filter := ASection.FilterDesk.FindFilterByName(AName);
  if Assigned(Filter) then
  begin
    AValue := Filter.FilterValue;
    Result := True;
  end;
end;

function FindColumnOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
var
  Column: TsmxCustomColumn;
begin
  Result := False;
  AValue := Variants.Null;
  Column := nil;
  if Assigned(ASection) then
    if Assigned(ASection.Grid) then
      Column := ASection.Grid.FindColumnByName(AName);
  if Assigned(Column) then
  begin
    AValue := Column.ColumnValue;
    Result := True;
  end;
end;

function FindFilterOnForm(AForm: TsmxCustomForm; const AName: String;
  var AValue: Variant): Boolean;
var
  i: Integer;
  List: TList;
begin
  Result := False;
  AValue := Variants.Null;
  List := TList.Create;
  try
    smxClassProcs.AllCells(AForm, List, [TsmxCustomSection]);
    for i := 0 to List.Count - 1 do
      if FindFilterOnSection(TsmxCustomSection(List[i]), AName, AValue) then
      begin
        Result := True;
        Break;
      end;
  finally
    List.Free;
  end;
end;

function FindColumnOnForm(AForm: TsmxCustomForm; const AName: String;
  var AValue: Variant): Boolean;
var
  i: Integer;
  List: TList;
begin
  Result := False;
  AValue := Variants.Null;
  List := TList.Create;
  try
    smxClassProcs.AllCells(AForm, List, [TsmxCustomSection]);
    for i := 0 to List.Count - 1 do
      if FindColumnOnSection(TsmxCustomSection(List[i]), AName, AValue) then
      begin
        Result := True;
        Break;
      end;
  finally
    List.Free;
  end;
end;

function GetAccessoryForm(ACell: TsmxBaseCell): TsmxCustomForm;
var
  Cell: TsmxBaseCell;
begin
  Result := nil;
  if not Assigned(ACell) then
    Exit;
  Cell := ACell.CellParent;
  while Assigned(Cell) and not Assigned(Result) do
  begin
    if Cell is TsmxCustomForm then
      Result := TsmxCustomForm(Cell);
    Cell := Cell.CellParent;
  end;
end;

end.
