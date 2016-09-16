{**************************************}
{                                      }
{            SalesMan v1.0             }
{         Cell class functions         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxClassFuncs;

interface

uses
  Classes, smxBaseClasses, smxClasses, smxTypes, smxBaseIntf, smxDBIntf,
  smxBaseTypes;

function CfgIDToCfgClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfgClass;
function CfgIDToCellClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCellClass;
function NewCfg(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfg;
function NewCell(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCell;
function NewForm(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil; AID: Integer = 0;
  AIsRegister: Boolean = False): TsmxCustomForm;
function ExistsParent(ACell, ACellParent: TsmxBaseCell): Boolean;
function GetAccessoryForm(ACell: TsmxBaseCell): TsmxCustomForm;
function RefreshAlgorithmParams(AAlgorithm: TsmxCustomAlgorithm;
  ADataLocationList: array of TsmxDataLocation): Boolean;
function RefreshRequstParams(ARequest: TsmxCustomRequest; const ADataSet: IsmxDataSet;
  ADataLocationList: array of TsmxDataLocation): Boolean;
function FindParentCellParam(ACell: TsmxBaseCell; const AName: String;
  var AValue: Variant): Boolean;

implementation

uses
  Variants, smxCfgs, smxFuncs, smxConsts, smxClassProcs, smxProcs, smxDBTypes,
  smxDBFuncs, smxManagerIntf;

function CfgIDToCfgClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfgClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
  DataSet: IsmxDataSet;
begin
  Result := nil;
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf
  else
    DataSet := ASelectDataSet;
  if Assigned(DataSet) then
    if smxDBFuncs.GetValueByKey(DataSet, ACfgID, ForeignKey, dsKey, dsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectDataSet := DataSet;
        TypeCfg.Load;
        TypeCfg.Read;
        Result := TypeCfg.CfgClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

function CfgIDToCellClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCellClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
  DataSet: IsmxDataSet;
begin
  Result := nil;
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf
  else
    DataSet := ASelectDataSet;
  if Assigned(DataSet) then
    if smxDBFuncs.GetValueByKey(DataSet, ACfgID, ForeignKey, dsKey, dsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectDataSet := DataSet;
        TypeCfg.Load;
        TypeCfg.Read;
        Result := TypeCfg.CellClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

function NewCfg(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfg;
var
  CfgClass: TsmxBaseCfgClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf
   else
    DataSet := ASelectDataSet;
  CfgClass := CfgIDToCfgClass(ACfgID, DataSet);
  if Assigned(CfgClass) then
  begin
    Result := CfgClass.Create(AOwner);
    Result.CfgID := ACfgID;
    Result.SelectDataSet := DataSet;
  end else
    raise EsmxCfgError.CreateResFmt(@smxConsts.rsCfgActionError,
      ['nil', ACfgID, 'create']);
end;

function NewCell(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCell;
var
  CellClass: TsmxBaseCellClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf
  else
    DataSet := ASelectDataSet;
  CellClass := CfgIDToCellClass(ACfgID, DataSet);
  if Assigned(CellClass) then
  begin
    Result := CellClass.Create(AOwner);
    Result.CfgID := ACfgID;
  end else
    raise EsmxCellError.CreateResFmt(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create']);
end;

function NewForm(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil; AID: Integer = 0;
  AIsRegister: Boolean = False): TsmxCustomForm;
var
  CellClass: TsmxBaseCellClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf
  else
    DataSet := ASelectDataSet;
  CellClass := CfgIDToCellClass(ACfgID, DataSet);
  if Assigned(CellClass) and CellClass.InheritsFrom(TsmxCustomForm) then
  begin
    Result := TsmxCustomFormClass(CellClass).Create(AOwner, AID);
    Result.CfgID := ACfgID;
    if AIsRegister and Assigned(smxProcs.gFormManagerIntf) then
      smxProcs.gFormManagerIntf.InsertFormControl(Result as IsmxFormControl);
  end else
    raise EsmxCellError.CreateResFmt(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create']);
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

function GetAccessoryForm(ACell: TsmxBaseCell): TsmxCustomForm;
begin
  Result := nil;
  while Assigned(ACell) do
  begin
    if ACell is TsmxCustomForm then
    begin
      Result := TsmxCustomForm(ACell);
      Break;
    end;
    ACell := ACell.CellParent;
  end;
end;

function RefreshCellParams(ACell: TsmxBaseCell; AParams: TsmxAlgorithmParams;
  ADataLocationList: array of TsmxDataLocation): Boolean;

  function IsDataLocation(ADataLocation: TsmxDataLocation): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if Length(ADataLocationList) = 0 then
      Result := True
    else
      for i := Low(ADataLocationList) to High(ADataLocationList) do
        if ADataLocation = ADataLocationList[i] then
        begin
          Result := True;
          Break;
        end;
  end;

  function FindEventParam(ACell: TsmxBaseCell; const AName: String;
    var AValue: Variant): Boolean;
  var
    Param: TsmxParam;
  begin
    Result := False;
    AValue := Variants.Null;
    if not Assigned(ACell) then
      Exit;
    Param := ACell.EventParams.FindByName(AName);
    if Assigned(Param) and (csEventParam in ACell.CellStates) then
      AValue := Param.ParamValue;
  end;

  function FindWorkParam(AForm: TsmxCustomForm; const AName: String;
    var AValue: Variant; AClassList: array of TsmxBaseCellClass): Boolean;
  var
    i: Integer;
    List: TList;
  begin
    Result := False;
    AValue := Variants.Null;
    if not Assigned(AForm) then
      Exit;
    List := TList.Create;
    try
      smxClassProcs.AllCells(AForm, List, AClassList, True);
      for i := 0 to List.Count - 1 do
        if TsmxWorkCell(List[i]).WorkParams(AName, AValue) then
        begin
          Result := True;
          Break;
        end;
    finally
      List.Free;
    end;
  end;

var
  Form: TsmxCustomForm;
  List: TList;
  i, j: Integer;
  Value: Variant;
begin
  Result := False;
  if not Assigned(ACell) or not Assigned(AParams) then
    Exit;

  Form := GetAccessoryForm(ACell);
  List := TList.Create;
  try
    for i := 0 to AParams.Count - 1 do
      if IsDataLocation(AParams[i].DataLocation) then
      begin
        Value := Variants.Null;

        case AParams[i].DataLocation of
          dlAssigned:
          begin
            Value := AParams[i].ParamValue;
          end;
          dlStorageParam:
          begin
            if Assigned(smxProcs.gStorageManagerIntf) then
              Value := smxProcs.gStorageManagerIntf.Values[AParams[i].ParamName];
          end;
          dlCellParam:
          begin
            ACell.CellParams(AParams[i].ParamName, Value);
          end;
          dlParentCellParam:
          begin
            List.Clear;
            smxClassProcs.AllParents(ACell, List, []);
            for j := 0 to List.Count - 1 do
              if TsmxBaseCell(List[j]).CellParams(AParams[i].ParamName, Value) then
                Break;
          end;
          dlEventParam:
          begin
            FindEventParam(ACell, AParams[i].ParamName, Value);
          end;
          dlParentEventParam:
          begin
            List.Clear;
            smxClassProcs.AllParents(ACell, List, []);
            for j := 0 to List.Count - 1 do
              if FindEventParam(TsmxBaseCell(List[j]), AParams[i].ParamName, Value) then
                Break;
          end;
          dlWorkCell:
          begin
            FindWorkParam(Form, AParams[i].ParamName, Value, [TsmxWorkCell]);
          end;
          dlParentWorkCell:
          begin
            List.Clear;
            smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
            for j := 0 to List.Count - 1 do
              if FindWorkParam(TsmxCustomForm(List[j]), AParams[i].ParamName, Value, [TsmxWorkCell]) then
                Break;
          end;
          dlFilterDesk:
          begin
            FindWorkParam(Form, AParams[i].ParamName, Value, [TsmxCustomFilterDesk]);
          end;
          dlParentFilterDesk:
          begin
            List.Clear;
            smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
            for j := 0 to List.Count - 1 do
              if FindWorkParam(TsmxCustomForm(List[j]), AParams[i].ParamName, Value, [TsmxCustomFilterDesk]) then
                Break;
          end;
          dlGrid:
          begin
            FindWorkParam(Form, AParams[i].ParamName, Value, [TsmxCustomGrid]);
          end;
          dlParentGrid:
          begin
            List.Clear;
            smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
            for j := 0 to List.Count - 1 do
              if FindWorkParam(TsmxCustomForm(List[j]), AParams[i].ParamName, Value, [TsmxCustomGrid]) then
                Break;
          end;
          dlTree:
          begin
            FindWorkParam(Form, AParams[i].ParamName, Value, [TsmxCustomTree]);
          end;
          dlParentTree:
          begin
            List.Clear;
            smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
            for j := 0 to List.Count - 1 do
              if FindWorkParam(TsmxCustomForm(List[j]), AParams[i].ParamName, Value, [TsmxCustomTree]) then
                Break;
          end;
        end;

        AParams[i].ParamValue := Value;
      end;
  finally
    List.Free;
  end;

  Result := True;
end;

function RefreshAlgorithmParams(AAlgorithm: TsmxCustomAlgorithm;
  ADataLocationList: array of TsmxDataLocation): Boolean;
var
  Cell: TsmxBaseCell;
begin
  Result := False;
  if not Assigned(AAlgorithm) then
    Exit;

  if Assigned(AAlgorithm.CellAlgorithm) then
    Cell := AAlgorithm.CellAlgorithm else
  if Assigned(AAlgorithm.CellEvent) then
    Cell := AAlgorithm.CellEvent
  else
    Cell := AAlgorithm;

  Result := RefreshCellParams(Cell, AAlgorithm.Params, ADataLocationList);
end;

function RefreshRequstParams(ARequest: TsmxCustomRequest; const ADataSet: IsmxDataSet;
  ADataLocationList: array of TsmxDataLocation): Boolean;

  procedure GetParams(AParams: TsmxAlgorithmParams);
  var
    i: Integer;
  begin
    if Assigned(ADataSet) then
      for i := 0 to ADataSet.ParamCount - 1 do
        with AParams.Add do
        begin
          DataLocation := ADataSet.Params[i].DataLocation;
          DataType := ADataSet.Params[i].DataType;
          ParamName := ADataSet.Params[i].ParamName;
          ParamType := ADataSet.Params[i].ParamType;
          ParamValue := ADataSet.Params[i].Value;
        end;
  end;

  procedure SetParams(AParams: TsmxAlgorithmParams);
  var
    i: Integer;
  begin
    if Assigned(ADataSet) then
      for i := 0 to ADataSet.ParamCount - 1 do
        ADataSet.Params[i].Value := AParams[i].ParamValue;
  end;

var
  Params: TsmxAlgorithmParams;
  Cell: TsmxBaseCell;
begin
  Result := False;
  if not Assigned(ARequest) or not Assigned(ADataSet) then
    Exit;

  if Assigned(ARequest.CellRequest) then
    Cell := ARequest.CellRequest
  else
    Cell := ARequest;

  Params := TsmxAlgorithmParams.Create(TsmxAlgorithmParam);
  try
    GetParams(Params);
    if RefreshCellParams(Cell, Params, ADataLocationList) then
    begin
      SetParams(Params);
      Result := True;
    end;
  finally
    Params.Free;
  end;
end;

function FindParentCellParam(ACell: TsmxBaseCell; const AName: String;
  var AValue: Variant): Boolean;
var
  List: TList;
  i: Integer;
begin
  Result := False;
  AValue := Variants.Null;
  if not Assigned(ACell) then
    Exit;

  List := TList.Create;
  try
    smxClassProcs.AllParents(ACell, List, [], True);
    for i := 0 to List.Count - 1 do
      if TsmxBaseCell(List[i]).CellParams(AName, AValue) then
      begin
        Result := True;
        Break;
      end;
  finally
    List.Free;
  end;
end;

end.
