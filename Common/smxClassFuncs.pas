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
function FindFilterOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
function FindColumnOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
function FindFilterOnForm(AForm: TsmxCustomForm; const AName: String;
  var AValue: Variant): Boolean;
function FindColumnOnForm(AForm: TsmxCustomForm; const AName: String;
  var AValue: Variant): Boolean;
function GetAccessoryForm(ACell: TsmxBaseCell): TsmxCustomForm;
function GetAlgorithmForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomAlgorithm; overload;
function GetAlgorithmForm(AForm: TsmxCustomForm; AEvent: TsmxComponentEvent): TsmxCustomAlgorithm; overload;
function GetEventForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxComponentEvent;
function GetRequestForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomRequest; overload;
function GetRequestForm(AForm: TsmxCustomForm; const ADataSet: IsmxDataSet): TsmxCustomRequest; overload;
function GetDataSetForm(AForm: TsmxCustomForm; ACfgID: Integer): IsmxDataSet;
function GetPopupMenuForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomPopupMenu;

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

function FindFilterOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
var
  Cell: TsmxOwnerCell;
  i: Integer;
begin
  Result := False;
  AValue := Variants.Null;
  Cell := nil;
  if Assigned(ASection) then
    for i := 0 to ASection.SlaveCount - 1 do
      if ASection.Slaves[i] is TsmxCustomFilterDesk then
      begin
        Cell := ASection.Slaves[i].FindSlaveByName(AName);
        if Assigned(Cell) then
          Break;
      end;
  if Assigned(Cell) then
  begin
    AValue := TsmxCustomFilter(Cell).Value;
    Result := True;
  end;
end;

function FindColumnOnSection(ASection: TsmxCustomSection; const AName: String;
  var AValue: Variant): Boolean;
var
  Cell: TsmxOwnerCell;
  i: Integer;
begin
  Result := False;
  AValue := Variants.Null;
  Cell := nil;
  if Assigned(ASection) then
    for i := 0 to ASection.SlaveCount - 1 do
      if ASection.Slaves[i] is TsmxCustomGrid then
      begin
        Cell := ASection.Slaves[i].FindSlaveByName(AName);
        if Assigned(Cell) then
          Break;
      end;
  if Assigned(Cell) then
  begin
    AValue := TsmxCustomColumn(Cell).Value;
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
  if not Assigned(AForm) then
    Exit;
  List := TList.Create;
  try
    smxClassProcs.AllCells(AForm, List, [TsmxCustomSection], AForm.Visible);
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
  if not Assigned(AForm) then
    Exit;
  List := TList.Create;
  try
    smxClassProcs.AllCells(AForm, List, [TsmxCustomSection], AForm.Visible);
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

function GetAlgorithmForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomAlgorithm;
begin
  Result := nil;
  if Assigned(AForm) and (ACfgID <> 0) then
    if Assigned(AForm.AlgorithmList) then
      Result := TsmxCustomAlgorithm(AForm.AlgorithmList.FindSlaveByCfgID(ACfgID));
end;

function GetEventForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxComponentEvent;
var
  Algorithm: TsmxCustomAlgorithm;
begin
  Algorithm := GetAlgorithmForm(AForm, ACfgID);
  if Assigned(Algorithm) then
    Result := Algorithm.OnExecute
  else
    Result := nil;
end;

function GetRequestForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomRequest;
begin
  Result := nil;
  if Assigned(AForm) and (ACfgID <> 0) then
    if Assigned(AForm.RequestList) then
      Result := TsmxCustomRequest(AForm.RequestList.FindSlaveByCfgID(ACfgID));
end;

function GetDataSetForm(AForm: TsmxCustomForm; ACfgID: Integer): IsmxDataSet;
var
  Request: TsmxCustomRequest;
begin
  Request := GetRequestForm(AForm, ACfgID);
  if Assigned(Request) then
    Result := Request.DataSet
  else
    Result := nil;
end;

function GetPopupMenuForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomPopupMenu;
begin
  Result := nil;
  if Assigned(AForm) and (ACfgID <> 0) then
    if Assigned(AForm.PopupList) then
      Result := TsmxCustomPopupMenu(AForm.PopupList.FindSlaveByCfgID(ACfgID));
end;

function GetAlgorithmForm(AForm: TsmxCustomForm; AEvent: TsmxComponentEvent): TsmxCustomAlgorithm;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(AForm) and Assigned(AEvent) then
    if Assigned(AForm.AlgorithmList) then
      for i := 0 to AForm.AlgorithmList.SlaveCount - 1 do
        if @AForm.AlgorithmList[i].OnExecute = @AEvent then
          Result := AForm.AlgorithmList[i];
end;

function GetRequestForm(AForm: TsmxCustomForm; const ADataSet: IsmxDataSet): TsmxCustomRequest;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(AForm) and Assigned(ADataSet) then
    if Assigned(AForm.RequestList) then
      for i := 0 to AForm.RequestList.SlaveCount - 1 do
        if AForm.RequestList[i].DataSet = ADataSet then
          Result := AForm.RequestList[i];
end;

end.
