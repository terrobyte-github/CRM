unit smxClassFuncs;

interface

uses
  Classes, smxBaseClasses, smxClasses, smxTypes, smxBaseIntf, smxDBIntf,
  smxBaseTypes;

function CfgIDToCfgClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfgClass;
function CfgIDToCellClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCellClass;
//function CfgIDToIntfClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxInterfacedPersistentClass;
function NewCfg(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfg;
function NewCell(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCell;
//function NewIntf(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): IsmxRefPersistent;
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
//function GetCellClassName(ACell: TsmxBaseCell): String;

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
    DataSet := smxClassProcs.gSelectDataSetIntf else
    DataSet := ASelectDataSet;
  if Assigned(DataSet) then
    if smxDBFuncs.GetValueByKey(DataSet, ACfgID, ForeignKey,
        {ASelectRequest.PerformanceMode,} dsKey, dsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        //TypeCfg.SelectRequest := ASelectRequest;
        TypeCfg.SelectDataSet := DataSet;
        //TypeCfg.Receive;
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
    DataSet := smxClassProcs.gSelectDataSetIntf else
    DataSet := ASelectDataSet;
  if Assigned(DataSet) then
    if smxDBFuncs.GetValueByKey(DataSet, ACfgID, ForeignKey,
        {ASelectRequest.PerformanceMode,} dsKey, dsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectDataSet := DataSet;
        //TypeCfg.Receive;
        TypeCfg.Load;
        TypeCfg.Read;
        Result := TypeCfg.CellClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

(*function CfgIDToIntfClass(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): TsmxInterfacedPersistentClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
  DataSet: IsmxDataSet;
begin
  Result := nil;
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gCfgSelectDataSet else
    DataSet := ASelectDataSet;
  if Assigned(DataSet) then
    if smxDBFuncs.GetValueByKey(DataSet, ACfgID, ForeignKey,
        {ASelectRequest.PerformanceMode,} dsKey, dsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectDataSet := DataSet;
        //TypeCfg.Receive;
        TypeCfg.Load;
        TypeCfg.Read;
        Result := TypeCfg.IntfClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;*)

function NewCfg(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil): TsmxBaseCfg;
var
  CfgClass: TsmxBaseCfgClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf else
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
  //IntfClass: TsmxInterfacedPersistentClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf else
    DataSet := ASelectDataSet;
  CellClass := CfgIDToCellClass(ACfgID, DataSet);
  if Assigned(CellClass) then
  begin
    //IntfClass := CfgIDToIntfClass(ACfgID, DataSet);
    //if Assigned(IntfClass) then
      Result := CellClass.Create(AOwner{, IntfClass});
    //else
      //Result := CellClass.Create(AOwner);
    Result.CfgID := ACfgID;
    //Result.SelectRequest := ASelectRequest;
    //Result.StorageManager := smxClassProcs.gStorageManagerIntf;
    //Result.LibraryManager := smxClassProcs.gLibraryManagerIntf;
    //Result.DatabaseManager := smxClassProcs.gDatabaseManagerIntf;
    //Result.FormManager := smxClassProcs.gFormManagerIntf;
    //Result.ImageListManager := smxClassProcs.gImageListManagerIntf;
  end else
    raise EsmxCellError.CreateResFmt(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create']);
end;

{function NewIntf(ACfgID: Integer; const ASelectDataSet: IsmxDataSet = nil): IsmxRefPersistent;
var
  IntfClass: TsmxInterfacedPersistentClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gCfgSelectDataSet else
    DataSet := ASelectDataSet;
  IntfClass := CfgIDToIntfClass(ACfgID, DataSet);
  if Assigned(IntfClass) then
    Result := IntfClass.Create as IsmxRefPersistent
  else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create'], ACfgID);
end;}

function NewForm(AOwner: TComponent; ACfgID: Integer;
  const ASelectDataSet: IsmxDataSet = nil; AID: Integer = 0;
  AIsRegister: Boolean = False): TsmxCustomForm;
var
  CellClass: TsmxBaseCellClass;
  //IntfClass: TsmxInterfacedPersistentClass;
  DataSet: IsmxDataSet;
begin
  if not Assigned(ASelectDataSet) then
    DataSet := smxClassProcs.gSelectDataSetIntf else
    DataSet := ASelectDataSet;
  CellClass := CfgIDToCellClass(ACfgID, DataSet);
  if Assigned(CellClass) and CellClass.InheritsFrom(TsmxCustomForm) then
  begin
    //IntfClass := CfgIDToIntfClass(ACfgID, DataSet);
    Result := TsmxCustomFormClass(CellClass).Create(AOwner{, IntfClass}, AID);
    {if AID = 0 then
    begin
      if Assigned(IntfClass) then
        Result := TsmxCustomFormClass(CellClass).CreateByImpl(AOwner, IntfClass.Create as IsmxRefPersistent)
      else
        Result := TsmxCustomFormClass(CellClass).Create(AOwner);
    end else
    begin
      if Assigned(IntfClass) then
        Result := TsmxCustomFormClass(CellClass).CreateByID(AOwner, AID, IntfClass.Create as IsmxRefPersistent)
      else
        Result := TsmxCustomFormClass(CellClass).CreateByID(AOwner, AID);
    end;}
    Result.CfgID := ACfgID;
    //Result.SelectRequest := ASelectRequest;
    //Result.StorageManager := smxClassProcs.gStorageManagerIntf;
    //Result.LibraryManager := smxClassProcs.gLibraryManagerIntf;
    //Result.DatabaseManager := smxClassProcs.gDatabaseManagerIntf;

    if AIsRegister and Assigned(smxProcs.gFormManagerIntf) then
      smxProcs.gFormManagerIntf.InsertFormControl(Result as IsmxFormControl);
      //Result.FormManager := smxProcs.gFormManagerIntf;

    //Result.ImageListManager := smxClassProcs.gImageListManagerIntf;
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
    if foHasValue in TsmxCustomFilter(Cell).FilterOptions then
      AValue := TsmxCustomFilter(Cell).FilterValue
    else
      AValue := smxFuncs.StrToVar(TsmxCustomFilter(Cell).FilterText);
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
    //if Assigned(ASection.Grid) then
      //Cell := ASection.Grid.FindSlaveByName(AName);
  if Assigned(Cell) then
  begin                                  
    if coHasValue in TsmxCustomColumn(Cell).ColumnOptions then
      AValue := TsmxCustomColumn(Cell).ColumnValue
    else
      AValue := smxFuncs.StrToVar(TsmxCustomColumn(Cell).ColumnCaption);
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
    smxClassProcs.AllCells(AForm, List, [TsmxCustomSection], AForm.CellVisible);
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
    smxClassProcs.AllCells(AForm, List, [TsmxCustomSection], AForm.CellVisible);
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
//var
  //Cell: TsmxBaseCell;
begin
  Result := nil;
  //if not Assigned(ACell) then
  //  Exit;
  //Cell := ACell.CellParent;
  while Assigned(ACell) {and not Assigned(Result)} do
  begin
    if ACell is TsmxCustomForm then
    begin
      Result := TsmxCustomForm(ACell);
      Break;
    end;
    {if Assigned(ACell.CellParent) then
      ACell := ACell.CellParent else
    if ACell is TsmxOwnerCell then
      ACell := TsmxOwnerCell(ACell).CellOwner else
      ACell := nil;}
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
    Result := Algorithm.OnExecute else
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
    Result := Request.DataSet else
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

{function GetCellClassName(ACell: TsmxBaseCell): String;
begin
  Result := '';
  if (not Assigned(smxProcs.gClassTypeManagerIntf))
      or (Assigned(smxProcs.gClassTypeManagerIntf)
        and not smxProcs.gClassTypeManagerIntf.ResolvedClassType(TPersistentClass(ACell.ClassType), Result)) then
    Result := ACell.ClassName;
end;

function GetCellClassType(const ACellClassTypeName: String): TsmxBaseCellClass;
var
  AClass: TPersistentClass;
begin
  Result := nil;
  if (not Assigned(smxProcs.gClassTypeManagerIntf))
      or (Assigned(smxProcs.gClassTypeManagerIntf)
        and not smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(ACellClassTypeName, AClass)) then
    AClass := Classes.GetClass(ACellClassTypeName);
  if Assigned(AClass) then
    if AClass.InheritsFrom(TsmxBaseCell) then
      Result := TsmxBaseCellClass(AClass);
end;}

end.
