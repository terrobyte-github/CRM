unit smxClassFuncs;

interface

uses
  Classes, smxBaseClasses, smxClasses, smxTypes, smxBaseIntf, smxDBIntf,
  smxRefIntf, smxBaseTypes;

function CfgIDToCfgClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfgClass;
function CfgIDToCellClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCellClass;
function CfgIDToIntfClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxInterfacedPersistentClass;
function NewCfg(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCfg;
function NewCell(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil): TsmxBaseCell;
function NewIntf(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): IsmxRefInterface;
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
function GetAlgorithmForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomAlgorithm; overload;
function GetAlgorithmForm(AForm: TsmxCustomForm; AEvent: TsmxComponentEvent): TsmxCustomAlgorithm; overload;
function GetEventForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxComponentEvent;
function GetRequestForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomRequest; overload;
function GetRequestForm(AForm: TsmxCustomForm; const ADataSet: IsmxDataSet): TsmxCustomRequest; overload;
function GetDataSetForm(AForm: TsmxCustomForm; ACfgID: Integer): IsmxDataSet;
function GetPopupMenuForm(AForm: TsmxCustomForm; ACfgID: Integer): TsmxCustomPopupMenu;

implementation

uses
  Variants, smxFuncs, smxConsts, smxClassProcs, smxProcs, smxDBTypes, smxDBFuncs,
  SysUtils;

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
        {ASelectRequest.PerformanceMode,} fsKey, fsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectRequest := ASelectRequest;
        //TypeCfg.Receive;
        TypeCfg.Load;
        TypeCfg.Read;
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
        {ASelectRequest.PerformanceMode,} fsKey, fsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectRequest := ASelectRequest;
        //TypeCfg.Receive;
        TypeCfg.Load;
        TypeCfg.Read;
        Result := TypeCfg.CellClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

function CfgIDToIntfClass(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): TsmxInterfacedPersistentClass;
var
  TypeCfg: TsmxTypeCfg;
  ForeignKey: Variant;
begin
  Result := nil;
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  if Assigned(ASelectRequest) then
    if smxDBFuncs.GetValueByKey(ASelectRequest.DataSet, ACfgID, ForeignKey,
        {ASelectRequest.PerformanceMode,} fsKey, fsForeignKey) then
    begin
      TypeCfg := TsmxTypeCfg.Create(nil);
      try
        TypeCfg.CfgID := ForeignKey;
        TypeCfg.SelectRequest := ASelectRequest;
        //TypeCfg.Receive;
        TypeCfg.Load;
        TypeCfg.Read;
        Result := TypeCfg.IntfClass;
      finally
        TypeCfg.Free;
      end;
    end;
end;

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
  IntfClass: TsmxInterfacedPersistentClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  CellClass := CfgIDToCellClass(ACfgID, ASelectRequest);
  if Assigned(CellClass) then
  begin
    IntfClass := CfgIDToIntfClass(ACfgID, ASelectRequest);
    if Assigned(IntfClass) then
      Result := CellClass.CreateByImpl(AOwner, IntfClass.Create as IsmxRefInterface)
    else
      Result := CellClass.Create(AOwner);
    Result.CfgID := ACfgID;
    //Result.SelectRequest := ASelectRequest;
    //Result.StorageManager := smxClassProcs.gStorageManagerIntf;
    //Result.LibraryManager := smxClassProcs.gLibraryManagerIntf;
    //Result.DatabaseManager := smxClassProcs.gDatabaseManagerIntf;
    //Result.FormManager := smxClassProcs.gFormManagerIntf;
    //Result.ImageListManager := smxClassProcs.gImageListManagerIntf;
  end else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create'], ACfgID);
end;

function NewIntf(ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil): IsmxRefInterface;
var
  IntfClass: TsmxInterfacedPersistentClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  IntfClass := CfgIDToIntfClass(ACfgID, ASelectRequest);
  if Assigned(IntfClass) then
    Result := IntfClass.Create as IsmxRefInterface
  else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create'], ACfgID);
end;

function NewForm(AOwner: TComponent; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil; AID: Integer = 0): TsmxCustomForm;
var
  CellClass: TsmxBaseCellClass;
  IntfClass: TsmxInterfacedPersistentClass;
begin
  if not Assigned(ASelectRequest) then
    ASelectRequest := smxClassProcs.gSelectRequest;
  CellClass := CfgIDToCellClass(ACfgID, ASelectRequest);
  if CellClass.InheritsFrom(TsmxCustomForm) then
  begin
    IntfClass := CfgIDToIntfClass(ACfgID, ASelectRequest);
    if AID = 0 then
    begin
      if Assigned(IntfClass) then
        Result := TsmxCustomFormClass(CellClass).CreateByImpl(AOwner, IntfClass.Create as IsmxRefInterface)
      else
        Result := TsmxCustomFormClass(CellClass).Create(AOwner);
    end else
    begin
      if Assigned(IntfClass) then
        Result := TsmxCustomFormClass(CellClass).CreateByID(AOwner, AID, IntfClass.Create as IsmxRefInterface)
      else
        Result := TsmxCustomFormClass(CellClass).CreateByID(AOwner, AID);
    end;
    Result.CfgID := ACfgID;
    //Result.SelectRequest := ASelectRequest;
    //Result.StorageManager := smxClassProcs.gStorageManagerIntf;
    //Result.LibraryManager := smxClassProcs.gLibraryManagerIntf;
    //Result.DatabaseManager := smxClassProcs.gDatabaseManagerIntf;
    Result.FormManager := smxClassProcs.gFormManagerIntf;
    //Result.ImageListManager := smxClassProcs.gImageListManagerIntf;
  end else
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellIDActionError,
      ['nil', ACfgID, 'create'], ACfgID);
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
    if foSetValue in TsmxCustomFilter(Cell).FilterOptions then
      AValue := TsmxCustomFilter(Cell).FilterValue
    else
      AValue := smxFuncs.StrToVar(TsmxCustomFilter(Cell).FilterCaption);
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
    if coSetValue in TsmxCustomColumn(Cell).ColumnOptions then
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

end.
