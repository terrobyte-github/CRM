{**************************************}
{                                      }
{            SalesMan v1.0             }
{     VirtualTree cell algorithms      }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Аleksandr          }
{                                      }
{**************************************}

unit smxVTAlgs;

interface

uses
  smxBaseClasses;

procedure ShowObjectProps(Sender: TsmxComponent);

implementation

uses
  Classes, Controls, Windows, Messages, TypInfo, Variants, StdCtrls, SysUtils,
  VirtualTrees, smxClasses, smxCfgs, smxStdCtrls, smxFuncs, smxProcs,
  smxClassProcs, smxClassFuncs, smxTypes, smxBaseIntf, smxBaseTypes,
  smxManagerIntf, smxConsts, smxCells, smxDBFuncs;

type
  TsmxObjectPage = (opProperties, opEvents, opTreeView);

  TsmxObjectPages = set of TsmxObjectPage;

const
  cAllObjectPages = [Low(TsmxObjectPage) .. High(TsmxObjectPage)];
  cObjectInspectorPages = cAllObjectPages - [opTreeView];
  cObjectTreeViewPages = [opTreeView];

procedure AddValue(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
  ACol: Integer; ARow: Pointer; AEditorType: TsmxEditorType);
var
  V: Variant;
begin
  V := Variants.VarArrayOf([Integer(Pointer(AObject)),
    Integer(Pointer(APropInfo)), Integer(AEditorType)]);
  ATree.TreeValues[ACol, ARow] := V;
end;

procedure AddObjectProps(AObject: TObject; ATree: TsmxCustomTree; AParentRow: Pointer);

  procedure AddClass(APropInfo: PPropInfo; ARow: Pointer);
  var
    Cls: TClass;
    Obj: TObject;
  begin
    // добавление контрола для редактирования
    Cls := TypInfo.GetTypeData(APropInfo^.PropType^)^.ClassType;
    Obj := TypInfo.GetObjectProp(AObject, APropInfo);
    if Cls.InheritsFrom(TsmxKit) then
    begin
      // add ...
      AddValue(AObject, APropInfo, ATree, 1, ARow, etButtonString);
      if Assigned(Obj) then
        ATree.TreeTexts[1, ARow] := Format('(%s)', [Obj.ClassName]);
    end else
    if Cls.InheritsFrom(TsmxBaseCell) then
    begin
      if Assigned(APropInfo^.SetProc) then
      begin
        // add box
        AddValue(AObject, APropInfo, ATree, 1, ARow, etPickString);
        if Assigned(Obj) then
          ATree.TreeTexts[1, ARow] := TsmxBaseCell(Obj).Name;
      end else
      begin
        // add blank
        AddValue(AObject, APropInfo, ATree, 1, ARow, etNone);
        if Assigned(Obj) then
        begin
          ATree.TreeTexts[1, ARow] := Format('(%s)', [Obj.ClassName]);
          AddObjectProps(Obj, ATree, ARow);
        end;
      end;
    end else
    begin
      // add blank
      AddValue(AObject, APropInfo, ATree, 1, ARow, etNone);
      if Assigned(Obj) then
      begin
        ATree.TreeTexts[1, ARow] := Format('(%s)', [Obj.ClassName]);
        AddObjectProps(Obj, ATree, ARow);
      end;
    end;
  end;

  procedure AddIntf(APropInfo: PPropInfo; ARow: Pointer);
  var
    Intf: IInterface;
    GUID: TGUID;
  begin
    GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
    Intf := TypInfo.GetInterfaceProp(AObject, APropInfo);
    if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent) then
    begin
      if Assigned(APropInfo^.SetProc) then
      begin
        AddValue(AObject, APropInfo, ATree, 1, ARow, etPickString);
        if SysUtils.Supports(Intf, IsmxRefComponent) then
          ATree.TreeTexts[1, ARow] := IsmxRefComponent(Intf).GetReference.Name;
      end else
      begin
        if SysUtils.Supports(Intf, IsmxRefComponent) then
        begin
          AddValue(AObject, APropInfo, ATree, 1, ARow, etNone);
          ATree.TreeTexts[1, ARow] := Format('(%s)', [IsmxRefComponent(Intf).GetReference.ClassName]);
          AddObjectProps(IsmxRefComponent(Intf).GetReference, ATree, ARow);
        end;
      end;
    end else
    if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefPersistent) then
    begin
      if SysUtils.Supports(Intf, IsmxRefPersistent) then
      begin
        AddValue(AObject, APropInfo, ATree, 1, ARow, etNone);
        ATree.TreeTexts[1, ARow] := Format('(%s)', [IsmxRefPersistent(Intf).GetReference.ClassName]);
        AddObjectProps(IsmxRefPersistent(Intf).GetReference, ATree, ARow);
      end;
    end;
  end;

  procedure AddEnumeration(APropInfo: PPropInfo; ARow: Pointer);
  begin
    AddValue(AObject, APropInfo, ATree, 1, ARow, etPickString);
    ATree.TreeTexts[1, ARow] := TypInfo.GetEnumProp(AObject, APropInfo);
  end;

  procedure AddSet(APropInfo: PPropInfo; ARow: Pointer);
  var
    SetStr, EnumStr: String;
    TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    i: Integer;
  begin
    AddValue(AObject, APropInfo, ATree, 1, ARow, etNone);
    SetStr := TypInfo.GetSetProp(AObject, APropInfo, True);
    ATree.TreeTexts[1, ARow] := SetStr;
    TypeInfo := TypInfo.GetTypeData(APropInfo^.PropType^)^.CompType^;
    TypeData := TypInfo.GetTypeData(TypeInfo);
    ATree.RowCount[ARow] := TypeData^.MaxValue - TypeData^.MinValue + 1;
    for i := TypeData^.MinValue to TypeData^.MaxValue do
    begin
      AddValue(AObject, APropInfo, ATree, 1, ATree.Rows[ARow, i], etPickString);
      EnumStr := TypInfo.GetEnumName(TypeInfo, i);
      ATree.TreeTexts[0, ATree.Rows[ARow, i]] := EnumStr;
      ATree.TreeTexts[1, ATree.Rows[ARow, i]] := SysUtils.BoolToStr(Pos(EnumStr, SetStr) > 0, True);
    end;
  end;

var
  PropList: PPropList;
  Count: Integer;
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AObject) then
    Exit;
  Count := TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo), TypInfo.tkProperties, nil);
  if Count <> 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo), TypInfo.tkProperties, PropList);
      ATree.RowCount[AParentRow] := Count;
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        ATree.TreeTexts[0, ATree.Rows[AParentRow, i]] := PropInfo^.Name;
        case PropInfo^.PropType^^.Kind of
          tkClass:
            AddClass(PropInfo, ATree.Rows[AParentRow, i]);
          tkInterface:
            AddIntf(PropInfo, ATree.Rows[AParentRow, i]);
          tkEnumeration:
            AddEnumeration(PropInfo, ATree.Rows[AParentRow, i]);
          tkSet:
            AddSet(PropInfo, ATree.Rows[AParentRow, i]);
          else
          begin
            AddValue(AObject, PropInfo, ATree, 1, ATree.Rows[AParentRow, i], etString);
            ATree.TreeTexts[1, ATree.Rows[AParentRow, i]] :=
              Variants.VarToStr(TypInfo.GetPropValue(AObject, PropInfo^.Name));
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure AddObjectEvents(AObject: TObject; ATree: TsmxCustomTree; AParentRow: Pointer);

  procedure AddMethod(APropInfo: PPropInfo; ARow: Pointer);
  var
    Obj: TObject;
    Method: TMethod;
  begin
    Method := TypInfo.GetMethodProp(AObject, APropInfo);
    AddValue(AObject, APropInfo, ATree, 1, ARow, etPickString);
    Obj := TObject(Method.Data);
    if Obj is TsmxComponent then
    begin
      AddValue(AObject, APropInfo, ATree, 1, ARow, etPickString);
      ATree.TreeTexts[1, ARow] :=
        Format('%s%s%s', [TsmxComponent(Obj).Name, smxConsts.cDelimiterObjAndMethName, TsmxComponent(Obj).MethodName(Method.Code)]);
    end;
  end;

var
  PropList: PPropList;
  Count: Integer;
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AObject) then
    Exit;
  Count := TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo), TypInfo.tkMethods, nil);
  if Count <> 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo), TypInfo.tkMethods, PropList);
      ATree.RowCount[AParentRow] := Count;
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        ATree.TreeTexts[0, ATree.Rows[AParentRow, i]] := PropInfo^.Name;
        AddMethod(PropInfo, ATree.Rows[AParentRow, i]);
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure AddCells(AObject: TObject; ATree: TsmxCustomTree; AParentRow: Pointer;
  AIsAddSelf: Boolean = False);
var
  Cell: TsmxBaseCell;
  i: Integer;
begin
  if AObject is TsmxBaseCell then
  begin
    Cell := TsmxBaseCell(AObject);
    if AIsAddSelf then
    begin
      ATree.RowCount[AParentRow] := 1;
      ATree.TreeTexts[0, ATree.Rows[AParentRow, 0]] := Format('%s(%s) [%d]', [Cell.Name, Cell.ClassName, Cell.CfgID]);
      ATree.TreeValues[0, ATree.Rows[AParentRow, 0]] := Integer(Cell);
      AParentRow := ATree.Rows[AParentRow, 0];
    end;
    ATree.RowCount[AParentRow] := Cell.CellCount;
    for i := 0 to Cell.CellCount - 1 do
    begin
      ATree.TreeTexts[0, ATree.Rows[AParentRow, i]] := Format('%s(%s) [%d]', [Cell.Cells[i].Name, Cell.Cells[i].ClassName, Cell.Cells[i].CfgID]);
      ATree.TreeValues[0, ATree.Rows[AParentRow, i]] := Integer(Cell.Cells[i]);
      if Cell.Cells[i].CellCount > 0 then
        AddCells(Cell.Cells[i], ATree, ATree.Rows[AParentRow, i]);
    end;
  end;
end;

function GetForm(ACfgID, AID: Integer): TsmxCustomForm;
var
  FormIntf: IsmxFormControl;
begin
  FormIntf := smxProcs.gFormManagerIntf.FindByComboID(ACfgID, AID);
  if Assigned(FormIntf) then
    Result := TsmxCustomForm(FormIntf.GetReference)
  else
    Result := nil;
end;

procedure RefreshFormObjectProps(AObject: TObject; AObjectPages: TsmxObjectPages);
var
  FormObjectProps: TsmxCustomForm;
  Tree: TsmxCustomTree;
begin
  FormObjectProps := GetForm(0, smxConsts.cFormObjectPropsID);
  if Assigned(FormObjectProps) then
  begin
    if opProperties in AObjectPages then
    begin
      Tree := TsmxCustomTree(FormObjectProps.Cells[0].Cells[0].Cells[0].Cells[0].Cells[0]);
      Tree.RowCount[Tree.RootRow] := 0;
      if Assigned(AObject) then
      begin
        Tree.Slaves[0].Options := Tree.Slaves[0].Options + [coEditing];
        AddObjectProps(AObject, Tree, Tree.RootRow);
        Tree.Slaves[0].Options := Tree.Slaves[0].Options - [coEditing];
      end;
    end;

    if opEvents in AObjectPages then
    begin
      Tree := TsmxCustomTree(FormObjectProps.Cells[0].Cells[0].Cells[0].Cells[1].Cells[0]);
      Tree.RowCount[Tree.RootRow] := 0;
      if Assigned(AObject) then
      begin
        Tree.Slaves[0].Options := Tree.Slaves[0].Options + [coEditing];
        AddObjectEvents(AObject, Tree, Tree.RootRow);
        Tree.Slaves[0].Options := Tree.Slaves[0].Options - [coEditing];
      end;
    end;

    if opTreeView in AObjectPages then
    begin
      Tree := TsmxCustomTree(FormObjectProps.Cells[0].Cells[1].Cells[1]);
      Tree.RowCount[Tree.RootRow] := 0;
      if Assigned(AObject) then
      begin
        Tree.Slaves[0].Options := Tree.Slaves[0].Options + [coEditing];
        AddCells(AObject, Tree, Tree.RootRow, True);
        Tree.Slaves[0].Options := Tree.Slaves[0].Options - [coEditing];
      end;
    end;
  end;
end;

function GetObjectFormObjectProps: TObject;
var
  FormObjectProps: TsmxCustomForm;
  Tree: TsmxCustomTree;
  V: Variant;
begin
  Result := nil;
  FormObjectProps := GetForm(0, smxConsts.cFormObjectPropsID);
  if Assigned(FormObjectProps) then
  begin
    Tree := TsmxCustomTree(FormObjectProps.Cells[0].Cells[0].Cells[0].Cells[0].Cells[0]);
    if Tree.RowCount[Tree.RootRow] <> 0 then
    begin
      V := Tree.TreeValues[1, Tree.Rows[Tree.RootRow, 0]];
      if Variants.VarIsArray(V) then
        Result := TObject(Integer(V[0]));
    end;
  end;
end;

function GetObjectFormTreeView: TObject;
var
  FormObjectProps: TsmxCustomForm;
  Tree: TsmxCustomTree;
begin
  Result := nil;
  FormObjectProps := GetForm(0, smxConsts.cFormObjectPropsID);
  if Assigned(FormObjectProps) then
  begin
    Tree := TsmxCustomTree(FormObjectProps.Cells[0].Cells[1].Cells[0]);
    if Tree.RowCount[Tree.RootRow] <> 0 then
      Result := TObject(Integer(Tree.TreeValues[0, Tree.Rows[Tree.RootRow, 0]]));
  end;
end;

procedure ActiveFormCellView(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
  NewObj: TObject;
  CurObj: TObject;
begin
  if Sender is TsmxCustomForm then
  begin
    Form := TsmxCustomForm(Sender);
    if Form.CellCount > 0 then
      NewObj := Form.Cells[0]
    else
      NewObj := nil;
    CurObj := GetObjectFormObjectProps;
    if CurObj <> NewObj then
      RefreshFormObjectProps(NewObj, cAllObjectPages);
  end;
end;

procedure DeactiveFormCellView(Sender: TsmxComponent);
begin
  //Inf('DeactiveFormCellView');
end;

procedure CloseFormCellView(Sender: TsmxComponent);
var
  CurObj: TObject;
begin
  CurObj := GetObjectFormObjectProps;
  if CurObj is TsmxBaseCell then
    TsmxBaseCell(CurObj).Finalize;
  RefreshFormObjectProps(nil, cAllObjectPages);
end;

function CreateFormCellView(AFormParent: TsmxCustomForm; ACfgID: Integer): TsmxCustomForm;
var
  FormClass: TsmxCustomFormClass;
  Method: TMethod;
begin
  Result := nil;
  if Assigned(smxProcs.gClassTypeManagerIntf) then
    FormClass := TsmxCustomFormClass(
      smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
        'TsmxStandardForm' +
          smxProcs.gClassTypeManagerIntf.Delimiter +
          'smxStdClasses.dll',
        True))
  else
    FormClass := nil;
  if Assigned(FormClass) then
  begin
    Result := FormClass.Create(nil, smxConsts.cFormCellViewID);
    if Assigned(smxProcs.gFormManagerIntf) then
      smxProcs.gFormManagerIntf.InsertFormControl(Result as IsmxFormControl);
    Result.CellParent := AFormParent;
    Result.Left := 500;
    Result.Top := 100;
    Method.Code := @ActiveFormCellView;
    Method.Data := Result;
    Result.OnActivate := TsmxComponentEvent(Method);
    Method.Code := @DeactiveFormCellView;
    Method.Data := Result;
    Result.OnDeactivate := TsmxComponentEvent(Method);
    Method.Code := @CloseFormCellView;
    Method.Data := Result;
    Result.OnClose := TsmxComponentEvent(Method);
    Result.Options := [foFreeOnClose];
    Result.CfgID := ACfgID;
  end;
end;

procedure ActivateFormSlaveList(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
  Grid: TsmxCustomGrid;
  CurObj: TObject;
  NewObj: TObject;
  KitItem: TsmxKitItem;
begin
  if Sender is TsmxCustomForm then
  begin
    Form := TsmxCustomForm(Sender);
    Grid := TsmxCustomGrid(Form.Cells[0]);
    if Grid.FocusedRowIndex <> -1 then
      KitItem := TsmxKitItem(Integer(Grid.GridValues[0, Grid.FocusedRowIndex]))
    else
      KitItem := nil;
    if Assigned(KitItem) then
      NewObj := KitItem.DisplayObject
    else
      NewObj := nil;
    CurObj := GetObjectFormObjectProps;
    if CurObj <> NewObj then
      RefreshFormObjectProps(NewObj, cObjectInspectorPages);
  end;
end;

procedure DeactiveFormSlaveList(Sender: TsmxComponent);
begin
  //Inf('DeactiveFormSlaveList');
end;

procedure ChangeRowGridFormSlaveList(Sender: TsmxComponent);
var
  Grid: TsmxCustomGrid;
  NewObj: TObject;
  CurObj: TObject;
  KitItem: TsmxKitItem;
begin
  if Sender is TsmxCustomGrid then
  begin
    Grid := TsmxCustomGrid(Sender);
    if Grid.FocusedRowIndex <> -1 then
      KitItem := TsmxKitItem(Integer(Grid.GridValues[0, Grid.FocusedRowIndex]))
    else
      KitItem := nil;
    if Assigned(KitItem) then
      NewObj := KitItem.DisplayObject
    else
      NewObj := nil;
    CurObj := GetObjectFormObjectProps;
    if CurObj <> NewObj then
      RefreshFormObjectProps(NewObj, cObjectInspectorPages);
  end;
end;

function FindFormSlaveList(AKit: TsmxKit): TsmxCustomForm;
var
  i: Integer;
  Form: TsmxCustomForm;
begin
  Result := nil;
  for i := 0 to smxProcs.gFormManagerIntf.FormControlCount - 1 do
  begin
    Form := TsmxCustomForm(smxProcs.gFormManagerIntf.FormControls[i].GetReference);
    if (Form.ID = smxConsts.cFormSlaveListID)
        and (TsmxKit(Form.Tag) = AKit) then
    begin
      Result := Form;
      Break;
    end;
  end;
end;

procedure AddSlaveFormSlaveList(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
  Grid: TsmxCustomGrid;
  Kit: TsmxKit;
  NewObj: TObject;
begin
  if Sender is TsmxCustomToolItem then
  begin
    Form := smxClassFuncs.GetAccessoryForm(TsmxCustomToolItem(Sender));
    Grid := TsmxCustomGrid(Form.Cells[0]);
    Kit := TsmxKit(Form.Tag);
    with Kit.Add do
    begin
      Grid.FocusedRowIndex := ItemIndex;
      NewObj := DisplayObject;
    end;
    if NewObj is TsmxBaseCell then
      TsmxBaseCell(NewObj).Name :=
        smxFuncs.FindUniqueName(TsmxBaseCell(NewObj).Owner, smxFuncs.ClassNameWithoutPrefix(TsmxBaseCell(NewObj).ClassName));
    RefreshFormObjectProps(NewObj, cObjectInspectorPages);
    if NewObj is TsmxBaseCell then
      RefreshFormObjectProps(TsmxBaseCell(NewObj).Owner, cObjectTreeViewPages);
  end;
end;

procedure DelSlaveFormSlaveList(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
  Grid: TsmxCustomGrid;
  Kit: TsmxKit;
  NewObj: TObject;
begin
  if Sender is TsmxCustomToolItem then
  begin
    Form := smxClassFuncs.GetAccessoryForm(TsmxCustomToolItem(Sender));
    Grid := TsmxCustomGrid(Form.Cells[0]);
    Kit := TsmxKit(Form.Tag);
    if Grid.FocusedRowIndex <> -1 then
    begin
      Kit.Delete(Grid.FocusedRowIndex);
      if Grid.FocusedRowIndex <> -1 then
        NewObj := Kit[Grid.FocusedRowIndex].DisplayObject else
        NewObj := nil;
      RefreshFormObjectProps(NewObj, cObjectInspectorPages);
      if NewObj is TsmxBaseCell then
        RefreshFormObjectProps(TsmxBaseCell(NewObj).Owner, cObjectTreeViewPages);
    end;
  end;
end;

function GetSlaveClass(AKit: TsmxKit): TsmxBaseCellClass;
var
  Form: TsmxCustomForm;
  SlaveClass: TsmxBaseCellClass;
  Filter: TsmxMultiFilter;
  i: Integer;
  PersistentClass: TPersistentClass;
  Request: TsmxCustomRequest;
  TypeCfgID: Variant;
begin
  Result := nil;
  if (AKit is TsmxObjectList) and Assigned(TsmxObjectList(AKit).Owner)
      and (TsmxObjectList(AKit).Owner is TsmxOwnerCell) then
    SlaveClass := TsmxOwnerCell(TsmxObjectList(AKit).Owner).GetBaseSlaveClass
  else
    SlaveClass := nil;

  if Assigned(SlaveClass) and Assigned(smxProcs.gClassTypeManagerIntf) then
  begin
    Form := smxClassFuncs.NewForm(nil, 1000078);
    Form.Initialize;
    Request := TsmxCustomRequest(Form.FindChildByName('RL1').FindChildByName('BDEQueryRequest1'));
    Request.Execute;
    Request.DataSet.First;
    while not Request.DataSet.Eof do
    begin
      if smxDBFuncs.GetFieldSenseValue(Request.DataSet, dsKey, 0, TypeCfgID) then
        smxProcs.gClassTypeManagerIntf.RegisterClassTypeName(smxClassFuncs.TypeCfgIDToCellClassName(TypeCfgID));
      Request.DataSet.Next;
    end;
    Filter := TsmxMultiFilter(Form.FindChildByName('FD1').FindChildByName('ComboBoxFilter1'));
    for i := 0 to smxProcs.gClassTypeManagerIntf.ClassTypeCount - 1 do
    begin
      PersistentClass := smxProcs.gClassTypeManagerIntf.ClassTypes[i];
      if not Assigned(PersistentClass) then
        PersistentClass := smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(smxProcs.gClassTypeManagerIntf.ClassTypeNames[i]);
      if Assigned(PersistentClass) and PersistentClass.InheritsFrom(SlaveClass) then
        Filter.AddItem(smxProcs.gClassTypeManagerIntf.ClassTypeNames[i],
                       Integer(smxProcs.gClassTypeManagerIntf.ClassTypes[i]));
    end;
    if Form.ShowModal = mrOk then
      Result := TsmxBaseCellClass(smxFuncs.VarToInt(Filter.Value));
  end;
end;

procedure AddSlaveAsFormSlaveList(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
  Grid: TsmxCustomGrid;
  Kit: TsmxKit;
  NewObj: TObject;
  SlaveClass: TsmxBaseCellClass;
begin
  if Sender is TsmxCustomToolItem then
  begin
    Form := smxClassFuncs.GetAccessoryForm(TsmxCustomToolItem(Sender));
    Grid := TsmxCustomGrid(Form.Cells[0]);
    Kit := TsmxKit(Form.Tag);
    SlaveClass := GetSlaveClass(Kit);
    if Assigned(SlaveClass) then
    begin
      with TsmxObjectList(Kit).Add(SlaveClass) do
      begin
        Grid.FocusedRowIndex := ItemIndex;
        NewObj := DisplayObject;
      end;
      if NewObj is TsmxBaseCell then
        TsmxBaseCell(NewObj).Name :=
          smxFuncs.FindUniqueName(TsmxBaseCell(NewObj).Owner, smxFuncs.ClassNameWithoutPrefix(TsmxBaseCell(NewObj).ClassName));
      RefreshFormObjectProps(NewObj, cObjectInspectorPages);
      if NewObj is TsmxBaseCell then
        RefreshFormObjectProps(TsmxBaseCell(NewObj).Owner, cObjectTreeViewPages);
    end;
  end;
end;

function CreateFormSlaveList(AFormParent: TsmxCustomForm; AKit: TsmxKit): TsmxCustomForm;
var
  FormClass: TsmxCustomFormClass;
  Method: TMethod;
  CellClass: TsmxBaseCellClass;
  Grid: TsmxCustomGrid;
  ToolBar: TsmxCustomToolBoard;
  ToolItem: TsmxCustomToolItem;
begin
  Result := nil;
  if Assigned(smxProcs.gClassTypeManagerIntf) then
    FormClass := TsmxCustomFormClass(
      smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
        'TsmxStandardForm' +
          smxProcs.gClassTypeManagerIntf.Delimiter +
          'smxStdClasses.dll',
        True))
  else
    FormClass := nil;
  if Assigned(FormClass) then
  begin
    Result := FormClass.Create(nil, smxConsts.cFormSlaveListID);
    if Assigned(smxProcs.gFormManagerIntf) then
      smxProcs.gFormManagerIntf.InsertFormControl(Result as IsmxFormControl);
    Result.CellParent := AFormParent;
    Result.Left := 300;
    Result.Top := 50;
    Method.Code := @ActivateFormSlaveList;
    Method.Data := Result;
    Result.OnActivate := TsmxComponentEvent(Method);
    Method.Code := @DeactiveFormSlaveList;
    Method.Data := Result;
    Result.OnDeactivate := TsmxComponentEvent(Method);
    Result.Options := [foFreeOnClose];
    Result.Tag := Integer(AKit);

    if Assigned(smxProcs.gClassTypeManagerIntf) then
      CellClass := TsmxBaseCellClass(
        smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
          'TsmxVTGrid' +
            smxProcs.gClassTypeManagerIntf.Delimiter +
            'smxVTClasses.dll',
          True))
    else
      CellClass := nil;
    if Assigned(CellClass) then
    begin
      Grid := TsmxCustomGrid(CellClass.Create(nil));
      Grid.CellParent := Result;
      Grid.Visible := True;
      Grid.Align := alClient;
      Grid.Options := [goEditing];
      Method.Code := @ChangeRowGridFormSlaveList;
      Method.Data := Grid;
      Grid.OnChangeRow := TsmxComponentEvent(Method);
      with Grid.AddSlave do
      begin
        Visible := True;
        Width := Grid.Width;
      end;
    end;

    //CellClass := TsmxBaseCellClass(Classes.GetClass('TsmxToolBar'));
    if Assigned(smxProcs.gClassTypeManagerIntf) then
      CellClass := TsmxBaseCellClass(
        smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
          'TsmxToolBar' +
            smxProcs.gClassTypeManagerIntf.Delimiter +
            'smxStdClasses.dll',
          True))
    else
      CellClass := nil;
    if Assigned(CellClass) then
    begin
      ToolBar := TsmxCustomToolBoard(CellClass.Create(nil));
      ToolBar.CellParent := Result;
      ToolBar.Visible := True;
      ToolBar.Align := alTop;
      ToolItem := ToolBar.AddSlave;
      ToolItem.Hint := 'Add Slave';
      Method.Code := @AddSlaveFormSlaveList;
      Method.Data := ToolItem;
      ToolItem.OnClick := TsmxComponentEvent(Method);
      ToolItem := ToolBar.AddSlave;
      ToolItem.Hint := 'Del Slave';
      Method.Code := @DelSlaveFormSlaveList;
      Method.Data := ToolItem;
      ToolItem.OnClick := TsmxComponentEvent(Method);
      ToolItem := ToolBar.AddSlave;
      ToolItem.Hint := 'Add Slave as...';
      Method.Code := @AddSlaveAsFormSlaveList;
      Method.Data := ToolItem;
      ToolItem.OnClick := TsmxComponentEvent(Method);
    end;
  end;
end;

procedure ChangeRowTreePageTreeView(Sender: TsmxComponent);
var
  Tree: TsmxCustomTree;
  NewObj: TObject;
  CurObj: TObject;
begin
  if Sender is TsmxCustomTree then
  begin
    Tree := TsmxCustomTree(Sender);
    if Assigned(Tree.FocusedRowIndex) then
      NewObj := TObject(Integer(Tree.TreeValues[0, Tree.FocusedRowIndex]))
    else
      NewObj := nil;
    CurObj := GetObjectFormObjectProps;
    if CurObj <> NewObj then
      RefreshFormObjectProps(NewObj, cObjectInspectorPages);
  end;
end;

procedure RefreshFormSlaveList(AKit: TsmxKit);
var
  Form: TsmxCustomForm;
  Grid: TsmxCustomGrid;
  i: Integer;
  OldFocusedRowIndex: Integer;
begin
  Form := FindFormSlaveList(AKit);
  if Assigned(Form) then
  begin
    Grid := TsmxCustomGrid(Form.Cells[0]);
    OldFocusedRowIndex := Grid.FocusedRowIndex;
    Grid.RowCount := AKit.Count;
    Grid.Slaves[0].Options := Grid.Slaves[0].Options + [coEditing];
    for i := 0 to AKit.Count - 1 do
    begin
      Grid.GridTexts[0, i] := Format('%d - %s', [i, AKit.Items[i].DisplayName]);
      Grid.GridValues[0, i] := Integer(AKit.Items[i]);
    end;
    Grid.Slaves[0].Options := Grid.Slaves[0].Options - [coEditing];
    if (OldFocusedRowIndex <> -1) and (OldFocusedRowIndex < Grid.RowCount) then
      Grid.FocusedRowIndex := OldFocusedRowIndex else
    if Grid.RowCount > 0 then
      Grid.FocusedRowIndex := 0;
  end;
end;

procedure ChangeKitFormSlaveList(Sender: TObject);
begin
  if Sender is TsmxKit then
    RefreshFormSlaveList(TsmxKit(Sender));
end;

procedure ClickButtonStringSlaveList(Sender: TObject);
var
  Form: TsmxCustomForm;
  Tree: TsmxCustomTree;
  V: Variant;
  Obj: TObject;
  PropInfo: PPropInfo;
  CfgID: Integer;
  CfgName: String;
  Kit: TsmxKit;
  Method: TMethod;
begin       
  Tree := TsmxCustomTree(TsmxButtonMaskEdit(Sender).Parent.Tag);
  V := Tree.TreeValues[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
  if not Variants.VarIsArray(V) then
  begin
    Exit;
  end else
  begin
    Obj := TObject(Integer(V[0]));
    PropInfo := PPropInfo(Integer(V[1]));
  end;
  Kit := TsmxKit(TypInfo.GetObjectProp(Obj, PropInfo));
  Form := FindFormSlaveList(Kit);
  if not Assigned(Form) then
  begin
    if Obj is TsmxBaseCell then
    begin
      Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(Obj));
      if (Form.ID <> smxConsts.cFormCellViewID) then
        Form := GetForm(Form.CfgID, smxConsts.cFormCellViewID);
      Form := CreateFormSlaveList(Form, Kit);
    end else
      Form := CreateFormSlaveList(smxClassFuncs.GetAccessoryForm(Tree), Kit);
    if Obj is TsmxBaseCell then
    begin
      CfgID := TsmxBaseCell(Obj).CfgID;
      CfgName := TsmxBaseCell(Obj).Name;
    end else
    begin
      CfgID := 0;
      CfgName := '';
    end;
    Form.Caption := Format('%s(%s).%s(%s) [%d]',
      [CfgName,
       Obj.ClassName,
       PropInfo^.Name,
       TypInfo.GetTypeData(PropInfo^.PropType^)^.ClassType.ClassName,
       CfgID]);
    RefreshFormSlaveList(Kit);
    Method.Code := @ChangeKitFormSlaveList;
    Method.Data := Kit;
    Kit.OnChange := TNotifyEvent(Method);
  end;
  Form.Show;
end;

procedure BeforeEditTreeObjectPropsPage(Sender: TsmxComponent);

  procedure FillClassProp(AObject: TObject; APropInfo: PPropInfo;
    ATree: TsmxCustomTree; const Value: String; AFindList: TList);
  var
    Cls: TClass;
    i: Integer;
    Method: TMethod;
  begin
    Cls := TypInfo.GetTypeData(APropInfo^.PropType^)^.ClassType;
    if ATree.Editor.Control is TComboBox then
    begin
      if Cls.InheritsFrom(TsmxBaseCell) then
        if Assigned(APropInfo^.SetProc) then
        begin
          TComboBox(ATree.Editor.Control).Items.AddObject('', nil);
          for i := 0 to AFindList.Count - 1 do
            if TObject(AFindList[i]).InheritsFrom(Cls) then
              if TComboBox(ATree.Editor.Control).Items.IndexOfObject(TObject(AFindList[i])) = -1 then
                TComboBox(ATree.Editor.Control).Items.AddObject(TsmxBaseCell(AFindList[i]).Name, AFindList[i]);
          TComboBox(ATree.Editor.Control).ItemIndex := TComboBox(ATree.Editor.Control).Items.IndexOf(Value);
        end;
    end else
    if ATree.Editor.Control is TsmxButtonMaskEdit then
    begin
      if Cls.InheritsFrom(TsmxKit) then
      begin
        TsmxButtonMaskEdit(ATree.Editor.Control).ReadOnly := False;
        TsmxButtonMaskEdit(ATree.Editor.Control).Text := Value;
        TsmxButtonMaskEdit(ATree.Editor.Control).ReadOnly := True;
        Method.Code := @ClickButtonStringSlaveList;
        Method.Data := TsmxButtonMaskEdit(ATree.Editor.Control);
        TsmxButtonMaskEdit(ATree.Editor.Control).OnButtonClick := TNotifyEvent(Method);
      end;
    end;
  end;

  procedure FillIntfProp(AObject: TObject; APropInfo: PPropInfo;
    ATree: TsmxCustomTree; const Value: String; AFindList: TList);
  var
    GUID: TGUID;
    i: Integer;
    Intf: IInterface;
  begin
    GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
    if ATree.Editor.Control is TComboBox then
    begin
      if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent) then
        if Assigned(APropInfo^.SetProc) then
        begin
          TComboBox(ATree.Editor.Control).Items.AddObject('', nil);
          for i := 0 to AFindList.Count - 1 do
            if SysUtils.Supports(TObject(AFindList[i]), GUID, Intf) then
              if TComboBox(ATree.Editor.Control).Items.IndexOfObject(IsmxRefComponent(Intf).GetReference) = -1 then
                TComboBox(ATree.Editor.Control).Items.AddObject(IsmxRefComponent(Intf).GetReference.Name, IsmxRefComponent(Intf).GetReference);
          TComboBox(ATree.Editor.Control).ItemIndex := TComboBox(ATree.Editor.Control).Items.IndexOf(Value);
        end;
    end;
  end;

  procedure FillEnumProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    const Value: String);
  var
    TypeData: PTypeData;
    i: Integer;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      TypeData := TypInfo.GetTypeData(APropInfo^.PropType^);
      for i := TypeData^.MinValue to TypeData^.MaxValue do
        TComboBox(ATree.Editor.Control).Items.Add(TypInfo.GetEnumName(APropInfo^.PropType^, i));
      TComboBox(ATree.Editor.Control).ItemIndex := TComboBox(ATree.Editor.Control).Items.IndexOf(Value);
    end;
  end;

  procedure FillSetProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    const Value: String);
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      TComboBox(ATree.Editor.Control).Items.Add(SysUtils.DefaultFalseBoolStr);
      TComboBox(ATree.Editor.Control).Items.Add(SysUtils.DefaultTrueBoolStr);
      TComboBox(ATree.Editor.Control).ItemIndex := TComboBox(ATree.Editor.Control).Items.IndexOf(Value);
    end;
  end;

  procedure FillVarProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    const Value: String);
  begin
    if ATree.Editor.Control is TEdit then
      TEdit(ATree.Editor.Control).Text := Value;
  end;

var
  Tree: TsmxCustomTree;
  V: Variant;
  Obj: TObject;
  PropInfo: PPropInfo;
  EditorType: TsmxEditorType;
  s: String;
  ObjOwner: TObject;
  FindList: TList;
begin
  if Sender is TsmxCustomTree then
  begin
    Tree := TsmxCustomTree(Sender);
    if Assigned(Tree.Editor) then
    begin
      V := Tree.TreeValues[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
      if not Variants.VarIsArray(V) then
      begin
        Exit;
      end else
      begin
        Obj := TObject(Integer(V[0]));
        PropInfo := PPropInfo(Integer(V[1]));
        EditorType := TsmxEditorType(Integer(V[2]));
      end;
      FindList := TList.Create;
      try
        if Obj is TsmxBaseCell then
        begin
          ObjOwner := GetObjectFormTreeView;
          if ObjOwner is TsmxBaseCell then
            smxClassProcs.RefList(TsmxBaseCell(ObjOwner), FindList);
        end;
        Tree.Editor.EditorType := EditorType;
        s := Tree.TreeTexts[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
        case PropInfo^.PropType^^.Kind of
          tkClass:
            FillClassProp(Obj, PropInfo, Tree, s, FindList);
          tkInterface:
            FillIntfProp(Obj, PropInfo, Tree, s, FindList);
          tkEnumeration:
            FillEnumProp(Obj, PropInfo, Tree, s);
          tkSet:
            FillSetProp(Obj, PropInfo, Tree, s);
          else
            FillVarProp(Obj, PropInfo, Tree, s);
        end;
      finally
        FindList.Free;
      end;
    end;
  end;
end;

procedure AfterEditTreeObjectPropsPage(Sender: TsmxComponent);

  procedure SaveClassProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  var
    Obj: TObject;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      if (TComboBox(ATree.Editor.Control).ItemIndex <> -1) and (TComboBox(ATree.Editor.Control).Text <> '') then
        Obj := TComboBox(ATree.Editor.Control).Items.Objects[TComboBox(ATree.Editor.Control).ItemIndex]
      else
        Obj := nil;
      TypInfo.SetObjectProp(AObject, APropInfo, Obj);
      Value := TComboBox(ATree.Editor.Control).Text;
    end else
    if ATree.Editor.Control is TsmxButtonMaskEdit then
    begin
      Value := TsmxButtonMaskEdit(ATree.Editor.Control).Text;
    end;
  end;

  procedure SaveIntfProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  var
    GUID: TGUID;
    Intf: IInterface;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
      if (TComboBox(ATree.Editor.Control).ItemIndex <> -1) and (TComboBox(ATree.Editor.Control).Text <> '') then
        TComboBox(ATree.Editor.Control).Items.Objects[TComboBox(ATree.Editor.Control).ItemIndex].GetInterface(GUID, Intf)
      else
        Intf := nil;
      TypInfo.SetInterfaceProp(AObject, APropInfo, Intf);
      Value := TComboBox(ATree.Editor.Control).Text;
    end;
  end;

  procedure SaveEnumProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      Value := TComboBox(ATree.Editor.Control).Text;
      TypInfo.SetEnumProp(AObject, APropInfo, Value);
    end;
  end;

  procedure SaveSetProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  var
    s: String;
    i: Integer;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      Value := TComboBox(ATree.Editor.Control).Text;
      s := '';
      for i := 0 to ATree.RowCount[ATree.ParentRow[ATree.Editor.RowIndex]] - 1 do
        if (((ATree.TreeTexts[ATree.Editor.ColIndex, ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]] = SysUtils.DefaultTrueBoolStr)
            and (ATree.Editor.RowIndex <> ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]))
          or ((Value = SysUtils.DefaultTrueBoolStr)
            and (ATree.Editor.RowIndex = ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]))) then
          s := s + ATree.TreeTexts[0, ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]] + ',';
      if s <> '' then
        Delete(s, Length(s), 1);
      ATree.TreeTexts[ATree.Editor.ColIndex, ATree.ParentRow[ATree.Editor.RowIndex]] :=
        SysUtils.Format('[%s]', [s]);
      TypInfo.SetSetProp(AObject, APropInfo, s);
    end;
  end;

  procedure SaveVarProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  begin
    if ATree.Editor.Control is TEdit then
    begin
      Value := TEdit(ATree.Editor.Control).Text;
      TypInfo.SetPropValue(AObject, APropInfo^.Name, smxFuncs.StrToVar(Value));
    end;
  end;

var
  Tree: TsmxCustomTree;
  V: Variant;
  Obj: TObject;
  PropInfo: PPropInfo;
  s: String;
begin
  if Sender is TsmxCustomTree then
  begin
    Tree := TsmxCustomTree(Sender);
    if Assigned(Tree.Editor) then
    begin
      V := Tree.TreeValues[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
      if not Variants.VarIsArray(V) then
      begin
        Exit;
      end else
      begin
        Obj := TObject(Integer(V[0]));
        PropInfo := PPropInfo(Integer(V[1]));
      end;
      s := '';
      case PropInfo^.PropType^^.Kind of
        tkClass:
          SaveClassProp(Obj, PropInfo, Tree, s);
        tkInterface:
          SaveIntfProp(Obj, PropInfo, Tree, s);
        tkEnumeration:
          SaveEnumProp(Obj, PropInfo, Tree, s);
        tkSet:
          SaveSetProp(Obj, PropInfo, Tree, s);
        else
          SaveVarProp(Obj, PropInfo, Tree, s);
      end;
      Tree.TreeTexts[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := s;
    end;
  end;
end;

procedure BeforeEditTreeObjectEventsPage(Sender: TsmxComponent);

  procedure FillMethodProp(AObject: TObject; APropInfo: PPropInfo;
    ATree: TsmxCustomTree; const Value: String; AFindList: TList);
  var
    i: Integer;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      if AObject is TsmxBaseCell then
      begin
        TComboBox(ATree.Editor.Control).Items.AddObject('', nil);
        for i := 0 to AFindList.Count - 1 do
          if (TObject(AFindList[i]) is TsmxCustomAlgorithm)
              and Assigned(TsmxCustomAlgorithm(AFindList[i]).OnExecute) then
            TComboBox(ATree.Editor.Control).Items.AddObject(Format('%s%s%s',
                [TsmxCustomAlgorithm(AFindList[i]).Name,
                 smxConsts.cDelimiterObjAndMethName,
                 TsmxCustomAlgorithm(AFindList[i]).MethodName(TMethod(TsmxCustomAlgorithm(AFindList[i]).OnExecute).Code)]),
               AFindList[i]);
        TComboBox(ATree.Editor.Control).ItemIndex := TComboBox(ATree.Editor.Control).Items.IndexOf(Value);
      end;
    end;
  end;

var
  Tree: TsmxCustomTree;
  V: Variant;
  Obj: TObject;
  PropInfo: PPropInfo;
  EditorType: TsmxEditorType;
  s: String;
  ObjOwner: TObject;
  FindList: TList;
begin
  if Sender is TsmxCustomTree then
  begin
    Tree := TsmxCustomTree(Sender);
    if Assigned(Tree.Editor) then
    begin
      V := Tree.TreeValues[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
      if not Variants.VarIsArray(V) then
      begin
        Exit;
      end else
      begin
        Obj := TObject(Integer(V[0]));
        PropInfo := PPropInfo(Integer(V[1]));
        EditorType := TsmxEditorType(Integer(V[2]));
      end;
      FindList := TList.Create;
      try           
        if Obj is TsmxBaseCell then
        begin
          ObjOwner := GetObjectFormTreeView;
          if ObjOwner is TsmxBaseCell then
            smxClassProcs.RefList(TsmxBaseCell(ObjOwner), FindList);
        end;
        Tree.Editor.EditorType := EditorType;
        s := Tree.TreeTexts[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
        FillMethodProp(Obj, PropInfo, Tree, s, FindList);
      finally
        FindList.Free;
      end;
    end;
  end;
end;

procedure ClickColumnTreeObjectPropsPage(Sender: TsmxComponent);
begin
  {if Sender is TsmxCustomColumn then
    if coEditing in TsmxCustomColumn(Sender).Options then
      with TVirtualStringTree((TsmxCustomColumn(Sender).CellOwner as IsmxRefComponent).GetInternalRef) do
      begin
        EditNode(FocusedNode, TsmxCustomColumn(Sender).SlaveIndex);
      end;}
end;

procedure ClickColumnTreeObjectEventsPage(Sender: TsmxComponent);
begin
  if Sender is TsmxCustomTree then
    //if coEditing in TsmxCustomColumn(Sender).Options then
      with TVirtualStringTree((TsmxCustomTree(Sender) as IsmxRefComponent).GetInternalRef) do
      begin
        if Assigned(FocusedNode) then
          EditNode(FocusedNode, FocusedColumn);
      end;
end;

procedure AfterEditTreeObjectEventsPage(Sender: TsmxComponent);

  procedure SaveMethodProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  var
    Obj: TObject;
    Method: TMethod;
    ObjName, MethName: String;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      if (TComboBox(ATree.Editor.Control).ItemIndex <> -1) and (TComboBox(ATree.Editor.Control).Text <> '') then
      begin
        Obj := TComboBox(ATree.Editor.Control).Items.Objects[TComboBox(ATree.Editor.Control).ItemIndex];
        smxProcs.SplitByDelimiter(TComboBox(ATree.Editor.Control).Text, smxConsts.cDelimiterObjAndMethName, ObjName, MethName);
      end else
      begin
        Obj := nil;
        ObjName := '';
        MethName := '';
      end;
      if Assigned(Obj) and (MethName <> '') then
      begin
        Method.Data := Obj;
        Method.Code := Obj.MethodAddress(MethName);
      end else
      begin
        Method.Data := nil;
        Method.Code := nil;
      end;
      TypInfo.SetMethodProp(AObject, APropInfo, Method);
      Value := TComboBox(ATree.Editor.Control).Text;
    end;
  end;

var
  Tree: TsmxCustomTree;
  V: Variant;
  Obj: TObject;
  PropInfo: PPropInfo;
  s: String;
begin
  if Sender is TsmxCustomTree then
  begin
    Tree := TsmxCustomTree(Sender);
    if Assigned(Tree.Editor) then
    begin
      V := Tree.TreeValues[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
      if not Variants.VarIsArray(V) then
      begin
        Exit;
      end else
      begin
        Obj := TObject(Integer(V[0]));
        PropInfo := PPropInfo(Integer(V[1]));
      end;
      s := '';
      SaveMethodProp(Obj, PropInfo, Tree, s);
      Tree.TreeTexts[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := s;
    end;
  end;
end;

procedure AddCellFormObjectProps(Sender: TsmxComponent);
begin
end;

procedure DelCellFormObjectProps(Sender: TsmxComponent);
begin
end;

function CreateFormObjectProps(AFormParent: TsmxCustomForm): TsmxCustomForm;
var
  FormClass: TsmxCustomFormClass;
  Method: TMethod;
  CellClass: TsmxBaseCellClass;
  PageManager: TsmxCustomPageManager;
  Page: TsmxCustomPage;
  Tree: TsmxCustomTree;
  ToolBar: TsmxCustomToolBoard;
  ToolItem: TsmxCustomToolItem;
begin
  Result := nil;
  //FormClass := TsmxCustomFormClass(Classes.GetClass('TsmxStandardForm'));
  if Assigned(smxProcs.gClassTypeManagerIntf) then
    FormClass := TsmxCustomFormClass(
      smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
        'TsmxStandardForm' +
          smxProcs.gClassTypeManagerIntf.Delimiter +
          'smxStdClasses.dll',
        True))
  else
    FormClass := nil;
  if Assigned(FormClass) then
  begin
    Result := FormClass.Create(nil, smxConsts.cFormObjectPropsID);
    if Assigned(smxProcs.gFormManagerIntf) then
      smxProcs.gFormManagerIntf.InsertFormControl(Result as IsmxFormControl);
    Result.CellParent := AFormParent;
    Result.Left := 0;
    Result.Top := 50;
    Result.Width := 220;
    Result.Height := 500;
    Result.Options := [foFreeOnClose];
    with Result.AddSlave do
    begin
      Visible := True;
      Align := alClient;
      PagePosition := ppLeft;
    end;

    Page := Result.Slaves[0].AddSlave;
    Page.Caption := 'Object Inspector';

    if Assigned(smxProcs.gClassTypeManagerIntf) then
      CellClass := TsmxBaseCellClass(
        smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
          'TsmxPageControl' +
            smxProcs.gClassTypeManagerIntf.Delimiter +
            'smxStdClasses.dll',
          True))
    else
      CellClass := nil;
    if Assigned(CellClass) then
    begin
      PageManager := TsmxCustomPageManager(CellClass.Create(nil));
      PageManager.CellParent := Page;
      PageManager.Visible := True;
      PageManager.Align := alClient;

      Page := PageManager.AddSlave;
      Page.Caption := 'Properties';

      if Assigned(smxProcs.gClassTypeManagerIntf) then
        CellClass := TsmxBaseCellClass(
          smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
            'TsmxVTTree' +
              smxProcs.gClassTypeManagerIntf.Delimiter +
              'smxVTClasses.dll',
            True))
      else
        CellClass := nil;
      if Assigned(CellClass) then
      begin
        Tree := TsmxCustomTree(CellClass.Create(nil));
        Tree.CellParent := Page;
        Tree.Visible := True;
        Tree.Align := alClient;
        Tree.Options := [toColLines, toRowLines, toEditing];
        Method.Code := @BeforeEditTreeObjectPropsPage;
        Method.Data := Tree;
        Tree.OnEditing := TsmxComponentEvent(Method);
        Method.Code := @AfterEditTreeObjectPropsPage;
        Method.Data := Tree;
        Tree.OnEdited := TsmxComponentEvent(Method);
        with Tree.AddSlave do
        begin
          Visible := True;
          Width := (Tree.Width - 50) div 2;
          Options := [coResize];
        end;
        with Tree.AddSlave do
        begin
          Visible := True;
          Width := (Tree.Width - 50) div 2;
          Options := [coEditing, coResize];
          Method.Code := @ClickColumnTreeObjectPropsPage;
          Method.Data := Tree;
          OnClick := TsmxComponentEvent(Method);
        end;
      end;

      Page := PageManager.AddSlave;
      Page.Caption := 'Events';

      if Assigned(smxProcs.gClassTypeManagerIntf) then
        CellClass := TsmxBaseCellClass(
          smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
            'TsmxVTTree' +
              smxProcs.gClassTypeManagerIntf.Delimiter +
              'smxVTClasses.dll',
            True))
      else
        CellClass := nil;
      if Assigned(CellClass) then
      begin
        Tree := TsmxCustomTree(CellClass.Create(nil));
        Tree.CellParent := Page;
        Tree.Visible := True;
        Tree.Align := alClient;
        Tree.Options := [toColLines, toRowLines, toEditing];
        Method.Code := @BeforeEditTreeObjectEventsPage;
        Method.Data := Tree;
        Tree.OnEditing := TsmxComponentEvent(Method);
        Method.Code := @AfterEditTreeObjectEventsPage;
        Method.Data := Tree;
        Tree.OnEdited := TsmxComponentEvent(Method);
        with Tree.AddSlave do
        begin
          Visible := True;
          Width := (Tree.Width - 50) div 2;
          Options := [coResize];
        end;
        with Tree.AddSlave do
        begin
          Visible := True;
          Width := (Tree.Width - 50) div 2;
          Options := [coEditing, coResize];
          Method.Code := @ClickColumnTreeObjectEventsPage;
          Method.Data := Tree;
          OnClick := TsmxComponentEvent(Method);
        end;
      end;
    end;

    Page := Result.Slaves[0].AddSlave;
    Page.Caption := 'Object TreeView';

    if Assigned(smxProcs.gClassTypeManagerIntf) then
      CellClass := TsmxBaseCellClass(
        smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
          'TsmxToolBar' +
            smxProcs.gClassTypeManagerIntf.Delimiter +
            'smxStdClasses.dll',
          True))
    else
      CellClass := nil;
    if Assigned(CellClass) then
    begin
      ToolBar := TsmxCustomToolBoard(CellClass.Create(nil));
      ToolBar.CellParent := Page;
      ToolBar.Visible := True;
      ToolBar.Align := alTop;
      ToolItem := ToolBar.AddSlave;
      ToolItem.Hint := 'Add Cell';
      Method.Code := @AddCellFormObjectProps;
      Method.Data := ToolItem;
      ToolItem.OnClick := TsmxComponentEvent(Method);
      ToolItem := ToolBar.AddSlave;
      ToolItem.Hint := 'Del Cell';
      Method.Code := @DelCellFormObjectProps;
      Method.Data := ToolItem;
      ToolItem.OnClick := TsmxComponentEvent(Method);
    end;

    if Assigned(smxProcs.gClassTypeManagerIntf) then
      CellClass := TsmxBaseCellClass(
        smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(
          'TsmxVTTree' +
            smxProcs.gClassTypeManagerIntf.Delimiter +
            'smxVTClasses.dll',
          True))
    else
      CellClass := nil;
    if Assigned(CellClass) then
    begin
      Tree := TsmxCustomTree(CellClass.Create(nil));
      Tree.CellParent := Page;
      Tree.Visible := True;
      Tree.Align := alClient;
      Tree.Options := [toEditing, toTreeLines];
      Method.Code := @ChangeRowTreePageTreeView;
      Method.Data := Tree;
      Tree.OnChangeRow := TsmxComponentEvent(Method);
      with Tree.AddSlave do
      begin
        Visible := True;
        Width := Tree.Width;
        Options := [coResize];
      end;
    end;
  end;
end;

procedure ShowObjectProps(Sender: TsmxComponent);
var
  CfgID, CfgType: Integer;
  CfgName: String;
  Obj: TObject;
  Form, FormParent: TsmxCustomForm;
begin
  if Sender is TsmxCustomAlgorithm then
  begin
    CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).Params, 'ConfID', 0);
    CfgType := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).Params, 'ConfType', 0);
    CfgName := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).Params, 'ConfName', '');
    if CfgID <> 0 then
    begin
      Obj := nil;
      try
        FormParent := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
        Form := GetForm(0, smxConsts.cFormObjectPropsID);
        if not Assigned(Form) then
          Form := CreateFormObjectProps(FormParent);
        Form.Show;
        if CfgType = 100 then // типы
        begin
          Obj := TsmxTypeCfg.Create(nil);
          TsmxTypeCfg(Obj).CfgID := CfgID;
          TsmxTypeCfg(Obj).SelectDataSet := smxClassProcs.gSelectDataSetIntf;
          TsmxTypeCfg(Obj).Load;
          TsmxTypeCfg(Obj).Read;
          RefreshFormObjectProps(Obj, cObjectInspectorPages);
        end else
        begin
          Form := GetForm(CfgID, smxConsts.cFormCellViewID);
          if not Assigned(Form) then
          begin
            Form := CreateFormCellView(FormParent, CfgID);
            Obj := smxClassFuncs.NewCell(Form, CfgID);
            TsmxBaseCell(Obj).CellParent := Form;
            TsmxBaseCell(Obj).Initialize;
            //TsmxBaseCell(Obj).IsDesigning := True;

            Form.Caption := Format('%s(%s) [%d]',
              [TsmxBaseCell(Obj).Name, TsmxBaseCell(Obj).ClassName, CfgID]);
          end;
          Form.Show;
        end;
      finally
        if Assigned(Obj) and (CfgType = 100) then
          Obj.Free;
      end;
    end;
  end;
end;

end.
