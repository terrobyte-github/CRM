unit smxVTAlgs;

interface

uses
  smxBaseClasses;

procedure GetPropsTree(Sender: TsmxComponent);
procedure SetPropsTree(Sender: TsmxComponent);
procedure AfterEditPropTree(Sender: TsmxComponent);
procedure BeforeEditPropTree(Sender: TsmxComponent);

implementation

uses
  Classes, Controls, Windows, Messages, TypInfo, Variants, StdCtrls, SysUtils,
  {VirtualTrees,} smxClasses, smxStdCtrls, smxFuncs, smxClassProcs,
  smxClassFuncs, smxTypes, smxBaseIntf, smxBaseTypes, smxManagerIntf;

procedure AddProps(AObject: TObject; ATree: TsmxCustomTree; AParentRow: Pointer;
  AFindList: TList);

  procedure AddValue(APropInfo: PPropInfo; ARow: Pointer; AEditorType: TsmxEditorType);
  var
    V: Variant;
  begin
    V := Variants.VarArrayOf([Integer(Pointer(AObject)),
      Integer(Pointer(APropInfo)), Integer(AEditorType)]);
    ATree.TreeValues[1, ARow] := V;
  end;

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
      AddValue(APropInfo, ARow, etButtonString);
      if Assigned(Obj) then
        ATree.TreeCaptions[1, ARow] := Format('(%s)', [Obj.ClassName]);
    end else
    if Cls.InheritsFrom(TsmxBaseCell) then
    begin
      // add box
      AddValue(APropInfo, ARow, etPickString);
      if Assigned(Obj) then
        ATree.TreeCaptions[1, ARow] := TsmxBaseCell(Obj).Name;
    end else
    begin
      // add blank
      AddValue(APropInfo, ARow, etNone);
      if Assigned(Obj) then
      begin
        ATree.TreeCaptions[1, ARow] := Format('(%s)', [Obj.ClassName]);
        AddProps(Obj, ATree, ARow, AFindList);
      end;
    end;
  end;

  procedure AddIntf(APropInfo: PPropInfo; ARow: Pointer);

    {function IsImplIntf(const AIntf: IsmxRefPersistent): Boolean;
    var
      Intf: IsmxRefPersistent;
    begin
      Result := AObject.GetInterface(IsmxRefPersistent, Intf)
        and (IsmxRefPersistent(Intf).GetReference = AIntf.GetReference);
    end;}

    function FindImplObj(const AIntf: IsmxRefComponent{IsmxRefPersistent}): TObject;
    var
      i: Integer;
      //Intf: IInterface;
    begin
      Result := nil;
      if Assigned(AIntf) and Assigned(AFindList) then
      begin
        for i := 0 to AFindList.Count - 1 do
          //if TObject(AFindList[i]).GetInterface(IsmxRefComponent{IsmxRefPersistent}, Intf)
              //and (IsmxRefComponent{IsmxRefPersistent}(Intf).GetReference = AIntf.GetReference) then
          if (TObject(AFindList[i]) is TsmxBaseCell)
              and (TsmxBaseCell(AFindList[i]).IsImplIntf(AIntf)) then
          begin
            Result := AFindList[i];
            Break;
          end;
      end;
    end;

  var
    Intf: IInterface;
    GUID: TGUID;
    Obj: TObject;
  begin
    GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
    Intf := TypInfo.GetInterfaceProp(AObject, APropInfo);
    if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent{IsmxRefPersistent}) then
    begin
      //if {SysUtils.Supports(Intf, IsmxRefPersistent)} {then
      //begin
        //if} {and} smxFuncs.IsImplIntf(AObject, IsmxRefComponent{IsmxRefPersistent}(Intf)) then
        if (AObject is TsmxBaseCell)
            and (TsmxBaseCell(AObject).IsImplIntf(IsmxRefComponent(Intf))) then
        begin
          AddValue(APropInfo, ARow, etNone);
          ATree.TreeCaptions[1, ARow] := Format('(%s)', [IsmxRefComponent{IsmxRefPersistent}(Intf).GetReference.ClassName]);
          AddProps(IsmxRefComponent{IsmxRefPersistent}(Intf).GetReference, ATree, ARow, AFindList);
        end else
        begin    
          AddValue(APropInfo, ARow, etPickString);
          Obj := FindImplObj(IsmxRefComponent{IsmxRefPersistent}(Intf));
          if Obj is TsmxBaseCell then
            ATree.TreeCaptions[1, ARow] := TsmxBaseCell(Obj).Name;
        end;
      //end;
    end;
  end;

  procedure AddEnumeration(APropInfo: PPropInfo; ARow: Pointer);
  begin
    AddValue(APropInfo, ARow, etPickString);
    ATree.TreeCaptions[1, ARow] := TypInfo.GetEnumProp(AObject, APropInfo);
  end;

  procedure AddSet(APropInfo: PPropInfo; ARow: Pointer);
  var
    SetStr, EnumStr: String;
    TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    i: Integer;
  begin
    AddValue(APropInfo, ARow, etNone);
    SetStr := TypInfo.GetSetProp(AObject, APropInfo, True);
    ATree.TreeCaptions[1, ARow] := SetStr;
    TypeInfo := TypInfo.GetTypeData(APropInfo^.PropType^)^.CompType^;
    TypeData := TypInfo.GetTypeData(TypeInfo);
    ATree.RowCount[ARow] := TypeData^.MaxValue - TypeData^.MinValue + 1;
    for i := TypeData^.MinValue to TypeData^.MaxValue do
    begin
      AddValue(APropInfo, ATree.Rows[ARow, i], etPickString);
      EnumStr := TypInfo.GetEnumName(TypeInfo, i);
      ATree.TreeCaptions[0, ATree.Rows[ARow, i]] := EnumStr;
      ATree.TreeCaptions[1, ATree.Rows[ARow, i]] := SysUtils.BoolToStr(Pos(EnumStr, SetStr) > 0, True);
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
        ATree.TreeCaptions[0, ATree.Rows[AParentRow, i]] := PropInfo^.Name;
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
            AddValue(PropInfo, ATree.Rows[AParentRow, i], etString);
            ATree.TreeCaptions[1, ATree.Rows[AParentRow, i]] :=
              Variants.VarToStr(TypInfo.GetPropValue(AObject, PropInfo^.Name));
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure CellPropsForm(Sender: TsmxComponent);

  procedure FillTree(ACell: TsmxBaseCell; ATree: TsmxCustomTree; AParentRow: Pointer);
  var
    i: Integer;
  begin
    ATree.RowCount[AParentRow] := ACell.CellCount;
    //ATree.Slaves[0].ColumnOptions := ATree.Slaves[0].ColumnOptions + [coEditing];
    for i := 0 to ACell.CellCount - 1 do
    begin
      ATree.TreeCaptions[0, ATree.Rows[AParentRow, i]] := Format('%s [%d]', [ACell.Cells[i].Name, ACell.Cells[i].CfgID]);
      ATree.TreeValues[0, ATree.Rows[AParentRow, i]] := Integer(ACell.Cells[i]);
      if ACell.Cells[i].CellCount > 0 then
        FillTree(ACell.Cells[i], ATree, ATree.Rows[AParentRow, i]);
    end;
    //ATree.Slaves[0].ColumnOptions := ATree.Slaves[0].ColumnOptions - [coEditing];
  end;

var
  Form, FormParent: TsmxCustomForm;
  //Cell: TsmxBaseCell;
  List, FindList: TList;
  Tree: TsmxCustomTree;
  V: Variant;
  NewObj, CurObj: TObject;
  FormIntf: IsmxForm;
begin
  if Sender is TsmxCustomForm then
  begin
    Form := TsmxCustomForm(Sender);
    {if Form.CfgID <> 0 then
      Cell := Form else}
    if Form.CellCount <> 0 then
      NewObj := Form.Cells[0] else
      NewObj := nil;
    if Form.CellParent is TsmxCustomForm then
      FormParent := TsmxCustomForm(Form.CellParent) else
      FormParent := nil;
    if Assigned(NewObj) and Assigned(FormParent) then
    begin
      List := TList.Create;
      try
        smxClassProcs.AllCells(FormParent, List, [TsmxCustomTree], True);
        if List.Count <> 0 then
        begin
          Tree := TsmxCustomTree(List[0]);
          CurObj := nil;
          if Tree.RowCount[Tree.RootRow] <> 0 then
          begin
            V := Tree.TreeValues[1, Tree.Rows[Tree.RootRow, 0]];
            if Variants.VarIsArray(V) then
              CurObj := TObject(Integer(V[0]));
          end;
          if NewObj <> CurObj then
          begin
            Tree.RowCount[Tree.RootRow] := 0;
            FindList := TList.Create;
            try
              if NewObj is TsmxBaseCell then
                smxClassProcs.AllCells(TsmxBaseCell(NewObj), FindList, []);
              //FindList.Add(Obj);
              AddProps(NewObj, Tree, Tree.RootRow, FindList);
              FormIntf := smxClassProcs.gFormManagerIntf.FindByComboID(0, -1);
              if Assigned(FormIntf) then
                if NewObj is TsmxBaseCell then
                begin
                  Form := TsmxCustomForm(FormIntf.GetReference);
                  Tree := TsmxCustomTree(Form.Cells[0]);
                  Tree.RowCount[Tree.RootRow] := 1;
                  Tree.Slaves[0].ColumnOptions := Tree.Slaves[0].ColumnOptions + [coEditing];
                  Tree.TreeCaptions[0, Tree.Rows[Tree.RootRow, 0]] := Format('%s [%d]', [TsmxBaseCell(NewObj).Name, TsmxBaseCell(NewObj).CfgID]);
                  Tree.TreeValues[0, Tree.Rows[Tree.RootRow, 0]] := Integer(TsmxBaseCell(NewObj));
                  FillTree(TsmxBaseCell(NewObj), Tree, Tree.Rows[Tree.RootRow, 0]);
                  Tree.Slaves[0].ColumnOptions := Tree.Slaves[0].ColumnOptions - [coEditing];
                end;
            finally
              FindList.Free;
            end;
          end;
        end;
      finally
        List.Free;
      end;
    end;
  end;
end;

procedure ItemPropsForm(Sender: TsmxComponent);
var
  Form, FormParent: TsmxCustomForm;
  List, FindList: TList;
  Tree: TsmxCustomTree;
  V: Variant;
  Obj: TObject;
  Grid: TsmxCustomGrid;
  CurObj: TObject;
  NewObj: TObject;
begin
  if Sender is TsmxCustomForm then
  begin
    Form := TsmxCustomForm(Sender);
    FormParent := TsmxCustomForm(Form.CellParent);
    ////////Obj := TObject(Form.CfgID);
    NewObj := nil;
    if Form.CellCount <> 0 then
    begin
      Grid := TsmxCustomGrid(Form.Cells[0]);
      if Grid.FocusedRowIndex <> -1 then
        NewObj := TObject(Integer(Grid.GridValues[0, Grid.FocusedRowIndex]));
    end;
    if Assigned(NewObj) then
    begin
      Obj := nil;
      if NewObj is TsmxKitItem then
      begin
        if TsmxKitItem(NewObj).Kit.Owner is TsmxBaseCell then
        begin
          Obj := TsmxBaseCell(TsmxKitItem(NewObj).Kit.Owner);
          if Assigned(TsmxBaseCell(Obj).Owner) then
            Obj := TsmxBaseCell(Obj).Owner;
        end;
      end;
      List := TList.Create;
      try
        smxClassProcs.AllCells(FormParent, List, [TsmxCustomTree], True);
        if List.Count <> 0 then
        begin
          Tree := TsmxCustomTree(List[0]);
          CurObj := nil;
          if Tree.RowCount[Tree.RootRow] <> 0 then
          begin
            V := Tree.TreeValues[1, Tree.Rows[Tree.RootRow, 0]];
            if Variants.VarIsArray(V) then
              CurObj := TObject(Integer(V[0]));
          end;
          if NewObj <> CurObj then
          begin
            Tree.RowCount[Tree.RootRow] := 0;
            FindList := TList.Create;
            try
              if Obj is TsmxBaseCell then
                smxClassProcs.AllCells(TsmxBaseCell(Obj), FindList, []);
              //FindList.Add(Obj);
              AddProps(NewObj, Tree, Tree.RootRow, FindList);
            finally
              FindList.Free;
            end;
          end;
        end;
      finally
        List.Free;
      end;
    end;
  end;
end;

procedure ChangeItemGrid(Sender: TsmxComponent);
var
  Grid: TsmxCustomGrid;
  Form, FormParent: TsmxCustomForm;
  List, FindList: TList;
  Tree: TsmxCustomTree;
  Obj: TObject;
  V: Variant;
  NewObj: TObject;
  CurObj: TObject;
begin
  if Sender is TsmxCustomGrid then
  begin
    Grid := TsmxCustomGrid(Sender);
    Form := TsmxCustomForm(Grid.CellParent);
    //////Obj := TObject(Form.CfgID);
    FormParent := TsmxCustomForm(Form.CellParent);
    if Grid.FocusedRowIndex <> -1 then
      NewObj := TObject(Integer(Grid.GridValues[0, Grid.FocusedRowIndex]))
    else
      NewObj := nil;
    if Assigned(NewObj) then
    begin
      Obj := nil;
      if NewObj is TsmxKitItem then
      begin
        if TsmxKitItem(NewObj).Kit.Owner is TsmxBaseCell then
        begin
          Obj := TsmxKitItem(NewObj).Kit.Owner;
          if Assigned(TsmxBaseCell(Obj).Owner) then
            Obj := TsmxBaseCell(Obj).Owner;
        end;
      end;
      List := TList.Create;
      try
        smxClassProcs.AllCells(FormParent, List, [TsmxCustomTree], True);
        if List.Count <> 0 then
        begin
          Tree := TsmxCustomTree(List[0]);
          CurObj := nil;
          if Tree.RowCount[Tree.RootRow] <> 0 then
          begin
            V := Tree.TreeValues[1, Tree.Rows[Tree.RootRow, 0]];
            if Variants.VarIsArray(V) then
              CurObj := TObject(Integer(V[0]));
          end;
          if NewObj <> CurObj then
          begin
            Tree.RowCount[Tree.RootRow] := 0;
            FindList := TList.Create;
            try
              if Obj is TsmxBaseCell then
                smxClassProcs.AllCells(TsmxBaseCell(Obj), FindList, []);
              //FindList.Add(Obj);
              AddProps(NewObj, Tree, Tree.RootRow, FindList);
            finally
              FindList.Free;
            end;
          end;
        end;
      finally
        List.Free;
      end;
    end;
  end;
end;

procedure ChangeItemTree(Sender: TsmxComponent);
var
  Form, FormParent: TsmxCustomForm;
  List, FindList: TList;
  Tree: TsmxCustomTree;
  Obj: TObject;
  V: Variant;
  NewObj: TObject;
  CurObj: TObject;
begin
  if Sender is TsmxCustomTree then
  begin
    Tree := TsmxCustomTree(Sender);
    Form := TsmxCustomForm(Tree.CellParent);
    //////Obj := TObject(Form.CfgID);
    FormParent := TsmxCustomForm(Form.CellParent);
    if Assigned(Tree.FocusedRowIndex) then
      NewObj := TObject(Integer(Tree.TreeValues[0, Tree.FocusedRowIndex]))
    else
      NewObj := nil;
    if Assigned(NewObj) then
    begin
      Obj := nil;
      if NewObj is TsmxKitItem then
      begin
        if TsmxKitItem(NewObj).Kit.Owner is TsmxBaseCell then
        begin
          Obj := TsmxKitItem(NewObj).Kit.Owner;
          if Assigned(TsmxBaseCell(Obj).Owner) then
            Obj := TsmxBaseCell(Obj).Owner;
        end;
      end;
      List := TList.Create;
      try
        smxClassProcs.AllCells(FormParent, List, [TsmxCustomTree], True);
        if List.Count <> 0 then
        begin
          Tree := TsmxCustomTree(List[0]);
          CurObj := nil;
          if Tree.RowCount[Tree.RootRow] <> 0 then
          begin
            V := Tree.TreeValues[1, Tree.Rows[Tree.RootRow, 0]];
            if Variants.VarIsArray(V) then
              CurObj := TObject(Integer(V[0]));
          end;
          if NewObj <> CurObj then
          begin
            Tree.RowCount[Tree.RootRow] := 0;
            FindList := TList.Create;
            try
              if Obj is TsmxBaseCell then
                smxClassProcs.AllCells(TsmxBaseCell(Obj), FindList, []);
              //FindList.Add(Obj);
              AddProps(NewObj, Tree, Tree.RootRow, FindList);
            finally
              FindList.Free;
            end;
          end;
        end;
      finally
        List.Free;
      end;
    end;
  end;
end;

procedure GetPropsTree(Sender: TsmxComponent);

  function CreateForm(AFormParent: TsmxCustomForm): TsmxCustomForm;
  var
    FormClass: TsmxCustomFormClass;
    Method: TMethod;
  begin
    Result := nil;
    FormClass := TsmxCustomFormClass(Classes.GetClass('TsmxStandardForm'));
    if Assigned(FormClass) then
    begin
      Result := FormClass.Create(nil);
      Result.FormManager := smxClassProcs.gFormManagerIntf;
      Result.CellParent := AFormParent;
      Result.CellLeft := 500;
      Result.CellTop := 100;
      Method.Code := @CellPropsForm;
      Method.Data := Result;
      Result.OnActivate := TsmxComponentEvent(Method);
      //Result.FormOptions := [foFreeOnClose];
      //Result.AddSlave.AddSlave.AddSlave;
    end;
  end;

  function CreateTreeForm(AFormParent: TsmxCustomForm): TsmxCustomForm;
  var
    FormClass: TsmxCustomFormClass;
    Method: TMethod;
    CellClass: TsmxBaseCellClass;
    Tree: TsmxCustomTree;
  begin
    FormClass := TsmxCustomFormClass(Classes.GetClass('TsmxStandardForm'));
    Result := FormClass.Create(nil, -1);
    Result.FormManager := smxClassProcs.gFormManagerIntf;
    Result.CellParent := AFormParent;
    Result.CellLeft := 50;
    Result.CellTop := 50;
    //Method.Code := @ItemPropsForm;
    //Method.Data := Result;
    //Result.OnActivate := TsmxComponentEvent(Method);

    CellClass := TsmxBaseCellClass(Classes.GetClass('TsmxVTTree'));
    if Assigned(CellClass) then
    begin
      Tree := TsmxCustomTree(CellClass.Create(nil));
      Tree.CellParent := Result;
      Tree.CellVisible := True;
      Tree.CellAlign := alClient;
      Tree.TreeOptions := [toEditing, toTreeLines];
      Method.Code := @ChangeItemTree;
      Method.Data := Tree;
      Tree.OnChangeRow := TsmxComponentEvent(Method);
      with Tree.AddSlave do
      begin
        CellVisible := True;
        CellWidth := Tree.CellWidth;
        ColumnOptions := [coHasValue];
      end;
    end;
  end;

  {function FindForm(AFormParent: TsmxCustomForm; ACfgID: Integer): TsmxCustomForm;
  var
    i: Integer;
    Cell: TsmxBaseCell;
  begin
    Result := nil;
    if Assigned(AFormParent) then
      for i := 0 to AFormParent.CellCount - 1 do
        if AFormParent.Cells[i] is TsmxCustomForm then
        begin
          Cell := AFormParent.Cells[i].FindChildByCfgID(ACfgID);
          if Assigned(Cell) then
          begin
            Result := TsmxCustomForm(AFormParent.Cells[i]);
            Break;
          end;
        end;
  end;}

var
  CfgID, CfgType: Integer;
  CfgName: String;
  Tree: TsmxCustomTree;
  Obj: TObject;
  //FindList: TList;
  Form, FormParent: TsmxCustomForm;
  //CellClass: TsmxBaseCellClass;
  //IntfClass: TsmxInterfacedPersistentClass;
  FormIntf: IsmxForm;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomTree then
    begin
      Tree := TsmxCustomTree(TsmxCustomAlgorithm(Sender).CellEvent);
      CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
      CfgType := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfType', 0);
      CfgName := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfName', '');
      if CfgID <> 0 then
      begin
        Obj := nil;
        try
          if CfgType = 100 then // типы
          begin
            Obj := TsmxTypeCfg.Create(nil);
            TsmxTypeCfg(Obj).CfgID := CfgID;
            TsmxTypeCfg(Obj).SelectDataSet := smxClassProcs.gCfgSelectDataSet;
            TsmxTypeCfg(Obj).Load;
            TsmxTypeCfg(Obj).Read;
          end else
          begin
            FormParent := smxClassFuncs.GetAccessoryForm(Tree);
            //Form := FindForm(FormParent, CfgID);
            FormIntf := smxClassProcs.gFormManagerIntf.FindByComboID(0, -1);
            if not Assigned(FormIntf) then
            begin
              Form := CreateTreeForm(FormParent);
              Form.Show;
            end else
              Form := TsmxCustomForm(FormIntf.GetReference);
            Form.CellCaption := 'CellTreeView';//Format('CellTreeView [%d] - %s %s', [CfgID, TsmxBaseCell(Obj).Name, TsmxBaseCell(Obj).ClassName{CfgName}]);

            FormIntf := smxClassProcs.gFormManagerIntf.FindByComboID(-CfgID);
            if not Assigned(FormIntf) then
            begin
              Obj := smxClassFuncs.NewCell(nil, CfgID);
              TsmxBaseCell(Obj).IsDesigning := True;
              Form := CreateForm(FormParent);
              Form.CellCaption := Format('[%d] - %s %s', [CfgID, TsmxBaseCell(Obj).Name, TsmxBaseCell(Obj).ClassName{CfgName}]);
              Form.CfgID := -CfgID;
              TsmxBaseCell(Obj).CellParent := Form;
              TsmxBaseCell(Obj).CfgID := CfgID;
              TsmxBaseCell(Obj).Initialize;
              FormIntf := Form as IsmxForm;
            end;
            FormIntf.Show;
          end;
        finally
          if Assigned(Obj) and (CfgType = 100) then
            Obj.Free;
        end;
      end;
    end;
end;

procedure SetPropsTree(Sender: TsmxComponent);
begin
  inf('SetPropsTree');
end;

procedure AfterEditPropTree(Sender: TsmxComponent);

  procedure SaveClassProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    var Value: String);
  var
    Obj: TObject;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      if TComboBox(ATree.Editor.Control).ItemIndex <> -1 then
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
    Intf: IInterface;
  begin
    if ATree.Editor.Control is TComboBox then
    begin
      if TComboBox(ATree.Editor.Control).ItemIndex <> -1 then
        TComboBox(ATree.Editor.Control).Items.Objects[TComboBox(ATree.Editor.Control).ItemIndex].GetInterface(IInterface, Intf)
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
        if (((ATree.TreeCaptions[ATree.Editor.ColIndex, ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]] = SysUtils.DefaultTrueBoolStr)
            and (ATree.Editor.RowIndex <> ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]))
          or ((Value = SysUtils.DefaultTrueBoolStr)
            and (ATree.Editor.RowIndex = ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]))) then
          s := s + ATree.TreeCaptions[0, ATree.Rows[ATree.ParentRow[ATree.Editor.RowIndex], i]] + ',';
      if s <> '' then
        Delete(s, Length(s), 1);
      ATree.TreeCaptions[ATree.Editor.ColIndex, ATree.ParentRow[ATree.Editor.RowIndex]] :=
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
  //EditorType: TsmxEditorType;
  s: String;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomTree then
    begin
      Tree := TsmxCustomTree(TsmxCustomAlgorithm(Sender).CellEvent);
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
          //EditorType := TsmxEditorType(Integer(V[2]));
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
        Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := s;

        {case EditorType of
          etString:
          begin
            if Tree.Editor.Control is TEdit then
              with TEdit(Tree.Editor.Control) do
              begin
                Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := Text;
                TypInfo.SetPropValue(Obj, PropInfo^.Name, smxFuncs.StrToVar(Text));
              end;
          end;
          etPickString:
          begin
            if Tree.Editor.Control is TComboBox then
              with TComboBox(Tree.Editor.Control) do
              begin
                case PropInfo^.PropType^^.Kind of
                  tkClass:
                  begin
                    Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := Items[ItemIndex];
                    SaveClassProp(Obj, PropInfo, TComboBox(Tree.Editor.Control));
                  end;
                  tkEnumeration:
                  begin
                    Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := Items[ItemIndex];
                    TypInfo.SetEnumProp(Obj, PropInfo, Items[ItemIndex]);
                  end;
                  tkSet:
                  begin
                    Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex] := Items[ItemIndex];
                    SaveSetProp(Obj, PropInfo, Tree);
                  end;
                end;
              end;
            end;
          end;}
            {end;
          vtPickString:
            begin
              FEdit := TComboBox.Create(nil);
              with FEdit as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value;
                Items.Add(Text);
                Items.Add('Standard');
                Items.Add('Additional');
                Items.Add('Win32');
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtNumber:
            begin
              FEdit := TMaskEdit.Create(nil);
              with FEdit as TMaskEdit do
              begin
                Visible := False;
                Parent := Tree;
                EditMask := '9999';
                Text := Data.Value;
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtPickNumber:
            begin
              FEdit := TComboBox.Create(nil);
              with FEdit as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value;
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtMemo:
            begin
              FEdit := TComboBox.Create(nil);
              // In reality this should be a drop down memo but this requires
              // a special control.
              with FEdit as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value;
                Items.Add(Data.Value);
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtDate:
            begin
              FEdit := TDateTimePicker.Create(nil);
              with FEdit as TDateTimePicker do
              begin
                Visible := False;
                Parent := Tree;
                CalColors.MonthBackColor := clWindow;
                CalColors.TextColor := clBlack;
                CalColors.TitleBackColor := clBtnShadow;
                CalColors.TitleTextColor := clBlack;
                CalColors.TrailingTextColor := clBtnFace;
                Date := StrToDate(Data.Value);
                OnKeyDown := EditKeyDown;
              end;
            end;}
        //else
          //Result := False;
        //end;
      end;
    end;
end;

procedure BeforeEditPropTree(Sender: TsmxComponent);

  function CreateForm({AObject: TObject;} APropInfo: PPropInfo;
    AFormParent: TsmxCustomForm): TsmxCustomForm;
  var
    FormClass: TsmxCustomFormClass;
    Method: TMethod;
    CellClass: TsmxBaseCellClass;
    Grid: TsmxCustomGrid;
    ToolBar: TsmxCustomToolBoard;
  begin
    FormClass := TsmxCustomFormClass(Classes.GetClass('TsmxStandardForm'));
    Result := FormClass.Create(nil, Integer(Pointer(APropInfo)));
    Result.FormManager := smxClassProcs.gFormManagerIntf;
    Result.CellParent := AFormParent;
    //Result.CfgID := Integer(Pointer(AObject));
    Result.CellLeft := 300;
    Result.CellTop := 50;
    Method.Code := @ItemPropsForm;
    Method.Data := Result;
    Result.OnActivate := TsmxComponentEvent(Method);

    CellClass := TsmxBaseCellClass(Classes.GetClass('TsmxVTGrid'));
    if Assigned(CellClass) then
    begin
      Grid := TsmxCustomGrid(CellClass.Create(nil));
      Grid.CellParent := Result;
      Grid.CellVisible := True;
      Grid.CellAlign := alClient;
      Grid.GridOptions := [goEditing];
      Method.Code := @ChangeItemGrid;
      Method.Data := Grid;
      Grid.OnChangeRow := TsmxComponentEvent(Method);
      with Grid.AddSlave do
      begin
        CellVisible := True;
        CellWidth := Grid.CellWidth;
        ColumnOptions := [coHasValue];
      end;
    end;

    CellClass := TsmxBaseCellClass(Classes.GetClass('TsmxToolBar'));
    if Assigned(CellClass) then
    begin
      ToolBar := TsmxCustomToolBoard(CellClass.Create(nil));
      ToolBar.CellParent := Result;
      ToolBar.CellVisible := True;
      ToolBar.CellAlign := alTop;
      with ToolBar.AddSlave do
      begin
        ;
      end;
    end;
  end;

  procedure FillClassPropForm(AObject: TObject; APropInfo: PPropInfo;
    AForm: TsmxCustomForm);
  var
    //Cls: TClass;
    Grid: TsmxCustomGrid;
    i: Integer;
    Obj: TObject;
  begin
    //Cls := TypInfo.GetTypeData(APropInfo^.PropType^)^.ClassType;
    Obj := TypInfo.GetObjectProp(AObject, APropInfo);
    Grid := nil;
    if AForm.CellCount > 0 then
      if AForm.Cells[0] is TsmxCustomGrid then
        Grid := TsmxCustomGrid(AForm.Cells[0]);
    if Assigned(Grid) then
    begin
      if Obj is TsmxKit then
      begin
        Grid.RowCount := TsmxKit(Obj).Count;
        Grid.Slaves[0].ColumnOptions := Grid.Slaves[0].ColumnOptions + [coEditing];
        for i := 0 to TsmxKit(Obj).Count - 1 do
        begin
          Grid.GridCaptions[0, i] := Format('%d - %s', [i, TsmxKit(Obj).Items[i].DisplayName]);
          Grid.GridValues[0, i] := Integer(Pointer(TsmxKit(Obj).Items[i].DisplayObject));
        end;
        Grid.Slaves[0].ColumnOptions := Grid.Slaves[0].ColumnOptions - [coEditing];
      end;
      if Grid.RowCount > 0 then
        Grid.FocusedRowIndex := 0;
    end;
  end;

  procedure ButtonClick(Sender: TObject);
  var
    Form: TsmxCustomForm;
    Tree: TsmxCustomTree;
    V: Variant;
    Obj: TObject;
    PropInfo: PPropInfo;
    FormIntf: IsmxForm;
    //Owner: TObject;
    CfgID: Integer;
    CfgName: String;
  begin
    Tree := nil;
    if Sender is TsmxButtonMaskEdit then
      if Assigned(TsmxButtonMaskEdit(Sender).Parent) then
        Tree := TsmxCustomTree(TsmxButtonMaskEdit(Sender).Parent.Tag);
    if Assigned(Tree) then
    begin
      V := Tree.TreeValues[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
      if not Variants.VarIsArray(V) then
      begin
        Exit;
      end else
      begin
        Obj := TObject(Integer(V[0]));
        PropInfo := PPropInfo(Integer(V[1]));
        //EditorType := TsmxEditorType(Integer(V[2]));
      end;

      {Owner := nil;
      if Obj is TsmxBaseCell then
      begin
        Owner := TsmxBaseCell(Obj).Owner;
      end else
      if Obj is TsmxKit then
      begin
        Owner := TsmxKit(Obj).Owner;
        if Owner is TsmxBaseCell then
          Owner := TsmxBaseCell(Owner).Owner;
      end;

      if Owner is TsmxBaseCell then
        CfgID := TsmxBaseCell(Owner).CfgID else
        CfgID := 0;}

      {if Obj is TsmxBaseCell then
        CfgID := TsmxBaseCell(Obj).CfgID else
        CfgID := 0;}

      FormIntf := smxClassProcs.gFormManagerIntf.FindByComboID(0{CfgID}, Integer(Pointer(PropInfo)));
      if not Assigned(FormIntf) then
      begin
        Form := CreateForm({Obj,} PropInfo, smxClassFuncs.GetAccessoryForm(Tree));
        //Form.CfgID := CfgID;
        if Obj is TsmxBaseCell then
        begin
          CfgID := TsmxBaseCell(Obj).CfgID;
          CfgName := TsmxBaseCell(Obj).Name;
        end else
        begin
          CfgID := 0;
          CfgName := '';
        end;
        Form.CellCaption := Format('[%d] - %s.%s %s', [CfgID, CfgName, PropInfo^.Name, TypInfo.GetTypeData(PropInfo^.PropType^)^.ClassType.ClassName]);
        FormIntf := Form as IsmxForm;
        case PropInfo^.PropType^^.Kind of
          tkClass:
            FillClassPropForm(Obj, PropInfo, Form);
        end;
      end;
      FormIntf.Show;
      {Grid.RowCount := TsmxOwnerCell(Obj).SlaveCount;
      for i := 0 to TsmxOwnerCell(Obj).SlaveCount - 1 do
      begin
        Grid.GridCaptions[0, i] := Format('%d - %s', [i, TsmxOwnerCell(Obj).Slaves[i].Name]);
        Grid.GridValues[0, i] := Integer(Pointer(TsmxOwnerCell(Obj).Slaves[i]));
      end;}
    end;
  end;

  procedure FillClassProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    const Value: String);
  var
    Cls: TClass;
    FindList: TList;
    i: Integer;
    Method: TMethod;
  begin
    Cls := TypInfo.GetTypeData(APropInfo^.PropType^)^.ClassType;
    if ATree.Editor.Control is TComboBox then
    begin
      if Cls.InheritsFrom(TsmxBaseCell) then
      begin
        FindList := TList.Create;
        try
          if AObject is TsmxBaseCell then
            smxClassProcs.AllCells(TsmxBaseCell(AObject), FindList, [TsmxBaseCellClass(Cls)]);
          //if AObject is Cls then
            //FindList.Add(AObject);
          for i := 0 to FindList.Count - 1 do
            TComboBox(ATree.Editor.Control).Items.AddObject(TsmxBaseCell(FindList[i]).Name, FindList[i]);
        finally
          FindList.Free;
        end;
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
        Method.Code := @ButtonClick;
        Method.Data := TsmxButtonMaskEdit(ATree.Editor.Control);
        TsmxButtonMaskEdit(ATree.Editor.Control).OnButtonClick := TNotifyEvent(Method);
      end;
    end;
  end;

  procedure FillIntfProp(AObject: TObject; APropInfo: PPropInfo; ATree: TsmxCustomTree;
    const Value: String);
  var
    GUID: TGUID;
    FindList: TList;
    i: Integer;
  begin
    GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
    if ATree.Editor.Control is TComboBox then
    begin
      if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent{IsmxRefPersistent}) then
      begin
        FindList := TList.Create;
        try
          if AObject is TsmxBaseCell then
          //begin
            smxClassProcs.AllCells(TsmxBaseCell(AObject), FindList, []);
            //FindList.Add(AObject);
          //end;
          for i := 0 to FindList.Count - 1 do
            if SysUtils.Supports(TObject(FindList[i]), GUID) then
              TComboBox(ATree.Editor.Control).Items.AddObject(TsmxBaseCell(FindList[i]).Name, FindList[i]);
        finally
          FindList.Free;
        end;
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
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomTree then
    begin
      Tree := TsmxCustomTree(TsmxCustomAlgorithm(Sender).CellEvent);
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
        Tree.Editor.EditorType := EditorType;
        s := Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
        case PropInfo^.PropType^^.Kind of
          tkClass:
            FillClassProp(Obj, PropInfo, Tree, s);
          tkInterface:
            FillIntfProp(Obj, PropInfo, Tree, s);
          tkEnumeration:
            FillEnumProp(Obj, PropInfo, Tree, s);
          tkSet:
            FillSetProp(Obj, PropInfo, Tree, s);
          else
            FillVarProp(Obj, PropInfo, Tree, s);
        end;

        {case EditorType of
          etString:
          begin
            if Tree.Editor.Control is TEdit then
              TEdit(Tree.Editor.Control).Text := Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex];
          end;
          etPickString:
          begin
            if Tree.Editor.Control is TComboBox then
              with TComboBox(Tree.Editor.Control) do
                case PropInfo^.PropType^^.Kind of
                  tkClass:
                  begin
                    FillClassProp(Obj, PropInfo, TComboBox(Tree.Editor.Control));
                    ItemIndex := Items.IndexOf(Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex]);
                  end;
                  tkInterface:
                  begin
                    FillIntfProp(Obj, PropInfo, TComboBox(Tree.Editor.Control));
                    ItemIndex := Items.IndexOf(Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex]);
                  end;
                  tkEnumeration:
                  begin
                    FillEnumProp(Obj, PropInfo, TComboBox(Tree.Editor.Control));
                    ItemIndex := Items.IndexOf(Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex]);
                  end;
                  tkSet:
                  begin
                    Items.Add(SysUtils.DefaultFalseBoolStr);
                    Items.Add(SysUtils.DefaultTrueBoolStr);
                    ItemIndex := Items.IndexOf(Tree.TreeCaptions[Tree.Editor.ColIndex, Tree.Editor.RowIndex]);
                  end;
                end;
          end;}
            {end;
          vtPickString:
            begin
              FEdit := TComboBox.Create(nil);
              with FEdit as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value;
                Items.Add(Text);
                Items.Add('Standard');
                Items.Add('Additional');
                Items.Add('Win32');
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtNumber:
            begin
              FEdit := TMaskEdit.Create(nil);
              with FEdit as TMaskEdit do
              begin
                Visible := False;
                Parent := Tree;
                EditMask := '9999';
                Text := Data.Value;
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtPickNumber:
            begin
              FEdit := TComboBox.Create(nil);
              with FEdit as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value;
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtMemo:
            begin
              FEdit := TComboBox.Create(nil);
              // In reality this should be a drop down memo but this requires
              // a special control.
              with FEdit as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value;
                Items.Add(Data.Value);
                OnKeyDown := EditKeyDown;
              end;
            end;
          vtDate:
            begin
              FEdit := TDateTimePicker.Create(nil);
              with FEdit as TDateTimePicker do
              begin
                Visible := False;
                Parent := Tree;
                CalColors.MonthBackColor := clWindow;
                CalColors.TextColor := clBlack;
                CalColors.TitleBackColor := clBtnShadow;
                CalColors.TitleTextColor := clBlack;
                CalColors.TrailingTextColor := clBtnFace;
                Date := StrToDate(Data.Value);
                OnKeyDown := EditKeyDown;
              end;
            end;}
        //else
          //Result := False;
        //end;
      end;
    end;
end;

end.
