unit smxClassProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxBaseClasses, smxClasses, smxManagerIntf,
  smxClassFuncs;

procedure AllCells(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIsOnlyActive: Boolean = False; AIncludeChildForm: Boolean = False);
procedure AllParents(ACell: TsmxBaseCell; AList: TList;
  AClassList: array of TsmxBaseCellClass);
procedure ClearProps(AObject: TObject);
procedure ReadProps(AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
procedure WriteProps(AObject: TObject; const ANode: IXMLNode);
procedure ResolvedProps(AObject: TObject; AResolvedList: TsmxResolvedKit);

var
  gSelectRequest: TsmxCustomRequest = nil;
  gStorageManagerIntf: IsmxStorageManager = nil;
  gLibraryManagerIntf: IsmxLibraryManager = nil;
  gDatabaseManagerIntf: IsmxDatabaseManager = nil;
  gFormManagerIntf: IsmxFormManager = nil;
  gImageListManagerIntf: IsmxImageListManager = nil;

implementation

uses
  Variants, SysUtils, TypInfo, smxConsts, smxDBIntf, smxRefIntf;

procedure AllCells(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIsOnlyActive: Boolean = False; AIncludeChildForm: Boolean = False);

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
    if not AIncludeChildForm and (ACurCell is TsmxCustomForm) then
      Exit;
    if (AIsEmpty or IsClass(ACurCell)) and (not AIsOnlyActive or IsActive(ACurCell)) then
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

procedure ClearProps(AObject: TObject);

  procedure ClearClass(PropInfo: PPropInfo);
  var
    Obj: TObject;
    Cls: TClass;
  begin
    Cls := TypInfo.GetTypeData(PropInfo^.PropType^)^.ClassType;
    Obj := TypInfo.GetObjectProp(AObject, PropInfo);
    if Cls.InheritsFrom(TsmxKit) then
      TsmxKit(Obj).Clear else
    if Cls.InheritsFrom(TCollection) then
      TCollection(Obj).Clear else
    if Cls.InheritsFrom(TsmxBaseCell) then
      TypInfo.SetObjectProp(AObject, PropInfo, nil) else
    if Cls.InheritsFrom(TComponent) then
      TypInfo.SetObjectProp(AObject, PropInfo, nil) else
    if Assigned(Obj) then
      ClearProps(Obj);
  end;

  procedure ClearIntf(PropInfo: PPropInfo);

    function IsImplIntf(const AIntf: IInterface): Boolean;
    var
      Intf: IsmxRefInterface;
    begin
      Result := AObject.GetInterface(IsmxRefInterface, Intf) and (Intf = AIntf);
    end;

  var
    Intf: IInterface;
    GUID: TGUID;
  begin
    if AObject is TsmxBaseCell then
    begin
      GUID := TypInfo.GetTypeData(PropInfo^.PropType^)^.Guid;
      Intf := TypInfo.GetInterfaceProp(AObject, PropInfo);
      if SysUtils.IsEqualGUID(GUID, IsmxDataSet)
          or SysUtils.Supports(Intf, IsmxDataSet) then
      begin
        if IsImplIntf(Intf) then
          ClearProps(IsmxDataSet(Intf).GetReference)
        else
          TypInfo.SetInterfaceProp(AObject, PropInfo, nil);
      end else
      if SysUtils.IsEqualGUID(GUID, IsmxRefInterface)
          or SysUtils.Supports(Intf, IsmxRefInterface) then
      begin
        if IsImplIntf(Intf) then
          ClearProps(IsmxRefInterface(Intf).GetReference)
        else
          TypInfo.SetInterfaceProp(AObject, PropInfo, nil);
      end;
    end;
  end;

  procedure ClearMethod(PropInfo: PPropInfo);
  const
    NilMethod: TMethod = (Code: nil; Data: nil);
  begin
    if AObject is TsmxBaseCell then
      TypInfo.SetMethodProp(AObject, PropInfo, NilMethod);
  end;

var
  Count: Integer;
  PropList: PPropList;
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AObject) then
    Exit;
  Count := TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo),
    TypInfo.tkProperties + TypInfo.tkMethods, nil);
  if Count <> 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo),
        TypInfo.tkProperties + TypInfo.tkMethods, PropList);
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        if TypInfo.IsStoredProp(AObject, PropInfo) then
        begin
          case PropInfo^.PropType^^.Kind of
            tkClass:
              ClearClass(PropInfo);
            tkInterface:
              ClearIntf(PropInfo);
            tkMethod:
              ClearMethod(PropInfo);
            tkInteger, tkFloat, tkChar, tkWChar, tkEnumeration, tkSet:
              if TypInfo.IsStoredProp(AObject, PropInfo) then
                TypInfo.SetOrdProp(AObject, PropInfo, 0);
            tkString, tkLString, tkWString:
              if TypInfo.IsStoredProp(AObject, PropInfo) then
                TypInfo.SetStrProp(AObject, PropInfo, '');
            tkVariant:
              if TypInfo.IsStoredProp(AObject, PropInfo) then
                TypInfo.SetVariantProp(AObject, PropInfo, Variants.Null);
            tkInt64:
              if TypInfo.IsStoredProp(AObject, PropInfo) then
                TypInfo.SetInt64Prop(AObject, PropInfo, 0);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure ReadProps(AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);

  procedure ReadClass(PropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
    Cls: TClass;
  begin
    n := Node.ChildNodes.FindNode(PropInfo^.Name);
    if Assigned(n) then
    begin
      Cls := TypInfo.GetTypeData(PropInfo^.PropType^)^.ClassType;
      Obj := TypInfo.GetObjectProp(AObject, PropInfo);
      if Cls.InheritsFrom(TsmxKit) then
      begin
        ReadProps(Obj, n, AResolvedList);
        for i := 0 to n.ChildNodes.Count - 1 do
          if n.ChildNodes[i].NodeName = smxConsts.cItemNodeName then
            ReadProps(TsmxKit(Obj).Add, n.ChildNodes[i], AResolvedList);
      end else
      if Cls.InheritsFrom(TCollection) then
      begin
        ReadProps(Obj, n, AResolvedList);
        for i := 0 to n.ChildNodes.Count - 1 do
          if n.ChildNodes[i].NodeName = smxConsts.cItemNodeName then
            ReadProps(TCollection(Obj).Add, n.ChildNodes[i], AResolvedList);
      end else
      if Cls.InheritsFrom(TsmxBaseCell) then
      begin
        if Assigned(AResolvedList) then
          if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
            with AResolvedList.Add do
            begin
              Instance := AObject;
              PropInfo := PropInfo;
              PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
            end;
      end else
      if Cls.InheritsFrom(TComponent) then
      begin
        if Assigned(AResolvedList) then
          if n.Attributes[smxConsts.cNameAttributeName] <> '' then
            with AResolvedList.Add do
            begin
              Instance := AObject;
              PropInfo := PropInfo;
              PropValue := n.Attributes[smxConsts.cNameAttributeName];
            end;
      end else
      begin
        if Assigned(Obj) then
          ReadProps(Obj, n, AResolvedList);
      end;
    end;
  end;

  procedure ReadIntf(PropInfo: PPropInfo; const Node: IXMLNode);

    function IsImplIntf(const AIntf: IInterface): Boolean;
    var
      Intf: IsmxRefInterface;
    begin
      Result := AObject.GetInterface(IsmxRefInterface, Intf) and (Intf = AIntf);
    end;

  var
    Intf: IInterface;
    n: IXMLNode;
    GUID: TGUID;
  begin
    n := Node.ChildNodes.FindNode(PropInfo^.Name);
    if Assigned(n) and (AObject is TsmxBaseCell) then
    begin
      GUID := TypInfo.GetTypeData(PropInfo^.PropType^)^.Guid;
      Intf := TypInfo.GetInterfaceProp(AObject, PropInfo);
      if SysUtils.IsEqualGUID(GUID, IsmxDataSet)
          or SysUtils.Supports(Intf, IsmxDataSet) then
      begin
        if IsImplIntf(Intf) then
          ReadProps(IsmxDataSet(Intf).GetReference, n, AResolvedList)
        else
        begin
          if Assigned(AResolvedList) then
            if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
              with AResolvedList.Add do
              begin
                Instance := AObject;
                PropInfo := PropInfo;
                PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
              end;
        end;
      end else
      if SysUtils.IsEqualGUID(GUID, IsmxRefInterface)
          or SysUtils.Supports(Intf, IsmxRefInterface) then
      begin
        if IsImplIntf(Intf) then
          ReadProps(IsmxRefInterface(Intf).GetReference, n, AResolvedList)
        else
        begin
          if Assigned(AResolvedList) then
            if n.Attributes[smxConsts.cNameAttributeName] <> '' then
              with AResolvedList.Add do
              begin
                Instance := AObject;
                PropInfo := PropInfo;
                PropValue := n.Attributes[smxConsts.cNameAttributeName];
              end;
        end;
      end;
    end;
  end;

  procedure ReadMethod(PropInfo: PPropInfo; const Node: IXMLNode);
  var
    n: IXMLNode;
  begin
    n := Node.ChildNodes.FindNode(PropInfo^.Name);
    if Assigned(n) and (AObject is TsmxBaseCell) then
    begin
      if Assigned(AResolvedList) then
        if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
          with AResolvedList.Add do
          begin
            Instance := AObject;
            PropInfo := PropInfo;
            PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
          end;
    end;
  end;

var
  Count: Integer;
  PropList: PPropList;
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AObject) or not Assigned(ANode) then
    Exit;
  Count := TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo),
    TypInfo.tkProperties + TypInfo.tkMethods, nil);
  if Count <> 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo),
        TypInfo.tkProperties + TypInfo.tkMethods, PropList);
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        if TypInfo.IsStoredProp(AObject, PropInfo) then
        begin
          case PropInfo^.PropType^^.Kind of
            tkClass:
              ReadClass(PropInfo, ANode);
            tkInterface:
              ReadIntf(PropInfo, ANode);
            tkMethod:
              ReadMethod(PropInfo, ANode);
            tkEnumeration:
              TypInfo.SetEnumProp(AObject, PropInfo, ANode.Attributes[PropInfo^.Name]);
            tkSet:
              TypInfo.SetSetProp(AObject, PropInfo, ANode.Attributes[PropInfo^.Name]);
            else
              TypInfo.SetPropValue(AObject, PropInfo^.Name, ANode.Attributes[PropInfo^.Name]);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure WriteProps(AObject: TObject; const ANode: IXMLNode);

  procedure WriteClass(PropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
  begin
    Obj := TypInfo.GetObjectProp(AObject, PropInfo);
    if Assigned(Obj) then
    begin
      n := Node.AddChild(PropInfo^.Name);
      if Obj is TsmxKit then
      begin
        WriteProps(Obj, n);
        for i := 0 to TsmxKit(Obj).Count - 1 do
          WriteProps(TsmxKit(Obj)[i], n.AddChild(smxConsts.cItemNodeName));
      end else
      if Obj is TCollection then
      begin
        WriteProps(Obj, n);
        for i := 0 to TCollection(Obj).Count - 1 do
          WriteProps(TCollection(Obj).Items[i], n.AddChild(smxConsts.cItemNodeName));
      end else
      if Obj is TsmxBaseCell then
      begin
        n.Attributes[smxConsts.cCfgIDAttributeName] := TsmxBaseCell(Obj).CfgID;
      end else
      if Obj is TComponent then
      begin
        n.Attributes[smxConsts.cNameAttributeName] := TComponent(Obj).Name;
      end else
      begin
        WriteProps(Obj, n);
      end;
    end;
  end;

  procedure WriteIntf(PropInfo: PPropInfo; const Node: IXMLNode);

    function IsImplIntf(const AIntf: IInterface): Boolean;
    var
      Intf: IsmxRefInterface;
    begin
      Result := AObject.GetInterface(IsmxRefInterface, Intf) and (Intf = AIntf);
    end;

  var
    Intf: IInterface;
    Obj: TObject;
    n: IXMLNode;
    Form: TsmxCustomForm;
  begin
    Intf := TypInfo.GetInterfaceProp(AObject, PropInfo);
    if Assigned(Intf) and (AObject is TsmxBaseCell) then
    begin
      n := Node.AddChild(PropInfo^.Name);
      if SysUtils.Supports(Intf, IsmxDataSet) then
      begin
        if IsImplIntf(Intf) then
          WriteProps(IsmxDataSet(Intf).GetReference, n)
        else
        begin
          Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(AObject));
          if Assigned(Form) then
          begin
            Obj := smxClassFuncs.GetRequestForm(Form, IsmxDataSet(Intf));
            if Obj is TsmxBaseCell then
              n.Attributes[smxConsts.cCfgIDAttributeName] := TsmxBaseCell(Obj).CfgID else
              n.Attributes[smxConsts.cCfgIDAttributeName] := 0;
          end;
        end;
      end else
      if SysUtils.Supports(Intf, IsmxRefInterface) then
      begin
        if IsImplIntf(Intf) then
          WriteProps(IsmxRefInterface(Intf).GetReference, n)
        else
        begin
          Obj := IsmxRefInterface(Intf).GetReference;
          if Obj is TsmxInterfacedPersistent then
            n.Attributes[smxConsts.cNameAttributeName] := TsmxInterfacedPersistent(Obj).Name else
            n.Attributes[smxConsts.cNameAttributeName] := '';
        end;
      end;
    end;
  end;

  procedure WriteMethod(PropInfo: PPropInfo; const Node: IXMLNode);
  var
    Method: TMethod;
    Obj: TObject;
    n: IXMLNode;
    Form: TsmxCustomForm;
  begin
    Method := TypInfo.GetMethodProp(AObject, PropInfo);
    if Assigned(Method.Code) and (AObject is TsmxBaseCell) then
    begin
      Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(AObject));
      if Assigned(Form) then
      begin
        n := ANode.AddChild(PropInfo^.Name);
        Obj := smxClassFuncs.GetAlgorithmForm(Form, TsmxComponentEvent(Method));
        if Obj is TsmxBaseCell then
          n.Attributes[smxConsts.cCfgIDAttributeName] := TsmxBaseCell(Obj).CfgID else
          n.Attributes[smxConsts.cCfgIDAttributeName] := 0;
      end;
    end;
  end;

var
  Count: Integer;
  PropList: PPropList;
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AObject) or not Assigned(ANode) then
    Exit;
  Count := TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo),
    TypInfo.tkProperties + TypInfo.tkMethods, nil);
  if Count <> 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      TypInfo.GetPropList(PTypeInfo(AObject.ClassInfo),
        TypInfo.tkProperties + TypInfo.tkMethods, PropList);
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList^[i];
        if TypInfo.IsStoredProp(AObject, PropInfo) then
        begin
          case PropInfo^.PropType^^.Kind of
            tkClass:
              WriteClass(PropInfo, ANode);
            tkInterface:
              WriteIntf(PropInfo, ANode);
            tkMethod:
              WriteMethod(PropInfo, ANode);
            tkEnumeration:
              ANode.Attributes[PropInfo^.Name] := TypInfo.GetEnumProp(AObject, PropInfo);
            tkSet:
              ANode.Attributes[PropInfo^.Name] := TypInfo.GetSetProp(AObject, PropInfo);
            else
              ANode.Attributes[PropInfo^.Name] := TypInfo.GetPropValue(AObject, PropInfo^.Name);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure ResolvedProps(AObject: TObject; AResolvedList: TsmxResolvedKit);

  procedure ResolvedClass(Item: TsmxResolvedItem);
  var
   Cls: TClass;
  begin
    Cls := TypInfo.GetTypeData(Item.PropInfo^.PropType^)^.ClassType;
    if Cls.InheritsFrom(TsmxCustomAlgorithm) and (AObject is TsmxCustomForm) then
    begin
      TypInfo.SetObjectProp(Item.Instance, Item.PropInfo,
        smxClassFuncs.GetAlgorithmForm(TsmxCustomForm(AObject), Item.PropValue));
    end else
    if Cls.InheritsFrom(TsmxCustomRequest) and (AObject is TsmxCustomForm) then
    begin
      TypInfo.SetObjectProp(Item.Instance, Item.PropInfo,
        smxClassFuncs.GetRequestForm(TsmxCustomForm(AObject), Item.PropValue));
    end else
    if Cls.InheritsFrom(TsmxBaseCell) and (AObject is TsmxBaseCell) then
    begin
      TypInfo.SetObjectProp(Item.Instance, Item.PropInfo,
        TsmxBaseCell(AObject).FindChildByCfgID(Item.PropValue));
    end;
  end;

  procedure ResolvedIntf(PropInfo: PPropInfo);
  begin
  end;

  procedure ResolvedMethod(PropInfo: PPropInfo);
  begin
  end;

var
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AObject) or not Assigned(AResolvedList) then
    Exit;
  for i := 0 to AResolvedList.Count - 1 do
  begin
    PropInfo := AResolvedList[i].PropInfo;
    case PropInfo^.PropType^^.Kind of
      tkClass:;
      tkInterface:;
      tkMethod:;
    end;
  end;
end;

procedure ReadCells(ACell: TsmxBaseCell; const ANode: IXMLNode);
{var
  i: Integer;
  n: IXMLNode;
  Cell, CellParent: TsmxBaseCell;}
begin
  {if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  n := ANode.ChildNodes.FindNode(smxConsts.cParentNodeName);
  if Assigned(n) then
  begin
    //if n.HasAttribute('CfgID') then

    //CellParent := ACell.FindChildByCfgID()
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = smxConsts.cCellNodeName then
        if n.ChildNodes[i].HasAttribute('CfgID') then
        begin
          //Cell := smxClassFuncs.NewCell();
          Cell.CellParent := ACell;
          //ReadProps(Cell, n.ChildNodes[i]);
        end;
  end;
  n := ANode.ChildNodes.FindNode(smxConsts.cOwnerNodeName);
  if Assigned(n) then
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = smxConsts.cSlaveNodeName then
        if n.ChildNodes[i].HasAttribute('CfgID') then
        begin
          //Cell := smxClassFuncs.NewCell();
          Cell.CellParent := ACell;
          //ReadProps(Cell, n.ChildNodes[i]);
        end;}
end;

procedure WriteCells(ACell: TsmxBaseCell; const ANode: IXMLNode);

  function IsWriteChild(ACell: TsmxBaseCell): Boolean;
  begin
    Result := True;
    if (ACell is TsmxOwnerCell) then
      if Assigned(TsmxOwnerCell(ACell).CellOwner) then
        Result := not TsmxOwnerCell(ACell).CellOwner.IsOwnerIsParent;
  end;

var
  i: Integer;
  n: IXMLNode;
begin
  {if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  WriteProps(ACell, ANode);
  if ACell.CellCount <> 0 then
  begin
    n := ANode.AddChild(smxConsts.cCellsNodeName);
    for i := 0 to ACell.CellCount - 1 do
      if IsWriteChild(ACell.Cells[i]) then
        WriteCells(ACell.Cells[i], n.AddChild(smxConsts.cCellNodeName));
  end;
  if ACell is TsmxOwnerCell then
    if TsmxOwnerCell(ACell).SlaveCount <> 0 then
    begin
      n := ANode.AddChild(smxConsts.cCellsNodeName);
      for i := 0 to TsmxOwnerCell(ACell).SlaveCount - 1 do
        WriteCells(TsmxOwnerCell(ACell).Slaves[i], n.AddChild(smxConsts.cCellNodeName));
    end;}


end;

end.
