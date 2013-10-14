unit smxClassProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxBaseClasses, smxClasses, smxManagerIntf{,
  smxClassFuncs};

procedure AllCells(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIsOnlyActive: Boolean = False; AIncludeChildForm: Boolean = False);
procedure AllParents(ACell: TsmxBaseCell; AList: TList;
  AClassList: array of TsmxBaseCellClass);
procedure ClearProps(AObject: TObject);
procedure ReadProps(AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
procedure WriteProps(AObject: TObject; const ANode: IXMLNode; AFindList: TList);
procedure ResolvedProps(AResolvedList: TsmxResolvedKit; AFindList: TList);
procedure ReadCell(ACell: TsmxBaseCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
procedure WriteCell(ACell: TsmxBaseCell; const ANode: IXMLNode; AFindList: TList);
procedure ReadSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
procedure WriteSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; AFindList: TList);

var
  gSelectRequest: TsmxCustomRequest = nil;
  //gSelectDataSet: IsmxDataSet = nil;
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
      TsmxKit(Obj).Clear {else
    if Cls.InheritsFrom(TCollection) then
      TCollection(Obj).Clear} else
    if Cls.InheritsFrom(TsmxBaseCell) then
      TypInfo.SetObjectProp(AObject, PropInfo, nil) {else
    if Cls.InheritsFrom(TComponent) then
      TypInfo.SetObjectProp(AObject, PropInfo, nil)} else
    if Assigned(Obj) then
      ClearProps(Obj);
  end;

  procedure ClearIntf(PropInfo: PPropInfo);

    function IsImplIntf(const AIntf: IsmxRefInterface): Boolean;
    var
      Intf: IsmxRefInterface;
    begin
      Result := AObject.GetInterface(IsmxRefInterface, Intf)
        and (IsmxRefInterface(Intf).GetReference = AIntf.GetReference);
    end;

  var
    Intf: IInterface;
    GUID: TGUID;
  begin
    if AObject is TsmxBaseCell then
    begin
      GUID := TypInfo.GetTypeData(PropInfo^.PropType^)^.Guid;
      Intf := TypInfo.GetInterfaceProp(AObject, PropInfo);
      if {SysUtils.IsEqualGUID(GUID, IsmxDataSet)
          or} SysUtils.Supports(Intf, IsmxRefInterface) {then
      begin
        if} and IsImplIntf(IsmxRefInterface(Intf)) then
          ClearProps(IsmxRefInterface(Intf).GetReference)
        else
          TypInfo.SetInterfaceProp(AObject, PropInfo, nil);
      //end
       {else
      if SysUtils.IsEqualGUID(GUID, IsmxRefInterface)
          or SysUtils.Supports(Intf, IsmxRefInterface) then
      begin
        if IsImplIntf(Intf) then
          ClearProps(IsmxRefInterface(Intf).GetReference)
        else
          TypInfo.SetInterfaceProp(AObject, PropInfo, nil);
      end};
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
              TypInfo.SetOrdProp(AObject, PropInfo, 0);
            tkString, tkLString, tkWString:
              TypInfo.SetStrProp(AObject, PropInfo, '');
            tkVariant:
              TypInfo.SetVariantProp(AObject, PropInfo, Variants.Null);
            tkInt64:
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
      end {else
      if Cls.InheritsFrom(TCollection) then
      begin
        ReadProps(Obj, n, AResolvedList);
        for i := 0 to n.ChildNodes.Count - 1 do
          if n.ChildNodes[i].NodeName = smxConsts.cItemNodeName then
            ReadProps(TCollection(Obj).Add, n.ChildNodes[i], AResolvedList);
      end} else
      if Cls.InheritsFrom(TsmxBaseCell) then
      begin
        if Assigned(AResolvedList) then
          if (n.Attributes[smxConsts.cCfgIDAttributeName] <> 0)
              or (n.Attributes[smxConsts.cNameAttributeName] <> '') then
            with AResolvedList.Add do
            begin
              Instance := AObject;
              PropInfo := PropInfo;
              if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
              begin
                //TVarData(PropValue).VType := vtInteger;
                PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
              end else
              begin
                //TVarData(PropValue).VType := vtString;
                PropValue := n.Attributes[smxConsts.cNameAttributeName];
              end;
            end;
      end {else
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
      end} else
      begin
        if Assigned(Obj) then
          ReadProps(Obj, n, AResolvedList);
      end;
    end;
  end;

  procedure ReadIntf(PropInfo: PPropInfo; const Node: IXMLNode);

    function IsImplIntf(const AIntf: IsmxRefInterface): Boolean;
    var
      Intf: IsmxRefInterface;
    begin
      Result := AObject.GetInterface(IsmxRefInterface, Intf)
        and (IsmxRefInterface(Intf).GetReference = AIntf.GetReference);
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
      if {SysUtils.IsEqualGUID(GUID, IsmxRefInterface)
          or} SysUtils.Supports(Intf, IsmxRefInterface) {then
      begin
        if} and IsImplIntf(IsmxRefInterface(Intf)) then
          ReadProps(IsmxRefInterface(Intf).GetReference, n, AResolvedList)
        else
        begin
          if Assigned(AResolvedList) then
          //begin
            if (n.Attributes[smxConsts.cCfgIDAttributeName] <> 0)
                or (n.Attributes[smxConsts.cNameAttributeName] <> '') then
              with AResolvedList.Add do
              begin
                Instance := AObject;
                PropInfo := PropInfo;
                if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
                begin
                  //TVarData(PropValue).VType := vtInteger;
                  PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
                end else
                begin
                  //TVarData(PropValue).VType := vtString;
                  PropValue := n.Attributes[smxConsts.cNameAttributeName];
                end;
              end;
          //end;
        end;
      //end
      {else
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
      end}//;
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
        if ((n.Attributes[smxConsts.cCfgIDAttributeName] <> 0)
              or (n.Attributes[smxConsts.cNameAttributeName] <> ''))
            and (n.Attributes[smxConsts.cProcNameAttributeName] <> '') then
          with AResolvedList.Add do
          begin
            Instance := AObject;
            PropInfo := PropInfo;
            if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
            begin
              //TVarData(PropValue).VType := vtInteger;
              PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
            end else
            begin
              //TVarData(PropValue).VType := vtString;
              PropValue := n.Attributes[smxConsts.cNameAttributeName];
            end;
            ProcName := n.Attributes[smxConsts.cProcNameAttributeName];
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
              if ANode.HasAttribute(PropInfo^.Name) then
                TypInfo.SetPropValue(AObject, PropInfo^.Name, ANode.Attributes[PropInfo^.Name]);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

procedure WriteProps(AObject: TObject; const ANode: IXMLNode; AFindList: TList);

  procedure WriteClass(PropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
  begin
    Obj := TypInfo.GetObjectProp(AObject, PropInfo);
    if Assigned(Obj) then
    begin
      n := Node.AddChild(PropInfo^.Name, 0);
      {if Obj is TsmxSlaveList then
      begin
        WriteProps(Obj, n, AFindList);
        for i := 0 to TsmxSlaveList(Obj).Count - 1 do
        begin
          n2 := n.AddChild(smxConsts.cItemNodeName)
          if Assigned(TsmxSlaveList(Obj)[i].Slave) then
          begin
            n2.Attributes[smxConsts.cIClassNameAttributeName] := '';
            n2.Attributes[smxConsts.cClassNameAttributeName] := TsmxSlaveList(Obj)[i].Slave.ClassName;
            //n2.Attributes[smxConsts.cCfgIDAttributeName] := TsmxSlaveList(Obj)[i].Slave.CfgID;
          end;
          WriteProps(TsmxSlaveList(Obj)[i], n2, AFindList);
        end;    // лучше вынести в отдельную процедуру, т.к. много особенностей
      end else}
      if Obj is TsmxKit then
      begin
        WriteProps(Obj, n, AFindList);
        for i := 0 to TsmxKit(Obj).Count - 1 do
          WriteProps(TsmxKit(Obj)[i], n.AddChild(smxConsts.cItemNodeName), AFindList);
      end {else
      if Obj is TCollection then
      begin
        WriteProps(Obj, n);
        for i := 0 to TCollection(Obj).Count - 1 do
          WriteProps(TCollection(Obj).Items[i], n.AddChild(smxConsts.cItemNodeName));
      end} else
      if Obj is TsmxBaseCell then
      begin
        if TsmxBaseCell(Obj).CfgID <> 0 then
          n.Attributes[smxConsts.cCfgIDAttributeName] := TsmxBaseCell(Obj).CfgID
        else
          n.Attributes[smxConsts.cNameAttributeName] := TsmxBaseCell(Obj).Name;
      end {else
      if Obj is TComponent then
      begin
        n.Attributes[smxConsts.cNameAttributeName] := TComponent(Obj).Name;
      end} else
      begin
        WriteProps(Obj, n, AFindList);
      end;
    end;
  end;

  procedure WriteIntf(PropInfo: PPropInfo; const Node: IXMLNode);

    function IsImplIntf(const AIntf: IsmxRefInterface): Boolean;
    var
      Intf: IsmxRefInterface;
    begin
      Result := AObject.GetInterface(IsmxRefInterface, Intf)
        and (IsmxRefInterface(Intf).GetReference = AIntf.GetReference);
    end;

    function FindImplObj(const AIntf: IsmxRefInterface): TObject;
    var
      i: Integer;
      Intf: IInterface;
    begin
      Result := nil;
      if Assigned(AFindList) then
      begin
        for i := 0 to AFindList.Count - 1 do
          if TObject(AFindList[i]).GetInterface(IsmxRefInterface, Intf)
              and (IsmxRefInterface(Intf).GetReference = AIntf.GetReference) then
          begin
            Result := AFindList[i];
            Break;
          end;
      end;
    end;

  var
    Intf: IInterface;
    Obj: TObject;
    n: IXMLNode;
    //Form: TsmxCustomForm;
  begin
    Intf := TypInfo.GetInterfaceProp(AObject, PropInfo);
    if Assigned(Intf) and (AObject is TsmxBaseCell) then
    begin
      n := Node.AddChild(PropInfo^.Name, 0);
      if SysUtils.Supports(Intf, IsmxRefInterface) then
      begin
        if IsImplIntf(IsmxRefInterface(Intf)) then
          WriteProps(IsmxRefInterface(Intf).GetReference, n, AFindList)
        else
        //begin
          //Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(AObject));
          //if Assigned(AChildList) then
        begin
          Obj := FindImplObj(IsmxRefInterface(Intf));
          if Obj is TsmxBaseCell then
          begin
            if TsmxBaseCell(Obj).CfgID <> 0 then
              n.Attributes[smxConsts.cCfgIDAttributeName] := TsmxBaseCell(Obj).CfgID
            else
              n.Attributes[smxConsts.cNameAttributeName] := TsmxBaseCell(Obj).Name;
          end;
        end;
        //end;
      end {else
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
      end};
    end;
  end;

  procedure WriteMethod(PropInfo: PPropInfo; const Node: IXMLNode);

    function FindMethodObj(Method: TMethod): TObject;
    var
      i: Integer;
    begin
      Result := nil;
      if Assigned(AFindList) then
      begin
        for i := 0 to AFindList.Count - 1 do
          if TObject(AFindList[i]).MethodName(Method.Code) <> '' then
          begin
            Result := TObject(AFindList[i]);
            Break;
          end;
      end;
    end;

  var
    Method: TMethod;
    Obj: TObject;
    n: IXMLNode;
    //Form: TsmxCustomForm;
  begin
    Method := TypInfo.GetMethodProp(AObject, PropInfo);
    if Assigned(Method.Code) and (AObject is TsmxBaseCell) then
    begin
      n := ANode.AddChild(PropInfo^.Name, 0);
      //Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(AObject));
      //if Assigned(Form) then
      //begin
        //Obj := smxClassFuncs.GetAlgorithmForm(Form, TsmxComponentEvent(Method));
        Obj := FindMethodObj(Method);
        if Obj is TsmxBaseCell then
        begin
          if TsmxBaseCell(Obj).CfgID <> 0 then
            n.Attributes[smxConsts.cCfgIDAttributeName] := TsmxBaseCell(Obj).CfgID
          else
            n.Attributes[smxConsts.cNameAttributeName] := TsmxBaseCell(Obj).Name;
          n.Attributes[smxConsts.cProcNameAttributeName] := Obj.MethodName(Method.Code);
        end;
      //end;
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
      for i := Count - 1 downto 0 do
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
              ANode.Attributes[PropInfo^.Name] := TypInfo.GetSetProp(AObject, PropInfo, True);
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

procedure ResolvedProps(AResolvedList: TsmxResolvedKit; AFindList: TList);

  procedure ResolvedClass(Item: TsmxResolvedItem);
  var
   Cls: TClass;
   i: Integer;
  begin
     if Assigned(AFindList) then
     begin
       Cls := TypInfo.GetTypeData(Item.PropInfo^.PropType^)^.ClassType;
       for i := 0 to AFindList.Count - 1 do
         if TObject(AFindList[i]).InheritsFrom(Cls) and (TObject(AFindList[i]) is TsmxBaseCell) then
           if (TsmxBaseCell(AFindList[i]).CfgID = Item.PropValue)
               or (TsmxBaseCell(AFindList[i]).Name = Item.PropValue) then
           begin
             TypInfo.SetObjectProp(Item.Instance, Item.PropInfo, TObject(AFindList[i]));
             Break;
           end;
     end;
  end;

  procedure ResolvedIntf(Item: TsmxResolvedItem);
  var
    GUID: TGUID;
    i: Integer;
    Intf: IInterface;
  begin
    if Assigned(AFindList) then
     begin
       GUID := TypInfo.GetTypeData(Item.PropInfo^.PropType^)^.Guid;
       for i := 0 to AFindList.Count - 1 do
         if SysUtils.Supports(TObject(AFindList[i]), GUID, Intf) and (TObject(AFindList[i]) is TsmxBaseCell) then
           if (TsmxBaseCell(AFindList[i]).CfgID = Item.PropValue)
               or (TsmxBaseCell(AFindList[i]).Name = Item.PropValue) then
           begin
             TypInfo.SetInterfaceProp(Item.Instance, Item.PropInfo, Intf);
             Break;
           end;
     end;
  end;

  procedure ResolvedMethod(Item: TsmxResolvedItem);
  var
    Method: TMethod;
    i: Integer;
  begin
    if Assigned(AFindList) then
     begin
       //Method := TypInfo.GetTypeData(Item.PropInfo^.PropType^)^.Guid;
       for i := 0 to AFindList.Count - 1 do
         if TObject(AFindList[i]) is TsmxBaseCell then
           if (TsmxBaseCell(AFindList[i]).CfgID = Item.PropValue)
               or (TsmxBaseCell(AFindList[i]).Name = Item.PropValue) then
           begin
             Method.Data := AFindList[i];
             Method.Code := TObject(AFindList[i]).MethodAddress(Item.ProcName);
             TypInfo.SetMethodProp(Item.Instance, Item.PropInfo, Method);
             Break;
           end;
     end;
  end;

var
  i: Integer;
  PropInfo: PPropInfo;
begin
  if not Assigned(AResolvedList) then
    Exit;
  for i := 0 to AResolvedList.Count - 1 do
  begin
    PropInfo := AResolvedList[i].PropInfo;
    case PropInfo^.PropType^^.Kind of
      tkClass:
        ResolvedClass(AResolvedList[i]);
      tkInterface:
        ResolvedIntf(AResolvedList[i]);
      tkMethod:
        ResolvedMethod(AResolvedList[i]);
    end;
  end;
end;

procedure ReadCell(ACell: TsmxBaseCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);

  function GetImplIntf(const ClassName: String): IsmxRefInterface;
  begin
    Result := nil;
    if ClassName <> '' then
      if SysUtils.Supports(Classes.FindClass(ClassName), IsmxRefInterface) then
        Classes.FindClass(ClassName).Create.GetInterface(IsmxRefInterface, Result);
  end;

var
  i: Integer;
  n: IXMLNode;
  Cell: TsmxBaseCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    n := ANode.ChildNodes[i];
    if n.NodeName = smxConsts.cCellNodeName then
    begin
      if n.Attributes[smxConsts.cClassNameAttributeName] <> '' then
      begin
        if n.Attributes[smxConsts.cIClassNameAttributeName] <> '' then
          Cell := TsmxBaseCellClass(Classes.FindClass(n.Attributes[smxConsts.cClassNameAttributeName])).CreateByImpl(
            nil, GetImplIntf(n.Attributes[smxConsts.cIClassNameAttributeName]))
        else
          Cell := TsmxBaseCellClass(Classes.FindClass(n.Attributes[smxConsts.cClassNameAttributeName])).Create(nil);
      end else
        Cell := nil;
      if Assigned(Cell) then
      begin
        Cell.CfgID := n.Attributes[smxConsts.cCfgIDAttributeName];
        Cell.CellParent := ACell;
        // м.б. глобалные объекты не присваивать... где-то уже это было...
        {Cell.StorageManager := gStorageManagerIntf;
        Cell.LibraryManager := gLibraryManagerIntf;
        Cell.DatabaseManager := gDatabaseManagerIntf;
        Cell.FormManager := gFormManagerIntf;
        Cell.ImageListManager := gImageListManagerIntf;}
        ReadProps(Cell, n, AResolvedList);
        ReadCell(Cell, n, AResolvedList);
      end;
    end;
  end;
end;

procedure WriteCell(ACell: TsmxBaseCell; const ANode: IXMLNode; AFindList: TList);

  function GetImplClassName(Cell: TsmxBaseCell): String;
  begin
    Result := '';
    if Assigned(Cell) then
      if Assigned(Cell.Implementor) then
        if Assigned(Cell.Implementor.GetReference) then
          Result := Cell.Implementor.GetReference.ClassName;
  end;

var
  i: Integer;
  n: IXMLNode;
  Cell: TsmxBaseCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  for i := 0 to ACell.CellCount - 1 do
  begin
    Cell := ACell.Cells[i];
    if Cell.IsWriteCell then
    begin
      n := ANode.AddChild(smxConsts.cCellNodeName);
      n.Attributes[smxConsts.cIClassNameAttributeName] := GetImplClassName(Cell);
      n.Attributes[smxConsts.cClassNameAttributeName] := Cell.ClassName;
      n.Attributes[smxConsts.cCfgIDAttributeName] := Cell.CfgID;
      WriteProps(Cell, n, AFindList);
      WriteCell(Cell, n, AFindList);
    end;
  end;
end;

procedure ReadSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);

  function GetImplIntf(const ClassName: String): IsmxRefInterface;
  begin
    Result := nil;
    if ClassName <> '' then
      if SysUtils.Supports(Classes.FindClass(ClassName), IsmxRefInterface) then
        Classes.FindClass(ClassName).Create.GetInterface(IsmxRefInterface, Result);
  end;

var
  i: Integer;
  n: IXMLNode;
  Cell: TsmxOwnerCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  for i := 0 to ANode.ChildNodes.Count - 1 do
  begin
    n := ANode.ChildNodes[i];
    if n.NodeName = smxConsts.cCellNodeName then
    begin
      if n.Attributes[smxConsts.cClassNameAttributeName] <> ACell.SlaveClass.ClassName then
        Cell := ACell.AddSlaveAsClass(
          TsmxOwnerCellClass(Classes.FindClass(n.Attributes[smxConsts.cClassNameAttributeName])),
          GetImplIntf(n.Attributes[smxConsts.cIClassNameAttributeName]))
      else
        Cell := ACell.AddSlave(GetImplIntf(n.Attributes[smxConsts.cIClassNameAttributeName]));
      Cell.CfgID := n.Attributes[smxConsts.cCfgIDAttributeName];
      ReadProps(Cell, n, AResolvedList);
      ReadSlave(Cell, n, AResolvedList);
    end;
  end;
end;

procedure WriteSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; AFindList: TList);

  function GetImplClassName(Cell: TsmxBaseCell): String;
  begin
    Result := '';
    if Assigned(Cell) then
      if Assigned(Cell.Implementor) then
        if Assigned(Cell.Implementor.GetReference) then
          Result := Cell.Implementor.GetReference.ClassName;
  end;

var
  i: Integer;
  n: IXMLNode;
  Cell: TsmxOwnerCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  for i := 0 to ACell.SlaveCount - 1 do
  begin
    Cell := ACell.Slaves[i];
    n := ANode.AddChild(smxConsts.cCellNodeName);
    n.Attributes[smxConsts.cIClassNameAttributeName] := GetImplClassName(Cell);
    n.Attributes[smxConsts.cClassNameAttributeName] := Cell.ClassName;
    n.Attributes[smxConsts.cCfgIDAttributeName] := Cell.CfgID;
    WriteProps(Cell, n, AFindList);
    WriteSlave(Cell, n, AFindList);
  end;
end;

end.
