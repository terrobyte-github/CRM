unit smxClassProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxBaseClasses, smxClasses, smxCfgs, smxDBIntf,
  smxManagerIntf, smxClassTypes;

procedure AllCells(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIsOnlyActive: Boolean = False; AIncludeChildForm: Boolean = False);
procedure AllParents(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIncludeParentForm: Boolean = False);
//procedure ClearProps(AObject: TObject);
procedure ReadProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
procedure WriteProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; ARefList: TList);
procedure ResolvedProps(AResolvedList: TsmxResolvedKit; ARefList: TList);
//procedure ReadCell(ACell: TsmxBaseCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
//procedure WriteCell(ACell: TsmxBaseCell; const ANode: IXMLNode; ARefList: TList);
//procedure ReadSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
//procedure WriteSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; ARefList: TList);
procedure RefList(ACell: TsmxBaseCell; AList: TList);

var
  //gSelectRequest: TsmxCustomRequest = nil;
  gMainDatabaseIntf: IsmxDatabase = nil;
  gSelectDataSetIntf: IsmxDataSet = nil;
  gDeleteDataSetIntf: IsmxDataSet = nil;
  gInsertDataSetIntf: IsmxDataSet = nil;
  gUpdateDataSetIntf: IsmxDataSet = nil;
  
implementation

uses
  Variants, SysUtils, TypInfo, smxProcs, smxFuncs, smxConsts, smxBaseIntf,
  smxTypes;

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
    if not AIncludeChildForm and (ACurCell is TsmxCustomForm)
        and not (foFrameForm in TsmxCustomForm(ACurCell).FormOptions) then
      Exit;
    if (AIsEmpty or IsClass(ACurCell))
        and (not AIsOnlyActive or IsActive(ACurCell))
        and (AList.IndexOf(ACurCell) = -1) then
      AList.Add(ACurCell);
    for i := 0 to ACurCell.CellCount - 1 do
      AddChilds(ACurCell.Cells[i], AIsEmpty);
  end;

var
  i: Integer;
begin
  if not Assigned(ACell) or not Assigned(AList) then
    Exit;
  //AList.Clear;
  for i := 0 to ACell.CellCount - 1 do
    AddChilds(ACell.Cells[i], Length(AClassList) = 0);
end;

procedure AllParents(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIncludeParentForm: Boolean = False);

  function IsClass(ACurCell: TsmxBaseCell): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(AClassList) to High(AClassList) do
      if ACurCell is AClassList[i] then
        Result := True;
  end;

  function IsInclude(ACurCell: TsmxBaseCell; var AIsFirstForm: Boolean): Boolean;
  begin
    Result := True;
    if ACurCell is TsmxCustomForm then
      if not AIsFirstForm then
        AIsFirstForm := True
      else
        Result := False;
  end;

var
  Cell: TsmxBaseCell;
  Empty: Boolean;
  IsFirstForm: Boolean;
begin
  if not Assigned(ACell) or not Assigned(AList) then
    Exit;
  //AList.Clear;
  Empty := Length(AClassList) = 0;
  IsFirstForm := ACell is TsmxCustomForm;
  {if Assigned(ACell.CellParent) then
    Cell := ACell.CellParent else
  if ACell is TsmxOwnerCell then
    Cell := TsmxOwnerCell(ACell).CellOwner else
    Cell := nil;}
  Cell := ACell.CellParent;
  while Assigned(Cell) do
  begin
    if (Empty or IsClass(Cell))
        and (AIncludeParentForm or IsInclude(Cell, IsFirstForm))
        and (AList.IndexOf(Cell) = -1) then
      AList.Add(Cell);
    {if Assigned(Cell.CellParent) then
      Cell := Cell.CellParent else
    if Cell is TsmxOwnerCell then
      Cell := TsmxOwnerCell(Cell).CellOwner else
      Cell := nil;}
    Cell := Cell.CellParent;
  end;
end;

(*procedure ClearProps(AObject: TObject);

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

  {procedure ClearChild(Cell: TsmxBaseCell);
  begin
  end;}

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
end;*)

procedure ReadProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);

  procedure ReadClass(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
    Cls: TClass;
    s: String;
  begin
    //n := Node.ChildNodes.FindNode(PropInfo^.Name);
    //if Assigned(n) then
    //begin
      Cls := TypInfo.GetTypeData(APropInfo^.PropType^)^.ClassType;
      Obj := TypInfo.GetObjectProp(AObject, APropInfo);
      if Cls.InheritsFrom(TsmxKit) then
      begin
        n := Node.ChildNodes.FindNode(APropInfo^.Name);
        if Assigned(n) then
        begin
          ReadProps(AOwner, Obj, n, AResolvedList);
          for i := 0 to n.ChildNodes.Count - 1 do
            if n.ChildNodes[i].NodeName = smxConsts.cItemNodeName then
              ReadProps(AOwner, TsmxKit(Obj).Add, n.ChildNodes[i], AResolvedList);
        end;
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
        if Assigned(APropInfo^.SetProc) then
        begin
          s := Variants.VarToStr(Node.Attributes[APropInfo^.Name]);
          if {Node.Attributes[PropInfo^.Name]} s <> '' then
            if Assigned(AResolvedList) then
            //if (n.Attributes[smxConsts.cCfgIDAttributeName] <> 0)
            //    or (n.Attributes[smxConsts.cNameAttributeName] <> '') then
              with AResolvedList.Add do
              begin
                Instance := AObject;
                PropInfo := APropInfo;
                {if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
                begin
                  //TVarData(PropValue).VType := vtInteger;
                  PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
                end else
                begin
                  //TVarData(PropValue).VType := vtString;
                  PropValue := n.Attributes[smxConsts.cNameAttributeName];
                end;}
                PropValue := s; //Variants.VarToStr(Node.Attributes[PropInfo^.Name]);
              end;
        end else
          ReadProps(AOwner, Obj, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
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
          ReadProps(AOwner, Obj, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
      end;
    //end;
  end;

  procedure ReadIntf(APropInfo: PPropInfo; const Node: IXMLNode);

    {function IsImplIntf(const AIntf: IsmxRefPersistent): Boolean;
    var
      Intf: IsmxRefPersistent;
    begin
      Result := AObject.GetInterface(IsmxRefPersistent, Intf)
        and (IsmxRefPersistent(Intf).GetReference = AIntf.GetReference);
    end;}

  var
    Intf: IInterface;
    //n: IXMLNode;
    GUID: TGUID;
    s: String;
    //Intf2: IsmxRefComponent;
  begin
    GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
    Intf := TypInfo.GetInterfaceProp(AObject, APropInfo);
    if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent) then
    begin
      if Assigned(APropInfo^.SetProc) then
      begin
        s := Variants.VarToStr(Node.Attributes[APropInfo^.Name]);
        if s <> '' then
          if Assigned(AResolvedList) then
            with AResolvedList.Add do
            begin
              Instance := AObject;
              PropInfo := APropInfo;
              PropValue := s;
            end;
      end else
      begin
        if SysUtils.Supports(Intf, IsmxRefComponent) then
          ReadProps(AOwner, IsmxRefComponent(Intf).GetReference, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
      end;
    end else
    if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefPersistent) then
    begin
      if SysUtils.Supports(Intf, IsmxRefPersistent) then
        ReadProps(AOwner, IsmxRefPersistent(Intf).GetReference, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
    end;

    //n := Node.ChildNodes.FindNode(PropInfo^.Name);
    (*if {Assigned(n) and (}AObject is TsmxBaseCell{)} then
    begin
      GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
      Intf := TypInfo.GetInterfaceProp(AObject, APropInfo);
      if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent{IsmxRefPersistent}) then
      begin
        //AObject.GetInterface(IsmxRefComponent{IsmxRefPersistent}, Intf2);
        //if Assigned(Intf) then
          //if IsmxRefComponent(Intf).GetReference = Intf2.GetReference then
            //Intf2 := nil;
        //if smxFuncs.IsImplIntf(AObject, IsmxRefComponent{IsmxRefPersistent}(Intf)) then
        if TsmxBaseCell(AObject).IsImplementedIntf(IsmxRefComponent(Intf)) then
        begin
          ReadProps(AOwner, IsmxRefComponent{IsmxRefPersistent}(Intf).GetReference, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
        end else
        begin
          s := Variants.VarToStr(Node.Attributes[APropInfo^.Name]);
          if {Node.Attributes[PropInfo^.Name]} s <> '' then
            if Assigned(AResolvedList) then
          //begin
            //if (n.Attributes[smxConsts.cCfgIDAttributeName] <> 0)
            //    or (n.Attributes[smxConsts.cNameAttributeName] <> '') then
              with AResolvedList.Add do
              begin
                Instance := AObject;
                PropInfo := APropInfo;
                {if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
                begin
                  //TVarData(PropValue).VType := vtInteger;
                  PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
                end else
                begin
                  //TVarData(PropValue).VType := vtString;
                  PropValue := n.Attributes[smxConsts.cNameAttributeName];
                end;}
                PropValue := s; //Variants.VarToStr(Node.Attributes[PropInfo^.Name]);
              end;
          //end;
        end;
      end;
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
    end;*)
  end;

  procedure ReadMethod(APropInfo: PPropInfo; const Node: IXMLNode);

    {procedure GetObjAndMethName(const Text: String; var ObjName, MethName: String);
    var
      i: Integer;
    begin
      ObjName := '';
      MethName := '';
      i := Pos(smxConsts.cDelimiterObjAndMethName, Text);
      if i <> 0 then
      begin
        ObjName := Copy(Text, 1, i - 1);
        MethName := Copy(Text, i + 1, Length(Text) - i);
      end;
    end;}

  var
    //n: IXMLNode;
    //ObjName, MethName: String;
    s: String;
  begin
    //n := Node.ChildNodes.FindNode(PropInfo^.Name);
    //GetObjAndMethName(Variants.VarToStr(Node.Attributes[APropInfo^.Name]), ObjName, MethName);
    s := Variants.VarToStr(Node.Attributes[APropInfo^.Name]);
    //if Assigned(n) and (AObject is TsmxBaseCell) then
    //if (ObjName <> '') and (MethName <> '') then
    if s <> '' then
    begin
      if Assigned(AResolvedList) then
        //if ((n.Attributes[smxConsts.cCfgIDAttributeName] <> 0)
        //      or (n.Attributes[smxConsts.cNameAttributeName] <> ''))
        //    and (n.Attributes[smxConsts.cProcNameAttributeName] <> '') then
          with AResolvedList.Add do
          begin
            Instance := AObject;
            PropInfo := APropInfo;
            {if n.Attributes[smxConsts.cCfgIDAttributeName] <> 0 then
            begin
              //TVarData(PropValue).VType := vtInteger;
              PropValue := n.Attributes[smxConsts.cCfgIDAttributeName];
            end else
            begin
              //TVarData(PropValue).VType := vtString;
              PropValue := n.Attributes[smxConsts.cNameAttributeName];
            end;}
            //PropValue := ObjName;
            PropValue := s;
            //ProcName := MethName; //n.Attributes[smxConsts.cProcNameAttributeName];
          end;
    end;
  end;

  (*procedure ReadChild(ACell: TsmxBaseCell; const Node: IXMLNode);

    {function GetImplIntf(const ClassName: String): IsmxRefPersistent;
    begin
      Result := nil;
      if ClassName <> '' then
        if SysUtils.Supports(Classes.FindClass(ClassName), IsmxRefPersistent) then
          Classes.FindClass(ClassName).Create.GetInterface(IsmxRefPersistent, Result);
    end;}

  var
    n: IXMLNode;
    i: Integer;
    Cell: TsmxBaseCell;
    ClsName{, IClsName}: String;
    CfgID: Integer;
  begin
    for i := 0 to Node.ChildNodes.Count - 1 do
    begin
      n := Node.ChildNodes[i];
      if n.NodeName = smxConsts.cCellNodeName then
      begin
        ClsName := n.Attributes[smxConsts.cClassNameAttributeName];
        //IClsName := n.Attributes[smxConsts.cIClassNameAttributeName];
        CfgID := n.Attributes[smxConsts.cCfgIDAttributeName];
        if ClsName <> '' then
          Cell := TsmxBaseCellClass(Classes.FindClass(ClsName)).Create(AOwner{,
            TsmxInterfacedPersistentClass(Classes.GetClass(IClsName))})
        else
          Cell := nil;
        if Assigned(Cell) then
        begin
          Cell.CfgID := CfgID;
          Cell.CellParent := ACell;
          ReadProps(AOwner, Cell, n, AResolvedList);
        end;
      end else
      if (n.NodeName = smxConsts.cSlaveNodeName) and (ACell is TsmxOwnerCell) then
      begin
        ClsName := n.Attributes[smxConsts.cClassNameAttributeName];
        //IClsName := n.Attributes[smxConsts.cIClassNameAttributeName];
        CfgID := n.Attributes[smxConsts.cCfgIDAttributeName];
        Cell := TsmxOwnerCell(ACell).SlaveList.Add(
          TsmxBaseCellClass(Classes.FindClass(ClsName)){,
          TsmxInterfacedPersistentClass(Classes.GetClass(IClsName))}).ItemObject;
        Cell.CfgID := CfgID;
        ReadProps(AOwner, Cell, n, AResolvedList);
      end;
    end;
  end;*)

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
              if ANode.HasAttribute(PropInfo^.Name) then
                TypInfo.SetEnumProp(AObject, PropInfo, ANode.Attributes[PropInfo^.Name]);
            tkSet:
              if ANode.HasAttribute(PropInfo^.Name) then
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
  {if (AObject is TsmxBaseCell) then
  begin
    if not Assigned(ACfg) then
      ReadChild(TsmxBaseCell(AObject), ANode)
    else
      TsmxBaseCell(AObject).SetProperties(ACfg);
  end;}
end;

procedure WriteProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; ARefList: TList);

  procedure WriteClass(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
  begin
    Obj := TypInfo.GetObjectProp(AObject, APropInfo);
    //if Assigned(Obj) then
    //begin
      //n := Node.AddChild(PropInfo^.Name, 0);
      {if Obj is TsmxSlaveList then
      begin
        WriteProps(Obj, n, ARefList);
        for i := 0 to TsmxSlaveList(Obj).Count - 1 do
        begin
          n2 := n.AddChild(smxConsts.cItemNodeName)
          if Assigned(TsmxSlaveList(Obj)[i].Slave) then
          begin
            n2.Attributes[smxConsts.cIClassNameAttributeName] := '';
            n2.Attributes[smxConsts.cClassNameAttributeName] := TsmxSlaveList(Obj)[i].Slave.ClassName;
            //n2.Attributes[smxConsts.cCfgIDAttributeName] := TsmxSlaveList(Obj)[i].Slave.CfgID;
          end;
          WriteProps(TsmxSlaveList(Obj)[i], n2, ARefList);
        end;    // лучше вынести в отдельную процедуру, т.к. много особенностей
      end else}
      if Obj is TsmxKit then
      begin
        n := Node.AddChild(APropInfo^.Name);
        WriteProps(AOwner, Obj, n, ARefList);
        for i := 0 to TsmxKit(Obj).Count - 1 do
          WriteProps(AOwner, TsmxKit(Obj)[i], n.AddChild(smxConsts.cItemNodeName), ARefList);
      end {else
      if Obj is TCollection then
      begin
        WriteProps(Obj, n);
        for i := 0 to TCollection(Obj).Count - 1 do
          WriteProps(TCollection(Obj).Items[i], n.AddChild(smxConsts.cItemNodeName));
      end} else
      if Obj is TsmxComponent then
      begin
        if Assigned(APropInfo^.SetProc) then
        begin

          //if TsmxBaseCell(Obj).CfgID <> 0 then
            //Node.Attributes[APropInfo^.Name{smxConsts.cCfgIDAttributeName}] := TsmxBaseCell(Obj).CfgID
          //else
          if Assigned(ARefList) and (ARefList.IndexOf(Obj) <> -1) then
            Node.Attributes[APropInfo^.Name] := TsmxComponent(Obj).Name
          else
            Node.Attributes[APropInfo^.Name] := '';

        end else
          WriteProps(AOwner, Obj, Node.AddChild(APropInfo^.Name), ARefList);
      end {else
      if Obj is TComponent then
      begin
        n.Attributes[smxConsts.cNameAttributeName] := TComponent(Obj).Name;
      end} else
      if Assigned(Obj) then
      begin
        WriteProps(AOwner, Obj, Node.AddChild(APropInfo^.Name), ARefList);
      end else
        Node.Attributes[APropInfo^.Name] := '';
    //end;
  end;

  procedure WriteIntf(APropInfo: PPropInfo; const Node: IXMLNode);

    {function IsImplIntf(const AIntf: IsmxRefPersistent): Boolean;
    var
      Intf: IsmxRefPersistent;
    begin
      Result := AObject.GetInterface(IsmxRefPersistent, Intf)
        and (IsmxRefPersistent(Intf).GetReference = AIntf.GetReference);
    end;}

    {function FindImplObj(const AIntf: IsmxRefComponent): TObject;
    var
      i: Integer;
      //Intf: IInterface;
    begin
      Result := nil;
      if Assigned(AIntf) and Assigned(ARefList) then
      begin
        for i := 0 to ARefList.Count - 1 do
          //if smxFuncs.IsImplIntf(TObject(ARefList[i]), AIntf) then
          if (TObject(ARefList[i]) is TsmxBaseCell)
              and TsmxBaseCell(TObject(ARefList[i])).IsImplementedIntf(AIntf) then
          begin
            Result := TObject(ARefList[i]);
            Break;
          end;
      end;
    end;}

    {function IndexOfIntf(const AIntf: IsmxRefComponent): Integer;
    var
      i: Integer;
      GUID: TGUID;
    begin
      Result := -1;
      GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
      if Assigned(ARefList) then
        for i := 0 to ARefList.Count - 1 do
          if (TObject(ARefList[i]) is TsmxBaseCell)
              and () then
          begin

          end;
    end;}

  var
    Intf: IInterface;
    //Obj: TObject;
    //n: IXMLNode;
    //Form: TsmxCustomForm;
  begin
    Intf := TypInfo.GetInterfaceProp(AObject, APropInfo);
    if SysUtils.Supports(Intf, IsmxRefComponent) then
    begin
      if Assigned(APropInfo^.SetProc) then
      begin
        if Assigned(ARefList) and (ARefList.IndexOf(IsmxRefComponent(Intf).GetReference) <> -1) then
          Node.Attributes[APropInfo^.Name] := IsmxRefComponent(Intf).GetReference.Name
        else
          Node.Attributes[APropInfo^.Name] := '';
      end else
        WriteProps(AOwner, IsmxRefComponent(Intf).GetReference, Node.AddChild(APropInfo^.Name), ARefList);
    end else
    if SysUtils.Supports(Intf, IsmxRefPersistent) then
    begin
      WriteProps(AOwner, IsmxRefPersistent(Intf).GetReference, Node.AddChild(APropInfo^.Name), ARefList);
    end else
      Node.Attributes[APropInfo^.Name] := '';

    (*if AObject is TsmxBaseCell then
    //begin
      //if {Assigned(Intf) and} SysUtils.Supports(Intf, IsmxRefComponent{IsmxRefPersistent}) {then
      //begin
      //  if} and smxFuncs.IsImplIntf(AObject, IsmxRefComponent{IsmxRefPersistent}(Intf)) then
        if TsmxBaseCell(AObject).IsImplementedIntf(IsmxRefComponent(Intf)) then
        begin
          //n := Node.AddChild(PropInfo^.Name, 0);
          WriteProps(AOwner, IsmxRefComponent{IsmxRefPersistent}(Intf).GetReference, Node.AddChild(APropInfo^.Name), ARefList);
        end else
        //begin
          //Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(AObject));
          //if Assigned(AChildList) then
        begin
          Obj := FindImplObj(IsmxRefComponent{IsmxRefPersistent}(Intf));
          if Obj is TsmxBaseCell then
          begin
            if TsmxBaseCell(Obj).CfgID <> 0 then
              Node.Attributes[APropInfo^.Name{smxConsts.cCfgIDAttributeName}] := TsmxBaseCell(Obj).CfgID
            else
              Node.Attributes[APropInfo^.Name{smxConsts.cNameAttributeName}] := TsmxBaseCell(Obj).Name;
          end else
            Node.Attributes[APropInfo^.Name] := '';
        end;
      {end else
        Node.Attributes[PropInfo^.Name] := ''};
      {if SysUtils.Supports(Intf, IsmxRefInterface) then
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
      end;}
    end;*)
  end;

  procedure WriteMethod(APropInfo: PPropInfo; const Node: IXMLNode);

    {function FindMethodObj(Method: TMethod): TObject;
    var
      i: Integer;
    begin
      Result := nil;
      if Assigned(Method.Code) and Assigned(ARefList) then
      begin
        for i := 0 to ARefList.Count - 1 do
          if (ARefList[i] = Method.Data)
              and (TObject(ARefList[i]).MethodName(Method.Code) <> '') then
          begin
            Result := TObject(ARefList[i]);
            Break;
          end;
      end;
    end;}

  var
    Method: TMethod;
    //Obj: TObject;
    //n: IXMLNode;
    //Form: TsmxCustomForm;
  begin
    Method := TypInfo.GetMethodProp(AObject, APropInfo);
    if TObject(Method.Data) is TsmxComponent then
    begin
      if Assigned(ARefList) and (ARefList.IndexOf(Method.Data) <> -1) then
        Node.Attributes[APropInfo^.Name] :=
          Format('%s%s%s', [TsmxComponent(Method.Data).Name, smxConsts.cDelimiterObjAndMethName, TsmxComponent(Method.Data).MethodName(Method.Code)])
      else
        Node.Attributes[APropInfo^.Name] := '';
    end else
      Node.Attributes[APropInfo^.Name] := '';

    (*if AObject is TsmxBaseCell then
    begin
      {if Assigned(Method.Code) then
      begin}
        //n := ANode.AddChild(PropInfo^.Name, 0);
        //Form := smxClassFuncs.GetAccessoryForm(TsmxBaseCell(AObject));
        //if Assigned(Form) then
        //begin
          //Obj := smxClassFuncs.GetAlgorithmForm(Form, TsmxComponentEvent(Method));
          Obj := FindMethodObj(Method);
          if Obj is TsmxBaseCell then
          begin
            if TsmxBaseCell(Obj).CfgID <> 0 then
              Node.Attributes[APropInfo^.Name{smxConsts.cCfgIDAttributeName}] :=
                Format('%d%s%s', [TsmxBaseCell(Obj).CfgID, smxConsts.cDelimiterObjAndMethName, Obj.MethodName(Method.Code)])
            else
              Node.Attributes[APropInfo^.Name{smxConsts.cNameAttributeName}] :=
                Format('%s%s%s', [TsmxBaseCell(Obj).Name, smxConsts.cDelimiterObjAndMethName, Obj.MethodName(Method.Code)]);
            //n.Attributes[smxConsts.cProcNameAttributeName] := Obj.MethodName(Method.Code);
          end else
            Node.Attributes[APropInfo^.Name] := '';
        //end;
      {end else
        Node.Attributes[PropInfo^.Name] := '';}
    end;*)
  end;

  (*procedure WriteChild(ACell: TsmxBaseCell; const Node: IXMLNode);

    {function GetImplClassName(Cell: TsmxBaseCell): String;
    begin
      Result := '';
      if Assigned(Cell) then
        if Assigned(Cell.Implementor) then
          if Assigned(Cell.Implementor.GetReference) then
            Result := Cell.Implementor.GetReference.ClassName;
    end;}

  var
    i: Integer;
    Cell: TsmxBaseCell;
    n: IXMLNode;
  begin
    for i := 0 to ACell.CellCount - 1 do
    begin
      Cell := ACell.Cells[i];
      if not ((Cell is TsmxOwnerCell) and Assigned(TsmxOwnerCell(Cell).CellOwner)) then
      begin
        n := Node.AddChild(smxConsts.cCellNodeName);
        WriteProps(AOwner, Cell, n, ARefList);
        //n.Attributes[smxConsts.cIClassNameAttributeName] := GetImplClassName(Cell);
        n.Attributes[smxConsts.cClassNameAttributeName] := Cell.ClassName;
        n.Attributes[smxConsts.cCfgIDAttributeName] := Cell.CfgID;
      end;
    end;
    if ACell is TsmxOwnerCell then
      for i := 0 to TsmxOwnerCell(ACell).SlaveCount - 1 do
      begin
        Cell := TsmxOwnerCell(ACell).Slaves[i];
        n := Node.AddChild(smxConsts.cSlaveNodeName);
        WriteProps(AOwner, Cell, n, ARefList);
        //n.Attributes[smxConsts.cIClassNameAttributeName] := GetImplClassName(Cell);
        n.Attributes[smxConsts.cClassNameAttributeName] := Cell.ClassName;
        n.Attributes[smxConsts.cCfgIDAttributeName] := Cell.CfgID;
      end;
  end;*)

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
  {if AObject is TsmxBaseCell then
  begin
    if not Assigned(ACfg) then
      WriteChild(TsmxBaseCell(AObject), ANode)
    else
      TsmxBaseCell(AObject).GetProperties(ACfg);
  end;}
end;

procedure ResolvedProps(AResolvedList: TsmxResolvedKit; ARefList: TList);

  procedure ResolvedClass(Item: TsmxResolvedItem);
  var
   Cls: TClass;
   i: Integer;
  begin
     if Assigned(ARefList) then
     begin
       Cls := TypInfo.GetTypeData(Item.PropInfo^.PropType^)^.ClassType;
       for i := 0 to ARefList.Count - 1 do
         if TObject(ARefList[i]).InheritsFrom(Cls)
             and (TObject(ARefList[i]) is TsmxComponent) then
           if {(TsmxBaseCell(ARefList[i]).CfgID = Item.PropValue)
               or (}TsmxComponent(ARefList[i]).Name = Item.PropValue{)} then
           begin
             TypInfo.SetObjectProp(Item.Instance, Item.PropInfo, TObject(ARefList[i]));
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
    if Assigned(ARefList) then
    begin
      GUID := TypInfo.GetTypeData(Item.PropInfo^.PropType^)^.Guid;
      for i := 0 to ARefList.Count - 1 do
        if SysUtils.Supports(TObject(ARefList[i]), GUID, Intf)
            and (TObject(ARefList[i]) is TsmxComponent) then
          if {(TsmxBaseCell(ARefList[i]).CfgID = Item.PropValue)
              or (}TsmxComponent(ARefList[i]).Name = Item.PropValue{)} then
          begin
            TypInfo.SetInterfaceProp(Item.Instance, Item.PropInfo, Intf);
            Break;
          end;
    end;
  end;

  procedure ResolvedMethod(Item: TsmxResolvedItem);

    {procedure GetObjAndMethName(const Text: String; var ObjName, MethName: String);
    var
      i: Integer;
    begin
      ObjName := '';
      MethName := '';
      i := Pos(smxConsts.cDelimiterObjAndMethName, Text);
      if i <> 0 then
      begin
        ObjName := Copy(Text, 1, i - 1);
        MethName := Copy(Text, i + 1, Length(Text) - i);
      end;
    end;}

  var
    Method: TMethod;
    i: Integer;
    ObjName, MethName: String;
  begin
    if Assigned(ARefList) then
    begin
      smxProcs.SplitByDelimiter(Variants.VarToStr(Item.PropValue),
        smxConsts.cDelimiterObjAndMethName, ObjName, MethName);
      //GetObjAndMethName(Variants.VarToStr(Item.PropValue), ObjName, MethName);
      if (ObjName <> '') and (MethName <> '') then
        for i := 0 to ARefList.Count - 1 do
          if TObject(ARefList[i]) is TsmxComponent then
            if ({(TsmxBaseCell(ARefList[i]).CfgID = SysUtils.StrToIntDef(ObjName, -1))
                  or (}TsmxComponent(ARefList[i]).Name = ObjName{)})
                and Assigned(TsmxComponent(ARefList[i]).MethodAddress(MethName)) then
            begin
              Method.Data := ARefList[i];
              Method.Code := TsmxComponent(ARefList[i]).MethodAddress(MethName);
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

(*procedure ReadCell(ACell: TsmxBaseCell; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);

  function GetImplIntf(const ClassName: String): IsmxRefInterface;
  begin
    Result := nil;
    if ClassName <> '' then
      if SysUtils.Supports(Classes.FindClass(ClassName), IsmxRefInterface) then
        Classes.FindClass(ClassName).Create.GetInterface(IsmxRefInterface, Result);
  end;

var
  //i: Integer;
  //n: IXMLNode;
  Cell: TsmxBaseCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  //for i := 0 to ANode.ChildNodes.Count - 1 do
  //begin
    //n := ANode.ChildNodes[i];
    //if n.NodeName = smxConsts.cCellNodeName then
    //begin
      if ANode.Attributes[smxConsts.cClassNameAttributeName] <> '' then
      begin
        if ANode.Attributes[smxConsts.cIClassNameAttributeName] <> '' then
          Cell := TsmxBaseCellClass(Classes.FindClass(ANode.Attributes[smxConsts.cClassNameAttributeName])).CreateByImpl(
            nil, GetImplIntf(ANode.Attributes[smxConsts.cIClassNameAttributeName]))
        else
          Cell := TsmxBaseCellClass(Classes.FindClass(ANode.Attributes[smxConsts.cClassNameAttributeName])).Create(nil);
      end else
        Cell := nil;
      if Assigned(Cell) then
      begin
        Cell.CfgID := ANode.Attributes[smxConsts.cCfgIDAttributeName];
        Cell.CellParent := ACell;
        // м.б. глобалные объекты не присваивать... где-то уже это было...
        {Cell.StorageManager := gStorageManagerIntf;
        Cell.LibraryManager := gLibraryManagerIntf;
        Cell.DatabaseManager := gDatabaseManagerIntf;
        Cell.FormManager := gFormManagerIntf;
        Cell.ImageListManager := gImageListManagerIntf;}
        ReadProps(Cell, ANode, AResolvedList);
        //ReadCell(Cell, n, AResolvedList);
        //if Cell is TsmxOwnerCell then
          //ReadSlave(TsmxOwnerCell(Cell), n, AResolvedList);
      end;
    //end;
  //end;
end;

procedure WriteCell(ACell: TsmxBaseCell; const ANode: IXMLNode; ARefList: TList);

  function GetImplClassName(Cell: TsmxBaseCell): String;
  begin
    Result := '';
    if Assigned(Cell) then
      if Assigned(Cell.Implementor) then
        if Assigned(Cell.Implementor.GetReference) then
          Result := Cell.Implementor.GetReference.ClassName;
  end;

//var
  //i: Integer;
  //n: IXMLNode;
  //Cell: TsmxBaseCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  //for i := 0 to ACell.CellCount - 1 do
  //begin
    //Cell := ACell.Cells[i];
    //if not ((Cell is TsmxOwnerCell) and Assigned(TsmxOwnerCell(Cell).CellOwner)) then
    //begin
      //n := ANode.AddChild(smxConsts.cCellNodeName);
      WriteProps(ACell, ANode, ARefList);
      ANode.Attributes[smxConsts.cIClassNameAttributeName] := GetImplClassName(ACell);
      ANode.Attributes[smxConsts.cClassNameAttributeName] := ACell.ClassName;
      ANode.Attributes[smxConsts.cCfgIDAttributeName] := ACell.CfgID;
      //WriteCell(Cell, n, ARefList);
      //if Cell is TsmxOwnerCell then
        //WriteSlave(TsmxOwnerCell(Cell), n, ARefList);
    //end;
  //end;
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
  //i: Integer;
  //n: IXMLNode;
  Cell: TsmxOwnerCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  //for i := 0 to ANode.ChildNodes.Count - 1 do
  //begin
    //n := ANode.ChildNodes[i];
    //if n.NodeName = smxConsts.cCellNodeName then
    //begin
      //if n.Attributes[smxConsts.cClassNameAttributeName] <> ACell.SlaveClass.ClassName then
        Cell := ACell.SlaveListNew.Add(
          TsmxOwnerCellClass(Classes.FindClass(ANode.Attributes[smxConsts.cClassNameAttributeName])),
          GetImplIntf(ANode.Attributes[smxConsts.cIClassNameAttributeName])).Slave;
        //Cell := ACell.AddSlaveAsClass(
          //TsmxOwnerCellClass(Classes.FindClass(n.Attributes[smxConsts.cClassNameAttributeName])),
          //GetImplIntf(n.Attributes[smxConsts.cIClassNameAttributeName]))
      //else
        //Cell := ACell.SlaveListNew.Add(nil, GetImplIntf(n.Attributes[smxConsts.cIClassNameAttributeName])).Slave;
        //Cell := ACell.AddSlave(GetImplIntf(n.Attributes[smxConsts.cIClassNameAttributeName]));

      Cell.CfgID := ANode.Attributes[smxConsts.cCfgIDAttributeName];
      ReadProps(Cell, ANode, AResolvedList);
      //ReadSlave(Cell, n, AResolvedList);
    //end;
  //end;
end;

procedure WriteSlave(ACell: TsmxOwnerCell; const ANode: IXMLNode; ARefList: TList);

  function GetImplClassName(Cell: TsmxBaseCell): String;
  begin
    Result := '';
    if Assigned(Cell) then
      if Assigned(Cell.Implementor) then
        if Assigned(Cell.Implementor.GetReference) then
          Result := Cell.Implementor.GetReference.ClassName;
  end;

//var
  //i: Integer;
  //n: IXMLNode;
  //Cell: TsmxOwnerCell;
begin
  if not Assigned(ACell) or not Assigned(ANode) then
    Exit;
  //for i := 0 to ACell.SlaveCount - 1 do
  //begin
    //Cell := ACell.Slaves[i];
    //n := ANode.AddChild(smxConsts.cSlaveNodeName);
    WriteProps(ACell, ANode, ARefList);
    ANode.Attributes[smxConsts.cIClassNameAttributeName] := GetImplClassName(ACell);
    ANode.Attributes[smxConsts.cClassNameAttributeName] := ACell.ClassName;
    ANode.Attributes[smxConsts.cCfgIDAttributeName] := ACell.CfgID;
    //WriteSlave(Cell, n, ARefList);
  //end;
end;*)

procedure RefList(ACell: TsmxBaseCell; AList: TList);

  procedure RefClass(AObject: TObject; APropInfo: PPropInfo);
  var
    Obj: TObject;
    Cls: TClass;
  begin
    Cls := TypInfo.GetTypeData(APropInfo^.PropType^)^.ClassType;
    Obj := TypInfo.GetObjectProp(AObject, APropInfo);
    if Cls.InheritsFrom(TsmxComponent) then
      if not Assigned(APropInfo^.SetProc) and Assigned(Obj) then
        if AList.IndexOf(Obj) = -1 then
          AList.Add(Obj);
  end;

  procedure RefIntf(AObject: TObject; APropInfo: PPropInfo);
  var
    Intf: IInterface;
    GUID: TGUID;
  begin
    GUID := TypInfo.GetTypeData(APropInfo^.PropType^)^.Guid;
    Intf := TypInfo.GetInterfaceProp(AObject, APropInfo);
    if smxFuncs.IntfInheritsFrom(APropInfo^.PropType^, IsmxRefComponent) then
      if not Assigned(APropInfo^.SetProc) and SysUtils.Supports(Intf, IsmxRefComponent) then
        if AList.IndexOf(IsmxRefComponent(Intf).GetReference) = -1 then
          AList.Add(IsmxRefComponent(Intf).GetReference);
  end;

var
  i, j: Integer;
  Count: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  if not Assigned(ACell) or not Assigned(AList) then
    Exit;
  AllCells(ACell, AList, []);
  for j := 0 to AList.Count -1 do
  begin
    Count := TypInfo.GetPropList(PTypeInfo(TObject(AList[j]).ClassInfo),
      [tkClass, tkInterface], nil);
    if Count <> 0 then
    begin
      GetMem(PropList, Count * SizeOf(Pointer));
      try
        TypInfo.GetPropList(PTypeInfo(TObject(AList[j]).ClassInfo),
          [tkClass, tkInterface], PropList);
        for i := 0 to Count - 1 do
        begin
          PropInfo := PropList^[i];
          case PropInfo^.PropType^^.Kind of
            tkClass:
              RefClass(TObject(AList[j]), PropInfo);
            tkInterface:
              RefIntf(TObject(AList[j]), PropInfo);
          end;
        end;
      finally
        FreeMem(PropList);
      end;
    end;
  end;
end;

end.
