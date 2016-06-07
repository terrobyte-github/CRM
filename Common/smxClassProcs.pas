{**************************************}
{                                      }
{            SalesMan v1.0             }
{        Cell class procedures         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxClassProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxBaseClasses, smxClasses, smxCfgs, smxDBIntf,
  smxManagerIntf, smxClassTypes;

procedure AllCells(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIsOnlyActive: Boolean = False; AIncludeChildForm: Boolean = False);
procedure AllParents(ACell: TsmxBaseCell; AList: TList; AClassList: array of TsmxBaseCellClass;
  AIncludeParentForm: Boolean = False);
procedure ReadProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);
procedure WriteProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; ARefList: TList);
procedure ResolvedProps(AResolvedList: TsmxResolvedKit; ARefList: TList);
procedure RefList(ACell: TsmxBaseCell; AList: TList);

var
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
      Result := TsmxControlCell(ACurCell).Active;
  end;

  procedure AddChilds(ACurCell: TsmxBaseCell; AIsEmpty: Boolean);
  var
    i: Integer;
  begin
    if not AIncludeChildForm and (ACurCell is TsmxCustomForm)
        and not (foFrameForm in TsmxCustomForm(ACurCell).Options) then
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
  Empty := Length(AClassList) = 0;
  IsFirstForm := ACell is TsmxCustomForm;
  Cell := ACell.CellParent;
  while Assigned(Cell) do
  begin
    if (Empty or IsClass(Cell))
        and (AIncludeParentForm or IsInclude(Cell, IsFirstForm))
        and (AList.IndexOf(Cell) = -1) then
      AList.Add(Cell);
    Cell := Cell.CellParent;
  end;
end;

procedure ReadProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; AResolvedList: TsmxResolvedKit);

  procedure ReadClass(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
    Cls: TClass;
    s: String;
  begin
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
    end else
    if Cls.InheritsFrom(TsmxBaseCell) then
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
        ReadProps(AOwner, Obj, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
    end else
    begin
      if Assigned(Obj) then
        ReadProps(AOwner, Obj, Node.ChildNodes.FindNode(APropInfo^.Name), AResolvedList);
    end;
  end;

  procedure ReadIntf(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Intf: IInterface;
    GUID: TGUID;
    s: String;
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
  end;

  procedure ReadMethod(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    s: String;
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
end;

procedure WriteProps(AOwner: TComponent; AObject: TObject; const ANode: IXMLNode; ARefList: TList);

  procedure WriteClass(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Obj: TObject;
    n: IXMLNode;
    i: Integer;
  begin
    Obj := TypInfo.GetObjectProp(AObject, APropInfo);
    if Obj is TsmxKit then
    begin
      n := Node.AddChild(APropInfo^.Name);
      WriteProps(AOwner, Obj, n, ARefList);
      for i := 0 to TsmxKit(Obj).Count - 1 do
        WriteProps(AOwner, TsmxKit(Obj)[i], n.AddChild(smxConsts.cItemNodeName), ARefList);
    end else
    if Obj is TsmxComponent then
    begin
      if Assigned(APropInfo^.SetProc) then
      begin
        if Assigned(ARefList) and (ARefList.IndexOf(Obj) <> -1) then
          Node.Attributes[APropInfo^.Name] := TsmxComponent(Obj).Name
        else
          Node.Attributes[APropInfo^.Name] := '';
      end else
        WriteProps(AOwner, Obj, Node.AddChild(APropInfo^.Name), ARefList);
    end else
    if Assigned(Obj) then
    begin
      WriteProps(AOwner, Obj, Node.AddChild(APropInfo^.Name), ARefList);
    end else
      Node.Attributes[APropInfo^.Name] := '';
  end;

  procedure WriteIntf(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Intf: IInterface;
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
  end;

  procedure WriteMethod(APropInfo: PPropInfo; const Node: IXMLNode);
  var
    Method: TMethod;
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
          if TsmxComponent(ARefList[i]).Name = Item.PropValue then
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
          if TsmxComponent(ARefList[i]).Name = Item.PropValue then
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
    ObjName, MethName: String;
  begin
    if Assigned(ARefList) then
    begin
      smxProcs.SplitByDelimiter(Variants.VarToStr(Item.PropValue),
        smxConsts.cDelimiterObjAndMethName, ObjName, MethName);
      if (ObjName <> '') and (MethName <> '') then
        for i := 0 to ARefList.Count - 1 do
          if TObject(ARefList[i]) is TsmxComponent then
            if (TsmxComponent(ARefList[i]).Name = ObjName)
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

{procedure RefList(ACell: TsmxBaseCell; AList: TList);

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
end;}

procedure RefList(ACell: TsmxBaseCell; AList: TList);
var
  i, j: Integer;
  Count: Integer;
  GUIDs: TsmxGUIDArray;
  Intf: IInterface;
  RefIntf: IsmxRefComponent;
begin
  if not Assigned(ACell) or not Assigned(AList) then
    Exit;
  AllCells(ACell, AList, []);
  for j := 0 to AList.Count - 1 do
  begin
    Count := smxFuncs.ImplementedIntf(TObject(AList[j]).ClassType, GUIDs);
    if Count > 0 then
      for i := 0 to Count - 1 do
        if TObject(AList[j]).GetInterface(GUIDs[i], Intf) then
          if SysUtils.Supports(Intf, IsmxRefComponent, RefIntf) then
            if AList.IndexOf(RefIntf.GetReference) = -1 then
              AList.Add(RefIntf.GetReference);
  end;
end;

end.
