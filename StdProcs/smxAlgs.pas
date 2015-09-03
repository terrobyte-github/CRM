{**************************************}
{                                      }
{            SalesMan v1.0             }
{       Standard cell algorithms       }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Аleksandr          }
{                                      }
{**************************************}

unit smxAlgs;

interface

uses
  smxBaseClasses;

procedure ApplyForm(Sender: TsmxComponent);
procedure CancelForm(Sender: TsmxComponent);
procedure PrepareForm(Sender: TsmxComponent);
procedure RefreshForm(Sender: TsmxComponent);
procedure ShowForm(Sender: TsmxComponent);
procedure ShowUniForm(Sender: TsmxComponent);
procedure ShowModalForm(Sender: TsmxComponent);
procedure CloseModalForm(Sender: TsmxComponent);
//procedure GetPropsTree(Sender: TsmxComponent);
//procedure SetPropsTree(Sender: TsmxComponent);
//procedure GetTypeKindNameGrid(Sender: TsmxComponent);
procedure CloseOkForm(Sender: TsmxComponent);
procedure CloseCancelForm(Sender: TsmxComponent);
procedure ShowMessageForm(Sender: TsmxComponent);
procedure ChangeRowPropsGrid(Sender: TsmxComponent);
procedure ClickColumnPropsGrid(Sender: TsmxComponent);

implementation

uses
  Classes, Controls, SysUtils, TypInfo, Variants, smxStdCtrls, smxClasses,
  smxFuncs, smxProcs, smxClassProcs, smxClassFuncs, smxTypes, smxBaseIntf,
  smxManagerIntf;

//var
  //Cfg: TsmxBaseCfg = nil;

procedure ApplyForm(Sender: TsmxComponent);
var
  List: TList;
  i: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomForm then
    begin
      List := TList.Create;
      try
        smxClassProcs.AllCells(TsmxCustomAlgorithm(Sender).CellEvent,
          List, [TsmxWorkCell]);
        for i := 0 to List.Count - 1 do
          TsmxWorkCell(List[i]).Apply;
      finally
        List.Free;
      end;
    end;
end;

procedure CancelForm(Sender: TsmxComponent);
var
  List: TList;
  i: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomForm then
    begin
      List := TList.Create;
      try
        smxClassProcs.AllCells(TsmxCustomAlgorithm(Sender).CellEvent,
          List, [TsmxWorkCell]);
        for i := 0 to List.Count - 1 do
          TsmxWorkCell(List[i]).Cancel;
      finally
        List.Free;
      end;
    end;
end;

procedure PrepareForm(Sender: TsmxComponent);
var
  List: TList;
  i: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomForm then
    begin
      List := TList.Create;
      try
        smxClassProcs.AllCells(TsmxCustomAlgorithm(Sender).CellEvent,
          List, [TsmxWorkCell], TsmxCustomForm(TsmxCustomAlgorithm(Sender).CellEvent).CellVisible);
        for i := 0 to List.Count - 1 do
          TsmxWorkCell(List[i]).Prepare;
      finally
        List.Free;
      end;
    end;
end;

procedure RefreshForm(Sender: TsmxComponent);
var
  List: TList;
  i: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomForm then
    begin
      List := TList.Create;
      try
        smxClassProcs.AllCells(TsmxCustomAlgorithm(Sender).CellEvent,
          List, [TsmxWorkCell], True);
        for i := 0 to List.Count - 1 do
          TsmxWorkCell(List[i]).Refresh;
      finally
        List.Free;
      end;
    end;
end;

procedure ShowForm(Sender: TsmxComponent);
var
  CfgID, ID: Integer;
  AccessoryForm, Form: TsmxCustomForm;
begin
  if Sender is TsmxCustomAlgorithm then
  begin
    CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
    ID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ID', 0);
    if CfgID <> 0 then
    begin
      AccessoryForm := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
      Form := smxClassFuncs.NewForm(AccessoryForm, CfgID, nil, ID, True);
      try
        Form.CellParent := AccessoryForm;
        Form.Initialize;
        Form.IntfID := AccessoryForm.IntfID;
        Form.Show;
      finally
        //Form.Free;
      end;
    end;
  end;
end;

procedure ShowUniForm(Sender: TsmxComponent);
var
  CfgID, ID: Integer;
  AccessoryForm, Form: TsmxCustomForm;
  FormIntf: IsmxFormControl;
begin
  if Sender is TsmxCustomAlgorithm then
  begin
    CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
    ID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ID', 0);
    if CfgID <> 0 then
    begin
      FormIntf := smxProcs.gFormManagerIntf.FindByComboID(CfgID, ID);
      if FormIntf.GetReference is TsmxCustomForm then
        Form := TsmxCustomForm(FormIntf.GetReference) else
        Form := nil;
      if not Assigned(Form) then
      begin
        AccessoryForm := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
        Form := smxClassFuncs.NewForm(AccessoryForm, CfgID, nil, ID, True);
        Form.CellParent := AccessoryForm;
        Form.Initialize;
        Form.IntfID := AccessoryForm.IntfID;
      end;
      Form.Show;
    end;
  end;
end;

procedure ShowModalForm(Sender: TsmxComponent);

  {procedure ExecuteAlg(Form: TsmxCustomForm; AlgCfgID: Integer);
  var
    Alg: TsmxCustomAlgorithm;
  begin
    Alg := smxClassFuncs.GetAlgorithmForm(Form, AlgCfgID);
    if Assigned(Alg) then
    begin
      Alg.RefreshParams;
      Alg.Execute;
    end;
  end;}

var
  CfgID, ID: Integer;
  AccessoryForm, Form: TsmxCustomForm;
  //OkAlgCfgID, CancelAlgCfgID: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
  begin
    CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
    ID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ID', 0);
    //OkAlgCfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'OkAlgCfgID', 0);
    //CancelAlgCfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'CancelAlgCfgID', 0);
    if CfgID <> 0 then
    begin
      AccessoryForm := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
      Form := smxClassFuncs.NewForm(AccessoryForm, CfgID, nil, ID, True);
      try
        Form.CellParent := AccessoryForm;
        //Form.IsNewInitialize := True;
        Form.Initialize;
        Form.IntfID := AccessoryForm.IntfID;
        Form.ShowModal;
        {if Form.ShowModal = mrOk then
        begin
          if OkAlgCfgID <> 0 then
            ExecuteAlg(Form, OkAlgCfgID);
        end else
        begin
          if CancelAlgCfgID <> 0 then
            ExecuteAlg(Form, CancelAlgCfgID);
        end;}
      finally
        Form.Free;
      end;
    end;
  end;
end;

procedure CloseModalForm(Sender: TsmxComponent);
var
  List: TList;
  i: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomForm then
      if TsmxCustomForm(TsmxCustomAlgorithm(Sender).CellEvent).ModalResult = mrOk then
      begin
        List := TList.Create;
        try
          smxClassProcs.AllCells(TsmxCustomAlgorithm(Sender).CellEvent,
            List, [TsmxCustomGrid, TsmxCustomFilterDesk]);
          for i := 0 to List.Count - 1 do
            if TObject(List[i]) is TsmxCustomGrid then
              TsmxCustomGrid(List[i]).Apply else
            if TObject(List[i]) is TsmxCustomFilterDesk then
              TsmxCustomFilterDesk(List[i]).Apply;
        finally
          List.Free;
        end;
      end;
end;

(*procedure GetPropsTree(Sender: TsmxComponent);

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

      function IsImplIntf(const AIntf: IsmxRefPersistent): Boolean;
      var
        Intf: IsmxRefPersistent;
      begin
        Result := AObject.GetInterface(IsmxRefPersistent, Intf)
          and (IsmxRefPersistent(Intf).GetReference = AIntf.GetReference);
      end;

      function FindImplObj(const AIntf: IsmxRefPersistent): TObject;
      var
        i: Integer;
        Intf: IInterface;
      begin
        Result := nil;
        if Assigned(AIntf) and Assigned(AFindList) then
        begin
          for i := 0 to AFindList.Count - 1 do
            if TObject(AFindList[i]).GetInterface(IsmxRefPersistent, Intf)
                and (IsmxRefPersistent(Intf).GetReference = AIntf.GetReference) then
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
      if SysUtils.Supports(Intf, IsmxRefPersistent) then
      begin
        if IsImplIntf(IsmxRefPersistent(Intf)) then
        begin
          AddValue(APropInfo, ARow, etNone);
          ATree.TreeCaptions[1, ARow] := Format('(%s)', [IsmxRefPersistent(Intf).GetReference.ClassName]);
          AddProps(IsmxRefPersistent(Intf).GetReference, ATree, ARow, AFindList);
        end else
        begin
          AddValue(APropInfo, ARow, etPickString);
          Obj := FindImplObj(IsmxRefPersistent(Intf));
          if Obj is TsmxBaseCell then
            ATree.TreeCaptions[1, ARow] := TsmxBaseCell(Obj).Name;
        end;
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

var
  CfgID, CfgType: Integer;
  Tree: TsmxCustomTree;
  Obj: TObject;
  FindList: TList;
  Form: TsmxCustomForm;
  //CellClass: TsmxBaseCellClass;
  //IntfClass: TsmxInterfacedPersistentClass;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomTree then
    begin
      Tree := TsmxCustomTree(TsmxCustomAlgorithm(Sender).CellEvent);
      CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
      CfgType := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfType', 0);
      if CfgID <> 0 then
      begin
        Obj := nil;
        FindList := TList.Create;
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
            //CellClass := smxClassFuncs.CfgIDToCellClass(CfgID);
            //IntfClass := smxClassFuncs.CfgIDToIntfClass(CfgID);
            Form := smxClassFuncs.GetAccessoryForm(Tree);
            {if Assigned(Form) and CellClass.InheritsFrom(TsmxControlCell) then
            begin
              Obj := Form.Slaves[1].Slaves[0].Slaves[0].AddSlaveAsClass(TsmxOwnerCellClass(CellClass), IntfClass);
              TsmxControlCell(Obj).CellVisible := True;
            end else
              //Obj := smxClassFuncs.NewCell(nil, CfgID);
              Obj := CellClass.Create(nil, IntfClass);
              TsmxBaseCell(Obj).CellParent := Tree;
            end;}
            Obj := smxClassFuncs.NewCell(nil, CfgID);
            TsmxBaseCell(Obj).CellParent := Form.Slaves[1].Slaves[0].Slaves[0];
            TsmxBaseCell(Obj).CfgID := CfgID;
            TsmxBaseCell(Obj).Initialize;
            smxClassProcs.AllCells(TsmxBaseCell(Obj), FindList, []);
            FindList.Add(Obj);
            if Obj is TsmxControlCell then
              TsmxControlCell(Obj).CellVisible := True;
          end;
          if Assigned(Obj) then
            AddProps(Obj, Tree, Tree.RootRow, FindList);
        finally
          FindList.Free;
          if Assigned(Obj) and (CfgType = 100) then
            Obj.Free;
        end;
      end;
    end;  
end;

procedure SetPropsTree(Sender: TsmxComponent);
var
  //CfgID, CfgType: Integer;
  //Cfg: TsmxBaseCfg;
  //Grid: TsmxCustomGrid;
  //PropNameColumn, PropValueColumn: String;
  //i, j: Integer;
  //PropName: String;
  //PropValue: Variant;
  PropKit: Integer;
  Cfg: TsmxBaseCfg;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomGrid then
    begin
      {Grid := TsmxCustomGrid(TsmxCustomAlgorithm(Sender).CellEvent);
      CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
      CfgType := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfType', 0);
      PropNameColumn := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropNameColumn', '');
      PropValueColumn := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropValueColumn', '');
      if (CfgID <> 0) and (PropNameColumn <> '') and (PropValueColumn <> '') then
      begin}
      PropKit := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropKit', 0);
      Exit;
        if Assigned(Cfg) and (PropKit = 0) then
        begin
          //Cfg.Return;
          Cfg.Write;
          Cfg.Save;
        end;

        {if CfgType = 100 then // типы
        begin
          Cfg := TsmxTypeCfg.Create(nil);
          Cfg.CfgID := CfgID;
          Cfg.SelectRequest := smxClassProcs.gSelectRequest;
        end else
          Cfg := smxClassFuncs.NewCfg(nil, CfgID);
        try
          for i := 0 to Grid.RowCount - 1 do
          begin
            PropName := '';
            PropValue := '';
            for j := 0 to Grid.SlaveCount - 1 do
            begin
              if Grid.Slaves[j].SlaveName = PropNameColumn then
                PropName := Grid.GridCaptions[j, i] else
              if Grid.Slaves[j].SlaveName = PropValueColumn then
                PropValue := Grid.GridValues[j, i];
            end;
            if PropName <> '' then
            begin
              if TypInfo.GetPropInfo(Cfg, PropName)^.PropType^^.Kind = tkClass then
                TypInfo.SetObjectProp(Cfg, PropName, nil)
              else
                TypInfo.SetPropValue(Cfg, PropName, PropValue);
            end;
          end;
          Cfg.Return;
        finally
          Cfg.Free;
        end;
      end;}
    end;
end;*)

(*procedure GetTypeKindNameGrid(Sender: TsmxComponent);
var
  Column: TsmxCustomColumn;
  CfgID: Integer;
  Cfg: TsmxBaseCfg;
  s: String;
  TypeData: PTypeData;
  Obj: TObject;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomColumn then
    begin
      Column := TsmxCustomColumn(TsmxCustomAlgorithm(Sender).CellEvent);
      CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
      if CfgID <> 0 then
      begin
        {Cfg := smxClassFuncs.NewCfg(nil, CfgID);
        try
          Cfg.Receive;
          s := TypInfo.GetEnumName(TypeInfo(TTypeKind), Integer(TypInfo.GetPropInfo(Cfg, Column.ColumnCaption)^.PropType^.Kind));

          if TypInfo.GetPropInfo(Cfg, Column.ColumnCaption)^.PropType^.Kind = tkClass then
          begin
            Obj := TypInfo.GetObjectProp(Cfg, Column.ColumnCaption);
            TypeData := TypInfo.GetTypeData(TypInfo.GetPropInfo(Cfg, Column.ColumnCaption)^.PropType^);
            if Assigned(TypeData) then
              s := s + ' ' + TypeData^.ClassType.ClassName;// + ' ' + inttostr(TypeData^.PropCount);
            if Obj is TsmxControlKit then
              s := s + ' ' + inttostr(TsmxControlKit(Obj).Count);
          end;

          smxFuncs.Inf(s);
        finally
          Cfg.Free;
        end;}
      end;
    end;
end;*)

procedure CloseOkForm(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
begin
  if Sender is TsmxCustomAlgorithm then
  begin
    Form := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
    if Assigned(Form) then
      Form.ModalResult := mrOk;
  end;
end;

procedure CloseCancelForm(Sender: TsmxComponent);
var
  Form: TsmxCustomForm;
begin
  if Sender is TsmxCustomAlgorithm then
  begin
    Form := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
    if Assigned(Form) then
      Form.ModalResult := mrCancel;
  end;
end;

procedure ShowMessageForm(Sender: TsmxComponent);
begin
  smxFuncs.Inf('Message');
end;

procedure ChangeRowPropsGrid(Sender: TsmxComponent);
var
  Grid: TsmxCustomGrid;
  RowIndex: Integer;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomGrid then
    begin
      Grid := TsmxCustomGrid(TsmxCustomAlgorithm(Sender).CellEvent);
      RowIndex := Grid.FocusedRowIndex;
      if RowIndex <> -1 then
        if not ((Grid.GridCaptions[2, RowIndex] = '2') or (Grid.GridCaptions[2, RowIndex] = '3')) then
          Grid.Slaves[2].ColumnOptions := Grid.Slaves[2].ColumnOptions + [coEditing] else
          Grid.Slaves[2].ColumnOptions := Grid.Slaves[2].ColumnOptions - [coEditing];
    end;
end;

procedure ClickColumnPropsGrid(Sender: TsmxComponent);
var
  Column: TsmxCustomColumn;
  PropKit: Integer;
  FormCfgID: Integer;
  AccessoryForm, Form: TsmxCustomForm;
  Cfg: TsmxBaseCfg;
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomColumn then
    begin
      Exit;
      Column := TsmxCustomColumn(TsmxCustomAlgorithm(Sender).CellEvent);
      Cfg := nil;
      if Assigned(Column.CellOwner) and Assigned(Cfg) then
      begin
        PropKit := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropKit', 0);
        FormCfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'FormCfgID', 0);
        if (PropKit in [2, 3]) and (FormCfgID <> 0) then
        begin
          AccessoryForm := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
          Form := smxClassFuncs.NewForm(AccessoryForm, FormCfgID, nil, 0, True);
          try
            Form.CellParent := AccessoryForm;
            Form.Initialize;
            Form.IntfID := AccessoryForm.IntfID;
            Form.ShowModal;
          finally
            Form.Free;
          end;
        end;
      end;
    end;
end;

{initialization

finalization
  if Assigned(Cfg) then
    Cfg.Free;}

end.

