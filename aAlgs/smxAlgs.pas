unit smxAlgs;

interface

uses
  smxBaseClasses;

procedure ApplyForm(Sender: TsmxComponent);
procedure PrepareForm(Sender: TsmxComponent);
procedure RefreshForm(Sender: TsmxComponent);
procedure ShowModalForm(Sender: TsmxComponent);
procedure CloseModalForm(Sender: TsmxComponent);
procedure GetCfgPropsGrid(Sender: TsmxComponent);
procedure SetCfgPropsGrid(Sender: TsmxComponent);
//procedure GetTypeKindNameGrid(Sender: TsmxComponent);
procedure CloseOkForm(Sender: TsmxComponent);
procedure CloseCancelForm(Sender: TsmxComponent);
procedure ShowMessageForm(Sender: TsmxComponent);
procedure ChangeRowPropsGrid(Sender: TsmxComponent);
procedure ClickColumnPropsGrid(Sender: TsmxComponent);

implementation

uses
  Classes, Controls, SysUtils, TypInfo, Variants, smxClasses, smxFuncs,
  smxClassProcs, smxClassFuncs, smxTypes;

var
  Cfg: TsmxBaseCfg = nil;

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
          List, [TsmxCustomGrid, TsmxCustomFilterDesk],
          TsmxCustomForm(TsmxCustomAlgorithm(Sender).CellEvent).CellVisible);
        for i := 0 to List.Count - 1 do
          if TObject(List[i]) is TsmxCustomGrid then
            TsmxCustomGrid(List[i]).Prepare else
          if TObject(List[i]) is TsmxCustomFilterDesk then
            TsmxCustomFilterDesk(List[i]).Prepare;
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
          List, [TsmxCustomGrid, TsmxCustomFilterDesk], True);
        for i := 0 to List.Count - 1 do
          if TObject(List[i]) is TsmxCustomGrid then
            TsmxCustomGrid(List[i]).Refresh else
          if TObject(List[i]) is TsmxCustomFilterDesk then
            TsmxCustomFilterDesk(List[i]).Refresh;
      finally
        List.Free;
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
      Form := smxClassFuncs.NewForm(AccessoryForm, CfgID, nil, ID);
      try
        Form.CellParent := AccessoryForm;
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

procedure GetCfgPropsGrid(Sender: TsmxComponent);

  function GetParentObj(ParentPropName: String): TObject;
  var
    List: TStrings;
    i: Integer;
  begin
    Result := nil;
    if Assigned(Cfg) and (ParentPropName <> '') then
    begin
      List := TStringList.Create;
      try
        List.Delimiter := '.';
        List.DelimitedText := ParentPropName;
        Result := Cfg;
        for i := 0 to List.Count - 1 do
          Result := TypInfo.GetObjectProp(Result, List[i]);
      finally
        List.Free;
      end;
    end;
  end;

var
  CfgID, CfgType: Integer;
  //Cfg: TsmxBaseCfg;
  PropList: PPropList;
  Count: Integer;
  Grid: TsmxCustomGrid;
  //PropNameColumn, PropValueColumn: String;
  i{, j}: Integer;
  //s: String;
  TypeData: PTypeData;
  ParentPropKit: Integer;
  ParentPropName, ParentPropValue: String;
  TypeInfo: PTypeInfo;
  Obj: TObject;
begin                           inf('yes');
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomGrid then
    begin
      Grid := TsmxCustomGrid(TsmxCustomAlgorithm(Sender).CellEvent);
      CfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfID', 0);
      CfgType := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ConfType', 0);
      ParentPropKit := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ParentPropKit', 0);
      ParentPropName := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ParentPropName', '');
      ParentPropValue := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'ParentPropValue', '');
      //PropNameColumn := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropNameColumn', '');
      //PropValueColumn := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropValueColumn', '');
      if (CfgID <> 0) {and (PropNameColumn <> '') and (PropValueColumn <> '')} then
      begin
        Obj := nil;
        //TypeInfo := nil;
        if ParentPropKit = 0 then
        begin
          if Assigned(Cfg) then
            SysUtils.FreeAndNil(Cfg);
          if CfgType = 100 then // типы
          begin
            Cfg := TsmxTypeCfg.Create(nil);
            Cfg.CfgID := CfgID;
            Cfg.SelectRequest := smxClassProcs.gSelectRequest;
          end else
            Cfg := smxClassFuncs.NewCfg(nil, CfgID);
        //try
          Cfg.Receive;
          Obj := Cfg;
        end else
        if (ParentPropKit = 2) and (ParentPropName <> '') then
        begin
          Obj := GetParentObj(ParentPropName);
        end;
        if Assigned(Obj) then
        begin
          TypeInfo := PTypeInfo(Obj.ClassInfo);
          Count := TypInfo.GetPropList(TypeInfo, TypInfo.tkProperties, nil);
          if Count <> 0 then
          begin
            GetMem(PropList, Count * SizeOf(Pointer));
            try
              TypInfo.GetPropList(TypeInfo, TypInfo.tkProperties, PropList);
              Grid.RowCount := Count;
              for i := 0 to Grid.RowCount - 1 do
              begin
                {for j := 0 to Grid.SlaveCount - 1 do
                begin
                  if Grid.Slaves[j].SlaveName = PropNameColumn then
                    s := PropList^[i]^.Name else
                  if Grid.Slaves[j].SlaveName = PropValueColumn then
                  begin
                    if PropList^[i]^.PropType^^.Kind = tkClass then
                      s := PropList^[i]^.PropType^^.Name else
                      s := Variants.VarToStr(TypInfo.GetPropValue(Cfg, PropList^[i]^.Name))
                  end else
                    s := '';
                  Grid.GridCaptions[j, i] := s;
                end;}
                Grid.GridCaptions[0, i] := PropList^[i]^.Name;
                if PropList^[i]^.PropType^^.Kind = tkClass then
                  Grid.GridCaptions[1, i] := PropList^[i]^.PropType^^.Name else
                  Grid.GridCaptions[1, i] := Variants.VarToStr(TypInfo.GetPropValue(Obj, PropList^[i]^.Name));
                if PropList^[i]^.PropType^^.Kind = tkClass then
                begin
                  TypeData := TypInfo.GetTypeData(PropList^[i]^.PropType^);
                  if TypeData.ClassType.InheritsFrom(TsmxSimpleKit) then
                    Grid.GridCaptions[2, i] := '3' else
                    Grid.GridCaptions[2, i] := '2';
                  if ParentPropName <> '' then
                    Grid.GridCaptions[3, i] := ParentPropName + '.' + PropList^[i]^.Name else
                    Grid.GridCaptions[3, i] := PropList^[i]^.Name;
                end else
                  Grid.GridCaptions[2, i] := '1';
              end;
            finally
              FreeMem(PropList);
            end;
          end;
        //finally
          //Cfg.Free;
        end;
      end;
    end;
end;

procedure SetCfgPropsGrid(Sender: TsmxComponent);
var
  //CfgID, CfgType: Integer;
  //Cfg: TsmxBaseCfg;
  //Grid: TsmxCustomGrid;
  //PropNameColumn, PropValueColumn: String;
  //i, j: Integer;
  //PropName: String;
  //PropValue: Variant;
  PropKit: Integer;
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

        if Assigned(Cfg) and (PropKit = 0) then
        begin
          Cfg.Return;
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
end;

procedure GetTypeKindNameGrid(Sender: TsmxComponent);
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
        Cfg := smxClassFuncs.NewCfg(nil, CfgID);
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
        end;
      end;
    end;
end;

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
begin
  if Sender is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Sender).CellEvent is TsmxCustomColumn then
    begin
      Column := TsmxCustomColumn(TsmxCustomAlgorithm(Sender).CellEvent);
      if Assigned(Column.CellOwner) and Assigned(Cfg) then
      begin
        PropKit := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'PropKit', 0);
        FormCfgID := smxFuncs.GetParamValueAs(TsmxCustomAlgorithm(Sender).AlgorithmParams, 'FormCfgID', 0);
        if (PropKit in [2, 3]) and (FormCfgID <> 0) then
        begin
          AccessoryForm := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Sender));
          Form := smxClassFuncs.NewForm(AccessoryForm, FormCfgID);
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

initialization

finalization
  if Assigned(Cfg) then
    Cfg.Free;

end.

