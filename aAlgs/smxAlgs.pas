unit smxAlgs;

interface

uses
  smxBaseClasses;

//type
  //{ EsmxAlgorithmError }

  //EsmxAlgorithmError = class(Exception);

procedure PrepareForm(Component: TsmxComponent);
procedure OpenModalForm(Component: TsmxComponent);
{procedure OpenFormByEventID(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure OpenFormByProblemID(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure CloseForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure RefreshForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure ApplyForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure SelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure UnSelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure ChangeFilterValue(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure ChangeStateForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure SelectAndPerformRequestA(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure SelectAndPerformRequestF(Algorithm: TsmxCustomAlgorithm; Params: Variant);}

implementation

uses
  {Windows, Controls, Variants, smxClassFuncs, smxLibFuncs, smxConsts, smxDBIntf,
  smxTypes, smxClassProcs, smxFuncs}
  Classes, Controls, SysUtils, smxClasses, smxClassProcs, smxClassFuncs;

procedure PrepareForm(Component: TsmxComponent);
var
  List: TList;
  i: Integer;
begin
  if Component is TsmxCustomAlgorithm then
    if TsmxCustomAlgorithm(Component).ActionCell is TsmxCustomForm then
    begin
      List := TList.Create;
      try
        smxClassProcs.AllCells(TsmxCustomForm(TsmxCustomAlgorithm(Component).ActionCell),
          List, [TsmxCustomGrid], True);
        for i := 0 to List.Count - 1 do
          TsmxCustomGrid(List[i]).Prepare;
      finally
        List.Free;
      end;
    end;
end;

procedure OpenModalForm(Component: TsmxComponent);
var
  Param: TsmxParam;
  CfgID, ID: Integer;
  RefreshGrid: Boolean;
  AccessoryForm, Form: TsmxCustomForm;
begin
  if Component is TsmxCustomAlgorithm then
  begin
    Param := TsmxCustomAlgorithm(Component).AlgorithmParams.FindByName('CfgID');
    if Assigned(Param) then
      CfgID := Param.ParamValue else
      CfgID := 0;
    Param := TsmxCustomAlgorithm(Component).AlgorithmParams.FindByName('ID');
    if Assigned(Param) then
      ID := Param.ParamValue else
      ID := 0;
    Param := TsmxCustomAlgorithm(Component).AlgorithmParams.FindByName('RefreshGrid');
    if Assigned(Param) then
      RefreshGrid := Param.ParamValue else //SysUtils.StrToBool(Param.ParamValue) else
      RefreshGrid := False;
    if CfgID > 0 then
    begin
      AccessoryForm := smxClassFuncs.GetAccessoryForm(TsmxCustomAlgorithm(Component));
      Form := smxClassFuncs.NewForm(AccessoryForm, CfgID, nil, ID);
      try
        Form.CellParent := AccessoryForm;
        {Form.StorageManager := smxLibFuncs.GetStorageManager;
        Form.LibraryManager := smxLibFuncs.GetLibraryManager;
        Form.DatabaseManager := smxLibFuncs.GetDatabaseManager;
        Form.FormManager := smxLibFuncs.GetFormManager;
        Form.ImageListManager := smxLibFuncs.GetImageListManager;}
        Form.Initialize;
        Form.IntfID := AccessoryForm.IntfID;
        if Form.ShowModal = mrOk then
          if TsmxCustomAlgorithm(Component).ActionCell is TsmxCustomGrid then
            if RefreshGrid then
              TsmxCustomGrid(TsmxCustomAlgorithm(Component).ActionCell).Refresh;
      finally
        Form.Free;
      end;
    end;
  end;
end;

{function PerformRequest(ARequest: TsmxCustomRequest; ASame: Boolean = False): Boolean;
var
  res: Integer;
  msg: String;
begin
  Result := False;
  if not Assigned(ARequest) then
    Exit;
  if not Assigned(ARequest.Database) or not Assigned(ARequest.CellDataSet) then
    Exit;
  with ARequest do
  begin
    if not Database.InTransaction then
      Database.StartTransaction;
    try
      res := 1; msg := '';
      Perform;//(ASame);
      case CellDataSet.DataSetType of
        dstQuery:
        begin
          res := GetFieldSenseValueDef(ARequest, fsResult, 1);
          msg := GetFieldSenseValueDef(ARequest, fsMessage, '');
        end;
        dstStoredProc:
        begin
          res := GetParamLocationValueDef(ARequest, plResult, 1);
          msg := GetParamLocationValueDef(ARequest, plMessage, '');
        end;
      end;
      if res = 0 then
        Database.CommitTransaction else
        Database.RollbackTransaction;
      if msg <> '' then
        Inf(msg);
      Result := res = 0;
    except
      Database.RollbackTransaction;
    end;
  end;
end;}

{procedure OpenForm(Component: TsmxComponent);
begin
  if Component is TsmxCustomAlgorithm then
  begin
    nil;
  end;
end;}

{procedure OpenForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName: String; FormCfgID, FormID, IntfID: Integer; ap: TsmxParams;
  c: TsmxBaseCell; f: TsmxCustomForm;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := GetParamValueDef(ap, 'FormCfgID', 0);
      FormID := GetParamValueDef(ap, 'FormID', 0);
      IntfID := GetParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if FormCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      //f := FrmManagerLib.FindByComboID(FormCfgID, FormID);
      f := GetFormByComboIDLib(FormCfgID, FormID);
      if Assigned(f) then
      begin
        f.ShowForm;
      end else
      begin
        //f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        f := NewForm(nil, GetDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        with f do
        begin
          //if IsCellLib(c, 'TsmxCustomForm') then
          if c is TsmxCustomForm then
            ParentForm := TsmxCustomForm(c);
          CommonStorage := ComStorageLib;
          LibraryManager := LibManagerLib;
          DatabaseManager := DBManagerLib;
          FormManager := FrmManagerLib;
          ImageList := ImgListLib;
          ShowForm;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure OpenFormByEventID(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName: String; FormCfgID, FormID, IntfID: Integer; ap: TsmxParams;
  c: TsmxBaseCell; f: TsmxCustomForm;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := GetParamValueDef(ap, 'FormCfgID', 0);
      FormID := GetParamValueDef(ap, '@EventID', 0);
      IntfID := GetParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (FormID > 0) then
    begin
      c := Algorithm.RootCell;
      //f := FrmManagerLib.FindByComboID(FormCfgID, FormID);
      f := GetFormByComboIDLib(FormCfgID, FormID);
      if Assigned(f) then
      begin
        f.ShowForm;
      end else
      begin
        //f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        f := NewForm(nil, GetDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        with f do
        begin
          //if IsCellLib(c, 'TsmxCustomForm') then
          if c is TsmxCustomForm then
            ParentForm := TsmxCustomForm(c);
          CommonStorage := ComStorageLib;
          LibraryManager := LibManagerLib;
          DatabaseManager := DBManagerLib;
          FormManager := FrmManagerLib;
          ImageList := ImgListLib;
          ShowForm;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure OpenFormByProblemID(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName: String; FormCfgID, FormID, IntfID: Integer; ap: TsmxParams;
  c: TsmxBaseCell; f: TsmxCustomForm;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := GetParamValueDef(ap, 'FormCfgID', 0);
      FormID := GetParamValueDef(ap, '@ProblemID', 0);
      IntfID := GetParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (FormID > 0) then
    begin
      c := Algorithm.RootCell;
      //f := FrmManagerLib.FindByComboID(FormCfgID, FormID);
      f := GetFormByComboIDLib(FormCfgID, FormID);
      if Assigned(f) then
      begin
        f.ShowForm;
      end else
      begin
        //f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        f := NewForm(nil, GetDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        with f do
        begin
          //if IsCellLib(c, 'TsmxCustomForm') then
          if c is TsmxCustomForm then
            ParentForm := TsmxCustomForm(c);
          CommonStorage := ComStorageLib;
          LibraryManager := LibManagerLib;
          DatabaseManager := DBManagerLib;
          FormManager := FrmManagerLib;
          ImageList := ImgListLib;
          ShowForm;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure CloseForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  //if IsCellLib(c, 'TsmxCustomForm') then
  if c is TsmxCustomForm then
    TsmxCustomForm(c).CloseForm;
end;

procedure RefreshForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  //if IsCellLib(c, 'TsmxCustomForm') then
  if c is TsmxCustomForm then
    TsmxCustomForm(c).Prepare(True);
end;

procedure ApplyForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  //if IsCellLib(c, 'TsmxCustomForm') then
  if c is TsmxCustomForm then
    TsmxCustomForm(c).Apply;
end;

procedure SelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  //if IsCellLib(c, 'TsmxCustomForm') then
  if c is TsmxCustomForm then
    TsmxCustomForm(c).FormModalResult := mrOk;
end;

procedure UnSelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  //if IsCellLib(c, 'TsmxCustomForm') then
  if c is TsmxCustomForm then
    TsmxCustomForm(c).FormModalResult := mrCancel;
end;

procedure ChangeFilterValue(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName: String; FormCfgID, FormID, IntfID: Integer; ap: TsmxParams;
  c: TsmxBaseCell; flt: TsmxCustomFilter; r: TsmxCustomRequest; fld: IsmxField;
  f: TsmxCustomForm;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := GetParamValueDef(ap, 'FormCfgID', 0);
      FormID := GetParamValueDef(ap, 'FormID', 0);
      IntfID := GetParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if FormCfgID > 0 then
    begin
      c := Algorithm.ParentCell;
      if c is TsmxCustomFilter then
        flt := TsmxCustomFilter(c) else
        flt := nil;
      c := Algorithm.RootCell;
      //if IsCellLib(flt, 'TsmxCustomFilter') then
      if Assigned(flt) then
      begin
        //f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        f := NewForm(nil, GetDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        try
          //if IsCellLib(c, 'TsmxCustomForm') then
          if c is TsmxCustomForm then
            f.ParentForm := TsmxCustomForm(c);
          f.CommonStorage := ComStorageLib;
          f.LibraryManager := LibManagerLib;
          f.DatabaseManager := DBManagerLib;
          f.FormManager := FrmManagerLib;
          f.ImageList := ImgListLib;
          if f.ShowModalForm = mrOk then
          begin
            r := nil;
            if Assigned(f.PageManagers[0]) then
              if Assigned(f.PageManagers[0].ActivePage) then
                if Assigned(f.PageManagers[0].ActivePage.Sections[0]) then
                  r := f.PageManagers[0].ActivePage.Sections[0].Request;
            if Assigned(r) then
            begin
              fld := r.FindFieldSense(fsKey);
              if Assigned(fld) then
                flt.FilterValue := fld.Value;
              fld := r.FindFieldSense(fsValue);
              if Assigned(fld) then
                flt.FilterText := fld.Value;
            end;
          end;
        finally
          f.Free;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure ChangeStateForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName, RequestDBName: String; RequestCfgID: Integer; FormRefresh: Boolean;
  ap: TsmxParams; c, r: TsmxBaseCell;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      RequestCfgID := GetParamValueDef(ap, 'RequestCfgID', 0);
      RequestDBName := GetParamValueDef(ap, 'RequestDBName', '');
      FormRefresh := GetParamValueDef(ap, 'FormRefresh', False);
    finally
      ap.Free;
    end;
    if RequestCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      //if IsCellLib(c, 'TsmxCustomForm') then
      if c is TsmxCustomForm then
      begin
        //r := NewCellLib(FindDatabaseByNameLib(CfgDBName), RequestCfgID);
        r := NewCell(nil, GetDatabaseByNameLib(CfgDBName), RequestCfgID);
        try
          //if not IsCellLib(r, 'TsmxCustomRequest') then
          if not (r is TsmxCustomRequest) then
            raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
          with TsmxCustomRequest(r) do
          begin
            ParentCell := c;
            //DatabaseName := RequestDBName;
            Database := GetDatabaseByNameLib(RequestDBName);
          end;
          if PerformRequest(TsmxCustomRequest(r)) then
            TsmxCustomForm(c).Prepare(FormRefresh);
        finally
          r.Free;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure SelectAndPerformRequestA(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName, RequestDBName: String; FormCfgID, FormID, RequestCfgID, IntfID: Integer;
  FormRefresh: Boolean; ap: TsmxParams; c, r: TsmxBaseCell; f: TsmxCustomForm;
  r2: TsmxCustomRequest; fld: IsmxField; prm: TsmxParam;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := GetParamValueDef(ap, 'FormCfgID', 0);
      FormID := GetParamValueDef(ap, 'FormID', 0);
      RequestCfgID := GetParamValueDef(ap, 'RequestCfgID', 0);
      RequestDBName := GetParamValueDef(ap, 'RequestDBName', '');
      IntfID := GetParamValueDef(ap, 'IntfID', 0);
      FormRefresh := GetParamValueDef(ap, 'FormRefresh', False);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (RequestCfgID > 0) then
    begin
      c := Algorithm.RootCell;
      //f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
      f := NewForm(nil, GetDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
      try
        //if IsCellLib(c, 'TsmxCustomForm') then
        if c is TsmxCustomForm then
          f.ParentForm := TsmxCustomForm(c);
        f.CommonStorage := ComStorageLib;
        f.LibraryManager := LibManagerLib;
        f.DatabaseManager := DBManagerLib;
        f.FormManager := FrmManagerLib;
        f.ImageList := ImgListLib;
        if f.ShowModalForm = mrOk then
        begin
          r2 := nil;
          if Assigned(f.PageManagers[0]) then
            if Assigned(f.PageManagers[0].ActivePage) then
              if Assigned(f.PageManagers[0].ActivePage.Sections[0]) then
                r2 := f.PageManagers[0].ActivePage.Sections[0].Request;
          if Assigned(r2) then
          begin
            prm := Algorithm.FindParamLocation(plInput);
            while Assigned(prm) do
            begin
              fld := r2.CellDataSet.FindField(prm.ParamName);
              if Assigned(fld) then
                prm.ParamValue := fld.Value;
              prm := Algorithm.FindParamLocation(plInput, prm.ItemIndex + 1);
            end;
          end;
          //r := NewCellLib(FindDatabaseByNameLib(CfgDBName), RequestCfgID);
          r := NewCell(nil, GetDatabaseByNameLib(CfgDBName), RequestCfgID);
          try
            //if not IsCellLib(r, 'TsmxCustomRequest') then
            if not (r is TsmxCustomRequest) then
              raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
            with TsmxCustomRequest(r) do
            begin
              ParentCell := Algorithm;
              Database := GetDatabaseByNameLib(RequestDBName);
            end;
            if PerformRequest(TsmxCustomRequest(r)) then
              //if IsCellLib(c, 'TsmxCustomForm') then
              if c is TsmxCustomForm then
                TsmxCustomForm(c).Prepare(FormRefresh);
          finally
            r.Free;
          end;
        end;
      finally
        f.Free;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure SelectAndPerformRequestF(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName, RequestDBName: String; FormCfgID, FormID, RequestCfgID, IntfID: Integer;
  FormRefresh: Boolean; ap: TsmxParams; c, r: TsmxBaseCell; f: TsmxCustomForm;
  r2: TsmxCustomRequest; fld: IsmxField; prm: IsmxParam;
begin
  try
    ap := TsmxParams.Create(TsmxParam);
    smxClassProcs.VarToParams(Params, ap);
    try
      CfgDBName := GetParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := GetParamValueDef(ap, 'FormCfgID', 0);
      FormID := GetParamValueDef(ap, 'FormID', 0);
      RequestCfgID := GetParamValueDef(ap, 'RequestCfgID', 0);
      RequestDBName := GetParamValueDef(ap, 'RequestDBName', '');
      IntfID := GetParamValueDef(ap, 'IntfID', 0);
      FormRefresh := GetParamValueDef(ap, 'FormRefresh', False);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (RequestCfgID > 0) then
    begin
      c := Algorithm.RootCell;
      //if IsCellLib(c, 'TsmxCustomForm') then
      if c is TsmxCustomForm then
      begin
        //f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        f := NewForm(nil, GetDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        try
          f.ParentForm := TsmxCustomForm(c);
          f.CommonStorage := ComStorageLib;
          f.LibraryManager := LibManagerLib;
          f.DatabaseManager := DBManagerLib;
          f.FormManager := FrmManagerLib;
          f.ImageList := ImgListLib;
          if f.ShowModalForm = mrOk then
          begin
            r2 := nil;
            if Assigned(f.PageManagers[0]) then
              if Assigned(f.PageManagers[0].ActivePage) then
                if Assigned(f.PageManagers[0].ActivePage.Sections[0]) then
                  r2 := f.PageManagers[0].ActivePage.Sections[0].Request;
            //r := NewCellLib(FindDatabaseByNameLib(CfgDBName), RequestCfgID);
            r := NewCell(nil, GetDatabaseByNameLib(CfgDBName), RequestCfgID);
            try
              //if not IsCellLib(r, 'TsmxCustomRequest') then
              if not (r is TsmxCustomRequest) then
                raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
              with TsmxCustomRequest(r) do
              begin
                ParentCell := c;
                Database := GetDatabaseByNameLib(RequestDBName);
                RefreshParams;
                if Assigned(r2) then
                begin
                  prm := FindParamLocation(plInput);
                  while Assigned(prm) do
                  begin
                    fld := r2.CellDataSet.FindField(prm.ParamName);
                    if Assigned(fld) then
                      prm.Value := fld.Value;
                    prm := FindParamLocation(plInput, prm.ParamNo + 1);
                  end;
                end;
              end;
              if PerformRequest(TsmxCustomRequest(r), True) then
                TsmxCustomForm(c).Prepare(FormRefresh);
            finally
              r.Free;
            end;
          end;
        finally
          f.Free;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;}

end.

