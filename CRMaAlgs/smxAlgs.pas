unit smxAlgs;

interface

uses
  SysUtils, smxClasses;

type
  { EsmxAlgorithmError }

  EsmxAlgorithmError = class(Exception);

procedure OpenForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure OpenFormByEventID(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure OpenFormByProblemID(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure CloseForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure RefreshForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure ApplyForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure SelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure UnSelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure ChangeFilterValue(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure ChangeStateForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure SelectAndPerformRequestA(Algorithm: TsmxCustomAlgorithm; Params: Variant);
procedure SelectAndPerformRequestF(Algorithm: TsmxCustomAlgorithm; Params: Variant);

implementation

uses
  Windows, Controls, Variants, smxClassFuncs, smxLibFuncs, smxConsts, smxDBIntf,
  smxTypes;

procedure OpenForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName: String; FormCfgID, FormID, IntfID: Integer; ap: TsmxParams;
  c: TsmxBaseCell; f: TsmxCustomForm;
begin
  try
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, 'FormID', 0);
      IntfID := ParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if FormCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      f := FindFormByComboIDLib(FormCfgID, FormID);
      if Assigned(f) then
      begin
        f.ShowForm;
      end else
      begin
        f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        with f do
        begin
          if IsCellLib(c, 'TsmxCustomForm') then
            ParentForm := TsmxCustomForm(c);
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
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, '@EventID', 0);
      IntfID := ParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (FormID > 0) then
    begin
      c := Algorithm.RootCell;
      f := FindFormByComboIDLib(FormCfgID, FormID);
      if Assigned(f) then
      begin
        f.ShowForm;
      end else
      begin
        f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        with f do
        begin
          if IsCellLib(c, 'TsmxCustomForm') then
            ParentForm := TsmxCustomForm(c);
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
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, '@ProblemID', 0);
      IntfID := ParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (FormID > 0) then
    begin
      c := Algorithm.RootCell;
      f := FindFormByComboIDLib(FormCfgID, FormID);
      if Assigned(f) then
      begin
        f.ShowForm;
      end else
      begin
        f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        with f do
        begin
          if IsCellLib(c, 'TsmxCustomForm') then
            ParentForm := TsmxCustomForm(c);
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
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).CloseForm;
end;

procedure RefreshForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).Prepare(True);
end;

procedure ApplyForm(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).Apply;
end;

procedure SelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).FormModalResult := mrOk;
end;

procedure UnSelectRecord(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var c: TsmxBaseCell;
begin
  c := Algorithm.RootCell;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).FormModalResult := mrCancel;
end;

procedure ChangeFilterValue(Algorithm: TsmxCustomAlgorithm; Params: Variant);
var CfgDBName: String; FormCfgID, FormID, IntfID: Integer; ap: TsmxParams;
  c, flt: TsmxBaseCell; r: TsmxCustomRequest; fld: IsmxField; f: TsmxCustomForm;
begin
  try
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, 'FormID', 0);
      IntfID := ParamValueDef(ap, 'IntfID', 0);
    finally
      ap.Free;
    end;
    if FormCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      flt := Algorithm.ParentCell;
      if IsCellLib(flt, 'TsmxCustomFilter') then
      begin
        f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        try
          with f do
          begin
            if IsCellLib(c, 'TsmxCustomForm') then
              ParentForm := TsmxCustomForm(c);
            if ShowModalForm = mrOk then
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
                  TsmxCustomFilter(flt).FilterValue := fld.Value;
                fld := r.FindFieldSense(fsValue);
                if Assigned(fld) then
                  TsmxCustomFilter(flt).FilterText := fld.Value;
              end;
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
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      RequestCfgID := ParamValueDef(ap, 'RequestCfgID', 0);
      RequestDBName := ParamValueDef(ap, 'RequestDBName', '');
      FormRefresh := ParamValueDef(ap, 'FormRefresh', False);
    finally
      ap.Free;
    end;
    if RequestCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      if IsCellLib(c, 'TsmxCustomForm') then
      begin
        r := NewCellLib(FindDatabaseByNameLib(CfgDBName), RequestCfgID);
        try
          if not IsCellLib(r, 'TsmxCustomRequest') then
            raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
          with TsmxCustomRequest(r) do
          begin
            ParentCell := c;
            DatabaseName := RequestDBName;
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
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, 'FormID', 0);
      RequestCfgID := ParamValueDef(ap, 'RequestCfgID', 0);
      RequestDBName := ParamValueDef(ap, 'RequestDBName', '');
      IntfID := ParamValueDef(ap, 'IntfID', 0);
      FormRefresh := ParamValueDef(ap, 'FormRefresh', False);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (RequestCfgID > 0) then
    begin
      c := Algorithm.RootCell;
      f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
      try
        with f do
        begin
          if IsCellLib(c, 'TsmxCustomForm') then
            ParentForm := TsmxCustomForm(c);
          if ShowModalForm = mrOk then
          begin
            r2 := nil;
            if Assigned(PageManagers[0]) then
              if Assigned(PageManagers[0].ActivePage) then
                if Assigned(PageManagers[0].ActivePage.Sections[0]) then
                  r2 := PageManagers[0].ActivePage.Sections[0].Request;
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
            r := NewCellLib(FindDatabaseByNameLib(CfgDBName), RequestCfgID);
            try
              if not IsCellLib(r, 'TsmxCustomRequest') then
                raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
              with TsmxCustomRequest(r) do
              begin
                ParentCell := Algorithm;
                DatabaseName := RequestDBName;
              end;
              if PerformRequest(TsmxCustomRequest(r)) then
                if IsCellLib(c, 'TsmxCustomForm') then
                  TsmxCustomForm(c).Prepare(FormRefresh);
            finally
              r.Free;
            end;
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
    ap := VarToParams(Params);
    try
      CfgDBName := ParamValueDef(ap, 'CfgDBName', '');
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, 'FormID', 0);
      RequestCfgID := ParamValueDef(ap, 'RequestCfgID', 0);
      RequestDBName := ParamValueDef(ap, 'RequestDBName', '');
      IntfID := ParamValueDef(ap, 'IntfID', 0);
      FormRefresh := ParamValueDef(ap, 'FormRefresh', False);
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (RequestCfgID > 0) then
    begin
      c := Algorithm.RootCell;
      if IsCellLib(c, 'TsmxCustomForm') then
      begin
        f := NewFormLib(FindDatabaseByNameLib(CfgDBName), FormCfgID, IntfID, FormID);
        try
          with f do
          begin
            ParentForm := TsmxCustomForm(c);
            if ShowModalForm = mrOk then
            begin
              r2 := nil;
              if Assigned(PageManagers[0]) then
                if Assigned(PageManagers[0].ActivePage) then
                  if Assigned(PageManagers[0].ActivePage.Sections[0]) then
                    r2 := PageManagers[0].ActivePage.Sections[0].Request;
              r := NewCellLib(FindDatabaseByNameLib(CfgDBName), RequestCfgID);
              try
                if not IsCellLib(r, 'TsmxCustomRequest') then
                  raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
                with TsmxCustomRequest(r) do
                begin
                  ParentCell := c;
                  DatabaseName := RequestDBName;
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

end.
