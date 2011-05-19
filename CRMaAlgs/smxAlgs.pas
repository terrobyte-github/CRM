unit smxAlgs;

interface

uses
  SysUtils, smxClasses;

type
  { EsmxAlgorithmError }

  EsmxAlgorithmError = class(Exception);

procedure OpenForm(Algorithm: TsmxAlgorithm; Params: Variant);
procedure OpenFormByEventID(Algorithm: TsmxAlgorithm; Params: Variant);
procedure OpenFormByProblemID(Algorithm: TsmxAlgorithm; Params: Variant);
procedure CloseForm(Algorithm: TsmxAlgorithm; Params: Variant);
procedure RefreshForm(Algorithm: TsmxAlgorithm; Params: Variant);
procedure ApplyForm(Algorithm: TsmxAlgorithm; Params: Variant);
procedure SelectRecord(Algorithm: TsmxAlgorithm; Params: Variant);
procedure UnSelectRecord(Algorithm: TsmxAlgorithm; Params: Variant);
procedure SelectOfFormSetFilterValue(Algorithm: TsmxAlgorithm; Params: Variant);
procedure PerformRequestFromForm(Algorithm: TsmxAlgorithm; Params: Variant);
procedure SelectOfFormPerformRequest(Algorithm: TsmxAlgorithm; Params: Variant);
procedure SelectOfFormPerformRequestFromAlgorithm(Algorithm: TsmxAlgorithm; Params: Variant);
procedure SelectOfFormPerformRequestFromForm(Algorithm: TsmxAlgorithm; Params: Variant);

implementation

uses
  Windows, Controls, Variants, smxCells, {smxFormManager, smxLibProcs,} smxLibFuncs,
  smxFuncs, smxCellFuncs, smxConsts, smxDBIntf, smxTypes;

procedure OpenForm(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID: Integer; ap: TsmxParams; p: TsmxParam; c, f: TsmxBaseCell;
  //Call: TsmxFuncCallBack;
begin
  try
    //FormCfgID := 0; FormID := 0;
    ap := VarToParams(Params);
    try
      FormCfgID := ParamValueDef(ap, 'FormCfgID', 0);
      FormID := ParamValueDef(ap, 'FormID', 0);
      {p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('FormID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;}
    finally
      ap.Free;
    end;
    if FormCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      //f := TsmxFormManager(Integer(Call(4))).FindByComboID(FormCfgID, FormID);
      f := FormManagerLib.FindByComboID(FormCfgID, FormID);
      if Assigned(f) then
      begin
        TsmxCustomForm(f).ShowForm;
      end else
      begin
        f := NewCellLib(FormCfgID, FormID);
        if IsCellLib(f, 'TsmxCustomForm') then
        begin
          with TsmxCustomForm(f) do
          begin
            if IsCellLib(c, 'TsmxCustomForm') then
              ParentForm := TsmxCustomForm(c);
            ShowForm;
          end;
        end else
        begin
          f.Free;
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure OpenFormByEventID(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID: Integer; ap: TsmxParams; p: TsmxParam; c, f: TsmxBaseCell;
  //Call: TsmxFuncCallBack;
begin
  try
    FormCfgID := 0; FormID := 0;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('@EventID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (FormID > 0) then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      //f := TsmxFormManager(Integer(Call(4))).FindByComboID(FormCfgID, FormID);
      f := FormManagerLib.FindByComboID(FormCfgID, FormID);
      if Assigned(f) then
      begin
        TsmxCustomForm(f).ShowForm;
      end else
      begin
        f := NewCellLib(FormCfgID, FormID);
        if IsCellLib(f, 'TsmxCustomForm') then
        begin
          with TsmxCustomForm(f) do
          begin
            if IsCellLib(c, 'TsmxCustomForm') then
              ParentForm := TsmxCustomForm(c);
            ShowForm;
          end;
        end else
        begin
          f.Free;
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure OpenFormByProblemID(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID: Integer; ap: TsmxParams; p: TsmxParam; c, f: TsmxBaseCell;
  //Call: TsmxFuncCallBack;
begin
  try
    FormCfgID := 0; FormID := 0;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('@ProblemID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (FormID > 0) then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      //f := TsmxFormManager(Integer(Call(4))).FindByComboID(FormCfgID, FormID);
      f := FormManagerLib.FindByComboID(FormCfgID, FormID);
      if Assigned(f) then
      begin
        TsmxCustomForm(f).ShowForm;
      end else
      begin
        f := NewCellLib(FormCfgID, FormID);
        if IsCellLib(f, 'TsmxCustomForm') then
        begin
          with TsmxCustomForm(f) do
          begin
            if IsCellLib(c, 'TsmxCustomForm') then
              ParentForm := TsmxCustomForm(c);
            ShowForm;
          end;
        end else
        begin
          f.Free;
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure CloseForm(Algorithm: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; //Call: TsmxFuncCallBack;
begin
  c := Algorithm.RootCell;
  //Call := Algorithm.Call;
  if IsCellLib(c, 'TsmxCustomForm') then
    //c.Free;
    TsmxCustomForm(c).CloseForm;
end;

procedure RefreshForm(Algorithm: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; //Call: TsmxFuncCallBack;
begin
  c := Algorithm.RootCell;
  //Call := Algorithm.Call;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).Prepare(True);
end;

procedure ApplyForm(Algorithm: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; //Call: TsmxFuncCallBack;
begin
  c := Algorithm.RootCell;
  //Call := Algorithm.Call;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).Apply;
end;

procedure SelectRecord(Algorithm: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; //Call: TsmxFuncCallBack;
begin
  c := Algorithm.RootCell;
  //Call := Algorithm.Call;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).FormModalResult := mrOk;
end;

procedure UnSelectRecord(Algorithm: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; //Call: TsmxFuncCallBack;
begin
  c := Algorithm.RootCell;
  //Call := Algorithm.Call;
  if IsCellLib(c, 'TsmxCustomForm') then
    TsmxCustomForm(c).FormModalResult := mrCancel;
end;

procedure SelectOfFormSetFilterValue(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID: Integer; ap: TsmxParams; p: TsmxParam; c, f, flt: TsmxBaseCell;
  r: TsmxCustomRequest; fld: IsmxField; //Call: TsmxFuncCallBack;
begin
  try
    FormCfgID := 0; FormID := 0;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('FormID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;
    finally
      ap.Free;
    end;
    if FormCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      flt := Algorithm.ParentCell;
      if IsCellLib(flt, 'TsmxCustomFilter') then
      begin
        f := NewCellLib(FormCfgID, FormID);
        try
          if not IsCellLib(f, 'TsmxCustomForm') then
            raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
          with TsmxCustomForm(f) do
          begin
            if IsCellLib(c, 'TsmxCustomForm') then
              ParentForm := TsmxCustomForm(c);
            if ShowModalForm = mrOk then
            begin
              r := nil;
              with TsmxCustomForm(f) do
                if Assigned(PageManagers[0]) then
                  if Assigned(PageManagers[0].ActivePage) then
                    if Assigned(PageManagers[0].ActivePage.Sections[0]) then
                      r := PageManagers[0].ActivePage.Sections[0].Request;
              //r := ActiveRequest(TsmxCustomForm(f));
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

procedure PerformRequestFromForm(Algorithm: TsmxAlgorithm; Params: Variant);
var RequestCfgID: Integer; ap: TsmxParams; p: TsmxParam; c, r: TsmxBaseCell;
  //Call: TsmxFuncCallBack;
begin
  try
    RequestCfgID := 0;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('RequestCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          RequestCfgID := p.ParamValue;
    finally
      ap.Free;
    end;
    if RequestCfgID > 0 then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      if IsCellLib(c, 'TsmxCustomForm') then
      begin
        r := NewCellLib(RequestCfgID);
        try
          if not IsCellLib(r, 'TsmxCustomRequest') then
            raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
          r.ParentCell := c;
          if PerformRequest(TsmxCustomRequest(r)) then
            TsmxCustomForm(c).Prepare(True);
        finally
          r.Free;
        end;
      end;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure SelectOfFormPerformRequest(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID, RequestCfgID: Integer; FormRefresh: Boolean;
  ap: TsmxParams; p: TsmxParam; c, f, r: TsmxBaseCell; //r2: TsmxCustomRequest;
  //i: Integer; //Call: TsmxFuncCallBack;
begin
  try
    FormCfgID := 0; FormID := 0;
    RequestCfgID := 0; FormRefresh := False;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('FormID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;
      p := ap.FindByName('RequestCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          RequestCfgID := p.ParamValue;
      p := ap.FindByName('FormRefresh');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormRefresh := p.ParamValue;
      if (FormCfgID > 0) and (RequestCfgID > 0) then
      begin
        c := Algorithm.RootCell;
        //Call := Algorithm.Call;
        f := NewCellLib(FormCfgID, FormID);
        try
          if not IsCellLib(f, 'TsmxCustomForm') then
            raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
          with TsmxCustomForm(f) do
          begin
            if IsCellLib(c, 'TsmxCustomForm') then
              ParentForm := TsmxCustomForm(c);
            if ShowModalForm = mrOk then
            begin
              {r2 := nil;
              if Assigned(PageManager) then
                if Assigned(PageManager.ActivePage) then
                  if Assigned(PageManager.ActivePage.Grid) then
                    r2 := PageManager.ActivePage.Grid.Request;}
              r := NewCellLib(RequestCfgID);
              try
                if not IsCellLib(r, 'TsmxCustomRequest') then
                  raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
                with TsmxCustomRequest(r) do
                begin
                  {for i := 0 to ap.Count - 1 do
                    if VarIsNull(ap[i].ParamValue) then
                      RequestParams[ap[i].ParamName] := '' else
                      RequestParams[ap[i].ParamName] := ap[i].ParamValue;
                  if Assigned(r2) then
                    for i := 0 to r2.CellDataSet.FieldCount - 1 do
                      if VarIsNull(r2.CellDataSet.Fields[i].Value) then
                        RequestParams[r2.CellDataSet.Fields[i].FieldName] := '' else
                        RequestParams[r2.CellDataSet.Fields[i].FieldName] :=
                          r2.CellDataSet.Fields[i].Value;}
                end;
                //PerformRequest(TsmxCustomRequest(r), True);
                if PerformRequest(TsmxCustomRequest(r), True) then
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
    finally
      ap.Free;
    end;
  except
    raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
  end;
end;

procedure SelectOfFormPerformRequestFromAlgorithm(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID, RequestCfgID: Integer; FormRefresh: Boolean;
  ap: TsmxParams; p: TsmxParam; c, f, r: TsmxBaseCell; r2: TsmxCustomRequest;
  fld: IsmxField; prm: TsmxParam; //Call: TsmxFuncCallBack;
begin
  try
    FormCfgID := 0; FormID := 0;
    RequestCfgID := 0; FormRefresh := False;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('FormID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;
      p := ap.FindByName('RequestCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          RequestCfgID := p.ParamValue;
      p := ap.FindByName('FormRefresh');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormRefresh := p.ParamValue;
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (RequestCfgID > 0) then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      f := NewCellLib(FormCfgID, FormID);
      try
        if not IsCellLib(f, 'TsmxCustomForm') then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        with TsmxCustomForm(f) do
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
              {fld := r2.FindFieldSense(fsParam);
              while Assigned(fld) do
              begin
                //if VarIsNull(fld.Value) then
                  //Algorithm.AlgorithmParams[fld.FieldName] := '' else
                  //Algorithm.AlgorithmParams[fld.FieldName] := fld.Value;
                //Algorithm.AlgorithmParams.Values[fld.FieldName] := fld.Value;
                with Algorithm.AlgorithmParams.Add do
                begin
                  ParamName := fld.FieldName;
                  ParamValue := fld.Value;
                end;
                fld := r2.FindFieldSense(fsParam, fld.FieldNo + 1);
              end;}

              prm := Algorithm.FindParamLocation(plInput);
              while Assigned(prm) do
              begin
                fld := r2.CellDataSet.FindField(prm.ParamName);
                  if Assigned(fld) then
                    prm.ParamValue := fld.Value;
                prm := Algorithm.FindParamLocation(plInput, prm.ItemIndex + 1);
              end;
            end;
            r := NewCellLib(RequestCfgID);
            try
              if not IsCellLib(r, 'TsmxCustomRequest') then
                raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
              r.ParentCell := Algorithm;
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

procedure SelectOfFormPerformRequestFromForm(Algorithm: TsmxAlgorithm; Params: Variant);
var FormCfgID, FormID, RequestCfgID: Integer; FormRefresh: Boolean;
  ap: TsmxParams; p: TsmxParam; c, f, r: TsmxBaseCell; r2: TsmxCustomRequest;
  fld: IsmxField; prm: IsmxParam; //Call: TsmxFuncCallBack;
begin
  try
    FormCfgID := 0; FormID := 0;
    RequestCfgID := 0; FormRefresh := False;
    ap := VarToParams(Params);
    try
      p := ap.FindByName('FormCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormCfgID := p.ParamValue;
      p := ap.FindByName('FormID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormID := p.ParamValue;
      p := ap.FindByName('RequestCfgID');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          RequestCfgID := p.ParamValue;
      p := ap.FindByName('FormRefresh');
      if Assigned(p) then
        if not VarIsNull(p.ParamValue) then
          FormRefresh := p.ParamValue;
    finally
      ap.Free;
    end;
    if (FormCfgID > 0) and (RequestCfgID > 0) then
    begin
      c := Algorithm.RootCell;
      //Call := Algorithm.Call;
      if IsCellLib(c, 'TsmxCustomForm') then
      begin
        f := NewCellLib(FormCfgID, FormID);
        try
          if not IsCellLib(f, 'TsmxCustomForm') then
            raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
          with TsmxCustomForm(f) do
          begin
            ParentForm := TsmxCustomForm(c);
            if ShowModalForm = mrOk then
            begin
              r2 := nil;
              if Assigned(PageManagers[0]) then
                if Assigned(PageManagers[0].ActivePage) then
                  if Assigned(PageManagers[0].ActivePage.Sections[0]) then
                    r2 := PageManagers[0].ActivePage.Sections[0].Request;
              r := NewCellLib(RequestCfgID);
              try
                if not IsCellLib(r, 'TsmxCustomRequest') then
                  raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
                with TsmxCustomRequest(r) do
                begin
                  ParentCell := c;
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

                    {fld := r2.FindFieldSense(fsParam);
                    while Assigned(fld) do
                    begin
                      prm := CellDataSet.FindParam(fld.FieldName);
                      if Assigned(prm) then
                        prm.Value := fld.Value;
                      //if VarIsNull(fld.Value) then
                        //RequestParams[fld.FieldName] := '' else
                        //RequestParams[fld.FieldName] := fld.Value;
                      fld := r2.FindFieldSense(fsParam, fld.FieldNo + 1);
                    end;}
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
