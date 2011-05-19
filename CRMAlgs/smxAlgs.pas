unit smxAlgs;

interface

uses
  SysUtils, smxClasses;

type
  { EsmxAlgorithmError }

  EsmxAlgorithmError = class(Exception);

procedure RefreshRequest(Action: TsmxAlgorithm; Params: Variant);
procedure OpenForm(Action: TsmxAlgorithm; Params: Variant);
procedure CloseForm(Action: TsmxAlgorithm; Params: Variant);
procedure RefreshForm(Action: TsmxAlgorithm; Params: Variant);
procedure OpenModalForm(Action: TsmxAlgorithm; Params: Variant);
procedure SelectRecord(Action: TsmxAlgorithm; Params: Variant);
procedure UnSelectRecord(Action: TsmxAlgorithm; Params: Variant);
//function OpenCard(Action: TsmxAlgorithm; Params: Variant): Variant;
procedure ExecuteAlgorithm(Action: TsmxAlgorithm; Params: Variant);
procedure SelectOfFormSetFilterValue(Action: TsmxAlgorithm; Params: Variant);
procedure PerformRequestFromForm(Action: TsmxAlgorithm; Params: Variant);
procedure SelectOfFormPerformRequest(Action: TsmxAlgorithm; Params: Variant);

implementation

uses
  Windows, Controls, Variants, smxCells, smxParams, smxFuncs,
  smxDLLFuncs, smxConsts, smxDBIntf, smxTypes, smxDLLTypes;

procedure RefreshRequest(Action: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; r: TsmxCustomRequest;
begin
  r := nil;
  c := Action.RootCell;
  if TsmxFuncIsCell(Integer(Action.Call(111)))(c, 'TsmxCustomForm') then
  begin
    with TsmxCustomForm(c) do
      if Assigned(PageManager) then
        if Assigned(PageManager.ActivePage) then
          if Assigned(PageManager.ActivePage.Grid) then
            r := PageManager.ActivePage.Grid.Request;
    if Assigned(r) then
      r.Perform;
  end;
end;

procedure OpenForm(Action: TsmxAlgorithm; Params: Variant);
var FormID: Integer; ap: TsmxParams; c, f: TsmxBaseCell;
  Call: TsmxFuncCallBack; i: Integer;
begin
  FormID := 0;
  ap := VarToParams(Params);
  try
    if ap.Count > 0 then
      FormID := ap[0].ParamValue;
    if FormID > 0 then
    begin
      c := Action.RootCell;
      Call := Action.Call;
      f := TsmxFormManager(Integer(Call(2))).FindByCfgID(FormID);
      if Assigned(f) then
      begin
        TsmxCustomForm(f).ShowForm;
      end else
      begin
        f := CreateCell(FormID, Call);
        if not ClassCell(f, 'TsmxCustomForm', Call) then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        with TsmxCustomForm(f) do
        begin
          if ClassCell(c, 'TsmxCustomForm', Call) then
            ParentForm := TsmxCustomForm(c);
          for i := 1 to ap.Count - 1 do
            FormParams[ap[i].ParamName] := ap[i].ParamValue;
          ShowForm;
        end;
      end;
    end;
  finally
    ap.Free;
  end;
end;

procedure CloseForm(Action: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; Call: TsmxFuncCallBack;
begin
  c := Action.RootCell;
  Call := Action.Call;
  if ClassCell(c, 'TsmxCustomForm', Call) then
    c.Free;
end;

procedure RefreshForm(Action: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; Call: TsmxFuncCallBack;
begin
  c := Action.RootCell;
  Call := Action.Call;
  if ClassCell(c, 'TsmxCustomForm', Call) then
    TsmxCustomForm(c).Prepare(True);
end;

procedure OpenModalForm(Action: TsmxAlgorithm; Params: Variant);
var ID: Integer; f: TsmxCustomForm; r: TsmxCustomRequest; c: TsmxBaseCell;
  apIn: TsmxParams; i: Integer; fld: IsmxField; flt: TsmxBaseCell;
begin
  //MessageBox(0, 'OpenForm', nil, 0);

  //f := TsmxStandardForm(NewCell(nil, 7, Action.Call));
  //f := TsmxStandardForm(tmp(Integer(Action.Call(101)))(nil, 7, Action.Call));
  //Result := Unassigned;
  ID := 0;
  apIn := VarToParams(Params);
  try
    if apIn.Count > 0 then
      ID := apIn[0].ParamValue;
    if ID > 0 then
    begin
      //f := TsmxCustomForm(NewCell(nil, ID, Action.Call));
      f := TsmxCustomForm(TsmxFuncNewCell(Integer(Action.Call(101)))(nil, ID, Action.Call));
      c := Action.RootCell;
      if TsmxFuncIsCell(Integer(Action.Call(111)))(c, 'TsmxCustomForm') then
        f.ParentForm := TsmxCustomForm(c);
      try
        for i := 1 to apIn.Count - 1 do
          f.FormParams[apIn[i].ParamName] := apIn[i].ParamValue;
        //f.ItemLeft := 300;
        //f.ItemTop := 300;
        //f.ItemWidth := 300;
        //f.ItemHeight := 400;
        if f.ShowModalForm = mrOk then
        begin
          r := nil;
          if Assigned(f.PageManager) then
            if Assigned(f.PageManager.ActivePage) then
              if Assigned(f.PageManager.ActivePage.Grid) then
                r := f.PageManager.ActivePage.Grid.Request;
          if Assigned(r) then
          begin
            {apOut := TsmxParams.Create(TsmxParam);
            try
              with apOut.Add do
              begin
                ParamName := 'Key';
                ParamValue := r.FindResultValue(fsKey);
              end;
              with apOut.Add do
              begin
                ParamName := 'Value';
                ParamValue := r.FindResultValue(fsValue);
              end;
              Result := ParamsToVar(apOut);
            finally
              apOut.Free;
            end;}
            fld := r.FindFieldSense(fsKey);
            if Assigned(fld) then
            begin
              //Result := fld.Value;
              flt := Action.ParentCell;
              if TsmxFuncIsCell(Integer(Action.Call(111)))(flt, 'TsmxCustomFilter') then
                TsmxCustomFilter(flt).FilterValue := fld.Value;
            end;
          end;
        end;
      finally
        f.Free;
      end;
    end;
  finally
    apIn.Free;
  end;

  {Result := Unassigned;
  ID := 0; t := 0;
  if VarIsArray(Params) then
    if VarArrayHighBound(Params, 1) = 1 then
    begin
      ID := Params[0];
      t := Params[1];
    end;
  if ID > 0 then
  begin                   //Application.Handle := Action.Call(0);
    //f := TsmxStandardForm.Create(nil, 7, Action.Call);
    //f := TsmxStandardForm(NewCell(nil, 7, Action.Call));
    //f := TsmxStandardForm.Create(nil, 7, Action.Call);
    //f := TsmxStandardForm(NewCell(nil, ID, Action.Call));
    
    f := TsmxCustomForm(NewCell(nil, ID, Action.Call));
    try
      f.FormParams['@DirType'] := IntToStr(t);
      f.ItemLeft := 300;
      f.ItemTop := 300;
      f.ItemWidth := 300;
      f.ItemHeight := 400;
      if f.ShowModalForm = mrOk then
      begin
        r := nil;
        if Assigned(f.PageManager) then
          if Assigned(f.PageManager.ActivePage) then
            r := f.PageManager.ActivePage.Request;
        if Assigned(r) then
        begin
          v := VarArrayCreate([0, 1], varVariant);
          v[0] := r.FindResultValue(fsKey);
          v[1] := r.FindResultValue(fsValue);
          Result := v;
          //Result := r.FindResultValue(fsKey);
        end;
      end;
    finally
      f.Free;
    end;
  end;}
end;

procedure SelectRecord(Action: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; Call: TsmxFuncCallBack;
begin
  c := Action.RootCell;
  Call := Action.Call;
  if ClassCell(c, 'TsmxCustomForm', Call) then
    TsmxCustomForm(c).FormModalResult := mrOk;
end;

procedure UnSelectRecord(Action: TsmxAlgorithm; Params: Variant);
var c: TsmxBaseCell; Call: TsmxFuncCallBack;
begin
  c := Action.RootCell;
  Call := Action.Call;
  if ClassCell(c, 'TsmxCustomForm', Call) then
    TsmxCustomForm(c).FormModalResult := mrCancel;
end;

{procedure OpenCard(Action: TsmxAlgorithm; Params: Variant);
var ID: Integer; f: TsmxCustomForm; ap: TsmxParams; h: HWND;
  fp: TsmxCustomFilterPanel; r: TsmxCustomRequest; i: Integer;
begin
  Result := Unassigned;
  ID := 0;
  ap := VarToParams(Params);
  try
    if ap.Count > 0 then
      ID := ap[0].ParamValue;
  finally
    ap.Free;
  end;
  if ID > 0 then
  begin
    f := TsmxCustomForm(TsmxCustomForm(Action.RootCell).FormManager.FindByID(ID));
    if Assigned(f) then
    begin
      h := TsmxCustomForm(Action.RootCell).FormManager.HandleOfForm(f);
      BringWindowToTop(h);
      SetActiveWindow(h);
    end else
    begin
      f := TsmxCustomForm(TsmxFuncNewCell(Integer(Action.Call(101)))(nil, ID, Action.Call));
      //f.ItemLeft := 250;
      //f.ItemTop := 250;
      //f.ItemWidth := 400;
      //f.ItemHeight := 250;
      f.ParentForm := TsmxCustomForm(Action.RootCell);
      f.ShowForm;
      fp := nil; r := nil;
      if Assigned(f.PageManager) then
        if Assigned(f.PageManager.ActivePage) then
          if Assigned(f.PageManager.ActivePage.Grid) then
          begin
            r := f.PageManager.ActivePage.Grid.Request;
            fp := f.PageManager.ActivePage.FilterPanel;
          end;
      if Assigned(r) and Assigned(fp) then
        for i := 0 to fp.FilterCount - 1 do
        begin
          //
        end;
    end
  end;
end;}

procedure ExecuteAlgorithm(Action: TsmxAlgorithm; Params: Variant);
var ID: Integer; ap: TsmxParams; c: TsmxBaseCell; r: TsmxCustomRequest;
  res, msg: Variant; f: IsmxField;
begin
  //Result := Unassigned;
  ID := 0;
  ap := VarToParams(Params);
  try
    if ap.Count > 0 then
      ID := ap[0].ParamValue;
    if ID > 0 then
    begin
      r := TsmxCustomRequest(TsmxFuncNewCell(Integer(Action.Call(101)))(nil, ID, Action.Call));
      c := Action.RootCell;
      if TsmxFuncIsCell(Integer(Action.Call(111)))(c, 'TsmxCustomForm') then
        if Assigned(TsmxCustomForm(c).PageManager) then
          r.ParentCell := TsmxCustomForm(c).PageManager.ActivePage;
      try
        if not(r.Database.InTransaction) then
          r.Database.StartTrans;
        try
          r.Perform;
          case r.CellDataSet.DataSetType of
            dstQuery:
            begin
              f := r.FindFieldSense(fsResult);
              if Assigned(f) then
                res := f.Value;
              f := r.FindFieldSense(fsMsg);
              if Assigned(f) then
                msg := f.Value;
              //res := r.FindResultValue(fsResult);
              //msg := r.FindResultValue(fsMsg);
            end;
            dstStoredProc:
            begin
              res := r.RequestParams['@Return_value'];
              msg := r.RequestParams['@Msg'];
            end;
          end;
          if StrToIntDef(res, 1) = 0 then
            r.Database.CommitTrans else
            r.Database.RollbackTrans;
          if StrToIntDef(res, 1) = 0 then
            if TsmxFuncIsCell(Integer(Action.Call(111)))(c, 'TsmxCustomForm') then
              TsmxCustomForm(c).Prepare(True);
          if msg <> '' then
          begin
            //MessageBox(0, PChar(String(msg)), nil, 0);
            //Application.Handle := HWND(Integer(Action.Call(0)));
            //Inf(msg);
            TsmxFuncInf(Integer(Action.Call(151)))(msg);
          end;
        except
          r.Database.RollbackTrans;
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        end;
      finally
        r.ParentCell := nil;
        r.Free;
      end;
    end;
  finally
    ap.Free;
  end;
end;

procedure SelectOfFormSetFilterValue(Action: TsmxAlgorithm; Params: Variant);
var FormID: Integer; ap: TsmxParams; c, f, p: TsmxBaseCell; r: TsmxCustomRequest;
  Call: TsmxFuncCallBack; i: Integer; fld: IsmxField;
begin
  FormID := 0;
  ap := VarToParams(Params);
  try
    if ap.Count > 0 then
      FormID := ap[0].ParamValue;
    if FormID > 0 then
    begin
      c := Action.RootCell;
      Call := Action.Call;
      p := Action.ParentCell;
      if not ClassCell(p, 'TsmxCustomFilter', Call) then
        raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
      f := CreateCell(FormID, Call);
      try
        if not ClassCell(f, 'TsmxCustomForm', Call) then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        with TsmxCustomForm(f) do
        begin
          if ClassCell(c, 'TsmxCustomForm', Call) then
            ParentForm := TsmxCustomForm(c);
          for i := 1 to ap.Count - 1 do
            FormParams[ap[i].ParamName] := ap[i].ParamValue;
          if ShowModalForm = mrOk then
          begin
            r := nil;
            with TsmxCustomForm(f) do
              if Assigned(PageManager) then
                if Assigned(PageManager.ActivePage) then
                  if Assigned(PageManager.ActivePage.Grid) then
                    r := PageManager.ActivePage.Grid.Request;
            if Assigned(r) then
            begin
              fld := r.FindFieldSense(fsKey);
              if Assigned(fld) then
                TsmxCustomFilter(p).FilterValue := fld.Value;
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
end;

procedure PerformRequestFromForm(Action: TsmxAlgorithm; Params: Variant);
var RequestID: Integer; ap: TsmxParams; c, r: TsmxBaseCell;
  Call: TsmxFuncCallBack; res: Boolean;
begin
  RequestID := 0;
  ap := VarToParams(Params);
  try
    if ap.Count > 0 then
      RequestID := ap[0].ParamValue;
    if RequestID > 0 then
    begin
      c := Action.RootCell;
      Call := Action.Call;
      r := CreateCell(RequestID, Call);
      try
        if not ClassCell(r, 'TsmxCustomRequest', Call) then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        if not ClassCell(c, 'TsmxCustomForm', Call) then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        r.ParentCell := c;
        res := PerformRequest(TsmxCustomRequest(r));
        if not res then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        TsmxCustomForm(c).Prepare(True);
      finally
        r.Free;
      end;
    end;
  finally
    ap.Free;
  end;
end;

procedure SelectOfFormPerformRequest(Action: TsmxAlgorithm; Params: Variant);
var FormID, RequestID: Integer; ap: TsmxParams; c, f, r: TsmxBaseCell;
  Call: TsmxFuncCallBack; i: Integer; res: Boolean;
begin
  FormID := 0; RequestID := 0;
  ap := VarToParams(Params);
  try
    if ap.Count > 1 then
    begin
      FormID := ap[0].ParamValue;
      RequestID := ap[1].ParamValue;
    end;
    if (FormID > 0) and (RequestID > 0) then
    begin
      c := Action.RootCell;
      Call := Action.Call;
      f := CreateCell(FormID, Call);
      try
        if not ClassCell(f, 'TsmxCustomForm', Call) then
          raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
        with TsmxCustomForm(f) do
        begin
          if ClassCell(c, 'TsmxCustomForm', Call) then
            ParentForm := TsmxCustomForm(c);
          for i := 2 to ap.Count - 1 do
            FormParams[ap[i].ParamName] := ap[i].ParamValue;
          if ShowModalForm = mrOk then
          begin
            r := CreateCell(RequestID, Call);
            try
              if not ClassCell(r, 'TsmxCustomRequest', Call) then
                raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
              r.ParentCell := f;
              res := PerformRequest(TsmxCustomRequest(r));
              if not res then
                raise EsmxAlgorithmError.CreateRes(@SAlgExecuteError);
              if ClassCell(c, 'TsmxCustomForm', Call) then
                TsmxCustomForm(c).Prepare(True);
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
end;

end.
