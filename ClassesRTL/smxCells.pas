{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Cells classes             }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxCells;

interface

uses
  Classes, {Controls, {ComCtrls, {DB, {DBGrids, {Forms,{ ExtCtrls, {StdCtrls{, Menus,}
  ActnList, {Windows,} ImgList, {Graphics, {smxBaseClasses,} smxClasses, {smxCfgs,}
  {smxStdCtrls{, smxDBIntf,} smxTypes{, smxClassTypes{, smxManagerIntf{, smxBaseTypes};

type
  { TsmxAction }

  TsmxAction = class(TsmxCustomAlgorithm)
  private
    FAction: TAction;
    function GetAction: TAction;
    procedure ActionExecute(Sender: TObject);
    //function GetParamValue(const ParamName: String): Variant;
  protected
    //procedure DoRefreshParams; override;
    function GetAlgorithmCaption: String; override;
    function GetAlgorithmEnabled: Boolean; override;
    function GetAlgorithmHint: String; override;
    function GetAlgorithmHotKey: Integer; override;
    function GetAlgorithmImageIndex: Integer; override;
    function GetAlgorithmVisible: Boolean; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalRef: Pointer; override;
    //function GetProcPointer: Pointer; virtual;
    //procedure InitializeEvent; virtual;
    //procedure InternalInitialize; override;
    procedure InternalRefreshParams; override;
    //procedure ResetCellProps; override;
    procedure SetAlgorithmCaption(const Value: String); override;
    procedure SetAlgorithmEnabled(Value: Boolean); override;
    procedure SetAlgorithmHint(const Value: String); override;
    procedure SetAlgorithmHotKey(Value: Integer); override;
    procedure SetAlgorithmImageIndex(Value: Integer); override;
    procedure SetAlgorithmVisible(Value: Boolean); override;
    //procedure ProcExec(Sender: TObject); virtual;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    //procedure SetLibraryManager(Value: TsmxCustomLibraryManager); override;
    procedure ChangeSlaveIndex(Value: Integer); override;

    property Action: TAction read GetAction;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //procedure Execute(Same: Boolean = False); override;
    //procedure RefreshParams; override;
    // function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam; override;
  published
    property AlgorithmCaption;
    property AlgorithmEnabled;
    property AlgorithmHint;
    property AlgorithmHotKey;
    property AlgorithmImageIndex;
    property AlgorithmParams;
    property AlgorithmVisible;

    property OnRefreshParams;
  end;

  { TsmxActionList }

  TsmxActionList = class(TsmxCustomAlgorithmList)
  private
    FActionList: TActionList;
    function GetActionList: TActionList;
  protected
    //function GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalInitialize; override;
    procedure SetImageList(Value: TCustomImageList); override;
    //procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;
    //procedure SetParentCell(Value: TsmxBaseCell); override;

    property ActionList: TActionList read GetActionList;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //function GetAlgorithmParamValue(CfgID: Integer; const ParamName: String;
    //  var ParamValue: Variant): Boolean; override;
    //procedure Initialize(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
    //  ASelectRequest: TsmxCustomRequest = nil); override;

    //property IsAltSlaveClass default True;
    //property IsOwnerIsParent default True;
  published
    property ImageListName;
    property SlaveList;
  end;

  { TsmxRequest }

  TsmxRequest = class(TsmxCustomRequest)
  //private
    //function GetParamValue(const ParamName: String): Variant;
  protected
    //procedure DoDelete; override;
    //procedure DoExecute; override;
    //procedure DoInsert; override;
    //procedure DoPrepare; override;
    //procedure DoRefreshParams; override;
    //procedure DoUpdate; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    //function GetDataSet: IsmxDataSet; virtual;
    //procedure InitializeDataSet; virtual;
    //procedure InternalInitialize; override;
    procedure InternalRefreshParams; override;
    //procedure ResetCellProps; override;
    //procedure SetCellProps; override;
  published
    property DatabaseName;
    property DataSet;
    property DeleteDataSet;
    property InsertDataSet;
    property OperationMode;
    property UpdateDataSet;

    property OnDelete;
    property OnExecute;
    property OnInsert;
    property OnPrepare;
    property OnRefreshParams;
    property OnUpdate;
  end;

  { TsmxRequestList }

  TsmxRequestList = class(TsmxCustomRequestList)
  protected
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;
  public
    //constructor Create(AOwner: TComponent); override;
    //function GetRequestParamValue(CfgID: Integer; const ParamName: String;
    //  var ParamValue: Variant): Boolean; override;

    //property IsAltSlaveClass default True;
    //property IsOwnerIsParent default True;
  published
    property SlaveList;
  end;

implementation

uses
  {SysUtils,} {Variants,} {ToolWin,} {Messages,} {smxCommonStorage, smxLibManager,
  smxDBManager, smxFormManager, smxGlobalVariables, smxDBConnection,}{ smxFuncs,}
  {smxDBFuncs,} smxProcs, smxClassFuncs, {smxLibFuncs,} {smxConsts,}
  smxClassProcs, smxBaseIntf;

{ TsmxAction }

{constructor TsmxAction.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //@FLibProc := LibManager.GetProcedure(Cfg.AlgLibrary, Cfg.AlgProcedure);
  //@FLibProc := FindProcedureByNameLib(Cfg.AlgLibrary, Cfg.AlgProcedure);
  //AddParams;
end;}

destructor TsmxAction.Destroy;
begin
  inherited Destroy;
  if Assigned(FAction) then
    FAction.Free;
end;

procedure TsmxAction.ActionExecute(Sender: TObject);
begin
  Execute;
end;

(*procedure TsmxAction.DoRefreshParams;
//var
  //AlgCfgID: Integer;
begin
  {if Assigned(OnRefreshParams) then
  begin
    if Cfg is TsmxAlgorithmCfg then
      AlgCfgID := TsmxAlgorithmCfg(Cfg).RefreshParamsCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefreshParams, AlgCfgID);
  end;}
end;*)

function TsmxAction.GetAction: TAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := TAction.Create({Self} nil); // nil as Self destroy inner object by execute free
    FAction.OnExecute := ActionExecute;
  end;
  Result := FAction;
end;

function TsmxAction.GetAlgorithmCaption: String;
begin
  Result := Action.Caption;
end;

procedure TsmxAction.SetAlgorithmCaption(const Value: String);
begin
  Action.Caption := Value;
end;

function TsmxAction.GetAlgorithmEnabled: Boolean;
begin
  Result := Action.Enabled;
end;

procedure TsmxAction.SetAlgorithmEnabled(Value: Boolean);
begin
  Action.Enabled := Value;
end;

function TsmxAction.GetAlgorithmHint: String;
begin
  Result := Action.Hint;
end;

procedure TsmxAction.SetAlgorithmHint(const Value: String);
begin
  Action.Hint := Value;
end;

function TsmxAction.GetAlgorithmHotKey: Integer;
begin
  Result := Integer(Action.ShortCut);
end;

procedure TsmxAction.SetAlgorithmHotKey(Value: Integer);
begin
  Action.ShortCut := TShortCut(Value);
end;

function TsmxAction.GetAlgorithmImageIndex: Integer;
begin
  Result := Integer(Action.ImageIndex);
end;

procedure TsmxAction.SetAlgorithmImageIndex(Value: Integer);
begin
  Action.ImageIndex := TImageIndex(Value);
end;

function TsmxAction.GetAlgorithmVisible: Boolean;
begin
  Result := Action.Visible;
end;

procedure TsmxAction.SetAlgorithmVisible(Value: Boolean);
begin
  Action.Visible := Value;
end;

{function TsmxAction.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxAlgorithmCfg;
end;}

function TsmxAction.GetInternalRef: Pointer;
begin
  Result := Pointer(Action);
end;

{function TsmxAction.GetParamValue(const ParamName: String): Variant;
var
  Param: TsmxParamKitItem;
begin
  Result := Variants.Null;
  if Cfg is TsmxAlgorithmCfg then
  begin
    Param := TsmxAlgorithmCfg(Cfg).AlgorithmParams.FindByName(ParamName);
    if Assigned(Param) then
      Result := Param.ParamValue;
  end;
end;}

{function TsmxAction.GetProcPointer: Pointer;
begin
  Result := nil;
end;}

{procedure TsmxAction.InitializeEvent;
var
  //Proc: Pointer;
  Method: TMethod;
begin
  //Proc := GetProcudure;
  if Assigned(ProcPointer) then
  begin
    Method.Code := ProcPointer;
    Method.Data := Self;
    OnExecute := TsmxComponentEvent(Method);
  end;
end;}

{procedure TsmxAction.InternalInitialize;
var
  i: Integer;
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxAlgorithmCfg then
  begin
    AlgorithmCaption := TsmxAlgorithmCfg(Cfg).AlgorithmCaption;
    AlgorithmEnabled := TsmxAlgorithmCfg(Cfg).AlgorithmEnabled;
    AlgorithmHint := TsmxAlgorithmCfg(Cfg).AlgorithmHint;
    AlgorithmHotKey := TsmxAlgorithmCfg(Cfg).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxAlgorithmCfg(Cfg).AlgorithmImageIndex;
    AlgorithmVisible := TsmxAlgorithmCfg(Cfg).AlgorithmVisible;
    for i := 0 to TsmxAlgorithmCfg(Cfg).AlgorithmParams.Count - 1 do
      with AlgorithmParams.Add do
      begin
        DataType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].DataType;
        ParamLocation := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamLocation;
        ParamType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamType;
        ParamName := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamName;
        ParamValue := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].Value;
      end;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxAlgorithmCfg(Cfg).RefreshParamsCfgID);
  end;
end;}

procedure TsmxAction.InternalRefreshParams;
var
  i, j: Integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
  Cell: TsmxOwnerCell;
begin
  //inherited InternalRefreshParams;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to AlgorithmParams.Count - 1 do
    begin
      //Value := Variants.Null;
      case AlgorithmParams[i].DataLocation of
        {plConst .. plOutput, plFilterDesk .. plParentGrid:
        begin
          Value := Variants.Null; //AlgorithmParams[i].ParamValue;
        end;}
        {plConst .. plInOutput:
        begin
          AlgorithmParams[i].ParamValue := GetParamValue(AlgorithmParams[i].ParamName);
          if Assigned(CellOwner) then
            if CellOwner.GetAlgorithmParamValue(CfgID, AlgorithmParams[i].ParamName, Value) then
              AlgorithmParams[i].ParamValue := Value;
        end;}
        dlFilterDesk:
        begin
          if CellAction is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellAction).FindSlaveByName(AlgorithmParams[i].ParamName);
            if Assigned(Cell) then
              AlgorithmParams[i].ParamValue := TsmxCustomFilter(Cell).FilterValue;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, AlgorithmParams[i].ParamName, Value) then
            AlgorithmParams[i].ParamValue := Value;
        end;
        dlGrid:
        begin
          if CellAction is TsmxCustomGrid then
          begin
            Cell := TsmxCustomGrid(CellAction).FindSlaveByName(AlgorithmParams[i].ParamName);
            if Assigned(Cell) then
              AlgorithmParams[i].ParamValue := TsmxCustomColumn(Cell).ColumnValue;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, AlgorithmParams[i].ParamName, Value) then
            AlgorithmParams[i].ParamValue := Value;
        end;
        dlParentFilterDesk:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
        dlParentGrid:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            AlgorithmParams[i].ParamValue := smxProcs.gStorageManagerIntf.Values[AlgorithmParams[i].ParamName];
        end;
        dlParentCellParam:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
      end;
      //AlgorithmParams[i].ParamValue := Value;
    end;
  finally
    List.Free;
  end;
end;

{procedure TsmxAction.ResetCellProps;
begin
  inherited ResetCellProps;
  AlgorithmCaption := '';
  AlgorithmEnabled := False;
  AlgorithmHint := '';
  AlgorithmHotKey := 0;
  AlgorithmImageIndex := -1;
  AlgorithmParams.Clear;
  AlgorithmVisible := False;
  OnExecute := nil;
  OnRefreshParams := nil;
end;}

procedure TsmxAction.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Action.ActionList := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject({_TsmxBaseCell}(CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TCustomActionList then
      Action.ActionList := TCustomActionList(Obj);
  end;
end;

(*procedure TsmxAction.SetCellProps;
var
  i: Integer;
  Form: TsmxCustomForm;
  //Method: TMethod;
begin
  inherited SetCellProps;
  if Cfg is TsmxAlgorithmCfg then
  begin
    AlgorithmCaption := TsmxAlgorithmCfg(Cfg).AlgorithmCaption;
    AlgorithmEnabled := TsmxAlgorithmCfg(Cfg).AlgorithmEnabled;
    AlgorithmHint := TsmxAlgorithmCfg(Cfg).AlgorithmHint;
    AlgorithmHotKey := TsmxAlgorithmCfg(Cfg).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxAlgorithmCfg(Cfg).AlgorithmImageIndex;
    AlgorithmVisible := TsmxAlgorithmCfg(Cfg).AlgorithmVisible;
    for i := 0 to TsmxAlgorithmCfg(Cfg).AlgorithmParams.Count - 1 do
      with AlgorithmParams.Add do
      begin
        DataType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].DataType;
        ParamLocation := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamLocation;
        ParamType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamType;
        ParamName := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamName;
        ParamValue := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamValue;
      end;
    {if Assigned(GetProcPointer()) then
    begin
      Method.Code := GetProcPointer;
      Method.Data := Self;
      OnExecute := TsmxComponentEvent(Method);
    end;}
    OnExecute := AlgorithmExecute;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxAlgorithmCfg(Cfg).RefreshParamsCfgID);
  end;
end;*)

{procedure TsmxAction.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  if Assigned(LibraryManager) then
    FLibProc := nil;
  inherited SetLibraryManager(Value);
  if Assigned(LibraryManager) then
    @FLibProc := LibraryManager.GetProcedure(Cfg.AlgLibrary, Cfg.AlgProcedure);
end;}

procedure TsmxAction.ChangeSlaveIndex(Value: Integer);
begin
  //inherited SetSlaveIndex(Value);
  Action.Index := Value;
end;

{ TsmxActionList }

{constructor TsmxActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;}

destructor TsmxActionList.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FActionList) then
    FActionList.Free;
end;

function TsmxActionList.GetActionList: TActionList;
begin
  if not Assigned(FActionList) then
    FActionList := TActionList.Create(nil);
  Result := FActionList;
end;

(*function TsmxActionList.GetAlgorithmParamValue(CfgID: Integer;
  const ParamName: String; var ParamValue: Variant): Boolean;
var
  //i: Integer;
  Param: TsmxParamKitItem;
  Item: TsmxOwnerKitItem;
begin
  Result := inherited GetAlgorithmParamValue(CfgID, ParamName, ParamValue);
  if Cfg is TsmxAlgorithmListCfg then
  begin
    Item := TsmxAlgorithmListCfg(Cfg).SlaveCells.FindByCfgID(CfgID);
    if Item is TsmxAlgorithmKitItem then
    begin
      Param := TsmxAlgorithmKitItem(Item).ItemParams.FindByName(ParamName);
      if Assigned(Param) then
      begin
        ParamValue := Param.ParamValue;
        Result := True;
      end;
    end;
  end;

    {if Algorithm.SlaveIndex < TsmxAlgorithmListCfg(Cfg).SlaveCells.Count then
      for i := 0 to Algorithm.AlgorithmParams.Count - 1 do
        Algorithm.AlgorithmParams[i].ParamValue :=
          TsmxAlgorithmListCfg(Cfg).SlaveCells[Algorithm.SlaveIndex].ItemParams[i].ParamValue;}

  {for i := 0 to TsmxAlgorithmKitItem(Item).ItemParams.Count - 1 do
  begin
    Param := TsmxCustomAlgorithm(Slave).AlgorithmParams.FindByName(TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamName);
    if Assigned(Param) then
      Param.ParamValue := TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamValue;
  end;}
end;*)

{function TsmxActionList.GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass;
begin
  if Cfg is TsmxAlgorithmListCfg then
    Result := TsmxOwnerCellClass(smxClassFuncs.CfgIDToCellClass(
      TsmxAlgorithmListCfg(Cfg).SlaveCells[Index].CfgID, SelectRequest))
  else
    Result := inherited GetAltSlaveClass(Index);
end;}

{function TsmxActionList.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxAlgorithmListCfg;
end;}

function TsmxActionList.GetInternalRef: Pointer;
begin
  Result := Pointer(ActionList);
end;

function TsmxActionList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxAction;
end;

procedure TsmxActionList.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    ActionList.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    ActionList.Images := ImageList;
end;

{procedure TsmxActionList.SetParentCell(Value: TsmxBaseCell);
begin
  inherited SetParentCell(Value);
  FActionList.Images := ImageList;
end;}

(*procedure TsmxActionList.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
var
  i: Integer;
  Param: TsmxParamKitItem;
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxCustomAlgorithm) and (Item is TsmxAlgorithmKitItem) then
  begin
    TsmxCustomAlgorithm(Slave).AlgorithmEnabled := TsmxAlgorithmKitItem(Item).ItemEnabled;
    TsmxCustomAlgorithm(Slave).AlgorithmVisible := TsmxAlgorithmKitItem(Item).ItemVisible;
    for i := 0 to TsmxCustomAlgorithm(Slave).AlgorithmParams.Count - 1 do
    begin
      Param := TsmxAlgorithmKitItem(Item).ItemParams.FindByName(TsmxCustomAlgorithm(Slave).AlgorithmParams[i].ParamName);
      if Assigned(Param) then
        TsmxCustomAlgorithm(Slave).AlgorithmParams[i].ParamValue := Param.ParamValue;
    end;

    {for i := 0 to TsmxAlgorithmKitItem(Item).ItemParams.Count - 1 do
    begin
      Param := TsmxCustomAlgorithm(Slave).AlgorithmParams.FindByName(TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamName);
      if Assigned(Param) then
        Param.ParamValue := TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamValue;
    end;}
  end;
end;*)

{ TsmxRequest }

{procedure TsmxRequest.DoDelete;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnDelete) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).DeleteAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnDelete, AlgCfgID);
  end;
end;}

{procedure TsmxRequest.DoExecute;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnExecute) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).ExecuteAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnExecute, AlgCfgID);
  end;
end;}

{procedure TsmxRequest.DoInsert;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnInsert) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).InsertAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnInsert, AlgCfgID);
  end;
end;}

{procedure TsmxRequest.DoPrepare;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnPrepare) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).PrepareAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnPrepare, AlgCfgID);
  end;
end;}

{procedure TsmxRequest.DoRefreshParams;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnRefreshParams) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).RefreshParamsAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefreshParams, AlgCfgID);
  end;
end;}

{procedure TsmxRequest.DoUpdate;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnUpdate) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).UpdateAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnUpdate, AlgCfgID);
  end;
end;}

{function TsmxRequest.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxRequestCfg;
end;}

{function TsmxRequest.GetDataSet: IsmxDataSet;
begin
  Result := nil;
end;}

(*procedure TsmxRequest.InitializeDataSet;
var
  i: Integer;
  //DataSetIntf: IsmxDataSet;
begin
  //DataSetIntf := GetDataSet;
  if Assigned(DataSet{Intf}) {and Assigned(Database)} then
  begin
    //DataSetIntf.Database := Database;
    if Cfg is TsmxRequestCfg then
    begin
      DataSet{Intf}.SQL.Text := TsmxRequestCfg(Cfg).SQLText;
      DataSet{Intf}.ClearFields;
      for i := 0 to TsmxRequestCfg(Cfg).Fields.Count - 1 do
        with DataSet{Intf}.AddField(TsmxRequestCfg(Cfg).Fields[i].DataType) do
        begin
          FieldName := TsmxRequestCfg(Cfg).Fields[i].FieldName;
          DisplayFormat := TsmxRequestCfg(Cfg).Fields[i].DisplayFormat;
          FieldSense := TsmxRequestCfg(Cfg).Fields[i].FieldSense;
          Precision := TsmxRequestCfg(Cfg).Fields[i].Precision;
          Size := TsmxRequestCfg(Cfg).Fields[i].Size;
        end;
      DataSet{Intf}.ClearParams;
      for i := 0 to TsmxRequestCfg(Cfg).Params.Count - 1 do
        with DataSet{Intf}.AddParam(TsmxRequestCfg(Cfg).Params[i].DataType) do
        begin
          ParamName := TsmxRequestCfg(Cfg).Params[i].ParamName;
          ParamLocation := TsmxRequestCfg(Cfg).Params[i].ParamLocation;
          ParamType := TsmxRequestCfg(Cfg).Params[i].ParamType;
          Value := TsmxRequestCfg(Cfg).Params[i].Value;
          NumericScale := TsmxRequestCfg(Cfg).Params[i].NumericScale;
          Precision := TsmxRequestCfg(Cfg).Params[i].Precision;
          Size := TsmxRequestCfg(Cfg).Params[i].Size;
        end;
    end;
    //DataSet := DataSetIntf;
  end;
end;*)

(*procedure TsmxRequest.InternalInitialize;
var
  Form: TsmxCustomForm;
  //DataSetIntf: IsmxDataSet;
  //i: Integer;
begin
  inherited InternalInitialize;
  if Cfg is TsmxRequestCfg then
  begin
    DatabaseName := TsmxRequestCfg(Cfg).DatabaseName;
    OperationMode := TsmxRequestCfg(Cfg).OperationMode;
    PerformanceMode := TsmxRequestCfg(Cfg).PerformanceMode;
    ModifyPerformances[mrDelete] := TsmxRequestCfg(Cfg).DeletePerformance;
    ModifyPerformances[mrInsert] := TsmxRequestCfg(Cfg).InsertPerformance;
    ModifyPerformances[mrUpdate] := TsmxRequestCfg(Cfg).UpdatePerformance;

    //DataSetIntf := smxClassFuncs.NewIntf(CfgID, SelectRequest) as IsmxDataSet;
    {DataSetIntf := GetDataSet;
    if Assigned(DataSetIntf) then
    begin
      DataSetIntf.SQL.Text := TsmxRequestCfg(Cfg).SQLText;
      for i := 0 to TsmxRequestCfg(Cfg).Fields.Count - 1 do
        with DataSetIntf.AddField(TsmxRequestCfg(Cfg).Fields[i].FieldName) do
        begin
          DisplayFormat := TsmxRequestCfg(Cfg).Fields[i].DisplayFormat;
          FieldSense := TsmxRequestCfg(Cfg).Fields[i].FieldSense;
        end;
      for i := 0 to TsmxRequestCfg(Cfg).Params.Count - 1 do
        with DataSetIntf.AddParam(TsmxRequestCfg(Cfg).Params[i].ParamName) do
        begin
          DataType := TsmxRequestCfg(Cfg).Params[i].DataType;
          ParamLocation := TsmxRequestCfg(Cfg).Params[i].ParamLocation;
          ParamType := TsmxRequestCfg(Cfg).Params[i].ParamType;
          Value := TsmxRequestCfg(Cfg).Params[i].Value;
        end;
      DataSet := DataSetIntf;
    end;}
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      ModifyRequests[mrDelete] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).DeleteReqCfgID);
      ModifyRequests[mrInsert] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).InsertReqCfgID);
      ModifyRequests[mrUpdate] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).UpdateReqCfgID);

      OnDelete := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnExecute := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnInsert := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnUpdate := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
    end;
  end; //else
    //raise EsmxCellError.CreateFmt(@rsCellActionError, [ClassName, 'initialize']);
end;*)

{function TsmxRequest.GetParamValue(const ParamName: String): Variant;
var
  Param: TsmxParamKitItem;
begin
  Result := Variants.Null;
  if Cfg is TsmxRequestCfg then
  begin
    Param := TsmxRequestCfg(Cfg).Params.FindByName(ParamName);
    if Assigned(Param) then
      Result := Param.ParamValue;
  end;
end;}

procedure TsmxRequest.InternalRefreshParams;
var
  i, j: integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
  Cell: TsmxOwnerCell;
begin
  inherited InternalRefreshParams;
  if not Assigned(CurDataSet) then
    Exit;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to CurDataSet.ParamCount - 1 do
    begin
      //Value := Variants.Null;
      case CurDataSet.Params[i].DataLocation of
        {plConst .. plOutput, plFilterDesk .. plParentGrid:
        begin
          Value := Variants.Null; //FCurDataSetIntf.Params[i].Value;
        end;}
        {plConst .. plInOutput:
        begin
          CurDataSet.Params[i].Value := GetParamValue(CurDataSet.Params[i].ParamName);
          if Assigned(CellOwner) then
            if CellOwner.GetRequestParamValue(CfgID, CurDataSet.Params[i].ParamName, Value) then
              CurDataSet.Params[i].Value := Value;
        end;}
        dlFilterDesk:
        begin
          {smxClassProcs.AllParents(Self, List, [TsmxCustomSection]);
          if List.Count > 0 then
            smxClassFuncs.FindFilterOnSection(TsmxCustomSection(List[0]),
              FCurDataSetIntf.Params[i].ParamName, Value);}
          //Cell := nil;
          if CellRequest is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellRequest).FindSlaveByName(CurDataSet.Params[i].ParamName);
            if Assigned(Cell) then
              CurDataSet.Params[i].Value := TsmxCustomFilter(Cell).FilterValue;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, CurDataSet.Params[i].ParamName, Value) then
            CurDataSet.Params[i].Value := Value;
        end;
        dlGrid:
        begin
          {smxClassProcs.AllParents(Self, List, [TsmxCustomPage]);
          if List.Count > 0 then
            for j := 0 to TsmxCustomPage(List[0]).SlaveCount - 1 do
              if not smxClassFuncs.ExistsParent(Self, TsmxCustomPage(List[0]).Slaves[j]) then
                if smxClassFuncs.FindColumnOnSection(TsmxCustomPage(List[0]).Slaves[j],
                    FCurDataSetIntf.Params[i].ParamName, Value) then
                  Break;}
          if CellRequest is TsmxCustomGrid then
          begin
            if CellRequest.CellParent is TsmxCustomSection then
              if CellRequest.CellParent.CellParent is TsmxCustomPage then
                with TsmxCustomPage(CellRequest.CellParent.CellParent) do
                  for j := 0 to SlaveCount - 1 do
                    if CellRequest.CellParent <> Slaves[j] then
                      if smxClassFuncs.FindColumnOnSection(Slaves[j], CurDataSet.Params[i].ParamName, Value) then
                      begin
                        CurDataSet.Params[i].Value := Value;
                        Break;
                      end;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, CurDataSet.Params[i].ParamName, Value) then
            CurDataSet.Params[i].Value := Value;
        end;
        dlParentFilterDesk:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        dlParentGrid:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            CurDataSet.Params[i].Value := smxProcs.gStorageManagerIntf.Values[CurDataSet.Params[i].ParamName];
        end;
        dlParentCellParam:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
      end;
      //FCurDataSetIntf.Params[i].Value := Value;
    end;
  finally
    List.Free;
  end;
end;

{procedure TsmxRequest.ResetCellProps;
begin
  inherited ResetCellProps;
  DatabaseName := '';
  //DataSet := nil;
  //ModifyPerformances[mrDelete] := pmOpen;
  //ModifyPerformances[mrInsert] := pmOpen;
  //ModifyPerformances[mrUpdate] := pmOpen;

  //DeletePerformance := pmOpen;
  //InsertPerformance := pmOpen;
  //UpdatePerformance := pmOpen;

  //ModifyRequests[mrDelete] := nil;
  //ModifyRequests[mrInsert] := nil;
  //ModifyRequests[mrUpdate] := nil;
  DeleteDataSet := nil;
  InsertDataSet := nil;
  UpdateDataSet := nil;
  OperationMode := omManual;
  //PerformanceMode := pmOpen;
  OnDelete := nil;
  OnExecute := nil;
  OnInsert := nil;
  OnPrepare := nil;
  OnRefreshParams := nil;
  OnUpdate := nil;
end;}

{procedure TsmxRequest.SetCellProps;
var
  i: Integer;
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxRequestCfg then
  begin
    DatabaseName := TsmxRequestCfg(Cfg).DatabaseName;
    OperationMode := TsmxRequestCfg(Cfg).OperationMode;
    //PerformanceMode := TsmxRequestCfg(Cfg).PerformanceMode;
    //ModifyPerformances[mrDelete] := TsmxRequestCfg(Cfg).DeletePerformance;
    //ModifyPerformances[mrInsert] := TsmxRequestCfg(Cfg).InsertPerformance;
    //ModifyPerformances[mrUpdate] := TsmxRequestCfg(Cfg).UpdatePerformance;

    //DeletePerformance := TsmxRequestCfg(Cfg).DeletePerformance;
    //InsertPerformance := TsmxRequestCfg(Cfg).InsertPerformance;
    //UpdatePerformance := TsmxRequestCfg(Cfg).UpdatePerformance;

    //DataSetIntf := smxClassFuncs.NewIntf(CfgID, SelectRequest) as IsmxDataSet;
    //DataSetIntf := GetDataSet;
    if Assigned(DataSet) then
    begin
      DataSet.SQLText := TsmxRequestCfg(Cfg).SQLText;
      DataSet.ClearFields;
      for i := 0 to TsmxRequestCfg(Cfg).Fields.Count - 1 do
        with DataSet.AddField do
        begin
          DataType :=  TsmxRequestCfg(Cfg).Fields[i].DataType;
          FieldName := TsmxRequestCfg(Cfg).Fields[i].FieldName;
          DisplayFormat := TsmxRequestCfg(Cfg).Fields[i].DisplayFormat;
          FieldSense := TsmxRequestCfg(Cfg).Fields[i].FieldSense;
          Precision := TsmxRequestCfg(Cfg).Fields[i].Precision;
          Size := TsmxRequestCfg(Cfg).Fields[i].Size;
        end;
      DataSet.ClearParams;
      for i := 0 to TsmxRequestCfg(Cfg).Params.Count - 1 do
        with DataSet.AddParam do
        begin
          DataType := TsmxRequestCfg(Cfg).Params[i].DataType;
          ParamName := TsmxRequestCfg(Cfg).Params[i].ParamName;
          ParamLocation := TsmxRequestCfg(Cfg).Params[i].ParamLocation;
          ParamType := TsmxRequestCfg(Cfg).Params[i].ParamType;
          Value := TsmxRequestCfg(Cfg).Params[i].ParamValue;
          NumericScale := TsmxRequestCfg(Cfg).Params[i].NumericScale;
          Precision := TsmxRequestCfg(Cfg).Params[i].Precision;
          Size := TsmxRequestCfg(Cfg).Params[i].Size;
        end;
    end;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      //ModifyRequests[mrDelete] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).DeleteReqCfgID);
      //ModifyRequests[mrInsert] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).InsertReqCfgID);
      //ModifyRequests[mrUpdate] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).UpdateReqCfgID);
      DeleteDataSet := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).DeleteReqCfgID);
      InsertDataSet := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).InsertReqCfgID);
      UpdateDataSet := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).UpdateReqCfgID);


      OnDelete := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnExecute := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnInsert := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnUpdate := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
    end;
  end;
end;}

{ TsmxRequestList }

{constructor TsmxRequestList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;}

{function TsmxRequestList.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxRequestListCfg;
end;}

{function TsmxRequestList.GetRequestParamValue(CfgID: Integer;
  const ParamName: String; var ParamValue: Variant): Boolean;
var
  //i: Integer;
  Param: TsmxParamKitItem;
  Item: TsmxOwnerKitItem;
begin
  Result := inherited GetRequestParamValue(CfgID, ParamName, ParamValue);
  if Cfg is TsmxRequestListCfg then
  begin
    Item := TsmxRequestListCfg(Cfg).SlaveCells.FindByCfgID(CfgID);
    if Item is TsmxRequestKitItem then
    begin
      Param := TsmxRequestKitItem(Item).ItemParams.FindByName(ParamName);
      if Assigned(Param) then
      begin
        ParamValue := Param.ParamValue;
        Result := True;
      end;
    end;
  end;
end;}

function TsmxRequestList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxRequest;
end;

{procedure TsmxRequestList.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
var
  i: Integer;
  Param: TsmxParamKitItem;
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxCustomRequest) and (Item is TsmxRequestKitItem) then
  begin
    TsmxCustomRequest(Slave).DatabaseName := TsmxRequestKitItem(Item).DatabaseName;
    TsmxCustomRequest(Slave).OperationMode := TsmxRequestKitItem(Item).OperationMode;
    if Assigned(TsmxCustomRequest(Slave).DataSet) then
      for i := 0 to TsmxCustomRequest(Slave).DataSet.ParamCount - 1 do
      begin
        Param := TsmxRequestKitItem(Item).ItemParams.FindByName(TsmxCustomRequest(Slave).DataSet.Params[i].ParamName);
        if Assigned(Param) then
          TsmxCustomRequest(Slave).DataSet.Params[i].Value := Param.ParamValue;
      end;
  end;
end;}

initialization
  Classes.RegisterClasses([TsmxAction, TsmxActionList, TsmxRequest,
    TsmxRequestList]);

end.
