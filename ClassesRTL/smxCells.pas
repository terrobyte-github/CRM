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
  Classes, Controls, {ComCtrls, {DB, {DBGrids,} Forms, ExtCtrls, StdCtrls,{ Menus,}
  ActnList, {Windows,} ImgList, Graphics, {smxBaseClasses,} smxClasses, {smxCfgs,}
  {smxStdCtrls,} smxDBIntf, smxTypes{, smxClassTypes{, smxManagerIntf{, smxBaseTypes};

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
    function GetAlgorithmCaption: TCaption; override;
    function GetAlgorithmEnabled: Boolean; override;
    function GetAlgorithmHint: String; override;
    function GetAlgorithmHotKey: TShortCut; override;
    function GetAlgorithmImageIndex: TImageIndex; override;
    function GetAlgorithmVisible: Boolean; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalRef: Pointer; override;
    //function GetProcPointer: Pointer; virtual;
    //procedure InitializeEvent; virtual;
    //procedure InternalInitialize; override;
    procedure InternalRefreshParams; override;
    //procedure ResetCellProps; override;
    procedure SetAlgorithmCaption(const Value: TCaption); override;
    procedure SetAlgorithmEnabled(Value: Boolean); override;
    procedure SetAlgorithmHint(const Value: String); override;
    procedure SetAlgorithmHotKey(Value: TShortCut); override;
    procedure SetAlgorithmImageIndex(Value: TImageIndex); override;
    procedure SetAlgorithmVisible(Value: Boolean); override;
    //procedure ProcExec(Sender: TObject); virtual;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    //procedure SetLibraryManager(Value: TsmxCustomLibraryManager); override;
    procedure ChangeObjectIndex(Value: Integer); override;

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

  {TsmxActionList = class(TsmxCustomAlgorithmList)
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
  end;}

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

  {TsmxRequestList = class(TsmxCustomRequestList)
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
  end;}

  { TsmxFilter }

  TsmxFilter = class(TsmxCustomFilter)
  private
    FHeader: TLabel;
    FPanel: TPanel;
    function GetHeader: TLabel;
    function GetPanel: TPanel;
  protected
    //procedure DoChangeFilter; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellAnchors: TAnchors; override;
    //function GetCellCursor: TCursor; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    //function GetFilter: TObject; virtual;
    function GetHeaderAlignment: TAlignment; override;
    function GetCellCaption: TCaption; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetInternalRef: Pointer; override;
    //procedure InternalInitialize; override;
    //procedure ResetCellProps; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellAnchors(Value: TAnchors); override;
    //procedure SetCellCursor(Value: TCursor); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetCellCaption(const Value: TCaption); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;
    //procedure UnBindFilterObjects; virtual;

    property Header: TLabel read GetHeader;
    property Panel: TPanel read GetPanel;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  published
    property CellAlign;
    property CellAnchors;
    property CellEnabled;
    property CellHeight;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property HeaderAlignment;
    property HeaderCaption;
    property HeaderColor;
    property HeaderFont;
    property PopupMenu;
  end;

  { TsmxPanelFilterDesk }

  {TsmxPanelFilterDesk = class(TsmxCustomFilterDesk)
  private
    function GetCfg: TsmxPanelFilterDeskCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxPanelFilterDeskCfg read GetCfg;
  end;}

  { TsmxPanelFilterDesk }

  {TsmxPanelFilterDesk = class(TsmxCustomFilterDesk)
  private
    FPanel: TPanel;
    function GetPanel: TPanel;
    procedure RefreshValueParams(DataSet: IsmxDataSet);
  protected
    //procedure DoApply; override;
    //procedure DoPrepare; override;
    //procedure DoRefresh; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellAnchors: TAnchors; override;
    //function GetCellCursor: TCursor; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    //function GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass; override;
    function GetCellCaption: String; override;
    function GetCellHint: String; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalApply; override;
    //procedure InternalInitialize; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    //procedure ResetCellProps; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellAnchors(Value: TAnchors); override;
    //procedure SetCellCursor(Value: TCursor); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    //procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;

    property Panel: TPanel read GetPanel;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property IsAltSlaveClass default True;
  published
    property CellAlign;
    property CellAnchors;
    property CellCaption;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellHint;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property PopupMenu;
    property SlaveList;

    property OnDoubleSnap;
    property OnSnap;
  end;}

  { TsmxForm }

  TsmxForm = class(TsmxCustomForm)
  private
    FForm: TForm;
    FFormImageIndex: TImageIndex;
    FIsMainForm: Boolean;
    function GetForm: TForm;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    //procedure FormDestroy(Sender: TObject);
    //procedure PrepareForm;
  protected
    //procedure DoClose; override;
    //procedure DoShow; override;
    function GetCellActive: Boolean; override;
    function GetCellCaption: TCaption; override;
    function GetCellHint: String; override;
    function GetCellImageIndex: TImageIndex; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetFormBorder: TsmxFormBorder; override;
    function GetFormPosition: TsmxFormPosition; override;
    function GetInternalRef: Pointer; override;
    //function GetIsMainForm: Boolean; override;
    function GetIsMaximize: Boolean; override;
    function GetModalResult: TModalResult; override;
    procedure InternalClose; override;
    //procedure InternalInitialize; override;
    procedure InternalShow; override;
    function InternalShowModal: TModalResult; override;
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //procedure ResetCellProps; override;
    procedure SetCellActive(Value: Boolean); override;
    procedure SetCellCaption(const Value: TCaption); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellImageIndex(Value: TImageIndex); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    procedure SetFormBorder(Value: TsmxFormBorder); override;
    procedure SetFormOptions(Value: TsmxFormOptions); override;
    procedure SetFormPosition(Value: TsmxFormPosition); override;
    procedure SetImageList(Value: TCustomImageList); override;
    //procedure SetIsFrameForm(Value: Boolean); override;
    //procedure SetIsMainForm(Value: Boolean); override;
    //procedure SetIsDesigning(Value: Boolean); override;
    procedure SetIsMaximize(Value: Boolean); override;
    procedure SetModalResult(Value: TModalResult); override;
    procedure SetPopupMenu(Value: TsmxCustomPopupMenu); override;

    property Form: TForm read GetForm;
    property IsMainForm: Boolean read FIsMainForm;
  public
    destructor Destroy; override;
    //procedure Close; override;
    //procedure Show; override;
    //function ShowModal: TModalResult; override;
  published
    //property CellActive;
    property CellAlign;
    property CellAnchors;
    property CellCaption;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellHint;
    property CellImageIndex;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property FormBorder;
    property FormOptions;
    property FormPosition;
    property ImageListName;
    property IsMaximize;
    property PopupMenu;

    property OnActivate;
    property OnClose;
    property OnDeactivate;
    property OnDoubleClick;
    property OnShow;
    property OnClick;
  end;

implementation

uses
  {SysUtils,} Variants, Windows, Menus, {ToolWin,} Messages, {smxCommonStorage, smxLibManager,
  smxDBManager, smxFormManager, smxGlobalVariables, smxDBConnection,} smxFuncs,
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

function TsmxAction.GetAlgorithmCaption: TCaption;
begin
  Result := TCaption(Action.Caption);
end;

procedure TsmxAction.SetAlgorithmCaption(const Value: TCaption);
begin
  Action.Caption := String(Value);
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

function TsmxAction.GetAlgorithmHotKey: TShortCut;
begin
  Result := Action.ShortCut;
end;

procedure TsmxAction.SetAlgorithmHotKey(Value: TShortCut);
begin
  Action.ShortCut := Value;
end;

function TsmxAction.GetAlgorithmImageIndex: TImageIndex;
begin
  Result := Action.ImageIndex;
end;

procedure TsmxAction.SetAlgorithmImageIndex(Value: TImageIndex);
begin
  Action.ImageIndex := Value;
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
          List.Clear;
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
          List.Clear;
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
          List.Clear;
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

procedure TsmxAction.ChangeObjectIndex(Value: Integer);
begin
  //inherited SetSlaveIndex(Value);
  Action.Index := Value;
end;

{ TsmxActionList }

(*{constructor TsmxActionList.Create(AOwner: TComponent);
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

(*function TsmxActionList.GetInternalRef: Pointer;
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
end;*)

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
          List.Clear;
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
          List.Clear;
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
          List.Clear;
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

{function TsmxRequestList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxRequest;
end;}

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

{ TsmxFilter }

{constructor TsmxFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FilterName := Cfg.FilterName;
  FPanel := TPanel.Create(Self);
  FPanel.Height := 49;
  FPanel.BevelOuter := bvNone;
  FHeader := TLabel.Create(Self);
  FHeader.Parent := FPanel;
  FHeader.Font.Color := TColor(Cfg.FilterHeader.Font.Color);
  FHeader.Font.Name := Cfg.FilterHeader.Font.Name;
  FHeader.Font.Size := Cfg.FilterHeader.Font.Size;
  FHeader.Font.Style := Cfg.FilterHeader.Font.Style;
  FHeader.Caption := Cfg.FilterHeader.Text;
  FHeader.Color := TColor(Cfg.FilterHeader.Color);
  case Cfg.FilterHeader.Align of
    taLeftJustify:
    begin
      FHeader.Anchors := [akLeft];
      FHeader.Left := 4;
    end;
    taRightJustify:
    begin
      FHeader.Anchors := [akRight];
      FHeader.Left := FPanel.Width - FHeader.Width - 4;
    end;
    taCenter:
    begin
      FHeader.Anchors := [akLeft, akRight];
      FHeader.Left := (FPanel.Width - FHeader.Width) div 2;
    end;
  end;
  FHeader.Top := 4;
end;}

destructor TsmxFilter.Destroy;
begin
  inherited Destroy;
  //UnBindFilterObjects;
  //if GetFilter is TControl then
    //TControl(GetFilter).Parent := nil;
  if Assigned(FHeader) then
    FHeader.Free;
  if Assigned(FPanel) then
    FPanel.Free;
end;

{procedure TsmxFilter.DoChangeFilter;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnChangeFilter) then
  begin
    if Cfg is TsmxFilterCfg then
      AlgCfgID := TsmxFilterCfg(Cfg).ChangeFilterAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnChangeFilter, AlgCfgID);
  end;
end;}

{function TsmxFilter.GetCellAlign: TAlign;
begin
  Result := Panel.Align;
end;

procedure TsmxFilter.SetCellAlign(Value: TAlign);
begin
  Panel.Align := Value;
end;

function TsmxFilter.GetCellAnchors: TAnchors;
begin
  Result := Panel.Anchors;
end;

procedure TsmxFilter.SetCellAnchors(Value: TAnchors);
begin
  Panel.Anchors := Value;
end;

function TsmxFilter.GetCellCursor: TCursor;
begin
  Result := Panel.Cursor;
end;

procedure TsmxFilter.SetCellCursor(Value: TCursor);
begin
  Panel.Cursor := Value;
end;

function TsmxFilter.GetCellEnabled: Boolean;
begin
  Result := Panel.Enabled;
end;

procedure TsmxFilter.SetCellEnabled(Value: Boolean);
begin
  Panel.Enabled := Value;
end;

function TsmxFilter.GetCellHeight: Integer;
begin
  Result := Panel.Height;
end;

procedure TsmxFilter.SetCellHeight(Value: Integer);
begin
  Panel.Height := Value;
end;

function TsmxFilter.GetCellLeft: Integer;
begin
  Result := Panel.Left;
end;

procedure TsmxFilter.SetCellLeft(Value: Integer);
begin
  Panel.Left := Value;
end;

function TsmxFilter.GetCellTop: Integer;
begin
  Result := Panel.Top;
end;

procedure TsmxFilter.SetCellTop(Value: Integer);
begin
  Panel.Top := Value;
end;

function TsmxFilter.GetCellVisible: Boolean;
begin
  Result := Panel.Visible;
end;

procedure TsmxFilter.SetCellVisible(Value: Boolean);
begin
  Panel.Visible := Value;
end;

function TsmxFilter.GetCellWidth: Integer;
begin
  Result := Panel.Width;
end;

procedure TsmxFilter.SetCellWidth(Value: Integer);
begin
  Panel.Width := Value;
end;}

{function TsmxFilter.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxFilterCfg;
end;}

{function TsmxFilter.GetFilter: TObject;
begin
  Result := nil;
end;}

function TsmxFilter.GetHeader: TLabel;
begin
  if not Assigned(FHeader) then
  begin
    FHeader := TLabel.Create(nil);
    FHeader.Parent := Panel;
    FHeader.Left := 4;
    FHeader.Top := 4;
    FHeader.Width := Panel.Width - 8;
    FHeader.Anchors := [akLeft, akRight];
  end;
  Result := FHeader;
end;

function TsmxFilter.GetHeaderAlignment: TAlignment;
begin
  Result := Header.Alignment;
end;

procedure TsmxFilter.SetHeaderAlignment(Value: TAlignment);
begin
  Header.Alignment := Value;
end;

function TsmxFilter.GetCellCaption: TCaption;
begin
  Result := Header.Caption;
end;

procedure TsmxFilter.SetCellCaption(const Value: TCaption);
begin
  Header.Caption := Value;
end;

function TsmxFilter.GetHeaderColor: TColor;
begin
  Result := Header.Color;
end;

procedure TsmxFilter.SetHeaderColor(Value: TColor);
begin
  Header.Color := Value;
end;

function TsmxFilter.GetHeaderFont: TFont;
begin
  Result := Header.Font;
end;

procedure TsmxFilter.SetHeaderFont(Value: TFont);
begin
  Header.Font := Value;
end;

function TsmxFilter.GetInternalRef: Pointer;
begin
  Result := Pointer(Panel);
end;

function TsmxFilter.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanel.Create(nil);
    FPanel.Caption := '';
    FPanel.Height := 49;
    FPanel.BevelOuter := bvNone;
  end;
  Result := FPanel;
end;

{procedure TsmxFilter.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxFilterCfg then
  begin
    DisplayFormat := TsmxFilterCfg(Cfg).DisplayFormat;
    FilterAlignment := TsmxFilterCfg(Cfg).FilterText.Alignment;
    FilterCaption := TsmxFilterCfg(Cfg).FilterText.Caption;
    FilterColor := TColor(TsmxFilterCfg(Cfg).FilterText.Color);
    FilterFont.Color := TColor(TsmxFilterCfg(Cfg).FilterText.Font.Color);
    FilterFont.Name := TsmxFilterCfg(Cfg).FilterText.Font.Name;
    FilterFont.Size := TsmxFilterCfg(Cfg).FilterText.Font.Size;
    FilterFont.Style := TsmxFilterCfg(Cfg).FilterText.Font.Style;
    FilterOptions := TsmxFilterCfg(Cfg).FilterOptions;
    FilterValue := TsmxFilterCfg(Cfg).FilterValue;
    HeaderAlignment := TsmxFilterCfg(Cfg).FilterHeader.Alignment;
    HeaderCaption := TsmxFilterCfg(Cfg).FilterHeader.Caption;
    HeaderColor := TColor(TsmxFilterCfg(Cfg).FilterHeader.Color);
    HeaderFont.Color := TColor(TsmxFilterCfg(Cfg).FilterHeader.Font.Color);
    HeaderFont.Name := TsmxFilterCfg(Cfg).FilterHeader.Font.Name;
    HeaderFont.Size := TsmxFilterCfg(Cfg).FilterHeader.Font.Size;
    HeaderFont.Style := TsmxFilterCfg(Cfg).FilterHeader.Font.Style;
    ValueFormat := TsmxFilterCfg(Cfg).ValueFormat;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      //Request := smxClassFuncs.GetRequestForm(Form, TsmxFilterCfg(Cfg).RequestCfgID);
      OnChangeFilter := smxClassFuncs.GetEventForm(Form, TsmxFilterCfg(Cfg).ChangeFilterAlgCfgID);
    end;
  end;
end;}

{procedure TsmxFilter.ResetCellProps;
begin
  inherited ResetCellProps;
  DisplayFormat := '';
  FilterAlignment := taLeftJustify;
  FilterCaption := '';
  FilterColor := Graphics.clBlack;
  FilterFont.Color := Graphics.clBlack;
  FilterFont.Name := '';
  FilterFont.Size := 0;
  FilterFont.Style := [];
  FilterOptions := [];
  FilterValue := Variants.Null;
  HeaderAlignment := taLeftJustify;
  HeaderCaption := '';
  HeaderColor := Graphics.clBlack;
  HeaderFont.Color := Graphics.clBlack;
  HeaderFont.Name := '';
  HeaderFont.Size := 0;
  HeaderFont.Style := [];
  //Request := nil;
  ValueFormat := '';
  OnChangeFilter := nil;
end;}

procedure TsmxFilter.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject({_TsmxBaseCell}(CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

{procedure TsmxFilter.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxFilterCfg then
  begin
    DisplayFormat := TsmxFilterCfg(Cfg).DisplayFormat;
    FilterAlignment := TsmxFilterCfg(Cfg).FilterText.Alignment;
    FilterCaption := TsmxFilterCfg(Cfg).FilterText.Caption;
    FilterColor := TColor(TsmxFilterCfg(Cfg).FilterText.Color);
    FilterFont.Color := TColor(TsmxFilterCfg(Cfg).FilterText.Font.Color);
    FilterFont.Name := TsmxFilterCfg(Cfg).FilterText.Font.Name;
    FilterFont.Size := TsmxFilterCfg(Cfg).FilterText.Font.Size;
    FilterFont.Style := TsmxFilterCfg(Cfg).FilterText.Font.Style;
    FilterOptions := TsmxFilterCfg(Cfg).FilterOptions;
    FilterValue := TsmxFilterCfg(Cfg).FilterValue;
    HeaderAlignment := TsmxFilterCfg(Cfg).FilterHeader.Alignment;
    HeaderCaption := TsmxFilterCfg(Cfg).FilterHeader.Caption;
    HeaderColor := TColor(TsmxFilterCfg(Cfg).FilterHeader.Color);
    HeaderFont.Color := TColor(TsmxFilterCfg(Cfg).FilterHeader.Font.Color);
    HeaderFont.Name := TsmxFilterCfg(Cfg).FilterHeader.Font.Name;
    HeaderFont.Size := TsmxFilterCfg(Cfg).FilterHeader.Font.Size;
    HeaderFont.Style := TsmxFilterCfg(Cfg).FilterHeader.Font.Style;
    ValueFormat := TsmxFilterCfg(Cfg).ValueFormat;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      //Request := smxClassFuncs.GetRequestForm(Form, TsmxFilterCfg(Cfg).RequestCfgID);
      OnChangeFilter := smxClassFuncs.GetEventForm(Form, TsmxFilterCfg(Cfg).ChangeFilterAlgCfgID);
    end;
  end;
end;}

{procedure TsmxFilter.UnBindFilterObjects;
begin
  if Assigned(FHeader) then
    FHeader.Parent := nil;
end;}

{ TsmxPanelFilterDesk }

{procedure TsmxPanelFilterDesk.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  with Cfg.ApplyRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        ApplyRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.PrepareRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        PrepareRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  FilterList.Count := Cfg.Filters.Count;
  for i := 0 to Cfg.Filters.Count - 1 do
    with Cfg.Filters[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomFilter then
          FilterList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxPanelFilterDesk.GetCfg: TsmxPanelFilterDeskCfg;
begin
  Result := TsmxPanelFilterDeskCfg(inherited Cfg);
end;

procedure TsmxPanelFilterDesk.InitChilds;
var i: Integer;
begin
  if Assigned(ApplyRequest) then
    with Cfg.ApplyRequest do
    begin
      ApplyRequest.DatabaseName := DatabaseName;
      ApplyRequest.OperationMode := Operation;
    end;
  if Assigned(PrepareRequest) then
    with Cfg.PrepareRequest do
    begin
      PrepareRequest.DatabaseName := DatabaseName;
      PrepareRequest.OperationMode := Operation;
    end;
  for i := 0 to Cfg.Filters.Count - 1 do
    with Cfg.Filters[i] do
    begin
      Filters[i].CellAlign := UnitAlign;
      Filters[i].CellEnabled := UnitEnabled;
      Filters[i].CellHeight := UnitHeight;
      Filters[i].CellLeft := UnitLeft;
      Filters[i].CellTop := UnitTop;
      Filters[i].CellVisible := UnitVisible;
      Filters[i].CellWidth := UnitWidth;
    end;
end;}

{ TsmxPanelFilterDesk }

{constructor TsmxPanelFilterDesk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;}

(*destructor TsmxPanelFilterDesk.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FPanel) then
    FPanel.Free;
end;

{procedure TsmxPanelFilterDesk.DoApply;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnApply) then
  begin
    if Cfg is TsmxPanelFilterDeskCfg then
      AlgCfgID := TsmxPanelFilterDeskCfg(Cfg).ApplyAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnApply, AlgCfgID);
  end;
end;}

procedure TsmxPanelFilterDesk.InternalApply;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalApply;
  if Assigned(Request) then
  begin
    DataSet := Request.UpdateDataSet; // ModifyRequests[mrUpdate];
    if Assigned(DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        for i := 0 to SlaveCount - 1 do
        begin
          if foApply in Slaves[i].FilterOptions then
          begin
            //if foSetValue in Slaves[i].FilterOptions then
              DataSet.ParamByName(Slaves[i].Name).Value :=
                Slaves[i].FilterValue;
            //if foSetCaption in Slaves[i].FilterOptions then
              DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value :=
                smxFuncs.StrToVar(Slaves[i].FilterCaption);
          end;

          {if foApplyValue in Slaves[i].FilterOptions then
            DataSet.ParamByName(Slaves[i].SlaveName).Value :=
              Slaves[i].FilterValue;
          if foApplyText in Slaves[i].FilterOptions then
            DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value :=
              smxFuncs.StrToVar(Slaves[i].FilterCaption);}
        end;
        Request.Update;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

{procedure TsmxPanelFilterDesk.DoPrepare;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnPrepare) then
  begin
    if Cfg is TsmxPanelFilterDeskCfg then
      AlgCfgID := TsmxPanelFilterDeskCfg(Cfg).PrepareAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnPrepare, AlgCfgID);
  end;
end;}

procedure TsmxPanelFilterDesk.InternalPrepare;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalPrepare;
  if Assigned(Request) then
  begin
    DataSet := Request.DataSet;
    if Assigned(DataSet) then
    begin
      DataSet.Close;
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        Request.Prepare;
        if DataSet.Active then
          for i := 0 to SlaveCount - 1 do
          begin
            if foPrepare in Slaves[i].FilterOptions then
              case DataSet.PerformanceMode of
                pmOpen:
                begin
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.FieldByName(Slaves[i].Name).Value;
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                  {if foPrepareValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.FieldByName(Slaves[i].SlaveName).Value;
                  if foPrepareText in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
                end;
                pmExecute:
                begin
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.ParamByName(Slaves[i].Name).Value;
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                  {if foPrepareValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.ParamByName(Slaves[i].SlaveName).Value;
                  if foPrepareText in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
                end;
              end;
          end;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

{procedure TsmxPanelFilterDesk.DoRefresh;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnRefresh) then
  begin
    if Cfg is TsmxPanelFilterDeskCfg then
      AlgCfgID := TsmxPanelFilterDeskCfg(Cfg).RefreshAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefresh, AlgCfgID);
  end;
end;}

procedure TsmxPanelFilterDesk.InternalRefresh;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalPrepare;
  if Assigned(Request) then
  begin
    DataSet := Request.DataSet;
    if Assigned(DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        Request.Execute;
        for i := 0 to SlaveCount - 1 do
        begin
          if foPrepare in Slaves[i].FilterOptions then
            case DataSet.PerformanceMode of
              pmOpen:
              begin
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.FieldByName(Slaves[i].Name).Value;
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                {if foPrepareValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.FieldByName(Slaves[i].SlaveName).Value;
                if foPrepareText in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
              end;
              pmExecute:
              begin
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.ParamByName(Slaves[i].Name).Value;
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                {if foPrepareValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.ParamByName(Slaves[i].SlaveName).Value;
                if foPrepareText in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
              end;
            end;
          {case Request.PerformanceMode of
            pmOpen:
            begin
              if foPrepareValue in Slaves[i].FilterOptions then
                Slaves[i].FilterValue :=
                  DataSet.FieldByName(Slaves[i].SlaveName).Value;
              if foPrepareText in Slaves[i].FilterOptions then
                Slaves[i].FilterCaption :=
                  Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);
            end;
            pmExecute:
            begin
              if foPrepareValue in Slaves[i].FilterOptions then
                Slaves[i].FilterValue :=
                  DataSet.ParamByName(Slaves[i].SlaveName).Value;
              if foPrepareText in Slaves[i].FilterOptions then
                Slaves[i].FilterCaption :=
                  Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);
            end;
          end;}
        end;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

{function TsmxPanelFilterDesk.GetCellAlign: TAlign;
begin
  Result := Panel.Align;
end;

procedure TsmxPanelFilterDesk.SetCellAlign(Value: TAlign);
begin
  Panel.Align := Value;
end;

function TsmxPanelFilterDesk.GetCellAnchors: TAnchors;
begin
  Result := Panel.Anchors;
end;

procedure TsmxPanelFilterDesk.SetCellAnchors(Value: TAnchors);
begin
  Panel.Anchors := Value;
end;

function TsmxPanelFilterDesk.GetCellCursor: TCursor;
begin
  Result := Panel.Cursor;
end;

procedure TsmxPanelFilterDesk.SetCellCursor(Value: TCursor);
begin
  Panel.Cursor := Value;
end;

function TsmxPanelFilterDesk.GetCellEnabled: Boolean;
begin
  Result := Panel.Enabled;
end;

procedure TsmxPanelFilterDesk.SetCellEnabled(Value: Boolean);
begin
  Panel.Enabled := Value;
end;

function TsmxPanelFilterDesk.GetCellHeight: Integer;
begin
  Result := Panel.Height;
end;

procedure TsmxPanelFilterDesk.SetCellHeight(Value: Integer);
begin
  Panel.Height := Value;
end;

function TsmxPanelFilterDesk.GetCellLeft: Integer;
begin
  Result := Panel.Left;
end;

procedure TsmxPanelFilterDesk.SetCellLeft(Value: Integer);
begin
  Panel.Left := Value;
end;

function TsmxPanelFilterDesk.GetCellTop: Integer;
begin
  Result := Panel.Top;
end;

procedure TsmxPanelFilterDesk.SetCellTop(Value: Integer);
begin
  Panel.Top := Value;
end;

function TsmxPanelFilterDesk.GetCellVisible: Boolean;
begin
  Result := Panel.Visible;
end;

procedure TsmxPanelFilterDesk.SetCellVisible(Value: Boolean);
begin
  Panel.Visible := Value;
end;

function TsmxPanelFilterDesk.GetCellWidth: Integer;
begin
  Result := Panel.Width;
end;

procedure TsmxPanelFilterDesk.SetCellWidth(Value: Integer);
begin
  Panel.Width := Value;
end;}

function TsmxPanelFilterDesk.GetCellCaption: String;
begin
  Result := Panel.Caption;
end;

procedure TsmxPanelFilterDesk.SetCellCaption(const Value: String);
begin
  Panel.Caption := Value;
end;

function TsmxPanelFilterDesk.GetCellHint: String;
begin
  Result := Panel.Hint;
end;

procedure TsmxPanelFilterDesk.SetCellHint(const Value: String);
begin
  Panel.Hint := Value;
end;

{function TsmxPanelFilterDesk.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxPanelFilterDeskCfg;
end;}

function TsmxPanelFilterDesk.GetInternalRef: Pointer;
begin
  Result := Pointer(Panel);
end;

function TsmxPanelFilterDesk.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanel.Create(nil);
    FPanel.Caption := '';
    FPanel.BevelOuter := bvNone;
  end;
  Result := FPanel;
end;

{function TsmxPanelFilterDesk.GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass;
begin
  if Cfg is TsmxPanelFilterDeskCfg then
    Result := TsmxOwnerCellClass(smxClassFuncs.CfgIDToCellClass(
      TsmxPanelFilterDeskCfg(Cfg).SlaveCells[Index].CfgID, SelectRequest))
  else
    Result := inherited GetAltSlaveClass(Index);
end;}

function TsmxPanelFilterDesk.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxFilter;
end;

{procedure TsmxPanelFilterDesk.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxPanelFilterDeskCfg then
  begin
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxPanelFilterDeskCfg(Cfg).RequestCfgID);
      OnApply := smxClassFuncs.GetEventForm(Form, TsmxPanelFilterDeskCfg(Cfg).ApplyAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxPanelFilterDeskCfg(Cfg).PrepareAlgCfgID);
    end;
  end;
end;}

procedure TsmxPanelFilterDesk.RefreshValueParams(DataSet: IsmxDataSet);
var
  i, j: Integer;
  List: TList;
  Value: Variant;
begin
  List := TList.Create;
  try
    for i := 0 to DataSet.ParamCount - 1 do
      case DataSet.Params[i].DataLocation of
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            DataSet.Params[i].Value := smxProcs.gStorageManagerIntf.Values[DataSet.Params[i].ParamName];
        end;
        dlParentCellParam:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(DataSet.Params[i].ParamName, Value) then
            begin
              DataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        else
          DataSet.Params[i].Value := Variants.Null;
      end;
  finally
    List.Free;
  end;
end;

{procedure TsmxPanelFilterDesk.ResetCellProps;
begin
  inherited ResetCellProps;
  //OnApply := nil;
  //OnPrepare := nil;
  //OnRefresh := nil;
  Request := nil;
end;}

procedure TsmxPanelFilterDesk.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject({_TsmxBaseCell}(CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

{procedure TsmxPanelFilterDesk.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxPanelFilterDeskCfg then
  begin
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxPanelFilterDeskCfg(Cfg).RequestCfgID);
      //OnApply := smxClassFuncs.GetEventForm(Form, TsmxPanelFilterDeskCfg(Cfg).ApplyAlgCfgID);
      //OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxPanelFilterDeskCfg(Cfg).PrepareAlgCfgID);
      //OnRefresh := smxClassFuncs.GetEventForm(Form, TsmxPanelFilterDeskCfg(Cfg).RefreshAlgCfgID);
    end;
  end;
end;}

{procedure TsmxPanelFilterDesk.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxCustomFilter) and (Item is TsmxFilterKitItem) then
  begin
    TsmxCustomFilter(Slave).DisplayFormat := TsmxFilterKitItem(Item).DisplayFormat;
    TsmxCustomFilter(Slave).FilterOptions := TsmxFilterKitItem(Item).FilterOptions;
    TsmxCustomFilter(Slave).ValueFormat := TsmxFilterKitItem(Item).ValueFormat;
  end;
end;}*)

{ TsmxForm }

destructor TsmxForm.Destroy;
begin
  inherited Destroy;
  if Assigned(FForm) then
  {begin
    if FIsMainForm then
      FForm := nil
    else}
      FForm.Free;
  {end;}
end;

{procedure TsmxForm.Close;
begin
  Form.OnClose := nil;
  Form.Close;
  Form.OnClose := FormClose;
end;}

{procedure TsmxForm.DoClose;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnClose) then
  begin
    if Cfg is TsmxFormCfg then
      AlgCfgID := TsmxFormCfg(Cfg).CloseAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnClose, AlgCfgID);
  end;
end;}

procedure TsmxForm.InternalClose;
begin
  //Form.OnClose := nil;

  //Form.Close;

  if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) then
    begin
      if (foFreeOnClose in FormOptions) and not IsDesigning then
      begin
        //Action := caNone;
        Free;
      end else
        //Action := caHide;
        Form.Hide;
    end;
  end else
  begin
    //FForm.Hide;


    FForm := nil;
    //Free;
  end;


  {if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) and FIsCloseUser then
      Free;
  end else
    FForm := nil;}
  //Form.OnClose := FormClose;

  {if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) and (foFreeOnClose in FormOptions) then
    begin
      //Action := caNone;
      Free;
    end else
      Form.Close;
  end else
  begin
    FForm := nil;
    Free;
  end;}
end;

{procedure TsmxForm.DoShow;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnShow) then
  begin
    if Cfg is TsmxFormCfg then
      AlgCfgID := TsmxFormCfg(Cfg).ShowAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnShow, AlgCfgID);
  end;
end;}

procedure TsmxForm.InternalShow;
begin                
  //Form.OnClose := FormClose;
  Form.Show;
end;

function TsmxForm.InternalShowModal: TModalResult;
begin
  //Form.OnClose := FormClose;
  Result := TModalResult(Form.ShowModal);
end;

procedure TsmxForm.FormActivate(Sender: TObject);
begin
  Activate;
end;

procedure TsmxForm.FormDeactivate(Sender: TObject);
begin
  Deactivate;
end;

procedure TsmxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Action := caNone;
  Close;

  //DoClose;
  {if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) then
    begin
      if (foFreeOnClose in FormOptions) and not IsDesigning then
      begin
        Action := caNone;
        Free;
      end else
        Action := caHide;
    end;
  end else
  begin
    FForm := nil;
    Free;
  end;}
end;

{procedure TsmxForm.FormDestroy(Sender: TObject);
begin
  if FIsMainForm then
  begin
    FForm := nil;
    Free;
  end;
end;}

function TsmxForm.GetCellActive: Boolean;
begin
  Result := Form.Active;
end;

procedure TsmxForm.SetCellActive(Value: Boolean);
begin
  //Windows.BringWindowToTop()
  //Form.SetFocus
  //Windows.GetDesktopWindow
  {if Value then
    Windows.SetActiveWindow(Form.Handle)
  else
    Windows.SetForegroundWindow(Form.Handle);}
  if Value then
    Form.Perform(WM_ACTIVATE, WA_ACTIVE, 0)
  else
    Form.Perform(WM_ACTIVATE, WA_INACTIVE, 0);
end;

function TsmxForm.GetCellCaption: TCaption;
begin
  Result := Form.Caption;
end;

procedure TsmxForm.SetCellCaption(const Value: TCaption);
begin
  Form.Caption := Value;
end;

function TsmxForm.GetCellHint: String;
begin
  Result := Form.Hint;
end;

procedure TsmxForm.SetCellHint(const Value: String);
begin
  Form.Hint := Value;
end;

function TsmxForm.GetCellImageIndex: TImageIndex;
begin
  Result := FFormImageIndex;
end;

procedure TsmxForm.SetCellImageIndex(Value: TImageIndex);
begin
  if FFormImageIndex <> -1 then
    Form.Icon := nil;
  FFormImageIndex := Value;
  if FFormImageIndex <> -1 then
    if Assigned(ImageList) then
      ImageList.GetIcon(Integer(FFormImageIndex), Form.Icon);
end;

{function TsmxForm.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxFormCfg;
end;}

function TsmxForm.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    //if IsApplicationForm then
    if not FIsMainForm then
    begin
      if not Assigned(Forms.Application.MainForm) then
      begin
        Forms.Application.CreateForm(TForm, FForm);
        FIsMainForm := True;
      end else
        FForm := TForm.Create(nil);
      FForm.OnClose := FormClose;
      //FForm.OnDestroy := FormDestroy;
      FForm.OnActivate := FormActivate;
      FForm.OnDeactivate := FormDeactivate;
      //FIsMainForm := Forms.Application.MainForm = FForm;
      //if FIsMainForm then
        //FForm.FreeNotification(Self);
    end;// else
      //FForm := TForm.Create(Self);
    //else
      //FForm := TForm.Create(Self);
  end;
  Result := FForm;
end;

function TsmxForm.GetFormBorder: TsmxFormBorder;
const
  InOutBorder: array[TFormBorderStyle] of TsmxFormBorder =
    (fbNone, fbDialog, fbSizeable, fbDialog, fbDialog, fbSizeable);
begin
  Result := InOutBorder[Form.BorderStyle];
end;

procedure TsmxForm.SetFormBorder(Value: TsmxFormBorder);
const
  OutInBorder: array[TsmxFormBorder] of TFormBorderStyle =
    (bsNone, bsDialog, bsSizeable);
begin
  Form.BorderStyle := OutInBorder[Value];
end;

function TsmxForm.GetFormPosition: TsmxFormPosition;
const
  InOutPosition: array[TPosition] of TsmxFormPosition =
    (fpDesigned, fpDesigned, fpDesigned, fpDesigned, fpScreenCenter,
     fpScreenCenter, fpOwnerFormCenter, fpOwnerFormCenter);
begin
  Result := InOutPosition[Form.Position];
end;

procedure TsmxForm.SetFormPosition(Value: TsmxFormPosition);
const
  OutInPosition: array[TsmxFormPosition] of TPosition =
    (poDesigned, poScreenCenter, poOwnerFormCenter);
begin
  Form.Position := OutInPosition[Value];
end;

function TsmxForm.GetInternalRef: Pointer;
begin
  Result := Pointer(Form);
end;

{function TsmxForm.GetIsMainForm: Boolean;
begin
  Result := Forms.Application.MainForm = Form;
end;}

function TsmxForm.GetIsMaximize: Boolean;
begin
  Result := Form.WindowState = wsMaximized;
end;

procedure TsmxForm.SetIsMaximize(Value: Boolean);
begin
  if Value then
    Form.WindowState := wsMaximized else
    Form.WindowState := wsNormal;
end;

function TsmxForm.GetModalResult: TModalResult;
begin
  Result := Form.ModalResult;
end;

procedure TsmxForm.SetModalResult(Value: TModalResult);
begin
  Form.ModalResult := Value;
end;

{procedure TsmxForm.InternalInitialize;
var
  Cell: TsmxBaseCell;
begin
  inherited InternalInitialize;
  if Cfg is TsmxFormCfg then
  begin
    FormBorder := TsmxFormCfg(Cfg).FormBorder;
    FormPosition := TsmxFormCfg(Cfg).FormPosition;
    IsMaximize := TsmxFormCfg(Cfg).IsMaximize;

    if TsmxFormCfg(Cfg).AlgorithmListCfgID > 0 then
    begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).AlgorithmListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      AlgorithmList := TsmxCustomAlgorithmList(Cell);
    end;

    if TsmxFormCfg(Cfg).PopupListCfgID > 0 then
    begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).PopupListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      PopupList := TsmxCustomPopupList(Cell);
    end;

    if TsmxFormCfg(Cfg).RequestListCfgID > 0 then
    begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).RequestListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      RequestList := TsmxCustomRequestList(Cell);
    end;
  end;
end;}

{procedure TsmxForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;}

{procedure TsmxForm.PrepareForm;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    smxClassProcs.AllCells(Self, List, [TsmxCustomGrid], True);
    for i := 0 to List.Count - 1 do
      TsmxCustomGrid(List[i]).Prepare;
  finally
    List.Free;
  end;
end;}

{procedure TsmxForm.ResetCellProps;
begin
  inherited ResetCellProps;
  FormBorder := fbNone;
  FormPosition := fpDesigned;
  IsMaximize := False;
  AlgorithmList := nil;
  OnClose := nil;
  OnShow := nil;
  PopupList := nil;
  RequestList := nil;
end;}

(*procedure TsmxForm.SetCellProps;

  function CreateCell(ACfgID: Integer): TsmxBaseCell;
  begin
    Result := smxClassFuncs.NewCell(nil, ACfgID{, SelectRequest});
    Result.CellParent := Self;
    Result.Initialize;
  end;

begin
  if Cfg is TsmxFormCfg then
  begin
    FormBorder := TsmxFormCfg(Cfg).FormBorder;
    FormPosition := TsmxFormCfg(Cfg).FormPosition;
    IsMaximize := TsmxFormCfg(Cfg).IsMaximize;

    if TsmxFormCfg(Cfg).AlgorithmListCfgID > 0 then
      AlgorithmList := TsmxCustomAlgorithmList(CreateCell(TsmxFormCfg(Cfg).AlgorithmListCfgID));
    {begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).AlgorithmListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      AlgorithmList := TsmxCustomAlgorithmList(Cell);
    end;}

    if TsmxFormCfg(Cfg).PopupListCfgID > 0 then
      PopupList := TsmxCustomPopupList(CreateCell(TsmxFormCfg(Cfg).PopupListCfgID));
    {begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).PopupListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      PopupList := TsmxCustomPopupList(Cell);
    end;}

    if TsmxFormCfg(Cfg).RequestListCfgID > 0 then
      RequestList := TsmxCustomRequestList(CreateCell(TsmxFormCfg(Cfg).RequestListCfgID));
    {begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).RequestListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      RequestList := TsmxCustomRequestList(Cell);
    end;}

    OnClose := smxClassFuncs.GetEventForm(Self, TsmxFormCfg(Cfg).CloseAlgCfgID);
    OnShow := smxClassFuncs.GetEventForm(Self, TsmxFormCfg(Cfg).ShowAlgCfgID);
  end;
  inherited SetCellProps;
  {if Cfg is TsmxFormCfg then
  begin
    FormBorder := TsmxFormCfg(Cfg).FormBorder;
    FormPosition := TsmxFormCfg(Cfg).FormPosition;
    IsMaximize := TsmxFormCfg(Cfg).IsMaximize;
  end;}
end;*)

procedure TsmxForm.SetFormOptions(Value: TsmxFormOptions);
var
  Obj: TObject;
begin
  if (foFrameForm in FormOptions) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetFormOptions(Value);
  if (foFrameForm in FormOptions) and Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Form.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxForm.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) and Assigned(Form) then
    Form.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) and Assigned(Form) then
    if FFormImageIndex <> -1 then
      ImageList.GetIcon(FFormImageIndex, Form.Icon);
end;

{procedure TsmxForm.SetIsFrameForm(Value: Boolean);
var
  Obj: TObject;
begin
  if IsFrameForm and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetIsFrameForm(Value);
  if IsFrameForm and Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Form.Parent := TWinControl(Obj);
  end;
end;}

{procedure TsmxForm.SetIsMainForm(Value: Boolean);
begin
  if IsMainForm <> Value then
  begin
    if not Assigned(Forms.Application.MainForm) then
    begin
      if Assigned(FForm) then
      begin
        FForm.Free;

      end;
      Forms.Application.CreateForm();
    end;
  end;
end;}

{procedure TsmxForm.Show;
begin
  Form.Show;
  PrepareForm;
end;

function TsmxForm.ShowModal: TModalResult;
begin
  PrepareForm;
  Result := TModalResult(Form.ShowModal);
end;}

procedure TsmxForm.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetCellParent(Value);
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Form.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxForm.SetPopupMenu(Value: TsmxCustomPopupMenu);
var
  Obj: TObject;
begin
  if Assigned(PopupMenu) then
    Form.PopupMenu := nil;
  inherited SetPopupMenu(Value);
  if Assigned(PopupMenu) then
  begin
    Obj := TObject({_TsmxBaseCell}(CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TPopupMenu then
      Form.PopupMenu := TPopupMenu(Obj);
  end;
end;

//initialization
  //Classes.RegisterClasses([TsmxAction, {TsmxActionList,} TsmxRequest,
    //{TsmxRequestList,} TsmxFilter, {TsmxPanelFilterDesk,} TsmxForm]);

end.
