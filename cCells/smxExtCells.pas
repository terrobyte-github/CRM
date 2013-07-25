{**************************************}
{                                      }
{            SalesMan v1.0             }
{        ExtendedCells classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxExtCells;

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, Buttons, ImgList, Graphics,
  XMLIntf, smxBaseClasses, smxClasses, smxCfgs, smxCells, smxDBIntf, smxTypes;

type
  { TsmxLibAlgorithmCfg }

  TsmxLibAlgorithmCfg = class(TsmxAlgorithmCfg)
  private
    FAlgorithmLibrary: String;
    FAlgorithmProcName: String;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetAlgorithmLibrary(const Value: String); virtual;
    procedure SetAlgorithmProcName(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property AlgorithmLibrary: String read FAlgorithmLibrary write SetAlgorithmLibrary;
    property AlgorithmProcName: String read FAlgorithmProcName write SetAlgorithmProcName;
  end;

  { TsmxLibAction }

  TsmxLibAction = class(TsmxAction)
  private
    FAlgorithmLibrary: String;
    FAlgorithmProcName: String;
    FProcPointer: Pointer;
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetProcPointer: Pointer; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetAlgorithmLibrary(const Value: String); virtual;
    procedure SetAlgorithmProcName(const Value: String); virtual;
    procedure SetCellProps; override;
  public
    procedure Assign(Source: TPersistent); override;

    property AlgorithmLibrary: String read FAlgorithmLibrary write SetAlgorithmLibrary;
    property AlgorithmProcName: String read FAlgorithmProcName write SetAlgorithmProcName;
  end;

  { TsmxRefRequestCfg }

  TsmxRefRequestCfg = class(TsmxRequestCfg)
  private
    FRequestClassName: String;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetRequestClassName(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property RequestClassName: String read FRequestClassName write SetRequestClassName;
  end;

  { TsmxRefRequest }

  TsmxRefRequest = class(TsmxRequest)
  private
    FDataSetIntf: IsmxDataSet;
    FRequestClass: TsmxInterfacedPersistentClass;
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetDataSet: IsmxDataSet; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellProps; override;
    procedure SetRequestClass(Value: TsmxInterfacedPersistentClass); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property RequestClass: TsmxInterfacedPersistentClass read FRequestClass write SetRequestClass;
  end;

  { TsmxDBRequestCfg }

  TsmxDBRequestCfg = class(TsmxRequestCfg)
  private
    FDataSetType: TsmxDataSetType;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetDataSetType(Value: TsmxDataSetType); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property DataSetType: TsmxDataSetType read FDataSetType write SetDataSetType;
  end;

  { TsmxDBRequest }

  TsmxDBRequest = class(TsmxRequest)
  private
    FDataSetIntf: IsmxDataSet;
    FDataSetType: TsmxDataSetType;
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetDataSet: IsmxDataSet; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellProps; override;
    procedure SetDatabase(const Value: IsmxDatabase); override;
    procedure SetDataSetType(Value: TsmxDataSetType); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DataSetType: TsmxDataSetType read FDataSetType write SetDataSetType;
  end;

  { TsmxLibRequestCfg }

  TsmxLibRequestCfg = class(TsmxRequestCfg)
  private
    FDataSetType: TsmxDataSetType;
    FRequestFuncName: String;
    FRequestLibrary: String;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetDataSetType(Value: TsmxDataSetType); virtual;
    procedure SetRequestFuncName(const Value: String); virtual;
    procedure SetRequestLibrary(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property DataSetType: TsmxDataSetType read FDataSetType write SetDataSetType;
    property RequestFuncName: String read FRequestFuncName write SetRequestFuncName;
    property RequestLibrary: String read FRequestLibrary write SetRequestLibrary;
  end;

  { TsmxLibRequest }

  TsmxLibRequest = class(TsmxRequest)
  private
    FDataSetIntf: IsmxDataSet;
    FDataSetType: TsmxDataSetType;
    FRequestFuncName: String;
    FRequestLibrary: String;
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetDataSet: IsmxDataSet; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellProps; override;
    procedure SetDataSetType(Value: TsmxDataSetType); virtual;
    procedure SetRequestFuncName(const Value: String); virtual;
    procedure SetRequestLibrary(const Value: String); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DataSetType: TsmxDataSetType read FDataSetType write SetDataSetType;
    property RequestFuncName: String read FRequestFuncName write SetRequestFuncName;
    property RequestLibrary: String read FRequestLibrary write SetRequestLibrary;
  end;

  { TsmxEditFilter }

  TsmxEditFilter = class(TsmxPanelFilter)
  private
    FEdit: TEdit;
    function GetEdit: TEdit;
    procedure EditChange(Sender: TObject);
  protected
    function GetCellCaption: String; override;
    function GetCellEnabled: Boolean; override;
    function GetCellHint: String; override;
    function GetFilterColor: TColor; override;
    //function GetFilter: TObject; override;
    function GetFilterFont: TFont; override;
    function GetFilterValue: Variant; override;
    //function GetInternalObject: TObject; override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetFilterColor(Value: TColor); override;
    procedure SetFilterFont(Value: TFont); override;
    procedure SetFilterValue(const Value: Variant); override;
    //procedure UnBindFilterObjects; override;

    property Edit: TEdit read GetEdit;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxDateTimeFilter }

  {TsmxDateTimeFilter = class(TsmxPanelFilter)
  private
    FDateTime: TDateTimePicker;
    procedure SetFormat;
  protected
    function GetCellEnabled: Boolean; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetFilterValue(Value: Variant); override;

    property DateTime: TDateTimePicker read FDateTime;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxBitBtnFilter }

  {TsmxBitBtnFilter = class(TsmxPanelFilter)
  private
    FBitBtn: TBitBtn;
    FValue: Variant;
  protected
    function GetCellEnabled: Boolean; override;
    function GetFilterText: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetFilterText(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure Initialize; override;
    procedure UnInitialize; override;

    property BitBtn: TBitBtn read FBitBtn;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxNumEditFilter }

  {TsmxNumEditFilter = class(TsmxPanelFilter)
  private
    FEdit: TEdit;
  protected
    procedure ProcKeyPress(Sender: TObject; var Key: Char);
    function GetCellEnabled: Boolean; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetFilterValue(Value: Variant); override;

    property Edit: TEdit read FEdit;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxLabelFilter }

  {TsmxLabelFilter = class(TsmxPanelFilter)
  private
    FSLabel: TLabel;
    FValue: Variant;
  protected
    function GetFilterText: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetFilterText(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;

    property SLabel: TLabel read FSLabel;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxStateKitItem }

  TsmxStateKit = class;

  TsmxStateKitItem = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FCurrentIntfID: Integer;
    FItemEnabled: Boolean;
    FItemVisible: Boolean;
    FPriorIntfID: Integer;
    function GetHKit: TsmxStateKit;
    function GetItem(Index: Integer): TsmxStateKitItem;
    function GetParent: TsmxStateKitItem;
    procedure SetCurrentIntfID(Value: Integer);
    procedure SetItem(Index: Integer; Value: TsmxStateKitItem);
    procedure SetParent(Value: TsmxStateKitItem);
    procedure SwitchIntfID;
  protected
    procedure SetItemEnabled(Value: Boolean); virtual;
    procedure SetItemVisible(Value: Boolean); virtual;

    property PriorIntfID: Integer read FPriorIntfID write FPriorIntfID;
  public
    function Add: TsmxStateKitItem;
    procedure Assign(Source: TsmxHKitItem); override;
    function FindByCfgID(CfgID: Integer; AmongAll: Boolean = False): TsmxStateKitItem;

    property CfgID: Integer read FCfgID write FCfgID;
    property CurrentIntfID: Integer read FCurrentIntfID write SetCurrentIntfID;
    property HKit: TsmxStateKit read GetHKit;
    property Items[Index: Integer]: TsmxStateKitItem read GetItem write SetItem; default;
    property ItemEnabled: Boolean read FItemEnabled write SetItemEnabled;
    property ItemVisible: Boolean read FItemVisible write SetItemVisible;
    property Parent: TsmxStateKitItem read GetParent write SetParent;
  end;

  { TsmxStateKit }

  TsmxStateKit = class(TsmxHKit)
  private
    FIntfID: Integer;
    function GetRoot: TsmxStateKitItem;
    procedure SetRoot(Value: TsmxStateKitItem);
  public
    procedure Assign(Source: TsmxHKit); override;

    property IntfID: Integer read FIntfID write FIntfID;
    property Root: TsmxStateKitItem read GetRoot write SetRoot;
  end;

  { TsmxCellState }

  TsmxCellStates = class;

  TsmxCellState = class(TsmxKitItem)
  private
    FStateID: Integer;
    FStateKit: TsmxStateKit;
    function GetKit: TsmxCellStates;
    procedure SetKit(Value: TsmxCellStates);
    function GetStateKit: TsmxStateKit;
    procedure SetStateKit(Value: TsmxStateKit);
  public
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxCellStates read GetKit write SetKit;
    property StateID: Integer read FStateID write FStateID;
    property StateKit: TsmxStateKit read GetStateKit write SetStateKit;
  end;

  { TsmxCellStates }

  TsmxCellStates = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxCellState;
    procedure SetItem(Index: Integer; Value: TsmxCellState);
  public
    function Add: TsmxCellState;
    function FindByStateID(StateID: Integer): TsmxCellState;

    property Items[Index: Integer]: TsmxCellState read GetItem write SetItem; default;
  end;

  { TsmxSimpleStateCfg }

  TsmxSimpleStateCfg = class(TsmxStateCfg)
  private
    FCellStates: TsmxCellStates;
    function GetCellStates: TsmxCellStates;
    procedure SetCellStates(Value: TsmxCellStates);
  protected
    procedure ReadIntf(const Node: IXMLNode; ID: Integer); override;
    procedure WriteIntf(const Node: IXMLNode; ID: Integer); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property CellStates: TsmxCellStates read GetCellStates write SetCellStates;
  end;

  { TsmxStateFormCfg }

  TsmxStateFormCfg = class(TsmxFormCfg)
  private
    FStateID: Integer;
    FStateReqCfgID: Integer;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetStateID(Value: Integer); virtual;
    procedure SetStateReqCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property StateID: Integer read FStateID write SetStateID;
    property StateReqCfgID: Integer read FStateReqCfgID write SetStateReqCfgID;
  end;

  { TsmxStateForm }

  TsmxStateForm = class(TsmxForm)
  private
    FStateCfg: TsmxStateCfg;
    //FStateCfgClass: TsmxStateCfgClass;
    FStateID: Integer;
    FStateRequest: TsmxCustomRequest;
    function GetStateCfg: TsmxStateCfg;
    //procedure SetStateCfg(Value: TsmxStateCfg);
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetStateCfgClass: TsmxStateCfgClass; virtual;
    //procedure LockState; virtual;
    //procedure InternalInitialize; override;
    //procedure PutState; virtual;
    procedure RefreshStateCfg; virtual;
    procedure RefreshStateID; virtual;
    procedure ResetCellProps; override;
    procedure SetCellProps; override;
    procedure SetCfgID(Value: Integer); override;
    procedure SetIntfID(Value: Integer); override;
    //procedure SetStateCfgClass(Value: TsmxStateCfgClass); virtual;
    procedure SetStateID(Value: Integer); virtual;
    procedure SetStateRequest(Value: TsmxCustomRequest); virtual;

    property StateCfg: TsmxStateCfg read GetStateCfg;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; override;

    //property StateCfgClass: TsmxStateCfgClass read GetStateCfgClass;// write SetStateCfgClass;
    property StateID: Integer read FStateID write SetStateID;
    property StateRequest: TsmxCustomRequest read FStateRequest write SetStateRequest;
  end;

  { TsmxStandardFormCfg }

  TsmxStandardFormCfg = class(TsmxStateFormCfg)
  private
    FControlBoard: TsmxControlKitItem;
    FMainMenu: TsmxControlKitItem;
    FStatusBoard: TsmxControlKitItem;
    function GetControlBoard: TsmxControlKitItem;
    function GetMainMenu: TsmxControlKitItem;
    function GetStatusBoard: TsmxControlKitItem;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetControlBoard(Value: TsmxControlKitItem); virtual;
    procedure SetMainMenu(Value: TsmxControlKitItem); virtual;
    procedure SetStatusBoard(Value: TsmxControlKitItem); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property ControlBoard: TsmxControlKitItem read GetControlBoard write SetControlBoard;
    property MainMenu: TsmxControlKitItem read GetMainMenu write SetMainMenu;
    property StatusBoard: TsmxControlKitItem read GetStatusBoard write SetStatusBoard;
  end;

  { TsmxStandardForm }

  TsmxStandardForm = class(TsmxStateForm)
  private
    //FAlgorithmList: TsmxCustomAlgorithmList;
    FControlBoard: TsmxCustomControlBoard;
    FMainMenu: TsmxCustomMainMenu;
    //FRequestList: TsmxCustomRequestList;
    FStatusBoard: TsmxCustomStatusBoard;
    function GetSlave(Index: Integer): TsmxCustomPageManager;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPageManager);
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalApply; override;
    //procedure InternalPrepare; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    //procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetCellProps; override;
    procedure SetControlBoard(Value: TsmxCustomControlBoard); virtual;
    procedure SetMainMenu(Value: TsmxCustomMainMenu); virtual;
    //procedure SetRequestList(Value: TsmxCustomRequestList); virtual;
    procedure SetStatusBoard(Value: TsmxCustomStatusBoard); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function AddSlave: TsmxCustomPageManager;

    //property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write SetAlgorithmList;
    property ControlBoard: TsmxCustomControlBoard read FControlBoard write SetControlBoard;
    property MainMenu: TsmxCustomMainMenu read FMainMenu write SetMainMenu;
    //property RequestList: TsmxCustomRequestList read FRequestList write SetRequestList;
    property Slaves[Index: Integer]: TsmxCustomPageManager read GetSlave write SetSlave; default;
    property StatusBoard: TsmxCustomStatusBoard read FStatusBoard write SetStatusBoard;
  end;

implementation

uses
  Variants, Forms, SysUtils, TypInfo, smxFuncs, smxClassFuncs, smxDBTypes;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

{ TsmxLibAlgorithmCfg }

procedure TsmxLibAlgorithmCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxLibAlgorithmCfg then
  begin
    AlgorithmLibrary := TsmxLibAlgorithmCfg(Source).AlgorithmLibrary;
    AlgorithmProcName := TsmxLibAlgorithmCfg(Source).AlgorithmProcName;
  end;
end;

procedure TsmxLibAlgorithmCfg.Clear;
begin
  inherited Clear;
  AlgorithmLibrary := '';
  AlgorithmProcName := '';
end;

procedure TsmxLibAlgorithmCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  AlgorithmLibrary := Node.Attributes['AlgorithmLibrary'];
  AlgorithmProcName := Node.Attributes['AlgorithmProcName'];
end;

procedure TsmxLibAlgorithmCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['AlgorithmLibrary'] := AlgorithmLibrary;
  Node.Attributes['AlgorithmProcName'] := AlgorithmProcName;
end;

procedure TsmxLibAlgorithmCfg.SetAlgorithmLibrary(const Value: String);
begin
  FAlgorithmLibrary := Value;
end;

procedure TsmxLibAlgorithmCfg.SetAlgorithmProcName(const Value: String);
begin
  FAlgorithmProcName := Value;
end;

{ TsmxLibAction }

procedure TsmxLibAction.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxLibAction then
  begin
    AlgorithmLibrary := TsmxLibAction(Source).AlgorithmLibrary;
    AlgorithmProcName := TsmxLibAction(Source).AlgorithmProcName;
  end;
end;

function TsmxLibAction.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxLibAlgorithmCfg;
end;

function TsmxLibAction.GetProcPointer: Pointer;
begin
  if not Assigned(FProcPointer) then
    if Assigned(LibraryManager) then
      FProcPointer := LibraryManager.GetProcedure(FAlgorithmLibrary, FAlgorithmProcName);
  Result := FProcPointer;
end;

(*procedure TsmxLibAction.InternalInitialize;
//var
  //Proc: Pointer;
  //Method: TMethod;
begin
  inherited InternalInitialize;
  if Cfg is TsmxLibAlgorithmCfg then
  begin
    AlgorithmLibrary := TsmxLibAlgorithmCfg(Cfg).AlgorithmLibrary;
    AlgorithmProcName := TsmxLibAlgorithmCfg(Cfg).AlgorithmProcName;
    InitializeEvent;
    {Proc := GetProcudure;
    if Assigned(Proc) then
    begin
      Method.Code := Proc;
      Method.Data := Self;
      OnExecute := TsmxComponentEvent(Method);
    end;}
  end;
end;*)

procedure TsmxLibAction.ResetCellProps;
begin
  inherited ResetCellProps;
  AlgorithmLibrary := '';
  AlgorithmProcName := '';
end;

procedure TsmxLibAction.SetAlgorithmLibrary(const Value: String);
begin
  if FAlgorithmLibrary <> Value then
  begin
    FAlgorithmLibrary := Value;
    FProcPointer := nil;
  end;
end;

procedure TsmxLibAction.SetAlgorithmProcName(const Value: String);
begin
  if FAlgorithmProcName <> Value then
  begin
    FAlgorithmProcName := Value;
    FProcPointer := nil;
  end;
end;

{procedure TsmxLibAction.Initialize(const ACfgDatabase: IsmxDatabase;
  ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil);
begin
  //inherited Initialize(ACfgDatabase, ACfgID, ASelectRequest);
  Cfg := smxClassFuncs.NewCfg(Self, ACfgDatabase, ACfgID, ASelectRequest);
  try
    if Cfg is TsmxActionCfg then
      with TsmxActionCfg(Cfg) do
      begin
        AlgorithmCaption := AlgDefCaption;
        AlgorithmHint := AlgDefHint;
        AlgorithmHotKey := AlgDefHotKey;
        AlgorithmImageIndex := AlgImageIndex;
        AlgorithmParams := AlgParams;
        AlgorithmLibrary := AlgLibrary;
        AlgorithmProcName := AlgProcedure;
      end
    else
      raise EsmxCellError.CreateResFmt(@SCellBuildError, [ACfgID]);
  finally
    Cfg.Free;
  end;
end;}

{procedure TsmxLibAction.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Action.ActionList := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TCustomActionList then
      Action.ActionList := TCustomActionList(Obj);
  end;
end;}

procedure TsmxLibAction.SetCellProps;
//var
  //Method: TMethod;
begin
  if Cfg is TsmxLibAlgorithmCfg then
  begin
    AlgorithmLibrary := TsmxLibAlgorithmCfg(Cfg).AlgorithmLibrary;
    AlgorithmProcName := TsmxLibAlgorithmCfg(Cfg).AlgorithmProcName;
    //InitializeEvent;
    //Proc := GetProcudure;
    {if Assigned(ProcPointer) then
    begin
      Method.Code := ProcPointer;
      Method.Data := Self;
      OnExecute := TsmxComponentEvent(Method);
    end;}
  end;
  inherited SetCellProps;
end;

{ TsmxRefRequestCfg }

procedure TsmxRefRequestCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxRefRequestCfg then
    RequestClassName := TsmxRefRequestCfg(Source).RequestClassName;
end;

procedure TsmxRefRequestCfg.Clear;
begin
  inherited Clear;
  RequestClassName := '';
end;

procedure TsmxRefRequestCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  RequestClassName := Node.Attributes['RequestClassName'];
end;

procedure TsmxRefRequestCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['RequestClassName'] := RequestClassName;
end;

procedure TsmxRefRequestCfg.SetRequestClassName(const Value: String);
begin
  FRequestClassName := Value;
end;

{ TsmxRefRequest }

destructor TsmxRefRequest.Destroy;
begin
  FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxRefRequest.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxRefRequest then
    RequestClass := TsmxRefRequest(Source).RequestClass;
end;

function TsmxRefRequest.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxRefRequestCfg;
end;

function TsmxRefRequest.GetDataSet: IsmxDataSet;
begin
  if not Assigned(FDataSetIntf) then
    if Assigned(FRequestClass) then
      FDataSetIntf := FRequestClass.Create as IsmxDataSet;
  Result := FDataSetIntf;
  {if Assigned(FRequestClass) then
    Result := FRequestClass.Create as IsmxDataSet else
    Result := nil;}
end;

{procedure TsmxRefRequest.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxRefRequestCfg then
  begin
    RequestClass := TsmxInterfacedPersistentClass(
      Classes.FindClass(TsmxRefRequestCfg(Cfg).RequestClassName));
    InitializeDataSet;
  end;
end;}

procedure TsmxRefRequest.ResetCellProps;
begin
  inherited ResetCellProps;
  RequestClass := nil;
end;

procedure TsmxRefRequest.SetCellProps;
begin
  if Cfg is TsmxRefRequestCfg then
    RequestClass := TsmxInterfacedPersistentClass(
      Classes.FindClass(TsmxRefRequestCfg(Cfg).RequestClassName));
  inherited SetCellProps;
end;

procedure TsmxRefRequest.SetRequestClass(Value: TsmxInterfacedPersistentClass);
begin
  if FRequestClass <> Value then
  begin
    FRequestClass := Value;
    FDataSetIntf := nil;
  end;
end;

{ TsmxDBRequestCfg }

procedure TsmxDBRequestCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxDBRequestCfg then
    DataSetType := TsmxDBRequestCfg(Source).DataSetType;
end;

procedure TsmxDBRequestCfg.Clear;
begin
  inherited Clear;
  DataSetType := dstQuery;
end;

procedure TsmxDBRequestCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  DataSetType := TsmxDataSetType(TypInfo.GetEnumValue(TypeInfo(TsmxDataSetType), Node.Attributes['DataSetType']));
end;

procedure TsmxDBRequestCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['DataSetType'] := TypInfo.GetEnumName(TypeInfo(TsmxDataSetType), Integer(DataSetType));
end;

procedure TsmxDBRequestCfg.SetDataSetType(Value: TsmxDataSetType);
begin
  FDataSetType := Value;
end;

{ TsmxDBRequest }

destructor TsmxDBRequest.Destroy;
begin
  FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxDBRequest.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxDBRequest then
    DataSetType := TsmxDBRequest(Source).DataSetType;
end;

function TsmxDBRequest.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxDBRequestCfg;
end;

function TsmxDBRequest.GetDataSet: IsmxDataSet;
begin
  if not Assigned(FDataSetIntf) then
    if Assigned(Database) then
      FDataSetIntf := Database.NewDataSet(FDataSetType);
  Result := FDataSetIntf;
  {if Assigned(Database) then
    Result := Database.NewDataSet(FDataSetType) else
    Result := nil;}
end;

{procedure TsmxDBRequest.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxDBRequestCfg then
  begin
    DataSetType := TsmxDBRequestCfg(Cfg).DataSetType;
    InitializeDataSet;
  end;
end;}

procedure TsmxDBRequest.ResetCellProps;
begin
  inherited ResetCellProps;
  DataSetType := dstQuery;
end;

procedure TsmxDBRequest.SetCellProps;
begin
  if Cfg is TsmxDBRequestCfg then
    DataSetType := TsmxDBRequestCfg(Cfg).DataSetType;
  inherited SetCellProps;
end;

procedure TsmxDBRequest.SetDatabase(const Value: IsmxDatabase);
begin
  if Database <> Value then
    FDataSetIntf := nil;
  inherited SetDatabase(Value);
end;

procedure TsmxDBRequest.SetDataSetType(Value: TsmxDataSetType);
begin
  if FDataSetType <> Value then
  begin
    FDataSetType := Value;
    FDataSetIntf := nil;
  end;
end;

{ TsmxLibRequestCfg }

procedure TsmxLibRequestCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxLibRequestCfg then
  begin
    DataSetType := TsmxLibRequestCfg(Source).DataSetType;
    RequestFuncName := TsmxLibRequestCfg(Source).RequestFuncName;
    RequestLibrary := TsmxLibRequestCfg(Source).RequestLibrary;
  end;
end;

procedure TsmxLibRequestCfg.Clear;
begin
  inherited Clear;
  DataSetType := dstQuery;
  RequestFuncName := '';
  RequestLibrary := '';
end;

procedure TsmxLibRequestCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  DataSetType := TsmxDataSetType(TypInfo.GetEnumValue(TypeInfo(TsmxDataSetType), Node.Attributes['DataSetType']));
  RequestFuncName := Node.Attributes['RequestFuncName'];
  RequestLibrary := Node.Attributes['RequestLibrary'];
end;

procedure TsmxLibRequestCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['DataSetType'] := TypInfo.GetEnumName(TypeInfo(TsmxDataSetType), Integer(DataSetType));
  Node.Attributes['RequestFuncName'] := RequestFuncName;
  Node.Attributes['RequestLibrary'] := RequestLibrary;
end;

procedure TsmxLibRequestCfg.SetDataSetType(Value: TsmxDataSetType);
begin
  FDataSetType := Value;
end;

procedure TsmxLibRequestCfg.SetRequestFuncName(const Value: String);
begin
  FRequestFuncName := Value;
end;

procedure TsmxLibRequestCfg.SetRequestLibrary(const Value: String);
begin
  FRequestLibrary := Value;
end;

{ TsmxLibRequest }

destructor TsmxLibRequest.Destroy;
begin
  FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxLibRequest.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxLibRequest then
  begin
    DataSetType := TsmxLibRequest(Source).DataSetType;
    RequestFuncName := TsmxLibRequest(Source).RequestFuncName;
    RequestLibrary := TsmxLibRequest(Source).RequestLibrary;
  end;
end;

function TsmxLibRequest.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxLibRequestCfg;
end;

function TsmxLibRequest.GetDataSet: IsmxDataSet;
var
  Func: TsmxFuncNewDataSet;
begin
  if not Assigned(FDataSetIntf) then
    if Assigned(LibraryManager) then
    begin
      Func := LibraryManager.GetProcedure(FRequestLibrary, FRequestFuncName);
      if Assigned(Func) then
        FDataSetIntf := Func(FDataSetType)
    end;
  Result := FDataSetIntf;

  {if Assigned(LibraryManager) then
    Func := LibraryManager.GetProcedure(RequestLibrary, RequestFuncName) else
    Func := nil;
  if Assigned(Func) then
    Result := Func(FDataSetType) else
    Result := nil;}
end;

{procedure TsmxLibRequest.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxLibRequestCfg then
  begin
    DataSetType := TsmxLibRequestCfg(Cfg).DataSetType;
    RequestFuncName := TsmxLibRequestCfg(Cfg).RequestFuncName;
    RequestLibrary := TsmxLibRequestCfg(Cfg).RequestLibrary;
    InitializeDataSet;
  end;
end;}

procedure TsmxLibRequest.ResetCellProps;
begin
  inherited ResetCellProps;
  DataSetType := dstQuery;
  RequestFuncName := '';
  RequestLibrary := '';
end;

procedure TsmxLibRequest.SetCellProps;
begin
  if Cfg is TsmxLibRequestCfg then
  begin
    DataSetType := TsmxLibRequestCfg(Cfg).DataSetType;
    RequestFuncName := TsmxLibRequestCfg(Cfg).RequestFuncName;
    RequestLibrary := TsmxLibRequestCfg(Cfg).RequestLibrary;
  end;
  inherited SetCellProps;
end;

procedure TsmxLibRequest.SetDataSetType(Value: TsmxDataSetType);
begin
  if FDataSetType <> Value then
  begin
    FDataSetType := Value;
    FDataSetIntf := nil;
  end;
end;

procedure TsmxLibRequest.SetRequestFuncName(const Value: String);
begin
  if FRequestFuncName <> Value then
  begin
    FRequestFuncName := Value;
    FDataSetIntf := nil;
  end;
end;

procedure TsmxLibRequest.SetRequestLibrary(const Value: String);
begin
  if FRequestLibrary <> Value then
  begin
    FRequestLibrary := Value;
    FDataSetIntf := nil;
  end;
end;

{ TsmxEditFilter }

{constructor TsmxEditFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Panel;
  FEdit.AutoSize := False;
  FEdit.Width := Panel.Width - 8;
  FEdit.Anchors := [akLeft, akRight];
  FEdit.Left := 4;
  FEdit.Top := 20;
  FEdit.Font.Color := Cfg.FilterFont.Color;
  FEdit.Font.Name := Cfg.FilterFont.Name;
  FEdit.Font.Size := Cfg.FilterFont.Size;
  FEdit.Font.Style := Cfg.FilterFont.Style;
  FEdit.Text := '';
end;}

destructor TsmxEditFilter.Destroy;
begin
  //FEdit.Parent := nil;
  if Assigned(FEdit) then
    FEdit.Free;
  inherited Destroy;
end;

procedure TsmxEditFilter.EditChange(Sender: TObject);
begin
  ChangeFilter;
end;

function TsmxEditFilter.GetCellCaption: String;
begin
  Result := Edit.Text;
end;

procedure TsmxEditFilter.SetCellCaption(const Value: String);
begin
  Edit.Text := Value;
end;

function TsmxEditFilter.GetCellEnabled: Boolean;
begin
  Result := Edit.Enabled;
end;

procedure TsmxEditFilter.SetCellEnabled(Value: Boolean);
begin
  Edit.Enabled := Value;
end;

function TsmxEditFilter.GetCellHint: String;
begin
  Result := Edit.Hint;
end;

procedure TsmxEditFilter.SetCellHint(const Value: String);
begin
  Edit.Hint := Value;
end;

function TsmxEditFilter.GetEdit: TEdit;
begin
  if not Assigned(FEdit) then
  begin
    FEdit := TEdit.Create(nil);
    FEdit.Parent := Panel;
    FEdit.AutoSize := False;
    FEdit.Left := 4;
    FEdit.Top := 20;
    FEdit.Width := Panel.Width - 8;
    FEdit.Anchors := [akLeft, akRight];
    FEdit.OnChange := EditChange;
    FEdit.OnClick := ControlClick;
    FEdit.OnDblClick := ControlDblClick;
  end;
  Result := FEdit;
end;

function TsmxEditFilter.GetFilterColor: TColor;
begin
  Result := Edit.Color;
end;

procedure TsmxEditFilter.SetFilterColor(Value: TColor);
begin
  Edit.Color := Value;
end;

function TsmxEditFilter.GetFilterFont: TFont;
begin
  Result := Edit.Font;
end;

procedure TsmxEditFilter.SetFilterFont(Value: TFont);
begin
  Edit.Font := Value;
end;

function TsmxEditFilter.GetFilterValue: Variant;
begin
  Result := smxFuncs.StrToVar(Edit.Text); //FEdit.Text;
  //if Result = '' then
    //Result := Null;
end;

procedure TsmxEditFilter.SetFilterValue(const Value: Variant);
begin
  //if VarIsEmpty(Value) or VarIsNull(Value) then
    //FEdit.Text := '' else
    Edit.Text := Variants.VarToStr(Value);
end;

{function TsmxEditFilter.GetFilter: TObject;
begin
  Result := Edit;
end;}

{function TsmxEditFilter.GetInternalObject: TObject;
begin
  Result := Edit;
end;}

{procedure TsmxEditFilter.UnBindFilterObjects;
begin
  inherited UnBindFilterObjects;
  if Assigned(FEdit) then
    FEdit.Parent := nil;
end;}

{ TsmxDateTimeFilter }

{constructor TsmxDateTimeFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FDateTime := TDateTimePicker.Create(Self);
  SetFormat;
  FDateTime.Parent := Panel;
  FDateTime.Width := Panel.Width - 8;
  FDateTime.Anchors := [akLeft, akRight];
  FDateTime.Left := 4;
  FDateTime.Top := 20;
  FDateTime.ShowCheckbox := True;
  FDateTime.Font.Color := Cfg.FilterFont.Color;
  FDateTime.Font.Name := Cfg.FilterFont.Name;
  FDateTime.Font.Size := Cfg.FilterFont.Size;
  FDateTime.Font.Style := Cfg.FilterFont.Style;
  FDateTime.DateTime := Date;
end;

destructor TsmxDateTimeFilter.Destroy;
begin
  FDateTime.Parent := nil;
  FDateTime.Free;
  inherited Destroy;
end;

function TsmxDateTimeFilter.GetCellEnabled: Boolean;
begin
  Result := FDateTime.Enabled;
end;

function TsmxDateTimeFilter.GetFilterValue: Variant;
begin
  Result := FDateTime.DateTime;
  if (Result = StrToDate('30.12.1899')) or not(FDateTime.Checked) then
    Result := Null else
  if Cfg.ValueFormat <> '' then
    Result := FormatDateTime(Cfg.ValueFormat, Result);
end;

function TsmxDateTimeFilter.GetInternalObject: TObject;
begin
  Result := FDateTime;
end;

procedure TsmxDateTimeFilter.SetCellEnabled(Value: Boolean);
begin
  FDateTime.Enabled := Value;
end;

procedure TsmxDateTimeFilter.SetFilterValue(Value: Variant);
var d: TDateTime;
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    d := StrToDate('30.12.1899') else
    d := StrToDateTimeDef(Value, StrToDate('30.12.1899'));
  if d = StrToDate('30.12.1899') then
  begin
    FDateTime.DateTime := Date;
    FDateTime.Checked := False;
  end else
  begin
    FDateTime.DateTime := d;
    FDateTime.Checked := True;
  end;
end;

procedure TsmxDateTimeFilter.SetFormat;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FDateTime.Parent := f;
    FDateTime.Format := Cfg.DisplayFormat;
    FDateTime.Parent := nil;
  finally
    f.Free;
  end;
end;}

{ TsmxBitBtnFilter }

{constructor TsmxBitBtnFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FBitBtn := TBitBtn.Create(Self);
  FBitBtn.Parent := Panel;
  FBitBtn.Width := Panel.Width - 8;
  FBitBtn.Anchors := [akLeft, akRight];
  FBitBtn.Left := 4;
  FBitBtn.Top := 20;
  FBitBtn.Margin := 2;
  FBitBtn.Font.Color := Cfg.FilterFont.Color;
  FBitBtn.Font.Name := Cfg.FilterFont.Name;
  FBitBtn.Font.Size := Cfg.FilterFont.Size;
  FBitBtn.Font.Style := Cfg.FilterFont.Style;
  FValue := Null;
  FBitBtn.Caption := '';
  Initialize;
  InstallParent;
end;

destructor TsmxBitBtnFilter.Destroy;
begin
  UnInstallParent;
  UnInitialize;
  FBitBtn.Parent := nil;
  FBitBtn.Free;
  inherited Destroy;
end;

function TsmxBitBtnFilter.GetCellEnabled: Boolean;
begin
  Result := FBitBtn.Enabled;
end;

function TsmxBitBtnFilter.GetFilterText: String;
begin
  Result := String(FBitBtn.Caption);
end;

function TsmxBitBtnFilter.GetFilterValue: Variant;
begin
  Result := FValue;
end;

function TsmxBitBtnFilter.GetInternalObject: TObject;
begin
  Result := FBitBtn;
end;

procedure TsmxBitBtnFilter.SetCellEnabled(Value: Boolean);
begin
  FBitBtn.Enabled := Value;
end;

procedure TsmxBitBtnFilter.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    FBitBtn.Glyph := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    if Assigned(Algorithm) then
      if Algorithm.CellImageIndex >= 0 then
        ImageList.GetBitmap(Algorithm.CellImageIndex, FBitBtn.Glyph);
end;

procedure TsmxBitBtnFilter.SetFilterText(Value: String);
begin
  FBitBtn.Caption := TCaption(Value);
end;

procedure TsmxBitBtnFilter.SetFilterValue(Value: Variant);
begin
  FValue := Value;
end;

procedure TsmxBitBtnFilter.Initialize;
var c: TObject;
begin
  if Assigned(Algorithm) then
  begin
    //if Algorithm.CellImageIndex >= 0 then
      //ImageList.GetBitmap(Algorithm.CellImageIndex, FBitBtn.Glyph);
    c := _TsmxBaseCell(Algorithm).GetInternalObject;
    if c is TBasicAction then
      FBitBtn.Action := TBasicAction(c);
  end;
end;

procedure TsmxBitBtnFilter.UnInitialize;
begin
  FBitBtn.Action := nil;
end;}

{ TsmxNumEditFilter }

{constructor TsmxNumEditFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Panel;
  FEdit.AutoSize := False;
  FEdit.Width := Panel.Width - 8;
  FEdit.Anchors := [akLeft, akRight];
  FEdit.Left := 4;
  FEdit.Top := 20;
  FEdit.OnKeyPress := ProcKeyPress;
  FEdit.Font.Color := Cfg.FilterFont.Color;
  FEdit.Font.Name := Cfg.FilterFont.Name;
  FEdit.Font.Size := Cfg.FilterFont.Size;
  FEdit.Font.Style := Cfg.FilterFont.Style;
  FEdit.Text := '';
end;

destructor TsmxNumEditFilter.Destroy;
begin
  FEdit.OnKeyPress := nil;
  FEdit.Parent := nil;
  FEdit.Free;
  inherited Destroy;
end;

function TsmxNumEditFilter.GetCellEnabled: Boolean;
begin
  Result := FEdit.Enabled;
end;

function TsmxNumEditFilter.GetFilterValue: Variant;
begin
  Result := FEdit.Text;
  if Result = '' then
    Result := Null else
  if Cfg.ValueFormat <> '' then
    Result := FormatFloat(Cfg.ValueFormat, Result);
end;

function TsmxNumEditFilter.GetInternalObject: TObject;
begin
  Result := FEdit;
end;

procedure TsmxNumEditFilter.ProcKeyPress(Sender: TObject; var Key: Char);

  function CountSep(s: String): Integer;
  var i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(s) do
      if s[i] in ['.', ','] then
        Result := Result + 1;
  end;

var s: String;
begin
  s := FEdit.Text;
  if not(Key in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ',', #8]) then
    Key := #0;
  if (Key in ['.', ',']) and (CountSep(s) > 0) then
    Key := #0;
end;

procedure TsmxNumEditFilter.SetCellEnabled(Value: Boolean);
begin
  FEdit.Enabled := Value;
end;

procedure TsmxNumEditFilter.SetFilterValue(Value: Variant);
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    FEdit.Text := '' else
    FEdit.Text := FloatToStr(StrToFloatDef(Value, 0));
end;}

{ TsmxLabelFilter }

{constructor TsmxLabelFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FSLabel := TLabel.Create(Self);
  FSLabel.Parent := Panel;
  FSLabel.Width := Panel.Width - 8;
  FSLabel.Anchors := [akLeft, akRight];
  FSLabel.Left := 4;
  FSLabel.Top := 20;
  FSLabel.Font.Color := Cfg.FilterFont.Color;
  FSLabel.Font.Name := Cfg.FilterFont.Name;
  FSLabel.Font.Size := Cfg.FilterFont.Size;
  FSLabel.Font.Style := Cfg.FilterFont.Style;
  FValue := Null;
  FSLabel.Caption := '';
end;

destructor TsmxLabelFilter.Destroy;
begin
  FSLabel.Parent := nil;
  FSLabel.Free;
  inherited Destroy;
end;

function TsmxLabelFilter.GetFilterText: String;
begin
  Result := String(FSLabel.Caption);
end;

function TsmxLabelFilter.GetFilterValue: Variant;
begin
  Result := FValue;
end;

function TsmxLabelFilter.GetInternalObject: TObject;
begin
  Result := FSLabel;
end;

procedure TsmxLabelFilter.SetFilterText(Value: String);
begin
  FSLabel.Caption := TCaption(Value);
end;

procedure TsmxLabelFilter.SetFilterValue(Value: Variant);
begin
  FValue := Value;
end;}

{ TsmxStateKitItem }

function TsmxStateKitItem.Add: TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Add);
end;

procedure TsmxStateKitItem.Assign(Source: TsmxHKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxStateKitItem then
  begin
    CfgID := TsmxStateKitItem(Source).CfgID;
    CurrentIntfID := TsmxStateKitItem(Source).CurrentIntfID;
    ItemEnabled := TsmxStateKitItem(Source).ItemEnabled;
    ItemVisible := TsmxStateKitItem(Source).ItemVisible;
  end;
end;

function TsmxStateKitItem.FindByCfgID(CfgID: Integer; AmongAll: Boolean = False): TsmxStateKitItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].CfgID = CfgID then
      Result := Items[i] else
    if AmongAll then
      Result := Items[i].FindByCfgID(CfgID, AmongAll);
    if Assigned(Result) then
      Break;
  end;
end;

function TsmxStateKitItem.GetHKit: TsmxStateKit;
begin
  Result := TsmxStateKit(inherited HKit);
end;

function TsmxStateKitItem.GetItem(Index: Integer): TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Items[Index]);
end;

procedure TsmxStateKitItem.SetItem(Index: Integer; Value: TsmxStateKitItem);
begin
  inherited Items[Index] := Value;
end;

function TsmxStateKitItem.GetParent: TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Parent);
end;

procedure TsmxStateKitItem.SetParent(Value: TsmxStateKitItem);
begin
  inherited Parent := Value;
end;

procedure TsmxStateKitItem.SetCurrentIntfID(Value: Integer);
begin
  if FCurrentIntfID <> Value then
  begin
    FPriorIntfID := FCurrentIntfID;
    FCurrentIntfID := Value;
  end;
end;

procedure TsmxStateKitItem.SetItemEnabled(Value: Boolean);
begin
  if FItemEnabled <> Value then
  begin
    FItemEnabled := Value;
    SwitchIntfID;
  end;
end;

procedure TsmxStateKitItem.SetItemVisible(Value: Boolean);
begin
  if FItemVisible <> Value then
  begin
    FItemVisible := Value;
    SwitchIntfID;
  end;
end;

procedure TsmxStateKitItem.SwitchIntfID;
var
  StateUnit: TsmxStateKitItem;
  IntfID: Integer;
begin
  if FCurrentIntfID = FPriorIntfID then
    IntfID := HKit.IntfID else
    IntfID := FPriorIntfID;
  StateUnit := Self;
  repeat
    StateUnit.CurrentIntfID := IntfID;
    StateUnit := StateUnit.Parent;
  until not StateUnit.HasParent;
end;

{ TsmxStateKit }

procedure TsmxStateKit.Assign(Source: TsmxHKit);
begin
  inherited Assign(Source);
  if Source is TsmxStateKit then
    IntfID := TsmxStateKit(Source).IntfID;
end;

function TsmxStateKit.GetRoot: TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Root);
end;

procedure TsmxStateKit.SetRoot(Value: TsmxStateKitItem);
begin
  inherited Root := Value;
end;

{ TsmxCellState }

destructor TsmxCellState.Destroy;
begin
  if Assigned(FStateKit) then
    FStateKit.Free;
  inherited Destroy;
end;

procedure TsmxCellState.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxCellState then
  begin
    StateID := TsmxCellState(Source).StateID;
    StateKit := TsmxCellState(Source).StateKit;
  end else
    inherited Assign(Source);
end;

function TsmxCellState.GetKit: TsmxCellStates;
begin
  Result := TsmxCellStates(inherited Kit);
end;

procedure TsmxCellState.SetKit(Value: TsmxCellStates);
begin
  inherited Kit := Value;
end;

function TsmxCellState.GetStateKit: TsmxStateKit;
begin
  if not Assigned(FStateKit) then
    FStateKit := TsmxStateKit.Create(TsmxStateKitItem);
  Result := FStateKit;
end;

procedure TsmxCellState.SetStateKit(Value: TsmxStateKit);
begin
  StateKit.Assign(Value);
end;

{ TsmxCellStates }

function TsmxCellStates.Add: TsmxCellState;
begin
  Result := TsmxCellState(inherited Add);
end;

function TsmxCellStates.FindByStateID(StateID: Integer): TsmxCellState;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].StateID = StateID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxCellStates.GetItem(Index: Integer): TsmxCellState;
begin
  Result := TsmxCellState(inherited Items[Index]);
end;

procedure TsmxCellStates.SetItem(Index: Integer; Value: TsmxCellState);
begin
  inherited Items[Index] := Value;
end;

{ TsmxSimpleStateCfg }

destructor TsmxSimpleStateCfg.Destroy;
begin
  if Assigned(FCellStates) then
    FCellStates.Free;
  inherited Destroy;
end;

procedure TsmxSimpleStateCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxSimpleStateCfg then
    CellStates := TsmxSimpleStateCfg(Source).CellStates;
end;

procedure TsmxSimpleStateCfg.Clear;
begin
  if Assigned(FCellStates) then
    FCellStates.Clear;
end;

function TsmxSimpleStateCfg.GetCellStates: TsmxCellStates;
begin
  if not Assigned(FCellStates) then
    FCellStates := TsmxCellStates.Create(TsmxCellState);
  Result := FCellStates;
end;

procedure TsmxSimpleStateCfg.SetCellStates(Value: TsmxCellStates);
begin
  CellStates.Assign(Value);
end;

procedure TsmxSimpleStateCfg.ReadIntf(const Node: IXMLNode; ID: Integer);

  procedure AddItems(const ANode: IXMLNode; AItem: TsmxStateKitItem);
  var
    i: Integer;
    Item: TsmxStateKitItem;
  begin
    Item := AItem.FindByCfgID(ANode.Attributes['StateID']);
    if not Assigned(Item) then
      Item := AItem.Add;
    Item.CurrentIntfID := ID;
    Item.FCfgID := ANode.Attributes['CfgID'];
    Item.FItemEnabled := ANode.Attributes['ItemEnabled']; //SysUtils.StrToBool(ANode.Attributes['ItemEnabled']);
    Item.FItemVisible := ANode.Attributes['ItemVisible']; //SysUtils.StrToBool(ANode.Attributes['ItemVisible']);
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'Cell' then
        AddItems(ANode.ChildNodes[i], Item);
  end;

var
  n, n2: IXMLNode;
  i, j: Integer;
  State: TsmxCellState;
begin
  inherited ReadIntf(Node, ID);
  n := Node.ChildNodes.FindNode('States');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'State' then
      begin
        State := CellStates.FindByStateID(n.ChildNodes[i].Attributes['StateID']);
        if not Assigned(State) then
        begin
          State := CellStates.Add;
          State.StateID := n.ChildNodes[i].Attributes['StateID'];
          State.StateKit.IntfID := IntfID;
        end;
        n2 := n.ChildNodes[i].ChildNodes.FindNode('Cells');
        if Assigned(n2) and (n2.ChildNodes.Count > 0) then
          for j := 0 to n2.ChildNodes.Count - 1 do
            if n2.ChildNodes[j].NodeName = 'Cell' then
              AddItems(n2.ChildNodes[j], State.StateKit.Root);
      end;
  end;
end;

procedure TsmxSimpleStateCfg.WriteIntf(const Node: IXMLNode; ID: Integer);

  procedure AddNodes(const ANode: IXMLNode; AItem: TsmxStateKitItem);
  var
    n: IXMLNode;
    i: Integer;
  begin
    if AItem.CurrentIntfID = ID then
    begin
      n := ANode.AddChild('Cell');
      n.Attributes['CfgID'] := AItem.CfgID;
      n.Attributes['Enabled'] := SysUtils.BoolToStr(AItem.ItemEnabled, True);
      n.Attributes['Visible'] := SysUtils.BoolToStr(AItem.ItemVisible, True);
    end;
    for i := 0 to AItem.Count - 1 do
      AddNodes(n, AItem[i]);
  end;

var
  n, n2: IXMLNode;
  i, j: Integer;
begin
  inherited WriteIntf(Node, ID);
  n := Node.AddChild('States');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('State');
    n2.Attributes['StateID'] := CellStates[i].StateID;
    for j := 0 to CellStates[i].StateKit.Root.Count - 1 do
      AddNodes(n2.AddChild('Cells'), CellStates[i].StateKit.Root[j]);
  end;
end;

{ TsmxStateFormCfg }

procedure TsmxStateFormCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateFormCfg then
  begin
    StateID := TsmxStateFormCfg(Source).StateID;
    StateReqCfgID := TsmxStateFormCfg(Source).StateReqCfgID;
  end;
end;

procedure TsmxStateFormCfg.Clear;
begin
  inherited Clear;
  StateID := 0;
  StateReqCfgID := 0;
end;

procedure TsmxStateFormCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  StateID := Node.Attributes['StateID'];
  StateReqCfgID := Node.Attributes['StateReqCfgID'];
end;

procedure TsmxStateFormCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['StateID'] := StateID;
  Node.Attributes['StateReqCfgID'] := StateReqCfgID;
end;

procedure TsmxStateFormCfg.SetStateID(Value: Integer);
begin
  FStateID := Value;
end;

procedure TsmxStateFormCfg.SetStateReqCfgID(Value: Integer);
begin
  FStateReqCfgID := Value;
end;

{ TsmxStateForm }

destructor TsmxStateForm.Destroy;
begin
  if Assigned(FStateCfg) then
    FStateCfg.Free;
  inherited Destroy;
end;

procedure TsmxStateForm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateForm then
  //begin
    //StateCfgClass := TsmxStateForm(Source).StateCfgClass;
    StateID := TsmxStateForm(Source).StateID;
  //end;
end;

function TsmxStateForm.CellParams(const Name: String; var Value: Variant): Boolean;
begin
  if SysUtils.AnsiCompareText(Name, 'StateID') = 0 then
  begin
    Value := FStateID;
    Result := True;
  end else
    Result := inherited CellParams(Name, Value);
end;

function TsmxStateForm.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxStateFormCfg;
end;

function TsmxStateForm.GetStateCfg: TsmxStateCfg;
begin
  if not Assigned(FStateCfg) then
  //begin
    FStateCfg := GetStateCfgClass.Create(Self);
    {FStateCfg.CfgID := CfgID;
    FStateCfg.IntfID := IntfID;
    FStateCfg.SelectRequest := StateRequest;
    RefreshStateCfg;
  end;}
  Result := FStateCfg;
end;

function TsmxStateForm.GetStateCfgClass: TsmxStateCfgClass;
begin
  Result := TsmxStateCfg;
end;

{procedure TsmxStateForm.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxStateFormCfg then
  begin
    StateID := TsmxStateFormCfg(Cfg).StateID;
    if Assigned(RequestList) then
      StateRequest := TsmxCustomRequest(RequestList.FindSlaveByCfgID(
        TsmxStateFormCfg(Cfg).StateReqCfgID));
  end;
end;}

{procedure TsmxStateForm.PutState;

  procedure PutCell(AItem: TsmxStateKitItem; ACell: TsmxBaseCell);
  var
    i: Integer;
    Cell: TsmxBaseCell;
  begin
    Cell := ACell.FindByCfgID(AItem.CfgID);
    if Assigned(Cell) then
    begin
      if Cell is TsmxCustomAlgorithm then
      begin
        TsmxCustomAlgorithm(Cell).AlgorithmEnabled := AItem.ItemEnabled;
        TsmxCustomAlgorithm(Cell).AlgorithmVisible := AItem.ItemVisible;
      end else
      if Cell is TsmxControlCell then
      begin
        TsmxControlCell(Cell).CellEnabled := AItem.ItemEnabled;
        TsmxControlCell(Cell).CellVisible := AItem.ItemVisible;
      end;
      for i := 0 to AItem.Count - 1 do
        PutCell(AItem[i], Cell);
    end;
  end;

var
  i: Integer;
  CellState: TsmxCellState;
begin
  CellState := StateCfg.CellStates.FindByStateID(FStateID);
  if Assigned(CellState) then
    for i := 0 to CellState.StateKit.Root.Count - 1 do
      PutCell(CellState.StateKit.Root[i], Self);
end;}

procedure TsmxStateForm.RefreshStateCfg;
begin
  //if Assigned(StateCfg) then
    if Assigned(StateCfg.SelectRequest) and (StateCfg.CfgID <> 0)
        and (StateCfg.IntfID <> 0) then
    begin
      StateCfg.Receive;
      //PutState;
    end else
    begin
      StateCfg.Clear;
      //LockState;
    end;
end;

procedure TsmxStateForm.RefreshStateID;
begin
end;

procedure TsmxStateForm.ResetCellProps;
begin
  inherited ResetCellProps;
  StateID := 0;
  StateRequest := nil;
end;

procedure TsmxStateForm.SetCellProps;
begin
  if Cfg is TsmxStateFormCfg then
  begin
    StateID := TsmxStateFormCfg(Cfg).StateID;
    if Assigned(RequestList) then
      StateRequest := TsmxCustomRequest(RequestList.FindSlaveByCfgID(
        TsmxStateFormCfg(Cfg).StateReqCfgID));
  end;
  inherited SetCellProps;
end;

procedure TsmxStateForm.SetCfgID(Value: Integer);
begin
  if CfgID <> Value then
  begin
    inherited SetCfgID(Value);
    StateCfg.CfgID := Value;
    RefreshStateCfg;
    RefreshStateID;
    {if Assigned(FStateCfg) then
    begin
      FStateCfg.CfgID := Value;
      RefreshStateCfg;
    end;}
    {if Assigned(FStateCfg) then
    begin
      FStateCfg.Free;
      FStateCfg := nil;
    end;}
  end;
end;

procedure TsmxStateForm.SetIntfID(Value: Integer);
begin
  if IntfID <> Value then
  begin
    inherited SetIntfID(Value);
    StateCfg.IntfID := Value;
    RefreshStateCfg;
    RefreshStateID;
    {if Assigned(FStateCfg) then
    begin
      FStateCfg.IntfID := Value;
      RefreshStateCfg;
    end;}
    {if Assigned(FStateCfg) then
    begin
      FStateCfg.Free;
      FStateCfg := nil;
    end;}
  end;
end;

procedure TsmxStateForm.SetStateID(Value: Integer);
begin
  if FStateID <> Value then
  begin
    FStateID := Value;
    //PutState;
    RefreshStateID;
  end;
end;

procedure TsmxStateForm.SetStateRequest(Value: TsmxCustomRequest);
begin
  if FStateRequest <> Value then
  begin
    FStateRequest := Value;
    StateCfg.SelectRequest := Value;
    RefreshStateCfg;
    RefreshStateID;
    {if Assigned(FStateCfg) then
    begin
      FStateCfg.SelectRequest := Value;
      RefreshStateCfg;
    end;}
  end;
end;

{procedure TsmxStateForm.SetStateCfgClass(Value: TsmxStateCfgClass);
begin
  if FStateCfgClass <> Value then
  begin
    FStateCfgClass := Value;
    if Assigned(FStateCfg) then
    begin
      FStateCfg.Free;
      FStateCfg := nil;
    end;
  end;
end;}

{procedure TsmxStateForm.LockState;

  procedure LockCell(ACell: TsmxBaseCell);
  var
    i: Integer;
  begin
    if ACell is TsmxCustomAlgorithm then
      TsmxCustomAlgorithm(ACell).AlgorithmEnabled := False else
    if ACell is TsmxControlCell then
      TsmxControlCell(ACell).CellEnabled := False;
    for i := 0 to ACell.CellCount - 1 do
      LockCell(ACell.Cells[i]);
  end;

var
  i: Integer;
begin
  for i := 0 to CellCount -1 do
    LockCell(Cells[i]);
end;}

{ TsmxStandardFormCfg }

constructor TsmxStandardFormCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'PageControls';
  SlaveCells.ItemNodeName := 'PageControl';
  SlaveCells.IsWriteEmpty := True;
end;

destructor TsmxStandardFormCfg.Destroy;
begin
  if Assigned(FControlBoard) then
    FControlBoard.Free;
  if Assigned(FMainMenu) then
    FMainMenu.Free;
  if Assigned(FStatusBoard) then
    FStatusBoard.Free;
  inherited Destroy;
end;

procedure TsmxStandardFormCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStandardFormCfg then
  begin
    ControlBoard := TsmxStandardFormCfg(Source).ControlBoard;
    MainMenu := TsmxStandardFormCfg(Source).MainMenu;
    StatusBoard := TsmxStandardFormCfg(Source).StatusBoard;
  end;
end;

procedure TsmxStandardFormCfg.Clear;
begin
  inherited Clear;
  if Assigned(FControlBoard) then
    FControlBoard.Clear;
  if Assigned(FMainMenu) then
    FMainMenu.Clear;
  if Assigned(FStatusBoard) then
    FStatusBoard.Clear;
end;

function TsmxStandardFormCfg.GetControlBoard: TsmxControlKitItem;
begin
  if not Assigned(FControlBoard) then
    FControlBoard := TsmxControlKitItem.Create(nil);
  Result := FControlBoard;
end;

procedure TsmxStandardFormCfg.SetControlBoard(Value: TsmxControlKitItem);
begin
  ControlBoard.Assign(Value);
end;

function TsmxStandardFormCfg.GetMainMenu: TsmxControlKitItem;
begin
  if not Assigned(FMainMenu) then
    FMainMenu := TsmxControlKitItem.Create(nil);
  Result := FMainMenu;
end;

procedure TsmxStandardFormCfg.SetMainMenu(Value: TsmxControlKitItem);
begin
  MainMenu.Assign(Value);
end;

function TsmxStandardFormCfg.GetStatusBoard: TsmxControlKitItem;
begin
  if not Assigned(FStatusBoard) then
    FStatusBoard := TsmxControlKitItem.Create(nil);
  Result := FStatusBoard;
end;

procedure TsmxStandardFormCfg.SetStatusBoard(Value: TsmxControlKitItem);
begin
  StatusBoard.Assign(Value);
end;

procedure TsmxStandardFormCfg.ReadCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited ReadCell(Node);
  n := Node.ChildNodes.FindNode('ControlBoard');
  if Assigned(n) then
    ControlBoard.Read(n);
    {with ControlBoard do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}

  n := Node.ChildNodes.FindNode('MainMenu');
  if Assigned(n) then
    MainMenu.Read(n);
    {with MainMenu do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}

  n := Node.ChildNodes.FindNode('StatusBoard');
  if Assigned(n) then
    StatusBoard.Read(n);
    {with StatusBoard do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}
end;

procedure TsmxStandardFormCfg.WriteCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited WriteCell(Node);
  n := Node.AddChild('ControlBoard');
  ControlBoard.Write(n);
  {with ControlBoard do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}

  n := Node.AddChild('MainMenu');
  MainMenu.Write(n);
  {with MainMenu do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}

  n := Node.AddChild('StatusBoard');
  StatusBoard.Write(n);
  {with StatusBoard do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}
end;

{ TsmxStandardForm }

constructor TsmxStandardForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

function TsmxStandardForm.AddSlave: TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited AddSlave);
end;

function TsmxStandardForm.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxStandardFormCfg;
end;

function TsmxStandardForm.GetSlave(Index: Integer): TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited Slaves[Index]);
end;

procedure TsmxStandardForm.SetSlave(Index: Integer; Value: TsmxCustomPageManager);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxStandardForm.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxPageControl;
end;

{procedure TsmxStandardForm.InternalApply;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    smxClassProcs.AllCells(Self, List, [TsmxCustomFilterDesk]);
    for i := 0 to List.Count - 1 do
      TsmxCustomFilterDesk(List[i]).Apply;
  finally
    List.Free;
  end;
end;

procedure TsmxStandardForm.InternalPrepare;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    smxClassProcs.AllCells(Self, List, [TsmxCustomFilterDesk]);
    for i := 0 to List.Count - 1 do
      TsmxCustomFilterDesk(List[i]).Prepare;
  finally
    List.Free;
  end;
end;}

(*procedure TsmxStandardForm.InternalInitialize;

  function CreateControl(AItem: TsmxControlKitItem): TsmxControlCell;
  begin
    {Result := nil;
    if AItem.CfgID > 0 then
    begin}
      Result := TsmxControlCell(smxClassFuncs.NewCell(nil, AItem.CfgID, SelectRequest));
      Result.CellParent := Self;
      Result.Initialize;
      Result.CellActive := AItem.ItemActive;
      Result.CellAlign := AItem.ItemAlign;
      Result.CellAnchors := AItem.ItemAnchors;
      Result.CellCursor := AItem.ItemCursor;
      Result.CellEnabled := AItem.ItemEnabled;
      Result.CellHeight := AItem.ItemHeight;
      Result.CellLeft := AItem.ItemLeft;
      Result.CellTop := AItem.ItemTop;
      Result.CellVisible := AItem.ItemVisible;
      Result.CellWidth := AItem.ItemWidth;
      if Assigned(PopupList) then
        Result.PopupMenu := TsmxCustomPopupMenu(PopupList.FindSlaveByCfgID(AItem.PopupMenuCfgID));
    //end;
  end;

begin
  inherited InternalInitialize;
  if Cfg is TsmxStandardFormCfg then
  begin
    if TsmxStandardFormCfg(Cfg).ControlBoard.CfgID > 0 then
      ControlBoard := TsmxCustomControlBoard(CreateControl(TsmxStandardFormCfg(Cfg).ControlBoard));
    if TsmxStandardFormCfg(Cfg).MainMenu.CfgID > 0 then
      MainMenu := TsmxCustomMainMenu(CreateControl(TsmxStandardFormCfg(Cfg).MainMenu));
    if TsmxStandardFormCfg(Cfg).StatusBoard.CfgID > 0 then
      StatusBoard := TsmxCustomStatusBoard(CreateControl(TsmxStandardFormCfg(Cfg).StatusBoard));
  end;
end;*)

procedure TsmxStandardForm.ResetCellProps;
begin
  inherited ResetCellProps;
  ControlBoard := nil;
  MainMenu := nil;
  StatusBoard := nil;
end;

{procedure TsmxStandardForm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
end;}

procedure TsmxStandardForm.SetCellProps;

  function CreateControl(AItem: TsmxControlKitItem): TsmxControlCell;
  begin
    {Result := nil;
    if AItem.CfgID > 0 then
    begin}
      Result := TsmxControlCell(smxClassFuncs.NewCell(nil, AItem.CfgID, SelectRequest));
      Result.CellParent := Self;
      Result.Initialize;
      Result.CellActive := AItem.ItemActive;
      Result.CellAlign := AItem.ItemAlign;
      Result.CellAnchors := AItem.ItemAnchors;
      Result.CellCursor := AItem.ItemCursor;
      Result.CellEnabled := AItem.ItemEnabled;
      Result.CellHeight := AItem.ItemHeight;
      Result.CellLeft := AItem.ItemLeft;
      Result.CellTop := AItem.ItemTop;
      Result.CellVisible := AItem.ItemVisible;
      Result.CellWidth := AItem.ItemWidth;
      if Assigned(PopupList) then
        Result.PopupMenu := TsmxCustomPopupMenu(PopupList.FindSlaveByCfgID(AItem.PopupMenuCfgID));
    //end;
  end;

begin
  if Cfg is TsmxStandardFormCfg then
  begin
    if TsmxStandardFormCfg(Cfg).ControlBoard.CfgID > 0 then
      ControlBoard := TsmxCustomControlBoard(CreateControl(TsmxStandardFormCfg(Cfg).ControlBoard));
    if TsmxStandardFormCfg(Cfg).MainMenu.CfgID > 0 then
      MainMenu := TsmxCustomMainMenu(CreateControl(TsmxStandardFormCfg(Cfg).MainMenu));
    if TsmxStandardFormCfg(Cfg).StatusBoard.CfgID > 0 then
      StatusBoard := TsmxCustomStatusBoard(CreateControl(TsmxStandardFormCfg(Cfg).StatusBoard));
  end;
  inherited SetCellProps;
end;

procedure TsmxStandardForm.SetControlBoard(Value: TsmxCustomControlBoard);
begin
  FControlBoard := Value;
end;

procedure TsmxStandardForm.SetMainMenu(Value: TsmxCustomMainMenu);
begin
  FMainMenu := Value;
end;

{procedure TsmxStandardForm.SetRequestList(Value: TsmxCustomRequestList);
begin
  FRequestList := Value;
end;}

procedure TsmxStandardForm.SetStatusBoard(Value: TsmxCustomStatusBoard);
begin
  FStatusBoard := Value;
end;

initialization
  Classes.RegisterClasses([TsmxLibAlgorithmCfg, TsmxLibAction,
    TsmxRefRequestCfg, TsmxRefRequest, TsmxDBRequestCfg, TsmxDBRequest,
    TsmxLibRequestCfg, TsmxLibRequest, TsmxEditFilter{, TsmxDateTimeFilter,
    TsmxBitBtnFilter, TsmxNumEditFilter, TsmxLabelFilter}, TsmxStateFormCfg,
    TsmxStateForm, TsmxStandardFormCfg, TsmxStandardForm]);

{finalization
  UnRegisterClasses([TsmxEditFilter, TsmxDateTimeFilter, TsmxBitBtnFilter,
    TsmxNumEditFilter, TsmxLabelFilter]);}

end.
