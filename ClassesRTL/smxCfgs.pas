{**************************************}
{                                      }
{            SalesMan v1.0             }
{        Configuration classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxCfgs;

interface

uses
  Classes, TypInfo, XMLIntf, smxBaseClasses, smxClasses;

type
  { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxBaseCfg)
  private
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadType(const Node: IXMLNode); virtual;
    procedure SetCellClass(Value: TsmxBaseCellClass); virtual;
    procedure SetCellClassName(const Value: String); virtual;
    procedure SetCfgClass(Value: TsmxBaseCfgClass); virtual;
    procedure SetCfgClassName(const Value: String); virtual;
    procedure WriteType(const Node: IXMLNode); virtual;
  public
    procedure ClearCfg; override;
    procedure Read; override;
    procedure Write; override;

    property CellClass: TsmxBaseCellClass read FCellClass write SetCellClass;
    property CfgClass: TsmxBaseCfgClass read FCfgClass write SetCfgClass;
  published
    property CellClassName: String read FCellClassName write SetCellClassName;
    property CfgClassName: String read FCfgClassName write SetCfgClassName;
  end;

  { TsmxResolvedItem }

  TsmxResolvedKit = class;

  TsmxResolvedItem = class(TsmxKitItem)
  private
    FInstance: TObject;
    //FProcName: String;
    FPropInfo: PPropInfo;
    FPropValue: Variant;
    function GetKit: TsmxResolvedKit;
    procedure SetKit(Value: TsmxResolvedKit);
  protected
    procedure SetInstance(Value: TObject); virtual;
    //procedure SetProcName(const Value: String); virtual;
    procedure SetPropInfo(Value: PPropInfo); virtual;
    procedure SetPropValue(const Value: Variant); virtual;
  public
    procedure Assign(Source: TsmxKitItem); override;

    property Instance: TObject read FInstance write SetInstance;
    property Kit: TsmxResolvedKit read GetKit write SetKit;
    //property ProcName: String read FProcName write SetProcName;
    property PropInfo: PPropInfo read FPropInfo write SetPropInfo;
    property PropValue: Variant read FPropValue write SetPropValue;
  end;

  { TsmxResolvedKit }

  TsmxResolvedKit = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxResolvedItem;
    procedure SetItem(Index: Integer; Value: TsmxResolvedItem);
    //function GetValue(const Name: String): Variant;
    //procedure SetValue(const Name: String; const Value: Variant);
  public
    function Add: TsmxResolvedItem;
    function FindByCombo(Instance: TObject; PropInfo: PPropInfo): TsmxResolvedItem;

    property Items[Index: Integer]: TsmxResolvedItem read GetItem write SetItem; default;
    //property Values[const Name: String]: Variant read GetValue write SetValue;
  end;

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

  { TsmxCellfg }

  TsmxCellCfg = class(TsmxBaseCfg)
  private
    FCurNode: IXMLNode;
    FRefList: TList;
    FResolvedList: TsmxResolvedKit;
    function GetCellOwner: TsmxBaseCell;
    function GetResolvedList: TsmxResolvedKit;
    function GetRefList: TList;
  protected
    function GetCurNode: IXMLNode; virtual;
    procedure SetCurNode(const Value: IXMLNode); virtual;
    procedure SetRefList(Value: TList); virtual;
    procedure SetResolvedList(Value: TsmxResolvedKit); virtual;
  public
    destructor Destroy; override;
    procedure ClearCfg; override;
    procedure ClearXML; override;
    procedure Read; override;
    procedure ReadCell(Cell: TsmxBaseCell); virtual;
    procedure Write; override;
    procedure WriteCell(Cell: TsmxBaseCell); virtual;

    property CellOwner: TsmxBaseCell read GetCellOwner;
    property CurNode: IXMLNode read GetCurNode write SetCurNode;
    property RefList: TList read GetRefList write SetRefList;
    property ResolvedList: TsmxResolvedKit read GetResolvedList write SetResolvedList;
  end;

  { TsmxStateCfg }

  TsmxStateCfg = class(TsmxBaseCfg)
  private
    FCellStates: TsmxCellStates;
    FIntfID: Integer;
    FRecID: Integer;
    function GetCellStates: TsmxCellStates;
    procedure SetCellStates(Value: TsmxCellStates);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadIntf(const Node: IXMLNode; ID: Integer); virtual;
    procedure SetIntfID(Value: Integer); virtual;
    procedure SetRecID(Value: Integer); virtual;
    procedure WriteIntf(const Node: IXMLNode; ID: Integer); virtual;
  public
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
    procedure ClearCfg; override;
    procedure Load; override;
    procedure Read; override;
    procedure Remove; override;
    procedure Save; override;
    procedure Write; override;

    property CellStates: TsmxCellStates read GetCellStates write SetCellStates;
    property IntfID: Integer read FIntfID write SetIntfID;
    property RecID: Integer read FRecID write SetRecID;
  end;

  { TsmxSimpleStateCfg }

  {TsmxSimpleStateCfg = class(TsmxStateCfg)
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
    procedure ClearCfg; override;

    property CellStates: TsmxCellStates read GetCellStates write SetCellStates;
  end;}

implementation

uses
  Variants, SysUtils, smxClassProcs, smxDBFuncs, smxDBIntf, smxProcs, smxFuncs,
  smxConsts, smxTypes;

{ TsmxTypeCfg }

procedure TsmxTypeCfg.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TsmxTypeCfg then
  begin
    TsmxTypeCfg(Dest).CellClass := CellClass;
    TsmxTypeCfg(Dest).CfgClass := CfgClass;
  end;
end;

procedure TsmxTypeCfg.ClearCfg;
begin
  inherited ClearCfg;
  CellClassName := '';
  CfgClassName := '';
end;

procedure TsmxTypeCfg.Read;
var
  n: IXMLNode;
begin
  inherited Read;
  n := RootNode.ChildNodes.FindNode(smxConsts.cTypeNodeName);
  if Assigned(n) then
    ReadType(n);
end;

procedure TsmxTypeCfg.Write;
var
  n: IXMLNode;
begin
  inherited Write;
  n := RootNode.AddChild(smxConsts.cTypeNodeName);
  WriteType(n);
end;

procedure TsmxTypeCfg.ReadType(const Node: IXMLNode);
begin
  CellClassName := Node.Attributes['CellClassName'];
  CfgClassName := Node.Attributes['CfgClassName'];
end;

procedure TsmxTypeCfg.WriteType(const Node: IXMLNode);
begin
  Node.Attributes['CellClassName'] := CellClassName;
  Node.Attributes['CfgClassName'] := CfgClassName;
end;

procedure TsmxTypeCfg.SetCellClass(Value: TsmxBaseCellClass);
begin
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} Assigned(FCellClass) then
    FCellClassName := '';
  FCellClass := Value;
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} Assigned(FCellClass) then
    FCellClassName := smxFuncs.ResolvedClassType(FCellClass);
    //FCellClassName := smxProcs.gClassTypeManagerIntf.ResolvedClassType(FCellClass);
    //FCellClassName := FCellClass.ClassName;
end;

procedure TsmxTypeCfg.SetCellClassName(const Value: String);
begin
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} (FCellClassName <> '') then
    FCellClass := nil;
  FCellClassName := Value;
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} (FCellClassName <> '') then
    FCellClass := TsmxBaseCellClass(smxFuncs.ResolvedClassTypeName(FCellClassName, True));
    //FCellClass := TsmxBaseCellClass(smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(FCellClassName, True));
    //FCellClass := TsmxBaseCellClass(smxProcs.gClassTypeManagerIntf.FindByName(FCellClassName));
    //FCellClass := TsmxBaseCellClass(Classes.FindClass(FCellClassName));
end;

procedure TsmxTypeCfg.SetCfgClass(Value: TsmxBaseCfgClass);
begin
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} Assigned(FCfgClass) then
    FCfgClassName := '';
  FCfgClass := Value;
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} Assigned(FCfgClass) then
    FCfgClassName := smxFuncs.ResolvedClassType(FCfgClass);
    //FCfgClassName := smxProcs.gClassTypeManagerIntf.ResolvedClassType(FCfgClass);
    //FCfgClassName := FCfgClass.ClassName;
end;

procedure TsmxTypeCfg.SetCfgClassName(const Value: String);
begin
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} (FCfgClassName <> '') then
    FCfgClass := nil;
  FCfgClassName := Value;
  if {Assigned(smxProcs.gClassTypeManagerIntf) and} (FCfgClassName <> '') then
    FCfgClass := TsmxBaseCfgClass(smxFuncs.ResolvedClassTypeName(FCfgClassName, True));
    //FCfgClass := TsmxBaseCfgClass(smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(FCfgClassName, True));
    //FCfgClass := TsmxBaseCfgClass(smxProcs.gClassTypeManagerIntf.FindByName(FCfgClassName));
    //FCfgClass := TsmxBaseCfgClass(Classes.FindClass(FCfgClassName));
end;

{ TsmxResolvedItem }

procedure TsmxResolvedItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxResolvedItem then
  begin
    //Instance := TsmxResolvedItem(Source).Instance;
    //ProcName := TsmxResolvedItem(Source).ProcName;
    //PropInfo := TsmxResolvedItem(Source).PropInfo;
    PropValue := TsmxResolvedItem(Source).PropValue;
  end else
    inherited Assign(Source);
end;

function TsmxResolvedItem.GetKit: TsmxResolvedKit;
begin
  Result := TsmxResolvedKit(inherited Kit);
end;

procedure TsmxResolvedItem.SetKit(Value: TsmxResolvedKit);
begin
  inherited Kit := Value;
end;

procedure TsmxResolvedItem.SetInstance(Value: TObject);
begin
  FInstance := Value;
end;

{procedure TsmxResolvedItem.SetProcName(const Value: String);
begin
  FProcName := Value;
end;}

procedure TsmxResolvedItem.SetPropInfo(Value: PPropInfo);
begin
  FPropInfo := Value;
end;

procedure TsmxResolvedItem.SetPropValue(const Value: Variant);
begin
  FPropValue := Value;
end;

{ TsmxResolvedKit }

function TsmxResolvedKit.Add: TsmxResolvedItem;
begin
  Result := TsmxResolvedItem(inherited Add);
end;

function TsmxResolvedKit.FindByCombo(Instance: TObject; PropInfo: PPropInfo): TsmxResolvedItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i].Instance = Instance) and (Items[i].PropInfo = PropInfo) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxResolvedKit.GetItem(Index: Integer): TsmxResolvedItem;
begin
  Result := TsmxResolvedItem(inherited Items[Index]);
end;

procedure TsmxResolvedKit.SetItem(Index: Integer; Value: TsmxResolvedItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxCellCfg }

destructor TsmxCellCfg.Destroy;
begin
  SetCurNode(nil);
  if Assigned(FResolvedList) then
    FResolvedList.Free;
  if Assigned(FRefList) then
    FRefList.Free;
  inherited Destroy;
end;

procedure TsmxCellCfg.ClearCfg;
begin
  inherited ClearCfg;
  if Assigned(FRefList) then
    FRefList.Clear;
  if Assigned(FResolvedList) then
    FResolvedList.Clear;
end;

procedure TsmxCellCfg.ClearXML;
begin
  inherited ClearXML;
  CurNode := nil;
end;

function TsmxCellCfg.GetCellOwner: TsmxBaseCell;
begin
  if Owner is TsmxBaseCell then
    Result := TsmxBaseCell(Owner) else
    Result := nil;
end;

function TsmxCellCfg.GetCurNode: IXMLNode;
begin
  if not Assigned(FCurNode) then
    FCurNode := RootNode;
  Result := FCurNode;
end;

procedure TsmxCellCfg.SetCurNode(const Value: IXMLNode);
begin
  FCurNode := Value;
end;

function TsmxCellCfg.GetRefList: TList;
begin
  if not Assigned(FRefList) then
    FRefList := TList.Create;
  Result := FRefList;
end;

procedure TsmxCellCfg.SetRefList(Value: TList);
begin
  RefList.Assign(Value);
end;

function TsmxCellCfg.GetResolvedList: TsmxResolvedKit;
begin
  if not Assigned(FResolvedList) then
    FResolvedList := TsmxResolvedKit.Create(TsmxResolvedItem);
  Result := FResolvedList;
end;

procedure TsmxCellCfg.SetResolvedList(Value: TsmxResolvedKit);
begin
  ResolvedList.Assign(Value);
end;

procedure TsmxCellCfg.Read;
var
  n: IXMLNode;
begin
  inherited Read;
  if Assigned(CellOwner) then
  begin
    n := RootNode.ChildNodes.FindNode(smxConsts.cCellNodeName);
    if Assigned(n) then
    begin
      CurNode := n;
      ReadCell(CellOwner);
      //smxClassProcs.AllCells(CellOwner, FindList, []);
      smxClassProcs.RefList(CellOwner, RefList);
      smxClassProcs.ResolvedProps(ResolvedList, RefList);
    end;
  end;
end;

procedure TsmxCellCfg.Write;
var
  n: IXMLNode;
begin
  inherited Write;
  if Assigned(CellOwner) then
  begin
    RefList.Clear;
    //smxClassProcs.AllCells(CellOwner, FindList, []);
    smxClassProcs.RefList(CellOwner, RefList);
    n := RootNode.AddChild(smxConsts.cCellNodeName);
    CurNode := n;
    WriteCell(CellOwner);
  end;
end;

procedure TsmxCellCfg.ReadCell(Cell: TsmxBaseCell);
begin
  if Assigned(Cell) then
  begin
    smxClassProcs.ReadProps(CellOwner, Cell, CurNode, ResolvedList);
    Cell.SetProperties(Self);
  end;
end;

procedure TsmxCellCfg.WriteCell(Cell: TsmxBaseCell);
begin
  if Assigned(Cell) then
  begin
    smxClassProcs.WriteProps(CellOwner, Cell, CurNode, RefList);
    Cell.GetProperties(Self);
  end;
end;

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

{ TsmxStateCfg }

destructor TsmxStateCfg.Destroy;
begin
  if Assigned(FCellStates) then
    FCellStates.Free;
  inherited Destroy;
end;

{procedure TsmxStateCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateCfg then
  begin
    IntfID := TsmxStateCfg(Source).IntfID;
    RecID := TsmxStateCfg(Source).RecID;
  end;
end;}

procedure TsmxStateCfg.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TsmxStateCfg then
  begin
    TsmxStateCfg(Dest).IntfID := IntfID;
    //TsmxStateCfg(Dest).RecID := RecID;
    TsmxStateCfg(Dest).CellStates := CellStates;
  end;
end;

procedure TsmxStateCfg.ClearCfg;
begin
  inherited ClearCfg;
  if Assigned(FCellStates) then
    FCellStates.Clear;
end;

function TsmxStateCfg.GetCellStates: TsmxCellStates;
begin
  if not Assigned(FCellStates) then
    FCellStates := TsmxCellStates.Create(TsmxCellState);
  Result := FCellStates;
end;

procedure TsmxStateCfg.SetCellStates(Value: TsmxCellStates);
begin
  CellStates.Assign(Value);
end;

procedure TsmxStateCfg.Load;
var
  Key, Value: Variant;
  KeySense: TsmxDataSense;
begin
  if not Assigned(SelectDataSet)
      or not ((FRecID <> 0) or ((CfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, CfgID, 'load'], Self);
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([CfgID, FIntfID]);
    KeySense := dsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := dsKey;
  end;
  if smxDBFuncs.GetValueByKey(SelectDataSet, Key, Value,
      {FSelectRequest.PerformanceMode,} KeySense) then
  begin
    if smxDBFuncs.LocateByKey(SelectDataSet, Key, KeySense) then
    begin
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value,
          {FSelectRequest.PerformanceMode,} dsKey) then
        FRecID := Value else
        FRecID := 0;
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value{,
          FSelectRequest.PerformanceMode}) then
        XMLDoc.XML.Text := Value else
        XMLDoc.XML.Text := '';
      try
        XMLDoc.Active := True;
      except
        on E: Exception do
          raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
            [ClassName, CfgID, 'load', E.Message], Self);
      end;
    end;
  end;
end;

procedure TsmxStateCfg.Save;
var
  DataSet: IsmxDataSet;
  //Request: IsmxDataSet;
  //Performance: TsmxPerformanceMode;
  Key, Value: Variant;
  KeySense: TsmxDataSense;
begin
  if ((((FRecID = 0) and not Assigned(InsertDataSet))
        or ((FRecID <> 0) and not Assigned(UpdateDataSet)))
      or not ((FRecID <> 0) or ((CfgID <> 0) and (FIntfID <> 0)))) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, CfgID, 'save'], Self);
  if FRecID = 0 then
  begin
    DataSet := InsertDataSet; // ModifyRequests[mrInsert];
    //Performance := FSelectRequest.InsertPerformance; // ModifyPerformances[mrInsert];
    Key := Variants.VarArrayOf([CfgID, FIntfID]);
    KeySense := dsForeignKey;
  end else
  begin
    DataSet := UpdateDataSet; // ModifyRequests[mrUpdate];
    //Performance := FSelectRequest.UpdatePerformance; // ModifyPerformances[mrUpdate];
    Key := FRecID;
    KeySense := dsKey;
  end;
  {if not Assigned(Request) then
    raise EsmxCellError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);}
  {if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := fsKey;
  end;}
  if smxDBFuncs.SetValueByKey(DataSet, Key, XMLDoc.XML.Text, {Performance,} KeySense) then
    if smxDBFuncs.GetCurrentValue(DataSet, Value, {Performance,} dsKey) then
      FRecID := Value;
end;

procedure TsmxStateCfg.Read;
var
  Value: Variant;
  CurIntfID: Integer;
  XMLTextTemp: String;
begin
  inherited Read;
  XMLTextTemp := XMLDoc.XML.Text;
  if Assigned(SelectDataSet) then
  begin
    SelectDataSet.First;
    while not SelectDataSet.Eof do
    begin
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value{, FSelectRequest.PerformanceMode}) then
        XMLDoc.XML.Text := Value else
        XMLDoc.XML.Text := '';
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value, {FSelectRequest.PerformanceMode,} dsForeignKey) then
        CurIntfID := smxFuncs.GetSingleValue(Value, 0, 1) else
        CurIntfID := 0;
      try
        XMLDoc.Active := True;
        ReadIntf(RootNode, CurIntfID);
      except
        on E: Exception do
          raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
            [ClassName, CfgID, 'read', E.Message], Self);
      end;
      SelectDataSet.Next;
    end;
  end;
  XMLDoc.XML.Text := XMLTextTemp;
  try
    XMLDoc.Active := True;
    ReadIntf(RootNode, IntfID);
  except
    on E: Exception do
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, CfgID, 'read', E.Message], Self);
  end;
end;

procedure TsmxStateCfg.Write;
begin
  inherited Write;
  WriteIntf(RootNode, IntfID);
end;

procedure TsmxStateCfg.ReadIntf(const Node: IXMLNode; ID: Integer);

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
  //inherited ReadIntf(Node, ID);
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

procedure TsmxStateCfg.WriteIntf(const Node: IXMLNode; ID: Integer);

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
  //inherited WriteIntf(Node, ID);
  n := Node.AddChild('States');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('State');
    n2.Attributes['StateID'] := CellStates[i].StateID;
    for j := 0 to CellStates[i].StateKit.Root.Count - 1 do
      AddNodes(n2.AddChild('Cells'), CellStates[i].StateKit.Root[j]);
  end;
end;

procedure TsmxStateCfg.Remove;
var
  //Request: IsmxDataSet;
  //Performance: TsmxPerformanceMode;
  Key: Variant;
  KeySense: TsmxDataSense;
begin
  if not Assigned(DeleteDataSet)
      or not ((FRecID <> 0) or ((CfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, CfgID, 'remove'], Self);
  //Request := FSelectRequest.DeleteDataSet; // ModifyRequests[mrDelete];
  //Performance := FSelectRequest.DeletePerformance; // ModifyPerformances[mrDelete];
  {if not Assigned(Request) then
    raise EsmxCellError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);}
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([CfgID, FIntfID]);
    KeySense := dsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := dsKey;
  end;
  if smxDBFuncs.SetValueByKey(DeleteDataSet, Key, Variants.Null, KeySense) then
    FRecID := 0;
end;

procedure TsmxStateCfg.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

procedure TsmxStateCfg.SetRecID(Value: Integer);
begin
  FRecID := Value;
end;

{ TsmxSimpleStateCfg }

{destructor TsmxSimpleStateCfg.Destroy;
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

procedure TsmxSimpleStateCfg.ClearCfg;
begin
  inherited ClearCfg;
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
end;}

//initialization
  //smxProcs{Classes}.RegisterClasses([TsmxTypeCfg, TsmxCellCfg, TsmxStateCfg]);

//finalization
  //Classes.UnRegisterClasses([TsmxTypeCfg, TsmxCellCfg, TsmxStateCfg]);

end.
