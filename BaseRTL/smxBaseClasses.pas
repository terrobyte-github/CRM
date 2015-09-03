{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Base classes              }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxBaseClasses;

interface

uses
  Classes, SysUtils, XMLIntf, smxBaseIntf, smxDBIntf, smxTypes, smxClassIntf;

type
  { TsmxComponent }

  EsmxBaseError = class(Exception);

  TsmxComponent = class;

  EsmxComponentError = class(EsmxBaseError)
  private
    FComponent: TsmxComponent;
  public
    constructor CreateByComponent(Ident: Integer; const Args: array of const;
      Component: TsmxComponent); overload;
    constructor CreateByComponent(ResStringRec: PResStringRec; const Args: array of const;
      Component: TsmxComponent); overload;

    property Component: TsmxComponent read FComponent;
  end;

  TsmxComponent = class(TComponent, IsmxBaseInterface, IsmxRefComponent)
  protected
    function GetController: IsmxBaseInterface; virtual;
    function GetDescription: String; virtual;
    function GetInternalRef: Pointer; virtual;
    function GetReference: TComponent;
    function GetVersion: String; virtual;
    function IsCountedObj: Boolean; virtual;
  public
    function IsImplementedIntf(const Intf: IsmxBaseInterface): Boolean; virtual;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  published
    property Name stored True;
  end;

  TsmxComponentClass = class of TsmxComponent;

  { TsmxInterfacedComponent }

  TsmxInterfacedComponent = class(TsmxComponent, IInterface, IsmxBaseInterface)
  private
    FController: Pointer;
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetController: IsmxBaseInterface; override;
    function IsCountedObj: Boolean; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  public
    constructor Create(const AController: IsmxBaseInterface); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Controller: IsmxBaseInterface read GetController;
    property RefCount: Integer read FRefCount;
  end;

  TsmxInterfacedComponentClass = class of TsmxInterfacedComponent;

 { TsmxPersistent }

  TsmxPersistent = class(TPersistent, IInterface, IsmxBaseInterface, IsmxRefPersistent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetController: IsmxBaseInterface; virtual;
    function GetDescription: String; virtual;
    function GetInternalRef: Pointer; virtual;
    function GetReference: TPersistent;
    function GetVersion: String; virtual;
    function IsCountedObj: Boolean; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  public
    function IsImplementedIntf(const Intf: IsmxBaseInterface): Boolean; virtual;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  TsmxPersistentClass = class of TsmxPersistent;

  { TsmxInterfacedPersistent }

  TsmxInterfacedPersistent = class(TsmxPersistent, IInterface, IsmxBaseInterface)
  private
    FController: Pointer;
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetController: IsmxBaseInterface; override;
    function IsCountedObj: Boolean; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  public
    constructor Create(const AController: IsmxBaseInterface); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Controller: IsmxBaseInterface read GetController;
    property RefCount: Integer read FRefCount;
  end;

  TsmxInterfacedPersistentClass = class of TsmxInterfacedPersistent;

  { TsmxKitItem }

  EsmxListError = class(EsmxBaseError);

  TsmxKit = class;

  TsmxKitItem = class(TPersistent)
  private
    FKit: TsmxKit;
    procedure SetKit(Value: TsmxKit);
  protected
    function GetDisplayName: String; virtual;
    function GetDisplayObject: TObject; virtual;
    function GetIndex: Integer; virtual;
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(AKit: TsmxKit); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); reintroduce; virtual;

    property DisplayName: String read GetDisplayName;
    property DisplayObject: TObject read GetDisplayObject;
    property ItemIndex: Integer read GetIndex write SetIndex;
    property Kit: TsmxKit read FKit write SetKit;
  end;

  TsmxKitItemClass = class of TsmxKitItem;

  { TsmxKit }

  TsmxKit = class(TPersistent)
  private
    FKitItemClass: TsmxKitItemClass;
    FKitList: TList;
    FOnChange: TNotifyEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxKitItem;
    function GetKitList: TList;
    procedure SetItem(Index: Integer; Value: TsmxKitItem);
  protected
    procedure DoChange; virtual;
    procedure InternalChange; virtual;

    property KitList: TList read GetKitList;
  public
    constructor Create(AItemClass: TsmxKitItemClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKit); reintroduce; virtual;
    function Add: TsmxKitItem;
    procedure Change;
    procedure Clear;
    procedure Delete(Index: Integer);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem write SetItem; default;
    property KitItemClass: TsmxKitItemClass read FKitItemClass;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TsmxKitClass = class of TsmxKit;

  { TsmxHKitItem }

  TsmxHKit = class;

  TsmxHKitItem = class(TPersistent)
  private
    FHKit: TsmxHKit;
    FHKitItemList: TList;
    FParent: TsmxHKitItem;
    function GetCount: Integer;
    function GetHKitItemList: TList;
    function GetItem(Index: Integer): TsmxHKitItem;
    procedure SetItem(Index: Integer; Value: TsmxHKitItem);
    procedure SetParent(Value: TsmxHKitItem);
  protected
    function GetIndex: Integer; virtual;
    procedure SetIndex(Value: Integer); virtual;

    property HKitItemList: TList read GetHKitItemList;
  public
    constructor Create(AHKit: TsmxHKit); virtual;
    destructor Destroy; override;
    function Add: TsmxHKitItem;
    procedure Assign(Source: TsmxHKitItem); reintroduce; virtual;
    procedure Clear;
    procedure Delete(Index: Integer);
    function HasChilds: Boolean;
    function HasParent: Boolean;

    property Count: Integer read GetCount;
    property HKit: TsmxHKit read FHKit;
    property ItemIndex: Integer read GetIndex write SetIndex;
    property Items[Index: Integer]: TsmxHKitItem read GetItem write SetItem; default;
    property Parent: TsmxHKitItem read FParent write SetParent;
  end;

  TsmxHKitItemClass = class of TsmxHKitItem;

  { TsmxHKit }

  TsmxHKit = class(TPersistent)
  private
    FHKitItemClass: TsmxHKitItemClass;
    FRoot: TsmxHKitItem;
    FOnChange: TNotifyEvent;
    function GetRoot: TsmxHKitItem;
    procedure SetRoot(Value: TsmxHKitItem);
  protected
    procedure DoChange; virtual;
    procedure InternalChange; virtual;
  public
    constructor Create(AItemClass: TsmxHKitItemClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxHKit); reintroduce; virtual;
    procedure Change;

    property HKitItemClass: TsmxHKitItemClass read FHKitItemClass;
    property Root: TsmxHKitItem read GetRoot write SetRoot;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TsmxHKitClass = class of TsmxHKit;

  { TsmxParam }

  TsmxParams = class;

  TsmxParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
    function GetKit: TsmxParams;
    procedure SetKit(Value: TsmxParams);
  protected
    function GetParamValue: Variant; virtual;
    procedure SetParamName(const Value: String); virtual;
    procedure SetParamValue(const Value: Variant); virtual;
  public
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxParams read GetKit write SetKit;
    property ParamName: String read FParamName write SetParamName;
    property ParamValue: Variant read GetParamValue write SetParamValue;
  end;

  { TsmxParams }

  TsmxParams = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxParam;
    procedure SetItem(Index: Integer; Value: TsmxParam);
  public
    function Add: TsmxParam;
    function FindByName(const ParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem write SetItem; default;
  end;

  { EsmxCfgError }

  EsmxCfgError = class(EsmxComponentError)
  protected
    function GetCfgID: Integer; virtual;
  public
    property CfgID: Integer read GetCfgID;
  end;

  { TsmxBaseCfg }

  TsmxBaseCfg = class(TsmxComponent)
  private
    FCfgID: Integer;
    FDeleteDataSetIntf: IsmxDataSet;
    FInsertDataSetIntf: IsmxDataSet;
    FSelectDataSetIntf: IsmxDataSet;
    FUpdateDataSetIntf: IsmxDataSet;
    FXMLDocIntf: IXMLDocument;
    function GetRootNode: IXMLNode;
    function GetXMLDoc: IXMLDocument;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetXMLText: String; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetDataSet(Index: TsmxRequestType; const Value: IsmxDataSet); virtual;
    procedure SetXMLText(const Value: String); virtual;

    property RootNode: IXMLNode read GetRootNode;
    property XMLDoc: IXMLDocument read GetXMLDoc;
  public
    destructor Destroy; override;
    procedure ClearCfg; virtual;
    procedure ClearXML; virtual;
    procedure Load; virtual;
    procedure Read; virtual;
    procedure Remove; virtual;
    procedure Save; virtual;
    procedure Write; virtual;

    property CfgID: Integer read FCfgID write SetCfgID;
    property DeleteDataSet: IsmxDataSet index rtDelete read FDeleteDataSetIntf write SetDataSet;
    property InsertDataSet: IsmxDataSet index rtInsert read FInsertDataSetIntf write SetDataSet;
    property SelectDataSet: IsmxDataSet index rtSelect read FSelectDataSetIntf write SetDataSet;
    property UpdateDataSet: IsmxDataSet index rtUpdate read FUpdateDataSetIntf write SetDataSet;
    property XMLText: String read GetXMLText write SetXMLText;
  end;

  TsmxBaseCfgClass = class of TsmxBaseCfg;

  { TsmxObjectItem }

  TsmxObjectList = class;

  TsmxObjectItem = class(TsmxKitItem)
  private
    function GetKit: TsmxObjectList;
    procedure SetKit(Value: TsmxObjectList);
  protected
    FObjectItem: TPersistent;
    FObjectItemIntf: Pointer;
    procedure AddObject; overload;
    procedure AddObject(ObjectClass: TPersistentClass); overload;
    procedure DelObject;
    function GetDisplayName: String; override;
    function GetDisplayObject: TObject; override;
    procedure SetIndex(Value: Integer); override;
    procedure SetObjectItem(Value: TPersistent);
  public
    constructor Create(AKit: TsmxKit); overload; override;
    constructor Create(AKit: TsmxKit; AObjectClass: TPersistentClass); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxObjectList read GetKit write SetKit;
    property ObjectItem: TPersistent read FObjectItem write SetObjectItem;
  end;

  TsmxObjectItemClass = class of TsmxObjectItem;

  { TsmxObjectList }

  TsmxObjectList = class(TsmxKit)
  private
    FOwner: TPersistent;
    FOwnerIntf: Pointer;
    function GetItem(Index: Integer): TsmxObjectItem;
    procedure SetItem(Index: Integer; Value: TsmxObjectItem);
  public
    constructor Create(AOwner: TPersistent; AItemClass: TsmxKitItemClass); reintroduce; overload; virtual;
    function Add: TsmxObjectItem; overload;
    function Add(ObjectClass: TPersistentClass): TsmxObjectItem; overload;
    procedure Assign(Source: TsmxKit); override;

    property Items[Index: Integer]: TsmxObjectItem read GetItem write SetItem; default;
    property Owner: TPersistent read FOwner;
  end;

  TsmxObjectListClass = class of TsmxObjectList;

implementation

uses
  Windows, Variants, smxFuncs, smxDBFuncs, smxConsts;

{$I ..\Resource\smxVers.inc}

{ EsmxComponentError }

constructor EsmxComponentError.CreateByComponent(Ident: Integer;
  const Args: array of const; Component: TsmxComponent);
begin
  CreateResFmt(Ident, Args);
  FComponent := Component;
end;

constructor EsmxComponentError.CreateByComponent(ResStringRec: PResStringRec;
  const Args: array of const; Component: TsmxComponent);
begin
  CreateResFmt(ResStringRec, Args);
  FComponent := Component;
end;

{ EsmxCfgError }

function EsmxCfgError.GetCfgID: Integer;
begin
  if FComponent is TsmxBaseCfg then
    Result := TsmxBaseCfg(FComponent).CfgID
  else
    Result := 0;
end;

{ TsmxComponent }

function TsmxComponent.GetController: IsmxBaseInterface;
begin
  Result := nil;
end;

function TsmxComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxComponent.GetInternalRef: Pointer;
begin
  Result := nil;
end;

function TsmxComponent.GetReference: TComponent;
begin
  Result := Self;
end;

function TsmxComponent.GetVersion: String;
begin
  Result := cVersion;
end;

function TsmxComponent.IsCountedObj: Boolean;
begin
  Result := False;
end;

function TsmxComponent.IsImplementedIntf(const Intf: IsmxBaseInterface): Boolean;
var
  AIntf: IsmxRefComponent;
begin
  Result := Assigned(Intf)
    and SysUtils.Supports(Intf, IsmxRefComponent, AIntf)
    and (AIntf.GetReference = Self);
end;

{ TsmxInterfacedComponent }

constructor TsmxInterfacedComponent.Create(const AController: IsmxBaseInterface);
begin
  inherited Create(nil);
  FController := Pointer(AController);
end;

function TsmxInterfacedComponent._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface(FController)._AddRef
  else
    Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedComponent._Release: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface(FController)._Release
  else
  begin
    Result := Windows.InterlockedDecrement(FRefCount);
    if Result = 0 then
      Destroy;
  end;
end;

procedure TsmxInterfacedComponent.AfterConstruction;
begin
  Windows.InterlockedDecrement(FRefCount);
end;

procedure TsmxInterfacedComponent.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
  inherited BeforeDestruction;
end;

function TsmxInterfacedComponent.GetController: IsmxBaseInterface;
begin
  Result := IsmxBaseInterface(FController);
end;

function TsmxInterfacedComponent.IsCountedObj: Boolean;
begin
  Result := True;
end;

class function TsmxInterfacedComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedComponent(Result).FRefCount := 1;
end;

function TsmxInterfacedComponent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface(FController).QueryInterface(IID, Obj)
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{ TsmxInterfacedPersistent }

constructor TsmxInterfacedPersistent.Create(const AController: IsmxBaseInterface);
begin
  FController := Pointer(AController);
end;

function TsmxInterfacedPersistent._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface(FController)._AddRef
  else
    Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedPersistent._Release: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface(FController)._Release
  else
  begin
    Result := Windows.InterlockedDecrement(FRefCount);
    if Result = 0 then
      Destroy;
  end;
end;

procedure TsmxInterfacedPersistent.AfterConstruction;
begin
  Windows.InterlockedDecrement(FRefCount);
end;

procedure TsmxInterfacedPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

function TsmxInterfacedPersistent.GetController: IsmxBaseInterface;
begin
  Result := IsmxBaseInterface(FController);
end;

function TsmxInterfacedPersistent.IsCountedObj: Boolean;
begin
  Result := True;
end;

class function TsmxInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedPersistent(Result).FRefCount := 1;
end;

function TsmxInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface(FController).QueryInterface(IID, Obj)
  else
    Result := inherited QueryInterface(IID, Obj);
end;

{ TsmxKitItem }

constructor TsmxKitItem.Create(AKit: TsmxKit);
begin
  FKit := AKit;
end;

destructor TsmxKitItem.Destroy;
begin
  SetKit(nil);
  inherited Destroy;
end;

procedure TsmxKitItem.Assign(Source: TsmxKitItem);
var
  Name: String;
begin
  if Assigned(Source) then
    Name := Source.ClassName
  else
    Name := 'nil';
  raise EsmxListError.CreateResFmt(@smxConsts.rsAssignError, [Name, ClassName]);
end;

function TsmxKitItem.GetDisplayName: String;
begin
  if Assigned(FKit) then
    Result := Format('%s_%d', [ClassName, ItemIndex])
  else
    Result := ClassName;
end;

function TsmxKitItem.GetDisplayObject: TObject;
begin
  Result := Self;
end;

function TsmxKitItem.GetIndex: Integer;
begin
  if Assigned(FKit) then
    Result := FKit.KitList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TsmxKitItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) and Assigned(FKit) then
  begin
    FKit.KitList.Move(CurIndex, Value);
    FKit.Change;
  end;
end;

procedure TsmxKitItem.SetKit(Value: TsmxKit);
begin
  if Assigned(FKit) then
  begin
    FKit.KitList.Remove(Self);
    FKit.Change;
  end;
  FKit := Value;
  if Assigned(FKit) then
  begin
    FKit.KitList.Add(Self);
    FKit.Change;
  end;
end;

{ TsmxKit }

constructor TsmxKit.Create(AItemClass: TsmxKitItemClass);
begin
  FKitItemClass := AItemClass;
end;

destructor TsmxKit.Destroy;
begin
  if Assigned(FKitList) then
  begin
    Clear;
    FKitList.Free;
  end;
  inherited Destroy;
end;

function TsmxKit.Add: TsmxKitItem;
begin
  Result := FKitItemClass.Create(Self);
  KitList.Add(Result);
  Change;
end;

procedure TsmxKit.Assign(Source: TsmxKit);
var
  i: Integer;
begin
  Clear;
  if Assigned(Source) then
    for i := 0 to Source.Count - 1 do
      Add.Assign(Source[i]);
end;

procedure TsmxKit.Clear;
var
  i: Integer;
begin
  for i := KitList.Count - 1 downto 0 do
    TsmxKitItem(KitList[i]).Free;
end;

procedure TsmxKit.Delete(Index: Integer);
begin
  TsmxKitItem(KitList[Index]).Free;
end;

procedure TsmxKit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TsmxKit.InternalChange;
begin
end;

procedure TsmxKit.Change;
begin
  InternalChange;
  DoChange;
end;

function TsmxKit.GetCount: Integer;
begin
  Result := KitList.Count;
end;

function TsmxKit.GetItem(Index: Integer): TsmxKitItem;
begin
  Result := KitList[Index];
end;

procedure TsmxKit.SetItem(Index: Integer; Value: TsmxKitItem);
begin
  TsmxKitItem(KitList[Index]).Assign(Value);
end;

function TsmxKit.GetKitList: TList;
begin
  if not Assigned(FKitList) then
    FKitList := TList.Create;
  Result := FKitList;
end;

{ TsmxHKitItem }

constructor TsmxHKitItem.Create(AHKit: TsmxHKit);
begin
  FHKit := AHKit;
end;

destructor TsmxHKitItem.Destroy;
begin
  SetParent(nil);
  if Assigned(FHKitItemList) then
  begin
    Clear;
    FHKitItemList.Free;
  end;
  inherited Destroy;
end;

function TsmxHKitItem.Add: TsmxHKitItem;
begin
  Result := FHKit.FHKitItemClass.Create(FHKit);
  Result.FParent := Self;
  HKitItemList.Add(Result);
  if Assigned(FHKit) then
    FHKit.Change;
end;

procedure TsmxHKitItem.Assign(Source: TsmxHKitItem);
var
  i: Integer;
begin
  Clear;
  if Assigned(Source) then
    for i := 0 to Source.Count - 1 do
      Add.Assign(Source[i]);
end;

procedure TsmxHKitItem.Clear;
var
  i: Integer;
begin
  for i := HKitItemList.Count - 1 downto 0 do
    TsmxHKitItem(HKitItemList[i]).Free;
end;

procedure TsmxHKitItem.Delete(Index: Integer);
begin
  TsmxHKitItem(HKitItemList[Index]).Free;
end;

function TsmxHKitItem.GetCount: Integer;
begin
  Result := HKitItemList.Count;
end;

function TsmxHKitItem.GetHKitItemList: TList;
begin
  if not Assigned(FHKitItemList) then
    FHKitItemList := TList.Create;
  Result := FHKitItemList;
end;

function TsmxHKitItem.GetIndex: Integer;
begin
  if Assigned(FParent) then
    Result := FParent.HKitItemList.IndexOf(Self) else
    Result := -1;
end;

procedure TsmxHKitItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) and Assigned(FParent) then
  begin
    FParent.HKitItemList.Move(CurIndex, Value);
    if Assigned(FHKit) then
      FHKit.Change;
  end;
end;

function TsmxHKitItem.GetItem(Index: Integer): TsmxHKitItem;
begin
  Result := HKitItemList[Index];
end;

procedure TsmxHKitItem.SetItem(Index: Integer; Value: TsmxHKitItem);
begin
  TsmxHKitItem(HKitItemList[Index]).Assign(Value);
end;

function TsmxHKitItem.HasChilds: Boolean;
begin
  Result := HKitItemList.Count <> 0;
end;

function TsmxHKitItem.HasParent: Boolean;
begin
  Result := Assigned(FParent) and (FParent <> FHKit.Root);
end;

procedure TsmxHKitItem.SetParent(Value: TsmxHKitItem);
begin
  if Assigned(FParent) then
  begin
    FParent.HKitItemList.Remove(Self);
    FHKit := nil;
    if Assigned(FParent.FHKit) then
      FParent.FHKit.Change;
  end;
  FParent := Value;
  if Assigned(FParent) then
  begin
    FParent.HKitItemList.Add(Self);
    FHKit := FParent.FHKit;
    if Assigned(FParent.FHKit) then
      FParent.FHKit.Change;
  end;
end;

{ TsmxHKit }

constructor TsmxHKit.Create(AItemClass: TsmxHKitItemClass);
begin
  FHKitItemClass := AItemClass;
end;

destructor TsmxHKit.Destroy;
begin
  if Assigned(FRoot) then
    FRoot.Free;
  inherited Destroy;
end;

procedure TsmxHKit.Assign(Source: TsmxHKit);
begin
  Root.Clear;
  if Assigned(Source) then
    Root.Assign(Source.Root);
end;

procedure TsmxHKit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TsmxHKit.InternalChange;
begin
end;

procedure TsmxHKit.Change;
begin
  InternalChange;
  DoChange;
end;

function TsmxHKit.GetRoot: TsmxHKitItem;
begin
  if not Assigned(FRoot) then
    FRoot := FHKitItemClass.Create(Self);
  Result := FRoot;
end;

procedure TsmxHKit.SetRoot(Value: TsmxHKitItem);
begin
  Root.Assign(Value);
end;

{ TsmxParam }

procedure TsmxParam.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxParam then
  begin
    ParamName := TsmxParam(Source).ParamName;
    ParamValue := TsmxParam(Source).ParamValue;
  end else
    inherited Assign(Source);
end;

function TsmxParam.GetKit: TsmxParams;
begin
  Result := TsmxParams(inherited Kit);
end;

procedure TsmxParam.SetKit(Value: TsmxParams);
begin
  inherited Kit := Value;
end;

function TsmxParam.GetParamValue: Variant;
begin
  if Variants.VarIsClear(FParamValue) then
    FParamValue := Variants.Null;
  Result := FParamValue;
end;

procedure TsmxParam.SetParamValue(const Value: Variant);
begin
  FParamValue := Value;
end;

procedure TsmxParam.SetParamName(const Value: String);
begin
  FParamName := Value;
end;

{ TsmxParams }

function TsmxParams.Add: TsmxParam;
begin
  Result := TsmxParam(inherited Add);
end;

function TsmxParams.FindByName(const ParamName: String): TsmxParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, ParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxParams.GetItem(Index: Integer): TsmxParam;
begin
  Result := TsmxParam(inherited Items[Index]);
end;

procedure TsmxParams.SetItem(Index: Integer; Value: TsmxParam);
begin
  inherited Items[Index] := Value;
end;

{ TsmxPersistent }

function TsmxPersistent._AddRef: Integer;
begin
  Result := -1;
end;

function TsmxPersistent._Release: Integer;
begin
  Result := -1;
end;

function TsmxPersistent.GetController: IsmxBaseInterface;
begin
  Result := nil;
end;

function TsmxPersistent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxPersistent.GetInternalRef: Pointer;
begin
  Result := nil;
end;

function TsmxPersistent.GetReference: TPersistent;
begin
  Result := Self;
end;

function TsmxPersistent.GetVersion: String;
begin
  Result := cVersion;
end;

function TsmxPersistent.IsImplementedIntf(const Intf: IsmxBaseInterface): Boolean;
var
  AIntf: IsmxRefPersistent;
begin
  Result := Assigned(Intf)
    and SysUtils.Supports(Intf, IsmxRefPersistent, AIntf)
    and (AIntf.GetReference = Self);
end;

function TsmxPersistent.IsCountedObj: Boolean;
begin
  Result := False;
end;

function TsmxPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TsmxBaseCfg }

destructor TsmxBaseCfg.Destroy;
begin
  FXMLDocIntf := nil;
  SetDataSet(rtSelect, nil);
  SetDataSet(rtDelete, nil);
  SetDataSet(rtInsert, nil);
  SetDataSet(rtUpdate, nil);
  inherited Destroy;
end;

procedure TsmxBaseCfg.AssignTo(Dest: TPersistent);
begin
  if Dest is TsmxBaseCfg then
    TsmxBaseCfg(Dest).CfgID := CfgID
  else
    inherited AssignTo(Dest);
end;

procedure TsmxBaseCfg.ClearCfg;
begin
end;

procedure TsmxBaseCfg.ClearXML;
begin
  RootNode.ChildNodes.Clear;
  RootNode.AttributeNodes.Clear;
end;

function TsmxBaseCfg.GetRootNode: IXMLNode;
begin
  XMLDoc.Active := True;
  Result := XMLDoc.ChildNodes.FindNode(smxConsts.cRootNodeName);
  if not Assigned(Result) then
    Result := XMLDoc.AddChild(smxConsts.cRootNodeName);
end;

function TsmxBaseCfg.GetXMLDoc: IXMLDocument;
begin
  if not Assigned(FXMLDocIntf) then
    FXMLDocIntf := smxFuncs.NewXMLDoc;
  Result := FXMLDocIntf;
end;

function TsmxBaseCfg.GetXMLText: String;
begin
  try
    XMLDoc.Active := True;
    Result := smxFuncs.UnFormatXMLText(XMLDoc.XML.Text);
  except
    on E: Exception do
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, FCfgID, 'write', E.Message], Self);
  end;
end;

procedure TsmxBaseCfg.SetXMLText(const Value: String);
begin
  try
    XMLDoc.XML.Text := smxFuncs.FormatXMLText(Value);
    XMLDoc.Active := True;
  except
    on E: Exception do
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, FCfgID, 'load', E.Message], Self);
  end;
end;

procedure TsmxBaseCfg.Load;
var
  Value: Variant;
begin
  if not Assigned(FSelectDataSetIntf) or (FCfgID = 0) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'load'], Self);
  if smxDBFuncs.GetValueByKey(FSelectDataSetIntf, FCfgID, Value) then
    XMLDoc.XML.Text := Value
  else
    XMLDoc.XML.Text := '';
  try
    XMLDoc.Active := True;
  except
    on E: Exception do
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, FCfgID, 'load', E.Message], Self);
  end;
end;

procedure TsmxBaseCfg.Save;
var
  DataSet: IsmxDataSet;
  Key: Variant;
  InTransaction: Boolean;
begin
  if ((FCfgID = 0) and not Assigned(FInsertDataSetIntf))
      or ((FCfgID <> 0) and not Assigned(FUpdateDataSetIntf)) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], Self);
  try
    XMLDoc.Active := True;
  except
    on E: Exception do
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, FCfgID, 'save', E.Message], Self);
  end;
  if FCfgID = 0 then
  begin
    DataSet := FInsertDataSetIntf;
    Key := Variants.Null;
  end else
  begin
    DataSet := FUpdateDataSetIntf;
    Key := FCfgID;
  end;
  InTransaction := DataSet.Database.InTransaction;
  if not InTransaction then
    DataSet.Database.StartTransaction;
  try
    if smxDBFuncs.SetValueByKey(DataSet, Key, XMLDoc.XML.Text) then
    begin
      FCfgID := Key;
      if not InTransaction then
        DataSet.Database.CommitTransaction;
    end else
    begin
      if not InTransaction then
        DataSet.Database.RollbackTransaction;
    end;
  except
    on E: Exception do
    begin
      if not InTransaction then
        DataSet.Database.RollbackTransaction;
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, FCfgID, 'save', E.Message], Self);
    end;
  end;
end;

procedure TsmxBaseCfg.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(SelectDataSet) and not SelectDataSet.IsCountedObj and (AComponent = SelectDataSet.GetReference) then
      SelectDataSet := nil;
    if Assigned(DeleteDataSet) and not DeleteDataSet.IsCountedObj and (AComponent = DeleteDataSet.GetReference) then
      DeleteDataSet := nil else
    if Assigned(InsertDataSet) and not InsertDataSet.IsCountedObj and (AComponent = InsertDataSet.GetReference) then
      InsertDataSet := nil else
    if Assigned(UpdateDataSet) and not UpdateDataSet.IsCountedObj and (AComponent = UpdateDataSet.GetReference) then
      UpdateDataSet := nil;
  end;
end;

procedure TsmxBaseCfg.Read;
begin
  ClearCfg;
end;

procedure TsmxBaseCfg.Write;
begin
  ClearXML;
end;

procedure TsmxBaseCfg.Remove;
var
  Key: Variant;
  InTransaction: Boolean;
begin
  if not Assigned(FDeleteDataSetIntf) or (FCfgID = 0) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], Self);
  Key := FCfgID;
  InTransaction := FDeleteDataSetIntf.Database.InTransaction;
  if not InTransaction then
    FDeleteDataSetIntf.Database.StartTransaction;
  try
    if smxDBFuncs.SetValueByKey(FDeleteDataSetIntf, Key, Variants.Null) then
    begin
      FCfgID := 0;
      XMLDoc.XML.Text := smxConsts.cXMLDocTextDef;
      if not InTransaction then
        FDeleteDataSetIntf.Database.CommitTransaction;
    end else
    begin
      if not InTransaction then
        FDeleteDataSetIntf.Database.RollbackTransaction;
    end;
  except
    on E: Exception do
    begin
      if not InTransaction then
        FDeleteDataSetIntf.Database.RollbackTransaction;
      raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, FCfgID, 'remove', E.Message], Self);
    end;
  end;
end;

procedure TsmxBaseCfg.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

procedure TsmxBaseCfg.SetDataSet(Index: TsmxRequestType; const Value: IsmxDataSet);
begin
  case Index of
    rtSelect: FSelectDataSetIntf := Value;
    rtDelete: FDeleteDataSetIntf := Value;
    rtInsert: FInsertDataSetIntf := Value;
    rtUpdate: FUpdateDataSetIntf := Value;
  end;
  if Assigned(Value) and not Value.IsCountedObj then
    Value.GetReference.FreeNotification(Self);
end;

{ TsmxObjectItem }

constructor TsmxObjectItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  AddObject;
end;

constructor TsmxObjectItem.Create(AKit: TsmxKit; AObjectClass: TPersistentClass);
begin
  inherited Create(AKit);
  AddObject(AObjectClass);
end;

destructor TsmxObjectItem.Destroy;
begin
  DelObject;
  inherited Destroy;
end;

procedure TsmxObjectItem.AddObject;
var
  ObjectItemIntf: IsmxObjectItem;
begin
  if Assigned(Kit) then
    if Assigned(Kit.Owner) then
    begin
      IsmxObjectList(Kit.FOwnerIntf).CreateObject(Self);
      if Assigned(FObjectItem) then
        if FObjectItem.GetInterface(IsmxObjectItem, ObjectItemIntf) then
        begin
          FObjectItemIntf := Pointer(ObjectItemIntf);
          ObjectItemIntf.ChangeObjectOwner(Kit.Owner);
        end else
          raise EsmxListError.CreateResFmt(@smxConsts.rsListActionError, [ClassName, 'create']);
    end;
end;

procedure TsmxObjectItem.AddObject(ObjectClass: TPersistentClass);
var
  ObjectItemIntf: IsmxObjectItem;
begin
  if Assigned(Kit) then
    if Assigned(Kit.Owner) then
    begin
      IsmxObjectList(Kit.FOwnerIntf).CreateObject(Self, ObjectClass);
      if Assigned(FObjectItem) then
        if FObjectItem.GetInterface(IsmxObjectItem, ObjectItemIntf) then
        begin
          FObjectItemIntf := Pointer(ObjectItemIntf);
          ObjectItemIntf.ChangeObjectOwner(Kit.Owner);
        end else
          raise EsmxListError.CreateResFmt(@smxConsts.rsListActionError, [ClassName, 'create']);;
    end;
end;

procedure TsmxObjectItem.DelObject;
begin
  if Assigned(Kit) then
    if Assigned(Kit.Owner) then
    begin
      if Assigned(FObjectItem) and Assigned(FObjectItemIntf) then
        IsmxObjectItem(FObjectItemIntf).ChangeObjectOwner(nil);
      IsmxObjectList(Kit.FOwnerIntf).DestroyObject(Self);
    end;
end;

procedure TsmxObjectItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxObjectItem then
    ObjectItem := TsmxObjectItem(Source).ObjectItem
  else
    inherited Assign(Source);
end;

function TsmxObjectItem.GetDisplayName: String;
begin
  if Assigned(ObjectItem) then
  begin
    if Assigned(Kit) then
      Result := Format('%s_%d', [ObjectItem.ClassName, ItemIndex])
    else
      Result := ObjectItem.ClassName;
  end else
    Result := inherited GetDisplayName;
end;

function TsmxObjectItem.GetDisplayObject: TObject;
begin
  if Assigned(ObjectItem) then
    Result := ObjectItem
  else
    Result := inherited GetDisplayObject;
end;

function TsmxObjectItem.GetKit: TsmxObjectList;
begin
  Result := TsmxObjectList(inherited Kit);
end;

procedure TsmxObjectItem.SetKit(Value: TsmxObjectList);
begin
  if Assigned(Kit) then
    if Assigned(Kit.Owner) then
      if Assigned(ObjectItem) and Assigned(FObjectItemIntf) then
        IsmxObjectItem(FObjectItemIntf).ChangeObjectOwner(nil);
  inherited Kit := Value;
  if Assigned(Kit) then
    if Assigned(Kit.Owner) then
      if Assigned(ObjectItem) and Assigned(FObjectItemIntf) then
        IsmxObjectItem(FObjectItemIntf).ChangeObjectOwner(Kit.Owner);
end;

procedure TsmxObjectItem.SetObjectItem(Value: TPersistent);
begin
  if Assigned(FObjectItem) then
    FObjectItem.Assign(Value);
end;

procedure TsmxObjectItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  if Assigned(ObjectItem) and Assigned(FObjectItemIntf) then
    IsmxObjectItem(FObjectItemIntf).ChangeObjectIndex(Value);
end;

{ TsmxObjectList }

constructor TsmxObjectList.Create(AOwner: TPersistent; AItemClass: TsmxKitItemClass);
var
  ObjectListIntf: IsmxObjectList;
begin
  Create(AItemClass);
  FOwner := AOwner;
  if Assigned(FOwner) then
    if FOwner.GetInterface(IsmxObjectList, ObjectListIntf) then
      FOwnerIntf := Pointer(ObjectListIntf);
end;

function TsmxObjectList.Add: TsmxObjectItem;
begin
  Result := TsmxObjectItem(inherited Add);
end;

function TsmxObjectList.Add(ObjectClass: TPersistentClass): TsmxObjectItem;
begin
  Result := TsmxObjectItemClass(KitItemClass).Create(Self, ObjectClass);
  KitList.Add(Result);
  Change;
end;

procedure TsmxObjectList.Assign(Source: TsmxKit);
var
  i: Integer;
begin
  if Source is TsmxObjectList then
  begin
    Clear;
    for i := 0 to Source.Count - 1 do
      if Assigned(TsmxObjectList(Source)[i].ObjectItem) then
        Add(TPersistentClass(TsmxObjectList(Source)[i].ObjectItem.ClassType)).Assign(Source[i])
      else
        Add.Assign(Source[i]);
  end else
    inherited Assign(Source);
end;

function TsmxObjectList.GetItem(Index: Integer): TsmxObjectItem;
begin
  Result := TsmxObjectItem(inherited Items[Index]);
end;

procedure TsmxObjectList.SetItem(Index: Integer; Value: TsmxObjectItem);
begin
  inherited Items[Index] := Value;
end;

end.
