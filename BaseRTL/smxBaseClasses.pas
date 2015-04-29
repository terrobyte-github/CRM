unit smxBaseClasses;

interface

uses
  Classes, SysUtils, smxBaseIntf, smxDBIntf, XMLIntf, smxTypes, smxClassIntf;

//const
  //IID_IsmxRefInterface: TGUID = '{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}';

type
  { TsmxComponent }

  EsmxBaseError = class(Exception);

  TsmxComponent = class;

  EsmxComponentError = class(EsmxBaseError)
  private
    //FOriginMessage: String;
    FComponent: TsmxComponent;
  public
    //constructor CreateByOrigin(Ident: Integer; const Args: array of const;
      //const OriginMessage: String); overload;
    //constructor CreateByOrigin(ResStringRec: PResStringRec; const Args: array of const;
      //const OriginMessage: String); overload;
    constructor CreateByComponent(Ident: Integer; const Args: array of const;
      Component: TsmxComponent); overload;
    constructor CreateByComponent(ResStringRec: PResStringRec; const Args: array of const;
      Component: TsmxComponent); overload;

    //property OriginMessage: String read FOriginMessage write FOriginMessage;
    property Component: TsmxComponent read FComponent;
  end;

  TsmxComponent = class(TComponent, IsmxBaseInterface, IsmxRefComponent)
  protected
    function GetController: IsmxBaseInterface; virtual;
    function GetDescription: String; virtual;
    function GetInternalRef: Pointer; virtual;
    function GetReference: TComponent;
    function GetVersion: String; virtual;
    function IsInterfacedObj: Boolean; virtual;
  public
    function IsImplIntf(const Intf: IsmxBaseInterface): Boolean; virtual;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  TsmxComponentClass = class of TsmxComponent;

  { TsmxInterfacedComponent }

  TsmxInterfacedComponent = class(TsmxComponent, IInterface)
  private
    FController: Pointer;
    //function GetController: IsmxBaseInterface;//IsmxRefComponent;
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetController: IsmxBaseInterface; override;
    function IsInterfacedObj: Boolean; override;
  public
    { TODO : убрать owner из конструктора }
    constructor Create(AOwner: TComponent; const AController: IsmxBaseInterface{IsmxRefComponent}); reintroduce; overload; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Controller: IsmxBaseInterface{IsmxRefComponent} read GetController;
    property RefCount: Integer read FRefCount;
  end;

  TsmxInterfacedComponentClass = class of TsmxInterfacedComponent;

  { TsmxInterfacedObject }

  {TsmxInterfacedObject = class(TInterfacedObject, IsmxBaseInterface)
  protected
    function GetDescription: String; virtual;
    function GetVersion: String; virtual;
  public
    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;}

  //TsmxInterfacedObjectClass = class of TsmxInterfacedObject;

  { IsmxRefInterface }

  (*TsmxInterfacedPersistent = class;

  IsmxRefInterface = interface(IsmxBaseInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    function GetInternalRef: Pointer;
    function GetReference: TsmxInterfacedPersistent;
  end;*)

  { TsmxPersistent }

  TsmxPersistent = class(TPersistent, IInterface, IsmxBaseInterface, IsmxRefPersistent)
  //private
    //FName: String;
    //FController: Pointer;
    //function GetController: IsmxRefComponent;
  protected
    //FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetController: IsmxBaseInterface; virtual;
    function GetDescription: String; virtual;
    function GetInternalRef: Pointer; virtual;
    function GetReference: TPersistent;
    function GetVersion: String; virtual;
    function IsInterfacedObj: Boolean; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    //procedure SetName(const Value: String); virtual;
  public
    //constructor Create(const AController: IsmxRefComponent); overload;
    //procedure AfterConstruction; override;
    //procedure BeforeDestruction; override;
    //class function NewInstance: TObject; override;
    function IsImplIntf(const Intf: IsmxBaseInterface): Boolean; virtual;

    //property Controller: IsmxRefComponent read GetController;
    property Description: String read GetDescription;
    //property Name: String read FName write SetName;
    //property RefCount: Integer read FRefCount;
    property Version: String read GetVersion;
  end;

  TsmxPersistentClass = class of TsmxPersistent;

  { TsmxInterfacedPersistent }

  TsmxInterfacedPersistent = class(TsmxPersistent, IInterface{, IsmxBaseInterface, IsmxRefPersistent})
  private
    //FName: String;
    FController: Pointer;
    //function GetController: IsmxBaseInterface;//IsmxRefComponent;
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetController: IsmxBaseInterface; override;
    //function GetDescription: String; virtual;
    //function GetInternalRef: Pointer; virtual;
    //function GetReference: TPersistent;
    //function GetVersion: String; virtual;
    function IsInterfacedObj: Boolean; override;
    //function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    //procedure SetName(const Value: String); virtual;
  public
    constructor Create(const AController: IsmxBaseInterface{IsmxRefComponent}); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Controller: IsmxBaseInterface{IsmxRefComponent} read GetController;
    //property Description: String read GetDescription;
    //property Name: String read FName write SetName;
    property RefCount: Integer read FRefCount;
    //property Version: String read GetVersion;
  end;

  TsmxInterfacedPersistentClass = class of TsmxInterfacedPersistent;

  { TsmxInterfacedComponent }

  {TsmxInterfacedComponent = class(TComponent, IInterface, IsmxBaseInterface)
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetDescription: String; virtual;
    function GetVersion: String; virtual;
    //function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Description: String read GetDescription;
    property RefCount: Integer read FRefCount;
    property Version: String read GetVersion;
  end;

  TsmxInterfacedComponentClass = class of TsmxInterfacedComponent;}

  { TsmxFreeNotificationObject }

  {TsmxFreeNotificationObject = class(TInterfacedObject, IsmxFreeNotification)
  private
    FFreeNotifies: TInterfaceList;
    FOwner: IsmxOwnerNotification;
    function GetFreeNotifies: TInterfaceList;
  protected
    procedure FreeNotification(const Sender: IsmxFreeNotification); virtual;

    property FreeNotifies: TInterfaceList read GetFreeNotifies;
  public
    constructor Create(const Owner: IsmxOwnerNotification); virtual;
    destructor Destroy; override;
    function GetOwnerNotification: IsmxOwnerNotification;
    procedure InsertFreeNotification(const Reciever: IsmxFreeNotification);
    procedure RemoveFreeNotification(const Reciever: IsmxFreeNotification);
  end;

  TsmxFreeNotificationObjectClass = class of TsmxFreeNotificationObject;}

  { TsmxInfoObject }

{$M+}
  TsmxInfoObject = class(TObject)
  end;
{$M-}

  { TsmxKitItem }

  EsmxListError = class(EsmxBaseError);

  TsmxKit = class;
   // может унаследываться от persistent
  TsmxKitItem = class(TsmxInfoObject)
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
    procedure Assign(Source: TsmxKitItem); virtual;

    property DisplayName: String read GetDisplayName;
    property DisplayObject: TObject read GetDisplayObject;
    property ItemIndex: Integer read GetIndex write SetIndex;
    property Kit: TsmxKit read FKit write SetKit;
  end;

  TsmxKitItemClass = class of TsmxKitItem;

  { TsmxKit }

  TsmxKit = class(TsmxInfoObject)
  private
    FKitItemClass: TsmxKitItemClass;
    FKitList: TList;
    FOnChange: TNotifyEvent;
    //FOwner: TPersistent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxKitItem;
    function GetKitList: TList;
    procedure SetItem(Index: Integer; Value: TsmxKitItem);
  protected
    //function GetOwner: TPersistent; virtual;
    procedure DoChange; virtual;
    procedure InternalChange; virtual;

    property KitList: TList read GetKitList;
  public
    constructor Create(AItemClass: TsmxKitItemClass); {overload;} virtual;
    //constructor Create(AOwner: TPersistent; AItemClass: TsmxKitItemClass); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKit); virtual;
    function Add: TsmxKitItem;
    procedure Change;
    procedure Clear;
    procedure Delete(Index: Integer);
    //function IndexOf(Item: TsmxKitItem): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem write SetItem; default;
    property KitItemClass: TsmxKitItemClass read FKitItemClass;
    //property Owner: TPersistent read FOwner;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TsmxKitClass = class of TsmxKit;

  { TsmxHKitItem }

  TsmxHKit = class;

  TsmxHKitItem = class(TsmxInfoObject)
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
    procedure Assign(Source: TsmxHKitItem); virtual;
    procedure Clear;
    procedure Delete(Index: Integer);
    function HasChilds: Boolean;
    function HasParent: Boolean;
    //function IndexOf(Item: TsmxHKitItem): Integer;

    property Count: Integer read GetCount;
    property HKit: TsmxHKit read FHKit;
    property ItemIndex: Integer read GetIndex write SetIndex;
    property Items[Index: Integer]: TsmxHKitItem read GetItem write SetItem; default;
    property Parent: TsmxHKitItem read FParent write SetParent;
  end;

  TsmxHKitItemClass = class of TsmxHKitItem;

  { TsmxHKit }

  TsmxHKit = class(TsmxInfoObject)
  private
    FHKitItemClass: TsmxHKitItemClass;
    FRoot: TsmxHKitItem;
    function GetRoot: TsmxHKitItem;
    procedure SetRoot(Value: TsmxHKitItem);
  public
    constructor Create(AItemClass: TsmxHKitItemClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxHKit); virtual;

    property HKitItemClass: TsmxHKitItemClass read FHKitItemClass;
    property Root: TsmxHKitItem read GetRoot write SetRoot;
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
    //function GetValue(const Name: String): Variant;
    //procedure SetValue(const Name: String; const Value: Variant);
  public
    function Add: TsmxParam;
    function FindByName(const ParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem write SetItem; default;
    //property Values[const Name: String]: Variant read GetValue write SetValue;
  end;

  { EsmxCfgError }

  EsmxCfgError = class(EsmxComponentError)
  //private
    //FCfgID: Integer;
  protected
    function GetCfgID: Integer; virtual;
  public
    {constructor CreateByComponent(Ident: Integer;
      const Args: array of const; CfgID: Integer); overload;
    constructor CreateByComponent(ResStringRec: PResStringRec;
      const Args: array of const; CfgID: Integer); overload;}

    property CfgID: Integer read GetCfgID;// write FCfgID;
  end;

  { TsmxBaseCfg }

  //TsmxCustomRequest = class;

  TsmxBaseCfg = class(TsmxComponent)
  private
    FCfgID: Integer;
    FDeleteDataSetIntf: IsmxDataSet;
    FInsertDataSetIntf: IsmxDataSet;
    FSelectDataSetIntf: IsmxDataSet;
    FUpdateDataSetIntf: IsmxDataSet;
    //FSelectRequest: TsmxCustomRequest;
    FXMLDocIntf: IXMLDocument;
    function GetRootNode: IXMLNode;
    function GetXMLDoc: IXMLDocument;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetXMLText: String; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetModifyDataSet(Index: TsmxModifyRequest; const Value: IsmxDataSet); virtual;
    procedure SetSelectDataSet(const Value: IsmxDataSet); virtual;
    //procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;
    procedure SetXMLText(const Value: String); virtual;

    property RootNode: IXMLNode read GetRootNode;
    property XMLDoc: IXMLDocument read GetXMLDoc;
  public
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
    procedure ClearCfg; virtual;
    procedure ClearXML; virtual;
    procedure Load; virtual;
    procedure Read; virtual;
    //procedure Receive;
    procedure Remove; virtual;
    //procedure Return;
    procedure Save; virtual;
    procedure Write; virtual;

    property CfgID: Integer read FCfgID write SetCfgID;
    property DeleteDataSet: IsmxDataSet index rtDelete read FDeleteDataSetIntf write SetModifyDataSet;
    property InsertDataSet: IsmxDataSet index rtInsert read FInsertDataSetIntf write SetModifyDataSet;
    property SelectDataSet: IsmxDataSet read FSelectDataSetIntf write SetSelectDataSet;
    property UpdateDataSet: IsmxDataSet index rtUpdate read FUpdateDataSetIntf write SetModifyDataSet;
    //property SelectRequest: TsmxCustomRequest read FSelectRequest write SetSelectRequest;
    property XMLText: String read GetXMLText write SetXMLText;
  end;

  TsmxBaseCfgClass = class of TsmxBaseCfg;

  { TsmxObjectItem }

  TsmxObjectList = class;

  TsmxObjectItem = class(TsmxKitItem)
  private
    function GetKit: TsmxObjectList;
    function GetItemObjectInterface: IsmxObjectItem;
    procedure SetItemObject(Value: TPersistent);
    procedure SetKit(Value: TsmxObjectList);
  protected
    FItemObject: TPersistent;
    //procedure AddObject(ObjectClass: TPersistentClass = nil); virtual;
    procedure AddObject; overload; //virtual;
    procedure AddObject(ObjectClass: TPersistentClass); {reintroduce;} overload; //virtual;
    procedure DelObject; //virtual;
    function GetDisplayName: String; override;
    function GetDisplayObject: TObject; override;
    procedure SetIndex(Value: Integer); override;

    property ItemObjectInterface: IsmxObjectItem read GetItemObjectInterface;
  public
    constructor Create(AKit: TsmxKit); overload; override;
    constructor Create(AKit: TsmxKit; AObjectClass: TPersistentClass); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;
    //procedure InitializeItemObject(ItemObject: TPersistent);

    property Kit: TsmxObjectList read GetKit write SetKit;
    property ItemObject: TPersistent read FItemObject write SetItemObject;
  end;

  TsmxObjectItemClass = class of TsmxObjectItem;

  { TsmxObjectList }

  TsmxObjectList = class(TsmxKit)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TsmxObjectItem;
    function GetOwnerObjectInterface: IsmxObjectList;
    procedure SetItem(Index: Integer; Value: TsmxObjectItem);
  protected
    property OwnerObjectInterface: IsmxObjectList read GetOwnerObjectInterface;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TsmxKitItemClass); reintroduce; overload; virtual;
    function Add: TsmxObjectItem; overload;
    function Add(ObjectClass: TPersistentClass): TsmxObjectItem; overload;
    procedure Assign(Source: TsmxKit); override;

    property Items[Index: Integer]: TsmxObjectItem read GetItem write SetItem; default;
    property Owner: TPersistent read FOwner;
  end;

  TsmxObjectListClass = class of TsmxObjectList;

  { TsmxMultipleObjectItem }

  (*TsmxMultipleObjectList = class;

  TsmxMultipleObjectItem = class(TsmxObjectItem)
  private
    function GetKit: TsmxMultipleObjectList;
    procedure SetKit(Value: TsmxMultipleObjectList);
  protected
    procedure AddObject(ObjectClass: TPersistentClass); {reintroduce;} overload; //virtual;
  public
    constructor Create(AKit: TsmxKit; AObjectClass: TPersistentClass); reintroduce; overload; virtual;

    property Kit: TsmxMultipleObjectList read GetKit write SetKit;
  end;

  TsmxMultipleObjectItemClass = class of TsmxMultipleObjectItem;*)

  { TsmxMultipleObjectList }

  {TsmxMultipleObjectList = class(TsmxObjectList)
  private
    function GetItem(Index: Integer): TsmxMultipleObjectItem;
    procedure SetItem(Index: Integer; Value: TsmxMultipleObjectItem);
    function GetOwnerObjectInterface: IsmxMultipleObjectList;
  protected
    property OwnerObjectInterface: IsmxMultipleObjectList read GetOwnerObjectInterface;
  public
    function Add: TsmxMultipleObjectItem; overload;
    function Add(ObjectClass: TPersistentClass): TsmxMultipleObjectItem; overload;
    procedure Assign(Source: TsmxKit); override;

    property Items[Index: Integer]: TsmxMultipleObjectItem read GetItem write SetItem; default;
  end;

  TsmxMultipleObjectListClass = class of TsmxMultipleObjectList;}

implementation

uses
  Windows, Variants, smxFuncs, smxDBFuncs, smxConsts;

{$I ..\Resource\smxVers.inc}

{ EsmxComponentError }

{constructor EsmxComponentError.CreateByOrigin(Ident: Integer;
  const Args: array of const; const OriginMessage: String);
begin
  CreateResFmt(Ident, Args);
  FOriginMessage := OriginMessage;
end;

constructor EsmxComponentError.CreateByOrigin(ResStringRec: PResStringRec;
  const Args: array of const; const OriginMessage: String);
begin
  CreateResFmt(ResStringRec, Args);
  FOriginMessage := OriginMessage;
end;}

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

{constructor EsmxCfgError.CreateByComponent(Ident: Integer;
  const Args: array of const; CfgID: Integer);
begin
  CreateResFmt(Ident, Args);
  FCfgID := CfgID;
end;

constructor EsmxCfgError.CreateByComponent(ResStringRec: PResStringRec;
  const Args: array of const; CfgID: Integer);
begin
  CreateResFmt(ResStringRec, Args);
  FCfgID := CfgID;
end;}

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

function TsmxComponent.IsInterfacedObj: Boolean;
begin
  Result := False;
end;

function TsmxComponent.IsImplIntf(const Intf: IsmxBaseInterface): Boolean;
var
  AIntf: IsmxRefComponent;
begin
  Result := Assigned(Intf)
    and SysUtils.Supports(Intf, IsmxRefComponent, AIntf)
    and (AIntf.GetReference = Self);
end;

{ TsmxInterfacedComponent }

constructor TsmxInterfacedComponent.Create(AOwner: TComponent; const AController: IsmxBaseInterface{IsmxRefComponent});
begin
  Create(AOwner);
  FController := Pointer(AController);
end;

function TsmxInterfacedComponent._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface{IsmxRefComponent}(FController)._AddRef
  else
    Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedComponent._Release: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface{IsmxRefComponent}(FController)._Release
  else
    Result := Windows.InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
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

function TsmxInterfacedComponent.GetController: IsmxBaseInterface;//IsmxRefComponent;
begin
  Result := IsmxBaseInterface{IsmxRefComponent}(FController);
end;

function TsmxInterfacedComponent.IsInterfacedObj: Boolean;
begin
  Result := True; //not Assigned(FController);
end;

class function TsmxInterfacedComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedComponent(Result).FRefCount := 1;
end;

{ TsmxInterfacedObject }

{function TsmxInterfacedObject.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedObject.GetVersion: String;
begin
  Result := cVersion;
end;}

{ TsmxInterfacedPersistent }

constructor TsmxInterfacedPersistent.Create(const AController: IsmxBaseInterface{IsmxRefComponent});
begin
  FController := Pointer(AController);
end;

function TsmxInterfacedPersistent._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface{IsmxRefComponent}(FController)._AddRef
  else
    Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedPersistent._Release: Integer;
begin
  if Assigned(FController) then
    Result := IsmxBaseInterface{IsmxRefComponent}(FController)._Release
  else
    Result := Windows.InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
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

function TsmxInterfacedPersistent.GetController: IsmxBaseInterface;//IsmxRefComponent;
begin
  Result := IsmxBaseInterface{IsmxRefComponent}(FController);
end;

{function TsmxInterfacedPersistent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedPersistent.GetInternalRef: Pointer;
begin
  Result := nil;
end;

function TsmxInterfacedPersistent.GetReference: TPersistent;
begin
  Result := Self;
end;

function TsmxInterfacedPersistent.GetVersion: String;
begin
  Result := cVersion;
end;}

function TsmxInterfacedPersistent.IsInterfacedObj: Boolean;
begin
  Result := True; //not Assigned(FController);
end;

class function TsmxInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedPersistent(Result).FRefCount := 1;
end;

{function TsmxInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  //if Assigned(FController) then
    //Result := IsmxRefComponent(FController).QueryInterface(IID, Obj) else
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;}

{procedure TsmxInterfacedPersistent.SetName(const Value: String);
begin
  FName := Value;
end;}

{ TsmxInterfacedComponent }

{function TsmxInterfacedComponent._AddRef: Integer;
begin
  Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedComponent._Release: Integer;
begin
  Result := Windows.InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TsmxInterfacedComponent.AfterConstruction;
begin
  inherited AfterConstruction;
  Windows.InterlockedDecrement(FRefCount);
end;

procedure TsmxInterfacedComponent.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

function TsmxInterfacedComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedComponent.GetVersion: String;
begin
  Result := cVersion;
end;

class function TsmxInterfacedComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedComponent(Result).FRefCount := 1;
end;}

{function TsmxInterfacedComponent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;}

{ TsmxFreeNotificationObject }

{constructor TsmxFreeNotificationObject.Create(const Owner: IsmxOwnerNotification);
begin
  FOwner := Owner;
end;

destructor TsmxFreeNotificationObject.Destroy;
begin
  if Assigned(FFreeNotifies) then
    while FFreeNotifies.Count > 0 do
      IsmxFreeNotification(FFreeNotifies[FFreeNotifies.Count - 1]).FreeNotification(Self as IsmxFreeNotification);
  inherited Destroy;
end;}

(*procedure TsmxFreeNotificationObject.FreeNotification(const Sender: IsmxFreeNotification);
begin
  RemoveFreeNotification(Sender);
  if Assigned(FOwner) then
    FOwner.OwnerFreeNotification(Sender);
  {if Assigned(Sender) then
    if Assigned(Sender.GetOwnerNotification) then
      Sender.GetOwnerNotification.OwnerFreeNotification(Sender);}
end;*)

{function TsmxFreeNotificationObject.GetFreeNotifies: TInterfaceList;
begin
  if not Assigned(FFreeNotifies) then
    FFreeNotifies := TInterfaceList.Create;
  Result := FFreeNotifies;
end;

function TsmxFreeNotificationObject.GetOwnerNotification: IsmxOwnerNotification;
begin
  Result := FOwner;
end;

procedure TsmxFreeNotificationObject.InsertFreeNotification(const Reciever: IsmxFreeNotification);
begin
  if Assigned(Reciever) then
    if FreeNotifies.IndexOf(Reciever) < 0 then
    begin
      FreeNotifies.Add(Reciever);
      Reciever.InsertFreeNotification(Self as IsmxFreeNotification);
    end;
end;

procedure TsmxFreeNotificationObject.RemoveFreeNotification(const Reciever: IsmxFreeNotification);
begin
  if Assigned(Reciever) then
  begin
    FreeNotifies.Remove(Reciever);
    Reciever.RemoveFreeNotification(Self as IsmxFreeNotification);
  end;
end;}

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
    Name := Source.ClassName else
    Name := 'nil';
  raise EsmxListError.CreateResFmt(@smxConsts.rsAssignError, [Name, ClassName]);
end;

function TsmxKitItem.GetDisplayName: String;
begin
  Result := ClassName;
end;

function TsmxKitItem.GetDisplayObject: TObject;
begin
  Result := Self;
end;

function TsmxKitItem.GetIndex: Integer;
begin
  if Assigned(FKit) then
    Result := FKit.KitList.IndexOf(Self) else
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

{constructor TsmxKit.Create(AOwner: TPersistent; AItemClass: TsmxKitItemClass);
begin
  Create(AItemClass);
  FOwner := AOwner;
end;}

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

{function TsmxKit.GetOwner: TPersistent;
begin
  Result := nil;
end;}

{function TsmxKit.IndexOf(Item: TsmxKitItem): Integer;
begin
  Result := KitList.IndexOf(Item);
end;}

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
    FParent.HKitItemList.Move(CurIndex, Value);
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
  Result := FParent <> FHKit.Root;
end;

{function TsmxHKitItem.IndexOf(Item: TsmxHKitItem): Integer;
begin
  Result := HKitItemList.IndexOf(Item);
end;}

procedure TsmxHKitItem.SetParent(Value: TsmxHKitItem);
begin
  if Assigned(FParent) then
  begin
    FParent.HKitItemList.Remove(Self);
    FHKit := nil;
  end;
  FParent := Value;
  if Assigned(FParent) then
  begin
    FParent.HKitItemList.Add(Self);
    FHKit := FParent.FHKit;
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

{function TsmxParams.GetValue(const Name: String): Variant;
var
  Param: TsmxParam;
begin
  Param := FindByName(Name);
  if Assigned(Param) then
    Result := Param.ParamValue
  else
    with Add do
    begin
      ParamName := Name;
      Result := ParamValue;
    end;
end;

procedure TsmxParams.SetValue(const Name: String; const Value: Variant);
var
  Param: TsmxParam;
begin
  Param := FindByName(Name);
  if Assigned(Param) then
    Param.ParamValue := Value
  else
    with Add do
    begin
      ParamName := Name;
      ParamValue := Value;
    end;
end;}

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

function TsmxPersistent.IsImplIntf(const Intf: IsmxBaseInterface): Boolean;
var
  AIntf: IsmxRefPersistent;
begin
  Result := Assigned(Intf)
    and SysUtils.Supports(Intf, IsmxRefPersistent, AIntf)
    and (AIntf.GetReference = Self);
end;

function TsmxPersistent.IsInterfacedObj: Boolean;
begin
  Result := False;
end;

function TsmxPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;

{ TsmxBaseCfg }

destructor TsmxBaseCfg.Destroy;
begin
  FXMLDocIntf := nil;
  SetSelectDataSet(nil);
  SetModifyDataSet(rtDelete, nil);
  SetModifyDataSet(rtInsert, nil);
  SetModifyDataSet(rtUpdate, nil);
  inherited Destroy;
end;

{procedure TsmxBaseCfg.Assign(Source: TPersistent);
begin
  if Source is TsmxBaseCfg then
    CfgID := TsmxBaseCfg(Source).CfgID
  else
    inherited Assign(Source);
end;}

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
    FXMLDocIntf := smxFuncs.NewXML;
  Result := FXMLDocIntf;
end;

function TsmxBaseCfg.GetXMLText: String;
begin
  try
    XMLDoc.Active := True;
    { TODO -oПоляков Александр : временно закомментировал }
    //Write;
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
    { TODO -oПоляков Александр : временно закомментировал }
    //Read;
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
  if smxDBFuncs.GetValueByKey(FSelectDataSetIntf,
      FCfgID, Value{, FSelectRequest.PerformanceMode}) then
    XMLDoc.XML.Text := Value else
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
  //Request: IsmxDataSet;
  //Performance: TsmxPerformanceMode;
  DataSet: IsmxDataSet;
  Key: Variant;
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
    //Request := FSelectRequest.InsertDataSet; //ModifyRequests[mrInsert];
    //Performance := FSelectRequest.InsertPerformance; //ModifyPerformances[mrInsert];
  end else
  begin
    DataSet := FUpdateDataSetIntf;
    Key := FCfgID;
    //Request := FSelectRequest.UpdateDataSet; //ModifyRequests[mrUpdate];
    //Performance := FSelectRequest.UpdatePerformance; //ModifyPerformances[mrUpdate];
  end;
  {if not Assigned(Request) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);}
  if smxDBFuncs.SetValueByKey(DataSet, Key, XMLDoc.XML.Text{, Performance}) then
    FCfgID := Key;
end;

procedure TsmxBaseCfg.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(SelectDataSet) {and smxFuncs.GetRefComponent(DeleteDataSet.GetReference, Component)
        and (AComponent = Component)} and (AComponent = SelectDataSet.GetReference) then
      SelectDataSet := nil;
    if Assigned(DeleteDataSet) {and smxFuncs.GetRefComponent(DeleteDataSet.GetReference, Component)
        and (AComponent = Component)} and (AComponent = DeleteDataSet.GetReference) then
      DeleteDataSet := nil else
    if Assigned(InsertDataSet) {and smxFuncs.GetRefComponent(InsertDataSet.GetReference, Component)
        and (AComponent = Component)} and (AComponent = InsertDataSet.GetReference) then
      InsertDataSet := nil else
    if Assigned(UpdateDataSet) {and smxFuncs.GetRefComponent(UpdateDataSet.GetReference, Component)
        and (AComponent = Component)} and (AComponent = UpdateDataSet.GetReference) then
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

{procedure TsmxBaseCfg.Receive;
begin
  Load;
  Read;
end;}

{procedure TsmxBaseCfg.Return;
begin
  Write;
  Save;
end;}

procedure TsmxBaseCfg.Remove;
var
  //DataSet: IsmxDataSet;
  //Request: IsmxDataSet;
  //Performance: TsmxPerformanceMode;
  Key: Variant;
begin
  if not Assigned(FDeleteDataSetIntf) or (FCfgID = 0) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], Self);
  //Request := FSelectRequest.DeleteDataSet; //ModifyRequests[mrDelete];
  //Performance := FSelectRequest.DeletePerformance; //ModifyPerformances[mrDelete];
  {if not Assigned(FDeleteDataSetIntf) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);}
  Key := FCfgID;
  if smxDBFuncs.SetValueByKey(FDeleteDataSetIntf, Key, Variants.Null{, Performance}) then
  begin
    FCfgID := 0;
    XMLDoc.XML.Text := smxConsts.cXMLDocTextDef;
  end;
end;

procedure TsmxBaseCfg.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

{procedure TsmxBaseCfg.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
  if Assigned(FSelectRequest) then
    FSelectRequest.FreeNotification(Self);
end;}

procedure TsmxBaseCfg.SetModifyDataSet(Index: TsmxModifyRequest; const Value: IsmxDataSet);
begin
  case Index of
    rtDelete: FDeleteDataSetIntf := Value;
    rtInsert: FInsertDataSetIntf := Value;
    rtUpdate: FUpdateDataSetIntf := Value;
  end;
  if Assigned(Value) then
    Value.GetReference.FreeNotification(Self);
end;

procedure TsmxBaseCfg.SetSelectDataSet(const Value: IsmxDataSet);
begin
  FSelectDataSetIntf := Value;
  if Assigned(FSelectDataSetIntf) then
    FSelectDataSetIntf.GetReference.FreeNotification(Self);
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
begin
  if Assigned(Kit) then
    if Assigned(Kit.OwnerObjectInterface) then
      Kit.OwnerObjectInterface.CreateObject(Self);
end;

procedure TsmxObjectItem.AddObject(ObjectClass: TPersistentClass);
begin
  if Assigned(Kit) then
    if Assigned(Kit.OwnerObjectInterface) then
      Kit.OwnerObjectInterface.CreateObject(Self, ObjectClass);
end;

procedure TsmxObjectItem.DelObject;
begin
  if Assigned(Kit) then
    if Assigned(Kit.OwnerObjectInterface) then
      Kit.OwnerObjectInterface.DestroyObject(Self);
end;

procedure TsmxObjectItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxObjectItem then
    ItemObject := TsmxObjectItem(Source).ItemObject
  else
    inherited Assign(Source);
end;

function TsmxObjectItem.GetDisplayName: String;
begin
  if Assigned(FItemObject) then
    Result := FItemObject.ClassName
  else
    Result := inherited GetDisplayName;
end;

function TsmxObjectItem.GetDisplayObject: TObject;
begin
  if Assigned(FItemObject) then
    Result := FItemObject
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
    //if Assigned(Kit.Owner) then
      if Assigned(ItemObjectInterface) then
        ItemObjectInterface.ChangeObjectOwner(nil);
  inherited Kit := Value;
  if Assigned(Kit) then
    //if Assigned(Kit.Owner) then
      if Assigned(ItemObjectInterface) then
        ItemObjectInterface.ChangeObjectOwner(Kit.Owner);
end;

function TsmxObjectItem.GetItemObjectInterface: IsmxObjectItem;
begin
  if Assigned(FItemObject) then
    FItemObject.GetInterface(IsmxObjectItem, Result)
  else
    Result := nil;
end;

procedure TsmxObjectItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  if Assigned(ItemObjectInterface) then
    ItemObjectInterface.ChangeObjectIndex(Value);
end;

procedure TsmxObjectItem.SetItemObject(Value: TPersistent);
begin
  if Assigned(FItemObject) then
    FItemObject.Assign(Value);
end;

{procedure TsmxObjectItem.InitializeItemObject(ItemObject: TPersistent);
begin
  FItemObject := ItemObject;
end;}

{ TsmxObjectList }

constructor TsmxObjectList.Create(AOwner: TPersistent; AItemClass: TsmxKitItemClass);
begin
  Create(AItemClass);
  FOwner := AOwner;
end;

function TsmxObjectList.Add: TsmxObjectItem;
begin
  Result := TsmxObjectItem(inherited Add);
end;

function TsmxObjectList.Add(ObjectClass: TPersistentClass): TsmxObjectItem;
begin
  //if Assigned(OwnerObjectInterface) then
    //OwnerObjectInterface.CheckObjectClass(ObjectClass);
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
      if Assigned(TsmxObjectList(Source)[i].ItemObject) then
        Add(TPersistentClass(TsmxObjectList(Source)[i].ItemObject.ClassType)).Assign(Source[i])
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

function TsmxObjectList.GetOwnerObjectInterface: IsmxObjectList;
begin
  {if Assigned(Owner) then
    Owner.GetInterface(IsmxObjectList, Result)
  else
    Result := nil;}
  if Assigned(Owner) then
    //Result := Owner as IsmxObjectList
    Owner.GetInterface(IsmxObjectList, Result)
  else
    Result := nil;
end;

{ TsmxMultipleObjectItem }

{constructor TsmxMultipleObjectItem.Create(AKit: TsmxKit; AObjectClass: TPersistentClass);
begin
  inherited Create(AKit);
  AddObject(AObjectClass);
end;

procedure TsmxMultipleObjectItem.AddObject(ObjectClass: TPersistentClass);
begin
  if Assigned(Kit) then
    if Assigned(Kit.OwnerObjectInterface) then
      Kit.OwnerObjectInterface.CreateObject(Self, ObjectClass);
end;

function TsmxMultipleObjectItem.GetKit: TsmxMultipleObjectList;
begin
  Result := TsmxMultipleObjectList(inherited Kit);
end;

procedure TsmxMultipleObjectItem.SetKit(Value: TsmxMultipleObjectList);
begin
  inherited Kit := Value;
end;}

{ TsmxMultipleObjectList }

{function TsmxMultipleObjectList.Add: TsmxMultipleObjectItem;
begin
  Result := TsmxMultipleObjectItem(inherited Add);
end;

function TsmxMultipleObjectList.Add(ObjectClass: TPersistentClass): TsmxMultipleObjectItem;
begin
  if Assigned(OwnerObjectInterface) then
    OwnerObjectInterface.CheckObjectClass(ObjectClass);
  Result := TsmxMultipleObjectItemClass(KitItemClass).Create(Self, ObjectClass);
  KitList.Add(Result);
  Change;
end;

procedure TsmxMultipleObjectList.Assign(Source: TsmxKit);
var
  i: Integer;
begin
  if Source is TsmxMultipleObjectList then
  begin
    Clear;
    for i := 0 to Source.Count - 1 do
      if Assigned(TsmxMultipleObjectList(Source)[i].ItemObject) then
        Add(TPersistentClass(TsmxMultipleObjectList(Source)[i].ItemObject.ClassType)).Assign(Source[i])
      else
        Add.Assign(Source[i]);
  end else
    inherited Assign(Source);
end;

function TsmxMultipleObjectList.GetItem(Index: Integer): TsmxMultipleObjectItem;
begin
  Result := TsmxMultipleObjectItem(inherited Items[Index]);
end;

procedure TsmxMultipleObjectList.SetItem(Index: Integer; Value: TsmxMultipleObjectItem);
begin
  inherited Items[Index] := Value;
end;

function TsmxMultipleObjectList.GetOwnerObjectInterface: IsmxMultipleObjectList;
begin
  if Assigned(Owner) then
    Owner.GetInterface(IsmxMultipleObjectList, Result)
  else
    Result := nil;
end;}

end.
