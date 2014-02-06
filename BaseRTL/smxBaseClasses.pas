unit smxBaseClasses;

interface

uses
  Classes, SysUtils, smxBaseIntf;

const
  IID_IsmxRefInterface: TGUID = '{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}';

type
  { TsmxComponent }

  EsmxBaseError = class(Exception);

  EsmxComponentError = class(EsmxBaseError)
  {private
    FOriginMessage: String;
  public
    constructor CreateByOrigin(Ident: Integer; const Args: array of const;
      const OriginMessage: String); overload;
    constructor CreateByOrigin(ResStringRec: PResStringRec; const Args: array of const;
      const OriginMessage: String); overload;

    property OriginMessage: String read FOriginMessage write FOriginMessage;}
  end;

  TsmxComponent = class(TComponent, IsmxBaseInterface, IsmxRefComponent)
  protected
    function GetDescription: String; virtual;
    function GetInternalRef: Pointer; virtual;
    function GetReference: TComponent;
    function GetVersion: String; virtual;
  public
    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  TsmxComponentClass = class of TsmxComponent;

  { TsmxInterfacedComponent }

  TsmxInterfacedComponent = class(TsmxComponent)
  private
    FController: Pointer;
    function GetController: IsmxRefComponent;
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const AController: IsmxRefComponent); reintroduce; overload; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Controller: IsmxRefComponent read GetController;
    property RefCount: Integer read FRefCount;
  end;

  TsmxInterfacedComponentClass = class of TsmxInterfacedComponent;

  { TsmxInterfacedObject }

  TsmxInterfacedObject = class(TInterfacedObject, IsmxBaseInterface)
  protected
    function GetDescription: String; virtual;
    function GetVersion: String; virtual;
  public
    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  TsmxInterfacedObjectClass = class of TsmxInterfacedObject;

  { IsmxRefInterface }

  (*TsmxInterfacedPersistent = class;

  IsmxRefInterface = interface(IsmxBaseInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    function GetInternalRef: Pointer;
    function GetReference: TsmxInterfacedPersistent;
  end;*)

  { TsmxInterfacedPersistent }

  TsmxInterfacedPersistent = class(TPersistent, IInterface, IsmxBaseInterface, IsmxRefPersistent)
  private
    //FName: String;
    FController: Pointer;
    function GetController: IsmxRefComponent;
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetDescription: String; virtual;
    function GetInternalRef: Pointer; virtual;
    function GetReference: TPersistent;
    function GetVersion: String; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    //procedure SetName(const Value: String); virtual;
  public
    constructor Create(const AController: IsmxRefComponent); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Controller: IsmxRefComponent read GetController;
    property Description: String read GetDescription;
    //property Name: String read FName write SetName;
    property RefCount: Integer read FRefCount;
    property Version: String read GetVersion;
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
   // ����� �������������� �� persistent
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
    FOwner: TObject;
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxKitItem;
    function GetKitList: TList;
    procedure SetItem(Index: Integer; Value: TsmxKitItem);
  protected
    //function GetOwner: TPersistent; virtual;

    property KitList: TList read GetKitList;
  public
    constructor Create(AItemClass: TsmxKitItemClass); overload; virtual;
    constructor Create(AOwner: TObject; AItemClass: TsmxKitItemClass); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKit); virtual;
    function Add: TsmxKitItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    //function IndexOf(Item: TsmxKitItem): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem write SetItem; default;
    property KitItemClass: TsmxKitItemClass read FKitItemClass;
    property Owner: TObject read FOwner;
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

implementation

uses
  Windows, Variants, smxConsts;

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

{ TsmxComponent }

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

{ TsmxInterfacedComponent }

constructor TsmxInterfacedComponent.Create(const AController: IsmxRefComponent);
begin
  Create(AController.GetReference);
  FController := Pointer(AController);
end;

function TsmxInterfacedComponent._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IsmxRefComponent(FController)._AddRef
  else
    Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedComponent._Release: Integer;
begin
  if Assigned(FController) then
    Result := IsmxRefComponent(FController)._Release
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

function TsmxInterfacedComponent.GetController: IsmxRefComponent;
begin
  Result := IsmxRefComponent(FController);
end;

class function TsmxInterfacedComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedComponent(Result).FRefCount := 1;
end;

{ TsmxInterfacedObject }

function TsmxInterfacedObject.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedObject.GetVersion: String;
begin
  Result := cVersion;
end;

{ TsmxInterfacedPersistent }

constructor TsmxInterfacedPersistent.Create(const AController: IsmxRefComponent);
begin
  FController := Pointer(AController);
end;

function TsmxInterfacedPersistent._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IsmxRefComponent(FController)._AddRef
  else
    Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedPersistent._Release: Integer;
begin
  if Assigned(FController) then
    Result := IsmxRefComponent(FController)._Release
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

function TsmxInterfacedPersistent.GetController: IsmxRefComponent;
begin
  Result := IsmxRefComponent(FController);
end;

function TsmxInterfacedPersistent.GetDescription: String;
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
end;

class function TsmxInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedPersistent(Result).FRefCount := 1;
end;

function TsmxInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  //if Assigned(FController) then
    //Result := IsmxRefComponent(FController).QueryInterface(IID, Obj) else
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;

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
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FKit.KitList.Move(CurIndex, Value);
end;

procedure TsmxKitItem.SetKit(Value: TsmxKit);
begin
  if Assigned(FKit) then
    FKit.KitList.Remove(Self);
  FKit := Value;
  if Assigned(FKit) then
    FKit.KitList.Add(Self);
end;

{ TsmxKit }

constructor TsmxKit.Create(AItemClass: TsmxKitItemClass);
begin
  FKitItemClass := AItemClass;
end;

constructor TsmxKit.Create(AOwner: TObject; AItemClass: TsmxKitItemClass);
begin
  Create(AItemClass);
  FOwner := AOwner;
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
  if (CurIndex >= 0) and (CurIndex <> Value) then
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

end.