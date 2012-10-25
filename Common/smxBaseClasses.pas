unit smxBaseClasses;

interface

uses
  Classes, SysUtils, smxBaseIntf;

type
  { TsmxComponent }

  EsmxBaseError = class(Exception);

  EsmxComponentError = class(EsmxBaseError)
  private
    FOriginMessage: String;
  public
    constructor CreateByOrigin(Ident: Integer; const Args: array of const;
      const OriginMessage: String); overload;
    constructor CreateByOrigin(ResStringRec: PResStringRec; const Args: array of const;
      const OriginMessage: String); overload;

    property OriginMessage: String read FOriginMessage write FOriginMessage;
  end;

  TsmxComponent = class(TComponent)
  protected
    function GetVersion: String; virtual;
  public
    class function GetDescription: String; virtual;

    property Version: String read GetVersion;
  end;

  TsmxComponentClass = class of TsmxComponent;

  TsmxComponentEvent = procedure(Component: TsmxComponent) of object;

  { TsmxInterfacedComponent }

  TsmxInterfacedComponent = class(TInterfacedObject, IsmxBaseInterface)
  protected
    function GetDescription: String; virtual;
    function GetVersion: String; virtual;
  public
    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  TsmxInterfacedComponentClass = class of TsmxInterfacedComponent;

  { TsmxInterfacedComponent2 }

  TsmxInterfacedComponent2 = class(TPersistent, IInterface, IsmxBaseInterface)
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetDescription: String; virtual;
    function GetVersion: String; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property Description: String read GetDescription;
    property RefCount: Integer read FRefCount;
    property Version: String read GetVersion;
  end;

  { TsmxKitItem }

  TsmxKit = class;

  TsmxKitItem = class(TObject)
  private
    FKit: TsmxKit;
    procedure SetKit(Value: TsmxKit);
  protected
    function GetIndex: Integer; virtual;
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(AKit: TsmxKit); virtual;
    procedure Assign(Source: TsmxKitItem); virtual;

    property ItemIndex: Integer read GetIndex write SetIndex;
    property Kit: TsmxKit read FKit write SetKit;
  end;

  TsmxKitItemClass = class of TsmxKitItem;

  { TsmxKit }

  TsmxKit = class(TObject)
  private
    FKitItemClass: TsmxKitItemClass;
    FKitList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxKitItem;
    function GetKitList: TList;
    procedure SetItem(Index: Integer; Value: TsmxKitItem);
  protected
    property KitItemClass: TsmxKitItemClass read FKitItemClass;
    property KitList: TList read GetKitList;
  public
    constructor Create(AItemClass: TsmxKitItemClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKit); virtual;
    function Add: TsmxKitItem;
    procedure Clear;
    procedure Delete(Index: Integer);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem write SetItem; default;
  end;

  { TsmxHKitItem }

  TsmxHKit = class;

  TsmxHKitItem = class(TObject)
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

    property Count: Integer read GetCount;
    property HKit: TsmxHKit read FHKit;
    property ItemIndex: Integer read GetIndex write SetIndex;
    property Items[Index: Integer]: TsmxHKitItem read GetItem write SetItem; default;
    property Parent: TsmxHKitItem read FParent write SetParent;
  end;

  TsmxHKitItemClass = class of TsmxHKitItem;

  { TsmxHKit }

  TsmxHKit = class(TObject)
  private
    FHKitItemClass: TsmxHKitItemClass;
    FRoot: TsmxHKitItem;
    function GetRoot: TsmxHKitItem;
    procedure SetRoot(Value: TsmxHKitItem);
  protected
    property HKitItemClass: TsmxHKitItemClass read FHKitItemClass;
  public
    constructor Create(AItemClass: TsmxHKitItemClass); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxHKit); virtual;

    property Root: TsmxHKitItem read GetRoot write SetRoot;
  end;

implementation

uses
  Windows, smxConsts;

{$I ..\Resource\smxVers.inc}

{ EsmxComponentError }

constructor EsmxComponentError.CreateByOrigin(Ident: Integer;
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
end;

{ TsmxComponent }

class function TsmxComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxComponent.GetVersion: String;
begin
  Result := cVersion;
end;

{ TsmxInterfacedComponent }

function TsmxInterfacedComponent.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedComponent.GetVersion: String;
begin
  Result := cVersion;
end;

{ TsmxInterfacedComponent2 }

function TsmxInterfacedComponent2._AddRef: Integer;
begin
  Result := Windows.InterlockedIncrement(FRefCount);
end;

function TsmxInterfacedComponent2._Release: Integer;
begin
  Result := Windows.InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TsmxInterfacedComponent2.AfterConstruction;
begin
  Windows.InterlockedDecrement(FRefCount);
end;

procedure TsmxInterfacedComponent2.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
end;

function TsmxInterfacedComponent2.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxInterfacedComponent2.GetVersion: String;
begin
  Result := cVersion;
end;

class function TsmxInterfacedComponent2.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TsmxInterfacedComponent2(Result).FRefCount := 1;
end;

function TsmxInterfacedComponent2.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0 else
    Result := E_NOINTERFACE;
end;

{ TsmxKitItem }

constructor TsmxKitItem.Create(AKit: TsmxKit);
begin
  FKit := AKit;
end;

procedure TsmxKitItem.Assign(Source: TsmxKitItem);
var
  Name: String;
begin
  if Assigned(Source) then
    Name := Source.ClassName else
    Name := 'nil';
  raise EsmxBaseError.CreateResFmt(@smxConsts.rsAssignError, [Name, ClassName]);
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
  begin
    TsmxKitItem(KitList[i]).Free;
    KitList.Delete(i);
  end;
end;

procedure TsmxKit.Delete(Index: Integer);
begin
  TsmxKitItem(KitList[Index]).Free;
  KitList.Delete(Index);
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
  begin
    TsmxHKitItem(HKitItemList[i]).Free;
    HKitItemList.Delete(i);
  end;
end;

procedure TsmxHKitItem.Delete(Index: Integer);
begin
  TsmxHKitItem(HKitItemList[Index]).Free;
  HKitItemList.Delete(Index);
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

end.



