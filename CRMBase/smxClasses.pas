unit smxClasses;

interface

uses
  Classes, Controls, SysUtils, Windows, XMLIntf, smxBaseClasses, smxDBIntf,
  smxTypes;

type
  { TsmxBaseCfg }

  TsmxTargetRequest = class;

  EsmxCfgError = class(Exception);

  //TsmxFuncCallBack = function(I: Integer): Variant of object;

  TsmxBaseCfg = class(TsmxComponent)
  private
    FCall: TsmxFuncCallBack;
    //FCfgName: String;
    FDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    FTargetRequest: TsmxTargetRequest;
    FXMLDocIntf: IXMLDocument;
    //function GetCfgName: String;
    function GetDatabase: IsmxDatabase;
    function GetXMLText: String;
    procedure SetXMLText(Value: String);
  protected
    procedure LoadCell; virtual;
    procedure ReadCell; virtual;
    procedure SaveCell; virtual;
    procedure WriteCell; virtual;

    property TargetRequest: TsmxTargetRequest read FTargetRequest;
    property XMLDoc: IXMLDocument read FXMLDocIntf;
    property XMLText: String read GetXMLText write SetXMLText;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); reintroduce; //virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Finalize; virtual;
    procedure Initialize; virtual;

    property Call: TsmxFuncCallBack read FCall;
    //property CfgName: String read GetCfgName;
    property Database: IsmxDatabase read GetDatabase;
    property CfgID: Integer read FCfgID;
  end;

  TsmxBaseCfgClass = class of TsmxBaseCfg;

  { TsmxBaseCell }

  EsmxCellError = class(Exception);

  //TsmxBaseCell = class;
  //TsmxBaseCellClass = class of TsmxBaseCell;
  //TsmxTypeCfg = class;

  TsmxBaseCell = class(TsmxComponent)
  private
    FCall: TsmxFuncCallBack;
    FCfg: TsmxBaseCfg;
    //FCfgClass: TsmxBaseCfgClass;
    //FChildList: TList;
    FDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    FImageList: TImageList;
    FParentCell: TsmxBaseCell;
    //FType: TsmxTypeCfg;
    //function GetCfgClass: TsmxBaseCfgClass;
    //procedure ClearParent;
    //function GetChildCount: Integer;
    //function GetChild(Index: Integer): TsmxBaseCell;
    function GetDatabase: IsmxDatabase;
    function GetImageList: TImageList;
    //function GetOwnerCell: TsmxBaseCell;
    function GetRootCell: TsmxBaseCell;
    //procedure InsertChild(AChild: TsmxBaseCell);
    //procedure RemoveChild(AChild: TsmxBaseCell);
  protected
    procedure CreateChilds; virtual;
    procedure DestroyChilds; virtual;
    function GetInternalObject: TObject; virtual;
    //function IDToCfgClass(ACfgID: Integer): TsmxBaseCfgClass; virtual;
    //function IDToItemClass(ACfgID: Integer): TsmxBaseCellClass; virtual;
    procedure InitChilds; virtual;
    procedure InstallParent; virtual;
    procedure SetParentCell(AParent: TsmxBaseCell); virtual;
    procedure UnInstallParent; virtual;

    property Cfg: TsmxBaseCfg read FCfg; //GetCfg;
    //property CfgClass: TsmxBaseCfgClass read GetCfgClass;
    //property ChildList: TList read FChildList;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); reintroduce; virtual;
    destructor Destroy; override;
    //function FindChildByID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;

    property Call: TsmxFuncCallBack read FCall;
    //property ChildCount: Integer read GetChildCount;
    //property Childs[Index: Integer]: TsmxBaseCell read GetChild;
    property Database: IsmxDatabase read GetDatabase;
    property CfgID: Integer read FCfgID;
    property ImageList: TImageList read GetImageList;
    //property Item: TObject read GetInternalObject;
    //property OwnerCell: TsmxBaseCell read GetOwnerCell;
    property ParentCell: TsmxBaseCell read FParentCell write SetParentCell;
    property RootCell: TsmxBaseCell read GetRootCell;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

  { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxBaseCfg)
  private
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    procedure SetCfgClassName(Value: String);
    procedure SetCellClassName(Value: String);
  protected
    procedure LoadCell; override;
    procedure ReadCell; override;
    procedure SaveCell; override;
    procedure WriteCell; override;
  public
    property CfgClass: TsmxBaseCfgClass read FCfgClass;
    property CfgClassName: String read FCfgClassName write SetCfgClassName;
    property CellClass: TsmxBaseCellClass read FCellClass;
    property CellClassName: String read FCellClassName write SetCellClassName;
  end;

  { TsmxControlCell }

  TsmxControlCell = class(TsmxBaseCell)
  protected
    //procedure CreateChilds; virtual;
    //procedure DestroyChilds; virtual;
    //function GetInternalObject: TObject; virtual;
    function GetCellAlign: TAlign; virtual;
    function GetCellCursor: TCursor; virtual;
    function GetCellEnable: Boolean; virtual;
    function GetCellHeight: Integer; virtual;
    function GetCellLeft: Integer; virtual;
    //function GetItemParent: TObject; virtual;
    function GetCellTop: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    function GetCellWidth: Integer; virtual;
    //procedure InitChilds; virtual;
    //procedure InstallParent; virtual;
    procedure SetCellAlign(Value: TAlign); virtual;
    procedure SetCellCursor(Value: TCursor); virtual;
    procedure SetCellEnable(Value: Boolean); virtual;
    procedure SetCellHeight(Value: Integer); virtual;
    procedure SetCellLeft(Value: Integer); virtual;
    //procedure SetItemParent(Value: TObject); virtual;
    procedure SetCellTop(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;
    procedure SetCellWidth(Value: Integer); virtual;
    //procedure UnInstallParent; virtual;

    //property Item: TObject read GetInternalObject;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //destructor Destroy; override;
    procedure Apply; virtual;
    procedure Prepare(Forcibly: Boolean = False); virtual;
    //procedure PutParams(ParamList: Variant); virtual;

    property CellAlign: TAlign read GetCellAlign write SetCellAlign;
    property CellCursor: TCursor read GetCellCursor write SetCellCursor;
    property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    property CellHeight: Integer read GetCellHeight write SetCellHeight;
    property CellLeft: Integer read GetCellLeft write SetCellLeft;
    //property ItemParent: TObject read GetItemParent write SetItemParent;
    property CellTop: Integer read GetCellTop write SetCellTop;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    property CellWidth: Integer read GetCellWidth write SetCellWidth;
  end;

  { TsmxTargetRequest }

  EsmxTargetRequestError = class(Exception);

  //TsmxReturnType = (rtOpen, rtExecute);

  TsmxTargetRequest = class(TsmxComponent)
  private
    FDatabaseIntf: IsmxDatabase;
    FRequestIntf: IsmxDataSet;
    FParamList: TStrings;
    function GetParamList: TStrings;
    function GetParam(Key: String): String;
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetParam(Key: String; const Value: String);
  protected
    property ParamList: TStrings read GetParamList;
  public
    destructor Destroy; override;
    procedure ClearParams;
    procedure DoExecute(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil);
    function DoRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      Res: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
    function ForRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      Get: Boolean = False; Mode: TsmxReturnType = rtOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function NewRequest(SQLText: String = ''; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function PrepRequest(const ARequest: IsmxDataSet; Get: Boolean = True;
      Mode: TsmxReturnType = rtOpen; const DSFrom: IsmxDataSet = nil): Boolean;
    //function StreamToStr(Stream: TStream): String;
    //function StrToStream(Str: String): TStream;

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property TargetParams[Key: String]: String read GetParam write SetParam; default;
  end;

  { TsmxKitItem }

  //EsmxKitItemError = class(Exception);

  TsmxKit = class;

  TsmxKitItem = class(TObject)
  private
    FKit: TsmxKit;
  public
    constructor Create(AKit: TsmxKit); virtual;

    property Kit: TsmxKit read FKit;
  end;

  TsmxKitItemClass = class of TsmxKitItem;

  { TsmxKit }

  TsmxKit = class(TObject)
  private
    FList: TList;
    FCellClass: TsmxKitItemClass;
  protected
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxKitItem;
    //procedure SetItem(Index: Integer; Value: TsmxKitItem);
  public
    constructor Create(AItemClass: TsmxKitItemClass);
    destructor Destroy; override;
    function Add: TsmxKitItem;
    procedure Clear;
    function Insert(Index: Integer): TsmxKitItem;
    procedure Remove(AItem: TsmxKitItem);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem; default; //write SetItem
  end;

  { TsmxHKitItem }

  TsmxHKitItem = class;
  TsmxHKitItemClass = class of TsmxHKitItem;

  TsmxHKit = class;

  TsmxHKitItem = class(TObject)
  private
    FList: TList;
    FCellClass: TsmxHKitItemClass;
    FParent: TsmxHKitItem;
    FHKit: TsmxHKit;
  protected
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxHKitItem;
    //procedure SetItem(Index: Integer; Value: TsmxHKitItem);

    property HKit: TsmxHKit read FHKit;
  public
    constructor Create(AHKit: TsmxHKit); virtual; //(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass); virtual;
    destructor Destroy; override;
    function Add: TsmxHKitItem;
    procedure Clear;
    function HasChilds: Boolean;
    function HasParent: Boolean;
    procedure Remove(AItem: TsmxHKitItem);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxHKitItem read GetItem; default; //write SetItem
    property Parent: TsmxHKitItem read FParent;
  end;

  { TsmxHKit }

  TsmxHKit = class(TObject)
  private
    FCellClass: TsmxHKitItemClass;
    FRoot: TsmxHKitItem;
  public
    constructor Create(AItemClass: TsmxHKitItemClass);
    destructor Destroy; override;

    property Root: TsmxHKitItem read FRoot;
  end;

  { TsmxCallBackParam }

  {TsmxCallBackParam = class(TsmxKitItem)
  private
    FParamIndex: Integer;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamIndex: Integer read FParamIndex write FParamIndex;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;}

  { TsmxCallBackParams }

  {TsmxCallBackParams = class(TsmxKit)
  protected
    function GetInternalObject(Index: Integer): TsmxCallBackParam;
  public
    function Add: TsmxCallBackParam;
    function FindByIndex(AIndex: Integer): TsmxCallBackParam;

    property Items[Index: Integer]: TsmxCallBackParam read GetInternalObject; default;
  end;}

  { TsmxCallBack }

  //EsmxCallBackError = class(Exception);

  {TsmxCallBack = class(TsmxComponent)
  private
    FParamList: TsmxCallBackParams;
  protected
    property ParamList: TsmxCallBackParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure AddParam(Index: Integer; Value: Variant);
    //procedure DelParam(Index: Integer);
    function GetParam(Index: Integer): Variant;
    procedure SetParam(Index: Integer; Value: Variant);

    property ParamList[Index: Integer]: Variant read GetParam write SetParam; default;
  end;}

  { TsmxMFormsItem }

  {TsmxMFormsItem = class(TsmxKitItem)
  private
    FFormHandle: HWND;
    FFormObj: TObject;
    FFormPtr: TsmxBaseCell;
  public
    constructor Create(AKit: TsmxKit); override;

    property FormHandle: HWND read FFormHandle write FFormHandle;
    property FormObj: TObject read FFormObj write FFormObj;
    property FormPtr: TsmxBaseCell read FFormPtr write FFormPtr;
  end;}

  { TsmxMFormsItems }

  {TsmxMFormsItems = class(TsmxKit)
  protected
    function GetInternalObject(Index: Integer): TsmxMFormsItem;
  public
    function Add: TsmxMFormsItem;
    function FindByForm(AForm: TsmxBaseCell): TsmxMFormsItem;
    function FindByHandle(AHandle: HWND): TsmxMFormsItem;

    property Items[Index: Integer]: TsmxMFormsItem read GetInternalObject; default;
  end;}

  { TsmxFormManager }

  {TsmxFormManager = class(TsmxComponent)
  private
    FFormList: TsmxMFormsItems;
  protected
    function GetForm(Handle: HWND): TsmxBaseCell;
    //procedure SetForm(Handle: HWND; Value: TsmxCustomForm);
    procedure CloseForms;

    property FormList: TsmxMFormsItems read FFormList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertForm(AForm: TsmxBaseCell);
    procedure RemoveForm(AForm: TsmxBaseCell);

    property Form[Handle: HWND]: TsmxBaseCell read GetForm; default; // write SetForm; default;
  end;}

  { TsmxFormItem }

  TsmxFormItem = class(TsmxKitItem)
  private
    FForm: TsmxBaseCell;
    //FFormConfID: Integer;
    FFormHandle: HWND;
    FFormCfgID: Integer;
  public
    constructor Create(AKit: TsmxKit); override;

    property Form: TsmxBaseCell read FForm write FForm;
    //property FormConfID: Integer read FFormConfID write FFormConfID;
    property FormHandle: HWND read FFormHandle write FFormHandle;
    property FormCfgID: Integer read FFormCfgID write FFormCfgID;
  end;

  { TsmxFormItems }

  TsmxFormItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxFormItem;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxFormItem;
    function FindByForm(AForm: TsmxBaseCell): TsmxFormItem;
    function FindByHandle(AHandle: HWND): TsmxFormItem;
    function FindByCfgID(ACfgID: Integer): TsmxFormItem;

    property Items[Index: Integer]: TsmxFormItem read GetItem {write SetItem}; default;
  end;

  { TsmxFormManager }

  TsmxFormManager = class(TsmxComponent)
  private
    //FFormList: TList;
    FFormList: TsmxFormItems;
    //function GetForm(Index: Integer): TsmxBaseCell;
    //function GetFormCount: Integer;
    //function GetHandle(Handle: HWND): TsmxBaseCell;
  protected
    //procedure SetForm(Handle: HWND; Value: TsmxCustomForm);
    procedure DestroyForms;

    property FormList: TsmxFormItems read FFormList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByCfgID(ACfgID: Integer): TsmxBaseCell;
    function FindByHandle(AHandle: HWND): TsmxBaseCell;
    //function HandleOfForm(AForm: TsmxBaseCell): HWND;
    procedure InsertForm(AForm: TsmxBaseCell);
    procedure RemoveForm(AForm: TsmxBaseCell);

    //property FormCount: Integer read GetFormCount;
    //property FormList[Index: Integer]: TsmxBaseCell read GetForm; default;
    //property FormList: TsmxFormItems read FFormList;
    //property Handles[Handle: HWND]: TsmxBaseCell read GetHandle;
  end;

  { TsmxMActionsItem }

  {TsmxMActionsItem = class(TsmxKitItem)
  private
    FActionObj: TObject;
    FActionPtr: TsmxBaseCell;
  public
    constructor Create(AKit: TsmxKit); override;

    property ActionObj: TObject read FActionObj write FActionObj;
    property ActionPtr: TsmxBaseCell read FActionPtr write FActionPtr;
  end;}

  { TsmxMActionsItems }

  {TsmxMActionsItems = class(TsmxKit)
  protected
    function GetInternalObject(Index: Integer): TsmxMActionsItem;
  public
    function Add: TsmxMActionsItem;
    function FindByAction(AAction: TsmxBaseCell): TsmxMActionsItem;
    function FindByObject(AObject: TObject): TsmxMActionsItem;

    property Items[Index: Integer]: TsmxMActionsItem read GetInternalObject; default;
  end;}

  { TsmxManagerActions }

  {TsmxManagerActions = class(TsmxComponent)
  private
    FActionList: TsmxMActionsItems;
  protected
    function GetAction(Obj: TObject): TsmxBaseCell;
    //procedure SetForm(Handle: HWND; Value: TsmxCustomForm);
    //procedure CloseForms;

    property ActionList: TsmxMActionsItems read FActionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertAction(AAction: TsmxBaseCell);
    procedure RemoveAction(AAction: TsmxBaseCell);

    property Action[Obj: TObject]: TsmxBaseCell read GetAction; default; // write SetForm; default;
  end;}

  { TsmxCustomForm }

  //TsmxFormManager = class;

  {TsmxCustomForm = class(TsmxControlCell)
  private
    FFormManager: TsmxFormManager;
    FParamList: TStrings;
    function GetFormParam(Key: String): String;
    function GetFormManager: TsmxFormManager;
    function GetParams: TStrings;
    procedure SetFormParam(Key: String; Value: String);
  protected
    function GetFormModalResult:  TModalResult; virtual;
    procedure SetFormModalResult(Value: TModalResult); virtual;

    property ParamList: TStrings read GetParams;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    procedure CloseForm; virtual;
    procedure ShowForm; virtual;
    function ShowModalForm: TModalResult; virtual;

    property FormManager: TsmxFormManager read GetFormManager;
    property FormModalResult: TModalResult read GetFormModalResult write SetFormModalResult;
    property FormParams[Key: String]: String read GetFormParam write SetFormParam;
  end;}

  { TsmxAlgorithm }

  TsmxAlgorithm = class(TsmxBaseCell)
  private
    //FAlgorithmCell: TsmxControlCell;
    //FAlgorithmList: TsmxCustomAlgorithmList;
    //FResultParams: Variant;
    //function GetAlgorithmList: TsmxCustomAlgorithmList;
  protected
    function GetCellCaption: String; virtual;
    function GetCellParams: Variant; virtual;
    function GetCellEnable: Boolean; virtual;
    function GetCellHotKey: Integer; virtual;
    function GetCellImageIndex: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    //procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetCellCaption(Value: String); virtual;
    procedure SetCellEnable(Value: Boolean); virtual;
    procedure SetCellHotKey(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;
  public
    procedure Execute; virtual;

    //property AlgorithmList: TsmxCustomAlgorithmList read GetAlgorithmList; //FAlgorithmList write SetAlgorithmList;
    //property AlgorithmCell: TsmxControlCell read FAlgorithmCell write FAlgorithmCell;
    property CellCaption: String read GetCellCaption write SetCellCaption;
    property CellParams: Variant read GetCellParams;
    property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    property CellHotKey: Integer read GetCellHotKey write SetCellHotKey;
    property CellImageIndex: Integer read GetCellImageIndex;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    //property ResultParams: Variant read FResultParams write FResultParams;
  end;

  { TsmxLibItem }

  TsmxLibItem = class(TsmxKitItem)
  private
    FLibHandle: THandle;
    FLibName: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property LibHandle: THandle read FLibHandle write FLibHandle;
    property LibName: String read FLibName write FLibName;
  end;

  { TsmxLibItems }

  TsmxLibItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxLibItem;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxLibItem;
    function FindByName(ALibName: String): TsmxLibItem;

    property Items[Index: Integer]: TsmxLibItem read GetItem {write SetItem}; default;
  end;

  { TsmxLibManager }

  EsmxLibManagerError = class(Exception);

  TsmxLibManager = class(TsmxComponent)
  private
    FLibList: TsmxLibItems;
    function GetLibrary(Name: String): THandle;
  protected
    procedure FreeLibs;

    property LibList: TsmxLibItems read FLibList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Libraries[Name: String]: THandle read GetLibrary; default;
  end;

  { TsmxToolBar }

  {TsmxToolBar = class(TsmxComponent)
  private
    FToolBar: TToolBar;
  protected
    property ToolBar: TToolBar read FToolBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddAlgorithm(Algorithm: TsmxAlgorithm); virtual;
    procedure DelAlgorithm(Algorithm: TsmxAlgorithm); virtual;
  end;}

implementation

uses
  DB, Variants, XMLDoc, StrUtils, smxFuncs, smxConsts;

{ TsmxBaseCfg }

constructor TsmxBaseCfg.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner);
  FCfgID := ACfgID;
  FCall := ACall;
  FTargetRequest := TsmxTargetRequest.Create(Self);
  FTargetRequest.Database := Database;
  FXMLDocIntf := NewXMLDocument;
  Initialize;
end;

destructor TsmxBaseCfg.Destroy;
begin
  FTargetRequest.Free;
  FDatabaseIntf := nil;
  FXMLDocIntf := nil;
  inherited Destroy;
end;

procedure TsmxBaseCfg.Clear;
begin
end;

{function TsmxBaseCfg.GetCfgName: String;
begin
  if FCfgName = '' then
  begin
    FTargetRequest['ConfID'] := IntToStr(FCfgID);
    FCfgName := FTargetRequest.DoRequest('select ConfName from tConfigs where ConfID = :ConfID');
  end;
  Result := FCfgName;
end;}

function TsmxBaseCfg.GetDatabase: IsmxDatabase;
begin
  if not(Assigned(FDatabaseIntf)) then
    FDatabaseIntf := IsmxDatabase(Integer(FCall(1)));
  Result := FDatabaseIntf;
end;

function TsmxBaseCfg.GetXMLText: String;
begin
  Result := String(UTF8Decode(FormatXMLData(WideString(FXMLDocIntf.XML.Text))));
end;

procedure TsmxBaseCfg.Finalize;
begin
  try
    WriteCell;
    SaveCell;
  except
    raise EsmxCfgError.CreateRes(@SCfgFinalizeError);
  end;
end;

procedure TsmxBaseCfg.Initialize;
begin
  try
    LoadCell;
    ReadCell;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;

procedure TsmxBaseCfg.LoadCell;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] :=
    FTargetRequest.DoRequest('select ConfBlob from tConfigs where ConfID = :ConfID');
  if FTargetRequest['ConfBlob'] <> '' then
  begin
    FXMLDocIntf.XML.Text := FTargetRequest['ConfBlob'];
    FXMLDocIntf.Active := True;
  end;
end;

procedure TsmxBaseCfg.ReadCell;
begin
end;

procedure TsmxBaseCfg.SaveCell;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  FTargetRequest.DoExecute('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');
end;

procedure TsmxBaseCfg.SetXMLText(Value: String);
var i: Integer; sl: TStrings;
begin
  sl := TStringList.Create;
  try
    sl.Text := Value;
    for i := 0 to sl.Count - 1 do
      sl[i] := TrimLeft(TrimRight(sl[i]));
    FXMLDocIntf.XML.Text :=
      String(UTF8Encode(WideString(AnsiReplaceStr(sl.Text, sLineBreak, ''))));
    try
      ReadCell;
    except
      raise EsmxCfgError.CreateRes(@SCfgInitializeError);
    end;
  finally
    sl.Free;
  end;
end;

procedure TsmxBaseCfg.WriteCell;
begin
end;

{ TsmxBaseCell }

constructor TsmxBaseCell.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner);
  FCfgID := ACfgID;
  FCall := ACall;
  //FCfg := CfgClass.Create(Self, FCfgID, FCall);

  //FCfg := IDToCfgClass(FCfgID, FCall).Create(Self, FCfgID, FCall);
  FCfg := NewCfg(Self, FCfgID, FCall);

  //FType := TsmxTypeCfg.Create(Self, FCfgID, FCall);
  //FCfg := FType.CfgClass.Create(Self, FCfgID, FCall);

  //FChildList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxBaseCell.Destroy;
begin
  DestroyChilds;
  //ClearParent;
  //FChildList.Free;

  FCfg.Free;
  //FType.Free;
  FDatabaseIntf := nil;
  inherited Destroy;
end;

{procedure TsmxBaseCell.ClearParent;
var i: Integer;
begin
  for i := ChildCount - 1 downto 0 do
    Childs[i].SetParentCell(nil);
end;}

procedure TsmxBaseCell.CreateChilds;
begin
end;

procedure TsmxBaseCell.DestroyChilds;
begin
end;

{function TsmxBaseCell.FindChildByID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;
var i: Integer;

  function Find(ACell: TsmxBaseCell): TsmxBaseCell;
  var i: Integer;
  begin
    Result := nil;
    for i := 0 to ACell.ChildCount - 1 do
    begin
      if ACell.Childs[i].CfgID = ACfgID then
      begin
        Result := ACell.Childs[i];
        Break;
      end;
      Result := Find(ACell.Childs[i]);
      if Assigned(Result) then
        Break;
    end;
  end;

begin
  Result := nil;
  for i := 0 to ChildCount - 1 do
  begin
    if Childs[i].CfgID = ACfgID then
    begin
      Result := Childs[i];
      Break;
    end;
    if AmongAll then
    begin
      Result := Find(Childs[i]);
      if Assigned(Result) then
        Break;
    end;
  end;
end;}

{function TsmxBaseCell.GetCfgClass: TsmxBaseCfgClass;
begin
  if not(Assigned(FCfgClass)) then
    FCfgClass := IDToCfgClass(FCfgID, FCall);
  Result := FCfgClass;
end;}

{function TsmxBaseCell.GetChildCount: Integer;
begin
  Result := FChildList.Count;
end;}

{function TsmxBaseCell.GetChild(Index: Integer): TsmxBaseCell;
begin
  Result := TsmxBaseCell(FChildList[Index]);
end;}

function TsmxBaseCell.GetDatabase: IsmxDatabase;
begin
  if not(Assigned(FDatabaseIntf)) then
    FDatabaseIntf := IsmxDatabase(Integer(Call(1)));
  Result := FDatabaseIntf;
end;

function TsmxBaseCell.GetImageList: TImageList;
begin
  if not(Assigned(FImageList)) then
    FImageList := TImageList(Integer(FCall(4)));
  Result := FImageList;
end;

function TsmxBaseCell.GetInternalObject: TObject;
begin
  Result := nil;
end;

{function TsmxBaseCell.GetOwnerCell: TsmxBaseCell;
var c: TsmxBaseCell;
begin
  Result := nil;
  c := Self;
  while Assigned(c.ParentCell) and not(c is TsmxCustomForm) do
    c := c.ParentCell;
  if c is TsmxCustomForm then
    Result := c;
end;}

function TsmxBaseCell.GetRootCell: TsmxBaseCell;
begin
  Result := Self;
  while Assigned(Result.FParentCell) do
    Result := Result.FParentCell;
end;

{function TsmxBaseCell.IDToCfgClass(ACfgID: Integer): TsmxBaseCfgClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ACfgID, FCall);
  try
    Result := t.CfgClass;
  finally
    t.Free;
  end;
end;}

{function TsmxBaseCell.IDToItemClass(ACfgID: Integer): TsmxBaseCellClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ACfgID, FCall);
  try
    Result := t.CellClass;
  finally
    t.Free;
  end;
end;}

procedure TsmxBaseCell.InitChilds;
begin
end;

{procedure TsmxBaseCell.InsertChild(AChild: TsmxBaseCell);
begin
  FChildList.Add(AChild);
end;}

procedure TsmxBaseCell.InstallParent;
begin
end;

{procedure TsmxBaseCell.RemoveChild(AChild: TsmxBaseCell);
begin
  FChildList.Remove(AChild);
end;}

procedure TsmxBaseCell.SetParentCell(AParent: TsmxBaseCell);
begin
  //if Assigned(FParentCell) then
    //FParentCell.RemoveChild(Self);
  FParentCell := AParent;
  //if Assigned(AParent) then
    //AParent.InsertChild(Self);
end;

procedure TsmxBaseCell.UnInstallParent;
begin
end;

{ TsmxTypeCfg }

procedure TsmxTypeCfg.LoadCell;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] :=
    FTargetRequest.DoRequest('select c2.ConfBlob from tConfigs c ' +
      'join tConfigs c2 on c2.ConfID = c.ConfConfID ' +
      'where c.ConfID = :ConfID');
  if FTargetRequest['ConfBlob'] <> '' then
  begin
    FXMLDocIntf.XML.Text := FTargetRequest['ConfBlob'];
    FXMLDocIntf.Active := True;
  end;
end;

procedure TsmxTypeCfg.ReadCell;
var r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('root');
  if not(Assigned(r)) then
    Exit;

  n := r.ChildNodes.FindNode('item');
  if Assigned(n) then
  begin
    CfgClassName := n.Attributes['cfgclassname'];
    CellClassName := n.Attributes['cellclassname'];
  end;
end;

procedure TsmxTypeCfg.SaveCell;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  FTargetRequest.DoExecute('update tConfigs c set c.ConfBlob = :ConfBlob ' +
    'where c.ConfID = ' +
    '(select c2.ConfConfID from tConfigs c2 where c2.ConfID = :ConfID');
end;

procedure TsmxTypeCfg.SetCfgClassName(Value: String);
begin
  if FCfgClassName <> Value then
  begin
    FCfgClassName := Value;
    FCfgClass := TsmxBaseCfgClass(FindClass(FCfgClassName));
  end;
end;

procedure TsmxTypeCfg.SetCellClassName(Value: String);
begin
  if FCellClassName <> Value then
  begin
    FCellClassName := Value;
    FCellClass := TsmxBaseCellClass(FindClass(FCellClassName));
  end;
end;

procedure TsmxTypeCfg.WriteCell;
var r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('root');
  if not(Assigned(r)) then
    r := FXMLDocIntf.AddChild('root');

  n := r.ChildNodes.FindNode('item');
  if not(Assigned(n)) then
    n := r.AddChild('type');
  n.Attributes['cfgclassname'] := CfgClassName;
  n.Attributes['cellclassname'] := CellClassName;
end;

{ TsmxControlCell }

{constructor TsmxControlCell.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  CreateChilds;
  InitChilds;
end;

destructor TsmxControlCell.Destroy;
begin
  DestroyChilds;
  inherited Destroy;
end;}

procedure TsmxControlCell.Apply;
begin
end;

{procedure TsmxControlCell.CreateChilds;
begin
end;

procedure TsmxControlCell.DestroyChilds;
begin
end;}

{function TsmxControlCell.GetInternalObject: TObject;
begin
  Result := nil;
end;}

function TsmxControlCell.GetCellAlign: TAlign;
begin
  Result := alNone;
end;

function TsmxControlCell.GetCellCursor: TCursor;
begin
  Result := crDefault;
end;

function TsmxControlCell.GetCellEnable: Boolean;
begin
  Result := False;
end;

function TsmxControlCell.GetCellHeight: Integer;
begin
  Result := 0;
end;

function TsmxControlCell.GetCellLeft: Integer;
begin
  Result := 0;
end;

{function TsmxControlCell.GetItemParent: TObject;
begin
  Result := nil;
end;}

function TsmxControlCell.GetCellTop: Integer;
begin
  Result := 0;
end;

function TsmxControlCell.GetCellVisible: Boolean;
begin
  Result := False;
end;

function TsmxControlCell.GetCellWidth: Integer;
begin
  Result := 0;
end;

{procedure TsmxControlCell.InitChilds;
begin
end;}

{procedure TsmxControlCell.InstallParent;
begin
end;}

procedure TsmxControlCell.Prepare(Forcibly: Boolean = False);
begin
end;

{procedure TsmxControlCell.PutParams(ParamList: Variant);
begin
end;}

procedure TsmxControlCell.SetCellAlign(Value: TAlign);
begin
end;

procedure TsmxControlCell.SetCellCursor(Value: TCursor);
begin
end;

procedure TsmxControlCell.SetCellEnable(Value: Boolean);
begin
end;

procedure TsmxControlCell.SetCellHeight(Value: Integer);
begin
end;

procedure TsmxControlCell.SetCellLeft(Value: Integer);
begin
end;

{procedure TsmxControlCell.SetItemParent(Value: TObject);
begin
end;}

procedure TsmxControlCell.SetCellTop(Value: Integer);
begin
end;

procedure TsmxControlCell.SetCellVisible(Value: Boolean);
begin
end;

procedure TsmxControlCell.SetCellWidth(Value: Integer);
begin
end;

{procedure TsmxControlCell.UnInstallParent;
begin
end;}

{ TsmxTargetRequest }

destructor TsmxTargetRequest.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  FDatabaseIntf := nil;  
  FRequestIntf := nil;
  inherited Destroy;
end;

procedure TsmxTargetRequest.ClearParams;
begin
  ParamList.Clear;
end;

function TsmxTargetRequest.GetParamList: TStrings;
begin
  if not(Assigned(FParamList)) then
    FParamList := TStringList.Create;
  Result := FParamList;
end;

function TsmxTargetRequest.GetParam(Key: String): String;
begin
  Result := ParamList.Values[AnsiUpperCase(Key)];
end;

procedure TsmxTargetRequest.DoExecute(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  const DSFrom: IsmxDataSet = nil);
begin
  if Assigned(FRequestIntf) then
  begin
    FRequestIntf.Close;
    if FRequestIntf.DataSetType <> RequestType then
      FRequestIntf := NewRequest('', RequestType);
  end else
    FRequestIntf := NewRequest('', RequestType);

  with FRequestIntf do
  begin
    if SQL.Text <> SQLText then
      SQL.Text := SQLText;
    PrepRequest(FRequestIntf, True, rtExecute, DSFrom);
  end;
end;

function TsmxTargetRequest.DoRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Res: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
begin
  with ForRequest(SQLText, RequestType, True, rtOpen, DSFrom) do
  try
    if Res = '' then
      Res := Fields[0].FieldName;
    Result := FieldByName(Res).Value;
    if VarIsNull(Result) then
      Result := '';
  finally
    Close;
  end;
end;

function TsmxTargetRequest.ForRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Get: Boolean = False; Mode: TsmxReturnType = rtOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  if Assigned(FRequestIntf) then
  begin
    FRequestIntf.Close;
    if FRequestIntf.DataSetType <> RequestType then
      FRequestIntf := NewRequest('', RequestType);
  end else
    FRequestIntf := NewRequest('', RequestType);

  with FRequestIntf do
    if SQL.Text <> SQLText then
      SQL.Text := SQLText;
  PrepRequest(FRequestIntf, Get, Mode, DSFrom);
  Result := FRequestIntf;
end;

function TsmxTargetRequest.NewRequest(SQLText: String = '';
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  Result := nil;
  if Assigned(Database) then
  begin
    Result := Database.GetNewDataSet(RequestType);
    Result.Database := Database;
    Result.SQL.Text := SQLText;
    PrepRequest(Result, False, rtOpen, DSFrom);
  end else
    raise EsmxTargetRequestError.CreateRes(@STargetRequestDatabaseError);
end;

function TsmxTargetRequest.PrepRequest(const ARequest: IsmxDataSet; Get: Boolean = True;
  Mode: TsmxReturnType = rtOpen; const DSFrom: IsmxDataSet = nil): Boolean;
var i: Integer; f: IsmxField; v: Variant;
begin
  Result := False;
  with ARequest do
  begin
    Close;
    if not Prepared then
      Prepare;
    for i := 0 to ParamCount - 1 do
      if Params[i].ParamType in [ptInput, ptInputOutput] then
      begin
        f := nil;
        if Assigned(DSFrom) then
          f := DSFrom.FindField(Params[i].ParamName);
        if Assigned(f) then
          Params[i].AssignParam(f)
        else
        begin
          if TargetParams[Params[i].ParamName] = '' then
            Params[i].Value := Null else
            Params[i].Value := TargetParams[Params[i].ParamName];
        end;
      end;
    if Get then
    begin
      case Mode of
        rtOpen: Open;
        rtExecute: Execute;
      end;
      for i := 0 to ParamCount - 1 do
        if Params[i].ParamType in [ptOutput, ptInputOutput] then
        begin
          if VarIsNull(Params[i].Value) then
            TargetParams[Params[i].ParamName] := '' else
            TargetParams[Params[i].ParamName] := Params[i].Value;
        end else
        if Params[i].ParamType in [ptResult] then
        begin
          TargetParams[Params[i].ParamName] := Params[i].Value;
          v := Params[i].Value;
        end;

      case Mode of
        rtOpen: Result := RecordCount > 0;
        rtExecute: Result := v = 0;
      end;
    end;
  end;
end;

procedure TsmxTargetRequest.SetDatabase(const Value: IsmxDatabase);
begin
  if FDatabaseIntf <> Value then
  begin
    if Assigned(FRequestIntf) then
    begin
      FRequestIntf.Close;
      FRequestIntf := nil;
    end;
    {if Assigned(FDatabaseIntf) then
    begin
      if FDatabaseIntf.InTransaction then
        FDatabaseIntf.RollbackTrans;
    end;}
    FDatabaseIntf := Value;
  end;
end;

procedure TsmxTargetRequest.SetParam(Key: String; const Value: String);
begin
  ParamList.Values[AnsiUpperCase(Key)] := Value;
end;

{function TsmxTargetRequest.StreamToStr(Stream: TStream): String;
var Len: Integer;
begin
  with Stream do
  begin
    Position := 0;
    Len := Size;
    SetLength(Result, Len);
    ReadBuffer(Pointer(Result)^, Len);
  end;
end;

function TsmxTargetRequest.StrToStream(Str: String): TStream;
var Len: Integer;
begin
  Result := TMemoryStream.Create;
  with Result do
  begin
    Len := Length(Str);
    Size := Len;
    WriteBuffer(Pointer(Str)^, Len);
    Position := 0;
  end;
end;}

{ TsmxKitItem }

constructor TsmxKitItem.Create(AKit: TsmxKit);
begin
  inherited Create;
  FKit := AKit;
end;

{ TsmxKit }

constructor TsmxKit.Create(AItemClass: TsmxKitItemClass);
begin
  inherited Create;
  FList := TList.Create;
  FCellClass := AItemClass;
end;

destructor TsmxKit.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TsmxKit.Add: TsmxKitItem;
begin
  Result := FCellClass.Create(Self);
  FList.Add(Result);
end;

procedure TsmxKit.Clear;
var i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    TsmxKitItem(FList[i]).Free;
    FList.Delete(i);
    //Remove(FList[i]);
  end;
end;

function TsmxKit.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TsmxKit.GetItem(Index: Integer): TsmxKitItem;
begin
  Result := FList[Index];
end;

function TsmxKit.Insert(Index: Integer): TsmxKitItem;
begin
  FList.Insert(Index, nil);
  Result := FCellClass.Create(Self);
  FList[Index] := Result;
end;

procedure TsmxKit.Remove(AItem: TsmxKitItem);
var temp: Integer;
begin
  temp := FList.Remove(AItem);
  if temp >= 0 then
    AItem.Free;
end;

{procedure TsmxKit.SetItem(Index: Integer; Value: TsmxKitItem);
var temp: TsmxKitItem;
begin
  if Value.ClassType <> FCellClass then
    raise EsmxKitItemError.CreateRes(@SKitItemInvalid);
  temp := FList[Index];
  FList[Index] := Value;
  if Assigned(temp) then
    temp.Free;
end;}

{ TsmxHKitItem }

constructor TsmxHKitItem.Create(AHKit: TsmxHKit);//(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass);
begin
  inherited Create;
  //FParent := AParent;
  //FCellClass := AItemClass;
  FHKit := AHKit;
  FCellClass := FHKit.FCellClass;
  FList := TList.Create;
end;

destructor TsmxHKitItem.Destroy;
begin
  //if Assigned(FList) then
  //begin
    Clear;
    FList.Free;
  //end;
  inherited Destroy;
end;

function TsmxHKitItem.Add: TsmxHKitItem;
begin
  Result := FCellClass.Create(FHKit); //(Self, FCellClass);
  Result.FParent := Self;
  //if not(Assigned(FList)) then
  //  FList := TList.Create;
  FList.Add(Result);
end;

procedure TsmxHKitItem.Clear;
var i: Integer;
begin
  //if Assigned(FList) then
    for i := FList.Count - 1 downto 0 do
    begin
      TsmxHKitItem(FList[i]).Free;
      FList.Delete(i);
      //Remove(FList[i]);
    end;
end;

function TsmxHKitItem.GetCount: Integer;
begin
  //if Assigned(FList) then
    Result := FList.Count; //else
    //Result := 0;
end;

function TsmxHKitItem.GetItem(Index: Integer): TsmxHKitItem;
begin
  //if Assigned(FList) then
    Result := FList[Index]; //else
    //raise EsmxKitItemError.CreateRes(@SKitItemIndexError);
end;

function TsmxHKitItem.HasChilds: Boolean;
begin
  if FList.Count = 0 then
    Result := False else
    Result := True;
end;

function TsmxHKitItem.HasParent: Boolean;
begin
  if FParent = FHKit.FRoot then
    Result := False else
    Result := True;
end;

procedure TsmxHKitItem.Remove(AItem: TsmxHKitItem);
var temp: Integer;
begin
  //if Assigned(FList) then
  //begin
    temp := FList.Remove(AItem);
    if temp >= 0 then
      AItem.Free;
  //end else
    //raise EsmxKitItemError.CreateRes(@SKitItemNotFound);
end;

{procedure TsmxHKitItem.SetItem(Index: Integer; Value: TsmxHKitItem);
var temp: TsmxHKitItem;
begin
  if Value.ClassType <> FCellClass then
    raise EsmxKitItemError.CreateRes(@SKitItemInvalid);
  if Assigned(Value.FParent) then
    raise EsmxKitItemError.CreateRes(@SKitItemReset);
  if Assigned(FList) then
  begin
    temp := FList[Index];
    FList[Index] := Value;
    Value.FParent := Self;
    if Assigned(temp) then
      temp.Free;
  end else
    raise EsmxKitItemError.CreateRes(@SKitItemIndexError);
end;}

{ TsmxHKit }

constructor TsmxHKit.Create(AItemClass: TsmxHKitItemClass);
begin
  inherited Create;
  FCellClass := AItemClass;
  FRoot := FCellClass.Create(Self); //(nil, FCellClass);
end;

destructor TsmxHKit.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

{ TsmxCallBackParam }

{constructor TsmxCallBackParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamIndex := 0;
  FParamValue := Unassigned;
end;}

{ TsmxCallBackParams }

{function TsmxCallBackParams.Add: TsmxCallBackParam;
begin
  Result := TsmxCallBackParam(inherited Add);
end;

function TsmxCallBackParams.FindByIndex(AIndex: Integer): TsmxCallBackParam;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ParamIndex = AIndex then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxCallBackParams.GetInternalObject(Index: Integer): TsmxCallBackParam;
begin
  Result := TsmxCallBackParam(inherited Items[Index]);
end;}

{ TsmxCallBack }

{constructor TsmxCallBack.Create(AOwner: TComponent);
begin
  FParamList := TsmxCallBackParams.Create(TsmxCallBackParam);
end;

destructor TsmxCallBack.Destroy;
begin
  FParamList.Free;
end;}

{procedure TsmxCallBack.AddParam(Index: Integer; Value: Variant);
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if not(Assigned(p)) then
    with FParamList.Add do
    begin
      ParamIndex := Index;
      ParamValue := Value;
    end
  else
    //raise EsmxCallBackError.CreateRes(@SCallBackIndexIsBusy);
    p.ParamValue := Value;
end;}

{procedure TsmxCallBack.DelParam(Index: Integer);
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    FParamList.Remove(p); //else
    //raise EsmxCallBackError.CreateRes(@SCallBackIndexError);
end;}

{function TsmxCallBack.GetParam(Index: Integer): Variant;
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Unassigned;
end;

procedure TsmxCallBack.SetParam(Index: Integer; Value: Variant);
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    p.ParamValue := Value
  else
    with FParamList.Add do
    begin
      ParamIndex := Index;
      ParamValue := Value;
    end;
  //else
    //raise EsmxCallBackError.CreateRes(@SCallBackIndexError);
end;}

{ TsmxMFormsItem }

{constructor TsmxMFormsItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FFormHandle := 0;
  FFormObj := nil;
  FFormPtr := nil;
end;}

{ TsmxMFormsItems }

{function TsmxMFormsItems.Add: TsmxMFormsItem;
begin
  Result := TsmxMFormsItem(inherited Add);
end;

function TsmxMFormsItems.FindByForm(AForm: TsmxBaseCell): TsmxMFormsItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FormPtr = AForm then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxMFormsItems.FindByHandle(AHandle: HWND): TsmxMFormsItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FormHandle = AHandle then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxMFormsItems.GetInternalObject(Index: Integer): TsmxMFormsItem;
begin
  Result := TsmxMFormsItem(inherited Items[Index]);
end;}

{ TsmxFormManager }

{constructor TsmxFormManager.Create(AOwner: TComponent);
begin
  FFormList := TsmxMFormsItems.Create(TsmxMFormsItem);
end;

destructor TsmxFormManager.Destroy;
begin
  CloseForms;
  FFormList.Free;
end;

procedure TsmxFormManager.CloseForms;
var i: Integer; f: TsmxBaseCell;
begin
  for i := FFormList.Count - 1 downto 0 do
  begin
    f := FFormList[i].FormPtr;
    if Assigned(f) then
      f.Free;
  end;
end;

function TsmxFormManager.GetForm(Handle: HWND): TsmxBaseCell;
var f: TsmxMFormsItem;
begin
  f := FFormList.FindByHandle(Handle);
  if Assigned(f) then
    Result := f.FormPtr else
    Result := nil;
end;}

{procedure TsmxFormManager.SetForm(Handle: HWND; Value: TsmxCustomForm);
var f: TsmxMFormsItem;
begin
  f := FFormList.FindByHandle(Handle);
  if Assigned(f) then
    f.FormPtr := Value
  else
    with FFormList.Add do
    begin
      FormHandle := Handle;
      FormPtr := Value;
    end;
end;}

{procedure TsmxFormManager.InsertForm(AForm: TsmxBaseCell);
var f: TsmxMFormsItem; h: HWND; c: TObject;
begin
  f := FFormList.FindByForm(AForm);
  if not(Assigned(f)) then
  begin
    h := 0;
    c := AForm.GetInternalObject;
    if Assigned(c) then
      //if c is TCustomForm then
      //  h := TCustomForm(c).Handle;
      if c is TWinControl then
        h := TWinControl(c).Handle;
    with FFormList.Add do
    begin
      FormHandle := h;
      FormObj := c;
      FormPtr := AForm;
    end;
  end;
end;

procedure TsmxFormManager.RemoveForm(AForm: TsmxBaseCell);
var f: TsmxMFormsItem;
begin
  f := FFormList.FindByForm(AForm);
  if Assigned(f) then
    FFormList.Remove(f);
end;}

{ TsmxFormItem }

constructor TsmxFormItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FForm := nil;
  //FFormConfID := 0;
  FFormHandle := 0;
  FFormCfgID := 0;
end;

{ TsmxFormItems }

function TsmxFormItems.Add: TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Add);
end;

function TsmxFormItems.FindByForm(AForm: TsmxBaseCell): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Form = AForm then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxFormItems.FindByHandle(AHandle: HWND): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FormHandle = AHandle then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxFormItems.FindByCfgID(ACfgID: Integer): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FormCfgID = ACfgID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxFormItems.GetItem(Index: Integer): TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Items[Index]);
end;

{procedure TsmxFormItems.SetItem(Index: Integer; Value: TsmxFormItem);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxFormManager }

constructor TsmxFormManager.Create(AOwner: TComponent);
begin
  //FFormList := TList.Create;
  FFormList := TsmxFormItems.Create(TsmxFormItem);
end;

destructor TsmxFormManager.Destroy;
begin
  DestroyForms;
  //FFormList.Free;
  FFormList.Free;
end;

procedure TsmxFormManager.DestroyForms;
var i: Integer;
begin
  {for i := FFormList.Count - 1 downto 0 do
  begin
    TsmxBaseCell(FFormList[i]).Free;
    //FFormList.Delete(i);
  end;}
  for i := FFormList.Count - 1 downto 0 do
    FFormList[i].Form.Free;
end;

function TsmxFormManager.FindByHandle(AHandle: HWND): TsmxBaseCell;
var f: TsmxFormItem; //i: Integer; c: TObject; h: HWND;
begin
  Result := nil;
  f := FormList.FindByHandle(AHandle);
  if Assigned(f) then
    Result := f.Form;

  {for i := 0 to FormCount - 1 do
  begin
    h := 0;
    c := FormList[i].GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        h := TWinControl(c).Handle;
    if AHandle = h then
    begin
      Result := FormList[i];
      Break;
    end;
  end;}
end;

function TsmxFormManager.FindByCfgID(ACfgID: Integer): TsmxBaseCell;
var f: TsmxFormItem; //i: Integer;
begin
  Result := nil;
  f := FormList.FindByCfgID(ACfgID);
  if Assigned(f) then
    Result := f.Form;

  {for i := 0 to FormCount - 1 do
    if FormList[i].CfgID = ACfgID then
    begin
      Result := FormList[i];
      Break;
    end;}
end;

{function TsmxFormManager.GetFormCount: Integer;
begin
  Result := FFormList.Count;
end;}

{function TsmxFormManager.GetForm(Index: Integer): TsmxBaseCell;
begin
  Result := TsmxBaseCell(FFormList[Index]);
end;}

{function TsmxFormManager.GetHandle(Handle: HWND): TsmxBaseCell;
var i: Integer; c: TObject; h: HWND;
begin
  Result := nil;
  for i := 0 to FormCount - 1 do
  begin
    h := 0;
    c := FormList[i].GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        h := TWinControl(c).Handle;
    if Handle = h then
    begin
      Result := FormList[i];
      Break;
    end;
  end;
end;}

{function TsmxFormManager.HandleOfForm(AForm: TsmxBaseCell): HWND;
var i: Integer; c: TObject;
begin
  Result := 0;
  i := FFormList.IndexOf(AForm);
  if i >= 0 then
  begin
    c := FormList[i].GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        Result := TWinControl(c).Handle;
  end;
end;}

procedure TsmxFormManager.InsertForm(AForm: TsmxBaseCell);
var f: TsmxFormItem; c: TObject; //i: Integer;
begin
  {i := FFormList.IndexOf(AForm);
  if i = -1 then
    FFormList.Add(AForm);}
  f := FormList.FindByForm(AForm);
  if not Assigned(f) then
    with FormList.Add do
    begin
      Form := AForm;
      c := AForm.GetInternalObject;
      if c is TWinControl then
        FormHandle := TWinControl(c).Handle;
      FormCfgID := AForm.CfgID;
    end;
end;

procedure TsmxFormManager.RemoveForm(AForm: TsmxBaseCell);
var f: TsmxFormItem;
begin
  //FFormList.Remove(AForm);
  f := FormList.FindByForm(AForm);
  if Assigned(f) then
    FormList.Remove(f);
end;

{ TsmxMActionsItem }

{constructor TsmxMActionsItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FActionObj := nil;
  FActionPtr := nil;
end;}

{ TsmxMActionsItems }

{function TsmxMActionsItems.Add: TsmxMActionsItem;
begin
  Result := TsmxMActionsItem(inherited Add);
end;

function TsmxMActionsItems.FindByAction(AAction: TsmxBaseCell): TsmxMActionsItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ActionPtr = AAction then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxMActionsItems.FindByObject(AObject: TObject): TsmxMActionsItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ActionObj = AObject then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxMActionsItems.GetInternalObject(Index: Integer): TsmxMActionsItem;
begin
  Result := TsmxMActionsItem(inherited Items[Index]);
end;}

{ TsmxManagerActions }

{constructor TsmxManagerActions.Create(AOwner: TComponent);
begin
  FActionList := TsmxMActionsItems.Create(TsmxMActionsItem);
end;

destructor TsmxManagerActions.Destroy;
begin
  FActionList.Free;
end;

function TsmxManagerActions.GetAction(Obj: TObject): TsmxBaseCell;
var a: TsmxMActionsItem;
begin
  a := FActionList.FindByObject(Obj);
  if Assigned(a) then
    Result := a.ActionPtr else
    Result := nil;
end;

procedure TsmxManagerActions.InsertAction(AAction: TsmxBaseCell);
var a: TsmxMActionsItem; c: TObject;
begin
  a := FActionList.FindByAction(AAction);
  if not(Assigned(a)) then
  begin
    c := AAction.GetInternalObject;
    with FActionList.Add do
    begin
      ActionObj := c;
      ActionPtr := AAction;
    end;
  end;
end;

procedure TsmxManagerActions.RemoveAction(AAction: TsmxBaseCell);
var a: TsmxMActionsItem;
begin
  a := FActionList.FindByAction(AAction);
  if Assigned(a) then
    FActionList.Remove(a);
end;}

{ TsmxCustomForm }

{constructor TsmxCustomForm.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FPageManager := TsmxCustomPageManager(IDToItemClass(Cfg.PageManager.CfgID).Create(Self, Cfg.PageManager.CfgID, Call));
  SetSizePosChilds;
end;}

{destructor TsmxCustomForm.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

procedure TsmxCustomForm.CloseForm;
begin
end;

function TsmxCustomForm.GetFormModalResult:  TModalResult;
begin
  Result := mrNone;
end;

function TsmxCustomForm.GetFormParam(Key: String): String;
begin
  Result := Params.Values[AnsiUpperCase(Key)];
end;

function TsmxCustomForm.GetFormManager: TsmxFormManager;
begin
  if not(Assigned(FFormManager)) then
    FFormManager := TsmxFormManager(Integer(Call(2)));
  Result := FFormManager;
end;

function TsmxCustomForm.GetParams: TStrings;
begin
  if not(Assigned(FParamList)) then
    FParamList := TStringList.Create;
  Result := FParamList;
end;

procedure TsmxCustomForm.SetFormModalResult(Value: TModalResult);
begin
end;

procedure TsmxCustomForm.SetFormParam(Key: String; Value: String);
begin
  Params.Values[AnsiUpperCase(Key)] := Value;
end;

procedure TsmxCustomForm.ShowForm;
begin
end;

function TsmxCustomForm.ShowModalForm: TModalResult;
begin
  Result := mrNone;
end;}

{ TsmxAlgorithm }

procedure TsmxAlgorithm.Execute;
begin
end;

{function TsmxAlgorithm.GetAlgorithmList: TsmxCustomAlgorithmList;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomAlgorithmList then
      Result := TsmxCustomAlgorithmList(ParentCell);
end;}

function TsmxAlgorithm.GetCellCaption: String;
begin
  Result := '';
end;

function TsmxAlgorithm.GetCellParams: Variant;
begin
  Result := Unassigned;
end;

function TsmxAlgorithm.GetCellEnable: Boolean;
begin
  Result := False;
end;

function TsmxAlgorithm.GetCellHotKey: Integer;
begin
  Result := 0;
end;

function TsmxAlgorithm.GetCellImageIndex: Integer;
begin
  Result := -1;
end;

function TsmxAlgorithm.GetCellVisible: Boolean;
begin
  Result := False;
end;

{procedure TsmxAlgorithm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
end;}

procedure TsmxAlgorithm.SetCellCaption(Value: String);
begin
end;

procedure TsmxAlgorithm.SetCellEnable(Value: Boolean);
begin
end;

procedure TsmxAlgorithm.SetCellHotKey(Value: Integer);
begin
end;

procedure TsmxAlgorithm.SetCellVisible(Value: Boolean);
begin
end;

{ TsmxLibItem }

constructor TsmxLibItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FLibHandle := 0;
  FLibName := '';
end;

{ TsmxLibItems }

function TsmxLibItems.Add: TsmxLibItem;
begin
  Result := TsmxLibItem(inherited Add);
end;

function TsmxLibItems.FindByName(ALibName: String): TsmxLibItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].LibName, ALibName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxLibItems.GetItem(Index: Integer): TsmxLibItem;
begin
  Result := TsmxLibItem(inherited Items[Index]);
end;

{procedure TsmxLibItems.SetItem(Index: Integer; Value: TsmxLibItem);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxLibManager }

constructor TsmxLibManager.Create(AOwner: TComponent);
begin
  FLibList := TsmxLibItems.Create(TsmxLibItem);
end;

destructor TsmxLibManager.Destroy;
begin
  FreeLibs;
  FLibList.Free;
end;

procedure TsmxLibManager.FreeLibs;
var i: Integer; h: THandle;
begin
  for i := FLibList.Count - 1 downto 0 do
  begin
    h := FLibList[i].LibHandle;
    if h > 0 then
      try
        FreeLibrary(h);
      except
        raise EsmxLibManagerError.CreateRes(@SLibManagerFreeError);
      end;
  end;
end;

function TsmxLibManager.GetLibrary(Name: String): THandle;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(Name);
  if not Assigned(l) then
  begin
    l := FLibList.Add;
    with l do
      try
        LibHandle := LoadLibrary(PChar(Name));
        LibName := Name;
      except
        raise EsmxLibManagerError.CreateRes(@SLibManagerLoadError);
      end;
  end;
  Result := l.LibHandle;
end;

{ TsmxToolBar }

{constructor TsmxToolBar.Create(AOwner: TComponent);
begin
  FToolBar := ToolBar.Create(Self);
end;

destructor TsmxToolBar.Destroy;
begin
  FToolBar.Free;
end;}

end.

