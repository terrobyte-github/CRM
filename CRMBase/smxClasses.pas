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
    //FCall: TsmxFuncCallBack;
    //FCfgName: String;
    FDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    //FID: Integer;
    FTargetRequest: TsmxTargetRequest;
    FXMLDocIntf: IXMLDocument;
    //function GetCfgName: String;
    //function GetDatabase: IsmxDatabase;
  protected
    procedure LoadCfg; virtual;
    procedure ReadCfg; virtual;
    procedure SaveCfg; virtual;
    procedure WriteCfg; virtual;
    function GetXMLText: String; virtual;
    procedure SetXMLText(Value: String); virtual;

    property TargetRequest: TsmxTargetRequest read FTargetRequest;
    property XMLDoc: IXMLDocument read FXMLDocIntf;
    property XMLText: String read GetXMLText write SetXMLText;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Finalize; //virtual;
    procedure Initialize; //virtual;

    //property Call: TsmxFuncCallBack read FCall;
    //property CfgName: String read GetCfgName;
    property Database: IsmxDatabase read FDatabaseIntf; //GetDatabase;
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
    //FCall: TsmxFuncCallBack;
    FCfg: TsmxBaseCfg;
    //FCfgClass: TsmxBaseCfgClass;
    FCellList: TList;
    FDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    FID: Integer;
    //FImageList: TImageList;
    FParentCell: TsmxBaseCell;
    //FType: TsmxTypeCfg;
    //function GetCfgClass: TsmxBaseCfgClass;
    //procedure ClearParent;
    function GetCellCount: Integer;
    function GetCell(Index: Integer): TsmxBaseCell;
    //function GetDatabase: IsmxDatabase;
    //function GetImageList: TImageList;
    //function GetOwnerCell: TsmxBaseCell;
    function GetRootCell: TsmxBaseCell;
    //procedure InsertChild(AChild: TsmxBaseCell);
    //procedure RemoveChild(AChild: TsmxBaseCell);
  protected
    procedure CreateChilds; virtual;
    procedure DestroyChilds; virtual;
    //function GetCompID: String; virtual;
    function GetInternalObject: TObject; virtual;
    //function IDToCfgClass(ACfgID: Integer): TsmxBaseCfgClass; virtual;
    //function IDToItemClass(ACfgID: Integer): TsmxBaseCellClass; virtual;
    procedure InitChilds; virtual;
    procedure InstallParent; virtual;
    procedure SetParentCell(AParent: TsmxBaseCell); virtual;
    procedure UnInstallParent; virtual;
    procedure Initialize; virtual;
    procedure UnInitialize; virtual;

    property Cfg: TsmxBaseCfg read FCfg; //GetCfg;
    //property CfgClass: TsmxBaseCfgClass read GetCfgClass;
    property CellList: TList read FCellList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;
    function FindCellByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;

    //property Call: TsmxFuncCallBack read FCall;
    property CellCount: Integer read GetCellCount;
    property Cells[Index: Integer]: TsmxBaseCell read GetCell;
    property Database: IsmxDatabase read FDatabaseIntf; //GetDatabase;
    property CfgID: Integer read FCfgID;
    property ID: Integer read FID default 0;
    //property ImageList: TImageList read GetImageList;
    //property Item: TObject read GetInternalObject;
    //property OwnerCell: TsmxBaseCell read GetOwnerCell;
    property ParentCell: TsmxBaseCell read FParentCell write SetParentCell;
    property RootCell: TsmxBaseCell read GetRootCell;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

  { TsmxCellCfg }

  TsmxCellCfg = class(TsmxBaseCfg)
  private
    //FCall: TsmxFuncCallBack;
    //FCfgName: String;
    //FDatabaseIntf: IsmxDatabase;
    //FCfgID: Integer;
    //FTargetRequest: TsmxTargetRequest;
    //FXMLDocIntf: IXMLDocument;
    //function GetCfgName: String;
    //function GetDatabase: IsmxDatabase;
  protected
    procedure LoadCfg; override;
    //procedure ReadCfg; virtual;
    procedure SaveCfg; override;
    //procedure WriteCfg; virtual;
    //function GetXMLText: String; override;
    //procedure SetXMLText(Value: String); override;

    //property TargetRequest: TsmxTargetRequest read FTargetRequest;
    //property XMLDoc: IXMLDocument read FXMLDocIntf;
    //property XMLText: String read GetXMLText write SetXMLText;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      //ACfgID: Integer; AID: Integer = 0); override;
    //destructor Destroy; override;
    //procedure Clear; virtual;
    //procedure Finalize; virtual;
    //procedure Initialize; virtual;

    //property Call: TsmxFuncCallBack read FCall;
    //property CfgName: String read GetCfgName;
    //property Database: IsmxDatabase read FDatabaseIntf; //GetDatabase;
    //property CfgID: Integer read FCfgID;
  end;

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
    procedure LoadCfg; override;
    procedure ReadCfg; override;
    procedure SaveCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

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

  //EsmxTargetRequestError = class(Exception);

  //TsmxReturnType = (rtOpen, rtExecute);

  TsmxTargetRequest = class(TsmxComponent)
  private
    FDatabaseIntf: IsmxDatabase;
    FRequestIntf: IsmxDataSet;
    FParamList: TStrings;
    function GetParamList: TStrings;
    function GetValue(Key: String): String;
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetValue(Key: String; const Value: String);
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
      Get: Boolean = False; Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function NewRequest(SQLText: String = ''; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function PrepRequest(const ARequest: IsmxDataSet; Get: Boolean = True;
      Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): Boolean;
    //function StreamToStr(Stream: TStream): String;
    //function StrToStream(Str: String): TStream;

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[Key: String]: String read GetValue write SetValue; default;
  end;

  { TsmxKitItem }

  //EsmxKitItemError = class(Exception);

  TsmxKit = class;

  TsmxKitItem = class(TObject)
  private
    FKit: TsmxKit;
    function GetIndex: Integer;
  public
    constructor Create(AKit: TsmxKit); virtual;

    property ItemIndex: Integer read GetIndex;
    property Kit: TsmxKit read FKit;
  end;

  TsmxKitItemClass = class of TsmxKitItem;

  { TsmxKit }

  EsmxKitError = class(Exception);

  TsmxKit = class(TObject)
  private
    FList: TList;
    FCellClass: TsmxKitItemClass;
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxKitItem;
    //procedure SetItem(Index: Integer; Value: TsmxKitItem);
  public
    constructor Create(AItemClass: TsmxKitItemClass);
    destructor Destroy; override;
    function Add: TsmxKitItem;
    procedure Clear;
    //function IndexOf(AItem: TsmxKitItem): Integer;
    //function Insert(Index: Integer): TsmxKitItem;
    procedure Remove(AItem: TsmxKitItem);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem; default; //write SetItem
  end;

  { TsmxHKitItem }

  //TsmxHKitItem = class;
  //TsmxHKitItemClass = class of TsmxHKitItem;

  TsmxHKit = class;

  TsmxHKitItem = class(TObject)
  private
    FList: TList;
    //FCellClass: TsmxHKitItemClass;
    FParent: TsmxHKitItem;
    FHKit: TsmxHKit;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TsmxHKitItem;
    //procedure SetItem(Index: Integer; Value: TsmxHKitItem);
  public
    constructor Create(AHKit: TsmxHKit); virtual; //(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass); virtual;
    destructor Destroy; override;
    function Add: TsmxHKitItem;
    procedure Clear;
    function HasChilds: Boolean;
    function HasParent: Boolean;
    procedure Remove(AItem: TsmxHKitItem);

    property Count: Integer read GetCount;
    property HKit: TsmxHKit read FHKit;
    property ItemIndex: Integer read GetIndex;
    property Items[Index: Integer]: TsmxHKitItem read GetItem; default; //write SetItem
    property Parent: TsmxHKitItem read FParent;
  end;

  TsmxHKitItemClass = class of TsmxHKitItem;

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
    function GetItem(Index: Integer): TsmxCallBackParam;
  public
    function Add: TsmxCallBackParam;
    function FindByIndex(AIndex: Integer): TsmxCallBackParam;

    property Items[Index: Integer]: TsmxCallBackParam read GetItem; default;
  end;}

  { TsmxCallBack }

  {TsmxCallBack = class(TsmxComponent)
  private
    FParamList: TsmxCallBackParams;
  protected
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; Value: Variant);
    
    property ParamList: TsmxCallBackParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ParamValues[Index: Integer]: Variant read GetValue write SetValue; default;
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

  {TsmxFormItem = class(TsmxKitItem)
  private
    FForm: TsmxBaseCell;
    FFormHandle: HWND;
    FFormCfgID: Integer;
    FFormID: Integer;
    //FFormComboID: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property Form: TsmxBaseCell read FForm write FForm;
    property FormHandle: HWND read FFormHandle write FFormHandle;
    property FormCfgID: Integer read FFormCfgID write FFormCfgID;
    property FormID: Integer read FFormID write FFormID;
    //property FormComboID: String read FFormComboID write FFormComboID;
  end;}

  { TsmxFormItems }

  {TsmxFormItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxFormItem;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxFormItem;
    function FindByForm(AForm: TsmxBaseCell): TsmxFormItem;
    function FindByHandle(AHandle: HWND): TsmxFormItem;
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxFormItem;
    //function FindByComboID(AComboID: String): TsmxFormItem;

    property Items[Index: Integer]: TsmxFormItem read GetItem; default; //write SetItem
  end;}

  { TsmxFormManager }

  {TsmxFormManager = class(TsmxComponent)
  private
    //FFormList: TList;
    FFormList: TsmxFormItems;
    function GetForm(Index: Integer): TsmxBaseCell;
    function GetFormCount: Integer;
    //function GetHandle(Handle: HWND): TsmxBaseCell;
    procedure DestroyForms;
  protected
    property FormList: TsmxFormItems read FFormList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
    //function FindByComboID(AComboID: String): TsmxBaseCell;
    function FindByHandle(AHandle: HWND): TsmxBaseCell;
    //function HandleOfForm(AForm: TsmxBaseCell): HWND;
    procedure InsertForm(AForm: TsmxBaseCell);
    procedure RemoveForm(AForm: TsmxBaseCell);

    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TsmxBaseCell read GetForm; default;
    //property FormList: TsmxFormItems read FFormList;
    //property Handles[Handle: HWND]: TsmxBaseCell read GetHandle;
  end;}

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

  { TsmxAlgorithmCfg }

  {TsmxAlgorithmCfg = class(TsmxBaseCfg)
  private
    FAlgCaption: String;
    FAlgHotKey: Integer;
    FAlgImageIndex: Integer;
    FAlgLibrary: String;
    FAlgParams: TsmxLocationParams;
    FAlgProcedure: String;
    function GetAlgParams: TsmxLocationParams;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    //constructor Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack); override;
    //destructor Destroy; override;
    procedure Clear; override;
    //procedure Initialize; override;

    property AlgCaption: String read FAlgCaption write FAlgCaption;
    property AlgHotKey: Integer read FAlgHotKey write FAlgHotKey;
    property AlgImageIndex: Integer read FAlgImageIndex write FAlgImageIndex;
    property AlgLibrary: String read FAlgLibrary write FAlgLibrary;
    property AlgParams: TsmxLocationParams read GetAlgParams;
    property AlgProcedure: String read FAlgProcedure write FAlgProcedure;
  end;}

  { TsmxParam }

  TsmxParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxParams }

  //EsmxParamsError = class(Exception);

  TsmxParams = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxParam;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  public
    function Add: TsmxParam;
    function FindByName(AParamName: String): TsmxParam;
    //function ParamByName(AParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem {write SetItem}; default;
    property Values[Name: String]: Variant read GetValue write SetValue;
  end;

  { TsmxAlgorithm }

  TsmxAlgorithm = class(TsmxBaseCell)
  private
    //FAlgorithmCell: TsmxControlCell;
    //FAlgorithmList: TsmxCustomAlgorithmList;
    //FParamList: TStrings;

    //FParamList: TsmxParams;
    //FAlgorithmParams: TsmxParams;
    FParams: TsmxParams;

    //FResultParams: Variant;
    //function GetAlgorithmList: TsmxCustomAlgorithmList;
    //function GetParamList: TStrings;

    //function GetParamList: TsmxParams;
    //function GetCount: Integer;
    //function GetParam(Index: Integer): TsmxParam;
    //function GetValue(Name: String): Variant;
    //procedure SetValue(Name: String; Value: Variant);

    //function GetAlgorithmParams: TsmxParams;
    function GetParams: TsmxParams;
    //function GetParam(Key: String): String;
    //procedure SetParam(Key: String; Value: String);
  protected
    function GetCellCaption: String; virtual;
    //function GetCellParams: Variant; virtual;
    function GetCellEnable: Boolean; virtual;
    function GetCellHotKey: Integer; virtual;
    function GetCellImageIndex: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    //procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetCellCaption(Value: String); virtual;
    procedure SetCellEnable(Value: Boolean); virtual;
    procedure SetCellHotKey(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;

    //property ParamList: TStrings read GetParamList;

    //property ParamList: TsmxParams read GetParamList;
  public
    //constructor Create(AOwner: TComponent; ACall: TsmxFuncCallBack;
      //ACfgID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
    procedure Execute(Same: Boolean = False); virtual;
    procedure RefreshParams; virtual;
    function FindParamLocation(AParamLocation: TsmxParamLocation;
      StartPos: Integer = 0): TsmxParam; virtual;

    //property AlgorithmList: TsmxCustomAlgorithmList read GetAlgorithmList; //FAlgorithmList write SetAlgorithmList;
    //property AlgorithmCell: TsmxControlCell read FAlgorithmCell write FAlgorithmCell;
    //property AlgorithmParams[Key: String]: String read GetParam write SetParam;
    //property AlgorithmParams: TsmxParams read GetAlgorithmParams;

    //property AlgorithmParams: TsmxParams read GetAlgorithmParams;
    property Params: TsmxParams read GetParams;
    //property ParamValues[Name: String]: Variant read GetValue; //write SetValue;
    //property ParamCount: Integer read GetCount;
    //property Params[Index: Integer]: TsmxParam read GetParam; default;

    property CellCaption: String read GetCellCaption write SetCellCaption;
    //property CellParams: Variant read GetCellParams;
    property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    property CellHotKey: Integer read GetCellHotKey write SetCellHotKey;
    property CellImageIndex: Integer read GetCellImageIndex;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    //property ResultParams: Variant read FResultParams write FResultParams;
  end;

  { TsmxLibItem }

  {TsmxLibItem = class(TsmxKitItem)
  private
    FLibHandle: THandle;
    FLibName: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property LibHandle: THandle read FLibHandle write FLibHandle;
    property LibName: String read FLibName write FLibName;
  end;}

  { TsmxLibItems }

  {TsmxLibItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxLibItem;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxLibItem;
    function FindByName(ALibName: String): TsmxLibItem;

    property Items[Index: Integer]: TsmxLibItem read GetItem; default; //write SetItem
  end;}

  { TsmxLibManager }

  {EsmxLibManagerError = class(Exception);

  TsmxLibManager = class(TsmxComponent)
  private
    FLibList: TsmxLibItems;
    function GetLibrary(Name: String): THandle;
    procedure FreeLibs;
  protected
    property LibList: TsmxLibItems read FLibList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Libraries[Name: String]: THandle read GetLibrary; default;
  end;}

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

  { TsmxGlobalParam }

  //TsmxGlobalParamType = (gptSystem, gptUser);

  {TsmxGlobalParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;}

  { TsmxGlobalParams }

  {TsmxGlobalParams = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxGlobalParam;
  public
    function Add: TsmxGlobalParam;
    function FindByName(AParamName: String): TsmxGlobalParam;

    property Items[Index: Integer]: TsmxGlobalParam read GetItem; default;
  end;}

  { TsmxGlobalStorage }

  {TsmxGlobalStorage = class(TsmxComponent)
  private
    FParamList: TsmxGlobalParams;
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  protected
    property ParamList: TsmxGlobalParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure InsertParam(AName: String; AValue: Variant);
    //procedure RemoveParam(AName: String);
    //function ExistsParam(AName: String): Boolean;

    property ParamValues[Name: String]: Variant read GetValue write SetValue; default;
  end;}

  { TsmxStateUnit }

  TsmxStateUnits = class;

  TsmxStateUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FIntfID: Integer;
    FUnitEnable: Boolean;
    FUnitVisible: Boolean;
    function GetHKit: TsmxStateUnits;
    function GetItem(Index: Integer): TsmxStateUnit;
    function GetParent: TsmxStateUnit;
    procedure SetUnitEnable(Value: Boolean);
    procedure SetUnitVisible(Value: Boolean);
    procedure ChangeIntfID;
  public
    constructor Create(AHKit: TsmxHKit); override;
    function Add: TsmxStateUnit;
    function FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxStateUnit;

    property CfgID: Integer read FCfgID write FCfgID;
    property HKit: TsmxStateUnits read GetHKit;
    property IntfID: Integer read FIntfID write FIntfID;
    property Items[Index: Integer]: TsmxStateUnit read GetItem; default;
    property Parent: TsmxStateUnit read GetParent;
    property UnitEnable: Boolean read FUnitEnable write SetUnitEnable; //FUnitEnable;
    property UnitVisible: Boolean read FUnitVisible write SetUnitVisible; //FUnitVisible;
  end;

  { TsmxStateUnits }

  TsmxStateUnits = class(TsmxHKit)
  private
    FIntfID: Integer;
    //FIsChangeIntfID: Boolean;
    function GetRoot: TsmxStateUnit;
  public
    property Root: TsmxStateUnit read GetRoot;
    property IntfID: Integer read FIntfID write FIntfID default 0;
    //property IsChangeIntfID: Boolean read FIsChangeIntfID write FIsChangeIntfID default False;
  end;

  { TsmxCellState }

  TsmxCellState = class(TsmxKitItem)
  private
    FID: Integer;
    FStateUnits: TsmxStateUnits;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;

    property ID: Integer read FID write FID;
    property StateUnits: TsmxStateUnits read FStateUnits;
  end;

  { TsmxCellStates }

  TsmxCellStates = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxCellState;
  public
    function Add: TsmxCellState;
    function FindByID(AID: Integer): TsmxCellState;

    property Items[Index: Integer]: TsmxCellState read GetItem {write SetItem}; default;
  end;

  { TsmxIntfItem }

  {TsmxIntfItem = class(TsmxKitItem)
  private
    FID: Integer;
    FCellStates: TsmxCellStates;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;

    property ID: Integer read FID write FID;
    property CellStates: TsmxCellStates read FCellStates;
  end;}

  { TsmxIntfItems }

  {TsmxIntfItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxIntfItem;
  public
    function Add: TsmxIntfItem;
    function FindByID(AID: Integer): TsmxIntfItem;

    property Items[Index: Integer]: TsmxIntfItem read GetItem; default; //write SetItem
  end;}
  
  { TsmxXMLDocItem }

  TsmxXMLDocItem = class(TsmxKitItem)
  private
    FID: Integer;
    FXMLDocIntf: IXMLDocument;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;

    property ID: Integer read FID write FID;
    property XMLDoc: IXMLDocument read FXMLDocIntf write FXMLDocIntf;
  end;

  { TsmxXMLDocItems }

  TsmxXMLDocItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxXMLDocItem;
  public
    function Add: TsmxXMLDocItem;
    function FindByID(AID: Integer): TsmxXMLDocItem;

    property Items[Index: Integer]: TsmxXMLDocItem read GetItem; default;
  end;

  { TsmxStateCfg }

  TsmxStateCfg = class(TsmxBaseCfg)
  private
    FIntfID: Integer;
    FXMLDocList: TsmxXMLDocItems;
    //FCellStatesList: TsmxIntfItems;
    //FDataSetIntf: IsmxDataSet;
    FCellStates: TsmxCellStates;
    //function GetXMLDocList: TsmxXMLDocItems;
    //function GetXMLDocText(ID: Integer): String;
    //procedure SetXMLDocText(ID: Integer; Value: String);
    //function GetCellStatesList: TsmxIntfItems;
    function GetCellStates: TsmxCellStates;
  protected
    procedure LoadCfg; override;
    procedure ReadCfg; override;
    procedure SaveCfg; override;
    procedure WriteCfg; override;
    function GetXMLText: String; override;
    procedure SetXMLText(Value: String); override;
    function GetFullXMLText: String; virtual;

    property XMLDocList: TsmxXMLDocItems read FXMLDocList; //GetXMLDocList;
    property FullXMLText: String read GetFullXMLText;
    //property XMLDocText[ID: Integer]: String read GetXMLDocText write SetXMLDocText;
    //property CellStatesList: TsmxIntfItems read GetCellStatesList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer); override;
    constructor CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer; AIntfID: Integer); virtual;
    destructor Destroy; override;
    procedure Clear; override;

    property CellStates: TsmxCellStates read GetCellStates;
    property IntfID: Integer read FIntfID default 0;
  end;

  TsmxStateCfgClass = class of TsmxStateCfg;

  { TsmxProjectItem }

  TsmxProjectItem = class(TsmxKitItem)
  private
    FProjectName: String;
    FGeneration: TsmxGenerationMode;
    FLibraryName: String;
    FFunctionNameOrProgID: String;
    FWindowsAuthorization: Boolean;
    FDatabaseName: String;
    FDriverName: String;
    FLoginPrompt: Boolean;
    FParams: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property ProjectName: String read FProjectName write FProjectName;
    property Generation: TsmxGenerationMode read FGeneration write FGeneration;
    property LibraryName: String read FLibraryName write FLibraryName;
    property FunctionNameOrProgID: String read FFunctionNameOrProgID write FFunctionNameOrProgID;
    property WindowsAuthorization: Boolean read FWindowsAuthorization write FWindowsAuthorization;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property DriverName: String read FDriverName write FDriverName;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Params: String read FParams write FParams;
  end;

  { TsmxProjectItems }

  TsmxProjectItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxProjectItem;
  public
    function Add: TsmxProjectItem;
    function FindByName(AProjectName: String): TsmxProjectItem;

    property Items[Index: Integer]: TsmxProjectItem read GetItem; default;
  end;

  { TsmxProjectManager }

  TsmxProjectManager = class(TsmxComponent)
  private
    FFileName: String;
    FProjectList: TsmxProjectItems;
    procedure SetFileName(Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadProjects;
    procedure WriteProjects;

    property FileName: String read FFileName write SetFileName;
    property ProjectList: TsmxProjectItems read FProjectList;
  end;
  
implementation

uses
  DB, Variants, XMLDoc, smxFuncs, smxConsts;

{ TsmxBaseCfg }

constructor TsmxBaseCfg.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer);
begin
  inherited Create(AOwner);
  FDatabaseIntf := ADatabase;
  FCfgID := ACfgID;
  //FCall := ACall;
  FTargetRequest := TsmxTargetRequest.Create(Self);
  FTargetRequest.Database := FDatabaseIntf; //Database;
  FXMLDocIntf := NewXMLDocument;
  //Initialize;
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

{function TsmxBaseCfg.GetDatabase: IsmxDatabase;
begin
  if not Assigned(FDatabaseIntf) and Assigned(FCall) then
    FDatabaseIntf := IsmxDatabase(Integer(FCall(1)));
  Result := FDatabaseIntf;
end;}

function TsmxBaseCfg.GetXMLText: String;
begin
  Result := FormatXMLText(FXMLDocIntf.XML.Text);
  //Result := '';
end;

procedure TsmxBaseCfg.Finalize;
begin
  try
    WriteCfg;
    SaveCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgFinalizeError);
  end;
end;

procedure TsmxBaseCfg.Initialize;
begin
  try
    Clear;
    LoadCfg;
    ReadCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;

procedure TsmxBaseCfg.LoadCfg;
begin
  {FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] :=
    FTargetRequest.DoRequest('select ConfBlob from tConfigs where ConfID = :ConfID');
  if FTargetRequest['ConfBlob'] <> '' then
  begin
    FXMLDocIntf.XML.Text := FTargetRequest['ConfBlob'];
    FXMLDocIntf.Active := True;
  end;}
end;

procedure TsmxBaseCfg.ReadCfg;
begin
end;

procedure TsmxBaseCfg.SaveCfg;
begin
  {FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  FTargetRequest.DoExecute('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');}
end;

procedure TsmxBaseCfg.SetXMLText(Value: String);
begin
  try
    FXMLDocIntf.XML.Text := UnFormatXMLText(Value);
    FXMLDocIntf.Active := True;
    Clear;
    ReadCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;

procedure TsmxBaseCfg.WriteCfg;
begin
end;

{ TsmxBaseCell }

constructor TsmxBaseCell.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner);
  FDatabaseIntf := ADatabase;
  FCfgID := ACfgID;
  FID := AID;
  //FCall := ACall;
  //FCfg := CfgClass.Create(Self, FCfgID, FCall);

  //FCfg := IDToCfgClass(FCfgID, FCall).Create(Self, FCfgID, FCall);
  FCfg := NewCfg(Self, FDatabaseIntf, FCfgID);
  FCfg.Initialize;

  //FType := TsmxTypeCfg.Create(Self, FCfgID, FCall);
  //FCfg := FType.CfgClass.Create(Self, FCfgID, FCall);

  FCellList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxBaseCell.Destroy;
begin
  DestroyChilds;
  //ClearParent;
  FCellList.Free;

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

function TsmxBaseCell.FindCellByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;

  function Find(ACell: TsmxBaseCell): TsmxBaseCell;
  var i: Integer;
  begin
    Result := nil;
    if ACell.CfgID = ACfgID then
      Result := ACell else
    if AmongAll then
      for i := 0 to ACell.CellCount - 1 do
      begin
        Result := Find(ACell.Cells[i]);
        if Assigned(Result) then
          Break;
      end;
  end;

var i: Integer;
begin
  Result := nil;
  for i := 0 to CellCount - 1 do
  begin
    Result := Find(Cells[i]);
    if Assigned(Result) then
      Break;
  end;
end;

{function TsmxBaseCell.GetCfgClass: TsmxBaseCfgClass;
begin
  if not(Assigned(FCfgClass)) then
    FCfgClass := IDToCfgClass(FCfgID, FCall);
  Result := FCfgClass;
end;}

function TsmxBaseCell.GetCellCount: Integer;
begin
  Result := FCellList.Count;
end;

function TsmxBaseCell.GetCell(Index: Integer): TsmxBaseCell;
begin
  Result := TsmxBaseCell(FCellList[Index]);
end;

{function TsmxBaseCell.GetDatabase: IsmxDatabase;
begin
  if not Assigned(FDatabaseIntf) and Assigned(FCall) then
    FDatabaseIntf := IsmxDatabase(Integer(FCall(1)));
  Result := FDatabaseIntf;
end;}

{function TsmxBaseCell.GetImageList: TImageList;
begin
  if not Assigned(FImageList) and Assigned(FCall) then
    FImageList := TImageList(Integer(FCall(5)));
  Result := FImageList;
end;}

{function TsmxBaseCell.GetCompID: String;
begin
  Result := IntToStr(FCfgID);
end;}

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

procedure TsmxBaseCell.Initialize;
begin
end;

procedure TsmxBaseCell.InstallParent;
begin
end;

{procedure TsmxBaseCell.RemoveChild(AChild: TsmxBaseCell);
begin
  FChildList.Remove(AChild);
end;}

procedure TsmxBaseCell.SetParentCell(AParent: TsmxBaseCell);
begin
  if Assigned(FParentCell) then
    FParentCell.FCellList.Remove(Self);
  FParentCell := AParent;
  if Assigned(AParent) then
    AParent.FCellList.Add(Self);
end;

procedure TsmxBaseCell.UnInitialize;
begin
end;

procedure TsmxBaseCell.UnInstallParent;
begin
end;

{ TsmxCellCfg }

{constructor TsmxCellCfg.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADatabase, ACfgID, AID);
  //FDatabaseIntf := ADB;
  //FCfgID := ACfgID;
  //FCall := ACall;
  //FTargetRequest := TsmxTargetRequest.Create(Self);
  //FTargetRequest.Database := FDatabaseIntf; //Database;
  FXMLDocIntf := NewXMLDocument;
  //Initialize;
end;}

{destructor TsmxCellCfg.Destroy;
begin
  //FTargetRequest.Free;
  //FDatabaseIntf := nil;
  FXMLDocIntf := nil;
  inherited Destroy;
end;}

{procedure TsmxCellCfg.Clear;
begin
end;}

{function TsmxCellCfg.GetCfgName: String;
begin
  if FCfgName = '' then
  begin
    FTargetRequest['ConfID'] := IntToStr(FCfgID);
    FCfgName := FTargetRequest.DoRequest('select ConfName from tConfigs where ConfID = :ConfID');
  end;
  Result := FCfgName;
end;}

{function TsmxCellCfg.GetDatabase: IsmxDatabase;
begin
  if not Assigned(FDatabaseIntf) and Assigned(FCall) then
    FDatabaseIntf := IsmxDatabase(Integer(FCall(1)));
  Result := FDatabaseIntf;
end;}

{function TsmxCellCfg.GetXMLText: String;
begin
  Result := FormatXMLText(FXMLDocIntf.XML.Text);
end;}

{procedure TsmxCellCfg.Finalize;
begin
  try
    WriteCfg;
    SaveCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgFinalizeError);
  end;
end;}

{procedure TsmxCellCfg.Initialize;
begin
  try
    Clear;
    LoadCfg;
    ReadCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;}

procedure TsmxCellCfg.LoadCfg;
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

{procedure TsmxCellCfg.ReadCfg;
begin
end;}

procedure TsmxCellCfg.SaveCfg;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  FTargetRequest.DoExecute('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');
end;

{procedure TsmxCellCfg.SetXMLText(Value: String);
begin
  try
    FXMLDocIntf.XML.Text := UnFormatXMLText(Value);
    FXMLDocIntf.Active := True;
    Clear;
    ReadCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;}

{procedure TsmxCellCfg.WriteCfg;
begin
end;]

{ TsmxTypeCfg }

procedure TsmxTypeCfg.Clear;
begin
  FCellClassName := '';
  FCellClass := nil;
  FCfgClassName := '';
  FCfgClass := nil;
end;

procedure TsmxTypeCfg.LoadCfg;
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

procedure TsmxTypeCfg.ReadCfg;
var r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('cell');
  if Assigned(n) then
  begin
    CfgClassName := n.Attributes['cfgclassname'];
    CellClassName := n.Attributes['cellclassname'];
  end;
end;

procedure TsmxTypeCfg.SaveCfg;
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
    if FCfgClassName = '' then
      FCfgClass := nil else
      FCfgClass := TsmxBaseCfgClass(FindClass(FCfgClassName));
  end;
end;

procedure TsmxTypeCfg.SetCellClassName(Value: String);
begin
  if FCellClassName <> Value then
  begin
    FCellClassName := Value;
    if FCellClassName = '' then
      FCellClass := nil else
      FCellClass := TsmxBaseCellClass(FindClass(FCellClassName));
  end;
end;

procedure TsmxTypeCfg.WriteCfg;
var r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := FXMLDocIntf.AddChild('root');

  n := r.AddChild('cell');
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
  if not Assigned(FParamList) then
    FParamList := TStringList.Create;
  Result := FParamList;
end;

function TsmxTargetRequest.GetValue(Key: String): String;
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
    PrepRequest(FRequestIntf, True, pmExecute, DSFrom);
  end;
end;

function TsmxTargetRequest.DoRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Res: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
begin
  with ForRequest(SQLText, RequestType, True, pmOpen, DSFrom) do
  try
    if Res = '' then
      Res := Fields[0].FieldName;
    Result := FieldByName(Res).Value;
    //if VarIsNull(Result) then
      //Result := '';
  finally
    Close;
  end;
end;

function TsmxTargetRequest.ForRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Get: Boolean = False; Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
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
  PrepRequest(FRequestIntf, Get, Perform, DSFrom);
  Result := FRequestIntf;
end;

function TsmxTargetRequest.NewRequest(SQLText: String = '';
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  Result := nil;
  if not Assigned(Database) then
    //raise EsmxTargetRequestError.CreateRes(@STargetRequestDatabaseError);
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  Result := Database.NewDataSet(RequestType);
  Result.Database := Database;
  Result.SQL.Text := SQLText;
  PrepRequest(Result, False, pmOpen, DSFrom);
end;

function TsmxTargetRequest.PrepRequest(const ARequest: IsmxDataSet; Get: Boolean = True;
  Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): Boolean;
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
          if ParamValues[Params[i].ParamName] = '' then
            Params[i].Value := Null else
            Params[i].Value := ParamValues[Params[i].ParamName];
        end;
      end;
    if Get then
    begin
      case Perform of
        pmOpen: Open;
        pmExecute: Execute;
      end;
      for i := 0 to ParamCount - 1 do
        if Params[i].ParamType in [ptOutput, ptInputOutput] then
        begin
          if VarIsNull(Params[i].Value) then
            ParamValues[Params[i].ParamName] := '' else
            ParamValues[Params[i].ParamName] := Params[i].Value;
        end else
        if Params[i].ParamType in [ptResult] then
        begin
          ParamValues[Params[i].ParamName] := Params[i].Value;
          v := Params[i].Value;
        end;

      case Perform of
        pmOpen: Result := RecordCount > 0;
        pmExecute: Result := v = 0;
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

procedure TsmxTargetRequest.SetValue(Key: String; const Value: String);
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

function TsmxKitItem.GetIndex: Integer;
begin
  if Assigned(FKit) then
    Result := FKit.FList.IndexOf(Self) else
    Result := -1;
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

{function TsmxKit.IndexOf(AItem: TsmxKitItem): Integer;
begin
  Result := FList.IndexOf(AItem);
end;}

{function TsmxKit.Insert(Index: Integer): TsmxKitItem;
begin
  FList.Insert(Index, nil);
  Result := FCellClass.Create(Self);
  FList[Index] := Result;
end;}

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
  //FCellClass := FHKit.FCellClass;
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
  Result := FHKit.FCellClass.Create(FHKit); //FCellClass.Create(FHKit); //(Self, FCellClass);
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

function TsmxHKitItem.GetIndex: Integer;
begin
  if Assigned(FParent) then
    Result := FParent.FList.IndexOf(Self) else
    Result := -1;
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

{ TsmxCallBackParam }

{constructor TsmxCallBackParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamIndex := -1;
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

function TsmxCallBackParams.GetItem(Index: Integer): TsmxCallBackParam;
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
end;

function TsmxCallBack.GetValue(Index: Integer): Variant;
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Unassigned;
end;

procedure TsmxCallBack.SetValue(Index: Integer; Value: Variant);
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

{constructor TsmxFormItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FForm := nil;
  FFormHandle := 0;
  FFormCfgID := 0;
  FFormID := 0;
  //FFormComboID := '';
end;}

{ TsmxFormItems }

{function TsmxFormItems.Add: TsmxFormItem;
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

function TsmxFormItems.FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i].FormCfgID = ACfgID) and (Items[i].FormID = AID) then
    begin
      Result := Items[i];
      Break;
    end;
end;}

{function TsmxFormItems.FindByComboID(AComboID: String): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].FormComboID, AComboID) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;}

{function TsmxFormItems.GetItem(Index: Integer): TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Items[Index]);
end;}

{procedure TsmxFormItems.SetItem(Index: Integer; Value: TsmxFormItem);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxFormManager }

{constructor TsmxFormManager.Create(AOwner: TComponent);
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
end;

function TsmxFormManager.FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
var f: TsmxFormItem; 
begin
  Result := nil;
  f := FormList.FindByComboID(ACfgID, AID);
  if Assigned(f) then
    Result := f.Form;
end;}

{function TsmxFormManager.FindByComboID(AComboID: String): TsmxBaseCell;
var f: TsmxFormItem;
begin
  Result := nil;
  f := FormList.FindByComboID(AComboID);
  if Assigned(f) then
    Result := f.Form;
end;}

{function TsmxFormManager.GetFormCount: Integer;
begin
  Result := FFormList.Count;
end;

function TsmxFormManager.GetForm(Index: Integer): TsmxBaseCell;
begin
  Result := FFormList[Index].Form;
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

{procedure TsmxFormManager.InsertForm(AForm: TsmxBaseCell);
var f: TsmxFormItem; c: TObject; //i: Integer;
begin
  f := FormList.FindByForm(AForm);
  if not Assigned(f) then
    with FormList.Add do
    begin
      Form := AForm;
      c := AForm.GetInternalObject;
      if c is TWinControl then
        FormHandle := TWinControl(c).Handle;
      FormCfgID := AForm.CfgID;
      FormID := AForm.ID;
      //FormComboID := IntToStr(AForm.CfgID) + '.' + IntToStr(AForm.ID);
    end;
end;

procedure TsmxFormManager.RemoveForm(AForm: TsmxBaseCell);
var f: TsmxFormItem;
begin
  //FFormList.Remove(AForm);
  f := FormList.FindByForm(AForm);
  if Assigned(f) then
    FormList.Remove(f);
end;}

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
    FFormManager := TsmxFormManager(Integer(Call(4)));
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

{ TsmxAlgorithmCfg }

{procedure TsmxAlgorithmCfg.Clear;
begin
  FAlgCaption := ''; //CfgName;
  FAlgHotKey := 0;
  FAlgImageIndex := -1;
  FAlgLibrary := '';
  FAlgProcedure := '';
  AlgParams.Clear;
end;

function TsmxAlgorithmCfg.GetAlgParams: TsmxLocationParams;
begin
  if not Assigned(FAlgParams) then
    FAlgParams := TsmxLocationParams.Create(TsmxLocationParam);
  Result := FAlgParams;
end;

procedure TsmxAlgorithmCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('algorithm');
  if Assigned(n) then
  begin
    AlgLibrary := n.Attributes['library'];
    AlgProcedure := n.Attributes['procedure'];
    AlgCaption := n.Attributes['caption'];
    AlgHotKey := n.Attributes['hotkey'];
    AlgImageIndex := n.Attributes['imageindex'];
  end;

  n := r.ChildNodes.FindNode('params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'param' then
        with AlgParams.Add do
        begin
          ParamName := n.ChildNodes[i].Attributes['name'];
          ParamDefValue := VarStringToVar(n.ChildNodes[i].Attributes['defvalue']);
          ParamLocation := n.ChildNodes[i].Attributes['location'];
        end;
  end;
end;

procedure TsmxAlgorithmCfg.WriteCfg;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('algorithm');
  n.Attributes['library'] := AlgLibrary;
  n.Attributes['procedure'] := AlgProcedure;
  n.Attributes['caption'] := AlgCaption;
  n.Attributes['hotkey'] := AlgHotKey;
  n.Attributes['imageindex'] := AlgImageIndex;

  n := r.AddChild('params');
  for i := 0 to AlgParams.Count - 1 do
    with n.AddChild('param') do
    begin
      Attributes['name'] := AlgParams[i].ParamName;
      Attributes['defvalue'] := AlgParams[i].ParamDefValue;
      Attributes['location'] := AlgParams[i].ParamLocation;
    end;
end;}

{ TsmxParam }

constructor TsmxParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamName := '';
  FParamValue := Null; //Unassigned;
end;

{ TsmxParams }

function TsmxParams.Add: TsmxParam;
begin
  Result := TsmxParam(inherited Add);
end;

function TsmxParams.FindByName(AParamName: String): TsmxParam;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxParams.GetItem(Index: Integer): TsmxParam;
begin
  Result := TsmxParam(inherited Items[Index]);
end;

{function TsmxParams.ParamByName(AParamName: String): TsmxParam;
begin
  Result := FindByName(AParamName);
  if not Assigned(Result) then
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
end;}

{procedure TsmxParams.SetItem(Index: Integer; Value: TsmxParam);
begin
  inherited Items[Index] := Value;
end;}

function TsmxParams.GetValue(Name: String): Variant;
var p: TsmxParam;
begin
  p := FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
    //Result := Null; //Unassigned;
end;

procedure TsmxParams.SetValue(Name: String; Value: Variant);
var p: TsmxParam;
begin
  p := FindByName(Name);
  if Assigned(p) then
    p.ParamValue := Value else
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
  //else
    //with Add do
    //begin
      //ParamName := Name;
      //ParamValue := Value;
    //end;
end;

{ TsmxAlgorithm }

{constructor TsmxAlgorithm.Create(AOwner: TComponent; ACall: TsmxFuncCallBack;
  ACfgID: Integer; AID: Integer = 0);
begin
  FParamList := TsmxParams.Create(TsmxParam);
end;}

destructor TsmxAlgorithm.Destroy;
begin
  //if Assigned(FParamList) then
    //FParamList.Free;
  //if Assigned(FAlgorithmParams) then
    //FAlgorithmParams.Free;
  if Assigned(FParams) then
    FParams.Free;
  inherited Destroy;
end;

procedure TsmxAlgorithm.Execute(Same: Boolean = False);
begin
end;

function TsmxAlgorithm.FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam;
begin
  Result := nil;
end;

{function TsmxAlgorithm.GetAlgorithmList: TsmxCustomAlgorithmList;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomAlgorithmList then
      Result := TsmxCustomAlgorithmList(ParentCell);
end;}

{function TsmxAlgorithm.GetAlgorithmParams: TsmxParams;
begin
  if not Assigned(FAlgorithmParams) then
    FAlgorithmParams := TsmxParams.Create(TsmxParam);
  Result := FAlgorithmParams;
end;}

function TsmxAlgorithm.GetParams: TsmxParams;
begin
  if not Assigned(FParams) then
    FParams := TsmxParams.Create(TsmxParam);
  Result := FParams;
end;

{function TsmxAlgorithm.GetParamList: TsmxParams;
begin
  if not Assigned(FParamList) then
    FParamList := TsmxParams.Create(TsmxParam);
  Result := FParamList;
end;}

{function TsmxAlgorithm.GetCount: Integer;
begin
  Result := ParamList.Count;
end;}

{function TsmxAlgorithm.GetParam(Index: Integer): TsmxParam;
begin
  Result := ParamList[Index];
end;}

{function TsmxAlgorithm.GetValue(Name: String): Variant;
var p: TsmxParam;
begin
  p := ParamList.FindByName(Name);
  //p := AlgorithmParams.FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
end;}

{procedure TsmxAlgorithm.SetValue(Name: String; Value: Variant);
var p: TsmxParam;
begin
  p := ParamList.FindByName(Name);
  //p := AlgorithmParams.FindByName(Name);
  if Assigned(p) then
    p.ParamValue := Value else
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
end;}

{function TsmxAlgorithm.GetParamList: TStrings;
begin
  if not Assigned(FParamList) then
    FParamList := TStringList.Create;
  Result := FParamList;
end;

function TsmxAlgorithm.GetParam(Key: String): String;
begin
  Result := ParamList.Values[AnsiUpperCase(Key)];
end;

procedure TsmxAlgorithm.SetParam(Key: String; Value: String);
begin
  ParamList.Values[AnsiUpperCase(Key)] := Value;
end;}

function TsmxAlgorithm.GetCellCaption: String;
begin
  Result := '';
end;

{function TsmxAlgorithm.GetCellParams: Variant;
begin
  Result := Unassigned;
end;}

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

procedure TsmxAlgorithm.RefreshParams;
begin
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

{constructor TsmxLibItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FLibHandle := 0;
  FLibName := '';
end;}

{ TsmxLibItems }

{function TsmxLibItems.Add: TsmxLibItem;
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
end;}

{procedure TsmxLibItems.SetItem(Index: Integer; Value: TsmxLibItem);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxLibManager }

{constructor TsmxLibManager.Create(AOwner: TComponent);
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
  if Assigned(l) then
  begin
    Result := l.LibHandle;
  end else
  begin
    try
      Result := LoadLibrary(PChar(Name));
    except
      raise EsmxLibManagerError.CreateRes(@SLibManagerLoadError);
    end;
    with FLibList.Add do
    begin
      LibHandle := Result;
      LibName := Name;
    end;
  end;
end;}

{ TsmxToolBar }

{constructor TsmxToolBar.Create(AOwner: TComponent);
begin
  FToolBar := ToolBar.Create(Self);
end;

destructor TsmxToolBar.Destroy;
begin
  FToolBar.Free;
end;}

{ TsmxGlobalParam }

{constructor TsmxGlobalParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamName := '';
  FParamValue := Null;
end;}

{ TsmxGlobalParams }

{function TsmxGlobalParams.Add: TsmxGlobalParam;
begin
  Result := TsmxGlobalParam(inherited Add);
end;

function TsmxGlobalParams.FindByName(AParamName: String): TsmxGlobalParam;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxGlobalParams.GetItem(Index: Integer): TsmxGlobalParam;
begin
  Result := TsmxGlobalParam(inherited Items[Index]);
end;}

{ TsmxGlobalStorage }

{constructor TsmxGlobalStorage.Create(AOwner: TComponent);
begin
  FParamList := TsmxGlobalParams.Create(TsmxGlobalParam);
end;

destructor TsmxGlobalStorage.Destroy;
begin
  FParamList.Free;
end;}

{procedure TsmxGlobalStorage.InsertParam(AName: String; AValue: Variant);
var p: TsmxParam;
begin
  p := FParamList.FindByName(AName);
  if Assigned(p) then
    raise EsmxCellError.CreateRes(@SCellParamNotFound)
  else
    with FParamList.Add do
    begin
      ParamName := AName;
      ParamValue := AValue;
    end;
end;}

{function TsmxGlobalStorage.GetValue(Name: String): Variant;
var p: TsmxGlobalParam;
begin
  p := FParamList.FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Null;
end;

procedure TsmxGlobalStorage.SetValue(Name: String; Value: Variant);
var p: TsmxGlobalParam;
begin
  p := FParamList.FindByName(Name);
  if Assigned(p) then
    p.ParamValue := Value
  else
    with FParamList.Add do
    begin
      ParamName := Name;
      ParamValue := Value;
    end;
end;}

{ TsmxStateUnit }

constructor TsmxStateUnit.Create(AHKit: TsmxHKit);
begin
  inherited Create(AHKit);
  FCfgID := 0;
  FIntfID := 0;
  FUnitEnable := False;
  FUnitVisible := False;
end;

function TsmxStateUnit.Add: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Add);
end;

procedure TsmxStateUnit.ChangeIntfID;
var u: TsmxStateUnit; ID: Integer;
begin
  ID := HKit.IntfID;
  if FIntfID <> ID then
  begin
    u := Self;
    while u.HasParent do
    begin
      u.IntfID := ID;
      u := u.Parent;
    end;
  end;
end;

function TsmxStateUnit.FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxStateUnit;

  function Find(AUnit: TsmxStateUnit): TsmxStateUnit;
  var i: Integer;
  begin
    Result := nil;
    if AUnit.CfgID = ACfgID then
      Result := AUnit else
    if AmongAll then
      for i := 0 to AUnit.Count - 1 do
      begin
        Result := Find(AUnit.Items[i]);
        if Assigned(Result) then
          Break;
      end;
  end;

var i: Integer;  
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Find(Items[i]);
    if Assigned(Result) then
      Break;
  end;
end;

function TsmxStateUnit.GetHKit: TsmxStateUnits;
begin
  Result := TsmxStateUnits(inherited HKit);
end;

function TsmxStateUnit.GetItem(Index: Integer): TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Items[Index]);
end;

function TsmxStateUnit.GetParent: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Parent);
end;

procedure TsmxStateUnit.SetUnitEnable(Value: Boolean);
begin
  FUnitEnable := Value;
  ChangeIntfID;
end;

procedure TsmxStateUnit.SetUnitVisible(Value: Boolean);
begin
  FUnitVisible := Value;
  ChangeIntfID;
end;

{ TsmxStateUnits }

function TsmxStateUnits.GetRoot: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Root);
end;

{ TsmxCellState }

constructor TsmxCellState.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FID := 0;
  FStateUnits := TsmxStateUnits.Create(TsmxStateUnit);
end;

destructor TsmxCellState.Destroy;
begin
  FStateUnits.Free;
  inherited Destroy;
end;

{ TsmxCellStates }

function TsmxCellStates.Add: TsmxCellState;
begin
  Result := TsmxCellState(inherited Add);
end;

function TsmxCellStates.FindByID(AID: Integer): TsmxCellState;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID = AID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxCellStates.GetItem(Index: Integer): TsmxCellState;
begin
  Result := TsmxCellState(inherited Items[Index]);
end;

{ TsmxIntfItem }

{constructor TsmxIntfItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FID := 0;
  FCellStates := TsmxCellStates.Create(TsmxCellState);
end;

destructor TsmxIntfItem.Destroy;
begin
  FCellStates.Free;
  inherited Destroy;
end;}

{ TsmxIntfItems }

{function TsmxIntfItems.Add: TsmxIntfItem;
begin
  Result := TsmxIntfItem(inherited Add);
end;

function TsmxIntfItems.FindByID(AID: Integer): TsmxIntfItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID = AID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxIntfItems.GetItem(Index: Integer): TsmxIntfItem;
begin
  Result := TsmxIntfItem(inherited Items[Index]);
end;}

{ TsmxXMLDocItem }

constructor TsmxXMLDocItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FID := 0;
  FXMLDocIntf := nil; //NewXMLDocument; 
end;

destructor TsmxXMLDocItem.Destroy;
begin
  FXMLDocIntf := nil;
  inherited Destroy;
end;

{ TsmxXMLDocItems }

function TsmxXMLDocItems.Add: TsmxXMLDocItem;
begin
  Result := TsmxXMLDocItem(inherited Add);
end;

function TsmxXMLDocItems.FindByID(AID: Integer): TsmxXMLDocItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID = AID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxXMLDocItems.GetItem(Index: Integer): TsmxXMLDocItem;
begin
  Result := TsmxXMLDocItem(inherited Items[Index]);
end;

{ TsmxStateCfg }

constructor TsmxStateCfg.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FXMLDocList := TsmxXMLDocItems.Create(TsmxXMLDocItem);
end;

constructor TsmxStateCfg.CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AIntfID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FIntfID := AIntfID;
  FXMLDocList := TsmxXMLDocItems.Create(TsmxXMLDocItem);
end;

destructor TsmxStateCfg.Destroy;
begin
  //if Assigned(FXMLDocList) then
    FXMLDocList.Free;
  if Assigned(FCellStates) then
    FCellStates.Free;
  //FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxStateCfg.Clear;
begin
  CellStates.Clear;
end;

{function TsmxStateCfg.GetXMLDocList: TsmxXMLDocItems;
begin
  if not Assigned(FXMLDocList) then
    FXMLDocList := TsmxXMLDocItems.Create(TsmxXMLDocItem);
  Result := FXMLDocList;
end;}

{function TsmxStateCfg.GetCellStatesList: TsmxIntfItems;
begin
  if not Assigned(FCellStatesList) then
    FCellStatesList := TsmxIntfItems.Create(TsmxIntfItem);
  Result := FCellStatesList;
end;}

function TsmxStateCfg.GetCellStates: TsmxCellStates;
begin
  if not Assigned(FCellStates) then
    FCellStates := TsmxCellStates.Create(TsmxCellState);
  Result := FCellStates;
end;

function TsmxStateCfg.GetXMLText: String;
var XMLDocItem: TsmxXMLDocItem;
begin
  XMLDocItem := FXMLDocList.FindByID(IntfID);
  if Assigned(XMLDocItem) then
    Result := FormatXMLText(XMLDocItem.XMLDoc.XML.Text) else
    Result := '';
end;

procedure TsmxStateCfg.SetXMLText(Value: String);
var XMLDocItem: TsmxXMLDocItem;
begin
  XMLDocItem := FXMLDocList.FindByID(IntfID);
  if not Assigned(XMLDocItem) then
  begin
    XMLDocItem := FXMLDocList.Add;
    XMLDocItem.ID := IntfID;
  end;
  try
    XMLDocItem.XMLDoc.XML.Text := UnFormatXMLText(Value);
    XMLDocItem.XMLDoc.Active := True;
    Clear;
    ReadCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;

function TsmxStateCfg.GetFullXMLText: String;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('cell');
    with n do
    begin
      Attributes['id'] := AUnit.CfgID;
      Attributes['enable'] := AUnit.UnitEnable;
      Attributes['visible'] := AUnit.UnitVisible;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;

var r, n, n2, n3: IXMLNode; i, j: Integer;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := FXMLDocIntf.AddChild('root');

  n := r.AddChild('states');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('state');
    n2.Attributes['id'] := CellStates[i].ID;
    n3 := n2.AddChild('cells');
    for j := 0 to CellStates[i].StateUnits.Root.Count - 1 do
      AddNodes(n3, CellStates[i].StateUnits.Root[j]);
  end;
  Result := FormatXMLText(FXMLDocIntf.XML.Text);
end;

{function TsmxStateCfg.GetXMLDocText(ID: Integer): String;
var d: TsmxXMLDocItem;
begin
  d := XMLDocList.FindByID(ID);
  if Assigned(d) then
    Result := String(UTF8Decode(FormatXMLData(WideString(d.XMLDoc.XML.Text)))) else
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
end;}

{procedure TsmxStateCfg.SetXMLDocText(ID: Integer; Value: String);
var d: TsmxXMLDocItem; i: Integer; sl: TStrings;
begin
  d := XMLDocList.FindByID(ID);
  if Assigned(d) then
  begin
    try
      sl := TStringList.Create;
      try
        sl.Text := Value;
        for i := 0 to sl.Count - 1 do
          sl[i] := Trim(sl[i]);
        d.XMLDoc.XML.Text :=
          String(UTF8Encode(WideString(AnsiReplaceStr(sl.Text, sLineBreak, ''))));
        d.XMLDoc.Active := True;
        Clear;
        ReadCfg;
      finally
        sl.Free;
      end;
    except
      raise EsmxCfgError.CreateRes(@SCfgInitializeError);
    end;
  end else
  begin
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;}

procedure TsmxStateCfg.LoadCfg;
var IntfID: Integer; XMLText: String; XMLDocIntf: IXMLDocument;
begin
  {FTargetRequest['IntfID'] := 0;
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['IntfConfBlob'] :=
    FTargetRequest.DoRequest('select IntfConfBlob from tIntfsConfs ' +
      'where IntfID = :IntfID and ConfID = :ConfID');
  if FTargetRequest['IntfConfBlob'] <> '' then
  begin
    FXMLDocIntf.XML.Text := FTargetRequest['IntfConfBlob'];
    FXMLDocIntf.Active := True;
  end;}

  FXMLDocList.Clear;
  FTargetRequest['IntfID'] := IntToStr(FIntfID);
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  with FTargetRequest.ForRequest('select ic.IntfID, ic.IntfConfBlob ' +
    'from f_ParentInterfaces(:IntfID) pis ' +
    'join tIntfsConfs ic on ic.IntfID = pis.ID and ic.ConfID = :ConfID ' +
    'order by pis.HLevel desc', dstQuery, True) do
  begin
    First;
    while not Eof do
    begin
      IntfID := FieldByName('IntfID').Value;
      XMLText := VarToStr(FieldByName('IntfConfBlob').Value);
      //if XMLText <> '' then
      //begin
        XMLDocIntf := NewXMLDocument;
        if XMLText <> '' then
        begin
          XMLDocIntf.XML.Text := XMLText;
          XMLDocIntf.Active := True;
        end;
        with FXMLDocList.Add do
        begin
          ID := IntfID;
          XMLDoc := XMLDocIntf;
          //if XMLText <> '' then
            //XMLDoc.XML.Text := XMLText;
          //XMLDoc.Active := True;
        end;
      //end;
      Next;
    end;
  end;

  {XMLDocItem := XMLDocList.FindByID(0); //ID
  if Assigned(XMLDocItem) then
  begin
    FXMLDocIntf.XML.Text := XMLDocItem.XMLDoc.XML.Text;
    FXMLDocIntf.Active := True;
  end;}
end;

procedure TsmxStateCfg.ReadCfg;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxStateUnit; AIntfID: Integer);
  var i: Integer; u: TsmxStateUnit;
  begin
    //u := AUnit.Add;
    u := AUnit.FindByCfgID(ANode.Attributes['id']);
    if not Assigned(u) then
      u := AUnit.Add;
    with u do
    begin
      FIntfID := AIntfID; //XMLDocList[k].ID; //HKit.IntfID;
      FCfgID := ANode.Attributes['id'];
      FUnitEnable := ANode.Attributes['enable'];
      FUnitVisible := ANode.Attributes['visible'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'cell' then
        AddUnits(ANode.ChildNodes[i], u, AIntfID);
  end;

var r, n, n2: IXMLNode; i, j, k: Integer; s: TsmxCellState;
begin
  for k := 0 to FXMLDocList.Count - 1 do
  begin
    r := FXMLDocList[k].XMLDoc.ChildNodes.FindNode('root');
    if Assigned(r) then
    begin
      n := r.ChildNodes.FindNode('states');
      if Assigned(n) and (n.ChildNodes.Count > 0) then
      begin
        for i := 0 to n.ChildNodes.Count - 1 do
          if n.ChildNodes[i].NodeName = 'state' then
          begin
            s := CellStates.FindByID(n.ChildNodes[i].Attributes['id']);
            if not Assigned(s) then
            begin
              s := CellStates.Add;
              s.StateUnits.IntfID := FIntfID;
            end;
            //with CellStates.Add do
            with s do
            begin
              //StateUnits.IntfID := 0; //ID
              //StateUnits.IsChangeIntfID := False;
              ID := n.ChildNodes[i].Attributes['id'];
              n2 := n.ChildNodes[i].ChildNodes.FindNode('cells');
              if Assigned(n2) and (n2.ChildNodes.Count > 0) then
                for j := 0 to n2.ChildNodes.Count - 1 do
                  if n2.ChildNodes[j].NodeName = 'cell' then
                    AddUnits(n2.ChildNodes[j], StateUnits.Root, FXMLDocList[k].ID);
              //StateUnits.IsChangeIntfID := True;
            end;
          end;
      end;
    end;
  end;

  {r := FXMLDocIntf.ChildNodes.FindNode('root');
  if Assigned(r) then
  begin
    n := r.ChildNodes.FindNode('states');
    if Assigned(n) and (n.ChildNodes.Count > 0) then
    begin
      for i := 0 to n.ChildNodes.Count - 1 do
        if n.ChildNodes[i].NodeName = 'state' then
          with CellStates.Add do
          begin
            StateUnits.IntfID := XMLDocList[k].ID;
            StateUnits.IsChangeIntfID := False;
            ID := n.ChildNodes[i].Attributes['id'];
            n2 := n.ChildNodes[i].ChildNodes.FindNode('cells');
            if Assigned(n2) and (n2.ChildNodes.Count > 0) then
              for j := 0 to n2.ChildNodes.Count - 1 do
                if n2.ChildNodes[j].NodeName = 'cell' then
                  AddUnits(n2.ChildNodes[j], StateUnits.Root);
            StateUnits.IsChangeIntfID := True;
          end;
    end;
  end;}
end;

procedure TsmxStateCfg.SaveCfg;
var XMLDocItem: TsmxXMLDocItem; 
begin
  XMLDocItem := FXMLDocList.FindByID(FIntfID);
  if Assigned(XMLDocItem) then
  begin
    FTargetRequest['IntfID'] := IntToStr(FIntfID);
    FTargetRequest['ConfID'] := IntToStr(FCfgID);
    FTargetRequest['IntfConfBlob'] := XMLDocItem.XMLDoc.XML.Text; //FXMLDocIntf.XML.Text;
    if VarIsNull(FTargetRequest.DoRequest('select IntfConfID from tIntfsConfs ' +
        'where IntfID = :IntfID and ConfID = :ConfID')) then
      FTargetRequest.DoExecute('insert into tIntfsConfs (IntfID, ConfID, IntfConfBlob) ' +
        'values (:IntfID, :ConfID, :IntfConfBlob)') else
      FTargetRequest.DoExecute('update tIntfsConfs set IntfConfBlob = :IntfConfBlob ' +
        'where IntfID = :IntfID and ConfID = :ConfID');
  end;
  //FTargetRequest.DoExecute('insert into tIntfsConfs (IntfID, ConfID, IntfConfBlob) ' +
    //'values (:IntfID, :ConfID, :IntfConfBlob)') else
  //FTargetRequest.DoExecute('update tIntfsConfs ic set ic.IntfConfBlob = :IntfConfBlob ' +
    //'where ic.IntfConfID = :IntfConfID');
end;

procedure TsmxStateCfg.WriteCfg;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; n: IXMLNode;
  begin
    if AUnit.IntfID = FIntfID then
    begin
      n := ANode.AddChild('cell');
      with n do
      begin
        Attributes['id'] := AUnit.CfgID;
        Attributes['enable'] := AUnit.UnitEnable;
        Attributes['visible'] := AUnit.UnitVisible;
      end;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;

var r, n, n2, n3: IXMLNode; i, j: Integer; XMLDocItem: TsmxXMLDocItem;
begin
  {XMLDocList.Clear;
  for k := 0 to CellStatesList.Count - 1 do
  begin
    XMLDocIntf := NewXMLDocument;
    r := XMLDocIntf.AddChild('root');

    n := r.AddChild('states');
    for i := 0 to CellStatesList[k].CellStates.Count - 1 do
    begin
      n2 := n.AddChild('state');
      n2.Attributes['id'] := CellStatesList[k].CellStates[i].ID;
      n3 := n2.AddChild('cells');
      for j := 0 to CellStatesList[k].CellStates[i].StateUnits.Root.Count - 1 do
        AddNodes(n3, CellStatesList[k].CellStates[i].StateUnits.Root[j]);
    end;

    with XMLDocList.Add do
    begin
      ID := CellStatesList[k].ID;
      XMLDoc := XMLDocIntf;
    end;
  end;}

  XMLDocItem := FXMLDocList.FindByID(FIntfID);
  if not Assigned(XMLDocItem) then
  begin
    XMLDocItem := FXMLDocList.Add;
    XMLDocItem.ID := FIntfID;
  end;

  //r := FXMLDocIntf.ChildNodes.FindNode('root');
  r := XMLDocItem.XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    //r := FXMLDocIntf.AddChild('root');
    r := XMLDocItem.XMLDoc.AddChild('root');

  n := r.AddChild('states');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('state');
    n2.Attributes['id'] := CellStates[i].ID;
    n3 := n2.AddChild('cells');
    for j := 0 to CellStates[i].StateUnits.Root.Count - 1 do
      AddNodes(n3, CellStates[i].StateUnits.Root[j]);
  end;
end;

{ TsmxProjectItem }

constructor TsmxProjectItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FProjectName := '';
  FGeneration := gmFunction;
  FLibraryName := '';
  FFunctionNameOrProgID := '';
  FWindowsAuthorization := False;
  FDatabaseName := '';
  FDriverName := '';
  FLoginPrompt := False;
  FParams := '';
end;

{ TsmxProjectItems }

function TsmxProjectItems.Add: TsmxProjectItem;
begin
  Result := TsmxProjectItem(inherited Add);
end;

function TsmxProjectItems.FindByName(AProjectName: String): TsmxProjectItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ProjectName, AProjectName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxProjectItems.GetItem(Index: Integer): TsmxProjectItem;
begin
  Result := TsmxProjectItem(inherited Items[Index]);
end;

{ TsmxProjectManager }

constructor TsmxProjectManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProjectList := TsmxProjectItems.Create(TsmxProjectItem);
end;

destructor TsmxProjectManager.Destroy;
begin
  FProjectList.Free;
  inherited Destroy;
end;

procedure TsmxProjectManager.SetFileName(Value: String);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    FProjectList.Clear;
    ReadProjects;
  end;
end;

procedure TsmxProjectManager.ReadProjects;
var fs: TFileStream; pc: TsmxProjectConnection;
begin
  if FileExists(FFileName) then
  begin
    fs := TFileStream.Create(FFileName, fmOpenRead);
    try
      while fs.Position <> fs.Size do
      begin
        fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));
        with FProjectList.Add do
        begin
          ProjectName := pc.ProjectName;
          Generation := pc.Generation;
          LibraryName := pc.LibraryName;
          FunctionNameOrProgID := pc.FunctionNameOrProgID;
          WindowsAuthorization := pc.WindowsAuthorization;
          DatabaseName := pc.DatabaseName;
          DriverName := pc.DriverName;
          LoginPrompt := pc.LoginPrompt;
          Params := pc.Params;
        end;
      end;
    finally
      fs.Free;
    end;
  end;
end;

procedure TsmxProjectManager.WriteProjects;
var fs: TFileStream; pc: TsmxProjectConnection; i: Integer;
begin
  if FFileName <> '' then
  begin
    fs := TFileStream.Create(FFileName, fmCreate);
    try
      for i := 0 to FProjectList.Count - 1 do
      begin
        with pc do
        begin
          ProjectName := FProjectList[i].ProjectName;
          Generation := FProjectList[i].Generation;
          LibraryName := FProjectList[i].LibraryName;
          FunctionNameOrProgID := FProjectList[i].FunctionNameOrProgID;
          WindowsAuthorization := FProjectList[i].WindowsAuthorization;
          DatabaseName := FProjectList[i].DatabaseName;
          DriverName := FProjectList[i].DriverName;
          LoginPrompt := FProjectList[i].LoginPrompt;
          Params := FProjectList[i].Params;
        end;
        fs.WriteBuffer(pc, SizeOf(TsmxProjectConnection));
      end;
    finally
      fs.Free;
    end;
  end;
end;

end.

