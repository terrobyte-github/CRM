{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Program classes            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Аleksandr          }
{                                      }
{**************************************}

unit smxClasses;

interface

uses
  Classes, Controls, SysUtils, Windows, Forms, ImgList, XMLIntf, smxBaseClasses,
  smxDBIntf, smxTypes, smxDBTypes;

type
  { TsmxBaseCfg }

  //TsmxTargetRequest = class;
  TsmxCustomRequest = class;
  //TsmxRequestCfg = class;

  EsmxCfgError = class(Exception);

  TsmxBaseCfg = class(TsmxComponent)
  private
    FCfgDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    //FTargetRequest: TsmxTargetRequest;
    //FRequest: TsmxCustomRequest;
    FXMLDocIntf: IXMLDocument;
    FSelectRequest: TsmxCustomRequest;
    //FModifySetting: TsmxModifySetting;
    //FSelectRequestCfg: TsmxRequestCfg;
    //function CfgIDToDataSet(ACfgID: Integer): IsmxDataSet;
    //function GetRequest(ARequestCfg: TsmxRequestCfg): IsmxDataSet;
    //function GetRequestCfg(ACfgID: Integer): TsmxRequestCfg;
  protected
    function GetXMLText: String; virtual;
    procedure SetXMLText(Value: String); virtual;
    procedure SetCfgDatabase(const Value: IsmxDatabase); virtual;
    procedure SetCfgID(Value: Integer); virtual;
    //procedure SetModifySetting(Value: TsmxModifySetting); virtual;
    //procedure SetSelectRequestCfg(Value: TsmxRequestCfg); virtual;
    procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;
    procedure LoadCfg; virtual;
    procedure SaveCfg; virtual;

    //property TargetRequest: TsmxTargetRequest read FTargetRequest;
    //property Request: TsmxCustomRequest read FRequest;
    property XMLDoc: IXMLDocument read FXMLDocIntf;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); reintroduce; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    //procedure Finalize; virtual;
    //procedure Initialize; virtual;

    procedure ReadCfg; virtual;
    procedure WriteCfg; virtual;
    procedure Remove; virtual;
    procedure Receive;
    procedure Return;

    property CfgDatabase: IsmxDatabase read FCfgDatabaseIntf write SetCfgDatabase;
    property CfgID: Integer read FCfgID write SetCfgID;
    //property ModifySetting: TsmxModifySetting read FModifySetting write SetModifySetting;
    //property SelectRequestCfg: TsmxRequestCfg read FSelectRequestCfg write SetSelectRequestCfg;
    property SelectRequest: TsmxCustomRequest read FSelectRequest write SetSelectRequest;
    property XMLText: String read GetXMLText write SetXMLText;
  end;

  TsmxBaseCfgClass = class of TsmxBaseCfg;

  { TsmxCustomCommonStorage }

  TsmxCustomCommonStorage = class(TsmxComponent)
  protected
    function GetValue(Name: String): Variant; virtual;
    procedure SetValue(Name: String; Value: Variant); virtual;
  public
    function FindByName(AParamName: String): Variant; virtual;

    property ParamValues[Name: String]: Variant read GetValue write SetValue; default;
  end;

  { TsmxCustomLibraryManager }

  TsmxCustomLibraryManager = class(TsmxComponent)
  private
    FLibPath: String;
    FProcLibInfoName: String;
    FCheckComp: Boolean;
  protected
    procedure SetLibPath(Value: String); virtual;
    procedure SetProcLibInfoName(Value: String); virtual;
    procedure SetCheckComp(Value: Boolean); virtual;
  public
    function GetProcedure(ALibHandle: THandle; AProcName: String): Pointer; overload; virtual;
    function GetProcedure(ALibName, AProcName: String): Pointer; overload; virtual;
    function CheckLibraryComp(ALibHandle: THandle): Boolean; overload; virtual;
    function CheckLibraryComp(ALibName: String): Boolean; overload; virtual;
    function CallLibrary(ALibName: String): THandle; virtual;

    property CheckComp: Boolean read FCheckComp write SetCheckComp;
    property LibPath: String read FLibPath write SetLibPath;
    property ProcLibInfoName: String read FProcLibInfoName write SetProcLibInfoName;
  end;

  { TsmxCustomDatabaseManager }

  TsmxDatabaseConnection = class;

  TsmxCustomDatabaseManager = class(TsmxComponent)
  public
    //function FindByName(ADatabaseName: String): IsmxDatabase; virtual;
    function FindByName(ADatabaseName: String): TsmxDatabaseConnection; virtual;
    procedure InsertDBConnection(AConnection: TsmxDatabaseConnection); virtual;
    procedure RemoveDBConnection(AConnection: TsmxDatabaseConnection); virtual;
  end;

  { TsmxCustomFormManager }

  TsmxCustomForm = class;

  TsmxCustomFormManager = class(TsmxComponent)
  public
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm; virtual;
    function FindByHandle(AHandle: HWND): TsmxCustomForm; virtual;
    procedure InsertForm(AForm: TsmxCustomForm); virtual;
    procedure RemoveForm(AForm: TsmxCustomForm); virtual;
  end;

  { TsmxBaseCell }

  EsmxCellError = class(Exception);

  //TsmxBaseCellClass = class of TsmxBaseCell;

  TsmxBaseCell = class(TsmxComponent)
  private
    //FCfg: TsmxBaseCfg;
    FCellList: TList;
    FCfgDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    FImageList: TCustomImageList;
    FParentCell: TsmxBaseCell;
    FSelectRequest: TsmxCustomRequest;
    //FCommonStorage: TsmxCustomCommonStorage;
    //FLibraryManager: TsmxCustomLibraryManager;
    //FDatabaseManager: TsmxCustomDatabaseManager;
    function GetCellCount: Integer;
    //procedure SetCellCount(Value: Integer);
    function GetCell(Index: Integer): TsmxBaseCell;
    //procedure SetCell(Index: Integer; Value: TsmxBaseCell);
    function GetRootCell: TsmxBaseCell;
    function GetAccessoryForm: TsmxCustomForm;
    procedure ClearCells;
  protected
    //procedure CreateChilds; virtual;
    //procedure DestroyChilds; virtual;
    function GetInternalObject: TObject; virtual;
    //procedure InitChilds; virtual;
    //procedure InstallParent; virtual;
    function GetImageList: TCustomImageList; virtual;
    procedure SetImageList(Value: TCustomImageList); virtual;
    procedure SetParentCell(Value: TsmxBaseCell); virtual;
    //procedure UnInstallParent; virtual;
    //procedure UnInitialize; virtual;
    //procedure SetCommonStorage(Value: TsmxCustomCommonStorage); virtual;
    //procedure SetLibraryManager(Value: TsmxCustomLibraryManager); virtual;

    //property Cfg: TsmxBaseCfg read FCfg;
    property CellList: TList read FCellList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Initialize(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
      ASelectRequest: TsmxCustomRequest = nil); virtual;
    function FindCellByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;
    procedure AllCells(AList: TList; AClassList: Array of TsmxComponentClass);

    property CellCount: Integer read GetCellCount; //write SetCellCount;
    property Cells[Index: Integer]: TsmxBaseCell read GetCell; //write SetCell;
    property CfgDatabase: IsmxDatabase read FCfgDatabaseIntf;
    property CfgID: Integer read FCfgID;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property ParentCell: TsmxBaseCell read FParentCell write SetParentCell;
    property RootCell: TsmxBaseCell read GetRootCell;
    //property CommonStorage: TsmxCustomCommonStorage read FCommonStorage write SetCommonStorage;
    //property LibraryManager: TsmxCustomLibraryManager read FLibraryManager write SetLibraryManager;
    property AccessoryForm: TsmxCustomForm read GetAccessoryForm;
    property SelectRequest: TsmxCustomRequest read FSelectRequest;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

  { TsmxCellCfg }

  TsmxCellCfg = class(TsmxBaseCfg)
  //protected
  public
    procedure LoadCfg; override;
    procedure SaveCfg; override;
  end;

  { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxBaseCfg)
  private
    //FTypeCfgID: Integer;
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    procedure SetCfgClass(Value: TsmxBaseCfgClass);
    procedure SetCfgClassName(Value: String);
    procedure SetCellClass(Value: TsmxBaseCellClass);
    procedure SetCellClassName(Value: String);
  //protected
    //procedure LoadCfg; override;
    //procedure ReadCfg; override;
    //procedure SaveCfg; override;
    //procedure WriteCfg; override;
    //function GetTypeCfgID: Integer; virtual;
    //procedure SetCfgID(Value: Integer); override;
    //procedure SetSelectRequest(Value: TsmxCustomRequest); override;
  public
    procedure Clear; override;
    procedure ReadCfg; override;
    procedure WriteCfg; override;
    //procedure Remove; override;

    //property TypeCfgID: Integer read GetTypeCfgID;
    property CfgClass: TsmxBaseCfgClass read FCfgClass write SetCfgClass;
    property CfgClassName: String read FCfgClassName write SetCfgClassName;
    property CellClass: TsmxBaseCellClass read FCellClass write SetCellClass;
    property CellClassName: String read FCellClassName write SetCellClassName;
  end;

  { TsmxControlCell }

  TsmxControlCell = class(TsmxBaseCell)
  protected
    function GetCellAlign: TAlign; virtual;
    function GetCellCursor: TCursor; virtual;
    function GetCellEnable: Boolean; virtual;
    function GetCellHeight: Integer; virtual;
    function GetCellLeft: Integer; virtual;
    function GetCellTop: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    function GetCellWidth: Integer; virtual;
    procedure SetCellAlign(Value: TAlign); virtual;
    procedure SetCellCursor(Value: TCursor); virtual;
    procedure SetCellEnable(Value: Boolean); virtual;
    procedure SetCellHeight(Value: Integer); virtual;
    procedure SetCellLeft(Value: Integer); virtual;
    procedure SetCellTop(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;
    procedure SetCellWidth(Value: Integer); virtual;
  public
    //procedure Apply; virtual;
    //procedure Prepare(Forcibly: Boolean = False); virtual;

    property CellAlign: TAlign read GetCellAlign write SetCellAlign;
    property CellCursor: TCursor read GetCellCursor write SetCellCursor;
    property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    property CellHeight: Integer read GetCellHeight write SetCellHeight;
    property CellLeft: Integer read GetCellLeft write SetCellLeft;
    property CellTop: Integer read GetCellTop write SetCellTop;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    property CellWidth: Integer read GetCellWidth write SetCellWidth;
  end;

  { TsmxTargetRequest }

  TsmxTargetRequest = class(TsmxComponent)
  private
    FDatabaseIntf: IsmxDatabase;
    FRequestIntf: IsmxDataSet;
    FParamList: TStrings;
    FStream: TStream;
    function GetParamList: TStrings;
    function GetValue(Key: String): String;
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetValue(Key: String; const Value: String);
    function GetStream: TStream;
  protected
    property ParamList: TStrings read GetParamList;
    property Stream: TStream read GetStream;
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

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[Key: String]: String read GetValue write SetValue; default;
  end;

  { TsmxKitItem }

  TsmxKit = class;

  TsmxKitItem = class(TObject)
  private
    FKit: TsmxKit;
  protected
    function GetIndex: Integer; virtual;
    procedure SetIndex(Value: Integer); virtual;
    procedure SetKit(Value: TsmxKit); virtual;
  public
    constructor Create(AKit: TsmxKit); virtual;
    procedure Assign(AKitItem: TsmxKitItem); virtual;

    property ItemIndex: Integer read GetIndex write SetIndex;
    property Kit: TsmxKit read FKit write SetKit;
  end;

  TsmxKitItemClass = class of TsmxKitItem;

  { TsmxKit }

  EsmxKitError = class(Exception);

  TsmxKit = class(TObject)
  private
    FList: TList;
    FCellClass: TsmxKitItemClass;
    function GetCount: Integer;
    //procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): TsmxKitItem;
    procedure SetItem(Index: Integer; Value: TsmxKitItem);
  public
    constructor Create(AItemClass: TsmxKitItemClass);
    destructor Destroy; override;
    function Add: TsmxKitItem;
    procedure Clear;
    //function Insert(Index: Integer): TsmxKitItem;
    //procedure Remove(AItem: TsmxKitItem);
    procedure Delete(AIndex: Integer);
    procedure Assign(AKit: TsmxKit); virtual;

    property Count: Integer read GetCount; //write SetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem write SetItem; default;
  end;

  { TsmxHKitItem }

  TsmxHKit = class;

  TsmxHKitItem = class(TObject)
  private
    FList: TList;
    FParent: TsmxHKitItem;
    FHKit: TsmxHKit;
    function GetCount: Integer;
    //procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): TsmxHKitItem;
    procedure SetItem(Index: Integer; Value: TsmxHKitItem);
  protected
    //procedure Insert(AItem: TsmxHKitItem);
    //procedure Remove(AItem: TsmxHKitItem);
    function GetIndex: Integer; virtual;
    procedure SetIndex(Value: Integer); virtual;
    procedure SetParent(Value: TsmxHKitItem); virtual;
  public
    constructor Create(AHKit: TsmxHKit); virtual;
    destructor Destroy; override;
    function Add: TsmxHKitItem;
    procedure Clear;
    function HasChilds: Boolean;
    function HasParent: Boolean;
    //procedure Remove(AItem: TsmxHKitItem);
    procedure Delete(AIndex: Integer);
    procedure Assign(AHKitItem: TsmxHKitItem); virtual;

    property Count: Integer read GetCount; //write SetCount;
    property HKit: TsmxHKit read FHKit;
    property ItemIndex: Integer read GetIndex write SetIndex;
    property Items[Index: Integer]: TsmxHKitItem read GetItem write SetItem; default;
    property Parent: TsmxHKitItem read FParent write SetParent;
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
    procedure Assign(AHKit: TsmxHKit); virtual;

    property Root: TsmxHKitItem read FRoot;
  end;

  { TsmxParam }

  TsmxParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;
    procedure Assign(AKitItem: TsmxKitItem); override;

    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxParams }

  TsmxParams = class(TsmxKit)
  private
    //FIsCheckExists: Boolean;
    function GetItem(Index: Integer): TsmxParam;
    procedure SetItem(Index: Integer; Value: TsmxParam);
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  public
    function Add: TsmxParam;
    function FindByName(AParamName: String): TsmxParam;
    //procedure Assign(AKit: TsmxKit); override;
    function ParamByName(AParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem write SetItem; default;
    property Values[Name: String]: Variant read GetValue write SetValue; //default;
    //property IsCheckExists: Boolean read FIsCheckExists write FIsCheckExists;
  end;

  { TsmxStateUnit }

  TsmxStateUnits = class;

  TsmxStateUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FCurrentIntfID: Integer;
    FSourceIntfID: Integer;
    FUnitEnable: Boolean;
    FUnitVisible: Boolean;
    function GetHKit: TsmxStateUnits;
    function GetItem(Index: Integer): TsmxStateUnit;
    procedure SetItem(Index: Integer; Value: TsmxStateUnit);
    function GetParent: TsmxStateUnit;
    procedure SetParent(Value: TsmxStateUnit);
    procedure SwitchIntfID;
  protected
    procedure SetUnitEnable(Value: Boolean); virtual;
    procedure SetUnitVisible(Value: Boolean); virtual;
    procedure SetSourceIntfID(Value: Integer); virtual;
    procedure SetCurrentIntfID(Value: Integer); virtual;
    procedure SetCfgID(Value: Integer); virtual;
  public
    constructor Create(AHKit: TsmxHKit); override;
    function Add: TsmxStateUnit;
    function FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxStateUnit;

    property CfgID: Integer read FCfgID write SetCfgID;
    property HKit: TsmxStateUnits read GetHKit;
    property CurrentIntfID: Integer read FCurrentIntfID write SetCurrentIntfID;
    property SourceIntfID: Integer read FSourceIntfID write SetSourceIntfID;
    property Items[Index: Integer]: TsmxStateUnit read GetItem write SetItem; default;
    property Parent: TsmxStateUnit read GetParent write SetParent;
    property UnitEnable: Boolean read FUnitEnable write SetUnitEnable;
    property UnitVisible: Boolean read FUnitVisible write SetUnitVisible;
  end;

  { TsmxStateUnits }

  TsmxStateUnits = class(TsmxHKit)
  private
    FIntfID: Integer;
    function GetRoot: TsmxStateUnit;
  protected
    procedure SetIntfID(Value: Integer); virtual;
  public
    property Root: TsmxStateUnit read GetRoot;
    property IntfID: Integer read FIntfID write SetIntfID;
  end;

  { TsmxCellState }

  TsmxCellState = class(TsmxKitItem)
  private
    FStateID: Integer;
    FStateUnits: TsmxStateUnits;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;

    property StateID: Integer read FStateID write FStateID;
    property StateUnits: TsmxStateUnits read FStateUnits;
  end;

  { TsmxCellStates }

  TsmxCellStates = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxCellState;
  public
    function Add: TsmxCellState;
    function FindByStateID(AStateID: Integer): TsmxCellState;

    property Items[Index: Integer]: TsmxCellState read GetItem; default;
  end;

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

  { TsmxIntfCfg }

  TsmxIntfCfg = class(TsmxBaseCfg)
  private
    FIntfID: Integer;
    //FXMLDocList: TsmxXMLDocItems;
    FCellStates: TsmxCellStates;
    //FStateRequest: TsmxRequestSetting;
    //FStateInterfaces: TsmxInterfaceCfg;
    //FRequest: TsmxCustomRequest;
    function GetCellStates: TsmxCellStates;
    //function GetRequest: TsmxCustomRequest;
  protected
    procedure LoadCfg; override;
    //procedure ReadCfg; override;
    procedure SaveCfg; override;
    //procedure WriteCfg; override;
    function GetXMLText: String; override;
    procedure SetXMLText(Value: String); override;
    function GetFullXMLText: String; virtual;
    procedure SetIntfID(Value: Integer); virtual;
    //procedure SetStateRequest(Value: TsmxRequestSetting); virtual;
    //function GetStateInterfaces: TsmxInterfaceCfg; virtual;

    //property XMLDocList: TsmxXMLDocItems read FXMLDocList;
    //property Request: TsmxCustomRequest read GetRequest;
  public
    {constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer); override;
    constructor CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer; AIntfID: Integer); virtual;}
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ReadCfg; override;
    procedure WriteCfg; override;

    property CellStates: TsmxCellStates read GetCellStates;
    property IntfID: Integer read FIntfID write SetIntfID;
    property FullXMLText: String read GetFullXMLText;
    //property StateRequest: TsmxRequestSetting read FStateRequest write SetStateRequest;
    //property StateInterfaces: TsmxInterfaceCfg read FStateInterfaces;
  end;

  //TsmxIntfCfgClass = class of TsmxIntfCfg;

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
    FUserName: String;
    FPassword: String;
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
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadProjects;
    procedure WriteProjects;

    property FileName: String read FFileName write FFileName;
    property ProjectList: TsmxProjectItems read FProjectList;
  end;

  { TsmxDatabaseConnection }

  TsmxDatabaseConnection = class(TsmxComponent)
  private
    FDatabaseName: String;
    FLibraryName: String;
    FFunctionNameOrProgID: String;
    FDriverName: String;
    FLoginPrompt: Boolean;
    FParams: String;
    FGenerationMode: TsmxGenerationMode;
    FUserName: String;
    FPassword: String;
    FDatabaseIntf: IsmxDatabase;
    FLibraryManager: TsmxCustomLibraryManager;
    FDatabaseManager: TsmxCustomDatabaseManager;
    function CreateDatabaseAsFunc: IsmxDatabase;
    function CreateDatabaseAsCOM: IsmxDatabase;
    procedure SetDatabaseName(Value: String);
    procedure SetLibraryManager(Value: TsmxCustomLibraryManager);
    procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager);
  public
    destructor Destroy; override;
    procedure ConnectToDatabase;
    procedure DisconnectFromDatabase;

    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property LibraryName: String read FLibraryName write FLibraryName;
    property FunctionNameOrProgID: String read FFunctionNameOrProgID write FFunctionNameOrProgID;
    property DriverName: String read FDriverName write FDriverName;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Params: String read FParams write FParams;
    property GenerationMode: TsmxGenerationMode read FGenerationMode write FGenerationMode;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property Database: IsmxDatabase read FDatabaseIntf;
    property LibraryManager: TsmxCustomLibraryManager read FLibraryManager write SetLibraryManager;
    property DatabaseManager: TsmxCustomDatabaseManager read FDatabaseManager write SetDatabaseManager;
  end;

  { TsmxCustomRequest }

  TsmxSenseFields = class;

  TsmxLocationParams = class;

  TsmxCustomRequest = class(TsmxBaseCell)
  private
    //FDataSetIntf: IsmxDataSet;
    FDataSetType: TsmxDataSetType;
    FDatabaseName: String;
    FDatabaseIntf: IsmxDatabase;
    FOperationMode: TsmxOperationMode;
    FDatabaseManager: TsmxCustomDatabaseManager;
    FPerformanceMode: TsmxPerformanceMode;
    FSQLText: String;
    FRequestModify: TsmxModifySetting;
    FRequestFields: TsmxSenseFields;
    FRequestParams: TsmxLocationParams;
    FInsertRequest: TsmxCustomRequest;
    FUpdateRequest: TsmxCustomRequest;
    FDeleteRequest: TsmxCustomRequest;
    //FDataStream: TStream;
    FCommonStorage: TsmxCustomCommonStorage;
    FPrepared: Boolean;
    //function GetDataStream: TStream;
    //function GetDataSet: IsmxDataSet;
  protected
    //FDataSetIntf: IsmxDataSet;
    //function GetInternalObject: TObject; override;
    function GetDatabase: IsmxDatabase; virtual;
    procedure SetDatabaseName(Value: String); virtual;
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager); virtual;
    //function GetDataSet: IsmxDataSet; virtual;
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;
    procedure SetSQLText(Value: String); virtual;
    procedure SetRequestModify(Value: TsmxModifySetting); virtual;
    procedure SetDataSetType(Value: TsmxDataSetType); virtual;
    procedure SetOperationMode(Value: TsmxOperationMode); virtual;
    //function GetRequestFields: TsmxSenseFields;
    //function GetRequestParams: TsmxLocationParams;
    procedure SetRequestFields(Value: TsmxSenseFields); virtual;
    procedure SetRequestParams(Value: TsmxLocationParams); virtual;
    function GetModifyRequest(Modify: TsmxModifyRequest): TsmxCustomRequest; virtual;
    procedure SetCommonStorage(Value: TsmxCustomCommonStorage); virtual;
    function GetDatabaseManager: TsmxCustomDatabaseManager; virtual;
    function GetCommonStorage: TsmxCustomCommonStorage; virtual;
    function GetParamValue(Name: String): Variant; virtual;
    procedure SetParamValue(Name: String; Value: Variant); virtual;
    function GetFieldValue(Name: String): Variant; virtual;
    procedure SetFieldValue(Name: String; Value: Variant); virtual;
    procedure SetPrepared(Value: Boolean); virtual;
    function GetPrepared: Boolean; virtual;

    //property CellDataSet: IsmxDataSet read GetDataSet;
    //property DataStream: TStream read GetDataStream;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //function FindBySense(ASense: TsmxFieldSense; AFields: TsmxFieldArray): Integer;
    //function FindByLocation(ALocation: TsmxParamLocation; AParams: TsmxParamArray): Integer;
    //function FindField(AFieldName: String): IsmxField;
    //function FindParam(AParamName: String): IsmxParam;
    procedure Perform; virtual;
    procedure RefreshParams; virtual;
    procedure Prepare; virtual;
    procedure First; virtual;
    procedure Last; virtual;
    procedure Next; virtual;
    procedure Prior; virtual;
    function GetBof: Boolean; virtual;
    function GetEof: Boolean; virtual;

    //property CellDataSet: IsmxDataSet read FDataSetIntf; //GetDataSet;
    property DataSetType: TsmxDataSetType read FDataSetType write SetDataSetType;
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property Database: IsmxDatabase read {FDatabaseIntf} GetDatabase write SetDatabase;
    property OperationMode: TsmxOperationMode read FOperationMode write SetOperationMode {default omManual};
    property DatabaseManager: TsmxCustomDatabaseManager read GetDatabaseManager write SetDatabaseManager;
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write SetPerformanceMode;
    property SQLText: String read FSQLText write SetSQLText;
    property RequestModify: TsmxModifySetting read FRequestModify write SetRequestModify;
    property RequestFields: TsmxSenseFields read FRequestFields write SetRequestFields;
    property RequestParams: TsmxLocationParams read FRequestParams write SetRequestParams;
    property ModifyRequest[Modify: TsmxModifyRequest]: TsmxCustomRequest read GetModifyRequest;
    property CommonStorage: TsmxCustomCommonStorage read GetCommonStorage write SetCommonStorage;
    property ParamValue[Name: String]: Variant read GetParamValue write SetParamValue;
    property FieldValue[Name: String]: Variant read GetFieldValue write SetFieldValue;
    property Bof: Boolean read GetBof;
    property Eof: Boolean read GetEof;
    property Prepared: Boolean read GetPrepared write SetPrepared;
  end;

  { TsmxCustomColumn }

  TsmxCustomColumn = class(TsmxControlCell)
  end;

  { TsmxCustomGrid }

  TsmxCustomGrid = class(TsmxControlCell)
  private
    FColumnList: TList;
    FRequest: TsmxCustomRequest;
    function GetColumn(Index: Integer): TsmxCustomColumn;
    function GetColumnCount: Integer;
    procedure SetColumnCount(Value: Integer);
    procedure SetColumn(Index: Integer; Value: TsmxCustomColumn);
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
    //function GetColumnClass: TClass;

    property ColumnList: TList read FColumnList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure ClearColumns;

    property ColumnCount: Integer read GetColumnCount write SetColumnCount;
    property Columns[Index: Integer]: TsmxCustomColumn read GetColumn write SetColumn; default;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
  end;

  { TsmxCustomAlgorithm }

  TsmxCustomAlgorithm = class(TsmxBaseCell)
  private
    FAlgorithmParams: TsmxLocationParams;
    //FMenuPointID: Integer;
    //FToolBoardID: Integer;
    //function GetParams: TsmxParams;
    //FParams: TsmxParams;
    FCommonStorage: TsmxCustomCommonStorage;
    FLibraryManager: TsmxCustomLibraryManager;
  protected
    //procedure AddParams; virtual;
    function GetAlgorithmCaption: String; virtual;
    function GetAlgorithmEnable: Boolean; virtual;
    function GetAlgorithmHotKey: Integer; virtual;
    function GetAlgorithmImageIndex: Integer; virtual;
    function GetAlgorithmVisible: Boolean; virtual;
    procedure SetAlgorithmCaption(Value: String); virtual;
    procedure SetAlgorithmEnable(Value: Boolean); virtual;
    procedure SetAlgorithmHotKey(Value: Integer); virtual;
    procedure SetAlgorithmImageIndex(Value: Integer); virtual;
    procedure SetAlgorithmVisible(Value: Boolean); virtual;
    function GetAlgorithmHint: String; virtual;
    procedure SetAlgorithmHint(Value: String); virtual;
    procedure SetAlgorithmParams(Value: TsmxLocationParams); virtual;
    function GetParamValue(Name: String): Variant; virtual;
    procedure SetParamValue(Name: String; Value: Variant); virtual;
    function GetCommonStorage: TsmxCustomCommonStorage; virtual;
    function GetLibraryManager: TsmxCustomLibraryManager; virtual;
    procedure SetCommonStorage(Value: TsmxCustomCommonStorage); virtual;
    procedure SetLibraryManager(Value: TsmxCustomLibraryManager); virtual;

    //property Params: TsmxParams read FParams;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure RefreshParams; virtual;
    //function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam; virtual;

    property AlgorithmParams: TsmxLocationParams read FAlgorithmParams write SetAlgorithmParams;
    property AlgorithmCaption: String read GetAlgorithmCaption write SetAlgorithmCaption;
    property AlgorithmHint: String read GetAlgorithmHint write SetAlgorithmHint;
    property AlgorithmEnable: Boolean read GetAlgorithmEnable write SetAlgorithmEnable;
    property AlgorithmHotKey: Integer read GetAlgorithmHotKey write SetAlgorithmHotKey;
    property AlgorithmImageIndex: Integer read GetAlgorithmImageIndex write SetAlgorithmImageIndex;
    property AlgorithmVisible: Boolean read GetAlgorithmVisible write SetAlgorithmVisible;
    //property MenuPointID: Integer read FMenuPointID write FMenuPointID default 0;
    //property ToolBoardID: Integer read FToolBoardID write FToolBoardID default 0;
    property ParamValue[Name: String]: Variant read GetParamValue write SetParamValue;
    property CommonStorage: TsmxCustomCommonStorage read GetCommonStorage write SetCommonStorage;
    property LibraryManager: TsmxCustomLibraryManager read GetLibraryManager write SetLibraryManager;
  end;

  { TsmxCustomLibAlgorithm }

  {TsmxCustomLibAlgorithm = class(TsmxCustomAlgorithm)
  private
    FLibManager: TsmxCustomLibraryManager;
  protected
    procedure SetLibraryManager(Value: TsmxCustomLibraryManager); virtual;
  public
    property LibManager: TsmxCustomLibraryManager read FLibManager write SetLibraryManager;
  end;}

  { TsmxCustomAlgorithmList }

  TsmxCustomAlgorithmList = class(TsmxBaseCell)
  private
    FAlgorithmList: TList;
    //FMasterMenu: TsmxCustomMainMenu;
    //FControlBoard: TsmxCustomControlBoard;
    //FIsCreateToolButton: Boolean;
    //FIsCreateMenuPoint: Boolean;
    function GetAlgorithm(Index: Integer): TsmxCustomAlgorithm;
    procedure SetAlgorithm(Index: Integer; Value: TsmxCustomAlgorithm);
    function GetAlgorithmCount: Integer;
    procedure SetAlgorithmCount(Value: Integer);
    //procedure AddAlgorithmsTo(ACell: TsmxControlCell);
    //procedure DelAlgorithmsTo(ACell: TsmxControlCell);
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;
    //procedure SetIsCreateMenuPoint(Value: Boolean); virtual;
    //procedure SetIsCreateToolButton(Value: Boolean); virtual;
    //procedure SetMasterMenu(Value: TsmxCustomMainMenu); virtual;
    //procedure SetControlBoard(Value: TsmxCustomControlBoard); virtual;

    property AlgorithmList: TList read FAlgorithmList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure ClearAlgorithms;
    //function FindAlgorithmByCfgID(ACfgID: Integer): TsmxCustomAlgorithm;
    //procedure AddAlgorithmsTo(ACell: TsmxControlCell); virtual;
    //procedure DelAlgorithmsTo(ACell: TsmxControlCell); virtual;
    procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
    procedure RemoveAlgorithm(Algorithm: TsmxCustomAlgorithm);

    property AlgorithmCount: Integer read GetAlgorithmCount write SetAlgorithmCount;
    property Algorithms[Index: Integer]: TsmxCustomAlgorithm read GetAlgorithm write SetAlgorithm; default;
    //property MasterMenu: TsmxCustomMainMenu read FMasterMenu write SetMasterMenu;
    //property ControlBoard: TsmxCustomControlBoard read FControlBoard write SetControlBoard;
    //property IsCreateToolButton: Boolean read FIsCreateToolButton write FIsCreateToolButton default False;
    //property IsCreateMenuPoint: Boolean read FIsCreateMenuPoint write FIsCreateMenuPoint default False;
  end;

  { TsmxCustomFilter }

  TsmxCustomFilter = class(TsmxControlCell)
  private
    FAlgorithm: TsmxCustomAlgorithm;
    FFilterName: String;
    //FFilterText: String;
    //FFilterValue: Variant;
    FTextRequest: TsmxCustomRequest;
    FIsPutText: Boolean;
  protected
    //procedure DestroyChilds; override;
    function GetFilterText: String; virtual;
    function GetFilterValue: Variant; virtual;
    //procedure InstallParent; override;
    procedure SetFilterText(Value: String); virtual;
    procedure SetFilterValue(Value: Variant); virtual;
    procedure SetFilterName(Value: String); virtual;
    //procedure UnInstallParent; override;
    procedure SetAlgorithm(Value: TsmxCustomAlgorithm); virtual;
    procedure SetTextRequest(Value: TsmxCustomRequest); virtual;
    procedure SetIsPutText(Value: Boolean); virtual;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    //destructor Destroy; override;

    property Algorithm: TsmxCustomAlgorithm read FAlgorithm write SetAlgorithm;
    property FilterName: String read FFilterName write SetFilterName;
    property FilterText: String read GetFilterText write SetFilterText;
    property FilterValue: Variant read GetFilterValue write SetFilterValue;
    property TextRequest: TsmxCustomRequest read FTextRequest write SetTextRequest;
    property IsPutText: Boolean read FIsPutText write SetIsPutText;
  end;

  { TsmxCustomFilterDesk }

  TsmxCustomFilterDesk = class(TsmxControlCell)
  private
    FApplyRequest: TsmxCustomRequest;
    FFilterList: TList;
    FSelectRequest: TsmxCustomRequest;
    function GetFilter(Index: Integer): TsmxCustomFilter;
    procedure SetFilter(Index: Integer; Value: TsmxCustomFilter);
    function GetFilterCount: Integer;
    procedure SetFilterCount(Value: Integer);
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;
    procedure SetApplyRequest(Value: TsmxCustomRequest); virtual;
    procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;

    property FilterList: TList read FFilterList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    function FindFilterByName(const AFilterName: String): TsmxCustomFilter;
    procedure Apply; virtual;
    procedure Prepare; virtual;
    procedure ClearFilters;
    procedure AddFilter(Filter: TsmxCustomFilter);
    procedure RemoveFilter(Filter: TsmxCustomFilter);

    property FilterCount: Integer read GetFilterCount write SetFilterCount;
    property Filters[Index: Integer]: TsmxCustomFilter read GetFilter write SetFilter; default;
    property ApplyRequest: TsmxCustomRequest read FApplyRequest write SetApplyRequest;
    property SelectRequest: TsmxCustomRequest read FSelectRequest write SetSelectRequest;
  end;

  { TsmxCustomSection }

  TsmxCustomSection = class(TsmxControlCell)
  private
    FFilterDesk: TsmxCustomFilterDesk;
    FGrid: TsmxCustomGrid;
    //FRequest: TsmxCustomRequest;
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;
    procedure SetGrid(Value: TsmxCustomGrid); virtual;
    //procedure SetRequest(Value: TsmxCustomRequest); virtual;
    //procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager); override;
    //procedure Initialize; override;
    //procedure UnInitialize; override;
    procedure SetFilterDesk(Value: TsmxCustomFilterDesk); virtual;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    //destructor Destroy; override;
    //procedure Apply; override;
    //procedure Prepare(Forcibly: Boolean = False); override;

    property Grid: TsmxCustomGrid read FGrid write SetGrid;
    property FilterDesk: TsmxCustomFilterDesk read FFilterDesk write SetFilterDesk;
    //property Request: TsmxCustomRequest read FRequest write SetRequest;
  end;

  { TsmxCustomPage }

  TsmxCustomPage = class(TsmxControlCell)
  private
    FSectionList: TList;
    function GetSection(Index: Integer): TsmxCustomSection;
    function GetSectionCount: Integer;
    procedure SetSectionCount(Value: Integer);
    procedure SetSection(Index: Integer; Value: TsmxCustomSection);
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property SectionList: TList read FSectionList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //procedure Apply; override;
    //procedure Prepare(Forcibly: Boolean = False); override;
    procedure ClearSectioins;
    procedure AddSection(Section: TsmxCustomSection);
    procedure RemoveSection(Section: TsmxCustomSection);

    property SectionCount: Integer read GetSectionCount write SetSectionCount;
    property Sections[Index: Integer]: TsmxCustomSection read GetSection write SetSection; default;
  end;

  { TsmxCustomPageManager }

  TsmxCustomPageManager = class(TsmxControlCell)
  private
    FPageList: TList;
    function GetPage(Index: Integer): TsmxCustomPage;
    procedure SetPage(Index: Integer; Value: TsmxCustomPage);
    function GetPageCount: Integer;
    procedure SetPageCount(Value: Integer);
  protected
    //procedure DestroyChilds; override;
    function GetActivePage: TsmxCustomPage; virtual;
    procedure SetActivePage(Value: TsmxCustomPage); virtual;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property PageList: TList read FPageList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //procedure Apply; override;
    //procedure Prepare(Forcibly: Boolean = False); override;
    procedure ClearPages;
    procedure AddPage(Page: TsmxCustomPage);
    procedure RemovePage(Page: TsmxCustomPage);

    property ActivePage: TsmxCustomPage read GetActivePage write SetActivePage;
    property PageCount: Integer read GetPageCount write SetPageCount;
    property Pages[Index: Integer]: TsmxCustomPage read GetPage write SetPage; default;
  end;

  { TsmxCustomMenuItem }

  TsmxCustomMenuItem = class(TsmxControlCell)
  private
    FMenuItemList: TList;
    FAlgorithm: TsmxCustomAlgorithm;
    function GetMenuItem(Index: Integer): TsmxCustomMenuItem;
    procedure SetMenuItem(Index: Integer; Value: TsmxCustomMenuItem);
    function GetMenuItemCount: Integer;
    procedure SetMenuItemCount(Value: Integer);
  protected
    procedure SetAlgorithm(Value: TsmxCustomAlgorithm); virtual;

    property MenuItemList: TList read FMenuItemList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure ClearMenuItems;
    procedure AddMenuItem(MenuItem: TsmxCustomMenuItem);
    procedure RemoveMenuItem(MenuItem: TsmxCustomMenuItem);

    property MenuItemCount: Integer read GetMenuItemCount write SetMenuItemCount;
    property MenuItems[Index: Integer]: TsmxCustomMenuItem read GetMenuItem write SetMenuItem; default;
    property Algorithm: TsmxCustomAlgorithm read FAlgorithm write SetAlgorithm;
  end;

  { TsmxCustomMainMenu }

  TsmxCustomMainMenu = class(TsmxControlCell)
  private
    //FMenuItemList: TList;
    //function GetMenuItem(Index: Integer): TsmxCustomMenuItem;
    //procedure SetMenuItem(Index: Integer; Value: TsmxCustomMenuItem);
    //function GetMenuItemCount: Integer;
    //procedure SetMenuItemCount(Value: Integer);
    FMenuItems: TsmxCustomMenuItem;
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;

    //property MenuItemList: TList read FMenuItemList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //function FindMenuPointByCfgID(ACfgID: Integer): TsmxCustomMenuPoint;
    //function MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint; virtual;
    //procedure ClearMenuItems;
    //procedure AddMenuItem(MenuItem: TsmxCustomMenuItem);
    //procedure RemoveMenuItem(MenuItem: TsmxCustomMenuItem);

    //property MenuItemCount: Integer read GetMenuItemCount write SetMenuItemCount;
    //property MenuItems[Index: Integer]: TsmxCustomMenuItem read GetMenuItem write SetMenuItem; default;
    property MenuItems: TsmxCustomMenuItem read FMenuItems;
  end;

  { TsmxCustomToolBoard }

  TsmxCustomToolBoard = class(TsmxControlCell)
  public
    //procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); virtual;
    //procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); virtual;
  end;

  { TsmxCustomControlBoard }

  TsmxCustomControlBoard = class(TsmxControlCell)
  private
    FToolBoardList: TList;
    function GetToolBoard(Index: Integer): TsmxCustomToolBoard;
    procedure SetToolBoard(Index: Integer; Value: TsmxCustomToolBoard);
    function GetToolBoardCount: Integer;
    procedure SetToolBoardCount(Value: Integer);
  protected
    //procedure DestroyChilds; override;
    //procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property ToolBoardList: TList read FToolBoardList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //function FindToolBoardByCfgID(ACfgID: Integer): TsmxCustomToolBoard;
    //procedure Prepare(Forcibly: Boolean = False); override;
    procedure ClearToolBoards;
    procedure AddToolBoard(ToolBoard: TsmxCustomToolBoard);
    procedure RemoveToolBoard(ToolBoard: TsmxCustomToolBoard);

    property ToolBoardCount: Integer read GetToolBoardCount write SetToolBoardCount;
    property ToolBoards[Index: Integer]: TsmxCustomToolBoard read GetToolBoard write SetToolBoard; default;
  end;

  { TsmxCustomStatusBoard }

  TsmxCustomStatusBoard = class(TsmxControlCell)
  end;

  { TsmxCustomForm }

  TsmxCustomForm = class(TsmxControlCell)
  private
    FAlgorithmList: TsmxCustomAlgorithmList;
    FControlBoard: TsmxCustomControlBoard;
    FMainMenu: TsmxCustomMainMenu;
    FPageManagerList: TList;
    FStatusBoard: TsmxCustomStatusBoard;
    FParentForm: TsmxCustomForm;
    FID: Integer;
    FIntfID: Integer;
    FStateID: Integer;
    FStateRequest: TsmxCustomRequest;
    FStateCfg: TsmxIntfCfg;
    FFormManager: TsmxCustomFormManager;
    FDatabaseManager: TsmxCustomDatabaseManager;
    FCommonStorage: TsmxCustomCommonStorage;
    FLibraryManager: TsmxCustomLibraryManager;
    function GetPageManager(Index: Integer): TsmxCustomPageManager;
    function GetPageManagerCount: Integer;
    procedure SetPageManager(Index: Integer; Value: TsmxCustomPageManager);
    procedure SetPageManagerCount(Value: Integer);
  protected
    //procedure AddAlgorithms; virtual;
    //procedure DelAlgorithms; virtual;
    //procedure DestroyChilds; override;
    function GetFormModalResult: TModalResult; virtual;
    //procedure InstallParent; override;
    procedure SetFormModalResult(Value: TModalResult); virtual;
    procedure SetParentForm(Value: TsmxCustomForm); virtual;
    //procedure UnInstallParent; override;
    procedure ChangeState; virtual;
    procedure PutState; virtual;
    procedure SetFormManager(Value: TsmxCustomFormManager); virtual;
    procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager); virtual;
    procedure SetCommonStorage(Value: TsmxCustomCommonStorage); virtual;
    procedure SetLibraryManager(Value: TsmxCustomLibraryManager); virtual;
    procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetControlBoard(Value: TsmxCustomControlBoard); virtual;
    procedure SetMainMenu(Value: TsmxCustomMainMenu); virtual;
    procedure SetStatusBoard(Value: TsmxCustomStatusBoard); virtual;
    procedure SetStateRequest(Value: TsmxCustomRequest); virtual;
    //procedure SetIsMainForm(Value: Boolean); virtual;
    procedure SetStateID(Value: Integer); virtual;
    procedure SetIntfID(Value: Integer); virtual;

    property StateCfg: TsmxIntfCfg read FStateCfg;
    property PageManagerList: TList read FPageManagerList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer); override;
    constructor CreateByID(AOwner: TComponent; AID: Integer); virtual;
    destructor Destroy; override;
    procedure Apply; virtual;
    procedure CloseForm; virtual;
    procedure ShowForm; virtual;
    function ShowModalForm: TModalResult; virtual;
    procedure Prepare; virtual;
    procedure ClearPageManagers;
    procedure AddPageManager(PageManager: TsmxCustomPageManager);
    procedure RemovePageManager(PageManager: TsmxCustomPageManager);

    property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write SetAlgorithmList;
    property ControlBoard: TsmxCustomControlBoard read FControlBoard write SetControlBoard;
    property FormModalResult: TModalResult read GetFormModalResult write SetFormModalResult;
    property MainMenu: TsmxCustomMainMenu read FMainMenu write SetMainMenu;
    property ParentForm: TsmxCustomForm read FParentForm write SetParentForm;
    property PageManagerCount: Integer read GetPageManagerCount write SetPageManagerCount;
    property PageManagers[Index: Integer]: TsmxCustomPageManager read GetPageManager write SetPageManager; default;
    property StatusBoard: TsmxCustomStatusBoard read FStatusBoard write SetStatusBoard;
    property ID: Integer read FID;
    property IntfID: Integer read FIntfID write SetIntfID;
    property StateID: Integer read FStateID write SetStateID;
    property StateRequest: TsmxCustomRequest read FStateRequest write SetStateRequest;
    property FormManager: TsmxCustomFormManager read FFormManager write SetFormManager;
    property DatabaseManager: TsmxCustomDatabaseManager read FDatabaseManager write SetDatabaseManager;
    property CommonStorage: TsmxCustomCommonStorage read FCommonStorage write SetCommonStorage;
    property LibraryManager: TsmxCustomLibraryManager read FLibraryManager write SetLibraryManager;
    //property IsMainForm: Boolean read FIsMainForm write SetIsMainForm;
  end;

  TsmxCustomFormClass = class of TsmxCustomForm;

  { TsmxCustomMasterForm }

  TsmxCustomMasterForm = class(TsmxCustomForm)
  protected
    FForm: TForm;
    procedure SetForm(Value: TForm); virtual;
  public
    property Form: TForm read FForm write SetForm;
  end;

  { TsmxLocationParam }

  TsmxLocationParam = class(TsmxKitItem)
  private
    FParamLocation: TsmxParamLocation;
    FParamName: String;
    FParamDefValue: Variant;
    FParamType: TsmxParamType;
    FDataType: TsmxDataType;
  public
    constructor Create(AKit: TsmxKit); override;
    procedure Assign(AKitItem: TsmxKitItem); override;

    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamName: String read FParamName write FParamName;
    property ParamDefValue: Variant read FParamDefValue write FParamDefValue;
    property ParamType: TsmxParamType read FParamType write FParamType;
    property DataType: TsmxDataType read FDataType write FDataType;
  end;

  TsmxLocationParamArray = Array of TsmxLocationParam;

  { TsmxLocationParams }

  TsmxLocationParams = class(TsmxKit)
  private
    FModified: Boolean;
    function GetItem(Index: Integer): TsmxLocationParam;
    procedure SetItem(Index: Integer; Value: TsmxLocationParam);
    function GetLocationCount(ALocation: TsmxParamLocation): Integer;
  protected
    procedure SetModified(Value: Boolean); virtual;
  public
    function Add: TsmxLocationParam;
    function FindByName(AParamName: String): TsmxLocationParam;
    //procedure Assign(AKit: TsmxKit); override;
    function ParamByName(AParamName: String): TsmxLocationParam;
    function FindByLocation(ALocation: TsmxParamLocation; var AParams: TsmxLocationParamArray): Integer;

    property Items[Index: Integer]: TsmxLocationParam read GetItem write SetItem; default;
    //property LocationCount[Location: TsmxParamLocation]: Integer read GetLocationCount;
    property Modified: Boolean read FModified write SetModified;
  end;

  { TsmxSenseField }

  TsmxSenseField = class(TsmxKitItem)
  private
    FFieldFormat: String;
    FFieldName: String;
    FFieldSense: TsmxFieldSense;
  public
    constructor Create(AKit: TsmxKit); override;
    procedure Assign(AKitItem: TsmxKitItem); override;

    property FieldFormat: String read FFieldFormat write FFieldFormat;
    property FieldName: String read FFieldName write FFieldName;
    property FieldSense: TsmxFieldSense read FFieldSense write FFieldSense;
  end;

  TsmxSenseFieldArray = Array of TsmxSenseField;

  { TsmxSenseFields }

  TsmxSenseFields = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxSenseField;
    procedure SetItem(Index: Integer; Value: TsmxSenseField);
    function GetSenseCount(ASense: TsmxFieldSense): Integer;
  public
    function Add: TsmxSenseField;
    function FindByName(AFieldName: String): TsmxSenseField;
    //procedure Assign(AKit: TsmxKit); override;
    function FieldByName(AFieldName: String): TsmxSenseField;
    function FindBySense(ASense: TsmxFieldSense; var AFields: TsmxSenseFieldArray): Integer;

    property Items[Index: Integer]: TsmxSenseField read GetItem write SetItem; default;
  end;

  { TsmxRequestCfg }

  {TsmxRequestCfg = class(TsmxCellCfg)
  private
    FDataSetType: TsmxDataSetType;
    FPerformanceMode: TsmxPerformanceMode;
    FRequestFields: TsmxSenseFields;
    FRequestParams: TsmxLocationParams;
    FSQLText: String;
    FModifySetting: TsmxModifySetting;
    function GetRequestFields: TsmxSenseFields;
    function GetRequestParams: TsmxLocationParams;
    procedure SetModifySetting(Value: TsmxModifySetting);
  //protected
    //procedure ReadCfg; override;
    //procedure WriteCfg; override;
    //procedure SetModifySetting(Value: TsmxModifySetting); virtual;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure ReadCfg; override;
    procedure WriteCfg; override;

    property DataSetType: TsmxDataSetType read FDataSetType write FDataSetType;
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write FPerformanceMode;
    property RequestFields: TsmxSenseFields read GetRequestFields;
    property RequestParams: TsmxLocationParams read GetRequestParams;
    property SQLText: String read FSQLText write FSQLText;
    property ModifySetting: TsmxModifySetting read FModifySetting write SetModifySetting;
  end;}

implementation

uses
  DB, Variants, ComObj, StrUtils{, XMLDoc}, smxFuncs, smxClassFuncs,
  smxConsts, smxProcs, smxDBFuncs;

{ TsmxBaseCfg }

{constructor TsmxBaseCfg.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer);
begin
  inherited Create(AOwner);
  FCfgDatabaseIntf := ADatabase;
  FCfgID := ACfgID;
  FTargetRequest := TsmxTargetRequest.Create(Self);
  FTargetRequest.Database := FCfgDatabaseIntf;
  FXMLDocIntf := NewXML; //NewXMLDocument;
end;}

constructor TsmxBaseCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FCfgDatabaseIntf := ADatabase;
  //FCfgID := ACfgID;
  //FTargetRequest := TsmxTargetRequest.Create(Self);
  //FTargetRequest.Database := FCfgDatabaseIntf;
  FXMLDocIntf := smxFuncs.NewXML; //NewXMLDocument;
end;

destructor TsmxBaseCfg.Destroy;
begin
  //FTargetRequest.Free;
  FCfgDatabaseIntf := nil;
  FXMLDocIntf := nil;
  inherited Destroy;
end;

procedure TsmxBaseCfg.Clear;
begin
end;

function TsmxBaseCfg.GetXMLText: String;
begin
  Result := FormatXMLText(FXMLDocIntf.XML.Text);
end;

{procedure TsmxBaseCfg.Finalize;
begin
  try
    WriteCfg;
    SaveCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgFinalizeError);
  end;
end;}

{procedure TsmxBaseCfg.Initialize;
begin
  try
    //Clear;
    LoadCfg;
    ReadCfg;
  except
    raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  end;
end;}

procedure TsmxBaseCfg.LoadCfg;
var
  //Request: IsmxDataSet;
  Value: Variant;
begin
  {if not(Assigned(FCfgDatabaseIntf) and Assigned(FSelectRequestCfg) and (FCfgID > 0)) then
    raise EsmxCfgError.CreateResFmt(@SCfgLoadError, [ClassName, FCfgID]);
  try
    Request := GetRequest(FSelectRequestCfg);
    try
      if smxDBFuncs.GetValueByKeyDS(Request, FCfgID, Value, FSelectRequestCfg.PerformanceMode) then
      begin
        FXMLDocIntf.XML.Text := Value;
        FXMLDocIntf.Active := True;
      end;
    finally
      Request := nil;
    end;
  except
    raise EsmxCfgError.CreateResFmt(@SCfgLoadError, [ClassName, FCfgID]);
  end;}

  if not Assigned(FSelectRequest) or (FCfgID = 0) then
    raise EsmxCfgError.CreateResFmt(@SCfgLoadError, [ClassName, FCfgID]);
  {if not Assigned(FSelectRequest.CellDataSet) then
    raise EsmxCfgError.CreateResFmt(@SCfgLoadError, [ClassName, FCfgID]);}
  FSelectRequest.Prepare;
  {if smxDBFuncs.GetValueByKeyDS
  (FSelectRequest.CellDataSet,
      FCfgID, Value, FSelectRequest.PerformanceMode) then}
  if smxClassFuncs.GetRequestValueByKey(FSelectRequest, FCfgID, Value) then
  begin
    FXMLDocIntf.XML.Text := Value;
    FXMLDocIntf.Active := True;
  end;
end;

procedure TsmxBaseCfg.ReadCfg;
begin
end;

procedure TsmxBaseCfg.SaveCfg;
var
  //Request: IsmxDataSet;
  Request: TsmxCustomRequest;
  //RequestCfgID: Integer;
  //RequestCfg: TsmxRequestCfg;
  Key: Variant;
begin
  {if not(Assigned(FCfgDatabaseIntf) and Assigned(FSelectRequestCfg)) then
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  if FCfgID = 0 then
    RequestCfgID := FSelectRequestCfg.ModifySetting.InsertCfgID else
    RequestCfgID := FSelectRequestCfg.ModifySetting.UpdateCfgID;
  if RequestCfgID = 0 then
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  try
    RequestCfg := GetRequestCfg(RequestCfgID);
    try
      Request := GetRequest(RequestCfg);
      try
        Key := FCfgID;
        if smxDBFuncs.SetValueByKeyDS(Request, Key, FXMLDocIntf.XML.Text, RequestCfg.PerformanceMode) then
          FCfgID := Key else
          raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
      finally
        Request := nil;
      end;
    finally
      RequestCfg.Free;
    end;
  except
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  end;}

  if not Assigned(FSelectRequest) then
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  if FCfgID = 0 then
    Request := FSelectRequest.ModifyRequest[mrInsert] else
    Request := FSelectRequest.ModifyRequest[mrUpdate];
  if not Assigned(Request) then
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  Request.Prepare;
  Key := FCfgID;
  {if smxDBFuncs.SetValueByKeyDS(Request.CellDataSet, Key,
      FXMLDocIntf.XML.Text, Request.PerformanceMode) then
    FCfgID := Key
  else
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);}
  if smxClassFuncs.SetRequestValueByKey(Request, Key, FXMLDocIntf.XML.Text) then
    FCfgID := Key;
end;

procedure TsmxBaseCfg.SetXMLText(Value: String);
begin
  //try
    FXMLDocIntf.XML.Text := UnFormatXMLText(Value);
    FXMLDocIntf.Active := True;
    //Clear;
    ReadCfg;
  //except
    //raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  //end;
end;

procedure TsmxBaseCfg.WriteCfg;
begin
end;

procedure TsmxBaseCfg.SetCfgDatabase(const Value: IsmxDatabase);
begin
  FCfgDatabaseIntf := Value;
end;

procedure TsmxBaseCfg.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

{procedure TsmxBaseCfg.SetModifySetting(Value: TsmxModifySetting);
begin
  FModifySetting := Value;
end;}

{procedure TsmxBaseCfg.SetSelectRequestCfg(Value: TsmxRequestCfg);
begin
  FSelectRequestCfg := Value;
end;}

{function TsmxBaseCfg.CfgIDToDataSet(ACfgID: Integer): IsmxDataSet;
var
  RequestCfg: TsmxRequestCfg;
begin
  Result := nil;
  if Assigned(FCfgDatabaseIntf) and Assigned(FSelectRequestCfg) and (ACfgID > 0) then
  begin
    RequestCfg := TsmxRequestCfg.Create(nil);
    try
      RequestCfg.CfgDatabase := FCfgDatabaseIntf;
      RequestCfg.CfgID := ACfgID;
      RequestCfg.SelectRequestCfg := FSelectRequestCfg;
      RequestCfg.LoadCfg;
      RequestCfg.ReadCfg;
      Result := FCfgDatabaseIntf.NewDataSet(RequestCfg.DataSetType);
      Result.Database := FCfgDatabaseIntf;
      Result.SQL.Text := RequestCfg.SQLText;
      //Result.Prepare;
    finally
      RequestCfg.Free;
    end;
  end;
end;}

{function TsmxBaseCfg.GetRequest(ARequestCfg: TsmxRequestCfg): IsmxDataSet;
var
  i: Integer;
begin
  Result := FCfgDatabaseIntf.NewDataSet(ARequestCfg.DataSetType);
  Result.Database := FCfgDatabaseIntf;
  Result.SQL.Text := ARequestCfg.SQLText;
  Result.Prepared := True;
  for i := 0 to ARequestCfg.RequestParams.Count - 1 do
    Result.ParamByName(ARequestCfg.RequestParams[i].ParamName).ParamLocation :=
      ARequestCfg.RequestParams[i].ParamLocation;
  for i := 0 to ARequestCfg.RequestFields.Count - 1 do
    Result.FieldByName(ARequestCfg.RequestFields[i].FieldName).FieldSense :=
      ARequestCfg.RequestFields[i].FieldSense;
end;

function TsmxBaseCfg.GetRequestCfg(ACfgID: Integer): TsmxRequestCfg;
begin
  Result := TsmxRequestCfg.Create(nil);
  Result.CfgDatabase := FCfgDatabaseIntf;
  Result.CfgID := ACfgID;
  Result.SelectRequestCfg := FSelectRequestCfg;
  Result.LoadCfg;
  Result.ReadCfg;
end;}

procedure TsmxBaseCfg.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
end;

procedure TsmxBaseCfg.Remove;
var
  Request: TsmxCustomRequest;
  Key: Variant;
begin
  if not Assigned(FSelectRequest) or (FCfgID = 0) then
    raise EsmxCfgError.CreateResFmt(@SCfgRemoveError, [ClassName, FCfgID]);
  Request := FSelectRequest.ModifyRequest[mrDelete];
  if not Assigned(Request) then
    raise EsmxCfgError.CreateResFmt(@SCfgRemoveError, [ClassName, FCfgID]);
  Request.Prepare;
  Key := FCfgID;
  if smxClassFuncs.SetRequestValueByKey(Request, Key, Variants.Null) then
    FCfgID := 0;
end;

procedure TsmxBaseCfg.Receive;
begin
  LoadCfg;
  ReadCfg;
end;

procedure TsmxBaseCfg.Return;
begin
  WriteCfg;
  SaveCfg;
end;

{ TsmxCustomCommonStorage }

function TsmxCustomCommonStorage.FindByName(AParamName: String): Variant;
begin
  Result := Variants.Null;
end;

function TsmxCustomCommonStorage.GetValue(Name: String): Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomCommonStorage.SetValue(Name: String; Value: Variant);
begin
end;

{ TsmxCustomLibraryManager }

function TsmxCustomLibraryManager.GetProcedure(ALibHandle: THandle; AProcName: String): Pointer;
begin
  Result := nil;
end;

function TsmxCustomLibraryManager.GetProcedure(ALibName, AProcName: String): Pointer;
begin
  Result := nil;
end;

function TsmxCustomLibraryManager.CheckLibraryComp(ALibHandle: THandle): Boolean;
begin
  Result := False;
end;

function TsmxCustomLibraryManager.CheckLibraryComp(ALibName: String): Boolean;
begin
  Result := False;
end;

function TsmxCustomLibraryManager.CallLibrary(ALibName: String): THandle;
begin
  Result := 0;
end;

{ TsmxCustomDatabaseManager }

{function TsmxCustomDatabaseManager.FindByName(ADatabaseName: String): IsmxDatabase;
begin
  Result := nil;
end;}

function TsmxCustomDatabaseManager.FindByName(ADatabaseName: String): TsmxDatabaseConnection;
begin
  Result := nil;
end;

procedure TsmxCustomDatabaseManager.InsertDBConnection(AConnection: TsmxDatabaseConnection);
begin
end;

procedure TsmxCustomDatabaseManager.RemoveDBConnection(AConnection: TsmxDatabaseConnection);
begin
end;

procedure TsmxCustomLibraryManager.SetCheckComp(Value: Boolean);
begin
  FCheckComp := Value;
end;

procedure TsmxCustomLibraryManager.SetLibPath(Value: String);
begin
  FLibPath := Value;
end;

procedure TsmxCustomLibraryManager.SetProcLibInfoName(Value: String);
begin
  FProcLibInfoName := Value;
end;

{ TsmxCustomFormManager }

function TsmxCustomFormManager.FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
begin
  Result := nil;
end;

function TsmxCustomFormManager.FindByHandle(AHandle: HWND): TsmxCustomForm;
begin
  Result := nil;
end;

procedure TsmxCustomFormManager.InsertForm(AForm: TsmxCustomForm);
begin
end;

procedure TsmxCustomFormManager.RemoveForm(AForm: TsmxCustomForm);
begin
end;

{ TsmxBaseCell }

constructor TsmxBaseCell.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner);
  //FCfgDatabaseIntf := ADatabase;
  //FCfgID := ACfgID;
  //FCfg := NewCfg(Self, FCfgDatabaseIntf, FCfgID);
  //FCfg.Initialize;
  //FCfg.LoadCfg;
  //FCfg.ReadCfg;
  FCellList := TList.Create;
end;

destructor TsmxBaseCell.Destroy;
begin
  SetParentCell(nil);
  ClearCells;
  FCellList.Free;
  //FCfg.Free;
  FCfgDatabaseIntf := nil;
  inherited Destroy;
end;

{procedure TsmxBaseCell.CreateChilds;
begin
end;

procedure TsmxBaseCell.DestroyChilds;
begin
end;}

procedure TsmxBaseCell.ClearCells;
var
  i: Integer;
begin
  for i := FCellList.Count - 1 downto 0 do
    if Assigned(FCellList[i]) then
      TsmxBaseCell(FCellList[i]).Free;
  FCellList.Clear;
end;

{function TsmxBaseCell.FindCellByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;

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
end;}

function TsmxBaseCell.FindCellByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;
var
  i: Integer;
  List: TList;
begin
  Result := nil;
  if AmongAll then
  begin
    List := TList.Create;
    try
      AllCells(List, []);
      for i := 0 to List.Count - 1 do
        if TsmxBaseCell(List[i]).CfgID = ACfgID then
          Result := TsmxBaseCell(List[i]);
    finally
      List.Free;
    end;
  end else
  begin
    for i := 0 to CellCount - 1 do
      if Cells[i].CfgID = ACfgID then
        Result := Cells[i];
  end;
end;

function TsmxBaseCell.GetCellCount: Integer;
begin
  Result := FCellList.Count;
end;

function TsmxBaseCell.GetCell(Index: Integer): TsmxBaseCell;
begin
  Result := TsmxBaseCell(FCellList[Index]);
end;

function TsmxBaseCell.GetInternalObject: TObject;
begin
  Result := nil;
end;

function TsmxBaseCell.GetRootCell: TsmxBaseCell;
begin
  Result := Self;
  while Assigned(Result.FParentCell) do
    Result := Result.FParentCell;
end;

{procedure TsmxBaseCell.InitChilds;
begin
end;}

procedure TsmxBaseCell.Initialize(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil);
begin
  FCfgDatabaseIntf := ACfgDatabase;
  FCfgID := ACfgID;
  FSelectRequest := ASelectRequest;
end;

{procedure TsmxBaseCell.InstallParent;
begin
end;}

procedure TsmxBaseCell.SetImageList(Value: TCustomImageList);
//var i: Integer;
begin
  {if Assigned(FImageList) then
    for i := 0 to CellCount - 1 do
      Cells[i].ImageList := nil;}
  FImageList := Value;
  {if Assigned(FImageList) then
    for i := 0 to CellCount - 1 do
      Cells[i].ImageList := FImageList;}
end;

{procedure TsmxBaseCell.SetCommonStorage(Value: TsmxCustomCommonStorage);
var i: Integer;
begin
  if Assigned(FCommonStorage) then
    for i := 0 to CellCount - 1 do
      Cells[i].CommonStorage := nil;
  FCommonStorage := Value;
  if Assigned(FCommonStorage) then
    for i := 0 to CellCount - 1 do
      Cells[i].CommonStorage := FCommonStorage;
end;}

{procedure TsmxBaseCell.SetLibraryManager(Value: TsmxCustomLibraryManager);
var i: Integer;
begin
  if Assigned(FLibraryManager) then
    for i := 0 to CellCount - 1 do
      Cells[i].LibraryManager := nil;
  FLibraryManager := Value;
  if Assigned(FLibraryManager) then
    for i := 0 to CellCount - 1 do
      Cells[i].LibraryManager := FLibraryManager;
end;}

procedure TsmxBaseCell.SetParentCell(Value: TsmxBaseCell);
//var
  //Temp: Integer;
begin
  {if Assigned(FParentCell) then
  begin
    Temp := FParentCell.FCellList.IndexOf(Self);
    if Temp > -1 then
      FParentCell.FCellList.Delete(Temp);
  end;
  FParentCell := Value;
  if Assigned(FParentCell) then
  begin
    Temp := FParentCell.FCellList.IndexOf(Self);
    if Temp > -1 then
      FParentCell.FCellList.Add(Self);
  end;}
  if Assigned(FParentCell) then
    FParentCell.FCellList.Remove(Self);
  FParentCell := Value;
  if Assigned(FParentCell) then
    FParentCell.FCellList.Add(Self);
end;

{procedure TsmxBaseCell.UnInitialize;
begin
end;

procedure TsmxBaseCell.UnInstallParent;
var i: Integer;
begin
  for i := CellCount - 1 downto 0 do
    Cells[i].ParentCell := nil;
end;}

function TsmxBaseCell.GetAccessoryForm: TsmxCustomForm;
var
  Cell: TsmxBaseCell;
begin
  Result := nil;
  Cell := Self;
  while Assigned(Cell) and not(Cell is TsmxCustomForm) do
    Cell := Cell.FParentCell;
  if Cell is TsmxCustomForm then
    Result := TsmxCustomForm(Cell);
end;

function TsmxBaseCell.GetImageList: TCustomImageList;
var
  Cell: TsmxBaseCell;
begin
  if not Assigned(FImageList) and Assigned(FParentCell) then
  begin
    Cell := FParentCell;
    while not Assigned(FImageList) and Assigned(Cell) do
    begin
      FImageList := Cell.ImageList;
      Cell := Cell.ParentCell;
    end;
  end;
  Result := FImageList;
end;

{procedure TsmxBaseCell.SetCell(Index: Integer; Value: TsmxBaseCell);
var
  Cell: TsmxBaseCell;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  Cell := FCellList[Index];
  FCellList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(Cell) then
    Cell.Free;
end;

procedure TsmxBaseCell.SetCellCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FCellList.Count then
    for i := FCellList.Count - 1 downto Value - 1 do
      if Assigned(FCellList[i]) then
        TsmxBaseCell(FCellList[i]).Free;
  FCellList.Count := Value;
end;}

procedure TsmxBaseCell.AllCells(AList: TList; AClassList: Array of TsmxComponentClass);

  function IsClass(AClass: TClass): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(AClassList) to High(AClassList) do
      if AClass.InheritsFrom(AClassList[i]) then
        Result := True;
  end;

  procedure AddChilds(ACell: TsmxBaseCell);
  var
    i: Integer;
  begin
    if IsClass(ACell.ClassType) then
      AList.Add(ACell);
    for i := 0 to ACell.CellCount - 1 do
      AddChilds(ACell.Cells[i]);
  end;

var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to CellCount - 1 do
    AddChilds(Cells[i]);
end;

{ TsmxCellCfg }

procedure TsmxCellCfg.LoadCfg;
var
  //Cell: TsmxBaseCell;
  Request: TsmxCustomRequest;
  Value: Variant;
begin
  {FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] :=
    FTargetRequest.DoRequest('select ConfBlob from tConfigs where ConfID = :ConfID');
  if FTargetRequest['ConfBlob'] <> '' then
  begin
    FXMLDocIntf.XML.Text := FTargetRequest['ConfBlob'];
    FXMLDocIntf.Active := True;
  end;}

  //if Assigned(FCfgDatabaseIntf) and (FUpdateSetting.SelectCfgID > 0) and (FCfgID > 0) then
  begin
    //Cell := smxClassFuncs.NewCell(nil, FCfgDatabaseIntf, FUpdateSetting.SelectCfgID);
    Request := TsmxCustomRequest.Create(nil, FCfgDatabaseIntf, FCfgID);
    try
      {if Cell is TsmxCustomRequest then
        if smxClassFuncs.GetRequestValueByKey(TsmxCustomRequest(Cell), FCfgID, Value) then
        begin
          FXMLDocIntf.XML.Text := Value;
          FXMLDocIntf.Active := True;
        end;}
    finally
      //Cell.Free;
      Request.Free;
    end;
  end;
end;

procedure TsmxCellCfg.SaveCfg;
var
  //Request: IsmxDataSet;
  Cell: TsmxBaseCell;
begin
  {FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  //FTargetRequest.DoExecute('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');
  Request := FTargetRequest.NewRequest('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');
  try
    Request.ParamByName('ConfBlob').DataType := ftBlob;
    FTargetRequest.PrepRequest(Request, True, pmExecute);
  finally
    Request := nil;
  end;}

  //if Assigned(FCfgDatabaseIntf) and (FUpdateSetting.UpdateCfgID > 0) and (FCfgID > 0) then
  {begin
    //Cell := smxClassFuncs.NewCell(nil, FCfgDatabaseIntf, FUpdateSetting.UpdateCfgID);
    try
      if Cell is TsmxCustomRequest then
        smxClassFuncs.SetRequestValueByKey(TsmxCustomRequest(Cell), FCfgID, FXMLDocIntf.XML.Text);
    finally
      Cell.Free;
    end;
  end;}
end;

{ TsmxTypeCfg }

procedure TsmxTypeCfg.Clear;
begin
  FCellClassName := '';
  FCellClass := nil;
  FCfgClassName := '';
  FCfgClass := nil;
end;

{procedure TsmxTypeCfg.LoadCfg;
var
  Value: Variant;
begin
  if not Assigned(FSelectRequest) or (TypeCfgID = 0) then
    raise EsmxCfgError.CreateResFmt(@SCfgLoadError, [ClassName, FCfgID]);
  FSelectRequest.Prepare;
  if smxClassFuncs.GetRequestValueByKey(FSelectRequest, TypeCfgID, Value) then
  begin
    FXMLDocIntf.XML.Text := Value;
    FXMLDocIntf.Active := True;
  end;
end;}

procedure TsmxTypeCfg.ReadCfg;
var
  r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    CfgClassName := n.Attributes['CfgClassName'];
    CellClassName := n.Attributes['CellClassName'];
  end;
end;

{procedure TsmxTypeCfg.SaveCfg;
var
  Request: TsmxCustomRequest;
  ForeignKey: Variant;
begin
  if not Assigned(FSelectRequest) then
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  if TypeCfgID = 0 then
    Request := FSelectRequest.ModifyRequest[mrInsert] else
    Request := FSelectRequest.ModifyRequest[mrUpdate];
  if not Assigned(Request) then
    raise EsmxCfgError.CreateResFmt(@SCfgSaveError, [ClassName, FCfgID]);
  Request.Prepare;
  ForeignKey := TypeCfgID;
  if smxClassFuncs.SetRequestValueByKey(Request, ForeignKey, FXMLDocIntf.XML.Text) then
    FTypeCfgID := ForeignKey;
end;}

procedure TsmxTypeCfg.SetCfgClass(Value: TsmxBaseCfgClass);
begin
  if Assigned(FCfgClass) then
    FCfgClassName := '';
  FCfgClass := Value;
  if Assigned(FCfgClass) then
    FCfgClassName := FCfgClass.ClassName;
end;

procedure TsmxTypeCfg.SetCfgClassName(Value: String);
begin
  if FCfgClassName <> '' then
    FCfgClass := nil;
  FCfgClassName := Value;
  if FCfgClassName <> '' then
    FCfgClass := TsmxBaseCfgClass(FindClass(FCfgClassName));
end;

procedure TsmxTypeCfg.SetCellClass(Value: TsmxBaseCellClass);
begin
  if Assigned(FCellClass) then
    FCellClassName := '';
  FCellClass := Value;
  if Assigned(FCellClass) then
    FCellClassName := FCellClass.ClassName;
end;

procedure TsmxTypeCfg.SetCellClassName(Value: String);
begin
  if FCellClassName <> '' then
    FCellClass := nil;
  FCellClassName := Value;
  if FCellClassName <> '' then
    FCellClass := TsmxBaseCellClass(FindClass(FCellClassName));
end;

procedure TsmxTypeCfg.WriteCfg;
var r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := FXMLDocIntf.AddChild('Root');

  n := r.AddChild('Cell');
  n.Attributes['CfgClassName'] := CfgClassName;
  n.Attributes['CellClassName'] := CellClassName;
end;

{function TsmxTypeCfg.GetTypeCfgID: Integer;
var
  ForeignKey: Variant;
begin
  if (FTypeCfgID = 0) and (FCfgID <> 0) and Assigned(FSelectRequest) then
    if smxClassFuncs.GetRequestForeignKeyByKey(FSelectRequest, FCfgID, ForeignKey) then
      FTypeCfgID := ForeignKey;
  Result := FTypeCfgID;
end;}

{procedure TsmxTypeCfg.Remove;
var
  Request: TsmxCustomRequest;
  ForeignKey: Variant;
begin
  if not Assigned(FSelectRequest) or (TypeCfgID = 0) then
    raise EsmxCfgError.CreateResFmt(@SCfgRemoveError, [ClassName, FCfgID]);
  Request := FSelectRequest.ModifyRequest[mrDelete];
  if not Assigned(Request) then
    raise EsmxCfgError.CreateResFmt(@SCfgRemoveError, [ClassName, FCfgID]);
  Request.Prepare;
  ForeignKey := TypeCfgID;
  if smxClassFuncs.SetRequestValueByKey(Request, ForeignKey, Variants.Null) then
    FTypeCfgID := 0;
end;

procedure TsmxTypeCfg.SetCfgID(Value: Integer);
begin
  inherited SetCfgID(Value);
  FTypeCfgID := 0;
end;

procedure TsmxTypeCfg.SetSelectRequest(Value: TsmxCustomRequest);
begin
  inherited SetSelectRequest(Value);
  FTypeCfgID := 0;
end;}

{ TsmxControlCell }

//procedure TsmxControlCell.Apply;
//var i: Integer;
//begin
  {for i := 0 to CellCount - 1 do
    if Cells[i] is TsmxControlCell then
      TsmxControlCell(Cells[i]).Apply;}
//end;

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

//procedure TsmxControlCell.Prepare(Forcibly: Boolean = False);
//var i: Integer;
//begin
  {for i := 0 to CellCount - 1 do
    if Cells[i] is TsmxControlCell then
      TsmxControlCell(Cells[i]).Prepare(Forcibly);} {else
    if Cells[i] is TsmxCustomRequest then
      TsmxCustomRequest(Cells[i]).Prepare(Forcibly);}
//end;

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

procedure TsmxControlCell.SetCellTop(Value: Integer);
begin
end;

procedure TsmxControlCell.SetCellVisible(Value: Boolean);
begin
end;

procedure TsmxControlCell.SetCellWidth(Value: Integer);
begin
end;

{ TsmxTargetRequest }

destructor TsmxTargetRequest.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  if Assigned(FStream) then
    FStream.Free;
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

  if FRequestIntf.SQL.Text <> SQLText then
    FRequestIntf.SQL.Text := SQLText;
  PrepRequest(FRequestIntf, True, pmExecute, DSFrom);
end;

function TsmxTargetRequest.DoRequest(SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Res: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
begin
  with ForRequest(SQLText, RequestType, True, pmOpen, DSFrom) do
  try
    if Res = '' then
      Res := Fields[0].FieldName;
    Result := FieldByName(Res).Value;
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

  if FRequestIntf.SQL.Text <> SQLText then
    FRequestIntf.SQL.Text := SQLText;
  PrepRequest(FRequestIntf, Get, Perform, DSFrom);
  Result := FRequestIntf;
end;

function TsmxTargetRequest.NewRequest(SQLText: String = '';
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  Result := nil;
  if Assigned(Database) then
    Result := Database.NewDataSet(RequestType) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
  Result.Database := Database;
  if SQLText <> '' then
  begin
    Result.SQL.Text := SQLText;
    PrepRequest(Result, False, pmOpen, DSFrom);
  end;
end;

function TsmxTargetRequest.PrepRequest(const ARequest: IsmxDataSet; Get: Boolean = True;
  Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): Boolean;
var
  i: Integer;
  Field: IsmxField;
  Res: Variant;
  Str: String;
begin
  Result := False;
  with ARequest do
  begin
    Close;
    if not Prepared then
      Prepared := True;
    for i := 0 to ParamCount - 1 do
      if Params[i].ParamType in [ptInput, ptInputOutput] then
      begin
        if Assigned(DSFrom) then
        begin
          Field := DSFrom.FindField(Params[i].ParamName);
          if Assigned(Field) then
            Params[i].AssignParam(Field) else
            Params[i].Value := Null;
        end else
        begin
          if Params[i].IsBlob then
          begin
            smxProcs.StrToStream(ParamValues[Params[i].ParamName], Stream);
            Params[i].LoadFromStream(Stream);
          end else
          if ParamValues[Params[i].ParamName] <> '' then
            Params[i].Value := ParamValues[Params[i].ParamName] else
            Params[i].Value := Null;
        end;
      end;
    if Get then
    begin
      case Perform of
        pmOpen: Open;
        pmExecute: Execute;
      end;
      Res := 0;
      for i := 0 to ParamCount - 1 do
        if Params[i].ParamType in [ptOutput, ptInputOutput] then
        begin
          if Params[i].IsBlob then
          begin
            Params[i].SaveToStream(Stream);
            smxProcs.StreamToStr(Stream, Str);
            ParamValues[Params[i].ParamName] := Str;
          end else
          if not Variants.VarIsNull(Params[i].Value) then
            ParamValues[Params[i].ParamName] := Params[i].Value else
            ParamValues[Params[i].ParamName] := '';
        end else
        if Params[i].ParamType = ptResult then
        begin
          ParamValues[Params[i].ParamName] := Params[i].Value;
          Res := Params[i].Value;
        end;

      case Perform of
        pmOpen: Result := RecordCount > 0;
        pmExecute: Result := Res = 0;
      end;
    end;
  end;
end;

procedure TsmxTargetRequest.SetDatabase(const Value: IsmxDatabase);
begin
  if Assigned(FRequestIntf) then
  begin
    FRequestIntf.Close;
    FRequestIntf := nil;
  end;
  FDatabaseIntf := Value;
end;

procedure TsmxTargetRequest.SetValue(Key: String; const Value: String);
begin
  ParamList.Values[AnsiUpperCase(Key)] := Value;
end;

function TsmxTargetRequest.GetStream: TStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;
  Result := FStream;  
end;

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

procedure TsmxKitItem.Assign(AKitItem: TsmxKitItem);
var
  Name: String;
begin
  if Assigned(AKitItem) then
    Name := AKitItem.ClassName else
    Name := 'nil';
  raise EsmxKitError.CreateResFmt(@SKitItemAssignError, [Name, ClassName]);
end;

procedure TsmxKitItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FKit.FList.Move(CurIndex, Value);
end;

procedure TsmxKitItem.SetKit(Value: TsmxKit);
begin
  if Assigned(FKit) then
    FKit.FList.Remove(Self);
  FKit := Value;
  if Assigned(FKit) then
    FKit.FList.Add(Self);
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
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TsmxKitItem(FList[i]).Free;
  FList.Clear;
end;

function TsmxKit.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TsmxKit.GetItem(Index: Integer): TsmxKitItem;
begin
  Result := FList[Index];
end;

{function TsmxKit.Insert(Index: Integer): TsmxKitItem;
begin
  FList.Insert(Index, nil);
  Result := FCellClass.Create(Self);
  FList[Index] := Result;
end;}

{procedure TsmxKit.Remove(AItem: TsmxKitItem);
//var
  //Temp: Integer;
begin
  if Assigned(AItem) then
  begin
    FList.Remove(AItem);
    AItem.FKit := nil;
  end;
  //Temp := FList.Remove(AItem);
  //if Temp >= 0 then
    //AItem.Free;
end;}

procedure TsmxKit.SetItem(Index: Integer; Value: TsmxKitItem);
//var
  //Item: TsmxKitItem;
begin
  {if not Assigned(Value) then
    raise EsmxKitError.CreateResFmt(@SKitItemClassTypeError, ['nil', ClassName]);
  if Value.ClassType <> FCellClass then
    raise EsmxKitError.CreateResFmt(@SKitItemClassTypeError, [Value.ClassName, ClassName]);
  Item := FList[Index];
  FList[Index] := Value;
  Value.FKit := Self;
  if Assigned(Item) then
    Item.Free;}
  TsmxKitItem(FList[Index]).Assign(Value);
end;

procedure TsmxKit.Assign(AKit: TsmxKit);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AKit.Count - 1 do
    Add.Assign(AKit[i]);
end;

{procedure TsmxKit.SetCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FList.Count then
    for i := Value - 1 to Count - 1 do
      TsmxKitItem(FList[i]).Free;
  FList.Count := Value;
end;}

procedure TsmxKit.Delete(AIndex: Integer);
var
  Item: TsmxKitItem;
begin
  Item := FList[AIndex];
  FList.Delete(AIndex);
  if Assigned(Item) then
    Item.Free;
end;

{ TsmxHKitItem }

constructor TsmxHKitItem.Create(AHKit: TsmxHKit);
begin
  inherited Create;
  FHKit := AHKit;
  FList := TList.Create;
end;

destructor TsmxHKitItem.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TsmxHKitItem.Add: TsmxHKitItem;
begin
  Result := FHKit.FCellClass.Create(FHKit);
  Result.FParent := Self;
  FList.Add(Result);
end;

procedure TsmxHKitItem.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TsmxHKitItem(FList[i]).Free;
  FList.Clear;
end;

function TsmxHKitItem.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TsmxHKitItem.GetIndex: Integer;
begin
  if Assigned(FParent) then
    Result := FParent.FList.IndexOf(Self) else
    Result := -1;
end;

function TsmxHKitItem.GetItem(Index: Integer): TsmxHKitItem;
begin
  Result := FList[Index];
end;

function TsmxHKitItem.HasChilds: Boolean;
begin
  Result := FList.Count <> 0;
end;

function TsmxHKitItem.HasParent: Boolean;
begin
  Result := FParent <> FHKit.FRoot;
end;

{procedure TsmxHKitItem.Remove(AItem: TsmxHKitItem);
var
  Temp: Integer;
begin
  if Assigned(AItem) then
  begin
    FList.Remove(AItem);
    AItem.FParent := nil;
  end;
  //Temp := FList.Remove(AItem);
  //if Temp >= 0 then
  //  AItem.Free;
end;}

procedure TsmxHKitItem.SetItem(Index: Integer; Value: TsmxHKitItem);
//var
  //Item: TsmxHKitItem;
begin
  {if not Assigned(Value) then
    raise EsmxKitError.CreateResFmt(@SKitItemClassTypeError, ['nil', ClassName]);
  if Value.ClassType <> FHKit.FCellClass then
    raise EsmxKitError.CreateResFmt(@SKitItemClassTypeError, [Value.ClassName, ClassName]);
  Item := FList[Index];
  FList[Index] := Value;
  Value.FHKit := FHKit;
  if Assigned(Value.FParent) then
    Value.FParent.FList.Remove(Value);
  Value.FParent := Self;
  if Assigned(Item) then
    Item.Free;}
  TsmxHKitItem(FList[Index]).Assign(Value);
end;

{procedure TsmxHKitItem.SetCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FList.Count then
    for i := Value - 1 to FList.Count - 1 do
      TsmxHKitItem(FList[i]).Free;
  FList.Count := Value;
end;}

procedure TsmxHKitItem.Delete(AIndex: Integer);
var
  Item: TsmxHKitItem;
begin
  Item := FList[AIndex];
  FList.Delete(AIndex);
  if Assigned(Item) then
    Item.Free;
end;

procedure TsmxHKitItem.SetParent(Value: TsmxHKitItem);
begin
  {if Assigned(Value) then
    if Value.ClassType <> FHKit.FCellClass then
      raise EsmxKitError.CreateResFmt(@SKitItemClassTypeError, [Value.ClassName, ClassName]);}
  if Assigned(FParent) then
  begin
    FParent.FList.Remove(Self);
    FHKit := nil;
  end;
  FParent := Value;
  if Assigned(FParent) then
  begin
    FParent.FList.Add(Self);
    FHKit := FParent.FHKit;
  end;
end;

{procedure TsmxHKitItem.Insert(AItem: TsmxHKitItem);
begin
  if Assigned(AItem) then
  begin
    FList.Add(AItem);
    AItem.FParent := Self;
  end;
end;}

procedure TsmxHKitItem.Assign(AHKitItem: TsmxHKitItem);
var
  Name: String;
begin
  if Assigned(AHKitItem) then
    Name := AHKitItem.ClassName else
    Name := 'nil';
  raise EsmxKitError.CreateResFmt(@SKitItemAssignError, [Name, ClassName]);
end;

procedure TsmxHKitItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FParent.FList.Move(CurIndex, Value);
end;

{ TsmxHKit }

constructor TsmxHKit.Create(AItemClass: TsmxHKitItemClass);
begin
  inherited Create;
  FCellClass := AItemClass;
  FRoot := FCellClass.Create(Self);
end;

destructor TsmxHKit.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

procedure TsmxHKit.Assign(AHKit: TsmxHKit);

  procedure AssignChild(ADest, ASource: TsmxHKitItem);
  var
    i: Integer;
    Item: TsmxHKitItem;
  begin
    for i := 0 to ASource.Count - 1 do
    begin
      Item := ADest.Add;
      Item.Assign(ASource[i]);
      AssignChild(Item, ASource[i]);
    end;
  end;

var
  i: Integer;
  Item: TsmxHKitItem;
begin
  FRoot.Clear;
  for i := 0 to AHKit.Root.Count - 1 do
  begin
    Item := FRoot.Add;
    Item.Assign(AHKit.Root[i]);
    AssignChild(Item, AHKit.Root[i]);
  end;
end;

{ TsmxParam }

constructor TsmxParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamName := '';
  FParamValue := Variants.Null;
end;

procedure TsmxParam.Assign(AKitItem: TsmxKitItem);
begin
  if AKitItem is TsmxParam then
  begin
    ParamName := TsmxParam(AKitItem).ParamName;
    ParamValue := TsmxParam(AKitItem).ParamValue;
  end else
    inherited Assign(AKitItem);
end;

{ TsmxParams }

function TsmxParams.Add: TsmxParam;
begin
  Result := TsmxParam(inherited Add);
end;

{procedure TsmxParams.Assign(AKit: TsmxKit);
var
  i: Integer;
begin
  if AKit is TsmxParams then
  begin
    Clear;
    for i := 0 to AKit.Count - 1 do
      Add.Assign(AKit[i]);
  end else
    inherited Assign(AKit);
end;}

function TsmxParams.FindByName(AParamName: String): TsmxParam;
var
  i: Integer;
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

function TsmxParams.GetValue(Name: String): Variant;
var
  Param: TsmxParam;
begin
  Param := FindByName(Name);
  //if FIsCheckExists and not Assigned(Param) then
    //raise EsmxKitError.CreateResFmt(@SKitItemNotFound, [ClassName, Name]);
  if Assigned(Param) then
    Result := Param.ParamValue else
    Result := Variants.Null;
    //raise EsmxKitError.CreateRes(@SKitItemNotFound);
end;

function TsmxParams.ParamByName(AParamName: String): TsmxParam;
begin
  Result := FindByName(AParamName);
  if not Assigned(Result) then
    raise EsmxKitError.CreateResFmt(@SKitItemNotFound, [ClassName, AParamName]);
end;

procedure TsmxParams.SetItem(Index: Integer; Value: TsmxParam);
begin
  inherited Items[Index] := Value;
end;

procedure TsmxParams.SetValue(Name: String; Value: Variant);
var
  Param: TsmxParam;
begin
  Param := FindByName(Name);
  //if FIsCheckExists and not Assigned(Param) then
    //raise EsmxKitError.CreateResFmt(@SKitItemNotFound, [ClassName, Name]);
  if Assigned(Param) then
    Param.ParamValue := Value
  else
    with Add do
    begin
      ParamName := Name;
      ParamValue := Value;
    end;
    //raise EsmxKitError.CreateRes(@SKitItemNotFound);
end;

{ TsmxStateUnit }

constructor TsmxStateUnit.Create(AHKit: TsmxHKit);
begin
  inherited Create(AHKit);
  FUnitEnable := False;
  FUnitVisible := False;
end;

function TsmxStateUnit.Add: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Add);
end;

procedure TsmxStateUnit.SwitchIntfID;
var
  StateUnit: TsmxStateUnit;
  IntfID: Integer;
begin
  if FCurrentIntfID = FSourceIntfID then
    IntfID := HKit.IntfID else
    IntfID := FSourceIntfID;
  //if FIntfID <> HKit.IntfID then
  //begin
    StateUnit := Self;

    //StateUnit.CurrentIntfID := IntfID; //HKit.IntfID;
    //while StateUnit.HasParent do
    //begin
      //StateUnit := StateUnit.Parent;
      //StateUnit.CurrentIntfID := IntfID; //HKit.IntfID;
    //end;

    repeat
      StateUnit.CurrentIntfID := IntfID;
      StateUnit := StateUnit.Parent;
    until not StateUnit.HasParent;

  //end;
end;

function TsmxStateUnit.FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxStateUnit;

  function Find(AUnit: TsmxStateUnit): TsmxStateUnit;
  var
    i: Integer;
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

var
  i: Integer;
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
  if FUnitEnable <> Value then
  begin
    FUnitEnable := Value;
    SwitchIntfID;
  end;
end;

procedure TsmxStateUnit.SetUnitVisible(Value: Boolean);
begin
  if FUnitVisible <> Value then
  begin
    FUnitVisible := Value;
    SwitchIntfID;
  end;
end;

procedure TsmxStateUnit.SetSourceIntfID(Value: Integer);
begin
  FSourceIntfID := Value;
  if CurrentIntfID = 0 then
    CurrentIntfID := FSourceIntfID;
end;

procedure TsmxStateUnit.SetCurrentIntfID(Value: Integer);
begin
  FCurrentIntfID := Value;
end;

procedure TsmxStateUnit.SetParent(Value: TsmxStateUnit);
begin
  inherited SetParent(Value);
end;

procedure TsmxStateUnit.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

procedure TsmxStateUnit.SetItem(Index: Integer; Value: TsmxStateUnit);
begin
  inherited Items[Index] := Value;
end;

{ TsmxStateUnits }

function TsmxStateUnits.GetRoot: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Root);
end;

procedure TsmxStateUnits.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

{ TsmxCellState }

constructor TsmxCellState.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
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

function TsmxCellStates.FindByStateID(AStateID: Integer): TsmxCellState;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].StateID = AStateID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxCellStates.GetItem(Index: Integer): TsmxCellState;
begin
  Result := TsmxCellState(inherited Items[Index]);
end;

{ TsmxXMLDocItem }

constructor TsmxXMLDocItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FID := 0;
  FXMLDocIntf := nil;
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

{ TsmxIntfCfg }

{constructor TsmxIntfCfg.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FXMLDocList := TsmxXMLDocItems.Create(TsmxXMLDocItem);
end;

constructor TsmxIntfCfg.CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AIntfID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FIntfID := AIntfID;
  FXMLDocList := TsmxXMLDocItems.Create(TsmxXMLDocItem);
end;}

{constructor TsmxIntfCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FXMLDocList := TsmxXMLDocItems.Create(TsmxXMLDocItem);
  //FStateInterfaces := TsmxInterfaceCfg.Create(Self);
end;}

destructor TsmxIntfCfg.Destroy;
begin
  //FXMLDocList.Free;
  if Assigned(FCellStates) then
    FCellStates.Free;
  //FStateInterfaces.Free;
  //if Assigned(FRequest) then
    //FRequest.Free;
  inherited Destroy;
end;

procedure TsmxIntfCfg.Clear;
begin
  if Assigned(FCellStates) then
    CellStates.Clear;
  {with FStateRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;}
end;

function TsmxIntfCfg.GetCellStates: TsmxCellStates;
begin
  if not Assigned(FCellStates) then
    FCellStates := TsmxCellStates.Create(TsmxCellState);
  Result := FCellStates;
end;

function TsmxIntfCfg.GetXMLText: String;
//var XMLDocItem: TsmxXMLDocItem;
begin
  {XMLDocItem := FXMLDocList.FindByID(IntfID);
  if Assigned(XMLDocItem) then
    Result := FormatXMLText(XMLDocItem.XMLDoc.XML.Text) else
    Result := '';}
end;

procedure TsmxIntfCfg.SetXMLText(Value: String);
//var XMLDocItem: TsmxXMLDocItem;
begin
  {XMLDocItem := FXMLDocList.FindByID(IntfID);
  if not Assigned(XMLDocItem) then
  begin
    XMLDocItem := FXMLDocList.Add;
    XMLDocItem.ID := IntfID;
  end;
  //try
    XMLDocItem.XMLDoc.XML.Text := UnFormatXMLText(Value);
    XMLDocItem.XMLDoc.Active := True;
    //Clear;
    ReadCfg;
  //except
    //raise EsmxCfgError.CreateRes(@SCfgInitializeError);
  //end;}
end;

function TsmxIntfCfg.GetFullXMLText: String;

  {procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('Cell');
    with n do
    begin
      Attributes['CfgID'] := AUnit.CfgID;
      Attributes['Enable'] := AUnit.UnitEnable;
      Attributes['Visible'] := AUnit.UnitVisible;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;}

//var r, n, n2, n3: IXMLNode; i, j: Integer;
begin
  {r := FXMLDocIntf.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := FXMLDocIntf.AddChild('Root');

  n := r.AddChild('States');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('State');
    n2.Attributes['StateID'] := CellStates[i].StateID;
    n3 := n2.AddChild('Cells');
    for j := 0 to CellStates[i].StateUnits.Root.Count - 1 do
      AddNodes(n3, CellStates[i].StateUnits.Root[j]);
  end;
  Result := FormatXMLText(FXMLDocIntf.XML.Text);}
end;

procedure TsmxIntfCfg.LoadCfg;
//var IntfID: Integer; XMLText: String; XMLDocIntf: IXMLDocument;
begin
  {FXMLDocList.Clear;
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
      XMLDocIntf := NewXML; //NewXMLDocument;
      if XMLText <> '' then
      begin
        XMLDocIntf.XML.Text := XMLText;
        XMLDocIntf.Active := True;
      end;
      with FXMLDocList.Add do
      begin
        ID := IntfID;
        XMLDoc := XMLDocIntf;
      end;
      Next;
    end;
  end;}
end;

procedure TsmxIntfCfg.ReadCfg;

  {procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxStateUnit; AIntfID: Integer);
  var i: Integer; u: TsmxStateUnit;
  begin
    u := AUnit.FindByCfgID(ANode.Attributes['StateID']);
    if not Assigned(u) then
      u := AUnit.Add;
    with u do
    begin
      FIntfID := AIntfID;
      FCfgID := ANode.Attributes['CfgID'];
      FUnitEnable := ANode.Attributes['Enable'];
      FUnitVisible := ANode.Attributes['Visible'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'Cell' then
        AddUnits(ANode.ChildNodes[i], u, AIntfID);
  end;}

//var r, n, n2: IXMLNode; i, j, k: Integer; s: TsmxCellState;
begin
  {Clear;
  for k := 0 to FXMLDocList.Count - 1 do
  begin
    r := FXMLDocList[k].XMLDoc.ChildNodes.FindNode('Root');
    if Assigned(r) then
    begin
      n := r.ChildNodes.FindNode('States');
      if Assigned(n) and (n.ChildNodes.Count > 0) then
      begin
        for i := 0 to n.ChildNodes.Count - 1 do
          if n.ChildNodes[i].NodeName = 'State' then
          begin
            s := CellStates.FindByStateID(n.ChildNodes[i].Attributes['StateID']);
            if not Assigned(s) then
            begin
              s := CellStates.Add;
              s.StateUnits.IntfID := FIntfID;
            end;
            with s do
            begin
              StateID := n.ChildNodes[i].Attributes['StateID'];
              n2 := n.ChildNodes[i].ChildNodes.FindNode('Cells');
              if Assigned(n2) and (n2.ChildNodes.Count > 0) then
                for j := 0 to n2.ChildNodes.Count - 1 do
                  if n2.ChildNodes[j].NodeName = 'Cell' then
                    AddUnits(n2.ChildNodes[j], StateUnits.Root, FXMLDocList[k].ID);
            end;
          end;
      end;
    end;
  end;}
end;

procedure TsmxIntfCfg.SaveCfg;
//var XMLDocItem: TsmxXMLDocItem;
begin
  {XMLDocItem := FXMLDocList.FindByID(FIntfID);
  if Assigned(XMLDocItem) then
  begin
    FTargetRequest['IntfID'] := IntToStr(FIntfID);
    FTargetRequest['ConfID'] := IntToStr(FCfgID);
    FTargetRequest['IntfConfBlob'] := XMLDocItem.XMLDoc.XML.Text;
    if VarIsNull(FTargetRequest.DoRequest('select IntfConfID from tIntfsConfs ' +
        'where IntfID = :IntfID and ConfID = :ConfID')) then
      FTargetRequest.DoExecute('insert into tIntfsConfs (IntfID, ConfID, IntfConfBlob) ' +
        'values (:IntfID, :ConfID, :IntfConfBlob)') else
      FTargetRequest.DoExecute('update tIntfsConfs set IntfConfBlob = :IntfConfBlob ' +
        'where IntfID = :IntfID and ConfID = :ConfID');
  //end;}
end;

procedure TsmxIntfCfg.WriteCfg;

  {procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; n: IXMLNode;
  begin
    if AUnit.IntfID = FIntfID then
    begin
      n := ANode.AddChild('Cell');
      with n do
      begin
        Attributes['CfgID'] := AUnit.CfgID;
        Attributes['Enable'] := AUnit.UnitEnable;
        Attributes['Visible'] := AUnit.UnitVisible;
      end;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;}

//var r, n, n2, n3: IXMLNode; i, j: Integer; XMLDocItem: TsmxXMLDocItem;
begin
  {XMLDocItem := FXMLDocList.FindByID(FIntfID);
  if not Assigned(XMLDocItem) then
  begin
    XMLDocItem := FXMLDocList.Add;
    XMLDocItem.ID := FIntfID;
  end;

  r := XMLDocItem.XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDocItem.XMLDoc.AddChild('Root');

  n := r.AddChild('States');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('State');
    n2.Attributes['StateID'] := CellStates[i].StateID;
    n3 := n2.AddChild('Cells');
    for j := 0 to CellStates[i].StateUnits.Root.Count - 1 do
      AddNodes(n3, CellStates[i].StateUnits.Root[j]);
  end;}
end;

procedure TsmxIntfCfg.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

{function TsmxIntfCfg.GetRequest: TsmxCustomRequest;
var
  Cell: TsmxBaseCell;
begin
  if Assigned(FRequest) then
    if FRequest.CfgID <> FStateRequest.CfgID then
    begin
      FRequest.Free;
      FRequest := nil;
    end;
  if not Assigned(FRequest) and Assigned(FCfgDatabaseIntf) and (FStateRequest.CfgID > 0) then
  begin
    Cell := smxClassFuncs.NewCell(Self, FCfgDatabaseIntf, FStateRequest.CfgID, FSelectRequest);
    if Cell is TsmxCustomRequest then
    begin
      FRequest := TsmxCustomRequest(Cell);
      FRequest.Initialize(FCfgDatabaseIntf, FStateRequest.CfgID, FSelectRequest);
    end else
    begin
      Cell.Free;
      raise EsmxCfgError.CreateResFmt(@SCfgBuildError, [ClassName, FCfgID]);
    end;
  end;
  Result := FRequest;
end;}

{procedure TsmxIntfCfg.SetStateRequest(Value: TsmxRequestSetting);
begin
  FStateRequest := Value;
end;}

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
  FUserName := '';
  FPassword := '';
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

procedure TsmxProjectManager.ReadProjects;
var fs: TFileStream; pc: TsmxProjectConnection;
begin
  FProjectList.Clear;
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
          UserName := pc.UserName;
          Password := pc.Password;
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
          UserName := FProjectList[i].UserName;
          Password := FProjectList[i].Password;
        end;
        fs.WriteBuffer(pc, SizeOf(TsmxProjectConnection));
      end;
    finally
      fs.Free;
    end;
  end;
end;

{ TsmxDatabaseConnection }

destructor TsmxDatabaseConnection.Destroy;
begin
  DisconnectFromDatabase;
  SetDatabaseManager(nil);
  inherited Destroy;
end;

function TsmxDatabaseConnection.CreateDatabaseAsFunc: IsmxDatabase;
var
  FuncNewDatabase: TsmxFuncNewDatabase;
begin
  if Assigned(FLibraryManager) then
    @FuncNewDatabase := FLibraryManager.GetProcedure(FLibraryName, FFunctionNameOrProgID) else
    @FuncNewDatabase := nil;
  if Assigned(FuncNewDatabase) then
    Result := FuncNewDatabase
  else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;

function TsmxDatabaseConnection.CreateDatabaseAsCOM: IsmxDatabase;
var
  Comp: Boolean;
begin
  if Assigned(FLibraryManager) then
    Comp := FLibraryManager.CheckLibraryComp(FLibraryName) else
    Comp := False;
  if Comp then
  begin
    RegisterComServer(FLibraryName);
    Result := CreateComObject(ProgIDToClassID(FFunctionNameOrProgID)) as IsmxDatabase;
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;

procedure TsmxDatabaseConnection.ConnectToDatabase;
begin
  DisconnectFromDatabase;
  case FGenerationMode of
    gmFunction: FDatabaseIntf := CreateDatabaseAsFunc;
    gmCOM: FDatabaseIntf := CreateDatabaseAsCOM;
  end;
  with FDatabaseIntf do
  begin
    DatabaseName := FDatabaseName;
    DriverName := FDriverName;
    LoginPrompt := FLoginPrompt;
    Params.Text := AnsiReplaceText(AnsiReplaceText(FParams, '%UserName%', FUserName), '%Password%', FPassword);
    try
      Connected := True;
    except
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
    end;
  end;
  //if Assigned(FDatabaseManager) then
    //FDatabaseManager.InsertDBConnection(Self);
end;

procedure TsmxDatabaseConnection.DisconnectFromDatabase;
begin
  //if Assigned(FDatabaseManager) then
    //FDatabaseManager.RemoveDBConnection(Self);
  if Assigned(FDatabaseIntf) then
  begin
    if FDatabaseIntf.InTransaction then
      FDatabaseIntf.RollbackTransaction;
    FDatabaseIntf.Connected := False;
    FDatabaseIntf := nil;
  end;
end;

procedure TsmxDatabaseConnection.SetDatabaseName(Value: String);
begin
  if (FDatabaseName <> '') and Assigned(FDatabaseManager) then
    FDatabaseManager.RemoveDBConnection(Self);
  FDatabaseName := Value;
  if (FDatabaseName <> '') and Assigned(FDatabaseManager) then
    FDatabaseManager.InsertDBConnection(Self);
end;

procedure TsmxDatabaseConnection.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  FLibraryManager := Value;
end;

procedure TsmxDatabaseConnection.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
begin
  if Assigned(FDatabaseManager) and (FDatabaseName <> '') then
    FDatabaseManager.RemoveDBConnection(Self);
  FDatabaseManager := Value;
  if Assigned(FDatabaseManager) and (FDatabaseName <> '') then
    FDatabaseManager.InsertDBConnection(Self);
end;

{ TsmxCustomRequest }

constructor TsmxCustomRequest.Create(AOwner: TComponent;
  const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FRequestFields := TsmxSenseFields.Create(TsmxSenseField);
  FRequestParams := TsmxLocationParams.Create(TsmxLocationParam);
end;

destructor TsmxCustomRequest.Destroy;
begin
  FRequestParams.Free;
  FRequestFields.Free;
  //FDataSetIntf := nil;
  FDatabaseIntf := nil;
  if Assigned(FInsertRequest) then
    FInsertRequest.Free;
  if Assigned(FUpdateRequest) then
    FUpdateRequest.Free;
  if Assigned(FDeleteRequest) then
    FDeleteRequest.Free;
  //if Assigned(FDataStream) then
    //FDataStream.Free;
  inherited Destroy;
end;

{function TsmxCustomRequest.GetInternalObject: TObject;
begin
  if Assigned(FDataSetIntf) then
    Result := FDataSetIntf.GetDataSet else
    Result := nil;
end;}

{function TsmxCustomRequest.FindBySense(ASense: TsmxFieldSense;
  AFields: TsmxFieldArray): Integer;
var
  Fields: TsmxSenseFieldArray;
  Field: IsmxField;
  i, j: Integer;
begin
  Fields := nil;
  Result := RequestFields.FindBySense(ASense, Fields);
  SetLength(AFields, Result);
  if Result > 0 then
  begin
    j := 0;
    for i := 0 to Result do
    begin
      Field := FindField(Fields[i].FieldName);
      if Assigned(Field) then
      begin
        AFields[j] := Field;
        Inc(j);
      end;
    end;
    if j < Result then
    begin
      SetLength(AFields, j);
      Result := j;
    end;
  end;
end;}

{function TsmxCustomRequest.FindBySense(ASense: TsmxFieldSense;
  AFields: TsmxFieldArray): Integer;
var
  Fields: TsmxSenseFieldArray;
  i: Integer;
begin
  Fields := nil;
  if Assigned(CellDataSet) then
    Result := RequestFields.FindBySense(ASense, Fields) else
    Result := 0;
  SetLength(AFields, Result);
  if Result > 0 then
    for i := 0 to Result - 1 do
      AFields[i] := CellDataSet.FieldByName(Fields[i].FieldName);
end;}

{function TsmxCustomRequest.FindByLocation(ALocation: TsmxParamLocation;
  AParams: TsmxParamArray): Integer;
var
  Params: TsmxLocationParamArray;
  Param: IsmxParam;
  i, j: Integer;
begin
  Params := nil;
  Result := RequestParams.FindByLocation(ALocation, Params);
  SetLength(AParams, Result);
  if Result > 0 then
  begin
    j := 0;
    for i := 0 to Result do
    begin
      Param := FindParam(Params[i].ParamName);
      if Assigned(Param) then
      begin
        AParams[j] := Param;
        Inc(j);
      end;
    end;
    if j < Result then
    begin
      SetLength(AParams, j);
      Result := j;
    end;
  end;
end;}

{function TsmxCustomRequest.FindByLocation(ALocation: TsmxParamLocation;
  AParams: TsmxParamArray): Integer;
var
  Params: TsmxLocationParamArray;
  i: Integer;
begin
  Params := nil;
  if Assigned(CellDataSet) then
    Result := RequestParams.FindByLocation(ALocation, Params) else
    Result := 0;
  SetLength(AParams, Result);
  if Result > 0 then
    for i := 0 to Result - 1 do
      AParams[i] := CellDataSet.ParamByName(Params[i].ParamName);
end;}

procedure TsmxCustomRequest.Perform;
//var
  //i: Integer;
begin
  {if Assigned(CellDataSet) then
  begin
    try
      CellDataSet.Close;
      case PerformanceMode of
        pmOpen: CellDataSet.Open;
        pmExecute: CellDataSet.Execute;
      end;
      //for i := 0 to CellDataSet.FieldCount - 1 do
        //CellDataSet.Fields[i].DisplayFormat :=
          //RequestFields.FieldByName(CellDataSet.Fields[i].FieldName).FieldFormat;
    except
      raise EsmxCellError.CreateResFmt(@SCellRequestPerformError, [ClassName, CfgID]);
    end;
  end;}
end;

procedure TsmxCustomRequest.Prepare;
//var
  //i: Integer;
begin
  {if Assigned(CellDataSet) then
  begin
    CellDataSet.Database := Database;
    CellDataSet.SQL.Text := SQLText;
    if not CellDataSet.Prepared then
      CellDataSet.Prepared := True;
    CellDataSet.ClearParams;
    for i := 0 to RequestParams.Count - 1 do
      with CellDataSet.AddParam do
      begin
        ParamName := RequestParams[i].ParamName;
        DataType := RequestParams[i].DataType;
        ParamType := RequestParams[i].ParamType;
        //Value := RequestParams[i].ParamDefValue;
      end;
    CellDataSet.ClearFields;
    for i := 0 to RequestFields.Count - 1 do
      with CellDataSet.AddField(RequestFields[i].FieldName) do
      begin
        DisplayFormat := RequestFields[i].FieldFormat;
      end;
    if OperationMode = omAutomatic then
    begin
      RefreshParams;
      Perform;
    end;
  end;}
end;

procedure TsmxCustomRequest.RefreshParams;

  {function FindFilterOnForm(AForm: TsmxCustomForm; AName: String;
    var AValue: Variant): Boolean;
  var
    i, j: Integer;
    Page: TsmxCustomPage;
    Filter: TsmxCustomFilter;
  begin
    Result := False;
    AValue := Variants.Null;
    Filter := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
        Page := AForm.PageManagers[i].ActivePage;
        if Assigned(Page) then
          for j := 0 to Page.SectionCount - 1 do
          begin
            if Assigned(Page.Sections[j].FilterDesk) then
              Filter := Page.Sections[j].FilterDesk.FindFilterByName(AName);
            if Assigned(Filter) then
            begin
              AValue := Filter.FilterValue;
              Result := True;
              Exit;
            end;
          end;
      end;
  end;

  function FindFieldOnForm(AForm: TsmxCustomForm; AName: String;
    var AValue: Variant): Boolean;
  var
    i, j: Integer;
    Page: TsmxCustomPage;
    Field: TsmxSenseField;
  begin
    Result := False;
    AValue := Variants.Null;
    Field := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
        Page := AForm.PageManagers[i].ActivePage;
        if Assigned(Page) then
          for j := 0 to Page.SectionCount - 1 do
          begin
            if Assigned(Page.Sections[j].Request) then
              Field := Page.Sections[j].Request.RequestFields.FindByName(AName);
            if Assigned(Field) then
            begin
              AValue := Page.Sections[j].Request.FieldValue[AName];
              Result := True;
              Exit;
            end;
          end;
      end;
  end;}

//var
  //i, j: integer;
  //Form, PForm: TsmxCustomForm;

  //Filter: TsmxCustomFilter;
  //Field: TsmxSenseField;

  //Value: Variant;
  //Res: Boolean;
begin
  {if not Assigned(CellDataSet) then
    Exit;
  Form := AccessoryForm;
  for i := 0 to RequestParams.Count - 1 do
    //with RequestParams[i] do
    begin
      Value := Variants.Null;
      case RequestParams[i].ParamLocation of
        plConst,
        plKey,
        plValue,
        plResult,
        plMessage,
        plForeignKey,
        plInput,
        plOutput:
        begin
          Value := RequestParams[i].ParamDefValue; //RequestParams.ParamByName(RequestParams[i].ParamName).ParamDefValue;
        end;
        plFilterDesk:
        begin
          //Filter := nil;
          if ParentCell is TsmxCustomSection then
            with TsmxCustomSection(ParentCell) do
              if Assigned(FilterDesk) then
                if Assigned(FilterDesk.FindFilterByName(RequestParams[i].ParamName)) then
                  Value := FilterDesk.FindFilterByName(RequestParams[i].ParamName).FilterValue;
          //if Assigned(Filter) then
            //Value := Filter.FilterValue;
        end;
        plGrid:
        begin
          //Field := nil;
          if ParentCell is TsmxCustomSection then
            if ParentCell.ParentCell is TsmxCustomPage then
              with TsmxCustomPage(ParentCell.ParentCell) do
                for j := 0 to SectionCount - 1 do
                  if Sections[j] <> ParentCell then
                    if Assigned(Sections[j].Request) then
                      if Assigned(Sections[j].Request.RequestFields.FindByName(RequestParams[i].ParamName)) then
                    //begin
                      //Field := Sections[j].Request.RequestFields.FindByName(RequestParams[i].ParamName);
                      //if Assigned(Field) then
                        begin
                          Value := Sections[j].Request.FieldValue[RequestParams[i].ParamName];
                          Break;
                        end;
                    //end;
          //if Assigned(Field) then
            //Value := FieldValue[Field.FieldName];
            //Value := Field.Value;
        end;
        plParentFilterDesk:
        begin
          //Filter := nil;
          Res := False;
          if Assigned(Form) then
            PForm := Form.ParentForm else
            PForm := nil;
          while Assigned(PForm) and not Res do
          begin
            Res := FindFilterOnForm(PForm, RequestParams[i].ParamName, Value);
            PForm := PForm.ParentForm;
          end;
          //if Assigned(Filter) then
            //Value := Filter.FilterValue;
        end;
        plParentGrid:
        begin
          //Field := nil;
          Res := False;
          if Assigned(Form) then
            PForm := Form.ParentForm else
            PForm := nil;
          while Assigned(PForm) and not Res do
          begin
            Res := FindFieldOnForm(PForm, RequestParams[i].ParamName, Value);
            PForm := PForm.ParentForm;
          end;
          //if Assigned(Field) then
            //Value := FieldValue[Field.FieldName];
            //Value := Field.Value;
        end;
        plCommonParams:
        begin
          if Assigned(CommonStorage) then
            Value := CommonStorage[RequestParams[i].ParamName];
        end;
        plFormID:
        begin
          if Assigned(Form) then
            Value := Form.ID;
        end;
      end;
      CellDataSet.ParamByName(RequestParams[i].ParamName).Value := Value;
    end;}
end;

procedure TsmxCustomRequest.SetDatabase(const Value: IsmxDatabase);
begin
  if FDatabaseIntf <> Value then
  begin
    FDatabaseIntf := Value;
    //FDataSetIntf := nil;
    SetPrepared(False);
  end;

  {if Assigned(FDatabaseIntf) and not(FDataSetType = dstUnknown) then
    FDataSetIntf := nil;
  FDatabaseIntf := Value;
  if Assigned(FDatabaseIntf) and not(FDataSetType = dstUnknown) then
    FDataSetIntf := FDatabaseIntf.NewDataSet(FDataSetType);}
end;

procedure TsmxCustomRequest.SetDatabaseName(Value: String);
//var
  //DatabaseConnection: TsmxDataBaseConnection;
begin
  {if (FDatabaseName <> '') and Assigned(FDatabaseManager) then
    SetDatabase(nil);}
  if FDatabaseName <> Value then
  begin
    FDatabaseName := Value;
    SetDatabase(nil);
  end;
  {if (FDatabaseName <> '') and Assigned(FDatabaseManager) then
  begin
    DatabaseConnection := FDatabaseManager.FindByName(FDatabaseName);
    if Assigned(DatabaseConnection) then
      SetDatabase(DatabaseConnection.Database);
  end;}
end;

procedure TsmxCustomRequest.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
//var
  //DatabaseConnection: TsmxDataBaseConnection;
begin
  {if Assigned(FDatabaseManager) and (FDatabaseName <> '') then
    SetDatabase(nil);}
  if FDatabaseManager <> Value then
  begin
    FDatabaseManager := Value;
    SetDatabase(nil);
  end;
  {if Assigned(FDatabaseManager) and (FDatabaseName <> '') then
  begin
    DatabaseConnection := FDatabaseManager.FindByName(FDatabaseName);
    if Assigned(DatabaseConnection) then
      SetDatabase(DatabaseConnection.Database);
  end;}
end;

function TsmxCustomRequest.GetDatabase: IsmxDatabase;
var
  DatabaseConnection: TsmxDatabaseConnection;
begin
  if not Assigned(FDatabaseIntf) then
  begin
    if Assigned(DatabaseManager) and (FDatabaseName <> '') then
    begin
      DatabaseConnection := DatabaseManager.FindByName(FDatabaseName);
      if Assigned(DatabaseConnection) then
        FDatabaseIntf := DatabaseConnection.Database;
    end else
      FDatabaseIntf := FCfgDatabaseIntf;
  end;
  Result := FDatabaseIntf;
end;

{function TsmxCustomRequest.GetDataSet: IsmxDataSet;
begin
  if not Assigned(FDataSetIntf) and Assigned(Database) and (FDataSetType <> dstUnknown) then
    FDataSetIntf := Database.NewDataSet(FDataSetType);
  Result := FDataSetIntf;
end;}

procedure TsmxCustomRequest.SetPerformanceMode(Value: TsmxPerformanceMode);
begin
  FPerformanceMode := Value;
end;

procedure TsmxCustomRequest.SetRequestModify(Value: TsmxModifySetting);
begin
  if (FRequestModify.InsertCfgID <> Value.InsertCfgID) and Assigned(FInsertRequest) then
  begin
    FInsertRequest.Free;
    FInsertRequest := nil;
  end;
  if (FRequestModify.UpdateCfgID <> Value.UpdateCfgID) and Assigned(FUpdateRequest) then
  begin
    FUpdateRequest.Free;
    FUpdateRequest := nil;
  end;
  if (FRequestModify.DeleteCfgID <> Value.DeleteCfgID) and Assigned(FDeleteRequest) then
  begin
    FDeleteRequest.Free;
    FDeleteRequest := nil;
  end;
  FRequestModify := Value;
end;

procedure TsmxCustomRequest.SetSQLText(Value: String);
begin
  if FSQLText <> Value then
  begin
    FSQLText := Value;
    SetPrepared(False);
    {if Assigned(FDataSetIntf) then
      FDataSetIntf.Prepared := False;}
  end;
end;

procedure TsmxCustomRequest.SetDataSetType(Value: TsmxDataSetType);
begin
  //if FDataSetType <> Value then
  //begin
    FDataSetType := Value;
    //FDataSetIntf := nil;
  //end;
  {if not(FDataSetType = dstUnknown) and Assigned(FDatabaseIntf) then
    FDataSetIntf := nil;
  FDataSetType := Value;
  if not(FDataSetType = dstUnknown) and Assigned(FDatabaseIntf) then
    FDataSetIntf := FDatabaseIntf.NewDataSet(FDataSetType);}
end;

{function TsmxCustomRequest.FindField(AFieldName: String): IsmxField;
begin
  if Assigned(FDataSetIntf) then
    Result := FDataSetIntf.FindField(AFieldName) else
    Result := nil;
end;

function TsmxCustomRequest.FindParam(AParamName: String): IsmxParam;
begin
  if Assigned(FDataSetIntf) then
    Result := FDataSetIntf.FindParam(AParamName) else
    Result := nil;
end;}

procedure TsmxCustomRequest.SetOperationMode(Value: TsmxOperationMode);
begin
  FOperationMode := Value;
end;

{function TsmxCustomRequest.GetRequestFields: TsmxSenseFields;
begin
  if not Assigned(FRequestFields) then
    FRequestFields := TsmxSenseFields.Create(TsmxSenseField);
  Result := FRequestFields;
end;

function TsmxCustomRequest.GetRequestParams: TsmxLocationParams;
begin
  if not Assigned(FRequestParams) then
    FRequestParams := TsmxLocationParams.Create(TsmxLocationParam);
  Result := FRequestParams;
end;}

procedure TsmxCustomRequest.SetRequestFields(Value: TsmxSenseFields);
begin
  FRequestFields.Assign(Value);
end;

procedure TsmxCustomRequest.SetRequestParams(Value: TsmxLocationParams);
begin
  FRequestParams.Assign(Value);
end;

function TsmxCustomRequest.GetModifyRequest(Modify: TsmxModifyRequest): TsmxCustomRequest;
begin
  Result := nil;
  case Modify of
    mrInsert:
    begin
      if not Assigned(FInsertRequest) and (FRequestModify.InsertCfgID > 0) then
      begin
        FInsertRequest := TsmxCustomRequest(TsmxBaseCellClass(ClassType).Create(Self,
          FCfgDatabaseIntf, FRequestModify.InsertCfgID));
        FInsertRequest.Initialize(FCfgDatabaseIntf, FRequestModify.InsertCfgID, FSelectRequest);
      end;
      Result := FInsertRequest;
    end;
    mrUpdate:
    begin
      if not Assigned(FUpdateRequest) and (FRequestModify.UpdateCfgID > 0) then
      begin
        FUpdateRequest := TsmxCustomRequest(TsmxBaseCellClass(ClassType).Create(Self,
          FCfgDatabaseIntf, FRequestModify.UpdateCfgID));
        FUpdateRequest.Initialize(FCfgDatabaseIntf, FRequestModify.UpdateCfgID, FSelectRequest);
      end;
      Result := FUpdateRequest;
    end;
    mrDelete:
    begin
      if not Assigned(FDeleteRequest) and (FRequestModify.DeleteCfgID > 0) then
      begin
        FDeleteRequest := TsmxCustomRequest(TsmxBaseCellClass(ClassType).Create(Self,
          FCfgDatabaseIntf, FRequestModify.DeleteCfgID));
        FDeleteRequest.Initialize(FCfgDatabaseIntf, FRequestModify.DeleteCfgID, FSelectRequest);
      end;
      Result := FDeleteRequest;
    end;
  end;
end;

{function TsmxCustomRequest.GetDataStream: TStream;
begin
  if not Assigned(FDataStream) then
    FDataStream := TMemoryStream.Create;
  Result := FDataStream;
end;}

procedure TsmxCustomRequest.SetCommonStorage(Value: TsmxCustomCommonStorage);
begin
  FCommonStorage := Value;
end;

function TsmxCustomRequest.GetDatabaseManager: TsmxCustomDatabaseManager;
begin
  if not Assigned(FDatabaseManager) and Assigned(AccessoryForm) then
    FDatabaseManager := AccessoryForm.DatabaseManager;
  Result := FDatabaseManager;
end;

function TsmxCustomRequest.GetCommonStorage: TsmxCustomCommonStorage;
begin
  if not Assigned(FCommonStorage) and Assigned(AccessoryForm) then
    FCommonStorage := AccessoryForm.CommonStorage;
  Result := FCommonStorage;
end;

function TsmxCustomRequest.GetFieldValue(Name: String): Variant;
//var
  //Field: IsmxField;
  //Str: String;
begin
  //Result := Variants.Null;
  //if Assigned(CellDataSet) then
    //Result := CellDataSet.FieldByName(Name) else
    Result := Variants.Null;
  {begin
    Field := CellDataSet.FieldByName(Name);
    if Field.IsBlob then
    begin
      Field.SaveToStream(DataStream);
      smxProcs.StreamToStr(DataStream, Str);
      Result := Str;
    end else
      Result := Field.Value;
  end;}
end;

function TsmxCustomRequest.GetParamValue(Name: String): Variant;
//var
  //Param: IsmxParam;
  //Str: String;
begin
  //Result := Variants.Null;
  //if Assigned(CellDataSet) then
    //Result := CellDataSet.ParamByName(Name).Value else
    Result := Variants.Null;
  {begin
    Param := CellDataSet.ParamByName(Name);
    if Param.IsBlob then
    begin
      Param.SaveToStream(DataStream);
      smxProcs.StreamToStr(DataStream, Str);
      Result := Str;
    end else
      Result := Param.Value;
  end;}
end;

procedure TsmxCustomRequest.SetFieldValue(Name: String; Value: Variant);
//var
  //Field: IsmxField;
begin
  //if Assigned(CellDataSet) then
    //CellDataSet.FieldByName(Name).Value := Value;
  {begin
    Field := CellDataSet.FieldByName(Name);
    if Field.IsBlob then
    begin
      smxProcs.StrToStream(Value, DataStream);
      Field.LoadFromStream(DataStream);
    end else
      Field.Value := Value;
  end;}
end;

procedure TsmxCustomRequest.SetParamValue(Name: String; Value: Variant);
//var
  //Param: IsmxParam;
begin
  //if Assigned(CellDataSet) then
    //CellDataSet.ParamByName(Name).Value := Value;
  {begin
    Param := CellDataSet.ParamByName(Name);
    if Param.IsBlob then
    begin
      smxProcs.StrToStream(Value, DataStream);
      Param.LoadFromStream(DataStream);
    end else
      Param.Value := Value;
  end;}
end;

procedure TsmxCustomRequest.First;
begin
end;

function TsmxCustomRequest.GetBof: Boolean;
begin
 Result := False;
end;

function TsmxCustomRequest.GetEof: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomRequest.Last;
begin
end;

procedure TsmxCustomRequest.Next;
begin
end;

procedure TsmxCustomRequest.Prior;
begin
end;

procedure TsmxCustomRequest.SetPrepared(Value: Boolean);
begin
  if FPrepared <> Value then
  begin
    FPrepared := Value;
    if FPrepared then
      if FOperationMode = omAutomatic then
      begin
        RefreshParams;
        Perform;
      end;
  end;
end;

function TsmxCustomRequest.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

{ TsmxCustomGrid }

constructor TsmxCustomGrid.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FColumnList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomGrid.Destroy;
begin
  //DestroyChilds;
  ClearColumns;
  FColumnList.Free;
  inherited Destroy;
end;

{procedure TsmxCustomGrid.DestroyChilds;
var i: Integer;
begin
  for i := FColumnList.Count - 1 downto 0 do
  begin
    TsmxCustomColumn(FColumnList[i]).Free;
    FColumnList.Delete(i);
  end;
end;}

function TsmxCustomGrid.GetColumn(Index: Integer): TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(FColumnList[Index]);
end;

function TsmxCustomGrid.GetColumnCount: Integer;
begin
  Result := FColumnList.Count;
end;

{procedure TsmxCustomGrid.InstallParent;
var i: Integer;
begin
  for i := 0 to ColumnCount - 1 do
    Columns[i].ParentCell := Self;
end;}

procedure TsmxCustomGrid.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
end;

{procedure TsmxCustomGrid.UnInstallParent;
var i: Integer;
begin
  for i := 0 to ColumnCount - 1 do
    Columns[i].ParentCell := nil;
end;}

procedure TsmxCustomGrid.SetColumn(Index: Integer; Value: TsmxCustomColumn);
var
  Column: TsmxCustomColumn;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  Column := FColumnList[Index];
  FColumnList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(Column) then
    Column.Free;
end;

procedure TsmxCustomGrid.ClearColumns;
var
  i: Integer;
begin
  for i := FColumnList.Count - 1 downto 0 do
    if Assigned(FColumnList[i]) then
      TsmxCustomColumn(FColumnList[i]).Free;
  FColumnList.Clear;
end;

procedure TsmxCustomGrid.SetColumnCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FColumnList.Count then
    for i := Value - 1 to FColumnList.Count - 1 do
      if Assigned(FColumnList[i]) then
        TsmxCustomColumn(FColumnList[i]).Free;
  FColumnList.Count := Value;
end;

{ TsmxCustomAlgorithm }

constructor TsmxCustomAlgorithm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //AddParams;
  FAlgorithmParams := TsmxLocationParams.Create(TsmxLocationParam);
  //FParams := TsmxParams.Create(TsmxParam);
end;

destructor TsmxCustomAlgorithm.Destroy;
begin
  //if Assigned(FParams) then
  //FParams.Free;
  FAlgorithmParams.Free;
  inherited Destroy;
end;

{procedure TsmxCustomAlgorithm.AddParams;
begin
end;}

procedure TsmxCustomAlgorithm.Execute;
begin
end;

{function TsmxCustomAlgorithm.FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam;
begin
  Result := nil;
end;}

{function TsmxCustomAlgorithm.GetParams: TsmxParams;
begin
  if not Assigned(FParams) then
    FParams := TsmxParams.Create(TsmxParam);
  Result := FParams;
end;}

function TsmxCustomAlgorithm.GetAlgorithmCaption: String;
begin
  Result := '';
end;

function TsmxCustomAlgorithm.GetAlgorithmEnable: Boolean;
begin
  Result := False;
end;

function TsmxCustomAlgorithm.GetAlgorithmHotKey: Integer;
begin
  Result := 0;
end;

function TsmxCustomAlgorithm.GetAlgorithmImageIndex: Integer;
begin
  Result := -1;
end;

function TsmxCustomAlgorithm.GetAlgorithmVisible: Boolean;
begin
  Result := False;
end;

function TsmxCustomAlgorithm.GetParamValue(Name: String): Variant;
begin
  //Result := Params.ParamByName(Name).ParamValue;
  Result := Variants.Null;
end;

procedure TsmxCustomAlgorithm.SetParamValue(Name: String; Value: Variant);
begin
  //Params.ParamByName(Name).ParamValue := Value;
end;

procedure TsmxCustomAlgorithm.RefreshParams;

  {function FindFilterOnForm(AForm: TsmxCustomForm; AName: String;
    var AValue: Variant): Boolean;
  var
    i, j: Integer;
    Page: TsmxCustomPage;
    Filter: TsmxCustomFilter;
  begin
    Result := False;
    AValue := Variants.Null;
    Filter := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
        Page := AForm.PageManagers[i].ActivePage;
        if Assigned(Page) then
          for j := 0 to Page.SectionCount - 1 do
          begin
            if Assigned(Page.Sections[j].FilterDesk) then
              Filter := Page.Sections[j].FilterDesk.FindFilterByName(AName);
            if Assigned(Filter) then
            begin
              AValue := Filter.FilterValue;
              Result := True;
              Exit;
            end;
          end;
      end;
  end;

  function FindFieldOnForm(AForm: TsmxCustomForm; AName: String;
    var AValue: Variant): Boolean;
  var
    i, j: Integer;
    Page: TsmxCustomPage;
    Field: TsmxSenseField;
  begin
    Result := False;
    AValue := Variants.Null;
    Field := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
        Page := AForm.PageManagers[i].ActivePage;
        if Assigned(Page) then
          for j := 0 to Page.SectionCount - 1 do
          begin
            if Assigned(Page.Sections[j].Request) then
              Field := Page.Sections[j].Request.RequestFields.FindByName(AName);
            if Assigned(Field) then
            begin
              AValue := Page.Sections[j].Request.FieldValue[AName];
              Result := True;
              Exit;
            end;
          end;
      end;
  end;}

//var
  //i: integer;
  //Value: Variant;
  //Form, PForm: TsmxCustomForm;

  //Filter: TsmxCustomFilter;
  //Field: TsmxSenseField;

  //Res: Boolean;
begin
  {Form := AccessoryForm;
  for i := 0 to AlgorithmParams.Count - 1 do
    //with AlgorithmParams[i] do
    begin
      Value := Variants.Null;
      case AlgorithmParams[i].ParamLocation of
        plConst,
        plKey,
        plValue,
        plResult,
        plMessage,
        plForeignKey,
        plInput,
        plOutput:
        begin
          Value := AlgorithmParams[i].ParamDefValue; //AlgorithmParams.ParamByName(AlgorithmParams[i].ParamName).ParamDefValue;
        end;
        plFilterDesk:
        begin

          if Assigned(Form) then
            FindFilterOnForm(Form, AlgorithmParams[i].ParamName, Value);
        end;
        plGrid:
        begin
            //Value := Field.Value;
          if Assigned(Form) then
            FindFieldOnForm(Form, AlgorithmParams[i].ParamName, Value);
        end;
        plParentFilterDesk:
        begin
          //Filter := nil;
          Res := False;
          if Assigned(Form) then
            PForm := Form.ParentForm else
            PForm := nil;
          while Assigned(PForm) and not Res do
          begin
            Res := FindFilterOnForm(PForm, AlgorithmParams[i].ParamName, Value);
            PForm := PForm.ParentForm;
          end;
          //if Assigned(Filter) then
            //Value := Filter.FilterValue;
        end;
        plParentGrid:
        begin
          //Field := nil;
          Res := False;
          if Assigned(Form) then
            PForm := Form.ParentForm else
            PForm := nil;
          while Assigned(PForm) and not Res do
          begin
            Res := FindFieldOnForm(PForm, AlgorithmParams[i].ParamName, Value);
            PForm := PForm.ParentForm;
          end;
          //if Assigned(Field) then
            //Value := FieldValue[Field.FieldName];
            //Value := Field.Value;
        end;
        //plParentParams: v := Null;
        plCommonParams:
        begin
          //v := CommonStorage.ParamValues[ParamName];
          //v := FindCommonParamByNameLib(ParamName);
          if Assigned(CommonStorage) then
            Value := CommonStorage[AlgorithmParams[i].ParamName];
        end;
        //plParentCfgID: v := Null;

        plFormID:
        begin
          if Assigned(Form) then
            Value := Form.ID;
        end;
      end;

      Params.ParamByName(AlgorithmParams[i].ParamName).ParamValue := Value;


    end;}
end;

procedure TsmxCustomAlgorithm.SetAlgorithmParams(Value: TsmxLocationParams);
begin
  FAlgorithmParams.Assign(Value);
end;

procedure TsmxCustomAlgorithm.SetAlgorithmCaption(Value: String);
begin
end;

procedure TsmxCustomAlgorithm.SetAlgorithmEnable(Value: Boolean);
begin
end;

procedure TsmxCustomAlgorithm.SetAlgorithmHotKey(Value: Integer);
begin
end;

procedure TsmxCustomAlgorithm.SetAlgorithmImageIndex(Value: Integer);
begin
end;

procedure TsmxCustomAlgorithm.SetAlgorithmVisible(Value: Boolean);
begin
end;

function TsmxCustomAlgorithm.GetCommonStorage: TsmxCustomCommonStorage;
begin
  if not Assigned(FCommonStorage) and Assigned(AccessoryForm) then
    FCommonStorage := AccessoryForm.CommonStorage;
  Result := FCommonStorage;
end;

{ TsmxCustomLibAlgorithm }

{procedure TsmxCustomLibAlgorithm.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  FLibManager := Value;
end;}

function TsmxCustomAlgorithm.GetLibraryManager: TsmxCustomLibraryManager;
begin
  if not Assigned(FLibraryManager) and Assigned(AccessoryForm) then
    FLibraryManager := AccessoryForm.LibraryManager;
  Result := FLibraryManager;
end;

procedure TsmxCustomAlgorithm.SetCommonStorage(Value: TsmxCustomCommonStorage);
begin
  FCommonStorage := Value;
end;

procedure TsmxCustomAlgorithm.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  FLibraryManager := Value;
end;

function TsmxCustomAlgorithm.GetAlgorithmHint: String;
begin
  Result := '';
end;

procedure TsmxCustomAlgorithm.SetAlgorithmHint(Value: String);
begin
end;

{ TsmxCustomAlgorithmList }

constructor TsmxCustomAlgorithmList.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FAlgorithmList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomAlgorithmList.Destroy;
begin
  //DestroyChilds;
  ClearAlgorithms;
  FAlgorithmList.Free;
  inherited Destroy;
end;

procedure TsmxCustomAlgorithmList.ClearAlgorithms;
var
  i: Integer;
begin
  for i := 0 to FAlgorithmList.Count - 1 do
    if Assigned(FAlgorithmList[i]) then
      TsmxCustomAlgorithm(FAlgorithmList[i]).Free;
  FAlgorithmList.Clear;
end;

{procedure TsmxCustomAlgorithmList.DestroyChilds;
var i: Integer;
begin
  for i := FAlgorithmList.Count - 1 downto 0 do
  begin
    TsmxCustomAlgorithm(FAlgorithmList[i]).Free;
    FAlgorithmList.Delete(i);
  end;
end;}

{function TsmxCustomAlgorithmList.FindAlgorithmByCfgID(ACfgID: Integer): TsmxCustomAlgorithm;
var i: Integer;
begin
  Result := nil;
  for i := 0 to AlgorithmCount - 1 do
    if Algorithms[i].CfgID = ACfgID then
    begin
      Result := Algorithms[i];
      Break;
    end;
end;}

function TsmxCustomAlgorithmList.GetAlgorithm(Index: Integer): TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(FAlgorithmList[Index]);
end;

procedure TsmxCustomAlgorithmList.SetAlgorithm(Index: Integer; Value: TsmxCustomAlgorithm);
var
  Algorithm: TsmxCustomAlgorithm;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  Algorithm := FAlgorithmList[Index];
  FAlgorithmList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(Algorithm) then
    Algorithm.Free;
end;

function TsmxCustomAlgorithmList.GetAlgorithmCount: Integer;
begin
  Result := FAlgorithmList.Count;
end;

procedure TsmxCustomAlgorithmList.SetAlgorithmCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FAlgorithmList.Count then
    for i := Value - 1 to FAlgorithmList.Count - 1 do
      if Assigned(FAlgorithmList[i]) then
        TsmxCustomAlgorithm(FAlgorithmList[i]).Free;
  FAlgorithmList.Count := Value;
end;

{procedure TsmxCustomAlgorithmList.InstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    Algorithms[i].ParentCell := Self;
end;}

{procedure TsmxCustomAlgorithmList.UnInstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    Algorithms[i].ParentCell := nil;
end;}

{procedure TsmxCustomAlgorithmList.AddAlgorithmsTo(ACell: TsmxControlCell);
var i: Integer; mp: TsmxCustomMenuPoint; tb: TsmxCustomToolBoard;
begin
  if ACell is TsmxCustomMainMenu and FIsCreateMenuPoint then
  begin
    for i := 0 to AlgorithmCount - 1 do
    begin
//      mp := TsmxCustomMainMenu(ACell).FindMenuPointByCfgID(Algorithms[i].MenuPointID);
      if Assigned(mp) then
        mp.AddAlgorithm(Algorithms[i]);
    end;
  end else
  if ACell is TsmxCustomControlBoard and FIsCreateToolButton then
  begin
    for i := AlgorithmCount - 1 downto 0 do
    begin
      //tb := TsmxCustomControlBoard(ACell).FindToolBoardByCfgID(Algorithms[i].ToolBoardID);
      if Assigned(tb) then
        tb.AddAlgorithm(Algorithms[i]);
    end;
  end;
end;

procedure TsmxCustomAlgorithmList.DelAlgorithmsTo(ACell: TsmxControlCell);
var i: Integer; mp: TsmxCustomMenuPoint; tb: TsmxCustomToolBoard;
begin
  if ACell is TsmxCustomMainMenu then
  begin
    for i := AlgorithmCount - 1 downto 0 do
    begin
      //mp := TsmxCustomMainMenu(ACell).FindMenuPointByCfgID(Algorithms[i].MenuPointID);
      if Assigned(mp) then
        mp.DelAlgorithm(Algorithms[i]);
    end;
  end else
  if ACell is TsmxCustomControlBoard then
  begin
    for i := AlgorithmCount - 1 downto 0 do
    begin
      //tb := TsmxCustomControlBoard(ACell).FindToolBoardByCfgID(Algorithms[i].ToolBoardID);
      if Assigned(tb) then
        tb.DelAlgorithm(Algorithms[i]);
    end;
  end;
end;}

{procedure TsmxCustomAlgorithmList.SetMasterMenu(Value: TsmxCustomMainMenu);
begin
  if Assigned(FMasterMenu) and FIsCreateMenuPoint then
    DelAlgorithmsTo(FMasterMenu);
  FMasterMenu := Value;
  if Assigned(FMasterMenu) and FIsCreateMenuPoint then
    AddAlgorithmsTo(FMasterMenu);
end;

procedure TsmxCustomAlgorithmList.SetControlBoard(Value: TsmxCustomControlBoard);
begin
  if Assigned(FControlBoard) and FIsCreateToolButton then
    DelAlgorithmsTo(FControlBoard);
  FControlBoard := Value;
  if Assigned(FControlBoard) and FIsCreateToolButton then
    AddAlgorithmsTo(FControlBoard);
end;

procedure TsmxCustomAlgorithmList.SetIsCreateMenuPoint(Value: Boolean);
begin
  if Assigned(FMasterMenu) and FIsCreateMenuPoint then
    DelAlgorithmsTo(FMasterMenu);
  FIsCreateMenuPoint := Value;
  if Assigned(FMasterMenu) and FIsCreateMenuPoint then
    AddAlgorithmsTo(FMasterMenu);
end;

procedure TsmxCustomAlgorithmList.SetIsCreateToolButton(Value: Boolean);
begin
  if Assigned(FControlBoard) and FIsCreateToolButton then
    DelAlgorithmsTo(FControlBoard);
  FIsCreateToolButton := Value;
  if Assigned(FControlBoard) and FIsCreateToolButton then
    AddAlgorithmsTo(FControlBoard);
end;}

{ TsmxCustomFilter }

//constructor TsmxCustomFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
//begin
  //inherited Create(AOwner, ADatabase, ACfgID);
  //CreateChilds;
  //InitChilds;
//end;

//destructor TsmxCustomFilter.Destroy;
//begin
  //DestroyChilds;
  //inherited Destroy;
//end;

{procedure TsmxCustomFilter.DestroyChilds;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.Free;
end;}

function TsmxCustomFilter.GetFilterText: String;
begin
  Result := '';
end;

function TsmxCustomFilter.GetFilterValue: Variant;
begin
  Result := Variants.Null;
end;

{procedure TsmxCustomFilter.InstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := Self;
end;}

procedure TsmxCustomFilter.SetFilterName(Value: String);
begin
  FFilterName := Value;
end;

procedure TsmxCustomFilter.SetFilterText(Value: String);
begin
  //FFilterText := Value;
end;

procedure TsmxCustomFilter.SetFilterValue(Value: Variant);
var
  Text: Variant;
begin
  //if FFilterValue <> Value then
  //begin
    //FFilterValue := Value;
    if FIsPutText and Assigned(FTextRequest) then
    begin
      FTextRequest.Prepare;
      smxClassFuncs.GetRequestValueByKey(FTextRequest, Value, Text);
      FilterText := Variants.VarToStr(Text);
    end;
  //end;
end;

{procedure TsmxCustomFilter.UnInstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := nil;
end;}

procedure TsmxCustomFilter.SetIsPutText(Value: Boolean);
begin
  FIsPutText := Value;
end;

procedure TsmxCustomFilter.SetAlgorithm(Value: TsmxCustomAlgorithm);
begin
  FAlgorithm := Value;
  //FSelectAlgorithm.ParentCell := Self;
end;

procedure TsmxCustomFilter.SetTextRequest(Value: TsmxCustomRequest);
begin
  FTextRequest := Value;
  //FTextRequest.ParentCell := Self;  должен назначать создатель фильтра?
end;

procedure TsmxCustomAlgorithmList.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
  if Assigned(Algorithm) then
  begin
    FAlgorithmList.Add(Algorithm);
    Algorithm.ParentCell := Self;
  end;
end;

procedure TsmxCustomAlgorithmList.RemoveAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
  if Assigned(Algorithm) then
  begin
    FAlgorithmList.Remove(Algorithm);
    Algorithm.ParentCell := Self;
  end;  
end;

{ TsmxCustomFilterDesk }

constructor TsmxCustomFilterDesk.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FFilterList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomFilterDesk.Destroy;
begin
  //DestroyChilds;
  ClearFilters;
  FFilterList.Free;
  inherited Destroy;
end;

{procedure TsmxCustomFilterDesk.DestroyChilds;
var i: Integer;
begin
  if Assigned(FApplyRequest) then
    FApplyRequest.Free;
  if Assigned(FPrepareRequest) then
    FPrepareRequest.Free;
  for i := FFilterList.Count - 1 downto 0 do
  begin
    TsmxCustomFilter(FFilterList[i]).Free;
    FFilterList.Delete(i);
  end;
end;}

function TsmxCustomFilterDesk.FindFilterByName(const AFilterName: String): TsmxCustomFilter;
var i: Integer;
begin
  Result := nil;
  for i := 0 to FilterCount - 1 do
    if AnsiCompareText(Filters[i].FilterName, AFilterName) = 0 then
    begin
      Result := Filters[i];
      Break;
    end;
end;

function TsmxCustomFilterDesk.GetFilter(Index: Integer): TsmxCustomFilter;
begin
  Result := TsmxCustomFilter(FFilterList[Index]);
end;

function TsmxCustomFilterDesk.GetFilterCount: Integer;
begin
  Result := FFilterList.Count;
end;

{procedure TsmxCustomFilterDesk.InstallParent;
var i: Integer;
begin
  if Assigned(FApplyRequest) then
    FApplyRequest.ParentCell := Self;
  if Assigned(FPrepareRequest) then
    FPrepareRequest.ParentCell := Self;
  for i := 0 to FilterCount - 1 do
    Filters[i].ParentCell := Self;
end;}

{procedure TsmxCustomFilterDesk.UnInstallParent;
var i: Integer;
begin
  if Assigned(FApplyRequest) then
    FApplyRequest.ParentCell := nil;
  if Assigned(FPrepareRequest) then
    FPrepareRequest.ParentCell := nil;
  for i := 0 to FilterCount - 1 do
    Filters[i].ParentCell := nil;
end;}

procedure TsmxCustomFilterDesk.Apply;
var
  i: Integer;
  //p: IsmxParam;
begin
  ////inherited Apply;
  //if Assigned(ApplyRequest) then
    //ApplyRequest.Perform;

  if Assigned(ApplyRequest) then
    //if Assigned(ApplyRequest.CellDataSet) then
    begin
      ApplyRequest.Prepare;
      ApplyRequest.RefreshParams;
      for i := 0 to FilterCount - 1 do
      begin
        //p := ApplyRequest.CellDataSet.FindParam(Filters[i].FilterName);
        //p := ApplyRequest.FindParam(Filters[i].FilterName);
        //if Assigned(p) then
          //p.Value := Filters[i].FilterValue;
        if Assigned(ApplyRequest.RequestParams.FindByName(Filters[i].FilterName)) then
          ApplyRequest.ParamValue[Filters[i].FilterName] := Filters[i].FilterValue;
      end;
      ApplyRequest.Perform;
    end;
end;

procedure TsmxCustomFilterDesk.Prepare;
var
  i: Integer;
  //f: IsmxField;
begin
  //inherited Prepare(Forcibly);
  if Assigned(SelectRequest) then
    //if Assigned(PrepareRequest.CellDataSet) then
      //if {not PrepareRequest.CellDataSet.Active and} (SelectRequest.OperationMode = omAutomatic) or Forcibly then
      begin
        SelectRequest.Prepare;
        SelectRequest.RefreshParams;
        SelectRequest.Perform;
        for i := 0 to FilterCount - 1 do
        begin
          //f := PrepareRequest.CellDataSet.FindField(Filters[i].FilterName);
          //if Assigned(f) then
          if Assigned(SelectRequest.RequestFields.FindByName(Filters[i].FilterName)) then
            Filters[i].FilterValue := SelectRequest.FieldValue[Filters[i].FilterName] else
            Filters[i].FilterValue := Variants.Null;
          //f := PrepareRequest.CellDataSet.FindField(Filters[i].FilterName + 'Text');
          //if Assigned(f) then
          if not Filters[i].IsPutText then
            if Assigned(SelectRequest.RequestFields.FindByName(Filters[i].FilterName + SFilterNameSuffix)) then
              Filters[i].FilterText := Variants.VarToStr(SelectRequest.FieldValue[Filters[i].FilterName + SFilterNameSuffix]) else
              Filters[i].FilterText := '';
        end;
      end;
end;

procedure TsmxCustomFilterDesk.ClearFilters;
var
  i: Integer;
begin
  for i := 0 to FFilterList.Count - 1 do
    if Assigned(FFilterList[i]) then
      TsmxCustomFilter(FFilterList[i]).Free;
  FFilterList.Clear;
end;

procedure TsmxCustomFilterDesk.SetApplyRequest(Value: TsmxCustomRequest);
begin
  FApplyRequest := Value;
  //FApplyRequest.ParentCell := Self;
end;

procedure TsmxCustomFilterDesk.SetFilter(Index: Integer; Value: TsmxCustomFilter);
var
  Filter: TsmxCustomFilter;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  Filter := FFilterList[Index];
  FFilterList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(Filter) then
    Filter.Free;
end;

procedure TsmxCustomFilterDesk.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
  //FSelectRequest.ParentCell := Self;
end;

procedure TsmxCustomFilterDesk.SetFilterCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FFilterList.Count then
    for i := Value - 1 to FFilterList.Count - 1 do
      if Assigned(FFilterList[i]) then
        TsmxCustomFilter(FFilterList[i]).Free;
  FFilterList.Count := Value;
end;

procedure TsmxCustomFilterDesk.AddFilter(Filter: TsmxCustomFilter);
begin
  if Assigned(Filter) then
  begin
    FFilterList.Add(Filter);
    Filter.ParentCell := Self;
  end;
end;

procedure TsmxCustomFilterDesk.RemoveFilter(Filter: TsmxCustomFilter);
begin
  if Assigned(Filter) then
  begin
    FFilterList.Remove(Filter);
    Filter.ParentCell := nil;
  end;
end;

{ TsmxCustomSection }

{constructor TsmxCustomSection.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //CreateChilds;
  //InitChilds;
end;}

{destructor TsmxCustomSection.Destroy;
begin
  //DestroyChilds;
  inherited Destroy;
end;}

{procedure TsmxCustomSection.Apply;
begin
  if Assigned(Grid) then
    Grid.Apply;
  if Assigned(FilterDesk) then
    FilterDesk.Apply;
end;}

{procedure TsmxCustomSection.DestroyChilds;
begin
  if Assigned(FRequest) then
    FRequest.Free;
  if Assigned(FGrid) then
    FGrid.Free;
  if Assigned(FFilterDesk) then
    FFilterDesk.Free;
end;

procedure TsmxCustomSection.InstallParent;
begin
  if Assigned(FRequest) then
    FRequest.ParentCell := Self;
  if Assigned(FGrid) then
    FGrid.ParentCell := Self;
  if Assigned(FFilterDesk) then
    FFilterDesk.ParentCell := Self;
end;}

{procedure TsmxCustomSection.UnInstallParent;
begin
  if Assigned(FRequest) then
    FRequest.ParentCell := nil;
  if Assigned(FGrid) then
    FGrid.ParentCell := nil;
  if Assigned(FFilterDesk) then
    FFilterDesk.ParentCell := nil;
end;}

{procedure TsmxCustomSection.Prepare(Forcibly: Boolean = False);
begin
  //inherited Prepare(Forcibly);
  if Assigned(Request) then
    Request.Prepare; //(Forcibly);
  if Assigned(Grid) then
    Grid.Prepare(Forcibly);
  if Assigned(FilterDesk) then
    FilterDesk.Prepare(Forcibly);
  if Assigned(Request) and Assigned(Grid) then
    Grid.Request := Request;
end;}

{procedure TsmxCustomSection.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
begin
  if Assigned(DatabaseManager) then
    UnInitialize;
  inherited SetDatabaseManager(Value);
  if Assigned(DatabaseManager) then
    Initialize;
end;}

procedure TsmxCustomSection.SetFilterDesk(Value: TsmxCustomFilterDesk);
begin
  FFilterDesk := Value;
end;

procedure TsmxCustomSection.SetGrid(Value: TsmxCustomGrid);
begin
  FGrid := Value;
end;

{procedure TsmxCustomSection.SetRequest(Value: TsmxCustomRequest);
begin
  if Assigned(FRequest) and Assigned(FGrid) then
    FGrid.Request := nil;
  FRequest := Value;
  if Assigned(FRequest) and Assigned(FGrid) then
    FGrid.Request := FRequest;
end;}

{procedure TsmxCustomSection.Initialize;
begin
  if Assigned(FRequest) and Assigned(FGrid) then
    FGrid.Request := FRequest;
end;

procedure TsmxCustomSection.UnInitialize;
begin
  if Assigned(FRequest) and Assigned(FGrid) then
    FGrid.Request := nil;
end;}

{ TsmxCustomPage }

constructor TsmxCustomPage.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FSectionList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomPage.Destroy;
begin
  //DestroyChilds;
  ClearSectioins;
  FSectionList.Free;
  inherited Destroy;
end;

{procedure TsmxCustomPage.Apply;
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].Apply;
end;}

{procedure TsmxCustomPage.DestroyChilds;
var i: Integer;
begin
  for i := FSectionList.Count - 1 downto 0 do
  begin
    TsmxCustomSection(FSectionList[i]).Free;
    FSectionList.Delete(i);
  end;
end;}

function TsmxCustomPage.GetSection(Index: Integer): TsmxCustomSection;
begin
  Result := TsmxCustomSection(FSectionList[Index]);
end;

function TsmxCustomPage.GetSectionCount: Integer;
begin
  Result := FSectionList.Count;
end;

{procedure TsmxCustomPage.InstallParent;
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].ParentCell := Self;
end;}

{procedure TsmxCustomPage.UnInstallParent;
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].ParentCell := nil;
end;}

{procedure TsmxCustomPage.Prepare(Forcibly: Boolean = False);
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].Prepare(Forcibly);
end;}

procedure TsmxCustomPage.AddSection(Section: TsmxCustomSection);
begin
  if Assigned(Section) then
  begin
    FSectionList.Add(Section);
    Section.ParentCell := Self;
  end;
end;

procedure TsmxCustomPage.ClearSectioins;
var
  i: Integer;
begin
  for i := 0 to FSectionList.Count - 1 do
    if Assigned(FSectionList[i]) then
      TsmxCustomSection(FSectionList[i]).Free;
  FSectionList.Clear;
end;

procedure TsmxCustomPage.RemoveSection(Section: TsmxCustomSection);
begin
  if Assigned(Section) then
  begin
    FSectionList.Remove(Section);
    Section.ParentCell := nil;
  end;
end;

procedure TsmxCustomPage.SetSection(Index: Integer; Value: TsmxCustomSection);
var
  Section: TsmxCustomSection;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  Section := FSectionList[Index];
  FSectionList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(Section) then
    Section.Free;
end;

procedure TsmxCustomPage.SetSectionCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FSectionList.Count then
    for i := Value - 1 to FSectionList.Count - 1 do
      if Assigned(FSectionList[i]) then
        TsmxCustomSection(FSectionList[i]).Free;
  FSectionList.Count := Value;
end;

{ TsmxCustomPageManager }

constructor TsmxCustomPageManager.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPageList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomPageManager.Destroy;
begin
  //DestroyChilds;
  ClearPages;
  FPageList.Free;
  inherited Destroy;
end;

{procedure TsmxCustomPageManager.Apply;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    Pages[i].Apply;
end;}

{procedure TsmxCustomPageManager.DestroyChilds;
var i: Integer;
begin
  for i := FPageList.Count - 1 downto 0 do
  begin
    TsmxCustomPage(FPageList[i]).Free;
    FPageList.Delete(i);
  end;
end;}

procedure TsmxCustomPageManager.AddPage(Page: TsmxCustomPage);
begin
  if Assigned(Page) then
  begin
    FPageList.Add(Page);
    Page.ParentCell := Self;
  end;
end;

procedure TsmxCustomPageManager.ClearPages;
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
    if Assigned(FPageList[i]) then
      TsmxCustomPage(FPageList[i]).Free;
  FPageList.Clear;
end;

function TsmxCustomPageManager.GetActivePage: TsmxCustomPage;
begin
  Result := nil;
end;

function TsmxCustomPageManager.GetPage(Index: Integer): TsmxCustomPage;
begin
  Result := TsmxCustomPage(FPageList[Index]);
end;

function TsmxCustomPageManager.GetPageCount: Integer;
begin
  Result := FPageList.Count;
end;

{procedure TsmxCustomPageManager.InstallParent;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    Pages[i].ParentCell := Self;
end;}

procedure TsmxCustomPageManager.RemovePage(Page: TsmxCustomPage);
begin
  if Assigned(Page) then
  begin
    FPageList.Remove(Page);
    Page.ParentCell := nil;
  end;
end;

procedure TsmxCustomPageManager.SetActivePage(Value: TsmxCustomPage);
begin
end;

{procedure TsmxCustomPageManager.UnInstallParent;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    Pages[i].ParentCell := nil;
end;}

{procedure TsmxCustomPageManager.Prepare(Forcibly: Boolean = False);
var p: TsmxCustomPage;
begin
  p := ActivePage;
  if Assigned(p) then
    p.Prepare(Forcibly);
end;}

procedure TsmxCustomPageManager.SetPage(Index: Integer; Value: TsmxCustomPage);
var
  Page: TsmxCustomPage;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  Page := FPageList[Index];
  FPageList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(Page) then
    Page.Free;
end;

procedure TsmxCustomPageManager.SetPageCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FPageList.Count then
    for i := Value - 1 to FPageList.Count - 1 do
      if Assigned(FPageList[i]) then
        TsmxCustomPage(FPageList[i]).Free;
  FPageList.Count := Value;
end;

{ TsmxCustomMenuItem }

constructor TsmxCustomMenuItem.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FMenuItemList := TList.Create;
end;

destructor TsmxCustomMenuItem.Destroy;
begin
  ClearMenuItems;
  FMenuItemList.Free;
  inherited Destroy;
end;

procedure TsmxCustomMenuItem.AddMenuItem(MenuItem: TsmxCustomMenuItem);
begin
  if Assigned(MenuItem) then
  begin
    FMenuItemList.Add(MenuItem);
    MenuItem.ParentCell := Self;
  end;
end;

procedure TsmxCustomMenuItem.ClearMenuItems;
var
  i: Integer;
begin
  for i := 0 to FMenuItemList.Count - 1 do
    if Assigned(FMenuItemList[i]) then
      TsmxCustomMenuItem(FMenuItemList[i]).Free;
  FMenuItemList.Clear;
end;

function TsmxCustomMenuItem.GetMenuItem(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(FMenuItemList[Index]);
end;

function TsmxCustomMenuItem.GetMenuItemCount: Integer;
begin
  Result := FMenuItemList.Count;
end;

procedure TsmxCustomMenuItem.RemoveMenuItem(MenuItem: TsmxCustomMenuItem);
begin
  if Assigned(MenuItem) then
  begin
    FMenuItemList.Remove(MenuItem);
    MenuItem.ParentCell := nil;
  end;
end;

procedure TsmxCustomMenuItem.SetAlgorithm(Value: TsmxCustomAlgorithm);
begin
  FAlgorithm := Value;
end;

procedure TsmxCustomMenuItem.SetMenuItem(Index: Integer; Value: TsmxCustomMenuItem);
var
  MenuItem: TsmxCustomMenuItem;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  MenuItem := FMenuItemList[Index];
  FMenuItemList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(MenuItem) then
    MenuItem.Free;
end;

procedure TsmxCustomMenuItem.SetMenuItemCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FMenuItemList.Count - 1 then
    for i := Value - 1 to FMenuItemList.Count - 1 do
      if Assigned(FMenuItemList[i]) then
        TsmxCustomMenuItem(FMenuItemList[i]).Free;
  FMenuItemList.Count := Value;
end;

{ TsmxCustomMainMenu }

constructor TsmxCustomMainMenu.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //FMenuItemList := TList.Create;
  //CreateChilds;
  //InitChilds;
  FMenuItems := TsmxCustomMenuItem.Create(Self, nil, 0);
  FMenuItems.ParentCell := Self;
end;

destructor TsmxCustomMainMenu.Destroy;
begin
  //DestroyChilds;
  //ClearMenuItems;
  //FMenuItemList.Free;
  FMenuItems.Free;
  inherited Destroy;
end;

{procedure TsmxCustomMainMenu.DestroyChilds;
var i: Integer;
begin
  for i := FMenuPointList.Count - 1 downto 0 do
  begin
    TsmxCustomMenuPoint(FMenuPointList[i]).Free;
    FMenuPointList.Delete(i);
  end;
end;}

{function TsmxCustomMainMenu.FindMenuPointByCfgID(ACfgID: Integer): TsmxCustomMenuPoint;
var i: Integer;
begin
  Result := nil;
  for i := 0 to MenuPointCount - 1 do
    if MenuPoints[i].CfgID = ACfgID then
    begin
      Result := MenuPoints[i];
      Break;
    end;
end;}

{function TsmxCustomMainMenu.GetMenuItem(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(FMenuItemList[Index]);
end;

function TsmxCustomMainMenu.GetMenuItemCount: Integer;
begin
  Result := FMenuItemList.Count;
end;}

{function TsmxCustomMainMenu.MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint;
begin
  Result := nil;
end;}

{procedure TsmxCustomMainMenu.InstallParent;
var i: Integer; mp: TsmxCustomMenuPoint;
begin
  for i := 0 to MenuPointCount - 1 do
  begin
    mp := MenuPointParent(MenuPoints[i].CfgID);
    if Assigned(mp) then
      MenuPoints[i].ParentCell := mp else
      MenuPoints[i].ParentCell := Self;
  end;
end;}

{procedure TsmxCustomMainMenu.UnInstallParent;
var i: Integer;
begin
  for i := 0 to MenuPointCount - 1 do
    MenuPoints[i].ParentCell := nil;
end;}

{procedure TsmxCustomMainMenu.AddMenuItem(MenuItem: TsmxCustomMenuItem);
begin
  if Assigned(MenuItem) then
  begin
    FMenuItemList.Add(MenuItem);
    MenuItem.ParentCell := Self;
  end;
end;

procedure TsmxCustomMainMenu.ClearMenuItems;
var
  i: Integer;
begin
  for i := 0 to FMenuItemList.Count - 1 do
    if Assigned(FMenuItemList[i]) then
      TsmxCustomMenuItem(FMenuItemList[i]).Free;
  FMenuItemList.Clear;
end;

procedure TsmxCustomMainMenu.RemoveMenuItem(MenuItem: TsmxCustomMenuItem);
begin
  if Assigned(MenuItem) then
  begin
    FMenuItemList.Remove(MenuItem);
    MenuItem.ParentCell := nil;
  end;
end;

procedure TsmxCustomMainMenu.SetMenuItem(Index: Integer; Value: TsmxCustomMenuItem);
var
  MenuItem: TsmxCustomMenuItem;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  MenuItem := FMenuItemList[Index];
  FMenuItemList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(MenuItem) then
    MenuItem.Free;
end;

procedure TsmxCustomMainMenu.SetMenuItemCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FMenuItemList.Count - 1 then
    for i := Value - 1 to FMenuItemList.Count - 1 do
      if Assigned(FMenuItemList[i]) then
        TsmxCustomMenuItem(FMenuItemList[i]).Free;
  FMenuItemList.Count := Value;
end;}

{ TsmxCustomToolBoard }

{procedure TsmxCustomToolBoard.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
end;

procedure TsmxCustomToolBoard.DelAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
end;}

{ TsmxCustomControlBoard }

constructor TsmxCustomControlBoard.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FToolBoardList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomControlBoard.Destroy;
begin
  //DestroyChilds;
  FToolBoardList.Free;
  inherited Destroy;
end;

procedure TsmxCustomControlBoard.AddToolBoard(ToolBoard: TsmxCustomToolBoard);
begin
  if Assigned(ToolBoard) then
  begin
    FToolBoardList.Add(ToolBoard);
    ToolBoard.ParentCell := Self;
  end;
end;

procedure TsmxCustomControlBoard.ClearToolBoards;
var
  i: Integer;
begin
  for i := 0 to FToolBoardList.Count - 1 do
    if Assigned(FToolBoardList[i]) then
      TsmxCustomToolBoard(FToolBoardList[i]).Free;
  FToolBoardList.Clear;
end;

{procedure TsmxCustomControlBoard.DestroyChilds;
var i: Integer;
begin
  for i := FToolBoardList.Count - 1 downto 0 do
  begin
    TsmxCustomToolBoard(FToolBoardList[i]).Free;
    FToolBoardList.Delete(i);
  end;
end;}

{function TsmxCustomControlBoard.FindToolBoardByCfgID(ACfgID: Integer): TsmxCustomToolBoard;
var i: Integer;
begin
  Result := nil;
  for i := 0 to ToolBoardCount - 1 do
    if ToolBoards[i].CfgID = ACfgID then
    begin
      Result := ToolBoards[i];
      Break;
    end;
end;}

function TsmxCustomControlBoard.GetToolBoard(Index: Integer): TsmxCustomToolBoard;
begin
  Result := TsmxCustomToolBoard(FToolBoardList[Index]);
end;

function TsmxCustomControlBoard.GetToolBoardCount: Integer;
begin
  Result := FToolBoardList.Count;
end;

{procedure TsmxCustomControlBoard.InstallParent;
var i: Integer;
begin
  for i := 0 to ToolBoardCount - 1 do
    ToolBoards[i].ParentCell := Self;
end;}

{procedure TsmxCustomControlBoard.UnInstallParent;
var i: Integer;
begin
  for i := 0 to ToolBoardCount - 1 do
    ToolBoards[i].ParentCell := nil;
end;}

{procedure TsmxCustomControlBoard.Prepare(Forcibly: Boolean = False);
var i: Integer;
begin
  for i := 0 to ToolBoardCount - 1 do
    ToolBoards[i].Prepare(Forcibly);
end;}

procedure TsmxCustomControlBoard.RemoveToolBoard(ToolBoard: TsmxCustomToolBoard);
begin
  if Assigned(ToolBoard) then
  begin
    FToolBoardList.Remove(ToolBoard);
    ToolBoard.ParentCell := nil;
  end;
end;

procedure TsmxCustomControlBoard.SetToolBoard(Index: Integer; Value: TsmxCustomToolBoard);
var
  ToolBoard: TsmxCustomToolBoard;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  ToolBoard := FToolBoardList[Index];
  FToolBoardList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(ToolBoard) then
    ToolBoard.Free;
end;

procedure TsmxCustomControlBoard.SetToolBoardCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FToolBoardList.Count then
    for i := Value - 1 to FToolBoardList.Count - 1 do
      if Assigned(FToolBoardList[i]) then
        TsmxCustomToolBoard(FToolBoardList[i]).Free;
  FToolBoardList.Count := Value;
end;

{ TsmxCustomForm }

constructor TsmxCustomForm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //FID := AID;
  FPageManagerList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

constructor TsmxCustomForm.CreateByID(AOwner: TComponent; AID: Integer);
begin
  inherited Create(AOwner, nil, 0);
  //FIntfID := AIntfID;
  FID := AID;
  //FStateCfg := TsmxIntfCfg.CreateByIntfID(Self, CfgDatabase, CfgID, FIntfID);
  FStateCfg := TsmxIntfCfg.Create(Self);
  //FStateCfg.CfgDatabase := ADatabase;
  //FStateCfg.CfgID := ACfgID;
  //FStateCfg.IntfID := AIntfID;
  //FStateCfg.Initialize;
  //FStateCfg.LoadCfg;
  //FStateCfg.ReadCfg;
  FPageManagerList := TList.Create;
  //CreateChilds;
  //InitChilds;
end;

destructor TsmxCustomForm.Destroy;
begin
  //DestroyChilds;
  ClearPageManagers;
  FPageManagerList.Free;
  if Assigned(FStateCfg) then
    FStateCfg.Free;
  inherited Destroy;
end;

procedure TsmxCustomForm.Apply;
var
  List: TList;
  i: Integer;
begin
  {for i := 0 to PageManagerCount - 1 do
    PageManagers[i].Apply;
  if Assigned(MasterMenu) then
    MasterMenu.Apply;
  if Assigned(ControlBoard) then
    ControlBoard.Apply;}
  List := TList.Create;
  try
    AllCells(List, [TsmxCustomFilterDesk]);
    for i := 0 to List.Count - 1 do
      TsmxCustomFilterDesk(List[i]).Apply;
  finally
    List.Free;
  end;
end;

{procedure TsmxCustomForm.DestroyChilds;
var i: Integer;
begin
  for i := FPageManagerList.Count - 1 downto 0 do
  begin
    TsmxCustomPageManager(FPageManagerList[i]).Free;
    FPageManagerList.Delete(i);
  end;
  if Assigned(FMasterMenu) then
    FMasterMenu.Free;
  if Assigned(FAlgorithmList) then
    FAlgorithmList.Free;
  if Assigned(FControlBoard) then
    FControlBoard.Free;
  if Assigned(FStateRequest) then
    FStateRequest.Free;
  //if Assigned(FStatusBoard) then
    //FStatusBoard.Free;
end;}

{procedure TsmxCustomForm.AddAlgorithms;
var i: Integer; mi: TsmxCustomMenuPoint; tb: TsmxCustomToolBoard;
begin
  if Assigned(AlgorithmList) then
  begin
    if AlgorithmList.IsCreateToolButton and Assigned(ControlBoard) then
      for i := AlgorithmList.AlgorithmCount - 1 downto 0 do
      begin
        tb := ControlBoard.FindToolBoardByCfgID(AlgorithmList[i].ToolBoardID);
        if Assigned(tb) then
          tb.AddAlgorithm(AlgorithmList[i]);
      end;
    if AlgorithmList.IsCreateMenuPoint and Assigned(MasterMenu) then
      for i := 0 to AlgorithmList.AlgorithmCount - 1 do
      begin
        mi := MasterMenu.FindMenuPointByCfgID(AlgorithmList[i].MenuPointID);
        if Assigned(mi) then
          mi.AddAlgorithm(AlgorithmList[i]);
      end;
  end;
end;

procedure TsmxCustomForm.DelAlgorithms;
var i: Integer; mi: TsmxCustomMenuPoint; tb: TsmxCustomToolBoard;
begin
  if Assigned(AlgorithmList) then
  begin
    if AlgorithmList.IsCreateToolButton and Assigned(ControlBoard) then
      for i := AlgorithmList.AlgorithmCount - 1 downto 0 do
      begin
        tb := ControlBoard.FindToolBoardByCfgID(AlgorithmList[i].ToolBoardID);
        if Assigned(tb) then
          tb.DelAlgorithm(AlgorithmList[i]);
      end;
    if AlgorithmList.IsCreateMenuPoint and Assigned(MasterMenu) then
      for i := AlgorithmList.AlgorithmCount - 1 downto 0 do
      begin
        mi := MasterMenu.FindMenuPointByCfgID(AlgorithmList[i].MenuPointID);
        if Assigned(mi) then
          mi.DelAlgorithm(AlgorithmList[i]);
      end;
  end;
end;}

procedure TsmxCustomForm.ChangeState;
var ID: Integer; v: Variant; //f: IsmxField;
begin
  {ID := 0;
  if Assigned(FStateRequest) then
  begin
    FStateRequest.Perform;
    f := FStateRequest.FindFieldSense(fsKey);
    if Assigned(f) then
      if not VarIsNull(f.Value) then
        ID := f.Value;
  end;}
  ID := 0;
  if Assigned(FStateRequest) then
  begin
    FStateRequest.Perform;
    ID := 0; //GetFieldSenseValueDef(FStateRequest, fsKey, 0);
  end;
  if FStateID <> ID then
  begin
    FStateID := ID;
    PutState;
  end;
end;

procedure TsmxCustomForm.CloseForm;
begin
end;

function TsmxCustomForm.GetPageManager(Index: Integer): TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(FPageManagerList[Index]);
end;

function TsmxCustomForm.GetPageManagerCount: Integer;
begin
  Result := FPageManagerList.Count;
end;

function TsmxCustomForm.GetFormModalResult:  TModalResult;
begin
  Result := mrNone;
end;

{procedure TsmxCustomForm.InstallParent;
var i: Integer;
begin
  for i := 0 to PageManagerCount - 1 do
    PageManagers[i].ParentCell := Self;
  if Assigned(FMasterMenu) then
    FMasterMenu.ParentCell := Self;
  if Assigned(FAlgorithmList) then
    FAlgorithmList.ParentCell := Self;
  if Assigned(FControlBoard) then
    FControlBoard.ParentCell := Self;
  if Assigned(FStateRequest) then
    FStateRequest.ParentCell := Self;
  //if Assigned(FStatusBoard) then
    //FStatusBoard.ParentCell := Self;
end;}

procedure TsmxCustomForm.PutState;

  procedure PutCell(AUnit: TsmxStateUnit; ACell: TsmxBaseCell);
  var i: Integer; c: TsmxBaseCell;
  begin
    c := ACell.FindCellByCfgID(AUnit.CfgID);
    if Assigned(c) then
    begin
      if c is TsmxCustomAlgorithm then
        with TsmxCustomAlgorithm(c) do
        begin
          CellEnable := AUnit.UnitEnable;
          CellVisible := AUnit.UnitVisible;
        end
      else
      if c is TsmxControlCell then
        with TsmxControlCell(c) do
        begin
          CellEnable := AUnit.UnitEnable;
          CellVisible := AUnit.UnitVisible;
        end;
      for i := 0 to AUnit.Count - 1 do
        PutCell(AUnit[i], c);
    end;
  end;

var i: Integer; cs: TsmxCellState;
begin
  cs := nil;
  if Assigned(FStateCfg) then
    cs := FStateCfg.CellStates.FindByStateID(FStateID);
  if Assigned(cs) then
    for i := 0 to cs.StateUnits.Root.Count - 1 do
      PutCell(cs.StateUnits.Root[i], Self);
end;

procedure TsmxCustomForm.SetFormManager(Value: TsmxCustomFormManager);
begin
  if Assigned(FFormManager) then
    FFormManager.RemoveForm(Self);
  FFormManager := Value;
  if Assigned(FFormManager) then
    FFormManager.InsertForm(Self);
end;

procedure TsmxCustomForm.SetFormModalResult(Value: TModalResult);
begin
end;

procedure TsmxCustomForm.SetParentForm(Value: TsmxCustomForm);
begin
  FParentForm := Value;
end;

procedure TsmxCustomForm.ShowForm;
begin
end;

function TsmxCustomForm.ShowModalForm: TModalResult;
begin
  Result := mrNone;
end;

{procedure TsmxCustomForm.UnInstallParent;
var i: Integer;
begin
  for i := 0 to PageManagerCount - 1 do
    PageManagers[i].ParentCell := nil;
  if Assigned(FMasterMenu) then
    FMasterMenu.ParentCell := nil;
  if Assigned(FAlgorithmList) then
    FAlgorithmList.ParentCell := nil;
  if Assigned(FControlBoard) then
    FControlBoard.ParentCell := nil;
  if Assigned(FStateRequest) then
    FStateRequest.ParentCell := nil;
  //if Assigned(FStatusBoard) then
    //FStatusBoard.ParentCell := nil;
end;}

procedure TsmxCustomForm.Prepare;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    AllCells(List, [TsmxCustomFilterDesk]);
    for i := 0 to List.Count - 1 do
      TsmxCustomFilterDesk(List[i]).Prepare;
  finally
    List.Free;
  end;
  {ChangeState;
  //inherited Prepare(Forcibly);
  for i := 0 to PageManagerCount - 1 do
    PageManagers[i].Prepare(Forcibly);
  if Assigned(MasterMenu) then
    MasterMenu.Prepare(Forcibly);
  if Assigned(ControlBoard) then
    ControlBoard.Prepare(Forcibly);}
end;

procedure TsmxCustomForm.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
begin
  FDatabaseManager := Value;
end;

procedure TsmxCustomForm.SetCommonStorage(Value: TsmxCustomCommonStorage);
begin
  FCommonStorage := Value;
end;

procedure TsmxCustomForm.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  FLibraryManager := Value;
end;

procedure TsmxCustomForm.AddPageManager(PageManager: TsmxCustomPageManager);
begin
  if Assigned(PageManager) then
  begin
    FPageManagerList.Add(PageManager);
    PageManager.ParentCell := Self;
  end;
end;

procedure TsmxCustomForm.ClearPageManagers;
var
  i: Integer;
begin
  for i := 0 to FPageManagerList.Count - 1 do
    if Assigned(FPageManagerList[i]) then
      TsmxCustomPageManager(FPageManagerList[i]).Free;
  FPageManagerList.Clear;
end;

procedure TsmxCustomForm.RemovePageManager(PageManager: TsmxCustomPageManager);
begin
  if Assigned(PageManager) then
  begin
    FPageManagerList.Remove(PageManager);
    PageManager.ParentCell := nil;
  end;
end;

procedure TsmxCustomForm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
end;

procedure TsmxCustomForm.SetControlBoard(Value: TsmxCustomControlBoard);
begin
  FControlBoard := Value;
end;

procedure TsmxCustomForm.SetIntfID(Value: Integer);
begin

end;

procedure TsmxCustomForm.SetMainMenu(Value: TsmxCustomMainMenu);
begin
  FMainMenu := Value;
end;

procedure TsmxCustomForm.SetPageManager(Index: Integer; Value: TsmxCustomPageManager);
var
  PageManager: TsmxCustomPageManager;
begin
  if not Assigned(Value) then
    raise EsmxCellError.CreateResFmt(@SCellBuildError, [FCfgID]);
  PageManager := FPageManagerList[Index];
  FPageManagerList[Index] := Value;
  Value.ParentCell := Self;
  if Assigned(PageManager) then
    PageManager.Free;
end;

procedure TsmxCustomForm.SetPageManagerCount(Value: Integer);
var
  i: Integer;
begin
  if Value < FPageManagerList.Count then
    for i := Value - 1 to FPageManagerList.Count - 1 do
      if Assigned(FPageManagerList[i]) then
        TsmxCustomPageManager(FPageManagerList[i]).Free;
  FPageManagerList.Count := Value;
end;

procedure TsmxCustomForm.SetStateID(Value: Integer);
begin

end;

procedure TsmxCustomForm.SetStateRequest(Value: TsmxCustomRequest);
begin
  FStateRequest := Value;
end;

procedure TsmxCustomForm.SetStatusBoard(Value: TsmxCustomStatusBoard);
begin
  FStatusBoard := Value;
end;

{ TsmxCustomMasterForm }

procedure TsmxCustomMasterForm.SetForm(Value: TForm);
begin
  FForm := Value;
end;

{ TsmxLocationParam }

constructor TsmxLocationParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamLocation := plConst;
  FParamName := '';
  FParamDefValue := Null;
  FParamType := ptUnknown;
  FDataType := ftUnknown;
end;

procedure TsmxLocationParam.Assign(AKitItem: TsmxKitItem);
begin
  if AKitItem is TsmxLocationParam then
  begin
    ParamLocation := TsmxLocationParam(AKitItem).ParamLocation;
    ParamName := TsmxLocationParam(AKitItem).ParamName;
    ParamDefValue := TsmxLocationParam(AKitItem).ParamDefValue;
    ParamType := TsmxLocationParam(AKitItem).ParamType;
    DataType := TsmxLocationParam(AKitItem).DataType;
  end else
    inherited Assign(AKitItem);
end;

{ TsmxLocationParams }

function TsmxLocationParams.Add: TsmxLocationParam;
begin
  Result := TsmxLocationParam(inherited Add);
end;

{procedure TsmxLocationParams.Assign(AKit: TsmxKit);
var
  i: Integer;
begin
  if AKit is TsmxLocationParams then
  begin
    Clear;
    for i := 0 to AKit.Count - 1 do
      Add.Assign(AKit[i]);
  end;
  inherited Assign(AKit);
end;}

function TsmxLocationParams.FindByName(AParamName: String): TsmxLocationParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxLocationParams.GetItem(Index: Integer): TsmxLocationParam;
begin
  Result := TsmxLocationParam(inherited Items[Index]);
end;

function TsmxLocationParams.GetLocationCount(ALocation: TsmxParamLocation): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].ParamLocation = ALocation then
      Inc(Result);
end;

function TsmxLocationParams.ParamByName(AParamName: String): TsmxLocationParam;
begin
  Result := FindByName(AParamName);
  if not Assigned(Result) then
    raise EsmxKitError.CreateResFmt(@SKitItemNotFound, [ClassName, AParamName]);
end;

function TsmxLocationParams.FindByLocation(ALocation: TsmxParamLocation;
  var AParams: TsmxLocationParamArray): Integer;
var
  i, j: Integer;
begin
  Result := GetLocationCount(ALocation);
  SetLength(AParams, Result);
  if Result > 0 then
  begin
    j := 0;
    for i := 0 to Count - 1 do
      if Items[i].ParamLocation = ALocation then
      begin
        AParams[j] := Items[i];
        Inc(j);
      end;
  end;
end;

procedure TsmxLocationParams.SetItem(Index: Integer; Value: TsmxLocationParam);
begin
  inherited Items[Index] := Value;
end;

procedure TsmxLocationParams.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

{ TsmxSenseField }

procedure TsmxSenseField.Assign(AKitItem: TsmxKitItem);
begin
  if AKitItem is TsmxSenseField then
  begin
    FieldFormat := TsmxSenseField(AKitItem).FieldFormat;
    FieldName := TsmxSenseField(AKitItem).FieldName;
    FieldSense := TsmxSenseField(AKitItem).FieldSense;
  end else
    inherited Assign(AKitItem);
end;

constructor TsmxSenseField.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FFieldFormat := '';
  FFieldName := '';
  FFieldSense := fsGeneral;
end;

{ TsmxSenseFields }

function TsmxSenseFields.Add: TsmxSenseField;
begin
  Result := TsmxSenseField(inherited Add);
end;

{procedure TsmxSenseFields.Assign(AKit: TsmxKit);
var
  i: Integer;
begin
  if AKit is TsmxSenseFields then
  begin
    Clear;
    for i := 0 to AKit.Count - 1 do
      Add.Assign(AKit[i]);
  end else
    inherited Assign(AKit);
end;}

function TsmxSenseFields.FieldByName(AFieldName: String): TsmxSenseField;
begin
  Result := FindByName(AFieldName);
  if not Assigned(Result) then
    raise EsmxKitError.CreateResFmt(@SKitItemNotFound, [ClassName, AFieldName]);
end;

function TsmxSenseFields.FindBySense(ASense: TsmxFieldSense;
  var AFields: TsmxSenseFieldArray): Integer;
var
  i, j: Integer;
begin
  Result := GetSenseCount(ASense);
  SetLength(AFields, Result);
  if Result > 0 then
  begin
    j := 0;
    for i := 0 to Count - 1 do
      if Items[i].FieldSense = ASense then
      begin
        AFields[j] := Items[i];
        Inc(j);
      end;
  end;
end;

function TsmxSenseFields.FindByName(AFieldName: String): TsmxSenseField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].FieldName, AFieldName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxSenseFields.GetItem(Index: Integer): TsmxSenseField;
begin
  Result := TsmxSenseField(inherited Items[Index]);
end;

function TsmxSenseFields.GetSenseCount(ASense: TsmxFieldSense): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].FieldSense = ASense then
      Inc(Result);
end;

procedure TsmxSenseFields.SetItem(Index: Integer; Value: TsmxSenseField);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestCfg }

{destructor TsmxRequestCfg.Destroy;
begin
  if Assigned(FRequestParams) then
    FRequestParams.Free;
  if Assigned(FRequestFields) then
    FRequestFields.Free;
  inherited Destroy;
end;

procedure TsmxRequestCfg.Clear;
begin
  FDataSetType := dstUnknown;
  FPerformanceMode := pmOpen;
  FSQLText := '';
  if Assigned(FRequestParams) then
    FRequestParams.Clear;
  if Assigned(FRequestFields) then
    FRequestFields.Clear;
end;

function TsmxRequestCfg.GetRequestFields: TsmxSenseFields;
begin
  if not Assigned(FRequestFields) then
    FRequestFields := TsmxSenseFields.Create(TsmxSenseField);
  Result := FRequestFields;
end;

function TsmxRequestCfg.GetRequestParams: TsmxLocationParams;
begin
  if not Assigned(FRequestParams) then
    FRequestParams := TsmxLocationParams.Create(TsmxLocationParam);
  Result := FRequestParams;
end;

procedure TsmxRequestCfg.ReadCfg;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Request');
  if Assigned(n) then
  begin
    SQLText := n.Attributes['SQLText'];
    DataSetType := n.Attributes['Type'];
    PerformanceMode := n.Attributes['Perform'];
  end;

  n := r.ChildNodes.FindNode('Params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //RequestParams.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Param' then
        with RequestParams.Add do
        begin
          ParamName := n.ChildNodes[i].Attributes['Name'];
          ParamDefValue := VarStringToVar(n.ChildNodes[i].Attributes['DefValue']);
          ParamLocation := n.ChildNodes[i].Attributes['Location'];
        end;
  end;

  n := r.ChildNodes.FindNode('Fields');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //RequestFields.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Field' then
        with RequestFields.Add do
        begin
          FieldName := n.ChildNodes[i].Attributes['Name'];
          FieldFormat := n.ChildNodes[i].Attributes['Format'];
          FieldSense := n.ChildNodes[i].Attributes['Sense'];
        end;
  end;
end;

procedure TsmxRequestCfg.WriteCfg;
var
  r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Request');
  n.Attributes['SQLText'] := SQLText;
  n.Attributes['Type'] := DataSetType;
  n.Attributes['Perform'] := PerformanceMode;

  n := r.AddChild('Params');
  for i := 0 to RequestParams.Count - 1 do
    with n.AddChild('Param') do
    begin
      Attributes['Name'] := RequestParams[i].ParamName;
      Attributes['DefValue'] := RequestParams[i].ParamDefValue;
      Attributes['Location'] := RequestParams[i].ParamLocation;
    end;

  n := r.AddChild('Fields');
  for i := 0 to RequestFields.Count - 1 do
    with n.AddChild('Field') do
    begin
      Attributes['Name'] := RequestFields[i].FieldName;
      Attributes['Format'] := RequestFields[i].FieldFormat;
      Attributes['Sense'] := RequestFields[i].FieldSense;
    end;
end;

procedure TsmxRequestCfg.SetModifySetting(Value: TsmxModifySetting);
begin
  FModifySetting := Value;
end;}

end.
