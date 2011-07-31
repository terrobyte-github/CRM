{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Program classes            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxClasses;

interface

uses
  Classes, Controls, SysUtils, Windows, Forms, ImgList, XMLIntf, smxBaseClasses,
  smxDBIntf, smxTypes;

type
  { TsmxBaseCfg }

  TsmxTargetRequest = class;

  EsmxCfgError = class(Exception);

  TsmxBaseCfg = class(TsmxComponent)
  private
    FCfgDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    FTargetRequest: TsmxTargetRequest;
    FXMLDocIntf: IXMLDocument;
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
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Finalize;
    procedure Initialize;

    property CfgDatabase: IsmxDatabase read FCfgDatabaseIntf;
    property CfgID: Integer read FCfgID;
  end;

  TsmxBaseCfgClass = class of TsmxBaseCfg;

  { TsmxCustomCommonStorage }

  TsmxCustomCommonStorage = class(TsmxComponent)
  public
    function FindByName(AParamName: String): Variant; virtual;
  end;

  { TsmxCustomLibraryManager }

  TsmxCustomLibraryManager = class(TsmxComponent)
  public
    function GetProcedure(ALibHandle: THandle; AProcName: String): Pointer; overload; virtual;
    function GetProcedure(ALibName, AProcName: String): Pointer; overload; virtual;
    function CheckLibraryComp(ALibHandle: THandle): Boolean; overload; virtual;
    function CheckLibraryComp(ALibName: String): Boolean; overload; virtual;
    function CallLibrary(ALibName: String): THandle; virtual;
  end;

  { TsmxCustomDatabaseManager }

  TsmxDBConnection = class;

  TsmxCustomDatabaseManager = class(TsmxComponent)
  public
    //function FindByName(ADatabaseName: String): IsmxDatabase; virtual;
    function FindByName(ADatabaseName: String): TsmxDBConnection; virtual;
    procedure InsertDBConnection(ADBConnection: TsmxDBConnection); virtual;
    procedure RemoveDBConnection(ADBConnection: TsmxDBConnection); virtual;
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

  TsmxBaseCell = class(TsmxComponent)
  private
    FCfg: TsmxBaseCfg;
    FCellList: TList;
    FCfgDatabaseIntf: IsmxDatabase;
    FCfgID: Integer;
    FImageList: TCustomImageList;
    FParentCell: TsmxBaseCell;
    FCommonStorage: TsmxCustomCommonStorage;
    FLibraryManager: TsmxCustomLibraryManager;
    FDatabaseManager: TsmxCustomDatabaseManager;
    function GetCellCount: Integer;
    function GetCell(Index: Integer): TsmxBaseCell;
    function GetRootCell: TsmxBaseCell;
  protected
    procedure CreateChilds; virtual;
    procedure DestroyChilds; virtual;
    function GetInternalObject: TObject; virtual;
    procedure InitChilds; virtual;
    procedure InstallParent; virtual;
    procedure SetImageList(Value: TCustomImageList); virtual;
    procedure SetParentCell(Value: TsmxBaseCell); virtual;
    procedure UnInstallParent; virtual;
    procedure Initialize; virtual;
    procedure UnInitialize; virtual;
    procedure SetCommonStorage(Value: TsmxCustomCommonStorage); virtual;
    procedure SetLibraryManager(Value: TsmxCustomLibraryManager); virtual;
    procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager); virtual;

    property Cfg: TsmxBaseCfg read FCfg;
    property CellList: TList read FCellList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); reintroduce; virtual;
    destructor Destroy; override;
    function FindCellByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxBaseCell;

    property CellCount: Integer read GetCellCount;
    property Cells[Index: Integer]: TsmxBaseCell read GetCell;
    property CfgDatabase: IsmxDatabase read FCfgDatabaseIntf;
    property CfgID: Integer read FCfgID;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ParentCell: TsmxBaseCell read FParentCell write SetParentCell;
    property RootCell: TsmxBaseCell read GetRootCell;
    property CommonStorage: TsmxCustomCommonStorage read FCommonStorage write SetCommonStorage;
    property LibraryManager: TsmxCustomLibraryManager read FLibraryManager write SetLibraryManager;
    property DatabaseManager: TsmxCustomDatabaseManager read FDatabaseManager write SetDatabaseManager;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

  { TsmxCellCfg }

  TsmxCellCfg = class(TsmxBaseCfg)
  protected
    procedure LoadCfg; override;
    procedure SaveCfg; override;
  end;

  { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxBaseCfg)
  private
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    procedure SetCfgClass(Value: TsmxBaseCfgClass);
    procedure SetCfgClassName(Value: String);
    procedure SetCellClass(Value: TsmxBaseCellClass);
    procedure SetCellClassName(Value: String);
  protected
    procedure LoadCfg; override;
    procedure ReadCfg; override;
    procedure SaveCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

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
    procedure Apply; virtual;
    procedure Prepare(Forcibly: Boolean = False); virtual;

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

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[Key: String]: String read GetValue write SetValue; default;
  end;

  { TsmxKitItem }

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
    //function Insert(Index: Integer): TsmxKitItem;
    procedure Remove(AItem: TsmxKitItem);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxKitItem read GetItem; default; //write SetItem
  end;

  { TsmxHKitItem }

  TsmxHKit = class;

  TsmxHKitItem = class(TObject)
  private
    FList: TList;
      FParent: TsmxHKitItem;
    FHKit: TsmxHKit;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetItem(Index: Integer): TsmxHKitItem;
    //procedure SetItem(Index: Integer; Value: TsmxHKitItem);
  public
    constructor Create(AHKit: TsmxHKit); virtual;
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

  TsmxParams = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxParam;
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  public
    function Add: TsmxParam;
    function FindByName(AParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem; default;
    property Values[Name: String]: Variant read GetValue write SetValue;
  end;

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
    property UnitEnable: Boolean read FUnitEnable write SetUnitEnable;
    property UnitVisible: Boolean read FUnitVisible write SetUnitVisible;
  end;

  { TsmxStateUnits }

  TsmxStateUnits = class(TsmxHKit)
  private
    FIntfID: Integer;
    function GetRoot: TsmxStateUnit;
  public
    property Root: TsmxStateUnit read GetRoot;
    property IntfID: Integer read FIntfID write FIntfID default 0;
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

  { TsmxStateCfg }

  TsmxStateCfg = class(TsmxBaseCfg)
  private
    FIntfID: Integer;
    FXMLDocList: TsmxXMLDocItems;
    FCellStates: TsmxCellStates;
    function GetCellStates: TsmxCellStates;
  protected
    procedure LoadCfg; override;
    procedure ReadCfg; override;
    procedure SaveCfg; override;
    procedure WriteCfg; override;
    function GetXMLText: String; override;
    procedure SetXMLText(Value: String); override;
    function GetFullXMLText: String; virtual;

    property XMLDocList: TsmxXMLDocItems read FXMLDocList;
    property FullXMLText: String read GetFullXMLText;
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

  { TsmxDBConnection }

  TsmxDBConnection = class(TsmxComponent)
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

  TsmxCustomRequest = class(TsmxBaseCell)
  private
    FDatabaseName: String;
    FDatabaseIntf: IsmxDatabase;
    FOperationMode: TsmxOperationMode;
  protected
    FDataSetIntf: IsmxDataSet;
    //function GetInternalObject: TObject; override;
    procedure SetDatabaseName(Value: String); virtual;
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager); override;
  public
    destructor Destroy; override;
    function FindFieldSense(AFieldSense: TsmxFieldSense; StartPos: Integer = 0): IsmxField; virtual;
    function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): IsmxParam; virtual;
    procedure Perform(Same: Boolean = False); virtual;
    procedure RefreshParams; virtual;
    procedure Prepare(Forcibly: Boolean = False); virtual;

    property CellDataSet: IsmxDataSet read FDataSetIntf;
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property OperationMode: TsmxOperationMode read FOperationMode write FOperationMode default omManual;
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
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;

    property ColumnList: TList read FColumnList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;

    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TsmxCustomColumn read GetColumn; default;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
  end;

  { TsmxCustomAlgorithm }

  TsmxCustomAlgorithm = class(TsmxBaseCell)
  private
    FParams: TsmxParams;
    FMenuPointID: Integer;
    FToolBoardID: Integer;
    function GetParams: TsmxParams;
  protected
    procedure AddParams; virtual;
    function GetCellCaption: String; virtual;
    function GetCellEnable: Boolean; virtual;
    function GetCellHotKey: Integer; virtual;
    function GetCellImageIndex: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    procedure SetCellCaption(Value: String); virtual;
    procedure SetCellEnable(Value: Boolean); virtual;
    procedure SetCellHotKey(Value: Integer); virtual;
    procedure SetCellImageIndex(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure Execute(Same: Boolean = False); virtual;
    procedure RefreshParams; virtual;
    function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam; virtual;

    property Params: TsmxParams read GetParams;
    property CellCaption: String read GetCellCaption write SetCellCaption;
    property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    property CellHotKey: Integer read GetCellHotKey write SetCellHotKey;
    property CellImageIndex: Integer read GetCellImageIndex write SetCellImageIndex;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    property MenuPointID: Integer read FMenuPointID write FMenuPointID default 0;
    property ToolBoardID: Integer read FToolBoardID write FToolBoardID default 0;
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
    //FMasterMenu: TsmxCustomMasterMenu;
    //FControlBoard: TsmxCustomControlBoard;
    FIsCreateToolButton: Boolean;
    FIsCreateMenuPoint: Boolean;
    function GetAlgorithm(Index: Integer): TsmxCustomAlgorithm;
    function GetAlgorithmCount: Integer;
    //procedure AddAlgorithmsTo(ACell: TsmxControlCell);
    //procedure DelAlgorithmsTo(ACell: TsmxControlCell);
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;
    //procedure SetIsCreateMenuPoint(Value: Boolean); virtual;
    //procedure SetIsCreateToolButton(Value: Boolean); virtual;
    //procedure SetMasterMenu(Value: TsmxCustomMasterMenu); virtual;
    //procedure SetControlBoard(Value: TsmxCustomControlBoard); virtual;

    property AlgorithmList: TList read FAlgorithmList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    function FindAlgorithmByCfgID(ACfgID: Integer): TsmxCustomAlgorithm;
    procedure AddAlgorithmsTo(ACell: TsmxControlCell); virtual;
    procedure DelAlgorithmsTo(ACell: TsmxControlCell); virtual;

    property AlgorithmCount: Integer read GetAlgorithmCount;
    property Algorithms[Index: Integer]: TsmxCustomAlgorithm read GetAlgorithm; default;
    //property MasterMenu: TsmxCustomMasterMenu read FMasterMenu write SetMasterMenu;
    //property ControlBoard: TsmxCustomControlBoard read FControlBoard write SetControlBoard;
    property IsCreateToolButton: Boolean read FIsCreateToolButton write FIsCreateToolButton default False;
    property IsCreateMenuPoint: Boolean read FIsCreateMenuPoint write FIsCreateMenuPoint default False;
  end;

  { TsmxCustomFilter }

  TsmxCustomFilter = class(TsmxControlCell)
  private
    FAlgorithm: TsmxCustomAlgorithm;
    FFilterName: String;
  protected
    procedure DestroyChilds; override;
    function GetFilterText: String; virtual;
    function GetFilterValue: Variant; virtual;
    procedure InstallParent; override;
    procedure SetFilterText(Value: String); virtual;
    procedure SetFilterValue(Value: Variant); virtual;
    //procedure UnInstallParent; override;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;

    property Algorithm: TsmxCustomAlgorithm read FAlgorithm write FAlgorithm;
    property FilterName: String read FFilterName write FFilterName;
    property FilterText: String read GetFilterText write SetFilterText;
    property FilterValue: Variant read GetFilterValue write SetFilterValue;
  end;

  { TsmxCustomFilterDesk }

  TsmxCustomFilterDesk = class(TsmxControlCell)
  private
    FApplyRequest: TsmxCustomRequest;
    FFilterList: TList;
    FPrepareRequest: TsmxCustomRequest;
    function GetFilter(Index: Integer): TsmxCustomFilter;
    function GetFilterCount: Integer;
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property FilterList: TList read FFilterList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    function FindFilterByName(const AFilterName: String): TsmxCustomFilter;
    procedure Apply; override;
    procedure Prepare(Forcibly: Boolean = False); override;

    property FilterCount: Integer read GetFilterCount;
    property Filters[Index: Integer]: TsmxCustomFilter read GetFilter; default;
    property ApplyRequest: TsmxCustomRequest read FApplyRequest write FApplyRequest;
    property PrepareRequest: TsmxCustomRequest read FPrepareRequest write FPrepareRequest;
  end;

  { TsmxCustomSection }

  TsmxCustomSection = class(TsmxControlCell)
  private
    FFilterDesk: TsmxCustomFilterDesk;
    FGrid: TsmxCustomGrid;
    FRequest: TsmxCustomRequest;
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;
    //procedure SetGrid(Value: TsmxCustomGrid); virtual;
    //procedure SetRequest(Value: TsmxCustomRequest); virtual;
    //procedure SetDatabaseManager(Value: TsmxCustomDatabaseManager); override;
    //procedure Initialize; override;
    //procedure UnInitialize; override;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure Apply; override;
    procedure Prepare(Forcibly: Boolean = False); override;

    property Grid: TsmxCustomGrid read FGrid write FGrid; //SetGrid;
    property FilterDesk: TsmxCustomFilterDesk read FFilterDesk write FFilterDesk;
    property Request: TsmxCustomRequest read FRequest write FRequest; //SetRequest;
  end;

  { TsmxCustomPage }

  TsmxCustomPage = class(TsmxControlCell)
  private
    FSectionList: TList;
    function GetSection(Index: Integer): TsmxCustomSection;
    function GetSectionCount: Integer;
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property SectionList: TList read FSectionList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure Apply; override;
    procedure Prepare(Forcibly: Boolean = False); override;
    
    property SectionCount: Integer read GetSectionCount;
    property Sections[Index: Integer]: TsmxCustomSection read GetSection; default;
  end;

  { TsmxCustomPageManager }

  TsmxCustomPageManager = class(TsmxControlCell)
  private
    FPageList: TList;
    function GetPage(Index: Integer): TsmxCustomPage;
    function GetPageCount: Integer;
  protected
    procedure DestroyChilds; override;
    function GetActivePage: TsmxCustomPage; virtual;
    procedure SetActivePage(Value: TsmxCustomPage); virtual;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property PageList: TList read FPageList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure Apply; override;
    procedure Prepare(Forcibly: Boolean = False); override;

    property ActivePage: TsmxCustomPage read GetActivePage write SetActivePage;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TsmxCustomPage read GetPage; default;
  end;

  { TsmxCustomMenuPoint }

  TsmxCustomMenuPoint = class(TsmxControlCell)
  public
    procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); virtual;
    procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); virtual;
  end;

  { TsmxCustomMasterMenu }

  TsmxCustomMasterMenu = class(TsmxControlCell)
  private
    FMenuPointList: TList;
    function GetMenuPoint(Index: Integer): TsmxCustomMenuPoint;
    function GetMenuPointCount: Integer;
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property MenuPointList: TList read FMenuPointList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    function FindMenuPointByCfgID(ACfgID: Integer): TsmxCustomMenuPoint;
    function MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint; virtual;

    property MenuPointCount: Integer read GetMenuPointCount;
    property MenuPoints[Index: Integer]: TsmxCustomMenuPoint read GetMenuPoint; default;
  end;

  { TsmxCustomToolBoard }

  TsmxCustomToolBoard = class(TsmxControlCell)
  public
    procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); virtual;
    procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); virtual;
  end;

  { TsmxCustomControlBoard }

  TsmxCustomControlBoard = class(TsmxControlCell)
  private
    FToolBoardList: TList;
    function GetToolBoard(Index: Integer): TsmxCustomToolBoard;
    function GetToolBoardCount: Integer;
  protected
    procedure DestroyChilds; override;
    procedure InstallParent; override;
    //procedure UnInstallParent; override;

    property ToolBoardList: TList read FToolBoardList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    function FindToolBoardByCfgID(ACfgID: Integer): TsmxCustomToolBoard;
    procedure Prepare(Forcibly: Boolean = False); override;

    property ToolBoardCount: Integer read GetToolBoardCount;
    property ToolBoards[Index: Integer]: TsmxCustomToolBoard read GetToolBoard; default;
  end;

  { TsmxCustomStatusBoard }

  TsmxCustomStatusBoard = class(TsmxControlCell)
  end;

  { TsmxCustomForm }

  TsmxCustomForm = class(TsmxControlCell)
  private
    FAlgorithmList: TsmxCustomAlgorithmList;
    FControlBoard: TsmxCustomControlBoard;
    FMasterMenu: TsmxCustomMasterMenu;
    FPageManagerList: TList;
    FStatusBoard: TsmxCustomStatusBoard;
    FParentForm: TsmxCustomForm;
    FID: Integer;
    FIntfID: Integer;
    FStateID: Integer;
    FStateRequest: TsmxCustomRequest;
    FStateCfg: TsmxStateCfg;
    FFormManager: TsmxCustomFormManager;
    function GetPageManager(Index: Integer): TsmxCustomPageManager;
    function GetPageManagerCount: Integer;
  protected
    //procedure AddAlgorithms; virtual;
    //procedure DelAlgorithms; virtual;
    procedure DestroyChilds; override;
    function GetFormModalResult: TModalResult; virtual;
    procedure InstallParent; override;
    procedure SetFormModalResult(Value: TModalResult); virtual;
    procedure SetParentForm(Value: TsmxCustomForm); virtual;
    //procedure UnInstallParent; override;
    procedure ChangeState; virtual;
    procedure PutState; virtual;
    procedure SetFormManager(Value: TsmxCustomFormManager); virtual;

    property StateCfg: TsmxStateCfg read FStateCfg;
    property PageManagerList: TList read FPageManagerList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); reintroduce; virtual;
    constructor CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID, AIntfID: Integer; AID: Integer = 0); virtual;
    destructor Destroy; override;
    procedure Apply; override;
    procedure CloseForm; virtual;
    procedure ShowForm; virtual;
    function ShowModalForm: TModalResult; virtual;
    procedure Prepare(Forcibly: Boolean = False); override;

    property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write FAlgorithmList;
    property ControlBoard: TsmxCustomControlBoard read FControlBoard write FControlBoard;
    property FormModalResult: TModalResult read GetFormModalResult write SetFormModalResult;
    property MasterMenu: TsmxCustomMasterMenu read FMasterMenu write FMasterMenu;
    property ParentForm: TsmxCustomForm read FParentForm write SetParentForm;
    property PageManagerCount: Integer read GetPageManagerCount;
    property PageManagers[Index: Integer]: TsmxCustomPageManager read GetPageManager; default;
    property StatusBoard: TsmxCustomStatusBoard read FStatusBoard write FStatusBoard;
    property ID: Integer read FID default 0;
    property IntfID: Integer read FIntfID default 0;
    property StateID: Integer read FStateID default 0;
    property StateRequest: TsmxCustomRequest read FStateRequest write FStateRequest;
    property FormManager: TsmxCustomFormManager read FFormManager write SetFormManager;
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

implementation

uses
  DB, Variants, ComObj, StrUtils{, XMLDoc}, smxFuncs, smxClassFuncs, smxDBTypes,
  smxConsts;

{ TsmxBaseCfg }

constructor TsmxBaseCfg.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer);
begin
  inherited Create(AOwner);
  FCfgDatabaseIntf := ADatabase;
  FCfgID := ACfgID;
  FTargetRequest := TsmxTargetRequest.Create(Self);
  FTargetRequest.Database := FCfgDatabaseIntf;
  FXMLDocIntf := NewXML; //NewXMLDocument;
end;

destructor TsmxBaseCfg.Destroy;
begin
  FTargetRequest.Free;
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
end;

procedure TsmxBaseCfg.ReadCfg;
begin
end;

procedure TsmxBaseCfg.SaveCfg;
begin
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

{ TsmxCustomCommonStorage }

function TsmxCustomCommonStorage.FindByName(AParamName: String): Variant;
begin
  Result := Null;
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

function TsmxCustomDatabaseManager.FindByName(ADatabaseName: String): TsmxDBConnection;
begin
  Result := nil;
end;

procedure TsmxCustomDatabaseManager.InsertDBConnection(ADBConnection: TsmxDBConnection);
begin
end;

procedure TsmxCustomDatabaseManager.RemoveDBConnection(ADBConnection: TsmxDBConnection);
begin
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
  FCfgDatabaseIntf := ADatabase;
  FCfgID := ACfgID;
  FCfg := NewCfg(Self, FCfgDatabaseIntf, FCfgID);
  FCfg.Initialize;  
  FCellList := TList.Create;
end;

destructor TsmxBaseCell.Destroy;
begin
  SetParentCell(nil);
  FCellList.Free;
  FCfg.Free;
  FCfgDatabaseIntf := nil;
  inherited Destroy;
end;

procedure TsmxBaseCell.CreateChilds;
begin
end;

procedure TsmxBaseCell.DestroyChilds;
begin
end;

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

procedure TsmxBaseCell.InitChilds;
begin
end;

procedure TsmxBaseCell.Initialize;
begin
end;

procedure TsmxBaseCell.InstallParent;
begin
end;

procedure TsmxBaseCell.SetImageList(Value: TCustomImageList);
var i: Integer;
begin
  if Assigned(FImageList) then
    for i := 0 to CellCount - 1 do
      Cells[i].ImageList := nil;
  FImageList := Value;
  if Assigned(FImageList) then
    for i := 0 to CellCount - 1 do
      Cells[i].ImageList := FImageList;
end;

procedure TsmxBaseCell.SetCommonStorage(Value: TsmxCustomCommonStorage);
var i: Integer;
begin
  if Assigned(FCommonStorage) then
    for i := 0 to CellCount - 1 do
      Cells[i].CommonStorage := nil;
  FCommonStorage := Value;
  if Assigned(FCommonStorage) then
    for i := 0 to CellCount - 1 do
      Cells[i].CommonStorage := FCommonStorage;
end;

procedure TsmxBaseCell.SetLibraryManager(Value: TsmxCustomLibraryManager);
var i: Integer;
begin
  if Assigned(FLibraryManager) then
    for i := 0 to CellCount - 1 do
      Cells[i].LibraryManager := nil;
  FLibraryManager := Value;
  if Assigned(FLibraryManager) then
    for i := 0 to CellCount - 1 do
      Cells[i].LibraryManager := FLibraryManager;
end;

procedure TsmxBaseCell.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
var i: Integer;
begin
  if Assigned(FDatabaseManager) then
    for i := 0 to CellCount - 1 do
      Cells[i].DatabaseManager := nil;
  FDatabaseManager := Value;
  if Assigned(FDatabaseManager) then
    for i := 0 to CellCount - 1 do
      Cells[i].DatabaseManager := FDatabaseManager;
end;

procedure TsmxBaseCell.SetParentCell(Value: TsmxBaseCell);
begin
  if Assigned(FParentCell) then
    FParentCell.FCellList.Remove(Self);
  FParentCell := Value;
  if Assigned(FParentCell) then
    FParentCell.FCellList.Add(Self);
end;

procedure TsmxBaseCell.UnInitialize;
begin
end;

procedure TsmxBaseCell.UnInstallParent;
var i: Integer;
begin
  for i := CellCount - 1 downto 0 do
    Cells[i].ParentCell := nil;
end;

{ TsmxCellCfg }

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

procedure TsmxCellCfg.SaveCfg;
var
  Request: IsmxDataSet;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  //FTargetRequest.DoExecute('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');
  Request := FTargetRequest.NewRequest('update tConfigs set ConfBlob = :ConfBlob where ConfID = :ConfID');
  try
    Request.ParamByName('ConfBlob').DataType := ftBlob;
    FTargetRequest.PrepRequest(Request, True, pmExecute);
  finally
    Request := nil;
  end;
end;

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

procedure TsmxTypeCfg.SaveCfg;
begin
  FTargetRequest['ConfID'] := IntToStr(FCfgID);
  FTargetRequest['ConfBlob'] := FXMLDocIntf.XML.Text;
  FTargetRequest.DoExecute('update tConfigs c set c.ConfBlob = :ConfBlob ' +
    'where c.ConfID = ' +
    '(select c2.ConfConfID from tConfigs c2 where c2.ConfID = :ConfID');
end;

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

{ TsmxControlCell }

procedure TsmxControlCell.Apply;
//var i: Integer;
begin
  {for i := 0 to CellCount - 1 do
    if Cells[i] is TsmxControlCell then
      TsmxControlCell(Cells[i]).Apply;}
end;

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

procedure TsmxControlCell.Prepare(Forcibly: Boolean = False);
//var i: Integer;
begin
  {for i := 0 to CellCount - 1 do
    if Cells[i] is TsmxControlCell then
      TsmxControlCell(Cells[i]).Prepare(Forcibly);} {else
    if Cells[i] is TsmxCustomRequest then
      TsmxCustomRequest(Cells[i]).Prepare(Forcibly);}
end;

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
var i: Integer; f: IsmxField; v: Variant; s: TStream;
begin
  Result := False;
  with ARequest do
  begin
    Close;
    if not Prepared then
      Prepare;
    for i := 0 to ParamCount - 1 do
      if Params[i].ParamType in [ptUnknown, ptInput, ptInputOutput] then
      begin
        f := nil;
        if Assigned(DSFrom) then
          f := DSFrom.FindField(Params[i].ParamName);
        if Assigned(f) then
          Params[i].AssignParam(f)
        else
        begin
          if ParamValues[Params[i].ParamName] = '' then
            Params[i].Value := Null
          else
          begin
            if Params[i].DataType = ftBlob then
            begin
              s := StrToStream(ParamValues[Params[i].ParamName]);
              try
                Params[i].LoadStream(s);
              finally
                s.Free;
              end;
            end else
              Params[i].Value := ParamValues[Params[i].ParamName];
          end;
        end;
      end;
    if Get then
    begin
      case Perform of
        pmOpen: Open;
        pmExecute: Execute;
      end;
      v := 0;
      for i := 0 to ParamCount - 1 do
        if Params[i].ParamType in [ptUnknown, ptOutput, ptInputOutput] then
        begin
          if VarIsNull(Params[i].Value) then
            ParamValues[Params[i].ParamName] := ''
          else
          begin
            ParamValues[Params[i].ParamName] := Params[i].Value;
          end;
        end else
        if Params[i].ParamType = ptResult then
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
var i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    TsmxHKitItem(FList[i]).Free;
    FList.Delete(i);
  end;
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
  temp := FList.Remove(AItem);
  if temp >= 0 then
    AItem.Free;
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
  FRoot := FCellClass.Create(Self);
end;

destructor TsmxHKit.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

{ TsmxParam }

constructor TsmxParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamName := '';
  FParamValue := Null;
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

function TsmxParams.GetValue(Name: String): Variant;
var p: TsmxParam;
begin
  p := FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
end;

procedure TsmxParams.SetValue(Name: String; Value: Variant);
var p: TsmxParam;
begin
  p := FindByName(Name);
  if Assigned(p) then
    p.ParamValue := Value else
    raise EsmxKitError.CreateRes(@SKitParamNotFound);
end;

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
  FXMLDocList.Free;
  if Assigned(FCellStates) then
    FCellStates.Free;
  inherited Destroy;
end;

procedure TsmxStateCfg.Clear;
begin
  CellStates.Clear;
end;

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

procedure TsmxStateCfg.LoadCfg;
var IntfID: Integer; XMLText: String; XMLDocIntf: IXMLDocument;
begin
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
  end;
end;

procedure TsmxStateCfg.ReadCfg;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxStateUnit; AIntfID: Integer);
  var i: Integer; u: TsmxStateUnit;
  begin
    u := AUnit.FindByCfgID(ANode.Attributes['id']);
    if not Assigned(u) then
      u := AUnit.Add;
    with u do
    begin
      FIntfID := AIntfID;
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
            with s do
            begin
              ID := n.ChildNodes[i].Attributes['id'];
              n2 := n.ChildNodes[i].ChildNodes.FindNode('cells');
              if Assigned(n2) and (n2.ChildNodes.Count > 0) then
                for j := 0 to n2.ChildNodes.Count - 1 do
                  if n2.ChildNodes[j].NodeName = 'cell' then
                    AddUnits(n2.ChildNodes[j], StateUnits.Root, FXMLDocList[k].ID);
            end;
          end;
      end;
    end;
  end;
end;

procedure TsmxStateCfg.SaveCfg;
var XMLDocItem: TsmxXMLDocItem; 
begin
  XMLDocItem := FXMLDocList.FindByID(FIntfID);
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
  end;
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
  XMLDocItem := FXMLDocList.FindByID(FIntfID);
  if not Assigned(XMLDocItem) then
  begin
    XMLDocItem := FXMLDocList.Add;
    XMLDocItem.ID := FIntfID;
  end;

  r := XMLDocItem.XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
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

{ TsmxDBConnection }

destructor TsmxDBConnection.Destroy;
begin
  DisconnectFromDatabase;
  SetDatabaseManager(nil);
  inherited Destroy;
end;

function TsmxDBConnection.CreateDatabaseAsFunc: IsmxDatabase;
var FuncNewDatabase: TsmxFuncNewDatabase;
begin
  if Assigned(FLibraryManager) then
    @FuncNewDatabase := FLibraryManager.GetProcedure(FLibraryName, FFunctionNameOrProgID) else
    FuncNewDatabase := nil;
  if Assigned(FuncNewDatabase) then
    Result := FuncNewDatabase else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;

function TsmxDBConnection.CreateDatabaseAsCOM: IsmxDatabase;
var Comp: Boolean;
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

procedure TsmxDBConnection.ConnectToDatabase;
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

procedure TsmxDBConnection.DisconnectFromDatabase;
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

procedure TsmxDBConnection.SetDatabaseName(Value: String);
begin
  if (FDatabaseName <> '') and Assigned(FDatabaseManager) then
    FDatabaseManager.RemoveDBConnection(Self);
  FDatabaseName := Value;
  if (FDatabaseName <> '') and Assigned(FDatabaseManager) then
    FDatabaseManager.InsertDBConnection(Self);
end;

procedure TsmxDBConnection.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  FLibraryManager := Value;
end;

procedure TsmxDBConnection.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
begin
  if Assigned(FDatabaseManager) and (FDatabaseName <> '') then
    FDatabaseManager.RemoveDBConnection(Self);
  FDatabaseManager := Value;
  if Assigned(FDatabaseManager) and (FDatabaseName <> '') then
    FDatabaseManager.InsertDBConnection(Self);
end;

{ TsmxCustomRequest }

destructor TsmxCustomRequest.Destroy;
begin
  FDataSetIntf := nil;
  FDatabaseIntf := nil;
  inherited Destroy;
end;

{function TsmxCustomRequest.GetInternalObject: TObject;
begin
  if Assigned(FDataSetIntf) then
    Result := FDataSetIntf.GetDataSet else
    Result := nil;
end;}

function TsmxCustomRequest.FindFieldSense(AFieldSense: TsmxFieldSense; StartPos: Integer = 0): IsmxField;
begin
  Result := nil;
end;

function TsmxCustomRequest.FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): IsmxParam;
begin
  Result := nil;
end;

procedure TsmxCustomRequest.Perform(Same: Boolean = False);
begin
end;

procedure TsmxCustomRequest.Prepare(Forcibly: Boolean = False);
begin
  if Assigned(CellDataSet) then
    if not CellDataSet.Active and (OperationMode = omAutomatic) or Forcibly then
      Perform;
end;

procedure TsmxCustomRequest.RefreshParams;
begin
end;

procedure TsmxCustomRequest.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

procedure TsmxCustomRequest.SetDatabaseName(Value: String);
var dbc: TsmxDBConnection;
begin
  if FDatabaseName <> '' then
    SetDatabase(nil);
  FDatabaseName := Value;
  if FDatabaseName <> '' then
    if Assigned(DatabaseManager) then
    begin
      dbc := DatabaseManager.FindByName(FDatabaseName);
      if Assigned(dbc) then
        if Assigned(dbc.Database) then
          SetDatabase(dbc.Database);
    end;
end;

procedure TsmxCustomRequest.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
var dbc: TsmxDBConnection;
begin
  if Assigned(DatabaseManager) then
    SetDatabase(nil);
  inherited SetDatabaseManager(Value);
  if Assigned(DatabaseManager) then
  begin
    dbc := DatabaseManager.FindByName(FDatabaseName);
    if Assigned(dbc) then
      if Assigned(dbc.Database) then
        SetDatabase(dbc.Database);
  end;
end;

{ TsmxCustomGrid }

constructor TsmxCustomGrid.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FColumnList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomGrid.Destroy;
begin
  DestroyChilds;
  FColumnList.Free;
  inherited Destroy;
end;

procedure TsmxCustomGrid.DestroyChilds;
var i: Integer;
begin
  for i := FColumnList.Count - 1 downto 0 do
  begin
    TsmxCustomColumn(FColumnList[i]).Free;
    FColumnList.Delete(i);
  end;
end;

function TsmxCustomGrid.GetColumn(Index: Integer): TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(FColumnList[Index]);
end;

function TsmxCustomGrid.GetColumnCount: Integer;
begin
  Result := FColumnList.Count;
end;

procedure TsmxCustomGrid.InstallParent;
var i: Integer;
begin
  for i := 0 to ColumnCount - 1 do
    Columns[i].ParentCell := Self;
end;

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

{ TsmxCustomAlgorithm }

constructor TsmxCustomAlgorithm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  AddParams;
end;

destructor TsmxCustomAlgorithm.Destroy;
begin
  if Assigned(FParams) then
    FParams.Free;
  inherited Destroy;
end;

procedure TsmxCustomAlgorithm.AddParams;
begin
end;

procedure TsmxCustomAlgorithm.Execute(Same: Boolean = False);
begin
end;

function TsmxCustomAlgorithm.FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam;
begin
  Result := nil;
end;

function TsmxCustomAlgorithm.GetParams: TsmxParams;
begin
  if not Assigned(FParams) then
    FParams := TsmxParams.Create(TsmxParam);
  Result := FParams;
end;

function TsmxCustomAlgorithm.GetCellCaption: String;
begin
  Result := '';
end;

function TsmxCustomAlgorithm.GetCellEnable: Boolean;
begin
  Result := False;
end;

function TsmxCustomAlgorithm.GetCellHotKey: Integer;
begin
  Result := 0;
end;

function TsmxCustomAlgorithm.GetCellImageIndex: Integer;
begin
  Result := -1;
end;

function TsmxCustomAlgorithm.GetCellVisible: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomAlgorithm.RefreshParams;
begin
end;

procedure TsmxCustomAlgorithm.SetCellCaption(Value: String);
begin
end;

procedure TsmxCustomAlgorithm.SetCellEnable(Value: Boolean);
begin
end;

procedure TsmxCustomAlgorithm.SetCellHotKey(Value: Integer);
begin
end;

procedure TsmxCustomAlgorithm.SetCellImageIndex(Value: Integer);
begin
end;

procedure TsmxCustomAlgorithm.SetCellVisible(Value: Boolean);
begin
end;

{ TsmxCustomLibAlgorithm }

{procedure TsmxCustomLibAlgorithm.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  FLibManager := Value;
end;}

{ TsmxCustomAlgorithmList }

constructor TsmxCustomAlgorithmList.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FAlgorithmList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomAlgorithmList.Destroy;
begin
  DestroyChilds;
  FAlgorithmList.Free;
  inherited Destroy;
end;

procedure TsmxCustomAlgorithmList.DestroyChilds;
var i: Integer;
begin
  for i := FAlgorithmList.Count - 1 downto 0 do
  begin
    TsmxCustomAlgorithm(FAlgorithmList[i]).Free;
    FAlgorithmList.Delete(i);
  end;
end;

function TsmxCustomAlgorithmList.FindAlgorithmByCfgID(ACfgID: Integer): TsmxCustomAlgorithm;
var i: Integer;
begin
  Result := nil;
  for i := 0 to AlgorithmCount - 1 do
    if Algorithms[i].CfgID = ACfgID then
    begin
      Result := Algorithms[i];
      Break;
    end;
end;

function TsmxCustomAlgorithmList.GetAlgorithm(Index: Integer): TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(FAlgorithmList[Index]);
end;

function TsmxCustomAlgorithmList.GetAlgorithmCount: Integer;
begin
  Result := FAlgorithmList.Count;
end;

procedure TsmxCustomAlgorithmList.InstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    Algorithms[i].ParentCell := Self;
end;

{procedure TsmxCustomAlgorithmList.UnInstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    Algorithms[i].ParentCell := nil;
end;}

procedure TsmxCustomAlgorithmList.AddAlgorithmsTo(ACell: TsmxControlCell);
var i: Integer; mp: TsmxCustomMenuPoint; tb: TsmxCustomToolBoard;
begin
  if ACell is TsmxCustomMasterMenu and FIsCreateMenuPoint then
  begin
    for i := 0 to AlgorithmCount - 1 do
    begin
      mp := TsmxCustomMasterMenu(ACell).FindMenuPointByCfgID(Algorithms[i].MenuPointID);
      if Assigned(mp) then
        mp.AddAlgorithm(Algorithms[i]);
    end;
  end else
  if ACell is TsmxCustomControlBoard and FIsCreateToolButton then
  begin
    for i := AlgorithmCount - 1 downto 0 do
    begin
      tb := TsmxCustomControlBoard(ACell).FindToolBoardByCfgID(Algorithms[i].ToolBoardID);
      if Assigned(tb) then
        tb.AddAlgorithm(Algorithms[i]);
    end;
  end;
end;

procedure TsmxCustomAlgorithmList.DelAlgorithmsTo(ACell: TsmxControlCell);
var i: Integer; mp: TsmxCustomMenuPoint; tb: TsmxCustomToolBoard;
begin
  if ACell is TsmxCustomMasterMenu then
  begin
    for i := AlgorithmCount - 1 downto 0 do
    begin
      mp := TsmxCustomMasterMenu(ACell).FindMenuPointByCfgID(Algorithms[i].MenuPointID);
      if Assigned(mp) then
        mp.DelAlgorithm(Algorithms[i]);
    end;
  end else
  if ACell is TsmxCustomControlBoard then
  begin
    for i := AlgorithmCount - 1 downto 0 do
    begin
      tb := TsmxCustomControlBoard(ACell).FindToolBoardByCfgID(Algorithms[i].ToolBoardID);
      if Assigned(tb) then
        tb.DelAlgorithm(Algorithms[i]);
    end;
  end;
end;

{procedure TsmxCustomAlgorithmList.SetMasterMenu(Value: TsmxCustomMasterMenu);
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

constructor TsmxCustomFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomFilter.Destroy;
begin
  DestroyChilds;
  inherited Destroy;
end;

procedure TsmxCustomFilter.DestroyChilds;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.Free;
end;

function TsmxCustomFilter.GetFilterText: String;
begin
  Result := '';
end;

function TsmxCustomFilter.GetFilterValue: Variant;
begin
  Result := Null;
end;

procedure TsmxCustomFilter.InstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := Self;
end;

procedure TsmxCustomFilter.SetFilterText(Value: String);
begin
end;

procedure TsmxCustomFilter.SetFilterValue(Value: Variant);
begin
end;

{procedure TsmxCustomFilter.UnInstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := nil;
end;}

{ TsmxCustomFilterDesk }

constructor TsmxCustomFilterDesk.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FFilterList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomFilterDesk.Destroy;
begin
  DestroyChilds;
  FFilterList.Free;
  inherited Destroy;
end;

procedure TsmxCustomFilterDesk.DestroyChilds;
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
end;

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

procedure TsmxCustomFilterDesk.InstallParent;
var i: Integer;
begin
  if Assigned(FApplyRequest) then
    FApplyRequest.ParentCell := Self;
  if Assigned(FPrepareRequest) then
    FPrepareRequest.ParentCell := Self;
  for i := 0 to FilterCount - 1 do
    Filters[i].ParentCell := Self;
end;

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
var i: Integer; p: IsmxParam;
begin
  ////inherited Apply;
  //if Assigned(ApplyRequest) then
    //ApplyRequest.Perform;
  if Assigned(ApplyRequest) then
    if Assigned(ApplyRequest.CellDataSet) then
    begin
      ApplyRequest.RefreshParams;
      for i := 0 to FilterCount - 1 do
      begin
        p := ApplyRequest.CellDataSet.FindParam(Filters[i].FilterName);
        if Assigned(p) then
          p.Value := Filters[i].FilterValue;
      end;
      ApplyRequest.Perform(True);
    end;
end;

procedure TsmxCustomFilterDesk.Prepare(Forcibly: Boolean = False);
var i: Integer; f: IsmxField;
begin
  //inherited Prepare(Forcibly);
  if Assigned(PrepareRequest) then
    if Assigned(PrepareRequest.CellDataSet) then
      if not PrepareRequest.CellDataSet.Active and (PrepareRequest.OperationMode = omAutomatic) or Forcibly then
      begin
        PrepareRequest.Perform;
        for i := 0 to FilterCount - 1 do
        begin
          f := PrepareRequest.CellDataSet.FindField(Filters[i].FilterName);
          if Assigned(f) then
            Filters[i].FilterValue := f.Value else
            Filters[i].FilterValue := Null;
          f := PrepareRequest.CellDataSet.FindField(Filters[i].FilterName + 'Text');
          if Assigned(f) then
          begin
            if VarIsNull(f.Value) then
              Filters[i].FilterText := '' else
              Filters[i].FilterText := f.Value;
          end else
            Filters[i].FilterText := '';
        end;
      end;
end;

{ TsmxCustomSection }

constructor TsmxCustomSection.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomSection.Destroy;
begin
  DestroyChilds;
  inherited Destroy;
end;

procedure TsmxCustomSection.Apply;
begin
  if Assigned(Grid) then
    Grid.Apply;
  if Assigned(FilterDesk) then
    FilterDesk.Apply;
end;

procedure TsmxCustomSection.DestroyChilds;
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
end;

{procedure TsmxCustomSection.UnInstallParent;
begin
  if Assigned(FRequest) then
    FRequest.ParentCell := nil;
  if Assigned(FGrid) then
    FGrid.ParentCell := nil;
  if Assigned(FFilterDesk) then
    FFilterDesk.ParentCell := nil;
end;}

procedure TsmxCustomSection.Prepare(Forcibly: Boolean = False);
begin
  //inherited Prepare(Forcibly);
  if Assigned(Request) then
    Request.Prepare(Forcibly);
  if Assigned(Grid) then
    Grid.Prepare(Forcibly);
  if Assigned(FilterDesk) then
    FilterDesk.Prepare(Forcibly);
  if Assigned(Request) and Assigned(Grid) then
    Grid.Request := Request;  
end;

{procedure TsmxCustomSection.SetDatabaseManager(Value: TsmxCustomDatabaseManager);
begin
  if Assigned(DatabaseManager) then
    UnInitialize;
  inherited SetDatabaseManager(Value);
  if Assigned(DatabaseManager) then
    Initialize;
end;}

{procedure TsmxCustomSection.SetGrid(Value: TsmxCustomGrid);
begin
  if Assigned(FRequest) and Assigned(FGrid) then
    FGrid.Request := nil;
  FGrid := Value;
  if Assigned(FRequest) and Assigned(FGrid) then
    FGrid.Request := FRequest;
end;

procedure TsmxCustomSection.SetRequest(Value: TsmxCustomRequest);
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
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomPage.Destroy;
begin
  DestroyChilds;
  FSectionList.Free;
  inherited Destroy;
end;

procedure TsmxCustomPage.Apply;
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].Apply;
end;

procedure TsmxCustomPage.DestroyChilds;
var i: Integer;
begin
  for i := FSectionList.Count - 1 downto 0 do
  begin
    TsmxCustomSection(FSectionList[i]).Free;
    FSectionList.Delete(i);
  end;
end;

function TsmxCustomPage.GetSection(Index: Integer): TsmxCustomSection;
begin
  Result := TsmxCustomSection(FSectionList[Index]);
end;

function TsmxCustomPage.GetSectionCount: Integer;
begin
  Result := FSectionList.Count;
end;

procedure TsmxCustomPage.InstallParent;
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].ParentCell := Self;
end;

{procedure TsmxCustomPage.UnInstallParent;
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].ParentCell := nil;
end;}

procedure TsmxCustomPage.Prepare(Forcibly: Boolean = False);
var i: Integer;
begin
  for i := 0 to SectionCount - 1 do
    Sections[i].Prepare(Forcibly);
end;

{ TsmxCustomPageManager }

constructor TsmxCustomPageManager.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPageList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomPageManager.Destroy;
begin
  DestroyChilds;
  FPageList.Free;
  inherited Destroy;
end;

procedure TsmxCustomPageManager.Apply;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    Pages[i].Apply;
end;

procedure TsmxCustomPageManager.DestroyChilds;
var i: Integer;
begin
  for i := FPageList.Count - 1 downto 0 do
  begin
    TsmxCustomPage(FPageList[i]).Free;
    FPageList.Delete(i);
  end;
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

procedure TsmxCustomPageManager.InstallParent;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    Pages[i].ParentCell := Self;
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

procedure TsmxCustomPageManager.Prepare(Forcibly: Boolean = False);
var p: TsmxCustomPage;
begin
  p := ActivePage;
  if Assigned(p) then
    p.Prepare(Forcibly);
end;

{ TsmxCustomMenuPoint }

procedure TsmxCustomMenuPoint.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
end;

procedure TsmxCustomMenuPoint.DelAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
end;

{ TsmxCustomMasterMenu }

constructor TsmxCustomMasterMenu.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FMenuPointList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomMasterMenu.Destroy;
begin
  DestroyChilds;
  FMenuPointList.Free;
  inherited Destroy;
end;

procedure TsmxCustomMasterMenu.DestroyChilds;
var i: Integer;
begin
  for i := FMenuPointList.Count - 1 downto 0 do
  begin
    TsmxCustomMenuPoint(FMenuPointList[i]).Free;
    FMenuPointList.Delete(i);
  end;
end;

function TsmxCustomMasterMenu.FindMenuPointByCfgID(ACfgID: Integer): TsmxCustomMenuPoint;
var i: Integer;
begin
  Result := nil;
  for i := 0 to MenuPointCount - 1 do
    if MenuPoints[i].CfgID = ACfgID then
    begin
      Result := MenuPoints[i];
      Break;
    end;
end;

function TsmxCustomMasterMenu.GetMenuPoint(Index: Integer): TsmxCustomMenuPoint;
begin
  Result := TsmxCustomMenuPoint(FMenuPointList[Index]);
end;

function TsmxCustomMasterMenu.GetMenuPointCount: Integer;
begin
  Result := FMenuPointList.Count;
end;

function TsmxCustomMasterMenu.MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint;
begin
  Result := nil;
end;

procedure TsmxCustomMasterMenu.InstallParent;
var i: Integer; mp: TsmxCustomMenuPoint;
begin
  for i := 0 to MenuPointCount - 1 do
  begin
    mp := MenuPointParent(MenuPoints[i].CfgID);
    if Assigned(mp) then
      MenuPoints[i].ParentCell := mp else
      MenuPoints[i].ParentCell := Self;
  end;
end;

{procedure TsmxCustomMasterMenu.UnInstallParent;
var i: Integer;
begin
  for i := 0 to MenuPointCount - 1 do
    MenuPoints[i].ParentCell := nil;
end;}

{ TsmxCustomToolBoard }

procedure TsmxCustomToolBoard.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
end;

procedure TsmxCustomToolBoard.DelAlgorithm(Algorithm: TsmxCustomAlgorithm);
begin
end;

{ TsmxCustomControlBoard }

constructor TsmxCustomControlBoard.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FToolBoardList := TList.Create;
  CreateChilds;
  InitChilds;
end;

destructor TsmxCustomControlBoard.Destroy;
begin
  DestroyChilds;
  FToolBoardList.Free;
  inherited Destroy;
end;

procedure TsmxCustomControlBoard.DestroyChilds;
var i: Integer;
begin
  for i := FToolBoardList.Count - 1 downto 0 do
  begin
    TsmxCustomToolBoard(FToolBoardList[i]).Free;
    FToolBoardList.Delete(i);
  end;
end;

function TsmxCustomControlBoard.FindToolBoardByCfgID(ACfgID: Integer): TsmxCustomToolBoard;
var i: Integer;
begin
  Result := nil;
  for i := 0 to ToolBoardCount - 1 do
    if ToolBoards[i].CfgID = ACfgID then
    begin
      Result := ToolBoards[i];
      Break;
    end;
end;

function TsmxCustomControlBoard.GetToolBoard(Index: Integer): TsmxCustomToolBoard;
begin
  Result := TsmxCustomToolBoard(FToolBoardList[Index]);
end;

function TsmxCustomControlBoard.GetToolBoardCount: Integer;
begin
  Result := FToolBoardList.Count;
end;

procedure TsmxCustomControlBoard.InstallParent;
var i: Integer;
begin
  for i := 0 to ToolBoardCount - 1 do
    ToolBoards[i].ParentCell := Self;
end;

{procedure TsmxCustomControlBoard.UnInstallParent;
var i: Integer;
begin
  for i := 0 to ToolBoardCount - 1 do
    ToolBoards[i].ParentCell := nil;
end;}

procedure TsmxCustomControlBoard.Prepare(Forcibly: Boolean = False);
var i: Integer;
begin
  for i := 0 to ToolBoardCount - 1 do
    ToolBoards[i].Prepare(Forcibly);
end;

{ TsmxCustomForm }

constructor TsmxCustomForm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FID := AID;
  FPageManagerList := TList.Create;
  CreateChilds;
  InitChilds;
end;

constructor TsmxCustomForm.CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID, AIntfID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FIntfID := AIntfID;
  FID := AID;
  FStateCfg := TsmxStateCfg.CreateByIntfID(Self, CfgDatabase, CfgID, FIntfID);
  FStateCfg.Initialize;
  FPageManagerList := TList.Create;
  CreateChilds; 
  InitChilds;
end;

destructor TsmxCustomForm.Destroy;
begin
  DestroyChilds;
  FPageManagerList.Free;
  if Assigned(FStateCfg) then
    FStateCfg.Free;
  inherited Destroy;
end;

procedure TsmxCustomForm.Apply;
var i: Integer;
begin
  for i := 0 to PageManagerCount - 1 do
    PageManagers[i].Apply;
  if Assigned(MasterMenu) then
    MasterMenu.Apply;
  if Assigned(ControlBoard) then
    ControlBoard.Apply;
end;

procedure TsmxCustomForm.DestroyChilds;
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
end;

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
var ID: Integer; //f: IsmxField;
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
  if Assigned(FStateRequest) then
  begin
    FStateRequest.Perform;
    ID := FieldSenseValueDef(FStateRequest, fsKey, 0);
  end else
    ID := 0;
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

procedure TsmxCustomForm.InstallParent;
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
end;

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
    cs := FStateCfg.CellStates.FindByID(FStateID);
  if Assigned(cs) then
    for i := 0 to cs.StateUnits.Root.Count - 1 do
      PutCell(cs.StateUnits.Root[i], Self);
end;

procedure TsmxCustomForm.SetFormManager(Value: TsmxCustomFormManager);
begin
  FFormManager := Value;
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

procedure TsmxCustomForm.Prepare(Forcibly: Boolean = False);
var i: Integer;
begin
  ChangeState;
  //inherited Prepare(Forcibly);
  for i := 0 to PageManagerCount - 1 do
    PageManagers[i].Prepare(Forcibly);
  if Assigned(MasterMenu) then
    MasterMenu.Prepare(Forcibly);
  if Assigned(ControlBoard) then
    ControlBoard.Prepare(Forcibly);
end;

{ TsmxCustomMasterForm }

procedure TsmxCustomMasterForm.SetForm(Value: TForm);
begin
  FForm := Value;
end;

end.

