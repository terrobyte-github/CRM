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
  Classes, Controls, Graphics, smxBaseClasses, smxClasses, smxDBIntf, smxTypes;

type
  { TsmxRequestField }

  TsmxRequestFields = class;

  TsmxRequestField = class(TsmxKitItem)
  private
    FDisplayFormat: String;
    FFieldName: String;
    FFieldSense: TsmxFieldSense;
    function GetKit: TsmxRequestFields;
    procedure SetKit(Value: TsmxRequestFields);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    property FieldName: String read FFieldName write FFieldName;
    property FieldSense: TsmxFieldSense read FFieldSense write FFieldSense;
    property Kit: TsmxRequestFields read GetKit write SetKit;
  end;

  { TsmxRequestFields }

  TsmxRequestFields = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxRequestField;
    procedure SetItem(Index: Integer; Value: TsmxRequestField);
  public
    function Add: TsmxRequestField;
    function FindByName(const AFieldName: String): TsmxRequestField;

    property Items[Index: Integer]: TsmxRequestField read GetItem write SetItem; default;
  end;

  { TsmxRequestParam }

  TsmxRequestParams = class;

  TsmxRequestParam = class(TsmxKitItem)
  private
    FDataType: TsmxDataType;
    FNumericScale: Integer;
    FPrecision: Integer;
    FSize: Integer;
    FParamName: String;
    FParamLocation: TsmxParamLocation;
    FParamType: TsmxParamType;
    FValue: Variant;
    function GetKit: TsmxRequestParams;
    procedure SetKit(Value: TsmxRequestParams);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property DataType: TsmxDataType read FDataType write FDataType;
    property Kit: TsmxRequestParams read GetKit write SetKit;
    property NumericScale: Integer read FNumericScale write FNumericScale;
    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamName: String read FParamName write FParamName;
    property ParamType: TsmxParamType read FParamType write FParamType;
    property Precision: Integer read FPrecision write FPrecision;
    property Size: Integer read FSize write FSize;
    property Value: Variant read FValue write FValue;
  end;

  { TsmxRequestParams }

  TsmxRequestParams = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxRequestParam;
    procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxRequestParam;
    function FindByName(const AParamName: String): TsmxRequestParam;

    property Items[Index: Integer]: TsmxRequestParam read GetItem write SetItem; default;
  end;

  { TsmxRequestCfg }

  TsmxRequestCfg = class(TsmxBaseCfg)
  private
    FDeleteAlgCfgID: Integer;
    FDeleteReqCfgID: Integer;
    FExecureAlgCfgID: Integer;
    FFields: TsmxRequestFields;
    FInsertAlgCfgID: Integer;
    FInsertReqCfgID: Integer;
    FParams: TsmxRequestParams;
    FPerformanceMode: TsmxPerformanceMode;
    FPrepareAlgCfgID: Integer;
    FRefreshParamsAlgCfgID: Integer;
    FSQLText: String;
    FUpdateAlgCfgID: Integer;
    FUpdateReqCfgID: Integer;
    function GetFields: TsmxRequestFields;
    function GetParams: TsmxRequestParams;
  protected
    procedure SetDeleteAlgCfgID(Value: Integer); virtual;
    procedure SetDeleteReqCfgID(Value: Integer); virtual;
    procedure SetExecuteAlgCfgID(Value: Integer); virtual;
    procedure SetFields(Value: TsmxRequestFields); virtual;
    procedure SetInsertAlgCfgID(Value: Integer); virtual;
    procedure SetInsertReqCfgID(Value: Integer); virtual;
    procedure SetParams(Value: TsmxRequestParams); virtual;
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;
    procedure SetPrepareAlgCfgID(Value: Integer); virtual;
    procedure SetRefreshParamsAlgCfgID(Value: Integer); virtual;
    procedure SetSQLText(const Value: String); virtual;
    procedure SetUpdateAlgCfgID(Value: Integer); virtual;
    procedure SetUpdateReqCfgID(Value: Integer); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property DeleteAlgCfgID: Integer read FDeleteAlgCfgID write SetDeleteAlgCfgID;
    property DeleteReqCfgID: Integer read FDeleteReqCfgID write SetDeleteReqCfgID;
    property ExecuteAlgCfgID: Integer read FExecureAlgCfgID write SetExecuteAlgCfgID;
    property Fields: TsmxRequestFields read GetFields write SetFields;
    property InsertAlgCfgID: Integer read FInsertAlgCfgID write SetInsertAlgCfgID;
    property InsertReqCfgID: Integer read FInsertReqCfgID write SetInsertReqCfgID;
    property Params: TsmxRequestParams read GetParams write SetParams;
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write SetPerformanceMode;
    property PrepareAlgCfgID: Integer read FPrepareAlgCfgID write SetPrepareAlgCfgID;
    property RefreshParamsAlgCfgID: Integer read FRefreshParamsAlgCfgID write SetRefreshParamsAlgCfgID;
    property SQLText: String read FSQLText write SetSQLText;
    property UpdateAlgCfgID: Integer read FUpdateAlgCfgID write SetUpdateAlgCfgID;
    property UpdateReqCfgID: Integer read FUpdateReqCfgID write SetUpdateReqCfgID;
  end;

  { TsmxSimpleKitItem }

  TsmxSimpleKit = class;

  TsmxSimpleKitItem = class(TsmxKitItem)
  private
    FCfgID: Integer;
    function GetKit: TsmxSimpleKit;
    procedure SetKit(Value: TsmxSimpleKit);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property CfgID: Integer read FCfgID write FCfgID;
    property Kit: TsmxSimpleKit read GetKit write SetKit;
  end;

  { TsmxSimpleKit }

  TsmxSimpleKit = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxSimpleKitItem;
    procedure SetItem(Index: Integer; Value: TsmxSimpleKitItem);
  public
    function Add: TsmxSimpleKitItem;

    property Items[Index: Integer]: TsmxSimpleKitItem read GetItem write SetItem; default;
  end;

  { TsmxRequestKitItem }

  TsmxRequestKit = class;

  TsmxRequestKitItem = class(TsmxSimpleKitItem)
  private
    FDatabaseName: String;
    FOperationMode: TsmxOperationMode;
    function GetKit: TsmxRequestKit;
    procedure SetKit(Value: TsmxRequestKit);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property Kit: TsmxRequestKit read GetKit write SetKit;
    property OperationMode: TsmxOperationMode read FOperationMode write FOperationMode;
  end;

  { TsmxRequestKit }

  TsmxRequestKit = class(TsmxSimpleKit)
  private
    function GetItem(Index: Integer): TsmxRequestKitItem;
    procedure SetItem(Index: Integer; Value: TsmxRequestKitItem);
  public
    function Add: TsmxRequestKitItem;

    property Items[Index: Integer]: TsmxRequestKitItem read GetItem write SetItem; default;
  end;

  { TsmxRequestListCfg }

  TsmxRequestListCfg = class(TsmxBaseCfg)
  private
    FRequests: TsmxRequestKit;
    function GetRequests: TsmxRequestKit;
  protected
    procedure SetRequests(Value: TsmxRequestKit); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property Requests: TsmxRequestKit read GetRequests write SetRequests;
  end;

  { TsmxControlKitItem }

  TsmxControlKit = class;

  TsmxControlKitItem = class(TsmxSimpleKitItem)
  private
    FItemActive: Boolean;
    FItemAlign: TAlign;
    FItemCursor: TCursor;
    FItemEnable: Boolean;
    FItemHeight: Integer;
    FItemLeft: Integer;
    FItemTop: Integer;
    FItemVisible: Boolean;
    FItemWidth: Integer;
    FPopupMenu: Integer;
    function GetKit: TsmxControlKit;
    procedure SetKit(Value: TsmxControlKit);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property ItemActive: Boolean read FItemActive write FItemActive;
    property ItemAlign: TAlign read FItemAlign write FItemAlign;
    property ItemCursor: TCursor read FItemCursor write FItemCursor;
    property ItemEnable: Boolean read FItemEnable write FItemEnable;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ItemLeft: Integer read FItemLeft write FItemLeft;
    property ItemTop: Integer read FItemTop write FItemTop;
    property ItemVisible: Boolean read FItemVisible write FItemVisible;
    property ItemWidth: Integer read FItemWidth write FItemWidth;
    property Kit: TsmxControlKit read GetKit write SetKit;
    property PopupMenuCfgID: Integer read FPopupMenu write FPopupMenu;
  end;

  { TsmxControlKit }

  TsmxControlKit = class(TsmxSimpleKit)
  private
    function GetItem(Index: Integer): TsmxControlKitItem;
    procedure SetItem(Index: Integer; Value: TsmxControlKitItem);
  public
    function Add: TsmxControlKitItem;

    property Items[Index: Integer]: TsmxControlKitItem read GetItem write SetItem; default;
  end;

  { TsmxControlCfg }

  TsmxControlCfg = class(TsmxBaseCfg)
  private
    FApplyAlgCfgID: Integer;
    FBackupAlgCfgID: Integer;
    FChangeActiveControlAlgCfgID: Integer;
    FCfgCursor: TCursor;
    FCfgHeight: Integer;
    FCfgWidth: Integer;
    FChildCells: TsmxControlKit;
    FPrepareAlgCfgID: Integer;
    FRestoreAlgCfgID: Integer;
    function GetChildCells: TsmxControlKit;
  protected
    procedure SetApplyAlgCfgID(Value: Integer); virtual;
    procedure SetBackupAlgCfgID(Value: Integer); virtual;
    procedure SetCfgCursor(Value: TCursor); virtual;
    procedure SetCfgHeight(Value: Integer); virtual;
    procedure SetCfgWidth(Value: Integer); virtual;
    procedure SetChangeActiveControlAlgCfgID(Value: Integer); virtual;
    procedure SetChildCells(Value: TsmxControlKit); virtual;
    procedure SetPrepareAlgCfgID(Value: Integer); virtual;
    procedure SetRestoreAlgCfgID(Value: Integer); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property ApplyAlgCfgID: Integer read FApplyAlgCfgID write SetApplyAlgCfgID;
    property BackupAlgCfgID: Integer read FBackupAlgCfgID write SetBackupAlgCfgID;
    property CfgCursor: TCursor read FCfgCursor write SetCfgCursor;
    property CfgHeight: Integer read FCfgHeight write SetCfgHeight;
    property CfgWidth: Integer read FCfgWidth write SetCfgWidth;
    property ChangeActiveControlAlgCfgID: Integer read FChangeActiveControlAlgCfgID write SetChangeActiveControlAlgCfgID;
    property ChildCells: TsmxControlKit read GetChildCells write SetChildCells;
    property PrepareAlgCfgID: Integer read FPrepareAlgCfgID write SetPrepareAlgCfgID;
    property RestoreAlgCfgID: Integer read FRestoreAlgCfgID write SetRestoreAlgCfgID;
  end;

  { TsmxControlCfg }

  TsmxActionCfg = class(TsmxControlCfg)
  private
    FAlgorithmCfgID: Integer;
    FCfgCaption: String;
    FCfgHint: String;
    FCfgHotKey: Integer;
    FCfgImageIndex: Integer;
    FExecuteAlgCfgID: Integer;
  protected
    procedure SetAlgorithmCfgID(Value: Integer); virtual;
    procedure SetCfgCaption(const Value: String); virtual;
    procedure SetCfgHint(const Value: String); virtual;
    procedure SetCfgHotKey(Value: Integer); virtual;
    procedure SetCfgImageIndex(Value: Integer); virtual;
    procedure SetExecuteAlgCfgID(Value: Integer); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property AlgorithmCfgID: Integer read FAlgorithmCfgID write SetAlgorithmCfgID;
    property CfgCaption: String read FCfgCaption write SetCfgCaption;
    property CfgHint: String read FCfgHint write SetCfgHint;
    property CfgHotKey: Integer read FCfgHotKey write SetCfgHotKey;
    property CfgImageIndex: Integer read FCfgImageIndex write SetCfgImageIndex default -1;
    property ExecuteAlgCfgID: Integer read FExecuteAlgCfgID write SetExecuteAlgCfgID;
  end;

  { TsmxColumnCfg }

  TsmxColumnCfg = class(TsmxControlCfg)
  private
    FColumnFieldName: String;
    FColumnText: TsmxCellText;
    FColumnTitle: TsmxCellText;
    FPressHeaderAlgCfgID: Integer;
  protected
    procedure SetColumnFieldName(const Value: String); virtual;
    procedure SetColumnText(Value: TsmxCellText); virtual;
    procedure SetColumnTitle(Value: TsmxCellText); virtual;
    procedure SetPressHeaderAlgCfgID(Value: Integer); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property ColumnFieldName: String read FColumnFieldName write SetColumnFieldName;
    property ColumnText: TsmxCellText read FColumnText write SetColumnText;
    property ColumnTitle: TsmxCellText read FColumnTitle write SetColumnTitle;
    property PressHeaderAlgCfgID: Integer read FPressHeaderAlgCfgID write SetPressHeaderAlgCfgID;
  end;

  { TsmxGridCfg }

  TsmxGridCfg = class(TsmxControlCfg)
  private
    FChangeRowAlgCfgID: Integer;
    FGridOptions: TsmxGridOptions;
    FPressDoubleAlgCfgID: Integer;
    FRequestCfgID: Integer;
  protected
    procedure SetChangeRowAlgCfgID(Value: Integer); virtual;
    procedure SetGridOptions(Value: TsmxGridOptions); virtual;
    procedure SetPressDoubleAlgCfgID(Value: Integer); virtual;
    procedure SetRequestCfgID(Value: Integer); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property ChangeRowAlgCfgID: Integer read FChangeRowAlgCfgID write SetChangeRowAlgCfgID;
    property GridOptions: TsmxGridOptions read FGridOptions write SetGridOptions;
    property PressDoubleAlgCfgID: Integer read FPressDoubleAlgCfgID write SetPressDoubleAlgCfgID;
    property RequestCfgID: Integer read FRequestCfgID write SetRequestCfgID;
  end;

  { TsmxAlgorithmCfg }

  TsmxAlgorithmCfg = class(TsmxBaseCfg)
  private
    FAlgorithmCaption: String;
    FAlgorithmCells: TsmxSimpleKit;
    FAlgorithmHint: String;
    FAlgorithmHotKey: Integer;
    FAlgorithmImageIndex: Integer;
    FAlgorithmParams: TsmxAlgorithmParams;
    FRefreshParamsCfgID: Integer;
    function GetAlgorithmCells: TsmxSimpleKit;
    function GetAlgorithmParams: TsmxAlgorithmParams;
  protected
    procedure SetAlgorithmCaption(const Value: String); virtual;
    procedure SetAlgorithmCells(Value: TsmxSimpleKit); virtual;
    procedure SetAlgorithmHint(const Value: String); virtual;
    procedure SetAlgorithmHotKey(Value: Integer); virtual;
    procedure SetAlgorithmImageIndex(Value: Integer); virtual;
    procedure SetAlgorithmParams(Value: TsmxAlgorithmParams); virtual;
    procedure SetRefreshParamsCfgID(Value: Integer); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property AlgorithmCaption: String read FAlgorithmCaption write SetAlgorithmCaption;
    property AlgortihmCells: TsmxSimpleKit read GetAlgorithmCells write SetAlgorithmCells;
    property AlgorithmHint: String read FAlgorithmHint write SetAlgorithmHint;
    property AlgorithmHotKey: Integer read FAlgorithmHotKey write SetAlgorithmHotKey;
    property AlgorithmImageIndex: Integer read FAlgorithmImageIndex write SetAlgorithmImageIndex;
    property AlgorithmParams: TsmxAlgorithmParams read GetAlgorithmParams write SetAlgorithmParams;
    property RefreshParamsCfgID: Integer read FRefreshParamsCfgID write SetRefreshParamsCfgID;
  end;

  { TsmxLibAlgorithmCfg }

  TsmxLibAlgorithmCfg = class(TsmxAlgorithmCfg)
  private
    FAlgorithmLibrary: String;
    FAlgorithmProcedure: String;
  protected
    procedure SetAlgorithmLibrary(const Value: String); virtual;
    procedure SetAlgorithmProcedure(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property AlgorithmLibrary: String read FAlgorithmLibrary write SetAlgorithmLibrary;
    property AlgorithmProcedure: String read FAlgorithmProcedure write SetAlgorithmProcedure;
  end;

  { TsmxAlgorithmListCfg }

  TsmxAlgorithmListCfg = class(TsmxBaseCfg)
  private
    FAlgorithms: TsmxSimpleKit;
    function GetAlgorithms: TsmxSimpleKit;
  protected
    procedure SetAlgorithms(Value: TsmxSimpleKit); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    //property Algorithms: TsmxSimpleKit read GetAlgorithms write SetAlgorithms;
  end;

  { TsmxFilterCfg }

  TsmxFilterCfg = class(TsmxBaseCfg)
  private
    FDisplayFormat: String;
    FFilterFont: TsmxCellFont;
    FFilterHeader: TsmxCellText;
    FFilterName: String;
    FValueFormat: String;
    FFilter: TsmxCellText;
    procedure SetDisplayFormat(const Value: String);
    procedure SetFilterFont(const Value: TsmxCellText);
    procedure SetFilterHeader(const Value: TsmxCellText);
    procedure SetFilterName(const Value: String);
    procedure SetValueFormat(const Value: String);
  protected
    procedure SetAction(Value: TsmxAlgorithmSetting); virtual;
    procedure SetChange(Value: TsmxAlgorithmSetting); virtual;
  public
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    //property Action: TsmxAlgorithmSetting read FAction write SetAction;
    //property Change: TsmxAlgorithmSetting read FChange write SetChange;
    property DisplayFormat: String read FDisplayFormat write SetDisplayFormat;
    property Filter: TsmxCellText read FFilter write SetFilterFont;
    property FilterHeader: TsmxCellText read FFilterHeader write SetFilterHeader;
    property FilterName: String read FFilterName write SetFilterName;
    property ValueFormat: String read FValueFormat write SetValueFormat;
  end;

  { TsmxFilterDeskCfg }

  TsmxFilterDeskCfg = class(TsmxBaseCfg)
  private
    FApplyRequest: TsmxRequestSetting;
    FFilters: TsmxControlKit;
    FPrepareRequest: TsmxRequestSetting;
    function GetFilters: TsmxControlKit;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property ApplyRequest: TsmxRequestSetting read FApplyRequest write FApplyRequest;
    property Filters: TsmxControlKit read GetFilters;
    property PrepareRequest: TsmxRequestSetting read FPrepareRequest write FPrepareRequest;
  end;

  { TsmxSectionCfg }

  TsmxSectionCfg = class(TsmxBaseCfg)
  private
    FFilterPanel: TsmxControlCellSetting;
    FGrid: TsmxControlCellSetting;
    FRequest: TsmxRequestSetting;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property FilterPanel: TsmxControlCellSetting read FFilterPanel write FFilterPanel;
    property Grid: TsmxControlCellSetting read FGrid write FGrid;
    property Request: TsmxRequestSetting read FRequest write FRequest;
  end;

  { TsmxPageCfg }

  TsmxPageCfg = class(TsmxBaseCfg)
  private
    FPageCaption: String;
    FPageImageIndex: Integer;
    FSections: TsmxControlKit;
    function GetSections: TsmxControlKit;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property PageCaption: String read FPageCaption write FPageCaption;
    property PageImageIndex: Integer read FPageImageIndex write FPageImageIndex;
    property Sections: TsmxControlKit read GetSections;
  end;

  { TsmxPageManagerCfg }

  TsmxPageManagerCfg = class(TsmxBaseCfg)
  private
    FSheets: TsmxControlKit;
    function GetSheets: TsmxControlKit;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property Sheets: TsmxControlKit read GetSheets;
  end;

  { TsmxMenuItemCfg }

  TsmxMenuItemCfg = class(TsmxBaseCfg)
  private
    FItemCaption: String;
    FItemImageIndex: Integer;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property ItemCaption: String read FItemCaption write FItemCaption;
    property ItemImageIndex: Integer read FItemImageIndex write FItemImageIndex;
  end;

  { TsmxHVisibleUnit }

  TsmxHVisibleUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FItemAlign: TAlign;
    FItemEnable: Boolean;
    FItemHeight: Integer;
    FItemLeft: Integer;
    FItemTop: Integer;
    FItemVisible: Boolean;
    FItemWidth: Integer;
    function GetItem(Index: Integer): TsmxHVisibleUnit;
    function GetParent: TsmxHVisibleUnit;
  public
    constructor Create(AHKit: TsmxHKit); override;
    function Add: TsmxHVisibleUnit;
    function FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxHVisibleUnit;

    property CfgID: Integer read FCfgID write FCfgID;
    property Items[Index: Integer]: TsmxHVisibleUnit read GetItem; default;
    property Parent: TsmxHVisibleUnit read GetParent;
    property ItemAlign: TAlign read FItemAlign write FItemAlign;
    property ItemEnable: Boolean read FItemEnable write FItemEnable;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ItemLeft: Integer read FItemLeft write FItemLeft;
    property ItemTop: Integer read FItemTop write FItemTop;
    property ItemVisible: Boolean read FItemVisible write FItemVisible;
    property ItemWidth: Integer read FItemWidth write FItemWidth;
  end;

  { TsmxHVisibleUnits }

  TsmxHVisibleUnits = class(TsmxHKit)
  private
    function GetRoot: TsmxHVisibleUnit;
  public
    property Root: TsmxHVisibleUnit read GetRoot;
  end;

  { TsmxMainMenuCfg }

  TsmxMainMenuCfg = class(TsmxBaseCfg)
  private
    FMenuUnits: TsmxHVisibleUnits;
    function GetMenuUnits: TsmxHVisibleUnits;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property MenuUnits: TsmxHVisibleUnits read GetMenuUnits;
  end;

  { TsmxToolBoardCfg }

  TsmxToolBoardCfg = class(TsmxBaseCfg)
  private
    FBarFlat: Boolean;
    FBarShowCaptions: Boolean;
    FBarShowHint: Boolean;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property BarFlat: Boolean read FBarFlat write FBarFlat;
    property BarShowCaptions: Boolean read FBarShowCaptions write FBarShowCaptions;
    property BarShowHint: Boolean read FBarShowHint write FBarShowHint;
  end;

  { TsmxControlBoardCfg }

  TsmxControlBoardCfg = class(TsmxBaseCfg)
  private
    FBarUnits: TsmxControlKit;
    function GetBarUnits: TsmxControlKit;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property BarUnits: TsmxControlKit read GetBarUnits;
  end;

  { TsmxFormCfg }

  TsmxFormCfg = class(TsmxBaseCfg)
  private
    FAlgorithmList: TsmxAlgorithmListSetting;
    FControlBar: TsmxControlCellSetting;
    FFormCaption: String;
    FFormImageIndex: Integer;
    FFormPositionSize: TsmxPositionSize;
    FMainMenu: TsmxControlCellSetting;
    FStateRequest: TsmxRequestSetting;
    FPageManagers: TsmxControlKit;
    FStatusBar: TsmxControlCellSetting;
    function GetPageManagers: TsmxControlKit;
  //protected
    //procedure Read; override;
    //procedure Write; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property AlgorithmList: TsmxAlgorithmListSetting read FAlgorithmList write FAlgorithmList;
    property ControlBar: TsmxControlCellSetting read FControlBar write FControlBar;
    property FormCaption: String read FFormCaption write FFormCaption;
    property FormImageIndex: Integer read FFormImageIndex write FFormImageIndex;
    property FormPositionSize: TsmxPositionSize read FFormPositionSize write FFormPositionSize;
    property MainMenu: TsmxControlCellSetting read FMainMenu write FMainMenu;
    property PageManagers: TsmxControlKit read GetPageManagers;
    property StateRequest: TsmxRequestSetting read FStateRequest write FStateRequest;
    property StatusBar: TsmxControlCellSetting read FStatusBar write FStatusBar;
  end;

implementation

uses
  XMLIntf, SysUtils, Variants, smxFuncs, smxConsts;

{ TsmxRequestField }

procedure TsmxRequestField.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxRequestField then
  begin
    DisplayFormat := TsmxRequestField(Source).DisplayFormat;
    FieldName := TsmxRequestField(Source).FieldName;
    FieldSense := TsmxRequestField(Source).FieldSense;
  end else
    inherited Assign(Source);
end;

function TsmxRequestField.GetKit: TsmxRequestFields;
begin
  Result := TsmxRequestFields(inherited Kit);
end;

procedure TsmxRequestField.SetKit(Value: TsmxRequestFields);
begin
  inherited Kit := Value;
end;

{ TsmxRequestFields }

function TsmxRequestFields.Add: TsmxRequestField;
begin
  Result := TsmxRequestField(inherited Add);
end;

function TsmxRequestFields.FindByName(const AFieldName: String): TsmxRequestField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].FieldName, AFieldName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxRequestFields.GetItem(Index: Integer): TsmxRequestField;
begin
  Result := TsmxRequestField(inherited Items[Index]);
end;

procedure TsmxRequestFields.SetItem(Value: TsmxRequestField);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestParam }

procedure TsmxRequestParam.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxRequestParam then
  begin
    DataType := TsmxRequestParam(Source).DataType;
    NumericScale := TsmxRequestParam(Source).NumericScale;
    ParamLocation := TsmxRequestParam(Source).ParamLocation;
    ParamName := TsmxRequestParam(Source).ParamName;
    ParamType := TsmxRequestParam(Source).ParamType;
    Precision := TsmxRequestParam(Source).Precision;
    Size := TsmxRequestParam(Source).Size;
    Value := TsmxRequestParam(Source).Value;
  end else
    inherited Assign(Source);
end;

function TsmxRequestParam.GetKit: TsmxRequestParams;
begin
  Result := TsmxRequestParams(inherited Kit);
end;

procedure TsmxRequestParam.SetKit(Value: TsmxRequestParams);
begin
  inherited Kit := Value;
end;

{ TsmxRequestParams }

function TsmxRequestParams.Add: TsmxLocationParam;
begin
  Result := TsmxRequestParam(inherited Add);
end;

function TsmxRequestParams.FindByName(const AParamName: String): TsmxRequestParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxRequestParams.GetItem(Index: Integer): TsmxLocationParam;
begin
  Result := TsmxRequestParam(inherited Items[Index]);
end;

procedure TsmxRequestParams.SetItem(Value: TsmxRequestParam);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestCfg }

destructor TsmxRequestCfg.Destroy;
begin
  if Assigned(FFields) then
    FFields.Free;
  if Assigned(FParams) then
    FParams.Free;
  inherited Destroy;
end;

procedure TsmxRequestCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxRequestCfg then
  begin
    DeleteAlgCfgID := TsmxRequestCfg(Source).DeleteAlgCfgID;
    DeleteReqCfgID := TsmxRequestCfg(Source).DeleteReqCfgID;
    ExecuteAlgCfgID := TsmxRequestCfg(Source).ExecuteAlgCfgID;
    Fields := TsmxRequestCfg(Source).Fields;
    InsertAlgCfgID := TsmxRequestCfg(Source).InsertAlgCfgID;
    InsertReqCfgID := TsmxRequestCfg(Source).InsertReqCfgID;
    Params := TsmxRequestCfg(Source).Params;
    PerformanceMode := TsmxRequestCfg(Source).PerformanceMode;
    PrepareAlgCfgID := TsmxRequestCfg(Source).PrepareAlgCfgID;
    RefreshParamsAlgCfgID := TsmxRequestCfg(Source).RefreshParamsAlgCfgID;
    SQLText := TsmxRequestCfg(Source).SQLText;
    UpdateAlgCfgID := TsmxRequestCfg(Source).UpdateAlgCfgID;
    UpdateReqCfgID := TsmxRequestCfg(Source).UpdateReqCfgID;
  end;
end;

procedure TsmxRequestCfg.Clear;
begin
  DeleteAlgCfgID := 0;
  DeleteReqCfgID := 0;
  ExecureAlgCfgID := 0;
  if Assigned(FFields) then
    FFields.Clear;
  InsertAlgCfgID := 0;
  InsertReqCfgID := 0;
  if Assigned(FParams) then
    FParams.Clear;
  PerformanceMode := pmOpen;
  PrepareAlgCfgID := 0;
  RefreshParamsAlgCfgID := 0;
  SQLText := '';
  UpdateAlgCfgID := 0;
  UpdateReqCfgID := 0;
end;

function TsmxRequestCfg.GetFields: TsmxRequestFields;
begin
  if not Assigned(FFields) then
    FFields := TsmxRequestFields.Create(TsmxRequestField);
  Result := FFields;
end;

procedure TsmxRequestCfg.SetFields(Value: TsmxRequestFields);
begin
  Fields.Assign(Value);
end;

function TsmxRequestCfg.GetParams: TsmxRequestParams;
begin
  if not Assigned(FParams) then
    FParams := TsmxRequestParams.Create(TsmxRequestParam);
  Result := FParams;
end;

procedure TsmxRequestCfg.SetParams(Value: TsmxRequestParams);
begin
  Params.Assign(Value);
end;

procedure TsmxRequestCfg.Read;
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
    PerformanceMode := n.Attributes['Perform'];
  end;

  n := r.ChildNodes.FindNode('Fields');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Field' then
        with Fields.Add do
        begin
          FieldFormat := n.ChildNodes[i].Attributes['Format'];
          FieldName := n.ChildNodes[i].Attributes['Name'];
          FieldSense := n.ChildNodes[i].Attributes['Sense'];
        end;
  end;

  n := r.ChildNodes.FindNode('Params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Param' then
        with Params.Add do
        begin
          DataType := n.ChildNodes[i].Attributes['DataType'];
          NumericScale := n.ChildNodes[i].Attributes['NumericScale'];
          ParamLocation := n.ChildNodes[i].Attributes['Location'];
          ParamName := n.ChildNodes[i].Attributes['Name'];
          ParamType := n.ChildNodes[i].Attributes['ParamType'];
          Precision := n.ChildNodes[i].Attributes['Precision'];
          Size := n.ChildNodes[i].Attributes['Size'];
          Value := n.ChildNodes[i].Attributes['Value'];
        end;
  end;

  n := r.ChildNodes.FindNode('Modify');
  if Assigned(n) then
  begin
    DeleteReqCfgID := n.Attributes['DeleteReqCfgID'];
    InsertReqCfgID := n.Attributes['InsertReqCfgID'];
    UpdateReqCfgID := n.Attributes['UpdateReqCfgID'];
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
  begin
    DeleteAlgCfgID := n.Attributes['DeleteAlgCfgID'];
    ExecuteAlgCfgID := n.Attributes['ExecuteAlgCfgID'];
    InsertAlgCfgID := n.Attributes['InsertAlgCfgID'];
    PrepareAlgCfgID := n.Attributes['PrepareAlgCfgID'];
    RefreshParamsAlgCfgID := n.Attributes['RefreshParamsAlgCfgID'];
    UpdateAlgCfgID := n.Attributes['UpdateAlgCfgID'];
  end;
end;

procedure TsmxRequestCfg.Write;
var
  r, n: IXMLNode;
  i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Request');
  n.Attributes['SQLText'] := SQLText;
  n.Attributes['Type'] := DataSetType;
  n.Attributes['Perform'] := PerformanceMode;

  n := r.AddChild('Fields');
  for i := 0 to Fields.Count - 1 do
    with n.AddChild('Field') do
    begin
      Attributes['Format'] := Fields[i].FieldFormat;
      Attributes['Name'] := Fields[i].FieldName;
      Attributes['Sense'] := Fields[i].FieldSense;
    end;

  n := r.AddChild('Params');
  for i := 0 to Params.Count - 1 do
    with n.AddChild('Param') do
    begin
      Attributes['DataType'] := Params[i].DataType;
      Attributes['NumericScale'] := Params[i].NumericScale;
      Attributes['Location'] := Params[i].ParamLocation;
      Attributes['Name'] := Params[i].ParamName;
      Attributes['ParamType'] := Params[i].ParamType;
      Attributes['Precision'] := Params[i].Precision;
      Attributes['Size'] := Params[i].Size;
      Attributes['Value'] := Params[i].Value;
    end;

  n := r.AddChild('Modify');
  if Assigned(n) then
  begin
    n.Attributes['DeleteReqCfgID'] := DeleteReqCfgID;
    n.Attributes['InsertReqCfgID'] := InsertReqCfgID;
    n.Attributes['UpdateReqCfgID'] := UpdateReqCfgID;
  end;

  n := r.AddChild('Event');
  if Assigned(n) then
  begin
    n.Attributes['DeleteAlgCfgID'] := DeleteAlgCfgID;
    n.Attributes['ExecuteAlgCfgID'] := ExecuteAlgCfgID;
    n.Attributes['InsertAlgCfgID'] := InsertAlgCfgID;
    n.Attributes['PrepareAlgCfgID'] := PrepareAlgCfgID;
    n.Attributes['RefreshParamsAlgCfgID'] := RefreshParamsAlgCfgID;
    n.Attributes['UpdateAlgCfgID'] := UpdateAlgCfgID;
  end;
end;

procedure TsmxRequestCfg.SetDeleteAlgCfgID(Value: Integer);
begin
  FDeleteAlgCfgID := Value;
end;

procedure TsmxRequestCfg.SetDeleteReqCfgID(Value: Integer);
begin
  FDeleteReqCfgID := Value;
end;

procedure TsmxRequestCfg.SetExecuteAlgCfgID(Value: Integer);
begin
  FExecureAlgCfgID := Value;
end;

procedure TsmxRequestCfg.SetInsertAlgCfgID(Value: Integer);
begin
  FInsertAlgCfgID := Value;
end;

procedure TsmxRequestCfg.SetInsertReqCfgID(Value: Integer);
begin
  FInsertReqCfgID := Value;
end;

procedure TsmxRequestCfg.SetPerformanceMode(Value: TsmxPerformanceMode);
begin
  FPerformanceMode := Value;
end;

procedure TsmxRequestCfg.SetPrepareAlgCfgID(Value: Integer);
begin
  FPrepareAlgCfgID := Value;
end;

procedure TsmxRequestCfg.SetRefreshParamsAlgCfgID(Value: Integer);
begin
  FRefreshParamsAlgCfgID := Value;
end;

procedure TsmxRequestCfg.SetSQLText(const Value: String);
begin
  FSQLText := Value;
end;

procedure TsmxRequestCfg.SetUpdateAlgCfgID(Value: Integer);
begin
  FUpdateAlgCfgID := Value;
end;

procedure TsmxRequestCfg.SetUpdateReqCfgID(Value: Integer);
begin
  FUpdateReqCfgID := Value;
end;

{ TsmxSimpleKitItem }

procedure TsmxSimpleKitItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxSimpleKitItem then
    CfgID := TsmxSimpleKitItem(Source).CfgID
  else
    inherited Assign(Source);
end;

function TsmxSimpleKitItem.GetKit: TsmxSimpleKit;
begin
  Result := TsmxSimpleKit(inherited Kit);
end;

procedure TsmxSimpleKitItem.SetKit(Value: TsmxSimpleKit);
begin
  inherited Kit := Value;
end;

{ TsmxSimpleKit }

function TsmxSimpleKit.Add: TsmxSimpleKitItem;
begin
  Result := TsmxSimpleKitItem(inherited Add);
end;

function TsmxSimpleKit.GetItem(Index: Integer): TsmxSimpleKitItem;
begin
  Result := TsmxSimpleKitItem(inherited Items[Index]);
end;

procedure TsmxSimpleKit.SetItem(Index: Integer; Value: TsmxSimpleKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestKitItem }

procedure TsmxRequestKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxRequestKitItem then
  begin
    DatabaseName := TsmxRequestKitItem(Source).DatabaseName;
    OperationMode := TsmxRequestKitItem(Source).OperationMode;
  end;
end;

function TsmxRequestKitItem.GetKit: TsmxRequestKit;
begin
  Result := TsmxRequestKit(inherited Kit);
end;

procedure TsmxRequestKitItem.SetKit(Value: TsmxRequestKit);
begin
  inherited Kit := Value;
end;

{ TsmxRequestKit }

function TsmxRequestKit.Add: TsmxRequestKitItem;
begin
  Result := TsmxRequestKitItem(inherited Add);
end;

function TsmxRequestKit.GetItem(Index: Integer): TsmxRequestKitItem;
begin
  Result := TsmxRequestKitItem(inherited Items[Index]);
end;

procedure TsmxRequestKit.SetItem(Index: Integer; Value: TsmxRequestKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestListCfg }

destructor TsmxRequestListCfg.Destroy;
begin
  if Assigned(FRequests) then
    FRequests.Free;
  inherited Destroy;
end;

procedure TsmxRequestListCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxRequestListCfg then
    Requests := TsmxRequestListCfg(Source).Requests;
end;

procedure TsmxRequestListCfg.Clear;
begin
  if Assigned(FRequests) then
    FRequests.Clear;
end;

function TsmxRequestListCfg.GetRequests: TsmxRequesKit;
begin
  if not Assigned(FRequests) then
    FRequests := TsmxRequestKit.Create(TsmxRequestKitItem);
  Result := FRequests;
end;

procedure TsmxRequestListCfg.SetRequests(Value: TsmxRequestKit);
begin
  Requests.Assign(Value);
end;

procedure TsmxRequestListCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Requests');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Request' then
        with Requests.Add do
        begin
          DatabaseName := n.ChildNodes[i].Attributes['DatabaseName'];
          OperationMode := n.ChildNodes[i].Attributes['OperationMode'];
        end;
  end;
end;

procedure TsmxRequestListCfg.Write;
var
  r, n: IXMLNode;
  i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Requests');
  for i := 0 to Requests.Count - 1 do
    with n.AddChild('Request') do
    begin
      Attributes['DatabaseName'] := Requests[i].DatabaseName;
      Attributes['OperationMode'] := Requests[i].OperationMode;
    end;
end;

{ TsmxControlKitItem }

procedure TsmxControlKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxControlKitItem then
  begin
    ItemActive := TsmxControlKitItem(Source).ItemActive;
    ItemAlign := TsmxControlKitItem(Source).ItemAlign;
    ItemCursor := TsmxControlKitItem(Source).ItemCursor;
    ItemEnable := TsmxControlKitItem(Source).ItemEnable;
    ItemHeight := TsmxControlKitItem(Source).ItemHeight;
    ItemLeft := TsmxControlKitItem(Source).ItemLeft;
    ItemTop := TsmxControlKitItem(Source).ItemTop;
    ItemVisible := TsmxControlKitItem(Source).ItemVisible;
    ItemWidth := TsmxControlKitItem(Source).ItemWidth;
    PopupMenuCfgID := TsmxControlKitItem(Source).PopupMenuCfgID;
  end;
end;

function TsmxControlKitItem.GetKit: TsmxControlKit;
begin
  Result := TsmxControlKit(inherited Kit);
end;

procedure TsmxControlKitItem.SetKit(Value: TsmxControlKit);
begin
  inherited Kit := Value;
end;

{ TsmxControlKit }

function TsmxControlKit.Add: TsmxControlKitItem;
begin
  Result := TsmxControlKitItem(inherited Add);
end;

function TsmxControlKit.GetItem(Index: Integer): TsmxControlKitItem;
begin
  Result := TsmxControlKitItem(inherited Items[Index]);
end;

procedure TsmxControlKit.SetItem(Index: Integer; Value: TsmxControlKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxControlCfg }

destructor TsmxControlCfg.Destroy;
begin
  if Assigned(FChildCells) then
    FChildCells.Free;
  inherited Destroy;
end;

procedure TsmxControlCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxControlCfg then
  begin
    ApplyAlgCfgID := TsmxControlCfg(Source).ApplyAlgCfgID;
    BackupAlgCfgID := TsmxControlCfg(Source).BackupAlgCfgID;
    CfgCursor := TsmxControlCfg(Source).CfgCursor;
    CfgHeight := TsmxControlCfg(Source).CfgHeight;
    CfgWidth := TsmxControlCfg(Source).CfgWidth;
    ChangeActiveControlAlgCfgID := TsmxControlCfg(Source).ChangeActiveControlAlgCfgID;
    ChildCells := TsmxControlCfg(Source).ChildCells;
    PrepareAlgCfgID := TsmxControlCfg(Source).PrepareAlgCfgID;
    RestoreAlgCfgID := TsmxControlCfg(Source).RestoreAlgCfgID;
  end;
end;

procedure TsmxControlCfg.Clear;
begin
  ApplyAlgCfgID := 0;
  BackupAlgCfgID := 0;
  CfgCursor := Controls.crDefault;
  CfgHeight := 0;
  CfgWidth := 0;
  ChangeActiveControlAlgCfgID := 0;
  if Assigned(FChildCells) then
    FChildCells.Clear;
  PrepareAlgCfgID := 0;
  RestoreAlgCfgID := 0;
end;

function TsmxControlCfg.GetChildCells: TsmxControlKit;
begin
  if not Assigned(FChildCells) then
    FChildCells := TsmxControlKit.Create(TsmxControlKitItem);
  Result := FChildCells;
end;

procedure TsmxControlCfg.SetChildCells(Value: TsmxControlKit);
begin
  ChildCells.Assign(Value);
end;

procedure TsmxControlCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    CfgCursor := n.Attributes['Cursor'];
    CfgHeight := n.Attributes['Height'];
    CfgWidth := n.Attributes['Width'];
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
  begin
    ApplyAlgCfgID := n.Attributes['ApplyAlgCfgID'];
    BackupAlgCfgID := n.Attributes['BackupAlgCfgID'];
    ChangeActiveControlAlgCfgID := n.Attributes['ChangeActiveControlAlgCfgID'];
    PrepareAlgCfgID := n.Attributes['PrepareAlgCfgID'];
    RestoreAlgCfgID := n.Attributes['RestoreAlgCfgID'];
  end;

  n := r.ChildNodes.FindNode('ChildCells');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'ChildControl' then
        with ChildCells.Add do
        begin
          ItemActive := n.ChildNodes[i].Attributes['Active'];
          ItemAlign := n.ChildNodes[i].Attributes['Align'];
          ItemCursor := n.ChildNodes[i].Attributes['Cursor'];
          ItemEnable := n.ChildNodes[i].Attributes['Enable'];
          ItemHeight := n.ChildNodes[i].Attributes['Height'];
          ItemLeft := n.ChildNodes[i].Attributes['Left'];
          ItemTop := n.ChildNodes[i].Attributes['Top'];
          ItemVisible := n.ChildNodes[i].Attributes['Visible'];
          ItemWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxControlCfg.Write;
var
  r, n: IXMLNode;
  i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Cell');
  n.Attributes['Cursor'] := CfgCursor;
  n.Attributes['Height'] := CfgHeight;
  n.Attributes['Width'] := CfgWidth;

  n := r.AddChild('Event');
  n.Attributes['ApplyAlgCfgID'] := ApplyAlgCfgID;
  n.Attributes['BackupAlgCfgID'] := BackupAlgCfgID;
  n.Attributes['ChangeActiveControlAlgCfgID'] := ChangeActiveControlAlgCfgID;
  n.Attributes['PrepareAlgCfgID'] := PrepareAlgCfgID;
  n.Attributes['RestoreAlgCfgID'] := RestoreAlgCfgID;

  n := r.AddChild('ChildCells');
  for i := 0 to ChildCells.Count - 1 do
    with n.AddChild('ChildControl') do
    begin
      Attributes['Active'] := ChildCells[i].ItemActive;
      Attributes['Align'] := ChildCells[i].ItemAlign;
      Attributes['Cursor'] := ChildCells[i].ItemCursor;
      Attributes['Enable'] := ChildCells[i].ItemEnable;
      Attributes['Height'] := ChildCells[i].ItemHeight;
      Attributes['Left'] := ChildCells[i].ItemLeft;
      Attributes['Top'] := ChildCells[i].ItemTop;
      Attributes['Visible'] := ChildCells[i].ItemVisible;
      Attributes['Width'] := ChildCells[i].ItemWidth;
    end;
end;

procedure TsmxControlCfg.SetApplyAlgCfgID(Value: Integer);
begin
  FApplyAlgCfgID := Value;
end;

procedure TsmxControlCfg.SetBackupAlgCfgID(Value: Integer);
begin
  FBackupAlgCfgID := Value;
end;

procedure TsmxControlCfg.SetCfgCursor(Value: TCursor);
begin
  FCfgCursor := Value;
end;

procedure TsmxControlCfg.SetCfgHeight(Value: Integer);
begin
  FCfgHeight := Value;
end;

procedure TsmxControlCfg.SetCfgWidth(Value: Integer);
begin
  FCfgWidth := Value;
end;

procedure TsmxControlCfg.SetChangeActiveControlAlgCfgID(Value: Integer);
begin
  FChangeActiveControlAlgCfgID := Value;
end;

procedure TsmxControlCfg.SetPrepareAlgCfgID(Value: Integer);
begin
  FPrepareAlgCfgID := Value;
end;

procedure TsmxControlCfg.SetRestoreAlgCfgID(Value: Integer);
begin
  FRestoreAlgCfgID := Value;
end;

{ TsmxActionCfg }

procedure TsmxActionCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxActionCfg then
  begin
    AlgorithmCfgID := TsmxActionCfg(Source).AlgorithmCfgID;
    CfgCaption := TsmxActionCfg(Source).CfgCaption;
    CfgHint := TsmxActionCfg(Source).CfgHint;
    CfgHotKey := TsmxActionCfg(Source).CfgHotKey;
    CfgImageIndex := TsmxActionCfg(Source).CfgImageIndex;
    ExecuteAlgCfgID := TsmxActionCfg(Source).ExecuteAlgCfgID;
  end;
end;

procedure TsmxActionCfg.Clear;
begin
  inherited Clear;
  AlgorithmCfgID := 0;
  CfgCaption := ''
  CfgHint := '';
  CfgHotKey := 0;
  CfgImageIndex := -1;
  ExecuteAlgCfgID := 0;
end;

procedure TsmxActionCfg.Read;
var
  r, n: IXMLNode;
begin
  inherited Read;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    AlgorithmCfgID := n.Attributes['AlgorithmCfgID'];
    CfgCaption := n.Attributes['Caption'];
    CfgHint := n.Attributes['Hint'];
    CfgHotKey := n.Attributes['HotKey'];
    CfgImageIndex := n.Attributes['ImageIndex'];
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
    ExecuteAlgCfgID := n.Attributes['ExecuteAlgCfgID'];
end;

procedure TsmxActionCfg.Write;
var
  r, n: IXMLNode;
begin
  inherited Write;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    n.Attributes['AlgorithmCfgID'] := AlgorithmCfgID;
    n.Attributes['Caption'] := CfgCaption;
    n.Attributes['Hint'] := CfgHint;
    n.Attributes['HotKey'] := CfgHotKey;
    n.Attributes['ImageIndex'] := CfgImageIndex;
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
    n.Attributes['ExecuteAlgCfgID'] := ExecuteAlgCfgID;
end;

procedure TsmxActionCfg.SetAlgorithmCfgID(Value: Integer);
begin
  FAlgorithmCfgID := Value;
end;

procedure TsmxActionCfg.SetCfgCaption(const Value: String);
begin
  FCfgCaption := Value;
end;

procedure TsmxActionCfg.SetCfgHint(const Value: String);
begin
  FCfgHint := Value;
end;

procedure TsmxActionCfg.SetCfgHotKey(Value: Integer);
begin
  FCfgHotKey := Value;
end;

procedure TsmxActionCfg.SetCfgImageIndex(Value: Integer);
begin
  FCfgImageIndex := Value;
end;

procedure TsmxActionCfg.SetExecuteAlgCfgID(Value: Integer);
begin
  FExecuteAlgCfgID := Value;
end;

{ TsmxColumnCfg }

procedure TsmxColumnCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxColumnCfg then
  begin
    ColumnFieldName := TsmxColumnCfg(Source).ColumnFieldName;
    ColumnText := TsmxColumnCfg(Source).ColumnText;
    ColumnTitle := TsmxColumnCfg(Source).ColumnTitle;
    PressHeaderAlgCfgID := TsmxColumnCfg(Source).PressHeaderAlgCfgID;
  end;
end;

procedure TsmxColumnCfg.Clear;
begin
  ColumnFieldName := '';
  ColumnText := smxFuncs.DefCellText;
  ColumnTitle := smxFuncs.DefCellText;
  ColumnTitle.Color := Integer(Graphics.clBtnFace);
end;

procedure TsmxColumnCfg.Read;
var
  r, n, n2, n3: IXMLNode;
begin
  inherited Read;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    ColumnFieldName := n.Attributes['FieldName'];
    n2 := n.ChildNodes.FindNode('Column');
    if Assigned(n2) then
      with ColumnText do
      begin
        Align := n2.Attributes['Align'];
        Color := n2.Attributes['Color'];
        n3 := n2.ChildNodes.FindNode('Font');
        if Assigned(n3) then
          with Font do
          begin
            Color := n3.Attributes['Color'];
            Name := n3.Attributes['Name'];
            Size := n3.Attributes['Size'];
            Style := TFontStyles(Byte(n3.Attributes['Style']));
          end;
      end;
    n2 := n.ChildNodes.FindNode('Title');
    if Assigned(n2) then
      with ColumnTitle do
      begin
        Caption := n2.Attributes['Caption'];
        Align := n2.Attributes['Align'];
        Color := n2.Attributes['Color'];
        n3 := n2.ChildNodes.FindNode('Font');
        if Assigned(n3) then
          with Font do
          begin
            Color := n3.Attributes['Color'];
            Name := n3.Attributes['Name'];
            Size := n3.Attributes['Size'];
            Style := TFontStyles(Byte(n3.Attributes['Style']));
          end;
      end;
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
    PressHeaderAlgCfgID := n.Attributes['PressHeaderAlgCfgID'];
end;

procedure TsmxColumnCfg.Write;
var
  r, n, n2, n3: IXMLNode;
begin
  inherited Write;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    n.Attributes['FieldName'] := ColumnFieldName;
    n2 := n.AddChild('Column');
    with ColumnText do
    begin
      n2.Attributes['Align'] := Align;
      n2.Attributes['Color'] := Color;
      n3 := n.AddChild('Font');
      with Font do
      begin
        n3.Attributes['Color'] := Color;
        n3.Attributes['Name'] := Name;
        n3.Attributes['Size'] := Size;
        n3.Attributes['Style'] := Byte(Style);
      end;
    end;

    n2 := n.AddChild('Title');
    with ColumnTitle do
    begin
      n2.Attributes['Caption'] := Caption;
      n2.Attributes['Align'] := Align;
      n2.Attributes['Color'] := Color;
      n3 := n2.AddChild('Font');
      with Font do
      begin
        n3.Attributes['Color'] := Color;
        n3.Attributes['Name'] := Name;
        n3.Attributes['Size'] := Size;
        n3.Attributes['Style'] := Byte(Style);
      end;
    end;
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
    n.Attributes['PressHeaderAlgCfgID'] := PressHeaderAlgCfgID;
end;

procedure TsmxColumnCfg.SetColumnFieldName(const Value: String);
begin
  FColumnFieldName := Value;
end;

procedure TsmxColumnCfg.SetColumnText(Value: TsmxCellText);
begin
  FColumnText := Value;
end;

procedure TsmxColumnCfg.SetColumnTitle(Value: TsmxCellText);
begin
  FColumnTitle := Value;
end;

procedure TsmxColumnCfg.SetPressHeaderAlgCfgID(const Value: Integer);
begin
  FPressHeaderAlgCfgID := Value;
end;

{ TsmxGridCfg }

procedure TsmxGridCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxGridCfg then
  begin
    ChangeRowAlgCfgID := TsmxGridCfg(Source).ChangeRowAlgCfgID;
    GridOptions := TsmxGridCfg(Source).GridOptions;
    PressDoubleAlgCfgID := TsmxGridCfg(Source).PressDoubleAlgCfgID;
    RequestCfgID := TsmxGridCfg(Source).RequestCfgID;
  end;
end;

procedure TsmxGridCfg.Clear;
begin
  inherited Clear;
  ChangeRowAlgCfgID := 0;
  GridOptions := [];
  PressDoubleAlgCfgID := 0;
  RequestCfgID := 0;
end;

procedure TsmxGridCfg.Read;
var
  r, n: IXMLNode;
begin
  inherited Read;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    GridOptions := TsmxGridOptions(Byte(n.Attributes['GridOptions']));
    RequestCfgID := n.Attributes['RequestCfgID'];
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
  begin
    ChangeRowAlgCfgID := n.Attributes['ChangeRowAlgCfgID'];
    PressDoubleAlgCfgID := n.Attributes['PressDoubleAlgCfgID'];
  end;
end;

procedure TsmxGridCfg.Write;
var
  r, n: IXMLNode;
begin
  inherited Write;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    n.Attributes['GridOptions'] := Byte(GridOptions);
    n.Attributes['RequestCfgID'] := RequestCfgID;
  end;

  n := r.ChildNodes.FindNode('Event');
  if Assigned(n) then
  begin
    n.Attributes['ChangeRowAlgCfgID'] := ChangeRowAlgCfgID;
    n.Attributes['PressDoubleAlgCfgID'] := PressDoubleAlgCfgID;
  end;
end;

procedure TsmxGridCfg.SetChangeRowAlgCfgID(Value: Integer);
begin
  FChangeRowAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetGridOptions(Value: TsmxGridOptions);
begin
  FGridOptions := Value;
end;

procedure TsmxGridCfg.SetPressDoubleAlgCfgID(Value: Integer);
begin
  FPressDoubleAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetRequestCfgID(Value: Integer);
begin
  FRequestCfgID := Value;
end;

{ TsmxAlgorithmCfg }

destructor TsmxAlgorithmCfg.Destroy;
begin
  if Assigned(FAlgorithmParams) then
    FAlgorithmParams.Free;
  inherited Destroy;
end;

procedure TsmxAlgorithmCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmCfg then
  begin
    AlgorithmCaption := TsmxAlgorithmCfg(Source).AlgorithmCaption;
    AlgorithmHint := TsmxAlgorithmCfg(Source).AlgorithmHint;
    AlgorithmHotKey := TsmxAlgorithmCfg(Source).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxAlgorithmCfg(Source).AlgorithmImageIndex;
    AlgorithmParams := TsmxAlgorithmCfg(Source).AlgorithmParams;
  end;
end;

procedure TsmxAlgorithmCfg.Clear;
begin
  AlgorithmCaption := '';
  AlgorithmHint := '';
  AlgorithmHotKey := 0;
  AlgorithmImageIndex := -1;
  if Assigned(FAlgorithmParams) then
    FAlgorithmParams.Clear;
end;

function TsmxAlgorithmCfg.GetAlgorithmCells: TsmxSimpleKit;
begin
  if not Assigned(FAlgorithmCells) then
    FAlgorithmCells := TsmxSimpleKit.Create(TsmxSimpleKitItem);
  Result := FAlgorithmCells;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmCells(Value: TsmxSimpleKit);
begin
  AlgorithmCells.Assign(Value);
end;

function TsmxAlgorithmCfg.GetAlgorithmParams: TsmxAlgorithmParams;
begin
  if not Assigned(FAlgorithmParams) then
    FAlgorithmParams := TsmxAlgorithmParams.Create(TsmxAlgorithmParam);
  Result := FAlgorithmParams;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmParams(Value: TsmxAlgorithmParams);
begin
  AlgorithmParams.Assign(Value);
end;

{function TsmxAlgorithmCfg.GetExecuteProcedure: TsmxComponentEvent;
begin
  Result := nil;
end;}

procedure TsmxAlgorithmCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Algorithm');
  if Assigned(n) then
  begin
    AlgorithmCaption := n.Attributes['Caption'];
    AlgorithmHint := n.Attributes['Hint'];
    AlgorithmHotKey := n.Attributes['HotKey'];
    AlgorithmImageIndex := n.Attributes['ImageIndex'];
    RefreshParamsCfgID := n.Attributes['RefreshParamsCfgID'];
  end;

  n := r.ChildNodes.FindNode('Params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Param' then
        with AlgorithmParams.Add do
        begin
          DataType := n.ChildNodes[i].Attributes['DataType'];
          ParamLocation := n.ChildNodes[i].Attributes['Location'];
          ParamName := n.ChildNodes[i].Attributes['Name'];
          ParamType := n.ChildNodes[i].Attributes['ParamType'];
          ParamValue := smxFuncs.StrToVar(Variants.VarToStr(n.ChildNodes[i].Attributes['Value']));
        end;
  end;
end;

procedure TsmxAlgorithmCfg.Write;
var
  r, n: IXMLNode;
  i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Algorithm');
  n.Attributes['Caption'] := AlgorithmCaption;
  n.Attributes['Hint'] := AlgorithmHint;
  n.Attributes['HotKey'] := AlgorithmHotKey;
  n.Attributes['ImageIndex'] := AlgorithmImageIndex;
  n.Attributes['RefreshParamsCfgID'] := RefreshParamsCfgID;

  n := r.AddChild('Params');
  for i := 0 to AlgParams.Count - 1 do
    with n.AddChild('Param') do
    begin
      Attributes['DataType'] := AlgorithmParams[i].DataType;
      Attributes['Location'] := AlgorithmParams[i].ParamLocation;
      Attributes['Name'] := AlgorithmParams[i].ParamName;
      Attributes['ParamType'] := AlgorithmParams[i].ParamType;
      Attributes['Value'] := AlgorithmParams[i].ParamValue;
    end;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmCaption(const Value: String);
begin
  FAlgorithmCaption := Value;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmHint(const Value: String);
begin
  FAlgorithmHint := Value;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmHotKey(Value: Integer);
begin
  FAlgorithmHotKey := Value;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmImageIndex(Value: Integer);
begin
  FAlgorithmImageIndex := Value;
end;

procedure TsmxAlgorithmCfg.SetRefreshParamsCfgID(Value: Integer);
begin
  FRefreshParamsCfgID := Value;
end;

{ TsmxLibAlgorithmCfg }

procedure TsmxLibAlgorithmCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxLibAlgorithmCfg then
  begin
    AlgorithmLibrary := TsmxLibAlgorithmCfg(Source).AlgorithmLibrary;
    AlgorithmProcedure := TsmxLibAlgorithmCfg(Source).AlgorithmProcedure;
  end;
end;

procedure TsmxLibAlgorithmCfg.Clear;
begin
  inherited Clear;
  AlgorithmLibrary := '';
  AlgorithmProcedure := '';
end;

procedure TsmxLibAlgorithmCfg.Read;
var
  r, n: IXMLNode;
begin
  inherited Read;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Algorithm');
  if Assigned(n) then
  begin
    AlgorithmLibrary := n.Attributes['Library'];
    AlgorithmProcedure := n.Attributes['Procedure'];
  end;
end;

procedure TsmxLibAlgorithmCfg.Write;
var
  r, n: IXMLNode;
begin
  inherited Write;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Algorithm');
  if Assigned(n) then
  begin
    n.Attributes['Library'] := AlgorithmLibrary;
    n.Attributes['Procedure'] := AlgorithmProcedure;
  end;
end;

procedure TsmxLibAlgorithmCfg.SetAlgorithmLibrary(const Value: String);
begin
  FAlgorithmLibrary := Value;
end;

procedure TsmxLibAlgorithmCfg.SetAlgorithmProcedure(const Value: String);
begin
  FAlgorithmProcedure := Value;
end;

{ TsmxAlgorithmListCfg }

destructor TsmxAlgorithmListCfg.Destroy;
begin
  if Assigned(FAlgorithms) then
    FAlgorithms.Free;
  inherited Destroy;
end;

procedure TsmxAlgorithmListCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmListCfg then
    Algorithms := TsmxAlgorithmListCfg(Source).Algorithms;
end;

procedure TsmxAlgorithmListCfg.Clear;
begin
  if Assigned(FAlgorithms) then
    FAlgorithms.Clear;
end;

function TsmxAlgorithmListCfg.GetAlgorithms: TsmxSimpleKit;
begin
  if not Assigned(FAlgorithms) then
    FAlgorithms := TsmxSimpleKit.Create(TsmxSimpleKitItem);
  Result := FAlgorithms;
end;

{procedure TsmxAlgorithmListCfg.SetAlgoritms(Value: TsmxSimpleKit);
begin
  //Algorithms.Assign(Value);
end;}

procedure TsmxAlgorithmListCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Algorithms');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
    begin
      if n.ChildNodes[i].NodeName = 'Algorithm' then
        with AlgorithmItems.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          AlgorithmMenuItemCfgID := n.ChildNodes[i].Attributes['MenuItemCfgID'];
          AlgorithmToolBarCfgID := n.ChildNodes[i].Attributes['ToolBarCfgID'];
          AlgorithmEnable := n.ChildNodes[i].Attributes['Enable'];
          AlgorithmVisible := n.ChildNodes[i].Attributes['Visible'];
          AlgorithmHotKey := n.ChildNodes[i].Attributes['HotKey'];
          AlgorithmCaption := n.ChildNodes[i].Attributes['Caption'];
          AlgorithmHint := n.ChildNodes[i].Attributes['Hint'];
        end;
    end;
  end;
end;

procedure TsmxAlgorithmListCfg.Write;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Algorithms');
  for i := 0 to AlgorithmItems.Count - 1 do
    with n.AddChild('Algorithm') do
    begin
      Attributes['CfgID'] := AlgorithmItems[i].CfgID;
      Attributes['MenuItemCfgID'] := AlgorithmItems[i].AlgorithmMenuItemCfgID;
      Attributes['ToolBarCfgID'] := AlgorithmItems[i].AlgorithmToolBarCfgID;
      Attributes['Enable'] := BoolToStr(AlgorithmItems[i].AlgorithmEnable, True);
      Attributes['Visible'] := BoolToStr(AlgorithmItems[i].AlgorithmVisible, True);
      Attributes['HotKey'] := AlgorithmItems[i].AlgorithmHotKey;
      Attributes['Caption'] := AlgorithmItems[i].AlgorithmCaption;
      Attributes['Hint'] := AlgorithmItems[i].AlgorithmHint;
    end;
end;

{ TsmxFilterCfg }

procedure TsmxFilterCfg.Clear;
begin
  FFilterName := '';
  FDisplayFormat := '';
  FValueFormat := '';
  with FFilterFont do
  begin
    Color := Integer(clWindowText);
    Name := 'MS Sans Serif';
    Size := 8;
    Style := [];
  end;
  with FFilterHeader do
  begin
    Text := '';
    Align := taLeftJustify;
    Color := Integer(clBtnFace);
    with Font do
    begin
      Color := Integer(clWindowText);
      Name := 'MS Sans Serif';
      Size := 8;
      Style := [];
    end;
  end;
  with FAlgorithm do
  begin
    CfgID := 0;
    Caption := '';
    Enable := False;
    HotKey := 0;
    Visible := False;
  end;
end;

procedure TsmxFilterCfg.Read;
var
  r, n, n2: IXMLNode;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Filter');
  if Assigned(n) then
  begin
    FilterName := n.Attributes['Name'];
    DisplayFormat := n.Attributes['DisplayFormat'];
    ValueFormat := n.Attributes['ValueFormat'];
    n2 := n.ChildNodes.FindNode('Font');
    if Assigned(n2) then
      with FilterFont do
      begin
        Color := n2.Attributes['Color'];
        Name := n2.Attributes['Name'];
        Size := n2.Attributes['Size'];
        Style := TFontStyles(Byte(n2.Attributes['Style']));
      end;
  end;

  n := r.ChildNodes.FindNode('Header');
  if Assigned(n) then
    with FilterHeader do
    begin
      Text := n.Attributes['Text'];
      Align := n.Attributes['Align'];
      Color := n.Attributes['Color'];
      n2 := n.ChildNodes.FindNode('Font');
      if Assigned(n2) then
        with Font do
        begin
          Color := n2.Attributes['Color'];
          Name := n2.Attributes['Name'];
          Size := n2.Attributes['Size'];
          Style := TFontStyles(Byte(n2.Attributes['Size']));
        end;
    end;

  n := r.ChildNodes.FindNode('Algorithm');
  if Assigned(n) then
    with Algorithm do
    begin
      CfgID := n.Attributes['CfgID'];
      Caption := n.Attributes['Caption'];
      Enable := n.Attributes['Enable'];
      HotKey := n.Attributes['HotKey'];
      Visible := n.Attributes['Visible'];
    end;
end;

procedure TsmxFilterCfg.SetAction(Value: TsmxAlgorithmSetting);
begin

end;

procedure TsmxFilterCfg.SetChange(Value: TsmxAlgorithmSetting);
begin

end;

procedure TsmxFilterCfg.SetDisplayFormat(const Value: String);
begin
  FDisplayFormat := Value;
end;

procedure TsmxFilterCfg.SetFilterFont(const Value: TsmxCellText);
begin
  FFilter := Value;
end;

procedure TsmxFilterCfg.SetFilterHeader(const Value: TsmxCellText);
begin
  FFilterHeader := Value;
end;

procedure TsmxFilterCfg.SetFilterName(const Value: String);
begin
  FFilterName := Value;
end;

procedure TsmxFilterCfg.SetValueFormat(const Value: String);
begin
  FValueFormat := Value;
end;

procedure TsmxFilterCfg.Write;
var r, n, n2: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Filter');
  n.Attributes['Name'] := FilterName;
  n.Attributes['DisplayFormat'] := DisplayFormat;
  n.Attributes['ValueFormat'] := ValueFormat;
  n2 := n.AddChild('Font');
  with FilterFont do
  begin
    n2.Attributes['Color'] := Color;
    n2.Attributes['Name'] := Name;
    n2.Attributes['Size'] := Size;
    n2.Attributes['Style'] := Byte(Style);
  end;

  n := r.AddChild('Header');
  with FilterHeader do
  begin
    n.Attributes['Text'] := Text;
    n.Attributes['Align'] := Align;
    n.Attributes['Color'] := Color;
    n2 := n.AddChild('Font');
    with Font do
    begin
      n2.Attributes['Color'] := Color;
      n2.Attributes['Name'] := Name;
      n2.Attributes['Size'] := Size;
      n2.Attributes['Style'] := Byte(Style);
    end;
  end;

  n := r.AddChild('Algorithm');
  with Algorithm do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Caption'] := Caption;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['HotKey'] := HotKey;
    n.Attributes['Visible'] := BoolToStr(Visible, True);
  end;
end;

{ TsmxFilterDeskCfg }

destructor TsmxFilterDeskCfg.Destroy;
begin
  if Assigned(FFilters) then
    FFilters.Free;
  inherited Destroy;
end;

procedure TsmxFilterDeskCfg.Clear;
begin
  with FApplyRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  with FPrepareRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  if Assigned(FFilters) then
    FFilters.Clear;
end;

function TsmxFilterDeskCfg.GetFilters: TsmxControlKit;
begin
  if not Assigned(FFilters) then
    FFilters := TsmxControlKit.Create(TsmxControlKitItem);
  Result := FFilters;
end;

procedure TsmxFilterDeskCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('ApplyRequest');
  if Assigned(n) then
    with ApplyRequest do
    begin
      CfgID := n.Attributes['CfgID'];
      Operation := n.Attributes['Operation'];
      DatabaseName := n.Attributes['DatabaseName'];
    end;

  n := r.ChildNodes.FindNode('PrepareRequest');
  if Assigned(n) then
    with PrepareRequest do
    begin
      CfgID := n.Attributes['CfgID'];
      Operation := n.Attributes['Operation'];
      DatabaseName := n.Attributes['DatabaseName'];
    end;

  n := r.ChildNodes.FindNode('Filters');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //Filters.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Filter' then
        with Filters.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          ItemAlign := n.ChildNodes[i].Attributes['Align'];
          ItemEnable := n.ChildNodes[i].Attributes['Enable'];
          ItemHeight := n.ChildNodes[i].Attributes['Height'];
          ItemLeft := n.ChildNodes[i].Attributes['Left'];
          ItemTop := n.ChildNodes[i].Attributes['Top'];
          ItemVisible := n.ChildNodes[i].Attributes['Visible'];
          ItemWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxFilterDeskCfg.Write;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('ApplyRequest');
  with ApplyRequest do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Operation'] := Operation;
    n.Attributes['DatabaseName'] := DatabaseName;
  end;

  n := r.AddChild('PrepareRequest');
  with PrepareRequest do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Operation'] := Operation;
    n.Attributes['DatabaseName'] := DatabaseName;
  end;

  n := r.AddChild('Filters');
  for i := 0 to Filters.Count - 1 do
    with n.AddChild('Filter') do
    begin
      Attributes['CfgID'] := Filters[i].CfgID;
      Attributes['Align'] := Filters[i].ItemAlign;
      Attributes['Enable'] := BoolToStr(Filters[i].ItemEnable, True);
      Attributes['Height'] := Filters[i].ItemHeight;
      Attributes['Left'] := Filters[i].ItemLeft;
      Attributes['Top'] := Filters[i].ItemTop;
      Attributes['Visible'] := BoolToStr(Filters[i].ItemVisible, True);
      Attributes['Width'] := Filters[i].ItemWidth;
    end;
end;

{ TsmxSectionCfg }

procedure TsmxSectionCfg.Clear;
begin
  with FRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  with FGrid do
  begin
    CfgID := 0;
    Align := alNone;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with FFilterPanel do
  begin
    CfgID := 0;
    Align := alNone;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
end;

procedure TsmxSectionCfg.Read;
var
  r, n: IXMLNode;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Request');
  if Assigned(n) then
    with Request do
    begin
      CfgID := n.Attributes['CfgID'];
      Operation := n.Attributes['Operation'];
      DatabaseName := n.Attributes['DatabaseName'];
    end;

  n := r.ChildNodes.FindNode('Grid');
  if Assigned(n) then
    with Grid do
    begin
      CfgID := n.Attributes['CfgID'];
      Align := n.Attributes['Align'];
      Enable := n.Attributes['Enable'];
      Visible := n.Attributes['Visible'];
      with PositionSize do
      begin
        Height := n.Attributes['Height'];
        Left := n.Attributes['Left'];
        Top := n.Attributes['Top'];
        Width := n.Attributes['Width'];
      end;
    end;

  n := r.ChildNodes.FindNode('FilterPanel');
  if Assigned(n) then
    with FilterPanel do
    begin
      CfgID := n.Attributes['CfgID'];
      Align := n.Attributes['Align'];
      Enable := n.Attributes['Enable'];
      Visible := n.Attributes['Visible'];
      with PositionSize do
      begin
        Height := n.Attributes['Height'];
        Left := n.Attributes['Left'];
        Top := n.Attributes['Top'];
        Width := n.Attributes['Width'];
      end;
    end;  
end;

procedure TsmxSectionCfg.Write;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Request');
  with Request do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Operation'] := Operation;
    n.Attributes['DatabaseName'] := DatabaseName;
  end;

  n := r.AddChild('Grid');
  with Grid do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['Visible'] := BoolToStr(Visible, True);
    with PositionSize do
    begin
      n.Attributes['Height'] := Height;
      n.Attributes['Left'] := Left;
      n.Attributes['Top'] := Top;
      n.Attributes['Width'] := Width;
    end;
  end;

  n := r.AddChild('FilterPanel');
  with FilterPanel do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['Visible'] := BoolToStr(Visible, True);
    with PositionSize do
    begin
      n.Attributes['Height'] := Height;
      n.Attributes['Left'] := Left;
      n.Attributes['Top'] := Top;
      n.Attributes['Width'] := Width;
    end;
  end;
end;

{ TsmxPageCfg }

procedure TsmxPageCfg.Clear;
begin
  FPageCaption := '';
  FPageImageIndex := -1;
  if Assigned(FSections) then
    FSections.Clear;
end;

function TsmxPageCfg.GetSections: TsmxControlKit;
begin
  if not Assigned(FSections) then
    FSections := TsmxControlKit.Create(TsmxControlKitItem);
  Result := FSections;
end;

procedure TsmxPageCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Page');
  if Assigned(n) then
  begin
    PageCaption := n.Attributes['Caption'];
    PageImageIndex := n.Attributes['ImageIndex'];
  end;

  n := r.ChildNodes.FindNode('Sections');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //Sections.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Section' then
        with Sections.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          ItemAlign := n.ChildNodes[i].Attributes['Align'];
          ItemEnable := n.ChildNodes[i].Attributes['Enable'];
          ItemHeight := n.ChildNodes[i].Attributes['Height'];
          ItemLeft := n.ChildNodes[i].Attributes['Left'];
          ItemTop := n.ChildNodes[i].Attributes['Top'];
          ItemVisible := n.ChildNodes[i].Attributes['Visible'];
          ItemWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxPageCfg.Write;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Page');
  n.Attributes['Caption'] := PageCaption;
  n.Attributes['ImageIndex'] := PageImageIndex;

  n := r.AddChild('Sections');
  for i := 0 to Sections.Count - 1 do
    with n.AddChild('Section') do
    begin
      Attributes['CfgID'] := Sections[i].CfgID;
      Attributes['Align'] := Sections[i].ItemAlign;
      Attributes['Enable'] := BoolToStr(Sections[i].ItemEnable, True);
      Attributes['Height'] := Sections[i].ItemHeight;
      Attributes['Left'] := Sections[i].ItemLeft;
      Attributes['Top'] := Sections[i].ItemTop;
      Attributes['Visible'] := BoolToStr(Sections[i].ItemVisible, True);
      Attributes['Width'] := Sections[i].ItemWidth;
    end;
end;

{ TsmxPageManagerCfg }

destructor TsmxPageManagerCfg.Destroy;
begin
  if Assigned(FSheets) then
    FSheets.Free;
  inherited Destroy;
end;

procedure TsmxPageManagerCfg.Clear;
begin
  if Assigned(FSheets) then
    FSheets.Clear;
end;

function TsmxPageManagerCfg.GetSheets: TsmxControlKit;
begin
  if not Assigned(FSheets) then
    FSheets := TsmxControlKit.Create(TsmxControlKitItem);
  Result := FSheets;
end;

procedure TsmxPageManagerCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Pages');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //Sheets.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Page' then
        with Sheets.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          ItemAlign := n.ChildNodes[i].Attributes['Align'];
          ItemEnable := n.ChildNodes[i].Attributes['Enable'];
          ItemHeight := n.ChildNodes[i].Attributes['Height'];
          ItemLeft := n.ChildNodes[i].Attributes['Left'];
          ItemTop := n.ChildNodes[i].Attributes['Top'];
          ItemVisible := n.ChildNodes[i].Attributes['Visible'];
          ItemWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxPageManagerCfg.Write;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Pages');
  for i := 0 to Sheets.Count - 1 do
    with n.AddChild('Page') do
    begin
      Attributes['CfgID'] := Sheets[i].CfgID;
      Attributes['Align'] := Sheets[i].ItemAlign;
      Attributes['Enable'] := BoolToStr(Sheets[i].ItemEnable, True);
      Attributes['Height'] := Sheets[i].ItemHeight;
      Attributes['Left'] := Sheets[i].ItemLeft;
      Attributes['Top'] := Sheets[i].ItemTop;
      Attributes['Visible'] := BoolToStr(Sheets[i].ItemVisible, True);
      Attributes['Width'] := Sheets[i].ItemWidth;
    end;
end;

{ TsmxMenuItemCfg }

procedure TsmxMenuItemCfg.Clear;
begin
  FItemCaption := '';
  FItemImageIndex := -1;
end;

procedure TsmxMenuItemCfg.Read;
var
  r, n: IXMLNode;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('MenuItem');
  if Assigned(n) then
  begin
    ItemCaption := n.Attributes['Caption'];
    ItemImageIndex := n.Attributes['ImageIndex'];
  end; 
end;

procedure TsmxMenuItemCfg.Write;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('MenuItem');
  n.Attributes['Caption'] := ItemCaption;
  n.Attributes['ImageIndex'] := ItemImageIndex;
end;

{ TsmxHVisibleUnit }

constructor TsmxHVisibleUnit.Create(AHKit: TsmxHKit);
begin
  inherited Create(AHKit);
  FCfgID := 0;
  FItemAlign := alNone;
  FItemEnable := False;
  FItemHeight := 0;
  FItemLeft := 0;
  FItemTop := 0;
  FItemVisible := False;
  FItemWidth := 0;
end;

function TsmxHVisibleUnit.Add: TsmxHVisibleUnit;
begin
  Result := TsmxHVisibleUnit(inherited Add);
end;

function TsmxHVisibleUnit.FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxHVisibleUnit;

  function Find(AUnit: TsmxHVisibleUnit): TsmxHVisibleUnit;
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

function TsmxHVisibleUnit.GetItem(Index: Integer): TsmxHVisibleUnit;
begin
  Result := TsmxHVisibleUnit(inherited Items[Index]);
end;

function TsmxHVisibleUnit.GetParent: TsmxHVisibleUnit;
begin
  Result := TsmxHVisibleUnit(inherited Parent);
end;

{ TsmxHVisibleUnits }

function TsmxHVisibleUnits.GetRoot: TsmxHVisibleUnit;
begin
  Result := TsmxHVisibleUnit(inherited Root);
end;

{ TsmxMainMenuCfg }

destructor TsmxMainMenuCfg.Destroy;
begin
  if Assigned(FMenuUnits) then
    FMenuUnits.Free;
  inherited Destroy;
end;

procedure TsmxMainMenuCfg.Clear;
begin
  if Assigned(FMenuUnits) then
    FMenuUnits.Root.Clear;
end;

function TsmxMainMenuCfg.GetMenuUnits: TsmxHVisibleUnits;
begin
  if not Assigned(FMenuUnits) then
    FMenuUnits := TsmxHVisibleUnits.Create(TsmxHVisibleUnit);
  Result := FMenuUnits;
end;

procedure TsmxMainMenuCfg.Read;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxHVisibleUnit);
  var
    i: Integer;
    u: TsmxHVisibleUnit;
  begin
    u := AUnit.Add;
    with u do
    begin
      CfgID := ANode.Attributes['CfgID'];
      ItemAlign := ANode.Attributes['Align'];
      ItemEnable := ANode.Attributes['Enable'];
      ItemHeight := ANode.Attributes['Height'];
      ItemLeft := ANode.Attributes['Left'];
      ItemTop := ANode.Attributes['Top'];
      ItemVisible := ANode.Attributes['Visible'];
      ItemWidth := ANode.Attributes['Width'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'MenuItem' then
        AddUnits(ANode.ChildNodes[i], u);
  end;

var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('MenuItems');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //MenuUnits.Root.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'MenuItem' then
        AddUnits(n.ChildNodes[i], MenuUnits.Root);
  end; 
end;

procedure TsmxMainMenuCfg.Write;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxHVisibleUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('MenuItem');
    with n do
    begin
      Attributes['CfgID'] := AUnit.CfgID;
      Attributes['Align'] := AUnit.ItemAlign;
      Attributes['Enable'] := BoolToStr(AUnit.ItemEnable, True);
      Attributes['Height'] := AUnit.ItemHeight;
      Attributes['Left'] := AUnit.ItemLeft;
      Attributes['Top'] := AUnit.ItemTop;
      Attributes['Visible'] := BoolToStr(AUnit.ItemVisible, True);
      Attributes['Width'] := AUnit.ItemWidth;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;

var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('MenuItems');
  for i := 0 to MenuUnits.Root.Count - 1 do
    AddNodes(n, MenuUnits.Root[i]);
end;

{ TsmxToolBoardCfg }

procedure TsmxToolBoardCfg.Clear;
begin
  FBarFlat := False;
  FBarShowCaptions := False;
  FBarShowHint := False;
end;

procedure TsmxToolBoardCfg.Read;
var
  r, n: IXMLNode;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('ToolBar');
  if Assigned(n) then
  begin
    BarFlat := n.Attributes['Flat'];
    BarShowCaptions := n.Attributes['ShowCaptions'];
    BarShowHint := n.Attributes['ShowHint'];
  end;
end;

procedure TsmxToolBoardCfg.Write;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('ToolBar');
  n.Attributes['Flat'] := BoolToStr(BarFlat, True);
  n.Attributes['ShowCaptions'] := BoolToStr(BarShowCaptions, True);
  n.Attributes['ShowHint'] := BoolToStr(BarShowHint, True);
end;

{ TsmxControlBoardCfg }

procedure TsmxControlBoardCfg.Clear;
begin
  if Assigned(FBarUnits) then
    FBarUnits.Clear;
end;

function TsmxControlBoardCfg.GetBarUnits: TsmxControlKit;
begin
  if not Assigned(FBarUnits) then
    FBarUnits := TsmxControlKit.Create(TsmxControlKitItem);
  Result := FBarUnits;
end;

procedure TsmxControlBoardCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Bars');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //BarUnits.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Bar' then
        with BarUnits.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          ItemAlign := n.ChildNodes[i].Attributes['Align'];
          ItemEnable := n.ChildNodes[i].Attributes['Enable'];
          ItemHeight := n.ChildNodes[i].Attributes['Height'];
          ItemLeft := n.ChildNodes[i].Attributes['Left'];
          ItemTop := n.ChildNodes[i].Attributes['Top'];
          ItemVisible := n.ChildNodes[i].Attributes['Visible'];
          ItemWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxControlBoardCfg.Write;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Bars');
  for i := 0 to BarUnits.Count - 1 do
    with n.AddChild('Bar') do
    begin
      Attributes['CfgID'] := BarUnits[i].CfgID;
      Attributes['Align'] := BarUnits[i].ItemAlign;
      Attributes['Enable'] := BoolToStr(BarUnits[i].ItemEnable, True);
      Attributes['Height'] := BarUnits[i].ItemHeight;
      Attributes['Left'] := BarUnits[i].ItemLeft;
      Attributes['Top'] := BarUnits[i].ItemTop;
      Attributes['Visible'] := BoolToStr(BarUnits[i].ItemVisible, True);
      Attributes['Width'] := BarUnits[i].ItemWidth;
    end;
end;

{ TsmxFormCfg }

destructor TsmxFormCfg.Destroy;
begin
  if Assigned(FPageManagers) then
    FPageManagers.Free;
  inherited Destroy;
end;

procedure TsmxFormCfg.Clear;
begin
  FFormCaption := '';
  FFormImageIndex := -1;
  with FFormPositionSize do
  begin
    Height := 480;
    Left := 150;
    Top := 100;
    Width := 640;
  end;
  with FAlgorithmList do
  begin
    CfgID := 0;
    IsCreateMenuItem := False;
    IsCreateToolButton := False;
  end;
  with FControlBar do
  begin
    CfgID := 0;
    Align := alNone;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with FMainMenu do
  begin
    CfgID := 0;
    Align := alNone;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with FStateRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  with FStatusBar do
  begin
    CfgID := 0;
    Align := alNone;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  if Assigned(FPageManagers) then
    FPageManagers.Clear;
end;

function TsmxFormCfg.GetPageManagers: TsmxControlKit;
begin
  if not Assigned(FPageManagers) then
    FPageManagers := TsmxControlKit.Create(TsmxControlKitItem);
  Result := FPageManagers;
end;

procedure TsmxFormCfg.Read;
var
  r, n: IXMLNode;
  i: Integer;
begin
  Clear;
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Form');
  if Assigned(n) then
  begin
    FormCaption := n.Attributes['Caption'];
    FormImageIndex := n.Attributes['ImageIndex'];
    with FormPositionSize do
    begin
      Height := n.Attributes['Height'];
      Left := n.Attributes['Left'];
      Top := n.Attributes['Top'];
      Width := n.Attributes['Width'];
    end;
  end;

  n := r.ChildNodes.FindNode('PageManagers');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    //PageManagers.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'PageManager' then
        with PageManagers.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          ItemAlign := n.ChildNodes[i].Attributes['Align'];
          ItemEnable := n.ChildNodes[i].Attributes['Enable'];
          ItemHeight := n.ChildNodes[i].Attributes['Height'];
          ItemLeft := n.ChildNodes[i].Attributes['Left'];
          ItemTop := n.ChildNodes[i].Attributes['Top'];
          ItemVisible := n.ChildNodes[i].Attributes['Visible'];
          ItemWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;

  n := r.ChildNodes.FindNode('MainMenu');
  if Assigned(n) then
    with MainMenu do
    begin
      CfgID := n.Attributes['CfgID'];
      Align := n.Attributes['Align'];
      Enable := n.Attributes['Enable'];
      Visible := n.Attributes['Visible'];
      with PositionSize do
      begin
        Height := n.Attributes['Height'];
        Left := n.Attributes['Left'];
        Top := n.Attributes['Top'];
        Width := n.Attributes['Width'];
      end;
    end;

  n := r.ChildNodes.FindNode('AlgorithmList');
  if Assigned(n) then
    with AlgorithmList do
    begin
      CfgID := n.Attributes['CfgID'];
      IsCreateMenuItem := n.Attributes['IsCreateMenuItem'];
      IsCreateToolButton := n.Attributes['IsCreateToolButton'];
    end;

  n := r.ChildNodes.FindNode('ControlBar');
  if Assigned(n) then
    with ControlBar do
    begin
      CfgID := n.Attributes['CfgID'];
      Align := n.Attributes['Align'];
      Enable := n.Attributes['Enable'];
      Visible := n.Attributes['Visible'];
      with PositionSize do
      begin
        Height := n.Attributes['Height'];
        Left := n.Attributes['Left'];
        Top := n.Attributes['Top'];
        Width := n.Attributes['Width'];
      end;
    end;

  n := r.ChildNodes.FindNode('StateRequest');
  if Assigned(n) then
    with StateRequest do
    begin
      CfgID := n.Attributes['CfgID'];
      Operation := n.Attributes['Operation'];
      DatabaseName := n.Attributes['DatabaseName'];
    end;

  n := r.ChildNodes.FindNode('StatusBar');
  if Assigned(n) then
    with StatusBar do
    begin
      CfgID := n.Attributes['CfgID'];
      Align := n.Attributes['Align'];
      Enable := n.Attributes['Enable'];
      Visible := n.Attributes['Visible'];
      with PositionSize do
      begin
        Height := n.Attributes['Height'];
        Left := n.Attributes['Left'];
        Top := n.Attributes['Top'];
        Width := n.Attributes['Width'];
      end;
    end;
end;

procedure TsmxFormCfg.Write;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Form');
  n.Attributes['Caption'] := FormCaption;
  n.Attributes['ImageIndex'] := FormImageIndex;
  with FormPositionSize do
  begin
    n.Attributes['Height'] := Height;
    n.Attributes['Left'] := Left;
    n.Attributes['Top'] := Top;
    n.Attributes['Width'] := Width;
  end;

  n := r.AddChild('PageManagers');
  for i := 0 to PageManagers.Count - 1 do
    with n.AddChild('PageManager') do
    begin
      Attributes['CfgID'] := PageManagers[i].CfgID;
      Attributes['Align'] := PageManagers[i].ItemAlign;
      Attributes['Enable'] := BoolToStr(PageManagers[i].ItemEnable, True);
      Attributes['Height'] := PageManagers[i].ItemHeight;
      Attributes['Left'] := PageManagers[i].ItemLeft;
      Attributes['Top'] := PageManagers[i].ItemTop;
      Attributes['Visible'] := BoolToStr(PageManagers[i].ItemVisible, True);
      Attributes['Width'] := PageManagers[i].ItemWidth;
    end;

  n := r.AddChild('MainMenu');
  with MainMenu do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['Visible'] := BoolToStr(Visible, True);
    with PositionSize do
    begin
      n.Attributes['Height'] := Height;
      n.Attributes['Left'] := Left;
      n.Attributes['Top'] := Top;
      n.Attributes['Width'] := Width;
    end;
  end;

  n := r.AddChild('AlgorithmList');
  with AlgorithmList do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['IsCreateMenuItem'] := BoolToStr(IsCreateMenuItem, True);
    n.Attributes['IsCreateToolButton'] := BoolToStr(IsCreateToolButton, True);
  end;

  n := r.AddChild('ControlBar');
  with ControlBar do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['Visible'] := BoolToStr(Visible, True);
    with PositionSize do
    begin
      n.Attributes['Height'] := Height;
      n.Attributes['Left'] := Left;
      n.Attributes['Top'] := Top;
      n.Attributes['Width'] := Width;
    end;
  end;

  n := r.AddChild('StateRequest');
  with StateRequest do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Operation'] := Operation;
    n.Attributes['DatabaseName'] := DatabaseName;
  end;

  n := r.AddChild('StatusBar');
  with StatusBar do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['Visible'] := BoolToStr(Visible, True);
    with PositionSize do
    begin
      n.Attributes['Height'] := Height;
      n.Attributes['Left'] := Left;
      n.Attributes['Top'] := Top;
      n.Attributes['Width'] := Width;
    end;
  end;
end;

initialization
  RegisterClasses([TsmxRequestCfg, TsmxColumnCfg, TsmxGridCfg, TsmxLibAlgorithmCfg,
    TsmxAlgorithmListCfg,TsmxFilterCfg, TsmxFilterDeskCfg, TsmxSectionCfg,
    TsmxPageCfg, TsmxPageManagerCfg, TsmxMenuItemCfg, TsmxMainMenuCfg,
    TsmxToolBoardCfg, TsmxControlBoardCfg, TsmxFormCfg]);

{finalization
  UnRegisterClasses([TsmxRequestCfg, TsmxColumnCfg, TsmxGridCfg, TsmxLibAlgorithmCfg,
    TsmxAlgorithmListCfg,TsmxFilterCfg, TsmxFilterDeskCfg, TsmxSectionCfg,
    TsmxPageCfg, TsmxPageManagerCfg, TsmxMenuItemCfg, TsmxMainMenuCfg,
    TsmxToolBoardCfg, TsmxControlBoardCfg, TsmxFormCfg]);}

end.
