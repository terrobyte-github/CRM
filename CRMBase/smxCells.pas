unit smxCells;

interface

uses
  Classes, Controls, ComCtrls, DB, DBGrids, Forms, ExtCtrls, StdCtrls, Menus,
  ActnList, Windows, smxClasses, smxCfgs, smxWheelDBGrid, smxDBIntf, smxTypes;

type
  { TsmxCustomRequest }

  //TsmxCustomPage = class;

  TsmxCustomRequest = class(TsmxBaseCell)
  private
    //FDataSetIntf: IsmxDataSet;
    //FPage: TsmxCustomPage;
    FParamList: TStrings;
    function GetCfg: TsmxRequestCfg;
    //function GetPage: TsmxCustomPage;
    function GetParamList: TStrings;
    function GetParam(Key: String): String;
    procedure SetParam(Key: String; Value: String);
  protected
    function GetCellDataSet: IsmxDataSet; virtual;
    //function GetCellDataSetType: TsmxDataSetType; virtual;
    //function GetCellParam(Key: String): String; virtual;
    //function GetPrepared: Boolean; virtual;
    //function GetResultValue: Variant; virtual;
    //procedure SetPage(Value: TsmxCustomPage); virtual;

    property Cfg: TsmxRequestCfg read GetCfg;
    //property DataSet: IsmxDataSet read FDataSetIntf;
    property ParamList: TStrings read GetParamList;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    function FindFieldSense(AFieldSense: TsmxFieldSense): IsmxField;
    //function FindParamLocation(const AParam: IsmxParam): TsmxParamLocation;
    procedure Perform(Same: Boolean = False); virtual;
    procedure RefreshParams; virtual;

    //property DataSet: IsmxDataSet read FDataSetIntf;
    property CellDataSet: IsmxDataSet read GetCellDataSet;
    //property CellDataSetType: TsmxDataSetType read GetCellDataSetType;
    //property CellParams[Key: String]: String read GetCellParam;
    property RequestParams[Key: String]: String read GetParam write SetParam;
    //property Page: TsmxCustomPage read GetPage; //FPage write SetPage;
    //property Prepared: Boolean read GetPrepared;
    //property ResultValue: Variant read GetResultValue;
  end;

  { TsmxRequest }

  TsmxRequest = class(TsmxCustomRequest)
  private
    FDataSetIntf: IsmxDataSet;
    //FTargetRequest: TsmxTargetRequest;
  protected
    //function GetInternalObject: TObject; override;
    function GetCellDataSet: IsmxDataSet; override;
    //function GetCellDataSetType: TsmxDataSetType; override;
    //function GetCellParam(Key: String): String; override;
    //function GetPrepared: Boolean; override;
    //function GetResultValue: Variant; override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;

    //property DataSet: IsmxDataSet read FDataSetIntf;
    //property TargetRequest: TsmxTargetRequest read FTargetRequest;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    procedure Perform(Same: Boolean = False); override;
  end;

  { TsmxCustomFilter }

  //TsmxCustomGrid = class;

  TsmxCustomColumn = class(TsmxControlCell)
  private
    //FFilterName: String;
    //FGrid: TsmxCustomGrid;
    function GetCfg: TsmxColumnCfg;
    //function GetGrid: TsmxCustomGrid;
    //function GetFilterPanel: TsmxCustomFilterPanel;
  protected
    //function GetFilterValue: Variant; virtual;
    //procedure SetFilterValue(Value: Variant); virtual;
    //procedure SetGrid(Value: TsmxCustomGrid); virtual;

    property Cfg: TsmxColumnCfg read GetCfg;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;

    //property FilterName: String read FFilterName;
    //property FilterValue: Variant read GetFilterValue write SetFilterValue;
    //property Grid: TsmxCustomGrid read GetGrid; //FGrid write SetGrid;
  end;

  { TsmxDBColumn }

  TsmxDBColumn = class(TsmxCustomColumn)
  private
    FColumn: TColumn;
  protected
    function GetInternalObject: TObject; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellEnable: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetItemParent: TObject; override;
    //function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure SetGrid(Value: TsmxCustomGrid); override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellEnable(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemParent(Value: TObject); override;
    //procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;


    property Column: TColumn read FColumn;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
  end;

  { TsmxCustomGrid }

  TsmxCustomGrid = class(TsmxControlCell)
  private
    FColumnList: TList;
    //FPage: TsmxCustomPage;
    FRequest: TsmxCustomRequest;
    function GetCfg: TsmxGridCfg;
    function GetColumn(Index: Integer): TsmxCustomColumn;
    function GetColumnCount: Integer;
    //function GetPage: TsmxCustomPage;
    //function GetRequestMode: TsmxOperationMode;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetPage(Value: TsmxCustomPage); virtual;
    //procedure SetRequest(Value: TsmxCustomRequest); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxGridCfg read GetCfg;
    property ColumnList: TList read FColumnList;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //procedure ApplyGrid; virtual;
    procedure Prepare(Forcibly: Boolean = False); override;
    //procedure PrepareGrid; virtual;
    //procedure RefreshGrid; virtual;

    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TsmxCustomColumn read GetColumn; default;
    //property Page: TsmxCustomPage read GetPage; //FPage write SetPage;
    property Request: TsmxCustomRequest read FRequest; //write SetRequest;
    //property RequestMode: TsmxOperationMode read GetRequestMode;
  end;

  { TsmxDBGrid }

  TsmxDBGrid = class(TsmxCustomGrid)
  private
    FDataSource: TDataSource;
    FGrid: TsmxWheelDBGrid; //TDBGrid;
  protected
    //procedure AddColumns; virtual;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellCursor: TCursor; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    //function GetItemParent: TObject; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure InstallParent; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellCursor(Value: TCursor); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    //procedure SetPage(Value: TsmxCustomPage); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;
    //procedure SetRequest(Value: TsmxCustomRequest); override;
    //procedure UnInstallParent; override;
    //procedure WheelDownProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
    //  var Handled: Boolean); virtual;
    //procedure WheelUpProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
    //  var Handled: Boolean); virtual;

    //property Grid: TDBGrid read FGrid;
    property Grid: TsmxWheelDBGrid read FGrid;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    //procedure ApplyGrid; override;
    //procedure PrepareGrid; override;
    //procedure RefreshGrid; override;
  end;

  { TsmxCustomFilter }

  //TsmxCustomFilterPanel = class;
  //TsmxAlgorithm = class;

  TsmxCustomFilter = class(TsmxControlCell)
  private
    FAlgorithm: TsmxAlgorithm;
    //FAlgorithmList: TsmxCustomAlgorithmList;
    //FFilterName: String;
    //FFilterPanel: TsmxCustomFilterPanel;
    FRequest: TsmxCustomRequest;
    //function GetAlgorithmID: Integer;
    //function GetAlgorithmParams: Variant;
    function GetCfg: TsmxFilterCfg;
    function GetFilterName: String;
    //function GetFilterPanel: TsmxCustomFilterPanel;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    //function GetFilterCaption: String; virtual;
    function GetFilterValue: Variant; virtual;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetAlgorithm(Value: TsmxAlgorithm); virtual;
    //procedure SetFilterCaption(Value: String); virtual;
    procedure SetFilterValue(Value: Variant); virtual;
    //procedure SetFilterPanel(Value: TsmxCustomFilterPanel); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxFilterCfg read GetCfg;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;

    property Algorithm: TsmxAlgorithm read FAlgorithm; //write SetAlgorithm;
    //property AlgorithmID: Integer read GetAlgorithmID;
    //property AlgorithmParams: Variant read GetAlgorithmParams;
    //property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList;
    //property FilterCaption: String read GetFilterCaption write SetFilterCaption;
    property FilterName: String read GetFilterName;
    //property FilterPanel: TsmxCustomFilterPanel read GetFilterPanel; //FFilterPanel write SetFilterPanel;
    property FilterValue: Variant read GetFilterValue write SetFilterValue;
    property Request: TsmxCustomRequest read FRequest;
  end;

  { TsmxPanelFilter }

  TsmxPanelFilter = class(TsmxCustomFilter)
  private
    FHeader: TLabel;
    FPanel: TPanel;
  protected
    //function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    //function GetItemParent: TObject; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure InstallParent; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    //procedure SetPanelFilters(Value: TsmxCustomFilterPanel); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;
    //procedure UnInstallParent; override;

    property Header: TLabel read FHeader;
    property Panel: TPanel read FPanel;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
  end;

  { TsmxCustomFilterPanel }

  TsmxCustomAlgorithmList = class;

  TsmxCustomFilterPanel = class(TsmxControlCell)
  private
    //FAlgorithmList: TsmxCustomAlgorithmList;
    FFilterList: TList;
    //FPage: TsmxCustomPage;
    FRequest: TsmxCustomRequest;
    function GetCfg: TsmxFilterPanelCfg;
    function GetFilter(Index: Integer): TsmxCustomFilter;
    function GetFilterCount: Integer;
    //function GetPage: TsmxCustomPage;
    //function GetRequestMode: TsmxOperationMode;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetPage(Value: TsmxCustomPage); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxFilterPanelCfg read GetCfg;
    property FilterList: TList read FFilterList;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //procedure ApplyFilterPanel; virtual;
    function FindFilterByName(const AFilterName: String): TsmxCustomFilter;
    procedure Prepare(Forcibly: Boolean = False); override;
    //procedure PrepareFilterPanel; virtual;
    //procedure RefreshFilterPanel; virtual;

    //property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList;
    property FilterCount: Integer read GetFilterCount;
    property Filters[Index: Integer]: TsmxCustomFilter read GetFilter; default;
    //property Page: TsmxCustomPage read GetPage; //FPage write SetPage;
    property Request: TsmxCustomRequest read FRequest;
    //property RequestMode: TsmxOperationMode read GetRequestMode;
  end;

  { TsmxFilterPanel }

  TsmxFilterPanel = class(TsmxCustomFilterPanel)
  private
    FPanel: TPanel;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    //function GetItemParent: TObject; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure InstallParent; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    //procedure SetPage(Value: TsmxCustomPage); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;
    //procedure UnInstallParent; override;

    property Panel: TPanel read FPanel;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    //procedure ApplyFilterPanel; override;
    //procedure PrepareFilterPanel; override;
    //procedure RefreshFilterPanel; override;
  end;

  { TsmxCustomPage }

  //TsmxCustomPageManager = class;

  TsmxCustomPage = class(TsmxControlCell)
  private
    FFilterPanel: TsmxCustomFilterPanel;
    FGrid: TsmxCustomGrid;
    //FPageManager: TsmxCustomPageManager;
    //FRequest: TsmxCustomRequest;
    function GetCfg: TsmxPageCfg;
    //function GetPageManager: TsmxCustomPageManager;
    //function GetRequestMode: TsmxOperationMode;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetManagerPages(Value: TsmxCustomPageManager); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxPageCfg read GetCfg;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //destructor Destroy; override;

    property Grid: TsmxCustomGrid read FGrid;
    property FilterPanel: TsmxCustomFilterPanel read FFilterPanel;
    //property PageManager: TsmxCustomPageManager read GetPageManager; //FPageManager write SetManagerPages;
    //property Request: TsmxCustomRequest read FRequest;
    //property RequestMode: TsmxOperationMode read GetRequestMode;
  end;

  { TsmxTabSheet }

  TsmxTabSheet = class(TsmxCustomPage)
  private
    FPage: TTabSheet;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    //function GetItemParent: TObject; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure InstallParent; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    //procedure SetManagerPages(Value: TsmxCustomPageManager); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;
    //procedure UnInstallParent; override;

    property Page: TTabSheet read FPage;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
  end;

  { TsmxCustomPageManager }

  //TsmxCustomForm = class;

  TsmxCustomPageManager = class(TsmxControlCell)
  private
    //FForm: TsmxCustomForm;
    FPageList: TList;
    function GetCfg: TsmxPageManagerCfg;
    //function GetForm: TsmxCustomForm;
    function GetPage(Index: Integer): TsmxCustomPage;
    function GetPageCount: Integer;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    function GetActivePage: TsmxCustomPage; virtual;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetActivePage(Value: TsmxCustomPage); virtual;
    //procedure SetForm(Value: TsmxCustomForm); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxPageManagerCfg read GetCfg;
    property PageList: TList read FPageList;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //destructor Destroy; override;

    property ActivePage: TsmxCustomPage read GetActivePage; //write SetActivePage;
    //property Form: TsmxCustomForm read GetForm; //FForm write SetForm;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TsmxCustomPage read GetPage; default;
  end;

  { TsmxPageControl }

  TsmxPageControl = class(TsmxCustomPageManager)
  private
    FPageControl: TPageControl;
  protected
    function GetActivePage: TsmxCustomPage; override;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    //function GetItemParent: TObject; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure InstallParent; override;
    //procedure SetForm(Value: TsmxCustomForm); override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;
    procedure UnInstallParent; override;

    property PageControl: TPageControl read FPageControl;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
  end;

  { TsmxAlgorithm }

  //TsmxCustomAlgorithmList = class;

  {TsmxAlgorithm = class(TsmxBaseCell)
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
  end;}

  { TsmxCustomLibAlgorithm }

  //TsmxProcLibAlgExecute = procedure(Sender: TObject) of object;
  TsmxProcAlgExecute = procedure(Action: TsmxAlgorithm; Params: Variant);
  //TsmxFuncAlgExecute = function(Action: TsmxAlgorithm; Params: Variant): Variant;

  //TsmxProcInitDLL = procedure(ACall: TsmxFuncCallBack);
  //TsmxFuncManagerActions = function: TsmxManagerActions;

  TsmxCustomLibAlgorithm = class(TsmxAlgorithm)
  private
    //FAlgorithmList: TsmxCustomAlgorithmList;
    FLibHandle: THandle;
    //FLibInitProc: TsmxProcInitDLL;
    FLibManager: TsmxLibManager;
    //FLibManagerActions: TsmxManagerActions;
    FLibProc: TsmxProcAlgExecute; //TsmxProcLibAlgExecute;
    //FLibFunc: TsmxFuncAlgExecute; //TsmxProcLibAlgExecute;
    function GetCfg: TsmxLibAlgorithmCfg;
    function GetLibManager: TsmxLibManager;
  protected
    procedure ExecProc(Sender: TObject); virtual;
    function GetCellParams: Variant; override;
    //function GetCellEnable: Boolean; virtual;
    function GetCellImageIndex: Integer; override;
    //function GetCellVisible: Boolean; virtual;
    procedure LoadLib; virtual;
    //procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    //procedure SetCellEnable(Value: Boolean); virtual;
    //procedure SetCellVisible(Value: Boolean); virtual;
    procedure UnLoadLib; virtual;

    property Cfg: TsmxLibAlgorithmCfg read GetCfg;
    property LibHandle: THandle read FLibHandle;
    //property LibInitProc: TsmxProcInitDLL read FLibInitProc;
    //property LibManagerActions: TsmxManagerActions read FLibManagerActions;
    property LibProc: TsmxProcAlgExecute read FLibProc; //TsmxProcLibAlgExecute read FLibProc;
    //property LibFunc: TsmxFuncAlgExecute read FLibFunc; //TsmxProcLibAlgExecute read FLibProc;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //destructor Destroy; override;
    procedure Execute; override;

    //property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write SetAlgorithmList;
    //property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    //property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    property LibManager: TsmxLibManager read GetLibManager;
  end;

  { TsmxLibAlgorithm }

  TsmxLibAlgorithm = class(TsmxCustomLibAlgorithm)
  private
    FAction: TAction;
  protected
    //procedure AddEvent; virtual;
    //procedure DelEvent; virtual;
    function GetInternalObject: TObject; override;
    function GetCellCaption: String; override;
    function GetCellEnable: Boolean; override;
    function GetCellHotKey: Integer; override;
    function GetCellVisible: Boolean; override;
    //procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); override;
    procedure SetCellCaption(Value: String); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHotKey(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;

    property Action: TAction read FAction;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
  end;

  { TsmxCustomAlgorithmList }

  TsmxCustomAlgorithmList = class(TsmxBaseCell)
  private
    //FForm: TsmxCustomForm;
    FAlgorithmList: TList;
    function GetAlgorithm(Index: Integer): TsmxAlgorithm;
    //function GetAlgorithmButton(CfgID: Integer): Boolean;
    function GetAlgorithmCount: Integer;
    //function GetAlgorithmMenuItemID(CfgID: Integer): Integer;
    //function GetAlgorithmParams(ID: Integer): Variant;
    function GetCfg: TsmxAlgorithmListCfg;
    //function GetForm: TsmxCustomForm;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetForm(Value: TsmxCustomForm); virtual;
    procedure UnInstallParent; override;
    
    property Cfg: TsmxAlgorithmListCfg read GetCfg;
    property AlgorithmList: TList read FAlgorithmList;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //destructor Destroy; override;
    function AlgorithmButton(ACfgID: Integer): Boolean;
    function AlgorithmMenuItemID(ACfgID: Integer): Integer;
    function FindAlgorithmByCfgID(ACfgID: Integer): TsmxAlgorithm;

    //property AlgorithmButton[CfgID: Integer]: Boolean read GetAlgorithmButton;
    property AlgorithmCount: Integer read GetAlgorithmCount;
    //property AlgorithmMenuItemID[CfgID: Integer]: Integer read GetAlgorithmMenuItemID;
    property Algorithms[Index: Integer]: TsmxAlgorithm read GetAlgorithm; default;
    //property AlgorithmParams[ID: Integer]: Variant read GetAlgorithmParams;
    //property Form: TsmxCustomForm read GetForm; //FForm write SetForm;
  end;

  { TsmxActionList }

  TsmxActionList = class(TsmxCustomAlgorithmList)
  private
    FActionList: TActionList;
  protected
    function GetInternalObject: TObject; override;
    //procedure InstallParent; override;
    //procedure SetForm(Value: TsmxCustomForm); override;
    //procedure SetParentCell(AParent: TsmxBaseCell); override;
    //procedure UnInstallParent; override;

    property ActionList: TActionList read FActionList;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
  end;

  { TsmxCustomMenuItem }

  //TsmxCustomMainMenu = class;

  {TsmxCustomMenuItem = class(TsmxControlCell)
  private
    FMainMenu: TsmxCustomMainMenu;
    FMenuItemList: TList;
    FMenuItemName: String;
    FMenuItemParent: TsmxCustomMenuItem;
    function GetCfg: TsmxMenuItemCfg;
    function GetMenuItem(Index: Integer): TsmxCustomMenuItem;
    function GetMenuItemCount: Integer;
  protected
    //procedure AddMenuItem(AItem: TsmxCustomMenuItem); virtual;
    procedure SetMainMenu(Value: TsmxCustomMainMenu); virtual;
    procedure SetMenuItemParent(Value: TsmxCustomMenuItem); virtual;

    property Cfg: TsmxMenuItemCfg read GetCfg;
    property MenuItemList: TList read FMenuItemList;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;

    property MainMenu: TsmxCustomMainMenu read FMainMenu write SetMainMenu;
    property MenuItemCount: Integer read GetMenuItemCount;
    property MenuItemName: String read FMenuItemName;
    property MenuItemParent: TsmxCustomMenuItem read FMenuItemParent write SetMenuItemParent;
    property MenuItems[Index: Integer]: TsmxCustomMenuItem read GetMenuItem; default;
  end;}

  TsmxCustomMenuItem = class(TsmxControlCell)
  private
    //FMainMenu: TsmxCustomMainMenu;
    //FMenuItemName: String;
    //FMenuItemParent: TsmxCustomMenuItem;
    function GetCfg: TsmxMenuItemCfg;
    //function GetMainMenu: TsmxCustomMainMenu;
  protected
    //procedure SetMainMenu(Value: TsmxCustomMainMenu); virtual;
    //procedure SetMenuItemParent(Value: TsmxCustomMenuItem); virtual;

    property Cfg: TsmxMenuItemCfg read GetCfg;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    procedure AddAlgorithm(Algorithm: TsmxAlgorithm); virtual;
    procedure DelAlgorithm(Algorithm: TsmxAlgorithm); virtual;

    //property MainMenu: TsmxCustomMainMenu read GetMainMenu; //FMainMenu write SetMainMenu;
    //property MenuItemName: String read FMenuItemName;
    //property MenuItemParent: TsmxCustomMenuItem read FMenuItemParent write SetMenuItemParent;
  end;

  { TsmxMenuItem }

  TsmxMenuItem = class(TsmxCustomMenuItem)
  private
    FMenuItem: TMenuItem;
  protected
    function GetInternalObject: TObject; override;
    function GetCellEnable: Boolean; override;
    function GetCellVisible: Boolean; override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellVisible(Value: Boolean); override;
    //procedure SetMainMenu(Value: TsmxCustomMainMenu); override;
    //procedure SetMenuItemParent(Value: TsmxCustomMenuItem); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;

    property MenuItem: TMenuItem read FMenuItem;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    procedure AddAlgorithm(Algorithm: TsmxAlgorithm); override;
    //procedure AddMenuItem(AItem: TsmxCustomMenuItem); override;
    procedure DelAlgorithm(Algorithm: TsmxAlgorithm); override;
  end;

  { TsmxMenuUnitMenuItem }

  {TsmxMenuUnitMenuItem = class(TsmxKitItem)
  private
    FMenuUnit: TsmxHVisibleUnit;
    FMenuItem: TsmxCustomMenuItem;
  public
    constructor Create(AKit: TsmxKit); override;

    property MenuUnit: TsmxHVisibleUnit read FMenuUnit write FMenuUnit;
    property MenuItem: TsmxCustomMenuItem read FMenuItem write FMenuItem;
  end;}

  { TsmxMenuUnitMenuItems }

  {TsmxMenuUnitMenuItems = class(TsmxKit)
  protected
    function GetInternalObject(Index: Integer): TsmxMenuUnitMenuItem;
  public
    function Add: TsmxMenuUnitMenuItem;
    function ItemToUnit(AMenuItem: TsmxCustomMenuItem): TsmxHVisibleUnit;
    function UnitToItem(AMenuUnit: TsmxHVisibleUnit): TsmxCustomMenuItem;

    property Items[Index: Integer]: TsmxMenuUnitMenuItem read GetInternalObject; default;
  end;}

  { TsmxCustomMainMenu }

  TsmxCustomMainMenu = class(TsmxControlCell)
  private
    //FForm: TsmxCustomForm;
    FMenuItemList: TList;
    //FMenuItemRoot: TsmxCustomMenuItem;
    //FMenuUnitMenuItems: TsmxMenuUnitMenuItems;
    function GetCfg: TsmxMainMenuCfg;
    function GetMenuItem(Index: Integer): TsmxCustomMenuItem;
    function GetMenuItemCount: Integer;
    //function GetForm: TsmxCustomForm;
  protected
    procedure CreateChilds; override;
    procedure DestroyChilds; override;
    procedure InitChilds; override;
    procedure InstallParent; override;
    //procedure SetForm(Value: TsmxCustomForm); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxMainMenuCfg read GetCfg;
    property MenuItemList: TList read FMenuItemList;
    //property MenuUnitMenuItems: TsmxMenuUnitMenuItems read FMenuUnitMenuItems;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //procedure AddLibAlg(Alg: TsmxCustomLibAlgorithm); virtual;
    //function FindMenuItem(const AMenuItemName: String): TsmxCustomMenuItem;
    function FindMenuItemByCfgID(ACfgID: Integer): TsmxCustomMenuItem;
    function MenuItemHasParent(ACfgID: Integer): Boolean;

    //property Form: TsmxCustomForm read GetForm; //FForm write SetForm;
    property MenuItemCount: Integer read GetMenuItemCount;
    //property MenuItemRoot: TsmxCustomMenuItem read FMenuItemRoot;
    property MenuItems[Index: Integer]: TsmxCustomMenuItem read GetMenuItem; default;
  end;

  { TsmxMainMenu }

  TsmxMainMenu = class(TsmxCustomMainMenu)
  private
    FMainMenu: TMainMenu;
    //FParent: TCustomForm;
  protected
    //procedure AddItems; virtual;
    function GetInternalObject: TObject; override;
    function GetCellEnable: Boolean; override;
    //function GetItemParent: TObject; override;
    function GetCellVisible: Boolean; override;
    //procedure InstallParent; override;
    //procedure SetForm(Value: TsmxCustomForm); override;
    procedure SetCellEnable(Value: Boolean); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;
    //procedure UnInstallParent; override;

    property MainMenu: TMainMenu read FMainMenu;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    //procedure AddLibAlg(Alg: TsmxCustomLibAlgorithm); override;
    //function FindMenuItem(MenuItemName: String): TMenuItem;
  end;

  { TsmxCustomControlBar }

  TsmxCustomControlBar = class(TsmxControlCell)
  private
    function GetCfg: TsmxControlBarCfg;
  protected
    property Cfg: TsmxControlBarCfg read GetCfg;
  public
    procedure AddAlgorithm(Algorithm: TsmxAlgorithm); virtual;
    procedure DelAlgorithm(Algorithm: TsmxAlgorithm); virtual;
  end;

  { TsmxControlBar }

  TsmxControlBar = class(TsmxCustomControlBar)
  private
    FControlBar: TControlBar;
    FToolBarList: TList;
  protected
    procedure AddToolBar;
    procedure CreateToolBars;
    procedure DestroyToolBars;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(AParent: TsmxBaseCell); override;

    property ControlBar: TControlBar read FControlBar;
    property ToolBarList: TList read FToolBarList write FToolBarList;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    procedure AddAlgorithm(Algorithm: TsmxAlgorithm); override;
    procedure DelAlgorithm(Algorithm: TsmxAlgorithm); override;
  end;

  { TsmxCustomForm }

  //TsmxFormManager = class;

  TsmxCustomForm = class(TsmxControlCell)
  private
    FAlgorithmList: TsmxCustomAlgorithmList;
    FControlBar: TsmxCustomControlBar;
    FFormManager: TsmxFormManager;
    //FFormModalResult: TModalResult;
    FParamList: TStrings;
    FMainMenu: TsmxCustomMainMenu;
    FPageManager: TsmxCustomPageManager;
    FParentForm: TsmxCustomForm;
    function GetCfg: TsmxFormCfg;
    //function GetIsCreateMenuItem: Boolean;
    function GetFormManager: TsmxFormManager;
    //function GetOwnerForm: TsmxCustomForm;
    function GetParamList: TStrings;
    function GetParam(Key: String): String;
    procedure SetParam(Key: String; Value: String);
  protected
    procedure AddAlgorithmsToBar; virtual;
    procedure AddAlgorithmsToMenu; virtual;
    procedure CreateChilds; override;
    procedure DelAlgorithmsToBar; virtual;
    procedure DelAlgorithmsToMenu; virtual;
    procedure DestroyChilds; override;
    //function GetActiveRequest: TsmxCustomRequest; virtual;
    function GetFormModalResult:  TModalResult; virtual;
    procedure InitChilds; override;
    procedure InstallParent; override;
    procedure SetFormModalResult(Value: TModalResult); virtual;
    //procedure SetItemMainMenu(Value: TObject); virtual;
    procedure SetParentForm(Value: TsmxCustomForm); virtual;
    procedure UnInstallParent; override;

    property Cfg: TsmxFormCfg read GetCfg;
    property ParamList: TStrings read GetParamList;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    procedure CloseForm; virtual;
    procedure Prepare(Forcibly: Boolean = False); override;
    //procedure PrepareForm; virtual;
    //procedure RefreshForm; virtual;
    procedure ShowForm; virtual;
    function ShowModalForm: TModalResult; virtual;

    //property ActiveRequest: TsmxCustomRequest read GetActiveRequest;
    property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList;
    property ControlBar: TsmxCustomControlBar read FControlBar;
    property FormManager: TsmxFormManager read GetFormManager;
    property FormModalResult: TModalResult read GetFormModalResult write SetFormModalResult;
    property FormParams[Key: String]: String read GetParam write SetParam;
    //property IsCreateMenuItem: Boolean read GetIsCreateMenuItem;
    property MainMenu: TsmxCustomMainMenu read FMainMenu;
    property ParentForm: TsmxCustomForm read FParentForm write SetParentForm;
    property PageManager: TsmxCustomPageManager read FPageManager;
  end;

  { TsmxStandardForm }

  TsmxStandardForm = class(TsmxCustomForm)
  private
    FForm: TForm;
  protected
    //procedure AddAlgorithmsToTool; virtual;
    procedure CloseProc(Sender: TObject; var Action: TCloseAction); virtual;
    //procedure DelAlgorithmsToTool; virtual;
    function GetFormModalResult:  TModalResult; override;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    //function GetItemMainMenu: TObject; override;
    //function GetItemParent: TObject; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure InstallParent; override;
    procedure SetFormModalResult(Value: TModalResult); override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemMainMenu(Value: TObject); override;
    //procedure SetItemParent(Value: TObject); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    //procedure UnInstallParent; override;

    property Form: TForm read FForm;
  public
    constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    destructor Destroy; override;
    procedure CloseForm; override;
    //procedure PrepareForm; override;
    //procedure RefreshForm; override;
    procedure ShowForm; override;
    function ShowModalForm: TModalResult; override;
  end;

  { TsmxMainForm }

  TsmxMainForm = class(TsmxCustomForm)
  private
    FForm: TForm;
    procedure SetForm(AForm: TForm);
  protected
    //procedure AddAlgorithmsToTool; virtual;
    procedure CloseProc(Sender: TObject; var Action: TCloseAction); virtual;
    procedure CloseQueryProc(Sender: TObject; var CanClose: Boolean); virtual;
    //procedure DelAlgorithmsToTool; virtual;
    //function GetFormModalResult:  TModalResult; override;
    function GetInternalObject: TObject; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellEnable: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetItemMainMenu: TObject; override;
    //function GetItemParent: TObject; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    procedure Initialize; virtual;
    //procedure InstallParent; override;
    //procedure SetFormModalResult(Value: TModalResult); override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellEnable(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    //procedure SetItemMainMenu(Value: TObject); override;
    //procedure SetItemParent(Value: TObject); override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure UnInitialize; virtual;
    //procedure UnInstallParent; override;
  public
    //constructor Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack); override;
    //destructor Destroy; override;
    procedure CloseForm; override;
    //procedure PrepareForm; override;
    //procedure RefreshForm; override;
    procedure ShowForm; override;
    //function ShowModalForm: TModalResult; override;

    property Form: TForm read FForm write SetForm;
  end;

  { TsmxMFormsItem }

  {TsmxMFormsItem = class(TsmxKitItem)
  private
    FFormHandle: HWND;
    FFormPtr: TsmxCustomForm;
  public
    constructor Create(AKit: TsmxKit); override;

    property FormHandle: HWND read FFormHandle write FFormHandle;
    property FormPtr: TsmxCustomForm read FFormPtr write FFormPtr;
  end;}

  { TsmxMFormsItems }

  {TsmxMFormsItems = class(TsmxKit)
  protected
    function GetInternalObject(Index: Integer): TsmxMFormsItem;
  public
    function Add: TsmxMFormsItem;
    function FindByForm(AForm: TsmxCustomForm): TsmxMFormsItem;
    function FindByHandle(AHandle: HWND): TsmxMFormsItem;

    property Items[Index: Integer]: TsmxMFormsItem read GetInternalObject; default;
  end;}

  { TsmxFormManager }

  {TsmxFormManager = class(TsmxComponent)
  private
    FFormList: TsmxMFormsItems;
  protected
    function GetForm(Handle: HWND): TsmxCustomForm;
    //procedure SetForm(Handle: HWND; Value: TsmxCustomForm);
    procedure CloseForms;

    property FormList: TsmxMFormsItems read FFormList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertForm(AForm: TsmxCustomForm);
    procedure RemoveForm(AForm: TsmxCustomForm);

    property Form[Handle: HWND]: TsmxCustomForm read GetForm; default; // write SetForm; default;
  end;}

implementation

uses
  SysUtils, Variants, Graphics, ImgList, ToolWin, smxParams, smxFuncs,
  smxConsts;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

{ TsmxCustomRequest }

{constructor TsmxCustomRequest.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FDataSetIntf := Database.GetNewDataSet(Cfg.DataSetType);
  FDataSetIntf.SQL.Text := Cfg.SQLText;}
  {if AOwner is TsmxCustomPage then
    FPage := TsmxCustomPage(AOwner);}
{end;}

destructor TsmxCustomRequest.Destroy;
begin
  //FDataSetIntf.Close;
  //FDataSetIntf := nil;
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

function TsmxCustomRequest.FindFieldSense(AFieldSense: TsmxFieldSense): IsmxField;
var i: Integer; s: String;
begin
  Result := nil;
  s := '';
  for i := 0 to Cfg.RequestFields.Count - 1 do
    with Cfg.RequestFields[i] do
      if FieldSense = AFieldSense then
      begin
        s := FieldName;
        Break;
      end;
  if s <> '' then
  begin
    Result := GetCellDataSet.FindField(s);
    //if Assigned(f) then
    //  Result := f.Value;
  end; {else
  if (s = '') and (AFieldSense = fsResult) then
  begin
    Result := CellParams['Result'];
    //c := _TsmxBaseCell(GetInternalObject);
    //if c is TsmxTargetRequest then
      //Result := TsmxTargetRequest(c).RParams['Result'];
  end else
  if (s = '') and (AFieldSense = fsMsg) then
  begin
    Result := CellParams['@Msg'];
    //c := _TsmxBaseCell(GetInternalObject);
    //if c is TsmxTargetRequest then
      //Result := TsmxTargetRequest(c).RParams['Msg'];
  end;}
end;

{function TsmxCustomRequest.FindParamLocation(const AParam: IsmxParam): TsmxParamLocation;
var p: TsmxRequestParam;
begin
  Result := plNowhere;
  p := Cfg.RequestParams.FindByName(AParam.ParamName);
  if Assigned(p) then
    Result := p.ParamLocation;
end;}

function TsmxCustomRequest.GetCfg: TsmxRequestCfg;
begin
  Result := TsmxRequestCfg(inherited Cfg);
end;

function TsmxCustomRequest.GetCellDataSet: IsmxDataSet;
begin
  Result := nil;
end;

{function TsmxCustomRequest.GetCellDataSetType: TsmxDataSetType;
begin
  Result := dstUnknown;
end;}

{function TsmxCustomRequest.GetCellParam(Key: String): String;
begin
  Result := '';
end;}

{function TsmxCustomRequest.GetPage: TsmxCustomPage;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomPage then
      Result := TsmxCustomPage(ParentCell);
end;}

{function TsmxCustomRequest.GetPrepared: Boolean;
begin
  Result := False;
end;}

function TsmxCustomRequest.GetParam(Key: String): String;
begin
  Result := ParamList.Values[AnsiUpperCase(Key)];
end;

function TsmxCustomRequest.GetParamList: TStrings;
begin
  if not(Assigned(FParamList)) then
    FParamList := TStringList.Create;
  Result := FParamList;
end;

{function TsmxCustomRequest.GetResultValue: Variant;
begin
  Result := Unassigned;
end;}

procedure TsmxCustomRequest.Perform(Same: Boolean = False);
//var i: Integer;
begin
  {try
    if Assigned(CellDataSet) then
      with CellDataSet do
      begin
        Close;
        if not Prepared then
          Prepare;
        for i := 0 to ParamCount - 1 do
          if Params[i].ParamType in [ptInput, ptInputOutput] then
      begin
        f := nil;
        //s := Params[i].ParamName;
        //if (Length(s) > 0) and (s[1] = '@') then
          //Delete(s, 1, 1);
        if Assigned(DSFrom) then
          f := DSFrom.FindField(Params[i].ParamName);
        if Assigned(f) then
          Params[i].AssignParam(f)
        else
        begin
          if RParams[Params[i].ParamName] = '' then
            Params[i].Value := Null else
            Params[i].Value := RParams[Params[i].ParamName];
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
            RParams[Params[i].ParamName] := '' else
            RParams[Params[i].ParamName] := Params[i].Value;
        end else
        if Params[i].ParamType in [ptResult] then
        begin
          RParams[Params[i].ParamName] := Params[i].Value;
          v := Params[i].Value;
        end;

      case Mode of
        rtOpen: Result := RecordCount > 0;
        rtExecute: Result := v = 0;
      end;
    end;
  end;
    //TargetRequest.PrepRequest(CellDataSet, True, Cfg.Mode);
  except
    raise EsmxCellError.CreateRes(@SCellRequestPerformError);
  end;
  for i := 0 to Cfg.RequestFields.Count - 1 do
    with Cfg.RequestFields[i] do
      CellDataSet.FieldByName(FieldName).DisplayFormat := FieldFormat;}
end;

procedure TsmxCustomRequest.RefreshParams;
var i: integer; v: Variant; c: TsmxBaseCell; f, fp: TsmxCustomForm;
  p: TsmxCustomPage; flt: TsmxCustomFilter; fld: IsmxField;
begin
  c := RootCell;
  f := nil;
  if c is TsmxCustomForm then
    f := TsmxCustomForm(c);
  for i := 0 to Cfg.RequestParams.Count - 1 do
    with Cfg.RequestParams[i] do
    begin
      v := Null;
      case ParamLocation of
        //plNowhere, plOut: v := Null;
        //plConst: v := ParamDefValue;
        plFilterPanel:
        begin
          flt := nil;
          p := nil;
          if Assigned(f) then
            if Assigned(f.PageManager) then
              p := f.PageManager.ActivePage;
          if Assigned(p) then
            if Assigned(p.FilterPanel) then
              flt := p.FilterPanel.FindFilterByName(ParamName);
          if Assigned(flt) then
            v := flt.FilterValue;
        end;
        plGrid:
        begin
          fld := nil;
          p := nil;
          if Assigned(f) then
            if Assigned(f.PageManager) then
              p := f.PageManager.ActivePage;
          if Assigned(p) then
            if Assigned(p.Grid) then
              if Assigned(p.Grid.Request) then
                if Assigned(p.Grid.Request.CellDataSet) then
                  fld := p.Grid.Request.CellDataSet.FindField(ParamName);
          if Assigned(fld) then
            v := fld.Value;
        end;
        plParentFormFilterPanel:
        begin
          flt := nil;
          fp := nil;
          if Assigned(f) then
            fp := f.ParentForm;
          while Assigned(fp) and not Assigned(flt) do
          begin
            p := nil;
            if Assigned(fp.PageManager) then
              p := fp.PageManager.ActivePage;
            if Assigned(p) then
              if Assigned(p.FilterPanel) then
                flt := p.FilterPanel.FindFilterByName(ParamName);
            if Assigned(flt) then
              v := flt.FilterValue;
            fp := fp.ParentForm;
          end;
        end;
        plParentFormGrid:
        begin
          fld := nil;
          fp := nil;
          if Assigned(f) then
            fp := f.ParentForm;
          while Assigned(fp) and not Assigned(fld) do
          begin
            p := nil;
            if Assigned(fp.PageManager) then
              p := fp.PageManager.ActivePage;
            if Assigned(p) then
              if Assigned(p.Grid) then
                if Assigned(p.Grid.Request) then
                  if Assigned(p.Grid.Request.CellDataSet) then
                    fld := p.Grid.Request.CellDataSet.FindField(ParamName);
            if Assigned(fld) then
              v := fld.Value;
            fp := fp.ParentForm;
          end;
        end;
        plFormParams:
        begin
          if Assigned(f) then
            if f.FormParams[ParamName] <> '' then
              v := f.FormParams[ParamName];
        end;
      end;

      if VarIsNull(v) then
      begin
        if VarIsNull(ParamDefValue) then
          RequestParams[ParamName] := '' else
          RequestParams[ParamName] := ParamDefValue;
      end else
        RequestParams[ParamName] := v;
    end;
end;

{procedure TsmxCustomRequest.SetPage(Value: TsmxCustomPage);
begin
  FPage := Value;
end;}

procedure TsmxCustomRequest.SetParam(Key: String; Value: String);
begin
  ParamList.Values[AnsiUpperCase(Key)] := Value;
end;

{ TsmxRequest }

constructor TsmxRequest.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FDataSetIntf := Database.GetNewDataSet(Cfg.DataSetType);
  FDataSetIntf.SQL.Text := Cfg.SQLText;
  //FTargetRequest := TsmxTargetRequest.Create(Self);
  //FTargetRequest.Database := Database;
end;

destructor TsmxRequest.Destroy;
begin
  FDataSetIntf.Close;
  FDataSetIntf := nil;
  //FTargetRequest.Free;
  inherited Destroy;
end;

{function TsmxRequest.GetInternalObject: TObject;
begin
  Result := FTargetRequest;
end;}

function TsmxRequest.GetCellDataSet: IsmxDataSet;
begin
  Result := FDataSetIntf;
end;

{function TsmxRequest.GetCellDataSetType: TsmxDataSetType;
begin
  Result := Cfg.DataSetType;
end;}

{function TsmxRequest.GetCellParam(Key: String): String;
begin
  Result := FTargetRequest.RParams[Key];
end;}

{function TsmxRequest.GetPrepared: Boolean;
begin
  Result := FDataSetIntf.Prepared;
end;}

{function TsmxRequest.GetResultValue: Variant;
var i: Integer; s: String; f: IsmxField;
begin
  Result := Unassigned;
  s := '';
  for i := 0 to Cfg.RequestFields.Count - 1 do
    with Cfg.RequestFields[i] do
      if FieldSense = fsResult then
      begin
        s := FieldName;
        Break;
      end;
  if s <> '' then
  begin
    f := FDataSetIntf.FindField(s);
    if Assigned(f) then
      Result := f.Value;
  end;
end;}

{procedure TsmxRequest.Perform;
var i: integer; f, fp: TsmxCustomForm; p: TsmxCustomPage; flt: TsmxCustomFilter;
  fld: IsmxField; v: Variant;
begin
  //TargetRequest.ClearParams;
  f := nil;
  if RootCell is TsmxCustomForm then
    f := TsmxCustomForm(RootCell);
  if Assigned(f) then
  for i := 0 to Cfg.RequestParams.Count - 1 do
    with Cfg.RequestParams[i] do
    begin
      v := Null;
      p := nil;
      flt := nil;
      fld := nil;
      case ParamLocation of
        //plNowhere, plOut: v := Null;
        plConst: v := ParamDefValue;
        plFilterPanel:
        begin
          //v := Null;
          //if VarIsNull(v) then
            //TargetRequest[ParamName] := '' else
            //TargetRequest[ParamName] := v;

          if Assigned(f.PageManager) then
            p := f.PageManager.ActivePage;
          if Assigned(p) then
            if Assigned(p.FilterPanel) then
              flt := p.FilterPanel.FindFilterByName(ParamName);
          if Assigned(flt) then
            v := flt.FilterValue;
        end;
        plGrid:
        begin
          if Assigned(f.PageManager) then
            p := f.PageManager.ActivePage;
          if Assigned(p) then
            if Assigned(p.Grid) then
              if Assigned(p.Grid.Request) then
                if Assigned(p.Grid.Request.CellDataSet) then
                  fld := p.Grid.Request.CellDataSet.FindField(ParamName);
          if Assigned(fld) then
            v := fld.Value;
        end;
        plParentFormFilterPanel:
        begin
          //v := Null;

          //if VarIsNull(v) then
            //TargetRequest[ParamName] := '' else
            //TargetRequest[ParamName] := v;

          fp := f.ParentForm;
          while Assigned(fp) and not Assigned(flt) do
          begin
            if Assigned(fp.PageManager) then
              p := fp.PageManager.ActivePage;
            if Assigned(p) then
              if Assigned(p.FilterPanel) then
                flt := p.FilterPanel.FindFilterByName(ParamName);
            if Assigned(flt) then
              v := flt.FilterValue;
            fp := fp.ParentForm;
          end;
        end;
        plParentFormGrid:
        begin
          //ds := nil;

          fp := f.ParentForm;
          while Assigned(fp) and not Assigned(fld) do
          begin
            if Assigned(fp.PageManager) then
              p := fp.PageManager.ActivePage;
            if Assigned(p) then
              if Assigned(p.Grid) then
                if Assigned(p.Grid.Request) then
                  if Assigned(p.Grid.Request.CellDataSet) then
                    fld := p.Grid.Request.CellDataSet.FindField(ParamName);
            if Assigned(fld) then
              v := fld.Value;
            fp := fp.ParentForm;
          end;
        end;
        plFormParams:
        begin
          v := f.FormParams[ParamName];
          if v = '' then
            v := Null;
        end;
      end;
      //if (TargetRequest[ParamName] = '') and not VarIsNull(ParamDefValue) then
        //TargetRequest[ParamName] := ParamDefValue;
      if VarIsNull(v) then
      begin
        if VarIsNull(ParamDefValue) then
          TargetRequest[ParamName] := '' else
          TargetRequest[ParamName] := ParamDefValue;
      end else
        TargetRequest[ParamName] := v;
    end;
  //g := nil;
  //if ParentCell is TsmxCustomPage then
    //g := TsmxCustomPage(ParentCell).Grid;
  try
    //if Assigned(g) then
      //g.CellCursor := crSQLWait;
    //TargetRequest.PrepRequest(CellDataSet, True, Cfg.Mode, ds);
    TargetRequest.PrepRequest(CellDataSet, True, Cfg.Mode);
  finally
    //if Assigned(g) then
      //g.CellCursor := crDefault;
  end;
  for i := 0 to Cfg.RequestFields.Count - 1 do
    with Cfg.RequestFields[i] do
      //DataSet.Fields[i].DisplayFormat := FieldFormat;
      CellDataSet.FieldByName(FieldName).DisplayFormat := FieldFormat;
end;}

procedure TsmxRequest.Perform(Same: Boolean = False);
var i: Integer;
begin
  if not Same then
    RefreshParams;
  with CellDataSet do
  begin
    Close;
    if not Prepared then
      Prepare;
    for i := 0 to ParamCount - 1 do
      if Params[i].ParamType in [ptInput, ptInputOutput] then
      begin
        if RequestParams[Params[i].ParamName] = '' then
          Params[i].Value := Null else
          Params[i].Value := RequestParams[Params[i].ParamName];
      end;
    try
      case Cfg.Mode of
        rtOpen: Open;
        rtExecute: Execute;
      end;
    except
      raise EsmxCellError.CreateRes(@SCellRequestPerformError);
    end;
    for i := 0 to ParamCount - 1 do
      if Params[i].ParamType in [ptOutput, ptInputOutput, ptResult] then
      begin
        if VarIsNull(Params[i].Value) then
          RequestParams[Params[i].ParamName] := '' else
          RequestParams[Params[i].ParamName] := Params[i].Value;
      end;
    for i := 0 to Cfg.RequestFields.Count - 1 do
      with Cfg.RequestFields[i] do
        FieldByName(FieldName).DisplayFormat := FieldFormat;
  end;

  {if not Same then
    RefreshParams;
  with CellDataSet do
  begin
    Close;
    if not Prepared then
      Prepare;
    for i := 0 to Cfg.RequestParams.Count - 1 do
      with Cfg.RequestParams[i] do
        if RequestParams[ParamName] = '' then
          ParamByName(ParamName).Value := Null else
          ParamByName(ParamName).Value := RequestParams[ParamName];
    try
      case Cfg.Mode of
        rtOpen: Open;
        rtExecute: Execute;
      end;
    except
      raise EsmxCellError.CreateRes(@SCellRequestPerformError);
    end;
    for i := 0 to Cfg.RequestParams.Count - 1 do
      with Cfg.RequestParams[i] do
        if VarIsNull(ParamByName(ParamName).Value) then
          RequestParams[ParamName] := '' else
          RequestParams[ParamName] := ParamByName(ParamName).Value;
    for i := 0 to Cfg.RequestFields.Count - 1 do
      with Cfg.RequestFields[i] do
        FieldByName(FieldName).DisplayFormat := FieldFormat;
  end;}
end;

procedure TsmxRequest.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TCustomDBGrid then
      if Assigned(TCustomDBGrid(c).DataSource) then
        TCustomDBGrid(c).DataSource.DataSet := nil;
  end;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TCustomDBGrid then
      if Assigned(TCustomDBGrid(c).DataSource) and (FDataSetIntf.GetDataSet is TDataSet) then
        TCustomDBGrid(c).DataSource.DataSet := TDataSet(FDataSetIntf.GetDataSet);
  end;
end;

{ TsmxCustomColumn }

function TsmxCustomColumn.GetCfg: TsmxColumnCfg;
begin
  Result := TsmxColumnCfg(inherited Cfg);
end;

{function TsmxCustomColumn.GetGrid: TsmxCustomGrid;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomGrid then
      Result := TsmxCustomGrid(ParentCell);
end;}

{procedure TsmxCustomColumn.SetGrid(Value: TsmxCustomGrid);
begin
  FGrid := Value;
end;}

{ TsmxDBColumn }

constructor TsmxDBColumn.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FColumn := TColumn.Create(nil);
  FColumn.FieldName := Cfg.ColumnFieldName;
  FColumn.Alignment := Cfg.ColumnText.Align;
  FColumn.Color := Cfg.ColumnText.Color;
  //FColumn.Width := Cfg.ColumnWidth;
  FColumn.Font.Color := Cfg.ColumnText.Font.Color;
  FColumn.Font.Name := Cfg.ColumnText.Font.Name;
  FColumn.Font.Size := Cfg.ColumnText.Font.Size;
  FColumn.Font.Style := Cfg.ColumnText.Font.Style;
  FColumn.Title.Caption := Cfg.ColumnTitle.Text;
  FColumn.Title.Alignment := Cfg.ColumnTitle.Align;
  FColumn.Title.Color := Cfg.ColumnTitle.Color;
  FColumn.Title.Font.Color := Cfg.ColumnTitle.Font.Color;
  FColumn.Title.Font.Name := Cfg.ColumnTitle.Font.Name;
  FColumn.Title.Font.Size := Cfg.ColumnTitle.Font.Size;
  FColumn.Title.Font.Style := Cfg.ColumnTitle.Font.Style;
end;

destructor TsmxDBColumn.Destroy;
begin
  FColumn.Free;
  inherited Destroy;
end;

function TsmxDBColumn.GetInternalObject: TObject;
begin
  Result := FColumn;
end;

{function TsmxDBColumn.GetCellAlign: TAlign;
begin
  Result := FColumn.Align;
end;}

{function TsmxDBColumn.GetCellEnable: Boolean;
begin
  Result := FColumn.Enabled;
end;}

{function TsmxDBColumn.GetCellHeight: Integer;
begin
  Result := FColumn.Height;
end;}

{function TsmxDBColumn.GetCellLeft: Integer;
begin
  Result := FColumn.Left;
end;}

{function TsmxPanelFilter.GetItemParent: TObject;
begin
  Result := FPanel.Parent;
end;}

{function TsmxDBColumn.GetCellTop: Integer;
begin
  Result := FColumn.Top;
end;}

function TsmxDBColumn.GetCellVisible: Boolean;
begin
  Result := FColumn.Visible;
end;

function TsmxDBColumn.GetCellWidth: Integer;
begin
  Result := FColumn.Width;
end;

{procedure TsmxDBColumn.SetGrid(Value: TsmxCustomGrid);
var c: TObject;
begin
  if Assigned(FGrid) then
    FColumn.Collection := nil;
  inherited SetGrid(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TDBGrid then
        FColumn.Collection := TDBGrid(c).Columns;
  end;
end;}

{procedure TsmxDBColumn.SetCellAlign(Value: TAlign);
begin
  FColumn.Align := Value;
end;}

{procedure TsmxDBColumn.SetCellEnable(Value: Boolean);
begin
  FColumn.Enabled := Value;
end;}

{procedure TsmxDBColumn.SetCellHeight(Value: Integer);
begin
  FColumn.Height := Value;
end;}

{procedure TsmxDBColumn.SetCellLeft(Value: Integer);
begin
  FColumn.Left := Value;
end;}

{procedure TsmxPanelFilter.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FPanel.Parent := TWinControl(Value) else
    FPanel.Parent := nil;
end;}

{procedure TsmxDBColumn.SetCellTop(Value: Integer);
begin
  FColumn.Top := Value;
end;}

procedure TsmxDBColumn.SetCellVisible(Value: Boolean);
begin
  FColumn.Visible := Value;
end;

procedure TsmxDBColumn.SetCellWidth(Value: Integer);
begin
  FColumn.Width := Value;
end;

procedure TsmxDBColumn.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FColumn.Collection := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TDBGrid then
        FColumn.Collection := TDBGrid(c).Columns;
  end;
end;

{ TsmxCustomGrid }

{constructor TsmxCustomGrid.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  if AOwner is TsmxCustomPage then
    FPage := TsmxCustomPage(AOwner);
end;}

{procedure TsmxCustomGrid.ApplyGrid;
begin
end;}

procedure TsmxCustomGrid.CreateChilds;
var i: Integer;
begin
  with Cfg.Request do
    if ID > 0 then
      FRequest := TsmxCustomRequest(NewCell(Self, ID, Call));
  FColumnList := TList.Create;
  FColumnList.Count := Cfg.GridColumns.Count;
  for i := 0 to Cfg.GridColumns.Count - 1 do
    with Cfg.GridColumns[i] do
      if ID > 0 then
      begin
        //FColumnList[i] := IDToItemClass(ID, Call).Create(Self, ID, Call);
        FColumnList[i] := NewCell(Self, ID, Call);
        //TsmxCustomPage(FPageList[i]).PageManager := Self;
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

procedure TsmxCustomGrid.DestroyChilds;
var i: Integer;
begin
  if Assigned(FRequest) then
    FRequest.Free;
  for i := FColumnList.Count - 1 downto 0 do
  begin
    TsmxCustomColumn(FColumnList[i]).Free;
    FColumnList.Delete(i);
  end;
  FColumnList.Free;
end;

function TsmxCustomGrid.GetCfg: TsmxGridCfg;
begin
  Result := TsmxGridCfg(inherited Cfg);
end;

function TsmxCustomGrid.GetColumn(Index: Integer): TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(FColumnList[Index]);
end;

function TsmxCustomGrid.GetColumnCount: Integer;
begin
  Result := FColumnList.Count;
end;

{function TsmxCustomGrid.GetPage: TsmxCustomPage;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomPage then
      Result := TsmxCustomPage(ParentCell);
end;}

{function TsmxCustomGrid.GetRequestMode: TsmxOperationMode;
begin
  Result := Cfg.Request.Mode;
end;}

procedure TsmxCustomGrid.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.GridColumns.Count - 1 do
    with Cfg.GridColumns[i] do
    begin
      Columns[i].CellAlign := UnitAlign;
      Columns[i].CellEnable := UnitEnable;
      Columns[i].CellHeight := UnitHeight;
      Columns[i].CellLeft := UnitLeft;
      Columns[i].CellTop := UnitTop;
      Columns[i].CellVisible := UnitVisible;
      Columns[i].CellWidth := UnitWidth;
    end;
end;

procedure TsmxCustomGrid.InstallParent;
var i: Integer;
begin
  FRequest.ParentCell := Self;
  for i := 0 to ColumnCount - 1 do
    with Columns[i] do
      //Grid := Self;
      ParentCell := Self;
end;

procedure TsmxCustomGrid.Prepare(Forcibly: Boolean = False);
begin
  if Assigned(Request) then
    if Assigned(Request.CellDataSet) then
      if not(Request.CellDataSet.Prepared) and (Cfg.Request.Mode = omAutomatic) or Forcibly then
        Request.Perform;
end;

{procedure TsmxCustomGrid.PrepareGrid;
begin
  if Assigned(Request) then
    if Assigned(Request.CellDataSet) then
      if not(Request.CellDataSet.Prepared) and (Cfg.Request.Mode = omAutomatic) then
        Request.Perform;
end;}

{procedure TsmxCustomGrid.RefreshGrid;
begin
  if Assigned(Request) then
    Request.Perform;
end;}

{procedure TsmxCustomGrid.SetPage(Value: TsmxCustomPage);
begin
  FPage := Value;
end;}

{procedure TsmxCustomGrid.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
end;}

procedure TsmxCustomGrid.UnInstallParent;
var i: Integer;
begin
  FRequest.ParentCell := nil;
  for i := 0 to ColumnCount - 1 do
    with Columns[i] do
      //Grid := nil;
      ParentCell := nil;
end;

{ TsmxDBGrid }

constructor TsmxDBGrid.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FDataSource := TDataSource.Create(Self);
  FGrid := TsmxWheelDBGrid.Create(Self); //TDBGrid.Create(Self);
  FGrid.DataSource := FDataSource;
  FGrid.Options := FGrid.Options - [dgEditing];
  //FGrid.OnMouseWheelDown := WheelDownProc;
  //FGrid.OnMouseWheelUp := WheelUpProc;
  InstallParent;
  //AddColumns;
end;

destructor TsmxDBGrid.Destroy;
begin
  //FGrid.OnMouseWheelDown := nil;
  //FGrid.OnMouseWheelUp := nil;
  UnInstallParent;
  FGrid.Free;
  FDataSource.Free;
  inherited Destroy;
end;

{procedure TsmxDBGrid.AddColumns;
var i: Integer;
begin
  FGrid.Columns.Clear;
  for i := 0 to Cfg.GridColumns.Count - 1 do
    with FGrid.Columns.Add do
    begin
      //FieldName := Cfg.GridColumns[i].Column.Text;
      FieldName := Cfg.GridColumns[i].FieldName;
      Alignment := Cfg.GridColumns[i].Column.Align;
      Color := TColor(Cfg.GridColumns[i].Column.Color);
      Width := Cfg.GridColumns[i].Width;
      Font.Color := TColor(Cfg.GridColumns[i].Column.Font.Color);
      Font.Name := Cfg.GridColumns[i].Column.Font.Name;
      Font.Size := Cfg.GridColumns[i].Column.Font.Size;
      Font.Style := Cfg.GridColumns[i].Column.Font.Style;
      Title.Caption := Cfg.GridColumns[i].Title.Text;
      Title.Alignment := Cfg.GridColumns[i].Title.Align;
      Title.Color := TColor(Cfg.GridColumns[i].Title.Color);
      Title.Font.Color := TColor(Cfg.GridColumns[i].Title.Font.Color);
      Title.Font.Name := Cfg.GridColumns[i].Title.Font.Name;
      Title.Font.Size := Cfg.GridColumns[i].Title.Font.Size;
      Title.Font.Style := Cfg.GridColumns[i].Title.Font.Style;
    end;
end;}

{procedure TsmxDBGrid.ApplyGrid;
begin
end;}

function TsmxDBGrid.GetInternalObject: TObject;
begin
  Result := FGrid;
end;

function TsmxDBGrid.GetCellAlign: TAlign;
begin
  Result := FGrid.Align;
end;

function TsmxDBGrid.GetCellCursor: TCursor;
begin
  Result := FGrid.Cursor;
end;

function TsmxDBGrid.GetCellEnable: Boolean;
begin
  Result := FGrid.Enabled;
end;

function TsmxDBGrid.GetCellHeight: Integer;
begin
  Result := FGrid.Height;
end;

function TsmxDBGrid.GetCellLeft: Integer;
begin
  Result := FGrid.Left;
end;

{function TsmxDBGrid.GetItemParent: TObject;
begin
  Result := FGrid.Parent;
end;}

function TsmxDBGrid.GetCellTop: Integer;
begin
  Result := FGrid.Top;
end;

function TsmxDBGrid.GetCellVisible: Boolean;
begin
  Result := FGrid.Visible;
end;

function TsmxDBGrid.GetCellWidth: Integer;
begin
  Result := FGrid.Width;
end;

{procedure TsmxDBGrid.InstallParent;
var i: Integer;
begin
  FRequest.ParentCell := Self;
  for i := 0 to ColumnCount - 1 do
    with Columns[i] do
      //Grid := Self;
      ParentCell := Self;
end;}

{procedure TsmxDBGrid.PrepareGrid;
begin
  if Assigned(FRequest) then
    if not(FRequest.CellDataSet.Prepared) and (Cfg.Request.Mode = omAutomatic) then
      FRequest.Perform;
end;}

{procedure TsmxDBGrid.RefreshGrid;
begin
  if Assigned(FRequest) then
    FRequest.Perform;
end;}

procedure TsmxDBGrid.SetCellAlign(Value: TAlign);
begin
  FGrid.Align := Value;
end;

procedure TsmxDBGrid.SetCellCursor(Value: TCursor);
begin
  FGrid.Cursor := Value;
end;

procedure TsmxDBGrid.SetCellEnable(Value: Boolean);
begin
  FGrid.Enabled := Value;
end;

procedure TsmxDBGrid.SetCellHeight(Value: Integer);
begin
  FGrid.Height := Value;
end;

procedure TsmxDBGrid.SetCellLeft(Value: Integer);
begin
  FGrid.Left := Value;
end;

{procedure TsmxDBGrid.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FGrid.Parent := TWinControl(Value) else
    FGrid.Parent := nil;
end;}

procedure TsmxDBGrid.SetCellTop(Value: Integer);
begin
  FGrid.Top := Value;
end;

procedure TsmxDBGrid.SetCellVisible(Value: Boolean);
begin
  FGrid.Visible := Value;
end;

procedure TsmxDBGrid.SetCellWidth(Value: Integer);
begin
  FGrid.Width := Value;
end;

{procedure TsmxDBGrid.SetPage(Value: TsmxCustomPage);
var c: TObject;
begin
  if Assigned(FPage) then
    FGrid.Parent := nil;
  inherited SetPage(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        FGrid.Parent := TWinControl(c);
  end;
end;}

procedure TsmxDBGrid.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FGrid.Parent := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TWinControl then
        FGrid.Parent := TWinControl(c);
  end;
end;

{procedure TsmxDBGrid.SetRequest(Value: TsmxCustomRequest);
var ds: TObject;
begin
  if Assigned(FRequest) then
    FDataSource.DataSet := nil;
  inherited SetRequest(Value);
  if Assigned(Value) then
  begin
    ds := Value.GetCellDataSet.GetDataSet;
    if Assigned(ds) then
      if ds is TDataSet then
        FDataSource.DataSet := TDataSet(ds);
  end;
end;}

{procedure TsmxDBGrid.UnInstallParent;
var i: Integer;
begin
  FRequest.ParentCell := nil;
  for i := 0 to ColumnCount - 1 do
    with Columns[i] do
      //Grid := nil;
      ParentCell := nil;
end;}

{procedure TsmxDBGrid.WheelDownProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  with Grid.DataSource do
    if Assigned(DataSet) then
      if DataSet.Active then
        if DataSet.RecNo < DataSet.RecordCount then
          DataSet.MoveBy(1);
  Handled := True;
end;}

{procedure TsmxDBGrid.WheelUpProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  with Grid.DataSource do
    if Assigned(DataSet) then
      if DataSet.Active then
        if DataSet.RecNo > 0 then
          DataSet.MoveBy(-1);
  Handled := True;
end;}

{ TsmxCustomFilter }

{constructor TsmxCustomFilter.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FFilterName := Cfg.FilterName;
end;}

procedure TsmxCustomFilter.CreateChilds;
begin
  with Cfg.Algorithm do
    if ID > 0 then
      FAlgorithm := TsmxAlgorithm(NewCell(Self, ID, Call));
      //FAlgorithmList := TsmxCustomAlgorithmList(NewCell(Self, ID, Call));
  with Cfg.Request do
    if ID > 0 then
      FRequest := TsmxCustomRequest(NewCell(Self, ID, Call));
end;

procedure TsmxCustomFilter.DestroyChilds;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.Free;
  if Assigned(FRequest) then
    FRequest.Free;
end;

{function TsmxCustomFilter.GetAlgorithmID: Integer;
begin
  Result := Cfg.AlgorithmID;
end;}

{function TsmxCustomFilter.GetAlgorithmParams: Variant;
begin
  Result := Cfg.Algorithm.Params;
end;}

{function TsmxCustomFilter.GetFilterCaption: String;
begin
  Result := '';
end;}

function TsmxCustomFilter.GetFilterName: String;
begin
  Result := Cfg.FilterName;
end;

function TsmxCustomFilter.GetFilterValue: Variant;
begin
  Result := Null;
end;

function TsmxCustomFilter.GetCfg: TsmxFilterCfg;
begin
  Result := TsmxFilterCfg(inherited Cfg);
end;

{function TsmxCustomFilter.GetFilterPanel: TsmxCustomFilterPanel;
//var c: TsmxBaseCell;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomFilterPanel then
      Result := TsmxCustomFilterPanel(ParentCell);
  //Result := nil;
  //c := Self;
  //while Assigned(c.ParentCell) and not(c is TsmxCustomFilterPanel) do
    //c := c.ParentCell;
  //if c is TsmxCustomFilterPanel then
    //Result := TsmxCustomFilterPanel(c);
end;}

procedure TsmxCustomFilter.InitChilds;
begin
  if Assigned(FAlgorithm) then
    with Cfg.Algorithm do
    begin
      FAlgorithm.CellCaption := Caption;
      FAlgorithm.CellEnable := Enable;
      FAlgorithm.CellHotKey := HotKey;
      FAlgorithm.CellVisible := Visible;
    end;
end;

procedure TsmxCustomFilter.InstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := Self;
  if Assigned(FRequest) then
    FRequest.ParentCell := Self;
end;

{procedure TsmxCustomFilter.SetAlgorithm(Value: TsmxAlgorithm);
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.AlgorithmCell := nil;
  FAlgorithm := Value;
  if Assigned(Value) then
    Value.AlgorithmCell := Self;
end;}

{procedure TsmxCustomFilter.SetFilterCaption(Value: String);
begin
end;}

procedure TsmxCustomFilter.SetFilterValue(Value: Variant);
begin
end;

{procedure TsmxCustomFilter.SetFilterPanel(Value: TsmxCustomFilterPanel);
begin
  FFilterPanel := Value;
end;}

procedure TsmxCustomFilter.UnInstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := nil;
  if Assigned(FRequest) then
    FRequest.ParentCell := nil;
end;

{ TsmxPanelFilter }

constructor TsmxPanelFilter.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FPanel := TPanel.Create(Self);
  FPanel.Height := 49;
  FPanel.BevelOuter := bvNone;
  FHeader := TLabel.Create(Self);
  FHeader.Parent := FPanel;
  FHeader.Font.Color := TColor(Cfg.FilterHeader.Font.Color);
  FHeader.Font.Name := Cfg.FilterHeader.Font.Name;
  FHeader.Font.Size := Cfg.FilterHeader.Font.Size;
  FHeader.Font.Style := Cfg.FilterHeader.Font.Style;
  FHeader.Caption := Cfg.FilterHeader.Text;
  FHeader.Color := TColor(Cfg.FilterHeader.Color);
  case Cfg.FilterHeader.Align of
    taLeftJustify:
    begin
      FHeader.Anchors := [akLeft];
      FHeader.Left := 4;
    end;
    taRightJustify:
    begin
      FHeader.Anchors := [akRight];
      FHeader.Left := FPanel.Width - FHeader.Width - 4;
    end;
    taCenter:
    begin
      FHeader.Anchors := [akLeft, akRight];
      FHeader.Left := (FPanel.Width - FHeader.Width) div 2;
    end;
  end;
  FHeader.Top := 4;
  //InstallParent;
end;

destructor TsmxPanelFilter.Destroy;
begin
  //UnInstallParent;
  FHeader.Parent := nil;
  FHeader.Free;
  FPanel.Free;
  inherited Destroy;
end;

{function TsmxPanelFilter.GetInternalObject: TObject;
begin
  Result := FPanel;
end;}

function TsmxPanelFilter.GetCellAlign: TAlign;
begin
  Result := FPanel.Align;
end;

function TsmxPanelFilter.GetCellEnable: Boolean;
begin
  Result := FPanel.Enabled;
end;

function TsmxPanelFilter.GetCellHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TsmxPanelFilter.GetCellLeft: Integer;
begin
  Result := FPanel.Left;
end;

{function TsmxPanelFilter.GetItemParent: TObject;
begin
  Result := FPanel.Parent;
end;}

function TsmxPanelFilter.GetCellTop: Integer;
begin
  Result := FPanel.Top;
end;

function TsmxPanelFilter.GetCellVisible: Boolean;
begin
  Result := FPanel.Visible;
end;

function TsmxPanelFilter.GetCellWidth: Integer;
begin
  Result := FPanel.Width;
end;

{procedure TsmxPanelFilter.InstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := Self;
  if Assigned(FRequest) then
    FRequest.ParentCell := Self;
end;}

procedure TsmxPanelFilter.SetCellAlign(Value: TAlign);
begin
  FPanel.Align := Value;
end;

procedure TsmxPanelFilter.SetCellEnable(Value: Boolean);
begin
  FPanel.Enabled := Value;
end;

procedure TsmxPanelFilter.SetCellHeight(Value: Integer);
begin
  FPanel.Height := Value;
end;

procedure TsmxPanelFilter.SetCellLeft(Value: Integer);
begin
  FPanel.Left := Value;
end;

{procedure TsmxPanelFilter.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FPanel.Parent := TWinControl(Value) else
    FPanel.Parent := nil;
end;}

procedure TsmxPanelFilter.SetCellTop(Value: Integer);
begin
  FPanel.Top := Value;
end;

procedure TsmxPanelFilter.SetCellVisible(Value: Boolean);
begin
  FPanel.Visible := Value;
end;

procedure TsmxPanelFilter.SetCellWidth(Value: Integer);
begin
  FPanel.Width := Value;
end;

{procedure TsmxPanelFilter.SetPanelFilters(Value: TsmxCustomFilterPanel);
var c: TObject;
begin
  if Assigned(FFilterPanel) then
    FPanel.Parent := nil;
  inherited SetPanelFilters(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        FPanel.Parent := TWinControl(c);
  end;
end;}

procedure TsmxPanelFilter.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPanel.Parent := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TWinControl then
        FPanel.Parent := TWinControl(c);
  end;
end;

{procedure TsmxPanelFilter.UnInstallParent;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ParentCell := nil;
  if Assigned(FRequest) then
    FRequest.ParentCell := nil;
end;}

{ TsmxCustomFilterPanel }

{constructor TsmxCustomFilterPanel.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  if AOwner is TsmxCustomPage then
    FPage := TsmxCustomPage(AOwner);
end;}

{procedure TsmxCustomFilterPanel.ApplyFilterPanel;
begin
end;}

procedure TsmxCustomFilterPanel.CreateChilds;
var i: Integer;
begin
  with Cfg.Request do
    if ID > 0 then
      FRequest := TsmxCustomRequest(NewCell(Self, ID, Call));
  FFilterList := TList.Create;
  FFilterList.Count := Cfg.Params.Count;
  for i := 0 to Cfg.Params.Count - 1 do
    with Cfg.Params[i] do
      if ID > 0 then
      begin
        //FFilterList[i] := IDToItemClass(ID, Call).Create(Self, ID, Call);
        FFilterList[i] := NewCell(Self, ID, Call);
        //TsmxCustomFilter(FFilterList[i]).FilterPanel := Self;
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
  {with Cfg.AlgorithmList do
    if ID > 0 then
      FAlgorithmList := TsmxCustomAlgorithmList(NewCell(Self, ID, Call));}
end;

procedure TsmxCustomFilterPanel.DestroyChilds;
var i: Integer;
begin
  if Assigned(FRequest) then
    FRequest.Free;
  for i := FFilterList.Count - 1 downto 0 do
  begin
    TsmxCustomFilter(FFilterList[i]).Free;
    FFilterList.Delete(i);
  end;
  FFilterList.Free;
  {if Assigned(FAlgorithmList) then
    FAlgorithmList.Free;}
end;

function TsmxCustomFilterPanel.FindFilterByName(const AFilterName: String): TsmxCustomFilter;
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

function TsmxCustomFilterPanel.GetCfg: TsmxFilterPanelCfg;
begin
  Result := TsmxFilterPanelCfg(inherited Cfg);
end;

function TsmxCustomFilterPanel.GetFilter(Index: Integer): TsmxCustomFilter;
begin
  Result := TsmxCustomFilter(FFilterList[Index]);
end;

function TsmxCustomFilterPanel.GetFilterCount: Integer;
begin
  Result := FFilterList.Count;
end;

{function TsmxCustomFilterPanel.GetPage: TsmxCustomPage;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomPage then
      Result := TsmxCustomPage(ParentCell);
end;}

{function TsmxCustomFilterPanel.GetRequestMode: TsmxOperationMode;
begin
  Result := Cfg.Request.Mode;
end;}

procedure TsmxCustomFilterPanel.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.Params.Count - 1 do
    with Cfg.Params[i] do
    begin
      Filters[i].CellAlign := UnitAlign;
      Filters[i].CellEnable := UnitEnable;
      Filters[i].CellHeight := UnitHeight;
      Filters[i].CellLeft := UnitLeft;
      Filters[i].CellTop := UnitTop;
      Filters[i].CellVisible := UnitVisible;
      Filters[i].CellWidth := UnitWidth;
      //if Filters[i].AlgorithmID > 0 then
      //  Filters[i].Algorithm := AlgorithmList.FindAlgorithmByID(Filters[i].AlgorithmID);
      {if Assigned(Filters[i].Algorithm) then
      begin
        Filters[i].Algorithm.CellEnable := UnitEnable;
        Filters[i].Algorithm.CellVisible := UnitVisible;
      end;}
    end;
end;

procedure TsmxCustomFilterPanel.InstallParent;
var i: Integer;
begin
  {for i := 0 to FilterCount - 1 do
    with Filters[i] do
      ItemParent := FPanel;}
  for i := 0 to FilterCount - 1 do
    with Filters[i] do
      //FilterPanel := Self;
      ParentCell := Self;
  if Assigned(FRequest) then
    FRequest.ParentCell := Self;
end;

procedure TsmxCustomFilterPanel.Prepare(Forcibly: Boolean = False);
var c: TsmxBaseCell; i: Integer; fk: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.CellDataSet) then
      if not(Request.CellDataSet.Prepared) and (Cfg.Request.Mode = omAutomatic) or Forcibly then
      begin
        Request.Perform;
        c := RootCell;
        if c is TsmxCustomForm then
          for i := 0 to Request.CellDataSet.FieldCount - 1 do
            with Request.CellDataSet.Fields[i] do
              if VarIsNull(Value) then
                TsmxCustomForm(c).FormParams[FieldName] := '' else
                TsmxCustomForm(c).FormParams[FieldName] := Value;
        for i := 0 to FilterCount - 1 do
        begin
          fk := Request.CellDataSet.FindField(Filters[i].FilterName);
          if Assigned(fk) then
            Filters[i].FilterValue := fk.Value else
            Filters[i].FilterValue := Null;
        end;
      end;
end;

{procedure TsmxCustomFilterPanel.PrepareFilterPanel;
var c: TsmxBaseCell; i: Integer; fk: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.CellDataSet) then
      if not(Request.CellDataSet.Prepared) and (Cfg.Request.Mode = omAutomatic) then
      begin
        Request.Perform;
        c := RootCell;
        if c is TsmxCustomForm then
          for i := 0 to Request.CellDataSet.FieldCount - 1 do
            with Request.CellDataSet.Fields[i] do
              if not VarIsNull(Value) then
                TsmxCustomForm(c).FormParams[FieldName] := Value;
        for i := 0 to FilterCount - 1 do
        begin
          fk := Request.CellDataSet.FindField(Filters[i].FilterName);
          if Assigned(fk) then
            Filters[i].FilterValue := fk.Value;
        end;
      end;
end;}

{procedure TsmxCustomFilterPanel.RefreshFilterPanel;
var c: TsmxBaseCell; i: Integer; fk: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.CellDataSet) then
    begin
      Request.Perform;
      c := RootCell;
      if c is TsmxCustomForm then
        for i := 0 to Request.CellDataSet.FieldCount - 1 do
          with Request.CellDataSet.Fields[i] do
            if not VarIsNull(Value) then
              TsmxCustomForm(c).FormParams[FieldName] := Value;
      for i := 0 to FilterCount - 1 do
      begin
        fk := FRequest.CellDataSet.FindField(Filters[i].FilterName);
        if Assigned(fk) then
          Filters[i].FilterValue := fk.Value;
      end;
    end;
end;}

{procedure TsmxCustomFilterPanel.SetPage(Value: TsmxCustomPage);
begin
  FPage := Value;
end;}

procedure TsmxCustomFilterPanel.UnInstallParent;
var i: Integer;
begin
  {for i := 0 to FilterCount - 1 do
    with Filters[i] do
      ItemParent := nil;}
  for i := 0 to FilterCount - 1 do
    with Filters[i] do
      //FilterPanel := nil;
      ParentCell := nil;
  if Assigned(FRequest) then
    FRequest.ParentCell := nil;
end;

{ TsmxFilterPanel }

constructor TsmxFilterPanel.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FPanel := TPanel.Create(Self);
  FPanel.BevelOuter := bvNone;
  InstallParent;
end;

destructor TsmxFilterPanel.Destroy;
begin
  UnInstallParent;
  FPanel.Free;
  inherited Destroy;
end;

{procedure TsmxFilterPanel.ApplyFilterPanel;
begin
end;}

function TsmxFilterPanel.GetInternalObject: TObject;
begin
  Result := FPanel;
end;

function TsmxFilterPanel.GetCellAlign: TAlign;
begin
  Result := FPanel.Align;
end;

function TsmxFilterPanel.GetCellEnable: Boolean;
begin
  Result := FPanel.Enabled;
end;

function TsmxFilterPanel.GetCellHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TsmxFilterPanel.GetCellLeft: Integer;
begin
  Result := FPanel.Left;
end;

{function TsmxFilterPanel.GetItemParent: TObject;
begin
  Result := FPanel.Parent;
end;}

function TsmxFilterPanel.GetCellTop: Integer;
begin
  Result := FPanel.Top;
end;

function TsmxFilterPanel.GetCellVisible: Boolean;
begin
  Result := FPanel.Visible;
end;

function TsmxFilterPanel.GetCellWidth: Integer;
begin
  Result := FPanel.Width;
end;

{procedure TsmxFilterPanel.InstallParent;
var i: Integer;
begin
  for i := 0 to FilterCount - 1 do
    with Filters[i] do
      //FilterPanel := Self;
      ParentCell := Self;
  if Assigned(FRequest) then
    FRequest.ParentCell := Self;
end;}

{procedure TsmxFilterPanel.PrepareFilterPanel;
var i: Integer; fk: IsmxField;
begin
  if Assigned(FRequest) then
    if not(FRequest.CellDataSet.Prepared) and (Cfg.Request.Mode = omAutomatic) then
    begin
      FRequest.Perform;
      for i := 0 to FilterCount - 1 do
      begin
        fk := FRequest.CellDataSet.FindField(Filters[i].FilterName);
        if Assigned(fk) then
          Filters[i].FilterValue := fk.Value;
      end;
    end;
end;}

{procedure TsmxFilterPanel.RefreshFilterPanel;
var i: Integer; fk: IsmxField;
begin
  if Assigned(FRequest) then
  begin
    FRequest.Perform;
    for i := 0 to FilterCount - 1 do
    begin
      fk := FRequest.CellDataSet.FindField(Filters[i].FilterName);
      if Assigned(fk) then
        Filters[i].FilterValue := fk.Value;
    end;
  end;
end;}

procedure TsmxFilterPanel.SetCellAlign(Value: TAlign);
begin
  FPanel.Align := Value;
end;

procedure TsmxFilterPanel.SetCellEnable(Value: Boolean);
begin
  FPanel.Enabled := Value;
end;

procedure TsmxFilterPanel.SetCellHeight(Value: Integer);
begin
  FPanel.Height := Value;
end;

procedure TsmxFilterPanel.SetCellLeft(Value: Integer);
begin
  FPanel.Left := Value;
end;

{procedure TsmxFilterPanel.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FPanel.Parent := TWinControl(Value) else
    FPanel.Parent := nil;
end;}

procedure TsmxFilterPanel.SetCellTop(Value: Integer);
begin
  FPanel.Top := Value;
end;

procedure TsmxFilterPanel.SetCellVisible(Value: Boolean);
begin
  FPanel.Visible := Value;
end;

procedure TsmxFilterPanel.SetCellWidth(Value: Integer);
begin
  FPanel.Width := Value;
end;

{procedure TsmxFilterPanel.SetPage(Value: TsmxCustomPage);
var c: TObject;
begin
  if Assigned(FPage) then
    FPanel.Parent := nil;
  inherited SetPage(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        FPanel.Parent := TWinControl(c);
  end;
end;}

procedure TsmxFilterPanel.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPanel.Parent := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TWinControl then
        FPanel.Parent := TWinControl(c);
  end;
end;

{procedure TsmxFilterPanel.UnInstallParent;
var i: Integer;
begin
  for i := 0 to FilterCount - 1 do
    with Filters[i] do
      //FilterPanel := nil;
      ParentCell := nil;
  if Assigned(FRequest) then
    FRequest.ParentCell := nil;
end;}

{ TsmxCustomPage }

{constructor TsmxCustomPage.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  if AOwner is TsmxCustomPageManager then
    FPageManager := TsmxCustomPageManager(AOwner);
end;}

{destructor TsmxCustomPage.Destroy;
begin
  FGrid.Free;
  FRequest.Free;
  inherited Destroy;
end;}

procedure TsmxCustomPage.CreateChilds;
begin
  {with Cfg.PageRequest do
    if ID > 0 then
    begin
      //FRequest := TsmxCustomRequest(IDToItemClass(ID, Call).Create(Self, ID, Call));
      FRequest := TsmxCustomRequest(NewCell(Self, ID, Call));
      //FRequest.Page := Self;
    end; //else
      //raise EsmxItemError.CreateRes(@SItemBuildError);}
  with Cfg.Grid do
    if ID > 0 then
    begin
      //if Assigned(FRequest) then
      //begin
        //FGrid := TsmxCustomGrid(IDToItemClass(ID, Call).Create(Self, ID, Call));
        FGrid := TsmxCustomGrid(NewCell(Self, ID, Call));
        //FGrid.Page := Self;

        //FGrid.Request := FRequest;

      //end else
        //raise EsmxItemError.CreateRes(@SItemBuildError);
    end; //else
      //raise EsmxItemError.CreateRes(@SItemBuildError);
  with Cfg.FilterPanel do
    if ID > 0 then
    begin
      //FFilterPanel := TsmxCustomFilterPanel(IDToItemClass(ID, Call).Create(Self, ID, Call));
      FFilterPanel := TsmxCustomFilterPanel(NewCell(Self, ID, Call));
      //FFilterPanel.Page := Self;
    end;
end;

procedure TsmxCustomPage.DestroyChilds;
begin
  if Assigned(FGrid) then
    FGrid.Free;
  {if Assigned(FRequest) then
    FRequest.Free;}
  if Assigned(FFilterPanel) then
    FFilterPanel.Free;
end;

function TsmxCustomPage.GetCfg: TsmxPageCfg;
begin
  Result := TsmxPageCfg(inherited Cfg);
end;

{function TsmxCustomPage.GetPageManager: TsmxCustomPageManager;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomPageManager then
      Result := TsmxCustomPageManager(ParentCell);
end;}

{function TsmxCustomPage.GetRequestMode: TsmxOperationMode;
begin
  Result := Cfg.PageRequest.Mode;
end;}

procedure TsmxCustomPage.InitChilds;
begin
  if Assigned(FGrid) then
  begin
    //FGrid.Request := FRequest;
    with Cfg.Grid do
    begin
      FGrid.CellAlign := Align;
      FGrid.CellEnable := Enable;
      FGrid.CellVisible := Visible;
      with PositionSize do
      begin
        FGrid.CellHeight := Height;
        FGrid.CellLeft := Left;
        FGrid.CellTop := Top;
        FGrid.CellWidth := Width;
      end;
    end;
  end;
  if Assigned(FFilterPanel) then
    with Cfg.FilterPanel do
    begin
      FFilterPanel.CellAlign := Align;
      FFilterPanel.CellEnable := Enable;
      FFilterPanel.CellVisible := Visible;
      with PositionSize do
      begin
        FFilterPanel.CellHeight := Height;
        FFilterPanel.CellLeft := Left;
        FFilterPanel.CellTop := Top;
        FFilterPanel.CellWidth := Width;
      end;
    end;
end;

procedure TsmxCustomPage.InstallParent;
begin
  {if Assigned(FRequest) then
    //FRequest.Page := Self;
    FRequest.ParentCell := Self;}
  if Assigned(FGrid) then
    //FGrid.Page := Self;
    FGrid.ParentCell := Self;
  if Assigned(FFilterPanel) then
    //FFilterPanel.Page := Self;
    FFilterPanel.ParentCell := Self;
end;

{procedure TsmxCustomPage.SetManagerPages(Value: TsmxCustomPageManager);
begin
  FPageManager := Value;
end;}

procedure TsmxCustomPage.UnInstallParent;
begin
  {if Assigned(FRequest) then
    //FRequest.Page := nil;
    FRequest.ParentCell := nil;}
  if Assigned(FGrid) then
    //FGrid.Page := nil;
    FGrid.ParentCell := nil;
  if Assigned(FFilterPanel) then
    //FFilterPanel.Page := nil;
    FFilterPanel.ParentCell := nil;
end;

{ TsmxTabSheet }

constructor TsmxTabSheet.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FPage := TTabSheet.Create(Self);
  FPage.Caption := Cfg.PageCaption;
  FPage.ImageIndex := TImageIndex(Cfg.PageImageIndex);
  InstallParent;
end;

destructor TsmxTabSheet.Destroy;
begin
  UnInstallParent;
  FPage.Free;
  inherited Destroy;
end;

function TsmxTabSheet.GetInternalObject: TObject;
begin
  Result := FPage;
end;

function TsmxTabSheet.GetCellAlign: TAlign;
begin
  Result := FPage.Align;
end;

function TsmxTabSheet.GetCellEnable: Boolean;
begin
  Result := FPage.Enabled;
end;

function TsmxTabSheet.GetCellHeight: Integer;
begin
  Result := FPage.Height;
end;

function TsmxTabSheet.GetCellLeft: Integer;
begin
  Result := FPage.Left;
end;

{function TsmxTabSheet.GetItemParent: TObject;
begin
  Result := FPage.Parent;
end;}

function TsmxTabSheet.GetCellTop: Integer;
begin
  Result := FPage.Top;
end;

function TsmxTabSheet.GetCellVisible: Boolean;
begin
  Result := FPage.TabVisible; //Result := FPage.Visible;
end;

function TsmxTabSheet.GetCellWidth: Integer;
begin
  Result := FPage.Width;
end;

{procedure TsmxTabSheet.InstallParent;
begin
  if Assigned(FGrid) then
    //FGrid.Page := Self;
    FGrid.ParentCell := Self;
  if Assigned(FFilterPanel) then
    //FFilterPanel.Page := Self;
    FFilterPanel.ParentCell := Self;
end;}

procedure TsmxTabSheet.SetCellAlign(Value: TAlign);
begin
  FPage.Align := Value;
end;

procedure TsmxTabSheet.SetCellEnable(Value: Boolean);
begin
  FPage.Enabled := Value;
end;

procedure TsmxTabSheet.SetCellHeight(Value: Integer);
begin
  FPage.Height := Value;
end;

procedure TsmxTabSheet.SetCellLeft(Value: Integer);
begin
  FPage.Left := Value;
end;

{procedure TsmxTabSheet.SetItemParent(Value: TObject);
begin
  if Value is TPageControl then
    FPage.PageControl := TPageControl(Value) else
    FPage.PageControl := nil;
end;}

procedure TsmxTabSheet.SetCellTop(Value: Integer);
begin
  FPage.Top := Value;
end;

procedure TsmxTabSheet.SetCellVisible(Value: Boolean);
begin
  FPage.TabVisible := Value; //FPage.Visible := Value;
end;

procedure TsmxTabSheet.SetCellWidth(Value: Integer);
begin
  FPage.Width := Value;
end;

{procedure TsmxTabSheet.SetManagerPages(Value: TsmxCustomPageManager);
var c: TObject;
begin
  if Assigned(FPageManager) then
    FPage.PageControl := nil;
  inherited SetManagerPages(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TPageControl then
        FPage.PageControl := TPageControl(c);
  end;
end;}

procedure TsmxTabSheet.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPage.PageControl := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TPageControl then
        FPage.PageControl := TPageControl(c);
  end;
end;

{procedure TsmxTabSheet.UnInstallParent;
begin
  if Assigned(FGrid) then
    //FGrid.Page := nil;
    FGrid.ParentCell := nil;
  if Assigned(FFilterPanel) then
    //FFilterPanel.Page := nil;
    FFilterPanel.ParentCell := nil;
end;}

{ TsmxCustomPageManager }

{constructor TsmxCustomPageManager.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  if AOwner is TsmxCustomForm then
    FForm := TsmxCustomForm(AOwner);
end;}

{destructor TsmxCustomPageManager.Destroy;
begin
  FPageList.Free;
  inherited Destroy;
end;}

procedure TsmxCustomPageManager.CreateChilds;
var i: Integer;
begin
  FPageList := TList.Create;
  FPageList.Count := Cfg.Sheets.Count;
  for i := 0 to Cfg.Sheets.Count - 1 do
    with Cfg.Sheets[i] do
      if ID > 0 then
      begin
        //FPageList[i] := IDToItemClass(ID, Call).Create(Self, ID, Call);
        FPageList[i] := NewCell(Self, ID, Call);
        //TsmxCustomPage(FPageList[i]).PageManager := Self;
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

procedure TsmxCustomPageManager.DestroyChilds;
var i: Integer;
begin
  for i := FPageList.Count - 1 downto 0 do
  begin
    TsmxCustomPage(FPageList[i]).Free;
    FPageList.Delete(i);
  end;
  FPageList.Free;
end;

function TsmxCustomPageManager.GetActivePage: TsmxCustomPage;
begin
  Result := nil;
end;

function TsmxCustomPageManager.GetCfg: TsmxPageManagerCfg;
begin
  Result := TsmxPageManagerCfg(inherited Cfg);
end;

{function TsmxCustomPageManager.GetForm: TsmxCustomForm;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomForm then
      Result := TsmxCustomForm(ParentCell);
end;}

function TsmxCustomPageManager.GetPage(Index: Integer): TsmxCustomPage;
begin
  Result := TsmxCustomPage(FPageList[Index]);
end;

function TsmxCustomPageManager.GetPageCount: Integer;
begin
  Result := FPageList.Count;
end;

procedure TsmxCustomPageManager.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.Sheets.Count - 1 do
    with Cfg.Sheets[i] do
    begin
      Pages[i].CellAlign := UnitAlign;
      Pages[i].CellEnable := UnitEnable;
      Pages[i].CellHeight := UnitHeight;
      Pages[i].CellLeft := UnitLeft;
      Pages[i].CellTop := UnitTop;
      Pages[i].CellVisible := UnitVisible;
      Pages[i].CellWidth := UnitWidth;
    end;
end;

procedure TsmxCustomPageManager.InstallParent;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    with Pages[i] do
      ParentCell := Self;
end;

{procedure TsmxCustomPageManager.SetForm(Value: TsmxCustomForm);
begin
  FForm := Value;
end;}

procedure TsmxCustomPageManager.UnInstallParent;
var i: Integer;
begin
  for i := 0 to PageCount - 1 do
    with Pages[i] do
      ParentCell := nil;
end;

{ TsmxPageControl }

constructor TsmxPageControl.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FPageControl := TPageControl.Create(Self);
  FPageControl.Images := ImageList;
  InstallParent;
end;

destructor TsmxPageControl.Destroy;
begin
  UnInstallParent;
  FPageControl.Free;
  inherited Destroy;
end;

function TsmxPageControl.GetActivePage: TsmxCustomPage;
begin
  Result := Pages[FPageControl.ActivePageIndex];
end;

function TsmxPageControl.GetInternalObject: TObject;
begin
  Result := FPageControl;
end;

function TsmxPageControl.GetCellAlign: TAlign;
begin
  Result := FPageControl.Align;
end;

function TsmxPageControl.GetCellEnable: Boolean;
begin
  Result := FPageControl.Enabled;
end;

function TsmxPageControl.GetCellHeight: Integer;
begin
  Result := FPageControl.Height;
end;

function TsmxPageControl.GetCellLeft: Integer;
begin
  Result := FPageControl.Left;
end;

{function TsmxPageControl.GetItemParent: TObject;
begin
  Result := FPageControl.Parent;
end;}

function TsmxPageControl.GetCellTop: Integer;
begin
  Result := FPageControl.Top;
end;

function TsmxPageControl.GetCellVisible: Boolean;
begin
  Result := FPageControl.Visible;
end;

function TsmxPageControl.GetCellWidth: Integer;
begin
  Result := FPageControl.Width;
end;

procedure TsmxPageControl.InstallParent;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FPageControl.Parent := f;
    inherited InstallParent;
    FPageControl.Parent := nil;
  finally
    f.Free;
  end;
end;

{procedure TsmxPageControl.SetForm(Value: TsmxCustomForm);
var c: TObject;
begin
  if Assigned(FForm) then
    FPageControl.Parent := nil;
  inherited SetForm(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        FPageControl.Parent := TWinControl(c);
  end;
end;}

procedure TsmxPageControl.SetCellAlign(Value: TAlign);
begin
  FPageControl.Align := Value;
end;

procedure TsmxPageControl.SetCellEnable(Value: Boolean);
begin
  FPageControl.Enabled := Value;
end;

procedure TsmxPageControl.SetCellHeight(Value: Integer);
begin
  FPageControl.Height := Value;
end;

procedure TsmxPageControl.SetCellLeft(Value: Integer);
begin
  FPageControl.Left := Value;
end;

{procedure TsmxPageControl.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FPageControl.Parent := TWinControl(Value) else
    FPageControl.Parent := nil;
end;}

procedure TsmxPageControl.SetCellTop(Value: Integer);
begin
  FPageControl.Top := Value;
end;

procedure TsmxPageControl.SetCellVisible(Value: Boolean);
begin
  FPageControl.Visible := Value;
end;

procedure TsmxPageControl.SetCellWidth(Value: Integer);
begin
  FPageControl.Width := Value;
end;

procedure TsmxPageControl.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPageControl.Parent := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TWinControl then
        FPageControl.Parent := TWinControl(c);
  end;
end;

procedure TsmxPageControl.UnInstallParent;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FPageControl.Parent := f;
    inherited UnInstallParent;
    FPageControl.Parent := nil;
  finally
    f.Free;
  end;
end;

{ TsmxAlgorithm }

{procedure TsmxAlgorithm.Execute;
begin
end;}

{function TsmxAlgorithm.GetAlgorithmList: TsmxCustomAlgorithmList;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomAlgorithmList then
      Result := TsmxCustomAlgorithmList(ParentCell);
end;}

{function TsmxAlgorithm.GetCellCaption: String;
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
end;}

{procedure TsmxAlgorithm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
end;}

{procedure TsmxAlgorithm.SetCellCaption(Value: String);
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
end;}

{ TsmxCustomLibAlgorithm }

{constructor TsmxCustomLibAlgorithm.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  LoadLib;
end;}

{destructor TsmxCustomLibAlgorithm.Destroy;
begin
  UnLoadLib;
  inherited Destroy;
end;}

procedure TsmxCustomLibAlgorithm.Execute;
begin
  ExecProc(nil);
end;

procedure TsmxCustomLibAlgorithm.ExecProc(Sender: TObject);
//var v: Variant;
begin
  if Assigned(FLibProc) then
  //if Assigned(FLibFunc) then
  begin
    //if Assigned(AlgorithmList) then
    {if ParentCell is TsmxCustomAlgorithmList then
      v := TsmxCustomAlgorithmList(ParentCell).AlgorithmParams[ID] else
    if ParentCell is TsmxCustomFilter then
      v := TsmxCustomFilter(ParentCell).AlgorithmParams else}

    //v := CellParams;

    //FLibProc(Self, v);
    //ResultParams := FLibFunc(Self, v);

    //v := FLibFunc(Self, CellParams);
    FLibProc(Self, CellParams);
    //if Assigned(AlgorithmCell) then
      //AlgorithmCell.PutParams(v);
    //if ParentCell is TsmxCustomFilter then
      //TsmxCustomFilter(ParentCell).FilterValue := v;
        
    {if ParentCell is TsmxCustomFilter then
    begin
      if VarIsArray(v) then
        if VarArrayHighBound(v, 1) = 1 then
        begin
          TsmxCustomFilter(ParentCell).FilterValue := v[0];
          TsmxCustomFilter(ParentCell).FilterCaption := v[1];
        end;
    end;}
  end;
end;

function TsmxCustomLibAlgorithm.GetCfg: TsmxLibAlgorithmCfg;
begin
  Result := TsmxLibAlgorithmCfg(inherited Cfg);
end;

function TsmxCustomLibAlgorithm.GetCellParams: Variant;
//var i: Integer;
begin
  {Result := Unassigned;
  if Cfg.AlgParams.Count > 0 then
  begin
    Result := VarArrayCreate([0, Cfg.AlgParams.Count - 1], varVariant);
    for i := 0 to Cfg.AlgParams.Count - 1 do
      Result[i] := Cfg.AlgParams[i].ParamValue;
  end;}
  Result := ParamsToVar(Cfg.AlgParams);
end;

{function TsmxCustomLibAlgorithm.GetCellEnable: Boolean;
begin
  Result := False;
end;}

function TsmxCustomLibAlgorithm.GetCellImageIndex: Integer;
begin
  Result := Cfg.AlgImageIndex;
end;

{function TsmxCustomLibAlgorithm.GetCellVisible: Boolean;
begin
  Result := False;
end;}

function TsmxCustomLibAlgorithm.GetLibManager: TsmxLibManager;
begin
  if not(Assigned(FLibManager)) then
    FLibManager := TsmxLibManager(Integer(Call(3)));
  Result := FLibManager;
end;

procedure TsmxCustomLibAlgorithm.LoadLib;
//var lm: TsmxLibManager; //f: TsmxFuncManagerActions;
begin
  //FLibHandle := LoadLibrary(PChar(Cfg.AlgLibrary));
  if not Assigned(LibManager) then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  FLibHandle := LibManager[Cfg.AlgLibrary];
  if FLibHandle = 0 then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  @FLibProc := GetProcAddress(FLibHandle, PChar(Cfg.AlgProcedure));
  //@FLibFunc := GetProcAddress(FLibHandle, PChar(Cfg.AlgProcedure));
  if not(Assigned(FLibProc)) then
  //if not(Assigned(FLibFunc)) then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  //@FLibInitProc := GetProcAddress(FLibHandle, PChar('InitDLL'));
  //if Assigned(FLibInitProc) then
    //FLibInitProc(Call);
  //@f := GetProcAddress(FLibHandle, PChar('ManagerActions'));
  //if Assigned(f) then
    //FLibManagerActions := f;
end;

{procedure TsmxCustomLibAlgorithm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
end;}

{procedure TsmxCustomLibAlgorithm.SetCellEnable(Value: Boolean);
begin
end;

procedure TsmxCustomLibAlgorithm.SetCellVisible(Value: Boolean);
begin
end;}

procedure TsmxCustomLibAlgorithm.UnLoadLib;
begin
  //FLibManagerActions := nil;
  //FLibInitProc := nil;
  FLibProc := nil;
  //FLibFunc := nil;
  {if FLibHandle <> 0 then
    FreeLibrary(FLibHandle);}
  FLibHandle := 0;
end;

{ TsmxLibAlgorithm }

constructor TsmxLibAlgorithm.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FAction := TAction.Create(Self);
  FAction.Caption := Cfg.AlgCaption;
  FAction.Hint := Cfg.AlgCaption;
  FAction.ShortCut := TShortCut(Cfg.AlgHotKey);
  FAction.ImageIndex := TImageIndex(Cfg.AlgImageIndex);
  FAction.OnExecute := ExecProc; //FLibProc;
  LoadLib;
  //if Assigned(FLibManagerActions) then
  //  FLibManagerActions.InsertAction(Self);
  //AddEvent;
  //FAction.ActionComponent.
end;

destructor TsmxLibAlgorithm.Destroy;
begin
  UnLoadLib;
  //DelEvent;
  //if Assigned(FLibManagerActions) then
    //FLibManagerActions.RemoveAction(Self);
  FAction.OnExecute := nil;
  FAction.Free;
  inherited Destroy;
end;

{procedure TsmxLibAlgorithm.AddEvent;
begin
  FLibHandle := LoadLibrary(PChar(Cfg.AlgLibrary));
  if FLibHandle = 0 then
    raise EsmxItemError.CreateRes(@SItemBuildError);
  @FLibProc := GetProcAddress(FLibHandle, PChar(Cfg.AlgProcedure));
  if not(Assigned(FLibProc)) then
    raise EsmxItemError.CreateRes(@SItemBuildError);
  FAction.OnExecute := FLibProc;
end;}

{procedure TsmxLibAlgorithm.DelEvent;
begin
  FAction.OnExecute := nil;
  FLibProc := nil;
  if FLibHandle <> 0 then
    FreeLibrary(FLibHandle);
end;}

function TsmxLibAlgorithm.GetInternalObject: TObject;
begin
  Result := FAction;
end;

function TsmxLibAlgorithm.GetCellCaption: String;
begin
  Result := FAction.Caption;
end;

function TsmxLibAlgorithm.GetCellEnable: Boolean;
begin
  Result := FAction.Enabled;
end;

function TsmxLibAlgorithm.GetCellHotKey: Integer;
begin
  Result := Integer(FAction.ShortCut);
end;

function TsmxLibAlgorithm.GetCellVisible: Boolean;
begin
  Result := FAction.Visible;
end;

{procedure TsmxLibAlgorithm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
var c: TObject;
begin
  if Assigned(FAlgorithmList) then
    FAction.ActionList := nil;
  inherited SetAlgorithmList(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TCustomActionList then
        FAction.ActionList := TCustomActionList(c);
  end;
end;}

procedure TsmxLibAlgorithm.SetCellCaption(Value: String);
begin
  FAction.Caption := Value;
end;

procedure TsmxLibAlgorithm.SetCellEnable(Value: Boolean);
begin
  FAction.Enabled := Value;
end;

procedure TsmxLibAlgorithm.SetCellHotKey(Value: Integer);
begin
  FAction.ShortCut := TShortCut(Value);
end;

procedure TsmxLibAlgorithm.SetCellVisible(Value: Boolean);
begin
  FAction.Visible := Value;
end;

procedure TsmxLibAlgorithm.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
  begin
    if ParentCell is TsmxCustomAlgorithmList then
    begin
      FAction.ActionList := nil;
    end else
    //if ParentCell is TsmxControlCell then
    if ParentCell is TsmxCustomFilter then
    begin
      c := _TsmxBaseCell(ParentCell).GetInternalObject;
      if c is TControl then
        TControl(c).Action := nil;
    end;
  end;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    if AParent is TsmxCustomAlgorithmList then
    begin
      c := _TsmxBaseCell(AParent).GetInternalObject;
      if c is TCustomActionList then
        FAction.ActionList := TCustomActionList(c);
    end else
    //if AParent is TsmxControlCell then
    if AParent is TsmxCustomFilter then
    begin
      c := _TsmxBaseCell(AParent).GetInternalObject;
      if c is TControl then
        TControl(c).Action := FAction;
    end;
  end;
end;

{ TsmxCustomAlgorithmList }

function TsmxCustomAlgorithmList.AlgorithmButton(ACfgID: Integer): Boolean;
var c: TsmxAlgorithmItem;
begin
  Result := False;
  c := Cfg.AlgorithmItems.FindByID(ACfgID);
  if Assigned(c) then
    Result := c.AlgorithmButton;
end;

function TsmxCustomAlgorithmList.AlgorithmMenuItemID(ACfgID: Integer): Integer;
var c: TsmxAlgorithmItem;
begin
  Result := 0;
  c := Cfg.AlgorithmItems.FindByID(ACfgID);
  if Assigned(c) then
    Result := c.AlgorithmMenuItemID;
end;

procedure TsmxCustomAlgorithmList.CreateChilds;
var i: Integer;
begin
  FAlgorithmList := TList.Create;
  FAlgorithmList.Count := Cfg.AlgorithmItems.Count;
  for i := 0 to Cfg.AlgorithmItems.Count - 1 do
    with Cfg.AlgorithmItems[i] do
      if ID > 0 then
      begin
        //FAlgorithmList[i] := IDToItemClass(ID, Call).Create(Self, ID, Call);
        FAlgorithmList[i] := NewCell(Self, ID, Call);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

procedure TsmxCustomAlgorithmList.DestroyChilds;
var i: Integer;
begin
  for i := FAlgorithmList.Count - 1 downto 0 do
  begin
    TsmxAlgorithmItem(FAlgorithmList[i]).Free;
    FAlgorithmList.Delete(i);
  end;
  FAlgorithmList.Free;
end;

function TsmxCustomAlgorithmList.FindAlgorithmByCfgID(ACfgID: Integer): TsmxAlgorithm;
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

function TsmxCustomAlgorithmList.GetAlgorithm(Index: Integer): TsmxAlgorithm;
begin
  Result := TsmxAlgorithm(FAlgorithmList[Index]);
end;

{function TsmxCustomAlgorithmList.GetAlgorithmButton(CfgID: Integer): Boolean;
var c: TsmxAlgorithmItem;
begin
  Result := False;
  c := Cfg.AlgorithmItems.FindByID(CfgID);
  if Assigned(c) then
    Result := c.AlgorithmButton;
end;}

function TsmxCustomAlgorithmList.GetAlgorithmCount: Integer;
begin
  Result := FAlgorithmList.Count;
end;

{function TsmxCustomAlgorithmList.GetAlgorithmMenuItemID(CfgID: Integer): Integer;
var c: TsmxAlgorithmItem;
begin
  //Result := Cfg.AlgorithmItems[Index].AlgorithmMenuItemID;
  Result := 0;
  c := Cfg.AlgorithmItems.FindByID(CfgID);
  if Assigned(c) then
    Result := c.AlgorithmMenuItemID;
end;}

{function TsmxCustomAlgorithmList.GetAlgorithmParams(ID: Integer): Variant;
var c: TsmxAlgorithmItem;
begin
  Result := Unassigned;
  c := Cfg.AlgorithmItems.FindByID(ID);
  if Assigned(c) then
    Result := c.AlgorithmParams;
end;}

function TsmxCustomAlgorithmList.GetCfg: TsmxAlgorithmListCfg;
begin
  Result := TsmxAlgorithmListCfg(inherited Cfg);
end;

{function TsmxCustomAlgorithmList.GetForm: TsmxCustomForm;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomForm then
      Result := TsmxCustomForm(ParentCell);
end;}

procedure TsmxCustomAlgorithmList.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.AlgorithmItems.Count - 1 do
    with Cfg.AlgorithmItems[i] do
    begin
      Algorithms[i].CellEnable := AlgorithmEnable;
      Algorithms[i].CellVisible := AlgorithmVisible;
      Algorithms[i].CellHotKey := AlgorithmHotKey;
      Algorithms[i].CellCaption := AlgorithmCaption;
    end;
end;

procedure TsmxCustomAlgorithmList.InstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    with Algorithms[i] do
      //AlgorithmList := Self;
      ParentCell := Self;
end;

{procedure TsmxCustomAlgorithmList.SetForm(Value: TsmxCustomForm);
begin
  FForm := Value;
end;}

procedure TsmxCustomAlgorithmList.UnInstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    with Algorithms[i] do
      //AlgorithmList := nil;
      ParentCell := nil;
end;

{ TsmxAlgorithmList }

constructor TsmxActionList.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FActionList := TActionList.Create(Self);
  FActionList.Images := ImageList;
  InstallParent;
end;

destructor TsmxActionList.Destroy;
begin
  UnInstallParent;
  FActionList.Free;
  inherited Destroy;
end;

function TsmxActionList.GetInternalObject: TObject;
begin
  Result := FActionList;
end;

{procedure TsmxActionList.InstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    with Algorithms[i] do
      //AlgorithmList := Self;
      ParentCell := Self;
end;}

{procedure TsmxActionList.SetForm(Value: TsmxCustomForm);
var i: Integer; mi: TsmxCustomMenuItem;
begin
  if Assigned(FForm) then
  begin
    if Assigned(FForm.MainMenu) then
    begin
      for i := 0 to Cfg.AlgorithmItems.Count - 1 do
        with Cfg.AlgorithmItems[i] do
        begin
          mi := FForm.MainMenu.FindMenuItem(AlgorithmMenuItemName);
          if Assigned(mi) then
            mi.DelAlgorithm(Algorithms[i]);
        end;
    end;
  end;
  inherited SetForm(Value);
  if Assigned(Value) then
  begin
    if Assigned(Value.MainMenu) then
    begin
      for i := 0 to Cfg.AlgorithmItems.Count - 1 do
        with Cfg.AlgorithmItems[i] do
        begin
          mi := FForm.MainMenu.FindMenuItem(AlgorithmMenuItemName);
          if Assigned(mi) then
            mi.AddAlgorithm(Algorithms[i]);
        end;
    end;
  end;
end;}

{procedure TsmxActionList.SetParentCell(AParent: TsmxBaseCell);
var i: Integer; mi: TsmxCustomMenuItem; f: TsmxCustomForm;
begin
  if Assigned(ParentCell) then
  begin
    if ParentCell is TsmxCustomForm then
    begin
      f := TsmxCustomForm(ParentCell);
      if Assigned(f.MainMenu) and f.IsCreateMenuItem then
      begin
        for i := Cfg.AlgorithmItems.Count - 1 downto 0 do
          with Cfg.AlgorithmItems[i] do
          begin
            mi := f.MainMenu.FindMenuItemByID(AlgorithmMenuItemID);
            if Assigned(mi) then
              mi.DelAlgorithm(Algorithms[i]);
          end;
      end;
    end;
  end;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    if AParent is TsmxCustomForm then
    begin
      f := TsmxCustomForm(ParentCell);
      if Assigned(f.MainMenu) and f.IsCreateMenuItem then
      begin
        for i := 0 to Cfg.AlgorithmItems.Count - 1 do
          with Cfg.AlgorithmItems[i] do
          begin
            mi := f.MainMenu.FindMenuItemByID(AlgorithmMenuItemID);
            if Assigned(mi) then
              mi.AddAlgorithm(Algorithms[i]);
          end;
      end;
    end;
  end;
end;}

{procedure TsmxActionList.UnInstallParent;
var i: Integer;
begin
  for i := 0 to AlgorithmCount - 1 do
    with Algorithms[i] do
      //AlgorithmList := nil;
      ParentCell := nil;
end;}

{ TsmxCustomMenuItem }

{constructor TsmxCustomMenuItem.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FMenuItemList := TList.Create;
  FMenuItemName := Cfg.ItemName;
end;

destructor TsmxCustomMenuItem.Destroy;
var i: Integer;
begin
  if Assigned(FMenuItemParent) then
    FMenuItemParent.MenuItemList.Remove(Self);
  for i := FMenuItemList.Count - 1 downto 0 do
  begin
    TsmxCustomMenuItem(FMenuItemList[i]).Free;
    FMenuItemList.Delete(i);
  end;
  FMenuItemList.Free;
  inherited Destroy;
end;}

{procedure TsmxCustomMenuItem.AddMenuItem(AItem: TsmxCustomMenuItem);
begin
  if not(Assigned(FMenuItemList)) then
    FMenuItemList := TList.Create;
  FMenuItemList.Add(AItem);
  AItem.FMenuItemParent := Self;
  AItem.FMainMenu := MainMenu;
  //AItem.SetMenuItemParent(Self);
end;}

{function TsmxCustomMenuItem.GetCfg: TsmxMenuItemCfg;
begin
  Result := TsmxMenuItemCfg(inherited Cfg);
end;

function TsmxCustomMenuItem.GetMenuItem(Index: Integer): TsmxCustomMenuItem;
begin
  //if Assigned(FMenuItemList) then
    Result := TsmxCustomMenuItem(FMenuItemList[Index]); //else
    //raise EsmxKitItemError.CreateRes(@SKitItemIndexError);
end;

function TsmxCustomMenuItem.GetMenuItemCount: Integer;
begin
  //if Assigned(FMenuItemList) then
    Result := FMenuItemList.Count; //else
    //Result := 0;
end;

procedure TsmxCustomMenuItem.SetMainMenu(Value: TsmxCustomMainMenu);
begin
  //if Assigned(FMainMenu) then
  //  FMainMenu.FMenuItemList.Remove(Self);
  //FMainMenu := Value;
  //if Assigned(Value) then
  //  Value.FMenuItemList.Add(Self);

  FMainMenu := Value;
end;

procedure TsmxCustomMenuItem.SetMenuItemParent(Value: TsmxCustomMenuItem);
begin
  if Assigned(FMenuItemParent) then
    FMenuItemParent.FMenuItemList.Remove(Self);
  FMenuItemParent := Value;
  if Assigned(Value) then
    Value.FMenuItemList.Add(Self);
end;}

{constructor TsmxCustomMenuItem.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FMenuItemName := Cfg.ItemName;
end;}

procedure TsmxCustomMenuItem.AddAlgorithm(Algorithm: TsmxAlgorithm);
begin
end;

procedure TsmxCustomMenuItem.DelAlgorithm(Algorithm: TsmxAlgorithm);
begin
end;

function TsmxCustomMenuItem.GetCfg: TsmxMenuItemCfg;
begin
  Result := TsmxMenuItemCfg(inherited Cfg);
end;

{function TsmxCustomMenuItem.GetMainMenu: TsmxCustomMainMenu;
var c: TsmxBaseCell;
begin
  Result := nil;
  c := Self;
  while Assigned(c.ParentCell) and not(c is TsmxCustomMainMenu) do
    c := c.ParentCell;
  if c is TsmxCustomMainMenu then
    Result := TsmxCustomMainMenu(c);
end;}

{procedure TsmxCustomMenuItem.SetMainMenu(Value: TsmxCustomMainMenu);
begin
  FMainMenu := Value;
end;}

{procedure TsmxCustomMenuItem.SetMenuItemParent(Value: TsmxCustomMenuItem);
begin
  FMenuItemParent := Value;
end;}

{ TsmxMenuItem }

constructor TsmxMenuItem.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FMenuItem := TMenuItem.Create(Self);
  FMenuItem.Caption := Cfg.ItemCaption;
  FMenuItem.ImageIndex := TImageIndex(Cfg.ItemImageIndex);
end;

destructor TsmxMenuItem.Destroy;
begin
  FMenuItem.Free;
  inherited Destroy;
end;

procedure TsmxMenuItem.AddAlgorithm(Algorithm: TsmxAlgorithm);
var c: TObject; mi: TMenuItem;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if Assigned(c) then
    if c is TBasicAction then
    begin
      mi := TMenuItem.Create(Self);
      mi.Action := TBasicAction(c);
      //mi.Tag := 1900;
      FMenuItem.Add(mi);
    end;
end;

{procedure TsmxMenuItem.AddMenuItem(AItem: TsmxCustomMenuItem);
var c: TObject;
begin
  inherited AddMenuItem(AItem);
  c := AItem.GetInternalObject;
  if Assigned(c) then
    if c is TMenuItem then
      FMenuItem.Add(TMenuItem(c));
end;}

procedure TsmxMenuItem.DelAlgorithm(Algorithm: TsmxAlgorithm);
var c: TObject; mi: TMenuItem; i: Integer;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if Assigned(c) then
    if c is TBasicAction then
      for i := FMenuItem.Count - 1 downto 0 do
      begin
        mi := FMenuItem[i];
        if mi.Action = c then
        begin
          FMenuItem.Remove(mi);
          mi.Action := nil;
          mi.Free;
        end;
      end;
end;

function TsmxMenuItem.GetInternalObject: TObject;
begin
  Result := FMenuItem;
end;

function TsmxMenuItem.GetCellEnable: Boolean;
begin
  Result := FMenuItem.Enabled;
end;

function TsmxMenuItem.GetCellVisible: Boolean;
begin
  Result := FMenuItem.Visible;
end;

procedure TsmxMenuItem.SetCellEnable(Value: Boolean);
begin
  FMenuItem.Enabled := Value;
end;

procedure TsmxMenuItem.SetCellVisible(Value: Boolean);
begin
  FMenuItem.Visible := Value;
end;

{procedure TsmxMenuItem.SetMainMenu(Value: TsmxCustomMainMenu);
var c: TObject;
begin
  if Assigned(FMainMenu) then
  begin
    c := FMainMenu.GetInternalObject;
    if Assigned(c) then
      if c is TMainMenu then
        TMainMenu(c).Items.Remove(FMenuItem);
  end;
  inherited SetMainMenu(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TMainMenu then
        TMainMenu(c).Items.Add(FMenuItem);
    SetMenuItemParent(nil);
  end;
end;}

{procedure TsmxMenuItem.SetMenuItemParent(Value: TsmxCustomMenuItem);
var c: TObject;
begin
  if Assigned(FMenuItemParent) then
  begin
    c := FMenuItemParent.GetInternalObject;
    if Assigned(c) then
      if c is TMenuItem then
        TMenuItem(c).Remove(FMenuItem);
  end;
  inherited SetMenuItemParent(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TMenuItem then
        TMenuItem(c).Add(FMenuItem);
    SetMainMenu(nil);
  end;
end;}

procedure TsmxMenuItem.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    //if Assigned(c) then
    //begin
      if c is TMenuItem then
        TMenuItem(c).Remove(FMenuItem) else
      if c is TMainMenu then
        TMainMenu(c).Items.Remove(FMenuItem);
    //end;
  end;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    //if Assigned(c) then
    //begin
      if c is TMenuItem then
        TMenuItem(c).Add(FMenuItem) else
      if c is TMainMenu then
        TMainMenu(c).Items.Add(FMenuItem);
    //end;
  end;
end;

{ TsmxMenuUnitMenuItem }

{constructor TsmxMenuUnitMenuItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FMenuUnit := nil;
  FMenuItem := nil;
end;}

{ TsmxMenuUnitMenuItems }

{function TsmxMenuUnitMenuItems.Add: TsmxMenuUnitMenuItem;
begin
  Result := TsmxMenuUnitMenuItem(inherited Add);
end;

function TsmxMenuUnitMenuItems.GetInternalObject(Index: Integer): TsmxMenuUnitMenuItem;
begin
  Result := TsmxMenuUnitMenuItem(inherited Items[Index]);
end;

function TsmxMenuUnitMenuItems.ItemToUnit(AMenuItem: TsmxCustomMenuItem): TsmxHVisibleUnit;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FMenuItem = AMenuItem then
    begin
      Result := Items[i].FMenuUnit;
      Break;
    end;
end;

function TsmxMenuUnitMenuItems.UnitToItem(AMenuUnit: TsmxHVisibleUnit): TsmxCustomMenuItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FMenuUnit = AMenuUnit then
    begin
      Result := Items[i].FMenuItem;
      Break;
    end;
end;}

{ TsmxCustomMainMenu }

{procedure TsmxCustomMainMenu.AddLibAlg(Alg: TsmxCustomLibAlgorithm);
begin
end;}

procedure TsmxCustomMainMenu.CreateChilds;
var i: Integer;

  procedure AddItems(AUnit: TsmxHVisibleUnit);
  var i: Integer; //mi: TsmxCustomMenuItem;
  begin
    with AUnit do
      if ID > 0 then
      begin
        //mi := TsmxCustomMenuItem(IDToItemClass(ID, Call).Create(Self, ID, Call));
        //FMenuItemList.Add(IDToItemClass(ID, Call).Create(Self, ID, Call));
        FMenuItemList.Add(NewCell(Self, ID, Call));
        {with FMenuUnitMenuItems.Add do
        begin
          MenuUnit := AUnit;
          MenuItem := mi;
        end;}
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    for i := 0 to AUnit.Count - 1 do
      AddItems(AUnit[i]);
  end;

begin
  FMenuItemList := TList.Create;
  //FMenuUnitMenuItems := TsmxMenuUnitMenuItems.Create(TsmxMenuUnitMenuItem);
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    AddItems(Cfg.MenuUnits.Root[i]);
end;

procedure TsmxCustomMainMenu.DestroyChilds;
var i: Integer;
begin
  //FMenuUnitMenuItems.Free;
  for i := FMenuItemList.Count - 1 downto 0 do
  begin
    TsmxCustomMenuItem(FMenuItemList[i]).Free;
    FMenuItemList.Delete(i);
  end;
  FMenuItemList.Free;
end;

{function TsmxCustomMainMenu.FindMenuItem(const AMenuItemName: String): TsmxCustomMenuItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to MenuItemCount - 1 do
    if AnsiCompareText(MenuItems[i].MenuItemName, AMenuItemName) = 0 then
    begin
      Result := MenuItems[i];
      Break;
    end;
end;}

function TsmxCustomMainMenu.FindMenuItemByCfgID(ACfgID: Integer): TsmxCustomMenuItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to MenuItemCount - 1 do
    if MenuItems[i].CfgID = ACfgID then
    begin
      Result := MenuItems[i];
      Break;
    end;
end;

function TsmxCustomMainMenu.GetCfg: TsmxMainMenuCfg;
begin
  Result := TsmxMainMenuCfg(inherited Cfg);
end;

{function TsmxCustomMainMenu.GetForm: TsmxCustomForm;
begin
  Result := nil;
  if Assigned(ParentCell) then
    if ParentCell is TsmxCustomForm then
      Result := TsmxCustomForm(ParentCell);
end;}

function TsmxCustomMainMenu.GetMenuItem(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(FMenuItemList[Index]);
end;

function TsmxCustomMainMenu.GetMenuItemCount: Integer;
begin
  Result := FMenuItemList.Count;
end;

function TsmxCustomMainMenu.MenuItemHasParent(ACfgID: Integer): Boolean;
var u: TsmxHVisibleUnit;
begin
  Result := True;
  u := Cfg.MenuUnits.Root.FindByID(ACfgID, True);
  if Assigned(u) then
    Result := u.HasParent;
end;

procedure TsmxCustomMainMenu.InitChilds;
var i: Integer;

  procedure InitItems(AUnit: TsmxHVisibleUnit);
  var i: Integer; mi: TsmxCustomMenuItem;
  begin
    //mi := FMenuUnitMenuItems.UnitToItem(AUnit);
    mi := FindMenuItemByCfgID(AUnit.ID);
    if Assigned(mi) then
      with AUnit do
      begin
        mi.CellAlign := UnitAlign;
        mi.CellEnable := UnitEnable;
        mi.CellHeight := UnitHeight;
        mi.CellLeft := UnitLeft;
        mi.CellTop := UnitTop;
        mi.CellVisible := UnitVisible;
        mi.CellWidth := UnitWidth;
      end;
    for i := 0 to AUnit.Count - 1 do
      InitItems(AUnit[i]);
  end;

begin
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    InitItems(Cfg.MenuUnits.Root[i]);
end;

procedure TsmxCustomMainMenu.InstallParent;
var i: Integer; u: TsmxHVisibleUnit;
begin
  for i := 0 to MenuItemCount - 1 do
  begin
    u := Cfg.MenuUnits.Root.FindByID(MenuItems[i].CfgID, True);
    if Assigned(u) then
      if u.HasParent then
        MenuItems[i].ParentCell := FindMenuItemByCfgID(u.Parent.ID) else
        MenuItems[i].ParentCell := Self;
  end;
end;

{procedure TsmxCustomMainMenu.SetForm(Value: TsmxCustomForm);
begin
  FForm := Value;
end;}

procedure TsmxCustomMainMenu.UnInstallParent;
var i: Integer; //u: TsmxHVisibleUnit;
begin
  {for i := 0 to MenuItemCount - 1 do
  begin
    u := MenuUnitMenuItems.ItemToUnit(MenuItems[i]);
    if u.HasParent then
      MenuItems[i].MenuItemParent := nil else
      MenuItems[i].MainMenu := nil;
  end;}
  for i := 0 to MenuItemCount - 1 do
    MenuItems[i].ParentCell := nil;
end;

{ TsmxMainMenu }

constructor TsmxMainMenu.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FMainMenu := TMainMenu.Create(Self);
  FMainMenu.Images := ImageList;
  //AddItems;
  InstallParent;
end;

destructor TsmxMainMenu.Destroy;
begin
  UnInstallParent;
  FMainMenu.Free;
  inherited Destroy;
end;

{procedure TsmxMainMenu.AddLibAlg(Alg: TsmxCustomLibAlgorithm);
begin
end;}

{procedure TsmxMainMenu.AddItems;
var i: Integer;

  procedure AddUnits(AItem: TMenuItem; AUnit: TsmxMenuUnit);
  var i: Integer; mi: TMenuItem;
  begin
    mi := TMenuItem.Create(AItem);
    with mi do
    begin
      Caption := AUnit.UnitCaption;
      Enabled := AUnit.UnitEnable;
      ImageIndex := AUnit.UnitImageIndex;
      //if AUnit.UnitName <> '' then
        Name := AUnit.UnitName;
      Visible := AUnit.UnitVisible;
    end;
    AItem.Add(mi);
    if AUnit.Count > 0 then
      for i := 0 to AUnit.Count - 1 do
        AddUnits(mi, AUnit[i]);
  end;

begin
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    AddUnits(MainMenu.Items, Cfg.MenuUnits.Root[i]);
end;}

{function TsmxMainMenu.FindMenuItem(MenuItemName: String): TMenuItem;
var i: Integer;

  //procedure Find(

begin
  //for
end;}

function TsmxMainMenu.GetInternalObject: TObject;
begin
  Result := FMainMenu;
end;

function TsmxMainMenu.GetCellEnable: Boolean;
begin
  Result := FMainMenu.Items.Enabled;
end;

{function TsmxMainMenu.GetItemParent: TObject;
begin
  //Result := FMainMenu.Parent;
  Result := FParent;
end;}

function TsmxMainMenu.GetCellVisible: Boolean;
begin
  Result := FMainMenu.Items.Visible;
end;

{procedure TsmxMainMenu.InstallParent;
var i: Integer; u: TsmxHVisibleUnit;
begin
  for i := 0 to MenuItemCount - 1 do
  begin
    u := Cfg.MenuUnits.Root.FindByID(MenuItems[i].CfgID, True);
    if Assigned(u) then
      if u.HasParent then
        MenuItems[i].ParentCell := FindMenuItemByCfgID(u.Parent.ID) else
        MenuItems[i].ParentCell := Self;
  end;
end;}

{procedure TsmxMainMenu.SetForm(Value: TsmxCustomForm);
var c: TObject;
begin
  if Assigned(FForm) then
  begin
    c := FForm.GetInternalObject;
    if Assigned(c) then
      if c is TCustomForm then
        TCustomForm(c).Menu := nil;
  end;
  inherited SetForm(Value);
  if Assigned(Value) then
  begin
    c := Value.GetInternalObject;
    if Assigned(c) then
      if c is TCustomForm then
        TCustomForm(c).Menu := FMainMenu;
  end;
end;}

procedure TsmxMainMenu.SetCellEnable(Value: Boolean);
begin
  FMainMenu.Items.Enabled := Value;
end;

{procedure TsmxMainMenu.SetItemParent(Value: TObject);
begin
  if Value is TCustomForm then
  begin
    FParent := TCustomForm(Value);
    TCustomForm(Value).Menu := FMainMenu;
  end else
  begin
    if Assigned(FParent) then
      FParent.Menu := nil;
    FParent := nil;
  end;}

  {if Value is TCustomForm then
    FMainMenu.WindowHandle := TCustomForm(Value).Handle else
    FMainMenu.WindowHandle := 0;}

  {if Value is TWinControl then
    FPageControl.Parent := TWinControl(Value) else
    FPageControl.Parent := nil;}
{end;}

procedure TsmxMainMenu.SetCellVisible(Value: Boolean);
begin
  FMainMenu.Items.Visible := Value;
end;

procedure TsmxMainMenu.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    //if Assigned(c) then
      if c is TCustomForm then
        TCustomForm(c).Menu := nil;
  end;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    //if Assigned(c) then
      if c is TCustomForm then
        TCustomForm(c).Menu := FMainMenu;
  end;
end;

{procedure TsmxMainMenu.UnInstallParent;
var i: Integer; //u: TsmxHVisibleUnit;
begin
  for i := 0 to MenuItemCount - 1 do
    MenuItems[i].ParentCell := nil;
end;}

{ TsmxCustomControlBar }

procedure TsmxCustomControlBar.AddAlgorithm(Algorithm: TsmxAlgorithm);
begin
end;

procedure TsmxCustomControlBar.DelAlgorithm(Algorithm: TsmxAlgorithm);
begin
end;

function TsmxCustomControlBar.GetCfg: TsmxControlBarCfg;
begin
  Result := TsmxControlBarCfg(inherited Cfg);
end;

{ TsmxControlBar }

constructor TsmxControlBar.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FControlBar := TControlBar.Create(Self);
  FControlBar.AutoSize := True;
  FControlBar.BevelInner := bvNone;
  FToolBarList := TList.Create;
  AddToolBar;
end;

destructor TsmxControlBar.Destroy;
begin
  DestroyToolBars;
  FToolBarList.Free;
  FControlBar.Free;
  inherited Destroy;
end;

procedure TsmxControlBar.AddAlgorithm(Algorithm: TsmxAlgorithm);
var c: TObject; p: TsmxBaseCell; i: Integer; tb: TToolBar; b: TToolButton;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
  begin
    //p := Algorithm.ParentCell;
    //if p is TsmxCustomAlgorithmList then
    //  i := 0 else
    //  i := 0;

    {if FToolBarList.Count = 0 then
    begin
      tb := TToolBar.Create(Self);
      tb.Parent := FControlBar;
      tb.AutoSize := True;
      tb.Images := ImageList;
      tb.Flat := Cfg.BarFlat;
      tb.ShowCaptions := Cfg.BarShowCaptions;
    end else}
      tb := TToolBar(FToolBarList[0]);
    if Assigned(tb) then
    begin
      b := TToolButton.Create(Self);
      tb.Width := tb.Width + b.Width;
      b.Parent := tb;
      b.ShowHint := Cfg.BarShowHint;
      b.Action := TBasicAction(c);
    end;
  end;
end;

procedure TsmxControlBar.AddToolBar;
var tb: TToolBar; f: TForm;
begin
  tb := TToolBar.Create(Self);
  f := TForm.Create(nil);
  try
    tb.Parent := f;
    tb.Images := ImageList;
    tb.Parent := nil;
  finally
    f.Free;
  end;
  tb.AutoSize := True;
  tb.EdgeBorders := tb.EdgeBorders - [ebTop];
  tb.Width := 0;
  tb.Flat := Cfg.BarFlat;
  tb.ShowCaptions := Cfg.BarShowCaptions;
  tb.ShowHint := Cfg.BarShowHint;
  tb.Parent := FControlBar;
  FToolBarList.Add(tb);
end;

procedure TsmxControlBar.CreateToolBars;
var m: TsmxCustomMainMenu; i: Integer;
begin
  if ParentCell is TsmxCustomForm then
  begin
    m := TsmxCustomForm(ParentCell).MainMenu;
    if Assigned(m) then
      for i := 0 to m.MenuItemCount - 1 do
        if not m.MenuItemHasParent(m[i].CfgID) then
          AddToolBar;
  end;
end;

procedure TsmxControlBar.DelAlgorithm(Algorithm: TsmxAlgorithm);
var c: TObject; tb: TToolBar; i: Integer; b: TToolButton;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
  begin
    tb := TToolBar(FToolBarList[0]);
    if Assigned(tb) then
      for i := tb.ButtonCount - 1 downto 0 do
      begin
        b := tb.Buttons[i];
        if b.Action = c then
        begin
          b.Action := nil;
          b.Parent := nil;
          b.Free;
        end;
      end;
  end;
end;

procedure TsmxControlBar.DestroyToolBars;
var i: Integer;
begin
  for i := FToolBarList.Count - 1 downto 0 do
  begin
    TToolBar(FToolBarList[i]).Free;
    FToolBarList.Delete(i);
  end;
end;

function TsmxControlBar.GetInternalObject: TObject;
begin
  Result := FControlBar;
end;

function TsmxControlBar.GetCellAlign: TAlign;
begin
  Result := FControlBar.Align;
end;

function TsmxControlBar.GetCellEnable: Boolean;
begin
  Result := FControlBar.Enabled;
end;

function TsmxControlBar.GetCellHeight: Integer;
begin
  Result := FControlBar.Height;
end;

function TsmxControlBar.GetCellLeft: Integer;
begin
  Result := FControlBar.Left;
end;

function TsmxControlBar.GetCellTop: Integer;
begin
  Result := FControlBar.Top;
end;

function TsmxControlBar.GetCellVisible: Boolean;
begin
  Result := FControlBar.Visible;
end;

function TsmxControlBar.GetCellWidth: Integer;
begin
  Result := FControlBar.Width;
end;

procedure TsmxControlBar.SetCellAlign(Value: TAlign);
begin
  FControlBar.Align := Value;
end;

procedure TsmxControlBar.SetCellEnable(Value: Boolean);
begin
  FControlBar.Enabled := Value;
end;

procedure TsmxControlBar.SetCellHeight(Value: Integer);
begin
  FControlBar.Height := Value;
end;

procedure TsmxControlBar.SetCellLeft(Value: Integer);
begin
  FControlBar.Left := Value;
end;

procedure TsmxControlBar.SetCellTop(Value: Integer);
begin
  FControlBar.Top := Value;
end;

procedure TsmxControlBar.SetCellVisible(Value: Boolean);
begin
  FControlBar.Visible := Value;
end;

procedure TsmxControlBar.SetCellWidth(Value: Integer);
begin
  FControlBar.Width := Value;
end;

procedure TsmxControlBar.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FControlBar.Parent := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    if c is TWinControl then
      FControlBar.Parent := TWinControl(c);
  end;
end;

{ TsmxCustomForm }

{constructor TsmxCustomForm.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FPageManager := TsmxCustomPageManager(IDToItemClass(Cfg.PageManager.ID).Create(Self, Cfg.PageManager.ID, Call));
  SetSizePosChilds;
end;}

destructor TsmxCustomForm.Destroy;
begin
  //FPageList.Free;
  //FForm.Free;
  //FPageManager.Free;
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

procedure TsmxCustomForm.AddAlgorithmsToBar;
var i: Integer;
begin
  if Cfg.AlgorithmList.IsCreateToolButton and Assigned(ControlBar) and Assigned(AlgorithmList) then
    for i := AlgorithmList.AlgorithmCount - 1 downto 0 do
      if AlgorithmList.AlgorithmButton(AlgorithmList[i].CfgID) then
        ControlBar.AddAlgorithm(AlgorithmList[i]);
end;

procedure TsmxCustomForm.AddAlgorithmsToMenu;
var i: Integer; mi: TsmxCustomMenuItem;
begin
  if Cfg.AlgorithmList.IsCreateMenuItem and Assigned(MainMenu) and Assigned(AlgorithmList) then
    for i := 0 to AlgorithmList.AlgorithmCount - 1 do
    begin
      mi := MainMenu.FindMenuItemByCfgID(AlgorithmList.AlgorithmMenuItemID(AlgorithmList[i].CfgID));
      if Assigned(mi) then
        mi.AddAlgorithm(AlgorithmList[i]);
    end;
end;

procedure TsmxCustomForm.CloseForm;
begin
end;

procedure TsmxCustomForm.CreateChilds;
begin
  with Cfg.PageManager do
    if ID > 0 then
    begin
      //FPageManager := TsmxCustomPageManager(IDToItemClass(ID, Call).Create(Self, ID, Call));
      FPageManager := TsmxCustomPageManager(NewCell(Self, ID, Call));
      //FPageManager.Form := Self;
    end;
  with Cfg.MainMenu do
    if ID > 0 then
    begin
      //FMainMenu := TsmxCustomMainMenu(IDToItemClass(ID, Call).Create(Self, ID, Call));
      FMainMenu := TsmxCustomMainMenu(NewCell(Self, ID, Call));
      //FPageManager.Form := Self;
    end;
  with Cfg.AlgorithmList do
    if ID > 0 then
      FAlgorithmList :=
        //TsmxCustomAlgorithmList(IDToItemClass(AlgorithmListID, Call).Create(Self, AlgorithmListID, Call));
        TsmxCustomAlgorithmList(NewCell(Self, ID, Call));
  with Cfg.ControlBar do
    if ID > 0 then
      FControlBar := TsmxCustomControlBar(NewCell(Self, ID, Call));
end;

procedure TsmxCustomForm.DelAlgorithmsToBar;
var i: Integer;
begin
  if Cfg.AlgorithmList.IsCreateToolButton and Assigned(ControlBar) and Assigned(AlgorithmList) then
    for i := AlgorithmList.AlgorithmCount - 1 downto 0 do
      if AlgorithmList.AlgorithmButton(AlgorithmList[i].CfgID) then
        ControlBar.DelAlgorithm(AlgorithmList[i]);
end;

procedure TsmxCustomForm.DelAlgorithmsToMenu;
var i: Integer; mi: TsmxCustomMenuItem;
begin
  if Cfg.AlgorithmList.IsCreateMenuItem and Assigned(MainMenu) and Assigned(AlgorithmList) then
    for i := AlgorithmList.AlgorithmCount - 1 downto 0 do
    begin
      mi := MainMenu.FindMenuItemByCfgID(AlgorithmList.AlgorithmMenuItemID(AlgorithmList[i].CfgID));
      if Assigned(mi) then
        mi.DelAlgorithm(AlgorithmList[i]);
    end;
end;

procedure TsmxCustomForm.DestroyChilds;
begin
  if Assigned(FPageManager) then
    FPageManager.Free;
  if Assigned(FMainMenu) then
    FMainMenu.Free;
  if Assigned(FAlgorithmList) then
    FAlgorithmList.Free;
  if Assigned(FControlBar) then
    FControlBar.Free;
end;

{function TsmxCustomForm.GetActiveRequest: TsmxCustomRequest;
begin
  Result := nil;
  if Assigned(PageManager) then
    if Assigned(PageManager.ActivePage) then
      if Assigned(PageManager.ActivePage.Grid) then
        Result := PageManager.ActivePage.Grid.Request;
end;}

function TsmxCustomForm.GetCfg: TsmxFormCfg;
begin
  Result := TsmxFormCfg(inherited Cfg);
end;

function TsmxCustomForm.GetFormModalResult:  TModalResult;
begin
  Result := mrNone;
end;

function TsmxCustomForm.GetParam(Key: String): String;
begin
  Result := ParamList.Values[AnsiUpperCase(Key)];
end;

{function TsmxCustomForm.GetIsCreateMenuItem: Boolean;
begin
  Result := Cfg.AlgorithmList.IsCreateMenuItem;
end;}

function TsmxCustomForm.GetFormManager: TsmxFormManager;
begin
  if not(Assigned(FFormManager)) then
    FFormManager := TsmxFormManager(Integer(Call(2)));
  Result := FFormManager;
end;

function TsmxCustomForm.GetParamList: TStrings;
begin
  if not(Assigned(FParamList)) then
    FParamList := TStringList.Create;
  Result := FParamList;
end;

{function TsmxCustomForm.GetOwnerForm: TsmxCustomForm;
begin
  if Owner is TsmxCustomForm then
    Result := TsmxCustomForm(Owner) else
    Result := nil;
end;}

procedure TsmxCustomForm.InitChilds;
begin
  if Assigned(FPageManager) then
    with Cfg.PageManager do
    begin
      FPageManager.CellAlign := Align;
      FPageManager.CellEnable := Enable;
      FPageManager.CellVisible := Visible;
      with PositionSize do
      begin
        FPageManager.CellHeight := Height;
        FPageManager.CellLeft := Left;
        FPageManager.CellTop := Top;
        FPageManager.CellWidth := Width;
      end;
    end;
  if Assigned(FMainMenu) then
    with Cfg.MainMenu do
    begin
      FMainMenu.CellAlign := Align;
      FMainMenu.CellEnable := Enable;
      FMainMenu.CellVisible := Visible;
      with PositionSize do
      begin
        FMainMenu.CellHeight := Height;
        FMainMenu.CellLeft := Left;
        FMainMenu.CellTop := Top;
        FMainMenu.CellWidth := Width;
      end;
    end;
  if Assigned(FControlBar) then
    with Cfg.ControlBar do
    begin
      FControlBar.CellAlign := Align;
      FControlBar.CellEnable := Enable;
      FControlBar.CellVisible := Visible;
      with PositionSize do
      begin
        FControlBar.CellHeight := Height;
        FControlBar.CellLeft := Left;
        FControlBar.CellTop := Top;
        FControlBar.CellWidth := Width;
      end;
    end;
end;

procedure TsmxCustomForm.InstallParent;
begin
  if Assigned(FPageManager) then
    //FPageManager.Form := Self;
    FPageManager.ParentCell := Self;
  if Assigned(FMainMenu) then
    //FMainMenu.Form := Self;
    FMainMenu.ParentCell := Self;
  if Assigned(FAlgorithmList) then
    //FAlgorithmList.Form := Self;
    FAlgorithmList.ParentCell := Self;
  if Assigned(FControlBar) then
    FControlBar.ParentCell := Self;
end;

procedure TsmxCustomForm.Prepare(Forcibly: Boolean = False);
var p: TsmxCustomPage;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    if Assigned(p.Grid) then
      p.Grid.Prepare(Forcibly);
    if Assigned(p.FilterPanel) then
      p.FilterPanel.Prepare(Forcibly);
  end;
end;

{procedure TsmxCustomForm.PrepareForm;
var p: TsmxCustomPage;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    if Assigned(p.Grid) then
      p.Grid.PrepareGrid;
    if Assigned(p.FilterPanel) then
      p.FilterPanel.PrepareFilterPanel;
  end;
end;}

{procedure TsmxCustomForm.RefreshForm;
var p: TsmxCustomPage;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    if Assigned(p.Grid) then
      p.Grid.RefreshGrid;
    if Assigned(p.FilterPanel) then
      p.FilterPanel.RefreshFilterPanel;
  end;
end;}

procedure TsmxCustomForm.SetFormModalResult(Value: TModalResult);
begin
end;

procedure TsmxCustomForm.SetParam(Key: String; Value: String);
begin
  ParamList.Values[AnsiUpperCase(Key)] := Value;
end;

{procedure TsmxCustomForm.SetItemMainMenu(Value: TObject);
begin
end;}

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

procedure TsmxCustomForm.UnInstallParent;
begin
  if Assigned(FPageManager) then
    //FPageManager.Form := nil;
    FPageManager.ParentCell := nil;
  if Assigned(FMainMenu) then
    //FMainMenu.Form := nil;
    FMainMenu.ParentCell := nil;
  if Assigned(FAlgorithmList) then
    //FAlgorithmList.Form := nil;
    FAlgorithmList.ParentCell := nil;
  if Assigned(FControlBar) then
    FControlBar.ParentCell := nil;
end;

{ TsmxStandardForm }

constructor TsmxStandardForm.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  FForm := TForm.Create(Self);
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;        
  if Cfg.FormImageIndex >= 0 then
    ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
  if Assigned(FormManager) then
    //FormManager[FForm.Handle] := Self;
    FormManager.InsertForm(Self);
  InstallParent;
  AddAlgorithmsToMenu;
  AddAlgorithmsToBar;
end;

destructor TsmxStandardForm.Destroy;
begin
  DelAlgorithmsToBar;
  DelAlgorithmsToMenu;
  UnInstallParent;
  if Assigned(FormManager) then
    //FormManager[FForm.Handle] := nil;
    FormManager.RemoveForm(Self);
  FForm.Free;
  inherited Destroy;
end;

procedure TsmxStandardForm.CloseForm;
begin
  Form.OnClose := nil;
  Form.Close;

  //Form.OnClose := nil;
  //Free;
end;

procedure TsmxStandardForm.CloseProc(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  Free;

  //CloseForm;
end;

function TsmxStandardForm.GetFormModalResult:  TModalResult;
begin
  Result := FForm.ModalResult;
end;

function TsmxStandardForm.GetInternalObject: TObject;
begin
  Result := FForm;
end;

function TsmxStandardForm.GetCellAlign: TAlign;
begin
  Result := FForm.Align;
end;

function TsmxStandardForm.GetCellEnable: Boolean;
begin
  Result := FForm.Enabled;
end;

function TsmxStandardForm.GetCellHeight: Integer;
begin
  Result := FForm.Height;
end;

function TsmxStandardForm.GetCellLeft: Integer;
begin
  Result := FForm.Left;
end;

{function TsmxStandardForm.GetItemMainMenu: TObject;
begin
  Result := nil;
end;}

{function TsmxStandardForm.GetItemParent: TObject;
begin
  Result := FForm.Parent;
end;}

function TsmxStandardForm.GetCellTop: Integer;
begin
  Result := FForm.Top;
end;

function TsmxStandardForm.GetCellVisible: Boolean;
begin
  Result := FForm.Visible;
end;

function TsmxStandardForm.GetCellWidth: Integer;
begin
  Result := FForm.Width;
end;

{procedure TsmxStandardForm.InstallParent;
begin
  if Assigned(FPageManager) then
    //FPageManager.Form := Self;
    FPageManager.ParentCell := Self;
  if Assigned(FMainMenu) then
    //FMainMenu.Form := Self;
    FMainMenu.ParentCell := Self;
  if Assigned(FAlgorithmList) then
    //FAlgorithmList.Form := Self;
    FAlgorithmList.ParentCell := Self;
  if Assigned(FControlBar) then
    FControlBar.ParentCell := Self;
end;}

{procedure TsmxStandardForm.PrepareForm;
var p: TsmxCustomPage; r: TsmxCustomRequest; fp: TsmxCustomFilterPanel;
  i: Integer; fk: IsmxField; v: Variant;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := nil;
    if Assigned(p.Grid) then
      p.Grid.PrepareGrid;
      //r := p.Grid.Request;
    //if Assigned(r) then
      //if not(r.CellDataSet.Prepared) and (p.Grid.RequestMode = omAutomatic) then
        //r.Perform;

    fp := p.FilterPanel;
    if Assigned(fp) then
    begin
      fp.PrepareFilterPanel;
      r := fp.Request;
      if Assigned(r) then
        //if not(r.CellDataSet.Prepared) and (fp.RequestMode = omAutomatic) then
        begin
          r.Perform;
          for i := 0 to fp.FilterCount - 1 do
          begin
            fk := r.CellDataSet.FindField(fp[i].FilterName);
            if Assigned(fk) then
              fp[i].FilterValue := fk.Value;
          //end;

          //for i := 0 to r.CellDataSet.FieldCount - 1 do
          with r.CellDataSet.Fields[i] do
            begin
              v := Value;
              if VarIsNull(v) then
                FormParams[FieldName] := '' else
                FormParams[FieldName] := Value;
            //end;

          //ap := TsmxParams.Create(TsmxParam);
          try
            for i := 0 to fp.FilterCount - 1 do
            begin
              fk := r.CellDataSet.FindField(fp[i].FilterName);
              fv := r.CellDataSet.FindField(fp[i].FilterName + 'Text');
              if Assigned(fk) and Assigned(fv) then
              begin
                ap.Clear;
                with ap.Add do
                begin
                  ParamName := 'Key';
                  ParamValue := fk.Value;
                end;
                with ap.Add do
                begin
                  ParamName := 'Value';
                  ParamValue := fv.Value;
                end;
                fp[i].FilterValue := ParamsToVar(ap);
              end else
              if Assigned(fk) then
              begin
                fp[i].FilterValue := fk.Value;
              end;
            end;
          finally
            ap.Free;
          //end;
        //end;
    end;
  end;
end;}

{procedure TsmxStandardForm.RefreshForm;
var p: TsmxCustomPage; r: TsmxCustomRequest; fp: TsmxCustomFilterPanel;
  i: Integer; fk: IsmxField; v: Variant;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := nil;
    if Assigned(p.Grid) then
      p.Grid.RefreshGrid;
      //r := p.Grid.Request;
    //if Assigned(r) then
      //r.Perform;

    fp := p.FilterPanel;
    if Assigned(fp) then
    begin
      fp.RefreshFilterPanel;
      r := fp.Request;
      if Assigned(r) then
      begin
        //r.Perform;
        for i := 0 to fp.FilterCount - 1 do
        begin
          fk := r.CellDataSet.FindField(fp[i].FilterName);
          if Assigned(fk) then
            fp[i].FilterValue := fk.Value else
            fp[i].FilterValue := Null;
        //end;

        for i := 0 to r.CellDataSet.FieldCount - 1 do
          with r.CellDataSet.Fields[i] do
          begin
            v := Value;
            if VarIsNull(v) then
              FormParams[FieldName] := '' else
              FormParams[FieldName] := Value;
          end;
      end;
    end;
  end;
end;}

procedure TsmxStandardForm.SetFormModalResult(Value: TModalResult);
begin
  FForm.ModalResult := Value;
end;

procedure TsmxStandardForm.SetCellAlign(Value: TAlign);
begin
  FForm.Align := Value;
end;

procedure TsmxStandardForm.SetCellEnable(Value: Boolean);
begin
  FForm.Enabled := Value;
end;

procedure TsmxStandardForm.SetCellHeight(Value: Integer);
begin
  FForm.Height := Value;
end;

procedure TsmxStandardForm.SetCellLeft(Value: Integer);
begin
  FForm.Left := Value;
end;

{procedure TsmxStandardForm.SetItemMainMenu(Value: TObject);
begin
  if Assigned(FForm.Menu) then
    FForm.Menu := nil;
  if Value is TMainMenu then
    FForm.Menu := TMainMenu(Value);
end;}

{procedure TsmxStandardForm.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FForm.Parent := TWinControl(Value) else
    FForm.Parent := nil;
end;}

procedure TsmxStandardForm.SetCellTop(Value: Integer);
begin
  FForm.Top := Value;
end;

procedure TsmxStandardForm.SetCellVisible(Value: Boolean);
begin
  FForm.Visible := Value;
end;

procedure TsmxStandardForm.SetCellWidth(Value: Integer);
begin
  FForm.Width := Value;
end;

procedure TsmxStandardForm.ShowForm;
//var p: TsmxCustomPage; r: TsmxCustomRequest;
begin
  Form.OnClose := CloseProc;
  Form.Show;
  Prepare;

  {p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := p.Request;
    if Assigned(r) then
      if not(r.CellDataSet.Prepared) and (p.RequestMode = omAutomatic) then
        r.Perform;
  end;}
end;

function TsmxStandardForm.ShowModalForm: TModalResult;
//var p: TsmxCustomPage; r: TsmxCustomRequest;
begin
  {p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := p.Request;
    if Assigned(r) then
      if not(r.CellDataSet.Prepared) and (p.RequestMode = omAutomatic) then
        r.Perform;
  end;}

  Prepare;
  //Form.OnClose := CloseProc;
  Result := Form.ShowModal;
end;

{procedure TsmxStandardForm.UnInstallParent;
begin
  if Assigned(FPageManager) then
    //FPageManager.Form := nil;
    FPageManager.ParentCell := nil;
  if Assigned(FMainMenu) then
    //FMainMenu.Form := nil;
    FMainMenu.ParentCell := nil;
  if Assigned(FAlgorithmList) then
    //FAlgorithmList.Form := nil;
    FAlgorithmList.ParentCell := nil;
  if Assigned(FControlBar) then
    FControlBar.ParentCell := nil;
end;}

{ TsmxMainForm }

{constructor TsmxMainForm.Create(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack);
begin
  inherited Create(AOwner, ACfgID, ACall);
  //FForm := TForm.Create(Self);
  //FForm.Left := Cfg.FormPositionSize.Left;
  //FForm.Top := Cfg.FormPositionSize.Top;
  //FForm.Height := Cfg.FormPositionSize.Height;
  //FForm.Width := Cfg.FormPositionSize.Width;
  //FForm.Caption := Cfg.FormCaption;
  //if Cfg.FormImageIndex >= 0 then
    //ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
  //if Assigned(FormManager) then
    ////FormManager[FForm.Handle] := Self;
    //FormManager.InsertForm(Self);
  //InstallParent;
  //AddAlgorithmsToMenu;
  //AddAlgorithmsToBar;
end;}

{destructor TsmxMainForm.Destroy;
begin
  //DelAlgorithmsToBar;
  //DelAlgorithmsToMenu;
  //UnInstallParent;
  //if Assigned(FormManager) then
    ////FormManager[FForm.Handle] := nil;
    //FormManager.RemoveForm(Self);
  //FForm.Free;
  //FForm := nil;
  inherited Destroy;
end;}

procedure TsmxMainForm.CloseForm;
begin
  if not Assigned(Form) then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  //UnInitialize;
  Form.OnClose := nil;
  Form.OnCloseQuery := nil;
  Form := nil;
  //Form.Close;
  Free;
end;

procedure TsmxMainForm.CloseProc(Sender: TObject; var Action: TCloseAction);
begin
  //Action := caNone;
  //CloseForm;
  CloseForm;
  //Action := caHide;
end;

procedure TsmxMainForm.CloseQueryProc(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Ask(CloseProgMessage);
end;

{function TsmxMainForm.GetFormModalResult:  TModalResult;
begin
  Result := FForm.ModalResult;
end;}

function TsmxMainForm.GetInternalObject: TObject;
begin
  Result := FForm;
end;

{function TsmxMainForm.GetCellAlign: TAlign;
begin
  Result := FForm.Align;
end;

function TsmxMainForm.GetCellEnable: Boolean;
begin
  Result := FForm.Enabled;
end;

function TsmxMainForm.GetCellHeight: Integer;
begin
  Result := FForm.Height;
end;

function TsmxMainForm.GetCellLeft: Integer;
begin
  Result := FForm.Left;
end;}

{function TsmxStandardForm.GetItemMainMenu: TObject;
begin
  Result := nil;
end;}

{function TsmxStandardForm.GetItemParent: TObject;
begin
  Result := FForm.Parent;
end;}

{function TsmxMainForm.GetCellTop: Integer;
begin
  Result := FForm.Top;
end;

function TsmxMainForm.GetCellVisible: Boolean;
begin
  Result := FForm.Visible;
end;

function TsmxMainForm.GetCellWidth: Integer;
begin
  Result := FForm.Width;
end;}

procedure TsmxMainForm.Initialize;
begin
  if not Assigned(FForm) then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  if Cfg.FormImageIndex >= 0 then
    ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
  //if Assigned(FormManager) then
  ////FormManager[FForm.Handle] := Self;
  //FormManager.InsertForm(Self);
  InstallParent;
  AddAlgorithmsToMenu;
  AddAlgorithmsToBar;
end;

{procedure TsmxMainForm.InstallParent;
begin
  if Assigned(FPageManager) then
    //FPageManager.Form := Self;
    FPageManager.ParentCell := Self;
  if Assigned(FMainMenu) then
    //FMainMenu.Form := Self;
    FMainMenu.ParentCell := Self;
  if Assigned(FAlgorithmList) then
    //FAlgorithmList.Form := Self;
    FAlgorithmList.ParentCell := Self;
  if Assigned(FControlBar) then
    FControlBar.ParentCell := Self;
end;}

{procedure TsmxMainForm.PrepareForm;
var p: TsmxCustomPage; r: TsmxCustomRequest; fp: TsmxCustomFilterPanel;
  i: Integer; fk: IsmxField; v: Variant;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := nil;
    if Assigned(p.Grid) then
      p.Grid.PrepareGrid;
      //r := p.Grid.Request;
    //if Assigned(r) then
      //if not(r.CellDataSet.Prepared) and (p.Grid.RequestMode = omAutomatic) then
        //r.Perform;

    fp := p.FilterPanel;
    if Assigned(fp) then
    begin
      fp.PrepareFilterPanel;
      r := fp.Request;
      if Assigned(r) then

          for i := 0 to r.CellDataSet.FieldCount - 1 do
          with r.CellDataSet.Fields[i] do
            begin
              v := Value;
              if VarIsNull(v) then
                FormParams[FieldName] := '' else
                FormParams[FieldName] := Value;
            end;

        //end;
    end;
  end;
end;}

{procedure TsmxMainForm.RefreshForm;
var p: TsmxCustomPage; r: TsmxCustomRequest; fp: TsmxCustomFilterPanel;
  i: Integer; fk: IsmxField; v: Variant;
begin
  p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := nil;
    if Assigned(p.Grid) then
      p.Grid.RefreshGrid;
      //r := p.Grid.Request;
    //if Assigned(r) then
      //r.Perform;

    fp := p.FilterPanel;
    if Assigned(fp) then
    begin
      fp.RefreshFilterPanel;
      r := fp.Request;
      if Assigned(r) then
      begin


        for i := 0 to r.CellDataSet.FieldCount - 1 do
          with r.CellDataSet.Fields[i] do
          begin
            v := Value;
            if VarIsNull(v) then
              FormParams[FieldName] := '' else
              FormParams[FieldName] := Value;
          end;
      end;
    end;
  end;
end;}

{procedure TsmxStandardForm.SetFormModalResult(Value: TModalResult);
begin
  FForm.ModalResult := Value;
end;}

{procedure TsmxMainForm.SetCellAlign(Value: TAlign);
begin
  FForm.Align := Value;
end;

procedure TsmxMainForm.SetCellEnable(Value: Boolean);
begin
  FForm.Enabled := Value;
end;

procedure TsmxMainForm.SetCellHeight(Value: Integer);
begin
  FForm.Height := Value;
end;

procedure TsmxMainForm.SetCellLeft(Value: Integer);
begin
  FForm.Left := Value;
end;}

{procedure TsmxStandardForm.SetItemMainMenu(Value: TObject);
begin
  if Assigned(FForm.Menu) then
    FForm.Menu := nil;
  if Value is TMainMenu then
    FForm.Menu := TMainMenu(Value);
end;}

{procedure TsmxStandardForm.SetItemParent(Value: TObject);
begin
  if Value is TWinControl then
    FForm.Parent := TWinControl(Value) else
    FForm.Parent := nil;
end;}

{procedure TsmxMainForm.SetCellTop(Value: Integer);
begin
  FForm.Top := Value;
end;

procedure TsmxMainForm.SetCellVisible(Value: Boolean);
begin
  FForm.Visible := Value;
end;

procedure TsmxMainForm.SetCellWidth(Value: Integer);
begin
  FForm.Width := Value;
end;}

procedure TsmxMainForm.SetForm(AForm: TForm);
begin
  if Assigned(FForm) then
  begin
    UnInitialize;
    FForm := nil;
  end;
  if Assigned(AForm) then
  begin
    FForm := AForm;
    Initialize;
  end;
end;

procedure TsmxMainForm.ShowForm;
//var p: TsmxCustomPage; r: TsmxCustomRequest;
begin
  //Initialize;
  //Form.OnClose := CloseProc;
  if not Assigned(Form) then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  Form.OnClose := CloseProc;
  Form.OnCloseQuery := CloseQueryProc;
  Form.Show;
  Prepare;

  {p := nil;
  if Assigned(PageManager) then
    p := PageManager.ActivePage;
  if Assigned(p) then
  begin
    r := p.Request;
    if Assigned(r) then
      if not(r.CellDataSet.Prepared) and (p.RequestMode = omAutomatic) then
        r.Perform;
  end;}
end;

{function TsmxMainForm.ShowModalForm: TModalResult;
//var p: TsmxCustomPage; r: TsmxCustomRequest;
begin
  PrepareForm;
  //Form.OnClose := CloseProc;
  Result := Form.ShowModal;
end;}

procedure TsmxMainForm.UnInitialize;
begin
  if not Assigned(FForm) then
    raise EsmxCellError.CreateRes(@SCellBuildError);
  DelAlgorithmsToBar;
  DelAlgorithmsToMenu;
  UnInstallParent;
  //if Assigned(FormManager) then
    ////FormManager[FForm.Handle] := nil;
    //FormManager.RemoveForm(Self);
  //FForm.Free;
end;

{procedure TsmxMainForm.UnInstallParent;
begin
  if Assigned(FPageManager) then
    //FPageManager.Form := nil;
    FPageManager.ParentCell := nil;
  if Assigned(FMainMenu) then
    //FMainMenu.Form := nil;
    FMainMenu.ParentCell := nil;
  if Assigned(FAlgorithmList) then
    //FAlgorithmList.Form := nil;
    FAlgorithmList.ParentCell := nil;
  if Assigned(FControlBar) then
    FControlBar.ParentCell := nil;
end;}

{ TsmxMFormsItem }

{constructor TsmxMFormsItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FFormHandle := 0;
  FFormPtr := nil;
end;}

{ TsmxMFormsItems }

{function TsmxMFormsItems.Add: TsmxMFormsItem;
begin
  Result := TsmxMFormsItem(inherited Add);
end;

function TsmxMFormsItems.FindByForm(AForm: TsmxCustomForm): TsmxMFormsItem;
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
var i: Integer; f: TsmxCustomForm;
begin
  for i := FFormList.Count - 1 downto 0 do
  begin
    f := FFormList[i].FormPtr;
    if Assigned(f) then
      f.Free;
  end;
end;

function TsmxFormManager.GetForm(Handle: HWND): TsmxCustomForm;
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

{procedure TsmxFormManager.InsertForm(AForm: TsmxCustomForm);
var f: TsmxMFormsItem; h: HWND; c: TObject;
begin
  f := FFormList.FindByForm(AForm);
  if not(Assigned(f)) then
  begin
    h := 0;
    c := AForm.GetInternalObject;
    if Assigned(c) then
      if c is TCustomForm then
        h := TCustomForm(c).Handle;
    with FFormList.Add do
    begin
      FormHandle := h;
      FormPtr := AForm;
    end;
  end;
end;

procedure TsmxFormManager.RemoveForm(AForm: TsmxCustomForm);
var f: TsmxMFormsItem;
begin
  f := FFormList.FindByForm(AForm);
  if Assigned(f) then
    FFormList.Remove(f);
end;}

initialization
  RegisterClasses([TsmxRequest, TsmxDBColumn, TsmxDBGrid, TsmxFilterPanel,
    TsmxTabSheet, TsmxPageControl, TsmxMenuItem, TsmxMainMenu, TsmxLibAlgorithm,
    TsmxActionList, TsmxControlBar, TsmxStandardForm, TsmxMainForm]);

end.
