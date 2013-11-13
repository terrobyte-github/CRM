{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Cells classes             }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Аleksandr          }
{                                      }
{**************************************}

unit smxCells;

interface

uses
  Classes, Controls, ComCtrls, DB, DBGrids, Forms, ExtCtrls, StdCtrls, Menus,
  ActnList, Windows, ImgList, Graphics, smxBaseClasses, smxClasses, smxCfgs,
  smxStdCtrls, smxDBIntf, smxTypes, smxClassTypes, smxManagerIntf, smxBaseTypes;

type
  { TsmxAction }

  TsmxAction = class(TsmxCustomAlgorithm)
  private
    FAction: TAction;
    function GetAction: TAction;
    procedure ActionExecute(Sender: TObject);
    //function GetParamValue(const ParamName: String): Variant;
  protected
    procedure DoRefreshParams; override;
    function GetAlgorithmCaption: String; override;
    function GetAlgorithmEnabled: Boolean; override;
    function GetAlgorithmHint: String; override;
    function GetAlgorithmHotKey: Integer; override;
    function GetAlgorithmImageIndex: Integer; override;
    function GetAlgorithmVisible: Boolean; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    //function GetProcPointer: Pointer; virtual;
    //procedure InitializeEvent; virtual;
    //procedure InternalInitialize; override;
    procedure InternalRefreshParams; override;
    procedure ResetCellProps; override;
    procedure SetAlgorithmCaption(const Value: String); override;
    procedure SetAlgorithmEnabled(Value: Boolean); override;
    procedure SetAlgorithmHint(const Value: String); override;
    procedure SetAlgorithmHotKey(Value: Integer); override;
    procedure SetAlgorithmImageIndex(Value: Integer); override;
    procedure SetAlgorithmVisible(Value: Boolean); override;
    //procedure ProcExec(Sender: TObject); virtual;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    //procedure SetLibraryManager(Value: TsmxCustomLibraryManager); override;
    procedure SetSlaveIndex(Value: Integer); override;

    property Action: TAction read GetAction;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //procedure Execute(Same: Boolean = False); override;
    //procedure RefreshParams; override;
    // function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam; override;
  published
    property AlgorithmCaption;
    property AlgorithmEnabled;
    property AlgorithmHint;
    property AlgorithmHotKey;
    property AlgorithmImageIndex;
    property AlgorithmParams;
    property AlgorithmVisible;

    property OnRefreshParams;
  end;

  { TsmxActionList }

  TsmxActionList = class(TsmxCustomAlgorithmList)
  private
    FActionList: TActionList;
    function GetActionList: TActionList;
  protected
    //function GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalInitialize; override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;
    //procedure SetParentCell(Value: TsmxBaseCell); override;

    property ActionList: TActionList read GetActionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAlgorithmParamValue(CfgID: Integer; const ParamName: String;
      var ParamValue: Variant): Boolean; override;
    //procedure Initialize(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
    //  ASelectRequest: TsmxCustomRequest = nil); override;

    //property IsAltSlaveClass default True;
    //property IsOwnerIsParent default True;
  published
    property ImageListName;
    property SlaveListNew;
  end;

  { TsmxRequest }

  TsmxRequest = class(TsmxCustomRequest)
  //private
    //function GetParamValue(const ParamName: String): Variant;
  protected
    procedure DoDelete; override;
    procedure DoExecute; override;
    procedure DoInsert; override;
    procedure DoPrepare; override;
    procedure DoRefreshParams; override;
    procedure DoUpdate; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    //function GetDataSet: IsmxDataSet; virtual;
    //procedure InitializeDataSet; virtual;
    //procedure InternalInitialize; override;
    procedure InternalRefreshParams; override;
    procedure ResetCellProps; override;
    procedure SetCellProps; override;
  published
    property DatabaseName;
    property DataSet;
    property DeleteDataSet;
    property InsertDataSet;
    property OperationMode;
    property UpdateDataSet;

    property OnDelete;
    property OnExecute;
    property OnInsert;
    property OnPrepare;
    property OnRefreshParams;
    property OnUpdate;
  end;

  { TsmxRequestList }

  TsmxRequestList = class(TsmxCustomRequestList)
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetRequestParamValue(CfgID: Integer; const ParamName: String;
      var ParamValue: Variant): Boolean; override;

    //property IsAltSlaveClass default True;
    //property IsOwnerIsParent default True;
  published
    property SlaveListNew;
  end;

  { TsmxColumn }

  TsmxColumn = class(TsmxCustomColumn)
  private
    FColumn: TColumn;
    function GetColumn: TColumn;
    //function GetFieldValue(const FieldName: String; var Value: Variant): Boolean;
    //function SetFieldValue(const FieldName: String; const Value: Variant): Boolean;
    function IsOwnerDrawHeader: Boolean;
  protected
    procedure DoSnapHeader; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetColumnAlignment: TAlignment; override;
    function GetColumnCaption: String; override;
    function GetColumnColor: TColor; override;
    function GetColumnFont: TFont; override;
    function GetColumnValue: Variant; override;
    //function GetFieldName: String; override;
    function GetHeaderAlignment: TAlignment; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetHeaderCaption: String; override;
    //function GetIsEditing: Boolean; override;
    function GetInternalObject: TObject; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetColumnAlignment(Value: TAlignment); override;
    procedure SetColumnCaption(const Value: String); override;
    procedure SetColumnColor(Value: TColor); override;
    procedure SetColumnFont(Value: TFont); override;
    procedure SetColumnOptions(Value: TsmxColumnOptions); override;
    procedure SetColumnValue(const Value: Variant); override;
    //procedure SetFieldName(const Value: String); override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderCaption(const Value: String); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetSlaveIndex(Value: Integer); override;
    //procedure SetSlaveName(const Value: String); override;

    property Column: TColumn read GetColumn;
  public
    destructor Destroy; override;
  published
    property ColumnAlignment;
    //property ColumnCaption;
    property ColumnColor;
    property ColumnFont;
    property ColumnOptions;
    property CellVisible;
    property CellWidth;
    property HeaderAlignment;
    property HeaderCaption;
    property HeaderColor;
    property HeaderFont;

    property OnSnap;
    property OnSnapHeader;
  end;

  { TsmxDBGrid }

  TsmxDBGrid = class(TsmxCustomGrid)
  private
    //FDataSource: TDataSource;
    FDBGrid: TsmxWheelDBGrid;
    //function GetDataSource: TDataSource;
    function GetDBGrid: TsmxWheelDBGrid;
    //function GetDataSource: TDataSource;
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDataChange(Sender: TObject; Field: TField);
    //procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DrawHeaderDefault;
  protected
    //procedure DoApply; override;
    procedure DoChangeRow; override;
    //procedure DoPrepare; override;
    //procedure DoRefresh; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellAnchors: TAnchors; override;
    //function GetCellCursor: TCursor; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    function GetCellHint: String; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetFocusedColIndex: Integer; override;
    function GetFocusedRowIndex: Integer; override;
    function GetGridCaption(ColIndex, RowIndex: Integer): String; override;
    function GetGridValue(ColIndex, RowIndex: Integer): Variant; override;
    function GetRowCount: Integer; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalApply; override;
    //procedure InternalInitialize; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure ResetCellProps; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellAnchors(Value: TAnchors); override;
    //procedure SetCellCursor(Value: TCursor); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Integer); override;
    procedure SetGridCaption(ColIndex, RowIndex: Integer; const Value: String); override;
    procedure SetGridOptions(Value: TsmxGridOptions); override;
    procedure SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant); override;
    procedure SetRequest(Value: TsmxCustomRequest); override;

    //property DataSource: TDataSource read GetDataSource;
    property DBGrid: TsmxWheelDBGrid read GetDBGrid;
  public
    destructor Destroy; override;
  published
    property CellAlign;
    property CellAnchors;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellHint;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property GridOptions;
    property PopupMenu;
    property Request;
    property SlaveListNew;

    property OnChangeRow;
    property OnDoubleSnap;
  end;

  { TsmxFilter }

  {TsmxFilter = class(TsmxCustomFilter)
  private
    function GetCfg: TsmxFilterCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxFilterCfg read GetCfg;
  end;}

  { TsmxPanelFilter }

  TsmxPanelFilter = class(TsmxCustomFilter)
  private
    FHeader: TLabel;
    FPanel: TPanel;
    function GetHeader: TLabel;
    function GetPanel: TPanel;
  protected
    procedure DoChangeFilter; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellAnchors: TAnchors; override;
    //function GetCellCursor: TCursor; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    //function GetFilter: TObject; virtual;
    function GetHeaderAlignment: TAlignment; override;
    function GetHeaderCaption: String; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetInternalObject: TObject; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellAnchors(Value: TAnchors); override;
    //procedure SetCellCursor(Value: TCursor); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderCaption(const Value: String); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;
    //procedure UnBindFilterObjects; virtual;

    property Header: TLabel read GetHeader;
    property Panel: TPanel read GetPanel;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  published
    property CellAlign;
    property CellAnchors;
    property CellEnabled;
    property CellHeight;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property HeaderAlignment;
    property HeaderCaption;
    property HeaderColor;
    property HeaderFont;
    property PopupMenu;
  end;

  { TsmxFilterDesk }

  {TsmxFilterDesk = class(TsmxCustomFilterDesk)
  private
    function GetCfg: TsmxFilterDeskCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxFilterDeskCfg read GetCfg;
  end;}

  { TsmxPanelFilterDesk }

  TsmxPanelFilterDesk = class(TsmxCustomFilterDesk)
  private
    FPanel: TPanel;
    function GetPanel: TPanel;
    procedure RefreshValueParams(DataSet: IsmxDataSet);
  protected
    //procedure DoApply; override;
    //procedure DoPrepare; override;
    //procedure DoRefresh; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellAnchors: TAnchors; override;
    //function GetCellCursor: TCursor; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    //function GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass; override;
    function GetCellCaption: String; override;
    function GetCellHint: String; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalApply; override;
    //procedure InternalInitialize; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure ResetCellProps; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellAnchors(Value: TAnchors); override;
    //procedure SetCellCursor(Value: TCursor); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;

    property Panel: TPanel read GetPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property IsAltSlaveClass default True;
  published
    property CellAlign;
    property CellAnchors;
    property CellCaption;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellHint;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property PopupMenu;
    property SlaveListNew;

    property OnDoubleSnap;
    property OnSnap;
  end;

  { TsmxSection }

  {TsmxSection = class(TsmxCustomSection)
  private
    function GetCfg: TsmxSectionCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxSectionCfg read GetCfg;
  end;}

  { TsmxPanelSection }

  TsmxPanelSection = class(TsmxCustomSection)
  private
    FPanel: TPanel;
    function GetPanel: TPanel;
  protected
    //function GetCellAlign: TAlign; override;
    //function GetCellAnchors: TAnchors; override;
    //function GetCellCursor: TCursor; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    //procedure InternalInitialize; override;
    //procedure ResetCellProps; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellAnchors(Value: TAnchors); override;
    //procedure SetCellCursor(Value: TCursor); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    //procedure SetFilterDesk(Value: TsmxCustomFilterDesk); override;
    //procedure SetGrid(Value: TsmxCustomGrid); override;

    property Panel: TPanel read GetPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property IsAltSlaveClass default True;
  published
    property CellAlign;
    property CellAnchors;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property PopupMenu;
    property SlaveListNew;

    property OnDoubleSnap;
    property OnSnap;
  end;

  { TsmxPage }

  {TsmxPage = class(TsmxCustomPage)
  private
    function GetCfg: TsmxPageCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxPageCfg read GetCfg;
  end;}

  { TsmxTabSheet }

  TsmxTabSheet = class(TsmxCustomPage)
  private
    FTabSheet: TTabSheet;
    //FWinControl: TWinControl;
    function GetTabSheet: TTabSheet;
  protected
    //function GetCellActive: Boolean; override;
    function GetCellCaption: String; override;
    function GetCellHint: String; override;
    function GetCellImageIndex: Integer; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalInitialize; override;
    //procedure SetCellActive(Value: Boolean); override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellImageIndex(Value: Integer); override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure SetSlaveIndex(Value: Integer); override;

    property TabSheet: TTabSheet read GetTabSheet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property IsAltSlaveClass default True;
  published
    property CellCaption;
    property CellHint;
    property CellImageIndex;
    property CellVisible;
    property PopupMenu;
    property SlaveListNew;
  end;

  { TsmxPageManager }

  {TsmxPageManager = class(TsmxCustomPageManager)
  private
    function GetCfg: TsmxPageManagerCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxPageManagerCfg read GetCfg;
  end;}

  { TsmxPageControl }

  TsmxPageControl = class(TsmxCustomPageManager)
  private
    FPageControl: TPageControl;
    //FWinControl: TWinControl;
    function GetPageControl: TPageControl;
    procedure PageControlChange(Sender: TObject);
  protected
    procedure DoChangePage; override;
    //function GetActivePage: TsmxCustomPage; override;
    function GetActivePageIndex: Integer; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetIsMultiLine: Boolean; override;
    function GetPageManagerStyle: TsmxPageManagerStyle; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InstallParent; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    //procedure SetActivePage(Value: TsmxCustomPage); override;
    procedure SetActivePageIndex(Value: Integer); override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetIsMultiLine(Value: Boolean); override;
    procedure SetPageManagerStyle(Value: TsmxPageManagerStyle); override;
    //procedure UnInstallParent; override;

    property PageControl: TPageControl read GetPageControl;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  published
    property ActivePageIndex;
    property CellAlign;
    property CellAnchors;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property ImageListName;
    property IsMultiLine;
    property PageManagerStyle;
    property PopupMenu;
    property SlaveListNew;

    property OnChangePage;
  end;

  { TsmxLibAction }

  {TsmxLibAction = class(TsmxAction)
  private
    FAlgorithmProcName: String;
    FAlgorithmLibrary: String;
    //FLibProc: TsmxProcAlgExecute;
    //FAction: TAction;
    //FParams: TsmxParams;
    //FAlgorithmLibrary: String;
    //FAlgorithmProcName: String;
    //procedure ActionExecute(Sender: TObject);
    //function GetAction: TAction;
    function GetProcPointer: Pointer;
  protected
    //function GetAlgorithmCaption: String; override;
    //function GetAlgorithmEnabled: Boolean; override;
    //function GetAlgorithmHint: String; override;
    //function GetAlgorithmHotKey: Integer; override;
    //function GetAlgorithmImageIndex: Integer; override;
    //function GetAlgorithmVisible: Boolean; override;
    //function GetInternalObject: TObject; override;
    procedure InternalInitialize; override;
    //procedure InternalRefreshParams; override;
    procedure ResetCellProps; override;
    //procedure SetAlgorithmCaption(const Value: String); override;
    //procedure SetAlgorithmEnabled(Value: Boolean); override;
    //procedure SetAlgorithmHint(const Value: String); override;
    //procedure SetAlgorithmHotKey(Value: Integer); override;
    //procedure SetAlgorithmImageIndex(Value: Integer); override;
    //procedure SetAlgorithmVisible(Value: Boolean); override;
    //procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetLibraryManager(Value: TsmxCustomLibraryManager); override;
    procedure SetAlgorithmLibrary(const Value: String); virtual;
    procedure SetAlgorithmProcName(const Value: String); virtual;

    //property Action: TAction read GetAction;
    //property Params: TsmxParams read FParams;
    //property LibProc: TsmxProcAlgExecute read GetLibProc;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    //destructor Destroy; override;
    //procedure Initialize(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
    //  ASelectRequest: TsmxCustomRequest = nil); override;
    //procedure RefreshParams; override;
    //procedure Execute; override;

    property AlgorithmLibrary: String read FAlgorithmLibrary write SetAlgorithmLibrary;
    property AlgorithmProcName: String read FAlgorithmProcName write SetAlgorithmProcName;
  end;}

  { TsmxActionList }

  {TsmxActionList = class(TsmxCustomAlgorithmList)
  private
    function GetCfg: TsmxActionListCfg;
  protected
    //procedure CreateChilds; override;
    //procedure InitChilds; override;

    property Cfg: TsmxActionListCfg read GetCfg;
  end;}

  { TsmxMenuPoint }

  {TsmxMenuPoint = class(TsmxCustomMenuPoint)
  private
    function GetCfg: TsmxMenuItemCfg;
  protected
    property Cfg: TsmxMenuItemCfg read GetCfg;
  end;}

  { TsmxMenuItem }

  TsmxMenuItem = class(TsmxCustomMenuItem)
  private
    FMenuItem: TMenuItem;
    function GetMenuItem: TMenuItem;
  protected
    function GetCellCaption: String; override;
    function GetCellEnabled: Boolean; override;
    function GetCellHint: String; override;
    function GetCellImageIndex: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetIsChecked: Boolean; override;
    function GetMenuItemHotKey: Integer; override;
    function GetMenuItemStyle: TsmxMenuItemStyle; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellImageIndex(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetIsChecked(Value: Boolean); override;
    procedure SetMenuItemHotKey(Value: Integer); override;
    procedure SetMenuItemStyle(Value: TsmxMenuItemStyle); override;
    procedure SetSlaveIndex(Value: Integer); override;

    property MenuItem: TMenuItem read GetMenuItem;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    //procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
    //procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
  end;

  { TsmxMasterMenu }

  {TsmxMasterMenu = class(TsmxCustomMasterMenu)
  private
    function GetCfg: TsmxMainMenuCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxMainMenuCfg read GetCfg;
  public
    function MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint; override;
  end;}

  { TsmxMainMenu }

  TsmxMainMenu = class(TsmxCustomMainMenu)
  private
    FMainMenu: TMainMenu;
    function GetMainMenu: TMainMenu;
  protected
    function GetCellEnabled: Boolean; override;
    function GetCellVisible: Boolean; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetImageList(Value: TCustomImageList); override;
    //procedure SetMenuItemList(Value: TsmxCustomMenuItem); override;

    property MainMenu: TMainMenu read GetMainMenu;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxPopupMenu }

  TsmxPopupMenu = class(TsmxCustomPopupMenu)
  private
    FPopupMenu: TPopupMenu;
    function GetPopupMenu: TPopupMenu;
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure SetImageList(Value: TCustomImageList); override;

    property PopupMenu: TPopupMenu read GetPopupMenu;
  public
    destructor Destroy; override;
  published
    property ImageListName;
  end;

  { TsmxPopupList }

  TsmxPopupList = class(TsmxCustomPopupList)
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TsmxToolItem }

  TsmxToolItem = class(TsmxCustomToolItem)
  private
    FToolButton: TToolButton;
    function GetToolButton: TToolButton;
  protected
    function GetCellCaption: String; override;
    function GetCellHint: String; override;
    function GetCellImageIndex: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetIsChecked: Boolean; override;
    function GetToolItemStyle: TsmxToolItemStyle; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellImageIndex(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetIsChecked(Value: Boolean); override;
    procedure SetToolItemStyle(Value: TsmxToolItemStyle); override;

    property ToolButton: TToolButton read GetToolButton;
  public
    destructor Destroy; override;
  published
    property CellCaption;
    property CellHint;
    property CellImageIndex;
    property IsChecked;
    property ToolItemStyle;
  end;

  { TsmxToolBar }

  TsmxToolBar = class(TsmxCustomToolBoard)
  private
    FToolBar: TToolBar;
    function GetToolBar: TToolBar;
  protected
    //function GetCellAlign: TAlign; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetIsFlat: Boolean; override;
    function GetIsShowCaptions: Boolean; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetIsFlat(Value: Boolean); override;
    procedure SetIsShowCaptions(Value: Boolean); override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    //procedure SetParentCell(Value: TsmxBaseCell); override;
    //procedure Initialize; override;
    //procedure UnInitialize; override;

    property ToolBar: TToolBar read GetToolBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
    //procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
    //procedure Prepare(Forcibly: Boolean = False); override;
  published
    property ImageListName;
    property IsFlat;
    property IsShowCaptions;
  end;

  { TsmxControlBoard }

  {TsmxControlBoard = class(TsmxCustomControlBoard)
  private
    function GetCfg: TsmxControlBoardCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxControlBoardCfg read GetCfg;
  end;}

  { TsmxControlBar }

  TsmxControlBar = class(TsmxCustomControlBoard)
  private
    FControlBar: TControlBar;
    function GetControlBar: TControlBar;
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetInternalObject: TObject; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    //function GetCellAlign: TAlign; override;
    //function GetCellEnabled: Boolean; override;
    //function GetCellHeight: Integer; override;
    //function GetCellLeft: Integer; override;
    //function GetCellTop: Integer; override;
    //function GetCellVisible: Boolean; override;
    //function GetCellWidth: Integer; override;
    //procedure SetCellAlign(Value: TAlign); override;
    //procedure SetCellEnabled(Value: Boolean); override;
    //procedure SetCellHeight(Value: Integer); override;
    //procedure SetCellLeft(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellTop(Value: Integer); override;
    //procedure SetCellVisible(Value: Boolean); override;
    //procedure SetCellWidth(Value: Integer); override;
    //procedure SetParentCell(Value: TsmxBaseCell); override;

    property ControlBar: TControlBar read GetControlBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TsmxStatusPanel }

  TsmxStatusPanel = class(TsmxCustomStatusItem)
  private
    FStatusPanel: TStatusPanel;
    function GetStatusPanel: TStatusPanel;
  protected
    procedure DoDrawPanel; override;
    function GetCellCaption: String; override;
    function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetStatusItemAlignment: TAlignment; override;
    function GetStatusItemStyle: TsmxStatusItemStyle; override;
    //procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetSlaveIndex(Value: Integer); override;
    procedure SetStatusItemAlignment(Value: TAlignment); override;
    procedure SetStatusItemStyle(Value: TsmxStatusItemStyle); override;

    property StatusPanel: TStatusPanel read GetStatusPanel;
  public
    destructor Destroy; override;
  end;

  { TsmxStatusBar }

  TsmxStatusBar = class(TsmxCustomStatusBoard)
  private
    FStatusBar: TStatusBar;
    function GetStatusBar: TStatusBar;
    procedure StatusBarDraw(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
  protected
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;

    property StatusBar: TStatusBar read GetStatusBar;
  public
    destructor Destroy; override;
  end;

  { TsmxForm }

  TsmxForm = class(TsmxCustomForm)
  private
    FForm: TForm;
    FFormImageIndex: Integer;
    FIsMainForm: Boolean;
    function GetForm: TForm;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    //procedure FormDestroy(Sender: TObject);
    //procedure PrepareForm;
  protected
    procedure DoClose; override;
    procedure DoShow; override;
    function GetCellActive: Boolean; override;
    function GetCellCaption: String; override;
    function GetCellHint: String; override;
    function GetCellImageIndex: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetFormBorder: TsmxFormBorder; override;
    function GetFormPosition: TsmxFormPosition; override;
    function GetInternalObject: TObject; override;
    //function GetIsMainForm: Boolean; override;
    function GetIsMaximize: Boolean; override;
    function GetModalResult: TModalResult; override;
    procedure InternalClose; override;
    //procedure InternalInitialize; override;
    procedure InternalShow; override;
    function InternalShowModal: TModalResult; override;
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetCellProps; override;
    procedure SetCellActive(Value: Boolean); override;
    procedure SetCellCaption(const Value: String); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellImageIndex(Value: Integer); override;
    procedure SetCellProps; override;
    procedure SetFormBorder(Value: TsmxFormBorder); override;
    procedure SetFormPosition(Value: TsmxFormPosition); override;
    procedure SetImageList(Value: TCustomImageList); override;
    //procedure SetIsMainForm(Value: Boolean); override;
    procedure SetIsMaximize(Value: Boolean); override;
    procedure SetModalResult(Value: TModalResult); override;
    procedure SetPopupMenu(Value: TsmxCustomPopupMenu); override;

    property Form: TForm read GetForm;
    property IsMainForm: Boolean read FIsMainForm;
  public
    destructor Destroy; override;
    //procedure Close; override;
    //procedure Show; override;
    //function ShowModal: TModalResult; override;
  published
    //property CellActive;
    property CellAlign;
    property CellAnchors;
    property CellCaption;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellHint;
    property CellImageIndex;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property FormBorder;
    property FormPosition;
    property ImageListName;
    property IsMaximize;
    property PopupMenu;

    property OnClose;
    property OnDoubleSnap;
    property OnShow;
    property OnSnap;
  end;

  { TsmxStandardForm }

  {TsmxStandardForm = class(TsmxCustomStandardForm)
  private
    FForm: TForm;
  protected
    procedure ProcClose(Sender: TObject; var Action: TCloseAction); virtual;
   // function GetFormModalResult:  TModalResult; override;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnabled: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //procedure SetFormManager(Value: TsmxCustomFormManager); override;
    //procedure SetFormModalResult(Value: TModalResult); override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    //procedure Initialize; override;
    //procedure UnInitialize; override;

    property Form: TForm read FForm;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
    //  ACfgID: Integer; AID: Integer = 0); override;
    //constructor CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
    //  ACfgID, AIntfID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
    //procedure CloseForm; override;
    //procedure ShowForm; override;
    //function ShowModalForm: TModalResult; override;
  end;}

  { TsmxMasterForm }

  {TsmxMasterForm = class(TsmxCustomMasterForm)
  private
    function GetCfg: TsmxFormCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxFormCfg read GetCfg;
  end;}

  { TsmxMainForm }

  {TsmxMainForm = class(TsmxMasterForm)
  private
    //FForm: TForm;
    //procedure SetForm(AForm: TForm);
  protected
    procedure ProcClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure ProcCloseQuery(Sender: TObject; var CanClose: Boolean); virtual;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnabled: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure Initialize; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure UnInitialize; override;
    procedure SetForm(Value: TForm); override;
  public
    procedure CloseForm; override;
    procedure ShowForm; override;

    //property Form: TForm read FForm write SetForm;
  end;}

implementation

uses
  SysUtils, Variants, ToolWin, Messages, {smxCommonStorage, smxLibManager,
  smxDBManager, smxFormManager, smxGlobalVariables, smxDBConnection,} smxFuncs,
  smxDBFuncs, smxClassFuncs, {smxLibFuncs,} smxConsts, smxClassProcs;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

  { _TMenuItem }

  _TMenuItem = class(TMenuItem)
  end;

  { _TWinControl }

  {_TWinControl = class(TWinControl)
  end;}

{ TsmxAction }

{constructor TsmxAction.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //@FLibProc := LibManager.GetProcedure(Cfg.AlgLibrary, Cfg.AlgProcedure);
  //@FLibProc := FindProcedureByNameLib(Cfg.AlgLibrary, Cfg.AlgProcedure);
  //AddParams;
end;}

destructor TsmxAction.Destroy;
begin
  inherited Destroy;
  if Assigned(FAction) then
    FAction.Free;
end;

procedure TsmxAction.ActionExecute(Sender: TObject);
begin
  Execute;
end;

procedure TsmxAction.DoRefreshParams;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnRefreshParams) then
  begin
    if Cfg is TsmxAlgorithmCfg then
      AlgCfgID := TsmxAlgorithmCfg(Cfg).RefreshParamsCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefreshParams, AlgCfgID);
  end;
end;

function TsmxAction.GetAction: TAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := TAction.Create({Self} nil); // nil as Self destroy inner object by execute free
    FAction.OnExecute := ActionExecute;
  end;
  Result := FAction;
end;

function TsmxAction.GetAlgorithmCaption: String;
begin
  Result := Action.Caption;
end;

procedure TsmxAction.SetAlgorithmCaption(const Value: String);
begin
  Action.Caption := Value;
end;

function TsmxAction.GetAlgorithmEnabled: Boolean;
begin
  Result := Action.Enabled;
end;

procedure TsmxAction.SetAlgorithmEnabled(Value: Boolean);
begin
  Action.Enabled := Value;
end;

function TsmxAction.GetAlgorithmHint: String;
begin
  Result := Action.Hint;
end;

procedure TsmxAction.SetAlgorithmHint(const Value: String);
begin
  Action.Hint := Value;
end;

function TsmxAction.GetAlgorithmHotKey: Integer;
begin
  Result := Integer(Action.ShortCut);
end;

procedure TsmxAction.SetAlgorithmHotKey(Value: Integer);
begin
  Action.ShortCut := TShortCut(Value);
end;

function TsmxAction.GetAlgorithmImageIndex: Integer;
begin
  Result := Integer(Action.ImageIndex);
end;

procedure TsmxAction.SetAlgorithmImageIndex(Value: Integer);
begin
  Action.ImageIndex := TImageIndex(Value);
end;

function TsmxAction.GetAlgorithmVisible: Boolean;
begin
  Result := Action.Visible;
end;

procedure TsmxAction.SetAlgorithmVisible(Value: Boolean);
begin
  Action.Visible := Value;
end;

function TsmxAction.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxAlgorithmCfg;
end;

function TsmxAction.GetInternalObject: TObject;
begin
  Result := Action;
end;

{function TsmxAction.GetParamValue(const ParamName: String): Variant;
var
  Param: TsmxParamKitItem;
begin
  Result := Variants.Null;
  if Cfg is TsmxAlgorithmCfg then
  begin
    Param := TsmxAlgorithmCfg(Cfg).AlgorithmParams.FindByName(ParamName);
    if Assigned(Param) then
      Result := Param.ParamValue;
  end;
end;}

{function TsmxAction.GetProcPointer: Pointer;
begin
  Result := nil;
end;}

{procedure TsmxAction.InitializeEvent;
var
  //Proc: Pointer;
  Method: TMethod;
begin
  //Proc := GetProcudure;
  if Assigned(ProcPointer) then
  begin
    Method.Code := ProcPointer;
    Method.Data := Self;
    OnExecute := TsmxComponentEvent(Method);
  end;
end;}

{procedure TsmxAction.InternalInitialize;
var
  i: Integer;
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxAlgorithmCfg then
  begin
    AlgorithmCaption := TsmxAlgorithmCfg(Cfg).AlgorithmCaption;
    AlgorithmEnabled := TsmxAlgorithmCfg(Cfg).AlgorithmEnabled;
    AlgorithmHint := TsmxAlgorithmCfg(Cfg).AlgorithmHint;
    AlgorithmHotKey := TsmxAlgorithmCfg(Cfg).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxAlgorithmCfg(Cfg).AlgorithmImageIndex;
    AlgorithmVisible := TsmxAlgorithmCfg(Cfg).AlgorithmVisible;
    for i := 0 to TsmxAlgorithmCfg(Cfg).AlgorithmParams.Count - 1 do
      with AlgorithmParams.Add do
      begin
        DataType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].DataType;
        ParamLocation := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamLocation;
        ParamType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamType;
        ParamName := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamName;
        ParamValue := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].Value;
      end;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxAlgorithmCfg(Cfg).RefreshParamsCfgID);
  end;
end;}

procedure TsmxAction.InternalRefreshParams;
var
  i, j: Integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
  Cell: TsmxOwnerCell;
begin
  //inherited InternalRefreshParams;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to AlgorithmParams.Count - 1 do
    begin
      //Value := Variants.Null;
      case AlgorithmParams[i].ParamLocation of
        {plConst .. plOutput, plFilterDesk .. plParentGrid:
        begin
          Value := Variants.Null; //AlgorithmParams[i].ParamValue;
        end;}
        {plConst .. plInOutput:
        begin
          AlgorithmParams[i].ParamValue := GetParamValue(AlgorithmParams[i].ParamName);
          if Assigned(CellOwner) then
            if CellOwner.GetAlgorithmParamValue(CfgID, AlgorithmParams[i].ParamName, Value) then
              AlgorithmParams[i].ParamValue := Value;
        end;}
        plFilterDesk:
        begin
          if CellAction is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellAction).FindSlaveByName(AlgorithmParams[i].ParamName);
            if Assigned(Cell) then
              AlgorithmParams[i].ParamValue := TsmxCustomFilter(Cell).FilterValue;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, AlgorithmParams[i].ParamName, Value) then
            AlgorithmParams[i].ParamValue := Value;
        end;
        plGrid:
        begin
          if CellAction is TsmxCustomGrid then
          begin
            Cell := TsmxCustomGrid(CellAction).FindSlaveByName(AlgorithmParams[i].ParamName);
            if Assigned(Cell) then
              AlgorithmParams[i].ParamValue := TsmxCustomColumn(Cell).ColumnValue;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, AlgorithmParams[i].ParamName, Value) then
            AlgorithmParams[i].ParamValue := Value;
        end;
        plParentFilterDesk:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
        plParentGrid:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
        plStorageParam:
        begin
          if Assigned(smxClassProcs.gStorageManagerIntf) then
            AlgorithmParams[i].ParamValue := smxClassProcs.gStorageManagerIntf.Values[AlgorithmParams[i].ParamName];
        end;
        plParentParam:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
      end;
      //AlgorithmParams[i].ParamValue := Value;
    end;
  finally
    List.Free;
  end;
end;

procedure TsmxAction.ResetCellProps;
begin
  inherited ResetCellProps;
  AlgorithmCaption := '';
  AlgorithmEnabled := False;
  AlgorithmHint := '';
  AlgorithmHotKey := 0;
  AlgorithmImageIndex := -1;
  AlgorithmParams.Clear;
  AlgorithmVisible := False;
  OnExecute := nil;
  OnRefreshParams := nil;
end;

procedure TsmxAction.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Action.ActionList := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TCustomActionList then
      Action.ActionList := TCustomActionList(Obj);
  end;
end;

procedure TsmxAction.SetCellProps;
var
  i: Integer;
  Form: TsmxCustomForm;
  //Method: TMethod;
begin
  inherited SetCellProps;
  if Cfg is TsmxAlgorithmCfg then
  begin
    AlgorithmCaption := TsmxAlgorithmCfg(Cfg).AlgorithmCaption;
    AlgorithmEnabled := TsmxAlgorithmCfg(Cfg).AlgorithmEnabled;
    AlgorithmHint := TsmxAlgorithmCfg(Cfg).AlgorithmHint;
    AlgorithmHotKey := TsmxAlgorithmCfg(Cfg).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxAlgorithmCfg(Cfg).AlgorithmImageIndex;
    AlgorithmVisible := TsmxAlgorithmCfg(Cfg).AlgorithmVisible;
    for i := 0 to TsmxAlgorithmCfg(Cfg).AlgorithmParams.Count - 1 do
      with AlgorithmParams.Add do
      begin
        DataType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].DataType;
        ParamLocation := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamLocation;
        ParamType := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamType;
        ParamName := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamName;
        ParamValue := TsmxAlgorithmCfg(Cfg).AlgorithmParams[i].ParamValue;
      end;
    {if Assigned(GetProcPointer()) then
    begin
      Method.Code := GetProcPointer;
      Method.Data := Self;
      OnExecute := TsmxComponentEvent(Method);
    end;}
    OnExecute := AlgorithmExecute;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxAlgorithmCfg(Cfg).RefreshParamsCfgID);
  end;
end;

{procedure TsmxAction.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  if Assigned(LibraryManager) then
    FLibProc := nil;
  inherited SetLibraryManager(Value);
  if Assigned(LibraryManager) then
    @FLibProc := LibraryManager.GetProcedure(Cfg.AlgLibrary, Cfg.AlgProcedure);
end;}

procedure TsmxAction.SetSlaveIndex(Value: Integer);
begin
  inherited SetSlaveIndex(Value);
  Action.Index := Value;
end;

{ TsmxActionList }

constructor TsmxActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

destructor TsmxActionList.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FActionList) then
    FActionList.Free;
end;

function TsmxActionList.GetActionList: TActionList;
begin
  if not Assigned(FActionList) then
    FActionList := TActionList.Create(nil);
  Result := FActionList;
end;

function TsmxActionList.GetAlgorithmParamValue(CfgID: Integer;
  const ParamName: String; var ParamValue: Variant): Boolean;
var
  //i: Integer;
  Param: TsmxParamKitItem;
  Item: TsmxOwnerKitItem;
begin
  Result := inherited GetAlgorithmParamValue(CfgID, ParamName, ParamValue);
  if Cfg is TsmxAlgorithmListCfg then
  begin
    Item := TsmxAlgorithmListCfg(Cfg).SlaveCells.FindByCfgID(CfgID);
    if Item is TsmxAlgorithmKitItem then
    begin
      Param := TsmxAlgorithmKitItem(Item).ItemParams.FindByName(ParamName);
      if Assigned(Param) then
      begin
        ParamValue := Param.ParamValue;
        Result := True;
      end;
    end;
  end;

    {if Algorithm.SlaveIndex < TsmxAlgorithmListCfg(Cfg).SlaveCells.Count then
      for i := 0 to Algorithm.AlgorithmParams.Count - 1 do
        Algorithm.AlgorithmParams[i].ParamValue :=
          TsmxAlgorithmListCfg(Cfg).SlaveCells[Algorithm.SlaveIndex].ItemParams[i].ParamValue;}

  {for i := 0 to TsmxAlgorithmKitItem(Item).ItemParams.Count - 1 do
  begin
    Param := TsmxCustomAlgorithm(Slave).AlgorithmParams.FindByName(TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamName);
    if Assigned(Param) then
      Param.ParamValue := TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamValue;
  end;}
end;

{function TsmxActionList.GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass;
begin
  if Cfg is TsmxAlgorithmListCfg then
    Result := TsmxOwnerCellClass(smxClassFuncs.CfgIDToCellClass(
      TsmxAlgorithmListCfg(Cfg).SlaveCells[Index].CfgID, SelectRequest))
  else
    Result := inherited GetAltSlaveClass(Index);
end;}

function TsmxActionList.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxAlgorithmListCfg;
end;

function TsmxActionList.GetInternalObject: TObject;
begin
  Result := ActionList;
end;

function TsmxActionList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxAction;
end;

procedure TsmxActionList.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    ActionList.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    ActionList.Images := ImageList;
end;

{procedure TsmxActionList.SetParentCell(Value: TsmxBaseCell);
begin
  inherited SetParentCell(Value);
  FActionList.Images := ImageList;
end;}

procedure TsmxActionList.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
var
  i: Integer;
  Param: TsmxParamKitItem;
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxCustomAlgorithm) and (Item is TsmxAlgorithmKitItem) then
  begin
    TsmxCustomAlgorithm(Slave).AlgorithmEnabled := TsmxAlgorithmKitItem(Item).ItemEnabled;
    TsmxCustomAlgorithm(Slave).AlgorithmVisible := TsmxAlgorithmKitItem(Item).ItemVisible;
    for i := 0 to TsmxCustomAlgorithm(Slave).AlgorithmParams.Count - 1 do
    begin
      Param := TsmxAlgorithmKitItem(Item).ItemParams.FindByName(TsmxCustomAlgorithm(Slave).AlgorithmParams[i].ParamName);
      if Assigned(Param) then
        TsmxCustomAlgorithm(Slave).AlgorithmParams[i].ParamValue := Param.ParamValue;
    end;

    {for i := 0 to TsmxAlgorithmKitItem(Item).ItemParams.Count - 1 do
    begin
      Param := TsmxCustomAlgorithm(Slave).AlgorithmParams.FindByName(TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamName);
      if Assigned(Param) then
        Param.ParamValue := TsmxAlgorithmKitItem(Item).ItemParams.Items[i].ParamValue;
    end;}
  end;
end;

{ TsmxRequest }

procedure TsmxRequest.DoDelete;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnDelete) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).DeleteAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnDelete, AlgCfgID);
  end;
end;

procedure TsmxRequest.DoExecute;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnExecute) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).ExecuteAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnExecute, AlgCfgID);
  end;
end;

procedure TsmxRequest.DoInsert;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnInsert) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).InsertAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnInsert, AlgCfgID);
  end;
end;

procedure TsmxRequest.DoPrepare;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnPrepare) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).PrepareAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnPrepare, AlgCfgID);
  end;
end;

procedure TsmxRequest.DoRefreshParams;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnRefreshParams) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).RefreshParamsAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefreshParams, AlgCfgID);
  end;
end;

procedure TsmxRequest.DoUpdate;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnUpdate) then
  begin
    if Cfg is TsmxRequestCfg then
      AlgCfgID := TsmxRequestCfg(Cfg).UpdateAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnUpdate, AlgCfgID);
  end;
end;

function TsmxRequest.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxRequestCfg;
end;

{function TsmxRequest.GetDataSet: IsmxDataSet;
begin
  Result := nil;
end;}

(*procedure TsmxRequest.InitializeDataSet;
var
  i: Integer;
  //DataSetIntf: IsmxDataSet;
begin
  //DataSetIntf := GetDataSet;
  if Assigned(DataSet{Intf}) {and Assigned(Database)} then
  begin
    //DataSetIntf.Database := Database;
    if Cfg is TsmxRequestCfg then
    begin
      DataSet{Intf}.SQL.Text := TsmxRequestCfg(Cfg).SQLText;
      DataSet{Intf}.ClearFields;
      for i := 0 to TsmxRequestCfg(Cfg).Fields.Count - 1 do
        with DataSet{Intf}.AddField(TsmxRequestCfg(Cfg).Fields[i].DataType) do
        begin
          FieldName := TsmxRequestCfg(Cfg).Fields[i].FieldName;
          DisplayFormat := TsmxRequestCfg(Cfg).Fields[i].DisplayFormat;
          FieldSense := TsmxRequestCfg(Cfg).Fields[i].FieldSense;
          Precision := TsmxRequestCfg(Cfg).Fields[i].Precision;
          Size := TsmxRequestCfg(Cfg).Fields[i].Size;
        end;
      DataSet{Intf}.ClearParams;
      for i := 0 to TsmxRequestCfg(Cfg).Params.Count - 1 do
        with DataSet{Intf}.AddParam(TsmxRequestCfg(Cfg).Params[i].DataType) do
        begin
          ParamName := TsmxRequestCfg(Cfg).Params[i].ParamName;
          ParamLocation := TsmxRequestCfg(Cfg).Params[i].ParamLocation;
          ParamType := TsmxRequestCfg(Cfg).Params[i].ParamType;
          Value := TsmxRequestCfg(Cfg).Params[i].Value;
          NumericScale := TsmxRequestCfg(Cfg).Params[i].NumericScale;
          Precision := TsmxRequestCfg(Cfg).Params[i].Precision;
          Size := TsmxRequestCfg(Cfg).Params[i].Size;
        end;
    end;
    //DataSet := DataSetIntf;
  end;
end;*)

(*procedure TsmxRequest.InternalInitialize;
var
  Form: TsmxCustomForm;
  //DataSetIntf: IsmxDataSet;
  //i: Integer;
begin
  inherited InternalInitialize;
  if Cfg is TsmxRequestCfg then
  begin
    DatabaseName := TsmxRequestCfg(Cfg).DatabaseName;
    OperationMode := TsmxRequestCfg(Cfg).OperationMode;
    PerformanceMode := TsmxRequestCfg(Cfg).PerformanceMode;
    ModifyPerformances[mrDelete] := TsmxRequestCfg(Cfg).DeletePerformance;
    ModifyPerformances[mrInsert] := TsmxRequestCfg(Cfg).InsertPerformance;
    ModifyPerformances[mrUpdate] := TsmxRequestCfg(Cfg).UpdatePerformance;

    //DataSetIntf := smxClassFuncs.NewIntf(CfgID, SelectRequest) as IsmxDataSet;
    {DataSetIntf := GetDataSet;
    if Assigned(DataSetIntf) then
    begin
      DataSetIntf.SQL.Text := TsmxRequestCfg(Cfg).SQLText;
      for i := 0 to TsmxRequestCfg(Cfg).Fields.Count - 1 do
        with DataSetIntf.AddField(TsmxRequestCfg(Cfg).Fields[i].FieldName) do
        begin
          DisplayFormat := TsmxRequestCfg(Cfg).Fields[i].DisplayFormat;
          FieldSense := TsmxRequestCfg(Cfg).Fields[i].FieldSense;
        end;
      for i := 0 to TsmxRequestCfg(Cfg).Params.Count - 1 do
        with DataSetIntf.AddParam(TsmxRequestCfg(Cfg).Params[i].ParamName) do
        begin
          DataType := TsmxRequestCfg(Cfg).Params[i].DataType;
          ParamLocation := TsmxRequestCfg(Cfg).Params[i].ParamLocation;
          ParamType := TsmxRequestCfg(Cfg).Params[i].ParamType;
          Value := TsmxRequestCfg(Cfg).Params[i].Value;
        end;
      DataSet := DataSetIntf;
    end;}
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      ModifyRequests[mrDelete] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).DeleteReqCfgID);
      ModifyRequests[mrInsert] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).InsertReqCfgID);
      ModifyRequests[mrUpdate] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).UpdateReqCfgID);

      OnDelete := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnExecute := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnInsert := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnUpdate := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
    end;
  end; //else
    //raise EsmxCellError.CreateFmt(@rsCellActionError, [ClassName, 'initialize']);
end;*)

{function TsmxRequest.GetParamValue(const ParamName: String): Variant;
var
  Param: TsmxParamKitItem;
begin
  Result := Variants.Null;
  if Cfg is TsmxRequestCfg then
  begin
    Param := TsmxRequestCfg(Cfg).Params.FindByName(ParamName);
    if Assigned(Param) then
      Result := Param.ParamValue;
  end;
end;}

procedure TsmxRequest.InternalRefreshParams;
var
  i, j: integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
  Cell: TsmxOwnerCell;
begin
  inherited InternalRefreshParams;
  if not Assigned(CurDataSet) then
    Exit;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to CurDataSet.ParamCount - 1 do
    begin
      //Value := Variants.Null;
      case CurDataSet.Params[i].ParamLocation of
        {plConst .. plOutput, plFilterDesk .. plParentGrid:
        begin
          Value := Variants.Null; //FCurDataSetIntf.Params[i].Value;
        end;}
        {plConst .. plInOutput:
        begin
          CurDataSet.Params[i].Value := GetParamValue(CurDataSet.Params[i].ParamName);
          if Assigned(CellOwner) then
            if CellOwner.GetRequestParamValue(CfgID, CurDataSet.Params[i].ParamName, Value) then
              CurDataSet.Params[i].Value := Value;
        end;}
        plFilterDesk:
        begin
          {smxClassProcs.AllParents(Self, List, [TsmxCustomSection]);
          if List.Count > 0 then
            smxClassFuncs.FindFilterOnSection(TsmxCustomSection(List[0]),
              FCurDataSetIntf.Params[i].ParamName, Value);}
          //Cell := nil;
          if CellRequest is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellRequest).FindSlaveByName(CurDataSet.Params[i].ParamName);
            if Assigned(Cell) then
              CurDataSet.Params[i].Value := TsmxCustomFilter(Cell).FilterValue;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, CurDataSet.Params[i].ParamName, Value) then
            CurDataSet.Params[i].Value := Value;
        end;
        plGrid:
        begin
          {smxClassProcs.AllParents(Self, List, [TsmxCustomPage]);
          if List.Count > 0 then
            for j := 0 to TsmxCustomPage(List[0]).SlaveCount - 1 do
              if not smxClassFuncs.ExistsParent(Self, TsmxCustomPage(List[0]).Slaves[j]) then
                if smxClassFuncs.FindColumnOnSection(TsmxCustomPage(List[0]).Slaves[j],
                    FCurDataSetIntf.Params[i].ParamName, Value) then
                  Break;}
          if CellRequest is TsmxCustomGrid then
          begin
            if CellRequest.CellParent is TsmxCustomSection then
              if CellRequest.CellParent.CellParent is TsmxCustomPage then
                with TsmxCustomPage(CellRequest.CellParent.CellParent) do
                  for j := 0 to SlaveCount - 1 do
                    if CellRequest.CellParent <> Slaves[j] then
                      if smxClassFuncs.FindColumnOnSection(Slaves[j], CurDataSet.Params[i].ParamName, Value) then
                      begin
                        CurDataSet.Params[i].Value := Value;
                        Break;
                      end;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, CurDataSet.Params[i].ParamName, Value) then
            CurDataSet.Params[i].Value := Value;
        end;
        plParentFilterDesk:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        plParentGrid:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        plStorageParam:
        begin
          if Assigned(smxClassProcs.gStorageManagerIntf) then
            CurDataSet.Params[i].Value := smxClassProcs.gStorageManagerIntf.Values[CurDataSet.Params[i].ParamName];
        end;
        plParentParam:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
      end;
      //FCurDataSetIntf.Params[i].Value := Value;
    end;
  finally
    List.Free;
  end;
end;

procedure TsmxRequest.ResetCellProps;
begin
  inherited ResetCellProps;
  DatabaseName := '';
  //DataSet := nil;
  //ModifyPerformances[mrDelete] := pmOpen;
  //ModifyPerformances[mrInsert] := pmOpen;
  //ModifyPerformances[mrUpdate] := pmOpen;

  //DeletePerformance := pmOpen;
  //InsertPerformance := pmOpen;
  //UpdatePerformance := pmOpen;

  //ModifyRequests[mrDelete] := nil;
  //ModifyRequests[mrInsert] := nil;
  //ModifyRequests[mrUpdate] := nil;
  DeleteDataSet := nil;
  InsertDataSet := nil;
  UpdateDataSet := nil;
  OperationMode := omManual;
  //PerformanceMode := pmOpen;
  OnDelete := nil;
  OnExecute := nil;
  OnInsert := nil;
  OnPrepare := nil;
  OnRefreshParams := nil;
  OnUpdate := nil;
end;

procedure TsmxRequest.SetCellProps;
var
  i: Integer;
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxRequestCfg then
  begin
    DatabaseName := TsmxRequestCfg(Cfg).DatabaseName;
    OperationMode := TsmxRequestCfg(Cfg).OperationMode;
    //PerformanceMode := TsmxRequestCfg(Cfg).PerformanceMode;
    //ModifyPerformances[mrDelete] := TsmxRequestCfg(Cfg).DeletePerformance;
    //ModifyPerformances[mrInsert] := TsmxRequestCfg(Cfg).InsertPerformance;
    //ModifyPerformances[mrUpdate] := TsmxRequestCfg(Cfg).UpdatePerformance;

    //DeletePerformance := TsmxRequestCfg(Cfg).DeletePerformance;
    //InsertPerformance := TsmxRequestCfg(Cfg).InsertPerformance;
    //UpdatePerformance := TsmxRequestCfg(Cfg).UpdatePerformance;

    //DataSetIntf := smxClassFuncs.NewIntf(CfgID, SelectRequest) as IsmxDataSet;
    //DataSetIntf := GetDataSet;
    if Assigned(DataSet) then
    begin
      DataSet.SQLText := TsmxRequestCfg(Cfg).SQLText;
      DataSet.ClearFields;
      for i := 0 to TsmxRequestCfg(Cfg).Fields.Count - 1 do
        with DataSet.AddField do
        begin
          DataType :=  TsmxRequestCfg(Cfg).Fields[i].DataType;
          FieldName := TsmxRequestCfg(Cfg).Fields[i].FieldName;
          DisplayFormat := TsmxRequestCfg(Cfg).Fields[i].DisplayFormat;
          FieldSense := TsmxRequestCfg(Cfg).Fields[i].FieldSense;
          Precision := TsmxRequestCfg(Cfg).Fields[i].Precision;
          Size := TsmxRequestCfg(Cfg).Fields[i].Size;
        end;
      DataSet.ClearParams;
      for i := 0 to TsmxRequestCfg(Cfg).Params.Count - 1 do
        with DataSet.AddParam do
        begin
          DataType := TsmxRequestCfg(Cfg).Params[i].DataType;
          ParamName := TsmxRequestCfg(Cfg).Params[i].ParamName;
          ParamLocation := TsmxRequestCfg(Cfg).Params[i].ParamLocation;
          ParamType := TsmxRequestCfg(Cfg).Params[i].ParamType;
          Value := TsmxRequestCfg(Cfg).Params[i].ParamValue;
          NumericScale := TsmxRequestCfg(Cfg).Params[i].NumericScale;
          Precision := TsmxRequestCfg(Cfg).Params[i].Precision;
          Size := TsmxRequestCfg(Cfg).Params[i].Size;
        end;
    end;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      //ModifyRequests[mrDelete] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).DeleteReqCfgID);
      //ModifyRequests[mrInsert] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).InsertReqCfgID);
      //ModifyRequests[mrUpdate] := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).UpdateReqCfgID);
      DeleteDataSet := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).DeleteReqCfgID);
      InsertDataSet := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).InsertReqCfgID);
      UpdateDataSet := smxClassFuncs.GetDataSetForm(Form, TsmxRequestCfg(Cfg).UpdateReqCfgID);


      OnDelete := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnExecute := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnInsert := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnRefreshParams := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
      OnUpdate := smxClassFuncs.GetEventForm(Form, TsmxRequestCfg(Cfg).DeleteAlgCfgID);
    end;
  end;
end;

{ TsmxRequestList }

constructor TsmxRequestList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

function TsmxRequestList.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxRequestListCfg;
end;

function TsmxRequestList.GetRequestParamValue(CfgID: Integer;
  const ParamName: String; var ParamValue: Variant): Boolean;
var
  //i: Integer;
  Param: TsmxParamKitItem;
  Item: TsmxOwnerKitItem;
begin
  Result := inherited GetRequestParamValue(CfgID, ParamName, ParamValue);
  if Cfg is TsmxRequestListCfg then
  begin
    Item := TsmxRequestListCfg(Cfg).SlaveCells.FindByCfgID(CfgID);
    if Item is TsmxRequestKitItem then
    begin
      Param := TsmxRequestKitItem(Item).ItemParams.FindByName(ParamName);
      if Assigned(Param) then
      begin
        ParamValue := Param.ParamValue;
        Result := True;
      end;
    end;
  end;
end;

function TsmxRequestList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxRequest;
end;

procedure TsmxRequestList.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
var
  i: Integer;
  Param: TsmxParamKitItem;
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxCustomRequest) and (Item is TsmxRequestKitItem) then
  begin
    TsmxCustomRequest(Slave).DatabaseName := TsmxRequestKitItem(Item).DatabaseName;
    TsmxCustomRequest(Slave).OperationMode := TsmxRequestKitItem(Item).OperationMode;
    if Assigned(TsmxCustomRequest(Slave).DataSet) then
      for i := 0 to TsmxCustomRequest(Slave).DataSet.ParamCount - 1 do
      begin
        Param := TsmxRequestKitItem(Item).ItemParams.FindByName(TsmxCustomRequest(Slave).DataSet.Params[i].ParamName);
        if Assigned(Param) then
          TsmxCustomRequest(Slave).DataSet.Params[i].Value := Param.ParamValue;
      end;
  end;
end;

{ TsmxColumn }

destructor TsmxColumn.Destroy;
begin
  inherited Destroy;
  if Assigned(FColumn) then
    FColumn.Free;
end;

procedure TsmxColumn.DoSnapHeader;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnSnapHeader) then
  begin
    if Cfg is TsmxColumnCfg then
      AlgCfgID := TsmxColumnCfg(Cfg).SnapAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnSnapHeader, AlgCfgID);
  end;
end;

function TsmxColumn.GetCellVisible: Boolean;
begin
  Result := Column.Visible;
end;

procedure TsmxColumn.SetCellVisible(Value: Boolean);
begin
  Column.Visible := Value;
end;

function TsmxColumn.GetCellWidth: Integer;
begin
  Result := Column.Width;
end;

procedure TsmxColumn.SetCellWidth(Value: Integer);
begin
  Column.Width := Value;
end;

function TsmxColumn.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxColumnCfg;
end;

function TsmxColumn.GetColumn: TColumn;
begin
  if not Assigned(FColumn) then
    FColumn := TColumn.Create(nil);
  Result := FColumn;
end;

function TsmxColumn.GetColumnAlignment: TAlignment;
begin
  Result := Column.Alignment;
end;

procedure TsmxColumn.SetColumnAlignment(Value: TAlignment);
begin
  Column.Alignment := Value;
end;

function TsmxColumn.GetColumnCaption: String;
//var
  //Val: Variant;
begin
  //Result := Variants.VarToStr(GetFieldValue(smxFuncs.GetTextFieldName(SlaveName)));
  //GetFieldValue(smxFuncs.GetTextFieldName(SlaveName), Val);
  //Result := Variants.VarToStr(Val);
  Result := '';
  if CellOwner is TsmxCustomGrid then
    //with TsmxCustomGrid(CellOwner) do
      //if FocusedRowIndex <> -1 then
    Result := TsmxCustomGrid(CellOwner).GridCaptions[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
end;

procedure TsmxColumn.SetColumnCaption(const Value: String);
//var
  //Val: Variant;
begin
  //SetFieldValue(smxFuncs.GetTextFieldName(SlaveName), smxFuncs.StrToVar(Value));
  //Val := Variants.VarToStr(Value);
  //SetFieldValue(smxFuncs.GetTextFieldName(SlaveName), Val);
  if CellOwner is TsmxCustomGrid then
    //with TsmxCustomGrid(CellOwner) do
      //if FocusedRowIndex <> -1 then
    TsmxCustomGrid(CellOwner).GridCaptions[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
end;

function TsmxColumn.GetColumnColor: TColor;
begin
  Result := Column.Color;
end;

procedure TsmxColumn.SetColumnColor(Value: TColor);
begin
  Column.Color := Value;
end;

function TsmxColumn.GetColumnFont: TFont;
begin
  Result := Column.Font;
end;

procedure TsmxColumn.SetColumnFont(Value: TFont);
begin
  Column.Font := Value;
end;

function TsmxColumn.GetColumnValue: Variant;
begin
  //if not GetFieldValue(smxFuncs.GetValueFieldName(SlaveName), Result) then
  //GetFieldValue(SlaveName, Result);
  //Result := GetFieldValue(smxFuncs.GetValueFieldName(SlaveName));

  Result := Variants.Null;
  if coSetValue in ColumnOptions then
    if CellOwner is TsmxCustomGrid then
      //with TsmxCustomGrid(CellOwner) do
        //if FocusedRowIndex <> -1 then
      Result := TsmxCustomGrid(CellOwner).GridValues[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
  {if Assigned(Column.Field) then
    Result := Column.Field.Value else
    Result := Variants.Null;}
end;

procedure TsmxColumn.SetColumnValue(const Value: Variant);
begin
  //SetFieldValue({smxFuncs.GetValueFieldName(}SlaveName{)}, Value);
  if coSetValue in ColumnOptions then
    if CellOwner is TsmxCustomGrid then
      //with TsmxCustomGrid(CellOwner) do
        //if FocusedRowIndex <> -1 then
      TsmxCustomGrid(CellOwner).GridValues[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
  {if Assigned(Column.Field) then
  begin

  end;}
end;

{function TsmxColumn.GetFieldName: String;
begin
  Result := Column.FieldName;
end;

procedure TsmxColumn.SetFieldName(const Value: String);
begin
  Column.FieldName := Value;
end;}

{function TsmxColumn.GetFieldValue(const FieldName: String; var Value: Variant): Boolean;
var
  Field: IsmxField;
begin
  Result := False;
  Value := Variants.Null;
  if Assigned(CellOwner) then
    if Assigned(CellOwner.Request) then
      if Assigned(CellOwner.Request.DataSet) then
      begin
        Field := CellOwner.Request.DataSet.FindField(FieldName);
        if Assigned(Field) then
        begin
          Value := Field.Value;
          Result := True;
        end;
      end;
end;}

{function TsmxColumn.SetFieldValue(const FieldName: String; const Value: Variant): Boolean;
var
  Field: IsmxField;
begin
  Result := False;
  if Assigned(CellOwner) then
    if Assigned(CellOwner.Request) then
      if Assigned(CellOwner.Request.DataSet) then
      begin
        Field := CellOwner.Request.DataSet.FindField(FieldName);
        if Assigned(Field) then
        begin
          CellOwner.Request.DataSet.Edit;
          Field.Value := Value;
          CellOwner.Request.DataSet.Post;
          Result := True;
        end;
      end;
end;}

function TsmxColumn.GetHeaderAlignment: TAlignment;
begin
  Result := Column.Title.Alignment;
end;

procedure TsmxColumn.SetHeaderAlignment(Value: TAlignment);
begin
  Column.Title.Alignment := Value;
end;

function TsmxColumn.GetHeaderCaption: String;
begin
  Result := Column.Title.Caption;
end;

procedure TsmxColumn.SetHeaderCaption(const Value: String);
begin
  Column.Title.Caption := Value;
end;

function TsmxColumn.GetHeaderColor: TColor;
begin
  Result := Column.Title.Color;
end;

procedure TsmxColumn.SetHeaderColor(Value: TColor);
begin
  if IsOwnerDrawHeader then
    Column.Title.Color := Value;
end;

function TsmxColumn.GetHeaderFont: TFont;
begin
  Result := Column.Title.Font;
end;

procedure TsmxColumn.SetHeaderFont(Value: TFont);
begin
  if IsOwnerDrawHeader then  // м.б. предется мудрить с установкой св-тв шрифта
    Column.Title.Font := Value;
end;

function TsmxColumn.GetInternalObject: TObject;
begin
  Result := Column;
end;

{function TsmxColumn.GetIsEditing: Boolean;
begin
  Result := not Column.ReadOnly;
end;}

{procedure TsmxColumn.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxGridColumnCfg then
  begin
    ColumnAlignment := TsmxGridColumnCfg(Cfg).ColumnText.Alignment;
    //ColumnCaption := TsmxGridColumnCfg(Cfg).ColumnText.Caption;
    ColumnColor := TColor(TsmxGridColumnCfg(Cfg).ColumnText.Color);
    ColumnFont.Color := TColor(TsmxGridColumnCfg(Cfg).ColumnText.Font.Color);
    ColumnFont.Name := TsmxGridColumnCfg(Cfg).ColumnText.Font.Name;
    ColumnFont.Size := TsmxGridColumnCfg(Cfg).ColumnText.Font.Size;
    ColumnFont.Style := TsmxGridColumnCfg(Cfg).ColumnText.Font.Style;
    FieldName := TsmxGridColumnCfg(Cfg).ColumnFieldName;
    HeaderAlignment := TsmxGridColumnCfg(Cfg).ColumnHeader.Alignment;
    HeaderCaption := TsmxGridColumnCfg(Cfg).ColumnHeader.Caption;
    HeaderColor := TColor(TsmxGridColumnCfg(Cfg).ColumnHeader.Color);
    HeaderFont.Color := TColor(TsmxGridColumnCfg(Cfg).ColumnHeader.Font.Color);
    HeaderFont.Name := TsmxGridColumnCfg(Cfg).ColumnHeader.Font.Name;
    HeaderFont.Size := TsmxGridColumnCfg(Cfg).ColumnHeader.Font.Size;
    HeaderFont.Style := TsmxGridColumnCfg(Cfg).ColumnHeader.Font.Style;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnSnapHeader := smxClassFuncs.GetEventForm(Form, TsmxGridColumnCfg(Cfg).SnapHeaderAlgCfgID);
  end;
end;}

function TsmxColumn.IsOwnerDrawHeader: Boolean;
begin
  if CellOwner is TsmxCustomGrid then
    Result := goOwnerDrawHeader in TsmxCustomGrid(CellOwner).GridOptions
  else
    Result := True;
end;

procedure TsmxColumn.ResetCellProps;
begin
  inherited ResetCellProps;
  ColumnAlignment := taLeftJustify;
  //ColumnCaption := TsmxGridColumnCfg(Cfg).ColumnText.Caption;
  ColumnColor := Graphics.clBlack;
  ColumnFont.Color := Graphics.clBlack;
  ColumnFont.Name := '';
  ColumnFont.Size := 0;
  ColumnFont.Style := [];
  ColumnOptions := [];
  //FieldName := '';
  HeaderAlignment := taLeftJustify;
  HeaderCaption := '';
  HeaderColor := Graphics.clBlack;
  HeaderFont.Color := Graphics.clBlack;
  HeaderFont.Name := '';
  HeaderFont.Size := 0;
  HeaderFont.Style := [];
  OnSnapHeader := nil;
end;

procedure TsmxColumn.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Column.Collection := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TDBGrid then
      Column.Collection := TDBGrid(Obj).Columns;
  end;
end;

procedure TsmxColumn.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxColumnCfg then
  begin
    ColumnAlignment := TsmxColumnCfg(Cfg).ColumnText.Alignment;
    //ColumnCaption := TsmxColumnCfg(Cfg).ColumnText.Caption;
    ColumnColor := TColor(TsmxColumnCfg(Cfg).ColumnText.Color);
    ColumnFont.Color := TColor(TsmxColumnCfg(Cfg).ColumnText.Font.Color);
    ColumnFont.Name := TsmxColumnCfg(Cfg).ColumnText.Font.Name;
    ColumnFont.Size := TsmxColumnCfg(Cfg).ColumnText.Font.Size;
    ColumnFont.Style := TsmxColumnCfg(Cfg).ColumnText.Font.Style;
    ColumnOptions := TsmxColumnCfg(Cfg).ColumnOptions;
    //FieldName := TsmxColumnCfg(Cfg).ColumnFieldName;
    HeaderAlignment := TsmxColumnCfg(Cfg).ColumnHeader.Alignment;
    HeaderCaption := TsmxColumnCfg(Cfg).ColumnHeader.Caption;
    HeaderColor := TColor(TsmxColumnCfg(Cfg).ColumnHeader.Color);
    HeaderFont.Color := TColor(TsmxColumnCfg(Cfg).ColumnHeader.Font.Color);
    HeaderFont.Name := TsmxColumnCfg(Cfg).ColumnHeader.Font.Name;
    HeaderFont.Size := TsmxColumnCfg(Cfg).ColumnHeader.Font.Size;
    HeaderFont.Style := TsmxColumnCfg(Cfg).ColumnHeader.Font.Style;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnSnapHeader := smxClassFuncs.GetEventForm(Form, TsmxColumnCfg(Cfg).SnapHeaderAlgCfgID);
  end;
end;

procedure TsmxColumn.SetColumnOptions(Value: TsmxColumnOptions);
begin
  inherited SetColumnOptions(Value);
  Column.ReadOnly := not (coEditing in Value);
end;

procedure TsmxColumn.SetSlaveIndex(Value: Integer);
begin
  inherited SetSlaveIndex(Value);
  Column.Index := Value;
end;

{procedure TsmxColumn.SetSlaveName(const Value: String);
begin
  inherited SetSlaveName(Value);
  Column.FieldName := Value;
end;}

procedure TsmxColumn.SetName(const NewName: TComponentName);
begin
  { TODO -oПоляков Александр : пока так. возможно, нужно создать отдельное проперти }
  inherited SetName(NewName);
  Column.FieldName := NewName;
end;

{ TsmxDBGrid }

destructor TsmxDBGrid.Destroy;
begin
  inherited Destroy;
  if Assigned(FDBGrid) then
  begin
    FDBGrid.DataSource.Free;
    FDBGrid.Free;
  end;
end;

{procedure TsmxDBGrid.DoApply;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnApply) then
  begin
    if Cfg is TsmxGridCfg then
      AlgCfgID := TsmxGridCfg(Cfg).ApplyAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnApply, AlgCfgID);
  end;
end;}

procedure TsmxDBGrid.DoChangeRow;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnChangeRow) then
  begin
    if Cfg is TsmxGridCfg then
      AlgCfgID := TsmxGridCfg(Cfg).ChangeRowAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnChangeRow, AlgCfgID);
  end;
end;

{procedure TsmxDBGrid.DoPrepare;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnPrepare) then
  begin
    if Cfg is TsmxGridCfg then
      AlgCfgID := TsmxGridCfg(Cfg).PrepareAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnPrepare, AlgCfgID);
  end;
end;}

procedure TsmxDBGrid.InternalPrepare;
var
  OldIsManualRefreshParams: Boolean;
begin
  inherited InternalPrepare;
  if Assigned(Request) then
  begin
    if Assigned(Request.DataSet) then
      Request.DataSet.Close;
    OldIsManualRefreshParams := Request.IsManualRefreshParams;
    try
      Request.IsManualRefreshParams := False;
      Request.Prepare;
    finally
      Request.IsManualRefreshParams := OldIsManualRefreshParams;
    end;
  end;
end;

{procedure TsmxDBGrid.DoRefresh;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnRefresh) then
  begin
    if Cfg is TsmxGridCfg then
      AlgCfgID := TsmxGridCfg(Cfg).RefreshAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefresh, AlgCfgID);
  end;
end;}

procedure TsmxDBGrid.InternalRefresh;
var
  OldIsManualRefreshParams: Boolean;
begin
  inherited InternalRefresh;
  if Assigned(Request) then
  begin
    OldIsManualRefreshParams := Request.IsManualRefreshParams;
    try
      Request.IsManualRefreshParams := False;
      Request.Execute;
    finally
      Request.IsManualRefreshParams := OldIsManualRefreshParams;
    end;
  end;
end;

procedure TsmxDBGrid.DrawHeaderDefault;
var
  i: Integer;
begin
  for i := 0 to SlaveCount - 1 do
  begin
    Slaves[i].HeaderColor := Graphics.clBtnFace;
    Slaves[i].HeaderFont := DBGrid.TitleFont;
  end;
end;

function TsmxDBGrid.GetFocusedColIndex: Integer;
begin
  Result := DBGrid.SelectedIndex;
end;

procedure TsmxDBGrid.SetFocusedColIndex(Value: Integer);
begin
  DBGrid.SelectedIndex := Value;
end;

function TsmxDBGrid.GetFocusedRowIndex: Integer;
begin
  {if Assigned(DBGrid.DataSource.DataSet) then
    Result := DBGrid.DataSource.DataSet.RecNo else
    Result := -1;}

  Result := -1;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
        Result := Request.DataSet.RecordNo;
end;

procedure TsmxDBGrid.SetFocusedRowIndex(Value: Integer);
begin
  {if Assigned(DBGrid.DataSource.DataSet) then
    DBGrid.DataSource.DataSet.RecNo := Value;}

  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
        Request.DataSet.RecordNo := Value;
end;

function TsmxDBGrid.GetGridCaption(ColIndex, RowIndex: Integer): String;
var
  //CurRecordNo: Integer;
  //RecordNo: Integer;
  //b: Boolean;
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  Result := '';
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        Bookmark := nil;
        if Assigned(DBGrid.DataSource.DataSet) then
        begin
          DBGrid.DataSource.DataSet.DisableControls;
          Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
        end;
        //b := False;
        //CurRecordNo := Request.DataSet.RecordNo;
        try
          Field := nil;
          if RowIndex <> -1 then
          begin
            if smxDBFuncs.SetNumberOfRecord(Request.DataSet, RowIndex) then
              Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
          end else
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
          if Assigned(Field) then
            Result := Variants.VarToStr(Field.Value);

          //Request.DataSet.GotoBookmark(Bookmark);
          if Assigned(DBGrid.DataSource.DataSet) then
            DBGrid.DataSource.DataSet.GotoBookmark(Bookmark);
        finally
          //Request.DataSet.RecordNo := CurRecordNo;

          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.FreeBookmark(Bookmark);
            DBGrid.DataSource.DataSet.EnableControls;
          end;
        end;
      end;
end;

procedure TsmxDBGrid.SetGridCaption(ColIndex, RowIndex: Integer; const Value: String);
var
  //OldRecordNo: Integer;
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //OldRecordNo := Request.DataSet.RecordNo;
        Bookmark := nil;
        if Assigned(DBGrid.DataSource.DataSet) then
        begin
          DBGrid.DataSource.DataSet.DisableControls;
          Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
        end;
        try
          //Request.DataSet.RecordNo := RowIndex;
          Field := nil;
          if RowIndex <> -1 then
          begin
            if smxDBFuncs.SetNumberOfRecord(Request.DataSet, RowIndex) then
              Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
          end else
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
          if Assigned(Field) then
          begin
            Request.DataSet.Edit;
            Field.Value := smxFuncs.StrToVar(Value);
            Request.DataSet.Post;
          end;
          if Assigned(DBGrid.DataSource.DataSet) then
            DBGrid.DataSource.DataSet.GotoBookmark(Bookmark);
        finally
          //Request.DataSet.RecordNo := OldRecordNo;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.FreeBookmark(Bookmark);
            DBGrid.DataSource.DataSet.EnableControls;
          end;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

function TsmxDBGrid.GetGridValue(ColIndex, RowIndex: Integer): Variant;
var
  //CurRecordNo: Integer;
  //RecordNo: Integer;
  //b: Boolean;
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  Result := Variants.Null;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        Bookmark := nil;
        if Assigned(DBGrid.DataSource.DataSet) then
        begin
          DBGrid.DataSource.DataSet.DisableControls;
          Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
        end;
        //b := False;
        //CurRecordNo := Request.DataSet.RecordNo;
        try
          Field := nil;
          if RowIndex <> -1 then
          begin
            if smxDBFuncs.SetNumberOfRecord(Request.DataSet, RowIndex) then
              Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
          end else
            Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
          if Assigned(Field) then
            Result := Field.Value;
          //Request.DataSet.GotoBookmark(Bookmark);
          if Assigned(DBGrid.DataSource.DataSet) then
            DBGrid.DataSource.DataSet.GotoBookmark(Bookmark);
        finally
          //Request.DataSet.RecordNo := CurRecordNo;

          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.FreeBookmark(Bookmark);
            DBGrid.DataSource.DataSet.EnableControls;
          end;
        end;
      end;
end;

procedure TsmxDBGrid.SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant);
var
  //OldRecordNo: Integer;
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //OldRecordNo := Request.DataSet.RecordNo;
        Bookmark := nil;
        if Assigned(DBGrid.DataSource.DataSet) then
        begin
          DBGrid.DataSource.DataSet.DisableControls;
          Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
        end;
        try
          //Request.DataSet.RecordNo := RowIndex;
          Field := nil;
          if RowIndex <> -1 then
          begin
            if smxDBFuncs.SetNumberOfRecord(Request.DataSet, RowIndex) then
              Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
          end else
            Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
          if Assigned(Field) then
          begin
            Request.DataSet.Edit;
            Field.Value := Value;
            Request.DataSet.Post;
          end;
          if Assigned(DBGrid.DataSource.DataSet) then
            DBGrid.DataSource.DataSet.GotoBookmark(Bookmark);
        finally
          //Request.DataSet.RecordNo := OldRecordNo;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.FreeBookmark(Bookmark);
            DBGrid.DataSource.DataSet.EnableControls;
          end;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

{procedure TsmxDBGrid.Initialize(const ACfgDatabase: IsmxDatabase; ACfgID: Integer;
  ASelectRequest: TsmxCustomRequest = nil);
var
  Cfg: TsmxBaseCfg;
  i: Integer;
  Cell: TsmxBaseCell;
begin
  inherited Initialize(ACfgDatabase, ACfgID, ASelectRequest);
  Cfg := smxClassFuncs.NewCfg(Self, ACfgDatabase, ACfgID, ASelectRequest);
  try
    if Cfg is TsmxDBGridCfg then
      with TsmxDBGridCfg(Cfg) do
      begin
        FGrid.Options := FGrid.Options - [dgEditing];
        if goColLines in Cfg.GridOptions then
          FGrid.Options := FGrid.Options + [dgColLines] else
          FGrid.Options := FGrid.Options - [dgColLines];
        if goRowLines in Cfg.GridOptions then
          FGrid.Options := FGrid.Options + [dgRowLines] else
          FGrid.Options := FGrid.Options - [dgRowLines];
        if goRowSelect in Cfg.GridOptions then
          FGrid.Options := FGrid.Options + [dgRowSelect] else
          FGrid.Options := FGrid.Options - [dgRowSelect];

        ClearColumns;
        ColumnList.Count := GridColumns.Count;
        for i := 0 to GridColumns.Count - 1 do
          if GridColumns[i].CfgID > 0 then
          begin
            Cell := smxClassFuncs.NewCell(Self, ACfgDatabase, GridColumns[i].CfgID, ASelectRequest);
            if Cell is TsmxCustomColumn then
            begin
              Cell.Initialize(ACfgDatabase, GridColumns[i].CfgID, ASelectRequest);
              Columns[i] := Cell;
            end else
            begin
              Cell.Free;
              raise EsmxCellError.CreateResFmt(@SCellBuildError, [ACfgID]);
            end;
          end else
            raise EsmxCellError.CreateResFmt(@SCellBuildError, [ACfgID]);
      end
    else
      raise EsmxCellError.CreateResFmt(@SCellBuildError, [ACfgID]);
  finally
    Cfg.Free;
  end;
end;}

{function TsmxDBGrid.GetCellAlign: TAlign;
begin
  Result := Grid.Align;
end;

procedure TsmxDBGrid.SetCellAlign(Value: TAlign);
begin
  Grid.Align := Value;
end;

function TsmxDBGrid.GetCellAnchors: TAnchors;
begin
  Result := Grid.Anchors;
end;

procedure TsmxDBGrid.SetCellAnchors(Value: TAnchors);
begin
  Grid.Anchors := Value;
end;

function TsmxDBGrid.GetCellCursor: TCursor;
begin
  Result := Grid.Cursor;
end;

procedure TsmxDBGrid.SetCellCursor(Value: TCursor);
begin
  Grid.Cursor := Value;
end;

function TsmxDBGrid.GetCellEnabled: Boolean;
begin
  Result := Grid.Enabled;
end;

procedure TsmxDBGrid.SetCellEnabled(Value: Boolean);
begin
  Grid.Enabled := Value;
end;

function TsmxDBGrid.GetCellHeight: Integer;
begin
  Result := Grid.Height;
end;

procedure TsmxDBGrid.SetCellHeight(Value: Integer);
begin
  Grid.Height := Value;
end;

function TsmxDBGrid.GetCellLeft: Integer;
begin
  Result := Grid.Left;
end;

procedure TsmxDBGrid.SetCellLeft(Value: Integer);
begin
  Grid.Left := Value;
end;

function TsmxDBGrid.GetCellTop: Integer;
begin
  Result := Grid.Top;
end;

procedure TsmxDBGrid.SetCellTop(Value: Integer);
begin
  Grid.Top := Value;
end;

function TsmxDBGrid.GetCellVisible: Boolean;
begin
  Result := Grid.Visible;
end;

procedure TsmxDBGrid.SetCellVisible(Value: Boolean);
begin
  Grid.Visible := Value;
end;

function TsmxDBGrid.GetCellWidth: Integer;
begin
  Result := Grid.Width;
end;

procedure TsmxDBGrid.SetCellWidth(Value: Integer);
begin
  Grid.Width := Value;
end;}

function TsmxDBGrid.GetCellHint: String;
begin
  Result := DBGrid.Hint;
end;

procedure TsmxDBGrid.SetCellHint(const Value: String);
begin
  DBGrid.Hint := Value;
end;

function TsmxDBGrid.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxGridCfg;
end;

function TsmxDBGrid.GetDBGrid: TsmxWheelDBGrid;
begin
  if not Assigned(FDBGrid) then
  begin
    FDBGrid := TsmxWheelDBGrid.Create(nil);
    //FDBGrid.Options := FDBGrid.Options - [dgEditing];
    FDBGrid.OnDblClick := ControlDblClick;
    FDBGrid.OnTitleClick := DBGridTitleClick;
    FDBGrid.OnCellClick := DBGridCellClick;
    FDBGrid.DataSource := TDataSource.Create(nil);
    FDBGrid.DataSource.OnDataChange := DBGridDataChange;
  end;
  Result := FDBGrid;
end;

function TsmxDBGrid.GetInternalObject: TObject;
begin
  Result := DBGrid;
end;

function TsmxDBGrid.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxColumn;
end;

procedure TsmxDBGrid.DBGridCellClick(Column: TColumn);
var
  Slave: TsmxOwnerCell;
begin
  Slave := FindSlaveByInternalObject(Column);
  if Slave is TsmxCustomColumn then
    TsmxCustomColumn(Slave).Snap;
end;

procedure TsmxDBGrid.DBGridDataChange(Sender: TObject; Field: TField);
begin
  ChangeRow;
end;

{procedure TsmxDBGrid.DBGridDblClick(Sender: TObject);
begin
  DoubleSnap;
end;}

procedure TsmxDBGrid.DBGridTitleClick(Column: TColumn);
var
  Slave: TsmxOwnerCell;
begin
  Slave := FindSlaveByInternalObject(Column);
  if Slave is TsmxCustomColumn then
    TsmxCustomColumn(Slave).SnapHeader;
end;

{procedure TsmxDBGrid.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxGridCfg then
  begin
    GridOptions := TsmxGridCfg(Cfg).GridOptions;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxGridCfg(Cfg).RequestCfgID);
      OnChangeRow := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).ChangeRowAlgCfgID);
      //OnPressDouble := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PressDoubleAlgCfgID);
      //OnPressHeader := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PressHeaderAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PrepareAlgCfgID);
      OnRefresh := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).RefreshAlgCfgID);
    end;
  end;
end;}

procedure TsmxDBGrid.ResetCellProps;
begin
  inherited ResetCellProps;
  GridOptions := [];
  //OnApply := nil;
  OnChangeRow := nil;
  //OnPrepare := nil;
  //OnRefresh := nil;
  Request := nil;
end;

procedure TsmxDBGrid.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    DBGrid.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      DBGrid.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxDBGrid.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxGridCfg then
  begin
    GridOptions := TsmxGridCfg(Cfg).GridOptions;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxGridCfg(Cfg).RequestCfgID);
      //OnApply := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).ApplyAlgCfgID);
      OnChangeRow := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).ChangeRowAlgCfgID);
      //OnPressDouble := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PressDoubleAlgCfgID);
      //OnPressHeader := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PressHeaderAlgCfgID);
      //OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PrepareAlgCfgID);
      //OnRefresh := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).RefreshAlgCfgID);
    end;
  end;
end;

procedure TsmxDBGrid.SetGridOptions(Value: TsmxGridOptions);
begin
  inherited SetGridOptions(Value);
  if goColLines in Value then
    DBGrid.Options := DBGrid.Options + [dgColLines] else
    DBGrid.Options := DBGrid.Options - [dgColLines];
  if goRowLines in Value then
    DBGrid.Options := DBGrid.Options + [dgRowLines] else
    DBGrid.Options := DBGrid.Options - [dgRowLines];
  if goRowSelect in Value then
    DBGrid.Options := DBGrid.Options + [dgRowSelect] else
    DBGrid.Options := DBGrid.Options - [dgRowSelect];
  if goShowHeader in Value then
    DBGrid.Options := DBGrid.Options + [dgTitles, dgIndicator] else
    DBGrid.Options := DBGrid.Options - [dgTitles, dgIndicator];
  if goEditing in Value then
    DBGrid.Options := DBGrid.Options + [dgEditing] else
    DBGrid.Options := DBGrid.Options - [dgEditing];  
  if not (goOwnerDrawHeader in Value) then
    DrawHeaderDefault;
end;

procedure TsmxDBGrid.SetRequest(Value: TsmxCustomRequest);
var
  Obj: TObject;
begin
  if Assigned(Request) then
    DBGrid.DataSource.DataSet := nil;
  inherited SetRequest(Value);
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
    begin
      Obj := TObject(Request.DataSet.GetInternalRef);
      if Obj is TDataSet then
        DBGrid.DataSource.DataSet := TDataSet(Obj);
    end;
end;

{ TsmxFilter }

{procedure TsmxFilter.CreateChilds;
var c: TsmxBaseCell;
begin
  with Cfg.Algorithm do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomAlgorithm then
        Algorithm := TsmxCustomAlgorithm(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
end;

function TsmxFilter.GetCfg: TsmxFilterCfg;
begin
  Result := TsmxFilterCfg(inherited Cfg);
end;

procedure TsmxFilter.InitChilds;
begin
  if Assigned(Algorithm) then
    with Cfg.Algorithm do
    begin
      Algorithm.CellCaption := Caption;
      Algorithm.CellEnabled := Enabled;
      Algorithm.CellHotKey := HotKey;
      Algorithm.CellVisible := Visible;
    end;
end;}

{ TsmxPanelFilter }

{constructor TsmxPanelFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FilterName := Cfg.FilterName;
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
end;}

destructor TsmxPanelFilter.Destroy;
begin
  inherited Destroy;
  //UnBindFilterObjects;
  //if GetFilter is TControl then
    //TControl(GetFilter).Parent := nil;
  if Assigned(FHeader) then
    FHeader.Free;
  if Assigned(FPanel) then
    FPanel.Free;
end;

procedure TsmxPanelFilter.DoChangeFilter;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnChangeFilter) then
  begin
    if Cfg is TsmxFilterCfg then
      AlgCfgID := TsmxFilterCfg(Cfg).ChangeFilterAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnChangeFilter, AlgCfgID);
  end;
end;

{function TsmxPanelFilter.GetCellAlign: TAlign;
begin
  Result := Panel.Align;
end;

procedure TsmxPanelFilter.SetCellAlign(Value: TAlign);
begin
  Panel.Align := Value;
end;

function TsmxPanelFilter.GetCellAnchors: TAnchors;
begin
  Result := Panel.Anchors;
end;

procedure TsmxPanelFilter.SetCellAnchors(Value: TAnchors);
begin
  Panel.Anchors := Value;
end;

function TsmxPanelFilter.GetCellCursor: TCursor;
begin
  Result := Panel.Cursor;
end;

procedure TsmxPanelFilter.SetCellCursor(Value: TCursor);
begin
  Panel.Cursor := Value;
end;

function TsmxPanelFilter.GetCellEnabled: Boolean;
begin
  Result := Panel.Enabled;
end;

procedure TsmxPanelFilter.SetCellEnabled(Value: Boolean);
begin
  Panel.Enabled := Value;
end;

function TsmxPanelFilter.GetCellHeight: Integer;
begin
  Result := Panel.Height;
end;

procedure TsmxPanelFilter.SetCellHeight(Value: Integer);
begin
  Panel.Height := Value;
end;

function TsmxPanelFilter.GetCellLeft: Integer;
begin
  Result := Panel.Left;
end;

procedure TsmxPanelFilter.SetCellLeft(Value: Integer);
begin
  Panel.Left := Value;
end;

function TsmxPanelFilter.GetCellTop: Integer;
begin
  Result := Panel.Top;
end;

procedure TsmxPanelFilter.SetCellTop(Value: Integer);
begin
  Panel.Top := Value;
end;

function TsmxPanelFilter.GetCellVisible: Boolean;
begin
  Result := Panel.Visible;
end;

procedure TsmxPanelFilter.SetCellVisible(Value: Boolean);
begin
  Panel.Visible := Value;
end;

function TsmxPanelFilter.GetCellWidth: Integer;
begin
  Result := Panel.Width;
end;

procedure TsmxPanelFilter.SetCellWidth(Value: Integer);
begin
  Panel.Width := Value;
end;}

function TsmxPanelFilter.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxFilterCfg;
end;

{function TsmxPanelFilter.GetFilter: TObject;
begin
  Result := nil;
end;}

function TsmxPanelFilter.GetHeader: TLabel;
begin
  if not Assigned(FHeader) then
  begin
    FHeader := TLabel.Create(nil);
    FHeader.Parent := Panel;
    FHeader.Left := 4;
    FHeader.Top := 4;
    FHeader.Width := Panel.Width - 8;
    FHeader.Anchors := [akLeft, akRight];
  end;
  Result := FHeader;
end;

function TsmxPanelFilter.GetHeaderAlignment: TAlignment;
begin
  Result := Header.Alignment;
end;

procedure TsmxPanelFilter.SetHeaderAlignment(Value: TAlignment);
begin
  Header.Alignment := Value;
end;

function TsmxPanelFilter.GetHeaderCaption: String;
begin
  Result := Header.Caption;
end;

procedure TsmxPanelFilter.SetHeaderCaption(const Value: String);
begin
  Header.Caption := Value;
end;

function TsmxPanelFilter.GetHeaderColor: TColor;
begin
  Result := Header.Color;
end;

procedure TsmxPanelFilter.SetHeaderColor(Value: TColor);
begin
  Header.Color := Value;
end;

function TsmxPanelFilter.GetHeaderFont: TFont;
begin
  Result := Header.Font;
end;

procedure TsmxPanelFilter.SetHeaderFont(Value: TFont);
begin
  Header.Font := Value;
end;

function TsmxPanelFilter.GetInternalObject: TObject;
begin
  Result := Panel;
end;

function TsmxPanelFilter.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanel.Create(nil);
    FPanel.Caption := '';
    FPanel.Height := 49;
    FPanel.BevelOuter := bvNone;
  end;
  Result := FPanel;
end;

function TsmxDBGrid.GetRowCount: Integer;
begin
  Result := 0;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      Result := Request.DataSet.RecordCount;
end;

{procedure TsmxPanelFilter.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxFilterCfg then
  begin
    DisplayFormat := TsmxFilterCfg(Cfg).DisplayFormat;
    FilterAlignment := TsmxFilterCfg(Cfg).FilterText.Alignment;
    FilterCaption := TsmxFilterCfg(Cfg).FilterText.Caption;
    FilterColor := TColor(TsmxFilterCfg(Cfg).FilterText.Color);
    FilterFont.Color := TColor(TsmxFilterCfg(Cfg).FilterText.Font.Color);
    FilterFont.Name := TsmxFilterCfg(Cfg).FilterText.Font.Name;
    FilterFont.Size := TsmxFilterCfg(Cfg).FilterText.Font.Size;
    FilterFont.Style := TsmxFilterCfg(Cfg).FilterText.Font.Style;
    FilterOptions := TsmxFilterCfg(Cfg).FilterOptions;
    FilterValue := TsmxFilterCfg(Cfg).FilterValue;
    HeaderAlignment := TsmxFilterCfg(Cfg).FilterHeader.Alignment;
    HeaderCaption := TsmxFilterCfg(Cfg).FilterHeader.Caption;
    HeaderColor := TColor(TsmxFilterCfg(Cfg).FilterHeader.Color);
    HeaderFont.Color := TColor(TsmxFilterCfg(Cfg).FilterHeader.Font.Color);
    HeaderFont.Name := TsmxFilterCfg(Cfg).FilterHeader.Font.Name;
    HeaderFont.Size := TsmxFilterCfg(Cfg).FilterHeader.Font.Size;
    HeaderFont.Style := TsmxFilterCfg(Cfg).FilterHeader.Font.Style;
    ValueFormat := TsmxFilterCfg(Cfg).ValueFormat;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      //Request := smxClassFuncs.GetRequestForm(Form, TsmxFilterCfg(Cfg).RequestCfgID);
      OnChangeFilter := smxClassFuncs.GetEventForm(Form, TsmxFilterCfg(Cfg).ChangeFilterAlgCfgID);
    end;
  end;
end;}

procedure TsmxPanelFilter.ResetCellProps;
begin
  inherited ResetCellProps;
  DisplayFormat := '';
  FilterAlignment := taLeftJustify;
  FilterCaption := '';
  FilterColor := Graphics.clBlack;
  FilterFont.Color := Graphics.clBlack;
  FilterFont.Name := '';
  FilterFont.Size := 0;
  FilterFont.Style := [];
  FilterOptions := [];
  FilterValue := Variants.Null;
  HeaderAlignment := taLeftJustify;
  HeaderCaption := '';
  HeaderColor := Graphics.clBlack;
  HeaderFont.Color := Graphics.clBlack;
  HeaderFont.Name := '';
  HeaderFont.Size := 0;
  HeaderFont.Style := [];
  //Request := nil;
  ValueFormat := '';
  OnChangeFilter := nil;
end;

procedure TsmxPanelFilter.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxPanelFilter.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxFilterCfg then
  begin
    DisplayFormat := TsmxFilterCfg(Cfg).DisplayFormat;
    FilterAlignment := TsmxFilterCfg(Cfg).FilterText.Alignment;
    FilterCaption := TsmxFilterCfg(Cfg).FilterText.Caption;
    FilterColor := TColor(TsmxFilterCfg(Cfg).FilterText.Color);
    FilterFont.Color := TColor(TsmxFilterCfg(Cfg).FilterText.Font.Color);
    FilterFont.Name := TsmxFilterCfg(Cfg).FilterText.Font.Name;
    FilterFont.Size := TsmxFilterCfg(Cfg).FilterText.Font.Size;
    FilterFont.Style := TsmxFilterCfg(Cfg).FilterText.Font.Style;
    FilterOptions := TsmxFilterCfg(Cfg).FilterOptions;
    FilterValue := TsmxFilterCfg(Cfg).FilterValue;
    HeaderAlignment := TsmxFilterCfg(Cfg).FilterHeader.Alignment;
    HeaderCaption := TsmxFilterCfg(Cfg).FilterHeader.Caption;
    HeaderColor := TColor(TsmxFilterCfg(Cfg).FilterHeader.Color);
    HeaderFont.Color := TColor(TsmxFilterCfg(Cfg).FilterHeader.Font.Color);
    HeaderFont.Name := TsmxFilterCfg(Cfg).FilterHeader.Font.Name;
    HeaderFont.Size := TsmxFilterCfg(Cfg).FilterHeader.Font.Size;
    HeaderFont.Style := TsmxFilterCfg(Cfg).FilterHeader.Font.Style;
    ValueFormat := TsmxFilterCfg(Cfg).ValueFormat;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      //Request := smxClassFuncs.GetRequestForm(Form, TsmxFilterCfg(Cfg).RequestCfgID);
      OnChangeFilter := smxClassFuncs.GetEventForm(Form, TsmxFilterCfg(Cfg).ChangeFilterAlgCfgID);
    end;
  end;
end;

{procedure TsmxPanelFilter.UnBindFilterObjects;
begin
  if Assigned(FHeader) then
    FHeader.Parent := nil;
end;}

{ TsmxFilterDesk }

{procedure TsmxFilterDesk.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  with Cfg.ApplyRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        ApplyRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.PrepareRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        PrepareRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  FilterList.Count := Cfg.Filters.Count;
  for i := 0 to Cfg.Filters.Count - 1 do
    with Cfg.Filters[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomFilter then
          FilterList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxFilterDesk.GetCfg: TsmxFilterDeskCfg;
begin
  Result := TsmxFilterDeskCfg(inherited Cfg);
end;

procedure TsmxFilterDesk.InitChilds;
var i: Integer;
begin
  if Assigned(ApplyRequest) then
    with Cfg.ApplyRequest do
    begin
      ApplyRequest.DatabaseName := DatabaseName;
      ApplyRequest.OperationMode := Operation;
    end;
  if Assigned(PrepareRequest) then
    with Cfg.PrepareRequest do
    begin
      PrepareRequest.DatabaseName := DatabaseName;
      PrepareRequest.OperationMode := Operation;
    end;
  for i := 0 to Cfg.Filters.Count - 1 do
    with Cfg.Filters[i] do
    begin
      Filters[i].CellAlign := UnitAlign;
      Filters[i].CellEnabled := UnitEnabled;
      Filters[i].CellHeight := UnitHeight;
      Filters[i].CellLeft := UnitLeft;
      Filters[i].CellTop := UnitTop;
      Filters[i].CellVisible := UnitVisible;
      Filters[i].CellWidth := UnitWidth;
    end;
end;}

{ TsmxPanelFilterDesk }

constructor TsmxPanelFilterDesk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

destructor TsmxPanelFilterDesk.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FPanel) then
    FPanel.Free;
end;

{procedure TsmxPanelFilterDesk.DoApply;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnApply) then
  begin
    if Cfg is TsmxFilterDeskCfg then
      AlgCfgID := TsmxFilterDeskCfg(Cfg).ApplyAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnApply, AlgCfgID);
  end;
end;}

procedure TsmxPanelFilterDesk.InternalApply;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalApply;
  if Assigned(Request) then
  begin
    DataSet := Request.UpdateDataSet; // ModifyRequests[mrUpdate];
    if Assigned(DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        for i := 0 to SlaveCount - 1 do
        begin
          if foApply in Slaves[i].FilterOptions then
          begin
            //if foSetValue in Slaves[i].FilterOptions then
              DataSet.ParamByName(Slaves[i].Name).Value :=
                Slaves[i].FilterValue;
            //if foSetCaption in Slaves[i].FilterOptions then
              DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value :=
                smxFuncs.StrToVar(Slaves[i].FilterCaption);
          end;

          {if foApplyValue in Slaves[i].FilterOptions then
            DataSet.ParamByName(Slaves[i].SlaveName).Value :=
              Slaves[i].FilterValue;
          if foApplyText in Slaves[i].FilterOptions then
            DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value :=
              smxFuncs.StrToVar(Slaves[i].FilterCaption);}
        end;
        Request.Update;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

{procedure TsmxPanelFilterDesk.DoPrepare;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnPrepare) then
  begin
    if Cfg is TsmxFilterDeskCfg then
      AlgCfgID := TsmxFilterDeskCfg(Cfg).PrepareAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnPrepare, AlgCfgID);
  end;
end;}

procedure TsmxPanelFilterDesk.InternalPrepare;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalPrepare;
  if Assigned(Request) then
  begin
    DataSet := Request.DataSet;
    if Assigned(DataSet) then
    begin
      DataSet.Close;
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        Request.Prepare;
        if DataSet.Active then
          for i := 0 to SlaveCount - 1 do
          begin
            if foPrepare in Slaves[i].FilterOptions then
              case DataSet.PerformanceMode of
                pmOpen:
                begin
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.FieldByName(Slaves[i].Name).Value;
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                  {if foPrepareValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.FieldByName(Slaves[i].SlaveName).Value;
                  if foPrepareText in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
                end;
                pmExecute:
                begin
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.ParamByName(Slaves[i].Name).Value;
                  //if foSetValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                  {if foPrepareValue in Slaves[i].FilterOptions then
                    Slaves[i].FilterValue :=
                      DataSet.ParamByName(Slaves[i].SlaveName).Value;
                  if foPrepareText in Slaves[i].FilterOptions then
                    Slaves[i].FilterCaption :=
                      Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
                end;
              end;
          end;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

{procedure TsmxPanelFilterDesk.DoRefresh;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnRefresh) then
  begin
    if Cfg is TsmxFilterDeskCfg then
      AlgCfgID := TsmxFilterDeskCfg(Cfg).RefreshAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnRefresh, AlgCfgID);
  end;
end;}

procedure TsmxPanelFilterDesk.InternalRefresh;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalPrepare;
  if Assigned(Request) then
  begin
    DataSet := Request.DataSet;
    if Assigned(DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        Request.Execute;
        for i := 0 to SlaveCount - 1 do
        begin
          if foPrepare in Slaves[i].FilterOptions then
            case DataSet.PerformanceMode of
              pmOpen:
              begin
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.FieldByName(Slaves[i].Name).Value;
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                {if foPrepareValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.FieldByName(Slaves[i].SlaveName).Value;
                if foPrepareText in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
              end;
              pmExecute:
              begin
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.ParamByName(Slaves[i].Name).Value;
                //if foSetValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                {if foPrepareValue in Slaves[i].FilterOptions then
                  Slaves[i].FilterValue :=
                    DataSet.ParamByName(Slaves[i].SlaveName).Value;
                if foPrepareText in Slaves[i].FilterOptions then
                  Slaves[i].FilterCaption :=
                    Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);}
              end;
            end;
          {case Request.PerformanceMode of
            pmOpen:
            begin
              if foPrepareValue in Slaves[i].FilterOptions then
                Slaves[i].FilterValue :=
                  DataSet.FieldByName(Slaves[i].SlaveName).Value;
              if foPrepareText in Slaves[i].FilterOptions then
                Slaves[i].FilterCaption :=
                  Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);
            end;
            pmExecute:
            begin
              if foPrepareValue in Slaves[i].FilterOptions then
                Slaves[i].FilterValue :=
                  DataSet.ParamByName(Slaves[i].SlaveName).Value;
              if foPrepareText in Slaves[i].FilterOptions then
                Slaves[i].FilterCaption :=
                  Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].SlaveName)).Value);
            end;
          end;}
        end;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

{function TsmxPanelFilterDesk.GetCellAlign: TAlign;
begin
  Result := Panel.Align;
end;

procedure TsmxPanelFilterDesk.SetCellAlign(Value: TAlign);
begin
  Panel.Align := Value;
end;

function TsmxPanelFilterDesk.GetCellAnchors: TAnchors;
begin
  Result := Panel.Anchors;
end;

procedure TsmxPanelFilterDesk.SetCellAnchors(Value: TAnchors);
begin
  Panel.Anchors := Value;
end;

function TsmxPanelFilterDesk.GetCellCursor: TCursor;
begin
  Result := Panel.Cursor;
end;

procedure TsmxPanelFilterDesk.SetCellCursor(Value: TCursor);
begin
  Panel.Cursor := Value;
end;

function TsmxPanelFilterDesk.GetCellEnabled: Boolean;
begin
  Result := Panel.Enabled;
end;

procedure TsmxPanelFilterDesk.SetCellEnabled(Value: Boolean);
begin
  Panel.Enabled := Value;
end;

function TsmxPanelFilterDesk.GetCellHeight: Integer;
begin
  Result := Panel.Height;
end;

procedure TsmxPanelFilterDesk.SetCellHeight(Value: Integer);
begin
  Panel.Height := Value;
end;

function TsmxPanelFilterDesk.GetCellLeft: Integer;
begin
  Result := Panel.Left;
end;

procedure TsmxPanelFilterDesk.SetCellLeft(Value: Integer);
begin
  Panel.Left := Value;
end;

function TsmxPanelFilterDesk.GetCellTop: Integer;
begin
  Result := Panel.Top;
end;

procedure TsmxPanelFilterDesk.SetCellTop(Value: Integer);
begin
  Panel.Top := Value;
end;

function TsmxPanelFilterDesk.GetCellVisible: Boolean;
begin
  Result := Panel.Visible;
end;

procedure TsmxPanelFilterDesk.SetCellVisible(Value: Boolean);
begin
  Panel.Visible := Value;
end;

function TsmxPanelFilterDesk.GetCellWidth: Integer;
begin
  Result := Panel.Width;
end;

procedure TsmxPanelFilterDesk.SetCellWidth(Value: Integer);
begin
  Panel.Width := Value;
end;}

function TsmxPanelFilterDesk.GetCellCaption: String;
begin
  Result := Panel.Caption;
end;

procedure TsmxPanelFilterDesk.SetCellCaption(const Value: String);
begin
  Panel.Caption := Value;
end;

function TsmxPanelFilterDesk.GetCellHint: String;
begin
  Result := Panel.Hint;
end;

procedure TsmxPanelFilterDesk.SetCellHint(const Value: String);
begin
  Panel.Hint := Value;
end;

function TsmxPanelFilterDesk.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxFilterDeskCfg;
end;

function TsmxPanelFilterDesk.GetInternalObject: TObject;
begin
  Result := Panel;
end;

function TsmxPanelFilterDesk.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanel.Create(nil);
    FPanel.Caption := '';
    FPanel.BevelOuter := bvNone;
  end;
  Result := FPanel;
end;

{function TsmxPanelFilterDesk.GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass;
begin
  if Cfg is TsmxFilterDeskCfg then
    Result := TsmxOwnerCellClass(smxClassFuncs.CfgIDToCellClass(
      TsmxFilterDeskCfg(Cfg).SlaveCells[Index].CfgID, SelectRequest))
  else
    Result := inherited GetAltSlaveClass(Index);
end;}

function TsmxPanelFilterDesk.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxPanelFilter;
end;

{procedure TsmxPanelFilterDesk.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxFilterDeskCfg then
  begin
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxFilterDeskCfg(Cfg).RequestCfgID);
      OnApply := smxClassFuncs.GetEventForm(Form, TsmxFilterDeskCfg(Cfg).ApplyAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxFilterDeskCfg(Cfg).PrepareAlgCfgID);
    end;
  end;
end;}

procedure TsmxPanelFilterDesk.RefreshValueParams(DataSet: IsmxDataSet);
var
  i, j: Integer;
  List: TList;
  Value: Variant;
begin
  List := TList.Create;
  try
    for i := 0 to DataSet.ParamCount - 1 do
      case DataSet.Params[i].ParamLocation of
        plStorageParam:
        begin
          if Assigned(smxClassProcs.gStorageManagerIntf) then
            DataSet.Params[i].Value := smxClassProcs.gStorageManagerIntf.Values[DataSet.Params[i].ParamName];
        end;
        plParentParam:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(DataSet.Params[i].ParamName, Value) then
            begin
              DataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        else
          DataSet.Params[i].Value := Variants.Null;
      end;
  finally
    List.Free;
  end;
end;

procedure TsmxPanelFilterDesk.ResetCellProps;
begin
  inherited ResetCellProps;
  //OnApply := nil;
  //OnPrepare := nil;
  //OnRefresh := nil;
  Request := nil;
end;

procedure TsmxPanelFilterDesk.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxPanelFilterDesk.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxFilterDeskCfg then
  begin
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxFilterDeskCfg(Cfg).RequestCfgID);
      //OnApply := smxClassFuncs.GetEventForm(Form, TsmxFilterDeskCfg(Cfg).ApplyAlgCfgID);
      //OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxFilterDeskCfg(Cfg).PrepareAlgCfgID);
      //OnRefresh := smxClassFuncs.GetEventForm(Form, TsmxFilterDeskCfg(Cfg).RefreshAlgCfgID);
    end;
  end;
end;

procedure TsmxPanelFilterDesk.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxCustomFilter) and (Item is TsmxFilterKitItem) then
  begin
    TsmxCustomFilter(Slave).DisplayFormat := TsmxFilterKitItem(Item).DisplayFormat;
    TsmxCustomFilter(Slave).FilterOptions := TsmxFilterKitItem(Item).FilterOptions;
    TsmxCustomFilter(Slave).ValueFormat := TsmxFilterKitItem(Item).ValueFormat;
  end;
end;

{ TsmxSection }

{procedure TsmxSection.CreateChilds;
var c: TsmxBaseCell;
begin
  with Cfg.Request do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        Request := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.Grid do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomGrid then
        Grid := TsmxCustomGrid(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.FilterPanel do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomFilterDesk then
        FilterDesk := TsmxCustomFilterDesk(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
end;

function TsmxSection.GetCfg: TsmxSectionCfg;
begin
  Result := TsmxSectionCfg(inherited Cfg);
end;

procedure TsmxSection.InitChilds;
begin
  if Assigned(Request) then
    with Cfg.Request do
    begin
      Request.DatabaseName := DatabaseName;
      Request.OperationMode := Operation;
    end;
  if Assigned(Grid) then
    with Cfg.Grid do
    begin
      Grid.CellAlign := Align;
      Grid.CellEnabled := Enabled;
      Grid.CellVisible := Visible;
      with PositionSize do
      begin
        Grid.CellHeight := Height;
        Grid.CellLeft := Left;
        Grid.CellTop := Top;
        Grid.CellWidth := Width;
      end;
    end;
  if Assigned(FilterDesk) then
    with Cfg.FilterPanel do
    begin
      FilterDesk.CellAlign := Align;
      FilterDesk.CellEnabled := Enabled;
      FilterDesk.CellVisible := Visible;
      with PositionSize do
      begin
        FilterDesk.CellHeight := Height;
        FilterDesk.CellLeft := Left;
        FilterDesk.CellTop := Top;
        FilterDesk.CellWidth := Width;
      end;
    end;
end;}

{ TsmxPanelSection }

constructor TsmxPanelSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

destructor TsmxPanelSection.Destroy;
begin
  //UnInstallParent;
  //UnInitialize;
  inherited Destroy;
  if Assigned(FPanel) then
    FPanel.Free;
end;

{function TsmxPanelSection.GetCellAlign: TAlign;
begin
  Result := Panel.Align;
end;

procedure TsmxPanelSection.SetCellAlign(Value: TAlign);
begin
  Panel.Align := Value;
end;

function TsmxPanelSection.GetCellAnchors: TAnchors;
begin
  Result := Panel.Anchors;
end;

procedure TsmxPanelSection.SetCellAnchors(Value: TAnchors);
begin
  Panel.Anchors := Value;
end;

function TsmxPanelSection.GetCellCursor: TCursor;
begin
  Result := Panel.Cursor;
end;

procedure TsmxPanelSection.SetCellCursor(Value: TCursor);
begin
  Panel.Cursor := Value;
end;

function TsmxPanelSection.GetCellEnabled: Boolean;
begin
  Result := Panel.Enabled;
end;

procedure TsmxPanelSection.SetCellEnabled(Value: Boolean);
begin
  Panel.Enabled := Value;
end;

function TsmxPanelSection.GetCellHeight: Integer;
begin
  Result := Panel.Height;
end;

procedure TsmxPanelSection.SetCellHeight(Value: Integer);
begin
  Panel.Height := Value;
end;

function TsmxPanelSection.GetCellLeft: Integer;
begin
  Result := Panel.Left;
end;

procedure TsmxPanelSection.SetCellLeft(Value: Integer);
begin
  Panel.Left := Value;
end;

function TsmxPanelSection.GetCellTop: Integer;
begin
  Result := Panel.Top;
end;

procedure TsmxPanelSection.SetCellTop(Value: Integer);
begin
  Panel.Top := Value;
end;

function TsmxPanelSection.GetCellVisible: Boolean;
begin
  Result := Panel.Visible;
end;

procedure TsmxPanelSection.SetCellVisible(Value: Boolean);
begin
  Panel.Visible := Value;
end;

function TsmxPanelSection.GetCellWidth: Integer;
begin
  Result := Panel.Width;
end;

procedure TsmxPanelSection.SetCellWidth(Value: Integer);
begin
  Panel.Width := Value;
end;}

function TsmxPanelSection.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxSectionCfg;
end;

function TsmxPanelSection.GetInternalObject: TObject;
begin
  Result := Panel;
end;

function TsmxPanelSection.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanel.Create(nil);
    FPanel.Caption := '';
    FPanel.BevelOuter := bvNone;
  end;
  Result := FPanel;
end;

{procedure TsmxPanelSection.InternalInitialize;

  function CreateControl(AItem: TsmxControlKitItem; AForm: TsmxCustomForm): TsmxControlCell;
  begin
    Result := nil;
    if AItem.CfgID > 0 then
    begin
      Result := TsmxControlCell(smxClassFuncs.NewCell(Self, AItem.CfgID, SelectRequest));
      Result.CellParent := Self;
      Result.Initialize;
      Result.CellActive := AItem.ItemActive;
      Result.CellAlign := AItem.ItemAlign;
      Result.CellAnchors := AItem.ItemAnchors;
      Result.CellCursor := AItem.ItemCursor;
      Result.CellEnabled := AItem.ItemEnabled;
      Result.CellHeight := AItem.ItemHeight;
      Result.CellLeft := AItem.ItemLeft;
      Result.CellTop := AItem.ItemTop;
      Result.CellVisible := AItem.ItemVisible;
      Result.CellWidth := AItem.ItemWidth;
      if Assigned(AForm) then
        Result.PopupMenu := smxClassFuncs.GetPopupMenuForm(AForm, AItem.PopupMenuCfgID);
    end;
  end;

var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxRequestSectionCfg then
  begin
    Form := smxClassFuncs.GetAccessoryForm(Self);
    FilterDesk := TsmxCustomFilterDesk(CreateControl(TsmxRequestSectionCfg(Cfg).FilterDesk, Form));
    Grid := TsmxCustomGrid(CreateControl(TsmxRequestSectionCfg(Cfg).Grid, Form));
  end;
end;

procedure TsmxPanelSection.ResetCellProps;
begin
  inherited ResetCellProps;
  FilterDesk := nil;
  Grid := nil;
end;}

procedure TsmxPanelSection.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

{procedure TsmxPanelSection.SetFilterDesk(Value: TsmxCustomFilterDesk);
begin
  inherited SetFilterDesk(Value);
  if Assigned(FilterDesk) then
    FilterDesk.CellParent := Self;
end;}

{ TsmxPage }

{procedure TsmxPage.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  SectionList.Count := Cfg.Sections.Count;
  for i := 0 to Cfg.Sections.Count - 1 do
    with Cfg.Sections[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomSection then
          SectionList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxPage.GetCfg: TsmxPageCfg;
begin
  Result := TsmxPageCfg(inherited Cfg);
end;

procedure TsmxPage.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.Sections.Count - 1 do
    with Cfg.Sections[i] do
    begin
      Sections[i].CellAlign := UnitAlign;
      Sections[i].CellEnabled := UnitEnabled;
      Sections[i].CellHeight := UnitHeight;
      Sections[i].CellLeft := UnitLeft;
      Sections[i].CellTop := UnitTop;
      Sections[i].CellVisible := UnitVisible;
      Sections[i].CellWidth := UnitWidth;
    end;
end;}

{ TsmxTabSheet }

constructor TsmxTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

destructor TsmxTabSheet.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FTabSheet) then
    FTabSheet.Free;
  //if Assigned(FWinControl) then
    //FWinControl.Free;
end;

{function TsmxTabSheet.GetCellActive: Boolean;
begin
  if Assigned(TabSheet.PageControl) then
    Result := TabSheet = TabSheet.PageControl.ActivePage else
    Result := False;
end;

procedure TsmxTabSheet.SetCellActive(Value: Boolean);
begin
  if Assigned(TabSheet.PageControl) then
    TabSheet.PageControl.ActivePage := TabSheet;
end;}

function TsmxTabSheet.GetCellCaption: String;
begin
  Result := TabSheet.Caption;
end;

procedure TsmxTabSheet.SetCellCaption(const Value: String);
begin
  TabSheet.Caption := Value;
end;

function TsmxTabSheet.GetCellHint: String;
begin
  Result := TabSheet.Hint;
end;

procedure TsmxTabSheet.SetCellHint(const Value: String);
begin
  TabSheet.Hint := Value;
end;

function TsmxTabSheet.GetCellImageIndex: Integer;
begin
  Result := TabSheet.ImageIndex;
end;

procedure TsmxTabSheet.SetCellImageIndex(Value: Integer);
begin
  TabSheet.ImageIndex := Value;
end;

function TsmxTabSheet.GetCellVisible: Boolean;
begin
  Result := TabSheet.TabVisible;
end;

procedure TsmxTabSheet.SetCellVisible(Value: Boolean);
var
  PageControl: TPageControl;
  Form: TForm;
begin
  if GetCellVisible <> Value then
  begin
    PageControl := TabSheet.PageControl;
    Form := nil;
    try
      if Assigned(PageControl) and not Assigned(PageControl.Parent) then
      begin
        Form := TForm.Create(nil);
        PageControl.Parent := Form;
      end;
      TabSheet.PageControl := nil;
      TabSheet.TabVisible := Value;
      TabSheet.PageControl := PageControl;
    finally
      if Assigned(Form) then
      begin
        PageControl.Parent := nil;
        Form.Free;
      end;
    end;
  end;
end;

{function TsmxTabSheet.GetCellAlign: TAlign;
begin
  Result := FPage.Align;
end;

function TsmxTabSheet.GetCellEnabled: Boolean;
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

function TsmxTabSheet.GetCellTop: Integer;
begin
  Result := FPage.Top;
end;

function TsmxTabSheet.GetCellWidth: Integer;
begin
  Result := FPage.Width;
end;

procedure TsmxTabSheet.SetCellAlign(Value: TAlign);
begin
  FPage.Align := Value;
end;

procedure TsmxTabSheet.SetCellEnabled(Value: Boolean);
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

procedure TsmxTabSheet.SetCellTop(Value: Integer);
begin
  FPage.Top := Value;
end;

procedure TsmxTabSheet.SetCellWidth(Value: Integer);
begin
  FPage.Width := Value;
end;}

function TsmxTabSheet.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxPageCfg;
end;

function TsmxTabSheet.GetInternalObject: TObject;
begin
  Result := TabSheet;
end;

function TsmxTabSheet.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxPanelSection;
end;

function TsmxTabSheet.GetTabSheet: TTabSheet;
begin
  if not Assigned(FTabSheet) then
  //begin
    //FWinControl := TForm.Create(nil);
    FTabSheet := TTabSheet.Create(nil);
    //FTabSheet.ControlStyle := FTabSheet.ControlStyle - [csAcceptsControls];
  //end;
  Result := FTabSheet;
end;

{procedure TsmxTabSheet.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxPageCfg then
  begin
  end;
end;}

procedure TsmxTabSheet.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
  Form: TForm;
  PageControl: TPageControl;
begin
  if Assigned(CellParent) then
  begin
    PageControl := TabSheet.PageControl;
    Form := nil;
    try
      if Assigned(PageControl) and not Assigned(PageControl.Parent) then
      begin
        Form := TForm.Create(nil);
        PageControl.Parent := Form;
      end;
      TabSheet.PageControl := nil;
    finally
      if Assigned(Form) then
      begin
        PageControl.Parent := nil;
        Form.Free;
      end;
    end;
  end;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TPageControl then
    begin
      PageControl := TPageControl(Obj);
      Form := nil;
      try
        if not Assigned(PageControl.Parent) then
        begin
          Form := TForm.Create(nil);
          PageControl.Parent := Form;
        end;
        TabSheet.PageControl := PageControl;
      finally
        if Assigned(Form) then
        begin
          PageControl.Parent := nil;
          Form.Free;
        end;
      end;
    end;
  end;
end;

procedure TsmxTabSheet.SetSlaveIndex(Value: Integer);
begin
  inherited SetSlaveIndex(Value);
  TabSheet.PageIndex := Value;
end;

{ TsmxPageManager }

{procedure TsmxPageManager.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  PageList.Count := Cfg.Sheets.Count;
  for i := 0 to Cfg.Sheets.Count - 1 do
    with Cfg.Sheets[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomPage then
          PageList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxPageManager.GetCfg: TsmxPageManagerCfg;
begin
  Result := TsmxPageManagerCfg(inherited Cfg);
end;

procedure TsmxPageManager.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.Sheets.Count - 1 do
    with Cfg.Sheets[i] do
    begin
      Pages[i].CellAlign := UnitAlign;
      Pages[i].CellEnabled := UnitEnabled;
      Pages[i].CellHeight := UnitHeight;
      Pages[i].CellLeft := UnitLeft;
      Pages[i].CellTop := UnitTop;
      Pages[i].CellVisible := UnitVisible;
      Pages[i].CellWidth := UnitWidth;
    end;
end;}

{ TsmxPageControl }

{constructor TsmxPageControl.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPageControl := TPageControl.Create(Self);
  //FPageControl.Images := ImageList;
  InstallParent;
end;}

destructor TsmxPageControl.Destroy;
begin
  //UnInstallParent;
  //if Assigned(FPageControl) then
    //FPageControl.Free;
  inherited Destroy;
  if Assigned(FPageControl) then
  //begin
    //if not Assigned(FPageControl.Parent) then
      //if Assigned(Forms.Application.MainForm) then
        //if _TWinControl(FPageControl).WindowHandle = Forms.Application.MainForm.Handle then
          //_TWinControl(FPageControl).WindowHandle := 0;
    FPageControl.Free;
  //end;
  //if Assigned(FWinControl) then
    //FWinControl.Free;
end;

procedure TsmxPageControl.DoChangePage;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnChangePage) then
  begin
    if Cfg is TsmxPageManagerCfg then
      AlgCfgID := TsmxPageManagerCfg(Cfg).ChangePageAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnChangePage, AlgCfgID);
  end;
end;

{function TsmxPageControl.GetActivePage: TsmxCustomPage;
begin
  Result := Pages[FPageControl.ActivePageIndex];
end;

procedure TsmxPageControl.SetActivePage(Value: TsmxCustomPage);
begin
  FPageControl.ActivePageIndex := PageList.IndexOf(Value);
end;

function TsmxPageControl.GetCellAlign: TAlign;
begin
  Result := FPageControl.Align;
end;

function TsmxPageControl.GetCellEnabled: Boolean;
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
end;}

{procedure TsmxPageControl.SetCellAlign(Value: TAlign);
begin
  FPageControl.Align := Value;
end;

procedure TsmxPageControl.SetCellEnabled(Value: Boolean);
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
end;}

{procedure TsmxPageControl.InstallParent;
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
end;}

function TsmxPageControl.GetActivePageIndex: Integer;
begin
  Result := PageControl.ActivePageIndex;
end;

procedure TsmxPageControl.SetActivePageIndex(Value: Integer);
begin
  PageControl.ActivePageIndex := Value;
end;

function TsmxPageControl.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxPageManagerCfg;
end;

function TsmxPageControl.GetInternalObject: TObject;
begin
  Result := PageControl;
end;

function TsmxPageControl.GetIsMultiLine: Boolean;
begin
  Result := PageControl.MultiLine;
end;

procedure TsmxPageControl.SetIsMultiLine(Value: Boolean);
begin
  PageControl.MultiLine := Value;
end;

function TsmxPageControl.GetPageManagerStyle: TsmxPageManagerStyle;
const
  InOutStyle: array[TTabStyle] of TsmxPageManagerStyle =
    (pmsTab, pmsTab, pmsFlat);
begin
  Result := InOutStyle[PageControl.Style];
end;

procedure TsmxPageControl.SetPageManagerStyle(Value: TsmxPageManagerStyle);
const
  OutInStyle: array[TsmxPageManagerStyle] of TTabStyle =
    (tsTabs, tsFlatButtons);
begin
  PageControl.Style := OutInStyle[Value];
end;

function TsmxPageControl.GetPageControl: TPageControl;
begin
  if not Assigned(FPageControl) then
  begin
    //FWinControl := TForm.Create(nil);
    FPageControl := TPageControl.Create(nil);
    //FPageControl.Parent := FWinControl;
    FPageControl.OnChange := PageControlChange;
  end;
  Result := FPageControl;
end;

function TsmxPageControl.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxTabSheet;
end;

{procedure TsmxPageControl.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxPageManagerCfg then
  begin
    IsMultiLine := TsmxPageManagerCfg(Cfg).IsMultiLine;
    PageManagerStyle := TsmxPageManagerCfg(Cfg).PageManagerStyle;
  end;
end;}

procedure TsmxPageControl.PageControlChange(Sender: TObject);
begin
  ChangePage;
end;

procedure TsmxPageControl.ResetCellProps;
begin
  inherited ResetCellProps;
  IsMultiLine := False;
  PageManagerStyle := pmsTab;
  OnChangePage := nil;
end;

procedure TsmxPageControl.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
  //begin
    PageControl.Parent := nil;
    //if Assigned(Forms.Application.MainForm) then
    //  _TWinControl(PageControl).WindowHandle := Forms.Application.MainForm.Handle;
  //end;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      PageControl.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxPageControl.SetCellProps;
begin
  inherited SetCellProps;
  if Cfg is TsmxPageManagerCfg then
  begin
    IsMultiLine := TsmxPageManagerCfg(Cfg).IsMultiLine;
    PageManagerStyle := TsmxPageManagerCfg(Cfg).PageManagerStyle;
  end;
end;

procedure TsmxPageControl.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    PageControl.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    PageControl.Images := ImageList;
end;


{procedure TsmxPageControl.UnInstallParent;
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
end;}

{ TsmxLibAction }

{constructor TsmxLibAction.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FAction := TAction.Create(Self);
  //FParams := TsmxParams.Create(TsmxParam);
  //FAction.Caption := Cfg.AlgDefCaption;
  //FAction.Hint := Cfg.AlgDefHint;
  //FAction.ShortCut := TShortCut(Cfg.AlgDefHotKey);
  //FAction.ImageIndex := TImageIndex(Cfg.AlgImageIndex);
  FAction.OnExecute := ProcExec;
  FParams := TsmxParams.Create(TsmxParam);
end;}

{destructor TsmxLibAction.Destroy;
begin
  //FParams.Free;
  inherited Destroy;
  //FAction.OnExecute := nil;
  if Assigned(FAction) then
    FAction.Free;
end;}

{function TsmxLibAction.GetAction: TAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := TAction.Create(Self);
    FAction.OnExecute := ActionExecute;
  end;
  Result := FAction;
end;

function TsmxLibAction.GetAlgorithmCaption: String;
begin
  Result := Action.Caption;
end;

procedure TsmxLibAction.SetAlgorithmCaption(const Value: String);
begin
  Action.Caption := Value;
end;

function TsmxLibAction.GetAlgorithmEnabled: Boolean;
begin
  Result := Action.Enabled;
end;

procedure TsmxLibAction.SetAlgorithmEnabled(Value: Boolean);
begin
  Action.Enabled := Value;
end;

function TsmxLibAction.GetAlgorithmHint: String;
begin
  Result := Action.Hint;
end;

procedure TsmxLibAction.SetAlgorithmHint(const Value: String);
begin
  Action.Hint := Value;
end;

function TsmxLibAction.GetAlgorithmHotKey: Integer;
begin
  Result := Integer(Action.ShortCut);
end;

procedure TsmxLibAction.SetAlgorithmHotKey(Value: Integer);
begin
  Action.ShortCut := TShortCut(Value);
end;

function TsmxLibAction.GetAlgorithmImageIndex: Integer;
begin
  Result := Integer(Action.ImageIndex);
end;

procedure TsmxLibAction.SetAlgorithmImageIndex(Value: Integer);
begin
  Action.ImageIndex := TImageIndex(Value);
end;

function TsmxLibAction.GetAlgorithmVisible: Boolean;
begin
  Result := Action.Visible;
end;

procedure TsmxLibAction.SetAlgorithmVisible(Value: Boolean);
begin
  Action.Visible := Value;
end;

function TsmxLibAction.GetInternalObject: TObject;
begin
  Result := Action;
end;}

{procedure TsmxLibAction.InternalInitialize;
var
  Proc: Pointer;
  Method: TMethod;
begin
  inherited InternalInitialize;
  if Cfg is TsmxLibAlgorithmCfg then
  begin
    AlgorithmLibrary := TsmxLibAlgorithmCfg(Cfg).AlgorithmLibrary;
    AlgorithmProcName := TsmxLibAlgorithmCfg(Cfg).AlgorithmProcedure;
    Proc := GetProcPointer;
    if Assigned(Proc) then
    begin
      Method.Code := Proc;
      Method.Data := Self;
      OnExecute := TsmxComponentEvent(Method);
    end;
  end;
end;}

{procedure TsmxLibAction.InternalRefreshParams;
var
  i: integer;
  Value: Variant;
  Form, PForm: TsmxCustomForm;
  //Filter: TsmxCustomFilter;
  //Field: TsmxSenseField;
  Res: Boolean;
begin
  //Form := AccessoryForm;
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
          ;//Value := AlgorithmParams[i].ParamDefValue; //AlgorithmParams.ParamByName(AlgorithmParams[i].ParamName).ParamDefValue;
        end;
        plFilterDesk:
        begin
          if Assigned(Form) then
            //FindFilterOnForm(Form, AlgorithmParams[i].ParamName, Value);
        end;
        plGrid:
        begin
            //Value := Field.Value;
          if Assigned(Form) then
            //FindFieldOnForm(Form, AlgorithmParams[i].ParamName, Value);
        end;
        plParentFilterDesk:
        begin
          //Filter := nil;
          Res := False;
          //if Assigned(Filter) then
            //Value := Filter.FilterValue;
        end;
        plParentGrid:
        begin
          //Field := nil;
          Res := False;
          //if Assigned(Field) then
            //Value := FieldValue[Field.FieldName];
            //Value := Field.Value;
        end;
        //plParentParams: v := Null;

      end;

      //Params.ParamByName(AlgorithmParams[i].ParamName).ParamValue := Value;


    end;
end;}

{procedure TsmxLibAction.ResetCellProps;
begin
  inherited ResetCellProps;
  AlgorithmLibrary := '';
  AlgorithmProcName := '';
end;}

{procedure TsmxLibAction.Execute;
begin
  if Assigned(LibProc) then
    LibProc(Self);
end;}

{procedure TsmxLibAction.ActionExecute(Sender: TObject);
begin
  Execute;
end;}

{procedure TsmxLibAction.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  inherited SetLibraryManager(Value);
  if LibraryManager <> Value then
    FLibProc := nil;
end;}

{function TsmxLibAction.GetProcPointer: Pointer;
begin
  if Assigned(LibraryManager) then
    Result := LibraryManager.GetProcedure(AlgorithmLibrary, AlgorithmProcName) else
    Result := nil;
end;

procedure TsmxLibAction.SetAlgorithmLibrary(const Value: String);
begin
  FAlgorithmLibrary := Value;
end;

procedure TsmxLibAction.SetAlgorithmProcName(const Value: String);
begin
  FAlgorithmProcName := Value;
end;}

{procedure TsmxLibAction.Initialize(const ACfgDatabase: IsmxDatabase;
  ACfgID: Integer; ASelectRequest: TsmxCustomRequest = nil);
begin
  //inherited Initialize(ACfgDatabase, ACfgID, ASelectRequest);
  Cfg := smxClassFuncs.NewCfg(Self, ACfgDatabase, ACfgID, ASelectRequest);
  try
    if Cfg is TsmxActionCfg then
      with TsmxActionCfg(Cfg) do
      begin
        AlgorithmCaption := AlgDefCaption;
        AlgorithmHint := AlgDefHint;
        AlgorithmHotKey := AlgDefHotKey;
        AlgorithmImageIndex := AlgImageIndex;
        AlgorithmParams := AlgParams;
        AlgorithmLibrary := AlgLibrary;
        AlgorithmProcName := AlgProcedure;
      end
    else
      raise EsmxCellError.CreateResFmt(@SCellBuildError, [ACfgID]);
  finally
    Cfg.Free;
  end;
end;}

{procedure TsmxLibAction.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Action.ActionList := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TCustomActionList then
      Action.ActionList := TCustomActionList(Obj);
  end;
end;}

{ TsmxActionList }

{procedure TsmxActionList.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  AlgorithmList.Count := Cfg.AlgorithmItems.Count;
  for i := 0 to Cfg.AlgorithmItems.Count - 1 do
    with Cfg.AlgorithmItems[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomAlgorithm then
          AlgorithmList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;}

{function TsmxActionList.GetCfg: TsmxActionListCfg;
begin
  Result := TsmxActionListCfg(inherited Cfg);
end;}

{procedure TsmxActionList.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.AlgorithmItems.Count - 1 do
    with Cfg.AlgorithmItems[i] do
    begin
      Algorithms[i].CellEnabled := AlgorithmEnabled;
      Algorithms[i].CellVisible := AlgorithmVisible;
      Algorithms[i].CellHotKey := AlgorithmHotKey;
      Algorithms[i].CellCaption := AlgorithmCaption;
      Algorithms[i].MenuPointID := AlgorithmMenuItemCfgID;
      Algorithms[i].ToolBoardID := AlgorithmToolBarCfgID;
    end;
end;}

{ TsmxMenuPoint }

{function TsmxMenuPoint.GetCfg: TsmxMenuItemCfg;
begin
  Result := TsmxMenuItemCfg(inherited Cfg);
end;}

{ TsmxMenuItem }

{constructor TsmxMenuItem.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FMenuItem := TMenuItem.Create(Self);
  FMenuItem.Caption := Cfg.ItemCaption;
  FMenuItem.ImageIndex := TImageIndex(Cfg.ItemImageIndex);
end;}

destructor TsmxMenuItem.Destroy;
begin
  inherited Destroy;
  if Assigned(FMenuItem) then
    FMenuItem.Free;
end;

function TsmxMenuItem.GetCellCaption: String;
begin
  Result := MenuItem.Caption;
end;

procedure TsmxMenuItem.SetCellCaption(const Value: String);
begin
  MenuItem.Caption := Value;
end;

function TsmxMenuItem.GetCellEnabled: Boolean;
begin
  Result := MenuItem.Enabled;
end;

procedure TsmxMenuItem.SetCellEnabled(Value: Boolean);
begin
  MenuItem.Enabled := Value;
end;

function TsmxMenuItem.GetCellHint: String;
begin
  Result := MenuItem.Hint;
end;

procedure TsmxMenuItem.SetCellHint(const Value: String);
begin
  MenuItem.Hint := Value;
end;

function TsmxMenuItem.GetCellImageIndex: Integer;
begin
  Result := Integer(MenuItem.ImageIndex);
end;

procedure TsmxMenuItem.SetCellImageIndex(Value: Integer);
begin
  MenuItem.ImageIndex := TImageIndex(Value);
end;

function TsmxMenuItem.GetCellVisible: Boolean;
begin
  Result := MenuItem.Visible;
end;

procedure TsmxMenuItem.SetCellVisible(Value: Boolean);
begin
  MenuItem.Visible := Value;
end;

function TsmxMenuItem.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxMenuItemCfg;
end;

function TsmxMenuItem.GetInternalObject: TObject;
begin
  Result := MenuItem;
end;

function TsmxMenuItem.GetIsChecked: Boolean;
begin
  Result := MenuItem.Checked;
end;

procedure TsmxMenuItem.SetIsChecked(Value: Boolean);
begin
  MenuItem.Checked := Value;
end;

function TsmxMenuItem.GetMenuItemHotKey: Integer;
begin
  Result := Integer(MenuItem.ShortCut);
end;

procedure TsmxMenuItem.SetMenuItemHotKey(Value: Integer);
begin
  MenuItem.ShortCut := TShortCut(Value);
end;

function TsmxMenuItem.GetMenuItemStyle: TsmxMenuItemStyle;
begin
  if MenuItem.Caption = '-' then
    Result := misDivider else
    Result := misPoint;
end;

procedure TsmxMenuItem.SetMenuItemStyle(Value: TsmxMenuItemStyle);
begin
  if Value = misDivider then
    MenuItem.Caption := '-';
end;

function TsmxMenuItem.GetMenuItem: TMenuItem;
begin
  if not Assigned(FMenuItem) then
    FMenuItem := TMenuItem.Create(nil);
  Result := FMenuItem;
end;

function TsmxMenuItem.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxMenuItem;
end;

{procedure TsmxMenuItem.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxMenuItemCfg then
  begin
    IsChecked := TsmxMenuItemCfg(Cfg).IsChecked;
    MenuItemHotKey := TsmxMenuItemCfg(Cfg).MenuItemHotKey;
    MenuItemStyle := TsmxMenuItemCfg(Cfg).MenuItemStyle;
  end;
end;}

procedure TsmxMenuItem.ResetCellProps;
begin
  inherited ResetCellProps;
  IsChecked := False;
  MenuItemHotKey := 0;
  MenuItemStyle := misPoint;
end;

procedure TsmxMenuItem.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TMenuItem then
      TMenuItem(Obj).Remove(MenuItem) else
    if Obj is TMainMenu then
      TMainMenu(Obj).Items.Remove(MenuItem);
  end;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TMenuItem then
      TMenuItem(Obj).Add(MenuItem) else
    if Obj is TMainMenu then
      TMainMenu(Obj).Items.Add(MenuItem);
  end;
end;

procedure TsmxMenuItem.SetCellProps;
begin
  inherited SetCellProps;
  if Cfg is TsmxMenuItemCfg then
  begin
    IsChecked := TsmxMenuItemCfg(Cfg).IsChecked;
    MenuItemHotKey := TsmxMenuItemCfg(Cfg).MenuItemHotKey;
    MenuItemStyle := TsmxMenuItemCfg(Cfg).MenuItemStyle;
  end;
end;

procedure TsmxMenuItem.SetSlaveIndex(Value: Integer);
begin
  inherited SetSlaveIndex(Value);
  MenuItem.MenuIndex := Value;
end;

{ TsmxMasterMenu }

{procedure TsmxMasterMenu.CreateChilds;

  procedure AddItems(AUnit: TsmxHVisibleUnit);
  var i: Integer; c: TsmxBaseCell;
  begin
    with AUnit do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomMenuPoint then
          MenuPointList.Add(c) else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    for i := 0 to AUnit.Count - 1 do
      AddItems(AUnit[i]);
  end;

var i: Integer;
begin
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    AddItems(Cfg.MenuUnits.Root[i]);
end;}

{function TsmxMasterMenu.GetCfg: TsmxMainMenuCfg;
begin
  Result := TsmxMainMenuCfg(inherited Cfg);
end;

function TsmxMasterMenu.MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint;
var u: TsmxHVisibleUnit;
begin
  Result := nil;
  u := Cfg.MenuUnits.Root.FindByCfgID(ACfgID, True);
  if Assigned(u) then
    if u.HasParent then
      Result := FindMenuPointByCfgID(u.Parent.CfgID);
end;

procedure TsmxMasterMenu.InitChilds;

  procedure InitItems(AUnit: TsmxHVisibleUnit);
  var i: Integer; mi: TsmxCustomMenuPoint;
  begin
    mi := FindMenuPointByCfgID(AUnit.CfgID);
    if Assigned(mi) then
      with AUnit do
      begin
        mi.CellAlign := UnitAlign;
        mi.CellEnabled := UnitEnabled;
        mi.CellHeight := UnitHeight;
        mi.CellLeft := UnitLeft;
        mi.CellTop := UnitTop;
        mi.CellVisible := UnitVisible;
        mi.CellWidth := UnitWidth;
      end;
    for i := 0 to AUnit.Count - 1 do
      InitItems(AUnit[i]);
  end;

var i: Integer;
begin
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    InitItems(Cfg.MenuUnits.Root[i]);
end;}

{ TsmxMainMenu }

{constructor TsmxMainMenu.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FMainMenu := TMainMenu.Create(Self);
  //FMainMenu.Images := ImageList;
  InstallParent;
end;}

destructor TsmxMainMenu.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FMainMenu) then
    FMainMenu.Free;
end;

function TsmxMainMenu.GetCellEnabled: Boolean;
begin
  Result := MainMenu.Items.Enabled;
end;

procedure TsmxMainMenu.SetCellEnabled(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to MainMenu.Items.Count - 1 do
    MainMenu.Items[i].Enabled := Value;
end;

function TsmxMainMenu.GetCellVisible: Boolean;
begin
  Result := MainMenu.Items.Visible;
end;

procedure TsmxMainMenu.SetCellVisible(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to MainMenu.Items.Count - 1 do
    MainMenu.Items[i].Visible := Value;
end;

function TsmxMainMenu.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxMainMenuCfg;
end;

function TsmxMainMenu.GetInternalObject: TObject;
begin
  Result := MainMenu;
end;

function TsmxMainMenu.GetMainMenu: TMainMenu;
begin
  if not Assigned(FMainMenu) then
    FMainMenu := TMainMenu.Create(nil);
  Result := FMainMenu;
end;

function TsmxMainMenu.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxMenuItem;
end;

procedure TsmxMainMenu.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    MainMenu.WindowHandle := 0;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TCustomForm then
      MainMenu.WindowHandle := TCustomForm(Obj).Handle;
  end;
end;

procedure TsmxMainMenu.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    MainMenu.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
  begin
    MainMenu.Images := ImageList;
    if MainMenu.Items.Count > 0 then
      _TMenuItem(MainMenu.Items[0]).MenuChanged(True);
  end;
end;

{procedure TsmxMainMenu.SetMenuItemList(Value: TsmxCustomMenuItem);
var
  Obj: TObject;
begin
  if Assigned(MenuItemList) then
    MainMenu.Items.Clear;
  inherited SetMenuItemList(Value);
  if Assigned(MenuItemList) then
  begin
    Obj := _TsmxBaseCell(MenuItemList).GetInternalObject;
    if Obj is TMenuItem then
      MainMenu.Items.Add(TMenuItem(Obj));
  end;
end;}

{ TsmxPopupMenu }

destructor TsmxPopupMenu.Destroy;
begin
  inherited Destroy;
  if Assigned(FPopupMenu) then
    FPopupMenu.Free;
end;

function TsmxPopupMenu.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxPopupMenuCfg;
end;

function TsmxPopupMenu.GetInternalObject: TObject;
begin
  Result := PopupMenu;
end;

function TsmxPopupMenu.GetPopupMenu: TPopupMenu;
begin
  if not Assigned(FPopupMenu) then
    FPopupMenu := TPopupMenu.Create(nil);
  Result := FPopupMenu;
end;

function TsmxPopupMenu.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxMenuItem;
end;

procedure TsmxPopupMenu.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    PopupMenu.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    PopupMenu.Images := Value;
end;

{ TsmxPopupList }

constructor TsmxPopupList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsAltSlaveClass := True;
end;

function TsmxPopupList.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxPopupListCfg;
end;

function TsmxPopupList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxPopupMenu;
end;

{ TsmxToolItem }

destructor TsmxToolItem.Destroy;
begin
  inherited Destroy;
  if Assigned(FToolButton) then
    FToolButton.Free;
end;

function TsmxToolItem.GetCellCaption: String;
begin
  Result := String(ToolButton.Caption);
end;

procedure TsmxToolItem.SetCellCaption(const Value: String);
begin
  ToolButton.Caption := TCaption(Value);
end;

function TsmxToolItem.GetCellHint: String;
begin
  Result := ToolButton.Hint;
end;

procedure TsmxToolItem.SetCellHint(const Value: String);
begin
  ToolButton.Hint := Value;
end;

function TsmxToolItem.GetCellImageIndex: Integer;
begin
  Result := Integer(ToolButton.ImageIndex);
end;

procedure TsmxToolItem.SetCellImageIndex(Value: Integer);
begin
  ToolButton.ImageIndex := TImageIndex(Value);
end;

function TsmxToolItem.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxToolItemCfg;
end;

function TsmxToolItem.GetInternalObject: TObject;
begin
  Result := ToolButton;
end;

function TsmxToolItem.GetIsChecked: Boolean;
begin
  Result := ToolButton.Down;
end;

procedure TsmxToolItem.SetIsChecked(Value: Boolean);
begin
  ToolButton.Down := Value;
end;

function TsmxToolItem.GetToolButton: TToolButton;
begin
  if not Assigned(FToolButton) then
    FToolButton := TToolButton.Create(nil);
  Result := FToolButton;
end;

function TsmxToolItem.GetToolItemStyle: TsmxToolItemStyle;
const
  InOutStyle: array[TToolButtonStyle] of TsmxToolItemStyle =
    (tisButton, tisCheck, tisButton, tisDivider, tisDivider);
begin
  Result := InOutStyle[ToolButton.Style];
end;

procedure TsmxToolItem.SetToolItemStyle(Value: TsmxToolItemStyle);
const
  OutInStyle: array[TsmxToolItemStyle] of TToolButtonStyle =
    (tbsButton, tbsCheck, tbsDivider);
begin
  ToolButton.Style := OutInStyle[Value];
end;

{procedure TsmxToolItem.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxToolItemCfg then
  begin
    IsChecked := TsmxToolItemCfg(Cfg).IsChecked;
    ToolItemStyle := TsmxToolItemCfg(Cfg).ToolItemStyle;
  end;
end;}

procedure TsmxToolItem.ResetCellProps;
begin
  inherited ResetCellProps;
  IsChecked := False;
  ToolItemStyle := tisButton;
end;

procedure TsmxToolItem.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    ToolButton.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      ToolButton.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxToolItem.SetCellProps;
begin
  inherited SetCellProps;
  if Cfg is TsmxToolItemCfg then
  begin
    IsChecked := TsmxToolItemCfg(Cfg).IsChecked;
    ToolItemStyle := TsmxToolItemCfg(Cfg).ToolItemStyle;
  end;
end;

{ TsmxToolBoard }

{function TsmxToolBoard.GetCfg: TsmxToolBoardCfg;
begin
  Result := TsmxToolBoardCfg(inherited Cfg);
end;}

{ TsmxToolBar }

constructor TsmxToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {FToolBar := TToolBar.Create(Self);
  FToolBar.AutoSize := True;
  FToolBar.EdgeBorders := FToolBar.EdgeBorders - [ebTop];
  FToolBar.Width := 0;
  FToolBar.Flat := Cfg.BarFlat;
  FToolBar.ShowCaptions := Cfg.BarShowCaptions;
  FToolBar.ShowHint := Cfg.BarShowHint;}
  //Initialize;
  IsAltSlaveClass := True;
end;

destructor TsmxToolBar.Destroy;
begin
  //UnInitialize;
  inherited Destroy;
  if Assigned(FToolBar) then
    FToolBar.Free;
end;

{procedure TsmxToolBar.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
var c: TObject; b: TToolButton;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
  begin
    b := TToolButton.Create(Self);
    b.Parent := FToolBar;
    b.ShowHint := Cfg.BarShowHint;
    b.Action := TBasicAction(c);
  end;
end;

procedure TsmxToolBar.DelAlgorithm(Algorithm: TsmxCustomAlgorithm);
var c: TObject; i: Integer; b: TToolButton;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
    for i := FToolBar.ButtonCount - 1 downto 0 do
    begin
      b := FToolBar.Buttons[i];
      if b.Action = c then
      begin
        b.Action := nil;
        b.Parent := nil;
        b.Free;
      end;
    end;
end;}

function TsmxToolBar.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxToolBoardCfg;
end;

function TsmxToolBar.GetInternalObject: TObject;
begin
  Result := ToolBar;
end;

{function TsmxToolBar.GetCellAlign: TAlign;
begin
  Result := FToolBar.Align;
end;

function TsmxToolBar.GetCellEnabled: Boolean;
begin
  Result := FToolBar.Enabled;
end;

function TsmxToolBar.GetCellHeight: Integer;
begin
  Result := FToolBar.Height;
end;

function TsmxToolBar.GetCellLeft: Integer;
begin
  Result := FToolBar.Left;
end;

function TsmxToolBar.GetCellTop: Integer;
begin
  Result := FToolBar.Top;
end;

function TsmxToolBar.GetCellVisible: Boolean;
begin
  Result := FToolBar.Visible;
end;

function TsmxToolBar.GetCellWidth: Integer;
begin
  Result := FToolBar.Width;
end;}

{procedure TsmxToolBar.Prepare(Forcibly: Boolean = False);
var i, w: Integer;
begin
  inherited Prepare(Forcibly);
  with FToolBar do
  begin
    w := 0;
    for i := 0 to ButtonCount - 1 do
      if Buttons[i].Visible then
        w := w + Buttons[i].Width;
    Width := w;
  end;
end;}

{procedure TsmxToolBar.SetCellAlign(Value: TAlign);
begin
  FToolBar.Align := Value;
end;

procedure TsmxToolBar.SetCellEnabled(Value: Boolean);
begin
  FToolBar.Enabled := Value;
end;

procedure TsmxToolBar.SetCellHeight(Value: Integer);
begin
  FToolBar.Height := Value;
end;

procedure TsmxToolBar.SetCellLeft(Value: Integer);
begin
  FToolBar.Left := Value;
end;

procedure TsmxToolBar.SetCellTop(Value: Integer);
begin
  FToolBar.Top := Value;
end;

procedure TsmxToolBar.SetCellVisible(Value: Boolean);
begin
  FToolBar.Visible := Value;
end;

procedure TsmxToolBar.SetCellWidth(Value: Integer);
begin
  FToolBar.Width := Value;
end;}

{procedure TsmxToolBar.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FToolBar.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FToolBar.Parent := TWinControl(c);
  end;
end;}

function TsmxToolBar.GetIsFlat: Boolean;
begin
  Result := ToolBar.Flat;
end;

procedure TsmxToolBar.SetIsFlat(Value: Boolean);
begin
  ToolBar.Flat := Value;
end;

function TsmxToolBar.GetIsShowCaptions: Boolean;
begin
  Result := ToolBar.ShowCaptions;
end;

procedure TsmxToolBar.SetIsShowCaptions(Value: Boolean);
begin
  ToolBar.ShowCaptions := Value;
end;

function TsmxToolBar.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxToolItem;
end;

function TsmxToolBar.GetToolBar: TToolBar;
begin
  if not Assigned(FToolBar) then
  begin
    FToolBar := TToolBar.Create(nil);
    FToolBar.AutoSize := True;
    FToolBar.EdgeBorders := FToolBar.EdgeBorders - [ebTop];
  end;
  Result := FToolBar;
end;

{procedure TsmxToolBar.InternalInitialize;
begin
  inherited InternalInitialize;
  if Cfg is TsmxToolBoardCfg then
  begin
    IsFlat := TsmxToolBoardCfg(Cfg).IsFlat;
    IsShowCaptions := TsmxToolBoardCfg(Cfg).IsShowCaptions;
  end;
end;}

procedure TsmxToolBar.ResetCellProps;
begin
  inherited ResetCellProps;
  IsFlat := False;
  IsShowCaptions := False;
end;

procedure TsmxToolBar.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    ToolBar.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      ToolBar.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxToolBar.SetCellProps;
begin
  inherited SetCellProps;
  if Cfg is TsmxToolBoardCfg then
  begin
    IsFlat := TsmxToolBoardCfg(Cfg).IsFlat;
    IsShowCaptions := TsmxToolBoardCfg(Cfg).IsShowCaptions;
  end;
end;

procedure TsmxToolBar.SetImageList(Value: TCustomImageList);
var
  Control: TWinControl;
begin
  {Form := Forms.GetParentForm(ToolBar);
  if Assigned(Form) then
    ToolBar.Images := Value
  else
  begin
    Form := TForm.Create(nil);
    try
      ToolBar.Parent := Form;
      ToolBar.Images := Value;
      ToolBar.Parent := nil;
    finally
      Form.Free;
    end;
  end;}
  Control := nil;
  try
    if not Assigned(ToolBar.Parent) then
    begin
      Control := TForm.Create(Self);
      ToolBar.Parent := Control;
    end;
    if Assigned(ImageList) then
      ToolBar.Images := nil;
    inherited SetImageList(Value);
    if Assigned(ImageList) then
      ToolBar.Images := ImageList;
  finally
    if Assigned(Control) then
    begin
      ToolBar.Parent := nil;
      Control.Free;
    end;
  end;
end;

{procedure TsmxToolBar.UnInitialize;
begin
  FToolBar.Images := nil;
end;}

{ TsmxControlBoard }

{procedure TsmxControlBoard.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  ToolBoardList.Count := Cfg.BarUnits.Count;
  for i := 0 to Cfg.BarUnits.Count - 1 do
    with Cfg.BarUnits[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomToolBoard then
          ToolBoardList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxControlBoard.GetCfg: TsmxControlBoardCfg;
begin
  Result := TsmxControlBoardCfg(inherited Cfg);
end;

procedure TsmxControlBoard.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.BarUnits.Count - 1 do
    with Cfg.BarUnits[i] do
    begin
      ToolBoards[i].CellAlign := UnitAlign;
      ToolBoards[i].CellEnabled := UnitEnabled;
      ToolBoards[i].CellHeight := UnitHeight;
      ToolBoards[i].CellLeft := UnitLeft;
      ToolBoards[i].CellTop := UnitTop;
      ToolBoards[i].CellVisible := UnitVisible;
      ToolBoards[i].CellWidth := UnitWidth;
    end;
end;}

{ TsmxControlBar }

constructor TsmxControlBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {FControlBar := TControlBar.Create(Self);
  FControlBar.AutoSize := True;
  FControlBar.BevelInner := bvNone;
  InstallParent;}
  IsAltSlaveClass := True;
end;

destructor TsmxControlBar.Destroy;
begin
  //UnInstallParent;
  inherited Destroy;
  if Assigned(FControlBar) then
    FControlBar.Free;
end;

function TsmxControlBar.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxControlBoardCfg;
end;

function TsmxControlBar.GetControlBar: TControlBar;
begin
  if not Assigned(FControlBar) then
  begin
    FControlBar := TControlBar.Create(nil);
    FControlBar.AutoSize := True;
    FControlBar.BevelInner := bvNone;
  end;
  Result := FControlBar;
end;

function TsmxControlBar.GetInternalObject: TObject;
begin
  Result := ControlBar;
end;

{function TsmxControlBar.GetCellAlign: TAlign;
begin
  Result := FControlBar.Align;
end;

function TsmxControlBar.GetCellEnabled: Boolean;
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

procedure TsmxControlBar.SetCellEnabled(Value: Boolean);
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
end;}

function TsmxControlBar.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxToolBar;
end;

procedure TsmxControlBar.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    ControlBar.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      ControlBar.Parent := TWinControl(Obj);
  end;
end;

{ TsmxStatusPanel }

destructor TsmxStatusPanel.Destroy;
begin
  inherited Destroy;
  if Assigned(FStatusPanel) then
    FStatusPanel.Free;
end;

procedure TsmxStatusPanel.DoDrawPanel;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnDrawPanel) then
  begin
    if Cfg is TsmxStatusItemCfg then
      AlgCfgID := TsmxStatusItemCfg(Cfg).DrawPanelAlgCfg else
      AlgCfgID := 0;
    DoEvent(OnDrawPanel, AlgCfgID);
  end;
end;

function TsmxStatusPanel.GetCellCaption: String;
begin
  Result := StatusPanel.Text;
end;

procedure TsmxStatusPanel.SetCellCaption(const Value: String);
begin
  StatusPanel.Text := Value;
end;

function TsmxStatusPanel.GetCellWidth: Integer;
begin
  Result := StatusPanel.Width;
end;

procedure TsmxStatusPanel.SetCellWidth(Value: Integer);
begin
  StatusPanel.Width := Value;
end;

function TsmxStatusPanel.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxStatusItemCfg;
end;

function TsmxStatusPanel.GetStatusItemAlignment: TAlignment;
begin
  Result := StatusPanel.Alignment;
end;

procedure TsmxStatusPanel.SetStatusItemAlignment(Value: TAlignment);
begin
  StatusPanel.Alignment := Value;
end;

function TsmxStatusPanel.GetStatusItemStyle: TsmxStatusItemStyle;
const
  InOutStyle: array[TStatusPanelStyle] of TsmxStatusItemStyle =
    (sisText, sisDraw);
begin
  Result := InOutStyle[StatusPanel.Style];
end;

procedure TsmxStatusPanel.SetStatusItemStyle(Value: TsmxStatusItemStyle);
const
  OutInStyle: array[TsmxStatusItemStyle] of TStatusPanelStyle =
    (psText, psOwnerDraw);
begin
  StatusPanel.Style := OutInStyle[Value];
end;

function TsmxStatusPanel.GetStatusPanel: TStatusPanel;
begin
  if not Assigned(FStatusPanel) then
    FStatusPanel := TStatusPanel.Create(nil);
  Result := FStatusPanel;
end;

{procedure TsmxStatusPanel.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxStatusItemCfg then
  begin
    StatusItemAlignment := TsmxStatusItemCfg(Cfg).StatusItemAlignment;
    StatusItemStyle := TsmxStatusItemCfg(Cfg).StatusItemStyle;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnDrawPanel := smxClassFuncs.GetEventForm(Form, TsmxStatusItemCfg(Cfg).DrawPanelAlgCfg);
  end;
end;}

procedure TsmxStatusPanel.ResetCellProps;
begin
  inherited ResetCellProps;
  StatusItemAlignment := taLeftJustify;
  StatusItemStyle := sisText;
  OnDrawPanel := nil;
end;

procedure TsmxStatusPanel.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    StatusPanel.Collection := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TStatusPanels then
      StatusPanel.Collection := TStatusPanels(Obj);
  end;
end;

procedure TsmxStatusPanel.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxStatusItemCfg then
  begin
    StatusItemAlignment := TsmxStatusItemCfg(Cfg).StatusItemAlignment;
    StatusItemStyle := TsmxStatusItemCfg(Cfg).StatusItemStyle;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnDrawPanel := smxClassFuncs.GetEventForm(Form, TsmxStatusItemCfg(Cfg).DrawPanelAlgCfg);
  end;
end;

procedure TsmxStatusPanel.SetSlaveIndex(Value: Integer);
begin
  inherited SetSlaveIndex(Value);
  StatusPanel.Index := Value;
end;

{ TsmxStatusBar }

destructor TsmxStatusBar.Destroy;
begin
  inherited Destroy;
  if Assigned(FStatusBar) then
    FStatusBar.Free;
end;

function TsmxStatusBar.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxStatusBoardCfg;
end;

function TsmxStatusBar.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxStatusPanel;
end;

function TsmxStatusBar.GetStatusBar: TStatusBar;
begin
  if not Assigned(FStatusBar) then
  begin
    FStatusBar := TStatusBar.Create(nil);
    FStatusBar.OnDrawPanel := StatusBarDraw;
  end;
  Result := FStatusBar;
end;

procedure TsmxStatusBar.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    StatusBar.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      StatusBar.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxStatusBar.StatusBarDraw(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
var
  Slave: TsmxOwnerCell;
begin
  Slave := FindSlaveByInternalObject(Panel);
  if Slave is TsmxCustomStatusItem then
    TsmxCustomStatusItem(Slave).DrawPanel;
end;

{ TsmxForm }

destructor TsmxForm.Destroy;
begin
  inherited Destroy;
  if Assigned(FForm) then
  {begin
    if FIsMainForm then
      FForm := nil
    else}
      FForm.Free;
  {end;}
end;

{procedure TsmxForm.Close;
begin
  Form.OnClose := nil;
  Form.Close;
  Form.OnClose := FormClose;
end;}

procedure TsmxForm.DoClose;
var
  AlgCfgID: Integer;
begin                  
  if Assigned(OnClose) then
  begin
    if Cfg is TsmxFormCfg then
      AlgCfgID := TsmxFormCfg(Cfg).CloseAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnClose, AlgCfgID);
  end;
end;

procedure TsmxForm.InternalClose;
begin
  Form.OnClose := nil;
  Form.Close;
  {if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) and FIsCloseUser then
      Free;
  end else
    FForm := nil;}
  //Form.OnClose := FormClose;
end;

procedure TsmxForm.DoShow;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnShow) then
  begin
    if Cfg is TsmxFormCfg then
      AlgCfgID := TsmxFormCfg(Cfg).ShowAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnShow, AlgCfgID);
  end;
end;

procedure TsmxForm.InternalShow;
begin
  Form.OnClose := FormClose;
  Form.Show;
end;

function TsmxForm.InternalShowModal: TModalResult;
begin
  Form.OnClose := FormClose;
  Result := TModalResult(Form.ShowModal);
end;

procedure TsmxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DoClose;
  if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) then
    begin
      Action := caNone;
      Free;
    end;
  end else
  begin
    FForm := nil;
    Free;
  end;
end;

{procedure TsmxForm.FormDestroy(Sender: TObject);
begin
  if FIsMainForm then
  begin
    FForm := nil;
    Free;
  end;
end;}

function TsmxForm.GetCellActive: Boolean;
begin
  Result := Form.Active;
end;

procedure TsmxForm.SetCellActive(Value: Boolean);
begin
  //Windows.BringWindowToTop()
  //Form.SetFocus
  //Windows.GetDesktopWindow
  {if Value then
    Windows.SetActiveWindow(Form.Handle)
  else
    Windows.SetForegroundWindow(Form.Handle);}
  if Value then
    Form.Perform(WM_ACTIVATE, WA_ACTIVE, 0)
  else
    Form.Perform(WM_ACTIVATE, WA_INACTIVE, 0);
end;

function TsmxForm.GetCellCaption: String;
begin
  Result := String(Form.Caption);
end;

procedure TsmxForm.SetCellCaption(const Value: String);
begin
  Form.Caption := TCaption(Value);
end;

function TsmxForm.GetCellHint: String;
begin
  Result := Form.Hint;
end;

procedure TsmxForm.SetCellHint(const Value: String);
begin
  Form.Hint := Value;
end;

function TsmxForm.GetCellImageIndex: Integer;
begin
  Result := FFormImageIndex;
end;

procedure TsmxForm.SetCellImageIndex(Value: Integer);
begin
  if FFormImageIndex <> -1 then
    Form.Icon := nil;
  FFormImageIndex := Value;
  if FFormImageIndex <> -1 then
    if Assigned(ImageList) then
      ImageList.GetIcon(FFormImageIndex, Form.Icon);
end;

function TsmxForm.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxFormCfg;
end;

function TsmxForm.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    //if IsApplicationForm then
    if not FIsMainForm then
    begin
      if not Assigned(Forms.Application.MainForm) then
      begin
        Forms.Application.CreateForm(TForm, FForm);
        FIsMainForm := True;
      end else
        FForm := TForm.Create(nil);
      //FForm.OnClose := FormClose;
      //FForm.OnDestroy := FormDestroy;

      //FIsMainForm := Forms.Application.MainForm = FForm;
      //if FIsMainForm then
        //FForm.FreeNotification(Self);
    end;// else
      //FForm := TForm.Create(Self);
    //else
      //FForm := TForm.Create(Self);
  end;
  Result := FForm;
end;

function TsmxForm.GetFormBorder: TsmxFormBorder;
const
  InOutBorder: array[TFormBorderStyle] of TsmxFormBorder =
    (fbNone, fbDialog, fbSizeable, fbDialog, fbDialog, fbSizeable);
begin
  Result := InOutBorder[Form.BorderStyle];
end;

procedure TsmxForm.SetFormBorder(Value: TsmxFormBorder);
const
  OutInBorder: array[TsmxFormBorder] of TFormBorderStyle =
    (bsNone, bsDialog, bsSizeable);
begin
  Form.BorderStyle := OutInBorder[Value];
end;

function TsmxForm.GetFormPosition: TsmxFormPosition;
const
  InOutPosition: array[TPosition] of TsmxFormPosition =
    (fpDesigned, fpDesigned, fpDesigned, fpDesigned, fpScreenCenter,
     fpScreenCenter, fpOwnerFormCenter, fpOwnerFormCenter);
begin
  Result := InOutPosition[Form.Position];
end;

procedure TsmxForm.SetFormPosition(Value: TsmxFormPosition);
const
  OutInPosition: array[TsmxFormPosition] of TPosition =
    (poDesigned, poScreenCenter, poOwnerFormCenter);
begin
  Form.Position := OutInPosition[Value];
end;

function TsmxForm.GetInternalObject: TObject;
begin
  Result := Form;
end;

{function TsmxForm.GetIsMainForm: Boolean;
begin
  Result := Forms.Application.MainForm = Form;
end;}

function TsmxForm.GetIsMaximize: Boolean;
begin
  Result := Form.WindowState = wsMaximized;
end;

procedure TsmxForm.SetIsMaximize(Value: Boolean);
begin
  if Value then
    Form.WindowState := wsMaximized else
    Form.WindowState := wsNormal;
end;

function TsmxForm.GetModalResult: TModalResult;
begin
  Result := Form.ModalResult;
end;

procedure TsmxForm.SetModalResult(Value: TModalResult);
begin
  Form.ModalResult := Value;
end;

{procedure TsmxForm.InternalInitialize;
var
  Cell: TsmxBaseCell;
begin
  inherited InternalInitialize;
  if Cfg is TsmxFormCfg then
  begin
    FormBorder := TsmxFormCfg(Cfg).FormBorder;
    FormPosition := TsmxFormCfg(Cfg).FormPosition;
    IsMaximize := TsmxFormCfg(Cfg).IsMaximize;

    if TsmxFormCfg(Cfg).AlgorithmListCfgID > 0 then
    begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).AlgorithmListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      AlgorithmList := TsmxCustomAlgorithmList(Cell);
    end;

    if TsmxFormCfg(Cfg).PopupListCfgID > 0 then
    begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).PopupListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      PopupList := TsmxCustomPopupList(Cell);
    end;

    if TsmxFormCfg(Cfg).RequestListCfgID > 0 then
    begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).RequestListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      RequestList := TsmxCustomRequestList(Cell);
    end;
  end;
end;}

{procedure TsmxForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;}

{procedure TsmxForm.PrepareForm;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    smxClassProcs.AllCells(Self, List, [TsmxCustomGrid], True);
    for i := 0 to List.Count - 1 do
      TsmxCustomGrid(List[i]).Prepare;
  finally
    List.Free;
  end;
end;}

procedure TsmxForm.ResetCellProps;
begin
  inherited ResetCellProps;
  FormBorder := fbNone;
  FormPosition := fpDesigned;
  IsMaximize := False;
  AlgorithmList := nil;
  OnClose := nil;
  OnShow := nil;
  PopupList := nil;
  RequestList := nil;
end;

procedure TsmxForm.SetCellProps;

  function CreateCell(ACfgID: Integer): TsmxBaseCell;
  begin
    Result := smxClassFuncs.NewCell(nil, ACfgID{, SelectRequest});
    Result.CellParent := Self;
    Result.Initialize;
  end;

begin
  if Cfg is TsmxFormCfg then
  begin
    FormBorder := TsmxFormCfg(Cfg).FormBorder;
    FormPosition := TsmxFormCfg(Cfg).FormPosition;
    IsMaximize := TsmxFormCfg(Cfg).IsMaximize;

    if TsmxFormCfg(Cfg).AlgorithmListCfgID > 0 then
      AlgorithmList := TsmxCustomAlgorithmList(CreateCell(TsmxFormCfg(Cfg).AlgorithmListCfgID));
    {begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).AlgorithmListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      AlgorithmList := TsmxCustomAlgorithmList(Cell);
    end;}

    if TsmxFormCfg(Cfg).PopupListCfgID > 0 then
      PopupList := TsmxCustomPopupList(CreateCell(TsmxFormCfg(Cfg).PopupListCfgID));
    {begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).PopupListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      PopupList := TsmxCustomPopupList(Cell);
    end;}

    if TsmxFormCfg(Cfg).RequestListCfgID > 0 then
      RequestList := TsmxCustomRequestList(CreateCell(TsmxFormCfg(Cfg).RequestListCfgID));
    {begin
      Cell := smxClassFuncs.NewCell(Self, TsmxFormCfg(Cfg).RequestListCfgID, SelectRequest);
      Cell.CellParent := Self;
      Cell.Initialize;
      RequestList := TsmxCustomRequestList(Cell);
    end;}
    
    OnClose := smxClassFuncs.GetEventForm(Self, TsmxFormCfg(Cfg).CloseAlgCfgID);
    OnShow := smxClassFuncs.GetEventForm(Self, TsmxFormCfg(Cfg).ShowAlgCfgID);
  end;
  inherited SetCellProps;
  {if Cfg is TsmxFormCfg then
  begin
    FormBorder := TsmxFormCfg(Cfg).FormBorder;
    FormPosition := TsmxFormCfg(Cfg).FormPosition;
    IsMaximize := TsmxFormCfg(Cfg).IsMaximize;
  end;}
end;

procedure TsmxForm.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) and Assigned(Form) then
    Form.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) and Assigned(Form) then
    if FFormImageIndex <> -1 then
      ImageList.GetIcon(FFormImageIndex, Form.Icon);
end;

{procedure TsmxForm.SetIsMainForm(Value: Boolean);
begin
  if IsMainForm <> Value then
  begin
    if not Assigned(Forms.Application.MainForm) then
    begin
      if Assigned(FForm) then
      begin
        FForm.Free;

      end;
      Forms.Application.CreateForm();
    end;
  end;
end;}

{procedure TsmxForm.Show;
begin
  Form.Show;
  PrepareForm;
end;

function TsmxForm.ShowModal: TModalResult;
begin
  PrepareForm;
  Result := TModalResult(Form.ShowModal);
end;}

procedure TsmxForm.SetPopupMenu(Value: TsmxCustomPopupMenu);
var
  Obj: TObject;
begin
  if Assigned(PopupMenu) then
    Form.PopupMenu := nil;
  inherited SetPopupMenu(Value);
  if Assigned(PopupMenu) then
  begin
    Obj := _TsmxBaseCell(PopupMenu).GetInternalObject;
    if Obj is TPopupMenu then
      Form.PopupMenu := TPopupMenu(Obj);
  end;
end;

{ TsmxStandardForm }

{constructor TsmxStandardForm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADatabase, ACfgID, AID);
  FForm := TForm.Create(Self);
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  //FormManager.InsertForm(Self);
  //AddFormIntoManagerLib(Self);
  InstallParent;
  Initialize;
  //AddAlgorithms;
  PutState;
end;}

{constructor TsmxStandardForm.CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID, AIntfID: Integer; AID: Integer = 0);
begin
  inherited CreateByIntfID(AOwner, ADatabase, ACfgID, AIntfID, AID);
  FForm := TForm.Create(Self);
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  //FormManager.InsertForm(Self);
  //AddFormIntoManagerLib(Self);
  InstallParent;
  Initialize;
  //AddAlgorithms;
  PutState;
end;}

{destructor TsmxStandardForm.Destroy;
begin
  //DelAlgorithms;
  //UnInitialize;
  //UnInstallParent;
  //FormManager.RemoveForm(Self);
  //DelFormFromManagerLib(Self);
  SetFormManager(nil);
  FForm.Free;
  inherited Destroy;
end;}

{procedure TsmxStandardForm.CloseForm;
begin
  Form.OnClose := nil;
  Free;
end;}

{procedure TsmxStandardForm.ProcClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  //CloseForm;
end;}

{function TsmxStandardForm.GetFormModalResult:  TModalResult;
begin
  Result := FForm.ModalResult;
end;}

{function TsmxStandardForm.GetInternalObject: TObject;
begin
  Result := FForm;
end;

function TsmxStandardForm.GetCellAlign: TAlign;
begin
  Result := FForm.Align;
end;

function TsmxStandardForm.GetCellEnabled: Boolean;
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
end;}

{procedure TsmxStandardForm.SetFormManager(Value: TsmxCustomFormManager);
begin
  if Assigned(FormManager) then
    FormManager.RemoveForm(Self);
  inherited SetFormManager(Value);
  if Assigned(FormManager) then
    FormManager.InsertForm(Self);
end;}

{procedure TsmxStandardForm.SetFormModalResult(Value: TModalResult);
begin
  FForm.ModalResult := Value;
end;}

{procedure TsmxStandardForm.SetCellAlign(Value: TAlign);
begin
  FForm.Align := Value;
end;

procedure TsmxStandardForm.SetCellEnabled(Value: Boolean);
begin
  FForm.Enabled := Value;
end;

procedure TsmxStandardForm.SetCellHeight(Value: Integer);
begin
  FForm.Height := Value;
end;}

{procedure TsmxStandardForm.SetImageList(Value: TCustomImageList);
begin}
  {if Assigned(ImageList) then
    FForm.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    if Cfg.FormImageIndex >= 0 then
      ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);}
{end;}

{procedure TsmxStandardForm.SetCellLeft(Value: Integer);
begin
  FForm.Left := Value;
end;

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
end;}

{procedure TsmxStandardForm.ShowForm;
begin
  Form.OnClose := ProcClose;
  Form.Show;
  Prepare;
end;}

{function TsmxStandardForm.ShowModalForm: TModalResult;
begin
  Prepare;
  Result := Form.ShowModal;
end;}

{procedure TsmxStandardForm.Initialize;
var c: TObject;
begin
  //if Cfg.FormImageIndex >= 0 then
    //ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);

  if Assigned(MasterMenu) then
  begin
    c := _TsmxBaseCell(MasterMenu).GetInternalObject;
    if c is TMainMenu then
      FForm.Menu := TMainMenu(c);
  end;
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := MasterMenu;
    AlgorithmList.AddAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := ControlBoard;
    AlgorithmList.AddAlgorithmsTo(ControlBoard);
end;

procedure TsmxStandardForm.UnInitialize;
begin
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := nil;
    AlgorithmList.DelAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := nil;
    AlgorithmList.DelAlgorithmsTo(ControlBoard);
  FForm.Menu := nil;
end;}

{ TsmxMasterForm }

{procedure TsmxMasterForm.CreateChilds;
var c: TsmxBaseCell; i: Integer;
begin
  PageManagerList.Count := Cfg.PageManagers.Count;
  for i := 0 to Cfg.PageManagers.Count - 1 do
    with Cfg.PageManagers[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomPageManager then
          PageManagerList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
  with Cfg.MainMenu do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomMasterMenu then
        MasterMenu := TsmxCustomMasterMenu(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.AlgorithmList do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomAlgorithmList then
        AlgorithmList := TsmxCustomAlgorithmList(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.ControlBar do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomControlBoard then
        ControlBoard := TsmxCustomControlBoard(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.StateRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        StateRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.StatusBar do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomStatusBoard then
        StatusBoard := TsmxCustomStatusBoard(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
end;

function TsmxMasterForm.GetCfg: TsmxFormCfg;
begin
  Result := TsmxFormCfg(inherited Cfg);
end;

procedure TsmxMasterForm.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.PageManagers.Count - 1 do
    with Cfg.PageManagers[i] do
    begin
      PageManagers[i].CellAlign := UnitAlign;
      PageManagers[i].CellEnabled := UnitEnabled;
      PageManagers[i].CellHeight := UnitHeight;
      PageManagers[i].CellLeft := UnitLeft;
      PageManagers[i].CellTop := UnitTop;
      PageManagers[i].CellVisible := UnitVisible;
      PageManagers[i].CellWidth := UnitWidth;
    end;
  if Assigned(MasterMenu) then
    with Cfg.MainMenu do
    begin
      MasterMenu.CellAlign := Align;
      MasterMenu.CellEnabled := Enabled;
      MasterMenu.CellVisible := Visible;
      with PositionSize do
      begin
        MasterMenu.CellHeight := Height;
        MasterMenu.CellLeft := Left;
        MasterMenu.CellTop := Top;
        MasterMenu.CellWidth := Width;
      end;
    end;
  if Assigned(ControlBoard) then
    with Cfg.ControlBar do
    begin
      ControlBoard.CellAlign := Align;
      ControlBoard.CellEnabled := Enabled;
      ControlBoard.CellVisible := Visible;
      with PositionSize do
      begin
        ControlBoard.CellHeight := Height;
        ControlBoard.CellLeft := Left;
        ControlBoard.CellTop := Top;
        ControlBoard.CellWidth := Width;
      end;
    end;
  if Assigned(AlgorithmList) then
    with Cfg.AlgorithmList do
    begin
      AlgorithmList.IsCreateToolButton := IsCreateToolButton;
      AlgorithmList.IsCreateMenuPoint := IsCreateMenuItem;
    end;
  if Assigned(StateRequest) then
    with Cfg.StateRequest do
    begin
      StateRequest.DatabaseName := DatabaseName;
      StateRequest.OperationMode := Operation;
    end;
  if Assigned(StatusBoard) then
    with Cfg.StatusBar do
    begin
      StatusBoard.CellAlign := Align;
      StatusBoard.CellEnabled := Enabled;
      StatusBoard.CellVisible := Visible;
      with PositionSize do
      begin
        StatusBoard.CellHeight := Height;
        StatusBoard.CellLeft := Left;
        StatusBoard.CellTop := Top;
        StatusBoard.CellWidth := Width;
      end;
    end;
end;}

{ TsmxMainForm }

{procedure TsmxMainForm.CloseForm;
begin
  if not Assigned(Form) then
    Exit;
  Form.OnClose := nil;
  Form.OnCloseQuery := nil;
  Form := nil;
  Free;
end;

procedure TsmxMainForm.ProcClose(Sender: TObject; var Action: TCloseAction);
begin
  CloseForm;
end;}

{procedure TsmxMainForm.ProcCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Ask('Закрыть программу?' );
end;

function TsmxMainForm.GetInternalObject: TObject;
begin
  Result := FForm;
end;

function TsmxMainForm.GetCellAlign: TAlign;
begin
  if Assigned(FForm) then
    Result := FForm.Align else
    Result := alNone;
end;

function TsmxMainForm.GetCellEnabled: Boolean;
begin
  if Assigned(FForm) then
    Result := FForm.Enabled else
    Result := False;
end;

function TsmxMainForm.GetCellHeight: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Height else
    Result := 0;
end;

function TsmxMainForm.GetCellLeft: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Left else
    Result := 0;
end;

function TsmxMainForm.GetCellTop: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Top else
    Result := 0;
end;

function TsmxMainForm.GetCellVisible: Boolean;
begin
  if Assigned(FForm) then
    Result := FForm.Visible else
    Result := False;
end;

function TsmxMainForm.GetCellWidth: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Width else
    Result := 0;
end;

procedure TsmxMainForm.Initialize;
var c: TObject;
begin
  if not Assigned(FForm) then
    Exit;
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  //if Cfg.FormImageIndex >= 0 then
    //ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
  if Assigned(MasterMenu) then
  begin
    c := _TsmxBaseCell(MasterMenu).GetInternalObject;
    if c is TMainMenu then
      FForm.Menu := TMainMenu(c);
  end;
  InstallParent;
  SetCommonStorage(CommonStorage);
  SetLibraryManager(LibraryManager);
  //SetDatabaseManager(DatabaseManager);
  SetFormManager(FormManager);
  SetImageList(ImageList);
  //AddAlgorithms;
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := MasterMenu;
    AlgorithmList.AddAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := ControlBoard;
    AlgorithmList.AddAlgorithmsTo(ControlBoard);
  PutState;
end;

procedure TsmxMainForm.SetCellAlign(Value: TAlign);
begin
  if Assigned(FForm) then
    FForm.Align := Value;
end;

procedure TsmxMainForm.SetCellEnabled(Value: Boolean);
begin
  if Assigned(FForm) then
    FForm.Enabled := Value;
end;

procedure TsmxMainForm.SetCellHeight(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Height := Value;
end;

procedure TsmxMainForm.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    if Assigned(FForm) then
      FForm.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    if Assigned(FForm) then
      if Cfg.FormImageIndex >= 0 then
        ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
end;

procedure TsmxMainForm.SetCellLeft(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Left := Value;
end;

procedure TsmxMainForm.SetCellTop(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Top := Value;
end;

procedure TsmxMainForm.SetCellVisible(Value: Boolean);
begin
  if Assigned(FForm) then
    FForm.Visible := Value;
end;

procedure TsmxMainForm.SetCellWidth(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Width := Value;
end;}

{procedure TsmxMainForm.SetForm(AForm: TForm);
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
end;}

{procedure TsmxMainForm.SetForm(Value: TForm);
begin
  if Assigned(FForm) then
    UnInitialize;
  inherited SetForm(Value);
  if Assigned(FForm) then
    Initialize;
end;

procedure TsmxMainForm.ShowForm;
begin
  if not Assigned(Form) then
    Exit;
  Form.OnClose := ProcClose;
  Form.OnCloseQuery := ProcCloseQuery;
  Form.Show;
  Prepare;
end;

procedure TsmxMainForm.UnInitialize;
begin
  if not Assigned(FForm) then
    Exit;
  //DelAlgorithms;
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := nil;
    AlgorithmList.DelAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := nil;
    AlgorithmList.DelAlgorithmsTo(ControlBoard);
  SetImageList(nil);
  UnInstallParent;
  FForm.Menu := nil;
end;}

initialization
  Classes.RegisterClasses([TsmxAction, TsmxActionList, TsmxRequest,
    TsmxRequestList, TsmxColumn, TsmxDBGrid, TsmxPanelFilter,
    TsmxPanelFilterDesk, TsmxPanelSection, TsmxTabSheet, TsmxPageControl,
    TsmxMenuItem, TsmxMainMenu, TsmxPopupMenu, TsmxPopupList, TsmxToolItem,
    TsmxToolBar, TsmxControlBar, TsmxStatusPanel, TsmxStatusBar, TsmxForm]);

end.
