{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Cell classes              }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov �leksandr          }
{                                      }
{**************************************}

unit smxClasses;

interface

uses
  Classes, Controls, ImgList, Graphics, TypInfo, XMLIntf, smxBaseClasses,
  smxTypes, smxDBIntf, smxManagerIntf, smxBaseIntf, smxBaseTypes, smxClassIntf;

type
  { EsmxCellError }

  EsmxCellError = class(EsmxComponentError)
  protected
    function GetCfgID: Integer; virtual;

    property CfgID: Integer read GetCfgID;
  end;

  { TsmxBaseCell }

  TsmxBaseCell = class(TsmxComponent)
  private
    FCellList: TList;
    FCellParent: TsmxBaseCell;
    FCellStates: TsmxCellStates;
    FCfg: TsmxBaseCfg;
    FEventParams: TsmxParams;
    FImageList: TCustomImageList;
    FImageListName: String;
    FIsDesigning: Boolean;
    FIsRecieveCfg: Boolean;
    FOnInitialize: TsmxComponentEvent;
    FOnFinalize: TsmxComponentEvent;
    function GetCell(Index: Integer): TsmxBaseCell;
    function GetCellCount: Integer;
    function GetCellIndex: Integer;
    function GetCellList: TList;
    function GetCellRoot: TsmxBaseCell;
    function GetCfg: TsmxBaseCfg;
    function GetEventParams: TsmxParams;
  protected
    procedure ClearCells;
    procedure DoInitialize; virtual;
    procedure DoFinalize; virtual;
    function FindChildByInternalRef(Ref: Pointer): TsmxBaseCell;
    function GetCfgClass: TsmxBaseCfgClass; virtual;
    function GetCfgID: Integer;
    procedure InternalFinalize; virtual;
    procedure InternalInitialize; virtual;
    function IsStoredCell(Cell: TsmxBaseCell): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCellStateEventParam(Value: Boolean);
    procedure SetCellFeedback; virtual;
    procedure SetCellParent(Value: TsmxBaseCell); virtual;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetEventParams(Value: TsmxParams); virtual;
    procedure SetImageList(Value: TCustomImageList); virtual;
    procedure SetImageListName(const Value: String);
    procedure SetIsDesigning(Value: Boolean); virtual;
    procedure SetIsRecieveCfg(Value: Boolean); virtual;

    property CellList: TList read GetCellList;
    property Cfg: TsmxBaseCfg read GetCfg;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; virtual;
    procedure Finalize;
    function FindChildByCfgID(CfgID: Integer): TsmxBaseCell;
    function FindChildByName(const Name: String): TsmxBaseCell;
    procedure GetProperties(DestCfg: TsmxBaseCfg); virtual;
    procedure Initialize;
    procedure SetProperties(SrcCfg: TsmxBaseCfg); virtual;

    property CellCount: Integer read GetCellCount;
    property CellIndex: Integer read GetCellIndex;
    property CellParent: TsmxBaseCell read FCellParent write SetCellParent;
    property CellRoot: TsmxBaseCell read GetCellRoot;
    property Cells[Index: Integer]: TsmxBaseCell read GetCell;
    property CellStates: TsmxCellStates read FCellStates;
    property CfgID: Integer read GetCfgID write SetCfgID;
    property EventParams: TsmxParams read GetEventParams write SetEventParams;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ImageListName: String read FImageListName write SetImageListName;
    property IsDesigning: Boolean read FIsDesigning write SetIsDesigning;
    property IsRecieveCfg: Boolean read FIsRecieveCfg write SetIsRecieveCfg default True;
  published
    property OnFinalize: TsmxComponentEvent read FOnFinalize write FOnFinalize;
    property OnInitialize: TsmxComponentEvent read FOnInitialize write FOnInitialize;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

  { TsmxSlaveItem }

  TsmxOwnerCell = class;

  TsmxSlaveList = class;

  TsmxSlaveItem = class(TsmxObjectItem)
  private
    function GetKit: TsmxSlaveList;
    procedure SetKit(Value: TsmxSlaveList);
    function GetObjectItem: TsmxOwnerCell;
    procedure SetObjectItem(Value: TsmxOwnerCell);
  protected
    function GetDisplayName: String; override;
  public
    property Kit: TsmxSlaveList read GetKit write SetKit;
    property ObjectItem: TsmxOwnerCell read GetObjectItem write SetObjectItem;
  end;

  TsmxSlaveItemClass = class of TsmxSlaveItem;

  { TsmxSlaveList }

  TsmxSlaveList = class(TsmxObjectList)
  private
    function GetItem(Index: Integer): TsmxSlaveItem;
    procedure SetItem(Index: Integer; Value: TsmxSlaveItem);
  public
    function Add: TsmxSlaveItem; overload;
    function Add(ObjectClass: TPersistentClass): TsmxSlaveItem; overload;

    property Items[Index: Integer]: TsmxSlaveItem read GetItem write SetItem; default;
  end;

  TsmxSlaveListClass = class of TsmxSlaveList;

  { TsmxOwnerCell }

  TsmxOwnerCell = class(TsmxBaseCell, IsmxObjectItem, IsmxObjectList)
  private
    FCellOwner: TsmxOwnerCell;
    FIsOwnerIsParent: Boolean;
    FSlaveList: TsmxSlaveList;
    function GetSlave(Index: Integer): TsmxOwnerCell;
    function GetSlaveCount: Integer;
    procedure SetCellOwner(Value: TsmxOwnerCell);
    procedure SetSlave(Index: Integer; Value: TsmxOwnerCell);
    function GetSlaveList: TsmxSlaveList;
    procedure SetSlaveList(Value: TsmxSlaveList);
    function GetSlaveIndex: Integer;
    procedure SetSlaveIndex(Value: Integer);
    function GetSlaveItem: TsmxSlaveItem;
  protected
    FSlaveItem: TsmxSlaveItem;
    procedure CreateObject(Item: TObject); overload; virtual;
    procedure CreateObject(Item: TObject; ObjectClass: TPersistentClass); overload; virtual;
    procedure DestroyObject(Item: TObject); virtual;
    function FindSlaveByInternalRef(Ref: Pointer): TsmxOwnerCell;
    function GetSlaveClass: TsmxBaseCellClass; virtual;
    procedure ChangeObjectIndex(Value: Integer); virtual;
    procedure ChangeObjectOwner(Value: TPersistent); virtual;
    procedure CheckObjectClass(ObjectClass: TPersistentClass);
    function IsStoredCell(Cell: TsmxBaseCell): Boolean; override;
    procedure SetIsOwnerIsParent(Value: Boolean); virtual;
    procedure SetName(const NewName: TComponentName); override;

    property SlaveItem: TsmxSlaveItem read GetSlaveItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddSlave: TsmxOwnerCell;
    function AddSlaveAsClass(CellClass: TsmxBaseCellClass): TsmxOwnerCell;
    procedure ClearSlaves;
    procedure DeleteSlave(Index: Integer);
    function FindSlaveByCfgID(CfgID: Integer): TsmxOwnerCell;
    function FindSlaveByName(const Name: String): TsmxOwnerCell;
    procedure GetProperties(DestCfg: TsmxBaseCfg); override;
    procedure SetProperties(SrcCfg: TsmxBaseCfg); override;

    property CellOwner: TsmxOwnerCell read FCellOwner write SetCellOwner;
    property IsOwnerIsParent: Boolean read FIsOwnerIsParent write SetIsOwnerIsParent default True;
    property SlaveClass: TsmxBaseCellClass read GetSlaveClass;
    property SlaveCount: Integer read GetSlaveCount;
    property Slaves[Index: Integer]: TsmxOwnerCell read GetSlave write SetSlave; default;
    property SlaveList: TsmxSlaveList read GetSlaveList write SetSlaveList stored False;
  published
    property SlaveIndex: Integer read GetSlaveIndex write SetSlaveIndex stored False default -1;
  end;

  { TsmxControlCell }

  TsmxCustomPopupMenu = class;

  TsmxControlCell = class(TsmxOwnerCell)
  private
    FOnBackup: TsmxComponentEvent;
    FOnRestore: TsmxComponentEvent;
    FOnDoubleClick: TsmxComponentEvent;
    FOnClick: TsmxComponentEvent;
    FPopupMenu: TsmxCustomPopupMenu;
    function GetSlave(Index: Integer): TsmxControlCell;
    procedure SetSlave(Index: Integer; Value: TsmxControlCell);
  protected
    procedure ControlDblClick(Sender: TObject);
    procedure ControlClick(Sender: TObject);
    procedure DoBackup; virtual;
    procedure DoDoubleClick; virtual;
    procedure DoRestore; virtual;
    procedure DoClick; virtual;
    function GetCellActive: Boolean; virtual;
    function GetCellAlign: TAlign; virtual;
    function GetCellAnchors: TAnchors; virtual;
    function GetCellCursor: TCursor; virtual;
    function GetCellEnabled: Boolean; virtual;
    function GetCellHeight: Integer; virtual;
    function GetCellLeft: Integer; virtual;
    function GetCellTop: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    function GetCellWidth: Integer; virtual;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalBackup; virtual;
    procedure InternalDoubleClick; virtual;
    procedure InternalRestore; virtual;
    procedure InternalClick; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCellActive(Value: Boolean); virtual;
    procedure SetCellAlign(Value: TAlign); virtual;
    procedure SetCellAnchors(Value: TAnchors); virtual;
    procedure SetCellCursor(Value: TCursor); virtual;
    procedure SetCellEnabled(Value: Boolean); virtual;
    procedure SetCellHeight(Value: Integer); virtual;
    procedure SetCellLeft(Value: Integer); virtual;
    procedure SetCellTop(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;
    procedure SetCellWidth(Value: Integer); virtual;
    procedure SetPopupMenu(Value: TsmxCustomPopupMenu); virtual;
  public
    function AddSlave: TsmxControlCell;
    procedure Assign(Source: TPersistent); override;
    procedure Backup;
    procedure DoubleClick;
    procedure Restore;
    procedure Click;

    property CellActive: Boolean read GetCellActive write SetCellActive;
    property CellAlign: TAlign read GetCellAlign write SetCellAlign;
    property CellAnchors: TAnchors read GetCellAnchors write SetCellAnchors;
    property CellCursor: TCursor read GetCellCursor write SetCellCursor;
    property CellEnabled: Boolean read GetCellEnabled write SetCellEnabled;
    property CellHeight: Integer read GetCellHeight write SetCellHeight;
    property CellLeft: Integer read GetCellLeft write SetCellLeft;
    property CellTop: Integer read GetCellTop write SetCellTop;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    property CellWidth: Integer read GetCellWidth write SetCellWidth;
    property PopupMenu: TsmxCustomPopupMenu read FPopupMenu write SetPopupMenu;
    property Slaves[Index: Integer]: TsmxControlCell read GetSlave write SetSlave; default;

    property OnDoubleClick: TsmxComponentEvent read FOnDoubleClick write FOnDoubleClick;
    property OnClick: TsmxComponentEvent read FOnClick write FOnClick;
  published
    property OnBackup: TsmxComponentEvent read FOnBackup write FOnBackup;
    property OnRestore: TsmxComponentEvent read FOnRestore write FOnRestore;
  end;

  { TsmxActionCell }

  TsmxCustomAlgorithm = class;

  TsmxActionCell = class(TsmxControlCell)
  private
    FAlgorithm: TsmxCustomAlgorithm;
    FIsSetAlgorithmEvents: Boolean;
    FIsSetAlgorithmProps: Boolean;
  protected
    function GetCellCaption: TCaption; virtual;
    function GetCellHint: String; virtual;
    function GetCellImageIndex: TImageIndex; virtual;
    procedure InternalClick; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlgorithm(Value: TsmxCustomAlgorithm); virtual;
    procedure SetAlgorithmEvents(Algorithm: TsmxCustomAlgorithm); virtual;
    procedure SetAlgorithmProps(Algorithm: TsmxCustomAlgorithm); virtual;
    procedure SetCellCaption(const Value: TCaption); virtual;
    procedure SetCellHint(const Value: String); virtual;
    procedure SetCellImageIndex(Value: TImageIndex); virtual;
    procedure SetIsSetAlgorithmEvents(Value: Boolean); virtual;
    procedure SetIsSetAlgorithmProps(Value: Boolean); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property CellCaption: TCaption read GetCellCaption write SetCellCaption;
    property CellHint: String read GetCellHint write SetCellHint;
    property CellImageIndex: TImageIndex read GetCellImageIndex write SetCellImageIndex;
  published
    property Algorithm: TsmxCustomAlgorithm read FAlgorithm write SetAlgorithm;
    property IsSetAlgorithmEvents: Boolean read FIsSetAlgorithmEvents write SetIsSetAlgorithmEvents;
    property IsSetAlgorithmProps: Boolean read FIsSetAlgorithmProps write SetIsSetAlgorithmProps;
  end;

  { TsmxWorkCell }

  TsmxWorkCell = class(TsmxActionCell)
  private
    FOnApply: TsmxComponentEvent;
    FOnCancel: TsmxComponentEvent;
    FOnPrepare: TsmxComponentEvent;
    FOnRefresh: TsmxComponentEvent;
  protected
    procedure DoApply; virtual;
    procedure DoCancel; virtual;
    procedure DoPrepare; virtual;
    procedure DoRefresh; virtual;
    procedure InternalApply; virtual;
    procedure InternalCancel; virtual;
    procedure InternalPrepare; virtual;
    procedure InternalRefresh; virtual;
  public
    procedure Apply;
    procedure Cancel;
    procedure Prepare;
    procedure Refresh;
  published
    property OnApply: TsmxComponentEvent read FOnApply write FOnApply;
    property OnCancel: TsmxComponentEvent read FOnCancel write FOnCancel;
    property OnPrepare: TsmxComponentEvent read FOnPrepare write FOnPrepare;
    property OnRefresh: TsmxComponentEvent read FOnRefresh write FOnRefresh;
  end;

  { TsmxAlgorithmParam }

  TsmxAlgorithmParams = class;

  TsmxAlgorithmParam = class(TsmxParam)
  private
    FDataLocation: TsmxDataLocation;
    FDataType: TsmxDataType;
    FParamType: TsmxParamType;
    function GetKit: TsmxAlgorithmParams;
    procedure SetKit(Value: TsmxAlgorithmParams);
  protected
    procedure SetDataType(Value: TsmxDataType); virtual;
  public
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxAlgorithmParams read GetKit write SetKit;
  published
    property DataLocation: TsmxDataLocation read FDataLocation write FDataLocation;
    property DataType: TsmxDataType read FDataType write SetDataType;
    property ParamName;
    property ParamType: TsmxParamType read FParamType write FParamType;
    property ParamValue;
  end;

  { TsmxAlgorithmParams }

  TsmxAlgorithmParams = class(TsmxParams)
  private
    function GetItem(Index: Integer): TsmxAlgorithmParam;
    procedure SetItem(Index: Integer; Value: TsmxAlgorithmParam);
  public
    function Add: TsmxAlgorithmParam;

    property Items[Index: Integer]: TsmxAlgorithmParam read GetItem write SetItem; default;
  end;

  { TsmxCustomAlgorithm }

  TsmxCustomAlgorithmList = class;

  TsmxCustomAlgorithm = class(TsmxOwnerCell)
  private
    FAlgorithmParams: TsmxAlgorithmParams;
    FCellAction: TsmxBaseCell;
    FCellEvent: TsmxBaseCell;
    FIsManualRefreshParams: Boolean;
    FOnExecute: TsmxComponentEvent;
    FOnRefreshParams: TsmxComponentEvent;
    function GetAlgorithmParams: TsmxAlgorithmParams;
    function GetCellOwner: TsmxCustomAlgorithmList;
    procedure SetCellOwner(Value: TsmxCustomAlgorithmList);
  protected
    procedure DoExecute; virtual;
    procedure DoRefreshParams; virtual;
    function GetAlgorithmCaption: TCaption; virtual;
    function GetAlgorithmEnabled: Boolean; virtual;
    function GetAlgorithmHint: String; virtual;
    function GetAlgorithmHotKey: TShortCut; virtual;
    function GetAlgorithmImageIndex: TImageIndex; virtual;
    function GetAlgorithmVisible: Boolean; virtual;
    procedure InternalExecute; virtual;
    procedure InternalRefreshParams; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlgorithmCaption(const Value: TCaption); virtual;
    procedure SetAlgorithmEnabled(Value: Boolean); virtual;
    procedure SetAlgorithmHint(const Value: String); virtual;
    procedure SetAlgorithmHotKey(Value: TShortCut); virtual;
    procedure SetAlgorithmImageIndex(Value: TImageIndex); virtual;
    procedure SetAlgorithmParams(Value: TsmxAlgorithmParams); virtual;
    procedure SetAlgorithmVisible(Value: Boolean); virtual;
    procedure SetCellAction(Value: TsmxBaseCell); virtual;
    procedure SetCellEvent(Value: TsmxBaseCell); virtual;
    procedure SetIsManualRefreshParams(Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute;
    procedure RefreshParams;

    property AlgorithmCaption: TCaption read GetAlgorithmCaption write SetAlgorithmCaption;
    property AlgorithmEnabled: Boolean read GetAlgorithmEnabled write SetAlgorithmEnabled;
    property AlgorithmHint: String read GetAlgorithmHint write SetAlgorithmHint;
    property AlgorithmHotKey: TShortCut read GetAlgorithmHotKey write SetAlgorithmHotKey;
    property AlgorithmImageIndex: TImageIndex read GetAlgorithmImageIndex write SetAlgorithmImageIndex;
    property AlgorithmParams: TsmxAlgorithmParams read GetAlgorithmParams write SetAlgorithmParams;
    property AlgorithmVisible: Boolean read GetAlgorithmVisible write SetAlgorithmVisible;
    property CellAction: TsmxBaseCell read FCellAction write SetCellAction;
    property CellEvent: TsmxBaseCell read FCellEvent write SetCellEvent;
    property CellOwner: TsmxCustomAlgorithmList read GetCellOwner write SetCellOwner;
    property IsManualRefreshParams: Boolean read FIsManualRefreshParams write SetIsManualRefreshParams;

    property OnRefreshParams: TsmxComponentEvent read FOnRefreshParams write FOnRefreshParams;
  published
    procedure AlgorithmExecute(Sender: TsmxComponent); virtual;

    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
  end;

  { TsmxCustomAlgorithmList }

  TsmxCustomAlgorithmList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomAlgorithm;
    procedure SetSlave(Index: Integer; Value: TsmxCustomAlgorithm);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomAlgorithm;
    function GetAlgorithmParamValue(CfgID: Integer; const ParamName: String;
      var ParamValue: Variant): Boolean; virtual;

    property Slaves[Index: Integer]: TsmxCustomAlgorithm read GetSlave write SetSlave; default;
  end;

  { TsmxCustomRequest }

  TsmxCustomRequestList = class;

  TsmxCustomRequest = class(TsmxOwnerCell)
  private
    FCellRequest: TsmxBaseCell;
    FCurDataSetIntf: IsmxDataSet;
    FDatabaseIntf: IsmxDatabase;
    FDatabaseName: String;
    FDataSetIntf: IsmxDataSet;
    FIsManualRefreshParams: Boolean;
    FOnDelete: TsmxComponentEvent;
    FOnExecute: TsmxComponentEvent;
    FOnInsert: TsmxComponentEvent;
    FOnPrepare: TsmxComponentEvent;
    FOnRefreshParams: TsmxComponentEvent;
    FOnUpdate: TsmxComponentEvent;
    FOperationMode: TsmxOperationMode;
    FDeleteDataSetIntf: IsmxDataSet;
    FInsertDataSetIntf: IsmxDataSet;
    FUpdateDataSetIntf: IsmxDataSet;
    function GetCellOwner: TsmxCustomRequestList;
    procedure PerformRequest;
    procedure SetCellOwner(Value: TsmxCustomRequestList);
    function GetDataSet: IsmxDataSet;
  protected
    procedure DoDelete; virtual;
    procedure DoExecute; virtual;
    procedure DoInsert; virtual;
    procedure DoPrepare; virtual;
    procedure DoRefreshParams; virtual;
    procedure DoUpdate; virtual;
    function GetDataSetClass: TsmxInterfacedComponentClass; virtual;
    procedure InternalDelete; virtual;
    procedure InternalExecute; virtual;
    procedure InternalInsert; virtual;
    procedure InternalPrepare; virtual;
    procedure InternalRefreshParams; virtual;
    procedure InternalUpdate; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCellRequest(Value: TsmxBaseCell); virtual;
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetDatabaseName(const Value: String); virtual;
    procedure SetIsManualRefreshParams(Value: Boolean); virtual;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetOperationMode(Value: TsmxOperationMode); virtual;
    procedure SetModifyDataSet(Index: TsmxModifyRequest; const Value: IsmxDataSet); virtual;

    property CurDataSet: IsmxDataSet read FCurDataSetIntf write FCurDataSetIntf;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete;
    procedure Execute;
    procedure Insert;
    procedure Prepare;
    procedure RefreshParams;
    procedure Update;

    property CellOwner: TsmxCustomRequestList read GetCellOwner write SetCellOwner;
    property CellRequest: TsmxBaseCell read FCellRequest write SetCellRequest;
    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property DataSet: IsmxDataSet read GetDataSet;
    property IsManualRefreshParams: Boolean read FIsManualRefreshParams write SetIsManualRefreshParams;
    property OperationMode: TsmxOperationMode read FOperationMode write SetOperationMode {default omManual};
    property DeleteDataSet: IsmxDataSet index rtDelete read FDeleteDataSetIntf write SetModifyDataSet;
    property InsertDataSet: IsmxDataSet index rtInsert read FInsertDataSetIntf write SetModifyDataSet;
    property UpdateDataSet: IsmxDataSet index rtUpdate read FUpdateDataSetIntf write SetModifyDataSet;

    property OnDelete: TsmxComponentEvent read FOnDelete write FOnDelete;
    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnInsert: TsmxComponentEvent read FOnInsert write FOnInsert;
    property OnPrepare: TsmxComponentEvent read FOnPrepare write FOnPrepare;
    property OnRefreshParams: TsmxComponentEvent read FOnRefreshParams write FOnRefreshParams;
    property OnUpdate: TsmxComponentEvent read FOnUpdate write FOnUpdate;
  end;

  { TsmxCustomRequestList }

  TsmxCustomRequestList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomRequest;
    procedure SetSlave(Index: Integer; Value: TsmxCustomRequest);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomRequest;
    function GetRequestParamValue(CfgID: Integer; const ParamName: String;
      var ParamValue: Variant): Boolean; virtual;

    property Slaves[Index: Integer]: TsmxCustomRequest read GetSlave write SetSlave; default;
  end;

  { TsmxCustomColumn }

  TsmxCustomColumns = class;

  TsmxCustomColumn = class(TsmxControlCell)
  private
    FColumnOptions: TsmxColumnOptions;
    FOnClickHeader: TsmxComponentEvent;
    function GetCellOwner: TsmxCustomColumns;
    procedure SetCellOwner(Value: TsmxCustomColumns);
  protected
    procedure DoClickHeader; virtual;
    function GetColumnAlignment: TAlignment; virtual;
    function GetColumnCaption: String; virtual;
    function GetColumnColor: TColor; virtual;
    function GetColumnFont: TFont; virtual;
    function GetColumnValue: Variant; virtual;
    function GetHeaderAlignment: TAlignment; virtual;
    function GetHeaderColor: TColor; virtual;
    function GetHeaderFont: TFont; virtual;
    function GetHeaderCaption: String; virtual;
    procedure InternalClickHeader; virtual;
    procedure SetColumnAlignment(Value: TAlignment); virtual;
    procedure SetColumnCaption(const Value: String); virtual;
    procedure SetColumnColor(Value: TColor); virtual;
    procedure SetColumnFont(Value: TFont); virtual;
    procedure SetColumnOptions(Value: TsmxColumnOptions); virtual;
    procedure SetColumnValue(const Value: Variant); virtual;
    procedure SetHeaderAlignment(Value: TAlignment); virtual;
    procedure SetHeaderCaption(const Value: String); virtual;
    procedure SetHeaderColor(Value: TColor); virtual;
    procedure SetHeaderFont(Value: TFont); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ClickHeader;

    property CellOwner: TsmxCustomColumns read GetCellOwner write SetCellOwner;
    property ColumnAlignment: TAlignment read GetColumnAlignment write SetColumnAlignment;
    property ColumnCaption: String read GetColumnCaption write SetColumnCaption;
    property ColumnColor: TColor read GetColumnColor write SetColumnColor;
    property ColumnFont: TFont read GetColumnFont write SetColumnFont;
    property ColumnOptions: TsmxColumnOptions read FColumnOptions write SetColumnOptions;
    property ColumnValue: Variant read GetColumnValue write SetColumnValue;
    property HeaderAlignment: TAlignment read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderCaption: String read GetHeaderCaption write SetHeaderCaption;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;

    property OnClickHeader: TsmxComponentEvent read FOnClickHeader write FOnClickHeader;
  end;

  { TsmxCustomColumns }

  TsmxCustomColumns = class(TsmxWorkCell)
  private
    function GetSlave(Index: Integer): TsmxCustomColumn;
    procedure SetSlave(Index: Integer; Value: TsmxCustomColumn);
  protected
    function GetFocusedColIndex: Integer; virtual;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetFocusedColIndex(Value: Integer); virtual;
  public
    function AddSlave: TsmxCustomColumn;

    property FocusedColIndex: Integer read GetFocusedColIndex write SetFocusedColIndex;
    property Slaves[Index: Integer]: TsmxCustomColumn read GetSlave write SetSlave; default;
  end;

  { TsmxCustomGrid }

  TsmxCustomGrid = class(TsmxCustomColumns)
  private
    FGridOptions: TsmxGridOptions;
    FOnChangeRow: TsmxComponentEvent;
    FRequest: TsmxCustomRequest;
  protected
    procedure DoChangeRow; virtual;
    function GetFocusedRowIndex: Integer; virtual;
    function GetGridCaption(ColIndex, RowIndex: Integer): String; virtual;
    function GetGridValue(ColIndex, RowIndex: Integer): Variant; virtual;
    function GetRowCount: Integer; virtual;
    procedure InternalApply; override;
    procedure InternalCancel; override;
    procedure InternalChangeRow; virtual;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetFocusedRowIndex(Value: Integer); virtual;
    procedure SetGridCaption(ColIndex, RowIndex: Integer; const Value: String); virtual;
    procedure SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant); virtual;
    procedure SetGridOptions(Value: TsmxGridOptions); virtual;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
    procedure SetRowCount(Value: Integer); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeRow;

    property FocusedRowIndex: Integer read GetFocusedRowIndex write SetFocusedRowIndex;
    property GridCaptions[ColIndex, RowIndex: Integer]: String read GetGridCaption write SetGridCaption;
    property GridValues[ColIndex, RowIndex: Integer]: Variant read GetGridValue write SetGridValue;
    property GridOptions: TsmxGridOptions read FGridOptions write SetGridOptions;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property RowCount: Integer read GetRowCount write SetRowCount;

    property OnChangeRow: TsmxComponentEvent read FOnChangeRow write FOnChangeRow;
  end;

  { TsmxCustomTree }

  TsmxCustomTree = class(TsmxCustomColumns)
  private
    FOnChangeRow: TsmxComponentEvent;
    FOnCollapse: TsmxComponentEvent;
    FOnEdited: TsmxComponentEvent;
    FOnEditing: TsmxComponentEvent;
    FOnExpand: TsmxComponentEvent;
    FRequest: TsmxCustomRequest;
    FTreeOptions: TsmxTreeOptions;
  protected
    procedure DoChangeRow; virtual;
    procedure DoCollapse; virtual;
    procedure DoEdited; virtual;
    procedure DoEditing; virtual;
    procedure DoExpand; virtual;
    function GetEditor: IsmxTreeEditor; virtual;
    function GetExpanded(RowIndex: Pointer): Boolean; virtual;
    function GetFocusedRowIndex: Pointer; virtual;
    function GetParentRow(RowIndex: Pointer): Pointer; virtual;
    function GetRootRow: Pointer; virtual;
    function GetRow(RowIndex: Pointer; Index: Integer): Pointer; virtual;
    function GetRowCount(RowIndex: Pointer): Integer; virtual;
    function GetTreeCaption(ColIndex: Integer; RowIndex: Pointer): String; virtual;
    function GetTreeValue(ColIndex: Integer; RowIndex: Pointer): Variant; virtual;
    procedure InternalApply; override;
    procedure InternalCancel; override;
    procedure InternalChangeRow; virtual;
    procedure InternalCollapse; virtual;
    procedure InternalEdited; virtual;
    procedure InternalEditing; virtual;
    procedure InternalExpand; virtual;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetExpanded(RowIndex: Pointer; Value: Boolean); virtual;
    procedure SetFocusedRowIndex(Value: Pointer); virtual;
    procedure SetParentRow(RowIndex, Value: Pointer); virtual;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
    procedure SetRowCount(RowIndex: Pointer; Value: Integer); virtual;
    procedure SetTreeCaption(ColIndex: Integer; RowIndex: Pointer; const Value: String); virtual;
    procedure SetTreeOptions(Value: TsmxTreeOptions); virtual;
    procedure SetTreeValue(ColIndex: Integer; RowIndex: Pointer; const Value: Variant); virtual;
  public
    function AddRow(RowIndex: Pointer): Pointer; virtual;
    procedure ChangeRow;
    procedure Collapse;
    procedure DelRow(RowIndex: Pointer); virtual;
    procedure Edited;
    procedure Editing;
    procedure Expand;
    function RowLevel(RowIndex: Pointer): Integer; virtual;

    property Editor: IsmxTreeEditor read GetEditor;
    property Expanded[RowIndex: Pointer]: Boolean read GetExpanded write SetExpanded;
    property FocusedRowIndex: Pointer read GetFocusedRowIndex write SetFocusedRowIndex;
    property ParentRow[RowIndex: Pointer]: Pointer read GetParentRow write SetParentRow;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property RootRow: Pointer read GetRootRow;
    property RowCount[RowIndex: Pointer]: Integer read GetRowCount write SetRowCount;
    property Rows[RowIndex: Pointer; Index: Integer]: Pointer read GetRow;
    property TreeCaptions[ColIndex: Integer; RowIndex: Pointer]: String read GetTreeCaption write SetTreeCaption;
    property TreeOptions: TsmxTreeOptions read FTreeOptions write SetTreeOptions;
    property TreeValues[ColIndex: Integer; RowIndex: Pointer]: Variant read GetTreeValue write SetTreeValue;

    property OnChangeRow: TsmxComponentEvent read FOnChangeRow write FOnChangeRow;
    property OnCollapse: TsmxComponentEvent read FOnCollapse write FOnCollapse;
    property OnEdited: TsmxComponentEvent read FOnEdited write FOnEdited;
    property OnEditing: TsmxComponentEvent read FOnEditing write FOnEditing;
    property OnExpand: TsmxComponentEvent read FOnExpand write FOnExpand;
  end;

  { TsmxCustomFilter }

  TsmxCustomFilterDesk = class;

  TsmxCustomFilter = class(TsmxActionCell)
  private
    FFilterOptions: TsmxFilterOptions;
    FOnChangeFilter: TsmxComponentEvent;
    function GetCellOwner: TsmxCustomFilterDesk;
    procedure SetCellOwner(Value: TsmxCustomFilterDesk);
  protected
    procedure DoChangeFilter; virtual;
    function GetDisplayFormat: String; virtual;
    function GetFilterAlignment: TAlignment; virtual;
    function GetFilterColor: TColor; virtual;
    function GetFilterFont: TFont; virtual;
    function GetFilterValue: Variant; virtual;
    function GetHeaderAlignment: TAlignment; virtual;
    function GetFilterText: String; virtual;
    function GetHeaderColor: TColor; virtual;
    function GetHeaderFont: TFont; virtual;
    function GetValueFormat: String; virtual;
    procedure InternalChangeFilter; virtual;
    procedure SetDisplayFormat(const Value: String); virtual;
    procedure SetFilterAlignment(Value: TAlignment); virtual;
    procedure SetFilterColor(Value: TColor); virtual;
    procedure SetFilterFont(Value: TFont); virtual;
    procedure SetFilterOptions(Value: TsmxFilterOptions); virtual;
    procedure SetFilterValue(const Value: Variant); virtual;
    procedure SetHeaderAlignment(Value: TAlignment); virtual;
    procedure SetFilterText(const Value: String); virtual;
    procedure SetHeaderColor(Value: TColor); virtual;
    procedure SetHeaderFont(Value: TFont); virtual;
    procedure SetValueFormat(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeFilter;

    property CellOwner: TsmxCustomFilterDesk read GetCellOwner write SetCellOwner;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property FilterAlignment: TAlignment read GetFilterAlignment write SetFilterAlignment;
    property FilterText: String read GetFilterText write SetFilterText;
    property FilterColor: TColor read GetFilterColor write SetFilterColor;
    property FilterFont: TFont read GetFilterFont write SetFilterFont;
    property FilterOptions: TsmxFilterOptions read FFilterOptions write SetFilterOptions;
    property FilterValue: Variant read GetFilterValue write SetFilterValue;
    property HeaderAlignment: TAlignment read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderCaption: TCaption read GetCellCaption write SetCellCaption;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property ValueFormat: String read GetValueFormat write SetValueFormat;

    property OnChangeFilter: TsmxComponentEvent read FOnChangeFilter write FOnChangeFilter;
  end;

  { TsmxCustomFilterDesk }

  TsmxCustomFilterDesk = class(TsmxWorkCell)
  private
    FRequest: TsmxCustomRequest;
    function GetSlave(Index: Integer): TsmxCustomFilter;
    procedure SetSlave(Index: Integer; Value: TsmxCustomFilter);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalApply; override;
    procedure InternalCancel; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
  public
    function AddSlave: TsmxCustomFilter;

    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property Slaves[Index: Integer]: TsmxCustomFilter read GetSlave write SetSlave; default;
  end;

  { TsmxCustomSection }

  TsmxCustomPage = class;

  TsmxCustomSection = class(TsmxControlCell)
  private
    function GetCellOwner: TsmxCustomPage;
    procedure SetCellOwner(Value: TsmxCustomPage);
  public
    property CellOwner: TsmxCustomPage read GetCellOwner write SetCellOwner;
  end;

  { TsmxCustomPage }

  TsmxCustomPageManager = class;

  TsmxCustomPage = class(TsmxActionCell)
  private
    function GetCellOwner: TsmxCustomPageManager;
    function GetSlave(Index: Integer): TsmxCustomSection;
    procedure SetCellOwner(Value: TsmxCustomPageManager);
    procedure SetSlave(Index: Integer; Value: TsmxCustomSection);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomSection;

    property CellOwner: TsmxCustomPageManager read GetCellOwner write SetCellOwner;
    property Slaves[Index: Integer]: TsmxCustomSection read GetSlave write SetSlave; default;
  end;

  { TsmxCustomPageManager }

  TsmxCustomPageManager = class(TsmxControlCell)
  private
    FOnChangePage: TsmxComponentEvent;
    function GetSlave(Index: Integer): TsmxCustomPage;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPage);
  protected
    procedure DoChangePage; virtual;
    function GetActivePageIndex: Integer; virtual;
    function GetIsMultiLine: Boolean; virtual;
    function GetPageManagerStyle: TsmxPageManagerStyle; virtual;
    function GetPagePosition: TsmxPagePosition; virtual;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalChangePage; virtual;
    procedure SetActivePageIndex(Value: Integer); virtual;
    procedure SetIsMultiLine(Value: Boolean); virtual;
    procedure SetPageManagerStyle(Value: TsmxPageManagerStyle); virtual;
    procedure SetPagePosition(Value: TsmxPagePosition); virtual;
  public
    function AddSlave: TsmxCustomPage;
    procedure Assign(Source: TPersistent); override;
    procedure ChangePage;

    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property IsMultiLine: Boolean read GetIsMultiLine write SetIsMultiLine;
    property PageManagerStyle: TsmxPageManagerStyle read GetPageManagerStyle write SetPageManagerStyle;
    property PagePosition: TsmxPagePosition read GetPagePosition write SetPagePosition;
    property Slaves[Index: Integer]: TsmxCustomPage read GetSlave write SetSlave; default;

    property OnChangePage: TsmxComponentEvent read FOnChangePage write FOnChangePage;
  end;

  { TsmxCustomMenuItem }

  TsmxCustomMenuItem = class(TsmxActionCell)
  private
    function GetCellOwner: TsmxControlCell;
    function GetSlave(Index: Integer): TsmxCustomMenuItem;
    procedure SetCellOwner(Value: TsmxControlCell);
    procedure SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
  protected
    function GetIsChecked: Boolean; virtual;
    function GetMenuItemHotKey: Integer; virtual;
    function GetMenuItemStyle: TsmxMenuItemStyle; virtual;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetAlgorithm(Value: TsmxCustomAlgorithm); override;
    procedure SetIsChecked(Value: Boolean); virtual;
    procedure SetMenuItemHotKey(Value: Integer); virtual;
    procedure SetMenuItemStyle(Value: TsmxMenuItemStyle); virtual;
  public
    function AddSlave: TsmxCustomMenuItem;
    procedure Assign(Source: TPersistent); override;

    property CellOwner: TsmxControlCell read GetCellOwner write SetCellOwner;
    property IsChecked: Boolean read GetIsChecked write SetIsChecked;
    property MenuItemHotKey: Integer read GetMenuItemHotKey write SetMenuItemHotKey;
    property MenuItemStyle: TsmxMenuItemStyle read GetMenuItemStyle write SetMenuItemStyle;
    property Slaves[Index: Integer]: TsmxCustomMenuItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomMainMenu }

  TsmxCustomMainMenu = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomMenuItem;
    procedure SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomMenuItem;

    property Slaves[Index: Integer]: TsmxCustomMenuItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomPopupMenu }

  TsmxCustomPopupList = class;

  TsmxCustomPopupMenu = class(TsmxControlCell)
  private
    function GetCellOwner: TsmxCustomPopupList;
    function GetSlave(Index: Integer): TsmxCustomMenuItem;
    procedure SetCellOwner(Value: TsmxCustomPopupList);
    procedure SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomMenuItem;

    property CellOwner: TsmxCustomPopupList read GetCellOwner write SetCellOwner;
    property Slaves[Index: Integer]: TsmxCustomMenuItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomPopupList }

  TsmxCustomPopupList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomPopupMenu;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPopupMenu);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomPopupMenu;

    property Slaves[Index: Integer]: TsmxCustomPopupMenu read GetSlave write SetSlave; default;
  end;

  { TsmxCustomToolItem }

  TsmxCustomToolBoard = class;

  TsmxCustomToolItem = class(TsmxActionCell)
  private
    function GetCellOwner: TsmxCustomToolBoard;
    procedure SetCellOwner(Value: TsmxCustomToolBoard);
  protected
    function GetIsChecked: Boolean; virtual;
    function GetToolItemStyle: TsmxToolItemStyle; virtual;
    procedure SetIsChecked(Value: Boolean); virtual;
    procedure SetToolItemStyle(Value: TsmxToolItemStyle); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property CellOwner: TsmxCustomToolBoard read GetCellOwner write SetCellOwner;
    property IsChecked: Boolean read GetIsChecked write SetIsChecked;
    property ToolItemStyle: TsmxToolItemStyle read GetToolItemStyle write SetToolItemStyle;
  end;

  { TsmxCustomToolBoard }

  TsmxCustomControlBoard = class;

  TsmxCustomToolBoard = class(TsmxControlCell)
  private
    function GetCellOwner: TsmxCustomControlBoard;
    function GetSlave(Index: Integer): TsmxCustomToolItem;
    procedure SetCellOwner(Value: TsmxCustomControlBoard);
    procedure SetSlave(Index: Integer; Value: TsmxCustomToolItem);
  protected
    function GetIsFlat: Boolean; virtual;
    function GetIsShowCaptions: Boolean; virtual;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetIsFlat(Value: Boolean); virtual;
    procedure SetIsShowCaptions(Value: Boolean); virtual;
  public
    function AddSlave: TsmxCustomToolItem;
    procedure Assign(Source: TPersistent); override;

    property CellOwner: TsmxCustomControlBoard read GetCellOwner write SetCellOwner;
    property IsFlat: Boolean read GetIsFlat write SetIsFlat;
    property IsShowCaptions: Boolean read GetIsShowCaptions write SetIsShowCaptions;
    property Slaves[Index: Integer]: TsmxCustomToolItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomControlBoard }

  TsmxCustomControlBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomToolBoard;
    procedure SetSlave(Index: Integer; Value: TsmxCustomToolBoard);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomToolBoard;

    property Slaves[Index: Integer]: TsmxCustomToolBoard read GetSlave write SetSlave; default;
  end;

  { TsmxCustomStatusItem }

  TsmxCustomStatusBoard = class;

  TsmxCustomStatusItem = class(TsmxActionCell)
  private
    FOnDrawPanel: TsmxComponentEvent;
    function GetCellOwner: TsmxCustomStatusBoard;
    procedure SetCellOwner(Value: TsmxCustomStatusBoard);
  protected
    procedure DoDrawPanel; virtual;
    function GetStatusItemAlignment: TAlignment; virtual;
    function GetStatusItemStyle: TsmxStatusItemStyle; virtual;
    procedure InternalDrawPanel; virtual;
    procedure SetStatusItemAlignment(Value: TAlignment); virtual;
    procedure SetStatusItemStyle(Value: TsmxStatusItemStyle); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure DrawPanel;

    property CellOwner: TsmxCustomStatusBoard read GetCellOwner write SetCellOwner;
    property StatusItemAlignment: TAlignment read GetStatusItemAlignment write SetStatusItemAlignment;
    property StatusItemStyle: TsmxStatusItemStyle read GetStatusItemStyle write SetStatusItemStyle;

    property OnDrawPanel: TsmxComponentEvent read FOnDrawPanel write FOnDrawPanel;
  end;

  { TsmxCustomStatusBoard }

  TsmxCustomStatusBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomStatusItem;
    procedure SetSlave(Index: Integer; Value: TsmxCustomStatusItem);
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  public
    function AddSlave: TsmxCustomStatusItem;

    property Slaves[Index: Integer]: TsmxCustomStatusItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomForm }

  TsmxCustomForm = class(TsmxActionCell, IsmxFormControl)
  private
    FAlgorithmList: TsmxCustomAlgorithmList;
    FFormOptions: TsmxFormOptions;
    FID: Integer;
    FIntfID: Integer;
    FOnActivate: TsmxComponentEvent;
    FOnClose: TsmxComponentEvent;
    FOnDeactivate: TsmxComponentEvent;
    FOnShow: TsmxComponentEvent;
    FPopupList: TsmxCustomPopupList;
    FRequestList: TsmxCustomRequestList;
    function GetSlave(Index: Integer): TsmxCustomPageManager;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPageManager);
  protected
    procedure DoActivate; virtual;
    procedure DoClose; virtual;
    procedure DoDeactivate; virtual;
    procedure DoShow; virtual;
    function GetFormBorder: TsmxFormBorder; virtual;
    function GetFormPosition: TsmxFormPosition; virtual;
    function GetID: Integer;
    function GetIsMaximize: Boolean; virtual;
    function GetModalResult: TModalResult; virtual;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalActivate; virtual;
    procedure InternalClose; virtual;
    procedure InternalDeactivate; virtual;
    procedure InternalShow; virtual;
    function InternalShowModal: TModalResult; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetFormBorder(Value: TsmxFormBorder); virtual;
    procedure SetFormOptions(Value: TsmxFormOptions); virtual;
    procedure SetFormPosition(Value: TsmxFormPosition); virtual;
    procedure SetIntfID(Value: Integer); virtual;
    procedure SetIsMaximize(Value: Boolean); virtual;
    procedure SetModalResult(Value: TModalResult); virtual;
    procedure SetPopupList(Value: TsmxCustomPopupList); virtual;
    procedure SetRequestList(Value: TsmxCustomRequestList); virtual;
  public
    constructor Create(AOwner: TComponent; AID: Integer); reintroduce; overload; virtual;
    function AddSlave: TsmxCustomPageManager;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; override;
    procedure Activate;
    procedure Close;
    procedure Deactivate;
    procedure Show;
    function ShowModal: TModalResult;

    property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write SetAlgorithmList;
    property FormBorder: TsmxFormBorder read GetFormBorder write SetFormBorder;
    property FormOptions: TsmxFormOptions read FFormOptions write SetFormOptions;
    property FormPosition: TsmxFormPosition read GetFormPosition write SetFormPosition;
    property ID: Integer read GetID;
    property IntfID: Integer read FIntfID write SetIntfID;
    property IsMaximize: Boolean read GetIsMaximize write SetIsMaximize;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
    property PopupList: TsmxCustomPopupList read FPopupList write SetPopupList;
    property RequestList: TsmxCustomRequestList read FRequestList write SetRequestList;
    property Slaves[Index: Integer]: TsmxCustomPageManager read GetSlave write SetSlave; default;

    property OnActivate: TsmxComponentEvent read FOnActivate write FOnActivate;
    property OnClose: TsmxComponentEvent read FOnClose write FOnClose;
    property OnDeactivate: TsmxComponentEvent read FOnDeactivate write FOnDeactivate;
    property OnShow: TsmxComponentEvent read FOnShow write FOnShow;
  end;

  TsmxCustomFormClass = class of TsmxCustomForm;

implementation

uses
  DB, Variants, SysUtils, StrUtils, smxCfgs, smxConsts, smxDBTypes, smxProcs,
  smxFuncs, smxDBFuncs, smxClassProcs, smxClassFuncs;

{ EsmxCellError }

function EsmxCellError.GetCfgID: Integer;
begin
  if Component is TsmxBaseCell then
    Result := TsmxBaseCell(Component).CfgID
  else
    Result := 0;
end;

{ TsmxBaseCell }

constructor TsmxBaseCell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetCellFeedback;
  FIsRecieveCfg := True;
end;

destructor TsmxBaseCell.Destroy;
begin
  SetCellParent(nil);
  if Assigned(FCellList) then
  begin
    ClearCells;
    FCellList.Free;
  end;
  if Assigned(FCfg) then
    FCfg.Free;
  if Assigned(FEventParams) then
    FEventParams.Free;
  inherited Destroy;
end;

procedure TsmxBaseCell.Assign(Source: TPersistent);
begin
  if Source is TsmxBaseCell then
    CfgID := TsmxBaseCell(Source).CfgID
  else
    inherited Assign(Source);
end;

function TsmxBaseCell.CellParams(const Name: String; var Value: Variant): Boolean;
begin
  if SysUtils.AnsiCompareText(Name, 'CfgID') = 0 then
  begin
    Value := Cfg.CfgID;
    Result := True;
  end else
  begin
    Value := Variants.Null;
    Result := False;
  end;
end;

procedure TsmxBaseCell.ClearCells;
var
  i: Integer;
begin
  for i := CellList.Count - 1 downto 0 do
    TsmxBaseCell(CellList[i]).Free;
end;

procedure TsmxBaseCell.DoInitialize;
begin
  if Assigned(FOnInitialize) then
    FOnInitialize(Self);
end;

procedure TsmxBaseCell.InternalInitialize;
begin
  try
    if FIsRecieveCfg then
    begin
      Cfg.SelectDataSet := smxClassProcs.gSelectDataSetIntf;
      Cfg.Load;
    end;
    Cfg.Read;
  except
    on E: Exception do
      raise EsmxCellError.CreateByComponent(@smxConsts.rsCellIDActionErrorM,
        [ClassName, CfgID, 'initialize', E.Message], Self);
  end;
end;

procedure TsmxBaseCell.Initialize;
begin
  InternalInitialize;
  DoInitialize;
  Include(FCellStates, csInitialized);
end;

function TsmxBaseCell.FindChildByCfgID(CfgID: Integer): TsmxBaseCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to CellCount - 1 do
    if Cells[i].CfgID = CfgID then
    begin
      Result := Cells[i];
      Break;
    end;
end;

function TsmxBaseCell.FindChildByInternalRef(Ref: Pointer): TsmxBaseCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to CellCount - 1 do
    if Cells[i].GetInternalRef = Ref then
    begin
      Result := Cells[i];
      Break;
    end;
end;

function TsmxBaseCell.FindChildByName(const Name: String): TsmxBaseCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to CellCount - 1 do
    if SysUtils.AnsiCompareText(Cells[i].Name, Name) = 0 then
    begin
      Result := Cells[i];
      Break;
    end;
end;

function TsmxBaseCell.GetCell(Index: Integer): TsmxBaseCell;
begin
  Result := TsmxBaseCell(CellList[Index]);
end;

function TsmxBaseCell.GetCellCount: Integer;
begin
  Result := CellList.Count;
end;

function TsmxBaseCell.GetCellIndex: Integer;
begin
  if Assigned(FCellParent) then
    Result := FCellParent.CellList.IndexOf(Self)
  else
    Result := -1;
end;

function TsmxBaseCell.GetCellList: TList;
begin
  if not Assigned(FCellList) then
    FCellList := TList.Create;
  Result := FCellList;
end;

function TsmxBaseCell.GetCellRoot: TsmxBaseCell;
begin
  Result := Self;
  while Assigned(Result.FCellParent) do
    Result := Result.FCellParent;
end;

function TsmxBaseCell.GetCfg: TsmxBaseCfg;
begin
  if not Assigned(FCfg) then
    FCfg := GetCfgClass.Create(Self);
  Result := FCfg;
end;

function TsmxBaseCell.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxCellCfg;
end;

function TsmxBaseCell.GetCfgID: Integer;
begin
  Result := Cfg.CfgID;
end;

procedure TsmxBaseCell.SetCfgID(Value: Integer);
begin
  Cfg.CfgID := Value;
end;

function TsmxBaseCell.GetEventParams: TsmxParams;
begin
  if not Assigned(FEventParams) then
    FEventParams := TsmxParams.Create(TsmxParam);
  Result := FEventParams;
end;

procedure TsmxBaseCell.SetEventParams(Value: TsmxParams);
begin
  EventParams.Assign(Value);
end;

procedure TsmxBaseCell.GetProperties(DestCfg: TsmxBaseCfg);
var
  i: Integer;
  CurNode, n: IXMLNode;
  Cell: TsmxBaseCell;
begin
  if DestCfg is TsmxCellCfg then
  begin
    CurNode := TsmxCellCfg(DestCfg).CurNode;
    try
      CurNode.Attributes[smxConsts.cCfgIDAttributeName] := CfgID;
      CurNode.Attributes[smxConsts.cClassNameAttributeName] := smxFuncs.ResolvedClassType(TPersistentClass(ClassType));
      for i := 0 to CellCount - 1 do
      begin
        Cell := Cells[i];
        if IsStoredCell(Cell) then
        begin
          n := CurNode.AddChild(smxConsts.cCellNodeName);
          TsmxCellCfg(DestCfg).CurNode := n;
          TsmxCellCfg(DestCfg).WriteCell(Cell);
        end;
      end;
    finally
      TsmxCellCfg(DestCfg).CurNode := CurNode;
    end;
  end;
end;

procedure TsmxBaseCell.SetProperties(SrcCfg: TsmxBaseCfg);
var
  i: Integer;
  CurNode, n: IXMLNode;
  ClsName: String;
  Cell: TsmxBaseCell;
  CellClass: TsmxBaseCellClass;
begin
  ClearCells;
  if SrcCfg is TsmxCellCfg then
  begin
    CurNode := TsmxCellCfg(SrcCfg).CurNode;
    try
      if CurNode.HasAttribute(smxConsts.cCfgIDAttributeName) then
        CfgID := SysUtils.StrToIntDef(CurNode.Attributes[smxConsts.cCfgIDAttributeName], 0);
      for i := 0 to CurNode.ChildNodes.Count - 1 do
      begin
        n := CurNode.ChildNodes[i];
        if n.NodeName = smxConsts.cCellNodeName then
        begin
          if n.HasAttribute(smxConsts.cClassNameAttributeName) then
            ClsName := Variants.VarToStr(n.Attributes[smxConsts.cClassNameAttributeName])
          else
            ClsName := '';
          if ClsName <> '' then
            CellClass := TsmxBaseCellClass(smxFuncs.ResolvedClassTypeName(ClsName, True))
          else
            CellClass := nil;
          if Assigned(CellClass) then
          begin
            Cell := CellClass.Create(TsmxCellCfg(SrcCfg).CellOwner);
            Cell.CellParent := Self;
            TsmxCellCfg(SrcCfg).CurNode := n;
            TsmxCellCfg(SrcCfg).ReadCell(Cell);
          end else
            raise EsmxCellError.CreateByComponent(@smxConsts.rsCellActionError,
              [ClassName, 'set properties'], Self);
        end;
      end;
    finally
      TsmxCellCfg(SrcCfg).CurNode := CurNode;
    end;
  end;
end;

function TsmxBaseCell.IsStoredCell(Cell: TsmxBaseCell): Boolean;
begin
  Result := Cell.FCellParent = Self;
end;

procedure TsmxBaseCell.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = ImageList) and (Operation = opRemove) then
    ImageList := nil;
end;

procedure TsmxBaseCell.SetCellStateEventParam(Value: Boolean);
begin
  if Value and (csEventParam in FCellStates) then
    raise EsmxCellError.CreateByComponent(@smxConsts.rsCellIDActionError,
      [ClassName, CfgID, 'set state'], Self);
  if Value then
    Include(FCellStates, csEventParam)
  else
    Exclude(FCellStates, csEventParam);
end;

procedure TsmxBaseCell.SetCellFeedback;
begin
  if TObject(GetInternalRef) is TComponent then
    TComponent(GetInternalRef).Tag := Longint(Self);
end;

procedure TsmxBaseCell.SetCellParent(Value: TsmxBaseCell);
begin
  if Assigned(FCellParent) then
    FCellParent.CellList.Remove(Self);
  FCellParent := Value;
  if Assigned(FCellParent) then
  begin
    FCellParent.CellList.Add(Self);
    SetIsDesigning(FCellParent.FIsDesigning);
  end;
end;

procedure TsmxBaseCell.SetImageList(Value: TCustomImageList);
begin
  FImageList := Value;
  if Assigned(FImageList) then
    FImageList.FreeNotification(Self);
end;

procedure TsmxBaseCell.SetImageListName(const Value: String);
begin
  if FImageListName <> '' then
    ImageList := nil;
  FImageListName := Value;
  if FImageListName <> '' then
  begin
    if Assigned(smxProcs.gImageListManagerIntf) then
      ImageList := smxProcs.gImageListManagerIntf.ResolvedImageListName(FImageListName, True);
  end;
end;

procedure TsmxBaseCell.SetIsDesigning(Value: Boolean);
var
  i: Integer;
begin
  FIsDesigning := Value;
  if Assigned(FCellList) then
    for i := 0 to FCellList.Count - 1 do
      TsmxBaseCell(FCellList[i]).IsDesigning := FIsDesigning;
end;

procedure TsmxBaseCell.SetIsRecieveCfg(Value: Boolean);
begin
  FIsRecieveCfg := Value;
end;

procedure TsmxBaseCell.InternalFinalize;
begin
  try
    Cfg.Write;
    if FIsRecieveCfg then
    begin
      Cfg.InsertDataSet := smxClassProcs.gInsertDataSetIntf;
      Cfg.UpdateDataSet := smxClassProcs.gUpdateDataSetIntf;
      Cfg.Save;
    end;
  except
    on E: Exception do
      raise EsmxCellError.CreateByComponent(@smxConsts.rsCellIDActionErrorM,
        [ClassName, CfgID, 'finalize', E.Message], Self);
  end;
end;

procedure TsmxBaseCell.DoFinalize;
begin
  if Assigned(FOnFinalize) then
    FOnFinalize(Self);
end;

procedure TsmxBaseCell.Finalize;
begin
  InternalFinalize;
  DoFinalize;
  Include(FCellStates, csFinalized);
end;

{ TsmxSlaveItem }

function TsmxSlaveItem.GetDisplayName: String;
begin
  if Assigned(ObjectItem) then
    Result := Format('%s(%s) [%d]', [ObjectItem.Name, ObjectItem.ClassName, ObjectItem.CfgID])
  else
    Result := inherited GetDisplayName;
end;

function TsmxSlaveItem.GetObjectItem: TsmxOwnerCell;
begin
  Result := TsmxOwnerCell(inherited ObjectItem)
end;

procedure TsmxSlaveItem.SetObjectItem(Value: TsmxOwnerCell);
begin
  inherited ObjectItem := Value;
end;


function TsmxSlaveItem.GetKit: TsmxSlaveList;
begin
  Result := TsmxSlaveList(inherited Kit);
end;

procedure TsmxSlaveItem.SetKit(Value: TsmxSlaveList);
begin
  inherited Kit := Value;
end;

{ TsmxSlaveList }

function TsmxSlaveList.Add: TsmxSlaveItem;
begin
  Result := TsmxSlaveItem(inherited Add);
end;

function TsmxSlaveList.Add(ObjectClass: TPersistentClass): TsmxSlaveItem;
begin
  Result := TsmxSlaveItem(inherited Add(ObjectClass));
end;

function TsmxSlaveList.GetItem(Index: Integer): TsmxSlaveItem;
begin
  Result := TsmxSlaveItem(inherited Items[Index]);
end;

procedure TsmxSlaveList.SetItem(Index: Integer; Value: TsmxSlaveItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxOwnerCell }

constructor TsmxOwnerCell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsOwnerIsParent := True;
end;

destructor TsmxOwnerCell.Destroy;
begin
  SetCellOwner(nil);
  if Assigned(FSlaveItem) then
  begin
    FSlaveItem.FObjectItem := nil;
    FSlaveItem.Free;
  end;
  if Assigned(FSlaveList) then
    FSlaveList.Free;
  inherited Destroy;
end;

function TsmxOwnerCell.AddSlave: TsmxOwnerCell;
begin
  Result := SlaveList.Add.ObjectItem;
end;

function TsmxOwnerCell.AddSlaveAsClass(CellClass: TsmxBaseCellClass): TsmxOwnerCell;
begin
  Result := SlaveList.Add(CellClass).ObjectItem;
end;

procedure TsmxOwnerCell.DeleteSlave(Index: Integer);
begin
  SlaveList.Delete(Index);
end;

procedure TsmxOwnerCell.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxOwnerCell then
    SlaveList := TsmxOwnerCell(Source).SlaveList;
end;

procedure TsmxOwnerCell.ChangeObjectIndex(Value: Integer);
begin
end;

procedure TsmxOwnerCell.ChangeObjectOwner(Value: TPersistent);
begin
  if Assigned(FCellOwner) then
  begin
    if FIsOwnerIsParent then
      CellParent := nil;
  end;
  FCellOwner := Value as TsmxOwnerCell;
  if Assigned(FCellOwner) then
  begin
    if FIsOwnerIsParent then
      CellParent := FCellOwner;
  end;
end;

procedure TsmxOwnerCell.CheckObjectClass(ObjectClass: TPersistentClass);
var
  ObjectClassName: String;
begin
  if not Assigned(ObjectClass) or not ObjectClass.InheritsFrom(GetSlaveClass) then
  begin
    if Assigned(ObjectClass) then
      ObjectClassName := ObjectClass.ClassName else
      ObjectClassName := 'nil';
    raise EsmxCellError.CreateResFmt(@smxConsts.rsListItemClassError,
      [ObjectClassName, ClassName]);
  end;
end;

procedure TsmxOwnerCell.ClearSlaves;
begin
  SlaveList.Clear;
end;

procedure TsmxOwnerCell.CreateObject(Item: TObject);
begin
  CreateObject(Item, GetSlaveClass);
end;

procedure TsmxOwnerCell.CreateObject(Item: TObject; ObjectClass: TPersistentClass);
var
  Slave: TsmxOwnerCell;
begin
  CheckObjectClass(ObjectClass);
  if Item is TsmxSlaveItem then
    if not Assigned(TsmxSlaveItem(Item).FObjectItem) then
    begin
      Slave := TsmxOwnerCell(TsmxBaseCellClass(ObjectClass).Create(Owner));
      Slave.FSlaveItem := TsmxSlaveItem(Item);
      TsmxSlaveItem(Item).FObjectItem := Slave;
    end;
end;

procedure TsmxOwnerCell.DestroyObject(Item: TObject);
var
  Slave: TsmxOwnerCell;
begin
  if Item is TsmxSlaveItem then
    if Assigned(TsmxSlaveItem(Item).FObjectItem) then
    begin
      Slave := TsmxOwnerCell(TsmxSlaveItem(Item).FObjectItem);
      TsmxSlaveItem(Item).FObjectItem := nil;
      Slave.FSlaveItem := nil;
      Slave.Free;
    end;
end;

function TsmxOwnerCell.FindSlaveByCfgID(CfgID: Integer): TsmxOwnerCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
  begin
    if Slaves[i].CfgID = CfgID then
    begin
      Result := Slaves[i];
      Break;
    end;
  end;
end;

function TsmxOwnerCell.FindSlaveByName(const Name: String): TsmxOwnerCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
    if SysUtils.AnsiCompareText(Slaves[i].Name, Name) = 0 then
    begin
      Result := Slaves[i];
      Break;
    end;
end;

function TsmxOwnerCell.FindSlaveByInternalRef(Ref: Pointer): TsmxOwnerCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
    if Slaves[i].GetInternalRef = Ref then
    begin
      Result := Slaves[i];
      Break;
    end;
end;

procedure TsmxOwnerCell.GetProperties(DestCfg: TsmxBaseCfg);
var
  i: Integer;
  CurNode, n: IXMLNode;
  Cell: TsmxOwnerCell;
begin
  inherited GetProperties(DestCfg);
  if DestCfg is TsmxCellCfg then
  begin
    CurNode := TsmxCellCfg(DestCfg).CurNode;
    try
      for i := 0 to SlaveCount - 1 do
      begin
        Cell := Slaves[i];
        n := CurNode.AddChild(smxConsts.cSlaveNodeName);
        TsmxCellCfg(DestCfg).CurNode := n;
        TsmxCellCfg(DestCfg).WriteCell(Cell);
      end;
    finally
      TsmxCellCfg(DestCfg).CurNode := CurNode;
    end;
  end;
end;

procedure TsmxOwnerCell.SetProperties(SrcCfg: TsmxBaseCfg);
var
  i: Integer;
  CurNode, n: IXMLNode;
  ClsName: String;
  Cell: TsmxOwnerCell;
  CellClass: TsmxBaseCellClass;
begin
  inherited SetProperties(SrcCfg);
  ClearSlaves;
  if SrcCfg is TsmxCellCfg then
  begin
    CurNode := TsmxCellCfg(SrcCfg).CurNode;
    try
      for i := 0 to CurNode.ChildNodes.Count - 1 do
      begin
        n := CurNode.ChildNodes[i];
        if n.NodeName = smxConsts.cSlaveNodeName then
        begin
          if n.HasAttribute(smxConsts.cClassNameAttributeName) then
            ClsName := Variants.VarToStr(n.Attributes[smxConsts.cClassNameAttributeName])
          else
            ClsName := '';
          if ClsName <> '' then
            CellClass := TsmxBaseCellClass(smxFuncs.ResolvedClassTypeName(ClsName, True))
          else
            CellClass := nil;
          if Assigned(CellClass) then
          begin
            Cell := AddSlaveAsClass(CellClass);
            TsmxCellCfg(SrcCfg).CurNode := n;
            TsmxCellCfg(SrcCfg).ReadCell(Cell);
          end else
            raise EsmxCellError.CreateByComponent(@smxConsts.rsCellActionError,
              [ClassName, 'set properties'], Self);
        end;
      end;
    finally
      TsmxCellCfg(SrcCfg).CurNode := CurNode;
    end;
  end;
end;

function TsmxOwnerCell.GetSlave(Index: Integer): TsmxOwnerCell;
begin
  Result := SlaveList[Index].ObjectItem;
end;

procedure TsmxOwnerCell.SetSlave(Index: Integer; Value: TsmxOwnerCell);
begin
  SlaveList[Index].ObjectItem := Value;
end;

function TsmxOwnerCell.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxOwnerCell;
end;

function TsmxOwnerCell.GetSlaveCount: Integer;
begin
  Result := SlaveList.Count;
end;

function TsmxOwnerCell.GetSlaveIndex: Integer;
begin
  Result := SlaveItem.ItemIndex;
end;

procedure TsmxOwnerCell.SetSlaveIndex(Value: Integer);
begin
  SlaveItem.ItemIndex := Value;
end;

function TsmxOwnerCell.GetSlaveList: TsmxSlaveList;
begin
  if not Assigned(FSlaveList) then
    FSlaveList := TsmxSlaveList.Create(Self, TsmxSlaveItem);
  Result := FSlaveList;
end;

procedure TsmxOwnerCell.SetSlaveList(Value: TsmxSlaveList);
begin
  SlaveList.Assign(Value);
end;

function TsmxOwnerCell.GetSlaveItem: TsmxSlaveItem;
begin
  if not Assigned(FSlaveItem) then
  begin
    FSlaveItem := TsmxSlaveItem.Create(nil);
    FSlaveItem.FObjectItem := Pointer(Self as IsmxObjectItem);
  end;
  Result := FSlaveItem;
end;

procedure TsmxOwnerCell.SetCellOwner(Value: TsmxOwnerCell);
begin
  if Assigned(FCellOwner) then
    SlaveItem.Kit := nil;
  if Assigned(Value) then
    SlaveItem.Kit := Value.SlaveList;
end;

function TsmxOwnerCell.IsStoredCell(Cell: TsmxBaseCell): Boolean;
begin
  if (Cell is TsmxOwnerCell) and (TsmxOwnerCell(Cell).FCellOwner = Self) then
    Result := False
  else
    Result := inherited IsStoredCell(Cell);
end;

procedure TsmxOwnerCell.SetIsOwnerIsParent(Value: Boolean);
begin
  FIsOwnerIsParent := Value;
end;

procedure TsmxOwnerCell.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if Assigned(FCellOwner) then
    FCellOwner.SlaveList.Change;
end;

{ TsmxControlCell }

function TsmxControlCell.AddSlave: TsmxControlCell;
begin
  Result := TsmxControlCell(inherited AddSlave);
end;

procedure TsmxControlCell.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxControlCell then
  begin
    CellActive := TsmxControlCell(Source).CellActive;
    CellAlign := TsmxControlCell(Source).CellAlign;
    CellAnchors := TsmxControlCell(Source).CellAnchors;
    CellCursor := TsmxControlCell(Source).CellCursor;
    CellEnabled := TsmxControlCell(Source).CellEnabled;
    CellHeight := TsmxControlCell(Source).CellHeight;
    CellLeft := TsmxControlCell(Source).CellLeft;
    CellTop := TsmxControlCell(Source).CellTop;
    CellVisible := TsmxControlCell(Source).CellVisible;
    CellWidth := TsmxControlCell(Source).CellWidth;
  end;
end;

procedure TsmxControlCell.ControlClick(Sender: TObject);
begin
  Click;
end;

procedure TsmxControlCell.ControlDblClick(Sender: TObject);
begin
  DoubleClick;
end;

procedure TsmxControlCell.DoBackup;
begin
  if Assigned(FOnBackup) then
    FOnBackup(Self);
end;

procedure TsmxControlCell.InternalBackup;
begin
end;

procedure TsmxControlCell.Backup;
begin
  InternalBackup;
  DoBackup;
end;

procedure TsmxControlCell.DoDoubleClick;
begin
  if Assigned(FOnDoubleClick) then
    FOnDoubleClick(Self);
end;

procedure TsmxControlCell.InternalDoubleClick;
begin
end;

procedure TsmxControlCell.DoubleClick;
begin
  InternalDoubleClick;
  DoDoubleClick;
end;

procedure TsmxControlCell.DoRestore;
begin
  if Assigned(FOnRestore) then
    FOnRestore(Self);
end;

procedure TsmxControlCell.InternalRestore;
begin
end;

procedure TsmxControlCell.Restore;
begin
  InternalRestore;
  DoRestore;
end;

procedure TsmxControlCell.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TsmxControlCell.InternalClick;
begin
end;

procedure TsmxControlCell.Click;
begin
  InternalClick;
  DoClick;
end;

function TsmxControlCell.GetCellActive: Boolean;
begin
  if TObject(GetInternalRef) is TWinControl then
    Result := TWinControl(GetInternalRef).CanFocus
  else
    Result := False;
end;

procedure TsmxControlCell.SetCellActive(Value: Boolean);
begin
  if TObject(GetInternalRef) is TWinControl then
    if Value then
    begin
      if TWinControl(GetInternalRef).CanFocus then
        TWinControl(GetInternalRef).SetFocus;
    end else
    begin
      if CellRoot is TsmxControlCell then
        TsmxControlCell(CellRoot).CellActive := True;
    end;
end;

function TsmxControlCell.GetCellAlign: TAlign;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Align
  else
    Result := alNone;
end;

procedure TsmxControlCell.SetCellAlign(Value: TAlign);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Align := Value;
end;

function TsmxControlCell.GetCellAnchors: TAnchors;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Anchors
  else
    Result := [];
end;

procedure TsmxControlCell.SetCellAnchors(Value: TAnchors);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Anchors := Value;
end;

function TsmxControlCell.GetCellCursor: TCursor;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Cursor
  else
    Result := crDefault;
end;

procedure TsmxControlCell.SetCellCursor(Value: TCursor);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Cursor := Value;
end;

function TsmxControlCell.GetCellEnabled: Boolean;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Enabled
  else
    Result := False;
end;

procedure TsmxControlCell.SetCellEnabled(Value: Boolean);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Enabled := Value;
end;

function TsmxControlCell.GetCellHeight: Integer;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Height
  else
    Result := 0;
end;

procedure TsmxControlCell.SetCellHeight(Value: Integer);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Height := Value;
end;

function TsmxControlCell.GetCellLeft: Integer;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Left
  else
    Result := 0;
end;

procedure TsmxControlCell.SetCellLeft(Value: Integer);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Left := Value;
end;

function TsmxControlCell.GetCellTop: Integer;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Top
  else
    Result := 0;
end;

procedure TsmxControlCell.SetCellTop(Value: Integer);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Top := Value;
end;

function TsmxControlCell.GetCellVisible: Boolean;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Visible
  else
    Result := False;
end;

procedure TsmxControlCell.SetCellVisible(Value: Boolean);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Visible := Value;
end;

function TsmxControlCell.GetCellWidth: Integer;
begin
  if TObject(GetInternalRef) is TControl then
    Result := TControl(GetInternalRef).Width
  else
    Result := 0;
end;

procedure TsmxControlCell.SetCellWidth(Value: Integer);
begin
  if TObject(GetInternalRef) is TControl then
    TControl(GetInternalRef).Width := Value;
end;

function TsmxControlCell.GetSlave(Index: Integer): TsmxControlCell;
begin
  Result := TsmxControlCell(inherited Slaves[Index]);
end;

procedure TsmxControlCell.SetSlave(Index: Integer; Value: TsmxControlCell);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxControlCell.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxControlCell;
end;

procedure TsmxControlCell.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = PopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TsmxControlCell.SetPopupMenu(Value: TsmxCustomPopupMenu);
begin
  FPopupMenu := Value;
  if Assigned(FPopupMenu) then
    FPopupMenu.FreeNotification(Self);
end;

{ TsmxActionCell }

procedure TsmxActionCell.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxActionCell then
  begin
    CellCaption := TsmxActionCell(Source).CellCaption;
    CellHint := TsmxActionCell(Source).CellHint;
    CellImageIndex := TsmxActionCell(Source).CellImageIndex;
    IsSetAlgorithmEvents := TsmxActionCell(Source).IsSetAlgorithmEvents;
    IsSetAlgorithmProps := TsmxActionCell(Source).IsSetAlgorithmProps;
  end;
end;

function TsmxActionCell.GetCellCaption: TCaption;
begin
  Result := '';
end;

procedure TsmxActionCell.SetCellCaption(const Value: TCaption);
begin
end;

function TsmxActionCell.GetCellHint: String;
begin
  Result := '';
end;

procedure TsmxActionCell.SetCellHint(const Value: String);
begin
end;

function TsmxActionCell.GetCellImageIndex: TImageIndex;
begin
  Result := -1;
end;

procedure TsmxActionCell.SetCellImageIndex(Value: TImageIndex);
begin
end;

procedure TsmxActionCell.InternalClick;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.CellAction := Self;
end;

procedure TsmxActionCell.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Algorithm) and (Operation = opRemove) then
    Algorithm := nil;
end;

procedure TsmxActionCell.SetAlgorithm(Value: TsmxCustomAlgorithm);
begin
  FAlgorithm := Value;
  if Assigned(FAlgorithm) then
  begin
    if FIsSetAlgorithmProps then
      SetAlgorithmProps(FAlgorithm);
    if FIsSetAlgorithmEvents then
      SetAlgorithmEvents(FAlgorithm);
    FAlgorithm.FreeNotification(Self);
  end;
end;

procedure TsmxActionCell.SetAlgorithmEvents(Algorithm: TsmxCustomAlgorithm);
begin
  if Assigned(Algorithm) then
    OnClick := Algorithm.OnExecute;
end;

procedure TsmxActionCell.SetAlgorithmProps(Algorithm: TsmxCustomAlgorithm);
begin
  if Assigned(Algorithm) then
  begin
    CellCaption := Algorithm.AlgorithmCaption;
    CellHint := Algorithm.AlgorithmHint;
    CellImageIndex := Algorithm.AlgorithmImageIndex;
  end;
end;

procedure TsmxActionCell.SetIsSetAlgorithmEvents(Value: Boolean);
begin
  if FIsSetAlgorithmEvents <> Value then
  begin
    FIsSetAlgorithmEvents := Value;
    if Value then
      SetAlgorithmEvents(FAlgorithm);
  end;
end;

procedure TsmxActionCell.SetIsSetAlgorithmProps(Value: Boolean);
begin
  if FIsSetAlgorithmProps <> Value then
  begin
    FIsSetAlgorithmProps := Value;
    if Value then
      SetAlgorithmProps(FAlgorithm);
  end;
end;

{ TsmxWorkCell }

procedure TsmxWorkCell.DoApply;
begin
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TsmxWorkCell.InternalApply;
begin
end;

procedure TsmxWorkCell.Apply;
begin
  InternalApply;
  DoApply;
end;

procedure TsmxWorkCell.DoCancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TsmxWorkCell.InternalCancel;
begin
end;

procedure TsmxWorkCell.Cancel;
begin
  InternalCancel;
  DoCancel;
end;

procedure TsmxWorkCell.DoPrepare;
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
end;

procedure TsmxWorkCell.InternalPrepare;
begin
end;

procedure TsmxWorkCell.Prepare;
begin
  InternalPrepare;
  DoPrepare;
end;

procedure TsmxWorkCell.DoRefresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TsmxWorkCell.InternalRefresh;
begin
end;

procedure TsmxWorkCell.Refresh;
begin
  InternalRefresh;
  DoRefresh;
end;

{ TsmxAlgorithmParam }

procedure TsmxAlgorithmParam.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmParam then
  begin
    DataLocation := TsmxAlgorithmParam(Source).DataLocation;
    DataType := TsmxAlgorithmParam(Source).DataType;
    ParamType := TsmxAlgorithmParam(Source).ParamType;
  end;
end;

function TsmxAlgorithmParam.GetKit: TsmxAlgorithmParams;
begin
  Result := TsmxAlgorithmParams(inherited Kit);
end;

procedure TsmxAlgorithmParam.SetKit(Value: TsmxAlgorithmParams);
begin
  inherited Kit := Value;
end;

procedure TsmxAlgorithmParam.SetDataType(Value: TsmxDataType);
var
  vType: Integer;
  Temp: Variant;
begin
  FDataType := Value;
  vType := smxDBFuncs.DataTypeToVarType(FDataType);
  if vType <> varError then
    try
      Temp := ParamValue;
      VarCast(Temp, Temp, vType);
      ParamValue := Temp;
    except
      ParamValue := Variants.Null;
    end
  else
    ParamValue := Variants.Null;
end;

{ TsmxAlgorithmParams }

function TsmxAlgorithmParams.Add: TsmxAlgorithmParam;
begin
  Result := TsmxAlgorithmParam(inherited Add);
end;

function TsmxAlgorithmParams.GetItem(Index: Integer): TsmxAlgorithmParam;
begin
  Result := TsmxAlgorithmParam(inherited Items[Index]);
end;

procedure TsmxAlgorithmParams.SetItem(Index: Integer; Value: TsmxAlgorithmParam);
begin
  inherited Items[Index] := Value;
end;

{ TsmxCustomAlgorithm }

constructor TsmxCustomAlgorithm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnExecute := AlgorithmExecute;
end;

destructor TsmxCustomAlgorithm.Destroy;
begin
  if Assigned(FAlgorithmParams) then
    FAlgorithmParams.Free;
  inherited Destroy;
end;

procedure TsmxCustomAlgorithm.AlgorithmExecute(Sender: TsmxComponent);
begin
end;

procedure TsmxCustomAlgorithm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomAlgorithm then
  begin
    AlgorithmCaption := TsmxCustomAlgorithm(Source).AlgorithmCaption;
    AlgorithmHint := TsmxCustomAlgorithm(Source).AlgorithmHint;
    AlgorithmHotKey := TsmxCustomAlgorithm(Source).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxCustomAlgorithm(Source).AlgorithmImageIndex;
    AlgorithmParams := TsmxCustomAlgorithm(Source).AlgorithmParams;
    AlgorithmVisible := TsmxCustomAlgorithm(Source).AlgorithmVisible;
  end;
end;

procedure TsmxCustomAlgorithm.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TsmxCustomAlgorithm.InternalExecute;
begin
end;

procedure TsmxCustomAlgorithm.Execute;
begin
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
  InternalExecute;
  DoExecute;
end;

procedure TsmxCustomAlgorithm.DoRefreshParams;
begin
  if Assigned(FOnRefreshParams) then
    FOnRefreshParams(Self);
end;

procedure TsmxCustomAlgorithm.InternalRefreshParams;
begin
end;

procedure TsmxCustomAlgorithm.RefreshParams;
begin
  InternalRefreshParams;
  DoRefreshParams;
end;

function TsmxCustomAlgorithm.GetAlgorithmCaption: TCaption;
begin
  Result := '';
end;

procedure TsmxCustomAlgorithm.SetAlgorithmCaption(const Value: TCaption);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmEnabled: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmEnabled(Value: Boolean);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmHotKey: TShortCut;
begin
  Result := 0;
end;

function TsmxCustomAlgorithm.GetAlgorithmHint: String;
begin
  Result := '';
end;

procedure TsmxCustomAlgorithm.SetAlgorithmHint(const Value: String);
begin
end;

procedure TsmxCustomAlgorithm.SetAlgorithmHotKey(Value: TShortCut);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmImageIndex: TImageIndex;
begin
  Result := -1;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmImageIndex(Value: TImageIndex);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmParams: TsmxAlgorithmParams;
begin
  if not Assigned(FAlgorithmParams) then
    FAlgorithmParams := TsmxAlgorithmParams.Create(TsmxAlgorithmParam);
  Result := FAlgorithmParams;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmParams(Value: TsmxAlgorithmParams);
begin
  AlgorithmParams.Assign(Value);
end;

function TsmxCustomAlgorithm.GetAlgorithmVisible: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmVisible(Value: Boolean);
begin
end;

function TsmxCustomAlgorithm.GetCellOwner: TsmxCustomAlgorithmList;
begin
  Result := TsmxCustomAlgorithmList(inherited CellOwner);
end;

procedure TsmxCustomAlgorithm.SetCellOwner(Value: TsmxCustomAlgorithmList);
begin
  inherited CellOwner := Value;
end;

procedure TsmxCustomAlgorithm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = CellAction then
      CellAction := nil else
    if AComponent = CellEvent then
      CellEvent := nil;
  end;
end;

procedure TsmxCustomAlgorithm.SetCellAction(Value: TsmxBaseCell);
begin
  FCellAction := Value;
  if Assigned(FCellAction) then
    FCellAction.FreeNotification(Self);
end;

procedure TsmxCustomAlgorithm.SetCellEvent(Value: TsmxBaseCell);
begin
  FCellEvent := Value;
  if Assigned(FCellEvent) then
    FCellEvent.FreeNotification(Self);
end;

procedure TsmxCustomAlgorithm.SetIsManualRefreshParams(Value: Boolean);
begin
  FIsManualRefreshParams := Value;
end;

{ TsmxCustomAlgorithmList }

function TsmxCustomAlgorithmList.AddSlave: TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(inherited AddSlave);
end;

function TsmxCustomAlgorithmList.GetAlgorithmParamValue(CfgID: Integer;
  const ParamName: String; var ParamValue: Variant): Boolean;
begin
  ParamValue := Variants.Null;
  Result := False;
end;

function TsmxCustomAlgorithmList.GetSlave(Index: Integer): TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(inherited Slaves[Index]);
end;

procedure TsmxCustomAlgorithmList.SetSlave(Index: Integer; Value: TsmxCustomAlgorithm);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomAlgorithmList.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomAlgorithm;
end;

{ TsmxCustomRequest }

destructor TsmxCustomRequest.Destroy;
begin
  SetDatabase(nil);
  SetModifyDataSet(rtDelete, nil);
  SetModifyDataSet(rtInsert, nil);
  SetModifyDataSet(rtUpdate, nil);
  FCurDataSetIntf := nil;
  FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxCustomRequest.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomRequest then
  begin
    DatabaseName := TsmxCustomRequest(Source).DatabaseName;
    IsManualRefreshParams := TsmxCustomRequest(Source).IsManualRefreshParams;
    OperationMode := TsmxCustomRequest(Source).OperationMode;
  end;
end;

procedure TsmxCustomRequest.DoDelete;
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self);
end;

procedure TsmxCustomRequest.InternalDelete;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.Delete;
begin
  FCurDataSetIntf := FDeleteDataSetIntf;
  InternalPrepare;
  DoPrepare;
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
  InternalDelete;
  DoDelete;
end;

procedure TsmxCustomRequest.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TsmxCustomRequest.InternalExecute;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.Execute;
begin
  FCurDataSetIntf := DataSet;
  InternalPrepare;
  DoPrepare;
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
  InternalExecute;
  DoExecute;
end;

procedure TsmxCustomRequest.DoInsert;
begin
  if Assigned(FOnInsert) then
    FOnInsert(Self);
end;

procedure TsmxCustomRequest.InternalInsert;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.Insert;
begin
  FCurDataSetIntf := FInsertDataSetIntf;
  InternalPrepare;
  DoPrepare;
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
  InternalInsert;
  DoInsert;
end;

procedure TsmxCustomRequest.DoPrepare;
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
end;

procedure TsmxCustomRequest.InternalPrepare;
begin
  if Assigned(FCurDataSetIntf) then
  begin
    if not Assigned(FCurDataSetIntf.Database) then
      FCurDataSetIntf.Database := FDatabaseIntf;
    if not FCurDataSetIntf.Prepared then
      FCurDataSetIntf.Prepared := True;
  end;
end;

procedure TsmxCustomRequest.Prepare;
begin
  FCurDataSetIntf := DataSet;
  InternalPrepare;
  DoPrepare;
  if FOperationMode = omAutomatic then
  begin
    if not FIsManualRefreshParams then
    begin
      InternalRefreshParams;
      DoRefreshParams;
    end;
    InternalExecute;
    DoExecute;
  end;
end;

procedure TsmxCustomRequest.DoRefreshParams;
begin
  if Assigned(FOnRefreshParams) then
    FOnRefreshParams(Self);
end;

procedure TsmxCustomRequest.InternalRefreshParams;
begin
end;

procedure TsmxCustomRequest.RefreshParams;
begin
  FCurDataSetIntf := DataSet;
  InternalRefreshParams;
  DoRefreshParams;
end;

procedure TsmxCustomRequest.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TsmxCustomRequest.InternalUpdate;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.Update;
begin
  FCurDataSetIntf := FUpdateDataSetIntf;
  InternalPrepare;
  DoPrepare;
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
  InternalUpdate;
  DoUpdate;
end;

function TsmxCustomRequest.GetCellOwner: TsmxCustomRequestList;
begin
  Result := TsmxCustomRequestList(inherited CellOwner);
end;

procedure TsmxCustomRequest.SetCellOwner(Value: TsmxCustomRequestList);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomRequest.GetDataSet: IsmxDataSet;
begin
  if not Assigned(FDataSetIntf) then
  begin
    FDataSetIntf := GetDataSetClass.Create(nil) as IsmxDataSet;
    FDataSetIntf.GetReference.Name :=
      StrUtils.IfThen(Name <> '', Format('%s_%s', [Name, 'DataSet']), FDataSetIntf.GetReference.Name);
  end;
  Result := FDataSetIntf;
end;

function TsmxCustomRequest.GetDataSetClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxInterfacedComponent;
end;

procedure TsmxCustomRequest.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = CellRequest then
      CellRequest := nil;
    if Assigned(Database) and not Database.IsCountedObj and (AComponent = Database.GetReference) then
      Database := nil else
    if Assigned(DeleteDataSet) and not DeleteDataSet.IsCountedObj and (AComponent = DeleteDataSet.GetReference) then
      DeleteDataSet := nil else
    if Assigned(InsertDataSet) and not InsertDataSet.IsCountedObj and (AComponent = InsertDataSet.GetReference) then
      InsertDataSet := nil else
    if Assigned(UpdateDataSet) and not UpdateDataSet.IsCountedObj and (AComponent = UpdateDataSet.GetReference) then
      UpdateDataSet := nil;
  end;
end;

procedure TsmxCustomRequest.PerformRequest;
begin
  if Assigned(FCurDataSetIntf) then
  begin
    try
      FCurDataSetIntf.Close;
      FCurDataSetIntf.Perform;
    except
      on E: Exception do
        raise EsmxCellError.CreateByComponent(@smxConsts.rsCellIDActionErrorM,
          [ClassName, 'execute', CfgID, E.Message], Self);
    end;
  end;
end;

procedure TsmxCustomRequest.SetCellRequest(Value: TsmxBaseCell);
begin
  FCellRequest := Value;
  if Assigned(FCellRequest) then
    FCellRequest.FreeNotification(Self);
end;

procedure TsmxCustomRequest.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
  if Assigned(FDatabaseIntf) and not FDatabaseIntf.IsCountedObj then
    FDatabaseIntf.GetReference.FreeNotification(Self);
end;

procedure TsmxCustomRequest.SetDatabaseName(const Value: String);
var
  DataEntity: IsmxDataEntity;
  ADatabase: IsmxDatabase;
begin
  if Assigned(smxProcs.gDatabaseManagerIntf) and (FDatabaseName <> '') then
    Database := nil;
  FDatabaseName := Value;
  if Assigned(smxProcs.gDatabaseManagerIntf) and (FDatabaseName <> '') then
  begin
    DataEntity := smxProcs.gDatabaseManagerIntf.FindByName(FDatabaseName);
    if SysUtils.Supports(DataEntity, IsmxDatabase, ADatabase) then
      Database := ADatabase;
  end;
end;

procedure TsmxCustomRequest.SetIsManualRefreshParams(Value: Boolean);
begin
  FIsManualRefreshParams := Value;
end;

procedure TsmxCustomRequest.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if Assigned(FDataSetIntf) then
    FDataSetIntf.GetReference.Name :=
      StrUtils.IfThen(Name <> '', Format('%s_%s', [Name, 'DataSet']), FDataSetIntf.GetReference.Name);
end;

procedure TsmxCustomRequest.SetOperationMode(Value: TsmxOperationMode);
begin
  FOperationMode := Value;
end;

procedure TsmxCustomRequest.SetModifyDataSet(Index: TsmxModifyRequest; const Value: IsmxDataSet);
begin
  case Index of
    rtDelete: FDeleteDataSetIntf := Value;
    rtInsert: FInsertDataSetIntf := Value;
    rtUpdate: FUpdateDataSetIntf := Value;
  end;
  if Assigned(Value) and not Value.IsCountedObj then
    Value.GetReference.FreeNotification(Self);
end;

{ TsmxCustomRequestList }

function TsmxCustomRequestList.AddSlave: TsmxCustomRequest;
begin
  Result := TsmxCustomRequest(inherited AddSlave);
end;

function TsmxCustomRequestList.GetSlave(Index: Integer): TsmxCustomRequest;
begin
  Result := TsmxCustomRequest(inherited Slaves[Index]);
end;

procedure TsmxCustomRequestList.SetSlave(Index: Integer; Value: TsmxCustomRequest);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomRequestList.GetRequestParamValue(CfgID: Integer;
  const ParamName: String; var ParamValue: Variant): Boolean;
begin
  ParamValue := Variants.Null;
  Result := False;
end;

function TsmxCustomRequestList.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomRequest;
end;

{ TsmxCustomColumn }

procedure TsmxCustomColumn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomColumn then
  begin
    ColumnAlignment := TsmxCustomColumn(Source).ColumnAlignment;
    ColumnColor := TsmxCustomColumn(Source).ColumnColor;
    ColumnFont := TsmxCustomColumn(Source).ColumnFont;
    ColumnCaption := TsmxCustomColumn(Source).ColumnCaption;
    ColumnOptions := TsmxCustomColumn(Source).ColumnOptions;
    ColumnValue := TsmxCustomColumn(Source).ColumnValue;
    HeaderAlignment := TsmxCustomColumn(Source).HeaderAlignment;
    HeaderColor := TsmxCustomColumn(Source).HeaderColor;
    HeaderFont := TsmxCustomColumn(Source).HeaderFont;
    HeaderCaption := TsmxCustomColumn(Source).HeaderCaption;
  end;
end;

procedure TsmxCustomColumn.DoClickHeader;
begin
  if Assigned(FOnClickHeader) then
    FOnClickHeader(Self);
end;

procedure TsmxCustomColumn.InternalClickHeader;
begin
end;

procedure TsmxCustomColumn.ClickHeader;
begin
  InternalClickHeader;
  DoClickHeader;
end;

function TsmxCustomColumn.GetCellOwner: TsmxCustomColumns;
begin
  Result := TsmxCustomColumns(inherited CellOwner);
end;

procedure TsmxCustomColumn.SetCellOwner(Value: TsmxCustomColumns);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomColumn.GetColumnAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TsmxCustomColumn.SetColumnAlignment(Value: TAlignment);
begin
end;

function TsmxCustomColumn.GetColumnColor: TColor;
begin
  Result := Graphics.clBlack;
end;

procedure TsmxCustomColumn.SetColumnColor(Value: TColor);
begin
end;

function TsmxCustomColumn.GetColumnFont: TFont;
begin
  Result := nil;
end;

procedure TsmxCustomColumn.SetColumnFont(Value: TFont);
begin
end;

function TsmxCustomColumn.GetColumnCaption: String;
begin
  Result := '';
end;

procedure TsmxCustomColumn.SetColumnCaption(const Value: String);
begin
end;

procedure TsmxCustomColumn.SetColumnOptions(Value: TsmxColumnOptions);
begin
  FColumnOptions := Value;
end;

function TsmxCustomColumn.GetColumnValue: Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomColumn.SetColumnValue(const Value: Variant);
begin
end;

function TsmxCustomColumn.GetHeaderAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TsmxCustomColumn.SetHeaderAlignment(Value: TAlignment);
begin
end;

function TsmxCustomColumn.GetHeaderCaption: String;
begin
  Result := '';
end;

procedure TsmxCustomColumn.SetHeaderCaption(const Value: String);
begin
end;

function TsmxCustomColumn.GetHeaderColor: TColor;
begin
  Result := Graphics.clBlack;
end;

procedure TsmxCustomColumn.SetHeaderColor(Value: TColor);
begin
end;

function TsmxCustomColumn.GetHeaderFont: TFont;
begin
  Result := nil;
end;

procedure TsmxCustomColumn.SetHeaderFont(Value: TFont);
begin
end;

{ TsmxCustomColumns }

function TsmxCustomColumns.AddSlave: TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(inherited AddSlave);
end;

function TsmxCustomColumns.GetFocusedColIndex: Integer;
begin
  Result := -1;
end;

procedure TsmxCustomColumns.SetFocusedColIndex(Value: Integer);
begin
end;

function TsmxCustomColumns.GetSlave(Index: Integer): TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(inherited Slaves[Index]);
end;

procedure TsmxCustomColumns.SetSlave(Index: Integer; Value: TsmxCustomColumn);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomColumns.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomColumn;
end;

{ TsmxCustomGrid }

procedure TsmxCustomGrid.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomGrid then
    GridOptions := TsmxCustomGrid(Source).GridOptions;
end;

procedure TsmxCustomGrid.DoChangeRow;
begin
  if Assigned(FOnChangeRow) then
    FOnChangeRow(Self);
end;

procedure TsmxCustomGrid.InternalChangeRow;
begin
end;

procedure TsmxCustomGrid.ChangeRow;
begin
  InternalChangeRow;
  DoChangeRow;
end;

function TsmxCustomGrid.GetFocusedRowIndex: Integer;
begin
  Result := -1;
end;

procedure TsmxCustomGrid.SetFocusedRowIndex(Value: Integer);
begin
end;

function TsmxCustomGrid.GetGridCaption(ColIndex, RowIndex: Integer): String;
begin
  Result := '';
end;

procedure TsmxCustomGrid.SetGridCaption(ColIndex, RowIndex: Integer; const Value: String);
begin
end;

function TsmxCustomGrid.GetGridValue(ColIndex, RowIndex: Integer): Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomGrid.SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant);
begin
end;

function TsmxCustomGrid.GetRowCount: Integer;
begin
  Result := 0;
end;

procedure TsmxCustomGrid.SetRowCount(Value: Integer);
begin
end;

procedure TsmxCustomGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Request) and (Operation = opRemove) then
    Request := nil;
end;

procedure TsmxCustomGrid.InternalApply;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomGrid.InternalCancel;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomGrid.InternalPrepare;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomGrid.InternalRefresh;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomGrid.SetGridOptions(Value: TsmxGridOptions);
begin
  FGridOptions := Value;
end;

procedure TsmxCustomGrid.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
  if Assigned(FRequest) then
    FRequest.FreeNotification(Self);
end;

{ TsmxCustomTree }

function TsmxCustomTree.AddRow(RowIndex: Pointer): Pointer;
begin
  Result := nil;
end;

procedure TsmxCustomTree.DelRow(RowIndex: Pointer);
begin
end;

procedure TsmxCustomTree.DoChangeRow;
begin
  if Assigned(FOnChangeRow) then
    FOnChangeRow(Self);
end;

procedure TsmxCustomTree.InternalChangeRow;
begin
end;

procedure TsmxCustomTree.ChangeRow;
begin
  InternalChangeRow;
  DoChangeRow;
end;

procedure TsmxCustomTree.DoCollapse;
begin
  if Assigned(FOnCollapse) then
    FOnCollapse(Self);
end;

procedure TsmxCustomTree.InternalCollapse;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomTree.Collapse;
begin
  InternalCollapse;
  DoCollapse;
end;

procedure TsmxCustomTree.DoEdited;
begin
  if Assigned(FOnEdited) then
    FOnEdited(Self);
end;

procedure TsmxCustomTree.InternalEdited;
begin
end;

procedure TsmxCustomTree.Edited;
begin
  InternalEdited;
  DoEdited;
end;

procedure TsmxCustomTree.DoEditing;
begin
  if Assigned(FOnEditing) then
    FOnEditing(Self);
end;

procedure TsmxCustomTree.InternalEditing;
begin
end;

procedure TsmxCustomTree.Editing;
begin
  InternalEditing;
  DoEditing;
end;

procedure TsmxCustomTree.DoExpand;
begin
  if Assigned(FOnExpand) then
    FOnExpand(Self);
end;

procedure TsmxCustomTree.InternalExpand;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomTree.Expand;
begin
  InternalExpand;
  DoExpand;
end;

function TsmxCustomTree.GetExpanded(RowIndex: Pointer): Boolean;
begin
  Result := False;
end;

procedure TsmxCustomTree.SetExpanded(RowIndex: Pointer; Value: Boolean);
begin
end;

function TsmxCustomTree.GetFocusedRowIndex: Pointer;
begin
  Result := nil;
end;

procedure TsmxCustomTree.SetFocusedRowIndex(Value: Pointer);
begin
end;

function TsmxCustomTree.GetEditor: IsmxTreeEditor;
begin
  Result := nil;
end;

function TsmxCustomTree.GetParentRow(RowIndex: Pointer): Pointer;
begin
  Result := nil;
end;

procedure TsmxCustomTree.SetParentRow(RowIndex, Value: Pointer);
begin
end;

function TsmxCustomTree.GetRootRow: Pointer;
begin
  Result := nil;
end;

function TsmxCustomTree.GetRow(RowIndex: Pointer; Index: Integer): Pointer;
begin
  Result := nil;
end;

function TsmxCustomTree.GetRowCount(RowIndex: Pointer): Integer;
begin
  Result := -1;
end;

procedure TsmxCustomTree.SetRowCount(RowIndex: Pointer; Value: Integer);
begin
end;

function TsmxCustomTree.GetTreeCaption(ColIndex: Integer; RowIndex: Pointer): String;
begin
  Result := '';
end;

procedure TsmxCustomTree.SetTreeCaption(ColIndex: Integer; RowIndex: Pointer; const Value: String);
begin
end;

function TsmxCustomTree.GetTreeValue(ColIndex: Integer; RowIndex: Pointer): Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomTree.SetTreeValue(ColIndex: Integer; RowIndex: Pointer; const Value: Variant);
begin
end;

procedure TsmxCustomTree.InternalApply;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomTree.InternalCancel;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomTree.InternalPrepare;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomTree.InternalRefresh;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomTree.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Request) and (Operation = opRemove) then
    Request := nil;
end;

function TsmxCustomTree.RowLevel(RowIndex: Pointer): Integer;
begin
  Result := -1;
end;

procedure TsmxCustomTree.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
  if Assigned(FRequest) then
    FRequest.FreeNotification(Self);
end;

procedure TsmxCustomTree.SetTreeOptions(Value: TsmxTreeOptions);
begin
  FTreeOptions := Value;
end;

{ TsmxCustomFilter }

procedure TsmxCustomFilter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomFilter then
  begin
    DisplayFormat := TsmxCustomFilter(Source).DisplayFormat;
    FilterOptions := TsmxCustomFilter(Source).FilterOptions;
    FilterValue := TsmxCustomFilter(Source).FilterValue;
    HeaderAlignment := TsmxCustomFilter(Source).HeaderAlignment;
    HeaderCaption := TsmxCustomFilter(Source).HeaderCaption;
    HeaderColor := TsmxCustomFilter(Source).HeaderColor;
    HeaderFont := TsmxCustomFilter(Source).HeaderFont;
    ValueFormat := TsmxCustomFilter(Source).ValueFormat;
  end;
end;

procedure TsmxCustomFilter.DoChangeFilter;
begin
  if Assigned(FOnChangeFilter) then
    FOnChangeFilter(Self);
end;

procedure TsmxCustomFilter.InternalChangeFilter;
begin
end;

procedure TsmxCustomFilter.ChangeFilter;
begin
  InternalChangeFilter;
  DoChangeFilter;
end;

function TsmxCustomFilter.GetCellOwner: TsmxCustomFilterDesk;
begin
  Result := TsmxCustomFilterDesk(inherited CellOwner);
end;

procedure TsmxCustomFilter.SetCellOwner(Value: TsmxCustomFilterDesk);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomFilter.GetDisplayFormat: String;
begin
  Result := '';
end;

procedure TsmxCustomFilter.SetDisplayFormat(const Value: String);
begin
end;

function TsmxCustomFilter.GetFilterAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TsmxCustomFilter.SetFilterAlignment(Value: TAlignment);
begin
end;

function TsmxCustomFilter.GetFilterColor: TColor;
begin
  Result := Graphics.clBlack;
end;

procedure TsmxCustomFilter.SetFilterColor(Value: TColor);
begin
end;

function TsmxCustomFilter.GetFilterFont: TFont;
begin
  Result := nil;
end;

procedure TsmxCustomFilter.SetFilterFont(Value: TFont);
begin
end;

function TsmxCustomFilter.GetFilterValue: Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomFilter.SetFilterValue(const Value: Variant);
begin
end;

function TsmxCustomFilter.GetHeaderAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TsmxCustomFilter.SetHeaderAlignment(Value: TAlignment);
begin
end;

function TsmxCustomFilter.GetFilterText: String;
begin
  Result := '';
end;

procedure TsmxCustomFilter.SetFilterText(const Value: String);
begin
end;

function TsmxCustomFilter.GetHeaderColor: TColor;
begin
  Result := 0;
end;

procedure TsmxCustomFilter.SetHeaderColor(Value: TColor);
begin
end;

function TsmxCustomFilter.GetHeaderFont: TFont;
begin
  Result := nil;
end;

procedure TsmxCustomFilter.SetHeaderFont(Value: TFont);
begin
end;

function TsmxCustomFilter.GetValueFormat: String;
begin
  Result := '';
end;

procedure TsmxCustomFilter.SetValueFormat(const Value: String);
begin
end;

procedure TsmxCustomFilter.SetFilterOptions(Value: TsmxFilterOptions);
begin
  FFilterOptions := Value;
end;

{ TsmxCustomFilterDesk }

function TsmxCustomFilterDesk.AddSlave: TsmxCustomFilter;
begin
  Result := TsmxCustomFilter(inherited AddSlave);
end;

function TsmxCustomFilterDesk.GetSlave(Index: Integer): TsmxCustomFilter;
begin
  Result := TsmxCustomFilter(inherited Slaves[Index]);
end;

procedure TsmxCustomFilterDesk.SetSlave(Index: Integer; Value: TsmxCustomFilter);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomFilterDesk.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomFilter;
end;

procedure TsmxCustomFilterDesk.InternalApply;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomFilterDesk.InternalCancel;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomFilterDesk.InternalPrepare;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomFilterDesk.InternalRefresh;
begin
  if Assigned(FRequest) then
    FRequest.CellRequest := Self;
end;

procedure TsmxCustomFilterDesk.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Request) and (Operation = opRemove) then
    Request := nil;
end;

procedure TsmxCustomFilterDesk.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
  if Assigned(FRequest) then
    FRequest.FreeNotification(Self);
end;

{ TsmxCustomSection }

function TsmxCustomSection.GetCellOwner: TsmxCustomPage;
begin
  Result := TsmxCustomPage(inherited CellOwner);
end;

procedure TsmxCustomSection.SetCellOwner(Value: TsmxCustomPage);
begin
  inherited CellOwner := Value;
end;

{ TsmxCustomPage }

function TsmxCustomPage.AddSlave: TsmxCustomSection;
begin
  Result := TsmxCustomSection(inherited AddSlave);
end;

function TsmxCustomPage.GetCellOwner: TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited CellOwner);
end;

procedure TsmxCustomPage.SetCellOwner(Value: TsmxCustomPageManager);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomPage.GetSlave(Index: Integer): TsmxCustomSection;
begin
  Result := TsmxCustomSection(inherited Slaves[Index]);
end;

procedure TsmxCustomPage.SetSlave(Index: Integer; Value: TsmxCustomSection);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomPage.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomSection;
end;

{ TsmxCustomPageManager }

function TsmxCustomPageManager.AddSlave: TsmxCustomPage;
begin
  Result := TsmxCustomPage(inherited AddSlave);
end;

procedure TsmxCustomPageManager.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomPageManager then
  begin
    IsMultiLine := TsmxCustomPageManager(Source).IsMultiLine;
    PageManagerStyle := TsmxCustomPageManager(Source).PageManagerStyle;
  end;
end;

procedure TsmxCustomPageManager.DoChangePage;
begin
  if Assigned(FOnChangePage) then
    FOnChangePage(Self);
end;

procedure TsmxCustomPageManager.InternalChangePage;
begin
end;

procedure TsmxCustomPageManager.ChangePage;
begin
  InternalChangePage;
  DoChangePage;
end;

function TsmxCustomPageManager.GetActivePageIndex: Integer;
begin
  Result := -1;
end;

procedure TsmxCustomPageManager.SetActivePageIndex(Value: Integer);
begin
end;

function TsmxCustomPageManager.GetSlave(Index: Integer): TsmxCustomPage;
begin
  Result := TsmxCustomPage(inherited Slaves[Index]);
end;

procedure TsmxCustomPageManager.SetSlave(Index: Integer; Value: TsmxCustomPage);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomPageManager.GetIsMultiLine: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomPageManager.SetIsMultiLine(Value: Boolean);
begin
end;

function TsmxCustomPageManager.GetPageManagerStyle: TsmxPageManagerStyle;
begin
  Result := pmsTab;
end;

procedure TsmxCustomPageManager.SetPageManagerStyle(Value: TsmxPageManagerStyle);
begin
end;

function TsmxCustomPageManager.GetPagePosition: TsmxPagePosition;
begin
  Result := ppTop;
end;

procedure TsmxCustomPageManager.SetPagePosition(Value: TsmxPagePosition);
begin
end;

function TsmxCustomPageManager.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomPage;
end;

{ TsmxCustomMenuItem }

function TsmxCustomMenuItem.AddSlave: TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited AddSlave);
end;

procedure TsmxCustomMenuItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomMenuItem then
  begin
    IsChecked := TsmxCustomMenuItem(Source).IsChecked;
    MenuItemHotKey := TsmxCustomMenuItem(Source).MenuItemHotKey;
    MenuItemStyle := TsmxCustomMenuItem(Source).MenuItemStyle;
  end;
end;

function TsmxCustomMenuItem.GetCellOwner: TsmxControlCell;
begin
  Result := TsmxControlCell(inherited CellOwner);
end;

procedure TsmxCustomMenuItem.SetCellOwner(Value: TsmxControlCell);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomMenuItem.GetIsChecked: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomMenuItem.SetIsChecked(Value: Boolean);
begin
end;

function TsmxCustomMenuItem.GetMenuItemHotKey: Integer;
begin
  Result := 0;
end;

procedure TsmxCustomMenuItem.SetMenuItemHotKey(Value: Integer);
begin
end;

function TsmxCustomMenuItem.GetMenuItemStyle: TsmxMenuItemStyle;
begin
  Result := misPoint;
end;

procedure TsmxCustomMenuItem.SetMenuItemStyle(Value: TsmxMenuItemStyle);
begin
end;

function TsmxCustomMenuItem.GetSlave(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited Slaves[Index]);
end;

procedure TsmxCustomMenuItem.SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomMenuItem.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomMenuItem;
end;

procedure TsmxCustomMenuItem.SetAlgorithm(Value: TsmxCustomAlgorithm);
begin
  inherited SetAlgorithm(Value);
  if Assigned(Algorithm) then
    MenuItemHotKey := Algorithm.AlgorithmHotKey;
end;

{ TsmxCustomMainMenu }

function TsmxCustomMainMenu.AddSlave: TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited AddSlave);
end;

function TsmxCustomMainMenu.GetSlave(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited Slaves[Index]);
end;

procedure TsmxCustomMainMenu.SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
begin
  Slaves[Index] := Value;
end;

function TsmxCustomMainMenu.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomMenuItem;
end;

{ TsmxCustomPopupMenu }

function TsmxCustomPopupMenu.AddSlave: TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited AddSlave);
end;

function TsmxCustomPopupMenu.GetCellOwner: TsmxCustomPopupList;
begin
  Result := TsmxCustomPopupList(inherited CellOwner);
end;

procedure TsmxCustomPopupMenu.SetCellOwner(Value: TsmxCustomPopupList);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomPopupMenu.GetSlave(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited Slaves[Index]);
end;

procedure TsmxCustomPopupMenu.SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
begin
  Slaves[Index] := Value;
end;

function TsmxCustomPopupMenu.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomMenuItem;
end;

{ TsmxCustomPopupList }

function TsmxCustomPopupList.AddSlave: TsmxCustomPopupMenu;
begin
  Result := TsmxCustomPopupMenu(inherited AddSlave);
end;

function TsmxCustomPopupList.GetSlave(Index: Integer): TsmxCustomPopupMenu;
begin
  Result := TsmxCustomPopupMenu(inherited Slaves[Index]);
end;

procedure TsmxCustomPopupList.SetSlave(Index: Integer; Value: TsmxCustomPopupMenu);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomPopupList.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomPopupMenu;
end;

{ TsmxCustomToolItem }

procedure TsmxCustomToolItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomToolItem then
  begin
    IsChecked := TsmxCustomToolItem(Source).IsChecked;
    ToolItemStyle := TsmxCustomToolItem(Source).ToolItemStyle;
  end;
end;

function TsmxCustomToolItem.GetCellOwner: TsmxCustomToolBoard;
begin
  Result := TsmxCustomToolBoard(inherited CellOwner);
end;

procedure TsmxCustomToolItem.SetCellOwner(Value: TsmxCustomToolBoard);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomToolItem.GetIsChecked: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomToolItem.SetIsChecked(Value: Boolean);
begin
end;

function TsmxCustomToolItem.GetToolItemStyle: TsmxToolItemStyle;
begin
  Result := tisButton;
end;

procedure TsmxCustomToolItem.SetToolItemStyle(Value: TsmxToolItemStyle);
begin
end;

{ TsmxCustomToolBoard }

function TsmxCustomToolBoard.AddSlave: TsmxCustomToolItem;
begin
  Result := TsmxCustomToolItem(inherited AddSlave);
end;

procedure TsmxCustomToolBoard.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomToolBoard then
  begin
    IsFlat := TsmxCustomToolBoard(Source).IsFlat;
    IsShowCaptions := TsmxCustomToolBoard(Source).IsShowCaptions;
  end;
end;

function TsmxCustomToolBoard.GetCellOwner: TsmxCustomControlBoard;
begin
  Result := TsmxCustomControlBoard(inherited CellOwner);
end;

procedure TsmxCustomToolBoard.SetCellOwner(Value: TsmxCustomControlBoard);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomToolBoard.GetIsFlat: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomToolBoard.SetIsFlat(Value: Boolean);
begin
end;

function TsmxCustomToolBoard.GetIsShowCaptions: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomToolBoard.SetIsShowCaptions(Value: Boolean);
begin
end;

function TsmxCustomToolBoard.GetSlave(Index: Integer): TsmxCustomToolItem;
begin
  Result := TsmxCustomToolItem(inherited Slaves[Index]);
end;

procedure TsmxCustomToolBoard.SetSlave(Index: Integer; Value: TsmxCustomToolItem);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomToolBoard.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomToolItem;
end;

{ TsmxCustomControlBoard }

function TsmxCustomControlBoard.AddSlave: TsmxCustomToolBoard;
begin
  Result := TsmxCustomToolBoard(inherited AddSlave);
end;

function TsmxCustomControlBoard.GetSlave(Index: Integer): TsmxCustomToolBoard;
begin
  Result := TsmxCustomToolBoard(inherited Slaves[Index]);
end;

procedure TsmxCustomControlBoard.SetSlave(Index: Integer; Value: TsmxCustomToolBoard);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomControlBoard.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomToolBoard;
end;

{ TsmxCustomStatusItem }

procedure TsmxCustomStatusItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomStatusItem then
  begin
    StatusItemAlignment := TsmxCustomStatusItem(Source).StatusItemAlignment;
    StatusItemStyle := TsmxCustomStatusItem(Source).StatusItemStyle;
  end;
end;

procedure TsmxCustomStatusItem.DoDrawPanel;
begin
  if Assigned(FOnDrawPanel) then
    FOnDrawPanel(Self);
end;

procedure TsmxCustomStatusItem.InternalDrawPanel;
begin
end;

procedure TsmxCustomStatusItem.DrawPanel;
begin
  InternalDrawPanel;
  DoDrawPanel;
end;

function TsmxCustomStatusItem.GetCellOwner: TsmxCustomStatusBoard;
begin
  Result := TsmxCustomStatusBoard(inherited CellOwner);
end;

procedure TsmxCustomStatusItem.SetCellOwner(Value: TsmxCustomStatusBoard);
begin
  inherited CellOwner := Value;
end;

function TsmxCustomStatusItem.GetStatusItemAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TsmxCustomStatusItem.SetStatusItemAlignment(Value: TAlignment);
begin
end;

function TsmxCustomStatusItem.GetStatusItemStyle: TsmxStatusItemStyle;
begin
  Result := sisText;
end;

procedure TsmxCustomStatusItem.SetStatusItemStyle(Value: TsmxStatusItemStyle);
begin
end;

{ TsmxCustomStatusBoard }

function TsmxCustomStatusBoard.AddSlave: TsmxCustomStatusItem;
begin
  Result := TsmxCustomStatusItem(inherited AddSlave);
end;

function TsmxCustomStatusBoard.GetSlave(Index: Integer): TsmxCustomStatusItem;
begin
  Result := TsmxCustomStatusItem(inherited Slaves[Index]);
end;

procedure TsmxCustomStatusBoard.SetSlave(Index: Integer; Value: TsmxCustomStatusItem);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomStatusBoard.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomStatusItem;
end;

{ TsmxCustomForm }

constructor TsmxCustomForm.Create(AOwner: TComponent; AID: Integer);
begin
  Create(AOwner);
  FID := AID;
end;

function TsmxCustomForm.AddSlave: TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited AddSlave);
end;

procedure TsmxCustomForm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomForm then
  begin
    FormBorder := TsmxCustomForm(Source).FormBorder;
    FormPosition := TsmxCustomForm(Source).FormPosition;
    IntfID := TsmxCustomForm(Source).IntfID;
    IsMaximize := TsmxCustomForm(Source).IsMaximize;
  end;
end;

function TsmxCustomForm.CellParams(const Name: String; var Value: Variant): Boolean;
begin
  if SysUtils.AnsiCompareText(Name, 'ID') = 0 then
  begin
    Value := FID;
    Result := True;
  end else
  if SysUtils.AnsiCompareText(Name, 'IntfID') = 0 then
  begin
    Value := FIntfID;
    Result := True;
  end else
    Result := inherited CellParams(Name, Value);
end;

procedure TsmxCustomForm.DoActivate;
begin
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TsmxCustomForm.InternalActivate;
begin
end;

procedure TsmxCustomForm.Activate;
begin
  InternalActivate;
  DoActivate;
end;

procedure TsmxCustomForm.DoDeactivate;
begin
  if Assigned(FOnDeActivate) then
    FOnDeactivate(Self);
end;

procedure TsmxCustomForm.InternalDeactivate;
begin
end;

procedure TsmxCustomForm.Deactivate;
begin
  InternalDeactivate;
  DoDeactivate;
end;

procedure TsmxCustomForm.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TsmxCustomForm.InternalClose;
begin
end;

procedure TsmxCustomForm.Close;
begin
  DoClose;
  InternalClose;
end;

procedure TsmxCustomForm.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TsmxCustomForm.InternalShow;
begin
end;

procedure TsmxCustomForm.Show;
begin
  InternalShow;
  DoShow;
end;

function TsmxCustomForm.InternalShowModal: TModalResult;
begin
  Result := mrNone;
end;

function TsmxCustomForm.ShowModal: TModalResult;
begin
  DoShow;
  Result := InternalShowModal;
end;

function TsmxCustomForm.GetIsMaximize: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomForm.SetIsMaximize(Value: Boolean);
begin
end;

function TsmxCustomForm.GetModalResult: TModalResult;
begin
  Result := mrNone;
end;

procedure TsmxCustomForm.SetModalResult(Value: TModalResult);
begin
end;

function TsmxCustomForm.GetID: Integer;
begin
  Result := FID;
end;

procedure TsmxCustomForm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
  if Assigned(FAlgorithmList) then
    FAlgorithmList.FreeNotification(Self);
end;

function TsmxCustomForm.GetFormBorder: TsmxFormBorder;
begin
  Result := fbNone;
end;

procedure TsmxCustomForm.SetFormBorder(Value: TsmxFormBorder);
begin
end;

function TsmxCustomForm.GetFormPosition: TsmxFormPosition;
begin
  Result := fpDesigned;
end;

procedure TsmxCustomForm.SetFormPosition(Value: TsmxFormPosition);
begin
end;

function TsmxCustomForm.GetSlave(Index: Integer): TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited Slaves[Index]);
end;

procedure TsmxCustomForm.SetSlave(Index: Integer; Value: TsmxCustomPageManager);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomForm.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxCustomPageManager;
end;

procedure TsmxCustomForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = AlgorithmList then
      AlgorithmList := nil else
    if AComponent = PopupList then
      PopupList := nil else
    if AComponent = RequestList then
      RequestList := nil;
  end;
end;

procedure TsmxCustomForm.SetFormOptions(Value: TsmxFormOptions);
begin
  FFormOptions := Value;
end;

procedure TsmxCustomForm.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

procedure TsmxCustomForm.SetPopupList(Value: TsmxCustomPopupList);
begin
  FPopupList := Value;
  if Assigned(FPopupList) then
    FPopupList.FreeNotification(Self);
end;

procedure TsmxCustomForm.SetRequestList(Value: TsmxCustomRequestList);
begin
  FRequestList := Value;
  if Assigned(FRequestList) then
    FRequestList.FreeNotification(Self);
end;

end.
