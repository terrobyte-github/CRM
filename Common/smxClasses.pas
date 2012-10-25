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
  Classes, Controls, ImgList, Graphics, XMLIntf, smxBaseClasses, smxTypes,
  smxDBIntf, smxManagerIntf;

type
  { EsmxCfgError }

  EsmxCellError = class(EsmxComponentError)
  private
    FCfgID: Integer;
  public
    constructor CreateByCfgID(Ident: Integer; const Args: array of const;
      CfgID: Integer; OriginMessage: String = ''); overload;
    constructor CreateByCfgID(ResStringRec: PResStringRec; const Args: array of const;
      CfgID: Integer; OriginMessage: String = ''); overload;

    property CfgID: Integer read FCfgID write FCfgID;
  end;

  { TsmxBaseCfg }

  TsmxCustomRequest = class;

  TsmxBaseCfg = class(TsmxComponent)
  private
    FCfgID: Integer;
    FSelectRequest: TsmxCustomRequest;
    FXMLDocIntf: IXMLDocument;
  protected
    function GetXMLText: String; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;
    procedure SetXMLText(const Value: String); virtual;

    property XMLDoc: IXMLDocument read FXMLDocIntf;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    procedure Read; virtual;
    procedure Receive;
    procedure Remove; virtual;
    procedure Return;
    procedure Write; virtual;

    property CfgID: Integer read FCfgID write SetCfgID;
    property SelectRequest: TsmxCustomRequest read FSelectRequest write SetSelectRequest;
    property XMLText: String read GetXMLText write SetXMLText;
  end;

  TsmxBaseCfgClass = class of TsmxBaseCfg;

  { TsmxBaseCell }

  TsmxBaseCell = class(TsmxComponent)
  private
    FCellList: TList;
    FCellParent: TsmxBaseCell;
    FCfgID: Integer;
    FDatabaseManagerIntf: IsmxDatabaseManager;
    FFormManagerIntf: IsmxFormManager;
    FImageList: TCustomImageList;
    FLibraryManagerIntf: IsmxLibraryManager;
    FOnInitialize: TsmxComponentEvent;
    FSelectRequest: TsmxCustomRequest;
    FStorageManagerIntf: IsmxStorageManager;
    procedure ClearCells;
    function GetCell(Index: Integer): TsmxBaseCell;
    function GetCellCount: Integer;
    function GetCellIndex: Integer;
    function GetCellList: TList;
    function GetCellRoot: TsmxBaseCell;
    function GetDatabaseManager: IsmxDatabaseManager;
    function GetFormManager: IsmxFormManager;
    function GetImageList: TCustomImageList;
    function GetLibraryManager: IsmxLibraryManager;
    function GetStorageManager: IsmxStorageManager;
  protected
    procedure DoInitialize; virtual;
    function GetInternalObject: TObject; virtual;
    procedure InternalInitialize; virtual;
    procedure SetCellParent(Value: TsmxBaseCell); virtual;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager); virtual;
    procedure SetFormManager(const Value: IsmxFormManager); virtual;
    procedure SetImageList(Value: TCustomImageList); virtual;
    procedure SetLibraryManager(const Value: IsmxLibraryManager); virtual;
    procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;
    procedure SetStorageManager(const Value: IsmxStorageManager); virtual;

    property CellList: TList read GetCellList;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; virtual;
    function FindByCfgID(ACfgID: Integer): TsmxBaseCell;
    procedure Initialize;

    property CellCount: Integer read GetCellCount;
    property CellIndex: Integer read GetCellIndex;
    property CellParent: TsmxBaseCell read FCellParent write SetCellParent;
    property CellRoot: TsmxBaseCell read GetCellRoot;
    property Cells[Index: Integer]: TsmxBaseCell read GetCell;
    property CfgID: Integer read FCfgID write SetCfgID;
    property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;
    property FormManager: IsmxFormManager read GetFormManager write SetFormManager;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property LibraryManager: IsmxLibraryManager read GetLibraryManager write SetLibraryManager;
    property SelectRequest: TsmxCustomRequest read FSelectRequest write SetSelectRequest;
    property StorageManager: IsmxStorageManager read GetStorageManager write SetStorageManager;

    property OnInitialize: TsmxComponentEvent read FOnInitialize write FOnInitialize;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

 { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxBaseCfg)
  private
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
    FIntfClass: TClass;
    FIntfClassName: String;
  protected
    procedure SetCellClass(Value: TsmxBaseCellClass); virtual;
    procedure SetCellClassName(const Value: String); virtual;
    procedure SetCfgClass(Value: TsmxBaseCfgClass); virtual;
    procedure SetCfgClassName(const Value: String); virtual;
    procedure SetIntfClass(Value: TClass); virtual;
    procedure SetIntfClassName(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property CellClass: TsmxBaseCellClass read FCellClass write SetCellClass;
    property CellClassName: String read FCellClassName write SetCellClassName;
    property CfgClass: TsmxBaseCfgClass read FCfgClass write SetCfgClass;
    property CfgClassName: String read FCfgClassName write SetCfgClassName;
    property IntfClass: TClass read FIntfClass write SetIntfClass;
    property IntfClassName: String read FIntfClassName write SetIntfClassName;
  end;

  { TsmxOwnerCell }

  TsmxOwnerCellClass = class of TsmxOwnerCell;

  TsmxOwnerCell = class(TsmxBaseCell)
  private
    FCellOwner: TsmxOwnerCell;
    FSlaveList: TList;
    function GetSlave(Index: Integer): TsmxOwnerCell;
    function GetSlaveCount: Integer;
    function GetSlaveIndex: Integer;
    function GetSlaveList: TList;
    procedure SetCellOwner(Value: TsmxOwnerCell);
    procedure SetSlave(Index: Integer; Value: TsmxOwnerCell);
  protected
    function GetSlaveClass: TsmxOwnerCellClass; virtual;
    procedure SetSlaveIndex(Value: Integer); virtual;

    property SlaveList: TList read GetSlaveList;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddSlave: TsmxOwnerCell;
    procedure ClearSlaves;
    procedure DeleteSlave(Index: Integer);

    property CellOwner: TsmxOwnerCell read FCellOwner write SetCellOwner;
    property SlaveClass: TsmxOwnerCellClass read GetSlaveClass;
    property SlaveCount: Integer read GetSlaveCount;
    property SlaveIndex: Integer read GetSlaveIndex write SetSlaveIndex;
    property Slaves[Index: Integer]: TsmxOwnerCell read GetSlave write SetSlave; default;
  end;

  { TsmxControlCell }

  TsmxCustomPopupMenu = class;

  TsmxControlCell = class(TsmxOwnerCell)
  private
    FOnChangeActiveControl: TsmxComponentEvent;
    FOnApply: TsmxComponentEvent;
    FOnBackup: TsmxComponentEvent;
    FOnPrepare: TsmxComponentEvent;
    FOnRestore: TsmxComponentEvent;
    FPopupMenu: TsmxCustomPopupMenu;
  protected
    procedure DoApply; virtual;
    procedure DoBackup; virtual;
    procedure DoChangeActiveControl; virtual;
    procedure DoPrepare; virtual;
    procedure DoRestore; virtual;
    function GetCellActive: Boolean; virtual;
    function GetCellAlign: TAlign; virtual;
    function GetCellCursor: TCursor; virtual;
    function GetCellEnable: Boolean; virtual;
    function GetCellHeight: Integer; virtual;
    function GetCellLeft: Integer; virtual;
    function GetCellTop: Integer; virtual;
    function GetCellVisible: Boolean; virtual;
    function GetCellWidth: Integer; virtual;
    procedure InternalApply; virtual;
    procedure InternalBackup; virtual;
    procedure InternalChangeAcitveControl; virtual;
    procedure InternalPrepare; virtual;
    procedure InternalRestore; virtual;
    procedure SetCellActive(Value: Boolean); virtual;
    procedure SetCellAlign(Value: TAlign); virtual;
    procedure SetCellCursor(Value: TCursor); virtual;
    procedure SetCellEnable(Value: Boolean); virtual;
    procedure SetCellHeight(Value: Integer); virtual;
    procedure SetCellLeft(Value: Integer); virtual;
    procedure SetCellTop(Value: Integer); virtual;
    procedure SetCellVisible(Value: Boolean); virtual;
    procedure SetCellWidth(Value: Integer); virtual;
    procedure SetPopupMenu(Value: TsmxCustomPopupMenu); virtual;
  public
    procedure Apply;
    procedure Assign(Source: TPersistent); override;
    procedure Backup;
    procedure ChangeActiveControl;
    procedure Prepare;
    procedure Restore;

    property CellActive: Boolean read GetCellActive write SetCellActive;
    property CellAlign: TAlign read GetCellAlign write SetCellAlign;
    property CellCursor: TCursor read GetCellCursor write SetCellCursor;
    property CellEnable: Boolean read GetCellEnable write SetCellEnable;
    property CellHeight: Integer read GetCellHeight write SetCellHeight;
    property CellLeft: Integer read GetCellLeft write SetCellLeft;
    property CellTop: Integer read GetCellTop write SetCellTop;
    property CellVisible: Boolean read GetCellVisible write SetCellVisible;
    property CellWidth: Integer read GetCellWidth write SetCellWidth;
    property PopupMenu: TsmxCustomPopupMenu read FPopupMenu write SetPopupMenu;

    property OnApply: TsmxComponentEvent read FOnApply write FOnApply;
    property OnBackup: TsmxComponentEvent read FOnBackup write FOnBackup;
    property OnChangeActiveControl: TsmxComponentEvent read FOnChangeActiveControl write FOnChangeActiveControl;
    property OnPrepare: TsmxComponentEvent read FOnPrepare write FOnPrepare;
    property OnRestore: TsmxComponentEvent read FOnRestore write FOnRestore;
  end;

  { TsmxActionCell }

  TsmxCustomAlgorithm = class;

  TsmxActionCell = class(TsmxControlCell)
  private
    FAlgorithm: TsmxCustomAlgorithm;
    FOnExecute: TsmxComponentEvent;
  protected
    procedure DoExecute; virtual;
    function GetCellCaption: String; virtual;
    function GetCellHint: String; virtual;
    function GetCellHotKey: Integer; virtual;
    function GetCellImageIndex: Integer; virtual;
    procedure InternalExecute; virtual;
    procedure SetAlgorithm(Value: TsmxCustomAlgorithm); virtual;
    procedure SetCellCaption(const Value: String); virtual;
    procedure SetCellHint(const Value: String); virtual;
    procedure SetCellHotKey(Value: Integer); virtual;
    procedure SetCellImageIndex(Value: Integer); virtual;
  public
    procedure Execute;
    procedure Assign(Source: TPersistent); override;

    property Algorithm: TsmxCustomAlgorithm read FAlgorithm write SetAlgorithm;
    property CellCaption: String read GetCellCaption write SetCellCaption;
    property CellHint: String read GetCellHint write SetCellHint;
    property CellHotKey: Integer read GetCellHotKey write SetCellHotKey;
    property CellImageIndex: Integer read GetCellImageIndex write SetCellImageIndex;

    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
  end;

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
    procedure SetParamValue(const Value: Variant); virtual;
  public
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxParams read GetKit write SetKit;
    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read GetParamValue write SetParamValue;
  end;

  { TsmxParams }

  TsmxParams = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxParam;
    procedure SetItem(Index: Integer; Value: TsmxParam);
    function GetValue(const Name: String): Variant;
    procedure SetValue(const Name: String; const Value: Variant);
  public
    function Add: TsmxParam;
    function FindByName(const AParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem write SetItem; default;
    property Values[const Name: String]: Variant read GetValue write SetValue;
  end;

  { TsmxTargetRequest }

  TsmxTargetRequest = class(TsmxComponent)
  private
    FDatabaseIntf: IsmxDatabase;
    FDataSetIntf: IsmxDataSet;
    FParamList: TsmxParams;
    function GetParamList: TsmxParams;
    function GetValue(const Key: String): Variant;
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetValue(const Key: String; const Value: Variant);
  protected
    property DataSet: IsmxDataSet read FDataSetIntf;
    property ParamList: TsmxParams read GetParamList;
  public
    destructor Destroy; override;
    procedure ClearParams;
    procedure DoExecute(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil);
    function DoRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      ResField: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
    function ForRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      Get: Boolean = False; Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function NewRequest(const SQLText: String = ''; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function PrepRequest(const ARequest: IsmxDataSet; Get: Boolean = True;
      Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): Boolean;

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[const Key: String]: Variant read GetValue write SetValue; default;
  end;

  { TsmxCustomRequest }

  TsmxCustomRequestList = class;

  //TsmxModifyRequestEvent = procedure(Request: TsmxCustomRequest; Modify: TsmxModifyRequest) of object;

  TsmxCustomRequest = class(TsmxOwnerCell)
  private
    FCurPerformanceMode: TsmxPerformanceMode;
    FCurDataSetIntf: IsmxDataSet;
    FDatabaseIntf: IsmxDatabase;
    FDatabaseName: String;
    FDataSetIntf: IsmxDataSet;
    FDeletePerformance: TsmxPerformanceMode;
    FDeleteRequestIntf: IsmxDataSet;
    FInsertPerformance: TsmxPerformanceMode;
    FInsertRequestIntf: IsmxDataSet;
    FOnDelete: TsmxComponentEvent;
    FOnExecute: TsmxComponentEvent;
    FOnInsert: TsmxComponentEvent;
    //FOnModify: TsmxModifyRequestEvent;
    FOnPrepare: TsmxComponentEvent;
    FOnRefreshParams: TsmxComponentEvent;
    FOnUpdate: TsmxComponentEvent;
    FOperationMode: TsmxOperationMode;
    FPerformanceMode: TsmxPerformanceMode;
    FUpdatePerformance: TsmxPerformanceMode;
    FUpdateRequestIntf: IsmxDataSet;
    function FindDatabase: IsmxDatabase;
    function GetModifyPerformance(Modify: TsmxModifyRequest): TsmxPerformanceMode;
    function GetModifyRequest(Modify: TsmxModifyRequest): IsmxDataSet;
    function GetCellOwner: TsmxCustomRequestList;
    procedure PerformRequest;
    procedure SetCellOwner(Value: TsmxCustomRequestList);
  protected
    procedure DoDelete; virtual;
    procedure DoExecute; virtual;
    procedure DoInsert; virtual;
    //procedure DoModify(Modify: TsmxModifyRequest); virtual;
    procedure DoPrepare; virtual;
    procedure DoRefreshParams; virtual;
    procedure DoUpdate; virtual;
    function GetDataSet: IsmxDataSet; virtual;
    procedure InternalDelete; virtual;
    procedure InternalExecute; virtual;
    procedure InternalInsert; virtual;
    //procedure InternalModify; virtual;
    procedure InternalPrepare; virtual;
    procedure InternalRefreshParams; virtual;
    procedure InternalUpdate; virtual;
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager); override;
    procedure SetDatabaseName(const Value: String); virtual;
    procedure SetDataSet(const Value: IsmxDataSet); virtual;
    procedure SetModifyPerformance(Modify: TsmxModifyRequest; Value: TsmxPerformanceMode); virtual;
    procedure SetModifyRequest(Modify: TsmxModifyRequest; const Value: IsmxDataSet); virtual;
    procedure SetOperationMode(Value: TsmxOperationMode); virtual;
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;

    property CurDataSet: IsmxDataSet read FCurDataSetIntf write FCurDataSetIntf;
    property CurPerformanceMode: TsmxPerformanceMode read FCurPerformanceMode write FCurPerformanceMode;
    property Database: IsmxDatabase read FDatabaseIntf write FDatabaseIntf;
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
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property DataSet: IsmxDataSet read FDataSetIntf write SetDataSet;
    property ModifyPerformances[Modify: TsmxModifyRequest]: TsmxPerformanceMode read GetModifyPerformance write SetModifyPerformance;
    property ModifyRequests[Modify: TsmxModifyRequest]: IsmxDataSet read GetModifyRequest write SetModifyRequest;
    property OperationMode: TsmxOperationMode read FOperationMode write SetOperationMode {default omManual};
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write SetPerformanceMode;

    property OnDelete: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnInsert: TsmxComponentEvent read FOnExecute write FOnExecute;
    //property OnModify: TsmxModifyRequestEvent read FOnModify write FOnModify;
    property OnPrepare: TsmxComponentEvent read FOnPrepare write FOnPrepare;
    property OnRefreshParams: TsmxComponentEvent read FOnRefreshParams write FOnRefreshParams;
    property OnUpdate: TsmxComponentEvent read FOnExecute write FOnExecute;
  end;

  { TsmxCustomRequestList }

  TsmxCustomRequestList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomRequest;
    procedure SetSlave(Index: Integer; Value: TsmxCustomRequest);
  public
    function AddSlave: TsmxCustomRequest;

    property Slaves[Index: Integer]: TsmxCustomRequest read GetSlave write SetSlave; default;
  end;

  { TsmxCustomColumn }

  TsmxCustomGrid = class;

  TsmxCustomColumn = class(TsmxControlCell)
  private
    FColumnName: String;
    FOnPressHeader: TsmxComponentEvent;
    function GetCellOwner: TsmxCustomGrid;
    procedure SetCellOwner(Value: TsmxCustomGrid);
  protected
    procedure DoPressHeader; virtual;
    function GetColumnAlignment: TAlignment; virtual;
    function GetColumnColor: TColor; virtual;
    function GetColumnFont: TFont; virtual;
    function GetColumnCaption: String; virtual;
    function GetColumnValue: Variant; virtual;
    function GetFieldName: String; virtual;
    function GetHeaderAlignment: TAlignment; virtual;
    function GetHeaderColor: TColor; virtual;
    function GetHeaderFont: TFont; virtual;
    function GetHeaderCaption: String; virtual;
    procedure InternalPressHeader; virtual;
    procedure SetColumnAlignment(Value: TAlignment); virtual;
    procedure SetColumnColor(Value: TColor); virtual;
    procedure SetColumnFont(Value: TFont); virtual;
    procedure SetColumnName(const Value: String); virtual;
    procedure SetColumnCaption(const Value: String); virtual;
    procedure SetColumnValue(const Value: Variant); virtual;
    procedure SetFieldName(const Value: String); virtual;
    procedure SetHeaderAlignment(Value: TAlignment); virtual;
    procedure SetHeaderColor(Value: TColor); virtual;
    procedure SetHeaderFont(Value: TFont); virtual;
    procedure SetHeaderCaption(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure PressHeader;

    property CellOwner: TsmxCustomGrid read GetCellOwner write SetCellOwner;
    property ColumnAlignment: TAlignment read GetColumnAlignment write SetColumnAlignment;
    property ColumnColor: TColor read GetColumnColor write SetColumnColor;
    property ColumnFont: TFont read GetColumnFont write SetColumnFont;
    property ColumnName: String read FColumnName write SetColumnName;
    property ColumnCaption: String read GetColumnCaption write SetColumnCaption;
    property ColumnValue: Variant read GetColumnValue write SetColumnValue;
    property FieldName: String read GetFieldName write SetFieldName;
    property HeaderAlignment: TAlignment read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property HeaderCaption: String read GetHeaderCaption write SetHeaderCaption;

    property OnPressHeader: TsmxComponentEvent read FOnPressHeader write FOnPressHeader;
  end;

  { TsmxCustomGrid }

  TsmxCustomGrid = class(TsmxControlCell)
  private
    FOptions: TsmxGridOptions;
    FRequest: TsmxCustomRequest;
    FOnChangeRow: TsmxComponentEvent;
    FOnPressDouble: TsmxComponentEvent;
    function GetSlave(Index: Integer): TsmxCustomColumn;
    procedure SetSlave(Index: Integer; Value: TsmxCustomColumn);
  protected
    procedure DoChangeRow; virtual;
    procedure DoPressDouble; virtual;
    procedure InternalChangeRow; virtual;
    procedure InternalPressDouble; virtual;
    procedure SetOptions(Value: TsmxGridOptions); virtual;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
  public
    function AddSlave: TsmxCustomColumn;
    procedure Assign(Source: TPersistent); override;
    function FindColumnByName(const AColumnName: String): TsmxCustomColumn;
    procedure ChangeRow;
    procedure PressDouble;

    property Options: TsmxGridOptions read FOptions write SetOptions;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property Slaves[Index: Integer]: TsmxCustomColumn read GetSlave write SetSlave; default;

    property OnChangeRow: TsmxComponentEvent read FOnChangeRow write FOnChangeRow;
    property OnPressDouble: TsmxComponentEvent read FOnPressDouble write FOnPressDouble;
  end;

  { TsmxAlgorithmParam }

  TsmxAlgorithmParams = class;

  TsmxAlgorithmParam = class(TsmxParam)
  private
    FDataType: TsmxDataType;
    FParamLocation: TsmxParamLocation;
    FParamType: TsmxParamType;
    function GetKit: TsmxAlgorithmParams;
    procedure SetKit(Value: TsmxAlgorithmParams);
  protected
    procedure SetDataType(Value: TsmxDataType); virtual;
  public
    procedure Assign(Source: TsmxKitItem); override;

    property DataType: TsmxDataType read FDataType write SetDataType;
    property Kit: TsmxAlgorithmParams read GetKit write SetKit;
    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamType: TsmxParamType read FParamType write FParamType;
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
    FActionCell: TsmxActionCell;
    FAlgorithmParams: TsmxAlgorithmParams;
    //FClientCells: TList;
    FOnExecute: TsmxComponentEvent;
    FOnRefreshParams: TsmxComponentEvent;
    //procedure ClearActions;
    function GetAlgorithmParams: TsmxAlgorithmParams;
    function GetCellOwner: TsmxCustomAlgorithmList;
    //function GetClientCells: TList;
    procedure SetCellOwner(Value: TsmxCustomAlgorithmList);
  protected
    procedure DoExecute; virtual;
    procedure DoRefreshParams; virtual;
    function GetAlgorithmCaption: String; virtual;
    function GetAlgorithmEnable: Boolean; virtual;
    function GetAlgorithmHint: String; virtual;
    function GetAlgorithmHotKey: Integer; virtual;
    function GetAlgorithmImageIndex: Integer; virtual;
    function GetAlgorithmVisible: Boolean; virtual;
    procedure InternalExecute; virtual;
    procedure InternalRefreshParams; virtual;
    procedure SetActionCell(Value: TsmxActionCell); virtual;
    procedure SetAlgorithmCaption(const Value: String); virtual;
    procedure SetAlgorithmEnable(Value: Boolean); virtual;
    procedure SetAlgorithmHint(const Value: String); virtual;
    procedure SetAlgorithmHotKey(Value: Integer); virtual;
    procedure SetAlgorithmImageIndex(Value: Integer); virtual;
    procedure SetAlgorithmParams(Value: TsmxAlgorithmParams); virtual;
    procedure SetAlgorithmVisible(Value: Boolean); virtual;
    //procedure SetOnExecute(Value: TsmxComponentEvent); virtual;

    //property ClientCells: TList read GetClientCells;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute;
    //procedure InsertClient(AClient: TsmxActionCell);
    procedure RefreshParams;
    //procedure RemoveClient(AClient: TsmxActionCell);

    property ActionCell: TsmxActionCell read FActionCell write SetActionCell;
    property AlgorithmCaption: String read GetAlgorithmCaption write SetAlgorithmCaption;
    property AlgorithmEnable: Boolean read GetAlgorithmEnable write SetAlgorithmEnable;
    property AlgorithmHint: String read GetAlgorithmHint write SetAlgorithmHint;
    property AlgorithmHotKey: Integer read GetAlgorithmHotKey write SetAlgorithmHotKey;
    property AlgorithmImageIndex: Integer read GetAlgorithmImageIndex write SetAlgorithmImageIndex;
    property AlgorithmParams: TsmxAlgorithmParams read GetAlgorithmParams write SetAlgorithmParams;
    property AlgorithmVisible: Boolean read GetAlgorithmVisible write SetAlgorithmVisible;
    property CellOwner: TsmxCustomAlgorithmList read GetCellOwner write SetCellOwner;

    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnRefreshParams: TsmxComponentEvent read FOnRefreshParams write FOnRefreshParams;
  end;

  { TsmxCustomLibAlgorithm }

  TsmxCustomLibAlgorithm = class(TsmxCustomAlgorithm)
  private
    FAlgorithmLibrary: String;
    FAlgorithmProcName: String;
  protected
    procedure SetAlgorithmLibrary(const Value: String); virtual;
    procedure SetAlgorithmProcName(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property AlgorithmLibrary: String read FAlgorithmLibrary write SetAlgorithmLibrary;
    property AlgorithmProcName: String read FAlgorithmProcName write SetAlgorithmProcName;
  end;

  { TsmxCustomAlgorithmList }

  TsmxCustomAlgorithmList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomAlgorithm;
    procedure SetSlave(Index: Integer; Value: TsmxCustomAlgorithm);
  public
    function AddSlave: TsmxCustomAlgorithm;

    property Slaves[Index: Integer]: TsmxCustomAlgorithm read GetSlave write SetSlave; default;
  end;

  { TsmxCustomFilter }

  TsmxCustomFilterDesk = class;

  TsmxCustomFilter = class(TsmxActionCell)
  private
    FFilterName: String;
    FFilterOptions: TsmxFilterOptions;
    FOnChange: TsmxComponentEvent;
    FRequest: TsmxCustomRequest;
    function GetCellOwner: TsmxCustomFilterDesk;
    procedure SetCellOwner(Value: TsmxCustomFilterDesk);
  protected
    procedure DoChange; virtual;
    function GetDisplayFormat: String; virtual;
    function GetFilterValue: Variant; virtual;
    function GetValueFormat: String; virtual;
    procedure InternalChange; virtual;
    procedure SetDisplayFormat(const Value: String); virtual;
    procedure SetFilterName(const Value: String); virtual;
    procedure SetFilterOptions(Value: TsmxFilterOptions); virtual;
    procedure SetFilterValue(const Value: Variant); virtual;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
    procedure SetValueFormat(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Change;

    property CellOwner: TsmxCustomFilterDesk read GetCellOwner write SetCellOwner;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property FilterCaption: String read GetCellCaption write SetCellCaption;
    property FilterName: String read FFilterName write SetFilterName;
    property FilterOptions: TsmxFilterOptions read FFilterOptions write SetFilterOptions;
    property FilterValue: Variant read GetFilterValue write SetFilterValue;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property ValueFormat: String read GetValueFormat write SetValueFormat;

    property OnChange: TsmxComponentEvent read FOnChange write FOnChange;
  end;

  { TsmxCustomFilterDesk }

  TsmxCustomFilterDesk = class(TsmxControlCell)
  private
    FRequest: TsmxCustomRequest;
    function GetSlave(Index: Integer): TsmxCustomFilter;
    procedure SetSlave(Index: Integer; Value: TsmxCustomFilter);
  protected
    procedure InternalApply; override;
    procedure InternalPrepare; override;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
  public
    function AddSlave: TsmxCustomFilter;
    function FindFilterByName(const AFilterName: String): TsmxCustomFilter;

    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property Slaves[Index: Integer]: TsmxCustomFilter read GetSlave write SetSlave; default;
  end;

  { TsmxCustomSection }

  TsmxCustomPage = class;

  TsmxCustomSection = class(TsmxControlCell)
  private
    FFilterDesk: TsmxCustomFilterDesk;
    FGrid: TsmxCustomGrid;
    function GetCellOwner: TsmxCustomPage;
    procedure SetCellOwner(Value: TsmxCustomPage);
  protected
    procedure SetFilterDesk(Value: TsmxCustomFilterDesk); virtual;
    procedure SetGrid(Value: TsmxCustomGrid); virtual;
  public
    property CellOwner: TsmxCustomPage read GetCellOwner write SetCellOwner;
    property FilterDesk: TsmxCustomFilterDesk read FFilterDesk write SetFilterDesk;
    property Grid: TsmxCustomGrid read FGrid write SetGrid;
  end;

  { TsmxCustomPage }

  TsmxCustomPageManager = class;

  TsmxCustomPage = class(TsmxControlCell)
  private
    function GetCellOwner: TsmxCustomPageManager;
    function GetSlave(Index: Integer): TsmxCustomSection;
    procedure SetCellOwner(Value: TsmxCustomPageManager);
    procedure SetSlave(Index: Integer; Value: TsmxCustomSection);
  public
    function AddSlave: TsmxCustomSection;

    property CellOwner: TsmxCustomPageManager read GetCellOwner write SetCellOwner;
    property Slaves[Index: Integer]: TsmxCustomSection read GetSlave write SetSlave; default;
  end;

  { TsmxCustomPageManager }

  TsmxCustomPageManager = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomPage;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPage);
  public
    function AddSlave: TsmxCustomPage;

    property Slaves[Index: Integer]: TsmxCustomPage read GetSlave write SetSlave; default;
  end;

  { TsmxCustomMenuItem }

  TsmxCustomMenuItem = class(TsmxActionCell)
  private
    function GetCellOwner: TsmxCustomMenuItem;
    function GetSlave(Index: Integer): TsmxCustomMenuItem;
    procedure SetCellOwner(Value: TsmxCustomMenuItem);
    procedure SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
  public
    function AddSlave: TsmxCustomMenuItem;

    property CellOwner: TsmxCustomMenuItem read GetCellOwner write SetCellOwner;
    property Slaves[Index: Integer]: TsmxCustomMenuItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomMainMenu }

  TsmxCustomMainMenu = class(TsmxControlCell)
  private
    FMenuItems: TsmxCustomMenuItem;
  protected
    procedure SetMenuItemList(Value: TsmxCustomMenuItem); virtual;
  public
    property MenuItems: TsmxCustomMenuItem read FMenuItems write SetMenuItemList;
  end;

  { TsmxCustomPopupMenu }

  TsmxCustomPopupMenu = class(TsmxControlCell)
  private
    FMenuItems: TsmxCustomMenuItem;
  protected
    procedure SetMenuItemList(Value: TsmxCustomMenuItem); virtual;
  public
    property MenuItems: TsmxCustomMenuItem read FMenuItems write SetMenuItemList;
  end;

  { TsmxCustomToolItem }

  TsmxCustomToolBoard = class;

  TsmxCustomToolItem = class(TsmxActionCell)
  private
    function GetCellOwner: TsmxCustomToolBoard;
    procedure SetCellOwner(Value: TsmxCustomToolBoard);
  public
    property CellOwner: TsmxCustomToolBoard read GetCellOwner write SetCellOwner;
  end;

  { TsmxCustomToolBoard }

  TsmxCustomToolBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomToolItem;
    procedure SetSlave(Index: Integer; Value: TsmxCustomToolItem);
  public
    function AddSlave: TsmxCustomToolItem;

    property Slaves[Index: Integer]: TsmxCustomToolItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomControlBoard }

  TsmxCustomControlBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomToolBoard;
    procedure SetSlave(Index: Integer; Value: TsmxCustomToolBoard);
  public
    function AddSlave: TsmxCustomToolBoard;

    property Slaves[Index: Integer]: TsmxCustomToolBoard read GetSlave write SetSlave; default;
  end;

  { TsmxCustomStatusItem }

  TsmxCustomStatusBoard = class;

  TsmxCustomStatusItem = class(TsmxActionCell)
  private
    function GetCellOwner: TsmxCustomStatusBoard;
    procedure SetCellOwner(Value: TsmxCustomStatusBoard);
  public
    property CellOwner: TsmxCustomStatusBoard read GetCellOwner write SetCellOwner;
  end;

  { TsmxCustomStatusBoard }

  TsmxCustomStatusBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomStatusItem;
    procedure SetSlave(Index: Integer; Value: TsmxCustomStatusItem);
  public
    function AddSlave: TsmxCustomStatusItem;

    property Slaves[Index: Integer]: TsmxCustomStatusItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomForm }

  TsmxCustomForm = class(TsmxControlCell, IsmxForm)
  private
    FID: Integer;
    FIntfID: Integer;
    function GetID: Integer;
    function GetCfgID: Integer;
    function GetInternalRef: Integer;
  protected
    function GetModalResult: TModalResult; virtual;
    procedure SetModalResult(Value: TModalResult); virtual;
    procedure SetFormManager(const Value: IsmxFormManager); override;
    procedure SetIntfID(Value: Integer); virtual;
  public
    constructor CreateByID(AOwner: TComponent; AID: Integer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; override;
    procedure Close; virtual;
    procedure Show; virtual;
    function ShowModal: TModalResult; virtual;

    property ID: Integer read GetID;
    property IntfID: Integer read FIntfID write SetIntfID;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
  end;

  TsmxCustomFormClass = class of TsmxCustomForm;

  { TsmxStateUnit }

  TsmxStateUnits = class;

  TsmxStateUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FCurrentIntfID: Integer;
    FPriorIntfID: Integer;
    FUnitEnable: Boolean;
    FUnitVisible: Boolean;
    function GetHKit: TsmxStateUnits;
    function GetItem(Index: Integer): TsmxStateUnit;
    function GetParent: TsmxStateUnit;
    procedure SetCurrentIntfID(Value: Integer);
    procedure SetItem(Index: Integer; Value: TsmxStateUnit);
    procedure SetParent(Value: TsmxStateUnit);
    procedure SwitchIntfID;
  protected
    procedure SetUnitEnable(Value: Boolean); virtual;
    procedure SetUnitVisible(Value: Boolean); virtual;

    property PriorIntfID: Integer read FPriorIntfID write FPriorIntfID;
  public
    function Add: TsmxStateUnit;
    procedure Assign(Source: TsmxHKitItem); override;
    function FindByCfgID(ACfgID: Integer; AAmongAll: Boolean = False): TsmxStateUnit;

    property CfgID: Integer read FCfgID write FCfgID;
    property CurrentIntfID: Integer read FCurrentIntfID write SetCurrentIntfID;
    property HKit: TsmxStateUnits read GetHKit;
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
    procedure SetRoot(Value: TsmxStateUnit);
  public
    procedure Assign(Source: TsmxHKit); override;

    property IntfID: Integer read FIntfID write FIntfID;
    property Root: TsmxStateUnit read GetRoot write SetRoot;
  end;

  { TsmxCellState }

  TsmxCellState = class(TsmxKitItem)
  private
    FStateID: Integer;
    FStateUnits: TsmxStateUnits;
    function GetStateUnits: TsmxStateUnits;
    procedure SetStateUnits(Value: TsmxStateUnits);
  public
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;

    property StateID: Integer read FStateID write FStateID;
    property StateUnits: TsmxStateUnits read GetStateUnits write SetStateUnits;
  end;

  { TsmxCellStates }

  TsmxCellStates = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxCellState;
    procedure SetItem(Index: Integer; Value: TsmxCellState);
  public
    function Add: TsmxCellState;
    function FindByStateID(AStateID: Integer): TsmxCellState;

    property Items[Index: Integer]: TsmxCellState read GetItem write SetItem; default;
  end;

  { TsmxStateCfg }

  TsmxStateCfg = class(TsmxBaseCfg)
  private
    FCellStates: TsmxCellStates;
    FID: Integer;
    FIntfID: Integer;
    function GetCellStates: TsmxCellStates;
    procedure SetCellStates(Value: TsmxCellStates);
  protected
    procedure Load; override;
    procedure Save; override;
    procedure SetXMLText(const Value: String); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property CellStates: TsmxCellStates read GetCellStates write SetCellStates;
    property ID: Integer read FID write FID;
    property IntfID: Integer read FIntfID write FIntfID;
  end;

  { TsmxCustomStateForm }

  TsmxCustomStateForm = class(TsmxCustomForm)
  private
    FStateCfg: TsmxBaseCfg;
    FStateID: Integer;
    FStateRequest: TsmxCustomRequest;
    procedure ChangeState;
    function GetStateCfg: TsmxStateCfg;
    procedure PutState;
    procedure SetStateCfg(Value: TsmxStateCfg);
  protected
    function GetStateCfgClass: TsmxBaseCfgClass; virtual;
    procedure SetCfgID(Value: Integer); override;
    procedure SetIntfID(Value: Integer); override;
    procedure SetStateID(Value: Integer); virtual;
    procedure SetStateRequest(Value: TsmxCustomRequest); virtual;

    property StateCfg: TsmxStateCfg read GetStateCfg write SetStateCfg;
  public
    destructor Destroy; override;
    function CellParams(const Name: String; var Value: Variant): Boolean; override;

    property StateCfgClass: TsmxBaseCfgClass read GetStateCfgClass;
    property StateID: Integer read FStateID write SetStateID;
    property StateRequest: TsmxCustomRequest read FStateRequest write SetStateRequest;
  end;

  { TsmxCustomStandardForm }

  TsmxCustomStandardForm = class(TsmxCustomStateForm)
  private
    FAlgorithmList: TsmxCustomAlgorithmList;
    FControlBoard: TsmxCustomControlBoard;
    FMainMenu: TsmxCustomMainMenu;
    FRequestList: TsmxCustomRequestList;
    FStatusBoard: TsmxCustomStatusBoard;
    function GetSlave(Index: Integer): TsmxCustomPageManager;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPageManager);
  protected
    procedure InternalApply; override;
    procedure InternalPrepare; override;
    procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetControlBoard(Value: TsmxCustomControlBoard); virtual;
    procedure SetMainMenu(Value: TsmxCustomMainMenu); virtual;
    procedure SetRequestList(Value: TsmxCustomRequestList); virtual;
    procedure SetStatusBoard(Value: TsmxCustomStatusBoard); virtual;
  public
    function AddSlave: TsmxCustomPageManager;

    property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write SetAlgorithmList;
    property ControlBoard: TsmxCustomControlBoard read FControlBoard write SetControlBoard;
    property MainMenu: TsmxCustomMainMenu read FMainMenu write SetMainMenu;
    property RequestList: TsmxCustomRequestList read FRequestList write SetRequestList;
    property Slaves[Index: Integer]: TsmxCustomPageManager read GetSlave write SetSlave; default;
    property StatusBoard: TsmxCustomStatusBoard read FStatusBoard write SetStatusBoard;
  end;

implementation

uses
  DB, Variants, SysUtils, smxConsts, smxDBTypes, smxFuncs, smxDBFuncs,
  smxClassProcs, smxClassFuncs;

{ EsmxCellError }

constructor EsmxCellError.CreateByCfgID(Ident: Integer;
  const Args: array of const; CfgID: Integer; OriginMessage: String = '');
begin
  CreateByOrigin(Ident, Args, OriginMessage);
  FCfgID := CfgID;
end;

constructor EsmxCellError.CreateByCfgID(ResStringRec: PResStringRec;
  const Args: array of const; CfgID: Integer; OriginMessage: String = '');
begin
  CreateByOrigin(ResStringRec, Args, OriginMessage);
  FCfgID := CfgID;
end;

{ TsmxBaseCfg }

constructor TsmxBaseCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLDocIntf := smxFuncs.NewXML;
end;

destructor TsmxBaseCfg.Destroy;
begin
  FXMLDocIntf := nil;
  inherited Destroy;
end;

procedure TsmxBaseCfg.Assign(Source: TPersistent);
begin
  if Source is TsmxBaseCfg then
  begin
    CfgID := TsmxBaseCfg(Source).CfgID;
    XMLText := TsmxBaseCfg(Source).XMLText;
  end else
    inherited Assign(Source);
end;

procedure TsmxBaseCfg.Clear;
begin
end;

function TsmxBaseCfg.GetXMLText: String;
begin
  Write;
  Result := smxFuncs.FormatXMLText(FXMLDocIntf.XML.Text);
end;

procedure TsmxBaseCfg.SetXMLText(const Value: String);
begin
  FXMLDocIntf.XML.Text := smxFuncs.UnFormatXMLText(Value);
  FXMLDocIntf.Active := True;
  Read;
end;

procedure TsmxBaseCfg.Load;
var
  Value: Variant;
begin
  if not Assigned(FSelectRequest) or (FCfgID = 0) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'load'], FCfgID);
  if smxDBFuncs.GetValueByKey(FSelectRequest.DataSet,
      FCfgID, Value, FSelectRequest.PerformanceMode) then
    FXMLDocIntf.XML.Text := smxFuncs.GetSingleValue(Value, '') else
    FXMLDocIntf.XML.Text := '';
  try
    FXMLDocIntf.Active := True;
  except
    on E: Exception do
      raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
        [ClassName, FCfgID, 'load'], FCfgID, E.Message);
  end;
end;

procedure TsmxBaseCfg.Save;
var
  Request: IsmxDataSet;
  Performance: TsmxPerformanceMode;
  Key: Variant;
begin
  if not Assigned(FSelectRequest) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);
  if FCfgID = 0 then
  begin
    Request := FSelectRequest.ModifyRequests[mrInsert];
    Performance := FSelectRequest.ModifyPerformances[mrInsert];
  end else
  begin
    Request := FSelectRequest.ModifyRequests[mrUpdate];
    Performance := FSelectRequest.ModifyPerformances[mrUpdate];
  end;
  if not Assigned(Request) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);
  Key := FCfgID;
  if smxDBFuncs.SetValueByKey(Request, Key, FXMLDocIntf.XML.Text, Performance) then
    FCfgID := smxFuncs.GetSingleValue(Key, 0);
end;

procedure TsmxBaseCfg.Remove;
var
  Request: IsmxDataSet;
  Performance: TsmxPerformanceMode;
  Key: Variant;
begin
  if not Assigned(FSelectRequest) or (FCfgID = 0) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);
  Request := FSelectRequest.ModifyRequests[mrDelete];
  Performance := FSelectRequest.ModifyPerformances[mrDelete];
  if not Assigned(Request) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);
  Key := FCfgID;
  if smxDBFuncs.SetValueByKey(Request, Key, Variants.Null, Performance) then
    FCfgID := 0;
end;

procedure TsmxBaseCfg.Read;
begin
end;

procedure TsmxBaseCfg.Write;
begin
end;

procedure TsmxBaseCfg.Receive;
begin
  Load;
  Read;
end;

procedure TsmxBaseCfg.Return;
begin
  Write;
  Save;
end;

procedure TsmxBaseCfg.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

procedure TsmxBaseCfg.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
end;

{ TsmxBaseCell }

destructor TsmxBaseCell.Destroy;
begin
  SetCellParent(nil);
  if Assigned(FCellList) then
  begin
    ClearCells;
    FCellList.Free;
  end;
  inherited Destroy;
end;

procedure TsmxBaseCell.Assign(Source: TPersistent);
begin
  if Source is TsmxBaseCell then
  begin
    CfgID := TsmxBaseCell(Source).CfgID;
    DatabaseManager := TsmxBaseCell(Source).DatabaseManager;
    FormManager := TsmxBaseCell(Source).FormManager;
    ImageList := TsmxBaseCell(Source).ImageList;
    LibraryManager := TsmxBaseCell(Source).LibraryManager;
    SelectRequest := TsmxBaseCell(Source).SelectRequest;
    StorageManager := TsmxBaseCell(Source).StorageManager;
  end else
    inherited Assign(Source);
end;

function TsmxBaseCell.CellParams(const Name: String; var Value: Variant): Boolean;
begin
  if SysUtils.AnsiCompareText(Name, 'CfgID') = 0 then
  begin
    Value := FCfgID;
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

function TsmxBaseCell.FindByCfgID(ACfgID: Integer): TsmxBaseCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to CellCount - 1 do
    if Cells[i].CfgID = ACfgID then
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
    Result := FCellParent.CellList.IndexOf(Self) else
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

function TsmxBaseCell.GetDatabaseManager: IsmxDatabaseManager;
begin
  if not Assigned(FDatabaseManagerIntf) and Assigned(FCellParent) then
    FDatabaseManagerIntf := FCellParent.DatabaseManager;
  Result := FDatabaseManagerIntf;
end;

procedure TsmxBaseCell.SetDatabaseManager(const Value: IsmxDatabaseManager);
begin
  FDatabaseManagerIntf := Value;
end;

function TsmxBaseCell.GetFormManager: IsmxFormManager;
begin
  if not Assigned(FFormManagerIntf) and Assigned(FCellParent) then
    FFormManagerIntf := FCellParent.FormManager;
  Result := FFormManagerIntf;
end;

procedure TsmxBaseCell.SetFormManager(const Value: IsmxFormManager);
begin
  FFormManagerIntf := Value;
end;

function TsmxBaseCell.GetImageList: TCustomImageList;
begin
  if not Assigned(FImageList) and Assigned(FCellParent) then
    FImageList := FCellParent.ImageList;
  Result := FImageList;
end;

procedure TsmxBaseCell.SetImageList(Value: TCustomImageList);
begin
  FImageList := Value;
end;

function TsmxBaseCell.GetInternalObject: TObject;
begin
  Result := nil;
end;

function TsmxBaseCell.GetLibraryManager: IsmxLibraryManager;
begin
  if not Assigned(FLibraryManagerIntf) and Assigned(FCellParent) then
    FLibraryManagerIntf := FCellParent.LibraryManager;
  Result := FLibraryManagerIntf;
end;

procedure TsmxBaseCell.SetLibraryManager(const Value: IsmxLibraryManager);
begin
  FLibraryManagerIntf := Value;
end;

function TsmxBaseCell.GetStorageManager: IsmxStorageManager;
begin
  if not Assigned(FStorageManagerIntf) and Assigned(FCellParent) then
    FStorageManagerIntf := FCellParent.StorageManager;
  Result := FStorageManagerIntf;
end;

procedure TsmxBaseCell.SetStorageManager(const Value: IsmxStorageManager);
begin
  FStorageManagerIntf := Value;
end;

procedure TsmxBaseCell.Initialize;
begin
  InternalInitialize;
  DoInitialize;
end;

procedure TsmxBaseCell.InternalInitialize;
begin
end;

procedure TsmxBaseCell.SetCellParent(Value: TsmxBaseCell);
begin
  if Assigned(FCellParent) then
    FCellParent.CellList.Remove(Self);
  FCellParent := Value;
  if Assigned(FCellParent) then
    FCellParent.CellList.Add(Self);
end;

procedure TsmxBaseCell.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

procedure TsmxBaseCell.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
end;

{ TsmxTypeCfg }

procedure TsmxTypeCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxTypeCfg then
  begin
    CellClass := TsmxTypeCfg(Source).CellClass;
    CfgClass := TsmxTypeCfg(Source).CfgClass;
    IntfClass := TsmxTypeCfg(Source).IntfClass;
  end;
end;

procedure TsmxTypeCfg.Clear;
begin
  FCellClass := nil;
  FCellClassName := '';
  FCfgClass := nil;
  FCfgClassName := '';
  FIntfClass := nil;
  FIntfClassName := '';
end;

procedure TsmxTypeCfg.Read;
var
  r, n: IXMLNode;
begin
  Clear;
  r := FXMLDocIntf.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Cell');
  if Assigned(n) then
  begin
    CellClassName := n.Attributes['CellClassName'];
    CfgClassName := n.Attributes['CfgClassName'];
    IntfClassName := n.Attributes['IntfClassName'];
  end;
end;

procedure TsmxTypeCfg.SetCellClass(Value: TsmxBaseCellClass);
begin
  if FCellClass <> Value then
  begin
    FCellClass := Value;
    if Assigned(FCellClass) then
      FCellClassName := FCellClass.ClassName else
      FCellClassName := '';
  end;
end;

procedure TsmxTypeCfg.SetCellClassName(const Value: String);
begin
  if FCellClassName <> Value then
  begin
    FCellClassName := Value;
    if FCellClassName <> '' then
      FCellClass := TsmxBaseCellClass(Classes.FindClass(FCellClassName)) else
      FCellClass := nil;
  end;
end;

procedure TsmxTypeCfg.SetCfgClass(Value: TsmxBaseCfgClass);
begin
   if FCfgClass <> Value then
  begin
    FCfgClass := Value;
    if Assigned(FCfgClass) then
      FCfgClassName := FCfgClass.ClassName else
      FCfgClassName := '';
  end;
end;

procedure TsmxTypeCfg.SetCfgClassName(const Value: String);
begin
  if FCfgClassName <> Value then
  begin
    FCfgClassName := Value;
    if FCfgClassName <> '' then
      FCfgClass := TsmxBaseCfgClass(Classes.FindClass(FCfgClassName)) else
      FCfgClass := nil;
  end;
end;

procedure TsmxTypeCfg.SetIntfClass(Value: TClass);
begin
  if FIntfClass <> Value then
  begin
    FIntfClass := Value;
    if Assigned(FIntfClass) then
      FIntfClassName := FIntfClass.ClassName else
      FIntfClassName := '';
  end;
end;

procedure TsmxTypeCfg.SetIntfClassName(const Value: String);
begin
  if FIntfClassName <> Value then
  begin
    FIntfClassName := Value;
    if FIntfClassName <> '' then
      FIntfClass := Classes.FindClass(FIntfClassName) else
      FIntfClass := nil;
  end;
end;

procedure TsmxTypeCfg.Write;
var
  r, n: IXMLNode;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear
  else
    r := FXMLDocIntf.AddChild('Root');

  n := r.AddChild('Cell');
  n.Attributes['CellClassName'] := CellClassName;
  n.Attributes['CfgClassName'] := CfgClassName;
  n.Attributes['IntfClassName'] := IntfClassName;
end;

{ TsmxOwnerCell }

destructor TsmxOwnerCell.Destroy;
begin
  SetCellOwner(nil);
  if Assigned(FSlaveList) then
  begin
    ClearSlaves;
    FSlaveList.Free;
  end;
  inherited Destroy;
end;

procedure TsmxOwnerCell.Assign(Source: TPersistent);
var
  i: Integer;
begin
  inherited Assign(Source);
  if Source is TsmxOwnerCell then
  begin
    ClearSlaves;
    for i := 0 to TsmxOwnerCell(Source).SlaveCount - 1 do
      AddSlave.Assign(TsmxOwnerCell(Source).Slaves[i]);
  end;
end;

function TsmxOwnerCell.AddSlave: TsmxOwnerCell;
begin
  Result := SlaveClass.Create(Self);
  Result.FCellOwner := Self;
  Result.SlaveList.Add(Result);
end;

procedure TsmxOwnerCell.ClearSlaves;
var
  i: Integer;
begin
  for i := SlaveList.Count - 1 downto 0 do
    TsmxOwnerCell(SlaveList[i]).Free;
  SlaveList.Clear;
end;

procedure TsmxOwnerCell.DeleteSlave(Index: Integer);
begin
  TsmxOwnerCell(FSlaveList[Index]).Free;
end;

function TsmxOwnerCell.GetSlave(Index: Integer): TsmxOwnerCell;
begin
  Result := TsmxOwnerCell(SlaveList[Index]);
end;

procedure TsmxOwnerCell.SetSlave(Index: Integer; Value: TsmxOwnerCell);
begin
  TsmxOwnerCell(SlaveList[Index]).Assign(Value);
end;

function TsmxOwnerCell.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxOwnerCell;
end;

function TsmxOwnerCell.GetSlaveCount: Integer;
begin
  Result := SlaveList.Count;
end;

function TsmxOwnerCell.GetSlaveIndex: Integer;
begin
  if Assigned(FCellOwner) then
    Result := FCellOwner.SlaveList.IndexOf(Self) else
    Result := -1;
end;

procedure TsmxOwnerCell.SetSlaveIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  if Assigned(FCellOwner) then
  begin
    CurIndex := GetSlaveIndex;
    if (CurIndex >= 0) and (CurIndex <> Value) then
      FCellOwner.SlaveList.Move(CurIndex, Value);
  end;
end;

function TsmxOwnerCell.GetSlaveList: TList;
begin
  if not Assigned(FSlaveList) then
    FSlaveList := TList.Create;
  Result := FSlaveList;
end;

procedure TsmxOwnerCell.SetCellOwner(Value: TsmxOwnerCell);
begin
  if Assigned(FCellOwner) then
    FCellOwner.SlaveList.Remove(Self);
  FCellOwner := Value;
  if Assigned(FCellOwner) then
    FCellOwner.SlaveList.Add(Self);
end;

{ TsmxControlCell }

procedure TsmxControlCell.Apply;
begin
  InternalApply;
  DoApply;
end;

procedure TsmxControlCell.Prepare;
begin
  InternalPrepare;
  DoPrepare;
end;

procedure TsmxControlCell.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxControlCell then
  begin
    CellAlign := TsmxControlCell(Source).CellAlign;
    CellCursor := TsmxControlCell(Source).CellCursor;
    CellEnable := TsmxControlCell(Source).CellEnable;
    CellHeight := TsmxControlCell(Source).CellHeight;
    CellLeft := TsmxControlCell(Source).CellLeft;
    CellTop := TsmxControlCell(Source).CellTop;
    CellVisible := TsmxControlCell(Source).CellVisible;
    CellWidth := TsmxControlCell(Source).CellWidth;
  end;
end;

procedure TsmxControlCell.Backup;
begin
  DoBackup;
  InternalBackup;
end;

procedure TsmxControlCell.Restore;
begin
  InternalRestore;
  DoRestore;
end;

procedure TsmxControlCell.ChangeActiveControl;
begin
  InternalChangeAcitveControl;
  DoChangeActiveControl;
end;

procedure TsmxControlCell.DoApply;
begin
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TsmxControlCell.DoBackup;
begin
  if Assigned(FOnBackup) then
    FOnBackup(Self);
end;

procedure TsmxControlCell.DoChangeActiveControl;
begin
  if Assigned(FOnChangeActiveControl) then
    FOnChangeActiveControl(Self);
end;

procedure TsmxControlCell.DoPrepare;
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
end;

procedure TsmxControlCell.DoRestore;
begin
  if Assigned(FOnRestore) then
    FOnRestore(Self);
end;

function TsmxControlCell.GetCellActive: Boolean;
begin
  Result := False;
end;

procedure TsmxControlCell.SetCellActive(Value: Boolean);
begin
end;

function TsmxControlCell.GetCellAlign: TAlign;
begin
  Result := alNone;
end;

procedure TsmxControlCell.SetCellAlign(Value: TAlign);
begin
end;

function TsmxControlCell.GetCellCursor: TCursor;
begin
  Result := crDefault;
end;

procedure TsmxControlCell.SetCellCursor(Value: TCursor);
begin
end;

function TsmxControlCell.GetCellEnable: Boolean;
begin
  Result := False;
end;

procedure TsmxControlCell.SetCellEnable(Value: Boolean);
begin
end;

function TsmxControlCell.GetCellHeight: Integer;
begin
  Result := 0;
end;

procedure TsmxControlCell.SetCellHeight(Value: Integer);
begin
end;

function TsmxControlCell.GetCellLeft: Integer;
begin
  Result := 0;
end;

procedure TsmxControlCell.SetCellLeft(Value: Integer);
begin
end;

function TsmxControlCell.GetCellTop: Integer;
begin
  Result := 0;
end;

procedure TsmxControlCell.SetCellTop(Value: Integer);
begin
end;

function TsmxControlCell.GetCellVisible: Boolean;
begin
  Result := False;
end;

procedure TsmxControlCell.SetCellVisible(Value: Boolean);
begin
end;

function TsmxControlCell.GetCellWidth: Integer;
begin
  Result := 0;
end;

procedure TsmxControlCell.SetCellWidth(Value: Integer);
begin
end;

procedure TsmxControlCell.InternalApply;
begin
end;

procedure TsmxControlCell.InternalBackup;
begin
end;

procedure TsmxControlCell.InternalChangeAcitveControl;
begin
end;

procedure TsmxControlCell.InternalPrepare;
begin
end;

procedure TsmxControlCell.InternalRestore;
begin
end;

procedure TsmxControlCell.SetPopupMenu(Value: TsmxCustomPopupMenu);
begin
  FPopupMenu := Value;
end;

{ TsmxActionCell }

procedure TsmxActionCell.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxActionCell then
  begin
    CellCaption := TsmxActionCell(Source).CellCaption;
    CellHint := TsmxActionCell(Source).CellHint;
    CellHotKey := TsmxActionCell(Source).CellHotKey;
    CellImageIndex := TsmxActionCell(Source).CellImageIndex;
  end;
end;

procedure TsmxActionCell.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TsmxActionCell.Execute;
begin
  DoExecute;
  InternalExecute;
end;

function TsmxActionCell.GetCellCaption: String;
begin
  Result := '';
end;

procedure TsmxActionCell.SetCellCaption(const Value: String);
begin
end;

function TsmxActionCell.GetCellHint: String;
begin
  Result := '';
end;

procedure TsmxActionCell.SetCellHint(const Value: String);
begin
end;

function TsmxActionCell.GetCellHotKey: Integer;
begin
  Result := 0;
end;

procedure TsmxActionCell.SetCellHotKey(Value: Integer);
begin
end;

function TsmxActionCell.GetCellImageIndex: Integer;
begin
  Result := -1;
end;

procedure TsmxActionCell.SetCellImageIndex(Value: Integer);
begin
end;

procedure TsmxActionCell.InternalExecute;
begin
  if Assigned(FAlgorithm) then
  begin
    FAlgorithm.ActionCell := Self;
    FAlgorithm.RefreshParams;
    FAlgorithm.Execute;
  end;
end;

procedure TsmxActionCell.SetAlgorithm(Value: TsmxCustomAlgorithm);
begin
  FAlgorithm := Value;
  if Assigned(FAlgorithm) then
  begin
    CellCaption := FAlgorithm.AlgorithmCaption;
    CellHint := FAlgorithm.AlgorithmHint;
    CellHotKey := FAlgorithm.AlgorithmHotKey;
    CellImageIndex := FAlgorithm.AlgorithmImageIndex;
  end;
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

{ TsmxParams }

function TsmxParams.Add: TsmxParam;
begin
  Result := TsmxParam(inherited Add);
end;

function TsmxParams.FindByName(const AParamName: String): TsmxParam;
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

procedure TsmxParams.SetItem(Index: Integer; Value: TsmxParam);
begin
  inherited Items[Index] := Value;
end;

function TsmxParams.GetValue(const Name: String): Variant;
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
end;

{ TsmxTargetRequest }

destructor TsmxTargetRequest.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  FDatabaseIntf := nil;
  FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxTargetRequest.ClearParams;
begin
  ParamList.Clear;
end;

procedure TsmxTargetRequest.DoExecute(const SQLText: String;
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil);
begin
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    if FDataSetIntf.DataSetType <> RequestType then
      FDataSetIntf := NewRequest('', RequestType);
  end else
    FDataSetIntf := NewRequest('', RequestType);

  if FDataSetIntf.SQL.Text <> SQLText then
    FDataSetIntf.SQL.Text := SQLText;
  PrepRequest(FDataSetIntf, True, pmExecute, DSFrom);
end;

function TsmxTargetRequest.DoRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  ResField: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
begin
  with ForRequest(SQLText, RequestType, True, pmOpen, DSFrom) do
  try
    if ResField = '' then
      ResField := Fields[0].FieldName;
    Result := FieldByName(ResField).Value;
  finally
    Close;
  end;
end;

function TsmxTargetRequest.ForRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Get: Boolean = False; Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    if FDataSetIntf.DataSetType <> RequestType then
      FDataSetIntf := NewRequest('', RequestType);
  end else
    FDataSetIntf := NewRequest('', RequestType);

  if FDataSetIntf.SQL.Text <> SQLText then
    FDataSetIntf.SQL.Text := SQLText;
  PrepRequest(FDataSetIntf, Get, Perform, DSFrom);
  Result := FDataSetIntf;
end;

function TsmxTargetRequest.GetParamList: TsmxParams;
begin
  if not Assigned(FParamList) then
    FParamList := TsmxParams.Create(TsmxParam);
  Result := FParamList;
end;

function TsmxTargetRequest.GetValue(const Key: String): Variant;
begin
  Result := FParamList.Values[Key];
end;

procedure TsmxTargetRequest.SetValue(const Key: String; const Value: Variant);
begin
  FParamList.Values[Key] := Value;
end;

function TsmxTargetRequest.NewRequest(const SQLText: String = '';
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  Result := nil;
  if Assigned(Database) then
    Result := Database.NewDataSet(RequestType)
  else
    raise EsmxComponentError.CreateResFmt(@smxConsts.rsActionError, [ClassName, 'NewRequest']);
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
            Params[i].AssignParam(Field)
          else
            Params[i].Clear;
        end else
          Params[i].Value := FParamList.Values[Params[i].ParamName];
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
          FParamList.Values[Params[i].ParamName] := Params[i].Value;
        end else
        if Params[i].ParamType = ptResult then
        begin
          FParamList.Values[Params[i].ParamName] := Params[i].Value;
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
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    FDataSetIntf := nil;
  end;
  FDatabaseIntf := Value;
end;

{ TsmxCustomRequest }

destructor TsmxCustomRequest.Destroy;
begin
  FDatabaseIntf := nil;
  FInsertRequestIntf := nil;
  FUpdateRequestIntf := nil;
  FDeleteRequestIntf := nil;
  FCurDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxCustomRequest.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomRequest then
  begin
    DatabaseName := TsmxCustomRequest(Source).DatabaseName;
    DataSet := TsmxCustomRequest(Source).DataSet;
    ModifyPerformances[mrInsert] := TsmxCustomRequest(Source).ModifyPerformances[mrInsert];
    ModifyPerformances[mrUpdate] := TsmxCustomRequest(Source).ModifyPerformances[mrUpdate];
    ModifyPerformances[mrDelete] := TsmxCustomRequest(Source).ModifyPerformances[mrDelete];
    ModifyRequests[mrInsert] := TsmxCustomRequest(Source).ModifyRequests[mrInsert];
    ModifyRequests[mrUpdate] := TsmxCustomRequest(Source).ModifyRequests[mrUpdate];
    ModifyRequests[mrDelete] := TsmxCustomRequest(Source).ModifyRequests[mrDelete];
    OperationMode := TsmxCustomRequest(Source).OperationMode;
    PerformanceMode := TsmxCustomRequest(Source).PerformanceMode;
  end;
end;

procedure TsmxCustomRequest.Delete;
begin
  FCurDataSetIntf := FDeleteRequestIntf;
  FCurPerformanceMode := FDeletePerformance;
  InternalPrepare;
  DoPrepare;
  InternalRefreshParams;
  DoRefreshParams;
  //DoModify(mrDelete);
  //InternalModify;
  DoDelete;
  InternalDelete;
end;

procedure TsmxCustomRequest.DoDelete;
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self);
end;

procedure TsmxCustomRequest.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TsmxCustomRequest.DoInsert;
begin
  if Assigned(FOnInsert) then
    FOnInsert(Self);
end;

{procedure TsmxCustomRequest.DoModify(Modify: TsmxModifyRequest);
begin
  if Assigned(FOnModify) then
    FOnModify(Self, Modify);
end;}

procedure TsmxCustomRequest.DoPrepare;
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
end;

procedure TsmxCustomRequest.DoRefreshParams;
begin
  if Assigned(FOnRefreshParams) then
    FOnRefreshParams(Self);
end;

procedure TsmxCustomRequest.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TsmxCustomRequest.Execute;
begin
  FCurDataSetIntf := DataSet;
  FCurPerformanceMode := FPerformanceMode;
  DoExecute;
  InternalExecute;
end;

function TsmxCustomRequest.FindDatabase: IsmxDatabase;
var
  Connection: IsmxConnection;
begin
  if Assigned(DatabaseManager) and (FDatabaseName <> '') then
  begin
    Connection := DatabaseManager.FindByName(FDatabaseName);
    if Assigned(Connection) then
      Result := Connection.Database;
  end else
    Result := nil;
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
  Result := nil;
end;

procedure TsmxCustomRequest.SetDataSet(const Value: IsmxDataSet);
begin
  FDataSetIntf := Value;
end;

function TsmxCustomRequest.GetModifyPerformance(Modify: TsmxModifyRequest): TsmxPerformanceMode;
begin
  Result := pmOpen;
  case Modify of
    mrInsert: Result := FInsertPerformance;
    mrUpdate: Result := FUpdatePerformance;
    mrDelete: Result := FDeletePerformance;
  end;
end;

procedure TsmxCustomRequest.SetModifyPerformance(Modify: TsmxModifyRequest; Value: TsmxPerformanceMode);
begin
  case Modify of
    mrInsert: FInsertPerformance := Value;
    mrUpdate: FUpdatePerformance := Value;
    mrDelete: FDeletePerformance := Value;
  end;
end;

function TsmxCustomRequest.GetModifyRequest(Modify: TsmxModifyRequest): IsmxDataSet;
begin
  Result := nil;
  case Modify of
    mrInsert: Result := FInsertRequestIntf;
    mrUpdate: Result := FUpdateRequestIntf;
    mrDelete: Result := FDeleteRequestIntf;
  end;
end;

procedure TsmxCustomRequest.Insert;
begin
  FCurDataSetIntf := FInsertRequestIntf;
  FCurPerformanceMode := FInsertPerformance;
  InternalPrepare;
  DoPrepare;
  InternalRefreshParams;
  DoRefreshParams;
  //DoModify(mrInsert);
  //InternalModify;
  DoInsert;
  InternalInsert;
end;

procedure TsmxCustomRequest.InternalDelete;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.InternalExecute;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.InternalInsert;
begin
  PerformRequest;
end;

{procedure TsmxCustomRequest.InternalModify;
begin
  PerformRequest;
end;}

procedure TsmxCustomRequest.InternalPrepare;
begin
  if Assigned(FCurDataSetIntf) then
  begin
    if not Assigned(FCurDataSetIntf.Database) then
      FCurDataSetIntf.Database := FDatabaseIntf;
    if not FCurDataSetIntf.Prepared then
      FCurDataSetIntf.Prepared := True;
    if (FOperationMode = omAutomatic) and (FCurDataSetIntf = DataSet) then
    begin
      RefreshParams;
      Execute;
    end;
  end;
end;

procedure TsmxCustomRequest.InternalRefreshParams;
var
  i, j: integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
begin
  if not Assigned(FCurDataSetIntf) then
    Exit;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to FCurDataSetIntf.ParamCount - 1 do
    begin
      Value := Variants.Null;
      case FCurDataSetIntf.Params[i].ParamLocation of
        plConst .. plOutput:
        begin
          Value := FCurDataSetIntf.Params[i].Value;
        end;
        plFilterDesk:
        begin
          smxClassProcs.AllParents(Self, List, [TsmxCustomSection]);
          if List.Count > 0 then
            smxClassFuncs.FindFilterOnSection(TsmxCustomSection(List[0]),
              FCurDataSetIntf.Params[i].ParamName, Value);
        end;
        plGrid:
        begin
          smxClassProcs.AllParents(Self, List, [TsmxCustomPage]);
          if List.Count > 0 then
            for j := 0 to TsmxCustomPage(List[0]).SlaveCount - 1 do
              if not smxClassFuncs.ExistsParent(Self, TsmxCustomPage(List[0]).Slaves[j]) then
                if smxClassFuncs.FindColumnOnSection(TsmxCustomPage(List[0]).Slaves[j],
                    FCurDataSetIntf.Params[i].ParamName, Value) then
                  Break;
        end;
        plParentFilterDesk:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm]);
          for j := 0 to List.Count - 1 do
            if FindFilterOnForm(TsmxCustomForm(List[j]), FCurDataSetIntf.Params[i].ParamName, Value) then
              Break;
        end;
        plParentGrid:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm]);
          for j := 0 to List.Count - 1 do
            if FindColumnOnForm(TsmxCustomForm(List[j]), FCurDataSetIntf.Params[i].ParamName, Value) then
              Break;
        end;
        plStorageParams:
        begin
          if Assigned(StorageManager) then
            Value := StorageManager.Values[FCurDataSetIntf.Params[i].ParamName];
        end;
        plParentParams:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(FCurDataSetIntf.Params[i].ParamName, Value) then
              Break;
        end;
      end;
      FCurDataSetIntf.Params[i].Value := Value;
    end;
  finally
    List.Free;
  end;
end;

procedure TsmxCustomRequest.InternalUpdate;
begin
  PerformRequest;
end;

procedure TsmxCustomRequest.PerformRequest;
begin
  if Assigned(FCurDataSetIntf) then
  begin
    try
      FCurDataSetIntf.Close;
      case FCurPerformanceMode of
        pmOpen: FCurDataSetIntf.Open;
        pmExecute: FCurDataSetIntf.Execute;
      end;
    except
      on E: Exception do
        raise EsmxCellError.CreateByCfgID(@smxConsts.rsCellActionError,
          [ClassName, 'execute'], CfgID, E.Message);
    end;
  end;
end;

procedure TsmxCustomRequest.Prepare;
begin
  FCurDataSetIntf := DataSet;
  FCurPerformanceMode := FPerformanceMode;
  InternalPrepare;
  DoPrepare;
end;

procedure TsmxCustomRequest.RefreshParams;
begin
  FCurDataSetIntf := DataSet;
  FCurPerformanceMode := FPerformanceMode;
  InternalRefreshParams;
  DoRefreshParams;
end;

procedure TsmxCustomRequest.SetDatabaseManager(const Value: IsmxDatabaseManager);
begin
  inherited SetDatabaseManager(Value);
  Database := FindDatabase;
end;

procedure TsmxCustomRequest.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
  FDatabaseIntf := FindDatabase;
end;

procedure TsmxCustomRequest.SetModifyRequest(Modify: TsmxModifyRequest; const Value: IsmxDataSet);
begin
  case Modify of
    mrInsert: FInsertRequestIntf := Value;
    mrUpdate: FUpdateRequestIntf := Value;
    mrDelete: FDeleteRequestIntf := Value;
  end;
end;

procedure TsmxCustomRequest.SetOperationMode(Value: TsmxOperationMode);
begin
  FOperationMode := Value;
end;

procedure TsmxCustomRequest.SetPerformanceMode(Value: TsmxPerformanceMode);
begin
  FPerformanceMode := Value;
end;

procedure TsmxCustomRequest.Update;
begin
  FCurDataSetIntf := FUpdateRequestIntf;
  FCurPerformanceMode := FUpdatePerformance;
  InternalPrepare;
  DoPrepare;
  InternalRefreshParams;
  DoRefreshParams;
  //DoModify(mrUpdate);
  //InternalModify;
  DoUpdate;
  InternalUpdate;
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

{ TsmxCustomColumn }

procedure TsmxCustomColumn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomColumn then
  begin
    ColumnAlignment := TsmxCustomColumn(Source).ColumnAlignment;
    ColumnColor := TsmxCustomColumn(Source).ColumnColor;
    ColumnFont := TsmxCustomColumn(Source).ColumnFont;
    ColumnName := TsmxCustomColumn(Source).ColumnName;
    ColumnCaption := TsmxCustomColumn(Source).ColumnCaption;
    ColumnValue := TsmxCustomColumn(Source).ColumnValue;
    FieldName := TsmxCustomColumn(Source).FieldName;
    HeaderAlignment := TsmxCustomColumn(Source).HeaderAlignment;
    HeaderColor := TsmxCustomColumn(Source).HeaderColor;
    HeaderFont := TsmxCustomColumn(Source).HeaderFont;
    HeaderCaption := TsmxCustomColumn(Source).HeaderCaption;
  end;
end;

procedure TsmxCustomColumn.DoPressHeader;
begin
  if Assigned(FOnPressHeader) then
    FOnPressHeader(Self);
end;

function TsmxCustomColumn.GetCellOwner: TsmxCustomGrid;
begin
  Result := TsmxCustomGrid(inherited CellOwner);
end;

procedure TsmxCustomColumn.SetCellOwner(Value: TsmxCustomGrid);
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
  Result := clBlack;
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

function TsmxCustomColumn.GetColumnValue: Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomColumn.SetColumnValue(const Value: Variant);
begin
end;

procedure TsmxCustomColumn.SetColumnName(const Value: String);
begin
  FColumnName := Value;
end;

function TsmxCustomColumn.GetFieldName: String;
begin
  Result := '';
end;

procedure TsmxCustomColumn.SetFieldName(const Value: String);
begin
end;

function TsmxCustomColumn.GetHeaderAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

procedure TsmxCustomColumn.SetHeaderAlignment(Value: TAlignment);
begin
end;

function TsmxCustomColumn.GetHeaderColor: TColor;
begin
  Result := clBlack;
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

function TsmxCustomColumn.GetHeaderCaption: String;
begin
  Result := '';
end;

procedure TsmxCustomColumn.SetHeaderCaption(const Value: String);
begin
end;

procedure TsmxCustomColumn.InternalPressHeader;
begin
end;

procedure TsmxCustomColumn.PressHeader;
begin
  InternalPressHeader;
  DoPressHeader;
end;

{ TsmxCustomGrid }

function TsmxCustomGrid.AddSlave: TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(inherited AddSlave);
end;

procedure TsmxCustomGrid.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomGrid then
    Options := TsmxCustomGrid(Source).Options;
end;

procedure TsmxCustomGrid.DoChangeRow;
begin
  if Assigned(FOnChangeRow) then
    FOnChangeRow(Self);
end;

procedure TsmxCustomGrid.DoPressDouble;
begin
  if Assigned(FOnPressDouble) then
    FOnPressDouble(Self);
end;

function TsmxCustomGrid.FindColumnByName(const AColumnName: String): TsmxCustomColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
    if SysUtils.AnsiCompareText(Slaves[i].ColumnName, AColumnName) = 0 then
    begin
      Result := Slaves[i];
      Break;
    end;
end;

function TsmxCustomGrid.GetSlave(Index: Integer): TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(inherited Slaves[Index]);
end;

procedure TsmxCustomGrid.SetSlave(Index: Integer; Value: TsmxCustomColumn);
begin
  inherited Slaves[Index] := Value;
end;

procedure TsmxCustomGrid.InternalChangeRow;
begin
end;

procedure TsmxCustomGrid.InternalPressDouble;
begin
end;

procedure TsmxCustomGrid.ChangeRow;
begin
  InternalChangeRow;
  DoChangeRow;
end;

procedure TsmxCustomGrid.PressDouble;
begin
  InternalPressDouble;
  DoPressDouble;
end;

procedure TsmxCustomGrid.SetOptions(Value: TsmxGridOptions);
begin
  FOptions := Value;
end;

procedure TsmxCustomGrid.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
end;

{ TsmxAlgorithmParam }

procedure TsmxAlgorithmParam.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmParam then
  begin
    DataType := TsmxAlgorithmParam(Source).DataType;
    ParamLocation := TsmxAlgorithmParam(Source).ParamLocation;
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

destructor TsmxCustomAlgorithm.Destroy;
begin
  if Assigned(FAlgorithmParams) then
    FAlgorithmParams.Free;
  {if Assigned(FClientCells) then
  begin
    ClearActions;
    FClientCells.Free;
  end;}
  inherited Destroy;
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

{procedure TsmxCustomAlgorithm.ClearActions;
var
  i: Integer;
begin
  for i := 0 to ClientCells.Count - 1 do
    TsmxActionCell(ClientCells[i]).Algorithm := nil;
end;}

procedure TsmxCustomAlgorithm.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TsmxCustomAlgorithm.DoRefreshParams;
begin
  if Assigned(FOnRefreshParams) then
    FOnRefreshParams(Self);
end;

procedure TsmxCustomAlgorithm.Execute;
begin
  InternalExecute;
  DoExecute;
end;

function TsmxCustomAlgorithm.GetAlgorithmCaption: String;
begin
  Result := '';
end;

procedure TsmxCustomAlgorithm.SetAlgorithmCaption(const Value: String);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmEnable: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmEnable(Value: Boolean);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmHotKey: Integer;
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

procedure TsmxCustomAlgorithm.SetAlgorithmHotKey(Value: Integer);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmImageIndex: Integer;
begin
  Result := -1;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmImageIndex(Value: Integer);
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

{function TsmxCustomAlgorithm.GetClientCells: TList;
begin
  if not Assigned(FClientCells) then
    FClientCells := TList.Create;
  Result := FClientCells;
end;}

{procedure TsmxCustomAlgorithm.InsertClient(AClient: TsmxActionCell);
begin
  if ClientCells.IndexOf(AClient) = -1 then
    ClientCells.Add(AClient);
end;}

{procedure TsmxCustomAlgorithm.RemoveClient(AClient: TsmxActionCell);
begin
  if ClientCells.IndexOf(AClient) <> -1 then
    ClientCells.Remove(AClient);
end;}

procedure TsmxCustomAlgorithm.InternalExecute;
begin
end;

procedure TsmxCustomAlgorithm.InternalRefreshParams;
var
  i, j: integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
begin
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to AlgorithmParams.Count - 1 do
    begin
      Value := Variants.Null;
      case AlgorithmParams[i].ParamLocation of
        plConst .. plOutput:
        begin
          Value := AlgorithmParams[i].ParamValue;
        end;
        plFilterDesk:
        begin
          smxClassFuncs.FindFilterOnForm(Form, AlgorithmParams[i].ParamName, Value);
        end;
        plGrid:
        begin
          smxClassFuncs.FindColumnOnForm(Form, AlgorithmParams[i].ParamName, Value);
        end;
        plParentFilterDesk:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm]);
          for j := 0 to List.Count - 1 do
            if FindFilterOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
              Break;
        end;
        plParentGrid:
        begin
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm]);
          for j := 0 to List.Count - 1 do
            if FindColumnOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
              Break;
        end;
        plStorageParams:
        begin
          if Assigned(StorageManager) then
            Value := StorageManager.Values[AlgorithmParams[i].ParamName];
        end;
        plParentParams:
        begin
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(AlgorithmParams[i].ParamName, Value) then
              Break;
        end;
      end;
      AlgorithmParams[i].ParamValue := Value;
    end;
  finally
    List.Free;
  end;
end;

procedure TsmxCustomAlgorithm.RefreshParams;
begin
  InternalRefreshParams;
  DoRefreshParams;
end;

procedure TsmxCustomAlgorithm.SetActionCell(Value: TsmxActionCell);
begin
  FActionCell := Value;
end;

{procedure TsmxCustomAlgorithm.SetOnExecute(Value: TsmxComponentEvent);
var
  i: Integer;
begin
  FOnExecute := Value;
  for i := 0 to ClientCells.Count - 1 do
    TsmxActionCell(ClientCells[i]).OnExecute := FOnExecute;
end;}

{ TsmxCustomLibAlgorithm }

procedure TsmxCustomLibAlgorithm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomLibAlgorithm then
  begin
    AlgorithmLibrary := TsmxCustomLibAlgorithm(Source).AlgorithmLibrary;
    AlgorithmProcName := TsmxCustomLibAlgorithm(Source).AlgorithmProcName;
  end;
end;

procedure TsmxCustomLibAlgorithm.SetAlgorithmLibrary(const Value: String);
begin
  FAlgorithmLibrary := Value;
end;

procedure TsmxCustomLibAlgorithm.SetAlgorithmProcName(const Value: String);
begin
  FAlgorithmProcName := Value;
end;

{ TsmxCustomAlgorithmList }

function TsmxCustomAlgorithmList.AddSlave: TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(inherited AddSlave);
end;

function TsmxCustomAlgorithmList.GetSlave(Index: Integer): TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(inherited Slaves[Index]);
end;

procedure TsmxCustomAlgorithmList.SetSlave(Index: Integer; Value: TsmxCustomAlgorithm);
begin
  inherited Slaves[Index] := Value;
end;

{ TsmxCustomFilter }

procedure TsmxCustomFilter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomFilter then
  begin
    FilterName := TsmxCustomFilter(Source).FilterName;
    FilterOptions := TsmxCustomFilter(Source).FilterOptions;
    FilterValue := TsmxCustomFilter(Source).FilterValue;
  end;
end;

procedure TsmxCustomFilter.Change;
begin
  InternalChange;
  DoChange;
end;

procedure TsmxCustomFilter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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

function TsmxCustomFilter.GetFilterValue: Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomFilter.SetFilterValue(const Value: Variant);
var
  Text: Variant;
begin
  if Assigned(FRequest) then
    if smxDBFuncs.GetValueByKey(FRequest.DataSet, Value,
        Text, FRequest.PerformanceMode) then
      CellCaption := Variants.VarToStr(Text[0]) else
      CellCaption := '';
end;

function TsmxCustomFilter.GetValueFormat: String;
begin
  Result := '';
end;

procedure TsmxCustomFilter.SetValueFormat(const Value: String);
begin
end;

procedure TsmxCustomFilter.InternalChange;
begin
end;

procedure TsmxCustomFilter.SetFilterName(const Value: String);
begin
  FFilterName := Value;
end;

procedure TsmxCustomFilter.SetFilterOptions(Value: TsmxFilterOptions);
begin
  FFilterOptions := Value;
end;

procedure TsmxCustomFilter.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
end;

{ TsmxCustomFilterDesk }

function TsmxCustomFilterDesk.AddSlave: TsmxCustomFilter;
begin
  Result := TsmxCustomFilter(inherited AddSlave);
end;

function TsmxCustomFilterDesk.FindFilterByName(const AFilterName: String): TsmxCustomFilter;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
    if SysUtils.AnsiCompareText(Slaves[i].FilterName, AFilterName) = 0 then
    begin
      Result := Slaves[i];
      Break;
    end;
end;

function TsmxCustomFilterDesk.GetSlave(Index: Integer): TsmxCustomFilter;
begin
  Result := TsmxCustomFilter(inherited Slaves[Index]);
end;

procedure TsmxCustomFilterDesk.SetSlave(Index: Integer; Value: TsmxCustomFilter);
begin
  inherited Slaves[Index] := Value;
end;

procedure TsmxCustomFilterDesk.InternalApply;
var
  i: Integer;
begin
  if Assigned(FRequest) then
    if Assigned(FRequest.ModifyRequests[mrUpdate]) then
    begin
      if FRequest.ModifyPerformances[mrUpdate] = pmOpen then
        FRequest.ModifyRequests[mrUpdate].Edit;
      for i := 0 to SlaveCount - 1 do
        if foApplyValue in Slaves[i].FilterOptions then
          case FRequest.ModifyPerformances[mrUpdate] of
            pmOpen: FRequest.ModifyRequests[mrUpdate].FieldByName(Slaves[i].FilterName).Value := Slaves[i].FilterValue;
            pmExecute: FRequest.ModifyRequests[mrUpdate].ParamByName(Slaves[i].FilterName).Value := Slaves[i].FilterValue;
          end;
      FRequest.Update;
      FRequest.ModifyRequests[mrUpdate].Post;
    end;
end;

procedure TsmxCustomFilterDesk.InternalPrepare;
var
  i: Integer;
begin
  if Assigned(FRequest) then
    if Assigned(FRequest.DataSet) then
    begin
      FRequest.Prepare;
      if FRequest.OperationMode = omManual then
      begin
        FRequest.RefreshParams;
        FRequest.Execute;
      end;
      for i := 0 to SlaveCount - 1 do
        if foPrepareValue in Slaves[i].FilterOptions then
          case FRequest.PerformanceMode of
            pmOpen: Slaves[i].FilterValue := FRequest.DataSet.FieldByName(Slaves[i].FilterName).Value;
            pmExecute: Slaves[i].FilterValue := FRequest.DataSet.ParamByName(Slaves[i].FilterName).Value;
          end;
    end;
end;

procedure TsmxCustomFilterDesk.SetRequest(Value: TsmxCustomRequest);
begin
  FRequest := Value;
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

procedure TsmxCustomSection.SetFilterDesk(Value: TsmxCustomFilterDesk);
begin
  FFilterDesk := Value;
end;

procedure TsmxCustomSection.SetGrid(Value: TsmxCustomGrid);
begin
  FGrid := Value;
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

{ TsmxCustomPageManager }

function TsmxCustomPageManager.AddSlave: TsmxCustomPage;
begin
  Result := TsmxCustomPage(inherited AddSlave);
end;

function TsmxCustomPageManager.GetSlave(Index: Integer): TsmxCustomPage;
begin
  Result := TsmxCustomPage(inherited Slaves[Index]);
end;

procedure TsmxCustomPageManager.SetSlave(Index: Integer; Value: TsmxCustomPage);
begin
  inherited Slaves[Index] := Value;
end;

{ TsmxCustomMenuItem }

function TsmxCustomMenuItem.AddSlave: TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited AddSlave);
end;

function TsmxCustomMenuItem.GetCellOwner: TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited CellOwner);
end;

procedure TsmxCustomMenuItem.SetCellOwner(Value: TsmxCustomMenuItem);
begin
  inherited CellOwner := Value;
end;


function TsmxCustomMenuItem.GetSlave(Index: Integer): TsmxCustomMenuItem;
begin
  Result := TsmxCustomMenuItem(inherited Slaves[Index]);
end;

procedure TsmxCustomMenuItem.SetSlave(Index: Integer; Value: TsmxCustomMenuItem);
begin
  inherited Slaves[Index] := Value;
end;

{ TsmxCustomMainMenu }

procedure TsmxCustomMainMenu.SetMenuItemList(Value: TsmxCustomMenuItem);
begin
  FMenuItems := Value;
end;

{ TsmxCustomPopupMenu }

procedure TsmxCustomPopupMenu.SetMenuItemList(Value: TsmxCustomMenuItem);
begin
  FMenuItems := Value;
end;

{ TsmxCustomToolItem }

function TsmxCustomToolItem.GetCellOwner: TsmxCustomToolBoard;
begin
  Result := TsmxCustomToolBoard(inherited CellOwner);
end;

procedure TsmxCustomToolItem.SetCellOwner(Value: TsmxCustomToolBoard);
begin
  inherited CellOwner := Value;
end;

{ TsmxCustomToolBoard }

function TsmxCustomToolBoard.AddSlave: TsmxCustomToolItem;
begin
  Result := TsmxCustomToolItem(inherited AddSlave);
end;

function TsmxCustomToolBoard.GetSlave(Index: Integer): TsmxCustomToolItem;
begin
  Result := TsmxCustomToolItem(inherited Slaves[Index]);
end;

procedure TsmxCustomToolBoard.SetSlave(Index: Integer; Value: TsmxCustomToolItem);
begin
  inherited Slaves[Index] := Value;
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

{ TsmxCustomStatusItem }

function TsmxCustomStatusItem.GetCellOwner: TsmxCustomStatusBoard;
begin
  Result := TsmxCustomStatusBoard(inherited CellOwner);
end;

procedure TsmxCustomStatusItem.SetCellOwner(Value: TsmxCustomStatusBoard);
begin
  inherited CellOwner := Value;
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

{ TsmxCustomForm }

constructor TsmxCustomForm.CreateByID(AOwner: TComponent; AID: Integer);
begin
  Create(AOwner);
  FID := AID;
end;

destructor TsmxCustomForm.Destroy;
begin
  SetFormManager(nil);
  inherited Destroy;
end;

procedure TsmxCustomForm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCustomForm then
    IntfID := TsmxCustomForm(Source).IntfID;
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

procedure TsmxCustomForm.Close;
begin
end;

procedure TsmxCustomForm.Show;
begin
end;

function TsmxCustomForm.GetCfgID: Integer;
begin
  Result := FCfgID;
end;

function TsmxCustomForm.GetInternalRef: Integer;
begin
  Result := Integer(Self);
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

procedure TsmxCustomForm.SetFormManager(const Value: IsmxFormManager);
begin
  if Assigned(FormManager) then
    FormManager.RemoveForm(Self);
  inherited SetFormManager(Value);
  if Assigned(FormManager) then
    FormManager.InsertForm(Self);
end;

procedure TsmxCustomForm.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

function TsmxCustomForm.ShowModal: TModalResult;
begin
  Result := mrNone;
end;

{ TsmxStateUnit }

function TsmxStateUnit.Add: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Add);
end;

procedure TsmxStateUnit.Assign(Source: TsmxHKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxStateUnit then
  begin
    CfgID := TsmxStateUnit(Source).CfgID;
    CurrentIntfID := TsmxStateUnit(Source).CurrentIntfID;
    UnitEnable := TsmxStateUnit(Source).UnitEnable;
    UnitVisible := TsmxStateUnit(Source).UnitVisible;
  end;
end;

function TsmxStateUnit.FindByCfgID(ACfgID: Integer; AAmongAll: Boolean = False): TsmxStateUnit;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].CfgID = ACfgID then
      Result := Items[i] else
    if AAmongAll then
      Result := Items[i].FindByCfgID(ACfgID, AAmongAll);
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

procedure TsmxStateUnit.SetItem(Index: Integer; Value: TsmxStateUnit);
begin
  inherited Items[Index] := Value;
end;

function TsmxStateUnit.GetParent: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Parent);
end;

procedure TsmxStateUnit.SetParent(Value: TsmxStateUnit);
begin
  inherited Parent := Value;
end;

procedure TsmxStateUnit.SetCurrentIntfID(Value: Integer);
begin
  if FCurrentIntfID <> Value then
  begin
    FPriorIntfID := FCurrentIntfID;
    FCurrentIntfID := Value;
  end;
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

procedure TsmxStateUnit.SwitchIntfID;
var
  StateUnit: TsmxStateUnit;
  IntfID: Integer;
begin
  if FCurrentIntfID = FPriorIntfID then
    IntfID := HKit.IntfID else
    IntfID := FPriorIntfID;
  StateUnit := Self;
  repeat
    StateUnit.CurrentIntfID := IntfID;
    StateUnit := StateUnit.Parent;
  until not StateUnit.HasParent;
end;

{ TsmxStateUnits }

procedure TsmxStateUnits.Assign(Source: TsmxHKit);
begin
  inherited Assign(Source);
  if Source is TsmxStateUnits then
    IntfID := TsmxStateUnits(Source).IntfID;
end;

function TsmxStateUnits.GetRoot: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Root);
end;

procedure TsmxStateUnits.SetRoot(Value: TsmxStateUnit);
begin
  inherited Root := Value;
end;

{ TsmxCellState }

destructor TsmxCellState.Destroy;
begin
  if Assigned(FStateUnits) then
    FStateUnits.Free;
  inherited Destroy;
end;

procedure TsmxCellState.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxCellState then
  begin
    StateID := TsmxCellState(Source).StateID;
    StateUnits := TsmxCellState(Source).StateUnits;
  end else
    inherited Assign(Source);
end;

function TsmxCellState.GetStateUnits: TsmxStateUnits;
begin
  if not Assigned(FStateUnits) then
    FStateUnits := TsmxStateUnits.Create(TsmxStateUnit);
  Result := FStateUnits;
end;

procedure TsmxCellState.SetStateUnits(Value: TsmxStateUnits);
begin
  StateUnits.Assign(Value);
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

procedure TsmxCellStates.SetItem(Index: Integer; Value: TsmxCellState);
begin
  inherited Items[Index] := Value;
end;

{ TsmxStateCfg }

destructor TsmxStateCfg.Destroy;
begin
  if Assigned(FCellStates) then
    FCellStates.Free;
  inherited Destroy;
end;

procedure TsmxStateCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateCfg then
  begin
    CellStates := TsmxStateCfg(Source).CellStates;
    ID := TsmxStateCfg(Source).ID;
    IntfID := TsmxStateCfg(Source).IntfID;
  end;
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

procedure TsmxStateCfg.SetCellStates(Value: TsmxCellStates);
begin
  CellStates.Assign(Value);
end;

procedure TsmxStateCfg.SetXMLText(const Value: String);
var
  Key: Variant;
  KeySense: TsmxBaseSense;
begin
  if Assigned(FSelectRequest) then
  begin
    if FID = 0 then
    begin
      Key := Variants.VarArrayOf([FCfgID, FIntfID]);
      KeySense := fsForeignKey;
    end else
    begin
      Key := FID;
      KeySense := fsKey;
    end;
    if smxDBFuncs.LocateByKey(FSelectRequest.DataSet, Key, KeySense) then
      if smxDBFuncs.SetCurrentValue(FSelectRequest.DataSet,
          smxFuncs.UnFormatXMLText(Value), FSelectRequest.PerformanceMode) then
        Read;
  end;
end;

procedure TsmxStateCfg.Load;
var
  Key, Value: Variant;
  KeySense: TsmxBaseSense;
begin
  if not Assigned(FSelectRequest)
      or not ((FID <> 0) or ((FCfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'load'], FCfgID);
  if FID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FID;
    KeySense := fsKey;
  end;
  if smxDBFuncs.GetValueByKey(FSelectRequest.DataSet, Key, Value,
      FSelectRequest.PerformanceMode, KeySense) then
  begin
    if smxDBFuncs.LocateByKey(FSelectRequest.DataSet, Key, KeySense) then
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value,
          FSelectRequest.PerformanceMode, fsKey) then
        FID := smxFuncs.GetSingleValue(Value, 0);
  end;
end;

procedure TsmxStateCfg.Read;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxStateUnit; AIntfID: Integer);
  var
    i: Integer;
    StateUnit: TsmxStateUnit;
  begin
    StateUnit := AUnit.FindByCfgID(ANode.Attributes['StateID']);
    if not Assigned(StateUnit) then
      StateUnit := AUnit.Add;
    with StateUnit do
    begin
      CurrentIntfID := AIntfID;
      FCfgID := ANode.Attributes['CfgID'];
      FUnitEnable := ANode.Attributes['Enable'];
      FUnitVisible := ANode.Attributes['Visible'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'Cell' then
        AddUnits(ANode.ChildNodes[i], StateUnit, AIntfID);
  end;

  procedure AddXMLDoc(const AXMLDoc: IXMLDocument; AIntfID: Integer);
  var
    r, n, n2: IXMLNode;
    i, j: Integer;
    State: TsmxCellState;
  begin
    r := AXMLDoc.ChildNodes.FindNode('Root');
    if not Assigned(r) then
      Exit;

    n := r.ChildNodes.FindNode('States');
    if Assigned(n) and (n.ChildNodes.Count > 0) then
    begin
      for i := 0 to n.ChildNodes.Count - 1 do
        if n.ChildNodes[i].NodeName = 'State' then
        begin
          State := CellStates.FindByStateID(n.ChildNodes[i].Attributes['StateID']);
          if not Assigned(State) then
          begin
            State := CellStates.Add;
            State.StateUnits.IntfID := FIntfID;
          end;
          with State do
          begin
            StateID := n.ChildNodes[i].Attributes['StateID'];
            n2 := n.ChildNodes[i].ChildNodes.FindNode('Cells');
            if Assigned(n2) and (n2.ChildNodes.Count > 0) then
              for j := 0 to n2.ChildNodes.Count - 1 do
                if n2.ChildNodes[j].NodeName = 'Cell' then
                  AddUnits(n2.ChildNodes[j], StateUnits.Root, AIntfID);
          end;
        end;
    end;
  end;

var
  Value: Variant;
  IntfID: Integer;
begin
  Clear;
  if Assigned(FSelectRequest) and Assigned(FSelectRequest.DataSet) then
  begin
    FSelectRequest.DataSet.First;
    while not FSelectRequest.DataSet.Eof do
    begin
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value, FSelectRequest.PerformanceMode) then
        FXMLDocIntf.XML.Text := smxFuncs.GetSingleValue(Value, '') else
        FXMLDocIntf.XML.Text := '';
      try
        FXMLDocIntf.Active := True;
      except
        on E: Exception do
          raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
            [ClassName, FCfgID, 'read'], FCfgID, E.Message);
      end;
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value, FSelectRequest.PerformanceMode, fsForeignKey) then
        IntfID := smxFuncs.GetSingleValue(Value, 0, 1) else
        IntfID := 0;
      if IntfID <> 0 then
        AddXMLDoc(FXMLDocIntf, IntfID);
      FSelectRequest.DataSet.Next;
    end;
  end;
end;

procedure TsmxStateCfg.Save;
var
  Request: IsmxDataSet;
  Performance: TsmxPerformanceMode;
  Key, Value: Variant;
  KeySense: TsmxBaseSense;
begin
  if not Assigned(FSelectRequest)
      or ((FID = 0) and ((FCfgID = 0) or (FIntfID = 0))) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);
  if FID = 0 then
  begin
    Request := FSelectRequest.ModifyRequests[mrInsert];
    Performance := FSelectRequest.ModifyPerformances[mrInsert];
  end else
  begin
    Request := FSelectRequest.ModifyRequests[mrUpdate];
    Performance := FSelectRequest.ModifyPerformances[mrUpdate];
  end;
  if not Assigned(Request) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);
  if FID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FID;
    KeySense := fsKey;
  end;
  if smxDBFuncs.SetValueByKey(Request, Key, FXMLDocIntf.XML.Text,
      Performance, KeySense) then
    if smxDBFuncs.GetCurrentValue(Request, Value, Performance, fsKey) then
      FID := smxFuncs.GetSingleValue(Value, 0);
end;

procedure TsmxStateCfg.Write;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var
    n: IXMLNode;
    i: Integer;
  begin
    if AUnit.CurrentIntfID = FIntfID then
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
  end;

var
  r, n, n2, n3: IXMLNode;
  i, j: Integer;
begin
  r := FXMLDocIntf.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear
  else
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
end;

{ TsmxCustomStateForm }

destructor TsmxCustomStateForm.Destroy;
begin
  if Assigned(FStateCfg) then
    FStateCfg.Free;
  inherited Destroy;
end;

function TsmxCustomStateForm.CellParams(const Name: String; var Value: Variant): Boolean;
begin
  if SysUtils.AnsiCompareText(Name, 'StateID') = 0 then
  begin
    Value := FStateID;
    Result := True;
  end else
    Result := inherited CellParams(Name, Value);
end;

procedure TsmxCustomStateForm.ChangeState;
begin
  if Assigned(StateCfg.SelectRequest) and (StateCfg.IntfID <> 0) and (StateCfg.CfgID <> 0) then
  begin
    StateCfg.Receive;
    PutState;
  end;
end;

function TsmxCustomStateForm.GetStateCfg: TsmxStateCfg;
begin
  if not Assigned(FStateCfg) then
    FStateCfg := GetStateCfgClass.Create(Self);
  Result := TsmxStateCfg(FStateCfg);
end;

procedure TsmxCustomStateForm.SetStateCfg(Value: TsmxStateCfg);
begin
  StateCfg.Assign(Value);
end;

function TsmxCustomStateForm.GetStateCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxStateCfg;
end;

procedure TsmxCustomStateForm.PutState;

  procedure PutCell(AUnit: TsmxStateUnit; ACell: TsmxBaseCell);
  var
    i: Integer;
    Cell: TsmxBaseCell;
  begin
    Cell := ACell.FindByCfgID(AUnit.CfgID);
    if Assigned(Cell) then
    begin
      if Cell is TsmxCustomAlgorithm then
        with TsmxCustomAlgorithm(Cell) do
        begin
          CellEnable := AUnit.UnitEnable;
          CellVisible := AUnit.UnitVisible;
        end
      else
      if Cell is TsmxControlCell then
        with TsmxControlCell(Cell) do
        begin
          CellEnable := AUnit.UnitEnable;
          CellVisible := AUnit.UnitVisible;
        end;
      for i := 0 to AUnit.Count - 1 do
        PutCell(AUnit[i], Cell);
    end;
  end;

var
  i: Integer;
  CellState: TsmxCellState;
begin
  CellState := StateCfg.CellStates.FindByStateID(FStateID);
  if Assigned(CellState) then
    for i := 0 to CellState.StateUnits.Root.Count - 1 do
      PutCell(CellState.StateUnits.Root[i], Self);
end;

procedure TsmxCustomStateForm.SetCfgID(Value: Integer);
begin
  if FCfgID <> Value then
  begin
    inherited SetCfgID(Value);
    StateCfg.CfgID := FCfgID;
    ChangeState;
  end;
end;

procedure TsmxCustomStateForm.SetIntfID(Value: Integer);
begin
  if FIntfID <> Value then
  begin
    inherited SetIntfID(Value);
    StateCfg.IntfID := FIntfID;
    ChangeState;
  end;
end;

procedure TsmxCustomStateForm.SetStateID(Value: Integer);
begin
  if FStateID <> Value then
  begin
    FStateID := Value;
    PutState;
  end;
end;

procedure TsmxCustomStateForm.SetStateRequest(Value: TsmxCustomRequest);
begin
  if FStateRequest <> Value then
  begin
    FStateRequest := Value;
    StateCfg.SelectRequest := FStateRequest;
    ChangeState;
  end;
end;

{ TsmxCustomStandardForm }

function TsmxCustomStandardForm.AddSlave: TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited AddSlave);
end;

function TsmxCustomStandardForm.GetSlave(Index: Integer): TsmxCustomPageManager;
begin
  Result := TsmxCustomPageManager(inherited Slaves[Index]);
end;

procedure TsmxCustomStandardForm.SetSlave(Index: Integer; Value: TsmxCustomPageManager);
begin
  inherited Slaves[Index] := Value;
end;

procedure TsmxCustomStandardForm.InternalApply;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    smxClassProcs.AllCells(Self, List, [TsmxCustomFilterDesk]);
    for i := 0 to List.Count - 1 do
      TsmxCustomFilterDesk(List[i]).Apply;
  finally
    List.Free;
  end;
end;

procedure TsmxCustomStandardForm.InternalPrepare;
var
  List: TList;
  i: Integer;
begin
  List := TList.Create;
  try
    smxClassProcs.AllCells(Self, List, [TsmxCustomFilterDesk]);
    for i := 0 to List.Count - 1 do
      TsmxCustomFilterDesk(List[i]).Prepare;
  finally
    List.Free;
  end;
end;

procedure TsmxCustomStandardForm.SetAlgorithmList(Value: TsmxCustomAlgorithmList);
begin
  FAlgorithmList := Value;
end;

procedure TsmxCustomStandardForm.SetControlBoard(Value: TsmxCustomControlBoard);
begin
  FControlBoard := Value;
end;

procedure TsmxCustomStandardForm.SetMainMenu(Value: TsmxCustomMainMenu);
begin
  FMainMenu := Value;
end;

procedure TsmxCustomStandardForm.SetRequestList(Value: TsmxCustomRequestList);
begin
  FRequestList := Value;
end;

procedure TsmxCustomStandardForm.SetStatusBoard(Value: TsmxCustomStatusBoard);
begin
  FStatusBoard := Value;
end;

end.
