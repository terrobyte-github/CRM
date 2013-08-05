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
  Classes, Controls, Graphics, XMLIntf, smxBaseClasses, smxClasses, smxDBIntf,
  smxTypes;

type
  { TsmxParamKitItem }

  TsmxParamKit = class;

  TsmxParamKitItem = class(TsmxSimpleKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
    function GetKit: TsmxParamKit;
    procedure SetKit(Value: TsmxParamKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxParamKit read GetKit write SetKit;
  published
    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxParamKit }

  TsmxParamKit = class(TsmxSimpleKit)
  private
    function GetItem(Index: Integer): TsmxParamKitItem;
    procedure SetItem(Index: Integer; Value: TsmxParamKitItem);
  public
    constructor Create; override;
    function Add: TsmxParamKitItem;
    function FindByName(const ParamName: String): TsmxParamKitItem;

    property Items[Index: Integer]: TsmxParamKitItem read GetItem write SetItem; default;
  end;

  { TsmxAlgorithmParamKitItem }

  TsmxAlgorithmParamKit = class;

  TsmxAlgorithmParamKitItem = class(TsmxParamKitItem)
  private
    FDataType: TsmxDataType;
    FParamLocation: TsmxParamLocation;
    FParamType: TsmxParamType;
    function GetKit: TsmxAlgorithmParamKit;
    procedure SetKit(Value: TsmxAlgorithmParamKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxAlgorithmParamKit read GetKit write SetKit;
  published
    property DataType: TsmxDataType read FDataType write FDataType;
    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamType: TsmxParamType read FParamType write FParamType;
  end;

  { TsmxAlgorithmParamKit }

  TsmxAlgorithmParamKit = class(TsmxParamKit)
  private
    function GetItem(Index: Integer): TsmxAlgorithmParamKitItem;
    procedure SetItem(Index: Integer; Value: TsmxAlgorithmParamKitItem);
  public
    constructor Create; override;
    function Add: TsmxAlgorithmParamKitItem;

    property Items[Index: Integer]: TsmxAlgorithmParamKitItem read GetItem write SetItem; default;
  end;

  { TsmxAlgorithmCfg }

  TsmxAlgorithmCfg = class(TsmxOwnerCellCfg)
  private
    FAlgorithmCaption: String;
    //FAlgorithmCells: TsmxOwnerKit;
    FAlgorithmHint: String;
    FAlgorithmHotKey: Integer;
    FAlgorithmImageIndex: Integer;
    FAlgorithmParams: TsmxAlgorithmParamKit;
    FRefreshParamsCfgID: Integer;
    FAlgorithmEnabled: Boolean;
    FAlgorithmVisible: Boolean;
    //function GetAlgorithmCells: TsmxOwnerKit;
    function GetAlgorithmParams: TsmxAlgorithmParamKit;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetAlgorithmCaption(const Value: String); virtual;
    //procedure SetAlgorithmCells(Value: TsmxOwnerKit); virtual;
    procedure SetAlgorithmEnabled(Value: Boolean); virtual;
    procedure SetAlgorithmHint(const Value: String); virtual;
    procedure SetAlgorithmHotKey(Value: Integer); virtual;
    procedure SetAlgorithmImageIndex(Value: Integer); virtual;
    procedure SetAlgorithmParams(Value: TsmxAlgorithmParamKit); virtual;
    procedure SetAlgorithmVisible(Value: Boolean); virtual;
    procedure SetRefreshParamsCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

  published
    property AlgorithmCaption: String read FAlgorithmCaption write SetAlgorithmCaption;
    //property AlgortihmCells: TsmxOwnerKit read GetAlgorithmCells write SetAlgorithmCells;
    property AlgorithmEnabled: Boolean read FAlgorithmEnabled write SetAlgorithmEnabled;
    property AlgorithmHint: String read FAlgorithmHint write SetAlgorithmHint;
    property AlgorithmHotKey: Integer read FAlgorithmHotKey write SetAlgorithmHotKey;
    property AlgorithmImageIndex: Integer read FAlgorithmImageIndex write SetAlgorithmImageIndex;
    property AlgorithmParams: TsmxAlgorithmParamKit read GetAlgorithmParams write SetAlgorithmParams;
    property AlgorithmVisible: Boolean read FAlgorithmVisible write SetAlgorithmVisible;
    property RefreshParamsCfgID: Integer read FRefreshParamsCfgID write SetRefreshParamsCfgID;
  end;

  { TsmxLibAlgorithmCfg }

  {TsmxLibAlgorithmCfg = class(TsmxAlgorithmCfg)
  private
    FAlgorithmLibrary: String;
    FAlgorithmProcedure: String;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetAlgorithmLibrary(const Value: String); virtual;
    procedure SetAlgorithmProcedure(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property AlgorithmLibrary: String read FAlgorithmLibrary write SetAlgorithmLibrary;
    property AlgorithmProcedure: String read FAlgorithmProcedure write SetAlgorithmProcedure;
  end;}

  { TsmxAlgorithmKitItem }

  TsmxAlgorithmKit = class;

  TsmxAlgorithmKitItem = class(TsmxOwnerKitItem)
  private
    FItemEnabled: Boolean;
    FItemParams: TsmxParamKit;
    FItemVisible: Boolean;
    function GetItemParams: TsmxParamKit;
    function GetKit: TsmxAlgorithmKit;
    procedure SetItemParams(Value: TsmxParamKit);
    procedure SetKit(Value: TsmxAlgorithmKit);
  public
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxAlgorithmKit read GetKit write SetKit;
  published
    property ItemEnabled: Boolean read FItemEnabled write FItemEnabled;
    property ItemParams: TsmxParamKit read GetItemParams write SetItemParams;
    property ItemVisible: Boolean read FItemVisible write FItemVisible;
  end;

  { TsmxAlgorithmKit }

  TsmxAlgorithmKit = class(TsmxOwnerKit)
  private
    function GetItem(Index: Integer): TsmxAlgorithmKitItem;
    procedure SetItem(Index: Integer; Value: TsmxAlgorithmKitItem);
  public
    constructor Create; override;
    function Add: TsmxAlgorithmKitItem;

    property Items[Index: Integer]: TsmxAlgorithmKitItem read GetItem write SetItem; default;
  end;

  { TsmxAlgorithmListCfg }

  TsmxAlgorithmListCfg = class(TsmxOwnerCellCfg)
  private
    function GetSlaveCells: TsmxAlgorithmKit;
    procedure SetSlaveCells(Value: TsmxAlgorithmKit);
  protected
    function GetSlaveCellsClass: TsmxSimpleKitClass; override;
    //procedure ReadSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode); override;
    //procedure WriteSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ImageListName;
    property SlaveCells: TsmxAlgorithmKit read GetSlaveCells write SetSlaveCells;
  end;

  { TsmxRequestFieldKitItem }

  TsmxRequestFieldKit = class;

  TsmxRequestFieldKitItem = class(TsmxSimpleKitItem)
  private
    FDataType: TsmxDataType;
    FDisplayFormat: String;
    FFieldName: String;
    FFieldSense: TsmxFieldSense;
    FPrecision: Integer;
    FSize: Integer;
    function GetKit: TsmxRequestFieldKit;
    procedure SetKit(Value: TsmxRequestFieldKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxRequestFieldKit read GetKit write SetKit;
  published
    property DataType: TsmxDataType read FDataType write FDataType;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    property FieldName: String read FFieldName write FFieldName;
    property FieldSense: TsmxFieldSense read FFieldSense write FFieldSense;
    property Precision: Integer read FPrecision write FPrecision;
    property Size: Integer read FSize write FSize;
  end;

  { TsmxRequestFieldKit }

  TsmxRequestFieldKit = class(TsmxSimpleKit)
  private
    function GetItem(Index: Integer): TsmxRequestFieldKitItem;
    procedure SetItem(Index: Integer; Value: TsmxRequestFieldKitItem);
  public
    constructor Create; override;
    function Add: TsmxRequestFieldKitItem;
    //function FindByName(const AFieldName: String): TsmxRequestFieldKitItem;

    property Items[Index: Integer]: TsmxRequestFieldKitItem read GetItem write SetItem; default;
  end;

  { TsmxRequestParamKitItem }

  TsmxRequestParamKit = class;

  TsmxRequestParamKitItem = class(TsmxAlgorithmParamKitItem)
  private
    FNumericScale: Integer;
    FPrecision: Integer;
    FSize: Integer;
    function GetKit: TsmxRequestParamKit;
    procedure SetKit(Value: TsmxRequestParamKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxRequestParamKit read GetKit write SetKit;
  published
    property NumericScale: Integer read FNumericScale write FNumericScale;
    property Precision: Integer read FPrecision write FPrecision;
    property Size: Integer read FSize write FSize;
  end;

  { TsmxRequestParamKit }

  TsmxRequestParamKit = class(TsmxAlgorithmParamKit)
  private
    function GetItem(Index: Integer): TsmxRequestParamKitItem;
    procedure SetItem(Index: Integer; Value: TsmxRequestParamKitItem);
  public
    constructor Create; override;
    function Add: TsmxRequestParamKitItem;
    //function FindByName(const Name: String): TsmxRequestParamKitItem;

    property Items[Index: Integer]: TsmxRequestParamKitItem read GetItem write SetItem; default;
  end;

  { TsmxRequestCfg }

  TsmxRequestCfg = class(TsmxOwnerCellCfg)
  private
    FDatabaseName: String;
    FDeleteAlgCfgID: Integer;
    FDeletePerformance: TsmxPerformanceMode;
    FDeleteReqCfgID: Integer;
    FExecureAlgCfgID: Integer;
    FFields: TsmxRequestFieldKit;
    FInsertAlgCfgID: Integer;
    FInsertPerformance: TsmxPerformanceMode;
    FInsertReqCfgID: Integer;
    FOperationMode: TsmxOperationMode;
    FParams: TsmxRequestParamKit;
    FPerformanceMode: TsmxPerformanceMode;
    FPrepareAlgCfgID: Integer;
    FRefreshParamsAlgCfgID: Integer;
    FSQLText: String;
    FUpdateAlgCfgID: Integer;
    FUpdatePerformance: TsmxPerformanceMode;
    FUpdateReqCfgID: Integer;
    function GetFields: TsmxRequestFieldKit;
    function GetParams: TsmxRequestParamKit;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetDatabaseName(const Value: String); virtual;
    procedure SetDeleteAlgCfgID(Value: Integer); virtual;
    procedure SetDeleteReqCfgID(Value: Integer); virtual;
    procedure SetExecuteAlgCfgID(Value: Integer); virtual;
    procedure SetFields(Value: TsmxRequestFieldKit); virtual;
    procedure SetInsertAlgCfgID(Value: Integer); virtual;
    procedure SetInsertReqCfgID(Value: Integer); virtual;
    procedure SetModifyPerformance(Index: Integer; Value: TsmxPerformanceMode); virtual;
    procedure SetOperationMode(Value: TsmxOperationMode); virtual;
    procedure SetParams(Value: TsmxRequestParamKit); virtual;
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;
    procedure SetPrepareAlgCfgID(Value: Integer); virtual;
    procedure SetRefreshParamsAlgCfgID(Value: Integer); virtual;
    procedure SetSQLText(const Value: String); virtual;
    procedure SetUpdateAlgCfgID(Value: Integer); virtual;
    procedure SetUpdateReqCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property DeleteAlgCfgID: Integer read FDeleteAlgCfgID write SetDeleteAlgCfgID;
    property DeletePerformance: TsmxPerformanceMode index 1 read FDeletePerformance write SetModifyPerformance;
    property DeleteReqCfgID: Integer read FDeleteReqCfgID write SetDeleteReqCfgID;
    property ExecuteAlgCfgID: Integer read FExecureAlgCfgID write SetExecuteAlgCfgID;
    property Fields: TsmxRequestFieldKit read GetFields write SetFields;
    property InsertAlgCfgID: Integer read FInsertAlgCfgID write SetInsertAlgCfgID;
    property InsertPerformance: TsmxPerformanceMode index 2 read FInsertPerformance write SetModifyPerformance;
    property InsertReqCfgID: Integer read FInsertReqCfgID write SetInsertReqCfgID;
    property OperationMode: TsmxOperationMode read FOperationMode write SetOperationMode;
    property Params: TsmxRequestParamKit read GetParams write SetParams;
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write SetPerformanceMode;
    property PrepareAlgCfgID: Integer read FPrepareAlgCfgID write SetPrepareAlgCfgID;
    property RefreshParamsAlgCfgID: Integer read FRefreshParamsAlgCfgID write SetRefreshParamsAlgCfgID;
    property SQLText: String read FSQLText write SetSQLText;
    property UpdateAlgCfgID: Integer read FUpdateAlgCfgID write SetUpdateAlgCfgID;
    property UpdatePerformance: TsmxPerformanceMode index 3 read FUpdatePerformance write SetModifyPerformance;
    property UpdateReqCfgID: Integer read FUpdateReqCfgID write SetUpdateReqCfgID;
  end;

  { TsmxRequestKitItem }

  TsmxRequestKit = class;

  TsmxRequestKitItem = class(TsmxOwnerKitItem)
  private
    FDatabaseName: String;
    FItemParams: TsmxParamKit;
    FOperationMode: TsmxOperationMode;
    function GetItemParams: TsmxParamKit;
    function GetKit: TsmxRequestKit;
    procedure SetItemParams(Value: TsmxParamKit);
    procedure SetKit(Value: TsmxRequestKit);
  public
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxRequestKit read GetKit write SetKit;
  published
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property ItemParams: TsmxParamKit read GetItemParams write SetItemParams;
    property OperationMode: TsmxOperationMode read FOperationMode write FOperationMode;
  end;

  { TsmxRequestKit }

  TsmxRequestKit = class(TsmxOwnerKit)
  private
    function GetItem(Index: Integer): TsmxRequestKitItem;
    procedure SetItem(Index: Integer; Value: TsmxRequestKitItem);
  public
    constructor Create; override;
    function Add: TsmxRequestKitItem;

    property Items[Index: Integer]: TsmxRequestKitItem read GetItem write SetItem; default;
  end;

  { TsmxRequestListCfg }

  TsmxRequestListCfg = class(TsmxOwnerCellCfg)
  private
    function GetSlaveCells: TsmxRequestKit;
    procedure SetSlaveCells(Value: TsmxRequestKit);
  protected
    function GetSlaveCellsClass: TsmxSimpleKitClass; override;
    //procedure ReadSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode); override;
    //procedure WriteSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SlaveCells: TsmxRequestKit read GetSlaveCells write SetSlaveCells;
  end;

  { TsmxColumnCfg }

  TsmxColumnCfg = class(TsmxControlCellCfg)
  private
    FColumnFieldName: String;
    FColumnHeader: TsmxText;
    FColumnText: TsmxText;
    FIsEditing: Boolean;
    FSnapHeaderAlgCfgID: Integer;
    function GetColumnHeader: TsmxText;
    function GetColumnText: TsmxText;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetColumnFieldName(const Value: String); virtual;
    procedure SetColumnHeader(Value: TsmxText); virtual;
    procedure SetColumnText(Value: TsmxText); virtual;
    procedure SetIsEditing(Value: Boolean); virtual;
    procedure SetSnapHeaderAlgCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgVisible;
    property CfgWidth;
    //property ColumnFieldName: String read FColumnFieldName write SetColumnFieldName;
    property ColumnHeader: TsmxText read GetColumnHeader write SetColumnHeader;
    //property ColumnName: String read FColumnName write SetColumnName;
    property ColumnText: TsmxText read GetColumnText write SetColumnText;
    property IsEditing: Boolean read FIsEditing write SetIsEditing;
    property PopupMenuCfgID;
    property SnapAlgCfgID;
    property SnapHeaderAlgCfgID: Integer read FSnapHeaderAlgCfgID write SetSnapHeaderAlgCfgID;
  end;

  { TsmxGridCfg }

  TsmxGridCfg = class(TsmxActionCellCfg)
  private
    FApplyAlgCfgID: Integer;
    FChangeRowAlgCfgID: Integer;
    FGridOptions: TsmxGridOptions;
    //FPressDoubleAlgCfgID: Integer;
    //FPressHeaderAlgCfgID: Integer;
    FPrepareAlgCfgID: Integer;
    FRefreshAlgCfgID: Integer;
    FRequestCfgID: Integer;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetApplyAlgCfgID(Value: Integer); virtual;
    procedure SetChangeRowAlgCfgID(Value: Integer); virtual;
    procedure SetGridOptions(Value: TsmxGridOptions); virtual;
    //procedure SetPressDoubleAlgCfgID(Value: Integer); virtual;
    //procedure SetPressHeaderAlgCfgID(Value: Integer); virtual;
    procedure SetPrepareAlgCfgID(Value: Integer); virtual;
    procedure SetRefreshAlgCfgID(Value: Integer); virtual;
    procedure SetRequestCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property ApplyAlgCfgID: Integer read FApplyAlgCfgID write SetApplyAlgCfgID;
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgHint;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property ChangeRowAlgCfgID: Integer read FChangeRowAlgCfgID write SetChangeRowAlgCfgID;
    property DoubleSnapAlgCfgID;
    property GridOptions: TsmxGridOptions read FGridOptions write SetGridOptions;
    property IsSetAlgorithmEvents;
    property PopupMenuCfgID;
    property PrepareAlgCfgID: Integer read FPrepareAlgCfgID write SetPrepareAlgCfgID;
    property RefreshAlgCfgID: Integer read FRefreshAlgCfgID write SetRefreshAlgCfgID;
    property RequestCfgID: Integer read FRequestCfgID write SetRequestCfgID;
    property SlaveCells;
    property SnapAlgCfgID;
    //property PressDoubleAlgCfgID: Integer read FPressDoubleAlgCfgID write SetPressDoubleAlgCfgID;
    //property PressHeaderAlgCfgID: Integer read FPressHeaderAlgCfgID write SetPressHeaderAlgCfgID;
  end;

  { TsmxFilterCfg }

  TsmxFilterCfg = class(TsmxActionCellCfg)
  private
    FChangeFilterAlgCfgID: Integer;
    FDisplayFormat: String;
    FFilterHeader: TsmxText;
    //FFilterName: String;
    FFilterText: TsmxText;
    FFilterOptions: TsmxFilterOptions;
    FFilterValue: Variant;
    //FRequestCfgID: Integer;
    FValueFormat: String;
    function GetFilterHeader: TsmxText;
    function GetFilterText: TsmxText;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetChangeFilterAlgCfgID(Value: Integer); virtual;
    procedure SetDisplayFormat(const Value: String); virtual;
    procedure SetFilterHeader(Value: TsmxText); virtual;
    //procedure SetFilterName(const Value: String); virtual;
    procedure SetFilterOptions(Value: TsmxFilterOptions); virtual;
    procedure SetFilterText(Value: TsmxText); virtual;
    procedure SetFilterValue(const Value: Variant); virtual;
    //procedure SetRequestCfgID(Value: Integer); virtual;
    procedure SetValueFormat(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCaption;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgHint;
    property CfgImageIndex;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property ChangeFilterAlgCfgID: Integer read FChangeFilterAlgCfgID write SetChangeFilterAlgCfgID;
    property DisplayFormat: String read FDisplayFormat write SetDisplayFormat;
    property DoubleSnapAlgCfgID;
    property FilterHeader: TsmxText read GetFilterHeader write SetFilterHeader;
    //property FilterName: String read FFilterName write SetFilterName;
    property FilterOptions: TsmxFilterOptions read FFilterOptions write SetFilterOptions;
    property FilterText: TsmxText read GetFilterText write SetFilterText;
    property FilterValue: Variant read FFilterValue write SetFilterValue;
    property ImageListName;
    property PopupMenuCfgID;
    //property RequestCfgID: Integer read FRequestCfgID write SetRequestCfgID;
    property SnapAlgCfgID;
    property ValueFormat: String read FValueFormat write SetValueFormat;
  end;

  { TsmxFilterKitItem }

  TsmxFilterKit = class;

  TsmxFilterKitItem = class(TsmxControlKitItem)
  private
    FDisplayFormat: String;
    FFilterOptions: TsmxFilterOptions;
    FValueFormat: String;
    function GetKit: TsmxFilterKit;
    procedure SetKit(Value: TsmxFilterKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property Kit: TsmxFilterKit read GetKit write SetKit;
  published
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    property FilterOptions: TsmxFilterOptions read FFilterOptions write FFilterOptions;
    property ValueFormat: String read FValueFormat write FValueFormat;
  end;

  { TsmxFilterKit }

  TsmxFilterKit = class(TsmxControlKit)
  private
    function GetItem(Index: Integer): TsmxFilterKitItem;
    procedure SetItem(Index: Integer; Value: TsmxFilterKitItem);
  public
    constructor Create; override;
    function Add: TsmxFilterKitItem;

    property Items[Index: Integer]: TsmxFilterKitItem read GetItem write SetItem; default;
  end;

  { TsmxFilterDeskCfg }

  TsmxFilterDeskCfg = class(TsmxActionCellCfg)
  private
    FApplyAlgCfgID: Integer;
    FPrepareAlgCfgID: Integer;
    FRefreshAlgCfgID: Integer;
    FRequestCfgID: Integer;
    function GetSlaveCells: TsmxFilterKit;
    procedure SetSlaveCells(Value: TsmxFilterKit);
  protected
    function GetSlaveCellsClass: TsmxSimpleKitClass; override;
    procedure ReadCell(const Node: IXMLNode); override;
    //procedure ReadSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode); override;
    procedure SetApplyAlgCfgID(Value: Integer); virtual;
    procedure SetPrepareAlgCfgID(Value: Integer); virtual;
    procedure SetRefreshAlgCfgID(Value: Integer); virtual;
    procedure SetRequestCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
    //procedure WriteSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property ApplyAlgCfgID: Integer read FApplyAlgCfgID write SetApplyAlgCfgID;
    property CfgAlign;
    property CfgAnchors;
    property CfgCaption;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgHint;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property DoubleSnapAlgCfgID;
    property PopupMenuCfgID;
    property PrepareAlgCfgID: Integer read FPrepareAlgCfgID write SetPrepareAlgCfgID;
    property RefreshAlgCfgID: Integer read FRefreshAlgCfgID write SetRefreshAlgCfgID;
    property RequestCfgID: Integer read FRequestCfgID write SetRequestCfgID;
    property SlaveCells: TsmxFilterKit read GetSlaveCells write SetSlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxRequestSectionCfg }

  {TsmxRequestSectionCfg = class(TsmxControlCellCfg)
  private
    FFilterDesk: TsmxControlKitItem;
    FGrid: TsmxControlKitItem;
    function GetFilterDesk: TsmxControlKitItem;
    function GetGrid: TsmxControlKitItem;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetFilterDesk(Value: TsmxControlKitItem); virtual;
    procedure SetGrid(Value: TsmxControlKitItem); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property DoubleSnapAlgCfgID;
    property FilterDesk: TsmxControlKitItem read GetFilterDesk write SetFilterDesk;
    property Grid: TsmxControlKitItem read GetGrid write SetGrid;
    property PopupMenuCfgID;
    property SnapAlgCfgID;
  end;}

  { TsmxSectionCfg }

  TsmxSectionCfg = class(TsmxControlCellCfg)
  {private
    FControl: TsmxControlKitItem;
    function GetControl: TsmxControlKitItem;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetControl(Value: TsmxControlKitItem); virtual;
    procedure WriteCell(const Node: IXMLNode); override;}
  public
    constructor Create(AOwner: TComponent); override;
    {destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;}
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    //property Control: TsmxControlKitItem read GetControl write SetControl;
    property DoubleSnapAlgCfgID;
    property PopupMenuCfgID;
    property SlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxPageCfg }

  TsmxPageCfg = class(TsmxActionCellCfg)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CfgActive;
    property CfgCaption;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgHint;
    property CfgImageIndex;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property PopupMenuCfgID;
    property SlaveCells;
  end;

  { TsmxPageManagerCfg }

  TsmxPageManagerCfg = class(TsmxControlCellCfg)
  private
    FActivePageIndex: Integer;
    FChangePageAlgCfgID: Integer;
    FIsMultiLine: Boolean;
    FPageManagerStyle: TsmxPageManagerStyle;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetActivePageIndex(Value: Integer); virtual;
    procedure SetChangePageAlgCfgID(Value: Integer); virtual;
    procedure SetIsMultiLine(Value: Boolean); virtual;
    procedure SetPageManagerStyle(Value: TsmxPageManagerStyle); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex;
    property ChangePageAlgCfgID: Integer read FChangePageAlgCfgID write SetChangePageAlgCfgID;
    property ImageListName;
    property IsMultiLine: Boolean read FIsMultiLine write SetIsMultiLine;
    property PageManagerStyle: TsmxPageManagerStyle read FPageManagerStyle write SetPageManagerStyle;
    property PopupMenuCfgID;
    property SlaveCells;
  end;

  { TsmxMenuItemCfg }

  TsmxMenuItemCfg = class(TsmxActionCellCfg)
  private
    FIsChecked: Boolean;
    FMenuItemHotKey: Integer;
    FMenuItemStyle: TsmxMenuItemStyle;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetIsChecked(Value: Boolean); virtual;
    procedure SetMenuItemHotKey(Value: Integer); virtual;
    procedure SetMenuItemStyle(Value: TsmxMenuItemStyle); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgCaption;
    property CfgEnabled;
    property CfgHint;
    property CfgImageIndex;
    property CfgVisible;
    property IsChecked: Boolean read FIsChecked write SetIsChecked;
    property IsSetAlgorithmEvents;
    property MenuItemHotKey: Integer read FMenuItemHotKey write SetMenuItemHotKey;
    property MenuItemStyle: TsmxMenuItemStyle read FMenuItemStyle write SetMenuItemStyle;
    property SlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxHVisibleUnit }

  {TsmxHVisibleUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FItemAlign: TAlign;
    FItemEnabled: Boolean;
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
    property ItemEnabled: Boolean read FItemEnabled write FItemEnabled;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ItemLeft: Integer read FItemLeft write FItemLeft;
    property ItemTop: Integer read FItemTop write FItemTop;
    property ItemVisible: Boolean read FItemVisible write FItemVisible;
    property ItemWidth: Integer read FItemWidth write FItemWidth;
  end;}

  { TsmxHVisibleUnits }

  {TsmxHVisibleUnits = class(TsmxHKit)
  private
    function GetRoot: TsmxHVisibleUnit;
  public
    property Root: TsmxHVisibleUnit read GetRoot;
  end;}

  { TsmxMainMenuCfg }

  TsmxMainMenuCfg = class(TsmxControlCellCfg)
  //private
    //FMenuItemCfgID: Integer;
  //protected
    //procedure ReadCell(const Node: IXMLNode); override;
    //procedure SetMenuItemCfgID(Value: Integer); virtual;
    //procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    //procedure Assign(Source: TPersistent); override;
    //procedure Clear; override;

    //property MenuItemCfgID: Integer read FMenuItemCfgID write SetMenuItemCfgID;
  published
    property CfgEnabled;
    property CfgVisible;
    property ImageListName;
    property SlaveCells;
  end;

  { TsmxPopupMenuCfg }

  TsmxPopupMenuCfg = class(TsmxControlCellCfg)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ImageListName;
    property SlaveCells;
  end;

  { TsmxPopupListCfg }

  TsmxPopupListCfg = class(TsmxOwnerCellCfg)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SlaveCells;
  end;

  { TsmxBoardItemCfg }

  TsmxToolItemCfg = class(TsmxActionCellCfg)
  private
    FIsChecked: Boolean;
    FToolItemStyle: TsmxToolItemStyle;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetIsChecked(Value: Boolean); virtual;
    procedure SetToolItemStyle(Value: TsmxToolItemStyle); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgCaption;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgHint;
    property CfgImageIndex;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property IsChecked: Boolean read FIsChecked write SetIsChecked;
    property IsSetAlgorithmEvents;
    property PopupMenuCfgID;
    property SnapAlgCfgID;
    property ToolItemStyle: TsmxToolItemStyle read FToolItemStyle write SetToolItemStyle;
  end;

  { TsmxToolBoardCfg }

  TsmxToolBoardCfg = class(TsmxControlCellCfg)
  private
    FIsFlat: Boolean;
    FIsShowCaptions: Boolean;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetIsFlat(Value: Boolean); virtual;
    procedure SetIsShowCaptions(Value: Boolean); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property DoubleSnapAlgCfgID;
    property ImageListName;
    property IsFlat: Boolean read FIsFlat write SetIsFlat;
    property IsShowCaptions: Boolean read FIsShowCaptions write SetIsShowCaptions;
    property PopupMenuCfgID;
    property SlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxControlBoardCfg }

  TsmxControlBoardCfg = class(TsmxControlCellCfg)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property DoubleSnapAlgCfgID;
    property PopupMenuCfgID;
    property SlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxStatusItemCfg }

  TsmxStatusItemCfg = class(TsmxActionCellCfg)
  private
    FDrawPanelAlgCfg: Integer;
    FStatusItemAlignment: TAlignment;
    FStatusItemStyle: TsmxStatusItemStyle;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetDrawPanelAlgCfg(Value: Integer); virtual;
    procedure SetStatusItemAlignment(Value: TAlignment); virtual;
    procedure SetStatusItemStyle(Value: TsmxStatusItemStyle); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property CfgCaption;
    property CfgImageIndex;
    property CfgWidth;
    property DrawPanelAlgCfg: Integer read FDrawPanelAlgCfg write SetDrawPanelAlgCfg;
    property ImageListName;
    property StatusItemAlignment: TAlignment read FStatusItemAlignment write SetStatusItemAlignment;
    property StatusItemStyle: TsmxStatusItemStyle read FStatusItemStyle write SetStatusItemStyle;
  end;

  { TsmxStatusBoardCfg }

  TsmxStatusBoardCfg = class(TsmxControlCellCfg)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CfgAlign;
    property CfgAnchors;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property DoubleSnapAlgCfgID;
    property PopupMenuCfgID;
    property SlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxFormCfg }

  TsmxFormCfg = class(TsmxActionCellCfg)
  private
    FAlgorithmListCfgID: Integer;
    FCloseAlgCfgID: Integer;
    FFormBorder: TsmxFormBorder;
    FFormPosition: TsmxFormPosition;
    //FIsMainForm: Boolean;
    FIsMaximize: Boolean;
    FPopupListCfgID: Integer;
    FRequestListCfgID: Integer;
    FShowAlgCfgID: Integer;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetAlgorithmListCfgID(Value: Integer); virtual;
    procedure SetCloseAlgCfgID(Value: Integer); virtual;
    procedure SetFormBorder(Value: TsmxFormBorder); virtual;
    procedure SetFormPosition(Value: TsmxFormPosition); virtual;
    //procedure SetIsMainForm(Value: Boolean); virtual;
    procedure SetIsMaximize(Value: Boolean); virtual;
    procedure SetPopupListCfgID(Value: Integer); virtual;
    procedure SetRequestListCfgID(Value: Integer); virtual;
    procedure SetShowAlgCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  published
    property AlgorithmListCfgID: Integer read FAlgorithmListCfgID write SetAlgorithmListCfgID;
    property CfgActive;
    property CfgAlign;
    property CfgAnchors;
    property CfgCaption;
    property CfgCursor;
    property CfgEnabled;
    property CfgHeight;
    property CfgHint;
    property CfgImageIndex;
    property CfgLeft;
    property CfgTop;
    property CfgVisible;
    property CfgWidth;
    property CloseAlgCfgID: Integer read FCloseAlgCfgID write SetCloseAlgCfgID;
    property DoubleSnapAlgCfgID;
    property FormBorder: TsmxFormBorder read FFormBorder write SetFormBorder;
    property FormPosition: TsmxFormPosition read FFormPosition write SetFormPosition;
    property ImageListName;
    //property IsMainForm: Boolean read FIsMainForm write SetIsMainForm;
    property IsMaximize: Boolean read FIsMaximize write SetIsMaximize;
    property IsSetAlgorithmEvents;
    property PopupListCfgID: Integer read FPopupListCfgID write SetPopupListCfgID;
    property PopupMenuCfgID;
    property RequestListCfgID: Integer read FRequestListCfgID write SetRequestListCfgID;
    property ShowAlgCfgID: Integer read FShowAlgCfgID write SetShowAlgCfgID;
    property SlaveCells;
    property SnapAlgCfgID;
  end;

  { TsmxStateKitItem }

  {TsmxStateKit = class;

  TsmxStateKitItem = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FCurrentIntfID: Integer;
    FItemEnabled: Boolean;
    FItemVisible: Boolean;
    FPriorIntfID: Integer;
    function GetHKit: TsmxStateKit;
    function GetItem(Index: Integer): TsmxStateKitItem;
    function GetParent: TsmxStateKitItem;
    procedure SetCurrentIntfID(Value: Integer);
    procedure SetItem(Index: Integer; Value: TsmxStateKitItem);
    procedure SetParent(Value: TsmxStateKitItem);
    procedure SwitchIntfID;
  protected
    procedure SetItemEnabled(Value: Boolean); virtual;
    procedure SetItemVisible(Value: Boolean); virtual;

    property PriorIntfID: Integer read FPriorIntfID write FPriorIntfID;
  public
    function Add: TsmxStateKitItem;
    procedure Assign(Source: TsmxHKitItem); override;
    function FindByCfgID(CfgID: Integer; AmongAll: Boolean = False): TsmxStateKitItem;

    property CfgID: Integer read FCfgID write FCfgID;
    property CurrentIntfID: Integer read FCurrentIntfID write SetCurrentIntfID;
    property HKit: TsmxStateKit read GetHKit;
    property Items[Index: Integer]: TsmxStateKitItem read GetItem write SetItem; default;
    property ItemEnabled: Boolean read FItemEnabled write SetItemEnabled;
    property ItemVisible: Boolean read FItemVisible write SetItemVisible;
    property Parent: TsmxStateKitItem read GetParent write SetParent;
  end;}

  { TsmxStateKit }

  {TsmxStateKit = class(TsmxHKit)
  private
    FIntfID: Integer;
    function GetRoot: TsmxStateKitItem;
    procedure SetRoot(Value: TsmxStateKitItem);
  public
    procedure Assign(Source: TsmxHKit); override;

    property IntfID: Integer read FIntfID write FIntfID;
    property Root: TsmxStateKitItem read GetRoot write SetRoot;
  end;}

  { TsmxCellState }

  {TsmxCellState = class(TsmxKitItem)
  private
    FStateID: Integer;
    FStateKit: TsmxStateKit;
    function GetStateKit: TsmxStateKit;
    procedure SetStateKit(Value: TsmxStateKit);
  public
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;

    property StateID: Integer read FStateID write FStateID;
    property StateKit: TsmxStateKit read GetStateKit write SetStateKit;
  end;}

  { TsmxCellStates }

  {TsmxCellStates = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxCellState;
    procedure SetItem(Index: Integer; Value: TsmxCellState);
  public
    function Add: TsmxCellState;
    function FindByStateID(StateID: Integer): TsmxCellState;

    property Items[Index: Integer]: TsmxCellState read GetItem write SetItem; default;
  end;}

  { TsmxSimpleStateCfg }

  {TsmxSimpleStateCfg = class(TsmxStateCfg)
  private
    FCellStates: TsmxCellStates;
    function GetCellStates: TsmxCellStates;
    procedure SetCellStates(Value: TsmxCellStates);
  protected
    procedure ReadIntf(const Node: IXMLNode; ID: Integer); override;
    procedure WriteIntf(const Node: IXMLNode; ID: Integer); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property CellStates: TsmxCellStates read GetCellStates write SetCellStates;
  end;}

  { TsmxStateFormCfg }

  {TsmxStateFormCfg = class(TsmxFormCfg)
  private
    FStateID: Integer;
    FStateReqCfgID: Integer;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetStateID(Value: Integer); virtual;
    procedure SetStateReqCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property StateID: Integer read FStateID write SetStateID;
    property StateReqCfgID: Integer read FStateReqCfgID write SetStateReqCfgID;
  end;}

  { TsmxStandardFormCfg }

  {TsmxStandardFormCfg = class(TsmxStateFormCfg)
  private
    FControlBoard: TsmxControlKitItem;
    FMainMenu: TsmxControlKitItem;
    FStatusBoard: TsmxControlKitItem;
    function GetControlBoard: TsmxControlKitItem;
    function GetMainMenu: TsmxControlKitItem;
    function GetStatusBoard: TsmxControlKitItem;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetControlBoard(Value: TsmxControlKitItem); virtual;
    procedure SetMainMenu(Value: TsmxControlKitItem); virtual;
    procedure SetStatusBoard(Value: TsmxControlKitItem); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property ControlBoard: TsmxControlKitItem read GetControlBoard write SetControlBoard;
    property MainMenu: TsmxControlKitItem read GetMainMenu write SetMainMenu;
    property StatusBoard: TsmxControlKitItem read GetStatusBoard write SetStatusBoard;
  end;}

implementation

uses
  DB, SysUtils, Variants, TypInfo, smxFuncs, smxProcs, smxConsts;

{ TsmxParamKitItem }

procedure TsmxParamKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxParamKitItem then
  begin
    ParamName := TsmxParamKitItem(Source).ParamName;
    ParamValue := TsmxParamKitItem(Source).ParamValue;
  end;
end;

procedure TsmxParamKitItem.Clear;
begin
  inherited Clear;
  ParamName := '';
  ParamValue := Variants.Null;
end;

function TsmxParamKitItem.GetKit: TsmxParamKit;
begin
  Result := TsmxParamKit(inherited Kit);
end;

procedure TsmxParamKitItem.SetKit(Value: TsmxParamKit);
begin
  inherited Kit := Value;
end;

procedure TsmxParamKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  ParamName := Node.Attributes['ParamName'];
  ParamValue := smxFuncs.StrToVar(Node.Attributes['ParamValue']);
end;

procedure TsmxParamKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['ParamName'] := ParamName;
  Node.Attributes['ParamValue'] := Variants.VarToStr(ParamValue);
end;

{ TsmxParamKit }

constructor TsmxParamKit.Create;
begin
  Create(TsmxParamKitItem);
end;

function TsmxParamKit.Add: TsmxParamKitItem;
begin
  Result := TsmxParamKitItem(inherited Add);
end;

function TsmxParamKit.FindByName(const ParamName: String): TsmxParamKitItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, ParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxParamKit.GetItem(Index: Integer): TsmxParamKitItem;
begin
  Result := TsmxParamKitItem(inherited Items[Index]);
end;

procedure TsmxParamKit.SetItem(Index: Integer; Value: TsmxParamKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxAlgorithmParamKitItem }

procedure TsmxAlgorithmParamKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmParamKitItem then
  begin
    DataType := TsmxAlgorithmParamKitItem(Source).DataType;
    ParamLocation := TsmxAlgorithmParamKitItem(Source).ParamLocation;
    ParamType := TsmxAlgorithmParamKitItem(Source).ParamType;
  end;
end;

procedure TsmxAlgorithmParamKitItem.Clear;
begin
  inherited Clear;
  DataType := ftUnknown;
  ParamLocation := plConst;
  ParamType := ptUnknown;
end;

function TsmxAlgorithmParamKitItem.GetKit: TsmxAlgorithmParamKit;
begin
  Result := TsmxAlgorithmParamKit(inherited Kit);
end;

procedure TsmxAlgorithmParamKitItem.SetKit(Value: TsmxAlgorithmParamKit);
begin
  inherited Kit := Value;
end;

procedure TsmxAlgorithmParamKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  DataType := TsmxDataType(TypInfo.GetEnumValue(TypeInfo(TsmxDataType), Node.Attributes['DataType']));
  ParamLocation := TsmxParamLocation(TypInfo.GetEnumValue(TypeInfo(TsmxParamLocation), Node.Attributes['ParamLocation']));
  ParamType := TsmxParamType(TypInfo.GetEnumValue(TypeInfo(TsmxParamType), Node.Attributes['ParamType']));
end;

procedure TsmxAlgorithmParamKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['DataType'] := TypInfo.GetEnumName(TypeInfo(TsmxDataType), Integer(DataType));
  Node.Attributes['ParamLocation'] := TypInfo.GetEnumName(TypeInfo(TsmxParamLocation), Integer(ParamLocation));
  Node.Attributes['ParamType'] := TypInfo.GetEnumName(TypeInfo(TsmxParamType), Integer(ParamType));
end;

{ TsmxAlgorithmParamKit }

constructor TsmxAlgorithmParamKit.Create;
begin
  Create(TsmxAlgorithmParamKitItem);
end;

function TsmxAlgorithmParamKit.Add: TsmxAlgorithmParamKitItem;
begin
  Result := TsmxAlgorithmParamKitItem(inherited Add);
end;

function TsmxAlgorithmParamKit.GetItem(Index: Integer): TsmxAlgorithmParamKitItem;
begin
  Result := TsmxAlgorithmParamKitItem(inherited Items[Index]);
end;

procedure TsmxAlgorithmParamKit.SetItem(Index: Integer; Value: TsmxAlgorithmParamKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxAlgorithmCfg }

destructor TsmxAlgorithmCfg.Destroy;
begin
  if Assigned(FAlgorithmParams) then
    FAlgorithmParams.Free;
  //if Assigned(FAlgorithmCells) then
    //FAlgorithmCells.Free;
  inherited Destroy;
end;

procedure TsmxAlgorithmCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmCfg then
  begin
    AlgorithmCaption := TsmxAlgorithmCfg(Source).AlgorithmCaption;
    //AlgortihmCells := TsmxAlgorithmCfg(Source).AlgortihmCells;
    AlgorithmEnabled := TsmxAlgorithmCfg(Source).AlgorithmEnabled;
    AlgorithmHint := TsmxAlgorithmCfg(Source).AlgorithmHint;
    AlgorithmHotKey := TsmxAlgorithmCfg(Source).AlgorithmHotKey;
    AlgorithmImageIndex := TsmxAlgorithmCfg(Source).AlgorithmImageIndex;
    AlgorithmParams := TsmxAlgorithmCfg(Source).AlgorithmParams;
    AlgorithmVisible := TsmxAlgorithmCfg(Source).AlgorithmVisible;
    RefreshParamsCfgID := TsmxAlgorithmCfg(Source).RefreshParamsCfgID;
  end;
end;

procedure TsmxAlgorithmCfg.Clear;
begin
  inherited Clear;
  AlgorithmCaption := '';
  //if Assigned(FAlgorithmCells) then
    //FAlgorithmCells.Clear;
  AlgorithmEnabled := False;
  AlgorithmHint := '';
  AlgorithmHotKey := 0;
  AlgorithmImageIndex := -1;
  if Assigned(FAlgorithmParams) then
    FAlgorithmParams.Clear;
  AlgorithmVisible := False;
  RefreshParamsCfgID := 0;
end;

{function TsmxAlgorithmCfg.GetAlgorithmCells: TsmxOwnerKit;
begin
  if not Assigned(FAlgorithmCells) then
    FAlgorithmCells := TsmxOwnerKit.Create(TsmxOwnerKitItem);
  Result := FAlgorithmCells;
end;}

{procedure TsmxAlgorithmCfg.SetAlgorithmCells(Value: TsmxOwnerKit);
begin
  AlgorithmCells.Assign(Value);
end;}

function TsmxAlgorithmCfg.GetAlgorithmParams: TsmxAlgorithmParamKit;
begin
  if not Assigned(FAlgorithmParams) then
  begin
    FAlgorithmParams := TsmxAlgorithmParamKit.Create;
    FAlgorithmParams.KitNodeName := 'Params';
    FAlgorithmParams.ItemNodeName := 'Param';
    FAlgorithmParams.IsWriteEmpty := True;
  end;
  Result := FAlgorithmParams;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmParams(Value: TsmxAlgorithmParamKit);
begin
  AlgorithmParams.Assign(Value);
end;

procedure TsmxAlgorithmCfg.ReadCell(const Node: IXMLNode);
//var
  //n: IXMLNode;
  //i: Integer;
begin
  inherited ReadCell(Node);
  AlgorithmCaption := Node.Attributes['Caption'];
  AlgorithmEnabled := Node.Attributes['Enabled']; //SysUtils.StrToBool(Node.Attributes['Enabled']);
  AlgorithmHint := Node.Attributes['Hint'];
  AlgorithmHotKey := Node.Attributes['HotKey'];
  AlgorithmImageIndex := Node.Attributes['ImageIndex'];
  AlgorithmVisible := Node.Attributes['Visible']; //SysUtils.StrToBool(Node.Attributes['Visible']);

  RefreshParamsCfgID := Node.Attributes['RefreshParamsCfgID'];

  {n := Node.ChildNodes.FindNode('ActionCells');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'ActionCell' then
        with AlgortihmCells.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
        end;
  end;}

  //n := Node.ChildNodes.FindNode('Params');
  //if Assigned(n) and (n.ChildNodes.Count > 0) then
    AlgorithmParams.Read(Node);
  {begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Param' then
        with AlgorithmParams.Add do
        begin
          DataType := n.ChildNodes[i].Attributes['DataType'];
          ParamLocation := n.ChildNodes[i].Attributes['Location'];
          ParamName := n.ChildNodes[i].Attributes['Name'];
          ParamType := n.ChildNodes[i].Attributes['ParamType'];
          ParamValue := smxFuncs.StrToVar(n.ChildNodes[i].Attributes['Value']);
        end;
  end;}
end;

procedure TsmxAlgorithmCfg.WriteCell(const Node: IXMLNode);
//var
  //n: IXMLNode;
  //i: Integer;
begin
  inherited WriteCell(Node);
  Node.Attributes['Caption'] := AlgorithmCaption;
  Node.Attributes['Enabled'] := SysUtils.BoolToStr(AlgorithmEnabled, True);
  Node.Attributes['Hint'] := AlgorithmHint;
  Node.Attributes['HotKey'] := AlgorithmHotKey;
  Node.Attributes['ImageIndex'] := AlgorithmImageIndex;
  Node.Attributes['Visible'] := SysUtils.BoolToStr(AlgorithmVisible, True);

  Node.Attributes['RefreshParamsCfgID'] := RefreshParamsCfgID;

  {n := Node.AddChild('ActionCells');
  for i := 0 to AlgortihmCells.Count - 1 do
    with n.AddChild('ActionCell') do
    begin
      Attributes['CfgID'] := AlgorithmCells[i].CfgID;
    end;}

  //n := Node.AddChild('Params');
  AlgorithmParams.Write(Node);
  {for i := 0 to AlgorithmParams.Count - 1 do
    with n.AddChild('Param') do
    begin
      Attributes['DataType'] := AlgorithmParams[i].DataType;
      Attributes['Location'] := AlgorithmParams[i].ParamLocation;
      Attributes['Name'] := AlgorithmParams[i].ParamName;
      Attributes['ParamType'] := AlgorithmParams[i].ParamType;
      Attributes['Value'] := Variants.VarToStr(AlgorithmParams[i].ParamValue);
    end;}
end;

procedure TsmxAlgorithmCfg.SetAlgorithmCaption(const Value: String);
begin
  FAlgorithmCaption := Value;
end;

procedure TsmxAlgorithmCfg.SetAlgorithmEnabled(Value: Boolean);
begin
  FAlgorithmEnabled := Value;
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

procedure TsmxAlgorithmCfg.SetAlgorithmVisible(Value: Boolean);
begin
  FAlgorithmVisible := Value;
end;

procedure TsmxAlgorithmCfg.SetRefreshParamsCfgID(Value: Integer);
begin
  FRefreshParamsCfgID := Value;
end;

{ TsmxLibAlgorithmCfg }

{procedure TsmxLibAlgorithmCfg.Assign(Source: TPersistent);
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

procedure TsmxLibAlgorithmCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  AlgorithmLibrary := Node.Attributes['Library'];
  AlgorithmProcedure := Node.Attributes['Procedure'];
end;

procedure TsmxLibAlgorithmCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['Library'] := AlgorithmLibrary;
  Node.Attributes['Procedure'] := AlgorithmProcedure;
end;

procedure TsmxLibAlgorithmCfg.SetAlgorithmLibrary(const Value: String);
begin
  FAlgorithmLibrary := Value;
end;

procedure TsmxLibAlgorithmCfg.SetAlgorithmProcedure(const Value: String);
begin
  FAlgorithmProcedure := Value;
end;}

{ TsmxAlgorithmKitItem }

destructor TsmxAlgorithmKitItem.Destroy;
begin
  if Assigned(FItemParams) then
    FItemParams.Free;
  inherited Destroy;
end;

procedure TsmxAlgorithmKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxAlgorithmKitItem then
  begin
    ItemEnabled := TsmxAlgorithmKitItem(Source).ItemEnabled;
    ItemParams := TsmxAlgorithmKitItem(Source).ItemParams;
    ItemVisible := TsmxAlgorithmKitItem(Source).ItemVisible;
  end;
end;

procedure TsmxAlgorithmKitItem.Clear;
begin
  inherited Clear;
  ItemEnabled := False;
  if Assigned(FItemParams) then
    FItemParams.Clear;
  ItemVisible := False;
end;

function TsmxAlgorithmKitItem.GetItemParams: TsmxParamKit;
begin
  if not Assigned(FItemParams) then
  begin
    FItemParams := TsmxParamKit.Create;
    FItemParams.KitNodeName := 'Params';
    FItemParams.ItemNodeName := 'Param';
    //FItemParams.IsWriteEmpty := True;
  end;
  Result := FItemParams;
end;

procedure TsmxAlgorithmKitItem.SetItemParams(Value: TsmxParamKit);
begin
  ItemParams.Assign(Value);
end;

function TsmxAlgorithmKitItem.GetKit: TsmxAlgorithmKit;
begin
  Result := TsmxAlgorithmKit(inherited Kit);
end;

procedure TsmxAlgorithmKitItem.SetKit(Value: TsmxAlgorithmKit);
begin
  inherited Kit := Value;
end;

procedure TsmxAlgorithmKitItem.Read(const Node: IXMLNode);
//var
  //i: Integer;
  //n: IXMLNode;
begin
  inherited Read(Node);
  ItemEnabled := Node.Attributes['ItemEnabled']; //SysUtils.StrToBool(Node.Attributes['ItemEnabled']);
  ItemVisible := Node.Attributes['ItemVisible']; //SysUtils.StrToBool(Node.Attributes['ItemVisible']);
  ItemParams.Read(Node);

  {n := Node.ChildNodes.FindNode('Params');
  if Assigned(n) then
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Param' then
        with ItemParams.Add do
        begin
          ParamName := n.ChildNodes[i].Attributes['ParamName'];
          ParamValue := smxFuncs.StrToVar(n.ChildNodes[i].Attributes['ParamValue']);
        end;}
end;

procedure TsmxAlgorithmKitItem.Write(const Node: IXMLNode);
//var
  //n: IXMLNode;
  //i: Integer;
begin
  inherited Write(Node);
  Node.Attributes['ItemEnabled'] := SysUtils.BoolToStr(ItemEnabled, True);
  Node.Attributes['ItemVisible'] := SysUtils.BoolToStr(ItemVisible, True);
  ItemParams.Write(Node);

  {n := Node.AddChild('Params');
  for i := 0 to ItemParams.Count - 1 do
    with n.AddChild('Param') do
    begin
      Attributes['ParamName'] := ItemParams[i].ParamName;
      Attributes['ParamValue'] := Variants.VarToStr(ItemParams[i].ParamValue);
    end;}
end;

{ TsmxAlgorithmKit }

constructor TsmxAlgorithmKit.Create;
begin
  Create(TsmxAlgorithmKitItem);
end;

function TsmxAlgorithmKit.Add: TsmxAlgorithmKitItem;
begin
  Result := TsmxAlgorithmKitItem(inherited Add);
end;

function TsmxAlgorithmKit.GetItem(Index: Integer): TsmxAlgorithmKitItem;
begin
  Result := TsmxAlgorithmKitItem(inherited Items[Index]);
end;

procedure TsmxAlgorithmKit.SetItem(Index: Integer; Value: TsmxAlgorithmKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxAlgorithmListCfg }

constructor TsmxAlgorithmListCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Algorithms';
  SlaveCells.ItemNodeName := 'Algorithm';
  SlaveCells.IsWriteEmpty := True;
end;

function TsmxAlgorithmListCfg.GetSlaveCells: TsmxAlgorithmKit;
begin
  Result := TsmxAlgorithmKit(inherited SlaveCells);
end;

procedure TsmxAlgorithmListCfg.SetSlaveCells(Value: TsmxAlgorithmKit);
begin
  inherited SlaveCells := Value;
end;

function TsmxAlgorithmListCfg.GetSlaveCellsClass: TsmxSimpleKitClass;
begin
  Result := TsmxAlgorithmKit;
end;

{procedure TsmxAlgorithmListCfg.ReadSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode);
begin
  inherited ReadSlaveCell(Slave, Node);
  TsmxAlgorithmKitItem(Slave).ItemEnabled := SysUtils.StrToBool(Node.Attributes['Enabled']);
  TsmxAlgorithmKitItem(Slave).ItemVisible := SysUtils.StrToBool(Node.Attributes['Visible']);
end;

procedure TsmxAlgorithmListCfg.WriteSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode);
begin
  inherited WriteSlaveCell(Slave, Node);
  Node.Attributes['Enabled'] := SysUtils.BoolToStr(TsmxAlgorithmKitItem(Slave).ItemEnabled, True);
  Node.Attributes['Visible'] := SysUtils.BoolToStr(TsmxAlgorithmKitItem(Slave).ItemVisible, True);
end;}

{ TsmxRequestFieldKitItem }

procedure TsmxRequestFieldKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxRequestFieldKitItem then
  begin
    DataType := TsmxRequestFieldKitItem(Source).DataType;
    DisplayFormat := TsmxRequestFieldKitItem(Source).DisplayFormat;
    FieldName := TsmxRequestFieldKitItem(Source).FieldName;
    FieldSense := TsmxRequestFieldKitItem(Source).FieldSense;
    Precision := TsmxRequestFieldKitItem(Source).Precision;
    Size := TsmxRequestFieldKitItem(Source).Size;
  end;
end;

procedure TsmxRequestFieldKitItem.Clear;
begin
  inherited Clear;
  DataType := ftUnknown;
  DisplayFormat := '';
  FieldName := '';
  FieldSense := fsGeneral;
  Precision := 0;
  Size := 0;
end;

function TsmxRequestFieldKitItem.GetKit: TsmxRequestFieldKit;
begin
  Result := TsmxRequestFieldKit(inherited Kit);
end;

procedure TsmxRequestFieldKitItem.SetKit(Value: TsmxRequestFieldKit);
begin
  inherited Kit := Value;
end;

procedure TsmxRequestFieldKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  DataType := TsmxDataType(TypInfo.GetEnumValue(TypeInfo(TsmxDataType), Node.Attributes['DataType']));
  DisplayFormat := Node.Attributes['DisplayFormat'];
  FieldName := Node.Attributes['FieldName'];
  FieldSense := TsmxFieldSense(TypInfo.GetEnumValue(TypeInfo(TsmxFieldSense), Node.Attributes['FieldSense']));
  Precision := Node.Attributes['Precision'];
  Size := Node.Attributes['Size'];
end;

procedure TsmxRequestFieldKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['DataType'] := TypInfo.GetEnumName(TypeInfo(TsmxDataType), Integer(DataType));
  Node.Attributes['DisplayFormat'] := DisplayFormat;
  Node.Attributes['FieldName'] := FieldName;
  Node.Attributes['FieldSense'] := TypInfo.GetEnumName(TypeInfo(TsmxFieldSense), Integer(FieldSense));
  Node.Attributes['Precision'] := Precision;
  Node.Attributes['Size'] := Size;
end;

{ TsmxRequestFieldKit }

constructor TsmxRequestFieldKit.Create;
begin
  Create(TsmxRequestFieldKitItem);
end;

function TsmxRequestFieldKit.Add: TsmxRequestFieldKitItem;
begin
  Result := TsmxRequestFieldKitItem(inherited Add);
end;

{function TsmxRequestFieldKit.FindByName(const AFieldName: String): TsmxRequestFieldKitItem;
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
end;}

function TsmxRequestFieldKit.GetItem(Index: Integer): TsmxRequestFieldKitItem;
begin
  Result := TsmxRequestFieldKitItem(inherited Items[Index]);
end;

procedure TsmxRequestFieldKit.SetItem(Index: Integer; Value: TsmxRequestFieldKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestParamKitItem }

procedure TsmxRequestParamKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxRequestParamKitItem then
  begin
    NumericScale := TsmxRequestParamKitItem(Source).NumericScale;
    Precision := TsmxRequestParamKitItem(Source).Precision;
    Size := TsmxRequestParamKitItem(Source).Size;
  end;
end;

procedure TsmxRequestParamKitItem.Clear;
begin
  inherited Clear;
  NumericScale := 0;
  Precision := 0;
  Size := 0;
end;

function TsmxRequestParamKitItem.GetKit: TsmxRequestParamKit;
begin
  Result := TsmxRequestParamKit(inherited Kit);
end;

procedure TsmxRequestParamKitItem.SetKit(Value: TsmxRequestParamKit);
begin
  inherited Kit := Value;
end;

procedure TsmxRequestParamKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  NumericScale := Node.Attributes['NumericScale'];
  Precision := Node.Attributes['Precision'];
  Size := Node.Attributes['Size'];
end;

procedure TsmxRequestParamKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['NumericScale'] := NumericScale;
  Node.Attributes['Precision'] := Precision;
  Node.Attributes['Size'] := Size;
end;

{ TsmxRequestParamKit }

constructor TsmxRequestParamKit.Create;
begin
  Create(TsmxRequestParamKitItem);
end;

function TsmxRequestParamKit.Add: TsmxRequestParamKitItem;
begin
  Result := TsmxRequestParamKitItem(inherited Add);
end;

{function TsmxRequestParamKit.FindByName(const Name: String): TsmxRequestParamKitItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, Name) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;}

function TsmxRequestParamKit.GetItem(Index: Integer): TsmxRequestParamKitItem;
begin
  Result := TsmxRequestParamKitItem(inherited Items[Index]);
end;

procedure TsmxRequestParamKit.SetItem(Index: Integer; Value: TsmxRequestParamKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxRequestCfg }

{constructor TsmxRequestCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Fields.KitNodeName := 'Fields';
  Fields.ItemNodeName := 'Field';
  Params.KitNodeName := 'Params';
  Params.ItemNodeName := 'Param';
end;}

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
    DatabaseName := TsmxRequestCfg(Source).DatabaseName;
    DeleteAlgCfgID := TsmxRequestCfg(Source).DeleteAlgCfgID;
    DeletePerformance := TsmxRequestCfg(Source).DeletePerformance;
    DeleteReqCfgID := TsmxRequestCfg(Source).DeleteReqCfgID;
    ExecuteAlgCfgID := TsmxRequestCfg(Source).ExecuteAlgCfgID;
    Fields := TsmxRequestCfg(Source).Fields;
    InsertAlgCfgID := TsmxRequestCfg(Source).InsertAlgCfgID;
    InsertPerformance := TsmxRequestCfg(Source).InsertPerformance;
    InsertReqCfgID := TsmxRequestCfg(Source).InsertReqCfgID;
    OperationMode := TsmxRequestCfg(Source).OperationMode;
    Params := TsmxRequestCfg(Source).Params;
    PerformanceMode := TsmxRequestCfg(Source).PerformanceMode;
    PrepareAlgCfgID := TsmxRequestCfg(Source).PrepareAlgCfgID;
    RefreshParamsAlgCfgID := TsmxRequestCfg(Source).RefreshParamsAlgCfgID;
    SQLText := TsmxRequestCfg(Source).SQLText;
    UpdateAlgCfgID := TsmxRequestCfg(Source).UpdateAlgCfgID;
    UpdatePerformance := TsmxRequestCfg(Source).UpdatePerformance;
    UpdateReqCfgID := TsmxRequestCfg(Source).UpdateReqCfgID;
  end;
end;

procedure TsmxRequestCfg.Clear;
begin
  inherited Clear;
  DatabaseName := '';
  DeleteAlgCfgID := 0;
  DeletePerformance := pmOpen;
  DeleteReqCfgID := 0;
  ExecuteAlgCfgID := 0;
  if Assigned(FFields) then
    FFields.Clear;
  InsertAlgCfgID := 0;
  InsertPerformance := pmOpen;
  InsertReqCfgID := 0;
  if Assigned(FParams) then
    FParams.Clear;
  OperationMode := omManual;
  PerformanceMode := pmOpen;
  PrepareAlgCfgID := 0;
  RefreshParamsAlgCfgID := 0;
  SQLText := '';
  UpdateAlgCfgID := 0;
  UpdatePerformance := pmOpen;
  UpdateReqCfgID := 0;
end;

function TsmxRequestCfg.GetFields: TsmxRequestFieldKit;
begin
  if not Assigned(FFields) then
  begin
    FFields := TsmxRequestFieldKit.Create;
    FFields.KitNodeName := 'Fields';
    FFields.ItemNodeName := 'Field';
    FFields.IsWriteEmpty := True;
  end;
  Result := FFields;
end;

procedure TsmxRequestCfg.SetFields(Value: TsmxRequestFieldKit);
begin
  Fields.Assign(Value);
end;

function TsmxRequestCfg.GetParams: TsmxRequestParamKit;
begin
  if not Assigned(FParams) then
  begin
    FParams := TsmxRequestParamKit.Create;
    FParams.KitNodeName := 'Params';
    FParams.ItemNodeName := 'Param';
    FParams.IsWriteEmpty := True;
  end;
  Result := FParams;
end;

procedure TsmxRequestCfg.SetParams(Value: TsmxRequestParamKit);
begin
  Params.Assign(Value);
end;

procedure TsmxRequestCfg.ReadCell(const Node: IXMLNode);
//var
  //n: IXMLNode;
  //i: Integer;
begin
  inherited ReadCell(Node);
  DatabaseName := Node.Attributes['DatabaseName'];
  OperationMode := TsmxOperationMode(TypInfo.GetEnumValue(TypeInfo(TsmxOperationMode), Node.Attributes['OperationMode']));
  PerformanceMode := TsmxPerformanceMode(TypInfo.GetEnumValue(TypeInfo(TsmxPerformanceMode), Node.Attributes['PerformanceMode']));
  SQLText := Node.Attributes['SQLText'];

  DeletePerformance := TsmxPerformanceMode(TypInfo.GetEnumValue(TypeInfo(TsmxPerformanceMode), Node.Attributes['DeletePerformance']));
  DeleteReqCfgID := Node.Attributes['DeleteReqCfgID'];
  InsertPerformance := TsmxPerformanceMode(TypInfo.GetEnumValue(TypeInfo(TsmxPerformanceMode), Node.Attributes['InsertPerformance']));
  InsertReqCfgID := Node.Attributes['InsertReqCfgID'];
  UpdatePerformance := TsmxPerformanceMode(TypInfo.GetEnumValue(TypeInfo(TsmxPerformanceMode), Node.Attributes['UpdatePerformance']));
  UpdateReqCfgID := Node.Attributes['UpdateReqCfgID'];

  DeleteAlgCfgID := Node.Attributes['DeleteAlgCfgID'];
  ExecuteAlgCfgID := Node.Attributes['ExecuteAlgCfgID'];
  InsertAlgCfgID := Node.Attributes['InsertAlgCfgID'];
  PrepareAlgCfgID := Node.Attributes['PrepareAlgCfgID'];
  RefreshParamsAlgCfgID := Node.Attributes['RefreshParamsAlgCfgID'];
  UpdateAlgCfgID := Node.Attributes['UpdateAlgCfgID'];

  {n := Node.ChildNodes.FindNode('Fields');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
    Fields.Read(n);}
  Fields.Read(Node);
  {begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Field' then
        with Fields.Add do
        begin
          DisplayFormat := n.ChildNodes[i].Attributes['Format'];
          FieldName := n.ChildNodes[i].Attributes['Name'];
          FieldSense := n.ChildNodes[i].Attributes['Sense'];
        end;
  end;}

  {n := Node.ChildNodes.FindNode('Params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
    Params.Read(n);}
  Params.Read(Node);
  {begin
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
  end;}
end;

procedure TsmxRequestCfg.WriteCell(const Node: IXMLNode);
//var
  //n: IXMLNode;
  //i: Integer;
begin
  inherited WriteCell(Node);
  Node.Attributes['DatabaseName'] := DatabaseName;
  Node.Attributes['OperationMode'] := TypInfo.GetEnumName(TypeInfo(TsmxOperationMode), Integer(OperationMode));
  Node.Attributes['PerformanceMode'] := TypInfo.GetEnumName(TypeInfo(TsmxPerformanceMode), Integer(PerformanceMode));
  Node.Attributes['SQLText'] := SQLText;

  Node.Attributes['DeletePerformance'] := TypInfo.GetEnumName(TypeInfo(TsmxPerformanceMode), Integer(DeletePerformance));
  Node.Attributes['DeleteReqCfgID'] := DeleteReqCfgID;
  Node.Attributes['InsertPerformance'] := TypInfo.GetEnumName(TypeInfo(TsmxPerformanceMode), Integer(InsertPerformance));
  Node.Attributes['InsertReqCfgID'] := InsertReqCfgID;
  Node.Attributes['UpdatePerformance'] := TypInfo.GetEnumName(TypeInfo(TsmxPerformanceMode), Integer(UpdatePerformance));
  Node.Attributes['UpdateReqCfgID'] := UpdateReqCfgID;

  Node.Attributes['DeleteAlgCfgID'] := DeleteAlgCfgID;
  Node.Attributes['ExecuteAlgCfgID'] := ExecuteAlgCfgID;
  Node.Attributes['InsertAlgCfgID'] := InsertAlgCfgID;
  Node.Attributes['PrepareAlgCfgID'] := PrepareAlgCfgID;
  Node.Attributes['RefreshParamsAlgCfgID'] := RefreshParamsAlgCfgID;
  Node.Attributes['UpdateAlgCfgID'] := UpdateAlgCfgID;

  {n := Node.AddChild('Fields');
  Fields.Write(n);}
  Fields.Write(Node);
  {for i := 0 to Fields.Count - 1 do
    with n.AddChild('Field') do
    begin
      Attributes['Format'] := Fields[i].DisplayFormat;
      Attributes['Name'] := Fields[i].FieldName;
      Attributes['Sense'] := Fields[i].FieldSense;
    end;}

  {n := Node.AddChild('Params');
  Params.Write(n);}
  Params.Write(Node);
  {for i := 0 to Params.Count - 1 do
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
    end;}
end;

procedure TsmxRequestCfg.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
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

procedure TsmxRequestCfg.SetModifyPerformance(Index: Integer; Value: TsmxPerformanceMode);
begin
  case Index of
    1: FDeletePerformance := Value;
    2: FInsertPerformance := Value;
    3: FUpdatePerformance := Value;
  end;
end;

procedure TsmxRequestCfg.SetOperationMode(Value: TsmxOperationMode);
begin
  FOperationMode := Value;
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

{ TsmxRequestKitItem }

destructor TsmxRequestKitItem.Destroy;
begin
  if Assigned(FItemParams) then
    FItemParams.Free;
  inherited Destroy;
end;

procedure TsmxRequestKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxRequestKitItem then
  begin
    DatabaseName := TsmxRequestKitItem(Source).DatabaseName;
    ItemParams := TsmxRequestKitItem(Source).ItemParams;
    OperationMode := TsmxRequestKitItem(Source).OperationMode;
  end;
end;

procedure TsmxRequestKitItem.Clear;
begin
  inherited Clear;
  DatabaseName := '';
  if Assigned(FItemParams) then
    FItemParams.Clear;
  OperationMode := omManual;
end;

function TsmxRequestKitItem.GetItemParams: TsmxParamKit;
begin
  if not Assigned(FItemParams) then
  begin
    FItemParams := TsmxParamKit.Create;
    FItemParams.KitNodeName := 'Params';
    FItemParams.ItemNodeName := 'Param';
    //FItemParams.IsWriteEmpty := True;
  end;
  Result := FItemParams;
end;

procedure TsmxRequestKitItem.SetItemParams(Value: TsmxParamKit);
begin
  ItemParams.Assign(Value);
end;

function TsmxRequestKitItem.GetKit: TsmxRequestKit;
begin
  Result := TsmxRequestKit(inherited Kit);
end;

procedure TsmxRequestKitItem.SetKit(Value: TsmxRequestKit);
begin
  inherited Kit := Value;
end;

procedure TsmxRequestKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  DatabaseName := Node.Attributes['DatabaseName'];
  OperationMode := TsmxOperationMode(TypInfo.GetEnumValue(TypeInfo(TsmxOperationMode), Node.Attributes['OperationMode']));
  ItemParams.Read(Node);
end;

procedure TsmxRequestKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['DatabaseName'] := DatabaseName;
  Node.Attributes['OperationMode'] := TypInfo.GetEnumName(TypeInfo(TsmxOperationMode), Integer(OperationMode));
  ItemParams.Write(Node);
end;

{ TsmxRequestKit }

constructor TsmxRequestKit.Create;
begin
  Create(TsmxRequestKitItem);
end;

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

constructor TsmxRequestListCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Requests';
  SlaveCells.ItemNodeName := 'Request';
  SlaveCells.IsWriteEmpty := True;
end;

function TsmxRequestListCfg.GetSlaveCells: TsmxRequestKit;
begin
  Result := TsmxRequestKit(inherited SlaveCells);
end;

procedure TsmxRequestListCfg.SetSlaveCells(Value: TsmxRequestKit);
begin
  inherited SlaveCells := Value;
end;

function TsmxRequestListCfg.GetSlaveCellsClass: TsmxSimpleKitClass;
begin
  Result := TsmxRequestKit;
end;

{procedure TsmxRequestListCfg.ReadSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode);
begin
  inherited ReadSlaveCell(Slave, Node);
  TsmxRequestKitItem(Slave).DatabaseName := Node.Attributes['DatabaseName'];
  TsmxRequestKitItem(Slave).OperationMode := Node.Attributes['OperationMode'];
end;

procedure TsmxRequestListCfg.WriteSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode);
begin
  inherited WriteSlaveCell(Slave, Node);
  Node.Attributes['DatabaseName'] := TsmxRequestKitItem(Slave).DatabaseName;
  Node.Attributes['OperationMode'] := TsmxRequestKitItem(Slave).OperationMode;
end;}

{ TsmxColumnCfg }

destructor TsmxColumnCfg.Destroy;
begin
  if Assigned(FColumnHeader) then
    FColumnHeader.Free;
  if Assigned(FColumnText) then
    FColumnText.Free;
  inherited Destroy;
end;

procedure TsmxColumnCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxColumnCfg then
  begin
    //ColumnFieldName := TsmxColumnCfg(Source).ColumnFieldName;
    ColumnHeader := TsmxColumnCfg(Source).ColumnHeader;
    ColumnText := TsmxColumnCfg(Source).ColumnText;
    IsEditing := TsmxColumnCfg(Source).IsEditing;
    SnapHeaderAlgCfgID := TsmxColumnCfg(Source).SnapHeaderAlgCfgID;
  end;
end;

procedure TsmxColumnCfg.Clear;
//var
  //DefText: TsmxCellText;
begin
  inherited Clear;
  //ColumnFieldName := '';
  //DefText := smxFuncs.DefCellText;
  //DefText.Color := Integer(Graphics.clBtnFace);
  ColumnHeader.Clear; //:= DefText;
  ColumnHeader.Color := Graphics.clBtnFace;
  ColumnText.Clear; //:= smxFuncs.DefCellText;
  IsEditing := False;
  SnapHeaderAlgCfgID := 0;
end;

function TsmxColumnCfg.GetColumnHeader: TsmxText;
begin
  if not Assigned(FColumnHeader) then
    FColumnHeader := TsmxText.Create;
  Result := FColumnHeader;
end;

procedure TsmxColumnCfg.SetColumnHeader(Value: TsmxText);
begin
  ColumnHeader.Assign(Value);
end;

function TsmxColumnCfg.GetColumnText: TsmxText;
begin
  if not Assigned(FColumnText) then
    FColumnText := TsmxText.Create;
  Result := FColumnText;
end;

procedure TsmxColumnCfg.SetColumnText(Value: TsmxText);
begin
  ColumnText.Assign(Value);
end;

procedure TsmxColumnCfg.ReadCell(const Node: IXMLNode);
var
  n{, n2}: IXMLNode;
  //Text: TsmxCellText;
begin
  inherited ReadCell(Node);
  //ColumnFieldName := Node.Attributes['FieldName'];
  IsEditing := Node.Attributes['IsEditing'];
  SnapHeaderAlgCfgID := Node.Attributes['SnapHeaderAlgCfgID'];

  n := Node.ChildNodes.FindNode('Column');
  if Assigned(n) then
    ColumnText.Read(n);
  //begin
    //smxProcs.ReadText(n, Text);
    //ColumnText := Text;
  //end;
    {with ColumnText do
    begin
      Align := n.Attributes['Align'];
      Color := n.Attributes['Color'];
      n2 := n.ChildNodes.FindNode('Font');
      if Assigned(n2) then
        with Font do
        begin
          Color := n2.Attributes['Color'];
          Name := n2.Attributes['Name'];
          Size := n2.Attributes['Size'];
          Style := TFontStyles(Byte(n2.Attributes['Style']));
        end;
    end;}

  n := Node.ChildNodes.FindNode('Header');
  if Assigned(n) then
    ColumnHeader.Read(n);
  //begin
    //smxProcs.ReadText(n, Text);
    //ColumnHeader := Text;
  //end;
    {with ColumnHeader do
    begin
      Caption := n.Attributes['Caption'];
      Align := n.Attributes['Align'];
      Color := n.Attributes['Color'];
      n2 := n.ChildNodes.FindNode('Font');
      if Assigned(n2) then
        with Font do
        begin
          Color := n2.Attributes['Color'];
          Name := n2.Attributes['Name'];
          Size := n2.Attributes['Size'];
          Style := TFontStyles(Byte(n2.Attributes['Style']));
        end;
    end;}
end;

procedure TsmxColumnCfg.WriteCell(const Node: IXMLNode);
var
  n{, n2}: IXMLNode;
begin
  inherited WriteCell(Node);
  //Node.Attributes['FieldName'] := ColumnFieldName;
  Node.Attributes['IsEditing'] := SysUtils.BoolToStr(IsEditing, True);
  Node.Attributes['SnapHeaderAlgCfgID'] := SnapHeaderAlgCfgID;

  n := Node.AddChild('Column');
  ColumnText.Write(n);
  //smxProcs.WriteText(n, ColumnText);
  {with ColumnText do
  begin
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
  end;}

  n := Node.AddChild('Header');
  ColumnHeader.Write(n);
  //smxProcs.WriteText(n, ColumnHeader);
  {with ColumnHeader do
  begin
    n.Attributes['Caption'] := Caption;
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
  end;}
end;

procedure TsmxColumnCfg.SetColumnFieldName(const Value: String);
begin
  FColumnFieldName := Value;
end;

procedure TsmxColumnCfg.SetIsEditing(Value: Boolean);
begin
  FIsEditing := Value;
end;

procedure TsmxColumnCfg.SetSnapHeaderAlgCfgID(Value: Integer);
begin
  FSnapHeaderAlgCfgID := Value;
end;

{ TsmxGridCfg }

constructor TsmxGridCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Columns';
  SlaveCells.ItemNodeName := 'Column';
  SlaveCells.IsWriteEmpty := True;
  IsSetAlgorithmEvents := True;
end;

procedure TsmxGridCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxGridCfg then
  begin
    //PressDoubleAlgCfgID := TsmxGridCfg(Source).PressDoubleAlgCfgID;
    //PressHeaderAlgCfgID := TsmxGridCfg(Source).PressHeaderAlgCfgID;

    ApplyAlgCfgID := TsmxGridCfg(Source).ApplyAlgCfgID;
    ChangeRowAlgCfgID := TsmxGridCfg(Source).ChangeRowAlgCfgID;
    GridOptions := TsmxGridCfg(Source).GridOptions;
    PrepareAlgCfgID := TsmxGridCfg(Source).PrepareAlgCfgID;
    RefreshAlgCfgID := TsmxGridCfg(Source).RefreshAlgCfgID;
    RequestCfgID := TsmxGridCfg(Source).RequestCfgID;
  end;
end;

procedure TsmxGridCfg.Clear;
begin
  inherited Clear;
  ApplyAlgCfgID := 0;
  ChangeRowAlgCfgID := 0;
  GridOptions := [];
  //PressDoubleAlgCfgID := 0;
  //PressHeaderAlgCfgID := 0;
  PrepareAlgCfgID := 0;
  RefreshAlgCfgID := 0;
  RequestCfgID := 0;
end;

procedure TsmxGridCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  GridOptions := TsmxGridOptions(smxFuncs.StrToSet(TypeInfo(TsmxGridOption), Node.Attributes['GridOptions']));
  RequestCfgID := Node.Attributes['RequestCfgID'];

  ApplyAlgCfgID := Node.Attributes['ApplyAlgCfgID'];
  ChangeRowAlgCfgID := Node.Attributes['ChangeRowAlgCfgID'];
  //PressDoubleAlgCfgID := Node.Attributes['PressDoubleAlgCfgID'];
  //PressHeaderAlgCfgID := Node.Attributes['PressHeaderAlgCfgID'];
  PrepareAlgCfgID := Node.Attributes['PrepareAlgCfgID'];
  RefreshAlgCfgID := Node.Attributes['RefreshAlgCfgID'];
end;

procedure TsmxGridCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['GridOptions'] := smxFuncs.SetToStr(TypeInfo(TsmxGridOption), Byte(GridOptions), True);
  Node.Attributes['RequestCfgID'] := RequestCfgID;

  Node.Attributes['ApplyAlgCfgID'] := ApplyAlgCfgID;
  Node.Attributes['ChangeRowAlgCfgID'] := ChangeRowAlgCfgID;
  //Node.Attributes['PressDoubleAlgCfgID'] := PressDoubleAlgCfgID;
  //Node.Attributes['PressHeaderAlgCfgID'] := PressHeaderAlgCfgID;
  Node.Attributes['PrepareAlgCfgID'] := PrepareAlgCfgID;
  Node.Attributes['RefreshAlgCfgID'] := RefreshAlgCfgID;
end;

procedure TsmxGridCfg.SetApplyAlgCfgID(Value: Integer);
begin
  FApplyAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetChangeRowAlgCfgID(Value: Integer);
begin
  FChangeRowAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetGridOptions(Value: TsmxGridOptions);
begin
  FGridOptions := Value;
end;

{procedure TsmxGridCfg.SetPressDoubleAlgCfgID(Value: Integer);
begin
  FPressDoubleAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetPressHeaderAlgCfgID(Value: Integer);
begin
  FPressHeaderAlgCfgID := Value;
end;}

procedure TsmxGridCfg.SetPrepareAlgCfgID(Value: Integer);
begin
  FPrepareAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetRefreshAlgCfgID(Value: Integer);
begin
  FRefreshAlgCfgID := Value;
end;

procedure TsmxGridCfg.SetRequestCfgID(Value: Integer);
begin
  FRequestCfgID := Value;
end;

{ TsmxFilterCfg }

destructor TsmxFilterCfg.Destroy;
begin
  if Assigned(FFilterHeader) then
    FFilterHeader.Free;
  if Assigned(FFilterText) then
    FFilterText.Free;
  inherited Destroy;
end;

procedure TsmxFilterCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxFilterCfg then
  begin
    ChangeFilterAlgCfgID := TsmxFilterCfg(Source).ChangeFilterAlgCfgID;
    DisplayFormat := TsmxFilterCfg(Source).DisplayFormat;
    FilterHeader := TsmxFilterCfg(Source).FilterHeader;
    //FilterName := TsmxFilterCfg(Source).FilterName;
    FilterOptions := TsmxFilterCfg(Source).FilterOptions;
    FilterText := TsmxFilterCfg(Source).FilterText;
    FilterValue := TsmxFilterCfg(Source).FilterValue;
    //RequestCfgID := TsmxFilterCfg(Source).RequestCfgID;
    ValueFormat := TsmxFilterCfg(Source).ValueFormat;
  end;
end;

procedure TsmxFilterCfg.Clear;
//var
  //DefText: TsmxCellText;
begin
  inherited Clear;
  ChangeFilterAlgCfgID := 0;
  DisplayFormat := '';
  //DefText := smxFuncs.DefCellText;
  //DefText.Color := Integer(Graphics.clBtnFace);
  FilterHeader.Clear;
  FilterHeader.Color := Graphics.clBtnFace;
  //FilterName := '';
  FilterOptions := [];
  //FilterText := smxFuncs.DefCellText;
  FilterText.Clear;
  FilterValue := Variants.Null;
  //RequestCfgID := 0;
  ValueFormat := '';
end;

function TsmxFilterCfg.GetFilterHeader: TsmxText;
begin
  if not Assigned(FFilterHeader) then
    FFilterHeader := TsmxText.Create;
  Result := FFilterHeader;
end;

procedure TsmxFilterCfg.SetFilterHeader(Value: TsmxText);
begin
  FilterHeader.Assign(Value);
end;

function TsmxFilterCfg.GetFilterText: TsmxText;
begin
  if not Assigned(FFilterText) then
    FFilterText := TsmxText.Create;
  Result := FFilterText;
end;

procedure TsmxFilterCfg.SetFilterText(Value: TsmxText);
begin
  FilterText.Assign(Value);
end;

procedure TsmxFilterCfg.ReadCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited ReadCell(Node);
  DisplayFormat := Node.Attributes['DisplayFormat'];
  //FilterName := Node.Attributes['FilterName'];
  FilterOptions := TsmxFilterOptions(smxFuncs.StrToSet(TypeInfo(TsmxFilterOption), Node.Attributes['FilterOptions']));
  FilterValue := smxFuncs.StrToVar(Node.Attributes['FilterValue']);
  //RequestCfgID := Node.Attributes['RequestCfgID'];
  ValueFormat := Node.Attributes['ValueFormat'];

  ChangeFilterAlgCfgID := Node.Attributes['ChangeFilterAlgCfgID'];

  n := Node.ChildNodes.FindNode('Filter');
  if Assigned(n) then
    FilterText.Read(n);
    //smxProcs.ReadText(n, FFilterText);
  {begin
    with FilterText do
    begin
      Caption := n.Attributes['Caption'];
      Align := n.Attributes['Align'];
      Color := n.Attributes['Color'];
      n2 := n.ChildNodes.FindNode('Font');
      if Assigned(n2) then
        with Font do
        begin
          Color := n2.Attributes['Color'];
          Name := n2.Attributes['Name'];
          Size := n2.Attributes['Size'];
          Style := TFontStyles(Byte(n2.Attributes['Style']));
        end;
    end;
  end;}

  n := Node.ChildNodes.FindNode('Header');
  if Assigned(n) then
    FilterHeader.Read(n);
    //smxProcs.ReadText(n, FFilterHeader);
    {with FilterHeader do
    begin
      Caption := n.Attributes['Caption'];
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
    end;}
end;

procedure TsmxFilterCfg.WriteCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited WriteCell(Node);
  Node.Attributes['DisplayFormat'] := DisplayFormat;
  //Node.Attributes['FilterName'] := FilterName;
  Node.Attributes['FilterOptions'] := smxFuncs.SetToStr(TypeInfo(TsmxFilterOption), Byte(FilterOptions), True);
  Node.Attributes['FilterValue'] := Variants.VarToStr(FilterValue);
  //Node.Attributes['RequestCfgID'] := RequestCfgID;
  Node.Attributes['ValueFormat'] := ValueFormat;

  Node.Attributes['ChangeFilterAlgCfgID'] := ChangeFilterAlgCfgID;

  n := Node.AddChild('Filter');
  FilterText.Write(n);
  //smxProcs.WriteText(n, FFilterText);
  {with FilterText do
  begin
    n.Attributes['Caption'] := Caption;
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
  end;}

  n := Node.AddChild('Header');
  FilterHeader.Write(n);
  //smxProcs.WriteText(n, FFilterHeader);
  {with FilterHeader do
  begin
    n.Attributes['Caption'] := Caption;
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
  end;}
end;

procedure TsmxFilterCfg.SetChangeFilterAlgCfgID(Value: Integer);
begin
  FChangeFilterAlgCfgID := Value;
end;

procedure TsmxFilterCfg.SetDisplayFormat(const Value: String);
begin
  FDisplayFormat := Value;
end;

{procedure TsmxFilterCfg.SetFilterName(const Value: String);
begin
  FFilterName := Value;
end;}

procedure TsmxFilterCfg.SetFilterOptions(Value: TsmxFilterOptions);
begin
  FFilterOptions := Value;
end;

procedure TsmxFilterCfg.SetFilterValue(const Value: Variant);
begin
  FFilterValue := Value;
end;

{procedure TsmxFilterCfg.SetRequestCfgID(Value: Integer);
begin
  FRequestCfgID := Value;
end;}

procedure TsmxFilterCfg.SetValueFormat(const Value: String);
begin
  FValueFormat := Value;
end;

{ TsmxFilterKitItem }

procedure TsmxFilterKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxFilterKitItem then
  begin
    DisplayFormat := TsmxFilterKitItem(Source).DisplayFormat;
    FilterOptions := TsmxFilterKitItem(Source).FilterOptions;
    ValueFormat := TsmxFilterKitItem(Source).ValueFormat;
  end;
end;

procedure TsmxFilterKitItem.Clear;
begin
  inherited Clear;
  DisplayFormat := '';
  FilterOptions := [];
  ValueFormat := '';
end;

function TsmxFilterKitItem.GetKit: TsmxFilterKit;
begin
  Result := TsmxFilterKit(inherited Kit);
end;

procedure TsmxFilterKitItem.SetKit(Value: TsmxFilterKit);
begin
  inherited Kit := Value;
end;

procedure TsmxFilterKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  DisplayFormat := Node.Attributes['DisplayFormat'];
  FilterOptions := TsmxFilterOptions(smxFuncs.StrToSet(TypeInfo(TsmxFilterOption), Node.Attributes['FilterOptions']));
  ValueFormat := Node.Attributes['ValueFormat'];
end;

procedure TsmxFilterKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['DisplayFormat'] := DisplayFormat;
  Node.Attributes['FilterOptions'] := smxFuncs.SetToStr(TypeInfo(TsmxFilterOption), Byte(FilterOptions), True);
  Node.Attributes['ValueFormat'] := ValueFormat;
end;

{ TsmxFilterKit }

constructor TsmxFilterKit.Create;
begin
  Create(TsmxFilterKitItem);
end;

function TsmxFilterKit.Add: TsmxFilterKitItem;
begin
  Result := TsmxFilterKitItem(inherited Add);
end;

function TsmxFilterKit.GetItem(Index: Integer): TsmxFilterKitItem;
begin
  Result := TsmxFilterKitItem(inherited Items[Index]);
end;

procedure TsmxFilterKit.SetItem(Index: Integer; Value: TsmxFilterKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxFilterDeskCfg }

constructor TsmxFilterDeskCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Filters';
  SlaveCells.ItemNodeName := 'Filter';
  SlaveCells.IsWriteEmpty := True;
end;

procedure TsmxFilterDeskCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxFilterDeskCfg then
  begin
    ApplyAlgCfgID := TsmxFilterDeskCfg(Source).ApplyAlgCfgID;
    PrepareAlgCfgID := TsmxFilterDeskCfg(Source).PrepareAlgCfgID;
    RefreshAlgCfgID := TsmxFilterDeskCfg(Source).RefreshAlgCfgID;
    RequestCfgID := TsmxFilterDeskCfg(Source).RequestCfgID;
  end;
end;

procedure TsmxFilterDeskCfg.Clear;
begin
  inherited Clear;
  ApplyAlgCfgID := 0;
  PrepareAlgCfgID := 0;
  PrepareAlgCfgID := 0;
  RequestCfgID := 0;
end;

function TsmxFilterDeskCfg.GetSlaveCells: TsmxFilterKit;
begin
  Result := TsmxFilterKit(inherited SlaveCells);
end;

procedure TsmxFilterDeskCfg.SetSlaveCells(Value: TsmxFilterKit);
begin
  inherited SlaveCells := Value;
end;

function TsmxFilterDeskCfg.GetSlaveCellsClass: TsmxSimpleKitClass;
begin
  Result := TsmxFilterKit;
end;

procedure TsmxFilterDeskCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  RequestCfgID := Node.Attributes['RequestCfgID'];

  ApplyAlgCfgID := Node.Attributes['ApplyAlgCfgID'];
  PrepareAlgCfgID := Node.Attributes['PrepareAlgCfgID'];
  RefreshAlgCfgID := Node.Attributes['RefreshAlgCfgID'];
end;

procedure TsmxFilterDeskCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['RequestCfgID'] := RequestCfgID;

  Node.Attributes['ApplyAlgCfgID'] := ApplyAlgCfgID;
  Node.Attributes['PrepareAlgCfgID'] := PrepareAlgCfgID;
  Node.Attributes['RefreshAlgCfgID'] := RefreshAlgCfgID;
end;

{procedure TsmxFilterDeskCfg.ReadSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode);
begin
  inherited ReadSlaveCell(Slave, Node);
  TsmxFilterKitItem(Slave).DisplayFormat := Node.Attributes['DisplayFormat'];
  TsmxFilterKitItem(Slave).FilterOptions := TsmxFilterOptions(Byte(Node.Attributes['FilterOptions']));
  TsmxFilterKitItem(Slave).ValueFormat := Node.Attributes['ValueFormat'];
end;

procedure TsmxFilterDeskCfg.WriteSlaveCell(Slave: TsmxOwnerKitItem; const Node: IXMLNode);
begin
  inherited WriteSlaveCell(Slave, Node);
  Node.Attributes['DisplayFormat'] := TsmxFilterKitItem(Slave).DisplayFormat;
  Node.Attributes['FilterOptions'] := Byte(TsmxFilterKitItem(Slave).FilterOptions);
  Node.Attributes['ValueFormat'] := TsmxFilterKitItem(Slave).ValueFormat;
end;}

procedure TsmxFilterDeskCfg.SetApplyAlgCfgID(Value: Integer);
begin
  FApplyAlgCfgID := Value;
end;

procedure TsmxFilterDeskCfg.SetPrepareAlgCfgID(Value: Integer);
begin
  FPrepareAlgCfgID := Value;
end;

procedure TsmxFilterDeskCfg.SetRefreshAlgCfgID(Value: Integer);
begin
  FRefreshAlgCfgID := Value;
end;

procedure TsmxFilterDeskCfg.SetRequestCfgID(Value: Integer);
begin
  FRequestCfgID := Value;
end;

{ TsmxRequestSectionCfg }

{destructor TsmxRequestSectionCfg.Destroy;
begin
  if Assigned(FFilterDesk) then
    FFilterDesk.Free;
  if Assigned(FGrid) then
    FGrid.Free;
  inherited Destroy;
end;

procedure TsmxRequestSectionCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxRequestSectionCfg then
  begin
    FilterDesk := TsmxRequestSectionCfg(Source).FilterDesk;
    Grid := TsmxRequestSectionCfg(Source).Grid;
  end;
end;

procedure TsmxRequestSectionCfg.Clear;
begin
  inherited Clear;
  if Assigned(FFilterDesk) then
    FFilterDesk.Clear;
  if Assigned(FGrid) then
    FGrid.Clear;
end;

function TsmxRequestSectionCfg.GetFilterDesk: TsmxControlKitItem;
begin
  if not Assigned(FFilterDesk) then
    FFilterDesk := TsmxControlKitItem.Create(nil);
  Result := FFilterDesk;
end;

procedure TsmxRequestSectionCfg.SetFilterDesk(Value: TsmxControlKitItem);
begin
  FilterDesk.Assign(Value);
end;

function TsmxRequestSectionCfg.GetGrid: TsmxControlKitItem;
begin
  if not Assigned(FGrid) then
    FGrid := TsmxControlKitItem.Create(nil);
  Result := FGrid;
end;

procedure TsmxRequestSectionCfg.SetGrid(Value: TsmxControlKitItem);
begin
  Grid.Assign(Value);
end;}

(*procedure TsmxRequestSectionCfg.ReadCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited ReadCell(Node);
  n := Node.ChildNodes.FindNode('FilterDesk');
  if Assigned(n) then
    FilterDesk.Read(n);
    {with FilterDesk do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}

  n := Node.ChildNodes.FindNode('Grid');
  if Assigned(n) then
    Grid.Read(n);
    {with Grid do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}
end;

procedure TsmxRequestSectionCfg.WriteCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited WriteCell(Node);
  n := Node.AddChild('FilterDesk');
  FilterDesk.Write(n);
  {with FilterDesk do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}

  n := Node.AddChild('Grid');
  Grid.Write(n);
  {with Grid do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}
end;*)

{ TsmxSectionCfg }

constructor TsmxSectionCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Controls';
  SlaveCells.ItemNodeName := 'Control';
  SlaveCells.IsWriteEmpty := True;
end;

{destructor TsmxSectionCfg.Destroy;
begin
  if Assigned(FControl) then
    FControl.Free;
  inherited Destroy;
end;

procedure TsmxSectionCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxSectionCfg then
    Control := TsmxSectionCfg(Source).Control;
end;

procedure TsmxSectionCfg.Clear;
begin
  inherited Clear;
  if Assigned(FControl) then
    FControl.Clear;
end;

function TsmxSectionCfg.GetControl: TsmxControlKitItem;
begin
  if not Assigned(FControl) then
    FControl := TsmxControlKitItem.Create(nil);
  Result := FControl;
end;

procedure TsmxSectionCfg.ReadCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited ReadCell(Node);
  n := Node.ChildNodes.FindNode('Control');
  if Assigned(n) then
    Control.Read(n);
end;

procedure TsmxSectionCfg.WriteCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited WriteCell(Node);
  n := Node.AddChild('Control');
  Control.Write(n);
end;

procedure TsmxSectionCfg.SetControl(Value: TsmxControlKitItem);
begin
  Control.Assign(Value);
end;}

{ TsmxPageCfg }

constructor TsmxPageCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Sections';
  SlaveCells.ItemNodeName := 'Section';
  SlaveCells.IsWriteEmpty := True;
end;

{procedure TsmxPageCfg.Clear;
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
          ItemEnabled := n.ChildNodes[i].Attributes['Enabled'];
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
      Attributes['Enabled'] := BoolToStr(Sections[i].ItemEnabled, True);
      Attributes['Height'] := Sections[i].ItemHeight;
      Attributes['Left'] := Sections[i].ItemLeft;
      Attributes['Top'] := Sections[i].ItemTop;
      Attributes['Visible'] := BoolToStr(Sections[i].ItemVisible, True);
      Attributes['Width'] := Sections[i].ItemWidth;
    end;
end;}

{ TsmxPageManagerCfg }

constructor TsmxPageManagerCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'Pages';
  SlaveCells.ItemNodeName := 'Page';
  SlaveCells.IsWriteEmpty := True;
end;

procedure TsmxPageManagerCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxPageManagerCfg then
  begin
    ActivePageIndex := TsmxPageManagerCfg(Source).ActivePageIndex;
    ChangePageAlgCfgID := TsmxPageManagerCfg(Source).ChangePageAlgCfgID;
    IsMultiLine := TsmxPageManagerCfg(Source).IsMultiLine;
    PageManagerStyle := TsmxPageManagerCfg(Source).PageManagerStyle;
  end;
end;

procedure TsmxPageManagerCfg.Clear;
begin
  inherited Clear;
  ActivePageIndex := -1;
  ChangePageAlgCfgID := 0;
  IsMultiLine := False;
  PageManagerStyle := pmsTab;
end;

procedure TsmxPageManagerCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  ActivePageIndex := Node.Attributes['ActivePageIndex'];
  ChangePageAlgCfgID := Node.Attributes['ChangePageAlgCfgID'];
  IsMultiLine := Node.Attributes['MultiLine']; //SysUtils.StrToBool(Node.Attributes['MultiLine']);
  PageManagerStyle := TsmxPageManagerStyle(TypInfo.GetEnumValue(TypeInfo(TsmxPageManagerStyle), Node.Attributes['PagesStyle']));
end;

procedure TsmxPageManagerCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['ActivePageIndex'] := ActivePageIndex;
  Node.Attributes['ChangePageAlgCfgID'] := ChangePageAlgCfgID;
  Node.Attributes['MultiLine'] := SysUtils.BoolToStr(IsMultiLine, True);
  Node.Attributes['PagesStyle'] := TypInfo.GetEnumName(TypeInfo(TsmxPageManagerStyle), Integer(PageManagerStyle));
end;

procedure TsmxPageManagerCfg.SetActivePageIndex(Value: Integer);
begin
  FActivePageIndex := Value;
end;

procedure TsmxPageManagerCfg.SetChangePageAlgCfgID(Value: Integer);
begin
  FChangePageAlgCfgID := Value;
end;

procedure TsmxPageManagerCfg.SetIsMultiLine(Value: Boolean);
begin
  FIsMultiLine := Value;
end;

procedure TsmxPageManagerCfg.SetPageManagerStyle(Value: TsmxPageManagerStyle);
begin
  FPageManagerStyle := Value;
end;

{ TsmxMenuItemCfg }

constructor TsmxMenuItemCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'MenuItems';
  SlaveCells.ItemNodeName := 'MenuItem';
  SlaveCells.IsWriteEmpty := True;
  IsSetAlgorithmEvents := True;
end;

procedure TsmxMenuItemCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxMenuItemCfg then
  begin
    IsChecked := TsmxMenuItemCfg(Source).IsChecked;
    MenuItemHotKey := TsmxMenuItemCfg(Source).MenuItemHotKey;
    MenuItemStyle := TsmxMenuItemCfg(Source).MenuItemStyle;
  end;
end;

procedure TsmxMenuItemCfg.Clear;
begin
  inherited Clear;
  IsChecked := False;
  MenuItemHotKey := 0;
  MenuItemStyle := misPoint;
end;

procedure TsmxMenuItemCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  IsChecked := Node.Attributes['IsChecked']; //SysUtils.StrToBool(Node.Attributes['IsChecked']);
  MenuItemHotKey := Node.Attributes['MenuItemHotKey'];
  MenuItemStyle := TsmxMenuItemStyle(TypInfo.GetEnumValue(TypeInfo(TsmxMenuItemStyle), Node.Attributes['MenuItemStyle']));
end;

procedure TsmxMenuItemCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['IsChecked'] := SysUtils.BoolToStr(IsChecked, True);
  Node.Attributes['MenuItemHotKey'] := MenuItemHotKey;
  Node.Attributes['MenuItemStyle'] := TypInfo.GetEnumName(TypeInfo(TsmxMenuItemStyle), Integer(MenuItemStyle));
end;

procedure TsmxMenuItemCfg.SetIsChecked(Value: Boolean);
begin
  FIsChecked := Value;
end;

procedure TsmxMenuItemCfg.SetMenuItemHotKey(Value: Integer);
begin
  FMenuItemHotKey := Value;
end;

procedure TsmxMenuItemCfg.SetMenuItemStyle(Value: TsmxMenuItemStyle);
begin
  FMenuItemStyle := Value;
end;

{ TsmxMainMenuCfg }

constructor TsmxMainMenuCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'MenuItems';
  SlaveCells.ItemNodeName := 'MenuItem';
  SlaveCells.IsWriteEmpty := True;
end;

{ TsmxPopupMenuCfg }

constructor TsmxPopupMenuCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'MenuItems';
  SlaveCells.ItemNodeName := 'MenuItem';
  SlaveCells.IsWriteEmpty := True;
end;

{ TsmxPopupListCfg }

constructor TsmxPopupListCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'PopupMenus';
  SlaveCells.ItemNodeName := 'PopupMenu';
  SlaveCells.IsWriteEmpty := True;
end;

{ TsmxToolItemCfg }

constructor TsmxToolItemCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsSetAlgorithmEvents := True;
end;

procedure TsmxToolItemCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxToolItemCfg then
  begin
    IsChecked := TsmxToolItemCfg(Source).IsChecked;
    ToolItemStyle := TsmxToolItemCfg(Source).ToolItemStyle;
  end;
end;

procedure TsmxToolItemCfg.Clear;
begin
  inherited Clear;
  IsChecked := False;
  ToolItemStyle := tisButton;
end;

procedure TsmxToolItemCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  IsChecked := Node.Attributes['IsChecked']; //SysUtils.StrToBool(Node.Attributes['IsChecked']);
  ToolItemStyle := TsmxToolItemStyle(TypInfo.GetEnumValue(TypeInfo(TsmxToolItemStyle), Node.Attributes['ToolItemStyle']));
end;

procedure TsmxToolItemCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['IsChecked'] := SysUtils.BoolToStr(IsChecked, True);
  Node.Attributes['ToolItemStyle'] := TypInfo.GetEnumName(TypeInfo(TsmxToolItemStyle), Integer(ToolItemStyle));
end;

procedure TsmxToolItemCfg.SetIsChecked(Value: Boolean);
begin
  FIsChecked := Value;
end;

procedure TsmxToolItemCfg.SetToolItemStyle(Value: TsmxToolItemStyle);
begin
  FToolItemStyle := Value;
end;

{ TsmxHVisibleUnit }

{constructor TsmxHVisibleUnit.Create(AHKit: TsmxHKit);
begin
  inherited Create(AHKit);
  FCfgID := 0;
  FItemAlign := alNone;
  FItemEnabled := False;
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
end;}

{ TsmxHVisibleUnits }

{function TsmxHVisibleUnits.GetRoot: TsmxHVisibleUnit;
begin
  Result := TsmxHVisibleUnit(inherited Root);
end;}

{ TsmxMainMenuCfg }

{destructor TsmxMainMenuCfg.Destroy;
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
      ItemEnabled := ANode.Attributes['Enabled'];
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
      Attributes['Enabled'] := BoolToStr(AUnit.ItemEnabled, True);
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
end;}

{ TsmxToolBoardCfg }

constructor TsmxToolBoardCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'ToolItems';
  SlaveCells.ItemNodeName := 'ToolItem';
  SlaveCells.IsWriteEmpty := True;
end;

procedure TsmxToolBoardCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxToolBoardCfg then
  begin
    IsFlat := TsmxToolBoardCfg(Source).IsFlat;
    IsShowCaptions := TsmxToolBoardCfg(Source).IsShowCaptions;
  end;
end;

procedure TsmxToolBoardCfg.Clear;
begin
  inherited Clear;
  IsFlat := False;
  IsShowCaptions := False;
end;

procedure TsmxToolBoardCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  IsFlat := Node.Attributes['IsFlat']; //SysUtils.StrToBool(Node.Attributes['IsFlat']);
  IsShowCaptions := Node.Attributes['IsShowCaptions']; //SysUtils.StrToBool(Node.Attributes['IsShowCaptions']);
end;

procedure TsmxToolBoardCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['IsFlat'] := SysUtils.BoolToStr(IsFlat, True);
  Node.Attributes['IsShowCaptions'] := SysUtils.BoolToStr(IsShowCaptions, True);
end;

procedure TsmxToolBoardCfg.SetIsFlat(Value: Boolean);
begin
  FIsFlat := Value;
end;

procedure TsmxToolBoardCfg.SetIsShowCaptions(Value: Boolean);
begin
  FIsShowCaptions := Value;
end;

{ TsmxControlBoardCfg }

constructor TsmxControlBoardCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'ToolBoards';
  SlaveCells.ItemNodeName := 'ToolBoard';
  SlaveCells.IsWriteEmpty := True;
end;

{procedure TsmxControlBoardCfg.Clear;
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
          ItemEnabled := n.ChildNodes[i].Attributes['Enabled'];
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
      Attributes['Enabled'] := BoolToStr(BarUnits[i].ItemEnabled, True);
      Attributes['Height'] := BarUnits[i].ItemHeight;
      Attributes['Left'] := BarUnits[i].ItemLeft;
      Attributes['Top'] := BarUnits[i].ItemTop;
      Attributes['Visible'] := BoolToStr(BarUnits[i].ItemVisible, True);
      Attributes['Width'] := BarUnits[i].ItemWidth;
    end;
end;}

{ TsmxStatusItemCfg }

procedure TsmxStatusItemCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStatusItemCfg then
  begin
    DrawPanelAlgCfg := TsmxStatusItemCfg(Source).DrawPanelAlgCfg;
    StatusItemAlignment := TsmxStatusItemCfg(Source).StatusItemAlignment;
    StatusItemStyle := TsmxStatusItemCfg(Source).StatusItemStyle;
  end;
end;

procedure TsmxStatusItemCfg.Clear;
begin
  inherited Clear;
  DrawPanelAlgCfg := 0;
  StatusItemAlignment := taLeftJustify;
  StatusItemStyle := sisText;
end;

procedure TsmxStatusItemCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  DrawPanelAlgCfg := Node.Attributes['DrawPanelAlgCfg'];
  StatusItemAlignment := TAlignment(TypInfo.GetEnumValue(TypeInfo(TAlignment), Node.Attributes['StatusItemAlignment']));
  StatusItemStyle := TsmxStatusItemStyle(TypInfo.GetEnumValue(TypeInfo(TsmxStatusItemStyle), Node.Attributes['StatusItemStyle']));
end;

procedure TsmxStatusItemCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['DrawPanelAlgCfg'] := DrawPanelAlgCfg;
  Node.Attributes['StatusItemAlignment'] := TypInfo.GetEnumName(TypeInfo(TAlignment), Integer(StatusItemAlignment));
  Node.Attributes['StatusItemStyle'] := TypInfo.GetEnumName(TypeInfo(TsmxStatusItemStyle), Integer(StatusItemStyle));
end;

procedure TsmxStatusItemCfg.SetDrawPanelAlgCfg(Value: Integer);
begin
  FDrawPanelAlgCfg := Value;
end;

procedure TsmxStatusItemCfg.SetStatusItemAlignment(Value: TAlignment);
begin
  FStatusItemAlignment := Value;
end;

procedure TsmxStatusItemCfg.SetStatusItemStyle(Value: TsmxStatusItemStyle);
begin
  FStatusItemStyle := Value;
end;

{ TsmxStatusBoardCfg }

constructor TsmxStatusBoardCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SlaveCells.KitNodeName := 'StatusItems';
  SlaveCells.ItemNodeName := 'StatusItem';
  SlaveCells.IsWriteEmpty := True;
end;

{ TsmxFormCfg }

constructor TsmxFormCfg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IsSetAlgorithmEvents := True;
end;

procedure TsmxFormCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxFormCfg then
  begin
    AlgorithmListCfgID := TsmxFormCfg(Source).AlgorithmListCfgID;
    CloseAlgCfgID := TsmxFormCfg(Source).CloseAlgCfgID;
    FormBorder := TsmxFormCfg(Source).FormBorder;
    FormPosition := TsmxFormCfg(Source).FormPosition;
    //IsMainForm := TsmxFormCfg(Source).IsMainForm;
    IsMaximize := TsmxFormCfg(Source).IsMaximize;
    PopupListCfgID := TsmxFormCfg(Source).PopupListCfgID;
    RequestListCfgID := TsmxFormCfg(Source).RequestListCfgID;
    ShowAlgCfgID := TsmxFormCfg(Source).ShowAlgCfgID;
  end;
end;

procedure TsmxFormCfg.Clear;
begin
  inherited Clear;
  AlgorithmListCfgID := 0;
  CloseAlgCfgID := 0;
  FormBorder := fbNone;
  FormPosition := fpDesigned;
  //IsMainForm := False;
  IsMaximize := False;
  PopupListCfgID := 0;
  RequestListCfgID := 0;
  ShowAlgCfgID := 0;
end;

procedure TsmxFormCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  AlgorithmListCfgID := Node.Attributes['AlgorithmListCfgID'];
  FormBorder := TsmxFormBorder(TypInfo.GetEnumValue(TypeInfo(TsmxFormBorder), Node.Attributes['FormBorder'])); // Node.Attributes['FormBorder'];
  FormPosition := TsmxFormPosition(TypInfo.GetEnumValue(TypeInfo(TsmxFormPosition), Node.Attributes['FormPosition'])); //Node.Attributes['FormPosition'];
  //IsMainForm := SysUtils.StrToBool(Node.Attributes['IsMainForm']);
  IsMaximize := Node.Attributes['IsMaximize']; //SysUtils.StrToBool(Node.Attributes['IsMaximize']);
  PopupListCfgID := Node.Attributes['PopupListCfgID'];
  RequestListCfgID := Node.Attributes['RequestListCfgID'];

  CloseAlgCfgID := Node.Attributes['CloseAlgCfgID'];
  ShowAlgCfgID := Node.Attributes['ShowAlgCfgID'];
end;

procedure TsmxFormCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['AlgorithmListCfgID'] := AlgorithmListCfgID;
  Node.Attributes['FormBorder'] := TypInfo.GetEnumName(TypeInfo(TsmxFormBorder), Integer(FormBorder));
  Node.Attributes['FormPosition'] := TypInfo.GetEnumName(TypeInfo(TsmxFormPosition), Integer(FormPosition));
  //Node.Attributes['IsMainForm'] := SysUtils.BoolToStr(IsMainForm, True);
  Node.Attributes['IsMaximize'] := SysUtils.BoolToStr(IsMaximize, True);
  Node.Attributes['PopupListCfgID'] := PopupListCfgID;
  Node.Attributes['RequestListCfgID'] := RequestListCfgID;

  Node.Attributes['CloseAlgCfgID'] := CloseAlgCfgID;
  Node.Attributes['ShowAlgCfgID'] := ShowAlgCfgID;
end;

procedure TsmxFormCfg.SetAlgorithmListCfgID(Value: Integer);
begin
  FAlgorithmListCfgID := Value;
end;

procedure TsmxFormCfg.SetCloseAlgCfgID(Value: Integer);
begin
  FCloseAlgCfgID := Value;
end;

procedure TsmxFormCfg.SetFormBorder(Value: TsmxFormBorder);
begin
  FFormBorder := Value;
end;

procedure TsmxFormCfg.SetFormPosition(Value: TsmxFormPosition);
begin
  FFormPosition := Value;
end;

{procedure TsmxFormCfg.SetIsMainForm(Value: Boolean);
begin
  FIsMainForm := Value;
end;}

procedure TsmxFormCfg.SetIsMaximize(Value: Boolean);
begin
  FIsMaximize := Value;
end;

procedure TsmxFormCfg.SetPopupListCfgID(Value: Integer);
begin
  FPopupListCfgID := Value;
end;

procedure TsmxFormCfg.SetRequestListCfgID(Value: Integer);
begin
  FRequestListCfgID := Value;
end;

procedure TsmxFormCfg.SetShowAlgCfgID(Value: Integer);
begin
  FShowAlgCfgID := Value;
end;

{ TsmxStateKitItem }

{function TsmxStateKitItem.Add: TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Add);
end;

procedure TsmxStateKitItem.Assign(Source: TsmxHKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxStateKitItem then
  begin
    CfgID := TsmxStateKitItem(Source).CfgID;
    CurrentIntfID := TsmxStateKitItem(Source).CurrentIntfID;
    ItemEnabled := TsmxStateKitItem(Source).ItemEnabled;
    ItemVisible := TsmxStateKitItem(Source).ItemVisible;
  end;
end;

function TsmxStateKitItem.FindByCfgID(CfgID: Integer; AmongAll: Boolean = False): TsmxStateKitItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].CfgID = CfgID then
      Result := Items[i] else
    if AmongAll then
      Result := Items[i].FindByCfgID(CfgID, AmongAll);
    if Assigned(Result) then
      Break;
  end;
end;

function TsmxStateKitItem.GetHKit: TsmxStateKit;
begin
  Result := TsmxStateKit(inherited HKit);
end;

function TsmxStateKitItem.GetItem(Index: Integer): TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Items[Index]);
end;

procedure TsmxStateKitItem.SetItem(Index: Integer; Value: TsmxStateKitItem);
begin
  inherited Items[Index] := Value;
end;

function TsmxStateKitItem.GetParent: TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Parent);
end;

procedure TsmxStateKitItem.SetParent(Value: TsmxStateKitItem);
begin
  inherited Parent := Value;
end;

procedure TsmxStateKitItem.SetCurrentIntfID(Value: Integer);
begin
  if FCurrentIntfID <> Value then
  begin
    FPriorIntfID := FCurrentIntfID;
    FCurrentIntfID := Value;
  end;
end;

procedure TsmxStateKitItem.SetItemEnabled(Value: Boolean);
begin
  if FItemEnabled <> Value then
  begin
    FItemEnabled := Value;
    SwitchIntfID;
  end;
end;

procedure TsmxStateKitItem.SetItemVisible(Value: Boolean);
begin
  if FItemVisible <> Value then
  begin
    FItemVisible := Value;
    SwitchIntfID;
  end;
end;

procedure TsmxStateKitItem.SwitchIntfID;
var
  StateUnit: TsmxStateKitItem;
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
end;}

{ TsmxStateKit }

{procedure TsmxStateKit.Assign(Source: TsmxHKit);
begin
  inherited Assign(Source);
  if Source is TsmxStateKit then
    IntfID := TsmxStateKit(Source).IntfID;
end;

function TsmxStateKit.GetRoot: TsmxStateKitItem;
begin
  Result := TsmxStateKitItem(inherited Root);
end;

procedure TsmxStateKit.SetRoot(Value: TsmxStateKitItem);
begin
  inherited Root := Value;
end;}

{ TsmxCellState }

{destructor TsmxCellState.Destroy;
begin
  if Assigned(FStateKit) then
    FStateKit.Free;
  inherited Destroy;
end;

procedure TsmxCellState.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxCellState then
  begin
    StateID := TsmxCellState(Source).StateID;
    StateKit := TsmxCellState(Source).StateKit;
  end else
    inherited Assign(Source);
end;

function TsmxCellState.GetStateKit: TsmxStateKit;
begin
  if not Assigned(FStateKit) then
    FStateKit := TsmxStateKit.Create(TsmxStateKitItem);
  Result := FStateKit;
end;

procedure TsmxCellState.SetStateKit(Value: TsmxStateKit);
begin
  StateKit.Assign(Value);
end;}

{ TsmxCellStates }

{function TsmxCellStates.Add: TsmxCellState;
begin
  Result := TsmxCellState(inherited Add);
end;

function TsmxCellStates.FindByStateID(StateID: Integer): TsmxCellState;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].StateID = StateID then
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
end;}

{ TsmxSimpleStateCfg }

{destructor TsmxSimpleStateCfg.Destroy;
begin
  if Assigned(FCellStates) then
    FCellStates.Free;
  inherited Destroy;
end;

procedure TsmxSimpleStateCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxSimpleStateCfg then
    CellStates := TsmxSimpleStateCfg(Source).CellStates;
end;

procedure TsmxSimpleStateCfg.Clear;
begin
  if Assigned(FCellStates) then
    FCellStates.Clear;
end;

function TsmxSimpleStateCfg.GetCellStates: TsmxCellStates;
begin
  if not Assigned(FCellStates) then
    FCellStates := TsmxCellStates.Create(TsmxCellState);
  Result := FCellStates;
end;

procedure TsmxSimpleStateCfg.SetCellStates(Value: TsmxCellStates);
begin
  CellStates.Assign(Value);
end;

procedure TsmxSimpleStateCfg.ReadIntf(const Node: IXMLNode; ID: Integer);

  procedure AddItems(const ANode: IXMLNode; AItem: TsmxStateKitItem);
  var
    i: Integer;
    Item: TsmxStateKitItem;
  begin
    Item := AItem.FindByCfgID(ANode.Attributes['StateID']);
    if not Assigned(Item) then
      Item := AItem.Add;
    Item.CurrentIntfID := ID;
    Item.FCfgID := ANode.Attributes['CfgID'];
    Item.FItemEnabled := SysUtils.StrToBool(ANode.Attributes['Enabled']);
    Item.FItemVisible := SysUtils.StrToBool(ANode.Attributes['Visible']);
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'Cell' then
        AddItems(ANode.ChildNodes[i], Item);
  end;

var
  n, n2: IXMLNode;
  i, j: Integer;
  State: TsmxCellState;
begin
  inherited ReadIntf(Node, ID);
  n := Node.ChildNodes.FindNode('States');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'State' then
      begin
        State := CellStates.FindByStateID(n.ChildNodes[i].Attributes['StateID']);
        if not Assigned(State) then
        begin
          State := CellStates.Add;
          State.StateID := n.ChildNodes[i].Attributes['StateID'];
          State.StateKit.IntfID := IntfID;
        end;
        n2 := n.ChildNodes[i].ChildNodes.FindNode('Cells');
        if Assigned(n2) and (n2.ChildNodes.Count > 0) then
          for j := 0 to n2.ChildNodes.Count - 1 do
            if n2.ChildNodes[j].NodeName = 'Cell' then
              AddItems(n2.ChildNodes[j], State.StateKit.Root);
      end;
  end;
end;

procedure TsmxSimpleStateCfg.WriteIntf(const Node: IXMLNode; ID: Integer);

  procedure AddNodes(const ANode: IXMLNode; AItem: TsmxStateKitItem);
  var
    n: IXMLNode;
    i: Integer;
  begin
    if AItem.CurrentIntfID = ID then
    begin
      n := ANode.AddChild('Cell');
      n.Attributes['CfgID'] := AItem.CfgID;
      n.Attributes['Enabled'] := SysUtils.BoolToStr(AItem.ItemEnabled, True);
      n.Attributes['Visible'] := SysUtils.BoolToStr(AItem.ItemVisible, True);
    end;
    for i := 0 to AItem.Count - 1 do
      AddNodes(n, AItem[i]);
  end;

var
  n, n2: IXMLNode;
  i, j: Integer;
begin
  inherited WriteIntf(Node, ID);
  n := Node.AddChild('States');
  for i := 0 to CellStates.Count - 1 do
  begin
    n2 := n.AddChild('State');
    n2.Attributes['StateID'] := CellStates[i].StateID;
    for j := 0 to CellStates[i].StateKit.Root.Count - 1 do
      AddNodes(n2.AddChild('Cells'), CellStates[i].StateKit.Root[j]);
  end;
end;}

{ TsmxStateFormCfg }

{procedure TsmxStateFormCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateFormCfg then
  begin
    StateID := TsmxStateFormCfg(Source).StateID;
    StateReqCfgID := TsmxStateFormCfg(Source).StateReqCfgID;
  end;
end;

procedure TsmxStateFormCfg.Clear;
begin
  inherited Clear;
  StateID := 0;
  StateReqCfgID := 0;
end;

procedure TsmxStateFormCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  StateID := Node.Attributes['StateID'];
  StateReqCfgID := Node.Attributes['StateReqCfgID'];
end;

procedure TsmxStateFormCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['StateID'] := StateID;
  Node.Attributes['StateReqCfgID'] := StateReqCfgID;
end;

procedure TsmxStateFormCfg.SetStateID(Value: Integer);
begin
  FStateID := Value;
end;

procedure TsmxStateFormCfg.SetStateReqCfgID(Value: Integer);
begin
  FStateReqCfgID := Value;
end;}

{ TsmxStandardFormCfg }

{destructor TsmxStandardFormCfg.Destroy;
begin
  if Assigned(FControlBoard) then
    FControlBoard.Free;
  if Assigned(FMainMenu) then
    FMainMenu.Free;
  if Assigned(FStatusBoard) then
    FStatusBoard.Free;
  inherited Destroy;
end;

procedure TsmxStandardFormCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStandardFormCfg then
  begin
    ControlBoard := TsmxStandardFormCfg(Source).ControlBoard;
    MainMenu := TsmxStandardFormCfg(Source).MainMenu;
    StatusBoard := TsmxStandardFormCfg(Source).StatusBoard;
  end;
end;

procedure TsmxStandardFormCfg.Clear;
begin
  inherited Clear;
  if Assigned(FControlBoard) then
    FControlBoard.Clear;
  if Assigned(FMainMenu) then
    FMainMenu.Clear;
  if Assigned(FStatusBoard) then
    FStatusBoard.Clear;
end;

function TsmxStandardFormCfg.GetControlBoard: TsmxControlKitItem;
begin
  if not Assigned(FControlBoard) then
    FControlBoard := TsmxControlKitItem.Create(nil);
  Result := FControlBoard;
end;

procedure TsmxStandardFormCfg.SetControlBoard(Value: TsmxControlKitItem);
begin
  ControlBoard.Assign(Value);
end;

function TsmxStandardFormCfg.GetMainMenu: TsmxControlKitItem;
begin
  if not Assigned(FMainMenu) then
    FMainMenu := TsmxControlKitItem.Create(nil);
  Result := FMainMenu;
end;

procedure TsmxStandardFormCfg.SetMainMenu(Value: TsmxControlKitItem);
begin
  MainMenu.Assign(Value);
end;

function TsmxStandardFormCfg.GetStatusBoard: TsmxControlKitItem;
begin
  if not Assigned(FStatusBoard) then
    FStatusBoard := TsmxControlKitItem.Create(nil);
  Result := FStatusBoard;
end;

procedure TsmxStandardFormCfg.SetStatusBoard(Value: TsmxControlKitItem);
begin
  StatusBoard.Assign(Value);
end;}

{procedure TsmxStandardFormCfg.ReadCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited ReadCell(Node);
  n := Node.ChildNodes.FindNode('ControlBoard');
  if Assigned(n) then
    ControlBoard.Read(n);}
    {with ControlBoard do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}

  {n := Node.ChildNodes.FindNode('MainMenu');
  if Assigned(n) then
    MainMenu.Read(n);}
    {with MainMenu do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}

  {n := Node.ChildNodes.FindNode('StatusBoard');
  if Assigned(n) then
    StatusBoard.Read(n);}
    {with StatusBoard do
    begin
      ItemActive := SysUtils.StrToBool(n.Attributes['Active']);
      ItemAlign := n.Attributes['Align'];
      ItemAnchors := TAnchors(Byte(n.Attributes['Anchors']));
      ItemCursor := n.Attributes['Cursor'];
      ItemEnabled := SysUtils.StrToBool(n.Attributes['Enabled']);
      ItemHeight := n.Attributes['Height'];
      ItemLeft := n.Attributes['Left'];
      ItemTop := n.Attributes['Top'];
      ItemVisible := SysUtils.StrToBool(n.Attributes['Visible']);
      ItemWidth := n.Attributes['Width'];
      PopupMenuCfgID := n.Attributes['PopupMenuCfgID'];
    end;}
{end;}

{procedure TsmxStandardFormCfg.WriteCell(const Node: IXMLNode);
var
  n: IXMLNode;
begin
  inherited WriteCell(Node);
  n := Node.AddChild('FilterDesk');
  ControlBoard.Write(n);}
  {with ControlBoard do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}

  {n := Node.AddChild('MainMenu');
  MainMenu.Write(n);}
  {with MainMenu do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}

  {n := Node.AddChild('StatusBoard');
  StatusBoard.Write(n);}
  {with StatusBoard do
  begin
    n.Attributes['Active'] := SysUtils.BoolToStr(ItemActive, True);
    n.Attributes['Align'] := ItemAlign;
    n.Attributes['Anchors'] := Byte(ItemAnchors);
    n.Attributes['Cursor'] := ItemCursor;
    n.Attributes['Enabled'] := SysUtils.BoolToStr(ItemEnabled, True);
    n.Attributes['Height'] := ItemHeight;
    n.Attributes['Left'] := ItemLeft;
    n.Attributes['Top'] := ItemTop;
    n.Attributes['Visible'] := SysUtils.BoolToStr(ItemVisible, True);
    n.Attributes['Width'] := ItemWidth;
    n.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
  end;}
{end;}

initialization
  Classes.RegisterClasses([TsmxAlgorithmCfg, TsmxAlgorithmListCfg,
    TsmxRequestCfg, TsmxRequestListCfg, TsmxColumnCfg, TsmxGridCfg,
    TsmxFilterCfg, TsmxFilterDeskCfg, TsmxSectionCfg, TsmxPageCfg,
    TsmxPageManagerCfg, TsmxMenuItemCfg, TsmxMainMenuCfg, TsmxPopupMenuCfg,
    TsmxPopupListCfg, TsmxToolItemCfg, TsmxToolBoardCfg, TsmxControlBoardCfg,
    TsmxStatusItemCfg, TsmxStatusBoardCfg, TsmxFormCfg]);

end.
