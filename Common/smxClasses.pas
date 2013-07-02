{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Program classes            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov �leksandr          }
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
    function GetRootNode: IXMLNode;
    function GetXMLDoc: IXMLDocument;
  protected
    function GetXMLText: String; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;
    procedure SetXMLText(const Value: String); virtual;

    property RootNode: IXMLNode read GetRootNode;
    property XMLDoc: IXMLDocument read GetXMLDoc;
  public
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

  { TsmxCellfg }

  TsmxCellCfg = class(TsmxBaseCfg)
  private
    FImageListName: String;
    FInitializeAlgCfgID: Integer;
  protected
    procedure ReadCell(const Node: IXMLNode); virtual;
    procedure SetImageListName(const Value: String); virtual;
    procedure SetInitializeAlgCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Read; override;
    procedure Write; override;

    property ImageListName: String read FImageListName write SetImageListName;
  published
    property InitializeAlgCfgID: Integer read FInitializeAlgCfgID write SetInitializeAlgCfgID;
  end;

  { TsmxBaseCell }

  TsmxBaseCell = class(TsmxComponent)
  private
    FCellList: TList;
    FCellParent: TsmxBaseCell;
    FCfg: TsmxBaseCfg;
    FCfgID: Integer;
    FDatabaseManagerIntf: IsmxDatabaseManager;
    FFormManagerIntf: IsmxFormManager;
    FImageList: TCustomImageList;
    FImageListManagerIntf: IsmxImageListManager;
    FImageListName: String;
    FIsRecieveCfg: Boolean;
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
    function GetCfg: TsmxBaseCfg;
    function GetDatabaseManager: IsmxDatabaseManager;
    function GetFormManager: IsmxFormManager;
    //function GetImageList: TCustomImageList;
    function GetImageListManager: IsmxImageListManager;
    function GetLibraryManager: IsmxLibraryManager;
    function GetStorageManager: IsmxStorageManager;
  protected
    procedure DoInitialize; virtual;
    function FindChildByInternalObject(Obj: TObject): TsmxBaseCell;
    function GetCfgClass: TsmxBaseCfgClass; virtual;
    function GetInternalObject: TObject; virtual;
    procedure InternalInitialize; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetCellProps; virtual;
    procedure SetCellFeedback; virtual;
    procedure SetCellParent(Value: TsmxBaseCell); virtual;
    procedure SetCfgID(Value: Integer); virtual;
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager); virtual;
    procedure SetFormManager(const Value: IsmxFormManager); virtual;
    procedure SetImageList(Value: TCustomImageList); virtual;
    procedure SetImageListName(const Value: String);
    procedure SetImageListManager(const Value: IsmxImageListManager); virtual;
    procedure SetIsRecieveCfg(Value: Boolean); virtual;
    procedure SetLibraryManager(const Value: IsmxLibraryManager); virtual;
    procedure SetSelectRequest(Value: TsmxCustomRequest); virtual;
    procedure SetStorageManager(const Value: IsmxStorageManager); virtual;

    property CellList: TList read GetCellList;
    property Cfg: TsmxBaseCfg read GetCfg;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; virtual;
    function FindChildByCfgID(CfgID: Integer): TsmxBaseCell;
    procedure Initialize;

    property CellCount: Integer read GetCellCount;
    property CellIndex: Integer read GetCellIndex;
    property CellParent: TsmxBaseCell read FCellParent write SetCellParent;
    property CellRoot: TsmxBaseCell read GetCellRoot;
    property Cells[Index: Integer]: TsmxBaseCell read GetCell;
    property CfgID: Integer read FCfgID write SetCfgID;
    property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;
    property FormManager: IsmxFormManager read GetFormManager write SetFormManager;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ImageListManager: IsmxImageListManager read GetImageListManager write SetImageListManager;
    property ImageListName: String read FImageListName write SetImageListName;
    property IsRecieveCfg: Boolean read FIsRecieveCfg write SetIsRecieveCfg;
    property LibraryManager: IsmxLibraryManager read GetLibraryManager write SetLibraryManager;
    property SelectRequest: TsmxCustomRequest read FSelectRequest write SetSelectRequest;
    property StorageManager: IsmxStorageManager read GetStorageManager write SetStorageManager;

    property OnInitialize: TsmxComponentEvent read FOnInitialize write FOnInitialize;
  end;

  TsmxBaseCellClass = class of TsmxBaseCell;

  { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxCellCfg)
  private
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetCellClass(Value: TsmxBaseCellClass); virtual;
    procedure SetCellClassName(const Value: String); virtual;
    procedure SetCfgClass(Value: TsmxBaseCfgClass); virtual;
    procedure SetCfgClassName(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property CellClass: TsmxBaseCellClass read FCellClass write SetCellClass;
    property CellClassName: String read FCellClassName write SetCellClassName;
    property CfgClass: TsmxBaseCfgClass read FCfgClass write SetCfgClass;
    property CfgClassName: String read FCfgClassName write SetCfgClassName;
  end;

  { TsmxSimpleKitItem }

  TsmxSimpleKit = class;

  TsmxSimpleKitItem = class(TsmxKitItem)
  private
    function GetKit: TsmxSimpleKit;
    procedure SetKit(Value: TsmxSimpleKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; virtual;
    procedure Read(const Node: IXMLNode); virtual;
    procedure Write(const Node: IXMLNode); virtual;

    property Kit: TsmxSimpleKit read GetKit write SetKit;
  end;

  TsmxSimpleKitItemClass = class of TsmxSimpleKitItem;

  { TsmxSimpleKit }

  TsmxSimpleKit = class(TsmxKit)
  private
    FChangeMode: TsmxChangeMode;
    FIsWriteEmpty: Boolean;
    FItemNodeName: String;
    FKitNodeName: String;
    function GetItem(Index: Integer): TsmxSimpleKitItem;
    procedure SetItem(Index: Integer; Value: TsmxSimpleKitItem);
  public
    constructor Create(AItemClass: TsmxKitItemClass); overload; override;
    constructor Create; reintroduce; overload; virtual;
    function Add: TsmxSimpleKitItem;
    procedure Read(const Node: IXMLNode); virtual;
    procedure Write(const Node: IXMLNode); virtual;

    property ChangeMode: TsmxChangeMode read FChangeMode write FChangeMode;
    property IsWriteEmpty: Boolean read FIsWriteEmpty write FIsWriteEmpty;
    property ItemNodeName: String read FItemNodeName write FItemNodeName;
    property Items[Index: Integer]: TsmxSimpleKitItem read GetItem write SetItem; default;
    property KitNodeName: String read FKitNodeName write FKitNodeName;
  end;

  TsmxSimpleKitClass = class of TsmxSimpleKit;

  { TsmxOwnerKitItem }

  TsmxOwnerKit = class;

  TsmxOwnerKitItem = class(TsmxSimpleKitItem)
  private
    FCfgID: Integer;
    function GetKit: TsmxOwnerKit;
    procedure SetKit(Value: TsmxOwnerKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property CfgID: Integer read FCfgID write FCfgID;
    property Kit: TsmxOwnerKit read GetKit write SetKit;
  end;

  { TsmxOwnerKit }

  TsmxOwnerKit = class(TsmxSimpleKit)
  private
    function GetItem(Index: Integer): TsmxOwnerKitItem;
    procedure SetItem(Index: Integer; Value: TsmxOwnerKitItem);
  public
    constructor Create; override;
    function Add: TsmxOwnerKitItem;

    property Items[Index: Integer]: TsmxOwnerKitItem read GetItem write SetItem; default;
  end;

  { TsmxOwnerCellCfg }

  TsmxOwnerCellCfg = class(TsmxCellCfg)
  private
    FSlaveCells: TsmxSimpleKit;
    FSlaveName: String;
    function GetSlaveCells: TsmxOwnerKit;
    procedure SetSlaveCells(Value: TsmxOwnerKit);
  protected
    function GetSlaveCellsClass: TsmxSimpleKitClass; virtual;
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetSlaveName(const Value: String); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    //procedure Read; override;
    //procedure Write; override;

    property SlaveCells: TsmxOwnerKit read GetSlaveCells write SetSlaveCells;
  published
    property SlaveName: String read FSlaveName write SetSlaveName;
  end;

  { TsmxOwnerCell }

  TsmxOwnerCellClass = class of TsmxOwnerCell;

  TsmxOwnerCell = class(TsmxBaseCell)
  private
    FCellOwner: TsmxOwnerCell;
    FIsAltSlaveClass: Boolean;
    FIsOwnerIsParent: Boolean;
    FSlaveList: TList;
    FSlaveName: String;
    function GetSlave(Index: Integer): TsmxOwnerCell;
    function GetSlaveCount: Integer;
    function GetSlaveIndex: Integer;
    function GetSlaveList: TList;
    procedure SetCellOwner(Value: TsmxOwnerCell);
    procedure SetSlave(Index: Integer; Value: TsmxOwnerCell);
  protected
    function FindSlaveByInternalObject(Obj: TObject): TsmxOwnerCell;
    function GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass; virtual;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; virtual;
    procedure InternalInitialize; override;
    procedure ResetCellProps; override;
    procedure SetIsAltSlaveClass(Value: Boolean); virtual;
    procedure SetIsOwnerIsParent(Value: Boolean); virtual;
    procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); virtual;
    procedure SetSlaveIndex(Value: Integer); virtual;
    procedure SetSlaveName(const Value: String); virtual;

    property SlaveList: TList read GetSlaveList;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddSlave: TsmxOwnerCell;
    procedure ClearSlaves;
    procedure DeleteSlave(Index: Integer);
    function FindSlaveByCfgID(CfgID: Integer): TsmxOwnerCell;
    function FindSlaveByName(const Name: String): TsmxOwnerCell;

    property CellOwner: TsmxOwnerCell read FCellOwner write SetCellOwner;
    property IsAltSlaveClass: Boolean read FIsAltSlaveClass write SetIsAltSlaveClass;
    property IsOwnerIsParent: Boolean read FIsOwnerIsParent write SetIsOwnerIsParent;
    property SlaveClass: TsmxOwnerCellClass read GetSlaveClass;
    property SlaveCount: Integer read GetSlaveCount;
    property SlaveIndex: Integer read GetSlaveIndex write SetSlaveIndex;
    property SlaveName: String read FSlaveName write SetSlaveName;
    property Slaves[Index: Integer]: TsmxOwnerCell read GetSlave write SetSlave; default;
  end;

  { TsmxControlKitItem }

  TsmxControlKit = class;

  TsmxControlKitItem = class(TsmxOwnerKitItem)
  private
    FItemActive: Boolean;
    FItemAlign: TAlign;
    FItemAnchors: TAnchors;
    FItemCursor: TCursor;
    FItemEnabled: Boolean;
    FItemHeight: Integer;
    FItemLeft: Integer;
    FItemTop: Integer;
    FItemVisible: Boolean;
    FItemWidth: Integer;
    FPopupMenuCfgID: Integer;
    function GetKit: TsmxControlKit;
    procedure SetKit(Value: TsmxControlKit);
  public
    procedure Assign(Source: TsmxKitItem); override;
    procedure Clear; override;
    procedure Read(const Node: IXMLNode); override;
    procedure Write(const Node: IXMLNode); override;

    property ItemActive: Boolean read FItemActive write FItemActive;
    property ItemAlign: TAlign read FItemAlign write FItemAlign;
    property ItemAnchors: TAnchors read FItemAnchors write FItemAnchors;
    property ItemCursor: TCursor read FItemCursor write FItemCursor;
    property ItemEnabled: Boolean read FItemEnabled write FItemEnabled;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property ItemLeft: Integer read FItemLeft write FItemLeft;
    property ItemTop: Integer read FItemTop write FItemTop;
    property ItemVisible: Boolean read FItemVisible write FItemVisible;
    property ItemWidth: Integer read FItemWidth write FItemWidth;
    property Kit: TsmxControlKit read GetKit write SetKit;
    property PopupMenuCfgID: Integer read FPopupMenuCfgID write FPopupMenuCfgID;
  end;

  { TsmxControlKit }

  TsmxControlKit = class(TsmxOwnerKit)
  private
    function GetItem(Index: Integer): TsmxControlKitItem;
    procedure SetItem(Index: Integer; Value: TsmxControlKitItem);
  public
    constructor Create; override;
    function Add: TsmxControlKitItem;

    property Items[Index: Integer]: TsmxControlKitItem read GetItem write SetItem; default;
  end;

  { TsmxControlCellCfg }

  TsmxControlCellCfg = class(TsmxOwnerCellCfg)
  private
    FBackupAlgCfgID: Integer;
    FCfgActive: Boolean;
    FCfgAlign: TAlign;
    FCfgAnchors: TAnchors;
    FCfgCursor: TCursor;
    FCfgEnabled: Boolean;
    FCfgHeight: Integer;
    FCfgLeft: Integer;
    FCfgTop: Integer;
    FCfgVisible: Boolean;
    FCfgWidth: Integer;
    FDoubleSnapAlgCfgID: Integer;
    FPopupMenuCfgID: Integer;
    FRestoreAlgCfgID: Integer;
    FSnapAlgCfgID: Integer;
    function GetSlaveCells: TsmxControlKit;
    procedure SetSlaveCells(Value: TsmxControlKit);
  protected
    function GetSlaveCellsClass: TsmxSimpleKitClass; override;
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetBackupAlgCfgID(Value: Integer); virtual;
    procedure SetCfgActive(Value: Boolean); virtual;
    procedure SetCfgAlign(Value: TAlign); virtual;
    procedure SetCfgAnchors(Value: TAnchors); virtual;
    procedure SetCfgCursor(Value: TCursor); virtual;
    procedure SetCfgEnabled(Value: Boolean); virtual;
    procedure SetCfgHeight(Value: Integer); virtual;
    procedure SetCfgLeft(Value: Integer); virtual;
    procedure SetCfgTop(Value: Integer); virtual;
    procedure SetCfgVisible(Value: Boolean); virtual;
    procedure SetCfgWidth(Value: Integer); virtual;
    procedure SetDoubleSnapAlgCfgID(Value: Integer); virtual;
    procedure SetPopupMenuCfgID(Value: Integer); virtual;
    procedure SetRestoreAlgCfgID(Value: Integer); virtual;
    procedure SetSnapAlgCfgID(Value: Integer); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property CfgActive: Boolean read FCfgActive write SetCfgActive;
    property CfgAlign: TAlign read FCfgAlign write SetCfgAlign;
    property CfgAnchors: TAnchors read FCfgAnchors write SetCfgAnchors;
    property CfgCursor: TCursor read FCfgCursor write SetCfgCursor;
    property CfgEnabled: Boolean read FCfgEnabled write SetCfgEnabled;
    property CfgHeight: Integer read FCfgHeight write SetCfgHeight;
    property CfgLeft: Integer read FCfgLeft write SetCfgLeft;
    property CfgTop: Integer read FCfgTop write SetCfgTop;
    property CfgVisible: Boolean read FCfgVisible write SetCfgVisible;
    property CfgWidth: Integer read FCfgWidth write SetCfgWidth;
    property DoubleSnapAlgCfgID: Integer read FDoubleSnapAlgCfgID write SetDoubleSnapAlgCfgID;
    property PopupMenuCfgID: Integer read FPopupMenuCfgID write SetPopupMenuCfgID;
    property SlaveCells: TsmxControlKit read GetSlaveCells write SetSlaveCells;
    property SnapAlgCfgID: Integer read FSnapAlgCfgID write SetSnapAlgCfgID;
  published
    property BackupAlgCfgID: Integer read FBackupAlgCfgID write SetBackupAlgCfgID;
    property RestoreAlgCfgID: Integer read FRestoreAlgCfgID write SetRestoreAlgCfgID;
  end;

  { TsmxControlCell }

  TsmxCustomPopupMenu = class;

  TsmxControlCell = class(TsmxOwnerCell)
  private
    FOnBackup: TsmxComponentEvent;
    FOnRestore: TsmxComponentEvent;
    FOnDoubleSnap: TsmxComponentEvent;
    FOnSnap: TsmxComponentEvent;
    FPopupMenu: TsmxCustomPopupMenu;
    function GetSlave(Index: Integer): TsmxControlCell;
    procedure SetSlave(Index: Integer; Value: TsmxControlCell);
  protected
    procedure ControlDblClick(Sender: TObject);
    procedure ControlClick(Sender: TObject);
    procedure DoBackup; virtual;
    procedure DoDoubleSnap; virtual;
    procedure DoRestore; virtual;
    procedure DoSnap; virtual;
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
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalBackup; virtual;
    procedure InternalDoubleSnap; virtual;
    procedure InternalInitialize; override;
    procedure InternalRestore; virtual;
    procedure InternalSnap; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetCellProps; override;
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
    procedure SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem); override;
  public
    function AddSlave: TsmxControlCell;
    procedure Assign(Source: TPersistent); override;
    procedure Backup;
    procedure DoubleSnap;
    procedure Restore;
    procedure Snap;

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
    property IsOwnerIsParent default True;
    property PopupMenu: TsmxCustomPopupMenu read FPopupMenu write SetPopupMenu;
    property Slaves[Index: Integer]: TsmxControlCell read GetSlave write SetSlave; default;

    property OnBackup: TsmxComponentEvent read FOnBackup write FOnBackup;
    property OnDoubleSnap: TsmxComponentEvent read FOnDoubleSnap write FOnDoubleSnap;
    property OnRestore: TsmxComponentEvent read FOnRestore write FOnRestore;
    property OnSnap: TsmxComponentEvent read FOnSnap write FOnSnap;
  end;

  { TsmxActionCellCfg }

  TsmxActionCellCfg = class(TsmxControlCellCfg)
  private
    FAlgorithmCfgID: Integer;
    FCfgCaption: String;
    FCfgHint: String;
    FCfgImageIndex: Integer;
    FIsSetAlgorithmEvents: Boolean;
    FIsSetAlgorithmProps: Boolean;
  protected
    procedure ReadCell(const Node: IXMLNode); override;
    procedure SetAlgorithmCfgID(Value: Integer); virtual;
    procedure SetCfgCaption(const Value: String); virtual;
    procedure SetCfgHint(const Value: String); virtual;
    procedure SetCfgImageIndex(Value: Integer); virtual;
    procedure SetIsSetAlgorithmEvents(Value: Boolean); virtual;
    procedure SetIsSetAlgorithmProps(Value: Boolean); virtual;
    procedure WriteCell(const Node: IXMLNode); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    property CfgCaption: String read FCfgCaption write SetCfgCaption;
    property CfgHint: String read FCfgHint write SetCfgHint;
    property CfgImageIndex: Integer read FCfgImageIndex write SetCfgImageIndex default -1;
  published
    property AlgorithmCfgID: Integer read FAlgorithmCfgID write SetAlgorithmCfgID;
    property IsSetAlgorithmEvents: Boolean read FIsSetAlgorithmEvents write SetIsSetAlgorithmEvents;
    property IsSetAlgorithmProps: Boolean read FIsSetAlgorithmProps write SetIsSetAlgorithmProps;
  end;

  { TsmxActionCell }

  TsmxCustomAlgorithm = class;

  TsmxActionCell = class(TsmxControlCell)
  private
    FAlgorithm: TsmxCustomAlgorithm;
    FIsSetAlgorithmEvents: Boolean;
    FIsSetAlgorithmProps: Boolean;
  protected
    function GetCellCaption: String; virtual;
    function GetCellHint: String; virtual;
    function GetCellImageIndex: Integer; virtual;
    function GetCfgClass: TsmxBaseCfgClass; override;
    procedure InternalInitialize; override;
    procedure InternalSnap; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetCellProps; override;
    procedure SetAlgorithm(Value: TsmxCustomAlgorithm); virtual;
    procedure SetCellCaption(const Value: String); virtual;
    procedure SetCellHint(const Value: String); virtual;
    procedure SetCellImageIndex(Value: Integer); virtual;
    procedure SetIsSetAlgorithmEvents(Value: Boolean); virtual;
    procedure SetIsSetAlgorithmProps(Value: Boolean); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property Algorithm: TsmxCustomAlgorithm read FAlgorithm write SetAlgorithm;
    property CellCaption: String read GetCellCaption write SetCellCaption;
    property CellHint: String read GetCellHint write SetCellHint;
    property CellImageIndex: Integer read GetCellImageIndex write SetCellImageIndex;
    property IsSetAlgorithmEvents: Boolean read FIsSetAlgorithmEvents write SetIsSetAlgorithmEvents default True;
    property IsSetAlgorithmProps: Boolean read FIsSetAlgorithmProps write SetIsSetAlgorithmProps default True;
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
    FAlgorithmParams: TsmxAlgorithmParams;
    FActionCell: TsmxBaseCell;
    FIsManualRefreshParams: Boolean;
    FOnExecute: TsmxComponentEvent;
    FOnRefreshParams: TsmxComponentEvent;
    function GetAlgorithmParams: TsmxAlgorithmParams;
    function GetCellOwner: TsmxCustomAlgorithmList;
    procedure SetCellOwner(Value: TsmxCustomAlgorithmList);
  protected
    procedure DoExecute; virtual;
    procedure DoRefreshParams; virtual;
    function GetAlgorithmCaption: String; virtual;
    function GetAlgorithmEnabled: Boolean; virtual;
    function GetAlgorithmHint: String; virtual;
    function GetAlgorithmHotKey: Integer; virtual;
    function GetAlgorithmImageIndex: Integer; virtual;
    function GetAlgorithmVisible: Boolean; virtual;
    procedure InternalExecute; virtual;
    procedure InternalRefreshParams; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActionCell(Value: TsmxBaseCell); virtual;
    procedure SetAlgorithmCaption(const Value: String); virtual;
    procedure SetAlgorithmEnabled(Value: Boolean); virtual;
    procedure SetAlgorithmHint(const Value: String); virtual;
    procedure SetAlgorithmHotKey(Value: Integer); virtual;
    procedure SetAlgorithmImageIndex(Value: Integer); virtual;
    procedure SetAlgorithmParams(Value: TsmxAlgorithmParams); virtual;
    procedure SetAlgorithmVisible(Value: Boolean); virtual;
    procedure SetIsManualRefreshParams(Value: Boolean); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute;
    procedure RefreshParams;

    property ActionCell: TsmxBaseCell read FActionCell write SetActionCell;
    property AlgorithmCaption: String read GetAlgorithmCaption write SetAlgorithmCaption;
    property AlgorithmEnabled: Boolean read GetAlgorithmEnabled write SetAlgorithmEnabled;
    property AlgorithmHint: String read GetAlgorithmHint write SetAlgorithmHint;
    property AlgorithmHotKey: Integer read GetAlgorithmHotKey write SetAlgorithmHotKey;
    property AlgorithmImageIndex: Integer read GetAlgorithmImageIndex write SetAlgorithmImageIndex;
    property AlgorithmParams: TsmxAlgorithmParams read GetAlgorithmParams write SetAlgorithmParams;
    property AlgorithmVisible: Boolean read GetAlgorithmVisible write SetAlgorithmVisible;
    property CellOwner: TsmxCustomAlgorithmList read GetCellOwner write SetCellOwner;
    property IsManualRefreshParams: Boolean read FIsManualRefreshParams write SetIsManualRefreshParams;

    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnRefreshParams: TsmxComponentEvent read FOnRefreshParams write FOnRefreshParams;
  end;

  { TsmxCustomAlgorithmList }

  TsmxCustomAlgorithmList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomAlgorithm;
    procedure SetSlave(Index: Integer; Value: TsmxCustomAlgorithm);
  protected
    function GetSlaveClass: TsmxOwnerCellClass; override;
  public
    function AddSlave: TsmxCustomAlgorithm;

    property Slaves[Index: Integer]: TsmxCustomAlgorithm read GetSlave write SetSlave; default;
  end;

  { TsmxCustomRequest }

  TsmxCustomRequestList = class;

  TsmxCustomRequest = class(TsmxOwnerCell)
  private
    FCurPerformanceMode: TsmxPerformanceMode;
    FCurDataSetIntf: IsmxDataSet;
    FDatabaseIntf: IsmxDatabase;
    FDatabaseName: String;
    FDataSetIntf: IsmxDataSet;
    FDeletePerformance: TsmxPerformanceMode;
    FDeleteRequestIntf: IsmxDataSet;
    FIsManualRefreshParams: Boolean;
    FInsertPerformance: TsmxPerformanceMode;
    FInsertRequestIntf: IsmxDataSet;
    FOnDelete: TsmxComponentEvent;
    FOnExecute: TsmxComponentEvent;
    FOnInsert: TsmxComponentEvent;
    FOnPrepare: TsmxComponentEvent;
    FOnRefreshParams: TsmxComponentEvent;
    FOnUpdate: TsmxComponentEvent;
    FOperationMode: TsmxOperationMode;
    FPerformanceMode: TsmxPerformanceMode;
    FRequestCell: TsmxBaseCell;
    FUpdatePerformance: TsmxPerformanceMode;
    FUpdateRequestIntf: IsmxDataSet;
    function GetModifyPerformance(Modify: TsmxModifyRequest): TsmxPerformanceMode;
    function GetModifyRequest(Modify: TsmxModifyRequest): IsmxDataSet;
    function GetCellOwner: TsmxCustomRequestList;
    procedure PerformRequest;
    procedure SetCellOwner(Value: TsmxCustomRequestList);
  protected
    procedure DoDelete; virtual;
    procedure DoExecute; virtual;
    procedure DoInsert; virtual;
    procedure DoPrepare; virtual;
    procedure DoRefreshParams; virtual;
    procedure DoUpdate; virtual;
    procedure InternalDelete; virtual;
    procedure InternalExecute; virtual;
    procedure InternalInsert; virtual;
    procedure InternalPrepare; virtual;
    procedure InternalRefreshParams; virtual;
    procedure InternalUpdate; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager); override;
    procedure SetDatabaseName(const Value: String); virtual;
    procedure SetDataSet(const Value: IsmxDataSet); virtual;
    procedure SetIsManualRefreshParams(Value: Boolean); virtual;
    procedure SetModifyPerformance(Modify: TsmxModifyRequest; Value: TsmxPerformanceMode); virtual;
    procedure SetModifyRequest(Modify: TsmxModifyRequest; const Value: IsmxDataSet); virtual;
    procedure SetOperationMode(Value: TsmxOperationMode); virtual;
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;
    procedure SetRequestCell(Value: TsmxBaseCell); virtual;

    property CurDataSet: IsmxDataSet read FCurDataSetIntf write FCurDataSetIntf;
    property CurPerformanceMode: TsmxPerformanceMode read FCurPerformanceMode write FCurPerformanceMode;
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
    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    property DataSet: IsmxDataSet read FDataSetIntf write SetDataSet;
    property IsManualRefreshParams: Boolean read FIsManualRefreshParams write SetIsManualRefreshParams;
    property ModifyPerformances[Modify: TsmxModifyRequest]: TsmxPerformanceMode read GetModifyPerformance write SetModifyPerformance;
    property ModifyRequests[Modify: TsmxModifyRequest]: IsmxDataSet read GetModifyRequest write SetModifyRequest;
    property OperationMode: TsmxOperationMode read FOperationMode write SetOperationMode {default omManual};
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write SetPerformanceMode;
    property RequestCell: TsmxBaseCell read FRequestCell write SetRequestCell;

    property OnDelete: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnExecute: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnInsert: TsmxComponentEvent read FOnExecute write FOnExecute;
    property OnPrepare: TsmxComponentEvent read FOnPrepare write FOnPrepare;
    property OnRefreshParams: TsmxComponentEvent read FOnRefreshParams write FOnRefreshParams;
    property OnUpdate: TsmxComponentEvent read FOnExecute write FOnExecute;
  end;

  { TsmxCustomRequestList }

  TsmxCustomRequestList = class(TsmxOwnerCell)
  private
    function GetSlave(Index: Integer): TsmxCustomRequest;
    procedure SetSlave(Index: Integer; Value: TsmxCustomRequest);
  protected
    function GetSlaveClass: TsmxOwnerCellClass; override;
  public
    function AddSlave: TsmxCustomRequest;

    property Slaves[Index: Integer]: TsmxCustomRequest read GetSlave write SetSlave; default;
  end;

  { TsmxCustomColumn }

  TsmxCustomGrid = class;

  TsmxCustomColumn = class(TsmxControlCell)
  private
    FOnSnapHeader: TsmxComponentEvent;
    function GetCellOwner: TsmxCustomGrid;
    procedure SetCellOwner(Value: TsmxCustomGrid);
  protected
    procedure DoSnapHeader; virtual;
    function GetColumnAlignment: TAlignment; virtual;
    function GetColumnCaption: String; virtual;
    function GetColumnColor: TColor; virtual;
    function GetColumnFont: TFont; virtual;
    function GetColumnValue: Variant; virtual;
    function GetFieldName: String; virtual;
    function GetHeaderAlignment: TAlignment; virtual;
    function GetHeaderColor: TColor; virtual;
    function GetHeaderFont: TFont; virtual;
    function GetHeaderCaption: String; virtual;
    procedure InternalSnapHeader; virtual;
    procedure SetColumnAlignment(Value: TAlignment); virtual;
    procedure SetColumnCaption(const Value: String); virtual;
    procedure SetColumnColor(Value: TColor); virtual;
    procedure SetColumnFont(Value: TFont); virtual;
    procedure SetColumnValue(const Value: Variant); virtual;
    procedure SetFieldName(const Value: String); virtual;
    procedure SetHeaderAlignment(Value: TAlignment); virtual;
    procedure SetHeaderCaption(const Value: String); virtual;
    procedure SetHeaderColor(Value: TColor); virtual;
    procedure SetHeaderFont(Value: TFont); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure SnapHeader;

    property CellOwner: TsmxCustomGrid read GetCellOwner write SetCellOwner;
    property ColumnAlignment: TAlignment read GetColumnAlignment write SetColumnAlignment;
    property ColumnCaption: String read GetColumnCaption write SetColumnCaption;
    property ColumnColor: TColor read GetColumnColor write SetColumnColor;
    property ColumnFont: TFont read GetColumnFont write SetColumnFont;
    property ColumnValue: Variant read GetColumnValue write SetColumnValue;
    property FieldName: String read GetFieldName write SetFieldName;
    property HeaderAlignment: TAlignment read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderCaption: String read GetHeaderCaption write SetHeaderCaption;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;

    property OnSnapHeader: TsmxComponentEvent read FOnSnapHeader write FOnSnapHeader;
  end;

  { TsmxCustomGrid }

  TsmxCustomGrid = class(TsmxActionCell)
  private
    FGridOptions: TsmxGridOptions;
    FRequest: TsmxCustomRequest;
    FOnChangeRow: TsmxComponentEvent;
    FOnRefresh: TsmxComponentEvent;
    function GetSlave(Index: Integer): TsmxCustomColumn;
    procedure SetSlave(Index: Integer; Value: TsmxCustomColumn);
  protected
    procedure DoChangeRow; virtual;
    procedure DoRefresh; virtual;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalChangeRow; virtual;
    procedure InternalRefresh; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetGridOptions(Value: TsmxGridOptions); virtual;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
  public
    function AddSlave: TsmxCustomColumn;
    procedure Assign(Source: TPersistent); override;
    procedure ChangeRow;
    procedure Refresh;

    property GridOptions: TsmxGridOptions read FGridOptions write SetGridOptions;
    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property Slaves[Index: Integer]: TsmxCustomColumn read GetSlave write SetSlave; default;

    property OnChangeRow: TsmxComponentEvent read FOnChangeRow write FOnChangeRow;
    property OnRefresh: TsmxComponentEvent read FOnRefresh write FOnRefresh;
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
    function GetHeaderCaption: String; virtual;
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
    procedure SetHeaderCaption(const Value: String); virtual;
    procedure SetHeaderColor(Value: TColor); virtual;
    procedure SetHeaderFont(Value: TFont); virtual;
    procedure SetValueFormat(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeFilter;

    property CellOwner: TsmxCustomFilterDesk read GetCellOwner write SetCellOwner;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property FilterAlignment: TAlignment read GetFilterAlignment write SetFilterAlignment;
    property FilterCaption: String read GetCellCaption write SetCellCaption;
    property FilterColor: TColor read GetFilterColor write SetFilterColor;
    property FilterFont: TFont read GetFilterFont write SetFilterFont;
    property FilterOptions: TsmxFilterOptions read FFilterOptions write SetFilterOptions;
    property FilterValue: Variant read GetFilterValue write SetFilterValue;
    property HeaderAlignment: TAlignment read GetHeaderAlignment write SetHeaderAlignment;
    property HeaderCaption: String read GetHeaderCaption write SetHeaderCaption;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property ValueFormat: String read GetValueFormat write SetValueFormat;

    property OnChangeFilter: TsmxComponentEvent read FOnChangeFilter write FOnChangeFilter;
  end;

  { TsmxCustomFilterDesk }

  TsmxCustomFilterDesk = class(TsmxActionCell)
  private
    FOnApply: TsmxComponentEvent;
    FOnPrepare: TsmxComponentEvent;
    FRequest: TsmxCustomRequest;
    function GetSlave(Index: Integer): TsmxCustomFilter;
    procedure SetSlave(Index: Integer; Value: TsmxCustomFilter);
  protected
    procedure DoApply; virtual;
    procedure DoPrepare; virtual;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalApply; virtual;
    procedure InternalPrepare; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetRequest(Value: TsmxCustomRequest); virtual;
  public
    function AddSlave: TsmxCustomFilter;
    procedure Apply;
    procedure Prepare;

    property Request: TsmxCustomRequest read FRequest write SetRequest;
    property Slaves[Index: Integer]: TsmxCustomFilter read GetSlave write SetSlave; default;

    property OnApply: TsmxComponentEvent read FOnApply write FOnApply;
    property OnPrepare: TsmxComponentEvent read FOnPrepare write FOnPrepare;
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetFilterDesk(Value: TsmxCustomFilterDesk); virtual;
    procedure SetGrid(Value: TsmxCustomGrid); virtual;
  public
    property CellOwner: TsmxCustomPage read GetCellOwner write SetCellOwner;
    property FilterDesk: TsmxCustomFilterDesk read FFilterDesk write SetFilterDesk;
    property Grid: TsmxCustomGrid read FGrid write SetGrid;
  end;

  { TsmxCustomPage }

  TsmxCustomPageManager = class;

  TsmxCustomPage = class(TsmxActionCell)
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
    FOnChangePage: TsmxComponentEvent;
    function GetSlave(Index: Integer): TsmxCustomPage;
    procedure SetSlave(Index: Integer; Value: TsmxCustomPage);
  protected
    procedure DoChangePage; virtual;
    function GetIsMultiLine: Boolean; virtual;
    function GetPageManagerStyle: TsmxPageManagerStyle; virtual;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalChangePage; virtual;
    procedure SetIsMultiLine(Value: Boolean); virtual;
    procedure SetPageManagerStyle(Value: TsmxPageManagerStyle); virtual;
  public
    function AddSlave: TsmxCustomPage;
    procedure Assign(Source: TPersistent); override;
    procedure ChangePage;

    property IsMultiLine: Boolean read GetIsMultiLine write SetIsMultiLine;
    property PageManagerStyle: TsmxPageManagerStyle read GetPageManagerStyle write SetPageManagerStyle;
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
    function GetSlaveClass: TsmxOwnerCellClass; override;
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
    function GetSlaveClass: TsmxOwnerCellClass; override;
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
    function GetSlaveClass: TsmxOwnerCellClass; override;
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
    function GetSlaveClass: TsmxOwnerCellClass; override;
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

  TsmxCustomToolBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomToolItem;
    procedure SetSlave(Index: Integer; Value: TsmxCustomToolItem);
  protected
    function GetIsFlat: Boolean; virtual;
    function GetIsShowCaptions: Boolean; virtual;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure SetIsFlat(Value: Boolean); virtual;
    procedure SetIsShowCaptions(Value: Boolean); virtual;
  public
    function AddSlave: TsmxCustomToolItem;
    procedure Assign(Source: TPersistent); override;

    property Slaves[Index: Integer]: TsmxCustomToolItem read GetSlave write SetSlave; default;
    property IsFlat: Boolean read GetIsFlat write SetIsFlat;
    property IsShowCaptions: Boolean read GetIsShowCaptions write SetIsShowCaptions;
  end;

  { TsmxCustomControlBoard }

  TsmxCustomControlBoard = class(TsmxControlCell)
  private
    function GetSlave(Index: Integer): TsmxCustomToolBoard;
    procedure SetSlave(Index: Integer; Value: TsmxCustomToolBoard);
  protected
    function GetSlaveClass: TsmxOwnerCellClass; override;
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
    function GetSlaveClass: TsmxOwnerCellClass; override;
  public
    function AddSlave: TsmxCustomStatusItem;

    property Slaves[Index: Integer]: TsmxCustomStatusItem read GetSlave write SetSlave; default;
  end;

  { TsmxCustomForm }

  TsmxCustomForm = class(TsmxActionCell, IsmxForm)
  private
    FAlgorithmList: TsmxCustomAlgorithmList;
    FID: Integer;
    FIntfID: Integer;
    FPopupList: TsmxCustomPopupList;
    FRequestList: TsmxCustomRequestList;
  protected
    procedure FreeForm;
    function GetCfgID: Integer;
    function GetFormBorder: TsmxFormBorder; virtual;
    function GetFormPosition: TsmxFormPosition; virtual;
    function GetID: Integer;
    //function GetIsApplicationForm: Boolean; virtual;
    function GetIsMaximize: Boolean; virtual;
    function GetModalResult: TModalResult; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlgorithmList(Value: TsmxCustomAlgorithmList); virtual;
    procedure SetFormBorder(Value: TsmxFormBorder); virtual;
    procedure SetFormManager(const Value: IsmxFormManager); override;
    procedure SetFormPosition(Value: TsmxFormPosition); virtual;
    procedure SetIntfID(Value: Integer); virtual;
    procedure SetIsMaximize(Value: Boolean); virtual;
    procedure SetModalResult(Value: TModalResult); virtual;
    procedure SetPopupList(Value: TsmxCustomPopupList); virtual;
    procedure SetRequestList(Value: TsmxCustomRequestList); virtual;
  public
    constructor CreateByID(AOwner: TComponent; AID: Integer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; override;
    procedure Close; virtual;
    procedure Show; virtual;
    function ShowModal: TModalResult; virtual;

    property AlgorithmList: TsmxCustomAlgorithmList read FAlgorithmList write SetAlgorithmList;
    property FormBorder: TsmxFormBorder read GetFormBorder write SetFormBorder;
    property FormPosition: TsmxFormPosition read GetFormPosition write SetFormPosition;
    property ID: Integer read GetID;
    property IntfID: Integer read FIntfID write SetIntfID;
    //property IsApplicationForm: Boolean read GetIsApplicationForm {write SetIsMainForm};
    property IsMaximize: Boolean read GetIsMaximize write SetIsMaximize;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
    property PopupList: TsmxCustomPopupList read FPopupList write SetPopupList;
    property RequestList: TsmxCustomRequestList read FRequestList write SetRequestList;
  end;

  TsmxCustomFormClass = class of TsmxCustomForm;

  { TsmxStateCfg }

  TsmxStateCfg = class(TsmxBaseCfg)
  private
    FIntfID: Integer;
    FRecID: Integer;
  protected
    procedure Load; override;
    procedure ReadIntf(const Node: IXMLNode; ID: Integer); virtual;
    procedure Save; override;
    procedure SetIntfID(Value: Integer); virtual;
    procedure SetRecID(Value: Integer); virtual;
    procedure WriteIntf(const Node: IXMLNode; ID: Integer); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Read; override;
    procedure Remove; override;
    procedure Write; override;

    property IntfID: Integer read FIntfID write SetIntfID;
    property RecID: Integer read FRecID write SetRecID;
  end;

  TsmxStateCfgClass = class of TsmxStateCfg;

implementation

uses
  DB, Variants, SysUtils, TypInfo, smxConsts, smxDBTypes, smxFuncs, smxDBFuncs,
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

destructor TsmxBaseCfg.Destroy;
begin
  FXMLDocIntf := nil;
  inherited Destroy;
end;

procedure TsmxBaseCfg.Assign(Source: TPersistent);
begin
  if Source is TsmxBaseCfg then
    CfgID := TsmxBaseCfg(Source).CfgID
  else
    inherited Assign(Source);
end;

procedure TsmxBaseCfg.Clear;
begin
end;

function TsmxBaseCfg.GetRootNode: IXMLNode;
begin
  Result := XMLDoc.ChildNodes.FindNode(smxConsts.cRootNodeName);
  if not Assigned(Result) then
    Result := XMLDoc.AddChild(smxConsts.cRootNodeName);
end;

function TsmxBaseCfg.GetXMLDoc: IXMLDocument;
begin
  if not Assigned(FXMLDocIntf) then
    FXMLDocIntf := smxFuncs.NewXML;
  Result := FXMLDocIntf;
end;

function TsmxBaseCfg.GetXMLText: String;
begin
  try
    XMLDoc.Active := True;
    Write;
    Result := smxFuncs.UnFormatXMLText(XMLDoc.XML.Text);
  except
    on E: Exception do
      raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
        [ClassName, FCfgID, 'write'], FCfgID, E.Message);
  end;
end;

procedure TsmxBaseCfg.SetXMLText(const Value: String);
begin
  try
    XMLDoc.XML.Text := smxFuncs.FormatXMLText(Value);
    XMLDoc.Active := True;
    Read;
  except
    on E: Exception do
      raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
        [ClassName, FCfgID, 'load'], FCfgID, E.Message);
  end;
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
    XMLDoc.XML.Text := Value else
    XMLDoc.XML.Text := '';
  try
    XMLDoc.Active := True;
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
  if smxDBFuncs.SetValueByKey(Request, Key, XMLDoc.XML.Text, Performance) then
    FCfgID := Key;
end;

procedure TsmxBaseCfg.Read;
begin
  Clear;
end;

procedure TsmxBaseCfg.Write;
begin
  RootNode.ChildNodes.Clear;
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

procedure TsmxBaseCfg.SetCfgID(Value: Integer);
begin
  FCfgID := Value;
end;

procedure TsmxBaseCfg.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
end;

{ TsmxCellCfg }

procedure TsmxCellCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxCellCfg then
  begin
    ImageListName := TsmxCellCfg(Source).ImageListName;
    InitializeAlgCfgID := TsmxCellCfg(Source).InitializeAlgCfgID;
  end;
end;

procedure TsmxCellCfg.Clear;
begin
  inherited Clear;
  ImageListName := '';
  InitializeAlgCfgID := 0;
end;

procedure TsmxCellCfg.Read;
var
  n: IXMLNode;
begin
  inherited Read;
  n := RootNode.ChildNodes.FindNode(smxConsts.cCellNodeName);
  if Assigned(n) then
    ReadCell(n);
end;

procedure TsmxCellCfg.Write;
var
  n: IXMLNode;
begin
  inherited Write;
  n := RootNode.AddChild(smxConsts.cCellNodeName);
  WriteCell(n);
end;

procedure TsmxCellCfg.ReadCell(const Node: IXMLNode);
begin
  ImageListName := Node.Attributes['ImageListName'];
  InitializeAlgCfgID := Node.Attributes['InitializeAlgCfgID'];
end;

procedure TsmxCellCfg.WriteCell(const Node: IXMLNode);
begin
  Node.Attributes['ImageListName'] := ImageListName;
  Node.Attributes['InitializeAlgCfgID'] := InitializeAlgCfgID;
end;

procedure TsmxCellCfg.SetImageListName(const Value: String);
begin
  FImageListName := Value;
end;

procedure TsmxCellCfg.SetInitializeAlgCfgID(Value: Integer);
begin
  FInitializeAlgCfgID := Value;
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
  inherited Destroy;
end;

procedure TsmxBaseCell.Assign(Source: TPersistent);
begin
  if Source is TsmxBaseCell then
  begin
    CfgID := TsmxBaseCell(Source).CfgID;
    DatabaseManager := TsmxBaseCell(Source).DatabaseManager;
    FormManager := TsmxBaseCell(Source).FormManager;
    //ImageList := TsmxBaseCell(Source).ImageList;
    ImageListManager := TsmxBaseCell(Source).ImageListManager;
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

procedure TsmxBaseCell.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  ResetCellProps;
  if FIsRecieveCfg then
  begin
    Cfg.CfgID := FCfgID;
    Cfg.SelectRequest := FSelectRequest;
    Cfg.Receive;
  end;
  if Cfg is TsmxCellCfg then
  begin
    ImageListName := TsmxCellCfg(Cfg).ImageListName;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnInitialize := smxClassFuncs.GetEventForm(Form, TsmxCellCfg(Cfg).InitializeAlgCfgID);
  end;
end;

procedure TsmxBaseCell.Initialize;
begin
  InternalInitialize;
  DoInitialize;
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

function TsmxBaseCell.FindChildByInternalObject(Obj: TObject): TsmxBaseCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to CellCount - 1 do
    if Cells[i].GetInternalObject = Obj then
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

{function TsmxBaseCell.GetImageList: TCustomImageList;
begin
  if not Assigned(FImageList) and Assigned(FCellParent) then
    FImageList := FCellParent.ImageList;
  Result := FImageList;
end;}

procedure TsmxBaseCell.SetImageList(Value: TCustomImageList);
begin
  FImageList := Value;
  if Assigned(FImageList) then
    FImageList.FreeNotification(Self);
end;

function TsmxBaseCell.GetImageListManager: IsmxImageListManager;
begin
  if not Assigned(FImageListManagerIntf) and Assigned(FCellParent) then
    FImageListManagerIntf := FCellParent.ImageListManager;
  Result := FImageListManagerIntf;
end;

procedure TsmxBaseCell.SetImageListManager(const Value: IsmxImageListManager);
var
  i: Integer;
begin
  if Assigned(FImageListManagerIntf) and (FImageListName <> '') then
    ImageList := nil;
  FImageListManagerIntf := Value;
  if Assigned(FImageListManagerIntf) and (FImageListName <> '') then
  begin
    i := FImageListManagerIntf.AddImageList(FImageListName);
    if i <> -1 then
      ImageList := FImageListManagerIntf.ImageLists[i];
  end;
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

procedure TsmxBaseCell.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ImageList then
      ImageList := nil else
    if AComponent = SelectRequest then
      SelectRequest := nil;
  end;
end;

procedure TsmxBaseCell.ResetCellProps;
begin
  ImageListName := '';
  OnInitialize := nil;
  if Assigned(FCellList) then
    ClearCells;
end;

procedure TsmxBaseCell.SetCellFeedback;
begin
  if GetInternalObject is TComponent then
    TComponent(GetInternalObject).Tag := Longint(Self);
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

procedure TsmxBaseCell.SetImageListName(const Value: String);
var
  i: Integer;
begin
  if Assigned(ImageListManager) and (FImageListName <> '') then
    ImageList := nil;
  FImageListName := Value;
  if Assigned(ImageListManager) and (FImageListName <> '') then
  begin
    i := ImageListManager.AddImageList(FImageListName);
    if i <> -1 then
      ImageList := ImageListManager.ImageLists[i];
  end;
end;

procedure TsmxBaseCell.SetIsRecieveCfg(Value: Boolean);
begin
  FIsRecieveCfg := Value;
end;

procedure TsmxBaseCell.SetSelectRequest(Value: TsmxCustomRequest);
begin
  FSelectRequest := Value;
  if Assigned(FSelectRequest) then
    FSelectRequest.FreeNotification(Self);
end;

{ TsmxTypeCfg }

procedure TsmxTypeCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxTypeCfg then
  begin
    CellClass := TsmxTypeCfg(Source).CellClass;
    CfgClass := TsmxTypeCfg(Source).CfgClass;
  end;
end;

procedure TsmxTypeCfg.Clear;
begin
  CellClassName := '';
  CfgClassName := '';
end;

procedure TsmxTypeCfg.ReadCell(const Node: IXMLNode);
begin
  CellClassName := Node.Attributes['CellClassName'];
  CfgClassName := Node.Attributes['CfgClassName'];
end;

procedure TsmxTypeCfg.WriteCell(const Node: IXMLNode);
begin
  Node.Attributes['CellClassName'] := CellClassName;
  Node.Attributes['CfgClassName'] := CfgClassName;
end;

procedure TsmxTypeCfg.SetCellClass(Value: TsmxBaseCellClass);
begin
  if Assigned(FCellClass) then
    FCellClassName := '';
  FCellClass := Value;
  if Assigned(FCellClass) then
    FCellClassName := FCellClass.ClassName;
end;

procedure TsmxTypeCfg.SetCellClassName(const Value: String);
begin
  if FCellClassName <> '' then
    FCellClass := nil;
  FCellClassName := Value;
  if FCellClassName <> '' then
    FCellClass := TsmxBaseCellClass(Classes.FindClass(FCellClassName));
end;

procedure TsmxTypeCfg.SetCfgClass(Value: TsmxBaseCfgClass);
begin
  if Assigned(FCfgClass) then
    FCfgClassName := '';
  FCfgClass := Value;
  if Assigned(FCfgClass) then
    FCfgClassName := FCfgClass.ClassName;
end;

procedure TsmxTypeCfg.SetCfgClassName(const Value: String);
begin
  if FCfgClassName <> '' then
    FCfgClass := nil;
  FCfgClassName := Value;
  if FCfgClassName <> '' then
    FCfgClass := TsmxBaseCfgClass(Classes.FindClass(FCfgClassName));
end;

{ TsmxSimpleKitItem }

procedure TsmxSimpleKitItem.Assign(Source: TsmxKitItem);
begin
  if not(Source is TsmxSimpleKitItem) then
    inherited Assign(Source);
end;

procedure TsmxSimpleKitItem.Clear;
begin
end;

function TsmxSimpleKitItem.GetKit: TsmxSimpleKit;
begin
  Result := TsmxSimpleKit(inherited Kit);
end;

procedure TsmxSimpleKitItem.SetKit(Value: TsmxSimpleKit);
begin
  inherited Kit := Value;
end;

procedure TsmxSimpleKitItem.Read(const Node: IXMLNode);
begin
  Clear;
end;

procedure TsmxSimpleKitItem.Write(const Node: IXMLNode);
begin
  Node.ChildNodes.Clear;
  Node.AttributeNodes.Clear;
end;

{ TsmxSimpleKit }

constructor TsmxSimpleKit.Create(AItemClass: TsmxKitItemClass);
begin
  inherited Create(AItemClass);
  FKitNodeName := smxConsts.cKitNodeName;
  FItemNodeName := smxConsts.cItemNodeName;
end;

constructor TsmxSimpleKit.Create;
begin
  Create(TsmxSimpleKitItem);
end;

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

procedure TsmxSimpleKit.Read(const Node: IXMLNode);
var
  i: Integer;
  n: IXMLNode;
begin
  if ChangeMode = cmReplace then
    Clear;
  n := Node.ChildNodes.FindNode(FKitNodeName);
  if Assigned(n) then
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = FItemNodeName then
        Add.Read(n.ChildNodes[i]);
end;

procedure TsmxSimpleKit.Write(const Node: IXMLNode);
var
  i: Integer;
  n: IXMLNode;
begin
  if (Count > 0) or FIsWriteEmpty then
  begin
    n := Node.ChildNodes.FindNode(FKitNodeName);
    if not Assigned(n) then
      n := Node.AddChild(FKitNodeName);
    if ChangeMode = cmReplace then
    begin
      n.ChildNodes.Clear;
      n.AttributeNodes.Clear;
    end;
    for i := 0 to Count - 1 do
      Items[i].Write(n.AddChild(FItemNodeName));
  end;
end;

{ TsmxOwnerKitItem }

procedure TsmxOwnerKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxOwnerKitItem then
    CfgID := TsmxOwnerKitItem(Source).CfgID;
end;

procedure TsmxOwnerKitItem.Clear;
begin
  CfgID := 0;
end;

function TsmxOwnerKitItem.GetKit: TsmxOwnerKit;
begin
  Result := TsmxOwnerKit(inherited Kit);
end;

procedure TsmxOwnerKitItem.SetKit(Value: TsmxOwnerKit);
begin
  inherited Kit := Value;
end;

procedure TsmxOwnerKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  CfgID := Node.Attributes['CfgID'];
end;

procedure TsmxOwnerKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['CfgID'] := CfgID;
end;

{ TsmxOwnerKit }

constructor TsmxOwnerKit.Create;
begin
  Create(TsmxOwnerKitItem);
end;

function TsmxOwnerKit.Add: TsmxOwnerKitItem;
begin
  Result := TsmxOwnerKitItem(inherited Add);
end;

function TsmxOwnerKit.GetItem(Index: Integer): TsmxOwnerKitItem;
begin
  Result := TsmxOwnerKitItem(inherited Items[Index]);
end;

procedure TsmxOwnerKit.SetItem(Index: Integer; Value: TsmxOwnerKitItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxOwnerCellCfg }

destructor TsmxOwnerCellCfg.Destroy;
begin
  if Assigned(FSlaveCells) then
    FSlaveCells.Free;
  inherited Destroy;
end;

procedure TsmxOwnerCellCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxOwnerCellCfg then
  begin
    SlaveCells := TsmxOwnerCellCfg(Source).SlaveCells;
    SlaveName := TsmxOwnerCellCfg(Source).SlaveName;
  end;
end;

procedure TsmxOwnerCellCfg.Clear;
begin
  if Assigned(FSlaveCells) then
    FSlaveCells.Clear;
  SlaveName := '';
end;

function TsmxOwnerCellCfg.GetSlaveCells: TsmxOwnerKit;
begin
  if not Assigned(FSlaveCells) then
    FSlaveCells := GetSlaveCellsClass.Create;
  Result := TsmxOwnerKit(FSlaveCells);
end;

procedure TsmxOwnerCellCfg.SetSlaveCells(Value: TsmxOwnerKit);
begin
  SlaveCells.Assign(Value);
end;

function TsmxOwnerCellCfg.GetSlaveCellsClass: TsmxSimpleKitClass;
begin
  Result := TsmxOwnerKit;
end;

{procedure TsmxOwnerCellCfg.Read;
var
  n: IXMLNode;
begin
  inherited Read;
  n := RootNode.ChildNodes.FindNode(smxConsts.cSlaveNodeName);
  if Assigned(n) and (n.ChildNodes.Count > 0) then
    SlaveCells.Read(n);
end;

procedure TsmxOwnerCellCfg.Write;
var
  n: IXMLNode;
begin
  inherited Write;
  n := RootNode.AddChild(smxConsts.cSlaveNodeName);
  SlaveCells.Write(n);
end;}

procedure TsmxOwnerCellCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  SlaveName := Node.Attributes['SlaveName'];
  SlaveCells.Read(Node);
end;

procedure TsmxOwnerCellCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['SlaveName'] := SlaveName;
  SlaveCells.Write(Node);
end;

procedure TsmxOwnerCellCfg.SetSlaveName(const Value: String);
begin
  FSlaveName := Value;
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
  if FIsAltSlaveClass then
    Result := GetAltSlaveClass(SlaveList.Count).Create(Self) else
    Result := SlaveClass.Create(Self);
  Result.FCellOwner := Self;
  if FIsOwnerIsParent then
    Result.CellParent := Self;
  SlaveList.Add(Result);
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
  TsmxOwnerCell(SlaveList[Index]).Free;
end;

function TsmxOwnerCell.FindSlaveByCfgID(CfgID: Integer): TsmxOwnerCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
    if Slaves[i].CfgID = CfgID then
    begin
      Result := Slaves[i];
      Break;
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

function TsmxOwnerCell.FindSlaveByInternalObject(Obj: TObject): TsmxOwnerCell;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to SlaveCount - 1 do
    if Slaves[i].GetInternalObject = Obj then
    begin
      Result := Slaves[i];
      Break;
    end;
end;

function TsmxOwnerCell.GetSlave(Index: Integer): TsmxOwnerCell;
begin
  Result := TsmxOwnerCell(SlaveList[Index]);
end;

procedure TsmxOwnerCell.SetSlave(Index: Integer; Value: TsmxOwnerCell);
begin
  TsmxOwnerCell(SlaveList[Index]).Assign(Value);
end;

function TsmxOwnerCell.GetAltSlaveClass(Index: Integer): TsmxOwnerCellClass;
begin
  if Cfg is TsmxOwnerCellCfg then
    Result := TsmxOwnerCellClass(smxClassFuncs.CfgIDToCellClass(
      TsmxOwnerCellCfg(Cfg).SlaveCells[Index].CfgID, SelectRequest))
  else
    Result := GetSlaveClass;
end;

function TsmxOwnerCell.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxOwnerCellCfg;
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

procedure TsmxOwnerCell.InternalInitialize;
var
  i: Integer;
  Cell: TsmxOwnerCell;
begin
  inherited InternalInitialize;
  if Cfg is TsmxOwnerCellCfg then
  begin
    SlaveName := TsmxOwnerCellCfg(Cfg).SlaveName;
    for i := 0 to TsmxOwnerCellCfg(Cfg).SlaveCells.Count - 1 do
    begin
      Cell := AddSlave;
      SetSlaveCellProps(Cell, TsmxOwnerCellCfg(Cfg).SlaveCells[i]);
      Cell.SelectRequest := SelectRequest;
      Cell.Initialize;
    end;
  end;
end;

procedure TsmxOwnerCell.ResetCellProps;
begin
  inherited ResetCellProps;
  SlaveName := '';
  if Assigned(FSlaveList) then
    ClearSlaves;
end;

procedure TsmxOwnerCell.SetIsAltSlaveClass(Value: Boolean);
begin
  FIsAltSlaveClass := Value;
end;

procedure TsmxOwnerCell.SetIsOwnerIsParent(Value: Boolean);
begin
  FIsOwnerIsParent := Value;
end;

procedure TsmxOwnerCell.SetCellOwner(Value: TsmxOwnerCell);
begin
  if Assigned(FCellOwner) then
  begin
    FCellOwner.SlaveList.Remove(Self);
    if FIsOwnerIsParent then
      CellParent := nil;
  end;
  FCellOwner := Value;
  if Assigned(FCellOwner) then
  begin
    FCellOwner.SlaveList.Add(Self);
    if FIsOwnerIsParent then
      CellParent := FCellOwner;
  end;
end;

procedure TsmxOwnerCell.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
begin
  Slave.CfgID := Item.CfgID;
end;

procedure TsmxOwnerCell.SetSlaveName(const Value: String);
begin
  FSlaveName := Value;
end;

{ TsmxControlKitItem }

procedure TsmxControlKitItem.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxControlKitItem then
  begin
    ItemActive := TsmxControlKitItem(Source).ItemActive;
    ItemAlign := TsmxControlKitItem(Source).ItemAlign;
    ItemAnchors := TsmxControlKitItem(Source).ItemAnchors;
    ItemCursor := TsmxControlKitItem(Source).ItemCursor;
    ItemEnabled := TsmxControlKitItem(Source).ItemEnabled;
    ItemHeight := TsmxControlKitItem(Source).ItemHeight;
    ItemLeft := TsmxControlKitItem(Source).ItemLeft;
    ItemTop := TsmxControlKitItem(Source).ItemTop;
    ItemVisible := TsmxControlKitItem(Source).ItemVisible;
    ItemWidth := TsmxControlKitItem(Source).ItemWidth;
    PopupMenuCfgID := TsmxControlKitItem(Source).PopupMenuCfgID;
  end;
end;

procedure TsmxControlKitItem.Clear;
begin
  inherited Clear;
  ItemActive := False;
  ItemAlign := alNone;
  ItemAnchors := [];
  ItemCursor := Controls.crDefault;
  ItemEnabled := False;
  ItemHeight := 0;
  ItemLeft := 0;
  ItemTop := 0;
  ItemVisible := False;
  ItemWidth := 0;
  PopupMenuCfgID := 0;
end;

function TsmxControlKitItem.GetKit: TsmxControlKit;
begin
  Result := TsmxControlKit(inherited Kit);
end;

procedure TsmxControlKitItem.SetKit(Value: TsmxControlKit);
begin
  inherited Kit := Value;
end;

procedure TsmxControlKitItem.Read(const Node: IXMLNode);
begin
  inherited Read(Node);
  ItemActive := Node.Attributes['ItemActive']; //SysUtils.StrToBool(Node.Attributes['ItemActive']);
  ItemAlign := TAlign(TypInfo.GetEnumValue(TypeInfo(TAlign), Node.Attributes['ItemAlign']));
  ItemAnchors := TAnchors(smxFuncs.StrToSet(TypeInfo(TAnchorKind), Node.Attributes['ItemAnchors']));
  ItemCursor := Node.Attributes['ItemCursor'];
  ItemEnabled := Node.Attributes['ItemEnabled']; //SysUtils.StrToBool(Node.Attributes['ItemEnabled']);
  ItemHeight := Node.Attributes['ItemHeight'];
  ItemLeft := Node.Attributes['ItemLeft'];
  ItemTop := Node.Attributes['ItemTop'];
  ItemVisible := Node.Attributes['ItemVisible']; //SysUtils.StrToBool(Node.Attributes['ItemVisible']);
  ItemWidth := Node.Attributes['ItemWidth'];
  PopupMenuCfgID := Node.Attributes['PopupMenuCfgID'];
end;

procedure TsmxControlKitItem.Write(const Node: IXMLNode);
begin
  inherited Write(Node);
  Node.Attributes['ItemActive'] := SysUtils.BoolToStr(ItemActive, True);
  Node.Attributes['ItemAlign'] := TypInfo.GetEnumName(TypeInfo(TAlign), Integer(ItemAlign));
  Node.Attributes['ItemAnchors'] := smxFuncs.SetToStr(TypeInfo(TAnchorKind), Byte(ItemAnchors), True);
  Node.Attributes['ItemCursor'] := ItemCursor;
  Node.Attributes['ItemEnabled'] := SysUtils.BoolToStr(ItemEnabled, True);
  Node.Attributes['ItemHeight'] := ItemHeight;
  Node.Attributes['ItemLeft'] := ItemLeft;
  Node.Attributes['ItemTop'] := ItemTop;
  Node.Attributes['ItemVisible'] := SysUtils.BoolToStr(ItemVisible, True);
  Node.Attributes['ItemWidth'] := ItemWidth;
  Node.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;
end;

{ TsmxControlKit }

constructor TsmxControlKit.Create;
begin
  Create(TsmxControlKitItem);
end;

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

{ TsmxControlCellCfg }

procedure TsmxControlCellCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxControlCellCfg then
  begin
    BackupAlgCfgID := TsmxControlCellCfg(Source).BackupAlgCfgID;
    CfgActive := TsmxControlCellCfg(Source).CfgActive;
    CfgAlign := TsmxControlCellCfg(Source).CfgAlign;
    CfgAnchors := TsmxControlCellCfg(Source).CfgAnchors;
    CfgCursor := TsmxControlCellCfg(Source).CfgCursor;
    CfgEnabled := TsmxControlCellCfg(Source).CfgEnabled;
    CfgHeight := TsmxControlCellCfg(Source).CfgHeight;
    CfgLeft := TsmxControlCellCfg(Source).CfgLeft;
    CfgTop := TsmxControlCellCfg(Source).CfgTop;
    CfgVisible := TsmxControlCellCfg(Source).CfgVisible;
    CfgWidth := TsmxControlCellCfg(Source).CfgWidth;
    DoubleSnapAlgCfgID := TsmxControlCellCfg(Source).DoubleSnapAlgCfgID;
    PopupMenuCfgID := TsmxControlCellCfg(Source).PopupMenuCfgID;
    RestoreAlgCfgID := TsmxControlCellCfg(Source).RestoreAlgCfgID;
    SnapAlgCfgID := TsmxControlCellCfg(Source).SnapAlgCfgID;
  end;
end;

procedure TsmxControlCellCfg.Clear;
begin
  inherited Clear;
  BackupAlgCfgID := 0;
  CfgActive := False;
  CfgAlign := alNone;
  CfgAnchors := [];
  CfgCursor := Controls.crDefault;
  CfgEnabled := False;
  CfgHeight := 0;
  CfgLeft := 0;
  CfgTop := 0;
  CfgVisible := False;
  CfgWidth := 0;
  DoubleSnapAlgCfgID := 0;
  PopupMenuCfgID := 0;
  RestoreAlgCfgID := 0;
  SnapAlgCfgID := 0;
end;

function TsmxControlCellCfg.GetSlaveCells: TsmxControlKit;
begin
  Result := TsmxControlKit(inherited SlaveCells);
end;

procedure TsmxControlCellCfg.SetSlaveCells(Value: TsmxControlKit);
begin
  inherited SlaveCells := Value;
end;

function TsmxControlCellCfg.GetSlaveCellsClass: TsmxSimpleKitClass;
begin
  Result := TsmxControlKit;
end;

procedure TsmxControlCellCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  CfgActive := Node.Attributes['Active']; //SysUtils.StrToBool(Node.Attributes['Active']);
  CfgAlign := TAlign(TypInfo.GetEnumValue(TypeInfo(TAlign), Node.Attributes['Align'])); //Node.Attributes['Align'];
  CfgAnchors := TAnchors(smxFuncs.StrToSet(TypeInfo(TAnchorKind), Node.Attributes['Anchors'])); //TAnchors(Byte(Node.Attributes['Anchors']));
  CfgCursor := Node.Attributes['Cursor'];
  CfgEnabled := Node.Attributes['Enabled']; //SysUtils.StrToBool(Node.Attributes['Enabled']);
  CfgHeight := Node.Attributes['Height'];
  CfgLeft := Node.Attributes['Left'];
  CfgTop := Node.Attributes['Top'];
  CfgVisible := Node.Attributes['Visible']; //SysUtils.StrToBool(Node.Attributes['Visible']);
  CfgWidth := Node.Attributes['Width'];
  PopupMenuCfgID := Node.Attributes['PopupMenuCfgID'];

  BackupAlgCfgID := Node.Attributes['BackupAlgCfgID'];
  DoubleSnapAlgCfgID := Node.Attributes['DoubleSnapAlgCfgID'];
  RestoreAlgCfgID := Node.Attributes['RestoreAlgCfgID'];
  SnapAlgCfgID := Node.Attributes['SnapAlgCfgID'];
end;

procedure TsmxControlCellCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['Active'] := SysUtils.BoolToStr(CfgActive, True);
  Node.Attributes['Align'] := TypInfo.GetEnumName(TypeInfo(TAlign), Integer(CfgAlign)); //CfgAlign;
  Node.Attributes['Anchors'] := smxFuncs.SetToStr(TypeInfo(TAnchorKind), Byte(CfgAnchors), True); //Byte(CfgAnchors);
  Node.Attributes['Cursor'] := CfgCursor;
  Node.Attributes['Enabled'] := SysUtils.BoolToStr(CfgEnabled, True);
  Node.Attributes['Height'] := CfgHeight;
  Node.Attributes['Left'] := CfgLeft;
  Node.Attributes['Top'] := CfgTop;
  Node.Attributes['Visible'] := SysUtils.BoolToStr(CfgVisible, True);
  Node.Attributes['Width'] := CfgWidth;
  Node.Attributes['PopupMenuCfgID'] := PopupMenuCfgID;

  Node.Attributes['BackupAlgCfgID'] := BackupAlgCfgID;
  Node.Attributes['DoubleSnapAlgCfgID'] := DoubleSnapAlgCfgID;
  Node.Attributes['RestoreAlgCfgID'] := RestoreAlgCfgID;
  Node.Attributes['SnapAlgCfgID'] := SnapAlgCfgID;
end;

procedure TsmxControlCellCfg.SetBackupAlgCfgID(Value: Integer);
begin
  FBackupAlgCfgID := Value;
end;

procedure TsmxControlCellCfg.SetCfgActive(Value: Boolean);
begin
  FCfgActive := Value;
end;

procedure TsmxControlCellCfg.SetCfgAlign(Value: TAlign);
begin
  FCfgAlign := Value;
end;

procedure TsmxControlCellCfg.SetCfgAnchors(Value: TAnchors);
begin
  FCfgAnchors := Value;
end;

procedure TsmxControlCellCfg.SetCfgCursor(Value: TCursor);
begin
  FCfgCursor := Value;
end;

procedure TsmxControlCellCfg.SetCfgEnabled(Value: Boolean);
begin
  FCfgEnabled := Value;
end;

procedure TsmxControlCellCfg.SetCfgHeight(Value: Integer);
begin
  FCfgHeight := Value;
end;

procedure TsmxControlCellCfg.SetCfgLeft(Value: Integer);
begin
  FCfgLeft := Value;
end;

procedure TsmxControlCellCfg.SetCfgTop(Value: Integer);
begin
  FCfgTop := Value;
end;

procedure TsmxControlCellCfg.SetCfgVisible(Value: Boolean);
begin
  FCfgVisible := Value;
end;

procedure TsmxControlCellCfg.SetCfgWidth(Value: Integer);
begin
  FCfgWidth := Value;
end;

procedure TsmxControlCellCfg.SetDoubleSnapAlgCfgID(Value: Integer);
begin
  FDoubleSnapAlgCfgID := Value;
end;

procedure TsmxControlCellCfg.SetPopupMenuCfgID(Value: Integer);
begin
  FPopupMenuCfgID := Value;
end;

procedure TsmxControlCellCfg.SetRestoreAlgCfgID(Value: Integer);
begin
  FRestoreAlgCfgID := Value;
end;

procedure TsmxControlCellCfg.SetSnapAlgCfgID(Value: Integer);
begin
  FSnapAlgCfgID := Value;
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
  Snap;
end;

procedure TsmxControlCell.ControlDblClick(Sender: TObject);
begin
  DoubleSnap;
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

procedure TsmxControlCell.DoDoubleSnap;
begin
  if Assigned(FOnDoubleSnap) then
    FOnDoubleSnap(Self);
end;

procedure TsmxControlCell.InternalDoubleSnap;
begin
end;

procedure TsmxControlCell.DoubleSnap;
begin
  InternalDoubleSnap;
  DoDoubleSnap;
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

procedure TsmxControlCell.DoSnap;
begin
  if Assigned(FOnSnap) then
    FOnSnap(Self);
end;

procedure TsmxControlCell.InternalSnap;
begin
end;

procedure TsmxControlCell.Snap;
begin
  InternalSnap;
  DoSnap;
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
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Align else
    Result := alNone;
end;

procedure TsmxControlCell.SetCellAlign(Value: TAlign);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Align := Value;
end;

function TsmxControlCell.GetCellAnchors: TAnchors;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Anchors else
    Result := [];
end;

procedure TsmxControlCell.SetCellAnchors(Value: TAnchors);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Anchors := Value;
end;

function TsmxControlCell.GetCellCursor: TCursor;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Cursor else
    Result := crDefault;
end;

procedure TsmxControlCell.SetCellCursor(Value: TCursor);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Cursor := Value;
end;

function TsmxControlCell.GetCellEnabled: Boolean;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Enabled else
    Result := False;
end;

procedure TsmxControlCell.SetCellEnabled(Value: Boolean);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Enabled := Value;
end;

function TsmxControlCell.GetCellHeight: Integer;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Height else
    Result := 0;
end;

procedure TsmxControlCell.SetCellHeight(Value: Integer);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Height := Value;
end;

function TsmxControlCell.GetCellLeft: Integer;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Left else
    Result := 0;
end;

procedure TsmxControlCell.SetCellLeft(Value: Integer);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Left := Value;
end;

function TsmxControlCell.GetCellTop: Integer;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Top else
    Result := 0;
end;

procedure TsmxControlCell.SetCellTop(Value: Integer);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Top := Value;
end;

function TsmxControlCell.GetCellVisible: Boolean;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Visible else
    Result := False;
end;

procedure TsmxControlCell.SetCellVisible(Value: Boolean);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Visible := Value;
end;

function TsmxControlCell.GetCellWidth: Integer;
begin
  if GetInternalObject is TControl then
    Result := TControl(GetInternalObject).Width else
    Result := 0;
end;

procedure TsmxControlCell.SetCellWidth(Value: Integer);
begin
  if GetInternalObject is TControl then
    TControl(GetInternalObject).Width := Value;
end;

function TsmxControlCell.GetSlave(Index: Integer): TsmxControlCell;
begin
  Result := TsmxControlCell(inherited Slaves[Index]);
end;

procedure TsmxControlCell.SetSlave(Index: Integer; Value: TsmxControlCell);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxControlCell.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxControlCellCfg;
end;

function TsmxControlCell.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxControlCell;
end;

procedure TsmxControlCell.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxControlCellCfg then
  begin
    CellActive := TsmxControlCellCfg(Cfg).CfgActive;
    CellAlign := TsmxControlCellCfg(Cfg).CfgAlign;
    CellAnchors := TsmxControlCellCfg(Cfg).CfgAnchors;
    CellCursor := TsmxControlCellCfg(Cfg).CfgCursor;
    CellEnabled := TsmxControlCellCfg(Cfg).CfgEnabled;
    CellHeight := TsmxControlCellCfg(Cfg).CfgHeight;
    CellLeft := TsmxControlCellCfg(Cfg).CfgLeft;
    CellTop := TsmxControlCellCfg(Cfg).CfgTop;
    CellVisible := TsmxControlCellCfg(Cfg).CfgVisible;
    CellWidth := TsmxControlCellCfg(Cfg).CfgWidth;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      PopupMenu := smxClassFuncs.GetPopupMenuForm(Form, TsmxControlCellCfg(Cfg).PopupMenuCfgID);
      OnBackup := smxClassFuncs.GetEventForm(Form, TsmxControlCellCfg(Cfg).BackupAlgCfgID);
      OnDoubleSnap := smxClassFuncs.GetEventForm(Form, TsmxControlCellCfg(Cfg).DoubleSnapAlgCfgID);
      OnRestore := smxClassFuncs.GetEventForm(Form, TsmxControlCellCfg(Cfg).RestoreAlgCfgID);
      OnSnap := smxClassFuncs.GetEventForm(Form, TsmxControlCellCfg(Cfg).SnapAlgCfgID);
    end;
  end;
end;

procedure TsmxControlCell.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = PopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TsmxControlCell.ResetCellProps;
begin
  inherited ResetCellProps;
  CellActive := False;
  CellAlign := alNone;
  CellAnchors := [];
  CellCursor := Controls.crDefault;
  CellEnabled := False;
  CellHeight := 0;
  CellLeft := 0;
  CellTop := 0;
  CellVisible := False;
  CellWidth := 0;
  PopupMenu := nil;
  OnBackup := nil;
  OnDoubleSnap := nil;
  OnRestore := nil;
  OnSnap := nil;
end;

procedure TsmxControlCell.SetPopupMenu(Value: TsmxCustomPopupMenu);
begin
  FPopupMenu := Value;
  if Assigned(FPopupMenu) then
    FPopupMenu.FreeNotification(Self);
end;

procedure TsmxControlCell.SetSlaveCellProps(Slave: TsmxOwnerCell; Item: TsmxOwnerKitItem);
var
  Form: TsmxCustomForm;
begin
  inherited SetSlaveCellProps(Slave, Item);
  if (Slave is TsmxControlCell) and (Item is TsmxControlKitItem) then
  begin
    TsmxControlCell(Slave).CellActive := TsmxControlKitItem(Item).ItemActive;
    TsmxControlCell(Slave).CellAlign := TsmxControlKitItem(Item).ItemAlign;
    TsmxControlCell(Slave).CellAnchors := TsmxControlKitItem(Item).ItemAnchors;
    TsmxControlCell(Slave).CellCursor := TsmxControlKitItem(Item).ItemCursor;
    TsmxControlCell(Slave).CellEnabled := TsmxControlKitItem(Item).ItemEnabled;
    TsmxControlCell(Slave).CellHeight := TsmxControlKitItem(Item).ItemHeight;
    TsmxControlCell(Slave).CellLeft := TsmxControlKitItem(Item).ItemLeft;
    TsmxControlCell(Slave).CellTop := TsmxControlKitItem(Item).ItemTop;
    TsmxControlCell(Slave).CellVisible := TsmxControlKitItem(Item).ItemVisible;
    TsmxControlCell(Slave).CellWidth := TsmxControlKitItem(Item).ItemWidth;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      TsmxControlCell(Slave).PopupMenu :=
        smxClassFuncs.GetPopupMenuForm(Form, TsmxControlKitItem(Item).PopupMenuCfgID);
  end;
end;

{ TsmxActionCellCfg }

procedure TsmxActionCellCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxActionCellCfg then
  begin
    AlgorithmCfgID := TsmxActionCellCfg(Source).AlgorithmCfgID;
    CfgCaption := TsmxActionCellCfg(Source).CfgCaption;
    CfgHint := TsmxActionCellCfg(Source).CfgHint;
    CfgImageIndex := TsmxActionCellCfg(Source).CfgImageIndex;
    IsSetAlgorithmEvents := TsmxActionCellCfg(Source).IsSetAlgorithmEvents;
    IsSetAlgorithmProps := TsmxActionCellCfg(Source).IsSetAlgorithmProps;
  end;
end;

procedure TsmxActionCellCfg.Clear;
begin
  inherited Clear;
  AlgorithmCfgID := 0;
  CfgCaption := '';
  CfgHint := '';
  CfgImageIndex := -1;
  IsSetAlgorithmEvents := False;
  IsSetAlgorithmProps := False;
end;

procedure TsmxActionCellCfg.ReadCell(const Node: IXMLNode);
begin
  inherited ReadCell(Node);
  AlgorithmCfgID := Node.Attributes['AlgorithmCfgID'];
  CfgCaption := Node.Attributes['Caption'];
  CfgHint := Node.Attributes['Hint'];
  CfgImageIndex := Node.Attributes['ImageIndex'];
  IsSetAlgorithmEvents := Node.Attributes['IsSetAlgorithmEvents'];
  IsSetAlgorithmProps := Node.Attributes['IsSetAlgorithmProps'];
end;

procedure TsmxActionCellCfg.WriteCell(const Node: IXMLNode);
begin
  inherited WriteCell(Node);
  Node.Attributes['AlgorithmCfgID'] := AlgorithmCfgID;
  Node.Attributes['Caption'] := CfgCaption;
  Node.Attributes['Hint'] := CfgHint;
  Node.Attributes['ImageIndex'] := CfgImageIndex;
  Node.Attributes['IsSetAlgorithmEvents'] := SysUtils.BoolToStr(IsSetAlgorithmEvents, True);
  Node.Attributes['IsSetAlgorithmProps'] := SysUtils.BoolToStr(IsSetAlgorithmProps, True);
end;

procedure TsmxActionCellCfg.SetAlgorithmCfgID(Value: Integer);
begin
  FAlgorithmCfgID := Value;
end;

procedure TsmxActionCellCfg.SetCfgCaption(const Value: String);
begin
  FCfgCaption := Value;
end;

procedure TsmxActionCellCfg.SetCfgHint(const Value: String);
begin
  FCfgHint := Value;
end;

procedure TsmxActionCellCfg.SetCfgImageIndex(Value: Integer);
begin
  FCfgImageIndex := Value;
end;

procedure TsmxActionCellCfg.SetIsSetAlgorithmEvents(Value: Boolean);
begin
  FIsSetAlgorithmEvents := Value;
end;

procedure TsmxActionCellCfg.SetIsSetAlgorithmProps(Value: Boolean);
begin
  FIsSetAlgorithmProps := Value;
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

function TsmxActionCell.GetCellImageIndex: Integer;
begin
  Result := -1;
end;

procedure TsmxActionCell.SetCellImageIndex(Value: Integer);
begin
end;

function TsmxActionCell.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxActionCellCfg;
end;

procedure TsmxActionCell.InternalInitialize;
var
  Form: TsmxCustomForm;
begin
  inherited InternalInitialize;
  if Cfg is TsmxActionCellCfg then
  begin
    CellCaption := TsmxActionCellCfg(Cfg).CfgCaption;
    CellHint := TsmxActionCellCfg(Cfg).CfgHint;
    CellImageIndex := TsmxActionCellCfg(Cfg).CfgImageIndex;
    IsSetAlgorithmEvents := TsmxActionCellCfg(Cfg).IsSetAlgorithmEvents;
    IsSetAlgorithmProps := TsmxActionCellCfg(Cfg).IsSetAlgorithmProps;

    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      Algorithm := smxClassFuncs.GetAlgorithmForm(Form, TsmxActionCellCfg(Cfg).AlgorithmCfgID);
  end;
end;

procedure TsmxActionCell.InternalSnap;
begin
  if Assigned(FAlgorithm) then
    FAlgorithm.ActionCell := Self;
end;

procedure TsmxActionCell.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Algorithm) and (Operation = opRemove) then
    Algorithm := nil;
end;

procedure TsmxActionCell.ResetCellProps;
begin
  inherited ResetCellProps;
  Algorithm := nil;
  CellCaption := '';
  CellHint := '';
  CellImageIndex := -1;
  IsSetAlgorithmEvents := False;
  IsSetAlgorithmProps := False;
end;

procedure TsmxActionCell.SetAlgorithm(Value: TsmxCustomAlgorithm);
begin
  FAlgorithm := Value;
  if Assigned(FAlgorithm) then
  begin
    if FIsSetAlgorithmProps then
    begin
      CellCaption := FAlgorithm.AlgorithmCaption;
      CellHint := FAlgorithm.AlgorithmHint;
      CellImageIndex := FAlgorithm.AlgorithmImageIndex;
    end;
    if FIsSetAlgorithmEvents then
      OnSnap := FAlgorithm.OnExecute;
    FAlgorithm.FreeNotification(Self);
  end;
end;

procedure TsmxActionCell.SetIsSetAlgorithmEvents(Value: Boolean);
begin
  FIsSetAlgorithmEvents := Value;
end;

procedure TsmxActionCell.SetIsSetAlgorithmProps(Value: Boolean);
begin
  FIsSetAlgorithmProps := Value;
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
  InternalExecute;
  DoExecute;
end;

procedure TsmxCustomAlgorithm.DoRefreshParams;
begin
  if Assigned(FOnRefreshParams) then
    FOnRefreshParams(Self);
end;

procedure TsmxCustomAlgorithm.InternalRefreshParams;
var
  i: Integer;
begin
  for i := 0 to AlgorithmParams.Count - 1 do
    AlgorithmParams[i].ParamValue := Variants.Null;
end;

procedure TsmxCustomAlgorithm.RefreshParams;
begin
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
end;

function TsmxCustomAlgorithm.GetAlgorithmCaption: String;
begin
  Result := '';
end;

procedure TsmxCustomAlgorithm.SetAlgorithmCaption(const Value: String);
begin
end;

function TsmxCustomAlgorithm.GetAlgorithmEnabled: Boolean;
begin
  Result := False;
end;

procedure TsmxCustomAlgorithm.SetAlgorithmEnabled(Value: Boolean);
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

procedure TsmxCustomAlgorithm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = ActionCell) and (Operation = opRemove) then
    ActionCell := nil;
end;

procedure TsmxCustomAlgorithm.SetActionCell(Value: TsmxBaseCell);
begin
  FActionCell := Value;
  if Assigned(FActionCell) then
    FActionCell.FreeNotification(Self);
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

function TsmxCustomAlgorithmList.GetSlave(Index: Integer): TsmxCustomAlgorithm;
begin
  Result := TsmxCustomAlgorithm(inherited Slaves[Index]);
end;

procedure TsmxCustomAlgorithmList.SetSlave(Index: Integer; Value: TsmxCustomAlgorithm);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomAlgorithmList.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxCustomAlgorithm;
end;

{ TsmxCustomRequest }

destructor TsmxCustomRequest.Destroy;
begin
  SetDatabase(nil);
  SetModifyRequest(mrInsert, nil);
  SetModifyRequest(mrUpdate, nil);
  SetModifyRequest(mrDelete, nil);
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
    ModifyPerformances[mrDelete] := TsmxCustomRequest(Source).ModifyPerformances[mrDelete];
    ModifyPerformances[mrInsert] := TsmxCustomRequest(Source).ModifyPerformances[mrInsert];
    ModifyPerformances[mrUpdate] := TsmxCustomRequest(Source).ModifyPerformances[mrUpdate];
    ModifyRequests[mrDelete] := TsmxCustomRequest(Source).ModifyRequests[mrDelete];
    ModifyRequests[mrInsert] := TsmxCustomRequest(Source).ModifyRequests[mrInsert];
    ModifyRequests[mrUpdate] := TsmxCustomRequest(Source).ModifyRequests[mrUpdate];
    OperationMode := TsmxCustomRequest(Source).OperationMode;
    PerformanceMode := TsmxCustomRequest(Source).PerformanceMode;
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
  FCurDataSetIntf := FDeleteRequestIntf;
  FCurPerformanceMode := FDeletePerformance;
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
  FCurPerformanceMode := FPerformanceMode;
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
  FCurDataSetIntf := FInsertRequestIntf;
  FCurPerformanceMode := FInsertPerformance;
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
    if (FOperationMode = omAutomatic) and (FCurDataSetIntf = DataSet) then
    begin
      RefreshParams;
      Execute;
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

procedure TsmxCustomRequest.DoRefreshParams;
begin
  if Assigned(FOnRefreshParams) then
    FOnRefreshParams(Self);
end;

procedure TsmxCustomRequest.InternalRefreshParams;
var
  i: integer;
begin
  if Assigned(CurDataSet) then
    for i := 0 to CurDataSet.ParamCount - 1 do
      CurDataSet.Params[i].Value := Variants.Null;
end;

procedure TsmxCustomRequest.RefreshParams;
begin
  FCurDataSetIntf := DataSet;
  FCurPerformanceMode := FPerformanceMode;
  if not FIsManualRefreshParams then
  begin
    InternalRefreshParams;
    DoRefreshParams;
  end;
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
  FCurDataSetIntf := FUpdateRequestIntf;
  FCurPerformanceMode := FUpdatePerformance;
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

procedure TsmxCustomRequest.SetDataSet(const Value: IsmxDataSet);
begin
  FDataSetIntf := Value;
end;

function TsmxCustomRequest.GetModifyPerformance(Modify: TsmxModifyRequest): TsmxPerformanceMode;
begin
  Result := pmOpen;
  case Modify of
    mrDelete: Result := FDeletePerformance;
    mrInsert: Result := FInsertPerformance;
    mrUpdate: Result := FUpdatePerformance;
  end;
end;

procedure TsmxCustomRequest.SetModifyPerformance(Modify: TsmxModifyRequest; Value: TsmxPerformanceMode);
begin
  case Modify of
    mrDelete: FDeletePerformance := Value;
    mrInsert: FInsertPerformance := Value;
    mrUpdate: FUpdatePerformance := Value;
  end;
end;

function TsmxCustomRequest.GetModifyRequest(Modify: TsmxModifyRequest): IsmxDataSet;
begin
  Result := nil;
  case Modify of
    mrDelete: Result := FDeleteRequestIntf;
    mrInsert: Result := FInsertRequestIntf;
    mrUpdate: Result := FUpdateRequestIntf;
  end;
end;

procedure TsmxCustomRequest.SetModifyRequest(Modify: TsmxModifyRequest; const Value: IsmxDataSet);
begin
  case Modify of
    mrDelete: FDeleteRequestIntf := Value;
    mrInsert: FInsertRequestIntf := Value;
    mrUpdate: FUpdateRequestIntf := Value;
  end;
end;

procedure TsmxCustomRequest.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = RequestCell) and (Operation = opRemove) then
    RequestCell := nil;
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

procedure TsmxCustomRequest.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

procedure TsmxCustomRequest.SetDatabaseManager(const Value: IsmxDatabaseManager);
var
  Connection: IsmxConnection;
begin
  if Assigned(DatabaseManager) and (FDatabaseName <> '') then
    Database := nil;
  inherited SetDatabaseManager(Value);
  if Assigned(DatabaseManager) and (FDatabaseName <> '') then
  begin
    Connection := DatabaseManager.FindByName(FDatabaseName);
    if Assigned(Connection) then
      Database := Connection.Database;
  end;
end;

procedure TsmxCustomRequest.SetDatabaseName(const Value: String);
var
  Connection: IsmxConnection;
begin
  if Assigned(DatabaseManager) and (FDatabaseName <> '') then
    Database := nil;
  FDatabaseName := Value;
  if Assigned(DatabaseManager) and (FDatabaseName <> '') then
  begin
    Connection := DatabaseManager.FindByName(FDatabaseName);
    if Assigned(Connection) then
      Database := Connection.Database;
  end;
end;

procedure TsmxCustomRequest.SetIsManualRefreshParams(Value: Boolean);
begin
  FIsManualRefreshParams := Value;
end;

procedure TsmxCustomRequest.SetOperationMode(Value: TsmxOperationMode);
begin
  FOperationMode := Value;
end;

procedure TsmxCustomRequest.SetPerformanceMode(Value: TsmxPerformanceMode);
begin
  FPerformanceMode := Value;
end;

procedure TsmxCustomRequest.SetRequestCell(Value: TsmxBaseCell);
begin
  FRequestCell := Value;
  if Assigned(FRequestCell) then
    FRequestCell.FreeNotification(Self);
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

function TsmxCustomRequestList.GetSlaveClass: TsmxOwnerCellClass;
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
    ColumnValue := TsmxCustomColumn(Source).ColumnValue;
    FieldName := TsmxCustomColumn(Source).FieldName;
    HeaderAlignment := TsmxCustomColumn(Source).HeaderAlignment;
    HeaderColor := TsmxCustomColumn(Source).HeaderColor;
    HeaderFont := TsmxCustomColumn(Source).HeaderFont;
    HeaderCaption := TsmxCustomColumn(Source).HeaderCaption;
  end;
end;

procedure TsmxCustomColumn.DoSnapHeader;
begin
  if Assigned(FOnSnapHeader) then
    FOnSnapHeader(Self);
end;

procedure TsmxCustomColumn.InternalSnapHeader;
begin
end;

procedure TsmxCustomColumn.SnapHeader;
begin
  InternalSnapHeader;
  DoSnapHeader;
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

function TsmxCustomColumn.GetColumnValue: Variant;
begin
  Result := Variants.Null;
end;

procedure TsmxCustomColumn.SetColumnValue(const Value: Variant);
begin
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

{ TsmxCustomGrid }

function TsmxCustomGrid.AddSlave: TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(inherited AddSlave);
end;

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

procedure TsmxCustomGrid.DoRefresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TsmxCustomGrid.InternalRefresh;
begin
  if Assigned(FRequest) then
    FRequest.RequestCell := Self;
end;

procedure TsmxCustomGrid.Refresh;
begin
  InternalRefresh;
  DoRefresh;
end;

function TsmxCustomGrid.GetSlave(Index: Integer): TsmxCustomColumn;
begin
  Result := TsmxCustomColumn(inherited Slaves[Index]);
end;

procedure TsmxCustomGrid.SetSlave(Index: Integer; Value: TsmxCustomColumn);
begin
  inherited Slaves[Index] := Value;
end;

function TsmxCustomGrid.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxCustomColumn;
end;

procedure TsmxCustomGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Request) and (Operation = opRemove) then
    Request := nil;
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

procedure TsmxCustomFilterDesk.DoApply;
begin
  if Assigned(FOnApply) then
    FOnApply(Self);
end;

procedure TsmxCustomFilterDesk.InternalApply;
begin
  if Assigned(FRequest) then
    FRequest.RequestCell := Self;
end;

procedure TsmxCustomFilterDesk.Apply;
begin
  InternalApply;
  DoApply;
end;

procedure TsmxCustomFilterDesk.DoPrepare;
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
end;

procedure TsmxCustomFilterDesk.InternalPrepare;
begin
  if Assigned(FRequest) then
    FRequest.RequestCell := Self;
end;

procedure TsmxCustomFilterDesk.Prepare;
begin
  InternalPrepare;
  DoPrepare;
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

function TsmxCustomFilter.GetHeaderCaption: String;
begin
  Result := '';
end;

procedure TsmxCustomFilter.SetHeaderCaption(const Value: String);
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

function TsmxCustomFilterDesk.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxCustomFilter;
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

procedure TsmxCustomSection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FilterDesk then
      FilterDesk := nil else
    if AComponent = Grid then
      Grid := nil;
  end;
end;

procedure TsmxCustomSection.SetCellOwner(Value: TsmxCustomPage);
begin
  inherited CellOwner := Value;
end;

procedure TsmxCustomSection.SetFilterDesk(Value: TsmxCustomFilterDesk);
begin
  FFilterDesk := Value;
  if Assigned(FFilterDesk) then
    FFilterDesk.FreeNotification(Self);
end;

procedure TsmxCustomSection.SetGrid(Value: TsmxCustomGrid);
begin
  FGrid := Value;
  if Assigned(FGrid) then
    FGrid.FreeNotification(Self);
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

function TsmxCustomPageManager.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomMenuItem.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomMainMenu.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomPopupMenu.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomPopupList.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomToolBoard.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomControlBoard.GetSlaveClass: TsmxOwnerCellClass;
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

function TsmxCustomStatusBoard.GetSlaveClass: TsmxOwnerCellClass;
begin
  Result := TsmxCustomStatusItem;
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

procedure TsmxCustomForm.Close;
begin
end;

procedure TsmxCustomForm.Show;
begin
end;

procedure TsmxCustomForm.FreeForm;
begin
  Free;
end;

function TsmxCustomForm.GetCfgID: Integer;
begin
  Result := FCfgID;
end;

{function TsmxCustomForm.GetIsApplicationForm: Boolean;
begin
  Result := False;
end;}

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

procedure TsmxCustomForm.SetFormManager(const Value: IsmxFormManager);
begin
  if Assigned(FormManager) then
    FormManager.RemoveForm(Self as IsmxForm);
  inherited SetFormManager(Value);
  if Assigned(FormManager) then
    FormManager.InsertForm(Self as IsmxForm);
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

function TsmxCustomForm.ShowModal: TModalResult;
begin
  Result := mrNone;
end;

{ TsmxStateCfg }

procedure TsmxStateCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateCfg then
  begin
    IntfID := TsmxStateCfg(Source).IntfID;
    RecID := TsmxStateCfg(Source).RecID;
  end;
end;

procedure TsmxStateCfg.Load;
var
  Key, Value: Variant;
  KeySense: TsmxBaseSense;
begin
  if not Assigned(FSelectRequest)
      or not ((FRecID <> 0) or ((FCfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'load'], FCfgID);
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := fsKey;
  end;
  if smxDBFuncs.GetValueByKey(FSelectRequest.DataSet, Key, Value,
      FSelectRequest.PerformanceMode, KeySense) then
  begin
    if smxDBFuncs.LocateByKey(FSelectRequest.DataSet, Key, KeySense) then
    begin
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value,
          FSelectRequest.PerformanceMode, fsKey) then
        FRecID := Value else
        FRecID := 0;
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value,
          FSelectRequest.PerformanceMode) then
        XMLDoc.XML.Text := Value else
        XMLDoc.XML.Text := '';
      try
        XMLDoc.Active := True;
      except
        on E: Exception do
          raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
            [ClassName, FCfgID, 'load'], FCfgID, E.Message);
      end;
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
      or not ((FRecID <> 0) or ((FCfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);
  if FRecID = 0 then
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
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := fsKey;
  end;
  if smxDBFuncs.SetValueByKey(Request, Key, XMLDoc.XML.Text, Performance, KeySense) then
    if smxDBFuncs.GetCurrentValue(Request, Value, Performance, fsKey) then
      FRecID := Value;
end;

procedure TsmxStateCfg.Read;
var
  Value: Variant;
  CurIntfID: Integer;
  XMLTextTemp: String;
begin
  inherited Read;
  XMLTextTemp := XMLDoc.XML.Text;
  if Assigned(FSelectRequest) and Assigned(FSelectRequest.DataSet) then
  begin
    FSelectRequest.DataSet.First;
    while not FSelectRequest.DataSet.Eof do
    begin
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value, FSelectRequest.PerformanceMode) then
        XMLDoc.XML.Text := Value else
        XMLDoc.XML.Text := '';
      if smxDBFuncs.GetCurrentValue(FSelectRequest.DataSet, Value, FSelectRequest.PerformanceMode, fsForeignKey) then
        CurIntfID := smxFuncs.GetSingleValue(Value, 0, 1) else
        CurIntfID := 0;
      try
        XMLDoc.Active := True;
        ReadIntf(RootNode, CurIntfID);
      except
        on E: Exception do
          raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
            [ClassName, FCfgID, 'read'], FCfgID, E.Message);
      end;
      FSelectRequest.DataSet.Next;
    end;
  end;
  XMLDoc.XML.Text := XMLTextTemp;
  try
    XMLDoc.Active := True;
    ReadIntf(RootNode, IntfID);
  except
    on E: Exception do
      raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
        [ClassName, FCfgID, 'read'], FCfgID, E.Message);
  end;
end;

procedure TsmxStateCfg.Write;
begin
  inherited Write;
  WriteIntf(RootNode, IntfID);
end;

procedure TsmxStateCfg.ReadIntf(const Node: IXMLNode; ID: Integer);
begin
end;

procedure TsmxStateCfg.WriteIntf(const Node: IXMLNode; ID: Integer);
begin
end;

procedure TsmxStateCfg.Remove;
var
  Request: IsmxDataSet;
  Performance: TsmxPerformanceMode;
  Key: Variant;
  KeySense: TsmxBaseSense;
begin
  if not Assigned(FSelectRequest)
      or not ((FRecID <> 0) or ((FCfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);
  Request := FSelectRequest.ModifyRequests[mrDelete];
  Performance := FSelectRequest.ModifyPerformances[mrDelete];
  if not Assigned(Request) then
    raise EsmxCellError.CreateByCfgID(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := fsKey;
  end;
  if smxDBFuncs.SetValueByKey(Request, Key, Variants.Null, Performance, KeySense) then
    FRecID := 0;
end;

procedure TsmxStateCfg.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

procedure TsmxStateCfg.SetRecID(Value: Integer);
begin
  FRecID := Value;
end;

end.
