unit smxCfgs;

interface

uses
  Classes, Controls, Graphics, smxClasses, {smxParams,} smxDBIntf, smxTypes;

type
  { TsmxColumnCfg }

  {TsmxCellFont = record
    Color: Integer;
    Name: String;
    Size: Integer;
    Style: TFontStyles;
  end;

  TsmxCellText = record
    Text: String;
    Align: TAlignment;
    Color: Integer;
    Font: TsmxCellFont;
  end;}

  TsmxColumnCfg = class(TsmxBaseCfg)
  private
    FColumnFieldName: String;
    FColumnText: TsmxCellText;
    FColumnTitle: TsmxCellText;
    //FColumnWidth: Integer;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    //constructor Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack); override;
    procedure Clear; override;
    //destructor Destroy; override;
    //procedure Initialize; override;

    property ColumnFieldName: String read FColumnFieldName write FColumnFieldName;
    property ColumnText: TsmxCellText read FColumnText write FColumnText;
    property ColumnTitle: TsmxCellText read FColumnTitle write FColumnTitle;
    //property ColumnWidth: Integer read FColumnWidth write FColumnWidth;
  end;

  { TsmxGridColumn }

  {TsmxGridColumn = class(TsmxKitItem)
  private
    FColumn: TsmxCellText;
    FFieldName: String;
    FTitle: TsmxCellText;
    FWidth: Integer;}
    {FColumnAlign: TAlignment;
    FColumnColor: Integer;
    FColumnFieldName: String;
    FColumnFont: TsmxCellFont;
    FColumnWidth: Integer;
    FTitleColor: Integer;
    FTitleText: TsmxCellText;}
  {public
    constructor Create(AKit: TsmxKit); override;

    property Column: TsmxCellText read FColumn write FColumn;
    property FieldName: String read FFieldName write FFieldName;
    property Title: TsmxCellText read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;}
    {property ColumnAlign: TAlignment read FColumnAlign write FColumnAlign;
    property ColumnColor: Integer read FColumnColor write FColumnColor;
    property ColumnFieldName: String read FColumnFieldName write FColumnFieldName;
    property ColumnFont: TsmxCellFont read FColumnFont write FColumnFont;
    property ColumnWidth: Integer read FColumnWidth write FColumnWidth;
    property TitleColor: Integer read FTitleColor write FTitleColor;
    property TitleText: TsmxCellText read FTitleText write FTitleText;}
  {end;}

  { TsmxGridColumns }

  {TsmxGridColumns = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxGridColumn;
    //procedure SetItem(Index: Integer; Value: TsmxGridColumn);
  public
    function Add: TsmxGridColumn;

    property Items[Index: Integer]: TsmxGridColumn read GetItem} {write SetItem}{; default;
  end;}

  { TsmxVisibleUnit }

  TsmxVisibleUnit = class(TsmxKitItem)
  private
    FCfgID: Integer;
    FUnitAlign: TAlign;
    FUnitEnable: Boolean;
    FUnitHeight: Integer;
    FUnitLeft: Integer;
    FUnitTop: Integer;
    FUnitVisible: Boolean;
    FUnitWidth: Integer;
  public
    constructor Create(AKit: TsmxKit); override;

    property CfgID: Integer read FCfgID write FCfgID;
    property UnitAlign: TAlign read FUnitAlign write FUnitAlign;
    property UnitEnable: Boolean read FUnitEnable write FUnitEnable;
    property UnitHeight: Integer read FUnitHeight write FUnitHeight;
    property UnitLeft: Integer read FUnitLeft write FUnitLeft;
    property UnitTop: Integer read FUnitTop write FUnitTop;
    property UnitVisible: Boolean read FUnitVisible write FUnitVisible;
    property UnitWidth: Integer read FUnitWidth write FUnitWidth;
  end;

  { TsmxVisibleUnits }

  TsmxVisibleUnits = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxVisibleUnit;
    //procedure SetItem(Index: Integer; Value: TsmxVisibleUnit);
  public
    function Add: TsmxVisibleUnit;

    property Items[Index: Integer]: TsmxVisibleUnit read GetItem {write SetItem}; default;
  end;

  { TsmxGridCfg }

  {TsmxOperationMode = (omManual, omAutomatic);

  TsmxRequestSetting = record
    ID: Integer;
    Mode: TsmxOperationMode;
  end;}

  TsmxGridCfg = class(TsmxBaseCfg)
  private
    FGridColLines: Boolean;
    FGridColumns: TsmxVisibleUnits;
    FGridRowLines: Boolean;
    FGridRowSelect: Boolean;
    FRequest: TsmxRequestSetting;  
    function GetGridColumns: TsmxVisibleUnits;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    //constructor Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack); override;
    destructor Destroy; override;
    procedure Clear; override;
    //procedure Initialize; override;

    property GridColLines: Boolean read FGridColLines write FGridColLines;
    property GridColumns: TsmxVisibleUnits read GetGridColumns;
    property GridRowLines: Boolean read FGridRowLines write FGridRowLines;
    property GridRowSelect: Boolean read FGridRowSelect write FGridRowSelect;
    property Request: TsmxRequestSetting read FRequest write FRequest;
  end;

  { TsmxLocationParam }

  {TsmxParamLocation = (plNowhere, plConst, plOut, plFilterPanel, plFormParams,
    plParentFormDataSet, plParentFormFilterPanel);}

  TsmxLocationParam = class(TsmxKitItem)
  private
    FParamLocation: TsmxParamLocation;
    FParamName: String;
    FParamDefValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamName: String read FParamName write FParamName;
    property ParamDefValue: Variant read FParamDefValue write FParamDefValue;
  end;

  { TsmxLocationParams }

  TsmxLocationParams = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxLocationParam;
    //procedure SetItem(Index: Integer; Value: TsmxLocationParam);
  public
    function Add: TsmxLocationParam;
    function FindByName(AParamName: String): TsmxLocationParam;

    property Items[Index: Integer]: TsmxLocationParam read GetItem {write SetItem}; default;
  end;

  { TsmxRequestField }

  //TsmxFieldSense = (fsGeneral, fsKey, fsValue, fsResult, fsMsg);

  TsmxRequestField = class(TsmxKitItem)
  private
    FFieldFormat: String;
    FFieldName: String;
    FFieldSense: TsmxFieldSense;
  public
    constructor Create(AKit: TsmxKit); override;

    property FieldFormat: String read FFieldFormat write FFieldFormat;
    property FieldName: String read FFieldName write FFieldName;
    property FieldSense: TsmxFieldSense read FFieldSense write FFieldSense;
  end;

  { TsmxRequestFields }

  TsmxRequestFields = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxRequestField;
    //procedure SetItem(Index: Integer; Value: TsmxRequestField);
  public
    function Add: TsmxRequestField;
    function FindByName(AFieldName: String): TsmxRequestField;

    property Items[Index: Integer]: TsmxRequestField read GetItem {write SetItem}; default;
  end;

  { TsmxRequestCfg }

  TsmxRequestCfg = class(TsmxBaseCfg)
  private
    FDataSetType: TsmxDataSetType;
    FMode: TsmxReturnType;
    FRequestFields: TsmxRequestFields;
    FRequestParams: TsmxLocationParams;
    FSQLText: String;
    function GetRequestFields: TsmxRequestFields;
    function GetRequestParams: TsmxLocationParams;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    //constructor Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack); override;
    destructor Destroy; override;
    procedure Clear; override;
    //procedure Initialize; override;
    procedure Refresh; virtual;

    property DataSetType: TsmxDataSetType read FDataSetType write FDataSetType;
    property Mode: TsmxReturnType read FMode write FMode;
    property RequestFields: TsmxRequestFields read GetRequestFields;
    property RequestParams: TsmxLocationParams read GetRequestParams;
    property SQLText: String read FSQLText write FSQLText;
  end;

  { TsmxPageCfg }
  
  {TsmxPositionSize = record
    Left, Top, Height, Width: Integer;
  end;

  TsmxControlCellSetting = record
    ID: Integer;
    Align: TAlign;
    Enable, Visible: Boolean;
    Height, Left, Top, Width: Integer;
  end;}

  TsmxPageCfg = class(TsmxBaseCfg)
  private
    FFilterPanel: TsmxControlCellSetting;
    FGrid: TsmxControlCellSetting;
    FPageCaption: String;
    FPageImageIndex: Integer;
    //FRequest: TsmxRequestSetting;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    procedure Clear; override;
    //procedure Initialize; override;

    property FilterPanel: TsmxControlCellSetting read FFilterPanel write FFilterPanel;
    property Grid: TsmxControlCellSetting read FGrid write FGrid;
    property PageCaption: String read FPageCaption write FPageCaption;
    property PageImageIndex: Integer read FPageImageIndex write FPageImageIndex;
    //property Request: TsmxRequestSetting read FRequest write FRequest;
  end;

  { TsmxPageManagerCfg }

  TsmxPageManagerCfg = class(TsmxBaseCfg)
  private
    FSheets: TsmxVisibleUnits;
    function GetSheets: TsmxVisibleUnits;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    //procedure Initialize; override;

    property Sheets: TsmxVisibleUnits read GetSheets;
  end;

  { TsmxStateUnit }

  {TsmxStateUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FUnitEnable: Boolean;
    FUnitVisible: Boolean;
  protected
    function GetItem(Index: Integer): TsmxStateUnit;
    function GetParent: TsmxStateUnit;
  public
    constructor Create(AHKit: TsmxHKit); override;
    function Add: TsmxStateUnit;
    function FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxStateUnit;

    property CfgID: Integer read FCfgID write FCfgID;
    property Items[Index: Integer]: TsmxStateUnit read GetItem; default;
    property Parent: TsmxStateUnit read GetParent;
    property UnitEnable: Boolean read FUnitEnable write FUnitEnable;
    property UnitVisible: Boolean read FUnitVisible write FUnitVisible;
  end;}

  { TsmxStateUnits }

  {TsmxStateUnits = class(TsmxHKit)
  protected
    function GetRoot: TsmxStateUnit;
  public
    property Root: TsmxStateUnit read GetRoot;
  end;}

  { TsmxFormState }

  TsmxFormState = class(TsmxKitItem)
  private
    FID: Integer;
    FStateUnits: TsmxStateUnits;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;

    property ID: Integer read FID write FID;
    property StateUnits: TsmxStateUnits read FStateUnits;
  end;

  { TsmxFormStates }

  TsmxFormStates = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxFormState;
  public
    function Add: TsmxFormState;
    function FindByID(AID: Integer): TsmxFormState;

    property Items[Index: Integer]: TsmxFormState read GetItem {write SetItem}; default;
  end;

  { TsmxFormCfg }

  {TsmxAlgorithmListSetting = record
    ID: Integer;
    IsCreateMenuItem: Boolean;
  end;}

  TsmxFormCfg = class(TsmxBaseCfg)
  private
    FAlgorithmList: TsmxAlgorithmListSetting;
    FControlBar: TsmxControlCellSetting;
    FFormCaption: String;
    FFormImageIndex: Integer;
    FFormPositionSize: TsmxPositionSize;
    FFormStates: TsmxFormStates;
    FMainMenu: TsmxControlCellSetting;
    FPageManager: TsmxControlCellSetting;
    FStateRequest: TsmxRequestSetting;
    function GetFormStates: TsmxFormStates;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    //procedure Initialize; override;

    property AlgorithmList: TsmxAlgorithmListSetting read FAlgorithmList write FAlgorithmList;
    property ControlBar: TsmxControlCellSetting read FControlBar write FControlBar;
    property FormCaption: String read FFormCaption write FFormCaption;
    property FormImageIndex: Integer read FFormImageIndex write FFormImageIndex;
    property FormPositionSize: TsmxPositionSize read FFormPositionSize write FFormPositionSize;
    property FormStates: TsmxFormStates read GetFormStates;
    property MainMenu: TsmxControlCellSetting read FMainMenu write FMainMenu;
    property PageManager: TsmxControlCellSetting read FPageManager write FPageManager;
    property StateRequest: TsmxRequestSetting read FStateRequest write FStateRequest;
  end;

  { TsmxFilterCfg }

  {TsmxAlgorithmSetting = record
    ID: Integer;
    Caption: String;
    Enable: Boolean;
    HotKey: Integer;
    Params: Variant;
    Visible: Boolean;
  end;}

  TsmxFilterCfg = class(TsmxBaseCfg)
  private
    FAlgorithm: TsmxAlgorithmSetting;
    //FAlgorithmID: Integer;
    //FAlgorithmParams: Variant;
    //FAlgorithmList: TsmxAlgorithmListSetting;
    FDisplayFormat: String;
    //FFilterDefValue: Variant;
    FFilterFont: TsmxCellFont;
    FFilterHeader: TsmxCellText;
    FFilterName: String;
    //FRequest: TsmxRequestSetting;
    FValueFormat: String;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    //constructor Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack); override;
    procedure Clear; override;
    //destructor Destroy; override;
    //procedure Initialize; override;

    property Algorithm: TsmxAlgorithmSetting read FAlgorithm write FAlgorithm;
    //property AlgorithmID: Integer read FAlgorithmID write FAlgorithmID;
    //property AlgorithmParams: Variant read FAlgorithmParams write FAlgorithmParams;
    //property AlgorithmList: TsmxAlgorithmListSetting read FAlgorithmList write FAlgorithmList;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    //property FilterDefValue: Variant read FFilterDefValue write FFilterDefValue;
    property FilterFont: TsmxCellFont read FFilterFont write FFilterFont;
    property FilterHeader: TsmxCellText read FFilterHeader write FFilterHeader;
    property FilterName: String read FFilterName write FFilterName;
    //property Request: TsmxRequestSetting read FRequest write FRequest;
    property ValueFormat: String read FValueFormat write FValueFormat;
  end;

  { TsmxFilterPanelCfg }

  TsmxFilterPanelCfg = class(TsmxBaseCfg)
  private
    //FAlgorithmList: TsmxAlgorithmListSetting;
    FApplyRequest: TsmxRequestSetting;
    FFilters: TsmxVisibleUnits;
    FPrepareRequest: TsmxRequestSetting;
    function GetFilters: TsmxVisibleUnits;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    //property AlgorithmList: TsmxAlgorithmListSetting read FAlgorithmList write FAlgorithmList;
    property ApplyRequest: TsmxRequestSetting read FApplyRequest write FApplyRequest;
    property Filters: TsmxVisibleUnits read GetFilters;
    property PrepareRequest: TsmxRequestSetting read FPrepareRequest write FPrepareRequest;
  end;

  { TsmxMenuItemCfg }

  TsmxMenuItemCfg = class(TsmxBaseCfg)
  private
    FItemCaption: String;
    FItemImageIndex: Integer;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    procedure Clear; override;
    //procedure Initialize; override;

    property ItemCaption: String read FItemCaption write FItemCaption;
    property ItemImageIndex: Integer read FItemImageIndex write FItemImageIndex;
  end;

  { TsmxMenuUnit }

  {TsmxMenuUnit = class(TsmxKitItem)
  private
    FID: Integer;
    FMMItemEnable: Boolean;
    FMMItemVisible: Boolean;
  public
    constructor Create(AKit: TsmxKit); override;

    property ID: Integer read FID write FID;
    property MMItemEnable: Boolean read FMMItemEnable write FMMItemEnable;
    property MMItemVisible: Boolean read FMMItemVisible write FMMItemVisible;
  end;}

  { TsmxMenuUnits }

  {TsmxMenuUnits = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxMenuUnit;
    //procedure SetItem(Index: Integer; Value: TsmxMenuUnit);
  public
    function Add: TsmxMenuUnit;
    //function Find(ParamName: String): TsmxLocationParam;

    property Items[Index: Integer]: TsmxMenuUnit read GetItem} {write SetItem}{; default;
  end;}

  { TsmxHVisibleUnit }

  TsmxHVisibleUnit = class(TsmxHKitItem)
  private
    FCfgID: Integer;
    FUnitAlign: TAlign;
    FUnitEnable: Boolean;
    FUnitHeight: Integer;
    FUnitLeft: Integer;
    FUnitTop: Integer;
    FUnitVisible: Boolean;
    FUnitWidth: Integer;
  protected
    function GetItem(Index: Integer): TsmxHVisibleUnit;
    function GetParent: TsmxHVisibleUnit;
  public
    constructor Create(AHKit: TsmxHKit); override; //(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass); override;
    function Add: TsmxHVisibleUnit;
    function FindByCfgID(ACfgID: Integer; AmongAll: Boolean = False): TsmxHVisibleUnit;

    property CfgID: Integer read FCfgID write FCfgID;
    property Items[Index: Integer]: TsmxHVisibleUnit read GetItem; default;
    property Parent: TsmxHVisibleUnit read GetParent;
    property UnitAlign: TAlign read FUnitAlign write FUnitAlign;
    property UnitEnable: Boolean read FUnitEnable write FUnitEnable;
    property UnitHeight: Integer read FUnitHeight write FUnitHeight;
    property UnitLeft: Integer read FUnitLeft write FUnitLeft;
    property UnitTop: Integer read FUnitTop write FUnitTop;
    property UnitVisible: Boolean read FUnitVisible write FUnitVisible;
    property UnitWidth: Integer read FUnitWidth write FUnitWidth;
  end;

  { TsmxHVisibleUnits }

  TsmxHVisibleUnits = class(TsmxHKit)
  protected
    function GetRoot: TsmxHVisibleUnit;
  public
    property Root: TsmxHVisibleUnit read GetRoot;
  end;

  { TsmxMenuUnit }

  {TsmxMenuUnit = class(TsmxHKitItem)
  private
    FUnitCaption: String;
    FUnitEnable: Boolean;
    FUnitImageIndex: Integer;
    FUnitName: String;
    FUnitVisible: Boolean;
  protected
    function GetItem(Index: Integer): TsmxMenuUnit;
  public
    constructor Create(AHKit: TsmxHKit); override; //(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass); override;
    function Add: TsmxMenuUnit;

    property Items[Index: Integer]: TsmxMenuUnit read GetItem; default;
    property UnitCaption: String read FUnitCaption write FUnitCaption;
    property UnitEnable: Boolean read FUnitEnable write FUnitEnable;
    property UnitImageIndex: Integer read FUnitImageIndex write FUnitImageIndex;
    property UnitName: String read FUnitName write FUnitName;
    property UnitVisible: Boolean read FUnitVisible write FUnitVisible;
  end;}

  { TsmxMenuUnits }

  {TsmxMenuUnits = class(TsmxHKit)
  protected
    function GetRoot: TsmxMenuUnit;
  public
    property Root: TsmxMenuUnit read GetRoot;
  end;}

  { TsmxMainMenuCfg }

  TsmxMainMenuCfg = class(TsmxBaseCfg)
  private
    FMenuUnits: TsmxHVisibleUnits; //TsmxMenuUnits;
    function GetMenuUnits: TsmxHVisibleUnits; //TsmxMenuUnits;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property MenuUnits: TsmxHVisibleUnits read GetMenuUnits; //TsmxMenuUnits read GetMenuUnits;
  end;
  
  { TsmxLibAlgorithmCfg }

  TsmxLibAlgorithmCfg = class(TsmxBaseCfg)
  private
    FAlgCaption: String;
    FAlgHotKey: Integer;
    FAlgImageIndex: Integer;
    FAlgLibrary: String;
    FAlgParams: TsmxLocationParams;
    FAlgProcedure: String;
    function GetAlgParams: TsmxLocationParams;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
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
  end;

  { TsmxAlgorithmItem }

  TsmxAlgorithmItem = class(TsmxKitItem)
  private
    FCfgID: Integer;
    //FAlgorithmButton: Boolean;
    FAlgorithmCaption: String;
    FAlgorithmEnable: Boolean;
    FAlgorithmHotKey: Integer;
    FAlgorithmMenuItemID: Integer;
    FAlgorithmToolBarID: Integer;
    //FAlgorithmParams: Variant;
    FAlgorithmVisible: Boolean;
  public
    constructor Create(AKit: TsmxKit); override;

    property CfgID: Integer read FCfgID write FCfgID;
    //property AlgorithmButton: Boolean read FAlgorithmButton write FAlgorithmButton;
    property AlgorithmCaption: String read FAlgorithmCaption write FAlgorithmCaption;
    property AlgorithmEnable: Boolean read FAlgorithmEnable write FAlgorithmEnable;
    property AlgorithmHotKey: Integer read FAlgorithmHotKey write FAlgorithmHotKey;
    property AlgorithmMenuItemID: Integer read FAlgorithmMenuItemID write FAlgorithmMenuItemID;
    property AlgorithmToolBarID: Integer read FAlgorithmToolBarID write FAlgorithmToolBarID;
    //property AlgorithmParams: Variant read FAlgorithmParams write FAlgorithmParams;
    property AlgorithmVisible: Boolean read FAlgorithmVisible write FAlgorithmVisible;
  end;

  { TsmxAlgorithmItems }

  TsmxAlgorithmItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxAlgorithmItem;
  public
    function Add: TsmxAlgorithmItem;
    function FindByCfgID(ACfgID: Integer): TsmxAlgorithmItem;

    property Items[Index: Integer]: TsmxAlgorithmItem read GetItem; default;
  end;

  { TsmxAlgorithmListCfg }

  TsmxAlgorithmListCfg = class(TsmxBaseCfg)
  private
    FAlgorithmItems: TsmxAlgorithmItems;
    function GetAlgorithmItems: TsmxAlgorithmItems;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property AlgorithmItems: TsmxAlgorithmItems read GetAlgorithmItems;
  end;

  { TsmxToolBarCfg }

  TsmxToolBarCfg = class(TsmxBaseCfg)
  private
    FBarFlat: Boolean;
    FBarShowCaptions: Boolean;
    FBarShowHint: Boolean;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    procedure Clear; override;

    property BarFlat: Boolean read FBarFlat write FBarFlat;
    property BarShowCaptions: Boolean read FBarShowCaptions write FBarShowCaptions;
    property BarShowHint: Boolean read FBarShowHint write FBarShowHint;
  end;

  { TsmxControlBarCfg }

  TsmxControlBarCfg = class(TsmxBaseCfg)
  private
    FBarUnits: TsmxVisibleUnits;
    function GetBarUnits: TsmxVisibleUnits;
  protected
    procedure ReadCell; override;
    procedure WriteCell; override;
  public
    procedure Clear; override;

    property BarUnits: TsmxVisibleUnits read GetBarUnits;
  end;
  
implementation

uses
  XMLIntf, SysUtils, Variants, smxFuncs, smxConsts;

{ TsmxColumnCfg }

procedure TsmxColumnCfg.Clear;
begin
  FColumnFieldName := '';
  with FColumnText do
  begin
    Text := '';
    Align := taLeftJustify;
    Color := Integer(clWindow);
    with Font do
    begin
      Color := Integer(clWindowText);
      Name := 'MS Sans Serif';
      Size := 8;
      Style := [];
    end;
  end;
  with FColumnTitle do
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
end;

{procedure TsmxColumnCfg.Initialize;
begin
  FColumnFieldName := '';
  //FColumnWidth := 50;
  with FColumnText do
  begin
    Text := '';
    Align := taLeftJustify;
    Color := Integer(clWindow);
    with Font do
    begin
      Color := Integer(clWindowText);
      Name := 'MS Sans Serif';
      Size := 8;
      Style := [];
    end;
  end;
  with FColumnTitle do
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
  inherited Initialize;
end;}

procedure TsmxColumnCfg.ReadCell;
var r, n, n2, n3: IXMLNode;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('column');
  if Assigned(n) then
  begin
    ColumnFieldName := n.Attributes['fieldname'];
    //ColumnWidth := n.Attributes['width'];
    with ColumnText do
    begin
      Align := n.Attributes['align'];
      Color := n.Attributes['color'];
      n2 := n.ChildNodes.FindNode('font');
      if Assigned(n2) then
        with Font do
        begin
          Color := n2.Attributes['color'];
          Name := n2.Attributes['name'];
          Size := n2.Attributes['size'];
          if n2.Attributes['bold'] then Style := Style + [fsBold];
          if n2.Attributes['italic'] then Style := Style + [fsItalic];
          if n2.Attributes['underline'] then Style := Style + [fsUnderline];
          if n2.Attributes['strikeout'] then Style := Style + [fsStrikeOut];
        end;
    end;
    n2 := n.ChildNodes.FindNode('title');
    if Assigned(n2) then
      with ColumnTitle do
      begin
        Text := n2.Attributes['text'];
        Align := n2.Attributes['align'];
        Color := n2.Attributes['color'];
        n3 := n2.ChildNodes.FindNode('font');
        if Assigned(n3) then
          with Font do
          begin
            Color := n3.Attributes['color'];
            Name := n3.Attributes['name'];
            Size := n3.Attributes['size'];
            if n3.Attributes['bold'] then Style := Style + [fsBold];
            if n3.Attributes['italic'] then Style := Style + [fsItalic];
            if n3.Attributes['underline'] then Style := Style + [fsUnderline];
            if n3.Attributes['strikeout'] then Style := Style + [fsStrikeOut];
          end;
      end;
  end;
end;

procedure TsmxColumnCfg.WriteCell;
var r, n, n2, n3: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('column');
  n.Attributes['fieldname'] := ColumnFieldName;
  with ColumnText do
  begin
    n.Attributes['align'] := Align;
    n.Attributes['color'] := Color;
    //n.Attributes['width'] := ColumnWidth;
    n2 := n.AddChild('font');
    with Font do
    begin
      n2.Attributes['color'] := Color;
      n2.Attributes['name'] := Name;
      n2.Attributes['size'] := Size;
      n2.Attributes['bold'] := fsBold in Style;
      n2.Attributes['italic'] := fsItalic in Style;
      n2.Attributes['underline'] := fsUnderline in Style;
      n2.Attributes['strikeout'] := fsStrikeOut in Style;
    end;
  end;

  n2 := n.AddChild('title');
  with ColumnTitle do
  begin
    n2.Attributes['text'] := Text;
    n2.Attributes['align'] := Align;
    n2.Attributes['color'] := Color;
    n3 := n2.AddChild('font');
    with Font do
    begin
      n3.Attributes['color'] := Color;
      n3.Attributes['name'] := Name;
      n3.Attributes['size'] := Size;
      n3.Attributes['bold'] := fsBold in Style;
      n3.Attributes['italic'] := fsItalic in Style;
      n3.Attributes['underline'] := fsUnderline in Style;
      n3.Attributes['strikeout'] := fsStrikeOut in Style;
    end;
  end;
end;  

{ TsmxGridColumn }

{constructor TsmxGridColumn.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FColumn.Align := taLeftJustify;
  FColumn.Color := Integer(clWindow);
  FColumn.Text := '';
  FColumn.Font.Color := Integer(clWindowText);
  FColumn.Font.Name := 'MS Sans Serif';
  FColumn.Font.Size := 8;
  FColumn.Font.Style := [];
  FFieldName := '';
  FWidth := 50;
  FTitle.Color := Integer(clBtnFace);
  FTitle.Text := '';
  FTitle.Align := taLeftJustify;
  FTitle.Font.Color := Integer(clWindowText);
  FTitle.Font.Name := 'MS Sans Serif';
  FTitle.Font.Size := 8;
  FTitle.Font.Style := [];}

  {FColumnAlign := taLeftJustify;
  FColumnColor := Integer(clWindow);
  FColumnFieldName := '';
  FColumnFont.Color := Integer(clWindowText);
  FColumnFont.Name := 'MS Sans Serif';
  FColumnFont.Size := 8;
  FColumnFont.Style := [];
  FColumnWidth := 50;
  FTitleColor := Integer(clBtnFace);
  FTitleText.Text := '';
  FTitleText.Align := taLeftJustify;
  FTitleText.Font.Color := Integer(clWindowText);
  FTitleText.Font.Name := 'MS Sans Serif';
  FTitleText.Font.Size := 8;
  FTitleText.Font.Style := [];}
{end;}

{ TsmxGridColumns }

{function TsmxGridColumns.Add: TsmxGridColumn;
begin
  Result := TsmxGridColumn(inherited Add);
end;

function TsmxGridColumns.GetItem(Index: Integer): TsmxGridColumn;
begin
  Result := TsmxGridColumn(inherited Items[Index]);
end;}

{procedure TsmxGridColumns.SetItem(Index: Integer; Value: TsmxGridColumn);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxVisibleUnit }

constructor TsmxVisibleUnit.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FCfgID := 0;
  FUnitAlign := alClient;
  FUnitEnable := True;
  FUnitHeight := 0;
  FUnitLeft := 0;
  FUnitTop := 0;
  FUnitVisible := True;
  FUnitWidth := 0;
end;

{ TsmxVisibleUnits }

function TsmxVisibleUnits.Add: TsmxVisibleUnit;
begin
  Result := TsmxVisibleUnit(inherited Add);
end;

function TsmxVisibleUnits.GetItem(Index: Integer): TsmxVisibleUnit;
begin
  Result := TsmxVisibleUnit(inherited Items[Index]);
end;

{procedure TsmxVisibleUnits.SetItem(Index: Integer; Value: TsmxVisibleUnit);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxGridCfg }

{constructor TsmxGridCfg.Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack);
begin
  inherited Create(AOwner, AID, ACall);
  FIDRequest := 0;
end;}

destructor TsmxGridCfg.Destroy;
begin
  if Assigned(FGridColumns) then
    FGridColumns.Free;
  inherited Destroy;
end;

procedure TsmxGridCfg.Clear;
begin
  //FRequestID := 0;
  GridColLines := True;
  GridRowLines := True;
  GridRowSelect := False;
  with Request do
  begin
    CfgID := 0;
    Mode := omManual;
  end;
  GridColumns.Clear;
end;

function TsmxGridCfg.GetGridColumns: TsmxVisibleUnits;
begin
  if not Assigned(FGridColumns) then
    FGridColumns := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FGridColumns;
end;

{procedure TsmxGridCfg.Initialize;
begin
  FDefRequestID := 0;
  inherited Initialize;
end;}

procedure TsmxGridCfg.ReadCell;
var r, n{, n2, n3}: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('grid');
  if Assigned(n) then
  begin
    GridColLines := n.Attributes['collines'];
    GridRowLines := n.Attributes['rowlines'];
    GridRowSelect := n.Attributes['rowselect'];
  end;

  n := r.ChildNodes.FindNode('request');
  if Assigned(n) then
    with Request do
    begin
      CfgID := n.Attributes['id'];
      Mode := n.Attributes['mode'];
    end;

  n := r.ChildNodes.FindNode('columns');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'column' then
        with GridColumns.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['id'];
          UnitAlign := n.ChildNodes[i].Attributes['align'];
          UnitEnable := n.ChildNodes[i].Attributes['enable'];
          UnitHeight := n.ChildNodes[i].Attributes['height'];
          UnitLeft := n.ChildNodes[i].Attributes['left'];
          UnitTop := n.ChildNodes[i].Attributes['top'];
          UnitVisible := n.ChildNodes[i].Attributes['visible'];
          UnitWidth := n.ChildNodes[i].Attributes['width'];

          {ColumnFieldName := n.ChildNodes[i].Attributes['fieldname'];
          ColumnAlign := n.ChildNodes[i].Attributes['align'];
          ColumnColor := n.ChildNodes[i].Attributes['color'];
          ColumnWidth := n.ChildNodes[i].Attributes['width'];}
          {FieldName := n.ChildNodes[i].Attributes['fieldname'];
          Width := n.ChildNodes[i].Attributes['width'];
          with Column do
          begin
            //Text := n.ChildNodes[i].Attributes['fieldname'];
            Align := n.ChildNodes[i].Attributes['align'];
            Color := n.ChildNodes[i].Attributes['color'];
          end;
          n2 := n.ChildNodes[i].ChildNodes.FindNode('font');
          if Assigned(n2) then
            with Column.Font do
            begin
              Color := n2.Attributes['color'];
              Name := n2.Attributes['name'];
              Size := n2.Attributes['size'];
              if n2.Attributes['bold'] then Style := Style + [fsBold];
              if n2.Attributes['italic'] then Style := Style + [fsItalic];
              if n2.Attributes['underline'] then Style := Style + [fsUnderline];
              if n2.Attributes['strikeout'] then Style := Style + [fsStrikeOut];
            end;

          n2 := n.ChildNodes[i].ChildNodes.FindNode('title');
          if Assigned(n2) then
            with Title do
            begin
              Text := n2.Attributes['text'];
              Align := n2.Attributes['align'];
              Color := n2.Attributes['color'];
              n3 := n2.ChildNodes.FindNode('font');
              if Assigned(n3) then
                with Title.Font do
                begin
                  Color := n3.Attributes['color'];
                  Name := n3.Attributes['name'];
                  Size := n3.Attributes['size'];
                  if n3.Attributes['bold'] then Style := Style + [fsBold];
                  if n3.Attributes['italic'] then Style := Style + [fsItalic];
                  if n3.Attributes['underline'] then Style := Style + [fsUnderline];
                  if n3.Attributes['strikeout'] then Style := Style + [fsStrikeOut];
                end;
            end;}
        end;
  end;
end;

procedure TsmxGridCfg.WriteCell;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('grid');
  n.Attributes['collines'] := GridColLines;
  n.Attributes['rowlines'] := GridRowLines;
  n.Attributes['rowselect'] := GridRowSelect;

  n := r.AddChild('request');
  with Request do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['mode'] := Mode;
  end;

  n := r.AddChild('columns');
  for i := 0 to GridColumns.Count - 1 do
    with n.AddChild('column') do
    begin
      Attributes['id'] := GridColumns[i].CfgID;
      Attributes['align'] := GridColumns[i].UnitAlign;
      Attributes['enable'] := GridColumns[i].UnitEnable;
      Attributes['height'] := GridColumns[i].UnitHeight;
      Attributes['left'] := GridColumns[i].UnitLeft;
      Attributes['top'] := GridColumns[i].UnitTop;
      Attributes['visible'] := GridColumns[i].UnitVisible;
      Attributes['width'] := GridColumns[i].UnitWidth;

      {Attributes['fieldname'] := GridColumns[i].ColumnFieldName;
      Attributes['align'] := GridColumns[i].ColumnAlign;
      Attributes['color'] := GridColumns[i].ColumnColor ;
      Attributes['width'] := GridColumns[i].ColumnWidth;}
      //Attributes['fieldname'] := GridColumns[i].Column.Text;
      {Attributes['fieldname'] := GridColumns[i].FieldName;
      Attributes['align'] := GridColumns[i].Column.Align;
      Attributes['color'] := GridColumns[i].Column.Color ;
      Attributes['width'] := GridColumns[i].Width;
      with AddChild('font') do
      begin
        Attributes['color'] := GridColumns[i].Column.Font.Color;
        Attributes['name'] := GridColumns[i].Column.Font.Name;
        Attributes['size'] := GridColumns[i].Column.Font.Size;
        Attributes['bold'] := fsBold in GridColumns[i].Column.Font.Style;
        Attributes['italic'] := fsItalic in GridColumns[i].Column.Font.Style;
        Attributes['underline'] := fsUnderline in GridColumns[i].Column.Font.Style;
        Attributes['strikeout'] := fsStrikeOut in GridColumns[i].Column.Font.Style;
      end;

      with AddChild('title') do
      begin
        Attributes['text'] := GridColumns[i].Title.Text;
        Attributes['align'] := GridColumns[i].Title.Align;
        Attributes['color'] := GridColumns[i].Title.Color;
        with AddChild('font') do
        begin
          Attributes['color'] := GridColumns[i].Title.Font.Color;
          Attributes['name'] := GridColumns[i].Title.Font.Name;
          Attributes['size'] := GridColumns[i].Title.Font.Size;
          Attributes['bold'] := fsBold in GridColumns[i].Title.Font.Style;
          Attributes['italic'] := fsItalic in GridColumns[i].Title.Font.Style;
          Attributes['underline'] := fsUnderline in GridColumns[i].Title.Font.Style;
          Attributes['strikeout'] := fsStrikeOut in GridColumns[i].Title.Font.Style;
        end;
      end;}
    end;
end;

{ TsmxLocationParam }

constructor TsmxLocationParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamLocation := plInput;
  FParamName := '';
  FParamDefValue := Null; //Unassigned;
end;

{ TsmxLocationParams }

function TsmxLocationParams.Add: TsmxLocationParam;
begin
  Result := TsmxLocationParam(inherited Add);
end;

function TsmxLocationParams.FindByName(AParamName: String): TsmxLocationParam;
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

function TsmxLocationParams.GetItem(Index: Integer): TsmxLocationParam;
begin
  Result := TsmxLocationParam(inherited Items[Index]);
end;

{procedure TsmxLocationParams.SetItem(Index: Integer; Value: TsmxLocationParam);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxRequestField }

constructor TsmxRequestField.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FFieldFormat := '';
  FFieldName := '';
  FFieldSense := fsGeneral;
end;

{ TsmxRequestFields }

function TsmxRequestFields.Add: TsmxRequestField;
begin
  Result := TsmxRequestField(inherited Add);
end;

function TsmxRequestFields.FindByName(AFieldName: String): TsmxRequestField;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].FieldName, AFieldName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxRequestFields.GetItem(Index: Integer): TsmxRequestField;
begin
  Result := TsmxRequestField(inherited Items[Index]);
end;

{procedure TsmxRequestFields.SetItem(Index: Integer; Value: TsmxRequestField);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxRequestCfg }

{constructor TsmxRequestCfg.Create(AOwner: TComponent; AID: Integer; ACall: TsmxCallBack);
begin
  inherited Create(AOwner, AID, ACall);
  FDataSetType := dstUnknown;
  FMode := rtOpen;
  FSQLText := '';
end;}

destructor TsmxRequestCfg.Destroy;
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
  FMode := rtOpen;
  FSQLText := '';
  RequestParams.Clear;
  RequestFields.Clear;
end;

function TsmxRequestCfg.GetRequestFields: TsmxRequestFields;
begin
  if not Assigned(FRequestFields) then
    FRequestFields := TsmxRequestFields.Create(TsmxRequestField);
  Result := FRequestFields;
end;

function TsmxRequestCfg.GetRequestParams: TsmxLocationParams;
begin
  if not Assigned(FRequestParams) then
    FRequestParams := TsmxLocationParams.Create(TsmxLocationParam);
  Result := FRequestParams;
end;

{procedure TsmxRequestCfg.Initialize;
begin
  FDataSetType := dstUnknown;
  FMode := rtOpen;
  FSQLText := '';
  inherited Initialize;
end;}

procedure TsmxRequestCfg.ReadCell;
var r, n: IXMLNode; i: Integer; //v: Variant;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('request');
  if Assigned(n) then
  begin
    SQLText := n.Attributes['sqltext'];
    DataSetType := n.Attributes['type'];
    Mode := n.Attributes['mode'];
  end;

  n := r.ChildNodes.FindNode('params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'param' then
        with RequestParams.Add do
        begin
          ParamName := n.ChildNodes[i].Attributes['name'];
          //ParamDefValue := n.ChildNodes[i].Attributes['defvalue'];
          ParamDefValue := VarStringToVar(n.ChildNodes[i].Attributes['defvalue']);
          //v := n.ChildNodes[i].AttributeNodes.FindNode('defvalue').NodeValue;
          ParamLocation := n.ChildNodes[i].Attributes['location'];
        end;
  end;

  n := r.ChildNodes.FindNode('fields');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'field' then
        with RequestFields.Add do
        begin
          FieldName := n.ChildNodes[i].Attributes['name'];
          FieldFormat := n.ChildNodes[i].Attributes['format'];
          FieldSense := n.ChildNodes[i].Attributes['sense'];
        end;
  end;
end;

procedure TsmxRequestCfg.Refresh;
var ds: IsmxDataSet; p: IsmxParam; rp: TsmxLocationParam; i: Integer;
  f: IsmxField; rf: TsmxRequestField;
begin
  ds := TargetRequest.ForRequest(SQLText, DataSetType);
  try
    try
      TargetRequest.PrepRequest(ds, True, Mode);
    except
      raise EsmxCfgError.CreateRes(@SCellRequestPerformError);
    end;

    for i := RequestParams.Count - 1 downto 0 do
    begin
      p := ds.FindParam(RequestParams[i].ParamName);
      if not Assigned(p) then
        RequestParams.Remove(RequestParams[i]);
    end;
    for i := 0 to ds.ParamCount - 1 do
    begin
      rp := RequestParams.FindByName(ds.Params[i].ParamName);
      if not Assigned(rp) then
        with RequestParams.Add do
          ParamName := ds.Params[i].ParamName;
    end;

    for i := RequestFields.Count - 1 downto 0 do
    begin
      f := ds.FindField(RequestFields[i].FieldName);
      if not Assigned(f) then
        RequestFields.Remove(RequestFields[i]);
    end;
    for i := 0 to ds.FieldCount - 1 do
    begin
      rf := RequestFields.FindByName(ds.Fields[i].FieldName);
      if not Assigned(rf) then
        with RequestFields.Add do
          FieldName := ds.Fields[i].FieldName;
    end;
  finally
    ds.Close;
    ds := nil;
  end;
end;

procedure TsmxRequestCfg.WriteCell;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('request');
  n.Attributes['sqltext'] := SQLText;
  n.Attributes['type'] := DataSetType;
  n.Attributes['mode'] := Mode;

  n := r.AddChild('params');
  for i := 0 to RequestParams.Count - 1 do
    with n.AddChild('param') do
    begin
      Attributes['name'] := RequestParams[i].ParamName;
      Attributes['defvalue'] := RequestParams[i].ParamDefValue;
      Attributes['location'] := RequestParams[i].ParamLocation;
    end;

  n := r.AddChild('fields');
  for i := 0 to RequestFields.Count - 1 do
    with n.AddChild('field') do
    begin
      Attributes['name'] := RequestFields[i].FieldName;
      Attributes['format'] := RequestFields[i].FieldFormat;
      Attributes['sense'] := RequestFields[i].FieldSense;
    end;
end;

{ TsmxPageCfg }

procedure TsmxPageCfg.Clear;
begin
  PageCaption := ''; //CfgName;
  PageImageIndex := -1;
  with Grid do
  begin
    CfgID := 0;
    Align := alClient;
    Enable := True;
    Visible := True;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with FilterPanel do
  begin
    CfgID := 0;
    Align := alTop;
    Enable := True;
    Visible := True;
    with PositionSize do
    begin
      Height := 49;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  {with PageRequest do
  begin
    ID := 0;
    Mode := omManual;
  end;}
end;

{procedure TsmxPageCfg.Initialize;
begin
  PageCaption := CfgName;
  PageImageIndex := -1;
  with PageGrid do
  begin
    ID := 0;
    Align := alClient;
    Enable := True;
    Height := 0;
    Left := 0;
    Top := 0;
    Visible := True;
    Width := 0;
  end;
  with PageFilterPanel do
  begin
    ID := 0;
    Align := alTop;
    Enable := True;
    Height := 49;
    Left := 0;
    Top := 0;
    Visible := True;
    Width := 0;
  end;
  with Request do
  begin
    ID := 0;
    Mode := omManual;
  end;
  inherited Initialize;
end;}

procedure TsmxPageCfg.ReadCell;
var r, n: IXMLNode;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('page');
  if Assigned(n) then
  begin
    PageCaption := n.Attributes['caption'];
    PageImageIndex := n.Attributes['imageindex'];
  end;

  {n := r.ChildNodes.FindNode('request');
  if Assigned(n) then
    with Request do
    begin
      ID := n.Attributes['id'];
      Mode := n.Attributes['mode'];
    end;}

  n := r.ChildNodes.FindNode('grid');
  if Assigned(n) then
    with Grid do
    begin
      CfgID := n.Attributes['id'];
      Align := n.Attributes['align'];
      Enable := n.Attributes['enable'];
      Visible := n.Attributes['visible'];
      with PositionSize do
      begin
        Height := n.Attributes['height'];
        Left := n.Attributes['left'];
        Top := n.Attributes['top'];
        Width := n.Attributes['width'];
      end;
    end;

  n := r.ChildNodes.FindNode('filterpanel');
  if Assigned(n) then
    with FilterPanel do
    begin
      CfgID := n.Attributes['id'];
      Align := n.Attributes['align'];
      Enable := n.Attributes['enable'];
      Visible := n.Attributes['visible'];
      with PositionSize do
      begin
        Height := n.Attributes['height'];
        Left := n.Attributes['left'];
        Top := n.Attributes['top'];
        Width := n.Attributes['width'];
      end;
    end;  
end;

procedure TsmxPageCfg.WriteCell;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('page');
  n.Attributes['caption'] := PageCaption;
  n.Attributes['imageindex'] := PageImageIndex;

  {n := r.ChildNodes.FindNode('request');
  if not(Assigned(n)) then
    n := r.AddChild('request');
  with Request do
  begin
    n.Attributes['id'] := ID;
    n.Attributes['mode'] := Mode;
  end;}

  n := r.AddChild('grid');
  with Grid do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['align'] := Align;
    n.Attributes['enable'] := Enable;
    n.Attributes['visible'] := Visible;
    with PositionSize do
    begin
      n.Attributes['height'] := Height;
      n.Attributes['left'] := Left;
      n.Attributes['top'] := Top;
      n.Attributes['width'] := Width;
    end;
  end;

  n := r.AddChild('filterpanel');
  with FilterPanel do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['align'] := Align;
    n.Attributes['enable'] := Enable;
    n.Attributes['visible'] := Visible;
    with PositionSize do
    begin
      n.Attributes['height'] := Height;
      n.Attributes['left'] := Left;
      n.Attributes['top'] := Top;
      n.Attributes['width'] := Width;
    end;
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
  Sheets.Clear;
end;

function TsmxPageManagerCfg.GetSheets: TsmxVisibleUnits;
begin
  if not Assigned(FSheets) then
    FSheets := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FSheets;
end;

{procedure TsmxPageManagerCfg.Initialize;
begin
  inherited Initialize;
end;}

procedure TsmxPageManagerCfg.ReadCell;
var r, n: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('pages');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'page' then
        with Sheets.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['id'];
          UnitAlign := n.ChildNodes[i].Attributes['align'];
          UnitEnable := n.ChildNodes[i].Attributes['enable'];
          UnitHeight := n.ChildNodes[i].Attributes['height'];
          UnitLeft := n.ChildNodes[i].Attributes['left'];
          UnitTop := n.ChildNodes[i].Attributes['top'];
          UnitVisible := n.ChildNodes[i].Attributes['visible'];
          UnitWidth := n.ChildNodes[i].Attributes['width'];
        end;
  end;
end;

procedure TsmxPageManagerCfg.WriteCell;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('pages');
  for i := 0 to Sheets.Count - 1 do
    with n.AddChild('page') do
    begin
      Attributes['id'] := Sheets[i].CfgID;
      Attributes['align'] := Sheets[i].UnitAlign;
      Attributes['enable'] := Sheets[i].UnitEnable;
      Attributes['height'] := Sheets[i].UnitHeight;
      Attributes['left'] := Sheets[i].UnitLeft;
      Attributes['top'] := Sheets[i].UnitTop;
      Attributes['visible'] := Sheets[i].UnitVisible;
      Attributes['width'] := Sheets[i].UnitWidth;
    end;
end;

{ TsmxStateUnit }

{constructor TsmxStateUnit.Create(AHKit: TsmxHKit);
begin
  inherited Create(AHKit);
  FCfgID := 0;
  FUnitEnable := True;
  FUnitVisible := True;
end;

function TsmxStateUnit.Add: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Add);
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

function TsmxStateUnit.GetItem(Index: Integer): TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Items[Index]);
end;

function TsmxStateUnit.GetParent: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Parent);
end;}

{ TsmxStateUnits }

{function TsmxStateUnits.GetRoot: TsmxStateUnit;
begin
  Result := TsmxStateUnit(inherited Root);
end;}

{ TsmxFormState }

constructor TsmxFormState.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FID := 0;
  FStateUnits := TsmxStateUnits.Create(TsmxStateUnit);
end;

destructor TsmxFormState.Destroy;
begin
  FStateUnits.Free;
  inherited Destroy;
end;

{ TsmxFormStates }

function TsmxFormStates.Add: TsmxFormState;
begin
  Result := TsmxFormState(inherited Add);
end;

function TsmxFormStates.FindByID(AID: Integer): TsmxFormState;
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

function TsmxFormStates.GetItem(Index: Integer): TsmxFormState;
begin
  Result := TsmxFormState(inherited Items[Index]);
end;

{ TsmxFormCfg }

destructor TsmxFormCfg.Destroy;
begin
  if Assigned(FFormStates) then
    FFormStates.Free;
  inherited Destroy;
end;

procedure TsmxFormCfg.Clear;
begin
  FormCaption := ''; //CfgName;
  FormImageIndex := -1;
  with FormPositionSize do
  begin
    Height := 480;
    Left := 150;
    Top := 100;
    Width := 640;
  end;
  with AlgorithmList do
  begin
    CfgID := 0;
    IsCreateMenuItem := False;
    IsCreateToolButton := False;
  end;
  with ControlBar do
  begin
    CfgID := 0;
    Align := alTop;
    Enable := True;
    Visible := True;
    with PositionSize do
    begin
      Height := 28;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with MainMenu do
  begin
    CfgID := 0;
    Align := alTop;
    Enable := True;
    Visible := True;
    with PositionSize do
    begin
      Height := 21;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with PageManager do
  begin
    CfgID := 0;
    Align := alClient;
    Enable := True;
    Visible := True;
    with PositionSize do
    begin
      Height := 0;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  FormStates.Clear;
end;

function TsmxFormCfg.GetFormStates: TsmxFormStates;
begin
  if not Assigned(FFormStates) then
    FFormStates := TsmxFormStates.Create(TsmxFormState);
  Result := FFormStates;
end;

{procedure TsmxFormCfg.Initialize;
begin
  AlgorithmListID := 0;
  FormCaption := CfgName;
  FormImageIndex := -1;
  with MainMenu do
  begin
    ID := 0;
    Align := alTop;
    Enable := True;
    Height := 21;
    Left := 0;
    Top := 0;
    Visible := True;
    Width := 0;
  end;
  with PageManager do
  begin
    ID := 0;
    Align := alClient;
    Enable := True;
    Height := 0;
    Left := 0;
    Top := 0;
    Visible := True;
    Width := 0;
  end;
  inherited Initialize;
end;}

procedure TsmxFormCfg.ReadCell;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; u: TsmxStateUnit;
  begin
    u := AUnit.Add;
    with u do
    begin
      CfgID := ANode.Attributes['id'];
      UnitEnable := ANode.Attributes['enable'];
      UnitVisible := ANode.Attributes['visible'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'cell' then
        AddUnits(ANode.ChildNodes[i], u);
  end;

var r, n, n2: IXMLNode; i, j: Integer;  
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('form');
  if Assigned(n) then
  begin
    FormCaption := n.Attributes['caption'];
    FormImageIndex := n.Attributes['imageindex'];
    with FormPositionSize do
    begin
      Height := n.Attributes['height'];
      Left := n.Attributes['left'];
      Top := n.Attributes['top'];
      Width := n.Attributes['width'];
    end;
  end;

  n := r.ChildNodes.FindNode('pagemanager');
  if Assigned(n) then
    with PageManager do
    begin
      CfgID := n.Attributes['id'];
      Align := n.Attributes['align'];
      Enable := n.Attributes['enable'];
      Visible := n.Attributes['visible'];
      with PositionSize do
      begin
        Height := n.Attributes['height'];
        Left := n.Attributes['left'];
        Top := n.Attributes['top'];
        Width := n.Attributes['width'];
      end;
    end;

  n := r.ChildNodes.FindNode('mainmenu');
  if Assigned(n) then
    with MainMenu do
    begin
      CfgID := n.Attributes['id'];
      Align := n.Attributes['align'];
      Enable := n.Attributes['enable'];
      Visible := n.Attributes['visible'];
      with PositionSize do
      begin
        Height := n.Attributes['height'];
        Left := n.Attributes['left'];
        Top := n.Attributes['top'];
        Width := n.Attributes['width'];
      end;
    end;

  n := r.ChildNodes.FindNode('algorithmlist');
  if Assigned(n) then
    with AlgorithmList do
    begin
      CfgID := n.Attributes['id'];
      IsCreateMenuItem := n.Attributes['iscreatemenuitem'];
      IsCreateToolButton := n.Attributes['iscreatetoolbutton'];
    end;

  n := r.ChildNodes.FindNode('controlbar');
  if Assigned(n) then
    with ControlBar do
    begin
      CfgID := n.Attributes['id'];
      Align := n.Attributes['align'];
      Enable := n.Attributes['enable'];
      Visible := n.Attributes['visible'];
      with PositionSize do
      begin
        Height := n.Attributes['height'];
        Left := n.Attributes['left'];
        Top := n.Attributes['top'];
        Width := n.Attributes['width'];
      end;
    end;

  n := r.ChildNodes.FindNode('staterequest');
  if Assigned(n) then
    with StateRequest do
    begin
      CfgID := n.Attributes['id'];
      Mode := n.Attributes['mode'];
    end;

  n := r.ChildNodes.FindNode('states');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'state' then
        with FormStates.Add do
        begin
          ID := n.ChildNodes[i].Attributes['id'];
          n2 := n.ChildNodes[i].ChildNodes.FindNode('cells');
          if Assigned(n2) and (n2.ChildNodes.Count > 0) then
            for j := 0 to n2.ChildNodes.Count - 1 do
              if n2.ChildNodes[j].NodeName = 'cell' then
                AddUnits(n2.ChildNodes[j], StateUnits.Root);
        end;
  end;
end;

procedure TsmxFormCfg.WriteCell;

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
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('form');
  n.Attributes['caption'] := FormCaption;
  n.Attributes['imageindex'] := FormImageIndex;
  with FormPositionSize do
  begin
    n.Attributes['height'] := Height;
    n.Attributes['left'] := Left;
    n.Attributes['top'] := Top;
    n.Attributes['width'] := Width;
  end;

  n := r.AddChild('pagemanager');
  with PageManager do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['align'] := Align;
    n.Attributes['enable'] := Enable;
    n.Attributes['visible'] := Visible;
    with PositionSize do
    begin
      n.Attributes['height'] := Height;
      n.Attributes['left'] := Left;
      n.Attributes['top'] := Top;
      n.Attributes['width'] := Width;
    end;
  end;

  n := r.AddChild('mainmenu');
  with MainMenu do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['align'] := Align;
    n.Attributes['enable'] := Enable;
    n.Attributes['visible'] := Visible;
    with PositionSize do
    begin
      n.Attributes['height'] := Height;
      n.Attributes['left'] := Left;
      n.Attributes['top'] := Top;
      n.Attributes['width'] := Width;
    end;
  end;

  n := r.AddChild('algorithmlist');
  with AlgorithmList do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['iscreatemenuitem'] := IsCreateMenuItem;
    n.Attributes['iscreatetoolbutton'] := IsCreateToolButton;
  end;

  n := r.AddChild('controlbar');
  with ControlBar do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['align'] := Align;
    n.Attributes['enable'] := Enable;
    n.Attributes['visible'] := Visible;
    with PositionSize do
    begin
      n.Attributes['height'] := Height;
      n.Attributes['left'] := Left;
      n.Attributes['top'] := Top;
      n.Attributes['width'] := Width;
    end;  
  end;

  n := r.AddChild('staterequest');
  with StateRequest do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['mode'] := Mode;
  end;

  n := r.AddChild('states');
  for i := 0 to FormStates.Count - 1 do
  begin
    n2 := n.AddChild('state');
    n2.Attributes['id'] := FormStates[i].ID;
    n3 := n2.AddChild('cells');
    for j := 0 to FormStates[i].StateUnits.Root.Count - 1 do
      AddNodes(n3, FormStates[i].StateUnits.Root[j]);
  end;
end;

{ TsmxFilterCfg }

procedure TsmxFilterCfg.Clear;
begin
  //FAlgorithmID := 0;
  FFilterName := ''; //Name;
  //FFilterDefValue := Null;
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
    Text := ''; //CfgName;
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
  with Algorithm do
  begin
    CfgID := 0;
    Caption := '';
    Enable := True;
    HotKey := 0;
    Visible := True;
    //Params := Unassigned;
  end;
  {with Request do
  begin
    ID := 0;
    Mode := omManual;
  end;}
end;

{procedure TsmxFilterCfg.Initialize;
begin
  with FFilterHeader do
  begin
    Text := CfgName;
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
  FFilterFormat := '';
  FFilterName := Name;
  FFilterText := '';
  inherited Initialize;
end;}

procedure TsmxFilterCfg.ReadCell;
var r, n, n2: IXMLNode;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('filter');
  if Assigned(n) then
  begin
    FilterName := n.Attributes['name'];
    //FilterDefValue := n.Attributes['defvalue'];
    DisplayFormat := n.Attributes['displayformat'];
    ValueFormat := n.Attributes['valueformat'];
    //AlgorithmID := n.Attributes['algorithmid'];
    n2 := n.ChildNodes.FindNode('font');
    if Assigned(n2) then
      with FilterFont do
      begin
        Color := n2.Attributes['color'];
        Name := n2.Attributes['name'];
        Size := n2.Attributes['size'];
        if n2.Attributes['bold'] then Style := Style + [fsBold];
        if n2.Attributes['italic'] then Style := Style + [fsItalic];
        if n2.Attributes['underline'] then Style := Style + [fsUnderline];
        if n2.Attributes['strikeout'] then Style := Style + [fsStrikeOut];
      end;
  end;

  n := r.ChildNodes.FindNode('header');
  if Assigned(n) then
    with FilterHeader do
    begin
      Text := n.Attributes['text'];
      Align := n.Attributes['align'];
      Color := n.Attributes['color'];
      n2 := n.ChildNodes.FindNode('font');
      if Assigned(n2) then
        with Font do
        begin
          Color := n2.Attributes['color'];
          Name := n2.Attributes['name'];
          Size := n2.Attributes['size'];
          if n2.Attributes['bold'] then Style := Style + [fsBold];
          if n2.Attributes['italic'] then Style := Style + [fsItalic];
          if n2.Attributes['underline'] then Style := Style + [fsUnderline];
          if n2.Attributes['strikeout'] then Style := Style + [fsStrikeOut];
        end;
    end;

  n := r.ChildNodes.FindNode('algorithm');
  if Assigned(n) then
    with Algorithm do
    begin
      CfgID := n.Attributes['id'];
      Caption := n.Attributes['caption'];
      Enable := n.Attributes['enable'];
      HotKey := n.Attributes['hotkey'];
      Visible := n.Attributes['visible'];
      {n2 := n.ChildNodes.FindNode('params');
      if (Assigned(n2)) and (n2.ChildNodes.Count > 0) then
      begin
        v := VarArrayCreate([0, n2.ChildNodes.Count - 1], varVariant);
        for j := 0 to n2.ChildNodes.Count - 1 do
          if n2.ChildNodes[j].NodeName = 'param' then
            v[j] := n2.ChildNodes[j].Attributes['value'];
        Params := v;
      end;}
    end;

  {n := r.ChildNodes.FindNode('request');
  if Assigned(n) then
    with Request do
    begin
      ID := n.Attributes['id'];
      Mode := n.Attributes['mode'];
    end;}

  {n := r.ChildNodes.FindNode('algorithmlist');
  if Assigned(n) then
    with AlgorithmList do
    begin
      ID := n.Attributes['id'];
      IsCreateMenuItem := n.Attributes['iscreatemenuitem'];
    end;}
end;

procedure TsmxFilterCfg.WriteCell;
var r, n, n2: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('filter');
  n.Attributes['name'] := FilterName;
  //n.Attributes['defvalue'] := FilterDefValue;
  n.Attributes['displayformat'] := DisplayFormat;
  n.Attributes['valueformat'] := ValueFormat;
  //n.Attributes['algorithmid'] := AlgorithmID;
  n2 := n.AddChild('font');
  with FilterFont do
  begin
    n2.Attributes['color'] := Color;
    n2.Attributes['name'] := Name;
    n2.Attributes['size'] := Size;
    n2.Attributes['bold'] := fsBold in Style;
    n2.Attributes['italic'] := fsItalic in Style;
    n2.Attributes['underline'] := fsUnderline in Style;
    n2.Attributes['strikeout'] := fsStrikeOut in Style;
  end;

  n := r.AddChild('header');
  with FilterHeader do
  begin
    n.Attributes['text'] := Text;
    n.Attributes['align'] := Align;
    n.Attributes['color'] := Color;
    n2 := n.AddChild('font');
    with Font do
    begin
      n2.Attributes['color'] := Color;
      n2.Attributes['name'] := Name;
      n2.Attributes['size'] := Size;
      n2.Attributes['bold'] := fsBold in Style;
      n2.Attributes['italic'] := fsItalic in Style;
      n2.Attributes['underline'] := fsUnderline in Style;
      n2.Attributes['strikeout'] := fsStrikeOut in Style;
    end;
  end;

  n := r.AddChild('algorithm');
  with Algorithm do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['caption'] := Caption;
    n.Attributes['enable'] := Enable;
    n.Attributes['hotkey'] := HotKey;
    n.Attributes['visible'] := Visible;
  end;

  {n := r.ChildNodes.FindNode('request');
  if not(Assigned(n)) then
    n := r.AddChild('request');
  with Request do
  begin
    n.Attributes['id'] := ID;
    n.Attributes['mode'] := Mode;
  end;}

  {n := r.ChildNodes.FindNode('algorithmlist');
  if not(Assigned(n)) then
    n := r.AddChild('algorithmlist');
  with AlgorithmList do
  begin
    n.Attributes['id'] := ID;
    n.Attributes['iscreatemenuitem'] := IsCreateMenuItem;
  end;}
end;

{ TsmxFilterPanelCfg }

destructor TsmxFilterPanelCfg.Destroy;
begin
  if Assigned(FFilters) then
    FFilters.Free;
  inherited Destroy;
end;

procedure TsmxFilterPanelCfg.Clear;
begin
  {with AlgorithmList do
  begin
    ID := 0;
    IsCreateMenuItem := False;
  end;}
  with ApplyRequest do
  begin
    CfgID := 0;
    Mode := omManual;
  end;
  with PrepareRequest do
  begin
    CfgID := 0;
    Mode := omManual;
  end;
  Filters.Clear;
end;

function TsmxFilterPanelCfg.GetFilters: TsmxVisibleUnits;
begin
  if not Assigned(FFilters) then
    FFilters := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FFilters;
end;

procedure TsmxFilterPanelCfg.ReadCell;
var r, n: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('applyrequest');
  if Assigned(n) then
    with ApplyRequest do
    begin
      CfgID := n.Attributes['id'];
      Mode := n.Attributes['mode'];
    end;

  n := r.ChildNodes.FindNode('preparerequest');
  if Assigned(n) then
    with PrepareRequest do
    begin
      CfgID := n.Attributes['id'];
      Mode := n.Attributes['mode'];
    end;

  n := r.ChildNodes.FindNode('filters');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'filter' then
        with Filters.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['id'];
          UnitAlign := n.ChildNodes[i].Attributes['align'];
          UnitEnable := n.ChildNodes[i].Attributes['enable'];
          UnitHeight := n.ChildNodes[i].Attributes['height'];
          UnitLeft := n.ChildNodes[i].Attributes['left'];
          UnitTop := n.ChildNodes[i].Attributes['top'];
          UnitVisible := n.ChildNodes[i].Attributes['visible'];
          UnitWidth := n.ChildNodes[i].Attributes['width'];
        end;
  end;

  {n := r.ChildNodes.FindNode('algorithmlist');
  if Assigned(n) then
    with AlgorithmList do
    begin
      ID := n.Attributes['id'];
      IsCreateMenuItem := n.Attributes['iscreatemenuitem'];
    end;}
end;

procedure TsmxFilterPanelCfg.WriteCell;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('applyrequest');
  with ApplyRequest do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['mode'] := Mode;
  end;

  n := r.AddChild('preparerequest');
  with PrepareRequest do
  begin
    n.Attributes['id'] := CfgID;
    n.Attributes['mode'] := Mode;
  end;

  n := r.AddChild('filters');
  for i := 0 to Filters.Count - 1 do
    with n.AddChild('filter') do
    begin
      Attributes['id'] := Filters[i].CfgID;
      Attributes['align'] := Filters[i].UnitAlign;
      Attributes['enable'] := Filters[i].UnitEnable;
      Attributes['height'] := Filters[i].UnitHeight;
      Attributes['left'] := Filters[i].UnitLeft;
      Attributes['top'] := Filters[i].UnitTop;
      Attributes['visible'] := Filters[i].UnitVisible;
      Attributes['width'] := Filters[i].UnitWidth;
    end;

  {n := r.ChildNodes.FindNode('algorithmlist');
  if not(Assigned(n)) then
    n := r.AddChild('algorithmlist');
  with AlgorithmList do
  begin
    n.Attributes['id'] := ID;
    n.Attributes['iscreatemenuitem'] := IsCreateMenuItem;
  end;}
end;

{ TsmxMenuItemCfg }

procedure TsmxMenuItemCfg.Clear;
begin
  FItemCaption := ''; //CfgName;
  FItemImageIndex := -1;
end;

{procedure TsmxMenuItemCfg.Initialize;
begin
  FItemCaption := CfgName;
  FItemImageIndex := -1;
  inherited Initialize;
end;}

procedure TsmxMenuItemCfg.ReadCell;
var r, n: IXMLNode;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('menuitem');
  if Assigned(n) then
  begin
    ItemCaption := n.Attributes['caption'];
    ItemImageIndex := n.Attributes['imageindex'];
  end;
end;

procedure TsmxMenuItemCfg.WriteCell;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('menuitem');
  n.Attributes['caption'] := ItemCaption;
  n.Attributes['imageindex'] := ItemImageIndex;
end;

{ TsmxMenuUnit }

{constructor TsmxMenuUnit.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FID := 0;
  FMMItemEnable := True;
  FMMItemVisible := True;
end;}

{ TsmxMenuUnits }

{function TsmxMenuUnits.Add: TsmxMenuUnit;
begin
  Result := TsmxMenuUnit(inherited Add);
end;}

{function TsmxMenuUnits.Find(ParamName: String): TsmxLocationParam;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ParamName, ParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;}

{function TsmxMenuUnits.GetItem(Index: Integer): TsmxMenuUnit;
begin
  Result := TsmxMenuUnit(inherited Items[Index]);
end;}

{procedure TsmxMenuUnits.SetItem(Index: Integer; Value: TsmxMenuUnit);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxHVisibleUnit }

constructor TsmxHVisibleUnit.Create(AHKit: TsmxHKit); //(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass);
begin
  inherited Create(AHKit); //(AParent, AItemClass);
  FCfgID := 0;
  FUnitAlign := alClient;
  FUnitEnable := True;
  FUnitHeight := 0;
  FUnitLeft := 0;
  FUnitTop := 0;
  FUnitVisible := True;
  FUnitWidth := 0;
end;

function TsmxHVisibleUnit.Add: TsmxHVisibleUnit;
begin
  Result := TsmxHVisibleUnit(inherited Add);
end;

{function TsmxHVisibleUnit.FindByID(AID: Integer; AmongAll: Boolean = False): TsmxHVisibleUnit;
var i: Integer;

  function Find(AUnit: TsmxHVisibleUnit): TsmxHVisibleUnit;
  var i: Integer;
  begin
    Result := nil;
    for i := 0 to AUnit.Count - 1 do
    begin
      if AUnit.Items[i].ID = AID then
      begin
        Result := AUnit.Items[i];
        Break;
      end;
      Result := Find(AUnit.Items[i]);
      if Assigned(Result) then
        Break;
    end;
  end;

begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ID = AID then
    begin
      Result := Items[i];
      Break;
    end;
    if AmongAll then
    begin
      Result := Find(Items[i]);
      if Assigned(Result) then
        Break;
    end;
  end;
end;}

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

{ TsmxMenuUnit }

{constructor TsmxMenuUnit.Create(AHKit: TsmxHKit); //(AParent: TsmxHKitItem; AItemClass: TsmxHKitItemClass);
begin
  inherited Create(AHKit); //(AParent, AItemClass);
  FUnitCaption := '';
  FUnitEnable := True;
  FUnitImageIndex := -1;
  FUnitName := '';
  FUnitVisible := True;
end;

function TsmxMenuUnit.Add: TsmxMenuUnit;
begin
  Result := TsmxMenuUnit(inherited Add);
end;

function TsmxMenuUnit.GetItem(Index: Integer): TsmxMenuUnit;
begin
  Result := TsmxMenuUnit(inherited Items[Index]);
end;}

{ TsmxMenuUnits }

{function TsmxMenuUnits.GetRoot: TsmxMenuUnit;
begin
  Result := TsmxMenuUnit(inherited Root);
end;}

{ TsmxMainMenuCfg }

destructor TsmxMainMenuCfg.Destroy;
begin
  if Assigned(FMenuUnits) then
    FMenuUnits.Free;
  inherited Destroy;
end;

procedure TsmxMainMenuCfg.Clear;
begin
  MenuUnits.Root.Clear;
end;

function TsmxMainMenuCfg.GetMenuUnits: TsmxHVisibleUnits;
begin
  if not Assigned(FMenuUnits) then
    FMenuUnits := TsmxHVisibleUnits.Create(TsmxHVisibleUnit);
  Result := FMenuUnits;
end;

{function TsmxMainMenuCfg.GetMenuUnits: TsmxMenuUnits;
begin
  if not(Assigned(FMenuUnits)) then
    FMenuUnits := TsmxMenuUnits.Create(TsmxMenuUnit);
  Result := FMenuUnits;
end;}

procedure TsmxMainMenuCfg.ReadCell;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxHVisibleUnit);
  var i: Integer; u: TsmxHVisibleUnit;
  begin
    u := AUnit.Add;
    with u do
    begin
      CfgID := ANode.Attributes['id'];
      UnitAlign := ANode.Attributes['align'];
      UnitEnable := ANode.Attributes['enable'];
      UnitHeight := ANode.Attributes['height'];
      UnitLeft := ANode.Attributes['left'];
      UnitTop := ANode.Attributes['top'];
      UnitVisible := ANode.Attributes['visible'];
      UnitWidth := ANode.Attributes['width'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'menuitem' then
        AddUnits(ANode.ChildNodes[i], u);
  end;

var r, n: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('menuitems');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'menuitem' then
        AddUnits(n.ChildNodes[i], MenuUnits.Root);
  end;
end;

{procedure TsmxMainMenuCfg.ReadCell;
var r, n: IXMLNode; i: Integer;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxMenuUnit);
  var i: Integer; u: TsmxMenuUnit;
  begin
    u := AUnit.Add;
    with u do
    begin
      UnitCaption := ANode.Attributes['caption'];
      UnitEnable := ANode.Attributes['enable'];
      UnitImageIndex := ANode.Attributes['imageindex'];
      UnitName := ANode.Attributes['name'];
      UnitVisible := ANode.Attributes['visible'];
    end;
    if ANode.ChildNodes.Count > 0 then
      for i := 0 to ANode.ChildNodes.Count - 1 do
        if ANode.ChildNodes[i].NodeName = 'menuitem' then
          AddUnits(ANode.ChildNodes[i], u);
  end;

begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if not(Assigned(r)) then
    Exit;

  n := r.ChildNodes.FindNode('menuitems');
  if (Assigned(n)) and (n.ChildNodes.Count > 0) then
  begin
    MenuUnits.Root.Clear;
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'menuitem' then
        AddUnits(n.ChildNodes[i], MenuUnits.Root);
  end;
end;}

procedure TsmxMainMenuCfg.WriteCell;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxHVisibleUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('menuitem');
    with n do
    begin
      Attributes['id'] := AUnit.CfgID;
      Attributes['align'] := AUnit.UnitAlign;
      Attributes['enable'] := AUnit.UnitEnable;
      Attributes['height'] := AUnit.UnitHeight;
      Attributes['left'] := AUnit.UnitLeft;
      Attributes['top'] := AUnit.UnitTop;
      Attributes['visible'] := AUnit.UnitVisible;
      Attributes['width'] := AUnit.UnitWidth;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;

var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('menuitems');
  for i := 0 to MenuUnits.Root.Count - 1 do
    AddNodes(n, MenuUnits.Root[i]);
end;

{procedure TsmxMainMenuCfg.WriteCell;
var r, n: IXMLNode; i: Integer;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxMenuUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('menuitem');
    with n do
    begin
      Attributes['caption'] := AUnit.UnitCaption;
      Attributes['enable'] := AUnit.UnitEnable;
      Attributes['imageindex'] := AUnit.UnitImageIndex;
      Attributes['name'] := AUnit.UnitName;
      Attributes['visible'] := AUnit.UnitVisible;
    end;
    if AUnit.Count > 0 then
      for i := 0 to AUnit.Count - 1 do
        AddNodes(n, AUnit[i]);
  end;

begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if not(Assigned(r)) then
    r := XMLDoc.AddChild('root');

  n := r.ChildNodes.FindNode('menuitems');
  if not(Assigned(n)) then
    n := r.AddChild('menuitems') else
    n.ChildNodes.Clear;

  for i := 0 to MenuUnits.Root.Count - 1 do
    AddNodes(n, MenuUnits.Root[i]);
end;}

{ TsmxLibAlgorithmCfg }

procedure TsmxLibAlgorithmCfg.Clear;
begin
  FAlgCaption := ''; //CfgName;
  FAlgHotKey := 0;
  FAlgImageIndex := -1;
  FAlgLibrary := '';
  FAlgProcedure := '';
  AlgParams.Clear;
end;

function TsmxLibAlgorithmCfg.GetAlgParams: TsmxLocationParams;
begin
  if not Assigned(FAlgParams) then
    FAlgParams := TsmxLocationParams.Create(TsmxLocationParam);
  Result := FAlgParams;
end;

{procedure TsmxLibAlgorithmCfg.Initialize;
begin
  FAlgCaption := CfgName;
  FAlgHotKey := 0;
  FAlgImageIndex := -1;
  FAlgLibrary := '';
  FAlgProcedure := '';
  inherited Initialize;
end;}

procedure TsmxLibAlgorithmCfg.ReadCell;
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

procedure TsmxLibAlgorithmCfg.WriteCell;
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
end;

{ TsmxAlgorithmItem }

constructor TsmxAlgorithmItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FCfgID := 0;
  //FAlgorithmButton := True;
  FAlgorithmCaption := '';
  FAlgorithmEnable := True;
  FAlgorithmHotKey := 0;
  FAlgorithmMenuItemID := 0;
  FAlgorithmToolBarID := 0;
  //FAlgorithmParams := Unassigned;
  FAlgorithmVisible := True;
end;

{ TsmxAlgorithmItems }

function TsmxAlgorithmItems.Add: TsmxAlgorithmItem;
begin
  Result := TsmxAlgorithmItem(inherited Add);
end;

function TsmxAlgorithmItems.FindByCfgID(ACfgID: Integer): TsmxAlgorithmItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].CfgID = ACfgID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxAlgorithmItems.GetItem(Index: Integer): TsmxAlgorithmItem;
begin
  Result := TsmxAlgorithmItem(inherited Items[Index]);
end;

{ TsmxAlgorithmListCfg }

destructor TsmxAlgorithmListCfg.Destroy;
begin
  if Assigned(FAlgorithmItems) then
    FAlgorithmItems.Free;
  inherited Destroy;
end;

procedure TsmxAlgorithmListCfg.Clear;
begin
  AlgorithmItems.Clear;
end;

function TsmxAlgorithmListCfg.GetAlgorithmItems: TsmxAlgorithmItems;
begin
  if not Assigned(FAlgorithmItems) then
    FAlgorithmItems := TsmxAlgorithmItems.Create(TsmxAlgorithmItem);
  Result := FAlgorithmItems;
end;

procedure TsmxAlgorithmListCfg.ReadCell;
var r, n: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('algorithms');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
    begin
      if n.ChildNodes[i].NodeName = 'algorithm' then
        with AlgorithmItems.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['id'];
          AlgorithmMenuItemID := n.ChildNodes[i].Attributes['menuitemid'];
          AlgorithmToolBarID := n.ChildNodes[i].Attributes['toolbarid'];
          //AlgorithmButton := n.ChildNodes[i].Attributes['button'];
          AlgorithmEnable := n.ChildNodes[i].Attributes['enable'];
          AlgorithmVisible := n.ChildNodes[i].Attributes['visible'];
          AlgorithmHotKey := n.ChildNodes[i].Attributes['hotkey'];
          AlgorithmCaption := n.ChildNodes[i].Attributes['caption'];
          {n2 := n.ChildNodes[i].ChildNodes.FindNode('params');
          if (Assigned(n2)) and (n2.ChildNodes.Count > 0) then
          begin
            v := VarArrayCreate([0, n2.ChildNodes.Count - 1], varVariant);
            for j := 0 to n2.ChildNodes.Count - 1 do
              if n2.ChildNodes[j].NodeName = 'param' then
                v[j] := n2.ChildNodes[j].Attributes['value'];
            AlgorithmParams := v;
          end;}
        end;
    end;
  end;
end;

procedure TsmxAlgorithmListCfg.WriteCell;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('algorithms');
  for i := 0 to AlgorithmItems.Count - 1 do
    with n.AddChild('algorithm') do
    begin
      Attributes['id'] := AlgorithmItems[i].CfgID;
      Attributes['menuitemid'] := AlgorithmItems[i].AlgorithmMenuItemID;
      Attributes['toolbarid'] := AlgorithmItems[i].AlgorithmToolBarID;
      //Attributes['button'] := AlgorithmItems[i].AlgorithmButton;
      Attributes['enable'] := AlgorithmItems[i].AlgorithmEnable;
      Attributes['visible'] := AlgorithmItems[i].AlgorithmVisible;
      Attributes['hotkey'] := AlgorithmItems[i].AlgorithmHotKey;
      Attributes['caption'] := AlgorithmItems[i].AlgorithmCaption;
      {n2 := n.ChildNodes.FindNode('params');
      if not(Assigned(n2)) then
        n2 := n.AddChild('params') else
        n2.ChildNodes.Clear;
      if VarIsArray(AlgorithmItems[i].AlgorithmParams) then
        for j := 0 to VarArrayHighBound(AlgorithmItems[i].AlgorithmParams, 1) do
          with n2.AddChild('param') do
            Attributes['value'] := AlgorithmItems[i].AlgorithmParams[j];}
    end;
end;

{ TsmxToolBarCfg }

procedure TsmxToolBarCfg.Clear;
begin
  FBarFlat := False;
  FBarShowCaptions := False;
  FBarShowHint := False;
end;

procedure TsmxToolBarCfg.ReadCell;
var r, n: IXMLNode;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('toolbar');
  if Assigned(n) then
  begin
    BarFlat := n.Attributes['flat'];
    BarShowCaptions := n.Attributes['showcaptions'];
    BarShowHint := n.Attributes['showhint'];
  end;
end;

procedure TsmxToolBarCfg.WriteCell;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('toolbar');
  n.Attributes['flat'] := BarFlat;
  n.Attributes['showcaptions'] := BarShowCaptions;
  n.Attributes['showhint'] := BarShowHint;
end;

{ TsmxControlBarCfg }

procedure TsmxControlBarCfg.Clear;
begin
  BarUnits.Clear;
end;

function TsmxControlBarCfg.GetBarUnits: TsmxVisibleUnits;
begin
  if not Assigned(FBarUnits) then
    FBarUnits := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FBarUnits;
end;

procedure TsmxControlBarCfg.ReadCell;
var r, n: IXMLNode; i: Integer;
begin
  //Clear;
  r := XMLDoc.ChildNodes.FindNode('root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('bars');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'bar' then
        with BarUnits.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['id'];
          UnitAlign := n.ChildNodes[i].Attributes['align'];
          UnitEnable := n.ChildNodes[i].Attributes['enable'];
          UnitHeight := n.ChildNodes[i].Attributes['height'];
          UnitLeft := n.ChildNodes[i].Attributes['left'];
          UnitTop := n.ChildNodes[i].Attributes['top'];
          UnitVisible := n.ChildNodes[i].Attributes['visible'];
          UnitWidth := n.ChildNodes[i].Attributes['width'];
        end;
  end;
end;

procedure TsmxControlBarCfg.WriteCell;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('root');

  n := r.AddChild('bars');
  for i := 0 to BarUnits.Count - 1 do
    with n.AddChild('bar') do
    begin
      Attributes['id'] := BarUnits[i].CfgID;
      Attributes['align'] := BarUnits[i].UnitAlign;
      Attributes['enable'] := BarUnits[i].UnitEnable;
      Attributes['height'] := BarUnits[i].UnitHeight;
      Attributes['left'] := BarUnits[i].UnitLeft;
      Attributes['top'] := BarUnits[i].UnitTop;
      Attributes['visible'] := BarUnits[i].UnitVisible;
      Attributes['width'] := BarUnits[i].UnitWidth;
    end;
end;

initialization
  RegisterClasses([TsmxRequestCfg, TsmxColumnCfg, TsmxGridCfg, TsmxFilterCfg,
    TsmxFilterPanelCfg, TsmxPageCfg, TsmxPageManagerCfg, TsmxMenuItemCfg,
    TsmxMainMenuCfg, TsmxLibAlgorithmCfg, TsmxAlgorithmListCfg, TsmxToolBarCfg,
    TsmxControlBarCfg, TsmxFormCfg]);

end.
