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
  Classes, Controls, Graphics, smxClasses, smxDBIntf, smxTypes;

type
  { TsmxLocationParam }

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
  private
    function GetItem(Index: Integer): TsmxLocationParam;
  public
    function Add: TsmxLocationParam;
    function FindByName(AParamName: String): TsmxLocationParam;

    property Items[Index: Integer]: TsmxLocationParam read GetItem; default;
  end;

  { TsmxRequestField }

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
  private
    function GetItem(Index: Integer): TsmxRequestField;
  public
    function Add: TsmxRequestField;
    function FindByName(AFieldName: String): TsmxRequestField;

    property Items[Index: Integer]: TsmxRequestField read GetItem; default;
  end;

  { TsmxRequestCfg }

  TsmxRequestCfg = class(TsmxCellCfg)
  private
    FDataSetType: TsmxDataSetType;
    FPerformanceMode: TsmxPerformanceMode;
    FRequestFields: TsmxRequestFields;
    FRequestParams: TsmxLocationParams;
    FSQLText: String;
    function GetRequestFields: TsmxRequestFields;
    function GetRequestParams: TsmxLocationParams;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property DataSetType: TsmxDataSetType read FDataSetType write FDataSetType;
    property PerformanceMode: TsmxPerformanceMode read FPerformanceMode write FPerformanceMode;
    property RequestFields: TsmxRequestFields read GetRequestFields;
    property RequestParams: TsmxLocationParams read GetRequestParams;
    property SQLText: String read FSQLText write FSQLText;
  end;

  { TsmxColumnCfg }

  TsmxColumnCfg = class(TsmxCellCfg)
  private
    FColumnFieldName: String;
    FColumnText: TsmxCellText;
    FColumnTitle: TsmxCellText;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property ColumnFieldName: String read FColumnFieldName write FColumnFieldName;
    property ColumnText: TsmxCellText read FColumnText write FColumnText;
    property ColumnTitle: TsmxCellText read FColumnTitle write FColumnTitle;
  end;

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
  private
    function GetItem(Index: Integer): TsmxVisibleUnit;
  public
    function Add: TsmxVisibleUnit;

    property Items[Index: Integer]: TsmxVisibleUnit read GetItem; default;
  end;

  { TsmxGridCfg }

  TsmxGridCfg = class(TsmxCellCfg)
  private
    FGridColLines: Boolean;
    FGridColumns: TsmxVisibleUnits;
    FGridRowLines: Boolean;
    FGridRowSelect: Boolean;
    function GetGridColumns: TsmxVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property GridColLines: Boolean read FGridColLines write FGridColLines;
    property GridColumns: TsmxVisibleUnits read GetGridColumns;
    property GridRowLines: Boolean read FGridRowLines write FGridRowLines;
    property GridRowSelect: Boolean read FGridRowSelect write FGridRowSelect;
  end;

  { TsmxLibAlgorithmCfg }

  TsmxLibAlgorithmCfg = class(TsmxCellCfg)
  private
    FAlgDefCaption: String;
    FAlgDefHint: String;
    FAlgDefHotKey: Integer;
    FAlgImageIndex: Integer;
    FAlgLibrary: String;
    FAlgParams: TsmxLocationParams;
    FAlgProcedure: String;
    function GetAlgParams: TsmxLocationParams;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property AlgDefCaption: String read FAlgDefCaption write FAlgDefCaption;
    property AlgDefHint: String read FAlgDefHint write FAlgDefHint;
    property AlgDefHotKey: Integer read FAlgDefHotKey write FAlgDefHotKey;
    property AlgImageIndex: Integer read FAlgImageIndex write FAlgImageIndex;
    property AlgLibrary: String read FAlgLibrary write FAlgLibrary;
    property AlgParams: TsmxLocationParams read GetAlgParams;
    property AlgProcedure: String read FAlgProcedure write FAlgProcedure;
  end;

  { TsmxAlgorithmItem }

  TsmxAlgorithmItem = class(TsmxKitItem)
  private
    FCfgID: Integer;
    FAlgorithmCaption: String;
    FAlgorithmHint: String;
    FAlgorithmEnable: Boolean;
    FAlgorithmHotKey: Integer;
    FAlgorithmMenuItemCfgID: Integer;
    FAlgorithmToolBarCfgID: Integer;
    FAlgorithmVisible: Boolean;
  public
    constructor Create(AKit: TsmxKit); override;

    property CfgID: Integer read FCfgID write FCfgID;
    property AlgorithmCaption: String read FAlgorithmCaption write FAlgorithmCaption;
    property AlgorithmHint: String read FAlgorithmHint write FAlgorithmHint;
    property AlgorithmEnable: Boolean read FAlgorithmEnable write FAlgorithmEnable;
    property AlgorithmHotKey: Integer read FAlgorithmHotKey write FAlgorithmHotKey;
    property AlgorithmMenuItemCfgID: Integer read FAlgorithmMenuItemCfgID write FAlgorithmMenuItemCfgID;
    property AlgorithmToolBarCfgID: Integer read FAlgorithmToolBarCfgID write FAlgorithmToolBarCfgID;
    property AlgorithmVisible: Boolean read FAlgorithmVisible write FAlgorithmVisible;
  end;

  { TsmxAlgorithmItems }

  TsmxAlgorithmItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxAlgorithmItem;
  public
    function Add: TsmxAlgorithmItem;
    function FindByCfgID(ACfgID: Integer): TsmxAlgorithmItem;

    property Items[Index: Integer]: TsmxAlgorithmItem read GetItem; default;
  end;

  { TsmxAlgorithmListCfg }

  TsmxAlgorithmListCfg = class(TsmxCellCfg)
  private
    FAlgorithmItems: TsmxAlgorithmItems;
    function GetAlgorithmItems: TsmxAlgorithmItems;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property AlgorithmItems: TsmxAlgorithmItems read GetAlgorithmItems;
  end;

  { TsmxFilterCfg }

  TsmxFilterCfg = class(TsmxCellCfg)
  private
    FAlgorithm: TsmxAlgorithmSetting;
    FDisplayFormat: String;
    FFilterFont: TsmxCellFont;
    FFilterHeader: TsmxCellText;
    FFilterName: String;
    FValueFormat: String;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property Algorithm: TsmxAlgorithmSetting read FAlgorithm write FAlgorithm;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    property FilterFont: TsmxCellFont read FFilterFont write FFilterFont;
    property FilterHeader: TsmxCellText read FFilterHeader write FFilterHeader;
    property FilterName: String read FFilterName write FFilterName;
    property ValueFormat: String read FValueFormat write FValueFormat;
  end;

  { TsmxFilterDeskCfg }

  TsmxFilterDeskCfg = class(TsmxCellCfg)
  private
    FApplyRequest: TsmxRequestSetting;
    FFilters: TsmxVisibleUnits;
    FPrepareRequest: TsmxRequestSetting;
    function GetFilters: TsmxVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property ApplyRequest: TsmxRequestSetting read FApplyRequest write FApplyRequest;
    property Filters: TsmxVisibleUnits read GetFilters;
    property PrepareRequest: TsmxRequestSetting read FPrepareRequest write FPrepareRequest;
  end;

  { TsmxSectionCfg }

  TsmxSectionCfg = class(TsmxCellCfg)
  private
    FFilterPanel: TsmxControlCellSetting;
    FGrid: TsmxControlCellSetting;
    FRequest: TsmxRequestSetting;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property FilterPanel: TsmxControlCellSetting read FFilterPanel write FFilterPanel;
    property Grid: TsmxControlCellSetting read FGrid write FGrid;
    property Request: TsmxRequestSetting read FRequest write FRequest;
  end;

  { TsmxPageCfg }

  TsmxPageCfg = class(TsmxCellCfg)
  private
    FPageCaption: String;
    FPageImageIndex: Integer;
    FSections: TsmxVisibleUnits;
    function GetSections: TsmxVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property PageCaption: String read FPageCaption write FPageCaption;
    property PageImageIndex: Integer read FPageImageIndex write FPageImageIndex;
    property Sections: TsmxVisibleUnits read GetSections;
  end;

  { TsmxPageManagerCfg }

  TsmxPageManagerCfg = class(TsmxCellCfg)
  private
    FSheets: TsmxVisibleUnits;
    function GetSheets: TsmxVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property Sheets: TsmxVisibleUnits read GetSheets;
  end;

  { TsmxMenuPointCfg }

  TsmxMenuPointCfg = class(TsmxCellCfg)
  private
    FItemCaption: String;
    FItemImageIndex: Integer;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property ItemCaption: String read FItemCaption write FItemCaption;
    property ItemImageIndex: Integer read FItemImageIndex write FItemImageIndex;
  end;

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
    function GetItem(Index: Integer): TsmxHVisibleUnit;
    function GetParent: TsmxHVisibleUnit;
  public
    constructor Create(AHKit: TsmxHKit); override;
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
  private
    function GetRoot: TsmxHVisibleUnit;
  public
    property Root: TsmxHVisibleUnit read GetRoot;
  end;

  { TsmxMasterMenuCfg }

  TsmxMasterMenuCfg = class(TsmxCellCfg)
  private
    FMenuUnits: TsmxHVisibleUnits;
    function GetMenuUnits: TsmxHVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property MenuUnits: TsmxHVisibleUnits read GetMenuUnits;
  end;

  { TsmxToolBoardCfg }

  TsmxToolBoardCfg = class(TsmxCellCfg)
  private
    FBarFlat: Boolean;
    FBarShowCaptions: Boolean;
    FBarShowHint: Boolean;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property BarFlat: Boolean read FBarFlat write FBarFlat;
    property BarShowCaptions: Boolean read FBarShowCaptions write FBarShowCaptions;
    property BarShowHint: Boolean read FBarShowHint write FBarShowHint;
  end;

  { TsmxControlBoardCfg }

  TsmxControlBoardCfg = class(TsmxCellCfg)
  private
    FBarUnits: TsmxVisibleUnits;
    function GetBarUnits: TsmxVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    procedure Clear; override;

    property BarUnits: TsmxVisibleUnits read GetBarUnits;
  end;

  { TsmxFormCfg }

  TsmxFormCfg = class(TsmxCellCfg)
  private
    FAlgorithmList: TsmxAlgorithmListSetting;
    FControlBar: TsmxControlCellSetting;
    FFormCaption: String;
    FFormImageIndex: Integer;
    FFormPositionSize: TsmxPositionSize;
    FMainMenu: TsmxControlCellSetting;
    FStateRequest: TsmxRequestSetting;
    FPageManagers: TsmxVisibleUnits;
    FStatusBar: TsmxControlCellSetting;
    function GetPageManagers: TsmxVisibleUnits;
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property AlgorithmList: TsmxAlgorithmListSetting read FAlgorithmList write FAlgorithmList;
    property ControlBar: TsmxControlCellSetting read FControlBar write FControlBar;
    property FormCaption: String read FFormCaption write FFormCaption;
    property FormImageIndex: Integer read FFormImageIndex write FFormImageIndex;
    property FormPositionSize: TsmxPositionSize read FFormPositionSize write FFormPositionSize;
    property MainMenu: TsmxControlCellSetting read FMainMenu write FMainMenu;
    property PageManagers: TsmxVisibleUnits read GetPageManagers;
    property StateRequest: TsmxRequestSetting read FStateRequest write FStateRequest;
    property StatusBar: TsmxControlCellSetting read FStatusBar write FStatusBar;
  end;

implementation

uses
  XMLIntf, SysUtils, Variants, smxFuncs, smxConsts;

{ TsmxLocationParam }

constructor TsmxLocationParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamLocation := plInput;
  FParamName := '';
  FParamDefValue := Null;
end;

{ TsmxLocationParams }

function TsmxLocationParams.Add: TsmxLocationParam;
begin
  Result := TsmxLocationParam(inherited Add);
end;

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

function TsmxRequestFields.GetItem(Index: Integer): TsmxRequestField;
begin
  Result := TsmxRequestField(inherited Items[Index]);
end;

{ TsmxRequestCfg }

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
  FPerformanceMode := pmOpen;
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

procedure TsmxRequestCfg.ReadCfg;
var
  r, n: IXMLNode; i: Integer;
begin
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

procedure TsmxColumnCfg.ReadCfg;
var
  r, n, n2, n3: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Column');
  if Assigned(n) then
  begin
    ColumnFieldName := n.Attributes['FieldName'];
    with ColumnText do
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
    end;
    n2 := n.ChildNodes.FindNode('Title');
    if Assigned(n2) then
      with ColumnTitle do
      begin
        Text := n2.Attributes['Text'];
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
end;

procedure TsmxColumnCfg.WriteCfg;
var
  r, n, n2, n3: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Column');
  n.Attributes['FieldName'] := ColumnFieldName;
  with ColumnText do
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
  end;

  n2 := n.AddChild('Title');
  with ColumnTitle do
  begin
    n2.Attributes['Text'] := Text;
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

{ TsmxVisibleUnit }

constructor TsmxVisibleUnit.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FCfgID := 0;
  FUnitAlign := alClient;
  FUnitEnable := False;
  FUnitHeight := 0;
  FUnitLeft := 0;
  FUnitTop := 0;
  FUnitVisible := False;
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

{ TsmxGridCfg }

destructor TsmxGridCfg.Destroy;
begin
  if Assigned(FGridColumns) then
    FGridColumns.Free;
  inherited Destroy;
end;

procedure TsmxGridCfg.Clear;
begin
  GridColLines := False;
  GridRowLines := False;
  GridRowSelect := False;
  GridColumns.Clear;
end;

function TsmxGridCfg.GetGridColumns: TsmxVisibleUnits;
begin
  if not Assigned(FGridColumns) then
    FGridColumns := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FGridColumns;
end;

procedure TsmxGridCfg.ReadCfg;
var
  r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Grid');
  if Assigned(n) then
  begin
    GridColLines := n.Attributes['ColLines'];
    GridRowLines := n.Attributes['RowLines'];
    GridRowSelect := n.Attributes['RowSelect'];
  end;

  n := r.ChildNodes.FindNode('Columns');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Column' then
        with GridColumns.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          UnitAlign := n.ChildNodes[i].Attributes['Align'];
          UnitEnable := n.ChildNodes[i].Attributes['Enable'];
          UnitHeight := n.ChildNodes[i].Attributes['Height'];
          UnitLeft := n.ChildNodes[i].Attributes['Left'];
          UnitTop := n.ChildNodes[i].Attributes['Top'];
          UnitVisible := n.ChildNodes[i].Attributes['Visible'];
          UnitWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxGridCfg.WriteCfg;
var
  r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Grid');
  n.Attributes['ColLines'] := GridColLines;
  n.Attributes['RowLines'] := GridRowLines;
  n.Attributes['RowSelect'] := GridRowSelect;

  n := r.AddChild('Columns');
  for i := 0 to GridColumns.Count - 1 do
    with n.AddChild('Column') do
    begin
      Attributes['CfgID'] := GridColumns[i].CfgID;
      Attributes['Align'] := GridColumns[i].UnitAlign;
      Attributes['Enable'] := GridColumns[i].UnitEnable;
      Attributes['Height'] := GridColumns[i].UnitHeight;
      Attributes['Left'] := GridColumns[i].UnitLeft;
      Attributes['Top'] := GridColumns[i].UnitTop;
      Attributes['Visible'] := GridColumns[i].UnitVisible;
      Attributes['Width'] := GridColumns[i].UnitWidth;
    end;
end;

{ TsmxLibAlgorithmCfg }

procedure TsmxLibAlgorithmCfg.Clear;
begin
  FAlgDefCaption := '';
  FAlgDefHint := '';
  FAlgDefHotKey := 0;
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

procedure TsmxLibAlgorithmCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Algorithm');
  if Assigned(n) then
  begin
    AlgLibrary := n.Attributes['Library'];
    AlgProcedure := n.Attributes['Procedure'];
    AlgDefCaption := n.Attributes['DefCaption'];
    AlgDefHotKey := n.Attributes['DefHotKey'];
    AlgDefHint := n.Attributes['DefHint'];
    AlgImageIndex := n.Attributes['ImageIndex'];
  end;

  n := r.ChildNodes.FindNode('Params');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Param' then
        with AlgParams.Add do
        begin
          ParamName := n.ChildNodes[i].Attributes['Name'];
          ParamDefValue := VarStringToVar(n.ChildNodes[i].Attributes['DefValue']);
          ParamLocation := n.ChildNodes[i].Attributes['Location'];
        end;
  end;
end;

procedure TsmxLibAlgorithmCfg.WriteCfg;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('Algorithm');
  n.Attributes['Library'] := AlgLibrary;
  n.Attributes['Procedure'] := AlgProcedure;
  n.Attributes['DefCaption'] := AlgDefCaption;
  n.Attributes['DefHotKey'] := AlgDefHotKey;
  n.Attributes['DefHint'] := AlgDefHint;
  n.Attributes['ImageIndex'] := AlgImageIndex;

  n := r.AddChild('Params');
  for i := 0 to AlgParams.Count - 1 do
    with n.AddChild('Param') do
    begin
      Attributes['Name'] := AlgParams[i].ParamName;
      Attributes['DefValue'] := AlgParams[i].ParamDefValue;
      Attributes['Location'] := AlgParams[i].ParamLocation;
    end;
end;

{ TsmxAlgorithmItem }

constructor TsmxAlgorithmItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FCfgID := 0;
  FAlgorithmCaption := '';
  FAlgorithmEnable := False;
  FAlgorithmHotKey := 0;
  FAlgorithmHint := '';
  FAlgorithmMenuItemCfgID := 0;
  FAlgorithmToolBarCfgID := 0;
  FAlgorithmVisible := False;
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

procedure TsmxAlgorithmListCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
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

procedure TsmxAlgorithmListCfg.WriteCfg;
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
      Attributes['Enable'] := AlgorithmItems[i].AlgorithmEnable;
      Attributes['Visible'] := AlgorithmItems[i].AlgorithmVisible;
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
  with Algorithm do
  begin
    CfgID := 0;
    Caption := '';
    Enable := False;
    HotKey := 0;
    Visible := False;
  end;
end;

procedure TsmxFilterCfg.ReadCfg;
var r, n, n2: IXMLNode;
begin
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

procedure TsmxFilterCfg.WriteCfg;
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
    n.Attributes['Enable'] := Enable;
    n.Attributes['HotKey'] := HotKey;
    n.Attributes['Visible'] := Visible;
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
  with ApplyRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  with PrepareRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  Filters.Clear;
end;

function TsmxFilterDeskCfg.GetFilters: TsmxVisibleUnits;
begin
  if not Assigned(FFilters) then
    FFilters := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FFilters;
end;

procedure TsmxFilterDeskCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
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
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Filter' then
        with Filters.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          UnitAlign := n.ChildNodes[i].Attributes['Align'];
          UnitEnable := n.ChildNodes[i].Attributes['Enable'];
          UnitHeight := n.ChildNodes[i].Attributes['Height'];
          UnitLeft := n.ChildNodes[i].Attributes['Left'];
          UnitTop := n.ChildNodes[i].Attributes['Top'];
          UnitVisible := n.ChildNodes[i].Attributes['Visible'];
          UnitWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxFilterDeskCfg.WriteCfg;
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
      Attributes['Align'] := Filters[i].UnitAlign;
      Attributes['Enable'] := Filters[i].UnitEnable;
      Attributes['Height'] := Filters[i].UnitHeight;
      Attributes['Left'] := Filters[i].UnitLeft;
      Attributes['Top'] := Filters[i].UnitTop;
      Attributes['Visible'] := Filters[i].UnitVisible;
      Attributes['Width'] := Filters[i].UnitWidth;
    end;
end;

{ TsmxSectionCfg }

procedure TsmxSectionCfg.Clear;
begin
  with Request do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  with Grid do
  begin
    CfgID := 0;
    Align := alClient;
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
  with FilterPanel do
  begin
    CfgID := 0;
    Align := alTop;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 49;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
end;

procedure TsmxSectionCfg.ReadCfg;
var r, n: IXMLNode;
begin
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

procedure TsmxSectionCfg.WriteCfg;
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
    n.Attributes['Enable'] := Enable;
    n.Attributes['Visible'] := Visible;
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
    n.Attributes['Enable'] := Enable;
    n.Attributes['Visible'] := Visible;
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
  PageCaption := '';
  PageImageIndex := -1;
  Sections.Clear;
end;

function TsmxPageCfg.GetSections: TsmxVisibleUnits;
begin
  if not Assigned(FSections) then
    FSections := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FSections;
end;

procedure TsmxPageCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
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
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Section' then
        with Sections.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          UnitAlign := n.ChildNodes[i].Attributes['Align'];
          UnitEnable := n.ChildNodes[i].Attributes['Enable'];
          UnitHeight := n.ChildNodes[i].Attributes['Height'];
          UnitLeft := n.ChildNodes[i].Attributes['Left'];
          UnitTop := n.ChildNodes[i].Attributes['Top'];
          UnitVisible := n.ChildNodes[i].Attributes['Visible'];
          UnitWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxPageCfg.WriteCfg;
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
      Attributes['Align'] := Sections[i].UnitAlign;
      Attributes['Enable'] := Sections[i].UnitEnable;
      Attributes['Height'] := Sections[i].UnitHeight;
      Attributes['Left'] := Sections[i].UnitLeft;
      Attributes['Top'] := Sections[i].UnitTop;
      Attributes['Visible'] := Sections[i].UnitVisible;
      Attributes['Width'] := Sections[i].UnitWidth;
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

procedure TsmxPageManagerCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Pages');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Page' then
        with Sheets.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          UnitAlign := n.ChildNodes[i].Attributes['Align'];
          UnitEnable := n.ChildNodes[i].Attributes['Enable'];
          UnitHeight := n.ChildNodes[i].Attributes['Height'];
          UnitLeft := n.ChildNodes[i].Attributes['Left'];
          UnitTop := n.ChildNodes[i].Attributes['Top'];
          UnitVisible := n.ChildNodes[i].Attributes['Visible'];
          UnitWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxPageManagerCfg.WriteCfg;
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
      Attributes['Align'] := Sheets[i].UnitAlign;
      Attributes['Enable'] := Sheets[i].UnitEnable;
      Attributes['Height'] := Sheets[i].UnitHeight;
      Attributes['Left'] := Sheets[i].UnitLeft;
      Attributes['Top'] := Sheets[i].UnitTop;
      Attributes['Visible'] := Sheets[i].UnitVisible;
      Attributes['Width'] := Sheets[i].UnitWidth;
    end;
end;

{ TsmxMenuPointCfg }

procedure TsmxMenuPointCfg.Clear;
begin
  FItemCaption := '';
  FItemImageIndex := -1;
end;

procedure TsmxMenuPointCfg.ReadCfg;
var r, n: IXMLNode;
begin
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

procedure TsmxMenuPointCfg.WriteCfg;
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
  FUnitAlign := alNone;
  FUnitEnable := False;
  FUnitHeight := 0;
  FUnitLeft := 0;
  FUnitTop := 0;
  FUnitVisible := False;
  FUnitWidth := 0;
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

{ TsmxMasterMenuCfg }

destructor TsmxMasterMenuCfg.Destroy;
begin
  if Assigned(FMenuUnits) then
    FMenuUnits.Free;
  inherited Destroy;
end;

procedure TsmxMasterMenuCfg.Clear;
begin
  MenuUnits.Root.Clear;
end;

function TsmxMasterMenuCfg.GetMenuUnits: TsmxHVisibleUnits;
begin
  if not Assigned(FMenuUnits) then
    FMenuUnits := TsmxHVisibleUnits.Create(TsmxHVisibleUnit);
  Result := FMenuUnits;
end;

procedure TsmxMasterMenuCfg.ReadCfg;

  procedure AddUnits(const ANode: IXMLNode; AUnit: TsmxHVisibleUnit);
  var i: Integer; u: TsmxHVisibleUnit;
  begin
    u := AUnit.Add;
    with u do
    begin
      CfgID := ANode.Attributes['CfgID'];
      UnitAlign := ANode.Attributes['Align'];
      UnitEnable := ANode.Attributes['Enable'];
      UnitHeight := ANode.Attributes['Height'];
      UnitLeft := ANode.Attributes['Left'];
      UnitTop := ANode.Attributes['Top'];
      UnitVisible := ANode.Attributes['Visible'];
      UnitWidth := ANode.Attributes['Width'];
    end;
    for i := 0 to ANode.ChildNodes.Count - 1 do
      if ANode.ChildNodes[i].NodeName = 'MenuItem' then
        AddUnits(ANode.ChildNodes[i], u);
  end;

var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('MenuItems');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'MenuItem' then
        AddUnits(n.ChildNodes[i], MenuUnits.Root);
  end; 
end;

procedure TsmxMasterMenuCfg.WriteCfg;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxHVisibleUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('MenuItem');
    with n do
    begin
      Attributes['CfgID'] := AUnit.CfgID;
      Attributes['Align'] := AUnit.UnitAlign;
      Attributes['Enable'] := AUnit.UnitEnable;
      Attributes['Height'] := AUnit.UnitHeight;
      Attributes['Left'] := AUnit.UnitLeft;
      Attributes['Top'] := AUnit.UnitTop;
      Attributes['Visible'] := AUnit.UnitVisible;
      Attributes['Width'] := AUnit.UnitWidth;
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

procedure TsmxToolBoardCfg.ReadCfg;
var r, n: IXMLNode;
begin
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

procedure TsmxToolBoardCfg.WriteCfg;
var r, n: IXMLNode;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if Assigned(r) then
    r.ChildNodes.Clear else
    r := XMLDoc.AddChild('Root');

  n := r.AddChild('ToolBar');
  n.Attributes['Flat'] := BarFlat;
  n.Attributes['ShowCaptions'] := BarShowCaptions;
  n.Attributes['ShowHint'] := BarShowHint;
end;

{ TsmxControlBoardCfg }

procedure TsmxControlBoardCfg.Clear;
begin
  BarUnits.Clear;
end;

function TsmxControlBoardCfg.GetBarUnits: TsmxVisibleUnits;
begin
  if not Assigned(FBarUnits) then
    FBarUnits := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FBarUnits;
end;

procedure TsmxControlBoardCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
  r := XMLDoc.ChildNodes.FindNode('Root');
  if not Assigned(r) then
    Exit;

  n := r.ChildNodes.FindNode('Bars');
  if Assigned(n) and (n.ChildNodes.Count > 0) then
  begin
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'Bar' then
        with BarUnits.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          UnitAlign := n.ChildNodes[i].Attributes['Align'];
          UnitEnable := n.ChildNodes[i].Attributes['Enable'];
          UnitHeight := n.ChildNodes[i].Attributes['Height'];
          UnitLeft := n.ChildNodes[i].Attributes['Left'];
          UnitTop := n.ChildNodes[i].Attributes['Top'];
          UnitVisible := n.ChildNodes[i].Attributes['Visible'];
          UnitWidth := n.ChildNodes[i].Attributes['Width'];
        end;
  end;
end;

procedure TsmxControlBoardCfg.WriteCfg;
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
      Attributes['Align'] := BarUnits[i].UnitAlign;
      Attributes['Enable'] := BarUnits[i].UnitEnable;
      Attributes['Height'] := BarUnits[i].UnitHeight;
      Attributes['Left'] := BarUnits[i].UnitLeft;
      Attributes['Top'] := BarUnits[i].UnitTop;
      Attributes['Visible'] := BarUnits[i].UnitVisible;
      Attributes['Width'] := BarUnits[i].UnitWidth;
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
  FormCaption := '';
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
    Enable := False;
    Visible := False;
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
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 21;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  with StateRequest do
  begin
    CfgID := 0;
    Operation := omManual;
    DatabaseName := '';
  end;
  with StatusBar do
  begin
    CfgID := 0;
    Align := alBottom;
    Enable := False;
    Visible := False;
    with PositionSize do
    begin
      Height := 19;
      Left := 0;
      Top := 0;
      Width := 0;
    end;
  end;
  PageManagers.Clear;
end;

function TsmxFormCfg.GetPageManagers: TsmxVisibleUnits;
begin
  if not Assigned(FPageManagers) then
    FPageManagers := TsmxVisibleUnits.Create(TsmxVisibleUnit);
  Result := FPageManagers;
end;

procedure TsmxFormCfg.ReadCfg;
var r, n: IXMLNode; i: Integer;
begin
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
    for i := 0 to n.ChildNodes.Count - 1 do
      if n.ChildNodes[i].NodeName = 'PageManager' then
        with PageManagers.Add do
        begin
          CfgID := n.ChildNodes[i].Attributes['CfgID'];
          UnitAlign := n.ChildNodes[i].Attributes['Align'];
          UnitEnable := n.ChildNodes[i].Attributes['Enable'];
          UnitHeight := n.ChildNodes[i].Attributes['Height'];
          UnitLeft := n.ChildNodes[i].Attributes['Left'];
          UnitTop := n.ChildNodes[i].Attributes['Top'];
          UnitVisible := n.ChildNodes[i].Attributes['Visible'];
          UnitWidth := n.ChildNodes[i].Attributes['Width'];
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

procedure TsmxFormCfg.WriteCfg;
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
      Attributes['Align'] := PageManagers[i].UnitAlign;
      Attributes['Enable'] := PageManagers[i].UnitEnable;
      Attributes['Height'] := PageManagers[i].UnitHeight;
      Attributes['Left'] := PageManagers[i].UnitLeft;
      Attributes['Top'] := PageManagers[i].UnitTop;
      Attributes['Visible'] := PageManagers[i].UnitVisible;
      Attributes['Width'] := PageManagers[i].UnitWidth;
    end;

  n := r.AddChild('MainMenu');
  with MainMenu do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := BoolToStr(Enable, True);
    n.Attributes['Visible'] := Visible;
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
    n.Attributes['IsCreateMenuItem'] := IsCreateMenuItem;
    n.Attributes['IsCreateToolButton'] := IsCreateToolButton;
  end;

  n := r.AddChild('ControlBar');
  with ControlBar do
  begin
    n.Attributes['CfgID'] := CfgID;
    n.Attributes['Align'] := Align;
    n.Attributes['Enable'] := Enable;
    n.Attributes['Visible'] := Visible;
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
    n.Attributes['Enable'] := Enable;
    n.Attributes['Visible'] := Visible;
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
    TsmxPageCfg, TsmxPageManagerCfg, TsmxMenuPointCfg, TsmxMasterMenuCfg,
    TsmxToolBoardCfg, TsmxControlBoardCfg, TsmxFormCfg]);

finalization
  UnRegisterClasses([TsmxRequestCfg, TsmxColumnCfg, TsmxGridCfg, TsmxLibAlgorithmCfg,
    TsmxAlgorithmListCfg,TsmxFilterCfg, TsmxFilterDeskCfg, TsmxSectionCfg,
    TsmxPageCfg, TsmxPageManagerCfg, TsmxMenuPointCfg, TsmxMasterMenuCfg,
    TsmxToolBoardCfg, TsmxControlBoardCfg, TsmxFormCfg]);

end.
