unit smxVTCells;

interface

uses
  Classes, Controls, Graphics, VirtualTrees, Types, Messages, StdCtrls,
  smxBaseClasses, smxClasses, smxTypes, smxBaseTypes, smxClassIntf;

type
  { TsmxVTColumn }

  TsmxVTColumn = class(TsmxCustomColumn)
  private
    //FIsEditing: Boolean;
    FVTColumn: TVirtualTreeColumn;
    FVTColumnFont: TFont;
    FVTHeaderColor: TColor;
    FVTHeaderFont: TFont;
    procedure ChangeVTColumnFont(Sender: TObject);
    procedure ChangeVTHeaderFont(Sender: TObject);
    procedure CreateVTColumn;
    procedure DestroyVTColumn;
    function GetVTColumn: TVirtualTreeColumn;
    function GetVTColumnFont: TFont;
    function GetVTHeaderFont: TFont;
    procedure InvalidateColumn;
    procedure InvalidateHeader;
  protected
    //procedure DoSnapHeader; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetColumnAlignment: TAlignment; override;
    function GetColumnCaption: String; override;
    function GetColumnColor: TColor; override;
    function GetColumnFont: TFont; override;
    function GetColumnValue: Variant; override;
    function GetHeaderAlignment: TAlignment; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetHeaderCaption: String; override;
    //function GetIsEditing: Boolean; override;
    function GetInternalRef: Pointer; override;
    //procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetColumnAlignment(Value: TAlignment); override;
    procedure SetColumnCaption(const Value: String); override;
    procedure SetColumnColor(Value: TColor); override;
    procedure SetColumnFont(Value: TFont); override;
    //procedure SetColumnOptions(Value: TsmxColumnOptions); override;
    procedure SetColumnValue(const Value: Variant); override;
    procedure SetCellFeedBack; override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderCaption(const Value: String); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;
    procedure ChangeObjectIndex(Value: Integer); override;

    property VTColumn: TVirtualTreeColumn read GetVTColumn;
    property VTColumnFont: TFont read GetVTColumnFont;
    property VTHeaderColor: TColor read FVTHeaderColor;
    property VTHeaderFont: TFont read GetVTHeaderFont;
  public
    destructor Destroy; override;
  published
    property ColumnAlignment;
    property ColumnColor;
    property ColumnFont;
    property ColumnOptions;
    property CellVisible;
    property CellWidth;
    property HeaderAlignment;
    property HeaderCaption;
    property HeaderColor;
    property HeaderFont;

    property OnClick;
    property OnClickHeader;
  end;

  { TsmxVTNode }

  TsmxVTNodes = class;

  TsmxVTNode = class(TsmxParam)
  private
    FColIndex: Integer;
    FNode: PVirtualNode;
    function GetKit: TsmxVTNodes;
    procedure SetKit(Value: TsmxVTNodes);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property ColIndex: Integer read FColIndex write FColIndex;
    property Kit: TsmxVTNodes read GetKit write SetKit;
    property Node: PVirtualNode read FNode write FNode;
  end;

  { TsmxVTNodes }

  TsmxVTNodes = class(TsmxParams)
  private
    function GetItem(Index: Integer): TsmxVTNode;
    procedure SetItem(Index: Integer; Value: TsmxVTNode);
    function GetNodeCaption(ColIndex: Integer; Node: PVirtualNode): String;
    function GetNodeValue(ColIndex: Integer; Node: PVirtualNode): Variant;
    procedure SetNodeCaption(ColIndex: Integer; Node: PVirtualNode; const Value: String);
    procedure SetNodeValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
  public
    function Add: TsmxVTNode;
    function FindNodeByIndex(ColIndex: Integer; Node: PVirtualNode): TsmxVTNode;

    property Items[Index: Integer]: TsmxVTNode read GetItem write SetItem; default;
    property NodeCaptions[ColIndex: Integer; Node: PVirtualNode]: String read GetNodeCaption write SetNodeCaption;
    property NodeValues[ColIndex: Integer; Node: PVirtualNode]: Variant read GetNodeValue write SetNodeValue;
  end;

  { TsmxVTEditor }

  TsmxVTEditor = class(TsmxInterfacedPersistent, IsmxOuterEditor, IsmxTreeEditor, IVTEditLink)
  private
    FControl: TWinControl;
    FTree: TVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: Integer;
    FEditorType: TsmxEditorType;
  protected
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetColIndex: Integer;
    function GetControl: TWinControl;
    function GetHolder: TWinControl;
    function GetRowIndex: Pointer;
    function GetEditorType: TsmxEditorType;
    procedure SetEditorType(Value: TsmxEditorType);
    //procedure SetControl(Value: TWinControl); virtual;
  public
    //destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;

    property ColIndex: Integer read GetColIndex;
    property Control: TWinControl read GetControl;// write SetControl;
    property EditorType: TsmxEditorType read GetEditorType write SetEditorType;
    property Holder: TWinControl read GetHolder;
    property RowIndex: Pointer read GetRowIndex;
    //property Tree: TVirtualStringTree read FTree;
    //property Column: Integer read FColumn;
    //property Node: PVirtualNode read FNode;

    //property OnEditing: TNotifyEvent read FOnEditing write FOnEditing;
    //property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
  end;

  //TsmxNodeProp = (npCaption, npValue);

  { TsmxVTGrid }

  TsmxVTGrid = class(TsmxCustomGrid)
  private
    //FNodeProp: TsmxNodeProp;
    FVTGrid: TVirtualStringTree;
    FVTNodes: TsmxVTNodes;
    function GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
    function GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
    function GetVTGrid: TVirtualStringTree;
    function GetVTNodes: TsmxVTNodes;
    function RowIndexToNode(RowIndex: Integer): PVirtualNode;
    procedure SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
    procedure SetDBValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
    function SetRecordNoByNode(Node: PVirtualNode): Boolean;
    procedure VTGridAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements);
    procedure VTGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTGridColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
    procedure VTGridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure VTGridFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
    procedure VTGridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VTGridHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure VTGridNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: UnicodeString);
    procedure VTGridPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  protected
    //procedure DoApply; override;
    //procedure DoChangeRow; override;
    //procedure DoPrepare; override;
    //procedure DoRefresh; override;
    function GetCellHint: String; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetFocusedRowIndex: Integer; override;
    function GetFocusedColIndex: Integer; override;
    function GetGridCaption(ColIndex, RowIndex: Integer): String; override;
    function GetGridValue(ColIndex, RowIndex: Integer): Variant; override;
    function GetInternalRef: Pointer; override;
    function GetRowCount: Integer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    //procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    procedure SetCellHint(const Value: String); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Integer); override;
    procedure SetGridCaption(ColIndex, RowIndex: Integer; const Value: String); override;
    procedure SetGridOptions(Value: TsmxGridOptions); override;
    procedure SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant); override;
    procedure SetRowCount(Value: Integer); override;

    property DBText[ColIndex: Integer; Node: PVirtualNode]: String read GetDBText write SetDBText;
    property DBValue[ColIndex: Integer; Node: PVirtualNode]: Variant read GetDBValue write SetDBValue;
    property VTGrid: TVirtualStringTree read GetVTGrid;
    property VTNodes: TsmxVTNodes read GetVTNodes;
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
    property SlaveList;

    property OnChangeRow;
    property OnDoubleClick;
  end;

  { TsmxVTTree }

  TsmxVTTree = class(TsmxCustomTree)
  private
    //FNodeProp: TsmxNodeProp;
    FVTTree: TVirtualStringTree;
    FVTNodes: TsmxVTNodes;
    //FOnEdited: TsmxComponentEvent;
    //FOnEditing: TsmxComponentEvent;
    //FOnEdited: TsmxComponentEvent;
    //FOnEditing: TsmxComponentEvent;
    FEditor: TsmxVTEditor;
    function GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
    function GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
    function GetVTTree: TVirtualStringTree;
    function GetVTNodes: TsmxVTNodes;
    //function RowIndexToNode(RowIndex: Integer): PVirtualNode;
    procedure SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
    procedure SetDBValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
    function SetRecordNoByNode(Node: PVirtualNode): Boolean;
    procedure VTTreeAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements);
    procedure VTTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTTreeCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTTreeColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
    procedure VTTreeCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VTTreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure VTTreeEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VTTreeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
    procedure VTTreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VTTreeHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure VTTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: UnicodeString);
    procedure VTTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  protected
    //procedure DoApply; override;
    //procedure DoChangeRow; override;
    //procedure DoCollapse; override;
    //procedure DoExpand; override;
    //procedure DoEdited; virtual;
    //procedure DoEditing; virtual;
    function GetCellHint: String; override;
    //function GetCfgClass: TsmxBaseCfgClass; override;
    function GetExpanded(RowIndex: Pointer): Boolean; override;
    function GetFocusedRowIndex: Pointer; override;
    function GetFocusedColIndex: Integer; override;
    function GetInternalRef: Pointer; override;
    function GetEditor: IsmxTreeEditor; override;
    //function GetEditorClass: TsmxInterfacedPersistentClass; override;
    function GetParentRow(RowIndex: Pointer): Pointer; override;
    function GetRootRow: Pointer; override;
    function GetRow(RowIndex: Pointer; Index: Integer): Pointer; override;
    function GetRowCount(RowIndex: Pointer): Integer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    function GetTreeCaption(ColIndex: Integer; RowIndex: Pointer): String; override;
    function GetTreeValue(ColIndex: Integer; RowIndex: Pointer): Variant; override;
    //procedure InternalPrepare; override;
    //procedure InternalRefresh; override;
    //procedure InternalEdited; virtual;
    //procedure InternalEditing; virtual;
    //procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    //procedure SetCellProps; override;
    procedure SetCellHint(const Value: String); override;
    procedure SetExpanded(RowIndex: Pointer; Value: Boolean); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Pointer); override;
    procedure SetParentRow(RowIndex, Value: Pointer); override;
    //procedure SetRow(RowIndex: Pointer; Index: Integer; Value: Pointer); override;
    procedure SetRowCount(RowIndex: Pointer; Value: Integer); override;
    procedure SetTreeCaption(ColIndex: Integer; RowIndex: Pointer; const Value: String); override;
    procedure SetTreeOptions(Value: TsmxTreeOptions); override;
    procedure SetTreeValue(ColIndex: Integer; RowIndex: Pointer; const Value: Variant); override;

    property DBText[ColIndex: Integer; Node: PVirtualNode]: String read GetDBText write SetDBText;
    property DBValue[ColIndex: Integer; Node: PVirtualNode]: Variant read GetDBValue write SetDBValue;
    property VTTree: TVirtualStringTree read GetVTTree;
    property VTNodes: TsmxVTNodes read GetVTNodes;
  public
    destructor Destroy; override;
    function AddRow(RowIndex: Pointer): Pointer; override;
    procedure DelRow(RowIndex: Pointer); override;
    function RowLevel(RowIndex: Pointer): Integer; override;
    //procedure Edited;
    //procedure Editing;

    //property Editor: TsmxVTEditor read GetEditor;
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
    property PopupMenu;
    property Request;
    property SlaveList;
    property TreeOptions;

    property OnChangeRow;
    property OnCollapse;
    property OnDoubleClick;
    property OnEdited;
    property OnEditing;
    property OnExpand;
  end;

implementation

uses
  Windows, Variants, Forms, TypInfo, {smxCfgs,} smxStdCtrls, smxFuncs, smxProcs,
  smxClassFuncs, smxDBFuncs, smxDBIntf, smxBaseIntf{, Mask};

//type
  { _TsmxBaseCell }

  //_TsmxBaseCell = class(TsmxBaseCell)
  //end;

{ TsmxVTColumn }

destructor TsmxVTColumn.Destroy;
begin
  inherited Destroy;
  if Assigned(FVTColumnFont) then
    FVTColumnFont.Free;
  if Assigned(FVTHeaderFont) then
    FVTHeaderFont.Free;
  if Assigned(FVTColumn) then
    DestroyVTColumn;
end;

procedure TsmxVTColumn.ChangeVTColumnFont(Sender: TObject);
begin
  InvalidateColumn;
end;

procedure TsmxVTColumn.ChangeVTHeaderFont(Sender: TObject);
begin
  InvalidateHeader;
end;

procedure TsmxVTColumn.CreateVTColumn;
var
  VT: TVirtualStringTree;
begin
  VT := TVirtualStringTree.Create(nil);
  try
    FVTColumn := TVirtualTreeColumn.Create(VT.Header.Columns);
    FVTColumn.Collection := nil;
  finally
    VT.Free;
  end;
end;

procedure TsmxVTColumn.DestroyVTColumn;
var
  VT: TVirtualStringTree;
begin
  VT := TVirtualStringTree.Create(nil);
  try
    FVTColumn.Collection := VT.Header.Columns;
    FVTColumn.Free;
  finally
    VT.Free;
  end;
end;

{procedure TsmxVTColumn.DoSnapHeader;
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
end;}

function TsmxVTColumn.GetCellVisible: Boolean;
begin
  Result := coVisible in VTColumn.Options;
end;

procedure TsmxVTColumn.SetCellVisible(Value: Boolean);
var
  VT: TVirtualStringTree;
begin
  if GetCellVisible <> Value then
  begin
    VT := nil;
    try
      if not Assigned(VTColumn.Collection) then
      begin
        VT := TVirtualStringTree.Create(nil);
        VTColumn.Collection := VT.Header.Columns;
      end;
      if Value then
        VTColumn.Options := VTColumn.Options + [coVisible] else
        VTColumn.Options := VTColumn.Options - [coVisible];
    finally
      if Assigned(VT) then
      begin
        VTColumn.Collection := nil;
        VT.Free;
      end;
    end;
  end;
end;

function TsmxVTColumn.GetCellWidth: Integer;
begin
  Result := VTColumn.Width;
end;

procedure TsmxVTColumn.SetCellWidth(Value: Integer);
var
  VT: TVirtualStringTree;
begin
  if GetCellWidth <> Value then
  begin
    VT := nil;
    try
      if not Assigned(VTColumn.Collection) then
      begin
        VT := TVirtualStringTree.Create(nil);
        VTColumn.Collection := VT.Header.Columns;
      end;
      VTColumn.Width := Value;
    finally
      if Assigned(VT) then
      begin
        VTColumn.Collection := nil;
        VT.Free;
      end;
    end;
  end;
end;

{function TsmxVTColumn.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxColumnCfg;
end;}

function TsmxVTColumn.GetColumnAlignment: TAlignment;
begin
  Result := VTColumn.Alignment;
end;

procedure TsmxVTColumn.SetColumnAlignment(Value: TAlignment);
var
  VT: TVirtualStringTree;
begin
  if GetColumnAlignment <> Value then
  begin
    VT := nil;
    try
      if not Assigned(VTColumn.Collection) then
      begin
        VT := TVirtualStringTree.Create(nil);
        VTColumn.Collection := VT.Header.Columns;
      end;
      VTColumn.Alignment := Value;
    finally
      if Assigned(VT) then
      begin
        VTColumn.Collection := nil;
        VT.Free;
      end;
    end;
  end;
end;

function TsmxVTColumn.GetColumnCaption: String;
begin
  Result := '';
  if CellOwner is TsmxCustomGrid then
  begin
    if TsmxCustomGrid(CellOwner).FocusedRowIndex <> -1 then
      Result := TsmxCustomGrid(CellOwner).GridCaptions[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
  end else
  if CellOwner is TsmxCustomTree then
  begin
    if Assigned(TsmxCustomTree(CellOwner).FocusedRowIndex) then
      Result := TsmxCustomTree(CellOwner).TreeCaptions[SlaveIndex, TsmxCustomTree(CellOwner).FocusedRowIndex];
  end;
end;

procedure TsmxVTColumn.SetColumnCaption(const Value: String);
//var
  //Val: Variant;
begin
  //Val := smxFuncs.StrToVar(Value);
  //SetFieldValue(smxFuncs.GetTextFieldName(SlaveName), Val);
  if CellOwner is TsmxCustomGrid then
  begin
    if TsmxCustomGrid(CellOwner).FocusedRowIndex <> -1 then
      TsmxCustomGrid(CellOwner).GridCaptions[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
  end else
  if CellOwner is TsmxCustomTree then
  begin
    if Assigned(TsmxCustomTree(CellOwner).FocusedRowIndex) then
      TsmxCustomTree(CellOwner).TreeCaptions[SlaveIndex, TsmxCustomTree(CellOwner).FocusedRowIndex] := Value;
  end;
end;

function TsmxVTColumn.GetColumnColor: TColor;
begin
  Result := VTColumn.Color;
end;

procedure TsmxVTColumn.SetColumnColor(Value: TColor);
var
  VT: TVirtualStringTree;
begin
  if GetColumnColor <> Value then
  begin
    VT := nil;
    try
      if not Assigned(VTColumn.Collection) then
      begin
        VT := TVirtualStringTree.Create(nil);
        VTColumn.Collection := VT.Header.Columns;
      end;
      VTColumn.Color := Value;
    finally
      if Assigned(VT) then
      begin
        VTColumn.Collection := nil;
        VT.Free;
      end;
    end;
  end;
end;

function TsmxVTColumn.GetColumnFont: TFont;
begin
  Result := VTColumnFont;
end;

procedure TsmxVTColumn.SetColumnFont(Value: TFont);
begin
  VTColumnFont.Assign(Value);
end;

function TsmxVTColumn.GetColumnValue: Variant;
begin
  Result := Variants.Null;
  //if coSetValue in ColumnOptions then
  //begin
    if CellOwner is TsmxCustomGrid then
    begin
      if TsmxCustomGrid(CellOwner).FocusedRowIndex <> -1 then
        Result := TsmxCustomGrid(CellOwner).GridValues[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
    end else
    if CellOwner is TsmxCustomTree then
    begin
      if Assigned(TsmxCustomTree(CellOwner).FocusedRowIndex) then
        Result := TsmxCustomTree(CellOwner).TreeValues[SlaveIndex, TsmxCustomTree(CellOwner).FocusedRowIndex];
    end;
  //end;
end;

procedure TsmxVTColumn.SetColumnValue(const Value: Variant);
begin
  //SetFieldValue(SlaveName, Value);
  //if coSetValue in ColumnOptions then
  //begin
    if CellOwner is TsmxCustomGrid then
    begin
      if TsmxCustomGrid(CellOwner).FocusedRowIndex <> -1 then
        TsmxCustomGrid(CellOwner).GridValues[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
    end else
    if CellOwner is TsmxCustomTree then
    begin
      if Assigned(TsmxCustomTree(CellOwner).FocusedRowIndex) then
        TsmxCustomTree(CellOwner).TreeValues[SlaveIndex, TsmxCustomTree(CellOwner).FocusedRowIndex] := Value;
    end;
  //end;
end;

function TsmxVTColumn.GetHeaderAlignment: TAlignment;
begin
  Result := VTColumn.CaptionAlignment;
end;

procedure TsmxVTColumn.SetHeaderAlignment(Value: TAlignment);
var
  VT: TVirtualStringTree;
begin
  if GetHeaderAlignment <> Value then
  begin
    VT := nil;
    try
      if not Assigned(VTColumn.Collection) then
      begin
        VT := TVirtualStringTree.Create(nil);
        VTColumn.Collection := VT.Header.Columns;
      end;
      VTColumn.CaptionAlignment := Value;
      VTColumn.Options := VTColumn.Options + [coUseCaptionAlignment];
    finally
      if Assigned(VT) then
      begin
        VTColumn.Collection := nil;
        VT.Free;
      end;
    end;  
  end;
end;

function TsmxVTColumn.GetHeaderCaption: String;
begin
  Result := String(VTColumn.Text);
end;

procedure TsmxVTColumn.SetHeaderCaption(const Value: String);
begin
  VTColumn.Text := WideString(Value);
end;

function TsmxVTColumn.GetHeaderColor: TColor;
begin
  Result := VTHeaderColor;
end;

procedure TsmxVTColumn.SetHeaderColor(Value: TColor);
begin
  FVTHeaderColor := Value;
  InvalidateHeader;
end;

function TsmxVTColumn.GetHeaderFont: TFont;
begin
  Result := VTHeaderFont;
end;

procedure TsmxVTColumn.SetHeaderFont(Value: TFont);
begin
  VTHeaderFont.Assign(Value);
end;

function TsmxVTColumn.GetInternalRef: Pointer;
begin
  Result := Pointer(VTColumn);
end;

{function TsmxVTColumn.GetIsEditing: Boolean;
begin
  Result := FIsEditing;
end;

procedure TsmxVTColumn.SetIsEditing(Value: Boolean);
begin
  FIsEditing := Value;
end;}

function TsmxVTColumn.GetVTColumn: TVirtualTreeColumn;
begin
  if not Assigned(FVTColumn) then
    CreateVTColumn;
  Result := FVTColumn;
end;

function TsmxVTColumn.GetVTColumnFont: TFont;
begin
  if not Assigned(FVTColumnFont) then
  begin
    FVTColumnFont := TFont.Create;
    FVTColumnFont.OnChange := ChangeVTColumnFont;
  end;
  Result := FVTColumnFont;
end;

function TsmxVTColumn.GetVTHeaderFont: TFont;
begin
  if not Assigned(FVTHeaderFont) then
  begin
    FVTHeaderFont := TFont.Create;
    FVTHeaderFont.OnChange := ChangeVTHeaderFont;
  end;
  Result := FVTHeaderFont;
end;

procedure TsmxVTColumn.InvalidateHeader;
begin
  if Assigned(CellOwner) then
    if {_TsmxBaseCell}TObject((CellOwner as IsmxRefComponent).GetInternalRef) is TVirtualStringTree then
      if TVirtualStringTree((CellOwner as IsmxRefComponent).GetInternalRef).HandleAllocated then
        TVirtualStringTree((CellOwner as IsmxRefComponent).GetInternalRef).Header.Invalidate(VTColumn);
end;

procedure TsmxVTColumn.InvalidateColumn;
begin
  if Assigned(CellOwner) then
    if TObject((CellOwner as IsmxRefComponent).GetInternalRef) is TVirtualStringTree then
      if TVirtualStringTree((CellOwner as IsmxRefComponent).GetInternalRef).HandleAllocated then
        TVirtualStringTree((CellOwner as IsmxRefComponent).GetInternalRef).InvalidateColumn(VTColumn.Index);
end;

{procedure TsmxVTColumn.ResetCellProps;
begin
  inherited ResetCellProps;
  ColumnAlignment := taLeftJustify;
  //ColumnCaption := TsmxColumnCfg(Cfg).ColumnText.Caption;
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
end;}

procedure TsmxVTColumn.SetCellFeedBack;
begin
  VTColumn.Tag := Integer(Self);
end;

procedure TsmxVTColumn.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    VTColumn.Collection := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TVirtualStringTree then
      VTColumn.Collection := TVirtualStringTree(Obj).Header.Columns;
  end;
end;

{procedure TsmxVTColumn.SetCellProps;
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
end;}

procedure TsmxVTColumn.ChangeObjectIndex(Value: Integer);
begin
  VTColumn.Index := Value;
end;

{ TsmxVTNode }

procedure TsmxVTNode.Assign(Source: TsmxKitItem);
begin
  inherited Assign(Source);
  if Source is TsmxVTNode then
  begin
    ColIndex := TsmxVTNode(Source).ColIndex;
    Node := TsmxVTNode(Source).Node;
  end;
end;

function TsmxVTNode.GetKit: TsmxVTNodes;
begin
  Result := TsmxVTNodes(inherited Kit);
end;

procedure TsmxVTNode.SetKit(Value: TsmxVTNodes);
begin
  inherited Kit := Value;
end;

{ TsmxVTNodes }

function TsmxVTNodes.Add: TsmxVTNode;
begin
  Result := TsmxVTNode(inherited Add);
end;

function TsmxVTNodes.FindNodeByIndex(ColIndex: Integer; Node: PVirtualNode): TsmxVTNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i].ColIndex = ColIndex) and (Items[i].Node = Node) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxVTNodes.GetItem(Index: Integer): TsmxVTNode;
begin
  Result := TsmxVTNode(inherited Items[Index]);
end;

procedure TsmxVTNodes.SetItem(Index: Integer; Value: TsmxVTNode);
begin
  inherited Items[Index] := Value;
end;

function TsmxVTNodes.GetNodeCaption(ColIndex: Integer; Node: PVirtualNode): String;
var
  VTNode: TsmxVTNode;
begin
  VTNode := FindNodeByIndex(ColIndex, Node);
  if not Assigned(VTNode) then
  begin
    VTNode := Add;
    VTNode.ColIndex := ColIndex;
    VTNode.Node := Node;
  end;
  Result := VTNode.ParamName;
end;

procedure TsmxVTNodes.SetNodeCaption(ColIndex: Integer; Node: PVirtualNode; const Value: String);
var
  VTNode: TsmxVTNode;
begin
  VTNode := FindNodeByIndex(ColIndex, Node);
  if not Assigned(VTNode) then
  begin
    VTNode := Add;
    VTNode.ColIndex := ColIndex;
    VTNode.Node := Node;
  end;
  VTNode.ParamName := Value;
end;

function TsmxVTNodes.GetNodeValue(ColIndex: Integer; Node: PVirtualNode): Variant;
var
  VTNode: TsmxVTNode;
begin
  VTNode := FindNodeByIndex(ColIndex, Node);
  if not Assigned(VTNode) then
  begin
    VTNode := Add;
    VTNode.ColIndex := ColIndex;
    VTNode.Node := Node;
  end;
  Result := VTNode.ParamValue;
end;

procedure TsmxVTNodes.SetNodeValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
var
  VTNode: TsmxVTNode;
begin
  VTNode := FindNodeByIndex(ColIndex, Node);
  if not Assigned(VTNode) then
  begin
    VTNode := Add;
    VTNode.ColIndex := ColIndex;
    VTNode.Node := Node;
  end;
  VTNode.ParamValue := Value;
end;

{ TsmxVTGrid }

destructor TsmxVTGrid.Destroy;
begin
  inherited Destroy;
  if Assigned(FVTNodes) then
    FVTNodes.Free;
  if Assigned(FVTGrid) then
    FVTGrid.Free;
end;

{procedure TsmxVTGrid.DoApply;
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

{procedure TsmxVTGrid.DoChangeRow;
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
end;}

{procedure TsmxVTGrid.DoPrepare;
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

procedure TsmxVTGrid.InternalPrepare;
var
  OldIsManualRefreshParams: Boolean;
begin
  inherited InternalPrepare;
  VTGrid.Clear;
  if Assigned(Request) then
  begin
    if Assigned(Request.DataSet) then
    begin
      Request.DataSet.Close;
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := False;
        Request.Prepare;
        if Request.DataSet.Active then
          VTGrid.RootNodeCount := Request.DataSet.RecordCount;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end else
  begin
    VTNodes.Clear;
  end;
end;

{procedure TsmxVTGrid.DoRefresh;
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

procedure TsmxVTGrid.InternalRefresh;
var
  OldIsManualRefreshParams: Boolean;
begin
  inherited InternalRefresh;
  VTGrid.Clear;
  if Assigned(Request) then
  begin
    if Assigned(Request.DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := False;
        Request.Execute;
        VTGrid.RootNodeCount := Request.DataSet.RecordCount;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end else
  begin
    VTNodes.Clear;
  end;
end;

function TsmxVTGrid.GetCellHint: String;
begin
  Result := VTGrid.Hint;
end;

procedure TsmxVTGrid.SetCellHint(const Value: String);
begin
  VTGrid.Hint := Value;
end;

{function TsmxVTGrid.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxGridCfg;
end;}

function TsmxVTGrid.GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  Result := '';
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
              Result := Variants.VarToStr(Field.Value);
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

procedure TsmxVTGrid.SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := smxFuncs.StrToVar(Value);
              Request.DataSet.Post;
            end;
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

function TsmxVTGrid.GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  Result := Variants.Null;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
              Result := Field.Value;
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

procedure TsmxVTGrid.SetDBValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := Value;
              Request.DataSet.Post;
            end;
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

function TsmxVTGrid.GetFocusedColIndex: Integer;
begin
  Result := VTGrid.FocusedColumn;
end;

procedure TsmxVTGrid.SetFocusedColIndex(Value: Integer);
begin
  VTGrid.FocusedColumn := Value;
end;

function TsmxVTGrid.GetFocusedRowIndex: Integer;
begin
  if Assigned(VTGrid.FocusedNode) then
    Result := VTGrid.FocusedNode.Index else
    Result := -1;
end;

procedure TsmxVTGrid.SetFocusedRowIndex(Value: Integer);
var
  Node: PVirtualNode;
begin
  Node := RowIndexToNode(Value);
  if Assigned(Node) then
    VTGrid.FocusedNode := Node;
end;

function TsmxVTGrid.GetGridCaption(ColIndex, RowIndex: Integer): String;
var
  Node: PVirtualNode;
begin
  Result := '';
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
  begin
    //FNodeProp := npCaption;
    //Result := VTGrid.Text[Node, ColIndex];
    if Assigned(Request) then
      Result := DBText[ColIndex, Node] else
      Result := VTNodes.NodeCaptions[ColIndex, Node];
  end;
end;

procedure TsmxVTGrid.SetGridCaption(ColIndex, RowIndex: Integer; const Value: String);
var
  Node: PVirtualNode;
begin
  if coEditing in Slaves[ColIndex].ColumnOptions then
  begin
    Node := RowIndexToNode(RowIndex);
    if Assigned(Node) then
    begin
      //FNodeProp := npCaption;
      //VTGrid.Text[Node, ColIndex] := Value;
      if Assigned(Request) then
        DBText[ColIndex, Node] := Value else
        VTNodes.NodeCaptions[ColIndex, Node] := Value;
    end;
  end;
end;

function TsmxVTGrid.GetGridValue(ColIndex, RowIndex: Integer): Variant;
var
  Node: PVirtualNode;
begin
  Result := Variants.Null;
  if coHasValue in Slaves[ColIndex].ColumnOptions then
  begin
    Node := RowIndexToNode(RowIndex);
    if Assigned(Node) then
    begin
      //FNodeProp := npValue;
      //Result := smxFuncs.StrToVar(VTGrid.Text[Node, ColIndex])
      if Assigned(Request) then
        Result := DBValue[ColIndex, Node] else
        Result := VTNodes.NodeValues[ColIndex, Node];
    end;
  end;
end;

procedure TsmxVTGrid.SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant);
var
  Node: PVirtualNode;
begin
  if [coEditing, coHasValue] * Slaves[ColIndex].ColumnOptions = [coEditing, coHasValue] then
  begin
    Node := RowIndexToNode(RowIndex);
    if Assigned(Node) then
    begin
      //FNodeProp := npValue;
      //VTGrid.Text[Node, ColIndex] := Variants.VarToStr(Value);
      if Assigned(Request) then
        DBValue[ColIndex, Node] := Value else
        VTNodes.NodeValues[ColIndex, Node] := Value;
    end;
  end;
end;

function TsmxVTGrid.GetInternalRef: Pointer;
begin
  Result := Pointer(VTGrid);
end;

function TsmxVTGrid.GetRowCount: Integer;
begin
  Result := VTGrid.RootNodeCount;
end;

procedure TsmxVTGrid.SetRowCount(Value: Integer);
begin
  if not Assigned(Request) then
    VTGrid.RootNodeCount := Value;
end;

function TsmxVTGrid.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxVTColumn;
end;

function TsmxVTGrid.GetVTGrid: TVirtualStringTree;
begin
  if not Assigned(FVTGrid) then
  begin
    FVTGrid := TVirtualStringTree.Create(nil);
    FVTGrid.TreeOptions.PaintOptions := FVTGrid.TreeOptions.PaintOptions -
      [toShowTreeLines, toShowRoot, toThemeAware];
    FVTGrid.TreeOptions.SelectionOptions := FVTGrid.TreeOptions.SelectionOptions +
      [toExtendedFocus];
    FVTGrid.TreeOptions.MiscOptions := FVTGrid.TreeOptions.MiscOptions +
      [toGridExtensions];
    FVTGrid.Header.Options := FVTGrid.Header.Options + [hoOwnerDraw];
    FVTGrid.LineStyle := lsSolid;
    FVTGrid.Colors.GridLineColor := clSilver;
    FVTGrid.EditDelay := 50;
    FVTGrid.OnAdvancedHeaderDraw := VTGridAdvancedHeaderDraw;
    FVTGrid.OnChange := VTGridChange;
    FVTGrid.OnColumnClick := VTGridColumnClick;
    FVTGrid.OnDblClick := ControlDblClick;
    FVTGrid.OnEditing := VTGridEditing;
    FVTGrid.OnFreeNode := VTGridFreeNode;
    FVTGrid.OnGetText := VTGridGetText;
    FVTGrid.OnHeaderClick := VTGridHeaderClick;
    FVTGrid.OnHeaderDrawQueryElements := VTGridHeaderDrawQueryElements;
    FVTGrid.OnNewText := VTGridNewText;
    FVTGrid.OnPaintText := VTGridPaintText;
  end;
  Result := FVTGrid;
end;

function TsmxVTGrid.GetVTNodes: TsmxVTNodes;
begin
  if not Assigned(FVTNodes) then
    FVTNodes := TsmxVTNodes.Create(TsmxVTNode);
  Result := FVTNodes;
end;

{procedure TsmxVTGrid.ResetCellProps;
begin
  inherited ResetCellProps;
  GridOptions := [];
  //OnApply := nil;
  OnChangeRow := nil;
  //OnPrepare := nil;
  //OnRefresh := nil;
  Request := nil;
end;}

function TsmxVTGrid.RowIndexToNode(RowIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  if RowIndex <> -1 then
  begin
    Node := VTGrid.RootNode.FirstChild;
    while Assigned(Node) and not Assigned(Result) do
    begin
      if Integer(Node.Index) = RowIndex then
        Result := Node;
      Node := Node.NextSibling;
    end;
  end;
end;

procedure TsmxVTGrid.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    VTGrid.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      VTGrid.Parent := TWinControl(Obj);  
  end;
end;

{procedure TsmxVTGrid.SetCellProps;
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
      //OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PrepareAlgCfgID);
      //OnRefresh := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).RefreshAlgCfgID);
    end;
  end;
end;}

procedure TsmxVTGrid.SetGridOptions(Value: TsmxGridOptions);
begin
  inherited SetGridOptions(Value);
  if goColLines in Value then
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions + [toShowVertGridLines] else
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions - [toShowVertGridLines];
  if goRowLines in Value then
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions + [toShowHorzGridLines] else
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions - [toShowHorzGridLines];
  if goRowSelect in Value then
    VTGrid.TreeOptions.SelectionOptions := VTGrid.TreeOptions.SelectionOptions + [toFullRowSelect] else
    VTGrid.TreeOptions.SelectionOptions := VTGrid.TreeOptions.SelectionOptions - [toFullRowSelect];
  if goShowHeader in Value then
    VTGrid.Header.Options := VTGrid.Header.Options + [hoVisible] else
    VTGrid.Header.Options := VTGrid.Header.Options - [hoVisible];
  {if goOwnerDrawHeader in Value then
    VTGrid.Header.Options := VTGrid.Header.Options + [hoOwnerDraw] else
    VTGrid.Header.Options := VTGrid.Header.Options - [hoOwnerDraw];}
  if goEditing in Value then
    VTGrid.TreeOptions.MiscOptions := VTGrid.TreeOptions.MiscOptions + [toEditable] else
    VTGrid.TreeOptions.MiscOptions := VTGrid.TreeOptions.MiscOptions - [toEditable];
end;

function TsmxVTGrid.SetRecordNoByNode(Node: PVirtualNode): Boolean;
begin
  Result := False;
  if not Assigned(Node) then
    Exit;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
        Result := smxDBFuncs.SetNumberOfRecord(Request.DataSet, Integer(Node.Index))
end;

procedure TsmxVTGrid.VTGridAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  Wrapper: TObject;
begin
  with PaintInfo do
  begin
    if Column <> nil then
    begin
      Wrapper := TObject(Column.Tag);
      if Wrapper is TsmxCustomColumn then
      begin
        if hpeBackground in Elements then
        begin
          TargetCanvas.Brush.Color := TsmxCustomColumn(Wrapper).HeaderColor;
          Windows.DrawFrameControl(TargetCanvas.Handle, PaintRectangle, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
        end;
        if hpeText in Elements then
        begin
          TargetCanvas.Font := TsmxCustomColumn(Wrapper).HeaderFont;
          TargetCanvas.TextOut(TextRectangle.Left, TextRectangle.Top, String(TsmxCustomColumn(Wrapper).HeaderCaption));
        end;
      end;
    end;
  end;
end;

procedure TsmxVTGrid.VTGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  ChangeRow;
end;

procedure TsmxVTGrid.VTGridColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TsmxCustomColumn(Wrapper).Snap;
end;

procedure TsmxVTGrid.VTGridEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    Allowed := coEditing in TsmxCustomColumn(Wrapper).ColumnOptions;
end;

procedure TsmxVTGrid.VTGridFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  i: Integer;
  VTNode: TsmxVTNode;
begin
  if not Assigned(Request) then
    for i := 0 to SlaveCount - 1 do
    begin
      VTNode := VTNodes.FindNodeByIndex(i, Node);
      if Assigned(VTNode) then
        VTNodes.Delete(VTNode.ItemIndex);
    end;
end;

procedure TsmxVTGrid.VTGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
begin
  if Assigned(Request) then
  begin
    //if FNodeProp = npCaption then
      Text := WideString(DBText[Column, Node]);// else
      //Text := WideString(Variants.VarToStr(DBValue[Column, Node]));
  end else
  begin
    //if FNodeProp = npCaption then
      Text := WideString(VTNodes.NodeCaptions[Column, Node]);// else
      //Text := WideString(Variants.VarToStr(VTNodes.NodeValues[Column, Node]));
  end;
end;

procedure TsmxVTGrid.VTGridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(Sender.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TsmxCustomColumn(Wrapper).SnapHeader;
end;

procedure TsmxVTGrid.VTGridHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if PaintInfo.Column <> nil then
    Elements := [hpeBackground, hpeText];
end;

procedure TsmxVTGrid.VTGridNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: UnicodeString);
begin
  if Assigned(Request) then
  begin
    //if FNodeProp = npCaption then
      DBText[Column, Node] := String(NewText);// else
      //DBValue[Column, Node] := smxFuncs.StrToVar(String(NewText));
  end else
  begin
    //if FNodeProp = npCaption then
      VTNodes.NodeCaptions[Column, Node] := String(NewText);// else
      //VTNodes.NodeValues[Column, Node] := smxFuncs.StrToVar(String(NewText));
  end;
end;

procedure TsmxVTGrid.VTGridPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TargetCanvas.Font := TsmxCustomColumn(Wrapper).ColumnFont;
end;

{ TsmxVTTree }

destructor TsmxVTTree.Destroy;
begin
  inherited Destroy;
  if Assigned(FVTNodes) then
    FVTNodes.Free;
  if Assigned(FVTTree) then
    FVTTree.Free;
  if Assigned(FEditor) then
    //FEditor.Free;
    FEditor._Release;
end;

function TsmxVTTree.AddRow(RowIndex: Pointer): Pointer;
begin
  if Assigned(RowIndex) then
    Result := VTTree.AddChild(RowIndex) else
    Result := nil;
end;

procedure TsmxVTTree.DelRow(RowIndex: Pointer);
begin
  if Assigned(RowIndex) then 
    VTTree.DeleteNode(RowIndex);
end;

{procedure TsmxVTTree.DoApply;
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

{procedure TsmxVTTree.DoChangeRow;
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
end;}

{procedure TsmxVTTree.DoCollapse;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnCollapse) then
  begin
    if Cfg is TsmxTreeCfg then
      AlgCfgID := TsmxTreeCfg(Cfg).CollapseAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnCollapse, AlgCfgID);
  end;
end;}

{procedure TsmxVTTree.InternalPrepare;
var
  OldIsManualRefreshParams: Boolean;
begin
  inherited InternalPrepare;
  VTTree.Clear;
  if Assigned(Request) then
  begin
    if Assigned(Request.DataSet) then
    begin
      Request.DataSet.Close;
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := False;
        Request.Prepare;
        if Request.DataSet.Active then
          VTTree.RootNodeCount := Request.DataSet.RecordCount;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end else
  begin
    VTNodes.Clear;
  end;
end;}

{procedure TsmxVTTree.DoExpand;
var
  AlgCfgID: Integer;
begin
  if Assigned(OnExpand) then
  begin
    if Cfg is TsmxTreeCfg then
      AlgCfgID := TsmxTreeCfg(Cfg).ExpandAlgCfgID else
      AlgCfgID := 0;
    DoEvent(OnExpand, AlgCfgID);
  end;
end;}

{procedure TsmxVTTree.InternalRefresh;
var
  OldIsManualRefreshParams: Boolean;
begin
  inherited InternalRefresh;
  VTTree.Clear;
  if Assigned(Request) then
  begin
    if Assigned(Request.DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := False;
        Request.Execute;
        VTTree.RootNodeCount := Request.DataSet.RecordCount;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end else
  begin
    VTNodes.Clear;
  end;
end;}

{procedure TsmxVTTree.DoEdited;
begin
  if Assigned(FOnEdited) then
    FOnEdited(Self);
end;

procedure TsmxVTTree.InternalEdited;
begin
end;

procedure TsmxVTTree.Edited;
begin
  InternalEdited;
  DoEdited;
end;}

{procedure TsmxVTTree.DoEditing;
begin
  if Assigned(FOnEditing) then
    FOnEditing(Self);
end;

procedure TsmxVTTree.InternalEditing;
begin
end;

procedure TsmxVTTree.Editing;
begin
  InternalEditing;
  DoEditing;
end;}

function TsmxVTTree.GetCellHint: String;
begin
  Result := VTTree.Hint;
end;

procedure TsmxVTTree.SetCellHint(const Value: String);
begin
  VTTree.Hint := Value;
end;

{function TsmxVTTree.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxTreeCfg;
end;}

function TsmxVTTree.GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  Result := '';
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
              Result := Variants.VarToStr(Field.Value);
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

procedure TsmxVTTree.SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := smxFuncs.StrToVar(Value);
              Request.DataSet.Post;
            end;
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

function TsmxVTTree.GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  Result := Variants.Null;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
              Result := Field.Value;
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

procedure TsmxVTTree.SetDBValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
var
  CurRecordNo: Integer;
  Field: IsmxField;
  //Bookmark: Pointer;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        //Request.DataSet.DisableControls;
        //Bookmark := Request.DataSet.GetBookmark;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetValueFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := Value;
              Request.DataSet.Post;
            end;
          end;
          //Request.DataSet.GotoBookmark(Bookmark);
        finally
          Request.DataSet.RecordNo := CurRecordNo;
          //Request.DataSet.FreeBookmark(Bookmark);
          //Request.DataSet.EnalbleControls;
        end;
      end;
end;

function TsmxVTTree.GetExpanded(RowIndex: Pointer): Boolean;
begin
  if Assigned(RowIndex) then
    Result := VTTree.Expanded[RowIndex] else
    Result := False;
end;

procedure TsmxVTTree.SetExpanded(RowIndex: Pointer; Value: Boolean);
begin
  if Assigned(RowIndex) then
    VTTree.Expanded[RowIndex] := Value;
end;

function TsmxVTTree.GetFocusedColIndex: Integer;
begin
  Result := VTTree.FocusedColumn;
end;

procedure TsmxVTTree.SetFocusedColIndex(Value: Integer);
begin
  VTTree.FocusedColumn := Value;
end;

function TsmxVTTree.GetFocusedRowIndex: Pointer;
begin
  Result := VTTree.FocusedNode;
end;

procedure TsmxVTTree.SetFocusedRowIndex(Value: Pointer);
begin
  VTTree.FocusedNode := PVirtualNode(Value);
end;

function TsmxVTTree.GetTreeCaption(ColIndex: Integer; RowIndex: Pointer): String;
begin
  Result := '';
  if Assigned(RowIndex) then
  begin
    //FNodeProp := npCaption;
    //Result := VTTree.Text[RowIndex, ColIndex]
    if Assigned(Request) then
      Result := DBText[ColIndex, RowIndex] else
      Result := VTNodes.NodeCaptions[ColIndex, RowIndex];
  end;
end;

procedure TsmxVTTree.SetTreeCaption(ColIndex: Integer; RowIndex: Pointer; const Value: String);
begin
  if coEditing in Slaves[ColIndex].ColumnOptions then
    if Assigned(RowIndex) then
    begin
      //FNodeProp := npCaption;
      //VTTree.Text[RowIndex, ColIndex] := Value;
      if Assigned(Request) then
        DBText[ColIndex, RowIndex] := Value else
        VTNodes.NodeCaptions[ColIndex, RowIndex] := Value;
    end;
end;

function TsmxVTTree.GetTreeValue(ColIndex: Integer; RowIndex: Pointer): Variant;
begin
  Result := Variants.Null;
  if coHasValue in Slaves[ColIndex].ColumnOptions then
    if Assigned(RowIndex) then
    begin
      //FNodeProp := npValue;
      //Result := smxFuncs.StrToVar(VTTree.Text[RowIndex, ColIndex])
      if Assigned(Request) then
        Result := DBValue[ColIndex, RowIndex] else
        Result := VTNodes.NodeValues[ColIndex, RowIndex];
    end;
end;

procedure TsmxVTTree.SetTreeValue(ColIndex: Integer; RowIndex: Pointer; const Value: Variant);
begin
  if [coEditing, coHasValue] * Slaves[ColIndex].ColumnOptions = [coEditing, coHasValue] then
    if Assigned(RowIndex) then
    begin
      //FNodeProp := npValue;
      //VTTree.Text[RowIndex, ColIndex] := Variants.VarToStr(Value);
      if Assigned(Request) then
        DBValue[ColIndex, RowIndex] := Value else
        VTNodes.NodeValues[ColIndex, RowIndex] := Value;
    end;
end;

function TsmxVTTree.GetInternalRef: Pointer;
begin
  Result := Pointer(VTTree);
end;

function TsmxVTTree.GetEditor: IsmxTreeEditor;
begin
  if not Assigned(FEditor) then
  begin
    FEditor := TsmxVTEditor.Create(nil{Self as IsmxRefComponent});
    FEditor._AddRef;
  end;
  Result := FEditor as IsmxTreeEditor;
end;

{function TsmxVTTree.GetEditorClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxVTEditor;
end;}

function TsmxVTTree.GetParentRow(RowIndex: Pointer): Pointer;
begin
  Result := VTTree.NodeParent[RowIndex];
end;

procedure TsmxVTTree.SetParentRow(RowIndex, Value: Pointer);
begin
  VTTree.NodeParent[RowIndex] := Value;
end;

function TsmxVTTree.GetRootRow: Pointer;
begin
  Result := VTTree.RootNode;
end;

function TsmxVTTree.GetRow(RowIndex: Pointer; Index: Integer): Pointer;
var
  Node: PVirtualNode;
begin
  Result := nil;
  if Assigned(RowIndex) then
  begin
    Node := VTTree.GetFirstChild(RowIndex);
    while Assigned(Node) do
    begin
      if Integer(Node.Index) = Index then
      begin
        Result := Node;
        Break;
      end;
      Node := Node.NextSibling;
    end;
  end;
end;

function TsmxVTTree.GetRowCount(RowIndex: Pointer): Integer;
begin
  if Assigned(RowIndex) then
    Result := VTTree.ChildCount[RowIndex] else
    Result := -1;
end;

procedure TsmxVTTree.SetRowCount(RowIndex: Pointer; Value: Integer);
begin
  if Assigned(RowIndex) then
    VTTree.ChildCount[RowIndex] := Value;
end;

function TsmxVTTree.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxVTColumn;
end;

function TsmxVTTree.GetVTTree: TVirtualStringTree;
begin
  if not Assigned(FVTTree) then
  begin
    FVTTree := TVirtualStringTree.Create(nil);
    FVTTree.TreeOptions.PaintOptions := FVTTree.TreeOptions.PaintOptions -
      [toThemeAware, toShowTreeLines];
    FVTTree.TreeOptions.SelectionOptions := FVTTree.TreeOptions.SelectionOptions +
      [toExtendedFocus];
    FVTTree.Header.Options := VTTree.Header.Options + [hoOwnerDraw];
    FVTTree.LineStyle := lsSolid;
    FVTTree.Colors.GridLineColor := clSilver;
    FVTTree.EditDelay := 50;
    FVTTree.Margin := 2;
    FVTTree.TextMargin := 0;
    FVTTree.OnAdvancedHeaderDraw := VTTreeAdvancedHeaderDraw;
    FVTTree.OnChange := VTTreeChange;
    FVTTree.OnCollapsed := VTTreeCollapsed;
    FVTTree.OnColumnClick := VTTreeColumnClick;
    FVTTree.OnCreateEditor := VTTreeCreateEditor;
    FVTTree.OnDblClick := ControlDblClick;
    FVTTree.OnEdited := VTTreeEdited;
    FVTTree.OnEditing := VTTreeEditing;
    FVTTree.OnExpanded := VTTreeExpanded;
    FVTTree.OnFreeNode := VTTreeFreeNode;
    FVTTree.OnGetText := VTTreeGetText;
    FVTTree.OnHeaderClick := VTTreeHeaderClick;
    FVTTree.OnHeaderDrawQueryElements := VTTreeHeaderDrawQueryElements;
    FVTTree.OnNewText := VTTreeNewText;
    FVTTree.OnPaintText := VTTreePaintText;
  end;
  Result := FVTTree;
end;

function TsmxVTTree.GetVTNodes: TsmxVTNodes;
begin
  if not Assigned(FVTNodes) then
    FVTNodes := TsmxVTNodes.Create(TsmxVTNode);
  Result := FVTNodes;
end;

{procedure TsmxVTTree.ResetCellProps;
begin
  inherited ResetCellProps;
  OnChangeRow := nil;
  OnCollapse := nil;
  OnExpand := nil;
  Request := nil;
  TreeOptions := [];
end;}

{function TsmxVTTree.RowIndexToNode(RowIndex: Integer): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  if RowIndex <> -1 then
  begin
    Node := TsmxVTTree.RootNode.FirstChild;
    while Assigned(Node) and not Assigned(Result) do
    begin
      if Integer(Node.Index) = RowIndex then
        Result := Node;
      Node := Node.NextSibling;
    end;
  end;
end;}

function TsmxVTTree.RowLevel(RowIndex: Pointer): Integer;
begin
  if Assigned(RowIndex) then
    Result := VTTree.GetNodeLevel(RowIndex) else
    Result := -1;
end;

procedure TsmxVTTree.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    VTTree.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      VTTree.Parent := TWinControl(Obj);
  end;
end;

{procedure TsmxVTTree.SetCellProps;
var
  Form: TsmxCustomForm;
begin
  inherited SetCellProps;
  if Cfg is TsmxTreeCfg then
  begin
    TreeOptions := TsmxTreeCfg(Cfg).TreeOptions;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
    begin
      Request := smxClassFuncs.GetRequestForm(Form, TsmxTreeCfg(Cfg).RequestCfgID);
      OnChangeRow := smxClassFuncs.GetEventForm(Form, TsmxTreeCfg(Cfg).ChangeRowAlgCfgID);
      OnCollapse := smxClassFuncs.GetEventForm(Form, TsmxTreeCfg(Cfg).CollapseAlgCfgID);
      OnExpand := smxClassFuncs.GetEventForm(Form, TsmxTreeCfg(Cfg).ExpandAlgCfgID);
    end;
  end;
end;}

procedure TsmxVTTree.SetTreeOptions(Value: TsmxTreeOptions);
begin
  inherited SetTreeOptions(Value);
  if toColLines in Value then
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions + [toShowVertGridLines] else
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions - [toShowVertGridLines];
  if toRowLines in Value then
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions + [toShowHorzGridLines] else
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions - [toShowHorzGridLines];
  if toRowSelect in Value then
    VTTree.TreeOptions.SelectionOptions := VTTree.TreeOptions.SelectionOptions + [toFullRowSelect] else
    VTTree.TreeOptions.SelectionOptions := VTTree.TreeOptions.SelectionOptions - [toFullRowSelect];
  if toShowHeader in Value then
    VTTree.Header.Options := VTTree.Header.Options + [hoVisible] else
    VTTree.Header.Options := VTTree.Header.Options - [hoVisible];
  {if toOwnerDrawHeader in Value then
    VTTree.Header.Options := VTTree.Header.Options + [hoOwnerDraw] else
    VTTree.Header.Options := VTTree.Header.Options - [hoOwnerDraw];}
  if toEditing in Value then
    VTTree.TreeOptions.MiscOptions := VTTree.TreeOptions.MiscOptions + [toEditable] else
    VTTree.TreeOptions.MiscOptions := VTTree.TreeOptions.MiscOptions - [toEditable];
  if toTreeLines in Value then
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions + [toShowTreeLines] else
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions - [toShowTreeLines];
end;

function TsmxVTTree.SetRecordNoByNode(Node: PVirtualNode): Boolean;
begin
  Result := False;
  if not Assigned(Node) then
    Exit;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
        Result := smxDBFuncs.SetNumberOfRecord(Request.DataSet, Integer(Node.Index))
end;

procedure TsmxVTTree.VTTreeAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  Wrapper: TObject;
begin
  with PaintInfo do
  begin
    if Column <> nil then
    begin
      Wrapper := TObject(Column.Tag);
      if Wrapper is TsmxCustomColumn then
      begin
        if hpeBackground in Elements then
        begin
          TargetCanvas.Brush.Color := TsmxCustomColumn(Wrapper).HeaderColor;
          Windows.DrawFrameControl(TargetCanvas.Handle, PaintRectangle, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
        end;
        if hpeText in Elements then
        begin
          TargetCanvas.Font := TsmxCustomColumn(Wrapper).HeaderFont;
          TargetCanvas.TextOut(TextRectangle.Left, TextRectangle.Top, String(TsmxCustomColumn(Wrapper).HeaderCaption));
        end;
      end;
    end;
  end;
end;

procedure TsmxVTTree.VTTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Wrapper: TObject;
begin
  ChangeRow;
  if toEditable in VTTree.TreeOptions.MiscOptions then
  begin
    Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[TVirtualStringTree(Sender).FocusedColumn].Tag);
    if Wrapper is TsmxCustomColumn then
      if coEditing in TsmxCustomColumn(Wrapper).ColumnOptions then
        if Assigned(Node) then
          VTTree.EditNode(Node, VTTree.FocusedColumn);
  end;      
end;

procedure TsmxVTTree.VTTreeCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Collapse;
end;

procedure TsmxVTTree.VTTreeColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TsmxCustomColumn(Wrapper).Snap;
end;

procedure TsmxVTTree.VTTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := Editor as IVTEditLink; //TsmxVTEditor.Create as IVTEditLink;
end;

procedure TsmxVTTree.VTTreeEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  //Edited;
end;

procedure TsmxVTTree.VTTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
  begin
    Allowed := coEditing in TsmxCustomColumn(Wrapper).ColumnOptions;
    //if Allowed then
      //Editing;
  end;
end;

procedure TsmxVTTree.VTTreeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Expand;
end;

procedure TsmxVTTree.VTTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  i: Integer;
  VTNode: TsmxVTNode;
begin
  if not Assigned(Request) then
    for i := 0 to SlaveCount - 1 do
    begin
      VTNode := VTNodes.FindNodeByIndex(i, Node);
      if Assigned(VTNode) then
        VTNodes.Delete(VTNode.ItemIndex);
    end;
end;

procedure TsmxVTTree.VTTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
begin
  if Assigned(Request) then
  begin
    //if FNodeProp = npCaption then
      Text := WideString(DBText[Column, Node]);// else
      //Text := WideString(Variants.VarToStr(DBValue[Column, Node]));
  end else
  begin
    //if FNodeProp = npCaption then
      Text := WideString(VTNodes.NodeCaptions[Column, Node]);// else
      //Text := WideString(Variants.VarToStr(VTNodes.NodeValues[Column, Node]));
  end;
end;

procedure TsmxVTTree.VTTreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(Sender.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TsmxCustomColumn(Wrapper).SnapHeader;
end;

procedure TsmxVTTree.VTTreeHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if PaintInfo.Column <> nil then
    Elements := [hpeBackground, hpeText];
end;

procedure TsmxVTTree.VTTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: UnicodeString);
begin
  if Assigned(Request) then
  begin
    //if FNodeProp = npCaption then
      DBText[Column, Node] := String(NewText);// else
      //DBValue[Column, Node] := smxFuncs.StrToVar(String(NewText));
  end else
  begin
    //if FNodeProp = npCaption then
      VTNodes.NodeCaptions[Column, Node] := String(NewText);// else
      //VTNodes.NodeValues[Column, Node] := smxFuncs.StrToVar(String(NewText));
  end;
end;

procedure TsmxVTTree.VTTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TargetCanvas.Font := TsmxCustomColumn(Wrapper).ColumnFont;
end;

{ TsmxVTEditor }

{destructor TsmxVTEditor.Destroy;
begin
  if Assigned(FControl) then
    FControl.Free;
  inherited Destroy;
end;}

function TsmxVTEditor.BeginEdit: Boolean;
begin      //inf('BeginEdit');
  //Result := True;
  //TsmxCustomTree(FTree.Tag).Editing;
  //FTree.Editing;
  if Assigned(Control) then
  begin
    Control.Show;
    Control.SetFocus;
    Result := True;
  end else
    Result := False;
end;

function TsmxVTEditor.CancelEdit: Boolean;
begin
  //Result := True;
  if Assigned(Control) then
  begin
    Control.Hide;
    Result := True;
  end else
    Result := False;
end;

procedure TsmxVTEditor.ControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := True;

  case Key of
    VK_ESCAPE:
      if CanAdvance then
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;
    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if FControl is TComboBox then
          CanAdvance := CanAdvance and not TComboBox(FControl).DroppedDown;
        {if FEdit is TDateTimePicker then
          CanAdvance :=  CanAdvance and not TDateTimePicker(FEdit).DroppedDown;}

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

function TsmxVTEditor.EndEdit: Boolean;
//var
  //Buffer: array[0 .. 1024] of Char;
  //S: String;
begin
  (*Result := True;

  //Data := FTree.GetNodeData(FNode);
  if FEdit is TComboBox then
    S := TComboBox(FEdit).Text
  else
  begin
    Windows.GetWindowText(FEdit.Handle, Buffer, 1024);
    S := Buffer;
  end;

  TsmxVTTree(FTree.Tag).TreeCaptions[FColumn, FNode] := S;



  {if S <> Data.Value then
  begin
    Data.Value := S;
    Data.Changed := True;
    FTree.InvalidateNode(FNode);
  end;}
  FEdit.Hide;
  FTree.SetFocus;*)
  //if Assigned(FOnEdited) then
    //FOnEdited(Self);
  //Result := True;
  //inf('EndEdit');
  TsmxCustomTree(FTree.Tag).Edited;
  if Assigned(Control) then
  begin
    Control.Hide;
    //TVirtualStringTree((Tree as IsmxRefComponent).GetInternalRef).SetFocus;
    FTree.SetFocus;
    Result := True;
  end else
    Result := False;
  //TsmxCustomTree(FTree.Tag).Edited;
  //Tree.Edited;
end;

function TsmxVTEditor.GetBounds: TRect;
begin
  if Assigned(Control) then
    Result := Control.BoundsRect; //else
    //Result := Types.Rect(0, 0, 0, 0);
end;

function TsmxVTEditor.GetColIndex: Integer;
begin
  Result := FColumn;
end;

function TsmxVTEditor.GetControl: TWinControl;
begin
  if not Assigned(FControl) then
    case FEditorType of
      etString:
      begin
        FControl := TEdit.Create(nil);
        with FControl as TEdit do
        begin
          Visible := False;
          Parent := FTree;
          AutoSize := False;
          BorderStyle := bsNone;
          BevelKind := bkFlat;
          OnKeyDown := ControlKeyDown;
        end;
      end;
      etPickString:
      begin
        FControl := TComboBox.Create(nil);
        with FControl as TComboBox do
        begin
          Visible := False;
          Parent := FTree;
          Style := csOwnerDrawFixed;
          ItemHeight := FNode.NodeHeight - 6;
          BevelKind := bkFlat;
          BevelInner := bvNone;
          OnKeyDown := ControlKeyDown;
        end;
      end;
      etButtonString:
      begin
        FControl := TsmxButtonMaskEdit.Create(nil);
        with FControl as TsmxButtonMaskEdit do
        begin
          Visible := False;
          Parent := FTree;
          AutoSize := False;
          BorderStyle := bsNone;
          BevelKind := bkFlat;
          ButtonGlyphKind := gkEllipsis;
          OnKeyDown := ControlKeyDown;
        end;
      end;
      else
        FControl := nil;
    end;
  Result := FControl;
end;

function TsmxVTEditor.GetHolder: TWinControl;
begin
  Result := FTree;
end;

function TsmxVTEditor.GetRowIndex: Pointer;
begin
  Result := Pointer(FNode);
end;

function TsmxVTEditor.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): Boolean;
{var
  V: Variant;
  Obj: TObject;
  PropInfo: PPropInfo;
  EditorType: TsmxEditorType;
  TypeData: PTypeData;
  i: Integer;}
begin      //inf('PrepareEdit');
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  //FTree := TsmxCustomTree(Tree.Tag);
  //FRowIndex := Pointer(Node);
  //FColIndex := Column;
  FEditorType := etNone;
  if Assigned(FControl) then
  begin
    FControl.Free;
    FControl := nil;
  end;
  TsmxCustomTree(FTree.Tag).Editing;
  
  // determine what edit type actually is needed
  (*if Assigned(FEdit) then
  begin
    FEdit.Free;
    FEdit := nil;
  end;
  //Data := FTree.GetNodeData(Node);
  V := TsmxVTTree(FTree.Tag).TreeValues[FColumn, FNode];
  if not Variants.VarIsArray(V) then
  begin
    Result := False;
    Exit;
  end else
  begin
    Obj := TObject(Integer(V[0]));
    PropInfo := PPropInfo(Integer(V[1]));
    EditorType := TsmxEditorType(Integer(V[2]));
  end;
  case EditorType of
  {case Data.ValueType of
    vtString:
      begin}
    etString:
    begin
      FEdit := TEdit.Create(nil);
      with FEdit as TEdit do
      begin
        Visible := False;
        Parent := Tree;
        //Text := Data.Value;
        Text := TsmxVTTree(FTree.Tag).TreeCaptions[FColumn, FNode];
        AutoSize := False;
        OnKeyDown := EditKeyDown;
      end;
    end;
    etPickString:
    begin
      FEdit := TComboBox.Create(nil);
      with FEdit as TComboBox do
      begin
        Visible := False;
        Parent := Tree;
        //Text := TsmxVTTree(FTree.Tag).TreeCaptions[FColumn, FNode];
        Style := csOwnerDrawFixed;
        //Height := FNode.NodeHeight - 8;
        ItemHeight := FNode.NodeHeight - 6;
        case PropInfo^.PropType^^.Kind of
          tkEnumeration:
          begin
            TypeData := TypInfo.GetTypeData(PropInfo^.PropType^);
            for i := TypeData^.MinValue to TypeData^.MaxValue do
              Items.Add(TypInfo.GetEnumName(PropInfo^.PropType^, i));
            ItemIndex := Items.IndexOf(TsmxVTTree(FTree.Tag).TreeCaptions[FColumn, FNode]);
          end;
        end;

        {Items.Add(Text);
        Items.Add('Standard');
        Items.Add('Additional');
        Items.Add('Win32');}
        OnKeyDown := EditKeyDown;
      end;
    end;
      {end;
    vtPickString:
      begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          Items.Add(Text);
          Items.Add('Standard');
          Items.Add('Additional');
          Items.Add('Win32');
          OnKeyDown := EditKeyDown;
        end;
      end;
    vtNumber:
      begin
        FEdit := TMaskEdit.Create(nil);
        with FEdit as TMaskEdit do
        begin
          Visible := False;
          Parent := Tree;
          EditMask := '9999';
          Text := Data.Value;
          OnKeyDown := EditKeyDown;
        end;
      end;
    vtPickNumber:
      begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          OnKeyDown := EditKeyDown;
        end;
      end;
    vtMemo:
      begin
        FEdit := TComboBox.Create(nil);
        // In reality this should be a drop down memo but this requires
        // a special control.
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          Items.Add(Data.Value);
          OnKeyDown := EditKeyDown;
        end;
      end;
    vtDate:
      begin
        FEdit := TDateTimePicker.Create(nil);
        with FEdit as TDateTimePicker do
        begin
          Visible := False;
          Parent := Tree;
          CalColors.MonthBackColor := clWindow;
          CalColors.TextColor := clBlack;
          CalColors.TitleBackColor := clBtnShadow;
          CalColors.TitleTextColor := clBlack;
          CalColors.TrailingTextColor := clBtnFace;
          Date := StrToDate(Data.Value);
          OnKeyDown := EditKeyDown;
        end;
      end;}
  else
    Result := False;
  end;*)
end;

procedure TsmxVTEditor.ProcessMessage(var Message: TMessage);
begin
  if Assigned(Control) then
    Control.WindowProc(Message);
end;

procedure TsmxVTEditor.SetBounds(R: TRect);
var
  Dummy: Integer;
begin    //inf('setbounds1');
  if Assigned(Control) then
  begin  //inf('setbounds2');
    //TVirtualStringTree((Tree as IsmxRefComponent).GetInternalRef).Header.Columns.GetColumnBounds(ColIndex, Dummy, R.Right);
    FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
    Dec(R.Right, 1);
    Dec(R.Left, 2);
    Control.BoundsRect := R;
  end;
end;

{procedure TsmxVTEditor.SetControl(Value: TWinControl);
begin
  if Assigned(FControl) then
  begin
    FControl.Free;
    FControl := nil;
  end;
  FControl := Value;
end;}

function TsmxVTEditor.GetEditorType: TsmxEditorType;
begin
  Result := FEditorType;
end;

procedure TsmxVTEditor.SetEditorType(Value: TsmxEditorType);
begin
  //if FEditorType <> Value then
  //begin
    FEditorType := Value;
    //if Assigned(FControl) then
    //begin
      //FControl.Free;
      //FControl := nil;
    //end;
  //end;
end;

initialization
  Classes.RegisterClasses([TsmxVTColumn, TsmxVTGrid, TsmxVTTree]);

finalization
  Classes.UnRegisterClasses([TsmxVTColumn, TsmxVTGrid, TsmxVTTree]);

end.
