{**************************************}
{                                      }
{            SalesMan v1.0             }
{       VirtualTree cell classes       }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxVTCells;

interface

uses
  Classes, Controls, Graphics, VirtualTrees, Types, Messages, StdCtrls,
  smxBaseClasses, smxClasses, smxTypes, smxBaseTypes, smxClassIntf;

type
  { TsmxVTColumn }

  TsmxVTColumn = class(TsmxCustomColumn)
  private
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
    function GetVisible: Boolean; override;
    function GetWidth: Integer; override;
    function GetAlignment: TAlignment; override;
    function GetText: String; override;
    function GetColor: TColor; override;
    function GetFont: TFont; override;
    function GetValue: Variant; override;
    function GetHeaderAlignment: TAlignment; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetHeaderText: String; override;
    function GetInternalRef: Pointer; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
    procedure SetAlignment(Value: TAlignment); override;
    procedure SetOptions(Value: TsmxColumnOptions); override;
    procedure SetText(const Value: String); override;
    procedure SetColor(Value: TColor); override;
    procedure SetFont(Value: TFont); override;
    procedure SetValue(const Value: Variant); override;
    procedure SetCellFeedBack; override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderText(const Value: String); override;
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
    property Alignment;
    property Color;
    property Font;
    property Options;
    property Visible;
    property Width;
    property HeaderAlignment;
    property HeaderText;
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
    function GetNodeText(ColIndex: Integer; Node: PVirtualNode): String;
    function GetNodeValue(ColIndex: Integer; Node: PVirtualNode): Variant;
    procedure SetNodeText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
    procedure SetNodeValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
  public
    function Add: TsmxVTNode;
    function FindNodeByIndex(ColIndex: Integer; Node: PVirtualNode): TsmxVTNode;

    property Items[Index: Integer]: TsmxVTNode read GetItem write SetItem; default;
    property NodeTexts[ColIndex: Integer; Node: PVirtualNode]: String read GetNodeText write SetNodeText;
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
  public
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;

    property ColIndex: Integer read GetColIndex;
    property Control: TWinControl read GetControl;
    property EditorType: TsmxEditorType read GetEditorType write SetEditorType;
    property Holder: TWinControl read GetHolder;
    property RowIndex: Pointer read GetRowIndex;
  end;

  { TsmxVTGrid }

  TsmxVTGrid = class(TsmxCustomGrid)
  private
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
    function GetHint: String; override;
    function GetFocusedRowIndex: Integer; override;
    function GetFocusedColIndex: Integer; override;
    function GetGridText(ColIndex, RowIndex: Integer): String; override;
    function GetGridValue(ColIndex, RowIndex: Integer): Variant; override;
    function GetInternalRef: Pointer; override;
    function GetRowCount: Integer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalApply; override;
    procedure InternalCancel; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetHint(const Value: String); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Integer); override;
    procedure SetOptions(Value: TsmxGridOptions); override;
    procedure SetGridText(ColIndex, RowIndex: Integer; const Value: String); override;
    procedure SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant); override;
    procedure SetRowCount(Value: Integer); override;

    property DBText[ColIndex: Integer; Node: PVirtualNode]: String read GetDBText write SetDBText;
    property DBValue[ColIndex: Integer; Node: PVirtualNode]: Variant read GetDBValue write SetDBValue;
    property VTGrid: TVirtualStringTree read GetVTGrid;
    property VTNodes: TsmxVTNodes read GetVTNodes;
  public
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Cursor;
    property Enabled;
    property Height;
    property Hint;
    property Left;
    property Top;
    property Visible;
    property Width;
    property Options;
    property PopupMenu;
    property Request;
    property SlaveList;

    property OnChangeRow;
    property OnDoubleClick;
  end;

  { TsmxVTTree }

  TsmxVTTree = class(TsmxCustomTree)
  private
    FVTTree: TVirtualStringTree;
    FVTNodes: TsmxVTNodes;
    FEditorIntf: IsmxTreeEditor;
    function GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
    function GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
    function GetVTTree: TVirtualStringTree;
    function GetVTNodes: TsmxVTNodes;
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
    function GetHint: String; override;
    function GetExpanded(RowIndex: Pointer): Boolean; override;
    function GetFocusedRowIndex: Pointer; override;
    function GetFocusedColIndex: Integer; override;
    function GetInternalRef: Pointer; override;
    function GetEditor: IsmxTreeEditor; override;
    function GetParentRow(RowIndex: Pointer): Pointer; override;
    function GetRootRow: Pointer; override;
    function GetRow(RowIndex: Pointer; Index: Integer): Pointer; override;
    function GetRowCount(RowIndex: Pointer): Integer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    function GetTreeText(ColIndex: Integer; RowIndex: Pointer): String; override;
    function GetTreeValue(ColIndex: Integer; RowIndex: Pointer): Variant; override;
    procedure InternalApply; override;
    procedure InternalCancel; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetHint(const Value: String); override;
    procedure SetExpanded(RowIndex: Pointer; Value: Boolean); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Pointer); override;
    procedure SetParentRow(RowIndex, Value: Pointer); override;
    procedure SetRowCount(RowIndex: Pointer; Value: Integer); override;
    procedure SetOptions(Value: TsmxTreeOptions); override;
    procedure SetTreeText(ColIndex: Integer; RowIndex: Pointer; const Value: String); override;
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
  published
    property Align;
    property Anchors;
    property Cursor;
    property Enabled;
    property Height;
    property Hint;
    property Left;
    property Top;
    property Visible;
    property Width;
    property PopupMenu;
    property Request;
    property SlaveList;
    property Options;

    property OnChangeRow;
    property OnCollapse;
    property OnDoubleClick;
    property OnEdited;
    property OnEditing;
    property OnExpand;
  end;

implementation

uses
  Windows, Variants, Forms, TypInfo, smxStdCtrls, smxFuncs, smxProcs,
  smxClassFuncs, smxDBFuncs, smxDBIntf, smxBaseIntf;

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

function TsmxVTColumn.GetVisible: Boolean;
begin
  Result := coVisible in VTColumn.Options;
end;

procedure TsmxVTColumn.SetVisible(Value: Boolean);
var
  VT: TVirtualStringTree;
begin
  if GetVisible <> Value then
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

function TsmxVTColumn.GetWidth: Integer;
begin
  Result := VTColumn.Width;
end;

procedure TsmxVTColumn.SetWidth(Value: Integer);
var
  VT: TVirtualStringTree;
begin
  if GetWidth <> Value then
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

function TsmxVTColumn.GetAlignment: TAlignment;
begin
  Result := VTColumn.Alignment;
end;

procedure TsmxVTColumn.SetAlignment(Value: TAlignment);
var
  VT: TVirtualStringTree;
begin
  if GetAlignment <> Value then
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

function TsmxVTColumn.GetText: String;
begin
  Result := '';
  if CellOwner is TsmxCustomGrid then
  begin
    if TsmxCustomGrid(CellOwner).FocusedRowIndex <> -1 then
      Result := TsmxCustomGrid(CellOwner).GridTexts[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
  end else
  if CellOwner is TsmxCustomTree then
  begin
    if Assigned(TsmxCustomTree(CellOwner).FocusedRowIndex) then
      Result := TsmxCustomTree(CellOwner).TreeTexts[SlaveIndex, TsmxCustomTree(CellOwner).FocusedRowIndex];
  end;
end;

procedure TsmxVTColumn.SetText(const Value: String);
begin
  if CellOwner is TsmxCustomGrid then
  begin
    if TsmxCustomGrid(CellOwner).FocusedRowIndex <> -1 then
      TsmxCustomGrid(CellOwner).GridTexts[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
  end else
  if CellOwner is TsmxCustomTree then
  begin
    if Assigned(TsmxCustomTree(CellOwner).FocusedRowIndex) then
      TsmxCustomTree(CellOwner).TreeTexts[SlaveIndex, TsmxCustomTree(CellOwner).FocusedRowIndex] := Value;
  end;
end;

function TsmxVTColumn.GetColor: TColor;
begin
  Result := VTColumn.Color;
end;

procedure TsmxVTColumn.SetColor(Value: TColor);
var
  VT: TVirtualStringTree;
begin
  if GetColor <> Value then
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

function TsmxVTColumn.GetFont: TFont;
begin
  Result := VTColumnFont;
end;

procedure TsmxVTColumn.SetFont(Value: TFont);
begin
  VTColumnFont.Assign(Value);
end;

function TsmxVTColumn.GetValue: Variant;
begin
  Result := Variants.Null;
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
end;

procedure TsmxVTColumn.SetValue(const Value: Variant);
begin
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

function TsmxVTColumn.GetHeaderText: String;
begin
  Result := String(VTColumn.Text);
end;

procedure TsmxVTColumn.SetHeaderText(const Value: String);
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
    if TObject((CellOwner as IsmxRefComponent).GetInternalRef) is TVirtualStringTree then
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

procedure TsmxVTColumn.ChangeObjectIndex(Value: Integer);
begin
  VTColumn.Index := Value;
end;

procedure TsmxVTColumn.SetOptions(Value: TsmxColumnOptions);
begin
  inherited SetOptions(Value);
  if coResize in Value then
    VTColumn.Options := VTColumn.Options + [coResizable]
  else
    VTColumn.Options := VTColumn.Options - [coResizable];  
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

function TsmxVTNodes.GetNodeText(ColIndex: Integer; Node: PVirtualNode): String;
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

procedure TsmxVTNodes.SetNodeText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
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

procedure TsmxVTGrid.InternalApply;
var
  Form: TsmxCustomForm;
begin
  inherited InternalApply;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  if Assigned(Request) then
    if Assigned(Form) and (Form.ID = 0) then
      Request.Insert
    else
      Request.Update;
end;

procedure TsmxVTGrid.InternalCancel;
begin
  inherited InternalCancel;
  if Assigned(Request) then
    Request.Delete;
end;

procedure TsmxVTGrid.InternalPrepare;
begin
  inherited InternalPrepare;
  VTGrid.Clear;
  if Assigned(Request) then
  begin
    Request.Prepare;
    if Assigned(Request.DataSet) and Request.DataSet.Active then
      VTGrid.RootNodeCount := Request.DataSet.RecordCount;
  end else
  begin
    VTNodes.Clear;
  end;
end;

procedure TsmxVTGrid.InternalRefresh;
begin
  inherited InternalRefresh;
  VTGrid.Clear;
  if Assigned(Request) then
  begin
    Request.Execute;
    if Assigned(Request.DataSet) then
      VTGrid.RootNodeCount := Request.DataSet.RecordCount;
  end else
  begin
    VTNodes.Clear;
  end;
end;

function TsmxVTGrid.GetHint: String;
begin
  Result := VTGrid.Hint;
end;

procedure TsmxVTGrid.SetHint(const Value: String);
begin
  VTGrid.Hint := Value;
end;

function TsmxVTGrid.GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  Result := '';
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetTextFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
              Result := Variants.VarToStr(Field.Value);
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

procedure TsmxVTGrid.SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetTextFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := smxFuncs.StrToVar(Value);
              Request.DataSet.Post;
            end;
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

function TsmxVTGrid.GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  Result := Variants.Null;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
              Result := Field.Value;
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

procedure TsmxVTGrid.SetDBValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := Value;
              Request.DataSet.Post;
            end;
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
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
    Result := VTGrid.FocusedNode.Index
  else
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

function TsmxVTGrid.GetGridText(ColIndex, RowIndex: Integer): String;
var
  Node: PVirtualNode;
begin
  Result := '';
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
  begin
    if Assigned(Request) then
      Result := DBText[ColIndex, Node]
    else
      Result := VTNodes.NodeTexts[ColIndex, Node];
  end;
end;

procedure TsmxVTGrid.SetGridText(ColIndex, RowIndex: Integer; const Value: String);
var
  Node: PVirtualNode;
begin
  if coEditing in Slaves[ColIndex].Options then
  begin
    Node := RowIndexToNode(RowIndex);
    if Assigned(Node) then
    begin
      if Assigned(Request) then
        DBText[ColIndex, Node] := Value
      else
        VTNodes.NodeTexts[ColIndex, Node] := Value;
    end;
  end;
end;

function TsmxVTGrid.GetGridValue(ColIndex, RowIndex: Integer): Variant;
var
  Node: PVirtualNode;
begin
  Result := Variants.Null;
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
  begin
    if Assigned(Request) then
      Result := DBValue[ColIndex, Node]
    else
      Result := VTNodes.NodeValues[ColIndex, Node];
  end;
end;

procedure TsmxVTGrid.SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant);
var
  Node: PVirtualNode;
begin
  if coEditing in Slaves[ColIndex].Options then
  begin
    Node := RowIndexToNode(RowIndex);
    if Assigned(Node) then
    begin
      if Assigned(Request) then
        DBValue[ColIndex, Node] := Value
      else
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

procedure TsmxVTGrid.SetOptions(Value: TsmxGridOptions);
begin
  inherited SetOptions(Value);
  if goColLines in Value then
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions + [toShowVertGridLines]
  else
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions - [toShowVertGridLines];
  if goRowLines in Value then
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions + [toShowHorzGridLines]
  else
    VTGrid.TreeOptions.PaintOptions := VTGrid.TreeOptions.PaintOptions - [toShowHorzGridLines];
  if goRowSelect in Value then
    VTGrid.TreeOptions.SelectionOptions := VTGrid.TreeOptions.SelectionOptions + [toFullRowSelect]
  else
    VTGrid.TreeOptions.SelectionOptions := VTGrid.TreeOptions.SelectionOptions - [toFullRowSelect];
  if goShowHeader in Value then
    VTGrid.Header.Options := VTGrid.Header.Options + [hoVisible]
  else
    VTGrid.Header.Options := VTGrid.Header.Options - [hoVisible];
  if goEditing in Value then
    VTGrid.TreeOptions.MiscOptions := VTGrid.TreeOptions.MiscOptions + [toEditable]
  else
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
          TargetCanvas.TextOut(TextRectangle.Left, TextRectangle.Top, String(TsmxCustomColumn(Wrapper).HeaderText));
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
    TsmxCustomColumn(Wrapper).Click;
end;

procedure TsmxVTGrid.VTGridEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    Allowed := coEditing in TsmxCustomColumn(Wrapper).Options;
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
    Text := WideString(DBText[Column, Node])
  else
    Text := WideString(VTNodes.NodeTexts[Column, Node]);
end;

procedure TsmxVTGrid.VTGridHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(Sender.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TsmxCustomColumn(Wrapper).ClickHeader;
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
    DBText[Column, Node] := String(NewText)
  else
    VTNodes.NodeTexts[Column, Node] := String(NewText);
end;

procedure TsmxVTGrid.VTGridPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TargetCanvas.Font := TsmxCustomColumn(Wrapper).Font;
end;

{ TsmxVTTree }

destructor TsmxVTTree.Destroy;
begin
  inherited Destroy;
  if Assigned(FVTNodes) then
    FVTNodes.Free;
  if Assigned(FVTTree) then
    FVTTree.Free;
  if Assigned(FEditorIntf) then
    FEditorIntf := nil;
end;

function TsmxVTTree.AddRow(RowIndex: Pointer): Pointer;
begin
  if Assigned(RowIndex) then
    Result := VTTree.AddChild(RowIndex)
  else
    Result := nil;
end;

procedure TsmxVTTree.DelRow(RowIndex: Pointer);
begin
  if Assigned(RowIndex) then 
    VTTree.DeleteNode(RowIndex);
end;

function TsmxVTTree.GetHint: String;
begin
  Result := VTTree.Hint;
end;

procedure TsmxVTTree.SetHint(const Value: String);
begin
  VTTree.Hint := Value;
end;

function TsmxVTTree.GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  Result := '';
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetTextFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
              Result := Variants.VarToStr(Field.Value);
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

procedure TsmxVTTree.SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(smxFuncs.GetTextFieldName(Slaves[ColIndex].Name));
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := smxFuncs.StrToVar(Value);
              Request.DataSet.Post;
            end;
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

function TsmxVTTree.GetDBValue(ColIndex: Integer; Node: PVirtualNode): Variant;
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  Result := Variants.Null;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
              Result := Field.Value;
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

procedure TsmxVTTree.SetDBValue(ColIndex: Integer; Node: PVirtualNode; const Value: Variant);
var
  CurRecordNo: Integer;
  Field: IsmxField;
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        CurRecordNo := Request.DataSet.RecordNo;
        try
          if SetRecordNoByNode(Node) then
          begin
            Field := Request.DataSet.FindField(Slaves[ColIndex].Name);
            if Assigned(Field) then
            begin
              Request.DataSet.Edit;
              Field.Value := Value;
              Request.DataSet.Post;
            end;
          end;
        finally
          Request.DataSet.RecordNo := CurRecordNo;
        end;
      end;
end;

function TsmxVTTree.GetExpanded(RowIndex: Pointer): Boolean;
begin
  if Assigned(RowIndex) then
    Result := VTTree.Expanded[RowIndex]
  else
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

function TsmxVTTree.GetTreeText(ColIndex: Integer; RowIndex: Pointer): String;
begin
  Result := '';
  if Assigned(RowIndex) then
  begin
    if Assigned(Request) then
      Result := DBText[ColIndex, RowIndex]
    else
      Result := VTNodes.NodeTexts[ColIndex, RowIndex];
  end;
end;

procedure TsmxVTTree.SetTreeText(ColIndex: Integer; RowIndex: Pointer; const Value: String);
begin
  if coEditing in Slaves[ColIndex].Options then
    if Assigned(RowIndex) then
    begin
      if Assigned(Request) then
        DBText[ColIndex, RowIndex] := Value
      else
        VTNodes.NodeTexts[ColIndex, RowIndex] := Value;
    end;
end;

function TsmxVTTree.GetTreeValue(ColIndex: Integer; RowIndex: Pointer): Variant;
begin
  Result := Variants.Null;
  if Assigned(RowIndex) then
  begin
    if Assigned(Request) then
      Result := DBValue[ColIndex, RowIndex]
    else
      Result := VTNodes.NodeValues[ColIndex, RowIndex];
  end;
end;

procedure TsmxVTTree.SetTreeValue(ColIndex: Integer; RowIndex: Pointer; const Value: Variant);
begin
  if coEditing in Slaves[ColIndex].Options then
    if Assigned(RowIndex) then
    begin
      if Assigned(Request) then
        DBValue[ColIndex, RowIndex] := Value
      else
        VTNodes.NodeValues[ColIndex, RowIndex] := Value;
    end;
end;

function TsmxVTTree.GetInternalRef: Pointer;
begin
  Result := Pointer(VTTree);
end;

function TsmxVTTree.GetEditor: IsmxTreeEditor;
begin
  if not Assigned(FEditorIntf) then
    FEditorIntf := TsmxVTEditor.Create(nil) as IsmxTreeEditor;
  Result := FEditorIntf;
end;

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
    Result := VTTree.ChildCount[RowIndex]
  else
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

procedure TsmxVTTree.InternalApply;
var
  Form: TsmxCustomForm;
begin
  inherited InternalApply;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  if Assigned(Request) then
    if Assigned(Form) and (Form.ID = 0) then
      Request.Insert
    else
      Request.Update;
end;

procedure TsmxVTTree.InternalCancel;
begin
  inherited InternalCancel;
  if Assigned(Request) then
    Request.Delete;
end;

procedure TsmxVTTree.InternalPrepare;
begin
  inherited InternalPrepare;
  VTTree.Clear;
  if Assigned(Request) then
  begin
    Request.Prepare;
    if Assigned(Request.DataSet) and Request.DataSet.Active then
      VTTree.RootNodeCount := Request.DataSet.RecordCount;
  end else
  begin
    VTNodes.Clear;
  end;
end;

procedure TsmxVTTree.InternalRefresh;
begin
  inherited InternalRefresh;
  VTTree.Clear;
  if Assigned(Request) then
  begin
    Request.Execute;
    if Assigned(Request.DataSet) then
      VTTree.RootNodeCount := Request.DataSet.RecordCount;
  end else
  begin
    VTNodes.Clear;
  end;
end;

function TsmxVTTree.RowLevel(RowIndex: Pointer): Integer;
begin
  if Assigned(RowIndex) then
    Result := VTTree.GetNodeLevel(RowIndex)
  else
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

procedure TsmxVTTree.SetOptions(Value: TsmxTreeOptions);
begin
  inherited SetOptions(Value);
  if toColLines in Value then
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions + [toShowVertGridLines]
  else
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions - [toShowVertGridLines];
  if toRowLines in Value then
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions + [toShowHorzGridLines]
  else
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions - [toShowHorzGridLines];
  if toRowSelect in Value then
    VTTree.TreeOptions.SelectionOptions := VTTree.TreeOptions.SelectionOptions + [toFullRowSelect]
  else
    VTTree.TreeOptions.SelectionOptions := VTTree.TreeOptions.SelectionOptions - [toFullRowSelect];
  if toShowHeader in Value then
    VTTree.Header.Options := VTTree.Header.Options + [hoVisible]
  else
    VTTree.Header.Options := VTTree.Header.Options - [hoVisible];
  if toEditing in Value then
    VTTree.TreeOptions.MiscOptions := VTTree.TreeOptions.MiscOptions + [toEditable]
  else
    VTTree.TreeOptions.MiscOptions := VTTree.TreeOptions.MiscOptions - [toEditable];
  if toTreeLines in Value then
    VTTree.TreeOptions.PaintOptions := VTTree.TreeOptions.PaintOptions + [toShowTreeLines]
  else
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
          TargetCanvas.TextOut(TextRectangle.Left, TextRectangle.Top, String(TsmxCustomColumn(Wrapper).HeaderText));
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
      if coEditing in TsmxCustomColumn(Wrapper).Options then
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
    TsmxCustomColumn(Wrapper).Click;
end;

procedure TsmxVTTree.VTTreeCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := Editor as IVTEditLink;
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
    Allowed := coEditing in TsmxCustomColumn(Wrapper).Options;
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
    Text := WideString(DBText[Column, Node])
  else
    Text := WideString(VTNodes.NodeTexts[Column, Node]);
end;

procedure TsmxVTTree.VTTreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(Sender.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TsmxCustomColumn(Wrapper).ClickHeader;
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
    DBText[Column, Node] := String(NewText)
  else
    VTNodes.NodeTexts[Column, Node] := String(NewText);
end;

procedure TsmxVTTree.VTTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Wrapper: TObject;
begin
  Wrapper := TObject(TVirtualStringTree(Sender).Header.Columns[Column].Tag);
  if Wrapper is TsmxCustomColumn then
    TargetCanvas.Font := TsmxCustomColumn(Wrapper).Font;
end;

{ TsmxVTEditor }

function TsmxVTEditor.BeginEdit: Boolean;
begin
  if Assigned(Control) then
  begin
    Control.Show;
    if Control.CanFocus then
      Control.SetFocus;
    Result := True;
  end else
    Result := False;
end;

function TsmxVTEditor.CancelEdit: Boolean;
begin
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
begin
  TsmxCustomTree(FTree.Tag).Edited;
  if Assigned(Control) then
  begin
    Control.Hide;
    if FTree.CanFocus then
      FTree.SetFocus;
    Result := True;
  end else
    Result := False;
end;

function TsmxVTEditor.GetBounds: TRect;
begin
  if Assigned(Control) then
    Result := Control.BoundsRect;
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
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  FEditorType := etNone;
  if Assigned(FControl) then
  begin
    FControl.Free;
    FControl := nil;
  end;
  TsmxCustomTree(FTree.Tag).Editing;
end;

procedure TsmxVTEditor.ProcessMessage(var Message: TMessage);
begin
  if Assigned(Control) then
    Control.WindowProc(Message);
end;

procedure TsmxVTEditor.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  if Assigned(Control) then
  begin
    FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
    Dec(R.Right, 1);
    Dec(R.Left, 2);
    Control.BoundsRect := R;
  end;
end;

function TsmxVTEditor.GetEditorType: TsmxEditorType;
begin
  Result := FEditorType;
end;

procedure TsmxVTEditor.SetEditorType(Value: TsmxEditorType);
begin
  FEditorType := Value;
end;

initialization
  Classes.RegisterClasses([TsmxVTColumn, TsmxVTGrid, TsmxVTTree]);

finalization
  Classes.UnRegisterClasses([TsmxVTColumn, TsmxVTGrid, TsmxVTTree]);

end.
