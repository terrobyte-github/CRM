unit smxVTCells;

interface

uses
  Classes, Controls, Graphics, VirtualTrees, smxBaseClasses, smxClasses,
  smxTypes;

type
  { TsmxVTColumn }

  TsmxVTColumn = class(TsmxCustomColumn)
  private
    FIsEditing: Boolean;
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
    procedure DoSnapHeader; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetColumnAlignment: TAlignment; override;
    function GetColumnCaption: String; override;
    function GetColumnColor: TColor; override;
    function GetColumnFont: TFont; override;
    function GetColumnValue: Variant; override;
    function GetHeaderAlignment: TAlignment; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetHeaderCaption: String; override;
    function GetIsEditing: Boolean; override;
    function GetInternalObject: TObject; override;
    procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetColumnAlignment(Value: TAlignment); override;
    procedure SetColumnCaption(const Value: String); override;
    procedure SetColumnColor(Value: TColor); override;
    procedure SetColumnFont(Value: TFont); override;
    procedure SetColumnValue(const Value: Variant); override;
    procedure SetCellFeedBack; override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderCaption(const Value: String); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;
    procedure SetIsEditing(Value: Boolean); override;
    procedure SetSlaveIndex(Value: Integer); override;

    property VTColumn: TVirtualTreeColumn read GetVTColumn;
    property VTColumnFont: TFont read GetVTColumnFont;
    property VTHeaderColor: TColor read FVTHeaderColor;
    property VTHeaderFont: TFont read GetVTHeaderFont;
  public
    destructor Destroy; override;
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

  { TsmxVTGrid }

  TsmxVTGrid = class(TsmxCustomGrid)
  private
    FVTGrid: TVirtualStringTree;
    FVTNodes: TsmxVTNodes;
    function GetDBText(ColIndex: Integer; Node: PVirtualNode): String;
    function GetVTGrid: TVirtualStringTree;
    function GetVTNodes: TsmxVTNodes;
    function RowIndexToNode(RowIndex: Integer): PVirtualNode;
    procedure SetDBText(ColIndex: Integer; Node: PVirtualNode; const Value: String);
    function SetRecordNoByNode(Node: PVirtualNode): Boolean;
    procedure VTGridAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements);
    procedure VTGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTGridColumnClick(Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
    procedure VTGridEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
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
    procedure DoApply; override;
    procedure DoChangeRow; override;
    procedure DoPrepare; override;
    procedure DoRefresh; override;
    function GetCellHint: String; override;
    function GetCfgClass: TsmxBaseCfgClass; override;
    function GetFocusedRowIndex: Integer; override;
    function GetFocusedColIndex: Integer; override;
    function GetGridCaption(ColIndex, RowIndex: Integer): String; override;
    function GetGridValue(ColIndex, RowIndex: Integer): Variant; override;
    function GetInternalObject: TObject; override;
    function GetRowCount: Integer; override;
    function GetSlaveClass: TsmxOwnerCellClass; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure ResetCellProps; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetCellProps; override;
    procedure SetCellHint(const Value: String); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Integer); override;
    procedure SetGridCaption(ColIndex, RowIndex: Integer; const Value: String); override;
    procedure SetGridOptions(Value: TsmxGridOptions); override;
    procedure SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant); override;
    procedure SetRowCount(Value: Integer); override;

    property DBText[ColIndex: Integer; Node: PVirtualNode]: String read GetDBText write SetDBText;
    property VTGrid: TVirtualStringTree read GetVTGrid;
    property VTNodes: TsmxVTNodes read GetVTNodes;
  public
    destructor Destroy; override;
  end;

implementation

uses
  Variants, smxCfgs, smxFuncs, smxClassFuncs, smxDBFuncs, smxDBIntf;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

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

procedure TsmxVTColumn.DoSnapHeader;
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

function TsmxVTColumn.GetCellVisible: Boolean;
begin
  Result := coVisible in VTColumn.Options;
end;

procedure TsmxVTColumn.SetCellVisible(Value: Boolean);
begin
  if Value then
    VTColumn.Options := VTColumn.Options + [coVisible] else
    VTColumn.Options := VTColumn.Options - [coVisible];
end;

function TsmxVTColumn.GetCellWidth: Integer;
begin
  Result := VTColumn.Width;
end;

procedure TsmxVTColumn.SetCellWidth(Value: Integer);
begin
  VTColumn.Width := Value;
end;

function TsmxVTColumn.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxColumnCfg;
end;

function TsmxVTColumn.GetColumnAlignment: TAlignment;
begin
  Result := VTColumn.Alignment;
end;

procedure TsmxVTColumn.SetColumnAlignment(Value: TAlignment);
begin
  VTColumn.Alignment := Value;
end;

function TsmxVTColumn.GetColumnCaption: String;
begin
  if Assigned(CellOwner) then
    if CellOwner.FocusedRowIndex <> -1 then
      Result := CellOwner.GridCaptions[SlaveIndex, CellOwner.FocusedRowIndex];
end;

procedure TsmxVTColumn.SetColumnCaption(const Value: String);
//var
  //Val: Variant;
begin
  //Val := smxFuncs.StrToVar(Value);
  //SetFieldValue(smxFuncs.GetTextFieldName(SlaveName), Val);
  if Assigned(CellOwner) then
    if CellOwner.FocusedRowIndex <> -1 then
      CellOwner.GridCaptions[SlaveIndex, CellOwner.FocusedRowIndex] := Value;
end;

function TsmxVTColumn.GetColumnColor: TColor;
begin
  Result := VTColumn.Color;
end;

procedure TsmxVTColumn.SetColumnColor(Value: TColor);
begin
  VTColumn.Color := Value;
end;

function TsmxVTColumn.GetColumnFont: TFont;
begin
  Result := VTColumnFont;
end;

procedure TsmxVTColumn.SetColumnFont(Value: TFont);
begin
  FVTColumnFont.Assign(Value);
end;

function TsmxVTColumn.GetColumnValue: Variant;
begin
  Result := Variants.Null;
  if Assigned(CellOwner) then
    if CellOwner.FocusedRowIndex <> -1 then
      Result := CellOwner.GridValues[SlaveIndex, CellOwner.FocusedRowIndex];
end;

procedure TsmxVTColumn.SetColumnValue(const Value: Variant);
begin
  //SetFieldValue(SlaveName, Value);
  if Assigned(CellOwner) then
    if CellOwner.FocusedRowIndex <> -1 then
      CellOwner.GridValues[SlaveIndex, CellOwner.FocusedRowIndex] := Value;
end;

function TsmxVTColumn.GetHeaderAlignment: TAlignment;
begin
  Result := VTColumn.CaptionAlignment;
end;

procedure TsmxVTColumn.SetHeaderAlignment(Value: TAlignment);
begin
  VTColumn.CaptionAlignment := Value;
  VTColumn.Options := VTColumn.Options + [coUseCaptionAlignment];
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
  FVTHeaderFont.Assign(Value);
end;

function TsmxVTColumn.GetInternalObject: TObject;
begin
  Result := VTColumn;
end;

function TsmxVTColumn.GetIsEditing: Boolean;
begin
  Result := FIsEditing;
end;

procedure TsmxVTColumn.SetIsEditing(Value: Boolean);
begin
  FIsEditing := Value;
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
  if _TsmxBaseCell(CellOwner).GetInternalObject is TVirtualStringTree then
    TVirtualStringTree(_TsmxBaseCell(CellOwner).GetInternalObject).Header.Invalidate(VTColumn);
end;

procedure TsmxVTColumn.InvalidateColumn;
begin
  if _TsmxBaseCell(CellOwner).GetInternalObject is TVirtualStringTree then
    TVirtualStringTree(_TsmxBaseCell(CellOwner).GetInternalObject).InvalidateColumn(VTColumn.Index);
end;

procedure TsmxVTColumn.ResetCellProps;
begin
  inherited ResetCellProps;
  ColumnAlignment := taLeftJustify;
  //ColumnCaption := TsmxColumnCfg(Cfg).ColumnText.Caption;
  ColumnColor := Graphics.clBlack;
  ColumnFont.Color := Graphics.clBlack;
  ColumnFont.Name := '';
  ColumnFont.Size := 0;
  ColumnFont.Style := [];
  //FieldName := '';
  HeaderAlignment := taLeftJustify;
  HeaderCaption := '';
  HeaderColor := Graphics.clBlack;
  HeaderFont.Color := Graphics.clBlack;
  HeaderFont.Name := '';
  HeaderFont.Size := 0;
  HeaderFont.Style := [];
  IsEditing := False;
  OnSnapHeader := nil;
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
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TVirtualStringTree then
      VTColumn.Collection := TVirtualStringTree(Obj).Header.Columns;
  end;
end;

procedure TsmxVTColumn.SetCellProps;
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
    //FieldName := TsmxColumnCfg(Cfg).ColumnFieldName;
    HeaderAlignment := TsmxColumnCfg(Cfg).ColumnHeader.Alignment;
    HeaderCaption := TsmxColumnCfg(Cfg).ColumnHeader.Caption;
    HeaderColor := TColor(TsmxColumnCfg(Cfg).ColumnHeader.Color);
    HeaderFont.Color := TColor(TsmxColumnCfg(Cfg).ColumnHeader.Font.Color);
    HeaderFont.Name := TsmxColumnCfg(Cfg).ColumnHeader.Font.Name;
    HeaderFont.Size := TsmxColumnCfg(Cfg).ColumnHeader.Font.Size;
    HeaderFont.Style := TsmxColumnCfg(Cfg).ColumnHeader.Font.Style;
    IsEditing := TsmxColumnCfg(Cfg).IsEditing;
    Form := smxClassFuncs.GetAccessoryForm(Self);
    if Assigned(Form) then
      OnSnapHeader := smxClassFuncs.GetEventForm(Form, TsmxColumnCfg(Cfg).SnapHeaderAlgCfgID);
  end;
end;

procedure TsmxVTColumn.SetSlaveIndex(Value: Integer);
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

procedure TsmxVTGrid.DoApply;
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
end;

procedure TsmxVTGrid.DoChangeRow;
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

procedure TsmxVTGrid.DoPrepare;
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
end;

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

procedure TsmxVTGrid.DoRefresh;
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
end;

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

function TsmxVTGrid.GetCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxGridCfg;
end;

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
            Field := Request.DataSet.FindField(Slaves[ColIndex].SlaveName);
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
            Field := Request.DataSet.FindField(Slaves[ColIndex].SlaveName);
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
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
    Result := VTGrid.Text[Node, ColIndex] else
    Result := '';
end;

procedure TsmxVTGrid.SetGridCaption(ColIndex, RowIndex: Integer; const Value: String);
var
  Node: PVirtualNode;
begin
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
    VTGrid.Text[Node, ColIndex] := Value;
end;

function TsmxVTGrid.GetGridValue(ColIndex, RowIndex: Integer): Variant;
var
  Node: PVirtualNode;
begin
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
    Result := smxFuncs.StrToVar(VTGrid.Text[Node, ColIndex]) else
    Result := Variants.Null;
end;

procedure TsmxVTGrid.SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant);
var
  Node: PVirtualNode;
begin
  Node := RowIndexToNode(RowIndex);
  if Assigned(Node) then
    VTGrid.Text[Node, ColIndex] := Variants.VarToStr(Value);
end;

function TsmxVTGrid.GetInternalObject: TObject;
begin
  Result := VTGrid;
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

function TsmxVTGrid.GetSlaveClass: TsmxOwnerCellClass;
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
    FVTGrid.LineStyle := lsSolid;
    FVTGrid.Colors.GridLineColor := clSilver;
    FVTGrid.EditDelay := 50;
    FVTGrid.OnAdvancedHeaderDraw := VTGridAdvancedHeaderDraw;
    FVTGrid.OnChange := VTGridChange;
    FVTGrid.OnColumnClick := VTGridColumnClick;
    FVTGrid.OnGetText := VTGridGetText;
    FVTGrid.OnDblClick := ControlDblClick;
    FVTGrid.OnEditing := VTGridEditing;
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

procedure TsmxVTGrid.ResetCellProps;
begin
  inherited ResetCellProps;
  GridOptions := [];
  OnApply := nil;
  OnChangeRow := nil;
  OnPrepare := nil;
  OnRefresh := nil;
  Request := nil;
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
    Obj := _TsmxBaseCell(CellParent).GetInternalObject;
    if Obj is TWinControl then
      VTGrid.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxVTGrid.SetCellProps;
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
      OnApply := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).ApplyAlgCfgID);
      OnChangeRow := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).ChangeRowAlgCfgID);
      OnPrepare := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).PrepareAlgCfgID);
      OnRefresh := smxClassFuncs.GetEventForm(Form, TsmxGridCfg(Cfg).RefreshAlgCfgID);
    end;
  end;
end;

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
  if goOwnerDrawHeader in Value then
    VTGrid.Header.Options := VTGrid.Header.Options + [hoOwnerDraw] else
    VTGrid.Header.Options := VTGrid.Header.Options - [hoOwnerDraw];
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
          TargetCanvas.FillRect(PaintRectangle);
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
    Allowed := TsmxCustomColumn(Wrapper).IsEditing;
end;

procedure TsmxVTGrid.VTGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);
begin
  if Assigned(Request) then
    Text := WideString(DBText[Column, Node])
  else
    Text := WideString(VTNodes.NodeCaptions[Column, Node]);
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
    DBText[Column, Node] := String(NewText)
  else
    VTNodes.NodeCaptions[Column, Node] := String(NewText);
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

initialization
  Classes.RegisterClasses([TsmxVTColumn, TsmxVTGrid]);

end.
