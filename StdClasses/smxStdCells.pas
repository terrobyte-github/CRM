{**************************************}
{                                      }
{            SalesMan v1.0             }
{        Standard cell classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxStdCells;

interface

uses
  Classes, Controls, ComCtrls, DB, DBGrids, Forms, ExtCtrls, StdCtrls, Buttons,
  Menus, ActnList, Windows, ImgList, Graphics, smxBaseClasses, smxClasses,
  smxCells, smxCfgs, smxStdCtrls, smxDBIntf, smxTypes, smxClassTypes,
  smxManagerIntf, smxBaseTypes;

type
  { TsmxLibAction }

  TsmxLibAction = class(TsmxAction)
  private
    FLibraryName: String;
    FProcedureName: String;
  protected
    procedure SetLibraryName(const Value: String); virtual;
    procedure SetProcedureName(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
  published
    procedure AlgorithmExecute(Sender: TsmxComponent); override;

    property LibraryName: String read FLibraryName write SetLibraryName;
    property ProcedureName: String read FProcedureName write SetProcedureName;
  end;

  { TsmxActionList }

  TsmxActionList = class(TsmxCustomAlgorithmList)
  private
    FActionList: TActionList;
    function GetActionList: TActionList;
  protected
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetImageList(Value: TCustomImageList); override;

    property ActionList: TActionList read GetActionList;
  public
    destructor Destroy; override;
  published
    property ImageListName;
    property SlaveList;
  end;

  { TsmxRequestList }

  TsmxRequestList = class(TsmxCustomRequestList)
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  published
    property SlaveList;
  end;

  { TsmxColumn }

  TsmxColumn = class(TsmxCustomColumn)
  private
    FColumn: TColumn;
    function GetColumn: TColumn;
  protected
    function GetVisible: Boolean; override;
    function GetWidth: Integer; override;
    function GetAlignment: TAlignment; override;
    function GetCaption: String; override;
    function GetColor: TColor; override;
    function GetFont: TFont; override;
    function GetValue: Variant; override;
    function GetHeaderAlignment: TAlignment; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetHeaderCaption: String; override;
    function GetInternalRef: Pointer; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
    procedure SetAlignment(Value: TAlignment); override;
    procedure SetCaption(const Value: String); override;
    procedure SetColor(Value: TColor); override;
    procedure SetFont(Value: TFont); override;
    procedure SetOptions(Value: TsmxColumnOptions); override;
    procedure SetValue(const Value: Variant); override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderCaption(const Value: String); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure ChangeObjectIndex(Value: Integer); override;

    property Column: TColumn read GetColumn;
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
    property HeaderCaption;
    property HeaderColor;
    property HeaderFont;

    property OnClick;
    property OnClickHeader;
  end;

  { TsmxDBGrid }

  TsmxDBGrid = class(TsmxCustomGrid)
  private
    FDBGrid: TsmxWheelDBGrid;
    function GetDBGrid: TsmxWheelDBGrid;
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDataChange(Sender: TObject; Field: TField);
    procedure DBGridTitleClick(Column: TColumn);
  protected
    function GetHint: String; override;
    function GetFocusedColIndex: Integer; override;
    function GetFocusedRowIndex: Integer; override;
    function GetGridCaption(ColIndex, RowIndex: Integer): String; override;
    function GetGridValue(ColIndex, RowIndex: Integer): Variant; override;
    function GetInternalRef: Pointer; override;
    function GetRowCount: Integer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetHint(const Value: String); override;
    procedure SetFocusedColIndex(Value: Integer); override;
    procedure SetFocusedRowIndex(Value: Integer); override;
    procedure SetGridCaption(ColIndex, RowIndex: Integer; const Value: String); override;
    procedure SetOptions(Value: TsmxGridOptions); override;
    procedure SetGridValue(ColIndex, RowIndex: Integer; const Value: Variant); override;
    procedure SetRequest(Value: TsmxCustomRequest); override;

    property DBGrid: TsmxWheelDBGrid read GetDBGrid;
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

  { TsmxEditFilter }

  TsmxEditFilter = class(TsmxFilter)
  private
    FEdit: TEdit;
    FValue: Variant;
    function GetEdit: TEdit;
    procedure EditChange(Sender: TObject);
  protected
    function GetText: String; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetColor: TColor; override;
    function GetFont: TFont; override;
    function GetValue: Variant; override;
    procedure SetText(const Value: String); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetColor(Value: TColor); override;
    procedure SetFont(Value: TFont); override;
    procedure SetValue(const Value: Variant); override;

    property Edit: TEdit read GetEdit;
  public
    destructor Destroy; override;
  end;

  { TsmxDateTimeFilter }

  {TsmxDateTimeFilter = class(TsmxFilter)
  private
    FDateTime: TDateTimePicker;
    procedure SetFormat;
  protected
    function GetCellEnabled: Boolean; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetFilterValue(Value: Variant); override;

    property DateTime: TDateTimePicker read FDateTime;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxBitBtnFilter }

  TsmxBitBtnFilter = class(TsmxFilter)
  private
    FBitBtn: TBitBtn;
    FValue: Variant;
    FButtonImageIndex: TImageIndex;
    function GetBitBtn: TBitBtn;
  protected
    function GetText: String; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetImageIndex: TImageIndex; override;
    function GetColor: TColor; override;
    function GetFont: TFont; override;
    function GetValue: Variant; override;
    procedure SetText(const Value: String); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: TImageIndex); override;
    procedure SetColor(Value: TColor); override;
    procedure SetFont(Value: TFont); override;
    procedure SetValue(const Value: Variant); override;
    procedure SetImageList(Value: TCustomImageList); override;

    property BitBtn: TBitBtn read GetBitBtn;
  public
    destructor Destroy; override;
  published
    property ImageIndex;
    property Text;
    property Font;
    property Options;
    property Value;
    property ImageListName;

    property OnClick;
  end;

  { TsmxNumEditFilter }

  {TsmxNumEditFilter = class(TsmxFilter)
  private
    FEdit: TEdit;
  protected
    procedure ProcKeyPress(Sender: TObject; var Key: Char);
    function GetCellEnabled: Boolean; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnabled(Value: Boolean); override;
    procedure SetFilterValue(Value: Variant); override;

    property Edit: TEdit read FEdit;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxLabelFilter }

  {TsmxLabelFilter = class(TsmxFilter)
  private
    FSLabel: TLabel;
    FValue: Variant;
  protected
    function GetFilterText: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetFilterText(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;

    property SLabel: TLabel read FSLabel;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;}

  { TsmxPanelFilterDesk }

  TsmxPanelFilterDesk = class(TsmxCustomFilterDesk)
  private
    FPanel: TPanel;
    function GetPanel: TPanel;
    procedure RefreshValueParams(DataSet: IsmxDataSet);
  protected
    function GetCaption: TCaption; override;
    function GetHint: String; override;
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure InternalApply; override;
    procedure InternalPrepare; override;
    procedure InternalRefresh; override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetHint(const Value: String); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;

    property Panel: TPanel read GetPanel;
  public
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property Cursor;
    property Enabled;
    property Height;
    property Hint;
    property Left;
    property Top;
    property Visible;
    property Width;
    property PopupMenu;
    property SlaveList;

    property OnDoubleClick;
    property OnClick;
  end;

  { TsmxPanelSection }

  TsmxPanelSection = class(TsmxCustomSection)
  private
    FPanel: TPanel;
    function GetPanel: TPanel;
  protected
    function GetInternalRef: Pointer; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;

    property Panel: TPanel read GetPanel;
  public
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Cursor;
    property Enabled;
    property Height;
    property Left;
    property Top;
    property Visible;
    property Width;
    property PopupMenu;
    property SlaveList;

    property OnDoubleClick;
    property OnClick;
  end;

  { TsmxTabSheet }

  TsmxTabSheet = class(TsmxCustomPage)
  private
    FTabSheet: TTabSheet;
    function GetTabSheet: TTabSheet;
  protected
    function GetCaption: TCaption; override;
    function GetHint: String; override;
    function GetImageIndex: TImageIndex; override;
    function GetVisible: Boolean; override;
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: TImageIndex); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetVisible(Value: Boolean); override;
    procedure ChangeObjectIndex(Value: Integer); override;

    property TabSheet: TTabSheet read GetTabSheet;
  public
    destructor Destroy; override;
  published
    property Caption;
    property Hint;
    property ImageIndex;
    property Visible;
    property PopupMenu;
    property SlaveList;
  end;

  { TsmxPageControl }

  TsmxPageControl = class(TsmxCustomPageManager)
  private
    FPageControl: TPageControl;
    function GetPageControl: TPageControl;
    procedure PageControlChange(Sender: TObject);
  protected
    function GetActivePageIndex: Integer; override;
    function GetInternalRef: Pointer; override;
    function GetIsMultiLine: Boolean; override;
    function GetStyle: TsmxPageManagerStyle; override;
    function GetPagePosition: TsmxPagePosition; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetActivePageIndex(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetIsMultiLine(Value: Boolean); override;
    procedure SetStyle(Value: TsmxPageManagerStyle); override;
    procedure SetPagePosition(Value: TsmxPagePosition); override;

    property PageControl: TPageControl read GetPageControl;
  public
    destructor Destroy; override;
  published
    property ActivePageIndex;
    property Align;
    property Anchors;
    property Cursor;
    property Enabled;
    property Height;
    property Left;
    property Top;
    property Visible;
    property Width;
    property ImageListName;
    property IsMultiLine;
    property Style;
    property PopupMenu;
    property SlaveList;

    property OnChangePage;
  end;

  { TsmxMenuItem }

  TsmxMenuItem = class(TsmxCustomMenuItem)
  private
    FMenuItem: TMenuItem;
    function GetMenuItem: TMenuItem;
  protected
    function GetCaption: TCaption; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetImageIndex: TImageIndex; override;
    function GetVisible: Boolean; override;
    function GetInternalRef: Pointer; override;
    function GetIsChecked: Boolean; override;
    function GetHotKey: Integer; override;
    function GetStyle: TsmxMenuItemStyle; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: TImageIndex); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetIsChecked(Value: Boolean); override;
    procedure SetHotKey(Value: Integer); override;
    procedure SetStyle(Value: TsmxMenuItemStyle); override;
    procedure ChangeObjectIndex(Value: Integer); override;

    property MenuItem: TMenuItem read GetMenuItem;
  public
    destructor Destroy; override;
  end;

  { TsmxMainMenu }

  TsmxMainMenu = class(TsmxCustomMainMenu)
  private
    FMainMenu: TMainMenu;
    function GetMainMenu: TMainMenu;
  protected
    function GetEnabled: Boolean; override;
    function GetVisible: Boolean; override;
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetImageList(Value: TCustomImageList); override;

    property MainMenu: TMainMenu read GetMainMenu;
  public
    destructor Destroy; override;
  end;

  { TsmxPopupMenu }

  TsmxPopupMenu = class(TsmxCustomPopupMenu)
  private
    FPopupMenu: TPopupMenu;
    function GetPopupMenu: TPopupMenu;
  protected
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
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
    function GetSlaveClass: TsmxBaseCellClass; override;
  end;

  { TsmxToolItem }

  TsmxToolItem = class(TsmxCustomToolItem)
  private
    FToolButton: TToolButton;
    function GetToolButton: TToolButton;
  protected
    function GetCaption: TCaption; override;
    function GetHint: String; override;
    function GetImageIndex: TImageIndex; override;
    function GetInternalRef: Pointer; override;
    function GetIsChecked: Boolean; override;
    function GetStyle: TsmxToolItemStyle; override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: TImageIndex); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetIsChecked(Value: Boolean); override;
    procedure SetStyle(Value: TsmxToolItemStyle); override;

    property ToolButton: TToolButton read GetToolButton;
  public
    destructor Destroy; override;
  published
    property Caption;
    property Hint;
    property ImageIndex;
    property IsChecked;
    property Style;

    property OnClick;
  end;

  { TsmxToolBar }

  TsmxToolBar = class(TsmxCustomToolBoard)
  private
    FToolBar: TToolBar;
    function GetToolBar: TToolBar;
  protected
    function GetInternalRef: Pointer; override;
    function GetIsFlat: Boolean; override;
    function GetIsShowCaptions: Boolean; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetIsFlat(Value: Boolean); override;
    procedure SetIsShowCaptions(Value: Boolean); override;

    property ToolBar: TToolBar read GetToolBar;
  public
    destructor Destroy; override;
  published
    property ImageListName;
    property IsFlat;
    property IsShowCaptions;
  end;

  { TsmxControlBar }

  TsmxControlBar = class(TsmxCustomControlBoard)
  private
    FControlBar: TControlBar;
    function GetControlBar: TControlBar;
  protected
    function GetInternalRef: Pointer; override;
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;

    property ControlBar: TControlBar read GetControlBar;
  public
    destructor Destroy; override;
  end;

  { TsmxStatusPanel }

  TsmxStatusPanel = class(TsmxCustomStatusItem)
  private
    FStatusPanel: TStatusPanel;
    function GetStatusPanel: TStatusPanel;
  protected
    function GetCaption: TCaption; override;
    function GetWidth: Integer; override;
    function GetAlignment: TAlignment; override;
    function GetStyle: TsmxStatusItemStyle; override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetWidth(Value: Integer); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure ChangeObjectIndex(Value: Integer); override;
    procedure SetAlignment(Value: TAlignment); override;
    procedure SetStyle(Value: TsmxStatusItemStyle); override;

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
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;

    property StatusBar: TStatusBar read GetStatusBar;
  public
    destructor Destroy; override;
  end;

  { TsmxStateForm }

  TsmxStateForm = class(TsmxForm)
  private
    FStateCfg: TsmxBaseCfg;
    FStateID: Integer;
    FStateRequest: TsmxCustomRequest;
    function GetStateCfg: TsmxStateCfg;
  protected
    function GetStateCfgClass: TsmxBaseCfgClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RefreshStateCfg; virtual;
    procedure RefreshStateID; virtual;
    procedure SetCfgID(Value: Integer); override;
    procedure SetIntfID(Value: Integer); override;
    procedure SetStateID(Value: Integer); virtual;
    procedure SetStateRequest(Value: TsmxCustomRequest); virtual;

    property StateCfg: TsmxStateCfg read GetStateCfg;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CellParams(const Name: String; var Value: Variant): Boolean; override;
  published
    property StateID: Integer read FStateID write SetStateID;
    property StateRequest: TsmxCustomRequest read FStateRequest write SetStateRequest;
  end;

  { TsmxStandardForm }

  TsmxStandardForm = class(TsmxStateForm)
  private
    FControlBoard: TsmxCustomControlBoard;
    FMainMenu: TsmxCustomMainMenu;
    FStatusBoard: TsmxCustomStatusBoard;
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetControlBoard(Value: TsmxCustomControlBoard); virtual;
    procedure SetMainMenu(Value: TsmxCustomMainMenu); virtual;
    procedure SetStatusBoard(Value: TsmxCustomStatusBoard); virtual;
  published
    property AlgorithmList;
    property ControlBoard: TsmxCustomControlBoard read FControlBoard write SetControlBoard;
    property MainMenu: TsmxCustomMainMenu read FMainMenu write SetMainMenu;
    property PopupList;
    property RequestList;
    property SlaveList;
    property StatusBoard: TsmxCustomStatusBoard read FStatusBoard write SetStatusBoard;
  end;

implementation

uses
  SysUtils, Variants, ToolWin, Messages,smxFuncs, smxProcs, smxDBFuncs,
  smxClassFuncs, smxConsts, smxClassProcs, smxBaseIntf;

type
  { _TMenuItem }

  _TMenuItem = class(TMenuItem)
  end;

{ TsmxLibAction }

procedure TsmxLibAction.AlgorithmExecute(Sender: TsmxComponent);
var
  ProcExecute: TsmxProcExecuteEvent;
begin
  if Assigned(smxProcs.gLibraryManagerIntf) then
  begin
    ProcExecute := smxProcs.gLibraryManagerIntf.GetProcedure(FLibraryName, FProcedureName);
    if Assigned(ProcExecute) then
    begin
      if Sender is TsmxBaseCell then
      begin
        CellEvent := TsmxBaseCell(Sender);
        if not IsManualRefreshParams then
          RefreshParams;
        ProcExecute(Self);
      end else
        ProcExecute(Sender);
    end;
  end;
end;

procedure TsmxLibAction.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxLibAction then
  begin
    LibraryName := TsmxLibAction(Source).LibraryName;
    ProcedureName := TsmxLibAction(Source).ProcedureName;
  end;
end;

procedure TsmxLibAction.SetLibraryName(const Value: String);
begin
  FLibraryName := Value;
end;

procedure TsmxLibAction.SetProcedureName(const Value: String);
begin
  FProcedureName := Value;
end;

{ TsmxActionList }

destructor TsmxActionList.Destroy;
begin
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

function TsmxActionList.GetInternalRef: Pointer;
begin
  Result := Pointer(ActionList);
end;

function TsmxActionList.GetSlaveClass: TsmxBaseCellClass;
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

{ TsmxRequestList }

function TsmxRequestList.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxRequest;
end;

{ TsmxColumn }

destructor TsmxColumn.Destroy;
begin
  inherited Destroy;
  if Assigned(FColumn) then
    FColumn.Free;
end;

function TsmxColumn.GetVisible: Boolean;
begin
  Result := Column.Visible;
end;

procedure TsmxColumn.SetVisible(Value: Boolean);
begin
  Column.Visible := Value;
end;

function TsmxColumn.GetWidth: Integer;
begin
  Result := Column.Width;
end;

procedure TsmxColumn.SetWidth(Value: Integer);
begin
  Column.Width := Value;
end;

function TsmxColumn.GetColumn: TColumn;
begin
  if not Assigned(FColumn) then
    FColumn := TColumn.Create(nil);
  Result := FColumn;
end;

function TsmxColumn.GetAlignment: TAlignment;
begin
  Result := Column.Alignment;
end;

procedure TsmxColumn.SetAlignment(Value: TAlignment);
begin
  Column.Alignment := Value;
end;

function TsmxColumn.GetCaption: String;
begin
  Result := '';
  if CellOwner is TsmxCustomGrid then
    Result := TsmxCustomGrid(CellOwner).GridCaptions[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
end;

procedure TsmxColumn.SetCaption(const Value: String);
begin
  if CellOwner is TsmxCustomGrid then
    TsmxCustomGrid(CellOwner).GridCaptions[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
end;

function TsmxColumn.GetColor: TColor;
begin
  Result := Column.Color;
end;

procedure TsmxColumn.SetColor(Value: TColor);
begin
  Column.Color := Value;
end;

function TsmxColumn.GetFont: TFont;
begin
  Result := Column.Font;
end;

procedure TsmxColumn.SetFont(Value: TFont);
begin
  Column.Font := Value;
end;

function TsmxColumn.GetValue: Variant;
begin
  Result := Variants.Null;
  if CellOwner is TsmxCustomGrid then
    Result := TsmxCustomGrid(CellOwner).GridValues[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex];
end;

procedure TsmxColumn.SetValue(const Value: Variant);
begin
  if CellOwner is TsmxCustomGrid then
    TsmxCustomGrid(CellOwner).GridValues[SlaveIndex, TsmxCustomGrid(CellOwner).FocusedRowIndex] := Value;
end;

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
  Column.Title.Color := Value;
end;

function TsmxColumn.GetHeaderFont: TFont;
begin
  Result := Column.Title.Font;
end;

procedure TsmxColumn.SetHeaderFont(Value: TFont);
begin
  Column.Title.Font := Value;
end;

function TsmxColumn.GetInternalRef: Pointer;
begin
  Result := Pointer(Column);
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
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TDBGrid then
      Column.Collection := TDBGrid(Obj).Columns;
  end;
end;

procedure TsmxColumn.SetOptions(Value: TsmxColumnOptions);
begin
  inherited SetOptions(Value);
  Column.ReadOnly := not (coEditing in Value);
end;

procedure TsmxColumn.ChangeObjectIndex(Value: Integer);
begin
  Column.Index := Value;
end;

procedure TsmxColumn.SetName(const NewName: TComponentName);
begin
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
  Result := -1;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
        Result := Request.DataSet.RecordNo;
end;

procedure TsmxDBGrid.SetFocusedRowIndex(Value: Integer);
begin
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
        Request.DataSet.RecordNo := Value;
end;

function TsmxDBGrid.GetGridCaption(ColIndex, RowIndex: Integer): String;
var
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  Result := '';
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      if Request.DataSet.Active then
      begin
        Bookmark := nil;
        if Assigned(DBGrid.DataSource.DataSet) then
        begin
          DBGrid.DataSource.DataSet.DisableControls;
          Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
        end;
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

          if Assigned(DBGrid.DataSource.DataSet) then
            DBGrid.DataSource.DataSet.GotoBookmark(Bookmark);
        finally
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
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  if coEditing in Slaves[ColIndex].Options then
    if Assigned(Request) then
      if Assigned(Request.DataSet) then
        if Request.DataSet.Active then
        begin
          Bookmark := nil;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.DisableControls;
            Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
          end;
          try
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
            if Assigned(DBGrid.DataSource.DataSet) then
            begin
              DBGrid.DataSource.DataSet.FreeBookmark(Bookmark);
              DBGrid.DataSource.DataSet.EnableControls;
            end;
          end;
        end;
end;

function TsmxDBGrid.GetGridValue(ColIndex, RowIndex: Integer): Variant;
var
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  Result := Variants.Null;
  if coHasValue in Slaves[ColIndex].Options then
    if Assigned(Request) then
      if Assigned(Request.DataSet) then
        if Request.DataSet.Active then
        begin
          Bookmark := nil;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.DisableControls;
            Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
          end;
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
            if Assigned(DBGrid.DataSource.DataSet) then
              DBGrid.DataSource.DataSet.GotoBookmark(Bookmark);
          finally
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
  Field: IsmxField;
  Bookmark: TBookmark;
begin
  if [coEditing, coHasValue] * Slaves[ColIndex].Options = [coEditing, coHasValue] then
    if Assigned(Request) then
      if Assigned(Request.DataSet) then
        if Request.DataSet.Active then
        begin
          Bookmark := nil;
          if Assigned(DBGrid.DataSource.DataSet) then
          begin
            DBGrid.DataSource.DataSet.DisableControls;
            Bookmark := DBGrid.DataSource.DataSet.GetBookmark;
          end;
          try
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
            if Assigned(DBGrid.DataSource.DataSet) then
            begin
              DBGrid.DataSource.DataSet.FreeBookmark(Bookmark);
              DBGrid.DataSource.DataSet.EnableControls;
            end;
          end;
        end;
end;

function TsmxDBGrid.GetHint: String;
begin
  Result := DBGrid.Hint;
end;

procedure TsmxDBGrid.SetHint(const Value: String);
begin
  DBGrid.Hint := Value;
end;

function TsmxDBGrid.GetDBGrid: TsmxWheelDBGrid;
begin
  if not Assigned(FDBGrid) then
  begin
    FDBGrid := TsmxWheelDBGrid.Create(nil);
    FDBGrid.OnDblClick := ControlDblClick;
    FDBGrid.OnTitleClick := DBGridTitleClick;
    FDBGrid.OnCellClick := DBGridCellClick;
    FDBGrid.DataSource := TDataSource.Create(nil);
    FDBGrid.DataSource.OnDataChange := DBGridDataChange;
  end;
  Result := FDBGrid;
end;

function TsmxDBGrid.GetInternalRef: Pointer;
begin
  Result := Pointer(DBGrid);
end;

function TsmxDBGrid.GetRowCount: Integer;
begin
  Result := 0;
  if Assigned(Request) then
    if Assigned(Request.DataSet) then
      Result := Request.DataSet.RecordCount;
end;

function TsmxDBGrid.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxColumn;
end;

procedure TsmxDBGrid.DBGridCellClick(Column: TColumn);
var
  Slave: TsmxOwnerCell;
begin
  Slave := FindSlaveByInternalRef(Pointer(Column));
  if Slave is TsmxCustomColumn then
    TsmxCustomColumn(Slave).Click;
end;

procedure TsmxDBGrid.DBGridDataChange(Sender: TObject; Field: TField);
begin
  ChangeRow;
end;

procedure TsmxDBGrid.DBGridTitleClick(Column: TColumn);
var
  Slave: TsmxOwnerCell;
begin
  Slave := FindSlaveByInternalRef(Pointer(Column));
  if Slave is TsmxCustomColumn then
    TsmxCustomColumn(Slave).ClickHeader;
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
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      DBGrid.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxDBGrid.SetOptions(Value: TsmxGridOptions);
begin
  inherited SetOptions(Value);
  if goColLines in Value then
    DBGrid.Options := DBGrid.Options + [dgColLines]
  else
    DBGrid.Options := DBGrid.Options - [dgColLines];
  if goRowLines in Value then
    DBGrid.Options := DBGrid.Options + [dgRowLines]
  else
    DBGrid.Options := DBGrid.Options - [dgRowLines];
  if goRowSelect in Value then
    DBGrid.Options := DBGrid.Options + [dgRowSelect]
  else
    DBGrid.Options := DBGrid.Options - [dgRowSelect];
  if goShowHeader in Value then
    DBGrid.Options := DBGrid.Options + [dgTitles, dgIndicator]
  else
    DBGrid.Options := DBGrid.Options - [dgTitles, dgIndicator];
  if goEditing in Value then
    DBGrid.Options := DBGrid.Options + [dgEditing]
  else
    DBGrid.Options := DBGrid.Options - [dgEditing];
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

{ TsmxEditFilter }

destructor TsmxEditFilter.Destroy;
begin
  if Assigned(FEdit) then
    FEdit.Free;
  inherited Destroy;
end;

procedure TsmxEditFilter.EditChange(Sender: TObject);
begin
  ChangeFilter;
end;

function TsmxEditFilter.GetText: String;
begin
  Result := String(Edit.Text);
end;

procedure TsmxEditFilter.SetText(const Value: String);
begin
  Edit.Text := TCaption(Value);
end;

function TsmxEditFilter.GetEnabled: Boolean;
begin
  Result := Edit.Enabled;
end;

procedure TsmxEditFilter.SetEnabled(Value: Boolean);
begin
  Edit.Enabled := Value;
end;

function TsmxEditFilter.GetHint: String;
begin
  Result := Edit.Hint;
end;

procedure TsmxEditFilter.SetHint(const Value: String);
begin
  Edit.Hint := Value;
end;

function TsmxEditFilter.GetEdit: TEdit;
begin
  if not Assigned(FEdit) then
  begin
    FEdit := TEdit.Create(nil);
    FEdit.Parent := Panel;
    FEdit.AutoSize := False;
    FEdit.Left := 4;
    FEdit.Top := 20;
    FEdit.Width := Panel.Width - 8;
    FEdit.Anchors := [akLeft, akRight];
    FEdit.OnChange := EditChange;
    FEdit.OnClick := ControlClick;
    FEdit.OnDblClick := ControlDblClick;
    FValue := Variants.Null;
  end;
  Result := FEdit;
end;

function TsmxEditFilter.GetColor: TColor;
begin
  Result := Edit.Color;
end;

procedure TsmxEditFilter.SetColor(Value: TColor);
begin
  Edit.Color := Value;
end;

function TsmxEditFilter.GetFont: TFont;
begin
  Result := Edit.Font;
end;

procedure TsmxEditFilter.SetFont(Value: TFont);
begin
  Edit.Font := Value;
end;

function TsmxEditFilter.GetValue: Variant;
begin
  if foHasValue in Options then
    Result := FValue
  else
    Result := Variants.Null;
end;

procedure TsmxEditFilter.SetValue(const Value: Variant);
begin
  if foHasValue in Options then
    FValue := Value;
end;

{ TsmxDateTimeFilter }

{constructor TsmxDateTimeFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FDateTime := TDateTimePicker.Create(Self);
  SetFormat;
  FDateTime.Parent := Panel;
  FDateTime.Width := Panel.Width - 8;
  FDateTime.Anchors := [akLeft, akRight];
  FDateTime.Left := 4;
  FDateTime.Top := 20;
  FDateTime.ShowCheckbox := True;
  FDateTime.Font.Color := Cfg.FilterFont.Color;
  FDateTime.Font.Name := Cfg.FilterFont.Name;
  FDateTime.Font.Size := Cfg.FilterFont.Size;
  FDateTime.Font.Style := Cfg.FilterFont.Style;
  FDateTime.DateTime := Date;
end;

destructor TsmxDateTimeFilter.Destroy;
begin
  FDateTime.Parent := nil;
  FDateTime.Free;
  inherited Destroy;
end;

function TsmxDateTimeFilter.GetCellEnabled: Boolean;
begin
  Result := FDateTime.Enabled;
end;

function TsmxDateTimeFilter.GetFilterValue: Variant;
begin
  Result := FDateTime.DateTime;
  if (Result = StrToDate('30.12.1899')) or not(FDateTime.Checked) then
    Result := Null else
  if Cfg.ValueFormat <> '' then
    Result := FormatDateTime(Cfg.ValueFormat, Result);
end;

function TsmxDateTimeFilter.GetInternalObject: TObject;
begin
  Result := FDateTime;
end;

procedure TsmxDateTimeFilter.SetCellEnabled(Value: Boolean);
begin
  FDateTime.Enabled := Value;
end;

procedure TsmxDateTimeFilter.SetFilterValue(Value: Variant);
var d: TDateTime;
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    d := StrToDate('30.12.1899') else
    d := StrToDateTimeDef(Value, StrToDate('30.12.1899'));
  if d = StrToDate('30.12.1899') then
  begin
    FDateTime.DateTime := Date;
    FDateTime.Checked := False;
  end else
  begin
    FDateTime.DateTime := d;
    FDateTime.Checked := True;
  end;
end;

procedure TsmxDateTimeFilter.SetFormat;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FDateTime.Parent := f;
    FDateTime.Format := Cfg.DisplayFormat;
    FDateTime.Parent := nil;
  finally
    f.Free;
  end;
end;}

{ TsmxBitBtnFilter }

destructor TsmxBitBtnFilter.Destroy;
begin
  if Assigned(FBitBtn) then
    FBitBtn.Free;
  inherited Destroy;
end;

function TsmxBitBtnFilter.GetEnabled: Boolean;
begin
  Result := BitBtn.Enabled;
end;

function TsmxBitBtnFilter.GetBitBtn: TBitBtn;
begin
  if not Assigned(FBitBtn) then
  begin
    FBitBtn := TBitBtn.Create(nil);
    FBitBtn.Parent := Panel;
    FBitBtn.Width := Panel.Width - 8;
    FBitBtn.Anchors := [akLeft, akRight];
    FBitBtn.Left := 4;
    FBitBtn.Top := 20;
    FBitBtn.OnClick := ControlClick;
    FValue := Variants.Null;
  end;
  Result := FBitBtn;
end;

function TsmxBitBtnFilter.GetText: String;
begin
  Result := String(BitBtn.Caption);
end;

function TsmxBitBtnFilter.GetHint: String;
begin
  Result := BitBtn.Hint;
end;

function TsmxBitBtnFilter.GetImageIndex: TImageIndex;
begin
  Result := FButtonImageIndex;
end;

procedure TsmxBitBtnFilter.SetImageIndex(Value: TImageIndex);
begin
  if FButtonImageIndex <> - 1 then
  begin
    BitBtn.Glyph := nil;
    BitBtn.Margin := -1;
  end;
  FButtonImageIndex := Value;
  if FButtonImageIndex <> - 1 then
    if Assigned(ImageList) then
    begin
      ImageList.GetBitmap(Integer(FButtonImageIndex), BitBtn.Glyph);
      BitBtn.Margin := 2;
    end;
end;

function TsmxBitBtnFilter.GetColor: TColor;
begin
  Result := Graphics.clBtnFace;
end;

function TsmxBitBtnFilter.GetFont: TFont;
begin
  Result := BitBtn.Font;
end;

procedure TsmxBitBtnFilter.SetText(const Value: String);
begin
  BitBtn.Caption := TCaption(Value);
end;

procedure TsmxBitBtnFilter.SetHint(const Value: String);
begin
  BitBtn.Hint := Value;
end;

procedure TsmxBitBtnFilter.SetColor(Value: TColor);
begin
end;

procedure TsmxBitBtnFilter.SetFont(Value: TFont);
begin
  BitBtn.Font := Value;
end;

function TsmxBitBtnFilter.GetValue: Variant;
begin
  if foHasValue in Options then
    Result := FValue
  else
    Result := Variants.Null;
end;

procedure TsmxBitBtnFilter.SetEnabled(Value: Boolean);
begin
  BitBtn.Enabled := Value;
end;

procedure TsmxBitBtnFilter.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
  begin
    BitBtn.Glyph := nil;
    BitBtn.Margin := -1;
  end;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    if FButtonImageIndex <> -1 then
    begin
      ImageList.GetBitmap(FButtonImageIndex, BitBtn.Glyph);
      BitBtn.Margin := 2;
    end;
end;

procedure TsmxBitBtnFilter.SetValue(const Value: Variant);
begin
  if foHasValue in Options then
  begin
    FValue := Value;
    ChangeFilter;
  end;  
end;

{ TsmxNumEditFilter }

{constructor TsmxNumEditFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Panel;
  FEdit.AutoSize := False;
  FEdit.Width := Panel.Width - 8;
  FEdit.Anchors := [akLeft, akRight];
  FEdit.Left := 4;
  FEdit.Top := 20;
  FEdit.OnKeyPress := ProcKeyPress;
  FEdit.Font.Color := Cfg.FilterFont.Color;
  FEdit.Font.Name := Cfg.FilterFont.Name;
  FEdit.Font.Size := Cfg.FilterFont.Size;
  FEdit.Font.Style := Cfg.FilterFont.Style;
  FEdit.Text := '';
end;

destructor TsmxNumEditFilter.Destroy;
begin
  FEdit.OnKeyPress := nil;
  FEdit.Parent := nil;
  FEdit.Free;
  inherited Destroy;
end;

function TsmxNumEditFilter.GetCellEnabled: Boolean;
begin
  Result := FEdit.Enabled;
end;

function TsmxNumEditFilter.GetFilterValue: Variant;
begin
  Result := FEdit.Text;
  if Result = '' then
    Result := Null else
  if Cfg.ValueFormat <> '' then
    Result := FormatFloat(Cfg.ValueFormat, Result);
end;

function TsmxNumEditFilter.GetInternalObject: TObject;
begin
  Result := FEdit;
end;

procedure TsmxNumEditFilter.ProcKeyPress(Sender: TObject; var Key: Char);

  function CountSep(s: String): Integer;
  var i: Integer;
  begin
    Result := 0;
    for i := 1 to Length(s) do
      if s[i] in ['.', ','] then
        Result := Result + 1;
  end;

var s: String;
begin
  s := FEdit.Text;
  if not(Key in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ',', #8]) then
    Key := #0;
  if (Key in ['.', ',']) and (CountSep(s) > 0) then
    Key := #0;
end;

procedure TsmxNumEditFilter.SetCellEnabled(Value: Boolean);
begin
  FEdit.Enabled := Value;
end;

procedure TsmxNumEditFilter.SetFilterValue(Value: Variant);
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    FEdit.Text := '' else
    FEdit.Text := FloatToStr(StrToFloatDef(Value, 0));
end;}

{ TsmxLabelFilter }

{constructor TsmxLabelFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FSLabel := TLabel.Create(Self);
  FSLabel.Parent := Panel;
  FSLabel.Width := Panel.Width - 8;
  FSLabel.Anchors := [akLeft, akRight];
  FSLabel.Left := 4;
  FSLabel.Top := 20;
  FSLabel.Font.Color := Cfg.FilterFont.Color;
  FSLabel.Font.Name := Cfg.FilterFont.Name;
  FSLabel.Font.Size := Cfg.FilterFont.Size;
  FSLabel.Font.Style := Cfg.FilterFont.Style;
  FValue := Null;
  FSLabel.Caption := '';
end;

destructor TsmxLabelFilter.Destroy;
begin
  FSLabel.Parent := nil;
  FSLabel.Free;
  inherited Destroy;
end;

function TsmxLabelFilter.GetFilterText: String;
begin
  Result := String(FSLabel.Caption);
end;

function TsmxLabelFilter.GetFilterValue: Variant;
begin
  Result := FValue;
end;

function TsmxLabelFilter.GetInternalObject: TObject;
begin
  Result := FSLabel;
end;

procedure TsmxLabelFilter.SetFilterText(Value: String);
begin
  FSLabel.Caption := TCaption(Value);
end;

procedure TsmxLabelFilter.SetFilterValue(Value: Variant);
begin
  FValue := Value;
end;}

{ TsmxPanelFilterDesk }

destructor TsmxPanelFilterDesk.Destroy;
begin
  inherited Destroy;
  if Assigned(FPanel) then
    FPanel.Free;
end;

procedure TsmxPanelFilterDesk.InternalApply;
var
  DataSet: IsmxDataSet;
  OldIsManualRefreshParams: Boolean;
  i: Integer;
begin
  inherited InternalApply;
  if Assigned(Request) then
  begin
    DataSet := Request.UpdateDataSet;
    if Assigned(DataSet) then
    begin
      OldIsManualRefreshParams := Request.IsManualRefreshParams;
      try
        Request.IsManualRefreshParams := True;
        RefreshValueParams(DataSet);
        for i := 0 to SlaveCount - 1 do
        begin
          if foApply in Slaves[i].Options then
          begin
            DataSet.ParamByName(Slaves[i].Name).Value :=
              Slaves[i].Value;
            DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value :=
              smxFuncs.StrToVar(Slaves[i].Text);
          end;
        end;
        Request.Update;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

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
            if foPrepare in Slaves[i].Options then
              case DataSet.PerformanceMode of
                pmOpen:
                begin
                  Slaves[i].Value :=
                    DataSet.FieldByName(Slaves[i].Name).Value;
                  Slaves[i].Text :=
                    Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                end;
                pmExecute:
                begin
                  Slaves[i].Value :=
                    DataSet.ParamByName(Slaves[i].Name).Value;
                  Slaves[i].Text :=
                    Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
                end;
              end;
          end;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

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
          if foPrepare in Slaves[i].Options then
            case DataSet.PerformanceMode of
              pmOpen:
              begin
                Slaves[i].Value :=
                  DataSet.FieldByName(Slaves[i].Name).Value;
                Slaves[i].Text :=
                  Variants.VarToStr(DataSet.FieldByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
              end;
              pmExecute:
              begin
                Slaves[i].Value :=
                  DataSet.ParamByName(Slaves[i].Name).Value;
                Slaves[i].Text :=
                  Variants.VarToStr(DataSet.ParamByName(smxFuncs.GetTextFieldName(Slaves[i].Name)).Value);
              end;
            end;
        end;
      finally
        Request.IsManualRefreshParams := OldIsManualRefreshParams;
      end;
    end;
  end;
end;

function TsmxPanelFilterDesk.GetCaption: TCaption;
begin
  Result := Panel.Caption;
end;

procedure TsmxPanelFilterDesk.SetCaption(const Value: TCaption);
begin
  Panel.Caption := Value;
end;

function TsmxPanelFilterDesk.GetHint: String;
begin
  Result := Panel.Hint;
end;

procedure TsmxPanelFilterDesk.SetHint(const Value: String);
begin
  Panel.Hint := Value;
end;

function TsmxPanelFilterDesk.GetInternalRef: Pointer;
begin
  Result := Pointer(Panel);
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

function TsmxPanelFilterDesk.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxFilter;
end;

procedure TsmxPanelFilterDesk.RefreshValueParams(DataSet: IsmxDataSet);
var
  i, j: Integer;
  List: TList;
  Value: Variant;
begin
  List := TList.Create;
  try
    for i := 0 to DataSet.ParamCount - 1 do
      case DataSet.Params[i].DataLocation of
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            DataSet.Params[i].Value := smxProcs.gStorageManagerIntf.Values[DataSet.Params[i].ParamName];
        end;
        dlParentCellParam:
        begin
          List.Clear;
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

procedure TsmxPanelFilterDesk.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

{ TsmxPanelSection }

destructor TsmxPanelSection.Destroy;
begin
  inherited Destroy;
  if Assigned(FPanel) then
    FPanel.Free;
end;

function TsmxPanelSection.GetInternalRef: Pointer;
begin
  Result := Pointer(Panel);
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

procedure TsmxPanelSection.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Panel.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Panel.Parent := TWinControl(Obj);
  end;
end;

{ TsmxTabSheet }

destructor TsmxTabSheet.Destroy;
begin
  inherited Destroy;
  if Assigned(FTabSheet) then
    FTabSheet.Free;
end;

function TsmxTabSheet.GetCaption: TCaption;
begin
  Result := TabSheet.Caption;
end;

procedure TsmxTabSheet.SetCaption(const Value: TCaption);
var
  PageControl: TPageControl;
  Form: TForm;
begin
  if GetCaption <> Value then
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
      TabSheet.Caption := Value;
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

function TsmxTabSheet.GetHint: String;
begin
  Result := TabSheet.Hint;
end;

procedure TsmxTabSheet.SetHint(const Value: String);
begin
  TabSheet.Hint := Value;
end;

function TsmxTabSheet.GetImageIndex: TImageIndex;
begin
  Result := TabSheet.ImageIndex;
end;

procedure TsmxTabSheet.SetImageIndex(Value: TImageIndex);
var
  PageControl: TPageControl;
  Form: TForm;
begin
  if GetImageIndex <> Value then
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
      TabSheet.ImageIndex := Value;
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

function TsmxTabSheet.GetVisible: Boolean;
begin
  Result := TabSheet.TabVisible;
end;

procedure TsmxTabSheet.SetVisible(Value: Boolean);
var
  PageControl: TPageControl;
  Form: TForm;
  RootControl: TWinControl;
begin
  if GetVisible <> Value then
  begin
    PageControl := TabSheet.PageControl;
    RootControl := smxFuncs.GetRootControl(PageControl);
    Form := nil;
    try
      if Assigned(PageControl) and not (RootControl is TCustomForm) then
      begin
        Form := TForm.Create(nil);
        RootControl.Parent := Form;
      end;
      TabSheet.PageControl := nil;
      TabSheet.TabVisible := Value;
      TabSheet.PageControl := PageControl;
    finally
      if Assigned(Form) then
      begin
        RootControl.Parent := nil;
        Form.Free;
      end;
    end;
  end;
end;

function TsmxTabSheet.GetInternalRef: Pointer;
begin
  Result := Pointer(TabSheet);
end;

function TsmxTabSheet.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxPanelSection;
end;

function TsmxTabSheet.GetTabSheet: TTabSheet;
begin
  if not Assigned(FTabSheet) then
  begin
    FTabSheet := TTabSheet.Create(nil);
    FTabSheet.ImageIndex := -1;
  end;
  Result := FTabSheet;
end;

procedure TsmxTabSheet.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
  Form: TForm;
  PageControl: TPageControl;
  RootControl: TWinControl;
begin   
  if Assigned(CellParent) then
  begin
    PageControl := TabSheet.PageControl;
    RootControl := smxFuncs.GetRootControl(PageControl);
    Form := nil;
    try
      if Assigned(PageControl) and not (RootControl is TCustomForm) then
      begin
        Form := TForm.Create(nil);
        RootControl.Parent := Form;
      end;
      TabSheet.PageControl := nil;
    finally
      if Assigned(Form) then
      begin
        RootControl.Parent := nil;
        Form.Free;
      end;
    end;
  end;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TPageControl then
    begin
      PageControl := TPageControl(Obj);
      RootControl := smxFuncs.GetRootControl(PageControl);
      Form := nil;
      try
        if Assigned(PageControl) and not (RootControl is TCustomForm) then
        begin
          Form := TForm.Create(nil);
          RootControl.Parent := Form;
        end;
        TabSheet.PageControl := PageControl;
      finally
        if Assigned(Form) then
        begin
          RootControl.Parent := nil;
          Form.Free;
        end;
      end;
    end;
  end;
end;

procedure TsmxTabSheet.ChangeObjectIndex(Value: Integer);
begin
  TabSheet.PageIndex := Value;
end;

{ TsmxPageControl }

destructor TsmxPageControl.Destroy;
begin
  inherited Destroy;
  if Assigned(FPageControl) then
    FPageControl.Free;
end;

function TsmxPageControl.GetActivePageIndex: Integer;
begin
  Result := PageControl.ActivePageIndex;
end;

procedure TsmxPageControl.SetActivePageIndex(Value: Integer);
var
  Form: TForm;
begin
  if GetActivePageIndex <> Value then
  begin
    Form := nil;
    try
      if not Assigned(PageControl.Parent) then
      begin
        Form := TForm.Create(nil);
        PageControl.Parent := Form;
      end;
      PageControl.ActivePageIndex := Value;
    finally
      if Assigned(Form) then
      begin
        PageControl.Parent := nil;
        Form.Free;
      end;
    end;
  end;
end;

function TsmxPageControl.GetInternalRef: Pointer;
begin
  Result := Pointer(PageControl);
end;

function TsmxPageControl.GetIsMultiLine: Boolean;
begin
  Result := PageControl.MultiLine;
end;

procedure TsmxPageControl.SetIsMultiLine(Value: Boolean);
begin
  PageControl.MultiLine := Value;
end;

function TsmxPageControl.GetStyle: TsmxPageManagerStyle;
const
  InOutStyle: array[TTabStyle] of TsmxPageManagerStyle =
    (pmsTab, pmsTab, pmsFlat);
begin
  Result := InOutStyle[PageControl.Style];
end;

procedure TsmxPageControl.SetStyle(Value: TsmxPageManagerStyle);
const
  OutInStyle: array[TsmxPageManagerStyle] of TTabStyle =
    (tsTabs, tsFlatButtons);
begin
  PageControl.Style := OutInStyle[Value];
end;

function TsmxPageControl.GetPagePosition: TsmxPagePosition;
begin
  Result := TsmxPagePosition(PageControl.TabPosition);
end;

procedure TsmxPageControl.SetPagePosition(Value: TsmxPagePosition);
begin
  PageControl.TabPosition := TTabPosition(Value);
end;

function TsmxPageControl.GetPageControl: TPageControl;
begin
  if not Assigned(FPageControl) then
  begin
    FPageControl := TPageControl.Create(nil);
    FPageControl.OnChange := PageControlChange;
  end;
  Result := FPageControl;
end;

function TsmxPageControl.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxTabSheet;
end;

procedure TsmxPageControl.PageControlChange(Sender: TObject);
begin
  ChangePage;
end;

procedure TsmxPageControl.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    PageControl.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      PageControl.Parent := TWinControl(Obj);
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

{ TsmxMenuItem }

destructor TsmxMenuItem.Destroy;
begin
  inherited Destroy;
  if Assigned(FMenuItem) then
    FMenuItem.Free;
end;

function TsmxMenuItem.GetCaption: TCaption;
begin
  Result := TCaption(MenuItem.Caption);
end;

procedure TsmxMenuItem.SetCaption(const Value: TCaption);
begin
  MenuItem.Caption := String(Value);
end;

function TsmxMenuItem.GetEnabled: Boolean;
begin
  Result := MenuItem.Enabled;
end;

procedure TsmxMenuItem.SetEnabled(Value: Boolean);
begin
  MenuItem.Enabled := Value;
end;

function TsmxMenuItem.GetHint: String;
begin
  Result := MenuItem.Hint;
end;

procedure TsmxMenuItem.SetHint(const Value: String);
begin
  MenuItem.Hint := Value;
end;

function TsmxMenuItem.GetImageIndex: TImageIndex;
begin
  Result := MenuItem.ImageIndex;
end;

procedure TsmxMenuItem.SetImageIndex(Value: TImageIndex);
begin
  MenuItem.ImageIndex := Value;
end;

function TsmxMenuItem.GetVisible: Boolean;
begin
  Result := MenuItem.Visible;
end;

procedure TsmxMenuItem.SetVisible(Value: Boolean);
begin
  MenuItem.Visible := Value;
end;

function TsmxMenuItem.GetInternalRef: Pointer;
begin
  Result := Pointer(MenuItem);
end;

function TsmxMenuItem.GetIsChecked: Boolean;
begin
  Result := MenuItem.Checked;
end;

procedure TsmxMenuItem.SetIsChecked(Value: Boolean);
begin
  MenuItem.Checked := Value;
end;

function TsmxMenuItem.GetHotKey: Integer;
begin
  Result := Integer(MenuItem.ShortCut);
end;

procedure TsmxMenuItem.SetHotKey(Value: Integer);
begin
  MenuItem.ShortCut := TShortCut(Value);
end;

function TsmxMenuItem.GetStyle: TsmxMenuItemStyle;
begin
  if MenuItem.Caption = '-' then
    Result := misDivider
  else
    Result := misPoint;
end;

procedure TsmxMenuItem.SetStyle(Value: TsmxMenuItemStyle);
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

function TsmxMenuItem.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxMenuItem;
end;

procedure TsmxMenuItem.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TMenuItem then
      TMenuItem(Obj).Remove(MenuItem) else
    if Obj is TMainMenu then
      TMainMenu(Obj).Items.Remove(MenuItem);
  end;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TMenuItem then
      TMenuItem(Obj).Add(MenuItem) else
    if Obj is TMainMenu then
      TMainMenu(Obj).Items.Add(MenuItem);
  end;
end;

procedure TsmxMenuItem.ChangeObjectIndex(Value: Integer);
begin
  MenuItem.MenuIndex := Value;
end;

{ TsmxMainMenu }

destructor TsmxMainMenu.Destroy;
begin
  inherited Destroy;
  if Assigned(FMainMenu) then
    FMainMenu.Free;
end;

function TsmxMainMenu.GetEnabled: Boolean;
begin
  Result := MainMenu.Items.Enabled;
end;

procedure TsmxMainMenu.SetEnabled(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to MainMenu.Items.Count - 1 do
    MainMenu.Items[i].Enabled := Value;
end;

function TsmxMainMenu.GetVisible: Boolean;
begin
  Result := MainMenu.Items.Visible;
end;

procedure TsmxMainMenu.SetVisible(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to MainMenu.Items.Count - 1 do
    MainMenu.Items[i].Visible := Value;
end;

function TsmxMainMenu.GetInternalRef: Pointer;
begin
  Result := Pointer(MainMenu);
end;

function TsmxMainMenu.GetMainMenu: TMainMenu;
begin
  if not Assigned(FMainMenu) then
    FMainMenu := TMainMenu.Create(nil);
  Result := FMainMenu;
end;

function TsmxMainMenu.GetSlaveClass: TsmxBaseCellClass;
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
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
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

{ TsmxPopupMenu }

destructor TsmxPopupMenu.Destroy;
begin
  inherited Destroy;
  if Assigned(FPopupMenu) then
    FPopupMenu.Free;
end;

function TsmxPopupMenu.GetInternalRef: Pointer;
begin
  Result := Pointer(PopupMenu);
end;

function TsmxPopupMenu.GetPopupMenu: TPopupMenu;
begin
  if not Assigned(FPopupMenu) then
    FPopupMenu := TPopupMenu.Create(nil);
  Result := FPopupMenu;
end;

function TsmxPopupMenu.GetSlaveClass: TsmxBaseCellClass;
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

function TsmxPopupList.GetSlaveClass: TsmxBaseCellClass;
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

function TsmxToolItem.GetCaption: TCaption;
begin
  Result := ToolButton.Caption;
end;

procedure TsmxToolItem.SetCaption(const Value: TCaption);
begin
  ToolButton.Caption := Value;
end;

function TsmxToolItem.GetHint: String;
begin
  Result := ToolButton.Hint;
end;

procedure TsmxToolItem.SetHint(const Value: String);
begin
  ToolButton.Hint := Value;
  ToolButton.ShowHint := Value <> '';
end;

function TsmxToolItem.GetImageIndex: TImageIndex;
begin
  Result := ToolButton.ImageIndex;
end;

procedure TsmxToolItem.SetImageIndex(Value: TImageIndex);
begin
  ToolButton.ImageIndex := Value;
end;

function TsmxToolItem.GetInternalRef: Pointer;
begin
  Result := Pointer(ToolButton);
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
  begin
    FToolButton := TToolButton.Create(nil);
    FToolButton.OnClick := ControlClick;
  end;
  Result := FToolButton;
end;

function TsmxToolItem.GetStyle: TsmxToolItemStyle;
const
  InOutStyle: array[TToolButtonStyle] of TsmxToolItemStyle =
    (tisButton, tisCheck, tisButton, tisDivider, tisDivider);
begin
  Result := InOutStyle[ToolButton.Style];
end;

procedure TsmxToolItem.SetStyle(Value: TsmxToolItemStyle);
const
  OutInStyle: array[TsmxToolItemStyle] of TToolButtonStyle =
    (tbsButton, tbsCheck, tbsDivider);
begin
  ToolButton.Style := OutInStyle[Value];
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
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
    begin
      if Obj is TToolBar then
        if TToolBar(Obj).ButtonCount > 0 then
          ToolButton.Left := TToolBar(Obj).Buttons[TToolBar(Obj).ButtonCount - 1].Left + 1;
      ToolButton.Parent := TWinControl(Obj);
    end;
  end;
end;

{ TsmxToolBar }

destructor TsmxToolBar.Destroy;
begin
  inherited Destroy;
  if Assigned(FToolBar) then
    FToolBar.Free;
end;

function TsmxToolBar.GetInternalRef: Pointer;
begin
  Result := Pointer(ToolBar);
end;

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

function TsmxToolBar.GetSlaveClass: TsmxBaseCellClass;
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

procedure TsmxToolBar.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    ToolBar.Parent := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      ToolBar.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxToolBar.SetImageList(Value: TCustomImageList);
var
  Control: TWinControl;
begin
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

{ TsmxControlBar }

destructor TsmxControlBar.Destroy;
begin
  inherited Destroy;
  if Assigned(FControlBar) then
    FControlBar.Free;
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

function TsmxControlBar.GetInternalRef: Pointer;
begin
  Result := Pointer(ControlBar);
end;

function TsmxControlBar.GetSlaveClass: TsmxBaseCellClass;
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
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
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

function TsmxStatusPanel.GetCaption: TCaption;
begin
  Result := TCaption(StatusPanel.Text);
end;

procedure TsmxStatusPanel.SetCaption(const Value: TCaption);
begin
  StatusPanel.Text := String(Value);
end;

function TsmxStatusPanel.GetWidth: Integer;
begin
  Result := StatusPanel.Width;
end;

procedure TsmxStatusPanel.SetWidth(Value: Integer);
begin
  StatusPanel.Width := Value;
end;

function TsmxStatusPanel.GetAlignment: TAlignment;
begin
  Result := StatusPanel.Alignment;
end;

procedure TsmxStatusPanel.SetAlignment(Value: TAlignment);
begin
  StatusPanel.Alignment := Value;
end;

function TsmxStatusPanel.GetStyle: TsmxStatusItemStyle;
const
  InOutStyle: array[TStatusPanelStyle] of TsmxStatusItemStyle =
    (sisText, sisDraw);
begin
  Result := InOutStyle[StatusPanel.Style];
end;

procedure TsmxStatusPanel.SetStyle(Value: TsmxStatusItemStyle);
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

procedure TsmxStatusPanel.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    StatusPanel.Collection := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TStatusPanels then
      StatusPanel.Collection := TStatusPanels(Obj);
  end;
end;

procedure TsmxStatusPanel.ChangeObjectIndex(Value: Integer);
begin
  StatusPanel.Index := Value;
end;

{ TsmxStatusBar }

destructor TsmxStatusBar.Destroy;
begin
  inherited Destroy;
  if Assigned(FStatusBar) then
    FStatusBar.Free;
end;

function TsmxStatusBar.GetSlaveClass: TsmxBaseCellClass;
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
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      StatusBar.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxStatusBar.StatusBarDraw(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
var
  Slave: TsmxOwnerCell;
begin
  Slave := FindSlaveByInternalRef(Pointer(Panel));
  if Slave is TsmxCustomStatusItem then
    TsmxCustomStatusItem(Slave).DrawPanel;
end;

{ TsmxStateForm }

destructor TsmxStateForm.Destroy;
begin
  if Assigned(FStateCfg) then
    FStateCfg.Free;  
  inherited Destroy;
end;

procedure TsmxStateForm.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateForm then
    StateID := TsmxStateForm(Source).StateID;
end;

function TsmxStateForm.CellParams(const Name: String; var Value: Variant): Boolean;
begin
  if SysUtils.AnsiCompareText(Name, 'StateID') = 0 then
  begin
    Value := FStateID;
    Result := True;
  end else
    Result := inherited CellParams(Name, Value);
end;

function TsmxStateForm.GetStateCfg: TsmxStateCfg;
begin
  if not Assigned(FStateCfg) then
    FStateCfg := GetStateCfgClass.Create(Self);
  Result := TsmxStateCfg(FStateCfg);
end;

function TsmxStateForm.GetStateCfgClass: TsmxBaseCfgClass;
begin
  Result := TsmxStateCfg;
end;

procedure TsmxStateForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = StateRequest) and (Operation = opRemove) then
    StateRequest := nil;
end;

procedure TsmxStateForm.RefreshStateCfg;
begin
  if Assigned(StateCfg.SelectDataSet) and (StateCfg.CfgID <> 0)
      and (StateCfg.IntfID <> 0) then
  begin
    StateCfg.Load;
    StateCfg.Read;
  end else
  begin
    StateCfg.ClearCfg;
  end;
end;

procedure TsmxStateForm.RefreshStateID;
begin
end;

procedure TsmxStateForm.SetCfgID(Value: Integer);
begin
  if CfgID <> Value then
  begin
    inherited SetCfgID(Value);
    StateCfg.CfgID := Value;
    RefreshStateCfg;
    RefreshStateID;
  end;
end;

procedure TsmxStateForm.SetIntfID(Value: Integer);
begin
  if IntfID <> Value then
  begin
    inherited SetIntfID(Value);
    StateCfg.IntfID := Value;
    RefreshStateCfg;
    RefreshStateID;
  end;
end;

procedure TsmxStateForm.SetStateID(Value: Integer);
begin
  if FStateID <> Value then
  begin
    FStateID := Value;
    RefreshStateID;
  end;
end;

procedure TsmxStateForm.SetStateRequest(Value: TsmxCustomRequest);
begin
  if FStateRequest <> Value then
  begin
    FStateRequest := Value;
    StateCfg.SelectDataSet := Value.DataSet;
    StateCfg.DeleteDataSet := Value.DeleteDataSet;
    StateCfg.InsertDataSet := Value.InsertDataSet;
    StateCfg.UpdateDataSet := Value.UpdateDataSet;
    RefreshStateCfg;
    RefreshStateID;
    if Assigned(FStateRequest) then
      FStateRequest.FreeNotification(Self);
  end;
end;

{ TsmxStandardForm }

function TsmxStandardForm.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxPageControl;
end;

procedure TsmxStandardForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ControlBoard then
      ControlBoard := nil else
    if AComponent = MainMenu then
      MainMenu := nil else
    if AComponent = StatusBoard then
      StatusBoard := nil;
  end;
end;

procedure TsmxStandardForm.SetControlBoard(Value: TsmxCustomControlBoard);
begin
  FControlBoard := Value;
end;

procedure TsmxStandardForm.SetMainMenu(Value: TsmxCustomMainMenu);
begin
  FMainMenu := Value;
end;

procedure TsmxStandardForm.SetStatusBoard(Value: TsmxCustomStatusBoard);
begin
  FStatusBoard := Value;
end;

initialization
  Classes.RegisterClasses([TsmxLibAction, TsmxActionList, TsmxRequestList,
    TsmxColumn, TsmxDBGrid, TsmxEditFilter, TsmxBitBtnFilter,
    TsmxPanelFilterDesk, TsmxPanelSection, TsmxTabSheet, TsmxPageControl,
    TsmxMenuItem, TsmxMainMenu, TsmxPopupMenu, TsmxPopupList, TsmxToolItem,
    TsmxToolBar, TsmxControlBar, TsmxStatusPanel, TsmxStatusBar,
    TsmxStandardForm]);

finalization
  Classes.UnRegisterClasses([TsmxLibAction, TsmxActionList, TsmxRequestList,
    TsmxColumn, TsmxDBGrid, TsmxEditFilter, TsmxBitBtnFilter,
    TsmxPanelFilterDesk, TsmxPanelSection, TsmxTabSheet, TsmxPageControl,
    TsmxMenuItem, TsmxMainMenu, TsmxPopupMenu, TsmxPopupList, TsmxToolItem,
    TsmxToolBar, TsmxControlBar, TsmxStatusPanel, TsmxStatusBar,
    TsmxStandardForm]);

end.
