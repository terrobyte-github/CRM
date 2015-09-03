{**************************************}
{                                      }
{            SalesMan v1.0             }
{         Preform cell classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxCells;

interface

uses
  Classes, Controls, Forms, ExtCtrls, StdCtrls, ActnList, ImgList, Graphics,
  smxClasses, smxDBIntf, smxTypes;

type
  { TsmxAction }

  TsmxAction = class(TsmxCustomAlgorithm)
  private
    FAction: TAction;
    function GetAction: TAction;
    procedure ActionExecute(Sender: TObject);
  protected
    function GetAlgorithmCaption: TCaption; override;
    function GetAlgorithmEnabled: Boolean; override;
    function GetAlgorithmHint: String; override;
    function GetAlgorithmHotKey: TShortCut; override;
    function GetAlgorithmImageIndex: TImageIndex; override;
    function GetAlgorithmVisible: Boolean; override;
    function GetInternalRef: Pointer; override;
    procedure InternalRefreshParams; override;
    procedure SetAlgorithmCaption(const Value: TCaption); override;
    procedure SetAlgorithmEnabled(Value: Boolean); override;
    procedure SetAlgorithmHint(const Value: String); override;
    procedure SetAlgorithmHotKey(Value: TShortCut); override;
    procedure SetAlgorithmImageIndex(Value: TImageIndex); override;
    procedure SetAlgorithmVisible(Value: Boolean); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure ChangeObjectIndex(Value: Integer); override;

    property Action: TAction read GetAction;
  public
    destructor Destroy; override;
  published
    property AlgorithmCaption;
    property AlgorithmEnabled;
    property AlgorithmHint;
    property AlgorithmHotKey;
    property AlgorithmImageIndex;
    property AlgorithmParams;
    property AlgorithmVisible;

    property OnRefreshParams;
  end;

  { TsmxRequest }

  TsmxRequest = class(TsmxCustomRequest)
  protected
    procedure InternalRefreshParams; override;
  published
    property DatabaseName;
    property DataSet;
    property DeleteDataSet;
    property InsertDataSet;
    property OperationMode;
    property UpdateDataSet;

    property OnDelete;
    property OnExecute;
    property OnInsert;
    property OnPrepare;
    property OnRefreshParams;
    property OnUpdate;
  end;

  { TsmxFilter }

  TsmxFilter = class(TsmxCustomFilter)
  private
    FHeader: TLabel;
    FPanel: TPanel;
    function GetHeader: TLabel;
    function GetPanel: TPanel;
  protected
    function GetHeaderAlignment: TAlignment; override;
    function GetCellCaption: TCaption; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetInternalRef: Pointer; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetCellCaption(const Value: TCaption); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;

    property Header: TLabel read GetHeader;
    property Panel: TPanel read GetPanel;
  public
    destructor Destroy; override;
  published
    property CellAlign;
    property CellAnchors;
    property CellEnabled;
    property CellHeight;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property HeaderAlignment;
    property HeaderCaption;
    property HeaderColor;
    property HeaderFont;
    property PopupMenu;
  end;

  { TsmxForm }

  TsmxForm = class(TsmxCustomForm)
  private
    FForm: TForm;
    FFormImageIndex: TImageIndex;
    FIsMainForm: Boolean;
    function GetForm: TForm;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
  protected
    function GetCellActive: Boolean; override;
    function GetCellCaption: TCaption; override;
    function GetCellHint: String; override;
    function GetCellImageIndex: TImageIndex; override;
    function GetFormBorder: TsmxFormBorder; override;
    function GetFormPosition: TsmxFormPosition; override;
    function GetInternalRef: Pointer; override;
    function GetIsMaximize: Boolean; override;
    function GetModalResult: TModalResult; override;
    procedure InternalClose; override;
    procedure InternalShow; override;
    function InternalShowModal: TModalResult; override;
    procedure SetCellActive(Value: Boolean); override;
    procedure SetCellCaption(const Value: TCaption); override;
    procedure SetCellHint(const Value: String); override;
    procedure SetCellImageIndex(Value: TImageIndex); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetFormBorder(Value: TsmxFormBorder); override;
    procedure SetFormOptions(Value: TsmxFormOptions); override;
    procedure SetFormPosition(Value: TsmxFormPosition); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetIsDesigning(Value: Boolean); override;
    procedure SetIsMaximize(Value: Boolean); override;
    procedure SetModalResult(Value: TModalResult); override;
    procedure SetPopupMenu(Value: TsmxCustomPopupMenu); override;

    property Form: TForm read GetForm;
    property IsMainForm: Boolean read FIsMainForm;
  public
    destructor Destroy; override;
  published
    property CellAlign;
    property CellAnchors;
    property CellCaption;
    property CellCursor;
    property CellEnabled;
    property CellHeight;
    property CellHint;
    property CellImageIndex;
    property CellLeft;
    property CellTop;
    property CellVisible;
    property CellWidth;
    property FormBorder;
    property FormOptions;
    property FormPosition;
    property ImageListName;
    property IsMaximize;
    property PopupMenu;

    property OnActivate;
    property OnClose;
    property OnDeactivate;
    property OnDoubleClick;
    property OnShow;
    property OnClick;
  end;

implementation

uses
  Variants, Windows, Menus, Messages, smxFuncs, smxProcs, smxClassFuncs,
  smxClassProcs, smxBaseIntf;

{ TsmxAction }

destructor TsmxAction.Destroy;
begin
  inherited Destroy;
  if Assigned(FAction) then
    FAction.Free;
end;

procedure TsmxAction.ActionExecute(Sender: TObject);
begin
  Execute;
end;

function TsmxAction.GetAction: TAction;
begin
  if not Assigned(FAction) then
  begin
    FAction := TAction.Create(nil);
    FAction.OnExecute := ActionExecute;
  end;
  Result := FAction;
end;

function TsmxAction.GetAlgorithmCaption: TCaption;
begin
  Result := TCaption(Action.Caption);
end;

procedure TsmxAction.SetAlgorithmCaption(const Value: TCaption);
begin
  Action.Caption := String(Value);
end;

function TsmxAction.GetAlgorithmEnabled: Boolean;
begin
  Result := Action.Enabled;
end;

procedure TsmxAction.SetAlgorithmEnabled(Value: Boolean);
begin
  Action.Enabled := Value;
end;

function TsmxAction.GetAlgorithmHint: String;
begin
  Result := Action.Hint;
end;

procedure TsmxAction.SetAlgorithmHint(const Value: String);
begin
  Action.Hint := Value;
end;

function TsmxAction.GetAlgorithmHotKey: TShortCut;
begin
  Result := Action.ShortCut;
end;

procedure TsmxAction.SetAlgorithmHotKey(Value: TShortCut);
begin
  Action.ShortCut := Value;
end;

function TsmxAction.GetAlgorithmImageIndex: TImageIndex;
begin
  Result := Action.ImageIndex;
end;

procedure TsmxAction.SetAlgorithmImageIndex(Value: TImageIndex);
begin
  Action.ImageIndex := Value;
end;

function TsmxAction.GetAlgorithmVisible: Boolean;
begin
  Result := Action.Visible;
end;

procedure TsmxAction.SetAlgorithmVisible(Value: Boolean);
begin
  Action.Visible := Value;
end;

function TsmxAction.GetInternalRef: Pointer;
begin
  Result := Pointer(Action);
end;

procedure TsmxAction.InternalRefreshParams;
var
  i, j: Integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
  Cell: TsmxOwnerCell;
begin
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to AlgorithmParams.Count - 1 do
    begin
      case AlgorithmParams[i].DataLocation of
        dlFilterDesk:
        begin
          if CellAction is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellAction).FindSlaveByName(AlgorithmParams[i].ParamName);
            if Assigned(Cell) then
              AlgorithmParams[i].ParamValue := TsmxCustomFilter(Cell).FilterValue;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, AlgorithmParams[i].ParamName, Value) then
            AlgorithmParams[i].ParamValue := Value;
        end;
        dlGrid:
        begin
          if CellAction is TsmxCustomGrid then
          begin
            Cell := TsmxCustomGrid(CellAction).FindSlaveByName(AlgorithmParams[i].ParamName);
            if Assigned(Cell) then
              AlgorithmParams[i].ParamValue := TsmxCustomColumn(Cell).ColumnValue;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, AlgorithmParams[i].ParamName, Value) then
            AlgorithmParams[i].ParamValue := Value;
        end;
        dlParentFilterDesk:
        begin
          List.Clear;
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
        dlParentGrid:
        begin
          List.Clear;
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            AlgorithmParams[i].ParamValue := smxProcs.gStorageManagerIntf.Values[AlgorithmParams[i].ParamName];
        end;
        dlParentCellParam:
        begin
          List.Clear;
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(AlgorithmParams[i].ParamName, Value) then
            begin
              AlgorithmParams[i].ParamValue := Value;
              Break;
            end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TsmxAction.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if Assigned(CellParent) then
    Action.ActionList := nil;
  inherited SetCellParent(Value);
  if Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TCustomActionList then
      Action.ActionList := TCustomActionList(Obj);
  end;
end;

procedure TsmxAction.ChangeObjectIndex(Value: Integer);
begin
  Action.Index := Value;
end;

{ TsmxRequest }

procedure TsmxRequest.InternalRefreshParams;
var
  i, j: integer;
  Form: TsmxCustomForm;
  Value: Variant;
  List: TList;
  Cell: TsmxOwnerCell;
begin
  inherited InternalRefreshParams;
  if not Assigned(CurDataSet) then
    Exit;
  Form := smxClassFuncs.GetAccessoryForm(Self);
  List := TList.Create;
  try
    for i := 0 to CurDataSet.ParamCount - 1 do
    begin
      case CurDataSet.Params[i].DataLocation of
        dlFilterDesk:
        begin
          if CellRequest is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellRequest).FindSlaveByName(CurDataSet.Params[i].ParamName);
            if Assigned(Cell) then
              CurDataSet.Params[i].Value := TsmxCustomFilter(Cell).FilterValue;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, CurDataSet.Params[i].ParamName, Value) then
            CurDataSet.Params[i].Value := Value;
        end;
        dlGrid:
        begin
          if CellRequest is TsmxCustomGrid then
          begin
            if CellRequest.CellParent is TsmxCustomSection then
              if CellRequest.CellParent.CellParent is TsmxCustomPage then
                with TsmxCustomPage(CellRequest.CellParent.CellParent) do
                  for j := 0 to SlaveCount - 1 do
                    if CellRequest.CellParent <> Slaves[j] then
                      if smxClassFuncs.FindColumnOnSection(Slaves[j], CurDataSet.Params[i].ParamName, Value) then
                      begin
                        CurDataSet.Params[i].Value := Value;
                        Break;
                      end;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, CurDataSet.Params[i].ParamName, Value) then
            CurDataSet.Params[i].Value := Value;
        end;
        dlParentFilterDesk:
        begin
          List.Clear;
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        dlParentGrid:
        begin
          List.Clear;
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            CurDataSet.Params[i].Value := smxProcs.gStorageManagerIntf.Values[CurDataSet.Params[i].ParamName];
        end;
        dlParentCellParam:
        begin
          List.Clear;
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(CurDataSet.Params[i].ParamName, Value) then
            begin
              CurDataSet.Params[i].Value := Value;
              Break;
            end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

{ TsmxFilter }

destructor TsmxFilter.Destroy;
begin
  inherited Destroy;
  if Assigned(FHeader) then
    FHeader.Free;
  if Assigned(FPanel) then
    FPanel.Free;
end;

function TsmxFilter.GetHeader: TLabel;
begin
  if not Assigned(FHeader) then
  begin
    FHeader := TLabel.Create(nil);
    FHeader.Parent := Panel;
    FHeader.Left := 4;
    FHeader.Top := 4;
    FHeader.Width := Panel.Width - 8;
    FHeader.Anchors := [akLeft, akRight];
  end;
  Result := FHeader;
end;

function TsmxFilter.GetHeaderAlignment: TAlignment;
begin
  Result := Header.Alignment;
end;

procedure TsmxFilter.SetHeaderAlignment(Value: TAlignment);
begin
  Header.Alignment := Value;
end;

function TsmxFilter.GetCellCaption: TCaption;
begin
  Result := Header.Caption;
end;

procedure TsmxFilter.SetCellCaption(const Value: TCaption);
begin
  Header.Caption := Value;
end;

function TsmxFilter.GetHeaderColor: TColor;
begin
  Result := Header.Color;
end;

procedure TsmxFilter.SetHeaderColor(Value: TColor);
begin
  Header.Color := Value;
end;

function TsmxFilter.GetHeaderFont: TFont;
begin
  Result := Header.Font;
end;

procedure TsmxFilter.SetHeaderFont(Value: TFont);
begin
  Header.Font := Value;
end;

function TsmxFilter.GetInternalRef: Pointer;
begin
  Result := Pointer(Panel);
end;

function TsmxFilter.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanel.Create(nil);
    FPanel.Caption := '';
    FPanel.Height := 49;
    FPanel.BevelOuter := bvNone;
  end;
  Result := FPanel;
end;

procedure TsmxFilter.SetCellParent(Value: TsmxBaseCell);
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

{ TsmxForm }

destructor TsmxForm.Destroy;
begin
  inherited Destroy;
  if Assigned(FForm) then
    FForm.Free;
end;

procedure TsmxForm.InternalClose;
begin
  if not FIsMainForm then
  begin
    if not (fsModal in (Form.FormState)) then
    begin
      if (foFreeOnClose in FormOptions) and not IsDesigning then
        Free
      else
        Form.Hide;
    end;
  end else
  begin
    FForm := nil;
  end;
end;

procedure TsmxForm.InternalShow;
begin
  Form.Show;
end;

function TsmxForm.InternalShowModal: TModalResult;
begin
  Result := TModalResult(Form.ShowModal);
end;

procedure TsmxForm.FormActivate(Sender: TObject);
begin
  Activate;
end;

procedure TsmxForm.FormDeactivate(Sender: TObject);
begin
  Deactivate;
end;

procedure TsmxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Close;
end;

function TsmxForm.GetCellActive: Boolean;
begin
  Result := Form.Active;
end;

procedure TsmxForm.SetCellActive(Value: Boolean);
begin
  if Value then
    Form.Perform(WM_ACTIVATE, WA_ACTIVE, 0)
  else
    Form.Perform(WM_ACTIVATE, WA_INACTIVE, 0);
end;

function TsmxForm.GetCellCaption: TCaption;
begin
  Result := Form.Caption;
end;

procedure TsmxForm.SetCellCaption(const Value: TCaption);
begin
  Form.Caption := Value;
end;

function TsmxForm.GetCellHint: String;
begin
  Result := Form.Hint;
end;

procedure TsmxForm.SetCellHint(const Value: String);
begin
  Form.Hint := Value;
end;

function TsmxForm.GetCellImageIndex: TImageIndex;
begin
  Result := FFormImageIndex;
end;

procedure TsmxForm.SetCellImageIndex(Value: TImageIndex);
begin
  if FFormImageIndex <> -1 then
    Form.Icon := nil;
  FFormImageIndex := Value;
  if FFormImageIndex <> -1 then
    if Assigned(ImageList) then
      ImageList.GetIcon(Integer(FFormImageIndex), Form.Icon);
end;

function TsmxForm.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    if not FIsMainForm then
    begin
      if not Assigned(Forms.Application.MainForm) then
      begin
        Forms.Application.CreateForm(TForm, FForm);
        FIsMainForm := True;
      end else
        FForm := TForm.Create(nil);
      FForm.OnClose := FormClose;
      FForm.OnActivate := FormActivate;
      FForm.OnDeactivate := FormDeactivate;
    end;
  end;
  Result := FForm;
end;

function TsmxForm.GetFormBorder: TsmxFormBorder;
const
  InOutBorder: array[TFormBorderStyle] of TsmxFormBorder =
    (fbNone, fbDialog, fbSizeable, fbDialog, fbDialog, fbSizeable);
begin
  Result := InOutBorder[Form.BorderStyle];
end;

procedure TsmxForm.SetFormBorder(Value: TsmxFormBorder);
const
  OutInBorder: array[TsmxFormBorder] of TFormBorderStyle =
    (bsNone, bsDialog, bsSizeable);
begin
  Form.BorderStyle := OutInBorder[Value];
end;

function TsmxForm.GetFormPosition: TsmxFormPosition;
const
  InOutPosition: array[TPosition] of TsmxFormPosition =
    (fpDesigned, fpDesigned, fpDesigned, fpDesigned, fpScreenCenter,
     fpScreenCenter, fpOwnerFormCenter, fpOwnerFormCenter);
begin
  Result := InOutPosition[Form.Position];
end;

procedure TsmxForm.SetFormPosition(Value: TsmxFormPosition);
const
  OutInPosition: array[TsmxFormPosition] of TPosition =
    (poDesigned, poScreenCenter, poOwnerFormCenter);
begin
  Form.Position := OutInPosition[Value];
end;

function TsmxForm.GetInternalRef: Pointer;
begin
  Result := Pointer(Form);
end;

function TsmxForm.GetIsMaximize: Boolean;
begin
  Result := Form.WindowState = wsMaximized;
end;

procedure TsmxForm.SetIsMaximize(Value: Boolean);
begin
  if Value then
    Form.WindowState := wsMaximized
  else
    Form.WindowState := wsNormal;
end;

function TsmxForm.GetModalResult: TModalResult;
begin
  Result := Form.ModalResult;
end;

procedure TsmxForm.SetModalResult(Value: TModalResult);
begin
  Form.ModalResult := Value;
end;

procedure TsmxForm.SetFormOptions(Value: TsmxFormOptions);
var
  Obj: TObject;
begin
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetFormOptions(Value);
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Form.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxForm.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) and Assigned(Form) then
    Form.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) and Assigned(Form) then
    if FFormImageIndex <> -1 then
      ImageList.GetIcon(FFormImageIndex, Form.Icon);
end;

procedure TsmxForm.SetIsDesigning(Value: Boolean);
var
  Obj: TObject;
begin
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetIsDesigning(Value);
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Form.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxForm.SetCellParent(Value: TsmxBaseCell);
var
  Obj: TObject;
begin
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetCellParent(Value);
  if ((foFrameForm in FormOptions) or IsDesigning) and Assigned(CellParent) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TWinControl then
      Form.Parent := TWinControl(Obj);
  end;
end;

procedure TsmxForm.SetPopupMenu(Value: TsmxCustomPopupMenu);
var
  Obj: TObject;
begin
  if Assigned(PopupMenu) then
    Form.PopupMenu := nil;
  inherited SetPopupMenu(Value);
  if Assigned(PopupMenu) then
  begin
    Obj := TObject((CellParent as IsmxRefComponent).GetInternalRef);
    if Obj is TPopupMenu then
      Form.PopupMenu := TPopupMenu(Obj);
  end;
end;

end.
