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
    function GetCaption: TCaption; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetHotKey: TShortCut; override;
    function GetImageIndex: TImageIndex; override;
    function GetVisible: Boolean; override;
    function GetInternalRef: Pointer; override;
    procedure InternalRefreshParams; override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetHotKey(Value: TShortCut); override;
    procedure SetImageIndex(Value: TImageIndex); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure ChangeObjectIndex(Value: Integer); override;

    property Action: TAction read GetAction;
  public
    destructor Destroy; override;
  published
    property Caption;
    property Enabled;
    property Hint;
    property HotKey;
    property ImageIndex;
    property Params;
    property Visible;

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
    function GetHeaderText: String; override;
    function GetHeaderColor: TColor; override;
    function GetHeaderFont: TFont; override;
    function GetInternalRef: Pointer; override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetHeaderAlignment(Value: TAlignment); override;
    procedure SetHeaderText(const Value: String); override;
    procedure SetHeaderColor(Value: TColor); override;
    procedure SetHeaderFont(Value: TFont); override;

    property Header: TLabel read GetHeader;
    property Panel: TPanel read GetPanel;
  public
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Enabled;
    property Height;
    property Left;
    property Top;
    property Visible;
    property Width;
    property HeaderAlignment;
    property HeaderColor;
    property HeaderFont;
    property HeaderText;
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
    function GetActive: Boolean; override;
    function GetCaption: TCaption; override;
    function GetHint: String; override;
    function GetImageIndex: TImageIndex; override;
    function GetBorder: TsmxFormBorder; override;
    function GetPosition: TsmxFormPosition; override;
    function GetInternalRef: Pointer; override;
    function GetIsMaximize: Boolean; override;
    function GetModalResult: TModalResult; override;
    procedure InternalClose; override;
    procedure InternalShow; override;
    function InternalShowModal: TModalResult; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetCaption(const Value: TCaption); override;
    procedure SetHint(const Value: String); override;
    procedure SetImageIndex(Value: TImageIndex); override;
    procedure SetCellParent(Value: TsmxBaseCell); override;
    procedure SetBorder(Value: TsmxFormBorder); override;
    procedure SetOptions(Value: TsmxFormOptions); override;
    procedure SetPosition(Value: TsmxFormPosition); override;
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
    property Align;
    property Anchors;
    property Caption;
    property Cursor;
    property Enabled;
    property Height;
    property Hint;
    property ImageIndex;
    property Left;
    property Top;
    property Visible;
    property Width;
    property Border;
    property Options;
    property Position;
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

function TsmxAction.GetCaption: TCaption;
begin
  Result := TCaption(Action.Caption);
end;

procedure TsmxAction.SetCaption(const Value: TCaption);
begin
  Action.Caption := String(Value);
end;

function TsmxAction.GetEnabled: Boolean;
begin
  Result := Action.Enabled;
end;

procedure TsmxAction.SetEnabled(Value: Boolean);
begin
  Action.Enabled := Value;
end;

function TsmxAction.GetHint: String;
begin
  Result := Action.Hint;
end;

procedure TsmxAction.SetHint(const Value: String);
begin
  Action.Hint := Value;
end;

function TsmxAction.GetHotKey: TShortCut;
begin
  Result := Action.ShortCut;
end;

procedure TsmxAction.SetHotKey(Value: TShortCut);
begin
  Action.ShortCut := Value;
end;

function TsmxAction.GetImageIndex: TImageIndex;
begin
  Result := Action.ImageIndex;
end;

procedure TsmxAction.SetImageIndex(Value: TImageIndex);
begin
  Action.ImageIndex := Value;
end;

function TsmxAction.GetVisible: Boolean;
begin
  Result := Action.Visible;
end;

procedure TsmxAction.SetVisible(Value: Boolean);
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
    for i := 0 to Params.Count - 1 do
    begin
      case Params[i].DataLocation of
        dlFilterDesk:
        begin
          if CellAlgorithm is TsmxCustomFilterDesk then
          begin
            Cell := TsmxCustomFilterDesk(CellAlgorithm).FindSlaveByName(Params[i].ParamName);
            if Assigned(Cell) then
              Params[i].ParamValue := TsmxCustomFilter(Cell).Value;
          end else
          if smxClassFuncs.FindFilterOnForm(Form, Params[i].ParamName, Value) then
            Params[i].ParamValue := Value;
        end;
        dlGrid:
        begin
          if CellAlgorithm is TsmxCustomGrid then
          begin
            Cell := TsmxCustomGrid(CellAlgorithm).FindSlaveByName(Params[i].ParamName);
            if Assigned(Cell) then
              Params[i].ParamValue := TsmxCustomColumn(Cell).Value;
          end else
          if smxClassFuncs.FindColumnOnForm(Form, Params[i].ParamName, Value) then
            Params[i].ParamValue := Value;
        end;
        dlParentFilterDesk:
        begin
          List.Clear;
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindFilterOnForm(TsmxCustomForm(List[j]), Params[i].ParamName, Value) then
            begin
              Params[i].ParamValue := Value;
              Break;
            end;
        end;
        dlParentGrid:
        begin
          List.Clear;
          smxClassProcs.AllParents(Form, List, [TsmxCustomForm], True);
          for j := 0 to List.Count - 1 do
            if smxClassFuncs.FindColumnOnForm(TsmxCustomForm(List[j]), Params[i].ParamName, Value) then
            begin
              Params[i].ParamValue := Value;
              Break;
            end;
        end;
        dlStorageParam:
        begin
          if Assigned(smxProcs.gStorageManagerIntf) then
            Params[i].ParamValue := smxProcs.gStorageManagerIntf.Values[Params[i].ParamName];
        end;
        dlParentCellParam:
        begin
          List.Clear;
          smxClassProcs.AllParents(Self, List, []);
          for j := 0 to List.Count - 1 do
            if TsmxBaseCell(List[j]).CellParams(Params[i].ParamName, Value) then
            begin
              Params[i].ParamValue := Value;
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
              CurDataSet.Params[i].Value := TsmxCustomFilter(Cell).Value;
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

function TsmxFilter.GetHeaderText: String;
begin
  Result := String(Header.Caption);
end;

procedure TsmxFilter.SetHeaderText(const Value: String);
begin
  Header.Caption := TCaption(Value);
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
      if (foFreeOnClose in Options) and not IsDesigning then
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

function TsmxForm.GetActive: Boolean;
begin
  Result := Form.Active;
end;

procedure TsmxForm.SetActive(Value: Boolean);
begin
  if Value then
    Form.Perform(WM_ACTIVATE, WA_ACTIVE, 0)
  else
    Form.Perform(WM_ACTIVATE, WA_INACTIVE, 0);
end;

function TsmxForm.GetCaption: TCaption;
begin
  Result := Form.Caption;
end;

procedure TsmxForm.SetCaption(const Value: TCaption);
begin
  Form.Caption := Value;
end;

function TsmxForm.GetHint: String;
begin
  Result := Form.Hint;
end;

procedure TsmxForm.SetHint(const Value: String);
begin
  Form.Hint := Value;
end;

function TsmxForm.GetImageIndex: TImageIndex;
begin
  Result := FFormImageIndex;
end;

procedure TsmxForm.SetImageIndex(Value: TImageIndex);
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

function TsmxForm.GetBorder: TsmxFormBorder;
const
  InOutBorder: array[TFormBorderStyle] of TsmxFormBorder =
    (fbNone, fbDialog, fbSizeable, fbDialog, fbDialog, fbSizeable);
begin
  Result := InOutBorder[Form.BorderStyle];
end;

procedure TsmxForm.SetBorder(Value: TsmxFormBorder);
const
  OutInBorder: array[TsmxFormBorder] of TFormBorderStyle =
    (bsNone, bsDialog, bsSizeable);
begin
  Form.BorderStyle := OutInBorder[Value];
end;

function TsmxForm.GetPosition: TsmxFormPosition;
const
  InOutPosition: array[TPosition] of TsmxFormPosition =
    (fpDesigned, fpDesigned, fpDesigned, fpDesigned, fpScreenCenter,
     fpScreenCenter, fpOwnerFormCenter, fpOwnerFormCenter);
begin
  Result := InOutPosition[Form.Position];
end;

procedure TsmxForm.SetPosition(Value: TsmxFormPosition);
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

procedure TsmxForm.SetOptions(Value: TsmxFormOptions);
var
  Obj: TObject;
begin
  if ((foFrameForm in Options) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetOptions(Value);
  if ((foFrameForm in Options) or IsDesigning) and Assigned(CellParent) then
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
  if ((foFrameForm in Options) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetIsDesigning(Value);
  if ((foFrameForm in Options) or IsDesigning) and Assigned(CellParent) then
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
  if ((foFrameForm in Options) or IsDesigning) and Assigned(CellParent) then
    Form.Parent := nil;
  inherited SetCellParent(Value);
  if ((foFrameForm in Options) or IsDesigning) and Assigned(CellParent) then
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
