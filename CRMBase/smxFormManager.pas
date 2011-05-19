{**************************************}
{                                      }
{            SalesMan v1.0             }
{         Form Manager classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxFormManager;

interface

uses
  Classes, Windows, smxBaseClasses, smxClasses, smxClassTypes;

type
  { TsmxFormItem }

  TsmxFormItem = class(TsmxKitItem)
  private
    FForm: TsmxCustomForm;
    FFormHandle: HWND;
    FFormCfgID: Integer;
    FFormID: Integer;
  public
    constructor Create(AKit: TsmxKit); override;

    property Form: TsmxCustomForm read FForm write FForm;
    property FormHandle: HWND read FFormHandle write FFormHandle;
    property FormCfgID: Integer read FFormCfgID write FFormCfgID;
    property FormID: Integer read FFormID write FFormID;
  end;

  { TsmxFormItems }

  TsmxFormItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxFormItem;
  public
    function Add: TsmxFormItem;
    function FindByForm(AForm: TsmxCustomForm): TsmxFormItem;
    function FindByHandle(AHandle: HWND): TsmxFormItem;
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxFormItem;

    property Items[Index: Integer]: TsmxFormItem read GetItem; default;
  end;

  { TsmxFormManager }

  TsmxFormManager = class(TsmxComponent)
  private
    FFormList: TsmxFormItems;
    function GetForm(Index: Integer): TsmxCustomForm;
    function GetFormCount: Integer;
    procedure DestroyForms;
  protected
    property FormList: TsmxFormItems read FFormList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
    function FindByHandle(AHandle: HWND): TsmxCustomForm;
    procedure InsertForm(AForm: TsmxCustomForm);
    procedure RemoveForm(AForm: TsmxCustomForm);

    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TsmxCustomForm read GetForm; default;
  end;

function FormManager: TsmxFormManager;
function FindFormByComboID(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
function FindFormByHandle(AHandle: HWND): TsmxCustomForm;

implementation

uses
  Controls;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

var
  _FormManager: TsmxFormManager = nil;

function FormManager: TsmxFormManager;
begin
  Result := _FormManager;
end;

function FindFormByComboID(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
begin
  Result := _FormManager.FindByComboID(ACfgID, AID);
end;

function FindFormByHandle(AHandle: HWND): TsmxCustomForm;
begin
  Result := _FormManager.FindByHandle(AHandle);
end;

{ TsmxFormItem }

constructor TsmxFormItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FForm := nil;
  FFormHandle := 0;
  FFormCfgID := 0;
  FFormID := 0;
end;

{ TsmxFormItems }

function TsmxFormItems.Add: TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Add);
end;

function TsmxFormItems.FindByForm(AForm: TsmxCustomForm): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Form = AForm then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxFormItems.FindByHandle(AHandle: HWND): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FormHandle = AHandle then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxFormItems.FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i].FormCfgID = ACfgID) and (Items[i].FormID = AID) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxFormItems.GetItem(Index: Integer): TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Items[Index]);
end;

{ TsmxFormManager }

constructor TsmxFormManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormList := TsmxFormItems.Create(TsmxFormItem);
end;

destructor TsmxFormManager.Destroy;
begin
  DestroyForms;
  FFormList.Free;
  inherited Destroy;
end;

procedure TsmxFormManager.DestroyForms;
var i: Integer;
begin
  for i := FFormList.Count - 1 downto 0 do
    FFormList[i].Form.Free;
end;

function TsmxFormManager.FindByHandle(AHandle: HWND): TsmxCustomForm;
var f: TsmxFormItem;
begin
  Result := nil;
  f := FormList.FindByHandle(AHandle);
  if Assigned(f) then
    Result := f.Form;
end;

function TsmxFormManager.FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;
var f: TsmxFormItem;
begin
  Result := nil;
  f := FormList.FindByComboID(ACfgID, AID);
  if Assigned(f) then
    Result := f.Form;
end;

function TsmxFormManager.GetFormCount: Integer;
begin
  Result := FFormList.Count;
end;

function TsmxFormManager.GetForm(Index: Integer): TsmxCustomForm;
begin
  Result := FFormList[Index].Form;
end;

procedure TsmxFormManager.InsertForm(AForm: TsmxCustomForm);
var f: TsmxFormItem; c: TObject;
begin
  f := FormList.FindByForm(AForm);
  if not Assigned(f) then
    with FormList.Add do
    begin
      Form := AForm;
      c := _TsmxBaseCell(AForm).GetInternalObject;
      if c is TWinControl then
        FormHandle := TWinControl(c).Handle;
      FormCfgID := AForm.CfgID;
      FormID := AForm.ID;
    end;
end;

procedure TsmxFormManager.RemoveForm(AForm: TsmxCustomForm);
var f: TsmxFormItem;
begin
  f := FormList.FindByForm(AForm);
  if Assigned(f) then
    FormList.Remove(f);
end;

initialization
  _FormManager := TsmxFormManager.Create(nil);

finalization
  _FormManager.Free;

end.
