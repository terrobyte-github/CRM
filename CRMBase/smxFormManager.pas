unit smxFormManager;

interface

uses
  Classes, Windows, smxBaseClasses, smxClasses;

type
  { TsmxFormItem }

  TsmxFormItem = class(TsmxKitItem)
  private
    FForm: TsmxBaseCell;
    FFormHandle: HWND;
    FFormCfgID: Integer;
    FFormID: Integer;
    //FFormComboID: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property Form: TsmxBaseCell read FForm write FForm;
    property FormHandle: HWND read FFormHandle write FFormHandle;
    property FormCfgID: Integer read FFormCfgID write FFormCfgID;
    property FormID: Integer read FFormID write FFormID;
    //property FormComboID: String read FFormComboID write FFormComboID;
  end;

  { TsmxFormItems }

  TsmxFormItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxFormItem;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxFormItem;
    function FindByForm(AForm: TsmxBaseCell): TsmxFormItem;
    function FindByHandle(AHandle: HWND): TsmxFormItem;
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxFormItem;
    //function FindByComboID(AComboID: String): TsmxFormItem;

    property Items[Index: Integer]: TsmxFormItem read GetItem {write SetItem}; default;
  end;

  { TsmxFormManager }

  TsmxFormManager = class(TsmxComponent)
  private
    //FFormList: TList;
    FFormList: TsmxFormItems;
    function GetForm(Index: Integer): TsmxBaseCell;
    function GetFormCount: Integer;
    //function GetHandle(Handle: HWND): TsmxBaseCell;
    procedure FreeForms;
  protected
    property FormList: TsmxFormItems read FFormList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
    //function FindByComboID(AComboID: String): TsmxBaseCell;
    function FindByHandle(AHandle: HWND): TsmxBaseCell;
    //function HandleOfForm(AForm: TsmxBaseCell): HWND;
    procedure InsertForm(AForm: TsmxBaseCell);
    procedure RemoveForm(AForm: TsmxBaseCell);

    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TsmxBaseCell read GetForm; default;
    //property FormList: TsmxFormItems read FFormList;
    //property Handles[Handle: HWND]: TsmxBaseCell read GetHandle;
  end;

function FormManager: TsmxFormManager;

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

{ TsmxFormItem }

constructor TsmxFormItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FForm := nil;
  FFormHandle := 0;
  FFormCfgID := 0;
  FFormID := 0;
  //FFormComboID := '';
end;

{ TsmxFormItems }

function TsmxFormItems.Add: TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Add);
end;

function TsmxFormItems.FindByForm(AForm: TsmxBaseCell): TsmxFormItem;
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

{function TsmxFormItems.FindByComboID(AComboID: String): TsmxFormItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].FormComboID, AComboID) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;}

function TsmxFormItems.GetItem(Index: Integer): TsmxFormItem;
begin
  Result := TsmxFormItem(inherited Items[Index]);
end;

{procedure TsmxFormItems.SetItem(Index: Integer; Value: TsmxFormItem);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxFormManager }

constructor TsmxFormManager.Create(AOwner: TComponent);
begin
  //FFormList := TList.Create;
  FFormList := TsmxFormItems.Create(TsmxFormItem);
end;

destructor TsmxFormManager.Destroy;
begin
  FreeForms;
  //FFormList.Free;
  FFormList.Free;
end;

procedure TsmxFormManager.FreeForms;
var i: Integer;
begin
  {for i := FFormList.Count - 1 downto 0 do
  begin
    TsmxBaseCell(FFormList[i]).Free;
    //FFormList.Delete(i);
  end;}
  for i := FFormList.Count - 1 downto 0 do
    FFormList[i].Form.Free;
end;

function TsmxFormManager.FindByHandle(AHandle: HWND): TsmxBaseCell;
var f: TsmxFormItem; //i: Integer; c: TObject; h: HWND;
begin
  Result := nil;
  f := FormList.FindByHandle(AHandle);
  if Assigned(f) then
    Result := f.Form;

  {for i := 0 to FormCount - 1 do
  begin
    h := 0;
    c := FormList[i].GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        h := TWinControl(c).Handle;
    if AHandle = h then
    begin
      Result := FormList[i];
      Break;
    end;
  end;}
end;

function TsmxFormManager.FindByComboID(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
var f: TsmxFormItem; 
begin
  Result := nil;
  f := FormList.FindByComboID(ACfgID, AID);
  if Assigned(f) then
    Result := f.Form;
end;

{function TsmxFormManager.FindByComboID(AComboID: String): TsmxBaseCell;
var f: TsmxFormItem;
begin
  Result := nil;
  f := FormList.FindByComboID(AComboID);
  if Assigned(f) then
    Result := f.Form;
end;}

function TsmxFormManager.GetFormCount: Integer;
begin
  Result := FFormList.Count;
end;

function TsmxFormManager.GetForm(Index: Integer): TsmxBaseCell;
begin
  Result := FFormList[Index].Form;
end;

{function TsmxFormManager.GetHandle(Handle: HWND): TsmxBaseCell;
var i: Integer; c: TObject; h: HWND;
begin
  Result := nil;
  for i := 0 to FormCount - 1 do
  begin
    h := 0;
    c := FormList[i].GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        h := TWinControl(c).Handle;
    if Handle = h then
    begin
      Result := FormList[i];
      Break;
    end;
  end;
end;}

{function TsmxFormManager.HandleOfForm(AForm: TsmxBaseCell): HWND;
var i: Integer; c: TObject;
begin
  Result := 0;
  i := FFormList.IndexOf(AForm);
  if i >= 0 then
  begin
    c := FormList[i].GetInternalObject;
    if Assigned(c) then
      if c is TWinControl then
        Result := TWinControl(c).Handle;
  end;
end;}

procedure TsmxFormManager.InsertForm(AForm: TsmxBaseCell);
var f: TsmxFormItem; c: TObject; //i: Integer;
begin
  {i := FFormList.IndexOf(AForm);
  if i = -1 then
    FFormList.Add(AForm);}
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
      //FormComboID := IntToStr(AForm.CfgID) + '.' + IntToStr(AForm.ID);
    end;
end;

procedure TsmxFormManager.RemoveForm(AForm: TsmxBaseCell);
var f: TsmxFormItem;
begin
  //FFormList.Remove(AForm);
  f := FormList.FindByForm(AForm);
  if Assigned(f) then
    FormList.Remove(f);
end;

initialization
  _FormManager := TsmxFormManager.Create(nil);

finalization
  _FormManager.Free;

end.
