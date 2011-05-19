unit smxFilterCells;

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, Buttons, smxClasses, smxCells,
  smxDBIntf, smxTypes;

type
  { TsmxEditFilter }

  TsmxEditFilter = class(TsmxPanelFilter)
  private
    FEdit: TEdit;
  protected
    function GetCellEnable: Boolean; override;
    //function GetFilterCaption: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnable(Value: Boolean); override;
    //procedure SetFilterCaption(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;

    property Edit: TEdit read FEdit;
  public
    constructor Create(AOwner: TComponent; const ADB: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
  end;

  { TsmxDateTimeFilter }

  TsmxDateTimeFilter = class(TsmxPanelFilter)
  private
    FDateTime: TDateTimePicker;
    procedure SetFormat;
  protected
    function GetCellEnable: Boolean; override;
    //function GetFilterCaption: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnable(Value: Boolean); override;
    //procedure SetFilterCaption(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;

    property DateTime: TDateTimePicker read FDateTime;
  public
    constructor Create(AOwner: TComponent; const ADB: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
  end;

  { TsmxBitBtnFilter }

  TsmxBitBtnFilter = class(TsmxPanelFilter)
  private
    FBitBtn: TBitBtn;
    FValue: Variant;
    //procedure SetAction;
  protected
    function GetCellEnable: Boolean; override;
    //function GetFilterCaption: String; override;
    function GetFilterText: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    //procedure InstallParent; override;
    //procedure SetAlgorithm(Value: TsmxCustomAlgorithm); override;
    procedure SetCellEnable(Value: Boolean); override;
    //procedure SetFilterCaption(Value: String); override;
    procedure SetFilterText(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;
    //procedure UnInstallParent; override;
    procedure Initialize; override;
    procedure UnInitialize; override;

    property BitBtn: TBitBtn read FBitBtn;
  public
    constructor Create(AOwner: TComponent; const ADB: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
    //procedure PutParams(Params: Variant); override;
  end;

  { TsmxNumEditFilter }

  TsmxNumEditFilter = class(TsmxPanelFilter)
  private
    FEdit: TEdit;
  protected
    //procedure ChangeProc(Sender: TObject); virtual;
    procedure KeyPressProc(Sender: TObject; var Key: Char);
    function GetCellEnable: Boolean; override;
    //function GetFilterCaption: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    procedure SetCellEnable(Value: Boolean); override;
    //procedure SetFilterCaption(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;

    property Edit: TEdit read FEdit;
  public
    constructor Create(AOwner: TComponent; const ADB: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
  end;

  { TsmxLabelFilter }

  TsmxLabelFilter = class(TsmxPanelFilter)
  private
    FSLabel: TLabel;
    FValue: Variant;
  protected
    //function GetCellEnable: Boolean; override;
    //function GetFilterCaption: String; override;
    function GetFilterText: String; override;
    function GetFilterValue: Variant; override;
    function GetInternalObject: TObject; override;
    //procedure SetCellEnable(Value: Boolean); override;
    //procedure SetFilterCaption(Value: String); override;
    procedure SetFilterText(Value: String); override;
    procedure SetFilterValue(Value: Variant); override;

    property SLabel: TLabel read FSLabel;
  public
    constructor Create(AOwner: TComponent; const ADB: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
  end;

implementation

uses
  Variants, Forms, SysUtils, Graphics, smxGlobal, smxFuncs;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

{ TsmxEditFilter }

constructor TsmxEditFilter.Create(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADB, ACfgID, AID);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Panel;
  FEdit.AutoSize := False;
  FEdit.Width := Panel.Width - 8;
  FEdit.Anchors := [akLeft, akRight];
  FEdit.Left := 4;
  FEdit.Top := 20;
  //FEdit.Hint := Cfg.FilterHeader.Text;
  //FEdit.ShowHint := True;
  //FEdit.Text := Cfg.FilterText;
  FEdit.Font.Color := Cfg.FilterFont.Color;
  FEdit.Font.Name := Cfg.FilterFont.Name;
  FEdit.Font.Size := Cfg.FilterFont.Size;
  FEdit.Font.Style := Cfg.FilterFont.Style;
  //SetFilterValue(Cfg.FilterDefValue);
  FEdit.Text := '';
end;

destructor TsmxEditFilter.Destroy;
begin
  FEdit.Parent := nil;
  FEdit.Free;
  inherited Destroy;
end;

function TsmxEditFilter.GetCellEnable: Boolean;
begin
  Result := FEdit.Enabled;
end;

{function TsmxEditFilter.GetFilterCaption: String;
begin
  Result := FEdit.Text;
end;}

function TsmxEditFilter.GetFilterValue: Variant;
begin
  Result := FEdit.Text;
  //if (Result = '') or (Result = Cfg.FilterText) then
  if Result = '' then
    Result := Null;
end;

function TsmxEditFilter.GetInternalObject: TObject;
begin
  Result := FEdit;
end;

procedure TsmxEditFilter.SetCellEnable(Value: Boolean);
begin
  FEdit.Enabled := Value;
end;

{procedure TsmxEditFilter.SetFilterCaption(Value: String);
begin
  FEdit.Text := Value;
end;}

procedure TsmxEditFilter.SetFilterValue(Value: Variant);
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    FEdit.Text := '' else
    FEdit.Text := Value;
end;

{ TsmxDateTimeFilter }

constructor TsmxDateTimeFilter.Create(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADB, ACfgID, AID);
  FDateTime := TDateTimePicker.Create(Self);
  SetFormat;
  FDateTime.Parent := Panel;
  FDateTime.Width := Panel.Width - 8;
  FDateTime.Anchors := [akLeft, akRight];
  FDateTime.Left := 4;
  FDateTime.Top := 20;
  //FDateTime.Hint := Cfg.FilterHeader.Text;
  //FDateTime.ShowHint := True;
  FDateTime.ShowCheckbox := True;
  //FDateTime.DateTime := StrToDateTimeEx(Cfg.FilterText);
  FDateTime.Font.Color := Cfg.FilterFont.Color;
  FDateTime.Font.Name := Cfg.FilterFont.Name;
  FDateTime.Font.Size := Cfg.FilterFont.Size;
  FDateTime.Font.Style := Cfg.FilterFont.Style;
  //SetFilterValue(StrToDateTimeEx(Cfg.FilterDefValue));
  FDateTime.DateTime := Date;
  //FDateTime.Checked := False;
end;

destructor TsmxDateTimeFilter.Destroy;
begin
  FDateTime.Parent := nil;
  FDateTime.Free;
  inherited Destroy;
end;

function TsmxDateTimeFilter.GetCellEnable: Boolean;
begin
  Result := FDateTime.Enabled;
end;

{function TsmxDateTimeFilter.GetFilterCaption: String;
begin
  Result := DateToStr(FDateTime.DateTime);
end;}

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

procedure TsmxDateTimeFilter.SetCellEnable(Value: Boolean);
begin
  FDateTime.Enabled := Value;
end;

{procedure TsmxDateTimeFilter.SetFilterCaption(Value: String);
begin
  FDateTime.DateTime := StrToDateTimeDef(Value, StrToDate('30.12.1899'));
end;}

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
end;

{ TsmxBitBtnFilter }

constructor TsmxBitBtnFilter.Create(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADB, ACfgID, AID);
  FBitBtn := TBitBtn.Create(Self);
  FBitBtn.Parent := Panel;
  FBitBtn.Width := Panel.Width - 8;
  FBitBtn.Anchors := [akLeft, akRight];
  FBitBtn.Left := 4;
  FBitBtn.Top := 20;
  //FBitBtn.Caption := Cfg.FilterText;
  //FBitBtn.Caption := Algorithm.ItemCaption;
  FBitBtn.Margin := 2;
  //FBitBtn.ShowHint := True;
  //SetAction;
  //FBitBtn.Caption := '';
  //FBitBtn.Caption := TCaption(Cfg.FilterText);
  //FValue := Null;
  FBitBtn.Font.Color := Cfg.FilterFont.Color;
  FBitBtn.Font.Name := Cfg.FilterFont.Name;
  FBitBtn.Font.Size := Cfg.FilterFont.Size;
  FBitBtn.Font.Style := Cfg.FilterFont.Style;
  //SetFilterValue(Cfg.FilterDefValue);
  FValue := Null;
  FBitBtn.Caption := '';
  Initialize;
  InstallParent;
end;

destructor TsmxBitBtnFilter.Destroy;
begin
  UnInstallParent;
  UnInitialize;
  FBitBtn.Parent := nil;
  FBitBtn.Free;
  inherited Destroy;
end;

function TsmxBitBtnFilter.GetCellEnable: Boolean;
begin
  Result := FBitBtn.Enabled;
end;

{function TsmxBitBtnFilter.GetFilterCaption: String;
begin
  Result := String(FBitBtn.Caption);
end;}

function TsmxBitBtnFilter.GetFilterText: String;
begin
  Result := String(FBitBtn.Caption);
end;

function TsmxBitBtnFilter.GetFilterValue: Variant;
begin
  Result := FValue;
end;

function TsmxBitBtnFilter.GetInternalObject: TObject;
begin
  Result := FBitBtn;
end;

{procedure TsmxBitBtnFilter.InstallParent;
begin
  if Assigned(Algorithm) then
    Algorithm.ParentCell := Self;
  if Assigned(Request) then
    Request.ParentCell := Self;
end;}

{procedure TsmxBitBtnFilter.PutParams(Params: Variant);
var ap: TsmxAlgorithmParams; p: TsmxAlgorithmParam;
begin
  ap := VarToAlgorithmParams(Params);
  try
    p := ap.FindByName('Key');
    if Assigned(p) then
      FilterValue := p.ParamValue;
    p := ap.FindByName('Value');
    if Assigned(p) then
      //FilterCaption := p.ParamValue;
      FBitBtn.Caption := p.ParamValue;
  finally
    ap.Free;
  end;
end;}

{procedure TsmxBitBtnFilter.SetAction;
var c: TObject;
begin
  c := nil;
  if Assigned(Algorithm) then
    c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
    FBitBtn.Action := TBasicAction(c);
end;}

{procedure TsmxBitBtnFilter.SetAlgorithm(Value: TsmxCustomAlgorithm);
var c: TObject;
begin
  if Assigned(Algorithm) then
    FBitBtn.Action := nil;
  inherited SetAlgorithm(Value);
  if Assigned(Value) then
  begin
    c := _TsmxBaseCell(Algorithm).GetInternalObject;
    if c is TBasicAction then
      FBitBtn.Action := TBasicAction(c);
  end;
end;}

procedure TsmxBitBtnFilter.SetCellEnable(Value: Boolean);
begin
  FBitBtn.Enabled := Value;
end;

{procedure TsmxBitBtnFilter.SetFilterCaption(Value: String);
begin
  FBitBtn.Caption := TCaption(Value);
end;}

procedure TsmxBitBtnFilter.SetFilterText(Value: String);
begin
  FBitBtn.Caption := TCaption(Value);
end;

procedure TsmxBitBtnFilter.SetFilterValue(Value: Variant);
//var v: Variant; f: IsmxField;
begin
  {if VarIsEmpty(Value) or VarIsNull(Value) then
  begin
    FValue := Null;
    FBitBtn.Caption := '';
  end else
  begin
    FValue := Value;
    if Assigned(Request) then
    begin
      Request.Perform;
      f := Request.FindFieldSense(fsValue);
      if Assigned(f) then
        v := f.Value;
      if not VarIsEmpty(v) and not VarIsNull(v) then
        FBitBtn.Caption := TCaption(v) else
        FBitBtn.Caption := '';
    end;
  end;}
  FValue := Value;
end;

procedure TsmxBitBtnFilter.Initialize;
var c: TObject;
begin
  if Assigned(Algorithm) then
  begin
    if Algorithm.CellImageIndex >= 0 then
      //if Assigned(ImageList) then
        ImageList.GetBitmap(Algorithm.CellImageIndex, FBitBtn.Glyph);
    c := _TsmxBaseCell(Algorithm).GetInternalObject;
    if c is TBasicAction then
      FBitBtn.Action := TBasicAction(c);
  end;
end;

procedure TsmxBitBtnFilter.UnInitialize;
begin
  FBitBtn.Action := nil;
end;

{procedure TsmxBitBtnFilter.UnInstallParent;
begin
  if Assigned(Algorithm) then
    Algorithm.ParentCell := nil;
  if Assigned(Request) then
    Request.ParentCell := nil;
end;}

{ TsmxNumEditFilter }

constructor TsmxNumEditFilter.Create(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADB, ACfgID, AID);
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Panel;
  FEdit.AutoSize := False;
  FEdit.Width := Panel.Width - 8;
  FEdit.Anchors := [akLeft, akRight];
  FEdit.Left := 4;
  FEdit.Top := 20;
  //FEdit.Hint := Cfg.FilterHeader.Text;
  //FEdit.ShowHint := True;
  //FEdit.Text := Cfg.FilterText;
  //FEdit.OnChange := ChangeProc;
  FEdit.OnKeyPress := KeyPressProc;
  FEdit.Font.Color := Cfg.FilterFont.Color;
  FEdit.Font.Name := Cfg.FilterFont.Name;
  FEdit.Font.Size := Cfg.FilterFont.Size;
  FEdit.Font.Style := Cfg.FilterFont.Style;
  //SetFilterValue(Cfg.FilterDefValue);
  FEdit.Text := '';
end;

destructor TsmxNumEditFilter.Destroy;
begin
  FEdit.OnKeyPress := nil;
  //FEdit.OnChange := nil;
  FEdit.Parent := nil;
  FEdit.Free;
  inherited Destroy;
end;

{procedure TsmxNumEditFilter.ChangeProc(Sender: TObject);
var s: String;
begin
  s := Trim(FEdit.Text);
  if (Cfg.DisplayFormat <> '') and (s <> '') then
    FEdit.Text := FormatFloat(Cfg.DisplayFormat, StrToFloatDef(s, 0));
end;}

function TsmxNumEditFilter.GetCellEnable: Boolean;
begin
  Result := FEdit.Enabled;
end;

{function TsmxNumEditFilter.GetFilterCaption: String;
begin
  Result := FEdit.Text;
end;}

function TsmxNumEditFilter.GetFilterValue: Variant;
begin
  Result := FEdit.Text;
  //if (Result = '') or (Result = Cfg.FilterText) then
  if Result = '' then
    Result := Null else
  if Cfg.ValueFormat <> '' then
    Result := FormatFloat(Cfg.ValueFormat, Result);
end;

function TsmxNumEditFilter.GetInternalObject: TObject;
begin
  Result := FEdit;
end;

procedure TsmxNumEditFilter.KeyPressProc(Sender: TObject; var Key: Char);

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

procedure TsmxNumEditFilter.SetCellEnable(Value: Boolean);
begin
  FEdit.Enabled := Value;
end;

{procedure TsmxNumEditFilter.SetFilterCaption(Value: String);
begin
  FEdit.Text := Value;
end;}

procedure TsmxNumEditFilter.SetFilterValue(Value: Variant);
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    FEdit.Text := '' else
    FEdit.Text := FloatToStr(StrToFloatDef(Value, 0));
end;

{ TsmxLabelFilter }

constructor TsmxLabelFilter.Create(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADB, ACfgID, AID);
  FSLabel := TLabel.Create(Self);
  FSLabel.Parent := Panel;
  FSLabel.Width := Panel.Width - 8;
  FSLabel.Anchors := [akLeft, akRight];
  FSLabel.Left := 4;
  FSLabel.Top := 20;
  //FEdit.Hint := Cfg.FilterHeader.Text;
  //FEdit.ShowHint := True;
  //FEdit.Text := Cfg.FilterText;
  //InstallParent;
  FSLabel.Font.Color := Cfg.FilterFont.Color;
  FSLabel.Font.Name := Cfg.FilterFont.Name;
  FSLabel.Font.Size := Cfg.FilterFont.Size;
  FSLabel.Font.Style := Cfg.FilterFont.Style;
  //SetFilterValue(Cfg.FilterDefValue);
  FValue := Null;
  FSLabel.Caption := '';
end;

destructor TsmxLabelFilter.Destroy;
begin
  //UnInstallParent;
  FSLabel.Parent := nil;
  FSLabel.Free;
  inherited Destroy;
end;

{function TsmxLabelFilter.GetCellEnable: Boolean;
begin
  Result := FSLabel.Enabled;
end;}

{function TsmxEditFilter.GetFilterCaption: String;
begin
  Result := FEdit.Text;
end;}

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

{procedure TsmxLabelFilter.SetCellEnable(Value: Boolean);
begin
  FSLabel.Enabled := Value;
end;}

{procedure TsmxLabelFilter.SetFilterCaption(Value: String);
begin
  FEdit.Text := Value;
end;}

procedure TsmxLabelFilter.SetFilterText(Value: String);
begin
  FSLabel.Caption := TCaption(Value);
end;

procedure TsmxLabelFilter.SetFilterValue(Value: Variant);
//var v: Variant; f: IsmxField;
begin
  {if VarIsEmpty(Value) or VarIsNull(Value) then
  begin
    FValue := Null;
    FSLabel.Caption := '';
  end else
  begin
    FValue := Value;
    if Assigned(Request) then
    begin
      Request.Perform;
      f := Request.FindFieldSense(fsValue);
      if Assigned(f) then
        v := f.Value;
      if not VarIsEmpty(v) and not VarIsNull(v) then
        FSLabel.Caption := TCaption(v) else
        FSLabel.Caption := '';
    end;
  end;}
  FValue := Value;
end;

initialization
  RegisterClasses([TsmxEditFilter, TsmxDateTimeFilter, TsmxBitBtnFilter,
    TsmxNumEditFilter, TsmxLabelFilter]);

end.
