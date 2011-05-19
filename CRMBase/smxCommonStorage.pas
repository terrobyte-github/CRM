unit smxCommonStorage;

interface

uses
  Classes, smxBaseClasses, smxClasses;

type
  { TsmxCommonParam }

  //TsmxGlobalParamType = (gptSystem, gptUser);

  TsmxCommonParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxCommonParams }

  TsmxCommonParams = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxCommonParam;
  public
    function Add: TsmxCommonParam;
    function FindByName(AParamName: String): TsmxCommonParam;

    property Items[Index: Integer]: TsmxCommonParam read GetItem; default;
  end;

  { TsmxCommonStorage }

  TsmxCommonStorage = class(TsmxComponent)
  private
    FParamList: TsmxCommonParams;
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  protected
    property ParamList: TsmxCommonParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure InsertParam(AName: String; AValue: Variant);
    //procedure RemoveParam(AName: String);
    //function ExistsParam(AName: String): Boolean;

    property ParamValues[Name: String]: Variant read GetValue write SetValue; default;
  end;

function CommonStorage: TsmxCommonStorage;

//const
  //FuncGlobalValue: TsmxFuncGlobalValue = nil;

implementation

uses
  Variants, SysUtils;

var
  _CommonStorage: TsmxCommonStorage = nil;

function CommonStorage: TsmxCommonStorage;
begin
  Result := _CommonStorage;
end;

{ TsmxCommonParam }

constructor TsmxCommonParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamName := '';
  FParamValue := Null;
end;

{ TsmxCommonParams }

function TsmxCommonParams.Add: TsmxCommonParam;
begin
  Result := TsmxCommonParam(inherited Add);
end;

function TsmxCommonParams.FindByName(AParamName: String): TsmxCommonParam;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxCommonParams.GetItem(Index: Integer): TsmxCommonParam;
begin
  Result := TsmxCommonParam(inherited Items[Index]);
end;

{ TsmxCommonStorage }

constructor TsmxCommonStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamList := TsmxCommonParams.Create(TsmxCommonParam);
end;

destructor TsmxCommonStorage.Destroy;
begin
  FParamList.Free;
  inherited Destroy;
end;

{procedure TsmxCommonStorage.InsertParam(AName: String; AValue: Variant);
var p: TsmxParam;
begin
  p := FParamList.FindByName(AName);
  if Assigned(p) then
    raise EsmxCellError.CreateRes(@SCellParamNotFound)
  else
    with FParamList.Add do
    begin
      ParamName := AName;
      ParamValue := AValue;
    end;
end;}

function TsmxCommonStorage.GetValue(Name: String): Variant;
var p: TsmxCommonParam;
begin
  p := FParamList.FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Null;
end;

procedure TsmxCommonStorage.SetValue(Name: String; Value: Variant);
var p: TsmxCommonParam;
begin
  p := FParamList.FindByName(Name);
  if Assigned(p) then
    p.ParamValue := Value
  else
    with FParamList.Add do
    begin
      ParamName := Name;
      ParamValue := Value;
    end;
end;

initialization
  _CommonStorage := TsmxCommonStorage.Create(nil);
  //FuncGlobalValue := _GlobalParams.GetValue;

finalization
  //FuncGlobalValue := nil;
  _CommonStorage.Free;

end.
