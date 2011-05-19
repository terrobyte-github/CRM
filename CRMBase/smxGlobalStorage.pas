unit smxGlobalStorage;

interface

uses
  Classes, smxBaseClasses, smxClasses;

type
  { TsmxGlobalParam }

  //TsmxGlobalParamType = (gptSystem, gptUser);

  TsmxGlobalParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxGlobalParams }

  TsmxGlobalParams = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxGlobalParam;
  public
    function Add: TsmxGlobalParam;
    function FindByName(AParamName: String): TsmxGlobalParam;

    property Items[Index: Integer]: TsmxGlobalParam read GetItem; default;
  end;

  { TsmxGlobalStorage }

  TsmxGlobalStorage = class(TsmxComponent)
  private
    FParamList: TsmxGlobalParams;
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  protected
    property ParamList: TsmxGlobalParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure InsertParam(AName: String; AValue: Variant);
    //procedure RemoveParam(AName: String);
    //function ExistsParam(AName: String): Boolean;

    property ParamValues[Name: String]: Variant read GetValue write SetValue; default;
  end;

function GlobalStorage: TsmxGlobalStorage;

//const
  //FuncGlobalValue: TsmxFuncGlobalValue = nil;

implementation

uses
  Variants, SysUtils;

var
  _GlobalStorage: TsmxGlobalStorage = nil;

function GlobalStorage: TsmxGlobalStorage;
begin
  Result := _GlobalStorage;
end;

{ TsmxGlobalParam }

constructor TsmxGlobalParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamName := '';
  FParamValue := Null;
end;

{ TsmxGlobalParams }

function TsmxGlobalParams.Add: TsmxGlobalParam;
begin
  Result := TsmxGlobalParam(inherited Add);
end;

function TsmxGlobalParams.FindByName(AParamName: String): TsmxGlobalParam;
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

function TsmxGlobalParams.GetItem(Index: Integer): TsmxGlobalParam;
begin
  Result := TsmxGlobalParam(inherited Items[Index]);
end;

{ TsmxGlobalStorage }

constructor TsmxGlobalStorage.Create(AOwner: TComponent);
begin
  FParamList := TsmxGlobalParams.Create(TsmxGlobalParam);
end;

destructor TsmxGlobalStorage.Destroy;
begin
  FParamList.Free;
end;

{procedure TsmxGlobalStorage.InsertParam(AName: String; AValue: Variant);
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

function TsmxGlobalStorage.GetValue(Name: String): Variant;
var p: TsmxGlobalParam;
begin
  p := FParamList.FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Null;
end;

procedure TsmxGlobalStorage.SetValue(Name: String; Value: Variant);
var p: TsmxGlobalParam;
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
  _GlobalStorage := TsmxGlobalStorage.Create(nil);
  //FuncGlobalValue := _GlobalParams.GetValue;

finalization
  //FuncGlobalValue := nil;
  _GlobalStorage.Free;

end.
