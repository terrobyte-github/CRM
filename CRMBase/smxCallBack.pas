unit smxCallBack;

interface

uses
  Classes, smxBaseClasses, smxClasses, smxTypes;

type
  { TsmxCallBackParam }

  TsmxCallBackParam = class(TsmxKitItem)
  private
    FParamIndex: Integer;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamIndex: Integer read FParamIndex write FParamIndex;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxCallBackParams }

  TsmxCallBackParams = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxCallBackParam;
  public
    function Add: TsmxCallBackParam;
    function FindByIndex(AIndex: Integer): TsmxCallBackParam;

    property Items[Index: Integer]: TsmxCallBackParam read GetItem; default;
  end;

  { TsmxCallBack }

  TsmxCallBack = class(TsmxComponent)
  private
    FParamList: TsmxCallBackParams;
    function GetParam(Index: Integer): Variant;
    procedure SetParam(Index: Integer; Value: Variant);
  protected
    property ParamList: TsmxCallBackParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CallParams[Index: Integer]: Variant read GetParam write SetParam; default;
  end;

function CallBack: TsmxCallBack;

var
  FuncCallBack: TsmxFuncCallBack = nil;

implementation

uses
  Variants;

var
  _CallBack: TsmxCallBack = nil;

function CallBack: TsmxCallBack;
begin
  Result := _CallBack;
end;

{ TsmxCallBackParam }

constructor TsmxCallBackParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamIndex := 0;
  FParamValue := Unassigned;
end;

{ TsmxCallBackParams }

function TsmxCallBackParams.Add: TsmxCallBackParam;
begin
  Result := TsmxCallBackParam(inherited Add);
end;

function TsmxCallBackParams.FindByIndex(AIndex: Integer): TsmxCallBackParam;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ParamIndex = AIndex then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxCallBackParams.GetItem(Index: Integer): TsmxCallBackParam;
begin
  Result := TsmxCallBackParam(inherited Items[Index]);
end;

{ TsmxCallBack }

constructor TsmxCallBack.Create(AOwner: TComponent);
begin
  FParamList := TsmxCallBackParams.Create(TsmxCallBackParam);
end;

destructor TsmxCallBack.Destroy;
begin
  FParamList.Free;
end;

function TsmxCallBack.GetParam(Index: Integer): Variant;
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Unassigned;
end;

procedure TsmxCallBack.SetParam(Index: Integer; Value: Variant);
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    p.ParamValue := Value
  else
    with FParamList.Add do
    begin
      ParamIndex := Index;
      ParamValue := Value;
    end;
end;

initialization
  _CallBack := TsmxCallBack.Create(nil);
  FuncCallBack := _CallBack.GetParam;

finalization
  FuncCallBack := nil;
  _CallBack.Free;

end.
