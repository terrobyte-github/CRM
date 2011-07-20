{**************************************}
{                                      }
{            SalesMan v1.0             }
{          CallBack classes            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxCallBack;

interface

uses
  Classes, smxBaseClasses, smxClasses, smxLibTypes;

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
  private
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
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; Value: Variant);
  protected
    property ParamList: TsmxCallBackParams read FParamList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ParamValues[Index: Integer]: Variant read GetValue write SetValue; default;
  end;

function CallBack: TsmxCallBack;

const
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
  FParamIndex := -1;
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
  inherited Create(AOwner);
  FParamList := TsmxCallBackParams.Create(TsmxCallBackParam);
end;

destructor TsmxCallBack.Destroy;
begin
  FParamList.Free;
  inherited Destroy;
end;

function TsmxCallBack.GetValue(Index: Integer): Variant;
var p: TsmxCallBackParam;
begin
  p := FParamList.FindByIndex(Index);
  if Assigned(p) then
    Result := p.ParamValue else
    Result := Unassigned;
end;

procedure TsmxCallBack.SetValue(Index: Integer; Value: Variant);
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
  FuncCallBack := _CallBack.GetValue;

finalization
  FuncCallBack := nil;
  _CallBack.Free;

end.
