unit smxParams;

interface

uses
  Classes, SysUtils, smxClasses;

type
  { TsmxParam }

  TsmxParam = class(TsmxKitItem)
  private
    FParamName: String;
    FParamValue: Variant;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamName: String read FParamName write FParamName;
    property ParamValue: Variant read FParamValue write FParamValue;
  end;

  { TsmxParams }

  EsmxParamsError = class(Exception);

  TsmxParams = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxParam;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
    function GetValue(Name: String): Variant;
    procedure SetValue(Name: String; Value: Variant);
  public
    function Add: TsmxParam;
    function FindByName(AParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem {write SetItem}; default;
    property Values[Name: String]: Variant read GetValue write SetValue;
  end;

implementation

uses
  Variants, smxConsts;

{ TsmxParam }

constructor TsmxParam.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FParamValue := Null; //Unassigned;
  FParamName := '';
end;

{ TsmxParams }

function TsmxParams.Add: TsmxParam;
begin
  Result := TsmxParam(inherited Add);
end;

function TsmxParams.FindByName(AParamName: String): TsmxParam;
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

function TsmxParams.GetItem(Index: Integer): TsmxParam;
begin
  Result := TsmxParam(inherited Items[Index]);
end;

{procedure TsmxParams.SetItem(Index: Integer; Value: TsmxParam);
begin
  inherited Items[Index] := Value;
end;}

function TsmxParams.GetValue(Name: String): Variant;
var p: TsmxParam;
begin
  p := FindByName(Name);
  if Assigned(p) then
    Result := p.ParamValue else
    raise EsmxParamsError.CreateRes(@SParamsParamNotFound);
    //Result := Null; //Unassigned;
end;

procedure TsmxParams.SetValue(Name: String; Value: Variant);
var p: TsmxParam;
begin
  p := FindByName(Name);
  if Assigned(p) then
    p.ParamValue := Value else
    raise EsmxParamsError.CreateRes(@SParamsParamNotFound);
  {else
    with Add do
    begin
      ParamName := Name;
      ParamValue := Value;
    end;}
end;

end.
