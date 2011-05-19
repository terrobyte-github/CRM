unit smxParams;

interface

uses
  Classes, smxClasses;

type
  { TsmxParam }

  TsmxParam = class(TsmxKitItem)
  private
    FParamValue: Variant;
    FParamName: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property ParamValue: Variant read FParamValue write FParamValue;
    property ParamName: String read FParamName write FParamName;
  end;

  { TsmxParams }

  TsmxParams = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxParam;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxParam;
    function FindByName(AParamName: String): TsmxParam;

    property Items[Index: Integer]: TsmxParam read GetItem {write SetItem}; default;
  end;

implementation

uses
  Variants, SysUtils;

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

end.
 