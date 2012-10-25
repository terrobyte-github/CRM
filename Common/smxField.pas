unit smxField;

interface

uses
  Classes, DB, smxBaseClasses, smxDBIntf{, smxTypes};

type
  { TsmxSimpleItem }

  TsmxSimpleKit = class;

  TsmxSimpleItem = class(TObject)
  private
    FKit: TsmxSimpleKit;
    function GetItemIndex: Integer;
  public
    constructor Create(AKit: TsmxSimpleKit);

    property ItemIndex: Integer read GetItemIndex;
  end;

  TsmxSimpleItemClass = class of TsmxSimpleItem;

  { TsmxSimpleKit }

  TsmxSimpleKit = class(TObject)
  private
    FList: TList;
    FItemClass: TsmxSimpleItemClass;
    function GetCount: Integer;
    function GetItem(Index: Integer): TsmxSimpleItem;
  public
    constructor Create(AItemClass: TsmxSimpleItemClass);
    destructor Destroy; override;
    function Add: TsmxSimpleItem;
    procedure Delete(AIndex: Integer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TsmxSimpleItem read GetItem; default;
  end;

  { TsmxSenseItem }

  TsmxSenseItem = class(TsmxSimpleItem)
  private
    FFieldSense: TsmxFieldSense;
    FFieldName: String;
  public
    property FieldSense: TsmxFieldSense read FFieldSense write FFieldSense;
    property FieldName: String read FFieldName write FFieldName;
  end;

  { TsmxSenseKit }

  TsmxSenseKit = class(TsmxSimpleKit)
  private
    function GetFieldSense(FieldName: String): TsmxFieldSense;
    procedure SetFieldSense(FieldName: String; Value: TsmxFieldSense);
    function GetItem(Index: Integer): TsmxSenseItem;
  public
    function Add: TsmxSenseItem;
    function FindByName(AFieldName: String): TsmxSenseItem;

    property FieldSense[FieldName: String]: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property Items[Index: Integer]: TsmxSenseItem read GetItem; default;
  end;

  { TsmxLocationItem }

  TsmxLocationItem = class(TsmxSimpleItem)
  private
    FParamLocation: TsmxParamLocation;
    FParamName: String;
  public
    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamName: String read FParamName write FParamName;
  end;

  { TsmxLocationKit }

  TsmxLocationKit = class(TsmxSimpleKit)
  private
    function GetParamLocation(ParamName: String): TsmxParamLocation;
    procedure SetParamLocation(ParamName: String; Value: TsmxParamLocation);
    function GetItem(Index: Integer): TsmxLocationItem;
  public
    function Add: TsmxLocationItem;
    function FindByName(AParamName: String): TsmxLocationItem;

    property ParamLocation[ParamName: String]: TsmxParamLocation read GetParamLocation write SetParamLocation;
    property Items[Index: Integer]: TsmxLocationItem read GetItem; default;
  end;

  { TsmxCustomField }

  TsmxCustomDataSet = class;

  TsmxCustomField = class(TsmxInterfacedComponent)
  private
    FInternalDataSet: TsmxCustomDataSet;
  protected
    //function GetDataSet: IsmxDataSet;
    function GetFieldSense: TsmxFieldSense;
    procedure SetFieldSense(Value: TsmxFieldSense);
    function GetFieldName: String;
    procedure SetFieldName(const Value: String);

    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet;
  public
    constructor Create(ADataSet: TsmxCustomDataSet);

    //property DataSet: IsmxDataSet read GetDataSet;
    //property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    //property FieldName: String read GetFieldName write SetFieldName;
  end;

  { TsmxCustomParam }

  TsmxCustomParam = class(TsmxInterfacedComponent)
  private
    FInternalDataSet: TsmxCustomDataSet;
  protected
    //function GetDataSet: IsmxDataSet;
    function GetParamLocation: TsmxParamLocation;
    procedure SetParamLocation(Value: TsmxParamLocation);
    function GetParamName: String;
    procedure SetParamName(const Value: String);

    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet;
  public
    constructor Create(ADataSet: TsmxCustomDataSet);

    //property DataSet: IsmxDataSet read GetDataSet;
    //property ParamLocation: TsmxParamLocation read GetParamLocation write SetParamLocation;
    //property ParamName: String read GetParamName write SetParamName;
  end;

  { TsmxCustomDataSet }

  TsmxCustomDataSet = class(TsmxInterfacedComponent)
  private
    FDataSetType: TsmxDataSetType;
    FSenseList: TsmxSenseKit;
    FLocationList: TsmxLocationKit;
    //function GetSenseList: TsmxSenseKit;
    //function GetLocationList: TsmxLocationKit;
  protected
    function GetDataSetType: TsmxDataSetType;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetParam(Index: Integer): IsmxParam;
    function GetParamCount: Integer;
    //procedure SetField(Index: Integer; const Value: IsmxField);
    //procedure SetParam(Index: Integer; const Value: IsmxParam);

    property SenseList: TsmxSenseKit read FSenseList; //GetSenseList;
    property LocationList: TsmxLocationKit read FLocationList; //GetLocationList;
  public
    constructor Create(ADataSetType: TsmxDataSetType);
    destructor Destroy; override;
    function FieldBySense(ASense: TsmxFieldSense; var AFields: TsmxFieldArray): Integer;
    function ParamByLocation(ALocation: TsmxParamLocation; var AParams: TsmxParamArray): Integer;


    //property DataSetType: TsmxDataSetType read GetDataSetType;
    //property FieldCount: Integer read GetFieldCount;
    //property Fields[Index: Integer]: IsmxField read GetField write SetField;
    //property ParamCount: Integer read GetParamCount;
    //property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
  end;

  { TsmxField }

  TsmxField = class(TsmxCustomField, IsmxField)
  private
    //FDataSet: TObject;
    FField: TField;
    //FStream: TStream;
    //function GetStream: TStream;
    //FSense: TsmxFieldSense;
  protected
    function GetDataSet: IsmxDataSet;
    //function GetCalculated: Boolean;
    function GetDataType: TsmxDataType;
    //function GetDefaultExpression: String;
    function GetDisplayFormat: String;
    function GetField: TObject;
    //function GetFieldKind: TsmxFieldKind;
    function GetFieldName: String;
    function GetFieldNo: Integer;
    //function GetFieldSense: TsmxFieldSense;
    function GetSize: Integer;
    function GetValue: Variant;
    //function GetIsBlob: Boolean;
    //function GetVersion: String; override;
    //procedure SetCalculated(Value: Boolean);
    //procedure SetDefaultExpression(Value: String);
    procedure SetDisplayFormat(Value: String);
    //procedure SetFieldKind(Value: TsmxFieldKind);
    procedure SetFieldName(Value: String);
    //procedure SetFieldSense(Value: TsmxFieldSense);
    procedure SetValue(Value: Variant);

    //property Stream: TStream read GetStream;
  public
    constructor Create(AField: TField; ADataSet: TsmxCustomDataSet);
    //destructor Destroy; override;
    //procedure AssignField(Source: TObject);
    procedure AssignField(const Source: IsmxField);
    //function CreateStream: TStream;
    procedure Clear;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property DataSet: IsmxDataSet read GetDataSet;
    //property Calculated: Boolean read GetCalculated write SetCalculated;
    property DataType: TsmxDataType read GetDataType;
    //property DefaultExpression: String read GetDefaultExpression write SetDefaultExpression;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    //property Field: TObject read GetField;
    //property FieldKind: TsmxFieldKind read GetFieldKind write SetFieldKind;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldNo: Integer read GetFieldNo;
    property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property Size: Integer read GetSize;
    property Value: Variant read GetValue write SetValue;
    //property IsBlob: Boolean read GetIsBlob;
  end;

implementation

uses
  SysUtils, smxConsts, smxProcs;

//const
  //Vers = '1.0';

{ TsmxField }

constructor TsmxField.Create(AField: TField; ADataSet: TsmxCustomDataSet);
begin
  inherited Create(ADataSet);
  FField := AField;
  //FSense := fsGeneral;
  //FDataSet := ADataSet;
end;

{destructor TsmxField.Destroy;
begin
  FDataSet := nil;
  FField := nil;
  //if Assigned(FStream) then
    //FStream.Free;
  inherited Destroy;
end;}

{procedure TsmxField.AssignField(Source: TObject);
begin
  FField.Assign(TPersistent(Source));
end;}

procedure TsmxField.AssignField(const Source: IsmxField);
//var f: TObject;
begin
  {f := Source.GetField;
  if f is TField then
    FField.Assign(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  if Assigned(Source) then
  begin
    Value := Source.Value;
    FieldSense := Source.FieldSense;
  end else
    Clear;
end;

{function TsmxField.CreateStream: TStream;
begin
  Result := FField.DataSet.CreateBlobStream(FField, bmRead);
end;}

procedure TsmxField.SaveToStream(Stream: TStream);
//var
  //s: TStream;
begin
  {s := FField.DataSet.CreateBlobStream(FField, bmRead);
  try
    Stream.CopyFrom(s, 0);
  finally
    s.Free;
  end;}
  smxProcs.StrToStream(FField.Value, Stream);
end;

{function TsmxField.GetCalculated: Boolean;
begin
  Result := FField.Calculated;
end;}

function TsmxField.GetDataType: TsmxDataType;
begin
  Result := FField.DataType;
end;

{function TsmxField.GetDefaultExpression: String;
begin
  Result := FField.DefaultExpression;
end;}

function TsmxField.GetDisplayFormat: String;
begin
  if FField is TAggregateField then
    Result := TAggregateField(FField).DisplayFormat else
  if FField is TDateTimeField then
    Result := TDateTimeField(FField).DisplayFormat else
  if FField is TNumericField then
    Result := TNumericField(FField).DisplayFormat else
  if FField is TSQLTimeStampField then
    Result := TSQLTimeStampField(FField).DisplayFormat else
    Result := '';
end;

function TsmxField.GetField: TObject;
begin
  Result := FField;
end;

{function TsmxField.GetFieldKind: TsmxFieldKind;
begin
  Result := FField.FieldKind;
end;}

function TsmxField.GetFieldName: String;
begin
  Result := FField.FieldName;
end;

function TsmxField.GetFieldNo: Integer;
begin
  Result := FField.FieldNo;
end;

function TsmxField.GetSize: Integer;
begin
  Result := FField.Size;
end;

function TsmxField.GetValue: Variant;
begin
  Result := FField.Value;
end;

{function TsmxField.GetVersion: String;
begin
  Result := Vers;
end;}

{procedure TsmxField.SetCalculated(Value: Boolean);
begin
  FField.Calculated := Value;
end;}

{procedure TsmxField.SetDefaultExpression(Value: String);
begin
  FField.DefaultExpression := Value;
end;}

procedure TsmxField.SetDisplayFormat(Value: String);
begin
  if FField is TAggregateField then
    TAggregateField(FField).DisplayFormat := Value else
  if FField is TDateTimeField then
    TDateTimeField(FField).DisplayFormat := Value else
  if FField is TNumericField then
    TNumericField(FField).DisplayFormat := Value else
  if FField is TSQLTimeStampField then
    TSQLTimeStampField(FField).DisplayFormat := Value;
end;

{procedure TsmxField.SetFieldKind(Value: TsmxFieldKind);
begin
  FField.FieldKind := Value;
end;}

procedure TsmxField.SetFieldName(Value: String);
begin
  inherited SetFieldName(Value);
  FField.FieldName := Value;
end;

procedure TsmxField.SetValue(Value: Variant);
begin
  FField.Value := Value;
end;

{function TsmxField.GetFieldSense: TsmxFieldSense;
begin
  Result := fsGeneral;
end;}

{procedure TsmxField.SetFieldSense(Value: TsmxFieldSense);
begin
end;}

{function TsmxField.GetIsBlob: Boolean;
begin
  Result := FField.IsBlob;
end;}

procedure TsmxField.LoadFromStream(Stream: TStream);
var
  Str: String;
begin
  smxProcs.StreamToStr(Stream, Str);
  FField.Value := Str;
end;

{function TsmxField.GetStream: TStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;
  Result := FStream;
end;}

procedure TsmxField.Clear;
begin
  FField.Clear;
end;

function TsmxField.IsBlob: Boolean;
begin
  Result := FField.IsBlob;
end;

function TsmxField.IsNull: Boolean;
begin
  Result := FField.IsNull;
end;

function TsmxField.GetDataSet: IsmxDataSet;
begin
  InternalDataSet.GetInterface(IsmxDataSet, Result);
end;

{ TsmxSimpleItem }

constructor TsmxSimpleItem.Create(AKit: TsmxSimpleKit);
begin
  inherited Create;
  FKit := AKit;
end;

function TsmxSimpleItem.GetItemIndex: Integer;
begin
  Result := FKit.FList.IndexOf(Self);
end;

{ TsmxSimpleKit }

constructor TsmxSimpleKit.Create(AItemClass: TsmxSimpleItemClass);
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TsmxSimpleKit.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TsmxSimpleKit.Add: TsmxSimpleItem;
begin
  Result := FItemClass.Create(Self);
  FList.Add(Result);
end;

procedure TsmxSimpleKit.Clear;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    TsmxSimpleItem(FList[i]).Free;
    FList.Delete(i);
  end;
end;

procedure TsmxSimpleKit.Delete(AIndex: Integer);
var
  Item: TsmxSimpleItem;
begin
  Item := FList[AIndex];
  FList.Delete(AIndex);
  if Assigned(Item) then
    Item.Free;
end;

function TsmxSimpleKit.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TsmxSimpleKit.GetItem(Index: Integer): TsmxSimpleItem;
begin
  Result := TsmxSimpleItem(FList[Index]);
end;

{ TsmxSenseKit }

function TsmxSenseKit.FindByName(AFieldName: String): TsmxSenseItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    if SysUtils.AnsiCompareText(TsmxSenseItem(FList[i]).FieldName, AFieldName) = 0 then
    begin
      Result := FList[i];
      Break;
    end;
end;

function TsmxSenseKit.GetFieldSense(FieldName: String): TsmxFieldSense;
var
  Item: TsmxSenseItem;
begin
  Item := FindByName(FieldName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.FieldName := FieldName;
  end;
  Result := Item.FieldSense;
end;

function TsmxSenseKit.Add: TsmxSenseItem;
begin
  Result := TsmxSenseItem(inherited Add);
end;

procedure TsmxSenseKit.SetFieldSense(FieldName: String; Value: TsmxFieldSense);
var
  Item: TsmxSenseItem;
begin
  Item := FindByName(FieldName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.FieldName := FieldName;
  end;
  Item.FieldSense := Value;
end;

function TsmxSenseKit.GetItem(Index: Integer): TsmxSenseItem;
begin
  Result := TsmxSenseItem(inherited Items[Index]);
end;

{ TsmxLocationKit }

function TsmxLocationKit.Add: TsmxLocationItem;
begin
  Result := TsmxLocationItem(inherited Add);
end;

function TsmxLocationKit.FindByName(AParamName: String): TsmxLocationItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    if SysUtils.AnsiCompareText(TsmxLocationItem(FList[i]).ParamName, AParamName) = 0 then
    begin
      Result := FList[i];
      Break;
    end;
end;

function TsmxLocationKit.GetItem(Index: Integer): TsmxLocationItem;
begin
  Result := TsmxLocationItem(inherited Items[Index]);
end;

function TsmxLocationKit.GetParamLocation(ParamName: String): TsmxParamLocation;
var
  Item: TsmxLocationItem;
begin
  Item := FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.ParamName := ParamName;
  end;
  Result := Item.ParamLocation;
end;

procedure TsmxLocationKit.SetParamLocation(ParamName: String; Value: TsmxParamLocation);
var
  Item: TsmxLocationItem;
begin
  Item := FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.ParamName := ParamName;
  end;
  Item.ParamLocation := Value;
end;

{ TsmxCustomField }

constructor TsmxCustomField.Create(ADataSet: TsmxCustomDataSet);
begin
  inherited Create;
  FInternalDataSet := ADataSet;
end;

{function TsmxCustomField.GetDataSet: IsmxDataSet;
begin
  FInternalDataSet.GetInterface(IsmxDataSet, Result);
end;}

function TsmxCustomField.GetFieldSense: TsmxFieldSense;
begin
  Result := FInternalDataSet.SenseList.FieldSense[GetFieldName];
end;

function TsmxCustomField.GetFieldName: String;
begin
  Result := '';
end;

procedure TsmxCustomField.SetFieldSense(Value: TsmxFieldSense);
begin
  FInternalDataSet.SenseList.FieldSense[GetFieldName] := Value;
end;

procedure TsmxCustomField.SetFieldName(const Value: String);
var
  Item: TsmxSenseItem;
begin
  if GetFieldName <> Value then
  begin
    Item := FInternalDataSet.SenseList.FindByName(GetFieldName);
    if Assigned(Item) then
      Item.FieldName := Value;
  end;
end;

{ TsmxCustomParam }

constructor TsmxCustomParam.Create(ADataSet: TsmxCustomDataSet);
begin
  inherited Create;
  FInternalDataSet := ADataSet;
end;

{function TsmxCustomParam.GetDataSet: IsmxDataSet;
begin
  FInternalDataSet.GetInterface(IsmxDataSet, Result);
end;}

function TsmxCustomParam.GetParamLocation: TsmxParamLocation;
begin
  Result := FInternalDataSet.LocationList.ParamLocation[GetParamName];
end;

function TsmxCustomParam.GetParamName: String;
begin
  Result := '';
end;

procedure TsmxCustomParam.SetParamLocation(Value: TsmxParamLocation);
begin
  FInternalDataSet.LocationList.ParamLocation[GetParamName] := Value;
end;

procedure TsmxCustomParam.SetParamName(const Value: String);
var
  Item: TsmxLocationItem;
begin
  if GetParamName <> Value then
  begin
    Item := FInternalDataSet.LocationList.FindByName(GetParamName);
    if Assigned(Item) then
      Item.ParamName := Value;
  end;
end;

{ TsmxCustomDataSet }

constructor TsmxCustomDataSet.Create(ADataSetType: TsmxDataSetType);
begin
  inherited Create;
  FDataSetType := ADataSetType;
  FSenseList := TsmxSenseKit.Create(TsmxSenseItem);
  FLocationList := TsmxLocationKit.Create(TsmxLocationItem);
end;

destructor TsmxCustomDataSet.Destroy;
begin
  //if Assigned(FSenseList) then
  FSenseList.Free;
  //if Assigned(FLocationList) then
  FLocationList.Free;
  inherited Destroy;
end;

function TsmxCustomDataSet.FieldBySense(ASense: TsmxFieldSense;
  var AFields: TsmxFieldArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AFields, 0);
  for i := 0 to GetFieldCount - 1 do
    if GetField(i).FieldSense = ASense then
    begin
      Inc(Result);
      SetLength(AFields, Result);
      AFields[Result - 1] := GetField(i);
    end;
end;

function TsmxCustomDataSet.GetDataSetType: TsmxDataSetType;
begin
  Result := FDataSetType;
end;

function TsmxCustomDataSet.GetField(Index: Integer): IsmxField;
begin
  Result := nil;
end;

function TsmxCustomDataSet.GetFieldCount: Integer;
begin
  Result := 0;
end;

{function TsmxCustomDataSet.GetLocationList: TsmxLocationKit;
begin
  if not Assigned(FLocationList) then
    FLocationList := TsmxLocationKit.Create(TsmxLocationItem);
  Result := FLocationList;
end;}

function TsmxCustomDataSet.GetParam(Index: Integer): IsmxParam;
begin
  Result := nil;
end;

function TsmxCustomDataSet.GetParamCount: Integer;
begin
  Result := 0;
end;

{function TsmxCustomDataSet.GetSenseList: TsmxSenseKit;
begin
  if not Assigned(FSenseList) then
    FSenseList := TsmxSenseKit.Create(TsmxSenseItem);
  Result := FSenseList;
end;}

function TsmxCustomDataSet.ParamByLocation(ALocation: TsmxParamLocation;
  var AParams: TsmxParamArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AParams, 0);
  for i := 0 to GetParamCount - 1 do
    if GetParam(i).ParamLocation = ALocation then
    begin
      Inc(Result);
      SetLength(AParams, Result);
      AParams[Result - 1] := GetParam(i);
    end;
end;

{procedure TsmxCustomDataSet.SetField(Index: Integer; const Value: IsmxField);
begin
end;

procedure TsmxCustomDataSet.SetParam(Index: Integer; const Value: IsmxParam);
begin
end;}

end.
