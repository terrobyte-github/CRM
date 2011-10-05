unit smxField;

interface

uses
  Classes, DB, smxBaseClasses, smxDBIntf, smxTypes;

type
  { TsmxField }

  TsmxField = class(TsmxInterfacedComponent, IsmxField)
  private
    FField: TField;
  protected
    function GetCalculated: Boolean;
    function GetDataType: TsmxDataType;
    function GetDefaultExpression: String;
    function GetDisplayFormat: String;
    function GetField: TObject;
    function GetFieldKind: TsmxFieldKind;
    function GetFieldName: String;
    function GetFieldNo: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    function GetFieldSense: TsmxFieldSense;
    function GetVersion: String; override;
    procedure SetCalculated(Value: Boolean);
    procedure SetDefaultExpression(Value: String);
    procedure SetDisplayFormat(Value: String);
    procedure SetFieldKind(Value: TsmxFieldKind);
    procedure SetFieldName(Value: String);
    procedure SetValue(Value: Variant);
    procedure SetFieldSense(Value: TsmxFieldSense);
  public
    constructor Create(AField: TField);
    destructor Destroy; override;
    //procedure AssignField(Source: TObject);
    procedure AssignField(const Source: IsmxField);
    //function CreateStream: TStream;
    procedure SaveToStream(Stream: TStream);

    property Calculated: Boolean read GetCalculated write SetCalculated;
    property DataType: TsmxDataType read GetDataType;
    property DefaultExpression: String read GetDefaultExpression write SetDefaultExpression;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    //property Field: TObject read GetField;
    property FieldKind: TsmxFieldKind read GetFieldKind write SetFieldKind;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldNo: Integer read GetFieldNo;
    property Size: Integer read GetSize;
    property Value: Variant read GetValue write SetValue;
    property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
  end;

implementation

uses
  smxConsts;

const
  Vers = '1.0';

{ TsmxField }

constructor TsmxField.Create(AField: TField);
begin
  inherited Create;
  FField := AField;
end;

destructor TsmxField.Destroy;
begin
  FField := nil;
  inherited Destroy;
end;

{procedure TsmxField.AssignField(Source: TObject);
begin
  FField.Assign(TPersistent(Source));
end;}

procedure TsmxField.AssignField(const Source: IsmxField);
var f: TObject;
begin
  f := Source.GetField;
  if f is TField then
    FField.Assign(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);
end;

{function TsmxField.CreateStream: TStream;
begin
  Result := FField.DataSet.CreateBlobStream(FField, bmRead);
end;}

procedure TsmxField.SaveToStream(Stream: TStream);
var
  s: TStream;
begin
  s := FField.DataSet.CreateBlobStream(FField, bmRead);
  try
    Stream.CopyFrom(s, 0);
  finally
    s.Free;
  end;
end;

function TsmxField.GetCalculated: Boolean;
begin
  Result := FField.Calculated;
end;

function TsmxField.GetDataType: TsmxDataType;
begin
  Result := FField.DataType;
end;

function TsmxField.GetDefaultExpression: String;
begin
  Result := FField.DefaultExpression;
end;

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

function TsmxField.GetFieldKind: TsmxFieldKind;
begin
  Result := FField.FieldKind;
end;

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

function TsmxField.GetVersion: String;
begin
  Result := Vers;
end;

procedure TsmxField.SetCalculated(Value: Boolean);
begin
  FField.Calculated := Value;
end;

procedure TsmxField.SetDefaultExpression(Value: String);
begin
  FField.DefaultExpression := Value;
end;

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

procedure TsmxField.SetFieldKind(Value: TsmxFieldKind);
begin
  FField.FieldKind := Value;
end;

procedure TsmxField.SetFieldName(Value: String);
begin
  FField.FieldName := Value;
end;

procedure TsmxField.SetValue(Value: Variant);
begin
  FField.Value := Value;
end;

function TsmxField.GetFieldSense: TsmxFieldSense;
begin
  Result := fsGeneral;
end;

procedure TsmxField.SetFieldSense(Value: TsmxFieldSense);
begin  
end;

end.
