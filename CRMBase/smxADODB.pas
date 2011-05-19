unit smxADODB;

interface

uses
  Classes, DB, ADODB, smxDBIntf, smxBaseClasses;

type
  { TsmxADODatabase }

  TsmxADODatabase = class(TsmxInterfacedComponent, IsmxDatabase)
  private
    FDatabase: TADOConnection;
    FParams: TStrings;
  protected
    function GetConnected: Boolean;
    function GetDatabase: TObject;
    function GetDatabaseName: String;
    function GetDriverName: String;
    function GetInTransaction: Boolean;
    function GetLoginPrompt: Boolean;
    function GetParams: TStrings;
    function GetVersion: String; override;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String);
    procedure SetDriverName(const Value: String);
    procedure SetLoginPrompt(Value: Boolean);
    procedure SetParams(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CommitTransaction;
    function NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
    procedure RollbackTransaction;
    procedure StartTransaction;

    property Connected: Boolean read GetConnected write SetConnected;
    //property Database: TObject read GetDatabase;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property DriverName: String read GetDriverName write SetDriverName;
    property InTransaction: Boolean read GetInTransaction;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property Params: TStrings read GetParams write SetParams;
  end;

  { TsmxADOParam }

  TsmxADOParam = class(TsmxInterfacedComponent, IsmxParam)
  private
    FParam: TParameter;
  protected
    function GetDataType: TsmxDataType;
    function GetNumericScale: Integer;
    function GetParam: TObject;
    function GetParamName: String;
    function GetParamNo: Integer;
    function GetParamType: TsmxParamType;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    function GetVersion: String; override;
    procedure SetDataType(Value: TsmxDataType);
    procedure SetNumericScale(Value: Integer);
    procedure SetParamName(const Value: String);
    procedure SetParamType(Value: TsmxParamType);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(Value: Variant);
  public
    constructor Create(AParam: TParameter);
    destructor Destroy; override;
    //procedure AssignParam(Source: TObject);
    procedure AssignParam(const Source: IInterface);
    procedure LoadStream(Stream: TStream);

    property DataType: TsmxDataType read GetDataType write SetDataType;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    //property Param: TObject read GetParam;
    property ParamName: String read GetParamName write SetParamName;
    property ParamNo: Integer read GetParamNo;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { TsmxADODataSet }

  TsmxADODataSet = class(TsmxInterfacedComponent, IsmxDataSet)
  private
    FADODataSet: TCustomADODataSet;
    FDatabaseIntf: IsmxDatabase;
  protected
    function GetActive: Boolean;
    function GetBof: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDataSet: TObject; virtual;
    function GetDataSetType: TsmxDataSetType; virtual;
    function GetEof: Boolean;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetParam(Index: Integer): IsmxParam;
    function GetParamCount: Integer;
    function GetPrepare: Boolean;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    function GetSQL: TStrings; virtual;
    function GetVersion: String; override;
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPrepare(Value: Boolean);
    procedure SetRecordNo(Value: Integer);
    procedure SetSQL(Value: TStrings); virtual;
  public
    procedure Add; 
    function AddField(const FieldName: String): IsmxField;
    function AddParam: IsmxParam;
    procedure ClearFields;
    procedure ClearParams;
    //function CreateStreamField(const Value: IsmxField): TStream;
    procedure Close;
    procedure Execute; virtual;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    function FindParam(const Value: String): IsmxParam;
    procedure First;
    procedure Last;
    //procedure LoadStreamParam(const Value: IsmxParam; Stream: TStream);
    function Locate(const KeyFields: String; const KeyValues: Variant): Boolean;
    procedure Next;
    procedure Open;
    function ParamByName(const Value: String): IsmxParam;
    procedure Post;
    procedure Prepare; virtual;
    procedure Prior;
    procedure Remove;
    procedure RemoveField(const Value: IsmxField);
    procedure RemoveParam(const Value: IsmxParam);

    property Active: Boolean read GetActive write SetActive;
    property Bof: Boolean read GetBof;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    //property DataSet: TObject read GetDataSet;
    property DataSetType: TsmxDataSetType read GetDataSetType;
    property Eof: Boolean read GetEof;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: IsmxField read GetField write SetField;
    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
    property Prepared: Boolean read GetPrepare write SetPrepare;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
    property SQL: TStrings read GetSQL write SetSQL;
  end;

  { TsmxADOQuery }

  TsmxADOQuery = class(TsmxADODataSet)
  protected
    function GetDataSet: TObject; override;
    function GetDataSetType: TsmxDataSetType; override;
    function GetSQL: TStrings; override;
    procedure SetSQL(Value: TStrings); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Prepare; override;
  end;

  { TsmxADOStoredProc }

  TsmxADOStoredProc = class(TsmxADODataSet)
  private
    FSQL: TStrings;
  protected
    function GetDataSet: TObject; override;
    function GetDataSetType: TsmxDataSetType; override;
    function GetSQL: TStrings; override;
    procedure SetSQL(Value: TStrings); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Prepare; override;
  end;

function NewADODatabase: IsmxDatabase;
//function NewADODataSet: IsmxDataSet;

implementation

uses
  StrUtils, {ActiveX,} smxConsts, smxField;

const
  Vers = '1.0';

type
  { _TCustomADODataSet }

  _TCustomADODataSet = class(TCustomADODataSet)
  end;

function NewADODatabase: IsmxDatabase;
begin
  Result := TsmxADODatabase.Create;
end;

{function NewADODataSet: IsmxDataSet;
begin
  Result := TsmxADODataSet.Create;
end;}

{ TsmxADODatabase }

constructor TsmxADODatabase.Create;
begin
  inherited Create;
  FDatabase := TADOConnection.Create(nil);
  FParams := TStringList.Create;
end;

destructor TsmxADODatabase.Destroy;
begin
  FDatabase.Free;
  FParams.Free;
  inherited Destroy;
end;

procedure TsmxADODatabase.CommitTransaction;
begin
  FDatabase.CommitTrans;
end;

function TsmxADODatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TsmxADODatabase.GetDatabase: TObject;
begin
  Result := FDatabase;
end;

function TsmxADODatabase.GetDatabaseName: String;
begin
  Result := String(FDatabase.Name);
end;

function TsmxADODatabase.GetDriverName: String;
begin
  Result := String(FDatabase.Provider);
end;

function TsmxADODatabase.GetInTransaction: Boolean;
begin
  Result := FDatabase.InTransaction;
end;

function TsmxADODatabase.GetLoginPrompt: Boolean;
begin
  Result := FDatabase.LoginPrompt;
end;

function TsmxADODatabase.GetParams: TStrings;
begin
  Result := FParams;
  //FDatabase.ConnectionString := WideString(FParams.Text);
end;

function TsmxADODatabase.GetVersion: String;
begin
  Result := Vers;
end;

function TsmxADODatabase.NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
begin
  case DataSetType of
    dstQuery:
    begin
      Result := TsmxADOQuery.Create;
      Result.Database := Self;
    end;
    dstStoredProc:
    begin
      Result := TsmxADOStoredProc.Create;
      Result.Database := Self;
    end;
    else Result := nil;
  end;
end;

procedure TsmxADODatabase.RollbackTransaction;
begin
  FDatabase.RollbackTrans;
end;

procedure TsmxADODatabase.SetConnected(Value: Boolean);
begin
  if Value then
    FDatabase.ConnectionString := WideString(FParams.Text);
  FDatabase.Connected := Value;
end;

procedure TsmxADODatabase.SetDatabaseName(const Value: String);
begin
  FDatabase.Name := Value;
end;

procedure TsmxADODatabase.SetDriverName(const Value: String);
begin
  FDatabase.Provider := WideString(Value);
end;

procedure TsmxADODatabase.SetLoginPrompt(Value: Boolean);
begin
  FDatabase.LoginPrompt := Value;
end;

procedure TsmxADODatabase.SetParams(Value: TStrings);
begin
  //FParams := Value;
  FParams.Assign(Value);

  //FDatabase.ConnectionString := WideString(FParams.Text);
end;

procedure TsmxADODatabase.StartTransaction;
begin
  FDatabase.BeginTrans;
end;

{ TsmxADOParam }

constructor TsmxADOParam.Create(AParam: TParameter);
begin
  inherited Create;
  FParam := AParam;  
end;

destructor TsmxADOParam.Destroy;
begin
  FParam := nil;
  inherited Destroy;
end;

{procedure TsmxADOParam.AssignParam(Source: TObject);
begin
  FParam.Assign(TPersistent(Source));
end;}

procedure TsmxADOParam.AssignParam(const Source: IInterface);
var p: TObject; ParamIntf: IsmxParam; FieldIntf: IsmxField;
begin
  if Source.QueryInterface(IsmxParam, ParamIntf) = S_OK then
    //p := (Source as IsmxParam).GetParam else
    p := ParamIntf.GetParam else
  if Source.QueryInterface(IsmxField, FieldIntf) = S_OK then
    //p := (Source as IsmxField).GetField else
    p := FieldIntf.GetField else
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
    p := nil;
  {if p is TParameter then
    FParam.Assign(TParameter(p)) else
  if p is TField then
    FParam.Assign(TField(p)) else
  if p is TParam then
    FParam.Assign(TParam(p)) else
  if p is TStrings then
    FParam.Assign(TStrings(p))}
  if p is TPersistent then
    FParam.Assign(TPersistent(p)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
end;

function TsmxADOParam.GetDataType: TsmxDataType;
begin
  Result := FParam.DataType
end;

function TsmxADOParam.GetNumericScale: Integer;
begin
  Result := FParam.NumericScale;
end;

function TsmxADOParam.GetParam: TObject;
begin
  Result := FParam;
end;

function TsmxADOParam.GetParamName: String;
begin
  Result := FParam.Name;
end;

function TsmxADOParam.GetParamNo: Integer;
begin
  Result := FParam.Index;
end;

function TsmxADOParam.GetParamType: TsmxParamType;
const
  DirectionToType: array[TParameterDirection] of TsmxParamType =
    (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
begin
  {case FParam.Direction of
    pdUnknown: Result := ptUnknown;
    pdInput: Result := ptInput;
    pdOutput: Result := ptOutput;
    pdInputOutput: Result := ptInputOutput;
    pdReturnValue: Result := ptResult;
    else Result := ptUnknown;
  end;}
  Result := DirectionToType[FParam.Direction];
end;

function TsmxADOParam.GetPrecision: Integer;
begin
  Result := FParam.Precision;
end;

function TsmxADOParam.GetSize: Integer;
begin
  Result := FParam.Size;
end;

function TsmxADOParam.GetValue: Variant;
begin
  Result := FParam.Value;
end;

function TsmxADOParam.GetVersion: String;
begin
  Result := Vers;
end;

procedure TsmxADOParam.LoadStream(Stream: TStream);
begin
  FParam.LoadFromStream(Stream, ftBlob);
end;

procedure TsmxADOParam.SetDataType(Value: TsmxDataType);
begin
  FParam.DataType := Value;
end;

procedure TsmxADOParam.SetNumericScale(Value: Integer);
begin
  FParam.NumericScale := Value;
end;

procedure TsmxADOParam.SetParamName(const Value: String);
begin
  FParam.Name := WideString(Value);
end;

procedure TsmxADOParam.SetParamType(Value: TsmxParamType);
const
  TypeToDirection: array[TsmxParamType] of TParameterDirection =
    (pdUnknown, pdInput, pdOutput, pdInputOutput, pdReturnValue);
begin
  {case Value of
    ptUnknown: FParam.Direction := pdUnknown;
    ptInput: FParam.Direction := pdInput;
    ptOutput: FParam.Direction := pdOutput;
    ptInputOutput: FParam.Direction := pdInputOutput;
    ptResult: FParam.Direction := pdReturnValue;
    else FParam.Direction := pdUnknown;
  end;}
  FParam.Direction := TypeToDirection[Value];
end;

procedure TsmxADOParam.SetPrecision(Value: Integer);
begin
  FParam.Precision := Value;
end;

procedure TsmxADOParam.SetSize(Value: Integer);
begin
  FParam.Size := Value;
end;

procedure TsmxADOParam.SetValue(Value: Variant);
begin
  FParam.Value := Value;
end;

{ TsmxADODataSet }

procedure TsmxADODataSet.Add;
begin
  FADODataSet.Append;
end;

function TsmxADODataSet.AddField(const FieldName: String): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FADODataSet.FieldDefList.FieldByName(FieldName).CreateField(FADODataSet);
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

function TsmxADODataSet.AddParam: IsmxParam;
var p: TParameter;
begin
  Result := nil;
  p := _TCustomADODataSet(FADODataSet).Parameters.AddParameter;
  if Assigned(p) then
    Result := TsmxADOParam.Create(p);
end;

procedure TsmxADODataSet.ClearFields;
begin
  FADODataSet.Fields.Clear;
end;

procedure TsmxADODataSet.ClearParams;
begin
  _TCustomADODataSet(FADODataSet).Parameters.Clear;
end;

{function TsmxADODataSet.CreateStreamField(const Value: IsmxField): TStream;
begin
  if Value.Field is TField then
    Result := FADODataSet.CreateBlobStream(TField(Value.Field), bmRead) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeField);
end;}

procedure TsmxADODataSet.Close;
begin
  FADODataSet.Close;
end;

procedure TsmxADODataSet.Execute;
begin
end;

function TsmxADODataSet.FindParam(const Value: String): IsmxParam;
var p: TParameter;
begin
  Result := nil;
  p := _TCustomADODataSet(FADODataSet).Parameters.FindParam(WideString(Value));
  if Assigned(p) then
    Result := TsmxADOParam.Create(p);
end;

function TsmxADODataSet.FieldByName(const FieldName: String): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FADODataSet.FieldByName(FieldName);
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

function TsmxADODataSet.FindField(const FieldName: String): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FADODataSet.FindField(FieldName);
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

procedure TsmxADODataSet.First;
begin
  FADODataSet.First;
end;

function TsmxADODataSet.GetActive: Boolean;
begin
  Result := FADODataSet.Active;
end;

function TsmxADODataSet.GetBof: Boolean;
begin
  Result := FADODataSet.Bof;
end;

function TsmxADODataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

function TsmxADODataSet.GetDataSet: TObject;
begin
  Result := nil;
end;

function TsmxADODataSet.GetDataSetType: TsmxDataSetType;
begin
  Result := dstUnknown;
end;

function TsmxADODataSet.GetEof: Boolean;
begin
  Result := FADODataSet.Eof;
end;

function TsmxADODataSet.GetField(Index: Integer): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FADODataSet.Fields[Index];
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

function TsmxADODataSet.GetFieldCount: Integer;
begin
  Result := FADODataSet.FieldCount;
end;

function TsmxADODataSet.GetParam(Index: Integer): IsmxParam;
var p: TParameter;
begin
  Result := nil;
  p := _TCustomADODataSet(FADODataSet).Parameters[Index];
  if Assigned(p) then
    Result := TsmxADOParam.Create(p);
end;

function TsmxADODataSet.GetParamCount: Integer;
begin
  Result := _TCustomADODataSet(FADODataSet).Parameters.Count;
end;

function TsmxADODataSet.GetPrepare: Boolean;
begin
  Result := _TCustomADODataSet(FADODataSet).Prepared;
end;

function TsmxADODataSet.GetRecordNo: Integer;
begin
  Result := FADODataSet.RecNo;
end;

function TsmxADODataSet.GetRecordCount: Integer;
begin
  Result := FADODataSet.RecordCount;
end;

function TsmxADODataSet.GetSQL: TStrings;
begin
  Result := nil;
end;

function TsmxADODataSet.GetVersion: String;
begin
  Result := Vers;
end;

procedure TsmxADODataSet.Last;
begin
  FADODataSet.Last;
end;

{procedure TsmxADODataSet.LoadStreamParam(const Value: IsmxParam; Stream: TStream);
begin
  if Value.Param is TParameter then
    TParameter(Value.Param).LoadFromStream(Stream, ftBlob) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeParam);
end;}

function TsmxADODataSet.Locate(const KeyFields: String; const KeyValues: Variant): Boolean;
begin
  Result := FADODataSet.Locate(KeyFields, KeyValues, []);
end;

procedure TsmxADODataSet.Next;
begin
  FADODataSet.Next;
end;

procedure TsmxADODataSet.Open;
begin
  FADODataSet.Open;
end;

function TsmxADODataSet.ParamByName(const Value: String): IsmxParam;
var p: TParameter;
begin
  Result := nil;
  p := _TCustomADODataSet(FADODataSet).Parameters.ParamByName(WideString(Value));
  if Assigned(p) then
    Result := TsmxADOParam.Create(p);
end;

procedure TsmxADODataSet.Post;
begin
  FADODataSet.Post;
end;

procedure TsmxADODataSet.Prepare;
begin
end;

procedure TsmxADODataSet.Prior;
begin
  FADODataSet.Prior;
end;

procedure TsmxADODataSet.Remove;
begin
  FADODataSet.Delete;
end;

procedure TsmxADODataSet.RemoveField(const Value: IsmxField);
var f: TObject;
begin
  f := Value.GetField;
  if f is TField then
    FADODataSet.Fields.Remove(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);
end;

procedure TsmxADODataSet.RemoveParam(const Value: IsmxParam);
var p: TObject;
begin
  p := Value.GetParam;
  if p is TParameter then
    TParameter(p).Collection := nil else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
end;

procedure TsmxADODataSet.SetActive(Value: Boolean);
begin
  FADODataSet.Active := Value;
end;

procedure TsmxADODataSet.SetDatabase(const Value: IsmxDatabase);
var d: TObject;
begin
  if FDatabaseIntf <> Value then
  begin
    d := Value.GetDatabase;
    if d is TADOConnection then
    begin
      FADODataSet.Close;
      FADODataSet.Connection := TADOConnection(d);
      FDatabaseIntf := Value;
    end else
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
  end;
end;

procedure TsmxADODataSet.SetField(Index: Integer; const Value: IsmxField);
var f: TObject;
begin
  f := Value.GetField;
  if f is TField then
    FADODataSet.Fields[Index] := TField(f) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);
end;

procedure TsmxADODataSet.SetParam(Index: Integer; const Value: IsmxParam);
var p: TObject;
begin
  p := Value.GetParam;
  if p is TParameter then
    _TCustomADODataSet(FADODataSet).Parameters[Index] := TParameter(p) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
end;

procedure TsmxADODataSet.SetPrepare(Value: Boolean);
begin
  _TCustomADODataSet(FADODataSet).Prepared := Value;
end;

procedure TsmxADODataSet.SetRecordNo(Value: Integer);
begin
  FADODataSet.RecNo := Value;
end;

procedure TsmxADODataSet.SetSQL(Value: TStrings);
begin
end;

{ TsmxADOQuery }

constructor TsmxADOQuery.Create;
begin
  inherited Create;
  FADODataSet := TADOQuery.Create(nil);
end;

destructor TsmxADOQuery.Destroy;
begin
  FADODataSet.Free;
  inherited Destroy;
end;

procedure TsmxADOQuery.Execute;
begin
  TADOQuery(FADODataSet).ExecSQL;
end;

function TsmxADOQuery.GetDataSet: TObject;
begin
  Result := TADOQuery(FADODataSet);
end;

function TsmxADOQuery.GetDataSetType: TsmxDataSetType;
begin
  Result := dstQuery;
end;

function TsmxADOQuery.GetSQL: TStrings;
begin
  Result := TADOQuery(FADODataSet).SQL;
end;

procedure TsmxADOQuery.Prepare;
begin
  TADOQuery(FADODataSet).Prepared := True;
end;

procedure TsmxADOQuery.SetSQL(Value: TStrings);
begin
  TADOQuery(FADODataSet).SQL := Value;
end;

{ TsmxADOStoredProc }

constructor TsmxADOStoredProc.Create;
begin
  inherited Create;
  FADODataSet := TADOStoredProc.Create(nil);
  FSQL := TStringList.Create;
end;

destructor TsmxADOStoredProc.Destroy;
begin
  FSQL.Free;
  FADODataSet.Free;
  inherited Destroy;
end;

procedure TsmxADOStoredProc.Execute;
begin
  TADOStoredProc(FADODataSet).ExecProc;
end;

function TsmxADOStoredProc.GetDataSet: TObject;
begin
  Result := TADOStoredProc(FADODataSet);
end;

function TsmxADOStoredProc.GetDataSetType: TsmxDataSetType;
begin
  Result := dstStoredProc;
end;

function TsmxADOStoredProc.GetSQL: TStrings;
begin
  Result := FSQL;
  //TADOStoredProc(FADODataSet).ProcedureName := FSQL.Text;
end;

procedure TsmxADOStoredProc.Prepare;
begin
  TADOStoredProc(FADODataSet).ProcedureName :=
    WideString(AnsiReplaceStr(FSQL.Text, sLineBreak, ''));
  TADOStoredProc(FADODataSet).Parameters.Refresh;
  TADOStoredProc(FADODataSet).Prepared := True;
end;

procedure TsmxADOStoredProc.SetSQL(Value: TStrings);
begin
  //FSQL := Value;
  FSQL.Assign(Value);

  //TADOStoredProc(FADODataSet).ProcedureName := FSQL.Text;
end;

//initialization
  //CoInitialize(nil);

//finalization
  //CoUninitialize;

end.

