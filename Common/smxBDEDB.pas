unit smxBDEDB;

interface

uses
  Classes, DB, DBTables, smxDBIntf, smxBaseClasses, smxTypes;

type
  { TsmxBDEDatabase }

  TsmxBDEDatabase = class(TsmxInterfacedComponent, IsmxDatabase)
  private
    FDatabase: TDatabase;
    //FParams: TStrings;
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

  { TsmxBDEParam }

  TsmxBDEParam = class(TsmxInterfacedComponent, IsmxParam)
  private
    FParam: TParam;
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
    function GetParamLocation: TsmxParamLocation;
    function GetVersion: String; override;
    procedure SetDataType(Value: TsmxDataType);
    procedure SetNumericScale(Value: Integer);
    procedure SetParamName(const Value: String);
    procedure SetParamType(Value: TsmxParamType);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(Value: Variant);
    procedure SetParamLocation(Value: TsmxParamLocation);
  public
    constructor Create(AParam: TParam);
    destructor Destroy; override;
    //procedure AssignParam(Source: TObject);
    procedure AssignParam(const Source: IInterface);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property DataType: TsmxDataType read GetDataType write SetDataType;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    //property Param: TObject read GetParam;
    property ParamName: String read GetParamName write SetParamName;
    property ParamNo: Integer read GetParamNo;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
    property ParamLocation: TsmxParamLocation read GetParamLocation write SetParamLocation;
  end;

  { TsmxBDEDataSet }

  TsmxBDEDataSet = class(TsmxInterfacedComponent, IsmxDataSet)
  private
    FBDEDataSet: TDBDataSet;
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
    //function GetIsDataSet: Boolean;
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
    //property IsDataSet: Boolean read GetIsDataSet;
    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
    property Prepared: Boolean read GetPrepare write SetPrepare;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
    property SQL: TStrings read GetSQL write SetSQL;
  end;

  { TsmxBDEQuery }

  TsmxBDEQuery = class(TsmxBDEDataSet)
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

  { TsmxBDEStoredProc }

  TsmxBDEStoredProc = class(TsmxBDEDataSet)
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

function NewBDEDatabase: IsmxDatabase;
//function NewADODataSet: IsmxDataSet;

implementation

uses
  StrUtils, {ActiveX,} smxConsts, smxField;

const
  Vers = '1.0';

//type
  { _TCustomADODataSet }

  //_TCustomADODataSet = class(TCustomADODataSet)
  //end;

function NewBDEDatabase: IsmxDatabase;
begin
  Result := TsmxBDEDatabase.Create;
end;

{function NewADODataSet: IsmxDataSet;
begin
  Result := TsmxBDEDataSet.Create;
end;}

{ TsmxBDEDatabase }

constructor TsmxBDEDatabase.Create;
begin
  inherited Create;
  FDatabase := TDatabase.Create(nil);
  FDatabase.HandleShared := True;
  //FParams := TStringList.Create;
end;

destructor TsmxBDEDatabase.Destroy;
begin
  FDatabase.Free;
  //FParams.Free;
  inherited Destroy;
end;

procedure TsmxBDEDatabase.CommitTransaction;
begin
  FDatabase.Commit;
end;

function TsmxBDEDatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TsmxBDEDatabase.GetDatabase: TObject;
begin
  Result := FDatabase;
end;

function TsmxBDEDatabase.GetDatabaseName: String;
begin
  Result := FDatabase.DatabaseName;
end;

function TsmxBDEDatabase.GetDriverName: String;
begin
  Result := FDatabase.DriverName;
end;

function TsmxBDEDatabase.GetInTransaction: Boolean;
begin
  Result := FDatabase.InTransaction;
end;

function TsmxBDEDatabase.GetLoginPrompt: Boolean;
begin
  Result := FDatabase.LoginPrompt;
end;

function TsmxBDEDatabase.GetParams: TStrings;
begin
  Result := FDatabase.Params; //FParams;
  //FDatabase.ConnectionString := WideString(FParams.Text);
end;

function TsmxBDEDatabase.GetVersion: String;
begin
  Result := Vers;
end;

function TsmxBDEDatabase.NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
begin
  Result := nil;
  case DataSetType of
    dstQuery: Result := TsmxBDEQuery.Create;
    dstStoredProc: Result := TsmxBDEStoredProc.Create;
    else
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfDataSetUnknown);
  end;
  {if Assigned(Result) then
    Result.Database := Self else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDataSetInvalid);}
end;

procedure TsmxBDEDatabase.RollbackTransaction;
begin
  FDatabase.Rollback;
end;

procedure TsmxBDEDatabase.SetConnected(Value: Boolean);
begin
  //if Value then
    //FDatabase.ConnectionString := WideString(FParams.Text);
  FDatabase.Connected := Value;
end;

procedure TsmxBDEDatabase.SetDatabaseName(const Value: String);
begin
  FDatabase.DatabaseName := Value;
end;

procedure TsmxBDEDatabase.SetDriverName(const Value: String);
begin
  //FDatabase.Provider := WideString(Value);
  FDatabase.DriverName := Value;
end;

procedure TsmxBDEDatabase.SetLoginPrompt(Value: Boolean);
begin
  FDatabase.LoginPrompt := Value;
end;

procedure TsmxBDEDatabase.SetParams(Value: TStrings);
begin
  //FParams := Value;
  //FParams.Assign(Value);
  FDatabase.Params := Value;

  //FDatabase.ConnectionString := WideString(FParams.Text);
end;

procedure TsmxBDEDatabase.StartTransaction;
begin
  FDatabase.StartTransaction;
end;

{ TsmxBDEParam }

constructor TsmxBDEParam.Create(AParam: TParam);
begin
  inherited Create;
  FParam := AParam;
end;

destructor TsmxBDEParam.Destroy;
begin
  FParam := nil;
  inherited Destroy;
end;

{procedure TsmxBDEParam.AssignParam(Source: TObject);
begin
  FParam.Assign(TPersistent(Source));
end;}

procedure TsmxBDEParam.AssignParam(const Source: IInterface);
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

function TsmxBDEParam.GetDataType: TsmxDataType;
begin
  Result := FParam.DataType;
end;

function TsmxBDEParam.GetNumericScale: Integer;
begin
  Result := FParam.NumericScale;
end;

function TsmxBDEParam.GetParam: TObject;
begin
  Result := FParam;
end;

function TsmxBDEParam.GetParamName: String;
begin
  Result := FParam.Name;
end;

function TsmxBDEParam.GetParamNo: Integer;
begin
  Result := FParam.Index;
end;

function TsmxBDEParam.GetParamType: TsmxParamType;
//const
  //DirectionToType: array[TParameterDirection] of TsmxParamType =
    //(ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
begin
  {case FParam.Direction of
    pdUnknown: Result := ptUnknown;
    pdInput: Result := ptInput;
    pdOutput: Result := ptOutput;
    pdInputOutput: Result := ptInputOutput;
    pdReturnValue: Result := ptResult;
    else Result := ptUnknown;
  end;}
  Result := TsmxParamType(FParam.ParamType); //DirectionToType[FParam.Direction];
end;

function TsmxBDEParam.GetPrecision: Integer;
begin
  Result := FParam.Precision;
end;

function TsmxBDEParam.GetSize: Integer;
begin
  Result := FParam.Size;
end;

function TsmxBDEParam.GetValue: Variant;
begin
  Result := FParam.Value;
end;

function TsmxBDEParam.GetVersion: String;
begin
  Result := Vers;
end;

procedure TsmxBDEParam.LoadFromStream(Stream: TStream);
begin
  FParam.LoadFromStream(Stream, ftBlob);
end;

procedure TsmxBDEParam.SetDataType(Value: TsmxDataType);
begin
  FParam.DataType := Value;
end;

procedure TsmxBDEParam.SetNumericScale(Value: Integer);
begin
  FParam.NumericScale := Value;
end;

procedure TsmxBDEParam.SetParamName(const Value: String);
begin
  FParam.Name := WideString(Value);
end;

procedure TsmxBDEParam.SetParamType(Value: TsmxParamType);
//const
  //TypeToDirection: array[TsmxParamType] of TParameterDirection =
    //(pdUnknown, pdInput, pdOutput, pdInputOutput, pdReturnValue);
begin
  {case Value of
    ptUnknown: FParam.Direction := pdUnknown;
    ptInput: FParam.Direction := pdInput;
    ptOutput: FParam.Direction := pdOutput;
    ptInputOutput: FParam.Direction := pdInputOutput;
    ptResult: FParam.Direction := pdReturnValue;
    else FParam.Direction := pdUnknown;
  end;}
  //FParam.Direction := TypeToDirection[Value];
  FParam.ParamType := TParamType(Value);
end;

procedure TsmxBDEParam.SetPrecision(Value: Integer);
begin
  FParam.Precision := Value;
end;

procedure TsmxBDEParam.SetSize(Value: Integer);
begin
  FParam.Size := Value;
end;

procedure TsmxBDEParam.SetValue(Value: Variant);
begin
  FParam.Value := Value;
end;

procedure TsmxBDEParam.SaveToStream(Stream: TStream);
begin
  //
end;

function TsmxBDEParam.GetParamLocation: TsmxParamLocation;
begin
  Result := plConst;
end;

procedure TsmxBDEParam.SetParamLocation(Value: TsmxParamLocation);
begin               
end;

{ TsmxBDEDataSet }

procedure TsmxBDEDataSet.Add;
begin
  FBDEDataSet.Append;
end;

function TsmxBDEDataSet.AddField(const FieldName: String): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FBDEDataSet.FieldDefList.FieldByName(FieldName).CreateField(FBDEDataSet);
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

function TsmxBDEDataSet.AddParam: IsmxParam;
var p: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    p := TParam(TQuery(FBDEDataSet).Params.Add) else
  if FBDEDataSet is TStoredProc then
    p := TParam(TStoredProc(FBDEDataSet).Params.Add) else
    p := nil;
  if Assigned(p) then
    Result := TsmxBDEParam.Create(p);
end;

procedure TsmxBDEDataSet.ClearFields;
begin
  FBDEDataSet.Fields.Clear;
end;

procedure TsmxBDEDataSet.ClearParams;
begin
  if FBDEDataSet is TQuery then
    TQuery(FBDEDataSet).Params.Clear else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).Params.Clear;
end;

{function TsmxBDEDataSet.CreateStreamField(const Value: IsmxField): TStream;
begin
  if Value.Field is TField then
    Result := FADODataSet.CreateBlobStream(TField(Value.Field), bmRead) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeField);
end;}

procedure TsmxBDEDataSet.Close;
begin
  FBDEDataSet.Close;
end;

procedure TsmxBDEDataSet.Execute;
begin
end;

function TsmxBDEDataSet.FindParam(const Value: String): IsmxParam;
var p: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    p := TQuery(FBDEDataSet).Params.FindParam(Value) else
  if FBDEDataSet is TStoredProc then
    p := TStoredProc(FBDEDataSet).Params.FindParam(Value) else
    p := nil;
  if Assigned(p) then
    Result := TsmxBDEParam.Create(p);
end;

function TsmxBDEDataSet.FieldByName(const FieldName: String): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FBDEDataSet.FieldByName(FieldName);
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

function TsmxBDEDataSet.FindField(const FieldName: String): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FBDEDataSet.FindField(FieldName);
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

procedure TsmxBDEDataSet.First;
begin
  FBDEDataSet.First;
end;

function TsmxBDEDataSet.GetActive: Boolean;
begin
  Result := FBDEDataSet.Active;
end;

function TsmxBDEDataSet.GetBof: Boolean;
begin
  Result := FBDEDataSet.Bof;
end;

function TsmxBDEDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

function TsmxBDEDataSet.GetDataSet: TObject;
begin
  Result := nil;
end;

function TsmxBDEDataSet.GetDataSetType: TsmxDataSetType;
begin
  Result := dstUnknown;
end;

function TsmxBDEDataSet.GetEof: Boolean;
begin
  Result := FBDEDataSet.Eof;
end;

function TsmxBDEDataSet.GetField(Index: Integer): IsmxField;
var f: TField;
begin
  Result := nil;
  f := FBDEDataSet.Fields[Index];
  if Assigned(f) then
    Result := TsmxField.Create(f);
end;

function TsmxBDEDataSet.GetFieldCount: Integer;
begin
  Result := FBDEDataSet.FieldCount;
end;

{function TsmxBDEDataSet.GetIsDataSet: Boolean;
begin
  Result := FBDEDataSet is TDataSet;
end;}

function TsmxBDEDataSet.GetParam(Index: Integer): IsmxParam;
var p: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    p := TQuery(FBDEDataSet).Params[Index] else
  if FBDEDataSet is TStoredProc then
    p := TStoredProc(FBDEDataSet).Params[Index] else
    p := nil;
  if Assigned(p) then
    Result := TsmxBDEParam.Create(p);
end;

function TsmxBDEDataSet.GetParamCount: Integer;
begin
  if FBDEDataSet is TQuery then
    Result := TQuery(FBDEDataSet).ParamCount else
  if FBDEDataSet is TStoredProc then
    Result := TStoredProc(FBDEDataSet).ParamCount else
    Result := 0;
end;

function TsmxBDEDataSet.GetPrepare: Boolean;
begin
  if FBDEDataSet is TQuery then
    Result := TQuery(FBDEDataSet).Prepared else
  if FBDEDataSet is TStoredProc then
    Result := TStoredProc(FBDEDataSet).Prepared else
    Result := False;
end;

function TsmxBDEDataSet.GetRecordNo: Integer;
begin
  Result := FBDEDataSet.RecNo;
end;

function TsmxBDEDataSet.GetRecordCount: Integer;
begin
  Result := FBDEDataSet.RecordCount;
end;

function TsmxBDEDataSet.GetSQL: TStrings;
begin
  Result := nil;
end;

function TsmxBDEDataSet.GetVersion: String;
begin
  Result := Vers;
end;

procedure TsmxBDEDataSet.Last;
begin
  FBDEDataSet.Last;
end;

{procedure TsmxBDEDataSet.LoadStreamParam(const Value: IsmxParam; Stream: TStream);
begin
  if Value.Param is TParameter then
    TParameter(Value.Param).LoadFromStream(Stream, ftBlob) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeParam);
end;}

function TsmxBDEDataSet.Locate(const KeyFields: String; const KeyValues: Variant): Boolean;
begin
  Result := FBDEDataSet.Locate(KeyFields, KeyValues, []);
end;

procedure TsmxBDEDataSet.Next;
begin
  FBDEDataSet.Next;
end;

procedure TsmxBDEDataSet.Open;
begin
  FBDEDataSet.Open;
end;

function TsmxBDEDataSet.ParamByName(const Value: String): IsmxParam;
var p: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    p := TQuery(FBDEDataSet).ParamByName(Value) else
  if FBDEDataSet is TStoredProc then
    p := TStoredProc(FBDEDataSet).ParamByName(Value) else
    p := nil;
  if Assigned(p) then
    Result := TsmxBDEParam.Create(p);
end;

procedure TsmxBDEDataSet.Post;
begin
  FBDEDataSet.Post;
end;

procedure TsmxBDEDataSet.Prepare;
begin
end;

procedure TsmxBDEDataSet.Prior;
begin
  FBDEDataSet.Prior;
end;

procedure TsmxBDEDataSet.Remove;
begin
  FBDEDataSet.Delete;
end;

procedure TsmxBDEDataSet.RemoveField(const Value: IsmxField);
var f: TObject;
begin
  f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields.Remove(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);
end;

procedure TsmxBDEDataSet.RemoveParam(const Value: IsmxParam);
var p: TObject;
begin
  p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params.RemoveParam(TParam(p)) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params.RemoveParam(TParam(p));
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
end;

procedure TsmxBDEDataSet.SetActive(Value: Boolean);
begin
  FBDEDataSet.Active := Value;
end;

procedure TsmxBDEDataSet.SetDatabase(const Value: IsmxDatabase);
var d: TObject;
begin
  if FDatabaseIntf <> Value then
  begin
    d := Value.GetDatabase;
    if d is TDatabase then
    begin
      FBDEDataSet.Close;
      FBDEDataSet.DatabaseName := TDatabase(d).DatabaseName;
      FDatabaseIntf := Value;
    end else
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
  end;
end;

procedure TsmxBDEDataSet.SetField(Index: Integer; const Value: IsmxField);
var f: TObject;
begin
  f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields[Index] := TField(f) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);
end;

procedure TsmxBDEDataSet.SetParam(Index: Integer; const Value: IsmxParam);
var p: TObject;
begin
  p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params[Index] := TParam(p) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params[Index] := TParam(p);
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
end;

procedure TsmxBDEDataSet.SetPrepare(Value: Boolean);
begin
  if FBDEDataSet is TQuery then
    TQuery(FBDEDataSet).Prepared := Value else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).Prepared := Value;
end;

procedure TsmxBDEDataSet.SetRecordNo(Value: Integer);
begin
  FBDEDataSet.RecNo := Value;
end;

procedure TsmxBDEDataSet.SetSQL(Value: TStrings);
begin
end;

{ TsmxBDEQuery }

constructor TsmxBDEQuery.Create;
begin
  inherited Create;
  FBDEDataSet := TQuery.Create(nil);
  //TQuery(FBDEDataSet).RequestLive := True;
end;

destructor TsmxBDEQuery.Destroy;
begin
  FBDEDataSet.Free;
  inherited Destroy;
end;

procedure TsmxBDEQuery.Execute;
begin
  TQuery(FBDEDataSet).ExecSQL;
end;

function TsmxBDEQuery.GetDataSet: TObject;
begin
  Result := TQuery(FBDEDataSet);
end;

function TsmxBDEQuery.GetDataSetType: TsmxDataSetType;
begin
  Result := dstQuery;
end;

function TsmxBDEQuery.GetSQL: TStrings;
begin
  Result := TQuery(FBDEDataSet).SQL;
end;

procedure TsmxBDEQuery.Prepare;
begin
  TQuery(FBDEDataSet).Prepare;
end;

procedure TsmxBDEQuery.SetSQL(Value: TStrings);
begin
  TQuery(FBDEDataSet).SQL := Value;
end;

{ TsmxBDEStoredProc }

constructor TsmxBDEStoredProc.Create;
begin
  inherited Create;
  FBDEDataSet := TStoredProc.Create(nil);
  FSQL := TStringList.Create;
end;

destructor TsmxBDEStoredProc.Destroy;
begin
  FSQL.Free;
  FBDEDataSet.Free;
  inherited Destroy;
end;

procedure TsmxBDEStoredProc.Execute;
begin
  TStoredProc(FBDEDataSet).ExecProc;
end;

function TsmxBDEStoredProc.GetDataSet: TObject;
begin
  Result := TStoredProc(FBDEDataSet);
end;

function TsmxBDEStoredProc.GetDataSetType: TsmxDataSetType;
begin
  Result := dstStoredProc;
end;

function TsmxBDEStoredProc.GetSQL: TStrings;
begin
  Result := FSQL;
  //TADOStoredProc(FADODataSet).ProcedureName := FSQL.Text;
end;

procedure TsmxBDEStoredProc.Prepare;
begin
  TStoredProc(FBDEDataSet).StoredProcName := AnsiReplaceStr(FSQL.Text, sLineBreak, '');
  TStoredProc(FBDEDataSet).Prepare;
end;

procedure TsmxBDEStoredProc.SetSQL(Value: TStrings);
begin
  //FSQL := Value;
  FSQL.Assign(Value);

  //TADOStoredProc(FADODataSet).ProcedureName := FSQL.Text;
end;

//initialization
  //CoInitialize(nil);
  //RegisterClasses([TsmxCoADODatabase]);

//finalization
  //CoUninitialize;

end.
