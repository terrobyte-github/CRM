unit smxBDEDB;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, Windows, ActiveX, ComObj, DB, DBTables, smxBaseClasses, smxDBClasses,
  smxDBIntf {, smxTypes};

const
  CLSID_smxCoBDEDatabase: TGUID = '{61272B45-B747-473C-8ECA-8148E607B057}';

type
  { TsmxCoBDEDatabase }

  TsmxCoBDEDatabase = class(TsmxCoDatabase)
  public
    procedure Initialize; override;
  end;

  { TsmxBDEDatabase }

  TsmxBDEDatabase = class(TsmxCustomDatabase, IsmxDatabase)
  private
    FDatabase: TDatabase;
    //FParams: TStrings;
  protected
    function GetConnected: Boolean;
    //function GetDatabase: TObject;
    function GetInternalRef: Integer;
    function GetDatabaseName: String;
    function GetDriverName: String;
    function GetInTransaction: Boolean;
    //function GetLoginPrompt: Boolean;
    function GetParams: TStrings;
    //function GetVersion: String; override;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String);
    procedure SetDriverName(const Value: String);
    //procedure SetLoginPrompt(Value: Boolean);
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
    //property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property Params: TStrings read GetParams write SetParams;
  end;

  { TsmxBDEField }

  {TsmxBDEField = class(TsmxField, IsmxField)
  protected
    function GetFieldSense: TsmxFieldSense;
    procedure SetFieldSense(Value: TsmxFieldSense);
  end;}

  { TsmxBDEParam }

  TsmxBDEParam = class(TsmxCustomParam, IsmxParam)
  private
    FParam: TParam;
    //FLocation: TsmxParamLocation;
    //FDataSet: TObject;
    FDataSetIntf: IsmxDataSet;
  protected
    function GetDataType: TsmxDataType;
    function GetNumericScale: Integer;
    //function GetParam: TObject;
    function GetParamName: String;
    function GetParamNo: Integer;
    function GetParamType: TsmxParamType;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    //function GetIsBlob: Boolean;
    function GetParamLocation: TsmxParamLocation;
    //function GetVersion: String; override;
    procedure SetDataType(Value: TsmxDataType);
    procedure SetNumericScale(Value: Integer);
    procedure SetParamName(const Value: String);
    procedure SetParamType(Value: TsmxParamType);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(Value: Variant);
    procedure SetParamLocation(Value: TsmxParamLocation);
    function GetDataSet: IsmxDataSet;
  public
    constructor Create(AParam: TParam; ADataSet: TsmxCustomDataSet);
    destructor Destroy; override;
    //procedure AssignParam(Source: TObject);
    procedure AssignParam(const Source: IInterface);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure Clear;

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
    //property IsBlob: Boolean read GetIsBlob;
    property DataSet: IsmxDataSet read GetDataSet;
  end;

  { TsmxBDEDataSet }

  TsmxBDEDataSet = class(TsmxCustomDataSet, IsmxDataSet)
  private
    FBDEDataSet: TDBDataSet;
    FDatabaseIntf: IsmxDatabase;
    //FDataSetType: TsmxDataSetType;
    FProcSQL: TStrings;
    //FSenseList: TsmxSenseKit;
    //FLocationList: TsmxLocationKit;
    function GetProcSQL: TStrings;
    //function GetSenseList: TsmxSenseKit;
    //function GetLocationList: TsmxLocationKit;
    procedure ProcChangeText(Sender: TObject);
    procedure CreateDataSet;
  protected
    function GetActive: Boolean;
    //function GetBof: Boolean;
    function GetDatabase: IsmxDatabase;
    //function GetDataSet: TObject; //virtual;
    function GetInternalRef: Integer;
    function GetDataSetType: TsmxDataSetType; //virtual;
    //function GetEof: Boolean;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    //function GetIsDataSet: Boolean;
    function GetParam(Index: Integer): IsmxParam;
    function GetParamCount: Integer;
    function GetPrepare: Boolean;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    function GetSQL: TStrings; //virtual;
    //function GetVersion: String; override;
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPrepare(Value: Boolean); //virtual;
    procedure SetRecordNo(Value: Integer);
    procedure SetSQL(Value: TStrings); //virtual;
    //procedure SetDataSetType(Value: TsmxDataSetType); virtual;

    property ProcSQL: TStrings read GetProcSQL;
    //property SenseList: TsmxSenseKit read GetSenseList;
    //property LocationList: TsmxLocationKit read GetLocationList;
  public
    constructor Create(ADataSetType: TsmxDataSetType); override;
    destructor Destroy; override;
    procedure Add;
    function AddField(const AFieldName: String; ASense: TsmxFieldSense = fsGeneral): IsmxField;
    function AddParam(const AParamName: String; ALocation: TsmxParamLocation = plConst): IsmxParam;
    procedure ClearFields;
    procedure ClearParams;
    //function CreateStreamField(const Value: IsmxField): TStream;
    procedure Close;
    procedure Execute; //virtual;
    function FieldByName(const AFieldName: String): IsmxField;
    function FindField(const AFieldName: String): IsmxField;
    function FindParam(const AParamName: String): IsmxParam;
    procedure First;
    procedure Last;
    //procedure LoadStreamParam(const Value: IsmxParam; Stream: TStream);
    function Locate(const AKeyFields: String; const AKeyValues: Variant): Boolean;
    procedure Next;
    procedure Open;
    function ParamByName(const AParamName: String): IsmxParam;
    procedure Post;
    //procedure Prepare; virtual;
    procedure Prior;
    procedure Remove;
    procedure RemoveField(const AField: IsmxField);
    procedure RemoveParam(const AParam: IsmxParam);
    function IsEmpty: Boolean;
    function Bof: Boolean;
    function Eof: Boolean;
    //function FieldBySense(ASense: TsmxFieldSense; var AFields: TsmxFieldArray): Integer;
    //function ParamByLocation(ALocation: TsmxParamLocation; var AParams: TsmxParamArray): Integer;
    procedure Edit;

    property Active: Boolean read GetActive write SetActive;
    //property Bof: Boolean read GetBof;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    //property DataSet: TObject read GetDataSet;
    property DataSetType: TsmxDataSetType read GetDataSetType;// write SetDataSetType;
    //property Eof: Boolean read GetEof;
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

  {TsmxBDEQuery = class(TsmxBDEDataSet)
  protected
    function GetDataSet: TObject; override;
    //function GetDataSetType: TsmxDataSetType; override;
    function GetSQL: TStrings; override;
    procedure SetSQL(Value: TStrings); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    //procedure Prepare; override;
  end;}

  { TsmxBDEStoredProc }

  {TsmxBDEStoredProc = class(TsmxBDEDataSet)
  private
    FSQL: TStrings;
  protected
    function GetDataSet: TObject; override;
    //function GetDataSetType: TsmxDataSetType; override;
    function GetSQL: TStrings; override;
    procedure SetSQL(Value: TStrings); override;
    procedure SetPrepare(Value: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    //procedure Prepare; override;
  end;}

function DatabaseCLSID: TGUID;
function NewDatabase: IsmxDatabase;
function NewDataSet(ADataSetType: TsmxDataSetType): IsmxDataSet;

implementation

uses
  ComServ, StrUtils, {ActiveX,} smxConsts, {smxField,} smxFuncs, smxProcs;

//const
  //Vers = '1.0';

//type
  { _TCustomADODataSet }

  //_TCustomADODataSet = class(TCustomADODataSet)
  //end;

function DatabaseCLSID: TGUID;
begin
  Result := CLSID_smxCoBDEDatabase;
end;

function NewDatabase: IsmxDatabase;
begin
  Result := TsmxBDEDatabase.Create as IsmxDatabase;
end;

function NewDataSet(ADataSetType: TsmxDataSetType): IsmxDataSet;
begin
  Result := TsmxBDEDataSet.Create(ADataSetType) as IsmxDataSet;
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
  FDatabase.LoginPrompt := False;
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

{function TsmxBDEDatabase.GetDatabase: TObject;
begin
  Result := FDatabase;
end;}

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

{function TsmxBDEDatabase.GetLoginPrompt: Boolean;
begin
  Result := FDatabase.LoginPrompt;
end;}

function TsmxBDEDatabase.GetParams: TStrings;
begin
  Result := FDatabase.Params; //FParams;
  //FDatabase.ConnectionString := WideString(FParams.Text);
end;

{function TsmxBDEDatabase.GetVersion: String;
begin
  Result := Vers;
end;}

function TsmxBDEDatabase.NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
begin
  //Result := nil;
  {case DataSetType of
    dstQuery: Result := TsmxBDEQuery.Create;
    dstStoredProc: Result := TsmxBDEStoredProc.Create;
    //else
      //raise EsmxDBInterfaceError.CreateRes(@SDBIntfDataSetUnknown);
  end;}
  Result := TsmxBDEDataSet.Create(DataSetType);
  Result.Database := Self;
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

{procedure TsmxBDEDatabase.SetLoginPrompt(Value: Boolean);
begin
  FDatabase.LoginPrompt := Value;
end;}

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

function TsmxBDEDatabase.GetInternalRef: Integer;
begin
  Result := Integer(FDatabase);
end;

{ TsmxBDEField }

{function TsmxBDEField.GetFieldSense: TsmxFieldSense;
begin
  //if Assigned()
end;

procedure TsmxBDEField.SetFieldSense(Value: TsmxFieldSense);
begin

end;}

{ TsmxBDEParam }

constructor TsmxBDEParam.Create(AParam: TParam; ADataSet: TsmxCustomDataSet);
begin
  inherited Create(ADataSet);
  FParam := AParam;
  //FLocation := plConst;
  //FDataSet := ADataSet;
end;

destructor TsmxBDEParam.Destroy;
begin
  FDataSetIntf := nil;
  //FParam := nil;
  inherited Destroy;
end;

{procedure TsmxBDEParam.AssignParam(Source: TObject);
begin
  FParam.Assign(TPersistent(Source));
end;}

procedure TsmxBDEParam.AssignParam(const Source: IInterface);

  procedure AssignFromParam(AParam: IsmxParam);
  begin
    DataType := AParam.DataType;
    if AParam.IsNull then
      Clear
    else
      Value := AParam.Value;
    ParamName := AParam.ParamName;
    ParamType := AParam.ParamType;
    Size := AParam.Size;
    Precision := AParam.Precision;
    NumericScale := AParam.NumericScale;
    ParamLocation := AParam.ParamLocation;
  end;

  procedure AssignFromField(AField: IsmxField);
  begin
    DataType := AField.DataType;
    if AField.IsNull then
      Clear
    else
      Value := AField.Value;
    ParamName := AField.FieldName;
    Size := AField.Size;
    if AField.DataType in [ftBCD, ftFMTBcd] then
      NumericScale := AField.Size;
  end;

var
  Param: IsmxParam; Field: IsmxField;
begin
  if Assigned(Source) then
  begin
    if Source.QueryInterface(IsmxParam, Param) = S_OK then
      //p := (Source as IsmxParam).GetParam else
      //p := ParamIntf.GetParam else
      AssignFromParam(Param) else
    if Source.QueryInterface(IsmxField, Field) = S_OK then
      //p := (Source as IsmxField).GetField else
      //p := FieldIntf.GetField else
      AssignFromField(Field);
  end else
    Clear;
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);
    //p := nil;
  {if p is TParameter then
    FParam.Assign(TParameter(p)) else
  if p is TField then
    FParam.Assign(TField(p)) else
  if p is TParam then
    FParam.Assign(TParam(p)) else
  if p is TStrings then
    FParam.Assign(TStrings(p))}
  {if p is TPersistent then
    FParam.Assign(TPersistent(p)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
end;

function TsmxBDEParam.GetDataType: TsmxDataType;
begin
  Result := FParam.DataType;
end;

function TsmxBDEParam.GetNumericScale: Integer;
begin
  Result := FParam.NumericScale;
end;

{function TsmxBDEParam.GetParam: TObject;
begin
  Result := FParam;
end;}

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

{function TsmxBDEParam.GetVersion: String;
begin
  Result := Vers;
end;}

procedure TsmxBDEParam.LoadFromStream(Stream: TStream);
var
  Str: String;
begin
  //FParam.LoadFromStream(Stream, ftBlob);
  smxProcs.StreamToStr(Stream, Str);
  FParam.Value := Str;
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
  //inherited SetParamName(Value);
  if ParamName <> Value then
  begin
    InternalDataSet.LocationList.Locations[Value] :=
      InternalDataSet.LocationList.Locations[ParamName];
    InternalDataSet.LocationList.Locations[ParamName] :=
      plConst;
  end;
  FParam.Name := Value;
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
  smxProcs.StrToStream(FParam.Value, Stream);
end;

function TsmxBDEParam.GetParamLocation: TsmxParamLocation;
begin
  Result := InternalDataSet.LocationList.Locations[ParamName];
end;

procedure TsmxBDEParam.SetParamLocation(Value: TsmxParamLocation);
begin
  InternalDataSet.LocationList.Locations[ParamName] := Value;
end;

{function TsmxBDEParam.GetIsBlob: Boolean;
begin
  Result := smxFuncs.IsBlobType(FParam.DataType, Length(FParam.Value));
end;}

procedure TsmxBDEParam.Clear;
begin
  FParam.Clear;
end;

function TsmxBDEParam.IsBlob: Boolean;
begin
  Result := smxFuncs.IsBlobType(FParam.DataType, FParam.Size);
end;

function TsmxBDEParam.IsNull: Boolean;
begin
  Result := FParam.IsNull;
end;

function TsmxBDEParam.GetDataSet: IsmxDataSet;
begin
  if not Assigned(FDataSetIntf) then
    InternalDataSet.GetInterface(IsmxDataSet, FDataSetIntf);
  Result := FDataSetIntf;
end;

{ TsmxBDEDataSet }

constructor TsmxBDEDataSet.Create(ADataSetType: TsmxDataSetType);
begin
  inherited Create(ADataSetType);
  //FDataSetType := ADataSetType;
  {case FDataSetType of
    dstQuery: FBDEDataSet := TQuery.Create(nil);
    dstStoredProc: FBDEDataSet := TStoredProc.Create(nil);
  end;}
  CreateDataSet;
end;

destructor TsmxBDEDataSet.Destroy;
begin
  if Assigned(FProcSQL) then
    FProcSQL.Free;
  //if Assigned(FSenseList) then
    //FSenseList.Free;
  //if Assigned(FLocationList) then
    //FLocationList.Free;
  FBDEDataSet.Free;
  FDatabaseIntf := nil;
  inherited Destroy;
end;

procedure TsmxBDEDataSet.CreateDataSet;
begin
  case DataSetType of
    dstQuery: FBDEDataSet := TQuery.Create(nil);
    dstStoredProc: FBDEDataSet := TStoredProc.Create(nil);
  end;
end;

procedure TsmxBDEDataSet.Add;
begin
  FBDEDataSet.Append;
end;

function TsmxBDEDataSet.AddField(const AFieldName: String;
  ASense: TsmxFieldSense = fsGeneral): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.FieldDefList.FieldByName(AFieldName).CreateField(FBDEDataSet);
  //Field := FBDEDataSet.FieldList.FieldByName(AFieldName);
  if Assigned(Field) then
  begin
    FBDEDataSet.Fields.Add(Field);
    Result := TsmxField.Create(Field, Self);
    with SenseList.Add do
    begin
      FieldName := AFieldName;
      FieldSense := ASense;
    end;
  end;
end;

function TsmxBDEDataSet.AddParam(const AParamName: String;
  ALocation: TsmxParamLocation = plConst): IsmxParam;
var
  Param: TParam;
begin
  Result := nil;
  Param := nil;
  if FBDEDataSet is TQuery then
  begin
    Param := TParam(TQuery(FBDEDataSet).Params.Add);
    Param.Name := AParamName;
  end else
  if FBDEDataSet is TStoredProc then
  begin
    Param := TParam(TStoredProc(FBDEDataSet).Params.Add);
    Param.Name := AParamName;
  end;
  if Assigned(Param) then
  begin
    Result := TsmxBDEParam.Create(Param, Self);
    with LocationList.Add do
    begin
      ParamName := AParamName;
      ParamLocation := ALocation;
    end;
  end;
end;

procedure TsmxBDEDataSet.ClearFields;
begin
  FBDEDataSet.Fields.Clear;
  SenseList.Clear;
end;

procedure TsmxBDEDataSet.ClearParams;
begin
  if FBDEDataSet is TQuery then
    TQuery(FBDEDataSet).Params.Clear else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).Params.Clear;
  LocationList.Clear;
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
  if FBDEDataSet is TQuery then
    TQuery(FBDEDataSet).ExecSQL else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).ExecProc;
end;

function TsmxBDEDataSet.FindParam(const AParamName: String): IsmxParam;
var
  Param: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).Params.FindParam(AParamName) else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).Params.FindParam(AParamName) else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);
end;

function TsmxBDEDataSet.FieldByName(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields.FieldByName(AFieldName);
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

function TsmxBDEDataSet.FindField(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields.FindField(AFieldName);
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

procedure TsmxBDEDataSet.First;
begin
  FBDEDataSet.First;
end;

function TsmxBDEDataSet.GetActive: Boolean;
begin
  Result := FBDEDataSet.Active;
end;

function TsmxBDEDataSet.Bof: Boolean;
begin
  Result := FBDEDataSet.Bof;
end;

function TsmxBDEDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

{function TsmxBDEDataSet.GetDataSet: TObject;
begin
  Result := FBDEDataSet;
end;}

function TsmxBDEDataSet.GetDataSetType: TsmxDataSetType;
begin
  Result := DataSetType;
end;

function TsmxBDEDataSet.Eof: Boolean;
begin
  Result := FBDEDataSet.Eof;
end;

function TsmxBDEDataSet.GetField(Index: Integer): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields[Index];
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
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
var
  Param: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).Params[Index] else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).Params[Index] else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);
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
  if FBDEDataSet is TQuery then
    Result := TQuery(FBDEDataSet).SQL else
  if FBDEDataSet is TStoredProc then
    Result := ProcSQL else
    Result := nil;
end;

{function TsmxBDEDataSet.GetVersion: String;
begin
  Result := Vers;
end;}

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

function TsmxBDEDataSet.Locate(const AKeyFields: String; const AKeyValues: Variant): Boolean;
begin
  Result := FBDEDataSet.Locate(AKeyFields, AKeyValues, []);
end;

{function TsmxBDEDataSet.Locate(ASense: TsmxFieldSense; const AKeyValue: Variant): Boolean;
var
  i: Integer;
  KeyFields: String;
begin
  KeyFields := '';
  for i := 0 to FBDEDataSet.Fields.Count - 1 do
    if SenseList.Senses[FBDEDataSet.Fields[i].FieldName] = ASense then
      KeyFields := KeyFields + FBDEDataSet.Fields[i].FieldName + ';';
  Result := Locate(KeyFields, AKeyValue);
end;}

procedure TsmxBDEDataSet.Next;
begin
  FBDEDataSet.Next;
end;

procedure TsmxBDEDataSet.Open;
begin
  FBDEDataSet.Open;
end;

function TsmxBDEDataSet.ParamByName(const AParamName: String): IsmxParam;
var
  Param: TParam;
begin
  Result := nil;
  if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).ParamByName(AParamName) else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).ParamByName(AParamName) else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);
end;

procedure TsmxBDEDataSet.Post;
begin
  FBDEDataSet.Post;
end;

{procedure TsmxBDEDataSet.Prepare;
begin
end;}

procedure TsmxBDEDataSet.Prior;
begin
  FBDEDataSet.Prior;
end;

procedure TsmxBDEDataSet.Remove;
begin
  FBDEDataSet.Delete;
end;

procedure TsmxBDEDataSet.RemoveField(const AField: IsmxField);
var
  Field: TField;
begin
  {f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields.Remove(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  if Assigned(AField) then
  begin
    //SenseList.Senses[AField.FieldName] := fsGeneral;
    SenseList.Remove(SenseList.FindByName(AField.FieldName));
    Field := FBDEDataSet.Fields.FindField(AField.FieldName);
    if Assigned(Field) then
      FBDEDataSet.Fields.Remove(Field);
  end;
end;

procedure TsmxBDEDataSet.RemoveParam(const AParam: IsmxParam);
var
  Param: TParam;
begin
  {p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params.RemoveParam(TParam(p)) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params.RemoveParam(TParam(p));
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
  if Assigned(AParam) then
  begin
    //LocationList.Locations[AParam.ParamName] := plConst;
    LocationList.Remove(LocationList.FindByName(AParam.ParamName));
    if FBDEDataSet is TQuery then
    begin
      Param := TQuery(FBDEDataSet).Params.FindParam(AParam.ParamName);
      if Assigned(Param) then
        TQuery(FBDEDataSet).Params.RemoveParam(Param);
    end else
    if FBDEDataSet is TStoredProc then
    begin
      Param := TStoredProc(FBDEDataSet).Params.FindParam(AParam.ParamName);
      if Assigned(Param) then
        TStoredProc(FBDEDataSet).Params.RemoveParam(Param);
    end;
  end;
end;

procedure TsmxBDEDataSet.SetActive(Value: Boolean);
begin
  FBDEDataSet.Active := Value;
end;

procedure TsmxBDEDataSet.SetDatabase(const Value: IsmxDatabase);
//var
  //Database: TObject;
begin
  {if FDatabaseIntf <> Value then
  begin
    Database := Value.GetDatabase;
    if Database is TDatabase then
    begin
      FBDEDataSet.Close;
      FBDEDataSet.DatabaseName := TDatabase(Database).DatabaseName;
      FDatabaseIntf := Value;
    end else
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
  end;}
  if FDatabaseIntf <> Value then
  begin
    FDatabaseIntf := Value;
    if Assigned(FDatabaseIntf) and (TObject(FDatabaseIntf.GetInternalRef) is TDatabase) then
      FBDEDataSet.DatabaseName := TDatabase(FDatabaseIntf.GetInternalRef).DatabaseName else
      FBDEDataSet.DatabaseName := '';
  end;
end;

procedure TsmxBDEDataSet.SetField(Index: Integer; const Value: IsmxField);
var
  Field: IsmxField;
begin
  {f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields[Index] := TField(f) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  Field := TsmxField.Create(FBDEDataSet.Fields[Index], Self);
  Field.AssignField(Value);
end;

procedure TsmxBDEDataSet.SetParam(Index: Integer; const Value: IsmxParam);
var
  Param: IsmxParam;
begin
  {p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params[Index] := TParam(p) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params[Index] := TParam(p);
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
  if FBDEDataSet is TQuery then
  begin
    Param := TsmxBDEParam.Create(TQuery(FBDEDataSet).Params[Index], Self);
    Param.AssignParam(Value);
  end else
  if FBDEDataSet is TStoredProc then
  begin
    Param := TsmxBDEParam.Create(TStoredProc(FBDEDataSet).Params[Index], Self);
    Param.AssignParam(Value);
  end;
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
  if FBDEDataSet is TQuery then
    TQuery(FBDEDataSet).SQL := Value else
  if FBDEDataSet is TStoredProc then
    ProcSQL.Assign(Value);
end;

function TsmxBDEDataSet.IsEmpty: Boolean;
begin
  Result := FBDEDataSet.IsEmpty;
end;

{procedure TsmxBDEDataSet.SetDataSetType(Value: TsmxDataSetType);
begin

end;}

{function TsmxBDEDataSet.FieldBySense(ASense: TsmxFieldSense;
  var AFields: TsmxFieldArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AFields, 0);
  for i := 0 to FieldCount - 1 do
    if Fields[i].FieldSense = ASense then
    begin
      Inc(Result);
      SetLength(AFields, Result);
      AFields[Result - 1] := Fields[i];
    end;
end;

function TsmxBDEDataSet.ParamByLocation(ALocation: TsmxParamLocation;
  var AParams: TsmxParamArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(AParams, 0);
  for i := 0 to ParamCount - 1 do
    if Params[i].ParamLocation = ALocation then
    begin
      Inc(Result);
      SetLength(AParams, Result);
      AParams[Result - 1] := Params[i];
    end;
end;}

procedure TsmxBDEDataSet.Edit;
begin
  FBDEDataSet.Edit;
end;

function TsmxBDEDataSet.GetProcSQL: TStrings;
begin
  if not Assigned(FProcSQL) then
  begin
    FProcSQL := TStringList.Create;
    TStringList(FProcSQL).OnChange := ProcChangeText;
  end;
  Result := FProcSQL;
end;

procedure TsmxBDEDataSet.ProcChangeText(Sender: TObject);
begin
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).StoredProcName :=
      StrUtils.AnsiReplaceStr(ProcSQL.Text, sLineBreak, '');
end;

function TsmxBDEDataSet.GetInternalRef: Integer;
begin
  Result := Integer(FBDEDataSet);
end;

{function TsmxBDEDataSet.GetLocationList: TsmxLocationKit;
begin
  if not Assigned(FLocationList) then
    FLocationList := TsmxLocationKit.Create(TsmxLocationItem);
  Result := FLocationList;
end;}

{function TsmxBDEDataSet.GetSenseList: TsmxSenseKit;
begin
  if not Assigned(FSenseList) then
    FSenseList := TsmxSenseKit.Create(TsmxSenseItem);
  Result := FSenseList;
end;}

{ TsmxBDEQuery }

{constructor TsmxBDEQuery.Create;
begin
  inherited Create;
  FBDEDataSet := TQuery.Create(nil);
  FDataSetType := dstQuery;
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
end;}

{function TsmxBDEQuery.GetDataSetType: TsmxDataSetType;
begin
  Result := dstQuery;
end;}

{function TsmxBDEQuery.GetSQL: TStrings;
begin
  Result := TQuery(FBDEDataSet).SQL;
end;}

{procedure TsmxBDEQuery.Prepare;
begin
  TQuery(FBDEDataSet).Prepare;
end;}

{procedure TsmxBDEQuery.SetSQL(Value: TStrings);
begin
  TQuery(FBDEDataSet).SQL := Value;
end;}

{ TsmxBDEStoredProc }

{constructor TsmxBDEStoredProc.Create;
begin
  inherited Create;
  FBDEDataSet := TStoredProc.Create(nil);
  FSQL := TStringList.Create;
  FDataSetType := dstStoredProc;
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
end;}

{function TsmxBDEStoredProc.GetDataSetType: TsmxDataSetType;
begin
  Result := dstStoredProc;
end;}

{function TsmxBDEStoredProc.GetSQL: TStrings;
begin
  Result := FSQL;
  //TADOStoredProc(FADODataSet).ProcedureName := FSQL.Text;
end;}

{procedure TsmxBDEStoredProc.Prepare;
begin
  TStoredProc(FBDEDataSet).StoredProcName := AnsiReplaceStr(FSQL.Text, sLineBreak, '');
  TStoredProc(FBDEDataSet).Prepare;
end;}

{procedure TsmxBDEStoredProc.SetPrepare(Value: Boolean);
begin
  TStoredProc(FBDEDataSet).StoredProcName :=
    AnsiReplaceStr(FSQL.Text, sLineBreak, '');
  inherited SetPrepare(Value);
end;

procedure TsmxBDEStoredProc.SetSQL(Value: TStrings);
begin
  //FSQL := Value;
  FSQL.Assign(Value);

  //TADOStoredProc(FADODataSet).ProcedureName := FSQL.Text;
end;}

{ TsmxCoBDEDatabase }

procedure TsmxCoBDEDatabase.Initialize;
begin
  inherited Initialize;
  FDatabaseIntf := NewDatabase;
end;

initialization
  TComObjectFactory.Create(ComServer, TsmxCoBDEDatabase, CLSID_smxCoBDEDatabase,
    'smxCoBDEDatabase', '', ciMultiInstance, tmApartment);

end.
