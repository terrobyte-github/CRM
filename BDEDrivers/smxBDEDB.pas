{**************************************}
{                                      }
{            SalesMan v1.0             }
{         BDE database classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxBDEDB;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, Windows, ActiveX, ComObj, DB, DBTables, smxBaseClasses, smxClasses,
  smxDBClasses, smxCells, smxDBIntf, smxBaseIntf, smxTypes;

const
  CLSID_smxCoBDEDatabase: TGUID = '{61272B45-B747-473C-8ECA-8148E607B057}';

type
  { TsmxCoBDEDatabase }

  TsmxCoBDEDatabase = class(TsmxCoDatabase)
  protected
    function GetDatabaseClass: TsmxInterfacedComponentClass; override;
  end;

  { TsmxBDEDatabase }

  TsmxBDEDatabase = class(TsmxCustomDatabase, IsmxDatabase)
  private
    FDatabase: TDatabase;
    function GetInternalDatabase: TDatabase;
  protected
    function GetConnected: Boolean;
    function GetDriverName: String;
    function GetInternalRef: Pointer; override;
    function GetInTransaction: Boolean;
    function GetParamText: String;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String); override;
    procedure SetDriverName(const Value: String);
    procedure SetParamText(const Value: String);

    property InternalDatabase: TDatabase read GetInternalDatabase;
  public
    destructor Destroy; override;
    procedure AssignDatabase(const Source: IsmxDatabase);
    procedure CommitTransaction;
    function NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
    procedure RollbackTransaction;
    procedure StartTransaction;

    property InTransaction: Boolean read GetInTransaction;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    property DriverName: String read GetDriverName write SetDriverName;
    property ParamText: String read GetParamText write SetParamText;
  end;

  { TsmxBDEParam }

  TsmxBDEParam = class(TsmxCustomParam, IsmxParam)
  protected
    FParam: TParam;
    procedure ChangeObjectIndex(Value: Integer); override;
    procedure ChangeObjectOwner(Value: TPersistent); override;
    function GetInternalRef: Pointer; override;
    procedure SetDataType(Value: TsmxDataType); override;
    procedure SetNumericScale(Value: Integer); override;
    procedure SetParamName(const Value: String); override;
    procedure SetParamType(Value: TsmxParamType); override;
    procedure SetPrecision(Value: Integer); override;
    procedure SetSize(Value: Integer); override;
    procedure SetValue(const Value: Variant); override;
  public
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property DataType;
    property NumericScale;
    property ParamType;
    property Precision;
    property Size;
    property Value;
  end;

  { TsmxBDEQuery }

  TsmxBDEQuery = class(TsmxDataSet, IsmxDataSet)
  private
    FParams: TParams;
    FQuery: TQuery;
    function GetInternalParams: TParams;
    function GetInternalQuery: TQuery;
  protected
    function GetInternalRef: Pointer; override;
    function GetDataSetType: TsmxDataSetType;
    function GetPrepare: Boolean;
    function GetSQLText: String;
    procedure SetDatabase(const Value: IsmxDatabase); override;
    procedure SetPrepare(Value: Boolean);
    procedure SetSQLText(const Value: String);
    function GetParamClass: TsmxInterfacedPersistentClass; override;

    property InternalParams: TParams read GetInternalParams;
    property InternalQuery: TQuery read GetInternalQuery;
  public
    destructor Destroy; override;
    procedure AssignDataSet(const Source: IsmxDataSet); override;
    procedure CreateInternalParam(Param: TsmxCustomParam); override;
    procedure DestroyInternalParam(Param: TsmxCustomParam); override;
    procedure Execute; override;
    procedure Open; override;

    property DataSetType: TsmxDataSetType read GetDataSetType;
    property Prepared: Boolean read GetPrepare write SetPrepare;
  published
    property FieldList;
    property ParamList;
    property SQLText: String read GetSQLText write SetSQLText;
  end;

  { TsmxBDEStoredProc }

  TsmxBDEStoredProc = class(TsmxDataSet, IsmxDataSet)
  private
    FParams: TParams;
    FStoredProc: TStoredProc;
    function GetInternalParams: TParams;
    function GetInternalStoredProc: TStoredProc;
  protected
    function GetInternalRef: Pointer; override;
    function GetDataSetType: TsmxDataSetType;
    function GetPrepare: Boolean;
    function GetSQLText: String;
    procedure SetDatabase(const Value: IsmxDatabase); override;
    procedure SetPrepare(Value: Boolean);
    procedure SetSQLText(const Value: String);
    function GetParamClass: TsmxInterfacedPersistentClass; override;

    property InternalParams: TParams read GetInternalParams;
    property InternalStoredProc: TStoredProc read GetInternalStoredProc;
  public
    destructor Destroy; override;
    procedure AssignDataSet(const Source: IsmxDataSet); override;
    procedure CreateInternalParam(Param: TsmxCustomParam); override;
    procedure DestroyInternalParam(Param: TsmxCustomParam); override;
    procedure Execute; override;
    procedure Open; override;

    property DataSetType: TsmxDataSetType read GetDataSetType;
    property Prepared: Boolean read GetPrepare write SetPrepare;
  published
    property FieldList;
    property ParamList;
    property SQLText: String read GetSQLText write SetSQLText;
  end;

  { TsmxBDEQueryRequest }

  TsmxBDEQueryRequest = class(TsmxRequest)
  protected
    function GetDataSetClass: TsmxInterfacedComponentClass; override;
  end;

  { TsmxBDEStoredProcRequest }

  TsmxBDEStoredProcRequest = class(TsmxRequest)
  protected
    function GetDataSetClass: TsmxInterfacedComponentClass; override;
  end;

  { TsmxBDEQueryRequestList }

  TsmxBDEQueryRequestList = class(TsmxRequestList)
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  end;

  { TsmxBDEStoredProcRequestList }

  TsmxBDEStoredProcRequestList = class(TsmxRequestList)
  protected
    function GetSlaveClass: TsmxBaseCellClass; override;
  end;

function DatabaseCLSID: TGUID;
function NewDatabase: IsmxDatabase;
function NewQuery: IsmxDataSet;
function NewProcedure: IsmxDataSet;

implementation

uses
  ComServ, StrUtils, Variants, smxConsts, smxFuncs, smxProcs, smxDBFuncs;

function DatabaseCLSID: TGUID;
begin
  Result := CLSID_smxCoBDEDatabase;
end;

function NewDatabase: IsmxDatabase;
begin
  Result := TsmxBDEDatabase.Create(nil) as IsmxDatabase;
end;

function NewQuery: IsmxDataSet;
begin
  Result := TsmxBDEQuery.Create(nil) as IsmxDataSet;
end;

function NewProcedure: IsmxDataSet;
begin
  Result := TsmxBDEStoredProc.Create(nil) as IsmxDataSet;
end;

{ TsmxCoBDEDatabase }

function TsmxCoBDEDatabase.GetDatabaseClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxBDEDatabase;
end;

{ TsmxBDEDatabase }

destructor TsmxBDEDatabase.Destroy;
begin
  if Assigned(FDatabase) then
    FDatabase.Free;
  inherited Destroy;
end;

procedure TsmxBDEDatabase.AssignDatabase(const Source: IsmxDatabase);
begin
  if Assigned(Source) then
  begin
    DatabaseName := Source.DatabaseName;
    DriverName := Source.DriverName;
    ParamText := Source.ParamText;
  end;
end;

procedure TsmxBDEDatabase.CommitTransaction;
begin
  InternalDatabase.Commit;
end;

procedure TsmxBDEDatabase.RollbackTransaction;
begin
  InternalDatabase.Rollback;
end;

function TsmxBDEDatabase.GetConnected: Boolean;
begin
  Result := InternalDatabase.Connected;
end;

procedure TsmxBDEDatabase.SetConnected(Value: Boolean);
begin
  InternalDatabase.Connected := Value;
end;

procedure TsmxBDEDatabase.SetDatabaseName(const Value: String);
begin
  inherited SetDatabaseName(Value);
  InternalDatabase.DatabaseName := Value;
end;

function TsmxBDEDatabase.GetDriverName: String;
begin
  Result := InternalDatabase.DriverName;
end;

procedure TsmxBDEDatabase.SetDriverName(const Value: String);
begin
  InternalDatabase.DriverName := Value;
end;

function TsmxBDEDatabase.GetInternalDatabase: TDatabase;
begin
  if not Assigned(FDatabase) then
  begin
    FDatabase := TDatabase.Create(nil);
    FDatabase.HandleShared := True;
    FDatabase.LoginPrompt := False;
  end;
  Result := FDatabase;
end;

function TsmxBDEDatabase.GetInternalRef: Pointer;
begin
  Result := Pointer(InternalDatabase);
end;

function TsmxBDEDatabase.GetInTransaction: Boolean;
begin
  Result := InternalDatabase.InTransaction;
end;

function TsmxBDEDatabase.GetParamText: String;
begin
  Result := InternalDatabase.Params.Text;
end;

procedure TsmxBDEDatabase.SetParamText(const Value: String);
begin
  InternalDatabase.Params.Text := Value;
end;

function TsmxBDEDatabase.NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
begin
  case DataSetType of
    dstQuery: Result := TsmxBDEQuery.Create(nil) as IsmxDataSet;
    dstStoredProc: Result := TsmxBDEStoredProc.Create(nil) as IsmxDataSet;
  end;
end;

procedure TsmxBDEDatabase.StartTransaction;
begin
  InternalDatabase.StartTransaction;
end;

{ TsmxBDEParam }

procedure TsmxBDEParam.ChangeObjectIndex(Value: Integer);
begin
  inherited ChangeObjectIndex(Value);
  if Assigned(FParam) then
    FParam.Index := Value;
end;

procedure TsmxBDEParam.ChangeObjectOwner(Value: TPersistent);
begin
  if InternalDataSet is TsmxDataSet then
  begin
    TsmxDataSet(InternalDataSet).DestroyInternalParam(Self);
  end;
  inherited ChangeObjectOwner(Value);
  if InternalDataSet is TsmxDataSet then
  begin
    TsmxDataSet(InternalDataSet).CreateInternalParam(Self);

    SetDataType(GetDataType);
    SetNumericScale(GetNumericScale);
    SetParamName(GetParamName);
    SetParamType(GetParamType);
    SetPrecision(GetPrecision);
    SetSize(GetSize);
    SetValue(GetValue);
  end;
end;

function TsmxBDEParam.GetInternalRef: Pointer;
begin
  Result := Pointer(FParam);
end;

procedure TsmxBDEParam.LoadFromStream(Stream: TStream);
var
  Str: String;
begin
  if Assigned(FParam) then
  begin
    smxProcs.StreamToStr(Stream, Str);
    FParam.Value := Str;
  end;
end;

procedure TsmxBDEParam.SetDataType(Value: TsmxDataType);
begin
  inherited SetDataType(Value);
  if Assigned(FParam) then
    FParam.DataType := Value;
end;

procedure TsmxBDEParam.SetNumericScale(Value: Integer);
begin
  inherited SetNumericScale(Value);
  if Assigned(FParam) then
    FParam.NumericScale := Value;
end;

procedure TsmxBDEParam.SetParamName(const Value: String);
begin
  inherited SetParamName(Value);
  if Assigned(FParam) then
    FParam.Name := Value;
end;

procedure TsmxBDEParam.SetParamType(Value: TsmxParamType);
begin
  inherited SetParamType(Value);
  if Assigned(FParam) then
    FParam.ParamType := TParamType(Value);
end;

procedure TsmxBDEParam.SetPrecision(Value: Integer);
begin
  inherited SetPrecision(Value);
  if Assigned(FParam) then
    FParam.Precision := Value;
end;

procedure TsmxBDEParam.SetSize(Value: Integer);
begin
  inherited SetSize(Value);
  if Assigned(FParam) then
    FParam.Size := Value;
end;

procedure TsmxBDEParam.SetValue(const Value: Variant);
begin
  inherited SetValue(Value);
  if Assigned(FParam) then
    FParam.Value := Value;
end;

procedure TsmxBDEParam.SaveToStream(Stream: TStream);
begin
  if Assigned(FParam) then
    smxProcs.StrToStream(FParam.Value, Stream);
end;

function TsmxBDEParam.IsBlob: Boolean;
begin
  if Assigned(FParam) then
    Result := smxDBFuncs.IsBlobType(FParam.DataType, FParam.Size)
  else
    Result := False;
end;

function TsmxBDEParam.IsNull: Boolean;
begin
  if Assigned(FParam) then
    Result := FParam.IsNull
  else
    Result := False;
end;

{ TsmxBDEQuery }

destructor TsmxBDEQuery.Destroy;
begin
  inherited Destroy;
  if Assigned(FParams) then
    FParams.Free;
  if Assigned(FQuery) then
    FQuery.Free;
end;

procedure TsmxBDEQuery.AssignDataSet(const Source: IsmxDataSet);
begin
  inherited AssignDataSet(Source);
  if Assigned(Source) then
    SQLText := Source.SQLText;
end;

procedure TsmxBDEQuery.Execute;
begin
  InternalQuery.Params.Assign(InternalParams);
  InternalQuery.ExecSQL;
end;

function TsmxBDEQuery.GetDataSetType: TsmxDataSetType;
begin
  Result := dstQuery;
end;

function TsmxBDEQuery.GetPrepare: Boolean;
begin
  Result := InternalQuery.Prepared;
end;

function TsmxBDEQuery.GetSQLText: String;
begin
  Result := InternalQuery.SQL.Text;
end;

procedure TsmxBDEQuery.SetSQLText(const Value: String);
begin
  InternalQuery.SQL.Text := Value;
end;

procedure TsmxBDEQuery.Open;
begin
  InternalQuery.Params.Assign(InternalParams);
  InternalQuery.Open;
end;

procedure TsmxBDEQuery.SetDatabase(const Value: IsmxDatabase);
begin
  if Assigned(Database) then
  begin
    InternalQuery.Close;
    InternalQuery.UnPrepare;
    InternalQuery.DatabaseName := '';
  end;
  inherited SetDatabase(Value);
  if Assigned(Database) then
    if TObject(Database.GetInternalRef) is TDatabase then
      InternalQuery.DatabaseName := TDatabase(Database.GetInternalRef).DatabaseName;
end;

procedure TsmxBDEQuery.SetPrepare(Value: Boolean);
begin
  InternalQuery.Prepared := Value;
end;

function TsmxBDEQuery.GetInternalRef: Pointer;
begin
  Result := Pointer(InternalQuery);
end;

function TsmxBDEQuery.GetInternalParams: TParams;
begin
  if not Assigned(FParams) then
    FParams := TParams.Create;
  Result := FParams;
end;

function TsmxBDEQuery.GetInternalQuery: TQuery;
begin
  if not Assigned(FQuery) then
    FQuery := TQuery.Create(nil);
  Result := FQuery;
end;

procedure TsmxBDEQuery.CreateInternalParam(Param: TsmxCustomParam);
begin
  if Param is TsmxBDEParam then
    if not Assigned(TsmxBDEParam(Param).FParam) then
      TsmxBDEParam(Param).FParam := TParam.Create(InternalParams);
end;

procedure TsmxBDEQuery.DestroyInternalParam(Param: TsmxCustomParam);
begin
  if Param is TsmxBDEParam then
    if Assigned(TsmxBDEParam(Param).FParam) then
    begin
      TsmxBDEParam(Param).FParam.Free;
      TsmxBDEParam(Param).FParam := nil;
    end;
end;

function TsmxBDEQuery.GetParamClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxBDEParam;
end;

{ TsmxBDEStoredProc }

destructor TsmxBDEStoredProc.Destroy;
begin
  inherited Destroy;
  if Assigned(FParams) then
    FParams.Free;
  if Assigned(FStoredProc) then
    FStoredProc.Free;
end;

procedure TsmxBDEStoredProc.AssignDataSet(const Source: IsmxDataSet);
begin
  inherited AssignDataSet(Source);
  if Assigned(Source) then
    SQLText := Source.SQLText;
end;

procedure TsmxBDEStoredProc.Execute;
begin
  InternalStoredProc.Params.Assign(InternalParams);
  InternalStoredProc.ExecProc;
end;

function TsmxBDEStoredProc.GetDataSetType: TsmxDataSetType;
begin
  Result := dstStoredProc;
end;

function TsmxBDEStoredProc.GetPrepare: Boolean;
begin
  Result := InternalStoredProc.Prepared;
end;

procedure TsmxBDEStoredProc.Open;
begin
  InternalStoredProc.Params.Assign(InternalParams);
  InternalStoredProc.Open;
end;

procedure TsmxBDEStoredProc.SetDatabase(const Value: IsmxDatabase);
begin
  if Assigned(Database) then
  begin
    InternalStoredProc.Close;
    InternalStoredProc.UnPrepare;
    InternalStoredProc.DatabaseName := '';
  end;
  inherited SetDatabase(Value);
  if Assigned(Database) then
    if TObject(Database.GetInternalRef) is TDatabase then
      InternalStoredProc.DatabaseName := TDatabase(Database.GetInternalRef).DatabaseName;
end;

procedure TsmxBDEStoredProc.SetPrepare(Value: Boolean);
begin
  InternalStoredProc.Prepared := Value;
end;

function TsmxBDEStoredProc.GetInternalRef: Pointer;
begin
  Result := Pointer(InternalStoredProc);
end;

function TsmxBDEStoredProc.GetInternalStoredProc: TStoredProc;
begin
  if not Assigned(FStoredProc) then
    FStoredProc := TStoredProc.Create(nil);
  Result := FStoredProc;
end;

procedure TsmxBDEStoredProc.CreateInternalParam(Param: TsmxCustomParam);
begin
  if Param is TsmxBDEParam then
    if not Assigned(TsmxBDEParam(Param).FParam) then
      TsmxBDEParam(Param).FParam := TParam.Create(InternalParams);
end;

procedure TsmxBDEStoredProc.DestroyInternalParam(Param: TsmxCustomParam);
begin
  if Param is TsmxBDEParam then
    if Assigned(TsmxBDEParam(Param).FParam) then
    begin
      TsmxBDEParam(Param).FParam.Free;
      TsmxBDEParam(Param).FParam := nil;
    end;
end;

function TsmxBDEStoredProc.GetParamClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxBDEParam;
end;

function TsmxBDEStoredProc.GetSQLText: String;
begin
  Result := InternalStoredProc.StoredProcName;
end;

procedure TsmxBDEStoredProc.SetSQLText(const Value: String);
begin
  InternalStoredProc.StoredProcName := Value;
end;

function TsmxBDEStoredProc.GetInternalParams: TParams;
begin
  if not Assigned(FParams) then
    FParams := TParams.Create;
  Result := FParams;
end;

{ TsmxBDEQueryRequest }

function TsmxBDEQueryRequest.GetDataSetClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxBDEQuery;
end;

{ TsmxBDEStoredProcRequest }

function TsmxBDEStoredProcRequest.GetDataSetClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxBDEStoredProc;
end;

{ TsmxBDEQueryRequestList }

function TsmxBDEQueryRequestList.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxBDEQueryRequest;
end;

{ TsmxBDEStoredProcRequestList }

function TsmxBDEStoredProcRequestList.GetSlaveClass: TsmxBaseCellClass;
begin
  Result := TsmxBDEStoredProcRequest;
end;

initialization
  Classes.RegisterClasses([TsmxBDEDatabase, TsmxBDEParam, TsmxBDEQuery,
    TsmxBDEStoredProc, TsmxBDEQueryRequest, TsmxBDEStoredProcRequest,
    TsmxBDEQueryRequestList, TsmxBDEStoredProcRequestList]);
  TComObjectFactory.Create(ComServer, TsmxCoBDEDatabase, CLSID_smxCoBDEDatabase,
    'smxCoBDEDatabase', '', ciMultiInstance, tmApartment);

finalization
  Classes.UnRegisterClasses([TsmxBDEDatabase, TsmxBDEParam, TsmxBDEQuery,
    TsmxBDEStoredProc, TsmxBDEQueryRequest, TsmxBDEStoredProcRequest,
    TsmxBDEQueryRequestList, TsmxBDEStoredProcRequestList]);

end.
