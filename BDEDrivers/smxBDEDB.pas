unit smxBDEDB;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Classes, Windows, ActiveX, ComObj, DB, DBTables, smxBaseClasses, smxDBClasses,
  smxCells, smxDBIntf, smxBaseIntf, smxTypes;

const
  CLSID_smxCoBDEDatabase: TGUID = '{61272B45-B747-473C-8ECA-8148E607B057}';

type
  { TsmxCoBDEDatabase }

  TsmxCoBDEDatabase = class(TsmxCoDatabase)
  protected
    function CreateDatabase: IsmxDatabase; override;
  //public
    //procedure Initialize; override;
  end;

  { TsmxBDEDatabase }

  TsmxBDEDatabase = class(TsmxCustomDatabase, IsmxDatabase)
  private
    FDatabase: TDatabase;
    function GetInternalDatabase: TDatabase;
    //FParams: TStrings;
  protected
    function GetConnected: Boolean;
    //function GetDatabase: TObject;
    //function GetDatabaseName: String; override;
    function GetDriverName: String;
    function GetInternalRef: Pointer; override;
    function GetInTransaction: Boolean;
    //function GetLoginPrompt: Boolean;
    //function GetParams: TStrings;
    function GetParamText: String;
    //function GetVersion: String; override;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String); override;
    procedure SetDriverName(const Value: String);
    //procedure SetLoginPrompt(Value: Boolean);
    //procedure SetParams(Value: TStrings);
    procedure SetParamText(const Value: String);

    property InternalDatabase: TDatabase read GetInternalDatabase;
  public
    //constructor Create;
    destructor Destroy; override;
    procedure AssignDatabase(const Source: IsmxDatabase);
    procedure CommitTransaction;
    function NewDataSet(DataSetType: TsmxDataSetType; Controller: IsmxBaseInterface = nil): IsmxDataSet;
    procedure RollbackTransaction;
    procedure StartTransaction;

    property InTransaction: Boolean read GetInTransaction;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    //property Database: TObject read GetDatabase;
    //property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property DriverName: String read GetDriverName write SetDriverName;
    //property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    //property Params: TStrings read GetParams write SetParams;
    property ParamText: String read GetParamText write SetParamText;
  end;

  { TsmxParam }

  {TsmxParam = class(TParam, IInterface, IsmxFreeNotification, IsmxOwnerNotification)
  private
    FFreeNotificationIntf: IsmxFreeNotification;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetReference: Pointer;
    procedure OwnerFreeNotification(const Sender: IsmxFreeNotification);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property FreeNotification: IsmxFreeNotification read FFreeNotificationIntf implements IsmxFreeNotification;
  end;}

  { TsmxBDEParam }

  TsmxBDEParam = class(TsmxCustomParam, IsmxParam)
  private
    //FParam: TParam;
    //FLocation: TsmxParamLocation;
    //FDataSet: TObject;
    //FDataSetIntf: IsmxDataSet;
    //function GetParam: TParam;
  protected
    FParam: TParam;
    procedure ChangeObjectIndex(Value: Integer); override;
    procedure ChangeObjectOwner(Value: TPersistent); override;
    //function GetDataType: TsmxDataType;
    function GetInternalRef: Pointer; override;
    //function GetNumericScale: Integer;
    //function GetParam: TObject;
    //function GetParamName: String; override;
    //function GetParamIndex: Integer;
    //function GetParamType: TsmxParamType;
    //function GetPrecision: Integer;
    //function GetReference: Pointer;
    //function GetSize: Integer;
    //function GetValue: Variant;
    //function GetIsBlob: Boolean;
    //function GetParamLocation: TsmxParamLocation;
    //function GetVersion: String; override;
    //procedure OwnerFreeNotification(const Sender: IsmxFreeNotification);
    procedure SetDataType(Value: TsmxDataType); override;
    //procedure SetInternalRef(Value: Pointer); override;
    procedure SetNumericScale(Value: Integer); override;
    //procedure SetParam(Value: TParam); virtual;
    //procedure SetParamIndex(Value: Integer);
    procedure SetParamName(const Value: String); override;
    procedure SetParamType(Value: TsmxParamType); override;
    procedure SetPrecision(Value: Integer); override;
    procedure SetSize(Value: Integer); override;
    procedure SetValue(const Value: Variant); override;
    //procedure SetParamLocation(Value: TsmxParamLocation);
    //function GetDataSet: IsmxDataSet;

    //property Param: TParam read FParam write FParam;
  public
    //constructor Create(Param: TParam; DataSet: TsmxCustomDataSet);
    //destructor Destroy; override;
    //procedure AssignParam(Source: TObject);
    //procedure AssignParam(const Source: IsmxParam); overload; override;
    //procedure AssignParam(const Source: IsmxField); overload; override;
    //procedure Clear; override;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    //property DataType: TsmxDataType read GetDataType write SetDataType;
    //property NumericScale: Integer read GetNumericScale write SetNumericScale;
    //property Param: TParam read FParam write SetParam;
    //property ParamName: String read GetParamName write SetParamName;
    //property ParamIndex: Integer read GetParamIndex write SetParamIndex;
    //property ParamType: TsmxParamType read GetParamType write SetParamType;
    //property Precision: Integer read GetPrecision write SetPrecision;
    //property Size: Integer read GetSize write SetSize;
    //property Value: Variant read GetValue write SetValue;
    //property ParamLocation: TsmxParamLocation read GetParamLocation write SetParamLocation;
    //property IsBlob: Boolean read GetIsBlob;
    //property DataSet: IsmxDataSet read GetDataSet;
  published
    property DataType;
    property NumericScale;
    property ParamType;
    property Precision;
    property Size;
    property Value;
  end;

  { TsmxQuery }

  {TsmxQuery = class(TQuery)
  public
    constructor Create(AOwner: TComponent); override;
  end;}

  { TsmxBDEQuery }

  TsmxBDEQuery = class(TsmxDataSet, IsmxDataSet)
  private
    FParams: TParams;
    FQuery: TQuery;
    //FBDEDataSet: TDBDataSet;
    //FDatabaseIntf: IsmxDatabase;
    //FDataSetType: TsmxDataSetType;
    //FProcSQL: TStrings;
    //FSenseList: TsmxSenseKit;
    //FLocationList: TsmxLocationKit;
    //function GetProcSQL: TStrings;
    //function GetSenseList: TsmxSenseKit;
    //function GetLocationList: TsmxLocationKit;
    //procedure ProcChangeText(Sender: TObject);
    //procedure CreateDataSet;
    function GetInternalParams: TParams;
    function GetInternalQuery: TQuery;
  protected
    //function GetActive: Boolean;
    //function GetBof: Boolean;
    //function GetDatabase: IsmxDatabase;
    //function GetDataSet: TObject; //virtual;
    function GetInternalRef: Pointer; override;
    function GetDataSetType: TsmxDataSetType; //virtual;
    //function GetEof: Boolean;
    //function GetField(Index: Integer): IsmxField;
    //function GetFieldCount: Integer;
    //function GetIsDataSet: Boolean;
    //function GetInternalDataSetClass: TComponentClass; override;

    //function GetParam(Index: Integer): IsmxParam;
    //function GetParamCount: Integer;
    //function GetParamIntf(Param: TParam): IsmxParam; virtual;
    function GetPrepare: Boolean;
    //function GetRecordNo: Integer;
    //function GetRecordCount: Integer;
    //function GetSQL: TStrings; //virtual;
    function GetSQLText: String;
    //function GetVersion: String; override;
    //procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase); override;
    //procedure SetField(Index: Integer; const Value: IsmxField);
    //procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPrepare(Value: Boolean); //virtual;
    //procedure SetRecordNo(Value: Integer);
    //procedure SetSQL(Value: TStrings); //virtual;
    procedure SetSQLText(const Value: String);
    //procedure SetDataSetType(Value: TsmxDataSetType); virtual;
    function GetParamClass: TsmxInterfacedPersistentClass; override;
    //procedure CreateInternalParam(Param: TsmxCustomParam); override;
    //procedure DestroyInternalParam(Param: TsmxCustomParam); override;

    //property ProcSQL: TStrings read GetProcSQL;
    //property SenseList: TsmxSenseKit read GetSenseList;
    //property LocationList: TsmxLocationKit read GetLocationList;
    property InternalParams: TParams read GetInternalParams;
    property InternalQuery: TQuery read GetInternalQuery;
  public
    //constructor Create(ADataSetType: TsmxDataSetType); override;
    destructor Destroy; override;
    //procedure Add;
    //function AddField(const AFieldName: String): IsmxField;
    //function AddParam(DataType: TsmxDataType): IsmxParam;
    procedure AssignDataSet(const Source: IsmxDataSet); override;
    //procedure ClearFields;
    //procedure ClearParams; override;
    //function CreateStreamField(const Value: IsmxField): TStream;
    procedure CreateInternalParam(Param: TsmxCustomParam); override;
    procedure DestroyInternalParam(Param: TsmxCustomParam); override;
    //procedure Close;
    procedure Execute; override;
    //function FieldByName(const AFieldName: String): IsmxField;
    //function FindField(const AFieldName: String): IsmxField;
    //function FindParam(const ParamName: String): IsmxParam;
    //procedure First;
    //procedure Last;
    //procedure LoadStreamParam(const Value: IsmxParam; Stream: TStream);
    //function Locate(const AKeyFields: String; const AKeyValues: Variant): Boolean;
    //procedure Next;
    procedure Open; override;
    //function ParamByName(const ParamName: String): IsmxParam;
    //procedure Post;
    //procedure Prepare; virtual;
    //procedure Prior;
    //procedure Remove;
    //procedure DeleteField(const AField: IsmxField);
    //procedure DeleteParam(const Param: IsmxParam); override;
    //function IsEmpty: Boolean;
    //function Bof: Boolean;
    //function Eof: Boolean;
    //function FieldBySense(ASense: TsmxFieldSense; var AFields: TsmxFieldArray): Integer;
    //function ParamByLocation(ALocation: TsmxParamLocation; var AParams: TsmxParamArray): Integer;
    //procedure Edit;

    //property Active: Boolean read GetActive write SetActive;
    //property Bof: Boolean read GetBof;
    //property Database: IsmxDatabase read GetDatabase write SetDatabase;
    //property DataSet: TObject read GetDataSet;
    property DataSetType: TsmxDataSetType read GetDataSetType;// write SetDataSetType;
    //property Eof: Boolean read GetEof;
    //property FieldCount: Integer read GetFieldCount;
    //property Fields[Index: Integer]: IsmxField read GetField write SetField;
    //property IsDataSet: Boolean read GetIsDataSet;
    //property ParamCount: Integer read GetParamCount;
    //property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
    property Prepared: Boolean read GetPrepare write SetPrepare;
    //property RecordNo: Integer read GetRecordNo write SetRecordNo;
    //property RecordCount: Integer read GetRecordCount;
  published
    property FieldList;
    property ParamList;
    //property SQL: TStrings read GetSQL write SetSQL;
    property SQLText: String read GetSQLText write SetSQLText;
  end;

  { TsmxBDEQuery }

  {TsmxBDEQuery = class(TsmxBDEQuery)
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

  TsmxBDEStoredProc = class(TsmxDataSet, IsmxDataSet)
  private
    FParams: TParams;
    FStoredProc: TStoredProc;
    //FProcSQL: TStrings;
    //function GetProcSQL: TStrings;
    function GetInternalParams: TParams;
    function GetInternalStoredProc: TStoredProc;
    //procedure ProcSQLChange(Sender: TObject);
  protected
    function GetInternalRef: Pointer; override;
    function GetDataSetType: TsmxDataSetType; //virtual;
    //function GetParam(Index: Integer): IsmxParam;
    //function GetParamCount: Integer;
    //function GetParamIntf(Param: TParam): IsmxParam; virtual;
    function GetPrepare: Boolean;
    //function GetSQL: TStrings;
    function GetSQLText: String;
    procedure SetDatabase(const Value: IsmxDatabase); override;
    //procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPrepare(Value: Boolean); //virtual;
    //procedure SetSQL(Value: TStrings);
    procedure SetSQLText(const Value: String);
    function GetParamClass: TsmxInterfacedPersistentClass; override;
    //procedure CreateInternalParam(Param: TsmxCustomParam); override;
    //procedure DestroyInternalParam(Param: TsmxCustomParam); override;

    //property ProcSQL: TStrings read GetProcSQL;
    property InternalParams: TParams read GetInternalParams;
    property InternalStoredProc: TStoredProc read GetInternalStoredProc;
  public
    destructor Destroy; override;
    //function AddParam(DataType: TsmxDataType): IsmxParam;
    procedure AssignDataSet(const Source: IsmxDataSet); override;
    //procedure ClearParams; override;
    procedure CreateInternalParam(Param: TsmxCustomParam); override;
    procedure DestroyInternalParam(Param: TsmxCustomParam); override;
    procedure Execute; override;
    //function FindParam(const ParamName: String): IsmxParam;
    //function ParamByName(const ParamName: String): IsmxParam;
    //procedure DeleteParam(const Param: IsmxParam); override;
    procedure Open; override;

    property DataSetType: TsmxDataSetType read GetDataSetType;// write SetDataSetType;
    //property ParamCount: Integer read GetParamCount;
    //property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
    property Prepared: Boolean read GetPrepare write SetPrepare;
  published
    property FieldList;
    property ParamList;
    //property SQL: TStrings read GetSQL write SetSQL;
    property SQLText: String read GetSQLText write SetSQLText;
  end;

  { TsmxReqBDEQuery }

  TsmxReqBDEQuery = class(TsmxRequest)
  protected
    function GetDataSetClass: TsmxInterfacedComponentClass; override;
  end;

  { TsmxReqBDEStoredProc }

  TsmxReqBDEStoredProc = class(TsmxRequest)
  protected
    function GetDataSetClass: TsmxInterfacedComponentClass; override;
  end;

function DatabaseCLSID: TGUID;
function NewDatabase(const Controller: IsmxBaseInterface = nil): IsmxDatabase;
function NewQuery(const Controller: IsmxBaseInterface = nil): IsmxDataSet;
function NewProcedure(const Controller: IsmxBaseInterface = nil): IsmxDataSet;

implementation

uses
  ComServ, StrUtils, Variants, smxConsts, smxFuncs, smxProcs, smxDBFuncs, smxLibProcs;

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

function NewDatabase(const Controller: IsmxBaseInterface = nil): IsmxDatabase;
begin
  Result := TsmxBDEDatabase.Create(nil, Controller) as IsmxDatabase;
end;

function NewQuery(const Controller: IsmxBaseInterface = nil): IsmxDataSet;
begin
  Result := TsmxBDEQuery.Create(nil, Controller) as IsmxDataSet;
end;

function NewProcedure(const Controller: IsmxBaseInterface = nil): IsmxDataSet;
begin
  Result := TsmxBDEStoredProc.Create(nil, Controller) as IsmxDataSet;
end;

{ TsmxBDEDatabase }

{constructor TsmxBDEDatabase.Create;
begin
  inherited Create;
  FDatabase := TDatabase.Create(nil);
  FDatabase.HandleShared := True;
  FDatabase.LoginPrompt := False;
end;}

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
    //Params := Source.Params;
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

{function TsmxBDEDatabase.GetDatabase: TObject;
begin
  Result := FDatabase;
end;}

{function TsmxBDEDatabase.GetDatabaseName: String;
begin
  Result := InternalDatabase.DatabaseName;
end;}

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

{function TsmxBDEDatabase.GetParams: TStrings;
begin
  Result := InternalDatabase.Params;
end;

procedure TsmxBDEDatabase.SetParams(Value: TStrings);
begin
  InternalDatabase.Params := Value;
end;}

function TsmxBDEDatabase.GetParamText: String;
begin
  Result := InternalDatabase.Params.Text;
end;

procedure TsmxBDEDatabase.SetParamText(const Value: String);
begin
  InternalDatabase.Params.Text := Value;
end;

function TsmxBDEDatabase.NewDataSet(DataSetType: TsmxDataSetType; Controller: IsmxBaseInterface = nil): IsmxDataSet;
begin
  //Result := nil;
  case DataSetType of
    dstQuery: Result := TsmxBDEQuery.Create(nil, Controller) as IsmxDataSet;
    dstStoredProc: Result := TsmxBDEStoredProc.Create(nil, Controller) as IsmxDataSet;
    //else
      //raise EsmxDBInterfaceError.CreateRes(@SDBIntfDataSetUnknown);
  end;
  //Result := TsmxBDEQuery.Create(DataSetType);
  //Result.Database := Self;
  //Result := nil;
  {if Assigned(Result) then
    Result.Database := Self else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDataSetInvalid);}
end;

procedure TsmxBDEDatabase.StartTransaction;
begin
  InternalDatabase.StartTransaction;
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

{constructor TsmxBDEParam.Create(AParam: TParam; ADataSet: TsmxCustomDataSet);
begin
  inherited Create(ADataSet);
  FParam := AParam;
  //FLocation := plConst;
  //FDataSet := ADataSet;
end;}

{destructor TsmxBDEParam.Destroy;
begin
  //FDataSetIntf := nil;
  //FParam := nil;
  //FNotificationIntf := nil;
  if Assigned(FParam) then
    FParam.Free;
  inherited Destroy;
end;}

{procedure TsmxBDEParam.AssignParam(Source: TObject);
begin
  FParam.Assign(TPersistent(Source));
end;}

(*procedure TsmxBDEParam.AssignParam(const Source: IsmxParam);

  {procedure AssignFromParam(AParam: IsmxParam);
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
  end;}

//var
  //Param: IsmxParam; Field: IsmxField;
begin
  inherited AssignParam(Source);
  if Assigned(Source) then
  begin
    //if Source.QueryInterface(IsmxParam, Param) = S_OK then
      //p := (Source as IsmxParam).GetParam else
      //p := ParamIntf.GetParam else
      //AssignFromParam(Param) else
    //if Source.QueryInterface(IsmxField, Field) = S_OK then
      //p := (Source as IsmxField).GetField else
      //p := FieldIntf.GetField else
      //AssignFromField(Field);
    DataType := Source.DataType;
    //if AParam.IsNull then
    //  Clear
    //else
    //  Value := AParam.Value;
    NumericScale := Source.NumericScale;
    //ParamName := Source.ParamName;
    ParamType := Source.ParamType;
    Precision := Source.Precision;
    //ParamLocation := Source.ParamLocation;
    Size := Source.Size;
    Value := Source.Value;
  end;// else
    //Clear;
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
end;*)

{procedure TsmxBDEParam.AssignParam(const Source: IsmxField);
begin
  inherited AssignParam(Source);
  if Assigned(Source) then
  begin
    DataType := Source.DataType;
    Value := Source.Value;
    //ParamName := Source.FieldName;
    Size := Source.Size;
    if Source.DataType in [ftBCD, ftFMTBcd] then
      NumericScale := Source.Size;
    Precision := Source.Precision;
  end;
end;}

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

{function TsmxBDEParam.GetDataType: TsmxDataType;
begin
  if Assigned(Param) then
    Result := Param.DataType else
    Result := ftUnknown;
end;}

function TsmxBDEParam.GetInternalRef: Pointer;
begin
  Result := Pointer(FParam);
end;

{function TsmxBDEParam.GetNumericScale: Integer;
begin
  if Assigned(Param) then
    Result := Param.NumericScale else
    Result := 0;
end;}

{function TsmxBDEParam.GetParam: TParam;
begin
  //if TObject(InternalRef) is TParam then
    //Result := TParam(InternalRef) else
    //Result := nil;
  if not Assigned(FParam) then
  begin
    //if InternalDataSet is TsmxBDEQuery then
      //FParam := TParam.Create(TsmxBDEQuery(InternalDataSet).InternalParams) else
    //if InternalDataSet is TsmxBDEStoredProc then
      //FParam := TParam.Create(TsmxBDEStoredProc(InternalDataSet).InternalParams) else
      FParam := TParam.Create(nil);
      SetInternalDataSet(GetInternalDataSet);
  end;
  Result := FParam;
end;}

{function TsmxBDEParam.GetParamName: String;
begin
  if Assigned(Param) then
    Result := Param.Name else
    Result := '';
end;

function TsmxBDEParam.GetParamIndex: Integer;
begin
  if Assigned(Param) then
    Result := Param.Index else
    Result := -1;
end;}

{procedure TsmxBDEParam.SetParamIndex(Value: Integer);
begin
  if Assigned(Param) then
    Param.Index := Value;
end;}

(*function TsmxBDEParam.GetParamType: TsmxParamType;
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
  if Assigned(Param) then
    Result := TsmxParamType(Param.ParamType) else
    Result := ptUnknown; //DirectionToType[FParam.Direction];
end;*)

{function TsmxBDEParam.GetPrecision: Integer;
begin
  if Assigned(Param) then
    Result := Param.Precision else
    Result := 0
end;}

{function TsmxBDEParam.GetReference: Pointer;
begin
  Result := Pointer(Self);
end;}

{function TsmxBDEParam.GetSize: Integer;
begin
  if Assigned(Param) then
    Result := Param.Size else
    Result := 0;
end;}

{function TsmxBDEParam.GetValue: Variant;
begin
  if Assigned(Param) then
    Result := Param.Value else
    Result := Variants.Null;
end;}

{function TsmxBDEParam.GetVersion: String;
begin
  Result := Vers;
end;}

procedure TsmxBDEParam.LoadFromStream(Stream: TStream);
var
  Str: String;
begin
  //FParam.LoadFromStream(Stream, ftBlob);
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
  {if ParamName <> Value then
  begin
    InternalDataSet.LocationList.Locations[Value] :=
      InternalDataSet.LocationList.Locations[ParamName];
    InternalDataSet.LocationList.Locations[ParamName] :=
      plConst;
  end;}
  if Assigned(FParam) then
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

{function TsmxBDEParam.GetParamLocation: TsmxParamLocation;
begin
  Result := InternalDataSet.LocationList.Locations[ParamName];
end;

procedure TsmxBDEParam.SetParamLocation(Value: TsmxParamLocation);
begin
  InternalDataSet.LocationList.Locations[ParamName] := Value;
end;}

{function TsmxBDEParam.GetIsBlob: Boolean;
begin
  Result := smxFuncs.IsBlobType(FParam.DataType, Length(FParam.Value));
end;}

{procedure TsmxBDEParam.Clear;
begin
  inherited Clear;
  //Assigned(Param) then
    //Param.Clear;
  DataType := ftUnknown;
  NumericScale := 0;
  ParamType := ptUnknown;
  Precision := 0;
  Size := 0;
  Value := Variants.Null;
end;}

function TsmxBDEParam.IsBlob: Boolean;
begin
  if Assigned(FParam) then
    Result := smxDBFuncs.IsBlobType(FParam.DataType, FParam.Size) else
    Result := False;
end;

function TsmxBDEParam.IsNull: Boolean;
begin
  if Assigned(FParam) then
    Result := FParam.IsNull else
    Result := False;
end;

{function TsmxBDEParam.GetDataSet: IsmxDataSet;
begin
  if not Assigned(FDataSetIntf) then
    InternalDataSet.GetInterface(IsmxDataSet, FDataSetIntf);
  Result := FDataSetIntf;
end;}

{procedure TsmxBDEParam.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Param) and (Operation = opRemove) then
    Param := nil;
end;}

{procedure TsmxBDEParam.OwnerFreeNotification(const Sender: IsmxFreeNotification);
begin
  if Assigned(Sender) then
    if Assigned(Sender.GetOwnerNotification) then
      if TObject(Sender.GetOwnerNotification.GetReference) = Param then
        Param := nil;
end;}

{procedure TsmxBDEParam.SetParam(Value: TParam);
begin
  FParam := Value;
  if FParam is TsmxParam then
    (TsmxParam(FParam) as IsmxFreeNotification).InsertFreeNotification(Self as IsmxFreeNotification);
end;}

{procedure TsmxBDEParam.SetInternalRef(Value: Pointer);
begin
  inherited SetInternalRef(Value);
  if TObject(Value) is TParam then
    FParam := TParam(Value) else
    FParam := nil;
end;}

{ TsmxBDEQuery }

(*constructor TsmxBDEQuery.Create(ADataSetType: TsmxDataSetType);
begin
  inherited Create(ADataSetType);
  //FDataSetType := ADataSetType;
  {case FDataSetType of
    dstQuery: FBDEDataSet := TQuery.Create(nil);
    dstStoredProc: FBDEDataSet := TStoredProc.Create(nil);
  end;}
  CreateDataSet;
end;*)

destructor TsmxBDEQuery.Destroy;
begin
  //if Assigned(FProcSQL) then
    //FProcSQL.Free;
  //if Assigned(FSenseList) then
    //FSenseList.Free;
  //if Assigned(FLocationList) then
    //FLocationList.Free;
  //FBDEDataSet.Free;
  //FDatabaseIntf := nil;
  inherited Destroy;
  if Assigned(FParams) then
    FParams.Free;
  if Assigned(FQuery) then
    FQuery.Free;
  //if Assigned(FInternalDataSet) then
    //FInternalDataSet.Free;
  //inherited Destroy;
end;

{procedure TsmxBDEQuery.CreateDataSet;
begin
  case DataSetType of
    dstQuery: FBDEDataSet := TQuery.Create(nil);
    dstStoredProc: FBDEDataSet := TStoredProc.Create(nil);
  end;
  ParamCheck := False;
end;

procedure TsmxBDEQuery.Add;
begin
  FBDEDataSet.Append;
end;}

(*function TsmxBDEQuery.AddField(const AFieldName: String): IsmxField;
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
    {with SenseList.Add do
    begin
      FieldName := AFieldName;
      FieldSense := ASense;
    end;}
  end;
end;*)

(*function TsmxBDEQuery.AddParam(DataType: TsmxDataType): IsmxParam;
var
  Param: TParam;
begin
  //inherited AddParam(ParamName);
  Param := TParam(InternalQuery.Params.Add);
  //Param.Name := ParamName;
  Param.DataType := DataType;
  Result := GetParamIntf(Param);
  {Param := nil;
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
  end;}
end;*)

procedure TsmxBDEQuery.AssignDataSet(const Source: IsmxDataSet);
begin
  inherited AssignDataSet(Source);
  if Assigned(Source) then
    SQLText := Source.SQLText;
end;

{procedure TsmxBDEQuery.ClearFields;
begin
  FBDEDataSet.Fields.Clear;
  SenseList.Clear;
end;}

{procedure TsmxBDEQuery.ClearParams;
begin
  inherited ClearParams;
  InternalQuery.Params.Clear;
end;}

{function TsmxBDEQuery.CreateStreamField(const Value: IsmxField): TStream;
begin
  if Value.Field is TField then
    Result := FADODataSet.CreateBlobStream(TField(Value.Field), bmRead) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeField);
end;}

{procedure TsmxBDEQuery.Close;
begin
  FBDEDataSet.Close;
end;}

procedure TsmxBDEQuery.Execute;
begin
  InternalQuery.Params.Assign(InternalParams);
  //if FBDEDataSet is TQuery then
    InternalQuery.ExecSQL {else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).ExecProc};
end;

(*function TsmxBDEQuery.FindParam(const ParamName: String): IsmxParam;
var
  Param: TParam;
begin
  Param := InternalQuery.Params.FindParam(ParamName);
  if Assigned(Param) then
    Result := GetParamIntf(Param) else
    Result := nil;
  {if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).Params.FindParam(AParamName) else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).Params.FindParam(AParamName) else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);}
end;*)

{function TsmxBDEQuery.FieldByName(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields.FieldByName(AFieldName);
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

function TsmxBDEQuery.FindField(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields.FindField(AFieldName);
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

procedure TsmxBDEQuery.First;
begin
  FBDEDataSet.First;
end;

function TsmxBDEQuery.GetActive: Boolean;
begin
  Result := FBDEDataSet.Active;
end;

function TsmxBDEQuery.Bof: Boolean;
begin
  Result := FBDEDataSet.Bof;
end;

function TsmxBDEQuery.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;}

{function TsmxBDEQuery.GetDataSet: TObject;
begin
  Result := FBDEDataSet;
end;}

function TsmxBDEQuery.GetDataSetType: TsmxDataSetType;
begin
  Result := dstQuery;
end;

{function TsmxBDEQuery.Eof: Boolean;
begin
  Result := FBDEDataSet.Eof;
end;

function TsmxBDEQuery.GetField(Index: Integer): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields[Index];
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

function TsmxBDEQuery.GetFieldCount: Integer;
begin
  Result := FBDEDataSet.FieldCount;
end;}

{function TsmxBDEQuery.GetIsDataSet: Boolean;
begin
  Result := FBDEDataSet is TDataSet;
end;}

(*function TsmxBDEQuery.GetParam(Index: Integer): IsmxParam;
var
  Param: TParam;
begin
  Param := InternalQuery.Params[Index];
  Result := GetParamIntf(Param);
  {if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).Params[Index] else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).Params[Index] else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);}
end;*)

(*function TsmxBDEQuery.GetParamCount: Integer;
begin
  //if FBDEDataSet is TQuery then
    Result := InternalQuery.ParamCount{ else
  if FBDEDataSet is TStoredProc then
    Result := TStoredProc(FBDEDataSet).ParamCount else
    Result := 0};
end;*)

function TsmxBDEQuery.GetPrepare: Boolean;
begin
  //if FBDEDataSet is TQuery then
    Result := InternalQuery.Prepared{ else
  if FBDEDataSet is TStoredProc then
    Result := TStoredProc(FBDEDataSet).Prepared else
    Result := False};
end;

function TsmxBDEQuery.GetSQLText: String;
begin
  Result := InternalQuery.SQL.Text;
end;

procedure TsmxBDEQuery.SetSQLText(const Value: String);
begin
  InternalQuery.SQL.Text := Value;
end;

{function TsmxBDEQuery.GetRecordNo: Integer;
begin
  Result := FBDEDataSet.RecNo;
end;

function TsmxBDEQuery.GetRecordCount: Integer;
begin
  Result := FBDEDataSet.RecordCount;
end;}

(*function TsmxBDEQuery.GetSQL: TStrings;
begin
  //if FBDEDataSet is TQuery then
    Result := InternalQuery.SQL{ else
  if FBDEDataSet is TStoredProc then
    Result := ProcSQL else
    Result := nil};
end;*)

{function TsmxBDEQuery.GetVersion: String;
begin
  Result := Vers;
end;}

{procedure TsmxBDEQuery.Last;
begin
  FBDEDataSet.Last;
end;}

{procedure TsmxBDEQuery.LoadStreamParam(const Value: IsmxParam; Stream: TStream);
begin
  if Value.Param is TParameter then
    TParameter(Value.Param).LoadFromStream(Stream, ftBlob) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeParam);
end;}

{function TsmxBDEQuery.Locate(const AKeyFields: String; const AKeyValues: Variant): Boolean;
begin
  Result := FBDEDataSet.Locate(AKeyFields, AKeyValues, []);
end;}

{function TsmxBDEQuery.Locate(ASense: TsmxFieldSense; const AKeyValue: Variant): Boolean;
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

{procedure TsmxBDEQuery.Next;
begin
  FBDEDataSet.Next;
end;}

procedure TsmxBDEQuery.Open;
begin
  InternalQuery.Params.Assign(InternalParams);
  InternalQuery.Open;
end;

(*function TsmxBDEQuery.ParamByName(const ParamName: String): IsmxParam;
var
  Param: TParam;
begin
  Param := InternalQuery.ParamByName(ParamName);
  Result := GetParamIntf(Param);
  {if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).ParamByName(AParamName) else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).ParamByName(AParamName) else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);}
end;*)

{procedure TsmxBDEQuery.Post;
begin
  FBDEDataSet.Post;
end;}

{procedure TsmxBDEQuery.Prepare;
begin
end;}

{procedure TsmxBDEQuery.Prior;
begin
  FBDEDataSet.Prior;
end;

procedure TsmxBDEQuery.Remove;
begin
  FBDEDataSet.Delete;
end;}

(*procedure TsmxBDEQuery.DeleteField(const AField: IsmxField);
var
  Field: TField;
begin
  {f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields.Remove(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  {if Assigned(AField) then
  begin
    //SenseList.Senses[AField.FieldName] := fsGeneral;
    SenseList.Remove(SenseList.FindByName(AField.FieldName));
    Field := FBDEDataSet.Fields.FindField(AField.FieldName);
    if Assigned(Field) then
      FBDEDataSet.Fields.Remove(Field);
  end;}
end;*)

(*procedure TsmxBDEQuery.DeleteParam(const Param: IsmxParam);
var
  AParam: TParam;
begin
  inherited DeleteParam(Param);
  if Assigned(Param) then
  begin
    AParam := InternalQuery.Params.FindParam(Param.ParamName);
    if Assigned(AParam) then
      InternalQuery.Params.RemoveParam(AParam);
  end;
  {p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params.RemoveParam(TParam(p)) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params.RemoveParam(TParam(p));
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
  {if Assigned(AParam) then
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
  end;}
end;*)

{procedure TsmxBDEQuery.SetActive(Value: Boolean);
begin
  FBDEDataSet.Active := Value;
end;}

//type
  //_TDBDataSet = class(TDBDataSet);
procedure TsmxBDEQuery.SetDatabase(const Value: IsmxDatabase);
//var
  //Database: TObject;
  //dbf: TDBFlags;
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
  if Assigned(Database) then
  begin
    //dbf := _TDBDataSet(InternalQuery).DBFlags;
    //if dbfOpened in dbf then
      //inf('yes');
    InternalQuery.Close;
    InternalQuery.UnPrepare;
    InternalQuery.DatabaseName := '';
  end;
  inherited SetDatabase(Value);
  if Assigned(Database) then
    if TObject(Database.GetInternalRef) is TDatabase then
      InternalQuery.DatabaseName := TDatabase(Database.GetInternalRef).DatabaseName;
  {if Database <> Value then
  begin
    FDatabaseIntf := Value;
    if Assigned(FDatabaseIntf) and (TObject(FDatabaseIntf.GetInternalRef) is TDatabase) then
      FBDEDataSet.DatabaseName := TDatabase(FDatabaseIntf.GetInternalRef).DatabaseName else
      FBDEDataSet.DatabaseName := '';
  end;}
end;

(*procedure TsmxBDEQuery.SetField(Index: Integer; const Value: IsmxField);
var
  Field: IsmxField;
begin
  {f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields[Index] := TField(f) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  Field := TsmxField.Create(FBDEDataSet.Fields[Index], Self);
  Field.AssignField(Value);
end;*)

(*procedure TsmxBDEQuery.SetParam(Index: Integer; const Value: IsmxParam);
var
  Param: IsmxParam;
begin
  Param := GetParamIntf(InternalQuery.Params[Index]);
  Param.AssignParam(Value);
  {p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params[Index] := TParam(p) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params[Index] := TParam(p);
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
  {if FBDEDataSet is TQuery then
  begin
    Param := TsmxBDEParam.Create(TQuery(FBDEDataSet).Params[Index], Self);
    Param.AssignParam(Value);
  end else
  if FBDEDataSet is TStoredProc then
  begin
    Param := TsmxBDEParam.Create(TStoredProc(FBDEDataSet).Params[Index], Self);
    Param.AssignParam(Value);
  end;}
end;*)

procedure TsmxBDEQuery.SetPrepare(Value: Boolean);
begin
  //if FBDEDataSet is TQuery then
  //if InternalQuery.DatabaseName <> '' then
    InternalQuery.Prepared := Value {else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).Prepared := Value};
  
end;

{procedure TsmxBDEQuery.SetRecordNo(Value: Integer);
begin
  FBDEDataSet.RecNo := Value;
end;}

(*procedure TsmxBDEQuery.SetSQL(Value: TStrings);
begin
  //if FBDEDataSet is TQuery then
    InternalQuery.SQL := Value {else
  if FBDEDataSet is TStoredProc then
    ProcSQL.Assign(Value)};
end;*)

{function TsmxBDEQuery.IsEmpty: Boolean;
begin
  Result := FBDEDataSet.IsEmpty;
end;}

{procedure TsmxBDEQuery.SetDataSetType(Value: TsmxDataSetType);
begin

end;}

{function TsmxBDEQuery.FieldBySense(ASense: TsmxFieldSense;
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

function TsmxBDEQuery.ParamByLocation(ALocation: TsmxParamLocation;
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

{procedure TsmxBDEQuery.Edit;
begin
  FBDEDataSet.Edit;
end;}

{function TsmxBDEQuery.GetProcSQL: TStrings;
begin
  if not Assigned(FProcSQL) then
  begin
    FProcSQL := TStringList.Create;
    TStringList(FProcSQL).OnChange := ProcChangeText;
  end;
  Result := FProcSQL;
end;}

{procedure TsmxBDEQuery.ProcChangeText(Sender: TObject);
begin
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).StoredProcName :=
      StrUtils.AnsiReplaceStr(ProcSQL.Text, sLineBreak, '');
end;}

function TsmxBDEQuery.GetInternalRef: Pointer;
begin
  Result := Pointer(InternalQuery);
end;

{function TsmxBDEQuery.GetInternalDataSetClass: TComponentClass;
begin
  Result := TQuery;
end;}

{function TsmxBDEQuery.GetLocationList: TsmxLocationKit;
begin
  if not Assigned(FLocationList) then
    FLocationList := TsmxLocationKit.Create(TsmxLocationItem);
  Result := FLocationList;
end;}

{function TsmxBDEQuery.GetSenseList: TsmxSenseKit;
begin
  if not Assigned(FSenseList) then
    FSenseList := TsmxSenseKit.Create(TsmxSenseItem);
  Result := FSenseList;
end;}

function TsmxBDEQuery.GetInternalParams: TParams;
begin
  if not Assigned(FParams) then
    FParams := TParams.Create;
  Result := FParams;
end;

function TsmxBDEQuery.GetInternalQuery: TQuery;
begin
  if not Assigned(FQuery) then
  //if not Assigned(FInternalDataSet) then
  //begin
    FQuery := TQuery.Create(nil);
    //FQuery.ParamCheck := False;
    //FQuery.RequestLive := True;
  //end;
  Result := FQuery;
end;

{function TsmxBDEQuery.GetParamIntf(Param: TParam): IsmxParam;
begin
  Result := TsmxBDEParam.Create(Self) as IsmxParam;
  Result.InternalRef := Pointer(Param);
end;}

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

{procedure TsmxCoBDEDatabase.Initialize;
begin
  inherited Initialize;
  FDatabaseIntf := TsmxBDEDatabase.Create as IsmxDatabase;
end;}

function TsmxCoBDEDatabase.CreateDatabase: IsmxDatabase;
begin
  Result := TsmxBDEDatabase.Create(nil, Self as IsmxBaseInterface) as IsmxDatabase;
end;

{ TsmxParam }

{constructor TsmxParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFreeNotificationIntf := TsmxFreeNotificationObject.Create(
    Self as IsmxOwnerNotification) as IsmxFreeNotification;
end;

destructor TsmxParam.Destroy;
begin
  FFreeNotificationIntf := nil;
  inherited Destroy;
end;

function TsmxParam.GetReference: Pointer;
begin
  Result := Pointer(Self);
end;

procedure TsmxParam.OwnerFreeNotification(const Sender: IsmxFreeNotification);
begin
end;

function TsmxParam._AddRef: Integer;
begin
  Result := -1;
end;

function TsmxParam._Release: Integer;
begin
  Result := -1;
end;

function TsmxParam.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := FFreeNotificationIntf.QueryInterface(IID, Obj);
end;}

{ TsmxBDEStoredProc }

destructor TsmxBDEStoredProc.Destroy;
begin
  //if Assigned(FProcSQL) then
    //FProcSQL.Free;
  //if Assigned(FSenseList) then
    //FSenseList.Free;
  //if Assigned(FLocationList) then
    //FLocationList.Free;
  //FBDEDataSet.Free;
  //FDatabaseIntf := nil;
  inherited Destroy;
  if Assigned(FParams) then
    FParams.Free;
  if Assigned(FStoredProc) then
    FStoredProc.Free;
  //if Assigned(FInternalDataSet) then
    //FInternalDataSet.Free;
  //inherited Destroy;
end;

{procedure TsmxBDEQuery.CreateDataSet;
begin
  case DataSetType of
    dstQuery: FBDEDataSet := TQuery.Create(nil);
    dstStoredProc: FBDEDataSet := TStoredProc.Create(nil);
  end;
  ParamCheck := False;
end;

procedure TsmxBDEQuery.Add;
begin
  FBDEDataSet.Append;
end;}

(*function TsmxBDEQuery.AddField(const AFieldName: String): IsmxField;
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
    {with SenseList.Add do
    begin
      FieldName := AFieldName;
      FieldSense := ASense;
    end;}
  end;
end;*)

(*function TsmxBDEStoredProc.AddParam(DataType: TsmxDataType): IsmxParam;
var
  Param: TParam;
begin
  //inherited AddParam(ParamName);
  Param := TParam(InternalStoredProc.Params.Add);
  //Param.Name := ParamName;
  Param.DataType := DataType;
  Result := GetParamIntf(Param);
  {Param := nil;
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
  end;}
end;*)

procedure TsmxBDEStoredProc.AssignDataSet(const Source: IsmxDataSet);
begin
  inherited AssignDataSet(Source);
  if Assigned(Source) then
    SQLText := Source.SQLText;
end;

{procedure TsmxBDEStoredProc.ClearFields;
begin
  FBDEDataSet.Fields.Clear;
  SenseList.Clear;
end;}

{procedure TsmxBDEStoredProc.ClearParams;
begin
  inherited ClearParams;
  InternalStoredProc.Params.Clear;
end;}

{function TsmxBDEStoredProc.CreateStreamField(const Value: IsmxField): TStream;
begin
  if Value.Field is TField then
    Result := FADODataSet.CreateBlobStream(TField(Value.Field), bmRead) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeField);
end;}

{procedure TsmxBDEStoredProc.Close;
begin
  FBDEDataSet.Close;
end;}

procedure TsmxBDEStoredProc.Execute;
begin
  InternalStoredProc.Params.Assign(InternalParams);
  //if FBDEDataSet is TQuery then
    InternalStoredProc.ExecProc {else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).ExecProc};
end;

(*function TsmxBDEStoredProc.FindParam(const ParamName: String): IsmxParam;
var
  Param: TParam;
begin
  Param := InternalStoredProc.Params.FindParam(ParamName);
  if Assigned(Param) then
    Result := GetParamIntf(Param) else
    Result := nil;
  {if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).Params.FindParam(AParamName) else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).Params.FindParam(AParamName) else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);}
end;*)

{function TsmxBDEStoredProc.FieldByName(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields.FieldByName(AFieldName);
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

function TsmxBDEStoredProc.FindField(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields.FindField(AFieldName);
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

procedure TsmxBDEStoredProc.First;
begin
  FBDEDataSet.First;
end;

function TsmxBDEStoredProc.GetActive: Boolean;
begin
  Result := FBDEDataSet.Active;
end;

function TsmxBDEStoredProc.Bof: Boolean;
begin
  Result := FBDEDataSet.Bof;
end;

function TsmxBDEStoredProc.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;}

{function TsmxBDEStoredProc.GetDataSet: TObject;
begin
  Result := FBDEDataSet;
end;}

function TsmxBDEStoredProc.GetDataSetType: TsmxDataSetType;
begin
  Result := dstStoredProc;
end;

{function TsmxBDEStoredProc.Eof: Boolean;
begin
  Result := FBDEDataSet.Eof;
end;

function TsmxBDEStoredProc.GetField(Index: Integer): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  Field := FBDEDataSet.Fields[Index];
  if Assigned(Field) then
    Result := TsmxField.Create(Field, Self);
end;

function TsmxBDEStoredProc.GetFieldCount: Integer;
begin
  Result := FBDEDataSet.FieldCount;
end;}

{function TsmxBDEStoredProc.GetIsDataSet: Boolean;
begin
  Result := FBDEDataSet is TDataSet;
end;}

(*function TsmxBDEStoredProc.GetParam(Index: Integer): IsmxParam;
var
  Param: TParam;
begin
  Param := InternalStoredProc.Params[Index];
  Result := GetParamIntf(Param);
  {if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).Params[Index] else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).Params[Index] else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);}
end;*)

(*function TsmxBDEStoredProc.GetParamCount: Integer;
begin
  //if FBDEDataSet is TQuery then
    Result := InternalStoredProc.ParamCount{ else
  if FBDEDataSet is TStoredProc then
    Result := TStoredProc(FBDEDataSet).ParamCount else
    Result := 0};
end;*)

function TsmxBDEStoredProc.GetPrepare: Boolean;
begin
  //if FBDEDataSet is TQuery then
    Result := InternalStoredProc.Prepared{ else
  if FBDEDataSet is TStoredProc then
    Result := TStoredProc(FBDEDataSet).Prepared else
    Result := False};
end;

{function TsmxBDEStoredProc.GetRecordNo: Integer;
begin
  Result := FBDEDataSet.RecNo;
end;

function TsmxBDEStoredProc.GetRecordCount: Integer;
begin
  Result := FBDEDataSet.RecordCount;
end;}

{function TsmxBDEStoredProc.GetSQL: TStrings;
begin
  //if FBDEDataSet is TQuery then
    //Result := InternalQuery.SQL else
  //if FBDEDataSet is TStoredProc then
    Result := ProcSQL; // else
    //Result := nil;
end;}

{function TsmxBDEStoredProc.GetVersion: String;
begin
  Result := Vers;
end;}

{procedure TsmxBDEStoredProc.Last;
begin
  FBDEDataSet.Last;
end;}

{procedure TsmxBDEStoredProc.LoadStreamParam(const Value: IsmxParam; Stream: TStream);
begin
  if Value.Param is TParameter then
    TParameter(Value.Param).LoadFromStream(Stream, ftBlob) else
    raise EsmxDBInterfaceError.CreateRes(@SInvalidTypeParam);
end;}

{function TsmxBDEStoredProc.Locate(const AKeyFields: String; const AKeyValues: Variant): Boolean;
begin
  Result := FBDEDataSet.Locate(AKeyFields, AKeyValues, []);
end;}

{function TsmxBDEStoredProc.Locate(ASense: TsmxFieldSense; const AKeyValue: Variant): Boolean;
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

{procedure TsmxBDEStoredProc.Next;
begin
  FBDEDataSet.Next;
end;}

procedure TsmxBDEStoredProc.Open;
begin
  InternalStoredProc.Params.Assign(InternalParams);
  InternalStoredProc.Open;
end;

(*function TsmxBDEStoredProc.ParamByName(const ParamName: String): IsmxParam;
var
  Param: TParam;
begin
  Param := InternalStoredProc.ParamByName(ParamName);
  Result := GetParamIntf(Param);
  {if FBDEDataSet is TQuery then
    Param := TQuery(FBDEDataSet).ParamByName(AParamName) else
  if FBDEDataSet is TStoredProc then
    Param := TStoredProc(FBDEDataSet).ParamByName(AParamName) else
    Param := nil;
  if Assigned(Param) then
    Result := TsmxBDEParam.Create(Param, Self);}
end;*)

{procedure TsmxBDEStoredProc.Post;
begin
  FBDEDataSet.Post;
end;}

{procedure TsmxBDEStoredProc.Prepare;
begin
end;}

{procedure TsmxBDEStoredProc.Prior;
begin
  FBDEDataSet.Prior;
end;

procedure TsmxBDEStoredProc.Remove;
begin
  FBDEDataSet.Delete;
end;}

(*procedure TsmxBDEStoredProc.DeleteField(const AField: IsmxField);
var
  Field: TField;
begin
  {f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields.Remove(TField(f)) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  {if Assigned(AField) then
  begin
    //SenseList.Senses[AField.FieldName] := fsGeneral;
    SenseList.Remove(SenseList.FindByName(AField.FieldName));
    Field := FBDEDataSet.Fields.FindField(AField.FieldName);
    if Assigned(Field) then
      FBDEDataSet.Fields.Remove(Field);
  end;}
end;*)

(*procedure TsmxBDEStoredProc.DeleteParam(const Param: IsmxParam);
var
  AParam: TParam;
begin
  inherited DeleteParam(Param);
  if Assigned(Param) then
  begin
    AParam := InternalStoredProc.Params.FindParam(Param.ParamName);
    if Assigned(AParam) then
      InternalStoredProc.Params.RemoveParam(AParam);
  end;
  {p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params.RemoveParam(TParam(p)) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params.RemoveParam(TParam(p));
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
  {if Assigned(AParam) then
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
  end;}
end;*)

{procedure TsmxBDEStoredProc.SetActive(Value: Boolean);
begin
  FBDEDataSet.Active := Value;
end;}

procedure TsmxBDEStoredProc.SetDatabase(const Value: IsmxDatabase);
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
  {if Database <> Value then
  begin
    FDatabaseIntf := Value;
    if Assigned(FDatabaseIntf) and (TObject(FDatabaseIntf.GetInternalRef) is TDatabase) then
      FBDEDataSet.DatabaseName := TDatabase(FDatabaseIntf.GetInternalRef).DatabaseName else
      FBDEDataSet.DatabaseName := '';
  end;}
end;

(*procedure TsmxBDEStoredProc.SetField(Index: Integer; const Value: IsmxField);
var
  Field: IsmxField;
begin
  {f := Value.GetField;
  if f is TField then
    FBDEDataSet.Fields[Index] := TField(f) else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfFieldInvalid);}
  Field := TsmxField.Create(FBDEDataSet.Fields[Index], Self);
  Field.AssignField(Value);
end;*)

(*procedure TsmxBDEStoredProc.SetParam(Index: Integer; const Value: IsmxParam);
var
  Param: IsmxParam;
begin
  Param := GetParamIntf(InternalStoredProc.Params[Index]);
  Param.AssignParam(Value);
  {p := Value.GetParam;
  if p is TParam then
  begin
    if FBDEDataSet is TQuery then
      TQuery(FBDEDataSet).Params[Index] := TParam(p) else
    if FBDEDataSet is TStoredProc then
      TStoredProc(FBDEDataSet).Params[Index] := TParam(p);
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfParamInvalid);}
  {if FBDEDataSet is TQuery then
  begin
    Param := TsmxBDEParam.Create(TQuery(FBDEDataSet).Params[Index], Self);
    Param.AssignParam(Value);
  end else
  if FBDEDataSet is TStoredProc then
  begin
    Param := TsmxBDEParam.Create(TStoredProc(FBDEDataSet).Params[Index], Self);
    Param.AssignParam(Value);
  end;}
end;*)

procedure TsmxBDEStoredProc.SetPrepare(Value: Boolean);
begin
  //if FBDEDataSet is TQuery then
  //if InternalStoredProc.DatabaseName <> '' then
    InternalStoredProc.Prepared := Value {else
  if FBDEDataSet is TStoredProc then
    TStoredProc(FBDEDataSet).Prepared := Value};
end;

{procedure TsmxBDEStoredProc.SetRecordNo(Value: Integer);
begin
  FBDEDataSet.RecNo := Value;
end;}

{procedure TsmxBDEStoredProc.SetSQL(Value: TStrings);
begin
  //if FBDEDataSet is TQuery then
    //InternalQuery.SQL := Value else
  //if FBDEDataSet is TStoredProc then
  ProcSQL.Assign(Value);
end;}

{function TsmxBDEStoredProc.IsEmpty: Boolean;
begin
  Result := FBDEDataSet.IsEmpty;
end;}

{procedure TsmxBDEStoredProc.SetDataSetType(Value: TsmxDataSetType);
begin

end;}

{function TsmxBDEStoredProc.FieldBySense(ASense: TsmxFieldSense;
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

function TsmxBDEStoredProc.ParamByLocation(ALocation: TsmxParamLocation;
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

{procedure TsmxBDEStoredProc.Edit;
begin
  FBDEDataSet.Edit;
end;}

{function TsmxBDEStoredProc.GetProcSQL: TStrings;
begin
  if not Assigned(FProcSQL) then
  begin
    FProcSQL := TStringList.Create;
    TStringList(FProcSQL).OnChange := ProcSQLChange;
  end;
  Result := FProcSQL;
end;}

{procedure TsmxBDEStoredProc.ProcSQLChange(Sender: TObject);
begin
  InternalStoredProc.StoredProcName :=
    StrUtils.AnsiReplaceStr(ProcSQL.Text, sLineBreak, '');
end;}

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

{function TsmxBDEStoredProc.GetParamIntf(Param: TParam): IsmxParam;
begin
  Result := TsmxBDEParam.Create(Self) as IsmxParam;
  Result.InternalRef := Pointer(Param);
end;}

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

{ TsmxReqBDEQuery }

function TsmxReqBDEQuery.GetDataSetClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxBDEQuery;
end;

{ TsmxReqBDEStoredProc }

function TsmxReqBDEStoredProc.GetDataSetClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxBDEStoredProc;
end;

initialization
  Classes.RegisterClasses([TsmxBDEDatabase, TsmxBDEParam, TsmxBDEQuery,
    TsmxBDEStoredProc, TsmxReqBDEQuery, TsmxReqBDEStoredProc]);
  TComObjectFactory.Create(ComServer, TsmxCoBDEDatabase, CLSID_smxCoBDEDatabase,
    'smxCoBDEDatabase', '', ciMultiInstance, tmApartment);

finalization
  Classes.UnRegisterClasses([TsmxBDEDatabase, TsmxBDEParam, TsmxBDEQuery,
    TsmxBDEStoredProc, TsmxReqBDEQuery, TsmxReqBDEStoredProc]);

end.
