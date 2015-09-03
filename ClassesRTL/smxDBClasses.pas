{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Database classes           }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxDBClasses;

interface

uses
  Classes, DB, ComObj, smxBaseClasses, smxBaseIntf, smxDBIntf, smxManagerIntf,
  smxTypes, smxClassIntf;

type
  { TsmxCoDatabase }

  TsmxCoDatabase = class(TComObject, IsmxBaseInterface, IsmxDatabase)
  private
    FDatabase: TsmxInterfacedComponent;
    function GetDatabase: IsmxDatabase;
  protected
    function GetController: IsmxBaseInterface; virtual;
    function GetDatabaseClass: TsmxInterfacedComponentClass; virtual;
    function GetDescription: String; virtual;
    function GetVersion: String; virtual;
    function IsCountedObj: Boolean; virtual;

    property Database: IsmxDatabase read GetDatabase implements IsmxDatabase;
  public
    destructor Destroy; override;
    procedure Initialize; override;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  { TsmxFieldItem }

  TsmxFieldList = class;

  TsmxCustomField = class;

  TsmxFieldItem = class(TsmxObjectItem)
  private
    function GetObjectItem: TsmxCustomField;
    function GetKit: TsmxFieldList;
    procedure SetObjectItem(Value: TsmxCustomField);
    procedure SetKit(Value: TsmxFieldList);
  public
    property Kit: TsmxFieldList read GetKit write SetKit;
  published
    property ObjectItem: TsmxCustomField read GetObjectItem write SetObjectItem;
  end;

  { TsmxFieldList }

  TsmxFieldList = class(TsmxObjectList)
  private
    function GetItem(Index: Integer): TsmxFieldItem;
    procedure SetItem(Index: Integer; Value: TsmxFieldItem);
  public
    function Add: TsmxFieldItem;
    function FindByName(const FieldName: String): TsmxFieldItem;

    property Items[Index: Integer]: TsmxFieldItem read GetItem write SetItem; default;
  end;

  { TsmxParamItem }

  TsmxParamList = class;

  TsmxCustomParam = class;

  TsmxParamItem = class(TsmxObjectItem)
  private
    function GetObjectItem: TsmxCustomParam;
    function GetKit: TsmxParamList;
    procedure SetObjectItem(Value: TsmxCustomParam);
    procedure SetKit(Value: TsmxParamList);
  public
    property Kit: TsmxParamList read GetKit write SetKit;
  published
    property ObjectItem: TsmxCustomParam read GetObjectItem write SetObjectItem;
  end;

  { TsmxParamList }

  TsmxParamList = class(TsmxObjectList)
  private
    function GetItem(Index: Integer): TsmxParamItem;
    procedure SetItem(Index: Integer; Value: TsmxParamItem);
  public
    function Add: TsmxParamItem;
    function FindByName(const ParamName: String): TsmxParamItem;

    property Items[Index: Integer]: TsmxParamItem read GetItem write SetItem; default;
  end;

  { TsmxCustomDatabase }

  TsmxCustomDatabase = class(TsmxInterfacedComponent, IsmxDataEntity)
  private
    FDatabaseName: String;
  protected
    function GetDatabaseName: String;
    function IsmxDataEntity.GetDataEntityName = GetDatabaseName;
    procedure SetDatabaseName(const Value: String); virtual;
    procedure IsmxDataEntity.SetDataEntityName = SetDatabaseName;
  public
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  { TsmxCustomField }

  TsmxCustomDataSet = class;

  TsmxCustomField = class(TsmxInterfacedPersistent, IsmxObjectItem)
  private
    FDataType: TsmxDataType;
    FDisplayFormat: String;
    FFieldName: String;
    FDataSense: TsmxDataSense;
    FInternalDataSet: TsmxCustomDataSet;
    FPrecision: Integer;
    FSize: Integer;
    function GetFieldItem: TsmxFieldItem;
  protected
    FFieldItem: TsmxFieldItem;
    procedure ChangeObjectIndex(Value: Integer); virtual;
    procedure ChangeObjectOwner(Value: TPersistent); virtual;
    function GetDataSense: TsmxDataSense;
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetDisplayFormat: String;
    function GetFieldIndex: Integer;
    function GetFieldName: String;
    function GetPrecision: Integer;
    function GetSize: Integer;
    procedure SetDataSense(Value: TsmxDataSense); virtual;
    procedure SetDataType(Value: TsmxDataType); virtual;
    procedure SetDisplayFormat(const Value: String); virtual;
    procedure SetFieldIndex(Value: Integer);
    procedure SetFieldName(const Value: String); virtual;
    procedure SetInternalDataSet(Value: TsmxCustomDataSet);
    procedure SetPrecision(Value: Integer); virtual;
    procedure SetSize(Value: Integer); virtual;

    property FieldItem: TsmxFieldItem read GetFieldItem;
  public
    destructor Destroy; override;
    procedure AssignField(const Source: IsmxField); virtual;
    procedure Clear; virtual;

    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet write SetInternalDataSet;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
  published
    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex stored False;
    property FieldName: String read GetFieldName write SetFieldName;
  end;

  { TsmxField }

  TsmxField = class(TsmxCustomField, IsmxField)
  protected
    FField: TField;
    procedure ChangeObjectIndex(Value: Integer); override;
    procedure ChangeObjectOwner(Value: TPersistent); override;
    function GetInternalRef: Pointer; override;
    function GetValue: Variant;
    procedure SetDataType(Value: TsmxDataType); override;
    procedure SetDisplayFormat(const Value: String); override;
    procedure SetFieldName(const Value: String); override;
    procedure SetPrecision(Value: Integer); override;
    procedure SetSize(Value: Integer); override;
    procedure SetValue(const Value: Variant);
  public
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property Value: Variant read GetValue write SetValue;
  published
    property DataType;
    property DisplayFormat;
    property Precision;
    property Size;
  end;

  { TsmxCustomParam }

  TsmxCustomParam = class(TsmxInterfacedPersistent, IsmxObjectItem)
  private
    FInternalDataSet: TsmxCustomDataSet;
    FDataSense: TsmxDataSense;
    FDataType: TsmxDataType;
    FNumericScale: Integer;
    FDataLocation: TsmxDataLocation;
    FParamName: String;
    FParamType: TsmxParamType;
    FPrecision: Integer;
    FSize: Integer;
    FValue: Variant;
    function GetParamItem: TsmxParamItem;
  protected
    FParamItem: TsmxParamItem;
    procedure ChangeObjectIndex(Value: Integer); virtual;
    procedure ChangeObjectOwner(Value: TPersistent); virtual;
    function GetDataSet: IsmxDataSet;
    function GetDataLocation: TsmxDataLocation;
    function GetDataSense: TsmxDataSense;
    function GetDataType: TsmxDataType;
    function GetNumericScale: Integer;
    function GetParamIndex: Integer;
    function GetParamName: String;
    function GetParamType: TsmxParamType;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    procedure SetDataLocation(Value: TsmxDataLocation); virtual;
    procedure SetDataSense(Value: TsmxDataSense); virtual;
    procedure SetDataType(Value: TsmxDataType); virtual;
    procedure SetInternalDataSet(Value: TsmxCustomDataSet);
    procedure SetNumericScale(Value: Integer); virtual;
    procedure SetParamIndex(Value: Integer);
    procedure SetParamName(const Value: String); virtual;
    procedure SetParamType(Value: TsmxParamType); virtual;
    procedure SetPrecision(Value: Integer); virtual;
    procedure SetSize(Value: Integer); virtual;
    procedure SetValue(const Value: Variant); virtual;

    property ParamItem: TsmxParamItem read GetParamItem;
  public
    destructor Destroy; override;
    procedure AssignParam(const Source: IsmxParam); overload; virtual;
    procedure AssignParam(const Source: IsmxField); overload; virtual;
    procedure Clear; virtual;

    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet write SetInternalDataSet;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  published
    property DataLocation: TsmxDataLocation read GetDataLocation write SetDataLocation;
    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property ParamIndex: Integer read GetParamIndex write SetParamIndex stored False;
    property ParamName: String read GetParamName write SetParamName;
  end;

  { TsmxCustomDataSet }

  TsmxCustomDataSet = class(TsmxInterfacedComponent, IsmxDataEntity, IsmxObjectList)
  private
    FDatabaseIntf: IsmxDatabase;
    FDataSetName: String;
    FParamList: TsmxParamList;
    FFieldList: TsmxFieldList;
    FPerformanceMode: TsmxPerformanceMode;
    function GetFieldList: TsmxFieldList;
    function GetParamList: TsmxParamList;
    procedure SetFieldList(Value: TsmxFieldList);
    procedure SetParamList(Value: TsmxParamList);
  protected
    procedure CheckObjectClass(Item: TObject; ObjectClass: TPersistentClass);
    procedure CreateObject(Item: TObject); overload; virtual;
    procedure CreateObject(Item: TObject; ObjectClass: TPersistentClass); overload; virtual;
    procedure DestroyObject(Item: TObject); virtual;
    function GetDatabase: IsmxDatabase;
    function GetDataSetName: String;
    function IsmxDataEntity.GetDataEntityName = GetDataSetName;
    function GetFieldClass: TsmxInterfacedPersistentClass; virtual;
    function GetParamClass: TsmxInterfacedPersistentClass; virtual;
    function GetPerformanceMode: TsmxPerformanceMode;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    procedure SetField(Index: Integer; const Value: IsmxField);
    function GetParamCount: Integer;
    function GetParam(Index: Integer): IsmxParam;
    procedure SetDataSetName(const Value: String); virtual;
    procedure IsmxDataEntity.SetDataEntityName = SetDataSetName;
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;
  public
    destructor Destroy; override;
    function AddField: IsmxField;
    function AddParam: IsmxParam;
    procedure AssignDataSet(const Source: IsmxDataSet); virtual;
    procedure ClearFields;
    procedure ClearParams;
    procedure DeleteField(Index: Integer);
    procedure DeleteParam(Index: Integer);
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    function FindParam(const ParamName: String): IsmxParam;
    function ParamByName(const ParamName: String): IsmxParam;

    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property DataSetName: String read GetDataSetName write SetDataSetName;
    property FieldCount: Integer read GetFieldCount;
    property FieldList: TsmxFieldList read GetFieldList write SetFieldList;
    property Fields[Index: Integer]: IsmxField read GetField write SetField;
    property ParamCount: Integer read GetParamCount;
    property ParamList: TsmxParamList read GetParamList write SetParamList;
    property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
  published
    property PerformanceMode: TsmxPerformanceMode read GetPerformanceMode write SetPerformanceMode;
  end;

  { TsmxDataSet }

  TsmxDataSet = class(TsmxCustomDataSet)
  private
    function GetInternalDataSet: TDataSet;
  protected
    function GetActive: Boolean;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetRecordNo(Value: Integer);
    function GetFieldClass: TsmxInterfacedPersistentClass; override;

    property InternalDataSet: TDataSet read GetInternalDataSet;
  public
    procedure Add;
    function Bof: Boolean;
    procedure Cancel;
    procedure CreateInternalField(Field: TsmxCustomField); virtual;
    procedure DestroyInternalField(Field: TsmxCustomField); virtual;
    procedure CreateInternalParam(Param: TsmxCustomParam); virtual;
    procedure DestroyInternalParam(Param: TsmxCustomParam); virtual;
    procedure Close; virtual;
    procedure Delete;
    procedure Edit;
    function Eof: Boolean;
    procedure Execute; virtual;
    procedure First;
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TsmxLocateOptions = []): Boolean;
    procedure Next;
    procedure Open; virtual;
    procedure Perform;
    procedure Post;
    procedure Prior;

    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
  published
    property Active: Boolean read GetActive write SetActive stored False;
  end;

  { TsmxTargetRequest }

  TsmxTargetRequest = class(TsmxComponent)
  private
    FDatabaseIntf: IsmxDatabase;
    FDataSetIntf: IsmxDataSet;
    FParamList: TsmxParams;
    function GetParamList: TsmxParams;
    function GetValue(const Key: String): Variant;
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetValue(const Key: String; const Value: Variant);
  protected
    property DataSet: IsmxDataSet read FDataSetIntf;
    property ParamList: TsmxParams read GetParamList;
  public
    destructor Destroy; override;
    procedure ClearParams;
    procedure DoExecute(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil);
    function DoRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      ResField: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
    function ForRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
      Get: Boolean = False; Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function NewRequest(const SQLText: String = ''; RequestType: TsmxDataSetType = dstQuery;
      const DSFrom: IsmxDataSet = nil): IsmxDataSet;
    function PrepRequest(const Request: IsmxDataSet; Get: Boolean = True;
      const DSFrom: IsmxDataSet = nil): Boolean;

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[const Key: String]: Variant read GetValue write SetValue; default;
  end;

implementation

uses
  Variants, SysUtils, smxProcs, smxFuncs, smxConsts;

{ TsmxCoDatabase }

procedure TsmxCoDatabase.Initialize;
begin
  FDatabase := GetDatabaseClass.Create(Self as IsmxBaseInterface);
end;

destructor TsmxCoDatabase.Destroy;
begin
  FDatabase.Free;
  inherited Destroy;
end;

function TsmxCoDatabase.GetController: IsmxBaseInterface;
begin
  Result := nil;
end;

function TsmxCoDatabase.GetDatabase: IsmxDatabase;
begin
  Result := FDatabase as IsmxDatabase;
end;

function TsmxCoDatabase.GetDatabaseClass: TsmxInterfacedComponentClass;
begin
  Result := TsmxInterfacedComponent;
end;

function TsmxCoDatabase.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxCoDatabase.GetVersion: String;
begin
  Result := '';
end;

function TsmxCoDatabase.IsCountedObj: Boolean;
begin
  Result := True;
end;

{ TsmxFieldItem }

function TsmxFieldItem.GetObjectItem: TsmxCustomField;
begin
  Result := TsmxCustomField(inherited ObjectItem);
end;

procedure TsmxFieldItem.SetObjectItem(Value: TsmxCustomField);
begin
  inherited ObjectItem := Value;
end;

function TsmxFieldItem.GetKit: TsmxFieldList;
begin
  Result := TsmxFieldList(inherited Kit);
end;

procedure TsmxFieldItem.SetKit(Value: TsmxFieldList);
begin
  inherited Kit := Value;
end;

{ TsmxFieldList }

function TsmxFieldList.Add: TsmxFieldItem;
begin
  Result := TsmxFieldItem(inherited Add);
end;

function TsmxFieldList.FindByName(const FieldName: String): TsmxFieldItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Assigned(Items[i].ObjectItem) then
      if SysUtils.AnsiCompareText(Items[i].ObjectItem.FieldName, FieldName) = 0 then
      begin
        Result := Items[i];
        Break;
      end;
end;

function TsmxFieldList.GetItem(Index: Integer): TsmxFieldItem;
begin
  Result := TsmxFieldItem(inherited Items[Index]);
end;

procedure TsmxFieldList.SetItem(Index: Integer; Value: TsmxFieldItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxParamItem }

function TsmxParamItem.GetObjectItem: TsmxCustomParam;
begin
  Result := TsmxCustomParam(inherited ObjectItem);
end;

procedure TsmxParamItem.SetObjectItem(Value: TsmxCustomParam);
begin
  inherited ObjectItem := Value;
end;

function TsmxParamItem.GetKit: TsmxParamList;
begin
  Result := TsmxParamList(inherited Kit);
end;

procedure TsmxParamItem.SetKit(Value: TsmxParamList);
begin
  inherited Kit := Value;
end;

{ TsmxParamList }

function TsmxParamList.Add: TsmxParamItem;
begin
  Result := TsmxParamItem(inherited Add);
end;

function TsmxParamList.FindByName(const ParamName: String): TsmxParamItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Assigned(Items[i].ObjectItem) then
      if SysUtils.AnsiCompareText(Items[i].ObjectItem.ParamName, ParamName) = 0 then
      begin
        Result := Items[i];
        Break;
      end;
end;

function TsmxParamList.GetItem(Index: Integer): TsmxParamItem;
begin
  Result := TsmxParamItem(inherited Items[Index]);
end;

procedure TsmxParamList.SetItem(Index: Integer; Value: TsmxParamItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxCustomDatabase }

function TsmxCustomDatabase.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

procedure TsmxCustomDatabase.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
end;

{ TsmxCustomField }

destructor TsmxCustomField.Destroy;
begin
  SetInternalDataSet(nil);
  if Assigned(FFieldItem) then
  begin
    FFieldItem.FObjectItem := nil;
    FFieldItem.Free;
  end;
  inherited Destroy;
end;

procedure TsmxCustomField.AssignField(const Source: IsmxField);
begin
  if Assigned(Source) then
  begin
    DataSense := Source.DataSense;
    DataType := Source.DataType;
    DisplayFormat := Source.DisplayFormat;
    FieldName := Source.FieldName;
    Precision := Source.Precision;
    Size := Source.Size;
  end else
    Clear;
end;

procedure TsmxCustomField.ChangeObjectIndex(Value: Integer);
begin
end;

procedure TsmxCustomField.ChangeObjectOwner(Value: TPersistent);
begin
  FInternalDataSet := Value as TsmxCustomDataSet;
end;

function TsmxCustomField.GetFieldItem: TsmxFieldItem;
begin
  if not Assigned(FFieldItem) then
  begin
    FFieldItem := TsmxFieldItem.Create(nil);
    FFieldItem.FObjectItem := Pointer(Self as IsmxObjectItem);
  end;
  Result := FFieldItem;
end;

procedure TsmxCustomField.Clear;
begin
  DataSense := dsGeneral;
  DataType := ftUnknown;
  DisplayFormat := '';
  FieldName := '';
  Precision := 0;
  Size := 0;
end;

function TsmxCustomField.GetDataSet: IsmxDataSet;
begin
  if Assigned(FInternalDataSet) then
    Result := FInternalDataSet as IsmxDataSet
  else
    Result := nil;
end;

function TsmxCustomField.GetFieldName: String;
begin
  Result := FFieldName;
end;

procedure TsmxCustomField.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

function TsmxCustomField.GetDataSense: TsmxDataSense;
begin
  Result := FDataSense;
end;

procedure TsmxCustomField.SetDataSense(Value: TsmxDataSense);
begin
  FDataSense := Value;
end;

procedure TsmxCustomField.SetInternalDataSet(Value: TsmxCustomDataSet);
begin
  if Assigned(FInternalDataSet) then
    FieldItem.Kit := nil;
  if Assigned(Value) then
    FieldItem.Kit := Value.FieldList;
end;

function TsmxCustomField.GetDataType: TsmxDataType;
begin
  Result := FDataType;
end;

function TsmxCustomField.GetDisplayFormat: String;
begin
  Result := FDisplayFormat;
end;

function TsmxCustomField.GetFieldIndex: Integer;
begin
  Result := FieldItem.ItemIndex;
end;

function TsmxCustomField.GetPrecision: Integer;
begin
  Result := FPrecision;
end;

function TsmxCustomField.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TsmxCustomField.SetDataType(Value: TsmxDataType);
begin
  FDataType := Value;
end;

procedure TsmxCustomField.SetDisplayFormat(const Value: String);
begin
  FDisplayFormat := Value;
end;

procedure TsmxCustomField.SetFieldIndex(Value: Integer);
begin
  FieldItem.ItemIndex := Value;
end;

procedure TsmxCustomField.SetPrecision(Value: Integer);
begin
  FPrecision := Value;
end;

procedure TsmxCustomField.SetSize(Value: Integer);
begin
  FSize := Value;
end;

{ TsmxField }

procedure TsmxField.ChangeObjectIndex(Value: Integer);
begin
  inherited ChangeObjectIndex(Value);
  if Assigned(FField) then
    FField.Index := Value;
end;

procedure TsmxField.ChangeObjectOwner(Value: TPersistent);
begin
  if InternalDataSet is TsmxDataSet then
    TsmxDataSet(InternalDataSet).DestroyInternalField(Self);
  inherited ChangeObjectOwner(Value);
  if InternalDataSet is TsmxDataSet then
  begin
    TsmxDataSet(InternalDataSet).CreateInternalField(Self);
    SetDisplayFormat(GetDisplayFormat);
    SetFieldName(GetFieldName);
    SetPrecision(GetPrecision);
    SetSize(GetSize);
  end;
end;

procedure TsmxField.SetDataType(Value: TsmxDataType);
begin
  if GetDataType <> Value then
  begin
    if InternalDataSet is TsmxDataSet then
      TsmxDataSet(InternalDataSet).DestroyInternalField(Self);
    inherited SetDataType(Value);
    if InternalDataSet is TsmxDataSet then
    begin
      TsmxDataSet(InternalDataSet).CreateInternalField(Self);
      SetDisplayFormat(GetDisplayFormat);
      SetFieldName(GetFieldName);
      SetPrecision(GetPrecision);
      SetSize(GetSize);
    end;
  end;
end;

procedure TsmxField.SetDisplayFormat(const Value: String);
begin
  inherited SetDisplayFormat(Value);
  if FField is TAggregateField then
    TAggregateField(FField).DisplayFormat := Value else
  if FField is TDateTimeField then
    TDateTimeField(FField).DisplayFormat := Value else
  if FField is TNumericField then
    TNumericField(FField).DisplayFormat := Value else
  if FField is TSQLTimeStampField then
    TSQLTimeStampField(FField).DisplayFormat := Value;
end;

procedure TsmxField.SetFieldName(const Value: String);
begin
  inherited SetFieldName(Value);
  if Assigned(FField) and (Value <> '') then
    FField.FieldName := Value;
end;

procedure TsmxField.SetPrecision(Value: Integer);
begin
  inherited SetPrecision(Value);
  if FField is TBCDField then
    TBCDField(FField).Precision := Value else
  if FField is TFMTBCDField then
    TFMTBCDField(FField).Precision := Value;
end;

procedure TsmxField.SetSize(Value: Integer);
begin
  inherited SetSize(Value);
  if Assigned(FField) then
    FField.Size := Value;
end;

function TsmxField.GetValue: Variant;
begin
  if Assigned(FField) then
    Result := FField.Value
  else
    Result := Variants.Null;
end;

procedure TsmxField.SetValue(const Value: Variant);
begin
  if Assigned(FField) then
    FField.Value := Value;
end;

function TsmxField.GetInternalRef: Pointer;
begin
  Result := Pointer(FField);
end;

function TsmxField.IsBlob: Boolean;
begin
  if Assigned(FField) then
    Result := FField.IsBlob
  else
    Result := False;
end;

function TsmxField.IsNull: Boolean;
begin
  if Assigned(FField) then
    Result := FField.IsNull
  else
    Result := False;
end;

procedure TsmxField.LoadFromStream(Stream: TStream);
var
  Str: String;
begin
  if Assigned(FField) then
  begin
    smxProcs.StreamToStr(Stream, Str);
    FField.Value := smxFuncs.StrToVar(Str);
  end;
end;

procedure TsmxField.SaveToStream(Stream: TStream);
begin
  if Assigned(FField) then
    smxProcs.StrToStream(Variants.VarToStr(FField.Value), Stream);
end;

{ TsmxCustomParam }

destructor TsmxCustomParam.Destroy;
begin
  SetInternalDataSet(nil);
  if Assigned(FParamItem) then
  begin
    FParamItem.FObjectItem := nil;
    FParamItem.Free;
  end;
  inherited Destroy;
end;

procedure TsmxCustomParam.AssignParam(const Source: IsmxParam);
begin
  if Assigned(Source) then
  begin
    DataLocation := Source.DataLocation;
    DataSense := Source.DataSense;
    DataType := Source.DataType;
    NumericScale := Source.NumericScale;
    ParamName := Source.ParamName;
    ParamType := Source.ParamType;
    Precision := Source.Precision;
    Size := Source.Size;
    Value := Source.Value;
  end else
    Clear;
end;

procedure TsmxCustomParam.AssignParam(const Source: IsmxField);
begin
  if Assigned(Source) then
  begin
    DataSense := Source.DataSense;
    DataType := Source.DataType;
    if Source.DataType in [ftBCD, ftFMTBcd] then
      NumericScale := Source.Size;
    ParamName := Source.FieldName;
    Precision := Source.Precision;
    Size := Source.Size;
    Value := Source.Value;
  end else
    Clear;
end;

procedure TsmxCustomParam.Clear;
begin
  DataLocation := dlAssigned;
  DataSense := dsGeneral;
  DataType := ftUnknown;
  NumericScale := 0;
  ParamName := '';
  ParamType := ptUnknown;
  Precision := 0;
  Size := 0;
  Value := Variants.Null;
end;

function TsmxCustomParam.GetDataSet: IsmxDataSet;
begin
  if Assigned(FInternalDataSet) then
    Result := FInternalDataSet as IsmxDataSet
  else
    Result := nil;
end;

function TsmxCustomParam.GetDataSense: TsmxDataSense;
begin
  Result := FDataSense;
end;

procedure TsmxCustomParam.SetDataSense(Value: TsmxDataSense);
begin
  FDataSense := Value;
end;

procedure TsmxCustomParam.ChangeObjectIndex(Value: Integer);
begin
end;

procedure TsmxCustomParam.ChangeObjectOwner(Value: TPersistent);
begin
  FInternalDataSet := Value as TsmxCustomDataSet;
end;

function TsmxCustomParam.GetParamItem: TsmxParamItem;
begin
  if not Assigned(FParamItem) then
  begin
    FParamItem := TsmxParamItem.Create(nil);
    FParamItem.FObjectItem := Pointer(Self as IsmxObjectItem);
  end;
  Result := FParamItem;
end;

function TsmxCustomParam.GetDataType: TsmxDataType;
begin
  Result := FDataType;
end;

function TsmxCustomParam.GetNumericScale: Integer;
begin
  Result := FNumericScale;
end;

function TsmxCustomParam.GetParamIndex: Integer;
begin
  Result := ParamItem.ItemIndex;
end;

function TsmxCustomParam.GetDataLocation: TsmxDataLocation;
begin
  Result := FDataLocation;
end;

function TsmxCustomParam.GetParamType: TsmxParamType;
begin
  Result := FParamType;
end;

function TsmxCustomParam.GetPrecision: Integer;
begin
  Result := FPrecision;
end;

function TsmxCustomParam.GetSize: Integer;
begin
  Result := FSize;
end;

function TsmxCustomParam.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TsmxCustomParam.SetParamIndex(Value: Integer);
begin
  ParamItem.ItemIndex := Value;
end;

procedure TsmxCustomParam.SetDataLocation(Value: TsmxDataLocation);
begin
  FDataLocation := Value;
end;

function TsmxCustomParam.GetParamName: String;
begin
  Result := FParamName;
end;

procedure TsmxCustomParam.SetParamName(const Value: String);
begin
  FParamName := Value;
end;

procedure TsmxCustomParam.SetDataType(Value: TsmxDataType);
begin
  FDataType := Value;
end;

procedure TsmxCustomParam.SetInternalDataSet(Value: TsmxCustomDataSet);
begin
  if Assigned(FInternalDataSet) then
    ParamItem.Kit := nil;
  if Assigned(Value) then
    ParamItem.Kit := Value.ParamList;
end;

procedure TsmxCustomParam.SetNumericScale(Value: Integer);
begin
  FNumericScale := Value;
end;

procedure TsmxCustomParam.SetParamType(Value: TsmxParamType);
begin
  FParamType := Value;
end;

procedure TsmxCustomParam.SetPrecision(Value: Integer);
begin
  FPrecision := Value;
end;

procedure TsmxCustomParam.SetSize(Value: Integer);
begin
  FSize := Value;
end;

procedure TsmxCustomParam.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TsmxCustomDataSet }

destructor TsmxCustomDataSet.Destroy;
begin
  SetDatabase(nil);
  if Assigned(FFieldList) then
    FFieldList.Free;
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

function TsmxCustomDataSet.AddField: IsmxField;
begin
  Result := FieldList.Add.ObjectItem as IsmxField;
end;

function TsmxCustomDataSet.AddParam: IsmxParam;
begin
  Result := ParamList.Add.ObjectItem as IsmxParam;
end;

procedure TsmxCustomDataSet.AssignDataSet(const Source: IsmxDataSet);
var
  i: Integer;
begin
  ClearFields;
  ClearParams;
  if Assigned(Source) then
  begin
    PerformanceMode := Source.PerformanceMode;
    for i := 0 to Source.FieldCount - 1 do
      AddField.AssignField(Source.Fields[i]);
    for i := 0 to Source.ParamCount - 1 do
      AddParam.AssignParam(Source.Params[i]);
  end;
end;

procedure TsmxCustomDataSet.ClearFields;
begin
  FieldList.Clear;
end;

procedure TsmxCustomDataSet.ClearParams;
begin
  ParamList.Clear;
end;

procedure TsmxCustomDataSet.DeleteField(Index: Integer);
begin
  FieldList.Delete(Index);
end;

procedure TsmxCustomDataSet.DeleteParam(Index: Integer);
begin
  ParamList.Delete(Index);
end;

function TsmxCustomDataSet.GetParamList: TsmxParamList;
begin
  if not Assigned(FParamList) then
    FParamList := TsmxParamList.Create(Self, TsmxParamItem);
  Result := FParamList;
end;

procedure TsmxCustomDataSet.SetParamList(Value: TsmxParamList);
begin
  ParamList.Assign(Value);
end;

function TsmxCustomDataSet.GetFieldList: TsmxFieldList;
begin
  if not Assigned(FFieldList) then
    FFieldList := TsmxFieldList.Create(Self, TsmxFieldItem);
  Result := FFieldList;
end;

procedure TsmxCustomDataSet.SetFieldList(Value: TsmxFieldList);
begin
  FieldList.Assign(Value);
end;

function TsmxCustomDataSet.GetFieldClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxCustomField;
end;

function TsmxCustomDataSet.GetParamClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxCustomParam;
end;

procedure TsmxCustomDataSet.CreateObject(Item: TObject);
begin
  if Item is TsmxFieldItem then
    CreateObject(Item, GetFieldClass) else
  if Item is TsmxParamItem then
    CreateObject(Item, GetParamClass);
end;

procedure TsmxCustomDataSet.CreateObject(Item: TObject; ObjectClass: TPersistentClass);
var
  Field: TsmxCustomField;
  Param: TsmxCustomParam;
begin
  CheckObjectClass(Item, ObjectClass);
  if Item is TsmxFieldItem then
  begin
    if not Assigned(TsmxFieldItem(Item).FObjectItem) then
    begin
      Field := TsmxCustomField(TsmxInterfacedPersistentClass(ObjectClass).Create(nil));
      Field.FFieldItem := TsmxFieldItem(Item);
      TsmxFieldItem(Item).FObjectItem := Field;
      Field._AddRef;
    end;
  end else
  if Item is TsmxParamItem then
  begin
    if not Assigned(TsmxParamItem(Item).FObjectItem) then
    begin
      Param := TsmxCustomParam(TsmxInterfacedPersistentClass(ObjectClass).Create(nil));
      Param.FParamItem := TsmxParamItem(Item);
      TsmxParamItem(Item).FObjectItem := Param;
      Param._AddRef;
    end;
  end;
end;

procedure TsmxCustomDataSet.DestroyObject(Item: TObject);
var
  Field: TsmxCustomField;
  Param: TsmxCustomParam;
begin
  if Item is TsmxFieldItem then
  begin
    if Assigned(TsmxFieldItem(Item).FObjectItem) then
    begin
      Field := TsmxCustomField(TsmxFieldItem(Item).FObjectItem);
      TsmxFieldItem(Item).FObjectItem := nil;
      Field.FFieldItem := nil;
      Field._Release;
    end;
  end else
  if Item is TsmxParamItem then
  begin
    if Assigned(TsmxParamItem(Item).FObjectItem) then
    begin
      Param := TsmxCustomParam(TsmxParamItem(Item).FObjectItem);
      TsmxParamItem(Item).FObjectItem := nil;
      Param.FParamItem := nil;
      Param._Release;
    end;
  end;
end;

function TsmxCustomDataSet.GetDataSetName: String;
begin
  Result := FDataSetName;
end;

procedure TsmxCustomDataSet.SetDataSetName(const Value: String);
begin
  FDataSetName := Value;
end;

function TsmxCustomDataSet.FieldByName(const FieldName: String): IsmxField;
var
  FieldItem: TsmxFieldItem;
begin
  FieldItem := FieldList.FindByName(FieldName);
  if Assigned(FieldItem) then
    Result := FieldItem.ObjectItem as IsmxField
  else
    raise EsmxListError.CreateFmt(smxConsts.rsListItemNotFound, [FieldName]);
end;

function TsmxCustomDataSet.FindField(const FieldName: String): IsmxField;
var
  FieldItem: TsmxFieldItem;
begin
  FieldItem := FieldList.FindByName(FieldName);
  if Assigned(FieldItem) then
    Result := FieldItem.ObjectItem as IsmxField
  else
    Result := nil;
end;

function TsmxCustomDataSet.FindParam(const ParamName: String): IsmxParam;
var
  ParamItem: TsmxParamItem;
begin
  ParamItem := ParamList.FindByName(ParamName);
  if Assigned(ParamItem) then
    Result := ParamItem.ObjectItem as IsmxParam
  else
    Result := nil;
end;

function TsmxCustomDataSet.ParamByName(const ParamName: String): IsmxParam;
var
  ParamItem: TsmxParamItem;
begin
  ParamItem := ParamList.FindByName(ParamName);
  if Assigned(ParamItem) then
    Result := ParamItem.ObjectItem as IsmxParam
  else
    raise EsmxListError.CreateFmt(smxConsts.rsListItemNotFound, [ParamName]);
end;

function TsmxCustomDataSet.GetField(Index: Integer): IsmxField;
begin
  Result := FieldList[Index].ObjectItem as IsmxField;
end;

function TsmxCustomDataSet.GetFieldCount: Integer;
begin
  Result := FieldList.Count;
end;

function TsmxCustomDataSet.GetParam(Index: Integer): IsmxParam;
begin
  Result := ParamList[Index].ObjectItem as IsmxParam;
end;

function TsmxCustomDataSet.GetParamCount: Integer;
begin
  Result := ParamList.Count;
end;

procedure TsmxCustomDataSet.SetField(Index: Integer; const Value: IsmxField);
begin
  (FieldList[Index].ObjectItem as IsmxField).AssignField(Value);
end;

procedure TsmxCustomDataSet.SetParam(Index: Integer; const Value: IsmxParam);
begin
  (ParamList[Index].ObjectItem as IsmxParam).AssignParam(Value);
end;

function TsmxCustomDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxCustomDataSet.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxCustomDataSet.GetPerformanceMode: TsmxPerformanceMode;
begin
  Result := FPerformanceMode;
end;

procedure TsmxCustomDataSet.SetPerformanceMode(Value: TsmxPerformanceMode);
begin
  FPerformanceMode := Value;
end;

procedure TsmxCustomDataSet.CheckObjectClass(Item: TObject; ObjectClass: TPersistentClass);
var
  ObjectClassName: String;
begin
  if not Assigned(ObjectClass)
      or not Assigned(Item)
      or ((Item is TsmxFieldItem) and not ObjectClass.InheritsFrom(GetFieldClass))
      or ((Item is TsmxParamItem) and not ObjectClass.InheritsFrom(GetParamClass)) then
  begin
    if Assigned(ObjectClass) then
      ObjectClassName := ObjectClass.ClassName
    else
      ObjectClassName := 'nil';
    raise EsmxComponentError.CreateResFmt(@smxConsts.rsListItemClassError,
      [ObjectClassName, ClassName]);
  end;
end;

{ TsmxDataSet }

procedure TsmxDataSet.Add;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Append;
end;

procedure TsmxDataSet.Delete;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Delete;
end;

function TsmxDataSet.Bof: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Bof
  else
    Result := False;
end;

function TsmxDataSet.Eof: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Eof
  else
    Result := False;
end;

procedure TsmxDataSet.Cancel;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Cancel;
end;

procedure TsmxDataSet.Post;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Post;
end;

procedure TsmxDataSet.Close;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Close;
end;

procedure TsmxDataSet.Open;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Open;
end;

procedure TsmxDataSet.Edit;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Edit;
end;

procedure TsmxDataSet.First;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.First;
end;

procedure TsmxDataSet.Last;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Last;
end;

function TsmxDataSet.GetActive: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Active
  else
    Result := False;
end;

procedure TsmxDataSet.SetActive(Value: Boolean);
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Active := Value;
end;

function TsmxDataSet.GetInternalDataSet: TDataSet;
begin
  if TObject(GetInternalRef) is TDataSet then
    Result := TDataSet(GetInternalRef)
  else
    Result := nil;
end;

function TsmxDataSet.GetRecordCount: Integer;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.RecordCount
  else
    Result := 0;
end;

function TsmxDataSet.GetRecordNo: Integer;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.RecNo
  else
    Result := 0;
end;

procedure TsmxDataSet.SetRecordNo(Value: Integer);
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.RecNo := Value;
end;

function TsmxDataSet.IsEmpty: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.IsEmpty
  else
    Result := False;
end;

function TsmxDataSet.Locate(const KeyFields: String; const KeyValues: Variant;
  Options: TsmxLocateOptions = []): Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Locate(KeyFields, KeyValues, TLocateOptions(Options))
  else
    Result := False;
end;

procedure TsmxDataSet.Next;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Next;
end;

procedure TsmxDataSet.Prior;
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Prior;
end;

procedure TsmxDataSet.CreateInternalField(Field: TsmxCustomField);
var
  FieldDef: TFieldDef;
begin
  if Field is TsmxField
    and not Assigned(TsmxField(Field).FField)
    and (Field.DataType <> ftUnknown)
    and Assigned(InternalDataSet) then
  begin
    FieldDef := InternalDataSet.FieldDefs.AddFieldDef;
    FieldDef.DataType := Field.DataType;
    TsmxField(Field).FField := FieldDef.CreateField(InternalDataSet);
  end;
end;

procedure TsmxDataSet.DestroyInternalField(Field: TsmxCustomField);
begin
  if Field is TsmxField then
    if Assigned(TsmxField(Field).FField) then
    begin
      TsmxField(Field).FField.Free;
      TsmxField(Field).FField := nil;
    end;
end;

procedure TsmxDataSet.CreateInternalParam(Param: TsmxCustomParam);
begin
end;

procedure TsmxDataSet.DestroyInternalParam(Param: TsmxCustomParam);
begin
end;

function TsmxDataSet.GetFieldClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxField;
end;

procedure TsmxDataSet.Execute;
begin
end;

procedure TsmxDataSet.Perform;
begin
  case PerformanceMode of
    pmOpen: Open;
    pmExecute: Execute;
  end;
end;

{ TsmxTargetRequest }

destructor TsmxTargetRequest.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  FDatabaseIntf := nil;
  FDataSetIntf := nil;
  inherited Destroy;
end;

procedure TsmxTargetRequest.ClearParams;
begin
  ParamList.Clear;
end;

procedure TsmxTargetRequest.DoExecute(const SQLText: String;
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil);
begin
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    if FDataSetIntf.DataSetType <> RequestType then
      FDataSetIntf := NewRequest('', RequestType);
  end else
    FDataSetIntf := NewRequest('', RequestType);

  if FDataSetIntf.SQLText <> SQLText then
    FDataSetIntf.SQLText := SQLText;
  FDataSetIntf.PerformanceMode := pmExecute;
  PrepRequest(FDataSetIntf, True, DSFrom);
end;

function TsmxTargetRequest.DoRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  ResField: String = ''; const DSFrom: IsmxDataSet = nil): Variant;
begin
  with ForRequest(SQLText, RequestType, True, pmOpen, DSFrom) do
  try
    if ResField = '' then
      ResField := Fields[0].FieldName;
    Result := FieldByName(ResField).Value;
  finally
    Close;
  end;
end;

function TsmxTargetRequest.ForRequest(const SQLText: String; RequestType: TsmxDataSetType = dstQuery;
  Get: Boolean = False; Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    if FDataSetIntf.DataSetType <> RequestType then
      FDataSetIntf := NewRequest('', RequestType);
  end else
    FDataSetIntf := NewRequest('', RequestType);

  if FDataSetIntf.SQLText <> SQLText then
    FDataSetIntf.SQLText := SQLText;
  FDataSetIntf.PerformanceMode := Perform;
  PrepRequest(FDataSetIntf, Get, DSFrom);
  Result := FDataSetIntf;
end;

function TsmxTargetRequest.GetParamList: TsmxParams;
begin
  if not Assigned(FParamList) then
    FParamList := TsmxParams.Create(TsmxParam);
  Result := FParamList;
end;

function TsmxTargetRequest.GetValue(const Key: String): Variant;
var
  Item: TsmxParam;
begin
  Item := ParamList.FindByName(Key);
  if not Assigned(Item) then
  begin
    Item := ParamList.Add;
    Item.ParamName := Key;
  end;
  Result := Item.ParamValue;
end;

procedure TsmxTargetRequest.SetValue(const Key: String; const Value: Variant);
var
  Item: TsmxParam;
begin
  Item := ParamList.FindByName(Key);
  if not Assigned(Item) then
  begin
    Item := ParamList.Add;
    Item.ParamName := Key;
  end;
  Item.ParamValue := Value;
end;

function TsmxTargetRequest.NewRequest(const SQLText: String = '';
  RequestType: TsmxDataSetType = dstQuery; const DSFrom: IsmxDataSet = nil): IsmxDataSet;
begin
  Result := nil;
  if Assigned(Database) then
    Result := Database.NewDataSet(RequestType)
  else
    raise EsmxComponentError.CreateResFmt(@smxConsts.rsActionError, [ClassName, 'NewRequest']);
  Result.Database := Database;
  Result.PerformanceMode := pmOpen;
  if SQLText <> '' then
  begin
    Result.SQLText := SQLText;
    PrepRequest(Result, False, DSFrom);
  end;
end;

function TsmxTargetRequest.PrepRequest(const Request: IsmxDataSet; Get: Boolean = True;
  const DSFrom: IsmxDataSet = nil): Boolean;
var
  i: Integer;
  Field: IsmxField;
  Res: Variant;
begin
  Result := False;
  with Request do
  begin
    Close;
    if not Prepared then
      Prepared := True;
    for i := 0 to ParamCount - 1 do
      if Params[i].ParamType in [ptInput, ptInputOutput] then
      begin
        if Assigned(DSFrom) then
        begin
          Field := DSFrom.FindField(Params[i].ParamName);
          if Assigned(Field) then
            Params[i].AssignParam(Field)
          else
            Params[i].Clear;
        end else
          Params[i].Value := ParamValues[Params[i].ParamName];
      end;
    if Get then
    begin
      Perform;
      Res := 0;
      for i := 0 to ParamCount - 1 do
        if Params[i].ParamType in [ptOutput, ptInputOutput] then
        begin
          ParamValues[Params[i].ParamName] := Params[i].Value;
        end else
        if Params[i].ParamType = ptResult then
        begin
          ParamValues[Params[i].ParamName] := Params[i].Value;
          Res := Params[i].Value;
        end;

      case PerformanceMode of
        pmOpen: Result := RecordCount > 0;
        pmExecute: Result := Res = 0;
      end;
    end;
  end;
end;

procedure TsmxTargetRequest.SetDatabase(const Value: IsmxDatabase);
begin
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    FDataSetIntf := nil;
  end;
  FDatabaseIntf := Value;
end;

end.
