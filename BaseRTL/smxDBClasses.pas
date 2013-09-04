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
  smxTypes;

type
  { TsmxCoDatabase }

  TsmxCoDatabase = class(TComObject, IsmxDatabase)
  private
    FDatabaseIntf: IsmxDatabase;
  protected
    function GetDatabaseIntf: IsmxDatabase; virtual;
  public
    destructor Destroy; override;
    procedure Initialize; override;

    property Database: IsmxDatabase read FDatabaseIntf implements IsmxDatabase;
  end;

  { TsmxSenseItem }

  TsmxSenseList = class;

  TsmxSenseItem = class(TsmxKitItem)
  private
    FFieldName: String;
    FFieldSense: TsmxFieldSense;
    function GetKit: TsmxSenseList;
    procedure SetKit(Value: TsmxSenseList);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property FieldName: String read FFieldName write FFieldName;
    property FieldSense: TsmxFieldSense read FFieldSense write FFieldSense;
    property Kit: TsmxSenseList read GetKit write SetKit;
  end;

  { TsmxSenseList }

  TsmxSenseList = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxSenseItem;
    //function GetSense(const FieldName: String): TsmxFieldSense;
    procedure SetItem(Index: Integer; Value: TsmxSenseItem);
    //procedure SetSense(const FieldName: String; Value: TsmxFieldSense);
  public
    function Add: TsmxSenseItem;
    function FindByName(const FieldName: String): TsmxSenseItem;
    //function IndexOfName(const FieldName: String): Integer;
    //function Insert(const FieldName: String): Integer;
    //function Remove(const FieldName: String): Integer;

    //property Senses[const FieldName: String]: TsmxFieldSense read GetSense write SetSense;
    property Items[Index: Integer]: TsmxSenseItem read GetItem write SetItem; default;
  end;

  { TsmxLocationItem }

  TsmxLocationList = class;

  TsmxLocationItem = class(TsmxKitItem)
  private
    FParamLocation: TsmxParamLocation;
    FParamName: String;
    function GetKit: TsmxLocationList;
    procedure SetKit(Value: TsmxLocationList);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxLocationList read GetKit write SetKit;
    property ParamLocation: TsmxParamLocation read FParamLocation write FParamLocation;
    property ParamName: String read FParamName write FParamName;
  end;

  { TsmxLocationList }

  TsmxLocationList = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxLocationItem;
    //function GetLocation(const ParamName: String): TsmxParamLocation;
    procedure SetItem(Index: Integer; Value: TsmxLocationItem);
    //procedure SetLocation(const ParamName: String; Value: TsmxParamLocation);
  public
    function Add: TsmxLocationItem;
    function FindByName(const ParamName: String): TsmxLocationItem;
    //function IndexOfName(const ParamName: String): Integer;
    //function InsertLocation(const ParamName: String; Location: TsmxParamLocation): Integer;
    //function RemoveLocation(const ParamName: String; Location: TsmxParamLocation): Integer;

    //property Locations[const ParamName: String]: TsmxParamLocation read GetLocation write SetLocation;
    property Items[Index: Integer]: TsmxLocationItem read GetItem write SetItem; default;
  end;

  { TsmxCustomDatabase }

  TsmxCustomDatabase = class(TsmxInterfacedPersistent)
  end;

  { TsmxCustomField }

  TsmxCustomDataSet = class;

  TsmxCustomField = class(TsmxInterfacedPersistent)
  private
    FDataSetIntf: IsmxDataSet;
    FInternalDataSet: TsmxCustomDataSet;
    FInternalRef: Pointer;
  protected
    function GetDataSet: IsmxDataSet;
    function GetFieldName: String; virtual;
    function GetFieldSense: TsmxFieldSense; virtual;
    function GetInternalRef: Pointer; override;
    procedure SetFieldName(const Value: String); virtual;
    procedure SetFieldSense(Value: TsmxFieldSense); virtual;
    procedure SetInternalRef(Value: Pointer); virtual;
    //procedure SetIntfDataSet(Value: TsmxCustomDataSet); virtual;
  public
    constructor Create({AOwner: TComponent;} InternalDataSet: TsmxCustomDataSet); virtual;
    destructor Destroy; override;
    procedure AssignField(const Source: IsmxField); virtual;
    procedure Clear; virtual;

    property DataSet: IsmxDataSet read GetDataSet;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet;
    property InternalRef: Pointer read GetInternalRef write SetInternalRef;
  end;

  { TsmxField }

  TsmxField = class(TsmxCustomField, IsmxField)
  private
    //FField: TField;
    function GetField: TField;
  protected
    //function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetDisplayFormat: String;
    function GetFieldIndex: Integer;
    function GetFieldName: String; override;
    //function GetFieldSense: TsmxFieldSense;
    //function GetInternalRef: Pointer; override;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDisplayFormat(const Value: String);
    //procedure SetField(Value: TField); virtual;
    procedure SetFieldIndex(Value: Integer);
    procedure SetFieldName(const Value: String); override;
    //procedure SetFieldSense(Value: TsmxFieldSense);
    //procedure SetInternalRef(Value: Pointer); override;
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(const Value: Variant);

    property Field: TField read GetField;
  public
    procedure AssignField(const Source: IsmxField); override;
    procedure Clear; override;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    //property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    //property Field: TField read FField write SetField;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;
    //property FieldName: String read GetFieldName write SetFieldName;
    //property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { TsmxCustomParam }

  TsmxCustomParam = class(TsmxInterfacedPersistent)
  private
    FDataSetIntf: IsmxDataSet;
    FInternalDataSet: TsmxCustomDataSet;
    FInternalRef: Pointer;
  protected
    function GetDataSet: IsmxDataSet;
    function GetInternalRef: Pointer; override;
    function GetParamLocation: TsmxParamLocation; virtual;
    function GetParamName: String; virtual;
    procedure SetInternalRef(Value: Pointer); virtual;
    procedure SetParamLocation(Value: TsmxParamLocation); virtual;
    procedure SetParamName(const Value: String); virtual;
    //procedure SetIntfDataSet(Value: TsmxCustomDataSet); virtual;
  public
    constructor Create(InternalDataSet: TsmxCustomDataSet); virtual;
    destructor Destroy; override;
    procedure AssignParam(const Source: IsmxParam); overload; virtual;
    procedure AssignParam(const Source: IsmxField); overload; virtual;
    procedure Clear; virtual;

    property DataSet: IsmxDataSet read GetDataSet;
    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet;
    property ParamName: String read GetParamName write SetParamName;
    property ParamLocation: TsmxParamLocation read GetParamLocation write SetParamLocation;
    property InternalRef: Pointer read GetInternalRef write SetInternalRef;
  end;

  { TsmxNotificationParam }

  {TsmxNotificationParam = class(TsmxCustomParam, IsmxFreeNotification)
  private
    FFreeNotificationIntf: IsmxFreeNotification;
  protected
    //procedure OwnerFreeNotification; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  public
    constructor Create(InternalDataSet: TsmxCustomDataSet); override;
    destructor Destroy; override;

    property FreeNotification: IsmxFreeNotification read FFreeNotificationIntf implements IsmxFreeNotification;
  end;}

  { TsmxReferenceItem }

  TsmxReferenceList = class;

  TsmxReferenceItem = class(TsmxKitItem)
  private
    FOwner: TsmxInterfacedPersistent;
    FReference: Pointer;
    function GetKit: TsmxReferenceList;
    procedure SetKit(Value: TsmxReferenceList);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property Owner: TsmxInterfacedPersistent read FOwner write FOwner;
    property Reference: Pointer read FReference write FReference;
    property Kit: TsmxReferenceList read GetKit write SetKit;
  end;

  { TsmxReferenceList }

  TsmxReferenceList = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxReferenceItem;
    procedure SetItem(Index: Integer; Value: TsmxReferenceItem);
  public
    function Add: TsmxReferenceItem;
    function FindByCombo(Owner: TsmxInterfacedPersistent; Reference: Pointer): TsmxReferenceItem;
    procedure InsertReference(Owner: TsmxInterfacedPersistent; Reference: Pointer);
    procedure RemoveReference(Owner: TsmxInterfacedPersistent; Reference: Pointer);

    property Items[Index: Integer]: TsmxReferenceItem read GetItem write SetItem; default;
  end;

  { TsmxCustomDataSet }

  TsmxCustomDataSet = class(TsmxInterfacedPersistent)
  private
    FFieldReferenceList: TsmxReferenceList;
    FLocationList: TsmxLocationList;
    FParamReferenceList: TsmxReferenceList;
    FSenseList: TsmxSenseList;
    function GetFieldReferenceList: TsmxReferenceList;
    function GetLocationList: TsmxLocationList;
    function GetParamReferenceList: TsmxReferenceList;
    function GetSenseList: TsmxSenseList;
    procedure ClearFieldReferences;
    procedure ClearParamReferences;
    procedure ResetFieldReference(Reference: Pointer);
    procedure ResetParamReference(Reference: Pointer);
  protected
    procedure SetFieldReferenceList(Value: TsmxReferenceList); virtual;
    procedure SetLocationList(Value: TsmxLocationList); virtual;
    procedure SetParamReferenceList(Value: TsmxReferenceList); virtual;
    procedure SetSenseList(Value: TsmxSenseList); virtual;
  public
    destructor Destroy; override;
    //function AddField(const FieldName: String): IsmxField; virtual;
    //function AddParam(const ParamName: String): IsmxParam; virtual;
    procedure ClearFields; virtual;
    procedure ClearParams; virtual;
    procedure DeleteField(const Field: IsmxField); virtual;
    procedure DeleteParam(const Param: IsmxParam); virtual;

    property FieldReferenceList: TsmxReferenceList read GetFieldReferenceList write SetFieldReferenceList;
    property LocationList: TsmxLocationList read GetLocationList write SetLocationList;
    property ParamReferenceList: TsmxReferenceList read GetParamReferenceList write SetParamReferenceList;
    property SenseList: TsmxSenseList read GetSenseList write SetSenseList;
  end;

  { TsmxDataSet }

  TsmxDataSet = class(TsmxCustomDataSet)
  private
    FDatabaseIntf: IsmxDatabase;
    //FInternalDataSet: TDataSet;
  protected
    function GetActive: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetFieldIntf(Field: TField): IsmxField; virtual;
    //function GetInternalDataSet: TDataSet; virtual;
    //function GetInternalDataSetClass: TComponentClass; virtual;
    //function GetInternalRef: Pointer; virtual;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetRecordNo(Value: Integer);

    //property InternalDataSet: TDataSet read GetInternalDataSet;
  public
    destructor Destroy; override;
    procedure Add;
    function AddField(DataType: TsmxDataType): IsmxField; virtual;
    function Bof: Boolean;
    procedure Cancel;
    procedure ClearFields; override;
    procedure Close;
    procedure Delete;
    procedure DeleteField(const Field: IsmxField); override;
    //procedure DisableControls;
    procedure Edit;
    //procedure EnalbleControls;
    function Eof: Boolean;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    procedure First;
    //procedure FreeBookmark(Bookmark: Pointer);
    //function GetBookmark: Pointer;
    //procedure GotoBookmark(Bookmark: Pointer);
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TsmxLocateOptions = []): Boolean;
    procedure Next;
    procedure Open;
    procedure Post;
    procedure Prior;

    property Active: Boolean read GetActive write SetActive;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: IsmxField read GetField write SetField;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
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
      Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): Boolean;

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[const Key: String]: Variant read GetValue write SetValue; default;
  end;

  { TsmxConnection }

  TsmxConnection = class(TsmxComponent, IsmxConnection)
  private
    FDatabaseIntf: IsmxDatabase;
    FDatabaseManagerIntf: IsmxDatabaseManager;
  protected
    procedure FreeConnection;
    function GetConnected: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDatabaseManager: IsmxDatabaseManager;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager);
  public
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    property Connected: Boolean read GetConnected write SetConnected;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;
  end;

implementation

uses
  Variants, SysUtils, smxProcs, smxFuncs, smxConsts;

{ TsmxCoDatabase }

destructor TsmxCoDatabase.Destroy;
begin
  FDatabaseIntf := nil;
  inherited Destroy;
end;

function TsmxCoDatabase.GetDatabaseIntf: IsmxDatabase;
begin
  Result := nil;
end;

procedure TsmxCoDatabase.Initialize;
begin
  inherited Initialize;
  FDatabaseIntf := GetDatabaseIntf;
end;

{ TsmxSenseItem }

procedure TsmxSenseItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxSenseItem then
  begin
    FieldName := TsmxSenseItem(Source).FieldName;
    FieldSense := TsmxSenseItem(Source).FieldSense;
  end else
    inherited Assign(Source);
end;

function TsmxSenseItem.GetKit: TsmxSenseList;
begin
  Result := TsmxSenseList(inherited Kit);
end;

procedure TsmxSenseItem.SetKit(Value: TsmxSenseList);
begin
  inherited Kit := Value;
end;

{ TsmxSenseList }

function TsmxSenseList.Add: TsmxSenseItem;
begin
  Result := TsmxSenseItem(inherited Add);
end;

function TsmxSenseList.FindByName(const FieldName: String): TsmxSenseItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].FieldName, FieldName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxSenseList.GetItem(Index: Integer): TsmxSenseItem;
begin
  Result := TsmxSenseItem(inherited Items[Index]);
end;

procedure TsmxSenseList.SetItem(Index: Integer; Value: TsmxSenseItem);
begin
  inherited Items[Index] := Value;
end;

{function TsmxSenseList.GetSense(const FieldName: String): TsmxFieldSense;
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

procedure TsmxSenseList.SetSense(const FieldName: String; Value: TsmxFieldSense);
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
end;}

{function TsmxSenseList.IndexOfName(const FieldName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].FieldName, FieldName) = 0 then
    begin
      Result := i;
      Break;
    end;
end;}

{function TsmxSenseList.Insert(const FieldName: String): Integer;
var
  Item: TsmxSenseItem;
begin
  Result := IndexOfName(FieldName);
  if Result = -1 then
  begin
    Item := Add;
    Item.FieldName := FieldName;
    Result := Item.ItemIndex;
  end;
end;

function TsmxSenseList.Remove(const FieldName: String): Integer;
begin
  Result := IndexOfName(FieldName);
  if Result <> -1 then
    Delete(Result);
end;}

{ TsmxLocationItem }

procedure TsmxLocationItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxLocationItem then
  begin
    ParamLocation := TsmxLocationItem(Source).ParamLocation;
    ParamName := TsmxLocationItem(Source).ParamName;
  end else
    inherited Assign(Source);
end;

function TsmxLocationItem.GetKit: TsmxLocationList;
begin
  Result := TsmxLocationList(inherited Kit);
end;

procedure TsmxLocationItem.SetKit(Value: TsmxLocationList);
begin
  inherited Kit := Value;
end;

{ TsmxLocationList }

function TsmxLocationList.Add: TsmxLocationItem;
begin
  Result := TsmxLocationItem(inherited Add);
end;

function TsmxLocationList.FindByName(const ParamName: String): TsmxLocationItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, ParamName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxLocationList.GetItem(Index: Integer): TsmxLocationItem;
begin
  Result := TsmxLocationItem(inherited Items[Index]);
end;

procedure TsmxLocationList.SetItem(Index: Integer; Value: TsmxLocationItem);
begin
  inherited Items[Index] := Value;
end;

{function TsmxLocationList.GetLocation(const ParamName: String): TsmxParamLocation;
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

procedure TsmxLocationList.SetLocation(const ParamName: String; Value: TsmxParamLocation);
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
end;}

{function TsmxLocationList.IndexOfName(const ParamName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, ParamName) = 0 then
    begin
      Result := i;
      Break;
    end;
end;}

{procedure TsmxLocationList.Insert(const ParamName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(ParamName);
  if CurIndex = -1 then
    Add.ParamName := ParamName;
end;

procedure TsmxLocationList.Remove(const ParamName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(ParamName);
  if CurIndex <> -1 then
    Delete(CurIndex);
end;}

{ TsmxCustomField }

constructor TsmxCustomField.Create({AOwner: TComponent;} InternalDataSet: TsmxCustomDataSet);
begin
  //inherited Create(AOwner);
  FInternalDataSet := InternalDataSet;
  FDataSetIntf := InternalDataSet as IsmxDataSet;
end;

destructor TsmxCustomField.Destroy;
begin
  FDataSetIntf := nil;
  SetInternalRef(nil);
  inherited Destroy;
end;

procedure TsmxCustomField.AssignField(const Source: IsmxField);
begin
  if Assigned(Source) then
    FieldSense := Source.FieldSense
  else
    Clear;
end;

procedure TsmxCustomField.Clear;
begin
  FieldSense := fsGeneral;
end;

function TsmxCustomField.GetDataSet: IsmxDataSet;
begin
  Result := FDataSetIntf;
end;

function TsmxCustomField.GetFieldName: String;
begin
  Result := '';
end;

procedure TsmxCustomField.SetFieldName(const Value: String);
var
  Item: TsmxSenseItem;
begin
  Item := FInternalDataSet.SenseList.FindByName(FieldName);
  if not Assigned(Item) then
    Item := FInternalDataSet.SenseList.Add;
  Item.FieldName := Value;
end;

function TsmxCustomField.GetFieldSense: TsmxFieldSense;
var
  Item: TsmxSenseItem;
begin
  Item := FInternalDataSet.SenseList.FindByName(FieldName);
  if not Assigned(Item) then
  begin
    Item := FInternalDataSet.SenseList.Add;
    Item.FieldName := FieldName;
  end;
  Result := Item.FieldSense;
end;

procedure TsmxCustomField.SetFieldSense(Value: TsmxFieldSense);
var
  Item: TsmxSenseItem;
begin
  Item := FInternalDataSet.SenseList.FindByName(FieldName);
  if not Assigned(Item) then
  begin
    Item := FInternalDataSet.SenseList.Add;
    Item.FieldName := FieldName;
  end;
  Item.FieldSense := Value;
end;

{procedure TsmxCustomField.SetIntfDataSet(Value: TsmxCustomDataSet);
begin
  FIntfDataSet := Value;
end;}

function TsmxCustomField.GetInternalRef: Pointer;
begin
  Result := FInternalRef;
end;

procedure TsmxCustomField.SetInternalRef(Value: Pointer);
begin
  if Assigned(FInternalRef) then
    FInternalDataSet.FieldReferenceList.RemoveReference(Self, FInternalRef);
  FInternalRef := Value;
  if Assigned(FInternalRef) then
    FInternalDataSet.FieldReferenceList.InsertReference(Self, FInternalRef);
end;

{ TsmxField }

procedure TsmxField.AssignField(const Source: IsmxField);
begin
  inherited AssignField(Source);
  if Assigned(Source) then
    Value := Source.Value;
end;

procedure TsmxField.Clear;
begin
  inherited Clear;
  if Assigned(Field) then
    Field.Clear;
end;

{function TsmxField.GetDataSet: IsmxDataSet;
begin
  if Assigned(IntfDataSet) then
    Result := IntfDataSet as IsmxDataSet else
    Result := nil;
end;}

function TsmxField.GetDataType: TsmxDataType;
begin
  if Assigned(Field) then
    Result := Field.DataType else
    Result := ftUnknown;
end;

function TsmxField.GetDisplayFormat: String;
begin
  if Field is TAggregateField then
    Result := TAggregateField(Field).DisplayFormat else
  if Field is TDateTimeField then
    Result := TDateTimeField(Field).DisplayFormat else
  if Field is TNumericField then
    Result := TNumericField(Field).DisplayFormat else
  if Field is TSQLTimeStampField then
    Result := TSQLTimeStampField(Field).DisplayFormat else
    Result := '';
end;

procedure TsmxField.SetDisplayFormat(const Value: String);
begin
  if Field is TAggregateField then
    TAggregateField(Field).DisplayFormat := Value else
  if Field is TDateTimeField then
    TDateTimeField(Field).DisplayFormat := Value else
  if Field is TNumericField then
    TNumericField(Field).DisplayFormat := Value else
  if Field is TSQLTimeStampField then
    TSQLTimeStampField(Field).DisplayFormat := Value;
end;

function TsmxField.GetField: TField;
begin
  if TObject(InternalRef) is TField then
    Result := TField(InternalRef) else
    Result := nil;
end;

function TsmxField.GetFieldIndex: Integer;
begin
  if Assigned(Field) then
    Result := Field.Index else
    Result := -1;
end;

procedure TsmxField.SetFieldIndex(Value: Integer);
begin
  if Assigned(Field) then
    Field.Index := Value;
end;

function TsmxField.GetFieldName: String;
begin
  Result := inherited GetFieldName;
  if Assigned(Field) then
    Result := Field.FieldName;
end;

procedure TsmxField.SetFieldName(const Value: String);
begin
  inherited SetFieldName(Value);
  if Assigned(Field) then
    Field.FieldName := Value;
end;

function TsmxField.GetPrecision: Integer;
begin
  if Field is TBCDField then
    Result := TBCDField(Field).Precision else
  if Field is TFMTBCDField then
    Result := TFMTBCDField(Field).Precision else
    Result := 0;
end;

procedure TsmxField.SetPrecision(Value: Integer);
begin
  if Field is TBCDField then
    TBCDField(Field).Precision := Value else
  if Field is TFMTBCDField then
    TFMTBCDField(Field).Precision := Value;
end;

function TsmxField.GetSize: Integer;
begin
  if Assigned(Field) then
    Result := Field.Size else
    Result := 0;
end;

procedure TsmxField.SetSize(Value: Integer);
begin
  if Assigned(Field) then
    Field.Size := Value;
end;

function TsmxField.GetValue: Variant;
begin
  if Assigned(Field) then
    Result := Field.Value else
    Result := Variants.Null;
end;

procedure TsmxField.SetValue(const Value: Variant);
begin
  if Assigned(Field) then
    Field.Value := Value;
end;

{function TsmxField.GetFieldSense: TsmxFieldSense;
begin
  if Assigned(FField) and Assigned(IntfDataSet) then
    Result := IntfDataSet.SenseList.Senses[FField.FieldName] else
    Result := fsGeneral;
end;

procedure TsmxField.SetFieldSense(Value: TsmxFieldSense);
begin
  if Assigned(FField) and Assigned(IntfDataSet) then
    IntfDataSet.SenseList.Senses[FField.FieldName] := Value;
end;}

{function TsmxField.GetInternalRef: Pointer;
begin
  Result := Pointer(FField);
end;}

{procedure TsmxField.SetInternalRef(Value: Pointer);
begin
  inherited SetInternalRef(Value);
  if TObject(Value) is TField then
    FField := TField(Value) else
    FField := nil;
end;}

function TsmxField.IsBlob: Boolean;
begin
  if Assigned(Field) then
    Result := Field.IsBlob else
    Result := False;
end;

function TsmxField.IsNull: Boolean;
begin
  if Assigned(Field) then
    Result := Field.IsNull else
    Result := False;
end;

procedure TsmxField.LoadFromStream(Stream: TStream);
var
  Str: String;
begin
  if Assigned(Field) then
  begin
    smxProcs.StreamToStr(Stream, Str);
    Field.Value := smxFuncs.StrToVar(Str);
  end;
end;

procedure TsmxField.SaveToStream(Stream: TStream);
begin
  if Assigned(Field) then
    smxProcs.StrToStream(Variants.VarToStr(Field.Value), Stream);
end;

{procedure TsmxField.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Field) and (Operation = opRemove) then
    Field := nil;
end;}

{procedure TsmxField.SetField(Value: TField);
begin
  FField := Value;
  if Assigned(FField) then
    FField.FreeNotification(Self);
end;}

{ TsmxCustomParam }

constructor TsmxCustomParam.Create(InternalDataSet: TsmxCustomDataSet);
begin
  FInternalDataSet := InternalDataSet;
  FDataSetIntf := InternalDataSet as IsmxDataSet;
end;

destructor TsmxCustomParam.Destroy;
begin
  FDataSetIntf := nil;
  SetInternalRef(nil);
  inherited Destroy;
end;

procedure TsmxCustomParam.AssignParam(const Source: IsmxParam);
begin
  if Assigned(Source) then
    ParamLocation := Source.ParamLocation
  else
    Clear;
end;

procedure TsmxCustomParam.AssignParam(const Source: IsmxField);
begin
  if not Assigned(Source) then
    Clear;
end;

procedure TsmxCustomParam.Clear;
begin
  ParamLocation := plConst;
end;

function TsmxCustomParam.GetDataSet: IsmxDataSet;
begin
  Result := FDataSetIntf;
end;

function TsmxCustomParam.GetParamLocation: TsmxParamLocation;
var
  Item: TsmxLocationItem;
begin
  Item := FInternalDataSet.LocationList.FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := FInternalDataSet.LocationList.Add;
    Item.ParamName := ParamName;
  end;
  Result := Item.ParamLocation;
end;

procedure TsmxCustomParam.SetParamLocation(Value: TsmxParamLocation);
var
  Item: TsmxLocationItem;
begin
  Item := FInternalDataSet.LocationList.FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := FInternalDataSet.LocationList.Add;
    Item.ParamName := ParamName;
  end;
  Item.ParamLocation := Value;
end;

function TsmxCustomParam.GetParamName: String;
begin
  Result := '';
end;

procedure TsmxCustomParam.SetParamName(const Value: String);
var
  Item: TsmxLocationItem;
begin
  Item := FInternalDataSet.LocationList.FindByName(ParamName);
  if not Assigned(Item) then
    Item := FInternalDataSet.LocationList.Add;
  Item.ParamName := Value;
end;

{procedure TsmxCustomParam.SetIntfDataSet(Value: TsmxCustomDataSet);
begin
  FIntfDataSet := Value;
end;}

{ TsmxNotificationParam }

{constructor TsmxNotificationParam.Create(InternalDataSet: TsmxCustomDataSet);
begin
  inherited Create(InternalDataSet);
  FFreeNotificationIntf := TsmxFreeNotificationObject.Create(
    Self as IsmxOwnerNotification) as IsmxFreeNotification;
end;

destructor TsmxNotificationParam.Destroy;
begin
  FFreeNotificationIntf := nil;
  inherited Destroy;
end;}

{procedure TsmxNotificationParam.OwnerFreeNotification;
begin
end;}

{function TsmxNotificationParam.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result = E_NOINTERFACE then
    Result := FFreeNotificationIntf.QueryInterface(IID, Obj);
end;}

function TsmxCustomParam.GetInternalRef: Pointer;
begin
  Result := FInternalRef;
end;

procedure TsmxCustomParam.SetInternalRef(Value: Pointer);
begin
  if Assigned(FInternalRef) then
    FInternalDataSet.ParamReferenceList.RemoveReference(Self, FInternalRef);
  FInternalRef := Value;
  if Assigned(FInternalRef) then
    FInternalDataSet.ParamReferenceList.InsertReference(Self, FInternalRef);
end;

{ TsmxCustomDataSet }

destructor TsmxCustomDataSet.Destroy;
begin
  if Assigned(FLocationList) then
    FLocationList.Free;
  if Assigned(FSenseList) then
    FSenseList.Free;
  if Assigned(FFieldReferenceList) then
  begin
    ClearFieldReferences;
    FFieldReferenceList.Free;
  end;
  if Assigned(FParamReferenceList) then
  begin
    ClearParamReferences;
    FParamReferenceList.Free;
  end;
  inherited Destroy;
end;

{function TsmxCustomDataSet.AddField(const FieldName: String): IsmxField;
var
  Item: TsmxSenseItem;
begin
  Result := nil;
  Item := SenseList.FindByName(FieldName);
  if not Assigned(Item) then
    SenseList.Add.FieldName := FieldName;
end;}

procedure TsmxCustomDataSet.DeleteField(const Field: IsmxField);
var
  Item: TsmxSenseItem;
begin
  if Assigned(Field) then
  begin
    Item := SenseList.FindByName(Field.FieldName);
    if Assigned(Item) then
      SenseList.Delete(Item.ItemIndex);
    ResetFieldReference(Field.InternalRef);
  end;
end;

{function TsmxCustomDataSet.AddParam(const ParamName: String): IsmxParam;
var
  Item: TsmxLocationItem;
begin
  Result := nil;
  Item := LocationList.FindByName(ParamName);
  if not Assigned(Item) then
    LocationList.Add.ParamName := ParamName;
end;}

procedure TsmxCustomDataSet.DeleteParam(const Param: IsmxParam);
var
  Item: TsmxLocationItem;
begin
  if Assigned(Param) then
  begin
    Item := LocationList.FindByName(Param.ParamName);
    if Assigned(Item) then
      LocationList.Delete(Item.ItemIndex);
    ResetParamReference(Param.InternalRef);
  end;
end;

procedure TsmxCustomDataSet.ClearFieldReferences;
begin
  while FieldReferenceList.Count > 0 do
    TsmxCustomField(FieldReferenceList[FieldReferenceList.Count - 1].Owner).InternalRef := nil;
end;

procedure TsmxCustomDataSet.ClearFields;
begin
  SenseList.Clear;
  ClearFieldReferences;
end;

procedure TsmxCustomDataSet.ClearParamReferences;
begin
  while ParamReferenceList.Count > 0 do
    TsmxCustomParam(ParamReferenceList[ParamReferenceList.Count - 1].Owner).InternalRef := nil;
end;

procedure TsmxCustomDataSet.ClearParams;
begin
  LocationList.Clear;
  ClearParamReferences;
end;

function TsmxCustomDataSet.GetFieldReferenceList: TsmxReferenceList;
begin
  if not Assigned(FFieldReferenceList) then
    FFieldReferenceList := TsmxReferenceList.Create(TsmxReferenceItem);
  Result := FFieldReferenceList;
end;

procedure TsmxCustomDataSet.SetFieldReferenceList(Value: TsmxReferenceList);
begin
  FieldReferenceList.Assign(Value);
end;

function TsmxCustomDataSet.GetLocationList: TsmxLocationList;
begin
  if not Assigned(FLocationList) then
    FLocationList := TsmxLocationList.Create(TsmxLocationItem);
  Result := FLocationList;
end;

procedure TsmxCustomDataSet.SetLocationList(Value: TsmxLocationList);
begin
  LocationList.Assign(Value);
end;

function TsmxCustomDataSet.GetParamReferenceList: TsmxReferenceList;
begin
  if not Assigned(FParamReferenceList) then
    FParamReferenceList := TsmxReferenceList.Create(TsmxReferenceItem);
  Result := FParamReferenceList;
end;

procedure TsmxCustomDataSet.SetParamReferenceList(Value: TsmxReferenceList);
begin
  ParamReferenceList.Assign(Value);
end;

function TsmxCustomDataSet.GetSenseList: TsmxSenseList;
begin
  if not Assigned(FSenseList) then
    FSenseList := TsmxSenseList.Create(TsmxSenseItem);
  Result := FSenseList;
end;

procedure TsmxCustomDataSet.SetSenseList(Value: TsmxSenseList);
begin
  SenseList.Assign(Value);
end;

procedure TsmxCustomDataSet.ResetFieldReference(Reference: Pointer);
var
  i: Integer;
begin
  for i := FieldReferenceList.Count - 1 downto 0 do
    if FieldReferenceList[i].Reference = Reference then
      TsmxCustomField(FieldReferenceList[i].Owner).InternalRef := nil;
end;

procedure TsmxCustomDataSet.ResetParamReference(Reference: Pointer);
var
  i: Integer;
begin
  for i := ParamReferenceList.Count - 1 downto 0 do
    if ParamReferenceList[i].Reference = Reference then
      TsmxCustomParam(ParamReferenceList[i].Owner).InternalRef := nil;
end;

{ TsmxDataSet }

destructor TsmxDataSet.Destroy;
begin
  FDatabaseIntf := nil;
  {if Assigned(FInternalDataSet) then
    FInternalDataSet.Free;}
  inherited Destroy;
end;

procedure TsmxDataSet.Add;
begin
  TDataSet(GetInternalRef).Append;
end;

procedure TsmxDataSet.Delete;
begin
  TDataSet(GetInternalRef).Delete;
end;

function TsmxDataSet.AddField(DataType: TsmxDataType): IsmxField;
var
  AField: TField;
  AFieldDef: TFieldDef;
begin
  //inherited AddField(FieldName);
  {if not TDataSet(GetInternalRef).FieldDefs.Updated then
    TDataSet(GetInternalRef).FieldDefs.Update;
  AField := TDataSet(GetInternalRef).FieldDefs.Find(FieldName).CreateField(TDataSet(GetInternalRef));}
  {if not TDataSet(GetInternalRef).FieldDefs.Updated then
    TDataSet(GetInternalRef).FieldDefs.Update;
  AField := TDataSet(GetInternalRef).FieldDefList.FieldByName(FieldName).CreateField(TDataSet(GetInternalRef));}
  AFieldDef := TDataSet(GetInternalRef).FieldDefs.AddFieldDef;
  AFieldDef.DataType := DataType;
  AField := AFieldDef.CreateField(TDataSet(GetInternalRef));
  Result := GetFieldIntf(AField);
end;

procedure TsmxDataSet.DeleteField(const Field: IsmxField);
var
  AField: TField;
  //Item: TsmxSenseItem;
begin
  inherited DeleteField(Field);
  if Assigned(Field) then
  begin
    AField := TDataSet(GetInternalRef).Fields.FindField(Field.FieldName);
    if Assigned(AField) then
    //begin
      TDataSet(GetInternalRef).Fields.Remove(AField);
      //Item := SenseList.FindByName(Field.FieldName);
      //if Assigned(Item) then
        //SenseList.Delete(Item.ItemIndex);
    //end;
  end;
end;

function TsmxDataSet.Bof: Boolean;
begin
  Result := TDataSet(GetInternalRef).Bof;
end;

function TsmxDataSet.Eof: Boolean;
begin
  Result := TDataSet(GetInternalRef).Eof;
end;

procedure TsmxDataSet.Cancel;
begin
  TDataSet(GetInternalRef).Cancel;
end;

procedure TsmxDataSet.Post;
begin
  TDataSet(GetInternalRef).Post;
end;

procedure TsmxDataSet.ClearFields;
begin
  inherited ClearFields;
  if TDataSet(GetInternalRef).Active then
    TDataSet(GetInternalRef).ClearFields;
  //SenseList.Clear;
end;

procedure TsmxDataSet.Close;
begin
  TDataSet(GetInternalRef).Close;
end;

procedure TsmxDataSet.Open;
begin
  TDataSet(GetInternalRef).Open;
end;

{procedure TsmxDataSet.DisableControls;
begin
  TDataSet(GetInternalRef).DisableControls;
end;

procedure TsmxDataSet.EnalbleControls;
begin
  TDataSet(GetInternalRef).EnableControls;
end;}

procedure TsmxDataSet.Edit;
begin
  TDataSet(GetInternalRef).Edit;
end;

function TsmxDataSet.FieldByName(const FieldName: String): IsmxField;
var
  Field: TField;
begin
  Field := TDataSet(GetInternalRef).Fields.FieldByName(FieldName);
  Result := GetFieldIntf(Field);
end;

function TsmxDataSet.FindField(const FieldName: String): IsmxField;
var
  Field: TField;
begin
  Field := TDataSet(GetInternalRef).Fields.FindField(FieldName);
  if Assigned(Field) then
    Result := GetFieldIntf(Field) else
    Result := nil;
end;

procedure TsmxDataSet.First;
begin
  TDataSet(GetInternalRef).First;
end;

procedure TsmxDataSet.Last;
begin
  TDataSet(GetInternalRef).Last;
end;

{procedure TsmxDataSet.FreeBookmark(Bookmark: Pointer);
begin
  TDataSet(GetInternalRef).FreeBookmark(Bookmark);
end;

function TsmxDataSet.GetBookmark: Pointer;
begin
  Result := TDataSet(GetInternalRef).GetBookmark;
end;

procedure TsmxDataSet.GotoBookmark(Bookmark: Pointer);
begin
  TDataSet(GetInternalRef).GotoBookmark(Bookmark);
end;}


function TsmxDataSet.GetActive: Boolean;
begin
  Result := TDataSet(GetInternalRef).Active;
end;

procedure TsmxDataSet.SetActive(Value: Boolean);
begin
  TDataSet(GetInternalRef).Active := Value;
end;

function TsmxDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxDataSet.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxDataSet.GetField(Index: Integer): IsmxField;
var
  Field: TField;
begin
  Field := TDataSet(GetInternalRef).Fields[Index];
  Result := GetFieldIntf(Field);
end;

procedure TsmxDataSet.SetField(Index: Integer; const Value: IsmxField);
var
  Field: IsmxField;
begin
  Field := GetFieldIntf(TDataSet(GetInternalRef).Fields[Index]);
  Field.AssignField(Value);
end;

function TsmxDataSet.GetFieldCount: Integer;
begin
  Result := TDataSet(GetInternalRef).FieldCount;
end;

function TsmxDataSet.GetFieldIntf(Field: TField): IsmxField;
begin
  Result := TsmxField.Create(Self) as IsmxField;
  Result.InternalRef := Pointer(Field);
end;

{function TsmxDataSet.GetInternalDataSet: TDataSet;
begin
  if not Assigned(FInternalDataSet) then
    FInternalDataSet := TDataSet.Create(nil); //(GetInternalDataSetClass.Create(nil));
  Result := FInternalDataSet;
end;}

{function TsmxDataSet.GetInternalDataSetClass: TComponentClass;
begin
  Result := TDataSet;
end;}

{function TsmxDataSet.GetInternalRef: Pointer;
begin
  Result := nil; //Pointer(InternalDataSet);
end;}

function TsmxDataSet.GetRecordCount: Integer;
begin
  Result := TDataSet(GetInternalRef).RecordCount;
end;

function TsmxDataSet.GetRecordNo: Integer;
begin
  Result := TDataSet(GetInternalRef).RecNo;
end;

procedure TsmxDataSet.SetRecordNo(Value: Integer);
begin
  TDataSet(GetInternalRef).RecNo := Value;
end;

function TsmxDataSet.IsEmpty: Boolean;
begin
  Result := TDataSet(GetInternalRef).IsEmpty;
end;

function TsmxDataSet.Locate(const KeyFields: String; const KeyValues: Variant;
  Options: TsmxLocateOptions = []): Boolean;
begin
  Result := TDataSet(GetInternalRef).Locate(KeyFields, KeyValues, TLocateOptions(Options));
end;

procedure TsmxDataSet.Next;
begin
  TDataSet(GetInternalRef).Next;
end;

procedure TsmxDataSet.Prior;
begin
  TDataSet(GetInternalRef).Prior;
end;

{ TsmxReferenceItem }

procedure TsmxReferenceItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxReferenceItem then
  begin
    Owner := TsmxReferenceItem(Source).Owner;
    Reference := TsmxReferenceItem(Source).Reference;
  end else
    inherited Assign(Source);
end;

function TsmxReferenceItem.GetKit: TsmxReferenceList;
begin
  Result := TsmxReferenceList(inherited Kit);
end;

procedure TsmxReferenceItem.SetKit(Value: TsmxReferenceList);
begin
  inherited Kit := Value;
end;

{ TsmxReferenceList }

function TsmxReferenceList.Add: TsmxReferenceItem;
begin
  Result := TsmxReferenceItem(inherited Add);
end;

function TsmxReferenceList.FindByCombo(Owner: TsmxInterfacedPersistent;
  Reference: Pointer): TsmxReferenceItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i].Owner = Owner) and (Items[i].Reference = Reference) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxReferenceList.GetItem(Index: Integer): TsmxReferenceItem;
begin
  Result := TsmxReferenceItem(inherited Items[Index]);
end;

procedure TsmxReferenceList.InsertReference(Owner: TsmxInterfacedPersistent; Reference: Pointer);
var
  Item: TsmxReferenceItem;
begin
  Item := FindByCombo(Owner, Reference);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.Owner := Owner;
    Item.Reference := Reference;
  end;
end;

procedure TsmxReferenceList.RemoveReference(Owner: TsmxInterfacedPersistent; Reference: Pointer);
var
  Item: TsmxReferenceItem;
begin
  Item := FindByCombo(Owner, Reference);
  if Assigned(Item) then
    Delete(Item.ItemIndex);
end;

procedure TsmxReferenceList.SetItem(Index: Integer; Value: TsmxReferenceItem);
begin
  inherited Items[Index] := Value;
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

  if FDataSetIntf.SQL.Text <> SQLText then
    FDataSetIntf.SQL.Text := SQLText;
  PrepRequest(FDataSetIntf, True, pmExecute, DSFrom);
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

  if FDataSetIntf.SQL.Text <> SQLText then
    FDataSetIntf.SQL.Text := SQLText;
  PrepRequest(FDataSetIntf, Get, Perform, DSFrom);
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
  if SQLText <> '' then
  begin
    Result.SQL.Text := SQLText;
    PrepRequest(Result, False, pmOpen, DSFrom);
  end;
end;

function TsmxTargetRequest.PrepRequest(const Request: IsmxDataSet; Get: Boolean = True;
  Perform: TsmxPerformanceMode = pmOpen; const DSFrom: IsmxDataSet = nil): Boolean;
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
      case Perform of
        pmOpen: Open;
        pmExecute: Execute;
      end;
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

      case Perform of
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

{ TsmxConnection }

destructor TsmxConnection.Destroy;
begin
  FDatabaseIntf := nil;
  SetDatabaseManager(nil);
  inherited Destroy;
end;

procedure TsmxConnection.Connect;
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.Connected := True;
end;

procedure TsmxConnection.Disconnect;
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.Connected := False;
end;

procedure TsmxConnection.FreeConnection;
begin
  Free;
end;

function TsmxConnection.GetConnected: Boolean;
begin
  if Assigned(FDatabaseIntf) then
    Result := FDatabaseIntf.Connected else
    Result := False;
end;

procedure TsmxConnection.SetConnected(Value: Boolean);
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.Connected := Value;
end;

function TsmxConnection.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxConnection.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxConnection.GetDatabaseManager: IsmxDatabaseManager;
begin
  Result := FDatabaseManagerIntf;
end;

procedure TsmxConnection.SetDatabaseManager(const Value: IsmxDatabaseManager);
begin
  if Assigned(FDatabaseManagerIntf) then
    FDatabaseManagerIntf.RemoveConnection(Self as IsmxConnection);
  FDatabaseManagerIntf := Value;
  if Assigned(FDatabaseManagerIntf) then
    FDatabaseManagerIntf.InsertConnection(Self as IsmxConnection);
end;

initialization
  Classes.RegisterClasses([TsmxField, TsmxConnection]);

end.
