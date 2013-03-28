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
  Classes, DB, ComObj, smxBaseClasses, smxBaseIntf, smxDBIntf;

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
  protected
    function GetDataSet: IsmxDataSet;
    function GetFieldName: String; virtual;
    function GetFieldSense: TsmxFieldSense; virtual;
    function GetInternalRef: Pointer; virtual;
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
    FField: TField;
  protected
    //function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetDisplayFormat: String;
    function GetFieldIndex: Integer;
    function GetFieldName: String; override;
    //function GetFieldSense: TsmxFieldSense;
    function GetInternalRef: Pointer; override;
    function GetSize: Integer;
    function GetValue: Variant;
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDisplayFormat(const Value: String);
    //procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: String); override;
    //procedure SetFieldSense(Value: TsmxFieldSense);
    procedure SetInternalRef(Value: Pointer); override;
    procedure SetValue(const Value: Variant);
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
    property FieldIndex: Integer read GetFieldIndex;
    //property FieldName: String read GetFieldName write SetFieldName;
    //property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property Size: Integer read GetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { TsmxCustomParam }

  TsmxCustomParam = class(TsmxInterfacedPersistent)
  private
    FDataSetIntf: IsmxDataSet;
    FInternalDataSet: TsmxCustomDataSet;
  protected
    function GetDataSet: IsmxDataSet;
    function GetInternalRef: Pointer;
    function GetParamLocation: TsmxParamLocation; virtual;
    function GetParamName: String; virtual;
    procedure SetInternalRef(Value: Pointer);
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
    function AddField(const FieldName: String): IsmxField; virtual;
    function AddParam(const ParamName: String): IsmxParam; virtual;
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
    function GetInternalRef: Pointer; virtual;
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
    function AddField(const FieldName: String): IsmxField; override;
    function Bof: Boolean;
    procedure Cancel;
    procedure ClearFields; override;
    procedure Close;
    procedure Delete;
    procedure DeleteField(const Field: IsmxField); override;
    procedure Edit;
    function Eof: Boolean;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    procedure First;
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

implementation

uses
  Variants, SysUtils, smxProcs, smxFuncs;

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
  Result := nil;
end;

procedure TsmxCustomField.SetInternalRef(Value: Pointer);
begin
  if Assigned(InternalRef) then
    FInternalDataSet.FieldReferenceList.RemoveReference(Self, InternalRef);
  if Assigned(Value) then
    FInternalDataSet.FieldReferenceList.InsertReference(Self, Value);
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
  if Assigned(FField) then
    FField.Clear;
end;

{function TsmxField.GetDataSet: IsmxDataSet;
begin
  if Assigned(IntfDataSet) then
    Result := IntfDataSet as IsmxDataSet else
    Result := nil;
end;}

function TsmxField.GetDataType: TsmxDataType;
begin
  if Assigned(FField) then
    Result := FField.DataType else
    Result := ftUnknown;
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

procedure TsmxField.SetDisplayFormat(const Value: String);
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

function TsmxField.GetFieldIndex: Integer;
begin
  if Assigned(FField) then
    Result := FField.FieldNo else
    Result := -1;
end;

function TsmxField.GetFieldName: String;
begin
  Result := inherited GetFieldName;
  if Assigned(FField) then
    Result := FField.FieldName;
end;

procedure TsmxField.SetFieldName(const Value: String);
begin
  inherited SetFieldName(Value);
  if Assigned(FField) then
    FField.FieldName := Value;
end;

function TsmxField.GetSize: Integer;
begin
  if Assigned(FField) then
    Result := FField.Size else
    Result := 0;
end;

function TsmxField.GetValue: Variant;
begin
  if Assigned(FField) then
    Result := FField.Value else
    Result := Variants.Null;
end;

procedure TsmxField.SetValue(const Value: Variant);
begin
  if Assigned(FField) then
    FField.Value := Value;
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

function TsmxField.IsBlob: Boolean;
begin
  if Assigned(FField) then
    Result := FField.IsBlob else
    Result := False;
end;

function TsmxField.IsNull: Boolean;
begin
  if Assigned(FField) then
    Result := FField.IsNull else
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

function TsmxField.GetInternalRef: Pointer;
begin
  Result := Pointer(FField);
end;

procedure TsmxField.SetInternalRef(Value: Pointer);
begin
  inherited SetInternalRef(Value);
  if TObject(Value) is TField then
    FField := TField(Value) else
    FField := nil;
end;

{ TsmxCustomParam }

constructor TsmxCustomParam.Create(InternalDataSet: TsmxCustomDataSet);
begin
  FInternalDataSet := InternalDataSet;
  FDataSetIntf := InternalDataSet as IsmxDataSet;
end;

destructor TsmxCustomParam.Destroy;
begin
  FDataSetIntf := nil;
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
  Result := nil;
end;

procedure TsmxCustomParam.SetInternalRef(Value: Pointer);
begin
  if Assigned(InternalRef) then
    FInternalDataSet.ParamReferenceList.RemoveReference(Self, InternalRef);
  if Assigned(Value) then
    FInternalDataSet.ParamReferenceList.InsertReference(Self, Value);
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

function TsmxCustomDataSet.AddField(const FieldName: String): IsmxField;
var
  Item: TsmxSenseItem;
begin
  Result := nil;
  Item := SenseList.FindByName(FieldName);
  if not Assigned(Item) then
    SenseList.Add.FieldName := FieldName;
end;

function TsmxCustomDataSet.AddParam(const ParamName: String): IsmxParam;
var
  Item: TsmxLocationItem;
begin
  Result := nil;
  Item := LocationList.FindByName(ParamName);
  if not Assigned(Item) then
    LocationList.Add.ParamName := ParamName;
end;

procedure TsmxCustomDataSet.ClearFields;
begin
  SenseList.Clear;
  ClearFieldReferences;
end;

procedure TsmxCustomDataSet.ClearParams;
begin
  LocationList.Clear;
  ClearParamReferences;
end;

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

function TsmxCustomDataSet.GetFieldReferenceList: TsmxReferenceList;
begin
  if not Assigned(FFieldReferenceList) then
    FFieldReferenceList := TsmxReferenceList.Create(TsmxReferenceItem);
  Result := FFieldReferenceList;
end;

function TsmxCustomDataSet.GetParamReferenceList: TsmxReferenceList;
begin
  if not Assigned(FParamReferenceList) then
    FParamReferenceList := TsmxReferenceList.Create(TsmxReferenceItem);
  Result := FParamReferenceList;
end;

procedure TsmxCustomDataSet.SetFieldReferenceList(Value: TsmxReferenceList);
begin
  FieldReferenceList.Assign(Value);
end;

procedure TsmxCustomDataSet.SetParamReferenceList(Value: TsmxReferenceList);
begin
  ParamReferenceList.Assign(Value);
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

procedure TsmxCustomDataSet.ClearFieldReferences;
begin
  while FFieldReferenceList.Count > 0 do
    TsmxCustomField(FFieldReferenceList[FFieldReferenceList.Count - 1].Owner).InternalRef := nil;
end;

procedure TsmxCustomDataSet.ClearParamReferences;
begin
  while FParamReferenceList.Count > 0 do
    TsmxCustomParam(FParamReferenceList[FParamReferenceList.Count - 1].Owner).InternalRef := nil;
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

function TsmxDataSet.AddField(const FieldName: String): IsmxField;
var
  AField: TField;
begin
  inherited AddField(FieldName);
  AField := TDataSet(GetInternalRef).FieldDefList.FieldByName(FieldName).CreateField(TDataSet(GetInternalRef));
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

function TsmxDataSet.GetInternalRef: Pointer;
begin
  Result := nil; //Pointer(InternalDataSet);
end;

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

initialization
  Classes.RegisterClass(TsmxField);

end.
