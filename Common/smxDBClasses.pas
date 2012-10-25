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
  Classes, DB, ComObj, smxBaseClasses, smxDBIntf;

type
  { TsmxCoDatabase }

  TsmxCoDatabase = class(TComObject, IsmxDatabase)
  protected
    FDatabaseIntf: IsmxDatabase;
  public
    destructor Destroy; override;

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
    function GetSense(const FieldName: String): TsmxFieldSense;
    procedure SetItem(Index: Integer; Value: TsmxSenseItem);
    procedure SetSense(const FieldName: String; Value: TsmxFieldSense);
  public
    function Add: TsmxSenseItem;
    function FindByName(const AFieldName: String): TsmxSenseItem;
    function IndexOfName(const AFieldName: String): Integer;
    procedure Insert(const AFieldName: String);
    procedure Remove(const AFieldName: String);

    property Senses[const FieldName: String]: TsmxFieldSense read GetSense write SetSense;
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
    function GetLocation(const ParamName: String): TsmxParamLocation;
    procedure SetItem(Index: Integer; Value: TsmxLocationItem);
    procedure SetLocation(const ParamName: String; Value: TsmxParamLocation);
  public
    function Add: TsmxLocationItem;
    function FindByName(const AParamName: String): TsmxLocationItem;
    function IndexOfName(const AParamName: String): Integer;
    procedure Insert(const AParamName: String);
    procedure Remove(const AParamName: String);

    property Locations[const ParamName: String]: TsmxParamLocation read GetLocation write SetLocation;
    property Items[Index: Integer]: TsmxLocationItem read GetItem write SetItem; default;
  end;

  { TsmxCustomDatabase }

  TsmxCustomDatabase = class(TsmxInterfacedComponent)
  end;

  { TsmxCustomField }

  TsmxCustomDataSet = class;

  TsmxCustomField = class(TsmxInterfacedComponent)
  private
    FIntfDataSet: TsmxCustomDataSet;
  protected
    procedure SetIntfDataSet(Value: TsmxCustomDataSet); virtual;
  public
    property IntfDataSet: TsmxCustomDataSet read FIntfDataSet write SetIntfDataSet;
  end;

  { TsmxField }

  TsmxField = class(TsmxCustomField, IsmxField)
  private
    FField: TField;
  protected
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetDisplayFormat: String;
    function GetFieldIndex: Integer;
    function GetFieldName: String;
    function GetFieldSense: TsmxFieldSense;
    function GetSize: Integer;
    function GetValue: Variant;
    procedure SetDisplayFormat(const Value: String);
    procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: String);
    procedure SetFieldSense(Value: TsmxFieldSense);
    procedure SetValue(const Value: Variant);
  public
    procedure AssignField(const Source: IsmxField);
    procedure Clear;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property Field: TField read FField write SetField;
    property FieldIndex: Integer read GetFieldIndex;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property Size: Integer read GetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { TsmxCustomParam }

  TsmxCustomParam = class(TsmxInterfacedComponent)
  private
    FIntfDataSet: TsmxCustomDataSet;
  protected
    procedure SetIntfDataSet(Value: TsmxCustomDataSet); virtual;
  public
    property IntfDataSet: TsmxCustomDataSet read FIntfDataSet write SetIntfDataSet;
  end;

  { TsmxCustomDataSet }

  TsmxCustomDataSet = class(TsmxInterfacedComponent)
  private
    FLocationList: TsmxLocationList;
    FSenseList: TsmxSenseList;
    function GetLocationList: TsmxLocationList;
    function GetSenseList: TsmxSenseList;
  protected
    procedure SetLocationList(Value: TsmxLocationList); virtual;
    procedure SetSenseList(Value: TsmxSenseList); virtual;
  public
    destructor Destroy; override;

    property LocationList: TsmxLocationList read GetLocationList write SetLocationList;
    property SenseList: TsmxSenseList read GetSenseList write SetSenseList;
  end;

  { TsmxDataSet }

  TsmxDataSet = class(TsmxCustomDataSet)
  private
    FDatabaseIntf: IsmxDatabase;
  protected
    function GetActive: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetInternalDataSet: TDataSet; virtual;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetFieldIntf(AField: TField): IsmxField; virtual;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetRecordNo(Value: Integer);

    property InternalDataSet: TDataSet read GetInternalDataSet;
  public
    destructor Destroy; override;
    procedure Add;
    function AddField(const AFieldName: String): IsmxField;
    function Bof: Boolean;
    procedure Cancel;
    procedure ClearFields;
    procedure Close;
    procedure Delete;
    procedure DeleteField(const AField: IsmxField);
    procedure Edit;
    function Eof: Boolean;
    function FieldByName(const AFieldName: String): IsmxField;
    function FindField(const AFieldName: String): IsmxField;
    procedure First;
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const AKeyFields: String; const AKeyValues: Variant): Boolean;
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
  Variants, SysUtils, smxProcs;

{ TsmxCoDatabase }

destructor TsmxCoDatabase.Destroy;
begin
  FDatabaseIntf := nil;
  inherited Destroy;
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

function TsmxSenseList.FindByName(const AFieldName: String): TsmxSenseItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].FieldName, AFieldName) = 0 then
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

function TsmxSenseList.GetSense(const FieldName: String): TsmxFieldSense;
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
end;

function TsmxSenseList.IndexOfName(const AFieldName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].FieldName, AFieldName) = 0 then
    begin
      Result := i;
      Break;
    end;
end;

procedure TsmxSenseList.Insert(const AFieldName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(AFieldName);
  if CurIndex = -1 then
    Add.FieldName := AFieldName;
end;

procedure TsmxSenseList.Remove(const AFieldName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(AFieldName);
  if CurIndex <> -1 then
    Delete(CurIndex);
end;

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

function TsmxLocationList.FindByName(const AParamName: String): TsmxLocationItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
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

function TsmxLocationList.GetLocation(const ParamName: String): TsmxParamLocation;
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
end;

function TsmxLocationList.IndexOfName(const AParamName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ParamName, AParamName) = 0 then
    begin
      Result := i;
      Break;
    end;
end;

procedure TsmxLocationList.Insert(const AParamName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(AParamName);
  if CurIndex = -1 then
    Add.ParamName := AParamName;
end;

procedure TsmxLocationList.Remove(const AParamName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(AParamName);
  if CurIndex <> -1 then
    Delete(CurIndex);
end;

{ TsmxCustomField }

procedure TsmxCustomField.SetIntfDataSet(Value: TsmxCustomDataSet);
begin
  FIntfDataSet := Value;
end;

{ TsmxField }

procedure TsmxField.AssignField(const Source: IsmxField);
begin
  if Assigned(Source) then
  begin
    Value := Source.Value;
    FieldSense := Source.FieldSense;
  end else
    Clear;
end;

procedure TsmxField.Clear;
begin
  if Assigned(FField) then
  begin
    FField.Clear;
    FieldSense := fsGeneral;
  end;
end;

function TsmxField.GetDataSet: IsmxDataSet;
begin
  if Assigned(IntfDataSet) then
    Result := IntfDataSet as IsmxDataSet else
    Result := nil;
end;

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
  if Assigned(FField) then
    Result := FField.FieldName else
    Result := '';
end;

procedure TsmxField.SetFieldName(const Value: String);
var
  OldSense: TsmxFieldSense;
begin
  if Assigned(FField) then
  begin
    OldSense := fsGeneral;
    if Assigned(IntfDataSet) then
    begin
      OldSense := IntfDataSet.SenseList.Senses[FField.FieldName];
      IntfDataSet.SenseList.Remove(FField.FieldName);
    end;
    FField.FieldName := Value;
    if Assigned(IntfDataSet) then
    begin
      IntfDataSet.SenseList.Insert(FField.FieldName);
      IntfDataSet.SenseList.Senses[FField.FieldName] := OldSense;
    end;
  end;
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

function TsmxField.GetFieldSense: TsmxFieldSense;
begin
  if Assigned(FField) and Assigned(IntfDataSet) then
    Result := IntfDataSet.SenseList.Senses[FField.FieldName] else
    Result := fsGeneral;
end;

procedure TsmxField.SetFieldSense(Value: TsmxFieldSense);
begin
  if Assigned(FField) and Assigned(IntfDataSet) then
    IntfDataSet.SenseList.Senses[FField.FieldName] := Value;
end;

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
    FField.Value := Str;
  end;
end;

procedure TsmxField.SaveToStream(Stream: TStream);
begin
  if Assigned(FField) then
    smxProcs.StrToStream(Variants.VarToStr(FField.Value), Stream);
end;

procedure TsmxField.SetField(Value: TField);
begin
  FField := Value;
end;

{ TsmxCustomParam }

procedure TsmxCustomParam.SetIntfDataSet(Value: TsmxCustomDataSet);
begin
  FIntfDataSet := Value;
end;

{ TsmxCustomDataSet }

destructor TsmxCustomDataSet.Destroy;
begin
  if Assigned(FLocationList) then
    FLocationList.Free;
  if Assigned(FSenseList) then
    FSenseList.Free;
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

{ TsmxDataSet }

destructor TsmxDataSet.Destroy;
begin
  FDatabaseIntf := nil;
  inherited Destroy;
end;

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

function TsmxDataSet.AddField(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  if Assigned(InternalDataSet) then
  begin
    Field := InternalDataSet.FieldDefList.FieldByName(AFieldName).CreateField(InternalDataSet);
    Result := GetFieldIntf(Field);
    SenseList.Insert(AFieldName);
  end;
end;

procedure TsmxDataSet.DeleteField(const AField: IsmxField);
var
  Field: TField;
begin
  if Assigned(InternalDataSet) and Assigned(AField) then
  begin
    Field := InternalDataSet.Fields.FindField(AField.FieldName);
    if Assigned(Field) then
    begin
      InternalDataSet.Fields.Remove(Field);
      SenseList.Remove(AField.FieldName);
    end;
  end;
end;

function TsmxDataSet.Bof: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Bof else
    Result := False;
end;

function TsmxDataSet.Eof: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Eof else
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

procedure TsmxDataSet.ClearFields;
begin
  if Assigned(InternalDataSet) then
  begin
    InternalDataSet.ClearFields;
    SenseList.Clear;
  end;
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

function TsmxDataSet.FieldByName(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  if Assigned(InternalDataSet) then
  begin
    Field := InternalDataSet.Fields.FieldByName(AFieldName);
    Result := GetFieldIntf(Field);
  end;
end;

function TsmxDataSet.FindField(const AFieldName: String): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  if Assigned(InternalDataSet) then
  begin
    Field := InternalDataSet.Fields.FindField(AFieldName);
    Result := GetFieldIntf(Field);
  end;
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
    Result := InternalDataSet.Active else
    Result := False;
end;

procedure TsmxDataSet.SetActive(Value: Boolean);
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.Active := Value;
end;

function TsmxDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxDataSet.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxDataSet.GetInternalDataSet: TDataSet;
begin
  Result := nil;
end;

function TsmxDataSet.GetField(Index: Integer): IsmxField;
var
  Field: TField;
begin
  Result := nil;
  if Assigned(InternalDataSet) then
  begin
    Field := InternalDataSet.Fields[Index];
    Result := GetFieldIntf(Field);
  end;
end;

procedure TsmxDataSet.SetField(Index: Integer; const Value: IsmxField);
var
  Field: IsmxField;
begin
  if Assigned(InternalDataSet) then
  begin
    Field := GetFieldIntf(InternalDataSet.Fields[Index]);
    Field.AssignField(Value);
  end;
end;

function TsmxDataSet.GetFieldCount: Integer;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.FieldCount else
    Result := 0;
end;

function TsmxDataSet.GetFieldIntf(AField: TField): IsmxField;
var
  IntfField: TsmxField;
begin
  IntfField := TsmxField.Create;
  IntfField.Field := AField;
  IntfField.IntfDataSet := Self;
  Result := IntfField as IsmxField;
end;

function TsmxDataSet.GetRecordCount: Integer;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.RecordCount else
    Result := 0;
end;

function TsmxDataSet.GetRecordNo: Integer;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.RecNo else
    Result := -1;
end;

procedure TsmxDataSet.SetRecordNo(Value: Integer);
begin
  if Assigned(InternalDataSet) then
    InternalDataSet.RecNo := Value;
end;

function TsmxDataSet.IsEmpty: Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.IsEmpty else
    Result := False;
end;

function TsmxDataSet.Locate(const AKeyFields: String;
  const AKeyValues: Variant): Boolean;
begin
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Locate(AKeyFields, AKeyValues, []) else
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

end.
