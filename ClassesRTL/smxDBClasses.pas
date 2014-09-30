{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Database classes           }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Аleksandr          }
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
    FDatabaseIntf: IsmxDatabase;
  protected
    function CreateDatabase: IsmxDatabase; virtual;
    function GetController: IsmxBaseInterface; virtual;
    function GetDescription: String;
    function GetVersion: String; virtual;
  public
    destructor Destroy; override;
    procedure Initialize; override;

    property Database: IsmxDatabase read FDatabaseIntf implements IsmxDatabase;
    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  { TsmxObjectItem }

  (*TsmxObjectList = class;

  TsmxObjectItem = class(TsmxKitItem)
  private
    function GetKit: TsmxObjectList;
    function GetItemObjectInterface: IsmxObjectItem;
    procedure SetItemObject(Value: TPersistent);
    procedure SetKit(Value: TsmxObjectList);
  protected
    FItemObject: TPersistent;
    //procedure AddObject(ObjectClass: TPersistentClass = nil); virtual;
    procedure AddObject; virtual;
    procedure DelObject; virtual;
    function GetDisplayName: String; override;
    function GetDisplayObject: TObject; override;
    procedure SetIndex(Value: Integer); override;

    property ItemObjectInterface: IsmxObjectItem read GetItemObjectInterface;
  public
    constructor Create(AKit: TsmxKit); {overload;} override;
    //constructor Create(AKit: TsmxKit; AObjectClass: TPersistentClass); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;
    //procedure InitializeItemObject(ItemObject: TPersistent);

    property Kit: TsmxObjectList read GetKit write SetKit;
    property ItemObject: TPersistent read FItemObject write SetItemObject;
  end;

  { TsmxObjectList }

  TsmxObjectList = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxObjectItem;
    function GetOwnerObjectInterface: IsmxObjectList;
    procedure SetItem(Index: Integer; Value: TsmxObjectItem);
  protected
    property OwnerObjectInterface: IsmxObjectList read GetOwnerObjectInterface;
  public
    function Add: TsmxObjectItem; {overload;}
    //function Add(ObjectClass: TPersistentClass): TsmxObjectItem; overload;
    //procedure Assign(Source: TsmxKit); override;

    property Items[Index: Integer]: TsmxObjectItem read GetItem write SetItem; default;
  end;*)

  { TsmxFieldItem }

  TsmxFieldList = class;
  //TsmxCustomField = class;

  {TsmxFieldItem = class(TsmxKitItem)
  private
    //FDataType: TsmxDataType;
    //FDisplayFormat: String;
    FField: IsmxField;
    //FFieldIndex: Integer;
    //FFieldName: String;
    //FFieldSense: TsmxDataSense;
    //FPrecision: Integer;
    //FSize: Integer;
    function GetKit: TsmxFieldList;
    procedure SetKit(Value: TsmxFieldList);
  protected
    procedure AddField; virtual;
    procedure DelField; virtual;
    function GetDataType: TsmxDataType; virtual;
    function GetDisplayFormat: String; virtual;
    function GetFieldIndex: Integer; virtual;
    function GetFieldName: String; virtual;
    function GetDataSense: TsmxDataSense; virtual;
    function GetPrecision: Integer; virtual;
    function GetSize: Integer; virtual;
    procedure SetDataType(Value: TsmxDataType); virtual;
    procedure SetDisplayFormat(const Value: String); virtual;
    //procedure SetField(Value: TsmxCustomField); virtual;
    procedure SetFieldIndex(Value: Integer); virtual;
    procedure SetFieldName(const Value: String); virtual;
    procedure SetDataSense(Value: TsmxDataSense); virtual;
    //procedure SetIndex(Value: Integer); override;
    procedure SetPrecision(Value: Integer); virtual;
    procedure SetSize(Value: Integer); virtual;

    property Field: IsmxField read FField write FField;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxFieldList read GetKit write SetKit;
  published
    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;
    property FieldName: String read GetFieldName write SetFieldName;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
  end;}

  TsmxCustomField = class;

  TsmxFieldItem = class(TsmxObjectItem)
  private
    function GetItemObject: TsmxCustomField;
    function GetKit: TsmxFieldList;
    procedure SetItemObject(Value: TsmxCustomField);
    procedure SetKit(Value: TsmxFieldList);
  public
    property Kit: TsmxFieldList read GetKit write SetKit;
  published
    property ItemObject: TsmxCustomField read GetItemObject write SetItemObject;
  end;

  //TsmxSenseItemClass = class of TsmxFieldItem;

  { TsmxFieldList }

  //TsmxCustomDataSet = class;

  TsmxFieldList = class(TsmxObjectList)
  private
    //FInternalDataSet: TsmxCustomDataSet;
    function GetItem(Index: Integer): TsmxFieldItem;
    //function GetSense(const FieldName: String): TsmxDataSense;
    procedure SetItem(Index: Integer; Value: TsmxFieldItem);
    //procedure SetSense(const FieldName: String; Value: TsmxDataSense);
  //protected
    //procedure AddField(Item: TsmxFieldItem); virtual;
    //procedure DelField(Item: TsmxFieldItem); virtual;

    //property InternalDataSet: TsmxCustomDataSet read FInternalDataSet write FInternalDataSet;
  public
    //constructor Create(AItemClass: TsmxKitItemClass; InternalDataSet: TsmxCustomDataSet); reintroduce; overload; virtual;
    function Add: TsmxFieldItem;
    //procedure Delete(Index: Integer);
    function FindByName(const FieldName: String): TsmxFieldItem;
    //function FindByField(const Field: IsmxField): TsmxFieldItem;
    //function IndexOfField(const Field: IsmxField): Integer;
    //function IndexOfFieldName(const FieldName: String): Integer;
    //function IndexOfFieldIndex(FieldIndex: Integer): Integer;
    //function Insert(const FieldName: String): Integer;
    //function Remove(const FieldName: String): Integer;

    //property Senses[const FieldName: String]: TsmxDataSense read GetSense write SetSense;
    property Items[Index: Integer]: TsmxFieldItem read GetItem write SetItem; default;
  end;

  { TsmxParamItem }

  TsmxParamList = class;
  //TsmxCustomParam = class;

  {TsmxParamItem = class(TsmxKitItem)
  private
    FParam: IsmxParam;
    //FDataType: TsmxDataType;
    //FNumericScale: Integer;
    //FParamLocation: TsmxDataLocation;
    //FParamName: String;
    //FParamType: TsmxParamType;
    //FParamValue: Variant;
    //FPrecision: Integer;
    //FSize: Integer;
    function GetKit: TsmxParamList;
    procedure SetKit(Value: TsmxParamList);
  protected
    procedure AddParam; virtual;
    procedure DelParam; virtual;
    function GetDataLocation: TsmxDataLocation; virtual;
    function GetDataSense: TsmxDataSense; virtual;
    function GetDataType: TsmxDataType; virtual;
    function GetNumericScale: Integer; virtual;
    function GetParamIndex: Integer; virtual;
    function GetParamName: String; virtual;
    function GetParamType: TsmxParamType; virtual;
    function GetParamValue: Variant; virtual;
    function GetPrecision: Integer; virtual;
    function GetSize: Integer; virtual;
    procedure SetDataLocation(Value: TsmxDataLocation); virtual;
    procedure SetDataSense(Value: TsmxDataSense); virtual;
    procedure SetDataType(Value: TsmxDataType); virtual;
    procedure SetNumericScale(Value: Integer); virtual;
    procedure SetParamIndex(Value: Integer); virtual;
    procedure SetParamName(const Value: String); virtual;
    procedure SetParamType(Value: TsmxParamType); virtual;
    procedure SetParamValue(const Value: Variant); virtual;
    procedure SetPrecision(Value: Integer); virtual;
    procedure SetSize(Value: Integer); virtual;

    property Param: IsmxParam read FParam write FParam;
  public
    constructor Create(AKit: TsmxKit); override;
    destructor Destroy; override;
    procedure Assign(Source: TsmxKitItem); override;

    property Kit: TsmxParamList read GetKit write SetKit;
  published
    property DataLocation: TsmxDataLocation read GetDataLocation write SetDataLocation;
    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    property ParamIndex: Integer read GetParamIndex write SetParamIndex;
    property ParamName: String read GetParamName write SetParamName;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property ParamValue: Variant read GetParamValue write SetParamValue;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
  end;}

  TsmxCustomParam = class;

  TsmxParamItem = class(TsmxObjectItem)
  private
    function GetItemObject: TsmxCustomParam;
    function GetKit: TsmxParamList;
    procedure SetItemObject(Value: TsmxCustomParam);
    procedure SetKit(Value: TsmxParamList);
  public
    property Kit: TsmxParamList read GetKit write SetKit;
  published
    property ItemObject: TsmxCustomParam read GetItemObject write SetItemObject;
  end;

  { TsmxParamList }

  TsmxParamList = class(TsmxObjectList)
  private
    //FInternalDataSet: TsmxCustomDataSet;
    function GetItem(Index: Integer): TsmxParamItem;
    //function GetLocation(const ParamName: String): TsmxDataLocation;
    procedure SetItem(Index: Integer; Value: TsmxParamItem);
    //procedure SetLocation(const ParamName: String; Value: TsmxDataLocation);
  //protected
    //procedure AddParam(Item: TsmxParamItem); virtual;
    //procedure DelParam(Item: TsmxParamItem); virtual;

    //property InternalDataSet: TsmxCustomDataSet read FInternalDataSet write FInternalDataSet;
  public
    function Add: TsmxParamItem;
    //procedure Delete(Index: Integer);
    //function IndexOfParam(const Param: IsmxParam): Integer;
    function FindByName(const ParamName: String): TsmxParamItem;
    //function IndexOfParamName(const ParamName: String): Integer;
    //function InsertLocation(const ParamName: String; Location: TsmxDataLocation): Integer;
    //function RemoveLocation(const ParamName: String; Location: TsmxDataLocation): Integer;

    //property Locations[const ParamName: String]: TsmxDataLocation read GetLocation write SetLocation;
    property Items[Index: Integer]: TsmxParamItem read GetItem write SetItem; default;
  end;

  { TsmxCustomDatabase }

  TsmxCustomDatabase = class(TsmxInterfacedComponent, IsmxConnection)
  private
    FDatabaseManagerIntf: IsmxDatabaseManager;
  protected
    function GetDatabase: IsmxDatabase;
    function GetDatabaseManager: IsmxDatabaseManager;
    function GetDatabaseName: String; virtual;
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager); virtual;
    procedure SetDatabaseName(const Value: String); virtual;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property Database: IsmxDatabase read GetDatabase;
    property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  { TsmxCustomField }

  TsmxCustomDataSet = class;

  TsmxCustomField = class(TsmxInterfacedPersistent, IsmxObjectItem)
  private
    //FDataSetIntf: IsmxDataSet;
    FDataType: TsmxDataType;
    FDisplayFormat: String;
    //FFieldIndex: Integer;
    FFieldName: String;
    FDataSense: TsmxDataSense;
    FInternalDataSet: TsmxCustomDataSet;
    //FInternalRef: Pointer;
    FPrecision: Integer;
    //FSenseItem: TsmxFieldItem;
    FSize: Integer;
    function GetFieldItem: TsmxFieldItem;
  protected
    FFieldItem: TsmxFieldItem;
    procedure ChangeObjectIndex(Value: Integer); virtual;
    procedure ChangeObjectOwner(Value: TPersistent); virtual;
    function GetDataSense: TsmxDataSense; //virtual;
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType; //virtual;
    function GetDisplayFormat: String; //virtual;
    function GetFieldIndex: Integer; //virtual;
    function GetFieldName: String; //virtual;
    //function GetInternalRef: Pointer; override;
    function GetPrecision: Integer; //virtual;
    function GetSize: Integer; //virtual;
    procedure SetDataSense(Value: TsmxDataSense); virtual;
    procedure SetDataType(Value: TsmxDataType); virtual;
    procedure SetDisplayFormat(const Value: String); virtual;
    procedure SetFieldIndex(Value: Integer); //virtual;
    procedure SetFieldName(const Value: String); virtual;
    procedure SetInternalDataSet(Value: TsmxCustomDataSet); //virtual;
    //procedure SetInternalRef(Value: Pointer); virtual;
    procedure SetPrecision(Value: Integer); virtual;
    procedure SetSize(Value: Integer); virtual;
    //procedure SetIntfDataSet(Value: TsmxCustomDataSet); virtual;

    //property SenseItem: TsmxFieldItem read FSenseItem write FSenseItem;
    property FieldItem: TsmxFieldItem read GetFieldItem;
  public
    //constructor Create({AOwner: TComponent;} InternalDataSet: TsmxCustomDataSet); virtual;
    destructor Destroy; override;
    procedure AssignField(const Source: IsmxField); virtual;
    procedure Clear; virtual;

    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property InternalDataSet: TsmxCustomDataSet read FInternalDataSet write SetInternalDataSet;
    //property InternalRef: Pointer read GetInternalRef write SetInternalRef;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
  published
    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex stored False;
    property FieldName: String read GetFieldName write SetFieldName;
  end;

  //TsmxCustomFieldClass = class of TsmxCustomField;

  { TsmxField }

  TsmxField = class(TsmxCustomField, IsmxField)
  private
    //FField: TField;
    //FDataType: TsmxDataType;
    //FDisplayFormat: String;
    //FFieldName: String;
    //FPrecision: Integer;
    //FSize: Integer;
    //FValue: Variant;
    //function GetField: TField;
    //function CreateField(DataType: TsmxDataType): TField;
    //procedure ChangeDisplayFormat(const Format: String);
    //procedure ChangeFieldName(const Name: String);
    //procedure ChangePrecision(Accuracy: Integer);
  protected
    FField: TField;
    procedure ChangeObjectIndex(Value: Integer); override;
    procedure ChangeObjectOwner(Value: TPersistent); override;
    //function GetDataSet: IsmxDataSet;
    //function GetDataType: TsmxDataType;
    //function GetDisplayFormat: String;
    //function GetFieldIndex: Integer; //override;
    //function GetFieldName: String; //override;
    //function GetDataSense: TsmxDataSense;
    function GetInternalRef: Pointer; override;
    //function GetPrecision: Integer;
    //function GetSize: Integer;
    function GetValue: Variant;
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataType(Value: TsmxDataType); override;
    procedure SetDisplayFormat(const Value: String); override;
    //procedure SetField(Value: TField); virtual;
    //procedure SetFieldIndex(Value: Integer); //override;
    procedure SetFieldName(const Value: String); override;
    //procedure SetDataSense(Value: TsmxDataSense);
    //procedure SetInternalRef(Value: Pointer); override;
    procedure SetPrecision(Value: Integer); override;
    procedure SetSize(Value: Integer); override;
    procedure SetValue(const Value: Variant);

    //property Field: TField read FField write FField;//GetField;
  public
    //destructor Destroy; override;
    procedure AssignField(const Source: IsmxField); override;
    procedure Clear; override;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    //property DataSet: IsmxDataSet read GetDataSet;
    //property DataType: TsmxDataType read GetDataType write SetDataType;
    //property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    //property Field: TField read FField write SetField;
    //property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;
    //property FieldName: String read GetFieldName write SetFieldName;
    //property FieldSense: TsmxDataSense read GetDataSense write SetDataSense;
    //property Precision: Integer read GetPrecision write SetPrecision;
    //property Size: Integer read GetSize write SetSize;
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
    //FDataSetIntf: IsmxDataSet;
    FInternalDataSet: TsmxCustomDataSet;
    //FInternalRef: Pointer;
    FDataSense: TsmxDataSense;
    FDataType: TsmxDataType;
    FNumericScale: Integer;
    //FParamIndex: Integer;
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
    //function GetInternalRef: Pointer; override;
    function GetDataLocation: TsmxDataLocation; //virtual;
    function GetDataSense: TsmxDataSense; //virtual;
    function GetDataType: TsmxDataType; //virtual;
    function GetNumericScale: Integer; //virtual;
    function GetParamIndex: Integer; //virtual;
    function GetParamName: String; //virtual;
    function GetParamType: TsmxParamType; //virtual;
    function GetPrecision: Integer; //virtual;
    function GetSize: Integer; //virtual;
    function GetValue: Variant; //virtual;
    //procedure SetInternalRef(Value: Pointer); virtual;
    procedure SetDataLocation(Value: TsmxDataLocation); virtual;
    procedure SetDataSense(Value: TsmxDataSense); virtual;
    procedure SetDataType(Value: TsmxDataType); virtual;
    procedure SetInternalDataSet(Value: TsmxCustomDataSet); //virtual;
    procedure SetNumericScale(Value: Integer); virtual;
    procedure SetParamIndex(Value: Integer); //virtual;
    procedure SetParamName(const Value: String); virtual;
    procedure SetParamType(Value: TsmxParamType); virtual;
    procedure SetPrecision(Value: Integer); virtual;
    procedure SetSize(Value: Integer); virtual;
    procedure SetValue(const Value: Variant); virtual;
    //procedure SetIntfDataSet(Value: TsmxCustomDataSet); virtual;

    property ParamItem: TsmxParamItem read GetParamItem;
  public
    //constructor Create(InternalDataSet: TsmxCustomDataSet); virtual;
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

  TsmxCustomParamClass = class of TsmxCustomParam;

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

  //TsmxReferenceList = class;

  {TsmxReferenceItem = class(TsmxKitItem)
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
  end;}

  { TsmxReferenceList }

  {TsmxReferenceList = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxReferenceItem;
    procedure SetItem(Index: Integer; Value: TsmxReferenceItem);
  public
    function Add: TsmxReferenceItem;
    function FindByCombo(Owner: TsmxInterfacedPersistent; Reference: Pointer): TsmxReferenceItem;
    procedure InsertReference(Owner: TsmxInterfacedPersistent; Reference: Pointer);
    procedure RemoveReference(Owner: TsmxInterfacedPersistent; Reference: Pointer);

    property Items[Index: Integer]: TsmxReferenceItem read GetItem write SetItem; default;
  end;}

  { TsmxCustomDataSet }

  TsmxCustomDataSet = class(TsmxInterfacedComponent, IsmxObjectList)
  private
    FDatabaseIntf: IsmxDatabase;
    //FFieldReferenceList: TsmxReferenceList;
    FParamList: TsmxParamList;
    //FParamReferenceList: TsmxReferenceList;
    FFieldList: TsmxFieldList;
    FPerformanceMode: TsmxPerformanceMode;
    //FFieldIntfList: TInterfaceList;
    //FParamIntfList: TInterfaceList;
    //function GetFieldReferenceList: TsmxReferenceList;
    function GetFieldList: TsmxFieldList;
    function GetParamList: TsmxParamList;
    procedure SetFieldList(Value: TsmxFieldList);
    procedure SetParamList(Value: TsmxParamList);
    //function GetParamReferenceList: TsmxReferenceList;
    //function GetFieldIntfList: TInterfaceList;
    //function GetParamIntfList: TInterfaceList;
    //procedure ClearFieldReferences;
    //procedure ClearParamReferences;
    //procedure ResetFieldReference(Reference: Pointer);
    //procedure ResetParamReference(Reference: Pointer);
  protected
    procedure CheckObjectClass(Item: TObject; ObjectClass: TPersistentClass);
    procedure CreateObject(Item: TObject); overload; virtual;
    procedure CreateObject(Item: TObject; ObjectClass: TPersistentClass); overload; virtual;
    procedure DestroyObject(Item: TObject); virtual;
    function GetDatabase: IsmxDatabase; //virtual;
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    function GetFieldClass: TsmxInterfacedPersistentClass; virtual;
    function GetParamClass: TsmxInterfacedPersistentClass; virtual;
    //procedure SetFieldReferenceList(Value: TsmxReferenceList); virtual;
    //procedure SetParamReferenceList(Value: TsmxReferenceList); virtual;
    //procedure CreateInternalField(Field: TsmxCustomField); virtual;
    //procedure CreateInternalParam(Param: TsmxCustomParam); virtual;
    //procedure DestroyInternalField(Field: TsmxCustomField); virtual;
    //procedure DestroyInternalParam(Param: TsmxCustomParam); virtual;
    function GetPerformanceMode: TsmxPerformanceMode;
    function GetField(Index: Integer): IsmxField; //virtual;
    function GetFieldCount: Integer; //virtual;
    procedure SetField(Index: Integer; const Value: IsmxField); //virtual;
    function GetParamCount: Integer; //virtual;
    function GetParam(Index: Integer): IsmxParam; //virtual;
    procedure SetParam(Index: Integer; const Value: IsmxParam); //virtual;
    procedure SetPerformanceMode(Value: TsmxPerformanceMode); virtual;
    //procedure SetDatabase(const Value: IsmxDatabase); virtual;

    //property FieldIntfList: TInterfaceList read GetFieldIntfList;
    //property ParamIntfList: TInterfaceList read GetParamIntfList;
  public
    destructor Destroy; override;
    function AddField: IsmxField; //virtual;
    function AddParam: IsmxParam; //virtual;
    procedure AssignDataSet(const Source: IsmxDataSet); virtual;
    procedure ClearFields; //virtual;
    procedure ClearParams; //virtual;
    procedure DeleteField(const Field: IsmxField); //virtual;
    procedure DeleteParam(const Param: IsmxParam); //virtual;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    function FindParam(const ParamName: String): IsmxParam;
    function ParamByName(const ParamName: String): IsmxParam;

    //property FieldReferenceList: TsmxReferenceList read GetFieldReferenceList write SetFieldReferenceList;
    //property ParamReferenceList: TsmxReferenceList read GetParamReferenceList write SetParamReferenceList;
    //property FieldIntfClass: TsmxCustomFieldClass read GetFieldIntfClass;
    //property ParamIntfClass: TsmxCustomParamClass read GetParamIntfClass;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
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
    //FPerformanceMode: TsmxPerformanceMode;
    //FDatabaseIntf: IsmxDatabase;
    //procedure ClearFieldRef;
    //procedure ClearParamRef;
    function GetInternalDataSet: TDataSet; 
  protected
    //FInternalDataSet: TDataSet;
    function GetActive: Boolean;
    //function GetDatabase: IsmxDatabase;
    //function GetField(Index: Integer): IsmxField;
    //function GetFieldCount: Integer;
    //function GetFieldIntf(Field: TField): IsmxField; virtual;
    //function GetInternalDataSetClass: TComponentClass; virtual;
    //function GetInternalRef: Pointer; override;
    //function GetFieldIntf: IsmxField; virtual;
    //function GetParamIntf: IsmxParam; virtual;
    //function GetPerformanceMode: TsmxPerformanceMode;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    //function GetSQL: TStrings; virtual;
    //procedure SetSQL(Value: TStrings); virtual;
    procedure SetActive(Value: Boolean);
    //procedure SetDatabase(const Value: IsmxDatabase); virtual;
    //procedure SetField(Index: Integer; const Value: IsmxField);
    //procedure SetPerformanceMode(Value: TsmxPerformanceMode);
    procedure SetRecordNo(Value: Integer);
    //procedure CreateInternalField(Field: TsmxCustomField); virtual;
    //procedure DestroyInternalField(Field: TsmxCustomField); virtual;
    //procedure CreateInternalParam(Param: TsmxCustomParam); virtual;
    //procedure DestroyInternalParam(Param: TsmxCustomParam); virtual;
    //procedure CreateObject(Item: TObject); override;
    //procedure DestroyObject(Item: TObject); override;
    function GetFieldClass: TsmxInterfacedPersistentClass; override;

    property InternalDataSet: TDataSet read GetInternalDataSet;//FInternalDataSet write FInternalDataSet;//GetInternalDataSet;
  public
    //destructor Destroy; override;
    procedure Add;
    //function AddField{(DataType: TsmxDataType)}: IsmxField; override;
    //function AddParam: IsmxParam; override;
    //procedure AssignDataSet(const Source: IsmxDataSet); override;
    function Bof: Boolean;
    procedure Cancel;
    //procedure ClearFields; override;
    //procedure ClearParams; override;
    //function CreateField(DataType: TsmxDataType): TObject; override;
    procedure CreateInternalField(Field: TsmxCustomField); virtual;
    procedure DestroyInternalField(Field: TsmxCustomField); virtual;
    procedure CreateInternalParam(Param: TsmxCustomParam); virtual;
    procedure DestroyInternalParam(Param: TsmxCustomParam); virtual;
    procedure Close; virtual;
    procedure Delete;
    //procedure DeleteField(const Field: IsmxField); override;
    //procedure DeleteParam(const Param: IsmxParam); override;
    //procedure DisableControls;
    procedure Edit;
    //procedure EnalbleControls;
    function Eof: Boolean;
    procedure Execute; virtual;
    //function FieldByName(const FieldName: String): IsmxField;
    //function FindField(const FieldName: String): IsmxField;
    procedure First;
    //procedure FreeBookmark(Bookmark: Pointer);
    //function GetBookmark: Pointer;
    //procedure GotoBookmark(Bookmark: Pointer);
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TsmxLocateOptions = []): Boolean;
    procedure Next;
    procedure Open; virtual;
    procedure Perform;
    procedure Post;
    procedure Prior;

    //property Database: IsmxDatabase read GetDatabase write SetDatabase;
    //property FieldCount: Integer read GetFieldCount;
    //property Fields[Index: Integer]: IsmxField read GetField write SetField;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
    //property SQL: TStrings read GetSQL write SetSQL;
  published
    property Active: Boolean read GetActive write SetActive stored False;
    //property PerformanceMode: TsmxPerformanceMode read GetPerformanceMode write SetPerformanceMode;
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
      {Perform: TsmxPerformanceMode = pmOpen;} const DSFrom: IsmxDataSet = nil): Boolean;

    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
    property ParamValues[const Key: String]: Variant read GetValue write SetValue; default;
  end;

  { TsmxCustomConnection }

  {TsmxCustomConnection = class(TsmxComponent, IsmxConnection)
  private
    FDatabaseIntf: IsmxDatabase;
    FDatabaseManagerIntf: IsmxDatabaseManager;
  protected
    //procedure FreeConnection;
    //function GetConnected: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDatabaseManager: IsmxDatabaseManager;
    //function GetDatabaseName: String; virtual;
    //procedure SetConnected(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager); virtual;
    //procedure SetDatabaseName(const Value: String); virtual;
  public
    destructor Destroy; override;
    //procedure Connect;
    //procedure Disconnect;

    //property Connected: Boolean read GetConnected write SetConnected;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;
  end;}

  { TsmxConnection }

  //TsmxConnection = class(TsmxCustomConnection)
  {private
    FDatabaseIntf: IsmxDatabase;
    //FDatabaseManagerIntf: IsmxDatabaseManager;
  protected
    //procedure FreeConnection;
    //function GetConnected: Boolean;
    function GetDatabase: IsmxDatabase;
    //function GetDatabaseManager: IsmxDatabaseManager;
    //function GetDatabaseName: String; override;
    //function GetInternalRef: Pointer; override;
    //procedure SetConnected(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase); virtual;
    //procedure SetDatabaseManager(const Value: IsmxDatabaseManager); virtual;
    //procedure SetDatabaseName(const Value: String); override;
  public
    destructor Destroy; override;
    //procedure Connect;
    //procedure Disconnect;

    //property Connected: Boolean read GetConnected write SetConnected;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    //property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;}
  //end;

  { IsmxObjectList }

  //IsmxSlaveInterface = interface(IsmxBaseInterface)
  //  procedure ChangeSlaveIndex(Value: Integer);
  //  procedure CreateSlave(Item: TsmxSlaveItem);
  ////  procedure CreateSlave(Item: TsmxSlaveItem; SlaveClass: TPersistentClass = nil);
  //  procedure DestroySlave(Item: TsmxSlaveItem);
  //  procedure ChangeSlaveOwner(Value: TPersistent);
  ////  procedure CheckSlaveClass(SlaveClass: TPersistentClass);
  //end;

implementation

uses
  Variants, SysUtils, smxProcs, smxFuncs, smxConsts;

{ TsmxCoDatabase }

procedure TsmxCoDatabase.Initialize;
begin
  inherited Initialize;
  FDatabaseIntf := CreateDatabase;
end;

destructor TsmxCoDatabase.Destroy;
begin
  if Assigned(FDatabaseIntf) then
  begin
    FDatabaseIntf.GetReference.Free;
    FDatabaseIntf := nil;
  end;
  inherited Destroy;
end;

function TsmxCoDatabase.CreateDatabase: IsmxDatabase;
begin
  Result := nil;
end;

function TsmxCoDatabase.GetController: IsmxBaseInterface;
begin
  Result := nil;
end;

function TsmxCoDatabase.GetDescription: String;
begin
  Result := ClassName;
end;

function TsmxCoDatabase.GetVersion: String;
begin
  Result := '';
end;

{ TsmxFieldItem }

{constructor TsmxFieldItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  AddField;
end;

destructor TsmxFieldItem.Destroy;
begin
  DelField;
  inherited Destroy;
end;

procedure TsmxFieldItem.AddField;
var
  AField: TsmxCustomField;
begin
  if Assigned(Kit) then
    if Assigned(Kit.InternalDataSet) then
      if not Assigned(FField) then
      begin
        AField := Kit.InternalDataSet.GetFieldIntfClass.Create;
        AField.InternalDataSet := Kit.InternalDataSet;
        FField := AField as IsmxField;
      end;
end;

procedure TsmxFieldItem.DelField;
var
  AField: TsmxCustomField;
begin
  if Assigned(Kit) then
    if Assigned(Kit.InternalDataSet) then
      if Assigned(FField) then
      begin
        AField := TsmxCustomField(FField.GetReference);
        AField.InternalDataSet := nil;
        FField := nil;
      end;
end;}

{procedure TsmxFieldItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxFieldItem then
  begin
    DataSense := TsmxFieldItem(Source).DataSense;
    DataType := TsmxFieldItem(Source).DataType;
    DisplayFormat := TsmxFieldItem(Source).DisplayFormat;
    FieldName := TsmxFieldItem(Source).FieldName;
    Precision := TsmxFieldItem(Source).Precision;
    Size := TsmxFieldItem(Source).Size;
  end else
    inherited Assign(Source);
end;}

function TsmxFieldItem.GetItemObject: TsmxCustomField;
begin
  Result := TsmxCustomField(inherited ItemObject);
end;

procedure TsmxFieldItem.SetItemObject(Value: TsmxCustomField);
begin
  inherited ItemObject := Value;
end;

function TsmxFieldItem.GetKit: TsmxFieldList;
begin
  Result := TsmxFieldList(inherited Kit);
end;

procedure TsmxFieldItem.SetKit(Value: TsmxFieldList);
begin
  //if Assigned(Kit) then
    //DelField;
  inherited Kit := Value;
  //if Assigned(Kit) then
    //AddField;
end;

{procedure TsmxFieldItem.SetDataType(Value: TsmxDataType);
begin
  if Assigned(FField) then
    FField.DataType := Value;
end;

procedure TsmxFieldItem.SetDisplayFormat(const Value: String);
begin
  if Assigned(FField) then
    FField.DisplayFormat := Value;
end;}

{procedure TsmxFieldItem.SetField(Value: TsmxCustomField);
begin
  if Assigned(FField) then
    FField.SenseItem := nil;
  FField := Value;
  if Assigned(FField) then
  begin
    FField.SenseItem := Self;
    FField.DataType := DataType;
    FField.DisplayFormat := DisplayFormat;
    FField.FieldIndex := FieldIndex;
    FField.FieldName := FieldName;
    FField.FieldSense := FieldSense;
    FField.Precision := Precision;
    FField.Size := Size;
  end;
end;}

{procedure TsmxFieldItem.SetFieldIndex(Value: Integer);
begin
  if Assigned(FField) then
    FField.FieldIndex := Value;
end;

procedure TsmxFieldItem.SetFieldName(const Value: String);
begin
  if Assigned(FField) then
    FField.FieldName := Value;
end;}

{procedure TsmxFieldItem.SetDataSense(Value: TsmxDataSense);
begin
  if Assigned(FField) then
    FField.DataSense := Value;
end;

procedure TsmxFieldItem.SetPrecision(Value: Integer);
begin
  if Assigned(FField) then
    FField.Precision := Value;
end;

procedure TsmxFieldItem.SetSize(Value: Integer);
begin
  if Assigned(FField) then
    FField.Size := Value;
end;

function TsmxFieldItem.GetDataType: TsmxDataType;
begin
  if Assigned(FField) then
    Result := FField.DataType else
    Result := ftUnknown;
end;

function TsmxFieldItem.GetDisplayFormat: String;
begin
  if Assigned(FField) then
    Result := FField.DisplayFormat else
    Result := '';
end;

function TsmxFieldItem.GetFieldIndex: Integer;
begin
  if Assigned(FField) then
    Result := FField.FieldIndex else
    Result := -1;
end;

function TsmxFieldItem.GetFieldName: String;
begin
  if Assigned(FField) then
    Result := FField.FieldName else
    Result := '';
end;}

{function TsmxFieldItem.GetDataSense: TsmxDataSense;
begin
  if Assigned(FField) then
    Result := FField.DataSense else
    Result := dsGeneral;
end;

function TsmxFieldItem.GetPrecision: Integer;
begin
  if Assigned(FField) then
    Result := FField.Precision else
    Result := 0;
end;

function TsmxFieldItem.GetSize: Integer;
begin
  if Assigned(FField) then
    Result := FField.Size else
    Result := 0;
end;}

{ TsmxFieldList }

{constructor TsmxFieldList.Create(AItemClass: TsmxKitItemClass;
  InternalDataSet: TsmxCustomDataSet);
begin
  Create(AItemClass);
  FInternalDataSet := InternalDataSet;
end;}

function TsmxFieldList.Add: TsmxFieldItem;
begin
  Result := TsmxFieldItem(inherited Add);
  //AddField(Result);
end;

{procedure TsmxFieldList.Delete(Index: Integer);
begin
  DelField(Items[Index]);
  inherited Delete(Index);
end;}

{procedure TsmxFieldList.AddField(Item: TsmxFieldItem);
var
  Field: TsmxCustomField;
begin
  if Assigned(FInternalDataSet) then
    if not Assigned(Item.Field) then
    begin
      Field := FInternalDataSet.GetFieldIntfClass.Create;
      Field.InternalDataSet := FInternalDataSet;
      Item.Field := Field as IsmxField;
    end;
end;

procedure TsmxFieldList.DelField(Item: TsmxFieldItem);
var
  Field: TsmxCustomField;
begin
  if Assigned(FInternalDataSet) then
    if Assigned(Item.Field) then
    begin
      Field := TsmxCustomField(Item.Field.GetReference);
      Field.InternalDataSet := nil;
      Item.Field := nil;
    end;
end;}

{function TsmxFieldList.FindByField(const Field: IsmxField): TsmxFieldItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Field = Field then
    begin
      Result := Items[i];
      Break;
    end;
end;}

function TsmxFieldList.FindByName(const FieldName: String): TsmxFieldItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Assigned(Items[i].ItemObject) then
      if SysUtils.AnsiCompareText(Items[i].ItemObject.FieldName, FieldName) = 0 then
      begin
        Result := Items[i];
        Break;
      end;
end;

function TsmxFieldList.GetItem(Index: Integer): TsmxFieldItem;
begin
  Result := TsmxFieldItem(inherited Items[Index]);
end;

{function TsmxFieldList.IndexOfField(const Field: IsmxField): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].Field = Field then
    begin
      Result := i;
      Break;
    end;
end;}

procedure TsmxFieldList.SetItem(Index: Integer; Value: TsmxFieldItem);
begin
  inherited Items[Index] := Value;
end;

{function TsmxFieldList.GetSense(const FieldName: String): TsmxDataSense;
var
  Item: TsmxFieldItem;
begin
  Item := FindByName(FieldName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.FieldName := FieldName;
  end;
  Result := Item.FieldSense;
end;

procedure TsmxFieldList.SetSense(const FieldName: String; Value: TsmxDataSense);
var
  Item: TsmxFieldItem;
begin
  Item := FindByName(FieldName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.FieldName := FieldName;
  end;
  Item.FieldSense := Value;
end;}

{function TsmxFieldList.IndexOfFieldName(const FieldName: String): Integer;
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

{function TsmxFieldList.IndexOfFieldIndex(FieldIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].FieldIndex = FieldIndex then
    begin
      Result := i;
      Break;
    end;
end;}

{function TsmxFieldList.Insert(const FieldName: String): Integer;
var
  Item: TsmxFieldItem;
begin
  Result := IndexOfName(FieldName);
  if Result = -1 then
  begin
    Item := Add;
    Item.FieldName := FieldName;
    Result := Item.ItemIndex;
  end;
end;

function TsmxFieldList.Remove(const FieldName: String): Integer;
begin
  Result := IndexOfName(FieldName);
  if Result <> -1 then
    Delete(Result);
end;}

{ TsmxParamItem }

{constructor TsmxParamItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  AddParam;
end;

destructor TsmxParamItem.Destroy;
begin
  DelParam;
  inherited Destroy;
end;

procedure TsmxParamItem.AddParam;
var
  AParam: TsmxCustomParam;
begin
  if Assigned(Kit) then
    if Assigned(Kit.InternalDataSet) then
      if not Assigned(FParam) then
      begin
        AParam := Kit.InternalDataSet.GetParamIntfClass.Create;
        AParam.InternalDataSet := Kit.InternalDataSet;
        FParam := AParam as IsmxParam;
      end;
end;

procedure TsmxParamItem.DelParam;
var
  AParam: TsmxCustomParam;
begin
  if Assigned(Kit) then
    if Assigned(Kit.InternalDataSet) then
      if Assigned(FParam) then
      begin
        AParam := TsmxCustomParam(FParam.GetReference);
        AParam.InternalDataSet := nil;
        FParam := nil;
      end;
end;

procedure TsmxParamItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxParamItem then
  begin
    DataLocation := TsmxParamItem(Source).DataLocation;
    DataSense := TsmxParamItem(Source).DataSense;
    DataType := TsmxParamItem(Source).DataType;
    NumericScale := TsmxParamItem(Source).NumericScale;
    ParamName := TsmxParamItem(Source).ParamName;
    ParamType := TsmxParamItem(Source).ParamType;
    ParamValue := TsmxParamItem(Source).ParamValue;
    Precision := TsmxParamItem(Source).Precision;
    Size := TsmxParamItem(Source).Size;
  end else
    inherited Assign(Source);
end;

function TsmxParamItem.GetDataType: TsmxDataType;
begin
  if Assigned(FParam) then
    Result := FParam.DataType else
    Result := ftUnknown;
end;}

function TsmxParamItem.GetItemObject: TsmxCustomParam;
begin
  Result := TsmxCustomParam(inherited ItemObject);
end;

procedure TsmxParamItem.SetItemObject(Value: TsmxCustomParam);
begin
  inherited ItemObject := Value;
end;

function TsmxParamItem.GetKit: TsmxParamList;
begin
  Result := TsmxParamList(inherited Kit);
end;

{function TsmxParamItem.GetNumericScale: Integer;
begin
  if Assigned(FParam) then
    Result := FParam.NumericScale else
    Result := 0;
end;

function TsmxParamItem.GetDataLocation: TsmxDataLocation;
begin
  if Assigned(FParam) then
    Result := FParam.DataLocation else
    Result := dlAssigned;
end;

function TsmxParamItem.GetParamName: String;
begin
  if Assigned(FParam) then
    Result := FParam.ParamName else
    Result := '';
end;

function TsmxParamItem.GetParamType: TsmxParamType;
begin
  if Assigned(FParam) then
    Result := FParam.ParamType else
    Result := ptUnknown;
end;

function TsmxParamItem.GetParamValue: Variant;
begin
  if Assigned(FParam) then
    Result := FParam.Value else
    Result := Variants.Null;
end;

function TsmxParamItem.GetPrecision: Integer;
begin
  if Assigned(FParam) then
    Result := FParam.Precision else
    Result := 0;
end;

function TsmxParamItem.GetSize: Integer;
begin
  if Assigned(FParam) then
    Result := FParam.Size else
    Result := 0;
end;

procedure TsmxParamItem.SetDataType(Value: TsmxDataType);
begin
  if Assigned(FParam) then
    FParam.DataType := Value;
end;}

procedure TsmxParamItem.SetKit(Value: TsmxParamList);
begin
  //if Assigned(Kit) then
    //DelParam;
  inherited Kit := Value;
  //if Assigned(Kit) then
    //AddParam;
end;

{procedure TsmxParamItem.SetNumericScale(Value: Integer);
begin
  if Assigned(FParam) then
    FParam.NumericScale := Value;
end;

procedure TsmxParamItem.SetDataLocation(Value: TsmxDataLocation);
begin
  if Assigned(FParam) then
    FParam.DataLocation := Value;
end;

procedure TsmxParamItem.SetParamName(const Value: String);
begin
  if Assigned(FParam) then
    FParam.ParamName := Value;
end;

procedure TsmxParamItem.SetParamType(Value: TsmxParamType);
begin
  if Assigned(FParam) then
    FParam.ParamType := Value;
end;

procedure TsmxParamItem.SetParamValue(const Value: Variant);
begin
  if Assigned(FParam) then
    FParam.Value := Value;
end;

procedure TsmxParamItem.SetPrecision(Value: Integer);
begin
  if Assigned(FParam) then
    FParam.Precision := Value;
end;

procedure TsmxParamItem.SetSize(Value: Integer);
begin
  if Assigned(FParam) then
    FParam.Size := Value;
end;

function TsmxParamItem.GetParamIndex: Integer;
begin
  if Assigned(FParam) then
    Result := FParam.ParamIndex else
    Result := -1;
end;

procedure TsmxParamItem.SetParamIndex(Value: Integer);
begin
  if Assigned(FParam) then
    FParam.ParamIndex := Value;
end;

function TsmxParamItem.GetDataSense: TsmxDataSense;
begin
  if Assigned(FParam) then
    Result := FParam.DataSense else
    Result := dsGeneral;
end;

procedure TsmxParamItem.SetDataSense(Value: TsmxDataSense);
begin
  if Assigned(FParam) then
    FParam.DataSense := Value;
end;}

{ TsmxParamList }

function TsmxParamList.Add: TsmxParamItem;
begin
  Result := TsmxParamItem(inherited Add);
  //AddParam(Result);
end;

{procedure TsmxParamList.Delete(Index: Integer);
begin
  //DelParam(Items[Index]);
  inherited Delete(Index);
end;}

{procedure TsmxParamList.AddParam(Item: TsmxParamItem);
var
  Param: TsmxCustomParam;
begin
  if Assigned(FInternalDataSet) then
    if not Assigned(Item.Param) then
    begin
      Param := FInternalDataSet.GetParamIntfClass.Create;
      Param.InternalDataSet := FInternalDataSet;
      Item.Param := Param as IsmxParam;
    end;
end;

procedure TsmxParamList.DelParam(Item: TsmxParamItem);
var
  Param: TsmxCustomParam;
begin
  if Assigned(FInternalDataSet) then
    if Assigned(Item.Param) then
    begin
      Param := TsmxCustomParam(Item.Param.GetReference);
      Param.InternalDataSet := nil;
      Item.Param := nil;
    end;
end;}

function TsmxParamList.FindByName(const ParamName: String): TsmxParamItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Assigned(Items[i].ItemObject) then
      if SysUtils.AnsiCompareText(Items[i].ItemObject.ParamName, ParamName) = 0 then
      begin
        Result := Items[i];
        Break;
      end;
end;

function TsmxParamList.GetItem(Index: Integer): TsmxParamItem;
begin
  Result := TsmxParamItem(inherited Items[Index]);
end;

{function TsmxParamList.IndexOfParam(const Param: IsmxParam): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].Param = Param then
    begin
      Result := i;
      Break;
    end;
end;}

procedure TsmxParamList.SetItem(Index: Integer; Value: TsmxParamItem);
begin
  inherited Items[Index] := Value;
end;

{function TsmxParamList.GetLocation(const ParamName: String): TsmxDataLocation;
var
  Item: TsmxParamItem;
begin
  Item := FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.ParamName := ParamName;
  end;
  Result := Item.ParamLocation;
end;

procedure TsmxParamList.SetLocation(const ParamName: String; Value: TsmxDataLocation);
var
  Item: TsmxParamItem;
begin
  Item := FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := Add;
    Item.ParamName := ParamName;
  end;
  Item.ParamLocation := Value;
end;}

{function TsmxParamList.IndexOfParamName(const ParamName: String): Integer;
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

{procedure TsmxParamList.Insert(const ParamName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(ParamName);
  if CurIndex = -1 then
    Add.ParamName := ParamName;
end;

procedure TsmxParamList.Remove(const ParamName: String);
var
  CurIndex: Integer;
begin
  CurIndex := IndexOfName(ParamName);
  if CurIndex <> -1 then
    Delete(CurIndex);
end;}

{ TsmxCustomDatabase }

{constructor TsmxCustomDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(smxProcs.gDatabaseManagerIntf) then
    smxProcs.gDatabaseManagerIntf.InsertDatabase(Self as IsmxDatabase);
end;}

destructor TsmxCustomDatabase.Destroy;
begin
  SetDatabaseManager(nil);
  {if Assigned(smxProcs.gDatabaseManagerIntf) then
    smxProcs.gDatabaseManagerIntf.RemoveDatabase(Self as IsmxDatabase);}
  inherited Destroy;
end;

function TsmxCustomDatabase.GetDatabase: IsmxDatabase;
begin
  Result := Self as IsmxDatabase;
end;

function TsmxCustomDatabase.GetDatabaseManager: IsmxDatabaseManager;
begin
  Result := FDatabaseManagerIntf;
end;

procedure TsmxCustomDatabase.SetDatabaseManager(const Value: IsmxDatabaseManager);
begin
  if Assigned(FDatabaseManagerIntf) then
    FDatabaseManagerIntf.RemoveConnection(Self as IsmxConnection);
  FDatabaseManagerIntf := Value;
  if Assigned(FDatabaseManagerIntf) then
    FDatabaseManagerIntf.InsertConnection(Self as IsmxConnection);
end;

function TsmxCustomDatabase.GetDatabaseName: String;
begin
  Result := '';
end;

procedure TsmxCustomDatabase.SetDatabaseName(const Value: String);
begin
end;

{ TsmxCustomField }

(*constructor TsmxCustomField.Create({AOwner: TComponent;} InternalDataSet: TsmxCustomDataSet);
begin
  //inherited Create(AOwner);
  FInternalDataSet := InternalDataSet;
  // так нельзя, будет зацикливание ссылок
  //if Assigned(FInternalDataSet) then
    //FDataSetIntf := FInternalDataSet as IsmxDataSet;
end;*)

destructor TsmxCustomField.Destroy;
begin
  //FDataSetIntf := nil;
  //SetInternalRef(nil);
  SetInternalDataSet(nil);
  if Assigned(FFieldItem) then
  begin
    //FFieldItem.InitializeItemObject(nil);
    FFieldItem.FItemObject := nil;
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
  //if Assigned(FInternalDataSet) then
    //FInternalDataSet.DestroyInternalField(Self);
  //if Value is TsmxCustomDataSet then

    //FInternalDataSet := TsmxCustomDataSet(Value);
  FInternalDataSet := Value as TsmxCustomDataSet;

  //if Assigned(FInternalDataSet) then
    //FInternalDataSet.CreateInternalField(Self);
end;

function TsmxCustomField.GetFieldItem: TsmxFieldItem;
begin
  if not Assigned(FFieldItem) then
  begin
    FFieldItem := TsmxFieldItem.Create(nil);
    //FFieldItem.InitializeItemObject(Self);
    FFieldItem.FItemObject := Self;
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
    Result := {FDataSetIntf; //}FInternalDataSet as IsmxDataSet
  else
    Result := nil;
  //if SysUtils.Support(
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
  {if Assigned(FInternalDataSet) then
    FInternalDataSet.DestroyInternalField(Self);
  FInternalDataSet := Value;
  if Assigned(FInternalDataSet) then
    FInternalDataSet.CreateInternalField(Self);}
end;

{procedure TsmxCustomField.SetIntfDataSet(Value: TsmxCustomDataSet);
begin
  FIntfDataSet := Value;
end;}

{function TsmxCustomField.GetInternalRef: Pointer;
begin
  Result := FInternalRef;
end;}

{procedure TsmxCustomField.SetInternalRef(Value: Pointer);
begin
  //if Assigned(FInternalRef) then
    //FInternalDataSet.FieldReferenceList.RemoveReference(Self, FInternalRef);
  FInternalRef := Value;
  //if Assigned(FInternalRef) then
    //FInternalDataSet.FieldReferenceList.InsertReference(Self, FInternalRef);
end;}

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
  //Result := FFieldIndex;
  {if Assigned(FInternalDataSet) then
    Result := FInternalDataSet.FieldList.IndexOfField(Self as IsmxField)
  else
    Result := FFieldIndex;}
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
  {if Assigned(FInternalDataSet) then
    FInternalDataSet.FieldList[
      FInternalDataSet.FieldList.IndexOfField(Self as IsmxField)].ItemIndex := Value
  else
    FFieldIndex := Value;}
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

{destructor TsmxField.Destroy;
begin
  if Assigned(FField) then
    FField.Free;
  inherited Destroy;
end;}

procedure TsmxField.AssignField(const Source: IsmxField);
begin
  inherited AssignField(Source);
  if Assigned(Source) then
  begin
    //DataType := Source.DataType;
    //DisplayFormat := Source.DisplayFormat;
    //FieldName := Source.FieldName;
    //Precision := Source.Precision;
    //Size := Source.Size;
    Value := Source.Value;
  end;
  //else
    //Clear;
end;

procedure TsmxField.ChangeObjectIndex(Value: Integer);
begin
  inherited ChangeObjectIndex(Value);
  if Assigned(FField) then
    FField.Index := Value;
end;

procedure TsmxField.ChangeObjectOwner(Value: TPersistent);
{var
  ADisplayFormat: String;
  AFieldName: String;
  APrecision: Integer;
  ASize: Integer;
  AValue: Variant;}
begin
  //if Assigned(InternalDataSet) then
  if InternalDataSet is TsmxDataSet then
  begin
    {ADisplayFormat := GetDisplayFormat;
    AFieldName := GetFieldName;
    APrecision := GetPrecision;
    ASize := GetSize;
    AValue := GetValue;}
    //(InternalDataSet as IsmxObjectList).DestroyObject(FieldItem);
    TsmxDataSet(InternalDataSet).DestroyInternalField(Self);
  end;
  inherited ChangeObjectOwner(Value);
  //if Assigned(InternalDataSet) then
  if InternalDataSet is TsmxDataSet then
  begin
    //(InternalDataSet as IsmxObjectList).CreateObject(FieldItem);
    TsmxDataSet(InternalDataSet).CreateInternalField(Self);
    SetDisplayFormat(GetDisplayFormat);
    //SetFieldIndex(GetFieldIndex);
    SetFieldName(GetFieldName{FFieldName});
    SetPrecision(GetPrecision);
    SetSize(GetSize);
    //SetValue(FValue);
  end;
end;

procedure TsmxField.Clear;
begin
  inherited Clear;
  //if Assigned(Field) then
    //Field.Clear;
  //DataType := ftUnknown;
  //DisplayFormat := '';
  //FieldName := '';
  //Precision := 0;
  //Size := 0;
  Value := Variants.Null;
  //if Assigned(Field) then
    //Field.Clear;
end;

{function TsmxField.GetDataSet: IsmxDataSet;
begin
  if Assigned(IntfDataSet) then
    Result := IntfDataSet as IsmxDataSet else
    Result := nil;
end;}

//function TsmxField.GetDataType: TsmxDataType;
//begin
  {if Assigned(FField) then
    Result := FField.DataType else
    Result := ftUnknown;}
  //Result := FDataType;
//end;

procedure TsmxField.SetDataType(Value: TsmxDataType);
//var
  //AField: TField;
  //ADisplayFormat: String;
  //AFieldName: String;
  //APrecision: Integer;
  //ASize: Integer;
  //AValue: Variant;
begin
  if GetDataType <> Value then
  begin
    //if Assigned(FField) then
    //begin
      //ADisplayFormat := GetDisplayFormat;
      //AFieldName := GetFieldName;
      //APrecision := GetPrecision;
      //ASize := GetSize;
      //AValue := GetValue;
    //end;
    //AField := FField;
    //if Assigned(InternalDataSet) then
      //(InternalDataSet as IsmxObjectList).DestroyObject(FieldItem);
    if InternalDataSet is TsmxDataSet then
      TsmxDataSet(InternalDataSet).DestroyInternalField(Self);
    //FDataType := Value;
    inherited SetDataType(Value);
    //if Assigned(InternalDataSet) then
    if InternalDataSet is TsmxDataSet then
    begin
      //(InternalDataSet as IsmxObjectList).CreateObject(FieldItem);
      TsmxDataSet(InternalDataSet).CreateInternalField(Self);

      SetDisplayFormat(GetDisplayFormat);
      //SetFieldIndex(GetFieldIndex);
      SetFieldName(GetFieldName{FFieldName});
      SetPrecision(GetPrecision);
      SetSize(GetSize);
      //SetValue(FValue);
    end;
    //if Assigned(FField) then
    //begin
      //SetDisplayFormat(ADisplayFormat);
      //SetFieldName(AFieldName);
      //SetPrecision(APrecision);
      //SetSize(ASize);
      //SetValue(AValue);
    //end;

    //if Assigned(InternalDataSet) then
      //InternalDataSet.DestroyInternalField(Self);
    //inherited SetDataType(Value);
    {if Assigned(InternalDataSet) then
    begin
      InternalDataSet.CreateInternalField(Self);
      if Assigned(FField) then
      begin
        ChangeDisplayFormat(DisplayFormat);
        //FField.Index := FieldIndex;
        ChangeFieldName(FieldName);
        ChangePrecision(Precision);
        FField.Size := Size;
      end;
    end;}

    {if Assigned(FField) then
    begin
      FField.Free;
      FField := nil;
    end;
    if (Value <> ftUnknown) and Assigned(InternalDataSet) then
    begin
      FField := CreateField(Value);
      if Assigned(FField) then
      begin
        InstallDisplayFormat(DisplayFormat);
        FField.Index := FieldIndex;
        FField.FieldName := FieldName;
        InstallPrecision(Precision);
        FField.Size := Size;
      end;
    end;}
  end;
end;

//function TsmxField.GetDisplayFormat: String;
//begin
  {if FField is TAggregateField then
    Result := TAggregateField(FField).DisplayFormat else
  if FField is TDateTimeField then
    Result := TDateTimeField(FField).DisplayFormat else
  if FField is TNumericField then
    Result := TNumericField(FField).DisplayFormat else
  if FField is TSQLTimeStampField then
    Result := TSQLTimeStampField(FField).DisplayFormat else}
    //Result := FDisplayFormat;
//end;

procedure TsmxField.SetDisplayFormat(const Value: String);
begin
  inherited SetDisplayFormat(Value);
  //if Assigned(FField) then
    //ChangeDisplayFormat(Value);
  //FDisplayFormat := Value;
  if FField is TAggregateField then
    TAggregateField(FField).DisplayFormat := Value else
  if FField is TDateTimeField then
    TDateTimeField(FField).DisplayFormat := Value else
  if FField is TNumericField then
    TNumericField(FField).DisplayFormat := Value else
  if FField is TSQLTimeStampField then
    TSQLTimeStampField(FField).DisplayFormat := Value {else
    FDisplayFormat := Value};
end;

{function TsmxField.GetField: TField;
begin
  if TObject(InternalRef) is TField then
    Result := TField(InternalRef) else
    Result := nil;
end;}

{function TsmxField.GetFieldIndex: Integer;
begin
  if Assigned(Field) then
    Result := Field.Index
  else
    Result := inherited GetFieldIndex;
end;}

{procedure TsmxField.SetFieldIndex(Value: Integer);
begin
  inherited SetFieldIndex(Value);
  if Assigned(Field) then
    Field.Index := Value;
end;}

//function TsmxField.GetFieldName: String;
//begin
  //Result := inherited GetFieldName;
  {if Assigned(FField) then
    Result := FField.FieldName else}
    //Result := FFieldName;
//end;

procedure TsmxField.SetFieldName(const Value: String);
begin
  inherited SetFieldName(Value);
  //if Assigned(FField) then
    //ChangeFieldName(Value);
  //FFieldName := Value;
  if Assigned(FField) and (Value <> '') then
    FField.FieldName := Value;
end;

//function TsmxField.GetPrecision: Integer;
//begin
  {if FField is TBCDField then
    Result := TBCDField(FField).Precision else
  if FField is TFMTBCDField then
    Result := TFMTBCDField(FField).Precision else}
    //Result := FPrecision;
//end;

procedure TsmxField.SetPrecision(Value: Integer);
begin
  inherited SetPrecision(Value);
  //if Assigned(FField) then
    //ChangePrecision(Value);
  //FPrecision := Value;
  if FField is TBCDField then
    TBCDField(FField).Precision := Value else
  if FField is TFMTBCDField then
    TFMTBCDField(FField).Precision := Value {else
    FPrecision := Value};
end;

//function TsmxField.GetSize: Integer;
//begin
  {if Assigned(FField) then
    Result := FField.Size else}
    //Result := FSize;
//end;

procedure TsmxField.SetSize(Value: Integer);
begin
  inherited SetSize(Value);
  //FSize := Value;
  if Assigned(FField) then
    FField.Size := Value{ else
    FSize := Value};
end;

function TsmxField.GetValue: Variant;
begin
  if Assigned(FField) then
    Result := FField.Value else
    Result := {FValue;} Variants.Null;
end;

procedure TsmxField.SetValue(const Value: Variant);
begin
  //FValue := Value;
  if Assigned(FField) then
    FField.Value := Value {else
    FValue := Value};
end;

{function TsmxField.GetDataSense: TsmxDataSense;
begin
  if Assigned(FField) and Assigned(IntfDataSet) then
    Result := IntfDataSet.SenseList.Senses[FField.FieldName] else
    Result := fsGeneral;
end;

procedure TsmxField.SetDataSense(Value: TsmxDataSense);
begin
  if Assigned(FField) and Assigned(IntfDataSet) then
    IntfDataSet.SenseList.Senses[FField.FieldName] := Value;
end;}

function TsmxField.GetInternalRef: Pointer;
begin
  Result := Pointer(FField);
end;

{procedure TsmxField.SetInternalRef(Value: Pointer);
begin
  inherited SetInternalRef(Value);
  if TObject(Value) is TField then
    FField := TField(Value) else
    FField := nil;
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

{procedure TsmxField.ChangeDisplayFormat(const Format: String);
begin
  if FField is TAggregateField then
    TAggregateField(FField).DisplayFormat := Format else
  if FField is TDateTimeField then
    TDateTimeField(FField).DisplayFormat := Format else
  if FField is TNumericField then
    TNumericField(FField).DisplayFormat := Format else
  if FField is TSQLTimeStampField then
    TSQLTimeStampField(FField).DisplayFormat := Format;
end;

procedure TsmxField.ChangeFieldName(const Name: String);
begin
  if Assigned(FField) and (Name <> '') then
    FField.FieldName := Name;
end;

procedure TsmxField.ChangePrecision(Accuracy: Integer);
begin
  if FField is TBCDField then
    TBCDField(FField).Precision := Accuracy else
  if FField is TFMTBCDField then
    TFMTBCDField(FField).Precision := Accuracy;
end;}

{function TsmxField.CreateField(DataType: TsmxDataType): TField;
var
  FieldDef: TFieldDef;
  Field: TField;
begin
  Result := nil;
  if Assigned(InternalDataSet) then
    if TObject(InternalDataSet.GetInternalRef) is TDataSet then
    begin
      FieldDef := TDataSet(InternalDataSet.GetInternalRef).FieldDefs.AddFieldDef;
      FieldDef.DataType := DataType;
      Field := FieldDef.CreateField(TDataSet(InternalDataSet.GetInternalRef));
      Result := Field;
    end;
end;}

{ TsmxCustomParam }

(*constructor TsmxCustomParam.Create(InternalDataSet: TsmxCustomDataSet);
begin
  FInternalDataSet := InternalDataSet;
  {if Assigned(FInternalDataSet) then
    FDataSetIntf := FInternalDataSet as IsmxDataSet;}
end;*)

destructor TsmxCustomParam.Destroy;
begin
  //FDataSetIntf := nil;
  //SetInternalRef(nil);
  SetInternalDataSet(nil);
  if Assigned(FParamItem) then
  begin
    //FFieldItem.InitializeItemObject(nil);
    FParamItem.FItemObject := nil;
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
    Result := {FDataSetIntf; //}FInternalDataSet as IsmxDataSet
  else
    Result := nil;
end;

{function TsmxCustomParam.GetDataLocation: TsmxDataLocation;
var
  Item: TsmxParamItem;
begin
  Item := FInternalDataSet.LocationList.FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := FInternalDataSet.LocationList.Add;
    Item.ParamName := ParamName;
  end;
  Result := Item.ParamLocation;
end;}

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
  //if Value is TsmxCustomDataSet then
    //FInternalDataSet := TsmxCustomDataSet(Value);
  FInternalDataSet := Value as TsmxCustomDataSet;
end;

function TsmxCustomParam.GetParamItem: TsmxParamItem;
begin
  if not Assigned(FParamItem) then
  begin
    FParamItem := TsmxParamItem.Create(nil);
    //FFieldItem.InitializeItemObject(Self);
    FParamItem.FItemObject := Self;
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
  {if Assigned(FInternalDataSet) then
    Result := FInternalDataSet.ParamList.IndexOfParam(Self as IsmxParam)
  else
    Result := FParamIndex;}
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
  Result := FValue; // может перенести в наследников?
end;

procedure TsmxCustomParam.SetParamIndex(Value: Integer);
begin
  {if Assigned(FInternalDataSet) then
    FInternalDataSet.ParamList[
      FInternalDataSet.ParamList.IndexOfParam(Self as IsmxParam)].ItemIndex := Value
  else
    FParamIndex := Value;}
  ParamItem.ItemIndex := Value;
end;

procedure TsmxCustomParam.SetDataLocation(Value: TsmxDataLocation);
//var
  //Item: TsmxParamItem;
begin
  {Item := FInternalDataSet.LocationList.FindByName(ParamName);
  if not Assigned(Item) then
  begin
    Item := FInternalDataSet.LocationList.Add;
    Item.ParamName := ParamName;
  end;
  Item.ParamLocation := Value;}
  FDataLocation := Value;
end;

function TsmxCustomParam.GetParamName: String;
begin
  Result := FParamName;
end;

procedure TsmxCustomParam.SetParamName(const Value: String);
//var
  //Item: TsmxParamItem;
begin
  {Item := FInternalDataSet.LocationList.FindByName(ParamName);
  if not Assigned(Item) then
    Item := FInternalDataSet.LocationList.Add;
  Item.ParamName := Value;}
  //FParamName := Value;
  FParamName := Value;
end;

procedure TsmxCustomParam.SetDataType(Value: TsmxDataType);
begin
  FDataType := Value;
end;

procedure TsmxCustomParam.SetInternalDataSet(Value: TsmxCustomDataSet);
begin
  {if Assigned(FInternalDataSet) then
    FInternalDataSet.DestroyInternalParam(Self);
  FInternalDataSet := Value;
  if Assigned(FInternalDataSet) then
    FInternalDataSet.CreateInternalParam(Self);}
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

{function TsmxCustomParam.GetInternalRef: Pointer;
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
end;}

{ TsmxCustomDataSet }

destructor TsmxCustomDataSet.Destroy;
begin
  SetDatabase(nil);
  if Assigned(FFieldList) then
    FFieldList.Free;
  if Assigned(FParamList) then
    FParamList.Free;
  {if Assigned(FFieldIntfList) then
    FFieldIntfList.Free;
  if Assigned(FParamIntfList) then
    FParamIntfList.Free;}
  {if Assigned(FFieldReferenceList) then
  begin
    ClearFieldReferences;
    FFieldReferenceList.Free;
  end;
  if Assigned(FParamReferenceList) then
  begin
    ClearParamReferences;
    FParamReferenceList.Free;
  end;}
  inherited Destroy;
end;

function TsmxCustomDataSet.AddField: IsmxField;
begin
  {Result := GetFieldIntfClass.Create(Self) as IsmxField;
  FieldIntfList.Add(Result);}
  //Result := FieldList.Add.Field;
  Result := FieldList.Add.ItemObject as IsmxField;
end;

function TsmxCustomDataSet.AddParam: IsmxParam;
begin
  {Result := GetParamIntfClass.Create(Self) as IsmxParam;
  ParamIntfList.Add(Result);}
  //Result := ParamList.Add.Param;
  Result := ParamList.Add.ItemObject as IsmxParam;
end;

{function TsmxCustomDataSet.AddField(const FieldName: String): IsmxField;
var
  Item: TsmxFieldItem;
begin
  Result := nil;
  Item := SenseList.FindByName(FieldName);
  if not Assigned(Item) then
    SenseList.Add.FieldName := FieldName;
end;}

procedure TsmxCustomDataSet.ClearFields;
begin
  //FieldIntfList.Clear;
  FieldList.Clear;
  //ClearFieldReferences;
end;

procedure TsmxCustomDataSet.ClearParams;
begin
  //ParamIntfList.Clear;
  ParamList.Clear;
  //ClearParamReferences;
end;

procedure TsmxCustomDataSet.DeleteField(const Field: IsmxField);
//var
  //Item: TsmxFieldItem;
  //i: Integer;
begin
  if Assigned(Field) then
    FieldList.Delete(Field.FieldIndex);
  //i := FieldList.IndexOfField(Field);
  //if i <> -1 then
    //FieldList.Delete(i);
  {i := FieldIntfList.IndexOf(Field);
  if i <> -1 then
  begin
    FieldIntfList.Delete(i);
    FieldList.Delete(i);
  end;}
  {if Assigned(Field) then
  begin
    Item := SenseList.FindByName(Field.FieldName);
    if Assigned(Item) then
      SenseList.Delete(Item.ItemIndex);
    ResetFieldReference(Field.InternalRef);
  end;}
end;

{function TsmxCustomDataSet.AddParam(const ParamName: String): IsmxParam;
var
  Item: TsmxParamItem;
begin
  Result := nil;
  Item := LocationList.FindByName(ParamName);
  if not Assigned(Item) then
    LocationList.Add.ParamName := ParamName;
end;}

procedure TsmxCustomDataSet.DeleteParam(const Param: IsmxParam);
//var
  //Item: TsmxParamItem;
  //i: Integer;
begin
  if Assigned(Param) then
    ParamList.Delete(Param.ParamIndex);
  //i := ParamList.IndexOfParam(Param);
  //if i <> -1 then
    //ParamList.Delete(i);
  {i := ParamIntfList.IndexOf(Param);
  if i <> -1 then
  begin
    ParamIntfList.Delete(i);
    ParamList.Delete(i);
  end;}
  {if Assigned(Param) then
  begin
    Item := LocationList.FindByName(Param.ParamName);
    if Assigned(Item) then
      LocationList.Delete(Item.ItemIndex);
    ResetParamReference(Param.InternalRef);
  end;}
end;

{procedure TsmxCustomDataSet.ClearFieldReferences;
begin
  while FieldReferenceList.Count > 0 do
    TsmxCustomField(FieldReferenceList[FieldReferenceList.Count - 1].Owner).InternalRef := nil;
end;}

{procedure TsmxCustomDataSet.ClearParamReferences;
begin
  while ParamReferenceList.Count > 0 do
    TsmxCustomParam(ParamReferenceList[ParamReferenceList.Count - 1].Owner).InternalRef := nil;
end;}

{function TsmxCustomDataSet.GetFieldReferenceList: TsmxReferenceList;
begin
  if not Assigned(FFieldReferenceList) then
    FFieldReferenceList := TsmxReferenceList.Create(TsmxReferenceItem);
  Result := FFieldReferenceList;
end;

procedure TsmxCustomDataSet.SetFieldReferenceList(Value: TsmxReferenceList);
begin
  FieldReferenceList.Assign(Value);
end;}

function TsmxCustomDataSet.GetParamList: TsmxParamList;
begin
  if not Assigned(FParamList) then
  //begin
    FParamList := TsmxParamList.Create(Self, TsmxParamItem);
    //FParamList.InternalDataSet := Self;
  //end;
  Result := FParamList;
end;

procedure TsmxCustomDataSet.SetParamList(Value: TsmxParamList);
begin
  ParamList.Assign(Value);
end;

{function TsmxCustomDataSet.GetParamReferenceList: TsmxReferenceList;
begin
  if not Assigned(FParamReferenceList) then
    FParamReferenceList := TsmxReferenceList.Create(TsmxReferenceItem);
  Result := FParamReferenceList;
end;

procedure TsmxCustomDataSet.SetParamReferenceList(Value: TsmxReferenceList);
begin
  ParamReferenceList.Assign(Value);
end;}

function TsmxCustomDataSet.GetFieldList: TsmxFieldList;
begin
  if not Assigned(FFieldList) then
  //begin
    FFieldList := TsmxFieldList.Create(Self, TsmxFieldItem);
    //FFieldList.InternalDataSet := Self;
  //end;
  Result := FFieldList;
end;

procedure TsmxCustomDataSet.SetFieldList(Value: TsmxFieldList);
begin
  FieldList.Assign(Value);
end;

{procedure TsmxCustomDataSet.ResetFieldReference(Reference: Pointer);
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
end;}

{function TsmxCustomDataSet.GetFieldIntfList: TInterfaceList;
begin
  if not Assigned(FFieldIntfList) then
    FFieldIntfList := TInterfaceList.Create;
  Result := FFieldIntfList;
end;}

{function TsmxCustomDataSet.GetParamIntfList: TInterfaceList;
begin
  if not Assigned(FParamIntfList) then
    FParamIntfList := TInterfaceList.Create;
  Result := FParamIntfList;
end;}

function TsmxCustomDataSet.GetFieldClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxCustomField;
end;

function TsmxCustomDataSet.GetParamClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxCustomParam;
end;

{function TsmxCustomDataSet.CreateField(DataType: TsmxDataType): TObject;
begin
  Result := nil;
end;

function TsmxCustomDataSet.CreateParam(DataType: TsmxDataType): TObject;
begin
  Result := nil;
end;}

{procedure TsmxCustomDataSet.CreateInternalField(Field: TsmxCustomField);
begin
end;

procedure TsmxCustomDataSet.CreateInternalParam(Param: TsmxCustomParam);
begin
end;

procedure TsmxCustomDataSet.DestroyInternalField(Field: TsmxCustomField);
begin
end;

procedure TsmxCustomDataSet.DestroyInternalParam(Param: TsmxCustomParam);
begin
end;}

procedure TsmxCustomDataSet.CreateObject(Item: TObject);
//var
  //Field: TsmxCustomField;
  //Param: TsmxCustomParam;
begin
  if Item is TsmxFieldItem then
  begin
    {if not Assigned(TsmxFieldItem(Item).FItemObject) then
    begin
      Field := TsmxCustomField(GetFieldClass.Create(Self as IsmxBaseInterface));
      //Field.FInternalDataSet := Self;
      Field.FFieldItem := TsmxFieldItem(Item);
      TsmxFieldItem(Item).FItemObject := Field;
      Field.ChangeObjectOwner(Self);
    end;}
    CreateObject(Item, GetFieldClass);
  end else
  if Item is TsmxParamItem then
  begin
    {if not Assigned(TsmxParamItem(Item).FItemObject) then
    begin
      Param := TsmxCustomParam(GetParamClass.Create(Self as IsmxBaseInterface));
      //Param.FInternalDataSet := Self;
      Param.FParamItem := TsmxParamItem(Item);
      TsmxParamItem(Item).FItemObject := Param;
      Param.ChangeObjectOwner(Self);
    end;}
    CreateObject(Item, GetParamClass);
  end;
end;

procedure TsmxCustomDataSet.CreateObject(Item: TObject; ObjectClass: TPersistentClass);
var
  Field: TsmxCustomField;
  Param: TsmxCustomParam;
begin
  CheckObjectClass(Item, ObjectClass);
  if Item is TsmxFieldItem then
  begin
    if not Assigned(TsmxFieldItem(Item).FItemObject) then
    begin
      Field := TsmxCustomField(TsmxInterfacedPersistentClass(ObjectClass).Create(Self as IsmxBaseInterface));
      //Field.FInternalDataSet := Self;
      Field.FFieldItem := TsmxFieldItem(Item);
      TsmxFieldItem(Item).FItemObject := Field;
      Field.ChangeObjectOwner(Self);
    end;
  end else
  if Item is TsmxParamItem then
  begin
    if not Assigned(TsmxParamItem(Item).FItemObject) then
    begin
      Param := TsmxCustomParam(TsmxInterfacedPersistentClass(ObjectClass).Create(Self as IsmxBaseInterface));
      //Param.FInternalDataSet := Self;
      Param.FParamItem := TsmxParamItem(Item);
      TsmxParamItem(Item).FItemObject := Param;
      Param.ChangeObjectOwner(Self);
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
    if Assigned(TsmxFieldItem(Item).FItemObject) then
    begin
      Field := TsmxCustomField(TsmxFieldItem(Item).FItemObject);
      TsmxFieldItem(Item).FItemObject := nil;
      Field.FFieldItem := nil;
      Field.Free;
    end;
  end else
  if Item is TsmxParamItem then
  begin
    if Assigned(TsmxParamItem(Item).FItemObject) then
    begin
      Param := TsmxCustomParam(TsmxParamItem(Item).FItemObject);
      TsmxParamItem(Item).FItemObject := nil;
      Param.FParamItem := nil;
      Param.Free;
    end;
  end;
end;

function TsmxCustomDataSet.FieldByName(const FieldName: String): IsmxField;
var
  //i: Integer;
  FieldItem: TsmxFieldItem;
begin
  FieldItem := FieldList.FindByName(FieldName);
  if Assigned(FieldItem) then
    Result := FieldItem.ItemObject as IsmxField
  else
    raise EsmxListError.CreateFmt(smxConsts.rsListItemNotFound, [FieldName]);
  {i := FieldList.IndexOfFieldName(FieldName);
  if i <> -1 then
    Result := FieldList[i].Field
  else
    raise EsmxListError.CreateFmt(smxConsts.rsListItemNotFound, [FieldName]);}
end;

function TsmxCustomDataSet.FindField(const FieldName: String): IsmxField;
var
  //i: Integer;
  FieldItem: TsmxFieldItem;
begin
  FieldItem := FieldList.FindByName(FieldName);
  if Assigned(FieldItem) then
    Result := FieldItem.ItemObject as IsmxField
  else
    Result := nil;

  {i := FieldList.IndexOfFieldName(FieldName);
  if i <> -1 then
    Result := FieldList[i].Field else
    Result := nil;}
end;

function TsmxCustomDataSet.FindParam(const ParamName: String): IsmxParam;
var
  //i: Integer;
  ParamItem: TsmxParamItem;
begin
  ParamItem := ParamList.FindByName(ParamName);
  if Assigned(ParamItem) then
    Result := ParamItem.ItemObject as IsmxParam
  else
    Result := nil;
  {i := ParamList.IndexOfParamName(ParamName);
  if i <> -1 then
    Result := ParamList[i].Param else
    Result := nil;}
end;

function TsmxCustomDataSet.GetField(Index: Integer): IsmxField;
begin
  Result := FieldList[Index].ItemObject as IsmxField;
end;

function TsmxCustomDataSet.GetFieldCount: Integer;
begin
  Result := FieldList.Count;
end;

function TsmxCustomDataSet.GetParam(Index: Integer): IsmxParam;
begin
  Result := ParamList[Index].ItemObject as IsmxParam;
end;

function TsmxCustomDataSet.GetParamCount: Integer;
begin
  Result := ParamList.Count;
end;

function TsmxCustomDataSet.ParamByName(const ParamName: String): IsmxParam;
var
  //i: Integer;
  ParamItem: TsmxParamItem;
begin
  ParamItem := ParamList.FindByName(ParamName);
  if Assigned(ParamItem) then
    Result := ParamItem.ItemObject as IsmxParam
  else
    raise EsmxListError.CreateFmt(smxConsts.rsListItemNotFound, [ParamName]);

  {i := ParamList.IndexOfParamName(ParamName);
  if i <> -1 then
    Result := ParamList[i].Param
  else
    raise EsmxListError.CreateFmt(smxConsts.rsListItemNotFound, [ParamName]);}
end;

procedure TsmxCustomDataSet.SetField(Index: Integer; const Value: IsmxField);
begin
  (FieldList[Index].ItemObject as IsmxField).AssignField(Value);
end;

procedure TsmxCustomDataSet.SetParam(Index: Integer; const Value: IsmxParam);
begin
  (ParamList[Index].ItemObject as IsmxParam).AssignParam(Value);
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

procedure TsmxCustomDataSet.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxCustomDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

{ TsmxDataSet }

{destructor TsmxDataSet.Destroy;
begin
  //FDatabaseIntf := nil;

  //if Assigned(FInternalDataSet) then
    //FInternalDataSet.Free;

  //if Assigned(FFieldIntfList) then
  //begin
    //ClearFieldRef;
    //FFieldIntfList.Free;
  //end;
  //if Assigned(FParamIntfList) then
  //begin
    //ClearParamRef;
    //FParamIntfList.Free;
  //end;
  Close;
  inherited Destroy;
end;}

procedure TsmxDataSet.Add;
begin
  //TDataSet(GetInternalRef).Append;
  if Assigned(InternalDataSet) then
    InternalDataSet.Append;
end;

procedure TsmxDataSet.Delete;
begin
  //TDataSet(GetInternalRef).Delete;
  if Assigned(InternalDataSet) then
    InternalDataSet.Delete;
end;

//function TsmxDataSet.AddField{(DataType: TsmxDataType)}: IsmxField;
//var
  //AField: TField;
  //AFieldDef: TFieldDef;
//begin
  //inherited AddField(FieldName);
  {if not TDataSet(GetInternalRef).FieldDefs.Updated then
    TDataSet(GetInternalRef).FieldDefs.Update;
  AField := TDataSet(GetInternalRef).FieldDefs.Find(FieldName).CreateField(TDataSet(GetInternalRef));}
  {if not TDataSet(GetInternalRef).FieldDefs.Updated then
    TDataSet(GetInternalRef).FieldDefs.Update;
  AField := TDataSet(GetInternalRef).FieldDefList.FieldByName(FieldName).CreateField(TDataSet(GetInternalRef));}



  //AFieldDef := TDataSet(GetInternalRef).FieldDefs.AddFieldDef;
  //AFieldDef.DataType := DataType;
  //AField := AFieldDef.CreateField(TDataSet(GetInternalRef));
  //Result := GetFieldIntf(AField);



  //inherited AddField;
  //Result := GetFieldIntf;
  //FieldIntfList.Add(Result);
//end;

(*procedure TsmxDataSet.DeleteField(const Field: IsmxField);
//var
  //AField: TField;
  //Item: TsmxFieldItem;
begin
  inherited DeleteField(Field);
  //FieldIntfList.Delete(Field.FieldIndex);
  TDataSet(GetInternalRef).Fields.Remove(TField(Field.InternalRef));
  {if Assigned(Field) then
  begin
    AField := TDataSet(GetInternalRef).Fields.FindField(Field.FieldName);
    if Assigned(AField) then
    //begin
      TDataSet(GetInternalRef).Fields.Remove(AField);
      //Item := SenseList.FindByName(Field.FieldName);
      //if Assigned(Item) then
        //SenseList.Delete(Item.ItemIndex);
    //end;
  end;}
end;*)

function TsmxDataSet.Bof: Boolean;
begin
  //Result := TDataSet(GetInternalRef).Bof;
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Bof else
    Result := False;
end;

function TsmxDataSet.Eof: Boolean;
begin
  //Result := TDataSet(GetInternalRef).Eof;
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Eof else
    Result := False;
end;

procedure TsmxDataSet.Cancel;
begin
  //TDataSet(GetInternalRef).Cancel;
  if Assigned(InternalDataSet) then
    InternalDataSet.Cancel;
end;

procedure TsmxDataSet.Post;
begin
  //TDataSet(GetInternalRef).Post;
  if Assigned(InternalDataSet) then
    InternalDataSet.Post;
end;

{procedure TsmxDataSet.ClearFields;
begin
  inherited ClearFields;
  //FieldIntfList.Clear;
  if TDataSet(GetInternalRef).Active then
    TDataSet(GetInternalRef).ClearFields;
  //SenseList.Clear;
end;}

procedure TsmxDataSet.Close;
begin
  //TDataSet(GetInternalRef).Close;
  if Assigned(InternalDataSet) then
    InternalDataSet.Close;
end;

procedure TsmxDataSet.Open;
begin
  //TDataSet(GetInternalRef).Open;
  if Assigned(InternalDataSet) then
    InternalDataSet.Open;
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
  //TDataSet(GetInternalRef).Edit;
  if Assigned(InternalDataSet) then
    InternalDataSet.Edit;
end;

{function TsmxDataSet.FieldByName(const FieldName: String): IsmxField;
var
  //Field: TField;
  i: Integer;
begin
  i := FieldList.IndexOfFieldName(FieldName);
  if i <> -1 then
    Result := FieldList[i].Field;
  //Field := TDataSet(GetInternalRef).Fields.FieldByName(FieldName);
  //Result := GetFieldIntf(Field);
end;

function TsmxDataSet.FindField(const FieldName: String): IsmxField;
var
  Field: TField;
begin
  Field := TDataSet(GetInternalRef).Fields.FindField(FieldName);
  if Assigned(Field) then
    Result := GetFieldIntf(Field) else
    Result := nil;
end;}

procedure TsmxDataSet.First;
begin
  //TDataSet(GetInternalRef).First;
  if Assigned(InternalDataSet) then
    InternalDataSet.First;
end;

procedure TsmxDataSet.Last;
begin
  //TDataSet(GetInternalRef).Last;
  if Assigned(InternalDataSet) then
    InternalDataSet.Last;
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
  //Result := TDataSet(GetInternalRef).Active;
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Active else
    Result := False;
end;

procedure TsmxDataSet.SetActive(Value: Boolean);
begin
  //TDataSet(GetInternalRef).Active := Value;
  if Assigned(InternalDataSet) then
    InternalDataSet.Active := Value;
end;

{function TsmxDataSet.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxDataSet.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;}

{function TsmxDataSet.GetField(Index: Integer): IsmxField;
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
end;}

{function TsmxDataSet.GetFieldIntf(Field: TField): IsmxField;
begin
  Result := TsmxField.Create(Self) as IsmxField;
  Result.InternalRef := Pointer(Field);
end;}

function TsmxDataSet.GetInternalDataSet: TDataSet;
begin
  //if not Assigned(FInternalDataSet) then
    //FInternalDataSet := TDataSet.Create(nil); //(GetInternalDataSetClass.Create(nil));
  //Result := FInternalDataSet;
  if TObject(GetInternalRef) is TDataSet then
    Result := TDataSet(GetInternalRef) else
    Result := nil;
end;

{function TsmxDataSet.GetInternalDataSetClass: TComponentClass;
begin
  Result := TDataSet;
end;}

{function TsmxDataSet.GetInternalRef: Pointer;
begin
  Result := Pointer(InternalDataSet);//nil; //Pointer(InternalDataSet);
end;}

function TsmxDataSet.GetRecordCount: Integer;
begin
  //Result := TDataSet(GetInternalRef).RecordCount;
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.RecordCount else
    Result := 0;
end;

function TsmxDataSet.GetRecordNo: Integer;
begin
  //Result := TDataSet(GetInternalRef).RecNo;
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.RecNo else
    Result := 0;
end;

procedure TsmxDataSet.SetRecordNo(Value: Integer);
begin
  //TDataSet(GetInternalRef).RecNo := Value;
  if Assigned(InternalDataSet) then
    InternalDataSet.RecNo := Value;
end;

function TsmxDataSet.IsEmpty: Boolean;
begin
  //Result := TDataSet(GetInternalRef).IsEmpty;
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.IsEmpty else
    Result := False;
end;

function TsmxDataSet.Locate(const KeyFields: String; const KeyValues: Variant;
  Options: TsmxLocateOptions = []): Boolean;
begin
  //Result := TDataSet(GetInternalRef).Locate(KeyFields, KeyValues, TLocateOptions(Options));
  if Assigned(InternalDataSet) then
    Result := InternalDataSet.Locate(KeyFields, KeyValues, TLocateOptions(Options)) else
    Result := False;
end;

procedure TsmxDataSet.Next;
begin
  //TDataSet(GetInternalRef).Next;
  if Assigned(InternalDataSet) then
    InternalDataSet.Next;
end;

procedure TsmxDataSet.Prior;
begin
  //TDataSet(GetInternalRef).Prior;
  if Assigned(InternalDataSet) then
    InternalDataSet.Prior;
end;

{function TsmxDataSet.AddParam: IsmxParam;
begin
  inherited AddParam;
  Result := GetParamIntf;
  ParamIntfList.Add(Result);
end;

procedure TsmxDataSet.ClearParams;
begin
  inherited ClearParams;
  ParamIntfList.Clear;
end;

procedure TsmxDataSet.DeleteParam(const Param: IsmxParam);
begin
  inherited DeleteParam(Param);
  ParamIntfList.Delete(Param.ParamIndex);
end;}

{function TsmxDataSet.GetFieldIntfList: TInterfaceList;
begin
  if not Assigned(FFieldIntfList) then
    FFieldIntfList := TInterfaceList.Create;
  Result := FFieldIntfList;
end;

function TsmxDataSet.GetParamIntfList: TInterfaceList;
begin
  if not Assigned(FParamIntfList) then
    FParamIntfList := TInterfaceList.Create;
  Result := FParamIntfList;
end;}

{function TsmxDataSet.GetFieldIntf: IsmxField;
begin
  Result := TsmxField.Create(Self) as IsmxField;
end;

function TsmxDataSet.GetParamIntf: IsmxParam;
begin
  Result := nil;
end;}

{function TsmxDataSet.CreateField(DataType: TsmxDataType): TObject;
var
  FieldDef: TFieldDef;
  Field: TField;
begin
  FieldDef := TDataSet(GetInternalRef).FieldDefs.AddFieldDef;
  FieldDef.DataType := DataType;
  Field := FieldDef.CreateField(TDataSet(GetInternalRef));
  Result := Field;
end;}

{procedure TsmxDataSet.CreateInternalField(Field: TsmxCustomField);
var
  AFieldDef: TFieldDef;
begin
  if Field is TsmxField then
    if not Assigned(TsmxField(Field).Field) and (Field.DataType <> ftUnknown) then
    begin
      AFieldDef := TDataSet(GetInternalRef).FieldDefs.AddFieldDef;
      AFieldDef.DataType := Field.DataType;
      TsmxField(Field).Field := AFieldDef.CreateField(TDataSet(GetInternalRef));
    end;
end;

procedure TsmxDataSet.DestroyInternalField(Field: TsmxCustomField);
begin
  if Field is TsmxField then
    if Assigned(TsmxField(Field).Field) then
    begin
      TsmxField(Field).Field.Free;
      TsmxField(Field).Field := nil;
    end;
end;}

procedure TsmxDataSet.CreateInternalField(Field: TsmxCustomField);
var
  FieldDef: TFieldDef;
begin
  if Field is TsmxField//Assigned(Field)
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
      //if Active then
        //Close;
      TsmxField(Field).FField.Free;
      TsmxField(Field).FField := nil;
    end;
end;

{procedure TsmxDataSet.CreateObject(Item: TObject);
var
  Field: TsmxField;
begin
  if Item is TsmxFieldItem then
  begin
    if not Assigned(TsmxFieldItem(Item).FItemObject) then
    begin
      Field := TsmxField.Create(Self as IsmxBaseInterface);
      Field.InternalDataSet := Self;
      Field.FFieldItem := TsmxFieldItem(Item);
      TsmxFieldItem(Item).FItemObject := Field;
    end;
  end else
    inherited CreateObject(Item);
end;

procedure TsmxDataSet.DestroyObject(Item: TObject);
var
  Field: TsmxField;
begin
  if Item is TsmxFieldItem then
  begin
    if Assigned(TsmxFieldItem(Item).FItemObject) then
    begin
      Field := TsmxField(TsmxFieldItem(Item).FItemObject);
      TsmxFieldItem(Item).FItemObject := nil;
      Field.FFieldItem := nil;
      Field.Free;
    end;
  end else
    inherited DestroyObject(Item);
end;}

function TsmxDataSet.GetFieldClass: TsmxInterfacedPersistentClass;
begin
  Result := TsmxField;
end;

{ TsmxReferenceItem }

{procedure TsmxReferenceItem.Assign(Source: TsmxKitItem);
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
end;}

{ TsmxReferenceList }

{function TsmxReferenceList.Add: TsmxReferenceItem;
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
end;}

{function TsmxDataSet.GetSQL: TStrings;
begin
  Result := nil;
end;

procedure TsmxDataSet.SetSQL(Value: TStrings);
begin
end;}

{procedure TsmxDataSet.AssignDataSet(const Source: IsmxDataSet);
begin
  inherited AssignDataSet(Source);
  if Assigned(SQL) then
  begin
    SQL.Clear;
    if Assigned(Source) then
      SQL.Assign(Source.SQL);
  end;
end;}

procedure TsmxDataSet.Execute;
begin
end;

{function TsmxDataSet.GetPerformanceMode: TsmxPerformanceMode;
begin
  Result := FPerformanceMode;
end;}

procedure TsmxDataSet.Perform;
begin
  case PerformanceMode of
    pmOpen: Open;
    pmExecute: Execute;
  end;
end;

{procedure TsmxDataSet.SetPerformanceMode(Value: TsmxPerformanceMode);
begin
  FPerformanceMode := Value;
end;}

{procedure TsmxDataSet.AssignDataSet(const Source: IsmxDataSet);
begin
  inherited AssignDataSet(Source);
  if Assigned(Source) then
    PerformanceMode := Source.PerformanceMode;
end;}

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
      ObjectClassName := ObjectClass.ClassName else
      ObjectClassName := 'nil';
    raise EsmxComponentError.CreateResFmt(@smxConsts.rsListItemClassError,
      [ObjectClassName, ClassName]);
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
  PrepRequest(FDataSetIntf, True, {pmExecute,} DSFrom);
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
  PrepRequest(FDataSetIntf, Get, {Perform,} DSFrom);
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
    PrepRequest(Result, False, {pmOpen,} DSFrom);
  end;
end;

function TsmxTargetRequest.PrepRequest(const Request: IsmxDataSet; Get: Boolean = True;
  {Perform: TsmxPerformanceMode = pmOpen;} const DSFrom: IsmxDataSet = nil): Boolean;
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
      {case Perform of
        pmOpen: Open;
        pmExecute: Execute;
      end;}
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

{ TsmxCustomConnection }

(*destructor TsmxCustomConnection.Destroy;
begin
  SetDatabaseManager(nil);
  inherited Destroy;
end;

{procedure TsmxConnection.Connect;
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.Connected := True;
end;

procedure TsmxConnection.Disconnect;
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.Connected := False;
end;}

{procedure TsmxConnection.FreeConnection;
begin
  Free;
end;}

{function TsmxConnection.GetConnected: Boolean;
begin
  if Assigned(FDatabaseIntf) then
    Result := FDatabaseIntf.Connected else
    Result := False;
end;

procedure TsmxConnection.SetConnected(Value: Boolean);
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.Connected := Value;
end;}

function TsmxCustomConnection.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxCustomConnection.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxCustomConnection.GetDatabaseManager: IsmxDatabaseManager;
begin
  Result := FDatabaseManagerIntf;
end;

procedure TsmxCustomConnection.SetDatabaseManager(const Value: IsmxDatabaseManager);
begin
  if Assigned(FDatabaseManagerIntf) then
    FDatabaseManagerIntf.RemoveConnection(Self as IsmxConnection);
  FDatabaseManagerIntf := Value;
  if Assigned(FDatabaseManagerIntf) then
    FDatabaseManagerIntf.InsertConnection(Self as IsmxConnection);
end;

{function TsmxCustomConnection.GetDatabaseName: String;
begin
  Result := '';
end;

procedure TsmxCustomConnection.SetDatabaseName(const Value: String);
begin
end;}*)

{ TsmxConnection }

{destructor TsmxConnection.Destroy;
begin
  SetDatabase(nil);
  inherited Destroy;
end;

function TsmxConnection.GetDatabase: IsmxDatabase;
begin
  Result := FDatabaseIntf;
end;

procedure TsmxConnection.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
end;

function TsmxConnection.GetDatabaseName: String;
begin
  if Assigned(FDatabaseIntf) then
    Result := FDatabaseIntf.DatabaseName else
    Result := '';
end;

procedure TsmxConnection.SetDatabaseName(const Value: String);
begin
  if Assigned(FDatabaseIntf) then
    FDatabaseIntf.DatabaseName := Value;
end;

function TsmxConnection.GetInternalRef: Pointer;
begin
  Result := Pointer(FDatabaseIntf);
end;}

{ TsmxObjectItem }

(*constructor TsmxObjectItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  AddObject;
end;

destructor TsmxObjectItem.Destroy;
begin
  DelObject;
  inherited Destroy;
end;

procedure TsmxObjectItem.AddObject;
begin
  if Assigned(Kit) then
    if Assigned(Kit.OwnerObjectInterface) then
      Kit.OwnerObjectInterface.CreateObject(Self);
end;

procedure TsmxObjectItem.DelObject;
begin
  if Assigned(Kit) then
    if Assigned(Kit.OwnerObjectInterface) then
      Kit.OwnerObjectInterface.DestroyObject(Self);
end;

procedure TsmxObjectItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxObjectItem then
    ItemObject := TsmxObjectItem(Source).ItemObject
  else
    inherited Assign(Source);
end;

function TsmxObjectItem.GetDisplayName: String;
begin
  if Assigned(FItemObject) then
    Result := FItemObject.ClassName
  else
    Result := inherited GetDisplayName;
end;

function TsmxObjectItem.GetDisplayObject: TObject;
begin
  if Assigned(FItemObject) then
    Result := FItemObject
  else
    Result := inherited GetDisplayObject;
end;

function TsmxObjectItem.GetKit: TsmxObjectList;
begin
  Result := TsmxObjectList(inherited Kit);
end;

procedure TsmxObjectItem.SetKit(Value: TsmxObjectList);
begin
  if Assigned(Kit) then
    //if Assigned(Kit.Owner) then
      if Assigned(ItemObjectInterface) then
        ItemObjectInterface.ChangeObjectOwner(nil);
  inherited Kit := Value;
  if Assigned(Kit) then
    //if Assigned(Kit.Owner) then
      if Assigned(ItemObjectInterface) then
        ItemObjectInterface.ChangeObjectOwner(Kit.Owner);
end;

function TsmxObjectItem.GetItemObjectInterface: IsmxObjectItem;
begin
  if Assigned(FItemObject) then
    FItemObject.GetInterface(IsmxObjectItem, Result)
  else
    Result := nil;
end;

procedure TsmxObjectItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  if Assigned(ItemObjectInterface) then
    ItemObjectInterface.ChangeObjectIndex(Value);
end;

procedure TsmxObjectItem.SetItemObject(Value: TPersistent);
begin
  if Assigned(FItemObject) then
    FItemObject.Assign(Value);
end;

{procedure TsmxObjectItem.InitializeItemObject(ItemObject: TPersistent);
begin
  FItemObject := ItemObject;
end;}

{ TsmxObjectList }

function TsmxObjectList.Add: TsmxObjectItem;
begin
  Result := TsmxObjectItem(inherited Add);
end;

(*procedure TsmxObjectList.Assign(Source: TsmxKit);
var
  i: Integer;
begin
  if Source is TsmxObjectList then
  begin
    Clear;
    for i := 0 to Source.Count - 1 do
      if Assigned(TsmxSlaveList(Source)[i].Slave) then
        Add(TsmxOwnerCellClass(TsmxSlaveList(Source)[i].Slave.ClassType){,
          TsmxSlaveList(Source)[i].Slave.ImplementorClass}).Assign(Source[i])
      else
        Add.Assign(Source[i]);
  end else
    inherited Assign(Source);
end;*)

(*function TsmxObjectList.GetItem(Index: Integer): TsmxObjectItem;
begin
  Result := TsmxObjectItem(inherited Items[Index]);
end;

procedure TsmxObjectList.SetItem(Index: Integer; Value: TsmxObjectItem);
begin
  inherited Items[Index] := Value;
end;

function TsmxObjectList.GetOwnerObjectInterface: IsmxObjectList;
begin
  {if Assigned(Owner) then
    Owner.GetInterface(IsmxObjectList, Result)
  else
    Result := nil;}
  if Assigned(Owner) then
    //Result := Owner as IsmxObjectList
    Owner.GetInterface(IsmxObjectList, Result)
  else
    Result := nil;
end;*)

procedure TsmxDataSet.CreateInternalParam(Param: TsmxCustomParam);
begin
end;

procedure TsmxDataSet.DestroyInternalParam(Param: TsmxCustomParam);
begin
end;

initialization
  Classes.RegisterClasses([TsmxField{, TsmxConnection}]);

end.
