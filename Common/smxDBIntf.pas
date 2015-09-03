{**************************************}
{                                      }
{            SalesMan v1.0             }
{         Database interfaces          }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxDBIntf;

interface

uses
  Classes, smxBaseIntf, smxTypes;

const
  IID_IsmxDatabase: TGUID = '{6C2E66AD-62E3-4E2E-B207-FB4F0D62F09A}';
  IID_IsmxField: TGUID = '{BB7372C0-3457-487F-AB76-70717AFD7938}';
  IID_IsmxParam: TGUID = '{564458C3-CD9E-402C-800A-06C6065CCF1B}';
  IID_IsmxDataSet: TGUID = '{BF4B869C-77FA-4714-B4B1-E8CDFC08FECB}';

type
  { IsmxDatabase }

  IsmxDataSet = interface;

  IsmxDatabase = interface(IsmxRefComponent)
    ['{6C2E66AD-62E3-4E2E-B207-FB4F0D62F09A}']
    procedure AssignDatabase(const Source: IsmxDatabase);
    procedure CommitTransaction;
    function GetConnected: Boolean;
    function GetDatabaseName: String;
    function GetDriverName: String;
    function GetInTransaction: Boolean;
    function GetParamText: String;
    function NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
    procedure RollbackTransaction;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String);
    procedure SetDriverName(const Value: String);
    procedure SetParamText(const Value: String);
    procedure StartTransaction;

    property Connected: Boolean read GetConnected write SetConnected;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property DriverName: String read GetDriverName write SetDriverName;
    property InTransaction: Boolean read GetInTransaction;
    property ParamText: String read GetParamText write SetParamText;
  end;

  { IsmxField }

  IsmxField = interface(IsmxRefPersistent)
    ['{BB7372C0-3457-487F-AB76-70717AFD7938}']
    procedure AssignField(const Source: IsmxField);
    procedure Clear;
    function GetDataSense: TsmxDataSense;
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetDisplayFormat: String;
    function GetFieldName: String;
    function GetFieldIndex: Integer;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SetDataSense(Value: TsmxDataSense);
    procedure SetDataType(Value: TsmxDataType);
    procedure SetDisplayFormat(const Value: String);
    procedure SetFieldIndex(Value: Integer);
    procedure SetFieldName(const Value: String);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(const Value: Variant);

    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { IsmxParam }

  IsmxParam = interface(IsmxRefPersistent)
    ['{564458C3-CD9E-402C-800A-06C6065CCF1B}']
    procedure AssignParam(const Source: IsmxParam); overload;
    procedure AssignParam(const Source: IsmxField); overload;
    procedure Clear;
    function GetDataLocation: TsmxDataLocation;
    function GetDataSense: TsmxDataSense;
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetNumericScale: Integer;
    function GetParamIndex: Integer;
    function GetParamName: String;
    function GetParamType: TsmxParamType;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SetDataLocation(Value: TsmxDataLocation);
    procedure SetDataSense(Value: TsmxDataSense);
    procedure SetDataType(Value: TsmxDataType);
    procedure SetNumericScale(Value: Integer);
    procedure SetParamIndex(Value: Integer);
    procedure SetParamName(const Value: String);
    procedure SetParamType(Value: TsmxParamType);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(const Value: Variant);

    property DataLocation: TsmxDataLocation read GetDataLocation write SetDataLocation;
    property DataSense: TsmxDataSense read GetDataSense write SetDataSense;
    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    property ParamName: String read GetParamName write SetParamName;
    property ParamIndex: Integer read GetParamIndex write SetParamIndex;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { IsmxDataSet }

  IsmxDataSet = interface(IsmxRefComponent)
    ['{BF4B869C-77FA-4714-B4B1-E8CDFC08FECB}']
    procedure Add;
    function AddField: IsmxField;
    function AddParam: IsmxParam;
    procedure AssignDataSet(const Source: IsmxDataSet);
    function Bof: Boolean;
    procedure Cancel;
    procedure ClearFields;
    procedure ClearParams;
    procedure Close;
    procedure Delete;
    procedure DeleteParam(Index: Integer);
    procedure DeleteField(Index: Integer);
    procedure Edit;
    function Eof: Boolean;
    procedure Execute;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    function FindParam(const ParamName: String): IsmxParam;
    procedure First;
    function GetActive: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDataSetName: String;
    function GetDataSetType: TsmxDataSetType;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetParamCount: Integer;
    function GetParam(Index: Integer): IsmxParam;
    function GetPerformanceMode: TsmxPerformanceMode;
    function GetPrepare: Boolean;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    function GetSQLText: String;
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TsmxLocateOptions = []): Boolean;
    procedure Next;
    procedure Open;
    function ParamByName(const ParamName: String): IsmxParam;
    procedure Perform;
    procedure Post;
    procedure Prior;
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetDataSetName(const Value: String);
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPerformanceMode(Value: TsmxPerformanceMode);
    procedure SetPrepare(Value: Boolean);
    procedure SetRecordNo(Value: Integer);
    procedure SetSQLText(const Value: String);

    property Active: Boolean read GetActive write SetActive;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property DataSetName: String read GetDataSetName write SetDataSetName;
    property DataSetType: TsmxDataSetType read GetDataSetType;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: IsmxField read GetField write SetField;
    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
    property PerformanceMode: TsmxPerformanceMode read GetPerformanceMode write SetPerformanceMode;
    property Prepared: Boolean read GetPrepare write SetPrepare;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
    property SQLText: String read GetSQLText write SetSQLText;
  end;

implementation

end.
