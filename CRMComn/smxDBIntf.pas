unit smxDBIntf;

interface

uses
  Classes, DB, SysUtils;

const
  IID_IsmxDatabase = '{6C2E66AD-62E3-4E2E-B207-FB4F0D62F09A}';
  IID_IsmxField = '{BB7372C0-3457-487F-AB76-70717AFD7938}';
  IID_IsmxParam = '{564458C3-CD9E-402C-800A-06C6065CCF1B}';
  IID_IsmxDataSet = '{BF4B869C-77FA-4714-B4B1-E8CDFC08FECB}';

type
  { IsmxDatabase }

  EsmxDBInterfaceError = class(Exception);

  IsmxDataSet = interface;

  TsmxDataSetType = (dstUnknown, dstQuery, dstStoredProc);

  IsmxDatabase = interface(IInterface)
    ['{6C2E66AD-62E3-4E2E-B207-FB4F0D62F09A}']
    procedure CommitTransaction;
    function GetConnected: Boolean;
    function GetDatabase: TObject;
    function GetDatabaseName: String;
    function GetDriverName: String;
    function GetInTransaction: Boolean;
    function GetLoginPrompt: Boolean;
    function GetParams: TStrings;
    function NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
    procedure RollbackTransaction;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String);
    procedure SetDriverName(const Value: String);
    procedure SetLoginPrompt(Value: Boolean);
    procedure SetParams(Value: TStrings);
    procedure StartTransaction;

    property Connected: Boolean read GetConnected write SetConnected;
    //property Database: TObject read GetDatabase;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property DriverName: String read GetDriverName write SetDriverName;
    property InTransaction: Boolean read GetInTransaction;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property Params: TStrings read GetParams write SetParams;
  end;

  { IsmxField }

  TsmxFieldKind = TFieldKind;

  TsmxDataType = TFieldType;

  IsmxField = interface(IInterface)
    ['{BB7372C0-3457-487F-AB76-70717AFD7938}']
    //procedure AssignField(Source: TObject);
    procedure AssignField(const Source: IsmxField);
    function CreateStream: TStream;
    function GetCalculated: Boolean;
    function GetDataType: TsmxDataType;
    function GetDefaultExpression: String;
    function GetDisplayFormat: String;
    function GetField: TObject;
    function GetFieldKind: TsmxFieldKind;
    function GetFieldName: String;
    function GetFieldNo: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    procedure SetCalculated(Value: Boolean);
    procedure SetDefaultExpression(Value: String);
    procedure SetDisplayFormat(Value: String);
    procedure SetFieldKind(Value: TsmxFieldKind);
    procedure SetFieldName(Value: String);
    procedure SetValue(Value: Variant);

    property Calculated: Boolean read GetCalculated write SetCalculated;
    property DataType: TsmxDataType read GetDataType;
    property DefaultExpression: String read GetDefaultExpression write SetDefaultExpression;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    //property Field: TObject read GetField;
    property FieldKind: TsmxFieldKind read GetFieldKind write SetFieldKind;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldNo: Integer read GetFieldNo;
    property Size: Integer read GetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { IsmxParam }

  TsmxParamType = TParamType;

  IsmxParam = interface(IInterface)
    ['{564458C3-CD9E-402C-800A-06C6065CCF1B}']
    //procedure AssignParam(Source: TObject);
    procedure AssignParam(const Source: IInterface);
    function GetDataType: TsmxDataType;
    function GetNumericScale: Integer;
    function GetParam: TObject;
    function GetParamName: String;
    function GetParamNo: Integer;
    function GetParamType: TsmxParamType;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    procedure LoadStream(Stream: TStream);
    procedure SetDataType(Value: TsmxDataType);
    procedure SetNumericScale(Value: Integer);
    procedure SetParamName(const Value: String);
    procedure SetParamType(Value: TsmxParamType);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(Value: Variant);

    property DataType: TsmxDataType read GetDataType write SetDataType;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    //property Param: TObject read GetParam;
    property ParamName: String read GetParamName write SetParamName;
    property ParamNo: Integer read GetParamNo;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;
  
  { IsmxDataSet }

  IsmxDataSet = interface(IInterface)
    ['{BF4B869C-77FA-4714-B4B1-E8CDFC08FECB}']
    procedure Add;
    function AddField(const FieldName: String): IsmxField;
    function AddParam: IsmxParam;
    procedure ClearFields;
    procedure ClearParams;
    //function CreateStreamField(const Value: IsmxField): TStream;
    procedure Close;
    procedure Execute;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    function FindParam(const Value: String): IsmxParam;
    procedure First;
    function GetActive: Boolean;
    function GetBof: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDataSet: TObject;
    function GetDataSetType: TsmxDataSetType;
    function GetEof: Boolean;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetParamCount: Integer;
    function GetParam(Index: Integer): IsmxParam;
    function GetPrepare: Boolean;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    function GetSQL: TStrings;
    procedure Last;
    //procedure LoadStreamParam(const Value: IsmxParam; Stream: TStream);
    function Locate(const KeyFields: String; const KeyValues: Variant): Boolean;
    procedure Next;
    procedure Open;
    function ParamByName(const Value: String): IsmxParam;
    procedure Post;
    procedure Prepare;
    procedure Prior;
    procedure Remove;
    procedure RemoveParam(const Value: IsmxParam);
    procedure RemoveField(const Value: IsmxField);
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPrepare(Value: Boolean);
    procedure SetRecordNo(Value: Integer);
    procedure SetSQL(Value: TStrings);

    property Active: Boolean read GetActive write SetActive;
    property Bof: Boolean read GetBof;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    //property DataSet: TObject read GetDataSet;
    property DataSetType: TsmxDataSetType read GetDataSetType;
    property Eof: Boolean read GetEof;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: IsmxField read GetField write SetField;
    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: IsmxParam read GetParam write SetParam;
    property Prepared: Boolean read GetPrepare write SetPrepare;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property RecordCount: Integer read GetRecordCount;
    property SQL: TStrings read GetSQL write SetSQL;
  end;

implementation

end.
 