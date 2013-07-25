unit smxDBIntf;

interface

uses
  Classes, DB, smxBaseIntf;

const
  IID_IsmxDatabase: TGUID = '{6C2E66AD-62E3-4E2E-B207-FB4F0D62F09A}';
  IID_IsmxField: TGUID = '{BB7372C0-3457-487F-AB76-70717AFD7938}';
  IID_IsmxParam: TGUID = '{564458C3-CD9E-402C-800A-06C6065CCF1B}';
  IID_IsmxDataSet: TGUID = '{BF4B869C-77FA-4714-B4B1-E8CDFC08FECB}';

type
  { IsmxDatabase }

  IsmxDataSet = interface;

  TsmxDataSetType = (dstQuery, dstStoredProc);

  IsmxDatabase = interface(IsmxBaseInterface)
    ['{6C2E66AD-62E3-4E2E-B207-FB4F0D62F09A}']
    procedure AssignDatabase(const Source: IsmxDatabase);
    procedure CommitTransaction;
    function GetConnected: Boolean;
    function GetInternalRef: Pointer;
    function GetDatabaseName: String;
    function GetDriverName: String;
    function GetInTransaction: Boolean;
    function GetParams: TStrings;
    function NewDataSet(DataSetType: TsmxDataSetType): IsmxDataSet;
    procedure RollbackTransaction;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabaseName(const Value: String);
    procedure SetDriverName(const Value: String);
    procedure SetParams(Value: TStrings);
    procedure StartTransaction;

    property Connected: Boolean read GetConnected write SetConnected;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property DriverName: String read GetDriverName write SetDriverName;
    property InTransaction: Boolean read GetInTransaction;
    property Params: TStrings read GetParams write SetParams;
  end;

  { IsmxField }

  TsmxDataType = TFieldType;

  TsmxFieldSense = (fsGeneral, fsKey, fsValue, fsResult, fsMessage,
    fsForeignKey);

  IsmxField = interface(IsmxBaseInterface)
    ['{BB7372C0-3457-487F-AB76-70717AFD7938}']
    procedure AssignField(const Source: IsmxField);
    procedure Clear;
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    function GetDisplayFormat: String;
    function GetFieldName: String;
    function GetFieldIndex: Integer;
    function GetFieldSense: TsmxFieldSense;
    function GetInternalRef: Pointer;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SetDisplayFormat(const Value: String);
    procedure SetFieldIndex(Value: Integer);
    procedure SetFieldName(const Value: String);
    procedure SetFieldSense(Value: TsmxFieldSense);
    procedure SetInternalRef(Value: Pointer);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(const Value: Variant);

    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType;
    property DisplayFormat: String read GetDisplayFormat write SetDisplayFormat;
    property FieldName: String read GetFieldName write SetFieldName;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex;
    property FieldSense: TsmxFieldSense read GetFieldSense write SetFieldSense;
    property InternalRef: Pointer read GetInternalRef write SetInternalRef;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { IsmxParam }

  TsmxParamType = TParamType;

  TsmxParamLocation = (plConst, plKey, plValue, plResult, plMessage,
    plForeignKey, plInput, plOutput, plInOutput, plStorageParam,
    plParentParam, plFilterDesk, plGrid, plParentFilterDesk, plParentGrid);

  IsmxParam = interface(IsmxBaseInterface)
    ['{564458C3-CD9E-402C-800A-06C6065CCF1B}']
    procedure AssignParam(const Source: IsmxParam); overload;
    procedure AssignParam(const Source: IsmxField); overload;
    procedure Clear;
    function GetDataSet: IsmxDataSet;
    function GetDataType: TsmxDataType;
    //function GetDefValue: Variant;
    function GetInternalRef: Pointer;
    function GetNumericScale: Integer;
    function GetParamLocation: TsmxParamLocation;
    function GetParamName: String;
    function GetParamIndex: Integer;
    function GetParamType: TsmxParamType;
    function GetPrecision: Integer;
    function GetSize: Integer;
    function GetValue: Variant;
    function IsBlob: Boolean;
    function IsNull: Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SetDataType(Value: TsmxDataType);
    //procedure SetDefValue(const Value: Variant);
    procedure SetInternalRef(Value: Pointer);
    procedure SetNumericScale(Value: Integer);
    procedure SetParamIndex(Value: Integer);
    procedure SetParamLocation(Value: TsmxParamLocation);
    procedure SetParamName(const Value: String);
    procedure SetParamType(Value: TsmxParamType);
    procedure SetPrecision(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetValue(const Value: Variant);

    property DataSet: IsmxDataSet read GetDataSet;
    property DataType: TsmxDataType read GetDataType write SetDataType;
    property InternalRef: Pointer read GetInternalRef write SetInternalRef;
    property NumericScale: Integer read GetNumericScale write SetNumericScale;
    property ParamLocation: TsmxParamLocation read GetParamLocation write SetParamLocation;
    property ParamName: String read GetParamName write SetParamName;
    property ParamIndex: Integer read GetParamIndex write SetParamIndex;
    property ParamType: TsmxParamType read GetParamType write SetParamType;
    property Precision: Integer read GetPrecision write SetPrecision;
    property Size: Integer read GetSize write SetSize;
    property Value: Variant read GetValue write SetValue;
  end;

  { IsmxDataSet }

  TsmxLocateOptions = TLocateOptions;

  IsmxDataSet = interface(IsmxBaseInterface)
    ['{BF4B869C-77FA-4714-B4B1-E8CDFC08FECB}']
    procedure Add;
    //function AddField(const FieldName: String): IsmxField;
    //function AddParam(const ParamName: String): IsmxParam;
    function AddField(DataType: TsmxDataType): IsmxField;
    function AddParam(DataType: TsmxDataType): IsmxParam;
    procedure AssignDataSet(const Source: IsmxDataSet);
    function Bof: Boolean;
    procedure Cancel;
    procedure ClearFields;
    procedure ClearParams;
    procedure Close;
    procedure Delete;
    procedure DeleteParam(const Param: IsmxParam);
    procedure DeleteField(const Field: IsmxField);
    procedure Edit;
    function Eof: Boolean;
    procedure Execute;
    function FieldByName(const FieldName: String): IsmxField;
    function FindField(const FieldName: String): IsmxField;
    function FindParam(const ParamName: String): IsmxParam;
    procedure First;
    function GetActive: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDataSetType: TsmxDataSetType;
    function GetField(Index: Integer): IsmxField;
    function GetFieldCount: Integer;
    function GetInternalRef: Pointer;
    function GetParamCount: Integer;
    function GetParam(Index: Integer): IsmxParam;
    function GetPrepare: Boolean;
    function GetRecordNo: Integer;
    function GetRecordCount: Integer;
    function GetSQL: TStrings;
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TsmxLocateOptions = []): Boolean;
    procedure Next;
    procedure Open;
    function ParamByName(const ParamName: String): IsmxParam;
    procedure Post;
    procedure Prior;
    procedure SetActive(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetField(Index: Integer; const Value: IsmxField);
    procedure SetParam(Index: Integer; const Value: IsmxParam);
    procedure SetPrepare(Value: Boolean);
    procedure SetRecordNo(Value: Integer);
    procedure SetSQL(Value: TStrings);

    property Active: Boolean read GetActive write SetActive;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property DataSetType: TsmxDataSetType read GetDataSetType;
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
