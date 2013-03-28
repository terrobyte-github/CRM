unit smxManagerIntf;

interface

uses
  Classes, Controls, smxBaseIntf, smxDBIntf;

const
  IID_IsmxCallBackManager: TGUID = '{402AC1B6-6DEB-4CD2-93FB-7528D6DEA805}';
  IID_IsmxStorageManager: TGUID = '{972F6869-AF48-482D-933E-F763266A15B0}';
  IID_IsmxLibraryManager: TGUID = '{F778C1C0-A698-4EF0-8DCB-904A7AF1E6C0}';
  IID_IsmxConnection: TGUID = '{24415658-D331-48A8-8560-1845F3076622}';
  IID_IsmxDatabaseManager: TGUID = '{AD0FCDC5-ED53-430D-9A09-D4096818EE17}';
  IID_IsmxForm: TGUID = '{6A6AE753-A9BB-4617-B65E-4BC0DD271966}';
  IID_IsmxFormManager: TGUID = '{718F6D65-D295-4CCB-B1E5-0BCEDD2F544B}';

type
  { IsmxCallBackManager }

  IsmxCallBackManager = interface(IsmxBaseInterface)
    ['{402AC1B6-6DEB-4CD2-93FB-7528D6DEA805}']
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; const Value: Variant);

    property Values[Index: Integer]: Variant read GetValue write SetValue;
  end;

  { IsmxStorageManager }

  IsmxStorageManager = interface(IsmxBaseInterface)
    ['{972F6869-AF48-482D-933E-F763266A15B0}']
    function GetValue(const Name: String): Variant;
    procedure SetValue(const Name: String; const Value: Variant);

    property Values[const Name: String]: Variant read GetValue write SetValue;
  end;

  { IsmxLibraryManager }

  IsmxLibraryManager = interface(IsmxBaseInterface)
    ['{F778C1C0-A698-4EF0-8DCB-904A7AF1E6C0}']
    function CallLibrary(const ALibName: String): THandle;
    function CheckLibraryComp(ALibHandle: THandle): Boolean; overload;
    function CheckLibraryComp(const ALibName: String): Boolean; overload;
    function FindByName(const ALibName: String): THandle;
    function GetIsCheckComp: Boolean;
    function GetLibPath: String;
    function GetLibrary(Index: Integer): THandle;
    function GetLibraryCount: Integer;
    function GetProcedure(ALibHandle: THandle; const AProcName: String): Pointer; overload;
    function GetProcedure(const ALibName, AProcName: String): Pointer; overload;
    function GetProcLibInfoName: String;
    procedure InsertLibrary(const ALibName: String); overload;
    procedure InsertLibrary(AHandle: THandle); overload;
    procedure RemoveLibrary(const ALibName: String); overload;
    procedure RemoveLibrary(AHandle: THandle); overload;
    procedure SetIsCheckComp(Value: Boolean);
    procedure SetLibPath(const Value: String);
    procedure SetProcLibInfoName(const Value: String);

    property IsCheckComp: Boolean read GetIsCheckComp write SetIsCheckComp;
    property LibPath: String read GetLibPath write SetLibPath;
    property Libraries[Index: Integer]: THandle read GetLibrary;
    property LibraryCount: Integer read GetLibraryCount;
    property ProcLibInfoName: String read GetProcLibInfoName write SetProcLibInfoName;
  end;

  { IsmxConnection }

  IsmxDatabaseManager = interface;

  IsmxConnection = interface(IInterface)
    ['{24415658-D331-48A8-8560-1845F3076622}']
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDatabaseManager: IsmxDatabaseManager;
    function GetInternalRef: Pointer;
    procedure SetConnected(Value: Boolean);
    procedure SetDatabase(const Value: IsmxDatabase);
    procedure SetDatabaseManager(const Value: IsmxDatabaseManager);

    property Connected: Boolean read GetConnected write SetConnected;
    property Database: IsmxDatabase read GetDatabase write SetDatabase;
    property DatabaseManager: IsmxDatabaseManager read GetDatabaseManager write SetDatabaseManager;
  end;

  { IsmxDatabaseManager }

  IsmxDatabaseManager = interface(IsmxBaseInterface)
    ['{AD0FCDC5-ED53-430D-9A09-D4096818EE17}']
    function FindByName(const ADatabaseName: String): IsmxConnection;
    function GetConnectionCount: Integer;
    function GetConnection(Index: Integer): IsmxConnection;
    procedure InsertConnection(AConnection: IsmxConnection);
    procedure RemoveConnection(AConnection: IsmxConnection);

    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: IsmxConnection read GetConnection;
  end;

  { IsmxForm }

  IsmxFormManager = interface;

  IsmxForm = interface(IInterface)
    ['{6A6AE753-A9BB-4617-B65E-4BC0DD271966}']
    procedure Close;
    function GetCfgID: Integer;
    function GetFormManager: IsmxFormManager;
    function GetID: Integer;
    function GetInternalRef: Pointer;
    function GetModalResult: TModalResult;
    procedure SetCfgID(Value: Integer);
    procedure SetFormManager(const Value: IsmxFormManager);
    procedure SetModalResult(Value: TModalResult);
    procedure Show;
    function ShowModal: TModalResult;

    property CfgID: Integer read GetCfgID write SetCfgID;
    property FormManager: IsmxFormManager read GetFormManager write SetFormManager;
    property ID: Integer read GetID;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
  end;

  { IsmxFormManager }

  IsmxFormManager = interface(IsmxBaseInterface)
    ['{718F6D65-D295-4CCB-B1E5-0BCEDD2F544B}']
    function FindByComboID(ACfgID: Integer; AID: Integer = 0): IsmxForm;
    function GetFormCount: Integer;
    function GetForm(Index: Integer): IsmxForm;
    procedure InsertForm(AForm: IsmxForm);
    procedure RemoveForm(AForm: IsmxForm);

    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: IsmxForm read GetForm;
  end;

implementation

end.
