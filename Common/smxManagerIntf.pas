unit smxManagerIntf;

interface

uses
  Classes, Controls, ImgList, smxBaseIntf, smxDBIntf, smxLibTypes;

const
  IID_IsmxCallBackManager: TGUID = '{402AC1B6-6DEB-4CD2-93FB-7528D6DEA805}';
  IID_IsmxStorageManager: TGUID = '{972F6869-AF48-482D-933E-F763266A15B0}';
  IID_IsmxLibraryManager: TGUID = '{F778C1C0-A698-4EF0-8DCB-904A7AF1E6C0}';
  IID_IsmxConnection: TGUID = '{24415658-D331-48A8-8560-1845F3076622}';
  IID_IsmxDatabaseManager: TGUID = '{AD0FCDC5-ED53-430D-9A09-D4096818EE17}';
  IID_IsmxForm: TGUID = '{6A6AE753-A9BB-4617-B65E-4BC0DD271966}';
  IID_IsmxFormManager: TGUID = '{718F6D65-D295-4CCB-B1E5-0BCEDD2F544B}';
  IID_IsmxImageListManager: TGUID = '{2557DE80-5DF0-4474-B686-D340690E6E54}';

type
  { IsmxCallBackManager }

  IsmxCallBackManager = interface(IsmxBaseInterface)
    ['{402AC1B6-6DEB-4CD2-93FB-7528D6DEA805}']
    function GetFuncCallBack: TsmxFuncCallBack;
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; const Value: Variant);

    property Values[Index: Integer]: Variant read GetValue write SetValue; default;
  end;

  { IsmxStorageManager }

  IsmxStorageManager = interface(IsmxBaseInterface)
    ['{972F6869-AF48-482D-933E-F763266A15B0}']
    function GetValue(const Name: String): Variant;
    procedure SetValue(const Name: String; const Value: Variant);

    property Values[const Name: String]: Variant read GetValue write SetValue; default;
  end;

  { IsmxLibraryManager }

  IsmxLibraryManager = interface(IsmxBaseInterface)
    ['{F778C1C0-A698-4EF0-8DCB-904A7AF1E6C0}']
    //function CallLibrary(const LibName: String): THandle;
    //function CheckLibraryComp(LibHandle: THandle): Boolean; overload;
    //function CheckLibraryComp(const LibName: String): Boolean; overload;
    function AddLibrary(const LibName: String): Integer;
    procedure DeleteLibrary(const LibName: String);
    function GetCallBackManager: IsmxCallBackManager;
    function GetCheckHandle: LongWord;
    function GetIsCheckComp: Boolean;
    function GetLibInfoProcName: String;
    function GetLibPath: String;
    function GetLibrary(Index: Integer): THandle;
    function GetLibraryCount: Integer;
    function GetLibraryInfo(LibHandle: THandle; var LibInfo: TsmxLibInfo): Boolean; overload;
    function GetLibraryInfo(const LibName: String; var LibInfo: TsmxLibInfo): Boolean; overload;
    function GetProcedure(LibHandle: THandle; const ProcName: String): Pointer; overload;
    function GetProcedure(const LibName, ProcName: String): Pointer; overload;
    function IndexOfName(const LibName: String): Integer;
    function IndexOfHandle(LibHandle: THandle): Integer;
    function InsertLibrary(Handle: THandle): Integer;
    procedure RemoveLibrary(Handle: THandle);
    procedure SetCallBackManager(const Value: IsmxCallBackManager);
    procedure SetCheckHandle(Value: LongWord);
    procedure SetIsCheckComp(Value: Boolean);
    procedure SetLibInfoProcName(const Value: String);
    procedure SetLibPath(const Value: String);

    property CallBackManager: IsmxCallBackManager read GetCallBackManager write SetCallBackManager;
    property CheckHandle: LongWord read GetCheckHandle write SetCheckHandle;
    property IsCheckComp: Boolean read GetIsCheckComp write SetIsCheckComp;
    property LibInfoProcName: String read GetLibInfoProcName write SetLibInfoProcName;
    property LibPath: String read GetLibPath write SetLibPath;
    property Libraries[Index: Integer]: THandle read GetLibrary; default;
    property LibraryCount: Integer read GetLibraryCount;
  end;

  { IsmxConnection }

  IsmxDatabaseManager = interface;

  IsmxConnection = interface(IsmxBaseInterface)
    ['{24415658-D331-48A8-8560-1845F3076622}']
    procedure Connect;
    procedure Disconnect;
    procedure FreeConnection;
    function GetConnected: Boolean;
    function GetDatabase: IsmxDatabase;
    function GetDatabaseManager: IsmxDatabaseManager;
    //function GetInternalRef: Pointer;
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
    function FindByName(const DatabaseName: String): IsmxConnection;
    function GetConnectionCount: Integer;
    function GetConnection(Index: Integer): IsmxConnection;
    procedure InsertConnection(Connection: IsmxConnection);
    procedure RemoveConnection(Connection: IsmxConnection);

    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: IsmxConnection read GetConnection; default;
  end;

  { IsmxForm }

  IsmxFormManager = interface;

  IsmxForm = interface(IsmxBaseInterface)
    ['{6A6AE753-A9BB-4617-B65E-4BC0DD271966}']
    procedure Close;
    procedure FreeForm;
    function GetCfgID: Integer;
    function GetFormManager: IsmxFormManager;
    function GetID: Integer;
    //function GetInternalRef: Pointer;
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
    function FindByComboID(CfgID: Integer; ID: Integer = 0): IsmxForm;
    function GetFormCount: Integer;
    function GetForm(Index: Integer): IsmxForm;
    procedure InsertForm(Form: IsmxForm);
    procedure RemoveForm(Form: IsmxForm);

    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: IsmxForm read GetForm; default;
  end;

  { IsmxImageListManager }

  IsmxImageListManager = interface(IsmxBaseInterface)
    ['{2557DE80-5DF0-4474-B686-D340690E6E54}']
    // ImageListName = format text: "LibName[Delimiter]ResName"
    function AddImageList(const ImageListName: String): Integer;
    procedure DeleteImageList(const ImageListName: String);
    function FindByName(const ImageListName: String): TCustomImageList;
    function GetDelimiter: String;
    function GetImageList(Index: Integer): TCustomImageList;
    function GetImageListCount: Integer;
    function GetLibraryManager: IsmxLibraryManager;
    //function GetNewResourceFuncName: String;
    procedure SetDelimiter(const Value: String);
    procedure SetLibraryManager(const Value: IsmxLibraryManager);
    //procedure SetNewResourceFuncName(const Value: String);

    property Delimiter: String read GetDelimiter write SetDelimiter;
    property ImageLists[Index: Integer]: TCustomImageList read GetImageList; default;
    property ImageListCount: Integer read GetImageListCount;
    property LibraryManager: IsmxLibraryManager read GetLibraryManager write SetLibraryManager;
    //property NewResourceFuncName: String read GetNewResourceFuncName write SetNewResourceFuncName;
  end;

implementation

end.
