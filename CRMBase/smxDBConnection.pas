unit smxDBConnection;

interface

uses
  Classes, smxBaseClasses, smxTypes, smxDBIntf;

type
  { TsmxDBConnection }

  TsmxDBConnection = class(TsmxComponent)
  private
    FDatabaseName: String;
    FLibraryName: String;
    FFunctionNameOrProgID: String;
    FDriverName: String;
    FLoginPrompt: Boolean;
    FParams: String;
    FGenerationMode: TsmxGenerationMode;
    //FProgID: String;
    FUser: String;
    FPassword: String;
    FDatabaseIntf: IsmxDatabase;
    function CreateDatabaseAsFunc: IsmxDatabase;
    function CreateDatabaseAsCOM: IsmxDatabase;
  public
    //constructor Create(AOwner: TComponent; AGeneration: TsmxGenerationMode;
      //ADBName, ALibName, AFuncNameOrProgID: String); reintroduce; virtual;
    //constructor CreateAsCOM(AOwner: TComponent; ADBName, ALibName, AProgID: String); virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property LibraryName: String read FLibraryName write FLibraryName;
    property FunctionNameOrProgID: String read FFunctionNameOrProgID write FFunctionNameOrProgID;
    property DriverName: String read FDriverName write FDriverName;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Params: String read FParams write FParams;
    property GenerationMode: TsmxGenerationMode read FGenerationMode write FGenerationMode;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property Database: IsmxDatabase read FDatabaseIntf;
  end;

implementation

uses
  StrUtils, ComObj, smxLibManager, smxDBManager, smxConsts;

{ TsmxDBConnection }

{constructor TsmxDBConnection.Create(AOwner: TComponent; AGeneration: TsmxGenerationMode;
  ADBName, ALibName, AFuncNameOrProgID: String);
begin
  inherited Create(AOwner);
  FDatabaseName := ADBName;
  FLibraryName := ALibName;
  FFunctionNameOrProgID := AFuncNameOrProgID;
  case AGeneration of
    gmFunction: FDatabaseIntf := CreateDatabaseAsFunc;
    gmCOM: FDatabaseIntf := CreateDatabaseAsCOM;
  end;
  FDatabaseIntf.DatabaseName := FDatabaseName;
  DBManager.InsertDBConnection(Self);
end;}

{constructor TsmxDBConnection.CreateAsCOM(AOwner: TComponent; ADBName, ALibName, AProgID: String);
begin
  inherited Create(AOwner);
  FDatabaseName := ADBName;
  FLibraryName := ALibName;
  FProgID := AProgID;
  FDatabaseIntf := CreateDatabaseCOM;
  FDatabaseIntf.DatabaseName := FDatabaseName;
  DBManager.InsertDBConnection(Self);
end;}

destructor TsmxDBConnection.Destroy;
begin
  //DBManager.RemoveDBConnection(Self);
  Disconnect;
  FDatabaseIntf := nil;
  inherited Destroy;
end;

function TsmxDBConnection.CreateDatabaseAsFunc: IsmxDatabase;
var FuncCreateDatabase: TsmxFuncCreateDatabase;
begin
  FuncCreateDatabase := LibManager.GetProcedure(FLibraryName, FFunctionNameOrProgID);
  if Assigned(FuncCreateDatabase) then
    Result := FuncCreateDatabase else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;

function TsmxDBConnection.CreateDatabaseAsCOM: IsmxDatabase;
begin
  if LibManager.CheckLibraryComp(FLibraryName) then
  begin
    RegisterComServer(FLibraryName);
    Result := CreateComObject(ProgIDToClassID(FFunctionNameOrProgID)) as IsmxDatabase;
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;

procedure TsmxDBConnection.Connect;
begin
  Disconnect;
  case FGenerationMode of
    gmFunction: FDatabaseIntf := CreateDatabaseAsFunc;
    gmCOM: FDatabaseIntf := CreateDatabaseAsCOM;
  end;
  with FDatabaseIntf do
  begin
    DatabaseName := FDatabaseName;
    DriverName := FDriverName;
    LoginPrompt := FLoginPrompt;
    Params.Text := AnsiReplaceText(AnsiReplaceText(FParams, '%User%', FUser), '%Password%', FPassword);
    try
      Connected := True;
    except
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
    end;
  end;
  DBManager.InsertDBConnection(Self);
end;

procedure TsmxDBConnection.Disconnect;
begin
  DBManager.RemoveDBConnection(Self);
  if Assigned(FDatabaseIntf) then
  begin
    if FDatabaseIntf.InTransaction then
      FDatabaseIntf.RollbackTransaction;
    FDatabaseIntf.Connected := False;
  end;
end;

end.
 