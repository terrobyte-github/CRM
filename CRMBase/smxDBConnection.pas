unit smxDBConnection;

interface

uses
  Classes, smxBaseClasses, smxDBIntf;

type
  { TsmxDBConnection }

  TsmxDBConnection = class(TsmxComponent)
  private
    FLibraryName: String;
    FProcedureName: String;
    FUserName: String;
    FPassword: String;
    FDatabaseIntf: IsmxDatabase;
    procedure CreateDatabase;
  public
    constructor Create(AOwner: TComponent; ALibraryName, AProcedureName: String); reintroduce; virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    property LibraryName: String read FLibraryName;
    property ProcedureName: String read FProcedureName;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property Database: IsmxDatabase read FDatabaseIntf;
  end;

implementation

uses
  StrUtils, smxDBManager, smxLibManager, smxTypes, smxConsts;

{ TsmxDBConnection }

constructor TsmxDBConnection.Create(AOwner: TComponent; ALibraryName, AProcedureName: String);
begin
  inherited Create(AOwner);
  FLibraryName := ALibraryName;
  FProcedureName := AProcedureName;
  CreateDatabase;
  DBManager.InsertDBConnection(Self);
end;

destructor TsmxDBConnection.Destroy;
begin
  DBManager.RemoveDBConnection(Self);
  Disconnect;
  FDatabaseIntf := nil;
  inherited Destroy;
end;

procedure TsmxDBConnection.CreateDatabase;
var FuncDatabase: TsmxFuncDatabaseCreate;
begin
  FuncDatabase := LibManager.GetProcedure(FLibraryName, FProcedureName);
  if Assigned(FuncDatabase) then
    FDatabaseIntf := FuncDatabase else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;

procedure TsmxDBConnection.Connect;
begin
  Disconnect;
  with FDatabaseIntf do
  begin
    AnsiReplaceText(Params.Text, '%User%', FUserName);
    AnsiReplaceText(Params.Text, '%Password%', FPassword);
    try
      Connected := True;
    except
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
    end;
  end;
end;

procedure TsmxDBConnection.Disconnect;
begin
  if FDatabaseIntf.InTransaction then
    FDatabaseIntf.RollbackTransaction;
  FDatabaseIntf.Connected := False;
end;

end.
 