unit smxDBManager;

interface

uses
  Classes, Windows, smxBaseClasses, smxClasses, smxDBConnection, smxDBIntf;

type
  { TsmxDBItem }

  TsmxDBItem = class(TsmxKitItem)
  private
    FDBConnection: TsmxDBConnection;
    FDatabaseName: String;
    FDatabaseIntf: IsmxDatabase;
  public
    constructor Create(AKit: TsmxKit); override;

    property DBConnection: TsmxDBConnection read FDBConnection write FDBConnection;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property Database: IsmxDatabase read FDatabaseIntf write FDatabaseIntf;
  end;

  { TsmxDBItems }

  TsmxDBItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxDBItem;
  public
    function Add: TsmxDBItem;
    function FindByDBConnection(ADBConnection: TsmxDBConnection): TsmxDBItem;
    function FindByName(ADatabaseName: String): TsmxDBItem;
    function FindByDatabase(const ADatabase: IsmxDatabase): TsmxDBItem;

    property Items[Index: Integer]: TsmxDBItem read GetItem; default;
  end;

  { TsmxDBManager }

  TsmxDBManager = class(TsmxComponent)
  private
    FDBConnectionList: TsmxDBItems;
    function GetDBConnection(Index: Integer): TsmxDBConnection;
    function GetDBConnectionCount: Integer;
    procedure FreeDBConnections;
  protected
    property DBConnectionList: TsmxDBItems read FDBConnectionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByName(ADatabaseName: String): TsmxDBConnection;
    procedure InsertDBConnection(ADBConnection: TsmxDBConnection);
    procedure RemoveDBConnection(ADBConnection: TsmxDBConnection);

    property DBConnectionCount: Integer read GetDBConnectionCount;
    property DBConnections[Index: Integer]: TsmxDBConnection read GetDBConnection; default;
  end;

function DBManager: TsmxDBManager;

implementation

uses
  SysUtils;

var
  _DBManager: TsmxDBManager = nil;

function DBManager: TsmxDBManager;
begin
  Result := _DBManager;
end;

{ TsmxDBItem }

constructor TsmxDBItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FDBConnection := nil;
  FDatabaseName := '';
  FDatabaseIntf := nil;
end;

{ TsmxDBItems }

function TsmxDBItems.Add: TsmxDBItem;
begin
  Result := TsmxDBItem(inherited Add);
end;

function TsmxDBItems.FindByName(ADatabaseName: String): TsmxDBItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].DatabaseName, ADatabaseName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxDBItems.FindByDatabase(const ADatabase: IsmxDatabase): TsmxDBItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Database = ADatabase then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxDBItems.FindByDBConnection(ADBConnection: TsmxDBConnection): TsmxDBItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].DBConnection = ADBConnection then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxDBItems.GetItem(Index: Integer): TsmxDBItem;
begin
  Result := TsmxDBItem(inherited Items[Index]);
end;

{ TsmxDBManager }

constructor TsmxDBManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBConnectionList := TsmxDBItems.Create(TsmxDBItem);
end;

destructor TsmxDBManager.Destroy;
begin
  FreeDBConnections;
  FDBConnectionList.Free;
  inherited Destroy;
end;

procedure TsmxDBManager.FreeDBConnections;
var i: Integer;
begin
  for i := FDBConnectionList.Count - 1 downto 0 do
    FDBConnectionList[i].DBConnection.Free;
end;

function TsmxDBManager.GetDBConnectionCount: Integer;
begin
  Result := FDBConnectionList.Count;
end;

function TsmxDBManager.GetDBConnection(Index: Integer): TsmxDBConnection;
begin
  Result := FDBConnectionList[Index].DBConnection;
end;

function TsmxDBManager.FindByName(ADatabaseName: String): TsmxDBConnection;
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByName(ADatabaseName);
  if Assigned(d) then
    Result := d.DBConnection else
    Result := nil;
end;

procedure TsmxDBManager.InsertDBConnection(ADBConnection: TsmxDBConnection);
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByDBConnection(ADBConnection);
  if not Assigned(d) then
    with DBConnectionList.Add do
    begin
      DBConnection := ADBConnection;
      Database := ADBConnection.Database;
      DatabaseName := ADBConnection.Database.DatabaseName;
    end;
end;

procedure TsmxDBManager.RemoveDBConnection(ADBConnection: TsmxDBConnection);
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByDBConnection(ADBConnection);
  if Assigned(d) then
    DBConnectionList.Remove(d);
end;

initialization
  _DBManager := TsmxDBManager.Create(nil);

finalization
  _DBManager.Free;

end.
