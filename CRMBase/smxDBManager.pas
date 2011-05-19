{**************************************}
{                                      }
{            SalesMan v1.0             }
{       Database Manager classes       }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxDBManager;

interface

uses
  Classes, smxClasses;

type
  { TsmxDBItem }

  TsmxDBItem = class(TsmxKitItem)
  private
    FDBConnection: TsmxDBConnection;
    FDatabaseName: String;
    //FDatabaseIntf: IsmxDatabase;
  public
    constructor Create(AKit: TsmxKit); override;

    property DBConnection: TsmxDBConnection read FDBConnection write FDBConnection;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    //property Database: IsmxDatabase read FDatabaseIntf write FDatabaseIntf;
  end;

  { TsmxDBItems }

  TsmxDBItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxDBItem;
  public
    function Add: TsmxDBItem;
    function FindByDBConnection(ADBConnection: TsmxDBConnection): TsmxDBItem;
    function FindByName(ADatabaseName: String): TsmxDBItem;
    //function FindByDatabase(const ADatabase: IsmxDatabase): TsmxDBItem;

    property Items[Index: Integer]: TsmxDBItem read GetItem; default;
  end;

  { TsmxDatabaseManager }

  TsmxDatabaseManager = class(TsmxCustomDatabaseManager)
  private
    FDBConnectionList: TsmxDBItems;
    function GetDBConnection(Index: Integer): TsmxDBConnection;
    function GetDBConnectionCount: Integer;
    procedure DestroyDBConnections;
  protected
    property DBConnectionList: TsmxDBItems read FDBConnectionList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByName(ADatabaseName: String): TsmxDBConnection; override;
    //function FindByName(ADatabaseName: String): IsmxDatabase; override;
    procedure InsertDBConnection(ADBConnection: TsmxDBConnection); override;
    procedure RemoveDBConnection(ADBConnection: TsmxDBConnection); override;

    property DBConnectionCount: Integer read GetDBConnectionCount;
    property DBConnections[Index: Integer]: TsmxDBConnection read GetDBConnection; default;
  end;

function DBManager: TsmxDatabaseManager;
//function FindDatabaseByName(ADatabaseName: String): IsmxDatabase;

implementation

uses
  SysUtils;

var
  _DBManager: TsmxDatabaseManager = nil;

function DBManager: TsmxDatabaseManager;
begin
  Result := _DBManager;
end;

{function FindDatabaseByName(ADatabaseName: String): IsmxDatabase;
var dbc: TsmxDBConnection;
begin
  dbc := _DBManager.FindByName(ADatabaseName);
  if Assigned(dbc) then
    Result := dbc.Database else
    Result := nil;
end;}

{ TsmxDBItem }

constructor TsmxDBItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FDBConnection := nil;
  FDatabaseName := '';
  //FDatabaseIntf := nil;
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

{function TsmxDBItems.FindByDatabase(const ADatabase: IsmxDatabase): TsmxDBItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Database = ADatabase then
    begin
      Result := Items[i];
      Break;
    end;
end;}

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

{ TsmxDatabaseManager }

constructor TsmxDatabaseManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBConnectionList := TsmxDBItems.Create(TsmxDBItem);
end;

destructor TsmxDatabaseManager.Destroy;
begin
  DestroyDBConnections;
  FDBConnectionList.Free;
  inherited Destroy;
end;

procedure TsmxDatabaseManager.DestroyDBConnections;
var i: Integer;
begin
  for i := FDBConnectionList.Count - 1 downto 0 do
    FDBConnectionList[i].DBConnection.Free;
end;

function TsmxDatabaseManager.GetDBConnectionCount: Integer;
begin
  Result := FDBConnectionList.Count;
end;

function TsmxDatabaseManager.GetDBConnection(Index: Integer): TsmxDBConnection;
begin
  Result := FDBConnectionList[Index].DBConnection;
end;

function TsmxDatabaseManager.FindByName(ADatabaseName: String): TsmxDBConnection;
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByName(ADatabaseName);
  if Assigned(d) then
    Result := d.DBConnection else
    Result := nil;
end;

{function TsmxDatabaseManager.FindByName(ADatabaseName: String): IsmxDatabase;
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByName(ADatabaseName);
  if Assigned(d) then
    //Result := d.Database else
    Result := d.DBConnection.Database else
    Result := nil;
end;}

procedure TsmxDatabaseManager.InsertDBConnection(ADBConnection: TsmxDBConnection);
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByDBConnection(ADBConnection);
  if not Assigned(d) then
    with DBConnectionList.Add do
    begin
      DBConnection := ADBConnection;
      DatabaseName := ADBConnection.DatabaseName;
      //Database := ADBConnection.Database;
      //DatabaseName := ADBConnection.Database.DatabaseName;
    end;
end;

procedure TsmxDatabaseManager.RemoveDBConnection(ADBConnection: TsmxDBConnection);
var d: TsmxDBItem;
begin
  d := DBConnectionList.FindByDBConnection(ADBConnection);
  if Assigned(d) then
    DBConnectionList.Remove(d);
end;

initialization
  _DBManager := TsmxDatabaseManager.Create(nil);

finalization
  _DBManager.Free;

end.
