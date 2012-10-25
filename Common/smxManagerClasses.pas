{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Manager classes            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxManagerClasses;

interface

uses
  Classes, smxBaseClasses, smxTypes;

type
  { TsmxProjectItem }

  TsmxProjectItem = class(TsmxKitItem)
  private
    FProjectName: String;
    FGeneration: TsmxGenerationMode;
    FLibraryName: String;
    FFunctionNameOrProgID: String;
    FWindowsAuthorization: Boolean;
    FDatabaseName: String;
    FDriverName: String;
    FLoginPrompt: Boolean;
    FParams: String;
    FUserName: String;
    FPassword: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property ProjectName: String read FProjectName write FProjectName;
    property Generation: TsmxGenerationMode read FGeneration write FGeneration;
    property LibraryName: String read FLibraryName write FLibraryName;
    property FunctionNameOrProgID: String read FFunctionNameOrProgID write FFunctionNameOrProgID;
    property WindowsAuthorization: Boolean read FWindowsAuthorization write FWindowsAuthorization;
    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property DriverName: String read FDriverName write FDriverName;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Params: String read FParams write FParams;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
  end;

  { TsmxProjectItems }

  TsmxProjectItems = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxProjectItem;
  public
    function Add: TsmxProjectItem;
    function FindByName(AProjectName: String): TsmxProjectItem;

    property Items[Index: Integer]: TsmxProjectItem read GetItem; default;
  end;

  { TsmxProjectManager }

  TsmxProjectManager = class(TsmxComponent)
  private
    FFileName: String;
    FProjectList: TsmxProjectItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadProjects;
    procedure WriteProjects;

    property FileName: String read FFileName write FFileName;
    property ProjectList: TsmxProjectItems read FProjectList;
  end;

implementation

uses
  SysUtils;

{ TsmxProjectItem }

constructor TsmxProjectItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FProjectName := '';
  FGeneration := gmFunction;
  FLibraryName := '';
  FFunctionNameOrProgID := '';
  FWindowsAuthorization := False;
  FDatabaseName := '';
  FDriverName := '';
  FLoginPrompt := False;
  FParams := '';
  FUserName := '';
  FPassword := '';
end;

{ TsmxProjectItems }

function TsmxProjectItems.Add: TsmxProjectItem;
begin
  Result := TsmxProjectItem(inherited Add);
end;

function TsmxProjectItems.FindByName(AProjectName: String): TsmxProjectItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].ProjectName, AProjectName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxProjectItems.GetItem(Index: Integer): TsmxProjectItem;
begin
  Result := TsmxProjectItem(inherited Items[Index]);
end;

{ TsmxProjectManager }

constructor TsmxProjectManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProjectList := TsmxProjectItems.Create(TsmxProjectItem);
end;

destructor TsmxProjectManager.Destroy;
begin
  FProjectList.Free;
  inherited Destroy;
end;

procedure TsmxProjectManager.ReadProjects;
var fs: TFileStream; pc: TsmxProjectConnection;
begin
  FProjectList.Clear;
  if FileExists(FFileName) then
  begin
    fs := TFileStream.Create(FFileName, fmOpenRead);
    try
      while fs.Position <> fs.Size do
      begin
        fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));
        with FProjectList.Add do
        begin
          ProjectName := pc.ProjectName;
          Generation := pc.Generation;
          LibraryName := pc.LibraryName;
          FunctionNameOrProgID := pc.FunctionNameOrProgID;
          WindowsAuthorization := pc.WindowsAuthorization;
          DatabaseName := pc.DatabaseName;
          DriverName := pc.DriverName;
          LoginPrompt := pc.LoginPrompt;
          Params := pc.Params;
          UserName := pc.UserName;
          Password := pc.Password;
        end;
      end;
    finally
      fs.Free;
    end;
  end;
end;

procedure TsmxProjectManager.WriteProjects;
var fs: TFileStream; pc: TsmxProjectConnection; i: Integer;
begin
  if FFileName <> '' then
  begin
    fs := TFileStream.Create(FFileName, fmCreate);
    try
      for i := 0 to FProjectList.Count - 1 do
      begin
        with pc do
        begin
          ProjectName := FProjectList[i].ProjectName;
          Generation := FProjectList[i].Generation;
          LibraryName := FProjectList[i].LibraryName;
          FunctionNameOrProgID := FProjectList[i].FunctionNameOrProgID;
          WindowsAuthorization := FProjectList[i].WindowsAuthorization;
          DatabaseName := FProjectList[i].DatabaseName;
          DriverName := FProjectList[i].DriverName;
          LoginPrompt := FProjectList[i].LoginPrompt;
          Params := FProjectList[i].Params;
          UserName := FProjectList[i].UserName;
          Password := FProjectList[i].Password;
        end;
        fs.WriteBuffer(pc, SizeOf(TsmxProjectConnection));
      end;
    finally
      fs.Free;
    end;
  end;
end;

end.
