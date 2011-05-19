unit smxGlobal;

interface

uses
  Controls, smxBaseClasses, smxClasses, smxDBIntf, smxTypes;

function ImageList: TImageList;
function DataBase: IsmxDatabase;
function TargetRequest: TsmxTargetRequest;
function ConnectDatabase: Boolean;
//function ConnectDatabase(AProjectName: String; AUserName: String = ''; APassword: String = ''): Boolean;
//procedure DisconnectDatabase;
procedure LoadImage;

implementation

{$R ..\Resource\pic.res}

uses
  Classes, Windows, Forms, ImgList, ActiveX, IniFiles, SysUtils, StrUtils,
  smxCells, smxADODB, smxGlobalStorage, smxLibManager, smxDBManager,
  smxDBConnection, smxFuncs, smxConsts;

type
  { TsmxDBConnection }

  {TsmxDBConnection = class(TsmxComponent)
  private
    //FProjectName: String;
    FLibraryName: String;
    FProcedureName: String;
    FUserName: String;
    FPassword: String;
    //FConnected: Boolean;
    FDatabaseIntf: IsmxDatabase;
    //procedure SetProjectName(Value: String);
    //procedure SetUserName(Value: String);
    //procedure SetPassword(Value: String);
    //procedure SetConnected(Value: Boolean);
    procedure PrepareDatabase;
  public
    constructor Create(AOwner: TComponent; ALibraryName, AProcedureName: String); reintroduce; virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    //property ProjectName: String read FProjectName write SetProjectName;
    property LibraryName: String read FLibraryName; //write FLibraryName;
    property ProcedureName: String read FProcedureName; //write FProcedureName;
    property UserName: String read FUserName write FUserName; //SetUserName;
    property Password: String read FPassword write FPassword; //SetPassword;
    //property Connected: Boolean read FConnected write SetConnected;
    property Database: IsmxDatabase read FDatabaseIntf;
  end;}

  { _TCustomImageList }

  _TCustomImageList = class(TCustomImageList)
  end;

var
  _ImageList: TImageList = nil;
  _Database: IsmxDatabase = nil;
  _TargetRequest: TsmxTargetRequest = nil;
  _DBConnection: TsmxDBConnection = nil;


function ImageList: TImageList;
begin
  Result := _ImageList;
end;

function DataBase: IsmxDatabase;
begin
  Result := _DataBase;
end;

function TargetRequest: TsmxTargetRequest;
begin
  Result := _TargetRequest;
end;

procedure LoadImage;
var rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, 'pic', RT_RCDATA);
  try
    _TCustomImageList(_ImageList).ReadData(rs);
  finally
    rs.Free;
  end;
end;

function CheckIntfUser: Boolean;
var c: TsmxBaseCell; f: IsmxField; IntfID: Integer; IntfName: String;
begin
  Result := False;
  try
    c := NewCell(nil, _Database, 1000277);
    try
      if c is TsmxCustomRequest then
        with TsmxCustomRequest(c) do
        begin
          Perform;
          f := FindFieldSense(fsKey);
          if Assigned(f) then
            IntfID := f.Value else
            IntfID := 0;
          GlobalStorage['IntfID'] := IntfID;
          GlobalStorage['@IntfID'] := IntfID;
          f := FindFieldSense(fsValue);
          if Assigned(f) then
            IntfName := f.Value else
            IntfName := '';
          GlobalStorage['IntfName'] := IntfName;
          GlobalStorage['@IntfName'] := IntfName;
          if IntfID > 0 then
            Result := True;
        end;
    finally
      c.Free;
    end;
  except
    raise EsmxCellError.CreateRes(@SCellBuildError);
  end;
end;

function CheckUser: Boolean;
var c: TsmxBaseCell; p: IsmxParam; UserID: Integer; UserName: String;
begin
  Result := False;
  try
    c := NewCell(nil, _Database, 1000236);
    try
      if c is TsmxCustomRequest then
        with TsmxCustomRequest(c) do
        begin
          Perform;
          p := FindParamLocation(plKey);
          if Assigned(p) then
            UserID := p.Value else
            UserID := 0;
          GlobalStorage['UserID'] := UserID;
          GlobalStorage['@UserID'] := UserID;
          p := FindParamLocation(plValue);
          if Assigned(p) then
            UserName := p.Value else
            UserName := '';
          GlobalStorage['UserName'] := UserName;
          GlobalStorage['@UserName'] := UserName;
          if UserID > 0 then
            Result := CheckIntfUser else
          if UserID = -1 then
            Inf(UserName);
        end;
    finally
      c.Free;
    end;
  except
    raise EsmxCellError.CreateRes(@SCellBuildError);
  end;
end;

{procedure CreateDatabase;
var dbc: TsmxDBConnection;
begin
  dbc := TsmxDBConnection.Create(nil);
  try
    dbc.ProjectName := 'CRM';
    dbc.Connected := True;
  finally
    dbc.Free;
  end;
end;}

function ConnectDatabase: Boolean;
var f: TIniFile;
begin
  Result := False;
  if FileExists(SFileConnectName) then
  begin
    try
      f := TIniFile.Create(ExtractFilePath(Application.ExeName) + SFileConnectName);
      try
        with _Database do
        begin
          if Connected then
            Connected := False;
          DatabaseName := 'Base';
          LoginPrompt := False;
          f.ReadSectionValues('Params', Params);
          Connected := True;
          Result := CheckUser;
          //CreateDatabase;
        end;
      finally
        f.Free;
      end;
    except
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
    end;
  end;
end;

{function ConnectDatabase(AProjectName: String; AUserName: String = ''; APassword: String = ''): Boolean;
var fs: TFileStream;  pc: TsmxProjectConnection; FuncDatabase: TsmxFuncDatabaseCreate;
  //DatabaseIntf: IsmxDatabase;
begin
  Result := False;
  if FileExists(SFileProjectName) and (AProjectName <> '') then
  begin
    fs := TFileStream.Create(SFileProjectName, fmOpenRead);
    try
      while (fs.Position <> fs.Size) and (AnsiCompareText(pc.ProjectName, AProjectName) <> 0) do
        fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));

      if AnsiCompareText(pc.ProjectName, AProjectName) = 0 then
      begin
        //h := LibManager[pc.LibraryName];
        //if h = 0 then
          //raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
        //@FuncDatabase := GetProcAddress(h, PChar(pc.ProcedureName));



        @FuncDatabase := LibManager.GetProcedure(pc.LibraryName, pc.ProcedureName);
        if Assigned(FuncDatabase) then
          _Database := FuncDatabase;
        if Assigned(_Database) then
          with _Database do
          begin
            DatabaseName := pc.DatabaseName;
            if pc.DriverName <> '' then
              DriverName := pc.DriverName;
            LoginPrompt := pc.LoginPrompt;
            AnsiReplaceText(pc.Params, '%User%', AUserName);
            AnsiReplaceText(pc.Params, '%Password%', APassword);
            Params.Text := pc.Params;
            try
              Connected := True;
              //DBManager.InsertDatabase(DatabaseIntf);
              Result := CheckUser;
            except
              _Database := nil;
              raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
            end;
          end;
      end;
    finally
      fs.Free;
    end;
  end;
end;}

{function ConnectDatabase(AProjectName: String; AUserName: String = ''; APassword: String = ''): Boolean;
var fs: TFileStream;  pc: TsmxProjectConnection;
begin
  Result := False;
  if Assigned(_DBConnection) then
  begin
    _DBConnection.Free;
    _DBConnection := nil;
  end;
  if FileExists(SFileProjectName) and (AProjectName <> '') then
  begin
    fs := TFileStream.Create(SFileProjectName, fmOpenRead);
    try
      while (fs.Position <> fs.Size) and (AnsiCompareText(pc.ProjectName, AProjectName) <> 0) do
        fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));

      if AnsiCompareText(pc.ProjectName, AProjectName) = 0 then
      begin
        _DBConnection := TsmxDBConnection.Create(nil, pc.LibraryName, pc.ProcedureName);
        _DBConnection.UserName := AUserName;
        _DBConnection.Password := APassword;
        _DBConnection.Database.DatabaseName := pc.DatabaseName;
        if pc.DriverName <> '' then
          _DBConnection.Database.DriverName := pc.DriverName;
        _DBConnection.Database.LoginPrompt := pc.LoginPrompt;
        _DBConnection.Database.Params.Text := pc.Params;
        _DBConnection.Connect;
        _Database := _DBConnection.Database;
        Result := CheckUser;
      end;
    finally
      fs.Free;
    end;
  end;
end;}

{procedure DisconnectDatabase;
begin
  if Assigned(_Database) then
  begin
    if _Database.InTransaction then
      _Database.RollbackTransaction;
    _Database.Connected := False;
    _Database := nil;
  end;
end;}

{procedure DisconnectDatabase;
begin
  if Assigned(_DBConnection) then
  begin
    _DBConnection.Free;
    _DBConnection := nil;
  end;
end;}

{function ShowLoginPrompt: IsmxDatabase;
begin

end;}

{ TsmxDBConnection }

{constructor TsmxDBConnection.Create(AOwner: TComponent; ALibraryName, AProcedureName: String);
begin
  inherited Create(AOwner);
  FLibraryName := ALibraryName;
  FProcedureName := AProcedureName;
  PrepareDatabase;
  DBManager.InsertDatabase(FDatabaseIntf);
end;

destructor TsmxDBConnection.Destroy;
begin
  //SetConnected(False);
  //FDatabaseIntf := nil;

  DBManager.RemoveDatabase(FDatabaseIntf);
  Disconnect;
  FDatabaseIntf := nil;
  inherited Destroy;
end;

procedure TsmxDBConnection.PrepareDatabase;
var FuncDatabase: TsmxFuncDatabaseCreate;
begin
  FuncDatabase := LibManager.GetProcedure(FLibraryName, FProcedureName);
  if Assigned(FuncDatabase) then
    FDatabaseIntf := FuncDatabase else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
end;}

{procedure TsmxDBConnection.SetProjectName(Value: String);
begin
  if FProjectName <> Value then
  begin
    SetConnected(False);
    FProjectName := Value;
  end;
end;}

{procedure TsmxDBConnection.SetUserName(Value: String);
begin
  if FUserName <> Value then
  begin
    //SetConnected(False);
    Disconnect;
    FUserName := Value;
  end;
end;}

{procedure TsmxDBConnection.SetPassword(Value: String);
begin
  if FPassword <> Value then
  begin
    //SetConnected(False);
    Disconnect;
    FPassword := Value;
  end;
end;}

{procedure TsmxDBConnection.SetConnected(Value: Boolean);
begin
  if Value then
    Connect else
    Disconnect;
  FConnected := Value;
end;}

{procedure TsmxDBConnection.Connect;
//var //fs: TFileStream;  pc: TsmxProjectConnection; //h: THandle;
  //FuncDatabase: TsmxFuncDatabaseCreate;
begin
  //if not Assigned(FDatabaseIntf) then
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  //if Connected then
    Disconnect;
  //FuncDatabase := LibManager.GetProcedure(FLibraryName, FProcedureName);
  //if Assigned(FuncDatabase) then
    //FDatabaseIntf := FuncDatabase;

  with FDatabaseIntf do
  begin
    //DatabaseName := pc.DatabaseName;
    //DriverName := pc.DriverName;
    //LoginPrompt := pc.LoginPrompt;
    //AnsiReplaceText(pc.Params, '%User%', FUserName);
    //AnsiReplaceText(pc.Params, '%Password%', FPassword);
    //Params.Text := pc.Params;

    AnsiReplaceText(Params.Text, '%User%', FUserName);
    AnsiReplaceText(Params.Text, '%Password%', FPassword);
    try
      Connected := True;
      //DBManager.InsertDatabase(FDatabaseIntf);
    except
      //FDatabaseIntf := nil;
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
    end;
  end;
  //FConnected := True;}


  {if FileExists(SFileProjectName) and (FProjectName <> '') then
  begin
    fs := TFileStream.Create(SFileProjectName, fmOpenRead);
    try
      while (fs.Position <> fs.Size) and (AnsiCompareText(pc.ProjectName, FProjectName) <> 0) do
        fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));

      if AnsiCompareText(pc.ProjectName, FProjectName) = 0 then
      begin
        //h := LibManager[pc.LibraryName];
        //if h = 0 then
          //raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
        //@FuncDatabase := GetProcAddress(h, PChar(pc.ProcedureName));

        @FuncDatabase := LibManager.GetProcedure(pc.LibraryName, pc.ProcedureName);
        if Assigned(FuncDatabase) then
          FDatabaseIntf := FuncDatabase;
        if not Assigned(FDatabaseIntf) then
          raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
        with FDatabaseIntf do
        begin
          DatabaseName := pc.DatabaseName;
          DriverName := pc.DriverName;
          LoginPrompt := pc.LoginPrompt;
          AnsiReplaceText(pc.Params, '%User%', FUserName);
          AnsiReplaceText(pc.Params, '%Password%', FPassword);
          Params.Text := pc.Params;
          try
            Connected := True;
            DBManager.InsertDatabase(FDatabaseIntf);
          except
            FDatabaseIntf := nil;
            raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
          end;
        end;
      end;
    finally
      fs.Free;
    end;
  end else
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);}
//end;

{procedure TsmxDBConnection.Disconnect;
begin
  //if Assigned(FDatabaseIntf) then
  //begin
    //DBManager.RemoveDatabase(FDatabaseIntf);
    if FDatabaseIntf.InTransaction then
      FDatabaseIntf.RollbackTransaction;
    FDatabaseIntf.Connected := False;
    //FConnected := False;
    //FDatabaseIntf := nil;
  //end;
end;}

initialization
  _ImageList := TImageList.Create(nil);
  CoInitialize(nil);
  _Database := NewADODatabase;
  _TargetRequest := TsmxTargetRequest.Create(nil);

finalization
  _TargetRequest.Free;
  _Database := nil;
  CoUninitialize;
  //DisconnectDatabase;
  _ImageList.Free;

end.
