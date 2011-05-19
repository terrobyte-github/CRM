unit smxGlobalVariables;

interface

uses
  Classes, Controls, smxClasses, smxDBConnection{, smxDBIntf};

function ImageList: TImageList;
//function DataBase: IsmxDatabase;
//function DBConnection: TsmxDBConnection;
function TargetRequest: TsmxTargetRequest;
//function ConnectDatabase: Boolean;
function ConnectDatabase(AProjectName: String; AUserName: String = ''; APassword: String = ''): Boolean;
procedure DisconnectDatabase;
procedure LoadImage;

//function CheckUser: Boolean;

//var
  //ProcDBListAdd: TsmxProcDBListAdd = nil;

implementation

{$R ..\Resource\pic.res}

uses
  Windows, {Forms,} ImgList, {ActiveX,} {IniFiles,} {SysUtils,} {StrUtils,} {ComObj,}
  smxCommonStorage, {smxLibManager,} {smxDBConnection,} smxCells,
  smxFuncs, smxTypes, smxConsts, smxDBIntf;

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
  //_Database: IsmxDatabase = nil;
  _TargetRequest: TsmxTargetRequest = nil;
  _DBConnection: TsmxDBConnection = nil;

function ImageList: TImageList;
begin
  Result := _ImageList;
end;

{function DataBase: IsmxDatabase;
begin
  Result := _DataBase;
end;}

function DBConnection: TsmxDBConnection;
begin
  Result := _DBConnection;
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
    c := NewCell(nil, _DBConnection.Database, 1000277);
    try
      if c is TsmxCustomRequest then
        with TsmxCustomRequest(c) do
        begin
          Perform;
          f := FindFieldSense(fsKey);
          if Assigned(f) then
            IntfID := f.Value else
            IntfID := 0;
          CommonStorage['IntfID'] := IntfID;
          CommonStorage['@IntfID'] := IntfID;
          f := FindFieldSense(fsValue);
          if Assigned(f) then
            IntfName := f.Value else
            IntfName := '';
          CommonStorage['IntfName'] := IntfName;
          CommonStorage['@IntfName'] := IntfName;
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
    c := NewCell(nil, _DBConnection.Database, 1000236);
    try
      if c is TsmxCustomRequest then
        with TsmxCustomRequest(c) do
        begin
          Perform;
          p := FindParamLocation(plKey);
          if Assigned(p) then
            UserID := p.Value else
            UserID := 0;
          CommonStorage['UserID'] := UserID;
          CommonStorage['@UserID'] := UserID;
          p := FindParamLocation(plValue);
          if Assigned(p) then
            UserName := p.Value else
            UserName := '';
          CommonStorage['UserName'] := UserName;
          CommonStorage['@UserName'] := UserName;
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

{function ConnectDatabase: Boolean;
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
end;}

function ConnectDatabase(AProjectName: String; AUserName: String = ''; APassword: String = ''): Boolean;
var pm: TsmxProjectManager; pr: TsmxProjectItem; //dbc: TsmxDBConnection;
begin
  Result := False;
  pm := TsmxProjectManager.Create(nil);
  pm.FileName := SFileProjectName;
  try
    pr := pm.ProjectList.FindByName(AProjectName);
    if Assigned(pr) then
    begin
      //dbc := TsmxDBConnection.Create(nil, pi.Generation, pi.DatabaseName, pi.LibraryName, pi.FunctionNameOrProgID);
      _DBConnection := TsmxDBConnection.Create(nil);
      try
        with _DBConnection do
        begin
          DatabaseName := pr.DatabaseName;
          LibraryName := pr.LibraryName;
          FunctionNameOrProgID := pr.FunctionNameOrProgID;
          GenerationMode := pr.Generation;
          DriverName := pr.DriverName;
          LoginPrompt := pr.LoginPrompt;
          Params := pr.Params;
          User := AUserName;
          Password := APassword;
          Connect;
          //_Database := Database;
          Result := CheckUser;
        end;
      except
        _DBConnection.Free;
        raise;
      end;
    end;
  finally
    pm.Free;
  end;
end;

{function ConnectDatabase(AProjectName: String; AUserName: String = ''; APassword: String = ''): Boolean;
var fs: TFileStream;  pc: TsmxProjectConnection; FuncCreateDatabase: TsmxFuncCreateDatabase;
  c: TPersistentClass; h: THandle; //db: TsmxDatabase;
  FuncDatabaseCLSID: TsmxFuncDatabaseCLSID; DatabaseCLSID: TGUID; i: IsmxDatabase;
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



        //@FuncDatabase := LibManager.GetProcedure(pc.LibraryName, pc.ProcedureName);
        //if Assigned(FuncDatabase) then
          //_Database := FuncDatabase;

        //@FuncDatabaseCLSID := LibManager.GetProcedure(pc.LibraryName, 'CLSIDDatabase');
        //_LibHandle := LoadLibrary(PChar(String(pc.LibraryName)));

        //h := LoadLibrary(PChar(String(pc.LibraryName)));
        h :=  LibManager.GetLibrary(pc.LibraryName);
        if h > 0 then
        begin
          //@FuncDatabaseCLSID := GetProcAddress(h, PChar('CLSIDDatabase'));
          @FuncCreateDatabase := GetProcAddress(h, PChar(String(pc.ProcedureNameOrProgID)));
          //if Assigned(FuncDatabaseCLSID) then
          if Assigned(FuncCreateDatabase) then
          begin
            //DatabaseCLSID := FuncDatabaseCLSID;
            //RegisterComServer(pc.LibraryName);
            //_Database := CreateComObject(DatabaseCLSID) as IsmxDatabase;

            _Database := FuncCreateDatabase;
            //_Database := NewADODatabase;
          end;
          //FreeLibrary(h);
        end;


          //c := FindClass('TsmxCoADODatabase');
        //if Assigned(c) then
        //begin
          //_Database := TsmxCoDatabaseClass(c).CreateDatabase;
        //end;

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
              //Result := CheckUser;
              Result := True;
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

procedure DisconnectDatabase;
begin
  if Assigned(_DBConnection) then
  begin
    _DBConnection.Free;
    _DBConnection := nil;
  end;
end;

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
  //CoInitialize(nil);
  //_Database := NewADODatabase;
  //ConnectDatabase('CRM');
  _TargetRequest := TsmxTargetRequest.Create(nil);
  //_DBList := TsmxDBList.Create(nil);
  //ProcDBListAdd := _DBList.Add;

finalization
  //ProcDBListAdd := nil;
  //_DBList.Free;
  _TargetRequest.Free;
  //_Database := nil;
  //CoUninitialize;
  //DisconnectDatabase;
  _ImageList.Free;

end.
