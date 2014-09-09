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
  Classes, ImgList, smxBaseClasses, smxTypes, smxDBIntf, smxManagerIntf,
  smxLibTypes;

type
  { TsmxProjectItem }

  TsmxProjectList = class;

  TsmxProjectItem = class(TsmxKitItem)
  private
    FDatabaseName: String;
    FDriverName: String;
    FFuncOrClassNameOrProgID: String;
    FProjectName: String;
    FGeneration: TsmxGenerationMode;
    FIsUseUser: Boolean;
    FIsWindowsAuthorization: Boolean;
    FLibraryName: String;
    FParams: String;
    FPassword: String;
    FUserName: String;
    function GetKit: TsmxProjectList;
    procedure SetKit(Value: TsmxProjectList);
  public
    procedure Assign(Source: TsmxKitItem); override;

    property DatabaseName: String read FDatabaseName write FDatabaseName;
    property DriverName: String read FDriverName write FDriverName;
    property FuncOrClassNameOrProgID: String read FFuncOrClassNameOrProgID write FFuncOrClassNameOrProgID;
    property Generation: TsmxGenerationMode read FGeneration write FGeneration;
    property IsUseUser: Boolean read FIsUseUser write FIsUseUser;
    property IsWindowsAuthorization: Boolean read FIsWindowsAuthorization write FIsWindowsAuthorization;
    property Kit: TsmxProjectList read GetKit write SetKit;
    property LibraryName: String read FLibraryName write FLibraryName;
    property Params: String read FParams write FParams;
    property Password: String read FPassword write FPassword;
    property ProjectName: String read FProjectName write FProjectName;
    property UserName: String read FUserName write FUserName;
  end;

  { TsmxProjectList }

  TsmxProjectList = class(TsmxKit)
  protected
    function GetItem(Index: Integer): TsmxProjectItem;
    procedure SetItem(Index: Integer; Value: TsmxProjectItem);
  public
    function Add: TsmxProjectItem;
    function FindByName(const ProjectName: String): TsmxProjectItem;

    property Items[Index: Integer]: TsmxProjectItem read GetItem write SetItem; default;
  end;

  { TsmxProjectManager }

  TsmxProjectManager = class(TsmxComponent)
  private
    FFileName: String;
    FProjectList: TsmxProjectList;
    function GetProjectList: TsmxProjectList;
  public
    destructor Destroy; override;
    procedure ReadProjects;
    procedure WriteProjects;

    property FileName: String read FFileName write FFileName;
    property ProjectList: TsmxProjectList read GetProjectList;
  end;

  { TsmxCallBackManager }

  TsmxCallBackManager = class(TsmxComponent, IsmxCallBackManager)
  private
    FParamList: TsmxParams;
    function GetParamList: TsmxParams;
  protected
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; const Value: Variant);

    property ParamList: TsmxParams read GetParamList;
  public
    destructor Destroy; override;
    function GetFuncCallBack: TsmxFuncCallBack;

    property Values[Index: Integer]: Variant read GetValue write SetValue; default;
  end;

  { TsmxStorageManager }

  TsmxStorageManager = class(TsmxComponent, IsmxStorageManager)
  private
    FParamList: TsmxParams;
    function GetParamList: TsmxParams;
  protected
    function GetValue(const Name: String): Variant;
    procedure SetValue(const Name: String; const Value: Variant);

    property ParamList: TsmxParams read GetParamList;
  public
    destructor Destroy; override;

    property Values[const Name: String]: Variant read GetValue write SetValue; default;
  end;

  { TsmxLibraryManager }

  TsmxLibraryManager = class(TsmxComponent, IsmxLibraryManager)
  private
    FCheckHandle: LongWord;
    //FCallBackManagerIntf: IsmxCallBackManager;
    FIsCheckComp: Boolean;
    FLibInfoProcName: String;
    FLibPath: String;
    FLibraryList: TsmxParams;
    FProgVersMajor: Word;
    FProgVersMinor: Word;
    function CheckLibraryComp(LibHandle: THandle): Boolean;
    procedure ClearLibInfo(var LibInfo: TsmxLibInfo);
    procedure ClearLibraries;
    function GetLibraryList: TsmxParams;
    procedure PrepareLibrary(LibHandle: THandle);
  protected
    //function GetCallBackManager: IsmxCallBackManager;
    function GetCheckHandle: LongWord;
    function GetIsCheckComp: Boolean;
    function GetLibInfoProcName: String;
    function GetLibPath: String;
    function GetLibrary(Index: Integer): THandle;
    function GetLibraryCount: Integer;
    function IndexOf(LibHandle: THandle): Integer;
    //procedure SetCallBackManager(const Value: IsmxCallBackManager);
    procedure SetCheckHandle(Value: LongWord);
    procedure SetIsCheckComp(Value: Boolean);
    procedure SetLibInfoProcName(const Value: String);
    procedure SetLibPath(const Value: String);

    property LibraryList: TsmxParams read GetLibraryList;
  public
    destructor Destroy; override;
    function AddLibrary(const LibName: String): Integer;
    procedure DeleteLibrary(const LibName: String);
    function FindByName(const LibName: String): THandle;
    function GetLibraryInfo(LibHandle: THandle; var LibInfo: TsmxLibInfo): Boolean; overload;
    function GetLibraryInfo(const LibName: String; var LibInfo: TsmxLibInfo): Boolean; overload;
    function GetProcedure(LibHandle: THandle; const ProcName: String): Pointer; overload;
    function GetProcedure(const LibName, ProcName: String): Pointer; overload;
    //function IndexOfName(const LibName: String): Integer;
    //function IndexOfHandle(LibHandle: THandle): Integer;
    function InsertLibrary(LibHandle: THandle): Integer;
    procedure RemoveLibrary(LibHandle: THandle);

    //property CallBackManager: IsmxCallBackManager read GetCallBackManager write SetCallBackManager;
    property CheckHandle: LongWord read GetCheckHandle write SetCheckHandle;
    property IsCheckComp: Boolean read GetIsCheckComp write SetIsCheckComp;
    property LibPath: String read GetLibPath write SetLibPath;
    property LibInfoProcName: String read GetLibInfoProcName write SetLibInfoProcName;
    property Libraries[Index: Integer]: THandle read GetLibrary; default;
    property LibraryCount: Integer read GetLibraryCount;
  end;

  { TsmxDatabaseManager }

  TsmxDatabaseManager = class(TsmxComponent, IsmxDatabaseManager)
  private
    FConnectionList: TInterfaceList;
    procedure ClearConnections;
    function GetConnectionList: TInterfaceList;
  protected
    function GetConnectionCount: Integer;
    function GetConnection(Index: Integer): IsmxConnection;

    property ConnectionList: TInterfaceList read GetConnectionList;
  public
    destructor Destroy; override;
    function FindByName(const DatabaseName: String): IsmxConnection;
    procedure InsertConnection(const Connection: IsmxConnection);
    procedure RemoveConnection(const Connection: IsmxConnection);

    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: IsmxConnection read GetConnection; default;
  end;

  { TsmxFormManager }

  TsmxFormManager = class(TsmxComponent, IsmxFormManager)
  private
    FFormList: TInterfaceList;
    procedure ClearForms;
    function GetFormList: TInterfaceList;
  protected
    function GetFormCount: Integer;
    function GetForm(Index: Integer): IsmxForm;

    property FormList: TInterfaceList read GetFormList;
  public
    destructor Destroy; override;
    function FindByComboID(CfgID: Integer; ID: Integer = 0): IsmxForm;
    procedure InsertForm(const Form: IsmxForm);
    procedure RemoveForm(const Form: IsmxForm);

    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: IsmxForm read GetForm; default;
  end;

  { TsmxImageListManager }

  TsmxImageListManager = class(TsmxComponent, IsmxImageListManager)
  private
    FDelimiter: String;
    FImageListRegister: TsmxParams;
    //FLibraryManagerIntf: IsmxLibraryManager;
    //FNewResourceFuncName: String;
    procedure ClearImageListRegister;
    function GetImageListRegister: TsmxParams;
    function LoadImageList(const ImageListName: String): TCustomImageList;
  protected
    function GetDelimiter: String;
    function GetImageListCount: Integer;
    function GetImageList(Index: Integer): TCustomImageList;
    //function GetLibraryManager: IsmxLibraryManager;
    //function GetNewResourceFuncName: String;
    function IndexOf(ImageList: TCustomImageList): Integer;
    procedure SetDelimiter(const Value: String);
    //procedure SetLibraryManager(const Value: IsmxLibraryManager);
    //procedure SetNewResourceFuncName(const Value: String);

    property ImageListRegister: TsmxParams read GetImageListRegister;
  public
    destructor Destroy; override;
    function AddImageList(const ImageListName: String): Integer;
    procedure DeleteImageList(const ImageListName: String);
    function FindByName(const ImageListName: String): TCustomImageList;
    procedure InsertImageList(ImageList: TCustomImageList);
    procedure RemoveImageList(ImageList: TCustomImageList);

    property Delimiter: String read GetDelimiter write SetDelimiter;
    property ImageListCount: Integer read GetImageListCount;
    property ImageLists[Index: Integer]: TCustomImageList read GetImageList; default;
    //property LibraryManager: IsmxLibraryManager read GetLibraryManager write SetLibraryManager;
    //property NewResourceFuncName: String read GetNewResourceFuncName write SetNewResourceFuncName;
  end;

  { TsmxClassTypeManager }

  TsmxClassTypeManager = class(TsmxComponent, IsmxClassTypeManager)
  private
    FDelimiter: String;
    FClassTypeList: TsmxParams;
    //FLibraryManagerIntf: IsmxLibraryManager;
    procedure ClearClassTypeList;
    function GetClassTypeList: TsmxParams;
    function LoadClassType(const ClassTypeName: String): TPersistentClass;
  protected
    function GetDelimiter: String;
    function GetClassTypeCount: Integer;
    function GetClassType(Index: Integer): TPersistentClass;
    //function GetLibraryManager: IsmxLibraryManager;
    function IndexOf(ClassType: TPersistentClass): Integer;
    procedure SetDelimiter(const Value: String);
    //procedure SetLibraryManager(const Value: IsmxLibraryManager);

    property ClassTypeList: TsmxParams read GetClassTypeList;
  public
    destructor Destroy; override;
    function AddClassType(const ClassTypeName: String): Integer;
    procedure DeleteClassType(const ClassTypeName: String);
    function FindByName(const ClassTypeName: String): TPersistentClass;
    procedure InsertClassType(ClassType: TPersistentClass);
    procedure RemoveClassType(ClassType: TPersistentClass);

    property Delimiter: String read GetDelimiter write SetDelimiter;
    property ClassTypeCount: Integer read GetClassTypeCount;
    property ClassTypes[Index: Integer]: TPersistentClass read GetClassType; default;
    //property LibraryManager: IsmxLibraryManager read GetLibraryManager write SetLibraryManager;
  end;

implementation

uses
  DB, Windows, Controls, SysUtils, smxProcs, smxConsts;

{ TsmxProjectItem }

procedure TsmxProjectItem.Assign(Source: TsmxKitItem);
begin
  if Source is TsmxProjectItem then
  begin
    DatabaseName := TsmxProjectItem(Source).DatabaseName;
    DriverName := TsmxProjectItem(Source).DriverName;
    FuncOrClassNameOrProgID := TsmxProjectItem(Source).FuncOrClassNameOrProgID;
    Generation := TsmxProjectItem(Source).Generation;
    IsUseUser := TsmxProjectItem(Source).IsUseUser;
    IsWindowsAuthorization := TsmxProjectItem(Source).IsWindowsAuthorization;
    LibraryName := TsmxProjectItem(Source).LibraryName;
    Params := TsmxProjectItem(Source).Params;
    Password := TsmxProjectItem(Source).Password;
    ProjectName := TsmxProjectItem(Source).ProjectName;
    UserName := TsmxProjectItem(Source).UserName;
  end else
    inherited Assign(Source);
end;

function TsmxProjectItem.GetKit: TsmxProjectList;
begin
  Result := TsmxProjectList(inherited Kit);
end;

procedure TsmxProjectItem.SetKit(Value: TsmxProjectList);
begin
  inherited Kit := Value;
end;

{ TsmxProjectList }

function TsmxProjectList.Add: TsmxProjectItem;
begin
  Result := TsmxProjectItem(inherited Add);
end;

function TsmxProjectList.FindByName(const ProjectName: String): TsmxProjectItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(Items[i].ProjectName, ProjectName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TsmxProjectList.GetItem(Index: Integer): TsmxProjectItem;
begin
  Result := TsmxProjectItem(inherited Items[Index]);
end;

procedure TsmxProjectList.SetItem(Index: Integer; Value: TsmxProjectItem);
begin
  inherited Items[Index] := Value;
end;

{ TsmxProjectManager }

destructor TsmxProjectManager.Destroy;
begin
  if Assigned(FProjectList) then
    FProjectList.Free;
  inherited Destroy;
end;

function TsmxProjectManager.GetProjectList: TsmxProjectList;
begin
  if not Assigned(FProjectList) then
    FProjectList := TsmxProjectList.Create(TsmxProjectItem);
  Result := FProjectList;
end;

procedure TsmxProjectManager.ReadProjects;
var
  fs: TFileStream;
  pc: TsmxProjectConnection;
begin
  ProjectList.Clear;
  if SysUtils.FileExists(FFileName) then
  begin
    fs := TFileStream.Create(FFileName, fmOpenRead);
    try
      while fs.Position <> fs.Size do
      begin
        fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));
        with ProjectList.Add do
        begin
          DatabaseName := pc.DatabaseName;
          DriverName := pc.DriverName;
          FuncOrClassNameOrProgID := pc.FuncOrClassNameOrProgID;
          Generation := pc.Generation;
          IsUseUser := pc.IsUseUser;
          IsWindowsAuthorization := pc.IsWindowsAuthorization;
          LibraryName := pc.LibraryName;
          Params := pc.Params;
          Password := pc.Password;
          ProjectName := pc.ProjectName;
          UserName := pc.UserName;
        end;
      end;
    finally
      fs.Free;
    end;
  end;
end;

procedure TsmxProjectManager.WriteProjects;
var
  fs: TFileStream;
  pc: TsmxProjectConnection;
  i: Integer;
begin
  if FFileName <> '' then
  begin
    fs := TFileStream.Create(FFileName, fmCreate);
    try
      for i := 0 to ProjectList.Count - 1 do
      begin
        with pc do
        begin
          DatabaseName := ProjectList[i].DatabaseName;
          DriverName := ProjectList[i].DriverName;
          FuncOrClassNameOrProgID := ProjectList[i].FuncOrClassNameOrProgID;
          Generation := ProjectList[i].Generation;
          IsUseUser := ProjectList[i].IsUseUser;
          IsWindowsAuthorization := ProjectList[i].IsWindowsAuthorization;
          LibraryName := ProjectList[i].LibraryName;
          Password := ProjectList[i].Password;
          Params := ProjectList[i].Params;
          ProjectName := ProjectList[i].ProjectName;
          UserName := ProjectList[i].UserName;
        end;
        fs.WriteBuffer(pc, SizeOf(TsmxProjectConnection));
      end;
    finally
      fs.Free;
    end;
  end;
end;

{ TsmxCallBackManager }

destructor TsmxCallBackManager.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

function TsmxCallBackManager.GetFuncCallBack: TsmxFuncCallBack;
begin
  Result := GetValue;
end;

function TsmxCallBackManager.GetValue(Index: Integer): Variant;
var
  Item: TsmxParam;
begin
  Item := ParamList.FindByName(SysUtils.IntToStr(Index));
  if not Assigned(Item) then
  begin
    Item := ParamList.Add;
    Item.ParamName := SysUtils.IntToStr(Index);
  end;
  Result := Item.ParamValue;
end;

procedure TsmxCallBackManager.SetValue(Index: Integer; const Value: Variant);
var
  Item: TsmxParam;
begin
  Item := ParamList.FindByName(SysUtils.IntToStr(Index));
  if not Assigned(Item) then
  begin
    Item := ParamList.Add;
    Item.ParamName := SysUtils.IntToStr(Index);
  end;
  Item.ParamValue := Value;
end;

function TsmxCallBackManager.GetParamList: TsmxParams;
begin
  if not Assigned(FParamList) then
    FParamList := TsmxParams.Create(TsmxParam);
  Result := FParamList;
end;

{ TsmxStorageManager }

destructor TsmxStorageManager.Destroy;
begin
  if Assigned(FParamList) then
    FParamList.Free;
  inherited Destroy;
end;

function TsmxStorageManager.GetValue(const Name: String): Variant;
var
  Item: TsmxParam;
begin
  Item := ParamList.FindByName(Name);
  if not Assigned(Item) then
  begin
    Item := ParamList.Add;
    Item.ParamName := Name;
  end;
  Result := Item.ParamValue;
end;

procedure TsmxStorageManager.SetValue(const Name: String; const Value: Variant);
var
  Item: TsmxParam;
begin
  Item := ParamList.FindByName(Name);
  if not Assigned(Item) then
  begin
    Item := ParamList.Add;
    Item.ParamName := Name;
  end;
  Item.ParamValue := Value;
end;

function TsmxStorageManager.GetParamList: TsmxParams;
begin
  if not Assigned(FParamList) then
    FParamList := TsmxParams.Create(TsmxParam);
  Result := FParamList;
end;

{ TsmxLibraryManager }

destructor TsmxLibraryManager.Destroy;
begin
  if Assigned(FLibraryList) then
  begin
    ClearLibraries;
    FLibraryList.Free;
  end;
  //FCallBackManagerIntf := nil;
  inherited Destroy;
end;

function TsmxLibraryManager.AddLibrary(const LibName: String): Integer;
var
  Handle: THandle;
  Item: TsmxParam;
begin
  Item := LibraryList.FindByName(LibName);
  if not Assigned(Item) and (LibName <> '') then
  begin
    Handle := Windows.LoadLibrary(PChar(LibPath + LibName));
    if Handle > Windows.HINSTANCE_ERROR then
    begin
      if CheckLibraryComp(Handle) then
      begin
        PrepareLibrary(Handle);
        Item := LibraryList.Add;
        with Item do
        begin
          ParamName := LibName;
          ParamValue := Handle;
        end;
      end else
        Windows.FreeLibrary(Handle);
    end;
  end;
  if Assigned(Item) then
    Result := Item.ItemIndex else
    Result := -1;
end;

procedure TsmxLibraryManager.DeleteLibrary(const LibName: String);
var
  Item: TsmxParam;
begin
  Item := LibraryList.FindByName(LibName);
  if Assigned(Item) then
  begin
    Windows.FreeLibrary(THandle(LibraryList[Item.ItemIndex].ParamValue));
    LibraryList.Delete(Item.ItemIndex);
  end;
end;

function TsmxLibraryManager.CheckLibraryComp(LibHandle: THandle): Boolean;

  function CheckVersion: Boolean;
  var
    li: TsmxLibInfo;
  begin
    Result := False;
    if LibHandle > Windows.HINSTANCE_ERROR then
      if GetLibraryInfo(LibHandle, li) then
        if (li.ProgVersion.Major > FProgVersMajor)
            or ((li.ProgVersion.Major = FProgVersMajor)
              and (li.ProgVersion.Minor >= FProgVersMinor)) then
          Result := True;
  end;

begin
  Result := not IsCheckComp or CheckVersion;
end;

procedure TsmxLibraryManager.ClearLibInfo(var LibInfo: TsmxLibInfo);
begin
  with LibInfo do
  begin
    FullName := '';
    Description := '';
    with LibVersion do
    begin
      Major := 0;
      Minor := 0;
      Release := 0;
      Build := 0;
    end;
    LibTypes := [];
    with ProgVersion do
    begin
      Major := 0;
      Minor := 0;
      Release := 0;
      Build := 0;
    end;
    ProcInitLib := nil;
  end;
end;

procedure TsmxLibraryManager.ClearLibraries;
var
  i: Integer;
begin
  for i := LibraryList.Count - 1 downto 0 do
  begin
    Windows.FreeLibrary(THandle(LibraryList[i].ParamValue));
    LibraryList.Delete(i);
  end;
end;

{function TsmxLibraryManager.GetCallBackManager: IsmxCallBackManager;
begin
  Result := FCallBackManagerIntf;
end;

procedure TsmxLibraryManager.SetCallBackManager(const Value: IsmxCallBackManager);
begin
  FCallBackManagerIntf := Value;
end;}

function TsmxLibraryManager.GetCheckHandle: LongWord;
begin
  Result := FCheckHandle;
end;

procedure TsmxLibraryManager.SetCheckHandle(Value: LongWord);
var
  VersM, VersL: Cardinal;
begin
  if FCheckHandle > 0 then
  begin
    FProgVersMajor := 0;
    FProgVersMinor := 0;
  end;
  FCheckHandle := Value;
  if FCheckHandle > 0 then
  begin
    smxProcs.GetFileFullVersion(SysUtils.GetModuleName(FCheckHandle), VersM, VersL);
    FProgVersMajor := LongRec(VersM).Hi;
    FProgVersMinor := LongRec(VersM).Lo;
  end;
end;

function TsmxLibraryManager.GetIsCheckComp: Boolean;
begin
  Result := FIsCheckComp;
end;

procedure TsmxLibraryManager.SetIsCheckComp(Value: Boolean);
begin
  FIsCheckComp := Value;
end;

function TsmxLibraryManager.GetLibInfoProcName: String;
begin
  Result := FLibInfoProcName;
end;

procedure TsmxLibraryManager.SetLibInfoProcName(const Value: String);
begin
  FLibInfoProcName := Value;
end;

function TsmxLibraryManager.GetLibPath: String;
begin
  Result := FLibPath;
end;

procedure TsmxLibraryManager.SetLibPath(const Value: String);
begin
  FLibPath := Value;
end;

function TsmxLibraryManager.GetLibrary(Index: Integer): THandle;
begin
  Result := THandle(LibraryList[Index].ParamValue);
end;

function TsmxLibraryManager.GetLibraryCount: Integer;
begin
  Result := LibraryList.Count;
end;

function TsmxLibraryManager.GetLibraryInfo(LibHandle: THandle; var LibInfo: TsmxLibInfo): Boolean;
var
  ProcLibInfo: TsmxProcLibInfo;
begin
  Result := False;
  ClearLibInfo(LibInfo);
  if LibHandle > Windows.HINSTANCE_ERROR then
  begin
    @ProcLibInfo := Windows.GetProcAddress(LibHandle, PChar(LibInfoProcName));
    if Assigned(ProcLibInfo) then
    begin
      ProcLibInfo(LibInfo);
      Result := True;
    end;
  end;
end;

function TsmxLibraryManager.GetLibraryInfo(const LibName: String; var LibInfo: TsmxLibInfo): Boolean;
var
  Item: TsmxParam;
  Handle: THandle;
begin
  Item := LibraryList.FindByName(LibName);
  if not Assigned(Item) then
  begin
    Handle := Windows.LoadLibrary(PChar(LibPath + LibName));
    try
      Result := GetLibraryInfo(Handle, LibInfo);
    finally
      if Handle > Windows.HINSTANCE_ERROR then
        Windows.FreeLibrary(Handle);
    end;
  end else
    Result := GetLibraryInfo(THandle(Item.ParamValue), LibInfo);
end;

function TsmxLibraryManager.GetLibraryList: TsmxParams;
begin
  if not Assigned(FLibraryList) then
    FLibraryList := TsmxParams.Create(TsmxParam);
  Result := FLibraryList;
end;

function TsmxLibraryManager.GetProcedure(LibHandle: THandle; const ProcName: String): Pointer;
begin
  if (LibHandle > Windows.HINSTANCE_ERROR) and (ProcName <> '') then
    Result := Windows.GetProcAddress(LibHandle, PChar(ProcName)) else
    Result := nil;
end;

function TsmxLibraryManager.GetProcedure(const LibName, ProcName: String): Pointer;
var
  Item: TsmxParam;
  Index: Integer;
begin
  Item := LibraryList.FindByName(LibName);
  if Assigned(Item) then
    Result := GetProcedure(THandle(Item.ParamValue), ProcName)
  else
  begin
    Index := AddLibrary(LibName);
    if Index <> -1 then
      Result := GetProcedure(THandle(LibraryList[Index].ParamValue), ProcName) else
      Result := nil;
  end;
end;

function TsmxLibraryManager.IndexOf(LibHandle: THandle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to LibraryList.Count - 1 do
    if THandle(LibraryList[i].ParamValue) = LibHandle then
    begin
      Result := i;
      Break;
    end;
end;

{function TsmxLibraryManager.IndexOfName(const LibName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to LibraryList.Count - 1 do
    if SysUtils.AnsiCompareText(LibraryList[i].ParamName, LibName) = 0 then
    begin
      Result := LibraryList[i].ItemIndex;
      Break;
    end;
end;}

function TsmxLibraryManager.FindByName(const LibName: String): THandle;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to LibraryList.Count - 1 do
    if SysUtils.AnsiCompareText(LibraryList[i].ParamName, LibName) = 0 then
    begin
      Result := THandle(LibraryList[i].ParamValue);
      Break;
    end;
end;

function TsmxLibraryManager.InsertLibrary(LibHandle: THandle): Integer;
begin
  Result := IndexOf(LibHandle);
  if Result = -1 then
    if LibHandle > Windows.HINSTANCE_ERROR then
      if CheckLibraryComp(LibHandle) then
      begin
        PrepareLibrary(LibHandle);
        with LibraryList.Add do
        begin
          ParamName := SysUtils.ExtractFileName(SysUtils.GetModuleName(LibHandle));
          ParamValue := LibHandle;
          Result := ItemIndex;
        end;
      end;
end;

procedure TsmxLibraryManager.RemoveLibrary(LibHandle: THandle);
var
  Index: Integer;
begin
  Index := IndexOf(LibHandle);
  if Index <> -1 then
    LibraryList.Delete(Index);
end;

procedure TsmxLibraryManager.PrepareLibrary(LibHandle: THandle);
var
  li: TsmxLibInfo;
  ProcInitLib: TsmxProcInitLib;
  FuncCallBack: TsmxFuncCallBack;
begin
  if GetLibraryInfo(LibHandle, li) then
    ProcInitLib := li.ProcInitLib else
    ProcInitLib := nil;
  if Assigned(ProcInitLib) then
  begin
    if Assigned(smxProcs.gCallBackManagerIntf) then
      FuncCallBack := smxProcs.gCallBackManagerIntf.GetFuncCallBack else
      FuncCallBack := nil;
    ProcInitLib(FuncCallBack);
  end;
end;

{ TsmxDatabaseManager }

destructor TsmxDatabaseManager.Destroy;
begin
  if Assigned(FConnectionList) then
  begin
    ClearConnections;
    FConnectionList.Free;
  end;
  inherited Destroy;
end;

procedure TsmxDatabaseManager.ClearConnections;
var
  i: Integer;
begin
  for i := ConnectionList.Count - 1 downto 0 do
    //(DatabaseList[i] as IsmxDatabase).
    //if (ConnectionList[i] as IsmxConnection).GetReference is TsmxCustomConnection then
      (ConnectionList[i] as IsmxConnection).GetReference.Free; //FreeConnection;
    //else
      //(ConnectionList[i] as IsmxConnection).DatabaseManager := nil;
end;

function TsmxDatabaseManager.FindByName(const DatabaseName: String): IsmxConnection;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ConnectionList.Count - 1 do
    if Assigned((ConnectionList[i] as IsmxConnection).Database) then
      if SysUtils.AnsiCompareText((ConnectionList[i] as IsmxConnection).Database.DatabaseName, DatabaseName) = 0 then
      begin
        Result := ConnectionList[i] as IsmxConnection;
        Break;
      end;
end;

function TsmxDatabaseManager.GetConnection(Index: Integer): IsmxConnection;
begin
  Result := ConnectionList[Index] as IsmxConnection;
end;

function TsmxDatabaseManager.GetConnectionCount: Integer;
begin
  Result := ConnectionList.Count;
end;

function TsmxDatabaseManager.GetConnectionList: TInterfaceList;
begin
  if not Assigned(FConnectionList) then
    FConnectionList := TInterfaceList.Create;
  Result := FConnectionList;
end;

procedure TsmxDatabaseManager.InsertConnection(const Connection: IsmxConnection);
begin
  if ConnectionList.IndexOf(Connection) = -1 then
    ConnectionList.Add(Connection);
end;

procedure TsmxDatabaseManager.RemoveConnection(const Connection: IsmxConnection);
begin
  if ConnectionList.IndexOf(Connection) <> -1 then
    ConnectionList.Remove(Connection);
end;

{ TsmxFormManager }

destructor TsmxFormManager.Destroy;
begin
  if Assigned(FFormList) then
  begin
    ClearForms;
    FFormList.Free;
  end;
  inherited Destroy;
end;

procedure TsmxFormManager.ClearForms;
var
  i: Integer;
begin
  for i := FormList.Count - 1 downto 0 do
    (FormList[i] as IsmxForm).GetReference.Free; //FreeForm;
end;

function TsmxFormManager.FindByComboID(CfgID: Integer; ID: Integer = 0): IsmxForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FormList.Count - 1 do
    if ((FormList[i] as IsmxForm).CfgID = CfgID) and ((FormList[i] as IsmxForm).ID = ID) then
    begin
      Result := FormList[i] as IsmxForm;
      Break;
    end;
end;

function TsmxFormManager.GetForm(Index: Integer): IsmxForm;
begin
  Result := FormList[Index] as IsmxForm;
end;

function TsmxFormManager.GetFormCount: Integer;
begin
  Result := FormList.Count;
end;

function TsmxFormManager.GetFormList: TInterfaceList;
begin
  if not Assigned(FFormList) then
    FFormList := TInterfaceList.Create;
  Result := FFormList;
end;

procedure TsmxFormManager.InsertForm(const Form: IsmxForm);
begin
  if FormList.IndexOf(Form) = -1 then
    FormList.Add(Form);
end;

procedure TsmxFormManager.RemoveForm(const Form: IsmxForm);
begin
  if FormList.IndexOf(Form) <> -1 then
    FormList.Remove(Form);
end;

{ TsmxImageListManager }

destructor TsmxImageListManager.Destroy;
begin
  if Assigned(FImageListRegister) then
  begin
    ClearImageListRegister;
    FImageListRegister.Free;
  end;
  inherited Destroy;
end;

function TsmxImageListManager.AddImageList(const ImageListName: String): Integer;
var
  Item: TsmxParam;
  ImageList: TCustomImageList;
begin
  Item := ImageListRegister.FindByName(ImageListName);
  if not Assigned(Item) then
  begin
    ImageList := LoadImageList(ImageListName);
    if Assigned(ImageList) then
    begin
      Item := ImageListRegister.Add;
      Item.ParamName := ImageListName;
      Item.ParamValue := Integer(ImageList);
    end;
  end;
  if Assigned(Item) then
    Result := Item.ItemIndex else
    Result := -1;
end;

procedure TsmxImageListManager.DeleteImageList(const ImageListName: String);
var
  Item: TsmxParam;
begin
  Item := ImageListRegister.FindByName(ImageListName);
  if Assigned(Item) then
  begin
    TCustomImageList(Integer(Item.ParamValue)).Free;
    ImageListRegister.Delete(Item.ItemIndex);
  end;
end;

procedure TsmxImageListManager.ClearImageListRegister;
var
  i: Integer;
begin
  for i := ImageListRegister.Count - 1 downto 0 do
  begin
    TCustomImageList(Integer(ImageListRegister[i].ParamValue)).Free;
    ImageListRegister.Delete(i);
  end;
end;

function TsmxImageListManager.FindByName(const ImageListName: String): TCustomImageList;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ImageListRegister.Count - 1 do
    if SysUtils.AnsiCompareText(ImageListRegister[i].ParamName, ImageListName) = 0 then
    begin
      Result := TCustomImageList(Integer(ImageListRegister[i].ParamValue));
      Break;
    end;
end;

function TsmxImageListManager.GetDelimiter: String;
begin
  Result := FDelimiter;
end;

procedure TsmxImageListManager.SetDelimiter(const Value: String);
begin
  FDelimiter := Value;
end;

function TsmxImageListManager.GetImageList(Index: Integer): TCustomImageList;
begin
  Result := TCustomImageList(Integer(ImageListRegister[Index].ParamValue));
end;

function TsmxImageListManager.GetImageListCount: Integer;
begin
  Result := ImageListRegister.Count;
end;

function TsmxImageListManager.GetImageListRegister: TsmxParams;
begin
  if not Assigned(FImageListRegister) then
    FImageListRegister := TsmxParams.Create(TsmxParam);
  Result := FImageListRegister;
end;

{function TsmxImageListManager.GetLibraryManager: IsmxLibraryManager;
begin
  Result := FLibraryManagerIntf;
end;

procedure TsmxImageListManager.SetLibraryManager(const Value: IsmxLibraryManager);
begin
  FLibraryManagerIntf := Value;
end;}

{function TsmxImageListManager.GetNewResourceFuncName: String;
begin

end;

procedure TsmxImageListManager.SetNewResourceFuncName(const Value: String);
begin

end;}

function TsmxImageListManager.IndexOf(ImageList: TCustomImageList): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to ImageListRegister.Count - 1 do
    if TCustomImageList(Integer(ImageListRegister[i].ParamValue)) = ImageList then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TsmxImageListManager.InsertImageList(ImageList: TCustomImageList);
var
  Index: Integer;
begin
  Index := IndexOf(ImageList);
  if Index = -1 then
    if Assigned(ImageList) then
      with ImageListRegister.Add do
      begin
        ParamName := ImageList.Name;
        ParamValue := Integer(ImageList);
      end;
end;

procedure TsmxImageListManager.RemoveImageList(ImageList: TCustomImageList);
var
  Index: Integer;
begin
  Index := IndexOf(ImageList);
  if Index <> -1 then
    ImageListRegister.Delete(Index);
end;

function TsmxImageListManager.LoadImageList(const ImageListName: String): TCustomImageList;

  procedure ParseImageListName(var ResName, LibName: String);
  var
    i: Integer;
  begin
    i := Pos(FDelimiter, ImageListName);
    if i > 0 then
    begin
      ResName := Copy(ImageListName, 1, i - 1);
      LibName := Copy(ImageListName, i + Length(FDelimiter), MaxInt);
    end else
    begin
      ResName := ImageListName;
      LibName := '';
    end;
  end;

var
  ResName, LibName: String;
  rs: TResourceStream;
  i: Integer;
  Handle: Longword;
begin
  Result := nil;
  ParseImageListName(ResName, LibName);
  if ResName <> '' then
  begin
    Handle := 0;
    if LibName <> '' then
    begin
      if Assigned(smxProcs.gLibraryManagerIntf) then
      begin
        i := smxProcs.gLibraryManagerIntf.AddLibrary(LibName);
        if i <> -1 then
          Handle := smxProcs.gLibraryManagerIntf[i];
      end
    end else
      Handle := HInstance;
    if Handle <> 0 then
    begin
      rs := TResourceStream.Create(Handle, ResName, RT_RCDATA);
      try
        Result := TImageList.Create(nil);
        smxProcs.LoadImagesFromStream(Result, rs);
      finally
        rs.Free;
      end;
    end;
  end;
end;

{ TsmxClassTypeManager }

destructor TsmxClassTypeManager.Destroy;
begin
  if Assigned(FClassTypeList) then
  begin
    ClearClassTypeList;
    FClassTypeList.Free;
  end;
  inherited Destroy;
end;

function TsmxClassTypeManager.AddClassType(const ClassTypeName: String): Integer;
var
  Item: TsmxParam;
  ClassType: TPersistentClass;
begin
  Item := ClassTypeList.FindByName(ClassTypeName);
  if not Assigned(Item) then
  begin
    ClassType := LoadClassType(ClassTypeName);
    if Assigned(ClassType) then
    begin
      Item := ClassTypeList.Add;
      Item.ParamName := ClassTypeName;
      Item.ParamValue := Integer(ClassType);
    end;
  end;
  if Assigned(Item) then
    Result := Item.ItemIndex else
    Result := -1;
end;

procedure TsmxClassTypeManager.DeleteClassType(const ClassTypeName: String);
var
  Item: TsmxParam;
begin
  Item := ClassTypeList.FindByName(ClassTypeName);
  if Assigned(Item) then
  //begin
    //TPersistentClass(Integer(Item.ParamValue)).Free;
    ClassTypeList.Delete(Item.ItemIndex);
  //end;
end;

procedure TsmxClassTypeManager.ClearClassTypeList;
var
  i: Integer;
begin
  for i := ClassTypeList.Count - 1 downto 0 do
  //begin
    //TCustomImageList(Integer(ImageListRegister[i].ParamValue)).Free;
    ClassTypeList.Delete(i);
  //end;
end;

function TsmxClassTypeManager.FindByName(const ClassTypeName: String): TPersistentClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ClassTypeList.Count - 1 do
    if SysUtils.AnsiCompareText(ClassTypeList[i].ParamName, ClassTypeName) = 0 then
    begin
      Result := TPersistentClass(Integer(ClassTypeList[i].ParamValue));
      Break;
    end;
end;

function TsmxClassTypeManager.GetClassType(Index: Integer): TPersistentClass;
begin
  Result := TPersistentClass(Integer(ClassTypeList[Index].ParamValue));
end;

function TsmxClassTypeManager.GetClassTypeCount: Integer;
begin
  Result := ClassTypeList.Count;
end;

function TsmxClassTypeManager.GetClassTypeList: TsmxParams;
begin
  if not Assigned(FClassTypeList) then
    FClassTypeList := TsmxParams.Create(TsmxParam);
  Result := FClassTypeList;
end;

function TsmxClassTypeManager.GetDelimiter: String;
begin
  Result := FDelimiter;
end;

procedure TsmxClassTypeManager.SetDelimiter(const Value: String);
begin
  FDelimiter := Value;
end;

function TsmxClassTypeManager.IndexOf(ClassType: TPersistentClass): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to ClassTypeList.Count - 1 do
    if TPersistentClass(Integer(ClassTypeList[i].ParamValue)) = ClassType then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TsmxClassTypeManager.InsertClassType(ClassType: TPersistentClass);
var
  Index: Integer;
begin
  Index := IndexOf(ClassType);
  if Index = -1 then
    if Assigned(ClassType) then
      with ClassTypeList.Add do
      begin
        ParamName := ClassType.ClassName;
        ParamValue := Integer(ClassType);
      end;
end;

procedure TsmxClassTypeManager.RemoveClassType(ClassType: TPersistentClass);
var
  Index: Integer;
begin
  Index := IndexOf(ClassType);
  if Index <> -1 then
    ClassTypeList.Delete(Index);
end;

function TsmxClassTypeManager.LoadClassType(const ClassTypeName: String): TPersistentClass;

  procedure ParseClassTypeName(var ClassName, LibName: String);
  var
    i: Integer;
  begin
    i := Pos(FDelimiter, ClassTypeName);
    if i > 0 then
    begin
      ClassName := Copy(ClassTypeName, 1, i - 1);
      LibName := Copy(ClassTypeName, i + Length(FDelimiter), MaxInt);
    end else
    begin
      ClassName := ClassTypeName;
      LibName := '';
    end;
  end;

var
  ClassName, LibName: String;
  i: Integer;
  Handle: Longword;
begin
  Result := nil;
  ParseClassTypeName(ClassName, LibName);
  if ClassName <> '' then
  begin
    Handle := 0;
    if LibName <> '' then
    begin
      if Assigned(smxProcs.gLibraryManagerIntf) then
      begin
        i := smxProcs.gLibraryManagerIntf.AddLibrary(LibName);
        if i <> -1 then
          Handle := smxProcs.gLibraryManagerIntf[i];
      end
    end else
      Handle := HInstance;
    if Handle <> 0 then   
      Result := Classes.GetClass(ClassName);
  end;
end;

initialization
  Classes.RegisterClasses([TsmxProjectManager, TsmxCallBackManager,
    TsmxStorageManager, TsmxLibraryManager, TsmxDatabaseManager,
    TsmxFormManager, TsmxImageListManager, TsmxClassTypeManager]);

end.
