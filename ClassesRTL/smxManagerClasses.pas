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
    FDataEntityList: TInterfaceList;
    procedure ClearDataEntities;
    function GetDataEntityList: TInterfaceList;
  protected
    function GetDataEntityCount: Integer;
    function GetDataEntity(Index: Integer): IsmxDataEntity;

    property DataEntityList: TInterfaceList read GetDataEntityList;
  public
    destructor Destroy; override;
    function FindByName(const DataEntityName: String): IsmxDataEntity;
    procedure InsertDataEntity(const DataEntity: IsmxDataEntity);
    procedure RemoveDataEntity(const DataEntity: IsmxDataEntity);

    property DataEntityCount: Integer read GetDataEntityCount;
    property DataEntities[Index: Integer]: IsmxDataEntity read GetDataEntity; default;
  end;

  { TsmxFormManager }

  TsmxFormManager = class(TsmxComponent, IsmxFormManager)
  private
    FFormControlList: TInterfaceList;
    procedure ClearFormControls;
    function GetFormControlList: TInterfaceList;
  protected
    function GetFormControlCount: Integer;
    function GetFormControl(Index: Integer): IsmxFormControl;

    property FormControlList: TInterfaceList read GetFormControlList;
  public
    destructor Destroy; override;
    function FindByComboID(CfgID: Integer; ID: Integer = 0): IsmxFormControl;
    procedure InsertFormControl(const FormControl: IsmxFormControl);
    procedure RemoveFormControl(const FormControl: IsmxFormControl);

    property FormControlCount: Integer read GetFormControlCount;
    property FormControls[Index: Integer]: IsmxFormControl read GetFormControl; default;
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
    function VarToInt(const Value: Variant): Integer;
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
    //function AddImageList(const ImageListName: String): Integer;
    //procedure DeleteImageList(const ImageListName: String);
    function FindByName(const ImageListName: String): TCustomImageList;
    //procedure InsertImageList(ImageList: TCustomImageList);
    procedure RegisterImageListName(const ImageListName: String; ImageList: TCustomImageList = nil);
    //procedure RemoveImageList(ImageList: TCustomImageList);
    function ResolvedImageList(ImageList: TCustomImageList): String;
    function ResolvedImageListName(const ImageListName: String; IsRegister: Boolean = False): TCustomImageList;
    procedure UnRegisterImageListName(const ImageListName: String);

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
    function VarToInt(const Value: Variant): Integer;
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
    //function AddClassType(const ClassTypeName: String): Integer;
    //procedure DeleteClassType(const ClassTypeName: String);
    function FindByName(const ClassTypeName: String): TPersistentClass;
    //procedure InsertClassType(ClassType: TPersistentClass);
    //procedure RemoveClassType(ClassType: TPersistentClass);
    procedure RegisterClassTypeName(const ClassTypeName: String; ClassType: TPersistentClass = nil);
    //function ResolvedClassType(ClassType: TPersistentClass;
    //  out ClassTypeName: String): Boolean;
    //function ResolvedClassTypeName(const ClassTypeName: String;
    //  out ClassType: TPersistentClass): Boolean;
    function ResolvedClassType(ClassType: TPersistentClass): String;
    function ResolvedClassTypeName(const ClassTypeName: String; IsRegister: Boolean = False): TPersistentClass;
    procedure UnRegisterClassTypeName(const ClassTypeName: String);

    property Delimiter: String read GetDelimiter write SetDelimiter;
    property ClassTypeCount: Integer read GetClassTypeCount;
    property ClassTypes[Index: Integer]: TPersistentClass read GetClassType; default;
    //property LibraryManager: IsmxLibraryManager read GetLibraryManager write SetLibraryManager;
  end;

implementation

uses
  DB, Windows, Controls, SysUtils, Variants, smxProcs, smxConsts;

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
    //Windows.FreeLibrary(THandle(LibraryList[Item.ItemIndex].ParamValue));
    Windows.FreeLibrary(THandle(Item.ParamValue));
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
  Item: TsmxParam;
begin
  Result := 0;
  Item := LibraryList.FindByName(LibName);
  if Assigned(Item) then
    Result := THandle(Item.ParamValue);
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
  if Assigned(FDataEntityList) then
  begin
    ClearDataEntities;
    FDataEntityList.Free;
  end;
  inherited Destroy;
end;

procedure TsmxDatabaseManager.ClearDataEntities;
var
  i: Integer;
begin
  for i := DataEntityList.Count - 1 downto 0 do
    //if not ((DataEntityList[i] as IsmxDataEntity).GetReference is TsmxInterfacedComponent)
        //or IsImplIntf((DataEntityList[i] as IsmxDataEntity).GetController) then
    if not (DataEntityList[i] as IsmxDataEntity).IsInterfacedObj
        or IsImplIntf((DataEntityList[i] as IsmxDataEntity).GetController) then
    begin
      (DataEntityList[i] as IsmxDataEntity).GetReference.Free;
      //DataEntityList[i] := nil;
    end
    //if IsImplIntf((DataEntityList[i] as IsmxDataEntity).GetController) then
      //(DataEntityList[i] as IsmxDataEntity).GetReference.Free
    else
      DataEntityList[i] := nil;
    //(DatabaseList[i] as IsmxDatabase).GetReference.Free;
end;

function TsmxDatabaseManager.FindByName(const DataEntityName: String): IsmxDataEntity;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to DataEntityList.Count - 1 do
    if SysUtils.AnsiCompareText((DataEntityList[i] as IsmxDataEntity).DataEntityName, DataEntityName) = 0 then
    begin
      Result := DataEntityList[i] as IsmxDataEntity;
      Break;
    end;
end;

function TsmxDatabaseManager.GetDataEntity(Index: Integer): IsmxDataEntity;
begin
  Result := DataEntityList[Index] as IsmxDataEntity;
end;

function TsmxDatabaseManager.GetDataEntityCount: Integer;
begin
  Result := DataEntityList.Count;
end;

function TsmxDatabaseManager.GetDataEntityList: TInterfaceList;
begin
  if not Assigned(FDataEntityList) then
    FDataEntityList := TInterfaceList.Create;
  Result := FDataEntityList;
end;

procedure TsmxDatabaseManager.InsertDataEntity(const DataEntity: IsmxDataEntity);
begin
  if Assigned(DataEntity) then
    if DataEntityList.IndexOf(DataEntity) = -1 then
    begin
      DataEntityList.Add(DataEntity);
      DataEntity.ChangeDatabaseManager(Self);
    end;
end;

procedure TsmxDatabaseManager.RemoveDataEntity(const DataEntity: IsmxDataEntity);
begin
  if Assigned(DataEntity) then
    if DataEntityList.IndexOf(DataEntity) <> -1 then
    begin
      DataEntityList.Remove(DataEntity);
      DataEntity.ChangeDatabaseManager(nil);
    end;
end;

{ TsmxFormManager }

destructor TsmxFormManager.Destroy;
begin
  if Assigned(FFormControlList) then
  begin
    ClearFormControls;
    FFormControlList.Free;
  end;
  inherited Destroy;
end;

procedure TsmxFormManager.ClearFormControls;
var
  i: Integer;
begin
  for i := FormControlList.Count - 1 downto 0 do
    //if not ((FormControlList[i] as IsmxFormControl).GetReference is TsmxInterfacedComponent)
    //    or IsImplIntf((FormControlList[i] as IsmxFormControl).GetController) then
    if not (FormControlList[i] as IsmxFormControl).IsInterfacedObj
        or IsImplIntf((FormControlList[i] as IsmxFormControl).GetController) then
      (FormControlList[i] as IsmxFormControl).GetReference.Free
    else
      FormControlList[i] := nil;
end;

function TsmxFormManager.FindByComboID(CfgID: Integer; ID: Integer = 0): IsmxFormControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FormControlList.Count - 1 do
    if ((FormControlList[i] as IsmxFormControl).CfgID = CfgID) and ((FormControlList[i] as IsmxFormControl).ID = ID) then
    begin
      Result := FormControlList[i] as IsmxFormControl;
      Break;
    end;
end;

function TsmxFormManager.GetFormControl(Index: Integer): IsmxFormControl;
begin
  Result := FormControlList[Index] as IsmxFormControl;
end;

function TsmxFormManager.GetFormControlCount: Integer;
begin
  Result := FormControlList.Count;
end;

function TsmxFormManager.GetFormControlList: TInterfaceList;
begin
  if not Assigned(FFormControlList) then
    FFormControlList := TInterfaceList.Create;
  Result := FFormControlList;
end;

procedure TsmxFormManager.InsertFormControl(const FormControl: IsmxFormControl);
begin
  if FormControlList.IndexOf(FormControl) = -1 then
  begin
    FormControlList.Add(FormControl);
    FormControl.ChangeFormManager(Self);
  end;
end;

procedure TsmxFormManager.RemoveFormControl(const FormControl: IsmxFormControl);
begin
  if FormControlList.IndexOf(FormControl) <> -1 then
  begin
    FormControlList.Remove(FormControl);
    FormControl.ChangeFormManager(nil);
  end;
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

{function TsmxImageListManager.AddImageList(const ImageListName: String): Integer;
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
end;}

procedure TsmxImageListManager.ClearImageListRegister;
var
  i: Integer;
begin
  for i := ImageListRegister.Count - 1 downto 0 do
    if Assigned(TCustomImageList(VarToInt(ImageListRegister[i].ParamValue))) then
    begin
      TCustomImageList(Integer(ImageListRegister[i].ParamValue)).Free;
      ImageListRegister.Delete(i);
    end;
end;

function TsmxImageListManager.FindByName(const ImageListName: String): TCustomImageList;
var
  //i: Integer;
  Item: TsmxParam;
begin
  Result := nil;
  Item := ImageListRegister.FindByName(ImageListName);
  if Assigned(Item) then
    Result := TCustomImageList(VarToInt(Item.ParamValue));
  {for i := 0 to ImageListRegister.Count - 1 do
    if SysUtils.AnsiCompareText(ImageListRegister[i].ParamName, ImageListName) = 0 then
    begin
      Result := TCustomImageList(Integer(ImageListRegister[i].ParamValue));
      Break;
    end;}
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
  Result := TCustomImageList(VarToInt(ImageListRegister[Index].ParamValue));
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
    if TCustomImageList(VarToInt(ImageListRegister[i].ParamValue)) = ImageList then
    begin
      Result := i;
      Exit;
    end;
end;

{procedure TsmxImageListManager.InsertImageList(ImageList: TCustomImageList);
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
end;}

function TsmxImageListManager.LoadImageList(const ImageListName: String): TCustomImageList;
var
  ResName, LibName: String;
  rs: TResourceStream;
  i: Integer;
  Handle: Longword;
begin
  Result := nil;
  smxProcs.SplitByDelimiter(ImageListName, FDelimiter, ResName, LibName);
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

procedure TsmxImageListManager.RegisterImageListName(const ImageListName: String; ImageList: TCustomImageList = nil);
var
  Item: TsmxParam;
begin
  Item := ImageListRegister.FindByName(ImageListName);
  if not Assigned(Item) then
  begin
    Item := ImageListRegister.Add;
    Item.ParamName := ImageListName;
    if Assigned(ImageList) then
      Item.ParamValue := Integer(ImageList);
  end;
end;

procedure TsmxImageListManager.UnRegisterImageListName(const ImageListName: String);
var
  Item: TsmxParam;
begin
  Item := ImageListRegister.FindByName(ImageListName);
  if Assigned(Item) then
    ImageListRegister.Delete(Item.ItemIndex);
end;

function TsmxImageListManager.ResolvedImageList(ImageList: TCustomImageList): String;
var
  i: Integer;
begin
  Result := '';
  i := IndexOf(ImageList);
  if i <> -1 then
    Result := ImageListRegister[i].ParamName;
end;

function TsmxImageListManager.ResolvedImageListName(const ImageListName: String;
  IsRegister: Boolean): TCustomImageList;
var
  Item: TsmxParam;
begin
  Result := nil;
  Item := ImageListRegister.FindByName(ImageListName);
  if not Assigned(Item) and IsRegister then
  begin
    Item := ImageListRegister.Add;
    Item.ParamName := ImageListName;
  end;
  if Assigned(Item) then
  begin
    if Variants.VarIsNull(Item.ParamValue) then
      Item.ParamValue := Integer(LoadImageList(ImageListName));
    Result := TCustomImageList(Integer(Item.ParamValue));
  end;
end;

function TsmxImageListManager.VarToInt(const Value: Variant): Integer;
begin
  if Variants.VarIsNull(Value) then
    Result := 0
  else
    Result := Value;
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

{function TsmxClassTypeManager.AddClassType(const ClassTypeName: String): Integer;
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
end;}

procedure TsmxClassTypeManager.ClearClassTypeList;
//var
  //i: Integer;
begin
  ClassTypeList.Clear;
  //for i := ClassTypeList.Count - 1 downto 0 do
  //begin
    //TCustomImageList(Integer(ImageListRegister[i].ParamValue)).Free;
    //ClassTypeList.Delete(i);
  //end;
end;

function TsmxClassTypeManager.FindByName(const ClassTypeName: String): TPersistentClass;
var
  Item: TsmxParam;
  //i: Integer;
begin
  Result := nil;
  Item := ClassTypeList.FindByName(ClassTypeName);
  if Assigned(Item) then
    Result := TPersistentClass(VarToInt(Item.ParamValue));
  {for i := 0 to ClassTypeList.Count - 1 do
    if SysUtils.AnsiCompareText(ClassTypeList[i].ParamName, ClassTypeName) = 0 then
    begin
      Result := TPersistentClass(Integer(ClassTypeList[i].ParamValue));
      Break;
    end;}
end;

function TsmxClassTypeManager.GetClassType(Index: Integer): TPersistentClass;
begin
  Result := TPersistentClass(VarToInt(ClassTypeList[Index].ParamValue));
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
    if TPersistentClass(VarToInt(ClassTypeList[i].ParamValue)) = ClassType then
    begin
      Result := i;
      Exit;
    end;
end;

{procedure TsmxClassTypeManager.InsertClassType(ClassType: TPersistentClass);
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
end;}

function TsmxClassTypeManager.LoadClassType(const ClassTypeName: String): TPersistentClass;
var
  ClassName, LibName: String;
  i: Integer;
  Handle: Longword;
begin
  Result := nil;
  smxProcs.SplitByDelimiter(ClassTypeName, FDelimiter, ClassName, LibName);
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

procedure TsmxClassTypeManager.RegisterClassTypeName(const ClassTypeName: String;
  ClassType: TPersistentClass = nil);
var
  Item: TsmxParam;
begin
  Item := ClassTypeList.FindByName(ClassTypeName);
  if not Assigned(Item) then
  begin
    Item := ClassTypeList.Add;
    Item.ParamName := ClassTypeName;
    if Assigned(ClassType) then
      Item.ParamValue := Integer(ClassType);
  end;
end;

procedure TsmxClassTypeManager.UnRegisterClassTypeName(const ClassTypeName: String);
var
  Item: TsmxParam;
begin
  Item := ClassTypeList.FindByName(ClassTypeName);
  if Assigned(Item) then
    ClassTypeList.Delete(Item.ItemIndex);
end;

function TsmxClassTypeManager.ResolvedClassType(ClassType: TPersistentClass): String;
var
  i: Integer;
begin
  Result := '';
  i := IndexOf(ClassType);
  if i <> -1 then
    Result := ClassTypeList[i].ParamName;
end;

function TsmxClassTypeManager.ResolvedClassTypeName(const ClassTypeName: String;
  IsRegister: Boolean = False): TPersistentClass;
var
  Item: TsmxParam;
begin
  Result := nil;
  Item := ClassTypeList.FindByName(ClassTypeName);
  if not Assigned(Item) and IsRegister then
  begin
    Item := ClassTypeList.Add;
    Item.ParamName := ClassTypeName;
  end;
  if Assigned(Item) then
  begin
    if Variants.VarIsNull(Item.ParamValue) then
      Item.ParamValue := Integer(LoadClassType(ClassTypeName));
      //Result := LoadClassType(ClassTypeName)
    //else
    Result := TPersistentClass(Integer(Item.ParamValue));
    //if not Assigned(Result) then
      //Result := LoadClassType(ClassTypeName);
  end;
end;

function TsmxClassTypeManager.VarToInt(const Value: Variant): Integer;
begin
  if Variants.VarIsNull(Value) then
    Result := 0
  else
    Result := Value;
end;

//initialization
  //smxProcs{Classes}.RegisterClasses([TsmxProjectManager, TsmxCallBackManager,
    //TsmxStorageManager, TsmxLibraryManager, TsmxDatabaseManager,
    //TsmxFormManager, TsmxImageListManager, TsmxClassTypeManager]);

//finalization
  //Classes.UnRegisterClasses([TsmxProjectManager, TsmxCallBackManager,
    //TsmxStorageManager, TsmxLibraryManager, TsmxDatabaseManager,
    //TsmxFormManager, TsmxImageListManager, TsmxClassTypeManager]);

end.
