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
    function GetCheckHandle: LongWord;
    function GetIsCheckComp: Boolean;
    function GetLibInfoProcName: String;
    function GetLibPath: String;
    function GetLibrary(Index: Integer): THandle;
    function GetLibraryCount: Integer;
    function GetLibraryName(Index: Integer): String;
    function IndexOf(LibHandle: THandle): Integer;
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
    function InsertLibrary(LibHandle: THandle): Integer;
    procedure RemoveLibrary(LibHandle: THandle);

    property CheckHandle: LongWord read GetCheckHandle write SetCheckHandle;
    property IsCheckComp: Boolean read GetIsCheckComp write SetIsCheckComp;
    property LibPath: String read GetLibPath write SetLibPath;
    property LibInfoProcName: String read GetLibInfoProcName write SetLibInfoProcName;
    property Libraries[Index: Integer]: THandle read GetLibrary; default;
    property LibraryCount: Integer read GetLibraryCount;
    property LibraryNames[Index: Integer]: String read GetLibraryName;
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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
    procedure ClearImageListRegister;
    function GetImageListRegister: TsmxParams;
    function LoadImageList(const ImageListName: String): TCustomImageList;
  protected
    function GetDelimiter: String;
    function GetImageListCount: Integer;
    function GetImageList(Index: Integer): TCustomImageList;
    function GetImageListName(Index: Integer): String;
    function IndexOf(ImageList: TCustomImageList): Integer;
    procedure SetDelimiter(const Value: String);

    property ImageListRegister: TsmxParams read GetImageListRegister;
  public
    destructor Destroy; override;
    function FindByName(const ImageListName: String): TCustomImageList;
    procedure RegisterImageListName(const ImageListName: String; ImageList: TCustomImageList = nil);
    function ResolvedImageList(ImageList: TCustomImageList): String;
    function ResolvedImageListName(const ImageListName: String; IsRegister: Boolean = False): TCustomImageList;
    procedure UnRegisterImageListName(const ImageListName: String);

    property Delimiter: String read GetDelimiter write SetDelimiter;
    property ImageListCount: Integer read GetImageListCount;
    property ImageListNames[Index: Integer]: String read GetImageListName;
    property ImageLists[Index: Integer]: TCustomImageList read GetImageList; default;
  end;

  { TsmxClassTypeManager }

  TsmxClassTypeManager = class(TsmxComponent, IsmxClassTypeManager)
  private
    FDelimiter: String;
    FClassTypeList: TsmxParams;
    procedure ClearClassTypeList;
    function GetClassTypeList: TsmxParams;
    function LoadClassType(const ClassTypeName: String): TPersistentClass;
  protected
    function GetDelimiter: String;
    function GetClassType(Index: Integer): TPersistentClass;
    function GetClassTypeCount: Integer;
    function GetClassTypeName(Index: Integer): String;
    function IndexOf(ClassType: TPersistentClass): Integer;
    procedure SetDelimiter(const Value: String);

    property ClassTypeList: TsmxParams read GetClassTypeList;
  public
    destructor Destroy; override;
    function FindByName(const ClassTypeName: String): TPersistentClass;
    procedure RegisterClassTypeName(const ClassTypeName: String; ClassType: TPersistentClass = nil);
    function ResolvedClassType(ClassType: TPersistentClass): String;
    function ResolvedClassTypeName(const ClassTypeName: String; IsRegister: Boolean = False): TPersistentClass;
    procedure UnRegisterClassTypeName(const ClassTypeName: String);

    property Delimiter: String read GetDelimiter write SetDelimiter;
    property ClassTypeCount: Integer read GetClassTypeCount;
    property ClassTypeNames[Index: Integer]: String read GetClassTypeName;
    property ClassTypes[Index: Integer]: TPersistentClass read GetClassType; default;
  end;

implementation

uses
  DB, Windows, Controls, SysUtils, Variants, smxProcs, smxFuncs, smxConsts;

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

function TsmxLibraryManager.GetLibraryName(Index: Integer): String;
begin
  Result := LibraryList[Index].ParamName;
end;

function TsmxLibraryManager.GetProcedure(LibHandle: THandle; const ProcName: String): Pointer;
begin
  if (LibHandle > Windows.HINSTANCE_ERROR) and (ProcName <> '') then
    Result := Windows.GetProcAddress(LibHandle, PChar(ProcName))
  else
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
      Result := GetProcedure(THandle(LibraryList[Index].ParamValue), ProcName)
    else
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
    ProcInitLib := li.ProcInitLib
  else
    ProcInitLib := nil;
  if Assigned(ProcInitLib) then
  begin
    if Assigned(smxProcs.gCallBackManagerIntf) then
      FuncCallBack := smxProcs.gCallBackManagerIntf.GetFuncCallBack
    else
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
  DataEntity: IsmxDataEntity;
  Component: TComponent;
begin
  for i := DataEntityList.Count - 1 downto 0 do
  begin
    DataEntity := DataEntityList[i] as IsmxDataEntity;
    if not DataEntity.IsCountedObj then
    begin
      Component := DataEntity.GetReference;
      DataEntityList.Delete(i);
      DataEntity := nil;
      Component.Free;
    end else
    begin
      DataEntityList.Delete(i);
      DataEntity := nil;
    end;
  end;
end;

function TsmxDatabaseManager.FindByName(const DataEntityName: String): IsmxDataEntity;
var
  i: Integer;
  DataEntity: IsmxDataEntity;
begin
  Result := nil;
  for i := 0 to DataEntityList.Count - 1 do
  begin
    DataEntity := DataEntityList[i] as IsmxDataEntity;
    if SysUtils.AnsiCompareText(DataEntity.DataEntityName, DataEntityName) = 0 then
    begin
      Result := DataEntity;
      Break;
    end;
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
      if not DataEntity.IsCountedObj then
        DataEntity.GetReference.FreeNotification(Self);
    end;
end;

procedure TsmxDatabaseManager.RemoveDataEntity(const DataEntity: IsmxDataEntity);
begin
  if Assigned(DataEntity) then
    if DataEntityList.IndexOf(DataEntity) <> -1 then
    begin
      if not DataEntity.IsCountedObj then
        DataEntity.GetReference.RemoveFreeNotification(Self);
      DataEntityList.Remove(DataEntity);
    end;
end;

procedure TsmxDatabaseManager.Notification(AComponent: TComponent; Operation: TOperation);
var
  DataEntity: IsmxDataEntity;
begin
  inherited Notification(AComponent, Operation);
  if SysUtils.Supports(AComponent, IsmxDataEntity, DataEntity)
      and not DataEntity.IsCountedObj
      and (DataEntityList.IndexOf(DataEntity) <> -1)
      and (Operation = opRemove) then
    DataEntityList.Remove(DataEntity);
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
  FormControl: IsmxFormControl;
  Component: TComponent;
begin
  for i := FormControlList.Count - 1 downto 0 do
  begin
    FormControl := FormControlList[i] as IsmxFormControl;
    if not FormControl.IsCountedObj then
    begin
      Component := FormControl.GetReference;
      FormControlList.Delete(i);
      FormControl := nil;
      Component.Free;
    end else
    begin
      FormControlList.Delete(i);
      FormControl := nil;
    end;
  end;
end;

function TsmxFormManager.FindByComboID(CfgID: Integer; ID: Integer = 0): IsmxFormControl;
var
  i: Integer;
  FormControl: IsmxFormControl;
begin
  Result := nil;
  for i := 0 to FormControlList.Count - 1 do
  begin
    FormControl := FormControlList[i] as IsmxFormControl;
    if (FormControl.CfgID = CfgID) and (FormControl.ID = ID) then
    begin
      Result := FormControl;
      Break;
    end;
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
  if Assigned(FormControl) then
    if FormControlList.IndexOf(FormControl) = -1 then
    begin
      FormControlList.Add(FormControl);
      if not FormControl.IsCountedObj then
        FormControl.GetReference.FreeNotification(Self);
    end;
end;

procedure TsmxFormManager.RemoveFormControl(const FormControl: IsmxFormControl);
begin
  if Assigned(FormControl) then
    if FormControlList.IndexOf(FormControl) <> -1 then
    begin
      if not FormControl.IsCountedObj then
        FormControl.GetReference.RemoveFreeNotification(Self);
      FormControlList.Remove(FormControl);
    end;
end;

procedure TsmxFormManager.Notification(AComponent: TComponent; Operation: TOperation);
var
  FormControl: IsmxFormControl;
begin
  inherited Notification(AComponent, Operation);
  if SysUtils.Supports(AComponent, IsmxFormControl, FormControl)
      and not FormControl.IsCountedObj
      and (FormControlList.IndexOf(FormControl) <> -1)
      and (Operation = opRemove) then
    FormControlList.Remove(FormControl);
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

procedure TsmxImageListManager.ClearImageListRegister;
var
  i: Integer;
begin
  for i := ImageListRegister.Count - 1 downto 0 do
    if Assigned(TCustomImageList(smxFuncs.VarToInt(ImageListRegister[i].ParamValue))) then
    begin
      TCustomImageList(Integer(ImageListRegister[i].ParamValue)).Free;
      ImageListRegister.Delete(i);
    end;
end;

function TsmxImageListManager.FindByName(const ImageListName: String): TCustomImageList;
var
  Item: TsmxParam;
begin
  Result := nil;
  Item := ImageListRegister.FindByName(ImageListName);
  if Assigned(Item) then
    Result := TCustomImageList(smxFuncs.VarToInt(Item.ParamValue));
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
  Result := TCustomImageList(smxFuncs.VarToInt(ImageListRegister[Index].ParamValue));
end;

function TsmxImageListManager.GetImageListCount: Integer;
begin
  Result := ImageListRegister.Count;
end;

function TsmxImageListManager.GetImageListName(Index: Integer): String;
begin
  Result := ImageListRegister[Index].ParamName;
end;

function TsmxImageListManager.GetImageListRegister: TsmxParams;
begin
  if not Assigned(FImageListRegister) then
    FImageListRegister := TsmxParams.Create(TsmxParam);
  Result := FImageListRegister;
end;

function TsmxImageListManager.IndexOf(ImageList: TCustomImageList): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to ImageListRegister.Count - 1 do
    if TCustomImageList(smxFuncs.VarToInt(ImageListRegister[i].ParamValue)) = ImageList then
    begin
      Result := i;
      Exit;
    end;
end;

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
  IsRegister: Boolean = False): TCustomImageList;
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

procedure TsmxClassTypeManager.ClearClassTypeList;
begin
  ClassTypeList.Clear;
end;

function TsmxClassTypeManager.FindByName(const ClassTypeName: String): TPersistentClass;
var
  Item: TsmxParam;
begin
  Result := nil;
  Item := ClassTypeList.FindByName(ClassTypeName);
  if Assigned(Item) then
    Result := TPersistentClass(smxFuncs.VarToInt(Item.ParamValue));
end;

function TsmxClassTypeManager.GetClassType(Index: Integer): TPersistentClass;
begin
  Result := TPersistentClass(smxFuncs.VarToInt(ClassTypeList[Index].ParamValue));
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

function TsmxClassTypeManager.GetClassTypeName(Index: Integer): String;
begin
  Result := ClassTypeList[Index].ParamName;
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
    if TPersistentClass(smxFuncs.VarToInt(ClassTypeList[i].ParamValue)) = ClassType then
    begin
      Result := i;
      Exit;
    end;
end;

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
    Result := TPersistentClass(Integer(Item.ParamValue));
  end;
end;

end.
