unit smxLibManager;

interface

uses
  Classes, SysUtils, smxBaseClasses, smxClasses, smxTypes, smxLibTypes;

type
  { TsmxLibItem }

  TsmxLibItem = class(TsmxKitItem)
  private
    FLibHandle: THandle;
    FLibName: String;
    //FLibInfo: TsmxLibInfo;
  public
    constructor Create(AKit: TsmxKit); override;

    property LibHandle: THandle read FLibHandle write FLibHandle;
    property LibName: String read FLibName write FLibName;
    //property LibInfo: TsmxLibInfo read FLibInfo write FLibInfo;
  end;

  { TsmxLibItems }

  TsmxLibItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxLibItem;
  public
    function Add: TsmxLibItem;
    function FindByName(ALibName: String): TsmxLibItem;
    //function FindByHandle(ALibHandle: THandle): TsmxLibItem;

    property Items[Index: Integer]: TsmxLibItem read GetItem; default;
  end;

  { TsmxLibManager }

  //EsmxLibManagerError = class(Exception);

  TsmxLibManager = class(TsmxComponent)
  private
    FLibList: TsmxLibItems;
    //function GetLibrary(Name: String): THandle;
    function GetLib(Index: Integer): THandle;
    function GetLibCount: Integer;
    procedure FreeLibs;
  protected
    function PrepareLibrary(ALibName: String): THandle;
    //function PrepareLibrary(ALibName: String; var ALibHandle: THandle): Boolean;
    //function PrepareLibrary(ALibHandle: THandle): Boolean;
    procedure ClearLibInfo(var ALibInfo: TsmxLibInfo);
    function GetLibHandle(ALibName: String): THandle;
    
    //procedure UnPrepareLibraty(ALibItem: TsmxLibItem);
    //function GetLibInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean;

    property LibList: TsmxLibItems read FLibList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //function InsertLibrary(ALibName: String): THandle;
    //procedure RemoveLibrary(ALibName: String);

    function FindByName(ALibName: String): THandle;

    //function FindLibInfoByName(ALibName: String): TsmxLibInfo;
    //function FindLibInfoByHandle(ALibHandle: THandle): TsmxLibInfo;

    function CheckLibraryComp(ALibHandle: THandle): Boolean; overload;
    function CheckLibraryComp(ALibName: String): Boolean; overload;
    function GetProcedure(ALibHandle: THandle; AProcName: String): Pointer; overload;
    function GetProcedure(ALibName, AProcName: String): Pointer; overload;
    function GetLibrary(ALibName: String): THandle;
    function GetLibraryInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean; overload;
    function GetLibraryInfo(ALibName: String; var ALibInfo: TsmxLibInfo): Boolean; overload;

    //property Libraries[Name: String]: THandle read GetLibrary; default;
    property LibraryCount: Integer read GetLibCount;
    property Libraries[Index: Integer]: THandle read GetLib; default;
  end;

function LibManager: TsmxLibManager;

implementation

uses
  Windows, smxCommonStorage, smxCallBack, smxConsts;

var
  _LibManager: TsmxLibManager = nil;

function LibManager: TsmxLibManager;
begin
  Result := _LibManager;
end;

{ TsmxLibItem }

constructor TsmxLibItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FLibHandle := 0;
  FLibName := '';
  //FLibRefCount := 0;
end;

{ TsmxLibItems }

function TsmxLibItems.Add: TsmxLibItem;
begin
  Result := TsmxLibItem(inherited Add);
end;

function TsmxLibItems.FindByName(ALibName: String): TsmxLibItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].LibName, ALibName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

{function TsmxLibItems.FindByHandle(ALibHandle: THandle): TsmxLibItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].LibHandle = ALibHandle then
    begin
      Result := Items[i];
      Break;
    end;
end;}

function TsmxLibItems.GetItem(Index: Integer): TsmxLibItem;
begin
  Result := TsmxLibItem(inherited Items[Index]);
end;

{ TsmxLibManager }

constructor TsmxLibManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibList := TsmxLibItems.Create(TsmxLibItem);
end;

destructor TsmxLibManager.Destroy;
begin
  FreeLibs;
  FLibList.Free;
  inherited Destroy;
end;

procedure TsmxLibManager.FreeLibs;
var i: Integer; h: THandle;
begin
  for i := FLibList.Count - 1 downto 0 do
  begin
    h := FLibList[i].LibHandle;
    if h > 0 then
      FreeLibrary(h);
  end;
end;

procedure TsmxLibManager.ClearLibInfo(var ALibInfo: TsmxLibInfo);
begin
  with ALibInfo do
  begin
    FullName := '';
    Description := '';
    with LibVers do
    begin
      Major := 0;
      Minor := 0;
      Release := 0;
      Build := 0;
    end;
    LibTypes := [];
    with CompProgVers do
    begin
      Major := 0;
      Minor := 0;
      Release := 0;
      Build := 0;
    end;
    ProcInitLib := nil;
  end;
end;

{function TsmxLibManager.GetLibrary(Name: String): THandle;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(Name);
  if Assigned(l) then
    Result := l.LibHandle else
    Result := PrepareLibrary(Name);
end;}

function TsmxLibManager.FindByName(ALibName: String): THandle;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := l.LibHandle else
    Result := 0;
end;

function TsmxLibManager.GetLib(Index: Integer): THandle;
begin
  Result := TsmxLibItem(FLibList[Index]).LibHandle;
end;

function TsmxLibManager.GetLibCount: Integer;
begin
  Result := FLibList.Count;
end;

function TsmxLibManager.GetLibrary(ALibName: String): THandle;
var l: TsmxLibItem; //pc: TsmxProjectConnection; fs: TFileStream;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := l.LibHandle
  else
  begin
    Result := PrepareLibrary(ALibName);
    if Result > 0 then
      with FLibList.Add do
      begin
        LibHandle := Result;
        LibName := ALibName;
      end;
  end;

  {with pc do
  begin
    ProjectName := 'CRM';
    LibraryName := 'dADO';
    ProcedureName := 'NewADODatabase';
    WindowsAuthorization := True;
    DatabaseName := 'Base';
    DriverName := '';
    LoginPrompt := False;
    Params := 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Initial Catalog=crm;Data Source=localhost;';
  end;}
  //fs := TFileStream.Create(SFileProjectName, fmOpenRead);
  //fs := TFileStream.Create(SFileProjectName, fmCreate);
  //try
    //fs.ReadBuffer(pc, SizeOf(TsmxProjectConnection));
    //fs.WriteBuffer(pc, SizeOf(TsmxProjectConnection));
  //finally
    //fs.Free;
  //end;

  {else
  begin
    Result := LoadLibrary(PChar(ALibName));
    if Result > 0 then
    begin
      if PrepareLibrary(Result) then
      begin
        with FLibList.Add do
        begin
          LibHandle := Result;
          LibName := ALibName;
        end;
      end else
      begin
        FreeLibrary(Result);
        Result := 0;
      end;
    end; 
  end;}

  {else
  begin
    Result := LoadLibrary(PChar(ALibName));
    if PrepareLibrary(Result) then


    Result := PrepareLibrary(ALibName);
    if Result > 0 then
      with FLibList.Add do
      begin
        LibHandle := Result;
        LibName := ALibName;
      end;
  end;}
end;

function TsmxLibManager.GetLibraryInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean;
var ProcLibInfo: TsmxProcLibInfo;
begin
  Result := False;
  ClearLibInfo(ALibInfo);
  if ALibHandle > 0 then
  begin
    @ProcLibInfo := GetProcAddress(ALibHandle, PChar(SProcLibInfoName));
    if Assigned(ProcLibInfo) then
    begin
      ProcLibInfo(ALibInfo);
      Result := True;
    end;
  end;
end;

function TsmxLibManager.GetLibraryInfo(ALibName: String; var ALibInfo: TsmxLibInfo): Boolean;
var l: TsmxLibItem; h: THandle;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := GetLibraryInfo(l.LibHandle, ALibInfo)
  else
  begin
    h := GetLibHandle(ALibName);
    Result := GetLibraryInfo(h, ALibInfo);
    if h > 0 then
      FreeLibrary(h);
  end;
end;

function TsmxLibManager.GetProcedure(ALibHandle: THandle; AProcName: String): Pointer;
begin
  if ALibHandle > 0 then
    Result := GetProcAddress(ALibHandle, PChar(AProcName)) else
    Result := nil;
end;

function TsmxLibManager.GetProcedure(ALibName, AProcName: String): Pointer;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := GetProcedure(l.LibHandle, AProcName) else
    Result := GetProcedure(GetLibrary(ALibName), AProcName);
end;

{function TsmxLibManager.InsertLibrary(ALibName: String): THandle;
var l: TsmxLibItem; li: TsmxLibInfo;
begin
  Result := 0;
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
  begin
    Result := l.LibHandle;
    l.IncRef;
  end else
  begin
    Result := PrepareLibrary(ALibName);
    if (Result > 0) and GetLibInfo(Result, li) then
    begin
      l := FLibList.Add;
      with l do
      begin
        LibHandle := Result;
        LibName := ALibName;
        LibInfo := li;
      end;
      l.IncRef;
    end;
  end;
end;}

{procedure TsmxLibManager.RemoveLibrary(ALibName: String);
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
  begin
    l.DecRef;
    if l.LibRefCount = 0 then
    begin
      UnPrepareLibrary(l.LibHandle);
      FLibList.Remove(l);
    end;
  end;
end;}

{procedure TsmxLibManager.UnPrepareLibrary(ALibHandle: THandle);
var h: THandle;
begin
  if ALibHandle > 0 then
    FreeLibrary(ALibHandle);
end;}

{function TsmxLibManager.GetLibInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean;
var ProcLibInfo: TsmxProcLibInfo;
begin
  Result := False;
  if ALibHandle > 0 then
  begin
    @ProcLibInfo := GetProcAddress(ALibHandle, PChar(SProcLibInfoName));
    if Assigned(ProcLibInfo) then
    begin
      ProcLibInfo(ALibInfo);
      Result := True;
    end;
  end;
end;}

{function TsmxLibManager.PrepareLibrary(ALibName: String; var ALibHandle: THandle): Boolean;
var li: TsmxLibInfo; ProcInitLib: TsmxProcInitLib;
begin
  Result := False;
  ALibHandle := GetLibHandle(ALibName);
  if ALibHandle > 0 then
    if CheckLibraryComp(ALibHandle) then
    begin
      if GetLibraryInfo(ALibHandle, li) then
        ProcInitLib := li.ProcInitLib else
        ProcInitLib := nil;
      if Assigned(ProcInitLib) then
        ProcInitLib(FuncCallBack);
      Result := True;
    end else
    begin
      FreeLibrary(ALibHandle);
      ALibHandle := 0;
    end;
end;}

function TsmxLibManager.PrepareLibrary(ALibName: String): THandle;
var li: TsmxLibInfo; ProcInitLib: TsmxProcInitLib;
begin
  Result := GetLibHandle(ALibName);
  if Result > 0 then
    if CheckLibraryComp(Result) then
    begin
      if GetLibraryInfo(Result, li) then
        ProcInitLib := li.ProcInitLib else
        ProcInitLib := nil;
      if Assigned(ProcInitLib) then
        ProcInitLib(FuncCallBack);
    end else
    begin
      FreeLibrary(Result);
      Result := 0;
    end;
end;

function TsmxLibManager.CheckLibraryComp(ALibHandle: THandle): Boolean;

  function CheckComp(ACompProgVers: TsmxVers): Boolean;
  var ProgVersMajor, ProgVersMinor: Word;
  begin
    Result := False;
    ProgVersMajor := CommonStorage['ProgVersMajor'];
    ProgVersMinor := CommonStorage['ProgVersMinor'];
    if (ACompProgVers.Major > ProgVersMajor)
        or ((ACompProgVers.Major = ProgVersMajor)
          and (ACompProgVers.Minor >= ProgVersMinor)) then
      Result := True;
  end;

var li: TsmxLibInfo;
begin
  Result := False;
  if ALibHandle > 0 then
    if GetLibraryInfo(ALibHandle, li) then
      Result := CheckComp(li.CompProgVers);
end;

function TsmxLibManager.CheckLibraryComp(ALibName: String): Boolean;
var l: TsmxLibItem; h: THandle;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := CheckLibraryComp(l.LibHandle)
  else
  begin
    h := GetLibHandle(ALibName);
    Result := CheckLibraryComp(h);
    if h > 0 then
      FreeLibrary(h);
  end;
end;

function TsmxLibManager.GetLibHandle(ALibName: String): THandle;
begin
  Result := LoadLibrary(PChar(ALibName));
  if Result <= HINSTANCE_ERROR then
    Result := 0;
end;

{function TsmxLibManager.PrepareLibrary(ALibHandle: THandle): Boolean;

  function CheckComp(ACompProgVers: TsmxVers): Boolean;
  var ProgVersMajor, ProgVersMinor: Word;
  begin
    Result := False;
    ProgVersMajor := GlobalStorage['ProgVersMajor'];
    ProgVersMinor := GlobalStorage['ProgVersMinor'];
    if (ACompProgVers.Major > ProgVersMajor)
        or ((ACompProgVers.Major = ProgVersMajor)
          and (ACompProgVers.Minor >= ProgVersMinor)) then
      Result := True;
  end;

var li: TsmxLibInfo; ProcLibInit: TsmxProcLibInit;
begin
  Result := False;
  if GetLibraryInfo(ALibHandle, li) then
    Result := CheckComp(li.CompProgVers);
  if Result then
  begin
    ProcLibInit := li.ProcLibInit;
    if Assigned(ProcLibInit) then
      ProcLibInit(FuncCallBack);
  end;
end;}

{function TsmxLibManager.PrepareLibrary(ALibName: String): THandle;

  function CheckComp(ACompProgVers: TsmxVers): Boolean;
  var ProgVersMajor, ProgVersMinor: Word;
  begin
    Result := False;
    ProgVersMajor := GlobalStorage['ProgVersMajor'];
    ProgVersMinor := GlobalStorage['ProgVersMinor'];
    if (ACompProgVers.Major > ProgVersMajor)
        or ((ACompProgVers.Major = ProgVersMajor)
          and (ACompProgVers.Minor >= ProgVersMinor)) then
      Result := True;
  end;

var ProcLibInfo: TsmxProcLibInfo; li: TsmxLibInfo; ProcLibInit: TsmxProcLibInit;
begin
  Result := LoadLibrary(PChar(ALibName));
  if Result > 0 then
  begin
    @ProcLibInfo := GetProcAddress(Result, PChar(SProcLibInfoName));
    if Assigned(ProcLibInfo) then
    begin
      ProcLibInfo(li);
      if CheckComp(li.CompProgVers) then
      begin
        ProcLibInit := li.ProcLibInit;
        if Assigned(ProcLibInit) then
          ProcLibInit(FuncCallBack);
        with FLibList.Add do
        begin
          LibHandle := Result;
          LibName := ALibName;
          LibInfo := li;
        end;
      end else
      begin
        FreeLibrary(Result);
        Result := 0;
      end;
    end else
    begin
      FreeLibrary(Result);
      Result := 0;
    end;
  end;
end;}

initialization
  _LibManager := TsmxLibManager.Create(nil);

finalization
  _LibManager.Free;

end.
