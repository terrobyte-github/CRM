{**************************************}
{                                      }
{            SalesMan v1.0             }
{       Library Manager classes        }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxLibManager;

interface

uses
  Classes, smxClasses, smxLibTypes;

type
  { TsmxLibItem }

  TsmxLibItem = class(TsmxKitItem)
  private
    FLibHandle: THandle;
    FLibName: String;
  public
    constructor Create(AKit: TsmxKit); override;

    property LibHandle: THandle read FLibHandle write FLibHandle;
    property LibName: String read FLibName write FLibName;
  end;

  { TsmxLibItems }

  TsmxLibItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxLibItem;
  public
    function Add: TsmxLibItem;
    function FindByName(ALibName: String): TsmxLibItem;

    property Items[Index: Integer]: TsmxLibItem read GetItem; default;
  end;

  { TsmxLibraryManager }

  TsmxLibraryManager = class(TsmxCustomLibraryManager)
  private
    FLibList: TsmxLibItems;
    FProgVersMajor: Word;
    FProgVersMinor: Word;
    //FLibPath: String;
    //FProcLibInfoName: String;
    function GetLibrary(Index: Integer): THandle;
    function GetLibraryCount: Integer;
    procedure DestroyLibs;
  protected
    function PrepareLibrary(ALibName: String): THandle;
    procedure ClearLibInfo(var ALibInfo: TsmxLibInfo);
    function GetLibHandle(ALibName: String): THandle;
    procedure FillProgVers;

    property LibList: TsmxLibItems read FLibList;
    property ProgVersMajor: Word read FProgVersMajor;
    property ProgVersMinor: Word read FProgVersMinor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByName(ALibName: String): THandle;
    function CheckLibraryComp(ALibHandle: THandle): Boolean; overload; override;
    function CheckLibraryComp(ALibName: String): Boolean; overload; override;
    function GetProcedure(ALibHandle: THandle; AProcName: String): Pointer; overload; override;
    function GetProcedure(ALibName, AProcName: String): Pointer; overload; override;
    function CallLibrary(ALibName: String): THandle; override;
    function GetLibraryInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean; overload;
    function GetLibraryInfo(ALibName: String; var ALibInfo: TsmxLibInfo): Boolean; overload;

    property LibraryCount: Integer read GetLibraryCount;
    property Libraries[Index: Integer]: THandle read GetLibrary; default;
    //property LibPath: String read FLibPath write FLibPath;
    //property ProcLibInfoName: String read FProcLibInfoName write FProcLibInfoName;
  end;

function LibManager: TsmxLibraryManager;
//function FindProcedureByName(ALibName, AProcName: String): Pointer;

implementation

uses
  Windows, SysUtils, smxCallBack, smxProcs, smxConsts;

var
  _LibManager: TsmxLibraryManager = nil;

function LibManager: TsmxLibraryManager;
begin
  Result := _LibManager;
end;

{function FindProcedureByName(ALibName, AProcName: String): Pointer;
begin
  Result := _LibManager.GetProcedure(ALibName, AProcName);
end;}

{ TsmxLibItem }

constructor TsmxLibItem.Create(AKit: TsmxKit);
begin
  inherited Create(AKit);
  FLibHandle := 0;
  FLibName := '';
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

function TsmxLibItems.GetItem(Index: Integer): TsmxLibItem;
begin
  Result := TsmxLibItem(inherited Items[Index]);
end;

{ TsmxLibraryManager }

constructor TsmxLibraryManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibList := TsmxLibItems.Create(TsmxLibItem);
  FillProgVers;
end;

destructor TsmxLibraryManager.Destroy;
begin
  DestroyLibs;
  FLibList.Free;
  inherited Destroy;
end;

procedure TsmxLibraryManager.DestroyLibs;
var i: Integer; h: THandle;
begin
  for i := FLibList.Count - 1 downto 0 do
  begin
    h := FLibList[i].LibHandle;
    if h > 0 then
      FreeLibrary(h);
  end;
end;

procedure TsmxLibraryManager.ClearLibInfo(var ALibInfo: TsmxLibInfo);
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

function TsmxLibraryManager.FindByName(ALibName: String): THandle;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := l.LibHandle else
    Result := 0;
end;

procedure TsmxLibraryManager.FillProgVers;
var VersM, VersL: Cardinal;
begin
  GetFileFullVersion(GetModuleName(HInstance), VersM, VersL);
  FProgVersMajor := LongRec(VersM).Hi;
  FProgVersMinor := LongRec(VersM).Lo;
end;

function TsmxLibraryManager.GetLibrary(Index: Integer): THandle;
begin
  Result := TsmxLibItem(FLibList[Index]).LibHandle;
end;

function TsmxLibraryManager.GetLibraryCount: Integer;
begin
  Result := FLibList.Count;
end;

function TsmxLibraryManager.CallLibrary(ALibName: String): THandle;
var l: TsmxLibItem;
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
end;

function TsmxLibraryManager.GetLibraryInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean;
var ProcLibInfo: TsmxProcLibInfo;
begin
  Result := False;
  ClearLibInfo(ALibInfo);
  //Finalize(ALibInfo);
  if ALibHandle > 0 then
  begin
    @ProcLibInfo := GetProcAddress(ALibHandle, PChar(ProcLibInfoName) {PChar(SProcLibInfoName)});
    if Assigned(ProcLibInfo) then
    begin
      ProcLibInfo(ALibInfo);
      Result := True;
    end;
  end;
end;

function TsmxLibraryManager.GetLibraryInfo(ALibName: String; var ALibInfo: TsmxLibInfo): Boolean;
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

function TsmxLibraryManager.GetProcedure(ALibHandle: THandle; AProcName: String): Pointer;
begin
  if ALibHandle > 0 then
    Result := GetProcAddress(ALibHandle, PChar(AProcName)) else
    Result := nil;
end;

function TsmxLibraryManager.GetProcedure(ALibName, AProcName: String): Pointer;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(ALibName);
  if Assigned(l) then
    Result := GetProcedure(l.LibHandle, AProcName) else
    Result := GetProcedure(CallLibrary(ALibName), AProcName);
end;

function TsmxLibraryManager.PrepareLibrary(ALibName: String): THandle;
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

function TsmxLibraryManager.CheckLibraryComp(ALibHandle: THandle): Boolean;
var li: TsmxLibInfo;
begin
  if CheckComp then
  begin
    Result := False;
    if ALibHandle > 0 then
      if GetLibraryInfo(ALibHandle, li) then
        if (li.CompProgVers.Major > FProgVersMajor)
            or ((li.CompProgVers.Major = FProgVersMajor)
              and (li.CompProgVers.Minor >= FProgVersMinor)) then
          Result := True;
  end else
    Result := True;
end;

function TsmxLibraryManager.CheckLibraryComp(ALibName: String): Boolean;
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

function TsmxLibraryManager.GetLibHandle(ALibName: String): THandle;
begin
  Result := LoadLibrary(PChar(LibPath + ALibName));
  if Result <= HINSTANCE_ERROR then
    Result := 0;
end;

initialization
  _LibManager := TsmxLibraryManager.Create(nil);

finalization
  _LibManager.Free;

end.
