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
  Classes, SysUtils, smxBaseClasses, smxClasses, smxTypes, smxLibTypes;

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

  { TsmxLibManager }

  TsmxLibManager = class(TsmxComponent)
  private
    FLibList: TsmxLibItems;
    function GetLib(Index: Integer): THandle;
    function GetLibCount: Integer;
    procedure DestroyLibs;
  protected
    function PrepareLibrary(ALibName: String): THandle;
    procedure ClearLibInfo(var ALibInfo: TsmxLibInfo);
    function GetLibHandle(ALibName: String): THandle;

    property LibList: TsmxLibItems read FLibList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByName(ALibName: String): THandle;
    function CheckLibraryComp(ALibHandle: THandle): Boolean; overload;
    function CheckLibraryComp(ALibName: String): Boolean; overload;
    function GetProcedure(ALibHandle: THandle; AProcName: String): Pointer; overload;
    function GetProcedure(ALibName, AProcName: String): Pointer; overload;
    function GetLibrary(ALibName: String): THandle;
    function GetLibraryInfo(ALibHandle: THandle; var ALibInfo: TsmxLibInfo): Boolean; overload;
    function GetLibraryInfo(ALibName: String; var ALibInfo: TsmxLibInfo): Boolean; overload;

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

{ TsmxLibManager }

constructor TsmxLibManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibList := TsmxLibItems.Create(TsmxLibItem);
end;

destructor TsmxLibManager.Destroy;
begin
  DestroyLibs;
  FLibList.Free;
  inherited Destroy;
end;

procedure TsmxLibManager.DestroyLibs;
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

initialization
  _LibManager := TsmxLibManager.Create(nil);

finalization
  _LibManager.Free;

end.
