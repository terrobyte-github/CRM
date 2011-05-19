unit smxLibManager;

interface

uses
  Classes, SysUtils, smxBaseClasses, smxClasses, smxTypes;

type
  { TsmxLibItem }

  TsmxLibItem = class(TsmxKitItem)
  private
    FLibHandle: THandle;
    FLibName: String;
    FLibInfo: TsmxLibInfo;
  public
    constructor Create(AKit: TsmxKit); override;

    property LibHandle: THandle read FLibHandle write FLibHandle;
    property LibName: String read FLibName write FLibName;
    property LibInfo: TsmxLibInfo read FLibInfo write FLibInfo;
  end;

  { TsmxLibItems }

  TsmxLibItems = class(TsmxKit)
  private
    function GetItem(Index: Integer): TsmxLibItem;
    //procedure SetItem(Index: Integer; Value: TsmxRequestParam);
  public
    function Add: TsmxLibItem;
    function FindByName(ALibName: String): TsmxLibItem;

    property Items[Index: Integer]: TsmxLibItem read GetItem {write SetItem}; default;
  end;

  { TsmxLibManager }

  //EsmxLibManagerError = class(Exception);

  TsmxLibManager = class(TsmxComponent)
  private
    FLibList: TsmxLibItems;
    function GetLibrary(Name: String): THandle;
    procedure FreeLibs;
  protected
    function PrepareLibrary(ALibName: String): THandle;

    property LibList: TsmxLibItems read FLibList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Libraries[Name: String]: THandle read GetLibrary; default;
  end;

function LibManager: TsmxLibManager;

implementation

uses
  Windows, smxCallBack, smxGlobalStorage, smxConsts;

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

{procedure TsmxLibItems.SetItem(Index: Integer; Value: TsmxLibItem);
begin
  inherited Items[Index] := Value;
end;}

{ TsmxLibManager }

constructor TsmxLibManager.Create(AOwner: TComponent);
begin
  FLibList := TsmxLibItems.Create(TsmxLibItem);
end;

destructor TsmxLibManager.Destroy;
begin
  FreeLibs;
  FLibList.Free;
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

function TsmxLibManager.GetLibrary(Name: String): THandle;
var l: TsmxLibItem;
begin
  l := FLibList.FindByName(Name);
  if Assigned(l) then
    Result := l.LibHandle else
    Result := PrepareLibrary(Name);
end;

function TsmxLibManager.PrepareLibrary(ALibName: String): THandle;

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

var FuncLibInfo: TsmxFuncLibInfo; li: TsmxLibInfo;
  FuncLibInit: TsmxFuncLibInit;
begin
  Result := LoadLibrary(PChar(ALibName));
  if Result > 0 then
  begin
    @FuncLibInfo := GetProcAddress(Result, PChar(SFuncLibInfoName));
    if Assigned(FuncLibInfo) then
    begin
      FuncLibInfo(li);
      if CheckComp(li.CompProgVers) then
      begin
        FuncLibInit := li.FuncLibInit;
        if Assigned(FuncLibInit) then
          FuncLibInit(FuncCallBack);
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
end;

initialization
  _LibManager := TsmxLibManager.Create(nil);

finalization
  _LibManager.Free;

end.
