unit smxGlobal;

interface

uses
  Controls, smxClasses, smxDBIntf, smxTypes;

function ImageList: TImageList;
function DataBase: IsmxDatabase;
function TargetRequest: TsmxTargetRequest;
function ConnectDatabase: Boolean;
procedure LoadImage;

implementation

{$R ..\Resource\pic.res}

uses
  Classes, Windows, Forms, ImgList, ActiveX, IniFiles, SysUtils, smxADODB,
  smxFuncs, smxCallBack, smxCells, smxGlobalStorage, smxConsts;

type
  _TCustomImageList = class(TCustomImageList)
  end;

var
  _ImageList: TImageList = nil;
  _Database: IsmxDatabase = nil;
  _TargetRequest: TsmxTargetRequest = nil;

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

function GetIntfUser: Boolean;
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
          GlobalStorage['@IntfID'] := IntfID;
          f := FindFieldSense(fsValue);
          if Assigned(f) then
            IntfName := f.Value else
            IntfName := '';
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
          GlobalStorage['@UserID'] := UserID;
          p := FindParamLocation(plValue);
          if Assigned(p) then
            UserName := p.Value else
            UserName := '';
          GlobalStorage['@UserName'] := UserName;
          if UserID > 0 then
            Result := GetIntfUser else
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
        end;
      finally
        f.Free;
      end;
    except
      raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
    end;
  end;
end;

initialization
  _ImageList := TImageList.Create(nil);
  CoInitialize(nil);
  _Database := NewADODatabase;
  _TargetRequest := TsmxTargetRequest.Create(nil);

finalization
  _TargetRequest.Free;
  _Database := nil;
  CoUninitialize;
  _ImageList.Free;

end.
