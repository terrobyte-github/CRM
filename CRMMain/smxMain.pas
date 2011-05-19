unit smxMain;

interface

procedure Initialize;
function ConnectDatabase: Boolean;
function CreateMainForm: Boolean;

implementation

{$R ..\Resource\pic.res}

uses
  Classes, Controls, Windows, Forms, ActiveX, ImgList, SysUtils, IniFiles,
  smxClasses, smxCells, smxCallBack, smxADODB, smxFuncs, smxDBIntf, smxTypes,
  smxConsts;

type
  _TCustomImageList = class(TCustomImageList)
  end;

var
  MainForm: TForm = nil;
  ImageList: TImageList = nil;
  Database: IsmxDatabase = nil;
  FormManager: TsmxFormManager = nil;
  LibManager: TsmxLibManager = nil;
  TargetRequest: TsmxTargetRequest = nil;

function CheckUser: Boolean;
var c: TsmxBaseCell; UserID: Integer; UserName: String;
begin
  Result := False;
  try
    c := NewCell(nil, 1000236, FuncCallBack);
    try
      if c is TsmxCustomRequest then
        with TsmxCustomRequest(c) do
        begin
          Perform;
          UserID := StrToIntDef(RequestParams['@UserID'], 0);
          UserName := RequestParams['@UserName'];
          if UserID > 0 then
            Result := True;
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

procedure Initialize;
var rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, 'pic', RT_RCDATA);
  try
    _TCustomImageList(ImageList).ReadData(rs);
  finally
    rs.Free;
  end;

  CallBack[0] := Integer(Application.Handle);
  CallBack[1] := Integer(Database);
  CallBack[2] := Integer(FormManager);
  CallBack[3] := Integer(LibManager);
  CallBack[4] := Integer(ImageList);
  CallBack[5] := Integer(TargetRequest);
  CallBack[101] := Integer(@smxFuncs.NewCell);
  CallBack[111] := Integer(@smxFuncs.IsCell);
  CallBack[151] := Integer(@smxFuncs.Inf);
end;

function ConnectDatabase: Boolean;
var f: TIniFile;
begin
  Result := False;
  if FileExists(FileConnectParams) then
  begin
    f := TIniFile.Create(ExtractFilePath(Application.ExeName) + FileConnectParams);
    try
      with Database do
      begin
        if Connected then
          Connected := False;
        DatabaseName := 'Base';
        LoginPrompt := False;
        f.ReadSectionValues('Params', Params);
        try
          Connected := True;
          Result := CheckUser;
        except
          raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
        end;
      end;
    finally
      f.Free;
    end;
  end;
end;

function CreateMainForm: Boolean;
var c: TsmxBaseCell;
begin
  Result := False;
  try
    c := NewCell(nil, 1000218, FuncCallBack);
    if c is TsmxMainForm then
      with TsmxMainForm(c) do
      begin
        Application.ShowMainForm := False;
        Application.CreateForm(TForm, MainForm);
        Form := MainForm;
        ShowForm;
        Result := True;   
      end
    else
      c.Free;
  except
    raise EsmxCellError.CreateRes(@SCellBuildError);
  end;
end;

initialization
  ImageList := TImageList.Create(nil);
  CoInitialize(nil);
  Database := NewADODatabase;
  TargetRequest := TsmxTargetRequest.Create(nil);
  LibManager := TsmxLibManager.Create(nil);
  FormManager := TsmxFormManager.Create(nil);

finalization
  FormManager.Free;
  LibManager.Free;
  TargetRequest.Free;
  Database := nil;
  CoUninitialize;
  ImageList.Free;

end.
