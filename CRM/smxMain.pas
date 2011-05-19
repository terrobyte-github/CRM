unit smxMain;

interface

procedure Initialize;
function CreateMainForm: Boolean;
//procedure SaveProgVers;
function ConnectDatabase(AProjectName: String; ALogin: String = ''; APassword: String = ''): Boolean;
procedure DisconnectDatabase;
procedure LoadCell;
procedure LoadImage;

implementation

uses
  Classes, ImgList, Forms, Controls, SysUtils, smxClasses,
  smxFormManager, smxDBManager, smxLibManager, smxCallBack, smxCommonStorage,
  smxClassFuncs, smxFuncs, smxProcs, smxTypes, smxDBIntf, smxConsts;

type
  { _TCustomImageList }

  _TCustomImageList = class(TCustomImageList)
  end;

var
  _MainForm: TForm = nil;
  _ImgList: TImageList = nil;
  _DBConnection: TsmxDBConnection = nil;

procedure SaveProgVers;
var VersM, VersL: Cardinal;
begin
  GetFileFullVersion(Application.ExeName, VersM, VersL);
  ComStorage['ProgVersMajor'] := LongRec(VersM).Hi;
  ComStorage['ProgVersMinor'] := LongRec(VersM).Lo;
  ComStorage['ProgVersRelease'] := LongRec(VersL).Hi;
  ComStorage['ProgVersBuild'] := LongRec(VersL).Lo;
  ComStorage['ProgVers'] :=
    IntToStr(LongRec(VersM).Hi) + '.' +
    IntToStr(LongRec(VersM).Lo) + '.' +
    IntToStr(LongRec(VersL).Hi) + '.' +
    IntToStr(LongRec(VersL).Lo);
end;

procedure AssignCallBackParams;
begin
  CallBack[0] := Integer(Application.Handle);
  CallBack[1] := Integer(ComStorage);
  CallBack[2] := Integer(LibManager);
  CallBack[3] := Integer(DBManager);
  CallBack[4] := Integer(FrmManager);
  CallBack[5] := Integer(_ImgList);
  //CallBack[6] := Integer(TargetRequest);
  //CallBack[10] := Integer(DBConnection.Database);

  //CallBack[101] := Integer(DBConnection.Database);

  //CallBack[111] := Integer(@smxCommonStorage.FindCommonParamByName);

  //CallBack[121] := Integer(@smxLibManager.FindProcedureByName);

  //CallBack[131] := Integer(@smxDBManager.FindDatabaseByName);

  //CallBack[141] := Integer(@smxFormManager.FindFormByComboID);
  //CallBack[142] := Integer(@smxFormManager.FindFormByHandle);
  //CallBack[143] := Integer(@smxFormManager.AddFormIntoManager);
  //CallBack[144] := Integer(@smxFormManager.DelFormFromManager);

  //CallBack[201] := Integer(@smxClassFuncs.NewCell);
  //CallBack[202] := Integer(@smxClassFuncs.NewForm);
  //CallBack[203] := Integer(@smxClassFuncs.IsCell);
end;

procedure Initialize;
begin
  ////LoadImage;
  SaveProgVers;
  AssignCallBackParams;
  LoadCell;
  LoadImage;
end;

function CreateMainForm: Boolean;
var f: TsmxCustomForm; IntfID: Integer;
begin
  IntfID := ComStorage['IntfID'];
  f := NewForm(nil, _DBConnection.Database, 1000218, IntfID);
  if f is TsmxCustomMasterForm then
  begin
    Application.ShowMainForm := False;
    Application.CreateForm(TForm, _MainForm);
    with TsmxCustomMasterForm(f) do
    begin
      Form := _MainForm;
      CommonStorage := ComStorage;
      LibraryManager := LibManager;
      DatabaseManager := DBManager;
      FormManager := FrmManager;
      ImageList := _ImgList;
      ShowForm;
    end;
    Result := True;
  end else
  begin
    f.Free;
    raise EsmxCellError.CreateRes(@SCellBuildError);
    Result := False;
  end;
end;

function CheckIntf(const ADatabase: IsmxDatabase): Boolean;
var c: TsmxBaseCell; IntfID, IntfName: Variant;
begin
  Result := False;
  c := NewCell(nil, ADatabase, 1000277);
  try
    if c is TsmxCustomRequest then
    begin
      with TsmxCustomRequest(c) do
      begin
        Database := ADatabase;
        CommonStorage := ComStorage;
      end;
      if RequestReturnKeyValue(TsmxCustomRequest(c), IntfID, IntfName) then
      begin
        Result := IntfID > 0;
        if Result then
        begin
          ComStorage['IntfID'] := IntfID;
          ComStorage['@IntfID'] := IntfID;
          ComStorage['IntfName'] := IntfName;
          ComStorage['@IntfName'] := IntfName;
        end;
      end;
    end;
  finally
    c.Free;
  end;
end;

function CheckUser(const ADatabase: IsmxDatabase): Boolean;
var c: TsmxBaseCell; UserID, UserName: Variant;
begin
  Result := False;
  c := NewCell(nil, ADatabase, 1000236);
  try
    if c is TsmxCustomRequest then
    begin
      with TsmxCustomRequest(c) do
      begin
        Database := ADatabase;
        CommonStorage := ComStorage;
      end;
      if RequestReturnKeyValue(TsmxCustomRequest(c), UserID, UserName) then
      begin
        if UserID = -1 then
          Inf(UserName) else
        if UserID > 0 then
        begin
          ComStorage['UserID'] := UserID;
          ComStorage['@UserID'] := UserID;
          ComStorage['UserName'] := UserName;
          ComStorage['@UserName'] := UserName;
          Result := CheckIntf(ADatabase);
        end;
      end;
    end;
  finally
    c.Free;
  end;
end;

function ConnectDatabase(AProjectName: String; ALogin: String = ''; APassword: String = ''): Boolean;
var pm: TsmxProjectManager; pr: TsmxProjectItem;
begin
  Result := False;
  pm := TsmxProjectManager.Create(nil);
  pm.FileName := SFileProjectName;
  pm.ReadProjects;
  try
    pr := pm.ProjectList.FindByName(AProjectName);
    if Assigned(pr) then
    begin
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
          UserName := ALogin;
          Password := APassword;
          LibraryManager := LibManager;
          DatabaseManager := DBManager;
          ConnectToDatabase;
          Result := CheckUser(_DBConnection.Database);
          if Result then
          begin
            ComStorage['CfgDBName'] := pr.DatabaseName;
            ComStorage['@CfgDBName'] := pr.DatabaseName;
            ComStorage['RequestDBName'] := pr.DatabaseName;
            ComStorage['@RequestDBName'] := pr.DatabaseName;
          end;
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

procedure DisconnectDatabase;
begin
  if Assigned(_DBConnection) then
  begin
    _DBConnection.Free;
    _DBConnection := nil;
  end;
end;

procedure LoadCell;
begin
  LibManager.CallLibrary('cCells.dll');
end;

procedure LoadImage;
var rs: TResourceStream; FuncLoadResource: TsmxFuncLoadResource;
begin
  @FuncLoadResource := LibManager.GetProcedure('rImages.dll', SFuncLoadResourceName);
  if Assigned(FuncLoadResource) then
  begin
    rs := FuncLoadResource('pic');
    try
      _TCustomImageList(_ImgList).ReadData(rs);
    finally
      rs.Free;
    end;
  end;
end;

initialization
  _ImgList := TImageList.Create(nil);

finalization
  _ImgList.Free;

end.
