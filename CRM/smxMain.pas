unit smxMain;

interface

procedure Initialize;
function CreateMainForm: Boolean;
//procedure SaveProgVers;
//function ConnectDatabase(AProjectName: String; ALogin: String = ''; APassword: String = ''): Boolean;
//procedure DisconnectDatabase;
//procedure LoadCell;
//procedure LoadImage;
function LogIn: Boolean;

implementation

uses
  Classes, ImgList, Forms, Controls, Windows, SysUtils, StdCtrls, Graphics,
  IniFiles, smxClasses, smxFormManager, smxDBManager, smxLibManager, smxCallBack,
  smxCommonStorage, smxClassFuncs, smxFuncs, smxProcs, smxTypes, smxDBIntf,  
  smxConsts;

type
  { _TCustomImageList }

  _TCustomImageList = class(TCustomImageList)
  end;

var
  _MainForm: TForm = nil;
  _ImgList: TImageList = nil;
  _DBConnection: TsmxDBConnection = nil;

procedure SaveProgInfo;
var s: String; VersM, VersL: Cardinal;
begin
  s := Application.ExeName;
  ComStorage['CRMExe'] := s;
  ComStorage['CRMPath'] := ExtractFilePath(s);
  GetFileFullVersion(s, VersM, VersL);
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
end;

procedure LoadCell;
begin
  LibManager.CallLibrary('cCells.dll');
end;

procedure LoadImage;
var rs: TResourceStream; FuncNewResource: TsmxFuncNewResource;
begin
  @FuncNewResource := LibManager.GetProcedure('rImages.dll', 'NewResource');
  if Assigned(FuncNewResource) then
  begin
    rs := FuncNewResource('pic');
    try
      //_TCustomImageList(_ImgList).ReadData(rs);
      LoadImagesFromStream(_ImgList, rs);
    finally
      rs.Free;
    end;
  end;
end;

procedure LoadCfg;
var f: TIniFile; sl, sl2: TStringList; i, j: Integer;
begin
  if FileExists(ComStorage['CRMPath'] + SFileConfigurationName) then
  begin
    f := TIniFile.Create(ComStorage['CRMPath'] + SFileConfigurationName);
    try
      sl := TStringList.Create;
      try
        sl2 := TStringList.Create;
        try
          f.ReadSections(sl);
          for i := 0 to sl.Count - 1 do
          begin
            f.ReadSectionValues(sl[i], sl2);
            for j := 0 to sl2.Count - 1 do
              ComStorage[sl2.Names[j]] := sl2.Values[sl2.Names[j]]; //sl2.ValueFromIndex[j];
          end;
        finally
          sl2.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      f.Free;
    end;
  end;
end;

procedure SaveVariables;
var s: String;
begin
  s := ComStorage['CRMPath'];
  SetEnvironmentVariable(PChar('CRM'), PChar(s));
  s := ';' + s + ComStorage['LibPath'];
  SetEnvironmentVariable(PChar('Path'), PChar(GetEnvironmentVariable('Path') + s));
  {s := GetEnvironmentVariable('Path'); inf(s);
  s2 := ExtractFileDir(Application.ExeName); inf(s2);
  SetEnvironmentVariable(PChar('CRM'), PChar(s2)); inf('setvar');
  if s <> '' then
    s := s + ';' + s2 else
    s := s2;
  s2 := ComStorage['LibPath'];
  if s2 <> '' then
    s := s + '\' + s2;
  SetEnvironmentVariable(PChar('Path'), PChar(s));}
end;

procedure Initialize;
begin
  SaveProgInfo;
  AssignCallBackParams;
  LoadCfg;
  SaveVariables;
  LibManager.LibPath := ComStorage['LibPath'];
  LibManager.ProcLibInfoName := ComStorage['ProcLibInfo'];
  LoadCell;
  LoadImage;
end;

function CreateMainForm: Boolean;
var f: TsmxCustomForm; IntfID: Integer; c: TsmxBaseCell;
begin
  //IntfID := ComStorage['IntfID'];
  IntfID := 0;
  f := NewForm(nil, _DBConnection.Database, 1000002);
  //c := NewCell(nil, _DBConnection.Database, 1000002);
  if f is TsmxCustomMasterForm then
  //if c is TsmxCustomMasterForm then
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
    //raise EsmxCellError.CreateRes(@SCellBuildError);
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
          ComStorage['@UsedrID'] := UserID;
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
  try
    pm.FileName := ComStorage['CfgPath'] + ComStorage['FileProject']; //SFileProjectName;
    pm.ReadProjects;
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
          if ALogin = '' then
            UserName := pr.UserName else
            UserName := ALogin;
          if APassword = '' then
            Password := pr.Password else
            Password := APassword;
          LibraryManager := LibManager;
          DatabaseManager := DBManager;
          ConnectToDatabase;
          //Result := CheckUser(_DBConnection.Database);
          Result := True;
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

{procedure DisconnectDatabase;
begin
  if Assigned(_DBConnection) then
  begin
    _DBConnection.Free;
    _DBConnection := nil;
  end;
end;}

function LogIn: Boolean;

  {procedure ProcOnChange(Sender: TObject);
  var c: TControl; p: TsmxProjectItem;
  begin
    if Sender is TComboBox then
      with TComboBox(Sender) do
      begin
        p := TsmxProjectItem(Items.Objects[ItemIndex]);
        c := Parent.FindChildControl('Login');
        if c is TEdit then
          with TEdit(c) do
          begin
            if Assigned(p) then
            begin
              Enabled := not p.WindowsAuthorization;
              if Enabled then
                Color := clWindow else
                Color := clBtnFace;
            end else
            begin
              Enabled := True;
              Color := clWindow;
            end;
          end;
        c := Parent.FindChildControl('Password');
        if c is TEdit then
          with TEdit(c) do
          begin
            if Assigned(p) then
            begin
              Enabled := not p.WindowsAuthorization;
              if Enabled then
                Color := clWindow else
                Color := clBtnFace;
            end else
            begin
              Enabled := True;
              Color := clWindow;
            end;
          end;
      end;
  end;}

//var f: TForm; l, l2, l3: TLabel; cb: TComboBox; e, e2: TEdit; b, b2: TButton;
  //pm: TsmxProjectManager; i: Integer; m: TMethod;
begin
  {Result := False;
  f := TForm.Create(nil);
  try
    pm := TsmxProjectManager.Create(nil);
    try
      pm.FileName := SFileProjectName;
      pm.ReadProjects;
      with f do
      begin
        Height := 160;
        Width := 310;
        Caption := 'Вход в программу';
        Position := poScreenCenter;
        BorderIcons := [biSystemMenu];
        BorderStyle := bsSingle;
        l := TLabel.Create(f);
        with l do
        begin
          Parent := f;
          Left := 10;
          Top := 5;
          Caption := 'Проект:';
        end;
        cb := TComboBox.Create(f);
        with cb do
        begin
          Parent := f;
          Left := 10;
          Top := 21;
          Width := 200;
          Style := csDropDownList;
          m.Data := cb;
          m.Code := @ProcOnChange;
          OnChange := TNotifyEvent(m);
          Clear;
          for i := 0 to pm.ProjectList.Count - 1 do
            AddItem(pm.ProjectList[i].ProjectName, pm.ProjectList[i]);
          if Items.Count > 0 then
            ItemIndex := 0;
        end;
        l2 := TLabel.Create(f);
        with l2 do
        begin
          Parent := f;
          Left := 10;
          Top := 47;
          Caption := 'Логин:';
        end;
        e := TEdit.Create(f);
        with e do
        begin
          Parent := f;
          Left := 10;
          Top := 63;
          Width := 200;
          Name := 'Login';
          Text := '';
        end;
        l3 := TLabel.Create(f);
        with l3 do
        begin
          Parent := f;
          Left := 10;
          Top := 89;
          Caption := 'Пароль:';
        end;
        e2 := TEdit.Create(f);
        with e2 do
        begin
          Parent := f;
          Left := 10;
          Top := 105;
          Width := 200;
          PasswordChar := '*';
          Name := 'Password';
          Text := '';
        end;
        b := TButton.Create(f);
        with b do
        begin
          Parent := f;
          Left := 220;
          Top := 63;
          Caption := 'Ввод';
          ModalResult := mrOk;
        end;
        b2 := TButton.Create(f);
        with b2 do
        begin
          Parent := f;
          Left := 220;
          Top := 105;
          Caption := 'Отмена';
          ModalResult := mrCancel;
        end;
        ProcOnChange(cb);
        if ShowModal = mrOk then
          if cb.ItemIndex >= 0 then
            Result := ConnectDatabase(cb.Items[cb.ItemIndex], e.Text, e2.Text);
      end;
    finally
      pm.Free;
    end;
  finally
    f.Free;
  end;}
  Result := ConnectDatabase('CRM2');
end;

initialization
  _ImgList := TImageList.Create(nil);

finalization
  _ImgList.Free;

end.
