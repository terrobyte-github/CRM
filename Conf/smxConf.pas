unit smxConf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, smxClasses, smxDBIntf;

type
  TfrmConf = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Edit2: TEdit;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button4: TButton;
    Button5: TButton;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Button1: TButton;
    StatusBar1: TStatusBar;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox3: TComboBox;
    CheckBox1: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    Button8: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    //FDatabaseIntf: IsmxDatabase;
    FCfg: TsmxBaseCfg;
    FTargetRequest: TsmxTargetRequest;
    FImageList: TImageList;
    FProjectManager: TsmxProjectManager;
    FDBConnection: TsmxDBConnection;
    procedure CheckDatabase;
    procedure SaveProgVers;
    procedure FillIntfList;
    //function GetFullStateCfg: String;
    procedure LoadImage;
    procedure FillProjectList;
    function GetCellCfg(CfgID: Integer): String;
    function GetStateCfg(CfgID, IntfID: Integer): String;
  public
    //property Database: IsmxDatabase read FDatabaseIntf;
    //property TargetRequest: TsmxTargetRequest read FTargetRequest;
    //property ProjectManager: TsmxProjectManager read FProjectManager;
  end;

var
  frmConf: TfrmConf;

implementation

{$R *.dfm}
{$R ..\Resource\pic.res}

uses
  ImgList, smxCommonStorage, smxLibManager, smxDBManager, smxFuncs, smxTypes,
  smxConsts, smxProcs, smxProject;

type
  { _TsmxBaseCfg }

  _TsmxBaseCfg = class(TsmxBaseCfg)
  end;

  { _TsmxStateCfg }
  _TsmxStateCfg = class(TsmxStateCfg)
  end;

  { _TCustomImageList }

  _TCustomImageList = class(TCustomImageList)
  end;

procedure TfrmConf.FormCreate(Sender: TObject);
begin
  //smxMain.Initialize;
  //FDatabaseIntf := IsmxDatabase(Integer(FuncCallBack(1)));
  //FDatabaseIntf.DatabaseName := 'Base';
  //FDatabaseIntf.LoginPrompt := False;
  FTargetRequest := TsmxTargetRequest.Create(Self);
  FImageList := TImageList.Create(Self);
  LoadImage;
  FProjectManager := TsmxProjectManager.Create(Self);
  FProjectManager.FileName := SFileProjectName;
  FProjectManager.ReadProjects;
  FillProjectList;
  SaveProgVers;
end;

procedure TfrmConf.FormDestroy(Sender: TObject);
begin
  FProjectManager.Free;
  FImageList.Free;
  FTargetRequest.Free;
  //FDatabaseIntf := nil;
  if Assigned(FDBConnection) then
    FDBConnection.Free;
  if Assigned(FCfg) then
    FCfg.Free;
end;

procedure TfrmConf.CheckDatabase;
begin
  if not Assigned(FDBConnection) then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  if not Assigned(FDBConnection.Database) then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
end;

procedure TfrmConf.SaveProgVers;
var VersM, VersL: Cardinal;
begin
  GetFileFullVersion(PChar(Application.ExeName), VersM, VersL);
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

procedure TfrmConf.FillIntfList;
var s: String; IntfID: Integer; IntfName: String;
begin
  ComboBox3.Clear;
  CheckDatabase;
  FTargetRequest.Database := FDBConnection.Database;
  s := 'select i.IntfID, replicate(''  '', ti.HLevel) + i.IntfName as IntfName ' +
    'from f_TreeInterfaces(1000001) ti ' +
    'join tInterfaces i on i.IntfID = ti.ID';
  with FTargetRequest.ForRequest(s, dstQuery, True) do
  begin
    First;
    while not Eof do
    begin
      IntfID := FieldByName('IntfID').Value;
      IntfName := FieldByName('IntfName').Value;
      ComboBox3.AddItem(IntfName, TObject(IntfID));
      Next;
    end;
  end;
end;

procedure TfrmConf.LoadImage;
var rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, 'pic', RT_RCDATA);
  try
    _TCustomImageList(FImageList).ReadData(rs);
  finally
    rs.Free;
  end;
end;

procedure TfrmConf.FillProjectList;
var i: Integer;
begin
  ComboBox1.Clear;
  for i := 0 to FProjectManager.ProjectList.Count - 1 do
    ComboBox1.Items.Add(FProjectManager.ProjectList[i].ProjectName);
end;

{function TfrmConf.GetFullStateCfg;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('cell');
    with n do
    begin
      Attributes['id'] := AUnit.CfgID;
      Attributes['enable'] := AUnit.UnitEnable;
      Attributes['visible'] := AUnit.UnitVisible;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;

var r, n, n2, n3: IXMLNode; i, j: Integer; XMLDocIntf: IXMLDocument;
begin
  Result := '';
  if not (FCfg is TsmxStateCfg) then
    Exit;
  try
    XMLDocIntf := NewXMLDocument;
    r := XMLDocIntf.ChildNodes.FindNode('root');
    if Assigned(r) then
      r.ChildNodes.Clear else
      r := XMLDocIntf.AddChild('root');

    with TsmxStateCfg(FCfg) do
    begin
      n := r.AddChild('states');
      for i := 0 to CellStates.Count - 1 do
      begin
        n2 := n.AddChild('state');
        n2.Attributes['id'] := CellStates[i].ID;
        n3 := n2.AddChild('cells');
        for j := 0 to CellStates[i].StateUnits.Root.Count - 1 do
          AddNodes(n3, CellStates[i].StateUnits.Root[j]);
      end;
    end;
    Result := FormatXMLText(XMLDocIntf.XML.Text); 
  finally
    XMLDocIntf := nil;
  end;
end;}

function TfrmConf.GetCellCfg(CfgID: Integer): String;
begin
  CheckDatabase;
  if CfgID > 0 then
  begin
    FCfg := TsmxCellCfg.Create(nil, FDBConnection.Database, CfgID);
    FCfg.Initialize;
    Result := _TsmxBaseCfg(FCfg).XMLText;
  end else
    Result := '';
end;

function TfrmConf.GetStateCfg(CfgID, IntfID: Integer): String;
begin
  CheckDatabase;
  if (CfgID > 0) and (IntfID > 0) then
  begin
    FCfg := TsmxStateCfg.CreateByIntfID(nil, FDBConnection.Database, CfgID, IntfID);
    FCfg.Initialize;
    if CheckBox1.Checked then
      Result := _TsmxStateCfg(FCfg).FullXMLText else //GetFullStateCfg
      Result := _TsmxStateCfg(FCfg).XMLText;
  end else
    Result := '';
end;

procedure TfrmConf.Button1Click(Sender: TObject);
var i: Integer; s: String; pr: TsmxProjectItem;
begin
  {if FDatabaseIntf.Connected then
  begin
    FDatabaseIntf.Connected := False;
    StatusBar1.Panels[1].Text := 'Статус: disconnect';
  end else
  begin
    s := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
    if s <> '' then
    begin
      with FDatabaseIntf.Params do
      begin
        Clear;
        Add('Provider=SQLOLEDB.1;');
        Add('Integrated Security=SSPI;');
        Add('Initial Catalog=crm;');
        Add('Data Source=' + s + ';');
      end;
      try
        FDatabaseIntf.Connected := True;
        FTargetRequest.Database := FDatabaseIntf;
        FillIntfList;
        StatusBar1.Panels[0].Text := 'Сервер: ' + s;
        StatusBar1.Panels[1].Text := 'Статус: connect';
        //StatusBar1.Panels[2].Text := 'Пользователь: ';
      except
        raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
      end;
    end;
  end;}
  i := frmConf.ComboBox1.ItemIndex;
  if i >= 0 then
  begin
    s := frmConf.ComboBox1.Items[i];
    pr := FProjectManager.ProjectList.FindByName(s);
    if Assigned(pr) then
    begin
      frmProject := TfrmProject.Create(Self);
      try
        with frmProject, pr do
        begin
          Edit1.Text := ProjectName;
          ComboBox1.ItemIndex := Integer(Generation);
          Edit2.Text := LibraryName;
          Edit3.Text := FunctionNameOrProgID;
          CheckBox1.Checked := WindowsAuthorization;
          Edit4.Text := DatabaseName;
          Edit5.Text := DriverName;
          CheckBox2.Checked := LoginPrompt;
          Memo1.Lines.Text := Params;
          if ShowModal = mrOk then
          begin
            ProjectName := Edit1.Text;
            Generation := TsmxGenerationMode(ComboBox1.ItemIndex);
            LibraryName := Edit2.Text;
            FunctionNameOrProgID := Edit3.Text;
            WindowsAuthorization := CheckBox1.Checked;
            DatabaseName := Edit4.Text;
            DriverName := Edit5.Text;
            LoginPrompt := CheckBox2.Checked;
            Params := Memo1.Lines.Text;
            FillProjectList;
            frmConf.ComboBox1.ItemIndex := i;
          end;
        end;
      finally
        FreeAndNil(frmProject);
      end;
    end;
  end;
end;

procedure TfrmConf.Button4Click(Sender: TObject);
var CfgID: Integer; IntfID: Integer;
begin
  //if not Assigned(FDBConnection) then
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
  //if not FDBConnection.Database.Connected then
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  //CheckDatabase;
  if Assigned(FCfg) then
    FreeAndNil(FCfg);
  Memo1.Lines.Clear;
  CfgID := StrToIntDef(Edit2.Text, 0);
  if ComboBox3.ItemIndex >= 0 then
    IntfID := Integer(ComboBox3.Items.Objects[ComboBox3.ItemIndex]) else
    IntfID := 0;
  case ComboBox2.ItemIndex of
    0: Memo1.Lines.Text := GetCellCfg(CfgID);
    1: Memo1.Lines.Text := GetStateCfg(CfgID, IntfID);
  end;
end;

procedure TfrmConf.Button5Click(Sender: TObject);
begin
  //CheckDatabase;
  if Assigned(FCfg) then
  begin
    _TsmxBaseCfg(FCfg).XMLText := Memo1.Lines.Text;
    FCfg.Finalize;
  end;
end;

procedure TfrmConf.Button6Click(Sender: TObject);
begin
  if StrToIntDef(Edit1.Text, -1) >= 0 then
    Edit1.Text := HotKeyToStr(StrToInt(Edit1.Text)) else
    Edit1.Text := IntToStr(StrToHotKey(Edit1.Text));
end;

procedure TfrmConf.Button7Click(Sender: TObject);
var i: integer; form: TForm; lv: TListView; //il: TImageList;
begin
  //il := TImageList(Integer(FuncCallBack(5)));
  form := TForm.Create(Application);
  with form do
  try
    ClientHeight := 480;
    ClientWidth := 640;
    Position := poScreenCenter;
    lv := TListView.Create(form);
    with lv do
    try
      Parent := form;
      Align := alClient;
      LargeImages := FImageList;
      for i := 0 to FImageList.Count - 1 do
        with Items.Add do
        begin
          Caption := IntToStr(i);
          ImageIndex := i;
        end;
      ShowModal;
    finally
      FreeAndNil(lv);
    end;
  finally
    FreeAndNil(form);
  end;    
end;

procedure TfrmConf.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0:
    begin
      CheckBox1.Enabled := False;
      Button5.Enabled := True;
    end;
    1:
    begin
      CheckBox1.Enabled := True;
      if CheckBox1.Checked then
        Button5.Enabled := False else
        Button5.Enabled := True;
    end;
  end;
end;

procedure TfrmConf.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Button5.Enabled := False else
    Button5.Enabled := True;
  Button4.OnClick(nil);
end;

procedure TfrmConf.Button3Click(Sender: TObject);
var i: Integer; s: String; pr: TsmxProjectItem;
begin
  i := ComboBox1.ItemIndex;
  if i >= 0 then
  begin
    s := ComboBox1.Items[i];
    if Ask('Удалить проект - ' + s + '?') then
    begin
      pr := FProjectManager.ProjectList.FindByName(s);
      if Assigned(pr) then
      begin
        FProjectManager.ProjectList.Remove(pr);
        //FProjectManager.WriteProjects;
        FillProjectList;
        ComboBox1.OnChange(nil);
      end;
    end;
  end;
end;

procedure TfrmConf.Button8Click(Sender: TObject);
begin
  FProjectManager.WriteProjects;
end;

procedure TfrmConf.ComboBox1Change(Sender: TObject);
var i: Integer; s: String; pr: TsmxProjectItem;
begin
  if Assigned(FDBConnection) then
    FreeAndNil(FDBConnection);
  StatusBar1.Panels[0].Text := 'Проект:';
  StatusBar1.Panels[1].Text := 'Статус: disconnect';
  ComboBox3.Clear;

  i := ComboBox1.ItemIndex;
  if i >= 0 then
  begin
    s := ComboBox1.Items[i];
    pr := FProjectManager.ProjectList.FindByName(s);
    if Assigned(pr) then
    begin
      //FDBConnection := TsmxDBConnection.Create(nil, pr.Generation, pr.DatabaseName, pr.LibraryName, pr.FunctionNameOrProgID);
      FDBConnection := TsmxDBConnection.Create(nil);
      with FDBConnection do
      begin
        DatabaseName := pr.DatabaseName;
        LibraryName := pr.LibraryName;
        FunctionNameOrProgID := pr.FunctionNameOrProgID;
        GenerationMode := pr.Generation;
        DriverName := pr.DriverName;
        LoginPrompt := pr.LoginPrompt;
        Params := pr.Params;
        UserName := '';
        Password := '';
        LibraryManager := LibManager;
        DatabaseManager := DBManager;
        ConnectToDatabase;
        //FTargetRequest.Database := Database;
        FillIntfList;
        StatusBar1.Panels[0].Text := 'Проект: ' + s;
        StatusBar1.Panels[1].Text := 'Статус: connect';
      end;
    end;
  end;
end;

procedure TfrmConf.Button2Click(Sender: TObject);
begin
  frmProject := TfrmProject.Create(Self);
  try
    with frmProject do
    begin
      if ShowModal = mrOk then
      begin
        with FProjectManager.ProjectList.Add do
        begin
          ProjectName := Edit1.Text;
          Generation := TsmxGenerationMode(ComboBox1.ItemIndex);
          LibraryName := Edit2.Text;
          FunctionNameOrProgID := Edit3.Text;
          WindowsAuthorization := CheckBox1.Checked;
          DatabaseName := Edit4.Text;
          DriverName := Edit5.Text;
          LoginPrompt := CheckBox2.Checked;
          Params := Memo1.Lines.Text;
        end;
        FillProjectList;
        frmConf.ComboBox1.ItemIndex := frmConf.ComboBox1.Items.IndexOf(Edit1.Text);
      end;
    end;
  finally
    FreeAndNil(frmProject);
  end;
end;

end.
