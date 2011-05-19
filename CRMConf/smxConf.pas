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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    FDatabaseIntf: IsmxDatabase;
    FCfg: TsmxBaseCfg;
    FTargetRequest: TsmxTargetRequest;
    procedure FillIntfList;
    //function GetFullStateCfg: String;
    function GetCellCfg(CfgID: Integer): String;
    function GetStateCfg(CfgID, IntfID: Integer): String;
  public
    property Database: IsmxDatabase read FDatabaseIntf;
    property TargetRequest: TsmxTargetRequest read FTargetRequest;
  end;

var
  frmConf: TfrmConf;

implementation

{$R *.dfm}

uses
  {XMLIntf, XMLDoc,} smxADODB, smxFuncs, smxCallBack, smxMain, smxConsts;

type
  _TsmxBaseCfg = class(TsmxBaseCfg)
  end;

  _TsmxStateCfg = class(TsmxStateCfg)
  end;

procedure TfrmConf.FormCreate(Sender: TObject);
begin
  smxMain.Initialize;
  FDatabaseIntf := IsmxDatabase(Integer(FuncCallBack(1)));
  FDatabaseIntf.DatabaseName := 'Base';
  FDatabaseIntf.LoginPrompt := False;
  FTargetRequest := TsmxTargetRequest.Create(Self);
end;

procedure TfrmConf.FormDestroy(Sender: TObject);
begin
  FTargetRequest.Free;
  FDatabaseIntf := nil;
  if Assigned(FCfg) then
    FCfg.Free;
end;

procedure TfrmConf.FillIntfList;
var s: String; IntfID: Integer; IntfName: String;
begin
  ComboBox3.Clear;
  s := 'select i.IntfID, replicate(''  '', ti.HLevel) + i.IntfName as IntfName ' +
    'from f_TreeInterfaces(1000001) ti ' +
    'join tInterfaces i on i.IntfID = ti.ID';
  with TargetRequest.ForRequest(s, dstQuery, True) do
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
  if CfgID > 0 then
  begin
    FCfg := TsmxCellCfg.Create(nil, FDatabaseIntf, CfgID);
    FCfg.Initialize;
    Result := _TsmxBaseCfg(FCfg).XMLText;
  end else
    Result := '';
end;

function TfrmConf.GetStateCfg(CfgID, IntfID: Integer): String;
begin
  if (CfgID > 0) and (IntfID > 0) then
  begin
    FCfg := TsmxStateCfg.CreateByIntfID(nil, FDatabaseIntf, CfgID, IntfID);
    FCfg.Initialize;
    if CheckBox1.Checked then
      Result := _TsmxStateCfg(FCfg).FullXMLText else //GetFullStateCfg
      Result := _TsmxStateCfg(FCfg).XMLText;
  end else
    Result := '';
end;

procedure TfrmConf.Button1Click(Sender: TObject);
var s: String;
begin
  if Database.Connected then
  begin
    Database.Connected := False;
    StatusBar1.Panels[1].Text := 'Статус: disconnect';
  end else
  begin
    s := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
    if s <> '' then
    begin
      Database.Params.Clear;
      Database.Params.Add('Provider=SQLOLEDB.1;');
      Database.Params.Add('Integrated Security=SSPI;');
      Database.Params.Add('Initial Catalog=crm;');
      Database.Params.Add('Data Source=' + s + ';');
      try
        Database.Connected := True;
        TargetRequest.Database := Database;
        FillIntfList;
        StatusBar1.Panels[0].Text := 'Сервер: ' + s;
        StatusBar1.Panels[1].Text := 'Статус: connect';
        //StatusBar1.Panels[2].Text := 'Пользователь: ';
      except
        raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
      end;
    end;
  end;
end;

procedure TfrmConf.Button4Click(Sender: TObject);
var CfgID: Integer; IntfID: Integer;
begin
  if not Database.Connected then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
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
  if not Database.Connected then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
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
var i: integer; form: TForm; lv: TListView; il: TImageList;
begin
  il := TImageList(Integer(FuncCallBack(5)));
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
      LargeImages := il;
      for i := 0 to il.Count - 1 do
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
  Button4.Click;  
end;

end.
