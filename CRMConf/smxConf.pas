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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    FDatabaseIntf: IsmxDatabase;
    FCfg: TsmxBaseCfg;
  public
    property Database: IsmxDatabase read FDatabaseIntf;
  end;

var
  frmConf: TfrmConf;

implementation

{$R *.dfm}

uses
  smxADODB, smxFuncs, smxCallBack, smxMain, smxConsts;

type
  _TsmxBaseCfg = class(TsmxBaseCfg)
  end;

procedure TfrmConf.FormCreate(Sender: TObject);
begin
  smxMain.Initialize;
  FDatabaseIntf := IsmxDatabase(Integer(FuncCallBack(1)));
  FDatabaseIntf.DatabaseName := 'Base';
  FDatabaseIntf.LoginPrompt := False;
end;

procedure TfrmConf.FormDestroy(Sender: TObject);
begin
  FDatabaseIntf := nil;
  if Assigned(FCfg) then
    FCfg.Free;
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
        StatusBar1.Panels[0].Text := 'Сервер: ' + s;
        StatusBar1.Panels[1].Text := 'Статус: connect';
      except
        raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
      end;
    end;
  end;
end;

procedure TfrmConf.Button4Click(Sender: TObject);
var CfgID: Integer;
begin
  if not Database.Connected then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  if Assigned(FCfg) then
    FreeAndNil(FCfg);
  Memo1.Lines.Clear;
  CfgID := StrToIntDef(Edit2.Text, 0);
  if CfgID > 0 then
  begin
    FCfg := TsmxBaseCfg.Create(nil, FDatabaseIntf, CfgID);
    Memo1.Lines.Text := _TsmxBaseCfg(FCfg).XMLText;
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
  il := TImageList(Integer(FuncCallBack(4)));
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

end.
