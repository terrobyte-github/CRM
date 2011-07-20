unit smxProjects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, smxClasses;

type
  TfrmProjects = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    Edit4: TEdit;
    Edit5: TEdit;
    CheckBox2: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Label8: TLabel;
    Button3: TButton;
    Button4: TButton;
    Edit6: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Edit7: TEdit;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
  private
    FProjectManager: TsmxProjectManager;
    procedure FillProjectList;
    procedure FillProjectParams;
    procedure ClearProjectParams;
  public
    { Public declarations }
  end;

var
  frmProjects: TfrmProjects;

implementation

{$R *.dfm}

uses
  smxFuncs, smxTypes, smxConsts;

procedure TfrmProjects.FormCreate(Sender: TObject);
begin
  FProjectManager := TsmxProjectManager.Create(Self);
  FProjectManager.FileName := '..\Cfg\proj.dat'; //'proj.dat'; //SFileProjectName;
  FProjectManager.ReadProjects;
  FillProjectList;
  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := 0;
  FillProjectParams;
end;

procedure TfrmProjects.FormDestroy(Sender: TObject);
begin
  FProjectManager.Free;
end;

procedure TfrmProjects.FillProjectList;
var i: Integer;
begin
  ListBox1.Clear;
  for i := 0 to FProjectManager.ProjectList.Count - 1 do
    ListBox1.Items.Add(FProjectManager.ProjectList[i].ProjectName);
end;

procedure TfrmProjects.FillProjectParams;
var i: Integer; pr: TsmxProjectItem;
begin
  i := ListBox1.ItemIndex;
  if i >= 0 then
    pr := FProjectManager.ProjectList.FindByName(ListBox1.Items[i]) else
    pr := nil;
  if Assigned(pr) then
    with pr do
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
      Edit6.Text := UserName;
      Edit7.Text := Password;
    end
  else
    ClearProjectParams;
end;

procedure TfrmProjects.ClearProjectParams;
begin
  Edit1.Text := '';
  ComboBox1.ItemIndex := 0;
  Edit2.Text := '';
  Edit3.Text := '';
  CheckBox1.Checked := False;
  Edit4.Text := '';
  Edit5.Text := '';
  CheckBox2.Checked := False;
  Memo1.Lines.Clear;
  Edit6.Text := '';
  Edit7.Text := '';
end;

procedure TfrmProjects.Button1Click(Sender: TObject);
begin
  FProjectManager.WriteProjects;
end;

procedure TfrmProjects.ListBox1Click(Sender: TObject);
begin
  FillProjectParams;
end;

procedure TfrmProjects.Button3Click(Sender: TObject);
var s: String;
begin
  if InputQuery('¬ведите название проекта', '', s) then
  begin
    with FProjectManager.ProjectList.Add do
      ProjectName := s;
    FillProjectList;
    if ListBox1.Items.Count > 0 then
      ListBox1.ItemIndex := ListBox1.Items.Count - 1 else
      ListBox1.ItemIndex := -1;
    FillProjectParams;
  end;
end;

procedure TfrmProjects.Button4Click(Sender: TObject);
var i: Integer; s: String; pr: TsmxProjectItem;
begin
  i := ListBox1.ItemIndex;
  if i >= 0 then
  begin
    s := ListBox1.Items[i];
    if Ask('”далить проект - ' + s + '?') then
    begin
      pr := FProjectManager.ProjectList.FindByName(s);
      if Assigned(pr) then
      begin
        FProjectManager.ProjectList.Remove(pr);
        FillProjectList;
        if ListBox1.Items.Count - 1 >= i then
          ListBox1.ItemIndex := i else
        if ListBox1.Items.Count > 0 then
          ListBox1.ItemIndex := ListBox1.Items.Count - 1 else
          ListBox1.ItemIndex := -1;
        FillProjectParams;
      end;
    end;
  end;
end;

procedure TfrmProjects.Edit1Exit(Sender: TObject);
var i: Integer; pr: TsmxProjectItem;
begin
  i := ListBox1.ItemIndex;
  if i >= 0 then
  begin
    pr := FProjectManager.ProjectList.FindByName(ListBox1.Items[i]);
    if Assigned(pr) and (Sender is TComponent) then
    begin
      with TComponent(Sender) do
        case Tag of
          2: pr.Generation := TsmxGenerationMode(ComboBox1.ItemIndex);
          3: pr.LibraryName := Edit2.Text;
          4: pr.FunctionNameOrProgID := Edit3.Text;
          5: pr.DatabaseName := Edit4.Text;
          6: pr.DriverName := Edit5.Text;
          7: pr.WindowsAuthorization := CheckBox1.Checked;
          8: pr.LoginPrompt := CheckBox2.Checked;
          9: pr.Params := Memo1.Lines.Text;
          10: pr.UserName := Edit6.Text;
          11: pr.Password := Edit7.Text;
        end;
    end;
  end;
end;

end.
