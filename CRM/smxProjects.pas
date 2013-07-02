unit smxProjects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, smxClasses, smxManagerClasses;

type
  TfProjects = class(TForm)
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
    Button2: TButton;
    ListBox1: TListBox;
    Label8: TLabel;
    Button3: TButton;
    Button4: TButton;
    Edit6: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Edit7: TEdit;
    lNote: TLabel;
    Button1: TButton;
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    FProjectManager: TsmxProjectManager;
    FMacroUserName: String;
    FMacroPassword: String;
    procedure ClearProjectParams;
    procedure FillProjectList;
    procedure FillProjectParams;
    procedure SetMacroPassword(const Value: String);
    procedure SetMacroUserName(const Value: String);
    procedure SetProjectManager(Value: TsmxProjectManager);
  public
    property MacroPassword: String read FMacroPassword write SetMacroPassword;
    property MacroUserName: String read FMacroUserName write SetMacroUserName;
    property ProjectManager: TsmxProjectManager read FProjectManager write SetProjectManager;
  end;

function ShowProjects(ProjectManager: TsmxProjectManager;
  MacroUserName, MacroPassword: String): Boolean;

implementation

{$R *.dfm}

uses
  smxFuncs, smxTypes, smxConsts;

const
  Note = 'В параметрах можно использовать макросы: %s и %s';

function ShowProjects(ProjectManager: TsmxProjectManager;
  MacroUserName, MacroPassword: String): Boolean;
var
  fProjects: TfProjects;
begin
  fProjects := TfProjects.Create(nil);
  try
    fProjects.MacroUserName := MacroUserName;
    fProjects.MacroPassword := MacroPassword;
    fProjects.ProjectManager := ProjectManager;
    Result := fProjects.ShowModal = mrOk;
  finally
    fProjects.Free;
  end;
end;

{ TfProjects }

procedure TfProjects.ClearProjectParams;
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

procedure TfProjects.FillProjectList;
var
  i: Integer;
begin
  if Assigned(FProjectManager) then
    for i := 0 to FProjectManager.ProjectList.Count - 1 do
      ListBox1.AddItem(FProjectManager.ProjectList[i].ProjectName, FProjectManager.ProjectList[i]);
end;

procedure TfProjects.FillProjectParams;
var
  Item: TsmxProjectItem;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    Item := TsmxProjectItem(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    if Assigned(Item) then
    begin
      Edit1.Text := Item.ProjectName;
      ComboBox1.ItemIndex := Integer(Item.Generation);
      Edit2.Text := Item.LibraryName;
      Edit3.Text := Item.FuncOrClassNameOrProgID;
      CheckBox1.Checked := Item.IsWindowsAuthorization;
      Edit4.Text := Item.DatabaseName;
      Edit5.Text := Item.DriverName;
      CheckBox2.Checked := Item.IsUseUser;
      Memo1.Lines.Text := Item.Params;
      Edit6.Text := Item.UserName;
      Edit7.Text := Item.Password;
    end;
  end;
end;

procedure TfProjects.SetMacroPassword(const Value: String);
begin
  FMacroPassword := Value;
  lNote.Caption := SysUtils.Format(Note, [FMacroUserName, FMacroPassword]);
end;

procedure TfProjects.SetMacroUserName(const Value: String);
begin
  FMacroUserName := Value;
  lNote.Caption := SysUtils.Format(Note, [FMacroUserName, FMacroPassword]);
end;

procedure TfProjects.SetProjectManager(Value: TsmxProjectManager);
begin
  if Assigned(FProjectManager) then
  begin
    ListBox1.Clear;
    ClearProjectParams;
  end;
  FProjectManager := Value;
  if Assigned(FProjectManager) then
  begin
    FillProjectList;
    if ListBox1.Items.Count > 0 then
    begin
      ListBox1.ItemIndex := 0;
      FillProjectParams;
    end;
  end;
end;

procedure TfProjects.ListBox1Click(Sender: TObject);
begin
  FillProjectParams;
end;

procedure TfProjects.Button3Click(Sender: TObject);
var
  s: String;
  Item: TsmxProjectItem;
begin
  if Assigned(FProjectManager) then
    if Dialogs.InputQuery('Введите название проекта', '', s) then
    begin
      Item := FProjectManager.ProjectList.Add;
      Item.ProjectName := s;
      ListBox1.AddItem(s, Item);
      ListBox1.ItemIndex := ListBox1.Count - 1;
      FillProjectParams;
    end;
end;

procedure TfProjects.Button4Click(Sender: TObject);
var
  Item: TsmxProjectItem;
  CurIndex: Integer;
begin
  if Assigned(FProjectManager) then
  begin
    CurIndex := ListBox1.ItemIndex;
    if CurIndex <> -1 then
    begin
      Item := TsmxProjectItem(ListBox1.Items.Objects[CurIndex]);
      if Assigned(Item) then
        if smxFuncs.Ask('Удалить проект - ' + Item.ProjectName + '?') then
        begin
          FProjectManager.ProjectList.Delete(Item.ItemIndex);
          ListBox1.Items.Delete(CurIndex);
          if CurIndex <= ListBox1.Count then
            ListBox1.ItemIndex := CurIndex else
            ListBox1.ItemIndex := CurIndex - 1;
          if ListBox1.ItemIndex <> -1 then
            FillProjectParams
          else
            ClearProjectParams;
        end;
    end;
  end;
end;

procedure TfProjects.Edit1Exit(Sender: TObject);
var
  Item: TsmxProjectItem;
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    Item := TsmxProjectItem(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    if Assigned(Item) and (Sender is TComponent) then
      case TComponent(Sender).Tag of
        2: Item.Generation := TsmxGenerationMode(ComboBox1.ItemIndex);
        3: Item.LibraryName := Edit2.Text;
        4: Item.FuncOrClassNameOrProgID := Edit3.Text;
        5: Item.DatabaseName := Edit4.Text;
        6: Item.DriverName := Edit5.Text;
        7: Item.IsWindowsAuthorization := CheckBox1.Checked;
        8: Item.IsUseUser := CheckBox2.Checked;
        9: Item.Params := Memo1.Lines.Text;
        10: Item.UserName := Edit6.Text;
        11: Item.Password := Edit7.Text;
      end;
  end;
end;

procedure TfProjects.CheckBox2Click(Sender: TObject);
begin
  Edit6.Enabled := CheckBox2.Checked;
  Edit7.Enabled := CheckBox2.Checked;
end;

end.
