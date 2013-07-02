unit smxLogIn;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, smxManagerClasses, smxTypes;

type
  TfLogIn = class(TForm)
    Label3: TLabel;
    cbProject: TComboBox;
    Label2: TLabel;
    ePassword: TEdit;
    Label1: TLabel;
    eUserName: TEdit;
    bEnter: TButton;
    bCancel: TButton;
    bProjects: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbProjectChange(Sender: TObject);
    procedure bProjectsClick(Sender: TObject);
    procedure eUserNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FIsConf: Boolean;
    FMacroUserName: String;
    FMacroPassword: String;
    FProjectManager: TsmxProjectManager;
    procedure ClearConnection(var Connection: TsmxProjectConnection);
    procedure FillConnection(var Connection: TsmxProjectConnection);
    procedure FillProjects;
    function GetFileName: String;
    function GetPassword: String;
    function GetProjectName: String;
    function GetUserName: String;
    procedure SetEditEnabled(Edit: TEdit; Enabled: Boolean);
    procedure SetFileName(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetProjectName(const Value: String);
    procedure SetUserName(const Value: String);
  public
    property FileName: String read GetFileName write SetFileName;
    property IsConf: Boolean read FIsConf write FIsConf;
    property MacroPassword: String read FMacroPassword write FMacroPassword;
    property MacroUserName: String read FMacroUserName write FMacroUserName;
    property Password: String read GetPassword write SetPassword;
    property ProjectName: String read GetProjectName write SetProjectName;
    property UserName: String read GetUserName write SetUserName;
  end;

function ShowLogIn(FileName: String; MacroUserName, MacroPassword: String;
  var Connection: TsmxProjectConnection; var UserName, Password: String;
  var IsConf: Boolean): Boolean;

implementation

{$R *.dfm}

uses
  smxProjects, smxPassword, smxFuncs;

const
  ProjectPassword = 'qwe';

function ShowLogIn(FileName: String; MacroUserName, MacroPassword: String;
  var Connection: TsmxProjectConnection; var UserName, Password: String;
  var IsConf: Boolean): Boolean;
var
  fLogIn: TfLogIn;
begin
  Result := False;
  fLogIn := TfLogIn.Create(nil);
  try
    fLogIn.FileName := FileName;
    fLogIn.MacroUserName := MacroUserName;
    fLogIn.MacroPassword := MacroPassword;
    fLogIn.ProjectName := Connection.ProjectName;
    fLogIn.UserName := UserName;
    if fLogIn.ShowModal = mrOk then
    begin
      fLogIn.FillConnection(Connection);
      UserName := fLogIn.UserName;
      Password := fLogIn.Password;
      IsConf := fLogIn.IsConf;
      Result := True;
    end;
  finally
    fLogIn.Free;
  end;
end;

{ TfLogIn }

procedure TfLogIn.FormCreate(Sender: TObject);
begin
  FProjectManager := TsmxProjectManager.Create(Self);
end;

procedure TfLogIn.FormDestroy(Sender: TObject);
begin
  FProjectManager.Free;
end;

procedure TfLogIn.ClearConnection(var Connection: TsmxProjectConnection);
begin
  with Connection do
  begin
    ProjectName := '';
    Generation := gmFunction;
    LibraryName := '';
    FuncOrClassNameOrProgID := '';
    IsWindowsAuthorization := False;
    DatabaseName := '';
    DriverName := '';
    Params := '';
    IsUseUser := False;
    UserName := '';
    Password := '';
  end;
end;

procedure TfLogIn.FillConnection(var Connection: TsmxProjectConnection);
var
  Item: TsmxProjectItem;
begin
  ClearConnection(Connection);
  if cbProject.ItemIndex <> -1 then
  begin
    Item := TsmxProjectItem(cbProject.Items.Objects[cbProject.ItemIndex]);
    with Connection do
    begin
      ProjectName := Item.ProjectName;
      Generation := Item.Generation;
      LibraryName := Item.LibraryName;
      FuncOrClassNameOrProgID := Item.FuncOrClassNameOrProgID;
      IsWindowsAuthorization := Item.IsWindowsAuthorization;
      DatabaseName := Item.DatabaseName;
      DriverName := Item.DriverName;
      Params := Item.Params;
      IsUseUser := Item.IsUseUser;
      UserName := Item.UserName;
      Password := Item.Password;
    end;
  end;
end;

procedure TfLogIn.FillProjects;
var
  i: Integer;
begin
  cbProject.Clear;
  for i := 0 to FProjectManager.ProjectList.Count - 1 do
    cbProject.AddItem(FProjectManager.ProjectList[i].ProjectName,
      FProjectManager.ProjectList[i]);
  if cbProject.Items.Count > 0 then
    cbProject.ItemIndex := 0;
end;

function TfLogIn.GetFileName: String;
begin
  Result := FProjectManager.FileName;
end;

procedure TfLogIn.SetFileName(const Value: String);
begin
  FProjectManager.FileName := Value;
  FProjectManager.ReadProjects;
  FillProjects;
end;

function TfLogIn.GetPassword: String;
begin
  Result := ePassword.Text;
end;

procedure TfLogIn.SetPassword(const Value: String);
begin
  ePassword.Text := Value;
end;

function TfLogIn.GetProjectName: String;
begin
  Result := cbProject.Text;
end;

procedure TfLogIn.SetProjectName(const Value: String);
var
  i, CurIndex: Integer;
begin
  CurIndex := cbProject.ItemIndex;
  for i := 0 to cbProject.Items.Count - 1 do
    if SysUtils.AnsiCompareText(cbProject.Items[i], Value) = 0 then
      CurIndex := i;
  cbProject.ItemIndex := CurIndex;
end;

function TfLogIn.GetUserName: String;
begin
  Result := eUserName.Text;
end;

procedure TfLogIn.SetUserName(const Value: String);
begin
  eUserName.Text := Value;
end;

procedure TfLogIn.SetEditEnabled(Edit: TEdit; Enabled: Boolean);
begin
  Edit.Enabled := Enabled;
  if Enabled then
    Edit.Color := Graphics.clWindow else
    Edit.Color := Graphics.clBtnFace;
end;

procedure TfLogIn.cbProjectChange(Sender: TObject);
var
  Item: TsmxProjectItem;
  b: Boolean;
begin
  with TComboBox(Sender) do
  begin
    if ItemIndex <> -1 then
    begin
      Item := TsmxProjectItem(Items.Objects[ItemIndex]);
      if Assigned(Item) then
        b := not Item.IsWindowsAuthorization else
        b := True;
      SetEditEnabled(eUserName, b);
      SetEditEnabled(ePassword, b);
    end;
  end;
end;

procedure TfLogIn.bProjectsClick(Sender: TObject);
var
  s: String;
begin
  if smxPassword.ShowPassword(s) then
    if s = ProjectPassword then
    begin
      if smxProjects.ShowProjects(FProjectManager, FMacroUserName, FMacroPassword) then
        FProjectManager.WriteProjects else
        FProjectManager.ReadProjects;
      s := ProjectName;
      FillProjects;
      ProjectName := s;
    end else
      smxFuncs.Inf('¬веден неправильный пароль.');
end;

procedure TfLogIn.eUserNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Windows.VK_RETURN) and (ssCtrl in Shift) then
  begin
    FIsConf := True;
    ModalResult := mrOk;
  end;
end;

end.
