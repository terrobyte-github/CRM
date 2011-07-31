unit smxAddCell;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, smxClasses, smxDBIntf;

type
  TfrmAddCell = class(TForm)
    Edit1: TEdit;
    ComboBox1: TComboBox;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    Edit2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FTargetRequest: TsmxTargetRequest;
    FDatabaseIntf: IsmxDatabase;
    procedure SetDatabase(const Value: IsmxDatabase);
  public
    procedure FillTypes;
    property Database: IsmxDatabase read FDatabaseIntf write SetDatabase;
  end;

var
  frmAddCell: TfrmAddCell;

implementation

{$R *.dfm}

uses
  DB, smxFuncs;

procedure TfrmAddCell.FormCreate(Sender: TObject);
begin
  FTargetRequest := TsmxTargetRequest.Create(nil);
  RadioGroup1Click(nil);
end;

procedure TfrmAddCell.FormDestroy(Sender: TObject);
begin
  FTargetRequest.Free;
  FDatabaseIntf := nil;
end;

procedure TfrmAddCell.FillTypes;
var
  s: String;
begin
  ComboBox1.Clear;
  s := 'select * from tconfigs where conftype = 100';
  with FTargetRequest.ForRequest(s, dstQuery, True) do
  begin
    First;
    while not Eof do
    begin
      ComboBox1.AddItem(FieldByName('ConfName').Value, TObject(Integer(FieldByName('ConfID').Value)));
      Next;
    end;
  end;
end;

procedure TfrmAddCell.SetDatabase(const Value: IsmxDatabase);
begin
  FDatabaseIntf := Value;
  FTargetRequest.Database := Value;
  FillTypes;
end;

procedure TfrmAddCell.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: ComboBox1.Enabled := False;
    1: ComboBox1.Enabled := True;
  end;
end;

procedure TfrmAddCell.Button1Click(Sender: TObject);
begin
  if (RadioGroup1.ItemIndex = 1) and (ComboBox1.ItemIndex = -1) then
  begin
    inf('Не указан ConfConfID');
    ModalResult := mrNone;
    Exit;
  end;
  FTargetRequest['AConfID'] := Edit1.Text;
  FTargetRequest['AConfName'] := Edit2.Text;
  case RadioGroup1.ItemIndex of
    0:
    begin
      FTargetRequest['AConfConfID'] := '';
      FTargetRequest['AConfType'] := '100';
    end;
    1:
    begin
      FTargetRequest['AConfConfID'] := IntToStr(Integer(ComboBox1.Items.Objects[ComboBox1.ItemIndex]));
      FTargetRequest['AConfType'] := '200';
    end;
  end;
  FTargetRequest.DoExecute('cfg_add_item', dstStoredProc);
  Edit1.Text := FTargetRequest['ConfID'];
end;

end.
