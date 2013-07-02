unit smxPassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfPassword = class(TForm)
    Label1: TLabel;
    ePassword: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
  public
  end;

function ShowPassword(var Password: String): Boolean;

implementation

{$R *.dfm}

function ShowPassword(var Password: String): Boolean;
var
  fPassword: TfPassword;
begin
  Result := False;
  fPassword := TfPassword.Create(nil);
  try
    Password := '';
    if fPassword.ShowModal = mrOk then
    begin
      Password := fPassword.ePassword.Text;
      Result := True;
    end;
  finally
    fPassword.Free;
  end;
end;

end.
