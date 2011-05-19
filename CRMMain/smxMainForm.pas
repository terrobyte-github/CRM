unit smxMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
//{$R ..\Resource\pic.res}

type
  _TCustomImageList = class(TCustomImageList)
  end;

procedure TForm1.FormCreate(Sender: TObject);
var rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, 'pic', RT_RCDATA);
  try
    _TCustomImageList(Form1.ImageList1).ReadData(rs);
  finally
    rs.Free;
  end;
end;

end.
