unit smxMain;

interface

procedure Initialize;
function CreateMainForm: Boolean;
//procedure SaveProgVers;

implementation

uses
  Classes, Windows, Forms, SysUtils, smxClasses, smxCells, smxCallBack,
  smxGlobal, smxGlobalStorage, smxLibManager, smxFormManager, smxProcs,
  smxFuncs, smxConsts;

var
  MainForm: TForm = nil;

procedure SaveProgVers;
var VersM, VersL: Cardinal;
begin
  GetFileFullVersion(PChar(Application.ExeName), VersM, VersL);
  GlobalStorage['ProgVersMajor'] := LongRec(VersM).Hi;
  GlobalStorage['ProgVersMinor'] := LongRec(VersM).Lo;
  GlobalStorage['ProgVersRelease'] := LongRec(VersL).Hi;
  GlobalStorage['ProgVersBuild'] := LongRec(VersL).Lo;
  GlobalStorage['ProgVers'] :=
    IntToStr(LongRec(VersM).Hi) + '.' +
    IntToStr(LongRec(VersM).Lo) + '.' +
    IntToStr(LongRec(VersL).Hi) + '.' +
    IntToStr(LongRec(VersL).Lo);
end;

procedure AssignCallBackParams;
begin
  CallBack[0] := Integer(Application.Handle);
  CallBack[1] := Integer(Database);
  CallBack[2] := Integer(GlobalStorage);
  CallBack[3] := Integer(LibManager);
  CallBack[4] := Integer(FormManager);
  CallBack[5] := Integer(ImageList);
  CallBack[6] := Integer(TargetRequest);
  CallBack[101] := Integer(@smxFuncs.NewCell);
  CallBack[111] := Integer(@smxFuncs.IsCell);
  //CallBack[121] := Integer(@smxGlblParams.FuncGlobalValue);
  //CallBack[151] := Integer(@smxFuncs.Inf);
end;

procedure Initialize;
begin
  LoadImage;
  SaveProgVers;
  AssignCallBackParams;
end;

function CreateMainForm: Boolean;
var c: TsmxBaseCell;
begin
  c := NewCell(nil, Database, 1000218);
  if c is TsmxMainForm then
  begin
    Application.ShowMainForm := False;
    Application.CreateForm(TForm, MainForm);
    with TsmxMainForm(c) do
    begin
      Form := MainForm;
      ShowForm;
    end;
    Result := True;
  end else
  begin
    c.Free;
    raise EsmxCellError.CreateRes(@SCellBuildError);
    Result := False;
  end;
end;

end.
