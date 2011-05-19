unit smxMain;

interface

procedure Initialize;
function CreateMainForm: Boolean;
//procedure SaveProgVers;

implementation

uses
  Classes, Windows, Forms, SysUtils, smxCommonStorage, smxCallBack, smxLibManager,
  smxDBManager, smxFormManager, smxGlobalVariables, smxClasses, smxCells,
  smxFuncs, smxProcs, smxConsts;

var
  MainForm: TForm = nil;

procedure SaveProgVers;
var VersM, VersL: Cardinal;
begin
  GetFileFullVersion(PChar(Application.ExeName), VersM, VersL);
  CommonStorage['ProgVersMajor'] := LongRec(VersM).Hi;
  CommonStorage['ProgVersMinor'] := LongRec(VersM).Lo;
  CommonStorage['ProgVersRelease'] := LongRec(VersL).Hi;
  CommonStorage['ProgVersBuild'] := LongRec(VersL).Lo;
  CommonStorage['ProgVers'] :=
    IntToStr(LongRec(VersM).Hi) + '.' +
    IntToStr(LongRec(VersM).Lo) + '.' +
    IntToStr(LongRec(VersL).Hi) + '.' +
    IntToStr(LongRec(VersL).Lo);
end;

procedure AssignCallBackParams;
begin
  CallBack[0] := Integer(Application.Handle);
  CallBack[1] := Integer(DBManager);
  //CallBack[2] := Integer(CommonStorage);
  //CallBack[3] := Integer(LibManager);
  CallBack[4] := Integer(FormManager);
  //CallBack[5] := Integer(ImageList);
  //CallBack[6] := Integer(TargetRequest);
  //CallBack[7] := Integer(@smxGlobal.DBListAdd);

  CallBack[141] := Integer(@smxFormManager.FindFormByComboID);
  CallBack[142] := Integer(@smxFormManager.FindFormByHandle);

  CallBack[201] := Integer(@smxFuncs.NewCell);
  CallBack[202] := Integer(@smxFuncs.IsCell);
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
  c := NewCell(nil, DBManager.DBConnections[0].Database, 1000218);
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
