unit smxMain;

interface

procedure Initialize;
function CreateMainForm: Boolean;
//procedure SaveProgVers;

implementation

uses
  Classes, Windows, Forms, SysUtils, smxClasses, smxCommonStorage, smxCallBack,
  smxLibManager, smxDBManager, smxFormManager, smxGlobalVariables, smxCells,
  smxFuncs, smxClassFuncs, smxProcs, smxConsts;

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
  //CallBack[1] := Integer(CommonStorage);
  //CallBack[2] := Integer(LibManager);
  //CallBack[3] := Integer(DBManager);
  //CallBack[4] := Integer(FormManager);
  //CallBack[5] := Integer(ImageList);
  //CallBack[6] := Integer(TargetRequest);
  //CallBack[10] := Integer(DBConnection.Database);

  //CallBack[101] := Integer(DBConnection.Database);

  CallBack[131] := Integer(@smxDBManager.FindDatabaseByName);

  CallBack[141] := Integer(@smxFormManager.FindFormByComboID);
  CallBack[142] := Integer(@smxFormManager.FindFormByHandle);

  CallBack[201] := Integer(@smxClassFuncs.NewCell);
  CallBack[202] := Integer(@smxClassFuncs.NewForm);
  CallBack[203] := Integer(@smxClassFuncs.IsCell);
end;

procedure Initialize;
begin
  LoadImage;
  SaveProgVers;
  AssignCallBackParams;
end;

function CreateMainForm: Boolean;
var f: TsmxCustomForm; IntfID: Integer;
begin
  IntfID := CommonStorage.ParamValues['IntfID'];
  f := NewForm(nil, DBManager.DBConnections[0].Database, 1000218, IntfID);
  if f is TsmxMainForm then
  begin
    Application.ShowMainForm := False;
    Application.CreateForm(TForm, MainForm);
    with TsmxMainForm(f) do
    begin
      Form := MainForm;
      ShowForm;
    end;
    Result := True;
  end else
  begin
    f.Free;
    raise EsmxCellError.CreateRes(@SCellBuildError);
    Result := False;
  end;
end;

end.
