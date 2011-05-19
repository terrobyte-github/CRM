unit smxLibProcs;

interface

uses
  smxLibTypes;

procedure LibInfo(var ALibInfo: TsmxLibInfo);
procedure LibInit(ACall: TsmxFuncCallBack);
procedure FillInfo(ACompMajor, ACompMinor: Word; AProc: TsmxProcInitLib = nil;
  ATypes: TsmxLibTypes = [ltAlgorithm]; ADesc: String = '');

const
  Call: TsmxFuncCallBack = nil;

implementation

uses
  Forms, Windows, SysUtils, smxProcs;

var
  Info: TsmxLibInfo;

procedure LibInfo(var ALibInfo: TsmxLibInfo);
begin
  ALibInfo := Info;
end;

procedure LibInit(ACall: TsmxFuncCallBack);
begin
  Call := ACall;
  Application.Handle := HWND(Integer(Call(0)));
end;

procedure FillInfo(ACompMajor, ACompMinor: Word; AProc: TsmxProcInitLib = nil;
  ATypes: TsmxLibTypes = [ltAlgorithm]; ADesc: String = '');
var s: String; VersM, VersL: Cardinal;
begin
  s := GetModuleName(HInstance);
  with Info do
  begin
    FullName := s;
    Description := ADesc;
    GetFileFullVersion(PChar(s), VersM, VersL);
    with LibVers do
    begin
      Major := LongRec(VersM).Hi;
      Minor := LongRec(VersM).Lo;
      Release := LongRec(VersL).Hi;
      Build := LongRec(VersL).Lo;
    end;
    LibTypes := ATypes;
    with CompProgVers do
    begin
      Major := ACompMajor;
      Minor := ACompMinor;
      Release := 0;
      Build := 0;
    end;
    ProcInitLib := AProc;
  end;
end;

end.
