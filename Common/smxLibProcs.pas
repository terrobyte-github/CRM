unit smxLibProcs;

interface

uses
  smxLibTypes;

procedure LibInfo(var ALibInfo: TsmxLibInfo);
procedure InitLib(ACall: TsmxFuncCallBack);
procedure FillInfo(ACompMajor, ACompMinor: Word; AProc: TsmxProcInitLib = nil;
  ATypes: TsmxLibTypes = [ltAlgorithm]; const ADesc: String = '');

var
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

procedure InitLib(ACall: TsmxFuncCallBack);
begin
  Call := ACall;
end;

procedure FillInfo(ACompMajor, ACompMinor: Word; AProc: TsmxProcInitLib = nil;
  ATypes: TsmxLibTypes = [ltAlgorithm]; const ADesc: String = '');
var
  s: String;
  VersM, VersL: Cardinal;
begin
  s := SysUtils.GetModuleName(HInstance);
  with Info do
  begin
    FullName := s;
    Description := ADesc;
    smxProcs.GetFileFullVersion(s, VersM, VersL);
    with LibVersion do
    begin
      Major := LongRec(VersM).Hi;
      Minor := LongRec(VersM).Lo;
      Release := LongRec(VersL).Hi;
      Build := LongRec(VersL).Lo;
    end;
    LibTypes := ATypes;
    with ProgVersion do
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
