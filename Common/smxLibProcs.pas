unit smxLibProcs;

interface

uses
  Classes, smxLibTypes;

procedure LibInfo(var ALibInfo: TsmxLibInfo);
procedure InitLib(ACall: TsmxFuncCallBack);
procedure FillInfo(ACompMajor, ACompMinor: Word; AProc: TsmxProcInitLib = nil;
  ATypes: TsmxLibTypes = [ltAlgorithm]; const ADesc: String = '');
//procedure RegisterClasses(AClasses: array of TPersistentClass; AInstance: Cardinal = 0);

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

{procedure RegisterClasses(AClasses: array of TPersistentClass; AInstance: Cardinal = 0);
var
  i: Integer;
  ModuleName: String;
  BInstance: Cardinal;
  b: Boolean;
begin
  Classes.RegisterClasses(AClasses);
  if Assigned(smxProcs.gClassTypeManagerIntf) then
  begin
    if AInstance = 0 then
      BInstance := HInstance else
      BInstance := AInstance;
    ModuleName := SysUtils.ExtractFileName(SysUtils.GetModuleName(BInstance));
    b := System.IsLibrary;
    for i := Low(AClasses) to High(AClasses) do
      if b then
        smxProcs.gClassTypeManagerIntf.RegisterClassTypeName(SysUtils.Format('%s%s%s',
          [AClasses[i].ClassName, gClassTypeManagerIntf.Delimiter, ModuleName]))
      else
        smxProcs.gClassTypeManagerIntf.RegisterClassTypeName(AClasses[i].ClassName);
  end;
end;}

end.
