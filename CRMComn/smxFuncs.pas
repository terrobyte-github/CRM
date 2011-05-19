unit smxFuncs;

interface

uses
  Classes, Windows, smxClasses, smxCells, smxParams, smxTypes;

function GetCurrentForm(ACall: TsmxFuncCallBack): TsmxCustomForm;
function GetCurrentPage(ACall: TsmxFuncCallBack): TsmxCustomPage;
function GetCurrentRequest(ACall: TsmxFuncCallBack): TsmxCustomRequest;
function IDToCfgClass(ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCfgClass;
function IDToCellClass(ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCellClass;
function NewCfg(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCfg;
function NewCell(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCell;
function StreamToStr(Stream: TStream): String;
function StrToStream(const Str: String): TStream;
function HotKeyToStr(Key: Integer): String;
function StrToHotKey(const Str: String): Integer;
function StrToDateTimeEx(const Str: String): TDateTime;
function VarToParams(V: Variant): TsmxParams;
function ParamsToVar(Params: TsmxParams): Variant;
//function IsForm(ACell: TsmxBaseCell): Boolean;
//function IsFilter(ACell: TsmxBaseCell): Boolean;
function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function Ask(M: String; uType: Cardinal): Integer; overload;
function Ask(M: String): Boolean; overload;
function Inf(M: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;

implementation

uses
  Forms, Controls, Menus, SysUtils, Variants;

function GetCurrentForm(ACall: TsmxFuncCallBack): TsmxCustomForm;
var h: HWND; f: TsmxBaseCell;
begin
  Result := nil;
  h := GetActiveWindow;
  if h > 0 then
  begin
    f := TsmxFormManager(Integer(ACall(2))).FindByHandle(h);
    if Assigned(f) then
      if f is TsmxCustomForm then
        Result := TsmxCustomForm(f);
  end;
end;

function GetCurrentPage(ACall: TsmxFuncCallBack): TsmxCustomPage;
var f: TsmxCustomForm;
begin
  Result := nil;
  f := GetCurrentForm(ACall);
  if Assigned(f) then
    if Assigned(f.PageManager) then
      Result := f.PageManager.ActivePage;
end;

function GetCurrentRequest(ACall: TsmxFuncCallBack): TsmxCustomRequest;
var p: TsmxCustomPage;
begin
  Result := nil;
  p := GetCurrentPage(ACall);
  if Assigned(p) then
    if Assigned(p.Grid) then
      Result := p.Grid.Request;
end;

function IDToCfgClass(ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCfgClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ACfgID, ACall);
  try
    Result := t.CfgClass;
  finally
    t.Free;
  end;
end;

function IDToCellClass(ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCellClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ACfgID, ACall);
  try
    Result := t.CellClass;
  finally
    t.Free;
  end;
end;

function NewCfg(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCfg;
begin
  Result := IDToCfgClass(ACfgID, ACall).Create(AOwner, ACfgID, ACall);
end;

function NewCell(AOwner: TComponent; ACfgID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCell;
begin
  Result := IDToCellClass(ACfgID, ACall).Create(AOwner, ACfgID, ACall);
end;

function StreamToStr(Stream: TStream): String;
var Len: Integer;
begin
  with Stream do
  begin
    Position := 0;
    Len := Size;
    SetLength(Result, Len);
    ReadBuffer(Pointer(Result)^, Len);
  end;
end;

function StrToStream(const Str: String): TStream;
var Len: Integer;
begin
  Result := TMemoryStream.Create;
  with Result do
  begin
    Len := Length(Str);
    Size := Len;
    WriteBuffer(Pointer(Str)^, Len);
    Position := 0;
  end;
end;

function HotKeyToStr(Key: Integer): String;
begin
  Result := ShortCutToText(TShortCut(Key));
end;

function StrToHotKey(const Str: String): Integer;
begin
  Result := Integer(TextToShortCut(Str));
end;

function StrToDateTimeEx(const Str: String): TDateTime;
begin
  if AnsiCompareText(Str, 'today') = 0 then
    Result := Date else
  if AnsiCompareText(Str, 'time') = 0 then
    Result := Time else
  if AnsiCompareText(Str, 'now') = 0 then
    Result := Now else
    Result := StrToDateDef(Str, StrToDate('30.12.1899'));
end;

function VarToParams(V: Variant): TsmxParams;
var i: Integer;
begin
  Result := TsmxParams.Create(TsmxParam);
  if VarIsArray(V) and (VarArrayHighBound(V, 1) = 1) then
    if VarIsArray(V[0]) and VarIsArray(V[1])
      and (VarArrayHighBound(V[0], 1) = VarArrayHighBound(V[1], 1)) then
    begin
      for i := 0 to VarArrayHighBound(V[0], 1) do
        with Result.Add do
        begin
          ParamName := V[0][i];
          ParamValue := V[1][i];
        end;
    end;
end;

function ParamsToVar(Params: TsmxParams): Variant;
var i: Integer; v1, v2: Variant;
begin
  Result := Unassigned;
  if Params.Count > 0 then
  begin
    Result := VarArrayCreate([0, 1], varVariant);
    v1 := VarArrayCreate([0, Params.Count - 1], varVariant);
    v2 := VarArrayCreate([0, Params.Count - 1], varVariant);
    for i := 0 to Params.Count - 1 do
    begin
      v1[i] := Params[i].ParamName;
      v2[i] := Params[i].ParamValue;
    end;
    Result[0] := v1;
    Result[1] := v2;
  end;
end;

{function IsForm(ACell: TsmxBaseCell): Boolean;
begin
  if ACell is TsmxCustomForm then
    Result := True else
    Result := False;
end;}

{function IsFilter(ACell: TsmxBaseCell): Boolean;
begin
  if ACell is TsmxCustomFilter then
    Result := True else
    Result := False;
end;}

function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
var CellClass: TsmxBaseCellClass;
begin
  CellClass := TsmxBaseCellClass(FindClass(ACellClassName));
  if ACell is CellClass then
    Result := True else
    Result := False;
end;

function Ask(M: String; uType: Cardinal): Integer;
var s: String;
begin
  case uType and $F0 of
    MB_ICONWARNING: s := 'Внимание';
    MB_ICONINFORMATION: s := 'К информации';
    MB_ICONQUESTION: s := 'Подтверждение';
    else s := 'Ошибка!';
  end;
  Result := Application.MessageBox(PChar(M), PChar(s), uType);
end;

function Ask(M: String): Boolean;
begin
  Result := Ask(M, MB_YESNO + MB_ICONQUESTION) = mrYes;
end;

function Inf(M: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
begin
  Result := Ask(M, uType);
end;

end.
