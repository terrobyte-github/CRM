unit smxFuncs;

interface

uses
  Classes, Windows, smxClasses, smxCells, smxDBIntf, smxTypes;

function GetCurrentForm: TsmxCustomForm;
function GetCurrentPage: TsmxCustomPage;
function GetCurrentRequest: TsmxCustomRequest;
function CfgIDToCfgClass(const ADB: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
function CfgIDToCellClass(const ADB: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
function NewCfg(AOwner: TComponent; const ADB: IsmxDatabase; ACfgID: Integer): TsmxBaseCfg;
function NewCell(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
function StreamToStr(Stream: TStream): String;
function StrToStream(const Str: String): TStream;
function HotKeyToStr(Key: Integer): String;
function StrToHotKey(const Str: String): Integer;
function StrToDateTimeEx(const Str: String): TDateTime;
function VarToParams(V: Variant): TsmxParams;
function ParamsToVar(Params: TsmxParams): Variant;
function IsCell(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
function Ask(M: String; uType: Cardinal): Integer; overload;
function Ask(M: String): Boolean; overload;
function Inf(M: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
function VarStringToVar(V: Variant): Variant;
function ParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant;
function PerformRequest(ARequest: TsmxCustomRequest; Same: Boolean = False): Boolean;
function ActiveRequest(AForm: TsmxCustomForm): TsmxCustomRequest;
function FormatXMLText(AText: String): String;
function UnFormatXMLText(AText: String): String;

implementation

uses
  Forms, Controls, Menus, SysUtils, StrUtils, Variants, XMLDoc, smxFormManager;

function GetCurrentForm: TsmxCustomForm;
var h: HWND; f: TsmxBaseCell;
begin
  Result := nil;
  h := GetActiveWindow;
  if h > 0 then
  begin
    f := FormManager.FindByHandle(h);
    if Assigned(f) then
      if f is TsmxCustomForm then
        Result := TsmxCustomForm(f);
  end;
end;

function GetCurrentPage: TsmxCustomPage;
var f: TsmxCustomForm;
begin
  Result := nil;
  f := GetCurrentForm;
  if Assigned(f) then
    if Assigned(f.PageManager) then
      Result := f.PageManager.ActivePage;
end;

function GetCurrentRequest: TsmxCustomRequest;
var p: TsmxCustomPage;
begin
  Result := nil;
  p := GetCurrentPage;
  if Assigned(p) then
    if Assigned(p.Grid) then
      Result := p.Grid.Request;
end;

function CfgIDToCfgClass(const ADB: IsmxDatabase; ACfgID: Integer): TsmxBaseCfgClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ADB, ACfgID);
  try
    Result := t.CfgClass;
  finally
    t.Free;
  end;
end;

function CfgIDToCellClass(const ADB: IsmxDatabase; ACfgID: Integer): TsmxBaseCellClass;
var t: TsmxTypeCfg;
begin
  t := TsmxTypeCfg.Create(nil, ADB, ACfgID);
  try
    Result := t.CellClass;
  finally
    t.Free;
  end;
end;

function NewCfg(AOwner: TComponent; const ADB: IsmxDatabase; ACfgID: Integer): TsmxBaseCfg;
begin
  Result := CfgIDToCfgClass(ADB, ACfgID).Create(AOwner, ADB, ACfgID);
end;

function NewCell(AOwner: TComponent; const ADB: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
begin
  Result := CfgIDToCellClass(ADB, ACfgID).Create(AOwner, ADB, ACfgID, AID);
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
  if AnsiCompareText(Str, 'date') = 0 then
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

function VarStringToVar(V: Variant): Variant;
begin
  if VarToStr(V) = '' then
    Result := Null else
    Result := V;
end;

function ParamValueDef(AParams: TsmxParams; AName: String; ADefValue: Variant): Variant;
var p: TsmxParam;
begin
  Result := ADefValue;
  if Assigned(AParams) then
  begin
    p := AParams.FindByName(AName);
    if Assigned(p) then
      if not VarIsNull(p.ParamValue) then
        Result := p.ParamValue;
  end;
end;

function PerformRequest(ARequest: TsmxCustomRequest; Same: Boolean = False): Boolean;
var fld: IsmxField; prm: IsmxParam; res: Integer; msg: String;
begin
  Result := False;
  with ARequest do
  begin
    if not Database.InTransaction then
      Database.StartTrans;
    try
      res := -1; msg := '';
      Perform(Same);
      case CellDataSet.DataSetType of
        dstQuery:
        begin
          fld := FindFieldSense(fsResult);
          if Assigned(fld) then
            res := fld.Value;
          fld := FindFieldSense(fsMessage);
          if Assigned(fld) then
            msg := fld.Value;
        end;
        dstStoredProc:
        begin
          prm := FindParamLocation(plResult);
          if Assigned(prm) then
            res := prm.Value;
          prm := FindParamLocation(plMessage);
          if Assigned(prm) then
            msg := prm.Value;
        end;
      end;
      if res = 0 then
        Database.CommitTrans else
        Database.RollbackTrans;
      if msg <> '' then
        Inf(msg);
      Result := res = 0;
    except
      Database.RollbackTrans;
    end;
  end;
end;

function ActiveRequest(AForm: TsmxCustomForm): TsmxCustomRequest;
begin
  Result := nil;
  if Assigned(AForm) then
    if Assigned(AForm.PageManager) then
      if Assigned(AForm.PageManager.ActivePage) then
        if Assigned(AForm.PageManager.ActivePage.Grid) then
          Result := AForm.PageManager.ActivePage.Grid.Request;
end;

function FormatXMLText(AText: String): String;
begin
  Result := String(UTF8Decode(FormatXMLData(WideString(AText))));
end;

function UnFormatXMLText(AText: String): String;
var i: Integer; sl: TStrings;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := AText;
    for i := 0 to sl.Count - 1 do
      sl[i] := Trim(sl[i]);
    Result := String(UTF8Encode(WideString(AnsiReplaceStr(sl.Text, sLineBreak, ''))));
  finally
    sl.Free;
  end;
end;

end.
