{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Base functions            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Аleksandr          }
{                                      }
{**************************************}

unit smxFuncs;

interface

uses
  Classes, Controls, Windows, TypInfo, XMLIntf, smxBaseClasses, smxTypes,
  smxBaseIntf, smxConsts;

function HotKeyToStr(AKey: Integer): String;
function StrToHotKey(const AStr: String): Integer;
function StrToDateTimeEx(const AStr: String): TDateTime;
function Ask(const AMsg: String; AType: Cardinal): Integer; overload;
function Ask(const AMsg: String): Boolean; overload;
function Inf(const AMsg: String; AType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
function StrToVar(const AValue: String): Variant;
function NewXMLDoc(const AVersion: String = smxConsts.cXMLDocVersion;
  const AEncoding: String = smxConsts.cXMLDocEncoding): IXMLDocument;
function FormatXMLText(const AText: String): String;
function UnFormatXMLText(const AText: String): String;
function GetSingleValue(const AValues: Variant; const ADefValue: Variant;
  AIndex: Integer = 0): Variant;
function SetToStr(PTI: PTypeInfo; Value: Byte; Brackets: Boolean = False): String;
function StrToSet(PTI: PTypeInfo; const Value: String): Byte;
function GetValueFieldName(const AFieldName: String): String;
function GetTextFieldName(const AFieldName: String): String;
function IsTextFieldName(const AFieldName: String): Boolean;
function GetParamValue(AParams: TsmxParams; const AParamName: String; const ADefValue: Variant): Variant;
function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: Integer): Integer; overload;
function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: String): String; overload;
function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: Extended): Extended; overload;
function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: TDateTime): TDateTime; overload;
function IntfInheritsFrom(AIntfInfo: PTypeInfo; const IID: TGUID): Boolean;
function GetRootControl(AControl: TWinControl): TWinControl;
function FindUniqueName(AOwner: TComponent; const AName: String): String;
function ResolvedClassType(AClassType: TPersistentClass): String;
function ResolvedClassTypeName(const AClassTypeName: String; AIsRegister: Boolean = False): TPersistentClass;
function VarToInt(const AValue: Variant): Integer;
function IntToVar(AValue: Integer): Variant;
function ClassNameWithoutPrefix(AClassName: ShortString): ShortString;

implementation

uses
  Forms, Menus, Graphics, SysUtils, StrUtils, Variants, XMLDoc, smxProcs;

function HotKeyToStr(AKey: Integer): String;
begin
  Result := Menus.ShortCutToText(TShortCut(AKey));
end;

function StrToHotKey(const AStr: String): Integer;
begin
  Result := Integer(Menus.TextToShortCut(AStr));
end;

function StrToDateTimeEx(const AStr: String): TDateTime;
begin
  if SysUtils.AnsiCompareText(AStr, 'Date') = 0 then
    Result := SysUtils.Date else
  if SysUtils.AnsiCompareText(AStr, 'Time') = 0 then
    Result := SysUtils.Time else
  if SysUtils.AnsiCompareText(AStr, 'Now') = 0 then
    Result := SysUtils.Now
  else
    Result := SysUtils.StrToDateDef(AStr, SysUtils.StrToDate('30.12.1899'));
end;

function Ask(const AMsg: String; AType: Cardinal): Integer;
var
  s: String;
begin
  case AType and $F0 of
    MB_ICONWARNING: s := 'Внимание';
    MB_ICONINFORMATION: s := 'Информация';
    MB_ICONQUESTION: s := 'Подтверждение';
    else s := 'Ошибка';
  end;
  Result := Forms.Application.MessageBox(PChar(AMsg), PChar(s), AType);
end;

function Ask(const AMsg: String): Boolean;
begin
  Result := Ask(AMsg, MB_YESNO + MB_ICONQUESTION) = mrYes;
end;

function Inf(const AMsg: String; AType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
begin
  Result := Ask(AMsg, AType);
end;

function StrToVar(const AValue: String): Variant;
begin
  if AValue = '' then
    Result := Variants.Null
  else
    Result := AValue;
end;

function NewXMLDoc(const AVersion: String = smxConsts.cXMLDocVersion;
  const AEncoding: String = smxConsts.cXMLDocEncoding): IXMLDocument;
begin
  Result := XMLDoc.NewXMLDocument;
  Result.XML.Text := smxConsts.cXMLDocTextDef;
  if AVersion <> smxConsts.cXMLDocVersion then
    Result.Version := WideString(AVersion);
  if AEncoding <> smxConsts.cXMLDocEncoding then
    Result.Encoding := WideString(AEncoding);
end;

function FormatXMLText(const AText: String): String;
begin
  Result := String(UTF8Decode(XMLDoc.FormatXMLData(WideString(AText))));
end;

function UnFormatXMLText(const AText: String): String;
var
  i: Integer;
  sl: TStrings;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := AText;
    for i := 0 to sl.Count - 1 do
      sl[i] := SysUtils.Trim(sl[i]);
    Result := String(UTF8Encode(WideString(StrUtils.AnsiReplaceStr(sl.Text, sLineBreak, ''))));
  finally
    sl.Free;
  end;
end;

function GetSingleValue(const AValues: Variant; const ADefValue: Variant;
  AIndex: Integer = 0): Variant;
begin
  Result := ADefValue;
  if Variants.VarIsArray(AValues) then
    if AIndex <= Variants.VarArrayHighBound(AValues, 1) then
      Result := AValues[AIndex];
end;

function SetToStr(PTI: PTypeInfo; Value: Byte; Brackets: Boolean = False): String;
var
  i: Integer;
  s: TIntegerSet;
begin
  Result := '';
  Integer(s) := Value;
  with TypInfo.GetTypeData(PTI)^ do
    for i := MinValue to MaxValue do
      if i in s then
        Result := Result + TypInfo.GetEnumName(PTI, i) + ',';
  if Result <> '' then
    Delete(Result, Length(Result), 1);
  if Brackets then
    Result := '[' + Result + ']';
end;

function ByteToInt(Value: Byte): Integer;
begin
  Result := Value;
end;

function StrToSet(PTI: PTypeInfo; const Value: String): Byte;

  function NextWord(var P: PChar): String;
  var
    i: Integer;
  begin
    i := 0;
    while not(P[i] in [',', ' ', #0, ']']) do
      Inc(i);
    SetString(Result, P, i);
    while P[i] in [',', ' ', ']'] do
      Inc(i);
    Inc(P, i);
  end;

var
  P: PChar;
  EnumName: String;
  EnumValue: Longint;
  SetValue: Integer;
begin
  Result := 0;
  SetValue := 0;
  if Value = '' then
    Exit;
  P := PChar(Value);
  // skip leading bracket and whitespace
  while P^ in ['[', ' '] do
    Inc(P);
  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := TypInfo.GetEnumValue(PTI, EnumName);
    if EnumValue >= 0 then
      Include(TIntegerSet(SetValue), EnumValue);
    EnumName := NextWord(P);
  end;
  Result := SetValue;
end;

function GetValueFieldName(const AFieldName: String): String;
begin
  Result := '';
  if AFieldName = '' then
    Exit;
  if IsTextFieldName(AFieldName) then
    Result := Copy(AFieldName, 1, Pos(smxConsts.cSuffixTextFieldName, AFieldName) - 1)
  else
    Result := AFieldName;
end;

function GetTextFieldName(const AFieldName: String): String;
begin
  Result := '';
  if AFieldName = '' then
    Exit;
  if IsTextFieldName(AFieldName) then
    Result := AFieldName
  else
    Result := AFieldName + smxConsts.cSuffixTextFieldName;
end;

function IsTextFieldName(const AFieldName: String): Boolean;
var
  NameLength, SuffixLength: Integer;
begin
  Result := False;
  NameLength := Length(AFieldName);
  SuffixLength := Length(smxConsts.cSuffixTextFieldName);
  if NameLength > SuffixLength then
    Result := SysUtils.AnsiCompareText(Copy(AFieldName, NameLength - SuffixLength + 1, SuffixLength),
      smxConsts.cSuffixTextFieldName) = 0;
end;

function GetParamValue(AParams: TsmxParams; const AParamName: String; const ADefValue: Variant): Variant;
var
  Param: TsmxParam;
begin
  Result := ADefValue;
  if Assigned(AParams) then
  begin
    Param := AParams.FindByName(AParamName);
    if Assigned(Param) then
      Result := Param.ParamValue;
  end;
end;

function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: Integer): Integer;
begin
  Result := SysUtils.StrToIntDef(Variants.VarToStrDef(GetParamValue(AParams, AParamName, Variants.Null), ''), ADefValue);
end;

function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: String): String;
begin
  Result := Variants.VarToStrDef(GetParamValue(AParams, AParamName, Variants.Null), ADefValue);
end;

function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: Extended): Extended;
begin
  Result := SysUtils.StrToFloatDef(Variants.VarToStrDef(GetParamValue(AParams, AParamName, Variants.Null), ''), ADefValue);
end;

function GetParamValueAs(AParams: TsmxParams; const AParamName: String; const ADefValue: TDateTime): TDateTime;
begin
  Result := SysUtils.StrToDateTimeDef(Variants.VarToStrDef(GetParamValue(AParams, AParamName, Variants.Null), ''), ADefValue);
end;

function IntfInheritsFrom(AIntfInfo: PTypeInfo; const IID: TGUID): Boolean;
var
  IntfTypeData: PTypeData;
begin
  Result := False;
  IntfTypeData := TypInfo.GetTypeData(AIntfInfo);
  while Assigned(IntfTypeData) and not Result do
  begin
    Result := SysUtils.IsEqualGUID(IntfTypeData^.Guid, IID);
    IntfTypeData := TypInfo.GetTypeData(IntfTypeData^.IntfParent^);
  end;
end;

function GetRootControl(AControl: TWinControl): TWinControl;
begin
  Result := AControl;
  if Assigned(Result) then
    while Assigned(Result.Parent) do
      Result := Result.Parent;
end;

function FindUniqueName(AOwner: TComponent; const AName: String): String;
var
  i: Integer;
begin
  Result := AName;
  if Assigned(AOwner) then
  begin
    i := 0;
    while Assigned(AOwner.FindComponent(Result)) do
    begin
      Inc(i);
      Result := SysUtils.Format('%s_%d', [AName, i]);
    end;
  end;
end;

function ResolvedClassType(AClassType: TPersistentClass): String;
begin
  Result := '';
  if Assigned(AClassType) then
  begin
    if Assigned(smxProcs.gClassTypeManagerIntf) then
      Result := smxProcs.gClassTypeManagerIntf.ResolvedClassType(AClassType);
    if Result = '' then
      Result := AClassType.ClassName;
  end;
end;

function ResolvedClassTypeName(const AClassTypeName: String; AIsRegister: Boolean = False): TPersistentClass;
begin
  Result := nil;
  if AClassTypeName <> '' then
  begin
    if Assigned(smxProcs.gClassTypeManagerIntf) then
      Result := smxProcs.gClassTypeManagerIntf.ResolvedClassTypeName(AClassTypeName, AIsRegister);
    if not Assigned(Result) then
      Result := Classes.GetClass(AClassTypeName);
  end;
end;

function VarToInt(const AValue: Variant): Integer;
begin
  if Variants.VarIsNull(AValue) then
    Result := 0
  else
    Result := AValue;
end;

function IntToVar(AValue: Integer): Variant;
begin
  if AValue = 0 then
    Result := Variants.Null
  else
    Result := AValue;
end;

function ClassNameWithoutPrefix(AClassName: ShortString): ShortString;
begin
  if Pos(smxConsts.cPrefixClassName, AClassName) = 1 then
    Result := Copy(AClassName, Length(smxConsts.cPrefixClassName) + 1, MaxInt)
  else
    Result := AClassName;
end;

end.
