unit smxFuncs;

interface

uses
  Classes, Windows, XMLIntf, smxTypes, smxConsts;

function HotKeyToStr(AKey: Integer): String;
function StrToHotKey(const AStr: String): Integer;
function StrToDateTimeEx(const AStr: String): TDateTime;
function Ask(const AMsg: String; AType: Cardinal): Integer; overload;
function Ask(const AMsg: String): Boolean; overload;
function Inf(const AMsg: String; AType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;
function StrToVar(const AValue: String): Variant;
function NewXML(const AEncoding: String = 'UTF-8'): IXMLDocument;
function FormatXMLText(const AText: String): String;
function UnFormatXMLText(const AText: String): String;
function NewResource(const AName: String): TResourceStream;
function GetSingleValue(const AValues: Variant; const ADefValue: Variant;
  AIndex: Integer = 0): Variant;
function DefCellFont: TsmxCellFont;
function DefCellText: TsmxCellText;

implementation

uses
  Controls, Forms, Menus, Graphics, SysUtils, StrUtils, Variants, XMLDoc;

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
    Result := Date else
  if SysUtils.AnsiCompareText(AStr, 'Time') = 0 then
    Result := Time else
  if SysUtils.AnsiCompareText(AStr, 'Now') = 0 then
    Result := Now else
    Result := SysUtils.StrToDateDef(AStr, SysUtils.StrToDate('30.12.1899'));
end;

function Ask(const AMsg: String; AType: Cardinal): Integer;
var
  s: String;
begin
  case AType and $F0 of
    MB_ICONWARNING: s := 'Внимание';
    MB_ICONINFORMATION: s := 'К информации';
    MB_ICONQUESTION: s := 'Подтверждение';
    else s := 'Ошибка!';
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
    Result := Variants.Null else
    Result := AValue;
end;

function NewXML(const AEncoding: String = 'UTF-8'): IXMLDocument;
begin
  Result := XMLDoc.NewXMLDocument;
  Result.Encoding := WideString(AEncoding);
  Result.XML.Text := smxConsts.cXMLDocTextDef;
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

function NewResource(const AName: String): TResourceStream;
begin
  Result := TResourceStream.Create(HInstance, AName, RT_RCDATA);
  Result.Position := 0;
end;

function GetSingleValue(const AValues: Variant; const ADefValue: Variant;
  AIndex: Integer = 0): Variant;
begin
  Result := ADefValue;
  if Variants.VarIsArray(AValues) then
    if AIndex <= Variants.VarArrayHighBound(AValues, 1) then
      Result := AValues[AIndex];
end;

function DefCellFont: TsmxCellFont;
begin
  with Result do
  begin
    Color := Integer(Graphics.clWindowText);
    Name := 'MS Sans Serif';
    Size := 8;
    Style := [];
  end;
end;

function DefCellText: TsmxCellText;
begin
  with Result do
  begin
    Caption := '';
    Align := taLeftJustify;
    Color := Integer(Graphics.clWindow);
    Font := DefCellFont;
  end;
end;

end.
