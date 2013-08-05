unit smxProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxBaseClasses, smxTypes;

procedure GetFileFullVersion(const AFileName: String; var AVersMost, AVersLeast: Cardinal);
procedure LoadImagesFromStream(AImageList: TCustomImageList; AStream: TStream);
procedure StreamToStr(AStream: TStream; var AStr: String);
procedure StrToStream(const AStr: String; AStream: TStream);
{procedure ReadFont(const ANode: IXMLNode; var AFont: TsmxCellFont);
procedure WriteFont(const ANode: IXMLNode; const AFont: TsmxCellFont);
procedure ReadText(const ANode: IXMLNode; var AText: TsmxCellText);
procedure WriteText(const ANode: IXMLNode; const AText: TsmxCellText);}
procedure VarToParams(const AValue: Variant; AParams: TsmxParams);
procedure ParamsToVar(AParams: TsmxParams; var AValue: Variant);

implementation

uses
  Windows, Graphics, SysUtils, CommCtrl, Variants, TypInfo, smxFuncs, smxConsts;

procedure GetFileFullVersion(const AFileName: String; var AVersMost, AVersLeast: Cardinal);
var
  s, h, w: DWORD;
  buf: PChar;
  pfi: PVSFixedFileInfo;
begin
  AVersMost := 0; AVersLeast := 0;
  s := Windows.GetFileVersionInfoSize(PChar(AFileName), h);
  if s > 0 then
  begin
    buf := SysUtils.AllocMem(s);
    try
      if Windows.GetFileVersionInfo(PChar(AFileName), h, s, buf) then
        if Windows.VerQueryValue(buf, '\' , Pointer(pfi), w) then
        begin
          AVersMost := pfi.dwFileVersionMS;
          AVersLeast := pfi.dwFileVersionLS;
        end;
    finally
      FreeMem(buf, s);
    end;
  end;
end;

procedure LoadImagesFromStream(AImageList: TCustomImageList; AStream: TStream);
var
  LAdapter: TStreamAdapter;
begin
  AImageList.Handle := 0;
  LAdapter := TStreamAdapter.Create(AStream);
  try
    AImageList.Handle := CommCtrl.ImageList_Read(LAdapter);
  finally
    LAdapter.Free;
  end;
end;

procedure StreamToStr(AStream: TStream; var AStr: String);
var
  Len: Integer;
begin
  with AStream do
  begin
    Len := Size;
    SetLength(AStr, Len);
    Position := 0;
    ReadBuffer(Pointer(AStr)^, Len);
  end;
end;

procedure StrToStream(const AStr: String; AStream: TStream);
var
  Len: Integer;
begin
  with AStream do
  begin
    Len := Length(AStr);
    Size := Len;
    Position := 0;
    WriteBuffer(Pointer(AStr)^, Len);
  end;
end;

{procedure ReadFont(const ANode: IXMLNode; var AFont: TsmxCellFont);
begin
  AFont := smxFuncs.DefCellFont;
  if not Assigned(ANode) then
    Exit;
  AFont.Color := ANode.Attributes['Color'];
  AFont.Name := ANode.Attributes['Name'];
  AFont.Size := ANode.Attributes['Size'];
  AFont.Style := TFontStyles(smxFuncs.StrToSet(TypeInfo(TFontStyle), ANode.Attributes['Style']));
end;

procedure WriteFont(const ANode: IXMLNode; const AFont: TsmxCellFont);
begin
  if not Assigned(ANode) then
    Exit;
  ANode.ChildNodes.Clear;
  ANode.AttributeNodes.Clear;
  ANode.Attributes['Color'] := AFont.Color;
  ANode.Attributes['Name'] := AFont.Name;
  ANode.Attributes['Size'] := AFont.Size;
  ANode.Attributes['Style'] := smxFuncs.SetToStr(TypeInfo(TFontStyle), Byte(AFont.Style), True);
end;

procedure ReadText(const ANode: IXMLNode; var AText: TsmxCellText);
begin
  AText := smxFuncs.DefCellText;
  if not Assigned(ANode) then
    Exit;
  AText.Caption := ANode.Attributes['Caption'];
  AText.Alignment := TAlignment(TypInfo.GetEnumValue(TypeInfo(TAlignment), ANode.Attributes['Alignment']));
  AText.Color := ANode.Attributes['Color'];
  ReadFont(ANode.ChildNodes.FindNode(smxConsts.cFontNodeName), AText.Font);
end;

procedure WriteText(const ANode: IXMLNode; const AText: TsmxCellText);
begin
  if not Assigned(ANode) then
    Exit;
  ANode.ChildNodes.Clear;
  ANode.AttributeNodes.Clear;
  ANode.Attributes['Caption'] := AText.Caption;
  ANode.Attributes['Alignment'] := TypInfo.GetEnumName(TypeInfo(TAlignment), Integer(AText.Alignment));
  ANode.Attributes['Color'] := AText.Color;
  WriteFont(ANode.AddChild(smxConsts.cFontNodeName), AText.Font);
end;}

procedure VarToParams(const AValue: Variant; AParams: TsmxParams);
var
  i: Integer;
begin
  if not Assigned(AParams) then
    Exit;
  AParams.Clear;
  if Variants.VarIsArray(AValue) and (Variants.VarArrayHighBound(AValue, 1) = 1) then
    if Variants.VarIsArray(AValue[0]) and Variants.VarIsArray(AValue[1]) and
        (Variants.VarArrayHighBound(AValue[0], 1) = Variants.VarArrayHighBound(AValue[1], 1)) then
    begin
      for i := 0 to Variants.VarArrayHighBound(AValue[0], 1) do
        with AParams.Add do
        begin
          ParamName := AValue[0][i];
          ParamValue := AValue[1][i];
        end;
    end;
end;

procedure ParamsToVar(AParams: TsmxParams; var AValue: Variant);
var
  i: Integer;
  v1, v2: Variant;
begin
  if not Assigned(AParams) then
    Exit;
  AValue := Variants.Null;
  if AParams.Count > 0 then
  begin
    AValue := Variants.VarArrayCreate([0, 1], varVariant);
    v1 := Variants.VarArrayCreate([0, AParams.Count - 1], varVariant);
    v2 := Variants.VarArrayCreate([0, AParams.Count - 1], varVariant);
    for i := 0 to AParams.Count - 1 do
    begin
      v1[i] := AParams.Items[i].ParamName;
      v2[i] := AParams.Items[i].ParamValue;
    end;
    AValue[0] := v1;
    AValue[1] := v2;
  end;
end;

end.
