unit smxProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxTypes;

procedure GetFileFullVersion(const AFileName: String; var AVersMost, AVersLeast: Cardinal);
procedure LoadImagesFromStream(AImageList: TCustomImageList; AStream: TStream);
procedure StreamToStr(AStream: TStream; var AStr: String);
procedure StrToStream(const AStr: String; AStream: TStream);
procedure ReadFont(const ANode: IXMLNode; var AFont: TsmxCellFont);
procedure WriteFont(const ANode: IXMLNode; const AFont: TsmxCellFont);
procedure ReadText(const ANode: IXMLNode; var AText: TsmxCellText);
procedure WriteText(const ANode: IXMLNode; const AText: TsmxCellText);

implementation

uses
  Windows, Graphics, SysUtils, CommCtrl, smxFuncs, smxConsts;

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

procedure ReadFont(const ANode: IXMLNode; var AFont: TsmxCellFont);
begin
  AFont := smxFuncs.DefCellFont;
  if not Assigned(ANode) then
    Exit;
  AFont.Color := ANode.Attributes['Color'];
  AFont.Name := ANode.Attributes['Name'];
  AFont.Size := ANode.Attributes['Size'];
  AFont.Style := TFontStyles(Byte(ANode.Attributes['Style']));
end;

procedure WriteFont(const ANode: IXMLNode; const AFont: TsmxCellFont);
begin
  ANode.ChildNodes.Clear;
  ANode.AttributeNodes.Clear;
  if not Assigned(ANode) then
    Exit;
  ANode.Attributes['Color'] := AFont.Color;
  ANode.Attributes['Name'] := AFont.Name;
  ANode.Attributes['Size'] := AFont.Size;
  ANode.Attributes['Style'] := Byte(AFont.Style);
end;

procedure ReadText(const ANode: IXMLNode; var AText: TsmxCellText);
begin
  AText := smxFuncs.DefCellText;
  if not Assigned(ANode) then
    Exit;
  AText.Caption := ANode.Attributes['Caption'];
  AText.Align := ANode.Attributes['Align'];
  AText.Color := ANode.Attributes['Color'];
  ReadFont(ANode.ChildNodes.FindNode(smxConsts.cFontNodeName), AText.Font);
end;

procedure WriteText(const ANode: IXMLNode; const AText: TsmxCellText);
begin
  ANode.ChildNodes.Clear;
  ANode.AttributeNodes.Clear;
  if not Assigned(ANode) then
    Exit;
  ANode.Attributes['Caption'] := AText.Caption;
  ANode.Attributes['Align'] := AText.Align;
  ANode.Attributes['Color'] := AText.Color;
  WriteFont(ANode.AddChild(smxConsts.cFontNodeName), AText.Font);
end;

end.
