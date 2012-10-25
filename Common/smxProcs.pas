unit smxProcs;

interface

uses
  Classes, ImgList, smxTypes;

procedure GetFileFullVersion(const AFileName: String; var AVersMost, AVersLeast: Cardinal);
procedure LoadImagesFromStream(AImageList: TCustomImageList; AStream: TStream);
procedure StreamToStr(AStream: TStream; var AStr: String);
procedure StrToStream(const AStr: String; AStream: TStream);

implementation

uses
  Windows, SysUtils, CommCtrl;

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

end.
