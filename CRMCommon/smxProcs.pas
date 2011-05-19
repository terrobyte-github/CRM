unit smxProcs;

interface

procedure GetFileFullVersion(AFileName: String; var AVersMost, AVersLeast: Cardinal);

implementation

uses
  Windows, SysUtils;

procedure GetFileFullVersion(AFileName: String; var AVersMost, AVersLeast: Cardinal);
var s, h, w: DWORD; buf: PChar; pfi: PVSFixedFileInfo;
begin
  AVersMost := 0; AVersLeast := 0;
  s := GetFileVersionInfoSize(PChar(AFileName), h);
  if s > 0 then
  begin
    buf := AllocMem(s);
    try
      if GetFileVersionInfo(PChar(AFileName), h, s, buf) then
        if VerQueryValue(buf, '\' , Pointer(pfi), w) then
        begin
          AVersMost := pfi.dwFileVersionMS;
          AVersLeast := pfi.dwFileVersionLS;
        end;
    finally
      FreeMem(buf, s);
    end;
  end;
end;

end.
