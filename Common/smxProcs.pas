{**************************************}
{                                      }
{            SalesMan v1.0             }
{           Base procedures            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov �leksandr          }
{                                      }
{**************************************}

unit smxProcs;

interface

uses
  Classes, ImgList, XMLIntf, smxBaseClasses, smxManagerIntf;

procedure GetFileFullVersion(const AFileName: String; var AVersMost, AVersLeast: Cardinal);
procedure LoadImagesFromStream(AImageList: TCustomImageList; AStream: TStream);
procedure StreamToStr(AStream: TStream; var AStr: String);
procedure StrToStream(const AStr: String; AStream: TStream);
procedure VarToParams(const AValue: Variant; AParams: TsmxParams);
procedure ParamsToVar(AParams: TsmxParams; var AValue: Variant);
procedure SplitByDelimiter(const AStr, ADelimiter: String;
  var AName, AValue: String);
procedure RegisterClasses(AClasses: array of TPersistentClass; AInstance: Cardinal = 0);
procedure UnRegisterClasses(AClasses: array of TPersistentClass; AInstance: Cardinal = 0);

var
  gCallBackManagerIntf: IsmxCallBackManager = nil;
  gStorageManagerIntf: IsmxStorageManager = nil;
  gLibraryManagerIntf: IsmxLibraryManager = nil;
  gDatabaseManagerIntf: IsmxDatabaseManager = nil;
  gFormManagerIntf: IsmxFormManager = nil;
  gImageListManagerIntf: IsmxImageListManager = nil;
  gClassTypeManagerIntf: IsmxClassTypeManager = nil;

implementation

uses
  Windows, SysUtils, CommCtrl, Variants;

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

procedure SplitByDelimiter(const AStr, ADelimiter: String;
  var AName, AValue: String);
var
  i: Integer;
begin
  i := Pos(ADelimiter, AStr);
  if i > 0 then
  begin
    AName := Copy(AStr, 1, i - 1);
    AValue := Copy(AStr, i + Length(ADelimiter), MaxInt);
  end else
  begin
    AName := AStr;
    AValue := '';
  end;
end;

procedure RegisterClasses(AClasses: array of TPersistentClass; AInstance: Cardinal = 0);
var
  i: Integer;
  ModuleName: String;
begin
  Classes.RegisterClasses(AClasses);
  if Assigned(gClassTypeManagerIntf) then
  begin
    if AInstance <> 0 then
      ModuleName := SysUtils.ExtractFileName(SysUtils.GetModuleName(AInstance))
    else
      ModuleName := '';
    for i := Low(AClasses) to High(AClasses) do
      if ModuleName <> '' then
        gClassTypeManagerIntf.RegisterClassTypeName(
          SysUtils.Format('%s%s%s', [AClasses[i].ClassName, gClassTypeManagerIntf.Delimiter, ModuleName]),
          AClasses[i])
      else
        gClassTypeManagerIntf.RegisterClassTypeName(
          AClasses[i].ClassName,
          AClasses[i]);
  end;
end;

procedure UnRegisterClasses(AClasses: array of TPersistentClass; AInstance: Cardinal = 0);
var
  i: Integer;
  ModuleName: String;
begin
  Classes.UnRegisterClasses(AClasses);
  if Assigned(gClassTypeManagerIntf) then
  begin
    if AInstance <> 0 then
      ModuleName := SysUtils.ExtractFileName(SysUtils.GetModuleName(AInstance))
    else
      ModuleName := '';
    for i := Low(AClasses) to High(AClasses) do
      if ModuleName <> '' then
        gClassTypeManagerIntf.UnRegisterClassTypeName(SysUtils.Format('%s%s%s',
          [AClasses[i].ClassName, gClassTypeManagerIntf.Delimiter, ModuleName]))
      else
        gClassTypeManagerIntf.UnRegisterClassTypeName(AClasses[i].ClassName);
  end;
end;

end.
