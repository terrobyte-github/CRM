unit smxMain;

interface

procedure Initialize;
procedure Finalize;
//function CreateMainForm: Boolean;
function LogIn: Boolean;
procedure LogOut;
function Start: Boolean;
procedure Finish;

implementation

uses
  Classes, ImgList, Forms, Controls, Windows, SysUtils, StdCtrls, ComObj,
  Graphics, IniFiles, StrUtils, Variants, TypInfo, smxBaseClasses, smxClasses,
  smxClassFuncs, smxFuncs, smxProcs, smxTypes, smxBaseIntf, smxDBIntf, smxConsts,
  smxManagerClasses, smxManagerIntf, smxDBClasses, smxPConsts, smxClassProcs,
  smxDBTypes, smxDBFuncs, smxLibTypes, smxLogIn;

{$I ..\Resource\smxConf.inc}

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

var
  gCallBackManager: TsmxCallBackManager = nil;
  gStorageManager: TsmxStorageManager = nil;
  gLibraryManager: TsmxLibraryManager = nil;
  gDatabaseManager: TsmxDatabaseManager = nil;
  gFormManager: TsmxFormManager = nil;
  gImageListManager: TsmxImageListManager = nil;
  gClassTypeManager: TsmxClassTypeManager = nil;
  //gMainConnection: TsmxConnection = nil;
  //gMainDatabase: TsmxCustomDatabase = nil;
  gMainForm: TsmxCustomForm = nil;
  //gCfgSelectRequest: TsmxCustomRequest = nil;
  gDeleteRequest: TsmxCustomRequest = nil;
  gInsertRequest: TsmxCustomRequest = nil;
  gUpdateRequest: TsmxCustomRequest = nil;
  gIsConstructor: Boolean = False;

procedure CreateGlobalObjects;
begin
  gCallBackManager := TsmxCallBackManager.Create(nil);
  smxProcs.gCallBackManagerIntf := gCallBackManager as IsmxCallBackManager;
  gStorageManager := TsmxStorageManager.Create(nil);
  smxProcs.gStorageManagerIntf := gStorageManager as IsmxStorageManager;
  gLibraryManager := TsmxLibraryManager.Create(nil);
  smxProcs.gLibraryManagerIntf := gLibraryManager as IsmxLibraryManager;
  gClassTypeManager := TsmxClassTypeManager.Create(nil);
  smxProcs.gClassTypeManagerIntf := gClassTypeManager as IsmxClassTypeManager;
  gImageListManager := TsmxImageListManager.Create(nil);
  smxProcs.gImageListManagerIntf := gImageListManager as IsmxImageListManager;
  gDatabaseManager := TsmxDatabaseManager.Create(nil);
  smxProcs.gDatabaseManagerIntf := gDatabaseManager as IsmxDatabaseManager;
  gFormManager := TsmxFormManager.Create(nil);
  smxProcs.gFormManagerIntf := gFormManager as IsmxFormManager;
  //gMainConnection := TsmxConnection.Create(nil);
end;

procedure DestroyGlobalObjects;
begin
  //gMainConnection.Free;
  smxProcs.gFormManagerIntf := nil;
  gFormManager.Free;
  smxProcs.gDatabaseManagerIntf := nil;
  gDatabaseManager.Free;
  smxProcs.gImageListManagerIntf := nil;
  gImageListManager.Free;
  smxProcs.gClassTypeManagerIntf := nil;
  gClassTypeManager.Free;
  smxProcs.gLibraryManagerIntf := nil;
  gLibraryManager.Free;
  smxProcs.gStorageManagerIntf := nil;
  gStorageManager.Free;
  smxProcs.gCallBackManagerIntf := nil;
  gCallBackManager.Free;
end;

function IniValue(const SectionName, ParamName: String): Variant;
begin
  Result := gStorageManager[SectionName + smxPConsts.cConfigurationDelimiter + ParamName];
end;

procedure AssignGlobalObjects;
var
  s: String;
  VersM, VersL: Cardinal;
  {rs: TResourceStream;
  FuncNewResource: TsmxFuncNewResource;}
begin
  gCallBackManager[smxConsts.cApplicationHandle] := Forms.Application.Handle;
  gCallBackManager[smxConsts.cStorageManager] := Integer(gStorageManager as IsmxStorageManager);
  gCallBackManager[smxConsts.cLibraryManager] := Integer(gLibraryManager as IsmxLibraryManager);
  gCallBackManager[smxConsts.cDatabaseManager] := Integer(gDatabaseManager as IsmxDatabaseManager);
  gCallBackManager[smxConsts.cFormManager] := Integer(gFormManager as IsmxFormManager);
  gCallBackManager[smxConsts.cImageListManager] := Integer(gImageListManager as IsmxImageListManager);
  gCallBackManager[smxConsts.cClassTypeManager] := Integer(gClassTypeManager as IsmxClassTypeManager);
  //gCallBackManager[smxConsts.cMainConnection] := Integer(gMainConnection as IsmxConnection);

  s := ParamStr(0);
  gStorageManager[smxPConsts.cProgExeName] := s;
  gStorageManager[smxPConsts.cProgExePath] := SysUtils.ExtractFilePath(s);
  smxProcs.GetFileFullVersion(s, VersM, VersL);
  gStorageManager[smxPConsts.cProgVersionMajor] := LongRec(VersM).Hi;
  gStorageManager[smxPConsts.cProgVersionMinor] := LongRec(VersM).Lo;
  gStorageManager[smxPConsts.cProgVersionRelease] := LongRec(VersL).Hi;
  gStorageManager[smxPConsts.cProgVersionBuild] := LongRec(VersL).Lo;
  gStorageManager[smxPConsts.cProgVersion] :=
    SysUtils.Format('%d.%d.%d.%d',
      [LongRec(VersM).Hi, LongRec(VersM).Lo, LongRec(VersL).Hi, LongRec(VersL).Lo]);

  //gLibraryManager.CallBackManager := gCallBackManager as IsmxCallBackManager;
  gLibraryManager.CheckHandle := HInstance;
  gLibraryManager.IsCheckComp :=
    IniValue(smxPConsts.cManagerSectionName, smxPConsts.cLibraryCheckComp);
  gLibraryManager.LibPath := gStorageManager[smxPConsts.cProgExePath] +
    IniValue(smxPConsts.cPathSectionName, smxPConsts.cPathLib);
  gLibraryManager.LibInfoProcName :=
    IniValue(smxPConsts.cManagerSectionName, smxPConsts.cLibraryLibInfoProcName);

  {@FuncNewResource := gLibraryManager.GetProcedure(
    Variants.VarToStr(gStorageManager[smxPConsts.cImageSectionName + '.' + smxPConsts.cImageLibraryName]),
    Variants.VarToStr(gStorageManager[smxPConsts.cImageSectionName + '.' + smxPConsts.cImageFuncName]));
  if Assigned(FuncNewResource) then
  begin
    rs := FuncNewResource(gStorageManager[smxPConsts.cImageSectionName + '.' + smxPConsts.cImageResourceName]);
    try
      smxProcs.LoadImagesFromStream(gImageList, rs);
    finally
      rs.Free;
    end;
  end;}
  //gImageListManager.LibraryManager := gLibraryManager as IsmxLibraryManager;
  gImageListManager.Delimiter :=
    IniValue(smxPConsts.cManagerSectionName, smxPConsts.cImageListDelimiter);
  gClassTypeManager.Delimiter :=
    IniValue(smxPConsts.cManagerSectionName, smxPConsts.cClassTypeDelimiter);
end;

procedure LoadClassLibraries;
var
  sr: TSearchRec;
  li: TsmxLibInfo;
begin
  if SysUtils.FindFirst(
      gStorageManager[smxPConsts.cProgExePath] +
        gStorageManager[smxPConsts.cPathSectionName + '.' + smxPConsts.cPathLib] +
        '*.dll',
      faAnyFile, sr) = 0 then
    try
      repeat
        if gLibraryManager.GetLibraryInfo(sr.Name, li) then
          if ltCellClass in li.LibTypes then
            gLibraryManager.AddLibrary(sr.Name);
      until SysUtils.FindNext(sr) <> 0;
    finally
      SysUtils.FindClose(sr);
    end;
end;

{procedure LoadImages;
var
  rs: TResourceStream;
  FuncNewResource: TsmxFuncNewResource;
begin
  @FuncNewResource := gLibraryManager.GetProcedure(
    gStorageManager[smxPConsts.cImageSectionName + '.' + smxPConsts.cImageLibraryName],
    gStorageManager[smxPConsts.cImageSectionName + '.' + smxPConsts.cImageFuncName]);
  if Assigned(FuncNewResource) then
  begin
    rs := FuncNewResource(gStorageManager[smxPConsts.cImageSectionName + '.' + smxPConsts.cImageResourceName]);
    try
      smxProcs.LoadImagesFromStream(_ImageList, rs);
    finally
      rs.Free;
    end;
  end;
end;}

procedure LoadCfg;
var
  f: TIniFile;
  sl, sl2: TStringList;
  i, j: Integer;
  h: Integer;
  s: String;
begin
  s := SysUtils.ExtractFilePath(ParamStr(0)) + smxPConsts.cConfigurationFileName;
  if not SysUtils.FileExists(s) then
  begin
    h := SysUtils.FileCreate(s);
    try
      SysUtils.FileWrite(h, cIni, Length(cIni));
    finally
      SysUtils.FileClose(h);
    end;
  end;
  f := TIniFile.Create(s);
  try
    sl := TStringList.Create;
    try
      sl2 := TStringList.Create;
      try
        f.ReadSections(sl);
        for i := 0 to sl.Count - 1 do
        begin
          f.ReadSectionValues(sl[i], sl2);
          for j := 0 to sl2.Count - 1 do
            gStorageManager[sl.Strings[i] + '.' + sl2.Names[j]] := sl2.Values[sl2.Names[j]];
        end;
      finally
        sl2.Free;
      end;
    finally
      sl.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure CreateMainObjects;

  function CreateDataSet: IsmxDataSet;
  var
    DataSetClassName: String;
    DataSetLibName: String;
    DataSetProcName: String;
    DataSetTypeName: String;
    DataSetClass: TPersistentClass;
    FuncNewDataSet: TsmxFuncNewDataSet;
    Value: Integer;
  begin
    Result := nil;

    DataSetClassName := IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetClassName);
    DataSetLibName := IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetLibName);
    DataSetProcName := IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetProcName);
    DataSetTypeName := Variants.VarToStr(IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetType));
    if DataSetLibName <> '' then
    begin
      if DataSetClassName <> '' then
      begin
        DataSetClass := gClassTypeManager.ResolvedClassTypeName(
          DataSetClassName + gClassTypeManager.Delimiter + DataSetLibName,
          True);
        if Assigned(DataSetClass) and DataSetClass.InheritsFrom(TsmxInterfacedComponent) then
          Result := TsmxInterfacedComponentClass(DataSetClass).Create(nil{, smxProcs.gDatabaseManagerIntf}) as IsmxDataSet;
      end else
      if DataSetProcName <> '' then
      begin
        FuncNewDataSet := gLibraryManager.GetProcedure(DataSetLibName, DataSetProcName);
        if Assigned(FuncNewDataSet) then
          Result := FuncNewDataSet({smxProcs.gDatabaseManagerIntf});
      end;
    end else
    if DataSetTypeName <> '' then
    begin
      Value := TypInfo.GetEnumValue(TypeInfo(TsmxDataSetType), DataSetTypeName);
      if (Value <> -1) and Assigned(smxClassProcs.gMainDatabaseIntf) then
        Result := smxClassProcs.gMainDatabaseIntf.NewDataSet(TsmxDataSetType(Value){, smxProcs.gDatabaseManagerIntf});
    end;
  end;

var
  //DataSetClassName{, IReqClsName}: String;
  //DataSetLibName{, ReqProcName}: String;
  //DataSetProcName: String;
  //DataSetClass: TPersistentClass;
  //IntfClass: TsmxInterfacedPersistentClass;
  //FuncNewDataSet: TsmxFuncNewDataSet;
  DataSet: IsmxDataSet;
begin
  (*DataSetClassName := IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetClassName);
  //IReqClsName := gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitIRequestClassName];
  DataSetLibName := IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetLibName);
  DataSetProcName := IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigDataSetProcName);
  //DataSetClass := nil;
  DataSet := nil;
  if {(IReqClsName <> '') and (}DataSetLibName <> ''{)} then
    if DataSetClassName <> '' then
    begin
      DataSetClass := gClassTypeManager.ResolvedClassTypeName(
        DataSetClassName + gClassTypeManager.Delimiter + DataSetLibName,
        True);
      if Assigned(DataSetClass) and DataSetClass.InheritsFrom(TsmxInterfacedComponent) then
        DataSet := TsmxInterfacedComponentClass(DataSetClass).Create(nil, smxProcs.gDatabaseManagerIntf) as IsmxDataSet;
    end else
    if DataSetProcName <> '' then
    begin
      FuncNewDataSet := gLibraryManager.GetProcedure(DataSetLibName, DataSetProcName);
      if Assigned(FuncNewDataSet) then
        DataSet := FuncNewDataSet(smxProcs.gDatabaseManagerIntf);
    end;*)
  DataSet := CreateDataSet;
  if Assigned(DataSet) then
  begin
    DataSet.DataSetName := 'SelectDataSet';
    if SysUtils.Supports(DataSet, IsmxDataEntity) then
      smxProcs.gDatabaseManagerIntf.InsertDataEntity(DataSet as IsmxDataEntity);

    smxClassProcs.gSelectDataSetIntf := DataSet;
  end;

  DataSet := CreateDataSet;
  if Assigned(DataSet) then
  begin
    DataSet.DataSetName := 'DeleteDataSet';
    if SysUtils.Supports(DataSet, IsmxDataEntity) then
      smxProcs.gDatabaseManagerIntf.InsertDataEntity(DataSet as IsmxDataEntity);

    smxClassProcs.gDeleteDataSetIntf := DataSet;
  end;

  DataSet := CreateDataSet;
  if Assigned(DataSet) then
  begin
    DataSet.DataSetName := 'InsertDataSet';
    if SysUtils.Supports(DataSet, IsmxDataEntity) then
      smxProcs.gDatabaseManagerIntf.InsertDataEntity(DataSet as IsmxDataEntity);

    smxClassProcs.gInsertDataSetIntf := DataSet;
  end;

  DataSet := CreateDataSet;
  if Assigned(DataSet) then
  begin
    DataSet.DataSetName := 'UpdateDataSet';
    if SysUtils.Supports(DataSet, IsmxDataEntity) then
      smxProcs.gDatabaseManagerIntf.InsertDataEntity(DataSet as IsmxDataEntity);

    smxClassProcs.gUpdateDataSetIntf := DataSet;
  end;
    //if gLibraryManager.AddLibrary(ReqLibName) <> -1 then
      //CellClass := TsmxBaseCellClass(Classes.FindClass(ReqClsName));
  {if ReqClsName <> '' then
    CellClass := TsmxBaseCellClass(Classes.FindClass(ReqClsName))
  else
    CellClass := nil;}



  //if Assigned(ReqClass) and ReqClass.InheritsFrom(TsmxCustomRequest) then
  //begin
    //if {(IReqClsName <> '') and (}ReqLibName <> ''{)} then
    //begin
      //if gLibraryManager.AddLibrary(ReqLibName) <> -1 then
      //begin
        //IntfClass := TsmxInterfacedPersistentClass(Classes.GetClass(IReqClsName));
        //if Assigned(IntfClass) then
          //gCfgRequest := TsmxCustomRequest(CellClass.Create(nil, IntfClass));



        //gCfgSelectRequest := TsmxCustomRequest(TsmxBaseCellClass(ReqClass).Create(nil));




      //end;
    //end
    { else
    if (ReqLibName <> '') and (ReqProcName <> '') then
    begin
      FuncNewDataSet := gLibraryManager.GetProcedure(ReqLibName, ReqProcName);
      if Assigned(FuncNewDataSet) then
        gCfgRequest := TsmxCustomRequest(CellClass.Create(nil, FuncNewDataSet));
    end}//;
    //if Assigned(gCfgRequest) then
      //smxClassProcs.gCfgSelectDataSet := gCfgSelectRequest as IsmxDataSet; //gCfgRequest.DataSet;
    //smxClassProcs.gCfgDeleteDataSet := gCfgRequest.DeleteDataSet;
    //smxClassProcs.gCfgInsertDataSet := gCfgRequest.InsertDataSet;
    //smxClassProcs.gCfgUpdateDataSet := gCfgRequest.UpdateDataSet;
  //end;




  //gMainConnection := TsmxConnection.Create(nil);
end;

procedure DestroyMainObjects;
begin
  //if Assigned(gCfgSelectRequest) then
  //begin
    smxClassProcs.gDeleteDataSetIntf := nil;
    smxClassProcs.gInsertDataSetIntf := nil;
    smxClassProcs.gUpdateDataSetIntf := nil;
    smxClassProcs.gSelectDataSetIntf := nil;
    //if Assigned(gDeleteRequest) then
      //SysUtils.FreeAndNil(gDeleteRequest);
    //if Assigned(gInsertRequest) then
      //SysUtils.FreeAndNil(gInsertRequest);
    {if Assigned(gUpdateRequest) then
      SysUtils.FreeAndNil(gUpdateRequest);}
    {if Assigned(gCfgSelectRequest) then
      SysUtils.FreeAndNil(gCfgSelectRequest);}
  //end;
  //smxClassProcs.gMainDatabaseIntf := nil;
  //gMainConnection.Free;
end;

procedure AssignMainObjects;
type
  TsmxSetting = record
    Name: String;
    DataType: TsmxDataType;
    DataSense: TsmxDataSense;
    Size: Integer
  end;

  TsmxSettings = array of TsmxSetting;

  TsmxSettingType = (stField, stParam);

  function GetSettings(SettingType: TsmxSettingType): TsmxSettings;
  const
    Names: array[TsmxSettingType] of String = (smxPConsts.cConfigFieldNames, smxPConsts.cConfigParamNames);
    DataTypes: array[TsmxSettingType] of String = (smxPConsts.cConfigFieldDataTypes, smxPConsts.cConfigParamDataTypes);
    DataSenses: array[TsmxSettingType] of String = (smxPConsts.cConfigFieldDataSenses, smxPConsts.cConfigParamDataSenses);
    Sizes: array[TsmxSettingType] of String = (smxPConsts.cConfigFieldSizes, smxPConsts.cConfigParamSizes);
  var
    s: TStrings;
    i: Integer;
    c: Integer;
  begin
    s := TStringList.Create;
    try
      s.Delimiter := smxPConsts.cValueDelimiter;
      s.DelimitedText := IniValue(smxPConsts.cConfigSectionName, Names[SettingType]);
      c := s.Count;
      SetLength(Result, c);
      for i := 0 to c - 1 do
        Result[i].Name := s[i];
      s.DelimitedText := IniValue(smxPConsts.cConfigSectionName, DataTypes[SettingType]);
      for i := 0 to c - 1 do
        Result[i].DataType := TsmxDataType(TypInfo.GetEnumValue(TypeInfo(TsmxDataType), s[i]));
      s.DelimitedText := IniValue(smxPConsts.cConfigSectionName, DataSenses[SettingType]);
      for i := 0 to c - 1 do
        Result[i].DataSense := TsmxDataSense(TypInfo.GetEnumValue(TypeInfo(TsmxDataSense), s[i]));
      s.DelimitedText := IniValue(smxPConsts.cConfigSectionName, Sizes[SettingType]);
      for i := 0 to c - 1 do
        Result[i].Size := SysUtils.StrToInt(s[i]);
    finally
      s.Free;
    end;
  end;

  procedure AssignSelectDataSet;
  var
    Settings: TsmxSettings;
    i: Integer;
  begin
    smxClassProcs.gSelectDataSetIntf.SQLText :=
      IniValue(smxPConsts.cConfigSectionName, smxPConsts.cConfigSQLText);
    smxClassProcs.gSelectDataSetIntf.ClearFields;
    Settings := GetSettings(stField);
    for i := Low(Settings) to High(Settings) do
      with smxClassProcs.gSelectDataSetIntf.AddField do
      begin
        FieldName := Settings[i].Name;
        DataType := Settings[i].DataType;
        DataSense := Settings[i].DataSense;
        Size := Settings[i].Size;
      end;
    smxClassProcs.gSelectDataSetIntf.ClearParams;
    Settings := GetSettings(stParam);
    for i := Low(Settings) to High(Settings) do
      with smxClassProcs.gSelectDataSetIntf.AddParam do
      begin
        ParamName := Settings[i].Name;
        DataType := Settings[i].DataType;
        DataSense := Settings[i].DataSense;
        Size := Settings[i].Size;
      end;
  end;

//var
  //Request: TsmxCustomRequest;
  //DataSet: IsmxDataSet;
begin
  //if Assigned(gMainConnection) then
    //gMainConnection.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
  //if Assigned(gCfgSelectRequest) then
  if Assigned(smxClassProcs.gSelectDataSetIntf) then
  begin
    //_TsmxBaseCell(gCfgSelectRequest).Cfg.XMLText :=
      //Variants.VarToStr(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitCfgSelectReqXMLText]);
    AssignSelectDataSet;
    smxClassProcs.gSelectDataSetIntf.Database := smxClassProcs.gMainDatabaseIntf;
    smxClassProcs.gSelectDataSetIntf.Prepared := True;
    //gCfgSelectRequest.IsRecieveCfg := False;


    //smxClassProcs.gSelectRequest.StorageManager := gStorageManager as IsmxStorageManager;
    //smxClassProcs.gSelectRequest.LibraryManager := gLibraryManager as IsmxLibraryManager;
    //smxClassProcs.gSelectRequest.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
    //smxClassProcs.gSelectRequest.FormManager := gFormManager as IsmxFormManager;
    //smxClassProcs.gSelectRequest.ImageListManager := gImageListManager as IsmxImageListManager;


    //gCfgSelectRequest.Database := smxClassProcs.gMainDatabaseIntf;
    //gCfgSelectRequest.Initialize;
    //gCfgSelectRequest.Prepare;



    //smxClassProcs.gSelectRequest.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
    //smxClassProcs.gSelectRequest.DatabaseName := gMainConnection.Database.DatabaseName;



    //smxClassProcs.gCfgSelectDataSetIntf := gCfgSelectRequest as IsmxDataSet; //gCfgRequest.DataSet;
  end;

  if Assigned(smxClassProcs.gDeleteDataSetIntf) then
    if SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitDeleteReqCfgID], 0) <> 0 then
    begin
      gDeleteRequest := TsmxCustomRequest(smxClassFuncs.NewCell(
        nil,
        gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitDeleteReqCfgID],
        smxClassProcs.gSelectDataSetIntf));
      try
      gDeleteRequest.Initialize;
      smxClassProcs.gDeleteDataSetIntf.AssignDataSet(gDeleteRequest.DataSet);
      smxClassProcs.gDeleteDataSetIntf.Database := smxClassProcs.gMainDatabaseIntf;
      smxClassProcs.gDeleteDataSetIntf.Prepared := True;
      //gDeleteRequest.Database := smxClassProcs.gMainDatabaseIntf;
      //gDeleteRequest.Prepare;
      //smxClassProcs.gDeleteDataSetIntf := gDeleteRequest as IsmxDataSet;
      finally
        gDeleteRequest.Free;
        gDeleteRequest := nil;
      end;
    end;

  if Assigned(smxClassProcs.gInsertDataSetIntf) then
    if SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitInsertReqCfgID], 0) <> 0 then
    begin
      gInsertRequest := TsmxCustomRequest(smxClassFuncs.NewCell(
        nil,
        gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitInsertReqCfgID],
        smxClassProcs.gSelectDataSetIntf));
      try
      gInsertRequest.Initialize;
      smxClassProcs.gInsertDataSetIntf.AssignDataSet(gInsertRequest.DataSet);
      smxClassProcs.gInsertDataSetIntf.Database := smxClassProcs.gMainDatabaseIntf;
      smxClassProcs.gInsertDataSetIntf.Prepared := True;
      //gInsertRequest.Database := smxClassProcs.gMainDatabaseIntf;
      //gInsertRequest.Prepare;
      //smxClassProcs.gInsertDataSetIntf := gInsertRequest as IsmxDataSet;
      finally
        gInsertRequest.Free;
        gInsertRequest := nil;
      end;
    end;

  if Assigned(smxClassProcs.gUpdateDataSetIntf) then
    if SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitUpdateReqCfgID], 0) <> 0 then
    begin
      try
      gUpdateRequest := TsmxCustomRequest(smxClassFuncs.NewCell(
        nil,
        gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitUpdateReqCfgID],
        smxClassProcs.gSelectDataSetIntf));
      gUpdateRequest.Initialize;
      //gUpdateRequest.Database := smxClassProcs.gMainDatabaseIntf;
      //gUpdateRequest.Prepare;
      //smxClassProcs.gUpdateDataSetIntf := gUpdateRequest as IsmxDataSet;
      smxClassProcs.gUpdateDataSetIntf.AssignDataSet(gUpdateRequest.DataSet);
      smxClassProcs.gUpdateDataSetIntf.Database := smxClassProcs.gMainDatabaseIntf;
      smxClassProcs.gUpdateDataSetIntf.Prepared := True;
      finally
        gUpdateRequest.Free;
        gUpdateRequest := nil;
      end;
    end;
end;

procedure Initialize;
begin
  CreateGlobalObjects;
  LoadCfg;
  //SaveProgInfo;
  AssignGlobalObjects;

  //LoadClassLibraries;

  CreateMainObjects;
  //LibManager.LibPath := ComStorage['Path.Lib'];
  //LibManager.ProcLibInfoName := ComStorage['LibManager.ProcLibInfo'];
  //LibManager.CheckComp := ComStorage['LibManager.CheckComp'];
  //LoadCell;
  //LoadImages;
end;

procedure Finalize;
begin
  DestroyMainObjects;
  DestroyGlobalObjects;
end;

{function CreateMainForm: Boolean;
var f: TsmxCustomForm; IntfID: Integer; c: TsmxBaseCell;
begin
  //IntfID := ComStorage['IntfID'];
  IntfID := 0;
  f := NewForm(nil, _DBConnection.Database, 1000002);
  //c := NewCell(nil, _DBConnection.Database, 1000002);
  if f is TsmxCustomMasterForm then
  //if c is TsmxCustomMasterForm then
  begin
    Application.ShowMainForm := False;
    Application.CreateForm(TForm, _MainForm);
    with TsmxCustomMasterForm(f) do
    begin
      Form := _MainForm;
      CommonStorage := ComStorage;
      LibraryManager := LibManager;
      DatabaseManager := DBManager;
      FormManager := FrmManager;
      ImageList := _ImgList;
      ShowForm;
    end;
    Result := True;
  end else
  begin
    f.Free;
    //raise EsmxCellError.CreateRes(@SCellBuildError);
    Result := False;
  end;
end;}

{function CheckIntf(const ADatabase: IsmxDatabase): Boolean;
var c: TsmxBaseCell; IntfID, IntfName: Variant;
begin
  Result := False;
  c := NewCell(nil, ADatabase, 1000277);
  try
    if c is TsmxCustomRequest then
    begin
      with TsmxCustomRequest(c) do
      begin
        Database := ADatabase;
        CommonStorage := ComStorage;
      end;
      if GetRequestKeyAndValue(TsmxCustomRequest(c), IntfID, IntfName) then
      begin
        Result := IntfID > 0;
        if Result then
        begin
          ComStorage['IntfID'] := IntfID;
          ComStorage['@IntfID'] := IntfID;
          ComStorage['IntfName'] := IntfName;
          ComStorage['@IntfName'] := IntfName;
        end;
      end;
    end;
  finally
    c.Free;
  end;
end;}

{function ConnectDatabase(AProjectName: String; ALogin: String = ''; APassword: String = ''): Boolean;
var pm: TsmxProjectManager; pr: TsmxProjectItem;
begin
  Result := False;
  pm := TsmxProjectManager.Create(nil);
  try
    pm.FileName := ComStorage['Path.Cfg'] + ComStorage['Init.FileProject']; //SFileProjectName;
    pm.ReadProjects;
    pr := pm.ProjectList.FindByName(AProjectName);
    if Assigned(pr) then
    begin
      _DBConnection := TsmxDBConnection.Create(nil);
      try
        with _DBConnection do
        begin
          DatabaseName := pr.DatabaseName;
          LibraryName := pr.LibraryName;
          FunctionNameOrProgID := pr.FunctionNameOrProgID;
          GenerationMode := pr.Generation;
          DriverName := pr.DriverName;
          LoginPrompt := pr.LoginPrompt;
          Params := pr.Params;
          if ALogin = '' then
            UserName := pr.UserName else
            UserName := ALogin;
          if APassword = '' then
            Password := pr.Password else
            Password := APassword;
          LibraryManager := LibManager;
          DatabaseManager := DBManager;
          ConnectToDatabase;
          //Result := CheckUser(_DBConnection.Database);
          Result := True;
          if Result then
          begin
            ComStorage['CfgDBName'] := pr.DatabaseName;
            ComStorage['@CfgDBName'] := pr.DatabaseName;
            ComStorage['RequestDBName'] := pr.DatabaseName;
            ComStorage['@RequestDBName'] := pr.DatabaseName;
          end;
        end;
      except
        _DBConnection.Free;
        raise;
      end;
    end;
  finally
    pm.Free;
  end;
end;}

{procedure DisconnectDatabase;
begin
  if Assigned(_DBConnection) then
  begin
    _DBConnection.Free;
    _DBConnection := nil;
  end;
end;}

function LogIn: Boolean;

  function ConnectDatabase(Connection: TsmxProjectConnection; UserName, Password: String): Boolean;
  var
    Database: IsmxDatabase;
    FuncNewDatabase: TsmxFuncNewDatabase;
    DatabaseClass: TPersistentClass;
    //Connection: IsmxConnection;
  begin
    Result := False;
    Database := nil;
    case Connection.Generation of
      gmFunction:
      begin
        FuncNewDatabase := gLibraryManager.GetProcedure(Connection.LibraryName, Connection.FuncOrClassNameOrProgID);
        if Assigned(FuncNewDatabase) then
          Database := FuncNewDatabase({gDatabaseManager as IsmxBaseInterface});
      end;
      gmCOM:
      begin
        if gLibraryManager.AddLibrary(Connection.LibraryName) <> -1 then
        begin
          ComObj.RegisterComServer(Connection.LibraryName);
          Database := ComObj.CreateComObject(
            ComObj.ProgIDToClassID(Connection.FuncOrClassNameOrProgID)) as IsmxDatabase;
        end;
      end;
      gmClass:
      begin
        if gLibraryManager.AddLibrary(Connection.LibraryName) <> -1 then
        begin
          DatabaseClass := gClassTypeManager.ResolvedClassTypeName(
            Connection.FuncOrClassNameOrProgID + gClassTypeManager.Delimiter + Connection.LibraryName,
            True);
          if Assigned(DatabaseClass) and DatabaseClass.InheritsFrom(TsmxInterfacedComponent) then
            Database := TsmxInterfacedComponentClass(DatabaseClass).Create({nil,} nil {gDatabaseManager as IsmxBaseInterface}) as IsmxDatabase;
        end;
      end;
    end;
    if Assigned(Database) then
    begin
      Database.DatabaseName := Connection.DatabaseName;
      Database.DriverName := Connection.DriverName;
      Database.ParamText :=
        SysUtils.StringReplace(
          SysUtils.StringReplace(
            Connection.Params,
            gStorageManager[smxPConsts.cManagerSectionName + '.' + smxPConsts.cProjectMacroUserName],
            StrUtils.IfThen(Connection.IsUseUser, Connection.UserName, UserName),
            [rfReplaceAll, rfIgnoreCase]),
          gStorageManager[smxPConsts.cManagerSectionName + '.' + smxPConsts.cProjectMacroPassword],
          StrUtils.IfThen(Connection.IsUseUser, Connection.Password, Password),
          [rfReplaceAll, rfIgnoreCase]);
      //gMainConnection.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
      //gMainConnection.Connect;
      //Result := gMainConnection.Connected;
      Database.Connected := True;
      if Database.Connected then
      begin
        //gMainConnection.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
        //gMainConnection.Database := Database;

        if SysUtils.Supports(Database, IsmxDataEntity) then
          (gDatabaseManager as IsmxDatabaseManager).InsertDataEntity(Database as IsmxDataEntity);
          //Connection.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;

        smxClassProcs.gMainDatabaseIntf := Database;
        Result := True;
      end;
    end;
  end;

  function CheckUser(UserName, Password: String; var UserID: Integer): Boolean;
  var
    Cell: TsmxBaseCell;
    Value: Variant;
  begin
    Result := True; //False;
    UserID := -1;
    {if SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitCheckUserCfgID], 0) > 0 then
    begin
      Cell := smxClassFuncs.NewCell(
        nil,
        gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitCheckUserCfgID],
        smxClassProcs.gSelectRequest);
      try
        if Cell is TsmxCustomRequest then
        begin
          TsmxCustomRequest(Cell).Initialize;
          //TsmxCustomRequest(Cell).DatabaseName := gMainConnection.Database.DatabaseName;
          //TsmxCustomRequest(Cell).DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
          TsmxCustomRequest(Cell).Database := gMainConnection.Database;
          TsmxCustomRequest(Cell).IsManualRefreshParams := True;
          if smxDBFuncs.GetValueByKey(
              TsmxCustomRequest(Cell).DataSet,
              Variants.VarArrayOf([UserName, Password]),
              Value,
              TsmxCustomRequest(Cell).PerformanceMode,
              fsValue,
              fsKey) then
          begin
            UserID := Value;
            Result := True;
          end;
        end;
      finally
        Cell.Free;
      end;
    end;}
  end;

  function CheckIntf(UserID: Integer; var IntfID: Integer; var IntfName: String): Boolean;
  var
    Cell: TsmxBaseCell;
    Value: Variant;
  begin
    Result := True; //False;
    IntfID := -1;
    IntfName := '';
    {if SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitCheckIntfCfgID], 0) > 0 then
    begin
      Cell := smxClassFuncs.NewCell(
        nil,
        gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitCheckIntfCfgID],
        smxClassProcs.gSelectRequest);
      try
        if Cell is TsmxCustomRequest then
        begin
          TsmxCustomRequest(Cell).Initialize;
          //TsmxCustomRequest(Cell).DatabaseName := gMainConnection.Database.DatabaseName;
          //TsmxCustomRequest(Cell).DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
          TsmxCustomRequest(Cell).Database := gMainConnection.Database;
          TsmxCustomRequest(Cell).IsManualRefreshParams := True;
          if smxDBFuncs.GetValueByKey(TsmxCustomRequest(Cell).DataSet, UserID, Value,
              TsmxCustomRequest(Cell).PerformanceMode, fsForeignKey, fsValue) then
          begin
            IntfID := smxFuncs.GetSingleValue(Value, -1, 0);
            IntfName := smxFuncs.GetSingleValue(Value, '', 1);
            Result := True;
          end;
        end;
      finally
        Cell.Free;
      end;
    end;}
  end;

  (*function CreateMainForm(IsConf: Boolean; IntfID: Integer): Boolean;
  var
    CfgID: Integer;
  begin
    Result := False;
    if IsConf then
      CfgID := SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitConfFormCfgID], 0) else
      CfgID := SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitMainFormCfgID], 0);
    if CfgID > 0 then
    begin
      Forms.Application.ShowMainForm := False;
      gMainForm := smxClassFuncs.NewForm(nil, CfgID);
      {gMainForm.StorageManager := gStorageManager as IsmxStorageManager;
      gMainForm.LibraryManager := gLibraryManager as IsmxLibraryManager;
      gMainForm.DatabaseManager := gDatabaseManager as IsmxDatabaseManager;
      gMainForm.FormManager := gFormManager as IsmxFormManager;
      gMainForm.ImageListManager := gImageListManager as IsmxImageListManager;}
      gMainForm.Initialize;
      gMainForm.IntfID := IntfID;
      Result := True;
    end;
  end;*)

  procedure SaveDefValues(const UserName, ProjectName: String);
  var
    f: TIniFile;
  begin
    f := TIniFile.Create(gStorageManager[smxPConsts.cProgExePath] + smxPConsts.cConfigurationFileName);
    try
      f.WriteString(smxPConsts.cDefValueSectionName,
        smxPConsts.cDefValueUserName,
        UserName);
      f.WriteString(smxPConsts.cDefValueSectionName,
        smxPConsts.cDefValueProjectName,
        ProjectName);
    finally
      f.Free;
    end;
  end;

var
  Connection: TsmxProjectConnection;
  UserName, Password: String;
  //IsConf: Boolean;
  UserID: Integer;
  IntfID: Integer;
  IntfName: String;
begin
  Result := False;
  UserName := gStorageManager[smxPConsts.cDefValueSectionName + '.' + smxPConsts.cDefValueUserName];
  Connection.ProjectName := gStorageManager[smxPConsts.cDefValueSectionName + '.' + smxPConsts.cDefValueProjectName];
  if smxLogIn.ShowLogIn(
      gStorageManager[smxPConsts.cProgExePath] +
        gStorageManager[smxPConsts.cPathSectionName + '.' + smxPConsts.cPathCfg] +
        gStorageManager[smxPConsts.cManagerSectionName + '.' + smxPConsts.cProjectFileName],
      gStorageManager[smxPConsts.cManagerSectionName + '.' + smxPConsts.cProjectMacroUserName],
      gStorageManager[smxPConsts.cManagerSectionName + '.' + smxPConsts.cProjectMacroPassword],
      Connection, UserName, Password, gIsConstructor) then
  begin
    SaveDefValues(UserName, Connection.ProjectName);
    gStorageManager[smxPConsts.cDefValueSectionName + '.' + smxPConsts.cDefValueUserName] := UserName;
    gStorageManager[smxPConsts.cDefValueSectionName + '.' + smxPConsts.cDefValueProjectName] := Connection.ProjectName;
    if ConnectDatabase(Connection, UserName, Password) then
    begin
      gStorageManager[smxPConsts.cParamSectionName + '.' + smxPConsts.cParamDatabaseName] := Connection.DatabaseName;
      //AssignMainObjects;
      if CheckUser(StrUtils.IfThen(Connection.IsUseUser, Connection.UserName, UserName),
          Password, UserID) then
      begin
        gStorageManager[smxPConsts.cParamSectionName + '.' + smxPConsts.cParamUserName] :=
          StrUtils.IfThen(Connection.IsUseUser, Connection.UserName, UserName);
        gStorageManager[smxPConsts.cParamSectionName + '.' + smxPConsts.cParamUserID] := UserID;
        if CheckIntf(UserID, IntfID, IntfName) then
        begin
          gStorageManager[smxPConsts.cParamSectionName + '.' + smxPConsts.cParamIntfName] := IntfName;
          gStorageManager[smxPConsts.cParamSectionName + '.' + smxPConsts.cParamIntfID] := IntfID;
          //if CreateMainForm(IsConf, IntfID) then
          //begin
            //gMainForm.Show;
            Result := True;
          //end else
            //smxFuncs.Inf('Невозможно создать главную форму.');
        end else
          smxFuncs.Inf('Интерфейс пользователя неопределен.');
      end else
        smxFuncs.Inf('Пользователь неопределен.');
    end else
      smxFuncs.Inf('Невозможно подключиться к БД.');
  end;
end;

procedure LogOut;
begin
  //if Assigned(gMainConnection.Database) then
    //if gMainConnection.Database.Connected then
  if Assigned(smxClassProcs.gMainDatabaseIntf) then
  begin
    if smxClassProcs.gMainDatabaseIntf.Connected then
    begin
      //if gMainConnection.Database.InTransaction then
        //gMainConnection.Database.RollbackTransaction;
      if smxClassProcs.gMainDatabaseIntf.InTransaction then
        smxClassProcs.gMainDatabaseIntf.RollbackTransaction;
      //gMainConnection.Database.Connected := False;
      smxClassProcs.gMainDatabaseIntf.Connected := False;
    end;
    smxClassProcs.gMainDatabaseIntf := nil;
  end;
end;

function Start: Boolean;

  function CreateMainForm(CfgID, IntfID: Integer): Boolean;
  begin
    Result := False;
    if CfgID > 0 then
    begin
      Forms.Application.ShowMainForm := False;
      gMainForm := smxClassFuncs.NewForm(nil, CfgID);
      gMainForm.Initialize;
      gMainForm.IntfID := IntfID;
      Result := True;
    end;
  end;

var
  CfgID, IntfID: Integer;
begin
  AssignMainObjects;
  Result := False;
  if gIsConstructor then
    CfgID := SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitConfFormCfgID], 0)
  else
    CfgID := SysUtils.StrToIntDef(gStorageManager[smxPConsts.cInitSectionName + '.' + smxPConsts.cInitMainFormCfgID], 0);
  IntfID := gStorageManager[smxPConsts.cParamSectionName + '.' + smxPConsts.cParamIntfID];
  if CreateMainForm(CfgID, IntfID) then
  begin
    gMainForm.Show;
    Result := True;
  end else
    smxFuncs.Inf('Невозможно создать главную форму.');
end;

procedure Finish;
begin
end;

//initialization
  //CreateGlobalObjects;

//finalization
  //DestroyGlobalObjects;

end.
