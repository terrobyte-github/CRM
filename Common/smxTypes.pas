unit smxTypes;

interface

uses
  Classes, Controls, Graphics, DB, Types;

type
  {TsmxCellFont = record
    Color: Integer;
    Name: String;
    Size: Integer;
    Style: TFontStyles;
  end;

  TsmxCellText = record
    Caption: String;
    Alignment: TAlignment;
    Color: Integer;
    Font: TsmxCellFont;
  end;}

  TsmxOperationMode = (omManual, omAutomatic);

  {TsmxRequestSetting = record
    CfgID: Integer;
    Operation: TsmxOperationMode;
    DatabaseName: String;
  end;}

  TsmxGridOption = (goColLines, goRowLines, goRowSelect, goShowHeader,
    {goOwnerDrawHeader,} goEditing);

  TsmxGridOptions = set of TsmxGridOption;

  {TsmxControlCellSetting = record
    CfgID: Integer;
    Align: TAlign;
    Enable, Visible: Boolean;
    Position: TRect;
  end;}

  {TsmxAlgorithmListSetting = record
    CfgID: Integer;
    IsCreateMenuItem: Boolean;
    IsCreateToolButton: Boolean;
  end;}

  {TsmxAlgorithmSetting = record
    CfgID: Integer;
    Caption: String;
    Hint: String;
    HotKey: Integer;
    ImageIndex: Integer;
  end;}

  TsmxGenerationMode = (gmFunction, gmCOM, gmClass);

  TsmxProjectConnection = record
    ProjectName: ShortString;
    Generation: TsmxGenerationMode;
    LibraryName: ShortString;
    FuncOrClassNameOrProgID: ShortString;
    IsWindowsAuthorization: Boolean;
    DatabaseName: ShortString;
    DriverName: ShortString;
    Params: ShortString;
    IsUseUser: Boolean;
    UserName: ShortString;
    Password: ShortString;
  end;

  TsmxFuncDatabaseCLSID = function: TGUID;

  //TsmxFuncNewResource = function(Name: String): TResourceStream;

  {TsmxModifySetting = record
    InsertCfgID,
    UpdateCfgID,
    DeleteCfgID: Integer;
  end;}

  TsmxChangeMode = (cmReplace, cmAdd{, cmUnique});

  TsmxModifyRequest = (mrDelete, mrInsert, mrUpdate);

  TsmxColumnOption = (coEditing, coHasValue);

  TsmxColumnOptions = set of TsmxColumnOption;

  TsmxTreeOption = (toColLines, toRowLines, toRowSelect, toShowHeader,
    {toOwnerDrawHeader,} toEditing, toTreeLines);

  TsmxTreeOptions = set of TsmxTreeOption;

  TsmxFilterOption = (foApply, foPrepare, foEditing, foHasValue);

  TsmxFilterOptions = set of TsmxFilterOption;

  TsmxFormPosition = (fpDesigned, fpScreenCenter, fpOwnerFormCenter);

  TsmxFormBorder = (fbNone, fbDialog, fbSizeable);

  TsmxFormOption = (foFrameForm, foFreeOnClose);

  TsmxFormOptions = set of TsmxFormOption;

  TsmxPageManagerStyle = (pmsTab, pmsFlat);

  TsmxPagePosition = (ppTop, ppButtom, ppLeft, ppRight);

  TsmxMenuItemStyle = (misPoint, misDivider);

  TsmxToolItemStyle = (tisButton, tisCheck, tisDivider);

  TsmxStatusItemStyle = (sisText, sisDraw);

  TsmxEditorType = (etNone, etString, etNumber, etDate, etPickString,
    etButtonString);

  TsmxCellState = (csInitialized, csFinalized, {csModified, }csEventParam{, csDesigning});

  TsmxCellStates = set of TsmxCellState; {not need if csModified will}

  //TsmxCellStyle = (csRecieveCfg, csIsDesigning);
  TsmxCellOption = (coRecieveCfg, coIsDesigning); //, coEventParam must be alone
  //TsmxCellStyles = set of TsmxCellStyle;
  TsmxCellOptions = set of TsmxCellOption;

  TsmxDataSetType = (dstQuery, dstStoredProc);

  TsmxDataType = TFieldType;

  TsmxDataSense = (dsGeneral, dsKey, dsValue, dsResult, dsMessage,
    dsForeignKey);

  TsmxParamType = TParamType;

  TsmxDataLocation = (dlAssigned{, plKey, plValue, plResult, plMessage,
    plForeignKey, plInput, plOutput, plInOutput}, dlStorageParam, dlCellParam,
    dlParentCellParam, dlEventParam, dlParentEventParam, dlWorkCell,
    dlParentWorkCell, dlFilterDesk, dlParentFilterDesk, dlGrid, dlParentGrid,
    dlTree, dlParentTree);

  TsmxLocateOptions = TLocateOptions;

  TsmxPerformanceMode = (pmOpen, pmExecute);

implementation

end.
