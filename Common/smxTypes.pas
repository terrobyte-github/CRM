unit smxTypes;

interface

uses
  Classes, Controls, Graphics, Types;

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

  TsmxCellState = (csInitialized, csFinalized, csEventParam{, csDesigning});

  TsmxCellStates = set of TsmxCellState;

implementation

end.
