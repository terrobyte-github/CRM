unit smxTypes;

interface

uses
  Classes, Controls, Graphics, Types;

type
  TsmxCellFont = record
    Color: Integer;
    Name: String;
    Size: Integer;
    Style: TFontStyles;
  end;

  TsmxCellText = record
    Caption: String;
    Align: TAlignment;
    Color: Integer;
    Font: TsmxCellFont;
  end;

  TsmxPerformanceMode = (pmOpen, pmExecute);

  TsmxOperationMode = (omManual, omAutomatic);

  {TsmxRequestSetting = record
    CfgID: Integer;
    Operation: TsmxOperationMode;
    DatabaseName: String;
  end;}

  TsmxGridOption = (goColLines, goRowLines, goRowSelect);

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

  TsmxGenerationMode = (gmFunction, gmCOM);

  TsmxProjectConnection = record
    ProjectName: ShortString;
    Generation: TsmxGenerationMode;
    LibraryName: ShortString;
    FunctionNameOrProgID: ShortString;
    WindowsAuthorization: Boolean;
    DatabaseName: ShortString;
    DriverName: ShortString;
    LoginPrompt: Boolean;
    Params: ShortString;
    UserName: ShortString;
    Password: ShortString;
  end;

  TsmxFuncDatabaseCLSID = function: TGUID;

  TsmxFuncNewResource = function(Name: String; Stream: TResourceStream): Boolean;

  {TsmxModifySetting = record
    InsertCfgID,
    UpdateCfgID,
    DeleteCfgID: Integer;
  end;}

  TsmxChangeMode = (cmReplace, cmAdd);

  TsmxModifyRequest = (mrDelete, mrInsert, mrUpdate);

  TsmxFilterOption = (foApplyValue, foPrepareValue, foApplyText, foPrepareText);

  TsmxFilterOptions = set of TsmxFilterOption;

  TsmxFormPosition = (fpDefault, fpDesktopCenter, fpOwnerFormCenter);

  TsmxFormBorder = (fbNone, fbDialog, fbSizeable);

  TsmxPageManagerStyle = (pmsTab, pmsFlat);

  TsmxMenuItemStyle = (misPoint, misDivider);

  TsmxToolItemStyle = (tisButton, tisCheck, tisDivider);

  TsmxStatusItemStyle = (sisText, sisDraw);

implementation

end.
