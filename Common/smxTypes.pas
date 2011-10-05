unit smxTypes;

interface

uses
  Classes, Controls, Graphics;

type
  TsmxCellFont = record
    Color: Integer;
    Name: String;
    Size: Integer;
    Style: TFontStyles;
  end;

  TsmxCellText = record
    Text: String;
    Align: TAlignment;
    Color: Integer;
    Font: TsmxCellFont;
  end;

  TsmxParamLocation = (plConst, plKey, plValue, plResult, plMessage,
    plForeignKey, plInput, plOutput, plFilterDesk, plGrid,
    plParentFormFilterDesk, plParentFormGrid, plCommonParams,
    plFormIntfID, plFormID);

  TsmxFieldSense = (fsGeneral, fsKey, fsValue, fsResult, fsMessage,
    fsForeignKey);

  TsmxPerformanceMode = (pmOpen, pmExecute);    

  TsmxOperationMode = (omManual, omAutomatic);

  TsmxRequestSetting = record
    CfgID: Integer;
    Operation: TsmxOperationMode;
    DatabaseName: String;
  end;

  TsmxGridOption = (goColLines, goRowLines, goRowSelect);

  TsmxGridOptions = set of TsmxGridOption;

  TsmxPositionSize = record
    Left, Top, Height, Width: Integer;
  end;
  
  TsmxControlCellSetting = record
    CfgID: Integer;
    Align: TAlign;
    Enable, Visible: Boolean;
    PositionSize: TsmxPositionSize;
  end;

  TsmxAlgorithmListSetting = record
    CfgID: Integer;
    IsCreateMenuItem: Boolean;
    IsCreateToolButton: Boolean;
  end;

  TsmxAlgorithmSetting = record
    CfgID: Integer;
    Caption: String;
    Enable: Boolean;
    HotKey: Integer;
    Visible: Boolean;
  end;

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

  TsmxFuncNewResource = function(AName: String): TResourceStream;

  TsmxModifySetting = record
    InsertCfgID,
    UpdateCfgID,
    DeleteCfgID: Integer;
  end;

implementation

end.
