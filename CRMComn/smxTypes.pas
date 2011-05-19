unit smxTypes;

interface

uses
  Classes, Controls, Graphics, smxDBIntf;

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

  TsmxParamLocation = (plInput, plConst, plOutput, plFilterDesk, plGrid,
    plParentFormFilterDesk, plParentFormGrid, plParentParams, plCommonParams,
    plKey, plValue, plResult, plMessage, plParentCfgID, plFormIntfID, plFormID);

  TsmxFieldSense = (fsGeneral, fsKey, fsValue, fsResult, fsMessage);

  TsmxPerformanceMode = (pmOpen, pmExecute);    

  TsmxOperationMode = (omManual, omAutomatic);

  TsmxRequestSetting = record
    CfgID: Integer;
    Operation: TsmxOperationMode;
    DatabaseName: String;
  end;

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

  TsmxFuncCreateDatabase = function: IsmxDatabase;

  TsmxFuncDatabaseCLSID = function: TGUID;

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
  end;

implementation

end.
