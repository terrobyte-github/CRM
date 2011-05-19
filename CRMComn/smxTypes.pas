unit smxTypes;

interface

uses
  Classes, Controls, Graphics, smxDBIntf;

type
  TsmxFuncCallBack = function(Index: Integer): Variant of object;

  //TsmxFuncGlobalValue = function(Name: String): Variant of object;

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

  TsmxParamLocation = (plInput, plConst, plOutput, plFilterPanel, plGrid,
    plParentFormFilterPanel, plParentFormGrid, plAlgorithmParams, plGlobalParams,
    plFormID, plKey, plValue, plResult, plMessage{, plID});

  TsmxFieldSense = (fsGeneral, fsKey, fsValue, fsResult, fsMessage{, fsParam});

  TsmxReturnType = (rtOpen, rtExecute);

  TsmxOperationMode = (omManual, omAutomatic);

  TsmxRequestSetting = record
    CfgID: Integer;
    Mode: TsmxOperationMode;
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

  TsmxVers = record
    Major, Minor, Release, Build: Word;
  end;

  TsmxLibType = (ltAlgorithm, ltCellClass, ltDatabaseIntf);

  TsmxLibTypes = set of TsmxLibType;

  TsmxProcLibInit = procedure(ACall: TsmxFuncCallBack);

  TsmxLibInfo = record
    FullName: String;
    Description: String;
    LibVers: TsmxVers;
    LibTypes: TsmxLibTypes;
    CompProgVers: TsmxVers;
    ProcLibInit: TsmxProcLibInit;
  end;

  TsmxProcLibInfo = procedure(var ALibInfo: TsmxLibInfo);

  TsmxFuncDatabaseCreate = function: IsmxDatabase;

  TsmxProjectConnection = record
    ProjectName: ShortString;
    LibraryName: ShortString;
    ProcedureName: ShortString;
    WindowsAuthorization: Boolean;
    DatabaseName: ShortString;
    DriverName: ShortString;
    LoginPrompt: Boolean;
    Params: ShortString;
  end;

implementation

end.
