unit smxTypes;

interface

uses
  Classes, Controls, Graphics;

type
  TsmxFuncCallBack = function(I: Integer): Variant of object;

  TsmxReturnType = (rtOpen, rtExecute);

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

  TsmxOperationMode = (omManual, omAutomatic);

  TsmxRequestSetting = record
    ID: Integer;
    Mode: TsmxOperationMode;
  end;

  TsmxParamLocation = (plNowhere, plConst, plOut, plFilterPanel, plGrid,
    plParentFormFilterPanel, plParentFormGrid, plFormParams);

  TsmxFieldSense = (fsGeneral, fsKey, fsValue, fsResult, fsMsg);

  TsmxPositionSize = record
    Left, Top, Height, Width: Integer;
  end;
  
  TsmxControlCellSetting = record
    ID: Integer;
    Align: TAlign;
    Enable, Visible: Boolean;
    PositionSize: TsmxPositionSize;
  end;

  TsmxAlgorithmListSetting = record
    ID: Integer;
    IsCreateMenuItem: Boolean;
    IsCreateToolButton: Boolean;
  end;

  TsmxAlgorithmSetting = record
    ID: Integer;
    Caption: String;
    Enable: Boolean;
    HotKey: Integer;
    Visible: Boolean;
  end;

implementation

end.
