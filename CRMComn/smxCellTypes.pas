unit smxCellTypes;

interface

uses
  Classes, Windows, smxClasses, smxDBIntf;

type
  TsmxFuncNewCell = function(AOwner: TComponent; const ADB: IsmxDatabase;
    ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;

  TsmxFuncIsCell = function(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
  
  TsmxFuncFindFormByComboID = function(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;

  TsmxFuncFindFormByHandle = function(AHandle: HWND): TsmxBaseCell;

implementation

end.
 