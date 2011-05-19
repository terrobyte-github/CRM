unit smxLibTypes;

interface

uses
  Classes, Windows, smxClasses, smxDBIntf;

type
  TsmxFuncNewCell = function(AOwner: TComponent; const ADB: IsmxDatabase;
    ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;
  TsmxFuncIsCell = function(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
  //TsmxFuncInf = function(M: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;

implementation

end.
