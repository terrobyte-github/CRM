unit smxDLLTypes;

interface

uses
  Classes, Windows, smxClasses, smxTypes;

type
  TsmxFuncNewCell = function(AOwner: TComponent; AID: Integer; ACall: TsmxFuncCallBack): TsmxBaseCell;
  TsmxFuncIsCell = function(ACell: TsmxBaseCell; ACellClassName: String): Boolean;
  TsmxFuncInf = function(M: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;

implementation

end.
