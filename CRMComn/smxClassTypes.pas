unit smxClassTypes;

interface

uses
  Classes, Windows, smxClasses, smxDBIntf;

type
  TsmxProcAlgExecute = procedure(Algorithm: TsmxCustomAlgorithm; Params: Variant);

  TsmxFuncNewCell = function(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;

  TsmxFuncNewForm = function(AOwner: TComponent; const ADatabase: IsmxDatabase;
    ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;

  TsmxFuncIsCell = function(ACell: TsmxBaseCell; ACellClassName: String): Boolean;

  TsmxFuncFindFormByComboID = function(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;

  TsmxFuncFindFormByHandle = function(AHandle: HWND): TsmxCustomForm;

implementation

end.
 