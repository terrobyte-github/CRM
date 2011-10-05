unit smxClassTypes;

interface

uses
  Classes, Windows, smxClasses;

type
  TsmxProcAlgExecute = procedure(Algorithm: TsmxCustomAlgorithm; Params: Variant);

  //TsmxFuncNewCell = function(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer): TsmxBaseCell;

  //TsmxFuncNewForm = function(AOwner: TComponent; const ADatabase: IsmxDatabase;
    //ACfgID, AIntfID: Integer; AID: Integer = 0): TsmxCustomForm;

  //TsmxFuncIsCell = function(ACell: TsmxBaseCell; ACellClassName: String): Boolean;

  //TsmxFuncFindFormByComboID = function(ACfgID: Integer; AID: Integer = 0): TsmxCustomForm;

  //TsmxFuncFindFormByHandle = function(AHandle: HWND): TsmxCustomForm;

  //TsmxProcAddFormIntoManager = procedure(AForm: TsmxCustomForm);

  //TsmxProcDelFormFromManager = procedure(AForm: TsmxCustomForm);

implementation

end.

 