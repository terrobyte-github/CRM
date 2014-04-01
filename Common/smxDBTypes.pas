unit smxDBTypes;

interface

uses
  smxDBIntf, smxTypes;

type
  TsmxFuncNewDatabase = function: IsmxDatabase;

  TsmxFuncNewDataSet = function: IsmxDataSet;

  TsmxParamArray = array of IsmxParam;

  TsmxFieldArray = array of IsmxField;

  //TsmxBaseSense = fsKey .. fsForeignKey;

implementation

end.
 