unit smxDBTypes;

interface

uses
  smxBaseIntf, smxDBIntf, smxTypes;

type
  TsmxFuncNewDatabase = function({const Controller: IsmxBaseInterface = nil}): IsmxDatabase;

  TsmxFuncNewDataSet = function({const Controller: IsmxBaseInterface = nil}): IsmxDataSet;

  TsmxParamArray = array of IsmxParam;

  TsmxFieldArray = array of IsmxField;

  //TsmxBaseSense = fsKey .. fsForeignKey;

implementation

end.
