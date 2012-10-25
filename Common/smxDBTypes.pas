unit smxDBTypes;

interface

uses
  smxDBIntf;

type
  TsmxFuncNewDatabase = function: IsmxDatabase;

  TsmxFuncNewDataSet = function(ADataSetType: TsmxDataSetType): IsmxDataSet;

  TsmxParamArray = array of IsmxParam;

  TsmxFieldArray = array of IsmxField;

  TsmxBaseSense = fsKey .. fsForeignKey;

implementation

end.
 