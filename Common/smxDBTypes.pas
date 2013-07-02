unit smxDBTypes;

interface

uses
  smxDBIntf;

type
  TsmxFuncNewDatabase = function: IsmxDatabase;

  TsmxFuncNewDataSet = function(DataSetType: TsmxDataSetType): IsmxDataSet;

  TsmxParamArray = array of IsmxParam;

  TsmxFieldArray = array of IsmxField;

  TsmxBaseSense = fsKey .. fsForeignKey;

implementation

end.
 