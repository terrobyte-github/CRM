{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Database types            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov �leksandr          }
{                                      }
{**************************************}

unit smxDBTypes;

interface

uses
  smxDBIntf;

type
  TsmxFuncNewDatabase = function: IsmxDatabase;

  TsmxFuncNewDataSet = function: IsmxDataSet;

  TsmxParamArray = array of IsmxParam;

  TsmxFieldArray = array of IsmxField;

implementation

end.
