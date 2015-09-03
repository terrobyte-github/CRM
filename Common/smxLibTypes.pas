{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Library types             }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxLibTypes;

interface

type
  TsmxFuncCallBack = function(Index: Integer): Variant of object;

  TsmxVersion = record
    Major,
    Minor,
    Release,
    Build: Word;
  end;

  TsmxLibType = (ltAlgorithm, ltCellClass, ltDatabaseIntf, ltResource);

  TsmxLibTypes = set of TsmxLibType;

  TsmxProcInitLib = procedure(ACall: TsmxFuncCallBack);

  TsmxLibInfo = record
    FullName: String;
    Description: String;
    LibVersion: TsmxVersion;
    LibTypes: TsmxLibTypes;
    ProgVersion: TsmxVersion;
    ProcInitLib: TsmxProcInitLib;
  end;

  TsmxProcLibInfo = procedure(var ALibInfo: TsmxLibInfo);

  TsmxResourceType = (rtImageList, rtStringList, rtBinaryData);

implementation

end.
