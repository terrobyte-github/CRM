unit smxLibTypes;

interface

uses
  smxDBIntf;

type
  TsmxFuncCallBack = function(Index: Integer): Variant of object;

  TsmxVers = record
    Major, Minor, Release, Build: Word;
  end;

  TsmxLibType = (ltAlgorithm, ltCellClass, ltDatabaseIntf);

  TsmxLibTypes = set of TsmxLibType;

  TsmxProcInitLib = procedure(ACall: TsmxFuncCallBack);

  TsmxLibInfo = record
    FullName: String;
    Description: String;
    LibVers: TsmxVers;
    LibTypes: TsmxLibTypes;
    CompProgVers: TsmxVers;
    ProcInitLib: TsmxProcInitLib;
  end;

  TsmxProcLibInfo = procedure(var ALibInfo: TsmxLibInfo);

  TsmxFuncCommonParamValue = function(Name: String): Variant;

  TsmxFuncFindDatabaseByName = function(ADatabaseName: String): IsmxDatabase;

implementation

end.
