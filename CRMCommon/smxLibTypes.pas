unit smxLibTypes;

interface

uses
  Classes, smxDBIntf;

type
  TsmxFuncCallBack = function(Index: Integer): Variant of object;

  TsmxVers = record
    Major, Minor, Release, Build: Word;
  end;

  TsmxLibType = (ltAlgorithm, ltCellClass, ltDatabaseIntf, ltResource);

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

  TsmxResourceType = (rtImageList);

  //TsmxProcLoadResource = procedure(AStream: TResourceStream; AName: String);

  //TsmxFuncFindCommonParamByName = function(AName: String): Variant;

  //TsmxFuncFindDatabaseByName = function(ADatabaseName: String): IsmxDatabase;

  //TsmxFuncFindProcedureByName = function(ALibName, AProcName: String): Pointer;
  
implementation

end.
