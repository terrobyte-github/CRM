unit smxLibTypes;

interface

uses
  Classes, Windows, smxClasses, smxDBIntf;

type
  //TsmxFuncCallBack = function(Index: Integer): Variant of object;

  TsmxFuncNewCell = function(AOwner: TComponent; const ADB: IsmxDatabase;
    ACfgID: Integer; AID: Integer = 0): TsmxBaseCell;

  TsmxFuncIsCell = function(ACell: TsmxBaseCell; ACellClassName: String): Boolean;

  //TsmxFuncInf = function(M: String; uType: Cardinal = MB_OK + MB_ICONINFORMATION): Integer;

  TsmxFuncFindForm = function(ACfgID: Integer; AID: Integer = 0): TsmxBaseCell of object;

  {TsmxVers = record
    Major, Minor, Release, Build: Word;
  end;

  TsmxLibType = (ltAlgorithm, ltCellClass, ltDatabaseIntf);

  TsmxLibTypes = set of TsmxLibType;

  TsmxProcLibInit = procedure(ACall: TsmxFuncCallBack);

  TsmxLibInfo = record
    FullName: String;
    Description: String;
    LibVers: TsmxVers;
    LibTypes: TsmxLibTypes;
    CompProgVers: TsmxVers;
    ProcLibInit: TsmxProcLibInit;
  end;

  TsmxProcLibInfo = procedure(var ALibInfo: TsmxLibInfo);}

implementation

end.
