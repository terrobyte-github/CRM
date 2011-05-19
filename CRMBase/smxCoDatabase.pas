unit smxCoDatabase;

interface

uses
  ComObj, smxDBIntf;

type
  TsmxCoDatabase = class(TComObject, IsmxDatabase)
  protected
    FDatabaseIntf: IsmxDatabase;
  public
    destructor Destroy; override;

    property Database: IsmxDatabase read FDatabaseIntf implements IsmxDatabase;
  end;

implementation

destructor TsmxCoDatabase.Destroy;
begin
  FDatabaseIntf := nil;
  inherited Destroy;
end;

end.
