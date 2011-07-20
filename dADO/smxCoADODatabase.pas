unit smxCoADODatabase;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, smxCoDatabase;

const
  CLSID_ADODatabase: TGUID = '{2230C586-6721-490D-994E-496F303E2325}';

type
  TsmxCoADODatabase = class(TsmxCoDatabase)
  public
    procedure Initialize; override;
  end;

function DatabaseCLSID: TGUID;

implementation

uses
  ComServ, smxADODB;

function DatabaseCLSID: TGUID;
begin
  Result := CLSID_ADODatabase;
end;

procedure TsmxCoADODatabase.Initialize;
begin
  inherited Initialize;
  FDatabaseIntf := NewADODatabase;
end;

initialization
  TComObjectFactory.Create(ComServer, TsmxCoADODatabase, CLSID_ADODatabase,
    'ADODatabase', '', ciMultiInstance, tmApartment);

end.
