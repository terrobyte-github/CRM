unit smxCoBDEDatabase;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, smxCoDatabase;

const
  CLSID_BDEDatabase: TGUID = '{61272B45-B747-473C-8ECA-8148E607B057}';

type
  TsmxCoBDEDatabase = class(TsmxCoDatabase)
  public
    procedure Initialize; override;
  end;

function DatabaseCLSID: TGUID;

implementation

uses
  ComServ, smxBDEDB;

function DatabaseCLSID: TGUID;
begin
  Result := CLSID_BDEDatabase;
end;

procedure TsmxCoBDEDatabase.Initialize;
begin
  inherited Initialize;
  FDatabaseIntf := NewBDEDatabase;
end;

initialization
  TComObjectFactory.Create(ComServer, TsmxCoBDEDatabase, CLSID_BDEDatabase,
    'BDEDatabase', '', ciMultiInstance, tmApartment);

end.
