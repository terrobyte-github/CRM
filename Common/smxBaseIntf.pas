unit smxBaseIntf;

interface

uses
  Classes;

const
  IID_IsmxBaseInterface: TGUID = '{6785BD02-C530-4E8D-9001-A814F06B5045}';
  IID_IsmxRefPersistent: TGUID = '{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}';
  IID_IsmxRefComponent: TGUID = '{48732C0C-F577-4061-9B97-926BA4B715DA}';

type
  { IsmxBaseInterface }

  IsmxBaseInterface = interface(IInterface)
    ['{6785BD02-C530-4E8D-9001-A814F06B5045}']
    function GetVersion: String;
    function GetDescription: String;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  { IsmxRefComponent }

  IsmxRefComponent = interface(IsmxBaseInterface)
    ['{48732C0C-F577-4061-9B97-926BA4B715DA}']
    function GetInternalRef: Pointer;
    function GetReference: TComponent; 
  end;

  { IsmxRefPersistent }

  IsmxRefPersistent = interface(IsmxBaseInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    function GetInternalRef: Pointer;
    function GetReference: TPersistent;
  end;

implementation

end.
