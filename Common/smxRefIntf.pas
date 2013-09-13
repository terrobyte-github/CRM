unit smxRefIntf;

interface

uses
  Classes, smxBaseIntf;

const
  IID_IsmxRefInterface: TGUID = '{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}';

type
  { IsmxRefInterface }

  IsmxRefInterface = interface(IsmxBaseInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    function GetInternalRef: Pointer;
    function GetReference: TPersistent;
  end;

implementation

end.
