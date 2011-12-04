unit smxBaseIntf;

interface

const
  IID_IsmxBaseInterface: TGUID = '{6785BD02-C530-4E8D-9001-A814F06B5045}';

type
  { IsmxBaseInterface }

  IsmxBaseInterface = interface(IInterface)
    ['{6785BD02-C530-4E8D-9001-A814F06B5045}']
    function GetVersion: String;
    function GetDescription: String;

    property Version: String read GetVersion;
  end;

implementation

end.
