unit smxBaseIntf;

interface

const
  IID_IsmxBaseInterface: TGUID = '{6785BD02-C530-4E8D-9001-A814F06B5045}';
  //IID_IsmxRefInterface: TGUID = '{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}';

type
  { IsmxBaseInterface }

  IsmxBaseInterface = interface(IInterface)
    ['{6785BD02-C530-4E8D-9001-A814F06B5045}']
    function GetVersion: String;
    function GetDescription: String;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  { IsmxRefInterface }

  (*IsmxRefInterface = interface(IsmxBaseInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    function GetInternalRef: Pointer;
    function GetReference: Pointer;
    //property Name: String read GetName write SetName;
  end;*)

  { IsmxFreeNotification }

  (*IsmxFreeNotification = interface(IInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    procedure FreeNotification(const Sender: IsmxFreeNotification);
    function GetOwnerNotification: IsmxOwnerNotification;
    procedure InsertFreeNotification(const Receiver: IsmxFreeNotification);
    procedure RemoveFreeNotification(const Receiver: IsmxFreeNotification);
  end;*)

implementation

end.
