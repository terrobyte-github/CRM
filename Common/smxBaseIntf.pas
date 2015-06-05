unit smxBaseIntf;

interface

uses
  Classes;

const
  IID_IsmxBaseInterface: TGUID = '{6785BD02-C530-4E8D-9001-A814F06B5045}';
  IID_IsmxRefPersistent: TGUID = '{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}';
  IID_IsmxRefComponent: TGUID = '{48732C0C-F577-4061-9B97-926BA4B715DA}';
  IID_IsmxRefObject: TGUID = '{EF71E29F-E55F-44D6-8BF9-E4C751F4827F}';

type
  { IsmxBaseInterface }

  IsmxBaseInterface = interface(IInterface)
    ['{6785BD02-C530-4E8D-9001-A814F06B5045}']
    function GetController: IsmxBaseInterface;
    function GetDescription: String;
    function IsCountedObj{IsInterfacedObj}: Boolean;
    function GetVersion: String;

    property Description: String read GetDescription;
    property Version: String read GetVersion;
  end;

  { IsmxRefComponent }

  IsmxRefComponent = interface(IsmxBaseInterface)
    ['{48732C0C-F577-4061-9B97-926BA4B715DA}']
    function GetInternalRef: Pointer;
    function GetReference: TComponent;
    //function IsInterfaced: Boolean;
  end;

  { IsmxRefPersistent }

  IsmxRefPersistent = interface(IsmxBaseInterface)
    ['{AE2E363C-A7E6-47E9-ABE7-66E8C80473AB}']
    function GetInternalRef: Pointer;
    function GetReference: TPersistent;
    //function IsInterfaced: Boolean;
  end;

  { IsmxRefObject }

  IsmxRefObject = interface(IsmxBaseInterface)
    ['{EF71E29F-E55F-44D6-8BF9-E4C751F4827F}']
    function GetReference: TObject;
  end;

implementation

end.
