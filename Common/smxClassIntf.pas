unit smxClassIntf;

interface

uses
  Classes, Controls, smxBaseIntf, smxTypes;

const
  IID_IsmxOuterEditor: TGUID = '{4A673FFE-694E-46EE-84DF-5CBE80E2A263}';
  IID_IsmxGridEditor: TGUID = '{5255A278-E2AA-4060-AD50-18A544A601EE}';
  IID_IsmxTreeEditor: TGUID = '{3F6C125A-EA34-48B4-9219-74B2160ED237}';
  IID_IsmxObjectItem: TGUID = '{488FD65F-FEC5-4619-9251-BEC32DC20578}';
  IID_IsmxObjectList: TGUID = '{DF8EF255-3625-486A-903B-CAC224AF6837}';
  //IID_IsmxMultipleObjectList: TGUID = '{945A3E25-4BAE-4546-8E31-85B8C14C0488}';

type
  { IsmxOuterEditor }

  IsmxOuterEditor = interface(IsmxBaseInterface)
    ['{4A673FFE-694E-46EE-84DF-5CBE80E2A263}']
    function GetControl: TWinControl;
    function GetEditorType: TsmxEditorType;
    function GetHolder: TWinControl;
    procedure SetEditorType(Value: TsmxEditorType);

    property Control: TWinControl read GetControl;
    property EditorType: TsmxEditorType read GetEditorType write SetEditorType;
    property Holder: TWinControl read GetHolder;
  end;

  { IsmxGridEditor }

  IsmxGridEditor = interface(IsmxOuterEditor)
    ['{5255A278-E2AA-4060-AD50-18A544A601EE}']
    function GetColIndex: Integer;
    function GetRowIndex: Integer;

    property ColIndex: Integer read GetColIndex;
    property RowIndex: Integer read GetRowIndex;
  end;

  { IsmxTreeEditor }

  IsmxTreeEditor = interface(IsmxOuterEditor)
    ['{3F6C125A-EA34-48B4-9219-74B2160ED237}']
    function GetColIndex: Integer;
    function GetRowIndex: Pointer;

    property ColIndex: Integer read GetColIndex;
    property RowIndex: Pointer read GetRowIndex;
  end;

  { IsmxObjectItem }

  IsmxObjectItem = interface(IsmxBaseInterface)
    ['{488FD65F-FEC5-4619-9251-BEC32DC20578}']
    procedure ChangeObjectIndex(Value: Integer);
    procedure ChangeObjectOwner(Value: TPersistent);
  end;

  { IsmxObjectList }

  IsmxObjectList = interface(IsmxBaseInterface)
    ['{DF8EF255-3625-486A-903B-CAC224AF6837}']
    //procedure CheckObjectClass(ObjectClass: TPersistentClass);
    procedure CreateObject(Item: TObject); overload;
    procedure CreateObject(Item: TObject; ObjectClass: TPersistentClass); overload;
    procedure DestroyObject(Item: TObject);
  end;

  { IsmxMultipleObjectList }

  (*IsmxMultipleObjectList = interface(IsmxObjectList)
    ['{945A3E25-4BAE-4546-8E31-85B8C14C0488}']
    procedure CreateObject(Item: TObject; ObjectClass: TPersistentClass);
    procedure CheckObjectClass(ObjectClass: TPersistentClass);
  end;*)

implementation

end.
