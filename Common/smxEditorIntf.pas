unit smxEditorIntf;

interface

uses
  Controls, smxBaseIntf, smxTypes;

const
  IID_IsmxOuterEditor: TGUID = '{3F6C125A-EA34-48B4-9219-74B2160ED237}';

type
  { IsmxOuterEditor }

  IsmxOuterEditor = interface(IsmxBaseInterface)
    ['{3F6C125A-EA34-48B4-9219-74B2160ED237}']
    function GetColIndex: Integer;
    function GetControl: TWinControl;
    function GetEditorType: TsmxEditorType;
    function GetHolder: TWinControl;
    function GetRowIndex: Pointer;
    procedure SetEditorType(Value: TsmxEditorType);

    property ColIndex: Integer read GetColIndex;
    property Control: TWinControl read GetControl;
    property EditorType: TsmxEditorType read GetEditorType write SetEditorType;
    property Holder: TWinControl read GetHolder;
    property RowIndex: Pointer read GetRowIndex;
  end;

implementation

end.
