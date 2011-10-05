{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Cells classes             }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxCells;

interface

uses
  Classes, Controls, ComCtrls, DB, DBGrids, Forms, ExtCtrls, StdCtrls, Menus,
  ActnList, Windows, ImgList, smxClasses, smxCfgs, smxWheelDBGrid, smxDBIntf,
  smxTypes, smxClassTypes;

type
  { TsmxRequest }

  TsmxRequest = class(TsmxCustomRequest)
  private
    FDataSetIntf: IsmxDataSet;
    function GetCfg: TsmxRequestCfg;
  protected
    procedure Initialize; override;
    procedure UnInitialize; override;
    //procedure SetDatabaseName(Value: String); override;
    procedure SetDatabase(const Value: IsmxDatabase); override;
    //procedure SetDBManager(Value: TsmxCustomDBManager); override;

    property Cfg: TsmxRequestCfg read GetCfg;
  public
    function FindFieldSense(AFieldSense: TsmxFieldSense; StartPos: Integer = 0): IsmxField; override;
    function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): IsmxParam; override;
    procedure Perform; override;
    procedure RefreshParams; override;
  end;

  { TsmxColumn }

  TsmxColumn = class(TsmxCustomColumn)
  private
    function GetCfg: TsmxColumnCfg;
  protected
    property Cfg: TsmxColumnCfg read GetCfg;
  end;

  { TsmxDBColumn }

  TsmxDBColumn = class(TsmxColumn)
  private
    FColumn: TColumn;
  protected
    function GetInternalObject: TObject; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property Column: TColumn read FColumn;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxGrid }

  TsmxGrid = class(TsmxCustomGrid)
  private
    function GetCfg: TsmxGridCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxGridCfg read GetCfg;
  end;

  { TsmxDBGrid }

  TsmxDBGrid = class(TsmxGrid)
  private
    FDataSource: TDataSource;
    FGrid: TsmxWheelDBGrid;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellCursor: TCursor; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellCursor(Value: TCursor); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;
    procedure SetRequest(Value: TsmxCustomRequest); override;

    property Grid: TsmxWheelDBGrid read FGrid;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxFilter }

  TsmxFilter = class(TsmxCustomFilter)
  private
    function GetCfg: TsmxFilterCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxFilterCfg read GetCfg;
  end;

  { TsmxPanelFilter }

  TsmxPanelFilter = class(TsmxFilter)
  private
    FHeader: TLabel;
    FPanel: TPanel;
  protected
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property Header: TLabel read FHeader;
    property Panel: TPanel read FPanel;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxFilterDesk }

  TsmxFilterDesk = class(TsmxCustomFilterDesk)
  private
    function GetCfg: TsmxFilterDeskCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxFilterDeskCfg read GetCfg;
  end;

  { TsmxPanelFilterDesk }

  TsmxPanelFilterDesk = class(TsmxFilterDesk)
  private
    FPanel: TPanel;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property Panel: TPanel read FPanel;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxSection }

  TsmxSection = class(TsmxCustomSection)
  private
    function GetCfg: TsmxSectionCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxSectionCfg read GetCfg;
  end;

  { TsmxPanelSection }

  TsmxPanelSection = class(TsmxSection)
  private
    FPanel: TPanel;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property Panel: TPanel read FPanel;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxPage }

  TsmxPage = class(TsmxCustomPage)
  private
    function GetCfg: TsmxPageCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxPageCfg read GetCfg;
  end;

  { TsmxTabSheet }

  TsmxTabSheet = class(TsmxPage)
  private
    FPage: TTabSheet;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property Page: TTabSheet read FPage;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxPageManager }

  TsmxPageManager = class(TsmxCustomPageManager)
  private
    function GetCfg: TsmxPageManagerCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxPageManagerCfg read GetCfg;
  end;

  { TsmxPageControl }

  TsmxPageControl = class(TsmxPageManager)
  private
    FPageControl: TPageControl;
  protected
    function GetActivePage: TsmxCustomPage; override;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure InstallParent; override;
    procedure SetActivePage(Value: TsmxCustomPage); override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;
    procedure UnInstallParent; override;

    property PageControl: TPageControl read FPageControl;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxLibAlgorithm }

  TsmxLibAlgorithm = class(TsmxCustomAlgorithm)
  private
    FLibProc: TsmxProcAlgExecute;
    function GetCfg: TsmxLibAlgorithmCfg;
  protected
    procedure AddParams; override;
    procedure ProcExec(Sender: TObject); virtual;
    procedure SetLibraryManager(Value: TsmxCustomLibraryManager); override;

    property Cfg: TsmxLibAlgorithmCfg read GetCfg;
    property LibProc: TsmxProcAlgExecute read FLibProc;
  public
    //constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure Execute(Same: Boolean = False); override;
    procedure RefreshParams; override;
    function FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): TsmxParam; override;
  end;

  { TsmxActionLibAlgorithm }

  TsmxActionLibAlgorithm = class(TsmxLibAlgorithm)
  private
    FAction: TAction;
  protected
    function GetInternalObject: TObject; override;
    function GetCellCaption: String; override;
    function GetCellEnable: Boolean; override;
    function GetCellHotKey: Integer; override;
    function GetCellImageIndex: Integer; override;
    function GetCellVisible: Boolean; override;
    procedure SetCellCaption(Value: String); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHotKey(Value: Integer); override;
    procedure SetCellImageIndex(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property Action: TAction read FAction;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxAlgorithmList }

  TsmxAlgorithmList = class(TsmxCustomAlgorithmList)
  private
    function GetCfg: TsmxAlgorithmListCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxAlgorithmListCfg read GetCfg;
  end;

  { TsmxActionList }

  TsmxActionList = class(TsmxAlgorithmList)
  private
    FActionList: TActionList;
  protected
    function GetInternalObject: TObject; override;
    procedure SetImageList(Value: TCustomImageList); override;

    property ActionList: TActionList read FActionList;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxMenuPoint }

  TsmxMenuPoint = class(TsmxCustomMenuPoint)
  private
    function GetCfg: TsmxMenuItemCfg;
  protected
    property Cfg: TsmxMenuItemCfg read GetCfg;
  end;

  { TsmxMenuItem }

  TsmxMenuItem = class(TsmxMenuPoint)
  private
    FMenuItem: TMenuItem;
  protected
    function GetInternalObject: TObject; override;
    function GetCellEnable: Boolean; override;
    function GetCellVisible: Boolean; override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property MenuItem: TMenuItem read FMenuItem;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
    procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
  end;

  { TsmxMasterMenu }

  TsmxMasterMenu = class(TsmxCustomMasterMenu)
  private
    function GetCfg: TsmxMainMenuCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxMainMenuCfg read GetCfg;
  public
    function MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint; override;
  end;

  { TsmxMainMenu }

  TsmxMainMenu = class(TsmxMasterMenu)
  private
    FMainMenu: TMainMenu;
  protected
    function GetInternalObject: TObject; override;
    function GetCellEnable: Boolean; override;
    function GetCellVisible: Boolean; override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetImageList(Value: TCustomImageList); override;

    property MainMenu: TMainMenu read FMainMenu;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxToolBoard }

  TsmxToolBoard = class(TsmxCustomToolBoard)
  private
    function GetCfg: TsmxToolBoardCfg;
  protected
    property Cfg: TsmxToolBoardCfg read GetCfg;
  end;

  { TsmxToolBar }

  TsmxToolBar = class(TsmxToolBoard)
  private
    FToolBar: TToolBar;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;
    //procedure Initialize; override;
    //procedure UnInitialize; override;

    property ToolBar: TToolBar read FToolBar;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
    procedure AddAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
    procedure DelAlgorithm(Algorithm: TsmxCustomAlgorithm); override;
    procedure Prepare(Forcibly: Boolean = False); override;
  end;

  { TsmxControlBoard }

  TsmxControlBoard = class(TsmxCustomControlBoard)
  private
    function GetCfg: TsmxControlBoardCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxControlBoardCfg read GetCfg;
  end;

  { TsmxControlBar }

  TsmxControlBar = class(TsmxControlBoard)
  private
    FControlBar: TControlBar;
  protected
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetParentCell(Value: TsmxBaseCell); override;

    property ControlBar: TControlBar read FControlBar;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

  { TsmxForm }

  TsmxForm = class(TsmxCustomForm)
  private
    function GetCfg: TsmxFormCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxFormCfg read GetCfg;
  end;

  { TsmxStandardForm }

  TsmxStandardForm = class(TsmxForm)
  private
    FForm: TForm;
  protected
    procedure ProcClose(Sender: TObject; var Action: TCloseAction); virtual;
    function GetFormModalResult:  TModalResult; override;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure SetFormManager(Value: TsmxCustomFormManager); override;
    procedure SetFormModalResult(Value: TModalResult); override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure Initialize; override;
    procedure UnInitialize; override;

    property Form: TForm read FForm;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID: Integer; AID: Integer = 0); override;
    constructor CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
      ACfgID, AIntfID: Integer; AID: Integer = 0); override;
    destructor Destroy; override;
    procedure CloseForm; override;
    procedure ShowForm; override;
    function ShowModalForm: TModalResult; override;
  end;

  { TsmxMasterForm }

  TsmxMasterForm = class(TsmxCustomMasterForm)
  private
    function GetCfg: TsmxFormCfg;
  protected
    procedure CreateChilds; override;
    procedure InitChilds; override;

    property Cfg: TsmxFormCfg read GetCfg;
  end;

  { TsmxMainForm }

  TsmxMainForm = class(TsmxMasterForm)
  private
    //FForm: TForm;
    //procedure SetForm(AForm: TForm);
  protected
    procedure ProcClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure ProcCloseQuery(Sender: TObject; var CanClose: Boolean); virtual;
    function GetInternalObject: TObject; override;
    function GetCellAlign: TAlign; override;
    function GetCellEnable: Boolean; override;
    function GetCellHeight: Integer; override;
    function GetCellLeft: Integer; override;
    function GetCellTop: Integer; override;
    function GetCellVisible: Boolean; override;
    function GetCellWidth: Integer; override;
    procedure Initialize; override;
    procedure SetCellAlign(Value: TAlign); override;
    procedure SetCellEnable(Value: Boolean); override;
    procedure SetCellHeight(Value: Integer); override;
    procedure SetCellLeft(Value: Integer); override;
    procedure SetCellTop(Value: Integer); override;
    procedure SetCellVisible(Value: Boolean); override;
    procedure SetCellWidth(Value: Integer); override;
    procedure SetImageList(Value: TCustomImageList); override;
    procedure UnInitialize; override;
    procedure SetForm(Value: TForm); override;
  public
    procedure CloseForm; override;
    procedure ShowForm; override;

    //property Form: TForm read FForm write SetForm;
  end;

implementation

uses
  SysUtils, Variants, Graphics, ToolWin, {smxCommonStorage, smxLibManager,
  smxDBManager, smxFormManager, smxGlobalVariables, smxDBConnection,} smxFuncs,
  smxClassFuncs, {smxLibFuncs,} smxConsts, smxClassProcs;

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

  { _TMenuItem }

  _TMenuItem = class(TMenuItem)
  end;

function TsmxRequest.GetCfg: TsmxRequestCfg;
begin
  Result := TsmxRequestCfg(inherited Cfg);
end;

function TsmxRequest.FindFieldSense(AFieldSense: TsmxFieldSense; StartPos: Integer = 0): IsmxField;
var i: Integer;
begin
  Result := nil;
  if not Assigned(CellDataSet) then
    Exit;
  for i := StartPos to Cfg.RequestFields.Count - 1 do
    with Cfg.RequestFields[i] do
      if FieldSense = AFieldSense then
      begin
        Result := CellDataSet.FindField(FieldName);
        Break;
      end;
end;

function TsmxRequest.FindParamLocation(AParamLocation: TsmxParamLocation; StartPos: Integer = 0): IsmxParam;
var i: Integer;
begin
  Result := nil;
  if not Assigned(CellDataSet) then
    Exit;
  for i := StartPos to Cfg.RequestParams.Count - 1 do
    with Cfg.RequestParams[i] do
      if ParamLocation = AParamLocation then
      begin
        Result := CellDataSet.FindParam(ParamName);
        Break;
      end;
end;

procedure TsmxRequest.Perform;
var
  i: Integer;
begin
  if not Assigned(CellDataSet) then
    Exit;
  with CellDataSet do
  begin
    Close;
    try
      case Cfg.PerformanceMode of
        pmOpen: Open;
        pmExecute: Execute;
      end;
    except
      raise EsmxCellError.CreateRes(@SCellRequestPerformError);
    end;
    for i := 0 to Cfg.RequestFields.Count - 1 do
      with Cfg.RequestFields[i] do
        FieldByName(FieldName).DisplayFormat := FieldFormat;
  end;
end;

procedure TsmxRequest.RefreshParams;

  function FindFilterOnForm(AForm: TsmxCustomForm; AName: String): TsmxCustomFilter;
  var i, j: Integer; p: TsmxCustomPage;
  begin
    Result := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
         p := AForm.PageManagers[i].ActivePage;
         if Assigned(p) then
           for j := 0 to p.SectionCount - 1 do
           begin
             if Assigned(p.Sections[j].FilterDesk) then
               Result := p.Sections[j].FilterDesk.FindFilterByName(AName);
             if Assigned(Result) then
               Exit;
           end;
      end;
  end;

  function FindFieldOnForm(AForm: TsmxCustomForm; AName: String): IsmxField;
  var i, j: Integer; p: TsmxCustomPage;
  begin
    Result := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
        p := AForm.PageManagers[i].ActivePage;
        if Assigned(p) then
          for j := 0 to p.SectionCount - 1 do
          begin
            if Assigned(p.Sections[j].Request) then
              if Assigned(p.Sections[j].Request.CellDataSet) then
                Result := p.Sections[j].Request.CellDataSet.FindField(AName);
            if Assigned(Result) then
              Exit;
          end;
      end;
  end;

var i, j: integer; v: Variant; c: TsmxBaseCell; f, fp: TsmxCustomForm;
  p: TsmxCustomPage; s: TsmxCustomSection; flt: TsmxCustomFilter;
  fld: IsmxField; prm: TsmxParam;
begin
  if not Assigned(CellDataSet) then
    Exit;
  c := RootCell;
  if c is TsmxCustomForm then
    f := TsmxCustomForm(c) else
    f := nil;
  for i := 0 to Cfg.RequestParams.Count - 1 do
    with Cfg.RequestParams[i] do
    begin
      v := Null;
      case ParamLocation of
        plInput: v := Null;
        plConst: v := ParamDefValue;
        plOutput,
        plKey,
        plValue,
        plResult,
        plMessage: v := Null;
        plFilterDesk:
        begin
          flt := nil;
          c := ParentCell;
          if c is TsmxCustomSection then
            s := TsmxCustomSection(c) else
            s := nil;
          if Assigned(s) then
            if Assigned(s.FilterDesk) then
              flt := s.FilterDesk.FindFilterByName(ParamName);
          if Assigned(flt) then
            v := flt.FilterValue;
        end;
        plGrid:
        begin
          fld := nil;
          c := ParentCell;
          if c is TsmxCustomSection then
            s := TsmxCustomSection(c) else
            s := nil;
          if Assigned(s) then
          begin
            c := s.ParentCell;
            if c is TsmxCustomPage then
            begin
              p := TsmxCustomPage(c);
              for j := 0 to p.SectionCount - 1 do
              if p.Sections[j] <> s then
              begin
                if Assigned(p.Sections[j].Request) then
                  if Assigned(p.Sections[j].Request.CellDataSet) then
                    fld := p.Sections[j].Request.CellDataSet.FindField(ParamName);
                if Assigned(fld) then
                  Break;
              end;
            end;
          end;
          if Assigned(fld) then
            v := fld.Value;
        end;
        plParentFormFilterDesk:
        begin
          flt := nil;
          if Assigned(f) then
            fp := f.ParentForm else
            fp := nil;
          while Assigned(fp) and not Assigned(flt) do
          begin
            flt := FindFilterOnForm(fp, ParamName);
            fp := fp.ParentForm;
          end;
          if Assigned(flt) then
            v := flt.FilterValue;
        end;
        plParentFormGrid:
        begin
          fld := nil;
          if Assigned(f) then
            fp := f.ParentForm else
            fp := nil;
          while Assigned(fp) and not Assigned(fld) do
          begin
            fld := FindFieldOnForm(fp, ParamName);
            fp := fp.ParentForm;
          end;
          if Assigned(fld) then
            v := fld.Value;
        end;
        {plParentParams:
        begin
          prm := nil;
          c := ParentCell;
          if c is TsmxCustomAlgorithm then
            prm := TsmxCustomAlgorithm(c).Params.FindByName(ParamName);
          if Assigned(prm) then
            v := prm.ParamValue;
        end;}
        {plCommonParams:
        begin
          //v := CommonStorage.ParamValues[ParamName];
          //v := FindCommonParamByNameLib(ParamName);
          if Assigned(CommonStorage) then
            v := CommonStorage.FindByName(ParamName);
        end;}
        {plParentCfgID:
        begin
          c := ParentCell;
          if Assigned(c) then
            v := c.CfgID;
        end;}
        plFormIntfID:
        begin
          if Assigned(f) then
            v := f.IntfID;
        end;
        plFormID:
        begin
          if Assigned(f) then
            v := f.ID;
        end;
      end;

      if not VarIsNull(v) then
        CellDataSet.ParamByName(ParamName).Value := v else
        CellDataSet.ParamByName(ParamName).Value := ParamDefValue;
    end;
end;

procedure TsmxRequest.Initialize;
begin
  if Assigned(Database) then
  begin
    FDataSetIntf := Database.NewDataSet(Cfg.DataSetType);
    FDataSetIntf.SQL.Text := Cfg.SQLText;
    FDataSetIntf.Prepare;
  end;
end;

procedure TsmxRequest.UnInitialize;
begin
  if Assigned(FDataSetIntf) then
  begin
    FDataSetIntf.Close;
    FDataSetIntf := nil;
  end;
end;

procedure TsmxRequest.SetDatabase(const Value: IsmxDatabase);
begin
  if Assigned(Database) then
    UnInitialize;
  inherited SetDatabase(Value);
  if Assigned(Database) then
    Initialize;
end;

{procedure TsmxRequest.SetDatabaseName(Value: String);
//var db: IsmxDatabase; //dbc: TsmxDBConnection;
begin
  if Assigned(Database) then
    //SetDatabase(nil);
    UnInitialize;
  inherited SetDatabaseName(Value);
  //dbc := DBManager.FindByName(Value);
  //if Assigned(dbc) then
    //if Assigned(dbc.Database) then
      //SetDatabase(dbc.Database);
  //db := FindDatabaseByNameLib(Value);
  //if Assigned(db) then
    //SetDatabase(db);
  if Assigned(Database) then
    Initialize;
end;}

{procedure TsmxRequest.SetDBManager(Value: TsmxCustomDBManager);
begin
  if Assigned(Database) then
    UnInitialize;
  inherited SetDBManager(Value);
  if Assigned(Database) then
    Initialize;
end;}

{ TsmxColumn }

function TsmxColumn.GetCfg: TsmxColumnCfg;
begin
  Result := TsmxColumnCfg(inherited Cfg);
end;

{ TsmxDBColumn }

constructor TsmxDBColumn.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FColumn := TColumn.Create(nil);
  FColumn.FieldName := Cfg.ColumnFieldName;
  FColumn.Alignment := Cfg.ColumnText.Align;
  FColumn.Color := Cfg.ColumnText.Color;
  FColumn.Font.Color := Cfg.ColumnText.Font.Color;
  FColumn.Font.Name := Cfg.ColumnText.Font.Name;
  FColumn.Font.Size := Cfg.ColumnText.Font.Size;
  FColumn.Font.Style := Cfg.ColumnText.Font.Style;
  FColumn.Title.Caption := Cfg.ColumnTitle.Text;
  FColumn.Title.Alignment := Cfg.ColumnTitle.Align;
  FColumn.Title.Color := Cfg.ColumnTitle.Color;
  FColumn.Title.Font.Color := Cfg.ColumnTitle.Font.Color;
  FColumn.Title.Font.Name := Cfg.ColumnTitle.Font.Name;
  FColumn.Title.Font.Size := Cfg.ColumnTitle.Font.Size;
  FColumn.Title.Font.Style := Cfg.ColumnTitle.Font.Style;
end;

destructor TsmxDBColumn.Destroy;
begin
  FColumn.Free;
  inherited Destroy;
end;

function TsmxDBColumn.GetInternalObject: TObject;
begin
  Result := FColumn;
end;

function TsmxDBColumn.GetCellVisible: Boolean;
begin
  Result := FColumn.Visible;
end;

function TsmxDBColumn.GetCellWidth: Integer;
begin
  Result := FColumn.Width;
end;

procedure TsmxDBColumn.SetCellVisible(Value: Boolean);
begin
  FColumn.Visible := Value;
end;

procedure TsmxDBColumn.SetCellWidth(Value: Integer);
begin
  FColumn.Width := Value;
end;

procedure TsmxDBColumn.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FColumn.Collection := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TDBGrid then
      FColumn.Collection := TDBGrid(c).Columns;
  end;
end;

{ TsmxGrid }

procedure TsmxGrid.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  ColumnList.Count := Cfg.GridColumns.Count;
  for i := 0 to Cfg.GridColumns.Count - 1 do
    with Cfg.GridColumns[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomColumn then
          ColumnList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxGrid.GetCfg: TsmxGridCfg;
begin
  Result := TsmxGridCfg(inherited Cfg);
end;

procedure TsmxGrid.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.GridColumns.Count - 1 do
    with Cfg.GridColumns[i] do
    begin
      Columns[i].CellAlign := UnitAlign;
      Columns[i].CellEnable := UnitEnable;
      Columns[i].CellHeight := UnitHeight;
      Columns[i].CellLeft := UnitLeft;
      Columns[i].CellTop := UnitTop;
      Columns[i].CellVisible := UnitVisible;
      Columns[i].CellWidth := UnitWidth;
    end;
end;

{ TsmxDBGrid }

constructor TsmxDBGrid.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FDataSource := TDataSource.Create(Self);
  FGrid := TsmxWheelDBGrid.Create(Self);
  FGrid.DataSource := FDataSource;
  FGrid.Options := FGrid.Options - [dgEditing];
  //if Cfg.GridColLines then
  if goColLines in Cfg.GridOptions then
    FGrid.Options := FGrid.Options + [dgColLines] else
    FGrid.Options := FGrid.Options - [dgColLines];
  //if Cfg.GridRowLines then
  if goRowLines in Cfg.GridOptions then
    FGrid.Options := FGrid.Options + [dgRowLines] else
    FGrid.Options := FGrid.Options - [dgRowLines];
  //if Cfg.GridRowSelect then
  if goRowSelect in Cfg.GridOptions then
    FGrid.Options := FGrid.Options + [dgRowSelect] else
    FGrid.Options := FGrid.Options - [dgRowSelect];
  InstallParent;
end;

destructor TsmxDBGrid.Destroy;
begin
  UnInstallParent;
  FGrid.Free;
  FDataSource.Free;
  inherited Destroy;
end;

function TsmxDBGrid.GetInternalObject: TObject;
begin
  Result := FGrid;
end;

function TsmxDBGrid.GetCellAlign: TAlign;
begin
  Result := FGrid.Align;
end;

function TsmxDBGrid.GetCellCursor: TCursor;
begin
  Result := FGrid.Cursor;
end;

function TsmxDBGrid.GetCellEnable: Boolean;
begin
  Result := FGrid.Enabled;
end;

function TsmxDBGrid.GetCellHeight: Integer;
begin
  Result := FGrid.Height;
end;

function TsmxDBGrid.GetCellLeft: Integer;
begin
  Result := FGrid.Left;
end;

function TsmxDBGrid.GetCellTop: Integer;
begin
  Result := FGrid.Top;
end;

function TsmxDBGrid.GetCellVisible: Boolean;
begin
  Result := FGrid.Visible;
end;

function TsmxDBGrid.GetCellWidth: Integer;
begin
  Result := FGrid.Width;
end;

procedure TsmxDBGrid.SetCellAlign(Value: TAlign);
begin
  FGrid.Align := Value;
end;

procedure TsmxDBGrid.SetCellCursor(Value: TCursor);
begin
  FGrid.Cursor := Value;
end;

procedure TsmxDBGrid.SetCellEnable(Value: Boolean);
begin
  FGrid.Enabled := Value;
end;

procedure TsmxDBGrid.SetCellHeight(Value: Integer);
begin
  FGrid.Height := Value;
end;

procedure TsmxDBGrid.SetCellLeft(Value: Integer);
begin
  FGrid.Left := Value;
end;

procedure TsmxDBGrid.SetCellTop(Value: Integer);
begin
  FGrid.Top := Value;
end;

procedure TsmxDBGrid.SetCellVisible(Value: Boolean);
begin
  FGrid.Visible := Value;
end;

procedure TsmxDBGrid.SetCellWidth(Value: Integer);
begin
  FGrid.Width := Value;
end;

procedure TsmxDBGrid.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FGrid.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FGrid.Parent := TWinControl(c);
  end;
end;

procedure TsmxDBGrid.SetRequest(Value: TsmxCustomRequest);
var c: TObject;
begin
  if Assigned(Request) then
    FDataSource.DataSet := nil;
  inherited SetRequest(Value);
  if Assigned(Request) then
    if Assigned(Request.CellDataSet) then
    begin
      c := Request.CellDataSet.GetDataSet;
      if c is TDataSet then
        FDataSource.DataSet := TDataSet(c);
    end;
end;

{ TsmxFilter }

procedure TsmxFilter.CreateChilds;
var c: TsmxBaseCell;
begin
  with Cfg.Algorithm do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomAlgorithm then
        Algorithm := TsmxCustomAlgorithm(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
end;

function TsmxFilter.GetCfg: TsmxFilterCfg;
begin
  Result := TsmxFilterCfg(inherited Cfg);
end;

procedure TsmxFilter.InitChilds;
begin
  if Assigned(Algorithm) then
    with Cfg.Algorithm do
    begin
      Algorithm.CellCaption := Caption;
      Algorithm.CellEnable := Enable;
      Algorithm.CellHotKey := HotKey;
      Algorithm.CellVisible := Visible;
    end;
end;

{ TsmxPanelFilter }

constructor TsmxPanelFilter.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FilterName := Cfg.FilterName;
  FPanel := TPanel.Create(Self);
  FPanel.Height := 49;
  FPanel.BevelOuter := bvNone;
  FHeader := TLabel.Create(Self);
  FHeader.Parent := FPanel;
  FHeader.Font.Color := TColor(Cfg.FilterHeader.Font.Color);
  FHeader.Font.Name := Cfg.FilterHeader.Font.Name;
  FHeader.Font.Size := Cfg.FilterHeader.Font.Size;
  FHeader.Font.Style := Cfg.FilterHeader.Font.Style;
  FHeader.Caption := Cfg.FilterHeader.Text;
  FHeader.Color := TColor(Cfg.FilterHeader.Color);
  case Cfg.FilterHeader.Align of
    taLeftJustify:
    begin
      FHeader.Anchors := [akLeft];
      FHeader.Left := 4;
    end;
    taRightJustify:
    begin
      FHeader.Anchors := [akRight];
      FHeader.Left := FPanel.Width - FHeader.Width - 4;
    end;
    taCenter:
    begin
      FHeader.Anchors := [akLeft, akRight];
      FHeader.Left := (FPanel.Width - FHeader.Width) div 2;
    end;
  end;
  FHeader.Top := 4;
end;

destructor TsmxPanelFilter.Destroy;
begin
  FHeader.Parent := nil;
  FHeader.Free;
  FPanel.Free;
  inherited Destroy;
end;

function TsmxPanelFilter.GetCellAlign: TAlign;
begin
  Result := FPanel.Align;
end;

function TsmxPanelFilter.GetCellEnable: Boolean;
begin
  Result := FPanel.Enabled;
end;

function TsmxPanelFilter.GetCellHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TsmxPanelFilter.GetCellLeft: Integer;
begin
  Result := FPanel.Left;
end;

function TsmxPanelFilter.GetCellTop: Integer;
begin
  Result := FPanel.Top;
end;

function TsmxPanelFilter.GetCellVisible: Boolean;
begin
  Result := FPanel.Visible;
end;

function TsmxPanelFilter.GetCellWidth: Integer;
begin
  Result := FPanel.Width;
end;

procedure TsmxPanelFilter.SetCellAlign(Value: TAlign);
begin
  FPanel.Align := Value;
end;

procedure TsmxPanelFilter.SetCellEnable(Value: Boolean);
begin
  FPanel.Enabled := Value;
end;

procedure TsmxPanelFilter.SetCellHeight(Value: Integer);
begin
  FPanel.Height := Value;
end;

procedure TsmxPanelFilter.SetCellLeft(Value: Integer);
begin
  FPanel.Left := Value;
end;

procedure TsmxPanelFilter.SetCellTop(Value: Integer);
begin
  FPanel.Top := Value;
end;

procedure TsmxPanelFilter.SetCellVisible(Value: Boolean);
begin
  FPanel.Visible := Value;
end;

procedure TsmxPanelFilter.SetCellWidth(Value: Integer);
begin
  FPanel.Width := Value;
end;

procedure TsmxPanelFilter.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPanel.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FPanel.Parent := TWinControl(c);
  end;
end;

{ TsmxFilterDesk }

procedure TsmxFilterDesk.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  with Cfg.ApplyRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        ApplyRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.PrepareRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        PrepareRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  FilterList.Count := Cfg.Filters.Count;
  for i := 0 to Cfg.Filters.Count - 1 do
    with Cfg.Filters[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomFilter then
          FilterList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxFilterDesk.GetCfg: TsmxFilterDeskCfg;
begin
  Result := TsmxFilterDeskCfg(inherited Cfg);
end;

procedure TsmxFilterDesk.InitChilds;
var i: Integer;
begin
  if Assigned(ApplyRequest) then
    with Cfg.ApplyRequest do
    begin
      ApplyRequest.DatabaseName := DatabaseName;
      ApplyRequest.OperationMode := Operation;
    end;
  if Assigned(PrepareRequest) then
    with Cfg.PrepareRequest do
    begin
      PrepareRequest.DatabaseName := DatabaseName;
      PrepareRequest.OperationMode := Operation;
    end;
  for i := 0 to Cfg.Filters.Count - 1 do
    with Cfg.Filters[i] do
    begin
      Filters[i].CellAlign := UnitAlign;
      Filters[i].CellEnable := UnitEnable;
      Filters[i].CellHeight := UnitHeight;
      Filters[i].CellLeft := UnitLeft;
      Filters[i].CellTop := UnitTop;
      Filters[i].CellVisible := UnitVisible;
      Filters[i].CellWidth := UnitWidth;
    end;
end;

{ TsmxPanelFilterDesk }

constructor TsmxPanelFilterDesk.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPanel := TPanel.Create(Self);
  FPanel.BevelOuter := bvNone;
  InstallParent;
end;

destructor TsmxPanelFilterDesk.Destroy;
begin
  UnInstallParent;
  FPanel.Free;
  inherited Destroy;
end;

function TsmxPanelFilterDesk.GetInternalObject: TObject;
begin
  Result := FPanel;
end;

function TsmxPanelFilterDesk.GetCellAlign: TAlign;
begin
  Result := FPanel.Align;
end;

function TsmxPanelFilterDesk.GetCellEnable: Boolean;
begin
  Result := FPanel.Enabled;
end;

function TsmxPanelFilterDesk.GetCellHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TsmxPanelFilterDesk.GetCellLeft: Integer;
begin
  Result := FPanel.Left;
end;

function TsmxPanelFilterDesk.GetCellTop: Integer;
begin
  Result := FPanel.Top;
end;

function TsmxPanelFilterDesk.GetCellVisible: Boolean;
begin
  Result := FPanel.Visible;
end;

function TsmxPanelFilterDesk.GetCellWidth: Integer;
begin
  Result := FPanel.Width;
end;

procedure TsmxPanelFilterDesk.SetCellAlign(Value: TAlign);
begin
  FPanel.Align := Value;
end;

procedure TsmxPanelFilterDesk.SetCellEnable(Value: Boolean);
begin
  FPanel.Enabled := Value;
end;

procedure TsmxPanelFilterDesk.SetCellHeight(Value: Integer);
begin
  FPanel.Height := Value;
end;

procedure TsmxPanelFilterDesk.SetCellLeft(Value: Integer);
begin
  FPanel.Left := Value;
end;

procedure TsmxPanelFilterDesk.SetCellTop(Value: Integer);
begin
  FPanel.Top := Value;
end;

procedure TsmxPanelFilterDesk.SetCellVisible(Value: Boolean);
begin
  FPanel.Visible := Value;
end;

procedure TsmxPanelFilterDesk.SetCellWidth(Value: Integer);
begin
  FPanel.Width := Value;
end;

procedure TsmxPanelFilterDesk.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPanel.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FPanel.Parent := TWinControl(c);
  end;
end;

{ TsmxSection }

procedure TsmxSection.CreateChilds;
var c: TsmxBaseCell;
begin
  with Cfg.Request do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        Request := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.Grid do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomGrid then
        Grid := TsmxCustomGrid(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.FilterPanel do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomFilterDesk then
        FilterDesk := TsmxCustomFilterDesk(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
end;

function TsmxSection.GetCfg: TsmxSectionCfg;
begin
  Result := TsmxSectionCfg(inherited Cfg);
end;

procedure TsmxSection.InitChilds;
begin
  if Assigned(Request) then
    with Cfg.Request do
    begin
      Request.DatabaseName := DatabaseName;
      Request.OperationMode := Operation;
    end;
  if Assigned(Grid) then
    with Cfg.Grid do
    begin
      Grid.CellAlign := Align;
      Grid.CellEnable := Enable;
      Grid.CellVisible := Visible;
      with PositionSize do
      begin
        Grid.CellHeight := Height;
        Grid.CellLeft := Left;
        Grid.CellTop := Top;
        Grid.CellWidth := Width;
      end;
    end;
  if Assigned(FilterDesk) then
    with Cfg.FilterPanel do
    begin
      FilterDesk.CellAlign := Align;
      FilterDesk.CellEnable := Enable;
      FilterDesk.CellVisible := Visible;
      with PositionSize do
      begin
        FilterDesk.CellHeight := Height;
        FilterDesk.CellLeft := Left;
        FilterDesk.CellTop := Top;
        FilterDesk.CellWidth := Width;
      end;
    end;
end;

{ TsmxPanelSection }

constructor TsmxPanelSection.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPanel := TPanel.Create(Self);
  FPanel.BevelOuter := bvNone;
  //Initialize;
  InstallParent;
end;

destructor TsmxPanelSection.Destroy;
begin
  UnInstallParent;
  //UnInitialize;
  FPanel.Free;
  inherited Destroy;
end;

function TsmxPanelSection.GetInternalObject: TObject;
begin
  Result := FPanel;
end;

function TsmxPanelSection.GetCellAlign: TAlign;
begin
  Result := FPanel.Align;
end;

function TsmxPanelSection.GetCellEnable: Boolean;
begin
  Result := FPanel.Enabled;
end;

function TsmxPanelSection.GetCellHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TsmxPanelSection.GetCellLeft: Integer;
begin
  Result := FPanel.Left;
end;

function TsmxPanelSection.GetCellTop: Integer;
begin
  Result := FPanel.Top;
end;

function TsmxPanelSection.GetCellVisible: Boolean;
begin
  Result := FPanel.Visible;
end;

function TsmxPanelSection.GetCellWidth: Integer;
begin
  Result := FPanel.Width;
end;

procedure TsmxPanelSection.SetCellAlign(Value: TAlign);
begin
  FPanel.Align := Value;
end;

procedure TsmxPanelSection.SetCellEnable(Value: Boolean);
begin
  FPanel.Enabled := Value;
end;

procedure TsmxPanelSection.SetCellHeight(Value: Integer);
begin
  FPanel.Height := Value;
end;

procedure TsmxPanelSection.SetCellLeft(Value: Integer);
begin
  FPanel.Left := Value;
end;

procedure TsmxPanelSection.SetCellTop(Value: Integer);
begin
  FPanel.Top := Value;
end;

procedure TsmxPanelSection.SetCellVisible(Value: Boolean);
begin
  FPanel.Visible := Value;
end;

procedure TsmxPanelSection.SetCellWidth(Value: Integer);
begin
  FPanel.Width := Value;
end;

procedure TsmxPanelSection.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPanel.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FPanel.Parent := TWinControl(c);
  end;
end;

{ TsmxPage }

procedure TsmxPage.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  SectionList.Count := Cfg.Sections.Count;
  for i := 0 to Cfg.Sections.Count - 1 do
    with Cfg.Sections[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomSection then
          SectionList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxPage.GetCfg: TsmxPageCfg;
begin
  Result := TsmxPageCfg(inherited Cfg);
end;

procedure TsmxPage.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.Sections.Count - 1 do
    with Cfg.Sections[i] do
    begin
      Sections[i].CellAlign := UnitAlign;
      Sections[i].CellEnable := UnitEnable;
      Sections[i].CellHeight := UnitHeight;
      Sections[i].CellLeft := UnitLeft;
      Sections[i].CellTop := UnitTop;
      Sections[i].CellVisible := UnitVisible;
      Sections[i].CellWidth := UnitWidth;
    end;
end;

{ TsmxTabSheet }

constructor TsmxTabSheet.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPage := TTabSheet.Create(Self);
  FPage.Caption := Cfg.PageCaption;
  FPage.ImageIndex := TImageIndex(Cfg.PageImageIndex);
  InstallParent;
end;

destructor TsmxTabSheet.Destroy;
begin
  UnInstallParent;
  FPage.Free;
  inherited Destroy;
end;

function TsmxTabSheet.GetInternalObject: TObject;
begin
  Result := FPage;
end;

function TsmxTabSheet.GetCellAlign: TAlign;
begin
  Result := FPage.Align;
end;

function TsmxTabSheet.GetCellEnable: Boolean;
begin
  Result := FPage.Enabled;
end;

function TsmxTabSheet.GetCellHeight: Integer;
begin
  Result := FPage.Height;
end;

function TsmxTabSheet.GetCellLeft: Integer;
begin
  Result := FPage.Left;
end;

function TsmxTabSheet.GetCellTop: Integer;
begin
  Result := FPage.Top;
end;

function TsmxTabSheet.GetCellVisible: Boolean;
begin
  Result := FPage.TabVisible;
end;

function TsmxTabSheet.GetCellWidth: Integer;
begin
  Result := FPage.Width;
end;

procedure TsmxTabSheet.SetCellAlign(Value: TAlign);
begin
  FPage.Align := Value;
end;

procedure TsmxTabSheet.SetCellEnable(Value: Boolean);
begin
  FPage.Enabled := Value;
end;

procedure TsmxTabSheet.SetCellHeight(Value: Integer);
begin
  FPage.Height := Value;
end;

procedure TsmxTabSheet.SetCellLeft(Value: Integer);
begin
  FPage.Left := Value;
end;

procedure TsmxTabSheet.SetCellTop(Value: Integer);
begin
  FPage.Top := Value;
end;

procedure TsmxTabSheet.SetCellVisible(Value: Boolean);
begin
  FPage.TabVisible := Value;
end;

procedure TsmxTabSheet.SetCellWidth(Value: Integer);
begin
  FPage.Width := Value;
end;

procedure TsmxTabSheet.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPage.PageControl := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TPageControl then
      FPage.PageControl := TPageControl(c);
  end;
end;

{ TsmxPageManager }

procedure TsmxPageManager.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  PageList.Count := Cfg.Sheets.Count;
  for i := 0 to Cfg.Sheets.Count - 1 do
    with Cfg.Sheets[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomPage then
          PageList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxPageManager.GetCfg: TsmxPageManagerCfg;
begin
  Result := TsmxPageManagerCfg(inherited Cfg);
end;

procedure TsmxPageManager.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.Sheets.Count - 1 do
    with Cfg.Sheets[i] do
    begin
      Pages[i].CellAlign := UnitAlign;
      Pages[i].CellEnable := UnitEnable;
      Pages[i].CellHeight := UnitHeight;
      Pages[i].CellLeft := UnitLeft;
      Pages[i].CellTop := UnitTop;
      Pages[i].CellVisible := UnitVisible;
      Pages[i].CellWidth := UnitWidth;
    end;
end;

{ TsmxPageControl }

constructor TsmxPageControl.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FPageControl := TPageControl.Create(Self);
  //FPageControl.Images := ImageList;
  InstallParent;
end;

destructor TsmxPageControl.Destroy;
begin
  UnInstallParent;
  FPageControl.Free;
  inherited Destroy;
end;

function TsmxPageControl.GetActivePage: TsmxCustomPage;
begin
  Result := Pages[FPageControl.ActivePageIndex];
end;

function TsmxPageControl.GetInternalObject: TObject;
begin
  Result := FPageControl;
end;

function TsmxPageControl.GetCellAlign: TAlign;
begin
  Result := FPageControl.Align;
end;

function TsmxPageControl.GetCellEnable: Boolean;
begin
  Result := FPageControl.Enabled;
end;

function TsmxPageControl.GetCellHeight: Integer;
begin
  Result := FPageControl.Height;
end;

function TsmxPageControl.GetCellLeft: Integer;
begin
  Result := FPageControl.Left;
end;

function TsmxPageControl.GetCellTop: Integer;
begin
  Result := FPageControl.Top;
end;

function TsmxPageControl.GetCellVisible: Boolean;
begin
  Result := FPageControl.Visible;
end;

function TsmxPageControl.GetCellWidth: Integer;
begin
  Result := FPageControl.Width;
end;

procedure TsmxPageControl.InstallParent;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FPageControl.Parent := f;
    inherited InstallParent;
    FPageControl.Parent := nil;
  finally
    f.Free;
  end;
end;

procedure TsmxPageControl.SetActivePage(Value: TsmxCustomPage);
begin
  FPageControl.ActivePageIndex := PageList.IndexOf(Value);
end;

procedure TsmxPageControl.SetCellAlign(Value: TAlign);
begin
  FPageControl.Align := Value;
end;

procedure TsmxPageControl.SetCellEnable(Value: Boolean);
begin
  FPageControl.Enabled := Value;
end;

procedure TsmxPageControl.SetCellHeight(Value: Integer);
begin
  FPageControl.Height := Value;
end;

procedure TsmxPageControl.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    FPageControl.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    FPageControl.Images := ImageList;
end;

procedure TsmxPageControl.SetCellLeft(Value: Integer);
begin
  FPageControl.Left := Value;
end;

procedure TsmxPageControl.SetCellTop(Value: Integer);
begin
  FPageControl.Top := Value;
end;

procedure TsmxPageControl.SetCellVisible(Value: Boolean);
begin
  FPageControl.Visible := Value;
end;

procedure TsmxPageControl.SetCellWidth(Value: Integer);
begin
  FPageControl.Width := Value;
end;

procedure TsmxPageControl.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FPageControl.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FPageControl.Parent := TWinControl(c);
  end;
end;

procedure TsmxPageControl.UnInstallParent;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FPageControl.Parent := f;
    inherited UnInstallParent;
    FPageControl.Parent := nil;
  finally
    f.Free;
  end;
end;

{ TsmxLibAlgorithm }

{constructor TsmxLibAlgorithm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  //@FLibProc := LibManager.GetProcedure(Cfg.AlgLibrary, Cfg.AlgProcedure);
  //@FLibProc := FindProcedureByNameLib(Cfg.AlgLibrary, Cfg.AlgProcedure);
  //AddParams;
end;}

destructor TsmxLibAlgorithm.Destroy;
begin
  FLibProc := nil;
  inherited Destroy;
end;

procedure TsmxLibAlgorithm.AddParams;
var i: Integer;
begin
  for i := 0 to Cfg.AlgParams.Count - 1 do
    with Params.Add do
      ParamName := Cfg.AlgParams[i].ParamName;
end;

procedure TsmxLibAlgorithm.Execute(Same: Boolean = False);
var
  v: Variant;
begin
  if Assigned(FLibProc) then
  begin
    if not Same then
      RefreshParams;
    smxClassProcs.ParamsToVar(Params, v);
    FLibProc(Self, v);
  end;
end;

procedure TsmxLibAlgorithm.ProcExec(Sender: TObject);
begin
  Execute;
end;

function TsmxLibAlgorithm.FindParamLocation(AParamLocation: TsmxParamLocation;
  StartPos: Integer = 0): TsmxParam;
var i: Integer;
begin
  Result := nil;
  for i := StartPos to Cfg.AlgParams.Count - 1 do
    with Cfg.AlgParams[i] do
      if ParamLocation = AParamLocation then
      begin
        Result := Params.FindByName(ParamName);
        Break;
      end;
end;

function TsmxLibAlgorithm.GetCfg: TsmxLibAlgorithmCfg;
begin
  Result := TsmxLibAlgorithmCfg(inherited Cfg);
end;

procedure TsmxLibAlgorithm.RefreshParams;

  function FindFilterOnForm(AForm: TsmxCustomForm; AName: String): TsmxCustomFilter;
  var i, j: Integer; p: TsmxCustomPage;
  begin
    Result := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
         p := AForm.PageManagers[i].ActivePage;
         if Assigned(p) then
           for j := 0 to p.SectionCount - 1 do
           begin
             if Assigned(p.Sections[j].FilterDesk) then
               Result := p.Sections[j].FilterDesk.FindFilterByName(AName);
             if Assigned(Result) then
               Exit;
           end;
      end;
  end;

  function FindFieldOnForm(AForm: TsmxCustomForm; AName: String): IsmxField;
  var i, j: Integer; p: TsmxCustomPage;
  begin
    Result := nil;
    if Assigned(AForm) then
      for i := 0 to AForm.PageManagerCount - 1 do
      begin
        p := AForm.PageManagers[i].ActivePage;
        if Assigned(p) then
          for j := 0 to p.SectionCount - 1 do
          begin
            if Assigned(p.Sections[j].Request) then
              if Assigned(p.Sections[j].Request.CellDataSet) then
                Result := p.Sections[j].Request.CellDataSet.FindField(AName);
            if Assigned(Result) then
              Exit;
          end;
      end;
  end;

var i: integer; v: Variant; c: TsmxBaseCell; f, fp: TsmxCustomForm;
  flt: TsmxCustomFilter; fld: IsmxField;
begin
  c := RootCell;
  if c is TsmxCustomForm then
    f := TsmxCustomForm(c) else
    f := nil;
  for i := 0 to Cfg.AlgParams.Count - 1 do
    with Cfg.AlgParams[i] do
    begin
      v := Null;
      case ParamLocation of
        plInput: v := Null;
        plConst: v := ParamDefValue;
        plOutput,
        plKey,
        plValue,
        plResult,
        plMessage: v := Null;
        plFilterDesk:
        begin
          flt := nil;
          if Assigned(f) then
            flt := FindFilterOnForm(f, ParamName);
          if Assigned(flt) then
            v := flt.FilterValue;
        end;
        plGrid:
        begin
          fld := nil;
          if Assigned(f) then
            fld := FindFieldOnForm(f, ParamName);
          if Assigned(fld) then
            v := fld.Value;
        end;
        plParentFormFilterDesk:
        begin
          flt := nil;
          if Assigned(f) then
            fp := f.ParentForm else
            fp := nil;
          while Assigned(fp) and not Assigned(flt) do
          begin
            flt := FindFilterOnForm(fp, ParamName);
            fp := fp.ParentForm;
          end;
          if Assigned(flt) then
            v := flt.FilterValue;
        end;
        plParentFormGrid:
        begin
          fld := nil;
          if Assigned(f) then
            fp := f.ParentForm else
            fp := nil;
          while Assigned(fp) and not Assigned(fld) do
          begin
            fld := FindFieldOnForm(fp, ParamName);
            fp := fp.ParentForm;
          end;
          if Assigned(fld) then
            v := fld.Value;
        end;
        //plParentParams: v := Null;
        plCommonParams:
        begin
          //v := CommonStorage.ParamValues[ParamName];
          //v := FindCommonParamByNameLib(ParamName);
          if Assigned(CommonStorage) then
            v := CommonStorage.FindByName(ParamName);
        end;
        //plParentCfgID: v := Null;
        plFormIntfID:
        begin
          if Assigned(f) then
            v := f.IntfID;
        end;
        plFormID:
        begin
          if Assigned(f) then
            v := f.ID;
        end;
      end;

      if not VarIsNull(v) then
        Params.Values[ParamName] := v else
        Params.Values[ParamName] := ParamDefValue;
    end;
end;

procedure TsmxLibAlgorithm.SetLibraryManager(Value: TsmxCustomLibraryManager);
begin
  if Assigned(LibraryManager) then
    FLibProc := nil;
  inherited SetLibraryManager(Value);
  if Assigned(LibraryManager) then
    @FLibProc := LibraryManager.GetProcedure(Cfg.AlgLibrary, Cfg.AlgProcedure);
end;

{ TsmxActionLibAlgorithm }

constructor TsmxActionLibAlgorithm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FAction := TAction.Create(Self);
  FAction.Caption := Cfg.AlgDefCaption;
  FAction.Hint := Cfg.AlgDefHint;
  FAction.ShortCut := TShortCut(Cfg.AlgDefHotKey);
  FAction.ImageIndex := TImageIndex(Cfg.AlgImageIndex);
  FAction.OnExecute := ProcExec;
end;

destructor TsmxActionLibAlgorithm.Destroy;
begin
  FAction.OnExecute := nil;
  FAction.Free;
  inherited Destroy;
end;

function TsmxActionLibAlgorithm.GetInternalObject: TObject;
begin
  Result := FAction;
end;

function TsmxActionLibAlgorithm.GetCellCaption: String;
begin
  Result := FAction.Caption;
end;

function TsmxActionLibAlgorithm.GetCellEnable: Boolean;
begin
  Result := FAction.Enabled;
end;

function TsmxActionLibAlgorithm.GetCellHotKey: Integer;
begin
  Result := Integer(FAction.ShortCut);
end;

function TsmxActionLibAlgorithm.GetCellImageIndex: Integer;
begin
  Result := Integer(FAction.ImageIndex);
end;

function TsmxActionLibAlgorithm.GetCellVisible: Boolean;
begin
  Result := FAction.Visible;
end;

procedure TsmxActionLibAlgorithm.SetCellCaption(Value: String);
begin
  FAction.Caption := Value;
end;

procedure TsmxActionLibAlgorithm.SetCellEnable(Value: Boolean);
begin
  FAction.Enabled := Value;
end;

procedure TsmxActionLibAlgorithm.SetCellHotKey(Value: Integer);
begin
  FAction.ShortCut := TShortCut(Value);
end;

procedure TsmxActionLibAlgorithm.SetCellImageIndex(Value: Integer);
begin
  FAction.ImageIndex := TImageIndex(Value);
end;

procedure TsmxActionLibAlgorithm.SetCellVisible(Value: Boolean);
begin
  FAction.Visible := Value;
end;

procedure TsmxActionLibAlgorithm.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FAction.ActionList := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TCustomActionList then
      FAction.ActionList := TCustomActionList(c);
  end;
end;

{ TsmxAlgorithmList }

procedure TsmxAlgorithmList.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  AlgorithmList.Count := Cfg.AlgorithmItems.Count;
  for i := 0 to Cfg.AlgorithmItems.Count - 1 do
    with Cfg.AlgorithmItems[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomAlgorithm then
          AlgorithmList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxAlgorithmList.GetCfg: TsmxAlgorithmListCfg;
begin
  Result := TsmxAlgorithmListCfg(inherited Cfg);
end;

procedure TsmxAlgorithmList.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.AlgorithmItems.Count - 1 do
    with Cfg.AlgorithmItems[i] do
    begin
      Algorithms[i].CellEnable := AlgorithmEnable;
      Algorithms[i].CellVisible := AlgorithmVisible;
      Algorithms[i].CellHotKey := AlgorithmHotKey;
      Algorithms[i].CellCaption := AlgorithmCaption;
      Algorithms[i].MenuPointID := AlgorithmMenuItemCfgID;
      Algorithms[i].ToolBoardID := AlgorithmToolBarCfgID;
    end;
end;

{ TsmxActionList }

constructor TsmxActionList.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FActionList := TActionList.Create(Self);
  //FActionList.Images := ImageList;
  InstallParent;
end;

destructor TsmxActionList.Destroy;
begin
  UnInstallParent;
  FActionList.Free;
  inherited Destroy;
end;

function TsmxActionList.GetInternalObject: TObject;
begin
  Result := FActionList;
end;

procedure TsmxActionList.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    FActionList.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    FActionList.Images := ImageList;
end;

{ TsmxMenuPoint }

function TsmxMenuPoint.GetCfg: TsmxMenuItemCfg;
begin
  Result := TsmxMenuItemCfg(inherited Cfg);
end;

{ TsmxMenuItem }

constructor TsmxMenuItem.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin 
  inherited Create(AOwner, ADatabase, ACfgID);
  FMenuItem := TMenuItem.Create(Self);
  FMenuItem.Caption := Cfg.ItemCaption;
  FMenuItem.ImageIndex := TImageIndex(Cfg.ItemImageIndex);
end;

destructor TsmxMenuItem.Destroy;
begin
  FMenuItem.Free;
  inherited Destroy;
end;

procedure TsmxMenuItem.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
var c: TObject; mi: TMenuItem;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
  begin
    mi := TMenuItem.Create(Self);
    mi.Action := TBasicAction(c);
    FMenuItem.Add(mi);
  end;
end;

procedure TsmxMenuItem.DelAlgorithm(Algorithm: TsmxCustomAlgorithm);
var c: TObject; mi: TMenuItem; i: Integer;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
    for i := FMenuItem.Count - 1 downto 0 do
    begin
      mi := FMenuItem[i];
      if mi.Action = c then
      begin
        FMenuItem.Remove(mi);
        mi.Action := nil;
        mi.Free;
      end;
    end;
end;

function TsmxMenuItem.GetInternalObject: TObject;
begin
  Result := FMenuItem;
end;

function TsmxMenuItem.GetCellEnable: Boolean;
begin
  Result := FMenuItem.Enabled;
end;

function TsmxMenuItem.GetCellVisible: Boolean;
begin
  Result := FMenuItem.Visible;
end;

procedure TsmxMenuItem.SetCellEnable(Value: Boolean);
begin
  FMenuItem.Enabled := Value;
end;

procedure TsmxMenuItem.SetCellVisible(Value: Boolean);
begin
  FMenuItem.Visible := Value;
end;

procedure TsmxMenuItem.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TMenuItem then
      TMenuItem(c).Remove(FMenuItem) else
    if c is TMainMenu then
      TMainMenu(c).Items.Remove(FMenuItem);
  end;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TMenuItem then
      TMenuItem(c).Add(FMenuItem) else
    if c is TMainMenu then
      TMainMenu(c).Items.Add(FMenuItem);
  end;
end;

{ TsmxMasterMenu }

procedure TsmxMasterMenu.CreateChilds;

  procedure AddItems(AUnit: TsmxHVisibleUnit);
  var i: Integer; c: TsmxBaseCell;
  begin
    with AUnit do
      if CfgID > 0 then
      begin 
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomMenuPoint then
          MenuPointList.Add(c) else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    for i := 0 to AUnit.Count - 1 do
      AddItems(AUnit[i]);
  end;

var i: Integer;
begin
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    AddItems(Cfg.MenuUnits.Root[i]);
end;

function TsmxMasterMenu.GetCfg: TsmxMainMenuCfg;
begin
  Result := TsmxMainMenuCfg(inherited Cfg);
end;

function TsmxMasterMenu.MenuPointParent(ACfgID: Integer): TsmxCustomMenuPoint;
var u: TsmxHVisibleUnit;
begin
  Result := nil;
  u := Cfg.MenuUnits.Root.FindByCfgID(ACfgID, True);
  if Assigned(u) then
    if u.HasParent then
      Result := FindMenuPointByCfgID(u.Parent.CfgID);
end;

procedure TsmxMasterMenu.InitChilds;

  procedure InitItems(AUnit: TsmxHVisibleUnit);
  var i: Integer; mi: TsmxCustomMenuPoint;
  begin
    mi := FindMenuPointByCfgID(AUnit.CfgID);
    if Assigned(mi) then
      with AUnit do
      begin
        mi.CellAlign := UnitAlign;
        mi.CellEnable := UnitEnable;
        mi.CellHeight := UnitHeight;
        mi.CellLeft := UnitLeft;
        mi.CellTop := UnitTop;
        mi.CellVisible := UnitVisible;
        mi.CellWidth := UnitWidth;
      end;
    for i := 0 to AUnit.Count - 1 do
      InitItems(AUnit[i]);
  end;

var i: Integer;
begin
  for i := 0 to Cfg.MenuUnits.Root.Count - 1 do
    InitItems(Cfg.MenuUnits.Root[i]);
end;

{ TsmxMainMenu }

constructor TsmxMainMenu.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID); 
  FMainMenu := TMainMenu.Create(Self);
  //FMainMenu.Images := ImageList;
  InstallParent;
end;

destructor TsmxMainMenu.Destroy;
begin
  UnInstallParent;
  FMainMenu.Free;
  inherited Destroy;
end;

function TsmxMainMenu.GetInternalObject: TObject;
begin
  Result := FMainMenu;
end;

function TsmxMainMenu.GetCellEnable: Boolean;
begin
  Result := FMainMenu.Items.Enabled;
end;

function TsmxMainMenu.GetCellVisible: Boolean;
begin
  Result := FMainMenu.Items.Visible;
end;

procedure TsmxMainMenu.SetCellEnable(Value: Boolean);
var i: Integer;
begin
  for i := 0 to FMainMenu.Items.Count - 1 do
    FMainMenu.Items[i].Enabled := Value;
end;

procedure TsmxMainMenu.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    FMainMenu.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
  begin
    FMainMenu.Images := ImageList;
    if FMainMenu.Items.Count > 0 then
      _TMenuItem(FMainMenu.Items[0]).MenuChanged(True);
  end;
end;

procedure TsmxMainMenu.SetCellVisible(Value: Boolean);
var i: Integer;
begin
  for i := 0 to FMainMenu.Items.Count - 1 do
    FMainMenu.Items[i].Visible := Value;
end;

{ TsmxToolBoard }

function TsmxToolBoard.GetCfg: TsmxToolBoardCfg;
begin
  Result := TsmxToolBoardCfg(inherited Cfg);
end;

{ TsmxToolBar }

constructor TsmxToolBar.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FToolBar := TToolBar.Create(Self);
  FToolBar.AutoSize := True;
  FToolBar.EdgeBorders := FToolBar.EdgeBorders - [ebTop];
  FToolBar.Width := 0;
  FToolBar.Flat := Cfg.BarFlat;
  FToolBar.ShowCaptions := Cfg.BarShowCaptions;
  FToolBar.ShowHint := Cfg.BarShowHint;
  //Initialize;
end;

destructor TsmxToolBar.Destroy;
begin
  //UnInitialize; 
  FToolBar.Free;
  inherited Destroy;
end;

procedure TsmxToolBar.AddAlgorithm(Algorithm: TsmxCustomAlgorithm);
var c: TObject; b: TToolButton;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
  begin
    b := TToolButton.Create(Self);
    b.Parent := FToolBar;
    b.ShowHint := Cfg.BarShowHint;
    b.Action := TBasicAction(c);
  end;
end;

procedure TsmxToolBar.DelAlgorithm(Algorithm: TsmxCustomAlgorithm);
var c: TObject; i: Integer; b: TToolButton;
begin
  c := _TsmxBaseCell(Algorithm).GetInternalObject;
  if c is TBasicAction then
    for i := FToolBar.ButtonCount - 1 downto 0 do
    begin
      b := FToolBar.Buttons[i];
      if b.Action = c then
      begin
        b.Action := nil;
        b.Parent := nil;
        b.Free;
      end;
    end;
end;

function TsmxToolBar.GetInternalObject: TObject;
begin
  Result := FToolBar;
end;

function TsmxToolBar.GetCellAlign: TAlign;
begin
  Result := FToolBar.Align;
end;

function TsmxToolBar.GetCellEnable: Boolean;
begin
  Result := FToolBar.Enabled;
end;

function TsmxToolBar.GetCellHeight: Integer;
begin
  Result := FToolBar.Height;
end;

function TsmxToolBar.GetCellLeft: Integer;
begin
  Result := FToolBar.Left;
end;

function TsmxToolBar.GetCellTop: Integer;
begin
  Result := FToolBar.Top;
end;

function TsmxToolBar.GetCellVisible: Boolean;
begin
  Result := FToolBar.Visible;
end;

function TsmxToolBar.GetCellWidth: Integer;
begin
  Result := FToolBar.Width;
end;

procedure TsmxToolBar.Prepare(Forcibly: Boolean = False);
var i, w: Integer;
begin
  inherited Prepare(Forcibly);
  with FToolBar do
  begin
    w := 0;
    for i := 0 to ButtonCount - 1 do
      if Buttons[i].Visible then
        w := w + Buttons[i].Width;
    Width := w;
  end;
end;

procedure TsmxToolBar.SetCellAlign(Value: TAlign);
begin
  FToolBar.Align := Value;
end;

procedure TsmxToolBar.SetCellEnable(Value: Boolean);
begin
  FToolBar.Enabled := Value;
end;

procedure TsmxToolBar.SetCellHeight(Value: Integer);
begin
  FToolBar.Height := Value;
end;

procedure TsmxToolBar.SetImageList(Value: TCustomImageList);
//var f: TCustomForm;
begin
  {f := GetParentForm(FToolBar);
  if Assigned(f) then
    FToolBar.Images := Value
  else
  begin
    f := TForm.Create(nil);
    try
      FToolBar.Parent := f;
      FToolBar.Images := Value;
      FToolBar.Parent := nil;
    finally
      f.Free;
    end;
  end;}
  if Assigned(ImageList) then
    FToolBar.Images := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    FToolBar.Images := ImageList;
end;

procedure TsmxToolBar.SetCellLeft(Value: Integer);
begin
  FToolBar.Left := Value;
end;

procedure TsmxToolBar.SetCellTop(Value: Integer);
begin
  FToolBar.Top := Value;
end;

procedure TsmxToolBar.SetCellVisible(Value: Boolean);
begin
  FToolBar.Visible := Value;
end;

procedure TsmxToolBar.SetCellWidth(Value: Integer);
begin
  FToolBar.Width := Value;
end;

procedure TsmxToolBar.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FToolBar.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FToolBar.Parent := TWinControl(c);
  end;
end;

{procedure TsmxToolBar.Initialize;
var f: TForm;
begin
  f := TForm.Create(nil);
  try
    FToolBar.Parent := f;
    //FToolBar.Images := ImageList;
    FToolBar.Parent := nil;
  finally
    f.Free;
  end;
end;}

{procedure TsmxToolBar.UnInitialize;
begin
  FToolBar.Images := nil;
end;}

{ TsmxControlBoard }

procedure TsmxControlBoard.CreateChilds;
var i: Integer; c: TsmxBaseCell;
begin
  ToolBoardList.Count := Cfg.BarUnits.Count;
  for i := 0 to Cfg.BarUnits.Count - 1 do
    with Cfg.BarUnits[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomToolBoard then
          ToolBoardList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
end;

function TsmxControlBoard.GetCfg: TsmxControlBoardCfg;
begin
  Result := TsmxControlBoardCfg(inherited Cfg);
end;

procedure TsmxControlBoard.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.BarUnits.Count - 1 do
    with Cfg.BarUnits[i] do
    begin
      ToolBoards[i].CellAlign := UnitAlign;
      ToolBoards[i].CellEnable := UnitEnable;
      ToolBoards[i].CellHeight := UnitHeight;
      ToolBoards[i].CellLeft := UnitLeft;
      ToolBoards[i].CellTop := UnitTop;
      ToolBoards[i].CellVisible := UnitVisible;
      ToolBoards[i].CellWidth := UnitWidth;
    end;
end;

{ TsmxControlBar }

constructor TsmxControlBar.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FControlBar := TControlBar.Create(Self);
  FControlBar.AutoSize := True;
  FControlBar.BevelInner := bvNone;
  InstallParent;
end;

destructor TsmxControlBar.Destroy;
begin
  UnInstallParent;
  FControlBar.Free;
  inherited Destroy;
end;

function TsmxControlBar.GetInternalObject: TObject;
begin
  Result := FControlBar;
end;

function TsmxControlBar.GetCellAlign: TAlign;
begin
  Result := FControlBar.Align;
end;

function TsmxControlBar.GetCellEnable: Boolean;
begin
  Result := FControlBar.Enabled;
end;

function TsmxControlBar.GetCellHeight: Integer;
begin
  Result := FControlBar.Height;
end;

function TsmxControlBar.GetCellLeft: Integer;
begin
  Result := FControlBar.Left;
end;

function TsmxControlBar.GetCellTop: Integer;
begin
  Result := FControlBar.Top;
end;

function TsmxControlBar.GetCellVisible: Boolean;
begin
  Result := FControlBar.Visible;
end;

function TsmxControlBar.GetCellWidth: Integer;
begin
  Result := FControlBar.Width;
end;

procedure TsmxControlBar.SetCellAlign(Value: TAlign);
begin
  FControlBar.Align := Value;
end;

procedure TsmxControlBar.SetCellEnable(Value: Boolean);
begin
  FControlBar.Enabled := Value;
end;

procedure TsmxControlBar.SetCellHeight(Value: Integer);
begin
  FControlBar.Height := Value;
end;

procedure TsmxControlBar.SetCellLeft(Value: Integer);
begin
  FControlBar.Left := Value;
end;

procedure TsmxControlBar.SetCellTop(Value: Integer);
begin
  FControlBar.Top := Value;
end;

procedure TsmxControlBar.SetCellVisible(Value: Boolean);
begin
  FControlBar.Visible := Value;
end;

procedure TsmxControlBar.SetCellWidth(Value: Integer);
begin
  FControlBar.Width := Value;
end;

procedure TsmxControlBar.SetParentCell(Value: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FControlBar.Parent := nil;
  inherited SetParentCell(Value);
  if Assigned(ParentCell) then
  begin
    c := _TsmxBaseCell(ParentCell).GetInternalObject;
    if c is TWinControl then
      FControlBar.Parent := TWinControl(c);
  end;
end;

{ TsmxForm }

procedure TsmxForm.CreateChilds;
var c: TsmxBaseCell; i: Integer;
begin
  PageManagerList.Count := Cfg.PageManagers.Count;
  for i := 0 to Cfg.PageManagers.Count - 1 do
    with Cfg.PageManagers[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomPageManager then
          PageManagerList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
  with Cfg.MainMenu do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomMasterMenu then
        MasterMenu := TsmxCustomMasterMenu(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.AlgorithmList do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomAlgorithmList then
        AlgorithmList := TsmxCustomAlgorithmList(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.ControlBar do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomControlBoard then
        ControlBoard := TsmxCustomControlBoard(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.StateRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        StateRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  {with Cfg.StatusBar do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomStatusBoard then
        StatusBoard := TsmxCustomStatusBoard(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;}
end;

function TsmxForm.GetCfg: TsmxFormCfg;
begin
  Result := TsmxFormCfg(inherited Cfg);
end;

procedure TsmxForm.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.PageManagers.Count - 1 do
    with Cfg.PageManagers[i] do
    begin
      PageManagers[i].CellAlign := UnitAlign;
      PageManagers[i].CellEnable := UnitEnable;
      PageManagers[i].CellHeight := UnitHeight;
      PageManagers[i].CellLeft := UnitLeft;
      PageManagers[i].CellTop := UnitTop;
      PageManagers[i].CellVisible := UnitVisible;
      PageManagers[i].CellWidth := UnitWidth;
    end;
  if Assigned(MasterMenu) then
    with Cfg.MainMenu do
    begin
      MasterMenu.CellAlign := Align;
      MasterMenu.CellEnable := Enable;
      MasterMenu.CellVisible := Visible;
      with PositionSize do
      begin
        MasterMenu.CellHeight := Height;
        MasterMenu.CellLeft := Left;
        MasterMenu.CellTop := Top;
        MasterMenu.CellWidth := Width;
      end;
    end;
  if Assigned(ControlBoard) then
    with Cfg.ControlBar do
    begin
      ControlBoard.CellAlign := Align;
      ControlBoard.CellEnable := Enable;
      ControlBoard.CellVisible := Visible;
      with PositionSize do
      begin
        ControlBoard.CellHeight := Height;
        ControlBoard.CellLeft := Left;
        ControlBoard.CellTop := Top;
        ControlBoard.CellWidth := Width;
      end;
    end;
  if Assigned(AlgorithmList) then
    with Cfg.AlgorithmList do
    begin
      AlgorithmList.IsCreateToolButton := IsCreateToolButton;
      AlgorithmList.IsCreateMenuPoint := IsCreateMenuItem;
    end;
  if Assigned(StateRequest) then
    with Cfg.StateRequest do
    begin
      StateRequest.DatabaseName := DatabaseName;
      StateRequest.OperationMode := Operation;
    end;
  {if Assigned(StatusBoard) then
    with Cfg.StatusBar do
    begin
      StatusBoard.CellAlign := Align;
      StatusBoard.CellEnable := Enable;
      StatusBoard.CellVisible := Visible;
      with PositionSize do
      begin
        StatusBoard.CellHeight := Height;
        StatusBoard.CellLeft := Left;
        StatusBoard.CellTop := Top;
        StatusBoard.CellWidth := Width;
      end;
    end;}
end;

{ TsmxStandardForm }

constructor TsmxStandardForm.Create(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID: Integer; AID: Integer = 0);
begin
  inherited Create(AOwner, ADatabase, ACfgID, AID);
  FForm := TForm.Create(Self);
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  //FormManager.InsertForm(Self);
  //AddFormIntoManagerLib(Self);
  InstallParent;
  Initialize;
  //AddAlgorithms;
  PutState;
end;

constructor TsmxStandardForm.CreateByIntfID(AOwner: TComponent; const ADatabase: IsmxDatabase;
  ACfgID, AIntfID: Integer; AID: Integer = 0);
begin
  inherited CreateByIntfID(AOwner, ADatabase, ACfgID, AIntfID, AID);
  FForm := TForm.Create(Self);
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  //FormManager.InsertForm(Self);
  //AddFormIntoManagerLib(Self);
  InstallParent;
  Initialize;
  //AddAlgorithms;
  PutState;
end;

destructor TsmxStandardForm.Destroy;
begin
  //DelAlgorithms;
  UnInitialize;
  UnInstallParent;
  //FormManager.RemoveForm(Self);
  //DelFormFromManagerLib(Self);
  SetFormManager(nil);
  FForm.Free;
  inherited Destroy;
end;

procedure TsmxStandardForm.CloseForm;
begin
  Form.OnClose := nil;
  Free;
end;

procedure TsmxStandardForm.ProcClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
  CloseForm;
end;

function TsmxStandardForm.GetFormModalResult:  TModalResult;
begin
  Result := FForm.ModalResult;
end;

function TsmxStandardForm.GetInternalObject: TObject;
begin
  Result := FForm;
end;

function TsmxStandardForm.GetCellAlign: TAlign;
begin
  Result := FForm.Align;
end;

function TsmxStandardForm.GetCellEnable: Boolean;
begin
  Result := FForm.Enabled;
end;

function TsmxStandardForm.GetCellHeight: Integer;
begin
  Result := FForm.Height;
end;

function TsmxStandardForm.GetCellLeft: Integer;
begin
  Result := FForm.Left;
end;

function TsmxStandardForm.GetCellTop: Integer;
begin
  Result := FForm.Top;
end;

function TsmxStandardForm.GetCellVisible: Boolean;
begin
  Result := FForm.Visible;
end;

function TsmxStandardForm.GetCellWidth: Integer;
begin
  Result := FForm.Width;
end;

procedure TsmxStandardForm.SetFormManager(Value: TsmxCustomFormManager);
begin
  if Assigned(FormManager) then
    FormManager.RemoveForm(Self);
  inherited SetFormManager(Value);
  if Assigned(FormManager) then
    FormManager.InsertForm(Self);
end;

procedure TsmxStandardForm.SetFormModalResult(Value: TModalResult);
begin
  FForm.ModalResult := Value;
end;

procedure TsmxStandardForm.SetCellAlign(Value: TAlign);
begin
  FForm.Align := Value;
end;

procedure TsmxStandardForm.SetCellEnable(Value: Boolean);
begin
  FForm.Enabled := Value;
end;

procedure TsmxStandardForm.SetCellHeight(Value: Integer);
begin
  FForm.Height := Value;
end;

procedure TsmxStandardForm.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    FForm.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    if Cfg.FormImageIndex >= 0 then
      ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
end;

procedure TsmxStandardForm.SetCellLeft(Value: Integer);
begin
  FForm.Left := Value;
end;

procedure TsmxStandardForm.SetCellTop(Value: Integer);
begin
  FForm.Top := Value;
end;

procedure TsmxStandardForm.SetCellVisible(Value: Boolean);
begin
  FForm.Visible := Value;
end;

procedure TsmxStandardForm.SetCellWidth(Value: Integer);
begin
  FForm.Width := Value;
end;

procedure TsmxStandardForm.ShowForm;
begin
  Form.OnClose := ProcClose;
  Form.Show;
  Prepare;
end;

function TsmxStandardForm.ShowModalForm: TModalResult;
begin
  Prepare;
  Result := Form.ShowModal;
end;

procedure TsmxStandardForm.Initialize;
var c: TObject;
begin
  //if Cfg.FormImageIndex >= 0 then
    //ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);

  if Assigned(MasterMenu) then
  begin
    c := _TsmxBaseCell(MasterMenu).GetInternalObject;
    if c is TMainMenu then
      FForm.Menu := TMainMenu(c);
  end;
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := MasterMenu;
    AlgorithmList.AddAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := ControlBoard;
    AlgorithmList.AddAlgorithmsTo(ControlBoard);
end;

procedure TsmxStandardForm.UnInitialize;
begin
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := nil;
    AlgorithmList.DelAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := nil;
    AlgorithmList.DelAlgorithmsTo(ControlBoard);
  FForm.Menu := nil;
end;

{ TsmxMasterForm }

procedure TsmxMasterForm.CreateChilds;
var c: TsmxBaseCell; i: Integer;
begin
  PageManagerList.Count := Cfg.PageManagers.Count;
  for i := 0 to Cfg.PageManagers.Count - 1 do
    with Cfg.PageManagers[i] do
      if CfgID > 0 then
      begin
        c := NewCell(Self, CfgDatabase, CfgID);
        if c is TsmxCustomPageManager then
          PageManagerList[i] := c else
          raise EsmxCellError.CreateRes(@SCellBuildError);
      end else
        raise EsmxCellError.CreateRes(@SCellBuildError);
  with Cfg.MainMenu do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID); 
      if c is TsmxCustomMasterMenu then
        MasterMenu := TsmxCustomMasterMenu(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.AlgorithmList do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomAlgorithmList then
        AlgorithmList := TsmxCustomAlgorithmList(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.ControlBar do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomControlBoard then
        ControlBoard := TsmxCustomControlBoard(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  with Cfg.StateRequest do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomRequest then
        StateRequest := TsmxCustomRequest(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;
  {with Cfg.StatusBar do
    if CfgID > 0 then
    begin
      c := NewCell(Self, CfgDatabase, CfgID);
      if c is TsmxCustomStatusBoard then
        StatusBoard := TsmxCustomStatusBoard(c) else
        raise EsmxCellError.CreateRes(@SCellBuildError);
    end;}
end;

function TsmxMasterForm.GetCfg: TsmxFormCfg;
begin
  Result := TsmxFormCfg(inherited Cfg);
end;

procedure TsmxMasterForm.InitChilds;
var i: Integer;
begin
  for i := 0 to Cfg.PageManagers.Count - 1 do
    with Cfg.PageManagers[i] do
    begin
      PageManagers[i].CellAlign := UnitAlign;
      PageManagers[i].CellEnable := UnitEnable;
      PageManagers[i].CellHeight := UnitHeight;
      PageManagers[i].CellLeft := UnitLeft;
      PageManagers[i].CellTop := UnitTop;
      PageManagers[i].CellVisible := UnitVisible;
      PageManagers[i].CellWidth := UnitWidth;
    end;
  if Assigned(MasterMenu) then
    with Cfg.MainMenu do
    begin
      MasterMenu.CellAlign := Align;
      MasterMenu.CellEnable := Enable;
      MasterMenu.CellVisible := Visible;
      with PositionSize do
      begin
        MasterMenu.CellHeight := Height;
        MasterMenu.CellLeft := Left;
        MasterMenu.CellTop := Top;
        MasterMenu.CellWidth := Width;
      end;
    end;
  if Assigned(ControlBoard) then
    with Cfg.ControlBar do
    begin
      ControlBoard.CellAlign := Align;
      ControlBoard.CellEnable := Enable;
      ControlBoard.CellVisible := Visible;
      with PositionSize do
      begin
        ControlBoard.CellHeight := Height;
        ControlBoard.CellLeft := Left;
        ControlBoard.CellTop := Top;
        ControlBoard.CellWidth := Width;
      end;
    end;
  if Assigned(AlgorithmList) then
    with Cfg.AlgorithmList do
    begin
      AlgorithmList.IsCreateToolButton := IsCreateToolButton;
      AlgorithmList.IsCreateMenuPoint := IsCreateMenuItem;
    end;
  if Assigned(StateRequest) then
    with Cfg.StateRequest do
    begin
      StateRequest.DatabaseName := DatabaseName;
      StateRequest.OperationMode := Operation;
    end;
  {if Assigned(StatusBoard) then
    with Cfg.StatusBar do
    begin
      StatusBoard.CellAlign := Align;
      StatusBoard.CellEnable := Enable;
      StatusBoard.CellVisible := Visible;
      with PositionSize do
      begin
        StatusBoard.CellHeight := Height;
        StatusBoard.CellLeft := Left;
        StatusBoard.CellTop := Top;
        StatusBoard.CellWidth := Width;
      end;
    end;}
end;

{ TsmxMainForm }

procedure TsmxMainForm.CloseForm;
begin
  if not Assigned(Form) then
    Exit;
  Form.OnClose := nil;
  Form.OnCloseQuery := nil;
  Form := nil;
  Free;
end;

procedure TsmxMainForm.ProcClose(Sender: TObject; var Action: TCloseAction);
begin
  CloseForm;
end;

procedure TsmxMainForm.ProcCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Ask('Çàêðûòü ïðîãðàììó?' {SCloseProgMessage});
end;

function TsmxMainForm.GetInternalObject: TObject;
begin
  Result := FForm;
end;

function TsmxMainForm.GetCellAlign: TAlign;
begin
  if Assigned(FForm) then
    Result := FForm.Align else
    Result := alNone;
end;

function TsmxMainForm.GetCellEnable: Boolean;
begin
  if Assigned(FForm) then
    Result := FForm.Enabled else
    Result := False;
end;

function TsmxMainForm.GetCellHeight: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Height else
    Result := 0;
end;

function TsmxMainForm.GetCellLeft: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Left else
    Result := 0;
end;

function TsmxMainForm.GetCellTop: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Top else
    Result := 0;
end;

function TsmxMainForm.GetCellVisible: Boolean;
begin
  if Assigned(FForm) then
    Result := FForm.Visible else
    Result := False;
end;

function TsmxMainForm.GetCellWidth: Integer;
begin
  if Assigned(FForm) then
    Result := FForm.Width else
    Result := 0;
end;

procedure TsmxMainForm.Initialize;
var c: TObject;
begin
  if not Assigned(FForm) then
    Exit;
  FForm.Left := Cfg.FormPositionSize.Left;
  FForm.Top := Cfg.FormPositionSize.Top;
  FForm.Height := Cfg.FormPositionSize.Height;
  FForm.Width := Cfg.FormPositionSize.Width;
  FForm.Caption := Cfg.FormCaption;
  //if Cfg.FormImageIndex >= 0 then
    //ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
  if Assigned(MasterMenu) then
  begin
    c := _TsmxBaseCell(MasterMenu).GetInternalObject;
    if c is TMainMenu then
      FForm.Menu := TMainMenu(c);
  end;
  InstallParent;
  SetCommonStorage(CommonStorage);
  SetLibraryManager(LibraryManager);
  //SetDatabaseManager(DatabaseManager);
  SetFormManager(FormManager);
  SetImageList(ImageList);
  //AddAlgorithms;
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := MasterMenu;
    AlgorithmList.AddAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := ControlBoard;
    AlgorithmList.AddAlgorithmsTo(ControlBoard);
  PutState;
end;

procedure TsmxMainForm.SetCellAlign(Value: TAlign);
begin
  if Assigned(FForm) then
    FForm.Align := Value;
end;

procedure TsmxMainForm.SetCellEnable(Value: Boolean);
begin
  if Assigned(FForm) then
    FForm.Enabled := Value;
end;

procedure TsmxMainForm.SetCellHeight(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Height := Value;
end;

procedure TsmxMainForm.SetImageList(Value: TCustomImageList);
begin
  if Assigned(ImageList) then
    if Assigned(FForm) then
      FForm.Icon := nil;
  inherited SetImageList(Value);
  if Assigned(ImageList) then
    if Assigned(FForm) then
      if Cfg.FormImageIndex >= 0 then
        ImageList.GetIcon(Cfg.FormImageIndex, FForm.Icon);
end;

procedure TsmxMainForm.SetCellLeft(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Left := Value;
end;

procedure TsmxMainForm.SetCellTop(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Top := Value;
end;

procedure TsmxMainForm.SetCellVisible(Value: Boolean);
begin
  if Assigned(FForm) then
    FForm.Visible := Value;
end;

procedure TsmxMainForm.SetCellWidth(Value: Integer);
begin
  if Assigned(FForm) then
    FForm.Width := Value;
end;

{procedure TsmxMainForm.SetForm(AForm: TForm);
begin
  if Assigned(FForm) then
  begin
    UnInitialize;
    FForm := nil;
  end;
  if Assigned(AForm) then
  begin
    FForm := AForm;
    Initialize;
  end;
end;}

procedure TsmxMainForm.SetForm(Value: TForm);
begin
  if Assigned(FForm) then
    UnInitialize;
  inherited SetForm(Value);
  if Assigned(FForm) then
    Initialize;
end;

procedure TsmxMainForm.ShowForm;
begin
  if not Assigned(Form) then
    Exit;
  Form.OnClose := ProcClose;
  Form.OnCloseQuery := ProcCloseQuery;
  Form.Show;
  Prepare;
end;

procedure TsmxMainForm.UnInitialize;
begin
  if not Assigned(FForm) then
    Exit;
  //DelAlgorithms;
  if Assigned(AlgorithmList) and Assigned(MasterMenu) then
    //AlgorithmList.MasterMenu := nil;
    AlgorithmList.DelAlgorithmsTo(MasterMenu);
  if Assigned(AlgorithmList) and Assigned(ControlBoard) then
    //AlgorithmList.ControlBoard := nil;
    AlgorithmList.DelAlgorithmsTo(ControlBoard);
  SetImageList(nil);
  UnInstallParent;
  FForm.Menu := nil;
end;

initialization
  RegisterClasses([TsmxRequest, TsmxDBColumn, TsmxDBGrid, TsmxActionLibAlgorithm,
    TsmxActionList,TsmxPanelFilterDesk, TsmxPanelSection, TsmxTabSheet,
    TsmxPageControl, TsmxMenuItem, TsmxMainMenu, TsmxToolBar, TsmxControlBar,
    TsmxStandardForm, TsmxMainForm]);

finalization
  UnRegisterClasses([TsmxRequest, TsmxDBColumn, TsmxDBGrid, TsmxActionLibAlgorithm,
    TsmxActionList,TsmxPanelFilterDesk, TsmxPanelSection, TsmxTabSheet,
    TsmxPageControl, TsmxMenuItem, TsmxMainMenu, TsmxToolBar, TsmxControlBar,
    TsmxStandardForm, TsmxMainForm]);

end.
