unit smxStatusBar;

interface

uses
  Classes, ComCtrls, Controls, smxClasses, {smxCustomStatusBoard,} smxDBIntf;

type
  { TsmxStatusBoardCfg }

  TsmxStatusBoardCfg = class(TsmxCellCfg)
  protected
    procedure ReadCfg; override;
    procedure WriteCfg; override;
  end;

  { TsmxStatusBoard }

  TsmxStatusBoard = class(TsmxCustomStatusBoard)
  private
    function GetCfg: TsmxStatusBoardCfg;
  protected
    property Cfg: TsmxStatusBoardCfg read GetCfg;
  end;

  { TsmxStatusBar }

  TsmxStatusBar = class(TsmxStatusBoard)
  private
    FStatusBar: TStatusBar;
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
    procedure SetParentCell(AParent: TsmxBaseCell); override;

    property StatusBar: TStatusBar read FStatusBar;
  public
    constructor Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer); override;
    destructor Destroy; override;
  end;

implementation

type
  { _TsmxBaseCell }

  _TsmxBaseCell = class(TsmxBaseCell)
  end;

{ TsmxStatusBoardCfg }

procedure TsmxStatusBoardCfg.ReadCfg;
begin
end;

procedure TsmxStatusBoardCfg.WriteCfg;
begin
end;

{ TsmxStatusBoard }

function TsmxStatusBoard.GetCfg: TsmxStatusBoardCfg;
begin
  Result := TsmxStatusBoardCfg(inherited Cfg);
end;

{ TsmxStatusBar }

constructor TsmxStatusBar.Create(AOwner: TComponent; const ADatabase: IsmxDatabase; ACfgID: Integer);
begin
  inherited Create(AOwner, ADatabase, ACfgID);
  FStatusBar := TStatusBar.Create(Self);
end;

destructor TsmxStatusBar.Destroy;
begin
  FStatusBar.Free;
  inherited Destroy;
end;

function TsmxStatusBar.GetInternalObject: TObject;
begin
  Result := FStatusBar;
end;

function TsmxStatusBar.GetCellAlign: TAlign;
begin
  Result := FStatusBar.Align;
end;

function TsmxStatusBar.GetCellEnable: Boolean;
begin
  Result := FStatusBar.Enabled;
end;

function TsmxStatusBar.GetCellHeight: Integer;
begin
  Result := FStatusBar.Height;
end;

function TsmxStatusBar.GetCellLeft: Integer;
begin
  Result := FStatusBar.Left;
end;

function TsmxStatusBar.GetCellTop: Integer;
begin
  Result := FStatusBar.Top;
end;

function TsmxStatusBar.GetCellVisible: Boolean;
begin
  Result := FStatusBar.Visible;
end;

function TsmxStatusBar.GetCellWidth: Integer;
begin
  Result := FStatusBar.Width;
end;

procedure TsmxStatusBar.SetCellAlign(Value: TAlign);
begin
  FStatusBar.Align := Value;
end;

procedure TsmxStatusBar.SetCellEnable(Value: Boolean);
begin
  FStatusBar.Enabled := Value;
end;

procedure TsmxStatusBar.SetCellHeight(Value: Integer);
begin
  FStatusBar.Height := Value;
end;

procedure TsmxStatusBar.SetCellLeft(Value: Integer);
begin
  FStatusBar.Left := Value;
end;

procedure TsmxStatusBar.SetCellTop(Value: Integer);
begin
  FStatusBar.Top := Value;
end;

procedure TsmxStatusBar.SetCellVisible(Value: Boolean);
begin
  FStatusBar.Visible := Value;
end;

procedure TsmxStatusBar.SetCellWidth(Value: Integer);
begin
  FStatusBar.Width := Value;
end;

procedure TsmxStatusBar.SetParentCell(AParent: TsmxBaseCell);
var c: TObject;
begin
  if Assigned(ParentCell) then
    FStatusBar.Parent := nil;
  inherited SetParentCell(AParent);
  if Assigned(AParent) then
  begin
    c := _TsmxBaseCell(AParent).GetInternalObject;
    if c is TWinControl then
      FStatusBar.Parent := TWinControl(c);
  end;
end;

initialization
  RegisterClasses([TsmxStatusBoardCfg, TsmxStatusBar]);
  //RegistrationClasses([TsmxStatusBoardCfg, TsmxStatusBar]);

end.
