unit smxWheelDBGrid;

interface

uses
  SysUtils, Windows, Classes, Controls, Grids, DBGrids;

type
  TsmxWheelDBGrid = class(TDBGrid)
  private
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
  protected
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure WheelDownProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure WheelUpProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
  public
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
  end;

implementation

function TsmxWheelDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
  WheelDownProc(Self, Shift, MousePos, Result);
end;

function TsmxWheelDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
  WheelUpProc(Self, Shift, MousePos, Result);
end;

procedure TsmxWheelDBGrid.WheelDownProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  with TDBGrid(Sender) do
    if Assigned(DataSource) then
      if Assigned(DataSource.DataSet) then
        if DataSource.DataSet.Active then
          if DataSource.DataSet.RecNo < DataSource.DataSet.RecordCount then
            DataSource.DataSet.MoveBy(1);
  Handled := True;
end;

procedure TsmxWheelDBGrid.WheelUpProc(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
begin
  with TDBGrid(Sender) do
    if Assigned(DataSource) then
      if Assigned(DataSource.DataSet) then
        if DataSource.DataSet.Active then
          if DataSource.DataSet.RecNo > 0 then
            DataSource.DataSet.MoveBy(-1);
  Handled := True;
end;

end.
 