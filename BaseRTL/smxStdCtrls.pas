{**************************************}
{                                      }
{            SalesMan v1.0             }
{       StandardControls classes       }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxStdCtrls;

interface

uses
  SysUtils, Windows, Classes, Controls, Grids, DBGrids, Mask, Buttons, ImgList,
  Messages;

type
  { TsmxWheelDBGrid }

  TsmxWheelDBGrid = class(TDBGrid)
  private
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    procedure WheelDownProc;
    procedure WheelUpProc;
  protected
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
  end;

  { TsmxButtonMaskEdit }

  TsmxGlyphKind = (gkNone, gkImage, gkDropDown, gkEllipsis);

  TsmxButtonMaskEdit = class(TMaskEdit)
  private
    FBtnControl: TWinControl;
    FButton: TSpeedButton;
    FButtonGlyphKind: TsmxGlyphKind;
    FButtonImageIndex: Integer;
    FImages: TCustomImageList;
    FOnButtonClick: TNotifyEvent;
    procedure ButtonClick(Sender: TObject);
    function GetButtonCaption: String;
    function GetButtonHint: String;
    function GetButtonWidth: Integer;
    procedure RecreateGlyph;
    procedure SetEditRect;
    procedure SetImages(Value: TCustomImageList);
    procedure UpdateBtnBounds;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetButtonCaption(const Value: String); virtual;
    procedure SetButtonGlyphKind(Value: TsmxGlyphKind); virtual;
    procedure SetButtonHint(const Value: String); virtual;
    procedure SetButtonImageIndex(Value: Integer); virtual;
    procedure SetButtonWidth(Value: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ButtonCaption: String read GetButtonCaption write SetButtonCaption;
    property ButtonGlyphKind: TsmxGlyphKind read FButtonGlyphKind write SetButtonGlyphKind;
    property ButtonHint: String read GetButtonHint write SetButtonHint;
    property ButtonImageIndex: Integer read FButtonImageIndex write SetButtonImageIndex default -1;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property Images: TCustomImageList read FImages write SetImages;

    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

implementation

uses
  Graphics, Forms;

{ TsmxWheelDBGrid }

function TsmxWheelDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  WheelDownProc;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result)
  else
    Result := True;
end;

procedure TsmxWheelDBGrid.WheelDownProc;
begin
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    with DataSource.DataSet do
      if Active and (RecNo < RecordCount) then
        MoveBy(1);
end;

function TsmxWheelDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  WheelUpProc;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result)
  else
    Result := True;
end;

procedure TsmxWheelDBGrid.WheelUpProc;
begin
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    with DataSource.DataSet do
      if Active and (RecNo > 0) then
        MoveBy(-1);
end;

{ TsmxButtonMaskEdit }

constructor TsmxButtonMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  FBtnControl := TWinControl.Create(Self);
  FBtnControl.ControlStyle := FBtnControl.ControlStyle + [csReplicatable];
  FBtnControl.SetBounds(Width - 17 - 4, 0, 17, 17);
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FButton := TSpeedButton.Create(Self);
  FButton.ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks, csReplicatable];
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Margin := 0;
  FButton.Spacing := 0;
  FButton.ShowHint := True;
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  FButton.OnClick := ButtonClick;
  FButtonImageIndex := -1;
end;

procedure TsmxButtonMaskEdit.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Sender);
end;

procedure TsmxButtonMaskEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN or ES_LEFT;
end;

procedure TsmxButtonMaskEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

function TsmxButtonMaskEdit.GetButtonCaption: String;
begin
  Result := FButton.Caption;
end;

procedure TsmxButtonMaskEdit.SetButtonCaption(const Value: String);
begin
  FButton.Caption := Value;
end;

procedure TsmxButtonMaskEdit.SetButtonGlyphKind(Value: TsmxGlyphKind);
begin
  if FButtonGlyphKind <> Value then
  begin
    FButtonGlyphKind := Value;
    RecreateGlyph;
    if FButtonGlyphKind = gkDropDown then
    begin
      ButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
      with FButton do
        ControlStyle := ControlStyle + [csFixedWidth];
    end;
  end;
end;

function TsmxButtonMaskEdit.GetButtonHint: String;
begin
  Result := FButton.Hint;
end;

procedure TsmxButtonMaskEdit.SetButtonHint(const Value: String);
begin
  FButton.Hint := Value;
end;

function TsmxButtonMaskEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TsmxButtonMaskEdit.SetButtonWidth(Value: Integer);
begin
  FBtnControl.Left := FBtnControl.Left + FBtnControl.Width - Value;
  FBtnControl.Width := Value;
  FButton.Width := Value;
  SetEditRect;
end;

procedure TsmxButtonMaskEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Images) and (Operation = opRemove) then
    Images := nil;
end;

procedure TsmxButtonMaskEdit.RecreateGlyph;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, G, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      if FButton.Width - 6 > 1 then
        Width := FButton.Width - 6 else
        Width := 1;
      Height := 4;
      W := 2;
      G := (Result.Width - 3 * W) div 2;
      if G <= 0 then G := 1;
      if G > 3 then G := 3;
      I := (Width - 3 * W - 2 * G) div 2 + 1;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + G + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * G + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

var
  NewGlyph: TBitmap;
begin
  FButton.Glyph.Assign(nil);
  case FButtonGlyphKind of
    gkImage:
    begin
      if Assigned(FImages) and (FButtonImageIndex <> -1) and (FButtonImageIndex < FImages.Count) then
        FImages.GetBitmap(FButtonImageIndex, FButton.Glyph);
    end;
    gkDropDown:
      FButton.Glyph.Handle := Windows.LoadBitmap(0, PChar(32738));
    gkEllipsis:
    begin
      NewGlyph := CreateEllipsisGlyph;
      try
        FButton.Glyph := NewGlyph;
      finally
        NewGlyph.Destroy;
      end;
    end;
  end;
end;

procedure TsmxButtonMaskEdit.SetButtonImageIndex(Value: Integer);
begin
  if FButtonImageIndex <> Value then
  begin
    FButtonImageIndex := Value;
    if FButtonGlyphKind = gkImage then
      RecreateGlyph;
  end;
end;

procedure TsmxButtonMaskEdit.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, 0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TsmxButtonMaskEdit.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    if Assigned(FImages) then
      FImages.FreeNotification(Self);
    if FButtonGlyphKind = gkImage then
      RecreateGlyph;
  end;
end;

procedure TsmxButtonMaskEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
  if NewStyleControls then
  begin
    if Ctl3D and (BorderStyle = bsSingle) then
      BtnRect := Classes.Bounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 4)
    else
    begin
      if BorderStyle = bsSingle then
        BtnRect := Classes.Bounds(Width - FButton.Width - 2, 2, FButton.Width, Height - 4)
      else
        BtnRect := Classes.Bounds(Width - FButton.Width, 0, FButton.Width, Height);
    end;
  end else
    BtnRect := Classes.Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

procedure TsmxButtonMaskEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateBtnBounds;
end;

end.
 