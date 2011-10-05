unit smxConf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList, smxClasses, smxDBIntf,
  DB, DBTables;

type
  TfrmConf = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Edit2: TEdit;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Button4: TButton;
    Button5: TButton;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Button1: TButton;
    StatusBar1: TStatusBar;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox3: TComboBox;
    CheckBox1: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
  private
    //FDatabaseIntf: IsmxDatabase;
    FCfg: TsmxBaseCfg;
    FTargetRequest: TsmxTargetRequest;
    FImageList: TImageList;
    FProjectManager: TsmxProjectManager;
    FDBConnection: TsmxDBConnection;
    procedure CheckDatabase;
    procedure SaveProgVers;
    procedure FillIntfList;
    //function GetFullStateCfg: String;
    procedure LoadImage;
    procedure FillProjectList;
    function GetCellCfg(CfgID: Integer): String;
    function GetStateCfg(CfgID, IntfID: Integer): String;
    procedure LoadCfg;
    //procedure ReadData(ImageList: TCustomImageList; Stream: TStream);
    //procedure ReadD2Stream(ImageList: TCustomImageList; Stream: TStream);
    //procedure ReadD3Stream(ImageList: TCustomImageList; Stream: TStream);
  public
    //property Database: IsmxDatabase read FDatabaseIntf;
    //property TargetRequest: TsmxTargetRequest read FTargetRequest;
    //property ProjectManager: TsmxProjectManager read FProjectManager;
  end;

var
  frmConf: TfrmConf;

implementation

{$R *.dfm}
{$R ..\Resource\pic.res}

uses
  IniFiles, smxCommonStorage, smxLibManager, smxDBManager, smxFuncs,
  smxTypes, smxConsts, smxProcs, smxProjects, smxAddCell, smxClassFuncs;

type
  { _TsmxBaseCfg }

  _TsmxBaseCfg = class(TsmxBaseCfg)
  end;

  { _TsmxStateCfg }
  _TsmxStateCfg = class(TsmxStateCfg)
  end;

  { _TCustomImageList }

  _TCustomImageList = class(TCustomImageList)
  end;

procedure TfrmConf.FormCreate(Sender: TObject);
begin
  //smxMain.Initialize;
  //FDatabaseIntf := IsmxDatabase(Integer(FuncCallBack(1)));
  //FDatabaseIntf.DatabaseName := 'Base';
  //FDatabaseIntf.LoginPrompt := False;
  FTargetRequest := TsmxTargetRequest.Create(Self);
  FImageList := TImageList.Create(Self);
  LoadCfg;
  LoadImage;
  FProjectManager := TsmxProjectManager.Create(Self);
  FProjectManager.FileName := ComStorage['Path.Cfg'] + ComStorage['Init.FileProject']; //'..\Cfg\proj.dat'; //'proj.dat'; //SFileProjectName;
  FillProjectList;
  SaveProgVers;
  LibManager.LibPath := ComStorage['Path.Lib']; //'..\Lib\';
  LibManager.ProcLibInfoName := ComStorage['LibManager.ProcLibInfo']; //'LibInfo';
  LibManager.CheckComp := ComStorage['LibManager.CheckComp'];
  LibManager.CallLibrary('cCells.dll');
end;

procedure TfrmConf.FormDestroy(Sender: TObject);
begin
  FProjectManager.Free;
  FImageList.Free;
  FTargetRequest.Free;
  //FDatabaseIntf := nil;
  if Assigned(FDBConnection) then
    FDBConnection.Free;
  if Assigned(FCfg) then
    FCfg.Free;
end;

procedure TfrmConf.CheckDatabase;
begin
  if not Assigned(FDBConnection) then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  if not Assigned(FDBConnection.Database) then
    raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
end;

procedure TfrmConf.SaveProgVers;
var VersM, VersL: Cardinal;
begin
  GetFileFullVersion(PChar(Application.ExeName), VersM, VersL);
  ComStorage['ProgVersMajor'] := LongRec(VersM).Hi;
  ComStorage['ProgVersMinor'] := LongRec(VersM).Lo;
  ComStorage['ProgVersRelease'] := LongRec(VersL).Hi;
  ComStorage['ProgVersBuild'] := LongRec(VersL).Lo;
  ComStorage['ProgVers'] :=
    IntToStr(LongRec(VersM).Hi) + '.' +
    IntToStr(LongRec(VersM).Lo) + '.' +
    IntToStr(LongRec(VersL).Hi) + '.' +
    IntToStr(LongRec(VersL).Lo);
end;

procedure TfrmConf.FillIntfList;
var s: String; IntfID: Integer; IntfName: String;
begin
  ComboBox3.Clear;
  CheckDatabase;
  FTargetRequest.Database := FDBConnection.Database;
  s := 'select i.IntfID, replicate(''  '', ti.HLevel) + i.IntfName as IntfName ' +
    'from f_TreeInterfaces(1000001) ti ' +
    'join tInterfaces i on i.IntfID = ti.ID';
  with FTargetRequest.ForRequest(s, dstQuery, True) do
  begin
    First;
    while not Eof do
    begin
      IntfID := FieldByName('IntfID').Value;
      IntfName := FieldByName('IntfName').Value;
      ComboBox3.AddItem(IntfName, TObject(IntfID));
      Next;
    end;
  end;
end;

procedure TfrmConf.LoadImage;
var rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, 'pic', RT_RCDATA);
  try
    //_TCustomImageList(FImageList).ReadData(rs);
    LoadImagesFromStream(FImageList, rs);
  finally
    rs.Free;
  end;
end;

procedure TfrmConf.FillProjectList;
var i: Integer;
begin
  ComboBox1.Clear;
  FProjectManager.ReadProjects;
  for i := 0 to FProjectManager.ProjectList.Count - 1 do
    ComboBox1.Items.Add(FProjectManager.ProjectList[i].ProjectName);
end;

{procedure TfrmConf.ReadData(ImageList: TCustomImageList; Stream: TStream);
var
  CheckInt1, CheckInt2: Integer;
  CheckByte1, CheckByte2: Byte;
  StreamPos: Integer;
begin
  ImageList.Handle := 0;
  StreamPos := Stream.Position;              // check stream signature to
  Stream.Read(CheckInt1, SizeOf(CheckInt1)); // determine a Delphi 2 or Delphi
  Stream.Read(CheckInt2, SizeOf(CheckInt2)); // 3 imagelist stream.  Delphi 2
  CheckByte1 := Lo(LoWord(CheckInt1));       // streams can be read, but only
  CheckByte2 := Hi(LoWord(CheckInt1));       // Delphi 3 streams will be written
  Stream.Position := StreamPos;
  if (CheckInt1 <> CheckInt2) and (CheckByte1 = $49) and (CheckByte2 = $4C) then
    ReadD3Stream(ImageList, Stream) else
    ReadD2Stream(ImageList, Stream);
end;}

{procedure TfrmConf.ReadD2Stream(ImageList: TCustomImageList; Stream: TStream);
var
  FullImage, Image, FullMask, Mask: TBitmap;
  I, J, Size, Pos, Count: Integer;
  SrcRect: TRect;
begin
  ImageList.Clear;
  Stream.ReadBuffer(Size, SizeOf(Size));
  Stream.ReadBuffer(Count, SizeOf(Count));
  FullImage := TBitmap.Create;
  try
    Pos := Stream.Position;
    FullImage.LoadFromStream(Stream);
    Stream.Position := Pos + Size;
    FullMask := TBitmap.Create;
    try
      FullMask.LoadFromStream(Stream);
      Image := TBitmap.Create;
      Image.Width := Width;
      Image.Height := Height;
      Mask := TBitmap.Create;
      Mask.Monochrome := True;
      Mask.Width := Width;
      Mask.Height := Height;
      SrcRect := Rect(0, 0, Width, Height);
      //BeginUpdate;
      try
        for J := 0 to (FullImage.Height div Height) - 1 do
        begin
          if Count = 0 then Break;
          for I := 0 to (FullImage.Width div Width) - 1 do
          begin
            if Count = 0 then Break;
            Image.Canvas.CopyRect(SrcRect, FullImage.Canvas,
              Bounds(I * Width, J * Height, Width, Height));
            Mask.Canvas.CopyRect(SrcRect, FullMask.Canvas,
              Bounds(I * Width, J * Height, Width, Height));
            ImageList.Add(Image, Mask);
            Dec(Count);
          end;
        end;
      finally
        Image.Free;
        Mask.Free;
        //EndUpdate;
      end;
    finally
      FullMask.Free;
    end;
  finally
    FullImage.Free;
  end;
end;}

{procedure TfrmConf.ReadD3Stream(ImageList: TCustomImageList; Stream: TStream);
var
  LAdapter: TStreamAdapter;
  LTemp: TMemoryStream;
  LRetry: Boolean;
  LValue, LBitCount: Byte;
begin
  // attempt a simple read
  LAdapter := TStreamAdapter.Create(Stream);
  try
    ImageList.Handle := ImageList_Read(LAdapter);
  finally
    LAdapter.Free;
  end;

  // if we were not successful then attempt to fix up the really old ComCtl stream
  if not ImageList.HandleAllocated then
  begin

    // make a temp copy of the stream
    LRetry := False;
    LTemp := TMemoryStream.Create;
    try
      Stream.Position := 0;
      LTemp.LoadFromStream(Stream);

      // find the bad value imagelist header info
      LTemp.Position := 18;
      if (LTemp.Read(LValue, 1) = 1) and (LValue = 1) then
      begin

        // find the bitcount data farther on into the BitmapInfoHeader
        LTemp.Position := 56;
        if LTemp.Read(LBitCount, 1) = 1 then
        begin

          // correct the original value
          LValue := LValue or LBitCount;

          // back to the imagelist header info and patch it
          LTemp.Position := 18;
          LRetry := LTemp.Write(LValue, 1) = 1;
        end;
      end;

      // reattempt the load
      if LRetry then
      begin
        LTemp.Position := 0;
        LAdapter := TStreamAdapter.Create(LTemp);
        try
          ImageList.Handle := ImageList_Read(LAdapter);
        finally
          LAdapter.Free;
        end;
      end;

    finally
      LTemp.Free;
    end;

    // if we still didn't succeed then fail
    if not ImageList.HandleAllocated then
      raise EReadError.CreateRes(@SImageReadFail);
  end;
end;}

{function TfrmConf.GetFullStateCfg;

  procedure AddNodes(const ANode: IXMLNode; AUnit: TsmxStateUnit);
  var i: Integer; n: IXMLNode;
  begin
    n := ANode.AddChild('cell');
    with n do
    begin
      Attributes['id'] := AUnit.CfgID;
      Attributes['enable'] := AUnit.UnitEnable;
      Attributes['visible'] := AUnit.UnitVisible;
    end;
    for i := 0 to AUnit.Count - 1 do
      AddNodes(n, AUnit[i]);
  end;

var r, n, n2, n3: IXMLNode; i, j: Integer; XMLDocIntf: IXMLDocument;
begin
  Result := '';
  if not (FCfg is TsmxStateCfg) then
    Exit;
  try
    XMLDocIntf := NewXMLDocument;
    r := XMLDocIntf.ChildNodes.FindNode('root');
    if Assigned(r) then
      r.ChildNodes.Clear else
      r := XMLDocIntf.AddChild('root');

    with TsmxStateCfg(FCfg) do
    begin
      n := r.AddChild('states');
      for i := 0 to CellStates.Count - 1 do
      begin
        n2 := n.AddChild('state');
        n2.Attributes['id'] := CellStates[i].ID;
        n3 := n2.AddChild('cells');
        for j := 0 to CellStates[i].StateUnits.Root.Count - 1 do
          AddNodes(n3, CellStates[i].StateUnits.Root[j]);
      end;
    end;
    Result := FormatXMLText(XMLDocIntf.XML.Text); 
  finally
    XMLDocIntf := nil;
  end;
end;}

function TfrmConf.GetCellCfg(CfgID: Integer): String;
begin
  CheckDatabase;
  if CfgID > 0 then
  begin
    FCfg := smxClassFuncs.NewCfg(nil, FDBConnection.Database, CfgID);
    FCfg.Initialize;
    Result := _TsmxBaseCfg(FCfg).XMLText;
  end else
    Result := '';
end;

function TfrmConf.GetStateCfg(CfgID, IntfID: Integer): String;
begin
  CheckDatabase;
  if (CfgID > 0) and (IntfID > 0) then
  begin
    //FCfg := TsmxStateCfg.CreateByIntfID(nil, FDBConnection.Database, CfgID, IntfID);
    FCfg := TsmxStateCfg.Create(nil);
    TsmxStateCfg(FCfg).CfgDatabase := FDBConnection.Database;
    TsmxStateCfg(FCfg).CfgID := CfgID;
    TsmxStateCfg(FCfg).IntfID := IntfID;
    FCfg.Initialize;
    if CheckBox1.Checked then
      Result := _TsmxStateCfg(FCfg).FullXMLText else //GetFullStateCfg
      Result := _TsmxStateCfg(FCfg).XMLText;
  end else
    Result := '';
end;

procedure TfrmConf.Button1Click(Sender: TObject);
begin
  frmProjects := TfrmProjects.Create(Self);
  try
    if frmProjects.ShowModal = mrOk then
    begin
      FillProjectList;
      ComboBox1.ItemIndex := -1;
      ComboBox1.OnChange(nil);
    end;  
  finally
    FreeAndNil(frmProjects);
  end;
end;

procedure TfrmConf.Button4Click(Sender: TObject);
var CfgID: Integer; IntfID: Integer;
begin
  //if not Assigned(FDBConnection) then
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfDatabaseInvalid);
  //if not FDBConnection.Database.Connected then
    //raise EsmxDBInterfaceError.CreateRes(@SDBIntfConnectFailed);
  //CheckDatabase;
  if Assigned(FCfg) then
    FreeAndNil(FCfg);
  Memo1.Lines.Clear;
  CfgID := StrToIntDef(Edit2.Text, 0);
  if ComboBox3.ItemIndex >= 0 then
    IntfID := Integer(ComboBox3.Items.Objects[ComboBox3.ItemIndex]) else
    IntfID := 0;
  case ComboBox2.ItemIndex of
    0: Memo1.Lines.Text := GetCellCfg(CfgID);
    1: Memo1.Lines.Text := GetStateCfg(CfgID, IntfID);
  end;
end;

procedure TfrmConf.Button5Click(Sender: TObject);
begin
  //CheckDatabase;
  if Assigned(FCfg) then
  begin
    _TsmxBaseCfg(FCfg).XMLText := Memo1.Lines.Text;
    FCfg.Finalize;
  end;
end;

procedure TfrmConf.Button6Click(Sender: TObject);
begin
  if StrToIntDef(Edit1.Text, -1) >= 0 then
    Edit1.Text := HotKeyToStr(StrToInt(Edit1.Text)) else
    Edit1.Text := IntToStr(StrToHotKey(Edit1.Text));
end;

procedure TfrmConf.Button7Click(Sender: TObject);
var i: integer; form: TForm; lv: TListView; //il: TImageList;
begin
  //il := TImageList(Integer(FuncCallBack(5)));
  form := TForm.Create(Application);
  with form do
  try
    ClientHeight := 480;
    ClientWidth := 640;
    Position := poScreenCenter;
    lv := TListView.Create(form);
    with lv do
    try
      Parent := form;
      Align := alClient;
      LargeImages := FImageList;
      for i := 0 to FImageList.Count - 1 do
        with Items.Add do
        begin
          Caption := IntToStr(i);
          ImageIndex := i;
        end;
      ShowModal;
    finally
      FreeAndNil(lv);
    end;
  finally
    FreeAndNil(form);
  end;    
end;

procedure TfrmConf.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0:
    begin
      CheckBox1.Enabled := False;
      Button5.Enabled := True;
    end;
    1:
    begin
      CheckBox1.Enabled := True;
      if CheckBox1.Checked then
        Button5.Enabled := False else
        Button5.Enabled := True;
    end;
  end;
end;

procedure TfrmConf.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Button5.Enabled := False else
    Button5.Enabled := True;
  Button4.OnClick(nil);
end;

procedure TfrmConf.ComboBox1Change(Sender: TObject);
var i: Integer; s: String; pr: TsmxProjectItem;
begin
  if Assigned(FDBConnection) then
    FreeAndNil(FDBConnection);
  StatusBar1.Panels[0].Text := 'Проект:';
  StatusBar1.Panels[1].Text := 'Статус: disconnect';
  ComboBox3.Clear;

  i := ComboBox1.ItemIndex;
  if i >= 0 then
  begin
    s := ComboBox1.Items[i];
    pr := FProjectManager.ProjectList.FindByName(s);
    if Assigned(pr) then
    begin
      //FDBConnection := TsmxDBConnection.Create(nil, pr.Generation, pr.DatabaseName, pr.LibraryName, pr.FunctionNameOrProgID);
      FDBConnection := TsmxDBConnection.Create(nil);
      with FDBConnection do
      begin
        DatabaseName := pr.DatabaseName;
        LibraryName := pr.LibraryName;
        FunctionNameOrProgID := pr.FunctionNameOrProgID;
        GenerationMode := pr.Generation;
        DriverName := pr.DriverName;
        LoginPrompt := pr.LoginPrompt;
        Params := pr.Params;
        UserName := pr.UserName;
        Password := pr.Password;
        LibraryManager := LibManager;
        DatabaseManager := DBManager;
        ConnectToDatabase;
        //FTargetRequest.Database := Database;
        //FillIntfList;
        StatusBar1.Panels[0].Text := 'Проект: ' + s;
        StatusBar1.Panels[1].Text := 'Статус: connect';
      end;
    end;
  end;
end;

procedure TfrmConf.Button2Click(Sender: TObject);
begin
  if Assigned(FCfg) then
  begin
    Memo1.Clear;
    FCfg.Clear;
    _TsmxBaseCfg(FCfg).WriteCfg;
    case ComboBox2.ItemIndex of
      0: Memo1.Lines.Text := _TsmxBaseCfg(FCfg).XMLText;
      1: if CheckBox1.Checked then
           Memo1.Lines.Text := _TsmxStateCfg(FCfg).FullXMLText else
           Memo1.Lines.Text := _TsmxStateCfg(FCfg).XMLText;
    end;
  end;
end;

procedure TfrmConf.Button3Click(Sender: TObject);
begin
  CheckDatabase;
  frmAddCell := TfrmAddCell.Create(nil);
  try
    frmAddCell.Database := FDBConnection.Database;
    if frmAddCell.ShowModal = mrOk then
    begin
      ComboBox2.ItemIndex := 0;
      Edit2.Text := frmAddCell.Edit1.Text;
      Button4.Click;
      Button2.Click;
    end;
  finally
    frmAddCell.Free;
  end;
end;

procedure TfrmConf.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Button4.Click;
end;

procedure TfrmConf.LoadCfg;
var f: TIniFile; sl, sl2: TStringList; i, j: Integer;
begin
  ComStorage['ConfPath'] := ExtractFilePath(Application.ExeName);
  if FileExists(ComStorage['ConfPath'] + SFileConfigurationName) then
  begin
    f := TIniFile.Create(ComStorage['ConfPath'] + SFileConfigurationName);
    try
      sl := TStringList.Create;
      try
        sl2 := TStringList.Create;
        try
          f.ReadSections(sl);
          for i := 0 to sl.Count - 1 do
          begin
            f.ReadSectionValues(sl[i], sl2);
            for j := 0 to sl2.Count - 1 do
              ComStorage[sl.Strings[i] + '.' + sl2.Names[j]] := sl2.Values[sl2.Names[j]]; //sl2.ValueFromIndex[j];
          end;
        finally
          sl2.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      f.Free;
    end;
  end;
end;

end.
