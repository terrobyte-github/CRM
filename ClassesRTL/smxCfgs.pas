{**************************************}
{                                      }
{            SalesMan v1.0             }
{        Configuration classes         }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxCfgs;

interface

uses
  Classes, XMLIntf, smxBaseClasses, smxClasses;

type
  { TsmxTypeCfg }

  TsmxTypeCfg = class(TsmxBaseCfg)
  private
    FCellClass: TsmxBaseCellClass;
    FCellClassName: String;
    FCfgClass: TsmxBaseCfgClass;
    FCfgClassName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadType(const Node: IXMLNode); virtual;
    procedure SetCellClass(Value: TsmxBaseCellClass); virtual;
    procedure SetCellClassName(const Value: String); virtual;
    procedure SetCfgClass(Value: TsmxBaseCfgClass); virtual;
    procedure SetCfgClassName(const Value: String); virtual;
    procedure WriteType(const Node: IXMLNode); virtual;
  public
    procedure ClearCfg; override;
    procedure Read; override;
    procedure Write; override;

    property CellClass: TsmxBaseCellClass read FCellClass write SetCellClass;
    property CfgClass: TsmxBaseCfgClass read FCfgClass write SetCfgClass;
  published
    property CellClassName: String read FCellClassName write SetCellClassName;
    property CfgClassName: String read FCfgClassName write SetCfgClassName;
  end;

  { TsmxCellfg }

  TsmxCellCfg = class(TsmxBaseCfg)
  private
    FCurNode: IXMLNode;
    FFindList: TList;
    FResolvedList: TsmxResolvedKit;
    function GetCellOwner: TsmxBaseCell;
    function GetResolvedList: TsmxResolvedKit;
    function GetFindList: TList;
  protected
    function GetCurNode: IXMLNode; virtual;
    procedure SetCurNode(const Value: IXMLNode); virtual;
    procedure SetFindList(Value: TList); virtual;
    procedure SetResolvedList(Value: TsmxResolvedKit); virtual;
  public
    destructor Destroy; override;
    procedure ClearCfg; override;
    procedure ClearXML; override;
    procedure Read; override;
    procedure ReadCell(Cell: TsmxBaseCell); virtual;
    procedure Write; override;
    procedure WriteCell(Cell: TsmxBaseCell); virtual;

    property CellOwner: TsmxBaseCell read GetCellOwner;
    property CurNode: IXMLNode read GetCurNode write SetCurNode;
    property FindList: TList read GetFindList write SetFindList;
    property ResolvedList: TsmxResolvedKit read GetResolvedList write SetResolvedList;
  end;

  { TsmxStateCfg }

  TsmxStateCfg = class(TsmxBaseCfg)
  private
    FIntfID: Integer;
    FRecID: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadIntf(const Node: IXMLNode; ID: Integer); virtual;
    procedure SetIntfID(Value: Integer); virtual;
    procedure SetRecID(Value: Integer); virtual;
    procedure WriteIntf(const Node: IXMLNode; ID: Integer); virtual;
  public
    //procedure Assign(Source: TPersistent); override;
    procedure Load; override;
    procedure Read; override;
    procedure Remove; override;
    procedure Save; override;
    procedure Write; override;

    property IntfID: Integer read FIntfID write SetIntfID;
    property RecID: Integer read FRecID write SetRecID;
  end;

implementation

uses
  Variants, SysUtils, smxClassProcs, smxDBFuncs, smxDBIntf, smxFuncs, smxConsts,
  smxTypes;

{ TsmxTypeCfg }

procedure TsmxTypeCfg.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TsmxTypeCfg then
  begin
    TsmxTypeCfg(Dest).CellClass := CellClass;
    TsmxTypeCfg(Dest).CfgClass := CfgClass;
  end;
end;

procedure TsmxTypeCfg.ClearCfg;
begin
  inherited ClearCfg;
  CellClassName := '';
  CfgClassName := '';
end;

procedure TsmxTypeCfg.Read;
var
  n: IXMLNode;
begin
  inherited Read;
  n := RootNode.ChildNodes.FindNode(smxConsts.cTypeNodeName);
  if Assigned(n) then
    ReadType(n);
end;

procedure TsmxTypeCfg.Write;
var
  n: IXMLNode;
begin
  inherited Write;
  n := RootNode.AddChild(smxConsts.cTypeNodeName);
  WriteType(n);
end;

procedure TsmxTypeCfg.ReadType(const Node: IXMLNode);
begin
  CellClassName := Node.Attributes['CellClassName'];
  CfgClassName := Node.Attributes['CfgClassName'];
end;

procedure TsmxTypeCfg.WriteType(const Node: IXMLNode);
begin
  Node.Attributes['CellClassName'] := CellClassName;
  Node.Attributes['CfgClassName'] := CfgClassName;
end;

procedure TsmxTypeCfg.SetCellClass(Value: TsmxBaseCellClass);
begin
  if Assigned(FCellClass) then
    FCellClassName := '';
  FCellClass := Value;
  if Assigned(FCellClass) then
    FCellClassName := FCellClass.ClassName;
end;

procedure TsmxTypeCfg.SetCellClassName(const Value: String);
begin
  if FCellClassName <> '' then
    FCellClass := nil;
  FCellClassName := Value;
  if FCellClassName <> '' then
    FCellClass := TsmxBaseCellClass(Classes.FindClass(FCellClassName));
end;

procedure TsmxTypeCfg.SetCfgClass(Value: TsmxBaseCfgClass);
begin
  if Assigned(FCfgClass) then
    FCfgClassName := '';
  FCfgClass := Value;
  if Assigned(FCfgClass) then
    FCfgClassName := FCfgClass.ClassName;
end;

procedure TsmxTypeCfg.SetCfgClassName(const Value: String);
begin
  if FCfgClassName <> '' then
    FCfgClass := nil;
  FCfgClassName := Value;
  if FCfgClassName <> '' then
    FCfgClass := TsmxBaseCfgClass(Classes.FindClass(FCfgClassName));
end;

{ TsmxCellCfg }

destructor TsmxCellCfg.Destroy;
begin
  SetCurNode(nil);
  if Assigned(FResolvedList) then
    FResolvedList.Free;
  if Assigned(FFindList) then
    FFindList.Free;
  inherited Destroy;
end;

procedure TsmxCellCfg.ClearCfg;
begin
  inherited ClearCfg;
  if Assigned(FFindList) then
    FFindList.Clear;
  if Assigned(FResolvedList) then
    FResolvedList.Clear;
end;

procedure TsmxCellCfg.ClearXML;
begin
  inherited ClearXML;
  CurNode := nil;
end;

function TsmxCellCfg.GetCellOwner: TsmxBaseCell;
begin
  if Owner is TsmxBaseCell then
    Result := TsmxBaseCell(Owner) else
    Result := nil;
end;

function TsmxCellCfg.GetCurNode: IXMLNode;
begin
  if not Assigned(FCurNode) then
    FCurNode := RootNode;
  Result := FCurNode;
end;

procedure TsmxCellCfg.SetCurNode(const Value: IXMLNode);
begin
  FCurNode := Value;
end;

function TsmxCellCfg.GetFindList: TList;
begin
  if not Assigned(FFindList) then
    FFindList := TList.Create;
  Result := FFindList;
end;

procedure TsmxCellCfg.SetFindList(Value: TList);
begin
  FindList.Assign(Value);
end;

function TsmxCellCfg.GetResolvedList: TsmxResolvedKit;
begin
  if not Assigned(FResolvedList) then
    FResolvedList := TsmxResolvedKit.Create(TsmxResolvedItem);
  Result := FResolvedList;
end;

procedure TsmxCellCfg.SetResolvedList(Value: TsmxResolvedKit);
begin
  ResolvedList.Assign(Value);
end;

procedure TsmxCellCfg.Read;
var
  n: IXMLNode;
begin
  inherited Read;
  if Assigned(CellOwner) then
  begin
    n := RootNode.ChildNodes.FindNode(smxConsts.cCellNodeName);
    if Assigned(n) then
    begin
      CurNode := n;
      ReadCell(CellOwner);
      smxClassProcs.AllCells(CellOwner, FindList, []);
      smxClassProcs.ResolvedProps(ResolvedList, FindList);
    end;
  end;
end;

procedure TsmxCellCfg.Write;
var
  n: IXMLNode;
begin
  inherited Write;
  if Assigned(CellOwner) then
  begin
    smxClassProcs.AllCells(CellOwner, FindList, []);
    n := RootNode.AddChild(smxConsts.cCellNodeName);
    CurNode := n;
    WriteCell(CellOwner);
  end;
end;

procedure TsmxCellCfg.ReadCell(Cell: TsmxBaseCell);
begin
  if Assigned(Cell) then
  begin
    smxClassProcs.ReadProps(CellOwner, Cell, CurNode, ResolvedList);
    Cell.SetProperties(Self);
  end;
end;

procedure TsmxCellCfg.WriteCell(Cell: TsmxBaseCell);
begin
  if Assigned(Cell) then
  begin
    smxClassProcs.WriteProps(CellOwner, Cell, CurNode, FindList);
    Cell.GetProperties(Self);
  end;
end;

{ TsmxStateCfg }

{procedure TsmxStateCfg.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsmxStateCfg then
  begin
    IntfID := TsmxStateCfg(Source).IntfID;
    RecID := TsmxStateCfg(Source).RecID;
  end;
end;}

procedure TsmxStateCfg.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TsmxStateCfg then
  begin
    TsmxStateCfg(Dest).IntfID := IntfID;
    TsmxStateCfg(Dest).RecID := RecID;
  end;
end;

procedure TsmxStateCfg.Load;
var
  Key, Value: Variant;
  KeySense: TsmxDataSense;
begin
  if not Assigned(SelectDataSet)
      or not ((FRecID <> 0) or ((CfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, CfgID, 'load'], Self);
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([CfgID, FIntfID]);
    KeySense := dsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := dsKey;
  end;
  if smxDBFuncs.GetValueByKey(SelectDataSet, Key, Value,
      {FSelectRequest.PerformanceMode,} KeySense) then
  begin
    if smxDBFuncs.LocateByKey(SelectDataSet, Key, KeySense) then
    begin
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value,
          {FSelectRequest.PerformanceMode,} dsKey) then
        FRecID := Value else
        FRecID := 0;
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value{,
          FSelectRequest.PerformanceMode}) then
        XMLDoc.XML.Text := Value else
        XMLDoc.XML.Text := '';
      try
        XMLDoc.Active := True;
      except
        on E: Exception do
          raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
            [ClassName, CfgID, 'load', E.Message], Self);
      end;
    end;
  end;
end;

procedure TsmxStateCfg.Save;
var
  DataSet: IsmxDataSet;
  //Request: IsmxDataSet;
  //Performance: TsmxPerformanceMode;
  Key, Value: Variant;
  KeySense: TsmxDataSense;
begin
  if ((((FRecID = 0) and not Assigned(InsertDataSet))
        or ((FRecID <> 0) and not Assigned(UpdateDataSet)))
      or not ((FRecID <> 0) or ((CfgID <> 0) and (FIntfID <> 0)))) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, CfgID, 'save'], Self);
  if FRecID = 0 then
  begin
    DataSet := InsertDataSet; // ModifyRequests[mrInsert];
    //Performance := FSelectRequest.InsertPerformance; // ModifyPerformances[mrInsert];
    Key := Variants.VarArrayOf([CfgID, FIntfID]);
    KeySense := dsForeignKey;
  end else
  begin
    DataSet := UpdateDataSet; // ModifyRequests[mrUpdate];
    //Performance := FSelectRequest.UpdatePerformance; // ModifyPerformances[mrUpdate];
    Key := FRecID;
    KeySense := dsKey;
  end;
  {if not Assigned(Request) then
    raise EsmxCellError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'save'], FCfgID);}
  {if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([FCfgID, FIntfID]);
    KeySense := fsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := fsKey;
  end;}
  if smxDBFuncs.SetValueByKey(DataSet, Key, XMLDoc.XML.Text, {Performance,} KeySense) then
    if smxDBFuncs.GetCurrentValue(DataSet, Value, {Performance,} dsKey) then
      FRecID := Value;
end;

procedure TsmxStateCfg.Read;
var
  Value: Variant;
  CurIntfID: Integer;
  XMLTextTemp: String;
begin
  inherited Read;
  XMLTextTemp := XMLDoc.XML.Text;
  if Assigned(SelectDataSet) then
  begin
    SelectDataSet.First;
    while not SelectDataSet.Eof do
    begin
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value{, FSelectRequest.PerformanceMode}) then
        XMLDoc.XML.Text := Value else
        XMLDoc.XML.Text := '';
      if smxDBFuncs.GetCurrentValue(SelectDataSet, Value, {FSelectRequest.PerformanceMode,} dsForeignKey) then
        CurIntfID := smxFuncs.GetSingleValue(Value, 0, 1) else
        CurIntfID := 0;
      try
        XMLDoc.Active := True;
        ReadIntf(RootNode, CurIntfID);
      except
        on E: Exception do
          raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
            [ClassName, CfgID, 'read', E.Message], Self);
      end;
      SelectDataSet.Next;
    end;
  end;
  XMLDoc.XML.Text := XMLTextTemp;
  try
    XMLDoc.Active := True;
    ReadIntf(RootNode, IntfID);
  except
    on E: Exception do
      raise EsmxCellError.CreateByComponent(@smxConsts.rsCfgActionErrorM,
        [ClassName, CfgID, 'read', E.Message], Self);
  end;
end;

procedure TsmxStateCfg.Write;
begin
  inherited Write;
  WriteIntf(RootNode, IntfID);
end;

procedure TsmxStateCfg.ReadIntf(const Node: IXMLNode; ID: Integer);
begin
end;

procedure TsmxStateCfg.WriteIntf(const Node: IXMLNode; ID: Integer);
begin
end;

procedure TsmxStateCfg.Remove;
var
  //Request: IsmxDataSet;
  //Performance: TsmxPerformanceMode;
  Key: Variant;
  KeySense: TsmxDataSense;
begin
  if not Assigned(DeleteDataSet)
      or not ((FRecID <> 0) or ((CfgID <> 0) and (FIntfID <> 0))) then
    raise EsmxCfgError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, CfgID, 'remove'], Self);
  //Request := FSelectRequest.DeleteDataSet; // ModifyRequests[mrDelete];
  //Performance := FSelectRequest.DeletePerformance; // ModifyPerformances[mrDelete];
  {if not Assigned(Request) then
    raise EsmxCellError.CreateByComponent(@smxConsts.rsCfgActionError,
      [ClassName, FCfgID, 'remove'], FCfgID);}
  if FRecID = 0 then
  begin
    Key := Variants.VarArrayOf([CfgID, FIntfID]);
    KeySense := dsForeignKey;
  end else
  begin
    Key := FRecID;
    KeySense := dsKey;
  end;
  if smxDBFuncs.SetValueByKey(DeleteDataSet, Key, Variants.Null, KeySense) then
    FRecID := 0;
end;

procedure TsmxStateCfg.SetIntfID(Value: Integer);
begin
  FIntfID := Value;
end;

procedure TsmxStateCfg.SetRecID(Value: Integer);
begin
  FRecID := Value;
end;

initialization
  Classes.RegisterClasses([TsmxTypeCfg, TsmxCellCfg, TsmxStateCfg]);

end.
