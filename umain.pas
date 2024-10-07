unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources,
  StdCtrls, LCLType, LCLIntf, FPImage, IntfGraphics, GraphType,
  ComCtrls, CheckLst, ExtCtrls, laz.VirtualTrees, ImgList, DbugIntf,
  // JSON units for settings
  FPJson, JsonParser,
  // project units
  uData, uSettings, uSettingsRec,
  // ca units
  caDbg, caMidi, caMidiIntf, caMidiTypes;

type

  { TMain }

  TMain = class(TForm)
    CharacterList: TCheckListBox;
    CharacterListPanel: TPanel;
    CharacterListHeader: TPanel;
    CharacterListHeaderLabel: TLabel;
    PresetTree: TLazVirtualStringTree;
    procedure CharacterListClickCheck(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PresetTreeAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure PresetTreeAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
    procedure PresetTreeCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
    procedure PresetTreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
    procedure PresetTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure PresetTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure PresetTreeKeyPress(Sender: TObject; var Key: char);
    procedure PresetTreeKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    FPresetData: TPresetData;
    FSettings: TSettings;
    function GetSettings: TSettings;
    procedure AssignKeyUpEvent(AControl: TWinControl);
    procedure ClearCharacterList;
    procedure GlobalKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure LoadCategoryNodes;
    procedure LoadCharacterList;
    procedure LoadPresetNodesByPresetName;
    procedure LoadPresetNodesByCategory;
  private
    // Terminate stuff
    FClosingForm: TForm;
  class var FIsTerminating: boolean;
  public
    // Terminate stuff
    function CloseQuery: boolean; override;
    class function IsTerminating: boolean;
    class procedure TerminateApplication;
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.FormCreate(Sender: TObject);
begin
  AssignKeyUpEvent(Self);
  FPresetData := TPresetData.Create;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FPresetData.Free;
end;

function TMain.GetSettings: TSettings;
var
  SettingsForm: TSettingsForm;
begin
  SettingsForm := TSettingsForm.Create(nil);
  try
    SettingsForm.Settings := FSettings;
    SettingsForm.ShowModal;
  finally
    Result := SettingsForm.Settings;
    Result.WriteJSON;
    SettingsForm.Free;
  end;
end;

procedure TMain.AssignKeyUpEvent(AControl: TWinControl);
var
  i: Integer;
  ChildControl: TControl;
begin
  AControl.OnKeyUp := @GlobalKeyUp;
  for i := 0 to AControl.ControlCount - 1 do
  begin
    ChildControl := AControl.Controls[i];
    if ChildControl is TWinControl then
      AssignKeyUpEvent(TWinControl(ChildControl))
    else if ChildControl is TControl then
      TWinControl(ChildControl).OnKeyUp := @GlobalKeyUp; // Change TControl(ChildControl) to TWinControl(ChildControl)
  end;
end;

procedure TMain.GlobalKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  {$IFDEF DARWIN}
  if Shift = [ssMeta] then
  begin
    case Key of
      VK_Q:           TerminateApplication;
      VK_OEM_COMMA:   FSettings := GetSettings;
    end;
  end;
  {$ENDIF}
  //{$IFDEF LINUX}
  //{$ENDIF}
  //{$IFDEF WINDOWS}
  //{$ENDIF}
end;

procedure TMain.FormShow(Sender: TObject);
begin
  LoadCategoryNodes;
  LoadPresetNodesByPresetName;
  LoadPresetNodesByCategory;
  CharacterList.Color := clMoneyGreen;
end;

procedure TMain.PresetTreeAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
var
  Level: cardinal;
  IsSelected: boolean;
  IsFocused: boolean;
  IsFocusedNode: boolean;
  IsExpanded: boolean;

  procedure DrawBackground(Canvas: TCanvas; Selected, Focused: boolean);
  begin
    if Focused or Selected then
    begin
      Canvas.Pen.Color := PresetTree.Colors.FocusedSelectionColor;
      Canvas.Brush.Color := PresetTree.Colors.FocusedSelectionColor;
    end
    else
    begin
      Canvas.Pen.Color := PresetTree.Colors.UnfocusedColor;
      Canvas.Brush.Color := PresetTree.Colors.UnfocusedColor;
    end;
    Canvas.FillRect(1, 1, 16, 16);
  end;

  procedure EraseCategoryOutline(TargetCanvas: TCanvas; Selected, Focused: boolean);
  begin
    if Focused or Selected then
    begin
      TargetCanvas.Pen.Color := PresetTree.Colors.FocusedSelectionColor;
      TargetCanvas.Frame(0, 0, 200, 18);
    end;
  end;

  procedure ErasePresetNameOutline(TargetCanvas: TCanvas; Selected, FocusedNode: boolean);
  begin
    if FocusedNode then
    begin
      TargetCanvas.Pen.Color := PresetTree.Colors.FocusedSelectionColor;
      TargetCanvas.Frame(0, 0, 200, 18);
    end;
  end;

  procedure DrawArrow(Canvas: TCanvas; Expanded, Selected, Focused: boolean);
  var
    Points: array[0..2] of TPoint;
    ArrowColor: TColor;
  begin
    // Set colors
    if Focused or Selected then
      ArrowColor := PresetTree.Colors.SelectionTextColor
    else
      ArrowColor := PresetTree.Colors.TreeLineColor;
    Canvas.Pen.Color := ArrowColor;
    Canvas.Brush.Color := ArrowColor;
    // Do the actual drawing
    if Expanded then
    begin
      // Down arrow
      Points[0] := Point(6, 5);
      Points[1] := Point(14, 5);
      Points[2] := Point(10, 11);
    end
    else
    begin
      // Right arrow
      Points[0] := Point(6, 4);
      Points[1] := Point(12, 8);
      Points[2] := Point(6, 12);
    end;
    Canvas.Polygon(Points);
  end;

begin
  Level := PresetTree.GetNodeLevel(Node);
  if Level = 0 then
  begin
    // Set state flags
    IsSelected := PresetTree.Selected[Node] and PresetTree.Focused;
    IsFocused := (Node = PresetTree.FocusedNode) and PresetTree.Focused;
    IsExpanded := PresetTree.Expanded[Node];
    // Call the drawing nested procedures
    DrawBackground(TargetCanvas, IsSelected, IsFocused);
    DrawArrow(TargetCanvas, IsExpanded, IsSelected, IsFocused);
    EraseCategoryOutline(TargetCanvas, IsSelected, IsFocused);
  end;
  if Level = 1 then
  begin
    IsSelected := PresetTree.Selected[Node] and PresetTree.Focused;
    IsFocusedNode := Node = PresetTree.FocusedNode;
    ErasePresetNameOutline(TargetCanvas, IsSelected, IsFocusedNode);
  end;
end;

procedure TMain.PresetTreeAfterPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin

end;

procedure TMain.PresetTreeCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
begin
  ClearCharacterList;
end;

procedure TMain.PresetTreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
var
  RootNode: PVirtualNode;
  Level: cardinal;
  CatPtr: PCategory;
begin
  Level := PresetTree.GetNodeLevel(Node);
  if Level = 0 then
  begin
    RootNode := PresetTree.GetFirst;
    while Assigned(RootNode) do
    begin
      if RootNode <> Node then
        PresetTree.FullCollapse(RootNode);
      RootNode := PresetTree.GetNextSibling(RootNode);
    end;
    CatPtr := PresetTree.GetNodeData(Node);
    FPresetData.CurrentCategory := CatPtr^.Name;
    LoadCharacterList;
  end;
  Allowed := True;
end;

procedure TMain.PresetTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(PPreset);
end;

procedure TMain.PresetTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  PrePtr: PPreset;
  CatPtr: PCategory;
  Level: cardinal;
begin
  Level := PresetTree.GetNodeLevel(Node);
  if Level = 0 then
  begin
    CatPtr := PresetTree.GetNodeData(Node);
    if Column = 0 then
      CellText := CatPtr^.Name
    else
      CellText := '';
  end
  else
  begin
    PrePtr := PresetTree.GetNodeData(Node);
    case Column of
      0: CellText := PrePtr^.PresetName;
      1: CellText := PrePtr^.CC0.ToString;
      2: CellText := PrePtr^.PGM.ToString;
      3: CellText := PrePtr^.Characters;
      4: CellText := '';
    end;
  end;
end;

procedure TMain.PresetTreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin

  end;
end;

procedure TMain.CharacterListClickCheck(Sender: TObject);
begin
end;

procedure TMain.FormActivate(Sender: TObject);
begin
  ActiveControl := PresetTree;
  PresetTree.SetFocus;
  PresetTree.Selected[PresetTree.GetFirst] := True;
  PresetTree.TreeOptions.PaintOptions := PresetTree.TreeOptions.PaintOptions - [toShowHorzGridLines, toShowVertGridLines];
  //PresetTree.Colors.FocusedSelectionColor := clNone;
  //PresetTree.Colors.FocusedSelectionBorderColor := clNone;
  //PresetTree.FocusedColumn := 0;
end;

procedure TMain.PresetTreeKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //db(Key);
  //db(Shift);
end;

procedure TMain.ClearCharacterList;
begin
  CharacterList.Items.Clear;
end;

procedure TMain.LoadCategoryNodes;
var
  Index: Integer;
begin
  for Index := 0 to Pred(FPresetData.Categories.Count) do
    PresetTree.AddChild(nil, FPresetData.Categories[Index]);
end;

procedure TMain.LoadCharacterList;
begin
  CharacterList.Items := FPresetData.CharacterList;
  CharacterList.CheckAll(cbChecked);
end;

procedure TMain.LoadPresetNodesByPresetName;
var
  Preset: TPreset;
  PNode: PVirtualNode;
  CatPtr: PCategory;
  PresetIndex: Integer;
begin
  PNode := PresetTree.GetFirst;
  CatPtr := PresetTree.GetNodeData(PNode);
  if CatPtr^.Name = ALL_CATEGORIES then
  begin
    for PresetIndex := 0 to Pred(FPresetData.PresetsByPresetName.Count) do
    begin
      Preset := TPreset(FPresetData.PresetsByPresetName[PresetIndex]);
      PresetTree.AddChild(PNode, Preset);
    end;
  end;
end;

procedure TMain.LoadPresetNodesByCategory;
var
  Preset: TPreset;
  PNode: PVirtualNode;
  CatPtr: PCategory;
  PresetIndex: Integer;
begin
  PNode := PresetTree.GetFirst;
  if Assigned(PNode) then
  begin
    for PresetIndex := 0 to Pred(FPresetData.PresetsByCategory.Count) do
    begin
      Preset := TPreset(FPresetData.PresetsByCategory[PresetIndex]);
      CatPtr := PresetTree.GetNodeData(PNode);
      if Preset.Category <> CatPtr^.Name then
        PNode := PresetTree.GetNextSibling(PNode);
      PresetTree.AddChild(PNode, Preset);
    end;
  end;
end;

function TMain.CloseQuery: boolean;
begin
  if not Main.IsTerminating then
  begin
    Main.TerminateApplication;
    Result := False;  // Prevent the default close behavior
  end
  else
    Result := True;  // Allow the form to close if we're already terminating
end;

class function TMain.IsTerminating: boolean;
begin
  Result := FIsTerminating;
end;

class procedure TMain.TerminateApplication;
var
  ClosingForm: TForm;
  ClosingLabel: TLabel;
begin
  if not FIsTerminating then
  begin
    FIsTerminating := True;

    // Create a small form to display the closing message
    ClosingForm := TForm.Create(nil);
    try
      ClosingForm.BorderStyle := bsNone;
      ClosingForm.FormStyle := fsStayOnTop;
      ClosingForm.Position := poScreenCenter;
      ClosingForm.Width := 200;
      ClosingForm.Height := 50;
      ClosingForm.Color := clInfoBk;

      ClosingLabel := TLabel.Create(ClosingForm);
      ClosingLabel.Parent := ClosingForm;
      ClosingLabel.Align := alClient;
      ClosingLabel.Alignment := taCenter;
      ClosingLabel.Layout := tlCenter;
      ClosingLabel.Caption := 'Application is closing...';

      ClosingForm.Show;
      Application.ProcessMessages; // Ensure the form is displayed

      // Store the form reference
      Main.FClosingForm := ClosingForm;
    except
      ClosingForm.Free; // Free the form if an exception occurs
      raise;
    end;

    Application.Terminate;
  end;
end;

initialization
  TMain.FIsTerminating := False;

finalization
  if Assigned(Main.FClosingForm) then
    Main.FClosingForm.Free;

end.
