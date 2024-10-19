unit uMainForm;

{$MODE OBJFPC}{$H+}

interface

uses
  Buttons, caEdit, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources,
  StdCtrls, LCLType, LCLIntf, FPImage, IntfGraphics, GraphType, ComCtrls, CheckLst,
  ExtCtrls, laz.VirtualTrees, ImgList, DbugIntf,
  // JSON units for settings
  FPJson, JsonParser,
  // project units
  uData, uSettingsForm, uSettingsData,
  // ca units
  caDbg, caUtils, caMidi, caMidiIntf, caMidiTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    SearchEdit: TcaEdit;
    CharacterList: TCheckListBox;
    CharacterListPanel: TPanel;
    CharacterListHeader: TPanel;
    CharacterListHeaderLabel: TLabel;
    SpacerLabel: TLabel;
    OptionsButton: TSpeedButton;
    SearchPanel: TPanel;
    ToolbarPanel: TPanel;
    PresetTree: TLazVirtualStringTree;
    procedure caEditChange(Sender:TObject);
    procedure CharacterListClickCheck(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure PresetTreeAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure PresetTreeCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
    procedure PresetTreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
    procedure PresetTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PresetTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: integer);
    procedure PresetTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure PresetTreeKeyPress(Sender: TObject; var Key: char);
    procedure PresetTreeKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    FPresetData: TPresetData;
    FSettings: TSettingsData;
    FReturnPressed: boolean;
    procedure ClearCharacterList;
    procedure EnterKeyPressed(var Key: char);
    procedure GlobalKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure LoadCategoryNodes;
    procedure LoadCharacterList;
    procedure LoadPresetNodesByPresetName;
    procedure LoadPresetNodesByCategory;
    procedure ShowSettings;
    procedure ToggleSearchPanel;
  private
  public
  end;

const
  ENTER_KEY = #13;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSettings.Read;
  PresetTree.OnKeyUp := @GlobalKeyUp;
  CharacterList.OnKeyUp := @GlobalKeyUp;
  FPresetData := TPresetData.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FPresetData) then
    FreeAndNil(FPresetData);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  //Caption := 'Osmose Presets - ' + Width.ToString;
end;

procedure TMainForm.ShowSettings;
var
  SettingsForm: TSettingsForm;
begin
  SettingsForm := TSettingsForm.Create(nil);
  try
    SettingsForm.Settings := FSettings;
    SettingsForm.ShowModal;
    if SettingsForm.ModalResult = mrOk then
    begin
      FSettings := SettingsForm.Settings;
      FSettings.Save;
    end;
  finally
    SettingsForm.Free;
  end;
end;

procedure TMainForm.GlobalKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  {$IFDEF DARWIN}
  if Shift = [ssMeta] then
  begin
    case Key of
      VK_Q:           Close;
      VK_OEM_COMMA:   ShowSettings;
    end;
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  {$ENDIF}
  {$IFDEF WINDOWS}
  {$ENDIF}
  // all platforms
  if Shift = [] then
  begin
    case key of
      VK_S:           ToggleSearchPanel;
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  {$IFDEF DARWIN}
  ToolbarPanel.Visible := False;
  {$ENDIF}
  LoadCategoryNodes;
  LoadPresetNodesByPresetName;
  LoadPresetNodesByCategory;
  CharacterList.Color := PresetTree.Color;
  CharacterListHeaderLabel.Color := clBtnFace;
  {$IFDEF DARWIN}
  Caption := 'OsmosePresets 0.2.0 alpha - for macOS';
  {$ENDIF}
  {$IFDEF LINUX}
  Caption := 'OsmosePresets 0.2.0 alpha - for Linux';
  {$ENDIF}
  {$IFDEF WINDOWS}
  Caption := 'OsmosePresets 0.2.0 alpha - for Windows';
  {$ENDIF}
  PresetTree.Header.Height := 19;
end;

procedure TMainForm.OptionsButtonClick(Sender: TObject);
begin
  ShowSettings;
end;

procedure TMainForm.PresetTreeAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
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
      {$IFDEF DARWIN}
      Canvas.Pen.Color := PresetTree.Colors.UnfocusedColor;
      Canvas.Brush.Color := PresetTree.Colors.UnfocusedColor;
      {$ENDIF}
      {$IFDEF WINDOWS}
      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
      {$ENDIF}
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

procedure TMainForm.PresetTreeCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
begin
  ClearCharacterList;
end;

procedure TMainForm.PresetTreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: boolean);
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

procedure TMainForm.PresetTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  PrePtr: PPreset;
  CatPtr: PCategory;
  Level: cardinal;
begin
  Level := PresetTree.GetNodeLevel(Node);
  if Level = 0 then
  begin
    CatPtr := PresetTree.GetNodeData(Node);
    if Assigned(CatPtr^) then
      CatPtr^.Free;
  end
  else
  begin
    PrePtr := PresetTree.GetNodeData(Node);
    if Assigned(PrePtr^) then
      PrePtr^.Free;
  end;
end;

procedure TMainForm.PresetTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: integer);
begin
  NodeDataSize := SizeOf(PPreset);
end;

procedure TMainForm.PresetTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
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

procedure TMainForm.PresetTreeKeyPress(Sender: TObject; var Key: char);
begin
  {$IFDEF DARWIN}
  if FReturnPressed then
  begin
    FReturnPressed := False;
    Exit;
  end;
  {$ENDIF}
  FReturnPressed := True;
end;

procedure TMainForm.ToggleSearchPanel;
begin
  SearchPanel.Visible := not SearchPanel.Visible;
  SpacerLabel.Visible := SearchPanel.Visible;
end;

procedure TMainForm.EnterKeyPressed(var Key: char);
var
  ExcCount: integer = 0;
  PrePtr: PPreset;
  Level: integer;
  Node: PVirtualNode;
begin
  Node := PresetTree.FocusedNode;
  if Assigned(Node) then
  begin
    Level := PresetTree.GetNodeLevel(Node);
    if Level = 1 then
    begin
      PrePtr := PresetTree.GetNodeData(Node);
      if not Midi.SendCC(FSettings.DeviceIndex, 0, PrePtr^.CC0) then
        Inc(ExcCount);
      Sleep(200);
      if not Midi.SendPGM(FSettings.DeviceIndex, 0, PrePtr^.PGM) then
        Inc(ExcCount);
      if ExcCount > 0 then
        Key := #0;
    end;
  end;
end;

procedure TMainForm.CharacterListClickCheck(Sender: TObject);
begin
end;

procedure TMainForm.caEditChange(Sender:TObject);
begin
  LoadPresetNodesByPresetName;
  LoadPresetNodesByCategory;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  ActiveControl := PresetTree;
  PresetTree.SetFocus;
  PresetTree.Selected[PresetTree.GetFirst] := True;
  PresetTree.TreeOptions.PaintOptions := PresetTree.TreeOptions.PaintOptions - [toShowHorzGridLines, toShowVertGridLines];
  //PresetTree.Colors.FocusedSelectionColor := clNone;
  //PresetTree.Colors.FocusedSelectionBorderColor := clNone;
  //PresetTree.FocusedColumn := 0;
end;

procedure TMainForm.PresetTreeKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //db(Key);
  //db(Shift);
end;

procedure TMainForm.ClearCharacterList;
begin
  CharacterList.Items.Clear;
end;

procedure TMainForm.LoadCategoryNodes;
var
  Index: integer;
begin
  for Index := 0 to Pred(FPresetData.Categories.Count) do
    PresetTree.AddChild(nil, FPresetData.Categories[Index]);
end;

procedure TMainForm.LoadCharacterList;
begin
  CharacterList.Items := FPresetData.CharacterList;
  CharacterList.CheckAll(cbChecked);
end;

procedure TMainForm.LoadPresetNodesByPresetName;
var
  Preset: TPreset;
  PNode: PVirtualNode;
  CatPtr: PCategory;
  PresetIndex: integer;
  ShouldAdd: boolean;
  SearchKey: string;
begin
  PNode := PresetTree.GetFirst;
  CatPtr := PresetTree.GetNodeData(PNode);
  if CatPtr^.Name = ALL_CATEGORIES then
  begin
    for PresetIndex := 0 to Pred(FPresetData.PresetsByPresetName.Count) do
    begin
      Preset := TPreset(FPresetData.PresetsByPresetName[PresetIndex]);
      //// TODO: Need to filter here
      //SearchKey := SearchEdit.AEdit.Text;
      //if SearchKey <> '' then
      //begin
      //  //if Preset.PresetName[1] = 'j' then
      //  //begin
      //  //  Pass;
      //  //end;
      //  ShouldAdd := Pos(SearchKey, Preset.PresetName) > 0;
      //end
      //else
      //  ShouldAdd := True;
      //if ShouldAdd then
        PresetTree.AddChild(PNode, Preset);
    end;
  end;
end;

procedure TMainForm.LoadPresetNodesByCategory;
var
  Preset: TPreset;
  PNode: PVirtualNode;
  CatPtr: PCategory;
  PresetIndex: integer;
  SearchKey: string;
  ShouldAdd: boolean;
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
//
//      SearchKey := SearchEdit.AEdit.Text;
//      if SearchKey <> '' then
//        ShouldAdd := Pos(SearchKey, Preset.PresetName) > 0
//      else
//        ShouldAdd := True;
//      if ShouldAdd then
        PresetTree.AddChild(PNode, Preset);
    end;
  end;
end;

end.
