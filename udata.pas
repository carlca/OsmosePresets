unit uData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, LResources, Math;

const
  ALL_CATEGORIES = 'all';

type

  { TPreset }

  TPreset = class
  private
    FCC0: integer;
    FPGM: integer;
    FCategory: string;
    FCharacters: string;
    FCharacterList: TStrings;
    FPresetName: string;
    procedure UpdatePresetCharacterList;
    procedure SetCharacters(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    function MatchesCharacter(SearchCharacter: String): Boolean;
    property CC0: integer read FCC0 write FCC0;
    property PGM: integer read FPGM write FPGM;
    property Category: string read FCategory write FCategory;
    property Characters: String read FCharacters write SetCharacters;
    property CharacterList: TStrings read FCharacterList;
    property PresetName: string read FPresetName write FPresetName;
  end;

  PPreset = ^TPreset;

  { TCategory }

  TCategory = class
  private
    FName: string;
  public
    constructor Create(AName: string);
    property Name: string read FName write FName;
  end;

  PCategory = ^TCategory;

  { TPresetList }

  TPresetList = class(TFPObjectList)
  public
    procedure Update(ALines: TStrings);
  end;

  { TSortablePresetList }

  TSortablePresetList = class(TPresetList)
  public
    procedure SortByPresetName;
  end;

  { TPresetData }

  TPresetData = class
  private
    // private members
    FCurrentCategory: string;
    FPresetsByCategory: TPresetList;
    FPresetsByPresetName: TSortablePresetList;
    Flines: TStrings;
    FSortedAllPresetNames: TStrings;
    FCategories: TFPObjectList;
    FCharacterList: TStrings;
    // private methods
    procedure CreateObjects;
    procedure FreeObjects;
    procedure BuildCategories;
    procedure SetCurrentCategory(AValue: string);
    procedure UpdateCharacterList;
  public
    // lifetime
    constructor Create;
    destructor Destroy; override;
    // public methods
    procedure LoadPresetList;
    procedure UpdateAllPresetsList;
    // public properties
    property Categories: TFPObjectList read FCategories;
    property CharacterList: TStrings read FCharacterList;
    property CurrentCategory: string read FCurrentCategory write SetCurrentCategory;
    property Lines: TStrings read Flines;
    property PresetsByCategory: TPresetList read FPresetsByCategory;
    property PresetsByPresetName: TSortablePresetList read FPresetsByPresetName;
  end;

implementation

{$R 'OsmosePresetsResources.res'}

{ TPreset }

constructor TPreset.Create;
begin
  inherited;
  FCharacterList := TStringList.Create;
  TStringList(FCharacterList).Duplicates := dupIgnore;
end;

destructor TPreset.Destroy;
begin
  FCharacterList.Free;
  inherited Destroy;
end;

function TPreset.MatchesCharacter(SearchCharacter: String): Boolean;
var
  Character: string;
begin
  for Character in FCharacterList do
    if Character = SearchCharacter then
      MatchesCharacter := True;
  MatchesCharacter := False;
end;

procedure TPreset.UpdatePresetCharacterList;
var
  CharacterArray: array of string;
  Index: Integer;
begin
  FCharacterList.Clear;
  CharacterArray := FCharacters.Split(' + ');
  for Index := 0 to High(CharacterArray) do
    if CharacterArray[Index] <> '' then
      FCharacterList.Add(CharacterArray[Index]);
end;

procedure TPreset.SetCharacters(AValue: String);
begin
  if FCharacters = AValue then Exit;
  FCharacters := AValue;
  UpdatePresetCharacterList;
end;

{$R 'OsmoseCharactersResources.res'}

{ TCategory }

constructor TCategory.Create(AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TPresetList }

// 30, 1, "acid bass", "aggressive + analog + distorted + stereo"

procedure TPresetList.Update(ALines: TStrings);
var
  Line: string;
  Parts: TStringArray;
  Preset: TPreset;
  Category: string;
begin
  Clear;
  for Line in ALines do
  begin
    if not Line.Contains(',') then
      Category := Line
    else
      begin
      if Line.Contains(',') then
      begin
        Parts := Line.Split(',');
        Preset := TPreset.Create;
        try
          Preset.CC0 := Parts[0].ToInteger;
          Preset.PGM := Parts[1].ToInteger;
          Preset.PresetName := Parts[2].Trim.DeQuotedString(#34).Trim;
          Preset.Category := Category;
          Preset.Characters := Parts[3].Trim.DeQuotedString(#34).Trim;
        finally
          Add(Preset);
        end;
      end;
    end;
  end;
end;

{ TSortablePresetList }

function Compare(Item1, Item2: Pointer): Integer;
var
  Preset1: TPreset absolute Item1;
  Preset2: TPreset absolute Item2;
begin
  if Preset1.PresetName < Preset2.PresetName then
    Result := -1
  else if Preset1.PresetName > Preset2.PresetName then
    Result := 1
  else
    Result := 0;
end;

procedure TSortablePresetList.SortByPresetName;
begin
  Sort(@Compare);
end;

{ TPresetData }

constructor TPresetData.Create;
begin
  inherited;
  FCurrentCategory := '';
  CreateObjects;
  LoadPresetList;
  FPresetsByCategory.Update(Flines);
  FPresetsByPresetName.Update(Flines);
  FPresetsByPresetName.SortByPresetName;
  BuildCategories;
  UpdateCharacterList;
end;

destructor TPresetData.Destroy;
begin
  FreeObjects;
  inherited;
end;

procedure TPresetData.CreateObjects;
begin
  FPresetsByCategory := TPresetList.Create(True);
  FPresetsByPresetName := TSortablePresetList.Create(True);
  FLines := TStringList.Create;
  FSortedAllPresetNames := TStringList.Create;
  FCategories := TFPObjectList.Create(True);
  FCharacterList := TStringList.Create;
  TStringList(FCharacterList).Sorted := True;
end;

procedure TPresetData.FreeObjects;
begin
  FPresetsByCategory.Free;
  FPresetsByPresetName.Free;
  Flines.Free;
  FSortedAllPresetNames.Free;
  FCategories.Free;
  FCharacterList.Free;
end;

procedure TPresetData.BuildCategories;
var
  Line: string;
  Category: TCategory;
begin
  FCategories.Clear;
  FCategories.Add(TCategory.Create(ALL_CATEGORIES));
  for Line in FLines do
  begin
    if not Line.Contains(',') then
      FCategories.Add(TCategory.Create(Line));
  end;
  if FCategories.Count > 0 then
    FCurrentCategory := TCategory(FCategories[0]).Name;
end;

procedure TPresetData.SetCurrentCategory(AValue: string);
begin
  if FCurrentCategory = AValue then Exit;
  FCurrentCategory := AValue;
  UpdateCharacterList;
end;

procedure TPresetData.UpdateCharacterList;
var
  Preset: TPreset;
  PresetIndex: Integer;
  Character: string;
  CharToAdd: string;
begin
  FCharacterList.Clear;
  for PresetIndex := 0 to Pred(FPresetsByCategory.Count) do
  begin
    Preset := TPreset(FPresetsByCategory[PresetIndex]);
    if (Preset.Category = FCurrentCategory) or (FCurrentCategory = ALL_CATEGORIES) then
    begin
      for Character in Preset.CharacterList do
      begin
        CharToAdd := specialize IfThen<string>(Character = 'UNASSIGNED', '_UNASSIGNED', Character);
        FCharacterList.Add(CharToAdd);
      end;
    end;
  end;
end;

procedure TPresetData.LoadPresetList;
{$ifdef windows}
const
  RT_RCDATA = PChar(10);
{$endif}
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(HInstance, 'OSMOSEPRESETSRESOURCES', RT_RCDATA);
  try
    FLines.LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;

procedure TPresetData.UpdateAllPresetsList;
var
  Line: string;
begin
  FSortedAllPresetNames.Clear;
  for Line in FLines do
    FSortedAllPresetNames.Add(Line);
end;

end.

