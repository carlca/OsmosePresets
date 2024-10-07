unit uSettingsRec;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, LCLIntf, LazFileUtils,
  // JSON units for settings
  FPJson, JsonParser;

type
  TSettings = record
    DeviceName: string;
    function GetAppConfigPath: string;
    procedure WriteJSON;
  end;

implementation

function TSettings.GetAppConfigPath: string;
var
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(GetUserDir) + '.config' + PathDelim + 'OsmosePresets';
  ForceDirectories(Path);
  Result := Path;
end;

procedure TSettings.WriteJSON;
var
  JSONObj: TJSONObject;
  SL: TStrings;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add('Device Name', DeviceName);
    SL := TStringList.Create;
    try
      SL.Text := JSONObj.FormatJSON;
      SL.SaveToFile(GetAppConfigPath + PathDelim + 'settings.json');
    finally
      SL.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

end.

