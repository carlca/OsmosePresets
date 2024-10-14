unit uSettingsRec;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, LCLIntf, LazFileUtils,
  // JSON units for settings
  FPJson, JsonParser,
  // ca stuff
  caDbg;

const
  SETTINGS_JSON = 'settings.json';

type
  TSettings = record
  private
    FDeviceName: string;
    // private methods
    function GetAppConfigPath: string;
    function GetConfigFileName: string;
  public
    property DeviceName: string read FDeviceName write FDeviceName;
    procedure Read;
    procedure Save;
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

function TSettings.GetConfigFileName:string;
begin
  Result := GetAppConfigPath + PathDelim + SETTINGS_JSON;
end;

procedure TSettings.Read;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
begin
  JSONData := GetJSON(TFileStream.Create(GetConfigFileName, fmOpenRead));
  try
    if JSONData is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONData);
      Self.DeviceName := JSONObj.Get('DeviceName', '');
      db('Self.DeviceName', Self.DeviceName);
    end
    else
      raise Exception.Create('Invalid JSON format');
  finally
    JSONData.Free;
  end;
end;

procedure TSettings.Save;
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
      db(SL.Text);
      SL.SaveToFile(GetConfigFileName);
    finally
      SL.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

end.

