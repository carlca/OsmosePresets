unit uSettingsData;

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
  DEVICE_NAME = 'Device Name';
  DEVICE_INDEX = 'Device Index';
  CONFIG_EXT = '.config';
  APP_NAME = 'OsmosePresets';

type
  TSettingsData = record
  private
    FDeviceIndex: integer;
    FDeviceName: string;
    // private methods
    function GetAppConfigPath: string;
    function GetConfigFileName: string;
  public
    property DeviceIndex: integer read FDeviceIndex write FDeviceIndex;
    property DeviceName: string read FDeviceName write FDeviceName;
    procedure Read;
    procedure Save;
  end;

implementation

procedure TSettingsData.Read;
var
  JSONStream: TFileStream;
  JSONData: TJSONData;
begin
  if not FileExists(GetConfigFileName) then
    Save;
  JSONStream := TFileStream.Create(GetConfigFileName, fmOpenRead or fmShareDenyWrite);
  try
    JSONData := GetJSON(JSONStream);
    try
      Self.DeviceName := JSONData.FindPath(DEVICE_NAME).AsString;
      Self.DeviceIndex := JSONData.FindPath(DEVICE_INDEX).AsInteger;
    finally
      JSONData.Free;
    end;
  finally
    JSONStream.Free;
  end;
end;

function TSettingsData.GetAppConfigPath: string;
var
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(GetUserDir) + CONFIG_EXT + PathDelim + APP_NAME;
  ForceDirectories(Path);
  Result := Path;
end;

function TSettingsData.GetConfigFileName:string;
begin
  Result := GetAppConfigPath + PathDelim + SETTINGS_JSON;
end;

procedure TSettingsData.Save;
var
  JSONObj: TJSONObject;
  SL: TStrings;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.Add(DEVICE_INDEX, Self.DeviceIndex);
    JSONObj.Add(DEVICE_NAME, Self.DeviceName);
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

