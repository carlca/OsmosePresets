unit uSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls,StdCtrls,SysUtils, Forms, Controls, Graphics, Dialogs,
  caMidi, caMidiIntf, caMidiTypes, uSettingsRec;

type
  TSettingsForm = class(TForm)
    DeviceList:TListBox;
    TopPanel: TPanel;
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
  private
    FSettings: TSettings;
  public
    property Settings: TSettings read FSettings write FSettings;
  end;

implementation

{$R *.lfm}

procedure TSettingsForm.FormCreate(Sender:TObject);
var
  Index: integer;
begin
  Midi.GetDevices(ioOut, DeviceList.Items);
  Index := DeviceList.Items.IndexOf(FSettings.DeviceName);
  if Index >= 0 then
    DeviceList.Selected[Index] := True;
end;

procedure TSettingsForm.FormDestroy(Sender:TObject);
begin
  FSettings.DeviceName := DeviceList.GetSelectedText;
end;

end.

