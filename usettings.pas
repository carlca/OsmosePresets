unit uSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls, PairSplitter, StdCtrls, SysUtils, Forms, Controls, Graphics,
  Dialogs,
  // project units
  uSettingsRec,
  // ca units
  caMidi, caMidiIntf, caMidiTypes, caDbg;

type
  TSettingsForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    DeviceList: TListBox;
    ButtonPanel: TPanel;
    TopPanel: TPanel;
    procedure DeviceListSelectionChange(Sender: TObject; User: boolean);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSettings: TSettings;
    procedure RealignButtons;
  public
    property Settings: TSettings read FSettings write FSettings;
  end;

implementation

{$R *.lfm}

procedure TSettingsForm.FormShow(Sender: TObject);
var
  Index: integer;
begin
  Midi.GetDevices(ioOut, DeviceList.Items);
  Index := DeviceList.Items.IndexOf(FSettings.DeviceName);
  if Index >= 0 then
    DeviceList.Selected[Index] := True;
end;

procedure TSettingsForm.DeviceListSelectionChange(Sender: TObject; User: boolean);
begin
  FSettings.DeviceName := DeviceList.GetSelectedText;
end;

procedure TSettingsForm.FormResize(Sender: TObject);
begin
  RealignButtons;
end;

procedure TSettingsForm.RealignButtons;
var
  Centre: integer;
begin
  Centre := ButtonPanel.Width div 2;
  OKButton.Left := Centre - OKButton.Width - 2;
  CancelButton.Left := Centre + 2;
end;

end.
