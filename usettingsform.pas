unit uSettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, ExtCtrls, StdCtrls, SysUtils, Forms, Controls,
  Graphics, Dialogs,
  // project units
  usettingsdata,
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
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FErrors: TStrings;
    FSettings: TSettingsData;
    procedure RealignButtons;
  public
    property Settings: TSettingsData read FSettings write FSettings;
  end;

implementation

{$R *.lfm}

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  Midi.GetDevices(ioOut, DeviceList.Items, FErrors);
  DeviceList.ItemIndex := FSettings.DeviceIndex;
end;

procedure TSettingsForm.DeviceListSelectionChange(Sender: TObject; User: boolean);
begin
  FSettings.DeviceName := DeviceList.GetSelectedText;
  FSettings.DeviceIndex := DeviceList.ItemIndex;
end;

procedure TSettingsForm.FormCreate(Sender:TObject);
begin
  FErrors := TStringList.Create;
end;

procedure TSettingsForm.FormDestroy(Sender:TObject);
begin
  FErrors.Free;
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
