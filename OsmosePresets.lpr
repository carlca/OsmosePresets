program osmosepresets;

{$MODE OBJFPC}{$H+}
{$DEFINE DEBUG}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  caMidi,caMidiIntf,caMidiTypes,Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms,
  uMainForm,
  uData,
  uSettingsForm,
  uSettingsData;

begin
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh
  // Set up -gh output for the Leakview package:
  //UseHeapTrace := False;
  if FileExists('Heaptrc.log') then
    DeleteFile('Heaptrc.log');
  SetHeapTraceOutput('Heaptrc.log');
  {$ENDIF DEBUG}
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
