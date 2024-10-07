program osmosepresets;

{$mode objfpc}{$H+}

{$DEFINE debug}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, uMain, uData, uSettings, usettingsrec;

//{$R *.res}

const HeaptrcPath = '/Users/carlcaulkett/Code/FPC/OsmosePresets/heaptrc.txt';

begin
  {$IFDEF DEBUG}
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists(HeaptrcPath) then
    DeleteFile(HeaptrcPath);
  SetHeapTraceOutput(HeaptrcPath);
  {$ENDIF DEBUG}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

