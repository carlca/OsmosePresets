program Search_Replace;

uses
  SysUtils, Classes, StrUtils;

const
  InputFilePath = '/Users/carlcaulkett/Code/FPC/Osmose Presets/OsmoseCategories.pas';
  OutputFilePath = '/Users/carlcaulkett/Code/FPC/Osmose Presets/OsmoseCategories OUT.pas';

var
  InputFileStream: TFileStream;
  OutputFileStream: TFileStream;
  StringStream: TStringStream;
  Content: string;
  LinesIn: TStringList;
  i: Integer;
  j: Integer;
  Sub: string;
  SubOut: string;
  Commas: Integer;

procedure Add(var S: string; Added: string);
begin
  S := S + Added;
end;

begin
  try
    // Read input file into TStringStream
    InputFileStream := TFileStream.Create(InputFilePath, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(InputFileStream, 0);  // Copy entire file
        Content := StringStream.DataString;

        // Manipulate content
        LinesIn := TStringList.Create;
        try
          LinesIn.Text := Content;

          for i := 0 to LinesIn.Count - 1 do
          begin
            Sub := LinesIn[i];
            SubOut := '';
            Commas := 0;

            if Sub[1] = ',' then
            begin
              SubOut := Sub;
            end
            else
            begin
              //  In: 30,1,acid bass,aggressive + analog + distorted + stereo
              // Out: 30, 1, 'acid bass', 'aggressive + analog + distorted + stereo'
              for j := 1 to Length(Sub) do
              begin
                SubOut := SubOut + Sub[j];
                if Sub[j] = ',' then
                begin
                  Inc(Commas);
                  case Commas of
                    1:  Add(SubOut, ' ');
                    2:  Add(SubOut, ' "');
                    3:  SubOut := Copy(SubOut, 1, Length(SubOut) - 1) + '", "'
                    else
                      ;
                  end;
                end;
              end;
              SubOut += '"';
            end;

            LinesIn[i] := SubOut;
            WriteLn(SubOut);
          end;
          Content := LinesIn.Text;
        finally
          LinesIn.Free;
        end;

        // Write manipulated content to output file
        OutputFileStream := TFileStream.Create(OutputFilePath, fmCreate);
        try
          StringStream.Position := 0;
          StringStream.WriteString(Content);
          OutputFileStream.CopyFrom(StringStream, 0);

          WriteLn('File processing completed successfully.');
        finally
          OutputFileStream.Free;
        end;
      finally
        StringStream.Free;
      end;
    finally
      InputFileStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('An error occurred: ', E.Message);
    end;
  end;
end.
