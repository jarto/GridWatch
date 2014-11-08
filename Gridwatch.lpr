program Gridwatch;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, simulform
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Gridwatch green energy simulation';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TGridWatchSimulationForm, GridWatchSimulationForm);
  Application.Run;
end.

