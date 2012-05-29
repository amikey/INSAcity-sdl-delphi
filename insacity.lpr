program insacity;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, ugame
  { you can add units after this };

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.ShowModal;
  Application.ProcessMessages;

  Game := CGame.Create(Form1.NameForm.text, Form1.TreeDensityBar.Position, Form1.MountainDensityBar.Position );
  while not (Game.handle_events()) do
    Game.Refresh;
  Game.Free;
end.

