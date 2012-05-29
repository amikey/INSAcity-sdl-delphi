unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ugame;

type

  { TForm1 }

  TForm1 = class(TForm)
    LabelTreeDensity: TLabel;
    LabelMountainDensity: TLabel;
    NewGameButton: TButton;
    NameForm: TLabeledEdit;
    MenuBG: TImage;
    MountainDensityBar: TTrackBar;
    TreeDensityBar: TTrackBar;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  game: CGame;

implementation

{$R *.lfm}

{ TForm1 }



end.

