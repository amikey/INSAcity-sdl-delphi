unit ubuilding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL;

type

  CElement = class
  protected
    Name: string;
    message: string;
    image: pSDL_Surface;
  public
    constructor init(fname, fmessage: string; fimage: pSDL_Surface);
    destructor Destroy; override;
    function getimg(): pSDL_Surface;
    function getname(): string;
    function getmessage(): string;
  end;

  CBuilding = class(CElement)
  protected
    ptw, //work *
    ptfood, //food  *
    pthome, //housing *
    pten, //energy
    ptpol, //pollution (>0) or "ecology" (<0)
    price, //The construction cost of the building
    ptcost, //maintenance cost (<0) or wealth produced (>0)
    ptpop, //only used for banks that needs a certain number of people in the city
    // One point of each field marked (*) is required for a new citizen to settle in the city
    nb: integer; //number of buildings built

  public
    constructor init(fname, fmessage: string; fimage: pSDL_Surface;
      en, w, pol, food, home, cost, aprice: integer; pop: integer = 0);

    procedure add; //Adds a building (+1 to nb)
    procedure remove; //Removes a building (-1 to nb)
    procedure reset;

    //The following functions are getters for the buildings' stats
    function getEnergy(): integer;
    function getWork(): integer;
    function getHome(): integer;
    function getCost(): integer;
    function getPrice(): integer;
    function getPollution(): integer;
    function getFood(): integer;
    function getPop(): integer;
    // Gives the number of buildings that are built
    function getNumber(): integer;
    //Returns a summary of the buildings' stats (in a string)
    function getStats(): string;

    //Mainly used for "banks", where the revenues depend on the mayor's lvl
    procedure setCost(acost: integer);

  end;


implementation

// CElement methods (inherited by buildings also)

constructor CElement.init(fname, fmessage: string; fimage: pSDL_Surface);
begin
  Name := fname;
  image := fimage;
  message := fmessage;
end;

destructor CElement.Destroy;
begin
  inherited;
  SDL_FreeSurface(image);
end;

function CElement.getimg: pSDL_Surface;
begin
  Result := image;
end;

function CElement.getname(): string;
begin
  Result := Name;
end;

function CElement.getmessage(): string;
begin
  Result := message;
end;

// BUILDINGS methods

constructor CBuilding.init(fname, fmessage: string; fimage: pSDL_Surface;
  en, w, pol, food, home, cost, aprice: integer; pop: integer = 0);
begin
  inherited init(fname, fmessage, fimage);
  pten := en;
  ptw := w;
  ptpol := pol;
  ptfood := food;
  pthome := home;
  ptcost := cost;
  ptpop := pop;

  price := aprice;
  nb := 0;
end;

procedure CBuilding.add;
begin
  nb := nb + 1;
end;

procedure CBuilding.remove;
begin
  nb := nb - 1;
end;

procedure CBuilding.reset;
begin
  nb := 0;
end;

function CBuilding.getEnergy(): integer;
begin
  Result := pten;
end;

function CBuilding.getWork(): integer;
begin
  Result := ptw;
end;

procedure CBuilding.setCost(acost: integer);
begin
  ptcost := acost;
end;

function CBuilding.getCost(): integer;
begin
  Result := ptcost;
end;

function CBuilding.getPollution(): integer;
begin
  Result := ptpol;
end;

function CBuilding.getFood(): integer;
begin
  Result := ptfood;
end;

function CBuilding.getPrice(): integer;
begin
  Result := price;
end;

function CBuilding.getNumber(): integer;
begin
  Result := nb;
end;

function CBuilding.getHome(): integer;
begin
  Result := pthome;
end;

function CBuilding.getPop(): integer;
begin
  Result := ptpop;
end;

function CBuilding.getStats(): string;
var
  str: string;
begin
  str := '';
  if getCost < 0 then
    str := str + 'Costs ' + floattostr(getCost) + '$';
  if getCost > 0 then
    str := str + 'Produces ' + floattostr(getCost) + '$';

  if getHome <> 0 then
    str := str + '    +' + floattostr(getHome) + ' Beds';
  if getFood <> 0 then
    str := str + '    +' + floattostr(getFood) + ' Food';
  if getWork <> 0 then
    str := str + '    +' + floattostr(getWork) + ' Jobs';
  if getEnergy > 0 then
    str := str + '    +' + floattostr(getEnergy) + ' Energy';
  if getEnergy < 0 then
    str := str + '    ' + floattostr(getEnergy) + ' Energy';

  if getPollution > 0 then
    str := str + '    +' + floattostr(getPollution) + ' Pollution';
  if getPollution < 0 then
    str := str + '    ' + floattostr(getPollution) + ' Pollution';

  Result := str;
end;

end.

