unit upoints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubuilding, SDL_IMAGE, umayor;

const
  NUMBER_OF_BUILDINGS = 6;
  INITIAL_MONEY = 10000;

  {$IFDEF UNIX}
  IMG_PATH = 'img/';
  FONT_PATH = 'fonts/';
  {$ENDIF}
  {$IFDEF Win32}
  IMG_PATH = 'img\';
  FONT_PATH = 'fonts\';
  {$ENDIF}

  {$IFDEF UNIX}
  INTERFACE_PATH = IMG_PATH + 'interface/';

  {$ENDIF}

  {$IFDEF Win32}
  INTERFACE_PATH = IMG_PATH + 'interface\';
  {$ENDIF}

  FACTORY_PATH = IMG_PATH + 'factory.png';
  HOME_PATH = IMG_PATH + 'home.png';
  FARM_PATH = IMG_PATH + 'farm.png';
  ENERGY_PATH = IMG_PATH + 'energy.png';
  PARK_PATH = IMG_PATH + 'tree.png';
  FINANCE_PATH = IMG_PATH + 'bank.png';

  LVL_UP_PATH = INTERFACE_PATH + 'lvlup.png';

  HOME_EN = -1;
  HOME_COST = -1;
  HOME_HOME = 10;
  HOME_PRICE = 100;

  FARM_F = 10;
  FARM_COST = -1;
  FARM_PRICE = 100;

  FACTORY_EN = -3;
  FACTORY_POL = 6;
  FACTORY_W = 30;
  FACTORY_COST = -3;
  FACTORY_PRICE = 1000;

  ENERGY_EN = 15;
  ENERGY_POL = 2;
  ENERGY_COST = -5;
  ENERGY_PRICE = 100;

  PARK_POL = -5;
  PARK_COST = -2;
  PARK_PRICE = 100;

  FINANCE_BASE_COST = 30;
  FINANCE_PRICE = 600;
  FINANCE_POP = 20;

type
  CResources = class
  protected
    tab: array [1..NUMBER_OF_BUILDINGS] of CBuilding;
    // 1: home; 2: farm; 3: factory; 4:energy; 5:park; 6:finance

    history: array [0..NUMBER_OF_BUILDINGS] of integer;
    // Stores the total paid/received from the buildings + index 0 => taxes

    population, money: integer;
    temp_popu: double;
    mayor: CMayor;

  public
    constructor init(amayor: CMayor);
    destructor destroy; override;
    procedure reset;

    function getbuild(i: integer): CBuilding;
    function gethistory(i: integer): integer;

    function getmoney(): integer; //Returns how much money the user has
    function getpopulation(): integer; //Returns the number of citizens
    procedure addmoney(sum: integer); //Adds (or removes) a certain sum of money
    function growth_rate():double; //Updates the intrinsic rate of increase (describing population's growth)
    procedure calcres;//Calculate the new ressources (more or less population and/or money)

    function getcost(): integer; //Maintenance cost + Taxes income
    function gettaxes(): integer; //Taxes income only
    function gethome(): integer;
    function getwork(): integer;
    function getenergy(): integer;
    function getfood(): integer; //Don't forget: Pollution makes farms less productive
    function getpollution(): integer;

    function getresslist(): string;

  end;

implementation

constructor CResources.init(amayor: CMayor);
var
  i: integer;
begin
  tab[1] := CBuilding.init('Flats', 'Home sweet home', IMG_LOAD(HOME_PATH),
    HOME_EN, 0, 0, 0, HOME_HOME, HOME_COST, HOME_PRICE);
  tab[2] := CBuilding.init('Farm', 'Yet another small farm',
    IMG_LOAD(FARM_PATH), 0, 0, 0, FARM_F, 0, FARM_COST, FARM_PRICE);
  tab[3] := CBuilding.init('Factory', 'Biggest source of wealth (and smog) on earth !',
    IMG_LOAD(FACTORY_PATH), FACTORY_EN, FACTORY_W, FACTORY_POL, 0,
    0, FACTORY_COST, FACTORY_PRICE);
  tab[4] := CBuilding.init('Wind farm', 'It''s all about E=MC^2',
    IMG_LOAD(ENERGY_PATH), ENERGY_EN, 0, ENERGY_POL, 0, 0, ENERGY_COST, ENERGY_PRICE);
  tab[5] := CBuilding.init('Park', 'Let''s take a walk, in the park',
    IMG_LOAD(PARK_PATH), 0, 0, PARK_POL, 0, 0, PARK_COST, PARK_PRICE);
  tab[6] := CBuilding.init('Stock market',
    'Just don''t ask where all that money come from !', IMG_LOAD(FINANCE_PATH),
    0, 0, 0, 0, 0, FINANCE_BASE_COST, FINANCE_PRICE, FINANCE_POP);

  for i := 1 to NUMBER_OF_BUILDINGS do
    history[i] := 0;

  money := INITIAL_MONEY;
  population := 0;
  temp_popu := 0;
  mayor := amayor;
end;

procedure CResources.reset;
var
  i: integer;
begin
  money := INITIAL_MONEY;
  population := 0;
  for i := 1 to NUMBER_OF_BUILDINGS do
  begin
    history[i] := 0;
    tab[i].reset;
  end;
end;

function CResources.getbuild(i: integer): CBuilding;
begin
  if (i >= 1) and (i <= NUMBER_OF_BUILDINGS) then
    Result := self.tab[i]
  else
    Result := nil;
end;

function CResources.gethistory(i: integer): integer;
begin
  Result := history[i];
end;

function CResources.getmoney(): integer;
begin
  Result := money;
end;

function CResources.getpopulation(): integer;
begin
  Result := population;
end;


procedure CResources.addmoney(sum: integer);
begin
  self.money := self.money + sum;
end;

function CResources.growth_rate():double;
var
  factor, r:double;
begin
  //We modelize the "intrinsic rate of increase" as follows:

  //First, depending on the size of the population, we try to get a modest factor for the growth speed
  if (population <= 5) then
    factor := 0.35
  else if (population<=50) then
    factor := 0.005
  else
    factor := 0.0005;

  //Then we calculate the "intrinsic rate of increase" in function of the popllution (environmental effect)
  r := factor * (1 - getpollution() / 50);

  if r > factor then
    r := factor;
  //Note that r can be negative, and the population decrease, if the pollution is too high

  Result:=r;
end;

procedure CResources.calcres();
var
  min, i: integer;
begin

  // --- WEALTH ---

  //First, we add the benefits/cost of our buildings and the taxes to the city's budget
  self.addmoney(getcost() + gettaxes());

  //Then we "archive" the sum of costs/benefits (to be used in the game "Summary")
  for i := 1 to NUMBER_OF_BUILDINGS do
    history[i] := history[i] + tab[i].getCost() * tab[i].getNumber();
  history[0] := history[0] + gettaxes();

  // --- POPULATIONS GROWTH ---
  //For a person to set-up in the city, all criterias must be satisfied
  //So we calculate the less satisfied factor
  min := getwork();
  if min > getfood() then
    min := getfood();
  if min > gethome() then
    min := gethome();
  //And make the population move progressively to this number, by using some populations dynamics theories

  //First, in the case of an empty city, we add 1 citizen (to launch the exponential growth):
  if (population = 0) and (min <> 0) then
  begin
    population := 1;
    temp_popu := population;
  end;

  //In a basic populations dynamics model, we have: d(N)/N = N*r
  //N: population's size and r: intrisic rate of increase
  if (temp_popu < min) then
    temp_popu := temp_popu + sqr(population) * growth_rate();

  //Finally, we put this as the new population's size (with some tests)
  if (temp_popu <= min) and (temp_popu >= 0) then
    population := round(temp_popu)
  else if (temp_popu) > min then
    population := min
  else
    population := 0;

end;

function CResources.getcost(): integer;
var
  i, res: integer;
begin
  res := 0;
  //We update the banks cost (depending on the current mayor's lvl)
  tab[NUMBER_OF_BUILDINGS].setCost(mayor.getlvl() * FINANCE_BASE_COST);

  //We add-up the profit/cost of each building
  for i := 1 to NUMBER_OF_BUILDINGS do
    res := res + tab[i].getCost() * tab[i].getNumber();

  Result := res;
end;

function CResources.gettaxes(): integer;
begin
  Result := population * (1 + (mayor.getlvl() div 2));
end;

function CResources.gethome(): integer;
var
  i, res: integer;
begin
  res := 0;
  for i := 1 to NUMBER_OF_BUILDINGS do
    res := res + tab[i].getHome() * tab[i].getNumber();
  Result := res;
end;

function CResources.getwork(): integer;
var
  i, res: integer;
begin
  res := 0;
  for i := 1 to NUMBER_OF_BUILDINGS do
    res := res + tab[i].getWork() * tab[i].getNumber();
  Result := res;
end;

function CResources.getfood(): integer;
var
  i, res: integer;
  pollution_factor: double;
begin
  res := 0;
  if (getpollution() > 0) then
    pollution_factor := 1 - (getpollution() / 100)
  else
    pollution_factor := 1;

  for i := 1 to NUMBER_OF_BUILDINGS do
    res := res + round(tab[i].getFood() * tab[i].getNumber() * pollution_factor);
  Result := res;
end;

function CResources.getpollution(): integer;
var
  i, res: integer;
begin
  res := 0;
  for i := 1 to NUMBER_OF_BUILDINGS do
    res := res + tab[i].getPollution() * tab[i].getNumber();
  Result := res;
end;

function CResources.getenergy(): integer;
var
  i, res: integer;
begin
  res := 0;
  for i := 1 to NUMBER_OF_BUILDINGS do
    res := res + tab[i].getEnergy() * tab[i].getNumber();
  Result := res;
end;

function CResources.getresslist(): string;
begin
  Result := 'POPULATION :' + floattostr(population) + '    MONEY : $' +
    floattostr(money) + ' (' + floattostr(getcost() + gettaxes()) +
    ')     HOMES : ' + floattostr(gethome()) + '    WORK: ' +
    floattostr(getwork()) + '    FOOD: ' + floattostr(getfood()) +
    '    ENERGY: ' + floattostr(getenergy()) + '    POLLUTION: ' +
    floattostr(getpollution());
end;

destructor CResources.destroy;
var
  i:integer;
begin
  for i:=1 to NUMBER_OF_BUILDINGS do
    CBuilding(tab[i]).free;
  mayor.save;
  mayor.free;
end;

end.

