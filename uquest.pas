unit uquest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, upoints, SDL, SDL_IMAGE, unotif, umayor;

const
  MONEY_ICON = INTERFACE_PATH + 'money.png';
  PEOPLE_ICON = INTERFACE_PATH + 'people.png';
  BUILDINGS_ICON = INTERFACE_PATH + 'buildings.png';

type
  CQuest = class
  protected
    Name, description: string;
    badge: pSDL_Surface;
    xp: integer;
    done: boolean;
  public
    constructor Create(aname, adescription: string; abadge: pSDL_Surface; axp: integer);
    procedure check(ressources: CResources); virtual; abstract;
    function is_done(): boolean;
    function getname(): string;
    function getdescription(): string;
    function getbadge(): pSDL_Surface;
    function getxp(): integer;
  end;

  CMoneyQuest = class(CQuest)
  protected
    money: integer;
  public
    constructor Create(aname, adescription: string; abadge: pSDL_Surface;
      axp: integer; amoney: integer); overload;
    procedure check(ressources: CResources); override;
  end;

  CPopuQuest = class(CQuest)
  protected
    population: integer;
  public
    constructor Create(aname, adescription: string; abadge: pSDL_Surface;
      axp: integer; apopu: integer); overload;
    procedure check(ressources: CResources); override;
  end;

  CBuildingQuest = class(CQuest)
  protected
    index, num: integer;
  public
    constructor Create(aname, adescription: string; abadge: pSDL_Surface;
      axp: integer; aindex, anum: integer); overload;
    procedure check(ressources: CResources); override;
  end;

  CQuestList = class
  protected
    quests: TList;
    queue: CNotifQueue;
    ressources: CResources;
    mayor: CMayor;
    moneyicon, peopleicon, buildicon: pSDL_Surface;
  public
    constructor Create(aressources: CResources; amayor: CMayor; aqueue: CNotifQueue);
    destructor Destroy; override;
    procedure draw_queue;
  end;

implementation

//MAIN CLASS IMPLEMENTATION

constructor CQuest.Create(aname, adescription: string; abadge: pSDL_Surface;
  axp: integer);
begin
  Name := aname;
  description := adescription;
  badge := abadge;
  xp := axp;
  done := False;
end;

function CQuest.is_done(): boolean;
begin
  Result := done;
end;

function CQuest.getname(): string;
begin
  Result := Name;
end;

function CQuest.getdescription(): string;
begin
  Result := description;
end;

function CQuest.getbadge(): pSDL_Surface;
begin
  Result := badge;
end;

function CQuest.getxp(): integer;
begin
  Result := xp;
end;

//CHILD:MONEYQUEST IMPLEMENTATION

constructor CMoneyQuest.Create(aname, adescription: string;
  abadge: pSDL_Surface; axp: integer; amoney: integer);
begin
  inherited Create(aname, adescription, abadge, axp);
  money := amoney;
end;

procedure CMoneyQuest.check(ressources: CResources);
begin
  if (ressources.getmoney() >= money) then
    done := True;
end;


//CHILD:POPUQUEST IMPLEMENTATION

constructor CPopuQuest.Create(aname, adescription: string; abadge: pSDL_Surface;
  axp: integer; apopu: integer);
begin
  inherited Create(aname, adescription, abadge, axp);
  population := apopu;
end;

procedure CPopuQuest.check(ressources: CResources);
begin
  if (ressources.getpopulation() >= population) then
    done := True;
end;

//CHILD:BUILDINGQUEST IMPLEMENTATION

constructor CBuildingQuest.Create(aname, adescription: string;
  abadge: pSDL_Surface; axp: integer; aindex, anum: integer);
begin
  inherited Create(aname, adescription, abadge, axp);
  index := aindex;
  num := anum;
end;

procedure CBuildingQuest.check(ressources: CResources);
begin
  if (ressources.getbuild(index).getNumber() >= num) then
    done := True;
end;

//QUESTLIST IMPLEMENTATION

procedure CQuestList.draw_queue;
var
  i: integer;
begin
  for i := 0 to (quests.Count - 1) do
    if not (CQuest(quests[i]).is_done()) then
    begin
      CQuest(quests[i]).check(ressources);
      if CQuest(quests[i]).is_done() then
      begin
        mayor.addxp(CQuest(quests[i]).getxp());
        //Sends a notification (with infos about the achieved quest)
        queue.add(CQuest(quests[i]).getname(), CQuest(quests[i]).getdescription(),
          CQuest(quests[i]).getbadge(), NOTIF_LONG_DURATION);
      end;
    end;
end;

destructor CQuestList.Destroy;
var
  i: integer;
begin
  for i := 0 to (quests.Count - 1) do
    CQuest(quests[i]).Destroy;
  SDL_FreeSurface(moneyicon);
  SDL_FreeSurface(peopleicon);
  SDL_FreeSurface(buildicon);
end;


constructor CQuestList.Create(aressources: CResources; amayor: CMayor;
  aqueue: CNotifQueue);
begin
  queue := aqueue;
  ressources := aressources;
  mayor := amayor;

  moneyicon := IMG_LOAD(MONEY_ICON);
  peopleicon := IMG_LOAD(PEOPLE_ICON);
  buildicon := IMG_LOAD(BUILDINGS_ICON);
  SDL_SetAlpha(moneyicon, SDL_SRCALPHA or SDL_RLEACCEL, 80);
  SDL_SetAlpha(peopleicon, SDL_SRCALPHA or SDL_RLEACCEL, 80);
  SDL_SetAlpha(buildicon, SDL_SRCALPHA or SDL_RLEACCEL, 80);

  quests := TList.Create;
  quests.capacity := 50;

  quests.Add(CMoneyQuest.Create('Good start', 'Get more than $11,000',
    moneyicon, 50, 11000));
  quests.Add(CMoneyQuest.Create('Get money !', 'Get more than $15,000',
    moneyicon, 80, 15000));
  quests.Add(CMoneyQuest.Create('Millionaire in the making',
    'Get more than $20,000', moneyicon, 120, 20000));
  quests.Add(CMoneyQuest.Create('Rockfeller 2.0',
    'Get more than $25,000', moneyicon, 220, 25000));
  quests.Add(CMoneyQuest.Create('Bajillionaire',
    'Get more than $50,000', moneyicon, 480, 50000));

  quests.Add(CPopuQuest.Create('Happy few', 'Welcome your first 10 citizens !',
    peopleicon, 50, 10));
  quests.Add(CPopuQuest.Create('More of ''em coming', 'Get 20 citizens in your city',
    peopleicon, 80, 20));
  quests.Add(CPopuQuest.Create('The answer to life, the universe and everything',
    'Get 42 citizens', peopleicon, 100, 42));
  quests.Add(CPopuQuest.Create('Woodstock', 'Get +100 citizens',
    peopleicon, 220, 100));
  quests.Add(CPopuQuest.Create('I SEE PEOPLE EVERYWHERE', 'Get +200 citizens',
    peopleicon, 420, 200));
  quests.Add(CPopuQuest.Create('Like a BOSS', 'Get +1337 citizens',
    peopleicon, 2000, 1337));

  quests.Add(CBuildingQuest.Create('Home sweet home ...',
    'Build your first residential building',
    buildicon, 40, 1, 1));
  quests.Add(CBuildingQuest.Create('Central park', 'Build your first park',
    buildicon, 40, 5, 1));
  quests.Add(CBuildingQuest.Create('A table !', 'Build 3 farms',
    buildicon, 60, 2, 3));
  quests.Add(CBuildingQuest.Create('Stock borker', 'Build your first stock market',
     buildicon, 200, 6, 1));
  quests.Add(CBuildingQuest.Create('Industrial era', 'Build 4 factories',
    buildicon, 300, 3, 4));
  quests.Add(CBuildingQuest.Create('Meuuuuuh', 'Build 8 farms',
    buildicon, 350, 2, 8));

end;

end.

