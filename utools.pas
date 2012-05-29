unit utools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL, SDL_IMAGE, SDL_GFX, ubuilding, utext, upoints,
  utimer, umisc, umayor, usummary;

const

  //Initialization values for the screen
  SCREEN_DEFAULT_HEIGHT = 700;
  SCREEN_DEFAULT_WIDTH = 1024;
  BPP = 32;

  TOP_BAR = 20;
  MAP_LEFT = 0;

  TILE_SIZE = 84;

  BTN_SIZE = 60;
  BTN_MARGIN = 50;
  BTN_INNER_MARGIN = 4;

  INFOS_TEXT_BOTTOM = 65;
  INFOS_TEXT_LEFT = 15 + TILE_SIZE;
  INFOS_IMAGE_BOTTOM = -TILE_SIZE;
  INFOS_FONT_SIZE = 13;
  BUILDING_FONT_SIZE = 45;
  MESSAGE_FONT_SIZE = 24;
  MESSAGE_TEXT_BOTTOM = 30;

  MAX_MESSAGES = 50;
  MESSAGE_FILE = 'messages.txt';
  NEWS_FONT_SIZE = 12;
  NEWS_HEIGHT = 19;
  NEWS_SCROLL_SPEED = 12;
  NEWS_TOLERANCE = NEWS_SCROLL_SPEED * 2;
  NEWS_BOTTOM_OFFSET = 2;

  MENU_BTN_WIDTH = 70;

  NOTIF_BG = INTERFACE_PATH + 'notif_bg.png';

  BTN_IMG = INTERFACE_PATH + 'btn.png';
  BOTTOM_BG_PATH = INTERFACE_PATH + 'bottom_map.jpg';
  BOTTOM_INFO_BG_PATH = INTERFACE_PATH + 'bottom_info_map.jpg';
  NEWS_BG = INTERFACE_PATH + 'newspattern.jpg';
  NEWS_TITLE = INTERFACE_PATH + 'news_title.png';

  ANIVERS_FONT = FONT_PATH + 'anivers.ttf';
  TREBUCHET_FONT = FONT_PATH + 'trebuc.ttf';
  TREBUCHET_BOLD_FONT = FONT_PATH + 'trebucbd.ttf';

  BOTTOM_HEIGHT = 95;
  HINTS_FONT_SIZE = 15;
  HINTS_TOP = 10;
  HINTS_LEFT = 10;


type
  CTools = class
  protected
    bottomBox, topBox: pSDL_Surface; //Top and bottom frames
    btnBack, bottomBG, newsBG, newsTitle: pSDL_Surface;
    //Images used many times in the interface
    screen: pSDL_Surface; //Main screen, stored here for more conveniency
    ressources: CResources; //Game ressources (buildings, money, ...)
    mayor: CMayor; //The mayor in all his glory !
    trebuchet, trebuchet_bold, anivers: CText; //Fonts/Text style
    btnsActive, summaryActive: boolean;//True when this part of the interface is active
    newslist: TStringList;//A small "news" bar, just for fun :)
    summary: CSummary;
    timer: CTimer;//The SDL timer
    finalMessage: string;
    //The whole news concatanated in a string, stored as an attribute for more conveniency

  public
    constructor Create(aressources: CResources; amayor: CMayor; atimer: CTimer);
    destructor Destroy; override;
    procedure DrawTop; //Draws the top bar (money, population, ressources, time, ...)
    procedure DrawBottom; //Draws the bottom bar
    procedure GenerateInfos(building: CElement);
    //Generates and draws the bottom bar on "info mode"
    procedure DrawButtons; //Switch the bottom bar to "construction mode"
    function onbtn(x, y: integer): boolean; //True when the users clicks on a button
    function getBtn(x: integer): integer; //Gives the index of which button is clicked
    procedure GenerateNews;
    //Concatenate randomly the news and stores them in "finalMessage"
    procedure DrawNews; //Draws the news on the newsbar
    procedure DrawSummary; //Draws a summary of the city's ressources / mayor LVL/XP
    procedure onmenu(x, y: integer);
    function getAnivers(): CText;
    function getTrebuchet(): CText;
    function getTrebuchet_B(): CText;

  end;

implementation

constructor CTools.Create(aressources: CResources; amayor: CMayor; atimer: CTimer);
begin
  screen := SDL_GetVideoSurface();
  ressources := aressources;
  timer := atimer;
  mayor := amayor;
  newslist := TStringList.Create;
  newslist.LoadFromFile(FILES_PATH + MESSAGE_FILE);
  //loads the news from the .txt file containing them
  GenerateNews;

  //FONTS
  trebuchet := CText.Create(TREBUCHET_FONT);
  trebuchet_bold := CText.Create(TREBUCHET_BOLD_FONT);
  anivers := CText.Create(ANIVERS_FONT);

  //Summary
  summaryActive := False;
  summary := CSummary.Create(ressources, mayor, trebuchet, timer);


  topBox := SDL_CreateRGBSurface(SDL_SWSURFACE, SCREEN_DEFAULT_WIDTH,
    TOP_BAR, BPP, 0, 0, 0, 0);
  SDL_FillRect(topBox, nil, SDL_MapRGB(topBox^.format, 40, 40, 40));

  bottomBox := SDL_CreateRGBSurface(SDL_SWSURFACE, SCREEN_DEFAULT_WIDTH,
    BOTTOM_HEIGHT, BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
  bottomBG := IMG_LOAD(BOTTOM_BG_PATH);

  //Building interface (bottom pannel)
  bottomBG := IMG_LOAD(BOTTOM_BG_PATH);
  btnBack := IMG_LOAD(BTN_IMG);

  //"News" interface
  newsBG := IMG_LOAD(NEWS_BG);
  newsTitle := IMG_LOAD(NEWS_TITLE);

end;

destructor CTools.Destroy;
begin
  SDL_FreeSurface(topBox);
  SDL_FreeSurface(bottomBox);
  SDL_FreeSurface(bottomBG);
  SDL_FreeSurface(btnBack);
  SDL_FreeSurface(newsBG);
  SDL_FreeSurface(newsTitle);

  summary.Destroy;
  trebuchet.Free;
  trebuchet_bold.Free;
  anivers.Free;
  inherited;
end;

procedure CTools.DrawTop;
var
  x: integer;
begin
  if (topBox^.w <> screen^.w) then
  begin
    SDL_FreeSurface(topBox);
    topBox := SDL_CreateRGBSurface(SDL_SWSURFACE, screen^.w, TOP_BAR,
      BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
    SDL_FillRect(topBox, nil, SDL_MapRGB(topBox^.format, 40, 40, 40));
  end;
  SDL_BlitSurface(topBox, nil, screen, nil);
  x := round((screen^.w - Length(ressources.getresslist()) * 7.6) / 2);
  trebuchet.DrawTo(ressources.getresslist() + '    DATE :   ' +
    timer.Date(), screen, x, 0);
  trebuchet.DrawTo('SUMMARY', screen, screen^.w - MENU_BTN_WIDTH, 0);
end;

procedure CTools.DrawBottom;
begin
  fastBlit(bottomBox, screen, 0, screen^.h - BOTTOM_HEIGHT);
  //anivers.DrawTo('SPACE : Toggle Pause | SHIFT : Mouse scroll | ESCAPE : Clear | N / S : Start/Stop music | P : Toggle music | M : Toggle mute | A / Z : Change volume', screen, HINTS_LEFT, TOP_BAR + HINTS_TOP, HINTS_FONT_SIZE);
end;

procedure CTools.GenerateInfos(building: CElement);
begin
  if (screen^.w <> bottomBox^.w) then
  begin
    SDL_FreeSurface(bottomBox);
    bottomBox := SDL_CreateRGBSurface(SDL_SWSURFACE, screen^.w,
      BOTTOM_HEIGHT, BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
  end;

  //drawing the background ...
  fastBlit(bottomBG, bottomBox, 0, 0);

  if (building <> nil) then
  begin
    btnsActive := False;

    //blit image
    fastBlit(building.getimg(), bottomBox, 0, BOTTOM_HEIGHT + INFOS_IMAGE_BOTTOM);

    //blit buildings' name
    trebuchet_bold.DrawTo(building.getname(), bottomBox, INFOS_TEXT_LEFT,
      BOTTOM_HEIGHT - INFOS_FONT_SIZE - INFOS_TEXT_BOTTOM, BUILDING_FONT_SIZE);

    //blit buildings' stats
    trebuchet.DrawTo(CBuilding(building).getStats(), bottomBox, INFOS_TEXT_LEFT,
      BOTTOM_HEIGHT - INFOS_FONT_SIZE - 10, INFOS_FONT_SIZE, 200, 200, 200);

    //blit message
    anivers.DrawTo(building.getmessage(), bottomBox,
      INFOS_TEXT_LEFT + round(length(building.getname()) * 0.7 * BUILDING_FONT_SIZE),
      BOTTOM_HEIGHT - MESSAGE_FONT_SIZE - MESSAGE_TEXT_BOTTOM,
      MESSAGE_FONT_SIZE, 200, 200, 200);
  end
  else
    DrawButtons;
  //blit bottom
  fastBlit(bottomBox, screen, 0, screen^.h - BOTTOM_HEIGHT);
end;

procedure CTools.DrawButtons;
var
  i, x, y, leftmargin: integer;
  roto: pSDL_Surface;
begin
  btnsActive := True;
  roto := nil;
  leftmargin := (screen^.w - (BTN_SIZE + BTN_MARGIN) * NUMBER_OF_BUILDINGS +
    BTN_MARGIN) div 2;
  for i := 1 to NUMBER_OF_BUILDINGS do
  begin
    x := leftmargin + (i - 1) * (BTN_SIZE + BTN_MARGIN);
    y := (BOTTOM_HEIGHT - BTN_SIZE) div 2;
    fastBlit(btnBack, bottomBox, x, y);

    x := x + BTN_INNER_MARGIN;
    y := y + BTN_INNER_MARGIN;

    roto := rotozoomSurface(ressources.getbuild(i).getimg(), 0,
      (BTN_SIZE - 2 * BTN_INNER_MARGIN) / TILE_SIZE, 1);

    fastBlit(roto, bottomBox, x, y);
    //text under buttons
  end;
  SDL_FreeSurface(roto);
end;

procedure CTools.DrawNews;
var
  i, x, y, messageLength: integer;
begin
  x := 0;
  //Draws the news background
  for i := 0 to (screen^.w div newsBG^.w) do
  begin
    fastBlit(newsBG, screen, x, screen^.h - BOTTOM_HEIGHT - NEWS_HEIGHT);
    x := x + newsBG^.w;
  end;
  //Draws the text
  messageLength := round(length(finalMessage) * NEWS_FONT_SIZE * 0.6);
  y := screen^.h - BOTTOM_HEIGHT - NEWS_HEIGHT + NEWS_FONT_SIZE div
    2 - NEWS_BOTTOM_OFFSET;

  //When the game is paused
  if (timer.is_paused()) then
  begin
    anivers.DrawTo('Simulation paused', screen, screen^.w div 2, y, NEWS_FONT_SIZE);
  end
  else //When the game is not paused
  begin
    x := (screen^.w - ((timer.getTicks() div NEWS_SCROLL_SPEED))) mod messageLength;
    if (x >= screen^.w - NEWS_TOLERANCE) then
      GenerateNews;
    anivers.DrawTo(finalMessage, screen, x, y, NEWS_FONT_SIZE);
  end;

  //Draws the news "title" (the image at the left of the newsbar)
  fastBlit(newsTitle, screen, 0, screen^.h - BOTTOM_HEIGHT - NEWS_HEIGHT);

end;

procedure CTools.DrawSummary;
begin
  summary.draw;
end;

procedure CTools.GenerateNews;
var
  i, j: integer;
begin
  randomize;
  for i := 0 to newslist.Count - 1 do
  begin
    j := random(newslist.Count - i) + i;
    newslist.Exchange(i, j);
  end;
  finalMessage := 'INSACITY News';
  for i := 0 to newslist.Count - 1 do
    finalMessage := finalMessage + '  -  ' + newslist[i];
  finalMessage := finalMessage + ' - End of news. For now at least.';
end;

function CTools.onbtn(x, y: integer): boolean;
var
  x_is_over_9000: boolean;
  leftmargin: integer;
begin

  leftmargin := (screen^.w - (BTN_SIZE + BTN_MARGIN) * NUMBER_OF_BUILDINGS +
    BTN_MARGIN) div 2;

  x_is_over_9000 := x > (leftmargin + BTN_SIZE * NUMBER_OF_BUILDINGS +
    BTN_MARGIN * (NUMBER_OF_BUILDINGS - 1));
  x := (x - leftmargin) mod (BTN_SIZE + BTN_MARGIN);
  Result := ((btnsActive) and (x >= 0) and (x <= BTN_SIZE) and not
    (x_is_over_9000) and (y >= screen^.h - BOTTOM_HEIGHT +
    (BOTTOM_HEIGHT - BTN_SIZE) div 2) and (y <= screen^.h - BOTTOM_HEIGHT +
    (BOTTOM_HEIGHT - BTN_SIZE) div 2 + BTN_SIZE));
  //Seems messy and random, but it really isn't ! :)
end;

function CTools.getBtn(x: integer): integer;
var
  leftmargin: integer;
begin
  leftmargin := (screen^.w - (BTN_SIZE + BTN_MARGIN) * NUMBER_OF_BUILDINGS +
    BTN_MARGIN) div 2;
  Result := ((x - leftmargin) div (BTN_SIZE + BTN_MARGIN)) + 1;
end;

procedure CTools.onmenu(x, y: integer);
begin
  if summary.is_enabled() = True then
  begin
    timer.unpause;
    summary.toggle;
  end
  else if (y <= TOP_BAR) and (x > screen^.w - MENU_BTN_WIDTH) then
  begin
    summary.toggle;
    timer.pause();
  end;
end;

function CTools.getAnivers(): CText;
begin
  Result := anivers;
end;

function CTools.getTrebuchet(): CText;
begin
  Result := trebuchet;
end;

function CTools.getTrebuchet_B(): CText;
begin
  Result := trebuchet_bold;
end;

end.

