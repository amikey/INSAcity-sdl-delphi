unit usummary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, upoints, umayor, uprogressbar, SDL, SDL_IMAGE,
  utext, umisc, utimer, SDL_GFX;

const
  BPP = 32;
  TILE_SIZE = 84;
  SUM_BG_PATH = INTERFACE_PATH + 'dvsup.png';
  MONEY_ICON = INTERFACE_PATH + 'money.png';
  SUMMARY_PADDING = 80;
  SUMMARY_ALPHA = 230;
  SUMMARY_TEXTSIZE = 14;
  SUMMARY_TITLE_SIZE = 32;
  ICON_SIZE = 60;
  SUMMARY_LINE_HEIGHT = ICON_SIZE + 20;
  SUMMARY_TOP_PADDING = 100;
  SUMMARY_LEFT_PADDING = 250;
  SUMMARY_COLUMN = 320;
  SUMMARY_TOTAL_TEXTSIZE = 24;

type
  CSummary = class
  protected
    resources: CResources;
    mayor: CMayor;
    timer: CTimer;
    xpbar: CProgressBar; //A progress bar showing the XP of the mayor
    bg, pattern, moneyicon: pSDL_Surface;
    font: CText;
    active: boolean;
    start_time, currtime: integer;
  public
    constructor Create(aress: CResources; amayor: CMayor; afont: CText; atimer: CTimer);
    destructor Destroy; override;
    procedure toggle; //Toggles the "active" attribute between TRUE and FALSE.
    function is_enabled(): boolean;
    //Returns TRUE or false (whether the summary must be shown or not)
    procedure draw;
    // *Very* big procedure, to drow the summary (infos about the buildings, the mayor, ...)
  end;

implementation

constructor CSummary.Create(aress: CResources; amayor: CMayor;
  afont: CText; atimer: CTimer);
begin
  resources := aress;
  mayor := amayor;
  font := afont;
  timer := atimer;
  active := False;
  xpbar := CProgressBar.Create;

  pattern := IMG_LOAD(SUM_BG_PATH);
  moneyicon := IMG_LOAD(MONEY_ICON);

  bg := SDL_CreateRGBSurface(SDL_SWSURFACE, 0, 0, BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
end;

destructor CSummary.Destroy;
begin
  SDL_FreeSurface(bg);
  SDL_FreeSurface(pattern);
  SDL_FreeSurface(moneyicon);
  xpbar.Free;
end;

procedure CSummary.toggle;
begin
  if not (active) then
  begin
    active := True;
    start_time := Timer.getTicks();
    currtime := start_time;
  end
  else
    active := False;
end;

function CSummary.is_enabled(): boolean;
begin
  Result := active;
end;

procedure CSummary.draw;
var
  roto, sumwrap, screen: pSDL_Surface;
  sumw, sumh, i, x, y, left_padding, top_padding, xpbarw: integer;
  str: string;
begin
  screen := SDL_GetVideoSurface();
  sumw := screen^.w - 2 * SUMMARY_PADDING;
  sumh := screen^.h - 2 * SUMMARY_PADDING;

  if (active) and (sumw > 0) and (sumh > 0) then
  begin
    sumwrap := SDL_CreateRGBSurface(SDL_SWSURFACE, sumw, sumh, BPP,
      0, 0, 0, SDL_ALPHA_TRANSPARENT);

    if (bg^.w <> sumw) or (bg^.h <> sumh) then
    begin
      SDL_FreeSurface(bg);
      bg := SDL_CreateRGBSurface(SDL_SWSURFACE, sumw, sumh, BPP,
        0, 0, 0, SDL_ALPHA_TRANSPARENT);
      patternFill(bg, pattern);
    end;
    projectShadow(sumwrap, screen, SUMMARY_PADDING, SUMMARY_PADDING,
      4, SUMMARY_ALPHA - 100);
    fastBlit(bg, sumwrap, 0, 0);

    font.DrawTo('Summary', sumwrap, 20, 20, SUMMARY_TITLE_SIZE);

    //Depending on the screen resolution, we center all our elements:
    top_padding := (sumh - SUMMARY_LINE_HEIGHT * 4) div 2;
    left_padding := (sumw - SUMMARY_COLUMN * 2) div 2;
    y := top_padding;
    x := left_padding;

    //A very big loop to write each line: Icon - Name - Benefits (or cost);
    for i := 0 to NUMBER_OF_BUILDINGS do
    begin
      //We draw the icons
      if i = 0 then
        roto := rotozoomSurface(moneyicon, 0, ICON_SIZE / moneyicon^.w, 1)
      else
        roto := rotozoomSurface(resources.getbuild(i).getimg(), 0,
          ICON_SIZE / TILE_SIZE, 1);
      fastBlit(roto, sumwrap, x, y - (ICON_SIZE div 2));
      SDL_FreeSurface(roto);
      //Then, we write the buildings names (or taxes)
      if i = 0 then
        str := 'Taxes'
      else
        str := resources.getbuild(i).getname() + ' [' +
          floattostr(resources.getbuild(i).getNumber()) + ']';
      font.DrawTo(str, sumwrap, x + ICON_SIZE + 10, y, SUMMARY_TEXTSIZE);

      //And finally we write the total benefit/cost of this building (or taxes)
      if resources.gethistory(i) < 0 then
        font.DrawTo(floattostr(resources.gethistory(i)), sumwrap,
          x + SUMMARY_LEFT_PADDING, y, SUMMARY_TEXTSIZE, 220, 0, 0)
      else
        font.DrawTo('+' + floattostr(resources.gethistory(i)),
          sumwrap, x + SUMMARY_LEFT_PADDING, y, SUMMARY_TEXTSIZE, 0, 220, 0);
      y := y + SUMMARY_LINE_HEIGHT;
      if (i = 3) then
      begin
        x := x + SUMMARY_COLUMN;
        y := top_padding;
      end;
    end;

    //The current sum
    str := 'TOTAL : $' + floattostr(resources.getmoney());
    x := round((sumw - length(str) * SUMMARY_TOTAL_TEXTSIZE * 0.5) / 2);
    y := top_padding + 4 * SUMMARY_LINE_HEIGHT - SUMMARY_TOTAL_TEXTSIZE;
    font.DrawTo(str, sumwrap, x, y, SUMMARY_TOTAL_TEXTSIZE);

    //Mayor's name / LVL  / XP
    str := mayor.getname() + ' : LVL ' + floattostr(mayor.getlvl()) +
      ' (' + floattostr(mayor.getxp) + '/' + floattostr(mayor.getmaxxp) + ')';
    y := y + SUMMARY_TOTAL_TEXTSIZE + 10;
    x := round((sumw - length(str) * SUMMARY_TEXTSIZE * 0.5) / 2);
    font.DrawTo(str, sumwrap, x, y, SUMMARY_TEXTSIZE);

    //Mayor's XP progressbar
    xpbarw := (round(sumw * 0.8) div REMAINING_WIDTH) * REMAINING_WIDTH +
      LEFT_WIDTH + RIGHT_WIDTH;
    xpbar.draw(mayor.getxp(), mayor.getmaxxp(), xpbarw);
    fastBlit(xpbar.getbar(), sumwrap, (sumw - xpbarw) div 2, y + 50);


    fadeInOut(sumwrap, start_time, currtime, -1, SUMMARY_ALPHA);
    currtime := currtime + (1000 div 40); //A basic "timer"

    //And finally, we blit all this on the screen
    fastBlit(sumwrap, screen, SUMMARY_PADDING, SUMMARY_PADDING);

    SDL_FreeSurface(sumwrap);
  end;

end;

end.

