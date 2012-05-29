unit unotif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL, SDL_IMAGE, utimer, utools, utext, umisc;

const
  MAX_NOTIFS = 20;

  NOTIF_W = 259;
  NOTIF_H = 125;
  NOTIF_ALPHA = 240;
  NOTIF_SHORT_DURATION = 700;
  NOTIF_MEDIUM_DURATION = 1500;
  NOTIF_LONG_DURATION = 4000;
  NOTIF_FONT_SIZE = 14;
  NOTIF_TITLE_MARGIN = 8;

type
  CNotif = class
  protected
    title, message: string; //The title and message to show
    badge: pSDL_Surface; // Badge/icon to show
    lifespan: integer; // Duration of the notification

    startTime: integer; // The time where the notification was called
    timer: CTimer; // The program's timer
    bg: pSDL_Surface; // The notification widget background
    notiftext: CText; // Font style for notification message
  public
    constructor Create(aTimer: CTimer; atitle, amessage: string;
      aicon, abg: pSDL_Surface; atext: CText;
      aduration: integer = NOTIF_MEDIUM_DURATION);
    procedure init;
    function is_alive(): boolean;
    procedure draw();
  end;

  CNotifQueue = class
  protected
    queue: TList;
    timer: CTimer;
    bg: pSDL_Surface;
    textstyle: CText;
  public
    constructor Create(atimer: CTimer);
    destructor Destroy; override;
    procedure add(title, Text: string; badge: pSDL_Surface;
      duration: integer = NOTIF_MEDIUM_DURATION);
    procedure refresh;
    procedure draw;
  end;


implementation

constructor CNotif.Create(aTimer: CTimer; atitle, amessage: string;
  aicon, abg: pSDL_Surface; atext: CText; aduration: integer = NOTIF_MEDIUM_DURATION);
begin
  timer := atimer;
  title := atitle;
  message := amessage;
  badge := aicon;
  lifespan := aduration;
  notiftext := atext;
  bg := abg;
end;

procedure CNotif.init;
begin
  startTime := timer.getTicks();
end;

function CNotif.is_alive(): boolean;
begin
  if (timer.getTicks() - startTime < lifespan) and (timer.getTicks() > lifespan) then
    Result := True
  else
    Result := False;
end;

procedure CNotif.draw();
var
  screen, surface: pSDL_Surface;
  // The var "surface" contains all the notif. widget components: background, badge, text, ...
  y: integer;
  crop: pSDL_Rect;
  delta_t, currheight: integer;
begin

  screen := SDL_GetVideoSurface();
  surface := SDL_CreateRGBSurface(SDL_SWSURFACE, NOTIF_W, NOTIF_H,
    BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);

  SDL_FillRect(surface, nil, SDL_MapRGBA(surface^.format, 50, 50, 50,
    SDL_ALPHA_TRANSPARENT));


  fastBlit(bg, surface, 0, 0);

  notiftext.DrawTo(title, surface, NOTIF_TITLE_MARGIN, NOTIF_TITLE_MARGIN);

  if (badge <> nil) then
    fastBlit(badge, surface, (NOTIF_W - TILE_SIZE) div 2,
      ((NOTIF_H - TILE_SIZE) div 2) + 2);

  notiftext.DrawTo(message, surface, NOTIF_TITLE_MARGIN, NOTIF_H -
    3 * NOTIF_TITLE_MARGIN);

  delta_t := (timer.getTicks() - startTime);
  if ((delta_t div 3) < NOTIF_H) then
    currheight := (delta_t div 3)
  else
    currheight := NOTIF_H;

  y := screen^.h - BOTTOM_HEIGHT - NEWS_HEIGHT - currheight;

  NEW(crop);
  crop^.x := 0;
  crop^.y := 0;
  crop^.w := NOTIF_W;
  crop^.h := currheight;

  fadeInOut(surface, startTime, timer.getTicks(), lifespan, NOTIF_ALPHA);
  fastBlit(surface, screen, screen^.w - NOTIF_W, y, crop, NOTIF_H, NOTIF_W);

  SDL_FreeSurface(surface);
  DISPOSE(crop);
end;

constructor CNotifQueue.Create(atimer: CTimer);
begin
  timer := atimer;
  queue := TList.Create;
  queue.Capacity := MAX_NOTIFS;
  bg := IMG_LOAD(NOTIF_BG);
  textstyle := CText.Create(ANIVERS_FONT, NOTIF_FONT_SIZE, 220, 220, 220);
end;

destructor CNotifQueue.Destroy;
var
  i: integer;
begin
  for i := 0 to (queue.Count - 1) do
    CNotif(queue[i]).Free;
  queue.Free;
  SDL_FreeSurface(bg);
  textstyle.Destroy;
end;

procedure CNotifQueue.add(title, Text: string; badge: pSDL_Surface;
  duration: integer = NOTIF_MEDIUM_DURATION);
begin
  queue.Add(CNotif.Create(timer, title, Text, badge, bg, textstyle, duration));
  if queue.Count = 1 then
    CNotif(queue[0]).init;
end;

procedure CNotifQueue.refresh;
begin
  if (queue.Count > 0) and not (CNotif(queue[0]).is_alive()) then
  begin
    queue.Delete(0);
    if (queue.Count > 0) then
      CNotif(queue[0]).init;
  end;
end;

procedure CNotifQueue.draw;
begin
  refresh;
  if (queue.Count <> 0) then
    CNotif(queue[0]).draw;
end;

end.

