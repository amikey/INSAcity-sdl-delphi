unit ugame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  utools, umap,
  SDL, SDL_ttf, SDL_image, ubuilding, utimer, upoints, umusic, uquest,
  unotif, umayor, umisc;

const
  SCROLL_SPEED = 20;
  CAMERA_CROP = 50;
  FPS = 45;

  SELLING_FACTOR = 0.8;
  //Percentage of the price of a building you get when you sell it back

  DEFICIT_LIMIT = -1000;
//The max deficit in the city's budget before the player loses the game

type
  CGame = class
  protected
    // Some random boolean values ... (for volume, scrolling, ...)
    scrolling, lost, changingvol: boolean;
    // x/yoff and scrollx/y : map scrolling ; // buildIndex: index of the building to be built
    xoff, yoff, scrollx, scrolly, buildIndex: integer;
    // screen: this is the surface that we see on-screen // icon: the application's icon
    screen, icon: pSDL_Surface;
    // an object handling keyboard press and other events
    event: pSDL_Event;
    // the map (see umap)
    map: CMap;
    // most elements of the interface are stored in this object
    tools: CTools;
    // a timer implemented with SDL (and slightly modified)
    timer: CTimer;
    // the object storing our city's ressources
    resources: CResources;
    // music !
    music: CMusic;
    // the notifications queue, queues them and show them one after another
    notif_queue: CNotifQueue;
    // the mayor (and all his infos: XP, LVL, name, ...) and a LVL UP icon
    mayor: CMayor;
    lvlup_icon:pSDL_Surface;
    // the list of quests and "achievements" that can be done
    quests: CQuestList;

  public
    constructor Create(mayorname: string; tree_density, mountain_density : integer);
    destructor Destroy; override;
    //The MAIN procedure executed at each frame
    procedure refresh;
    //Scrolls the map with the desired x and y
    procedure scroll(x, y: integer);
    //Sets an offset to the map (used for scrolling)
    procedure setoff(x, y: integer);
    //Handles all the events (clicks, keyboard, etc.)
    function handle_events(): boolean;
    //Draws the main window (containing our game); Called when the windows is resized
    procedure DrawScreen(w, h: integer);
    //Builds a building (left click)
    procedure add(x, y: integer; building: CBuilding);
    //Removes a building (middle click)
    procedure remove(x, y: integer);
    //Make a translucent preview of the building's image follow the cursor
    procedure DrawBuildingCursor;
    //Sends a notification if the mayor levels up
    procedure mayor_notif;
    //Checks if everything is alright with the budget :) ends the game if the deficit is too high
    procedure CheckBudget;
    //Starts a new game, with a new map (but keeps the mayor's stats)
    procedure reset;
    function onResetBtn(x, y: integer): boolean;
  end;

implementation

constructor CGame.Create(mayorname: string; tree_density, mountain_density :integer);
begin
  SDL_Init(SDL_INIT_VIDEO); //Initialization of the video module
  TTF_INIT(); //Initialization of all the text related tools

  //We initialize the position of the screen on the center of the map
  setoff(XMAX * TILE_SIZE div 2, YMAX * TILE_SIZE div 2);

  //Initializaiton of the screen
  screen := SDL_SetVideoMode(SCREEN_DEFAULT_WIDTH, SCREEN_DEFAULT_HEIGHT,
    BPP, SDL_SWSURFACE or SDL_RESIZABLE);

  //This is required for repeated key press, for example when using directional keys to scroll on the map
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  if (screen <> nil) then
    //If everything went as planned, screen isn't supposed to be nil
  begin
    //Mayor
    mayor := CMayor.Create(mayorname);
    lvlup_icon := IMG_LOAD (LVL_UP_PATH);
    SDL_SetAlpha(lvlup_icon, SDL_SRCALPHA or SDL_RLEACCEL, 100);
    //Buildings, resources, ...
    resources := CResources.init(mayor);
    //Map
    map := CMap.init(tree_density, mountain_density);
    //The application's icon
    icon := IMG_LOAD(IMG_PATH + 'icon.png');
    SDL_WM_SetIcon(icon, nil);
    //Timer
    Timer := CTimer.Create;
    Timer.start();
    //Caption bar
    SDL_WM_SetCaption('INSAcity - VERSION BÊTA', nil);
    //This is required for repeated key press, for example
    //Events
    NEW(event);
    scrollx := 0;
    scrolly := 0;
    scrolling := False;
    changingvol := False;
    //Interface (CTools)
    tools := CTools.Create(resources, mayor, timer);
    tools.DrawTop;
    tools.GenerateInfos(nil);
    buildIndex := 0;
    //Music
    music := CMusic.Create;
    //Notifications
    notif_queue := CNotifQueue.Create(timer);
    //Quests
    quests := CQuestList.Create(resources, mayor, notif_queue);
    //We just started the game so ...
    lost := False;
  end;
end;

procedure CGame.refresh;
begin
  map.draw(xoff, yoff);

  //Draws the top bar (resources, population, ...)
  tools.DrawTop;
  //Draws the bottom panel (buildings info / construction panel)
  tools.DrawBottom;
  //Draws the news panel (random news, jokes, ...)
  tools.DrawNews;
  //If a quest is done, puts it in the notification queue
  quests.draw_queue;
  //If the mayor levels up, we send a notification
  mayor_notif;
  //Draw the queue's notifications (if any) one after another
  notif_queue.draw;
  //Draws a panel summarizing all the infos (if the user clicked on the "Summary" button)
  tools.DrawSummary;
  //Draws the image of the building to be built (on the cursor)
  DrawBuildingCursor;
  CheckBudget;

  //Music
  music.DrawVolumeBar;
  if (music.is_started()) then
    if not (music.is_playing()) then
      music.Next;

  SDL_Flip(screen); //Refreshes the screen

  //Limits the number of Frame Per Second (to avoid too much CPU use)
  Timer.wait(1000 / FPS);
  if (not (Timer.is_paused()) and (Timer.delay_passed())) then
    resources.calcres;
end;

procedure CGame.DrawBuildingCursor;
var
  x, y: integer;
  building_overlay: pSDL_Surface;
begin
  x := 0;
  y := 0;
  SDL_GetMouseState(x, y);
  x := x - (TILE_SIZE div 2);
  y := y - (TILE_SIZE div 2);

  if (buildindex <> 0) and (map.onmap(x, y)) then
  begin
    //Copy the buildings icon in a temporary surface
    building_overlay := SDL_ConvertSurface(
      resources.getbuild(buildindex).getimg(), resources.getbuild(
      buildindex).getimg()^.format, resources.getbuild(buildindex).getimg()^.flags);

    SDL_SetAlpha(building_overlay, SDL_SRCALPHA or SDL_RLEACCEL, 80);
    fastBlit(building_overlay, screen, x, y);
    SDL_FreeSurface(building_overlay);
  end;
end;

procedure CGame.mayor_notif;
begin
  if mayor.send_notif() then
    notif_queue.add('LEVEL UP !', 'You are now LVL '+floattostr(mayor.getlvl()), lvlup_icon, NOTIF_LONG_DURATION);
end;

function CGame.handle_events(): boolean;
var
  x, y: integer;
  keystate: pUInt8;
begin
  keystate := SDL_GetKeyState(nil);
  //shift key => mouse scroll
  scrolling := ((keystate[SDLK_RSHIFT] = 1) or (keystate[SDLK_LSHIFT] = 1));
  Result := False;
  while (SDL_POLLEVENT(event) = 1) do
  begin
    case (event^.type_) of
      SDL_VIDEORESIZE:
        DrawScreen(event^.resize.w, event^.resize.h);
      SDL_KEYUP:
        case (event^.key.keysym.sym) of
          SDLK_LEFT: scrollx := 0;
          SDLK_RIGHT: scrollx := 0;
          SDLK_UP: scrolly := 0;
          SDLK_DOWN: scrolly := 0;
        end;
      //key down
      SDL_KEYDOWN:
      begin
        //music events
        if (keystate[SDLK_N] = 1) then //n -> next song
          music.Next
        else if (keystate[SDLK_P] = 1) then //p -> pause/unpause music
          music.TogglePause
        else if (keystate[SDLK_S] = 1) then //s -> stop music
          music.Stop;

        if (keystate[SDLK_PLUS] = 1) or (keystate[SDLK_KP_PLUS] = 1) then //a -> up_volume
          music.up_volume
        else if (keystate[SDLK_MINUS] = 1) or (keystate[SDLK_KP_MINUS] = 1) then //z -> down_volume
          music.lower_volume
        else if (keystate[SDLK_M] = 1) then //m -> toggle mute
          music.ToggleMute;

        //Resetting the game/map
        if (keystate[SDLK_R] = 1) then
          reset;

        //Escape key
        if (keystate[SDLK_ESCAPE] = 1) then //escape -> clear toolbar
        begin
          buildindex := 0;
          tools.GenerateInfos(nil);
        end;

        //SPACE key (pause)
        if (keystate[SDLK_SPACE] = 1) then //space -> toggle pause
          timer.TogglePause;


        case (event^.key.keysym.sym) of
          //arrow scroll
          SDLK_LEFT: scrollx := -SCROLL_SPEED;
          SDLK_RIGHT: scrollx := SCROLL_SPEED;
          SDLK_UP: scrolly := -SCROLL_SPEED;
          SDLK_DOWN: scrolly := SCROLL_SPEED;
          //choosing buildings (with keypad)
          SDLK_KP1: buildindex := 1;
          SDLK_KP2: buildindex := 2;
          SDLK_KP3: buildindex := 3;
          SDLK_KP4: buildindex := 4;
          SDLK_KP5: buildindex := 5;
          SDLK_KP6: buildindex := 6;
          SDLK_KP7: map.setmapindex(1); //Normal map
          SDLK_KP8: map.setmapindex(2); //Polution map
          SDLK_KP9: map.setmapindex(3); //Population map
        end;

        //other quit event
        if (((keystate[SDLK_RCTRL] = 1) or (keystate[SDLK_LCTRL] = 1)) and
          (keystate[SDLK_Q] = 1)) then
          Result := True;
      end;
      //mouse down
      SDL_MOUSEBUTTONUP:
      begin
        changingvol := False;
      end;
      SDL_MOUSEBUTTONDOWN:
      begin
        x := event^.button.x;
        y := event^.button.y;

        //left click
        if (event^.button.button = SDL_BUTTON_LEFT) then
        begin
          if ((map.onmap(x, y)) and (buildIndex <> 0)) then
            add(xoff + x, yoff + y, resources.getbuild(buildIndex))
          else if (music.onVolumeBar(x,y)) then
          begin
            music.clickVolumeBar(x);
            changingvol := True;
          end
          else if (tools.onbtn(x, y)) then
            buildIndex := tools.getBtn(x)
          else if (lost) and (onResetBtn(x, y)) then
            reset;
          tools.onmenu(x, y);
        end
        //right click
        else if (event^.button.button = SDL_BUTTON_RIGHT) then
        begin
          if (map.onmap(x, y)) then
          begin
            tools.GenerateInfos(map.getBuilding(
              (x + xoff) div TILE_SIZE, (y + yoff) div TILE_SIZE));
            buildindex := 0;
          end;
        end
        //middle click
        else if (event^.button.button = SDL_BUTTON_MIDDLE) then
        begin
          if (map.onmap(x, y)) then
            remove(xoff + x, yoff + y);
        end
        //Scroll-wheel
        else if (event^.button.button = SDL_BUTTON_WHEELUP) then
        begin
          if (map.onmap(x, y)) then
          begin
            buildindex := buildindex + 1;
            if (buildindex > NUMBER_OF_BUILDINGS) then
              buildindex := 1;
          end
          else if (music.onVolumeBar(x,y)) then
          begin
            music.up_volume;
          end;
        end
        else if (event^.button.button = SDL_BUTTON_WHEELDOWN) then
        begin
          if (map.onmap(x, y)) then
          begin
            buildindex := buildindex - 1;
            if (buildindex < 1) then
              buildindex := 6;
          end
          else if (music.onVolumeBar(x,y)) then
          begin
            music.lower_volume;
          end;
        end;
      end;
      //quit
      SDL_QUITEV: Result := True;
      SDL_MOUSEMOTION:
      begin
        //volume bar
        x := event^.button.x;
        if (changingvol) then
          music.clickVolumeBar(x);
      end;
    end;
  end;
  //mouse scroll
  if ((scrolling) and ((event^.type_ <> SDL_ACTIVEEVENT) or
    (event^.active.gain = 1))) then
  begin
    SDL_GetMouseState(x, y);
    map.changebasis(x, y);
    if ((x >= 0) and (x <= CAMERA_CROP)) then
      scroll(-SCROLL_SPEED div 2, 0)
    else if ((x >= screen^.w - CAMERA_CROP) and (x <= screen^.w)) then
      scroll(SCROLL_SPEED div 2, 0);

    if ((y >= 0) and (y <= CAMERA_CROP)) then
      scroll(0, -SCROLL_SPEED div 2)
    else if ((y >= screen^.h - TOP_BAR - BOTTOM_HEIGHT - CAMERA_CROP) and
      (y <= screen^.h - TOP_BAR - BOTTOM_HEIGHT)) then
      scroll(0, SCROLL_SPEED div 2);
  end;
  scroll(scrollx, scrolly);
end;

procedure CGame.scroll(x, y: integer);
begin
  if (map.inmap(xoff + x, yoff + y)) then
    setoff(xoff + x, yoff + y);
end;

procedure CGame.setoff(x, y: integer);
begin
  xoff := x;
  yoff := y;
end;

procedure CGame.add(x, y: integer; building: CBuilding);
var
  can_afford, is_buildable, enough_energy, enough_pop: boolean;
  //Self-explenatory variables FTW \o/
begin
  map.changebasis(x, y);
  x := x div TILE_SIZE;
  y := y div TILE_SIZE;

  can_afford := (resources.getmoney() >= building.getCost());
  is_buildable := not (map.is_built(x, y));
  enough_energy := (resources.getenergy() + building.getEnergy() >= 0);
  enough_pop := (resources.getpopulation() >= building.getPop() *
    (building.getNumber() + 1));

  if can_afford and is_buildable and enough_energy and enough_pop then
  begin
    map.add(x, y, building);
    building.add;
    resources.addmoney(-building.getPrice());
    //notif_queue.add(building.getname() + ' built.', building.getStats(), resources.getbuild(buildIndex).getimg(), NOTIF_SHORT_DURATION);
  end;
end;

procedure CGame.remove(x, y: integer);
var
  building: CElement;
begin
  map.changebasis(x, y);
  x := x div TILE_SIZE;
  y := y div TILE_SIZE;
  building := map.getBuilding(x, y);
  if building <> nil then
  begin
    if (resources.getenergy() - CBuilding(building).getEnergy() >= 0) then
    begin
      resources.addmoney(round(SELLING_FACTOR * CBuilding(building).getPrice()));
      map.remove(x, y);
      tools.GenerateInfos(nil);
    end;
  end;
end;

procedure CGame.CheckBudget;
var
  blackbg: pSDL_Surface;
  w, h: integer;
begin
  if resources.getmoney() < DEFICIT_LIMIT then
  begin
    lost := True;
    buildindex:= 0;

    //Overlay the screen with a dark background
    blackbg := SDL_CreateRGBSurface(SDL_SWSURFACE, screen^.w, screen^.h,
      BPP, 0, 0, 0, 0);
    SDL_FillRect(blackbg, nil, SDL_MapRGB(blackbg^.format, 40, 40, 40));
    SDL_SetAlpha(blackbg, SDL_SRCALPHA or SDL_RLEACCEL, 220);
    fastBlit(blackbg, screen, 0, 0);
    SDl_FreeSurface(blackbg);


    //Write 'GAME OVER' at the center of the screen (size:60)
    w := (screen^.w div 2) - round(9 * 60 * 0.35);
    h := (screen^.h - BOTTOM_HEIGHT) div 2;
    Tools.getAnivers().DrawTo('GAME OVER', screen, w, h, 60);
    //Write 'Dare to restart?'
    w := (screen^.w div 2) - round(15 * 20 * 0.35);
    h := ((screen^.h + TOP_BAR - BOTTOM_HEIGHT) div 2) + 50;
    Tools.getAnivers().DrawTo('Dare to restart?', screen, w, h, 20);

  end;
end;

procedure CGame.reset;
begin
  map.Free;
  map := CMap.init;
  resources.reset;
  timer.start;

  lost := False;
end;

function CGame.onResetBtn(x, y: integer): boolean;
var
  w, h: integer;
begin
  Result := False;

  w := (screen^.w div 2) - round(9 * 60 * 0.35);
  h := (screen^.h - BOTTOM_HEIGHT) div 2;
  if (x >= w) and (x <= w + round(9 * 60 * 0.35 + 10)) and (y >= h) and
    (y <= h + 150) then
    Result := True;
end;

procedure CGame.DrawScreen(w, h: integer);
begin
  screen := SDL_SetVideoMode(w, h, BPP, SDL_SWSURFACE or SDL_RESIZABLE);
  //bug fix for resizing when at borders
  if not (map.inmap(xoff, 0)) then
    xoff := XMAX * TILE_SIZE - screen^.w;
  if not (map.inmap(0, yoff)) then
    yoff := YMAX * TILE_SIZE - screen^.h + TOP_BAR + BOTTOM_HEIGHT + NEWS_HEIGHT;

  tools.GenerateInfos(nil);
end;

destructor CGame.Destroy;
begin
  Timer.Destroy;
  tools.Destroy;
  map.Destroy;
  music.Destroy;
  notif_queue.Destroy;
  quests.Destroy;
  resources.destroy;

  SDL_FreeSurface(screen);
  SDL_FreeSurface(icon);
  SDL_FreeSurface(lvlup_icon);
  DISPOSE(event);
  TTF_Quit;
  SDL_Quit();
  inherited;
end;

end.

