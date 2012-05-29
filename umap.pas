unit umap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL, SDL_IMAGE, ubuilding, utools, umisc, upoints;

const

  XMAX = 35;
  YMAX = 20;

  GRASS_PATH = IMG_PATH + 'grassbig.png';
  TREE_PATH = IMG_PATH + 'tree2.png';
  COAST_PATH = IMG_PATH + 'coastbig.png';
  MOUTAIN_PATH = IMG_PATH + 'moutainbig.png';
  WATER_PATH = IMG_PATH + 'waterbig.png';

type
  TStatGrid = array [0..XMAX, 0..YMAX] of integer;

  //MAP CLASS: This class contains informations about the buildings/elements in each cell
  //but also procedures to draw these elements on the screen, or add/remove them.

  CMap = class
  protected
    //The surface containing the map
    mapsurface: pSDL_Surface;

    //Natural elements (trees and stuff)
    grass, tree, mountain, water, coast: CElement;

    //The matrice containing all the map's elements/buildings
    grid: array [0..XMAX, 0..YMAX] of array [1..3] of CElement;

    //Grids containing informations about the spread of pollution/population in the city
    pol_grid, popu_grid: TStatGrid;

    //1: Normal map; 2: Pollution map; 3: Population's map
    mapindex: integer;

    //Natural profile of the map (density of trees and mountains)
    tree_den, mountain_den: integer;

  public
    //A magic algorithm that generates a random map
    constructor init(tree_density, mountain_density:integer);
    constructor init; overload;

    //Crops the entire map on the screen
    procedure draw(xoff, yoff: integer);

    //Updates a particular tile (for example when removing or adding a building
    procedure updatemap(x, y: integer; radius: integer = 2);

    //Updates stats (pollution, ...)
    procedure updatestat(x, y: integer; building: CBuilding; factor: integer = 1);

    //Tells if the cursor is on the map
    function onmap(x, y: integer): boolean;
    // Shows if the (x,y) position [of the cursor] is on the map
    function inmap(x, y: integer): boolean;
    //Change of "basis". Used for scrolling
    procedure changebasis(var x, y: integer);
    //Adds a building in the (x,y) tile
    procedure add(x, y: integer; building: CBuilding);
    //Removes the building built in the (x,y) tile
    procedure remove(x, y: integer);
    procedure setmapindex(index: integer);
    function is_built(x, y: integer): boolean;
    //True when there's already a building (or a mountain!) in that tile
    function getBuilding(x, y: integer): CElement;
    //Returns the building built in that particular tile
    destructor Destroy; override;
  end;

implementation

constructor CMap.init(tree_density, mountain_density:integer);
var
  i, j, k, x, y: integer;
begin
  //We store density values
  tree_den:=tree_density;
  mountain_den:=mountain_density;

  //We initialize our elements
  grass := CElement.init('Grass', 'Not *that* kind of grass', IMG_LOAD(GRASS_PATH));
  tree := CElement.init('Tree', 'Made famous by Newton', IMG_LOAD(TREE_PATH));
  coast := CElement.init('Coast', 'Let''s build a sand castle !', IMG_LOAD(COAST_PATH));
  mountain := CElement.init('Mountain', 'Over 9000 ft high', IMG_LOAD(MOUTAIN_PATH));
  water := CElement.init('Ocean', 'Big, blue and boring.', IMG_LOAD(WATER_PATH));

  //The map shown is initially the normal one (and not pollution or population density map)
  mapindex := 1;

  for i := 0 to XMAX do
    for j := 0 to YMAX do
    begin
      popu_grid[i, j] := 0;  //At the beginning, there was no one ...
      pol_grid[i, j] := 0;   //.. and the air smelled good !
      for k := 1 to 3 do
        grid[i, j][k] := nil;  //No buildings as well
    end;


  for i := 0 to XMAX do
    for j := 0 to (YMAX - 5) do
    begin
      grid[i, j][1] := grass; // We grow some grass ...
      if (random(31-tree_density) mod (30-tree_density))=0 then
        grid[i, j][2] := tree; // ... plant some trees ...
      if (random(201-mountain_density) mod (200-mountain_density)) = 0 then
        //The following lines create mountain ranges
        //and not isolated mountains, which wouldn't be realistic ...
        for x := i - 1 to i + 1 do
          for y := j - 1 to j + 1 do
            if ((random(abs(x - i) + 2) mod (abs(x - i) + 1)) = 0) and
              (x >= 0) and (x <= XMAX) and (y >= 0) and (y <= YMAX - 7) then
              grid[x, y][2] := mountain; // ... sculpt some mountains ...
    end;

  for i := 0 to XMAX do
    grid[i, YMAX - 4][1] := coast; // ... cut the coast ...

  for i := 0 to XMAX do
    for j := YMAX - 3 to (YMAX) do
      grid[i, j][1] := water; // ... and fill the ocean. Et voilà !

  mapsurface := SDL_CreateRGBSurface(SDL_SWSURFACE, XMAX * TILE_SIZE,
    YMAX * TILE_SIZE, BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);

  //We initialize the grid
  for i := 0 to XMAX do
    for j := 0 to YMAX do
      updatemap(i, j);

end;

constructor CMap.init;
begin
  init(tree_den,mountain_den);
end;

procedure CMap.setmapindex(index: integer);
var
  i, j: integer;
begin
  mapindex := index;
  for i := 0 to XMAX do
    for j := 0 to YMAX do
      updatemap(i, j, 0);
end;

procedure CMap.updatemap(x, y: integer; radius: integer = 2);
var
  i, j, k, alpha: integer;
  surface: pSDL_Surface;
  datagrid: TStatGrid;
begin
  for k := 1 to 3 do
    if (grid[x, y][k] <> nil) then
      fastBlit(grid[x, y][k].getimg(), mapsurface, TILE_SIZE * x,
        TILE_SIZE * y, nil, TILE_SIZE, TILE_SIZE);

  if mapindex <> 1 then
  begin
    if mapindex = 2 then
      datagrid := pol_grid
    else if mapindex = 3 then
      datagrid := popu_grid;
    surface := SDL_CreateRGBSurface(SDL_SWSURFACE, TILE_SIZE, TILE_SIZE,
      BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
    for i := x - radius to x + radius do
      for j := y - radius to y + radius do
        if (i >= 0) and (i <= XMAX) and (j >= 0) and (j <= YMAX) then
        begin
          for k := 1 to 3 do
            if (grid[i, j][k] <> nil) then
              fastBlit(grid[i, j][k].getimg(), mapsurface, TILE_SIZE *
                i, TILE_SIZE * j, nil, TILE_SIZE, TILE_SIZE);

          //To avoid an alpha bigger than 255 or smaller than 0 (doesn't make sens):
          if (datagrid[i, j] <= 255) and (datagrid[i, j] >= 0) then
            alpha := datagrid[i, j];
          if datagrid[i, j] > 255 then
            alpha := 255;
          if datagrid[i, j] < 0 then
            alpha := 0;

          SDL_FillRect(surface, nil, SDL_MapRGB(surface^.format, 255, 0, 0));
          SDL_SetAlpha(surface, SDL_SRCALPHA or SDL_RLEACCEL, alpha);
          fastBlit(surface, mapsurface, TILE_SIZE * i, TILE_SIZE * j);
        end;
    SDL_FreeSurface(surface);
  end;
end;

procedure CMap.updatestat(x, y: integer; building: CBuilding; factor: integer = 1);
var
  i, j: integer;
  d: double;
begin
  if building.getPollution() <> 0 then
    for i := (x - 2) to (x + 2) do
      for j := (y - 2) to (y + 2) do
      begin
        if (i = x) and (j = y) then
          d := 0.5
        else
          d := sqrt(sqr(abs(x - i)) + sqr(abs(y - j)));
        if (i >= 0) and (i <= XMAX) and (j >= 0) and (j <= YMAX) then
          pol_grid[i, j] := pol_grid[i, j] + factor *
            round(9 * building.getPollution() / d);
        // We multiplied by 9 (or another number) to add "contrast" to the data

      end;

  //Same thing for the population grid
  if building.getHome() <> 0 then
    for i := (x - 3) to (x + 3) do
      for j := (y - 3) to (y + 3) do
      begin
        if (i = x) and (j = y) then
          d := 0.5
        else
          d := sqrt(sqr(abs(x - i)) + sqr(abs(y - j)));
        if (i >= 0) and (i <= XMAX) and (j >= 0) and (j <= YMAX) then
          popu_grid[i, j] := popu_grid[i, j] + factor *
            round(2 * building.getHome() / d);
      end;

end;

procedure CMap.draw(xoff, yoff: integer);
var
  screen: pSDL_Surface;
  crop: pSDL_Rect;
begin
  screen := SDL_GetVideoSurface();

  NEW(crop);
  crop^.w := screen^.w;
  crop^.h := screen^.h - TOP_BAR - BOTTOM_HEIGHT - NEWS_HEIGHT;
  crop^.x := xoff;
  crop^.y := yoff;

  fastBlit(mapsurface, screen, MAP_LEFT, TOP_BAR, crop);

  DISPOSE(crop);
end;

procedure CMap.add(x, y: integer; building: CBuilding);
begin
  grid[x, y][2] := nil; //We remove any "element" (eg: forest) from the construction area
  grid[x, y][3] := building; //We add the corresponding building
  updatestat(x, y, building);
  if mapindex = 1 then
    updatemap(x, y, 0)
  else
    updatemap(x, y);
end;

procedure CMap.remove(x, y: integer);
begin
  if grid[x, y][3] <> nil then
  begin
    CBuilding(grid[x, y][3]).remove;
    updatestat(x, y, CBuilding(grid[x, y][3]), -1);
    grid[x, y][3] := nil;
    if mapindex = 1 then
      updatemap(x, y, 0)
    else
      updatemap(x, y);
  end;
end;

function CMap.is_built(x, y: integer): boolean;
var
  is_building, is_unbuildable: boolean;
begin
  is_building := grid[x, y][3] <> nil;
  is_unbuildable := (grid[x, y][1].getname() = 'Coast') or
    (grid[x, y][1].getname() = 'Ocean');
  if (grid[x, y][2] <> nil) then
    is_unbuildable := (is_unbuildable) or (grid[x, y][2].getname() = 'Mountain');
  Result := is_building or is_unbuildable;
end;

procedure CMap.changebasis(var x, y: integer);
begin
  x := x - MAP_LEFT;
  y := y - TOP_BAR;
end;

function CMap.onmap(x, y: integer): boolean;
var
  screen: pSDL_Surface;
begin
  screen := SDL_GetVideoSurface();
  Result := ((x >= 0) and (x <= screen^.w) and (y >= TOP_BAR) and
    (y <= screen^.h - TOP_BAR - BOTTOM_HEIGHT - NEWS_HEIGHT));
end;

function CMap.inmap(x, y: integer): boolean;
var
  screen: pSDL_Surface;
begin
  screen := SDL_GetVideoSurface();
  Result := ((x >= 0) and (x <= XMAX * TILE_SIZE - screen^.w) and
    (y >= 0) and (y <= YMAX * TILE_SIZE - screen^.h + TOP_BAR +
    BOTTOM_HEIGHT + NEWS_HEIGHT));
end;

function CMap.getBuilding(x, y: integer): CElement;
begin
  Result := grid[x, y][3];
end;

destructor CMap.Destroy;
begin
  grass.Destroy;
  tree.Destroy;
  mountain.Destroy;
  coast.Destroy;
  water.Destroy;
  SDL_FreeSurface(mapsurface);
end;

end.

