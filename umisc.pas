unit umisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL;

const
  FADE_TIME = 100;

//An easier procedure to blit an images
procedure fastBlit(surface, dest: pSDL_Surface; x, y: integer;
  crop: pSDL_Rect = nil; w: integer = -1; h: integer = -1);

//Fade and (optionally) fade out effect
procedure fadeInOut(surface: pSDL_Surface; t0, tcurrent, lifespan: integer;
  total_opacity: integer = 255);

//Projects a "shadow" on a surface (image or other). Works better with rectangles (and squares).
procedure projectShadow(surface, dest: pSDL_Surface; x, y: integer;
  offset: integer = 3; opacity: integer = 100);

//Fills a surface with an image (pattern)
procedure patternFill(surface, img: pSDL_Surface; xmin: integer = 0;
  xmax: integer = -1; ymin: integer = 0; ymax: integer = -1);

implementation

procedure fastBlit(surface, dest: pSDL_Surface; x, y: integer;
  crop: pSDL_Rect = nil; w: integer = -1; h: integer = -1);
var
  tmp: pSDL_Rect;
begin
  NEW(tmp);
  tmp^.x := x;
  tmp^.y := y;
  if h = -1 then
    tmp^.h := surface^.h;
  if w = -1 then
    tmp^.w := surface^.w;
  SDL_BlitSurface(surface, crop, dest, tmp);
  DISPOSE(tmp);
end;

procedure fadeInOut(surface: pSDL_Surface; t0, tcurrent: integer;
  lifespan: integer = -1; total_opacity: integer = 255);
var
  delta_t, alpha: integer;
begin
  delta_t := tcurrent - t0;
  alpha := total_opacity; //100% of total_opacity, if not in FADE IN or FADE OUT

  //When lifespan=-1 e.g when we only FADE IN and stay at total_opacity
  if (lifespan = -1) then
    if (delta_t < FADE_TIME) then
      alpha := round(total_opacity * (delta_t / FADE_TIME));

  //FADE IN (first 1/4)
  if ((delta_t) < (lifespan div 4)) then
    alpha := round(total_opacity * 4 * (delta_t / lifespan));

  //FADE OUT (last 1/5), only if lifespan is finite
  if lifespan <> -1 then
    if (delta_t) > (lifespan * 4 div 5) then
      alpha := total_opacity - round(total_opacity *
        ((delta_t - lifespan * 4 / 5) / (lifespan / 5)));

  //Just to avoid any value of alpha out of the array 0..255
  if alpha > 255 then
    alpha := 255;
  if alpha < 0 then
    alpha := 0;

  SDL_SetAlpha(surface, SDL_SRCALPHA or SDL_RLEACCEL, alpha);
end;

procedure projectShadow(surface, dest: pSDL_Surface; x, y: integer;
  offset: integer = 3; opacity: integer = 100);
var
  shadow: pSDL_Surface;
begin
  shadow := SDL_CreateRGBSurface(SDL_SWSURFACE, surface^.w, surface^.h, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);
  SDL_FillRect(shadow, nil, SDL_MapRGB(dest^.format, 50, 50, 50));
  SDL_SetAlpha(shadow, SDL_SRCALPHA or SDL_RLEACCEL, opacity);
  fastBlit(shadow, dest, x + offset, y + offset);
  SDL_FreeSurface(shadow);
end;

procedure patternFill(surface, img: pSDL_Surface; xmin: integer = 0;
  xmax: integer = -1; ymin: integer = 0; ymax: integer = -1);
var
  i, j, x, y: integer;
begin

  for i := 0 to ((surface^.w - xmin) div img^.w) do
    for j := 0 to ((surface^.h - ymin) div img^.h) do
    begin
      x := xmin + i * img^.w;
      y := ymin + j * img^.h;
      if (x >= xmin) and (y >= ymin) then
        if ((xmax = -1) or (x <= xmax)) and ((ymax = -1) or (y <= ymax)) then
          fastBlit(img, surface, x, y);
    end;
end;

end.

