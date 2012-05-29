unit uprogressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL, SDL_IMAGE, umisc, upoints;

const
  LOADING_REMAINING_PATH = INTERFACE_PATH + 'loading_remaining.png';
  LOADING_DONE_PATH = INTERFACE_PATH + 'loading_done.png';
  LOADING_LEFT_PATH = INTERFACE_PATH + 'loading_left.png';
  LOADING_RIGHT_PATH = INTERFACE_PATH + 'loading_right.png';

  REMAINING_WIDTH = 32;
  RIGHT_WIDTH = 26;
  LEFT_WIDTH = 26;

  VOLBAR_BOTTOM = 10;
  VOLBAR_RIGHT = 10;
  VOLBAR_WIDTH = 100;
  VOLBAR_HEIGHT = 20;

type

  CProgressBar = class
  protected
    part, total, Width: integer;
    left, right, done, remaining, bar: pSDL_Surface;
  public
    constructor Create;
    destructor Destroy; override;
    procedure draw(apart, atotal, awidth: integer);
    function getbar(): pSDL_Surface;
  end;

  CVolumeBar = class(CProgressBar)
    protected
      startTime, currentTime: integer;
    public
      constructor Create;
      procedure draw(apart, atotal, awidth: integer);
      function getbar(): pSDL_Surface;
  end;


implementation

constructor CProgressBar.Create;
begin
  Width := 0;
  part := 0;
  total := 0;

  done := IMG_LOAD(LOADING_DONE_PATH);
  remaining := IMG_LOAD(LOADING_REMAINING_PATH);
  left := IMG_LOAD(LOADING_LEFT_PATH);
  right := IMG_LOAD(LOADING_RIGHT_PATH);

  bar := SDL_CreateRGBSurface(SDL_SWSURFACE, Width, remaining^.h, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);
end;

destructor CProgressBar.Destroy;
begin
  SDL_FreeSurface(done);
  SDL_FreeSurface(remaining);
  SDL_FreeSurface(left);
  SDL_FreeSurface(right);
  SDL_FreeSurface(bar);
end;

procedure CProgressBar.draw(apart, atotal, awidth: integer);
begin
  if (Width <> awidth) or (part <> apart) or (total <> atotal) then
  begin
    part := apart;
    total := atotal;
    Width := awidth;

    SDL_FreeSurface(bar);
    bar := SDL_CreateRGBSurface(SDL_SWSURFACE, Width, remaining^.h,
      32, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
    SDL_FillRect(bar, nil, SDL_MapRGBA(bar^.format, 0, 0, 0, 0));

    patternFill(bar, remaining, left^.w, Width - right^.w);
    patternFill(bar, done, left^.w, left^.w + round(
      (part / total) * (Width - left^.w - right^.w)));
    fastBlit(left, bar, 0, 0);
    fastBlit(right, bar, Width - right^.w, 0);
  end;
end;

function CProgressBar.getbar(): pSDL_Surface;
begin
  Result := bar;
end;

constructor CVolumeBar.create;
begin
  inherited create;
  SDL_FreeSurface(left);
  SDL_FreeSurface(right);
  SDL_FreeSurface(remaining);
  SDL_FreeSurface(done);

  startTime:=0;
  currentTime:=0;

  left := SDL_CreateRGBSurface(SDL_SWSURFACE, 1, VOLBAR_HEIGHT, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);
  right := SDL_CreateRGBSurface(SDL_SWSURFACE, 1, VOLBAR_HEIGHT, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);
  remaining := SDL_CreateRGBSurface(SDL_SWSURFACE, 1, VOLBAR_HEIGHT, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);
  done := SDL_CreateRGBSurface(SDL_SWSURFACE, 1, VOLBAR_HEIGHT, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);

  SDL_FillRect(left, nil, SDL_MapRGB(left^.format, 0, 120, 200));
  SDL_FillRect(right, nil, SDL_MapRGB(right^.format, 20, 20, 20));
  SDL_FillRect(done, nil, SDL_MapRGB(done^.format, 0, 120, 200));
  SDL_FillRect(remaining, nil, SDL_MapRGB(remaining^.format, 20, 20, 20));

  bar := SDL_CreateRGBSurface(SDL_SWSURFACE, VOLBAR_WIDTH, VOLBAR_HEIGHT, 32,
    0, 0, 0, SDL_ALPHA_TRANSPARENT);
end;

procedure CVolumeBar.draw(apart, atotal, awidth:integer);
begin
  inherited draw(apart, atotal, awidth);
  StartTime:=currentTime;
end;

function CVolumeBar.getbar(): pSDL_Surface;
begin
  Result:=(self as CProgressBar).getbar();
  currentTime:=currentTime+1;
  fadeInOut(bar, startTime, currentTime, 80, 200);
end;

end.

