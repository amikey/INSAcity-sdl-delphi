unit utext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL, SDL_ttf;

const
  FONT_DEFAULT_SIZE = 13;

type
  CText = class
  protected
    font: pointer;
    path: PChar;
    defaultsize: integer;
    color: pSDL_Color;
  public
    constructor Create(aPath: PChar; size: integer = FONT_DEFAULT_SIZE;
      r: integer = 255; g: integer = 255; b: integer = 255);
    destructor Destroy; override;
    procedure SetColor(r, g, b: byte);
    procedure OpenFont(size: integer);
    procedure DrawTo(Text: string; surface: pSDL_Surface; x, y: integer;
      size: integer = -1; r: integer = -1; g: integer = -1; b: integer = -1);
  end;

implementation

constructor CText.Create(aPath: PChar; size: integer = FONT_DEFAULT_SIZE;
  r: integer = 255; g: integer = 255; b: integer = 255);
begin
  NEW(color);
  path := aPath;
  defaultsize := size;
  OpenFont(size);
  SetColor(r, g, b);
end;

destructor CText.Destroy;
begin
  DISPOSE(color);
  if (font <> nil) then
    TTF_CLOSEFONT(font);
  inherited;
end;

procedure CText.OpenFont(size: integer);
begin
  if (font <> nil) then
    TTF_CloseFont(font);
  font := TTF_OPENFONT(path, size);
end;


procedure CText.SetColor(r, g, b: byte);
begin
  color^.r := r;
  color^.g := g;
  color^.b := b;
end;


procedure CText.DrawTo(Text: string; surface: pSDL_Surface; x, y: integer;
  size: integer = -1; r: integer = -1; g: integer = -1; b: integer = -1);
var
  tmp: pSDL_Rect;
  message: pSDL_Surface;
  textcolor: pSDL_Color;
begin
  NEW(textcolor);

  if (r <> -1) and (g <> -1) and (b <> -1) then
  begin
    textcolor^.r := r;
    textcolor^.g := g;
    textcolor^.b := b;
  end
  else
    textcolor^ := color^;

  if (size <> -1) then
    OpenFont(size);
  message := TTF_RenderText_Blended(font, PChar(Text), textcolor^);
  DISPOSE(textcolor);

  NEW(tmp);
  tmp^.x := x;
  tmp^.y := y;
  tmp^.h := surface^.h;
  tmp^.w := surface^.w;
  SDL_BlitSurface(message, nil, surface, tmp);

  DISPOSE(tmp);
  if (size <> -1) and (size <> defaultsize) then
    OpenFont(defaultsize);
  if message <> nil then
    SDL_FreeSurface(message);
end;

end.

