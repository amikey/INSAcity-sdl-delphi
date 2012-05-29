unit utimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SDL;

const
  DEFAULT_YEAR = 2012;
  MS_PER_YEAR = 200000;
  CALC_DELAY = 20;

type
  CTimer = class
  protected
    startTicks, pausedTicks: integer;
    paused, started: boolean;
  public
    constructor Create;
    procedure start;
    procedure stop;
    procedure pause;
    procedure unpause;
    procedure wait(ms: double);

    function getTicks(): integer;
    function is_started(): boolean;
    function is_paused(): boolean;

    function delay_passed(): boolean;
    procedure TogglePause;

    function Date(): string;
  end;

implementation

constructor CTimer.Create;
begin
  startTicks := 0;
  pausedTicks := 0;
  paused := False;
  started := False;
end;

procedure CTimer.start;
begin
  started := True;
  paused := False;
  startTicks := SDL_GetTicks();
end;

procedure CTimer.stop;
begin
  started := False;
  paused := False;
end;

procedure CTimer.pause;
begin
  if (started = True) and (paused = False) then
  begin
    paused := True;
    pausedTicks := SDL_GetTicks() - startTicks;
  end;
end;

procedure CTimer.unpause;
begin
  if (paused) then
  begin
    paused := False;
    startTicks := SDL_GetTicks() - pausedTicks;
    pausedTicks := 0;
  end;
end;

function CTimer.getTicks(): integer;
begin
  if (started) then
  begin
    if (paused) then
    begin
      Result := pausedTicks;
    end
    else
    begin
      Result := SDL_GetTicks() - startTicks;
    end;
  end
  else
    Result := 0;
end;

function CTimer.is_started(): boolean;
begin
  Result := started;
end;

function CTimer.is_paused(): boolean;
begin
  Result := paused;
end;

function CTimer.delay_passed(): boolean;
begin
  Result := (getTicks() mod CALC_DELAY) = 0;
end;

procedure CTimer.TogglePause;
begin
  if (paused) then
    unpause
  else
    pause;
end;

function CTimer.Date(): string;
begin
  Result := floattostr(((getTicks() mod MS_PER_YEAR) mod (MS_PER_YEAR div 12)) div
    (MS_PER_YEAR div (12 * 30)) + 1) + '/' + floattostr(
    (getTicks() mod MS_PER_YEAR) div (MS_PER_YEAR div 12) + 1) + '/' +
    floattostr(getTicks() div MS_PER_YEAR + DEFAULT_YEAR);
end;

procedure CTimer.wait(ms: double);
begin
  SDL_DELAY(round(ms));
end;

end.

