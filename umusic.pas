unit umusic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL, SDL_mixer_nosmpeg, FileUtil, usummary, umisc, uprogressbar;

const
  {$IFDEF UNIX}
  MUSIC_DIR = 'music/';
  SOUND_DIR = MUSIC_DIR + 'sounds/';
  {$ENDIF}
  {$IFDEF Win32}
  MUSIC_DIR = 'music\';
  SOUND_DIR = MUSIC_DIR + 'sounds\';
  {$ENDIF}
  AUDIO_FREQUENCY = 44100; //or 22050
  AUDIO_FORMAT = AUDIO_S16;
  AUDIO_CHANNELS = 2;
  AUDIO_CHUNKSIZE = 4096;
  MUSIC_FILES = '*.ogg';
  MAX_FILES = 42;
  DEFAULT_VOLUME = 100;
  VOLUME_INC = 4;
  MAX_VOLUME = 128;
  NOTIF_SOUND = SOUND_DIR + 'moom.ogg';

type
  CMusic = class
  protected
    volumebar: CVolumeBar;
    volbar: pSDL_Surface;
    background: pMix_Music;
    files: array[1..MAX_FILES] of string;
    playing, Count, volume, prevolume: integer;
    started: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Next;
    procedure LoadFiles;
    procedure TogglePause;
    procedure set_volume;
    procedure set_volume(vol: integer); overload;
    procedure up_volume;
    procedure lower_volume;
    procedure ToggleMute;
    procedure Stop;
    function is_started(): boolean;
    function is_playing(): boolean;
    function is_paused(): boolean;
    procedure DrawVolumeBar;
    function onVolumeBar(x, y: integer): boolean;
    procedure clickVolumeBar(x: integer);
    procedure setVolumeBar;
  end;

implementation

constructor CMusic.Create;
begin
  randomize;
  Count := 0;
  volume := 0;
  prevolume := 0;
  background := nil;
  Mix_OpenAudio(AUDIO_FREQUENCY, AUDIO_FORMAT, AUDIO_CHANNELS, AUDIO_CHUNKSIZE);
  volume := DEFAULT_VOLUME;
  //volume bar
  volumebar:= CVolumeBar.create;
  volbar := SDL_CreateRGBSurface(SDL_SWSURFACE, VOLBAR_WIDTH,
    VOLBAR_HEIGHT, BPP, 0, 0, 0, SDL_ALPHA_TRANSPARENT);
  set_volume(volume);
  LoadFiles;
  playing := random(Count + 1);
  started := False;
end;

procedure CMusic.LoadFiles;
var
  Rec: TSearchRec;
begin
  if FindFirst(MUSIC_DIR + MUSIC_FILES, faAnyFile - faDirectory, Rec) = 0 then
    try
      repeat
        Count := Count + 1;
        files[Count] := Rec.Name;
      until not ((FindNext(Rec) = 0) and (Count < MAX_FILES));
    finally
      FindClose(Rec);
    end;
end;

procedure CMusic.Next;
begin
  started := True;
  if (background <> nil) then
    Mix_FreeMusic(background);
  if (playing = MAX_FILES) then
    playing := 0;
  playing := playing + 1;
  background := Mix_LoadMus(PChar(MUSIC_DIR + files[playing]));
  Mix_PlayMusic(background, 0);
end;

procedure CMusic.Stop;
begin
  started := False;
  Mix_HaltMusic;
end;

procedure CMusic.TogglePause;
begin
  if (Mix_PausedMusic() = 1) then
    Mix_ResumeMusic
  else
    Mix_PauseMusic;
end;

destructor CMusic.Destroy;
begin
  SDL_FreeSurface(volbar);
  Mix_FreeMusic(background);
  Mix_CloseAudio;
end;

function CMusic.is_playing(): boolean;
begin
  Result := Mix_PlayingMusic() <> 0;
end;

function CMusic.is_paused(): boolean;
begin
  Result := Mix_PausedMusic() = 1;
end;

function CMusic.is_started(): boolean;
begin
  Result := started;
end;

procedure CMusic.set_volume(vol: integer);
begin
  if ((vol >= 0) and (vol <= MAX_VOLUME)) then
  begin
    volume := vol;
    Mix_VolumeMusic(vol);
    setVolumeBar;
  end;
end;

procedure CMusic.set_volume;
begin
  set_volume(volume);
end;

procedure CMusic.up_volume;
begin
  if (volume < MAX_VOLUME) then
    set_volume(volume + VOLUME_INC);
end;

procedure CMusic.lower_volume;
begin
  if (volume > 0) then
    set_volume(volume - VOLUME_INC);
end;

procedure CMusic.ToggleMute;
begin
  if (Mix_VolumeMusic(-1) <> 0) then
  begin
    prevolume := volume;
    set_volume(0);
  end
  else
  begin
     volume := prevolume;
    set_volume;
  end;
end;

procedure CMusic.DrawVolumeBar;
var
  screen: pSDL_Surface;
begin
  screen := SDL_GetVideoSurface();
  fastBlit(volumebar.getbar(), screen, screen^.w - VOLBAR_RIGHT - VOLBAR_WIDTH,
    screen^.h - VOLBAR_BOTTOM - VOLBAR_HEIGHT);
end;

procedure CMusic.setVolumeBar;
begin
  volumebar.draw(volume, MAX_VOLUME, VOLBAR_WIDTH);
end;

function CMusic.onVolumeBar(x, y: integer): boolean;
var
  screen: pSDL_Surface;
begin
  screen := SDL_GetVideoSurface();
  Result := (x >= screen^.w - VOLBAR_RIGHT - VOLBAR_WIDTH) and
    (x <= screen^.w - VOLBAR_RIGHT) and (y >= screen^.h - VOLBAR_BOTTOM -
    VOLBAR_HEIGHT) and (y <= screen^.h - VOLBAR_BOTTOM);
end;

procedure CMusic.clickVolumeBar(x: integer);
var
  screen: pSDL_Surface;
begin
  screen := SDL_GetVideoSurface();
  x := x - screen^.w + VOLBAR_RIGHT + VOLBAR_WIDTH;
  if (x < 0) then x := 0
  else if (x > VOLBAR_WIDTH) then x := VOLBAR_WIDTH;
  set_volume(round(x * MAX_VOLUME / VOLBAR_WIDTH));
end;

end.

