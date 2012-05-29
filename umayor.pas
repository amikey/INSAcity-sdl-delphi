unit umayor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  INIT_MAX_XP = 250;

  {$IFDEF UNIX}
  FILES_PATH = 'files/';
  {$ENDIF}

  {$IFDEF Win32}
  FILES_PATH = 'files\';
  {$ENDIF}

type

  //A pretty self-explanatory class. Basic +XP and LVL UP methods.
  CMayor = class
  protected
    name: string;
    xp, max_xp, lvl: integer;
    lvlup_notif: boolean; //True when the user must be notified that he leveld up
  public
    constructor Create(aname: string = 'Mayor');
    procedure addxp(plus_xp: integer);
    procedure lvlup;

    function getname(): string;
    function getxp(): integer;
    function getmaxxp(): integer;
    function getlvl(): integer;
    function send_notif():boolean;

    //Save/Load methods. Save files are located in "files/"
    procedure save;
    procedure load;
  end;


implementation

constructor CMayor.Create(aname: string = 'Mayor');
begin
  name := aname;
  max_xp := INIT_MAX_XP;
  lvl := 1;
  xp := 0;
  lvlup_notif:=false;
  load;
end;

procedure CMayor.addxp(plus_xp: integer);
var
  tmp: integer;
begin
  if (xp + plus_xp) < max_xp then
    xp := xp + plus_xp
  else
  begin
    tmp := plus_xp - (max_xp - xp);
    lvlup;
    addxp(tmp);
  end;
end;

procedure CMayor.lvlup;
begin
  lvl := lvl + 1;
  max_xp := round(max_xp *1.2);
  lvlup_notif:=true;
end;

function CMayor.getname(): string;
begin
  Result := name;
end;

function CMayor.getlvl(): integer;
begin
  Result := lvl;
end;

function CMayor.getxp(): integer;
begin
  Result := xp;
end;

function CMayor.getmaxxp(): integer;
begin
  Result := max_xp;
end;

function CMayor.send_notif():boolean;
begin
  Result:=lvlup_notif;
  lvlup_notif:=false;
end;

procedure CMayor.save;
var
  SaveFile:TStringList;
begin
  SaveFile:=TStringList.create;
  SaveFile.Add(name);
  SaveFile.Add(floattostr(lvl));
  SaveFile.Add(floattostr(xp));
  SaveFile.SaveToFile(FILES_PATH+name+'.txt');
  SaveFile.Clear;
end;

procedure CMayor.load;
var
  i:integer;
  LoadFile:TStringList;
begin
  LoadFile:=TStringList.create;

  //{$I+}
  try
    LoadFile.LoadFromFile(FILES_PATH+name+'.txt');
    for i:=1 to round(strtofloat(LoadFile[1])-1) do
      lvlup;
    xp:= round(strtofloat(LoadFile[2]));
  except
    begin
      writeln('No such mayor. Creating a new profile.');
    end;
  end;
end;

end.

