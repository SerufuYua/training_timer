unit MyCommon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMyStrs = Array of String;

function TimeToShortStr(ASeconds: Integer): String;
function TimeToFullStr(ASeconds: Integer): String;

implementation

function TimeToShortStr(ASeconds: Integer): String;
var
  min, sec: Integer;
begin
  sec:= ASeconds;
  min:= sec div 60;
  sec:= sec - (min * 60);
  Result:= Format('%.2d:%.2d', [min, sec]);
end;

function TimeToFullStr(ASeconds: Integer): String;
var
  min, sec: Integer;
begin
  sec:= ASeconds;
  min:= sec div 60;
  sec:= sec - (min * 60);
  Result:= Format('%dm %ds', [min, sec]);
end;

end.

