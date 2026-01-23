unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TFrameTimer }

  TFrameTimer = class(TFrame)
    ButtonStop: TButton;
    ButtonPause: TButton;
    LabelTime: TLabel;
    LabelTitle: TLabel;
    PanelCounter: TPanel;
    PanelControl: TPanel;
    TimerCount: TTimer;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerCountTimer(Sender: TObject);
  protected
    TimeMs: Cardinal;
    procedure ResetTimer;
    procedure ShowTime;
  public
    procedure Start(APeriods: Integer; ARoundTime, ARestTime, APrepareTime, AWarningTime: Cardinal);
  end;

implementation

type
  TTimePeriod = record
    Name, FinalSound: String;
    Time: Integer;
  end;

  TPeriodsList = Array of TTimePeriod;

var
 Periods: TPeriodsList;

const
  SoundStart = 'start.wav';
  SoundEnd = 'end.wav';
  SoundFinal = 'fin.wav';
  SoundWarn = 'warn.wav';

{$R *.lfm}

{ TFrameTimer }

procedure TFrameTimer.Start(APeriods: Integer; ARoundTime, ARestTime, APrepareTime, AWarningTime: Cardinal);
var
 i, lastPeriod: Integer;
begin
  { prepare periods list }
  SetLength(Periods, APeriods * 2);

  Periods[0].Name:= 'Prepare';
  Periods[0].FinalSound:= SoundStart;
  Periods[0].Time:= APrepareTime;

  lastPeriod:= APeriods * 2 - 1;

  for i:= 1 to lastPeriod do
  begin
    if ((i mod 2) = 0) then
    begin
      Periods[i].Name:= 'Rest';
      Periods[i].Time:= ARestTime;
      Periods[i].FinalSound:= SoundStart;
    end
    else
    begin
      Periods[i].Name:= 'Round ' + IntToStr((i div 2) + 1);
      Periods[i].Time:= ARoundTime;
      if (i = lastPeriod) then
        Periods[i].FinalSound:= SoundFinal
      else
        Periods[i].FinalSound:= SoundEnd;
    end;
  end;

  { enable timer }
  ResetTimer;
  TimerCount.Enabled:= True;
end;

procedure TFrameTimer.ResetTimer;
begin
  TimeMs:= 0;
  ShowTime;
end;

procedure TFrameTimer.TimerCountTimer(Sender: TObject);
begin
  TimeMs:= TimeMs + TimerCount.Interval;
  ShowTime;
end;

procedure TFrameTimer.ButtonPauseClick(Sender: TObject);
begin
  if TimerCount.Enabled then
  begin
    TimerCount.Enabled:= False;
    ButtonPause.Caption:= 'Continue';
  end
  else
  begin
    TimerCount.Enabled:= True;
    ButtonPause.Caption:= 'Pause';
  end;

end;

procedure TFrameTimer.ButtonStopClick(Sender: TObject);
begin
  TimerCount.Enabled:= False;
  ResetTimer;
end;

procedure TFrameTimer.ShowTime;
var
  min, sec: Cardinal;
begin
  sec:= TimeMs div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  LabelTime.Caption:= IntToStr(min) + ':' + IntToStr(sec);
end;

end.

