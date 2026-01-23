unit timer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  TTimePeriod = record
    Name, Sound: String;
    Time: Integer;
  end;

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
    procedure AddPeriod(AData: TTimePeriod);
    procedure Start;
  end;

implementation

{$R *.lfm}

{ TFrameTimer }

procedure TFrameTimer.AddPeriod(AData: TTimePeriod);
begin

end;

procedure TFrameTimer.Start;
begin
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

