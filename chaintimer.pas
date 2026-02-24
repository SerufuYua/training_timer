unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, ProgressBar,
  Config, uplaysound;

type

  { TFrameTimer }

  TTimePeriod = record
    Name: String;
    FinalSound: TSoundType;
    TimeMs, WarningTimeMs: Comp;
    Warning: Boolean;
    Color: TColor;
  end;

  TPeriodsList = Array of TTimePeriod;
  TStartEvent = procedure(ASetName: String; APeriods: TPeriodsList) of object;

  TFrameTimer = class(TFrame)
    ButtonRestart: TButton;
    ButtonStop: TButton;
    ButtonPause: TButton;
    FrameProgressUse: TFrameProgress;
    LabelFullTime: TLabel;
    LabelPeriod: TLabel;
    LabelSet: TLabel;
    PanelTime: TPanel;
    PanelLabels: TPanel;
    PanelCounter: TPanel;
    PanelControl: TPanel;
    PlaySound: Tplaysound;
    TimeCounter: TTimer;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRestartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimeCounterTimer(Sender: TObject);
  protected
    FPeriods: TPeriodsList;
    FStopEvent: TNotifyEvent;
    FPeriod: Integer;
    FStartTimeMs, FStartPauseMs, FLastTimeMsRemaining, FPeriodTimeMs,
      FWarningTimeMs, FFullTimeMs: Comp;
    FWarning: Boolean;
    FFinalSound: TSoundType;
    FSignalColor: TColor;
    procedure ResetTimer;
    procedure Pause;
    procedure Continue;
    procedure ShowTime(ATimeMs: Integer);
    procedure ShowFullTime(ATimeMs: Integer);
    procedure WritePeriod(AValue: Integer);
    function TimeMs: Comp;
  public
    procedure UpdateTime;
    procedure Start(ASetName: String; APeriods: TPeriodsList);
    property Period: Integer read FPeriod write WritePeriod;
    property StopEvent: TNotifyEvent write FStopEvent;
  end;

implementation

{$R *.lfm}

{ TFrameTimer }

procedure TFrameTimer.Start(ASetName: String; APeriods: TPeriodsList);
var
  i: Integer;
begin
  TimeCounter.Interval:= TimerInterval;
  TimeCounter.Enabled:= False;
  LabelSet.Caption:= ASetName;
  FPeriods:= APeriods;

  FFullTimeMs:= 0;
  for i:= Low(FPeriods) to High(FPeriods) do
  begin
    FFullTimeMs:= FFullTimeMs + FPeriods[i].TimeMs;
  end;

  { enable timer }
  ResetTimer;
end;

procedure TFrameTimer.ResetTimer;
begin
  FPeriodTimeMs:= 0;
  FStartTimeMs:= TimeMs;
  Period:= 0;
  TimeCounter.Enabled:= True;
  ButtonPause.Enabled:= True;
  ButtonPause.Caption:= 'Pause';
  PlaySound.SoundFile:= Sound(TSoundType.Init);
  PlaySound.Execute;
end;

procedure TFrameTimer.Pause;
begin
  FStartPauseMs:= TimeMs;
  TimeCounter.Enabled:= False;
  ButtonPause.Caption:= 'Continue...';
end;

procedure TFrameTimer.Continue;
begin
  FStartTimeMs:= FStartTimeMs + (TimeMs - FStartPauseMs);
  TimeCounter.Enabled:= True;
  ButtonPause.Caption:= 'Pause';
end;

procedure TFrameTimer.UpdateTime;
const
  initTime = 1000;

var
  TimeMsElapsed, TimeMsRemaining: Comp;

function IsTime(thisTime: Comp): Boolean; inline;
begin
  Result:= ((FLastTimeMsRemaining >= thisTime) AND
            (TimeMsRemaining < thisTime));
end;

begin
  TimeMsElapsed:= TimeMs - FStartTimeMs;
  TimeMsRemaining:= FPeriodTimeMs - TimeMsElapsed;

  { play warning and initial signals }
  if (FWarning AND IsTime(FWarningTimeMs)) then
  begin
    PlaySound.SoundFile:= Sound(TSoundType.Warn);
    PlaySound.Execute;
  end
  else
  if (IsTime(initTime) OR IsTime(initTime * 2) OR IsTime(initTime * 3)) then
  begin
    PlaySound.SoundFile:= Sound(TSoundType.Init);
    PlaySound.Execute;
  end;

  { color blink initial signal }
  if (IsTime(initTime * 1) OR
      IsTime(initTime * 2) OR
      IsTime(initTime * 3)) then
  begin
    FrameProgressUse.ProgressColor:= clBlack;
  end
  else
  if (IsTime(initTime * 1 - (initTime div 2)) OR
      IsTime(initTime * 2 - (initTime div 2)) OR
      IsTime(initTime * 3 - (initTime div 2))) then
  begin
    FrameProgressUse.ProgressColor:= FSignalColor;
  end;

  { color blink warning signal }
  if FWarning then
  begin
    if IsTime(FWarningTimeMs) then
    begin
      FrameProgressUse.ProgressColor:= clGray;
    end
    else
    if ((FWarningTimeMs > initTime) AND
        (IsTime(FWarningTimeMs - (initTime div 2)))) then
    begin
      FrameProgressUse.ProgressColor:= FSignalColor;
    end;
  end;

  { count time and change period }
  if (TimeMsRemaining > 0) then
  begin
    ShowTime(Round(TimeMsRemaining));
    ShowFullTime(Round(FFullTimeMs - TimeMsElapsed));
    FrameProgressUse.Progress:= FrameProgressUse.MaxProgress - Round(TimeMsRemaining);
  end
  else
  begin
    ShowTime(0);
    PlaySound.SoundFile:= Sound(FFinalSound);
    PlaySound.Execute;
    Period:= Period + 1;
  end;

  { remember Time Remaining }
  FLastTimeMsRemaining:= TimeMsRemaining;

end;

procedure TFrameTimer.ButtonPauseClick(Sender: TObject);
begin
  if TimeCounter.Enabled then
    Pause
  else
    Continue;
end;

procedure TFrameTimer.ButtonRestartClick(Sender: TObject);
begin
  ResetTimer;
end;

procedure TFrameTimer.ButtonStopClick(Sender: TObject);
begin
  TimeCounter.Enabled:= False;
  if Assigned(FStopEvent) then
    FStopEvent(self);
end;

procedure TFrameTimer.TimeCounterTimer(Sender: TObject);
begin
  UpdateTime;
end;

procedure TFrameTimer.ShowTime(ATimeMs: Integer);
var
  min, sec: Integer;
begin
  sec:= (ATimeMs + 1) div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  FrameProgressUse.Text:= IntToStr(min) + ':' + IntToStr(sec);
end;

procedure TFrameTimer.ShowFullTime(ATimeMs: Integer);
var
  min, sec: Integer;
begin
  sec:= (ATimeMs + 1) div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  LabelFullTime.Caption:= IntToStr(min) + ':' + IntToStr(sec);
end;

procedure TFrameTimer.WritePeriod(AValue: Integer);
begin
  if (AValue < Length(FPeriods)) then
  begin
    FPeriod:= AValue;
    ShowTime(Round(FPeriods[AValue].TimeMs));
    LabelPeriod.Caption:= FPeriods[AValue].Name;
    FSignalColor:= FPeriods[AValue].Color;
    FrameProgressUse.ProgressColor:= FSignalColor;
    FWarningTimeMs:= FPeriods[AValue].WarningTimeMs;
    FWarning:= FPeriods[AValue].Warning;
    FPeriodTimeMs:= FPeriodTimeMs + FPeriods[AValue].TimeMs;
    FrameProgressUse.MaxProgress:= Round(FPeriods[AValue].TimeMs);
    FrameProgressUse.MinProgress:= 0;
    FrameProgressUse.Progress:= 0;
    FFinalSound:= FPeriods[AValue].FinalSound;
  end
  else
  begin
    TimeCounter.Enabled:= False;
    FrameProgressUse.ProgressColor:= clBlack;
    ButtonPause.Enabled:= False;
  end;
end;

function TFrameTimer.TimeMs: Comp;
begin
  Result:= TimeStampToMSecs(DateTimeToTimeStamp(Time));
end;

end.

