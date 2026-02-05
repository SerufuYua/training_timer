unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, ProgressBar,
  uplaysound;

type

  { TFrameTimer }

  TTimePeriod = record
    Name, FinalSound: String;
    TimeMs, WarningTimeMs: Comp;
    Color: TColor;
  end;

  TPeriodsList = Array of TTimePeriod;

  TFrameTimer = class(TFrame)
    ButtonRestart: TButton;
    ButtonStop: TButton;
    ButtonPause: TButton;
    FrameProgressUse: TFrameProgress;
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
    FStartTimeMs, FLastTimeMsRemaining, FPeriodTimeMs, FWarningTimeMs: Comp;
    FFinalSound: String;
    FSignalColor: TColor;
    procedure ResetTimer;
    procedure Pause;
    procedure Continue;
    procedure ShowTime(ATimeMs: Comp);
    procedure WritePeriod(AValue: Integer);
    function TimeMs: Comp;
  public
    procedure UpdateTime;
    procedure Start(ASetName: String; APeriods: TPeriodsList);
    property Period: Integer read FPeriod write WritePeriod;
    property StopEvent: TNotifyEvent write FStopEvent;
  end;

implementation

uses
  Config;

{$R *.lfm}

{ TFrameTimer }

procedure TFrameTimer.Start(ASetName: String; APeriods: TPeriodsList);
begin
  TimeCounter.Interval:= TimerInterval;
  TimeCounter.Enabled:= False;
  LabelSet.Caption:= ASetName;
  FPeriods:= APeriods;

  { enable timer }
  ResetTimer;
end;

procedure TFrameTimer.ResetTimer;
begin
  FPeriodTimeMs:= 0;
  FStartTimeMs:= TimeMs;
  Period:= 0;
  Continue;
  ButtonPause.Enabled:= True;
  PlaySound.SoundFile:= SoundInit;
  PlaySound.Execute;
end;

procedure TFrameTimer.Pause;
begin
  TimeCounter.Enabled:= False;
  ButtonPause.Caption:= 'Continue...';
end;

procedure TFrameTimer.Continue;
begin
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
  if IsTime(FWarningTimeMs) then
  begin
    PlaySound.SoundFile:= SoundWarn;
    PlaySound.Execute;
  end
  else
  if (IsTime(initTime) OR IsTime(initTime * 2) OR IsTime(initTime * 3)) then
  begin
    PlaySound.SoundFile:= SoundInit;
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

  { count time and change period }
  if (TimeMsRemaining > 0) then
  begin
    ShowTime(TimeMsRemaining);
    FrameProgressUse.Progress:= FrameProgressUse.MaxProgress - TimeMsRemaining;
  end
  else
  begin
    ShowTime(0);
    PlaySound.SoundFile:= FFinalSound;
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

procedure TFrameTimer.ShowTime(ATimeMs: Comp);
var
  min, sec: Comp;
begin
  sec:= (ATimeMs + 1) div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  FrameProgressUse.Text:= IntToStr(min) + ':' + IntToStr(sec);
end;

procedure TFrameTimer.WritePeriod(AValue: Integer);
begin
  if (AValue < Length(FPeriods)) then
  begin
    FPeriod:= AValue;
    ShowTime(FPeriods[AValue].TimeMs);
    LabelPeriod.Caption:= FPeriods[AValue].Name;
    FSignalColor:= FPeriods[AValue].Color;
    FrameProgressUse.ProgressColor:= FSignalColor;
    FWarningTimeMs:= FPeriods[AValue].WarningTimeMs;
    FPeriodTimeMs:= FPeriodTimeMs + FPeriods[AValue].TimeMs;
    FrameProgressUse.MaxProgress:= FPeriods[AValue].TimeMs;
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

