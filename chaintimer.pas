unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, uplaysound,
  meProgressBarEx;

type

  { TFrameTimer }

  TTimePeriod = record
    Name, FinalSound: String;
    TimeMs, WarningTimeMs: Cardinal;
    Color: TColor;
  end;

  TPeriodsList = Array of TTimePeriod;

  TFrameTimer = class(TFrame)
    ButtonRestart: TButton;
    ButtonStop: TButton;
    ButtonPause: TButton;
    LabelTime: TLabel;
    LabelPeriod: TLabel;
    LabelSet: TLabel;
    ProgressBar: TmeProgressBarEx;
    PanelTime: TPanel;
    PanelLabels: TPanel;
    PanelCounter: TPanel;
    PanelControl: TPanel;
    PlaySound: Tplaysound;
    TimeCounter: TTimer;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRestartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure LabelTimeResize(Sender: TObject);
    procedure TimeCounterTimer(Sender: TObject);
  protected
    FPeriods: TPeriodsList;
    FStopEvent: TNotifyEvent;
    FPeriod: Integer;
    FPeriodTimeMs, FWarningTimeMs: Cardinal;
    FFinalSound: String;
    FSignalColor: TColor;
    procedure ResetTimer;
    procedure Pause;
    procedure Continue;
    procedure ShowTime(ATimeMs: Cardinal);
    procedure WritePeriod(AValue: Integer);
  public
    procedure UpdateTime(ATimeMsElapsed: Cardinal);
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

procedure TFrameTimer.UpdateTime(ATimeMsElapsed: Cardinal);
const
  initTime = 1000;

function IsTime(thisTime: Cardinal): Boolean; inline;
begin
  Result:= ((FPeriodTimeMs >= thisTime) AND
            ((FPeriodTimeMs - ATimeMsElapsed) < thisTime));
end;

begin
  { warning and initial signals }
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
    ProgressBar.ProgressColor:= clBlack;
  end
  else
  if (IsTime(initTime * 1 - (initTime div 2)) OR
      IsTime(initTime * 2 - (initTime div 2)) OR
      IsTime(initTime * 3 - (initTime div 2))) then
  begin
    ProgressBar.ProgressColor:= FSignalColor;
  end;

  { color blink warning signal }
  if IsTime(FWarningTimeMs) then
  begin
    ProgressBar.ProgressColor:= clGray;
  end
  else
  if ((FWarningTimeMs > initTime) AND
      (IsTime(FWarningTimeMs - (initTime div 2)))) then
  begin
    ProgressBar.ProgressColor:= FSignalColor;
  end;

  { count time and change period }
  if (FPeriodTimeMs > ATimeMsElapsed) then
  begin
    FPeriodTimeMs:= FPeriodTimeMs - ATimeMsElapsed;
    ShowTime(FPeriodTimeMs);
    ProgressBar.Progress:= ProgressBar.Max - FPeriodTimeMs;
  end
  else
  begin
    ShowTime(0);
    PlaySound.SoundFile:= FFinalSound;
    PlaySound.Execute;
    Period:= Period + 1;
  end;
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

procedure TFrameTimer.LabelTimeResize(Sender: TObject);
begin
  LabelTime.Font.Height:= LabelTime.Height;
end;

procedure TFrameTimer.TimeCounterTimer(Sender: TObject);
begin
  UpdateTime(TimeCounter.Interval);
end;

procedure TFrameTimer.ShowTime(ATimeMs: Cardinal);
var
  min, sec: Cardinal;
begin
  sec:= (ATimeMs + 1) div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  LabelTime.Caption:= IntToStr(min) + ':' + IntToStr(sec);

  ProgressBar.TextFormat:= IntToStr(min) + ':' + IntToStr(sec);
end;

procedure TFrameTimer.WritePeriod(AValue: Integer);
begin
  if (AValue < Length(FPeriods)) then
  begin
    FPeriod:= AValue;
    ShowTime(FPeriods[AValue].TimeMs);
    LabelPeriod.Caption:= FPeriods[AValue].Name;
    FSignalColor:= FPeriods[AValue].Color;
    ProgressBar.ProgressColor:= FSignalColor;
    FWarningTimeMs:= FPeriods[AValue].WarningTimeMs;
    FPeriodTimeMs:= FPeriods[AValue].TimeMs;
    ProgressBar.Max:= FPeriods[AValue].TimeMs;
    ProgressBar.Progress:= 0;
    FFinalSound:= FPeriods[AValue].FinalSound;
  end
  else
  begin
    TimeCounter.Enabled:= False;
    ProgressBar.ProgressColor:= clBlack;
    ButtonPause.Enabled:= False;
  end;
end;

end.

