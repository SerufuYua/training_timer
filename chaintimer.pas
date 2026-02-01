unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, uplaysound;

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
    PanelTime: TPanel;
    PanelLabels: TPanel;
    PanelCounter: TPanel;
    PanelControl: TPanel;
    PlaySound: Tplaysound;
    ShapeSignal: TShape;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRestartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure LabelTimeResize(Sender: TObject);
  protected
    FPeriods: TPeriodsList;
    FTimerEnable: Boolean;
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
  FTimerEnable:= False;
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
  FTimerEnable:= False;
  ButtonPause.Caption:= 'Continue...';
end;

procedure TFrameTimer.Continue;
begin
  FTimerEnable:= True;
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
  if (NOT FTimerEnable) then Exit;

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
    ShapeSignal.Brush.Color:= clBlack;
  end
  else
  if (IsTime(initTime * 1 - (initTime div 2)) OR
      IsTime(initTime * 2 - (initTime div 2)) OR
      IsTime(initTime * 3 - (initTime div 2))) then
  begin
    ShapeSignal.Brush.Color:= FSignalColor;
  end;

  { color blink warning signal }
  if IsTime(FWarningTimeMs) then
  begin
    ShapeSignal.Brush.Color:= clGray;
  end
  else
  if ((FWarningTimeMs > initTime) AND
      (IsTime(FWarningTimeMs - (initTime div 2)))) then
  begin
    ShapeSignal.Brush.Color:= FSignalColor;
  end;

  { count time and change period }
  if (FPeriodTimeMs > ATimeMsElapsed) then
  begin
    FPeriodTimeMs:= FPeriodTimeMs - ATimeMsElapsed;
    ShowTime(FPeriodTimeMs);
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
  if FTimerEnable then
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
  FTimerEnable:= False;
  if Assigned(FStopEvent) then
    FStopEvent(self);
end;

procedure TFrameTimer.LabelTimeResize(Sender: TObject);
begin
  LabelTime.Font.Height:= LabelTime.Height;
end;

procedure TFrameTimer.ShowTime(ATimeMs: Cardinal);
var
  min, sec: Cardinal;
begin
  sec:= (ATimeMs + 1) div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  LabelTime.Caption:= IntToStr(min) + ':' + IntToStr(sec);
end;

procedure TFrameTimer.WritePeriod(AValue: Integer);
begin
  if (AValue < Length(FPeriods)) then
  begin
    FPeriod:= AValue;
    ShowTime(FPeriods[AValue].TimeMs);
    LabelPeriod.Caption:= FPeriods[AValue].Name;
    FSignalColor:= FPeriods[AValue].Color;
    ShapeSignal.Brush.Color:= FSignalColor;
    FWarningTimeMs:= FPeriods[AValue].WarningTimeMs;
    FPeriodTimeMs:= FPeriods[AValue].TimeMs;
    FFinalSound:= FPeriods[AValue].FinalSound;
  end
  else
  begin
    FTimerEnable:= False;
    ShapeSignal.Brush.Color:= clBlack;
    ButtonPause.Enabled:= False;
  end;
end;

end.

