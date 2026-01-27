unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, uplaysound;

type

  TStopEvent = procedure of object;

  { TFrameTimer }

  TFrameTimer = class(TFrame)
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
    procedure ButtonStopClick(Sender: TObject);
    procedure LabelTimeResize(Sender: TObject);
  protected
    TimerEnable: Boolean;
    FStopEvent: TStopEvent;
    FPeriod: Integer;
    FPeriodTimeMs, FWarningTimeMs: Cardinal;
    FFinalSound: String;
    FSignalColor: TColor;
    procedure ResetTimer;
    procedure ShowTime(ATimeMs: Cardinal);
    procedure WritePeriod(AValue: Integer);
  public
    procedure UpdateTime(ATimeMsElapsed: Cardinal);
    procedure Start(ASetName: String; APeriods: Integer; ARoundTimeMs, ARestTimeMs, APrepareTimeMs, AWarningTimeMs: Cardinal);
    property Period: Integer read FPeriod write WritePeriod;
    property StopEvent: TStopEvent write FStopEvent;
  end;

implementation

type
  TTimePeriod = record
    Name, FinalSound: String;
    TimeMs, WarningTimeMs: Cardinal;
    Color: TColor;
  end;

  TPeriodsList = Array of TTimePeriod;

var
 Periods: TPeriodsList;

const
  SoundStart = 'data\sound\start.wav';
  SoundEnd = 'data\sound\end.wav';
  SoundFinal = 'data\sound\fin.wav';
  SoundWarn = 'data\sound\warn.wav';
  SoundInit = 'data\sound\init.wav';

{$R *.lfm}

{ TFrameTimer }

procedure TFrameTimer.Start(ASetName: String; APeriods: Integer; ARoundTimeMs, ARestTimeMs, APrepareTimeMs, AWarningTimeMs: Cardinal);
var
 i, lastPeriod: Integer;
begin
  TimerEnable:= False;

  LabelSet.Caption:= ASetName;

  { prepare periods list }
  SetLength(Periods, APeriods * 2);

  Periods[0].Name:= 'Prepare';
  Periods[0].FinalSound:= SoundStart;
  Periods[0].TimeMs:= APrepareTimeMs;
  Periods[0].WarningTimeMs:= AWarningTimeMs;
  Periods[0].Color:= clLime;

  lastPeriod:= APeriods * 2 - 1;

  for i:= 1 to lastPeriod do
  begin
    if ((i mod 2) = 0) then
    begin
      Periods[i].Name:= 'Rest';
      Periods[i].TimeMs:= ARestTimeMs;
      Periods[i].FinalSound:= SoundStart;
      Periods[i].Color:= clYellow;
    end
    else
    begin
      Periods[i].Name:= 'Round ' + IntToStr((i div 2) + 1);
      Periods[i].TimeMs:= ARoundTimeMs;
      Periods[i].WarningTimeMs:= AWarningTimeMs;
      Periods[i].Color:= clRed;
      if (i = lastPeriod) then
        Periods[i].FinalSound:= SoundFinal
      else
        Periods[i].FinalSound:= SoundEnd;
    end;
  end;

  { enable timer }
  ResetTimer;
  TimerEnable:= True;
end;

procedure TFrameTimer.ResetTimer;
begin
  Period:= 0;
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
  if (NOT TimerEnable) then Exit;

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
  if TimerEnable then
  begin
    TimerEnable:= False;
    ButtonPause.Caption:= 'Continue';
  end
  else
  begin
    TimerEnable:= True;
    ButtonPause.Caption:= 'Pause';
  end;

end;

procedure TFrameTimer.ButtonStopClick(Sender: TObject);
begin
  TimerEnable:= False;
  if Assigned(FStopEvent) then
    FStopEvent();
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
  if (AValue < Length(Periods)) then
  begin
    FPeriod:= AValue;
    ShowTime(Periods[AValue].TimeMs);
    LabelPeriod.Caption:= Periods[AValue].Name;
    FSignalColor:= Periods[AValue].Color;
    ShapeSignal.Brush.Color:= FSignalColor;
    FWarningTimeMs:= Periods[AValue].WarningTimeMs;
    FPeriodTimeMs:= Periods[AValue].TimeMs;
    FFinalSound:= Periods[AValue].FinalSound;
  end
  else
    TimerEnable:= False;
end;

end.

