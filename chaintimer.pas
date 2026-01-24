unit ChainTimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, uplaysound;

type

  TStopEvent = procedure of object;

  { TFrameTimer }

  TFrameTimer = class(TFrame)
    ButtonStop: TButton;
    ButtonPause: TButton;
    LabelTime: TLabel;
    LabelTitle: TLabel;
    PanelCounter: TPanel;
    PanelControl: TPanel;
    PlaySound: Tplaysound;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure LabelTimeResize(Sender: TObject);
  protected
    TimerEnable: Boolean;
    FStopEvent: TStopEvent;
    FPeriod: Integer;
    FWarningTime: Cardinal;
    procedure ResetTimer;
    procedure ShowTime(ATimeMs: Cardinal);
    procedure SetPeriod(AValue: Integer);
  public
    procedure Update(ATimeMsElapsed: Cardinal);
    procedure Start(APeriods: Integer; ARoundTime, ARestTime, APrepareTime, AWarningTime: Cardinal);
    property Period: Integer read FPeriod write SetPeriod;
    property StopEvent: TStopEvent write FStopEvent;
  end;

implementation

type
  TTimePeriod = record
    Name, FinalSound: String;
    TimeMs: Integer;
  end;

  TPeriodsList = Array of TTimePeriod;

var
 Periods: TPeriodsList;

const
  SoundStart = 'data\sound\start.wav';
  SoundEnd = 'end.wav';
  SoundFinal = 'fin.wav';
  SoundWarn = 'warn.wav';

{$R *.lfm}

{ TFrameTimer }

procedure TFrameTimer.Start(APeriods: Integer; ARoundTime, ARestTime, APrepareTime, AWarningTime: Cardinal);
var
 i, lastPeriod: Integer;
begin
  TimerEnable:= False;

  { prepare periods list }
  FWarningTime:= AWarningTime;
  SetLength(Periods, APeriods * 2);

  Periods[0].Name:= 'Prepare';
  Periods[0].FinalSound:= SoundStart;
  Periods[0].TimeMs:= APrepareTime;

  lastPeriod:= APeriods * 2 - 1;

  for i:= 1 to lastPeriod do
  begin
    if ((i mod 2) = 0) then
    begin
      Periods[i].Name:= 'Rest';
      Periods[i].TimeMs:= ARestTime;
      Periods[i].FinalSound:= SoundStart;
    end
    else
    begin
      Periods[i].Name:= 'Round ' + IntToStr((i div 2) + 1);
      Periods[i].TimeMs:= ARoundTime;
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

procedure TFrameTimer.Update(ATimeMsElapsed: Cardinal);
begin
  if (NOT TimerEnable) then Exit;

  if (Periods[Period].TimeMs > ATimeMsElapsed) then
  begin
    Periods[Period].TimeMs:= Periods[Period].TimeMs - ATimeMsElapsed;
    ShowTime(Periods[Period].TimeMs);
  end
  else
  begin
    ShowTime(0);
    PlaySound.SoundFile:= Periods[Period].FinalSound;
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
  sec:= ATimeMs div 1000;
  min:= sec div 60;
  sec:= sec - (min * 60);
  LabelTime.Caption:= IntToStr(min) + ':' + IntToStr(sec);
end;

procedure TFrameTimer.SetPeriod(AValue: Integer);
begin
  if (AValue < Length(Periods)) then
  begin
    FPeriod:= AValue;
    ShowTime(Periods[AValue].TimeMs);
    LabelTitle.Caption:= Periods[AValue].Name;
  end
  else
    TimerEnable:= False;
end;

end.

