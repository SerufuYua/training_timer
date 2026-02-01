unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, XMLPropStorage, Buttons, ChainTimer, Settings, Config;

type

  { TFormTTimer }

  TFormTTimer = class(TForm)
    FrameConfigUse: TFrameConfig;
    FrameSettingsUse: TFrameSettings;
    FrameTimerUse: TFrameTimer;
    ControlPageTimer: TPageControl;
    TabSettings: TTabSheet;
    TabConfig: TTabSheet;
    TabTraining: TTabSheet;
    TimeCounter: TTimer;
    PropStorage: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure PropStorageRestoreProperties(Sender: TObject);
    procedure PropStorageSaveProperties(Sender: TObject);
    procedure TimeCounterTimer(Sender: TObject);
  protected
    procedure SaveSettings;
    procedure LoadSettings;
    procedure StartEvent(ASetName: String; APeriods: TPeriodsList);
    procedure ReturnEvent(Sender: TObject);
  public

  end;

var
  FormTTimer: TFormTTimer;

implementation

{$R *.lfm}

{ TFormTTimer }

procedure TFormTTimer.FormCreate(Sender: TObject);
begin
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}ReturnEvent;
  FrameConfigUse.ReturnEvent:= {$ifdef FPC}@{$endif}ReturnEvent;
  FrameSettingsUse.StartEvent:= {$ifdef FPC}@{$endif}StartEvent;

  {$ifdef RELEASE}
  ControlPageTimer.ShowTabs:= False;
  {$endif}
end;

procedure TFormTTimer.PropStorageRestoreProperties(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFormTTimer.PropStorageSaveProperties(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormTTimer.TimeCounterTimer(Sender: TObject);
begin
  FrameTimerUse.UpdateTime(TimeCounter.Interval);
end;

procedure TFormTTimer.SaveSettings;
begin
  FrameSettingsUse.SaveSettings(PropStorage);
  FrameConfigUse.SaveSettings(PropStorage);
end;

procedure TFormTTimer.LoadSettings;
begin
  FrameSettingsUse.LoadSettings(PropStorage);
  FrameConfigUse.LoadSettings(PropStorage);
  TimeCounter.Interval:= FrameConfigUse.TimerInterval;
end;

procedure TFormTTimer.StartEvent(ASetName: String; APeriods: TPeriodsList);
begin
  FrameTimerUse.Start(ASetName, APeriods);
  ControlPageTimer.ActivePage:= TabTraining;
end;

procedure TFormTTimer.ReturnEvent(Sender: TObject);
begin
  ControlPageTimer.ActivePage:= TabSettings;

  { apply config }
  TimeCounter.Interval:= FrameConfigUse.TimerInterval;
end;

end.

