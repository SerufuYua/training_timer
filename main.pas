unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, XMLPropStorage, Buttons, ChainTimer, Settings, Config, About;

type

  { TFormTTimer }

  TFormTTimer = class(TForm)
    FrameAboutUse: TFrameAbout;
    FrameConfigUse: TFrameConfig;
    FrameSettingsUse: TFrameSettings;
    FrameTimerUse: TFrameTimer;
    ControlPageTimer: TPageControl;
    TabSettings: TTabSheet;
    TabConfig: TTabSheet;
    TabAbout: TTabSheet;
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
    procedure ConfigEvent(Sender: TObject);
    procedure AboutEvent(Sender: TObject);
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
  FrameConfigUse.AboutEvent:= {$ifdef FPC}@{$endif}AboutEvent;
  FrameAboutUse.ReturnEvent:= {$ifdef FPC}@{$endif}ReturnEvent;
  FrameSettingsUse.StartEvent:= {$ifdef FPC}@{$endif}StartEvent;
  FrameSettingsUse.ConfigEvent:= {$ifdef FPC}@{$endif}ConfigEvent;
  FrameSettingsUse.AboutEvent:= {$ifdef FPC}@{$endif}AboutEvent;

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
  TimeCounter.Interval:= TimerInterval;
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
  TimeCounter.Interval:= TimerInterval;
end;

procedure TFormTTimer.ConfigEvent(Sender: TObject);
begin
  ControlPageTimer.ActivePage:= TabConfig;
end;

procedure TFormTTimer.AboutEvent(Sender: TObject);
begin
  ControlPageTimer.ActivePage:= TabAbout;
end;

end.

