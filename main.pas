unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, ExtCtrls, XMLPropStorage, Buttons, ChainTimer, EditTime, Settings;

type

  { TFormTTimer }

  TFormTTimer = class(TForm)
    FrameSettingsUse: TFrameSettings;
    FrameTimerUse: TFrameTimer;
    ControlPageTimer: TPageControl;
    TabSettings: TTabSheet;
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
    procedure StopEvent;
  public

  end;

var
  FormTTimer: TFormTTimer;

implementation

{$R *.lfm}

{ TFormTTimer }

procedure TFormTTimer.FormCreate(Sender: TObject);
begin
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}StopEvent;
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
end;

procedure TFormTTimer.LoadSettings;
begin
  FrameSettingsUse.LoadSettings(PropStorage);
end;

procedure TFormTTimer.StartEvent(ASetName: String; APeriods: TPeriodsList);
begin
  FrameTimerUse.Start(ASetName, APeriods);
  ControlPageTimer.ActivePage:= TabTraining;
end;

procedure TFormTTimer.StopEvent;
begin
  ControlPageTimer.ActivePage:= TabSettings;
end;

end.

