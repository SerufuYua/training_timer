unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, XMLPropStorage, ChainTimer;

type

  { TFormTTimer }

  TFormTTimer = class(TForm)
    ButtonStart: TButton;
    BoxSettings: TComboBox;
    EditPrepareTimeS: TSpinEdit;
    EditWarningTimeS: TSpinEdit;
    EditRoundTimeS: TSpinEdit;
    EditRestTimeS: TSpinEdit;
    FrameTimerUse: TFrameTimer;
    LabelPrepareTime: TLabel;
    LabelWarningTime: TLabel;
    LabelRoundTime: TLabel;
    LabelRounds: TLabel;
    LabelRestTime: TLabel;
    ControlPageTimer: TPageControl;
    EditRounds: TSpinEdit;
    PanelSettings: TPanel;
    TabSettings: TTabSheet;
    TabTraining: TTabSheet;
    TimerCount: TTimer;
    PropStorage: TXMLPropStorage;
    procedure ButtonStartClick(Sender: TObject);
    procedure PropStorageRestoreProperties(Sender: TObject);
    procedure PropStorageSaveProperties(Sender: TObject);
    procedure TimerCountTimer(Sender: TObject);
  protected
    procedure SaveSettings;
    procedure LoadSettings;
    procedure StopEvent;
  public

  end;

var
  FormTTimer: TFormTTimer;

implementation

type
  TSettingsSimple = record
    Name: String;
    Rounds: Integer;
    RoundTimeMs, RestTimeMs, PrepareTimeMs, WarningTimeMs: Cardinal;
  end;

  TSettingsSimpleList = Array[1..10] of TSettingsSimple;

var
  SettingsSimpleList: TSettingsSimpleList;

{$R *.lfm}

{ TFormTTimer }

procedure TFormTTimer.PropStorageRestoreProperties(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFormTTimer.PropStorageSaveProperties(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormTTimer.ButtonStartClick(Sender: TObject);
begin
  FrameTimerUse.Start(EditRounds.Value,
                      EditRoundTimeS.Value * 1000,
                      EditRestTimeS.Value * 1000,
                      EditPrepareTimeS.Value * 1000,
                      EditWarningTimeS.Value * 1000);
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}StopEvent;
  ControlPageTimer.ActivePage:= TabTraining;
end;

procedure TFormTTimer.TimerCountTimer(Sender: TObject);
begin
  FrameTimerUse.TimeUpdate(TimerCount.Interval);
end;

procedure TFormTTimer.SaveSettings;
var
  i: Integer;
  path: String;
begin
  for i:= 1 to Length(TSettingsSimpleList) do
  begin
    path:= 'Settings/Set' + IntToStr(i) + '/';
    PropStorage.WriteString(path + 'Name', SettingsSimpleList[i].Name);
    PropStorage.WriteInteger(path + 'Rounds', SettingsSimpleList[i].Rounds);
    PropStorage.WriteInteger(path + 'RoundTimeMs', SettingsSimpleList[i].RoundTimeMs);
    PropStorage.WriteInteger(path + 'RestTimeMs', SettingsSimpleList[i].RestTimeMs);
    PropStorage.WriteInteger(path + 'PrepareTimeMs', SettingsSimpleList[i].PrepareTimeMs);
    PropStorage.WriteInteger(path + 'WarningTimeMs', SettingsSimpleList[i].WarningTimeMs);
  end;
end;

procedure TFormTTimer.LoadSettings;
var
  i: Integer;
  path: String;
begin
  for i:= 1 to Length(TSettingsSimpleList) do
  begin
    path:= 'Settings/Set' + IntToStr(i) + '/';
    SettingsSimpleList[i].Name:= PropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i));
    SettingsSimpleList[i].Rounds:= PropStorage.ReadInteger(path + 'Rounds', 2);
    SettingsSimpleList[i].RoundTimeMs:= PropStorage.ReadInteger(path + 'RoundTimeMs', 90000);
    SettingsSimpleList[i].RestTimeMs:= PropStorage.ReadInteger(path + 'RestTimeMs', 60000);
    SettingsSimpleList[i].PrepareTimeMs:= PropStorage.ReadInteger(path + 'PrepareTimeMs', 30000);
    SettingsSimpleList[i].WarningTimeMs:= PropStorage.ReadInteger(path + 'WarningTimeMs', 10000);
  end;
end;

procedure TFormTTimer.StopEvent;
begin
  ControlPageTimer.ActivePage:= TabSettings;
end;

end.

