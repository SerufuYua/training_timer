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
    EditName: TEdit;
    EditPrepareTimeS: TSpinEdit;
    EditWarningTimeS: TSpinEdit;
    EditRoundTimeS: TSpinEdit;
    EditRestTimeS: TSpinEdit;
    FrameTimerUse: TFrameTimer;
    LabelPrepareTime: TLabel;
    LabelName: TLabel;
    LabelWarningTime: TLabel;
    LabelRoundTime: TLabel;
    LabelRounds: TLabel;
    LabelRestTime: TLabel;
    ControlPageTimer: TPageControl;
    EditRounds: TSpinEdit;
    PanelSettings: TPanel;
    TabSettings: TTabSheet;
    TabTraining: TTabSheet;
    TimeCounter: TTimer;
    PropStorage: TXMLPropStorage;
    procedure BoxSettingsChange(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditSettingChange(Sender: TObject);
    procedure PropStorageRestoreProperties(Sender: TObject);
    procedure PropStorageSaveProperties(Sender: TObject);
    procedure TimeCounterTimer(Sender: TObject);
  protected
    procedure SaveSettings;
    procedure LoadSettings;
    procedure UpdateSettingsBox;
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

  TSettingsSimpleList = Array[0..9] of TSettingsSimple;

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
  FrameTimerUse.Start(EditName.Caption,
                      EditRounds.Value,
                      EditRoundTimeS.Value * 1000,
                      EditRestTimeS.Value * 1000,
                      EditPrepareTimeS.Value * 1000,
                      EditWarningTimeS.Value * 1000);
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}StopEvent;
  ControlPageTimer.ActivePage:= TabTraining;
end;

procedure TFormTTimer.EditSettingChange(Sender: TObject);
var
  component: TComponent;
  editStr: TEdit;
  editNum: TSpinEdit;
begin
  if ((NOT (Sender is TComponent)) OR (BoxSettings.ItemIndex < 0)) then Exit;

  component:= Sender as TComponent;

  case component.Name of
    'EditName':
    begin
      editStr:= component as TEdit;
      SettingsSimpleList[BoxSettings.ItemIndex].Name:= editStr.Caption;
      BoxSettings.Items[BoxSettings.ItemIndex]:= editStr.Caption;
      SettingsSimpleList[BoxSettings.ItemIndex].Name:= editStr.Caption;
    end;
    'EditRounds':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[BoxSettings.ItemIndex].Rounds:= editNum.Value;
    end;
    'EditRoundTimeS':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[BoxSettings.ItemIndex].RoundTimeMs:= editNum.Value * 1000;
    end;
    'EditRestTimeS':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[BoxSettings.ItemIndex].RestTimeMs:= editNum.Value * 1000;
    end;
    'EditPrepareTimeS':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[BoxSettings.ItemIndex].PrepareTimeMs:= editNum.Value * 1000;
    end;
    'EditWarningTimeS':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[BoxSettings.ItemIndex].WarningTimeMs:= editNum.Value * 1000;
    end;
  end;
end;

procedure TFormTTimer.BoxSettingsChange(Sender: TObject);
begin
  if (BoxSettings.ItemIndex < 0) then Exit;

  EditName.Caption:= SettingsSimpleList[BoxSettings.ItemIndex].Name;
  EditRounds.Value:= SettingsSimpleList[BoxSettings.ItemIndex].Rounds;
  EditRoundTimeS.Value:= SettingsSimpleList[BoxSettings.ItemIndex].RoundTimeMs div 1000;
  EditRestTimeS.Value:= SettingsSimpleList[BoxSettings.ItemIndex].RestTimeMs div 1000;
  EditPrepareTimeS.Value:= SettingsSimpleList[BoxSettings.ItemIndex].PrepareTimeMs div 1000;
  EditWarningTimeS.Value:= SettingsSimpleList[BoxSettings.ItemIndex].WarningTimeMs div 1000;
end;

procedure TFormTTimer.TimeCounterTimer(Sender: TObject);
begin
  FrameTimerUse.UpdateTime(TimeCounter.Interval);
end;

procedure TFormTTimer.SaveSettings;
var
  i: Integer;
  path: String;
begin
  for i:= 0 to (Length(TSettingsSimpleList) - 1) do
  begin
    path:= 'Settings/Set' + IntToStr(i + 1) + '/';
    PropStorage.WriteString(path + 'Name', SettingsSimpleList[i].Name);
    PropStorage.WriteInteger(path + 'Rounds', SettingsSimpleList[i].Rounds);
    PropStorage.WriteInteger(path + 'RoundTimeMs', SettingsSimpleList[i].RoundTimeMs);
    PropStorage.WriteInteger(path + 'RestTimeMs', SettingsSimpleList[i].RestTimeMs);
    PropStorage.WriteInteger(path + 'PrepareTimeMs', SettingsSimpleList[i].PrepareTimeMs);
    PropStorage.WriteInteger(path + 'WarningTimeMs', SettingsSimpleList[i].WarningTimeMs);
  end;

  PropStorage.WriteInteger('Settings/SetNum', BoxSettings.ItemIndex);
end;

procedure TFormTTimer.LoadSettings;
var
  i: Integer;
  path: String;
begin
  for i:= 0 to (Length(TSettingsSimpleList) - 1) do
  begin
    path:= 'Settings/Set' + IntToStr(i + 1) + '/';
    SettingsSimpleList[i].Name:= PropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i + 1));
    SettingsSimpleList[i].Rounds:= PropStorage.ReadInteger(path + 'Rounds', 2);
    SettingsSimpleList[i].RoundTimeMs:= PropStorage.ReadInteger(path + 'RoundTimeMs', 90000);
    SettingsSimpleList[i].RestTimeMs:= PropStorage.ReadInteger(path + 'RestTimeMs', 60000);
    SettingsSimpleList[i].PrepareTimeMs:= PropStorage.ReadInteger(path + 'PrepareTimeMs', 30000);
    SettingsSimpleList[i].WarningTimeMs:= PropStorage.ReadInteger(path + 'WarningTimeMs', 10000);
  end;

  UpdateSettingsBox;
  BoxSettings.ItemIndex:= PropStorage.ReadInteger('Settings/SetNum', 0);
  BoxSettingsChange(nil);
end;

procedure TFormTTimer.UpdateSettingsBox;
var
  i: Integer;
begin
  BoxSettings.Clear;

  for i:= 0 to (Length(TSettingsSimpleList) - 1) do
  begin
    BoxSettings.Items.Add(SettingsSimpleList[i].Name);
  end;
end;

procedure TFormTTimer.StopEvent;
begin
  ControlPageTimer.ActivePage:= TabSettings;
end;

end.

