unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, XMLPropStorage, ChainTimer, EditTime;

type

  { TFormTTimer }

  TFormTTimer = class(TForm)
    ButtonAddSet: TButton;
    ButtonRemoveSet: TButton;
    ButtonSeveSets: TButton;
    ButtonCopySet: TButton;
    ButtonStart: TButton;
    BoxSettings: TComboBox;
    EditName: TEdit;
    EditRoundTimeS: TFrameEditTime;
    EditRestTimeS: TFrameEditTime;
    EditPrepareTimeS: TFrameEditTime;
    EditWarningTimeS: TFrameEditTime;
    FrameTimerUse: TFrameTimer;
    LabelPrepareTime: TLabel;
    LabelName: TLabel;
    LabelWarningTime: TLabel;
    LabelRoundTime: TLabel;
    LabelRounds: TLabel;
    LabelRestTime: TLabel;
    ControlPageTimer: TPageControl;
    EditRounds: TSpinEdit;
    LabelStatistic: TLabel;
    LabelStatisticTime: TLabel;
    PanelSettingsCompose: TPanel;
    PanelSetsControl: TPanel;
    PanelSets: TPanel;
    PanelSettings: TPanel;
    TabSettings: TTabSheet;
    TabTraining: TTabSheet;
    TimeCounter: TTimer;
    PropStorage: TXMLPropStorage;
    procedure BoxSettingsChange(Sender: TObject);
    procedure ButtonSetControlClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditSettingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PropStorageRestoreProperties(Sender: TObject);
    procedure PropStorageSaveProperties(Sender: TObject);
    procedure TimeCounterTimer(Sender: TObject);
  protected
    procedure SaveSettings;
    procedure LoadSettings;
    procedure UpdateSettingsBox;
    procedure ShowStatistic;
    procedure StopEvent;
    procedure WriteSetIndex(AValue: Integer);
    function ReadSetIndex: Integer;
  public
    property SetIndex: Integer read ReadSetIndex write WriteSetIndex;
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

  TSettingsSimpleList = Array of TSettingsSimple;

var
  SettingsSimpleList: TSettingsSimpleList;

const
  DefaultName = 'New Set';
  DefaultRounds = 2;
  DefaultRoundTimeMs = 90000;
  DefaultRestTimeMs = 60000;
  DefaultPrepareTimeMs = 30000;
  DefaultWarningTimeMs = 10000;

{$R *.lfm}

{ TFormTTimer }

procedure TFormTTimer.FormCreate(Sender: TObject);
begin
  EditRoundTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditRestTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditPrepareTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditWarningTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
end;

procedure TFormTTimer.PropStorageRestoreProperties(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFormTTimer.PropStorageSaveProperties(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormTTimer.ButtonStartClick(Sender: TObject);
var
 i, lastPeriod: Integer;
 periods: TPeriodsList;
begin
  periods:= [];
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}StopEvent;

  { prepare periods list }
  SetLength(periods, EditRounds.Value * 2);

  periods[0].Name:= 'Prepare';
  periods[0].FinalSound:= SoundStart;
  periods[0].TimeMs:= EditPrepareTimeS.Value * 1000;
  periods[0].WarningTimeMs:= EditWarningTimeS.Value * 1000;
  periods[0].Color:= clLime;

  lastPeriod:= EditRounds.Value * 2 - 1;

  for i:= 1 to lastPeriod do
  begin
    if ((i mod 2) = 0) then
    begin
      periods[i].Name:= 'Rest';
      periods[i].TimeMs:= EditRestTimeS.Value * 1000;
      periods[i].FinalSound:= SoundStart;
      periods[i].Color:= clYellow;
    end
    else
    begin
      periods[i].Name:= 'Round ' + IntToStr((i div 2) + 1);
      periods[i].TimeMs:= EditRoundTimeS.Value * 1000;
      periods[i].WarningTimeMs:= EditWarningTimeS.Value * 1000;
      periods[i].Color:= clRed;
      if (i = lastPeriod) then
        periods[i].FinalSound:= SoundFinal
      else
        periods[i].FinalSound:= SoundEnd;
    end;
  end;

  { start timer }
  FrameTimerUse.Start(EditName.Caption, periods);
  ControlPageTimer.ActivePage:= TabTraining;
end;

procedure TFormTTimer.EditSettingChange(Sender: TObject);
var
  component: TComponent;
  editStr: TEdit;
  editNum: TSpinEdit;
  editTime: TFrameEditTime;
begin
  if ((NOT (Sender is TComponent)) OR (SetIndex < 0)) then Exit;

  component:= Sender as TComponent;

  case component.Name of
    'EditName':
    begin
      editStr:= component as TEdit;
      SettingsSimpleList[SetIndex].Name:= editStr.Caption;
      BoxSettings.Items[SetIndex]:= editStr.Caption;
      SettingsSimpleList[SetIndex].Name:= editStr.Caption;
    end;
    'EditRounds':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[SetIndex].Rounds:= editNum.Value;
    end;
    'EditRoundTimeS':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[SetIndex].RoundTimeMs:= editTime.Value * 1000;
    end;
    'EditRestTimeS':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[SetIndex].RestTimeMs:= editTime.Value * 1000;
    end;
    'EditPrepareTimeS':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[SetIndex].PrepareTimeMs:= editTime.Value * 1000;
    end;
    'EditWarningTimeS':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[SetIndex].WarningTimeMs:= editTime.Value * 1000;
    end;
  end;

  ShowStatistic;
end;

procedure TFormTTimer.BoxSettingsChange(Sender: TObject);
begin
  if (SetIndex < 0) then Exit;

  EditName.Caption:= SettingsSimpleList[SetIndex].Name;
  EditRounds.Value:= SettingsSimpleList[SetIndex].Rounds;
  EditRoundTimeS.Value:= SettingsSimpleList[SetIndex].RoundTimeMs div 1000;
  EditRestTimeS.Value:= SettingsSimpleList[SetIndex].RestTimeMs div 1000;
  EditPrepareTimeS.Value:= SettingsSimpleList[SetIndex].PrepareTimeMs div 1000;
  EditWarningTimeS.Value:= SettingsSimpleList[SetIndex].WarningTimeMs div 1000;

  ShowStatistic;
end;

procedure TFormTTimer.ButtonSetControlClick(Sender: TObject);
var
  component: TComponent;
  idx: Integer;
begin
  if (NOT (Sender is TComponent)) then Exit;

  component:= Sender as TComponent;

  case component.Name of
    'ButtonAddSet':
    begin
      SetLength(SettingsSimpleList, (Length(SettingsSimpleList) + 1));
      idx:= High(SettingsSimpleList);
      SettingsSimpleList[idx].Name:= DefaultName;
      SettingsSimpleList[idx].Rounds:= DefaultRounds;
      SettingsSimpleList[idx].RoundTimeMs:= DefaultRoundTimeMs;
      SettingsSimpleList[idx].RestTimeMs:= DefaultRestTimeMs;
      SettingsSimpleList[idx].PrepareTimeMs:= DefaultPrepareTimeMs;
      SettingsSimpleList[idx].WarningTimeMs:= DefaultWarningTimeMs;

      UpdateSettingsBox;
      SetIndex:= idx;
    end;
    'ButtonRemoveSet':
    begin
      if (Length(SettingsSimpleList) > 1) then
      begin
        idx:= SetIndex;
        BoxSettings.Items.Delete(idx);
        Delete(SettingsSimpleList, idx, 1);
        SetIndex:= 0;
      end;
    end;
    'ButtonCopySet':
    begin
      if ((Length(SettingsSimpleList) > 0) AND (SetIndex > -1)) then
      begin
        SetLength(SettingsSimpleList, (Length(SettingsSimpleList) + 1));
        idx:= High(SettingsSimpleList);
        SettingsSimpleList[idx]:= SettingsSimpleList[SetIndex];
        SettingsSimpleList[idx].Name:= SettingsSimpleList[idx].Name + ' Copy';

        UpdateSettingsBox;
        SetIndex:= idx;
      end;
    end;
    'ButtonSeveSets':
    begin
      PropStorage.Save;
    end;
  end;
end;

procedure TFormTTimer.TimeCounterTimer(Sender: TObject);
begin
  FrameTimerUse.UpdateTime(TimeCounter.Interval);
end;

procedure TFormTTimer.SaveSettings;
var
  i, num: Integer;
  path: String;
begin
  PropStorage.DoEraseSections(PropStorage.RootNodePath + '/Settings');

  num:= Length(SettingsSimpleList);
  PropStorage.WriteInteger('Settings/SetCount', num);

  for i:= 0 to (num - 1) do
  begin
    path:= 'Settings/Set' + IntToStr(i + 1) + '/';
    PropStorage.WriteString(path + 'Name', SettingsSimpleList[i].Name);
    PropStorage.WriteInteger(path + 'Rounds', SettingsSimpleList[i].Rounds);
    PropStorage.WriteInteger(path + 'RoundTimeMs', SettingsSimpleList[i].RoundTimeMs);
    PropStorage.WriteInteger(path + 'RestTimeMs', SettingsSimpleList[i].RestTimeMs);
    PropStorage.WriteInteger(path + 'PrepareTimeMs', SettingsSimpleList[i].PrepareTimeMs);
    PropStorage.WriteInteger(path + 'WarningTimeMs', SettingsSimpleList[i].WarningTimeMs);
  end;

  PropStorage.WriteInteger('Settings/SetNum', SetIndex);
end;

procedure TFormTTimer.LoadSettings;
var
  i, num: Integer;
  path: String;
begin
  num:= PropStorage.ReadInteger('Settings/SetCount', 0);

  if (num = 0) then
  begin
    SetLength(SettingsSimpleList, 1);
    SettingsSimpleList[0].Name:= DefaultName;
    SettingsSimpleList[0].Rounds:= DefaultRounds;
    SettingsSimpleList[0].RoundTimeMs:= DefaultRoundTimeMs;
    SettingsSimpleList[0].RestTimeMs:= DefaultRestTimeMs;
    SettingsSimpleList[0].PrepareTimeMs:= DefaultPrepareTimeMs;
    SettingsSimpleList[0].WarningTimeMs:= DefaultWarningTimeMs;

    UpdateSettingsBox;
    SetIndex:= 0;
  end
  else
  begin
    SetLength(SettingsSimpleList, num);

    for i:= 0 to (num - 1) do
    begin
      path:= 'Settings/Set' + IntToStr(i + 1) + '/';
      SettingsSimpleList[i].Name:= PropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i + 1));
      SettingsSimpleList[i].Rounds:= PropStorage.ReadInteger(path + 'Rounds', DefaultRounds);
      SettingsSimpleList[i].RoundTimeMs:= PropStorage.ReadInteger(path + 'RoundTimeMs', DefaultRoundTimeMs);
      SettingsSimpleList[i].RestTimeMs:= PropStorage.ReadInteger(path + 'RestTimeMs', DefaultRestTimeMs);
      SettingsSimpleList[i].PrepareTimeMs:= PropStorage.ReadInteger(path + 'PrepareTimeMs', DefaultPrepareTimeMs);
      SettingsSimpleList[i].WarningTimeMs:= PropStorage.ReadInteger(path + 'WarningTimeMs', DefaultWarningTimeMs);
    end;

    UpdateSettingsBox;
    SetIndex:= PropStorage.ReadInteger('Settings/SetNum', 0);
  end;
end;

procedure TFormTTimer.UpdateSettingsBox;
var
  i: Integer;
begin
  BoxSettings.Clear;

  for i:= 0 to (Length(SettingsSimpleList) - 1) do
  begin
    BoxSettings.Items.Add(SettingsSimpleList[i].Name);
  end;
end;

procedure TFormTTimer.ShowStatistic;
var
  min, sec: Cardinal;
begin
  sec:= EditPrepareTimeS.Value +
        EditRestTimeS.Value * (EditRounds.Value - 1) +
        EditRoundTimeS.Value * EditRounds.Value;
  min:= sec div 60;
  sec:= sec - (min * 60);

  LabelStatisticTime.Caption:= IntToStr(min) + ' m  ' + IntToStr(sec) + ' s';
end;

procedure TFormTTimer.StopEvent;
begin
  ControlPageTimer.ActivePage:= TabSettings;
end;

procedure TFormTTimer.WriteSetIndex(AValue: Integer);
begin
  BoxSettings.ItemIndex:= AValue;
  BoxSettingsChange(nil);
end;

function TFormTTimer.ReadSetIndex: Integer;
begin
  Result:= BoxSettings.ItemIndex;
end;

end.

