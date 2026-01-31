unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  EditTime, XMLPropStorage, ChainTimer;

type

  TStartEvent = procedure(ASetName: String; APeriods: TPeriodsList) of object;

  { TFrameSettings }

  TFrameSettings = class(TFrame)
    BoxSettings: TComboBox;
    ButtonAddSet: TButton;
    ButtonConfig: TBitBtn;
    ButtonCopySet: TButton;
    ButtonRemoveSet: TButton;
    ButtonStart: TButton;
    EditMin: TSpinEdit;
    EditMin1: TSpinEdit;
    EditMin2: TSpinEdit;
    EditMin3: TSpinEdit;
    EditName: TEdit;
    EditPrepareTimeS: TFrameEditTime;
    EditRestTimeS: TFrameEditTime;
    EditRounds: TSpinEdit;
    EditRoundTimeS: TFrameEditTime;
    EditSec: TSpinEdit;
    EditSec1: TSpinEdit;
    EditSec2: TSpinEdit;
    EditSec3: TSpinEdit;
    EditWarningTimeS: TFrameEditTime;
    LabelMin: TLabel;
    LabelMin1: TLabel;
    LabelMin2: TLabel;
    LabelMin3: TLabel;
    LabelName: TLabel;
    LabelPrepareTime: TLabel;
    LabelRestTime: TLabel;
    LabelRounds: TLabel;
    LabelRoundTime: TLabel;
    LabelSec: TLabel;
    LabelSec1: TLabel;
    LabelSec2: TLabel;
    LabelSec3: TLabel;
    LabelStatistic: TLabel;
    LabelStatisticTime: TLabel;
    LabelWarningTime: TLabel;
    PanelButtons: TPanel;
    PanelMin: TPanel;
    PanelMin1: TPanel;
    PanelMin2: TPanel;
    PanelMin3: TPanel;
    PanelSec: TPanel;
    PanelSec1: TPanel;
    PanelSec2: TPanel;
    PanelSec3: TPanel;
    PanelSets: TPanel;
    PanelSetsControl: TPanel;
    PanelSettings: TPanel;
    PanelSettingsCompose: TPanel;
    procedure BoxSettingsChange(Sender: TObject);
    procedure ButtonSetControlClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditSettingChange(Sender: TObject);
  protected
    FStartEvent: TStartEvent;
    procedure UpdateSettingsBox;
    procedure ShowStatistic;
    procedure WriteSetIndex(AValue: Integer);
    function ReadSetIndex: Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property SetIndex: Integer read ReadSetIndex write WriteSetIndex;
    property StartEvent: TStartEvent write FStartEvent;
  end;

implementation

uses
  Graphics;

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

constructor TFrameSettings.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  EditRoundTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditRestTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditPrepareTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditWarningTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
end;

procedure TFrameSettings.LoadSettings(APropStorage: TXMLPropStorage);
var
  i, num: Integer;
  path: String;
begin
  num:= APropStorage.ReadInteger('Settings/SetCount', 0);

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
      SettingsSimpleList[i].Name:= APropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i + 1));
      SettingsSimpleList[i].Rounds:= APropStorage.ReadInteger(path + 'Rounds', DefaultRounds);
      SettingsSimpleList[i].RoundTimeMs:= APropStorage.ReadInteger(path + 'RoundTimeMs', DefaultRoundTimeMs);
      SettingsSimpleList[i].RestTimeMs:= APropStorage.ReadInteger(path + 'RestTimeMs', DefaultRestTimeMs);
      SettingsSimpleList[i].PrepareTimeMs:= APropStorage.ReadInteger(path + 'PrepareTimeMs', DefaultPrepareTimeMs);
      SettingsSimpleList[i].WarningTimeMs:= APropStorage.ReadInteger(path + 'WarningTimeMs', DefaultWarningTimeMs);
    end;

    UpdateSettingsBox;
    SetIndex:= APropStorage.ReadInteger('Settings/SetNum', 0);
  end;
end;

procedure TFrameSettings.SaveSettings(APropStorage: TXMLPropStorage);
var
  i, num: Integer;
  path: String;
begin
  APropStorage.DoEraseSections(APropStorage.RootNodePath + '/Settings');

  num:= Length(SettingsSimpleList);
  APropStorage.WriteInteger('Settings/SetCount', num);

  for i:= 0 to (num - 1) do
  begin
    path:= 'Settings/Set' + IntToStr(i + 1) + '/';
    APropStorage.WriteString(path + 'Name', SettingsSimpleList[i].Name);
    APropStorage.WriteInteger(path + 'Rounds', SettingsSimpleList[i].Rounds);
    APropStorage.WriteInteger(path + 'RoundTimeMs', SettingsSimpleList[i].RoundTimeMs);
    APropStorage.WriteInteger(path + 'RestTimeMs', SettingsSimpleList[i].RestTimeMs);
    APropStorage.WriteInteger(path + 'PrepareTimeMs', SettingsSimpleList[i].PrepareTimeMs);
    APropStorage.WriteInteger(path + 'WarningTimeMs', SettingsSimpleList[i].WarningTimeMs);
  end;

  APropStorage.WriteInteger('Settings/SetNum', SetIndex);
end;

procedure TFrameSettings.EditSettingChange(Sender: TObject);
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

procedure TFrameSettings.BoxSettingsChange(Sender: TObject);
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

procedure TFrameSettings.ButtonSetControlClick(Sender: TObject);
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
  end;
end;

procedure TFrameSettings.ButtonStartClick(Sender: TObject);
var
 i, lastPeriod: Integer;
 periods: TPeriodsList;
begin
  periods:= [];

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
  if Assigned(FStartEvent) then
    FStartEvent(EditName.Caption, periods);
end;

procedure TFrameSettings.UpdateSettingsBox;
var
  i: Integer;
begin
  BoxSettings.Clear;

  for i:= 0 to (Length(SettingsSimpleList) - 1) do
  begin
    BoxSettings.Items.Add(SettingsSimpleList[i].Name);
  end;
end;

procedure TFrameSettings.ShowStatistic;
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

procedure TFrameSettings.WriteSetIndex(AValue: Integer);
begin
  BoxSettings.ItemIndex:= AValue;
  BoxSettingsChange(nil);
end;

function TFrameSettings.ReadSetIndex: Integer;
begin
  Result:= BoxSettings.ItemIndex;
end;

end.

