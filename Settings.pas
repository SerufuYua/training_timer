unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  EditTime, XMLPropStorage, ChainTimer, MyCommon;

type

  TListSetsCall = function: TMyStrs of object;
  TGetSetCall = function(AName: String): TPeriodsSettings of object;

  { TFrameSettings }

  TFrameSettings = class(TFrame)
    BoxSettings: TComboBox;
    ButtonAddSet: TButton;
    ButtonConfig: TBitBtn;
    ButtonCopySet: TButton;
    ButtonRemoveSet: TButton;
    ButtonStart: TButton;
    ButtonAbout: TButton;
    ButtonPro: TButton;
    CheckWarning: TCheckBox;
    EditMin: TSpinEdit;
    EditMin1: TSpinEdit;
    EditMin2: TSpinEdit;
    EditMin3: TSpinEdit;
    EditName: TEdit;
    EditPrepareTime: TFrameEditTime;
    EditRestTime: TFrameEditTime;
    EditRounds: TSpinEdit;
    EditRoundTime: TFrameEditTime;
    EditSec: TSpinEdit;
    EditSec1: TSpinEdit;
    EditSec2: TSpinEdit;
    EditSec3: TSpinEdit;
    EditWarningTime: TFrameEditTime;
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
    PanelEmpty1: TPanel;
    PanelDummy1: TPanel;
    PanelButtons: TPanel;
    PanelDummy2: TPanel;
    PanelDummy3: TPanel;
    PanelDummy4: TPanel;
    PanelDummy5: TPanel;
    PanelEmpty2: TPanel;
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
    procedure BoxSettingsSelect(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonConfigClick(Sender: TObject);
    procedure ButtonProClick(Sender: TObject);
    procedure ButtonSetControlClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditSettingChange(Sender: TObject);
  protected
    FStartEvent: TStartEvent;
    FProEvent, FConfigEvent, FAboutEvent: TNotifyEvent;
    procedure UpdateBoxSettings;
    procedure ShowStatistic;
    function MakePeriods(index: Integer): TPeriodsSettings;
    procedure WriteIndexSet(AValue: Integer);
    function ReadIndexSet: Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    function ListSets: TMyStrs;
    function GetSet(AName: String): TPeriodsSettings;
    property IndexSet: Integer read ReadIndexSet write WriteIndexSet;
    property StartEvent: TStartEvent write FStartEvent;
    property ProEvent: TNotifyEvent write FProEvent;
    property ConfigEvent: TNotifyEvent write FConfigEvent;
    property AboutEvent: TNotifyEvent write FAboutEvent;
  end;

implementation

uses
  Graphics, Config;

type
  TSettingsSimple = record
    Name: String;
    Rounds: Integer;
    RoundTimeMs, RestTimeMs, PrepareTimeMs, WarningTimeMs: Integer;
    Warning: Boolean;
  end;

  TSettingsSimpleList = Array of TSettingsSimple;

const
  SettingsStor = 'SettingsSimple';

var
  SettingsSimpleList: TSettingsSimpleList;

{$R *.lfm}

constructor TFrameSettings.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  EditRoundTime.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditRestTime.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditPrepareTime.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditWarningTime.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
end;

function TFrameSettings.MakePeriods(index: Integer): TPeriodsSettings;
var
 i, lastPeriod: Integer;
begin
  { prepare Result list }
  Result.Name:= SettingsSimpleList[index].Name;
  Result.Periods:= [];
  SetLength(Result.Periods, EditRounds.Value * 2);

  Result.Periods[0].Name:= 'Prepare';
  Result.Periods[0].TimeMs:= SettingsSimpleList[index].PrepareTimeMs;
  Result.Periods[0].WarningTimeMs:= SettingsSimpleList[index].WarningTimeMs;
  Result.Periods[0].Warning:= SettingsSimpleList[index].Warning;
  Result.Periods[0].Color:= DefaultColorPrepare;
  Result.Periods[0].FinalSound:= TSoundType.Start;

  lastPeriod:= SettingsSimpleList[index].Rounds * 2 - 1;

  for i:= 1 to lastPeriod do
  begin
    Result.Periods[i].WarningTimeMs:= SettingsSimpleList[index].WarningTimeMs;
    Result.Periods[i].Warning:= SettingsSimpleList[index].Warning;

    if ((i mod 2) = 0) then
    begin
      Result.Periods[i].Name:= 'Rest before Round ' + IntToStr((i div 2) + 1) + ' / ' + IntToStr(EditRounds.Value);
      Result.Periods[i].TimeMs:= SettingsSimpleList[index].RestTimeMs;
      Result.Periods[i].Color:= DefaultColorRest;
      Result.Periods[i].FinalSound:= TSoundType.Start;
    end
    else
    begin
      Result.Periods[i].Name:= 'Round ' + IntToStr((i div 2) + 1) + ' / ' + IntToStr(EditRounds.Value);
      Result.Periods[i].TimeMs:= SettingsSimpleList[index].RoundTimeMs;
      Result.Periods[i].Color:= DefaultColorRound;
      if (i = lastPeriod) then
        Result.Periods[i].FinalSound:= TSoundType.Final
      else
        Result.Periods[i].FinalSound:= TSoundType.Ending;
    end;
  end;
end;

procedure TFrameSettings.LoadSettings(APropStorage: TXMLPropStorage);
var
  i, num: Integer;
  path: String;
begin
  num:= APropStorage.ReadInteger(SettingsStor + '/CountSets', 0);

  if (num = 0) then
  begin
    SetLength(SettingsSimpleList, 1);
    SettingsSimpleList[0].Name:= DefaultSetName;
    SettingsSimpleList[0].Rounds:= DefaultRounds;
    SettingsSimpleList[0].RoundTimeMs:= DefaultRoundTimeMs;
    SettingsSimpleList[0].RestTimeMs:= DefaultRestTimeMs;
    SettingsSimpleList[0].PrepareTimeMs:= DefaultPrepareTimeMs;
    SettingsSimpleList[0].WarningTimeMs:= DefaultWarningTimeMs;
    SettingsSimpleList[0].Warning:= DefaultWarning;

    UpdateBoxSettings;
    IndexSet:= 0;
  end
  else
  begin
    SetLength(SettingsSimpleList, num);

    for i:= 0 to (num - 1) do
    begin
      path:= SettingsStor + '/Set' + IntToStr(i) + '/';
      SettingsSimpleList[i].Name:= APropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i + 1));
      SettingsSimpleList[i].Rounds:= APropStorage.ReadInteger(path + 'Rounds', DefaultRounds);
      SettingsSimpleList[i].RoundTimeMs:= APropStorage.ReadInteger(path + 'RoundTimeMs', DefaultRoundTimeMs);
      SettingsSimpleList[i].RestTimeMs:= APropStorage.ReadInteger(path + 'RestTimeMs', DefaultRestTimeMs);
      SettingsSimpleList[i].PrepareTimeMs:= APropStorage.ReadInteger(path + 'PrepareTimeMs', DefaultPrepareTimeMs);
      SettingsSimpleList[i].WarningTimeMs:= APropStorage.ReadInteger(path + 'WarningTimeMs', DefaultWarningTimeMs);
      SettingsSimpleList[i].Warning:= APropStorage.ReadBoolean(path + 'Warning', DefaultWarning);
    end;

    UpdateBoxSettings;
    IndexSet:= APropStorage.ReadInteger(SettingsStor + '/NumSet', 0);
  end;
end;

procedure TFrameSettings.SaveSettings(APropStorage: TXMLPropStorage);
var
  i, num: Integer;
  path: String;
begin
  APropStorage.DoEraseSections(APropStorage.RootNodePath + '/' + SettingsStor);

  num:= Length(SettingsSimpleList);
  APropStorage.WriteInteger(SettingsStor + '/CountSets', num);

  for i:= 0 to (num - 1) do
  begin
    path:= SettingsStor + '/Set' + IntToStr(i) + '/';
    APropStorage.WriteString(path + 'Name', SettingsSimpleList[i].Name);

    if (SettingsSimpleList[i].Rounds <> DefaultRounds) then
      APropStorage.WriteInteger(path + 'Rounds', SettingsSimpleList[i].Rounds);

    if (SettingsSimpleList[i].RoundTimeMs <> DefaultRoundTimeMs) then
      APropStorage.WriteInteger(path + 'RoundTimeMs', SettingsSimpleList[i].RoundTimeMs);

    if (SettingsSimpleList[i].RestTimeMs <> DefaultRestTimeMs) then
      APropStorage.WriteInteger(path + 'RestTimeMs', SettingsSimpleList[i].RestTimeMs);

    if (SettingsSimpleList[i].PrepareTimeMs <> DefaultPrepareTimeMs) then
      APropStorage.WriteInteger(path + 'PrepareTimeMs', SettingsSimpleList[i].PrepareTimeMs);

    if (SettingsSimpleList[i].WarningTimeMs <> DefaultWarningTimeMs) then
      APropStorage.WriteInteger(path + 'WarningTimeMs', SettingsSimpleList[i].WarningTimeMs);

    if (SettingsSimpleList[i].Warning <> DefaultWarning) then
      APropStorage.WriteBoolean(path + 'Warning', SettingsSimpleList[i].Warning);
  end;

  APropStorage.WriteInteger(SettingsStor + '/NumSet', IndexSet);
end;

function TFrameSettings.ListSets: TMyStrs;
var
  l, i: Integer;
begin
  Result:= [];

  l:= Length(SettingsSimpleList);
  SetLength(Result, l);

  for i:= 0 to (l - 1) do
    Result[i]:= SettingsSimpleList[i].Name;
end;

function TFrameSettings.GetSet(AName: String): TPeriodsSettings;
var
  i: Integer;
begin
  for i:= 0 to (Length(SettingsSimpleList) - 1) do
  begin
    if (SettingsSimpleList[i].Name = AName) then
    begin
      Result.Name:= SettingsSimpleList[i].Name;
      Result:= MakePeriods(i);
      Exit;
    end;
  end;
end;

procedure TFrameSettings.EditSettingChange(Sender: TObject);
var
  component: TComponent;
  editStr: TEdit;
  editNum: TSpinEdit;
  editTime: TFrameEditTime;
  editCheck: TCheckBox;
begin
  if ((NOT (Sender is TComponent)) OR (IndexSet < 0)) then Exit;

  component:= Sender as TComponent;

  case component.Name of
    'EditName':
    begin
      editStr:= component as TEdit;
      SettingsSimpleList[IndexSet].Name:= editStr.Caption;
      BoxSettings.Items[IndexSet]:= editStr.Caption;
    end;
    'EditRounds':
    begin
      editNum:= component as TSpinEdit;
      SettingsSimpleList[IndexSet].Rounds:= editNum.Value;
    end;
    'EditRoundTime':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[IndexSet].RoundTimeMs:= editTime.ValueSec * 1000;
    end;
    'EditRestTime':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[IndexSet].RestTimeMs:= editTime.ValueSec * 1000;
    end;
    'EditPrepareTime':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[IndexSet].PrepareTimeMs:= editTime.ValueSec * 1000;
    end;
    'EditWarningTime':
    begin
      editTime:= component as TFrameEditTime;
      SettingsSimpleList[IndexSet].WarningTimeMs:= editTime.ValueSec * 1000;
    end;
    'CheckWarning':
    begin
      editCheck:= component as TCheckBox;
      SettingsSimpleList[IndexSet].Warning:= editCheck.Checked;
      LabelWarningTime.Enabled:= editCheck.Checked;
      EditWarningTime.Enabled:= editCheck.Checked;
    end;
  end;

  ShowStatistic;
end;

procedure TFrameSettings.BoxSettingsSelect(Sender: TObject);
begin
  if (IndexSet < 0) then Exit;

  EditName.Caption:= SettingsSimpleList[IndexSet].Name;
  EditRounds.Value:= SettingsSimpleList[IndexSet].Rounds;
  EditRoundTime.ValueSec:= SettingsSimpleList[IndexSet].RoundTimeMs div 1000;
  EditRestTime.ValueSec:= SettingsSimpleList[IndexSet].RestTimeMs div 1000;
  EditPrepareTime.ValueSec:= SettingsSimpleList[IndexSet].PrepareTimeMs div 1000;
  EditWarningTime.ValueSec:= SettingsSimpleList[IndexSet].WarningTimeMs div 1000;
  CheckWarning.Checked:= SettingsSimpleList[IndexSet].Warning;
  LabelWarningTime.Enabled:= CheckWarning.Checked;
  EditWarningTime.Enabled:= CheckWarning.Checked;

  ShowStatistic;
end;

procedure TFrameSettings.ButtonAboutClick(Sender: TObject);
begin
  if Assigned(FAboutEvent) then
    FAboutEvent(self);
end;

procedure TFrameSettings.ButtonConfigClick(Sender: TObject);
begin
  if Assigned(FConfigEvent) then
    FConfigEvent(self);
end;

procedure TFrameSettings.ButtonProClick(Sender: TObject);
begin
  if Assigned(FProEvent) then
    FProEvent(self);
end;

procedure TFrameSettings.ButtonSetControlClick(Sender: TObject);
var
  component: TComponent;
  idx: Integer;
begin
  if (NOT (Sender is TComponent)) then Exit;

  idx:= IndexSet;
  component:= Sender as TComponent;
  case component.Name of
    'ButtonAddSet':
    begin
      SetLength(SettingsSimpleList, (Length(SettingsSimpleList) + 1));
      idx:= High(SettingsSimpleList);
      SettingsSimpleList[idx].Name:= DefaultSetName;
      SettingsSimpleList[idx].Rounds:= DefaultRounds;
      SettingsSimpleList[idx].RoundTimeMs:= DefaultRoundTimeMs;
      SettingsSimpleList[idx].RestTimeMs:= DefaultRestTimeMs;
      SettingsSimpleList[idx].PrepareTimeMs:= DefaultPrepareTimeMs;
      SettingsSimpleList[idx].WarningTimeMs:= DefaultWarningTimeMs;
      SettingsSimpleList[idx].Warning:= DefaultWarning;
    end;
    'ButtonRemoveSet':
    begin
      if (Length(SettingsSimpleList) > 1) then
      begin
        BoxSettings.Items.Delete(idx);
        Delete(SettingsSimpleList, idx, 1);
        idx:= 0;
      end;
    end;
    'ButtonCopySet':
    begin
      if ((Length(SettingsSimpleList) > 0) AND (IndexSet > -1)) then
      begin
        SetLength(SettingsSimpleList, (Length(SettingsSimpleList) + 1));
        idx:= High(SettingsSimpleList);
        SettingsSimpleList[idx]:= SettingsSimpleList[IndexSet];
        SettingsSimpleList[idx].Name:= SettingsSimpleList[idx].Name + ' Copy';
      end;
    end;
  end;

  UpdateBoxSettings;
  IndexSet:= idx;
end;

procedure TFrameSettings.ButtonStartClick(Sender: TObject);
begin
  { start timer }
  if Assigned(FStartEvent) then
  begin
    FStartEvent(MakePeriods(IndexSet));
  end;
end;

procedure TFrameSettings.UpdateBoxSettings;
var
  i: Integer;
begin
  BoxSettings.Clear;

  for i:= 0 to (Length(SettingsSimpleList) - 1) do
    BoxSettings.Items.Add(SettingsSimpleList[i].Name);
end;

procedure TFrameSettings.ShowStatistic;
var
  sec: Integer;
begin
  sec:= EditPrepareTime.ValueSec +
        EditRestTime.ValueSec * (EditRounds.Value - 1) +
        EditRoundTime.ValueSec * EditRounds.Value;

  LabelStatisticTime.Caption:= TimeToFullStr(sec);
end;

procedure TFrameSettings.WriteIndexSet(AValue: Integer);
begin
  BoxSettings.ItemIndex:= AValue;
  BoxSettingsSelect(nil);
end;

function TFrameSettings.ReadIndexSet: Integer;
begin
  Result:= BoxSettings.ItemIndex;
end;

end.

