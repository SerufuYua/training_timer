unit SettingsPro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  ColorBox, XMLPropStorage, Menus, EditTime, ChainTimer, Settings;

type

  { TFrameSettingsPro }

  TFrameSettingsPro = class(TFrame)
    BoxSettings: TComboBox;
    ButtonAdd: TButton;
    ButtonAbout: TButton;
    ButtonDown: TButton;
    ButtonUp: TButton;
    ButtonRemove: TButton;
    ButtonAddSet: TButton;
    ButtonConfig: TBitBtn;
    ButtonCopySet: TButton;
    ButtonImportSet: TButton;
    ButtonRemoveSet: TButton;
    ButtonStart: TButton;
    ButtonSimple: TButton;
    CheckWarning: TCheckBox;
    ColorBox: TColorBox;
    ComboSound: TComboBox;
    EditNameSet: TEdit;
    EditNamePeriod: TEdit;
    EditPeriodTime: TFrameEditTime;
    EditWarningTime: TFrameEditTime;
    LabelNameSet: TLabel;
    LabelPeriodTime: TLabel;
    LabelColor: TLabel;
    LabelNamePeriod: TLabel;
    LabelStatistic: TLabel;
    LabelStatisticTime: TLabel;
    LabelWarningTime: TLabel;
    LabelSoud: TLabel;
    ListPeriods: TListBox;
    PanelDummy6: TPanel;
    PanelDummy7: TPanel;
    PanelPeriodControl: TPanel;
    PanelPeriods: TPanel;
    PanelControl: TPanel;
    PanelSets: TPanel;
    PanelEmply3: TPanel;
    PanelButtons: TPanel;
    PanelDummy5: TPanel;
    PanelEmply4: TPanel;
    PanelSelectSets: TPanel;
    PanelSetsControl: TPanel;
    PanelPeriodSettings: TPanel;
    PanelSettingsCompose: TPanel;
    PanelSettingsCompose1: TPanel;
    PanelSettingsNameSet: TPanel;
    ImportMenu: TPopupMenu;
    procedure BoxSettingsSelect(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonConfigClick(Sender: TObject);
    procedure ButtonSimpleClick(Sender: TObject);
    procedure ButtonSetControlClick(Sender: TObject);
    procedure ButtonPeriodControlClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditSettingChange(Sender: TObject);
    procedure ListPeriodsSelectionChange(Sender: TObject; User: boolean);
    procedure ImportMenuClick(Sender: TObject);
  protected
    FStartEvent: TStartEvent;
    FSimpleEvent, FConfigEvent, FAboutEvent: TNotifyEvent;
    FSimpleListSets: TListSetsCall;
    FSimpleGetSet: TGetSetCall;
    procedure UpdateBoxSettings;
    procedure UpdateListPeriods;
    procedure ShowStatistic;
    function MakeDefaultPeriods: TPeriodsSettings;
    procedure WriteIndexSet(AValue: Integer);
    function ReadIndexSet: Integer;
    procedure WriteIndexPeriod(AValue: Integer);
    function ReadIndexPeriod: Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property IndexSet: Integer read ReadIndexSet write WriteIndexSet;
    property IndexPeriod: Integer read ReadIndexPeriod write WriteIndexPeriod;
    property StartEvent: TStartEvent write FStartEvent;
    property SimpleEvent: TNotifyEvent write FSimpleEvent;
    property ConfigEvent: TNotifyEvent write FConfigEvent;
    property AboutEvent: TNotifyEvent write FAboutEvent;
    property SimpleListSets: TListSetsCall write FSimpleListSets;
    property SimpleGetSet: TGetSetCall write FSimpleGetSet;
  end;

implementation

uses
  Graphics, TypInfo, Config, MyCommon;

const
  SettingsStor = 'SettingsPro';

var
  SettingsProList: TPeriodsSettingsList;

{$R *.lfm}

{ TFrameSettingsPro }

constructor TFrameSettingsPro.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);

  ComboSound.Clear;
  for i:= Ord(Low(TSoundType)) to Ord(High(TSoundType)) do
    ComboSound.Items.Add(GetEnumName(TypeInfo(TSoundType), Ord(i)));
  ComboSound.ItemIndex:= 0;

  EditPeriodTime.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditWarningTime.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
end;

function TFrameSettingsPro.MakeDefaultPeriods: TPeriodsSettings;
var
  i: Integer;
const
  lastPeriod = DefaultRounds * 2 - 1;
begin
  { prepare periods list }
  Result.Name:= DefaultSetName;
  Result.Periods:= [];
  SetLength(Result.Periods, DefaultRounds * 2);

  Result.Periods[0].Name:= 'Prepare';
  Result.Periods[0].TimeMs:= DefaultPrepareTimeMs;
  Result.Periods[0].WarningTimeMs:= DefaultWarningTimeMs;
  Result.Periods[0].Warning:= DefaultWarning;
  Result.Periods[0].Color:= DefaultColorPrepare;
  Result.Periods[0].FinalSound:= TSoundType.Start;

  for i:= 1 to lastPeriod do
  begin
    Result.Periods[i].WarningTimeMs:= DefaultWarningTimeMs;
    Result.Periods[i].Warning:= DefaultWarning;

    if ((i mod 2) = 0) then
    begin
      Result.Periods[i].Name:= 'Rest before Round ' + IntToStr((i div 2) + 1) + ' / ' + IntToStr(DefaultRounds);
      Result.Periods[i].TimeMs:= DefaultRestTimeMs;
      Result.Periods[i].Color:= DefaultColorRest;
      Result.Periods[i].FinalSound:= TSoundType.Start;
    end
    else
    begin
      Result.Periods[i].Name:= 'Round ' + IntToStr((i div 2) + 1) + ' / ' + IntToStr(DefaultRounds);
      Result.Periods[i].TimeMs:= DefaultRoundTimeMs;
      Result.Periods[i].Color:= clRed;
      if (i = lastPeriod) then
        Result.Periods[i].FinalSound:= TSoundType.Final
      else
        Result.Periods[i].FinalSound:= TSoundType.Ending;
    end;
  end;
end;

procedure TFrameSettingsPro.LoadSettings(APropStorage: TXMLPropStorage);
var
  i, j, countSets, countPeriods: Integer;
  path, pathPeriod: String;
begin
  countSets:= APropStorage.ReadInteger(SettingsStor + '/CountSets', 0);

  if (countSets = 0) then
  begin
    SetLength(SettingsProList, 1);
    SettingsProList[0]:= MakeDefaultPeriods;

    UpdateBoxSettings;
    IndexSet:= 0;
  end
  else
  begin
    SetLength(SettingsProList, countSets);

    for i:= 0 to (countSets - 1) do
    begin
      path:= SettingsStor + '/Set' + IntToStr(i) + '/';
      SettingsProList[i].Name:= APropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i + 1));

      countPeriods:= APropStorage.ReadInteger(path + 'CountPeriods', 0);
      SetLength(SettingsProList[i].Periods, countPeriods);
      for j:= 0 to (countPeriods - 1) do
      begin
        pathPeriod:= path + 'Period' + IntToStr(j) + '/';
        SettingsProList[i].Periods[j].Name:= APropStorage.ReadString(pathPeriod + 'Name', 'Period ' + IntToStr(j + 1));
        SettingsProList[i].Periods[j].TimeMs:= APropStorage.ReadInteger(pathPeriod + 'TimeMs', DefaultRoundTimeMs);
        SettingsProList[i].Periods[j].WarningTimeMs:= APropStorage.ReadInteger(pathPeriod + 'WarningTimeMs', DefaultWarningTimeMs);
        SettingsProList[i].Periods[j].Warning:= APropStorage.ReadBoolean(pathPeriod + 'Warning', DefaultWarning);
        SettingsProList[i].Periods[j].FinalSound:= TSoundType(APropStorage.ReadInteger(pathPeriod + 'FinalSound', Ord(DefaultFinalSound)));
        SettingsProList[i].Periods[j].Color:= StringToColor(APropStorage.ReadString(pathPeriod + 'Color', DefaultColorStr));
      end;
    end;

    UpdateBoxSettings;
    IndexSet:= APropStorage.ReadInteger(SettingsStor + '/NumSet', 0);
  end;
end;

procedure TFrameSettingsPro.SaveSettings(APropStorage: TXMLPropStorage);
var
  i, j, countSets, countPeriods: Integer;
  path, pathPeriod: String;
begin
  APropStorage.DoEraseSections(APropStorage.RootNodePath + '/' + SettingsStor);

  countSets:= Length(SettingsProList);
  APropStorage.WriteInteger(SettingsStor + '/CountSets', countSets);

  for i:= 0 to (countSets - 1) do
  begin
    path:= SettingsStor + '/Set' + IntToStr(i) + '/';
    APropStorage.WriteString(path + 'Name', SettingsProList[i].Name);

    countPeriods:= Length(SettingsProList[i].Periods);
    APropStorage.WriteInteger(path + 'CountPeriods', countPeriods);
    for j:= 0 to (countPeriods - 1) do
    begin
      pathPeriod:= path + 'Period' + IntToStr(j) + '/';

      APropStorage.WriteString(pathPeriod + 'Name', SettingsProList[i].Periods[j].Name);

      if (SettingsProList[i].Periods[j].TimeMs <> DefaultRoundTimeMs) then
        APropStorage.WriteInteger(pathPeriod + 'TimeMs', SettingsProList[i].Periods[j].TimeMs);

      if (SettingsProList[i].Periods[j].WarningTimeMs <> DefaultWarningTimeMs) then
        APropStorage.WriteInteger(pathPeriod + 'WarningTimeMs', SettingsProList[i].Periods[j].WarningTimeMs);

      if (SettingsProList[i].Periods[j].Warning <> DefaultWarning) then
        APropStorage.WriteBoolean(pathPeriod + 'Warning', SettingsProList[i].Periods[j].Warning);

      if (SettingsProList[i].Periods[j].FinalSound <> DefaultFinalSound) then
        APropStorage.WriteInteger(pathPeriod + 'FinalSound', Ord(SettingsProList[i].Periods[j].FinalSound));

      if (SettingsProList[i].Periods[j].Color <> StringToColor(DefaultColorStr)) then
        APropStorage.WriteString(pathPeriod + 'Color', ColorToString(SettingsProList[i].Periods[j].Color));
    end;
  end;

  APropStorage.WriteInteger(SettingsStor + '/NumSet', IndexSet);
end;

procedure TFrameSettingsPro.ButtonAboutClick(Sender: TObject);
begin
  if Assigned(FAboutEvent) then
    FAboutEvent(self);
end;

procedure TFrameSettingsPro.EditSettingChange(Sender: TObject);
var
  component: TComponent;
  editStr: TEdit;
  editTime: TFrameEditTime;
  editCheck: TCheckBox;
  editBox: TComboBox;
  editColor: TColorBox;
begin
  if ((NOT (Sender is TComponent)) OR (IndexSet < 0)) then Exit;

  component:= Sender as TComponent;

  case component.Name of
    'EditNameSet':
    begin
      editStr:= component as TEdit;
      SettingsProList[IndexSet].Name:= editStr.Caption;
      BoxSettings.Items[IndexSet]:= editStr.Caption;
    end;
    'EditNamePeriod':
    begin
      editStr:= component as TEdit;
      SettingsProList[IndexSet].Periods[IndexPeriod].Name:= editStr.Caption;
      ListPeriods.Items[IndexPeriod]:= editStr.Caption;
    end;
    'EditPeriodTime':
    begin
      editTime:= component as TFrameEditTime;
      SettingsProList[IndexSet].Periods[IndexPeriod].TimeMs:= editTime.ValueSec * 1000;
    end;
    'EditWarningTime':
    begin
      editTime:= component as TFrameEditTime;
      SettingsProList[IndexSet].Periods[IndexPeriod].WarningTimeMs:= editTime.ValueSec * 1000;
    end;
    'CheckWarning':
    begin
      editCheck:= component as TCheckBox;
      SettingsProList[IndexSet].Periods[IndexPeriod].Warning:= editCheck.Checked;
      LabelWarningTime.Enabled:= editCheck.Checked;
      EditWarningTime.Enabled:= editCheck.Checked;
    end;
    'ComboSound':
    begin
      editBox:= component as TComboBox;
      SettingsProList[IndexSet].Periods[IndexPeriod].FinalSound:= TSoundType(editBox.ItemIndex);
    end;
    'ColorBox':
    begin
      editColor:= component as TColorBox;
      SettingsProList[IndexSet].Periods[IndexPeriod].Color:= editColor.Selected;
    end;
  end;

  ShowStatistic;
end;

procedure TFrameSettingsPro.ListPeriodsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if ((IndexSet < 0) OR (IndexPeriod < 0)) then Exit;

  EditNamePeriod.Caption:= SettingsProList[IndexSet].Periods[IndexPeriod].Name;
  EditPeriodTime.ValueSec:= SettingsProList[IndexSet].Periods[IndexPeriod].TimeMs div 1000;
  EditWarningTime.ValueSec:= SettingsProList[IndexSet].Periods[IndexPeriod].WarningTimeMs div 1000;
  CheckWarning.Checked:= SettingsProList[IndexSet].Periods[IndexPeriod].Warning;
  ComboSound.ItemIndex:= Ord(SettingsProList[IndexSet].Periods[IndexPeriod].FinalSound);
  ColorBox.Selected:= SettingsProList[IndexSet].Periods[IndexPeriod].Color;
  PanelPeriodSettings.Enabled:= True;
end;

procedure TFrameSettingsPro.ImportMenuClick(Sender: TObject);
var
  idx: Integer;
  item: TMenuItem;
begin
  if (NOT (Sender is TMenuItem)) then Exit;
  item:= Sender as TMenuItem;

  if Assigned(FSimpleGetSet) then
  begin
    SetLength(SettingsProList, (Length(SettingsProList) + 1));
    idx:= High(SettingsProList);
    SettingsProList[idx]:= FSimpleGetSet(item.Caption);
    UpdateBoxSettings;
    IndexSet:= idx;
  end;
end;

procedure TFrameSettingsPro.BoxSettingsSelect(Sender: TObject);
begin
  if (IndexSet < 0) then Exit;

  EditNameSet.Caption:= SettingsProList[IndexSet].Name;
  UpdateListPeriods;
  ShowStatistic;
end;

procedure TFrameSettingsPro.ButtonConfigClick(Sender: TObject);
begin
  if Assigned(FConfigEvent) then
    FConfigEvent(self);
end;

procedure TFrameSettingsPro.ButtonSimpleClick(Sender: TObject);
begin
  if Assigned(FSimpleEvent) then
    FSimpleEvent(self);
end;

procedure TFrameSettingsPro.ButtonSetControlClick(Sender: TObject);
var
  component: TComponent;
  idx, i: Integer;
  item: TMenuItem;
  sets: TMyStrs;
begin
  if (NOT (Sender is TComponent)) then Exit;

  idx:= IndexSet;
  component:= Sender as TComponent;
  case component.Name of
    'ButtonAddSet':
    begin
      SetLength(SettingsProList, (Length(SettingsProList) + 1));
      idx:= High(SettingsProList);
      SettingsProList[idx]:= MakeDefaultPeriods;
    end;
    'ButtonRemoveSet':
    begin
      if (Length(SettingsProList) > 1) then
      begin
        BoxSettings.Items.Delete(idx);
        Delete(SettingsProList, idx, 1);
        idx:= 0;
      end;
    end;
    'ButtonCopySet':
    begin
      if ((Length(SettingsProList) > 0) AND (IndexSet > -1)) then
      begin
        SetLength(SettingsProList, (Length(SettingsProList) + 1));
        idx:= High(SettingsProList);
        SettingsProList[idx]:= SettingsProList[IndexSet];
        SettingsProList[idx].Name:= SettingsProList[idx].Name + ' Copy';
      end;
    end;
    'ButtonImportSet':
    begin
      if Assigned(FSimpleListSets) then
      begin
        ImportMenu.Items.Clear;
        sets:= FSimpleListSets();
        for i:= 0 to (Length(sets) - 1) do
        begin
          item:= TMenuItem.Create(ImportMenu);
          item.Caption:= sets[i];
          item.OnClick:= {$ifdef FPC}@{$endif}ImportMenuClick;
          ImportMenu.Items.Add(item);
        end;
        ImportMenu.PopUp;
      end;
    end;
  end;

  UpdateBoxSettings;
  IndexSet:= idx;
end;

procedure TFrameSettingsPro.ButtonPeriodControlClick(Sender: TObject);
var
  component: TComponent;
  idx: Integer;
  buff: TTimePeriod;
begin
  if (NOT (Sender is TComponent)) then Exit;

  idx:= IndexPeriod;
  component:= Sender as TComponent;
  case component.Name of
    'ButtonAdd':
    begin
      SetLength(SettingsProList[IndexSet].Periods, (Length(SettingsProList[IndexSet].Periods) + 1));
      idx:= High(SettingsProList[IndexSet].Periods);
      SettingsProList[IndexSet].Periods[idx].Name:= DefaultPeriodName;
      SettingsProList[IndexSet].Periods[idx].FinalSound:= DefaultFinalSound;
      SettingsProList[IndexSet].Periods[idx].TimeMs:= DefaultRoundTimeMs;
      SettingsProList[IndexSet].Periods[idx].WarningTimeMs:= DefaultWarningTimeMs;
      SettingsProList[IndexSet].Periods[idx].Warning:= DefaultWarning;
      SettingsProList[IndexSet].Periods[idx].Color:= DefaultColorPrepare;
    end;
    'ButtonRemove':
    begin
      if (Length(SettingsProList[IndexSet].Periods) > 0) then
      begin
        ListPeriods.Items.Delete(idx);
        Delete(SettingsProList[IndexSet].Periods, idx, 1);
        if (idx >= Length(SettingsProList[IndexSet].Periods)) then
          idx:= idx - 1;
      end;
    end;
    'ButtonUp':
    begin
      if (idx > 0) then
      begin
        buff:= SettingsProList[IndexSet].Periods[idx];
        SettingsProList[IndexSet].Periods[idx]:= SettingsProList[IndexSet].Periods[idx - 1];
        SettingsProList[IndexSet].Periods[idx - 1]:= buff;
        idx:= idx - 1;
      end;
    end;
    'ButtonDown':
    begin
      if (idx < (Length(SettingsProList[IndexSet].Periods) - 1)) then
      begin
        buff:= SettingsProList[IndexSet].Periods[idx];
        SettingsProList[IndexSet].Periods[idx]:= SettingsProList[IndexSet].Periods[idx + 1];
        SettingsProList[IndexSet].Periods[idx + 1]:= buff;
        idx:= idx + 1;
      end;
    end;
  end;

  UpdateListPeriods;
  IndexPeriod:= idx;
  ShowStatistic;
end;

procedure TFrameSettingsPro.ButtonStartClick(Sender: TObject);
begin

end;

procedure TFrameSettingsPro.UpdateBoxSettings;
var
  i: Integer;
begin
  BoxSettings.Clear;

  for i:= 0 to (Length(SettingsProList) - 1) do
    BoxSettings.Items.Add(SettingsProList[i].Name);
end;

procedure TFrameSettingsPro.UpdateListPeriods;
var
  i: Integer;
begin
  ListPeriods.Clear;

  for i:= 0 to (Length(SettingsProList[IndexSet].Periods) - 1) do
    ListPeriods.Items.Add(SettingsProList[IndexSet].Periods[i].Name);

  PanelPeriodSettings.Enabled:= False;
end;

procedure TFrameSettingsPro.ShowStatistic;
var
  sec, i: Integer;
begin
  sec:= 0;
  for i:= 0 to (Length(SettingsProList[IndexSet].Periods) - 1) do
    sec:= sec + SettingsProList[IndexSet].Periods[i].TimeMs div 1000;

  LabelStatisticTime.Caption:= TimeToFullStr(sec);
end;

procedure TFrameSettingsPro.WriteIndexSet(AValue: Integer);
begin
  if ((BoxSettings.Items.Count > 0) AND (AValue < BoxSettings.Items.Count)) then
  begin
    BoxSettings.ItemIndex:= AValue;
    BoxSettingsSelect(nil);
  end;
end;

function TFrameSettingsPro.ReadIndexSet: Integer;
begin
  Result:= BoxSettings.ItemIndex;
end;

procedure TFrameSettingsPro.WriteIndexPeriod(AValue: Integer);
begin
  if ((ListPeriods.Items.Count > 0) AND (AValue < ListPeriods.Items.Count)) then
    ListPeriods.ItemIndex:= AValue;
end;

function TFrameSettingsPro.ReadIndexPeriod: Integer;
begin
  Result:= ListPeriods.ItemIndex
end;

end.

