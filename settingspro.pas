unit SettingsPro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  ColorBox, XMLPropStorage, EditTime, ChainTimer;

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
    EditPeriodTimeS: TFrameEditTime;
    EditWarningTimeS: TFrameEditTime;
    LabelNameSet: TLabel;
    LabelName1: TLabel;
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
    PanelSettings: TPanel;
    PanelSettingsCompose: TPanel;
    PanelSettingsCompose1: TPanel;
    PanelSettingsNameSet: TPanel;
    procedure BoxSettingsChange(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonConfigClick(Sender: TObject);
    procedure ButtonSimpleClick(Sender: TObject);
    procedure ButtonSetControlClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditSettingChange(Sender: TObject);
  protected
    FStartEvent: TStartEvent;
    FSimpleEvent, FConfigEvent, FAboutEvent: TNotifyEvent;
    procedure UpdateBoxSettings;
    procedure UpdateListPeriods;
    procedure ShowStatistic;
    function MakeDefaultPeriods: TPeriodsList;
    procedure WriteIndexSet(AValue: Integer);
    function ReadIndexSet: Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property IndexSet: Integer read ReadIndexSet write WriteIndexSet;
    property StartEvent: TStartEvent write FStartEvent;
    property SimpleEvent: TNotifyEvent write FSimpleEvent;
    property ConfigEvent: TNotifyEvent write FConfigEvent;
    property AboutEvent: TNotifyEvent write FAboutEvent;
  end;

implementation

uses
  Graphics, TypInfo, Config;

type
  TSettingsPro = record
    Name: String;
    Periods: TPeriodsList;
  end;

  TSettingsProList = Array of TSettingsPro;

const
  SettingsStor = 'SettingsPro';

var
  SettingsProList: TSettingsProList;

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

  EditPeriodTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
  EditWarningTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
end;

function TFrameSettingsPro.MakeDefaultPeriods: TPeriodsList;
var
  i: Integer;
const
  lastPeriod = DefaultRounds * 2 - 1;
begin
    { prepare periods list }
    Result:= [];
    SetLength(Result, DefaultRounds * 2);

    Result[0].Name:= 'Prepare';
    Result[0].TimeMs:= DefaultPrepareTimeMs;
    Result[0].WarningTimeMs:= DefaultWarningTimeMs;
    Result[0].Warning:= DefaultWarning;
    Result[0].Color:= DefaultColorPrepare;
    Result[0].FinalSound:= TSoundType.Start;

    for i:= 1 to lastPeriod do
    begin
      Result[i].WarningTimeMs:= DefaultWarningTimeMs;
      Result[0].Warning:= DefaultWarning;

      if ((i mod 2) = 0) then
      begin
        Result[i].Name:= 'Rest before Round ' + IntToStr((i div 2) + 1) + ' / ' + IntToStr(DefaultRounds);
        Result[i].TimeMs:= DefaultRestTimeMs;
        Result[i].Color:= DefaultColorRest;
        Result[i].FinalSound:= TSoundType.Start;
      end
      else
      begin
        Result[i].Name:= 'Round ' + IntToStr((i div 2) + 1) + ' / ' + IntToStr(DefaultRounds);
        Result[i].TimeMs:= DefaultRoundTimeMs;
        Result[i].Color:= clRed;
        if (i = lastPeriod) then
          Result[i].FinalSound:= TSoundType.Final
        else
          Result[i].FinalSound:= TSoundType.Ending;
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
    SettingsProList[0].Name:= DefaultName;
    SettingsProList[0].Periods:= MakeDefaultPeriods;

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
//  editTime: TFrameEditTime;
//  editWarn: TCheckBox;
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
    'EditPeriodTimeS':
    begin
//      editTime:= component as TFrameEditTime;
//      SettingsProList[IndexSet].RoundTimeMs:= editTime.Value * 1000;
    end;
    'EditWarningTimeS':
    begin
//      editTime:= component as TFrameEditTime;
//      SettingsSimpleList[IndexSet].WarningTimeMs:= editTime.Value * 1000;
    end;
    'CheckWarning':
    begin
//      editWarn:= component as TCheckBox;
//      SettingsSimpleList[IndexSet].Warning:= editWarn.Checked;
//      LabelWarningTime.Enabled:= editWarn.Checked;
//      EditWarningTimeS.Enabled:= editWarn.Checked;
    end;
  end;

  ShowStatistic;
end;

procedure TFrameSettingsPro.BoxSettingsChange(Sender: TObject);
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
  idx: Integer;
begin
  if (NOT (Sender is TComponent)) then Exit;

  component:= Sender as TComponent;

  case component.Name of
    'ButtonAddSet':
    begin
      SetLength(SettingsProList, (Length(SettingsProList) + 1));
      idx:= High(SettingsProList);
      SettingsProList[idx].Name:= DefaultName;
      SettingsProList[idx].Periods:= MakeDefaultPeriods;

      UpdateBoxSettings;
      IndexSet:= idx;
    end;
    'ButtonRemoveSet':
    begin
      if (Length(SettingsProList) > 1) then
      begin
        idx:= IndexSet;
        BoxSettings.Items.Delete(idx);
        Delete(SettingsProList, idx, 1);
        IndexSet:= 0;
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

        UpdateBoxSettings;
        IndexSet:= idx;
      end;
    end;
  end;
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
end;

procedure TFrameSettingsPro.ShowStatistic;
var
  min, sec, i: Integer;
begin
  sec:= 0;
  for i:= 0 to (Length(SettingsProList[IndexSet].Periods) - 1) do
    sec:= sec + SettingsProList[IndexSet].Periods[i].TimeMs div 1000;

  min:= sec div 60;
  sec:= sec - (min * 60);

  LabelStatisticTime.Caption:= IntToStr(min) + ' m  ' + IntToStr(sec) + ' s';
end;

procedure TFrameSettingsPro.WriteIndexSet(AValue: Integer);
begin
  BoxSettings.ItemIndex:= AValue;
  BoxSettingsChange(nil);
end;

function TFrameSettingsPro.ReadIndexSet: Integer;
begin
  Result:= BoxSettings.ItemIndex;
end;

end.

