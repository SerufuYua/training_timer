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
    EditName: TEdit;
    EditPeriodTimeS: TFrameEditTime;
    EditWarningTimeS: TFrameEditTime;
    LabelName: TLabel;
    LabelPeriodTime: TLabel;
    LabelColor: TLabel;
    LabelStatistic: TLabel;
    LabelStatisticTime: TLabel;
    LabelWarningTime: TLabel;
    LabelSoud: TLabel;
    ListPeriods: TListBox;
    PanelDummy6: TPanel;
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
    procedure ButtonStartClick(Sender: TObject);
  protected
    FStartEvent: TStartEvent;
    FSimpleEvent, FConfigEvent, FAboutEvent: TNotifyEvent;
    procedure UpdateSettingsBox;
    procedure UpdateSetsList;
    procedure ShowStatistic;
    function MakeDefaultPeriods: TPeriodsList;
    procedure WriteSetIndex(AValue: Integer);
    function ReadSetIndex: Integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property SetIndex: Integer read ReadSetIndex write WriteSetIndex;
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

//  EditPeriodTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
//  EditWarningTimeS.OnChange:= {$ifdef FPC}@{$endif}EditSettingChange;
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

    UpdateSettingsBox;
    SetIndex:= 0;
  end
  else
  begin
    SetLength(SettingsProList, countSets);

    for i:= 0 to (countSets - 1) do
    begin
      path:= SettingsStor + '/Set' + IntToStr(i) + '/';
      SettingsProList[i].Name:= APropStorage.ReadString(path + 'Name', 'Set ' + IntToStr(i + 1));

      countPeriods:= APropStorage.ReadInteger(path + 'CountPeriods', DefaultRounds);
      SetLength(SettingsProList[i].Periods, countPeriods);
      for j:= 0 to (countPeriods - 1) do
      begin
        pathPeriod:= path + '/Period' + IntToStr(j) + '/';
        SettingsProList[i].Periods[j].Name:= APropStorage.ReadString(pathPeriod + 'Name', 'Period ' + IntToStr(j + 1));
        SettingsProList[i].Periods[j].TimeMs:= APropStorage.ReadInteger(pathPeriod + 'TimeMs', DefaultRoundTimeMs);
        SettingsProList[i].Periods[j].WarningTimeMs:= APropStorage.ReadInteger(pathPeriod + 'WarningTimeMs', DefaultWarningTimeMs);
        SettingsProList[i].Periods[j].Warning:= APropStorage.ReadBoolean(pathPeriod + 'Warning', DefaultWarning);
        SettingsProList[i].Periods[j].FinalSound:= TSoundType(APropStorage.ReadInteger(pathPeriod + 'FinalSound', 0));
        SettingsProList[i].Periods[j].Color:= StringToColor(APropStorage.ReadString(pathPeriod + 'Color', DefaultColorStr));
      end;
    end;

    UpdateSettingsBox;
    SetIndex:= APropStorage.ReadInteger(SettingsStor + '/NumSet', 0);
  end;
end;

procedure TFrameSettingsPro.SaveSettings(APropStorage: TXMLPropStorage);
{var
  i, num: Integer;
  path: String;}
begin
{  APropStorage.DoEraseSections(APropStorage.RootNodePath + '/Settings');

  num:= Length(SettingsSimpleList);
  APropStorage.WriteInteger('Settings/SetCount', num);

  for i:= 0 to (num - 1) do
  begin
    path:= 'Settings/Set' + IntToStr(i) + '/';
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

  APropStorage.WriteInteger('Settings/SetNum', SetIndex);       }
end;

procedure TFrameSettingsPro.ButtonAboutClick(Sender: TObject);
begin
  if Assigned(FAboutEvent) then
    FAboutEvent(self);
end;

procedure TFrameSettingsPro.BoxSettingsChange(Sender: TObject);
begin
  if (SetIndex < 0) then Exit;

  EditName.Caption:= SettingsProList[SetIndex].Name;
  UpdateSetsList;
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

procedure TFrameSettingsPro.ButtonStartClick(Sender: TObject);
begin

end;

procedure TFrameSettingsPro.UpdateSettingsBox;
var
  i: Integer;
begin
  BoxSettings.Clear;

  for i:= 0 to (Length(SettingsProList) - 1) do
    BoxSettings.Items.Add(SettingsProList[i].Name);
end;

procedure TFrameSettingsPro.UpdateSetsList;
var
  i: Integer;
begin
  ListPeriods.Clear;

  for i:= 0 to (Length(SettingsProList[SetIndex].Periods) - 1) do
    ListPeriods.Items.Add(SettingsProList[SetIndex].Periods[i].Name);
end;

procedure TFrameSettingsPro.ShowStatistic;
var
  min, sec, i: Integer;
begin
  sec:= 0;
  for i:= 0 to (Length(SettingsProList[SetIndex].Periods) - 1) do
    sec:= sec + SettingsProList[SetIndex].Periods[i].TimeMs;

  min:= sec div 60;
  sec:= sec - (min * 60);

  LabelStatisticTime.Caption:= IntToStr(min) + ' m  ' + IntToStr(sec) + ' s';
end;

procedure TFrameSettingsPro.WriteSetIndex(AValue: Integer);
begin
  BoxSettings.ItemIndex:= AValue;
  BoxSettingsChange(nil);
end;

function TFrameSettingsPro.ReadSetIndex: Integer;
begin
  Result:= BoxSettings.ItemIndex;
end;

end.

