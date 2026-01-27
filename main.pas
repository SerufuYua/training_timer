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
    'ButtonSeveSets':
    begin
      SaveSettings;
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

