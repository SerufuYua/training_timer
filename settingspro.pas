unit SettingsPro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  ColorBox, EditTime, ChainTimer;

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
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonConfigClick(Sender: TObject);
    procedure ButtonSimpleClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
  protected
    FStartEvent: TStartEvent;
    FSimpleEvent, FConfigEvent, FAboutEvent: TNotifyEvent;
  public
    property StartEvent: TStartEvent write FStartEvent;
    property SimpleEvent: TNotifyEvent write FSimpleEvent;
    property ConfigEvent: TNotifyEvent write FConfigEvent;
    property AboutEvent: TNotifyEvent write FAboutEvent;
  end;

implementation

{$R *.lfm}

{ TFrameSettingsPro }

procedure TFrameSettingsPro.ButtonAboutClick(Sender: TObject);
begin
  if Assigned(FAboutEvent) then
    FAboutEvent(self);
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

end.

