unit SettingsPro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  ColorBox, EditTime;

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
  private

  public

  end;

implementation

{$R *.lfm}

end.

