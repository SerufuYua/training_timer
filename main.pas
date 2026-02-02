unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  XMLPropStorage, Buttons, Generics.Collections,
  ChainTimer, Settings, Config, About;

type

  TTabStack = {$ifdef FPC}specialize{$endif} TObjectStack<TTabSheet>;

  { TFormTTimer }

  TFormTTimer = class(TForm)
    FrameAboutUse: TFrameAbout;
    FrameConfigUse: TFrameConfig;
    FrameSettingsUse: TFrameSettings;
    FrameTimerUse: TFrameTimer;
    ControlPageTimer: TPageControl;
    TabSettings: TTabSheet;
    TabConfig: TTabSheet;
    TabAbout: TTabSheet;
    TabTraining: TTabSheet;
    PropStorage: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure PropStorageRestoreProperties(Sender: TObject);
    procedure PropStorageSaveProperties(Sender: TObject);
  protected
    FTabStack: TTabStack;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure StartEvent(ASetName: String; APeriods: TPeriodsList);
    procedure ReturnEvent(Sender: TObject);
    procedure ConfigEvent(Sender: TObject);
    procedure AboutEvent(Sender: TObject);
    function ReadActiveTab: TTabSheet;
    procedure WriteActiveTab(const AValue: TTabSheet);
  public
    property ActiveTab: TTabSheet read ReadActiveTab write WriteActiveTab;
  end;

var
  FormTTimer: TFormTTimer;

implementation

{$R *.lfm}

{ TFormTTimer }

procedure TFormTTimer.FormCreate(Sender: TObject);
begin
  FTabStack:= TTabStack.Create(False);
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}ReturnEvent;
  FrameConfigUse.ReturnEvent:= {$ifdef FPC}@{$endif}ReturnEvent;
  FrameConfigUse.AboutEvent:= {$ifdef FPC}@{$endif}AboutEvent;
  FrameAboutUse.ReturnEvent:= {$ifdef FPC}@{$endif}ReturnEvent;
  FrameSettingsUse.StartEvent:= {$ifdef FPC}@{$endif}StartEvent;
  FrameSettingsUse.ConfigEvent:= {$ifdef FPC}@{$endif}ConfigEvent;
  FrameSettingsUse.AboutEvent:= {$ifdef FPC}@{$endif}AboutEvent;

  {$ifdef RELEASE}
  ControlPageTimer.ShowTabs:= False;
  {$endif}
end;

procedure TFormTTimer.PropStorageRestoreProperties(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFormTTimer.PropStorageSaveProperties(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormTTimer.SaveSettings;
begin
  FrameSettingsUse.SaveSettings(PropStorage);
  FrameConfigUse.SaveSettings(PropStorage);
end;

procedure TFormTTimer.LoadSettings;
begin
  FrameSettingsUse.LoadSettings(PropStorage);
  FrameConfigUse.LoadSettings(PropStorage);
end;

procedure TFormTTimer.StartEvent(ASetName: String; APeriods: TPeriodsList);
begin
  FrameTimerUse.Start(ASetName, APeriods);
  ActiveTab:= TabTraining;
end;

procedure TFormTTimer.ReturnEvent(Sender: TObject);
begin
  if (FTabStack.Count > 0) then
    ControlPageTimer.ActivePage:= FTabStack.Pop
  else
    ControlPageTimer.ActivePage:= TabSettings;
end;

procedure TFormTTimer.ConfigEvent(Sender: TObject);
begin
  ActiveTab:= TabConfig;
end;

procedure TFormTTimer.AboutEvent(Sender: TObject);
begin
  ActiveTab:= TabAbout;
end;

function TFormTTimer.ReadActiveTab: TTabSheet;
begin
  Result:= ControlPageTimer.ActivePage;
end;

procedure TFormTTimer.WriteActiveTab(const AValue: TTabSheet);
begin
  FTabStack.Push(ControlPageTimer.ActivePage);
  ControlPageTimer.ActivePage:= AValue;
end;

end.

