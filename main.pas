unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, ChainTimer;

type

  { TFormTTimer }

  TFormTTimer = class(TForm)
    ButtonStart: TButton;
    EditPrepareTime: TSpinEdit;
    EditWarningTime: TSpinEdit;
    EditRoundTime: TSpinEdit;
    EditRestTime: TSpinEdit;
    FrameTimerUse: TFrameTimer;
    LabelPrepareTime: TLabel;
    LabelWarningTime: TLabel;
    LabelRoundTime: TLabel;
    LabelRounds: TLabel;
    LabelRestTime: TLabel;
    ControlPageTimer: TPageControl;
    EditRounds: TSpinEdit;
    PanelSettings: TPanel;
    TabSettings: TTabSheet;
    TabTraining: TTabSheet;
    procedure ButtonStartClick(Sender: TObject);
  protected
    procedure StopEvent;
  public

  end;

var
  FormTTimer: TFormTTimer;

implementation

{$R *.lfm}

{ TFormTTimer }

procedure TFormTTimer.ButtonStartClick(Sender: TObject);
begin
  FrameTimerUse.Start(EditRounds.Value,
                      EditRoundTime.Value * 1000,
                      EditRestTime.Value * 1000,
                      EditPrepareTime.Value * 1000,
                      EditWarningTime.Value * 1000);
  FrameTimerUse.StopEvent:= {$ifdef FPC}@{$endif}StopEvent;
  ControlPageTimer.ActivePage:= TabTraining;
end;

procedure TFormTTimer.StopEvent;
begin
  ControlPageTimer.ActivePage:= TabSettings;
end;

end.

