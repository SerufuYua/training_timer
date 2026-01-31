unit Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn,
  Buttons;

type

  TStopEvent = procedure of object;

  { TFrameConfig }

  TFrameConfig = class(TFrame)
    ButtonPlayStart: TBitBtn;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonPlayEnd: TBitBtn;
    ButtonPlayFinal: TBitBtn;
    ButtonPlayInit: TBitBtn;
    ButtonPlayWarn: TBitBtn;
    EditSoundStart: TFileNameEdit;
    FileNameEnd: TFileNameEdit;
    FileNameFinal: TFileNameEdit;
    FileNameWarn: TFileNameEdit;
    FileNameInit: TFileNameEdit;
    LabelSoundEnd: TLabel;
    LabelSoundFinal: TLabel;
    LabelSoundWarn: TLabel;
    LabelSoundInit: TLabel;
    LabelTimerPeriod: TLabel;
    LabelSoundStart: TLabel;
    PanelEmpty: TPanel;
    PanelParameters: TPanel;
    PanelButtons: TPanel;
    EditTimerPeriod: TSpinEdit;
    procedure ButtonCancelClick(Sender: TObject);
  protected
    FCancelEvent: TNotifyEvent;
  public
    property CancelEvent: TNotifyEvent write FCancelEvent;
  end;

implementation

{$R *.lfm}

{ TFrameConfig }

procedure TFrameConfig.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FCancelEvent) then
    FCancelEvent(nil);
end;

end.

