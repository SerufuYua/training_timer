unit Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn,
  Buttons, XMLPropStorage, uplaysound;

type

  TStopEvent = procedure of object;

  { TFrameConfig }

  TFrameConfig = class(TFrame)
    ButtonDefault: TButton;
    ButtonPlayStart: TBitBtn;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonPlayEnd: TBitBtn;
    ButtonPlayFinal: TBitBtn;
    ButtonPlayInit: TBitBtn;
    ButtonPlayWarn: TBitBtn;
    FileNameStart: TFileNameEdit;
    FileNameEnd: TFileNameEdit;
    FileNameFinal: TFileNameEdit;
    FileNameWarn: TFileNameEdit;
    FileNameInit: TFileNameEdit;
    LabelSoundEnd: TLabel;
    LabelSoundFinal: TLabel;
    LabelSoundWarn: TLabel;
    LabelSoundInit: TLabel;
    LabelTimerInterval: TLabel;
    LabelSoundStart: TLabel;
    PanelEmpty: TPanel;
    PanelParameters: TPanel;
    PanelButtons: TPanel;
    EditTimerInterval: TSpinEdit;
    PlaySound: Tplaysound;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonDefaultClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonPlaySound(Sender: TObject);
  protected
    FReturnEvent: TNotifyEvent;
  public
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property ReturnEvent: TNotifyEvent write FReturnEvent;
  end;

var
  TimerInterval: Integer;
  SoundStart, SoundEnd, SoundFinal, SoundWarn, SoundInit: String;

implementation

const
  DefaultSoundStart = 'data\sound\start.wav';
  DefaultSoundEnd = 'data\sound\end.wav';
  DefaultSoundFinal = 'data\sound\fin.wav';
  DefaultSoundWarn = 'data\sound\warn.wav';
  DefaultSoundInit = 'data\sound\init.wav';
  DefaultTimerInterval = 100;

{$R *.lfm}

{ TFrameConfig }

procedure TFrameConfig.LoadSettings(APropStorage: TXMLPropStorage);
var
  path: String;
begin
  path:= 'Config';
  TimerInterval:= APropStorage.ReadInteger(path + '/TimerInterval', DefaultTimerInterval);
  SoundStart:= APropStorage.ReadString(path + '/SoundStart', DefaultSoundStart);
  SoundEnd:= APropStorage.ReadString(path + '/SoundEnd', DefaultSoundEnd);
  SoundFinal:= APropStorage.ReadString(path + '/SoundFinal', DefaultSoundFinal);
  SoundWarn:= APropStorage.ReadString(path + '/SoundWarn', DefaultSoundWarn);
  SoundInit:= APropStorage.ReadString(path + '/SoundInit', DefaultSoundInit);

  EditTimerInterval.Value:= TimerInterval;
  FileNameStart.Caption:= SoundStart;
  FileNameEnd.Caption:= SoundEnd;
  FileNameFinal.Caption:= SoundFinal;
  FileNameWarn.Caption:= SoundWarn;
  FileNameInit.Caption:= SoundInit;
end;

procedure TFrameConfig.SaveSettings(APropStorage: TXMLPropStorage);
var
  path: String;
begin
  path:= 'Config';
  APropStorage.WriteInteger(path + '/TimerInterval', TimerInterval);
  APropStorage.WriteString(path + '/SoundStart', SoundStart);
  APropStorage.WriteString(path + '/SoundEnd', SoundEnd);
  APropStorage.WriteString(path + '/SoundFinal', SoundFinal);
  APropStorage.WriteString(path + '/SoundWarn', SoundWarn);
  APropStorage.WriteString(path + '/SoundInit', SoundInit);
end;

procedure TFrameConfig.ButtonCancelClick(Sender: TObject);
begin
  EditTimerInterval.Value:= TimerInterval;
  FileNameStart.Caption:= SoundStart;
  FileNameEnd.Caption:= SoundEnd;
  FileNameFinal.Caption:= SoundFinal;
  FileNameWarn.Caption:= SoundWarn;
  FileNameInit.Caption:= SoundInit;

  if Assigned(FReturnEvent) then
    FReturnEvent(self);
end;

procedure TFrameConfig.ButtonDefaultClick(Sender: TObject);
begin
  TimerInterval:= DefaultTimerInterval;
  SoundStart:= DefaultSoundStart;
  SoundEnd:= DefaultSoundEnd;
  SoundFinal:= DefaultSoundFinal;
  SoundWarn:= DefaultSoundWarn;
  SoundInit:= DefaultSoundInit;
  EditTimerInterval.Value:= DefaultTimerInterval;
  FileNameStart.Caption:= DefaultSoundStart;
  FileNameEnd.Caption:= DefaultSoundEnd;
  FileNameFinal.Caption:= DefaultSoundFinal;
  FileNameWarn.Caption:= DefaultSoundWarn;
  FileNameInit.Caption:= DefaultSoundInit;
end;

procedure TFrameConfig.ButtonOkClick(Sender: TObject);
begin
  TimerInterval:= EditTimerInterval.Value;
  SoundStart:= FileNameStart.Caption;
  SoundEnd:= FileNameEnd.Caption;
  SoundFinal:= FileNameFinal.Caption;
  SoundWarn:= FileNameWarn.Caption;
  SoundInit:= FileNameInit.Caption;

  if Assigned(FReturnEvent) then
    FReturnEvent(self);
end;

procedure TFrameConfig.ButtonPlaySound(Sender: TObject);
var
  component: TComponent;
begin
  if (NOT (Sender is TComponent)) then Exit;
  component:= Sender as TComponent;

  case component.Name of
    'ButtonPlayStart': PlaySound.SoundFile:= FileNameStart.Caption;
    'ButtonPlayEnd':   PlaySound.SoundFile:= FileNameEnd.Caption;
    'ButtonPlayFinal': PlaySound.SoundFile:= FileNameFinal.Caption;
    'ButtonPlayWarn':  PlaySound.SoundFile:= FileNameWarn.Caption;
    'ButtonPlayInit':  PlaySound.SoundFile:= FileNameInit.Caption;
  end;

  PlaySound.Execute;
end;

end.

