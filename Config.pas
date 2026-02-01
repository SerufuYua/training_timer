unit Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn,
  Buttons, XMLPropStorage;

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
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
  protected
    FReturnEvent: TNotifyEvent;
    FTimerInterval: Integer;
    FSoundStart, FSoundEnd, FSoundFinal, FSoundWarn, FSoundInit: String;
  public
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property ReturnEvent: TNotifyEvent write FReturnEvent;
    property TimerInterval: Integer read FTimerInterval;
    property SoundStart: String read FSoundStart;
    property SoundEnd: String read FSoundEnd;
    property SoundFinal: String read FSoundFinal;
    property SoundWarn: String read FSoundWarn;
    property SoundInit: String read FSoundInit;
  end;

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
  FTimerInterval:= APropStorage.ReadInteger(path + '/TimerInterval', DefaultTimerInterval);
  FSoundStart:= APropStorage.ReadString(path + '/SoundStart', DefaultSoundStart);
  FSoundEnd:= APropStorage.ReadString(path + '/SoundEnd', DefaultSoundEnd);
  FSoundFinal:= APropStorage.ReadString(path + '/SoundFinal', DefaultSoundFinal);
  FSoundWarn:= APropStorage.ReadString(path + '/SoundWarn', DefaultSoundWarn);
  FSoundInit:= APropStorage.ReadString(path + '/SoundInit', DefaultSoundInit);

  EditTimerInterval.Value:= FTimerInterval;
  FileNameStart.Caption:= FSoundStart;
  FileNameEnd.Caption:= FSoundEnd;
  FileNameFinal.Caption:= FSoundFinal;
  FileNameWarn.Caption:= FSoundWarn;
  FileNameInit.Caption:= FSoundInit;
end;

procedure TFrameConfig.SaveSettings(APropStorage: TXMLPropStorage);
var
  path: String;
begin
  path:= 'Config';
  APropStorage.WriteInteger(path + '/TimerInterval', FTimerInterval);
  APropStorage.WriteString(path + '/SoundStart', FSoundStart);
  APropStorage.WriteString(path + '/SoundEnd', FSoundEnd);
  APropStorage.WriteString(path + '/SoundFinal', FSoundFinal);
  APropStorage.WriteString(path + '/SoundWarn', FSoundWarn);
  APropStorage.WriteString(path + '/SoundInit', FSoundInit);
end;

procedure TFrameConfig.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FReturnEvent) then
    FReturnEvent(nil);
end;

procedure TFrameConfig.ButtonOkClick(Sender: TObject);
begin
  FTimerInterval:= EditTimerInterval.Value;
  FSoundStart:= FileNameStart.Caption;
  FSoundEnd:= FileNameEnd.Caption;
  FSoundFinal:= FileNameFinal.Caption;
  FSoundWarn:= FileNameWarn.Caption;
  FSoundInit:= FileNameInit.Caption;

  if Assigned(FReturnEvent) then
    FReturnEvent(nil);
end;

end.

