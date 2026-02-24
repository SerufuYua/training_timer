unit Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn,
  Buttons, XMLPropStorage, Graphics, uplaysound;

type

  TStopEvent = procedure of object;
  TSoundType = (Start, Ending, Final, Warn, Init);

  { TFrameConfig }

  TFrameConfig = class(TFrame)
    ButtonAbout: TButton;
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
    procedure ButtonAboutClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonDefaultClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonPlaySound(Sender: TObject);
  protected
    FReturnEvent, FAboutEvent: TNotifyEvent;
  public
    procedure LoadSettings(APropStorage: TXMLPropStorage);
    procedure SaveSettings(APropStorage: TXMLPropStorage);
    property ReturnEvent: TNotifyEvent write FReturnEvent;
    property AboutEvent: TNotifyEvent write FAboutEvent;
  end;

  function Sound(value: TSoundType): String;
  function TimerInterval: Integer;

  const
    DefaultName = 'New Set';
    DefaultRounds = 2;
    DefaultRoundTimeMs = 90000;
    DefaultRestTimeMs = 60000;
    DefaultPrepareTimeMs = 30000;
    DefaultWarningTimeMs = 10000;
    DefaultWarning = True;
    DefaultFinalSound = Ord(TSoundType.Start);
    DefaultColorStr = '';
    DefaultColorPrepare = clLime;
    DefaultColorRest = clYellow;
    DefaultColorRound = clRed;

implementation

var
  ATimerInterval: Integer;
  ASoundStart, ASoundEnding, ASoundFinal, ASoundWarn, ASoundInit: String;

const
  DefaultSoundStart = 'data\sound\start.wav';
  DefaultSoundEnding = 'data\sound\end.wav';
  DefaultSoundFinal = 'data\sound\fin.wav';
  DefaultSoundWarn = 'data\sound\warn.wav';
  DefaultSoundInit = 'data\sound\init.wav';
  DefaultTimerInterval = 100;
  ParamTimerInterval = 'TimerInterval';
  ParamSoundStart = 'SoundStart';
  ParamSoundEnding = 'SoundEnding';
  ParamSoundFinal = 'SoundFinal';
  ParamSoundWarn = 'SoundWarn';
  ParamSoundInit = 'SoundInit';

{$R *.lfm}

{ TFrameConfig }

procedure TFrameConfig.LoadSettings(APropStorage: TXMLPropStorage);
var
  path: String;
begin
  path:= 'Config/';
  ATimerInterval:= APropStorage.ReadInteger(path + ParamTimerInterval, DefaultTimerInterval);
  ASoundStart:= APropStorage.ReadString(path + ParamSoundStart, DefaultSoundStart);
  ASoundEnding:= APropStorage.ReadString(path + ParamSoundEnding, DefaultSoundEnding);
  ASoundFinal:= APropStorage.ReadString(path + ParamSoundFinal, DefaultSoundFinal);
  ASoundWarn:= APropStorage.ReadString(path + ParamSoundWarn, DefaultSoundWarn);
  ASoundInit:= APropStorage.ReadString(path + ParamSoundInit, DefaultSoundInit);

  EditTimerInterval.Value:= ATimerInterval;
  FileNameStart.Caption:= ASoundStart;
  FileNameEnd.Caption:= ASoundEnding;
  FileNameFinal.Caption:= ASoundFinal;
  FileNameWarn.Caption:= ASoundWarn;
  FileNameInit.Caption:= ASoundInit;
end;

procedure TFrameConfig.SaveSettings(APropStorage: TXMLPropStorage);
var
  path: String;
begin
  APropStorage.DoEraseSections(APropStorage.RootNodePath + '/Config');

  path:= 'Config/';
  if (ATimerInterval <> DefaultTimerInterval) then
    APropStorage.WriteInteger(path + ParamTimerInterval, ATimerInterval);

  if (ASoundStart <> DefaultSoundStart) then
    APropStorage.WriteString(path + ParamSoundStart, ASoundStart);

  if (ASoundEnding <> DefaultSoundEnding) then
    APropStorage.WriteString(path + ParamSoundEnding, ASoundEnding);

  if (ASoundFinal <> DefaultSoundFinal) then
    APropStorage.WriteString(path + ParamSoundFinal, ASoundFinal);

  if (ASoundWarn <> DefaultSoundWarn) then
    APropStorage.WriteString(path + ParamSoundWarn, ASoundWarn);

  if (ASoundInit <> DefaultSoundInit) then
    APropStorage.WriteString(path + ParamSoundInit, ASoundInit);
end;

procedure TFrameConfig.ButtonCancelClick(Sender: TObject);
begin
  EditTimerInterval.Value:= ATimerInterval;
  FileNameStart.Caption:= ASoundStart;
  FileNameEnd.Caption:= ASoundEnding;
  FileNameFinal.Caption:= ASoundFinal;
  FileNameWarn.Caption:= ASoundWarn;
  FileNameInit.Caption:= ASoundInit;

  if Assigned(FReturnEvent) then
    FReturnEvent(self);
end;

procedure TFrameConfig.ButtonAboutClick(Sender: TObject);
begin
  if Assigned(FAboutEvent) then
    FAboutEvent(self);
end;

procedure TFrameConfig.ButtonDefaultClick(Sender: TObject);
begin
  ATimerInterval:= DefaultTimerInterval;
  ASoundStart:= DefaultSoundStart;
  ASoundEnding:= DefaultSoundEnding;
  ASoundFinal:= DefaultSoundFinal;
  ASoundWarn:= DefaultSoundWarn;
  ASoundInit:= DefaultSoundInit;
  EditTimerInterval.Value:= DefaultTimerInterval;
  FileNameStart.Caption:= DefaultSoundStart;
  FileNameEnd.Caption:= DefaultSoundEnding;
  FileNameFinal.Caption:= DefaultSoundFinal;
  FileNameWarn.Caption:= DefaultSoundWarn;
  FileNameInit.Caption:= DefaultSoundInit;
end;

procedure TFrameConfig.ButtonOkClick(Sender: TObject);
begin
  ATimerInterval:= EditTimerInterval.Value;
  ASoundStart:= FileNameStart.Caption;
  ASoundEnding:= FileNameEnd.Caption;
  ASoundFinal:= FileNameFinal.Caption;
  ASoundWarn:= FileNameWarn.Caption;
  ASoundInit:= FileNameInit.Caption;

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

function Sound(value: TSoundType): String;
begin
  case value of
    TSoundType.Start: Result:= ASoundStart;
    TSoundType.Ending:   Result:= ASoundEnding;
    TSoundType.Final: Result:= ASoundFinal;
    TSoundType.Warn:  Result:= ASoundWarn;
    TSoundType.Init:  Result:= ASoundInit;
  else
    Result:= ASoundInit;
  end;
end;

function TimerInterval: Integer;
begin
  Result:= ATimerInterval;
end;

end.

