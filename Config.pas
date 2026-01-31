unit Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, EditBtn;

type

  { TFrameConfig }

  TFrameConfig = class(TFrame)
    ButtonCancel: TButton;
    ButtonOk: TButton;
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
    PanelParameters: TPanel;
    PanelButtons: TPanel;
    EditTimerPeriod: TSpinEdit;
  private

  public

  end;

implementation

{$R *.lfm}

end.

