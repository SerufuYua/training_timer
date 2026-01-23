unit timer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TFrameTimer }

  TFrameTimer = class(TFrame)
    ButtonStop: TButton;
    ButtonPause: TButton;
    LabelTime: TLabel;
    LabelTitle: TLabel;
    PanelCounter: TPanel;
    PanelControl: TPanel;
  private

  public

  end;

implementation

{$R *.lfm}

end.

