unit About;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TFrameAbout }

  TFrameAbout = class(TFrame)
    ButtonOk: TButton;
    MemoAbout: TMemo;
    PanelInfo: TPanel;
    PanelButton: TPanel;
    procedure ButtonOkClick(Sender: TObject);
  protected
    FReturnEvent: TNotifyEvent;
  public
    property ReturnEvent: TNotifyEvent write FReturnEvent;
  end;

implementation

{$R *.lfm}

{ TFrameAbout }

procedure TFrameAbout.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FReturnEvent) then
    FReturnEvent(self);
end;

end.

