unit EditTime;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Spin, StdCtrls;

type

  { TFrameEditTime }

  TFrameEditTime = class(TFrame)
    EditSec: TSpinEdit;
    LabelMin: TLabel;
    LabelSec: TLabel;
    PanelMin: TPanel;
    EditMin: TSpinEdit;
    PanelSec: TPanel;
    procedure EditTimeChange(Sender: TObject);
  protected
    FOnChange: TNotifyEvent;
    procedure WriteValue(AValue: Integer);
    function ReadValue: Integer;
  public
    property Value: Integer read ReadValue write WriteValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.lfm}

procedure TFrameEditTime.EditTimeChange(Sender: TObject);
begin
  if (EditSec.Value > 59) then
  begin
    EditSec.Value:= 0;
    EditMin.Value:= EditMin.Value + 1;
  end
  else
  if (EditSec.Value < 0) then
  begin
    if (EditMin.Value > 0) then
    begin
      EditMin.Value:= EditMin.Value - 1;
      EditSec.Value:= 59;
    end
    else
    begin
      EditSec.Value:= 0;
    end;
  end;

  if (EditMin.Value < 0) then
    EditMin.Value:= 0;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TFrameEditTime.WriteValue(AValue: Integer);
var
  min, sec: Integer;
begin
  min:= AValue div 60;
  sec:= AValue - (min * 60);

  EditSec.Value:= sec;
  EditMin.Value:= min;
end;

function TFrameEditTime.ReadValue: Integer;
begin
  Result:= (EditMin.Value * 60) + EditSec.Value;
end;

end.

