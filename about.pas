unit About;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TFrameAbout }

  TFrameAbout = class(TFrame)
    ButtonOk: TButton;
    LabelFPC: TLabel;
    LabelLaz: TLabel;
    LabelTitle: TLabel;
    LabelVer: TLabel;
    LabelUrl: TLabel;
    LabelWeb: TLabel;
    MemoAbout: TMemo;
    PanelVerInfo: TPanel;
    PanelWebUrl: TPanel;
    PanelInfo: TPanel;
    PanelButton: TPanel;
    procedure ButtonOkClick(Sender: TObject);
    procedure LabelUrlClick(Sender: TObject);
    procedure LabelUrlMouseEnter(Sender: TObject);
    procedure LabelUrlMouseLeave(Sender: TObject);
  protected
    FReturnEvent: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    property ReturnEvent: TNotifyEvent write FReturnEvent;
  end;

implementation

uses
  LCLIntf, Graphics, FileInfo, LCLVersion;

const
  FMouseLeave: TColor = clBlue;
  FMouseEnter: TColor = clRed;

{$R *.lfm}

{ TFrameAbout }

constructor TFrameAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with TVersionInfo.Create do
  begin
    Load(HINSTANCE);
    LabelTitle.Caption:= StringFileInfo.Items[0].Values['ProductName'];
    LabelVer.Caption:= 'Version ' + StringFileInfo.Items[0].Values['FileVersion'];
    Free;
  end;

  LabelLaz.Caption:= 'Lazarus ' + lcl_version;
  LabelFPC.Caption:= 'Free Pascal ' + {$I %FPCVERSION%};

  LabelUrl.Font.Color:= FMouseLeave;
end;

procedure TFrameAbout.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FReturnEvent) then
    FReturnEvent(self);
end;

procedure TFrameAbout.LabelUrlClick(Sender: TObject);
begin
  OpenURL('https://github.com/SerufuYua/training_timer');
end;

procedure TFrameAbout.LabelUrlMouseEnter(Sender: TObject);
begin
  with Sender as TLabel do
  begin
    Font.Style:= [fsUnderLine];
    Font.Color:= FMouseEnter;
    Cursor:= crHandPoint;
  end;
end;

procedure TFrameAbout.LabelUrlMouseLeave(Sender: TObject);
begin
  with Sender as TLabel do
  begin
    Font.Style:= [];
    Font.Color:= FMouseLeave;
    Cursor:= crDefault;
  end;
end;

end.

