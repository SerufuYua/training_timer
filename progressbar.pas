unit ProgressBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics;

type

  { TFrameProgress }

  TFrameProgress = class(TFrame)
    PaintProgress: TPaintBox;
    procedure PaintProgressPaint(Sender: TObject);
  protected
    FBorderColor, FProgressColor: TColor;
    FMinProgress, FMaxProgress, FProgress: Integer;
    procedure WriteBorderColor(AValue: TColor);
    procedure WriteProgressColor(AValue: TColor);
    procedure WriteMinProgress(AValue: Integer);
    procedure WriteMaxProgress(AValue: Integer);
    procedure WriteProgress(AValue: Integer);
  public
    const
      DefaultWidth = 150;
      DefaultHeight = 20;
      DefaultColor = clBtnFace;
      DefaultBorderColor = clBlack;
      DefaultProgressColor = clSkyBlue;
      DefaultMinProgress = 0;
      DefaultMaxProgress = 100;
      DefaultProgress = 50;

    constructor Create(AOwner: TComponent); override;
    property Color default DefaultColor;
    property BorderColor: TColor read FBorderColor write WriteBorderColor default DefaultBorderColor;
    property ProgressColor: TColor read FProgressColor write WriteProgressColor default DefaultProgressColor;
    property MinProgress: Integer read FMinProgress write WriteMinProgress default DefaultMinProgress;
    property MaxProgress: Integer read FMaxProgress write WriteMaxProgress default DefaultMaxProgress;
    property Progress: Integer read FProgress write WriteProgress default DefaultProgress;
  end;

implementation

{$R *.lfm}

{ TFrameProgress }

constructor TFrameProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width:= DefaultWidth;
  Height:= DefaultHeight;
  Color:= DefaultColor;
  FBorderColor:= DefaultBorderColor;
  FProgressColor:= DefaultProgressColor;
  FMinProgress:= DefaultMinProgress;
  FMaxProgress:= DefaultMaxProgress;
  FProgress:= DefaultProgress;
end;

procedure TFrameProgress.PaintProgressPaint(Sender: TObject);
var
  ProgressWidth: Integer;
  ProgressRect, InnerRect: TRect;
begin
  { Draw border (1px) }
  PaintProgress.Canvas.Pen.Color:= FBorderColor;
  PaintProgress.Canvas.Brush.Color:= Color;
  PaintProgress.Canvas.Rectangle(0, 0, Width, Height);

  { Inner rectangle with 1px offset from border }
  InnerRect:= Rect(1, 1, Width - 1, Height - 1);

  { Fill background }
  PaintProgress.Canvas.FillRect(InnerRect);

  { Calculate progress width with proper offset }
  if FMaxProgress > FMinProgress then
    ProgressWidth:= Round((FProgress - FMinProgress) / (FMaxProgress - FMinProgress) * (InnerRect.Right - InnerRect.Left - 2))
  else
    ProgressWidth:= 0;

  { Draw progress bar with correct offset }
  if ProgressWidth > 0 then
  begin
    ProgressRect:= Rect(InnerRect.Left + 1,  { Left offset }
                        InnerRect.Top + 1,   { Top offset }
                        InnerRect.Left + 1 + ProgressWidth,
                        InnerRect.Bottom - 1 { Bottom offset } );

    PaintProgress.Canvas.Brush.Color:= FProgressColor;
    PaintProgress.Canvas.FillRect(ProgressRect);
  end;
end;

procedure TFrameProgress.WriteBorderColor(AValue: TColor);
begin
  if (FBorderColor = AValue) then Exit;
  FBorderColor:= AValue;
  Invalidate;
end;

procedure TFrameProgress.WriteProgressColor(AValue: TColor);
begin
  if (FProgressColor = AValue) then Exit;
  FProgressColor:= AValue;
  Invalidate;
end;

procedure TFrameProgress.WriteMinProgress(AValue: Integer);
begin
  if (FMinProgress = AValue) then Exit;
  FMinProgress:= AValue;
  if (FMinProgress > FMaxProgress) then
    FMaxProgress:= FMinProgress;
  if (FProgress < FMinProgress) then
    FProgress:= FMinProgress;
  Invalidate;
end;

procedure TFrameProgress.WriteMaxProgress(AValue: Integer);
begin
  if (FMaxProgress = AValue) then Exit;
  FMaxProgress:= AValue;
  if (FMaxProgress < FMinProgress) then
    FMaxProgress:= FMinProgress;
  if (FProgress > FMaxProgress) then
    FProgress:= FMaxProgress;
  Invalidate;
end;

procedure TFrameProgress.WriteProgress(AValue: Integer);
begin
  if (FProgress = AValue) then Exit;
  FProgress:= AValue;
  if (FProgress < FMinProgress) then
    FProgress:= FMinProgress;
  if (FProgress > FMaxProgress) then
    FProgress:= FMaxProgress;
  Invalidate;
end;

end.

