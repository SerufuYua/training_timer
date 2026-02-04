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
    FMin, FMax, FProgress: Integer;
    procedure WriteBorderColor(AValue: TColor);
    procedure WriteProgressColor(AValue: TColor);
    procedure WriteMin(AValue: Integer);
    procedure WriteMax(AValue: Integer);
    procedure WriteProgress(AValue: Integer);
  public
    const
      DefaultWidth = 150;
      DefaultHeight = 20;
      DefaultColor = clBtnFace;
      DefaultBorderColor = clBlack;
      DefaultProgressColor = clSkyBlue;
      DefaultMin = 0;
      DefaultMax = 100;
      DefaultProgress = 50;

    constructor Create(AOwner: TComponent); override;
    property Color default DefaultColor;
    property BorderColor: TColor read FBorderColor write WriteBorderColor default DefaultBorderColor;
    property ProgressColor: TColor read FProgressColor write WriteProgressColor default DefaultProgressColor;
    property Min: Integer read FMin write WriteMin default DefaultMin;
    property Max: Integer read FMax write WriteMax default DefaultMax;
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
  FMin:= DefaultMin;
  FMax:= DefaultMax;
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
  if FMax > FMin then
    ProgressWidth:= Round((FProgress - FMin) / (FMax - FMin) * (InnerRect.Right - InnerRect.Left - 2))
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

procedure TFrameProgress.WriteMin(AValue: Integer);
begin
  if (FMin = AValue) then Exit;
  FMin:= AValue;
  if (FMin > FMax) then
    FMax:= FMin;
  if (FProgress < FMin) then
    FProgress:= FMin;
  Invalidate;
end;

procedure TFrameProgress.WriteMax(AValue: Integer);
begin
  if (FMax = AValue) then Exit;
  FMax:= AValue;
  if (FMax < FMin) then
    FMax:= FMin;
  if (FProgress > FMax) then
    FProgress:= FMax;
  Invalidate;
end;

procedure TFrameProgress.WriteProgress(AValue: Integer);
begin
  if (FProgress = AValue) then Exit;
  FProgress:= AValue;
  if (FProgress < FMin) then
    FProgress:= FMin;
  if (FProgress > FMax) then
    FProgress:= FMax;
  Invalidate;
end;

end.

