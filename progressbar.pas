unit ProgressBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, Types;

type

  { TFrameProgress }

  TFrameProgress = class(TFrame)
    Painter: TPaintBox;
    procedure PainterPaint(Sender: TObject);
  protected
    FBorderColor, FProgressColor, FTextOutlineColor: TColor;
    FMinProgress, FMaxProgress, FProgress, FTextOutline, FBorder: Integer;
    FText: String;
    FFont: TFont;
    procedure WriteBorderColor(AValue: TColor);
    procedure WriteProgressColor(AValue: TColor);
    procedure WriteTextOutline(AValue: Integer);
    procedure WriteBorder(AValue: Integer);
    procedure WriteMinProgress(AValue: Integer);
    procedure WriteMaxProgress(AValue: Integer);
    procedure WriteProgress(AValue: Integer);
    procedure WriteFont(AValue: TFont);
    procedure WriteText(AValue: string);
    procedure WriteTextOutlineColor(AValue: TColor);
    procedure FontChanged(Sender: TObject); override;
    procedure CalculateFontSize;
  public
    const
      DefaultWidth = 150;
      DefaultHeight = 20;
      DefaultColor = clBtnFace;
      DefaultBorderColor = clBlack;
      DefaultProgressColor = clSkyBlue;
      DefaultTextColor = clBlack;
      DefaultTextOutlineColor = clWhite;
      DefaultTextOutline = 2;
      DefaultBorder = 2;
      DefaultMinProgress = 0;
      DefaultMaxProgress = 100;
      DefaultProgress = 50;
      DefaultText = '00:00';

    constructor Create(AOwner: TComponent); override;
    property Color default DefaultColor;
    property BorderColor: TColor read FBorderColor write WriteBorderColor default DefaultBorderColor;
    property ProgressColor: TColor read FProgressColor write WriteProgressColor default DefaultProgressColor;
    property TextOutlineColor: TColor read FTextOutlineColor write WriteTextOutlineColor default DefaultTextOutlineColor;
    property TextOutline: Integer read FTextOutline write WriteTextOutline default DefaultTextOutline;
    property Border: Integer read FBorder write WriteBorder default DefaultBorder;
    property MinProgress: Integer read FMinProgress write WriteMinProgress default DefaultMinProgress;
    property MaxProgress: Integer read FMaxProgress write WriteMaxProgress default DefaultMaxProgress;
    property Progress: Integer read FProgress write WriteProgress default DefaultProgress;
    property Font: TFont read FFont write WriteFont;
    property Text: string read FText write WriteText;
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
  FTextOutlineColor:= DefaultTextOutlineColor;
  FTextOutline:= DefaultTextOutline;
  FBorder:= DefaultBorder;
  FMinProgress:= DefaultMinProgress;
  FMaxProgress:= DefaultMaxProgress;
  FProgress:= DefaultProgress;
  FText:= DefaultText;

  FFont:= TFont.Create;
  FFont.OnChange:= @FontChanged;
  FFont.Color:= DefaultTextColor;
  FFont.Bold:= True;
  FFont.Quality:= fqDraft;
end;

procedure TFrameProgress.PainterPaint(Sender: TObject);
var
  ProgressWidth, TextX, TextY: Integer;
  ProgressRect, InnerRect: TRect;
  OldFont: TFont;
  TextSize: TSize;
begin
  { Draw border (1px) }
  Painter.Canvas.Pen.Color:= FBorderColor;
  Painter.Canvas.Brush.Color:= Color;
  Painter.Canvas.Rectangle(0, 0, Width, Height);

  { Inner rectangle with 1px offset from border }
  InnerRect:= Rect(1, 1, Width - 1, Height - 1);

  { Fill background }
  Painter.Canvas.FillRect(InnerRect);

  { Calculate progress width with proper offset }
  if FMaxProgress > FMinProgress then
    ProgressWidth:= Round((FProgress - FMinProgress) / (FMaxProgress - FMinProgress) * (InnerRect.Right - InnerRect.Left - 2))
  else
    ProgressWidth:= 0;

  { Draw progress bar with correct offset }
  if ProgressWidth > 0 then
  begin
    ProgressRect:= Rect(InnerRect.Left + FBorder,  { Left offset }
                        InnerRect.Top + FBorder,   { Top offset }
                        InnerRect.Left + FBorder + ProgressWidth,
                        InnerRect.Bottom - FBorder { Bottom offset } );

    Painter.Canvas.Brush.Color:= FProgressColor;
    Painter.Canvas.FillRect(ProgressRect);
  end;

  { Draw text }
  OldFont:= TFont.Create;
  try
    OldFont.Assign(Painter.Canvas.Font);
    Painter.Canvas.Font:= FFont;
    Painter.Canvas.Brush.Style := bsClear; { Transparent text background }

    { Calculate text position }
    CalculateFontSize;
    TextSize:= Painter.Canvas.TextExtent(FText);
    TextX:= (Width - TextSize.Width) div 2;
    TextY:= (Height - TextSize.Height) div 2;

    { Draw text outline }
    Painter.Canvas.Font.Color := FTextOutlineColor;
    Painter.Canvas.TextOut(TextX + FTextOutline, TextY + FTextOutline, FText);
    Painter.Canvas.TextOut(TextX - FTextOutline, TextY + FTextOutline, FText);
    Painter.Canvas.TextOut(TextX + FTextOutline, TextY - FTextOutline, FText);
    Painter.Canvas.TextOut(TextX - FTextOutline, TextY - FTextOutline, FText);

    { Draw main text }
    Painter.Canvas.Font.Color:= FFont.Color;
    Painter.Canvas.TextOut(TextX, TextY, FText);

    { Restore original font }
    Painter.Canvas.Font:= OldFont;
  finally
    OldFont.Free;
  end;
end;

procedure TFrameProgress.CalculateFontSize;
var
  i: Integer;
  TextSize: TSize;
begin
  { Calculate maximum font size }
  TextSize:= Painter.Canvas.TextExtent(FText);
  if ((TextSize.cx >= Width) OR (TextSize.cy >= Height)) then
  begin { too big }
    Painter.Canvas.Font.Size:= Painter.Canvas.Font.Size - 1;
    for i:= 1 to Height do
    begin
      TextSize:= Painter.Canvas.TextExtent(FText);
      if ((TextSize.cx >= Width) OR (TextSize.cy >= Height)) then
        Painter.Canvas.Font.Size:= Painter.Canvas.Font.Size - 1
      else
        Exit;
    end;
  end
  else
  begin { too small }
    Painter.Canvas.Font.Size:= Painter.Canvas.Font.Size + 1;
    for i:= 1 to Height do
    begin
      TextSize:= Painter.Canvas.TextExtent(FText);
      if ((TextSize.cx >= Width) OR (TextSize.cy >= Height)) then
      begin
        Painter.Canvas.Font.Size:= Painter.Canvas.Font.Size - 1;
        Exit;
      end
      else
        Painter.Canvas.Font.Size:= Painter.Canvas.Font.Size + 1
    end;
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

procedure TFrameProgress.WriteTextOutlineColor(AValue: TColor);
begin
  if FTextOutlineColor = AValue then Exit;
  FTextOutlineColor:= AValue;
  Invalidate;
end;

procedure TFrameProgress.WriteTextOutline(AValue: Integer);
begin
  if FTextOutline = AValue then Exit;
  FTextOutline:= AValue;
  Invalidate;
end;

procedure TFrameProgress.WriteBorder(AValue: Integer);
begin
  if FBorder = AValue then Exit;
  FBorder:= AValue;
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

procedure TFrameProgress.WriteFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TFrameProgress.WriteText(AValue: string);
begin
  if FText = AValue then Exit;
  FText:= AValue;
  Invalidate;
end;

procedure TFrameProgress.FontChanged(Sender: TObject);
begin
  inherited;
  Invalidate;
end;

end.

