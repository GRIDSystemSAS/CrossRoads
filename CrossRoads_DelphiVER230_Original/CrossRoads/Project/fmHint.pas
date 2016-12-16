unit fmHint;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts;

type
  TfrmHint = class(TForm)
    layBody: TLayout;
    imgHelp: TImage;
    rectHelp: TRectangle;
    txtHelp: TText;
    TimerHide: TTimer;
    procedure TimerHideTimer(Sender: TObject);
  private
    { Déclarations privées }
  public
    procedure ShowHint(aHint: string);
    procedure Initialize;
    procedure HideHint;
  end;

var
  frmHint: TfrmHint;

implementation

{$R *.fmx}

{ TfrmHint }

procedure TfrmHint.HideHint;
begin
  TimerHide.Enabled := False;
  txtHelp.Text := '';
  layBody.AnimateFloat('Opacity',0,0.5);
  layBody.Visible := False;
end;

procedure TfrmHint.Initialize;
begin
  TimerHide.Enabled := False;
  txtHelp.Text := '';
  layBody.Opacity := 0;
  layBody.Visible := False;
end;

procedure TfrmHint.ShowHint(aHint: string);
var
  lLen: integer;
begin
  layBody.Visible := True;
  txtHelp.Text := aHint;
  lLen := Length(aHint);
  rectHelp.Width := lLen*7;
  layBody.AnimateFloatDelay('Opacity',1,0.5,1);
  TimerHide.Interval := 3000;
  TimerHide.Enabled := True;
end;

procedure TfrmHint.TimerHideTimer(Sender: TObject);
begin
  HideHint;
end;

end.
