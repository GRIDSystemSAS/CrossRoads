unit fmLoading;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.StdCtrls;

type
  TfrmLoading = class(TForm)
    rectBody: TRectangle;
    layMain: TLayout;
    imgLogo: TImage;
    imgLoading: TImage;
    rectProgress: TRectangle;
    txtProgress: TText;
    pnlProgress: TRectangle;
    txtPercent: TText;
    txtVersion: TText;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    procedure Initialize;
    procedure ShowProgress(aText: string; aPercent: single);
  end;

var
  frmLoading: TfrmLoading;

implementation

uses unGlobal;

{$R *.fmx}

{ TfrmLoading }

procedure TfrmLoading.FormCreate(Sender: TObject);
begin
  txtVersion.Text := Format('Release V%s',[CST_ApplicationVersion]);
end;

procedure TfrmLoading.Initialize;
begin
  txtProgress.Text := '';
  pnlProgress.Width := 0;
  txtPercent.Text := '0%';
  btnClose.Visible := False;
  Application.ProcessMessages;
end;

procedure TfrmLoading.ShowProgress(aText: string; aPercent: single);
begin
  txtProgress.Text := aText;
  pnlProgress.Width := rectProgress.Width * aPercent / 100;
  txtPercent.Text := FloatToStr(aPercent)+'%';
  //btnClose.Visible := (aPercent>=100);
  Application.ProcessMessages;
end;

end.
