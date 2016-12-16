unit fmAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Filter.Effects, FMX.Memo, FMX.Effects;

type
  TfrmAbout = class(TForm)
    rectBody: TRectangle;
    imgLogo: TImage;
    rectVersion: TRectangle;
    txtVersion: TText;
    layBottom: TLayout;
    txtCopyright: TText;
    img2C: TImage;
    imgClose: TImage;
    MonochromeEffect1: TMonochromeEffect;
    layCopyright: TLayout;
    txtApplicationName: TText;
    layButtons: TLayout;
    txtInfos: TMemo;
    procedure imgCloseClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    procedure Initialize;
  end;

var
  frmAbout: TfrmAbout;

implementation

uses unGLobal;

{$R *.fmx}

{ TfrmAbout }

procedure TfrmAbout.imgCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmAbout.Initialize;
begin
  txtVersion.Text := Format('Version %s',[CST_ApplicationVersion]);
  txtApplicationName.Text := CST_ApplicationLongName;
  //
  txtInfos.Lines.Clear;
  txtInfos.Lines.Add(Format('DataDir= %s',[GlobalVar.DataDirectory]));
  txtInfos.Lines.Add(Format('ProjectNum= %d',[GlobalVar.CurrentProjectNum]));
end;

end.
