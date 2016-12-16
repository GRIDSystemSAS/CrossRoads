unit fmFolder;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Filter.Effects, FMX.Ani;

type
  TfrmFolder = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    rectFolder: TRectangle;
    txtFolder: TText;
    procedure txtFolderClick(Sender: TObject);
    procedure txtFolderMouseEnter(Sender: TObject);
    procedure txtFolderMouseLeave(Sender: TObject);
  private
    FFolder: string;
  public
    procedure Initialize; overload;
    procedure Initialize(aFolder: string; aDelay: single= 0); overload;
  end;

var
  frmFolder: TfrmFolder;

implementation

uses fmWelcomeMain;

{$R *.fmx}

procedure TfrmFolder.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtFolder.Text := '';
  FFolder := '';
end;

procedure TfrmFolder.Initialize(aFolder: string; aDelay: single= 0);
begin
  FFolder := aFolder;
  txtFolder.Text := UpperCase(FFolder);
  //
  layVisible.Visible := True;
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmFolder.txtFolderClick(Sender: TObject);
begin
  frmWelcomeMain.SaveEnvironment;
  frmWelcomeMain.InitializeFolder(FFolder);
end;

procedure TfrmFolder.txtFolderMouseEnter(Sender: TObject);
begin
  txtFolder.Fill.Color := TAlphaColorRec.Dimgray;
end;

procedure TfrmFolder.txtFolderMouseLeave(Sender: TObject);
begin
  txtFolder.Fill.Color := TAlphaColorRec.Silver;
end;

end.
