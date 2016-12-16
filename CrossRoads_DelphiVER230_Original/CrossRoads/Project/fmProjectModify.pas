unit fmProjectModify;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Filter.Effects, unStructure, FMX.Memo, FMX.Edit, FMX.Colors, FMX.ListBox,
  FMX.Ani;

type
  TfrmProjectModify = class(TForm)
    pnlBody: TRectangle;
    layHeader: TLayout;
    imgLogo: TImage;
    rectline: TRectangle;
    txtTitle: TText;
    layBottom: TLayout;
    imgOK: TImage;
    imgCancel: TImage;
    layButtons: TLayout;
    MonochromeEffect1: TMonochromeEffect;
    MonochromeEffect2: TMonochromeEffect;
    lblProjectName: TLabel;
    edtProjectName: TEdit;
    edtDescription: TMemo;
    lblDescription: TLabel;
    lblCreationDate: TLabel;
    lblModificationDate: TLabel;
    edtColor: TRectangle;
    procedure imgOKClick(Sender: TObject);
    procedure imgCancelClick(Sender: TObject);
  private
    FProject: TProjectRecord;
  public
    function Show(aProject: TProjectRecord): Boolean;
  end;

var
  frmProjectModify: TfrmProjectModify;

implementation

uses unGlobal;

{$R *.fmx}

function TfrmProjectModify.Show(aProject: TProjectRecord): Boolean;
begin
  FProject := aProject;
  edtProjectName.Text := FProject.ProjectName;
  edtDescription.Text := FProject.Description;
  lblCreationDate.Text := Format('Created on %s',[FormatDatetime(CST_FormatDatetime,FProject.CreationDate)]);
  lblModificationDate.Text := Format('Modified on %s',[FormatDatetime(CST_FormatDatetime,FProject.ModificationDate)]);
  edtColor.Fill.Color := FProject.ColorID;
  //
  edtProjectName.SetFocus;
  result := (ShowModal = mrOK);
end;

procedure TfrmProjectModify.imgCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmProjectModify.imgOKClick(Sender: TObject);
begin
  // update project
  FProject.ProjectName := edtProjectName.Text;
  FProject.Description := edtDescription.Text;
  FProject.ModificationDate := now;
  //FProject.ColorID := edtColor.Color;
  MainDatamodule.UpdateProject(FProject);
  //
  ModalResult := mrOK;
end;

end.
