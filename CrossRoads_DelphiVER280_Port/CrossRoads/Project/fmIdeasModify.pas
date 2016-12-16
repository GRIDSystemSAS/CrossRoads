unit fmIdeasModify;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Filter.Effects, unStructure, FMX.Memo, FMX.Edit, FMX.Colors, FMX.ListBox,
  FMX.Ani, unGlobal;

type
  TfrmIdeaModify = class(TForm)
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
    lblTaskName: TLabel;
    edtTaskName: TEdit;
    edtDescription: TMemo;
    lblDescription: TLabel;
    lblCreationDate: TLabel;
    lblModificationDate: TLabel;
    edtColor: TRectangle;
    rectNum: TRectangle;
    txtProjectTaskNum: TText;
    procedure imgOKClick(Sender: TObject);
    procedure imgCancelClick(Sender: TObject);
  private
    FTask: TTaskRecord;
    FProject: TProject;
  public
    function Show(aTask: TTaskRecord): Boolean;
  end;

var
  frmIdeaModify: TfrmIdeaModify;

implementation

{$R *.fmx}

function TfrmIdeaModify.Show(aTask: TTaskRecord): Boolean;
begin
  FTask := aTask;
  FProject := GlobalVar.ProjectList.Search(FTask.ProjectNum);
  //
  edtTaskName.Text := FTask.TaskName;
  edtDescription.Text := FTask.Description;
  lblCreationDate.Text := Format('Created on %s',[FormatDatetime(CST_FormtDatetime,FTask.CreationDate)]);
  lblModificationDate.Text := Format('Modified on %s',[FormatDatetime(CST_FormtDatetime,FTask.ModificationDate)]);
  edtColor.Fill.Color := FProject.Data.ColorID;
  txtProjectTaskNum.Text := Format('%d',[FTask.ProjectTaskNum]);
  //
  edtTaskName.SetFocus;
  result := (ShowModal = mrOK);
end;

procedure TfrmIdeaModify.imgCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmIdeaModify.imgOKClick(Sender: TObject);
var
  lProjectRec: TProjectRecord;
begin
  // update project
  FTask.TaskName := edtTaskName.Text;
  FTask.Description := edtDescription.Text;
  FTask.ModificationDate := now;
  MainDatamodule.UpdateTask(FTask);
  //
  ModalResult := mrOK;
end;

end.
