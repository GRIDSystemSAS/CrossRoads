unit fmTargetModify;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Filter.Effects, unStructure, FMX.Memo, FMX.Edit, FMX.Colors, FMX.ListBox,
  FMX.Ani, unGlobal, FMX.ExtCtrls;

type
  TfrmTargetModify = class(TForm)
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
    layTechs: TLayout;
    lblStatus: TLabel;
    edtPriority: TTrackBar;
    txtPriority: TText;
    lblPriority: TText;
    edtEstimation: TTrackBar;
    txtEstimation: TText;
    lblEstimation: TText;
    txtError: TText;
    rectPriority: TRectangle;
    txtDueDate: TText;
    edtDueDate: TCalendarEdit;
    imgDelete: TImage;
    FloatAnimation3: TFloatAnimation;
    MonochromeEffect3: TMonochromeEffect;
    procedure imgOKClick(Sender: TObject);
    procedure imgCancelClick(Sender: TObject);
    procedure edtPriorityChange(Sender: TObject);
    procedure edtEstimationChange(Sender: TObject);
    procedure imgDeleteClick(Sender: TObject);
  private
    FTask: TTaskRecord;
    FProject: TProject;
  public
    function Show(aTask: TTaskRecord; aType: integer): Boolean;
  end;

var
  frmTargetModify: TfrmTargetModify;

implementation

uses unFunctions;

{$R *.fmx}

function TfrmTargetModify.Show(aTask: TTaskRecord; aType: integer): Boolean;
begin
  edtEstimation.Max := GlobalVar.WeekCapacity;
  //
  FTask := aTask;
  FProject := GlobalVar.ProjectList.Search(FTask.ProjectNum);
  txtError.Visible := False;
  //
  case aType of
    CST_Target : txtTitle.Text := 'TARGET Insert/Modify...';
    CST_TargetTodo, CST_TargetTodoUnclassified,
     CST_TargetScheduled, CST_TargetScheduledWaiting : txtTitle.Text := 'TO-DO Insert/Modify...';
    CST_TargetTask : txtTitle.Text := 'TASK Insert/Modify...';
    CST_TargetComment : txtTitle.Text := 'COMMENT Insert/Modify...';
    CST_TargetLink : txtTitle.Text := 'LINK Insert/Modify...';
  end;
  if (aType=CST_TargetLink) then
    lblDescription.Text := 'Link'
  else
    lblDescription.Text := 'Description';
  rectNum.Visible := (aType = CST_TargetTodo)
                     or (aType = CST_TargetTodoUnclassified)
                     or (aType = CST_TargetScheduled)
                     or (aType = CST_TargetScheduledWaiting);
  //
  edtTaskName.Text := FTask.TaskName;
  edtDescription.Text := FTask.Description;
  edtPriority.Value := FTask.Priority;
  edtPriorityChange(self);
  edtPriority.Visible := (aType = CST_TargetTodo)
                         or (aType = CST_TargetTodoUnclassified)
                         or (aType = CST_TargetScheduled)
                         or (aType = CST_TargetScheduledWaiting);
  edtEstimation.Value := FTask.Estimation;
  edtEstimationChange(self);
  edtEstimation.Visible := (aType = CST_TargetTodo)
                           or (aType = CST_TargetTodoUnclassified)
                           or (aType = CST_TargetScheduled)
                           or (aType = CST_TargetScheduledWaiting);
  if FTask.DueDate = 0 then
    edtDueDate.Text := ''
  else
    edtDueDate.Date := FTask.DueDate;
  txtDueDate.Visible := (aType = CST_TargetTodo)
                           or (aType = CST_TargetTodoUnclassified)
                           or (aType = CST_TargetScheduled)
                           or (aType = CST_TargetScheduledWaiting);
  //
  lblCreationDate.Text := Format('Created on %s',[FormatDatetime(CST_FormatDatetime,FTask.CreationDate)]);
  lblModificationDate.Text := Format('Modified on %s',[FormatDatetime(CST_FormatDatetime,FTask.ModificationDate)]);
  edtColor.Fill.Color := FProject.Data.ColorID;
  txtProjectTaskNum.Text := Format('%d',[FTask.ProjectTaskNum]);
  lblStatus.Text := TypeDatamodule.GetStatusLabel(FTask.Status);
  //
  edtTaskName.SetFocus;
  result := (ShowModal = mrOK);
end;

procedure TfrmTargetModify.edtEstimationChange(Sender: TObject);
var
  lIntValue : integer;
begin
  lIntValue := round(edtEstimation.Value);
  edtEstimation.Value := lIntValue;
  txtEstimation.Text :=  Format('(%d)',[lIntValue]);
end;

procedure TfrmTargetModify.edtPriorityChange(Sender: TObject);
var
  lIntValue : integer;
begin
  lIntValue := round(edtPriority.Value);
  edtPriority.Value := lIntValue;
  txtPriority.Text :=  TypeDatamodule.GetPriorityLabel(lIntValue);
  rectPriority.Fill.Color := PriorityToColor(lIntValue);
end;

procedure TfrmTargetModify.imgCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmTargetModify.imgDeleteClick(Sender: TObject);
begin
  edtTaskName.SetFocus;
  edtDueDate.Date := 0;
  edtDueDate.Text := '';
end;

procedure TfrmTargetModify.imgOKClick(Sender: TObject);
var
  lIntValue : integer;
begin
  txtError.Visible := False;
  if trim(edtTaskName.Text)='' then
  begin
    txtError.Visible := True;
    edtTaskName.SetFocus;
    exit;
  end;
  // update project
  FTask.TaskName := edtTaskName.Text;
  FTask.Description := edtDescription.Text;
  FTask.ModificationDate := now;
  lIntValue := round(edtPriority.Value);
  FTask.Priority := lIntValue;
  lIntValue := round(edtEstimation.Value);
  FTask.Estimation := lIntValue;
  if edtDueDate.Text = '' then
    FTask.DueDate := 0
  else
    FTask.DueDate := edtDueDate.Date;
  //
  TaskDatamodule.UpdateTask(FTask);
  //
  ModalResult := mrOK;
end;

end.
