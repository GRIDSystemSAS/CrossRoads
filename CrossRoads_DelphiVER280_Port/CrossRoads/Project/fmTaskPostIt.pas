unit fmTaskPostIt;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, unStructure, FMX.Objects,
  FMX.Layouts, FMX.Effects, FMX.Ani, FMX.Filter.Effects, unGlobal;

type
  TfrmTaskPostIt = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    rectBody: TRectangle;
    layLeft: TLayout;
    txtDescription: TText;
    rectNum: TRectangle;
    txtTaskNum: TText;
    imgNext: TImage;
    FloatAnimation1: TFloatAnimation;
    MonochromeEffect1: TMonochromeEffect;
    imgPrevious: TImage;
    FloatAnimation2: TFloatAnimation;
    MonochromeEffect2: TMonochromeEffect;
    imgDone: TImage;
    laySupp: TLayout;
    rectPriority: TRectangle;
    txtEstimation: TText;
    rectEstim: TRectangle;
    pnlEstim: TRectangle;
    imgDelete: TImage;
    FloatAnimation3: TFloatAnimation;
    MonochromeEffect3: TMonochromeEffect;
    imgDoneBox: TImage;
    imgSchedule: TImage;
    txtDoneDate: TText;
    txtDueDate: TText;
    procedure txtDescriptionMouseEnter(Sender: TObject);
    procedure txtDescriptionMouseLeave(Sender: TObject);
    procedure txtDescriptionClick(Sender: TObject);
    procedure imgDeleteMouseEnter(Sender: TObject);
    procedure imgDeleteMouseLeave(Sender: TObject);
    procedure imgNextMouseEnter(Sender: TObject);
    procedure imgNextMouseLeave(Sender: TObject);
    procedure imgPreviousMouseEnter(Sender: TObject);
    procedure imgPreviousMouseLeave(Sender: TObject);
  private
    FTask: TTask;
    FTaskRecord: TTaskRecord;
    FType: integer;
    procedure SetOnNextImageClick(aEvent: TNotifyEvent);
    procedure SetOnPreviousImageClick(aEvent: TNotifyEvent);
    procedure SetOnDeleteClick(aEvent: TNotifyEvent);
  public
    procedure Initialize(aType: integer); overload;
    procedure Initialize(aTask: TTask; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    property OnNextImageClick: TNotifyEvent write SetOnNextImageClick;
    property OnPreviousImageClick: TNotifyEvent write SetOnPreviousImageClick;
    property OnDeleteClick: TNotifyEvent write SetOnDeleteClick;
  end;

var
  frmTaskPostIt: TfrmTaskPostIt;

implementation

uses fmTargetModify, dmTypeList, fmWelcomeMain, unFunctions, fmTasks;

{$R *.fmx}

procedure TfrmTaskPostIt.Initialize(aType: integer);
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDescription.Text := '';
  txtTaskNum.Text := '';
  FType := aType;
  //
  imgPrevious.Visible := False;
  imgNext.Visible := False;
  imgDone.Visible := False;
  imgDoneBox.Visible := False;
  imgDelete.Visible := False;
  imgSchedule.Visible := False;
end;

procedure TfrmTaskPostIt.imgDeleteMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Delete a to-do task...');
end;

procedure TfrmTaskPostIt.imgDeleteMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPostIt.imgNextMouseEnter(Sender: TObject);
begin
  case FType of
    CST_TargetTodo : frmWelcomeMain.ShowHint('Schedule to current week...');
    CST_TargetScheduled : frmWelcomeMain.ShowHint('Postpone to next week...');
    CST_TargetScheduledWaiting : frmWelcomeMain.ShowHint('Start a task...');
  end;
end;

procedure TfrmTaskPostIt.imgNextMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPostIt.imgPreviousMouseEnter(Sender: TObject);
begin
  case FType of
    CST_TargetScheduled : frmWelcomeMain.ShowHint('Cancel a schedule...');
    CST_TargetScheduledDone : frmWelcomeMain.ShowHint('Restart a task...');
  end;
end;

procedure TfrmTaskPostIt.imgPreviousMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPostIt.Initialize(aTask: TTask; aDelay: single);
var
  lProject: TProject;
begin
  FTask := aTask;
  FTaskRecord := FTask.Data;
  layVisible.Visible := True;
  imgNext.Tag := FTaskRecord.TaskNum;
  imgDelete.Tag := FTaskRecord.TaskNum;
  imgPrevious.Tag := FTaskRecord.TaskNum;
  //
  RefreshData(aDelay);
  //
  lProject := GlobalVar.ProjectList.Search(FTaskRecord.ProjectNum);
  if (FTaskRecord.ParentTaskNum = CST_UnclassifiedTodo) then
  begin
    rectNum.Fill.Color := TAlphaColorRec.Whitesmoke;
    txtTaskNum.Font.Style := [TFontStyle.fsBold, TFontStyle.fsItalic];
  end
  else
  begin
    rectNum.Fill.Color := lProject.Data.ColorID;
    txtTaskNum.Font.Style := [TFontStyle.fsBold];
  end;
end;

procedure TfrmTaskPostIt.RefreshData(aDelay: single);
begin
  txtDescription.Text := FTaskRecord.TaskName;
  txtTaskNum.Text := IntToStr(FTaskRecord.ProjectTaskNum);
  txtDoneDate.Text := FormatDatetime(CST_DateMonth, FTaskRecord.DoneDate);
  //
  txtDoneDate.Visible := (FType = CST_TargetResume);
  imgPrevious.Visible := (FType <> CST_Target)
                          and (FType <> CST_TargetTodo)
                          and (FType <> CST_TargetScheduledWaiting)
                          and (FType <> CST_TargetPendingInWelcomePage)
                          and (FType <> CST_TargetResume)
                          and (FTaskRecord.ParentTaskNum <> CST_UnclassifiedTodo)
                          or ((FTaskRecord.Status = staDone) and (FType <> CST_TargetTodo));
  imgNext.Visible := (FType <> CST_TargetScheduledDone)
                      and (FType <> CST_TargetPendingInWelcomePage)
                      and (FType <> CST_TargetResume)
                      and (FTaskRecord.Status <> staDone)
                      and (FTaskRecord.Estimation > 0);
  if (FType = CST_TargetTodo)
  and ((FTaskRecord.Status=staScheduled) or (FTaskRecord.Status=staInProgress)) then
  begin
    imgNext.Visible := False;
    imgSchedule.Visible := True;
  end;
  imgDone.Visible := (FTaskRecord.Status = staDone);
  imgDoneBox.Visible := (FTaskRecord.Status = staDone)
                        and (FType <> CST_TargetResume);
  imgDelete.Visible := False;
  //
  rectPriority.Fill.Color := PriorityToColor(FTaskRecord.Priority);
  txtDueDate.Text := '';
  //txtDueDate.Visible := False;
  if FTaskRecord.DueDate<>0 then
  begin
    txtDueDate.Text := FormatDatetime(CST_DateMonth, FTaskRecord.DueDate);
    //txtDueDate.Visible := True;
  end;
  txtEstimation.Text := Format('%d',[FTaskRecord.Estimation]);
//  rectBody.Fill.Color := TAlphaColorRec.White;
//  if (FTaskRecord.ParentTaskNum = CST_UnclassifiedTodo) then
//  begin
//    txtEstimation.Text := txtEstimation.Text + ' (U)';
//    rectBody.Fill.Color := TAlphaColorRec.Mintcream;
//  end;
  pnlEstim.Width := (FTaskRecord.Estimation/GlobalVar.WeekCapacity)*rectEstim.width;
  if (FTaskRecord.Status=staDone)
     and (FType <> CST_TargetPendingInWelcomePage)
     and (FType <> CST_TargetResume) then
  begin
    txtTaskNum.TextSettings.FontColor := TAlphaColorRec.Gray;
    txtDescription.TextSettings.FontColor := TAlphaColorRec.Gray;
  end
  else
  begin
    txtTaskNum.TextSettings.FontColor := TAlphaColorRec.Black;
    txtDescription.TextSettings.FontColor := TAlphaColorRec.Black;
  end;
  if (FTaskRecord.Status=staValidated)
    or ((FType = CST_TargetScheduledWaiting)
    and (FTaskRecord.ParentTaskNum = CST_UnclassifiedTodo)) then
    imgDelete.Visible := (TaskDatamodule.GetChildrenCount(FTaskRecord.ProjectNum, FTaskRecord.TaskNum)=0);
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmTaskPostIt.SetOnDeleteClick(aEvent: TNotifyEvent);
begin
  imgDelete.OnClick := aEvent;
end;

procedure TfrmTaskPostIt.SetOnNextImageClick(aEvent: TNotifyEvent);
begin
  imgNext.OnClick := aEvent;
end;

procedure TfrmTaskPostIt.SetOnPreviousImageClick(aEvent: TNotifyEvent);
begin
  imgPrevious.OnClick := aEvent;
end;



procedure TfrmTaskPostIt.txtDescriptionClick(Sender: TObject);
begin
  if (FTaskRecord.Status=staDone) or (FType = CST_TargetResume) then
  begin
    frmWelcomeMain.ShowTarget(FTask);
  end
  else
  begin
    if frmTargetModify.Show(FTaskRecord, FType) then
    begin
      FTask.RefreshData;
      FTaskRecord := FTask.Data;
      layVisible.Opacity := 0;
      RefreshData(0);
      case FType of
        CST_TargetTodo: frmTasks.UpdateToDoInfos;
      end;
    end;
  end;
end;

procedure TfrmTaskPostIt.txtDescriptionMouseEnter(Sender: TObject);
begin
  txtDescription.Font.Style := [TFontStyle.fsBold];
end;

procedure TfrmTaskPostIt.txtDescriptionMouseLeave(Sender: TObject);
begin
  txtDescription.Font.Style := [];
end;

end.
