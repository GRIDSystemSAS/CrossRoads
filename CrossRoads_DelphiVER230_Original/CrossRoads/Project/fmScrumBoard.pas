unit fmScrumBoard;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  FMX.Filter.Effects, FMX.Objects, fmSprintCal, unStructure, unGlobal;

type
  TfrmScrumBoard = class(TForm)
    layBody: TLayout;
    imgAddTodo: TImage;
    MonochromeEffect2: TMonochromeEffect;
    layWaiting: TLayout;
    rectWaiting: TRectangle;
    imgWaiting: TImage;
    imgWaitingNext: TImage;
    layWaitingContent: TLayout;
    layProgress: TLayout;
    rectInProgress: TRectangle;
    imgInProgress: TImage;
    imgProgressNext: TImage;
    layProgressContent: TLayout;
    layDone: TLayout;
    rectDone: TRectangle;
    imgDonePrev: TImage;
    Image5: TImage;
    layDoneContent: TLayout;
    layLeft: TLayout;
    rectLeft: TRectangle;
    layCalendar1: TLayout;
    imgProgressPrev: TImage;
    imgWaitingDelete: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgWaitingNextClick(Sender: TObject);
    procedure imgProgressNextClick(Sender: TObject);
    procedure imgProgressPrevClick(Sender: TObject);
    procedure imgDonePrevClick(Sender: TObject);
    procedure imgAddTodoClick(Sender: TObject);
    procedure imgWaitingDeleteClick(Sender: TObject);
    procedure imgAddTodoMouseEnter(Sender: TObject);
    procedure imgAddTodoMouseLeave(Sender: TObject);
  private
    FTaskList: TTaskList;
    FfrmSprint1: TfrmSprintCal;
    FfrmWaitingList: TList;
    FfrmProgressList: TList;
    FfrmDoneList: TList;
    procedure InitWaiting;
    procedure InitProgress;
    procedure InitDone;
  public
    procedure Initialize;
  end;

var
  frmScrumBoard: TfrmScrumBoard;

implementation

uses fmTaskPostIt, unFunctions, dmTypeList, fmTaskPending, fmTargetModify,
fmWelcomeMain;

{$R *.fmx}

procedure TfrmScrumBoard.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTaskPostIt : TfrmTaskPostIt;
  lfrmTaskPending : TfrmTaskPending;
begin
  FTaskList := TTaskList.Create;
  //
  FfrmSprint1 := TfrmSprintCal.Create(self);
  FfrmSprint1.layBody.Parent := layCalendar1;
  // Waiting
  FfrmWaitingList := TList.Create;
  for i := 0 to CST_Max_TodosToShow-1 do
  begin
    lfrmTaskPostIt := TfrmTaskPostIt.Create(nil);
    lfrmTaskPostIt.layBody.Parent := layWaitingContent;
    lfrmTaskPostIt.OnNextImageClick := imgWaitingNextClick;
    lfrmTaskPostIt.OnDeleteClick := imgWaitingDeleteClick;
    lfrmTaskPostIt.Initialize(CST_TargetScheduledWaiting);
    FfrmWaitingList.Add(lfrmTaskPostIt);
  end;
  //
  for i := CST_Max_TodosToShow-1 downto 0 do
  begin
    lfrmTaskPostIt := FfrmWaitingList[i];
    lfrmTaskPostIt.layBody.Align := TAlignLayout.alTop;
  end;
  // In Progress
  FfrmProgressList := TList.Create;
  for i := 0 to CST_Max_PendingsToShow-1 do
  begin
    lfrmTaskPending := TfrmTaskPending.Create(nil);
    lfrmTaskPending.layBody.Parent := layProgressContent;
    lfrmTaskPending.OnNextImageClick := imgProgressNextClick;
    lfrmTaskPending.OnPreviousImageClick := imgProgressPrevClick;
    lfrmTaskPending.Initialize(CST_TargetScheduledInProgress);
    FfrmProgressList.Add(lfrmTaskPending);
  end;
  //
  for i := CST_Max_PendingsToShow-1 downto 0 do
  begin
    lfrmTaskPending := FfrmProgressList[i];
    lfrmTaskPending.layBody.Align := TAlignLayout.alTop;
  end;
  // Done
  FfrmDoneList := TList.Create;
  for i := 0 to CST_Max_TodosToShow-1 do
  begin
    lfrmTaskPostIt := TfrmTaskPostIt.Create(nil);
    lfrmTaskPostIt.layBody.Parent := layDoneContent;
    lfrmTaskPostIt.OnPreviousImageClick := imgDonePrevClick;
    lfrmTaskPostIt.Initialize(CST_TargetScheduledDone);
    FfrmDoneList.Add(lfrmTaskPostIt);
  end;
  //
  for i := CST_Max_TodosToShow-1 downto 0 do
  begin
    lfrmTaskPostIt := FfrmDoneList[i];
    lfrmTaskPostIt.layBody.Align := TAlignLayout.alTop;
  end;
end;

procedure TfrmScrumBoard.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTaskPostIt : TfrmTaskPostIt;
  lfrmTaskPending : TfrmTaskPending;
begin
  FTaskList.Free;
  //
  for i := 0 to FfrmWaitingList.Count-1 do
  begin
    lfrmTaskPostIt := FfrmWaitingList[i];
    lfrmTaskPostIt.Free;
  end;
  FfrmWaitingList.Clear;
  //
  for i := 0 to FfrmProgressList.Count-1 do
  begin
    lfrmTaskPending := FfrmProgressList[i];
    lfrmTaskPending.Free;
  end;
  FfrmProgressList.Clear;
  //
  for i := 0 to FfrmDoneList.Count-1 do
  begin
    lfrmTaskPostIt := FfrmDoneList[i];
    lfrmTaskPostIt.Free;
  end;
  FfrmDoneList.Clear;
  //
  FfrmSprint1.Free;
end;

procedure TfrmScrumBoard.imgAddTodoClick(Sender: TObject);
var
  lProjectNum, lTaskNum, lProjectTaskNum: integer;
  lTask: TTask;
  lProjectRecord: TProjectRecord;
  lDescLog: TStringList;
begin
  lProjectNum := GlobalVar.CurrentProjectNum;
  lProjectTaskNum := GlobalVar.CurrentProject.Data.LastTaskNum+1;
  lTaskNum := MyParams.ParamInteger[prmNextTaskNum];
  lTask := TTask.Create;
  lTask.CreateTodoTarget(lTaskNum, lProjectNum, lProjectTaskNum, CST_UnclassifiedTodo);
  //
  if frmTargetModify.Show(lTask.Data, CST_TargetTodoUnclassified) then
  begin
    FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
    InitWaiting;
    //
    MyParams.ParamInteger[prmNextTaskNum] := lTaskNum+1;
    //
    lProjectRecord := GlobalVar.CurrentProject.Data;
    lProjectRecord.LastTaskNum := lProjectTaskNum;
    MainDatamodule.UpdateProject(lProjectRecord);
    GlobalVar.CurrentProject.RefreshData;
    //
    lTask := FTaskList.Search(lTaskNum);
    lDescLog := TStringList.Create;
    lDescLog.Add('A new unclassified task has been added:');
    lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
    lDescLog.Free;
  end;
end;

procedure TfrmScrumBoard.imgAddTodoMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add an unclassified task...');
end;

procedure TfrmScrumBoard.imgAddTodoMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmScrumBoard.imgDonePrevClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
begin
  // DONE >> IN PROGRESS
  lTask := FTaskList.Search(TControl(Sender).Tag);
  if Assigned(lTask) then
  begin
    lTaskRec := lTask.Data;
    lTaskRec.Status := staInProgress;
    TaskDatamodule.UpdateTask(lTaskRec);
    //
    lDescLog := TStringList.Create;
    lDescLog.Add('A to-do task has been restarted:');
    lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
    lDescLog.Free;
    //
    FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
    InitProgress;
    InitDone;
  end;
end;

procedure TfrmScrumBoard.imgProgressNextClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
  lProjectRec: TProjectRecord;
begin
  // IN PROGRESS >> DONE
  lTask := FTaskList.Search(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lTaskRec.Status := staDone;
  lTaskRec.DoneDate := now;
  TaskDatamodule.UpdateTask(lTaskRec);
  //
  lProjectRec := GlobalVar.CurrentProject.Data;
  lProjectRec.LastSprint := GlobalVar.CurrentSprintCode;
  MainDatamodule.UpdateProject(lProjectRec);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A to-do task has ended:');
  lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
  InitProgress;
  InitDone;
end;

procedure TfrmScrumBoard.imgProgressPrevClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
begin
  // IN PROGRESS >> WAITING
  lTask := FTaskList.Search(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lTaskRec.Status := staScheduled;
  TaskDatamodule.UpdateTask(lTaskRec);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A to-do task has been paused:');
  lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
  InitWaiting;
  InitProgress;
end;

procedure TfrmScrumBoard.imgWaitingDeleteClick(Sender: TObject);
begin
  // delete add. waiting task
  TaskDatamodule.DeleteTask(TControl(Sender).Tag);
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
  InitWaiting;
end;

procedure TfrmScrumBoard.imgWaitingNextClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
begin
  // WAITING >> IN PROGRESS
  lTask := FTaskList.Search(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lTaskRec.Status := staInProgress;
  lTaskRec.InProgressDate := now;
  TaskDatamodule.UpdateTask(lTaskRec);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A to-do task has been started:');
  lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
  InitWaiting;
  InitProgress;
end;

procedure TfrmScrumBoard.Initialize;
begin
  FfrmSprint1.Initialize(date);
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
  //
  InitWaiting;
  InitProgress;
  InitDone;
end;

procedure TfrmScrumBoard.InitWaiting;
var
  lfrmTaskPostIt: TfrmTaskPostIt;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lSprintCode: string;
begin
  // initialize waiting
  for i := 0 to CST_Max_TodosToShow - 1 do
  begin
    lfrmTaskPostIt := FfrmWaitingList[i];
    lfrmTaskPostIt.Initialize(CST_TargetScheduledWaiting);
  end;
  //
  lIndexForm := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_TodosToShow then
      break;
    //
    lTask := FTaskList.Items(i);
    lSprintCode := GetSprintCode(date);
    if (lTask.Data.SprintCode = lSprintCode) and (lTask.Data.Status = staScheduled) then
    begin
      lfrmTaskPostIt := FfrmWaitingList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmTaskPostIt.Initialize(lTask, lDelay);
      inc(lIndexForm);
    end;
  end;
end;

procedure TfrmScrumBoard.InitProgress;
var
  lfrmTaskPending: TfrmTaskPending;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lSprintCode: string;
begin
  // initialize in progress
  for i := 0 to CST_Max_PendingsToShow - 1 do
  begin
    lfrmTaskPending := FfrmProgressList[i];
    lfrmTaskPending.Initialize(CST_TargetScheduledInProgress);
  end;
  //
  lIndexForm := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_PendingsToShow then
      break;
    //
    lTask := FTaskList.Items(i);
    lSprintCode := GetSprintCode(date);
    if (lTask.Data.SprintCode = lSprintCode) and (lTask.Data.Status = staInProgress) then
    begin
      lfrmTaskPending := FfrmProgressList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmTaskPending.Initialize(lTask, lDelay);
      inc(lIndexForm);
    end;
  end;
end;

procedure TfrmScrumBoard.InitDone;
var
  lfrmTaskPostIt: TfrmTaskPostIt;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lSprintCode: string;
begin
  // initialize Done
  for i := 0 to CST_Max_TodosToShow - 1 do
  begin
    lfrmTaskPostIt := FfrmDoneList[i];
    lfrmTaskPostIt.Initialize(CST_TargetScheduledDone);
  end;
  //
  lIndexForm := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_TodosToShow then
      break;
    //
    lTask := FTaskList.Items(i);
    lSprintCode := GetSprintCode(date);
    if (lTask.Data.SprintCode = lSprintCode) and (lTask.Data.Status = staDone) then
    begin
      lfrmTaskPostIt := FfrmDoneList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmTaskPostIt.Initialize(lTask, lDelay);
      inc(lIndexForm);
    end;
  end;
end;

end.
