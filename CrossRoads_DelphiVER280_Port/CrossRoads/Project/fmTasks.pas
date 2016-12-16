unit fmTasks;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Filter.Effects, FMX.ListBox, fmSprint, unGlobal, fmTaskPostIt, fmTarget,
  FMX.ExtCtrls, FMX.Effects;

type
  TfrmTasks = class(TForm)
    layBody: TLayout;
    layBodyIdeas: TLayout;
    rectIdeas: TRectangle;
    imgIdeas: TImage;
    layTodo: TLayout;
    rectTodo: TRectangle;
    imgTodo: TImage;
    imgAddTarget: TImage;
    MonochromeEffect1: TMonochromeEffect;
    imgAddTodo: TImage;
    MonochromeEffect2: TMonochromeEffect;
    layIdeasContent: TLayout;
    layTodoContent: TLayout;
    laySchedule: TLayout;
    rectSchedule: TRectangle;
    imgSchedule: TImage;
    layScheduleTop: TLayout;
    layScheduleBody: TLayout;
    layScheduleBand1: TLayout;
    layCalendar1: TLayout;
    layCalendar2: TLayout;
    layCalendar3: TLayout;
    layCalendar4: TLayout;
    imgTargetDeleteClick: TImage;
    imgTodoClick: TImage;
    imgToDoDelete: TImage;
    layDateSelect: TLayout;
    imgPrev: TImage;
    MonochromeEffect3: TMonochromeEffect;
    imgNext: TImage;
    MonochromeEffect4: TMonochromeEffect;
    txtEstimToDo: TText;
    imgTargetArchiveClick: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgAddTargetClick(Sender: TObject);
    procedure imgAddTodoClick(Sender: TObject);
    procedure imgTodoClickClick(Sender: TObject);
    procedure imgToDoDeleteClick(Sender: TObject);
    procedure imgTargetDeleteClickClick(Sender: TObject);
    procedure imgPrevClick(Sender: TObject);
    procedure imgNextClick(Sender: TObject);
    procedure imgAddTargetMouseEnter(Sender: TObject);
    procedure imgAddTargetMouseLeave(Sender: TObject);
    procedure imgAddTodoMouseEnter(Sender: TObject);
    procedure imgAddTodoMouseLeave(Sender: TObject);
    procedure imgTargetArchiveClickClick(Sender: TObject);
  private
    FfrmSprint1, FfrmSprint2, FfrmSprint3, FfrmSprint4: TfrmSprint;
    FfrmTargetList, FfrmTargetTodoList: TList;
    FSelectedTargetNum: integer;
    FCurrentDate: TDatetime;
    procedure InitTargets;
    procedure RefreshCurrentTarget;
    procedure SetSelectedTargetNum(aValue: integer);

  public
    procedure Initialize;
    procedure InitSprints;
    procedure InitTodo;
    procedure UpdateToDoInfos;
    procedure SelectScreenTarget(aTaskNum: integer);
    //
    property SelectedTargetNum: integer read FSelectedTargetNum write SetSelectedTargetNum;
  end;

var
  frmTasks: TfrmTasks;

implementation

uses DateUtils, fmTargetModify, unStructure, dmTypeList, strUtils, unFunctions,
fmWelcomeMain;

{$R *.fmx}


procedure TfrmTasks.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTaskPostIt: TfrmTaskPostIt;
  lfrmTarget: TfrmTarget;
begin
  FfrmSprint1 := TfrmSprint.Create(self);
  FfrmSprint1.layBody.Parent := layCalendar1;
  //
  FfrmSprint2 := TfrmSprint.Create(self);
  FfrmSprint2.layBody.Parent := layCalendar2;
  //
  FfrmSprint3 := TfrmSprint.Create(self);
  FfrmSprint3.layBody.Parent := layCalendar3;
  //
  FfrmSprint4 := TfrmSprint.Create(self);
  FfrmSprint4.layBody.Parent := layCalendar4;
  // targets
  FfrmTargetList := TList.Create;
  for i := 0 to CST_Max_TargetsToShow-1 do
  begin
    lfrmTarget := TfrmTarget.Create(nil);
    lfrmTarget.layBody.Parent := layIdeasContent;
    lfrmTarget.Initialize(CST_Target);
    lfrmTarget.OnDeleteClick := imgTargetDeleteClickClick;
    lfrmTarget.OnArchiveClick := imgTargetArchiveClickClick;
    FfrmTargetList.Add(lfrmTarget);
  end;
  FSelectedTargetNum := -1;
  //
  for i := CST_Max_TargetsToShow-1 downto 0 do
  begin
    lfrmTarget := FfrmTargetList[i];
    lfrmTarget.layBody.Align := TAlignLayout.alTop;
  end;
  // to-do
  FfrmTargetTodoList := TList.Create;
  for i := 0 to CST_Max_TodosToShow-1 do
  begin
    lfrmTaskPostIt := TfrmTaskPostIt.Create(nil);
    lfrmTaskPostIt.layBody.Parent := layTodoContent;
    lfrmTaskPostIt.Initialize(CST_TargetTodo);
    lfrmTaskPostIt.OnNextImageClick := imgTodoClickClick;
    lfrmTaskPostIt.OnDeleteClick := imgToDoDeleteClick;
    FfrmTargetTodoList.Add(lfrmTaskPostIt);
  end;
  //
  for i := CST_Max_TodosToShow-1 downto 0 do
  begin
    lfrmTaskPostIt := FfrmTargetTodoList[i];
    lfrmTaskPostIt.layBody.Align := TAlignLayout.alTop;
  end;
  //
  FCurrentDate := date;
end;

procedure TfrmTasks.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTaskPostIt: TfrmTaskPostIt;
  lfrmTarget: TfrmTarget;
begin
  FfrmSprint1.Free;
  FfrmSprint2.Free;
  FfrmSprint3.Free;
  FfrmSprint4.Free;
  //
  for i := 0 to FfrmTargetList.Count-1 do
  begin
    lfrmTarget := FfrmTargetList[i];
    lfrmTarget.Free;
  end;
  FfrmTargetList.Clear;
  FfrmTargetList.Free;
  //
  for i := 0 to FfrmTargetTodoList.Count-1 do
  begin
    lfrmTaskPostIt := FfrmTargetTodoList[i];
    lfrmTaskPostIt.Free;
  end;
  FfrmTargetTodoList.Clear;
  FfrmTargetTodoList.Free;
end;

procedure TfrmTasks.imgAddTargetClick(Sender: TObject);
var
  lProjectNum, lTaskNum: integer;
  lTask: TTask;
  lDescLog: TStringList;
begin
  lProjectNum := GlobalVar.CurrentProjectNum;
  lTaskNum := MyParams.ParamInteger[prmNextTaskNum];
  lTask := TTask.Create;
  lTask.CreateTarget(lTaskNum, lProjectNum);
  //
  if frmTargetModify.Show(lTask.Data, CST_Target) then
  begin
    GlobalVar.CurrentProject.TargetAdd(lTaskNum);
    SelectedTargetNum := lTaskNum;
    InitTargets;
    //
    MyParams.ParamInteger[prmNextTaskNum] := lTaskNum+1;
    //
    lTask := GlobalVar.CurrentProject.TargetSearch(lTaskNum);
    lDescLog := TStringList.Create;
    lDescLog.Add('A new target has been added:');
    lDescLog.Add(Format('- %s',[lTask.Data.TaskName]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
    lDescLog.Free;
  end;
end;

procedure TfrmTasks.imgAddTargetMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add a target...');
end;

procedure TfrmTasks.imgAddTargetMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTasks.imgAddTodoClick(Sender: TObject);
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
  lTask.CreateTodoTarget(lTaskNum, lProjectNum, lProjectTaskNum, FSelectedTargetNum);
  //
  if frmTargetModify.Show(lTask.Data, CST_TargetTodo) then
  begin
    InitTodo;
    RefreshCurrentTarget;
    //
    MyParams.ParamInteger[prmNextTaskNum] := lTaskNum+1;
    //
    lProjectRecord := GlobalVar.CurrentProject.Data;
    lProjectRecord.LastTaskNum := lProjectTaskNum;
    MainDatamodule.UpdateProject(lProjectRecord);
    GlobalVar.CurrentProject.RefreshData;
    //
    lTask := GlobalVar.CurrentProject.TargetToDoSearch(lTaskNum);
    lDescLog := TStringList.Create;
    lDescLog.Add('A new to-do task has been added:');
    lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
    lDescLog.Free;
  end;
end;


procedure TfrmTasks.imgAddTodoMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add a to-do task...');
end;

procedure TfrmTasks.imgAddTodoMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTasks.imgNextClick(Sender: TObject);
begin
  FCurrentDate := IncWeek(FCurrentDate,1);
  InitSprints;
end;

procedure TfrmTasks.imgPrevClick(Sender: TObject);
begin
  FCurrentDate := IncWeek(FCurrentDate,-1);
  InitSprints;
end;

procedure TfrmTasks.imgTargetArchiveClickClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
begin
  if MessageDlg('Archive target ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  // Archive Target
  lTask := GlobalVar.CurrentProject.TargetSearch(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lTaskRec.Status := staArchived;
  TaskDatamodule.UpdateTask(lTaskRec);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A target has been archived:');
  lDescLog.Add(Format('- %s',[lTask.Data.TaskName]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  GlobalVar.CurrentProject.LoadTarget;
  //
  InitTargets;
end;

procedure TfrmTasks.imgTargetDeleteClickClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
begin
  if MessageDlg('Delete target ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  // Delete Target
  lTask := GlobalVar.CurrentProject.TargetSearch(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  TaskDatamodule.DeleteTask(lTaskRec.TaskNum);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A target has been deleted:');
  lDescLog.Add(Format('- %s',[lTask.Data.TaskName]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  GlobalVar.CurrentProject.LoadTarget;
  //
  InitTargets;
end;

procedure TfrmTasks.imgTodoClickClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
  lProjectRec: TProjectRecord;
begin
  // TO-DO >> SCHEDULED
  lTask := GlobalVar.CurrentProject.TargetToDoSearch(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lTaskRec.Status := staScheduled;
  lTaskRec.SprintCode := GlobalVar.CurrentSprintCode;
  lTaskRec.ScheduledDate := now;
  TaskDatamodule.UpdateTask(lTaskRec);
  GlobalVar.CurrentProject.LoadTargetToDo(FSelectedTargetNum);
  //
  lProjectRec := GlobalVar.CurrentProject.Data;
  if lProjectRec.FirstSprint='' then
    lProjectRec.FirstSprint := GlobalVar.CurrentSprintCode;
  lProjectRec.LastSprint := GlobalVar.CurrentSprintCode;
  MainDatamodule.UpdateProject(lProjectRec);
  //
  InitTodo;
  FfrmSprint2.InitTasks;
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A to-do task has been scheduled:');
  lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
  lDescLog.Add(Format('- scheduled on %s',[GlobalVar.CurrentSprintCode]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
end;


procedure TfrmTasks.imgToDoDeleteClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lDescLog: TStringList;
begin
  if MessageDlg('Delete to-do task ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  // Delete TO-DO
  lTask := GlobalVar.CurrentProject.TargetToDoSearch(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  TaskDatamodule.DeleteTask(lTaskRec.TaskNum);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A to-do task has been deleted:');
  lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  GlobalVar.CurrentProject.LoadTargetToDo(FSelectedTargetNum);
  //
  InitTodo;
end;

procedure TfrmTasks.InitSprints;
var
  lDate: TDatetime;
begin
  lDate := IncWeek(FCurrentDate,-1);
  FfrmSprint1.Initialize(lDate, CST_SprintOld);
  lDate := IncWeek(lDate);
  FfrmSprint2.Initialize(lDate, 1);
  lDate := IncWeek(lDate);
  FfrmSprint3.Initialize(lDate, 2);
  lDate := IncWeek(lDate);
  FfrmSprint4.Initialize(lDate, 3);
end;

procedure TfrmTasks.Initialize;
begin
  FSelectedTargetNum := MyParams.ParamInteger[Format('%s_%d',[prmSelectedTargetNum,GlobalVar.CurrentProjectNum])];
  //
  InitTargets;
  InitTodo;
  //
  InitSprints;
end;

procedure TfrmTasks.InitTargets;
var
  lfrmTarget: TfrmTarget;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  // initialize targets
  for i := 0 to CST_Max_TargetsToShow - 1 do
  begin
    lfrmTarget := FfrmTargetList[i];
    lfrmTarget.Initialize(CST_Target);
  end;
  //
  lIndexForm := 0;
  for i := 0 to GlobalVar.CurrentProject.TargetCount-1 do
  begin
    if lIndexForm >= CST_Max_TargetsToShow then
      break;
    //
    lTask := GlobalVar.CurrentProject.TargetItems(i);
    lfrmTarget := FfrmTargetList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmTarget.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
  end;
  //
  SelectScreenTarget(FSelectedTargetNum);
  //
  imgAddTarget.Visible := (GlobalVar.CurrentProject.TargetCount<CST_Max_TargetsToShow);
end;

procedure TfrmTasks.RefreshCurrentTarget;
var
  i: integer;
  lfrmTarget: TfrmTarget;
begin
  for i := 0 to FfrmTargetList.Count-1 do
  begin
    lfrmTarget := FfrmTargetList[i];
    if lfrmTarget.TaskNum = FSelectedTargetNum then
      lfrmTarget.RefreshData(0);
  end;
end;

procedure TfrmTasks.InitTodo;
var
  lfrmTaskPostIt: TfrmTaskPostIt;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  GlobalVar.CurrentProject.LoadTargetToDo(FSelectedTargetNum);
  // initialize to-do
  for i := 0 to CST_Max_TodosToShow - 1 do
  begin
    lfrmTaskPostIt := FfrmTargetTodoList[i];
    lfrmTaskPostIt.Initialize(CST_TargetTodo);
  end;
  //
  lIndexForm := 0;
  for i := 0 to GlobalVar.CurrentProject.TargetTodoCount - 1 do
  begin
    if lIndexForm >= CST_Max_TodosToShow then
      break;
    //
    lTask := GlobalVar.CurrentProject.TargetTodoItems(i);
    if lTask.Data.ParentTaskNum=FSelectedTargetNum then
    begin
      lfrmTaskPostIt := FfrmTargetTodoList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmTaskPostIt.Initialize(lTask, lDelay);
      inc(lIndexForm);
    end;
  end;
  //
  UpdateToDoInfos;
  imgAddTodo.Visible := (FSelectedTargetNum > 0)
                        and (GlobalVar.CurrentProject.TargetTodoCount<CST_Max_TodosToShow);
end;

procedure TfrmTasks.UpdateToDoInfos;
var
  i: Integer;
  lTask: TTask;
  lTotEstim, lTotDone: integer;
begin
  lTotEstim := 0;
  lTotDone := 0;
  for i := 0 to GlobalVar.CurrentProject.TargetTodoCount - 1 do
  begin
    lTask := GlobalVar.CurrentProject.TargetTodoItems(i);
    if lTask.Data.ParentTaskNum=FSelectedTargetNum then
    begin
      lTotEstim := lTotEstim + lTask.Data.Estimation;
      if lTask.Data.Status=staDone then
        lTotDone := lTotDone + lTask.Data.Estimation;
    end;
  end;
  //
  txtEstimToDo.Text := Format('%d task(s): %d/%d',[GlobalVar.CurrentProject.TargetTodoCount,lTotDone,lTotEstim]);
end;

procedure TfrmTasks.SelectScreenTarget(aTaskNum: integer);
var
  lfrmTarget: TfrmTarget;
  i: Integer;
  lOneSelected: Boolean;
begin
  // select a target screen
  lOneSelected := False;
  for i := 0 to FfrmTargetList.Count - 1 do
  begin
    lfrmTarget := FfrmTargetList[i];
    if lfrmTarget.TaskNum = aTaskNum then
    begin
      lfrmTarget.SelectTarget;
      lOneSelected := True;
      SelectedTargetNum := aTaskNum;
    end
    else
      lfrmTarget.UnSelectTarget;
  end;
  //
  if not lOneSelected or (aTaskNum=0) then
  begin
    // select first element...
    lfrmTarget := FfrmTargetList[0];
    SelectedTargetNum := lfrmTarget.TaskNum;
    lfrmTarget.SelectTarget;
  end;
  //
  InitTodo;
end;

procedure TfrmTasks.SetSelectedTargetNum(aValue: integer);
begin
  FSelectedTargetNum := aValue;
  MyParams.ParamInteger[Format('%s_%d',[prmSelectedTargetNum,GlobalVar.CurrentProjectNum])] := aValue;
end;

end.
