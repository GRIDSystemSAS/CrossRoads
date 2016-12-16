unit fmTargetTodo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unGlobal, FMX.Filter.Effects, FMX.Effects;

type
  TfrmTargetTodo = class(TForm)
    layBody: TLayout;
    layTodoLeft: TLayout;
    rectTodo: TRectangle;
    imgTodo: TImage;
    txtTaskName: TText;
    layTop: TLayout;
    rectTaskNum: TRectangle;
    txtTaskNum: TText;
    titDetails: TImage;
    layLeftBody: TLayout;
    txtDescription: TText;
    layTasks: TLayout;
    imgTasks: TImage;
    layTasksBody: TLayout;
    layLinks: TLayout;
    imgLinks: TImage;
    layNotes: TLayout;
    imgNotes: TImage;
    imgExit: TImage;
    MonochromeEffect1: TMonochromeEffect;
    rectDescription: TRectangle;
    lblPriority: TText;
    rectPriority: TRectangle;
    txtPriority: TText;
    lblEstim: TText;
    laySupp: TLayout;
    txtEstimation: TText;
    rectEstim: TRectangle;
    pnlEstim: TRectangle;
    lblWeek: TText;
    edtWeek: TText;
    lblBeginDate: TText;
    edtBeginDate: TText;
    lblEndDate: TText;
    edtEndDate: TText;
    layDetBottom: TLayout;
    lblCreationDate: TText;
    edtCreationDate: TText;
    lblModifDate: TText;
    edtModifDate: TText;
    lblStatus: TText;
    edtStatus: TText;
    layDetBody: TLayout;
    layDetTop: TLayout;
    imgTarget: TImage;
    txtTargetDesc: TText;
    layCommentBody: TScrollBox;
    layLinksBody: TLayout;
    lblScheduledDate: TText;
    txtScheduledDate: TText;
    txtDueDate: TText;
    edtDueDate: TText;
    procedure imgExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTask: TTask;
    FTaskList, FCommentList, FLinkList: TTaskList;
    FfrmTaskList, FfrmCommentList, FfrmLinkList: TList;
    procedure LoadTasks;


  public
    procedure Initialize(aTask: TTask);
    procedure LoadComments;
    procedure Loadlinks;
  end;

var
  frmTargetTodo: TfrmTargetTodo;

implementation

uses fmWelcomeMain, unFunctions, unStructure, fmTaskElement, fmCommentElement,
fmLinkElement;

{$R *.fmx}

procedure TfrmTargetTodo.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTask: TfrmTaskElement;
  lfrmComment: TfrmCommentElement;
  lfrmLink: TfrmLinkElement;
begin
  layLinks.Visible := CST_ActivateLink;
  //
  FTaskList := TTaskList.Create;
  // tasks
  FfrmTaskList := TList.Create;
  for i := 0 to CST_Max_TasksToShow-1 do
  begin
    lfrmTask := TfrmTaskElement.Create(nil);
    lfrmTask.Parent := nil;
    lfrmTask.layBody.Parent := layTasksBody;
    lfrmTask.Initialize;
    lfrmTask.layBody.Position.X := 0;
    lfrmTask.layBody.Position.Y := (i*CST_TasksHeight)+4;
    lfrmTask.layBody.Width := 345;
    FfrmTaskList.Add(lfrmTask);
  end;
  //
  FCommentList := TTaskList.Create;
  // comments
  FfrmCommentList := TList.Create;
  for i := 0 to CST_Max_CommentsToShow-1 do
  begin
    lfrmComment := TfrmCommentElement.Create(nil);
    lfrmComment.Parent := nil;
    lfrmComment.layBody.Parent := layCommentBody;
    lfrmComment.Initialize;
    lfrmComment.layBody.Position.X := 10;
    lfrmComment.layBody.Position.Y := (i*CST_CommentHeight)+4;
    lfrmComment.Parent := self;
    FfrmCommentList.Add(lfrmComment);
  end;
  //
  for i := 0 to FfrmCommentList.Count-1 do
  begin
    lfrmComment := FfrmCommentList[i];
    //lfrmComment.layBody.Width := 400;
    lfrmComment.layBody.Align := TAlignLayout.alTop;
  end;
  // links
  FLinkList := TTaskList.Create;
  FfrmLinkList := TList.Create;
  for i := 0 to CST_Max_LinksToShow-1 do
  begin
    lfrmLink := TfrmLinkElement.Create(nil);
    lfrmLink.Parent := self;
    lfrmLink.layBody.Parent := layLinksBody;
    lfrmLink.Initialize;
    lfrmLink.layBody.Position.X := 0;
    lfrmLink.layBody.Position.Y := (i*CST_LinksHeight)+4;
    lfrmLink.layBody.Width := 345;
    FfrmLinkList.Add(lfrmLink);
  end;
end;

procedure TfrmTargetTodo.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTask: TfrmTaskElement;
  lfrmComment: TfrmCommentElement;
  lfrmLink: TfrmLinkElement;
begin
  FTaskList.Free;
  FCommentList.Free;
  FLinkList.Free;
  //
  for i := 0 to FfrmTaskList.Count-1 do
  begin
    lfrmTask := FfrmTaskList[i];
    lfrmTask.Free;
  end;
  FfrmTaskList.Clear;
  FfrmTaskList.Free;
  //
  for i := 0 to FfrmCommentList.Count-1 do
  begin
    lfrmComment := FfrmCommentList[i];
    lfrmComment.Free;
  end;
  FfrmCommentList.Clear;
  FfrmCommentList.Free;
  //
  for i := 0 to FfrmLinkList.Count-1 do
  begin
    lfrmLink := FfrmLinkList[i];
    lfrmLink.Free;
  end;
  FfrmLinkList.Clear;
  FfrmLinkList.Free;
end;

procedure TfrmTargetTodo.imgExitClick(Sender: TObject);
begin
  frmWelcomeMain.ShowPrecMenu;
end;

procedure TfrmTargetTodo.Initialize(aTask: TTask);
var
  lSprintRec: TSprintRecord;
  lTarget: TTask;
begin
  FTask := aTask;
  //
  txtTaskName.Text := FTask.Data.TaskName;
  txtTaskNum.Text := IntToStr(FTask.Data.ProjectTaskNum);
  txtDescription.Text := FTask.Data.Description;
  //
  txtPriority.Text :=  TypeDatamodule.GetPriorityLabel(FTask.Data.Priority);
  rectPriority.Fill.Color := PriorityToColor(FTask.Data.Priority);
  //
  txtEstimation.Text := Format('%d',[FTask.Data.Estimation]);
  pnlEstim.Width := (FTask.Data.Estimation/GlobalVar.WeekCapacity)*rectEstim.width;
  //
  if FTask.Data.SprintCode<>'' then
  begin
    TypeDatamodule.GetSprint(FTask.Data.SprintCode, lSprintRec);
    edtWeek.Text := Format('%s (%s-%s)',
                            [FTask.Data.SprintCode,
                             FormatDatetime(CST_DateMonth,lSprintRec.BeginDate),
                             FormatDatetime(CST_DateMonth,lSprintRec.EndDate)])
  end
  else
    edtWeek.Text := '';
  //
  if FTask.Data.InProgressDate=0 then
    edtBeginDate.Text := '-'
  else
    edtBeginDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.InProgressDate);
  if FTask.Data.DoneDate=0 then
    edtEndDate.Text := '-'
  else
    edtEndDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.DoneDate);
  if FTask.Data.ScheduledDate=0 then
    txtScheduledDate.Text := '-'
  else
    txtScheduledDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.ScheduledDate);
  if FTask.Data.DueDate=0 then
    edtDueDate.Text := '-'
  else
    edtDueDate.Text := FormatDatetime(CST_FormatDate, FTask.Data.DueDate);
  //
  edtCreationDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.CreationDate);
  edtModifDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.ModificationDate);
  edtStatus.Text := TypeDatamodule.GetStatusLabel(FTask.Data.Status);
  //
  txtTargetDesc.Text := '?';
  if FTask.Data.ParentTaskNum=CST_UnclassifiedTodo then
  begin
    txtTargetDesc.Text := CST_UnclassifiedTodoLabel;
  end
  else
  begin
    lTarget := GlobalVar.CurrentProject.TargetSearch(FTask.Data.ParentTaskNum);
    if lTarget<>nil then
      txtTargetDesc.Text := lTarget.Data.TaskName;
  end;
  //
  LoadTasks;
  LoadComments;
  Loadlinks;
end;

procedure TfrmTargetTodo.LoadTasks;
var
  i: integer;
  lfrmTask: TfrmTaskElement;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  FTaskList.Load(FTask.Data.ProjectNum, CST_TargetTask, FTask.Data.TaskNum);
  //
  // initialize tasks
  for i := 0 to CST_Max_TasksToShow - 1 do
  begin
    lfrmTask := FfrmTaskList[i];
    lfrmTask.Initialize;
  end;
  //
  lIndexForm := 0;
  for i := 0 to FTaskList.Count-1 do
  begin
    if lIndexForm >= CST_Max_TasksToShow then
      break;
    //
    lTask := FTaskList.Items(i);
    lfrmTask := FfrmTaskList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmTask.IsReadOnly := True;
    lfrmTask.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
  end;
  //
  layTasksBody.Height := CST_TasksHeight*FTaskList.Count+20;
  if layTasksBody.Height < 300 then
    layTasksBody.Height := 300;
end;

procedure TfrmTargetTodo.LoadComments;
var
  i: integer;
  lfrmComment: TfrmCommentElement;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  FCommentList.Load(FTask.Data.ProjectNum, CST_TargetComment, FTask.Data.TaskNum);
  //
  // initialize Comments
  for i := 0 to CST_Max_CommentsToShow - 1 do
  begin
    lfrmComment := FfrmCommentList[i];
    lfrmComment.Initialize;
  end;
  //
  lIndexForm := 0;
  for i := 0 to FCommentList.Count-1 do
  begin
    if lIndexForm >= CST_Max_CommentsToShow then
      break;
    //
    lTask := FCommentList.Items(i);
    lfrmComment := FfrmCommentList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmComment.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
  end;
  //
end;

procedure TfrmTargetTodo.Loadlinks;
var
  i: integer;
  lfrmLink: TfrmLinkElement;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  FLinkList.Load(FTask.Data.ProjectNum, CST_TargetLink, FTask.Data.TaskNum);
  //
  // initialize Links
  for i := 0 to CST_Max_LinksToShow - 1 do
  begin
    lfrmLink := FfrmLinkList[i];
    lfrmLink.Initialize;
  end;
  //
  lIndexForm := 0;
  for i := 0 to FLinkList.Count-1 do
  begin
    if lIndexForm >= CST_Max_LinksToShow then
      break;
    //
    lTask := FLinkList.Items(i);
    lfrmLink := FfrmLinkList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmLink.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
  end;
  //
end;

end.
