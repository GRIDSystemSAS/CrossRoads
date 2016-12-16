unit fmTaskPending;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, unStructure, FMX.Objects,
  FMX.Layouts, FMX.Effects, FMX.Ani, FMX.Filter.Effects, unGlobal;

type
  TfrmTaskPending = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    rectHeader: TRectangle;
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
    rectBody: TRectangle;
    layMenu: TLayout;
    imgAddTask: TImage;
    MonochromeEffect3: TMonochromeEffect;
    layTaskContent: TLayout;
    rectBottom: TRectangle;
    rectComments: TRectangle;
    txtComments: TText;
    rectLinks: TRectangle;
    txtLinks: TText;
    laySupp: TLayout;
    rectPriority: TRectangle;
    txtEstimation: TText;
    rectEstim: TRectangle;
    pnlEstim: TRectangle;
    rectSupp: TRectangle;
    imgAddComments: TImage;
    MonochromeEffect4: TMonochromeEffect;
    imgAddLink: TImage;
    MonochromeEffect5: TMonochromeEffect;
    procedure txtDescriptionMouseEnter(Sender: TObject);
    procedure txtDescriptionMouseLeave(Sender: TObject);
    procedure txtDescriptionClick(Sender: TObject);
    procedure imgAddTaskClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgAddCommentsClick(Sender: TObject);
    procedure imgAddLinkClick(Sender: TObject);
    procedure imgNextMouseEnter(Sender: TObject);
    procedure imgNextMouseLeave(Sender: TObject);
    procedure imgAddTaskMouseEnter(Sender: TObject);
    procedure imgAddTaskMouseLeave(Sender: TObject);
    procedure imgAddCommentsMouseEnter(Sender: TObject);
    procedure imgAddCommentsMouseLeave(Sender: TObject);
    procedure imgPreviousMouseEnter(Sender: TObject);
    procedure imgPreviousMouseLeave(Sender: TObject);
  private
    FTask: TTask;
    FTaskRecord: TTaskRecord;
    FType: integer;
    FTaskList, FCommentList, FLinkList: TTaskList;
    FfrmTaskList: TList;
    procedure SetOnNextImageClick(aEvent: TNotifyEvent);
    procedure SetOnPreviousImageClick(aEvent: TNotifyEvent);
    procedure LoadComments;
    procedure LoadLinks;

  public
    procedure Initialize(aType: integer); overload;
    procedure Initialize(aTask: TTask; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    procedure LoadTasks;
    //
    property OnNextImageClick: TNotifyEvent write SetOnNextImageClick;
    property OnPreviousImageClick: TNotifyEvent write SetOnPreviousImageClick;
  end;

var
  frmTaskPending: TfrmTaskPending;

implementation

uses fmTargetModify, dmTypeList, fmWelcomeMain, fmTaskElement, unFunctions;

{$R *.fmx}

procedure TfrmTaskPending.Initialize(aType: integer);
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDescription.Text := '';
  txtTaskNum.Text := '';
  txtComments.Text := '';
  txtLinks.Text := '';
  FType := aType;
  //
  imgPrevious.Visible := (FType <> CST_Target)
                          and (FType <> CST_TargetScheduledWaiting);
  imgNext.Visible := (FType <> CST_TargetScheduledDone);
end;

procedure TfrmTaskPending.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTask: TfrmTaskElement;
begin
  FTaskList := TTaskList.Create;
  // tasks
  FfrmTaskList := TList.Create;
  for i := 0 to CST_Max_TasksToShow-1 do
  begin
    lfrmTask := TfrmTaskElement.Create(nil);
    lfrmTask.Parent := self;
    lfrmTask.layBody.Parent := layTaskContent;
    lfrmTask.Initialize;
    lfrmTask.layBody.Position.X := 0;
    lfrmTask.layBody.Position.Y := (i*CST_TasksHeight)+4;
    FfrmTaskList.Add(lfrmTask);
  end;
  //
  FCommentList := TTaskList.Create;
  FLinkList := TTaskList.Create;
  //
  rectLinks.Visible := CST_ActivateLink;
end;

procedure TfrmTaskPending.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTask: TfrmTaskElement;
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
end;

procedure TfrmTaskPending.imgAddCommentsClick(Sender: TObject);
var
  lProjectNum, lTaskNum, lParentTaskNum: integer;
  lTask: TTask;
begin
  lProjectNum := GlobalVar.CurrentProjectNum;
  lParentTaskNum := FTaskRecord.TaskNum;
  lTaskNum := MyParams.ParamInteger[prmNextTaskNum];
  lTask := TTask.Create;
  lTask.CreateComment(lTaskNum, lProjectNum, lParentTaskNum);
  //
  if frmTargetModify.Show(lTask.Data, CST_TargetComment) then
  begin
    LoadComments;
    //
    MyParams.ParamInteger[prmNextTaskNum] := lTaskNum+1;
  end;
end;

procedure TfrmTaskPending.imgAddCommentsMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add a comment...');
end;

procedure TfrmTaskPending.imgAddCommentsMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPending.imgAddLinkClick(Sender: TObject);
var
  lProjectNum, lTaskNum, lParentTaskNum: integer;
  lTask: TTask;
begin
  lProjectNum := GlobalVar.CurrentProjectNum;
  lParentTaskNum := FTaskRecord.TaskNum;
  lTaskNum := MyParams.ParamInteger[prmNextTaskNum];
  lTask := TTask.Create;
  lTask.CreateLink(lTaskNum, lProjectNum, lParentTaskNum);
  //
  if frmTargetModify.Show(lTask.Data, CST_TargetLink) then
  begin
    LoadLinks;
    //
    MyParams.ParamInteger[prmNextTaskNum] := lTaskNum+1;
  end;
end;

procedure TfrmTaskPending.imgAddTaskClick(Sender: TObject);
var
  lProjectNum, lTaskNum, lParentTaskNum: integer;
  lTask: TTask;
  lDescLog: TStringList;
begin
  lProjectNum := GlobalVar.CurrentProjectNum;
  lParentTaskNum := FTaskRecord.TaskNum;
  lTaskNum := MyParams.ParamInteger[prmNextTaskNum];
  lTask := TTask.Create;
  lTask.CreateTask(lTaskNum, lProjectNum, lParentTaskNum);
  //
  if frmTargetModify.Show(lTask.Data, CST_TargetTask) then
  begin
    LoadTasks;
    //
    lTask := FTaskList.Search(lTaskNum);
    lDescLog := TStringList.Create;
    lDescLog.Add('A new task has been added:');
    lDescLog.Add(Format('- %s',[lTask.Data.TaskName]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
    lDescLog.Free;
    //
    MyParams.ParamInteger[prmNextTaskNum] := lTaskNum+1;
  end;
end;

procedure TfrmTaskPending.imgAddTaskMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add a task to check...');
end;

procedure TfrmTaskPending.imgAddTaskMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPending.imgNextMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Resolve a task...');
end;

procedure TfrmTaskPending.imgNextMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPending.imgPreviousMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Put back to waiting...');
end;

procedure TfrmTaskPending.imgPreviousMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTaskPending.Initialize(aTask: TTask; aDelay: single);
var
  lProject: TProject;
begin
  FTask := aTask;
  FTaskRecord := FTask.Data;
  layVisible.Visible := True;
  imgNext.Tag := FTaskRecord.TaskNum;
  imgPrevious.Tag := FTaskRecord.TaskNum;
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
  //
  LoadTasks;
  LoadComments;
  LoadLinks;
end;

procedure TfrmTaskPending.LoadTasks;
var
  i: integer;
  lfrmTask: TfrmTaskElement;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  FTaskList.Load(FTaskRecord.ProjectNum, CST_TargetTask, FTaskRecord.TaskNum);
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
    lfrmTask.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
  end;
  //
  imgAddTask.Visible := (FTaskList.Count<CST_Max_TasksToShow);
  layBody.Height := CST_TasksHeight*FTaskList.Count+65;
  if layBody.Height < 75 then
    layBody.Height := 75;
end;

procedure TfrmTaskPending.LoadComments;
begin
  FCommentList.Load(FTaskRecord.ProjectNum, CST_TargetComment, FTaskRecord.TaskNum);
  //
  txtComments.Text := Format('%d comment(s)',[FCommentList.Count]);
end;

procedure TfrmTaskPending.LoadLinks;
begin
  FLinkList.Load(FTaskRecord.ProjectNum, CST_TargetLink, FTaskRecord.TaskNum);
  //
  txtLinks.Text := Format('%d link(s)',[FLinkList.Count]);
end;

procedure TfrmTaskPending.RefreshData(aDelay: single);
begin
  txtDescription.Text := FTaskRecord.TaskName;
  txtTaskNum.Text := IntToStr(FTaskRecord.ProjectTaskNum);
  //
  rectPriority.Fill.Color := PriorityToColor(FTaskRecord.Priority);
  txtEstimation.Text := Format('%d',[FTaskRecord.Estimation]);
  pnlEstim.Width := (FTaskRecord.Estimation/GlobalVar.WeekCapacity)*rectEstim.width;
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmTaskPending.SetOnNextImageClick(aEvent: TNotifyEvent);
begin
  imgNext.OnClick := aEvent;
end;

procedure TfrmTaskPending.SetOnPreviousImageClick(aEvent: TNotifyEvent);
begin
  imgPrevious.OnClick := aEvent;
end;



procedure TfrmTaskPending.txtDescriptionClick(Sender: TObject);
begin
  frmWelcomeMain.ShowTarget(FTask);
end;

procedure TfrmTaskPending.txtDescriptionMouseEnter(Sender: TObject);
begin
  txtDescription.Font.Style := [TFontStyle.fsBold];
end;

procedure TfrmTaskPending.txtDescriptionMouseLeave(Sender: TObject);
begin
  txtDescription.Font.Style := [];
end;

end.
