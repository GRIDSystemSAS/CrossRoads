unit fmTaskReport;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, unStructure, FMX.Objects,
  FMX.Layouts, FMX.Effects, FMX.Ani, FMX.Filter.Effects, unGlobal;

type
  TfrmTaskReport = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    rectHeader: TRectangle;
    layLeft: TLayout;
    txtDescription: TText;
    rectNum: TRectangle;
    txtTaskNum: TText;
    rectBody: TRectangle;
    layInfos: TLayout;
    layTaskContent: TLayout;
    rectBottom: TRectangle;
    laySupp: TLayout;
    rectPriority: TRectangle;
    txtEstimation: TText;
    rectEstim: TRectangle;
    pnlEstim: TRectangle;
    rectSupp: TRectangle;
    rectInfos: TRectangle;
    imgDone: TImage;
    lblBeginDate: TText;
    edtBeginDate: TText;
    lblEndDate: TText;
    edtEndDate: TText;
    txtDoneDate: TText;
    layBodyMain: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTask: TTask;
    FTaskRecord: TTaskRecord;
    FType: integer;
    FTaskList, FCommentList, FLinkList: TTaskList;
    FfrmTaskList, FfrmCommentList: TList;
    FSumHeightComments: extended;
    procedure LoadComments;
    procedure LoadLinks;

  public
    procedure Initialize(aType: integer); overload;
    procedure Initialize(aTask: TTask; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    procedure LoadTasks;
  end;

var
  frmTaskReport: TfrmTaskReport;

implementation

uses fmTargetModify, dmTypeList, fmWelcomeMain, fmTaskElement, unFunctions,
fmCommentElement;

const
  CST_Height_Body = 110;
  CST_Height_CheckTask = 58;

{$R *.fmx}

procedure TfrmTaskReport.Initialize(aType: integer);
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDescription.Text := '';
  txtTaskNum.Text := '';
  FType := aType;
end;

procedure TfrmTaskReport.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTask: TfrmTaskElement;
  lfrmComment: TfrmCommentElement;
begin
  FTaskList := TTaskList.Create;
  // tasks
  FfrmTaskList := TList.Create;
  for i := 0 to CST_Max_TasksToShow-1 do
  begin
    lfrmTask := TfrmTaskElement.Create(nil);
    lfrmTask.Parent := nil;
    lfrmTask.layBody.Parent := layTaskContent;
    lfrmTask.Initialize;
    lfrmTask.layBody.Position.X := 0;
    lfrmTask.layBody.Position.Y := (i*CST_TasksHeight)+4;
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
    lfrmComment.layBody.Parent := rectBottom;
    lfrmComment.Initialize;
    lfrmComment.layBody.Position.X := 10;
    lfrmComment.layBody.Position.Y := (i*CST_CommentHeight)+4;
    FfrmCommentList.Add(lfrmComment);
  end;
  //
  for i := 0 to FfrmCommentList.Count-1 do
  begin
    lfrmComment := FfrmCommentList[i];
    //lfrmComment.layBody.Width := 400;
    lfrmComment.layBody.Align := TAlignLayout.alTop;
  end;
  //
  FLinkList := TTaskList.Create;
end;

procedure TfrmTaskReport.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTask: TfrmTaskElement;
  lfrmComment: TfrmCommentElement;
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
end;

procedure TfrmTaskReport.Initialize(aTask: TTask; aDelay: single);
var
  lProject: TProject;
  lHeightcheckedTasks: extended;
begin
  FTask := aTask;
  FTaskRecord := FTask.Data;
  layVisible.Visible := True;
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
  //
  rectBottom.Height := FSumHeightComments+10;
  if rectBottom.Height<20 then
    rectBottom.Height := 20;
  //
  lHeightcheckedTasks := CST_TasksHeight*FTaskList.Count;
  if lHeightcheckedTasks < CST_Height_CheckTask then
    lHeightcheckedTasks := CST_Height_CheckTask;
  layBody.Height := lHeightcheckedTasks
                    +rectBottom.Height
                    +45;
  if layBody.Height < CST_Height_Body then
    layBody.Height := CST_Height_Body;
end;

procedure TfrmTaskReport.LoadTasks;
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
    lfrmTask.IsReadOnly := True;
    lfrmTask.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
  end;
end;

procedure TfrmTaskReport.LoadComments;
var
  i: integer;
  lfrmComment: TfrmCommentElement;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
begin
  FSumHeightComments := 0;
  FCommentList.Load(FTask.Data.ProjectNum, CST_TargetComment, FTask.Data.TaskNum);
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
    lfrmComment.IsReadOnly := True;
    lfrmComment.Initialize(lTask.Data, lDelay);
    inc(lIndexForm);
    //
    FSumHeightComments := FSumHeightComments+lfrmComment.layBody.Height;
  end;
  //
end;

procedure TfrmTaskReport.LoadLinks;
begin
  FLinkList.Load(FTaskRecord.ProjectNum, CST_TargetLink, FTaskRecord.TaskNum);
  //

end;

procedure TfrmTaskReport.RefreshData(aDelay: single);
var
  lParentLabel: string;
  lTarget: TTask;
begin
  txtTaskNum.Text := IntToStr(FTaskRecord.ProjectTaskNum);
  //
  lParentLabel := '?';
  if FTask.Data.ParentTaskNum=CST_UnclassifiedTodo then
  begin
    lParentLabel := CST_UnclassifiedTodoLabel;
  end
  else
  begin
    lTarget := GlobalVar.CurrentProject.TargetSearch(FTask.Data.ParentTaskNum);
    if lTarget<>nil then
      lParentLabel := lTarget.Data.TaskName;
  end;
  txtDescription.Text := Format('[%s] %s',[lParentLabel, FTaskRecord.TaskName]);
  //
  rectPriority.Fill.Color := PriorityToColor(FTaskRecord.Priority);
  txtEstimation.Text := Format('%d',[FTaskRecord.Estimation]);
  pnlEstim.Width := (FTaskRecord.Estimation/GlobalVar.WeekCapacity)*rectEstim.width;
  //
  imgDone.Visible := (FTaskRecord.Status = staDone);
  if FTask.Data.InProgressDate=0 then
    edtBeginDate.Text := '-'
  else
    edtBeginDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.InProgressDate);
  if FTask.Data.DoneDate=0 then
    edtEndDate.Text := '-'
  else
    edtEndDate.Text := FormatDatetime(CST_FormatDatetime, FTask.Data.DoneDate);
  txtDoneDate.Text := FormatDatetime(CST_DateMonth, FTaskRecord.DoneDate);
  txtDoneDate.Visible := (FTaskRecord.Status = staDone);
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;



end.
