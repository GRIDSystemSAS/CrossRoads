unit fmSprint;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure, unGlobal;

type
  TfrmSprint = class(TForm)
    layBody: TLayout;
    rectCalBody: TRectangle;
    rectHeader: TRectangle;
    layDate: TLayout;
    txtYear: TText;
    txtWeekNum: TText;
    LayHeaderBody: TLayout;
    txtPeriod: TText;
    layContent: TLayout;
    imgFond: TImage;
    layHeaderBottom: TLayout;
    imgNext: TImage;
    imgPrevious: TImage;
    layestim: TLayout;
    txtEstim: TText;
    layDisabled: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgNextClick(Sender: TObject);
    procedure imgPreviousClick(Sender: TObject);
  private
    FSprintRec: TSprintRecord;
    FfrmTargetList: TList;
    FSprintOrder: integer;
    FTaskList: TTaskList;
  public
    procedure Initialize(aDate: TDatetime; aSprintOrder: Integer);
    procedure InitTasks;
  end;

var
  frmSprint: TfrmSprint;

implementation

uses DateUtils, StrUtils, fmTaskPostIt, unFunctions, fmTasks, dmTypeList;

{$R *.fmx}

procedure TfrmSprint.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTaskPostIt : TfrmTaskPostIt;
begin
  FTaskList := TTaskList.Create;
  //
  FfrmTargetList := TList.Create;
  for i := 0 to CST_Max_TodosToShow-1 do
  begin
    lfrmTaskPostIt := TfrmTaskPostIt.Create(nil);
    lfrmTaskPostIt.layBody.Parent := layContent;
    lfrmTaskPostIt.OnNextImageClick := imgNextClick;
    lfrmTaskPostIt.OnPreviousImageClick := imgPreviousClick;
    lfrmTaskPostIt.Initialize(CST_TargetScheduled);
    FfrmTargetList.Add(lfrmTaskPostIt);
  end;
  //
  for i := CST_Max_TodosToShow-1 downto 0 do
  begin
    lfrmTaskPostIt := FfrmTargetList[i];
    lfrmTaskPostIt.layBody.Align := TAlignLayout.alTop;
  end;
  //
  layDisabled.BringToFront;
end;

procedure TfrmSprint.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTaskPostIt : TfrmTaskPostIt;
begin
  FTaskList.Free;
  //
  for i := 0 to FfrmTargetList.Count-1 do
  begin
    lfrmTaskPostIt := FfrmTargetList[i];
    lfrmTaskPostIt.Free;
  end;
  FfrmTargetList.Clear;
end;

procedure TfrmSprint.imgNextClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lNextSprintCode: string;
  lDescLog: TStringList;
begin
  // GOTO Next sprint
  if FSprintOrder>3 then exit;
  lTask := FTaskList.Search(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lNextSprintCode := GetSprintCode(FSprintRec.BeginDate+7);
  lTaskRec.SprintCode := lNextSprintCode;
  TaskDatamodule.UpdateTask(lTaskRec);
  //
  frmTasks.InitSprints;
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A to-do task has been postponed:');
  lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
  lDescLog.Add(Format('- postponed from %s to %s',[FSprintRec.SprintCode,lNextSprintCode]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
end;

procedure TfrmSprint.imgPreviousClick(Sender: TObject);
var
  lTask: TTask;
  lTaskRec: TTaskRecord;
  lNextSprintCode: string;
  lDescLog: TStringList;
begin
  // GOTO previous sprint
  lTask := FTaskList.Search(TControl(Sender).Tag);
  lTaskRec := lTask.Data;
  lDescLog := TStringList.Create;
  if FSprintOrder=1 then
  begin
    lTaskRec.Status := staValidated;
    lTaskRec.SprintCode := '';
    //
    lDescLog.Add('A schedule has been canceled:');
    lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  end
  else
  begin
    lNextSprintCode := GetSprintCode(FSprintRec.BeginDate-7);
    lTaskRec.SprintCode := lNextSprintCode;
    //
    lDescLog.Add('A to-do task has been postponed:');
    lDescLog.Add(Format('- %s (%d)',[lTask.Data.TaskName, lTask.Data.ProjectTaskNum]));
    lDescLog.Add(Format('- postponed from %s to %s',[FSprintRec.SprintCode,lNextSprintCode]));
    UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  end;
  lDescLog.Free;
  TaskDatamodule.UpdateTask(lTaskRec);
  //
  frmTasks.InitSprints;
  if FSprintOrder=1 then
    frmTasks.InitTodo;
end;

procedure TfrmSprint.Initialize(aDate: TDatetime; aSprintOrder: integer);
var
  lID: string;
const
  CST_DateMonth = 'DD.MM';
begin
  FSprintOrder := aSprintOrder;
  lID := GetSprintCode(aDate);
  TypeDatamodule.GetSprint(lID, FSprintRec);
  txtYear.Text := IntToSTr(FSprintRec.Year);
  txtWeekNum.Text := IntToSTr(FSprintRec.WeekNum);
  txtPeriod.Text := Format('from %s to %s',[FormatDatetime(CST_DateMonth,FSprintRec.BeginDate),
                                            FormatDatetime(CST_DateMonth,FSprintRec.EndDate)]);
  //
  layDisabled.Visible := (FSprintOrder=CST_SprintOld);
  if FSprintOrder=CST_SprintOld then
  begin
    rectHeader.Opacity := 0.7;
    imgFond.Opacity := 0.05;
  end
  else
  begin
    rectHeader.Opacity := 1;
    imgFond.Opacity := 0.2;
  end;
  InitTasks;
end;

procedure TfrmSprint.InitTasks;
var
  lfrmTaskPostIt: TfrmTaskPostIt;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lTotEstim, lTotDone: integer;
begin
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, FSprintRec.SprintCode);
  if (FSprintOrder=CST_SprintResume) then
    FTaskList.SortByDoneDate;
  // initialize to-do
  for i := 0 to CST_Max_TodosToShow - 1 do
  begin
    lfrmTaskPostIt := FfrmTargetList[i];
    if FSprintOrder=CST_SprintResume then
      lfrmTaskPostIt.Initialize(CST_TargetResume)
    else
      lfrmTaskPostIt.Initialize(CST_TargetScheduled);
  end;
  //
  lIndexForm := 0;
  lTotEstim := 0;
  lTotDone := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_TodosToShow then
      break;
    //
    lTask := FTaskList.Items(i);
    lfrmTaskPostIt := FfrmTargetList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmTaskPostIt.Initialize(lTask, lDelay);
    inc(lIndexForm);
    //
    lTotEstim := lTotEstim + lTask.Data.Estimation;
    if lTask.Data.Status=staDone then
      lTotDone := lTotDone + lTask.Data.Estimation;
  end;
  txtEstim.Text := Format('%d task(s): %d/%d',[FTaskList.Count,lTotDone,lTotEstim]);
end;

end.
