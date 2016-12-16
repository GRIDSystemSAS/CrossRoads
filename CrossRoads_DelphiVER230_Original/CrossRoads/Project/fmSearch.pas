unit fmSearch;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure, unGlobal, FMX.Filter.Effects, fmBurnUp, fmVelocity;

type
  TfrmSearch = class(TForm)
    layBody: TLayout;
    rectPrintBody: TRectangle;
    layPrintBox: TScrollBox;
    layPrintTop: TLayout;
    layPrintMain: TLayout;
    layMenu: TLayout;
    imgSave: TImage;
    MonochromeEffect1: TMonochromeEffect;
    imgPrev: TImage;
    MonochromeEffect3: TMonochromeEffect;
    imgNext: TImage;
    MonochromeEffect4: TMonochromeEffect;
    rectHeader: TRectangle;
    txtPeriod: TText;
    layDate: TLayout;
    txtYear: TText;
    txtWeekNum: TText;
    layTitle: TLayout;
    txtAppli: TText;
    layCharts: TLayout;
    layBurnup: TLayout;
    rectBurnup: TRectangle;
    txtBurnup: TText;
    layVelocity: TLayout;
    rectVelocity: TRectangle;
    txtVelocity: TText;
    rectStats: TRectangle;
    layPerf: TLayout;
    txtPerfPct: TText;
    lblPerf: TText;
    layPerfBody: TLayout;
    layPerfText: TLayout;
    txtBeginPerf: TText;
    txtEndPerf: TText;
    rectPerf: TRectangle;
    layUnSched: TLayout;
    layUnSchedBody: TLayout;
    txtUnschedPct: TText;
    layUnSchedText: TLayout;
    txtUnschedBegin: TText;
    txtUnschedEnd: TText;
    rectUnsched: TRectangle;
    txtUnschedHeader: TText;
    layUndone: TLayout;
    layUndoneBody: TLayout;
    txtUndonePct: TText;
    layUndoneBodyMain: TLayout;
    txtUndoneBegin: TText;
    txtUndoneEnd: TText;
    rectUndone: TRectangle;
    txtUndoneHeader: TText;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgSaveClick(Sender: TObject);
    procedure imgPrevClick(Sender: TObject);
    procedure imgNextClick(Sender: TObject);
  private
    FCurrentDate: TDatetime;
    FSprintCode: string;
    FInitOnce: Boolean;
    FTaskList: TTaskList;
    FfrmTaskReportList: TList;
    FSprintRec: TSprintRecord;
    FSumTaskReportHeight: extended;
    FfrmBurnUp: TfrmBurnUp;
    FfrmVelocity: TfrmVelocity;
    procedure InitTasks;
    procedure CheckStats;
    function RecalcStats(aDate: TDatetime): TStatsRecord;
  public
    procedure Initialize;

  end;

var
  frmSearch: TfrmSearch;

implementation

uses fmTaskReport, unFunctions, dmTypeList, IOUtils, DateUtils, fmWelcomeMain;


{$R *.fmx}

{ TfrmSearch }

procedure TfrmSearch.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmTaskReport : TfrmTaskReport;
begin
  FInitOnce := False;
  FTaskList := TTaskList.Create;
  //
  FfrmTaskReportList := TList.Create;
  for i := 0 to CST_Max_TodosToShow-1 do
  begin
    lfrmTaskReport := TfrmTaskReport.Create(nil);
    lfrmTaskReport.layBody.Parent := layPrintMain;
    lfrmTaskReport.Initialize(CST_TargetScheduledInProgress);
    FfrmTaskReportList.Add(lfrmTaskReport);
  end;
  //
  for i := CST_Max_TodosToShow-1 downto 0 do
  begin
    lfrmTaskReport := FfrmTaskReportList[i];
    lfrmTaskReport.layBody.Align := TAlignLayout.alTop;
  end;
  //
  FfrmBurnUp := TfrmBurnUp.Create(self);
  FfrmBurnUp.layBody.Parent := layBurnUp;
  //
  FfrmVelocity := TfrmVelocity.Create(self);
  FfrmVelocity.layBody.Parent := layVelocity;
end;

procedure TfrmSearch.FormDestroy(Sender: TObject);
var
  i: integer;
  lfrmTaskReport : TfrmTaskReport;
begin
  FTaskList.Free;
  //
  for i := 0 to FfrmTaskReportList.Count-1 do
  begin
    lfrmTaskReport := FfrmTaskReportList[i];
    lfrmTaskReport.Free;
  end;
  FfrmTaskReportList.Clear;
  //
  FfrmBurnUp.Free;
  FfrmVelocity.Free;
end;

procedure TfrmSearch.imgNextClick(Sender: TObject);
begin
  FCurrentDate := IncWeek(FCurrentDate,1);
  Initialize;
end;

procedure TfrmSearch.imgPrevClick(Sender: TObject);
begin
  FCurrentDate := IncWeek(FCurrentDate,-1);
  Initialize;
end;

procedure TfrmSearch.imgSaveClick(Sender: TObject);
var
  lBitmap: TBitmap;
begin
  if SaveDialog.Execute then
  begin
    layMenu.Visible := False;
    lBitmap := rectPrintBody.MakeScreenshot;
    lBitmap.SaveToFile(SaveDialog.FileName);
    layMenu.Visible := True;
  end;
end;

procedure TfrmSearch.Initialize;
var
  lSprintRec: TSprintRecord;
begin
  if not FInitOnce then
  begin
    FInitOnce := True;
    if GlobalVar.CurrentProject.Data.LastSprint<>'' then
    begin
      TypeDatamodule.GetSprint(GlobalVar.CurrentProject.Data.LastSprint, lSprintRec);
      FCurrentDate := lSprintRec.BeginDate;
      FSprintRec := lSprintRec;
    end
    else
      FCurrentDate := date;
  end;
  //
  FSprintCode := GetSprintCode(FCurrentDate);
  TypeDatamodule.GetSprint(FSprintCode, FSprintRec);
  //
  FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, FSprintCode);
  FTaskList.SortByDoneDate;
  //TaskDatamodule.FilterStatsOnWeek(FSprintCode);
  //
  CheckStats;
  InitTasks;
  FfrmBurnUp.Initialize(FCurrentDate);
  FfrmVelocity.Initialize(FCurrentDate);
end;

procedure TfrmSearch.CheckStats;
var
  lDate: TDatetime;
  lStatsRec: TStatsRecord;
  lInit, lEnd, lPct, lDiff: integer;
begin
  lDate := FSprintRec.BeginDate;
  while (lDate<=FSprintRec.EndDate) do
  begin
    if (lDate<Date) and not TaskDataModule.GetStats(lDate, lStatsRec) then
    begin
      lStatsRec := RecalcStats(lDate);
      TaskDataModule.UpdateStats(lStatsRec);
    end;
    //
    if lDate=Date then
      frmWelcomeMain.UpdateStats;
    //
    if (lDate>Date) then
    begin
      lStatsRec := GlobalVar.StatsRecord;
      lStatsRec.StatDate := lDate;
      TaskDataModule.UpdateStats(lStatsRec);
    end;
    lDate := lDate + 1;
  end;
  // calculate stats...
  // Performance
  TaskDataModule.GetStats(FSprintRec.BeginDate, lStatsRec);
  lInit := lStatsRec.SumScheduled;
  TaskDataModule.GetStats(FSprintRec.EndDate, lStatsRec);
  lEnd := lStatsRec.SumDone;
  lDiff := lEnd-lInit;
  if lInit=0 then
    lPct := 0
  else
    lPct := round(lDiff/lInit*100);
  if lPct<0 then
    txtPerfPct.Text := Format('%d',[lPct])+'%'
  else
    txtPerfPct.Text := Format('+%d',[lPct])+'%';
  txtBeginPerf.Text := Format('Initial scheduled : %d',[lInit]);
  if lDiff<0 then
    txtEndPerf.Text := Format('Final done : %d (%d)',[lEnd,lDiff])
  else
    txtEndPerf.Text := Format('Final done : %d (+%d)',[lEnd,lDiff]);
  // Unscheduled
  TaskDataModule.GetStats(FSprintRec.EndDate, lStatsRec);
  lInit := lStatsRec.SumScheduled;
  lEnd := lStatsRec.SumUnclassified;
  if lInit=0 then
    lPct := 0
  else
    lPct := round(lEnd/lInit*100);
  txtUnschedPct.Text := Format('%d',[lPct])+'%';
  txtUnschedBegin.Text := Format('Scheduled tasks : %d',[lInit]);
  txtUnschedEnd.Text := Format('Unscheduled tasks : %d',[lEnd]);
  // Unscheduled
  //TaskDataModule.GetStats(FSprintRec.EndDate, lStatsRec);
  lInit := lStatsRec.SumScheduled;
  lEnd := lStatsRec.SumScheduled-lStatsRec.SumDone;
  if lInit=0 then
    lPct := 0
  else
    lPct := round(lEnd/lInit*100);
  txtUndonePct.Text := Format('%d',[lPct])+'%';
  txtUndoneBegin.Text := Format('Scheduled tasks : %d',[lInit]);
  txtUndoneEnd.Text := Format('Unfinished tasks : %d',[lEnd]);
end;

function TfrmSearch.RecalcStats(aDate: TDatetime): TStatsRecord;
var
  lStatsRec: TStatsRecord;
  lNbScheduled, lNbUnclassified, lNbDone: Integer;
  lNbNew, lNbNewUnclassified, lNbInProgress: Integer;
  lSumScheduled, lSumUnclassified, lSumDone: Integer;
  lSumNew, lSumNewUnclassified, lSumInProgress: Integer;
  lTask: TTask;
  i: integer;
  lDateRef: TDatetime;
begin
  lDateRef := aDate+1;
  lNbScheduled := 0;
  lNbUnclassified := 0;
  lNbDone := 0;
  lNbNew := 0;
  lNbNewUnclassified := 0;
  lNbInProgress := 0;
  lSumScheduled := 0;
  lSumUnclassified := 0;
  lSumDone := 0;
  lSumNew := 0;
  lSumNewUnclassified := 0;
  lSumInProgress := 0;
  //
  for i := 0 to FTaskList.Count-1 do
  begin
    lTask := FTaskList.Items(i);
    if lTask.Data.ScheduledDate<lDateRef then
    begin
      inc(lNbScheduled);
      lSumScheduled := lSumScheduled + lTask.Data.Estimation;
      if (lTask.Data.ParentTaskNum=CST_UnclassifiedTodo) then
      begin
        inc(lNbUnclassified);
        lSumUnclassified := lSumUnclassified + lTask.Data.Estimation;
      end;
    end;
    //
    if (lTask.Data.Status=staDone) and (lTask.Data.DoneDate<lDateRef) then
    begin
      inc(lNbDone);
      lSumDone := lSumDone + lTask.Data.Estimation;
    end;
    //
    if (lTask.Data.Status=staInProgress) and (lTask.Data.InProgressDate<lDateRef) then
    begin
      inc(lNbInProgress);
      lSumInProgress := lSumInProgress + lTask.Data.Estimation;
    end;
    //
    if (lTask.Data.CreationDate>=FSprintRec.BeginDate)
        and (lTask.Data.CreationDate<=FSprintRec.EndDate)
        and (lTask.Data.CreationDate<lDateRef) then
    begin
      inc(lNbNew);
      lSumNew := lSumNew + lTask.Data.Estimation;
    end;
    //
    if (lTask.Data.CreationDate>=FSprintRec.BeginDate)
        and (lTask.Data.CreationDate<=FSprintRec.EndDate)
        and (lTask.Data.ParentTaskNum=CST_UnclassifiedTodo)
        and (lTask.Data.CreationDate<lDateRef) then
    begin
      inc(lNbNewUnclassified);
      lSumNewUnclassified := lSumNewUnclassified + lTask.Data.Estimation;
    end;
  end;
  //
  lStatsRec.StatDate := aDate;
  lStatsRec.SprintCode := FSprintRec.SprintCode;
  lStatsRec.NbScheduled := lNbScheduled;
  lStatsRec.NbUnclassified := lNbUnclassified;
  lStatsRec.NbDone := lNbDone;
  lStatsRec.NbInProgress := lNbInProgress;
  lStatsRec.NbNew := lNbNew;
  lStatsRec.NbNewUnclassified := lNbNewUnclassified;
  lStatsRec.SumScheduled := lSumScheduled;
  lStatsRec.SumUnclassified := lSumUnclassified;
  lStatsRec.SumDone := lSumDone;
  lStatsRec.SumInProgress := lSumInProgress;
  lStatsRec.SumNew := lSumNew;
  lStatsRec.SumNewUnclassified := lSumNewUnclassified;
  //
  result := lStatsRec;
end;

procedure TfrmSearch.InitTasks;
var
  lfrmTaskReport: TfrmTaskReport;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lTotEstim, lTotDone: integer;
begin
  txtYear.Text := IntToSTr(FSprintRec.Year);
  txtWeekNum.Text := IntToSTr(FSprintRec.WeekNum);
  txtPeriod.Text := Format('from %s to %s',[FormatDatetime(CST_FormatDate,FSprintRec.BeginDate),
                                            FormatDatetime(CST_FormatDate,FSprintRec.EndDate)]);

  // initialize in progress
  for i := 0 to CST_Max_TodosToShow - 1 do
  begin
    lfrmTaskReport := FfrmTaskReportList[i];
    lfrmTaskReport.Initialize(CST_TargetScheduledInProgress);
  end;
  //
  FSumTaskReportHeight := 0;
  lIndexForm := 0;
  lTotEstim := 0;
  lTotDone := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_TodosToShow then
      break;
    //
    lTask := FTaskList.Items(i);
    //
    lfrmTaskReport := FfrmTaskReportList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmTaskReport.Initialize(lTask, lDelay);
    inc(lIndexForm);
    //
    lTotEstim := lTotEstim + lTask.Data.Estimation;
    if lTask.Data.Status=staDone then
      lTotDone := lTotDone + lTask.Data.Estimation;
    //
    FSumTaskReportHeight := FSumTaskReportHeight + lfrmTaskReport.layBody.Height + 20;
  end;
  //
  rectPrintBody.Height := FSumTaskReportHeight + 100;
  if rectPrintBody.Height<900 then
    rectPrintBody.Height := 900;
  txtPeriod.Text := txtPeriod.Text + Format(' - %d task(s): %d/%d',[FTaskList.Count,lTotDone,lTotEstim]);
end;

end.
