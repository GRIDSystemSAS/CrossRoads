unit fmBurnDown;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart, FMX.Layouts, unStructure, unGlobal, Data.DB, Datasnap.DBClient,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMXTee.Series;

type
  TfrmBurnDown = class(TForm)
    layBody: TLayout;
    chtBurnDown: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSprintRec: TSprintRecord;
    FTaskList: TTaskList;
//    procedure InitData;
    procedure LoadData;
  public
    procedure Initialize(aDate: TDatetime);
  end;

var
  frmBurnDown: TfrmBurnDown;

implementation

uses unFunctions, dmTypeList;

{$R *.fmx}

procedure TfrmBurnDown.FormCreate(Sender: TObject);
begin
  FTaskList := TTaskList.Create;
end;

procedure TfrmBurnDown.FormDestroy(Sender: TObject);
begin
  FTaskList.Free;
end;

//procedure TfrmBurnDown.InitData;
//var
//  lDate: TDatetime;
//  lSumBegin, lMinus: extended;
//
//  function SumEstimation(aDate: Tdatetime): integer;
//  var
//    lNb, i: integer;
//    lTask: TTask;
//  begin
//    lNb := 0;
//    for i := 0 to FTaskList.Count-1 do
//    begin
//      lTask := FTaskList.Items(i);
//      if (lTask.Data.Status<>staDone) or (lTask.Data.DoneDate>=aDate) then
//        lNb := lNb + lTask.Data.Estimation;
//    end;
//    result := lNb;
//  end;
//
//  function SumDone(aDate: Tdatetime): integer;
//  var
//    lNb, i: integer;
//    lTask: TTask;
//    lDateCheck: Tdatetime;
//  begin
//    lNb := 0;
//    for i := 0 to FTaskList.Count-1 do
//    begin
//      lTask := FTaskList.Items(i);
//      lDateCheck := aDate;
//      if (lTask.Data.Status=staDone) and (lTask.Data.DoneDate>=lDateCheck)
//         and (lTask.Data.DoneDate<lDateCheck+1) then
//        lNb := lNb + lTask.Data.Estimation;
//    end;
//    result := lNb;
//  end;
//
//  function SumUnclassified(aDate: Tdatetime): integer;
//  var
//    lNb, i: integer;
//    lTask: TTask;
//    lDateCheck: Tdatetime;
//  begin
//    lNb := 0;
//    for i := 0 to FTaskList.Count-1 do
//    begin
//      lTask := FTaskList.Items(i);
//      lDateCheck := aDate;
//      if (lTask.Data.ParentTaskNum=CST_UnclassifiedTodo) and (lTask.Data.DoneDate>=lDateCheck)
//         and (lTask.Data.DoneDate<lDateCheck+1) then
//        lNb := lNb + lTask.Data.Estimation;
//    end;
//    result := lNb;
//  end;
//begin
//  lDate := FSprintRec.BeginDate;
//  Series1.Clear;
//  while (lDate<=FSprintRec.EndDate+1) do
//  begin
//    Series1.Add(SumEstimation(lDate), DateToChartDate(lDate));
//    lDate := lDate + 1;
//  end;
//  //
//  Series2.Clear;
//  lDate := FSprintRec.BeginDate;
//  lSumBegin := SumEstimation(lDate);
//  lMinus := lSumBegin/7;
//  Series2.Add(lSumBegin, DateToStr(lDate));
//  while (lDate<=FSprintRec.EndDate) do
//  begin
//    lSumBegin := lSumBegin - lMinus;
//    if lSumBegin<0 then
//      lSumBegin := 0;
//    lDate := lDate + 1;
//    Series2.Add(lSumBegin, DateToChartDate(lDate));
//  end;
//  //
//  lDate := FSprintRec.BeginDate;
//  Series3.Clear;
//  while (lDate<=FSprintRec.EndDate+1) do
//  begin
//    Series3.Add(SumDone(lDate), DateToChartDate(lDate));
//    lDate := lDate + 1;
//  end;
//  //
//  lDate := FSprintRec.BeginDate;
//  Series4.Clear;
//  while (lDate<=FSprintRec.EndDate+1) do
//  begin
//    Series4.Add(SumUnclassified(lDate), DateToChartDate(lDate));
//    lDate := lDate + 1;
//  end;
//end;

procedure TfrmBurnDown.Initialize(aDate: TDatetime);
var
  lID: string;
begin
  lID := GetSprintCode(aDate);
  TypeDatamodule.GetSprint(lID, FSprintRec);
  //
  LoadData;
end;

procedure TfrmBurnDown.LoadData;
var
  lDate: TDatetime;
  lSumBegin, lMinus: extended;
  lStatsRec: TStatsRecord;
  i: integer;
begin
  // Scheduled-Done
  lDate := FSprintRec.BeginDate;
  Series1.Clear;
  while (lDate<=FSprintRec.EndDate) do
  begin
    TaskDataModule.GetStats(lDate, lStatsRec);
    Series1.Add(lStatsRec.SumScheduled-lStatsRec.SumDone, DateToChartDate(lDate));
    lDate := lDate + 1;
  end;
  // Average
  Series2.Clear;
  lDate := FSprintRec.BeginDate;
  TaskDataModule.GetStats(lDate, lStatsRec);
  lSumBegin := lStatsRec.SumScheduled;
  lMinus := lSumBegin/6;
  Series2.Add(lSumBegin, DateToStr(lDate));
  for i := 1 to 6 do
  begin
    lSumBegin := lSumBegin - lMinus;
    if lSumBegin<0 then
      lSumBegin := 0;
    lDate := lDate + 1;
    Series2.Add(lSumBegin, DateToChartDate(lDate));
  end;
end;

end.
