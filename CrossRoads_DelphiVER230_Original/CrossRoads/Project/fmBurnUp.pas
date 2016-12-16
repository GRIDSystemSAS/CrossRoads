unit fmBurnUp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart, FMX.Layouts, unStructure, unGlobal, Data.DB, Datasnap.DBClient,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMXTee.Series;

type
  TfrmBurnUp = class(TForm)
    layBody: TLayout;
    chtBurnDown: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TAreaSeries;
    Series4: TAreaSeries;
    Series5: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSprintRec: TSprintRecord;
    FTaskList: TTaskList;
    procedure LoadData;
  public
    procedure Initialize(aDate: TDatetime);
  end;

var
  frmBurnUp: TfrmBurnUp;

implementation

uses unFunctions, dmTypeList;

{$R *.fmx}

procedure TfrmBurnUp.FormCreate(Sender: TObject);
begin
  FTaskList := TTaskList.Create;
end;

procedure TfrmBurnUp.FormDestroy(Sender: TObject);
begin
  FTaskList.Free;
end;


procedure TfrmBurnUp.Initialize(aDate: TDatetime);
var
  lID: string;
begin
  lID := GetSprintCode(aDate);
  TypeDatamodule.GetSprint(lID, FSprintRec);
  LoadData;
end;

procedure TfrmBurnUp.LoadData;
var
  lDate: TDatetime;
  lStatsRec: TStatsRecord;
  lSumBegin, lMinus, lAct: extended;
  i: integer;
begin
  // Burn-up
  Series1.Clear;
  lDate := FSprintRec.BeginDate;
  Series1.Add(0, DateToChartDate(lDate));
  while (lDate<=FSprintRec.EndDate) do
  begin
    TaskDataModule.GetStats(lDate, lStatsRec);
    Series1.Add(lStatsRec.SumDone, DateToChartDate(lDate+1));
    lDate := lDate + 1;
  end;
  // Burn-down
  lDate := FSprintRec.BeginDate;
  Series5.Clear;
  TaskDataModule.GetStats(lDate, lStatsRec);
  Series5.Add(lStatsRec.SumScheduled, DateToChartDate(lDate));
  while (lDate<=FSprintRec.EndDate) do
  begin
    TaskDataModule.GetStats(lDate, lStatsRec);
    Series5.Add(lStatsRec.SumScheduled-lStatsRec.SumDone, DateToChartDate(lDate+1));
    lDate := lDate + 1;
  end;
  // Average
  Series2.Clear;
  lDate := FSprintRec.EndDate;
  TaskDataModule.GetStats(lDate, lStatsRec);
  lSumBegin := lStatsRec.SumScheduled;
  lMinus := lSumBegin/7;
  lAct := 0;
  Series2.Add(lAct, DateToStr(lDate));
  for i := 1 to 7 do
  begin
    lAct := lAct + lMinus;
    if lAct>lSumBegin then
      lAct := lSumBegin;
    lDate := lDate + 1;
    Series2.Add(lAct, DateToChartDate(lDate));
  end;
  // Scheduled
  Series3.Clear;
  lDate := FSprintRec.BeginDate;
  while (lDate<=FSprintRec.EndDate) do
  begin
    TaskDataModule.GetStats(lDate, lStatsRec);
    Series3.Add(lStatsRec.SumScheduled-lStatsRec.SumUnclassified, DateToChartDate(lDate));
    lDate := lDate + 1;
  end;
  Series3.Add(lStatsRec.SumScheduled-lStatsRec.SumUnclassified, DateToChartDate(lDate));
  // Unscheduled
  Series4.Clear;
  lDate := FSprintRec.BeginDate;
  while (lDate<=FSprintRec.EndDate) do
  begin
    TaskDataModule.GetStats(lDate, lStatsRec);
    Series4.Add(lStatsRec.SumScheduled, DateToChartDate(lDate));
    lDate := lDate + 1;
  end;
  Series4.Add(lStatsRec.SumScheduled, DateToChartDate(lDate));
end;

end.
