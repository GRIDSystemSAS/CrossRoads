unit fmVelocity;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart, FMX.Layouts, unStructure, unGlobal, Data.DB, Datasnap.DBClient,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMXTee.Series;

type
  TfrmVelocity = class(TForm)
    layBody: TLayout;
    chtBurnDown: TChart;
    Series1: TBarSeries;
    Series3: TAreaSeries;
    Series4: TAreaSeries;
    Series2: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSprintRec: TSprintRecord;
    FTaskList: TTaskList;
    FSumDone, FSumSprint: integer;
  public
    procedure Initialize(aDate: TDatetime);
  end;

var
  frmVelocity: TfrmVelocity;

implementation

uses unFunctions, dmTypeList, StrUtils;

{$R *.fmx}

procedure TfrmVelocity.FormCreate(Sender: TObject);
begin
  FTaskList := TTaskList.Create;
end;

procedure TfrmVelocity.FormDestroy(Sender: TObject);
begin
  FTaskList.Free;
end;


procedure TfrmVelocity.Initialize(aDate: TDatetime);
var
  lID: string;
  lCurrDate: TDatetime;
  i: integer;
  lMoy: extended;
  lStatsRec: TStatsRecord;
  lSumTot: integer;
begin
  Series1.Clear;
  Series2.Clear;
  Series3.Clear;
  Series4.Clear;
  FSumDone := 0;
  FSumSprint := 0;
  //
  lCurrDate := aDate-63;
  lSumTot := 0;
  for i := 0 to 9 do
  begin
    lID := GetSprintCode(lCurrDate);
    TypeDatamodule.GetSprint(lID, FSprintRec);
    //
    TaskDataModule.GetStats(FSprintRec.EndDate, lStatsRec);
    Series1.Add(lStatsRec.SumDone, RightStr(FSprintRec.SprintCode,2));
    lSumTot :=  lStatsRec.SumScheduled;
    Series3.Add(lSumTot, RightStr(FSprintRec.SprintCode,2));
    lSumTot := lStatsRec.SumScheduled - lStatsRec.SumUnclassified;
    Series4.Add(lSumTot, RightStr(FSprintRec.SprintCode,2));
    if lStatsRec.SumDone>0 then
    begin
      FSumDone := FSumDone + lStatsRec.SumDone;
      inc(FSumSprint);
    end;
    //
    lCurrDate := lCurrDate + 7;
  end;
  //
  if FSumSprint>0 then
  begin
    lMoy := FSumDone/FSumSprint;
    lCurrDate := aDate-63;
    for i := 0 to 9 do
    begin
      lID := GetSprintCode(lCurrDate);
      TypeDatamodule.GetSprint(lID, FSprintRec);
      //
      Series2.Add(lMoy, RightStr(FSprintRec.SprintCode,2));
      //
      lCurrDate := lCurrDate + 7;
    end;
  end;
end;




end.
