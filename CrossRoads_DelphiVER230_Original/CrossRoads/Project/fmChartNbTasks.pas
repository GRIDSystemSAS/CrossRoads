unit fmChartNbTasks;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart, FMX.Layouts, unStructure, unGlobal, Data.DB, Datasnap.DBClient,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMXTee.Series;

type
  TfrmChartNbTasks = class(TForm)
    layBody: TLayout;
    chtBurnDown: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSprintRec: TSprintRecord;
    FTaskList: TTaskList;
    FSumDone, FSumCreated: integer;
    procedure InitData;
  public
    procedure Initialize(aDate: TDatetime);
  end;

var
  frmChartNbTasks: TfrmChartNbTasks;

implementation

uses unFunctions, dmTypeList;

{$R *.fmx}

procedure TfrmChartNbTasks.FormCreate(Sender: TObject);
begin
  FTaskList := TTaskList.Create;
end;

procedure TfrmChartNbTasks.FormDestroy(Sender: TObject);
begin
  FTaskList.Free;
end;

procedure TfrmChartNbTasks.InitData;
var
  lSumDone: integer;

  function SumDone: integer;
  var
    lNb, i: integer;
    lTask: TTask;
  begin
    lNb := 0;
    for i := 0 to FTaskList.Count-1 do
    begin
      lTask := FTaskList.Items(i);
      if (lTask.Data.Status=staDone)
        and (lTask.Data.DoneDate>=FSprintRec.BeginDate)
        and (lTask.Data.DoneDate<=FSprintRec.EndDate) then
        inc(lNb);
    end;
    result := lNb;
  end;

  function SumCreated: integer;
  var
    lNb, i: integer;
    lTask: TTask;
  begin
    lNb := 0;
    for i := 0 to FTaskList.Count-1 do
    begin
      lTask := FTaskList.Items(i);
      if (lTask.Data.CreationDate>=FSprintRec.BeginDate)
        and (lTask.Data.CreationDate<=FSprintRec.EndDate) then
        inc(lNb);
    end;
    result := lNb;
  end;
begin
  FSumDone := FSumDone + SumDone;
  Series1.Add(FSumDone, FSprintRec.SprintCode);
  //
  FSumCreated := FSumCreated + SumCreated;
  Series2.Add(FSumCreated, FSprintRec.SprintCode);
end;

procedure TfrmChartNbTasks.Initialize(aDate: TDatetime);
var
  lID: string;
  lCurrDate: TDatetime;
  i: integer;
begin
  Series1.Clear;
  Series2.Clear;
  FSumDone := 0;
  FSumCreated := 0;
  //
  lCurrDate := aDate-42;
  for i := 0 to 6 do
  begin
    lID := GetSprintCode(lCurrDate);
    TypeDatamodule.GetSprint(lID, FSprintRec);
    //
    FTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetTodoAll, 0, '');
    InitData;
    //
    lCurrDate := lCurrDate + 7;
  end;
end;

end.
