unit dmTaskList;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, unLSNSyncDB,
  unStructure;

type
  TdtmTaskList = class(TDataModule)
    cdsTask: TClientDataSet;
    cdsTaskTaskNum: TIntegerField;
    cdsTaskProjectNum: TIntegerField;
    cdsTaskProjectTaskNum: TIntegerField;
    cdsTaskTaskName: TStringField;
    cdsTaskDescription: TStringField;
    cdsTaskTaskType: TIntegerField;
    cdsTaskTaskSubType: TIntegerField;
    cdsTaskPriority: TIntegerField;
    cdsTaskStatus: TIntegerField;
    cdsTaskParentTaskNum: TIntegerField;
    cdsTaskExternalLink: TStringField;
    cdsTaskLinkType: TIntegerField;
    cdsTaskCreationDate: TDateTimeField;
    cdsTaskModificationDate: TDateTimeField;
    cdsTaskEstimation: TIntegerField;
    cdsTaskDuration: TIntegerField;
    cdsTaskInProgressDate: TDateTimeField;
    cdsTaskDoneDate: TDateTimeField;
    cdsTaskAssignTo: TIntegerField;
    cdsTaskDueDate: TDateTimeField;
    cdsTaskSprintCode: TStringField;
    cdsTaskCategory1: TIntegerField;
    cdsTaskCategory2: TIntegerField;
    cdsTaskScheduledDate: TDateTimeField;
    cdsTaskSortOrder: TIntegerField;
    cdsTaskTag: TIntegerField;
    cdsTaskCustomDate: TDateTimeField;
    cdsTaskAutoPostponedDate: TDateTimeField;
    cdsTaskAutoPostponedTimes: TIntegerField;
    cdsStats: TClientDataSet;
    cdsStatsStatDate: TDateField;
    cdsStatsSprintCode: TStringField;
    cdsStatsNbScheduled: TIntegerField;
    cdsStatsNbUnclassified: TIntegerField;
    cdsStatsNbDone: TIntegerField;
    cdsStatsNbNew: TIntegerField;
    cdsStatsNbNewUnclassified: TIntegerField;
    cdsStatsNbInProgress: TIntegerField;
    cdsStatsSumScheduled: TIntegerField;
    cdsStatsSumUnclassified: TIntegerField;
    cdsStatsSumDone: TIntegerField;
    cdsStatsSumNew: TIntegerField;
    cdsStatsSumNewUnclassified: TIntegerField;
    cdsStatsSumInProgress: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FInitialized: Boolean;
    FLSNSyncDB: TLSNSyncDB;

  public
    procedure Initialize(aDirectory: string);
    procedure Load;
    procedure Save;
    procedure PrepareData;
    //
    function GetTask(aTaskNum: integer; var aTaskRec: TTaskRecord): Boolean;
    procedure UpdateTask(aTaskRec: TTaskRecord);
    procedure DeleteTask(aTaskNum: integer);
    procedure FilterTaskOnParent(aProjectNum, aTaskCategory, aParentTaskNum: integer; aSprintCode: string= '');
    //
    function GetStats(aStatDate: TDatetime; var aStatsRec: TStatsRecord): Boolean;
    procedure UpdateStats(aStatsRec: TStatsRecord);
    procedure FilterStatsOnWeek(aSprintCode: string);
    // counters
    function GetTargetCount(aProjectNum: integer): integer;
    function GetToDoCount(aProjectNum: integer): integer;
    function GetDoneCount(aProjectNum: integer): integer;
    function GetScheduledCount(aProjectNum: integer): integer;
    function GetToDoForTargetCount(aProjectNum, aTargetNum: integer): integer;
    function GetDoneForTargetCount(aProjectNum, aTargetNum: integer): integer;
    function GetChildrenCount(aProjectNum, aTargetNum: integer): integer;
    //
    property Initialized: Boolean read FInitialized;
  end;

var
  dtmTaskList: TdtmTaskList;

implementation

uses dmTypeList, unGlobal, DateUtils, unFunctions;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdtmTaskList.DataModuleCreate(Sender: TObject);
begin
  FLSNSyncDB := TLSNSyncDB.Create;
  FInitialized := False;
end;

procedure TdtmTaskList.DataModuleDestroy(Sender: TObject);
begin
  FLSNSyncDB.Destroy;
end;

procedure TdtmTaskList.DeleteTask(aTaskNum: integer);
begin
  cdsTask.Filtered := False;
  if cdsTask.FindKey([aTaskNum]) then
    cdsTask.Delete;
end;

function TdtmTaskList.GetToDoCount(aProjectNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d and (Status = %d Or Status = %d Or Status = %d Or Status = %d)',
                    [aProjectNum, tstTarget, staValidated, staScheduled, staInProgress, staDone]);
//  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d',
//                    [aProjectNum, tstTarget]);
  cdsTask.Filtered := True;
  result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;

function TdtmTaskList.GetChildrenCount(aProjectNum,
  aTargetNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and ParentTaskNum=%d',
                    [aProjectNum, aTargetNum]);
  cdsTask.Filtered := True;
  Result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;

function TdtmTaskList.GetDoneCount(aProjectNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d and Status=%d',
                    [aProjectNum, tstTarget, staDone]);
  cdsTask.Filtered := True;
  result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;

function TdtmTaskList.GetTargetCount(aProjectNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d and Status = %d',
                    [aProjectNum, tstTarget, staCreated]);
  cdsTask.Filtered := True;
  result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;

function TdtmTaskList.GetScheduledCount(aProjectNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d and (Status=%d or Status=%d)',
                    [aProjectNum, tstTarget, staScheduled, staInProgress]);
  cdsTask.Filtered := True;
  result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;



function TdtmTaskList.GetToDoForTargetCount(aProjectNum, aTargetNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d and ParentTaskNum=%d and (Status=%d Or Status=%d Or Status=%d Or Status=%d)',
                    [aProjectNum, tstTarget, aTargetNum, staValidated, staScheduled, staInProgress, staDone]);
  cdsTask.Filtered := True;
  result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;

function TdtmTaskList.GetDoneForTargetCount(aProjectNum, aTargetNum: integer): integer;
begin
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('ProjectNum=%d and TaskType=%d and ParentTaskNum=%d and Status=%d',
                    [aProjectNum, tstTarget, aTargetNum, staDone]);
  cdsTask.Filtered := True;
  result := cdsTask.RecordCount;
  cdsTask.Filtered := False;
end;

procedure TdtmTaskList.FilterStatsOnWeek(aSprintCode: string);
var
  lFilter: string;
begin
  cdsStats.Filtered := False;
  lFilter := Format('SprintCode = ''%s'' ',[aSprintCode]);
  cdsStats.Filter := lFilter;
  cdsStats.Filtered := True;
end;

procedure TdtmTaskList.FilterTaskOnParent(aProjectNum, aTaskCategory, aParentTaskNum: integer; aSprintCode: string= '');
var
  lFilter: string;
begin
  cdsTask.Filtered := False;
  case aTaskCategory of
    CST_Target : lFilter := Format('ProjectNum=%d and TaskType=%d and Status=%d',[aProjectNum, tstTarget, staCreated]);
    CST_TargetTodo : lFilter := Format('ProjectNum=%d and TaskType=%d and ParentTaskNum=%d '
                                        +'and (Status =%d or Status =%d or Status =%d Or Status =%d)',[aProjectNum, tstTarget, aParentTaskNum, staValidated, staScheduled, staInProgress, staDone]);
    CST_TargetTodoAll : lFilter := Format('ProjectNum=%d and TaskType=%d '
                                        +'and (Status=%d or Status=%d or Status=%d or Status=%d)',[aProjectNum, tstTarget, staValidated, staScheduled, staInProgress, staDone]);
    CST_TargetToCome : lFilter := Format('ProjectNum=%d and TaskType=%d '
                                        +'and Status=%d and DueDate<>0 ',[aProjectNum, tstTarget, staValidated]);
    CST_TargetScheduled : lFilter := Format('ProjectNum=%d and TaskType=%d '
                                        +'and (Status=%d or Status=%d Or Status=%d) '
                                        +'and SprintCode = ''%s'' ',
                                        [aProjectNum, tstTarget, staScheduled, staInProgress, staDone,
                                        aSprintCode]);
    CST_TargetTask : lFilter := Format('ProjectNum=%d and TaskType=%d and ParentTaskNum=%d '
                                        +'and (Status=%d or Status=%d)',[aProjectNum, tstTask, aParentTaskNum, staCreated, staDone]);
    CST_TargetComment : lFilter := Format('ProjectNum=%d and TaskType=%d and ParentTaskNum=%d ',
                                          [aProjectNum, tstComment, aParentTaskNum]);
    CST_TargetLink : lFilter := Format('ProjectNum=%d and TaskType=%d and ParentTaskNum=%d ',
                                          [aProjectNum, tstLink, aParentTaskNum]);
  end;
  cdsTask.Filter := lFilter;
  cdsTask.Filtered := True;
end;

procedure TdtmTaskList.Initialize(aDirectory: string);
begin
  FLSNSyncDB.RegisterDataset(cdsTask, 'Tasks.cds');
  FLSNSyncDB.RegisterDataset(cdsStats, 'Stats.cds');
  FLSNSyncDB.Initialize(aDirectory, True);
  FInitialized := True;
end;

procedure TdtmTaskList.Load;
begin
  FLSNSyncDB.Load;
  PrepareData;
end;

procedure TdtmTaskList.Save;
begin
  cdsTask.Filtered := False;
  cdsStats.Filtered := False;
  FLSNSyncDB.Save;
end;



function TdtmTaskList.GetTask(aTaskNum: integer; var aTaskRec: TTaskRecord): Boolean;
begin
  result := False;
  if cdsTask.FindKey([aTaskNum]) then
  begin
    aTaskRec.TaskNum := cdsTask.FieldByName('TaskNum').AsInteger;
    aTaskRec.ProjectNum := cdsTask.FieldByName('ProjectNum').AsInteger;
    aTaskRec.ProjectTaskNum := cdsTask.FieldByName('ProjectTaskNum').AsInteger;
    aTaskRec.TaskName := cdsTask.FieldByName('TaskName').AsString;
    aTaskRec.Description := cdsTask.FieldByName('Description').AsString;
    aTaskRec.TaskType := cdsTask.FieldByName('TaskType').AsInteger;
    aTaskRec.TaskSubType := cdsTask.FieldByName('TaskSubType').AsInteger;
    aTaskRec.Priority := cdsTask.FieldByName('Priority').AsInteger;
    aTaskRec.Status := cdsTask.FieldByName('Status').AsInteger;
    aTaskRec.ParentTaskNum := cdsTask.FieldByName('ParentTaskNum').AsInteger;
    aTaskRec.ExternalLink := cdsTask.FieldByName('ExternalLink').AsString;
    aTaskRec.LinkType := cdsTask.FieldByName('LinkType').AsInteger;
    aTaskRec.CreationDate := cdsTask.FieldByName('CreationDate').AsDateTime;
    aTaskRec.ModificationDate := cdsTask.FieldByName('ModificationDate').AsDateTime;
    aTaskRec.Estimation := cdsTask.FieldByName('Estimation').AsInteger;
    aTaskRec.Duration := cdsTask.FieldByName('Duration').AsInteger;
    aTaskRec.InProgressDate := cdsTask.FieldByName('InProgressDate').AsDateTime;
    aTaskRec.DoneDate := cdsTask.FieldByName('DoneDate').AsDateTime;
    aTaskRec.AssignTo := cdsTask.FieldByName('AssignTo').AsInteger;
    aTaskRec.SprintCode := cdsTask.FieldByName('SprintCode').AsString;
    aTaskRec.DueDate := cdsTask.FieldByName('DueDate').AsDateTime;
    aTaskRec.Category1 := cdsTask.FieldByName('Category1').AsInteger;
    aTaskRec.Category2 := cdsTask.FieldByName('Category2').AsInteger;
    aTaskRec.ScheduledDate := cdsTask.FieldByName('ScheduledDate').AsDateTime;
    aTaskRec.AutoPostponedDate := cdsTask.FieldByName('AutoPostponedDate').AsDateTime;
    aTaskRec.AutoPostponedTimes := cdsTask.FieldByName('AutoPostponedTimes').AsInteger;
    aTaskRec.SortOrder := cdsTask.FieldByName('SortOrder').AsInteger;
    aTaskRec.Tag := cdsTask.FieldByName('Tag').AsInteger;
    aTaskRec.CustomDate := cdsTask.FieldByName('CustomDate').AsDateTime;
    result := True;
  end;
end;

procedure TdtmTaskList.UpdateTask(aTaskRec: TTaskRecord);
begin
  cdsTask.Filtered := False;
  if cdsTask.FindKey([aTaskRec.TaskNum]) then
  begin
    cdsTask.Edit;
    cdsTask.FieldByName('ProjectNum').AsInteger := aTaskRec.ProjectNum;
    cdsTask.FieldByName('ProjectTaskNum').AsInteger := aTaskRec.ProjectTaskNum;
    cdsTask.FieldByName('TaskName').AsString := aTaskRec.TaskName;
    cdsTask.FieldByName('Description').AsString := aTaskRec.Description;
    cdsTask.FieldByName('TaskType').AsInteger := aTaskRec.TaskType;
    cdsTask.FieldByName('TaskSubType').AsInteger := aTaskRec.TaskSubType;
    cdsTask.FieldByName('Priority').AsInteger := aTaskRec.Priority;
    cdsTask.FieldByName('Status').AsInteger := aTaskRec.Status;
    cdsTask.FieldByName('ParentTaskNum').AsInteger := aTaskRec.ParentTaskNum;
    cdsTask.FieldByName('ExternalLink').AsString := aTaskRec.ExternalLink;
    cdsTask.FieldByName('LinkType').AsInteger := aTaskRec.LinkType;
    cdsTask.FieldByName('CreationDate').AsDateTime := aTaskRec.CreationDate;
    cdsTask.FieldByName('ModificationDate').AsDateTime := aTaskRec.ModificationDate;
    cdsTask.FieldByName('Estimation').AsInteger := aTaskRec.Estimation;
    cdsTask.FieldByName('Duration').AsInteger := aTaskRec.Duration;
    cdsTask.FieldByName('InProgressDate').AsDateTime := aTaskRec.InProgressDate;
    cdsTask.FieldByName('DoneDate').AsDateTime := aTaskRec.DoneDate;
    cdsTask.FieldByName('AssignTo').AsInteger := aTaskRec.AssignTo;
    cdsTask.FieldByName('SprintCode').AsString := aTaskRec.SprintCode;
    cdsTask.FieldByName('DueDate').AsDateTime := aTaskRec.DueDate;
    cdsTask.FieldByName('Category1').AsInteger := aTaskRec.Category1;
    cdsTask.FieldByName('Category2').AsInteger := aTaskRec.Category2;
    cdsTask.FieldByName('ScheduledDate').AsDateTime := aTaskRec.ScheduledDate;
    cdsTask.FieldByName('AutoPostponedDate').AsDateTime := aTaskRec.AutoPostponedDate;
    cdsTask.FieldByName('AutoPostponedTimes').AsInteger := aTaskRec.AutoPostponedTimes;
    cdsTask.FieldByName('SortOrder').AsInteger := aTaskRec.SortOrder;
    cdsTask.FieldByName('Tag').AsInteger := aTaskRec.Tag;
    cdsTask.FieldByName('CustomDate').AsDateTime := aTaskRec.CustomDate;
    cdsTask.Post;
  end
  else
  begin
    cdsTask.Insert;
    cdsTask.FieldByName('TaskNum').AsInteger := aTaskRec.TaskNum;
    cdsTask.FieldByName('ProjectNum').AsInteger := aTaskRec.ProjectNum;
    cdsTask.FieldByName('ProjectTaskNum').AsInteger := aTaskRec.ProjectTaskNum;
    cdsTask.FieldByName('TaskName').AsString := aTaskRec.TaskName;
    cdsTask.FieldByName('Description').AsString := aTaskRec.Description;
    cdsTask.FieldByName('TaskType').AsInteger := aTaskRec.TaskType;
    cdsTask.FieldByName('TaskSubType').AsInteger := aTaskRec.TaskSubType;
    cdsTask.FieldByName('Priority').AsInteger := aTaskRec.Priority;
    cdsTask.FieldByName('Status').AsInteger := aTaskRec.Status;
    cdsTask.FieldByName('ParentTaskNum').AsInteger := aTaskRec.ParentTaskNum;
    cdsTask.FieldByName('ExternalLink').AsString := aTaskRec.ExternalLink;
    cdsTask.FieldByName('LinkType').AsInteger := aTaskRec.LinkType;
    cdsTask.FieldByName('CreationDate').AsDateTime := aTaskRec.CreationDate;
    cdsTask.FieldByName('ModificationDate').AsDateTime := aTaskRec.ModificationDate;
    cdsTask.FieldByName('Estimation').AsInteger := aTaskRec.Estimation;
    cdsTask.FieldByName('Duration').AsInteger := aTaskRec.Duration;
    cdsTask.FieldByName('InProgressDate').AsDateTime := aTaskRec.InProgressDate;
    cdsTask.FieldByName('DoneDate').AsDateTime := aTaskRec.DoneDate;
    cdsTask.FieldByName('AssignTo').AsInteger := aTaskRec.AssignTo;
    cdsTask.FieldByName('SprintCode').AsString := aTaskRec.SprintCode;
    cdsTask.FieldByName('DueDate').AsDateTime := aTaskRec.DueDate;
    cdsTask.FieldByName('Category1').AsInteger := aTaskRec.Category1;
    cdsTask.FieldByName('Category2').AsInteger := aTaskRec.Category2;
    cdsTask.FieldByName('ScheduledDate').AsDateTime := aTaskRec.ScheduledDate;
    cdsTask.FieldByName('AutoPostponedDate').AsDateTime := aTaskRec.AutoPostponedDate;
    cdsTask.FieldByName('AutoPostponedTimes').AsInteger := aTaskRec.AutoPostponedTimes;
    cdsTask.FieldByName('SortOrder').AsInteger := aTaskRec.SortOrder;
    cdsTask.FieldByName('Tag').AsInteger := aTaskRec.Tag;
    cdsTask.FieldByName('CustomDate').AsDateTime := aTaskRec.CustomDate;
    cdsTask.Post;
  end;
end;

procedure TdtmTaskList.PrepareData;
var
  lSprintCode: string;
  lSprintCodeInt: integer;
begin
  lSprintCode := GetSprintCode(date);
  lSprintCodeInt := SprintCodeToInt(lSprintCode);
  // reaffect old pending to-do
  cdsTask.Filtered := False;
  cdsTask.Filter := Format('TaskType=%d and (Status=%d or Status=%d)',
                    [tstTarget, staScheduled, staInProgress]);
  cdsTask.Filtered := True;
  cdsTask.First;
  while not cdsTask.Eof do
  begin
    if SprintCodeToInt(cdsTask.FieldByName('SprintCode').AsString)<lSprintCodeInt then
    begin
      cdsTask.Edit;
      cdsTask.FieldByName('SprintCode').AsString := lSprintCode;
      cdsTask.FieldByName('AutoPostponedDate').AsDateTime := now;
      cdsTask.FieldByName('AutoPostponedTimes').AsInteger := cdsTask.FieldByName('AutoPostponedTimes').AsInteger+1;
      cdsTask.Post;
    end;
    cdsTask.Next;
  end;
  cdsTask.Filtered := False;
end;



function TdtmTaskList.GetStats(aStatDate: TDatetime;
  var aStatsRec: TStatsRecord): Boolean;
begin
  result := False;
  if cdsStats.FindKey([aStatDate]) then
  begin
    aStatsRec.StatDate := cdsStats.FieldByName('StatDate').AsDatetime;
    aStatsRec.SprintCode := cdsStats.FieldByName('SprintCode').AsString;
    aStatsRec.NbScheduled := cdsStats.FieldByName('NbScheduled').AsInteger;
    aStatsRec.NbUnclassified := cdsStats.FieldByName('NbUnclassified').AsInteger;
    aStatsRec.NbDone := cdsStats.FieldByName('NbDone').AsInteger;
    aStatsRec.NbNew := cdsStats.FieldByName('NbNew').AsInteger;
    aStatsRec.NbNewUnclassified := cdsStats.FieldByName('NbNewUnclassified').AsInteger;
    aStatsRec.NbInProgress := cdsStats.FieldByName('NbInProgress').AsInteger;
    aStatsRec.SumScheduled := cdsStats.FieldByName('SumScheduled').AsInteger;
    aStatsRec.SumUnclassified := cdsStats.FieldByName('SumUnclassified').AsInteger;
    aStatsRec.SumDone := cdsStats.FieldByName('SumDone').AsInteger;
    aStatsRec.SumNew := cdsStats.FieldByName('SumNew').AsInteger;
    aStatsRec.SumNewUnclassified := cdsStats.FieldByName('SumNewUnclassified').AsInteger;
    aStatsRec.SumInProgress := cdsStats.FieldByName('SumInProgress').AsInteger;
    result := True;
  end
  else
  begin
    aStatsRec.StatDate := aStatDate;
    aStatsRec.SprintCode := GetSprintCode(aStatDate);
    aStatsRec.NbScheduled := 0;
    aStatsRec.NbUnclassified := 0;
    aStatsRec.NbDone := 0;
    aStatsRec.NbNew := 0;
    aStatsRec.NbNewUnclassified := 0;
    aStatsRec.NbInProgress := 0;
    aStatsRec.SumScheduled := 0;
    aStatsRec.SumUnclassified := 0;
    aStatsRec.SumDone := 0;
    aStatsRec.SumNew := 0;
    aStatsRec.SumNewUnclassified := 0;
    aStatsRec.SumInProgress := 0;
  end;
end;

procedure TdtmTaskList.UpdateStats(aStatsRec: TStatsRecord);
begin
  cdsStats.Filtered := False;
  if cdsStats.FindKey([aStatsRec.StatDate]) then
  begin
    cdsStats.Edit;
    cdsStats.FieldByName('SprintCode').AsString := aStatsRec.SprintCode;
    cdsStats.FieldByName('NbScheduled').AsInteger := aStatsRec.NbScheduled;
    cdsStats.FieldByName('NbUnclassified').AsInteger := aStatsRec.NbUnclassified;
    cdsStats.FieldByName('NbDone').AsInteger := aStatsRec.NbDone;
    cdsStats.FieldByName('NbNew').AsInteger := aStatsRec.NbNew;
    cdsStats.FieldByName('NbNewUnclassified').AsInteger := aStatsRec.NbNewUnclassified;
    cdsStats.FieldByName('NbInProgress').AsInteger := aStatsRec.NbInProgress;
    cdsStats.FieldByName('SumScheduled').AsInteger := aStatsRec.SumScheduled;
    cdsStats.FieldByName('SumUnclassified').AsInteger := aStatsRec.SumUnclassified;
    cdsStats.FieldByName('SumDone').AsInteger := aStatsRec.SumDone;
    cdsStats.FieldByName('SumNew').AsInteger := aStatsRec.SumNew;
    cdsStats.FieldByName('SumNewUnclassified').AsInteger := aStatsRec.SumNewUnclassified;
    cdsStats.FieldByName('SumInProgress').AsInteger := aStatsRec.SumInProgress;
    cdsStats.Post;
  end
  else
  begin
    cdsStats.Insert;
    cdsStats.FieldByName('StatDate').AsDatetime := aStatsRec.StatDate;
    cdsStats.FieldByName('SprintCode').AsString := aStatsRec.SprintCode;
    cdsStats.FieldByName('NbScheduled').AsInteger := aStatsRec.NbScheduled;
    cdsStats.FieldByName('NbUnclassified').AsInteger := aStatsRec.NbUnclassified;
    cdsStats.FieldByName('NbDone').AsInteger := aStatsRec.NbDone;
    cdsStats.FieldByName('NbNew').AsInteger := aStatsRec.NbNew;
    cdsStats.FieldByName('NbNewUnclassified').AsInteger := aStatsRec.NbNewUnclassified;
    cdsStats.FieldByName('NbInProgress').AsInteger := aStatsRec.NbInProgress;
    cdsStats.FieldByName('SumScheduled').AsInteger := aStatsRec.SumScheduled;
    cdsStats.FieldByName('SumUnclassified').AsInteger := aStatsRec.SumUnclassified;
    cdsStats.FieldByName('SumDone').AsInteger := aStatsRec.SumDone;
    cdsStats.FieldByName('SumNew').AsInteger := aStatsRec.SumNew;
    cdsStats.FieldByName('SumNewUnclassified').AsInteger := aStatsRec.SumNewUnclassified;
    cdsStats.FieldByName('SumInProgress').AsInteger := aStatsRec.SumInProgress;
    cdsStats.Post;
  end;
end;

end.
