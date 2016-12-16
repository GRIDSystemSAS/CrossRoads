unit unGlobal;

interface

uses System.SysUtils, System.UITypes, System.Classes,
     unLSNUtils, dmLSNParameters, dmProjectList, unStructure, unFunctions,
     dmTypeList, dmTaskList, IOUtils, dmUserLog;

type
  TTask = class
  private
    FTaskRecord: TTaskRecord;
    procedure InitCreatedTask;
  public
    constructor Create;
    destructor Destroy;
    procedure Initialize(aTaskNum: integer);
    procedure RefreshData;
    //
    procedure CreateTarget(aTaskNum, aProjectNum: integer);
    procedure CreateTodoTarget(aTaskNum, aProjectNum, aProjectTaskNum, aParentTaskNum: integer);
    procedure CreateTask(aTaskNum, aProjectNum, aParentTaskNum: integer);
    procedure CreateComment(aTaskNum, aProjectNum, aParentTaskNum: integer);
    procedure CreateLink(aTaskNum, aProjectNum, aParentTaskNum: integer);
    //
    property Data: TTaskRecord read FTaskRecord write FTaskRecord;
  end;

  TTaskList = class
  private
    FTaskList: TList;
  public
    constructor Create;
    destructor Destroy;
    function Add(aTaskNum: integer): TTask;
    procedure Load(aProjectNum, aTaskCategory, aParentTaskNum: integer; aSprintCode: string= '');
    function Search(aTaskNum: integer): TTask;
    function Count: integer;
    function Items(aIndex: integer): TTask;
    procedure SortByDoneDate;
    //
    function CountChildren(aTaskNum: integer): integer;
    function CountChildrenDone(aTaskNum: integer): integer;
  end;

  TSprint = class
  private
    FSprintRecord: TSprintRecord;
    FTaskList: TList;
  public
    constructor Create;
    destructor Destroy;
    procedure Initialize(aDate: TDatetime);
    function TaskCount: integer;
    function Tasks(aIndex: integer): TTask;
    procedure TaskAdd(aTask: TTask);
    //
    property Data: TSprintRecord read FSprintRecord write FSprintRecord;
  end;

  TProject = class
  private
    FProjectRecord: TProjectRecord;
    FTargetList: TTaskList;
    FTargetTodoList: TTaskList;
  public
    constructor Create;
    destructor Destroy;
    procedure Initialize(aProjectNum: integer);
    procedure RefreshData;
    function TargetCount: integer;
    function TargetItems(aIndex: integer): TTask;
    function TargetAdd(aTaskNum: integer): TTask;
    function TargetSearch(aTaskNum: integer): TTask;
    function TargetTodoCount: integer;
    function TargetTodoItems(aIndex: integer): TTask;
    function TargetTodoAdd(aTaskNum: integer): TTask;
    function TargetToDoSearch(aTaskNum: integer): TTask;
    //
    procedure LoadTarget;
    procedure LoadTargetToDo(aParentTaskNum: integer);
    //
    property Data: TProjectRecord read FProjectRecord write FProjectRecord;
  end;

  TProjectList = class
  private
    FProjectList: TList;
  public
    constructor Create;
    destructor Destroy;
    procedure Load;
    function Add(aProjectNum: integer): TProject;
    function Search(aProjectNum: integer): TProject;
    function Count: integer;
    function Items(aIndex: integer): TProject;
    procedure Clear;
  end;

  TGlobalVariable = class
  private
    FCurrentProjectNum: integer;
    FCurrentProject: TProject;
    FCurrentSprintCode: string;
    FMainDataDirectory, FDataDirectory: string;
    FSelectedFolder: string;
    FProjectList: TProjectList;
    FTargetList: TTaskList;
    FWeekCapacity: integer;
    FStatsRec: TStatsRecord;
    //
    procedure SetCurrentProjectNum(aValue: integer);
  public
    constructor Create;
    destructor Destroy;
    procedure Initialize;
    //
    property CurrentProjectNum: integer read FCurrentProjectNum write SetCurrentProjectNum;
    property CurrentProject: TProject read FCurrentProject write FCurrentProject;
    property CurrentSprintCode: string read FCurrentSprintCode write FCurrentSprintCode;
    property MainDataDirectory: string read FMainDataDirectory write FMainDataDirectory;
    property DataDirectory: string read FDataDirectory write FDataDirectory;
    property SelectedFolder: string read FSelectedFolder write FSelectedFolder;
    property ProjectList: TProjectList read FProjectList;
    property TargetList: TTaskList read FTargetList;
    property WeekCapacity: integer read FWeekCapacity write FWeekCapacity;
    property StatsRecord: TStatsRecord read FStatsRec write FStatsRec;
  end;


var
  GlobalVar : TGlobalVariable;
  MyPath: TLSNPath;
  MyGlobalParams: TdtmLSNParameters;
  MyParams: TdtmLSNParameters;
  MyFolders: TdtmLSNParameters;
  MainDatamodule: TdtmProjectList;
  TaskDatamodule: TdtmTaskList;
  TypeDatamodule: TdtmTypeList;
  UserLogDatamodule : TdtmUserLog;


const
  CST_ApplicationName = 'CrossRoads';
  CST_ApplicationVersion = '01.00';
  CST_ApplicationLongName = 'CrossRoads® Version 00.80.00.20120122';
  CST_FormatDatetime = 'DD.MM.YYYY hh:nn';
  CST_FormatDate = 'DD.MM.YYYY';
  CST_DateMonth = 'DD.MM';
  CST_DateForChart = 'DD.MM.YY';
  CST_ActivateLink = False;
  //
  CST_Max_ProjectsToShow = 15;
  CST_Max_TargetsToShow = 16;
  CST_Max_TodosToShow = 20;
  CST_Max_PendingsToShow = 10;
  CST_Max_FoldersToShow = 10;
  CST_Max_TasksToShow = 10;
  CST_Max_CommentsToShow = 10;
  CST_Max_LogsToShow = 15;
  CST_Max_LinksToShow = 2;
  CST_Max_PendingTasksinWelcome = 7;
  CST_Max_ComingTasksinWelcome = 4;
  CST_FolderSize = 100;
  CST_TasksHeight = 17;
  CST_LinksHeight = 17;
  CST_CommentHeight = 85;
  CST_LogHeight = 60;
  //
  CST_TransitionDelay = 0.04;
  //
  CST_Menu_Projects = 0;
  CST_Menu_Tasks = 1;
  CST_Menu_TargetToDo = 2;
  CST_Menu_ScrumBoard = 3;
  CST_Menu_Resume = 4;
  CST_Menu_Search = 5;
  //
  CST_Target = 1;
  CST_TargetTodo = 2;
  CST_TargetTodoUnclassified = 3;
  CST_TargetScheduled = 4;
  CST_TargetScheduledWaiting = 5;
  CST_TargetScheduledInProgress = 6;
  CST_TargetScheduledDone = 7;
  CST_TargetTask = 8;
  CST_TargetPendingInWelcomePage = 9;
  CST_TargetComment = 10;
  CST_TargetLink = 11;
  CST_TargetTodoAll = 12;
  CST_TargetResume = 13;
  CST_TargetToCome = 14;
  //
  CST_SprintOld = 0;
  CST_SprintResume = 99;
  //
  CST_UnclassifiedTodo = -99;
  CST_UnclassifiedTodoLabel = 'Unscheduled';
  //
  CST_UserLogType_Info = 1;
  // Parameters
  prmMainDataDirectory = 'MainDataDirectory';
  prmSelectedFolder = 'SelectedFolder';
  prmCurrentProjectNum = 'CurrentProjectNum';
  prmNextProjectNum = 'NextProjectNum';
  prmNextTaskNum = 'NextTaskNum';
  prmApplicationVersion = 'ApplicationVersion';
  prmWeekCapacity = 'WeekCapacity';
  prmNextFolderNum = 'NextFolderNum';
  prmSelectedTargetNum = 'SelectedTargetNum';
  prmFolderDirectory = '%s_DataDirectory';
  


implementation

uses DateUtils, StrUtils;

{$REGION 'TGlobalVariable'}

constructor TGlobalVariable.Create;
begin
  FProjectList := TProjectList.Create;
  FTargetList := TTaskList.Create;
end;

destructor TGlobalVariable.Destroy;
begin
  FProjectList.Free;
  FTargetList.Free;
end;

procedure TGlobalVariable.Initialize;
begin
  FProjectList.Clear;
  FProjectList.Load;
end;

procedure TGlobalVariable.SetCurrentProjectNum(aValue: integer);
var
  lPath: string;
begin
  FCurrentProjectNum := aValue;
  FCurrentProject := FProjectList.Search(FCurrentProjectNum);
  if FCurrentProject=nil then
    FCurrentProject := FProjectList.Add(FCurrentProjectNum);
  // save old taskdatamodule
  if TaskDatamodule.Initialized then
    TaskDatamodule.Save;
  // save old userlogdatamodule
  if UserLogDatamodule.Initialized then
    UserLogDatamodule.Save;
  //
  lPath := FDataDirectory+TPath.DirectorySeparatorChar+IntToStr(FCurrentProject.Data.ProjectNum);
  TaskDatamodule.Initialize(lPath);
  TaskDatamodule.Load;
  UserLogDatamodule.Initialize(lPath);
  UserLogDatamodule.Load;
  FCurrentProject.LoadTarget;
  //
  TaskDatamodule.GetStats(Date, FStatsRec);
end;

{$ENDREGION}

{$REGION 'TSprint'}

constructor TSprint.Create;
begin
  FTaskList := TList.Create;
end;

destructor TSprint.Destroy;
begin
  FTaskList.Free;
end;

procedure TSprint.Initialize(aDate: TDatetime);
var
  lID: string;
begin
  lID := IntToStr(YearOf(aDate))+'-'+RightStr('0'+IntToStr(WeekOf(aDate)),2);
  TypeDatamodule.GetSprint(lID, FSprintRecord);
end;

function TSprint.TaskCount: integer;
begin
  result := FTaskList.Count;
end;

function TSprint.Tasks(aIndex: integer): TTask;
begin
  result := FTaskList[aIndex];
end;

procedure TSprint.TaskAdd(aTask: TTask);
begin
  FTaskList.Add(aTask);
end;

{$ENDREGION}


{$REGION 'TTask'}

constructor TTask.Create;
begin

end;

destructor TTask.Destroy;
begin

end;

procedure TTask.Initialize(aTaskNum: integer);
begin
  TaskDatamodule.GetTask(aTaskNum, FTaskRecord);
end;

procedure TTask.RefreshData;
begin
  Initialize(FTaskRecord.TaskNum);
end;

procedure TTask.CreateComment(aTaskNum, aProjectNum, aParentTaskNum: integer);
begin
  // create new comment
  FTaskRecord.TaskNum := aTaskNum;
  FTaskRecord.ProjectNum := aProjectNum;
  FTaskRecord.ProjectTaskNum := 0;
  FTaskRecord.ParentTaskNum := aParentTaskNum;
  FTaskRecord.TaskType := tstComment;
  FTaskRecord.TaskSubType := 0;
  FTaskRecord.Status := staCreated;
  InitCreatedTask;
end;

procedure TTask.CreateLink(aTaskNum, aProjectNum, aParentTaskNum: integer);
begin
  // create new link
  FTaskRecord.TaskNum := aTaskNum;
  FTaskRecord.ProjectNum := aProjectNum;
  FTaskRecord.ProjectTaskNum := 0;
  FTaskRecord.ParentTaskNum := aParentTaskNum;
  FTaskRecord.TaskType := tstLink;
  FTaskRecord.TaskSubType := 0;
  FTaskRecord.Status := staCreated;
  InitCreatedTask;
end;

procedure TTask.InitCreatedTask;
begin
  FTaskRecord.CreationDate := now;
  FTaskRecord.ModificationDate := now;
  FTaskRecord.Estimation := 0;
  FTaskRecord.Duration := 0;
  FTaskRecord.SortOrder := 0;
  FTaskRecord.Tag := 0;
  FTaskRecord.AutoPostponedTimes := 0;
end;

procedure TTask.CreateTarget(aTaskNum, aProjectNum: integer);
begin
  // create new target
  FTaskRecord.TaskNum := aTaskNum;
  FTaskRecord.ProjectNum := aProjectNum;
  FTaskRecord.ProjectTaskNum := 0;
  FTaskRecord.TaskType := tstTarget;
  FTaskRecord.TaskSubType := 0;
  FTaskRecord.Status := staCreated;
  FTaskRecord.ParentTaskNum := 0;
  InitCreatedTask;
end;


procedure TTask.CreateTodoTarget(aTaskNum, aProjectNum, aProjectTaskNum, aParentTaskNum: integer);
begin
  // create new to-do
  FTaskRecord.TaskNum := aTaskNum;
  FTaskRecord.ProjectNum := aProjectNum;
  FTaskRecord.ProjectTaskNum := aProjectTaskNum;
  FTaskRecord.TaskType := tstTarget;
  FTaskRecord.TaskSubType := 0;
  FTaskRecord.Status := staValidated;
  FTaskRecord.ParentTaskNum := aParentTaskNum;
  InitCreatedTask;
  if aParentTaskNum=CST_UnclassifiedTodo then
  begin
    FTaskRecord.SprintCode := GlobalVar.CurrentSprintCode;
    FTaskRecord.Status := staScheduled;
    FTaskRecord.ScheduledDate := now;
  end;
end;

procedure TTask.CreateTask(aTaskNum, aProjectNum, aParentTaskNum: integer);
begin
  // create new task
  FTaskRecord.TaskNum := aTaskNum;
  FTaskRecord.ProjectNum := aProjectNum;
  FTaskRecord.ProjectTaskNum := 0;
  FTaskRecord.ParentTaskNum := aParentTaskNum;
  FTaskRecord.TaskType := tstTask;
  FTaskRecord.TaskSubType := 0;
  FTaskRecord.Status := staCreated;
  FTaskRecord.InProgressDate := now;
  InitCreatedTask;
end;


{$ENDREGION}

{$REGION 'TTaskList'}

constructor TTaskList.Create;
begin
  FTaskList := TList.Create;
end;

destructor TTaskList.Destroy;
begin
  FTaskList.Free;
end;

function TTaskList.Add(aTaskNum: integer): TTask;
var
  lTask: TTask;
begin
  lTask := TTask.Create;
  lTask.Initialize(aTaskNum);
  FTaskList.Add(lTask);
  result := lTask;
end;

procedure TTaskList.Load(aProjectNum, aTaskCategory, aParentTaskNum: integer; aSprintCode: string= '');
begin
  FTaskList.Clear;
  TaskDatamodule.FilterTaskOnParent(aProjectNum, aTaskCategory, aParentTaskNum, aSprintCode);
  TaskDatamodule.cdsTask.First;
  while not TaskDatamodule.cdsTask.Eof do
  begin
    Add(TaskDatamodule.cdsTask.FieldByName('TaskNum').AsInteger);
    TaskDatamodule.cdsTask.Next;
  end;
end;

function TTaskList.Search(aTaskNum: integer): TTask;
var
  i: integer;
  lTask: TTask;
begin
  result := nil;
  for i := 0 to FTaskList.Count-1 do
  begin
    lTask := FTaskList[i];
    if lTask.Data.TaskNum=aTaskNum then
    begin
      result := lTask;
      exit;
    end;
  end;
end;


procedure TTaskList.SortByDoneDate;
begin
  FTaskList.Sort(@CompareDoneDate);
end;

function TTaskList.Count: integer;
begin
  result := FTaskList.Count;
end;

function TTaskList.Items(aIndex: integer): TTask;
var
  lTask: TTask;
begin
  lTask := FTaskList[aIndex];
  result := lTask;
end;

function TTaskList.CountChildren(aTaskNum: integer): integer;
begin
  result := 0;
end;

function TTaskList.CountChildrenDone(aTaskNum: integer): integer;
var
  lTask: TTask;
  i, lCount: integer;
begin
  lCount := 0;
  for i := 0 to FTaskList.Count-1 do
  begin
    lTask := FTaskList[i];
    if (lTask.Data.ParentTaskNum=aTaskNum) and (lTask.Data.Status=staDone) then
      inc(lCount);
  end;
  result := lCount;
end;


{$ENDREGION}


{$REGION 'TProject'}

constructor TProject.Create;
begin
  FTargetList := TTaskList.Create;
  FTargetTodoList := TTaskList.Create;
end;

destructor TProject.Destroy;
begin
  FTargetList.Free;
  FTargetTodoList.Free;
end;

procedure TProject.Initialize(aProjectNum: integer);
begin
  if not MainDatamodule.GetProject(aProjectNum, FProjectRecord) then
  begin
    // create new project...
    FProjectRecord.ProjectNum := aProjectNum;
    FProjectRecord.ProjectName := 'My project';
    FProjectRecord.Description := '';
    FProjectRecord.CreationDate := now;
    FProjectRecord.ModificationDate := now;
    FProjectRecord.ColorID := ProjectNumToColor(aProjectNum);
    FProjectRecord.Status := staCreated;
    FProjectRecord.LastTaskNum := 0;
    FProjectRecord.Tag := 0;
    FProjectRecord.SortOrder := 0;
    FProjectRecord.FirstSprint := '';
    FProjectRecord.LastSprint := '';
    MainDatamodule.UpdateProject(FProjectRecord);
  end;
end;

procedure TProject.LoadTarget;
begin
  FTargetList.Load(FProjectRecord.ProjectNum, CST_Target, 0);
end;

procedure TProject.LoadTargetToDo(aParentTaskNum: integer);
begin
  FTargetTodoList.Load(FProjectRecord.ProjectNum, CST_TargetToDo, aParentTaskNum);
end;

procedure TProject.RefreshData;
begin
  Initialize(FProjectRecord.ProjectNum);
end;


function TProject.TargetCount: integer;
begin
  result := FTargetList.Count;
end;

function TProject.TargetItems(aIndex: integer): TTask;
begin
  result := FTargetList.Items(aIndex);
end;

function TProject.TargetAdd(aTaskNum: integer): TTask;
begin
  result := FTargetList.Add(aTaskNum);
end;

function TProject.TargetSearch(aTaskNum: integer): TTask;
begin
  result := FTargetList.Search(aTaskNum);
end;

function TProject.TargetTodoCount: integer;
begin
  result := FTargetTodoList.Count;
end;

function TProject.TargetTodoItems(aIndex: integer): TTask;
begin
  result := FTargetTodoList.Items(aIndex);
end;

function TProject.TargetToDoSearch(aTaskNum: integer): TTask;
begin
  result := FTargetTodoList.Search(aTaskNum);
end;

function TProject.TargetTodoAdd(aTaskNum: integer): TTask;
begin
  result := FTargetTodoList.Add(aTaskNum);
end;


{$ENDREGION}

{$REGION 'TProjectList'}

constructor TProjectList.Create;
begin
  FProjectList := TList.Create;
end;

destructor TProjectList.Destroy;
begin
  FProjectList.Free;
end;

function TProjectList.Add(aProjectNum: integer): TProject;
var
  lProject: TProject;
begin
  lProject := TProject.Create;
  lProject.Initialize(aProjectNum);
  FProjectList.Add(lProject);
  result := lProject;
end;

procedure TProjectList.Load;
begin
  Clear;
  MainDatamodule.cdsProject.First;
  while not MainDatamodule.cdsProject.Eof do
  begin
    if MainDatamodule.cdsProject.FieldByName('Status').AsInteger<>staArchived then
      Add(MainDatamodule.cdsProject.FieldByName('ProjectNum').AsInteger);
    MainDatamodule.cdsProject.Next;
  end;
end;

function TProjectList.Search(aProjectNum: integer): TProject;
var
  i: integer;
  lProject: TProject;
begin
  result := nil;
  for i := 0 to FProjectList.Count-1 do
  begin
    lProject := FProjectList[i];
    if lProject.Data.ProjectNum=aProjectNum then
    begin
      result := lProject;
      exit;
    end;
  end;
end;

procedure TProjectList.Clear;
begin
  FProjectList.Clear;
end;

function TProjectList.Count: integer;
begin
  result := FProjectList.Count;
end;

function TProjectList.Items(aIndex: integer): TProject;
var
  lProject: TProject;
begin
  lProject := FProjectList[aIndex];
  result := lProject;
end;

{$ENDREGION}

end.
